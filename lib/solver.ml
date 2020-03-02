open Lwt.Infix

let ( / ) = Filename.concat

type package = OpamPackage.t

let package_to_yojson x = `String (OpamPackage.to_string x)

let package_of_yojson = function
  | `String x -> Ok (OpamPackage.of_string x)
  | x -> Error (Fmt.strf "Bad JSON %a" Yojson.Safe.pp x)

type selection = {
  variant : string;
  packages : package list;
  commit : string;                    (* Commit in opam-repository to use *)
} [@@deriving yojson]

let with_dir path fn =
  let ch = Unix.opendir path in
  match fn ch with
  | x -> Unix.closedir ch; x
  | exception ex -> Unix.closedir ch; raise ex

let list_dir path =
  let rec aux ch =
    match Unix.readdir ch with
    | name -> name :: aux ch
    | exception End_of_file -> []
  in
  with_dir path aux

type variant = {
  id : string;
  arch : string;
  os : string;
  os_distribution : string;
  os_version : string;
  os_family : string;
  ocaml : OpamPackage.Version.t;
}

let pp_variant f x = Fmt.string f x.id

let variant ~arch ~os ~os_distribution ~os_version ~os_family ~ocaml =
  let id = Printf.sprintf "%s/%s/%s/%s/%s/%s" arch os os_family os_distribution os_version ocaml in
  let ocaml = OpamPackage.Version.of_string ocaml in
  { id; arch; os; os_distribution; os_version; os_family; ocaml }

module Context = struct
  type rejection = UserConstraint of OpamFormula.atom

  type t = {
    variant : variant;
    packages_dir : string;
    pins : (OpamPackage.Version.t * string) OpamPackage.Name.Map.t;     (* name -> version, opam path *)
    constraints : OpamFormula.version_constraint OpamTypes.name_map;    (* User-provided constraints *)
    test : OpamPackage.Name.Set.t;
  }

  let load t pkg =
    let { OpamPackage.name; version = _ } = pkg in
    let opam_path =
      match OpamPackage.Name.Map.find_opt name t.pins with
      | Some (_, path) -> path
      | None -> t.packages_dir / OpamPackage.Name.to_string name / OpamPackage.to_string pkg / "opam"
    in
    OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw opam_path))

  let user_restrictions t name =
    OpamPackage.Name.Map.find_opt name t.constraints

  let env t pkg v =
    if List.mem v OpamPackageVar.predefined_depends_variables then None
    else match OpamVariable.Full.to_string v with
      | "version" -> Some (OpamTypes.S (OpamPackage.Version.to_string (OpamPackage.version pkg)))
      | "arch" -> Some (OpamTypes.S t.variant.arch)
      | "os" -> Some (OpamTypes.S t.variant.os)
      | "os-distribution" -> Some (OpamTypes.S t.variant.os_distribution)
      | "os-version" -> Some (OpamTypes.S t.variant.os_version)
      | "os-family" -> Some (OpamTypes.S t.variant.os_family)
      | _ ->
        OpamConsole.warning "Unknown variable %S" (OpamVariable.Full.to_string v);
        None

  let filter_deps t pkg f =
    let test = OpamPackage.Name.Set.mem (OpamPackage.name pkg) t.test in
    f
    |> OpamFilter.partial_filter_formula (env t pkg)
    |> OpamFilter.filter_deps ~build:true ~post:true ~test ~doc:false ~dev:false ~default:false

  let candidates t name =
    match OpamPackage.Name.Map.find_opt name t.pins with
    | Some (version, _) -> [version, None]
    | None ->
      match list_dir (t.packages_dir / OpamPackage.Name.to_string name) with
      | versions ->
        let user_constraints = user_restrictions t name in
        versions
        |> List.filter_map (fun dir ->
            match OpamPackage.of_string_opt dir with
            | Some pkg -> Some (OpamPackage.version pkg)
            | None -> None
          )
        |> List.sort (fun a b -> OpamPackage.Version.compare b a)
        |> List.map (fun v ->
            match user_constraints with
            | Some test when not (OpamFormula.check_version_formula (OpamFormula.Atom test) v) ->
              v, Some (UserConstraint (name, Some test))  (* Reject *)
            | _ -> v, None
          )
      | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
        OpamConsole.log "opam-zi" "Package %S not found!" (OpamPackage.Name.to_string name);
        []

  let pp_rejection f = function
    | UserConstraint x -> Fmt.pf f "Rejected by user-specified constraint %s" (OpamFormula.string_of_atom x)
end

module Input = Opam_zi.Model(Context)
module Solver = Zeroinstall_solver.Make(Input)

let requirements ~context pkgs =
  let role =
    match pkgs with
    | [pkg] -> Input.role context pkg
    | pkgs ->
      let impl = Input.virtual_impl ~context ~depends:pkgs () in
      Input.virtual_role [impl]
  in
  { Input.role; command = None }

let dev = OpamPackage.Version.of_string "dev"

let pp_sel f pkg =
  Fmt.string f (OpamPackage.to_string pkg)

let ocaml_name = OpamPackage.Name.of_string "ocaml"

(* Use "git-log" to find the oldest commit with these package versions.
   This avoids invalidating the Docker build cache on every update to opam-repository. *)
let oldest_commit_with ~job ~opam_repository pkgs =
  let paths =
    pkgs |> List.filter_map (fun { OpamPackage.name; version } ->
        let name = OpamPackage.Name.to_string name in
        let version = OpamPackage.Version.to_string version in
        if version = "dev" then None
        else Some (Printf.sprintf "%s/%s.%s" name name version)
      )
  in
  let cmd = "git" :: "-C" :: opam_repository / "packages" :: "log" :: "-n" :: "1" :: "--format=format:%H" :: paths in
  let cmd = ("", Array.of_list cmd) in
  Current.Process.check_output ~cancellable:true ~job cmd >|= function
  | Error (`Msg m) -> Fmt.failwith "git-log failed! %s" m
  | Ok hash -> String.trim hash

let solve ~job ~opam_repository ~tree src ~variants =
  let src = Fpath.to_string src in
  let opam_repository = Fpath.to_string opam_repository in
  let pkgs =
    Tree_analysis.opam_files tree
    |> List.map (fun path ->
        let name = Filename.basename path |> Filename.chop_extension |> OpamPackage.Name.of_string in
        let version =
          let dir = Filename.dirname path in
          if dir = "." then dev
          else match OpamPackage.of_string_opt dir with
            | Some { OpamPackage.version; _ } -> version
            | None -> dev
        in
        OpamPackage.create name version, path
      ) in
  let root_pkgs =
    pkgs |> List.filter_map (fun (pkg, path) ->
        if Filename.dirname path = "." then Some (OpamPackage.name pkg)
        else None)
  in
  let pins =
    pkgs
    |> List.map (fun (pkg, path) -> OpamPackage.name pkg, (OpamPackage.version pkg, src / path))
    |> OpamPackage.Name.Map.of_list
  in
  let pp_name f name = Fmt.string f (OpamPackage.Name.to_string name) in
  Current.Job.log job "Solving for %a" (Fmt.(list ~sep:comma) pp_name) root_pkgs;
  variants |> Lwt_list.filter_map_s (fun variant ->
    Current.Job.log job "= %a =" pp_variant variant;
    let context = { Context.
                    variant;
                    packages_dir = opam_repository / "packages";
                    pins;
                    constraints = OpamPackage.Name.Map.singleton ocaml_name (`Eq, variant.ocaml);
                    test = OpamPackage.Name.Set.of_list root_pkgs;
                  } in
    let req = requirements ~context root_pkgs in
    let t0 = Unix.gettimeofday () in
    let r = Solver.do_solve ~closest_match:false req in
    let t1 = Unix.gettimeofday () in
    match r with
    | Some sels ->
      Current.Job.log job "Solve succeeded in %.2f s" (t1 -. t0);
      let pkgs =
        sels
        |> Solver.Output.to_map |> Solver.Output.RoleMap.to_seq |> List.of_seq
        |> List.filter_map (fun (_role, sel) -> Input.version (Solver.Output.unwrap sel))
      in
      Current.Job.log job "-> @[<hov>%a@]" Fmt.(list ~sep:sp pp_sel) pkgs;
      oldest_commit_with pkgs ~job ~opam_repository >|= fun commit ->
      Current.Job.log job "(valid since opam-repository commit %s)" commit;
      Some { variant = variant.id; packages = pkgs; commit }
    | None ->
      Current.Job.log job "Eliminated all possibilities in %.2f s" (t1 -. t0);
      Lwt.return_none
    )
