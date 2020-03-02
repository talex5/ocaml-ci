open Current.Syntax

type variant = {
  docker_tag : string;
  docker_context : string option;
  info : Solver.variant;
}

let crunch_list items = Dockerfile.(crunch (empty @@@ items))

let safe_char = function
  | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '_' -> true
  | _ -> false

let check_safe s =
  if not (Astring.String.for_all safe_char s) then
    Fmt.failwith "Unsafe characters in %S" s

let build_cache repo =
  let { Current_github.Repo_id.owner; name } = repo in
  check_safe owner;
  check_safe name;
  Printf.sprintf
    "--mount=type=cache,target=/src/_build,uid=1000,sharing=private,id=dune:%s:%s"
    owner name

(* If the package's directory name doesn't contain a dot then opam will default to
   using the last known version, which is usually wrong. In particular, if a multi-project
   repostory adds a new package with a constraint "{ =version }" on an existing one,
   this will fail because opam will pin the new package as "dev" but the old one with
   the version of its last release. *)
let maybe_add_dev ~dir name =
  if Fpath.is_current_dir dir || not (String.contains (Fpath.basename dir) '.') then name ^ ".dev" else name

(* Group opam files by directory.
   e.g. ["a/a1.opam"; "a/a2.opam"; "b/b1.opam"] ->
        [("a", ["a/a1.opam"; "a/a2.opam"], ["a1.dev"; "a2.dev"]);
         ("b", ["b/b1.opam"], ["b1.dev"])
        ] *)
let group_opam_files =
  ListLabels.fold_left ~init:[] ~f:(fun acc x ->
      let item = Fpath.v x in
      let dir = Fpath.parent item in
      let pkg = Filename.basename x |> Filename.chop_extension |> maybe_add_dev ~dir in
      match acc with
      | (prev_dir, prev_items, pkgs) :: rest when Fpath.equal dir prev_dir -> (prev_dir, x :: prev_items, pkg :: pkgs) :: rest
      | _ -> (dir, [x], [pkg]) :: acc
    )

(* Generate Dockerfile instructions to copy all the files in [items] into the
   image, creating the necessary directories first, and then pin them all. *)
let pin_opam_files groups =
  let open Dockerfile in
  let dirs = groups |> List.map (fun (dir, _, _) -> Printf.sprintf "%S" (Fpath.to_string dir)) |> String.concat " " in
  (run "mkdir -p %s" dirs @@@ (
    groups |> List.map (fun (dir, files, _) ->
        copy ~src:files ~dst:(Fpath.to_string dir) ()
      )
  )) @@ crunch_list (
    groups |> List.map (fun (dir, _, pkgs) ->
        pkgs
        |> List.map (fun pkg ->
            run "opam pin add -yn %s %S" pkg (Fpath.to_string dir)
          )
        |> crunch_list
      )
  )

(* Get the packages directly in "." *)
let rec get_root_opam_packages = function
  | [] -> []
  | (dir, _, pkgs) ::_ when Fpath.is_current_dir dir -> pkgs
  | _ :: rest -> get_root_opam_packages rest

let download_cache = "--mount=type=cache,target=/home/opam/.opam/download-cache,uid=1000"

type key =  {
  base : string;
  info : Analyse.Analysis.t;
  repo : Current_github.Repo_id.t;
  variant : string;
  packages : OpamPackage.t list;
  commit : string;
}

let dockerfile { base; info; repo; variant; packages; commit } =
  let opam_files = Analyse.Analysis.opam_files info in
  let groups = group_opam_files opam_files in
  let caches =
    if Analyse.Analysis.is_duniverse info then Printf.sprintf "%s %s" download_cache (build_cache repo)
    else download_cache
  in
  let root_pkgs = get_root_opam_packages groups in
  let packages = List.map OpamPackage.to_string packages in
  let non_root_pkgs = packages |> List.filter (fun pkg -> not (List.mem pkg root_pkgs)) in
  let open Dockerfile in
  let distro_extras =
    if Astring.String.is_prefix ~affix:"fedora" variant then
      run "sudo dnf install -y findutils" (* (we need xargs) *)
    else
      empty
  in
  let build_cmd =
    if Analyse.Analysis.is_duniverse info then
      run "%s opam depext --update -iy dune dune-configurator odoc" caches @@
      copy ~chown:"opam" ~src:["."] ~dst:"/src/" () @@
      run "%s opam exec -- dune build @install" caches @@
      run "%s opam exec -- dune runtest" caches @@
      run "%s opam exec -- dune build @doc" caches
    else
      copy ~chown:"opam" ~src:["."] ~dst:"/src/" () @@
      run "opam exec -- dune build @install @runtest && rm -rf _build"
  in
  comment "syntax = docker/dockerfile:experimental@sha256:ee85655c57140bd20a5ebc3bb802e7410ee9ac47ca92b193ed0ab17485024fe5" @@
  from base @@
  comment "%s" variant @@
  distro_extras @@
  workdir "/src" @@
  run "sudo chown opam /src" @@
  pin_opam_files groups @@
  run "cd ~/opam-repository && (git reset --hard %s || (git pull origin master && git reset --hard %s)) && opam update" commit commit @@
  env ["DEPS", String.concat " " non_root_pkgs] @@
  run "%s opam depext --update -y %s $DEPS" download_cache (String.concat " " root_pkgs) @@
  run "%s opam install $DEPS" download_cache @@
  (if Analyse.Analysis.is_duniverse info then run "opam pin remove $(opam pin -s) -n" else empty) @@
  build_cmd

let cache = Hashtbl.create 10000
let cache_max_size = 1000000

let dockerfile ~base ~info ~repo ~variant ~selection =
  let { Solver.packages; commit; variant = _ } = selection in
  let key = { base; info; repo; variant; packages; commit } in
  match Hashtbl.find_opt cache key with
  | Some x -> x
  | None ->
    let x = dockerfile key in
    if Hashtbl.length cache > cache_max_size then Hashtbl.clear cache;
    Hashtbl.add cache key x;
    x

module PC = Current_cache.Make(Pull)

let pull ~schedule selections =
  Current.component "pull" |>
  let> variant, _selections = selections in
  let tag = "ocurrent/opam:" ^ variant.docker_tag in
  PC.get ~schedule Pull.No_context { Pull.Key.docker_context = variant.docker_context; tag }

module BC = Current_cache.Make(Build)

let build ?timeout ?(squash=false) ?dockerfile ?pool ?(build_args=[]) selections src =
  Current.component "build" |>
  let> commit = src
  and> variant, _selections = selections
  and> dockerfile = Current.option_seq dockerfile in
  let dockerfile =
    match dockerfile with
    | None -> `File (Fpath.v "Dockerfile")
    | Some (`File _ as f) -> f
    | Some (`Contents c) -> `Contents (Dockerfile.string_of_t c)
  in
  BC.get { Build.pull = false; pool; timeout }
    { Build.Key.commit; dockerfile; docker_context = variant.docker_context; squash; build_args }

let v ~schedule ~selections ~repo ~analysis source =
  let open Current.Syntax in
  let info =
    let+ info = analysis in
    let opam_files = Analyse.Analysis.opam_files info in
    if opam_files = [] then failwith "No opam files found!";
    info
  in
  let dockerfile =
    let+ base = pull ~schedule selections
    and+ variant, selection = selections
    and+ repo = repo
    and+ info = info in
    let tag = "ocurrent/opam:" ^ variant.docker_tag in
    `Contents (dockerfile ~base ~info ~repo ~variant:tag ~selection)
  in
  let build = build ~dockerfile selections source in
  Current.map (fun _ -> `Built) build
