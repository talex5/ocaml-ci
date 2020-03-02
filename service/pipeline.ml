open Current.Syntax
open Ocaml_ci

module Git = Current_git
module Github = Current_github
module Docker = Current_docker.Default

let default_compiler = "4.09.0"

let major_minor ocaml_version =
  match Astring.String.cuts ~sep:"." ocaml_version with
  | major :: minor :: _ -> (major, minor)
  | _ -> Fmt.failwith "Invalid OCaml version %S" ocaml_version

let variants =
  (* Hint: use "opam config list" to get a platform's variables *)
  let variant = Ocaml_ci.Solver.variant ~arch:"x86_64" in
  let debian (module D : Conf.DOCKER) ocaml_version =
    let (major, minor) = major_minor ocaml_version in
    let docker_tag = Printf.sprintf "debian-10-ocaml-%s.%s" major minor in
    let info = variant ~os:"linux" ~os_family:"debian" ~os_distribution:"debian" ~os_version:"10" ~ocaml:ocaml_version in
    { Ocaml_ci.Opam_build.docker_tag; docker_context = D.docker_context; info } in
  let os ?docker_distro (module D : Conf.DOCKER) os os_family os_distribution os_version =
    let (major, minor) = major_minor default_compiler in
    let docker_distro = Option.value docker_distro ~default:os_distribution in
    let docker_tag = Printf.sprintf "%s-%s-ocaml-%s.%s" docker_distro os_version major minor in
    let info = variant ~os ~os_family ~os_distribution ~os_version ~ocaml:default_compiler in
    { Ocaml_ci.Opam_build.docker_tag; docker_context = D.docker_context; info } in
  [
    (* Compiler versions:*)
    "4.10", debian (module Conf.Builder_amd1) "4.10.0";
    "4.09", debian (module Conf.Builder_amd3) "4.09.0";
    "4.08", debian (module Conf.Builder_amd1) "4.08.1";
    "4.07", debian (module Conf.Builder_amd2) "4.07.1";
    "4.06", debian (module Conf.Builder_amd2) "4.06.1";
    "4.05", debian (module Conf.Builder_amd3) "4.05.0";
    "4.04", debian (module Conf.Builder_amd3) "4.04.2";
    "4.03", debian (module Conf.Builder_amd2) "4.03.0";
    "4.02", debian (module Conf.Builder_amd2) "4.02.3";
    (* Distributions:                         os       os-family  os-distribution os-version *)
    "alpine",   os (module Conf.Builder_amd1) "linux" "alpine"    "alpine"        "3.10";
    "ubuntu",   os (module Conf.Builder_amd2) "linux" "debian"    "ubuntu"        "19.04";
    "opensuse", os (module Conf.Builder_amd2) "linux" "opensuse"  "opensuse-leap" "15.1" ~docker_distro:"opensuse";
    "centos",   os (module Conf.Builder_amd3) "linux" "rhel"      "centos"        "8";
    "fedora",   os (module Conf.Builder_amd3) "linux" "fedora"    "fedora"        "30";
  ]

let variants_by_id =
  variants |> List.fold_left (fun acc (name, variant) ->
      Astring.String.Map.add variant.Ocaml_ci.Opam_build.info.Ocaml_ci.Solver.id (name, variant) acc
    ) Astring.String.Map.empty

let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

(* Link for GitHub statuses. *)
let url ~owner ~name ~hash = Uri.of_string (Printf.sprintf "https://ci.ocamllabs.io/github/%s/%s/commit/%s" owner name hash)

let _github_status_of_state ~head result =
  let+ head = head
  and+ result = result in
  let { Github.Repo_id.owner; name } = Github.Api.Commit.repo_id head in
  let hash = Github.Api.Commit.hash head in
  let url = url ~owner ~name ~hash in
  match result with
  | Ok _              -> Github.Api.Status.v ~url `Success ~description:"Passed"
  | Error (`Active _) -> Github.Api.Status.v ~url `Pending
  | Error (`Msg m)    -> Github.Api.Status.v ~url `Failure ~description:m

let _set_active_refs ~repo xs =
  let+ repo = repo
  and+ xs = xs in
  let repo = Github.Api.Repo.id repo in
  Index.set_active_refs ~repo (
    xs |> List.map @@ fun x ->
    let commit = Github.Api.Commit.id x in
    let gref = Git.Commit_id.gref commit in
    let hash = Git.Commit_id.hash commit in
    (gref, hash)
  );
  xs

let _job_id x =
  let+ job = Current.Analysis.get x in
  Current.Analysis.job_id job

module Lint = Ocaml_ci.Lint.Make (Conf.Builder_amd1)

let opam_repository () =
  Current_git.clone ~schedule:weekly "git://github.com/ocaml/opam-repository"

let build_with_docker ~repo ~analysis source =
  let pp f sel =
    let vid = sel.Ocaml_ci.Solver.variant in
    match Astring.String.Map.find_opt vid variants_by_id with
    | None -> Fmt.string f "UNKNOWN variant!"
    | Some (name, _) -> Fmt.string f name
  in
  let sels = Current.map Ocaml_ci.Analyse.Analysis.selections analysis in
  sels |> Current.list_map ~pp (fun item ->
      let selections =
        let+ { Solver.variant; _ } as item = item in
        let _name, variant = Astring.String.Map.get variant variants_by_id in (* XXX: find_opt *)
        variant, item
      in
      Opam_build.v ~schedule:weekly ~selections ~repo ~analysis source
    )
(*

  let build docker variant =
    let build_result =
      Opam_build.v ~docker ~schedule:weekly ~variant ~repo ~analysis source
    in
    build_result, job_id build_result
  in
  let lint_result = Lint.v ~schedule:weekly ~analysis ~source in
  List.map (fun (name, variant) -> name, build variant.context variant.docker_tag) variants @
  [ "lint", (lint_result, job_id lint_result) ]
*)

let list_errors ~ok errs =
  let groups =  (* Group by error message *)
    List.sort compare errs |> List.fold_left (fun acc (msg, l) ->
        match acc with
        | (m2, ls) :: acc' when m2 = msg -> (m2, l :: ls) :: acc'
        | _ -> (msg, [l]) :: acc
      ) []
  in
  Error (`Msg (
      match groups with
      | [] -> assert false
      | [ msg, _ ] when ok = 0 -> msg (* Everything failed with the same error *)
      | [ msg, ls ] -> Fmt.strf "%a failed: %s" Fmt.(list ~sep:(unit ", ") string) ls msg
      | _ ->
        (* Multiple error messages; just list everything that failed. *)
        let pp_label f (_, l) = Fmt.string f l in
        Fmt.strf "%a failed" Fmt.(list ~sep:(unit ", ") pp_label) errs
    ))

let _summarise results =
  results
  |> List.map (fun (label, build) ->
      let+ result = Current.state build ~hidden:true in
      (label, result)
    )
  |> Current.list_seq
  |> Current.map @@ fun results ->
  results |> List.fold_left (fun (ok, pending, err, skip) -> function
      | _, Ok `Checked -> (ok, pending, err, skip)  (* Don't count lint checks *)
      | _, Ok `Built -> (ok + 1, pending, err, skip)
      | l, Error `Msg m when Astring.String.is_prefix ~affix:"[SKIP]" m -> (ok, pending, err, (m, l) :: skip)
      | l, Error `Msg m -> (ok, pending, (m, l) :: err, skip)
      | _, Error `Active _ -> (ok, pending + 1, err, skip)
    ) (0, 0, [], [])
  |> fun (ok, pending, err, skip) ->
  if pending > 0 then Error (`Active `Running)
  else match ok, err, skip with
    | 0, [], skip -> list_errors ~ok:0 skip (* Everything was skipped - treat skips as errors *)
    | _, [], _ -> Ok ()                     (* No errors and at least one success *)
    | ok, err, _ -> list_errors ~ok err     (* Some errors found - report *)

let variants = variants |> List.map (fun (_name, variant) -> variant.Ocaml_ci.Opam_build.info) |> Ocaml_ci.Analyse.variants

let local_test repo () =
  let src = Git.Local.head_commit repo in
  let opam_repository = opam_repository () in
  let repo = Current.return { Github.Repo_id.owner = "local"; name = "test" }
  and analysis = Analyse.examine ~variants ~opam_repository src in
  build_with_docker ~repo ~analysis src
  |> Current.ignore_value
(*
  Current.component "summarise" |>
  let** result =
    build_with_docker ~repo ~analysis src
    |> List.map (fun (variant, (build, _job)) -> variant, build)
    |> summarise
  in
  Current.of_output result
*)

let v ~app () =
  ignore app;
  failwith "todo"
(*
  Github.App.installations app |> Current.list_iter ~pp:Github.Installation.pp @@ fun installation ->
  let repos = Github.Installation.repositories installation in
  repos |> Current.list_iter ~pp:Github.Api.Repo.pp @@ fun repo ->
  let refs = Github.Api.Repo.ci_refs repo |> set_active_refs ~repo in
  refs |> Current.list_iter ~pp:Github.Api.Commit.pp @@ fun head ->
  let src = Git.fetch (Current.map Github.Api.Commit.id head) in
  let opam_repository = opam_repository () in
  let analysis = Analyse.examine ~variants ~opam_repository src in
  let builds =
    let repo = Current.map Github.Api.Repo.id repo in
    build_with_docker ~repo ~analysis src in
  let jobs = builds
             |> List.map (fun (variant, (_build, job)) ->
                 let+ x = job in
                 (variant, x)
               )
             |> Current.list_seq
  in
  let summary =
    builds
    |> List.map (fun (variant, (build, _job)) -> variant, build)
    |> summarise
  in
  let status =
    let+ summary = summary in
    match summary with
    | Ok () -> `Passed
    | Error (`Active `Running) -> `Pending
    | Error (`Msg _) -> `Failed
  in
  let index =
    let+ commit = head
    and+ analysis = job_id analysis
    and+ jobs = jobs
    and+ status = status in
    let repo = Current_github.Api.Commit.repo_id commit in
    let hash = Current_github.Api.Commit.hash commit in
    Index.record ~repo ~hash ~status @@ ("(analysis)", analysis) :: jobs
  and set_github_status =
    summary
    |> github_status_of_state ~head
    |> Github.Api.Commit.set_status head "ocaml-ci"
  in
  Current.all [index; set_github_status]
*)
