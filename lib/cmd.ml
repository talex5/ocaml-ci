let context_args = function
  | None -> []
  | Some context -> ["--context"; context]

let config_args = function
  | None -> []
  | Some config -> ["--config"; Fpath.to_string config]

let docker ?config ~docker_context args =
  "", Array.of_list ("docker" :: config_args config @ context_args docker_context @ args)

let login ?config ~docker_context user =
  docker ?config ~docker_context ["login"; "--password-stdin"; "--username"; user]

let pp f (prog, args) =
  if prog <> "" then Fmt.pf f "[%S] " prog;
  Fmt.(list ~sep:sp (quote string)) f (Array.to_list args)
