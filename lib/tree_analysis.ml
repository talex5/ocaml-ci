open Lwt.Infix

let is_directory x =
  match Unix.lstat x with
  | Unix.{ st_kind = S_DIR; _ } -> true
  | _ -> false
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> false

let is_empty_file x =
  match Unix.lstat x with
  | Unix.{ st_kind = S_REG; st_size = 0; _ } -> true
  | _ -> false

let is_toplevel path = not (String.contains path '/')

let ( >>!= ) = Lwt_result.bind

type t = {
  is_duniverse : bool;
  opam_files : string list;
  ocamlformat_source : Analyse_ocamlformat.source option;
}
[@@deriving yojson]

let marshal t = to_yojson t |> Yojson.Safe.to_string

let unmarshal s =
  match Yojson.Safe.from_string s |> of_yojson with
  | Ok x -> x
  | Error e -> failwith e

let opam_files t = t.opam_files

let is_duniverse t = t.is_duniverse

let ocamlformat_source t = t.ocamlformat_source

let is_test_dir = Astring.String.is_prefix ~affix:"test"

let of_dir ~job dir =
  let is_duniverse = is_directory (Filename.concat (Fpath.to_string dir) "duniverse") in
  let cmd = "", [| "find"; "."; "-maxdepth"; "3"; "-name"; "*.opam" |] in
  Current.Process.check_output ~cwd:dir ~cancellable:true ~job cmd >>!= fun output ->
  let opam_files =
    String.split_on_char '\n' output
    |> List.sort String.compare
    |> List.filter_map (function
        | "" -> None
        | path ->
          let path =
            if Astring.String.is_prefix ~affix:"./" path then
              Astring.String.with_range ~first:2 path
            else path
          in
          let check_whitelist_path path =
            match Fpath.v path |> Fpath.segs with
            | [_file] -> true
            | ["duniverse"; _pkg; _file] -> true
            | _ when is_duniverse ->
              Current.Job.log job "WARNING: ignoring opam file %S as not in root or duniverse subdir" path; false
            | segs when List.exists is_test_dir segs ->
              Current.Job.log job "Ignoring test directory %S" path;
              false
            | _ -> true
          in
          let full_path = Filename.concat (Fpath.to_string dir) path in
          if is_empty_file full_path then (
            Current.Job.log job "WARNING: ignoring empty opam file %S" path;
            None
          ) else if check_whitelist_path path then
            Some path
          else None
      )
  in
  (* [opam_files] are used to detect vendored OCamlformat but this only works
     with duniverse, as other opam files are filtered above. *)
  Analyse_ocamlformat.get_ocamlformat_source job ~opam_files ~root:dir >>= fun ocamlformat_source ->
  let r = { opam_files; is_duniverse; ocamlformat_source } in
  Current.Job.log job "@[<v2>Results:@,%a@]" Yojson.Safe.(pretty_print ~std:true) (to_yojson r);
  if opam_files = [] then Lwt_result.fail (`Msg "No opam files found!")
  else if List.filter is_toplevel opam_files = [] then Lwt_result.fail (`Msg "No top-level opam files found!")
  else Lwt_result.return r
