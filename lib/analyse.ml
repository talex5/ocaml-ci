open Lwt.Infix
open Current.Syntax

let ( >>!= ) = Lwt_result.bind

let pool = Current.Pool.create ~label:"analyse" 2

type variants = {
  items : Solver.variant list;
  digest : string;
}

let variants items =
  let summary = Fmt.strf "%a" (Fmt.(list ~sep:sp) Solver.pp_variant) items in
  { items; digest = Digest.string summary }

module Analysis = struct
  type t = {
    tree : Tree_analysis.t;
    selections : Solver.selection list;
  } [@@deriving yojson]

  let marshal t = Yojson.Safe.to_string (to_yojson t)

  let unmarshal s =
    match of_yojson (Yojson.Safe.from_string s) with
    | Ok x -> x
    | Error e -> failwith e

  let opam_files t = Tree_analysis.opam_files t.tree
  let is_duniverse t = Tree_analysis.is_duniverse t.tree
  let ocamlformat_source t = Tree_analysis.ocamlformat_source t.tree
  let selections t = t.selections
end

module Examine = struct
  type t = No_context

  module Key = struct
    type t = {
      src : Current_git.Commit.t;
      opam_repository : Current_git.Commit.t;
      variants : variants;
    }

    let digest { src; opam_repository; variants } =
      let json = `Assoc [
        "src", `String (Current_git.Commit.id src);
        "opam-repository", `String (Current_git.Commit.id opam_repository);
        "variants", `String variants.digest;
      ] in
      Yojson.Safe.to_string json
  end

  module Value = Analysis

  let id = "ci-analyse"

  let build No_context job { Key.src; opam_repository; variants } =
    ignore opam_repository;
    Current.Job.start job ~pool ~level:Current.Level.Harmless >>= fun () ->
    Current_git.with_checkout ~job src @@ fun src ->
    Tree_analysis.of_dir ~job src >>!= fun tree ->
    Current_git.with_checkout ~job opam_repository @@ fun opam_repository ->
    Solver.solve ~job ~opam_repository ~tree src ~variants:variants.items >>= fun selections ->
    Lwt.return (Ok { Analysis.tree; selections })

  let pp f _ = Fmt.string f "Analyse"

  let auto_cancel = false
end

module Examine_cache = Current_cache.Make(Examine)

let examine ~variants ~opam_repository src =
  Current.component "Analyse" |>
  let> src = src
  and> opam_repository = opam_repository in
  Examine_cache.get Examine.No_context { Examine.Key.src; opam_repository; variants }
