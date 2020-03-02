module Analysis : sig
  type t

  val opam_files : t -> string list
  val is_duniverse : t -> bool
  val ocamlformat_source : t -> Analyse_ocamlformat.source option
  val selections : t -> Solver.selection list
end

type variants

val variants : Solver.variant list -> variants

val examine :
  variants:variants ->
  opam_repository:Current_git.Commit.t Current.t ->
  Current_git.Commit.t Current.t -> Analysis.t Current.t
(** [examine ~variants ~opam_repository src] returns a list of "*.opam" files in [src]. *)
