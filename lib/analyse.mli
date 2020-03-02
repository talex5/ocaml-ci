module Analysis : sig
  type t

  val opam_files : t -> string list
  val is_duniverse : t -> bool
  val ocamlformat_source : t -> Analyse_ocamlformat.source option

  val of_dir : job:Current.Job.t -> Fpath.t -> (t, [ `Msg of string ]) result Lwt.t
end

val examine : opam_repository:Current_git.Commit.t Current.t -> Current_git.Commit.t Current.t -> Analysis.t Current.t
(** [examine ~opam_repository src] returns a list of "*.opam" files in [src]. *)
