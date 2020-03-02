type t [@@deriving yojson]

include Current_cache.S.WITH_MARSHAL with type t := t

val opam_files : t -> string list
val is_duniverse : t -> bool
val ocamlformat_source : t -> Analyse_ocamlformat.source option
val of_dir : job:Current.Job.t -> Fpath.t -> (t, [ `Msg of string ]) result Lwt.t
