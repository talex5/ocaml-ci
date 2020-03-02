type variant = {
  docker_tag : string;
  docker_context : string option;
  info : Solver.variant;
}

(** Build and test all the opam packages in a given build context.
    [~variant] is the variant of the "ocurrent/opam" image.
    [~repo] is the ID of the repository-under-test on GitHub. *)
val v :
  schedule:Current_cache.Schedule.t ->
  selections:(variant * Solver.selection) Current.t ->
  repo:Current_github.Repo_id.t Current.t ->
  analysis:Analyse.Analysis.t Current.t ->
  Current_git.Commit.t Current.t ->
  [> `Built ] Current.t
