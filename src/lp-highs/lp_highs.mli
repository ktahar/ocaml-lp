(** High-level interface to HiGHS. *)

val solve :
     ?path:string
  -> ?msg:bool
  -> ?time_limit:float
  -> ?keep_files:bool
  -> ?options:(string * string) list
  -> Lp.Problem.t
  -> (float * float Lp.PMap.t, string) result
(** Run HiGHS and obtain the output solution.
    @param msg If false, no log is shown
    @param time_limit Maximum time for solver (in seconds)
    @param options List of additional options to pass to solver
    @param keep_files If true, files are saved in the current directory and not deleted after solving
    @param path Path to the solver binary (you can get binaries compiling from source - https://highs.dev)
 *)
