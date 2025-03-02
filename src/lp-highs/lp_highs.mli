(** High-level interface to HiGHS. *)

val solve :
     ?path:string
  -> ?msg:bool
  -> ?log_path:string
  -> ?time_limit:float
  -> ?keep_files:bool
  -> ?gap_rel:float
  -> ?gap_abs:float
  -> ?options:(string * string) list
  -> Lp.Problem.t
  -> (float * float Lp.PMap.t, string) result
(** Run HiGHS and obtain the output solution.
    @param path Path to the solver binary
           (you can get binaries by compiling from source - https://highs.dev).
           If this parameter is not set,
           the executable located at the path specified in the environment variable [HIGHS_CMD] is used.
           If both are unset, an error is thrown.
    @param msg If [false], no log is shown on console.
           [true] by default.
    @param log_path Path to the log file.
    @param time_limit Maximum time allowed for the solver (in seconds).
           By default, there is no time limit.
    @param gap_rel Relative gap tolerance for the solver to stop (in fraction).
    @param gap_abs Absolute gap tolerance for the solver to stop.
    @param options A list of additional options to pass to the solver.
    @param keep_files If [true], files are saved in the current directory and not deleted after solving.
           [false] by [default].
*)
