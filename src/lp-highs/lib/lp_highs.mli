(** High-level interface to HiGHS. *)

module Cmd : sig
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
  (** Run HiGHS command and obtain the output solution.
      This backend supports LP, MILP, and convex QP.
      MIQP and quadratically constrained classes (QCP/MIQCP) are unsupported.
      @param path Path to the solver binary.
      @param msg If [false], no log is shown on console.
      @param log_path Path to the log file.
      @param time_limit Maximum time allowed for the solver (in seconds).
      @param gap_rel Relative gap tolerance.
      @param gap_abs Absolute gap tolerance.
      @param options Additional HiGHS options in [(name, value)] string pairs.
      @param keep_files If [true], generated files are kept in the current directory.
      @return The objective value and assignments.
  *)
end

module Ctypes : sig
  val solve :
       ?msg:bool
    -> ?log_path:string
    -> ?time_limit:float
    -> ?gap_rel:float
    -> ?gap_abs:float
    -> ?options:(string * string) list
    -> Lp.Problem.t
    -> (float * float Lp.PMap.t, string) result
  (** Run HiGHS through the C API and obtain the output solution.
      This backend supports LP, MILP, and convex QP.
      MIQP and quadratically constrained classes (QCP/MIQCP) are unsupported.
      @param msg If [false], disables console logging in HiGHS.
      @param log_path Path to the HiGHS log file.
      @param time_limit Maximum time allowed for the solver (in seconds).
      @param gap_rel Relative MIP gap tolerance.
      @param gap_abs Absolute MIP gap tolerance.
      @param options Additional HiGHS options in [(name, value)] string pairs.
      @return The objective value and assignments.
  *)
end

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
    By default this uses {!Ctypes.solve}. If [path] is given or [keep_files=true],
    this falls back to {!Cmd.solve}.
*)
