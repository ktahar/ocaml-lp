(** High-level interface to HiGHS. *)

val solve :
     ?path:string
  -> ?msg:bool
  -> ?time_limit:float
  -> ?keep_files:bool
  -> ?options:(string * string) list
  -> Lp.Problem.t
  -> (float * float Lp.PMap.t, string) result
(** Solve the problem using HiGHS.
    @param options list of additional options to pass to solver.
 *)
