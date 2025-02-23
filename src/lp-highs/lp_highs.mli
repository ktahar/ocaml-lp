
(** High-level interface to HiGHS. *)

val solve :
  ?highs_path:string
  -> Lp.Problem.t
  -> (float * float Lp.PMap.t, string) result
(** Solve the problem using HiGHS. *)
