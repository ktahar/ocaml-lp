(** High-level interface to Gurobi. *)

val solve :
     ?write_fname:string
  -> ?term_output:bool
  -> Lp.Problem.t
  -> (float * float Lp.PMap.t, string) result
(** Solve the problem using Gurobi. *)
