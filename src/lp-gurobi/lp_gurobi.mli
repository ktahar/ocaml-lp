(** High-level interface to Gurobi *)

module PMap : Map.S with type key = Lp.Poly.t

val solve :
  ?write_fname:string -> Lp.Problem.t -> (float * float PMap.t, string) result
(** Solve the problem using Gurobi. *)
