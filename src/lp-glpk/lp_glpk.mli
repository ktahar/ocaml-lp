(** High-level interface to GLPK *)

module PMap : Map.S with type key = Lp.Poly.t

module Simplex : sig
  val solve : Lp.Problem.t -> (float * float PMap.t, string) result
end

module Milp : sig
  val solve : Lp.Problem.t -> (float * float PMap.t, string) result
end

val solve : Lp.Problem.t -> (float * float PMap.t, string) result
(** Solve the problem using GLPK.
   GLPK can solve only linear problems (LP or MILP).
*)
