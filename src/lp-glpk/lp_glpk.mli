(** High-level interface to GLPK. *)

module Simplex : sig
  val solve :
       ?term_output:bool
    -> Lp.Problem.t
    -> (float * float Lp.PMap.t, string) result
end

module Milp : sig
  val solve :
       ?term_output:bool
    -> Lp.Problem.t
    -> (float * float Lp.PMap.t, string) result
end

val solve :
  ?term_output:bool -> Lp.Problem.t -> (float * float Lp.PMap.t, string) result
(** Solve the problem using GLPK.
    GLPK can solve only linear problems (LP or MILP).
*)
