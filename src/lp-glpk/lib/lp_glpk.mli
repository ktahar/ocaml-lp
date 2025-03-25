(** High-level interface to GLPK. *)

module T = Lp_glpk_types.M

module Simplex : sig
  val solve :
       ?term_output:bool
    -> ?msg_lev:T.Msg.t option
    -> ?meth:T.Smcp.Meth.t option
    -> ?pricing:T.Smcp.Pt.t option
    -> ?r_test:T.Smcp.Rt.t option
    -> ?it_lim:int option
    -> ?tm_lim:int option
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
