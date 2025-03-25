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
  (** [solve pb] solves the problem [pb] using GLPK with the simplex algorithm.
    This function does not support integer or boolean variables. It passes arguments to the solver to guide the search. If the problem is optimal or feasible (when the search stopped), the found solution is returned. If the problem is malformed, unbounded, or infeasible, it returns an error.

    If the function is called without parameters, the default parameters from GLPK will be used.

    @param term_output Enable/disable terminal output.
    @param msg_lev Message level for terminal output.
    @param meth Simplex method option.
    @param pricing Pricing technique.
    @param r_test Ratio test technique.
    @param it_lim Simplex iteration limit.
    @param tm_lim Searching time limit, in milliseconds.

    @return
    - [Ok (obj, pmap)] if the problem is optimal or feasible.
    - [Error text] if the problem is malformed, unbounded, or infeasible. The [text] explains the error.
  *)
end

module Milp : sig
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

  (** [solve pb] solves the problem [pb] using GLPK with the intopt procedure.
    It passes arguments to the solver to guide the search. If the problem is optimal or feasible (when the search stopped), the found solution is returned. If the problem is malformed, unbounded, or infeasible, it returns an error.

    If the function is called without parameters, the default parameters from GLPK will be used.

    @param term_output Enable/disable terminal output.
    @param msg_lev Message level for terminal output.
    @param meth Simplex method option.
    @param pricing Pricing technique.
    @param r_test Ratio test technique.
    @param it_lim Simplex iteration limit.
    @param tm_lim Searching time limit, in milliseconds.

    @return
    - [Ok (obj, pmap)] if the problem is optimal or feasible.
    - [Error text] if the problem is malformed, unbounded, or infeasible. The [text] explains the error.
  *)
end

val solve :
  ?term_output:bool -> Lp.Problem.t -> (float * float Lp.PMap.t, string) result
(** Solve the problem using GLPK.
    GLPK can solve only linear problems (LP or MILP).
*)
