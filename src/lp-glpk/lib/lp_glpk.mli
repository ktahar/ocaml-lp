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
    -> ?br_tech:T.Iocp.Br.t option
    -> ?bt_tech:T.Iocp.Bt.t option
    -> ?pp_tech:T.Iocp.Pp.t option
    -> ?sr_heur:bool option
    -> ?fp_heur:bool option
    -> ?ps_heur:bool option
    -> ?ps_tm_lim:int option
    -> ?gmi_cuts:bool option
    -> ?mir_cuts:bool option
    -> ?cov_cuts:bool option
    -> ?clq_cuts:bool option
    -> ?mip_gap:float option
    -> ?tm_lim_int:int option
    -> Lp.Problem.t
    -> (float * float Lp.PMap.t, string) result

  (** [solve pb] solves the problem [pb] using GLPK with the intopt procedure.
    It passes arguments to the solver to guide the search.

    If the function is called without parameters, the default parameters from GLPK will be used.

    Generic parameters

    @param term_output Enable/disable terminal output.
    @param msg_lev Message level for terminal output.


    Simplex parameters

    @param meth Simplex method option.
    @param pricing Pricing technique.
    @param r_test Ratio test technique.
    @param it_lim Simplex iteration limit.
    @param tm_lim Simplex searching time limit, in milliseconds.

    Integer solver parameter

    @param br_tech Branching technique option
    @param bt_tech Backtracking technique option
    @param pp_tech Preprocessing technique option
    @param sr_heur Simple rounding heuristic option
    @param fp_heur Feasibility pump heuristic option
    @param ps_heur Proximity search heuristic option
    @param ps_tm_lim Time limit, in milliseconds, for the proximity search heuristic
    @param gmi_cuts Gomory's mixed integer cut option
    @param mir_cuts Mixed integer rounding (MIR) cut option
    @param cov_cuts Mixed cover cut option
    @param clq_cuts Clique cut option
    @param mip_gap The relative mip gap tolerance. If the relative mip gap for currently known best integer feasible solution falls below this tolerance, the solver terminates the search. This allows obtainig suboptimal integer feasible solutions if solving the problem to optimality takes too long time.
    @param tm_lim_int Searching time limit, in milliseconds.

    @return
    - [Ok (obj, pmap)] if the problem is optimal or feasible (when search stopped because of time limit, or gap reached).
    - [Error text] if the problem is malformed, unbounded, or infeasible. The [text] explains the error.
  *)
end

val solve :
  ?term_output:bool -> Lp.Problem.t -> (float * float Lp.PMap.t, string) result
(** Solve the problem using GLPK.
    GLPK can solve only linear problems (LP or MILP).
*)
