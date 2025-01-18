(** High-level interface to GLPK. *)
val solve :
     ?term_output:bool
  -> ?tol_int:float
  -> ?tol_obj:float option
  -> ?tm_lim:int option
  -> ?out_frq:int option
  -> ?out_dly:int option
  -> ?cb_size:int option
  -> ?mip_gap:float option
  -> ?mir_cuts:bool option
  -> ?gmi_cuts:bool option
  -> ?cov_cuts:bool option
  -> ?clq_cuts:bool option
  -> ?presolve:bool option
  -> ?binarize:bool option
  -> ?fp_heur:bool option
  -> ?ps_heur:bool option
  -> ?ps_tm_lim:int option
  -> ?sr_heur:bool option
  -> ?use_sol:bool option
  -> ?save_sol:string option
  -> ?alien:bool option
  -> ?flip:bool option
  -> Lp.Problem.t
  -> (float * float Lp.PMap.t, string) result
(** Solve the problem using GLPK. The solver only handles linear problems (LP or MILP).
   Optional parameters control solver behavior:
   
   @param term_output Enable/disable terminal output (default: true)
   @param tol_int Absolute tolerance for integer feasibility check (default: 1e-5)
   @param tol_obj Relative tolerance for objective value check (default: 1e-7)
   @param tm_lim Time limit in milliseconds (default: max_int)
   @param out_frq Output frequency in iterations (default: 5000)
   @param out_dly Output delay in milliseconds (default: 10000)
   @param cb_size Extra bytes for branch-and-bound nodes (default: 0)
   @param mip_gap Relative MIP gap tolerance (default: 0.0)
   @param mir_cuts Enable Mixed Integer Rounding cuts (default: false)
   @param gmi_cuts Enable Gomory's mixed integer cuts (default: false)
   @param cov_cuts Enable mixed cover cuts (default: false)
   @param clq_cuts Enable clique cuts (default: false)
   @param presolve Enable MIP presolver (default: false)
   @param binarize Enable LP presolver (default: false)
   @param fp_heur Enable feasibility pump heuristic (default: false)
   @param ps_heur Enable primal simplex heuristic (default: false)
   @param ps_tm_lim Primal simplex time limit in ms (default: 60000)
   @param sr_heur Enable simple rounding heuristic (default: true)
   @param use_sol Use existing solution as initial point (default: false)
   @param save_sol Filename to save solution
   @param alien Enable alien solver mode (default: false)
   @param flip Enable long-step dual simplex (default: true)

   @return Ok (obj_value, solution) if solved successfully,
           Error msg with explanation if failed
*)
