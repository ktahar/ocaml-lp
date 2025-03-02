(* fetch #define int constants from glpk.h *)

module M (F : Ctypes.TYPE) = struct
  let on = F.(constant "GLP_ON" int)

  let off = F.(constant "GLP_OFF" int)

  let min = F.(constant "GLP_MIN" int)

  let max = F.(constant "GLP_MAX" int)

  let cv = F.(constant "GLP_CV" int)

  let iv = F.(constant "GLP_IV" int)

  let bv = F.(constant "GLP_BV" int)

  let fr = F.(constant "GLP_FR" int)

  let lo = F.(constant "GLP_LO" int)

  let up = F.(constant "GLP_UP" int)

  let db = F.(constant "GLP_DB" int)

  let fx = F.(constant "GLP_FX" int)

  let undef = F.(constant "GLP_UNDEF" int)

  let feas = F.(constant "GLP_FEAS" int)

  let infeas = F.(constant "GLP_INFEAS" int)

  let nofeas = F.(constant "GLP_NOFEAS" int)

  let opt = F.(constant "GLP_OPT" int)

  let unbnd = F.(constant "GLP_UNBND" int)

  let msg_off = F.(constant "GLP_MSG_OFF" int)

  let msg_err = F.(constant "GLP_MSG_ERR" int)

  let msg_on = F.(constant "GLP_MSG_ON" int)

  let msg_all = F.(constant "GLP_MSG_ALL" int)

  let msg_dbg = F.(constant "GLP_MSG_DBG" int)

  let primal = F.(constant "GLP_PRIMAL" int)

  let dualp = F.(constant "GLP_DUALP" int)

  let dual = F.(constant "GLP_DUAL" int)

  let pt_std = F.(constant "GLP_PT_STD" int)

  let pt_pse = F.(constant "GLP_PT_PSE" int)

  let rt_std = F.(constant "GLP_RT_STD" int)

  let rt_har = F.(constant "GLP_RT_HAR" int)

  let rt_flip = F.(constant "GLP_RT_FLIP" int)

  let use_at = F.(constant "GLP_USE_AT" int)

  let use_nt = F.(constant "GLP_USE_NT" int)

  let br_ffv = F.(constant "GLP_BR_FFV" int)

  let br_lfv = F.(constant "GLP_BR_LFV" int)

  let br_mfv = F.(constant "GLP_BR_MFV" int)

  let br_dth = F.(constant "GLP_BR_DTH" int)

  let br_pch = F.(constant "GLP_BR_PCH" int)

  let bt_dfs = F.(constant "GLP_BT_DFS" int)

  let bt_bfs = F.(constant "GLP_BT_BFS" int)

  let bt_blb = F.(constant "GLP_BT_BLB" int)

  let bt_bph = F.(constant "GLP_BT_BPH" int)

  let pp_none = F.(constant "GLP_PP_NONE" int)

  let pp_root = F.(constant "GLP_PP_ROOT" int)

  let pp_all = F.(constant "GLP_PP_ALL" int)
end
