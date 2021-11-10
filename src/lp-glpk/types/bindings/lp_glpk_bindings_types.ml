module C = Lp_glpk_consts.M

module M (F : Ctypes.TYPE) = struct
  open Ctypes
  open F

  module Dir = struct
    type t = MIN | MAX

    let of_int i =
      if i = C.min then MIN
      else if i = C.max then MAX
      else failwith "Unexpected Direction flag"

    let to_int = function MIN -> C.min | MAX -> C.max

    let t = view ~read:of_int ~write:to_int int
  end

  module Vt = struct
    type t = CV | IV | BV

    let of_int i =
      if i = C.cv then CV
      else if i = C.iv then IV
      else if i = C.bv then BV
      else failwith "Unexpected Vtype flag"

    let to_int = function CV -> C.cv | IV -> C.iv | BV -> C.bv

    let t = view ~read:of_int ~write:to_int int
  end

  module Bnd = struct
    type t = FR | LO | UP | DB | FX

    let of_int i =
      if i = C.fr then FR
      else if i = C.lo then LO
      else if i = C.up then UP
      else if i = C.db then DB
      else if i = C.fx then FX
      else failwith "Unexpected Bound flag"

    let to_int = function
      | FR ->
          C.fr
      | LO ->
          C.lo
      | UP ->
          C.up
      | DB ->
          C.db
      | FX ->
          C.fx

    let t = view ~read:of_int ~write:to_int int
  end

  module Stat = struct
    type t = UNDEF | FEAS | INFEAS | NOFEAS | OPT | UNBND

    let of_int i =
      if i = C.undef then UNDEF
      else if i = C.feas then FEAS
      else if i = C.infeas then INFEAS
      else if i = C.nofeas then NOFEAS
      else if i = C.opt then OPT
      else if i = C.unbnd then UNBND
      else failwith "Unexpected Status flag"

    let to_int = function
      | UNDEF ->
          C.undef
      | FEAS ->
          C.feas
      | INFEAS ->
          C.infeas
      | NOFEAS ->
          C.nofeas
      | OPT ->
          C.opt
      | UNBND ->
          C.unbnd

    let to_string = function
      | UNDEF ->
          "Undefined"
      | FEAS ->
          "Feasible"
      | INFEAS ->
          "Infeasible"
      | NOFEAS ->
          "NoFeasible"
      | OPT ->
          "Optimal"
      | UNBND ->
          "Unbounded"

    let t = view ~read:of_int ~write:to_int int
  end

  (* GLP_ON = true and GLP_OFF = false *)
  module BoolInt = struct
    let of_int i = if i = C.off then false else true

    let to_int b = if b then C.on else C.off

    let t = view ~read:of_int ~write:to_int int
  end

  module Msg = struct
    type t = OFF | ERR | ON | ALL | DBG

    let of_int i =
      if i = C.msg_off then OFF
      else if i = C.msg_err then ERR
      else if i = C.msg_on then ON
      else if i = C.msg_all then ALL
      else if i = C.msg_dbg then DBG
      else failwith "Unexpected Msg flag"

    let to_int = function
      | OFF ->
          C.msg_off
      | ERR ->
          C.msg_err
      | ON ->
          C.msg_on
      | ALL ->
          C.msg_all
      | DBG ->
          C.msg_dbg

    let t = view ~read:of_int ~write:to_int int
  end

  module Smcp = struct
    module Meth = struct
      type t = PRIMAL | DUALP | DUAL

      let of_int i =
        if i = C.primal then PRIMAL
        else if i = C.dualp then DUALP
        else if i = C.dual then DUAL
        else failwith "Unexpected Method flag"

      let to_int = function
        | PRIMAL ->
            C.primal
        | DUALP ->
            C.dualp
        | DUAL ->
            C.dual

      let t = view ~read:of_int ~write:to_int int
    end

    module Pt = struct
      type t = STD | PSE

      let of_int i =
        if i = C.pt_std then STD
        else if i = C.pt_pse then PSE
        else failwith "Unexpected Pricing flag"

      let to_int = function STD -> C.pt_std | PSE -> C.pt_pse

      let t = view ~read:of_int ~write:to_int int
    end

    module Rt = struct
      type t = STD | HAR | FLIP

      let of_int i =
        if i = C.rt_std then STD
        else if i = C.rt_har then HAR
        else if i = C.rt_flip then FLIP
        else failwith "Unexpected Ratio Test flag"

      let to_int = function
        | STD ->
            C.rt_std
        | HAR ->
            C.rt_har
        | FLIP ->
            C.rt_flip

      let t = view ~read:of_int ~write:to_int int
    end

    module An = struct
      type t = AT | NT

      let of_int i =
        if i = C.use_at then AT
        else if i = C.use_nt then NT
        else failwith "Unexpected A or N flag"

      let to_int = function AT -> C.use_at | NT -> C.use_nt

      let t = view ~read:of_int ~write:to_int int
    end

    type t

    let t : t structure typ = typedef (structure "smcp") "glp_smcp"

    let msg_lev = field t "msg_lev" Msg.t

    let meth = field t "meth" Meth.t

    let pricing = field t "pricing" Pt.t

    let r_test = field t "r_test" Rt.t

    let tol_bnd = field t "tol_bnd" double

    let tol_dj = field t "tol_dj" double

    let tol_piv = field t "tol_piv" double

    let obj_ll = field t "obj_ll" double

    let obj_ul = field t "obj_ul" double

    let it_lim = field t "it_lim" int

    let tm_lim = field t "tm_lim" int

    let out_frq = field t "out_frq" int

    let out_dly = field t "out_dly" int

    let presolve = field t "presolve" BoolInt.t

    let excl = field t "excl" BoolInt.t

    let shift = field t "shift" BoolInt.t

    let aorn = field t "aorn" An.t

    let () = seal t
  end

  module Iocp = struct
    module Br = struct
      type t = FFV | LFV | MFV | DTH | PCH

      let of_int i =
        if i = C.br_ffv then FFV
        else if i = C.br_lfv then LFV
        else if i = C.br_mfv then MFV
        else if i = C.br_dth then DTH
        else if i = C.br_pch then PCH
        else failwith "Unexpected Branching Technique flag"

      let to_int = function
        | FFV ->
            C.br_ffv
        | LFV ->
            C.br_lfv
        | MFV ->
            C.br_mfv
        | DTH ->
            C.br_dth
        | PCH ->
            C.br_pch

      let t = view ~read:of_int ~write:to_int int
    end

    module Bt = struct
      type t = DFS | BFS | BLB | BPH

      let of_int i =
        if i = C.bt_dfs then DFS
        else if i = C.bt_bfs then BFS
        else if i = C.bt_blb then BLB
        else if i = C.bt_bph then BPH
        else failwith "Unexpected Backtracking Technique flag"

      let to_int = function
        | DFS ->
            C.bt_dfs
        | BFS ->
            C.bt_bfs
        | BLB ->
            C.bt_blb
        | BPH ->
            C.bt_bph

      let t = view ~read:of_int ~write:to_int int
    end

    module Pp = struct
      type t = NONE | ROOT | ALL

      let of_int i =
        if i = C.pp_none then NONE
        else if i = C.pp_root then ROOT
        else if i = C.pp_all then ALL
        else failwith "Unexpected Preprocessing flag"

      let to_int = function
        | NONE ->
            C.pp_none
        | ROOT ->
            C.pp_root
        | ALL ->
            C.pp_all

      let t = view ~read:of_int ~write:to_int int
    end

    type t

    let t : t structure typ = typedef (structure "iocp") "glp_iocp"

    let msg_lev = field t "msg_lev" Msg.t

    let br_tech = field t "br_tech" Br.t

    let bt_tech = field t "bt_tech" Bt.t

    let tol_int = field t "tol_int" double

    let tol_obj = field t "tol_obj" double

    let tm_lim = field t "tm_lim" int

    let out_frq = field t "out_frq" int

    let out_dly = field t "out_dly" int

    let cb_func = field t "cb_func" (ptr void)

    let cb_info = field t "cb_info" (ptr void)

    let cb_size = field t "cb_size" int

    let pp_tech = field t "pp_tech" Pp.t

    let mip_gap = field t "mip_gap" double

    let mir_cuts = field t "mir_cuts" BoolInt.t

    let gmi_cuts = field t "gmi_cuts" BoolInt.t

    let cov_cuts = field t "cov_cuts" BoolInt.t

    let clq_cuts = field t "clq_cuts" BoolInt.t

    let presolve = field t "presolve" BoolInt.t

    let binarize = field t "binarize" BoolInt.t

    let fp_heur = field t "fp_heur" BoolInt.t

    let ps_heur = field t "ps_heur" BoolInt.t

    let ps_tm_lim = field t "ps_tm_lim" int

    let sr_heur = field t "sr_heur" BoolInt.t

    let use_sol = field t "use_sol" BoolInt.t

    let save_sol = field t "save_sol" string

    let alien = field t "alien" BoolInt.t

    let flip = field t "flip" BoolInt.t

    let () = seal t
  end
end
