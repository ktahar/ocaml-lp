(* integer constants which are #defined in glpk.h version 4.65 and 5+ *)

module M (F : Ctypes.TYPE) = struct
  open Ctypes
  open F

  module Dir = struct
    type t = MIN | MAX

    let of_int = function
      | 1 ->
          MIN
      | 2 ->
          MAX
      | _ ->
          failwith "Unexpected Direction flag"

    let to_int = function MIN -> 1 | MAX -> 2

    let t = view ~read:of_int ~write:to_int int
  end

  module Vt = struct
    type t = CV | IV | BV

    let of_int = function
      | 1 ->
          CV
      | 2 ->
          IV
      | 3 ->
          BV
      | _ ->
          failwith "Unexpected Vtype flag"

    let to_int = function CV -> 1 | IV -> 2 | BV -> 3

    let t = view ~read:of_int ~write:to_int int
  end

  module Bnd = struct
    type t = FR | LO | UP | DB | FX

    let of_int = function
      | 1 ->
          FR
      | 2 ->
          LO
      | 3 ->
          UP
      | 4 ->
          DB
      | 5 ->
          FX
      | _ ->
          failwith "Unexpected Bound flag"

    let to_int = function FR -> 1 | LO -> 2 | UP -> 3 | DB -> 4 | FX -> 5

    let t = view ~read:of_int ~write:to_int int
  end

  module Stat = struct
    type t = UNDEF | FEAS | INFEAS | NOFEAS | OPT | UNBND

    let of_int = function
      | 1 ->
          UNDEF
      | 2 ->
          FEAS
      | 3 ->
          INFEAS
      | 4 ->
          NOFEAS
      | 5 ->
          OPT
      | 6 ->
          UNBND
      | _ ->
          failwith "Unexpected Status flag"

    let to_int = function
      | UNDEF ->
          1
      | FEAS ->
          2
      | INFEAS ->
          3
      | NOFEAS ->
          4
      | OPT ->
          5
      | UNBND ->
          6

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

  (* GLP_ON = 1 and GLP_OFF = 0 *)
  module BoolInt = struct
    let of_int i = if i = 0 then false else true

    let to_int b = if b then 1 else 0

    let t = view ~read:of_int ~write:to_int int
  end

  module Msg = struct
    type t = OFF | ERR | ON | ALL | DBG

    let of_int = function
      | 0 ->
          OFF
      | 1 ->
          ERR
      | 2 ->
          ON
      | 3 ->
          ALL
      | 4 ->
          DBG
      | _ ->
          failwith "Unexpected Msg flag"

    let to_int = function OFF -> 0 | ERR -> 1 | ON -> 2 | ALL -> 3 | DBG -> 4

    let t = view ~read:of_int ~write:to_int int
  end

  module Smcp = struct
    module Meth = struct
      type t = PRIMAL | DUALP | DUAL

      let of_int = function
        | 1 ->
            PRIMAL
        | 2 ->
            DUALP
        | 3 ->
            DUAL
        | _ ->
            failwith "Unexpected Method flag"

      let to_int = function PRIMAL -> 1 | DUALP -> 2 | DUAL -> 3

      let t = view ~read:of_int ~write:to_int int
    end

    module Pt = struct
      type t = STD | PSE

      let of_int = function
        | 0x11 ->
            STD
        | 0x22 ->
            PSE
        | _ ->
            failwith "Unexpected Pricing flag"

      let to_int = function STD -> 0x11 | PSE -> 0x22

      let t = view ~read:of_int ~write:to_int int
    end

    module Rt = struct
      type t = STD | HAR | FLIP

      let of_int = function
        | 0x11 ->
            STD
        | 0x22 ->
            HAR
        | 0x33 ->
            FLIP
        | _ ->
            failwith "Unexpected Ratio Test flag"

      let to_int = function STD -> 0x11 | HAR -> 0x22 | FLIP -> 0x33

      let t = view ~read:of_int ~write:to_int int
    end

    module An = struct
      type t = AT | NT

      let of_int = function
        | 1 ->
            AT
        | 2 ->
            NT
        | _ ->
            failwith "Unexpected A or N flag"

      let to_int = function AT -> 1 | NT -> 2

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

      let of_int = function
        | 1 ->
            FFV
        | 2 ->
            LFV
        | 3 ->
            MFV
        | 4 ->
            DTH
        | 5 ->
            PCH
        | _ ->
            failwith "Unexpected Branching Technique flag"

      let to_int = function
        | FFV ->
            1
        | LFV ->
            2
        | MFV ->
            3
        | DTH ->
            4
        | PCH ->
            5

      let t = view ~read:of_int ~write:to_int int
    end

    module Bt = struct
      type t = DFS | BFS | BLB | BPH

      let of_int = function
        | 1 ->
            DFS
        | 2 ->
            BFS
        | 3 ->
            BLB
        | 4 ->
            BPH
        | _ ->
            failwith "Unexpected Backtracking Technique flag"

      let to_int = function DFS -> 1 | BFS -> 2 | BLB -> 3 | BPH -> 4

      let t = view ~read:of_int ~write:to_int int
    end

    module Pp = struct
      type t = NONE | ROOT | ALL

      let of_int = function
        | 0 ->
            NONE
        | 1 ->
            ROOT
        | 2 ->
            ALL
        | _ ->
            failwith "Unexpected Preprocessing flag"

      let to_int = function NONE -> 0 | ROOT -> 1 | ALL -> 2

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
