open Ctypes
open Foreign

(* integer constants which are #defined in glpk.h version 4.65 and 5+ *)

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

  let t : t structure typ = structure "smcp"

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

    let to_int = function FFV -> 1 | LFV -> 2 | MFV -> 3 | DTH -> 4 | PCH -> 5

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

  let t : t structure typ = structure "iocp"

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

let set_term_out = foreign "glp_term_out" (BoolInt.t @-> returning void)

type prob = unit ptr

let prob : prob typ = ptr void

let create_prob = foreign "glp_create_prob" (void @-> returning prob)

let delete_prob = foreign "glp_delete_prob" (prob @-> returning void)

let set_prob_name =
  foreign "glp_set_prob_name" (prob @-> string @-> returning void)

let get_prob_name = foreign "glp_get_prob_name" (prob @-> returning string)

let set_obj_dir = foreign "glp_set_obj_dir" (prob @-> Dir.t @-> returning void)

let get_obj_dir = foreign "glp_get_obj_dir" (prob @-> returning Dir.t)

let add_rows = foreign "glp_add_rows" (prob @-> int @-> returning int)

let add_cols = foreign "glp_add_cols" (prob @-> int @-> returning int)

let set_row_name =
  foreign "glp_set_row_name" (prob @-> int @-> string @-> returning void)

let get_row_name = foreign "glp_get_row_name" (prob @-> int @-> returning string)

let set_col_name =
  foreign "glp_set_col_name" (prob @-> int @-> string @-> returning void)

let get_col_name = foreign "glp_get_col_name" (prob @-> int @-> returning string)

let set_row_bnds =
  foreign "glp_set_row_bnds"
    (prob @-> int @-> Bnd.t @-> double @-> double @-> returning void)

let set_col_bnds =
  foreign "glp_set_col_bnds"
    (prob @-> int @-> Bnd.t @-> double @-> double @-> returning void)

let set_obj_coef =
  foreign "glp_set_obj_coef" (prob @-> int @-> double @-> returning void)

let set_mat_row =
  foreign "glp_set_mat_row"
    (prob @-> int @-> int @-> ptr void @-> ptr void @-> returning void)

let set_mat_col =
  foreign "glp_set_mat_col"
    (prob @-> int @-> int @-> ptr void @-> ptr void @-> returning void)

let load_matrix =
  foreign "glp_load_matrix"
    (prob @-> int @-> ptr void @-> ptr void @-> ptr void @-> returning void)

let set_col_kind =
  foreign "glp_set_col_kind" (prob @-> int @-> Vt.t @-> returning void)

let get_col_kind = foreign "glp_get_col_kind" (prob @-> int @-> returning Vt.t)

let get_num_rows = foreign "glp_get_num_rows" (prob @-> returning int)

let get_num_cols = foreign "glp_get_num_cols" (prob @-> returning int)

let get_num_nz = foreign "glp_get_num_nz" (prob @-> returning int)

let get_num_int = foreign "glp_get_num_int" (prob @-> returning int)

let get_num_bin = foreign "glp_get_num_bin" (prob @-> returning int)

let init_smcp = foreign "glp_init_smcp" (ptr Smcp.t @-> returning void)

let init_iocp = foreign "glp_init_iocp" (ptr Iocp.t @-> returning void)

let simplex = foreign "glp_simplex" (prob @-> ptr Smcp.t @-> returning int)

let intopt = foreign "glp_intopt" (prob @-> ptr Iocp.t @-> returning int)

let get_status = foreign "glp_get_status" (prob @-> returning Stat.t)

let mip_status = foreign "glp_mip_status" (prob @-> returning Stat.t)

let get_obj_val = foreign "glp_get_obj_val" (prob @-> returning double)

let mip_obj_val = foreign "glp_mip_obj_val" (prob @-> returning double)

let get_row_prim = foreign "glp_get_row_prim" (prob @-> int @-> returning double)

let get_row_dual = foreign "glp_get_row_dual" (prob @-> int @-> returning double)

let mip_row_val = foreign "glp_mip_row_val" (prob @-> int @-> returning double)

let get_col_prim = foreign "glp_get_col_prim" (prob @-> int @-> returning double)

let get_col_dual = foreign "glp_get_col_dual" (prob @-> int @-> returning double)

let mip_col_val = foreign "glp_mip_col_val" (prob @-> int @-> returning double)
