open Ctypes
open Foreign

(* integer constants which are #defined in glpk.h *)

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
        failwith "Unexpected Bound flag"

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

  let t = view ~read:of_int ~write:to_int int
end

(* simplex method control parameters *)
module Smcp = struct
  type t

  let t : t structure typ = structure "smcp"

  let msg_lev = field t "msg_lev" int

  let meth = field t "meth" int

  let pricing = field t "pricing" int

  let r_test = field t "r_test" int

  let tol_bnd = field t "tol_bnd" double

  let tol_dj = field t "tol_dj" double

  let tol_piv = field t "tol_piv" double

  let obj_ll = field t "obj_ll" double

  let obj_ul = field t "obj_ul" double

  let it_lim = field t "it_lim" int

  let tm_lim = field t "tm_lim" int (* time limit in ms *)

  let out_frq = field t "out_frq" int (* display frequency in ms *)

  let out_dly = field t "out_dly" int (* display delay in ms *)

  let presolve = field t "presolve" int (* enable/disable presolver *)

  let excl = field t "excl" int

  let shift = field t "shift" int

  let aorn = field t "aorn" int

  let () = seal t
end

type prob = unit ptr

let prob : prob typ = ptr void

let create_prob = foreign "glp_create_prob" (void @-> returning prob)

let delete_prob = foreign "glp_delete_prob" (prob @-> returning void)

let set_prob_name =
  foreign "glp_set_prob_name" (prob @-> string @-> returning void)

let get_prob_name = foreign "glp_get_prob_name" (prob @-> returning string)

let set_obj_dir = foreign "glp_set_obj_dir" (prob @-> Dir.t @-> returning void)

let get_obj_dir = foreign "glp_get_obj_dir" (prob @-> returning Dir.t)

let set_row_name =
  foreign "glp_set_row_name" (prob @-> int @-> string @-> returning void)

let get_row_name = foreign "glp_get_row_name" (prob @-> int @-> returning string)

let set_col_name =
  foreign "glp_set_col_name" (prob @-> int @-> string @-> returning void)

let get_col_name = foreign "glp_get_col_name" (prob @-> int @-> returning string)

(** set_row_bnds [prob] [i] [bnd] [lb] [ub] sets bounds of i-th row (constraint).
 * If the row is not lower (upper) bounded, lb (ub) is just ignored.
 * If the row is equality constraint (Bnd.FX),
 * only [lb] is used and [ub] is ignored. *)
let set_row_bnds =
  foreign "glp_set_row_bnds"
    (prob @-> int @-> Bnd.t @-> double @-> double @-> returning void)

(** set_col_bnds [prob] [j] [bnd] [lb] [ub] sets bounds of j-th col (variable).
 * If the col is not lower (upper) bounded, lb (ub) is just ignored.
 * If the col is equality constraint (Bnd.FX),
 * only [lb] is used and [ub] is ignored. *)
let set_col_bnds =
  foreign "glp_set_col_bnds"
    (prob @-> int @-> Bnd.t @-> double @-> double @-> returning void)

(** set_obj_coef [prob] [j] sets the objective coefficient
 * at j-th col (variable) *)
let set_obj_coef =
  foreign "glp_set_obj_coef" (prob @-> int @-> double @-> returning void)

(** set_mat_row [prob] [i] [len] [indices] [vals] sets the i-th row of constraint matrix. *)
let set_mat_row =
  foreign "glp_set_mat_row"
    (prob @-> int @-> int @-> ptr int @-> ptr double @-> returning void)

(** set_mat_col [prob] [j] [len] [indices] [vals] sets the j-th column of constraint matrix. *)
let set_mat_col =
  foreign "glp_set_mat_col"
    (prob @-> int @-> int @-> ptr int @-> ptr double @-> returning void)

(** load_matrix [prob] [ne] [ia] [ja] [ar] sets the constraint matrix.
 * The matrix is represented as an sparce matrix.
 * for k=1 .. [ne], value [ar][k] is set at ([ia][k], [ja][k]) element. *)
let load_matrix =
  foreign "glp_load_matrix"
    (prob @-> int @-> ptr int @-> ptr int @-> ptr double @-> returning void)

let get_num_rows = foreign "glp_get_num_rows" (prob @-> returning int)

let get_num_cols = foreign "glp_get_num_cols" (prob @-> returning int)

let get_num_nz = foreign "glp_get_num_nz" (prob @-> returning int)

let init_smcp = foreign "glp_init_smcp" (ptr Smcp.t @-> returning void)

let simplex = foreign "glp_simplex" (prob @-> ptr Smcp.t @-> returning int)

let get_status = foreign "glp_get_status" (prob @-> returning Stat.t)

let get_obj_val = foreign "glp_get_obj_val" (prob @-> returning double)

let get_row_prim = foreign "glp_get_row_prim" (prob @-> int @-> returning double)

let get_row_dual = foreign "glp_get_row_dual" (prob @-> int @-> returning double)

let get_col_prim = foreign "glp_get_col_prim" (prob @-> int @-> returning double)

let get_col_dual = foreign "glp_get_col_dual" (prob @-> int @-> returning double)
