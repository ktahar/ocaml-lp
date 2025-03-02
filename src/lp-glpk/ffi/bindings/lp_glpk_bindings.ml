open Ctypes
module T = Lp_glpk_types.M

module M (F : Ctypes.FOREIGN) = struct
  open F

  let set_term_out = foreign "glp_term_out" (T.BoolInt.t @-> returning void)

  type prob = unit ptr

  let prob : prob typ = ptr void

  let create_prob = foreign "glp_create_prob" (void @-> returning prob)

  let delete_prob = foreign "glp_delete_prob" (prob @-> returning void)

  let set_prob_name =
    foreign "glp_set_prob_name" (prob @-> string @-> returning void)

  let get_prob_name = foreign "glp_get_prob_name" (prob @-> returning string)

  let set_obj_dir =
    foreign "glp_set_obj_dir" (prob @-> T.Dir.t @-> returning void)

  let get_obj_dir = foreign "glp_get_obj_dir" (prob @-> returning T.Dir.t)

  let add_rows = foreign "glp_add_rows" (prob @-> int @-> returning int)

  let add_cols = foreign "glp_add_cols" (prob @-> int @-> returning int)

  let set_row_name =
    foreign "glp_set_row_name" (prob @-> int @-> string @-> returning void)

  let get_row_name =
    foreign "glp_get_row_name" (prob @-> int @-> returning string)

  let set_col_name =
    foreign "glp_set_col_name" (prob @-> int @-> string @-> returning void)

  let get_col_name =
    foreign "glp_get_col_name" (prob @-> int @-> returning string)

  (** set_row_bnds [prob] [i] [bnd] [lb] [ub] sets bounds of i-th row (constraint).
    If the row is not lower (upper) bounded, lb (ub) is just ignored.
    If the row is equality constraint (Bnd.FX),
    only [lb] is used and [ub] is ignored.
*)
  let set_row_bnds =
    foreign "glp_set_row_bnds"
      (prob @-> int @-> T.Bnd.t @-> double @-> double @-> returning void)

  (** set_col_bnds [prob] [j] [bnd] [lb] [ub] sets bounds of j-th col (variable).
    If the col is not lower (upper) bounded, lb (ub) is just ignored.
    If the col is equality constraint (Bnd.FX),
    only [lb] is used and [ub] is ignored.
*)
  let set_col_bnds =
    foreign "glp_set_col_bnds"
      (prob @-> int @-> T.Bnd.t @-> double @-> double @-> returning void)

  (** set_obj_coef [prob] [j] sets the objective coefficient at j-th col (variable) *)
  let set_obj_coef =
    foreign "glp_set_obj_coef" (prob @-> int @-> double @-> returning void)

  (** set_mat_row [prob] [i] [len] [indices] [vals] sets the i-th row of constraint matrix. *)
  let set_mat_row =
    foreign "glp_set_mat_row"
      (prob @-> int @-> int @-> ptr void @-> ptr void @-> returning void)

  (** set_mat_col [prob] [j] [len] [indices] [vals] sets the j-th column of constraint matrix. *)
  let set_mat_col =
    foreign "glp_set_mat_col"
      (prob @-> int @-> int @-> ptr void @-> ptr void @-> returning void)

  (** load_matrix [prob] [ne] [ia] [ja] [ar] sets the constraint matrix.
    The matrix is represented as an sparce matrix.
    for k=1 .. [ne], value [ar][k] is set at ([ia][k], [ja][k]) element.
*)
  let load_matrix =
    foreign "glp_load_matrix"
      (prob @-> int @-> ptr void @-> ptr void @-> ptr void @-> returning void)

  let set_col_kind =
    foreign "glp_set_col_kind" (prob @-> int @-> T.Vt.t @-> returning void)

  let get_col_kind =
    foreign "glp_get_col_kind" (prob @-> int @-> returning T.Vt.t)

  let get_num_rows = foreign "glp_get_num_rows" (prob @-> returning int)

  let get_num_cols = foreign "glp_get_num_cols" (prob @-> returning int)

  let get_num_nz = foreign "glp_get_num_nz" (prob @-> returning int)

  let get_num_int = foreign "glp_get_num_int" (prob @-> returning int)

  let get_num_bin = foreign "glp_get_num_bin" (prob @-> returning int)

  let init_smcp = foreign "glp_init_smcp" (ptr T.Smcp.t @-> returning void)

  let init_iocp = foreign "glp_init_iocp" (ptr T.Iocp.t @-> returning void)

  let simplex = foreign "glp_simplex" (prob @-> ptr T.Smcp.t @-> returning int)

  let intopt = foreign "glp_intopt" (prob @-> ptr T.Iocp.t @-> returning int)

  let get_status = foreign "glp_get_status" (prob @-> returning T.Stat.t)

  let mip_status = foreign "glp_mip_status" (prob @-> returning T.Stat.t)

  let get_obj_val = foreign "glp_get_obj_val" (prob @-> returning double)

  let mip_obj_val = foreign "glp_mip_obj_val" (prob @-> returning double)

  let get_row_prim =
    foreign "glp_get_row_prim" (prob @-> int @-> returning double)

  let get_row_dual =
    foreign "glp_get_row_dual" (prob @-> int @-> returning double)

  let mip_row_val = foreign "glp_mip_row_val" (prob @-> int @-> returning double)

  let get_col_prim =
    foreign "glp_get_col_prim" (prob @-> int @-> returning double)

  let get_col_dual =
    foreign "glp_get_col_dual" (prob @-> int @-> returning double)

  let mip_col_val = foreign "glp_mip_col_val" (prob @-> int @-> returning double)
end
