(** Raw ctypes binding to GLPK. *)

module Dir : sig
  type t = MIN | MAX

  val of_int : int -> t

  val to_int : t -> int

  val t : t Ctypes.typ
end

module Vt : sig
  type t = CV | IV | BV

  val of_int : int -> t

  val to_int : t -> int

  val t : t Ctypes.typ
end

module Bnd : sig
  type t = FR | LO | UP | DB | FX

  val of_int : int -> t

  val to_int : t -> int

  val t : t Ctypes.typ
end

module Stat : sig
  type t = UNDEF | FEAS | INFEAS | NOFEAS | OPT | UNBND

  val of_int : int -> t

  val to_int : t -> int

  val to_string : t -> string

  val t : t Ctypes.typ
end

module Msg : sig
  type t = OFF | ERR | ON | ALL | DBG

  val of_int : int -> t

  val to_int : t -> int

  val t : t Ctypes.typ
end

(** Simplex method control parameters. *)
module Smcp : sig
  module Meth : sig
    type t = PRIMAL | DUALP | DUAL

    val of_int : int -> t

    val to_int : t -> int

    val t : t Ctypes.typ
  end

  module Pt : sig
    type t = STD | PSE

    val of_int : int -> t

    val to_int : t -> int

    val t : t Ctypes.typ
  end

  module Rt : sig
    type t = STD | HAR | FLIP

    val of_int : int -> t

    val to_int : t -> int

    val t : t Ctypes.typ
  end

  module An : sig
    type t = AT | NT

    val of_int : int -> t

    val to_int : t -> int

    val t : t Ctypes.typ
  end

  type t

  val t : t Ctypes.structure Ctypes.typ

  val msg_lev : (Msg.t, t Ctypes.structure) Ctypes.field

  val meth : (Meth.t, t Ctypes.structure) Ctypes.field

  val pricing : (Pt.t, t Ctypes.structure) Ctypes.field

  val r_test : (Rt.t, t Ctypes.structure) Ctypes.field

  val tol_bnd : (float, t Ctypes.structure) Ctypes.field

  val tol_dj : (float, t Ctypes.structure) Ctypes.field

  val tol_piv : (float, t Ctypes.structure) Ctypes.field

  val obj_ll : (float, t Ctypes.structure) Ctypes.field

  val obj_ul : (float, t Ctypes.structure) Ctypes.field

  val it_lim : (int, t Ctypes.structure) Ctypes.field

  val tm_lim : (int, t Ctypes.structure) Ctypes.field
  (** time limit in ms *)

  val out_frq : (int, t Ctypes.structure) Ctypes.field
  (** display frequency in ms *)

  val out_dly : (int, t Ctypes.structure) Ctypes.field
  (** display delay in ms *)

  val presolve : (bool, t Ctypes.structure) Ctypes.field
  (** enable/disable presolver *)

  val excl : (bool, t Ctypes.structure) Ctypes.field

  val shift : (bool, t Ctypes.structure) Ctypes.field

  val aorn : (An.t, t Ctypes.structure) Ctypes.field
end

(** Integer optimizer control parameters. *)
module Iocp : sig
  module Br : sig
    type t = FFV | LFV | MFV | DTH | PCH

    val of_int : int -> t

    val to_int : t -> int

    val t : t Ctypes.typ
  end

  module Bt : sig
    type t = DFS | BFS | BLB | BPH

    val of_int : int -> t

    val to_int : t -> int

    val t : t Ctypes.typ
  end

  module Pp : sig
    type t = NONE | ROOT | ALL

    val of_int : int -> t

    val to_int : t -> int

    val t : t Ctypes.typ
  end

  type t

  val t : t Ctypes.structure Ctypes.typ

  val msg_lev : (Msg.t, t Ctypes.structure) Ctypes.field

  val br_tech : (Br.t, t Ctypes.structure) Ctypes.field

  val bt_tech : (Bt.t, t Ctypes.structure) Ctypes.field

  val tol_int : (float, t Ctypes.structure) Ctypes.field

  val tol_obj : (float, t Ctypes.structure) Ctypes.field

  val tm_lim : (int, t Ctypes.structure) Ctypes.field
  (** time limit in ms *)

  val out_frq : (int, t Ctypes.structure) Ctypes.field
  (** display frequency in ms *)

  val out_dly : (int, t Ctypes.structure) Ctypes.field
  (** display delay in ms *)

  val cb_func : (unit Ctypes_static.ptr, t Ctypes.structure) Ctypes.field

  val cb_info : (unit Ctypes_static.ptr, t Ctypes.structure) Ctypes.field

  val cb_size : (int, t Ctypes.structure) Ctypes.field

  val pp_tech : (Pp.t, t Ctypes.structure) Ctypes.field

  val mip_gap : (float, t Ctypes.structure) Ctypes.field

  val mir_cuts : (bool, t Ctypes.structure) Ctypes.field

  val gmi_cuts : (bool, t Ctypes.structure) Ctypes.field

  val cov_cuts : (bool, t Ctypes.structure) Ctypes.field

  val clq_cuts : (bool, t Ctypes.structure) Ctypes.field

  val presolve : (bool, t Ctypes.structure) Ctypes.field

  val binarize : (bool, t Ctypes.structure) Ctypes.field

  val fp_heur : (bool, t Ctypes.structure) Ctypes.field

  val ps_heur : (bool, t Ctypes.structure) Ctypes.field

  val ps_tm_lim : (int, t Ctypes.structure) Ctypes.field
  (** proxy time limit in ms *)

  val sr_heur : (bool, t Ctypes.structure) Ctypes.field

  val use_sol : (bool, t Ctypes.structure) Ctypes.field

  val save_sol : (string, t Ctypes.structure) Ctypes.field

  val alien : (bool, t Ctypes.structure) Ctypes.field

  val flip : (bool, t Ctypes.structure) Ctypes.field
end

val set_term_out : bool -> unit

type prob

val prob : prob Ctypes.typ

val create_prob : unit -> prob

val delete_prob : prob -> unit

val set_prob_name : prob -> string -> unit

val get_prob_name : prob -> string

val set_obj_dir : prob -> Dir.t -> unit

val get_obj_dir : prob -> Dir.t

val add_rows : prob -> int -> int

val add_cols : prob -> int -> int

val set_row_name : prob -> int -> string -> unit

val get_row_name : prob -> int -> string

val set_col_name : prob -> int -> string -> unit

val get_col_name : prob -> int -> string

val set_row_bnds : prob -> int -> Bnd.t -> float -> float -> unit
(** set_row_bnds [prob] [i] [bnd] [lb] [ub] sets bounds of i-th row (constraint).
    If the row is not lower (upper) bounded, lb (ub) is just ignored.
    If the row is equality constraint (Bnd.FX),
    only [lb] is used and [ub] is ignored.
*)

val set_col_bnds : prob -> int -> Bnd.t -> float -> float -> unit
(** set_col_bnds [prob] [j] [bnd] [lb] [ub] sets bounds of j-th col (variable).
    If the col is not lower (upper) bounded, lb (ub) is just ignored.
    If the col is equality constraint (Bnd.FX),
    only [lb] is used and [ub] is ignored.
*)

val set_obj_coef : prob -> int -> float -> unit
(** set_obj_coef [prob] [j] sets the objective coefficient at j-th col (variable) *)

val set_mat_row :
  prob -> int -> int -> unit Ctypes_static.ptr -> unit Ctypes_static.ptr -> unit
(** set_mat_row [prob] [i] [len] [indices] [vals] sets the i-th row of constraint matrix. *)

val set_mat_col :
  prob -> int -> int -> unit Ctypes_static.ptr -> unit Ctypes_static.ptr -> unit
(** set_mat_col [prob] [j] [len] [indices] [vals] sets the j-th column of constraint matrix. *)

val load_matrix :
     prob
  -> int
  -> unit Ctypes_static.ptr
  -> unit Ctypes_static.ptr
  -> unit Ctypes_static.ptr
  -> unit
(** load_matrix [prob] [ne] [ia] [ja] [ar] sets the constraint matrix.
    The matrix is represented as an sparce matrix.
    for k=1 .. [ne], value [ar][k] is set at ([ia][k], [ja][k]) element.
*)

val set_col_kind : prob -> int -> Vt.t -> unit

val get_col_kind : prob -> int -> Vt.t

val get_num_rows : prob -> int

val get_num_cols : prob -> int

val get_num_nz : prob -> int

val get_num_int : prob -> int

val get_num_bin : prob -> int

val init_smcp : Smcp.t Ctypes.structure Ctypes_static.ptr -> unit

val init_iocp : Iocp.t Ctypes.structure Ctypes_static.ptr -> unit

val simplex : prob -> Smcp.t Ctypes.structure Ctypes_static.ptr -> int

val intopt : prob -> Iocp.t Ctypes.structure Ctypes_static.ptr -> int

val get_status : prob -> Stat.t

val mip_status : prob -> Stat.t

val get_obj_val : prob -> float

val mip_obj_val : prob -> float

val get_row_prim : prob -> int -> float

val get_row_dual : prob -> int -> float

val mip_row_val : prob -> int -> float

val get_col_prim : prob -> int -> float

val get_col_dual : prob -> int -> float

val mip_col_val : prob -> int -> float
