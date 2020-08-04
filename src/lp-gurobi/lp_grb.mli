(** Thin ctypes wrapper around Gurobi's C API. *)

exception Gurobi_error of string

(** Variable type. *)
module Vt : sig
  type t = CONTINUOUS | BINARY | INTEGER | SEMICONT | SEMIINT

  val of_char : char -> t

  val to_char : t -> char

  val t : t Ctypes.typ
end

(** Constraint sense. *)
module Cs : sig
  type t = LT | GT | EQ

  val of_char : char -> t

  val to_char : t -> char

  val t : t Ctypes.typ
end

(** Status of the model. *)
module Stat : sig
  type t = LOADED | OPTIMAL | INFEASIBLE | INF_OR_UNBD | UNBOUNDED | OTHER

  val of_int : int -> t

  val to_string : t -> string
end

(** *env in C. *)
type env

val env : env Ctypes.typ

(** *model in C. *)
type model

val model : model Ctypes.typ

val get_error_msg : env -> string

val check : env -> int -> unit

val start_env : env -> unit

val empty_env : ?start:bool -> unit -> env

val free_env : env -> unit

val set_int_param : env -> string -> int -> unit

val set_term_output : env -> bool -> unit

val new_model :
     env
  -> string
  -> float list
  -> float list
  -> float list
  -> Vt.t list
  -> string list
  -> model

val free_model : env -> model -> unit

val set_int_attr : env -> model -> string -> int -> unit

val get_int_attr : env -> model -> string -> int

val set_minimize : env -> model -> unit

val set_maximize : env -> model -> unit

val update_model : env -> model -> unit

val optimize : env -> model -> unit

val add_var :
     env
  -> model
  -> int list
  -> float list
  -> float
  -> float
  -> float
  -> Vt.t
  -> string
  -> unit

val add_constr :
  env -> model -> int list -> float list -> Cs.t -> float -> string -> unit

val add_qpterms : env -> model -> int list -> int list -> float list -> unit

val add_qconstr :
     env
  -> model
  -> int list
  -> float list
  -> int list
  -> int list
  -> float list
  -> Cs.t
  -> float
  -> string
  -> unit

val get_status : env -> model -> Stat.t

val get_dbl_attr : env -> model -> string -> float

val get_obj_val : env -> model -> float

val get_dbl_attr_array : env -> model -> string -> int -> int -> float list

val get_obj_x : env -> model -> int -> float list

val write : env -> model -> string -> unit
