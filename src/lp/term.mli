(** Module for a term in the polynomial.
    Typically, users don't need to use this module directly.
    See higher-level module for polynomials ({!module:Poly}) instead.
*)

type t =
  | Const of float
  | Linear of float * Var.t
  | Quad of float * Var.t * Var.t

val c : float -> t

val var : ?integer:bool -> ?lb:float -> ?ub:float -> string -> t

val of_var : Var.t -> t

val binary : string -> t

val range : ?integer:bool -> ?lb:float -> ?ub:float -> string -> int -> t array

val format_float : float -> string

val format_float_short : float -> string

val to_string : ?short:bool -> t -> string

val mul : t -> t -> t

val div : t -> t -> t

val zero : t

val one : t

val neg : t -> t

val sort : t -> t

val degree : t -> int

val near_zero : ?eps:float -> t -> bool

val common_var : t -> t -> bool

val collision : t -> t -> bool

val compare : t -> t -> int

val with_bound : string -> float -> float -> t -> t

val to_binary : string -> t -> t

val to_integer : string -> t -> t

val double_quad : t -> t

val half_quad : t -> t
