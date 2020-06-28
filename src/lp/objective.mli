(** Module for the objective. *)

(** Type for a maximize or minimize objective. *)
type t

val maximize : ?eps:float -> Poly.t -> t
(** Build an objective to maximize a polynomial.
    The polynomial is simplified ({!val:Poly.simplify}) on build.
    [eps] specifies the threshold of near-zero, defaulting to 10. *. epsilon_float.
*)

val minimize : ?eps:float -> Poly.t -> t
(** Build an objective to minimize a polynomial.
    The polynomial is simplified ({!val:Poly.simplify}) on build.
    [eps] specifies the threshold of near-zero, defaulting to 10. *. epsilon_float.
*)

val take_vars : t -> Var.t list
(** Make list of the variables in the objective. *)

val to_poly : t -> Poly.t
(** Get polynomial expression of the objective function. *)

val to_string : ?short:bool -> t -> string
(** Get string expression (LP file format) of the objective. *)

val with_bound : string -> float -> float -> t -> t
(** with_bound [name] [lb] [ub] transforms the bounds of the variable [name] with [lb] and [ub]. *)

val to_integer : string -> t -> t
(** to_integer [name] transforms the variable [name] into general integer variable. *)

val to_binary : string -> t -> t
(** to_binary [name] transforms the variable [name] into binary variable. *)

val degree : t -> int
(** Get degree of the objective. *)

val is_max : t -> bool
(** True (false) if the objective is of maximize (minimize). *)
