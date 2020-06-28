(** Module for the constraint. *)

(** Type for an equality or inequality constraint. *)
type t

val to_string : ?short:bool -> t -> string
(** Get string expression (LP file format) of the constraint. *)

val take_vars : t -> Var.t list
(** Make list of the variables in a constraint. *)

val degree : t -> int
(** Get degree of a constraint. *)

val constant : t -> bool
(** Check if the constraint is constant (no variable), and then invalid. *)

val eq : ?eps:float -> ?name:string -> Poly.t -> Poly.t -> t
(** Build an equality constraint. Optional [name] can be given.
    Polynomials are simplified ({!val:Poly.simplify}) on build.
    [eps] specifies the threshold of near-zero, defaulting to 10. *. epsilon_float.
*)

val lt : ?eps:float -> ?name:string -> Poly.t -> Poly.t -> t
(** Build an inequality constraint. Optional [name] can be given.
    Polynomials are simplified ({!val:Poly.simplify}) on build.
    [eps] specifies the threshold of near-zero, defaulting to 10. *. epsilon_float.
*)

val gt : ?eps:float -> ?name:string -> Poly.t -> Poly.t -> t
(** Build an inequality constraint. Optional [name] can be given.
    Polynomials are simplified ({!val:Poly.simplify}) on build.
    [eps] specifies the threshold of near-zero, defaulting to 10. *. epsilon_float.
*)

val ( =~ ) : Poly.t -> Poly.t -> t
(** Build an unnamed equality constraint. Polynomials are simplified on build. *)

val ( <~ ) : Poly.t -> Poly.t -> t
(** Build an unnamed inequality constraint. Polynomials are simplified on build. *)

val ( >~ ) : Poly.t -> Poly.t -> t
(** Build an unnamed inequality constraint. Polynomials are simplified on build. *)

val lhs : t -> Poly.t
(** Take left hand side (polynomial) of a constraint. *)

val rhs : t -> float
(** Take right hand side (float) of a constraint. *)

val sides : t -> Poly.t * float
(** Take pair of lhs and rhs of a constraint. *)

val name : t -> string
(** Take name of constraint. An empty string ("") is returned if it is unnamed. *)

val is_eq : t -> bool
(** True (false) if the constraint is of equality (inequality). *)

val with_bound : string -> float -> float -> t -> t
(** with_bound [name] [lb] [ub] transforms the bounds of the variable [name] with [lb] and [ub]. *)

val to_integer : string -> t -> t
(** to_integer [name] transforms the variable [name] into general integer variable. *)

val to_binary : string -> t -> t
(** to_binary [name] transforms the variable [name] into binary variable. *)
