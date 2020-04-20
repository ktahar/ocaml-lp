(** Type for the polynomial with order up to two (quadratic).
 * Internal representation (Term.t list) is intentionally exposed for now. *)
type t = Term.t list

(** Type for the polynomial classified by orders *)
type classified = {quad: t; linear: t; const: t}

val c : float -> t
(** make monomial of a constant value *)

val var : ?integer:bool -> ?lb:float -> ?ub:float -> string -> t
(** make monomial of a variable *)

val binary : string -> t
(** make monomial of a binary variable *)

val range : ?integer:bool -> ?lb:float -> ?ub:float -> string -> int -> t array
(** make array of monomials of a variable *)

val sort : t -> t
(** sort terms in the polynomial *)

val to_string : ?short:bool -> t -> string
(** get string expression of the polynomial *)

val partition : t -> t * t
(** partition terms into pair ( quad or linear, const ) *)

val classify : t -> classified
(** classify terms into three categories {quad, linear, const} *)

val collision : t -> bool
(** check if any variable collision exist in the polynomial *)

val simplify : t -> t
(** simplify the polynomial *)

val degree : t -> int
(** get the degree of polynomial *)

val take_vars : t -> Var.t list
(** listup all the variables in the polynomial *)

val neg : t -> t
(** negate the whole polynomial *)

val ( ~- ) : t -> t
(** negate the whole polynomial *)

val ( + ) : t -> t -> t
(** add (concatenate) two polynomials *)

val ( * ) : t -> t -> t
(** multiply two polynomials. specifically, performs polynomial expansion. *)

val dot : t -> t -> t
(** regard two polynomials as {i vectors} and take dot product. *)

val ( *@ ) : t -> t -> t
(** regard two polynomials as {i vectors} and take dot product. *)

val trans_bound : string -> float -> float -> t -> t
(** trans_bound [name] [lb] [ub] transforms the bounds of the variable [name] with [lb] and [ub] *)

val to_integer : string -> t -> t
(** to_integer [name] transforms the variable [name] into general integer variable *)

val to_binary : string -> t -> t
(** to_integer [name] transforms the variable [name] into binary variable *)
