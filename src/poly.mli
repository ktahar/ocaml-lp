(** Module for polynomial expression type *)

(** Type for the polynomial with order up to two (quadratic). *)
type t

(** Type for the polynomial classified by orders *)
type classified = {const: t; linear: t; quad: t}

val c : float -> t
(** Make monomial of a constant value *)

val var : ?integer:bool -> ?lb:float -> ?ub:float -> string -> t
(** Make monomial of a variable *)

val of_var : Var.t -> t
(** Make monomial from a Var.t *)

val of_term : Term.t -> t
(** Make monomial from a Term.t *)

val binary : string -> t
(** Make monomial of a binary variable *)

val range :
     ?integer:bool
  -> ?lb:float
  -> ?ub:float
  -> ?start:int
  -> int
  -> string
  -> t array
(** Make an array of monomials of a variable with uniform bounds *)

val range2 :
     ?integer:bool
  -> ?lb:float
  -> ?ub:float
  -> ?start0:int
  -> ?start1:int
  -> int
  -> int
  -> string
  -> t array array
(** Make 2D array of monomials of a variable with uniform bounds *)

val range3 :
     ?integer:bool
  -> ?lb:float
  -> ?ub:float
  -> ?start0:int
  -> ?start1:int
  -> ?start2:int
  -> int
  -> int
  -> int
  -> string
  -> t array array array
(** Make 3D array of monomials of a variable with uniform bounds *)

val rangeb : ?start:int -> int -> string -> t array
(** Make an array of monomials of a binary variable *)

val range2b :
  ?start0:int -> ?start1:int -> int -> int -> string -> t array array
(** Make 2D array of monomials of a binary variable *)

val range3b :
     ?start0:int
  -> ?start1:int
  -> ?start2:int
  -> int
  -> int
  -> int
  -> string
  -> t array array array
(** Make 3D array of monomials of a binary variable *)

val rangev :
     ?integer:bool
  -> ?lb:float array
  -> ?ub:float array
  -> ?start:int
  -> int
  -> string
  -> t array
(** Make an array of monomials of a variable with different bounds *)

val range2v :
     ?integer:bool
  -> ?lb:float array array
  -> ?ub:float array array
  -> ?start0:int
  -> ?start1:int
  -> int
  -> int
  -> string
  -> t array array
(** Make 2D array of monomials of a variable with different bounds *)

val range3v :
     ?integer:bool
  -> ?lb:float array array array
  -> ?ub:float array array array
  -> ?start0:int
  -> ?start1:int
  -> ?start2:int
  -> int
  -> int
  -> int
  -> string
  -> t array array array
(** Make 3D array of monomials of a variable with different bounds *)

val concat : t array -> t
(** Concatenate an array of polynomials into single polynomial *)

val concat_list : t list -> t
(** Concatenate a list of polynomials into single polynomial *)

val of_float_array : float array -> t
(** Convert a float array into a polynomial *)

val of_term_list : Term.t list -> t
(** Convert a list of terms into a polynomial *)

val to_float : t -> float
(** Convert a Constant monomial to float.
 * Raises Failure if it's not constant monomial *)

val zero : t
(** Constant zero *)

val one : t
(** Constant one *)

val sort : t -> t
(** Sort terms in the polynomial *)

val to_string : ?short:bool -> t -> string
(** Get string expression of the polynomial *)

val partition : t -> t * t
(** Partition terms into pair ( quad or linear, const ) *)

val classify : t -> classified
(** Classify terms into three categories {quad, linear, const} *)

val collision : t -> bool
(** Check if any variable collision exist in the polynomial *)

val simplify : ?epsilon:float -> t -> t
(** Simplify the polynomial.
    The polynomial is sorted and terms with same variables are accumulated.
    After that, near-zero terms are dropped. [epsilon] specifies the threshold
    of near-zero, defaulting to 10. *. epsilon_float.
 *)

val degree : t -> int
(** Get the degree of polynomial *)

val take_vars : t -> Var.t list
(** List up all the variables in the polynomial *)

val neg : t -> t
(** Negate the whole polynomial *)

val ( ~-- ) : t -> t
(** Negate the whole polynomial *)

val ( ++ ) : t -> t -> t
(** Add (concatenate) two polynomials *)

val ( -- ) : t -> t -> t
(** Subtract two polynomials (concatenate left with negated right ) *)

val expand : t -> t -> t
(** Multiply two polynomials. specifically, performs polynomial expansion. *)

val ( *~ ) : t -> t -> t
(** Multiply two polynomials. specifically, performs polynomial expansion. *)

val dot : t -> t -> t
(** Regard two polynomials as {i vectors} and take dot product. *)

val ( *@ ) : t -> t -> t
(** Regard two polynomials as {i vectors} and take dot product. *)

val equiv : t -> t -> bool
(** Check if two polynomials are equivalent *)

val divt : t -> Term.t -> t
(** Divide polynomial by a term. *)

val div : t -> t -> t
(** Divide polynomial by a {b univariate} polynomial.
    Be careful as this function raises exception in following cases.
    {ul { - failing to divide (without remainder) }
    { - multivariate polynomial denominator }
    { - zero division }
    }
 *)

val ( /~ ) : t -> t -> t
(** equivalent to div *)

val trans_bound : string -> float -> float -> t -> t
(** trans_bound [name] [lb] [ub] transforms the bounds of the variable [name] with [lb] and [ub] *)

val to_integer : string -> t -> t
(** to_integer [name] transforms the variable [name] into general integer variable *)

val to_binary : string -> t -> t
(** to_integer [name] transforms the variable [name] into binary variable *)

val double_quad : t -> t
(** double the coefficients in all quadratic terms in the polynomial *)

val half_quad : t -> t
(** half the coefficients in all quadratic terms in the polynomial *)

val map : (Term.t -> 'a) -> t -> 'a list
(** apply a function to all terms in the polynomial and build a list *)

val map_linear : (float -> Var.t -> 'a) -> t -> 'a list
(** apply a function only to linear terms in the polynomial and build a list.
 * Raise Failure if non-linear terms exist. *)

val mapi : (int -> Term.t -> 'a) -> t -> 'a list
(** apply a function to all terms in the polynomial and build a list *)

val iter : (Term.t -> unit) -> t -> unit
(** apply a function to all terms in the polynomial *)

val iteri : (int -> Term.t -> unit) -> t -> unit
(** apply a function to all terms in the polynomial *)

val iter_linear : (float -> Var.t -> unit) -> t -> unit
(** apply a function only to linear terms in the polynomial.
 * non-linear terms are just ignored. *)

val iter_linear_exn : (float -> Var.t -> unit) -> t -> unit
(** apply a function only to linear terms in the polynomial.
 * Raise Failure if non-linear terms exist. *)

val length : t -> int

val take_linear_coeffs : t -> float list
