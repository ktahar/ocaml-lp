(** Module for polynomial expression. *)

(** Type for the polynomial with order up to two (quadratic). *)
type t

(** Type for the polynomial classified by orders. *)
type classified = {const: t; linear: t; quad: t}

(** Type for the decomposed expression of the polynomial. *)
type decomposed =
  { const: float
  ; lcs: float list
  ; lvs: Var.t list
  ; qcs: float list
  ; qv0s: Var.t list
  ; qv1s: Var.t list }

val c : float -> t
(** Make monomial of a constant value. *)

val var : ?integer:bool -> ?lb:float -> ?ub:float -> string -> t
(** Make monomial of a variable. *)

val of_var : Var.t -> t
(** Make monomial from a {!type:Var.t}. *)

val of_term : Term.t -> t
(** Make monomial from a {!type:Term.t}. *)

val binary : string -> t
(** Make monomial of a binary variable. *)

val range :
     ?integer:bool
  -> ?lb:float
  -> ?ub:float
  -> ?start:int
  -> int
  -> string
  -> t array
(** Make an array of monomials of a variable with uniform bounds. *)

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
(** Make 2D array of monomials of a variable with uniform bounds. *)

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
(** Make 3D array of monomials of a variable with uniform bounds. *)

val rangeb : ?start:int -> int -> string -> t array
(** Make an array of monomials of a binary variable. *)

val range2b :
  ?start0:int -> ?start1:int -> int -> int -> string -> t array array
(** Make 2D array of monomials of a binary variable. *)

val range3b :
     ?start0:int
  -> ?start1:int
  -> ?start2:int
  -> int
  -> int
  -> int
  -> string
  -> t array array array
(** Make 3D array of monomials of a binary variable. *)

val rangev :
     ?integer:bool
  -> ?lb:float array
  -> ?ub:float array
  -> ?start:int
  -> int
  -> string
  -> t array
(** Make an array of monomials of a variable with different bounds. *)

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
(** Make 2D array of monomials of a variable with different bounds. *)

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
(** Make 3D array of monomials of a variable with different bounds. *)

val concat : t array -> t
(** Concatenate an array of polynomials into single polynomial. *)

val concat_list : t list -> t
(** Concatenate a list of polynomials into single polynomial. *)

val of_float_array : float array -> t
(** Convert a float array into a polynomial. *)

val of_term_list : Term.t list -> t
(** Convert a list of terms into a polynomial. *)

val to_float : t -> float
(** Convert a Constant monomial to float.
    @raise Failure if it's not constant monomial. *)

val zero : t
(** The monomial of constant zero. *)

val one : t
(** The monomial of constant one. *)

val sort : t -> t
(** Sort terms in the polynomial. *)

val compare : t -> t -> int
(** Compare the polynomial. *)

val to_string : ?short:bool -> t -> string
(** Get string expression of the polynomial. *)

val partition : t -> t * t
(** Partition terms into pair of (quad or linear, const) terms. *)

val classify : t -> classified
(** Classify terms into three categories: quad, linear, and const ({!type:classified}). *)

val decompose : t -> decomposed
(** Decompose the polynomial into {!type:decomposed}. *)

val collision : t -> bool
(** Check if any variable collision exist in the polynomial. *)

val simplify : ?eps:float -> t -> t
(** Simplify the polynomial.
    The polynomial is sorted and terms with same variables are accumulated.
    After that, near-zero terms are dropped.
    [eps] specifies the threshold of near-zero, defaulting to 10. *. epsilon_float.
 *)

val degree : t -> int
(** Get the degree of polynomial. *)

val take_vars : t -> Var.t list
(** List up all the variables in the polynomial. *)

val uniq_vars : t -> Var.t list
(** List up all the unique variables in the polynomial. *)

val linear_coeff : t -> Var.t -> float
(** take linear coefficient of a variable in a polynomial. *)

val quad_coeff : t -> Var.t -> Var.t -> float
(** take quad coefficient of the variables in a polynomial. *)

val neg : t -> t
(** Negate the polynomial (negate all terms in the polynomial). *)

val ( ~-- ) : t -> t
(** Negate the polynomial (negate all terms in the polynomial). *)

val ( ++ ) : t -> t -> t
(** Add (concatenate) two polynomials. *)

val ( -- ) : t -> t -> t
(** Subtract two polynomials (concatenate left with negated right). *)

val expand : t -> t -> t
(** Multiply two polynomials. Specifically, performs polynomial expansion. *)

val ( *~ ) : t -> t -> t
(** Infix equivalent of {!val:expand}. *)

val dot : t -> t -> t
(** Regard two polynomials as {i vectors} and take dot product.
    @raise Failure if the lengths of two polynomials are different.
*)

val ( *@ ) : t -> t -> t
(** Infix equivalent of {!val:dot}. *)

val equiv : ?eps:float -> t -> t -> bool
(** Check if two polynomials are equivalent *)

val divt : t -> Term.t -> t
(** Divide polynomial by a term. *)

val div : t -> t -> t
(** Divide polynomial by a {b univariate} polynomial.
    Be careful as this function raises exception in some cases.
    @raise Failure
    if failed to divide (with zero remainder) or denominator is multivariate polynomial.
    @raise Division_by_zero
    if denominator is zero.
*)

val ( /~ ) : t -> t -> t
(** Infix equivalent of {!val:div}. *)

val with_bound : string -> float -> float -> t -> t
(** with_bound [name] [lb] [ub] transforms the bounds of the variable [name] with [lb] and [ub]. *)

val to_integer : string -> t -> t
(** to_integer [name] transforms the variable [name] into general integer variable. *)

val to_binary : string -> t -> t
(** to_binary [name] transforms the variable [name] into binary variable. *)

val double_quad : t -> t
(** Double the coefficients in all quadratic terms in the polynomial. *)

val half_quad : t -> t
(** Half the coefficients in all quadratic terms in the polynomial. *)

val map : (Term.t -> 'a) -> t -> 'a list
(** Apply a function to all terms in the polynomial and build a list. *)

val map_linear : (float -> Var.t -> 'a) -> t -> 'a list
(** Apply a function only to linear terms in the polynomial and build a list.
    @raise Failure if non-linear terms exist. *)

val mapi : (int -> Term.t -> 'a) -> t -> 'a list
(** Apply a function to all terms in the polynomial and build a list. *)

val iter : (Term.t -> unit) -> t -> unit
(** Apply a function to all terms in the polynomial. *)

val iteri : (int -> Term.t -> unit) -> t -> unit
(** Apply a function to all terms in the polynomial. *)

val iter_linear : (float -> Var.t -> unit) -> t -> unit
(** Apply a function only to linear terms in the polynomial.
 * non-linear terms are just ignored. *)

val iter_linear_exn : (float -> Var.t -> unit) -> t -> unit
(** Apply a function only to linear terms in the polynomial.
    @raise Failure if non-linear terms exist. *)

val length : t -> int
(** Get number of terms (length) of a polynomial. *)

val take_linear_coeffs : t -> float list
(** Make list of coefficients in linear terms in a polynomial. *)
