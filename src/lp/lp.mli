(** top module of package {!module:Lp} *)

module Var = Var
module Term = Term
module Poly = Poly
module Cnstr = Cnstr
module Objective = Objective
module Problem = Problem

(** Module for the optimization problem class. *)
module Pclass = Problem.Pclass

(** Map with Poly.t key. It can be used to pack optimization result. *)
module PMap : Map.S with type key = Poly.t

(* polynomial builders and operators *)

val c : float -> Poly.t
(** Make monomial of a constant value. *)

val var : ?integer:bool -> ?lb:float -> ?ub:float -> string -> Poly.t
(** Make monomial of a variable. *)

val binary : string -> Poly.t
(** Make monomial of a binary variable. *)

val range :
     ?integer:bool
  -> ?lb:float
  -> ?ub:float
  -> ?start:int
  -> int
  -> string
  -> Poly.t array
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
  -> Poly.t array array
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
  -> Poly.t array array array
(** Make 3D array of monomials of a variable with uniform bounds. *)

val rangeb : ?start:int -> int -> string -> Poly.t array
(** Make an array of monomials of a binary variable. *)

val range2b :
  ?start0:int -> ?start1:int -> int -> int -> string -> Poly.t array array
(** Make 2D array of monomials of a binary variable. *)

val range3b :
     ?start0:int
  -> ?start1:int
  -> ?start2:int
  -> int
  -> int
  -> int
  -> string
  -> Poly.t array array array
(** Make 3D array of monomials of a binary variable. *)

val rangev :
     ?integer:bool
  -> ?lb:float array
  -> ?ub:float array
  -> ?start:int
  -> int
  -> string
  -> Poly.t array
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
  -> Poly.t array array
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
  -> Poly.t array array array
(** Make 3D array of monomials of a variable with different bounds. *)

val concat : Poly.t array -> Poly.t
(** Concatenate an array of polynomials into single polynomial. *)

val of_float_array : float array -> Poly.t
(** Convert a float array into a polynomial. *)

val zero : Poly.t
(** The monomial of constant zero. *)

val one : Poly.t
(** The monomial of constant one. *)

val ( ~-- ) : Poly.t -> Poly.t
(** Negate the polynomial (negate all terms in the polynomial). *)

val ( ++ ) : Poly.t -> Poly.t -> Poly.t
(** Add (concatenate) two polynomials. *)

val ( -- ) : Poly.t -> Poly.t -> Poly.t
(** Subtract two polynomials (concatenate left with negated right). *)

val expand : Poly.t -> Poly.t -> Poly.t
(** Multiply two polynomials. Specifically, performs polynomial expansion. *)

val ( *~ ) : Poly.t -> Poly.t -> Poly.t
(** Infix equivalent of {!val:expand}. *)

val dot : Poly.t -> Poly.t -> Poly.t
(** Regard two polynomials as {i vectors} and take dot product.
    @raise Failure if the lengths of two polynomials are different.
*)

val ( *@ ) : Poly.t -> Poly.t -> Poly.t
(** Infix equivalent of {!val:dot}. *)

val div : Poly.t -> Poly.t -> Poly.t
(** Divide polynomial by a {b univariate} polynomial.
    Be careful as this function raises exception in some cases.
    @raise Failure
    if failed to divide (with zero remainder) or denominator is multivariate polynomial.
    @raise Division_by_zero
    if denominator is zero.
*)

val ( /~ ) : Poly.t -> Poly.t -> Poly.t
(** Infix equivalent of {!val:div}. *)

val eq : ?eps:float -> ?name:string -> Poly.t -> Poly.t -> Cnstr.t
(** Build an equality constraint. Optional [name] can be given.
    Polynomials are simplified ({!val:Poly.simplify}) on build.
    [eps] specifies the threshold of near-zero, defaulting to 10. *. epsilon_float.
*)

val ( =~ ) : Poly.t -> Poly.t -> Cnstr.t
(** Build an unnamed equality constraint. Polynomials are simplified on build. *)

val lt : ?eps:float -> ?name:string -> Poly.t -> Poly.t -> Cnstr.t
(** Build an inequality constraint. Optional [name] can be given.
    Polynomials are simplified ({!val:Poly.simplify}) on build.
    [eps] specifies the threshold of near-zero, defaulting to 10. *. epsilon_float.
*)

val ( <~ ) : Poly.t -> Poly.t -> Cnstr.t
(** Build an unnamed inequality constraint. Polynomials are simplified on build. *)

val gt : ?eps:float -> ?name:string -> Poly.t -> Poly.t -> Cnstr.t
(** Build an inequality constraint. Optional [name] can be given.
    Polynomials are simplified ({!val:Poly.simplify}) on build.
    [eps] specifies the threshold of near-zero, defaulting to 10. *. epsilon_float.
*)

val ( >~ ) : Poly.t -> Poly.t -> Cnstr.t
(** Build an unnamed inequality constraint. Polynomials are simplified on build. *)

val maximize : ?eps:float -> Poly.t -> Objective.t
(** Build an objective to maximize a polynomial.
    The polynomial is simplified ({!val:Poly.simplify}) on build.
    [eps] specifies the threshold of near-zero, defaulting to 10. *. epsilon_float.
*)

val minimize : ?eps:float -> Poly.t -> Objective.t
(** Build an objective to minimize a polynomial.
    The polynomial is simplified ({!val:Poly.simplify}) on build.
    [eps] specifies the threshold of near-zero, defaulting to 10. *. epsilon_float.
*)

val make : ?name:string -> Objective.t -> Cnstr.t list -> Problem.t
(** Make problem from an {!type:Objective.t} and a constraint ({!type:Cnstr.t}) list.
    String [name] can be given optionally.
*)

(* model validation and manipulation *)

val validate : Problem.t -> bool
(** Validate the problem. [true] ([false]) means the problem is valid (invalid). *)

val classify : Problem.t -> Pclass.t
(** Classify the problem into {!type:Pclass.t}. *)

val vname_list : Problem.t -> string list
(** Make (unique and sorted) list of the variables in a problem. *)

(* IO *)

val to_string : ?short:bool -> Problem.t -> string
(** Express the problem in LP file format string. *)

val of_string : string -> Problem.t
(** Parse an LP file format string to build the problem. *)

val write : ?short:bool -> string -> Problem.t -> unit
(** write [fname] [problem] writes out [problem] to an LP file [fname]. *)

val read : string -> Problem.t
(** Parse an LP file to build the problem. *)
