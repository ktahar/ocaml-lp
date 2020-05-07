(** top module of package Lp *)

module Var = Var
module Term = Term
module Poly = Poly
module Cnstr = Constraint
module Obj = Objective
module Problem = Problem
module Pclass = Problem.Pclass

(* polynomial builders and operators *)

val c : float -> Poly.t
(** Make monomial of a constant value *)

val var : ?integer:bool -> ?lb:float -> ?ub:float -> string -> Poly.t
(** Make monomial of a variable *)

val binary : string -> Poly.t
(** Make monomial of a binary variable *)

val range :
  ?integer:bool -> ?lb:float -> ?ub:float -> string -> int -> Poly.t array
(** Make an array of monomials of a variable *)

val range2 :
     ?integer:bool
  -> ?lb:float
  -> ?ub:float
  -> string
  -> int
  -> int
  -> Poly.t array array
(** Make 2D array of monomials of a variable *)

val range3 :
     ?integer:bool
  -> ?lb:float
  -> ?ub:float
  -> string
  -> int
  -> int
  -> int
  -> Poly.t array array array
(** Make 3D array of monomials of a variable *)

val ( ~- ) : Poly.t -> Poly.t
(** Negate the whole polynomial *)

val ( + ) : Poly.t -> Poly.t -> Poly.t
(** Add (concatenate) two polynomials *)

val ( - ) : Poly.t -> Poly.t -> Poly.t
(** Subtract two polynomials (concatenate left with negated right ) *)

val expand : Poly.t -> Poly.t -> Poly.t
(** Multiply two polynomials. specifically, performs polynomial expansion. *)

val ( * ) : Poly.t -> Poly.t -> Poly.t
(** Multiply two polynomials. specifically, performs polynomial expansion. *)

val dot : Poly.t -> Poly.t -> Poly.t
(** Regard two polynomials as {i vectors} and take dot product. *)

val ( *@ ) : Poly.t -> Poly.t -> Poly.t
(** Regard two polynomials as {i vectors} and take dot product. *)

val div : Poly.t -> Poly.t -> Poly.t
(** Divide polynomial by a {b univariate} polynomial.
    Be careful as this function raises exception in following cases.
    {ul { - failing to divide (without remainder) }
    { - multivariate polynomial denominator }
    { - zero division }
    }
 *)

val ( / ) : Poly.t -> Poly.t -> Poly.t
(** Equivalent to div *)

val eq : ?name:string -> Poly.t -> Poly.t -> Constraint.t
(** Build an equality constraint. Optinal name can be given. Polynomials are simplified on build. *)

val ( =$ ) : Poly.t -> Poly.t -> Constraint.t
(** Build an unnamed equality constraint. Polynomials are simplified on build. *)

val lt : ?name:string -> Poly.t -> Poly.t -> Constraint.t
(** Build an inequality constraint. Optinal name can be given. Polynomials are simplified on build. *)

val ( <$ ) : Poly.t -> Poly.t -> Constraint.t
(** Build an unnamed inequality constraint. Polynomials are simplified on build. *)

val gt : ?name:string -> Poly.t -> Poly.t -> Constraint.t
(** Build an inequality constraint. Optinal name can be given. Polynomials are simplified on build. *)

val ( >$ ) : Poly.t -> Poly.t -> Constraint.t
(** Build an unnamed inequality constraint. Polynomials are simplified on build. *)

val maximize : Poly.t -> Objective.t
(** Build an objective to maximize a polynomial. The polynomial is simplified on build. *)

val minimize : Poly.t -> Objective.t
(** Build an objective to minimize a polynomial. The polynomial is simplified on build. *)

(* model validation *)

val validate : Problem.t -> bool
(** Validate the problem *)

val classify : Problem.t -> Pclass.t
(** Classify the problem into Pclass *)

(* IO *)

val to_string : ?short:bool -> Problem.t -> string
(** Express the problem in LP file format string *)

val of_string : string -> Problem.t
(** Parse an LP file format string to build the problem *)

val write : ?short:bool -> string -> Problem.t -> unit
(** write [fname] [problem] writes out [problem] to an LP file [fname] *)

val read : string -> Problem.t
(** Parse an LP file to build the problem *)
