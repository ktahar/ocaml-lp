type t =
  | Eq of string option * Poly.t * Poly.t
  | Ineq of string option * Poly.t * Poly.t

val to_string : ?short:bool -> t -> string

val simplify_sides : ?eps:float -> Poly.t -> Poly.t -> Poly.t * Poly.t

val simplify : ?eps:float -> t -> t

val take_vars : t -> Lp__.Var.t list

val degree : t -> int

val constant : t -> bool

val eq : ?eps:float -> ?name:string -> Poly.t -> Poly.t -> t

val lt : ?eps:float -> ?name:string -> Poly.t -> Poly.t -> t

val gt : ?eps:float -> ?name:string -> Poly.t -> Poly.t -> t

val ( =~ ) : Poly.t -> Poly.t -> t

val ( <~ ) : Poly.t -> Poly.t -> t

val ( >~ ) : Poly.t -> Poly.t -> t

val lhs : t -> Poly.t

val rhs : t -> Poly.t

val name : t -> string

val trans_bound : string -> float -> float -> t -> t

val to_integer : string -> t -> t

val to_binary : string -> t -> t
