type t = Max of Poly.t | Min of Poly.t

val maximize : ?eps:float -> Poly.t -> t

val minimize : ?eps:float -> Poly.t -> t

val take_vars : t -> Var.t list

val to_poly : t -> Poly.t

val to_string : ?short:bool -> t -> string

val degree : t -> int
