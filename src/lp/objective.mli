type t

val maximize : ?eps:float -> Poly.t -> t

val minimize : ?eps:float -> Poly.t -> t

val take_vars : t -> Var.t list

val to_poly : t -> Poly.t

val to_string : ?short:bool -> t -> string

val trans_bound : string -> float -> float -> t -> t

val to_integer : string -> t -> t

val to_binary : string -> t -> t

val degree : t -> int

val is_max : t -> bool
