type t

val to_string : ?short:bool -> t -> string

val take_vars : t -> Var.t list

val degree : t -> int

val constant : t -> bool

val eq : ?eps:float -> ?name:string -> Poly.t -> Poly.t -> t

val lt : ?eps:float -> ?name:string -> Poly.t -> Poly.t -> t

val gt : ?eps:float -> ?name:string -> Poly.t -> Poly.t -> t

val ( =~ ) : Poly.t -> Poly.t -> t

val ( <~ ) : Poly.t -> Poly.t -> t

val ( >~ ) : Poly.t -> Poly.t -> t

val lhs : t -> Poly.t

val rhs : t -> float

val sides : t -> Poly.t * float

val name : t -> string

val is_eq : t -> bool

val trans_bound : string -> float -> float -> t -> t

val to_integer : string -> t -> t

val to_binary : string -> t -> t
