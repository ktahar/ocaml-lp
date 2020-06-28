(** Module for LP variable.
    Typically, users don't need to use this module directly.
    See higher-level module for polynomials ({!module:Poly}) instead.
*)

type attr = Continuous of float * float | General of float * float | Binary

type t = {name: string; attr: attr}

val validate_name : string -> bool

val compare_name : t -> t -> int

val collision : t -> t -> bool

val make : ?integer:bool -> ?lb:float -> ?ub:float -> string -> t

val make_binary : string -> t

val range : ?integer:bool -> ?lb:float -> ?ub:float -> string -> int -> t array

val to_string : t -> string

val to_bound : t -> float * float

val to_bound_string : ?short:bool -> t -> string option

val with_bound : float -> float -> t -> t

val to_binary : t -> t

val to_integer : t -> t
