type t = Term.t list

type classified = {quad: t; linear: t; const: t}

val sort : t -> t

val to_string : ?short:bool -> t -> string

(* split terms into pair ( quad or linear, const ) *)
val split : t -> t * t

(* classify terms into three categories {quad, linear, const} *)
val classify : t -> classified

val collision : t -> bool

val simplify : t -> t

val has_quad : t -> bool

val take_vars : t -> Var.t list

val neg : t -> t

val ( ~- ) : t -> t

val trans_bound : string -> float -> float -> t -> t

val to_integer : string -> t -> t

val to_binary : string -> t -> t
