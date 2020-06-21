module Pclass : sig
  type t = LP | QP | QCP | MILP | MIQP | MIQCP

  val to_string : t -> string
end

type t = Objective.t * Cnstr.t list

val take_vars : t -> Var.t list

val uniq_vars : t -> Var.t list

val uniq_vars_struct : t -> Var.t list

val collision : t -> bool

val vname_list : t -> string list

val classify : t -> Pclass.t

val validate : t -> bool

val to_string : ?short:bool -> t -> string
