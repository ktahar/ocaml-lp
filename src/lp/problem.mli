module Pclass : sig
  type t = LP | QP | QCP | MILP | MIQP | MIQCP

  val to_string : t -> string
end

type t = Objective.t * Cnstr.t list

val take_vars : Objective.t * Cnstr.t list -> Var.t list

val uniq_vars : Objective.t * Cnstr.t list -> Var.t list

val uniq_vars_struct : Objective.t * Cnstr.t list -> Var.t list

val collision : Objective.t * Cnstr.t list -> bool

val vname_list : Objective.t * Cnstr.t list -> string list

val classify : Objective.t * Cnstr.t list -> Pclass.t

val validate : Objective.t * Cnstr.t list -> bool

val to_string : ?short:bool -> Objective.t * Cnstr.t list -> string
