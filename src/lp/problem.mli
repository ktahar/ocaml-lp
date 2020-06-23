(** Module for the optimization problem (model). *)

(** Module for the optimization problem class. *)
module Pclass : sig
  (** Optimization problem classes.
    - LP: Linear
    - QP: Quadratic
    - QCP: Quadratically Constrained; aka QCQP
    - MILP: Mixed Integer Linear
    - MIQP: Mixed Integer Quadratic
    - MIQCP: Mixed Integer Quadratically Constrained
  *)
  type t = LP | QP | QCP | MILP | MIQP | MIQCP

  val to_string : t -> string
  (** Express the problem class in string. *)
end

(** Type for an optimization problem (model). *)
type t = Objective.t * Cnstr.t list

val take_vars : t -> Var.t list
(** Make list of the variables in the problem. *)

val uniq_vars : t -> Var.t list
(** Make list of variables with unique names in the problem. *)

val uniq_vars_struct : t -> Var.t list
(** Make list of variables with unique structure (name and attributes) in the problem. *)

val collision : t -> bool
(** Check whether a collision (a variable with same name but different attributes)
    exists in the problem.
*)

val vname_list : t -> string list
(** Make list of (unique) variable names in the problem. *)

val classify : t -> Pclass.t
(** Classify the problem into {!type:Pclass.t}. *)

val validate : t -> bool
(** Validate the problem. [true] (false) means the problem is valid (invalid). *)

val to_string : ?short:bool -> t -> string
(** Express the problem in LP file format string. *)
