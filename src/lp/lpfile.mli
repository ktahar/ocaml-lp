type bound = {name: string; lb: float; ub: float}

type section =
  | Sobj of Objective.t
  | Scnstr of Cnstr.t list
  | Sbound of bound list
  | Sgeneral of string list
  | Sbinary of string list

type t = section list

val trans_binary :
  Objective.t * Cnstr.t list -> string -> Objective.t * Cnstr.t list

val trans_general :
  Objective.t * Cnstr.t list -> string -> Objective.t * Cnstr.t list

val trans_bound :
  Objective.t * Cnstr.t list -> bound -> Objective.t * Cnstr.t list

val trans_attrs :
  Objective.t * Cnstr.t list -> section list -> Objective.t * Cnstr.t list

val emit : section list -> Objective.t * Cnstr.t list
