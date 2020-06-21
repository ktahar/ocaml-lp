type bound = {name: string; lb: float; ub: float}

type section =
  | Sobj of Objective.t
  | Scnstr of Cnstr.t list
  | Sbound of bound list
  | Sgeneral of string list
  | Sbinary of string list

type t = section list

val emit : section list -> Problem.t
