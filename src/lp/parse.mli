val process : Lexing.lexbuf -> Lpfile.t option

val emit : Lexing.lexbuf -> Objective.t * Cnstr.t list

val from_string : string -> Objective.t * Cnstr.t list

val from_file : string -> Objective.t * Cnstr.t list
