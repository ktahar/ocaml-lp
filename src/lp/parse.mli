val process : Lexing.lexbuf -> Lpfile.t option

val emit : Lexing.lexbuf -> Problem.t

val from_string : string -> Problem.t

val from_file : string -> Problem.t
