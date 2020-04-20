(* export as sub modules *)
module Var = Var
module Term = Term
module Poly = Poly
module Cnstr = Constraint
module Obj = Objective
module Problem = Problem

(* export polynomial making functions and operators *)
let c = Poly.c

let var = Poly.var

let binary = Poly.binary

let ( ~- ) = Poly.( ~- )

let ( + ) = Poly.( + )

let ( - ) = Poly.( - )

let ( * ) = Poly.( * )

let dot = Poly.dot

let ( *@ ) = Poly.( *@ )

(* export constraint making functions and operators *)
let ( =$ ) = Constraint.( =$ )

let ( <$ ) = Constraint.( <$ )

let ( >$ ) = Constraint.( >$ )

(* export objective making functions *)
let maximize = Objective.maximize

let minimize = Objective.minimize

(* model validation *)
let validate = Problem.validate

(* IO *)
let to_string = Problem.to_string

let of_string s =
  let lexbuf = Lexing.from_string s in
  Lpfile.emit (Parser.sections Lexer.token lexbuf)

let write file problem =
  let ch = open_out file in
  Printf.fprintf ch "%s\n" (Problem.to_string problem) ;
  close_out ch

let load file =
  let ch = open_in file in
  try
    let lexbuf = Lexing.from_channel ch in
    let problem = Lpfile.emit (Parser.sections Lexer.token lexbuf) in
    close_in ch ; problem
  with e -> close_in_noerr ch ; raise e
