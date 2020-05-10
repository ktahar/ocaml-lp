(* export as sub modules *)
module Var = Var
module Term = Term
module Poly = Poly
module Cnstr = Constraint
module Obj = Objective
module Problem = Problem
module Pclass = Problem.Pclass

(* export polynomial builders and operators *)
let c = Poly.c

let var = Poly.var

let binary = Poly.binary

let range = Poly.range

let range2 = Poly.range2

let range3 = Poly.range3

let rangeb = Poly.rangeb

let range2b = Poly.range2b

let range3b = Poly.range3b

let rangev = Poly.rangev

let range2v = Poly.range2v

let range3v = Poly.range3v

let concat = Poly.concat

let of_float_array = Poly.of_float_array

let zero = Poly.zero

let one = Poly.one

let ( ~-- ) = Poly.( ~-- )

let ( ++ ) = Poly.( ++ )

let ( -- ) = Poly.( -- )

let expand = Poly.expand

let ( *~ ) = Poly.( *~ )

let dot = Poly.dot

let ( *@ ) = Poly.( *@ )

let div = Poly.div

let ( /~ ) = Poly.( /~ )

(* export constraint builders *)
let eq = Constraint.eq

let ( =~ ) = Constraint.( =~ )

let lt = Constraint.lt

let ( <~ ) = Constraint.( <~ )

let gt = Constraint.gt

let ( >~ ) = Constraint.( >~ )

(* export objective builders *)
let maximize = Objective.maximize

let minimize = Objective.minimize

(* model validation and classification *)
let validate = Problem.validate

let classify = Problem.classify

let vname_list = Problem.vname_list

(* IO *)
let to_string = Problem.to_string

let of_string s =
  let lexbuf = Lexing.from_string s in
  Lpfile.emit (Parser.sections Lexer.token lexbuf)

let read file =
  let ch = open_in file in
  try
    let lexbuf = Lexing.from_channel ch in
    let problem = Lpfile.emit (Parser.sections Lexer.token lexbuf) in
    close_in ch ; problem
  with e -> close_in_noerr ch ; raise e

let write ?(short = false) file problem =
  let ch = open_out file in
  Printf.fprintf ch "%s\n" (Problem.to_string ~short problem) ;
  close_out ch
