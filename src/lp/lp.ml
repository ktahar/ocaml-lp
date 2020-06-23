(* export as sub modules *)
module Var = Var
module Term = Term
module Poly = Poly
module Cnstr = Cnstr
module Objective = Objective
module Problem = Problem
module Pclass = Problem.Pclass
module PMap = Map.Make (Poly)

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

(* export constraint, objective, and problem builders *)
let eq = Cnstr.eq

let ( =~ ) = Cnstr.( =~ )

let lt = Cnstr.lt

let ( <~ ) = Cnstr.( <~ )

let gt = Cnstr.gt

let ( >~ ) = Cnstr.( >~ )

let maximize = Objective.maximize

let minimize = Objective.minimize

let make = Problem.make

(* model validation and classification *)
let validate = Problem.validate

let classify = Problem.classify

let vname_list = Problem.vname_list

(* IO *)
let to_string = Problem.to_string

let of_string = Parse.from_string

let read = Parse.from_file

let write ?(short = false) file problem =
  let ch = open_out file in
  Printf.fprintf ch "%s\n" (Problem.to_string ~short problem) ;
  close_out ch
