open Ctypes
open Foreign

type prob = unit ptr

let prob : prob typ = ptr void

let create_prob = foreign "glp_create_prob" (void @-> returning prob)

let set_prob_name = foreign "glp_set_prob_name" (prob @-> string @-> returning void)

let get_prob_name = foreign "glp_get_prob_name" (prob @-> returning string)
