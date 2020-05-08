open Ctypes
open Foreign

let libglpk = Dl.dlopen ~filename:"libglpk.so" ~flags:[Dl.RTLD_LAZY]

type prob = unit ptr

let prob : prob typ = ptr void

let ff = foreign ~from:libglpk

let create_prob = ff "glp_create_prob" (void @-> returning prob)

let set_prob_name = ff "glp_set_prob_name" (prob @-> string @-> returning void)

let get_prob_name = ff "glp_get_prob_name" (prob @-> returning string)
