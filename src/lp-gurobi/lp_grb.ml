(* Raw ctypes binding to gurobi's C API *)

open Ctypes
open Foreign

exception Gurobi_error of string

module Vt = struct
  type t = CONTINUOUS | BINARY | INTEGER | SEMICONT | SEMIINT

  let of_char = function
    | 'C' ->
        CONTINUOUS
    | 'B' ->
        BINARY
    | 'I' ->
        INTEGER
    | 'S' ->
        SEMICONT
    | 'N' ->
        SEMIINT
    | _ ->
        failwith "Unexpected Vtype flag"

  let to_char = function
    | CONTINUOUS ->
        'C'
    | BINARY ->
        'B'
    | INTEGER ->
        'I'
    | SEMICONT ->
        'S'
    | SEMIINT ->
        'N'

  let t = view ~read:of_char ~write:to_char char
end

(** constraint sense *)
module Cs = struct
  type t = LT | GT | EQ

  let of_char = function
    | '<' ->
        LT
    | '>' ->
        GT
    | '=' ->
        EQ
    | _ ->
        failwith "Unexpected Cs flag"

  let to_char = function LT -> '<' | GT -> '>' | EQ -> '='

  let t = view ~read:of_char ~write:to_char char
end

module Stat = struct
  type t = LOADED | OPTIMAL | INFEASIBLE | INF_OR_UNBD | UNBOUNDED | OTHER

  let of_int = function
    | 1 ->
        LOADED
    | 2 ->
        OPTIMAL
    | 3 ->
        INFEASIBLE
    | 4 ->
        INF_OR_UNBD
    | 5 ->
        UNBOUNDED
    | _ ->
        OTHER

  (* one of the limits is exceded *)
end

type env = unit ptr

let env : env typ = ptr void

type model = unit ptr

let model : model typ = ptr void

let get_error_msg = foreign "GRBgeterrormsg" (env @-> returning string)

let check env ret_code =
  if ret_code = 0 then () else raise (Gurobi_error (get_error_msg env))

let _empty_env = foreign "GRBemptyenv" (ptr env @-> returning int)

let empty_env penv =
  let ret_code = _empty_env penv in
  if ret_code = 0 then ()
    (* cannot use get_error_msg because env is not created yet *)
  else raise (Gurobi_error ("GRBemptyenv returned " ^ string_of_int ret_code))

let _start_env = foreign "GRBstartenv" (env @-> returning int)

let start_env e = check e (_start_env e)

let free_env = foreign "GRBfreeenv" (env @-> returning void)

let new_model =
  foreign "GRBnewmodel"
    ( env @-> ptr model @-> string @-> int @-> ptr void @-> ptr void
    @-> ptr void @-> ptr void @-> ptr void @-> returning int )

let free_model = foreign "GRBfreemodel" (model @-> returning void)

let set_int_attr =
  foreign "GRBsetintattr" (model @-> string @-> int @-> returning int)

let get_int_attr =
  foreign "GRBgetintattr" (model @-> string @-> ptr int @-> returning int)

let set_minimze model = set_int_attr model "ModelSense" 1

let set_maximize model = set_int_attr model "ModelSense" (-1)

let update_model = foreign "GRBupdatemodel" (model @-> returning int)

let optimize = foreign "GRBoptimize" (model @-> returning int)

let add_var =
  foreign "GRBaddvar"
    ( model @-> int @-> ptr int @-> ptr double @-> double @-> double @-> double
    @-> Vt.t @-> string @-> returning int )

let add_constr =
  foreign "GRBaddconstr"
    ( model @-> int @-> ptr int @-> ptr double @-> Cs.t @-> double @-> string
    @-> returning int )

(** quadratic obj and constraints *)
let add_qpterms =
  foreign "GRBaddqpterms"
    (model @-> int @-> ptr int @-> ptr int @-> ptr double @-> returning int)

let add_qconstr =
  foreign "GRBaddqconstr"
    ( model @-> int @-> ptr int @-> ptr double @-> int @-> ptr int @-> ptr int
    @-> ptr double @-> Cs.t @-> double @-> string @-> returning int )

let get_status model =
  let i = allocate int 0 in
  let _ = get_int_attr model "Status" i in
  Stat.of_int !@i

let get_dbl_attr =
  foreign "GRBgetdblattr" (model @-> string @-> ptr double @-> returning int)

let get_obj_val model =
  let v = allocate double 0.0 in
  let _ = get_dbl_attr model "ObjVal" v in
  !@v

let get_dbl_attr_array =
  foreign "GRBgetdblattrarray"
    (model @-> string @-> int @-> int @-> ptr double @-> returning int)

let get_obj_x model length =
  let xs = CArray.make double length in
  let _ = get_dbl_attr_array model "X" 0 length (CArray.start xs) in
  CArray.to_list xs
