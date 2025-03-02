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

  let to_string = function
    | LOADED ->
        "loaded"
    | OPTIMAL ->
        "optimal"
    | INFEASIBLE ->
        "infeasible"
    | INF_OR_UNBD ->
        "infeasible or unbounded"
    | UNBOUNDED ->
        "unbounded"
    | OTHER ->
        "unknown status"
end

type env = unit ptr

let env : env typ = ptr void

type model = unit ptr

let model : model typ = ptr void

let get_error_msg = foreign "GRBgeterrormsg" (env @-> returning string)

let check env ret_code =
  if ret_code = 0 then ()
  else
    raise (Gurobi_error (Printf.sprintf "%s (%d)" (get_error_msg env) ret_code))

let _empty_env = foreign "GRBemptyenv" (ptr env @-> returning int)

let _start_env = foreign "GRBstartenv" (env @-> returning int)

let start_env env = check env (_start_env env)

let empty_env ?(start = true) () =
  let penv = allocate env null in
  let ret_code = _empty_env penv in
  if ret_code = 0 then (
    if start then start_env !@penv else () ;
    !@penv )
  else
    (* cannot use get_error_msg because env is not created yet *)
    raise (Gurobi_error ("GRBemptyenv returned " ^ string_of_int ret_code))

(* no error code *)
let free_env = foreign "GRBfreeenv" (env @-> returning void)

let _set_int_param =
  foreign "GRBsetintparam" (env @-> string @-> int @-> returning int)

let set_int_param env name value = check env (_set_int_param env name value)

let set_term_output env output =
  let code = if output then 1 else 0 in
  set_int_param env "OutputFlag" code

let _new_model =
  foreign "GRBnewmodel"
    ( env @-> ptr model @-> string @-> int @-> ptr double @-> ptr double
    @-> ptr double @-> ptr char @-> ptr string @-> returning int )

let new_model env name objs lbs ubs vtypes vnames =
  let pmodel = allocate model null in
  let nvars = List.length objs in
  if
    List.exists
      (fun len -> len <> nvars)
      [List.length lbs; List.length ubs; List.length vtypes; List.length vnames]
  then raise (Gurobi_error "new_model: list lengths must be equal")
  else
    let aobjs = CArray.of_list double objs in
    let albs = CArray.of_list double lbs in
    let aubs = CArray.of_list double ubs in
    let avtypes = CArray.of_list char (List.map Vt.to_char vtypes) in
    let avnames = CArray.of_list string vnames in
    check env
      (_new_model env pmodel name nvars (CArray.start aobjs) (CArray.start albs)
         (CArray.start aubs) (CArray.start avtypes) (CArray.start avnames) ) ;
    !@pmodel

let _free_model = foreign "GRBfreemodel" (model @-> returning int)

let free_model env model = check env (_free_model model)

let _set_int_attr =
  foreign "GRBsetintattr" (model @-> string @-> int @-> returning int)

let set_int_attr env model name value =
  check env (_set_int_attr model name value)

let _get_int_attr =
  foreign "GRBgetintattr" (model @-> string @-> ptr int @-> returning int)

let get_int_attr env model name =
  let value = allocate int 0 in
  check env (_get_int_attr model name value) ;
  !@value

let set_minimize env model = set_int_attr env model "ModelSense" 1

let set_maximize env model = set_int_attr env model "ModelSense" (-1)

let _update_model = foreign "GRBupdatemodel" (model @-> returning int)

let update_model env model = check env (_update_model model)

let _optimize = foreign "GRBoptimize" (model @-> returning int)

let optimize env model = check env (_optimize model)

let _add_var =
  foreign "GRBaddvar"
    ( model @-> int @-> ptr int @-> ptr double @-> double @-> double @-> double
    @-> Vt.t @-> string @-> returning int )

let add_var env model inds vals obj lb ub vtype vname =
  let numnz = List.length inds in
  if numnz <> List.length vals then
    raise (Gurobi_error "add_var: list lengths must be equal")
  else
    let ainds = CArray.of_list int inds in
    let avals = CArray.of_list double vals in
    check env
      (_add_var model numnz (CArray.start ainds) (CArray.start avals) obj lb ub
         vtype vname )

let _add_constr =
  foreign "GRBaddconstr"
    ( model @-> int @-> ptr int @-> ptr double @-> Cs.t @-> double @-> string
    @-> returning int )

let add_constr env model cinds cvals sense rhs cname =
  let numnz = List.length cinds in
  if numnz <> List.length cvals then
    raise (Gurobi_error "add_constr: list lengths must be equal")
  else
    let ainds = CArray.of_list int cinds in
    let avals = CArray.of_list double cvals in
    check env
      (_add_constr model numnz (CArray.start ainds) (CArray.start avals) sense
         rhs cname )

(* quadratic obj and constraints *)

let _add_qpterms =
  foreign "GRBaddqpterms"
    (model @-> int @-> ptr int @-> ptr int @-> ptr double @-> returning int)

let add_qpterms env model rows cols vals =
  let numqnz = List.length vals in
  if List.exists (fun len -> len <> numqnz) (List.map List.length [rows; cols])
  then raise (Gurobi_error "new_model: list lengths must be equal")
  else
    let arows = CArray.of_list int rows in
    let acols = CArray.of_list int cols in
    let avals = CArray.of_list double vals in
    check env
      (_add_qpterms model numqnz (CArray.start arows) (CArray.start acols)
         (CArray.start avals) )

let _add_qconstr =
  foreign "GRBaddqconstr"
    ( model @-> int @-> ptr int @-> ptr double @-> int @-> ptr int @-> ptr int
    @-> ptr double @-> Cs.t @-> double @-> string @-> returning int )

let add_qconstr env model linds lvals qrows qcols qvals sense rhs cname =
  let numlnz = List.length lvals in
  let numqnz = List.length qvals in
  if numlnz <> List.length linds then
    raise (Gurobi_error "add_qconstr: list lengths must be equal (linears)")
  else if
    List.exists (fun len -> len <> numqnz) (List.map List.length [qrows; qcols])
  then raise (Gurobi_error "add_qconstr: list lengths must be equal (quads)")
  else
    let alinds = CArray.of_list int linds in
    let alvals = CArray.of_list double lvals in
    let aqrows = CArray.of_list int qrows in
    let aqcols = CArray.of_list int qcols in
    let aqvals = CArray.of_list double qvals in
    check env
      (_add_qconstr model numlnz (CArray.start alinds) (CArray.start alvals)
         numqnz (CArray.start aqrows) (CArray.start aqcols)
         (CArray.start aqvals) sense rhs cname )

let get_status env model = Stat.of_int (get_int_attr env model "Status")

let _get_dbl_attr =
  foreign "GRBgetdblattr" (model @-> string @-> ptr double @-> returning int)

let get_dbl_attr env model name =
  let value = allocate double 0.0 in
  check env (_get_dbl_attr model name value) ;
  !@value

let get_obj_val env model = get_dbl_attr env model "ObjVal"

let _get_dbl_attr_array =
  foreign "GRBgetdblattrarray"
    (model @-> string @-> int @-> int @-> ptr double @-> returning int)

let get_dbl_attr_array env model name start length =
  let xs = CArray.make double length in
  check env (_get_dbl_attr_array model name start length (CArray.start xs)) ;
  CArray.to_list xs

let get_obj_x env model length = get_dbl_attr_array env model "X" 0 length

let _write = foreign "GRBwrite" (model @-> string @-> returning int)

let write env model fname = check env (_write model fname)
