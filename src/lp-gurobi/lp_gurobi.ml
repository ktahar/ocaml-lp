(* solve MIP using Gurobi *)

module C = Ctypes
open Lp
open Lp_grb

module Var_attrs = struct
  type t =
    { objs: float list
    ; lbs: float list
    ; ubs: float list
    ; types: Vt.t list
    ; names: string list }

  let vtype v =
    let open Var in
    match v.attr with
    | Continuous _ ->
        Vt.CONTINUOUS
    | General _ ->
        Vt.INTEGER
    | Binary ->
        Vt.BINARY

  let make objective vars =
    let obj = List.map (Poly.linear_coeff (Obj.to_poly objective)) vars in
    { objs= obj
    ; lbs= List.map (fun v -> fst (Var.to_bound v)) vars
    ; ubs= List.map (fun v -> snd (Var.to_bound v)) vars
    ; types= List.map vtype vars
    ; names= List.map Var.to_string vars }
end

(* get 0-origin index of v in vars (Var.t list) *)
let rec idx_var (v : Var.t) = function
  | [] ->
      failwith (Printf.sprintf "cannot find %s in vars" v.name)
  | hd :: rest ->
      if hd = v then 0 else 1 + idx_var v rest

let idx vars = List.map (fun v -> idx_var v vars)

module Constr = struct
  type t =
    { linds: int list
    ; lvals: float list
    ; qrows: int list
    ; qcols: int list
    ; qvals: float list
    ; sense: Cs.t
    ; rhs: float
    ; cname: string }

  let to_sense = function Cnstr.Eq _ -> Cs.EQ | Cnstr.Ineq _ -> Cs.LT

  let of_cnstr vars cnstr =
    let dec = Poly.decompose (Cnstr.lhs cnstr) in
    let rhs = Poly.to_float (Cnstr.rhs cnstr) in
    let open Poly in
    { linds= idx vars dec.lvs
    ; lvals= dec.lcs
    ; qrows= idx vars dec.qv0s
    ; qcols= idx vars dec.qv1s
    ; qvals= dec.qcs
    ; sense= to_sense cnstr
    ; rhs
    ; cname= Cnstr.name cnstr }
end

let set_dir env model = function
  | Obj.Max _ ->
      set_maximize env model
  | Obj.Min _ ->
      set_minimize env model

let add_obj_qterms env model vars dobj =
  let open Lp.Poly in
  match dobj.qcs with
  | [] ->
      ()
  | _ ->
      add_qpterms env model (idx vars dobj.qv0s) (idx vars dobj.qv1s) dobj.qcs

let add_constraints env model vars =
  List.iter (fun c ->
      let cr = Constr.of_cnstr vars c in
      match cr.qvals with
      | [] ->
          add_constr env model cr.linds cr.lvals cr.sense cr.rhs cr.cname
      | _ ->
          add_qconstr env model cr.linds cr.lvals cr.qrows cr.qcols cr.qvals
            cr.sense cr.rhs cr.cname)

let solve ?(write_fname = "") problem =
  let obj = fst problem in
  let cnstrs = snd problem in
  let dobj = Poly.decompose (Obj.to_poly obj) in
  let vars = Problem.uniq_vars problem in
  let vattr = Var_attrs.make obj vars in
  let env = empty_env () in
  try
    let model =
      new_model env "model" vattr.objs vattr.lbs vattr.ubs vattr.types
        vattr.names
    in
    try
      set_dir env model obj ;
      add_obj_qterms env model vars dobj ;
      add_constraints env model vars cnstrs ;
      update_model env model ;
      if String.length write_fname > 0 then write env model write_fname else () ;
      optimize env model ;
      match get_status env model with
      | OPTIMAL ->
          let oval = get_obj_val env model in
          let len = List.length vars in
          let tbl = Hashtbl.create len in
          List.iter2
            (fun var value -> Hashtbl.add tbl (Poly.of_var var) value)
            vars (get_obj_x env model len) ;
          free_model env model ;
          free_env env ;
          Ok (oval, tbl)
      | stat ->
          failwith ("Problem is " ^ Stat.to_string stat)
    with e -> free_model env model ; raise e
  with
  | Gurobi_error msg | Failure msg ->
      free_env env ; Error msg
  | e ->
      (* unexpected exception *)
      free_env env ; raise e
