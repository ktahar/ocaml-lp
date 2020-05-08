(* solve LP and MILP using glpk *)

open Glp
module C = Ctypes
module Cnstr = Constraint

let check_class p =
  match Problem.classify p with
  | Problem.Pclass.LP | Problem.Pclass.MILP ->
      true
  | _ ->
      (* quadratic problems *)
      false

(* NOTE on array indexing
 * glpk's API treats Carray as 1-origin! Conventions in C are:
 * - declare a[n+1] instead of a[n] for an array of length n.
 * - ignore zero-th element a[0].
 * *)

(* get 1-origin index var in vars (Var.t list) *)
let rec idx_var (v : Var.t) = function
  | [] ->
      failwith (Printf.sprintf "cannot find %s in vars" v.name)
  | hd :: rest ->
      if hd = v then 1 else 1 + idx_var v rest

let set_obj prob vars obj =
  let coeff =
    List.iter (fun t ->
        match t with
        | Term.Linear (c, v) ->
            set_obj_coef prob (idx_var v vars) c
        | _ ->
            failwith "Glpk.set_obj: nonlinear term in objective")
  in
  match obj with
  | Objective.Max poly ->
      set_obj_dir prob Dir.MAX ; coeff poly
  | Objective.Min poly ->
      set_obj_dir prob Dir.MIN ; coeff poly

let set_cnstr prob vars i cnstr =
  let ri = i + 1 in
  let idx_of_term vars = function
    | Term.Linear (_, v) ->
        idx_var v vars
    | _ ->
        failwith "Glpk.set_cnstr: nonlinear term in constraint"
  in
  let c_of_term = function
    | Term.Linear (c, _) ->
        c
    | _ ->
        failwith "Glpk.set_cnstr: nonlinear term in constraint"
  in
  let take_const = function
    | [] ->
        0.0
    | [Term.Const c] ->
        c
    | _ ->
        failwith "Glpk.set_cnstr: rhs must be constant"
  in
  let coeff poly =
    let aindices = C.CArray.make C.int (1 + List.length poly) in
    let acoeffs = C.CArray.make C.double (1 + List.length poly) in
    let () =
      List.iteri
        (fun i v -> C.CArray.set aindices i v)
        (0 :: List.map (idx_of_term vars) poly)
    in
    let () =
      List.iteri
        (fun i v -> C.CArray.set acoeffs i v)
        (0.0 :: List.map c_of_term poly)
    in
    set_mat_row prob ri (List.length poly)
      (C.to_voidp (C.CArray.start aindices))
      (C.to_voidp (C.CArray.start acoeffs))
  in
  match cnstr with
  | Cnstr.Eq (_, lhs, rhs) ->
      set_row_bnds prob ri Bnd.FX (take_const rhs) 0.0 ;
      coeff lhs
  | Cnstr.Ineq (_, lhs, rhs) ->
      set_row_bnds prob ri Bnd.UP 0.0 (take_const rhs) ;
      coeff lhs

let set_cnstrs prob vars = List.iteri (set_cnstr prob vars)

let solve_simplex p =
  let obj = fst p in
  let cnstrs = snd p in
  let vars = Problem.uniq_vars p in
  let nrows = List.length cnstrs in
  let ncols = List.length vars in
  let prob = create_prob () in
  let smcp = C.make Smcp.t in
  let () = init_smcp (C.addr smcp) in
  (* TODO set solver parameters *)
  let _ = add_rows prob nrows in
  let _ = add_cols prob ncols in
  let () = set_obj prob vars obj in
  let () = set_cnstrs prob vars cnstrs in
  ()
