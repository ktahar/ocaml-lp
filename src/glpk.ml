(* solve LP and MILP using glpk *)

open Glp
module C = Ctypes
module Cnstr = Constraint

(* NOTE on array indexing
 * glpk's API treats Carray as 1-origin! Conventions in C are:
 * - declare a[n+1] instead of a[n] for an array of length n.
 * - ignore zero-th element a[0].
 * *)

module Simplex = struct
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
              failwith "set_obj: nonlinear term in objective")
    in
    match obj with
    | Objective.Max poly ->
        set_obj_dir prob Dir.MAX ; coeff poly
    | Objective.Min poly ->
        set_obj_dir prob Dir.MIN ; coeff poly

  let set_cnstr prob vars i cnstr =
    let ri = i + 1 in
    let idx_of_term = function
      | Term.Linear (_, v) ->
          idx_var v vars
      | _ ->
          failwith "set_cnstr: nonlinear term in constraint"
    in
    let c_of_term = function
      | Term.Linear (c, _) ->
          c
      | _ ->
          failwith "set_cnstr: nonlinear term in constraint"
    in
    let take_const = function
      | [] ->
          0.0
      | [Term.Const c] ->
          c
      | _ ->
          failwith "set_cnstr: rhs must be constant"
    in
    let coeff poly =
      let aindices = C.CArray.make C.int (1 + List.length poly) in
      let acoeffs = C.CArray.make C.double (1 + List.length poly) in
      let () =
        List.iteri
          (fun i v -> C.CArray.set aindices i v)
          (0 :: List.map idx_of_term poly)
        (* 0-th element is dummy *)
      in
      let () =
        List.iteri
          (fun i v -> C.CArray.set acoeffs i v)
          (0.0 :: List.map c_of_term poly)
        (* 0-th element is dummy *)
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

  let set_bounds prob =
    List.iteri (fun j var ->
        let cj = 1 + j in
        match var with
        | {Var.attr= Var.Continuous (lb, ub); _} ->
            if lb = Float.neg_infinity && ub = Float.infinity then
              set_col_bnds prob cj Bnd.FR 0.0 0.0
            else if ub = Float.infinity then set_col_bnds prob cj Bnd.LO lb 0.0
            else if lb = Float.neg_infinity then
              set_col_bnds prob cj Bnd.UP 0.0 ub
            else if lb <> ub then set_col_bnds prob cj Bnd.DB lb ub
            else set_col_bnds prob cj Bnd.FX lb ub
        | _ ->
            failwith "set_bounds: integer variable found")

  let solve_main p =
    let obj = fst p in
    let cnstrs = snd p in
    let vars = Problem.uniq_vars p in
    let nrows = List.length cnstrs in
    let ncols = List.length vars in
    let prob = create_prob () in
    let _ = add_rows prob nrows in
    let _ = add_cols prob ncols in
    let smcp = C.make Smcp.t in
    try
      init_smcp (C.addr smcp) ;
      (* TODO set solver parameters *)
      set_obj prob vars obj ;
      set_cnstrs prob vars cnstrs ;
      set_bounds prob vars ;
      let ret = simplex prob (C.addr smcp) in
      if ret <> 0 then failwith "non-zero return value"
      else
        match get_status prob with
        | Stat.OPT ->
            let oval = get_obj_val prob in
            let tbl = Hashtbl.create ncols in
            List.iteri
              (fun j v -> Hashtbl.add tbl v (get_col_prim prob (1 + j)))
              vars ;
            delete_prob prob ;
            Ok (oval, tbl)
        | status ->
            failwith ("Problem is " ^ Stat.to_string status)
    with Failure msg -> delete_prob prob ; Error msg

  let check_class p =
    match Problem.classify p with Problem.Pclass.LP -> true | _ -> false

  let solve p =
    if not (check_class p) then Error "simplex is only for LP" else solve_main p
end
