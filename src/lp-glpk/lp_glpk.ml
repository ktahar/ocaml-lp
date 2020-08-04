(* solve LP and MILP using glpk *)

module C = Ctypes
open Lp_glp
open Lp

let make_pmap vars f =
  List.fold_left
    (fun m (k, v) -> Lp.PMap.add k v m)
    Lp.PMap.empty
    (List.mapi (fun i v -> (Poly.of_var v, f i)) vars)

(* NOTE on array indexing
 * glpk's API treats Carray as 1-origin! Conventions in C are:
 * - declare a[n+1] instead of a[n] for an array of length n.
 * - ignore zero-th element a[0]. *)

(* get 1-origin index of v in vars (Var.t list) *)
let rec idx_var (v : Var.t) = function
  | [] ->
      failwith (Printf.sprintf "cannot find %s in vars" v.name)
  | hd :: rest ->
      if hd = v then 1 else 1 + idx_var v rest

let set_obj prob vars obj =
  if Objective.is_max obj then set_obj_dir prob Dir.MAX
  else set_obj_dir prob Dir.MIN ;
  Poly.iter_linear_exn
    (fun c v -> set_obj_coef prob (idx_var v vars) c)
    (Objective.to_poly obj)

let set_cnstr prob vars i cnstr =
  let ri = i + 1 in
  let coeff poly =
    let aindices = C.CArray.make C.int (1 + Poly.length poly) in
    let acoeffs = C.CArray.make C.double (1 + Poly.length poly) in
    let () =
      List.iteri
        (fun i v -> C.CArray.set aindices i v)
        (0 :: Poly.map_linear (fun _ v -> idx_var v vars) poly)
      (* 0-th element is dummy *)
    in
    let () =
      List.iteri
        (fun i v -> C.CArray.set acoeffs i v)
        (0.0 :: Poly.take_linear_coeffs poly)
      (* 0-th element is dummy *)
    in
    set_mat_row prob ri (Poly.length poly)
      (C.to_voidp (C.CArray.start aindices))
      (C.to_voidp (C.CArray.start acoeffs))
  in
  let lhs, rhs = Cnstr.sides cnstr in
  if Cnstr.is_eq cnstr then set_row_bnds prob ri Bnd.FX rhs 0.0
  else set_row_bnds prob ri Bnd.UP 0.0 rhs ;
  coeff lhs

let set_cnstrs prob vars = List.iteri (set_cnstr prob vars)

module Simplex = struct
  let set_cols prob =
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
            failwith "set_cols: integer variable found")

  let solve_main p =
    let obj, cnstrs = Problem.obj_cnstrs p in
    let vars = Problem.uniq_vars p in
    let nrows = List.length cnstrs in
    let ncols = List.length vars in
    let prob = create_prob () in
    ignore @@ add_rows prob nrows ;
    ignore @@ add_cols prob ncols ;
    let smcp = C.make Smcp.t in
    try
      init_smcp (C.addr smcp) ;
      (* TODO set solver parameters *)
      set_obj prob vars obj ;
      set_cnstrs prob vars cnstrs ;
      set_cols prob vars ;
      let ret = simplex prob (C.addr smcp) in
      (* TODO handle some of non-zero return values *)
      if ret <> 0 then failwith "non-zero return value from simplex"
      else
        match get_status prob with
        | Stat.OPT ->
            let obj = get_obj_val prob in
            let xs = make_pmap vars (fun i -> get_col_prim prob (i + 1)) in
            delete_prob prob ;
            Ok (obj, xs)
        | status ->
            failwith ("Problem is " ^ Stat.to_string status)
    with Failure msg -> delete_prob prob ; Error msg

  let solve ?(term_output = true) p =
    match Problem.classify p with
    | Pclass.LP ->
        set_term_out term_output ; solve_main p
    | _ ->
        Error "Lp_glpk.Simplex is only for LP"
end

module Milp = struct
  let set_cols prob =
    let set_bounds cj lb ub =
      if lb = Float.neg_infinity && ub = Float.infinity then
        set_col_bnds prob cj Bnd.FR 0.0 0.0
      else if ub = Float.infinity then set_col_bnds prob cj Bnd.LO lb 0.0
      else if lb = Float.neg_infinity then set_col_bnds prob cj Bnd.UP 0.0 ub
      else if lb <> ub then set_col_bnds prob cj Bnd.DB lb ub
      else set_col_bnds prob cj Bnd.FX lb ub
    in
    List.iteri (fun j var ->
        let cj = 1 + j in
        match var with
        | {Var.attr= Var.Continuous (lb, ub); _} ->
            set_bounds cj lb ub
        | {Var.attr= Var.General (lb, ub); _} ->
            set_bounds cj lb ub ; set_col_kind prob cj Vt.IV
        | {Var.attr= Var.Binary; _} ->
            set_bounds cj Float.zero Float.one ;
            set_col_kind prob cj Vt.BV)

  let solve_main p =
    let obj, cnstrs = Problem.obj_cnstrs p in
    let vars = Problem.uniq_vars p in
    let nrows = List.length cnstrs in
    let ncols = List.length vars in
    let prob = create_prob () in
    ignore @@ add_rows prob nrows ;
    ignore @@ add_cols prob ncols ;
    let smcp = C.make Smcp.t in
    let iocp = C.make Iocp.t in
    try
      init_smcp (C.addr smcp) ;
      init_iocp (C.addr iocp) ;
      (* TODO set solver parameters *)
      set_obj prob vars obj ;
      set_cnstrs prob vars cnstrs ;
      set_cols prob vars ;
      let ret = simplex prob (C.addr smcp) in
      (* TODO handle some of non-zero return values *)
      if ret <> 0 then failwith "non-zero return value from simplex"
      else
        match get_status prob with
        | Stat.OPT -> (
            let ret = intopt prob (C.addr iocp) in
            (* TODO handle some of non-zero return values *)
            if ret <> 0 then failwith "non-zero return value from intopt"
            else
              match mip_status prob with
              | Stat.OPT ->
                  let obj = mip_obj_val prob in
                  let xs = make_pmap vars (fun i -> mip_col_val prob (i + 1)) in
                  delete_prob prob ;
                  Ok (obj, xs)
              | status ->
                  failwith ("MILP is " ^ Stat.to_string status) )
        | status ->
            failwith ("LP relaxation is " ^ Stat.to_string status)
    with Failure msg -> delete_prob prob ; Error msg

  let solve ?(term_output = true) p =
    match Problem.classify p with
    | Pclass.MILP ->
        set_term_out term_output ; solve_main p
    | _ ->
        Error "Lp_glpk.Milp is only for MILP"
end

let solve ?(term_output = true) p =
  match Problem.classify p with
  | Pclass.LP ->
      Simplex.solve ~term_output p
  | Pclass.MILP ->
      Milp.solve ~term_output p
  | _ ->
      Error "glpk is only for LP or MILP"
