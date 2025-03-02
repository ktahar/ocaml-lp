open Js_of_ocaml

class type var = object
  method name : Js.js_string Js.t Js.prop
  method coef : float Js.prop
end

class type row_bound = object
  method _type : int Js.prop
  method ub : float Js.prop
  method lb : float Js.prop
end

class type col_bound = object
  method name : Js.js_string Js.t Js.prop
  method _type : int Js.prop
  method ub : float Js.prop
  method lb : float Js.prop
end

class type objective = object
  method direction : int Js.prop
  method name : Js.js_string Js.t Js.prop
  method vars : var Js.t Js.js_array Js.t Js.prop
end

class type options = object
  method mipgap : float Js.optdef Js.prop
  method tmlim : int Js.optdef Js.prop
  method msglev : int Js.optdef Js.prop
  method presol : bool Js.t Js.optdef Js.prop
  (* cb is TODO *)
  (* method cb *)
end

class type cnstr = object
  method name : Js.js_string Js.t Js.prop
  method vars : var Js.t Js.js_array Js.t Js.prop
  method bnds : row_bound Js.t Js.prop
end

class type prob = object
  method name : Js.js_string Js.t Js.prop
  method objective : objective Js.t Js.prop
  method subjectTo : cnstr Js.t Js.js_array Js.t Js.prop
  method bounds : col_bound Js.t Js.js_array Js.t Js.optdef Js.prop
  method binaries : Js.js_string Js.t Js.js_array Js.t Js.optdef Js.prop
  method generals : Js.js_string Js.t Js.js_array Js.t Js.optdef Js.prop
  (* specified in glpk.js API. but omit here as it's redundant. *)
  (* method options: options Js.t Js.optdef Js.prop *)
end

class type res = object
  method status : int Js.prop
  method z : float Js.prop
  method vars : 'a Js.t Js.prop
  method dual : 'a Js.t Js.prop Js.optdef
end

class type result = object
  method name : Js.js_string Js.t Js.prop
  method time : float Js.prop
  method result : res Js.t Js.prop
end

class type glpk = object
  (* direction *)
  method _GLP_MIN_ : int Js.readonly_prop
  method _GLP_MAX_ : int Js.readonly_prop

  (* type of auxiliary/structural variable *)
  method _GLP_FR_ : int Js.readonly_prop
  method _GLP_LO_ : int Js.readonly_prop
  method _GLP_UP_ : int Js.readonly_prop
  method _GLP_DB_ : int Js.readonly_prop
  method _GLP_FX_ : int Js.readonly_prop

  (* message level *)
  method _GLP_MSG_OFF_ : int Js.readonly_prop
  method _GLP_MSG_ERR_ : int Js.readonly_prop
  method _GLP_MSG_ON_ : int Js.readonly_prop
  method _GLP_MSG_ALL_ : int Js.readonly_prop
  method _GLP_MSG_DBG_ : int Js.readonly_prop

  (* solution status *)
  method _GLP_UNDEF_ : int Js.readonly_prop
  method _GLP_FEAS_ : int Js.readonly_prop
  method _GLP_INFEAS_ : int Js.readonly_prop
  method _GLP_NOFEAS_ : int Js.readonly_prop
  method _GLP_OPT_ : int Js.readonly_prop
  method _GLP_UNBND_ : int Js.readonly_prop

  (* version / solve *)
  method version : Js.js_string Js.t Js.prop
  method solve : prob Js.t -> result Js.t Js.meth
  method solve_opt : prob Js.t -> options Js.t -> result Js.t Js.meth
  method solve_msglev : prob Js.t -> int -> result Js.t Js.meth
end

let require_glpk module_ident : glpk Js.t =
  let require = Js.Unsafe.pure_js_expr "require" in
  let _GLPK =
    Js.Unsafe.(fun_call require [|inject (Js.string module_ident)|])
  in
  Js.Unsafe.fun_call _GLPK [||]

let js_var c v : var Js.t =
  object%js
    val mutable name = Js.string (Lp.Var.to_string v)
    val mutable coef = c
  end

let js_objective (glpk : glpk Js.t) obj : objective Js.t =
  let vs =
    Lp.Poly.map_linear js_var (Lp.Objective.to_poly obj) |> Array.of_list
  in
  let d =
    if Lp.Objective.is_max obj then glpk##._GLP_MAX_ else glpk##._GLP_MIN_
  in
  object%js
    val mutable direction = d
    val mutable name = Js.string "objective"
    val mutable vars = Js.array vs
  end

let js_cnstr (glpk : glpk Js.t) cn : cnstr Js.t =
  let vs = Lp.Poly.map_linear js_var (Lp.Cnstr.lhs cn) |> Array.of_list in
  let bnd t l u : row_bound Js.t =
    object%js
      val mutable _type = t
      val mutable ub = u
      val mutable lb = l
    end
  in
  let rhs = Lp.Cnstr.rhs cn in
  let b =
    if Lp.Cnstr.is_eq cn then bnd glpk##._GLP_FX_ rhs 0.0
    else bnd glpk##._GLP_UP_ 0.0 rhs
  in
  object%js
    val mutable name = Js.string (Lp.Cnstr.name cn)
    val mutable vars = Js.array vs
    val mutable bnds = b
  end

let js_bound (glpk : glpk Js.t) var : col_bound Js.t =
  let lb, ub = Lp.Var.to_bound var in
  let t, l, u =
    if lb = Float.neg_infinity && ub = Float.infinity then
      (glpk##._GLP_FR_, 0.0, 0.0)
    else if ub = Float.infinity then (glpk##._GLP_LO_, lb, 0.0)
    else if lb = Float.neg_infinity then (glpk##._GLP_UP_, 0.0, ub)
    else if lb <> ub then (glpk##._GLP_DB_, lb, ub)
    else (glpk##._GLP_FX_, lb, ub)
  in
  object%js
    val mutable name = Js.string (Lp.Var.to_string var)
    val mutable _type = t
    val mutable ub = u
    val mutable lb = l
  end

let js_ints vars =
  let c = Lp.Problem.Vars.classify vars in
  let to_js vs =
    vs |> List.map (fun v -> Js.string (Lp.Var.to_string v)) |> Array.of_list
  in
  (to_js c.binary, to_js c.general)

let js_prob glpk prob : prob Js.t =
  let obj, cnstrs = Lp.Problem.obj_cnstrs prob in
  let vars = Lp.Problem.uniq_vars prob in
  let n = match Lp.Problem.name prob with Some n -> n | None -> "problem" in
  let cs = cnstrs |> List.map (fun c -> js_cnstr glpk c) |> Array.of_list in
  let bnds = vars |> List.map (fun c -> js_bound glpk c) |> Array.of_list in
  let bins, gens = js_ints vars in
  object%js
    val mutable name = Js.string n
    val mutable objective = js_objective glpk obj
    val mutable subjectTo = Js.array cs
    val mutable bounds = Js.def (Js.array bnds)
    val mutable binaries = Js.def (Js.array bins)
    val mutable generals = Js.def (Js.array gens)
  end

let make_pmap vars res =
  List.fold_left
    (fun m (k, v) -> Lp.PMap.add k v m)
    Lp.PMap.empty
    (List.map
       (fun v ->
         ( Lp.Poly.of_var v
         , Js.float_of_number (Js.Unsafe.get res (Lp.Var.to_string v)) ) )
       vars )

let status_to_str glpk i =
  if i = glpk##._GLP_UNDEF_ then "Undefined"
  else if i = glpk##._GLP_FEAS_ then "Feasible"
  else if i = glpk##._GLP_INFEAS_ then "Infeasible"
  else if i = glpk##._GLP_NOFEAS_ then "NoFeasible"
  else if i = glpk##._GLP_OPT_ then "Optimal"
  else if i = glpk##._GLP_UNBND_ then "Unbounded"
  else "Unexpected Status"

let solve ?(term_output = true) (glpk : glpk Js.t) problem =
  match Lp.Problem.classify problem with
  | Lp.Pclass.LP | Lp.Pclass.MILP ->
      let prob = js_prob glpk problem in
      (* Firebug.console##log prob ; *)
      let lev =
        if term_output then glpk##._GLP_MSG_ON_ else glpk##._GLP_MSG_OFF_
      in
      let res = glpk##solve_msglev prob lev in
      (* Firebug.console##log res ; *)
      let r = res##.result in
      let status = r##.status in
      if status = glpk##._GLP_OPT_ then
        Ok (r##.z, make_pmap (Lp.Problem.uniq_vars problem) r##.vars)
      else Error ("Problem is " ^ status_to_str glpk status)
  | _ ->
      Error "glpk.js is only for LP or MILP"
