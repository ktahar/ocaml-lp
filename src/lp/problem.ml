module Pclass = struct
  type t = LP | QP | QCP | MILP | MIQP | MIQCP

  let to_string = function
    | LP ->
        "LP"
    | QP ->
        "QP"
    | QCP ->
        "QCP"
    | MILP ->
        "MILP"
    | MIQP ->
        "MIQP"
    | MIQCP ->
        "MIQCP"
end

module Vars = struct
  type t = Var.t list

  type classified = {continuous: t; general: t; binary: t}

  let classify vars =
    let rec classify_ cs gs bs = function
      | [] ->
          {continuous= List.rev cs; general= List.rev gs; binary= List.rev bs}
      | ({Var.attr= Var.Continuous _; _} as c) :: rest ->
          classify_ (c :: cs) gs bs rest
      | ({Var.attr= Var.General _; _} as g) :: rest ->
          classify_ cs (g :: gs) bs rest
      | ({Var.attr= Var.Binary; _} as b) :: rest ->
          classify_ cs gs (b :: bs) rest
    in
    classify_ [] [] [] vars

  let has_integer vars =
    List.exists
      (fun x ->
        match x with {Var.attr= Var.Continuous _; _} -> false | _ -> true )
      vars

  let to_vtype_string vars =
    let to_string li = li |> List.map Var.to_string |> String.concat " " in
    let bsec b = "binary\n " ^ to_string b in
    let gsec g = "general\n " ^ to_string g in
    match classify vars with
    | {general= []; binary= []; _} ->
        None
    | {general; binary= []; _} ->
        Some (gsec general)
    | {general= []; binary; _} ->
        Some (bsec binary)
    | {general; binary; _} ->
        Some (gsec general ^ "\n" ^ bsec binary)

  let to_bound_string ?(short = false) vars =
    let v =
      vars
      |> List.filter_map (Var.to_bound_string ~short)
      |> List.map (fun s -> " " ^ s)
    in
    match v with [] -> None | vs -> Some ("bounds\n" ^ String.concat "\n" vs)
end

module Cnstrs = struct
  let to_string ?(short = false) cnstrs =
    let c_string = Cnstr.to_string ~short in
    let body =
      cnstrs |> List.map c_string
      |> List.map (fun s -> " " ^ s)
      |> String.concat "\n"
    in
    "subject to\n" ^ body

  let degree cs = cs |> List.map Cnstr.degree |> List.fold_left max 0

  let constant_error i cnstr =
    if not (Cnstr.constant cnstr) then None
    else
      let name =
        match Cnstr.name cnstr with
        | "" ->
            Printf.sprintf "constraint#%d" (i + 1)
        | n ->
            "constraint '" ^ n ^ "'"
      in
      let rhs = Cnstr.rhs cnstr in
      let eps = 10. *. epsilon_float in
      let status =
        if Cnstr.is_eq cnstr then
          if Float.abs rhs < eps then "redundant" else "infeasible"
        else if rhs >= -.eps then "redundant"
        else "infeasible"
      in
      let relation =
        if Cnstr.is_eq cnstr then Printf.sprintf "0 = %.18g" rhs
        else Printf.sprintf "0 <= %.18g" rhs
      in
      Some (Printf.sprintf "%s is constant (%s): %s" name status relation)

  let validation_errors cs =
    cs |> List.mapi constant_error |> List.filter_map Fun.id
end

type t = {name: string option; obj: Objective.t; cnstrs: Cnstr.t list}

let make ?(name = "") obj cnstrs =
  if 0 = String.length name then {name= None; obj; cnstrs}
  else {name= Some name; obj; cnstrs}

let name p = p.name

let objective p = p.obj

let cnstrs p = p.cnstrs

let obj_cnstrs p = (p.obj, p.cnstrs)

let take_vars p =
  Objective.take_vars (objective p)
  @ List.concat (List.map Cnstr.take_vars (cnstrs p))

let uniq_vars p =
  let vars = take_vars p in
  List.sort_uniq Var.compare_name vars

let uniq_vars_struct p =
  let vars = take_vars p in
  List.sort_uniq compare vars

let collision p =
  let uniqn = List.length (uniq_vars p) in
  let uniql = List.length (uniq_vars_struct p) in
  uniqn <> uniql

let vname_list p = List.map Var.to_string (uniq_vars p)

let classify p =
  let odeg = Objective.degree (objective p) in
  let cdeg = Cnstrs.degree (cnstrs p) in
  if Vars.has_integer (uniq_vars p) then
    if cdeg = 2 then Pclass.MIQCP
    else if odeg = 2 then Pclass.MIQP
    else Pclass.MILP
  else if cdeg = 2 then Pclass.QCP
  else if odeg = 2 then Pclass.QP
  else Pclass.LP

let collision_error p =
  if collision p then
    let uniqn = List.length (uniq_vars p) in
    let uniql = List.length (uniq_vars_struct p) in
    Some
      (Printf.sprintf
         "variable name collision: %d unique names but %d distinct variable \
          definitions"
         uniqn uniql )
  else None

let validation_errors p =
  let errs = Cnstrs.validation_errors (cnstrs p) in
  match collision_error p with Some msg -> msg :: errs | None -> errs

let validate_result p =
  match validation_errors p with
  | [] ->
      Ok ()
  | errs ->
      Error (String.concat "; " errs)

let validate p = match validate_result p with Ok () -> true | Error _ -> false

let to_string ?(short = false) p =
  let obj = Objective.to_string ~short (objective p) in
  let cnstrs = Cnstrs.to_string ~short (cnstrs p) in
  let vars = uniq_vars p in
  let bound = Vars.to_bound_string ~short vars in
  let vtype = Vars.to_vtype_string vars in
  String.concat "\n"
    ( (match name p with None -> [] | Some n -> ["\\ " ^ n])
    @ [obj; cnstrs]
    @ (match bound with None -> [] | Some b -> [b])
    @ (match vtype with None -> [] | Some v -> [v])
    @ ["end"] )
