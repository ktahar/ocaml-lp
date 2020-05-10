module Pclass = struct
  type t =
    (* Linear *)
    | LP
    (* Quadratic *)
    | QP
    (* Quadratically Constrained; aka QCQP *)
    | QCP
    (* Mixed Integer Linear *)
    | MILP
    (* Mixed Integer Quadratic *)
    | MIQP
    (* Mixed Integer Quadratically Constrained *)
    | MIQCP

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

type t = Objective.t * Constraints.t

let take_vars p =
  Objective.take_vars (fst p)
  @ List.concat (List.map Constraint.take_vars (snd p))

let uniq_vars p =
  let vars = take_vars p in
  List.sort_uniq Var.compare_name vars

let uniq_vars_struct p =
  let vars = take_vars p in
  List.sort_uniq compare vars

let collision p =
  let uniqn = List.length (uniq_vars p) in
  let uniql = List.length (uniq_vars_struct p) in
  if uniqn = uniql then false
  else (
    Printf.printf "collision: uniq vars: %d uniq vars (struct): %d\n" uniqn
      uniql ;
    true )

let vname_list p = List.map Var.to_string (uniq_vars p)

let classify p =
  let odeg = Objective.degree (fst p) in
  let cdeg = Constraints.degree (snd p) in
  if Vars.has_integer (uniq_vars p) then
    if cdeg = 2 then Pclass.MIQCP
    else if odeg = 2 then Pclass.MIQP
    else Pclass.MILP
  else if cdeg = 2 then Pclass.QCP
  else if odeg = 2 then Pclass.QP
  else Pclass.LP

let validate p = not (collision p || Constraints.has_constant (snd p))

let to_string ?(short = false) p =
  let obj = Objective.to_string ~short (fst p) in
  let cnstrs = Constraints.to_string ~short (snd p) in
  let vars = uniq_vars p in
  let bound = Vars.to_bound_string ~short vars in
  let vtype = Vars.to_vtype_string vars in
  String.concat "\n"
    ( [obj; cnstrs]
    @ (match bound with None -> [] | Some b -> [b])
    @ (match vtype with None -> [] | Some v -> [v])
    @ ["end"] )
