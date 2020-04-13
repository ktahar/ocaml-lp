type t = Objective.t * Constraint.t list

(* TODO classify problem into pclass *)
type pclass =
  (* Linear *)
  | LP
  (* Integer Linear *)
  | ILP
  (* Mixed Integer Linear *)
  | MILP
  (* Mixed Integer Quadratic *)
  | MIQP
  (* Mixed Integer Quadratically Constrained *)
  | MIQCP

let take_vars p =
  Objective.take_vars (fst p)
  @ List.concat (List.map Constraint.take_vars (snd p))

let uniq_vars p =
  let vars = take_vars p in
  List.sort_uniq Var.compare vars

let uniq_vars_logical p =
  let vars = take_vars p in
  List.sort_uniq compare vars

let collision p =
  let uniqn = List.length (uniq_vars p) in
  let uniql = List.length (uniq_vars_logical p) in
  if uniqn = uniql then false
  else (
    Printf.printf "collision: uniq var names: %d uniq vars: %d\n" uniqn uniql ;
    true )

(* TODO add more validations *)
let validate p = not (collision p)

let cnstrs_to_string ?(short = false) cnstrs =
  let c_string = Constraint.to_string ~short in
  let body =
    cnstrs
    |> List.map Constraint.simplify
    |> List.map c_string
    |> List.map (fun s -> " " ^ s)
    |> String.concat "\n"
  in
  "subject to\n" ^ body

let to_string ?(short = false) p =
  let o_string = Objective.to_string ~short in
  let cs_string = cnstrs_to_string ~short in
  let vars = uniq_vars p in
  let bound = Vars.to_bound_string ~short vars in
  let vtype = Vars.to_vtype_string vars in
  String.concat "\n"
    ( [o_string (fst p); cs_string (snd p)]
    @ (match bound with None -> [] | Some b -> [b])
    @ (match vtype with None -> [] | Some v -> [v])
    @ ["end"] )
