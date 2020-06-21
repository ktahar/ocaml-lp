type t =
  | Eq of string option * Poly.t * Poly.t
  | Ineq of string option * Poly.t * Poly.t

let to_string ?(short = false) c =
  let p_string = Poly.to_string ~short in
  match c with
  | Eq (Some name, lhs, rhs) ->
      name ^ ": " ^ String.concat " = " [p_string lhs; p_string rhs]
  | Eq (None, lhs, rhs) ->
      String.concat " = " [p_string lhs; p_string rhs]
  | Ineq (Some name, lhs, rhs) ->
      name ^ ": " ^ String.concat " <= " [p_string lhs; p_string rhs]
  | Ineq (None, lhs, rhs) ->
      String.concat " <= " [p_string lhs; p_string rhs]

let simplify_sides ?(eps = 10. *. epsilon_float) lhs rhs =
  let vl, cl = Poly.partition lhs in
  let vr, cr = Poly.partition rhs in
  let newl = Poly.(vl -- vr) in
  let newr = Poly.(cr -- cl) in
  (Poly.simplify ~eps newl, Poly.simplify ~eps newr)

let simplify ?(eps = 10. *. epsilon_float) = function
  | Eq (name, lhs, rhs) ->
      let l, r = simplify_sides ~eps lhs rhs in
      Eq (name, l, r)
  | Ineq (name, lhs, rhs) ->
      let l, r = simplify_sides ~eps lhs rhs in
      Ineq (name, l, r)

let take_vars = function
  | Eq (_, lhs, rhs) | Ineq (_, lhs, rhs) ->
      Poly.take_vars lhs @ Poly.take_vars rhs

let degree = function
  | Eq (_, lhs, rhs) | Ineq (_, lhs, rhs) ->
      max (Poly.degree lhs) (Poly.degree rhs)

let constant c =
  if degree c > 0 then false
  else (
    ( match c with
    | Eq (Some n, _, _) | Ineq (Some n, _, _) ->
        Printf.printf "constraint %s is constant\n" n
    | _ ->
        print_endline "a constraint is constant" ) ;
    true )

let eq ?(eps = 10. *. epsilon_float) ?(name = "") lhs rhs =
  let l, r = simplify_sides ~eps lhs rhs in
  if String.length name > 0 then
    if Var.validate_name name then Eq (Some name, l, r)
    else failwith ("Invalid name for constraint: " ^ name)
  else Eq (None, l, r)

let lt ?(eps = 10. *. epsilon_float) ?(name = "") lhs rhs =
  let l, r = simplify_sides ~eps lhs rhs in
  if String.length name > 0 then
    if Var.validate_name name then Ineq (Some name, l, r)
    else failwith ("Invalid name for constraint: " ^ name)
  else Ineq (None, l, r)

let gt ?(eps = 10. *. epsilon_float) ?(name = "") lhs rhs =
  lt ~eps ~name rhs lhs

let ( =~ ) l r = eq l r

let ( <~ ) l r = lt l r

let ( >~ ) l r = gt l r

let lhs = function Eq (_, l, _) | Ineq (_, l, _) -> l

let rhs = function Eq (_, _, r) | Ineq (_, _, r) -> r

let name = function
  | Eq (Some name, _, _) | Ineq (Some name, _, _) ->
      name
  | _ ->
      ""

let trans_bound name lb ub = function
  | Eq (n, l, r) ->
      let newl = Poly.trans_bound name lb ub l in
      let newr = Poly.trans_bound name lb ub r in
      Eq (n, newl, newr)
  | Ineq (n, l, r) ->
      let newl = Poly.trans_bound name lb ub l in
      let newr = Poly.trans_bound name lb ub r in
      Ineq (n, newl, newr)

let to_integer name = function
  | Eq (n, l, r) ->
      let newl = Poly.to_integer name l in
      let newr = Poly.to_integer name r in
      Eq (n, newl, newr)
  | Ineq (n, l, r) ->
      let newl = Poly.to_integer name l in
      let newr = Poly.to_integer name r in
      Ineq (n, newl, newr)

let to_binary name = function
  | Eq (n, l, r) ->
      let newl = Poly.to_binary name l in
      let newr = Poly.to_binary name r in
      Eq (n, newl, newr)
  | Ineq (n, l, r) ->
      let newl = Poly.to_binary name l in
      let newr = Poly.to_binary name r in
      Ineq (n, newl, newr)