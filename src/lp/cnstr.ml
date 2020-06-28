type t =
  | Eq of string option * Poly.t * float
  | Ineq of string option * Poly.t * float

let to_string ?(short = false) c =
  let slist lhs rhs =
    [Poly.to_string ~short lhs; Poly.to_string ~short @@ Poly.c rhs]
  in
  match c with
  | Eq (Some name, lhs, rhs) ->
      name ^ ": " ^ String.concat " = " @@ slist lhs rhs
  | Eq (None, lhs, rhs) ->
      String.concat " = " @@ slist lhs rhs
  | Ineq (Some name, lhs, rhs) ->
      name ^ ": " ^ String.concat " <= " @@ slist lhs rhs
  | Ineq (None, lhs, rhs) ->
      String.concat " <= " @@ slist lhs rhs

let simplify_sides ?(eps = 10. *. epsilon_float) lhs rhs =
  let vl, cl = Poly.partition lhs in
  let vr, cr = Poly.partition rhs in
  let newl = Poly.(vl -- vr) in
  let newr = Poly.(cr -- cl) in
  (Poly.simplify ~eps newl, Poly.to_float @@ Poly.simplify ~eps newr)

let take_vars = function
  | Eq (_, lhs, _) | Ineq (_, lhs, _) ->
      Poly.take_vars lhs

let degree = function Eq (_, lhs, _) | Ineq (_, lhs, _) -> Poly.degree lhs

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

let sides = function Eq (_, l, r) | Ineq (_, l, r) -> (l, r)

let name = function
  | Eq (Some name, _, _) | Ineq (Some name, _, _) ->
      name
  | _ ->
      ""

let is_eq = function Eq _ -> true | Ineq _ -> false

let with_bound name lb ub = function
  | Eq (n, l, r) ->
      let newl = Poly.with_bound name lb ub l in
      Eq (n, newl, r)
  | Ineq (n, l, r) ->
      let newl = Poly.with_bound name lb ub l in
      Ineq (n, newl, r)

let to_integer name = function
  | Eq (n, l, r) ->
      let newl = Poly.to_integer name l in
      Eq (n, newl, r)
  | Ineq (n, l, r) ->
      let newl = Poly.to_integer name l in
      Ineq (n, newl, r)

let to_binary name = function
  | Eq (n, l, r) ->
      let newl = Poly.to_binary name l in
      Eq (n, newl, r)
  | Ineq (n, l, r) ->
      let newl = Poly.to_binary name l in
      Ineq (n, newl, r)
