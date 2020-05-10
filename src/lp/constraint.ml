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

let simplify_sides lhs rhs =
  let l = Poly.partition lhs in
  let r = Poly.partition rhs in
  let newl = Poly.(fst l -- fst r) in
  let newr = Poly.(snd r -- snd l) in
  (Poly.simplify newl, Poly.simplify newr)

let simplify = function
  | Eq (name, lhs, rhs) ->
      let s = simplify_sides lhs rhs in
      Eq (name, fst s, snd s)
  | Ineq (name, lhs, rhs) ->
      let s = simplify_sides lhs rhs in
      Ineq (name, fst s, snd s)

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

let eq ?(name = "") lhs rhs =
  let s = simplify_sides lhs rhs in
  if String.length name > 0 then
    if Var.validate_name name then Eq (Some name, fst s, snd s)
    else failwith ("Invalid name for constraint: " ^ name)
  else Eq (None, fst s, snd s)

let lt ?(name = "") lhs rhs =
  let s = simplify_sides lhs rhs in
  if String.length name > 0 then
    if Var.validate_name name then Ineq (Some name, fst s, snd s)
    else failwith ("Invalid name for constraint: " ^ name)
  else Ineq (None, fst s, snd s)

let gt ?(name = "") lhs rhs = lt ~name rhs lhs

let ( =~ ) l r = eq l r

let ( <~ ) l r = lt l r

let ( >~ ) l r = gt l r

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
