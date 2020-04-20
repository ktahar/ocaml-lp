type t = Max of Poly.t | Min of Poly.t | No_obj

let maximize p = Max (Poly.sort p)

let minimize p = Min (Poly.sort p)

let take_vars = function Max p | Min p -> Poly.take_vars p | No_obj -> []

let to_string ?(short = false) o =
  let p_string = Poly.to_string ~short in
  match o with
  | Max p when Poly.degree p >= 2 ->
      "maximize\n " ^ p_string p ^ " / 2"
  | Max p ->
      "maximize\n " ^ p_string p
  | Min p when Poly.degree p >= 2 ->
      "minimize\n " ^ p_string p ^ " / 2"
  | Min p ->
      "minimize\n " ^ p_string p
  | No_obj ->
      ""
