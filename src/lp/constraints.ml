type t = Constraint.t list

let to_string ?(short = false) cnstrs =
  let c_string = Constraint.to_string ~short in
  let body =
    cnstrs
    |> List.map Constraint.simplify
    |> List.map c_string
    |> List.map (fun s -> " " ^ s)
    |> String.concat "\n"
  in
  "subject to\n" ^ body

let has_constant cs = List.exists Constraint.constant cs

let degree cs = cs |> List.map Constraint.degree |> List.fold_left max 0
