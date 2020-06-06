type t = Cnstr.t list

let to_string ?(short = false) cnstrs =
  let c_string = Cnstr.to_string ~short in
  let body =
    cnstrs |> List.map c_string
    |> List.map (fun s -> " " ^ s)
    |> String.concat "\n"
  in
  "subject to\n" ^ body

let has_constant cs = List.exists Cnstr.constant cs

let degree cs = cs |> List.map Cnstr.degree |> List.fold_left max 0
