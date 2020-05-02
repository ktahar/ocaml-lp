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
      match x with {Var.attr= Var.Continuous _; _} -> false | _ -> true)
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
