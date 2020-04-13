type t = Var.t list

type classified = {continuous: t; general: t; binary: t}

let classify vars =
  let rec classify_ cs gs bs = function
    | [] ->
        {continuous= List.rev cs; general= List.rev gs; binary= List.rev bs}
    | ({Var.attr= Var.Continuous (_, _); _} as c) :: rest ->
        classify_ (c :: cs) gs bs rest
    | ({Var.attr= Var.General (_, _); _} as g) :: rest ->
        classify_ cs (g :: gs) bs rest
    | ({Var.attr= Var.Binary; _} as b) :: rest ->
        classify_ cs gs (b :: bs) rest
  in
  classify_ [] [] [] vars

let to_vtype_string vars =
  let to_string li = li |> List.map Var.to_string |> String.concat " " in
  let bsec b = "binary\n " ^ to_string b in
  let gsec g = "general\n " ^ to_string g in
  match classify vars with
  | {general= []; binary= []; _} ->
      None
  | {general= _ as gs; binary= []; _} ->
      Some (gsec gs)
  | {general= []; binary= _ as bs; _} ->
      Some (bsec bs)
  | {general= _ as gs; binary= _ as bs; _} ->
      Some (gsec gs ^ "\n" ^ bsec bs)

let to_bound_string ?(short = false) vars =
  let v =
    vars
    |> List.filter_map (Var.to_bound_string ~short)
    |> List.map (fun s -> " " ^ s)
  in
  match v with
  | [] ->
      None
  | _ as vs ->
      Some ("bounds\n" ^ String.concat "\n" vs)
