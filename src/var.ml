type attr =
  (* Continuous (float) *)
  | Continuous of float * float
  (* General integer *)
  | General of float * float
  (* Binary integer *)
  | Binary

type t = {name: string; attr: attr}

(* compare only names to sort terms correctly even when collision exists *)
let compare l r = String.compare l.name r.name

(* collision means same names
 * with different attributes (not equal logically) *)
let collision l r = compare l r = 0 && l <> r

let make ?(integer = false) ?(lb = Float.zero) ?(ub = Float.infinity) name =
  let attr =
    if not integer then Continuous (lb, ub)
    else if lb = Float.zero && ub = Float.one then Binary
    else General (lb, ub)
  in
  {name; attr}

let make_binary name = make ~integer:true ~lb:Float.zero ~ub:Float.one name

let range ?(integer = false) ?(lb = Float.zero) ?(ub = Float.infinity) name num
    =
  Array.init num (fun i -> make ~integer ~lb ~ub (name ^ string_of_int i))

let null = make ""

let to_string v = v.name

let to_bound_string ?(short = false) v =
  let default lb ub = lb = Float.zero && ub = Float.infinity in
  match v.attr with
  | General (lb, ub) when not (default lb ub) ->
      let round v = v |> Float.round |> Float.to_int in
      Some (Printf.sprintf "%d <= %s <= %d" (round lb) v.name (round ub))
  | Continuous (lb, ub) when not (default lb ub) ->
      if short then Some (Printf.sprintf "%.2f <= %s <= %.2f" lb v.name ub)
      else Some (Printf.sprintf "%.18e <= %s <= %.18e" lb v.name ub)
  | _ ->
      None

let trans_bound lb ub = function
  | {attr= Continuous _; name= _ as n} ->
      {name= n; attr= Continuous (lb, ub)}
  | {attr= General _; name= _ as n} ->
      {name= n; attr= General (lb, ub)}
  | _ as org ->
      org

let to_binary = function
  | {attr= Continuous _; name= _ as n} ->
      {name= n; attr= Binary}
  | {attr= General _; name= _ as n} ->
      {name= n; attr= Binary}
  | _ as org ->
      org

let to_integer = function
  | {attr= Continuous (lb, ub); name= _ as n} ->
      {name= n; attr= General (lb, ub)}
  | _ as org ->
      org
