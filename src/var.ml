type attr =
  (* Continuous (float) *)
  | Continuous of float * float
  (* General integer *)
  | General of float * float
  (* Binary integer *)
  | Binary

type t = {name: string; attr: attr}

let re = Str.regexp "^[a-zA-Z_][a-zA-Z0-9_!#\\$%&(),\\.;\\?@{}~]*$"

let validate_name n = Str.string_match re n 0

(* compare only names to sort terms correctly even when collision exists *)
let compare_name l r = String.compare l.name r.name

(* collision means same names
 * with different attributes (not equal logically) *)
let collision l r = compare_name l r = 0 && l <> r

let make ?(integer = false) ?(lb = Float.zero) ?(ub = Float.infinity) name =
  let attr =
    if not integer then Continuous (lb, ub)
    else if lb = Float.zero && ub = Float.one then Binary
    else General (lb, ub)
  in
  if validate_name name then {name; attr}
  else failwith ("Invalid name for variable: " ^ name)

let make_binary name = make ~integer:true ~lb:Float.zero ~ub:Float.one name

let range ?(integer = false) ?(lb = Float.zero) ?(ub = Float.infinity) name num
    =
  Array.init num (fun i -> make ~integer ~lb ~ub (name ^ string_of_int i))

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
  | {name; attr= Continuous _} ->
      {name; attr= Continuous (lb, ub)}
  | {name; attr= General _} ->
      {name; attr= General (lb, ub)}
  | org ->
      org

let to_binary = function
  | {name; attr= Continuous _} ->
      {name; attr= Binary}
  | {name; attr= General _} ->
      {name; attr= Binary}
  | org ->
      org

let to_integer = function
  | {name; attr= Continuous (lb, ub)} ->
      {name; attr= General (lb, ub)}
  | org ->
      org
