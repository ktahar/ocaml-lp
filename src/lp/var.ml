type attr =
  (* Continuous (float) *)
  | Continuous of float * float
  (* General integer *)
  | General of float * float
  (* Binary integer *)
  | Binary

type t = {name: string; attr: attr}

let pattern = Str.regexp "^[a-zA-Z_][a-zA-Z0-9_!#\\$%&(),\\.\\?@{}~]*$"

let keywords =
  [ "end"
  ; "max"
  ; "maximize"
  ; "min"
  ; "minimize"
  ; "st"
  ; "st."
  ; "s.t."
  ; "subject"
  ; "such"
  ; "bound"
  ; "bounds"
  ; "gen"
  ; "general"
  ; "generals"
  ; "bin"
  ; "binary"
  ; "binaries"
  ; "free"
  ; "infinity"
  ; "inf" ]

let validate_name n =
  Str.string_match pattern n 0
  && not (List.mem (String.lowercase_ascii n) keywords)

let delim = Str.regexp "_"

(* compare only names to sort terms correctly even when collision exists *)
let compare_name vl vr =
  let ls = Str.split delim vl.name in
  let rs = Str.split delim vr.name in
  if List.length ls <> List.length rs then String.compare vl.name vr.name
  else
    let rec comp left right =
      match (left, right) with
      | [], [] ->
          0
      | l :: lrest, r :: rrest -> (
        match (int_of_string_opt l, int_of_string_opt r) with
        | Some li, Some ri ->
            let c = Int.compare li ri in
            if c <> 0 then c else comp lrest rrest
        | _ ->
            let c = String.compare l r in
            if c <> 0 then c else comp lrest rrest )
      | _ ->
          failwith "comp: unexpected pattern"
    in
    comp ls rs

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
  Array.init num (fun i ->
      make ~integer ~lb ~ub (String.concat "_" [name; string_of_int i]) )

let to_string v = v.name

let to_bound v =
  match v.attr with
  | Binary ->
      (Float.zero, Float.one)
  | Continuous (lb, ub) | General (lb, ub) ->
      (lb, ub)

let to_bound_string ?(short = false) v =
  let default lb ub = lb = Float.zero && ub = Float.infinity in
  match v.attr with
  | General (lb, ub) when not (default lb ub) ->
      let round v = v |> Float.round |> Float.to_int in
      let lb_s =
        if Float.is_infinite lb then "-inf" else string_of_int (round lb)
      in
      let ub_s =
        if Float.is_infinite ub then "inf" else string_of_int (round ub)
      in
      Some (Printf.sprintf "%s <= %s <= %s" lb_s v.name ub_s)
  | Continuous (lb, ub) when not (default lb ub) ->
      if short then Some (Printf.sprintf "%.2f <= %s <= %.2f" lb v.name ub)
      else Some (Printf.sprintf "%.18e <= %s <= %.18e" lb v.name ub)
  | _ ->
      None

let with_bound lb ub = function
  | {name; attr= Continuous _} ->
      {name; attr= Continuous (lb, ub)}
  | {name; attr= General _} ->
      {name; attr= General (lb, ub)}
  | org ->
      org

let to_binary v = {v with attr= Binary}

let to_integer = function
  | {name; attr= Continuous (lb, ub)} ->
      {name; attr= General (lb, ub)}
  | org ->
      org
