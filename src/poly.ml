type t = Term.t list

type classified = {quad: t; linear: t; const: t}

let c x = [Term.c x]

let var ?(integer = false) ?(lb = Float.zero) ?(ub = Float.infinity) name =
  [Term.var ~integer ~lb ~ub name]

let binary name = [Term.var ~integer:true ~lb:Float.zero ~ub:Float.one name]

let range ?(integer = false) ?(lb = Float.zero) ?(ub = Float.infinity) name num
    =
  Array.init num (fun i -> [Term.var ~integer ~lb ~ub (name ^ string_of_int i)])

let zero = []

let sort p = List.sort Term.compare (List.map Term.sort p)

let partition poly =
  List.partition
    (fun t -> match t with Term.Const _ -> false | _ -> true)
    poly

let classify poly =
  let rec classify_ qs ls cs = function
    | [] ->
        {quad= List.rev qs; linear= List.rev ls; const= List.rev cs}
    | (Term.Const _ as c) :: rest ->
        classify_ qs ls (c :: cs) rest
    | (Term.Linear _ as l) :: rest ->
        classify_ qs (l :: ls) cs rest
    | (Term.Quad _ as q) :: rest ->
        classify_ (q :: qs) ls cs rest
  in
  classify_ [] [] [] poly

let classify_by var poly =
  let rec classify_ qs ls cs = function
    | [] ->
        {quad= List.rev qs; linear= List.rev ls; const= List.rev cs}
    | (Term.Const _ as c) :: rest ->
        classify_ qs ls (c :: cs) rest
    | (Term.Linear (_, v) as l) :: rest when v = var ->
        classify_ qs (l :: ls) cs rest
    | (Term.Linear _ as c) :: rest ->
        classify_ qs ls (c :: cs) rest
    | (Term.Quad (_, v0, v1) as q) :: rest when v0 = var && v1 = var ->
        classify_ (q :: qs) ls cs rest
    | (Term.Quad (_, v0, v1) as l) :: rest when v0 = var || v1 = var ->
        classify_ qs (l :: ls) cs rest
    | (Term.Quad _ as c) :: rest ->
        classify_ qs ls (c :: cs) rest
  in
  classify_ [] [] [] poly

let degree p = List.fold_left max 0 (List.map Term.degree p)

let take_vars poly =
  let rec take vars = function
    | [] ->
        vars
    | Term.Const _ :: rest ->
        take vars rest
    | Term.Linear (_, v) :: rest ->
        take (v :: vars) rest
    | Term.Quad (_, v0, v1) :: rest ->
        take (v0 :: v1 :: vars) rest
  in
  take [] poly

let uniq_vars poly =
  let vars = take_vars poly in
  List.sort_uniq Var.compare vars

let simplify ?(epsilon = 10. *. epsilon_float) poly =
  let rec simplify_ quads lins const = function
    | [] ->
        List.rev quads @ List.rev lins @ [Term.Const const]
    | Term.Const c :: rest ->
        simplify_ quads lins (c +. const) rest
    | (Term.Linear (newc, _) as newl) :: rest ->
        let simpl_l =
          match lins with
          | [] ->
              [newl]
          | (Term.Linear (c, v) as l) :: restr ->
              if Term.common_var newl l then Term.Linear (newc +. c, v) :: restr
              else newl :: l :: restr
          | _ ->
              failwith "simplify_: unexpected pattern"
        in
        simplify_ quads simpl_l const rest
    | (Term.Quad (newc, _, _) as newq) :: rest ->
        let simpl_q =
          match quads with
          | [] ->
              [newq]
          | (Term.Quad (c, v0, v1) as q) :: restr ->
              if Term.common_var newq q then
                Term.Quad (newc +. c, v0, v1) :: restr
              else newq :: q :: restr
          | _ ->
              failwith "simplify_: unexpected pattern"
        in
        simplify_ simpl_q lins const rest
  in
  let sorted = sort poly in
  List.filter
    (fun t -> not (Term.near_zero ~epsilon t))
    (simplify_ [] [] Float.zero sorted)

let collision p =
  let sorted = sort p in
  let res =
    List.fold_left
      (fun coll_term t ->
        (fst coll_term || Term.collision (snd coll_term) t, t))
      (false, Term.zero) sorted
  in
  fst res

let to_string ?(short = false) p =
  let ts_string = List.map (Term.to_string ~short) in
  String.concat " "
    (let cp = classify p in
     match cp with
     | {const= []; linear= []; quad= []} ->
         ["0.0"]
     | {const= _; linear= _; quad= []} ->
         ts_string (cp.const @ cp.linear)
     | {const= []; linear= []; quad= _} ->
         ["["] @ ts_string cp.quad @ ["]"]
     | _ ->
         ts_string (cp.const @ cp.linear)
         @ ["+"; "["] @ ts_string cp.quad @ ["]"])

let neg p = List.map Term.neg p

let ( ~- ) = neg

let ( + ) = ( @ )

let ( - ) pl pr = pl @ neg pr

let expand pl pr =
  List.concat (List.map (fun tl -> List.map (fun tr -> Term.( * ) tl tr) pr) pl)

let ( * ) = expand

let dot = List.map2 Term.( * )

let ( *@ ) = dot

let equiv pl pr = match simplify (pl - pr) with [] -> true | _ -> false

let divt poly term = List.map (fun t -> Term.( / ) t term) poly

let long_div var n d =
  let deg p =
    match classify_by var p with
    | {quad= []; linear= []; const= _} ->
        0
    | {quad= []; linear= _; _} ->
        1
    | _ ->
        2
  in
  let lead p =
    match classify_by var p with
    | {quad= []; linear= []; const= c} ->
        c
    | {quad= []; linear= l; _} ->
        l
    | {quad= q; _} ->
        q
  in
  let leadt p =
    match classify_by var p with
    | {quad= []; linear= []; const= [c]} ->
        c
    | {quad= []; linear= [l]; _} ->
        l
    | {quad= [q]; _} ->
        q
    | _ ->
        failwith "multi-variate polynomial is passed to leadt"
  in
  let rec loop q r =
    if equiv r zero || deg r < deg d then (q, r)
    else
      let t = divt (lead r) (leadt d) in
      loop (q + t) (simplify (r - (t * d)))
  in
  loop zero n

let div n d =
  match simplify d with
  | [] ->
      failwith "Division by zero"
  | [t] ->
      (* single term division *)
      divt n t
  | sd -> (
    match uniq_vars sd with
    | [v] -> (
      match long_div v (simplify n) sd with
      | q, [] ->
          q
      | _ ->
          failwith "Failed to long-divide" )
    | _ ->
        failwith "Cannot divide by multi-variate polynomial" )

let ( / ) = div

let trans_bound name lb ub p = List.map (Term.trans_bound name lb ub) p

let to_binary name p = List.map (Term.to_binary name) p

let to_integer name p = List.map (Term.to_integer name) p
