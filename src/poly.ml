type t = Term.t list

type classified = {quad: t; linear: t; const: t}

let sort p = List.sort Term.compare (List.map Term.sort p)

let split poly =
  let rec split_ qls cs = function
    | [] ->
        (qls, cs)
    | (Term.Const _ as c) :: rest ->
        split_ qls (c :: cs) rest
    | (_ as ql) :: rest ->
        split_ (ql :: qls) cs rest
  in
  split_ [] [] poly

let classify poly =
  let rec classify_ qs ls cs = function
    | [] ->
        {quad= List.rev qs; linear= List.rev ls; const= List.rev cs}
    | (Term.Const _ as c) :: rest ->
        classify_ qs ls (c :: cs) rest
    | (Term.Linear (_, _) as l) :: rest ->
        classify_ qs (l :: ls) cs rest
    | (Term.Quad (_, _, _) as q) :: rest ->
        classify_ (q :: qs) ls cs rest
  in
  classify_ [] [] [] poly

let has_quad poly =
  List.exists
    (fun t -> match t with Term.Quad (_, _, _) -> true | _ -> false)
    poly

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

let simplify poly =
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
    (fun t -> not (Term.near_zero t))
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
         @ ["+"; "["]
         @ ts_string cp.quad
         @ ["]"])

let neg p = List.map Term.neg p

let ( ~- ) = neg

let trans_bound name lb ub p = List.map (Term.trans_bound name lb ub) p

let to_binary name p = List.map (Term.to_binary name) p

let to_integer name p = List.map (Term.to_integer name) p
