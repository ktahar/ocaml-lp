type t = Term.t list

type classified = {const: t; linear: t; quad: t}

type decomposed =
  { const: float
  ; lcs: float list
  ; lvs: Var.t list
  ; qcs: float list
  ; qv0s: Var.t list
  ; qv1s: Var.t list }

let c x = [Term.c x]

let var ?(integer = false) ?(lb = Float.zero) ?(ub = Float.infinity) name =
  [Term.var ~integer ~lb ~ub name]

let of_var v = [Term.of_var v]

let of_term t = [t]

let binary name = [Term.var ~integer:true ~lb:Float.zero ~ub:Float.one name]

let range ?(integer = false) ?(lb = Float.zero) ?(ub = Float.infinity)
    ?(start = 0) stop name =
  Array.init (stop - start) (fun i ->
      var ~integer ~lb ~ub (String.concat "_" [name; string_of_int (start + i)]))

let range2 ?(integer = false) ?(lb = Float.zero) ?(ub = Float.infinity)
    ?(start0 = 0) ?(start1 = 0) stop0 stop1 name =
  Array.init (stop0 - start0) (fun i ->
      range ~integer ~lb ~ub ~start:start1 stop1
        (String.concat "_" [name; string_of_int (start0 + i)]))

let range3 ?(integer = false) ?(lb = Float.zero) ?(ub = Float.infinity)
    ?(start0 = 0) ?(start1 = 0) ?(start2 = 0) stop0 stop1 stop2 name =
  Array.init (stop0 - start0) (fun i ->
      range2 ~integer ~lb ~ub ~start0:start1 ~start1:start2 stop1 stop2
        (String.concat "_" [name; string_of_int (start0 + i)]))

let rangeb = range ~integer:true ~lb:Float.zero ~ub:Float.one

let range2b = range2 ~integer:true ~lb:Float.zero ~ub:Float.one

let range3b = range3 ~integer:true ~lb:Float.zero ~ub:Float.one

let rangev ?(integer = false) ?(lb = [||]) ?(ub = [||]) ?(start = 0) stop name =
  Array.init (stop - start) (fun i ->
      let l = if Array.length lb > 0 then lb.(i) else Float.zero in
      let u = if Array.length ub > 0 then ub.(i) else Float.infinity in
      var ~integer ~lb:l ~ub:u
        (String.concat "_" [name; string_of_int (start + i)]))

let range2v ?(integer = false) ?(lb = [||]) ?(ub = [||]) ?(start0 = 0)
    ?(start1 = 0) stop0 stop1 name =
  Array.init (stop0 - start0) (fun i ->
      let l = if Array.length lb > 0 then lb.(i) else [||] in
      let u = if Array.length ub > 0 then ub.(i) else [||] in
      rangev ~integer ~lb:l ~ub:u ~start:start1 stop1
        (String.concat "_" [name; string_of_int (start0 + i)]))

let range3v ?(integer = false) ?(lb = [||]) ?(ub = [||]) ?(start0 = 0)
    ?(start1 = 0) ?(start2 = 0) stop0 stop1 stop2 name =
  Array.init (stop0 - start0) (fun i ->
      let l = if Array.length lb > 0 then lb.(i) else [||] in
      let u = if Array.length ub > 0 then ub.(i) else [||] in
      range2v ~integer ~lb:l ~ub:u ~start0:start1 ~start1:start2 stop1 stop2
        (String.concat "_" [name; string_of_int (start0 + i)]))

let concat a = List.concat (Array.to_list a)

let concat_list = List.concat

let of_float_array fa = concat (Array.map c fa)

let of_term_list = Fun.id

let to_float = function
  | [] ->
      0.0
  | [Term.Const c] ->
      c
  | _ ->
      failwith "cannot convert to float as this is not const monomial"

let zero = []

let one = [Term.one]

let sort p = List.sort Term.compare (List.map Term.sort p)

let rec compare p0 p1 =
  match (p0, p1) with
  | [], [] ->
      0
  | _, [] ->
      1
  | [], _ ->
      -1
  | t0 :: rest0, t1 :: rest1 ->
      let c = Term.compare t0 t1 in
      if c <> 0 then c else compare rest0 rest1

let partition poly =
  List.partition
    (fun t -> match t with Term.Const _ -> false | _ -> true)
    poly

let classify poly =
  let rec classify_ cs ls qs = function
    | [] ->
        {const= List.rev cs; linear= List.rev ls; quad= List.rev qs}
    | (Term.Const _ as c) :: rest ->
        classify_ (c :: cs) ls qs rest
    | (Term.Linear _ as l) :: rest ->
        classify_ cs (l :: ls) qs rest
    | (Term.Quad _ as q) :: rest ->
        classify_ cs ls (q :: qs) rest
  in
  classify_ [] [] [] poly

let classify_by var poly =
  let rec classify_ cs ls qs = function
    | [] ->
        {const= List.rev cs; linear= List.rev ls; quad= List.rev qs}
    | (Term.Const _ as c) :: rest ->
        classify_ (c :: cs) ls qs rest
    | (Term.Linear (_, v) as l) :: rest when v = var ->
        classify_ cs (l :: ls) qs rest
    | (Term.Linear _ as c) :: rest ->
        classify_ (c :: cs) ls qs rest
    | (Term.Quad (_, v0, v1) as q) :: rest when v0 = var && v1 = var ->
        classify_ cs ls (q :: qs) rest
    | (Term.Quad (_, v0, v1) as l) :: rest when v0 = var || v1 = var ->
        classify_ cs (l :: ls) qs rest
    | (Term.Quad _ as c) :: rest ->
        classify_ (c :: cs) ls qs rest
  in
  classify_ [] [] [] poly

let decompose poly =
  let rec decompose_ const lcs lvs qcs qv0s qv1s = function
    | [] ->
        {const; lcs; lvs; qcs; qv0s; qv1s}
    | Term.Const c :: rest ->
        decompose_ (c +. const) lcs lvs qcs qv0s qv1s rest
    | Term.Linear (c, v) :: rest ->
        decompose_ const (c :: lcs) (v :: lvs) qcs qv0s qv1s rest
    | Term.Quad (c, v0, v1) :: rest ->
        decompose_ const lcs lvs (c :: qcs) (v0 :: qv0s) (v1 :: qv1s) rest
  in
  decompose_ Float.zero [] [] [] [] [] poly

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
  List.sort_uniq Var.compare_name vars

let linear_coeff poly var =
  let accum coeff = function
    | Term.Linear (c, v) when v = var ->
        c +. coeff
    | _ ->
        coeff
  in
  List.fold_left accum Float.zero poly

let quad_coeff poly v0 v1 =
  let accum coeff = function
    | Term.Quad (c, vv0, vv1)
      when (v0 = vv0 && v1 = vv1) || (v1 = vv0 && v0 = vv1) ->
        c +. coeff
    | _ ->
        coeff
  in
  List.fold_left accum Float.zero poly

let simplify ?(eps = 10. *. epsilon_float) poly =
  let rec simplify_ const lins quads = function
    | [] ->
        (Term.Const const :: List.rev lins) @ List.rev quads
    | Term.Const c :: rest ->
        simplify_ (c +. const) lins quads rest
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
        simplify_ const simpl_l quads rest
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
        simplify_ const lins simpl_q rest
  in
  poly |> sort |> simplify_ Float.zero [] []
  |> List.filter (fun t -> not (Term.near_zero ~eps t))

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

let ( ~-- ) = neg

let ( ++ ) = List.append

let ( -- ) pl pr = pl @ neg pr

let expand pl pr =
  List.concat (List.map (fun tl -> List.map (fun tr -> Term.mul tl tr) pr) pl)

let ( *~ ) = expand

let dot = List.map2 Term.mul

let ( *@ ) = dot

let equiv ?(eps = 10. *. epsilon_float) pl pr =
  match simplify ~eps (pl -- pr) with [] -> true | _ -> false

let divt poly term = List.map (fun t -> Term.div t term) poly

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
      loop (q ++ t) (simplify (r -- (t *~ d)))
  in
  loop zero n

let div n d =
  match simplify d with
  | [] ->
      raise Division_by_zero
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

let ( /~ ) = div

let with_bound name lb ub p = List.map (Term.with_bound name lb ub) p

let to_binary name p = List.map (Term.to_binary name) p

let to_integer name p = List.map (Term.to_integer name) p

let double_quad p = List.map Term.double_quad p

let half_quad p = List.map Term.half_quad p

let map = List.map

let map_linear f =
  List.map (fun t ->
      match t with
      | Term.Linear (c, v) ->
          f c v
      | _ ->
          failwith "non-linear term encountered")

let mapi = List.mapi

let iter = List.iter

let iter_linear f =
  List.iter (fun t -> match t with Term.Linear (c, v) -> f c v | _ -> ())

let iter_linear_exn f =
  List.iter (fun t ->
      match t with
      | Term.Linear (c, v) ->
          f c v
      | _ ->
          failwith "non-linear term encountered")

let iteri = List.iteri

let length = List.length

let take_linear_coeffs = map_linear (fun c _ -> c)
