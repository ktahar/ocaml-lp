type bound = {name: string; lb: float; ub: float}

type section =
  | Sobj of Objective.t
  | Scnstr of Constraint.t list
  | Sbound of bound list
  | Sgeneral of string list
  | Sbinary of string list

type problem = section list

let trans_binary p name =
  match p with
  | obj, cnstrs ->
      let newobj =
        match obj with
        | Objective.No_obj as n ->
            n
        | Objective.Max p ->
            Objective.Max (Poly.to_binary name p)
        | Objective.Min p ->
            Objective.Min (Poly.to_binary name p)
      in
      let newc = List.map (Constraint.to_binary name) cnstrs in
      (newobj, newc)

let trans_general p name =
  match p with
  | obj, cnstrs ->
      let newobj =
        match obj with
        | Objective.No_obj as n ->
            n
        | Objective.Max p ->
            Objective.Max (Poly.to_integer name p)
        | Objective.Min p ->
            Objective.Min (Poly.to_integer name p)
      in
      let newc = List.map (Constraint.to_integer name) cnstrs in
      (newobj, newc)

let trans_bound p b =
  match p with
  | obj, cnstrs ->
      let newobj =
        match obj with
        | Objective.No_obj as n ->
            n
        | Objective.Max p ->
            Objective.Max (Poly.trans_bound b.name b.lb b.ub p)
        | Objective.Min p ->
            Objective.Min (Poly.trans_bound b.name b.lb b.ub p)
      in
      let newc = List.map (Constraint.trans_bound b.name b.lb b.ub) cnstrs in
      (newobj, newc)

let rec trans_attrs problem = function
  | [] ->
      problem
  | Sbound bounds :: rest ->
      let newp = List.fold_left trans_bound problem bounds in
      trans_attrs newp rest
  | Sgeneral gen :: rest ->
      let newp = List.fold_left trans_general problem gen in
      trans_attrs newp rest
  | Sbinary bin :: rest ->
      let newp = List.fold_left trans_binary problem bin in
      trans_attrs newp rest
  | _ ->
      (* though parser will error in this pattern *)
      failwith "LP file is ill-formed (multiple obj or cnstr sections?)"

let transform = function
  | Sobj obj :: Scnstr cnstrs :: attrs ->
      trans_attrs (obj, cnstrs) attrs
  | _ ->
      failwith "LP file is ill-formed (no constraint ?)"
