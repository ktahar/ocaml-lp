type t =
  | Const of float
  | Linear of float * Var.t
  | Quad of float * Var.t * Var.t

let c x = Const x

let var ?(integer = false) ?(lb = Float.zero) ?(ub = Float.infinity) name =
  Linear (Float.one, Var.make ~integer ~lb ~ub name)

let binary name =
  Linear (Float.one, Var.make ~integer:true ~lb:Float.zero ~ub:Float.one name)

let range ?(integer = false) ?(lb = Float.zero) ?(ub = Float.infinity) name num
    =
  Array.init num (fun i ->
      Linear (Float.one, Var.make ~integer ~lb ~ub (name ^ string_of_int i)))

let format_float f =
  let s = Printf.sprintf "%+.18e" f in
  String.concat " " [String.sub s 0 1; String.sub s 1 (String.length s - 1)]

let format_float_short f =
  let s = Printf.sprintf "%+.2f" f in
  String.concat " " [String.sub s 0 1; String.sub s 1 (String.length s - 1)]

let to_string ?(short = false) t =
  let fmt = if short then format_float_short else format_float in
  match t with
  | Const c ->
      fmt c
  | Linear (c, v) ->
      fmt c ^ " " ^ Var.to_string v
  | Quad (c, v0, v1) when v0 = v1 ->
      fmt c ^ " " ^ Var.to_string v0 ^ " ^ 2"
  | Quad (c, v0, v1) ->
      fmt c ^ " " ^ Var.to_string v0 ^ " * " ^ Var.to_string v1

let ( * ) x y =
  match (x, y) with
  | Const c0, Const c1 ->
      Const (c0 *. c1)
  | Const c0, Linear (c1, v) ->
      Linear (c0 *. c1, v)
  | Linear (c0, v), Const c1 ->
      Linear (c0 *. c1, v)
  | Linear (c0, v0), Linear (c1, v1) ->
      Quad (c0 *. c1, v0, v1)
  | Quad (c0, v0, v1), Const c1 ->
      Quad (c0 *. c1, v0, v1)
  | Const c0, Quad (c1, v0, v1) ->
      Quad (c0 *. c1, v0, v1)
  | _ ->
      failwith "Unsupported operation (trying to create Cubic or Quartic ?)"

let ( / ) x y =
  match (x, y) with
  | Const c0, Const c1 ->
      Const (c0 /. c1)
  | Linear (c0, v), Const c1 ->
      Linear (c0 /. c1, v)
  | Linear (c0, v0), Linear (c1, v1) when v0 = v1 ->
      Const (c0 /. c1)
  | Quad (c0, v0, v1), Const c1 ->
      Quad (c0 /. c1, v0, v1)
  | Quad (c, v0, v1), Linear (cr, vr) when v0 = vr || v1 = vr ->
      if v0 = vr then Linear (c /. cr, v1) else Linear (c /. cr, v0)
  | Const c0, Quad (c1, v0, v1) ->
      Quad (c0 *. c1, v0, v1)
  | _ ->
      failwith "Unsupported operation (trying to invert variables ?)"

let zero = Const Float.zero

let neg = function
  | Const c ->
      Const (Float.neg c)
  | Linear (c, v) ->
      Linear (Float.neg c, v)
  | Quad (c, v0, v1) ->
      Quad (Float.neg c, v0, v1)

let sort = function
  | Quad (c, v0, v1) ->
      if v0 > v1 then Quad (c, v1, v0) else Quad (c, v0, v1)
  | _ as t ->
      t

let near_zero = function
  | Const c ->
      Float.abs c < epsilon_float
  | Linear (c, _) ->
      Float.abs c < epsilon_float
  | Quad (c, _, _) ->
      Float.abs c < epsilon_float

let common_var tl tr =
  let stl = sort tl in
  let str = sort tr in
  match (stl, str) with
  | Linear (_, vl), Linear (_, vr) ->
      vl = vr
  | Quad (_, vl0, vl1), Quad (_, vr0, vr1) ->
      vl0 = vr0 && vl1 = vr1
  | _ ->
      false

let collision tl tr =
  let stl = sort tl in
  let str = sort tr in
  match (stl, str) with
  | Linear (_, vl), Linear (_, vr) ->
      Var.collision vl vr
  | Quad (_, vl0, vl1), Quad (_, vr0, vr1) ->
      Var.collision vl0 vr0 || Var.collision vl1 vr1
  | _ ->
      false

let compare tl tr =
  match (tl, tr) with
  | Const cl, Const cr ->
      Float.compare cl cr
  | Linear (cl, vl), Linear (cr, vr) ->
      if vl <> vr then Var.compare vl vr else Float.compare cl cr
  | Quad (cl, vl0, vl1), Quad (cr, vr0, vr1) ->
      if vl0 <> vr0 then Var.compare vl0 vr0
      else if vl1 <> vr1 then Var.compare vl1 vr1
      else Float.compare cl cr
  | Linear _, Const _ ->
      1
  | Const _, Linear _ ->
      -1
  | Quad _, Const _ ->
      1
  | Const _, Quad _ ->
      -1
  | Quad _, Linear _ ->
      1
  | Linear _, Quad _ ->
      -1

let trans_bound name lb ub = function
  | Linear (c, v) when v.name = name ->
      Linear (c, Var.trans_bound lb ub v)
  | Quad (c, v0, v1) when v0.name = name && v1.name = name ->
      let newv = Var.trans_bound lb ub v0 in
      Quad (c, newv, newv)
  | Quad (c, v0, v1) when v0.name = name ->
      Quad (c, Var.trans_bound lb ub v0, v1)
  | Quad (c, v0, v1) when v1.name = name ->
      Quad (c, v0, Var.trans_bound lb ub v1)
  | _ as org ->
      org

let to_binary name = function
  | Linear (c, v) when v.name = name ->
      Linear (c, Var.to_binary v)
  | Quad (c, v0, v1) when v0.name = name && v1.name = name ->
      let newv = Var.to_binary v0 in
      Quad (c, newv, newv)
  | Quad (c, v0, v1) when v0.name = name ->
      Quad (c, Var.to_binary v0, v1)
  | Quad (c, v0, v1) when v1.name = name ->
      Quad (c, v0, Var.to_binary v1)
  | _ as org ->
      org

let to_integer name = function
  | Linear (c, v) when v.name = name ->
      Linear (c, Var.to_integer v)
  | Quad (c, v0, v1) when v0.name = name && v1.name = name ->
      let newv = Var.to_integer v0 in
      Quad (c, newv, newv)
  | Quad (c, v0, v1) when v0.name = name ->
      Quad (c, Var.to_integer v0, v1)
  | Quad (c, v0, v1) when v1.name = name ->
      Quad (c, v0, Var.to_integer v1)
  | _ as org ->
      org
