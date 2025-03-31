(* Example for Knapsack Problem *)
type item = {profit: float; weight: float}

type task = {items: item list; capacity: float}

type 'a instance = {x: 'a array; space_left: 'a; total_amount: 'a}
[@@deriving map, show]

let build task : Lp.Problem.t * Lp.Poly.t instance =
  let n = List.length task.items in
  let open Lp in
  let x =
    Array.init n (fun i ->
        Printf.sprintf "x_%d" i |> Lp.Poly.var ~integer:true ~lb:0. ~ub:1. )
  in
  (* Calculate constraints directly from the items list *)
  let constr =
    List.mapi (fun i item -> x.(i) *~ c item.weight) task.items
    |> List.fold_left ( ++ ) (c 0.0)
  in
  (* Calculate objective directly from the items list *)
  let obj =
    List.mapi (fun i item -> x.(i) *~ c item.profit) task.items
    |> List.fold_left ( ++ ) (c 0.0)
  in
  ( make (maximize obj) [constr <~ c task.capacity]
  , {x; space_left= c task.capacity -- constr; total_amount= obj} )

(* Solve and map solution back to original type *)
let solve task =
  let problem, instance = build task in
  match Lp_glpk.Milp.solve problem with
  | Ok (_, pmap) ->
      map_instance (Lp.compute_poly pmap) instance
  | Error msg ->
      Printf.printf "Failed to solve: %s\n" msg ;
      assert false

(* Example usage *)
let () =
  (* Create instance *)
  let task =
    { items=
        [ {profit= 10.; weight= 11.}
        ; {profit= 13.; weight= 15.}
        ; {profit= 18.; weight= 20.}
        ; {profit= 31.; weight= 35.}
        ; {profit= 7.; weight= 10.}
        ; {profit= 15.; weight= 33.} ]
    ; capacity= 47. }
  in
  (* Build and solve *)
  let result = solve task in
  (* Print selected items *)
  Format.printf "Solution: %a\n" (pp_instance Format.pp_print_float) result
