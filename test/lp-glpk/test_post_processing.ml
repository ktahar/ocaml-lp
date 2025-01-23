(* Knapsack Problem Example *)

type task = {n: int; profit: float array; weight: float array; capacity: float}

type 'a instance = {x: 'a array; space_left: 'a; total_amount: 'a}
[@@deriving map, show]

let build task : Lp.Problem.t * Lp.Poly.t instance =
  assert (task.n = Array.length task.weight) ;
  assert (task.n = Array.length task.profit) ;
  let open Lp in
  let x =
    Array.init task.n (fun i ->
        Printf.sprintf "x_%d" i |> Lp.Poly.var ~integer:true ~lb:0. ~ub:1. )
  in
  let constr =
    Array.map2 (fun x_i w_i -> x_i *~ c w_i) x task.weight
    |> Array.fold_left ( ++ ) (c 0.0)
  in
  let obj =
    Array.map2 (fun x_i c_i -> x_i *~ c c_i) x task.profit
    |> Array.fold_left ( ++ ) (c 0.0)
  in
  ( make (maximize obj) [constr <~ c task.capacity]
  , {x; space_left= c task.capacity -- constr; total_amount= obj} )

(* Solve and map solution back to original type *)
let solve task =
  let problem, instance = build task in
  match Lp_glpk.solve ~term_output:false problem with
  | Ok (_, pmap) ->
      map_instance (Lp.compute_poly pmap) instance
  | Error msg ->
      Printf.printf "Failed to solve: %s\n" msg ;
      assert false

(* Example usage *)
let () =
  (* Create instance *)
  let task =
    { n= 6
    ; profit= [|10.; 13.; 18.; 31.; 7.; 15.|]
    ; weight= [|11.; 15.; 20.; 35.; 10.; 33.|]
    ; capacity= 47. }
  in
  (* Build and solve *)
  let result = solve task in
  (* Print selected items *)
  Format.printf "Solution: %a\n" (pp_instance Format.pp_print_float) result
