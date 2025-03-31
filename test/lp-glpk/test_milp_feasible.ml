(* Stable Sets Problem 
   Finding maximum independent set in a graph where 
   node i is linked to nodes j where i - (i+1)/2 <= j < i *)

(* Type definitions *)
type task = int  (* Number of nodes in the graph *)
type 'a instance = { x: 'a array }  (* Solution representation *)

(* Build MILP model for the stable sets problem *)
let build_model n =
  let open Lp in
  
  (* Binary variables for each node *)
  let x = Array.init n (fun i ->
    Printf.sprintf "x_%d" i |> Poly.var ~integer:true ~lb:0. ~ub:1.
  ) in
  
  (* Generate constraints from the edge rule 
     Each node i is connected to (i+1)/2 previous nodes, specifically
     nodes [i-(i+1)/2, i-(i+1)/2+1, ..., i-1] *)
  let constraints = 
    List.flatten (
      List.init n (fun i ->
        List.init ((i+1)/2) (fun j ->
          let j = i - j - 1 in
          x.(i) ++ x.(j) <~ c 1.0
        )
      )
    )
  in
  
  (* Objective: maximize sum of selected nodes (size of stable set) *)
  let obj = Array.fold_left (++) (c 0.0) x in
  
  (make (maximize obj) constraints, { x })

(* Solve the stable sets problem with specified parameters *)
let solve_stable_set n =
  let problem, instance = build_model n in
  match Lp_glpk.Milp.solve ~term_output:false ~mip_gap:(Some 5.) problem with
  | Ok (_, pmap) -> 
      { x = Array.map (Lp.compute_poly pmap) instance.x }
  | Error _ -> 
      failwith "Failing test"

(* Extract nodes in the stable set from solution *)
let get_stable_set result =
  Array.mapi (fun i x -> (i, x > 0.5)) result.x
  |> Array.to_seq
  |> Seq.filter_map (fun (i, selected) -> if selected then Some i else None)
  |> List.of_seq

(* Main function that solves and returns the stable set *)
let find_maximum_stable_set n =
  let result = solve_stable_set n in
  get_stable_set result

(* Driver code to test the mip_gap functionality
   Using a 500% gap tolerance to demonstrate early termination of the solver. *)
let () = Printf.printf "%d\n" (List.length (find_maximum_stable_set 20))