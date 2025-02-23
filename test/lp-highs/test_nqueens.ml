(* N-Queens Problem *)

type task = {n: int}

type 'a instance = {x: 'a array array (* n x n array of binary variables *)}
[@@deriving map, show]

let build task : Lp.Problem.t * Lp.Poly.t instance =
  let open Lp in
  (* Create n x n binary variables *)
  let x =
    Array.init task.n (fun i ->
        Array.init task.n (fun j ->
            Printf.sprintf "x_%d_%d" i j
            |> Lp.Poly.var ~integer:true ~lb:0. ~ub:1. ) )
  in
  (* One queen per row *)
  let row_constraints =
    Array.map
      (fun row ->
        Array.fold_left ( ++ ) (c 0.0) row |> fun expr -> expr =~ c 1.0 )
      x
    |> Array.to_list
  in
  (* One queen per column *)
  let col_constraints =
    Array.init task.n (fun j ->
        Array.fold_left ( ++ ) (c 0.0) (Array.map (fun row -> row.(j)) x)
        |> fun expr -> expr =~ c 1.0 )
    |> Array.to_list
  in
  (* Diagonal constraints \ *)
  let diag1_constraints =
    List.init
      ((2 * task.n) - 1)
      (fun p ->
        let k = p - (task.n - 1) in
        Array.init task.n (fun i ->
            if 0 <= i - k && i - k < task.n then Some x.(i).(i - k) else None )
        |> Array.to_list
        |> List.filter_map (fun x -> x)
        |> List.fold_left ( ++ ) (c 0.0)
        |> fun expr -> expr <~ c 1.0 )
  in
  (* Diagonal constraints / *)
  let diag2_constraints =
    List.init
      ((2 * task.n) - 1)
      (fun p ->
        let k = p in
        Array.init task.n (fun i ->
            if 0 <= k - i && k - i < task.n then Some x.(i).(k - i) else None )
        |> Array.to_list
        |> List.filter_map (fun x -> x)
        |> List.fold_left ( ++ ) (c 0.0)
        |> fun expr -> expr <~ c 1.0 )
  in
  (* Create problem with all constraints *)
  let all_constraints =
    row_constraints @ col_constraints @ diag1_constraints @ diag2_constraints
  in
  (* No objective function needed, just feasibility *)
  (make (maximize (c 0.0)) all_constraints, {x})

let solve task =
  let problem, instance = build task in
  match Lp_glpk.solve ~term_output:false problem with
  | Ok (_, pmap) ->
      map_instance (Lp.compute_poly pmap) instance
  | Error msg ->
      Printf.printf "Failed to solve: %s\n" msg ;
      assert false

let print_solution result n =
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      if result.x.(i).(j) >= 0.99 then Printf.printf "Q "
      else Printf.printf ". "
    done ;
    Printf.printf "\n"
  done

let () =
  let task = {n= 8} in
  let result = solve task in
  print_solution result task.n
