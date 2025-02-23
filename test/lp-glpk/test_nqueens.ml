(* N-Queens Problem *)

type task = {n: int}

(** n x n array of binary variables. *)
type 'a instance = {x: 'a array array} [@@deriving map, show, ord, eq]

(** Create n x n binary variables. *)
let build_domain task : Lp.Poly.t instance =
  let x =
    Array.init task.n (fun i ->
        Array.init task.n (fun j ->
            Printf.sprintf "x_%d_%d" i j
            |> Lp.Poly.var ~integer:true ~lb:0. ~ub:1. ) )
  in
  {x}

(** Create fundamental constraints for solving the N-Queens problem. *)
let build_basic_constraints task {x} : Lp.Cnstr.t list =
  let open Lp in
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
  row_constraints @ col_constraints @ diag1_constraints @ diag2_constraints

(** Create a problem using the built constraints. *)
let build constraints : Lp.Problem.t =
  let open Lp in
  (* No objective function needed, just feasibility *)
  make (maximize (c 0.0)) constraints

(** Find one solution.
   The result is non-deterministic and may vary across different versions of the solver. *)
let solve1 task =
  let instance = build_domain task in
  let constraints = build_basic_constraints task instance in
  let problem = build constraints in
  match Lp_glpk.solve ~term_output:false problem with
  | Ok (_, pmap) ->
      map_instance (Lp.compute_poly pmap) instance
  | Error msg ->
      Printf.printf "Failed to solve: %s\n" msg ;
      assert false

(** Some auxiliary functions for the constraints to find all solutions. *)

let rec split8 = function
  | [] ->
      ([], [], [], [], [], [], [], [])
  | (x1, x2, x3, x4, x5, x6, x7, x8) :: t ->
      let t1, t2, t3, t4, t5, t6, t7, t8 = split8 t in
      ( x1 :: t1
      , x2 :: t2
      , x3 :: t3
      , x4 :: t4
      , x5 :: t5
      , x6 :: t6
      , x7 :: t7
      , x8 :: t8 )

let list_of_8tuple (x1, x2, x3, x4, x5, x6, x7, x8) =
  [x1; x2; x3; x4; x5; x6; x7; x8]

(** List all symmetric configurations of the given configuration [placement]. *)
let enum_symmetric_configs placement =
  let n = Array.length placement - 1 in
  Array.to_list placement
  |> List.mapi (fun i row ->
         Array.to_list row
         |> List.mapi (fun j prev ->
                if prev >= 0.99 then
                  (* Symmetric to the given position by rotation and reflection. *)
                  Some
                    ( (i, j)
                    , (i, n - j)
                    , (n - i, j)
                    , (n - i, n - j)
                    , (j, i)
                    , (n - j, i)
                    , (j, n - i)
                    , (n - j, n - i) )
                else None ) )
  |> List.concat
  |> List.filter_map (fun x -> x)
  |> split8 |> list_of_8tuple

(** Create additional constraints to eliminate previously found results
    and their symmetric placements. *)
let build_additional_constraints {x} previous_solution =
  let n = Array.length x - 1 in
  (* A list of the positions to avoid in the next trial. *)
  let positions_to_avoid = enum_symmetric_configs previous_solution in
  (* If all the queens are placed in the same positions,
     then the sum of the [x.(i).(j)] will be N
     (the size of the problem, which is now [n + 1]).
     Here, we add constraints to ensure the sum is smaller than N,
     so that the configuration is different.
  *)
  List.map
    (fun pos_to_avoid ->
      let open Lp in
      List.map (fun (i, j) -> x.(i).(j)) pos_to_avoid
      |> List.fold_left ( ++ ) (c 0.0)
      |> fun expr -> expr <~ c (float_of_int n) )
    positions_to_avoid

(** Find all solutions up to symmetry by repeatedly solving the problem
    with additional constraints to eliminate redundant results.
    The optional argument [max_trial_n] specifies the maximum number of trials,
    which defaults to infinity.
    Note that the result is non-deterministic since the placements are not sorted
    and symmetric placements are ignored
*)
let solve_all ?(max_trial_n = Int.max_int) task =
  let instance = build_domain task in
  let constraints = build_basic_constraints task instance in
  let rec helper i cnstrs =
    if i >= max_trial_n then
      failwith
        (Printf.sprintf
           "Error: You have exceeded the maximum number of trials (%d) at '%s'"
           i __FILE__ )
    else
      let problem = build (cnstrs @ constraints) in
      match Lp_glpk.solve ~term_output:false problem with
      | Ok (_, pmap) ->
          let answer = map_instance (Lp.compute_poly pmap) instance in
          answer
          :: helper (succ i)
               (build_additional_constraints instance answer.x @ cnstrs)
      | Error _ ->
          []
  in
  helper 0 []

(** Given solutions that are unique up to symmetry,
    enumerate all solutions, including their symmetric placements.
    The list of placements is sorted to ensure uniqueness.
*)
let enum_all_solutions task solutions =
  let instance_of placements =
    let x =
      Array.init task.n (fun i ->
          Array.init task.n (fun j ->
              if List.mem (i, j) placements then 1.0 else 0.0 ) )
    in
    {x}
  in
  List.concat_map (fun solution -> enum_symmetric_configs solution.x) solutions
  |> List.map instance_of
  |> List.sort_uniq (compare_instance Float.compare)

let print_solution result =
  let n = Array.length result.x in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      if result.x.(i).(j) >= 0.99 then Printf.printf "Q "
      else Printf.printf ". "
    done ;
    Printf.printf "\n"
  done

let () =
  let task = {n= 8} in
  (* Find one solution non-deterministically;
     the result may differ from previously obtained solutions. *)
  let result1 = solve1 task in
  (* Find all solutions;
     the result may differ from previously obtained solutions. *)
  let results = solve_all ~max_trial_n:100 task in
  Printf.printf
    "The number of solutions to the 8-Queens problem is '%d', up to symmetry\n\n"
    (List.length results) ;
  (* This value must be unique since we have listed all the results and have sorted. *)
  let all_solutions = enum_all_solutions task results in
  Printf.printf
    "The total number of solutions, including their symmetric placements, is \
     '%d'.\n\n"
    (List.length all_solutions) ;
  (* Check the first obtained result is included in the enumerated results. *)
  Printf.printf
    "The first obtained result is included in the enumerated results: '%b'.\n\n"
    (List.exists (equal_instance Float.equal result1) all_solutions) ;
  (* Print all the enumerated results. *)
  List.iteri
    (fun i result ->
      Printf.printf "\n#%d\n" (i + 1) ;
      print_solution result )
    all_solutions
