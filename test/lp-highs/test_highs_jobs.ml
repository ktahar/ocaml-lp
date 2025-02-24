(* test_jssp.ml *)

type task =
  { num_jobs: int
  ; num_machines: int
  ; processing_times: int array array
  ; machine_sequence: int array array }

type 'a instance = {start_times: 'a array array; completion_time: 'a}
[@@deriving map, show]

let build task : Lp.Problem.t * Lp.Poly.t instance =
  let open Lp in
  let num_jobs = task.num_jobs in
  let num_machines = task.num_machines in
  let processing_times = task.processing_times in
  let machine_sequence = task.machine_sequence in
  let m =
    Array.fold_left
      (fun acc row -> Array.fold_left ( + ) acc row)
      0 processing_times
  in
  let start_times =
    Array.init num_jobs (fun j ->
        Array.init num_machines (fun i ->
            Printf.sprintf "start_time_job_%d_machine_%d" (j + 1) (i + 1) |> var ) )
  in
  let y =
    Array.init num_jobs (fun j ->
        Array.init num_jobs (fun k ->
            Array.init num_machines (fun i ->
                Printf.sprintf "y_%d_%d_%d" (j + 1) (k + 1) (i + 1) |> binary ) ) )
  in
  let completion_time = var "completion_time" in
  let objective = completion_time in
  let machine_constraints =
    List.init num_jobs (fun j ->
        List.init (num_machines - 1) (fun i ->
            start_times.(j).(machine_sequence.(j).(i + 1))
            -- start_times.(j).(machine_sequence.(j).(i))
            >~ c (float_of_int processing_times.(j).(machine_sequence.(j).(i))) ) )
    |> List.concat
  in
  let precedence_constraints =
    List.init num_jobs (fun j ->
        List.init num_jobs (fun k ->
            if j <> k then
              List.init num_machines (fun i ->
                  [ start_times.(j).(i)
                    -- start_times.(k).(i)
                    ++ (c (float_of_int m) *~ y.(j).(k).(i))
                    >~ c (float_of_int processing_times.(k).(i))
                  ; start_times.(k).(i)
                    -- start_times.(j).(i)
                    -- (c (float_of_int m) *~ y.(j).(k).(i))
                    >~ c (float_of_int processing_times.(j).(i))
                       -- c (float_of_int m) ] )
            else [] ) )
    |> List.concat |> List.concat |> List.concat
  in
  let completion_constraints =
    List.init num_jobs (fun j ->
        completion_time
        -- start_times.(j).(machine_sequence.(j).(num_machines - 1))
        >~ c
             (float_of_int
                processing_times.(j).(machine_sequence.(j).(num_machines - 1)) ) )
  in
  let constraints =
    machine_constraints @ precedence_constraints @ completion_constraints
  in
  ( make ~name:"JSSP" (minimize objective) constraints
  , {start_times; completion_time} )

let solve task =
  let problem, instance = build task in
  match Lp_highs.solve ~options:[("log_to_console", "false")] problem with
  | Ok (_, pmap) ->
      map_instance (Lp.compute_poly pmap) instance
  | Error msg ->
      Printf.printf "Failed to solve: %s\n" msg ;
      assert false

let () =
  let task =
    { num_jobs= 3
    ; num_machines= 3
    ; processing_times= [|[|2; 1; 2|]; [|1; 2; 2|]; [|1; 2; 1|]|]
    ; machine_sequence= [|[|2; 0; 1|]; [|1; 2; 0|]; [|2; 1; 0|]|] }
  in
  let result = solve task in
  Format.printf "Solution:\n" ;
  Format.printf "Completion time: %.6f\n" result.completion_time ;
  Array.iteri
    (fun j job_starts ->
      Array.iteri
        (fun i start_time ->
          Format.printf "Task %d starts on machine %d at time %.6f\n" (j + 1)
            (i + 1) start_time )
        job_starts )
    result.start_times
