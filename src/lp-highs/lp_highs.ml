exception SolverError of string

(* Read a file and create a list of strings. *)
let read_lines filename =
  let ic = open_in filename in
  let rec aux acc =
    try
      let line = input_line ic in
      aux (line :: acc)
    with End_of_file -> close_in ic ; List.rev acc
  in
  aux []

(* Load the solution file of HiGHS. *)
let readsol filename =
  let lines = read_lines filename in
  (* Separate each line and transform them into a tuple (name, float value). *)
  let parse_arg line =
    let words = String.split_on_char ' ' line in
    match words with
    | [name; value_str] ->
        (name, float_of_string value_str)
    | _ ->
        raise (SolverError ("Invalid line format: " ^ line))
  in
  let _, obj =
    match List.find_opt (String.starts_with ~prefix:"Objective") lines with
    | None ->
        raise (SolverError "No objective")
    | Some objective_line ->
        parse_arg objective_line
  in
  (* Geth the line indecies of "# Columns" and "# Rows". *)
  let begin_index_opt =
    List.find_index (String.starts_with ~prefix:"# Columns") lines
  in
  let end_index_opt =
    List.find_index (String.starts_with ~prefix:"# Rows") lines
  in
  match (begin_index_opt, end_index_opt) with
  | Some bi, Some ei ->
      (* Get the sub-lines [start, ei). *)
      let sub_lines = List.filteri (fun idx _ -> idx > bi && idx < ei) lines in
      (obj, List.map parse_arg sub_lines)
  | _ ->
      raise (SolverError "Cannot read HiGHS solver output")

(** Write HiGHS additional options to file *)
let write_options filepath options =
  let oc = open_out filepath in
  let options =
    options
    |> List.map (fun (name, value) -> name ^ " = " ^ value)
    |> String.concat "\n"
  in
  Printf.fprintf oc "%s\n" options ;
  close_out oc

(** Insert timestamp "YYYYMMDD-hhmmss-Index-PID" to a given string (filename)
    with a placeholder. *)
let format_with_timestamp =
  let index = ref 0 in
  fun () ->
    let open Unix in
    let tm = localtime (time ()) in
    let tm =
      Printf.sprintf "%04d%02d%02d-%02d%02d%02d" (tm.tm_year + 1900)
        (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec
    in
    let timestamp = Printf.sprintf "%s-%d-%d" tm !index (Unix.getpid ()) in
    incr index ;
    fun format_str -> Printf.sprintf format_str timestamp

(* Run HiGHS and obtain the output solution.
   @param options list of additional options to pass to solver.
 *)
let solve ?highs_path ?(options = []) problem =
  try
    let highs_path =
      match highs_path with
      | None -> (
        try Sys.getenv "HIGHS_CMD"
        with Not_found ->
          raise (SolverError "The path to the HiGHS executable is not set.") )
      | Some path ->
          path
    in
    let format_with_timestamp = format_with_timestamp () in
    let lp_file = format_with_timestamp "tmp-%s.lp" in
    let options_file = format_with_timestamp "options-%s.txt" in
    Lp.write lp_file problem ;
    write_options options_file options ;
    let solution_file = format_with_timestamp "solution-%s.txt" in
    let command =
      Printf.sprintf "%s %s --solution_file %s --options_file %s" highs_path
        lp_file solution_file options_file
    in
    let result = Sys.command command in
    if result <> 0 then raise (SolverError "Failed to execute HiGHS") else () ;
    let obj, sol = readsol solution_file in
    Ok
      ( obj
      , List.fold_left
          (fun m (k, v) -> Lp.PMap.add k v m)
          Lp.PMap.empty
          (List.map (fun (v, x) -> (Lp.var v, x)) sol) )
  with SolverError msg -> Error msg
