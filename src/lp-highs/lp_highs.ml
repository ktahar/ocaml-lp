exception SolverError of string

(** Read a file and create a list of strings. *)
let read_lines filename =
  let ic = open_in filename in
  let rec aux acc =
    try
      let line = input_line ic in
      aux (line :: acc)
    with End_of_file -> close_in ic ; List.rev acc
  in
  aux []

(** Load the solution file of HiGHS. *)
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

(** Insert timestamp "YYYYMMDD-hhmmss-PID-Index" to a given filename. *)
let create_filename =
  let index = ref 0 in
  fun dirname ->
    let open Unix in
    let tm = localtime (time ()) in
    let timestamp =
      Printf.sprintf "%04d%02d%02d-%02d%02d%02d-PID%d-%d" (tm.tm_year + 1900)
        (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec
        (Unix.getpid ()) !index
    in
    incr index ;
    fun filename -> dirname ^ "/" ^ timestamp ^ "-" ^ filename

(** Run HiGHS and obtain the output solution.
    @param msg if False, no log is shown
    @param time_limit maximum time for solver (in seconds)
    @param options list of additional options to pass to solver
    @param keep_files if True, files are saved in the current directory and not deleted after solving
    @param path path to the solver binary (you can get binaries compiling from source - https://highs.dev)
 *)
let solve ?path ?(msg = true) ?time_limit ?(keep_files = false) ?(options = [])
    problem =
  try
    let highs_path =
      match path with
      | None -> (
        try Sys.getenv "HIGHS_CMD"
        with Not_found ->
          raise (SolverError "The path to the HiGHS executable is not set.") )
      | Some path ->
          path
    in
    let dir = if keep_files then "." else Filename.temp_dir "highs-" "" in
    let create_filename = create_filename dir in
    let lp_file = create_filename "problem.lp" in
    Lp.write lp_file problem ;
    let options_file = create_filename "options.txt" in
    let options =
      if msg then options else ("log_to_console", "false") :: options
    in
    let options =
      match time_limit with
      | None ->
          options
      | Some time_limit ->
          ("time_limit", string_of_float time_limit) :: options
    in
    write_options options_file options ;
    let solution_file = create_filename "solution.txt" in
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
