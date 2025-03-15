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

(** Read the solution file of HiGHS. *)
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
  let _, objective =
    match List.find_opt (String.starts_with ~prefix:"Objective") lines with
    | None ->
        raise (SolverError "No objective found.")
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
      (objective, List.map parse_arg sub_lines)
  | _ ->
      raise (SolverError "Cannot read HiGHS solver output.")

(** Write HiGHS additional options to file *)
let write_options filepath options =
  let options =
    ("write_solution_style", "0") :: options
    |> List.map (fun (name, value) -> name ^ " = " ^ value)
    |> String.concat "\n"
  in
  let oc = open_out filepath in
  Printf.fprintf oc "%s\n" options ;
  close_out oc

(** Run HiGHS and obtain the output solution.
    @param path Path to the solver binary
           (you can get binaries by compiling from source - https://highs.dev).
           If this parameter is not set,
           the executable located at the path specified in the environment variable [HIGHS_CMD] is used.
           If both are unset, an error is thrown.
    @param msg If [false], no log is shown on console.
           [true] by default.
    @param log_path Path to the log file.
    @param time_limit Maximum time allowed for the solver (in seconds).
           By default, there is no time limit.
    @param gap_rel Relative gap tolerance for the solver to stop (in fraction).
    @param gap_abs Absolute gap tolerance for the solver to stop.
    @param options A list of additional options to pass to the solver.
    @param keep_files If [true], files are saved in the current directory and not deleted after solving.
           [false] by [default].
    @return The pair of the status and the pair of the objective value and the assignments:
           [(status, (objective, assignments))].
*)
let solve ?path ?(msg = true) ?log_path ?time_limit ?(keep_files = false)
    ?gap_rel ?gap_abs ?(options = []) problem =
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
    let tmp_dirname = Filename.temp_dir "ocaml-lp-highs-" "" in
    let create_filename filename =
      Filename.concat (if keep_files then "." else tmp_dirname) filename
    in
    let log_filename =
      match log_path with
      | None ->
          Filename.concat tmp_dirname "HiGHS.log"
      | Some log_path ->
          log_path
    in
    let problem_filename = create_filename "problem.lp" in
    Lp.write problem_filename problem ;
    let options_filename = create_filename "options.txt" in
    let set_float_options name float_opt options =
      match float_opt with
      | None ->
          options
      | Some float_opt ->
          (name, string_of_float float_opt) :: options
    in
    let options =
      (if msg then options else ("log_to_console", "false") :: options)
      |> set_float_options "time_limit" time_limit
      |> set_float_options "mip_rel_gap" gap_rel
      |> set_float_options "mip_abs_gap" gap_abs
      |> List.cons ("log_file", log_filename)
    in
    write_options options_filename options ;
    let solution_filename = create_filename "solution.txt" in
    let command =
      Printf.sprintf "%s %s --solution_file %s --options_file %s" highs_path
        problem_filename solution_filename options_filename
    in
    let result = Sys.command command in
    (* HiGHS return code semantics
       (see: https://github.com/ERGO-Code/HiGHS/issues/527#issuecomment-946575028)
       - -1: error
       -  0: success
       -  1: warning
    *)
    if result < 0 then
      raise
        (SolverError
           "Error while executing HiGHS, use ?msg:true for more details." )
    else () ;
    let obj, sol = readsol solution_filename in
    Ok
      ( obj
      , List.fold_left
          (fun m (k, v) -> Lp.PMap.add k v m)
          Lp.PMap.empty
          (List.map (fun (v, x) -> (Lp.var v, x)) sol) )
  with SolverError msg -> Error msg
