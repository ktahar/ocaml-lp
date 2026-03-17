exception SolverError of string

module Cmd = struct
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
    let status =
      match List.nth_opt lines 1 with
      | Some value ->
          value
      | None ->
          raise (SolverError "Cannot read HiGHS status from solution file.")
    in
    if status = "Infeasible" || status = "Unbounded" then
      raise (SolverError ("Model is " ^ status))
    else () ;
    let parse_arg line =
      let words =
        String.split_on_char ' ' line |> List.filter (fun x -> x <> "")
      in
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
    let begin_index_opt =
      List.find_index (String.starts_with ~prefix:"# Columns") lines
    in
    let end_index_opt =
      List.find_index (String.starts_with ~prefix:"# Rows") lines
    in
    match (begin_index_opt, end_index_opt) with
    | Some bi, Some ei ->
        let sub_lines =
          List.filteri (fun idx _ -> idx > bi && idx < ei) lines
        in
        (objective, List.map parse_arg sub_lines)
    | _ ->
        raise (SolverError "Cannot read HiGHS solver output.")

  (** Write HiGHS additional options to file. *)
  let write_options filepath options =
    let options =
      ("write_solution_style", "0") :: options
      |> List.map (fun (name, value) -> name ^ " = " ^ value)
      |> String.concat "\n"
    in
    let oc = open_out filepath in
    Printf.fprintf oc "%s\n" options ;
    close_out oc

  let solve ?(path = "highs") ?(msg = true) ?log_path ?time_limit
      ?(keep_files = false) ?gap_rel ?gap_abs ?(options = []) problem =
    let solve_main () =
      try
        let tmp_dirname = Filename.temp_dir "ocaml-lp-highs-" "" in
        let create_filename filename =
          Filename.concat (if keep_files then "." else tmp_dirname) filename
        in
        let log_filename =
          match log_path with
          | None ->
              Filename.concat tmp_dirname "HiGHS.log"
          | Some path ->
              path
        in
        let problem_filename = create_filename "problem.lp" in
        Lp.write problem_filename problem ;
        let options_filename = create_filename "options.txt" in
        let set_float_options name float_opt options_acc =
          match float_opt with
          | None ->
              options_acc
          | Some value ->
              (name, string_of_float value) :: options_acc
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
          Printf.sprintf "%s %s --solution_file %s --options_file %s" path
            problem_filename solution_filename options_filename
        in
        let result = Sys.command command in
        (* HiGHS return code semantics
       (see: https://github.com/ERGO-Code/HiGHS/issues/527#issuecomment-946575028)
       -  0: success
       -  1: warning
       Values other than 0/1 are treated as command failure
       (e.g. command-not-found from the shell).
    *)
        if result <> 0 && result <> 1 then
          raise
            (SolverError
               (Printf.sprintf
                  "Error while executing HiGHS (exit code %d), use ?msg:true \
                   for more details."
                  result ) )
        else () ;
        let obj, sol = readsol solution_filename in
        Ok
          ( obj
          , List.fold_left
              (fun m (k, v) -> Lp.PMap.add k v m)
              Lp.PMap.empty
              (List.map (fun (v, x) -> (Lp.var v, x)) sol) )
      with
      | SolverError msg ->
          Error msg
      | Sys_error msg | Failure msg | Invalid_argument msg ->
          Error msg
      | exn ->
          Error (Printexc.to_string exn)
    in
    match Lp.Problem.classify problem with
    | Lp.Pclass.QCP | Lp.Pclass.MIQCP ->
        Error
          "Lp_highs.Cmd.solve doesn't support quadratically constrained \
           problems"
    | _ ->
        solve_main ()
end

module Ctypes = struct
  module C = Ctypes
  module T = Lp_highs_types.M
  module B = Lp_highs_ffi.M
  open Lp

  type model_data =
    { vars: Var.t list
    ; ncols: int
    ; nrows: int
    ; nnz: int
    ; sense: T.ObjSense.t
    ; col_cost: float C.CArray.t
    ; col_lower: float C.CArray.t
    ; col_upper: float C.CArray.t
    ; row_lower: float C.CArray.t
    ; row_upper: float C.CArray.t
    ; a_start: T.HighsInt.t C.CArray.t
    ; a_index: T.HighsInt.t C.CArray.t
    ; a_value: float C.CArray.t
    ; integrality: T.VarType.t C.CArray.t option }

  let make_pmap vars values =
    List.fold_left
      (fun m (k, v) -> PMap.add k v m)
      PMap.empty
      (List.mapi (fun i v -> (Poly.of_var v, C.CArray.get values i)) vars)

  let status_is_error = function T.Status.ERROR -> true | _ -> false

  let check_status stage status =
    if status_is_error status then
      raise
        (SolverError
           (Printf.sprintf "%s failed with status %s" stage
              (T.Status.to_string status) ) )

  let normalize_bound highs_inf value =
    if Float.is_infinite value then
      if value > 0.0 then highs_inf else -.highs_inf
    else value

  let parse_bool value =
    match String.lowercase_ascii (String.trim value) with
    | "1" | "true" | "on" | "yes" ->
        true
    | "0" | "false" | "off" | "no" ->
        false
    | _ ->
        raise (SolverError ("Cannot parse bool option value: " ^ value))

  let set_option highs (name, value) =
    let option_type_ptr = C.allocate T.HighsInt.t 0 in
    check_status
      (Printf.sprintf "Highs_getOptionType(%s)" name)
      (B.get_option_type highs name option_type_ptr) ;
    let option_type = T.OptionType.of_int C.(!@option_type_ptr) in
    match option_type with
    | T.OptionType.BOOL ->
        let int_value = if parse_bool value then 1 else 0 in
        check_status
          (Printf.sprintf "Highs_setBoolOptionValue(%s)" name)
          (B.set_bool_option_value highs name int_value)
    | T.OptionType.INT ->
        let int_value =
          try int_of_string value
          with Failure _ ->
            raise (SolverError ("Cannot parse int option value: " ^ value))
        in
        check_status
          (Printf.sprintf "Highs_setIntOptionValue(%s)" name)
          (B.set_int_option_value highs name int_value)
    | T.OptionType.DOUBLE ->
        let float_value =
          try float_of_string value
          with Failure _ ->
            raise (SolverError ("Cannot parse float option value: " ^ value))
        in
        check_status
          (Printf.sprintf "Highs_setDoubleOptionValue(%s)" name)
          (B.set_double_option_value highs name float_value)
    | T.OptionType.STRING ->
        check_status
          (Printf.sprintf "Highs_setStringOptionValue(%s)" name)
          (B.set_string_option_value highs name value)

  let apply_options highs ?(msg = true) ?log_path ?time_limit ?gap_rel ?gap_abs
      ?(options = []) () =
    let defaults =
      []
      |> (fun xs ->
      match log_path with None -> xs | Some path -> ("log_file", path) :: xs )
      |> (fun xs ->
      match time_limit with
      | None ->
          xs
      | Some value ->
          ("time_limit", string_of_float value) :: xs )
      |> (fun xs ->
      match gap_rel with
      | None ->
          xs
      | Some value ->
          ("mip_rel_gap", string_of_float value) :: xs )
      |> (fun xs ->
      match gap_abs with
      | None ->
          xs
      | Some value ->
          ("mip_abs_gap", string_of_float value) :: xs )
      |> (fun xs -> if msg then xs else ("log_to_console", "false") :: xs)
      |> List.rev
    in
    List.iter (set_option highs) (defaults @ options)

  let build_var_index vars =
    let index = Hashtbl.create (List.length vars) in
    List.iteri (fun i v -> Hashtbl.replace index v.Var.name i) vars ;
    index

  let idx_var index v =
    match Hashtbl.find_opt index v.Var.name with
    | Some i ->
        i
    | None ->
        raise (SolverError ("cannot find variable " ^ v.Var.name))

  let build_model_data highs problem pclass =
    let obj, cnstrs = Problem.obj_cnstrs problem in
    let vars = Problem.uniq_vars problem in
    let ncols = List.length vars in
    let nrows = List.length cnstrs in
    let var_index = build_var_index vars in
    let highs_inf = B.get_infinity highs in
    let col_cost = C.CArray.make C.double ncols in
    let col_lower = C.CArray.make C.double ncols in
    let col_upper = C.CArray.make C.double ncols in
    let row_lower = C.CArray.make C.double nrows in
    let row_upper = C.CArray.make C.double nrows in
    let integrality =
      match pclass with
      | Problem.Pclass.MILP ->
          Some (C.CArray.make T.VarType.t ncols)
      | _ ->
          None
    in
    let sense =
      if Objective.is_max obj then T.ObjSense.MAXIMIZE else T.ObjSense.MINIMIZE
    in
    List.iteri
      (fun j var ->
        let lb, ub =
          match var.Var.attr with
          | Var.Continuous (lb, ub) | Var.General (lb, ub) ->
              (normalize_bound highs_inf lb, normalize_bound highs_inf ub)
          | Var.Binary ->
              (0.0, 1.0)
        in
        C.CArray.set col_lower j lb ;
        C.CArray.set col_upper j ub ;
        match integrality with
        | None ->
            ()
        | Some data ->
            let vt =
              match var.Var.attr with
              | Var.Continuous _ ->
                  T.VarType.CONTINUOUS
              | Var.General _ | Var.Binary ->
                  T.VarType.INTEGER
            in
            C.CArray.set data j vt )
      vars ;
    Poly.iter_linear_exn
      (fun c v -> C.CArray.set col_cost (idx_var var_index v) c)
      (Objective.to_poly obj) ;
    let col_entries = Array.make ncols [] in
    List.iteri
      (fun i cnstr ->
        let lhs, rhs = Cnstr.sides cnstr in
        let rhs = normalize_bound highs_inf rhs in
        if Cnstr.is_eq cnstr then (
          C.CArray.set row_lower i rhs ;
          C.CArray.set row_upper i rhs )
        else (
          C.CArray.set row_lower i (-.highs_inf) ;
          C.CArray.set row_upper i rhs ) ;
        Poly.iter_linear_exn
          (fun coeff v ->
            let col = idx_var var_index v in
            col_entries.(col) <- (i, coeff) :: col_entries.(col) )
          lhs )
      cnstrs ;
    let nnz =
      Array.fold_left
        (fun acc entries -> acc + List.length entries)
        0 col_entries
    in
    let a_start = C.CArray.make T.HighsInt.t ncols in
    let a_index = C.CArray.make T.HighsInt.t nnz in
    let a_value = C.CArray.make C.double nnz in
    let pos = ref 0 in
    for col = 0 to ncols - 1 do
      C.CArray.set a_start col !pos ;
      let entries =
        List.sort
          (fun (row0, _) (row1, _) -> Int.compare row0 row1)
          col_entries.(col)
      in
      List.iter
        (fun (row, value) ->
          C.CArray.set a_index !pos row ;
          C.CArray.set a_value !pos value ;
          incr pos )
        entries
    done ;
    { vars
    ; ncols
    ; nrows
    ; nnz
    ; sense
    ; col_cost
    ; col_lower
    ; col_upper
    ; row_lower
    ; row_upper
    ; a_start
    ; a_index
    ; a_value
    ; integrality }

  let solve_model highs data =
    let pass_status =
      match data.integrality with
      | None ->
          B.pass_lp highs data.ncols data.nrows data.nnz T.MatrixFormat.COLWISE
            data.sense 0.0
            (C.CArray.start data.col_cost)
            (C.CArray.start data.col_lower)
            (C.CArray.start data.col_upper)
            (C.CArray.start data.row_lower)
            (C.CArray.start data.row_upper)
            (C.CArray.start data.a_start)
            (C.CArray.start data.a_index)
            (C.CArray.start data.a_value)
      | Some integrality ->
          B.pass_mip highs data.ncols data.nrows data.nnz T.MatrixFormat.COLWISE
            data.sense 0.0
            (C.CArray.start data.col_cost)
            (C.CArray.start data.col_lower)
            (C.CArray.start data.col_upper)
            (C.CArray.start data.row_lower)
            (C.CArray.start data.row_upper)
            (C.CArray.start data.a_start)
            (C.CArray.start data.a_index)
            (C.CArray.start data.a_value)
            (C.CArray.start integrality)
    in
    check_status "Highs_passLp/Highs_passMip" pass_status ;
    check_status "Highs_run" (B.run highs) ;
    let model_status = B.get_model_status highs in
    match model_status with
    | T.ModelStatus.OPTIMAL
    | T.ModelStatus.OBJECTIVE_BOUND
    | T.ModelStatus.OBJECTIVE_TARGET
    | T.ModelStatus.TIME_LIMIT
    | T.ModelStatus.ITERATION_LIMIT
    | T.ModelStatus.SOLUTION_LIMIT ->
        let ncols = B.get_num_col highs in
        let nrows = B.get_num_row highs in
        let col_value = C.CArray.make C.double ncols in
        let col_dual = C.CArray.make C.double ncols in
        let row_value = C.CArray.make C.double nrows in
        let row_dual = C.CArray.make C.double nrows in
        check_status "Highs_getSolution"
          (B.get_solution highs (C.CArray.start col_value)
             (C.CArray.start col_dual) (C.CArray.start row_value)
             (C.CArray.start row_dual) ) ;
        let obj = B.get_objective_value highs in
        Ok (obj, make_pmap data.vars col_value)
    | T.ModelStatus.INFEASIBLE ->
        Error "Model is Infeasible"
    | T.ModelStatus.UNBOUNDED ->
        Error "Model is Unbounded"
    | T.ModelStatus.UNBOUNDED_OR_INFEASIBLE ->
        Error "Model is UnboundedOrInfeasible"
    | status ->
        Error ("Model status: " ^ T.ModelStatus.to_string status)

  let solve ?(msg = true) ?log_path ?time_limit ?gap_rel ?gap_abs
      ?(options = []) problem =
    let pclass = Problem.classify problem in
    match pclass with
    | Problem.Pclass.LP | Problem.Pclass.MILP ->
        let highs = B.create () in
        if highs = C.null then Error "Highs_create returned null"
        else
          Fun.protect
            ~finally:(fun () -> B.destroy highs)
            (fun () ->
              try
                apply_options highs ~msg ?log_path ?time_limit ?gap_rel ?gap_abs
                  ~options () ;
                let data = build_model_data highs problem pclass in
                solve_model highs data
              with SolverError msg | Failure msg -> Error msg )
    | Problem.Pclass.QP ->
        (* TODO: Add convex QP support via Highs_passModel by passing Hessian
           data (q_start/q_index/q_value). *)
        Error "Lp_highs.Ctypes.solve supports only LP or MILP"
    | _ ->
        Error "Lp_highs.Ctypes.solve supports only LP or MILP"
end

let solve ?path ?(msg = true) ?log_path ?time_limit ?(keep_files = false)
    ?gap_rel ?gap_abs ?(options = []) problem =
  match (path, keep_files) with
  | Some _, _ | _, true ->
      Cmd.solve ?path ~msg ?log_path ?time_limit ~keep_files ?gap_rel ?gap_abs
        ~options problem
  | None, false ->
      Ctypes.solve ~msg ?log_path ?time_limit ?gap_rel ?gap_abs ~options problem
