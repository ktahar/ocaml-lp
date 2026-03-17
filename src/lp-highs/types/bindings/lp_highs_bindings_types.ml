module C = Lp_highs_consts.M

module M (F : Ctypes.TYPE) = struct
  open F

  module HighsInt = struct
    type t = int

    let t = int
  end

  module Status = struct
    type t = ERROR | OK | WARNING

    let of_int i =
      if i = C.status_error then ERROR
      else if i = C.status_ok then OK
      else if i = C.status_warning then WARNING
      else failwith "Unexpected HiGHS status"

    let to_int = function
      | ERROR ->
          C.status_error
      | OK ->
          C.status_ok
      | WARNING ->
          C.status_warning

    let to_string = function
      | ERROR ->
          "Error"
      | OK ->
          "Ok"
      | WARNING ->
          "Warning"

    let t = view ~read:of_int ~write:to_int HighsInt.t
  end

  module OptionType = struct
    type t = BOOL | INT | DOUBLE | STRING

    let of_int i =
      if i = C.option_type_bool then BOOL
      else if i = C.option_type_int then INT
      else if i = C.option_type_double then DOUBLE
      else if i = C.option_type_string then STRING
      else failwith "Unexpected HiGHS option type"

    let to_int = function
      | BOOL ->
          C.option_type_bool
      | INT ->
          C.option_type_int
      | DOUBLE ->
          C.option_type_double
      | STRING ->
          C.option_type_string

    let t = view ~read:of_int ~write:to_int HighsInt.t
  end

  module ObjSense = struct
    type t = MINIMIZE | MAXIMIZE

    let of_int i =
      if i = C.obj_sense_minimize then MINIMIZE
      else if i = C.obj_sense_maximize then MAXIMIZE
      else failwith "Unexpected HiGHS objective sense"

    let to_int = function
      | MINIMIZE ->
          C.obj_sense_minimize
      | MAXIMIZE ->
          C.obj_sense_maximize

    let t = view ~read:of_int ~write:to_int HighsInt.t
  end

  module MatrixFormat = struct
    type t = COLWISE | ROWWISE

    let of_int i =
      if i = C.matrix_format_colwise then COLWISE
      else if i = C.matrix_format_rowwise then ROWWISE
      else failwith "Unexpected HiGHS matrix format"

    let to_int = function
      | COLWISE ->
          C.matrix_format_colwise
      | ROWWISE ->
          C.matrix_format_rowwise

    let t = view ~read:of_int ~write:to_int HighsInt.t
  end

  module HessianFormat = struct
    type t = TRIANGULAR | SQUARE

    let of_int i =
      if i = C.hessian_format_triangular then TRIANGULAR
      else if i = C.hessian_format_square then SQUARE
      else failwith "Unexpected HiGHS Hessian format"

    let to_int = function
      | TRIANGULAR ->
          C.hessian_format_triangular
      | SQUARE ->
          C.hessian_format_square

    let t = view ~read:of_int ~write:to_int HighsInt.t
  end

  module VarType = struct
    type t =
      | CONTINUOUS
      | INTEGER
      | SEMI_CONTINUOUS
      | SEMI_INTEGER
      | IMPLICIT_INTEGER

    let of_int i =
      if i = C.var_type_continuous then CONTINUOUS
      else if i = C.var_type_integer then INTEGER
      else if i = C.var_type_semi_continuous then SEMI_CONTINUOUS
      else if i = C.var_type_semi_integer then SEMI_INTEGER
      else if i = C.var_type_implicit_integer then IMPLICIT_INTEGER
      else failwith "Unexpected HiGHS var type"

    let to_int = function
      | CONTINUOUS ->
          C.var_type_continuous
      | INTEGER ->
          C.var_type_integer
      | SEMI_CONTINUOUS ->
          C.var_type_semi_continuous
      | SEMI_INTEGER ->
          C.var_type_semi_integer
      | IMPLICIT_INTEGER ->
          C.var_type_implicit_integer

    let t = view ~read:of_int ~write:to_int HighsInt.t
  end

  module ModelStatus = struct
    type t =
      | NOTSET
      | LOAD_ERROR
      | MODEL_ERROR
      | PRESOLVE_ERROR
      | SOLVE_ERROR
      | POSTSOLVE_ERROR
      | MODEL_EMPTY
      | OPTIMAL
      | INFEASIBLE
      | UNBOUNDED_OR_INFEASIBLE
      | UNBOUNDED
      | OBJECTIVE_BOUND
      | OBJECTIVE_TARGET
      | TIME_LIMIT
      | ITERATION_LIMIT
      | UNKNOWN
      | SOLUTION_LIMIT
      | INTERRUPT

    let of_int i =
      if i = C.model_status_notset then NOTSET
      else if i = C.model_status_load_error then LOAD_ERROR
      else if i = C.model_status_model_error then MODEL_ERROR
      else if i = C.model_status_presolve_error then PRESOLVE_ERROR
      else if i = C.model_status_solve_error then SOLVE_ERROR
      else if i = C.model_status_postsolve_error then POSTSOLVE_ERROR
      else if i = C.model_status_model_empty then MODEL_EMPTY
      else if i = C.model_status_optimal then OPTIMAL
      else if i = C.model_status_infeasible then INFEASIBLE
      else if i = C.model_status_unbounded_or_infeasible then
        UNBOUNDED_OR_INFEASIBLE
      else if i = C.model_status_unbounded then UNBOUNDED
      else if i = C.model_status_objective_bound then OBJECTIVE_BOUND
      else if i = C.model_status_objective_target then OBJECTIVE_TARGET
      else if i = C.model_status_time_limit then TIME_LIMIT
      else if i = C.model_status_iteration_limit then ITERATION_LIMIT
      else if i = C.model_status_unknown then UNKNOWN
      else if i = C.model_status_solution_limit then SOLUTION_LIMIT
      else if i = C.model_status_interrupt then INTERRUPT
      else failwith "Unexpected HiGHS model status"

    let to_int = function
      | NOTSET ->
          C.model_status_notset
      | LOAD_ERROR ->
          C.model_status_load_error
      | MODEL_ERROR ->
          C.model_status_model_error
      | PRESOLVE_ERROR ->
          C.model_status_presolve_error
      | SOLVE_ERROR ->
          C.model_status_solve_error
      | POSTSOLVE_ERROR ->
          C.model_status_postsolve_error
      | MODEL_EMPTY ->
          C.model_status_model_empty
      | OPTIMAL ->
          C.model_status_optimal
      | INFEASIBLE ->
          C.model_status_infeasible
      | UNBOUNDED_OR_INFEASIBLE ->
          C.model_status_unbounded_or_infeasible
      | UNBOUNDED ->
          C.model_status_unbounded
      | OBJECTIVE_BOUND ->
          C.model_status_objective_bound
      | OBJECTIVE_TARGET ->
          C.model_status_objective_target
      | TIME_LIMIT ->
          C.model_status_time_limit
      | ITERATION_LIMIT ->
          C.model_status_iteration_limit
      | UNKNOWN ->
          C.model_status_unknown
      | SOLUTION_LIMIT ->
          C.model_status_solution_limit
      | INTERRUPT ->
          C.model_status_interrupt

    let to_string = function
      | NOTSET ->
          "NotSet"
      | LOAD_ERROR ->
          "LoadError"
      | MODEL_ERROR ->
          "ModelError"
      | PRESOLVE_ERROR ->
          "PresolveError"
      | SOLVE_ERROR ->
          "SolveError"
      | POSTSOLVE_ERROR ->
          "PostsolveError"
      | MODEL_EMPTY ->
          "ModelEmpty"
      | OPTIMAL ->
          "Optimal"
      | INFEASIBLE ->
          "Infeasible"
      | UNBOUNDED_OR_INFEASIBLE ->
          "UnboundedOrInfeasible"
      | UNBOUNDED ->
          "Unbounded"
      | OBJECTIVE_BOUND ->
          "ObjectiveBound"
      | OBJECTIVE_TARGET ->
          "ObjectiveTarget"
      | TIME_LIMIT ->
          "TimeLimit"
      | ITERATION_LIMIT ->
          "IterationLimit"
      | UNKNOWN ->
          "Unknown"
      | SOLUTION_LIMIT ->
          "SolutionLimit"
      | INTERRUPT ->
          "Interrupt"

    let t = view ~read:of_int ~write:to_int HighsInt.t
  end
end
