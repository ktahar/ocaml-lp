module M (F : Ctypes.TYPE) = struct
  let status_error = F.(constant "kHighsStatusError" int)

  let status_ok = F.(constant "kHighsStatusOk" int)

  let status_warning = F.(constant "kHighsStatusWarning" int)

  let var_type_continuous = F.(constant "kHighsVarTypeContinuous" int)

  let var_type_integer = F.(constant "kHighsVarTypeInteger" int)

  let var_type_semi_continuous = F.(constant "kHighsVarTypeSemiContinuous" int)

  let var_type_semi_integer = F.(constant "kHighsVarTypeSemiInteger" int)

  let var_type_implicit_integer = F.(constant "kHighsVarTypeImplicitInteger" int)

  let option_type_bool = F.(constant "kHighsOptionTypeBool" int)

  let option_type_int = F.(constant "kHighsOptionTypeInt" int)

  let option_type_double = F.(constant "kHighsOptionTypeDouble" int)

  let option_type_string = F.(constant "kHighsOptionTypeString" int)

  let obj_sense_minimize = F.(constant "kHighsObjSenseMinimize" int)

  let obj_sense_maximize = F.(constant "kHighsObjSenseMaximize" int)

  let matrix_format_colwise = F.(constant "kHighsMatrixFormatColwise" int)

  let matrix_format_rowwise = F.(constant "kHighsMatrixFormatRowwise" int)

  let hessian_format_triangular =
    F.(constant "kHighsHessianFormatTriangular" int)

  let hessian_format_square = F.(constant "kHighsHessianFormatSquare" int)

  let model_status_notset = F.(constant "kHighsModelStatusNotset" int)

  let model_status_load_error = F.(constant "kHighsModelStatusLoadError" int)

  let model_status_model_error = F.(constant "kHighsModelStatusModelError" int)

  let model_status_presolve_error =
    F.(constant "kHighsModelStatusPresolveError" int)

  let model_status_solve_error = F.(constant "kHighsModelStatusSolveError" int)

  let model_status_postsolve_error =
    F.(constant "kHighsModelStatusPostsolveError" int)

  let model_status_model_empty = F.(constant "kHighsModelStatusModelEmpty" int)

  let model_status_optimal = F.(constant "kHighsModelStatusOptimal" int)

  let model_status_infeasible = F.(constant "kHighsModelStatusInfeasible" int)

  let model_status_unbounded_or_infeasible =
    F.(constant "kHighsModelStatusUnboundedOrInfeasible" int)

  let model_status_unbounded = F.(constant "kHighsModelStatusUnbounded" int)

  let model_status_objective_bound =
    F.(constant "kHighsModelStatusObjectiveBound" int)

  let model_status_objective_target =
    F.(constant "kHighsModelStatusObjectiveTarget" int)

  let model_status_time_limit = F.(constant "kHighsModelStatusTimeLimit" int)

  let model_status_iteration_limit =
    F.(constant "kHighsModelStatusIterationLimit" int)

  let model_status_unknown = F.(constant "kHighsModelStatusUnknown" int)

  let model_status_solution_limit =
    F.(constant "kHighsModelStatusSolutionLimit" int)

  let model_status_interrupt = F.(constant "kHighsModelStatusInterrupt" int)
end
