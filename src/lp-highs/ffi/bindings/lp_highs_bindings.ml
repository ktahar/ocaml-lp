open Ctypes
module T = Lp_highs_types.M

module M (F : Ctypes.FOREIGN) = struct
  open F

  type highs = unit ptr

  let highs : highs typ = ptr void

  let version = foreign "Highs_version" (void @-> returning string)

  let create = foreign "Highs_create" (void @-> returning highs)

  let destroy = foreign "Highs_destroy" (highs @-> returning void)

  let get_infinity = foreign "Highs_getInfinity" (highs @-> returning double)

  let set_bool_option_value =
    foreign "Highs_setBoolOptionValue"
      (highs @-> string @-> T.HighsInt.t @-> returning T.Status.t)

  let set_int_option_value =
    foreign "Highs_setIntOptionValue"
      (highs @-> string @-> T.HighsInt.t @-> returning T.Status.t)

  let set_double_option_value =
    foreign "Highs_setDoubleOptionValue"
      (highs @-> string @-> double @-> returning T.Status.t)

  let set_string_option_value =
    foreign "Highs_setStringOptionValue"
      (highs @-> string @-> string @-> returning T.Status.t)

  let get_option_type =
    foreign "Highs_getOptionType"
      (highs @-> string @-> ptr T.HighsInt.t @-> returning T.Status.t)

  let pass_lp =
    foreign "Highs_passLp"
      ( highs @-> T.HighsInt.t @-> T.HighsInt.t @-> T.HighsInt.t
      @-> T.MatrixFormat.t @-> T.ObjSense.t @-> double @-> ptr double
      @-> ptr double @-> ptr double @-> ptr double @-> ptr double
      @-> ptr T.HighsInt.t @-> ptr T.HighsInt.t @-> ptr double
      @-> returning T.Status.t )

  let pass_mip =
    foreign "Highs_passMip"
      ( highs @-> T.HighsInt.t @-> T.HighsInt.t @-> T.HighsInt.t
      @-> T.MatrixFormat.t @-> T.ObjSense.t @-> double @-> ptr double
      @-> ptr double @-> ptr double @-> ptr double @-> ptr double
      @-> ptr T.HighsInt.t @-> ptr T.HighsInt.t @-> ptr double
      @-> ptr T.VarType.t @-> returning T.Status.t )

  let run = foreign "Highs_run" (highs @-> returning T.Status.t)

  let get_solution =
    foreign "Highs_getSolution"
      ( highs @-> ptr double @-> ptr double @-> ptr double @-> ptr double
      @-> returning T.Status.t )

  let get_model_status =
    foreign "Highs_getModelStatus" (highs @-> returning T.ModelStatus.t)

  let get_objective_value =
    foreign "Highs_getObjectiveValue" (highs @-> returning double)

  let get_num_col = foreign "Highs_getNumCol" (highs @-> returning T.HighsInt.t)

  let get_num_row = foreign "Highs_getNumRow" (highs @-> returning T.HighsInt.t)
end
