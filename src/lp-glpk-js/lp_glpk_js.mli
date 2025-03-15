(** Interface to GLPK via glpk.js. *)

open Js_of_ocaml

(** JS type for variable *)
class type var = object
  method name : Js.js_string Js.t Js.prop
  method coef : float Js.prop
end

(** JS type for row (constraint) bound *)
class type row_bound = object
  method _type : int Js.prop
  method ub : float Js.prop
  method lb : float Js.prop
end

(** JS type for column (variable) bound *)
class type col_bound = object
  method name : Js.js_string Js.t Js.prop
  method _type : int Js.prop
  method ub : float Js.prop
  method lb : float Js.prop
end

(** JS type for objective *)
class type objective = object
  method direction : int Js.prop
  method name : Js.js_string Js.t Js.prop
  method vars : var Js.t Js.js_array Js.t Js.prop
end

(** JS type for solver options *)
class type options = object
  method mipgap : float Js.optdef Js.prop
  method tmlim : int Js.optdef Js.prop
  method msglev : int Js.optdef Js.prop
  method presol : bool Js.t Js.optdef Js.prop
end

(** JS type for constraints *)
class type cnstr = object
  method name : Js.js_string Js.t Js.prop
  method vars : var Js.t Js.js_array Js.t Js.prop
  method bnds : row_bound Js.t Js.prop
end

(** JS type for problem *)
class type prob = object
  method name : Js.js_string Js.t Js.prop
  method objective : objective Js.t Js.prop
  method subjectTo : cnstr Js.t Js.js_array Js.t Js.prop
  method bounds : col_bound Js.t Js.js_array Js.t Js.optdef Js.prop
  method binaries : Js.js_string Js.t Js.js_array Js.t Js.optdef Js.prop
  method generals : Js.js_string Js.t Js.js_array Js.t Js.optdef Js.prop
end

(** JS type for result content *)
class type res = object
  method status : int Js.prop
  method z : float Js.prop
  method vars : 'a Js.t Js.prop
  method dual : 'a Js.t Js.prop Js.optdef
end

(** JS type for optimization result *)
class type result = object
  method name : Js.js_string Js.t Js.prop
  method time : float Js.prop
  method result : res Js.t Js.prop
end

(** main glpk.js interface object *)
class type glpk = object
  (* direction *)
  method _GLP_MIN_ : int Js.readonly_prop
  method _GLP_MAX_ : int Js.readonly_prop

  (* type of auxiliary/structural variable *)
  method _GLP_FR_ : int Js.readonly_prop
  method _GLP_LO_ : int Js.readonly_prop
  method _GLP_UP_ : int Js.readonly_prop
  method _GLP_DB_ : int Js.readonly_prop
  method _GLP_FX_ : int Js.readonly_prop

  (* message level *)
  method _GLP_MSG_OFF_ : int Js.readonly_prop
  method _GLP_MSG_ERR_ : int Js.readonly_prop
  method _GLP_MSG_ON_ : int Js.readonly_prop
  method _GLP_MSG_ALL_ : int Js.readonly_prop
  method _GLP_MSG_DBG_ : int Js.readonly_prop

  (* solution status *)
  method _GLP_UNDEF_ : int Js.readonly_prop
  method _GLP_FEAS_ : int Js.readonly_prop
  method _GLP_INFEAS_ : int Js.readonly_prop
  method _GLP_NOFEAS_ : int Js.readonly_prop
  method _GLP_OPT_ : int Js.readonly_prop
  method _GLP_UNBND_ : int Js.readonly_prop

  (* version / solve *)
  method version : Js.js_string Js.t Js.prop
  method solve : prob Js.t -> result Js.t Js.meth
  method solve_opt : prob Js.t -> options Js.t -> result Js.t Js.meth
  method solve_msglev : prob Js.t -> int -> result Js.t Js.meth
end

val require_glpk : string -> glpk Js.t
(** require and instantiate glpk.js interface given by a string identifier.
   [let glpk = require_glpk "glpk.js"]
   is roughly equivalent to following JS.
   {[
   var GLPK = require("glpk.js");
   var glpk = GLPK();
   ]}
*)

val solve :
     ?term_output:bool
  -> glpk Js.t
  -> Lp.Problem.t
  -> (float * float Lp.PMap.t, string) Stdlib.result
(** Solve the problem using GLPK via glpk.js.
    GLPK can solve only linear problems (LP or MILP).
    glpk.js interface object ({!type:glpk} Js.t) must be given, that can be built with {!val:require_glpk}.
*)
