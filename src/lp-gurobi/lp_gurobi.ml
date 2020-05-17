(* solve MIP using Gurobi *)

open Lp_grb
module C = Ctypes

let solve =
  (* TODO : now just showing how to make env and model, and then free these. *)
  let p_env = C.allocate env C.null in
  let _ = empty_env p_env in
  (* de-reference **env *)
  let e = C.(!@) p_env in
  let _ = start_env e in
  let p_model = C.allocate model C.null in
  let _ = new_model e p_model "model" 0 C.null C.null C.null C.null C.null in
  (* de-reference **model *)
  let m = C.(!@) p_model in
  let () = free_env e in
  let () = free_model m in
  ()
