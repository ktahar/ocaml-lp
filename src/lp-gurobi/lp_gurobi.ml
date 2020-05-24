(* solve MIP using Gurobi *)

open Lp_grb
module C = Ctypes

let solve =
  (* TODO : now just showing how to make env and model, and then free these. *)
  let env = empty_env () in
  try
    let model = new_model env "model" [] [] [] [] [] in
    try
      ()
    with e -> (free_model env model ; raise e)
  with e -> free_env env ; raise e
