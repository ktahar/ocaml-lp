let () =
  let x = Lp.var ~lb:0. ~ub:1. "x" in
  let y = Lp.var ~lb:0. ~ub:1. "y" in
  let pb =
    let open Lp in
    make (maximize (x ++ y)) [x ++ y <~ x]
  in
  match Lp_glpk.Simplex.solve ~it_lim:(Some 0) ~term_output:false pb with
  | Ok (obj, res) ->
      Option.iter (Printf.printf "%f %f\n" obj) (Lp.PMap.find_opt x res)
  | _ ->
      failwith "failing test"
