module To_test = struct
  let simplex_lp0 () =
    let p = Lp.read "lp0.lp" in
    match Lp_glpk.Simplex.solve p with Ok (ov, _) -> ov | Error _ -> 0.0

  let mip_milp0 () =
    let p = Lp.read "milp0.lp" in
    match Lp_glpk.Mip.solve p with Ok (ov, _) -> ov | Error _ -> 0.0
end

let simplex_lp0 () =
  Alcotest.(check (float 1e-7)) "simplex_lp0" 1.2 (To_test.simplex_lp0 ())

let mip_milp0 () =
  Alcotest.(check (float 1e-7)) "mip_milp0" 5.0 (To_test.mip_milp0 ())

let () =
  let open Alcotest in
  run "Glpk"
    [ ("simplex lp0", [test_case "simplex_lp0" `Quick simplex_lp0])
    ; ("mip milp0", [test_case "mip_milp0" `Quick mip_milp0]) ]
