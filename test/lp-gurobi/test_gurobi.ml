module To_test = struct
  let solve_lp0 () =
    let p = Lp.read "lp0.lp" in
    match Lp_gurobi.solve p with Ok (ov, _) -> ov | Error _ -> 0.0

  let solve_milp0 () =
    let p = Lp.read "milp0.lp" in
    match Lp_gurobi.solve p with Ok (ov, _) -> ov | Error _ -> 0.0

  let solve_qp0 () =
    let p = Lp.read "qp0.lp" in
    match Lp_gurobi.solve p with Ok (ov, _) -> ov | Error _ -> 0.0

  let solve_iqp0 () =
    let p = Lp.read "iqp0.lp" in
    match Lp_gurobi.solve p with Ok (ov, _) -> ov | Error _ -> 0.0

  let solve_qcp0 () =
    let p = Lp.read "qcp0.lp" in
    match Lp_gurobi.solve p with Ok (ov, _) -> ov | Error _ -> 0.0
end

let solve_lp0 () =
  Alcotest.(check (float 1e-7)) "solve_lp0" 1.2 (To_test.solve_lp0 ())

let solve_milp0 () =
  Alcotest.(check (float 1e-7)) "solve_milp0" 5.0 (To_test.solve_milp0 ())

let solve_qp0 () =
  Alcotest.(check (float 1e-3)) "solve_qp0" 2.11111 (To_test.solve_qp0 ())

let solve_iqp0 () =
  Alcotest.(check (float 1e-7)) "solve_iqp0" 3.0 (To_test.solve_iqp0 ())

let solve_qcp0 () =
  Alcotest.(check (float 1e-3)) "solve_qcp0" 0.326992 (To_test.solve_qcp0 ())

let () =
  let open Alcotest in
  run "Gurobi"
    [ ("solve_lp0", [test_case "solve_lp0" `Quick solve_lp0])
    ; ("solve_milp0", [test_case "solve_milp0" `Quick solve_milp0])
    ; ("solve_qp0", [test_case "solve_qp0" `Quick solve_qp0])
    ; ("solve_iqp0", [test_case "solve_iqp0" `Quick solve_iqp0])
    ; ("solve_qcp0", [test_case "solve_qcp0" `Quick solve_qcp0]) ]
