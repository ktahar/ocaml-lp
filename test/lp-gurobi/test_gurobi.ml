module To_test = struct
  let solve_obj filename =
    let p = Lp.read filename in
    match Lp_gurobi.solve p with Ok (obj, _) -> obj | Error _ -> nan
end

let solve_lp0 () =
  Alcotest.(check (float 1e-7)) "solve_lp0" 1.2 (To_test.solve_obj "lp0.lp")

let solve_lp1 () =
  Alcotest.(check (float 1e-5)) "solve_lp1" 265.0 (To_test.solve_obj "lp1.lp")

let solve_milp0 () =
  Alcotest.(check (float 1e-7))
    "solve_milp0" (-1.75)
    (To_test.solve_obj "milp0.lp")

let solve_milp1 () =
  Alcotest.(check (float 1e-5))
    "solve_milp1" 142.0
    (To_test.solve_obj "milp1.lp")

let solve_qp0 () =
  Alcotest.(check (float 1e-3)) "solve_qp0" 2.11111 (To_test.solve_obj "qp0.lp")

let solve_qp1 () =
  Alcotest.(check (float 1e-1)) "solve_qp1" 331.2 (To_test.solve_obj "qp1.lp")

let solve_iqp0 () =
  Alcotest.(check (float 1e-7)) "solve_iqp0" 3.0 (To_test.solve_obj "iqp0.lp")

let solve_qcp0 () =
  Alcotest.(check (float 1e-3))
    "solve_qcp0" 0.326992
    (To_test.solve_obj "qcp0.lp")

let solve_lp_const () =
  Alcotest.(check (float 1e-7))
    "solve_lp_const" 9.0
    (To_test.solve_obj "lp_const.lp")

let solve_milp_const () =
  Alcotest.(check (float 1e-7))
    "solve_milp_const" 6.0
    (To_test.solve_obj "milp_const.lp")

let () =
  let open Alcotest in
  run "Gurobi"
    [ ("solve_lp0", [test_case "solve_lp0" `Quick solve_lp0])
    ; ("solve_lp1", [test_case "solve_lp1" `Quick solve_lp1])
    ; ("solve_milp0", [test_case "solve_milp0" `Quick solve_milp0])
    ; ("solve_milp1", [test_case "solve_milp1" `Quick solve_milp1])
    ; ("solve_lp_const", [test_case "solve_lp_const" `Quick solve_lp_const])
    ; ( "solve_milp_const"
      , [test_case "solve_milp_const" `Quick solve_milp_const] )
    ; ("solve_qp0", [test_case "solve_qp0" `Quick solve_qp0])
    ; ("solve_qp1", [test_case "solve_qp1" `Quick solve_qp1])
    ; ("solve_iqp0", [test_case "solve_iqp0" `Quick solve_iqp0])
    ; ("solve_qcp0", [test_case "solve_qcp0" `Quick solve_qcp0]) ]
