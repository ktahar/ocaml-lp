module To_test = struct
  let to_list prob xs =
    List.map
      (fun v -> Lp.PMap.find (Lp.Poly.of_var v) xs)
      (Lp.Problem.uniq_vars prob)

  let solve ~cmd p = if cmd then Lp_highs.Cmd.solve p else Lp_highs.solve p

  let solve_file ?(cmd = false) filename =
    let p = Lp.read filename in
    solve ~cmd p

  let solve_obj ?(cmd = false) filename =
    match solve_file ~cmd filename with
    | Ok (obj, _) ->
        Ok obj
    | Error msg ->
        Error msg

  let solve_obj_float ?(cmd = false) filename =
    match solve_obj ~cmd filename with Ok obj -> obj | Error _ -> nan

  let solve_lp0 ?(cmd = false) () =
    let p = Lp.read "lp0.lp" in
    match solve ~cmd p with
    | Ok (obj, xs) ->
        obj :: to_list p xs
    | Error _ ->
        []

  let solve_milp0 ?(cmd = false) () =
    let p = Lp.read "milp0.lp" in
    match solve ~cmd p with
    | Ok (obj, xs) ->
        obj :: to_list p xs
    | Error _ ->
        []
end

let contains s sub =
  let ls = String.length s in
  let lsub = String.length sub in
  let rec loop i =
    if i + lsub > ls then false
    else if String.sub s i lsub = sub then true
    else loop (i + 1)
  in
  if lsub = 0 then true else loop 0

let solve_lp0_default () =
  Alcotest.(check (list (float 1e-7)))
    "solve_lp0_default" [1.2; 0.0; 1.2] (To_test.solve_lp0 ())

let solve_lp0_cmd () =
  Alcotest.(check (list (float 1e-7)))
    "solve_lp0_cmd" [1.2; 0.0; 1.2]
    (To_test.solve_lp0 ~cmd:true ())

let solve_lp1_default () =
  Alcotest.(check (float 1e-5))
    "solve_lp1_default" 265.0
    (To_test.solve_obj_float "lp1.lp")

let solve_lp1_cmd () =
  Alcotest.(check (float 1e-5))
    "solve_lp1_cmd" 265.0
    (To_test.solve_obj_float ~cmd:true "lp1.lp")

let solve_milp0_default () =
  Alcotest.(check (list (float 1e-7)))
    "solve_milp0_default"
    [-1.75; 1.0; -5.5; 5.25; 3.0]
    (To_test.solve_milp0 ())

let solve_milp0_cmd () =
  Alcotest.(check (list (float 1e-7)))
    "solve_milp0_cmd"
    [-1.75; 1.0; -5.5; 5.25; 3.0]
    (To_test.solve_milp0 ~cmd:true ())

let solve_milp1_default () =
  Alcotest.(check (float 1e-5))
    "solve_milp1_default" 142.0
    (To_test.solve_obj_float "milp1.lp")

let solve_milp1_cmd () =
  Alcotest.(check (float 1e-5))
    "solve_milp1_cmd" 142.0
    (To_test.solve_obj_float ~cmd:true "milp1.lp")

let solve_qp0_default () =
  Alcotest.(check (float 1e-6))
    "solve_qp0_default" 2.11111111111111
    (To_test.solve_obj_float "qp0.lp")

let solve_qp0_cmd () =
  Alcotest.(check (float 1e-6))
    "solve_qp0_cmd" 2.11111111111111
    (To_test.solve_obj_float ~cmd:true "qp0.lp")

let solve_qp1_default () =
  Alcotest.(check (float 1e-1))
    "solve_qp1_default" 331.2
    (To_test.solve_obj_float "qp1.lp")

let solve_qp1_default_cmd_consistent () =
  let obj_default =
    match To_test.solve_obj "qp1.lp" with
    | Ok obj ->
        obj
    | Error msg ->
        Alcotest.failf "solve_qp1_default failed: %s" msg
  in
  let obj_cmd =
    match To_test.solve_obj ~cmd:true "qp1.lp" with
    | Ok obj ->
        obj
    | Error msg ->
        Alcotest.failf "solve_qp1_cmd failed: %s" msg
  in
  Alcotest.(check (float 1e-6))
    "solve_qp1_default_cmd_consistent" obj_cmd obj_default

let solve_qp_nonconvex_rejected () =
  match To_test.solve_obj "qp_nonconvex.lp" with
  | Ok obj ->
      Alcotest.failf
        "non-convex QP should be rejected by HiGHS, but returned objective \
         %.12g"
        obj
  | Error msg ->
      let lmsg = String.lowercase_ascii msg in
      let looks_like_solver_rejection =
        contains lmsg "highs_run failed" || contains lmsg "status error"
      in
      Alcotest.(check bool)
        "solve_qp_nonconvex_rejected" true looks_like_solver_rejection

let solve_const_obj ?(eps = 1e-7) filename expected =
  let obj_default =
    match To_test.solve_obj filename with
    | Ok obj ->
        obj
    | Error msg ->
        Alcotest.failf "solve %s (default) failed: %s" filename msg
  in
  let obj_cmd =
    match To_test.solve_obj ~cmd:true filename with
    | Ok obj ->
        obj
    | Error msg ->
        Alcotest.failf "solve %s (cmd) failed: %s" filename msg
  in
  Alcotest.(check (float eps))
    (filename ^ "_default_expected")
    expected obj_default ;
  Alcotest.(check (float eps)) (filename ^ "_cmd_expected") expected obj_cmd

let solve_lp_const_offset () = solve_const_obj "lp_const.lp" 9.0
let solve_milp_const_offset () = solve_const_obj "milp_const.lp" 6.0
let solve_qp_const_offset () = solve_const_obj ~eps:1e-6 "qp_const.lp" 5.0

let () =
  let open Alcotest in
  run "HiGHS"
    [ ( "solve lp0"
      , [ test_case "solve_lp0_default" `Quick solve_lp0_default
        ; test_case "solve_lp0_cmd" `Quick solve_lp0_cmd ] )
    ; ( "solve lp1"
      , [ test_case "solve_lp1_default" `Quick solve_lp1_default
        ; test_case "solve_lp1_cmd" `Quick solve_lp1_cmd ] )
    ; ( "solve milp0"
      , [ test_case "solve_milp0_default" `Quick solve_milp0_default
        ; test_case "solve_milp0_cmd" `Quick solve_milp0_cmd ] )
    ; ( "solve milp1"
      , [ test_case "solve_milp1_default" `Quick solve_milp1_default
        ; test_case "solve_milp1_cmd" `Quick solve_milp1_cmd ] )
    ; ( "solve qp0"
      , [ test_case "solve_qp0_default" `Quick solve_qp0_default
        ; test_case "solve_qp0_cmd" `Quick solve_qp0_cmd ] )
    ; ( "solve qp1"
      , [ test_case "solve_qp1_default" `Quick solve_qp1_default
        ; test_case "solve_qp1_default_cmd_consistent" `Quick
            solve_qp1_default_cmd_consistent ] )
    ; ( "solve qp_nonconvex"
      , [ test_case "solve_qp_nonconvex_rejected" `Quick
            solve_qp_nonconvex_rejected ] )
    ; ( "solve objective offsets"
      , [ test_case "solve_lp_const_offset" `Quick solve_lp_const_offset
        ; test_case "solve_milp_const_offset" `Quick solve_milp_const_offset
        ; test_case "solve_qp_const_offset" `Quick solve_qp_const_offset ] ) ]
