module To_test = struct
  let to_list prob xs =
    List.map
      (fun v -> Lp.PMap.find (Lp.Poly.of_var v) xs)
      (Lp.Problem.uniq_vars prob)

  let solve ~cmd p = if cmd then Lp_highs.Cmd.solve p else Lp_highs.solve p

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

let solve_lp0_default () =
  Alcotest.(check (list (float 1e-7)))
    "solve_lp0_default" [1.2; 0.0; 1.2] (To_test.solve_lp0 ())

let solve_lp0_cmd () =
  Alcotest.(check (list (float 1e-7)))
    "solve_lp0_cmd" [1.2; 0.0; 1.2]
    (To_test.solve_lp0 ~cmd:true ())

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

let () =
  let open Alcotest in
  run "HiGHS"
    [ ( "solve lp0"
      , [ test_case "solve_lp0_default" `Quick solve_lp0_default
        ; test_case "solve_lp0_cmd" `Quick solve_lp0_cmd ] )
    ; ( "solve milp0"
      , [ test_case "solve_milp0_default" `Quick solve_milp0_default
        ; test_case "solve_milp0_cmd" `Quick solve_milp0_cmd ] ) ]
