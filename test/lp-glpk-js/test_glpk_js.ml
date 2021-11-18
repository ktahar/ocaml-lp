module To_test = struct
  let g = Lp_glpk_js.require_glpk "glpk.js"

  let to_list prob xs =
    List.map
      (fun v -> Lp.PMap.find (Lp.Poly.of_var v) xs)
      (Lp.Problem.uniq_vars prob)

  let solve_lp0 () =
    let p = Lp.read "lp0.lp" in
    match Lp_glpk_js.solve ~term_output:false g p with
    | Ok (obj, xs) ->
        obj :: to_list p xs
    | Error _ ->
        []

  let solve_milp0 () =
    let p = Lp.read "milp0.lp" in
    match Lp_glpk_js.solve ~term_output:false g p with
    | Ok (obj, xs) ->
        obj :: to_list p xs
    | Error _ ->
        []
end

let solve_lp0 () =
  Alcotest.(check (list (float 1e-7)))
    "solve_lp0" [1.2; 0.0; 1.2] (To_test.solve_lp0 ())

let solve_milp0 () =
  Alcotest.(check (list (float 1e-7)))
    "solve_milp0"
    [-1.75; 1.0; -5.5; 5.25; 3.0]
    (To_test.solve_milp0 ())

let () =
  let open Alcotest in
  run "GlpkJs"
    [ ("solve lp0", [test_case "solve_lp0" `Quick solve_lp0])
    ; ("solve milp0", [test_case "solve_milp0" `Quick solve_milp0]) ]
