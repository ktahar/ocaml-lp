module To_test = struct
  let g : Lp_glpk_js.glpk Js_of_ocaml.Js.t option ref = ref None
  let set_glpk glpk = g := Some glpk

  let get_glpk () =
    match !g with
    | Some glpk ->
        glpk
    | None ->
        failwith "glpk.js is not initialized"

  let to_list prob xs =
    List.map
      (fun v -> Lp.PMap.find (Lp.Poly.of_var v) xs)
      (Lp.Problem.uniq_vars prob)

  let solve_lp0 () =
    let p = Lp.read "lp0.lp" in
    match Lp_glpk_js.solve ~term_output:false (get_glpk ()) p with
    | Ok (obj, xs) ->
        obj :: to_list p xs
    | Error _ ->
        []

  let solve_milp0 () =
    let p = Lp.read "milp0.lp" in
    match Lp_glpk_js.solve ~term_output:false (get_glpk ()) p with
    | Ok (obj, xs) ->
        obj :: to_list p xs
    | Error _ ->
        []

  let solve_obj filename =
    let p = Lp.read filename in
    match Lp_glpk_js.solve ~term_output:false (get_glpk ()) p with
    | Ok (obj, _) ->
        Ok obj
    | Error msg ->
        Error msg

  let solve_lp0_async () =
    let p = Lp.read "lp0.lp" in
    let r = ref None in
    Lp_glpk_js.solve_async ~term_output:false (get_glpk ()) p (fun x ->
        r := Some x ) ;
    match !r with
    | Some (Ok (obj, xs)) ->
        obj :: to_list p xs
    | Some (Error _) | None ->
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

let solve_lp0_async () =
  Alcotest.(check (list (float 1e-7)))
    "solve_lp0_async" [1.2; 0.0; 1.2]
    (To_test.solve_lp0_async ())

let solve_lp_const () =
  match To_test.solve_obj "lp_const.lp" with
  | Ok obj ->
      Alcotest.(check (float 1e-7)) "solve_lp_const" 9.0 obj
  | Error msg ->
      Alcotest.failf "solve_lp_const failed: %s" msg

let solve_milp_const () =
  match To_test.solve_obj "milp_const.lp" with
  | Ok obj ->
      Alcotest.(check (float 1e-7)) "solve_milp_const" 6.0 obj
  | Error msg ->
      Alcotest.failf "solve_milp_const failed: %s" msg

let run_tests () =
  let open Alcotest in
  run "GlpkJs"
    [ ("solve lp0", [test_case "solve_lp0" `Quick solve_lp0])
    ; ("solve milp0", [test_case "solve_milp0" `Quick solve_milp0])
    ; ("solve lp_const", [test_case "solve_lp_const" `Quick solve_lp_const])
    ; ( "solve milp_const"
      , [test_case "solve_milp_const" `Quick solve_milp_const] )
    ; ("solve lp0 async", [test_case "solve_lp0_async" `Quick solve_lp0_async])
    ]

let () =
  Lp_glpk_js.require_glpk_async "glpk.js/node" (fun glpk ->
      To_test.set_glpk glpk ; run_tests () )
