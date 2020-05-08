module To_test = struct
  let simplex_lp0 () =
    let p = Lp.read "lp0.lp" in
    fst (Lp.Glpk.solve_simplex p)
end

let simplex_lp0 () =
  Alcotest.(check (float 1e-7)) "simplex_lp0" 1.2 (To_test.simplex_lp0 ())

let () =
  let open Alcotest in
  run "Glpk" [("simplex lp0", [test_case "simplex_lp0" `Quick simplex_lp0])]
