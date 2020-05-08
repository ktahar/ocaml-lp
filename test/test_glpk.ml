open Lp.Glpk

let prob = create_prob ()

module To_test = struct
  let set_get_pname () =
    set_prob_name prob "problem0" ;
    get_prob_name prob
end

let set_get_pname () =
  Alcotest.(check string) "set_get_pname" "problem0" (To_test.set_get_pname ())

let () =
  let open Alcotest in
  run "Glpk"
    [("set_get_pname", [test_case "set_get_pname" `Quick set_get_pname])]
