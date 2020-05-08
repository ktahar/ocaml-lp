open Lp.Glp
module C = Ctypes

let prob = create_prob ()

let smcp = C.make Smcp.t

let () = init_smcp (C.addr smcp)

module To_test = struct
  let set_get_pname () =
    set_prob_name prob "problem0" ;
    get_prob_name prob

  let smcp_msg_lev () =
    match C.getf smcp Smcp.msg_lev with Msg.ALL -> true | _ -> false

  let smcp_r_test () =
    match C.getf smcp Smcp.r_test with Smcp.Rt.HAR -> true | _ -> false

  let smcp_it_lim () = C.getf smcp Smcp.it_lim

  let smcp_presolve () = C.getf smcp Smcp.presolve
end

let set_get_pname () =
  Alcotest.(check string) "set_get_pname" "problem0" (To_test.set_get_pname ())

let smcp_msg_lev () =
  Alcotest.(check bool) "smcp_msg_lev" true (To_test.smcp_msg_lev ())

let smcp_r_test () =
  Alcotest.(check bool) "smcp_r_test" true (To_test.smcp_r_test ())

let smcp_it_lim () =
  Alcotest.(check int)
    "smcp_lt_lim"
    (Int32.to_int Int32.max_int)
    (To_test.smcp_it_lim ())

let smcp_presolve () =
  Alcotest.(check bool) "smcp_presolve" false (To_test.smcp_presolve ())

let () =
  let open Alcotest in
  run "Glp"
    [ ("set_get_pname", [test_case "set_get_pname" `Quick set_get_pname])
    ; ("smcp_msg_lev", [test_case "smcp_msg_lev" `Quick smcp_msg_lev])
    ; ("smcp_r_test", [test_case "smcp_r_test" `Quick smcp_r_test])
    ; ("smcp_it_lim", [test_case "smcp_it_lim" `Quick smcp_it_lim])
    ; ("smcp_presolve", [test_case "smcp_presolve" `Quick smcp_presolve]) ]
