module Term = Lp.Term

module To_test = struct
  let to_string () = Term.to_string (Term.var "a")
end

let test_to_string () =
  Alcotest.(check string)
    "to_string" "+ 1.000000000000000000e+00 a" (To_test.to_string ())

let () =
  let open Alcotest in
  run "Term" [("term", [test_case "To string" `Quick test_to_string])]
