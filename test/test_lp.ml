let lp0 =
  let open Lp in
  let x = var "x" in
  let y = var "y" in
  let c0 = x + c 1.2 * y <$ c 5.0 in
  let c1 = c 2.0 * x + y <$ c 1.2 in
  let obj = maximize (x + y) in
  let cnstrs = [c0; c1] in
  (obj, cnstrs)

let lp0s =
  {|maximize
 + 1.000000000000000000e+00 x + 1.000000000000000000e+00 y
subject to
 + 1.000000000000000000e+00 x + 1.199999999999999956e+00 y <= + 5.000000000000000000e+00
 + 2.000000000000000000e+00 x + 1.000000000000000000e+00 y <= + 1.199999999999999956e+00
end|}

let lp0s_comment =
  {|maximize \ bra bra
 + 1.000000000000000000e+00 x + 1.000000000000000000e+00 y
subject to
\yeah yeah
 + 1.000000000000000000e+00 x + 1.199999999999999956e+00 y <= + 5.000000000000000000e+00 \ la la
 + 2.000000000000000000e+00 x + 1.000000000000000000e+00 y <= + 1.199999999999999956e+00
end
\ ya ya|}

let bad0 =
  (* invalid model with variable collision *)
  let open Lp in
  let x = var "x" in
  let y = var "x" ~integer:true in
  let c0 = x + c 1.2 * y <$ c 5.0 in
  let c1 = c 2.0 * x + y <$ c 1.2 in
  let obj = minimize (x + y) in
  let cnstrs = [c0; c1] in
  (obj, cnstrs)

let mip0s =
  {|maximize
 + 1.00 x + 1.00 y + 1.00 z + [ + 3.00 b * w ] / 2
subject to
 - 1.00 w + 1.00 x + 2.00 y <= + 5.00
 + 2.00 x + [ + 1.00 b * y + 1.00 x * z ] <= + 2.00
bounds
 2.00 <= y <= 10.00
 0 <= z <= 3
general
 w z
binary
 b
end|}

let mip0 =
  let open Lp in
  let x = var "x" in
  let y = var "y" ~lb:2.0 ~ub:10.0 in
  let z = var ~integer:true ~ub:3.0 "z" in
  let w = var "w" ~integer:true in
  let b = binary "b" in
  let c0 = ~- w + x + c 2.0 * y <$ c 5.0 in
  let c1 = c 2.0 * x + b * y + x * z <$ c 2.0 in
  let obj = maximize (x + y + z + c 3.0 * w * b) in
  let cnstrs = [c0; c1] in
  (obj, cnstrs)

module To_test = struct
  let validate_lp0 () = Lp.validate lp0

  let validate_bad0 () = Lp.validate bad0

  let validate_mip0 () = Lp.validate mip0

  let lp0_to_string () = Lp.to_string lp0

  let lp0_of_to_string () = Lp.to_string (Lp.of_string lp0s)

  let lp0_read_to_string () = Lp.to_string (Lp.read "lp0.lp")

  let lp0_write_read_to_string () =
    Lp.write "_lp0.lp" lp0 ;
    Lp.to_string (Lp.read "_lp0.lp")

  let lp0_comment () = Lp.to_string (Lp.of_string lp0s_comment)

  let mip0_read_to_string () = Lp.to_string ~short:true (Lp.read "mip0.lp")

  let mip0a_read_to_string () = Lp.to_string ~short:true (Lp.read "mip0a.lp")

  let mip0_to_string () = Lp.to_string ~short:true mip0
end

let validate_lp0 () =
  Alcotest.(check bool) "validate_lp0" true (To_test.validate_lp0 ())

let validate_bad0 () =
  Alcotest.(check bool) "validate_bad0" false (To_test.validate_bad0 ())

let validate_mip0 () =
  Alcotest.(check bool) "validate_mip0" true (To_test.validate_mip0 ())

let lp0_to_string () =
  Alcotest.(check string) "lp0_to_string" lp0s (To_test.lp0_to_string ())

let lp0_of_to_string () =
  Alcotest.(check string) "lp0_of_to_string" lp0s (To_test.lp0_of_to_string ())

let lp0_read_to_string () =
  Alcotest.(check string)
    "lp0_read_to_string" lp0s
    (To_test.lp0_read_to_string ())

let lp0_write_read_to_string () =
  Alcotest.(check string)
    "lp0_write_read_to_string" (To_test.lp0_to_string ())
    (To_test.lp0_write_read_to_string ())

let lp0_comment () =
  Alcotest.(check string) "lp0_comment" lp0s (To_test.lp0_comment ())

let lp0_model_string () =
  Alcotest.(check string)
    "lp0 model string" (To_test.lp0_to_string ())
    (To_test.lp0_read_to_string ())

let mip0_to_string () =
  Alcotest.(check string) "mip0 to string" mip0s (To_test.mip0_to_string ())

let mip0_model_string () =
  Alcotest.(check string)
    "mip0 model string"
    (To_test.mip0_to_string ())
    (To_test.mip0_read_to_string ())

let mip0a_model_string () =
  Alcotest.(check string)
    "mip0a model string"
    (To_test.mip0_to_string ())
    (To_test.mip0a_read_to_string ())

let () =
  let open Alcotest in
  run "Lp"
    [ ("lp0 validation", [test_case "validate lp0" `Quick validate_lp0])
    ; ("bad0 validation", [test_case "validate bad0" `Quick validate_bad0])
    ; ("mip0 validation", [test_case "validate mip0" `Quick validate_mip0])
    ; ("lp0 string format", [test_case "lp0_to_string" `Quick lp0_to_string])
    ; ("lp0 string read", [test_case "lp0_of_to_string" `Quick lp0_of_to_string])
    ; ( "lp0 read file"
      , [test_case "lp0_read_to_string" `Quick lp0_read_to_string] )
    ; ("lp0 commented string read", [test_case "lp0_comment" `Quick lp0_comment])
    ; ( "lp0 write read to string"
      , [test_case "lp0_write_read_to_string" `Quick lp0_write_read_to_string]
      )
    ; ( "lp0 model string"
      , [test_case "lp0_model_string" `Quick lp0_model_string] )
    ; ("mip0 to string", [test_case "mip0_to_string" `Quick mip0_to_string])
    ; ( "mip0 model string"
      , [test_case "mip0_model_string" `Quick mip0_model_string] )
    ; ( "mip0a model string"
      , [test_case "mip0a_model_string" `Quick mip0a_model_string] ) ]
