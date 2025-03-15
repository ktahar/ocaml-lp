module Term = Lp.Term
module Poly = Lp.Poly

let x = Poly.var "x"
let y = Poly.var "y"
let z = Poly.var "z"
let a = Poly.var "a"
let b = Poly.var "b"

let p0 =
  let open Poly in
  (x *~ x) ++ (x *~ y) ++ (c 3. *~ x) ++ (c 2. *~ y) ++ z

module To_test = struct
  let equiv0 () = Poly.(equiv p0 p0)
  let equiv1 () = Poly.(equiv p0 (p0 ++ c 1.))

  let decompose0 () =
    let dec = Poly.decompose p0 in
    List.length dec.lcs = 3 && List.length dec.qcs = 2

  let expand0 () =
    Poly.(
      equiv
        (((c 2. *~ x) ++ y) *~ ((c 3. *~ x) ++ y ++ c 5.))
        ( (c 6. *~ x *~ x)
        ++ (y *~ y)
        ++ (c 5. *~ x *~ y)
        ++ (c 10. *~ x)
        ++ (c 5. *~ y) ) )

  let dot0 () =
    Poly.(
      equiv
        (dot ((c 2. *~ x) ++ y) (x ++ c 3.))
        ((c 2. *~ x *~ x) ++ (c 3. *~ y)) )

  let divt0 () =
    let t2x = Term.(mul (c 2.) (var "x")) in
    Poly.(
      equiv
        (divt ((c 2. *~ x *~ x) ++ (c 4. *~ x *~ y) ++ (c 6. *~ x)) t2x)
        (x ++ (c 2. *~ y) ++ c 3.) )

  let div0 () =
    Poly.(
      equiv
        (div ((c 2. *~ x *~ x) ++ (c 7. *~ x) -- c 15.) (x ++ c 5.))
        ((c 2. *~ x) -- c 3.) )

  let div1 () =
    Poly.(
      equiv
        (div
           ((c 6. *~ x *~ x) ++ (((c 2. *~ (a ++ b)) ++ c 3.) *~ x) ++ a ++ b)
           ((c 2. *~ x) ++ c 1.) )
        ((c 3. *~ x) ++ a ++ b) )
end

let equiv0 () = Alcotest.(check bool) "equiv0" true (To_test.equiv0 ())
let equiv1 () = Alcotest.(check bool) "equiv1" false (To_test.equiv1 ())

let decompose0 () =
  Alcotest.(check bool) "decompose0" true (To_test.decompose0 ())

let expand0 () = Alcotest.(check bool) "expand0" true (To_test.expand0 ())
let dot0 () = Alcotest.(check bool) "dot0" true (To_test.dot0 ())
let divt0 () = Alcotest.(check bool) "divt0" true (To_test.divt0 ())
let div0 () = Alcotest.(check bool) "div0" true (To_test.div0 ())
let div1 () = Alcotest.(check bool) "div1" true (To_test.div1 ())

let () =
  let open Alcotest in
  run "Poly"
    [ ("equiv0", [test_case "equiv0" `Quick equiv0])
    ; ("equiv1", [test_case "equiv1" `Quick equiv1])
    ; ("decompose0", [test_case "decompose0" `Quick decompose0])
    ; ("expand0", [test_case "expand0" `Quick expand0])
    ; ("dot0", [test_case "dot0" `Quick dot0])
    ; ("divt0", [test_case "divt0" `Quick divt0])
    ; ("div0", [test_case "div0" `Quick div0])
    ; ("div1", [test_case "div1" `Quick div1]) ]
