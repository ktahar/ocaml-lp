module Var = Lp.Var

let x = Var.make "x" ~lb:neg_infinity ~ub:infinity
let y = Var.make "y" ~lb:neg_infinity ~ub:3.0
let xi = Var.make "x" ~integer:true ~lb:neg_infinity ~ub:infinity
let yi = Var.make "y" ~integer:true ~lb:(-1.0) ~ub:infinity
let x_0_1 = Var.make "x_0_1"
let x_1_1 = Var.make "x_1_1"
let x_2_1 = Var.make "x_2_1"
let x_10_1 = Var.make "x_10_1"
let sort vars = List.sort Var.compare_name vars
let to_string_list = List.map Var.to_string

let to_bound_string_list =
  List.map (fun x ->
      Option.value ~default:"" (Var.to_bound_string ~short:true x) )

module To_test = struct
  let sort0 () = sort [x_10_1; y; x_1_1; x_2_1; x_0_1] |> to_string_list
  let bound_strs0 () = to_bound_string_list [x; y; xi; yi]
end

let sort0 () =
  Alcotest.(check (list string))
    "sort0"
    ["x_0_1"; "x_1_1"; "x_2_1"; "x_10_1"; "y"]
    (To_test.sort0 ())

let bound_strs0 () =
  Alcotest.(check (list string))
    "bound_strs0"
    [ "-inf <= x <= inf"
    ; "-inf <= y <= 3.00"
    ; "-inf <= x <= inf"
    ; "-1 <= y <= inf" ]
    (To_test.bound_strs0 ())

let () =
  let open Alcotest in
  run "Var"
    [ ("sort0", [test_case "sort0" `Quick sort0])
    ; ("bound_strs0", [test_case "bound_strs0" `Quick bound_strs0]) ]
