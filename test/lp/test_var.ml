module Var = Lp.Var

let x = Var.make "x"
let y = Var.make "y"
let x_0_1 = Var.make "x_0_1"
let x_1_1 = Var.make "x_1_1"
let x_2_1 = Var.make "x_2_1"
let x_10_1 = Var.make "x_10_1"
let sort vars = List.sort Var.compare_name vars
let to_string_list = List.map Var.to_string

module To_test = struct
  let sort0 () = sort [x_10_1; y; x_1_1; x_2_1; x_0_1] |> to_string_list
end

let sort0 () =
  Alcotest.(check (list string))
    "sort0"
    ["x_0_1"; "x_1_1"; "x_2_1"; "x_10_1"; "y"]
    (To_test.sort0 ())

let () =
  let open Alcotest in
  run "Var" [("sort0", [test_case "sort0" `Quick sort0])]
