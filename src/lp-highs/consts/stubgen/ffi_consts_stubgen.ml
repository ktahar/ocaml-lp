let prologue = "#include <interfaces/highs_c_api.h>"

let () =
  print_endline prologue ;
  Cstubs.Types.write_c Format.std_formatter (module Lp_highs_bindings_consts.M)
