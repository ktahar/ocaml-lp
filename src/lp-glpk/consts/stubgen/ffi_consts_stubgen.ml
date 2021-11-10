let prefix = "lp_glpk_stub"

let prologue = "#include <glpk.h>"

let () =
  print_endline prologue ;
  Cstubs.Types.write_c Format.std_formatter (module Lp_glpk_bindings_consts.M)
