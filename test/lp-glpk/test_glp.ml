open Lp_glp
module C = Ctypes

let prob = create_prob ()
let smcp = C.make Smcp.t
let () = init_smcp (C.addr smcp)
let iocp = C.make Iocp.t
let () = init_iocp (C.addr iocp)
let near_eq f0 f1 = Float.abs (f0 -. f1) < 1e-12
let max_int32 = Int32.to_int Int32.max_int

module To_test = struct
  let set_get_pname () =
    set_prob_name prob "problem0" ;
    get_prob_name prob

  (* we cannot use ( = ) to compare Ctypes structure *)
  let smcp_default () =
    List.for_all Fun.id
      [ (match C.getf smcp Smcp.msg_lev with Msg.ALL -> true | _ -> false)
      ; ( match C.getf smcp Smcp.meth with
        | Smcp.Meth.PRIMAL ->
            true
        | _ ->
            false )
      ; (match C.getf smcp Smcp.pricing with Smcp.Pt.PSE -> true | _ -> false)
      ; (match C.getf smcp Smcp.r_test with Smcp.Rt.HAR -> true | _ -> false)
      ; near_eq 1e-7 (C.getf smcp Smcp.tol_bnd)
      ; near_eq 1e-7 (C.getf smcp Smcp.tol_dj)
      ; near_eq 1e-9 (C.getf smcp Smcp.tol_piv)
      ; ~-.max_float = C.getf smcp Smcp.obj_ll
      ; max_float = C.getf smcp Smcp.obj_ul
      ; max_int32 = C.getf smcp Smcp.it_lim
      ; max_int32 = C.getf smcp Smcp.tm_lim
      ; 5000 = C.getf smcp Smcp.out_frq
      ; 0 = C.getf smcp Smcp.out_dly
      ; not (C.getf smcp Smcp.presolve)
      ; C.getf smcp Smcp.excl
      ; C.getf smcp Smcp.shift
      ; (match C.getf smcp Smcp.aorn with Smcp.An.NT -> true | _ -> false) ]

  let iocp_default () =
    List.for_all Fun.id
      [ (match C.getf iocp Iocp.msg_lev with Msg.ALL -> true | _ -> false)
      ; (match C.getf iocp Iocp.br_tech with Iocp.Br.DTH -> true | _ -> false)
      ; (match C.getf iocp Iocp.bt_tech with Iocp.Bt.BLB -> true | _ -> false)
      ; near_eq 1e-5 (C.getf iocp Iocp.tol_int)
      ; near_eq 1e-7 (C.getf iocp Iocp.tol_obj)
      ; max_int32 = C.getf iocp Iocp.tm_lim
      ; 5000 = C.getf iocp Iocp.out_frq
      ; 10000 = C.getf iocp Iocp.out_dly
      ; C.null = C.getf iocp Iocp.cb_func
      ; C.null = C.getf iocp Iocp.cb_info
      ; 0 = C.getf iocp Iocp.cb_size
      ; (match C.getf iocp Iocp.pp_tech with Iocp.Pp.ALL -> true | _ -> false)
      ; near_eq 0.0 (C.getf iocp Iocp.mip_gap)
      ; not (C.getf iocp Iocp.mir_cuts)
      ; not (C.getf iocp Iocp.gmi_cuts)
      ; not (C.getf iocp Iocp.cov_cuts)
      ; not (C.getf iocp Iocp.clq_cuts)
      ; not (C.getf iocp Iocp.presolve)
      ; not (C.getf iocp Iocp.binarize)
      ; not (C.getf iocp Iocp.fp_heur)
      ; not (C.getf iocp Iocp.ps_heur)
      ; 60000 = C.getf iocp Iocp.ps_tm_lim
      ; C.getf iocp Iocp.sr_heur
      ; not (C.getf iocp Iocp.use_sol)
        (* cannot get save_sol as it's initialized with nullptr *)
      ; not (C.getf iocp Iocp.alien)
      ; C.getf iocp Iocp.flip ]
end

let set_get_pname () =
  Alcotest.(check string) "set_get_pname" "problem0" (To_test.set_get_pname ())

let smcp_default () =
  Alcotest.(check bool) "smcp_default" true (To_test.smcp_default ())

let iocp_default () =
  Alcotest.(check bool) "iocp_default" true (To_test.iocp_default ())

let () =
  let open Alcotest in
  run "Glp"
    [ ("set_get_pname", [test_case "set_get_pname" `Quick set_get_pname])
      (* temporarily disable these tests due to dependency on GLPK version. *)
      (* TODO put more robust tests *)
      (* ; ("smcp_default", [test_case "smcp_default" `Quick smcp_default]) *)
      (* ; ("iocp_default", [test_case "iocp_default" `Quick iocp_default]) *)
    ]
