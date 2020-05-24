# ocaml-lp : LP and MIP modeling in OCaml

This library helps the modeling of Linear Programming (LP) and Mixed Integer Programming (MIP) in OCaml.
It supports the model with not only linear terms, but also quadratic terms.
The model can be imported-from / exported-to CPLEX LP file format, which can be loaded by various solvers.
It also has direct interfaces to some solvers.
Currently supported are [GLPK](https://www.gnu.org/software/glpk/) (GNU Linear Programming Kit) and [Gurobi](https://www.gurobi.com).

## Install

```bash
# optional but recommended to pin dev-repo as it's on quite early stage of development
opam pin lp --dev-repo
opam pin lp-glpk --dev-repo
opam pin lp-gurobi --dev-repo # if you have an access to Gurobi
opam install lp lp-glpk lp-gurobi
```

## Example

```ocaml
let x = Lp.var "x"
let y = Lp.var "y"

let problem =
  let open Lp in
  let obj = maximize (x ++ y) in
  let c0 = x ++ (c 1.2 *~ y) <~ c 5.0 in
  let c1 = (c 2.0 *~ x) ++ y <~ c 1.2 in
  let cnstrs = [c0; c1] in
  (obj, cnstrs)

let write () = Lp.write "my_problem.lp" problem

let solve () =
  (* For Gurobi, use Lp_gurobi.solve instead *)
  match Lp_glpk.solve problem with
  | Ok (obj, tbl) ->
      Printf.printf "Objective: %.2f\n" obj ;
      Printf.printf "x: %.2f y: %.2f\n"
        (Hashtbl.find tbl x) (Hashtbl.find tbl y)
  | Error msg ->
      print_endline msg

let () =
  if Lp.validate problem then (write () ; solve ())
  else print_endline "Oops, my problem is broken."
```

## Notes on GLPK interface

- To use this, compile your application with `-cclib -lglpk` flags.
- Since this is tested only on GLPK version 4.65, something may fail on other versions.

## Conformity to LP file format

Currently only basic features of LP file format are supported.
Yet to be supported are advanced features,
which are typically available on commercial solvers.
(There is no standard of LP file, though.)

### supported

- Single objective (linear and quadratic)
- Constraints (linear and quadratic)
- Bounds
- Variable types (General Integers and Binary Integers)

### not-supported

- Semi-continuous variables
- Multi-objective
- Lazy constraint
- Special ordered set (SOS)
- Piecewise-linear (PWL) objective and constraint
- General Constraint
- Scenario

## References

Some references to LP file format.

- [CPLEX](https://www.ibm.com/support/knowledgecenter/SSSA5P_12.7.1/ilog.odms.cplex.help/CPLEX/FileFormats/topics/LP.html)
- [Gurobi](https://www.gurobi.com/documentation/9.0/refman/lp_format.html)
- [lp_solve](http://lpsolve.sourceforge.net/5.5/CPLEX-format.htm)
- Manual of [GLPK](https://www.gnu.org/software/glpk/)

## License
MIT
