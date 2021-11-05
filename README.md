# ocaml-lp : LP and MIP modeling in OCaml

This library helps the modeling of Linear Programming (LP) and Mixed Integer Programming (MIP) in OCaml.
It supports the model with not only linear terms, but also quadratic terms.
The model can be imported-from / exported-to CPLEX LP file format, which can be loaded by various solvers.
It also has direct interfaces to some solvers.
Currently supported are [GLPK](https://www.gnu.org/software/glpk/) (GNU Linear Programming Kit) and [Gurobi](https://www.gurobi.com).

## Install

```bash
# optional but recommended to pin dev-repo
opam pin lp --dev-repo
opam pin lp-glpk --dev-repo
opam install lp lp-glpk
# If you have an access to Gurobi:
# opam pin lp-gurobi --dev-repo
# opam install lp-gurobi
```

## Example

A minimum example is shown below.
More detailed description can be found on [wiki](https://github.com/ktahar/ocaml-lp/wiki/).

```ocaml
let x = Lp.var "x"
let y = Lp.var "y"

let problem =
  let open Lp in
  let obj = maximize (x ++ y) in
  let c0 = x ++ (c 1.2 *~ y) <~ c 5.0 in
  let c1 = (c 2.0 *~ x) ++ y <~ c 1.2 in
  make obj [c0; c1]

let write () = Lp.write "my_problem.lp" problem

let solve () =
  (* For Gurobi, use Lp_gurobi instead *)
  match Lp_glpk.solve problem with
  | Ok (obj, xs) ->
      Printf.printf "Objective: %.2f\n" obj ;
      Printf.printf "x: %.2f y: %.2f\n"
        (Lp.PMap.find x xs) (Lp.PMap.find y xs)
  | Error msg ->
      print_endline msg

let () =
  if Lp.validate problem then (write () ; solve ())
  else print_endline "Oops, my problem is broken."
```

## Documentation

High level APIs have comments for odoc (or ocamldoc).
Generated docs can be found [online](https://ktahar.github.io/ocaml-lp/) or in [docs](docs) directory.

## Notes on GLPK interface

- To use this, compile your application with `-cclib -lglpk` flags.
- Since this is tested only on GLPK version 4.65 and 5+, something may fail on older versions.

## Development Status

Original author isn't developing this heavily because basic features are completed.
However, bug-reports, requests, or patches are always welcome via GitHub [issues](https://github.com/ktahar/ocaml-lp/issues) and [pull requests](https://github.com/ktahar/ocaml-lp/pulls).

### Conformity to LP file format

Currently only basic features of LP file format are supported.
Yet to be supported are advanced features,
which are typically available on commercial solvers.
(There is no standard of LP file, though.)

#### supported

- Single objective (linear and quadratic)
- Constraints (linear and quadratic)
- Bounds
- Variable types (general and binary integers)

#### not-supported

- Semi-continuous variables
- Multi-objective
- Lazy constraint
- Special ordered set (SOS)
- Piecewise-linear (PWL) objective and constraint
- General Constraint
- Scenario

## References

Some references to LP file format.

- [CPLEX](https://www.ibm.com/docs/en/icos/12.7.1.0?topic=cplex-lp-file-format-algebraic-representation)
- [Gurobi](https://www.gurobi.com/documentation/9.1/refman/lp_format.html)
- Manual of [GLPK](https://www.gnu.org/software/glpk/)

## License
MIT
