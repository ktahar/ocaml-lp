# ocaml-lp : LP and MIP modeling in OCaml

This library is a modeling tool for Linear Programming (LP) and Mixed Integer Programming (MIP).
The model can be exported to CPLEX LP file format, which can be loaded by various solvers.
Importing models from LP file is also supported.

## Install

```bash
# optional but recommended to pin dev-repo as it's on quite early stage of development
opam pin lp --dev-repo
opam install lp
```

## Example

```OCaml
let problem =
  let open Lp in
  let x = var "x" in
  let y = var "y" in
  let c0 = x + c 1.2 * y <$ c 5.0 in
  let c1 = c 2.0 * x + y <$ c 1.2 in
  let obj = maximize (x + y) in
  let cnstrs = [c0; c1] in
  (obj, cnstrs)

let () =
   if Lp.validate problem then
       Lp.write "my_problem.lp" problem
   else
       print_endline "Oops, my problem is broken."

```

## Status

Currently only basic features of LP file format are supported.
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
- Piecewise-linear objective and constraint
- General Constraint
- Scenario

## References

Some references to LP file format.

- [CPLEX](https://www.ibm.com/support/knowledgecenter/SSSA5P_12.7.1/ilog.odms.cplex.help/CPLEX/FileFormats/topics/LP.html)
- [Gurobi](https://www.gurobi.com/documentation/9.0/refman/lp_format.html)
- [lp_solve](http://lpsolve.sourceforge.net/5.5/CPLEX-format.htm)

## License
MIT
