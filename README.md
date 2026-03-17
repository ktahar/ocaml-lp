# ocaml-lp : LP and MIP modeling in OCaml

This library helps the modeling of Linear Programming (LP) and Mixed Integer Programming (MIP) in OCaml.
It supports the model with not only linear terms, but also quadratic terms.
The model can be imported-from / exported-to CPLEX LP file format, which can be loaded by various solvers.
It also has direct interfaces to some solvers.
Currently supported are [GLPK](https://www.gnu.org/software/glpk/) (GNU Linear Programming Kit), [HiGHS](https://highs.dev), and [Gurobi](https://www.gurobi.com).

## Install

```bash
# optional but recommended to pin dev-repo:
opam pin lp --dev-repo
opam pin lp-glpk --dev-repo
opam install lp lp-glpk
## If your application is based on js_of_ocaml:
opam pin lp-glpk-js --dev-repo
opam install lp-glpk-js
## If you want to use HiGHS (needs build/install as below before this):
opam pin lp-highs --dev-repo
opam install lp-highs
## If you have an access to Gurobi:
opam pin lp-gurobi --dev-repo
opam install lp-gurobi
```

If `opam pin <package-name> --dev-repo` does not work, you may also try

```bash
opam pin add <package-name> git+https://github.com/ktahar/ocaml-lp.git
```

### Developer setup

For development, clone this repository and use local path pins:

```bash
# 1. Install dependencies from local opam files:
opam install . --deps-only --with-test --with-dev-setup
# 2. Pin packages you want to develop to the current checkout:
opam pin add lp .
opam pin add lp-glpk .
opam pin add lp-glpk-js .
opam pin add lp-highs .
opam pin add lp-gurobi .
# 3. Build and run tests:
# If HiGHS is installed under `~/.local`, export `HIGHS_INCLUDE_DIR` and `HIGHS_LIB_DIR` first.
# (See "Build and install HiGHS" below.)
make # dune build
make test # dune runtest
# 4. Run optional solver-specific tests:
make test-lp-glpk-js # dune build @runtest_lp_glpk_js
make test-lp-highs # dune build @runtest_lp_highs
# 5. Run examples:
dune exec examples/lp-glpk/knapsack.exe
# 6. After edit, rebuild local packages from current working-dir source:
# (When you want to update installed packages, e.g., to test them in REPL.)
opam install lp --working-dir # or opam reinstall lp --working-dir
```

### Build and install HiGHS

For many OS distributions, binary packages for the HiGHS solver are not yet available.
So, if you want to use it, the most reliable approach is to clone the git repository and
build it manually as in the [documentation](https://ergo-code.github.io/HiGHS/dev/installation/).

#### Recommended 1: user-local install (`~/.local`, no `sudo`)

```bash
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX="$HOME/.local"
cmake --build build --parallel
cmake --install build
```

Then set environment variables:

```bash
export HIGHS_INCLUDE_DIR="${HOME}/.local/include/highs"
export HIGHS_LIB_DIR="${HOME}/.local/lib"
export LD_LIBRARY_PATH="${HIGHS_LIB_DIR}:${LD_LIBRARY_PATH}"
export PATH="${HOME}/.local/bin:${PATH}"
```

#### Recommended 2: system install (default prefix, needs `sudo`)

```bash
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build --parallel
sudo cmake --install build
sudo ldconfig
```

For this case, `HIGHS_INCLUDE_DIR` / `HIGHS_LIB_DIR` are usually not necessary.

### Notes on solver interfaces

#### lp-glpk

Since lp-glpk is tested only on GLPK version 4.65 and 5+, something may fail on older versions.

#### lp-glpk-js

lp-glpk-js is another interface to GLPK through [glpk.js](https://github.com/jvail/glpk.js/),
that is useful for applications based on js_of_ocaml.

The test of lp-glpk-js is optional and not part of default `dune runtest`.
For local development, install the JS dependency outside dune with `make setup-lp-glpk-js` and then run `make test-lp-glpk-js` or `dune build @runtest_lp_glpk_js`.
The current test setup pins glpk.js version to 5.0.0.

For compatibility, `Lp_glpk_js.require_glpk_async` supports both the legacy 4.x module shape and the 5.x async constructor, while `require_glpk` remains available for synchronous 4.x initialization.
Likewise, `Lp_glpk_js.solve_async` supports both synchronous and Promise-based `glpk.solve`.

#### lp-highs

The test of lp-highs is optional and not part of default `dune runtest`.
For local development, build/install HiGHS as above, install `lp-highs`,
and then run `make test-lp-highs` or `dune build @runtest_lp_highs`.

#### lp-gurobi

lp-gurobi is an interface to commercial solver Gurobi.
To work with this, compile your application with `-cclib -lgurobiXY` flags,
where XY is the version of Gurobi (e.g. 91).
The test of lp-gurobi is disabled by default.
To enable this, follow [test/lp-gurobi/README.md](test/lp-gurobi/README.md).

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
  (* For other interfaces, use Lp_glpk_js or Lp_gurobi instead *)
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

### examples directory

The [examples](examples) directory contains more usage examples.
To compile and execute, you can use `dune exec`:

```bash
dune exec examples/lp-glpk/knapsack.exe
```

## Documentation

High-level APIs have comments for odoc (or ocamldoc).
Generated docs can be found [online](https://ktahar.github.io/ocaml-lp/) or in [docs](docs) directory.

## Development Status

Development is not quite active now because basic features are completed.
However, there are several TODOs.
Bug-reports, requests, or patches are always welcome via GitHub [issues](https://github.com/ktahar/ocaml-lp/issues) and [pull requests](https://github.com/ktahar/ocaml-lp/pulls).

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

- [CPLEX](https://www.ibm.com/docs/en/icos/22.1.2?topic=cplex-lp-file-format-algebraic-representation)
- [Gurobi](https://www.gurobi.com/documentation/9.1/refman/lp_format.html)
- Manual of [GLPK](https://www.gnu.org/software/glpk/)

## License

[MIT](LICENSE.md)
