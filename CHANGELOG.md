# Changelog

## [Unreleased]

## [0.5.0] - 2025-06-22
### Added
- lp-highs package, interface to HiGHS (thanks @sano-jin).
- post-processing utility functions compute_term and compute_poly (thanks @adelaett).

## [0.4.0] - 2021-11-19
### Added
- lp-glpk-js package, interface to GLPK through glpk.js.

### Changed
- Lp_glpk uses stub-generation feature of ctypes, removing necessity of C linker flags on building user application.

## [0.3.0] - 2021-02-14
### Added
- Odoc documentation
- Error-position reports from the lexer / parser

### Changed
- Lp_glpk.solve (Lp_gurobi.solve) returns opt. values with Map(Poly.t) instead of Hashtbl.

### Fixed
- A bug of boundary string

## [0.2.0] - 2020-05-24
### Added
- Interface to Gurobi (lp-gurobi package)

### Fixed
- Bugs of the parser on quadratic term

## [0.1.0] - 2020-05-10
### Added
- Interface to GLPK (lp-glpk package)
- Monomial array generation (range)
- Problem classification and validation

## [0.0.2] - 2020-04-22
### Added
- Operators (subtraction and division) on polynomial
- Some lexer features

### Fixed
- Installation issue

## [0.0.1] - 2020-04-13
### Added
- Initial release
