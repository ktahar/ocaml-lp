.PHONY: build test fmt format docs clean setup-lp-glpk-js test-lp-glpk-js test-lp-highs

build:
	dune build

test:
	dune runtest

setup-lp-glpk-js:
	./scripts/setup-lp-glpk-js-test.sh

test-lp-glpk-js: setup-lp-glpk-js
	dune build @runtest_lp_glpk_js

test-lp-highs:
	dune build @runtest_lp_highs

fmt:
	dune build @fmt --auto-promote

format:
	dune build @fmt --auto-promote

docs:
	dune build @doc
	rm -rf docs
	cp -r _build/default/_doc/_html docs
	touch docs/.nojekyll

clean:
	dune clean
