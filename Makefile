.PHONY: build test fmt docs clean setup-lp-glpk-js test-lp-glpk-js

build:
	dune build

test:
	dune runtest

setup-lp-glpk-js:
	./scripts/setup-lp-glpk-js-test.sh

test-lp-glpk-js: setup-lp-glpk-js
	dune build @runtest_lp_glpk_js

fmt:
	dune build @fmt --auto-promote

docs:
	dune build @doc
	rm -rf docs
	cp -r _build/default/_doc/_html docs
	touch docs/.nojekyll

clean:
	dune clean
