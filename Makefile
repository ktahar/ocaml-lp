.PHONY: build test fmt format docs clean setup-glpk-js test-glpk-js test-highs

build:
	dune build

test:
	dune runtest

setup-glpk-js:
	./scripts/setup-glpk-js-test.sh

test-glpk-js: setup-glpk-js
	dune build @runtest_glpk_js

test-highs:
	dune build @runtest_highs

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
