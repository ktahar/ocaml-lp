.PHONY: build test fmt docs clean

build:
	dune build

test:
	dune runtest

fmt:
	dune build @fmt --auto-promote

docs:
	dune build @doc
	rm -rf docs
	cp -r _build/default/_doc/_html docs
	touch docs/.nojekyll

clean:
	dune clean
