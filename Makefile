.PHONY: build test fmt doc clean

build:
	dune build

test:
	dune runtest

fmt:
	dune build @fmt --auto-promote

doc:
	dune build @doc
	rm -r docs
	cp -r _build/default/_doc/_html docs
	touch docs/.nojekyll

clean:
	dune clean
