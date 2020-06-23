.PHONY: default test fmt doc clean

default:
	dune build

test:
	dune runtest

fmt:
	dune build @fmt

doc:
	dune build @doc
	rm -r docs
	cp -r _build/default/_doc/_html docs
	touch docs/.nojekyll

clean:
	dune clean
