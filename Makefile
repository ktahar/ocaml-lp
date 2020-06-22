.PHONY: default test fmt doc

default:
	dune build

test:
	dune runtest

fmt:
	dune build @fmt

doc:
	rm -r docs
	dune build @doc
	cp -r _build/default/_doc/_html docs
