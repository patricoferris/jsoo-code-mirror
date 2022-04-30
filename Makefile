.PHONY: example node bundle fmt
example:
	dune build --ignore-promoted-rules
	parcel _build/default/example/src/index.html

node:
	npm install

bundle: node
	dune build --profile=with-bundle

fmt:
	dune build @fmt --auto-promote
