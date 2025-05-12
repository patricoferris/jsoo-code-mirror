.PHONY: examples node bundle fmt
examples:
	dune build --ignore-promoted-rules
	(cd examples; python serve.py)

node:
	npm install

# Use that command if you want to re-generate the static bundled in the includes
# folder, for example if there was an update of code-mirror.
bundle: node
	dune build --profile=with-bundle

fmt:
	dune build @fmt --auto-promote
