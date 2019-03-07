.PHONY: doc docaml

docaml:
	dune build @src/all --profile release

install: docaml
	dune build @install && \
	dune install

doc: docaml install
	docaml build

clean:
	dune clean && \
	rm -rf doc
