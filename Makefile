.PHONY: doc docaml

docaml:
	dune build @src/all --profile release

install:
	dune build @install && \
	dune install

doc:
	docaml build

clean:
	dune clean && \
	docaml clean
