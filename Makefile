.PHONY: doc docaml

docaml:
	dune build @src/all --profile release

install: docaml
	dune build @install && \
	dune install

doc: docaml install
	# docaml src/AST.mli src/ASTpp.mli src/attribute.mli src/docgen.mli src/html.mli
	docaml src/attribute.mli src/docgen.mli src/html.mli

clean:
	dune clean && \
	rm -rf doc
