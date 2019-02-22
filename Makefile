all:
	ocamlbuild -use-ocamlfind -use-menhir -I src -package unix,str docaml.native

clean:
	rm -rf _build
