all:
	ocamlbuild -use-ocamlfind -use-menhir -I src -package unix,str docaml.native
