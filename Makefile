all:
	ocamlbuild -use-ocamlfind -use-menhir -cflags -rectypes -I src -package unix,str docaml.native
