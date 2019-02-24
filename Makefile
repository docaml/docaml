all:
	dune build @src/all --profile release

install:
	dune install

clean:
	dune clean
