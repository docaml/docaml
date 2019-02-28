all:
	dune build @src/all --profile release

install:
	dune build @install && \
	dune install

clean:
	dune clean
