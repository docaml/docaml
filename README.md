# docaml

Documentation generation for ocaml projects.
docaml looks at the provided `mli` files for comments to produce the documentation.
See the example to see how it works.

To try it out, just `make` and then `make install`.
After this you can use
```bash
docaml <file1>.mli ... <fileN>.mli
```
to build your documentation, it will then be available in the doc folder, in the same directory

## Warning
This is very much work in progress and starts as a copy from our own generator
for [ogaml](http://ogaml.github.io/).
Also the usage is highly likely to change to use config files in the near future.
For the time being this shouldn't be considered as a release.

## Dependencies

This project also takes advantage of several other projects:

- Monokai Sublime style.
- Highlight.js
- Tipue Search (not yet)

## Authors

Victor Lanvin

Théo Winterhalter
