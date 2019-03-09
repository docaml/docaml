# docaml
## Documentation generation for ocaml projects.

docaml takes in a list of ocaml interfaces (for now `mli` files only, but reasonml 
is also planned) and generates a **static**, **searchable** and **customisable** 
documentation as HTML/CSS/JS.

- [Installing](#installing)
- [How to use](#how-to-use)

### Dependencies

These are not dependencies per se, but are used in the built project.

- Monokai Sublime style
- jQuery
- Highlight.js
- Tipue Search

### Authors

Victor Lanvin and Théo Winterhalter

## Installing

For now you can install docaml directly from the sources.
For this you need `dune`, and then you can type `make`
followed by `make install`.

## How to Use

docaml expects a `docaml` file in order to run.
If you type `docaml init` it will generate a template that looks like this:

```
(* Name of your project, without spaces *)
name: TODO

(* Space separated list of modules (can be relative or absolute paths) *)
modules: module1.mli src/module2.mli

(* Custom CSS files, space separated *)
(* custom css: file1.css file2.css *)

(* Header logo, it will replace the name in the header on the left
   You can use any image format supported by HTML.
*)
(* header logo: logo.png *)

(* Favicon *)
(* favicon: favicon.ico *)
```

For instance docaml's own doc can be built with the following `docaml` file.
```
name: docaml
modules: src/attribute.mli src/docgen.mli src/html.mli src/config.mli
```

You can then type `docaml build` to create the documentation. It will be placed in
the `doc` subdirectory of the current directory.

In order to clean, you can use `docaml clean` which will remove the `doc`
folder and its subdirectories.
