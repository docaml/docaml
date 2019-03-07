type t = {
  name : string ;
  modules : string list
}

let from_file _ = {
  name = "{name}" ;
  modules = [
    "src/attribute.mli" ;
    "src/docgen.mli" ;
    "src/html.mli" ;
    "src/config.mli"
  ]
}

let name config = config.name

let modules config = config.modules
