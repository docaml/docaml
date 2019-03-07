type t = {
  name : string
}

let from_file _ = {
  name = "{name}"
}

let name config = config.name
