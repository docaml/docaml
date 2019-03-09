type content = {
  name : string ;
  value : string
}

type t =
  | Content of content
  | Boolean of string

let content_to_string a =
  Printf.sprintf "%s=\"%s\"" a.name a.value

let to_string a =
  match a with
  | Content c -> content_to_string c
  | Boolean n -> n

let list_to_string l =
  String.concat " " (List.map to_string l)

let mk name =
  fun value -> Content { name ; value }

let classes cl =
  mk "class" (String.concat " " cl)

let id n = mk "id" n
let href s = mk "href" s
let charset s = mk "charset" s
let rel s = mk "rel" s
let typ s = mk "type" s
let src s = mk "src" s
let style s = mk "style" s
let name s = mk "name" s
let pattern s = mk "pattern" s
let title s = mk "title" s
let action s = mk "action" s
let alt s = mk "alt" s

let required = Boolean "required"
