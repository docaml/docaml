type t = {
  name : string ;
  value : string
}

let to_string a =
  Printf.sprintf "%s=\"%s\"" a.name a.value

let list_to_string l =
  String.concat " " (List.map to_string l)

let mk name =
  fun value -> { name ; value }

let classes cl =
  mk "class" (String.concat " " cl)

let id n =
  mk "id" n

let href s =
  mk "href" s
