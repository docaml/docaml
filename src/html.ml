open Attribute

type t = {
  tag : string ;
  attributes : Attribute.t list ;
  children : t list option
}

let rec to_string h =
  match h.children with
  | Some l ->
    Printf.sprintf "<%s%s>%s</%s>"
      h.tag
      (Attribute.list_to_string h.attributes)
      (String.concat "" (List.map to_string l))
      h.tag
  | None ->
    Printf.sprintf "<%s%s>"
      h.tag
      (Attribute.list_to_string h.attributes)

type tag = Attribute.t list -> t list -> t
type void_tag = Attribute.t list -> t

let mk tag : tag =
  fun attributes children -> { tag ; attributes ; children = Some children }

let mkvoid tag : void_tag =
  fun attributes -> { tag ; attributes ; children = None }

let h1 = mk "h1"
