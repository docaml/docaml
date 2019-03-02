(* open Attribute *)

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

let html = mk "html"

let head = mk "head"
let link = mkvoid "link"
let meta = mkvoid "meta"
let style = mk "style"
let title = mk "title"

let body = mk "body"

let article = mk "article"
let aside = mk "aside"
let footer = mk "footer"
let header = mk "header"
let h1 = mk "h1"
let h2 = mk "h2"
let h3 = mk "h3"
let h4 = mk "h4"
let h5 = mk "h5"
let h6 = mk "h6"
let hgroup = mk "hgroup"
let main = mk "main"
let nav = mk "nav"
let section = mk "section"

let br = mkvoid "br"
