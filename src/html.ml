(* open Attribute *)

type _tag = {
  name : string ;
  attributes : Attribute.t list ;
  children : t list option
}

and t =
  | Text of string
  | Tag of _tag

let rec to_string h =
  begin match h with
  | Text s -> s
  | Tag t ->
    begin match t.children with
    | Some l ->
      Printf.sprintf "<%s%s>%s</%s>"
        t.name
        (Attribute.list_to_string t.attributes)
        (String.concat "" (List.map to_string l))
        t.name
    | None ->
      Printf.sprintf "<%s%s>"
        t.name
        (Attribute.list_to_string t.attributes)
    end
  end

type tag = Attribute.t list -> t list -> t
type void_tag = Attribute.t list -> t

let text s = Text s

let doctype =
  Tag { name = "!DOCTYPE html" ; attributes = [] ; children = None }

let mk name : tag =
  fun attributes children ->
    Tag { name ; attributes ; children = Some children }

let mkvoid name : void_tag =
  fun attributes ->
    Tag { name ; attributes ; children = None }

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

let div = mk "div"
let figure = mk "figure"
let hr = mkvoid "hr"
let li = mk "li"
let p = mk "p"
let ul = mk "ul"

let a = mk "a"
let br = mkvoid "br"
let code = mk "code"
let span = mk "span"

let img = mkvoid "img"

let script = mk "script"

let table = mk "table"
let tbody = mk "tbody"
let td = mk "td"
let tr = mk "tr"
