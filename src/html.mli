(** HTML representation *)

(** This module provides a representation of HTML that is easy to
  * manipulate in ocaml itself.
  * It does not aim to be complete. *)

(** Internal representation of HTML *)
type t

(** Utility to render in HTML format *)
val to_string : t -> string

(** Type of a regular tag *)
type tag = Attribute.t list -> t list -> t

(** Type of a void element, a void element doesn't have children *)
type void_tag = Attribute.t list -> t

(** Headers *)

(** h1 header *)
val h1 : tag

(** h2 header *)
val h2 : tag

(** h3 header *)
val h3 : tag

(** h4 header *)
val h4 : tag

(** h5 header *)
val h5 : tag

(** h6 header *)
val h6 : tag

(** Paragraphs *)

(** Line break *)
val br : void_tag
