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

(** h1 header *)
val h1 : tag
