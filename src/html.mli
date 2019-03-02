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

(** Main root *)

(** Top-level element of an HTML document *)
val html : tag

(** Document meta-data *)

(** Contains meta-data *)
val head : tag

(** External resource link *)
val link : void_tag

(** Meta-data *)
val meta : void_tag

(** Style information, like CSS *)
val style : tag

(** Title of the document *)
val title : tag

(** Sectioning root *)

(** Content of the document *)
val body : tag

(** Content sectioning *)

(** Self-contained composition *)
val article : tag

(** Content inderctly related to main content *)
val aside : tag

(** Footer of nearest sectioning content or sectioning root *)
val footer : tag

(** Header *)
val header : tag

(** Level 1 heading *)
val h1 : tag

(** Level 2 heading *)
val h2 : tag

(** Level 3 heading *)
val h3 : tag

(** Level 4 heading *)
val h4 : tag

(** Level 5 heading *)
val h5 : tag

(** Level 6 heading *)
val h6 : tag

(** Multi-level heading of a section of a document *)
val hgroup : tag

(** Main content *)
val main : tag

(** Navigation links *)
val nav : tag

(** Standalone section *)
val section : tag

(** Text content *)

(** Line break *)
val br : void_tag
