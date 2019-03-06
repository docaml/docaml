(** HTML representation *)

(** This module provides a representation of HTML that is easy to
  * manipulate in ocaml itself.
  * It does not aim to be complete. *)

(** Internal representation of HTML *)
type t

(** Utility to render in HTML format *)
val to_string : t -> string

(** Render an HTML document (typically the argument is the html tag)
  * This will add the doctype at the beginning.
 *)
val document_to_string : t -> string

(** Utility to ignore tags and render as plain text *)
val to_text : t -> string

(** Type of a regular tag *)
type tag = Attribute.t list -> t list -> t

(** Type of a void element, a void element doesn't have children *)
type void_tag = Attribute.t list -> t

(** Text *)
val text : string -> t

(*** Main root *)

(** Top-level element of an HTML document *)
val html : tag

(*** Document meta-data *)

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

(*** Sectioning root *)

(** Content of the document *)
val body : tag

(*** Content sectioning *)

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

(*** Text content *)

(** Generic container *)
val div : tag

(** Figure element *)
val figure : tag

(** Thematic break, usually a horizontal line *)
val hr : void_tag

(** Element in a list *)
val li : tag

(** Paragraph *)
val p : tag

(** Preformatted text *)
val pre : tag

(** Unordered list *)
val ul : tag

(*** Inline text *)

(** Anchor, hyperlink *)
val a : tag

(** Line break *)
val br : void_tag

(** Computer code *)
val code : tag

(** Generic inline container *)
val span : tag

(*** Image and multimedia *)

(** Image element *)
val img : void_tag

(*** Scripting *)

(** Embed or reference executable code like javascript *)
val script : tag

(*** Table content *)

(** Tabular data *)
val table : tag

(** Encompass a set of rows comprising the body of the table *)
val tbody : tag

(** Cell of a table *)
val td : tag

(** Row of cells of a table *)
val tr : tag

(*** Forms *)

(** Form *)
val form : tag

(** Input *)
val input : void_tag
