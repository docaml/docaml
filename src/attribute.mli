(** HTML attributes representation *)

(** This module provides a representation of HTML attributes.
  * It does not aim to be complete for the moment. *)

(** Internal representation of an attribute *)
type t

(** Utility to render an attribute in a format suitable for HTML *)
val to_string : t -> string

(** Utility to render an attribute list in a format suitable for HTML *)
val list_to_string : t list -> string

(** Class attribute, takes a list of classes as argument *)
val classes : string list -> t

(** Unique id of an element *)
val id : string -> t

(** URL of a linked source *)
val href : string -> t

(** Character encoding *)
val charset : string -> t

(** Relationship of the target object to the link object *)
val rel : string -> t

(** Type of element (correspond to attribute 'type') *)
val typ : string -> t

(** URL of embeddable content *)
val src : string -> t

(** Style of an element *)
val style : string -> t

(** Name of the element *)
val name : string -> t

(** Regular expression to validate against *)
val pattern : string -> t

(** Text to be displayed in tooltip when hovering over element *)
val title : string -> t

(** Indicates that an element is required *)
val required : t

(** URI of a program that processes the submitted form *)
val action : string -> t

(** Alternative text in case image can't be displayed *)
val alt : string -> t
