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
