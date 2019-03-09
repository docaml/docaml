(** Configuration *)

(** Type of user configuration *)
type t

(** TODO Get the configuration from a file *)
val from_file : string -> t

(** Project name *)
val name : t -> string

(** List of paths to modules *)
val modules : t -> string list

(** Optional list of custom CSS files *)
val custom_css : t -> string list option
