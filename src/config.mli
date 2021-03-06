(** Configuration *)

(** Type of user configuration *)
type t

(** Problem with parsing *)
exception Error of string

(** Get the configuration from a file *)
val from_file : string -> t

(** Project name *)
val name : t -> string

(** List of paths to modules *)
val modules : t -> string list

(** Optional list of custom CSS files *)
val custom_css : t -> string list option

(** Optional logo to replace name in header *)
val header_logo : t -> string option

(** Optional favicon *)
val favicon : t -> string option
