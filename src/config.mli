(** Configuration *)

(** Type of user configuration *)
type t

(** TODO Get the configuration from a file *)
val from_file : string -> t

(** Project name *)
val name : t -> string
