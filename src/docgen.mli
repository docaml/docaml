(** Documentation generation *)

(** [parse_from_file filename] *)
val parse_from_file : string -> AST.module_field list

(** [preprocess modulename fields] *)
val preprocess : string -> AST.module_field list -> ASTpp.module_data

(** [preprocess_file filename] *)
val preprocess_file : string -> ASTpp.module_data

(** [gen_header root modulename] *)
val gen_header : Config.t -> string -> string -> Html.t

(** [gen_main root module] *)
val gen_main : string -> ASTpp.module_data -> Html.t

(** [gen_index_main config root modules examplefile] *)
val gen_index_main : Config.t -> string -> ASTpp.module_data list -> Html.t

(** [gen_aside root modules] *)
val gen_aside :
  Config.t ->
  string ->
  ASTpp.module_data option ->
  ASTpp.module_data list ->
  Html.t

val gen_search_content : ASTpp.module_data -> string

val gen_search_note : ASTpp.module_data -> string
