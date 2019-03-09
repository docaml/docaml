(** Configuration file AST *)

type t =
| Name of string
| Modules of string list
| CustomCSS of string list
| HeaderLogo of string
| Favicon of string
