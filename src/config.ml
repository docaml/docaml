open Lexing
open Config_ast

type t = {
  name : string option ;
  modules : string list option ;
  custom_css : string list option ;
  header_logo : string option
}

exception Error of string

let error fmt = Printf.ksprintf (fun s -> raise (Error s)) fmt

let print_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  let str = Lexing.lexeme lexbuf in
  let begchar = pos.pos_cnum - pos.pos_bol + 1 in
  Printf.sprintf "In %s, line %d, characters %d-%d : %s"
    pos.pos_fname pos.pos_lnum begchar
    (begchar + (String.length str))
    (Lexing.lexeme lexbuf)

let parse_with_errors lexbuf =
  try
    Config_parser.file Config_lexer.token lexbuf
  with
  | Config_lexer.SyntaxError msg ->
    error "%s : %s" (print_position lexbuf) msg
  | Config_parser.Error ->
    error "%s : Syntax Error" (print_position lexbuf)

let check c =
  if c.name = None then error "Field name mandatory" ;
  if c.modules = None then error "Field modules mandatory"

let with_name c name =
  match c.name with
  | None -> { c with name = Some name }
  | Some _ -> error "Cannot set name twice"

let with_modules c modules =
  match c.modules with
  | None -> { c with modules = Some modules }
  | Some _ -> error "Cannot set modules twice"

let with_custom_css c custom_css =
  match c.custom_css with
  | None -> { c with custom_css = Some custom_css }
  | Some _ -> error "Cannot set custom css twice"

let with_header_logo c header_logo =
  match c.header_logo with
  | None -> { c with header_logo = Some header_logo }
  | Some _ -> error "Cannot set header logo twice"

let rec from_ast c ast =
  match ast with
  | Name name :: ast -> from_ast (with_name c name) ast
  | Modules modules :: ast -> from_ast (with_modules c modules) ast
  | CustomCSS custom_css :: ast -> from_ast (with_custom_css c custom_css) ast
  | HeaderLogo logo :: ast -> from_ast (with_header_logo c logo) ast
  | [] -> check c ; c

let from_file f =
  let input = open_in f in
  let lexbuf = from_channel input in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = f } ;
  let ast = parse_with_errors lexbuf in
  close_in input ;
  let default = {
    name = None ;
    modules = None ;
    custom_css = None ;
    header_logo = None
  } in
  from_ast default ast

let name config =
  match config.name with
  | Some name -> name
  | None -> assert false

let modules config =
  match config.modules with
  | Some modules -> modules
  | None -> assert false

let custom_css config = config.custom_css

let header_logo config = config.header_logo
