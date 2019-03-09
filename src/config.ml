open Lexing
open Config_ast

type t = {
  name : string option ;
  modules : string list option
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

let rec from_ast c ast =
  (* TODO Verify double assignments *)
  match ast with
  | Name name :: ast -> from_ast { c with name = Some name } ast
  | Modules modules :: ast -> from_ast { c with modules = Some modules } ast
  | [] -> check c ; c

let from_file f =
  let input = open_in f in
  let lexbuf = from_channel input in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = f } ;
  let ast = parse_with_errors lexbuf in
  close_in input ;
  let default = {
    name = None ;
    modules = None
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
