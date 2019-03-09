{
  open Config_parser

  exception SyntaxError of string

  let h_add k e t = Hashtbl.add t k e; t

  let keywords_table =
    Hashtbl.create 19
    |> h_add "name"      NAME
    |> h_add "modules"   MODULES

}

let newline = ('\013' * '\010')

let blank = [' ' '\009' '\012']

let identchar = ['A'-'Z' 'a'-'z' '_' '0'-'9']

rule token = parse
  | blank +
    { token lexbuf }
  | newline
    { Lexing.new_line lexbuf; token lexbuf }
  | eof
    {EOF}
  | "(*"   { read_comment lexbuf }
  | ":"  {COLON}
  | [^ '\\' '\n' ':' '(' ' ' '\009' '\012'] +
    {
      try Hashtbl.find keywords_table (Lexing.lexeme lexbuf)
      with Not_found -> STRING (Lexing.lexeme lexbuf)
    }
  | _  {raise (SyntaxError ("Syntax Error, unknown char."))}

and read_comment = parse
  | "*)"               { token lexbuf }
  | newline            { Lexing.new_line lexbuf ; read_comment lexbuf }
  | '*'                { read_comment lexbuf }
  | [^ '*' '\r' '\n']+ { read_comment lexbuf }
  | eof { raise (SyntaxError ("Comment not terminated")) }
  | _   { assert false }
