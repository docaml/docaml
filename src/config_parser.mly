%token EOF
%token COLON
%token NAME MODULES CUSTOM CSS HEADER LOGO FAVICON
%token <string> STRING

%start <Config_ast.t list> file
%%

file:
  | l = value_list ; EOF { List.rev l }
  ;

value_list:
  | (* Empty list *) { [] }
  | vl = value_list ; NAME ; COLON ; name = STRING { Config_ast.Name name :: vl }
  | vl = value_list ; MODULES ; COLON ; ml = string_list { Config_ast.Modules (List.rev ml) :: vl }
  | vl = value_list ; CUSTOM ; CSS ; COLON ; cl = string_list { Config_ast.CustomCSS (List.rev cl) :: vl }
  | vl = value_list ; HEADER ; LOGO ; COLON ; logo = STRING { Config_ast.HeaderLogo logo :: vl }
  | vl = value_list ; FAVICON ; COLON ; ico = STRING { Config_ast.Favicon ico :: vl }
  ;

string_list:
  | (* empty *) { [] }
  | pl = string_list ; path = STRING { path :: pl }
