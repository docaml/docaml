%token EOF
%token LPAREN RPAREN
%token COLON
%token LBRACE RBRACE LBRACK RBRACK
%token NAME MODULES

%start <Config_ast.t list> file
%%

file:
  | l = value_list ; EOF { List.rev l }
  ;

value_list:
  | (* Empty list *) { [] }
  | vl = value_list ; NAME ; COLON ; name = STRING { Name name :: vl }
  | vl = value_list ; MODULES ; COLON ; ml = path_list { Modules (List.rev ml) :: vl }
  ;

path_list:
  | (* empty *) { [] }
  | pl = path_list ; path = STRING { path :: pl }
