%token EOF
%token COLON
%token NAME MODULES
%token <string> STRING

%start <Config_ast.t list> file
%%

file:
  | l = value_list ; EOF { List.rev l }
  ;

value_list:
  | (* Empty list *) { [] }
  | vl = value_list ; NAME ; COLON ; name = STRING { Config_ast.Name name :: vl }
  | vl = value_list ; MODULES ; COLON ; ml = path_list { Config_ast.Modules (List.rev ml) :: vl }
  ;

path_list:
  | (* empty *) { [] }
  | pl = path_list ; path = STRING { path :: pl }
