let copy src dest =
  let command = Printf.sprintf "cp %s %s" src dest in
  if not (Sys.file_exists dest) then
    Unix.system command |> ignore

(* mkdir if does not exists *)
let fmkdir dir =
  if not (Sys.file_exists dir) then
    Unix.mkdir dir 0o777

let rec relative_root = function
  | 0 -> ""
  | n -> "../" ^ (relative_root (n-1))

let gen_relative directory modules =
  let open ASTpp in
  let rec gen_aux directory modl =
    let dir = Unix.getcwd () in
    fmkdir directory ;
    Unix.chdir directory ;
    let output = open_out (String.lowercase_ascii modl.modulename ^ ".html") in
    Printf.fprintf output "<!DOCTYPE html>\n<html>\n%s\n<body>%s\n%s</body>\n</html>"
      (Docgen.gen_header (relative_root (List.length modl.hierarchy)) modl.modulename)
      (Docgen.gen_aside (relative_root (List.length modl.hierarchy)) (Some modl) modules)
      (Docgen.gen_main (relative_root (List.length modl.hierarchy)) modl);
    close_out output;
    List.iter (fun modl' -> gen_aux (String.lowercase_ascii modl.modulename) modl')
      modl.submodules;
    List.iter (fun modl' -> gen_aux (String.lowercase_ascii modl.modulename) modl')
      modl.signatures;
    Unix.chdir dir
  in
  List.iter (gen_aux directory) modules

let () =
  (* Simply to allow install *)
  ignore (Html.h1 [] []) ;
  (* END Useless *)
  if Array.length Sys.argv < 1 then begin
    print_endline "Usage : docaml <file1.mli> ... <fileN.mli>";
    exit 2
  end;
  fmkdir "doc" ;
  fmkdir "doc/css" ;
  fmkdir "doc/script" ;
  fmkdir "doc/img" ;
  let modules =
    Array.to_list Sys.argv
    |> List.tl
    |> List.map (Docgen.preprocess_file)
  in
  gen_relative "doc" modules;
  let output = open_out "doc/index.html" in
  Printf.fprintf output "<!DOCTYPE html>\n<html>\n%s\n<body>%s\n%s</body>\n</html>"
    (Docgen.gen_header "" "Index")
    (Docgen.gen_aside "" None modules)
    (Docgen.gen_index_main "" modules);
  close_out output;
  (* Resource directory *)
  let rdir s =
    String.concat "" [
      Filename.dirname Sys.executable_name ;
      "/../share/docaml/" ;
      s
    ]
  in
  copy (rdir "doc.css") "doc/css/doc.css" ;
  copy (rdir "highlight.pack.js") "doc/script/highlight.pack.js" ;
  copy (rdir "doc.js") "doc/script/doc.js" ;
  copy (rdir "monokai.css") "doc/css/monokai.css"
