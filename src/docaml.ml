(* open Docgen *)

let copy src dest =
  let command = Printf.sprintf "cp %s %s" src dest in
  if not (Sys.file_exists dest) then
    Unix.system command |> ignore

let rec relative_root = function
  | 0 -> ""
  | n -> "../" ^ (relative_root (n-1))

let gen_relative directory modules =
  let open ASTpp in
  let rec gen_aux directory modl =
    let dir = Unix.getcwd () in
    if not (Sys.file_exists directory) then
      Unix.mkdir directory 0o777;
    Unix.chdir directory;
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
  if Array.length Sys.argv < 1 then begin
    print_endline "Usage : docaml <file1.mli> ... <fileN.mli>";
    exit 2
  end;
  if not (Sys.file_exists "doc") then
    Unix.mkdir "doc" 0o777;
  if not (Sys.file_exists "doc/css") then
    Unix.mkdir "doc/css" 0o777;
  if not (Sys.file_exists "doc/script") then
    Unix.mkdir "doc/script" 0o777;
  if not (Sys.file_exists "doc/img") then
    Unix.mkdir "doc/img" 0o777;
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
  (* Printf.printf "executable name: %s\n" Sys.executable_name ; *)
  (* TODO This is not good, because it is OS dependent *)
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
