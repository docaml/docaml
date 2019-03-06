let copy src dest =
  let command = Printf.sprintf "cp %s %s" src dest in
  if not (Sys.file_exists dest) then
    Unix.system command |> ignore

let cpdir src dest =
  let command = Printf.sprintf "cp -r %s %s" src dest in
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
    let page =
      let root = relative_root (List.length modl.hierarchy) in
      Html.html [] [
        Docgen.gen_header root modl.modulename ;
        Docgen.gen_aside root (Some modl) modules ;
        Docgen.gen_main root modl
      ]
    in
    Printf.fprintf output "%s" (Html.document_to_string page) ;
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
    print_endline "Usage : docaml <file1.mli> ... <fileN.mli>" ;
    exit 2
  end;
  fmkdir "doc" ;
  fmkdir "doc/css" ;
  fmkdir "doc/script" ;
  fmkdir "doc/img" ;
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
  copy (rdir "monokai.css") "doc/css/monokai.css" ;
  cpdir (rdir "tipuesearch/") "doc/script/tipuesearch/" ;
  (* Now we can generate the doc itself *)
  let modules =
    Array.to_list Sys.argv
    |> List.tl
    |> List.map (Docgen.preprocess_file)
  in
  gen_relative "doc" modules;
  (* Index page *)
  let output = open_out "doc/index.html" in
  let page =
    Html.html [] [
      Docgen.gen_header "" "Index" ;
      Docgen.gen_aside "" None modules ;
      Docgen.gen_index_main "" modules
    ]
  in
  Printf.fprintf output "%s" (Html.document_to_string page) ;
  close_out output ;
  (* Search page *)
  let output = open_out "doc/search.html" in
  let page =
    let open Html in
    let open Attribute in
    html [] [
      head [] [
        Docgen.gen_header "" "Search" ;
        Docgen.gen_aside "" None modules ;
        main [] [
          h1 [] [ text "Search" ] ;
          form [] [
            div [ classes [ "tipue_search_group" ] ] [
              input [
                typ "text" ;
                name "q" ;
                id "tipue_search_input" ;
                pattern ".{3,}" ;
                title "At least 3 characters" ;
                required
              ]
            ]
          ] ;
          div [ id "tipue_search_content" ] []
        ] ;
        script [] [
          text "$(document).ready(function() {
                  $('#tipue_search_input').tipuesearch();
                });"
        ]
      ]
    ]
  in
  Printf.fprintf output "%s" (Html.document_to_string page) ;
  close_out output ;
  (* Search content *)
  let flatmap f l = List.flatten (List.map f l) in
  let rec search_aux path modl : string list =
    let open ASTpp in
    let path = path ^ String.lowercase_ascii modl.modulename in
    let url = path ^ ".html" in
    let path = path ^ "/" in
    Printf.sprintf
      "{ \"title\":\"%s\", \"text\":\"%s\", \"tags\":\"%s\", \"note\":\"%s\", \"url\":\"%s\" }"
      modl.modulename
      (String.escaped (Docgen.gen_search_content modl))
      "" (* TODO Tags *)
      (String.escaped (Docgen.gen_search_note modl))
      url
    :: flatmap (search_aux path) modl.submodules
    @ flatmap (search_aux path) modl.signatures
  in
  let output = open_out "doc/script/tipuesearch/tipuesearch_content.js" in
  Printf.fprintf output
    "var tipuesearch = {\"pages\": [%s]};"
    (String.concat ", " (flatmap (search_aux "") modules)) ;
  close_out output
