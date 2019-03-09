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

let gen_relative config directory modules =
  let open ASTpp in
  let rec gen_aux directory modl =
    let dir = Unix.getcwd () in
    fmkdir directory ;
    Unix.chdir directory ;
    let output = open_out (String.lowercase_ascii modl.modulename ^ ".html") in
    let page =
      let root = relative_root (List.length modl.hierarchy) in
      Html.html [] [
        Docgen.gen_header config root modl.modulename ;
        Docgen.gen_aside config root (Some modl) modules ;
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

let docaml_build () =
  (* Getting configuration *)
  if not (Sys.file_exists "docaml") then
    Printf.printf "You should have a docaml file in this directory\n" ;
  let config = Config.from_file "docaml" in
  (* Creating the doc *)
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
  (* If there is custom css to copy, we do it now *)
  begin match Config.custom_css config with
  | Some css ->
    fmkdir "doc/css/custom" ;
    List.iter (fun f -> copy f ("doc/css/custom/" ^ Filename.basename f)) css
  | None -> ()
  end ;
  (* Same with custom logo *)
  begin match Config.header_logo config with
  | Some logo -> copy logo ("doc/img/" ^ Filename.basename logo)
  | None -> ()
  end ;
  (* And with favicon *)
  begin match Config.favicon config with
  | Some ico -> copy ico ("doc/img/" ^ Filename.basename ico)
  | None -> ()
  end ;
  (* Now we can generate the doc itself *)
  let modules =
    Config.modules config
    |> List.map Docgen.preprocess_file
  in
  gen_relative config "doc" modules;
  (* Index page *)
  let output = open_out "doc/index.html" in
  let page =
    Html.html [] [
      Docgen.gen_header config "" "Index" ;
      Docgen.gen_aside config "" None modules ;
      Docgen.gen_index_main config "" modules
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
        Docgen.gen_header config "" "Search" ;
        Docgen.gen_aside config "" None modules ;
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

let docaml_clean () =
  Printf.printf "Not implemented yet\n"

let docaml_init () =
  Printf.printf "Not implemented yet\n"

let () =
  let args =
    Array.to_list Sys.argv
    |> List.tl
  in
  match args with
  | [] ->
    if Sys.file_exists "docaml" then begin
      try begin
        let _ = Config.from_file "docaml" in
        Printf.printf "Type the following to build the doc: docaml build\n"
      end
      with Config.Error s ->
        Printf.printf "There are some errors in the `docaml` file:\n%s" s
    end
    else begin
      Printf.printf "`docaml` file is missing.\nUse `docaml init` to create it step by step.\n"
    end
  | [ "build" ] -> docaml_build ()
  | [ "clean" ] -> docaml_clean ()
  | [ "init" ] -> docaml_init ()
  | _ -> Printf.printf "Error. Type `docaml` for more information.\n"
