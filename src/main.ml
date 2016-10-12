open Types
open Printf

let usage () = print_string ("Usage: " ^ Sys.argv.(0) ^ " <filename>")
    |> print_newline;;

let location_message filename lexbuf = 
    let offs = Lexing.lexeme_start_p lexbuf in
    sprintf "%s: at line %d, column %d" filename offs.pos_lnum (offs.pos_cnum - offs.pos_bol + 1)

let safe_parse filename lexbuf = try Parser.init Lexer.read lexbuf with
    | Parser.Error          -> eprintf "Parse error: %s\n%s\n" 
                                 (Lexing.lexeme lexbuf)
                                 (location_message filename lexbuf);
                               exit (-1)
    | Lexer.SyntaxError msg -> eprintf "%s\n%s\n" msg
                                 (location_message filename lexbuf);
                                 exit (-1)

let parse file = open_in file
    |> Lexing.from_channel
    |> safe_parse file
    |> List.map Types.string_of_func
    |> String.concat "\n"
    |> print_string
    |> print_newline

let _ =
    if Array.length Sys.argv < 2
    then (usage (); ignore (exit 1))
    else parse Sys.argv.(1)

