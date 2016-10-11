open Types

let usage () = print_string ("Usage: " ^ Sys.argv.(0) ^ " <filename>")
    |> print_newline;;

let safe_parse lexbuf = try
    Parser.init Lexer.read lexbuf with
    | Parser.Error          -> print_string ("Error: " ^ (Lexing.lexeme lexbuf))
                               |> print_newline;
                               exit (-1)
    | Lexer.SyntaxError msg -> print_string msg
                               |> print_newline;
                               exit (-1)

let parse file = open_in file
    |> Lexing.from_channel
    |> safe_parse
    |> List.map Types.string_of_func
    |> String.concat "\n"
    |> print_string

let _ =
    if Array.length Sys.argv < 2
    then (usage (); ignore (exit 1))
    else parse Sys.argv.(1)

