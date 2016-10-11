let usage () = print_string ("Usage: " ^ Sys.argv.(0) ^ " <filename>")
    |> print_newline;;

let parse file = open_in file
    |> Lexing.from_channel
    |> Parser.init Lexer.read
    |> ignore

let _ =
    if Array.length Sys.argv < 2
    then (usage (); ignore (exit 1))
    else parse Sys.argv.(1)

