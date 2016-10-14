open Types
open Printf

(* Tree parsing functions *)
let parse = Error.safe Parser.init Lexer.read

let parse_print file buf = 
  parse file buf
  |> List.map string_of_func
  |> List.map wrap
  |> String.concat ",\n"
  |> sprintf "[%s]"
  |> print_string
  |> print_newline

let parse_from file =
  open_in file
  |> Lexing.from_channel
  |> parse file


