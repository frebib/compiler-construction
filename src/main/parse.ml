open Print
open Printf

(* Tree parsing functions *)
let parse = Error.safe Parser.init Lexer.read

let parse_print file buf =
  parse file buf
  |> List.map string_of_func
  |> String.concat ";\n"
  |> sprintf "[\n%s\n]"
  |> print_string
  |> print_newline

let parse_from file =
  open_in file
  |> Lexing.from_channel
  |> parse file

let parse_print_from file =
  open_in file
  |> Lexing.from_channel
  |> parse_print file

