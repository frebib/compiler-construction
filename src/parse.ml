open Print
open Printf

(* Tree parsing functions *)
let parse = Error.safe Parser.init Lexer.read

let parse_print file buf =
  parse file buf
  |> Print.string_of_exp
  |> Print.indent 2
  |> printf "[\n%s\n]\n"

let parse_from file =
  open_in file
  |> Lexing.from_channel
  |> parse file

let parse_print_from file =
  open_in file
  |> Lexing.from_channel
  |> parse_print file

