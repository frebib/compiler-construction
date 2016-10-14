open Types
open Error
open Printf
open Lexing

let usage () = printf "Usage: %s <filename>\n" Sys.argv.(0)

let location_message filename lexbuf = 
  let offs = Lexing.lexeme_start_p lexbuf in
  let col_beg = offs.pos_cnum - offs.pos_bol + 1 in
  let col_end = col_beg + (String.length (lexeme lexbuf)) in
  let line = offs.pos_lnum in
  sprintf "File: \"%s\" at line %d, columns %d-%d" filename line col_beg col_end

let safe_parse filename lexbuf =
  try Parser.init Lexer.read lexbuf
  with
  | Parser.Error -> eprintf "Unexpected token: %s\n%s\n" (Lexing.lexeme lexbuf)
                    (location_message filename lexbuf);
                    exit (-1)
  | CompileError (typ, get_msg) -> match typ with
    | _ -> eprintf "%s\n%s\n" (get_msg lexbuf) (location_message filename lexbuf);
           exit (-1)

let parse file = open_in file
  |> Lexing.from_channel
  |> safe_parse file
  |> List.map Types.string_of_func
  |> List.map wrap
  |> String.concat ",\n"
  |> sprintf "[%s]"
  |> print_string
  |> print_newline

let _ =
  if Array.length Sys.argv < 2
  then (usage (); exit 1)
  else parse Sys.argv.(1)

