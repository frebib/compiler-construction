open Types
open Error
open Printf
open Lexing

let usage () = printf "Usage: %s <filename>\n" Sys.argv.(0)

let location_message filename lexbuf = 
  let offs = Lexing.lexeme_start_p lexbuf in
  let col = offs.pos_cnum - offs.pos_bol + 1 in
  let line = offs.pos_lnum in
  sprintf "%s: at line %d, column %d" filename line col

let safe_parse filename lexbuf =
  try Parser.init Lexer.read lexbuf
  with
  | Parser.Error -> eprintf "Parse error on: %s\n%s\n" (Lexing.lexeme lexbuf)
                    (location_message filename lexbuf);
                    exit (-1)
  | CompileError (typ, get_msg) -> match typ with
    | _ -> eprintf "%s\n%s\n" (get_msg lexbuf) (location_message filename lexbuf);
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
  then (usage (); exit 1)
  else parse Sys.argv.(1)

