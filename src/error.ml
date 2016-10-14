open Printf
open Lexing

type error =
  | Syntax
  | Parse

exception CompileError of error * (Lexing.lexbuf -> string)

let error_of_fn typ fn = CompileError (typ, fn)
let error_of    typ s  = CompileError (typ, (fun _ -> s))
let error_empty tru fal = function
  | None -> raise fal
  | _    -> tru
;;

let error_no_semicolon = error_of Parse "Unexpected end of line. Semicolon expected"
let error_no_rbrace = error_of Parse "Unexpected token. } expected"

let location_message filename lexbuf = 
  let offs = Lexing.lexeme_start_p lexbuf in
  let col_beg = offs.pos_cnum - offs.pos_bol + 1 in
  let col_end = col_beg + (String.length (lexeme lexbuf)) in
  let line = offs.pos_lnum in
  sprintf "File \"%s\" at line %d, columns %d-%d" filename line col_beg col_end

let safe parse lex filename lexbuf =
  try parse lex lexbuf
  with
  | CompileError (typ, get_msg) -> (match typ with
    | _ -> eprintf "%s\n%s\n" (get_msg lexbuf) (location_message filename lexbuf);
           exit (-1))
  | _ -> eprintf "%s:\nUnexpected token: %s\n" (location_message filename lexbuf)
                    (lexeme lexbuf); exit (-1) 

