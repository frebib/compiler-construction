open Printf
open Lexing

type error =
  | Syntax
  | Parse
;;
let string_of_etype = function
  | Syntax -> "Syntax"
  | Parse  -> "Parse"
;;

exception CompileError of error * (Lexing.lexbuf -> string) option

let error_of_fn typ fn = CompileError (typ, Some fn)
let error_of    typ s  = CompileError (typ, Some (fun _ -> s))
let error_empty tru fal = function
  | None -> raise fal
  | _    -> tru
;;
let lexer_error = error_of_fn Syntax (fun buf -> "Unexpected token: " ^ (lexeme buf))
let syntax_error msg = error_of Parse msg

let error_message buf = function
  | CompileError (err, None) -> sprintf "%sError: Unspecified reason :(" (string_of_etype err)
  | CompileError (err, Some get_msg) -> sprintf "%sError: %s" (string_of_etype err) (get_msg buf)
  | Failure s -> failwith "Error: Failure " ^ s
  | _ -> failwith "Error: Unknown exception"
;;

let location_message filename lexbuf = 
  let offs = Lexing.lexeme_start_p lexbuf in
  let col_beg = offs.pos_cnum - offs.pos_bol + 1 in
  let col_end = col_beg + (String.length (lexeme lexbuf)) in
  let line = offs.pos_lnum in
  sprintf "File \"%s\" at line %d, columns %d-%d:" filename line col_beg col_end

let safe parse lex filename lexbuf =
  try parse lex lexbuf
  with err -> eprintf "%s\n%s" (location_message filename lexbuf) (error_message lexbuf err); exit (-1)
;;

