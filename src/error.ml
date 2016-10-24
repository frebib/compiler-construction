open Printf
open Lexing

type error =
  | Syntax
  | Parse
  | Eval
  | Unimplemented
;;
let string_of_etype = function
  | Syntax -> "Syntax"
  | Parse  -> "Parse"
  | Eval   -> "Eval"
  | Unimplemented -> "Unimplemented"
;;

exception CompileError of error * (Lexing.lexbuf -> string) option

let error_of_fn typ fn = CompileError (typ, Some fn)
let error_of    typ s  = CompileError (typ, Some (fun _ -> s))
let error_empty tru fal = function
  | None -> raise fal
  | _    -> tru
;;
let lexer_error = error_of_fn Syntax (fun buf -> "Unexpected token: " ^ (lexeme buf))
let unimpl_error msg = error_of Unimplemented msg
let syntax_error msg = error_of Parse msg
let eval_error msg = error_of Eval msg

let error_message buf = function
  | CompileError (err, None) -> sprintf "%sError: Unspecified reason :(" (string_of_etype err)
  | CompileError (err, Some get_msg) -> sprintf "%sError: %s" (string_of_etype err) (get_msg buf)
  | e -> Printexc.to_string e
;;

let line_of_buf buf = (Lexing.lexeme_start_p buf).pos_lnum
let col_of_buf buf  = let offs = Lexing.lexeme_start_p buf in
                      offs.pos_cnum - offs.pos_bol + 1

let location_message filename lexbuf = 
  let col_beg = col_of_buf lexbuf in
  let col_end = col_beg + (String.length (lexeme lexbuf)) in
  let line = line_of_buf lexbuf in
  sprintf "File \"%s\" at line %d, columns %d-%d:" filename line col_beg col_end

let print_trace err filename buf = eprintf "%s\n%s\n" (location_message filename buf) (error_message buf err)

let safe parse lex filename lexbuf =
  try parse lex lexbuf
  with err -> print_trace err filename lexbuf; exit (-1)
;;

