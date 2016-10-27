open Types
open Print
open Printf
open Lexing

type eval_err =
  | IncorrectType of string * expression
  | UndefinedVar of string
  | InvalidDeref of expression
  | OverAppliedArgs of int * int
  | Error of string
;;
let string_of_eval_etype = function
  | IncorrectType (typ, exp) -> sprintf "Incorrect type. Expected a %s type: %s" typ (string_of_exp exp)
  | UndefinedVar var -> sprintf "Variable '%s' is not defined" var
  | InvalidDeref exp -> sprintf "Cannot dereference a non-reference type: %s" (string_of_exp exp)
  | OverAppliedArgs (exp, act) -> sprintf "Function applied to too many arguments. Expected %d, Actual %d" exp act
  | Error s -> s
;; 
type error =
  | Syntax
  | Parse
  | Unimplemented
;;
let string_of_etype = function
  | Syntax -> "Syntax"
  | Parse  -> "Parse"
  | Unimplemented -> "Unimplemented"
;;

exception CompileError of error * (Lexing.lexbuf -> string) option
exception EvaluationError of eval_err

let error_of_fn typ fn = CompileError (typ, Some fn)
let error_of    typ s  = CompileError (typ, Some (fun _ -> s))
let error_empty tru fal = function
  | None -> raise fal
  | _    -> tru
;;
let lexer_error = error_of_fn Syntax (fun buf -> "Unexpected token: " ^ (lexeme buf))
let unimpl_error msg = error_of Unimplemented msg
let syntax_error msg = error_of Parse msg
let eval_error typ = EvaluationError typ

let error_message buf = function
  | CompileError (err, None) -> sprintf "%sError: Unspecified reason :(" (string_of_etype err)
  | CompileError (err, Some get_msg) -> sprintf "%sError: %s" (string_of_etype err) (get_msg buf)
  | EvaluationError typ -> sprintf "EvalError: %s" (string_of_eval_etype typ)
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

