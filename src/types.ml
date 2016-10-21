open List
open Printf

(* Types *)
type opcode =
  | Plus | Minus | Times | Divide | Modulus
  | Lth | Gth | Leq | Geq
  | PreInc | PreDec | PostInc | PostDec
  | Equal | Noteq | And | Or | Not

type expression =
  | Empty (* Empty function call: a() or empty statements: ;; *)
  | Seq of expression list
  | While of expression * expression (* while e do e *)
  | If of expression * expression * expression (* if e do e else e *)
  | Asg of expression * expression (* e := e *)
  | Deref of expression (* e *)
  | UnaryOp of opcode * expression (* !e *)
  | BinaryOp of opcode * expression * expression (* e + e *)
  | Application of expression * expression (* e(e) *)
  | Const of int (* 7 *)
  | Boolean of bool (* true; false *)
  | Readint (* read_int () *)
  | Printint of expression (* print_int (e) *)
  | Identifier of string (* x *)
  | Let of string * expression * expression (* let x = e in e *)
  | New of string * expression * expression (* new x = e in e *)

type fundef = string * string list * expression 
type program = fundef list


(* Helper functions *)
let esc s = "\"" ^ s ^ "\""
let wrap s = "(" ^ s ^ ")"
let wrap_sq s = "[" ^ s ^ "]"
let rec opt_prepend_seq l = function
  | None   -> seq_of_list l
  | Some a -> seq_of_list (a :: l)
and seq_of_list = function
  | []  -> Empty
  | [e] -> e
  | l   -> Seq l

and func_body = function (name, args, body) -> body
;;
