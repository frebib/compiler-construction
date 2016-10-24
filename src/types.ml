open List

(* Types *)
type opcode =
  | Plus | Minus | Times | Divide | Modulus
  | Lth | Gth | Leq | Geq
  | PreInc | PreDec | PostInc | PostDec
  | Equal | Noteq | And | Or | Not

type expression =
  | Empty (* Empty function call: a() or empty statements: ;; *)
  | Function of string list * expression
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

(* Helper functions *)
let esc s = "\"" ^ s ^ "\""
let wrap s = "(" ^ s ^ ")"
let wrap_sq s = "[" ^ s ^ "]"
let rec list_of_seq = function
  | Seq l -> l
  | e -> [e]
and append_seq l e = seq_of_list (List.append l e)
and opt_append_seq l = function
  | None   -> seq_of_list l
  | Some a -> append_seq l [a]
and seq_of_list = function
  | []  -> Empty
  | [e] -> e
  | l   -> Seq l

