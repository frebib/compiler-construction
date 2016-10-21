open List

(* Types *)
type opcode =
  | Plus | Minus | Times | Divide | Modulus
  | Lth | Gth | Leq | Geq
  | PreInc | PreDec | PostInc | PostDec
  | Equal | Noteq | And | Or | Not

type expression =
  | Empty (* Empty function call: a() or empty statements: ;; *)
  | Seq of expression * expression (* e; e *)
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
let rec opt_prepend_seq l opt = 
  let prepend l = function
  | None   -> l
  | Some a -> (a :: l)
  in seq_of_list (prepend l opt)

and seq_of_list = function
  | []      -> Empty
  | [x]     -> x
  | x :: xs -> Seq (x, seq_of_list xs)

and list_of_seq = function
  | Seq (hd, tl) -> hd :: list_of_seq tl
  | e            -> [e]

and func_body = function (name, args, body) -> body
;;
