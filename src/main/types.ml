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
  (* failwith ("Can't unflatten a non-Seq expression: " ^ string_of_exp e) *)


(* string_of_x functions mostly for debugging *)
and string_of_op = function
  | Plus -> "+" | Minus -> "-" | Times -> "*" |  Divide -> "/" | Modulus -> "%"
  | Lth -> "<" | Gth -> ">" | Leq -> "<=" | Geq -> ">="
  | PreInc | PostInc -> "++" | PreDec | PostDec -> "--"
  | Equal -> "==" | Noteq -> "!=" | And -> "&&" | Or -> "||" | Not -> "!"

and string_of_op_exp op exp = 
  let e = string_of_exp exp in
  match op with
  | Not               -> "Not " ^ wrap e
  | PostInc | PostDec -> wrap e ^ string_of_op op
  | PreInc | PreDec   -> string_of_op op ^ wrap e
  | _ -> failwith (sprintf "'%s' is not a unary operator" (string_of_op op))

and string_of_exp = function
  | Empty                 -> "Empty"
  | Seq (hd, tl)          -> "Seq [" ^ (list_of_seq (Seq (hd, tl)) |> map string_of_exp |>  String.concat ", ") ^ "]"
  | While (e, f)          -> "While (" ^ string_of_exp e ^ ") { " ^ string_of_exp f ^ " }"
  | If (e, a, b)          -> "If " ^ ([e;a;b] |> map string_of_exp |> map wrap |> String.concat ", ")
  | Asg (x, v)            -> "Assign " ^ string_of_exp x ^ " = " ^ string_of_exp v
  | Deref x               -> "Deref " ^ wrap (string_of_exp x)
  | UnaryOp (op, e)       -> string_of_op_exp op e
  | BinaryOp (op, e1, e2) -> string_of_exp e1 ^ " " ^ string_of_op op ^ " " ^ string_of_exp e2
  | Application (i, e)    -> "Application " ^ wrap (string_of_exp i) ^ ", " ^ wrap (string_of_exp e)
  | Const n               -> "Const " ^ string_of_int n
  | Boolean b             -> string_of_bool b
  | Readint               -> "read_int()"
  | Printint e            -> "print_int(" ^ string_of_exp e ^ ")"
  | Identifier s          -> "Identifier " ^ esc s
  | Let (x, v, e)         -> "Let " ^ esc x ^ " = " ^ wrap (string_of_exp v) ^ " in " ^ wrap (string_of_exp e)
  | New (x, v, e)         -> "New " ^ esc x ^ " = " ^ string_of_exp v ^ " in " ^ string_of_exp e

and string_of_func = function
  | (name, args, exp) -> let arr = String.concat ", " (map esc args) in
                         let exp_str = string_of_exp exp in
                         sprintf "Function (%s, [%s], { %s }" (esc name) arr exp_str 
