open List

type opcode =
  | Plus | Minus | Times | Divide
  | Lth | Gth | Leq | Geq
  | Equal | Noteq | And | Or | Not

type expression =
  | Empty (* Empty function call: a() or empty statements: ;; *)
  | Seq of expression * expression (* e; e *)
  | While of expression * expression (* while e do e *)
  | For of expression * expression * expression * expression (* for (e;e;e) do e *)
  | If of expression * expression * expression (* if e do e else e *)
  | Asg of expression * expression (* e := e *)
  | Deref of expression (* !e *)
  | Operator of opcode * expression * expression (* e + e *)
  | Application of expression * expression (* e(e) *)
  | Const of int (* 7 *)
  | Readint (* read_int () *)
  | Printint of expression (* print_int (e) *)
  | Identifier of string (* x *)
  | Let of string * expression * expression (* let x = e in e *)
  | New of string * expression * expression (* new x = e in e *)

type fundef = Func of string * string list * expression 
type program = fundef list

let esc s = "\"" ^ s ^ "\""
let rec flatten_exp = function
  | []      -> Empty
  | [x]     -> x
  | x :: xs -> Seq (x, flatten_exp xs)

and unflatten_exp = function
  | Seq (hd, tl) -> hd :: unflatten_exp tl
  | Empty        -> []
  | e            -> [e]
  (* failwith ("Can't unflatten a non-Seq expression: " ^ string_of_exp e) *)

(* Why this isn't implicit I have no idea *)
and string_of_op = function
    | Plus -> "+" | Minus -> "-" | Times -> "*" |  Divide -> "/"
    | Lth -> "<" | Gth -> ">" | Leq -> "<=" | Geq -> ">="
    | Equal -> "==" | Noteq -> "!=" | And -> "&&" | Or -> "||" | Not -> "!"

and string_of_exp = function
    | Empty                 -> "Empty"
    | Seq (hd, tl)          -> "Seq (" ^ (unflatten_exp (Seq (hd, tl)) |> map string_of_exp |> String.concat ", ") ^ ")" 
    | While (e, f)          -> "While (" ^ string_of_exp e ^ ") { " ^ string_of_exp f ^ " }"
    | For (a, b, c, d)      -> "For (" ^ (map string_of_exp [a;b;c] |> String.concat "; ") ^ ") {" ^ string_of_exp d ^ "}"
    | If (e, a, b)          -> "If (" ^ string_of_exp e ^ ") { " ^ (match b with
                                | Empty -> " }"
                                | _     -> " } Else { " ^ string_of_exp b ^ " }")
    | Asg (x, v)            -> string_of_exp x ^ " = " ^ string_of_exp v
    | Deref x               -> string_of_exp x
    | Operator (op, e1, e2) -> string_of_exp e1 ^ string_of_op op ^ string_of_exp e2
    | Application (i, e)    -> string_of_exp i ^ "(" ^ string_of_exp e ^ ")"
    | Const n               -> "Const " ^ string_of_int n
    | Readint               -> "read_int()"
    | Printint e            -> "print_int(" ^ string_of_exp e ^ ")"
    | Identifier s          -> "Identifier " ^ esc s
    | Let (x, v, e)         -> "Let " ^ x ^ " = " ^ string_of_exp v ^ " in " ^ string_of_exp e
    | New (x, v, e)         -> "New " ^ x ^ " = " ^ string_of_exp v ^ " in " ^ string_of_exp e

and string_of_func = function
    | Func (name, args, exp) -> "Function " ^ esc name ^ "(" ^ String.concat ", " args ^
                                    ") { " ^ string_of_exp exp ^ " }"
