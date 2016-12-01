open Printf 

type reg64 = 
  | RAX | RBX | RCX | RDX
  | RSI | RDI | RBP | RSP
  | R8  | R9  | R10 | R11
  | R12 | R13 | R14 | R15

(* All 64bit general purpose registers *)
let all_reg64 = [
  RAX; RBX; RCX; RDX;
  RSI; RDI; (*RBP; RSP;*)
  R8;  R9;  R10; R11;
  R12; R13; R14; R15;
]

type location =
  | DRegister of reg64
  | BasePtrOffs of int
  | ConstInt of int
  | Void

type target =
  | Register of reg64
  | EmptyRegister
  | Stack
  | Discard

let string_of_reg64 = fun e -> "%" ^ (match e with
  | RAX -> "rax" | RBX -> "rbx" | RCX -> "rcx" | RDX -> "rdx"
  | RSI -> "rax" | RDI -> "rdi" | RBP -> "rbp" | RSP -> "rsp"
  | R8 -> "r8"   | R9 -> "r9"   | R10 -> "r10" | R11 -> "r11"
  | R12 -> "r12" | R13 -> "r13" | R14 -> "r14" | R15 -> "r15")

let string_of_location : (location -> string) = function
  | DRegister v    -> string_of_reg64 v
  | BasePtrOffs o -> sprintf "%d(%%rbp)" o   
  | ConstInt i    -> sprintf "$%d" i
  | Void          -> "<void>"

let arg_reg = fun i -> Register (match i with
	| 0 -> RDI
	| 1 -> RSI
	| 2 -> RDX
	| 3 -> RCX
	| 4 -> R8
	| 5 -> R9
	| _ -> failwith "arg_reg")

