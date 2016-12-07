open Printf 

type reg = 
  (* 8 bit registers *)
  | AL   | BL   | CL   | DL
  | AH   | BH   | CH   | DH
  | SIL  | DIL  | BPL  | SPL
  | R8B  | R9B  | R10B | R11B 
  | R12B | R13B | R14B | R15B
  (* 64 bit registers *)
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
  | DRegister of reg
  | BasePtrOffs of int
  | ConstInt of int
  | Void

type target =
  | Register of reg
  | EmptyRegister
  | Stack
  | Discard

let string_of_reg = fun e -> "%" ^ (match e with
  (* 8 bit registers *)
  | AL   -> "al"   | BL   -> "bl"   | CL   -> "cl"   | DL   -> "dl"
  | AH   -> "ah"   | BH   -> "bh"   | CH   -> "ch"   | DH   -> "dh" 
  | SIL  -> "sil"  | DIL  -> "dil"  | BPL  -> "bpl"  | SPL  -> "spl"
  | R8B  -> "r8b"  | R9B  -> "r9b"  | R10B -> "r10b" | R11B -> "r11b"
  | R12B -> "r12b" | R13B -> "r13b" | R14B -> "r14b" | R15B -> "r15b"
  (* 64 bit registers *)
  | RAX -> "rax" | RBX -> "rbx" | RCX -> "rcx" | RDX -> "rdx"
  | RSI -> "rsi" | RDI -> "rdi" | RBP -> "rbp" | RSP -> "rsp"
  | R8  -> "r8"  | R9  -> "r9"  | R10 -> "r10" | R11 -> "r11"
  | R12 -> "r12" | R13 -> "r13" | R14 -> "r14" | R15 -> "r15")

let string_of_location = function
  | DRegister v   -> string_of_reg v
  | BasePtrOffs o -> sprintf "%d(%%rbp)" o   
  | ConstInt i    -> sprintf "$%d" i
  | Void          -> "<void>"

let lbyte_reg = function
  | RAX -> AL   | RBX -> BL
  | RCX -> CL   | RDX -> DL
  | RSI -> SIL  | RDI -> DIL
  | RBP -> BPL  | RSP -> SPL
  | R8  -> R8B  | R9  -> R9B
  | R10 -> R10B | R11 -> R11B
  | R12 -> R12B | R13 -> R13B
  | R14 -> R14B | R15 -> R15B
  | r -> failwith ("No matching low-byte register for " ^ string_of_reg r)

let arg_regs = [ RDI; RSI; RDX; RCX; R8; R9 ]
let arg_reg i = Register (List.nth arg_regs i)
let arg_dest i = if i < 6 then arg_reg i else Stack

