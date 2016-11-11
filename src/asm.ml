open Array
open Types
open Print
open Printf

let itob 				 = function 1 -> true | _ -> false
let btoi b       = if b then 1 else 0
let optoi op a b = btoi (op a b)
let btoifn o a b = o (itob a) (itob b)
let instr_of_op  = function
  | Plus    -> "add"
  | Minus   -> "sub"
  | Times   -> "mul"
  | Divide  -> "div"
  | Modulus -> "mod"
  | Lth     -> "lth"
  | Gth     -> "gth"
  | Leq     -> "leq"
  | Geq     -> "geq"
  | Equal   -> "eq"
  | Noteq   -> "neq"
  | And     -> "and"
  | Or      -> "or"
  | Not     -> "not"

let pad len str =
  let pad_sz = len - (String.length str) in
  if pad_sz == 0 then str
  else str ^ (Bytes.make pad_sz ' ' |> Bytes.to_string)

module type Compiler = sig
  type addr
  val empty_addr : addr

  val sp  : addr ref
  val fp  : addr ref
  val acc : addr ref
  
  val set_sp : addr -> unit
  val inc_sp : unit -> addr

  val ld  : int  -> unit
  val ldc : int  -> unit
  val st  : addr -> unit
  val mv  : addr -> addr -> unit
  val op  : opcode -> addr -> addr -> unit
end

module MakeCompiler (C : Compiler) = struct

  let symtbl = Hashtbl.create 1024
  let last   = ref (C.empty_addr)

  let rec compile = function
    | Let (v, e1, e2)  -> let addr1 = compile e1; !last in
                          Hashtbl.replace symtbl v !C.sp;
													let addr2 = compile e2; !last in
                          Hashtbl.remove symtbl v;
                          C.mv addr2 addr1;
                          C.set_sp addr1;
                          last := addr1

    | Identifier v     -> let addr1 = Hashtbl.find symtbl v in
                          let addr2 = C.inc_sp () in
                          C.mv addr1 addr2;
                          last := addr2

    | BinaryOp (o,e1,e2) -> let addr1 = compile e1; !last  in
                          let addr2 = compile e2; !last in
                          C.op o addr1 addr2;
                          C.set_sp addr1;
                          C.st addr1;
                          last := addr1

    | Const n          -> let addr = C.inc_sp () in
                          C.ldc n;
                          C.st addr;
                          last := addr

    | Boolean b        -> compile (Const (btoi b)) (* Booleans are just 1 or 0 *)

    | Seq l            -> List.iter compile l

		| Empty            -> ()
end

module Assembler = struct

  module Asmb = struct
    open Int64
    type addr = int64
    let empty_addr = zero

    let sp  = ref minus_one
    let fp  = ref zero
    let acc = ref zero

    let set_sp n  = sp := n
    let inc_sp () = sp := add !sp one; !sp

    let buf       = Buffer.create 1024
    let addstr s  = Buffer.add_string buf (s ^ "\n")

    let ld   ad   = addstr (sprintf "ld   %d" ad)
    let ldc  n    = addstr (sprintf "ldc  %d" n)
    let st   ad   = addstr (sprintf "st   r%Ld" ad)
    let mv   a b  = addstr (sprintf "mv   r%Ld, r%Ld" a b)
    let op o a b  = addstr (sprintf "%s r%Ld, r%Ld" (instr_of_op o |> pad 4) a b)
  end

  module AsmCmp = MakeCompiler(Asmb)

  let compile e = AsmCmp.compile e;
                  Buffer.output_buffer stdout Asmb.buf
end

module Interpreter = struct

  let fn_of_op = function
		| Plus    -> (+)
		| Minus   -> (-)
		| Times   -> ( * )
		| Divide  -> (/)
		| Modulus -> (mod)
		| o -> optoi (match o with
		| Lth     -> (<)
		| Gth     -> (>)
		| Leq     -> (<=)
		| Geq     -> (>=)
		| Equal   -> (=)
		| Noteq   -> (!=)
		| o -> btoifn (match o with
		| And     -> (&&)
		| Or      -> (||)
		| _ -> failwith "fn_of_op"))

  module Interp = struct
    type addr = int
    let empty_addr = 0

    let sp  = ref (-1)
    let fp  = ref 0
    let acc = ref 0

    let set_sp n  = sp := n
    let inc_sp () = sp := !sp + 1; !sp

    let stck = Array.make 32 0
    let acc  = ref 0

		let string_of_stck = sprintf "[%s]" (Array.fold_left (fun s i -> s ^ (string_of_int i) ^ ";") "" stck)

    let ld ad      = acc := Array.get stck ad
    let ldc n      = acc := n
    let st ad      = Array.set stck ad !acc
    let mv a b     = Array.set stck b (Array.get stck a)
    let op o i1 i2 = let v1 = Array.get stck i1 in
                     let v2 = Array.get stck i2 in
										 acc := (fn_of_op o) v1 v2
  end

  module InterpCmp = MakeCompiler(Interp)

  let interpret e = InterpCmp.compile e;
									Array.get Interp.stck !InterpCmp.last
									|> printf "%s\n%d\n" (Interp.string_of_stck)
end
