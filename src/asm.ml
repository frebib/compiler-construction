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
  | _ -> failwith "instr_of_op"
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


module type Compiler = sig
  type addr
  val empty_addr : addr

  val sp  : addr ref
  val fp  : addr ref
  val ip  : addr ref
  val acc : addr ref
  
  val set_sp : addr -> unit
  val inc_sp : unit -> addr
  val inc_ip : unit -> unit

  (* Special instruction for updating
   * the address of a jump after-the-fact *)
  val set_jmp : addr -> addr -> unit

  val ld  : addr -> unit
  val ldc : int  -> unit
  val st  : addr -> unit
  val mv  : addr -> addr -> unit
  val op  : opcode -> addr -> addr -> unit
  val jz  : addr -> unit
  val jmp : addr -> unit

  val comment : string -> unit

end

module MakeCompiler (C : Compiler) = struct

  let symtbl = Hashtbl.create 1024
  let last   = ref (C.empty_addr)

  let rec compile = function
    | Let (v, e1, e2)  -> let addr1 = compile e1; !last in
                          C.comment ("let " ^ v);
                          Hashtbl.replace symtbl v !C.sp;
													let addr2 = compile e2; !last in
                          Hashtbl.remove symtbl v;
                          C.mv addr2 addr1 |> C.inc_ip;
                          C.set_sp addr1 |> C.inc_ip;
                          last := addr1

    | Identifier v     -> let addr1 = Hashtbl.find symtbl v in
                          let addr2 = C.inc_sp () in
                          C.mv addr1 addr2 |> C.inc_ip;
                          last := addr2

    | BinaryOp (o,e1,e2) -> let addr1 = compile e1; !last  in
                          let addr2 = compile e2; !last in
                          C.op o addr1 addr2 |> C.inc_ip;
                          C.set_sp addr1;
                          C.st addr1 |> C.inc_ip;
                          last := addr1

    | Const n          -> let addr = C.inc_sp () in
                          C.ldc n |> C.inc_ip;
                          C.st addr |> C.inc_ip;
                          last := addr

    | Boolean b        -> C.comment ("bool " ^ string_of_bool b);
                          compile (Const (btoi b)) (* Booleans are just 1 or 0 *)
                          

    | Seq l            -> List.iter compile l

    | If (g, a, b)     -> let gval = compile g; !last in
                          C.ld gval; C.comment "Load if gate"; C.inc_ip ();
                          C.jz gval; C.comment "if";
                          let jmp_one = !C.ip in
                          C.inc_ip ();
                          compile a;
                          C.jmp jmp_one;
                          C.comment "else";
                          let jmp_two = !C.ip in
                          C.inc_ip ();
                          let pre_els = !C.ip in
                          compile b;
                          C.comment "outside if";
                          let pst_els = !C.ip in
                          C.set_jmp jmp_one pre_els;
                          C.set_jmp jmp_two pst_els

		| Empty            -> ()
end

module Assembler = struct
  open Int64

  module Asmb = struct
    type addr = int64
    let empty_addr = zero

    let sp  = ref minus_one
    let fp  = ref zero
    let ip  = ref zero
    let acc = ref zero

    let set_sp n  = sp := n
    let inc_sp () = sp := add !sp one; !sp
    let inc_ip () = ip := add !ip one

    let cmnts     = Hashtbl.create 256
    let buf       = Array.make 4096 ("", "")
    let addinst t = Array.set buf (to_int !ip) t

    let ld   ad   = addinst ("ld",  sprintf "r%Ld" ad)
    let ldc  n    = addinst ("ldc", string_of_int n)
    let st   ad   = addinst ("st",  sprintf "r%Ld" ad)
    let mv   a  b = addinst ("mv",  sprintf "r%Ld,  r%Ld" a b)
    let op o a  b = addinst (instr_of_op o, sprintf "r%Ld,  r%Ld" a b)
    let jz   ad   = addinst ("jz",  sprintf "r%Ld" ad)
    let jmp  ad   = addinst ("jmp", sprintf "r%Ld" ad)

    let comment s = Hashtbl.add cmnts !ip s

    let set_jmp ip ad = 
      let index = (to_int ip) in
      let instr = fst (Array.get buf index) in
      Array.set buf index (instr, sprintf "r%Ld" ad)

  end

  module AsmCmp = MakeCompiler(Asmb)

  let compile e = AsmCmp.compile e;
                  let str_arr = Array.sub Asmb.buf 0 ((to_int !Asmb.ip) - 1) 
                  |> Array.map (fun t -> sprintf "%-4s %s" (fst t) (snd t)) in
                  Hashtbl.iter (fun k v -> let i = to_int k in
                                           let s = Array.get str_arr i in
                                           Array.set str_arr i (sprintf "%-14s # %s" s v))
                  Asmb.cmnts;
                  Array.iteri (printf "%3.0d  %s\n" ) str_arr
end

module Interpreter = struct

  module Interp = struct
    type addr = int
    let empty_addr = 0

    let sp  = ref (-1)
    let fp  = ref 0
    let ip = ref 0
    let acc = ref 0

    let set_sp n  = sp := n
    let inc_sp () = sp := !sp + 1; !sp
    let inc_ip () = ip := !ip + 1

    let stck = Array.make 4096 0
    let acc  = ref 0

		let string_of_stck = sprintf "[%s]" (Array.fold_left (fun s i -> s ^ (string_of_int i) ^ ";") "" stck)

    let ld   ad   = acc := Array.get stck ad
    let ldc  n    = acc := n
    let st   ad   = Array.set stck ad !acc
    let mv   a  b = Array.set stck b (Array.get stck a)
    let op o a  b = let v1 = Array.get stck a in
                    let v2 = Array.get stck b in
									  acc := (fn_of_op o) v1 v2
    let jz   ad   = () (* Wait for both jumps to be set *)
    let jmp  ad   = ()

    let comment s = () (* Not required for interpretation *)

    let set_jmp ip ad = ()

  end

  module InterpCmp = MakeCompiler(Interp)

  let interpret e = InterpCmp.compile e;
									Array.get Interp.stck !InterpCmp.last
									|> printf "%s\n%d\n" (Interp.string_of_stck)
end
