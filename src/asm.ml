open Int64
open Array
open Types
open Print
open Printf

type cmp_typ = Ideal | X86_64

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


type 'addr instr =
  | Ld of 'addr | Ldc of int
  | St of 'addr | Mv  of 'addr * 'addr
  | Jz of 'addr | Jmp of 'addr
  | Op of opcode * 'addr * 'addr
  | Nop

let string_of_instr = function
  | Ld  ad     -> "ld",  sprintf "r%Ld" ad
  | Ldc n      -> "ldc", string_of_int n
  | St  ad     -> "st",  sprintf "r%Ld" ad
  | Jz  ad     -> "jz",  sprintf "l%Ld" ad
  | Jmp ad     -> "jmp", sprintf "l%Ld" ad
  | Mv (a,b)   -> "mv",  sprintf "r%Ld,  r%Ld" a b
  | Op (o,a,b) -> instr_of_op o, sprintf "r%Ld,  r%Ld" a b
  | Nop        -> "nop", ""

class virtual ['addr, 'ret] compiler = object (this)

  val virtual sp  : 'addr ref
  val virtual fp  : 'addr ref
  val virtual ip  : 'addr ref
  val virtual acc : 'addr ref
  
  method get_ip () = !ip
  method get_sp () = !sp
  method set_sp v  = sp := v
  method virtual inc_sp :  unit -> 'addr
  method virtual inc_ip :  unit ->  unit

  method ld  a     = Ld a         |> this#instruct
  method ldc n     = Ldc n        |> this#instruct
  method st  a     = St a         |> this#instruct
  method mv  a b   = Mv (a, b)    |> this#instruct
  method op  o a b = Op (o, a, b) |> this#instruct
  method jz  a     = Jz a         |> this#instruct
  method jmp a     = Jmp a        |> this#instruct

  method virtual instruct : 'addr instr -> unit

  method virtual get_buf   : unit -> 'ret
  method virtual comment   : string -> unit
  method virtual get_cmnts : unit -> ('addr, string) Hashtbl.t
end

class asmcompiler = object (this)
  inherit [int64, int64 instr array] compiler

  val buffer = Array.make 256 Nop
  method get_buf () = Array.sub buffer 0 ((to_int !ip) - 1)

  val sp  = ref zero
  val fp  = ref zero
  val ip  = ref zero
  val acc = ref zero

  method inc_sp () = sp := (add !sp one); !sp
  method inc_ip () = ip := (add !ip one)

  method instruct i = Array.set buffer (to_int !ip) i |> this#inc_ip

  val cmnts  = Hashtbl.create 256
  method comment s = Hashtbl.add cmnts !ip s
  method get_cmnts () = cmnts
end

class runcompile (cmp : ('addr, 'ret) compiler) = object (this)

  val symtbl = Hashtbl.create 1024
  val last   = ref (cmp#get_sp ())

  method cmpexp = function
    | Let (v, e1, e2)  -> let addr1 = this#cmpexp e1; !last in
                          cmp#comment ("let " ^ v);
                          Hashtbl.replace symtbl v (cmp#get_sp ());
													let addr2 = this#cmpexp e2; !last in
                          Hashtbl.remove symtbl v;
                          cmp#mv addr2 addr1;
                          cmp#set_sp addr1;
                          last := addr1

    | Identifier v     -> let addr1 = Hashtbl.find symtbl v in
                          let addr2 = cmp#inc_sp () in
                          cmp#mv addr1 addr2;
                          last := addr2

    | BinaryOp (o,e1,e2) -> let addr1 = this#cmpexp e1; !last  in
                          let addr2 = this#cmpexp e2; !last in
                          cmp#op o addr1 addr2;
                          cmp#set_sp addr1;
                          cmp#st addr1;
                          last := addr1

    | Const n          -> let addr = cmp#inc_sp () in
                          cmp#ldc n;
                          cmp#st addr;
                          last := addr

    | Boolean b        -> cmp#comment ("bool " ^ string_of_bool b);
                          this#cmpexp(Const (btoi b)) (* Booleans are just 1 or 0 *)
                          
    | Seq l            -> List.iter this#cmpexp l

    | If (g, a, b)     -> let gval = this#cmpexp g; !last in
                          cmp#ld gval; cmp#comment "Load if gate";
                          cmp#jz gval; cmp#comment "if";
                          let jmp_one = cmp#get_ip () in
                          this#cmpexp a;
                          cmp#jmp jmp_one;
                          cmp#comment "else";
                          let jmp_two = cmp#get_ip () in
                          cmp#inc_ip ();
                          let pre_els = cmp#get_ip () in
                          this#cmpexp b;
                          cmp#comment "outside if";
                          let pst_els = cmp#get_ip () in
                          ()
                          
                          (* Allocate a buffer to save into
                           * whilst the jumps are calculated *)
    | While (g, b)     -> let buf = new asmcompiler in
                          buf#comment "while";
                          this#cmpexp g;
                          buf#ld !last; cmp#comment "Load while cond";
                          buf#jz (buf#get_ip ()); cmp#comment "break loop";
                          let brk_loop = buf#get_ip () in
                          cmp#comment "body";
                          this#cmpexp b |> buf#inc_ip;
                          (* buf#set_jmp brk_loop buf#get_ip; *)
                          buf#comment "end loop";

		| Empty            -> ()

end

(* Functional? What's that? *)
let compile typ e =
  let impl = new asmcompiler in
  let compiler = new runcompile impl in
  let lines = compiler#cmpexp e
    |> impl#get_buf
    |> Array.map string_of_instr
    |> Array.map (fun t -> sprintf "%-4s %s" (fst t) (snd t)) in

  Hashtbl.iter (fun k v ->
    let i = to_int k in
    let s = Array.get lines i in
    Array.set lines i (sprintf "%-16s # %s" s v))
      (impl#get_cmnts ());
      
  Array.iteri (printf "%3d:\t%s\n") lines
;;

let interpret = compile X86_64

module Interpreter = struct

  module Interp = struct
    type addr = int
    let empty_addr () = 0

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

  (* 
  module InterpCmp = MakeCompiler(Interp)

  let interpret e = InterpCmp.compile e;
									Array.get Interp.stck !InterpCmp.last
									|> printf "%s\n%d\n" (Interp.string_of_stck)
  *)
end
