open Types
open Printf

type reg64 = 
  | RAX | RBX | RCX | RDX
  | RSI | RDI | RBP | RSP
  | R8  | R9  | R10 | R11
  | R12 | R13 | R14 | R15
type location =
  | Register of reg64
  | Stack of int
  | Const of int
  | Void

let sp = ref 0
let regs = ref []
let lblid = ref 0
let mklbl = sprintf ".LBL%d"

let code = ref (Buffer.create 1048576) (* 1024 ^ 2 *)
let funs = Buffer.create 1048576 (* 1024 ^ 2 *)
let add_instr s = Buffer.add_string !code ("\t" ^ s ^ "\n")
let add_label i = Buffer.add_string !code (mklbl i ^ ":\n")
let new_lblid _ = lblid := !lblid + 1; !lblid

let all_reg = [
  RAX; RBX; RCX; RDX;
  RSI; RDI; RBP; RSP;
  R8;  R9;  R10; R11;
  R12; R13; R14; R15;
]

let string_of_reg64 = function e -> "%" ^ 
  (e |> function
    | RAX -> "rax" | RBX -> "rbx" | RCX -> "rcx" | RDX -> "rdx"
    | RSI -> "rax" | RDI -> "rdi" | RBP -> "rbp" | RSP -> "rsp"
    | R8 -> "r8"   | R9 -> "r9"   | R10 -> "r10" | R11 -> "r11"
    | R12 -> "r12" | R13 -> "r13" | R14 -> "r14" | R15 -> "r15")
let string_of_location = function
  | Register v -> string_of_reg64 v
  | Stack o    -> sprintf "%d(%%rbp)" o   
  | Const i    -> sprintf "$%d" i
  | Void       -> "<void>"

let arg_reg = function
	| 0 -> RDI
	| 1 -> RSI
	| 2 -> RDX
	| 3 -> RCX
	| 4 -> R8
	| 5 -> R9
	| _ -> failwith "arg_reg"

let movto s = function
  | Register r -> sprintf "movq	%s, %s" s (string_of_reg64 r) |> add_instr
  | Stack offs -> sprintf "movq	%s, %d(%%rbp)" s offs |> add_instr;
                  sp := !sp + 1
  | Void -> ()

let next_reg symtbl =
  let unused =
    Hashtbl.fold (fun k v ac -> match v with
      | Register v -> if List.mem v ac then ac else v :: ac
      | _ -> ac) symtbl !regs
    |> fun l -> List.filter (fun x -> not (List.mem x l)) all_reg
  in
  printf "\n// ["; List.iter (printf "%s; ") (List.map string_of_reg64 unused); printf "]\n";
  if List.length unused < 1
  then Stack !sp
  else
    let reg = List.hd unused in
    regs := (reg :: !regs);
    Register reg

let free_reg = function
  | Register r -> regs := (List.filter (fun x -> r != x) !regs)
  | _ -> ()

let rec compile symtbl = function
  | Let (n, Function (a, b), i)
                       -> let funbuf = Buffer.create 1024 in
                          let sp_temp = !sp in sp := 0;
                          let code_temp = !code in code := funbuf;
                          let add_finstr s = Buffer.add_string funbuf ("\t" ^ s ^ "\n") in
                          let add_flabel s = Buffer.add_string funbuf (s ^ ":\n") in
                          add_finstr (".globl	" ^ n);
                          add_finstr (".type	" ^ n ^ ", @function");
                          add_flabel n; (* Add function name as label *)
                          add_finstr "pushq	%rbp";
                          add_finstr "movq	%rsp, %rbp";

                          let fsymtbl = Hashtbl.copy symtbl in
                          List.iteri (fun i a ->
                            Hashtbl.replace fsymtbl a 
                              (if i < 6 then
                                (add_instr ("pushq	" ^ (arg_reg i |> string_of_reg64));
                                sp := !sp + 1;
                                Stack (!sp * -8))
                              else
                                Stack ((i - 4) * 8))
                          ) a;
                          compile fsymtbl b;
                          sp := !sp - 1;

                          add_finstr "popq	%rax";
                          add_finstr "leave";
                          add_finstr "ret";
                          add_finstr (".size	" ^ n ^ ", .-" ^ n);

                          Buffer.add_buffer funs funbuf;

                          code := code_temp;
                          sp := sp_temp;

                          compile symtbl i

  | Let (v, e1, e2)    -> compile symtbl e1;
                          Hashtbl.replace symtbl v (Stack (!sp * -8));
                          compile symtbl e2;
                          Hashtbl.remove symtbl v;
                          add_instr "popq	%rax";
                          add_instr "popq	%rbx";
                          add_instr "pushq	%rax";
                          sp := !sp - 1

  | New (v, e1, e2)    -> compile symtbl e1;
                          add_instr (sprintf "leaq	%d(%%rbp), %%rax" (-8 * !sp));
                          add_instr "pushq	%rax";
                          sp := !sp + 1;
                          Hashtbl.replace symtbl v (Stack (!sp * -8));
                          compile symtbl e2;
                          Hashtbl.remove symtbl v;
                          add_instr "popq	%rax";
                          add_instr "addq	$8, %rsp"; (* Dispose of unused value *)
                          add_instr "popq	%rbx";
                          add_instr "pushq	%rax";
                          sp := !sp - 2

  | Deref e            -> compile symtbl e;
                          add_instr "popq	%rax";
                          add_instr "movq	(%rax), %rbx";
                          add_instr "pushq	%rbx"

  | Asg (v, e)         -> compile symtbl v;
                          compile symtbl e;
                          add_instr "popq	%rax";
                          add_instr "popq	%rbx";
                          add_instr "movq	%rax, (%rbx)";
                          add_instr "pushq	%rax";
                          sp := !sp - 1;

  | Identifier v       -> add_instr ("// Identifier " ^ v);
                          (match Hashtbl.find symtbl v with
                            | Register RAX -> ()
                            | Register r -> add_instr (sprintf "movq	%s, %%rax" (string_of_reg64 r))
                            | Stack offs -> add_instr (sprintf "movq	%d(%%rbp), %%rax" offs)
                          );
                          add_instr ("pushq	%rax");
                          sp := !sp + 1

  | Boolean b          -> compile symtbl (Const (if b then 1 else 0))
  | Const n            -> add_instr (sprintf "pushq	$%d" n);
                          sp := !sp + 1

  | BinaryOp (o, a, b) -> compile symtbl a;
                          compile symtbl b;
                          add_instr "popq	%rbx";
                          add_instr "popq	%rax";
                          (match o with
                            | Divide -> "xor	%rdx, %rdx" |> add_instr;
                                        "idivq	%rbx"     |> add_instr
                            | Equal  -> "cmp	%rbx, %rax" |> add_instr;
                                        "setz	%al"        |> add_instr;
                                        "movsbq	%al, %rax"|> add_instr
                            | Noteq  -> "cmp	%rbx, %rax" |> add_instr;
                                        "setnz	%al"      |> add_instr;
                                        "movsbq	%al, %rax"|> add_instr
                            | Lth    -> "cmp	%rbx, %rax" |> add_instr;
                                        "setl	%al"        |> add_instr;
                                        "movsbq	%al, %rax"|> add_instr
                            | Gth    -> "cmp	%rbx, %rax" |> add_instr;
                                        "setg	%al"        |> add_instr;
                                        "movsbq	%al, %rax"|> add_instr
                            | Leq    -> "cmp	%rbx, %rax" |> add_instr;
                                        "setle	%al"      |> add_instr;
                                        "movsbq	%al, %rax"|> add_instr
                            | Geq    -> "cmp	%rbx, %rax" |> add_instr;
                                        "setge	%al"      |> add_instr;
                                        "movsbq	%al, %rax"|> add_instr
                            | o -> (match o with
                            | Plus  -> "addq	%rbx, %rax"
                            | Minus -> "subq	%rbx, %rax"
                            | Times -> "imulq	%rbx, %rax"
                            | _ -> failwith (sprintf "BinaryOp(%s, ..)" (Print.string_of_op o)))
                              |> add_instr);
                          add_instr "pushq	%rax";
                          sp := !sp - 1

  | Readint            -> compile symtbl (Application (Identifier "readInt",  []))
  | Printint e         -> compile symtbl (Application (Identifier "printInt", [e]))

  | Application (e, a) -> (* Reverse args for cdecl convention *)
                          let arg_count = List.length a in
                          List.rev a |> List.iteri (fun i e ->
                            let i = arg_count - i - 1 in (* Add arguments in correct order *)
                            compile symtbl e;
                            if i < 6 then
                              add_instr ("popq	" ^ (arg_reg i |> string_of_reg64));
                              sp := !sp - 1
                          ); 

                          (match e with
                            | Identifier v -> add_instr ("call	" ^ v)
                            | _ -> failwith (sprintf "Application (%s, ..)" (Print.string_of_exp e))
                          );
                          
                          (* Remove all stack arguments *)
                          if arg_count > 6 then
                            add_instr (sprintf "addq	$%d, %%rsp" ((arg_count - 6) * 8));

                          add_instr "pushq	%rax";
                          sp := !sp + 1

  | Seq (hd::[])       -> compile symtbl hd      (* Keep last exp in Seq *)
  | Seq (hd::tl)       -> compile symtbl hd;
                          add_instr "popq	%rax"; (* Dispose of value *)
                          sp := !sp - 1;
                          compile symtbl (Seq tl)
  | Seq []             -> ()

  | If (g, a, b)       -> add_instr "// Begin if";
                          let elsjmp = new_lblid () in
                          let endjmp = new_lblid () in
                          compile symtbl g;
                          add_instr "popq	%rax";
                          add_instr "test	%rax, %rax";
                          add_instr ("je	" ^ (mklbl elsjmp));
                          sp := !sp - 1;
                          add_instr "// true";
                          compile symtbl a;
                          sp := !sp - 1;
                          add_instr ("jmp	" ^ (mklbl endjmp));
                          add_instr "// false";
                          add_label elsjmp;
                          compile symtbl b;
                          add_instr "// End if";
                          add_label endjmp

  | While (g, e)       -> add_instr "// Begin while";
                          let base_sp = !sp in
                          let loopjmp = new_lblid () in
                          let endjmp = new_lblid () in
                          add_label loopjmp;
                          compile symtbl g;
                          add_instr "popq	%rax";
                          add_instr "test	%rax, %rax";
                          add_instr ("je	" ^ (mklbl endjmp));
                          sp := !sp - 1;
                          compile symtbl e;
                          add_instr "addq	$8, %rsp"; (* Dispose of unused value *)
                          sp := base_sp;
                          add_instr ("jmp	" ^ (mklbl loopjmp));
                          add_label endjmp;
                          compile symtbl Empty;
                          add_instr "// End while"

  | Empty              -> add_instr "pushq	$0";
                          sp := !sp + 1
;;

let templ_printInt = 
".LC0:
	.string	\"%d\\n\"
	.text
	.globl	printInt
	.type	printInt, @function
printInt:
	subq	$8, %rsp
	movl	%edi, %esi
	xorl	%eax, %eax
	movl	$.LC0, %edi
	call	printf
	xorl	%eax, %eax
	addq	$8, %rsp
	ret
	.size	printInt, .-printInt\n"

let templ_readInt = 
".LC1:
	.string	\"%d+\\n\"
	.globl	readInt
	.type	readInt, @function
readInt:
	subq	$24, %rsp
	movl	$.LC1, %edi
	xorl	%eax, %eax
	leaq	12(%rsp), %rsi
	call	__isoc99_scanf
	movl	12(%rsp), %eax
	addq	$24, %rsp
	ret
	.size	readInt, .-readInt\n"

let main_prefix = 
"	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
	// End template code\n"
let main_suffix = 
"	// End of program code
	// Print and exit
	popq	%rdi
	call	printInt
	movl	$0, %eax
	leave
	ret
	.size	main, .-main\n"


let assemble e = 
  Buffer.reset funs;
  Buffer.add_string funs templ_printInt;
  Buffer.add_string funs templ_readInt;
  Buffer.reset !code;
  Buffer.add_string !code main_prefix;
  compile (Hashtbl.create 1024) e;
  Buffer.add_string !code main_suffix;
  Buffer.add_buffer funs !code;
  print_endline (Buffer.contents funs)
