open Types
open Printf

let sp = ref 0
let lblid = ref 0
let mklbl = sprintf "_%d"

let code = Buffer.create 1024
let add_instr s = Buffer.add_string code ("\t" ^ s ^ "\n")
let add_label i = let label = mklbl i in
                  Buffer.add_string code (label ^ ":\n")
let new_lblid _ = lblid := !lblid + 1; !lblid

let rec compile symtbl = function
  | Let (v, e1, e2)    -> compile symtbl e1;
                          Hashtbl.replace symtbl v !sp;
                          compile symtbl e2;
                          Hashtbl.remove symtbl v;
                          add_instr "popq	%rax";
                          add_instr "popq	%rbx";
                          add_instr "pushq	%rax";
                          sp := !sp - 1

  | Identifier v       -> let addr = Hashtbl.find symtbl v in
                          add_instr (sprintf "movq	%d(%%rbp), %%rax" (-16 - 8 * addr));
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
                                        "setz	%al"        |> add_instr
                            | Noteq  -> "cmp	%rbx, %rax" |> add_instr;
                                        "setnz	%al"      |> add_instr
                            | o -> (match o with
                            | Plus  -> "addq	%rbx, %rax"
                            | Minus -> "subq	%rbx, %rax"
                            | Times -> "imulq	%rbx, %rax")
                              |> add_instr);
                          add_instr "pushq	%rax";
                          sp := !sp - 1

  | Seq (hd::tl)       -> compile symtbl hd;
                          add_instr "popq	%rax"; (* Dispose of value *)
                          compile symtbl (Seq tl)

	| If (g, a, b)       -> Buffer.add_string code "// Begin if\n";
                          let elsjmp = new_lblid () in
                          let endjmp = new_lblid () in
                          compile symtbl g;
                          add_instr "popq	%rax";
                          add_instr "test	%rax, %rax";
                          add_instr ("je	" ^ (mklbl elsjmp));
                          sp := !sp - 1;
                          Buffer.add_string code "// true\n";
                          compile symtbl a;
                          add_instr ("jmp	" ^ (mklbl endjmp));
                          Buffer.add_string code "// false\n";
                          add_label elsjmp;
                          compile symtbl b;
                          Buffer.add_string code "// End if\n";
                          add_label endjmp;
;;

let templ_prefix = 
".LC0:
	.string	\"%d\\n\"
	.text
	.globl	printInt
	.type	printInt, @function
printInt:
.LFB2:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	movl	%eax, %esi
	movl	$.LC0, %edi
	movl	$0, %eax
	call	printf
	movl	$0, %edi
	call	exit
	.cfi_endproc
.LFE2:
	.size	printInt, .-printInt
	.globl	main
	.type	main, @function
main:
.LFB3:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	// End template code\n"
let templ_suffix = 
"	// End of program code
	// Print and exit
	popq	%rdi
	call	printInt
	movl	$1, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE3:
	.size	main, .-main
	.ident	\"GCC: (GNU) 6.2.1 20160830\"
	.section	.note.GNU-stack,\"\",@progbits\n"


let assemble e = 
  Buffer.reset code;
  Buffer.add_string code templ_prefix;
  compile (Hashtbl.create 1024) e;
  Buffer.add_string code templ_suffix;
  print_endline (Buffer.contents code)
