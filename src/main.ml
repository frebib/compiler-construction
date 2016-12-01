open Parse
open Print
open Printf

let usage = sprintf "Parses languages and prints the tree\nUsage: %s <filename>\n" Sys.argv.(0)

type cmd = Parse | Eval | Parseval | Compile | Interpret | Nothing
let cmd_of_string = function
  | "parse" -> Parse
  | "eval"  -> Eval
  | "compile" -> Compile
  | "parseval" -> Parseval
  | "interpret" -> Interpret
  | s -> Nothing
;;

let command  = ref Nothing
let is_test  = ref false
let quiet    = ref false
let optimise = ref false
let files    = ref []

let rec parse_args = function
  | [] -> ()
  | arg :: tail -> (match arg with
    | "parse" | "eval"
    | "compile" | "interpret"
    | "parseval"       -> if !command = Nothing
                          then command := cmd_of_string arg
                          else failwith (sprintf "Action already set: '%s'.\n" arg)
    | "-t" | "--test"  -> is_test := true
    | "-q" | "--quiet" -> quiet := true
    | "-o" | "--optimise" -> optimise := true
    | s                -> if !command != Nothing
                          then files := s :: !files
                          else failwith (sprintf "Unrecognised option: '%s'" s));
    parse_args tail
;;

let parse file = 
  if not !is_test then
    Parse.parse_from file
  else 
    open_in file
    |> Lexing.from_channel
    |> Error.safe Testpar.parse_test Testlex.read file
    |> Test.code
    |> Lexing.from_string
    |> parse file
;;

(* Currently without complex libraries, it's not
 * possible to read a data structure from file -_- *)
let eval file = failwith "Can't evaluate from file just yet."

let parse_optim p = let tree = parse p in 
                       if !optimise then tree |> Optim.optimise_prog else tree

let run_action cmd file = match cmd with
  | Parse    -> let prog = parse_optim file in
                (try
                  if !quiet then ()
                  else prog |> string_of_exp |> printf "%s\n"
                with
                  err -> raise err)

  | Eval     -> eval file
  | Parseval -> let tree = parse_optim file in
                printf "%s\n\n" (string_of_exp tree);
                Eval.eval tree
                |> string_of_exp
                |> printf "%s\n"
                
  | Compile  -> (parse_optim file |> AsmX86.assemble) stdout

  | Interpret -> parse_optim file |> Asm.interpret

  | Nothing  -> eprintf "Nothing to do."; exit 1

let _ =
  if Array.length Sys.argv < 2
  then (print_string usage; exit 1)
  else
    (* Parse all args, minus the executable itself *)
    parse_args (List.tl (Array.to_list Sys.argv));
    (* printf "cmd: %s\ntst: %b\nqut: %b\nfls: %s\n" Sys.argv.(1) !is_test !quiet (String.concat ", " !files |> sprintf "[%s]"); *)
    List.iter (run_action !command) !files
;;
