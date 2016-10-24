open Error
open Print
open Printf

(* Code, Output: name, code, tree *)
type test = 
  | Test of string * string * string
  | NamedTest of string * string * string * string
;;
let name_test name test = match test with
  | Test (code, tree, result) -> NamedTest (name, code, tree, result)
  | NamedTest _   -> test
;;

let code = function
  | Test (code, _, _)         -> code
  | NamedTest (_, code, _, _) -> code
and tree = function
  | Test (_, exp, _)         -> exp
  | NamedTest (_, _, exp, _) -> exp
and name = function
  | NamedTest (name, _, _, _) -> name
  | _ -> failwith "Test has no name"
;;

(* Exception thrown by test: type, line-num, col-num *)
type test_ret = 
  | Exception of exn
  | CompileErr of error * int * int
  | Program of Types.expression
;;
let err_from_buf typ buf = CompileErr (typ, line_of_buf buf, col_of_buf buf)
let prog_from_ret = function
  | Program p -> p
  | _ -> failwith "Cannot get program of an exceptional test return value"

(* Generating and running the test *)
let rec test_passed buf act exp = match act, exp with
  | Program _,    Program _
  | CompileErr _, CompileErr _ -> act = exp
  | Exception e,  _            -> (match e with
                                    | CompileError (typ, _) -> test_passed buf (err_from_buf typ buf) exp
                                    | _ -> false)
  | _ -> false
;;

let run_test name code tree result = 
  let buf = Lexing.from_string code in
  let actual = try Program (Parser.init Lexer.read buf)
               with err -> Exception err in
  
  if (test_passed buf actual tree) then
    try
      let output = Eval.eval (prog_from_ret actual) in
      if output = result then
        exit 0
      else 
        let out_str = string_of_exp output |> indent 2 |> sprintf "[\n%s\n]" in
        eprintf ":: Evaluation failed.\n  :: Output:\n%s\n" (indent 2 out_str); exit 1
    with err -> match err with
      | CompileError (Unimplemented, _) -> eprintf ":: %s\n" (error_message buf err); exit 10
      | CompileError (Eval, _) -> eprintf ":: %s\n" (error_message buf err); exit 3
      | _ -> raise err

  else
    (match actual with
      | Program prog  -> print_string (string_of_exp prog)
      | Exception err -> print_trace err name buf
      | CompileErr (e, _, _) -> let err = CompileError (e, Some (fun _ -> "")) in
                                print_trace err name buf);
    exit 2
;;

(* Generate OCaml code for a test *)
let rec generate_test = function
  | NamedTest (name, code, tree, result) -> sprintf "open Test\nopen Types\nopen Error\nopen Print\n\nlet _ =\n  let code = \"%s\" in\n  let tree = %s in\n  let result = %s in\n  run_test \"%s\" code tree result" (String.escaped code) (indent 2 (String.trim tree)) (indent 2 (String.trim result)) name
  | test -> generate_test (name_test "%inline%" test)
;;

