open Error
open Print
open Printf

(* Code, Output: name, code, tree *)
type test = 
  | Test of string * string * string option
  | NamedTest of string * string * string * string option
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
  | EvalErr of eval_err
  | Program of Types.expression
;;
let err_from_buf typ buf = CompileErr (typ, line_of_buf buf, col_of_buf buf)
let prog_from_ret = function
  | Program p -> p
  | _ -> failwith "Cannot get program of an exceptional test return value"

type test_res = 
  | Value of Types.expression
  | EvalErr of eval_err
  | Exception of exn
;;
let value_from_res = function
  | Value e -> e
  | _ -> failwith "Cannot get value of an exceptional test program evaluation"

(* Generating and running the test *)
let rec test_passed buf act exp = match act, exp with
  | Program _,    Program _
  | EvalErr _,    EvalErr _
  | CompileErr _, CompileErr _ -> act = exp
  | Exception e,  _            -> (match e with
                                    | CompileError (typ, _) -> test_passed buf (err_from_buf typ buf) exp
                                    | _ -> false)
  | _ -> false
;;
let rec eval_success out res = match out, res with
  | Value _, Value _
  | EvalErr _, EvalErr _ -> out = res
  | Exception e, _       -> (match e with
                              | EvaluationError typ -> eval_success (EvalErr typ) res
                              | _ -> false)
  | _ -> false
;;

let run_test name code tree result = 
  let buf = Lexing.from_string code in
  let actual = try Program (Parser.init Lexer.read buf)
               with err -> Exception err in
  
  if (test_passed buf actual tree) then
    if Option.absent result then
      exit 0
    else
      let result = Option.get result in
      let output = try Value (Eval.eval (prog_from_ret actual))
                   with err -> Exception err in
      if eval_success output result then
        exit 0
      else (match output with
        | Value e -> let out_str = string_of_exp e |> indent 2 |> sprintf "[\n%s\n]" |> indent 2 in
                     eprintf ":: Evaluation failed.\n  :: Output:\n%s\n" out_str; exit 3
        | EvalErr typ   -> print_trace (EvaluationError typ) name buf
        | Exception err -> print_trace err name buf)
  else
    (match actual with
      | Program prog  -> print_string (string_of_exp prog)
      | Exception err -> print_trace err name buf
      | EvalErr typ   -> print_trace (EvaluationError typ) name buf
      | CompileErr (e, _, _) -> let err = CompileError (e, Some (fun _ -> "")) in
                                print_trace err name buf);
    exit 2
;;

(* Generate OCaml code for a test *)
let rec generate_test = function
  | NamedTest (name, code, tree, result) -> sprintf "open Test\nopen Types\nopen Error\n\n\nlet _ =\n  let code = \"%s\" in\n  let tree = %s in\n  let result = %s in\n  run_test \"%s\" code tree result" (String.escaped code) (String.trim tree |> indent 2 ) (Option.map_def (fun o -> String.trim o |> sprintf "Some (%s)") "None" result) name
  | test -> generate_test (name_test "%inline%" test)
;;

