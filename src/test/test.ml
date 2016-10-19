open Error
open Print
open Printf

(* Code, Output: name, code, expected *)
type test = 
  | Test of string * string 
  | NamedTest of string * string * string
;;
let name_test name test = match test with
  | Test (code, expected) -> NamedTest (name, code, expected)
  | NamedTest _   -> test
;;

let code = function
  | Test (code, _)         -> code
  | NamedTest (_, code, _) -> code
and expected = function
  | Test (_, exp)         -> exp
  | NamedTest (_, _, exp) -> exp
and name = function
  | NamedTest (name, _, _) -> name
  | _ -> failwith "Test has no name"
;;

(* Exception thrown by test: type, line-num, col-num *)
type test_ret = 
  | Exception of exn
  | CompileErr of error * int * int
  | Program of Types.fundef list
;;
let err_from_buf typ buf = CompileErr (typ, line_of_buf buf, col_of_buf buf)

(* Indents lines by n spaces *)
let indent count =
  let spaces = Bytes.to_string (Bytes.make count ' ') in
  Str.global_replace (Str.regexp "\n") ("\n" ^ spaces)
;;

(* Generating and running the test *)
let rec test_passed buf act exp = match act, exp with
  | Program _,    Program _
  | CompileErr _, CompileErr _ -> act = exp
  | Exception e,  _            -> (match e with
                                    | CompileError (typ, _) -> test_passed buf (err_from_buf typ buf) exp
                                    | _ -> false)
  | _ -> false
;;

let run_test name code expected = 
  let buf = Lexing.from_string code in
  let actual = try Program (Parser.init Lexer.read buf)
               with err -> Exception err in
  
  if (test_passed buf actual expected) then
    exit 0
  else
    (match actual with
      | Program prog  -> print_string (prog |> List.map string_of_func |> String.concat "\n")
      | Exception err -> print_trace err name buf
      | CompileErr (e, _, _) -> let err = CompileError (e, Some (fun _ -> "")) in
                                print_trace err name buf);
    exit 1
;;

(* Generate OCaml code for a test *)
let rec generate_test = function
  | NamedTest (name, code, expected) -> sprintf "open Test\nopen Types\nopen Error\nopen Print\n\nlet _ =\n  let code = \"%s\" in\n  let expected = %s in\n  run_test \"%s\" code expected" (String.escaped code) (indent 2 (String.trim expected)) name
  | Test (code, exp) -> generate_test (NamedTest ("%inline%", code, exp))
;;

