open Printf

(* Code, Output *)
type test = string * string;;

let indent count =
  let spaces = Bytes.to_string (Bytes.make count ' ') in
  Str.global_replace (Str.regexp "\n") ("\n" ^ spaces)
;;

let generate_test = function
| (code, expected) -> sprintf "open Types\n\nlet _ =\n  let code = \"%s\" in\n  let expected = %s in\n  let actual = Parse.parse \"%%inline%%\" (Lexing.from_string code) in\n  print_string (if actual = expected then \"PASS!\" else \"FAIL\"); print_newline (); Parse.parse_print \"\" (Lexing.from_string code);  print_newline " (String.escaped code) (indent 2 (String.trim expected))
;;

