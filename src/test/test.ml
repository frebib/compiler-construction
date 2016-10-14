open Printf

(* Code, Output *)
type test = string * string;;

let generate_test = function
| (code, expected) -> sprintf "open Types\n\nlet _ =\n  let code = \"%s\" in\n  let expected = %s in\n  let actual = Parse.parse \"%%inline%%\" (Lexing.from_string code) in\n  print_string (string_of_bool (actual = expected)); print_newline (); Parse.parse_print \"\" (Lexing.from_string code);  print_newline "(String.escaped code) (String.trim expected)
;;

