type error =
  | Syntax
  | Parse

exception CompileError of error * (Lexing.lexbuf -> string)

let error_of_fn typ fn = CompileError (typ, fn)
let error_of    typ s  = CompileError (typ, (fun _ -> s))
let error_empty tru fal = function
  | None -> raise fal
  | _    -> tru
;;

let error_no_semicolon = error_of Parse "Unexpected end of line. Semicolon expected"
let error_no_rbrace = error_of Parse "Unexpected token. } expected"

