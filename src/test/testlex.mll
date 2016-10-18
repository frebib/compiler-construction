{ 
  open Error
  open Lexing
  open Testpar
}

let whitespace = [' ' '\t']+
let newline    = '\n' | '\r' | "\r\n"
let close_nl   = "%/" newline?

rule read = parse
  | whitespace  { read lexbuf }
  | newline     { new_line lexbuf; read lexbuf }
  | "/%"        { read lexbuf }
  | close_nl    { code "" lexbuf }
  | "TEST"      { TEST }
  | "OUTPUT"    { OUTPUT }
  | "END"       { END }
  | eof         { EOF }
  | _           { raise lexer_error }

and code str = parse
  | eof         { EOF }
  | "/%"        { CODE str }
  | newline     { new_line lexbuf; code (str ^ "\n") lexbuf }
  | _           { code (str ^ lexeme lexbuf) lexbuf }

