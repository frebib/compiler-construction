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
  | "//"        { line_comment read lexbuf }
  | "/*"        { comment read lexbuf }
  | "/%"        { read lexbuf }
  | close_nl    { code "" lexbuf }
  | "CODE"      { CODE }
  | "TREE"      { TREE }
  | "RESULT"    { RESULT }
  | "END"       { END }
  | eof         { EOF }
  | _           { raise lexer_error }

and code str = parse
  | eof         { EOF }
  | "/%"        { LINE str }
  | "//"        { line_comment (code str) lexbuf }
  | "/*"        { comment (code str) lexbuf }
  | newline     { new_line lexbuf; code (str ^ "\n") lexbuf }
  | _           { code (str ^ lexeme lexbuf) lexbuf }

and comment ret = parse
  | eof         { EOF }
  | newline     { new_line lexbuf; comment ret lexbuf }
  | "*/"        { ret lexbuf }
  | _           { comment ret lexbuf }

and line_comment ret = parse
  | newline     { new_line lexbuf; ret lexbuf }
  | _           { line_comment ret lexbuf }

