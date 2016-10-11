{
  open Lexing
  open Parser
  exception SyntaxError of string
}

(* Misc *)
let whitespace = [' ' '\t']+
let newline    = '\n' | '\r' | "\r\n"
(* Identifiers can't start with a number *)
let identifier = ['a'-'z' 'A'-'Z' '_' '~' '$'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '~' '$']+

rule read = parse
  | whitespace { read lexbuf }
  | newline    { new_line lexbuf; read lexbuf }
  | "//"       { read lexbuf (*line_comment "" lexbuf*) }

  | '.'        { PERIOD }
  | ','        { COMMA }
  | ':'        { COLON }
  | ';'        { SEMICOLON }
  | '{'        { LBRACE }
  | '}'        { RBRACE }
  | '('        { LPAREN }
  | ')'        { RPAREN }
  | '='        { EQUAL }
  | '<'        { GTHAN }
  | '>'        { LTHAN }
  | "<="       { LEQUAL }
  | ">="       { GEQUAL }

  | "function" { FUNCTION }
  | "return"   { RETURN }
  | "let"      { LET }
  | "var"      { VAR }
  | "null"     { NULL }
  | "while"    { WHILE }
  | "for"      { FOR }
  | "do"       { DO }
  | "if"       { IF }
  | "else"     { ELSE }

  | identifier { STRING (lexeme lexbuf) }
  | eof        { EOF }
  | _          { raise (SyntaxError ("Unexpected token: " ^ lexeme lexbuf)) }

and line_comment buf = parse
  | newline    { COMMENT buf }
  | _          { line_comment (buf ^ lexeme lexbuf) lexbuf }
