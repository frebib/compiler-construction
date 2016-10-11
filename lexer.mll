{
  open Parser
  exception SyntaxError of string
}

(* Misc *)
let whitespace = [' ' '\t']+
let newline    = '\n' | '\r' | "\r\n"
let identifier = ['a'-'z' 'A'-'Z' '0'-'9' '_' '~' '$' '%']

rule read = parse
  | whitespace { read lexbuf }
  | newline    { read lexbuf }
  | "//"       { line_comment "" lexbuf }
  | identifier { STRING (Lexing.lexeme lexbuf) }

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

  | eof        { EOF }
  | _          { raise (SyntaxError ("Unexpected: " ^ Lexing.lexeme lexbuf)) }

and line_comment buf = parse
  | newline    { COMMENT buf }
  | _          { line_comment (buf ^ Lexing.lexeme lexbuf) lexbuf }
