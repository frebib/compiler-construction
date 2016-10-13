{
  open Types
  open Error
  open Lexing
  open Parser
}

(* Misc *)
let whitespace = [' ' '\t']+
let newline    = '\n' | '\r' | "\r\n"
let number     = ['0'-'9'] ['0'-'9']*
(* Identifiers can't start with a number *)
let identifier = ['a'-'z' 'A'-'Z' '_' '~' '$'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '~' '$']*

rule read = parse
  | whitespace { read lexbuf }
  | newline    { new_line lexbuf; read lexbuf }
  | number     { INT (int_of_string (lexeme lexbuf)) }
  | "//"       { line_comment "" lexbuf }

  | '.'        { PERIOD }
  | ','        { COMMA }
  | ':'        { COLON }
  | ';'        { SEMICOLON }
  | '~'        { TILDE }
  | '{'        { LBRACE }
  | '}'        { RBRACE }
  | '('        { LPAREN }
  | ')'        { RPAREN }
  | '+'        { ADD }
  | '-'        { SUB }
  | '*'        { MUL }
  | '/'        { DIV }
  | "=="       { DBLEQUAL }
  | "!="       { NOTEQUAL }
  | '='        { EQUAL }
  | '<'        { GTHAN }
  | '>'        { LTHAN }
  | "<="       { LEQUAL }
  | ">="       { GEQUAL }

  | "function" { FUNCTION }
  | "return"   { RETURN }
  | "let"      { LET }
  | "var"      { VAR }
  | "while"    { WHILE }
  | "if"       { IF }
  | "else"     { ELSE }

  | identifier { STRING (lexeme lexbuf) }
  | eof        { EOF }
  | _          { raise (error_of_fn Syntax (fun buf -> "Unexpected token: " ^ lexeme buf)) }

and line_comment buf = parse
  | newline    { new_line lexbuf; read lexbuf }
  | _          { line_comment (buf ^ lexeme lexbuf) lexbuf }
