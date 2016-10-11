%{ 
    open Types
%}

(* Misc *)
%token          EOF
%token <string> COMMENT
%token <string> STRING
%token <int>    INT

(* Punctuation *)
%token          PERIOD COMMA COLON SEMICOLON
%token          LBRACE RBRACE LPAREN RPAREN
%token          EQUAL LTHAN GTHAN LEQUAL GEQUAL

(* Keywords *)
%token          FUNCTION RETURN
%token          IF ELSE
%token          LET VAR NULL
%token          WHILE FOR DO

%start <Types.program> init
%%

init:
    | ss = func* EOF                    { ss }

func:
    | FUNCTION name = STRING;
        args = sparams; ss = body       { Func (name, args, ss) }

params: 
    | LPAREN RPAREN                     { Empty }
    | LPAREN es = exp+ RPAREN           { flatten_exp es }

sparams: 
    | LPAREN ps = STRING* RPAREN        { ps }

body:
    | LBRACE ss = exp* RBRACE           { flatten_exp ss }

exp:
    | i = ident                         { i }
    | WHILE p = params; ss = body       { While (p, ss) }
    | FOR LPAREN  asg = exp;
        SEMICOLON cmp = exp;
        SEMICOLON inc = exp;
        RPAREN; es = body               { For (asg, cmp, inc, es) }
    (* Lookahead avoids shift/reduce conflict here *)
    | IF e = params; es = body          { If (e, es, Empty) }
    | IF e = params; ib = body      
        ELSE eb = body                  { If (e, ib, eb) }
    | var = ident EQUAL e = exp         { Asg (var, e) }
    | i = INT                           { Const i }

ident:
    | s = STRING                        { Identifier s }

