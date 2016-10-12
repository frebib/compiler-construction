%{ 
    open Types
%}

(* Misc *)
%token          EOF
%token <string> COMMENT
%token <string> STRING
%token <int>    INT

(* Punctuation *)
%token          PERIOD COMMA COLON SEMICOLON TILDE
%token          LBRACE RBRACE LPAREN RPAREN
%token          ADD SUB MUL DIV EQUAL
%token          DBLEQUAL NOTEQUAL LTHAN GTHAN LEQUAL GEQUAL

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

    (* Both positive and negative numbers *)
    | SUB i = INT                       { Const (-i) }
    | i = INT                           { Const i }

    (* Maths operations (not caring about types here)*)
    | e1 = exp ADD e2 = exp             { Operator (Plus, e1, e2) }
    | e1 = exp SUB e2 = exp             { Operator (Minus, e1, e2) }
    | e1 = exp MUL e2 = exp             { Operator (Times, e1, e2) }
    | e1 = exp DIV e2 = exp             { Operator (Divide, e1, e2) }

    | e1 = exp DBLEQUAL e2 = exp        { Operator (Equal, e1, e2) }
    | e1 = exp SUB e2 = exp             { Operator (Minus, e1, e2) }
    | e1 = exp MUL e2 = exp             { Operator (Times, e1, e2) }
    | e1 = exp DIV e2 = exp             { Operator (Divide, e1, e2) }

    (* Assigment & Declaration *)
    | asg = assign                      { asg }
    | LET var = STRING EQUAL e = exp;
        ine = exp                       { Let (var, e, ine) }

assign:
    | var = ident EQUAL e = exp         { Asg (var, e) }

ident:
    | s = STRING                        { Identifier s }

