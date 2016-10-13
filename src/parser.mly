%{ 
    open Types
    open Error
%}

(* Misc *)
%token          EOF
%token <string> STRING
%token <int>    INT

(* Punctuation *)
%token          PERIOD COMMA COLON SEMICOLON TILDE
%token          LBRACE RBRACE LPAREN RPAREN
%token          ADD SUB MUL DIV EQUAL
%token          DBLEQUAL NOTEQUAL LTHAN GTHAN LEQUAL GEQUAL

(* Keywords *)
%token          FUNCTION RETURN
%token          IF ELSE WHILE
%token          LET VAR

%nonassoc DBLEQUAL NOTEQUAL
%nonassoc LTHAN GTHAN LEQUAL GEQUAL
%left COMMA
%right EQUAL
%left ADD SUB
%left MUL DIV
%left RETURN
%right SEMICOLON
%left LPAREN
%right RPAREN
%left LBRACE
%right RBRACE

%start <Types.program> init
%%

init:
    | ss = func* EOF                    { ss }

func:
    | FUNCTION name = STRING args = comma_str_params; ss = body { (name, args, ss) }

body:
    | s = statement                     { s }
    | LBRACE ss = statement* RBRACE     { flatten_exp ss }

statement:
    | e = exp SEMICOLON                 { e }
    | e = blockexp                      { e }

blockexp:
    | IF e = exp_param ib = body
        ELSE eb = body                  { If (e, ib, eb) }
    | WHILE p = exp_param; ss = body    { While (p, ss) }

exp:
    | LPAREN e = exp RPAREN             { e }
    | LPAREN e = blockexp RPAREN        { e }

    | i = ident                         { Deref i }
    | c = const                         { c }
    | i = ident; ps = comma_exp_params  { Application (i, ps) }

    | e1 = exp; op = binop; e2 = exp    { Operator (op, e1, e2) }

    (* Assigment & Declaration *)
    | e = exp EQUAL v = exp             { Asg (e, v) }

    | RETURN e = exp                    { Return e }
    | VAR var = STRING EQUAL e = exp;
         SEMICOLON t = statement*       { New (var, e, flatten_exp t) }
    | LET var = STRING EQUAL e = exp;
        SEMICOLON t = statement*        { Let (var, e, flatten_exp t) }

%inline ident:
    | s = STRING { Identifier s }

%inline const:
    | i = INT   { Const i }

%inline binop:
    | ADD       { Plus }
    | SUB       { Minus }
    | MUL       { Times }
    | DIV       { Divide }

    | NOTEQUAL  { Noteq }
    | DBLEQUAL  { Equal }
    | LTHAN     { Lth }
    | GTHAN     { Gth }
    | LEQUAL    { Leq }
    | GEQUAL    { Geq }

exp_param: 
    | LPAREN e = exp RPAREN { e }
comma_exp_params: 
    | LPAREN es = separated_list(COMMA, exp) RPAREN { flatten_exp es }
comma_str_params: 
    | LPAREN ps = separated_list(COMMA, STRING) RPAREN { ps }

