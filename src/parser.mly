%{ 
    open Types
    open Error
%}

(* Misc *)
%token          EOF
%token <string> STRING
%token <int>    INT
%token <bool>   BOOL

(* Punctuation *)
%token          COMMA SEMICOLON
%token          LBRACE RBRACE LPAREN RPAREN
%token          ADD SUB MUL DIV EQUAL
%token          DBLEQUAL NOTEQUAL LTHAN GTHAN LEQUAL GEQUAL

(* Keywords *)
%token          FUNCTION RETURN
%token          IF ELSE WHILE
%token          LET VAR

%nonassoc DBLEQUAL NOTEQUAL
%nonassoc LTHAN GTHAN LEQUAL GEQUAL
%right EQUAL
%left ADD SUB
%left MUL DIV
%left RETURN

%start <Types.program> init
%%

init:
    | ss = func* EOF                    { ss }

func:
    | FUNCTION name = STRING args = comma_str_params; ss = body { (name, args, ss) }

body:
    (* A brace delimited body or a single statement*)
    | s = statement { s }
    | LBRACE ss = statement* d = def? RBRACE { flatten_exp (opt_prepend ss d) }

statement:
    (* A complete statement; a brace block or semicolon terminated expression*)
    | e = blockexp { e }
    | e = exp SEMICOLON { e }

blockexp:
    (* These block-level expressions usually contain {  } 
     * and specifically don't terminate with a semicolon *)
    | IF e = exp_param ib = body ELSE eb = body { If (e, ib, eb) }
    | WHILE p = exp_param ss = body             { While (p, ss) }

exp:
    (* Expressions are anything that can appear inline within a statement *)
    | LPAREN e = exp RPAREN             { e }
    | LPAREN e = blockexp RPAREN        { e }

    | c = const                         { c }
    | i = ident                         { Deref i }
    | i = ident; ps = comma_exp_params  { Application (i, ps) }

    | e = exp EQUAL v = exp             { Asg (e, v) }
    | e1 = exp; op = binop; e2 = exp    { Operator (op, e1, e2) }

    | RETURN e = exp                    { Return e }

def: 
    | VAR var = STRING EQUAL e = exp SEMICOLON i = defin { New (var, e, i) }
    | LET var = STRING EQUAL e = exp SEMICOLON i = defin { Let (var, e, i) }

defin:
    | d = def { d }
    | ss = statement* { flatten_exp ss }

%inline ident:
    | s = STRING { Identifier s }

%inline const:
    | b = BOOL  { Boolean b }
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

(* Expression or string parameters enclosed by (parenthesis) *)
exp_param: 
    | LPAREN e = exp RPAREN { e }
comma_exp_params: 
    | LPAREN es = separated_list(COMMA, exp) RPAREN { flatten_exp es }
comma_str_params: 
    | LPAREN ps = separated_list(COMMA, STRING) RPAREN { ps }

