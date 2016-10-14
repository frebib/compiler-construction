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
%token          COMMA SEMICOLON EXCLAM AND OR
%token          LBRACE RBRACE LPAREN RPAREN
%token          ADD SUB MUL DIV MOD EQUAL INC DEC
%token          DBLEQUAL NOTEQUAL LTHAN GTHAN LEQUAL GEQUAL

(* Keywords *)
%token          FUNCTION RETURN
%token          IF ELSE WHILE DO
%token          LET VAR

(* http://cs.stmarys.ca/~porter/csc/ref/cpp_operators.html *)
%nonassoc EQUAL
%right    ELSE DO
%left     OR
%left     AND
%right    DBLEQUAL NOTEQUAL
%nonassoc LTHAN GTHAN LEQUAL GEQUAL
%left     ADD SUB
%left     MUL DIV MOD
%right    EXCLAM
%nonassoc INC DEC
%right    RETURN

%start <Types.program> init
%%

init:
    | ss = func* EOF { ss }

func:
    | FUNCTION name = STRING args = comma_str_params; ss = body { (name, args, ss) }

body:
    (* A brace delimited body or a single statement*)
    | s = statement { s }
    | LBRACE ss = statement* d = def? RBRACE { opt_prepend_seq ss d }

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
    | op = pre_unop e = exp             { UnaryOp (op, e) }
    | e = exp op = post_unop            { UnaryOp (op, e) }
    | e1 = exp op = binop e2 = exp      { BinaryOp (op, e1, e2) }

    | RETURN e = exp                    { Return e }

    (* Inline statements are if/while statements without the
     * { } block syntax taking single expression arguments 
     * If >1 expressions are required, use the block-type
     * statements with parenthesis around: (if (x) { .. })
     *)
    | IF e = exp_param ib = exp ELSE eb = exp { If (e, ib, eb) }

    (* Inline while loops differ as they require the 'do' keyword *)
    (* Example: let x = while(true) do x++; *)
    | WHILE p = exp_param DO ss = exp { While (p, ss) }

def: 
    | VAR var = STRING EQUAL e = exp SEMICOLON i = defin { New (var, e, i) }
    | LET var = STRING EQUAL e = exp SEMICOLON i = defin { Let (var, e, i) }

defin:
    | d = def { d }
    | ss = statement* { seq_of_list ss }

%inline ident:
    | s = STRING { Identifier s }

%inline const:
    | b = BOOL  { Boolean b }
    | i = INT   { Const i }
%inline pre_unop:
    | INC       { PreInc }
    | DEC       { PreDec }
    | EXCLAM    { Not }

%inline post_unop:
    | INC       { PostInc }
    | DEC       { PostDec }

%inline binop:
    | ADD       { Plus }
    | SUB       { Minus }
    | MUL       { Times }
    | DIV       { Divide }
    | MOD       { Modulus }
    
    | AND {And}
    | OR {Or}

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
    | LPAREN es = separated_list(COMMA, exp) RPAREN { seq_of_list es }
comma_str_params: 
    | LPAREN ps = separated_list(COMMA, STRING) RPAREN { ps }

