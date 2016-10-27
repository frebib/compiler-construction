%{ 
    open Types
    open Error
%}

(* Misc *)
%token          EOF
%token <string> IDENT
%token <int>    INT
%token <bool>   BOOL

(* Punctuation *)
%token          COMMA SEMICOLON EXCLAM AND OR
%token          LBRACE RBRACE LPAREN RPAREN TILDE
%token          ADD SUB MUL DIV MOD EQUAL INC DEC
%token          DBLEQUAL NOTEQUAL LTHAN GTHAN LEQUAL GEQUAL

(* Keywords *)
%token          FUNCTION FUN ARROW RETURN
%token          READINT PRINTINT
%token          IF ELSE WHILE DO
%token          LET VAR

(* http://cs.stmarys.ca/~porter/csc/ref/cpp_operators.html *)
%nonassoc EQUAL
%right    ELSE DO
%right    ARROW
%left     OR
%left     AND
%right    DBLEQUAL NOTEQUAL
%nonassoc LTHAN GTHAN LEQUAL GEQUAL
%left     ADD SUB
%left     MUL DIV MOD
%left     TILDE
%right    EXCLAM
%nonassoc INC DEC
%right    RETURN

%start <Types.expression> init
%%

init:
    | d = defin EOF { d }

body:
    (* A brace delimited body or a single statement*)
    | s = statement { s }
    | b = blockbody { b }

blockbody:
    | LBRACE ss = statements RBRACE { ss }
    | LBRACE ss = statement* d = def? error { raise (syntax_error "Expected a '}'") }

statements:
    | ss = statement* d = def? { opt_append_seq ss d }

statement:
    (* A complete statement; a brace block or semicolon terminated expression*)
    | e = blockexp { e }
    | e = exp SEMICOLON { e }
    | e = exp error { raise (error_of_fn Parse (fun buf -> "Unexpected token '" ^ Lexing.lexeme buf ^ "'. Did you forget a semicolon?")) }
    
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
    | i = ident                         { i }
    | i = ident; ps = comma_exp_params  { Application (i, ps) }
    | TILDE e = exp                     { Deref e }
    | READINT LPAREN RPAREN             { Readint }
    | PRINTINT LPAREN e = exp RPAREN    { Printint e }

    | e = exp EQUAL v = exp             { Asg (e, v) }
    | op = pre_unop e = exp             { UnaryOp (op, e) }
    | e = exp op = post_unop            { UnaryOp (op, e) }
    | e1 = exp op = binop e2 = exp      { BinaryOp (op, e1, e2) }

    | f = anonfunc                      { f }
    | RETURN e = exp                    { e }

    (* Inline statements are if/while statements without the
     * { } block syntax taking single expression arguments 
     * If >1 expressions are required, use the block-type
     * statements with parenthesis around: (if (x) { .. }) *)
    | IF e = exp_param ib = exp ELSE eb = exp { If (e, ib, eb) }

    (* Inline while loops differ as they require the 'do' keyword *)
    (* Example: let x = while(true) do x++; *)
    | WHILE p = exp_param DO ss = exp { While (p, ss) }

anonfunc:
    | FUN p = comma_sep_str ARROW e = exp { Function(p, e) }

def: 
    | VAR var = IDENT EQUAL e = statement i = defin { New (var, e, i) }
    | LET var = IDENT EQUAL e = statement i = defin { Let (var, e, i) }
    | FUNCTION n = IDENT p = comma_str_params b = blockbody i = defin { Let (n, Function (p, b), i) }

defin:
    | d = def { d }
    | ss = statement* { seq_of_list ss }

%inline ident:
    | s = IDENT { Identifier s }

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
    
    | AND       { And }
    | OR        { Or }

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
    | LPAREN es = separated_list(COMMA, exp) RPAREN { es }
comma_str_params: 
    | LPAREN ps = separated_list(COMMA, IDENT) RPAREN { ps }
comma_sep_str: 
    | s = separated_list(COMMA, IDENT) { s }
    | LPAREN s = comma_sep_str RPAREN { s }

