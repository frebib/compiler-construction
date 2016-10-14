%{ 
  open Test 
  
  let join = String.concat "\n"
%}

%token <string> CODE
%token TEST OUTPUT END EOF

%start <Test.test> parse_test
%%

parse_test:
  | TEST
    c = CODE*
    OUTPUT
    o = CODE* 
    END EOF {(join c, join o) }
