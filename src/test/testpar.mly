%{ 
  open Test 
  let join = String.concat "\n"
%}

%token <string> LINE
%token CODE TREE RESULT END EOF

%start <Test.test> parse_test
%%

parse_test:
  | CODE c = LINE*
    TREE o = LINE* 
    r = result?
    END EOF { Test (join c, join o, r) }

result:
  | RESULT r = LINE* { join r }
