%{ 
  open Test 
  let join = String.concat "\n"
%}

%token <string> LINE
%token CODE TREE RESULT END EOF

%start <Test.test> parse_test
%%

parse_test:
  | CODE
    c = LINE*
    TREE
    o = LINE* 
    RESULT
    r = LINE* 
    END EOF { Test (join c, join o, join r) }
