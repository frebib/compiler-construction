# Testing
Every test in this project is defined in a file as described in the next section. Running the test through `bin/gentest` will generate an OCaml program which will, when run, parse, evaluate (and eventually compile) the code. It is a completely autonomous system, reporting any erros encountered with the test along the way.

To improve the usability, several useful scripts have been provided to make testing as fast as possible:
* `newtest <testname>` will generate an empty test file in the `test` directory
* `runtest [testname]` will build and run all tests, or specific tests when called with test file paths as arguments.
* `make clean test` as well as building the `bin/gentest` binary, will also call `runtest` against every test in the project.

## Test return values
Tests will return different error codes depending on the error, as well as any messages they may also print. Below are all the return codes used to provide an insight into what went wrong with the test.

Code | Reason
:---:|-------
0 | Success!
1 | General error occurred within the test framework
2 | An error occurred when parsing the code
3 | An error occurred with evaluating the code
4 | An error occurred with compiling the code

## Test file format
Any file in the `test/` directory with the `*.test` file extension will be considered as a valid test definition. The format of the file is split up into 3 or more ordered and distinct sections. Each section has a header of `/% <NAME HERE> %/` and is terminated by the start of the next section in the file. The last section identifier must be `/% END %/` as it denotes the end of the file.

Keyword | Usage | Extra
--------|-------|-------
CODE | A literal string of the code to test with the parser and compiler. |
TREE | The tree representation of the code in the format of the OCaml toplevel | 
RESULT | An expression equal to that returned from the code |
OUTPUT | A literal string matching the dumped output from stdout by the code | Optional
END | Denotes the end of the file

*_An example test file:_*

```
/% CODE %/

function main(argc, argv) {
  ...
}

/% TREE %/

Program [
  ("main", [], ...)
]

/% RESULT %/

...

/% END %/
```

## Roadmap
[ ] - Implementation of the `OUTPUT` section.
      _Note: It is currently an invaid token and will cause the test to fail_
[ ] - Testing without compilation. This is currently improbable due to the requirement of non-standard libraries
