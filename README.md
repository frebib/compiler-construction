# A Language parser written with Menhir

This project is written with [Merlin](https://github.com/the-lambda-church/merlin) in mind and provides some useful completion when using it. Make a full build with `make` and completion _should just work_.

### Parser sources
All parser source is stored in `src/`
* `main.ml`    - Top level program that invokes the lexer/parser
* `lexer.mll`  - Text lexer that converts text into tokens
* `parser.mly` - Token parser that generates the parse tree
* `eval.ml`    - Expression evaluation for 'running' programs without compilation  
  

* `types.ml` - The Abstract Syntax Tree definition and accompanying helper functions for construction
* `print.ml` - Utility functions for printing expressions as defined in `types.ml` in a OCaml 'toplevel' format
* `parse.ml` - Functions for running parse tasks, from file or `string` and to `Types.program` or `string`
* `error.ml` - A small utility library to handle exceptions thrown by the parser

### Testing sources
The testing suite consists of the following code in `test/generator`
* `test.ml` - The generator and helper functions used to build and by the test executables
* `testing.ml` - A small program to generate the code for the test programs
* `testlex.mll` and `testpar.mly` - Simple lexer and parser pair to extract the test cases from the `.test` files

# Compiling the parser

You'll need OCaml, menhir and ocamlfind.
Install them if not already either with opam [or manually](http://gallium.inria.fr/~fpottier/menhir/)
```
opam install menhir ocamlfind
```

Simply run:

```
make
```

or to clean then build:

```
make clean default
```

# Testing & Debugging
### Running the parser
The `bin/main` entrypoint has several functions.
For more information on how these work, consult the help page with `bin/main --help`

The first argument specified to the program is the command to run. The following options are available:
* `parse`    - Parses the program code and prints the tree
* `eval`     - Evaluates a tree and prints the result (including any printed text from running the program)
* `parseval` - Combines the above options, parsing then evaluating the program code.

Additionally this can be used with `*.test` formatted files too. When supplied with the `--test` flag, the program will parse the test data from the script before executing the given command.  

_It should be noted that this doesn't run the testing facility and does no checking that the test is correct or valid. You will have to use `runtest <file>` for that. More is explained about this in the test documentation in `doc/test.md`_

### Debugging the parser
The `explain` script will run `menhir --explain` on the `.mly` files and provide the output.
Run it with no arguments to explain all files or optionally specify a file name to debug.
```
./explain <filename>
```

### Testing the parser
Along with the parser, I have built a very simple test parser and generator. All tests reside within the `test/` directory and consist of a section of code to test followed by the OCaml tree that represents the code above. Upon compiling and running the tests, the code will be parsed and compared to the tree determining whether the test passed or failed.

To initialise the testing facility, build the 'test builder' program:
```
make test
```

Subsequent runs of the tests can be completed with either `make clean test` or the `runtest` script

More information about the test tools in the test documentation in `doc/test.md`
