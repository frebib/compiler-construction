# A Language parser written with Menhir

This project is written with [Merlin](https://github.com/the-lambda-church/merlin) in mind and provides some useful completion when using it. Make a full build with `make` and completion _should just work_.

### Parser sources
All parser source is stored in `src/`
* `main.ml` - Top level program that invokes the lexer/parser
* `lexer.mll` - Text lexer that converts text into tokens
* `parser.mly` - Token parser that generates the parse tree

* `types.ml` - The Abstract Syntax Tree definition and accompanying helper functions for construction and printing
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
Execute the program with a script to parse as the argument(s)

```
bin/main <inputfile>
```

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

#### Running the tests
Compiling and running all available tests are as simple as running the following script which will initialise the 'test builder', compile all tests in `test/` and run them, printing `PASS` or `FAIL` for each one in turn.

```
./runtest
```

#### Creating a test
Simply drop a file following the structure below into the `test/` directory and run the `./runtest <testname>` script to compile and run it. 

The `newtest` script when called with an argument will generate an empty test from template with appropriate naming and in the correct format, just as below.


```
/% TEST %/

function testme(abc) {
...
}
...

/% OUTPUT %/

[
    ("testme", ["abc"], ...);
    ...
]

/% END %/
```
