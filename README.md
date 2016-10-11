# A Language parser written with Menhir

* `main.ml` - Top level program that invokes the lexer/parser
* `lexer.mll` - Text lexer that converts text into tokens
* `parser.mly` - Token parser that generates the parse tree

# Compiling the parser

Simply run:

```
make
```

or to clean then build:

```
make clean all
```

## To run the parser

```
./main.native <inputfile>
```
