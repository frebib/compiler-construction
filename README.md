# A Language parser written with Menhir

* `main.ml` - Top level program that invokes the lexer/parser
* `lexer.mll` - Text lexer that converts text into tokens
* `parser.mly` - Token parser that generates the parse tree

This project is written with [Merlin](https://github.com/the-lambda-church/merlin) in mind and provides some useful completion when using it. Make a full build with `make` and completion _should just work_.

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
make clean all
```

## To run the parser

```
./main.native <inputfile>
```

