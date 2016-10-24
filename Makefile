SRC_DIR=src
TEST_SRC_DIR=src/test
GEN_SRC_DIR=test/generated
BINARY_DIR=bin
MENHIR_DIR=_menhir
TARGET=main.native

default: main
main: $(TARGET)

%.native:
	export OCAMLRUNPARAM=b
	ocamlbuild -use-menhir -use-ocamlfind -cflag -g $@
	mkdir -p $(BINARY_DIR)
	mv $@ $(BINARY_DIR)/$*

test: gentest.native
	mkdir -p _build/test
	./runtest $(TESTS)

clean:
	ocamlbuild -clean -quiet
	$(RM) -r $(MENHIR_DIR) $(BINARY_DIR) $(GEN_SRC_DIR)

.PHONY: clean test
