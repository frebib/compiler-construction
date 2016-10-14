GEN_SRC_DIR=src/test/generated
BINARY_DIR=bin
MENHIR_DIR=_menhir
TARGET=main.native

default: main
main: $(TARGET)

%.native:
	ocamlbuild -use-menhir -use-ocamlfind $@
	mkdir -p $(BINARY_DIR)
	mv $@ $(BINARY_DIR)/$*

test: testing.native
	mkdir -p _build/test

clean:
	ocamlbuild -clean
	$(RM) -r $(MENHIR_DIR) $(BINARY_DIR) $(GEN_SRC_DIR)

.PHONY: clean test
