BUILD_DIR=_build
MENHIR_DIR=_menhir
TARGET=main.native

default: main
main: $(TARGET)

%.native:
	ocamlbuild -use-menhir -use-ocamlfind $@
	mv $@ $*
clean:
	ocamlbuild -clean
	$(RM) -r $(MENHIR_DIR)

.PHONY: clean
