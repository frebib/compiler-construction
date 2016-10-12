BUILD_DIR=_build
MENHIR_DIR=_menhir
TARGET=main.native

all:
	ocamlbuild -use-menhir -use-ocamlfind $(TARGET)

clean:
	$(RM) -r $(BUILD_DIR) $(MENHIR_DIR) $(TARGET)

.PHONY: clean
