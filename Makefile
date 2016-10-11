BUILD_DIR=_build
TARGET=main.native

all:
	ocamlbuild -use-menhir -use-ocamlfind $(TARGET)

clean:
	$(RM) -r $(BUILD_DIR) $(TARGET)
