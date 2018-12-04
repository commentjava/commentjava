OCAMLBUILD := ocamlbuild


MAIN_DIR := Main
MAIN := Main

TEST_DIR := Test
TESTS := $( shell find $(TEST_DIR) -name '*.ml' )

MLY_FILES := $( shell find . -name '*.mly' )
MLL_FILES := $( shell find . -name '*.mll' )
ML_FILES := $( shell find . -name '*.ml' )

all: $(MAIN).native

$(MAIN).native: $(ML_FILES) $(MLL_FILES) $(MLY_FILES)
	ocamlbuild $(MAIN_DIR)/$(MAIN).native

build-tests: $(MLY_FILES) $(MLL_FILES) $(MLY_FILES) $(TESTS)
	ocamlbuild $(TEST_DIR)/$(TESTS).native

clean:
	$(OCAMLBUILD) -clean
.PHONY: clean

