MENHIR := menhir
OCAMLBUILD := ocamlbuild
MENHIRFLAGS := --table --dump --explain
OCAMLFLAGS := -use-ocamlfind -use-menhir -menhir "$(MENHIR) $(MENHIRFLAGS)" -package menhirLib

EXT := native

MAIN_DIR := Main
MAIN := Main

TEST_DIR := Test
TEST_FILES = $(shell find $(TEST_DIR) -name '*Test.ml')
TESTS := $(basename $(notdir $(TEST_FILES)))
TEST_BINS := $(addsuffix .$(EXT), $(TESTS))
RUN_TESTS := $(addprefix test-, $(TEST_BINS))

TEST_JDK = TestJDK

MLY_FILES := $(shell find . -name '*.mly' )
MLL_FILES := $(shell find . -name '*.mll' )
ML_FILES := $(shell find . -name '*.ml' )

ERRORS_FILE := _build/errors.txt

.PHONY: all clean test-all $(RUN_TESTS) test-jdk

# Building receipes
all: $(MAIN).native

$(MAIN).native: $(ML_FILES) $(MLL_FILES) $(MLY_FILES) Makefile
	$(OCAMLBUILD) $(OCAMLFLAGS) $(MAIN_DIR)/$(MAIN).$(EXT)

$(TEST_JDK).native: $(ML_FILES) $(MLL_FILES) $(MLY_FILES) Makefile
	$(OCAMLBUILD) $(OCAMLFLAGS) $(TEST_DIR)/$(TEST_JDK).$(EXT)

$(TEST_BINS): $(ML_FILES) $(MLL_FILES) $(MLY_FILES) Makefile
	$(OCAMLBUILD) $(OCAMLFLAGS) $(TEST_DIR)/$@

# Tests receipes
test-all: $(RUN_TESTS)

$(RUN_TESTS): $(TEST_BINS)
	./$(subst test-,,$@)

test-list:
	@for t in $(RUN_TESTS) ; do \
	echo "$$t" ; \
	done;
	@echo "test-all"
	@echo "test-jdk"

test-jdk: $(TEST_JDK).native
	./_build/$(TEST_DIR)/$(TEST_JDK).native

# TODO: In the future the ERRORS_FILE should be commited
list-errors: $(MAIN).native
	menhir --list-errors Parsing/*.mly --base result > $(ERRORS_FILE)

# Clean receipes
clean:
	rm -f $(ERRORS_FILE)
	$(OCAMLBUILD) -clean

