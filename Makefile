OCAMLBUILD := ocamlbuild

EXT := native

MAIN_DIR := Main
MAIN := Main

TEST_DIR := Test
TEST_FILES = $(shell find $(TEST_DIR) -name '*Test.ml')
TESTS := $(basename $(notdir $(TEST_FILES)))
TEST_BINS := $(addsuffix .$(EXT), $(TESTS))
RUN_TESTS := $(addprefix test-, $(TEST_BINS))

MLY_FILES := $(shell find . -name '*.mly' )
MLL_FILES := $(shell find . -name '*.mll' )
ML_FILES := $(shell find . -name '*.ml' )

.PHONY: all clean test-all $(RUN_TESTS)

# Building receipes
all: $(MAIN).native

$(MAIN).native: $(ML_FILES) $(MLL_FILES) $(MLY_FILES)
	$(OCAMLBUILD) $(MAIN_DIR)/$(MAIN).$(EXT)

TEST_BINS: $(MLY_FILES) $(MLL_FILES) $(MLY_FILES)
	$(OCAMLBUILD) $(TEST_DIR)/$@

# Tests receipes
test-all: $(RUN_TESTS)

$(RUN_TESTS):
	./$(subst test-,,$@)

test-list:
	@for t in $(RUN_TESTS) ; do \
	echo "$$t" ; \
	done;
	@echo "test-all"

# Clean receipes
clean:
	$(OCAMLBUILD) -clean

