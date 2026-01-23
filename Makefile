# Makefile for doing.el

EMACS ?= emacs
BATCH = $(EMACS) --batch -Q

# Source files
SRCS = doing.el
TESTS = tests/doing-test.el

# Load path for tests
LOAD_PATH = -L . -L tests

.PHONY: test test-verbose clean compile

# Run all tests
test:
	$(BATCH) $(LOAD_PATH) -l ert -l doing -l tests/doing-test.el \
		-f ert-run-tests-batch-and-exit

# Run tests with verbose output
test-verbose:
	$(BATCH) $(LOAD_PATH) -l ert -l doing -l tests/doing-test.el \
		--eval "(ert-run-tests-batch-and-exit '(or t))"

# Byte-compile all source files
compile:
	$(BATCH) $(LOAD_PATH) -f batch-byte-compile $(SRCS)

# Clean compiled files
clean:
	rm -f *.elc tests/*.elc

# Run a single test (usage: make test-one TEST=doing-test-function-exists)
test-one:
	$(BATCH) $(LOAD_PATH) -l ert -l doing -l tests/doing-test.el \
		--eval "(ert-run-tests-batch-and-exit '$(TEST))"
