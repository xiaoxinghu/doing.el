# Makefile for doing.el

EMACS ?= emacs
BATCH = $(EMACS) --batch -Q

# Source files
SRCS = doing.el
TESTS = tests/doing-test.el

# Load path for tests
LOAD_PATH = -L . -L tests

.PHONY: test test-verbose clean compile lint checkdoc check

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

# Lint with package-lint
lint:
	$(BATCH) -L . \
		--eval "(require 'package)" \
		--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\"))" \
		--eval "(package-initialize)" \
		--eval "(package-refresh-contents)" \
		--eval "(package-install 'package-lint)" \
		--eval "(require 'package-lint)" \
		--eval "(setq package-lint-main-file \"doing.el\")" \
		-f package-lint-batch-and-exit \
		doing.el doing-lib.el doing-now.el doing-note.el doing-finish.el \
		doing-again.el doing-cancel.el doing-current.el doing-rollover.el \
		doing-search.el doing-totals.el doing-utils.el doing-view.el \
		doing-view-commands.el

# Check documentation
checkdoc:
	$(BATCH) -L . --eval "(or (checkdoc-file \"doing.el\") (kill-emacs 1))"

# Run all checks
check: compile lint checkdoc test
