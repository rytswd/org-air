# org-air — test harness
#
# Targets:
#   make deps     install org-ql + test deps into repo-local .deps/ (idempotent)
#   make test     run the ERT suites in batch; non-zero exit on any failure
#                 (binary gate: tests/org-air-known-failures.el marks grind
#                 tests as expected-to-fail; a stale entry also fails)
#   make compile  byte-compile all org-air*.el with warnings visible
#
# Everything runs with a repo-local `package-user-dir' (.deps/); the user's
# ~/.emacs.d is never touched.

EMACS ?= emacs
DEPS  := .deps

INIT  := tests/org-air-test-init.el
BATCH := $(EMACS) -Q --batch -l $(INIT)

TEST_FILES := $(wildcard tests/*-test.el)
MANIFEST   := tests/org-air-known-failures.el
SRC_FILES  := $(wildcard org-air*.el)

.PHONY: all deps test compile clean

all: test

deps:
	$(BATCH) -l tests/org-air-test-deps.el

test: deps
	$(BATCH) $(patsubst %,-l %,$(TEST_FILES)) -l $(MANIFEST) \
	  -f org-air-test-apply-known-failures -f ert-run-tests-batch-and-exit

compile: deps
ifeq ($(strip $(SRC_FILES)),)
	@echo "compile: no org-air*.el files in this checkout; nothing to compile"
else
	$(BATCH) --eval '(setq byte-compile-error-on-warn nil)' \
	  -f batch-byte-compile $(SRC_FILES)
endif

clean:
	rm -f *.elc tests/*.elc
