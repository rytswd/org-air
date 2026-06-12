# org-air — test harness
#
# Targets:
#   make deps     install org-ql + test deps into repo-local .deps/ (idempotent)
#   make test     run the ERT suites in batch; non-zero exit on any failure
#                 (binary gate: tests/org-air-known-failures.el marks grind
#                 tests as expected-to-fail; a stale entry also fails)
#   make compile  byte-compile all org-air*.el with warnings visible
#   make lint     checkdoc + package-lint vs tests/org-air-lint-baseline.el
#                 (binary: new findings fail, stale baseline entries fail)
#   make check    the full gate: compile + lint + test (default target)
#   make regen-mockups  regenerate tests/fixtures/layout-mockup-*.txt from
#                 the REAL renderer (guards active); diff + design re-bless
#                 required before any gate verdict
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

.PHONY: all check deps test compile lint clean regen-mockups

all: check

check: compile lint test

deps:
	$(BATCH) -l tests/org-air-test-deps.el

test: deps
	$(BATCH) $(patsubst %,-l %,$(TEST_FILES)) -l $(MANIFEST) \
	  -f org-air-test-apply-known-failures -f ert-run-tests-batch-and-exit

lint: deps
	$(BATCH) -l tests/org-air-lint.el -f org-air-lint-batch

regen-mockups: deps
	$(BATCH) -l tests/org-air-regen-mockups.el -f org-air-regen-mockups

compile: deps
ifeq ($(strip $(SRC_FILES)),)
	@echo "compile: no org-air*.el files in this checkout; nothing to compile"
else
	$(BATCH) --eval '(setq byte-compile-error-on-warn nil)' \
	  -f batch-byte-compile $(SRC_FILES)
endif

clean:
	rm -f *.elc tests/*.elc
