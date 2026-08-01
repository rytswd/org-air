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
#   make bench-gui  THE R99 PERFORMANCE FENCE: expand a realistic section
#                 with the DEFAULT styles in a REAL frame and assert a
#                 wall-clock ceiling.  Needs DISPLAY; exits 2 (NOT 0) when
#                 there is none, because `emacs --batch' keeps
#                 gc-cons-percentage at 1.0 and therefore never collects --
#                 which is exactly why every batch benchmark this project
#                 ever ran was blind to the reported 1-second TAB.
#                 Run as part of `check-gui'.
#   make check-gui  THE THIRD GATE MODE (R97 B-2): the same ERTs under a
#                 REAL display, so the `(skip-unless (display-graphic-p))'
#                 tests -- never executed once in this project's history
#                 -- finally run.  Needs DISPLAY (or Xvfb); deliberately
#                 NOT part of `check', because a gate that cannot run
#                 everywhere is a gate people learn to ignore.
#                   make check-gui
#                   make check-gui GUI_SELECTOR='.*'   # whole suite, GUI
#                   make check-gui DISPLAY=:99         # explicit display
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

GUI_SELECTOR ?=
GUI_REPORT   ?= /tmp/org-air-gui-report.txt

BENCH_CEILING ?=

.PHONY: all check check-gui bench-gui gui-fence-skipped deps test compile \
        lint clean regen-mockups

all: check

check: compile lint test

# R99 -- printed by EVERY batch run, pass or fail, so a green batch gate can
# never be read as a performance verdict.  This is the fourth blind spot
# found by measuring the wrong machine; the notice is part of the fix.
gui-fence-skipped:
	@echo "================================================================="
	@echo "NOTE: the GUI PERFORMANCE FENCE IS NOT RUNNING."
	@echo "      This gate is batch-only, and 'emacs --batch' keeps"
	@echo "      gc-cons-percentage at 1.0: the collector that was 87% of"
	@echo "      the R99 defect never runs here, and string-width is free"
	@echo "      here and is not on a frame.  A green batch run says"
	@echo "      NOTHING about rendering speed.  On a display:"
	@echo "          make check-gui        # ERTs + the expand fence"
	@echo "          make bench-gui        # the expand fence alone"
	@echo "================================================================="

deps:
	$(BATCH) -l tests/org-air-test-deps.el

test: deps gui-fence-skipped
	$(BATCH) $(patsubst %,-l %,$(TEST_FILES)) -l $(MANIFEST) \
	  -f org-air-test-apply-known-failures -f ert-run-tests-batch-and-exit

lint: deps
	$(BATCH) -l tests/org-air-lint.el -f org-air-lint-batch

# R97 B-2 -- ERT under a real display.  NOT `--batch': the runner arms a
# `run-with-timer' one-shot and returns, so `-l' finishes and Emacs
# reaches its command loop with a mapped frame.  Loading the runner with
# `-f' (or running ERT during `-l') is the trap the R96 reviewer hit:
# idle timers cannot fire while a file is still being loaded, and you get
# a false "the board never loads".
check-gui: deps
	@if [ -z "$$DISPLAY" ]; then \
	  echo "check-gui: DISPLAY is unset -- run under a real X display or Xvfb"; \
	  echo "           e.g.  Xvfb :99 & DISPLAY=:99 make check-gui"; \
	  exit 2; \
	fi
	GUI_SELECTOR='$(GUI_SELECTOR)' ORG_AIR_GUI_REPORT='$(GUI_REPORT)' \
	  $(EMACS) -Q -l $(INIT) $(patsubst %,-l %,$(TEST_FILES)) -l $(MANIFEST) \
	    -f org-air-test-apply-known-failures -l tests/org-air-gui-runner.el
	@echo "check-gui: transcript in $(GUI_REPORT)"
	@$(MAKE) --no-print-directory bench-gui

# R99 -- the performance fence.  A SEPARATE process from the ERT run: it
# measures wall clock, so it must not share a heap with a full suite.
# Exit 2 when there is no display; the recipe lets that fail the target,
# because a fence that cannot run must never look like one that passed.
bench-gui: deps
	@if [ -z "$$DISPLAY" ]; then \
	  echo "bench-gui: DISPLAY is unset -- the expand fence CANNOT RUN."; \
	  echo "           This is NOT a pass.  e.g.  Xvfb :99 & DISPLAY=:99 make bench-gui"; \
	  exit 2; \
	fi
	$(EMACS) -Q -l $(INIT) -l org-air-bench.el \
	  $(if $(strip $(BENCH_CEILING)),--eval '(setq org-air-bench-expand-ceiling $(BENCH_CEILING))',) \
	  -f org-air-bench-batch-fence

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
