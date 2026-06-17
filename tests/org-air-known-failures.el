;;; org-air-known-failures.el --- expected-failure manifest for org-air -*- lexical-binding: t; -*-

;;; Commentary:
;; Single source of truth for tests that are EXPECTED to fail right now
;; (grind signals for the impl track).  `make test' loads this file after
;; all test suites and calls `org-air-test-apply-known-failures', which
;; marks every listed test with an expected result of :failed.
;;
;; Effect on orchestration: the suite is binary.
;;   - a listed test failing      -> expected   (exit 0)
;;   - a listed test PASSING      -> unexpected (exit 1) — remove it here!
;;   - an unlisted test failing   -> unexpected (exit 1)
;; So `make test' exit code is the single integration gate, and a stale
;; manifest entry is itself a failure, keeping this list honest.
;;
;; Workflow: when writing a spec-true test the impl does not satisfy yet,
;; add its symbol here with a short reason.  When impl fixes it, the run
;; goes red until the entry is deleted — that deletion is the closeout.

;;; Code:

(require 'ert)

(defvar org-air-test-known-failures
  '(
    ;; (test-symbol . "reason / spec reference")  — none right now.
    ;; 2026-06-12: 5 date-label sign-inversion entries closed out — fix
    ;; landed on trunk3 (pwuqtvlt) and all 5 regression tests pass.
    ;; 2026-06-12: the entire 17-entry v0.2 grind manifest closed out on
    ;; the integrated main line (wzmskzwn) — 16 entries verified
    ;;
    ;; ===================================================================
    ;; v0.4 ROUND-10 grind (impl track) — fixtures/tests that change BYTES
    ;; under the design-blessed D-P1.PAD / D-P4 / D-P5 contracts.
    ;;
    ;; 2026-06-17: round-10 TEST-track re-bless (change <this commit>).
    ;; 15 of the 19 entries CLOSED — fixtures regenerated from impl's tip
    ;; umrpsoxp via the FROZEN-CLOCK renderer (make regen-mockups, guards
    ;; active) and the byte/assertion tests re-blessed to the new D-P1.PAD
    ;; / D-P4 / D-P5 contracts:
    ;;   D-P1.PAD (pill pad-cols, text-layer geometry): mockup-80/120/160
    ;;     regen'd; r10-item-row, v1b, b1, b2, s5a, ux-u3 (x2) re-rendered
    ;;     wide (160) where the padded cluster no longer truncates the
    ;;     searched title; r9-c2 re-blessed to the NEW pure-overlay
    ;;     contract (pad-cols=0 -> pill == text; pad-cols=1 -> pill wider).
    ;;   D-P4 (centred calendar): r9-d5b re-blessed — text blocks share the
    ;;     content spine, the calendar grid is centred OFF it.
    ;;   D-P5 (shared-row project view): project-view-{state,dir,tag}
    ;;     fixtures regen'd; f5-tree-structure + f5-grouping-toggle
    ;;     rewritten to the shared-row sections (no box glyphs/roll-up).
    ;; -------------------------------------------------------------------
    ;; 2026-06-17: round-10 CLOSEOUT — the last 4 entries CLOSED on impl's
    ;; narrow-tier fix nzruxkrm (clamps the assembled LEFT to width -
    ;; cluster-w - gap, so a wide [#A] prefix + reserved pad cols can no
    ;; longer shove the V6 cluster right).  Byte-verified the fix is a
    ;; NO-OP at the wide tiers (W80/120/160/90/104/110 + project + empty
    ;; fixtures byte-identical to the earlier regen ovrwrtzt); regenerated
    ;; ONLY the W96/W100 mockups, which now show the V6 date column ALIGNED
    ;; (W96 and W100 each render a single date column).  All four deleted:
    ;;   org-air-layout-mockup-thresholds, org-air-layout-mockup-heights,
    ;;   org-air-r9-c1-narrow-resize-refits,
    ;;   org-air-r9-c3-text-scale-refit-consistent.
    ;; Round-10 manifest is EMPTY; the tests stay as permanent regression
    ;; guards (V6 alignment at every tier).
    ;; ===================================================================
    ;; passed-unexpectedly through the anti-tautology render guards
    ;; (real renderer produced the bytes: width seam, 80/120/160
    ;; composition, byte-precise §3 mockups, divider geometry,
    ;; persistent calendar, empty-board shape, summary integrity, rail
    ;; faces, TTY fallback); the 17th, org-air-viewport-no-trailing-
    ;; whitespace, was RETIRED with its test as §9.1-obsolete: spec rev
    ;; orwonzvz makes plain-space padding to full width the contract
    ;; (comparison is right-trimmed), so trailing whitespace is mandated
    ;; — drift protection now lives in the exact-width assertions and
    ;; the byte-precise mockup comparison.
    ;; 2026-06-12: all 3 F1/F2a/F3 data-variation entries closed out —
    ;; impl2's de-hardcoding (svknxvow) verified honest on both boards
    ;; (calendar = true data union, no TTY glyph leaks).  Note: the
    ;; fidelity review's hand-derived original-fixture union missed the
    ;; real Jun 19 deadline; the test now parses ground truth from the
    ;; fixture files directly.
    ;; 2026-06-13: mockup-80/120/160 closed out — fixtures regenerated
    ;; ONCE from the honest D1-D7 renderer (ymvlyvuk) via make
    ;; regen-mockups (guards active), plus the new threshold set
    ;; 90/96/100/104/110; design FINAL-blessed at rpuxmmlz.
    ;;
    ;; Screenshot round (GUI bugs the byte gate missed; impl2 punch
    ;; list — delete as fixes land):
    ;; 2026-06-13: S1 entry closed — impl2 nnoosnpn removed the
    ;; header-line path; verified header-line-format nil on both glyph
    ;; paths, and the v0.1 filter-chip test rewritten to read the
    ;; in-buffer band.
    ;; 2026-06-13: 4 stale-mockup entries closed — S-round one-shot
    ;; regen executed against impl2 nnoosnpn@addb0c59 (widths ×
    ;; {natural,24,50} + empty-board@120x50); routed to design for
    ;; re-blessing.
    ;; 2026-06-13: S5/S6 entries closed — impl2 nnoosnpn@addb0c59
    ;; verified honest: 3-tier glyph table complete incl. tees (ASCII
    ;; final fallback), org-air-view-height defcustom adopted per the
    ;; proposed contract, 120x50 fill + empty-board sparse fill exact.
    ;;
    ;; 2026-06-13: triage-round closeout (impl2 wrqpvwzt + test-track
    ;; reconciliation).  All 9 grind entries closed:
    ;;  - 6 verified passed-unexpectedly on impl2 wrqpvwzt: S7 right-
    ;;    margin (status ends at W-1), S8 buffer-local line-spacing,
    ;;    triage scope remap (S/M-s), process-inbox entry point, the
    ;;    dated-inbox 'file with r' hint, and S5a point-on-visible across
    ;;    all point-moving paths.
    ;;  - 3 fixed on the TEST track (orchestrator ruling; impl frozen):
    ;;    keymap-dispositions T-row now expects org-air-item-cycle-todo
    ;;    (org-air-item-todo stays the cl-defstruct accessor); and the
    ;;    data-variation board's dated items were re-dated inside the
    ;;    upcoming window (dated-inbox 23->17 Jun, midsummer 25->22 Jun)
    ;;    so every calendar mark is backed by a visible date-bucket row
    ;;    (ruling xsqrnoyn consistency invariant).  The section-rows test
    ;;    helper now scans each line for the org-air-section/org-air-item
    ;;    properties (they sit past the left margin), not just column 0.
    ;;
    ;; v0.3 ROUND-7 closeout (impl2 rlqqoumn): all 9 grind entries
    ;; verified passed-unexpectedly and deleted — R3 k≠kill+j/k scroll,
    ;; R7 today-cell-no-glyph + legend-drops-today, R8 Sunday-first, R10
    ;; tags-max-2 + right-cluster, R6 day-view-command + clickable-cells,
    ;; R4 footer-removed (the rail-hint expansion landed in impl2's amend,
    ;; so the verbs are no longer lost).
    ;;
    ;; v0.4 ROUND-8 closeout (impl2 ysmvqzto): all 9 F5/V6/V3 grind
    ;; entries verified passed-unexpectedly and deleted — F5 sources/
    ;; detect/command/mode/P/tree-structure/grouping, V6 date-column +
    ;; dates-align, V3 tag-style.  The V6 metadata table truncated long
    ;; titles at narrow widths, so the title-search helpers were rendered
    ;; wide (160) or switched to org-air-item-property lookups, and the
    ;; v1b inline tests were reworked to the V6 column order.
    ;;
    ;; 2026-06-14: f5-grouping-toggle distinctness grind CLOSED — impl2
    ;; yxvztrsy landed genuinely distinct dir/tag groupings (commands
    ;; renamed to org-air-project-group-by-{state,directory,tag}); the
    ;; three renders are now pairwise byte-different.  project-view-dir/
    ;; -tag regenerated to the distinct trees; state-view + V6 dashboard
    ;; stay blessed.
    ;;
    ;; v0.4 ROUND-9 C1-Q1 closeout (impl sxyxrpzk, verified on impl's
    ;; tip): all C1/C2/C3 guards pass and F1 (×3) + Q1 scope-reset closed
    ;; — impl landed `org-air-origin-style' (auto Denote-aware), the
    ;; Denote-aware `org-air-view--origin' (strip id/__tag/.org), and the
    ;; rail `(S clears)' scope-reset hint; the D5 `org-air-face-rail-key'
    ;; face also landed via design utwrpzmx.  Five manifest entries
    ;; deleted here as passed-unexpectedly.
    ;;
    ;; v0.4 ROUND-9 D5 closeout (impl settled tip ysvynsyl, verified):
    ;; ALL 9 D5 rail-structure grinds flipped GREEN — hrule-cap glyph,
    ;; the labelled-rule family (Summary/Filters/calendar-as-rule + named
    ;; Actions), content-spine inset (org-air-rail-content-inset=3),
    ;; spaced legend, short ledger-sum rule, column-aligned no-dot verbs,
    ;; and rail-key keycaps applied.  Manifest entries deleted; the tests
    ;; stay as permanent regression guards.  Round-9 manifest is EMPTY.
    )
  "Alist of (TEST-SYMBOL . REASON) for tests expected to fail.")

(defun org-air-test-apply-known-failures ()
  "Mark every test in `org-air-test-known-failures' as expected-to-fail.
Signal an error for manifest entries that name no loaded test, so the
manifest cannot rot silently."
  (pcase-dolist (`(,name . ,reason) org-air-test-known-failures)
    (let ((test (and (ert-test-boundp name) (ert-get-test name))))
      (unless test
        (error "Known-failures manifest names unknown test: %s (%s)"
               name reason))
      (setf (ert-test-expected-result-type test) :failed)
      (message "known-failure: %s — %s" name reason))))

(provide 'org-air-known-failures)
;;; org-air-known-failures.el ends here
