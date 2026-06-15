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
    ;; v0.4 ROUND-9 D5 grind punch list (design utwrpzmx,
    ;; air/v0.4/org-air-round9-design-d5.org — sidebar/context-rail
    ;; refinement, byte rail; impl is landing it now).  The rail-key FACE
    ;; landed, but it is not yet APPLIED and the byte rail structure is
    ;; still the round-8 rail — delete each as the D5 byte parts land:
    (org-air-r9-d5-hrule-cap-glyph-defined
     . "D5a: hrule-cap glyph (╶ . -) not registered in org-air-glyphs yet")
    (org-air-r9-d5a-rail-rule-family-has-cap
     . "D5a: rail rules (Summary/Filters) do not yet open with the ╶ hrule-cap")
    (org-air-r9-d5a-actions-block-named
     . "D5f: floating hints not yet a named Actions labelled-rule block")
    (org-air-r9-d5a-calendar-is-labelled-rule
     . "D5a: calendar month header not yet rendered as a labelled rule w/ right-anchored nav")
    (org-air-r9-d5c-legend-separated-and-spaced
     . "D5c: legend not yet blank-separated/spaced (◆ due  ● sched  · created)")
    (org-air-r9-d5d-ledger-sum-rule
     . "D5d: total still under a full-width hairline, not a short ledger-sum rule")
    (org-air-r9-d5f-actions-aligned-no-dot-separators
     . "D5f: Actions verbs still use · separators, not column alignment")
    (org-air-r9-d5f-rail-key-keycap-applied
     . "D5f: org-air-face-rail-key not yet applied to the Actions key tokens")
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
