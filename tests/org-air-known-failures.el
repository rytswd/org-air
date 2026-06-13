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
    ;; Screenshot-3 round:
    (org-air-s5a-point-on-visible-char-all-paths
     . "S5a gap: filter that hides the anchored item drops point onto fill whitespace (restore-position line/column fallback); all other paths pass")
    ;;
    ;; Triage round (air/v0.2/org-air-triage.org, ready) + S7/S8 tuning
    ;; (air/v0.2/org-air-aesthetics.org) — impl2 punch list:
    (org-air-triage-keymap-dispositions
     . "triage: disposition keys s/d/r/f/t/T/a/D/k/I bound to spec'd commands")
    (org-air-triage-keymap-scope-remap
     . "triage: scope remap — S=scope, M-s=scope-clear, s freed for schedule")
    (org-air-triage-dated-inbox-item-graduates
     . "triage graduation: inbox-file item WITH a date is not Inbox; joins its date bucket (classify)")
    (org-air-triage-inbox-section-only-undated
     . "triage graduation, render level: Inbox section holds only undated items (calendar-vs-bucket consistency)")
    (org-air-triage-process-inbox-command-exists
     . "triage: org-air-process-inbox guided mode entry point")
    (org-air-s7-header-status-ends-at-w-minus-1
     . "S7: header reserves one right-margin column — status ends at W-1, never W (byte-affecting: regen later)")
    (org-air-s8-line-spacing-zero-buffer-local
     . "S8 mechanism: line-spacing buffer-locally 0 in org-air-view-mode (pixel effect GUI-only, documented)")
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
