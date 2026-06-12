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
    ;; (test-symbol . "reason / spec reference")
    ;; 2026-06-12: 5 date-label sign-inversion entries closed out — fix
    ;; landed on trunk3 (pwuqtvlt) and all 5 regression tests pass.
    ;;
    ;; v0.2 full-viewport layout grind tests (air/v0.2/org-air-layout.org).
    ;; Expected to fail until the v0.2 renderer lands; delete each entry
    ;; as impl satisfies it.
    (org-air-viewport-render-honours-width-seam
     . "v0.2: renderer must consult the org-air-view-width seam (responsive layout)")
    (org-air-viewport-lines-compose-to-width-80
     . "v0.2: full-width composition — every non-blank line exactly 80 cols")
    (org-air-viewport-lines-compose-to-width-120
     . "v0.2: full-width composition — every non-blank line exactly 120 cols")
    (org-air-viewport-lines-compose-to-width-160
     . "v0.2: full-width composition — every non-blank line exactly 160 cols")
    (org-air-viewport-no-trailing-whitespace
     . "v0.2: no trailing-whitespace drift (current calendar rows end in blanks)")
    (org-air-viewport-calendar-present-zero-items
     . "v0.2: calendar ALWAYS rendered, incl. fully-empty view (current empty view skips it)")
    ;;
    ;; v0.2 phase-2 mockup-true tests (air/v0.2/org-air-layout-design.org §9).
    (org-air-layout-divider-unbroken-120
     . "design §9.1: pane divider runs unbroken at one column through the 120-col body band")
    (org-air-layout-divider-unbroken-160
     . "design §9.1/§1.4: divider unbroken at 160; item pane flexes so it sits right of the 120-col one")
    (org-air-layout-two-pane-at-threshold
     . "design §1.4: two-pane layout engages at exactly org-air-layout-two-pane-min (100)")
    (org-air-layout-calendar-survives-all-hiding-filter
     . "design §4.1/§9.2: calendar renders even when a filter hides every item")
    (org-air-layout-empty-board-holds-shape
     . "design §5.2/§9.3: zero-items board keeps headings+0 badges, summary total, filters placeholder, calendar")
    (org-air-layout-summary-counts-match-sections
     . "design §4.2/§9.4: summary rows mirror count badges; total equals banner visible count")
    (org-air-layout-rail-faces-applied
     . "design §7/§9.5: pane-border, rail-title, summary-number faces applied in a real render")
    (org-air-layout-tty-no-gui-glyph-leak
     . "design §6.1/§9.6: no GUI box/rule glyphs in a TTY render (current rule uses ─ unconditionally)")
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
