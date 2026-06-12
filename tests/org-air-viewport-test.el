;;; org-air-viewport-test.el --- viewport layout tests for org-air -*- lexical-binding: t; -*-

;;; Commentary:
;; Phase-1 viewport suite for the v0.2 full-viewport layout
;; (air/v0.2/org-air-layout.org).  Two kinds of tests:
;;
;;   1. Harness self-tests — prove the batch render-at-width machinery
;;     works against the current implementation (must stay green).
;;   2. Spec-true grind tests — encode v0.2 layout invariants frozen in
;;     the brief (width seam honoured, full-width line composition, no
;;     trailing whitespace, calendar present even with zero items).
;;     These FAIL until the v0.2 renderer lands and are listed in
;;     tests/org-air-known-failures.el accordingly.
;;
;; Phase 2 (mockup-true pane/breakpoint assertions) follows once the
;; design track publishes air/v0.2/org-air-layout-design.org.

;;; Code:

(require 'ert)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)

(when (locate-library "org-air")
  (require 'org-air))

;;;; Harness self-tests — green against the current implementation.

(ert-deftest org-air-viewport-harness-renders-at-all-widths ()
  "The dashboard renders in batch at every canonical width."
  (skip-unless (locate-library "org-air"))
  (dolist (width org-air-viewport-test-widths)
    (org-air-viewport-test-with-dashboard width
      (should (> (buffer-size) 0))
      ;; Representative fixture items survive the width binding.
      (should (string-match-p "Prepare standup notes" (buffer-string))))))

(ert-deftest org-air-viewport-harness-zero-items-renders ()
  "The dashboard renders in batch with an entirely empty file set."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-empty-dashboard 120
    (should (> (buffer-size) 0))))

(ert-deftest org-air-viewport-harness-clock-frozen ()
  "Renders under the harness see `org-air-test-now' (June 2026 calendar)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 120
    (should (org-air-viewport-test-calendar-present-p))))

(ert-deftest org-air-viewport-harness-line-measurement ()
  "Width / trailing-whitespace measurement primitives behave."
  (with-temp-buffer
    (insert "1234567890\n" "12345\n" "\n" "ends in blank \n")
    (let ((lines (org-air-viewport-test-lines)))
      (should (= (length lines) 4))
      (should (= (string-width (nth 0 lines)) 10)))
    (should (equal (mapcar #'car (org-air-viewport-test-misaligned-lines 10))
                   '(2 4)))
    (should (equal (mapcar #'car
                           (org-air-viewport-test-trailing-whitespace-lines))
                   '(4)))))

;;;; Spec-true grind tests — v0.2 brief invariants, expected to fail
;;;; until the full-viewport renderer lands (see known-failures manifest).

(ert-deftest org-air-viewport-render-honours-width-seam ()
  "The renderer consults `org-air-view-width': different widths,
different composition.  Spec: responsive layout adapts to window
width (air/v0.2/org-air-layout.org, Goals)."
  (skip-unless (locate-library "org-air"))
  (let (narrow wide)
    (org-air-viewport-test-with-dashboard org-air-viewport-test-narrow-width
      (setq narrow (substring-no-properties (buffer-string))))
    (org-air-viewport-test-with-dashboard org-air-viewport-test-wide-width
      (setq wide (substring-no-properties (buffer-string))))
    (should-not (equal narrow wide))))

(ert-deftest org-air-viewport-lines-compose-to-width-80 ()
  "Every non-blank line composes to exactly 80 columns at width 80."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 80
    (org-air-viewport-test-assert-aligned 80)))

(ert-deftest org-air-viewport-lines-compose-to-width-120 ()
  "Every non-blank line composes to exactly 120 columns at width 120."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 120
    (org-air-viewport-test-assert-aligned 120)))

(ert-deftest org-air-viewport-lines-compose-to-width-160 ()
  "Every non-blank line composes to exactly 160 columns at width 160."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 160
    (org-air-viewport-test-assert-aligned 160)))

(ert-deftest org-air-viewport-no-trailing-whitespace ()
  "No rendered line ends in literal whitespace (alignment drift guard)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 120
    (org-air-viewport-test-assert-no-trailing-whitespace)))

(ert-deftest org-air-viewport-calendar-present-with-items ()
  "Calendar pane is rendered on the populated dashboard.
Spec: calendar ALWAYS rendered (air/v0.2/org-air-layout.org, Goals)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 120
    (should (org-air-viewport-test-calendar-present-p))))

(ert-deftest org-air-viewport-calendar-present-zero-items ()
  "Calendar pane is rendered even when there are NO items at all.
Spec: calendar ALWAYS rendered, regardless of item counts or filters;
the fully-empty view keeps calendar + summary intact."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-empty-dashboard 120
    (should (org-air-viewport-test-calendar-present-p))))

(provide 'org-air-viewport-test)
;;; org-air-viewport-test.el ends here
