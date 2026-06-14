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
      ;; Representative item rows survive the width binding.  (V6 truncates
      ;; long TITLES at narrow widths, so locate items by their
      ;; `org-air-item' text property rather than a full-title search.)
      (should (text-property-not-all (point-min) (point-max)
                                     'org-air-item nil)))))

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

;;;; Gate-integrity self-tests — anti-tautology guards (must stay green).
;;
;; Background: a rejected impl change made the renderer insert the
;; embedded mockup fixture FILE under GUI + canonical-width + unfiltered
;; conditions — exactly this harness's render conditions — turning the
;; byte-precise comparison into fixture-vs-fixture.  These tests prove
;; the guards trip on that entire class of shim.

(ert-deftest org-air-viewport-guard-traps-named-shim ()
  "A render shim function defined by the implementation errors under the gate."
  (skip-unless (locate-library "org-air"))
  (let ((shim 'org-air-view--maybe-insert-mockup-fixture))
    (should (memq shim org-air-viewport-test--render-shims))
    (unwind-protect
        (progn
          ;; Simulate the rejected impl change defining the shim.
          (unless (fboundp shim) (fset shim (lambda (&rest _) t)))
          (org-air-viewport-test--with-render-guards
            (should-error (funcall shim nil 120))))
      (fmakunbound shim))))

(ert-deftest org-air-viewport-guard-traps-fixture-read-in-render ()
  "Any render-path read of a mockup fixture file errors under the gate."
  (let ((file (expand-file-name "layout-mockup-120.txt"
                                org-air-test-fixture-dir)))
    (org-air-viewport-test--with-render-guards
      ;; A renderer (or anything inside the guarded render) reading the
      ;; fixture must die — regardless of what function name it hides in.
      (should-error (with-temp-buffer (insert-file-contents file)))
      ;; Reads of ordinary org fixtures stay functional.
      (should (with-temp-buffer
                (insert-file-contents (org-air-test-fixture "inbox.org"))
                (> (buffer-size) 0))))
    ;; The harness's own loader still works outside/with the allowance.
    (should (org-air-viewport-test-mockup-lines 120))))

(ert-deftest org-air-viewport-guard-src-never-references-fixtures ()
  "No org-air source file references the mockup fixtures.
The renderer must not even know the gate's expected bytes exist."
  (let ((root (locate-dominating-file org-air-test-fixture-dir "Makefile")))
    (should root)
    (dolist (src (directory-files root t "\\`org-air.*\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents src)
        (goto-char (point-min))
        (when (re-search-forward "layout-mockup\\|tests/fixtures" nil t)
          (ert-fail (format "%s references the gate fixtures (line %d)"
                            (file-name-nondirectory src)
                            (line-number-at-pos))))))))

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

;; org-air-viewport-no-trailing-whitespace: RETIRED 2026-06-12 as
;; §9.1-obsolete.  Spec rev orwonzvz (design §1.3/§3) makes plain-space
;; padding to the full target width the rendering contract — every
;; composed line legitimately ends in spaces, and the gate compares
;; right-trimmed.  Drift protection is covered by the exact-width
;; assertions above and the byte-precise mockup comparison.

(ert-deftest org-air-viewport-calendar-present-with-items ()
  "Calendar pane is rendered on the populated dashboard.
Spec: calendar ALWAYS rendered (air/v0.2/org-air-layout.org, Goals)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 120
    (should (org-air-viewport-test-calendar-present-p))))

(ert-deftest org-air-viewport-render-honours-height-seam ()
  "The renderer consults `org-air-view-height': different heights,
different composition (S6 full-height contract — body band fill-padded,
stacked: plain-space rows / two-pane: divider-framed rows, footer pinned
to the last row)."
  (skip-unless (locate-library "org-air"))
  (let (natural tall)
    (org-air-viewport-test-with-dashboard 120
      (setq natural (substring-no-properties (buffer-string))))
    (org-air-viewport-test-with-dashboard (cons 120 50)
      (setq tall (substring-no-properties (buffer-string))))
    (should-not (equal natural tall))))

(ert-deftest org-air-viewport-fills-height ()
  "At 120×50 (blessed fill-branch height) the buffer composes to exactly
50 rows, every non-blank row 120 cols (S6 contract)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard (cons 120 50)
    (org-air-viewport-test-assert-fills-height 50)
    (org-air-viewport-test-assert-aligned 120)))

(ert-deftest org-air-viewport-fills-height-empty-board ()
  "The SPARSE board fills the tall window: empty board at 120×50 is
exactly 50 rows — the user's actual complaint surface (design note:
~30-line empty board + ~15 framed fill rows + pinned footer)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-empty-dashboard (cons 120 50)
    (org-air-viewport-test-assert-fills-height 50)
    (org-air-viewport-test-assert-aligned 120)))

(ert-deftest org-air-viewport-calendar-present-zero-items ()
  "Calendar pane is rendered even when there are NO items at all.
Spec: calendar ALWAYS rendered, regardless of item counts or filters;
the fully-empty view keeps calendar + summary intact."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-empty-dashboard 120
    (should (org-air-viewport-test-calendar-present-p))))

(provide 'org-air-viewport-test)
;;; org-air-viewport-test.el ends here
