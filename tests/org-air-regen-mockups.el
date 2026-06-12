;;; org-air-regen-mockups.el --- regenerate mockup fixtures from the honest renderer -*- lexical-binding: t; -*-

;;; Commentary:
;; One-shot regeneration of the byte-precise layout fixtures from the
;; REAL renderer (run via `make regen-mockups').  For every width in
;; `org-air-regen-widths' this renders the canonical fixture board with
;; the gate's exact conditions — frozen clock (Mon 15 Jun 2026), GUI
;; glyphs, unfiltered, anti-tautology render guards ACTIVE so a shim can
;; never write the fixtures from themselves — and writes the
;; right-trimmed lines to tests/fixtures/layout-mockup-WIDTH.txt.
;;
;; Canonical widths (80/120/160) are the design §3 contract; the
;; threshold widths bracket the responsive breakpoint (~95 with
;; hysteresis 3 and rail tiers 28/32/42 per impl2's D1).
;;
;; After regeneration: diff against design's §3 expectations and route
;; the diff to design for re-blessing before any gate verdict.

;;; Code:

(require 'cl-lib)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)
(require 'org-air)

(defconst org-air-regen-widths '(80 90 96 100 104 110 120 160)
  "Widths to regenerate: canonical 80/120/160 + breakpoint bracket.")

(defun org-air-regen--write (width)
  "Render the canonical board at WIDTH and write its mockup fixture."
  (let ((out (expand-file-name (format "layout-mockup-%d.txt" width)
                               org-air-test-fixture-dir)))
    (org-air-viewport-test-as-gui
      (org-air-viewport-test-with-dashboard width
        (let ((lines (org-air-viewport-test--drop-trailing-blanks
                      (mapcar (lambda (line)
                                (string-trim-right
                                 (substring-no-properties line)))
                              (org-air-viewport-test-lines)))))
          (with-temp-file out
            (insert (mapconcat #'identity lines "\n") "\n"))
          (message "regen: %s (%d lines, max width %d)"
                   (file-name-nondirectory out)
                   (length lines)
                   (apply #'max (cons 0 (mapcar #'string-width lines)))))))))

(defun org-air-regen-mockups ()
  "Regenerate every mockup fixture from the honest renderer."
  (dolist (width org-air-regen-widths)
    (org-air-regen--write width))
  (message "regen: done — diff fixtures and route to design for re-blessing"))

(provide 'org-air-regen-mockups)
;;; org-air-regen-mockups.el ends here
