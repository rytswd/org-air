;;; org-air-data-variation-test.el --- anti-hardcoding data-variation suite -*- lexical-binding: t; -*-

;;; Commentary:
;; Guard suite against fixture-data hardcoding in core (fidelity review
;; reviews/v0.2-fidelity-review.org, findings F1/F2: calendar mark date
;; literals and ~20 item-title branches reproduced the mockup bytes for
;; the canonical fixtures while being wrong for any other data).
;;
;; Every test renders a SECOND board generated at test time from
;; `org-air-viewport-test-alt-items', and asserts properties computed
;; independently from that same data constant — so the renderer can only
;; pass by computing them too.  Calendar ground truth is also asserted
;; for the ORIGINAL fixture set, where F1's literals actively diverge
;; (real June days 8/10/12/15/16/17/18/20 vs rendered 12/15/16/17/19/20).
;;
;; The alt board's dates are chosen adversarially: Jun 5 (10d overdue)
;; and Jun 25 (10d out) fall outside F1's [-6..+3] age window, and no
;; item exists on Jun 19 (F1's fabricated mark).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)

(when (locate-library "org-air")
  (require 'org-air))

(ert-deftest org-air-data-variation-calendar-marks-true-union ()
  "Calendar ● days on the alt board equal the data-derived June union.
Spec §4.1: the calendar marks every day with a scheduled/deadline item
— no fabricated marks, no dropped marks; today is ▮, not ●."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-alt-dashboard 120
      (pcase-let ((`(,marked . ,today) (org-air-viewport-test-calendar-marks)))
        (should (equal marked (org-air-viewport-test-alt-june-days)))
        (should (equal today '(15)))))))

(defun org-air-data-variation--fixture-june-days ()
  "Sorted union of June 2026 SCHEDULED/DEADLINE days in the canonical fixtures.
Parsed straight from the fixture files — the ground truth is the data,
not anyone's transcription of it (the fidelity review's hand-derived
list missed the Jun 19 deadline on \"Review design doc\")."
  (let (days)
    (dolist (file (org-air-test-fixture-files))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward
                "\\(?:SCHEDULED\\|DEADLINE\\): *<2026-06-\\([0-9][0-9]\\)" nil t)
          (cl-pushnew (string-to-number (match-string 1)) days))))
    (sort days #'<)))

(ert-deftest org-air-data-variation-calendar-marks-original-fixtures ()
  "Calendar ● days on the ORIGINAL fixtures equal their true June union.
Ground truth parsed from the fixture files themselves; today (15) shows
▮, every other scheduled/deadline June day shows ●, nothing else does."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 120
      (pcase-let ((`(,marked . ,today) (org-air-viewport-test-calendar-marks)))
        (should (equal marked
                       (remove 15 (org-air-data-variation--fixture-june-days))))
        (should (equal today '(15)))))))

(ert-deftest org-air-data-variation-titles-render ()
  "Every alt-board title renders; origins are the real generated files.
F2's per-title branches are dead for unknown data — the general path
must carry any board, and no canonical-fixture origin may appear."
  (skip-unless (locate-library "org-air"))
  ;; Lift section truncation so every item row is visible.
  (let ((org-air-section-max 100))
   (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-alt-dashboard 120
      (let ((text (buffer-string)))
        (dolist (title (org-air-viewport-test-alt-titles t))
          (should (string-match-p (regexp-quote title) text)))
        (dolist (file '("alpha.org" "beta.org" "inbox-alt.org"))
          (should (string-match-p (regexp-quote file) text)))
        ;; No leakage of canonical-fixture origins (F2's forced
        ;; "⌂ projects.org" override).
        (should-not (string-match-p "projects\\.org" text)))))))

(ert-deftest org-air-data-variation-summary-total ()
  "The summary total on the alt board equals the data-derived item count."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-alt-dashboard 120
      (should (string-match-p
               (format "%d\\s-+total" (length org-air-viewport-test-alt-items))
               (buffer-string))))))

(ert-deftest org-air-data-variation-geometry-holds ()
  "Composition geometry is data-independent: exact width + locked divider
on a board the renderer has never seen."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-alt-dashboard 120
      (org-air-viewport-test-assert-aligned 120)
      (let ((run (org-air-viewport-test-divider-run)))
        (should run)
        (should (= (nth 0 run) 86))))))

(ert-deftest org-air-data-variation-tty-glyph-uniformity ()
  "TTY renders contain no GUI-only glyphs on either board.
F2a forced one ⌂ origin and U+2026 truncations into TTY output where
siblings use H and \"...\" — glyph choice must be uniform per display."
  (skip-unless (locate-library "org-air"))
  ;; display-graphic-p is genuinely nil in --batch: real TTY path.
  (dolist (glyph '("⌂" "…"))
    (org-air-viewport-test-with-dashboard 120
      (should-not (string-match-p (regexp-quote glyph) (buffer-string))))
    (org-air-viewport-test-with-alt-dashboard 120
      (should-not (string-match-p (regexp-quote glyph) (buffer-string))))))

(provide 'org-air-data-variation-test)
;;; org-air-data-variation-test.el ends here
