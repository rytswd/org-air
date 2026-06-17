;;; org-air-round13-test.el --- round-13 grind suite for org-air -*- lexical-binding: t; -*-

;;; Commentary:
;; Spec-true grinds for v0.4 round-13 (air/v0.4/org-air-round13-design.org).
;; Written against the FROZEN design contracts, never the current impl, so
;; they double as the impl punch list while red.
;;
;;   D-P1  a TRULY solid divider: the pill svg is clamped to the font line
;;         height with an integer baseline ascent (org-air-view--svg-line-
;;         image) so pill rows never grow taller than a plain line; at
;;         org-air-line-spacing 0 the `│' glyph then fills EVERY body row →
;;         one unbroken divider column.  (The pixel "no row growth" needs a
;;         real GUI frame; here we assert the byte consequence — a
;;         contiguous full-height divider — plus the clamp mechanism.)
;;   D-P2  priority → a fixed 2-col slot on EVERY item row (`■ ' for a shown
;;         priority, `␣␣' otherwise) so titles left-align whether or not an
;;         item is prioritised; `■' is the priority-square glyph (TTY `#').
;;   D-P3  responsive: below `org-air-rail-min-width' (default 90) the view
;;         renders BOARD-ONLY — no rail / calendar / inspector, the item
;;         pane fills the whole window; the rail returns when wide again.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)

;;;; ---------------------------------------------------------------------
;;;; D-P1 — solid full-height divider + line-height-clamped svg.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r13-divider-contiguous-full-height ()
  "D-P1: in a two-pane render the `│' divider is ONE unbroken column down
the whole body — every body row that carries a divider shares the same
column and the run has no gaps (the round-12 regression drew a dashed
divider because pill rows grew taller than the font line).  Asserts the
byte consequence; the pixel `no row growth' needs a real GUI frame."
  (skip-unless (locate-library "org-air"))
  (dolist (width '(120 160))
    (ert-info ((format "width %d" width))
      (org-air-viewport-test-as-gui
        (org-air-viewport-test-with-dashboard width
          (let* ((positions (org-air-viewport-test-divider-positions))
                 (run (org-air-viewport-test-divider-run)))
            (should positions)
            (should run)
            ;; The longest unbroken run covers EVERY divider line (no gap
            ;; splits the column) and they all share one display column.
            (should (= (nth 2 run) (length positions)))
            (should (= 1 (length (delete-dups
                                  (mapcar #'cdr positions)))))))))))

(ert-deftest org-air-r13-svg-line-height-clamp-mechanism ()
  "D-P1.A: every org-air svg overlay goes through the shared
`org-air-view--svg-line-image' wrapper, which displays the image at an
INTEGER baseline `:ascent' (not `:ascent center') sized to the font line
height, so no overlay grows its row.  Asserts the clamp helper exists
(the pixel effect itself needs a GUI frame)."
  (skip-unless (locate-library "org-air"))
  (should (fboundp 'org-air-view--svg-line-image)))

;;;; ---------------------------------------------------------------------
;;;; D-P2 — fixed 2-col priority slot on every item row.
;;;; ---------------------------------------------------------------------

(defun org-air-r13--row-for-title (title)
  "Return the rendered item-pane line (pre-divider) containing TITLE, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward title nil t)
      (let ((bol (line-beginning-position)) (eol (line-end-position)))
        (car (split-string (buffer-substring-no-properties bol eol)
                            "[│|]"))))))

(ert-deftest org-air-r13-priority-square-glyph ()
  "D-P2: the priority-square glyph exists — GUI `■', pure-ASCII TTY `#'."
  (skip-unless (locate-library "org-air"))
  (should (assq 'priority-square org-air-glyphs))
  (org-air-viewport-test-as-gui
    (should (equal (org-air-layout-glyph 'priority-square) "■")))
  (should (equal (org-air-layout-glyph 'priority-square) "#")))

(ert-deftest org-air-r13-priority-slot-fixed-two-col ()
  "D-P2: the priority occupies a FIXED 2-col slot on every item row — `■ '
for a shown (A) priority, two blanks otherwise — so the title starts at
the SAME column on a prioritised vs a non-prioritised TODO row (today the
slot only existed on priority rows, so titles jumped).  Rendered wide so
nothing truncates."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 160
      (let* ((sq (org-air-layout-glyph 'priority-square))
             ;; A shown-priority (#A) TODO row and a plain TODO row.
             (prio (org-air-r13--row-for-title "Ship quarterly report"))
             (plain (org-air-r13--row-for-title "Chase missing invoice")))
        (should prio)
        (should plain)
        ;; The priority row carries the square in its slot after `TODO '.
        (should (string-match-p (concat "TODO " (regexp-quote sq) " ") prio))
        ;; The plain row reserves the slot as two blanks: `TODO ' + `  '
        ;; (keyword space + 2-col blank slot = 3 spaces before the title).
        (should (string-match-p "TODO   [A-Za-z]" plain))
        ;; …and it does NOT carry the square (the slot is blank there).
        (should-not (string-match-p (regexp-quote sq) plain))
        ;; Titles start at the SAME column on both rows (slot is uniform).
        (let ((pcol (string-match "Ship quarterly report" prio))
              (ccol (string-match "Chase missing invoice" plain)))
          (should pcol)
          (should ccol)
          (should (= pcol ccol)))))))

;;;; ---------------------------------------------------------------------
;;;; D-P3 — responsive board-only orientation below the rail min width.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r13-board-only-below-rail-min-width ()
  "D-P3: below `org-air-rail-min-width' (default 90) the view renders
BOARD-ONLY — no rail divider, no calendar, no inspector — and the item
pane fills the whole window; at a wide width the rail returns."
  (skip-unless (locate-library "org-air"))
  (should (boundp 'org-air-rail-min-width))
  ;; Narrow (70 < 90): board-only.
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 70
      (should-not (org-air-viewport-test-divider-run))
      (should-not (org-air-viewport-test-calendar-present-p))
      (let ((text (buffer-string)))
        (should-not (string-match-p "Inspector" text))
        ;; The board itself is intact: the section headers are present
        ;; (item titles truncate at this narrow width, so assert sections).
        (should (string-match-p "Inbox" text))
        (should (string-match-p "Needs attention" text))
        (should (string-match-p "High priority" text)))))
  ;; Wide (140 >= 90): the rail (divider + calendar) returns.
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 140
      (should (org-air-viewport-test-divider-run))
      (should (org-air-viewport-test-calendar-present-p)))))

(ert-deftest org-air-r13-board-only-byte-mockup ()
  "D-P3 [byte]: the width-70 board-only render equals its regenerated
fixture (layout-mockup-70.txt), right-trimmed — the blessed board-only
surface (no rail columns at all)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 70
      (org-air-viewport-test-assert-matches-mockup 70))))

(provide 'org-air-round13-test)
;;; org-air-round13-test.el ends here
