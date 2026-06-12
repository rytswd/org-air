;;; org-air-layout-test.el --- spec-true v0.2 layout tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Phase-2 suite: mockup-true assertions from the authoritative v0.2
;; layout spec, air/v0.2/org-air-layout-design.org (#+state: ready).
;; Encodes the §9 Test Plan: breakpoint geometry, the unbroken pane
;; divider, the always-present calendar (populated / empty / filtered
;; boards), the full-shape empty board, summary integrity, rail face
;; application, and TTY glyph fallback.
;;
;; All renders run in --batch where `display-graphic-p' is nil, so the
;; expected glyphs are the TTY fallbacks of design §6.1 (divider "|",
;; rules "-"); geometry helpers accept either form where presence (not
;; degradation) is being asserted.
;;
;; Tests the current implementation does not satisfy are listed in
;; tests/org-air-known-failures.el — the grind punch list for impl.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)

(when (locate-library "org-air")
  (require 'org-air))

;;;; §9.1 Geometry at each breakpoint — column-locked per the §3 mockups.
;;
;; Pane arithmetic from design §3 + §1.2 (divider is " │ ", one space
;; of breathing room either side, so the glyph sits at ITEM-PANE-WIDTH
;; + 1, zero-indexed):
;;   160 = 115 (items) + 3 + 42 (rail-wide)  -> divider column 116
;;   120 =  85 (items) + 3 + 32 (rail)       -> divider column  86
;;   100 =  65 (items) + 3 + 32 (rail)       -> divider column  66

(defconst org-air-layout-test--divider-columns
  '((100 . 66) (120 . 86) (160 . 116))
  "Spec-locked (WIDTH . DIVIDER-COLUMN) pairs from design §3/§1.4.")

(defun org-air-layout-test--locked-column (width)
  "Return the §3-locked divider column for WIDTH."
  (cdr (assq width org-air-layout-test--divider-columns)))

(ert-deftest org-air-layout-divider-unbroken-120 ()
  "At 120 cols (two-pane) the divider sits at one column on every body row.
Spec §1.2/§9.1: a single vertical rule runs the full height of the
body band; the zipped rail is at least calendar + summary tall."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 120
    (let ((run (org-air-viewport-test-divider-run)))
      (should run)
      (should (>= (nth 2 run) 10))
      ;; Column-locked: items 85 + " │ " + rail 32 (§3.2).
      (should (= (nth 0 run) (org-air-layout-test--locked-column 120))))))

(ert-deftest org-air-layout-divider-unbroken-160 ()
  "At 160 cols the divider runs unbroken at the §3.1-locked column.
Spec §1.4/§3.1: item pane 115 + rail `org-air-rail-width-wide' (42)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 160
    (let ((run (org-air-viewport-test-divider-run)))
      (should run)
      (should (>= (nth 2 run) 10))
      (should (= (nth 0 run) (org-air-layout-test--locked-column 160))))))

(ert-deftest org-air-layout-two-pane-at-threshold ()
  "At exactly `org-air-layout-two-pane-min' (default 100) the view is
two-pane: an unbroken divider run is present at the locked column
(items 65 + " │ " + rail 32).  Spec §1.4."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 100
    (let ((run (org-air-viewport-test-divider-run)))
      (should run)
      (should (>= (nth 2 run) 10))
      (should (= (nth 0 run) (org-air-layout-test--locked-column 100))))))

(ert-deftest org-air-layout-stacked-below-threshold ()
  "One column below the threshold (99) the view stacks: no body-height
divider run, yet the calendar and every section remain.  Spec §1.4/§3.3:
the rail relocates to a top band; the item sections run full width."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 99
    (let ((run (org-air-viewport-test-divider-run)))
      (should (or (null run) (< (nth 2 run) 5))))
    (should (org-air-viewport-test-calendar-present-p))
    (let ((text (buffer-string)))
      (pcase-dolist (`(,_bucket . ,title)
                     org-air-viewport-test-section-titles)
        (should (string-match-p (regexp-quote title) text))))))

;;;; §9.2 Calendar always present.

(ert-deftest org-air-layout-calendar-survives-all-hiding-filter ()
  "The calendar renders even when a tag filter hides EVERY item.
Spec §4.1: the calendar is the rail's anchor and never omitted \u2014 not
under a filter that hides every item."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 120
    (org-air-filter '("org-air-no-such-tag"))
    (should (= (or (org-air-viewport-test-banner-item-count) -1) 0))
    (should (org-air-viewport-test-calendar-present-p))))

;;;; §9.3 Empty board holds shape.

(ert-deftest org-air-layout-empty-board-holds-shape ()
  "With zero items the layout keeps its full skeleton.  Spec §5.2:
five section headings with 0 badges, the per-section empty placeholder
lines, a summary with a total row, the filters placeholder, and the
calendar grid all render."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-empty-dashboard 120
    (let ((text (buffer-string)))
      ;; Section skeleton: every heading present, every badge zero.
      (pcase-dolist (`(,_bucket . ,title)
                     org-air-viewport-test-section-titles)
        (should (string-match-p (regexp-quote title) text)))
      (let ((counts (org-air-viewport-test-section-counts)))
        (should (= (length counts) 5))
        (pcase-dolist (`(,bucket . ,count) counts)
          (should (equal (cons bucket count) (cons bucket 0)))))
      ;; Kind-specific empty placeholders (spec §5.1, v0.1 §7 wording).
      (should (string-match-p "Inbox zero — nothing to process\\." text))
      (should (string-match-p "Nothing overdue\\.\\s-+Nice\\.\\|Nothing overdue\\. Nice\\." text))
      ;; Rail: summary total, filters placeholder, calendar grid.
      (should (string-match-p "0\\s-+total" text))
      (should (string-match-p "No filters · all items" text))
      (should (org-air-viewport-test-calendar-present-p)))))

;;;; §9.4 Summary integrity.

(ert-deftest org-air-layout-summary-counts-match-sections ()
  "Summary rows mirror the per-section count badges; the total equals
the banner's visible-item count.  Spec §4.2/§9.4."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 120
    (let ((text (buffer-string))
          (counts (org-air-viewport-test-section-counts)))
      (should (= (length counts) 5))
      (pcase-dolist (`(,bucket . ,count) counts)
        (let ((title (cdr (assq bucket org-air-viewport-test-section-titles))))
          ;; Summary row: right-aligned NUMBER then the bucket label.
          (should (string-match-p (format "%d\\s-+%s" count title) text))))
      (let ((banner (org-air-viewport-test-banner-item-count)))
        (should banner)
        (should (string-match-p (format "%d\\s-+total" banner) text))))))

;;;; §9.5 Face application — integrated render, not byte-compile faith.

(ert-deftest org-air-layout-rail-faces-applied ()
  "The new v0.2 pane faces are actually applied in a real render:
pane border on the divider, rail titles on block labels, summary
number faces on the counts.  Spec §7/§9.5."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 120
    (should (org-air-viewport-test-face-applied-p 'org-air-face-pane-border))
    (should (org-air-viewport-test-face-applied-p 'org-air-face-rail-title))
    (should (org-air-viewport-test-face-applied-p 'org-air-face-summary-number))))

;;;; §9.6 TTY fallback — no GUI-only glyph leaks in --batch.

(ert-deftest org-air-layout-tty-no-gui-glyph-leak ()
  "In a TTY render (--batch: `display-graphic-p' is nil) no GUI box or
rule glyph leaks into the buffer.  Spec §6.1: divider degrades to |,
rules to -, box corners/tees to +, calendar pager to < >."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 120
    (let ((text (buffer-string)))
      (dolist (glyph '("│" "─" "┌" "┐" "└" "┘" "├" "┤" "‹" "›"))
        (should-not (string-match-p (regexp-quote glyph) text))))))

(provide 'org-air-layout-test)
;;; org-air-layout-test.el ends here
