;;; org-air-round7-test.el --- round-7 spec-true contracts -*- lexical-binding: t; -*-

;;; Commentary:
;; Spec-true tests for v0.3 round-7 (design vlpzyquw, #ready), encoded
;; against the FINAL contracts before impl2 builds the byte renderer.
;; The face-only bits (today-bg, day-header) already land via
;; org-air-faces.el, so those pass now; the BYTE/behaviour contracts
;; (R10 right-cluster, R7 cell+legend, R8 Sunday-first, R4 footer, R3
;; keys, R6 day view) fail until impl2 implements them and are listed in
;; tests/org-air-known-failures.el as a GRIND punch list — each flips to
;; passed-unexpectedly when implemented; delete its manifest entry to
;; close it out (the same contract every round).
;;
;; Round-7 also INVALIDATES some round-6-shaped tests (legend ■today,
;; the inline-placement test, Monday-first weekday).  Those are updated
;; on impl2's settled tip at closeout, not here.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)

(when (locate-library "org-air")
  (require 'org-air))

(defun org-air-r7-test--leftpane (title)
  "Return the rendered item row's LEFT pane (pre-divider) for TITLE."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward title nil t)
      (string-trim-right
       (car (split-string
             (buffer-substring-no-properties
              (line-beginning-position) (line-end-position))
             "[│|]"))))))

(defun org-air-r7-test--item-prop-runs ()
  "Count distinct `org-air-item' runs (item rows) in the buffer."
  (let ((n 0) (pos (point-min)))
    (while (< pos (point-max))
      (when (get-text-property pos 'org-air-item) (setq n (1+ n)))
      (setq pos (or (next-single-property-change pos 'org-air-item)
                    (point-max))))
    n))

;;;; R7 — today is a BACKGROUND highlight, no ■ glyph, legend drops today.

(ert-deftest org-air-r7-today-face-has-background ()
  "R7 [face]: `org-air-face-calendar-today' gains a today-highlight
background (its defface spec has a :background colour clause) + bold,
with a TTY :inverse-video fallback.  Face-only — lands via
org-air-faces.el."
  (skip-unless (locate-library "org-air"))
  (should (facep 'org-air-face-calendar-today))
  (let ((flat (flatten-tree (get 'org-air-face-calendar-today
                                 'face-defface-spec))))
    (should (memq :background flat))
    (should (memq :inverse-video flat))))

(ert-deftest org-air-r7-today-cell-no-glyph ()
  "R7 [byte]: the today cell prints the day number on the highlight with
NO ■ marker after it (15■ -> 15 on a filled cell).  The today cell is
found by its `org-air-face-calendar-today' face."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 120
      (let ((today (org-air-viewport-test--calendar-today-days)))
        (should today)
        (goto-char (point-min))
        (let ((checked nil))
          (while (re-search-forward "[0-9]+" nil t)
            (when (memq 'org-air-face-calendar-today
                        (org-air-viewport-test--face-list-at
                         (match-beginning 0)))
              (setq checked t)
              (should-not (eq (char-after (match-end 0)) ?■))))
          (should checked))))))

(ert-deftest org-air-r7-legend-drops-today ()
  "R7 [byte]: the calendar legend no longer names today (the background
highlight is self-evident): wide ◆ due  ● sched  · created / narrow
◆ due  ● sched (D5c glyph-spaced), with NO ■today token."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (dolist (width '(100 120))
      (org-air-viewport-test-with-dashboard width
        (let ((text (buffer-string))
              (today (org-air-viewport-test--calendar-today-glyph 'gui)))
          (should (string-match-p "◆ due    ● sched" text))
          (should-not (string-match-p (concat (regexp-quote today) "today")
                                      text)))))))

;;;; R8 — calendar starts Sunday.

(ert-deftest org-air-r8-calendar-sunday-first ()
  "R8 [byte]: the calendar starts on SUNDAY by default — the weekday
header reads `Su Mo Tu We Th Fr Sa' and `org-air-calendar-week-start'
defaults to 0 (users can set 1 for Monday)."
  (skip-unless (locate-library "org-air"))
  (should (= org-air-calendar-week-start 0))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 120
      (should (string-match-p "Su +Mo +Tu +We +Th +Fr +Sa" (buffer-string))))))

;;;; R4 — bottom footer band removed; hints live in the rail.

(ert-deftest org-air-r4-footer-band-removed ()
  "R4 [byte]: the bottom `[c]apture [g]refresh …' footer band is gone
(`org-air-show-footer' defaults nil); the verbs live in the rail's D5
Actions block (c capture / filter  s scope ...).  Rendered wide (160) so
the mid-tier elision (D5f narrow tiers truncate with …) does not clip
`s scope'."
  (skip-unless (locate-library "org-air"))
  (should (boundp 'org-air-show-footer))
  (should (null org-air-show-footer))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 160
      (let ((text (buffer-string)))
        ;; The bottom band's bracketed hints are gone…
        (should-not (string-match-p "\\[c\\]apture" text))
        ;; …but the rail still carries the verbs (D5 Actions block).
        (should (string-match-p "c capture" text))
        (should (string-match-p "s scope" text))))))

;;;; R10 — title left & clean; one right-aligned [date] <=2 tags origin.

(ert-deftest org-air-r10-tags-inline-max-is-2 ()
  "R10 [byte]: the inline tag cap drops to 2 (a quiet cluster, like the
single origin filename — not a column of 4–5)."
  (skip-unless (locate-library "org-air"))
  (should (= org-air-tags-inline-max 2)))

(ert-deftest org-air-r10-item-row-right-cluster ()
  "R10 [byte]: the title is LEFT & clean; a SINGLE right-aligned cluster
holds [date] <=2 tags> ⌂origin as one unit (the date JOINS the cluster
— not round-6's inline-after-title, not round-5's separate boxed
column).  One flex gap between the clean title and the right cluster;
origin flush-right (D2)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 120
      (let ((lp (org-air-r7-test--leftpane "Chase missing invoice")))
        (should lp)
        ;; Round-6's inline `<date> <tags>' crammed after the title is GONE:
        ;; the title is followed by clean flex space, then the cluster.
        (should-not (string-match-p "invoice  OVERDUE 7d #projects" lp))
        ;; Right cluster, as one unit at the right edge: date, then tags,
        ;; then the flush-right origin.
        (should (string-match-p
                 "OVERDUE 7d .*#projects.*⌂ projects\\.org\\'" lp))
        ;; The title is clean: a wide flex gap follows it (not crammed).
        (should (string-match-p "Chase missing invoice  +" lp))))))

;;;; R3 — j/k scroll; k is no longer the kill key.

(ert-deftest org-air-r3-k-not-kill-jk-scroll ()
  "R3 [keys]: k must NOT delete (it was the dangerous kill disposition);
j/k become scroll motions (down/up).  Kill moves off the motion key."
  (skip-unless (locate-library "org-air"))
  (let ((j (lookup-key org-air-view-mode-map (kbd "j")))
        (k (lookup-key org-air-view-mode-map (kbd "k"))))
    ;; The safety fix: k is not the kill command.
    (should-not (eq k 'org-air-item-kill))
    ;; j and k are bound to scroll/motion commands.
    (should (commandp j))
    (should (commandp k))
    (should (string-match-p "scroll\\|line\\|next\\|prev\\|down\\|up"
                            (symbol-name (if (symbolp j) j 'ignore))))
    (should (string-match-p "scroll\\|line\\|next\\|prev\\|down\\|up"
                            (symbol-name (if (symbolp k) k 'ignore))))))

;;;; R5 — scope actually narrows the board and the rail reflects it.

(ert-deftest org-air-r5-scope-narrows-and-rail-reflects ()
  "R5 [bug]: scoping narrows the board (fewer item rows) and the rail
shows the active scope label."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 120
    (let ((unscoped (org-air-r7-test--item-prop-runs)))
      (org-air-scope "#work")
      (let ((scoped (org-air-r7-test--item-prop-runs))
            (text (buffer-string)))
        (should (< scoped unscoped))
        (should (string-match-p "Scope:.*work\\|#work" text))))))

;;;; R2 — fill ends exactly at the derived height (no +1 overflow row).
;;
;; The user-visible +1 is a LIVE-GUI phenomenon: in a real frame
;; `window-body-height' can over-count the usable rows by one against the
;; mode-line, so the derivation (org-air-layout-current-height) hands the
;; fill one row too many.  That derivation is not reproducible in --batch
;; (no live window; the mock below shows the FILL itself is exact).  So
;; this is a deterministic guard on the FILL CONTRACT — given a derived
;; height H, the body composes to EXACTLY H rows, never H+1 — which locks
;; out any fill-side regression once impl2 corrects the derivation.  The
;; live-window derivation itself needs a GUI screenshot to confirm (R2
;; GUI-verify), noted for the user's next pass.

(ert-deftest org-air-r2-fill-is-exactly-derived-height ()
  "R2: with the height DERIVED (org-air-view-height nil), the body fills
to EXACTLY the derived height, never one row past it.  The derivation
seam `org-air-layout-current-height' is mocked so the assertion is
deterministic; heights are chosen above the natural content so the fill
pads (not truncates)."
  (skip-unless (locate-library "org-air"))
  (dolist (h '(44 50 60))
    (cl-letf (((symbol-function 'org-air-layout-current-height)
               (lambda (&optional _b) h)))
      (org-air-viewport-test-as-gui
        (org-air-test-with-fixtures
          (org-air-viewport-test--with-frozen-now
            (org-air-viewport-test--with-render-guards
              (let ((org-air-view-width 120) (org-air-view-height nil))
                (org-air)
                (unwind-protect
                    (with-current-buffer "*org-air*"
                      (let ((rows (length (split-string (buffer-string) "\n"))))
                        (ert-info ((format "derived height %d" h))
                          ;; Exactly H rows: no trailing +1 overflow row.
                          (should (= rows h)))))
                  (when (get-buffer "*org-air*")
                    (kill-buffer "*org-air*")))))))))))

;;;; R6 — single-day focus view + clickable/keyable calendar cells.

(ert-deftest org-air-r6-day-header-face ()
  "R6 [face]: the new `org-air-face-day-header' exists for the day-view
title (inherits the strong face)."
  (skip-unless (locate-library "org-air"))
  (should (facep 'org-air-face-day-header)))

(ert-deftest org-air-r6-day-view-command-exists ()
  "R6 [interaction]: `org-air-view-day' opens the single-day focus view
for a given date (a scoped render reusing the R10 item line)."
  (skip-unless (locate-library "org-air"))
  (should (fboundp 'org-air-view-day))
  (should (commandp 'org-air-view-day)))

(ert-deftest org-air-r6-calendar-cells-clickable ()
  "R6 [interaction]: each calendar day cell carries a `mouse-face' and a
keymap so mouse-1 / RET focuses that day (org-air-view-day)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 120
      ;; Find a calendar day-number cell carrying a date mark and assert it
      ;; has a mouse-face + keymap (focusable).
      (let ((found nil))
        (save-excursion
          (goto-char (point-min))
          (while (and (not found) (re-search-forward "[0-9]+" nil t))
            (let ((p (match-beginning 0)))
              (when (and (memq 'org-air-face-calendar-today
                               (org-air-viewport-test--face-list-at p))
                         (get-text-property p 'mouse-face)
                         (get-text-property p 'keymap))
                (setq found t)))))
        (should found)))))

(provide 'org-air-round7-test)
;;; org-air-round7-test.el ends here
