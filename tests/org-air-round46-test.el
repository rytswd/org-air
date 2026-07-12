;;; org-air-round46-test.el --- executing ERTs for v0.5 round-46 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.5 round-46 (air/v0.5/org-air-round46-design.org):
;; vertical arrow / j-k navigation intermittently drops point to the
;; LEFT-MOST char (col 0, the indent margin) or the RIGHT-MOST char (EOL)
;; on board rows, instead of holding a stable column down the item-title
;; band.
;;
;; ROOT (R46-1, confirmed): the R22-2/R29-2 title snap
;; `org-air-view--normalize-point' -> `org-air-view--dead-zone-p'
;; UNDER-fired.  Its gate required `org-air-item'/`org-air-doc' SOMEWHERE
;; on the line and guarded only the LEFT side (`(< point title-pos)`), so
;; every non-item row — section headers, the `…and N more' note, the
;; empty-section notes (`Nothing scheduled in the next 7 days.'), the
;; banner, the `─' rule — was excluded entirely (vertical motion left
;; point at col 0), and the RIGHT edge of item rows was unguarded (a high
;; goal column stayed at EOL).
;;
;; R46-2 fix: a UNIVERSAL per-row TITLE-BAND clamp.
;; `org-air-view--row-band' -> (START . END) per row kind (item/doc rows:
;; title .. last visible glyph of the row's own run; section/note/banner
;; rows: the row's first visible glyph; blank rows: nil), applied
;; two-edged by `org-air-view--normalize-point-now' behind the unchanged
;; R29-2 line-motion snapshot gate: col-0/gutter -> band start,
;; trailing-pad/EOL -> band end, in-band -> KEEP (goal column respected),
;; blank rows untouched.
;;
;; These ERTs drive the REAL command loop (pre/post-command hooks,
;; `this-command'/`last-command' bound — the R27-4/R29-2 dispatch
;; pattern) down AND up a realistic fixture board covering every trigger
;; row kind: section headers, the `…and N more' truncation note, the
;; `Nothing scheduled …' empty-section notes, the banner, the `─' rule,
;; and item rows with and without a date cell (pill/no-pill), with a
;; HOSTILE goal column (col 0 — the batch-deterministic LEFT-MOST
;; reproduction — and the row's EOL — the RIGHT-MOST edge).  Every
;; non-blank landing must sit INSIDE its row's visible band — never
;; col 0, never EOL — while a goal column already inside the title band
;; is preserved exactly (never hijacked).  The band is computed from the
;; RAW LINE TEXT (never from `org-air-view--row-band' — no tautology).
;;
;;   org-air-r46-2-native-vertical-nav-stays-in-band —
;;     NATIVE `next-line' / `previous-line' (the arrow commands).
;;   org-air-r46-2-evil-vertical-nav-stays-in-band —
;;     REAL evil from .deps (motion state): `evil-next-line' /
;;     `evil-previous-line' (vanilla evil's own j/k motions).
;;
;; Both FAIL with the R46-2 clamp reverted (the old item/doc-only,
;; left-only dead-zone gate leaves section headers and the notes at
;; col 0), and PASS under the universal band clamp.  Anti-tautology:
;; every walk must visit MORE than 12 non-blank rows, and the trigger
;; rows are asserted PRESENT in the rendered board.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)
(require 'org-air-round27-test)            ; live-board harness (R27-4)

(when (locate-library "org-air")
  (require 'org-air))

;;;; =====================================================================
;;;; Command-loop dispatch (R27-4/R29-2 pattern, goal column real).
;;;; =====================================================================

(defvar org-air-r46--last-cmd nil
  "The previously dispatched command, so goal-column tracking is real.")

(defun org-air-r46--dispatch (buf cmd)
  "Dispatch CMD in BUF with pre/post-command hooks run.
Binds `this-command'/`last-command' the way the command loop would and
records the POST-run `this-command' (evil's `evil-line-move' rewrites it
to `next-line'/`previous-line' — exactly what `line-move' checks for
goal-column persistence), so a hostile `temporary-goal-column' survives
the whole walk as in live use."
  (with-current-buffer buf
    (let ((this-command cmd)
          (last-command org-air-r46--last-cmd))
      (run-hooks 'pre-command-hook)
      (call-interactively cmd)
      (run-hooks 'post-command-hook)
      (setq org-air-r46--last-cmd this-command))))

;;;; =====================================================================
;;;; The visible band, computed from the RAW line text (no tautology).
;;;; =====================================================================

(defun org-air-r46--visible-band ()
  "Return the current line's visible band as buffer positions (S . E).
S is the position of the FIRST non-whitespace char, E of the LAST.
Returns nil for a genuinely blank/whitespace row (col 0 is the only
legitimate landing there — exempt).  Derived from the raw line text
only, NEVER from `org-air-view--row-band', so the assertion cannot
tautologically mirror the implementation."
  (save-excursion
    (let ((bol (line-beginning-position))
          (eol (line-end-position)))
      (goto-char bol)
      (skip-chars-forward " \t" eol)
      (when (< (point) eol)
        (let ((s (point)))
          (goto-char eol)
          (skip-chars-backward " \t" bol)
          (cons s (1- (point))))))))

(defun org-air-r46--row-title-col ()
  "Column of the current row's `org-air-row-title' mark, or nil.
Reads the render-time text property directly (a render artifact, not the
R46 clamp), so the goal-column assertion stays independent of the fix."
  (save-excursion
    (let* ((bol (line-beginning-position))
           (eol (line-end-position))
           (pos (if (get-text-property bol 'org-air-row-title) bol
                  (next-single-property-change bol 'org-air-row-title
                                               nil eol))))
      (when (and pos (get-text-property pos 'org-air-row-title))
        (goto-char pos)
        (current-column)))))

(defun org-air-r46--assert-in-band (buf what &optional goal-col)
  "Assert BUF's point sits INSIDE its row's visible band (R46-2).
Blank rows are exempt (return 0).  On a non-blank row point must be at
or after the first visible glyph (never the col-0 indent margin) and at
or before the last visible glyph (never the trailing pad / EOL).  With
GOAL-COL non-nil, a TITLE-marked row whose title band contains GOAL-COL
must land EXACTLY there — the goal column is respected, never hijacked.
Returns 1 for a validated non-blank landing, 0 for a blank row."
  (with-current-buffer buf
    (let ((band (org-air-r46--visible-band)))
      (if (null band)
          0
        (unless (and (>= (point) (car band)) (<= (point) (cdr band)))
          (ert-fail
           (format "%s: line %d lands col %d OUTSIDE visible band [%d..%d]: %S"
                   what (line-number-at-pos) (current-column)
                   (save-excursion (goto-char (car band)) (current-column))
                   (save-excursion (goto-char (cdr band)) (current-column))
                   (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position)))))
        (when goal-col
          (let ((tcol (org-air-r46--row-title-col))
                (ecol (save-excursion (goto-char (cdr band))
                                      (current-column))))
            (when (and tcol (<= tcol goal-col) (<= goal-col ecol)
                       (/= (current-column) goal-col))
              (ert-fail
               (format "%s: line %d goal col %d is INSIDE the title band [%d..%d] but landing is col %d (goal column hijacked)"
                       what (line-number-at-pos) goal-col tcol ecol
                       (current-column))))))
        1))))

(defun org-air-r46--walk (buf next prev &optional goal-col)
  "Drive NEXT to the last buffer line, then PREV back to the top, in BUF.
Every landing runs `org-air-r46--assert-in-band' (GOAL-COL passed
through).  Fresh goal-column tracking: the FIRST press records point's
current column as the goal, which then persists for the whole walk (the
hostile-goal harness).  Returns the number of non-blank-row landings."
  (let ((landings 0)
        (org-air-r46--last-cmd nil))
    (let ((down (with-current-buffer buf
                  (- (line-number-at-pos (point-max))
                     (line-number-at-pos (point)))))
          (up (with-current-buffer buf
                (1- (line-number-at-pos (point-max))))))
      (dotimes (_ down)
        (org-air-r46--dispatch buf next)
        (cl-incf landings (org-air-r46--assert-in-band buf next goal-col)))
      (dotimes (_ up)
        (org-air-r46--dispatch buf prev)
        (cl-incf landings (org-air-r46--assert-in-band buf prev goal-col))))
    landings))

(defun org-air-r46--assert-trigger-rows ()
  "Assert the current board really renders the R46 trigger rows.
Anti-tautology for the walks: section headers with counts, the `…and N
more' truncation note, item rows WITH a date cell (pill) and WITHOUT
(the inbox rows) must all be present."
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    ;; section headers (the col-0 LEFT-MOST victims).
    (should (string-match-p "Needs attention [0-9]+" text))
    (should (string-match-p "Upcoming [0-9]+" text))
    ;; the truncation note row (`…and N more' — ASCII `...' in batch).
    (should (string-match-p "and [0-9]+ more" text))
    ;; pill rows (a date cell) AND a no-pill row (an inbox row whose
    ;; middle cell is empty — no date, no quiet marker).
    (should (string-match-p "OVERDUE" text))
    (should (string-match-p "Call plumber" text))
    (save-excursion
      (goto-char (point-min))
      (search-forward "Call plumber")
      (let ((line (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))
        (should-not (string-match-p "OVERDUE\\|quiet\\|Today" line))))))

(defun org-air-r46--drive-hostile-walks (buf next prev)
  "Drive the three R46 goal-column walks over BUF with NEXT/PREV.
1. HOSTILE goal col 0 (point forced to the indent margin of the first
   item row) under the DEFAULT `line-move-visual' — on a batch frame the
   pixel goal of every visual line-move resolves to column 0, so this is
   the batch-deterministic LEFT-MOST reproduction on EVERY row.
2. HOSTILE goal EOL (point at the first item row's end-of-line — the
   RIGHT-MOST edge; the trailing pad / EOL escape) with
   `line-move-visual' nil — the TTY logical-column goal path, the only
   one whose goal column is column-true in batch.
3. IN-BAND goal (title col + 2, logical path): additionally asserts
   every title row whose band contains it lands EXACTLY there (goal
   respected — the clamp never hijacks an in-band column).
Each walk must validate MORE than 12 non-blank rows (anti-tautology)."
  ;; --- walk 1: goal column 0 (LEFT-MOST; default visual line-move). ---
  (with-current-buffer buf
    (org-air-view--goto-first-item)
    (beginning-of-line)
    (should (= (current-column) 0)))
  (should (> (org-air-r46--walk buf next prev) 12))
  ;; --- walk 2: goal column EOL (RIGHT-MOST; logical goal column). ---
  (let ((line-move-visual nil))
    (with-current-buffer buf
      (org-air-view--goto-first-item)
      (end-of-line)
      ;; the start really is PAST the row's visible band (a hostile goal).
      (should (> (point) (cdr (org-air-r46--visible-band)))))
    (should (> (org-air-r46--walk buf next prev) 12))
    ;; --- walk 3: goal INSIDE the title band — respected exactly. ---
    (let ((goal (with-current-buffer buf
                  (org-air-view--goto-first-item)
                  (let ((tcol (org-air-r46--row-title-col)))
                    (should tcol)
                    (forward-char 2)
                    (should (= (current-column) (+ tcol 2)))
                    (+ tcol 2)))))
      (should (> (org-air-r46--walk buf next prev goal) 12)))))

(defun org-air-r46--drive-empty-board-walk (buf next prev)
  "Re-render BUF with ZERO items and walk the empty-section notes.
The `Nothing scheduled in the next N days.' / `Nothing overdue. Nice.'
rows carry no item property — the R46-1 LEFT-MOST victims — so a col-0
hostile walk must land on their first visible glyph, never col 0."
  (with-current-buffer buf
    (setq org-air-view--items nil)
    ;; direct render (the harness pattern): `--render-current' would
    ;; re-query on nil items.
    (org-air-view--render nil org-air-view--tag-filter)
    ;; the trigger rows really are on this board (anti-tautology).
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "Nothing scheduled in the next" text))
      (should (string-match-p "Nothing overdue" text)))
    (org-air-view--goto-first-item)
    (beginning-of-line))
  (should (> (org-air-r46--walk buf next prev) 4)))

;;;; =====================================================================
;;;; R46-2 — NATIVE next-line / previous-line (the arrow commands).
;;;; =====================================================================

(ert-deftest org-air-r46-2-native-vertical-nav-stays-in-band ()
  "NATIVE `next-line'/`previous-line' down AND up the live fixture board
\(board-only AND side-window popped) with hostile goal columns (col 0 and
EOL): every non-blank landing sits INSIDE its row's visible band — never
the col-0 indent margin (section headers, the `…and N more' /
`Nothing scheduled …' notes, the banner), never the trailing pad / EOL
\(item-row right edges) — and a goal column already inside the title band
is respected EXACTLY.  Reverting the R46-2 universal band clamp FAILS:
the old item/doc-only dead-zone gate leaves point at col 0 on every
unguarded row."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-board
    ;; --- BOARD-ONLY (the user's popped-rail board shape). ---
    (let ((org-air-rail-min-width 500))
      (org-air-view--refresh-current)
      (should (eq org-air-view--orientation 'board-only))
      (org-air-r46--assert-trigger-rows)
      (org-air-r46--drive-hostile-walks
       (current-buffer) #'next-line #'previous-line))
    ;; --- SIDE-WINDOW (rail popped; board pane narrow). ---
    (org-air-r27--pop-rail)
    (should (eq org-air-view--orientation 'side-window))
    (org-air-r46--drive-hostile-walks
     (current-buffer) #'next-line #'previous-line)
    ;; --- EMPTY board: the `Nothing scheduled …' note rows. ---
    (let ((org-air-rail-min-width 500))
      (setq-local org-air-view--rail-popped-out nil)
      (org-air-rail--hide (current-buffer))
      (org-air-r46--drive-empty-board-walk
       (current-buffer) #'next-line #'previous-line))))

;;;; =====================================================================
;;;; R46-2 — REAL evil (motion state) j/k from .deps.
;;;; =====================================================================

(ert-deftest org-air-r46-2-evil-vertical-nav-stays-in-band ()
  "REAL evil (motion state) `evil-next-line'/`evil-previous-line' —
vanilla evil's own j/k motions, from .deps — down AND up the live fixture
board with hostile goal columns (col 0 and EOL): every non-blank landing
sits INSIDE its row's visible band (never col 0, never EOL) and an
in-band goal column is respected EXACTLY; then the empty board's
`Nothing scheduled …' note rows.  Reverting the R46-2 universal band
clamp FAILS (col-0 collapse on section headers and the notes)."
  (skip-unless (locate-library "org-air"))
  (skip-unless (locate-library "evil"))
  (require 'evil)
  (org-air-r27--with-live-board
    (evil-local-mode 1)
    (should (eq evil-state 'motion))
    ;; --- BOARD-ONLY. ---
    (let ((org-air-rail-min-width 500))
      (org-air-view--refresh-current)
      (should (eq org-air-view--orientation 'board-only))
      (org-air-r46--assert-trigger-rows)
      (org-air-r46--drive-hostile-walks
       (current-buffer) #'evil-next-line #'evil-previous-line)
      ;; --- EMPTY board: the `Nothing scheduled …' note rows. ---
      (org-air-r46--drive-empty-board-walk
       (current-buffer) #'evil-next-line #'evil-previous-line))))

(provide 'org-air-round46-test)
;;; org-air-round46-test.el ends here
