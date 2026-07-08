;;; org-air-round29-test.el --- executing ERTs for v0.5 round-29 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.5 round-29 (air/v0.5/org-air-round29-design.org).
;;
;;   R29-1  FRINGE-LESS GUI X-OVERFLOW — the R27-2 host-width seam (and the
;;          rail's own paint + the doc-session host) measured raw
;;          `window-body-width' while every other live path measured
;;          `org-air-layout--usable-columns' (GUI: `window-max-chars-per-line',
;;          which reserves the continuation-glyph column when the right
;;          fringe is absent).  In a fringe-less GUI the two disagree by ONE,
;;          so with the rail popped every composed line ran one column past
;;          the window edge.  The fringe-less GUI is simulated per the Emacs
;;          contract: `display-graphic-p' -> t and
;;          `window-max-chars-per-line' -> body - 1 (zero right fringe), with
;;          LIVE windows and NO width seam — exactly the round's measured
;;          reproduction harness.
;;
;;   R29-2  EVIL CURSOR SNAP — command-agnostic, line-motion-gated title
;;          snap; j/k in the shared core map; entry/restore normalize; late
;;          evil registration replay.  Driven with the REAL evil from .deps.
;;
;; Batch contract: BOTH items are byte-invisible — every golden identical
;; (the batch width seams bypass the host-width helper; in a TTY/batch frame
;; `org-air-layout--usable-columns' == `window-body-width').

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-project-test)            ; project fixture root
(require 'org-air-viewport-helpers)
(require 'org-air-round27-test)            ; live-board/-project harness

(when (locate-library "org-air")
  (require 'org-air))

;;;; =====================================================================
;;;; R29-1 — fringe-less GUI simulation harness.
;;;; =====================================================================

(defmacro org-air-r29--with-fringeless-gui (&rest body)
  "Run BODY under a simulated FRINGE-LESS GUI (R29-1 reproduction harness).
Per the documented Emacs contract: `display-graphic-p' -> t (so
`org-air-layout--usable-columns' takes its graphical branch) and
`window-fringes' reports a ZERO right fringe (R34-1: the continuation
glyph then steals the last text column, so usable == body - 1).  The
legacy `window-max-chars-per-line' stub is kept for any residual reader
but `org-air-layout--usable-columns' now derives from `window-body-width'
and the fringe (R34-1).  Live windows, no width seam — the round's
measured reproduction."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'display-graphic-p)
              (lambda (&optional _display) t))
             ((symbol-function 'window-fringes)
              (lambda (&optional _window) (list 0 0 nil)))
             ((symbol-function 'window-max-chars-per-line)
              (lambda (&optional window _face)
                (1- (window-body-width (or window (selected-window)))))))
     ,@body))

(defmacro org-air-r29--with-fringed-gui (&rest body)
  "Run BODY under a simulated GUI WITH fringes (R29-1 no-op variant).
`display-graphic-p' -> t and `window-fringes' reports a POSITIVE right
fringe (R34-1: the glyph lives in the fringe, so usable == body).  The
R29-1/R34-1 fix must change nothing here."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'display-graphic-p)
              (lambda (&optional _display) t))
             ((symbol-function 'window-fringes)
              (lambda (&optional _window) (list 8 8 nil)))
             ((symbol-function 'window-max-chars-per-line)
              (lambda (&optional window _face)
                (window-body-width (or window (selected-window))))))
     ,@body))

(defun org-air-r29--assert-lines-fit (win)
  "Assert every line of WIN's buffer fits WIN's usable columns."
  (let ((usable (org-air-layout--usable-columns win)))
    (with-current-buffer (window-buffer win)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let ((w (string-width
                    (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position)))))
            (unless (<= w usable)
              (ert-fail (format "line %d is %d cols wide > usable %d: %S"
                                (line-number-at-pos) w usable
                                (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position))))))
          (forward-line 1))))))

(defun org-air-r29--header-widths ()
  "Return (RAW . TRIMMED) display widths of the first buffer line."
  (save-excursion
    (goto-char (point-min))
    (let ((line (buffer-substring-no-properties (point) (line-end-position))))
      (cons (string-width line)
            (string-width (string-trim-right line))))))

;;;; =====================================================================
;;;; R29-1 — no composed line exceeds the usable columns; header contract.
;;;; =====================================================================

(ert-deftest org-air-r29-1-fringeless-no-line-exceeds-usable ()
  "Fringe-less GUI, all three rail modes, odd AND even host widths: after
render + refresh, every buffer line's `string-width' is <= the displaying
window's `org-air-layout--usable-columns'.  Trunk FAILED under the popped
side-window rail (every line one column past the usable area: 51 > 50)."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-board
    (org-air-r29--with-fringeless-gui
      ;; --- SIDE-WINDOW (popped rail): the trunk overflow. ---
      (org-air-r27--pop-rail)
      (org-air-view--refresh-current)
      (should (eq org-air-view--orientation 'side-window))
      (let ((bwin (get-buffer-window (current-buffer))))
        (should (window-live-p bwin))
        (should (eql org-air-view--rendered-width
                     (org-air-layout--usable-columns bwin)))
        (org-air-r29--assert-lines-fit bwin))
      ;; --- INLINE (two-pane) at the full frame width. ---
      (setq-local org-air-view--rail-popped-out nil)
      (org-air-rail--hide (current-buffer))
      (org-air-view--refresh-current)
      (should (eq org-air-view--orientation 'two-pane))
      (org-air-r29--assert-lines-fit (get-buffer-window (current-buffer)))
      ;; --- BOARD-ONLY at MULTIPLE widths, odd and even, via splits. ---
      (let ((org-air-rail-min-width 500)   ; force board-only at any width
            (widths nil))
        (dolist (size '(29 30))
          (let ((other (split-window (get-buffer-window (current-buffer))
                                     (- size) 'right)))
            (unwind-protect
                (let ((bwin (get-buffer-window (current-buffer))))
                  (with-selected-window bwin
                    (org-air-view--refresh-current))
                  (should (eq org-air-view--orientation 'board-only))
                  (push (org-air-layout--usable-columns bwin) widths)
                  (org-air-r29--assert-lines-fit bwin))
              (delete-window other))))
        ;; the split pair really exercised BOTH parities.
        (should (cl-find-if #'cl-oddp widths))
        (should (cl-find-if #'cl-evenp widths))))))

(ert-deftest org-air-r29-1-header-ends-at-contract-column ()
  "Fringe-less GUI: the S7 header contract holds where the user looks —
the right-trimmed header width equals compose-width - 1, the compose
width equals the window's USABLE columns, and the header's final column
is blank (the S7 margin column sits INSIDE the displayable area).  Trunk
FAILED under side-window: compose width = usable + 1, so the status glyph
sat flush at the window edge."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-board
    (org-air-r29--with-fringeless-gui
      (org-air-r27--pop-rail)
      (org-air-view--refresh-current)
      (should (eq org-air-view--orientation 'side-window))
      (let* ((bwin (get-buffer-window (current-buffer)))
             (usable (org-air-layout--usable-columns bwin))
             (hw (org-air-r29--header-widths)))
        ;; compose width == usable columns (the R29-1 fix).
        (should (eql org-air-view--rendered-width usable))
        ;; status ends at W-1 of the compose width (S7, unchanged).
        (should (= (cdr hw) (1- org-air-view--rendered-width)))
        ;; the final column of the header is blank (right-trimmed line is
        ;; strictly narrower than the compose width).
        (should (< (car hw) org-air-view--rendered-width)))
      ;; the INLINE header keeps the same contract (regression guard).
      (setq-local org-air-view--rail-popped-out nil)
      (org-air-rail--hide (current-buffer))
      (org-air-view--refresh-current)
      (let* ((bwin (get-buffer-window (current-buffer)))
             (usable (org-air-layout--usable-columns bwin))
             (hw (org-air-r29--header-widths)))
        (should (eql org-air-view--rendered-width usable))
        (should (= (cdr hw) (1- org-air-view--rendered-width)))))))

(ert-deftest org-air-r29-1-rail-buffer-fits ()
  "Fringe-less GUI: the popped rail buffer's OWN lines fit its side
window's usable columns.  Trunk FAILED (composed at the raw body width:
28 > 27)."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-board
    (org-air-r29--with-fringeless-gui
      (org-air-r27--pop-rail)
      (org-air-view--refresh-current)
      (let ((rwin (org-air-rail--side-window)))
        (should (window-live-p rwin))
        (should (eq (window-buffer rwin)
                    (get-buffer org-air-rail-buffer-name)))
        (org-air-r29--assert-lines-fit rwin)))))

(ert-deftest org-air-r29-1-project-fits-fringeless ()
  "Fringe-less GUI: the PROJECT view (default side-window placement,
R26-5) composes at its host window's usable columns — every line fits,
every doc row is composed to exactly the rendered width, and
`org-air-project--rendered-width' equals the usable columns.  Trunk
FAILED (the shared host-width helper measured raw body width)."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-project
    (should (org-air-rail--popped-p))
    (org-air-r29--with-fringeless-gui
      (org-air-view--refresh-current)
      (let* ((pwin (get-buffer-window (current-buffer)))
             (usable (org-air-layout--usable-columns pwin)))
        (should (window-live-p pwin))
        (should (eql org-air-project--rendered-width usable))
        (org-air-r29--assert-lines-fit pwin)
        (dolist (bol (org-air-r27--doc-row-bols))
          (save-excursion
            (goto-char bol)
            (should (= (string-width
                        (buffer-substring-no-properties
                         bol (line-end-position)))
                       org-air-project--rendered-width))))))))

(ert-deftest org-air-r29-1-fringed-gui-unchanged ()
  "A GUI WITH fringes has `window-max-chars-per-line' == body width, so
the R29-1 fix is a no-op there: with the rail popped, the rendered width
and the longest composed line are exactly the values of the plain
\(TTY-measured) render in the same window geometry."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-board
    ;; plain render (TTY tier: usable == body) — the trunk values.
    (org-air-r27--pop-rail)
    (org-air-view--refresh-current)
    (let ((plain-width org-air-view--rendered-width)
          (plain-longest (org-air-r27--longest-line))
          (bwin (get-buffer-window (current-buffer))))
      (should (eql plain-width (window-body-width bwin)))
      ;; fringed GUI: usable == body, so nothing moves.
      (org-air-r29--with-fringed-gui
        (org-air-view--refresh-current)
        (should (eql org-air-view--rendered-width plain-width))
        (should (= (org-air-r27--longest-line) plain-longest))
        (should (eql org-air-view--rendered-width
                     (window-body-width
                      (get-buffer-window (current-buffer)))))))))

;;;; =====================================================================
;;;; R29-2 — command-agnostic line-motion-gated title snap (evil cursor
;;;; stuck at column 0).  REAL evil from .deps (R27-4 pattern): live
;;;; windows, motion state, commands resolved via `key-binding' and
;;;; dispatched with pre/post-command hooks run.
;;;; =====================================================================

(defvar org-air-r29--last-cmd nil
  "The previously dispatched command, so goal-column repeats are real.")

(defun org-air-r29--dispatch (buf cmd)
  "Dispatch CMD in BUF with pre/post-command hooks run (R27-4 pattern).
Binds `this-command'/`last-command' the way the command loop would, so
line-motion goal-column tracking behaves as in real use."
  (with-current-buffer buf
    (let ((this-command cmd)
          (last-command org-air-r29--last-cmd))
      (run-hooks 'pre-command-hook)
      (call-interactively cmd)
      (run-hooks 'post-command-hook))
    (setq org-air-r29--last-cmd cmd)))

(defun org-air-r29--press (buf key)
  "Dispatch KEY in BUF via its live `key-binding' (hooks run)."
  (with-current-buffer buf
    (let ((cmd (key-binding (kbd key))))
      (should cmd)
      (org-air-r29--dispatch buf cmd)
      cmd)))

(defun org-air-r29--on-row-p (buf)
  "Non-nil when point's line in BUF owns an item/doc row."
  (with-current-buffer buf
    (and (or (org-air-view--row-property 'org-air-item)
             (org-air-view--row-property 'org-air-doc))
         t)))

(defun org-air-r29--assert-title-landing (buf what &optional exact)
  "When BUF's point sits on a row line, it must NOT be in the dead zone.
The title landing is `org-air-view--row-title-pos' — the char carrying
`org-air-row-title', or the row's first visible glyph for a row whose
title cell truncated away entirely (the narrow batch-frame tier; the
R21-2 documented fallback).  With EXACT non-nil (org-air's own j/k —
`org-air-view--goto-row-title' by construction) point must be exactly
there; otherwise (raw evil/native line motions) point must be ON or
AFTER it — a goal-column landing INSIDE the row text of a
differently-indented row is by design not hijacked (spec point 3), but
the gutter/column-0 dead zone always snaps.  Trunk left point at column
0 in every shape.  Returns 1 for a row landing, 0 otherwise."
  (with-current-buffer buf
    (if (not (org-air-r29--on-row-p buf))
        0
      (let ((want (org-air-view--row-title-pos)))
        (unless (if exact (= (point) want) (>= (point) want))
          (ert-fail (format "%s: row landing at line %d col %d, want%s col %d"
                            what (line-number-at-pos) (current-column)
                            (if exact "" " >=")
                            (save-excursion (goto-char want)
                                            (current-column)))))
        ;; when the row owns a title mark and the landing is exact, point
        ;; really is ON the marked char.
        (when (and exact (get-text-property want 'org-air-row-title))
          (unless (get-text-property (point) 'org-air-row-title)
            (ert-fail (format "%s: landing lost the title mark" what)))))
      1)))

(defun org-air-r29--drive-line-motions (buf)
  "Drive <down>/<up>/j/k line motions in BUF; every row landing must be
on the title char.  Returns the number of row landings (anti-tautology:
the caller asserts it is non-trivial)."
  (let ((landings 0)
        (org-air-r29--last-cmd nil))
    ;; from the very top (the user's gg / entry shape)...
    (with-current-buffer buf (goto-char (point-min)))
    (dotimes (_ 6)
      (org-air-r29--press buf "<down>")
      (cl-incf landings (org-air-r29--assert-title-landing buf "<down>")))
    (dotimes (_ 3)
      (org-air-r29--press buf "<up>")
      (cl-incf landings (org-air-r29--assert-title-landing buf "<up>")))
    ;; ...and from a FORCED column-0 start on a row (the stuck shape).
    (with-current-buffer buf
      (goto-char (or (text-property-not-all (point-min) (point-max)
                                            'org-air-item nil)
                     (text-property-not-all (point-min) (point-max)
                                            'org-air-doc nil)
                     (point-min)))
      (beginning-of-line))
    (setq org-air-r29--last-cmd nil)
    ;; org-air's own j/k land EXACTLY on the title, every depth/width.
    (dotimes (_ 3)
      (org-air-r29--press buf "j")
      (cl-incf landings (org-air-r29--assert-title-landing buf "j" t)))
    (dotimes (_ 2)
      (org-air-r29--press buf "k")
      (cl-incf landings (org-air-r29--assert-title-landing buf "k" t)))
    landings))

(ert-deftest org-air-r29-2-evil-lines-land-on-title-board ()
  "BOARD under REAL evil (motion state), ALL THREE compositions
\(board-only, side-window popped, two-pane): <down>/<up>
\(evil-next-line/evil-previous-line) and j/k from `point-min' AND from a
forced column-0 start — every landing on an item row puts point ON the
char carrying `org-air-row-title'.  Trunk FAILED in board-only and
side-window (the row property covers column 0, so the R22-2 snap never
fired and point stuck at column 0)."
  (skip-unless (locate-library "org-air"))
  (skip-unless (locate-library "evil"))
  (require 'evil)
  (org-air-r27--with-live-board
    (evil-local-mode 1)
    (should (eq evil-state 'motion))
    ;; j/k are org-air's own line motions (core map + R27-4 override).
    (should (eq (key-binding (kbd "j")) 'org-air-next-line))
    (should (eq (key-binding (kbd "k")) 'org-air-prev-line))
    ;; arrows stay evil's — the command-agnostic gate must catch them.
    (should (eq (key-binding (kbd "<down>")) 'evil-next-line))
    ;; --- BOARD-ONLY (the user's popped-rail board shape). ---
    (let ((org-air-rail-min-width 500))
      (org-air-view--refresh-current)
      (should (eq org-air-view--orientation 'board-only))
      (should (> (org-air-r29--drive-line-motions (current-buffer)) 3)))
    ;; --- SIDE-WINDOW (rail popped). ---
    (org-air-r27--pop-rail)
    (should (eq org-air-view--orientation 'side-window))
    (should (> (org-air-r29--drive-line-motions (current-buffer)) 3))
    ;; --- TWO-PANE (inline rail — the R22-2 shape keeps working). ---
    (setq-local org-air-view--rail-popped-out nil)
    (org-air-rail--hide (current-buffer))
    (org-air-view--refresh-current)
    (should (eq org-air-view--orientation 'two-pane))
    (should (> (org-air-r29--drive-line-motions (current-buffer)) 3))))

(ert-deftest org-air-r29-2-evil-goto-top-bottom-land-on-title ()
  "BOARD under REAL evil (motion state), the two STUCK shapes (board-only
AND side-window popped): the R27-4 overriding map wins the `g' prefix so
`gg'/`G' route to org-air's OWN `org-air-goto-top'/`org-air-goto-bottom'
\(NOT evil's `evil-goto-first-line'/`evil-goto-line'), and from a forced
bottom/column-0 start each lands ON the char carrying `org-air-row-title'.
Completes the `j/k/gg/G' motion matrix (the vim-convention line-motion
vocabulary the user drives) — no prior test exercised the top/bottom
motions under evil at all."
  (skip-unless (locate-library "org-air"))
  (skip-unless (locate-library "evil"))
  (require 'evil)
  (org-air-r27--with-live-board
    (evil-local-mode 1)
    (should (eq evil-state 'motion))
    ;; the R27-4 override wins the `g' prefix: `gg' -> org-air's top motion
    ;; (over evil's `evil-goto-first-line'), `G' -> org-air's bottom.
    (should (eq (key-binding (kbd "gg")) 'org-air-goto-top))
    (should (eq (key-binding (kbd "G")) 'org-air-goto-bottom))
    (dolist (shape '(board-only side-window))
      (if (eq shape 'board-only)
          (let ((org-air-rail-min-width 500))
            (org-air-view--refresh-current))
        (org-air-r27--pop-rail))
      (should (eq org-air-view--orientation shape))
      (let ((buf (current-buffer)))
        ;; G (goto-bottom) from the top: lands on the LAST row's title.
        (with-current-buffer buf (goto-char (point-min)))
        (org-air-r29--press buf "G")
        (should (= 1 (org-air-r29--assert-title-landing buf "G" t)))
        ;; gg (goto-top) from a forced column-0 bottom: FIRST row's title.
        (with-current-buffer buf (goto-char (point-max)) (beginning-of-line))
        (org-air-r29--press buf "gg")
        (should (= 1 (org-air-r29--assert-title-landing buf "gg" t)))))))

(ert-deftest org-air-r29-2-evil-lines-land-on-title-project ()
  "PROJECT under REAL evil (directory AND state groupings): j/k (now
core-bound — trunk had them UNBOUND here) and raw `evil-next-line' /
`evil-previous-line' / `evil-goto-line' driven directly — every doc-row
landing is on the title char.  Trunk FAILED (column 0 on every doc row)."
  (skip-unless (locate-library "org-air"))
  (skip-unless (locate-library "evil"))
  (require 'evil)
  (org-air-r27--with-live-project
    (evil-local-mode 1)
    (should (eq evil-state 'motion))
    ;; the trunk gap: j/k now resolve to org-air's motions here.
    (should (eq (key-binding (kbd "j")) 'org-air-next-line))
    (should (eq (key-binding (kbd "k")) 'org-air-prev-line))
    (dolist (group '(directory state))
      (setq org-air-project-group group)
      (org-air-view--refresh-current)
      (should (> (org-air-r29--drive-line-motions (current-buffer)) 3))
      ;; raw evil line motions, dispatched directly (no keymap layer):
      ;; traverse the WHOLE buffer with evil-next-line so EVERY doc row is
      ;; landed on (the narrow batch tier interleaves group headers/blanks,
      ;; so a fixed-count drive from point-min visits only a few rows),
      ;; then back up and jump — every doc-row landing is on the title.
      (let ((landings 0)
            (org-air-r29--last-cmd nil)
            (nlines (line-number-at-pos (point-max))))
        (with-current-buffer (current-buffer) (goto-char (point-min)))
        ;; (1- nlines) presses from line 1 lands exactly on the last line
        ;; — never a press PAST it (evil-next-line signals end-of-buffer).
        (dotimes (_ (1- nlines))
          (org-air-r29--dispatch (current-buffer) #'evil-next-line)
          (cl-incf landings (org-air-r29--assert-title-landing
                             (current-buffer) "evil-next-line")))
        (dotimes (_ 3)
          (org-air-r29--dispatch (current-buffer) #'evil-previous-line)
          (cl-incf landings (org-air-r29--assert-title-landing
                             (current-buffer) "evil-previous-line")))
        ;; evil G — the measured col-0 report.
        (org-air-r29--dispatch (current-buffer) #'evil-goto-line)
        (cl-incf landings (org-air-r29--assert-title-landing
                           (current-buffer) "evil-goto-line"))
        ;; anti-tautology: the traversal really visited doc rows (7 in the
        ;; fixture), every one validated on the title above.
        (should (> landings 3))))))

(ert-deftest org-air-r29-2-horizontal-not-hijacked ()
  "In-row horizontal motion is NEVER hijacked: with point on a title
under the board-only composition (where column 0 CARRIES the row
property), evil `l' -> title+1 and STAYS; `h' back INTO the gutter parks
on the todo cell and STAYS (the explicit spec point); `0' -> column 0
and STAYS; plain `forward-char'/`backward-char' within the row are
untouched.  Only the next LINE-crossing motion snaps (j from the gutter
lands the NEXT row's title)."
  (skip-unless (locate-library "org-air"))
  (skip-unless (locate-library "evil"))
  (require 'evil)
  (org-air-r27--with-live-board
    (let ((org-air-rail-min-width 500))
      (org-air-view--refresh-current)
      (should (eq org-air-view--orientation 'board-only))
      (evil-local-mode 1)
      (should (eq evil-state 'motion))
      (org-air-view--goto-first-item)
      (let* ((buf (current-buffer))
             (title (point))
             (org-air-r29--last-cmd nil))
        (should (get-text-property title 'org-air-row-title))
        ;; l -> one INTO the title, stays.
        (org-air-r29--press buf "l")
        (should (= (point) (1+ title)))
        ;; h h (evil-backward-char, dispatched directly — on the KEY
        ;; layer the special-mode parent owns `h' as `describe-mode', a
        ;; point-preserving no-op) -> back onto the title, then INTO the
        ;; gutter — and STAYS.
        (org-air-r29--dispatch buf #'evil-backward-char)
        (should (= (point) title))
        (org-air-r29--dispatch buf #'evil-backward-char)
        (should (= (point) (1- title)))
        (should (get-text-property (point) 'org-air-item)) ; gutter is owned
        ;; plain char motions inside the row: untouched.
        (org-air-r29--dispatch buf #'forward-char)
        (should (= (point) title))
        (org-air-r29--dispatch buf #'backward-char)
        (should (= (point) (1- title)))
        ;; evil `0' (dispatched directly — the special-mode parent maps
        ;; digits to `digit-argument' on the key layer) -> column 0, same
        ;; line, STAYS.
        (org-air-r29--dispatch buf #'evil-beginning-of-line)
        (should (zerop (current-column)))
        ;; evil `^' (evil-first-non-blank) -> the first non-blank glyph on
        ;; the SAME line (the title, past the blank gutter margin), STAYS
        ;; -- a same-line motion is never gated as a snap.
        (let ((line (line-number-at-pos)))
          (org-air-r29--dispatch buf #'evil-first-non-blank)
          (should (= (line-number-at-pos) line))
          (should (get-text-property (point) 'org-air-row-title)))
        ;; ...and the next LINE-crossing motion snaps to the NEXT title.
        (let ((line (line-number-at-pos)))
          (org-air-r29--press buf "j")
          (should (/= (line-number-at-pos) line))
          (should (get-text-property (point) 'org-air-row-title)))))))

(ert-deftest org-air-r29-2-non-evil-unchanged ()
  "WITHOUT evil: `C-n'/`C-p' from the title column preserve the goal
column exactly as on trunk (V6: all titles share one left edge, so the
preserved column lands on/after the title mark — never blocked, never
moved); `n'/`p' land on the title (unchanged R21-2 behavior)."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-board
    (should-not (bound-and-true-p evil-local-mode))
    (org-air-view--goto-first-item)
    (let* ((buf (current-buffer))
           (col (current-column))
           (org-air-r29--last-cmd nil))
      ;; C-n / C-p: goal column preserved through a repeat sequence.
      (org-air-r29--press buf "C-n")
      (should (= (current-column) col))
      (org-air-r29--press buf "C-n")
      (should (= (current-column) col))
      (org-air-r29--press buf "C-p")
      (should (= (current-column) col))
      ;; n / p land on the title (R21-2, unchanged; the shared helper
      ;; accepts the narrow-tier first-visible fallback).
      (org-air-r29--press buf "n")
      (should (= 1 (org-air-r29--assert-title-landing buf "n")))
      (org-air-r29--press buf "p")
      (should (= 1 (org-air-r29--assert-title-landing buf "p"))))))

(ert-deftest org-air-r29-2-entry-and-restore-normalize ()
  "Entry/restore tails normalize EXPLICITLY: point forced into the gutter
\(column 0 of an item row under board-only — property-covered, before the
title mark), then (a) a refresh cycle and (b) a pane open + return —
point ends ON the title char both times, immediately (no keystroke
needed)."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-board
    (let ((org-air-rail-min-width 500))
      (org-air-view--refresh-current)
      (should (eq org-air-view--orientation 'board-only))
      ;; (a) refresh cycle with point in the gutter.
      (org-air-view--goto-first-item)
      (beginning-of-line)
      (should (get-text-property (point) 'org-air-item))
      (org-air-view--refresh-current)
      (should (get-text-property (point) 'org-air-row-title))
      ;; (b) pane open + return with point forced into the gutter.
      (org-air-view--goto-first-item)
      (org-air-view-pane)
      (let ((board (current-buffer))
            (pane (org-air-view-pane--buffer)))
        (should (buffer-live-p pane))
        (with-current-buffer board
          (org-air-view--goto-first-item)
          (beginning-of-line))
        (with-current-buffer pane
          (org-air-view-pane-quit))
        (with-current-buffer board
          (should (get-text-property (point) 'org-air-row-title)))))))

(ert-deftest org-air-r29-2-late-evil-registration ()
  "A LATE-loading evil still registers every org-air view: in a FRESH
batch Emacs the org-air modes are initialised BEFORE (require \='evil);
after the load, the board and the project are motion-state with `j'
resolving to `org-air-next-line' (the `with-eval-after-load' replay over
`org-air-view--evil-modes' fired).  Trunk FAILED: the fboundp gate at
mode init silently skipped registration and nothing ever re-ran it."
  (skip-unless (locate-library "org-air"))
  (skip-unless (locate-library "evil"))
  (let* ((root (locate-dominating-file org-air-test-fixture-dir "Makefile"))
         (init (expand-file-name "tests/org-air-test-init.el" root))
         (script
          (prin1-to-string
           '(progn
              (require 'org-air)
              (when (featurep 'evil) (kill-emacs 2))
              ;; org-air modes initialise BEFORE evil exists.
              (with-temp-buffer (org-air-view-mode))
              (with-temp-buffer (org-air-project-mode))
              ;; the deferred load arrives — the replay must fire.
              (require 'evil)
              (unless (eq (evil-initial-state 'org-air-view-mode) 'motion)
                (kill-emacs 3))
              (unless (eq (evil-initial-state 'org-air-project-mode) 'motion)
                (kill-emacs 4))
              (dolist (mode '(org-air-view-mode org-air-project-mode))
                (with-temp-buffer
                  (funcall mode)
                  (evil-local-mode 1)
                  (unless (eq evil-state 'motion) (kill-emacs 5))
                  (unless (eq (key-binding (kbd "j")) 'org-air-next-line)
                    (kill-emacs 6))
                  (unless (eq (key-binding (kbd "k")) 'org-air-prev-line)
                    (kill-emacs 7))))
              (kill-emacs 0)))))
    (should root)
    (with-temp-buffer
      (let ((status (call-process
                     (or (getenv "EMACS") "emacs") nil t nil
                     "-Q" "--batch" "-l" init "--eval" script)))
        (unless (eql status 0)
          (ert-fail (format "late-evil subprocess exited %s: %s"
                            status (buffer-string))))))))

(provide 'org-air-round29-test)
;;; org-air-round29-test.el ends here
