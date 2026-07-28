;;; org-air-round91-test.el --- acceptance tests for round 91 -*- lexical-binding: t; -*-

;;; Commentary:
;; Round-91 acceptance: SCROLL STABILITY.  A user reported that pressing
;; `m' on the board "scrolls the buffer to a weird position".  Every
;; org-air repaint is `erase-buffer' + re-render, and the render pipeline
;; preserved POINT only: nothing recorded or restored the WINDOW's scroll
;; position, so after the erase each displaying window's `window-start'
;; marker collapsed to `point-min' and the row the user was standing on
;; landed on a different screen line (measured on the parent revision:
;; `window-start' 2745 -> 1, the row falling from screen line 6 to 46 in a
;; 22-row window, i.e. off-screen).  R90 turned `m' into a repaint, which
;; fired that latent defect on a high-frequency keystroke.
;;
;; The whole class had ZERO coverage.  These tests are the permanent
;; regression net.  They drive the REAL commands in a REAL window
;; (`set-window-buffer', `window-start', `count-screen-lines',
;; `vertical-motion' and `window-body-height' all work in --batch) and
;; assert only OBSERVABLE VIEWPORT FACTS:
;;
;;   * the 0-based screen line of the window's point inside the window
;;     (`count-screen-lines' with COUNT-FINAL-NEWLINE non-nil, which is
;;     the only measurement that is correct for a column-0 point),
;;   * `window-start' / `window-point',
;;   * `window-body-height' for on-screen-ness.
;;
;; No org-air-internal helper name is asserted on, so ANY correct
;; implementation of the rule "a repaint leaves the cursor's row on the
;; same screen line, in every window that shows the buffer" passes.
;;
;; NOTE on instruments: `pos-visible-in-window-p' is unusable in --batch
;; (a batch frame never realises glyph matrices; it returns nil even for
;; `point-min' of a plain buffer), so on-screen-ness is asserted as
;; OFFSET < `window-body-height' — screen-line arithmetic, which is
;; exact.  Nothing here is pixel-dependent, so nothing here is a GUI skip.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)

(when (locate-library "org-air")
  (require 'org-air)
  (require 'org-air-review))

(defvar org-air-r91--dir nil
  "Temporary corpus directory of the current round-91 test.")

(defconst org-air-r91--task-count 60
  "Number of TODO rows in the round-91 corpus.
Large enough that the expanded board is taller than the batch window,
which is the only interesting case for a viewport regression.")

(defconst org-air-r91--anchor-title "Task 40"
  "The row parked well down the buffer in every round-91 measurement.")

(defun org-air-r91--corpus-text ()
  "Return the round-91 corpus: TODO rows, every even one tagged `focus'.
The tag split gives a filter that KEEPS the anchor row (`Task 40' is
even) so the filter tests exercise a repaint that changes the buffer's
total line count without removing the row under point.

R93: each row carries an inactive stamp in its own body, 60 days before
the frozen clock.  Needs attention is an AGING rule now, so a corpus
written a millisecond ago puts every heading at age 0 against a
threshold of 30 and the section this suite expands would hold NO rows --
the board would be shorter than the window and every viewport
measurement below would fail on its precondition rather than on the
scroll behaviour it exists to pin."
  (let ((quiet (org-air-test-quiet-stamp)))
    (mapconcat (lambda (i)
                 (if (zerop (% i 2))
                     (format "* TODO Task %d :focus:\n%s\n" i quiet)
                   (format "* TODO Task %d\n%s\n" i quiet)))
               (number-sequence 1 org-air-r91--task-count) "")))

(defun org-air-r91--reset-query-state ()
  "Reset global query tables between round-91 corpora."
  (when (fboundp 'org-air-query-teardown)
    (org-air-query-teardown)
    (clrhash org-air-query--file-meta)
    (clrhash org-air-query--visits)
    (clrhash org-air-query--denote-id-index)
    (setq org-air-query--link-graph-dirty nil)))

(defmacro org-air-r91--with-corpus (&rest body)
  "Create the round-91 corpus and run BODY with isolated global state."
  (declare (indent 0) (debug t))
  `(let ((org-air-r91--dir (make-temp-file "org-air-r91-" t)))
     (unwind-protect
         (progn
           (org-air-r91--reset-query-state)
           (let ((coding-system-for-write 'utf-8-unix)
                 (file-name-handler-alist nil))
             (write-region (org-air-r91--corpus-text) nil
                           (expand-file-name "tasks.org" org-air-r91--dir)
                           nil 'silent)
             (write-region "" nil
                           (expand-file-name "inbox.org" org-air-r91--dir)
                           nil 'silent))
           (let ((org-air-files (list org-air-r91--dir))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-r91--dir))
                 (org-air-cache-file
                  (expand-file-name ".cache/board.eld" org-air-r91--dir))
                 (org-air-view-buffer-name "*org-air-r91*")
                 (org-air-view--edit-ring nil)
                 (org-air-view--edit-redo-ring nil)
                 (find-file-hook (copy-sequence find-file-hook))
                 (org-air-backlog-tag "backlog")
                 (org-air-plain-heading-type 'task)
                 (org-tags-column 0)
                 (create-lockfiles nil)
                 (inhibit-message t))
             ,@body))
       (org-air-r91--reset-query-state)
       (let ((kill-buffer-query-functions nil))
         (dolist (buf (buffer-list))
           (let ((file (buffer-file-name buf)))
             (when (and file (string-prefix-p org-air-r91--dir file))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r91--dir t))))

(defun org-air-r91--expand-attention ()
  "Expand the `Needs attention' section so the board outgrows the window."
  (let ((pos (org-air-view--find-property 'org-air-section 'attention)))
    (should pos)
    (goto-char pos)
    (org-air-toggle-section)))

(defmacro org-air-r91--with-window-board (&rest body)
  "Open the board in a REAL window, expand it, and run BODY in its buffer.
The board window is SELECTED, so `window-point' tracks `point' exactly
as it does for a user.  The window configuration is restored afterwards."
  (declare (indent 0) (debug t))
  `(org-air-r91--with-corpus
     (org-air-viewport-test--with-frozen-now
       (save-window-excursion
         (unwind-protect
             (progn
               (org-air)
               (let* ((buf (get-buffer org-air-view-buffer-name))
                      (win (and buf (get-buffer-window buf))))
                 (should buf)
                 (should (window-live-p win))
                 (select-window win)
                 (with-current-buffer buf
                   (org-air-r91--expand-attention)
                   ;; The whole point of this suite: a board TALLER than
                   ;; the window it is shown in.
                   (should (> (line-number-at-pos (point-max))
                              (window-body-height win)))
                   ,@body)))
           (let ((kill-buffer-query-functions nil)
                 (buf (get-buffer org-air-view-buffer-name)))
             (when buf (kill-buffer buf))))))))

;;;; ---------------------------------------------------------------------
;;;; Observable viewport instruments (no org-air internals)
;;;; ---------------------------------------------------------------------

(defun org-air-r91--offset (win)
  "Return the 0-based SCREEN line of WIN's point inside WIN.
COUNT-FINAL-NEWLINE is non-nil: with it nil `count-screen-lines' drops
the last line whenever END sits right after a newline, so a point at
COLUMN 0 would measure one screen line too high."
  (max 0 (1- (count-screen-lines (window-start win) (window-point win)
                                 t win))))

(defun org-air-r91--row (win)
  "Return the trimmed text of the buffer line at WIN's point."
  (save-excursion
    (goto-char (window-point win))
    (string-trim (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position)))))

(defun org-air-r91--goto-title (title)
  "Move point onto the board row naming TITLE and return that position."
  (goto-char (point-min))
  (should (re-search-forward (concat (regexp-quote title) "\\_>") nil t))
  (beginning-of-line)
  (org-air-view--goto-row-title)
  (point))

(defun org-air-r91--park (win offset)
  "Anchor WIN so its point sits OFFSET screen lines below `window-start'."
  (set-window-point win (point))
  (set-window-start win (save-excursion (vertical-motion (- offset) win)
                                        (point))
                    t)
  (should (= offset (org-air-r91--offset win)))
  offset)

(defun org-air-r91--park-anchor-row (win offset)
  "Park the anchor row in WIN at OFFSET and return (START . OFFSET)."
  (org-air-r91--goto-title org-air-r91--anchor-title)
  (org-air-r91--park win offset)
  (cons (window-start win) offset))

(defun org-air-r91--assert-stable (win before &optional label)
  "Assert WIN still shows its point row on screen line (cdr BEFORE).
BEFORE is the (START . OFFSET) pair captured before the repaint.  LABEL
names the command under test in the failure message."
  (let ((offset (org-air-r91--offset win))
        (what (or label "repaint")))
    ;; The cursor's row is redrawn on the SAME screen line.
    (should (equal (list what (cdr before)) (list what offset)))
    ;; ... which is only meaningful if the viewport did not collapse.
    (should (equal (list what (car before)) (list what (window-start win))))
    ;; ... and the landing is inside the window.
    (should (< offset (window-body-height win)))
    ;; The acting window's point is the buffer's point.
    (should (= (window-point win) (point)))))

(defun org-air-r91--assert-on-screen (win offset &optional label)
  "Assert WIN's point row sits on screen line OFFSET and is visible."
  (let ((what (or label "repaint")))
    (should (equal (list what offset) (list what (org-air-r91--offset win))))
    (should (< (org-air-r91--offset win) (window-body-height win)))))

;;;; =====================================================================
;;;; 1. The reported defect: `m' must not move the row on screen.
;;;; =====================================================================

(ert-deftest org-air-r91-mark-keeps-row-on-same-screen-line ()
  "`m' on a row well down a board taller than the window never scrolls.
This is the user's report verbatim.  On the parent revision the repaint
dropped `window-start' to `point-min' and the row fell from screen line
6 to 46 in a 22-row window."
  (skip-unless (fboundp 'org-air-toggle-mark))
  (org-air-r91--with-window-board
    (let* ((win (selected-window))
           (before (org-air-r91--park-anchor-row win 6)))
      (org-air-toggle-mark)
      (should (string-match-p (regexp-quote org-air-r91--anchor-title)
                              (org-air-r91--row win)))
      (org-air-r91--assert-stable win before "m"))))

(ert-deftest org-air-r91-unmark-keeps-row-on-same-screen-line ()
  "A second `m' (unmark) is a repaint too and is equally stable."
  (skip-unless (fboundp 'org-air-toggle-mark))
  (org-air-r91--with-window-board
    (let* ((win (selected-window))
           (before (org-air-r91--park-anchor-row win 8)))
      (org-air-toggle-mark)
      (org-air-r91--assert-stable win before "m (mark)")
      (org-air-toggle-mark)
      (org-air-r91--assert-stable win before "m (unmark)")
      (should (string-match-p (regexp-quote org-air-r91--anchor-title)
                              (org-air-r91--row win))))))

(ert-deftest org-air-r91-clear-marks-keeps-row-on-same-screen-line ()
  "`M' (clear all marks) repaints and must not move the row either."
  (skip-unless (fboundp 'org-air-clear-marks))
  (org-air-r91--with-window-board
    (let* ((win (selected-window))
           (before (org-air-r91--park-anchor-row win 5)))
      (org-air-toggle-mark)
      (org-air-r91--assert-stable win before "m")
      (org-air-clear-marks)
      (org-air-r91--assert-stable win before "M"))))

(ert-deftest org-air-r91-triage-verb-keeps-row-on-same-screen-line ()
  "A triage verb (`b', backlog toggle) repaints without a viewport jump."
  (skip-unless (fboundp 'org-air-item-backlog))
  (org-air-r91--with-window-board
    (let* ((win (selected-window))
           (before (org-air-r91--park-anchor-row win 7)))
      (org-air-item-backlog)
      ;; The row may be re-bucketed; the landing must still hold the line.
      (org-air-r91--assert-on-screen win (cdr before) "b backlog")
      (should (= (window-point win) (point))))))

;;;; =====================================================================
;;;; 2. Every other repaint entry point.
;;;; =====================================================================

(ert-deftest org-air-r91-filter-toggle-keeps-row-on-same-screen-line ()
  "`/' on and off keeps the row's screen line across a line-count change."
  (skip-unless (and (fboundp 'org-air-filter) (fboundp 'org-air-filter-clear)))
  (org-air-r91--with-window-board
    (let* ((win (selected-window))
           (before (org-air-r91--park-anchor-row win 6))
           (lines-before (line-number-at-pos (point-max))))
      (org-air-filter '("focus"))
      (let ((lines-filtered (line-number-at-pos (point-max))))
        ;; The repaint really did change the total line count.
        (should (< lines-filtered lines-before))
        ;; ... and the anchor row survived it (even tasks carry `focus').
        (should (string-match-p (regexp-quote org-air-r91--anchor-title)
                                (org-air-r91--row win)))
        (org-air-r91--assert-on-screen win (cdr before) "/ filter on")
        (should (= (window-point win) (point)))
        (should-not (= (window-start win) (point-min)))
        (org-air-filter-clear)
        (should (= lines-before (line-number-at-pos (point-max))))
        (org-air-r91--assert-on-screen win (cdr before) "/ filter off")
        (should (= (window-point win) (point)))
        (should-not (= (window-start win) (point-min)))))))

(ert-deftest org-air-r91-sort-cycle-keeps-row-on-same-screen-line ()
  "A sort cycle (and its reverse) repaints without moving the row."
  (skip-unless (fboundp 'org-air-view-sort-cycle))
  (org-air-r91--with-window-board
    (let* ((win (selected-window))
           (before (org-air-r91--park-anchor-row win 6)))
      (org-air-view-sort-cycle)
      (should (string-match-p (regexp-quote org-air-r91--anchor-title)
                              (org-air-r91--row win)))
      (org-air-r91--assert-on-screen win (cdr before) "sort cycle")
      (should (= (window-point win) (point)))
      (should-not (= (window-start win) (point-min)))
      (when (fboundp 'org-air-view-sort-reverse)
        (org-air-view-sort-reverse)
        (org-air-r91--assert-on-screen win (cdr before) "sort reverse")
        (should-not (= (window-start win) (point-min)))))))

(ert-deftest org-air-r91-scope-change-keeps-row-on-same-screen-line ()
  "`s' (scope) and `S' (scope clear) repaint without moving the row."
  (skip-unless (and (fboundp 'org-air-scope) (fboundp 'org-air-scope-clear)))
  (org-air-r91--with-window-board
    (let* ((win (selected-window))
           (before (org-air-r91--park-anchor-row win 9)))
      (org-air-scope "all")
      (org-air-r91--assert-stable win before "s scope")
      (org-air-scope-clear)
      (org-air-r91--assert-stable win before "S scope clear"))))

(ert-deftest org-air-r91-refresh-keeps-row-on-same-screen-line ()
  "`g' (refresh) — the synchronous swap tail — is scroll-stable."
  (skip-unless (fboundp 'org-air-refresh))
  (org-air-r91--with-window-board
    (let* ((win (selected-window))
           (before (org-air-r91--park-anchor-row win 6)))
      (org-air-refresh)
      (should (string-match-p (regexp-quote org-air-r91--anchor-title)
                              (org-air-r91--row win)))
      (org-air-r91--assert-stable win before "g refresh"))))

(ert-deftest org-air-r91-tab-splice-keeps-header-on-same-screen-line ()
  "TAB's body SPLICE path leaves the section header on its screen line."
  (skip-unless (fboundp 'org-air-toggle-section))
  (org-air-r91--with-window-board
    (let ((win (selected-window)))
      (goto-char (org-air-view--find-property 'org-air-section 'attention))
      (org-air-view--goto-row-title)
      (org-air-r91--park win 4)
      (let ((before (cons (window-start win) 4)))
        (org-air-toggle-section)          ; collapse
        (org-air-r91--assert-stable win before "TAB collapse (splice)")
        (org-air-toggle-section)          ; expand
        (org-air-r91--assert-stable win before "TAB expand (splice)")))))

(ert-deftest org-air-r91-tab-full-render-fallback-keeps-header-on-same-screen-line ()
  "TAB's FULL-RENDER fallback (no body band) is equally stable.
The splice path is stable by construction — it deletes from the first
CHANGED line down, which is always below the header the cursor sits on.
The fallback erases everything, so it is the path that actually
regressed; it is driven here by making the body band unknown."
  (skip-unless (and (fboundp 'org-air-toggle-section)
                    (fboundp 'org-air-view--body-region)))
  (org-air-r91--with-window-board
    (let ((win (selected-window)))
      (goto-char (org-air-view--find-property 'org-air-section 'attention))
      (org-air-view--goto-row-title)
      (org-air-r91--park win 4)
      (let ((before (cons (window-start win) 4)))
        (cl-letf (((symbol-function 'org-air-view--body-region)
                   (lambda (&rest _) nil)))
          (org-air-toggle-section)        ; collapse, full render
          (org-air-r91--assert-stable win before "TAB collapse (fallback)")
          (org-air-toggle-section)        ; expand, full render
          (org-air-r91--assert-stable win before "TAB expand (fallback)"))))))

(ert-deftest org-air-r91-undo-redo-repaint-keeps-row-on-same-screen-line ()
  "`u'/`U' repaints after a mutation keep the landing on its screen line."
  (skip-unless (and (fboundp 'org-air-item-done)
                    (fboundp 'org-air-edit-undo)
                    (fboundp 'org-air-edit-redo)))
  (org-air-r91--with-window-board
    (let* ((win (selected-window))
           (before (org-air-r91--park-anchor-row win 6)))
      ;; `D' removes the row; the replacement landing inherits its line.
      (org-air-item-done)
      (org-air-r91--assert-on-screen win (cdr before) "D done")
      (should (= (window-point win) (point)))
      (org-air-edit-undo)
      (org-air-r91--assert-on-screen win (cdr before) "u undo")
      (should (= (window-point win) (point)))
      (should-not (= (window-start win) (point-min)))
      (org-air-edit-redo)
      (org-air-r91--assert-on-screen win (cdr before) "U redo")
      (should (= (window-point win) (point)))
      (should-not (= (window-start win) (point-min))))))

(ert-deftest org-air-r91-rail-toggle-keeps-row-on-same-screen-line ()
  "The rail toggle repaints through the shared dispatch and is stable."
  (skip-unless (fboundp 'org-air-rail-toggle))
  (org-air-r91--with-window-board
    (let* ((win (selected-window))
           (before (org-air-r91--park-anchor-row win 6)))
      (org-air-rail-toggle)
      (org-air-r91--assert-on-screen win (cdr before) "rail toggle on")
      (org-air-rail-toggle)
      (org-air-r91--assert-on-screen win (cdr before) "rail toggle off")
      (should (= (window-point win) (point))))))

(ert-deftest org-air-r91-calendar-month-nav-keeps-row-on-same-screen-line ()
  "Calendar month navigation (`<' `>' `.') repaints without a jump."
  (skip-unless (and (fboundp 'org-air-calendar-next)
                    (fboundp 'org-air-calendar-prev)
                    (fboundp 'org-air-calendar-today)))
  (org-air-r91--with-window-board
    (let* ((win (selected-window))
           (before (org-air-r91--park-anchor-row win 6)))
      (org-air-calendar-next)
      (org-air-r91--assert-stable win before "> calendar next")
      (org-air-calendar-prev)
      (org-air-r91--assert-stable win before "< calendar prev")
      (org-air-calendar-today)
      (org-air-r91--assert-stable win before ". calendar today"))))

(ert-deftest org-air-r91-process-inbox-keeps-row-on-same-screen-line ()
  "`I' (process inbox) repaints the board without a viewport jump."
  (skip-unless (fboundp 'org-air-process-inbox))
  (org-air-r91--with-window-board
    (let* ((win (selected-window))
           (before (org-air-r91--park-anchor-row win 6)))
      (org-air-process-inbox)
      (org-air-r91--assert-on-screen win (cdr before) "I process-inbox")
      (should (= (window-point win) (point))))))

(ert-deftest org-air-r91-resize-repaint-keeps-row-on-same-screen-line ()
  "The debounced resize repaint keeps the row on its screen line."
  (skip-unless (fboundp 'org-air-view--resize-refresh))
  (org-air-r91--with-window-board
    (let* ((win (selected-window))
           (before (org-air-r91--park-anchor-row win 6)))
      ;; Force the width-change branch to fire a real repaint.
      (setq org-air-view--rendered-width nil)
      (org-air-view--resize-refresh)
      (org-air-r91--assert-stable win before "resize repaint"))))

;;;; =====================================================================
;;;; 3. Hostile geometry and vanished rows.
;;;; =====================================================================

(ert-deftest org-air-r91-vanished-row-landing-stays-on-screen ()
  "A row that VANISHES from the repaint leaves the landing on-screen.
Filtering to a lens that matches nothing removes the row under point
entirely; the replacement landing must inherit its screen line rather
than jump."
  (skip-unless (fboundp 'org-air-filter))
  (org-air-r91--with-window-board
    (let* ((win (selected-window))
           (before (org-air-r91--park-anchor-row win 6)))
      (org-air-filter '("no-such-tag-anywhere"))
      (should-not (string-match-p (regexp-quote org-air-r91--anchor-title)
                                  (org-air-r91--row win)))
      (org-air-r91--assert-on-screen win (cdr before) "/ empty lens")
      (should (= (window-point win) (point))))))

(ert-deftest org-air-r91-point-at-buffer-start-repaint-stable ()
  "A repaint with point at `point-min' leaves the viewport at the top."
  (skip-unless (fboundp 'org-air-refresh))
  (org-air-r91--with-window-board
    (let ((win (selected-window)))
      (goto-char (point-min))
      (set-window-point win (point))
      (set-window-start win (point-min) t)
      (should (= 0 (org-air-r91--offset win)))
      (org-air-refresh)
      (should (= (point-min) (window-start win)))
      (org-air-r91--assert-on-screen win 0 "refresh at buffer start"))))

(ert-deftest org-air-r91-point-at-buffer-end-repaint-stable ()
  "A repaint with point at `point-max' keeps the last row's screen line."
  (skip-unless (fboundp 'org-air-refresh))
  (org-air-r91--with-window-board
    (let* ((win (selected-window))
           (offset (- (window-body-height win) 1)))
      (goto-char (point-max))
      (org-air-r91--park win offset)
      (let ((start (window-start win)))
        (org-air-refresh)
        (org-air-r91--assert-on-screen win offset "refresh at buffer end")
        (should (= start (window-start win)))
        (should (= (window-point win) (point)))))))

(ert-deftest org-air-r91-window-shorter-than-offset-lands-on-screen ()
  "A window that SHRANK below the saved offset still lands on-screen.
The offset is captured from `window-start', which a split does not
move, so after the split the recorded offset is larger than the window
can show; the repaint must clamp it to the CURRENT geometry rather than
scroll the row out of view."
  (skip-unless (fboundp 'org-air-refresh))
  (org-air-r91--with-window-board
    (let* ((win (selected-window))
           (tall (window-body-height win)))
      (skip-unless (>= tall 16))
      (org-air-r91--park-anchor-row win 15)
      ;; Shrink the board window without touching its `window-start'.
      (let ((other (split-window win nil 'below)))
        (should (window-live-p other))
        (should (< (window-body-height win) 15))
        ;; The recorded offset no longer fits.
        (should (>= (org-air-r91--offset win) (window-body-height win)))
        (org-air-refresh)
        (should (< (org-air-r91--offset win) (window-body-height win)))
        (should (= (window-point win) (point)))
        (should (string-match-p (regexp-quote org-air-r91--anchor-title)
                                (org-air-r91--row win)))))))

(ert-deftest org-air-r91-pane-resync-repaint-lands-on-final-geometry ()
  "A repaint whose TAIL changes the window height anchors on the FINAL one.
The pane/inspector resync legitimately opens or closes a window at the
end of a repaint, so the viewport must be installed against the
post-resync geometry.  Simulated here by shrinking the board window
inside the repaint."
  (skip-unless (fboundp 'org-air-view--refresh-repaint))
  (org-air-r91--with-window-board
    (let ((win (selected-window)))
      (skip-unless (>= (window-body-height win) 16))
      (org-air-r91--park-anchor-row win 15)
      (let ((split-done nil))
        (cl-letf* ((erase (symbol-function 'erase-buffer))
                   (buf (current-buffer))
                   ((symbol-function 'erase-buffer)
                    (lambda (&rest args)
                      (when (and (not split-done) (eq (current-buffer) buf))
                        (setq split-done t)
                        (ignore-errors (split-window win nil 'below)))
                      (apply erase args))))
          (org-air-view--refresh-repaint))
        (should split-done))
      (should (< (org-air-r91--offset win) (window-body-height win)))
      (should (= (window-point win) (point))))))

(ert-deftest org-air-r91-column-zero-landing-does-not-drift-one-line ()
  "A COLUMN-0 point must not drift up one screen line across a repaint.
`count-screen-lines' with COUNT-FINAL-NEWLINE nil drops the last line
when END sits right after a newline, which is exactly a column-0 point;
measuring that way makes the repaint nudge the row UP by one.  A small
jump is still a jump."
  (skip-unless (fboundp 'org-air-toggle-mark))
  (org-air-r91--with-window-board
    (let ((win (selected-window)))
      (org-air-r91--goto-title org-air-r91--anchor-title)
      (beginning-of-line)
      (should (= 0 (current-column)))
      (org-air-r91--park win 6)
      (let ((before (cons (window-start win) 6)))
        (org-air-toggle-mark)
        (org-air-r91--assert-stable win before "m from column 0")
        (org-air-refresh)
        (org-air-r91--assert-stable win before "g from column 0")))))

;;;; =====================================================================
;;;; 4. Several windows, and no window at all.
;;;; =====================================================================

(ert-deftest org-air-r91-two-windows-each-keep-own-viewport ()
  "Acting in one window must not move the OTHER window's row or point.
`erase-buffer' clobbers every non-selected window's `window-point' as
well as its `window-start', so a fix that only repaired
`(selected-window)' would leave the second window at `point-min'."
  (skip-unless (fboundp 'org-air-toggle-mark))
  (org-air-r91--with-window-board
    (let* ((w1 (selected-window))
           (w2 (split-window w1 nil 'below)))
      (set-window-buffer w2 (current-buffer))
      (should (= 2 (length (get-buffer-window-list (current-buffer)
                                                   'nomini t))))
      ;; w1 shows Task 40 at line 4; w2 shows Task 20 at line 3.
      (org-air-r91--goto-title org-air-r91--anchor-title)
      (org-air-r91--park w1 4)
      (save-excursion
        (org-air-r91--goto-title "Task 20")
        (set-window-point w2 (point))
        (set-window-start w2 (save-excursion (vertical-motion -3 w2) (point))
                          t))
      (should (= 3 (org-air-r91--offset w2)))
      (select-window w1)
      (goto-char (window-point w1))
      (let ((s1 (window-start w1)) (s2 (window-start w2))
            (r2 (org-air-r91--row w2)))
        (org-air-toggle-mark)
        ;; The acting window is stable ...
        (should (= s1 (window-start w1)))
        (should (= 4 (org-air-r91--offset w1)))
        (should (string-match-p (regexp-quote org-air-r91--anchor-title)
                                (org-air-r91--row w1)))
        ;; ... and so is the bystander, on its OWN row and OWN line.
        (should (= s2 (window-start w2)))
        (should (= 3 (org-air-r91--offset w2)))
        (should (equal r2 (org-air-r91--row w2)))
        (should (< (org-air-r91--offset w2) (window-body-height w2)))))))

(ert-deftest org-air-r91-headless-repaint-is-a-total-no-op ()
  "With the board displayed in NO window a repaint is silent and harmless."
  (skip-unless (fboundp 'org-air-toggle-mark))
  (org-air-r91--with-window-board
    (let* ((win (selected-window))
           (bystander (get-buffer-create "*org-air-r91-elsewhere*")))
      (unwind-protect
          (progn
            (org-air-r91--goto-title org-air-r91--anchor-title)
            (set-window-buffer win bystander)
            (should (null (get-buffer-window-list (current-buffer)
                                                  'nomini t)))
            (let ((start (window-start win))
                  (wpt (window-point win)))
              ;; No signal, and the unrelated window is untouched.
              (org-air-toggle-mark)
              (org-air-refresh)
              (org-air-toggle-section)
              (should (eq (window-buffer win) bystander))
              (should (= start (window-start win)))
              (should (= wpt (window-point win)))))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer bystander))))))

(ert-deftest org-air-r91-repaint-survives-window-closed-mid-repaint ()
  "A window deleted or re-bufferred during the repaint never breaks it."
  (skip-unless (fboundp 'org-air-view--refresh-repaint))
  (org-air-r91--with-window-board
    (let* ((w1 (selected-window))
           (w2 (split-window w1 nil 'below))
           (bystander (get-buffer-create "*org-air-r91-elsewhere*")))
      (unwind-protect
          (progn
            (set-window-buffer w2 (current-buffer))
            (org-air-r91--park-anchor-row w1 4)
            (let ((killed nil))
              (cl-letf* ((erase (symbol-function 'erase-buffer))
                         (buf (current-buffer))
                         ((symbol-function 'erase-buffer)
                          (lambda (&rest args)
                            (when (and (not killed) (eq (current-buffer) buf))
                              (setq killed t)
                              (ignore-errors (set-window-buffer w2 bystander)))
                            (apply erase args))))
                (org-air-view--refresh-repaint))
              (should killed))
            ;; The acting window is still correct; nothing signalled.
            (should (= 4 (org-air-r91--offset w1)))
            (should (= (window-point w1) (point)))
            (should (eq (window-buffer w2) bystander)))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer bystander))))))

;;;; =====================================================================
;;;; 5. The API contract and the deliberate exclusions.
;;;; =====================================================================

(ert-deftest org-air-r91-repaint-never-forces-window-start ()
  "Any `window-start' a repaint installs is installed NON-forcibly.
A forced start pins the viewport even when it would leave point
off-screen; a non-forced one lets redisplay re-anchor, which is the only
safe contract for a computed start.  Asserted on the core Emacs API, not
on any org-air internal."
  (skip-unless (fboundp 'org-air-toggle-mark))
  (org-air-r91--with-window-board
    (let* ((win (selected-window))
           (forced nil)
           (calls 0))
      (org-air-r91--park-anchor-row win 6)
      (cl-letf* ((orig (symbol-function 'set-window-start))
                 (buf (current-buffer))
                 ((symbol-function 'set-window-start)
                  (lambda (w pos &optional noforce)
                    (when (and (window-live-p w)
                               (eq (window-buffer w) buf))
                      (setq calls (1+ calls))
                      (unless noforce (setq forced t)))
                    (funcall orig w pos noforce))))
        (org-air-toggle-mark)
        (org-air-refresh))
      (should (> calls 0))
      (should-not forced))))

(ert-deftest org-air-r91-plain-section-motion-is-not-anchored ()
  "Plain motion to the next section is an explicit jump, not a repaint.
TAB on a non-header, non-fold row moves point and must leave
`window-start' to ordinary Emacs scrolling — nothing may pin the old
screen line onto a deliberate jump."
  (skip-unless (fboundp 'org-air-next-section))
  (org-air-r91--with-window-board
    (let ((win (selected-window)))
      (org-air-r91--park-anchor-row win 6)
      (let ((start (window-start win))
            (pt (point))
            (tick (buffer-chars-modified-tick)))
        (org-air-toggle-section)        ; the third branch: plain motion
        ;; Point moved, the buffer was NOT repainted, the viewport marker
        ;; was left exactly where it was.
        (should-not (= pt (point)))
        (should (= tick (buffer-chars-modified-tick)))
        (should (= start (window-start win)))
        (should (= (window-point win) (point)))))))

(ert-deftest org-air-r91-board-open-does-not-inherit-a-stale-viewport ()
  "Opening the board is a creation, not a repaint: it anchors at the top."
  (skip-unless (fboundp 'org-air))
  (org-air-r91--with-window-board
    (let ((win (selected-window)))
      (org-air-r91--park-anchor-row win 6)
      ;; A fresh open of the same board buffer.
      (org-air)
      (let ((w (get-buffer-window (get-buffer org-air-view-buffer-name))))
        (should (window-live-p w))
        (should (< (org-air-r91--offset w) (window-body-height w)))))))

(ert-deftest org-air-r91-bookmark-jump-owns-its-landing ()
  "With a bookmark locator armed the JUMP owns the landing, not the seam.
A bookmark restore is an explicit request to go somewhere else, so the
pre-repaint viewport is meaningless; the only contract is that the
repaint lands on the bookmarked row and the acting window follows it."
  (skip-unless (and (boundp 'org-air-view--bookmark-locator)
                    (fboundp 'org-air-view--render-current)))
  (org-air-r91--with-window-board
    (let* ((win (selected-window)))
      (org-air-r91--park-anchor-row win 6)
      (setq org-air-view--bookmark-locator (list :title "Task 12"))
      (org-air-view--render-current)
      (should (string-match-p "Task 12" (org-air-r91--row win)))
      (should (= (window-point win) (point)))
      (should (null org-air-view--bookmark-locator)))))

(ert-deftest org-air-r91-visit-item-leaves-other-board-window-alone ()
  "Visiting an item is a jump in ANOTHER buffer; the board keeps its view."
  (skip-unless (fboundp 'org-air-visit-item))
  (org-air-r91--with-window-board
    (let* ((board (current-buffer))
           (win (selected-window)))
      (org-air-r91--park-anchor-row win 4)
      (let ((before (mapcar (lambda (w) (cons w (window-start w)))
                            (get-buffer-window-list board 'nomini t)))
            (tick (buffer-chars-modified-tick)))
        (should before)
        (ignore-errors (org-air-visit-item))
        ;; The board buffer was never repainted ...
        (with-current-buffer board
          (should (= tick (buffer-chars-modified-tick))))
        ;; ... so every window still showing it kept its exact viewport.
        (pcase-dolist (`(,w . ,start) before)
          (when (and (window-live-p w) (eq (window-buffer w) board))
            (should (= start (window-start w)))))))))

;;;; =====================================================================
;;;; 6. The other views: the shared refresh must not strand the landing.
;;;; =====================================================================

(defun org-air-r91--other-view-landing-on-screen (open mode)
  "Open a non-board view via OPEN, refresh it, assert MODE lands on-screen."
  (org-air-r91--with-corpus
    (org-air-viewport-test--with-frozen-now
      (save-window-excursion
        (let ((buf nil))
          (unwind-protect
              (when (ignore-errors (funcall open) t)
                (setq buf (current-buffer))
                (should (derived-mode-p mode))
                (set-window-buffer (selected-window) buf)
                (select-window (get-buffer-window buf))
                (with-current-buffer buf
                  (let ((win (selected-window)))
                    (goto-char (point-min))
                    (forward-line (min 6 (max 0 (1- (line-number-at-pos
                                                     (point-max))))))
                    (set-window-point win (point))
                    (set-window-start
                     win (save-excursion (vertical-motion -3 win) (point)) t)
                    (org-air-view--refresh-current)
                    ;; The landing (whatever the view chose) is visible and
                    ;; the acting window follows it.
                    (should (= (window-point win) (point)))
                    (should (< (org-air-r91--offset win)
                               (window-body-height win))))))
            (let ((kill-buffer-query-functions nil))
              (when (buffer-live-p buf) (kill-buffer buf)))))))))

(ert-deftest org-air-r91-review-shared-refresh-lands-on-screen ()
  "The review view's shared refresh leaves its landing inside the window."
  (skip-unless (fboundp 'org-air-review))
  (org-air-r91--other-view-landing-on-screen
   #'org-air-review 'org-air-review-mode))

(ert-deftest org-air-r91-revisit-shared-refresh-lands-on-screen ()
  "The revisit view's shared refresh leaves its landing inside the window."
  (skip-unless (fboundp 'org-air-revisit))
  (org-air-r91--other-view-landing-on-screen
   #'org-air-revisit 'org-air-revisit-mode))

(provide 'org-air-round91-test)
;;; org-air-round91-test.el ends here
