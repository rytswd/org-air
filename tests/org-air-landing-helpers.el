;;; org-air-landing-helpers.el --- landing / viewport / echo instruments -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared scaffolding for the three R92 suites:
;;
;;   tests/org-air-round92-test.el          — the R92 fences (PART A)
;;   tests/org-air-invariant-landing-test.el — invariant family P2 (PART B)
;;   tests/org-air-invariant-echo-test.el    — invariant family P1 (PART C)
;;
;; The R91 distinguished review measured the structural hole this file
;; exists to close: over 95 test files, `window-start' was asserted in 3
;; and `current-message' in ZERO.  The suite asserted what the RENDERER
;; PRODUCES and almost never what the USER PERCEIVES, which is how 1204
;; green tests coexisted with a product that scrolled the cursor's row
;; off screen on every keystroke — and how R91's own 31 new tests were
;; blind to the regression that same round shipped (they asserted
;; `offset < window-body-height', which is true both with and without a
;; banner scrolled off the top).
;;
;; Everything here is an OBSERVABLE instrument.  Nothing an assertion
;; built on these helpers can see is an org-air internal: the helpers
;; return buffer text, point, `window-start', `window-point', a 0-based
;; SCREEN-line offset, the list of lines a window actually shows, and
;; the echo-area text of a keystroke.
;;
;; Four corpora, one per view, each rendered into a REAL window:
;;
;;   BOARD    60 `* TODO Task NN' headings, `Needs attention' expanded so
;;            the board is TALLER than the batch window.
;;   PROJECT  40 Air docs (ready / dropped / draft), so the view carries a
;;            collapsed `… N dropped — TAB to show' fold row.
;;   REVISIT  30 prose (knowledge) notes with `org-air-revisit-page-limit'
;;            bound small, so the view carries an `…and N more' fold row.
;;   REVIEW   30 CLOSED items inside the frozen week plus 12 open ones, so
;;            Completed / Started / Carried over all have rows.
;;
;; INSTRUMENT NOTES (stated plainly, as R91's file did for its own):
;;
;; * `pos-visible-in-window-p' is unusable in --batch (a batch frame never
;;   realises glyph matrices), so VISIBILITY is screen-line arithmetic:
;;   the window shows the `window-body-height' display lines starting at
;;   `window-start', which is exact.  Nothing here is pixel-dependent, so
;;   nothing built on it belongs in the GUI-skip set.
;; * `current-message' is ALWAYS nil in --batch (a batch Emacs has no echo
;;   area; `message' goes to stderr).  `org-air-landing-test-echo-of'
;;   therefore prefers `current-message' when it is available — a real
;;   frame — and falls back to the last line the keystroke appended to the
;;   message LOG (`*Messages*'), which is the same string the echo area
;;   would have shown.  The assertion is still on user-visible TEXT.

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

;;;; =====================================================================
;;;; 1. Observable viewport instruments
;;;; =====================================================================

(defun org-air-landing-test-offset (win)
  "Return the 0-based SCREEN line of WIN's point inside WIN.
COUNT-FINAL-NEWLINE is non-nil: with it nil `count-screen-lines' drops
the last line whenever END sits right after a newline, so a point at
COLUMN 0 would measure one screen line too high."
  (max 0 (1- (count-screen-lines (window-start win) (window-point win)
                                 t win))))

(defun org-air-landing-test-park (win offset)
  "Anchor WIN so its point sits OFFSET screen lines below `window-start'.
Returns the (WINDOW-START . OFFSET) pair the repaint must reproduce."
  (set-window-point win (point))
  (set-window-start win (save-excursion (vertical-motion (- offset) win)
                                        (point))
                    t)
  (should (= offset (org-air-landing-test-offset win)))
  (cons (window-start win) offset))

(defun org-air-landing-test-visible-lines (win)
  "Return the buffer lines WIN actually SHOWS, top line first.
The window displays `window-body-height' DISPLAY lines starting at
`window-start'; this walks them with `vertical-motion' (which counts
display lines, so it is correct under wrapping too) and returns each
line's trimmed text.  This is the instrument R91 lacked: its other-view
tests could only say \"the landing is on screen\", which is true both
with and without the view's own banner scrolled off the top."
  (with-current-buffer (window-buffer win)
    (save-excursion
      (goto-char (window-start win))
      (let ((lines nil)
            (rows (window-body-height win)))
        (dotimes (_ rows)
          (push (string-trim
                 (buffer-substring-no-properties (line-beginning-position)
                                                 (line-end-position)))
                lines)
          (when (zerop (vertical-motion 1 win))
            (setq rows 0)))
        (nreverse lines)))))

(defun org-air-landing-test-visible-p (win regexp)
  "Return non-nil when WIN shows a line matching REGEXP."
  (and (seq-find (lambda (line) (string-match-p regexp line))
                 (org-air-landing-test-visible-lines win))
       t))

(defun org-air-landing-test-row (&optional win)
  "Return the trimmed text of the line at WIN's point (or at point)."
  (save-excursion
    (when win (goto-char (window-point win)))
    (string-trim (buffer-substring-no-properties (line-beginning-position)
                                                 (line-end-position)))))

(defvar org-air-landing-test-id-regexp nil
  "Regexp extracting the ROW IDENTITY from a row's text, per corpus.
Every corpus below names its rows with a unique, render-stable token
\(`Document number 07', `Note number 12', `Task number 29') so a row's
identity is readable straight out of the BUFFER TEXT — no org-air
internal, no struct, no text property is consulted by any assertion.")

(defun org-air-landing-test-row-id (&optional win)
  "Return the identity of the row at WIN's point, from the row's TEXT.
The corpus identity token when the row carries one (an item / doc /
note row), else the whole trimmed line — which is what names a section
header or a fold row."
  (let ((text (org-air-landing-test-row win)))
    (or (and org-air-landing-test-id-regexp
             (string-match org-air-landing-test-id-regexp text)
             (match-string 0 text))
        text)))

(defun org-air-landing-test-goto (regexp)
  "Move point onto the first row whose text matches REGEXP.
Lands on the row's TITLE the way every org-air motion does, so the
column the repaint must preserve is a realistic one."
  (goto-char (point-min))
  (should (re-search-forward regexp nil t))
  (beginning-of-line)
  (org-air-view--goto-row-title)
  (point))

(defun org-air-landing-test-on-glyph-p ()
  "Return non-nil when point sits on a VISIBLE GLYPH of its row.
The observable form of \"never the gutter, never the trailing pad\": the
character under the cursor is a real, non-blank glyph of the row's text.
A cursor parked in the leading indent, in the two-pane gutter, past the
row's last glyph or on `point-min' by accident fails this."
  (let ((ch (char-after)))
    (and ch (not (memq ch '(?\s ?\t ?\n))))))

(defun org-air-landing-test-any-row-p ()
  "Return non-nil when the current view renders at least one ROW.
An EMPTY view (a period with no work in it, a filter that matched
nothing) has no row to land on, so the landing laws that talk about rows
do not apply to it — and saying that out loud is better than a silent
exception inside an assertion."
  (save-excursion
    (goto-char (point-min))
    (let ((found nil))
      (while (and (not found) (not (eobp)))
        (if (or (org-air-view--row-property 'org-air-item)
                (org-air-view--row-property 'org-air-doc)
                (org-air-view--row-property 'org-air-revisit))
            (setq found t)
          (forward-line 1)))
      found)))

(defun org-air-landing-test-first-row-p ()
  "Return non-nil when point is on the FIRST row of the current view.
The first row is the earliest line carrying an item / doc / note / more
row property; the fallback landing of every view is exactly this line."
  (let ((line (line-number-at-pos))
        (first (save-excursion
                 (goto-char (point-min))
                 (let ((found nil))
                   (while (and (not found) (not (eobp)))
                     (if (or (org-air-view--row-property 'org-air-item)
                             (org-air-view--row-property 'org-air-doc)
                             (org-air-view--row-property 'org-air-revisit))
                         (setq found (line-number-at-pos))
                       (forward-line 1)))
                   found))))
    (and first (= line first))))

;;;; =====================================================================
;;;; 2. The echo-area instrument (invariant family P1)
;;;; =====================================================================

(defvar org-air-landing-test--echo-serial 0
  "Counter making each echo probe's log sentinel unique.")

(defun org-air-landing-test-echo-of (thunk)
  "Return the echo-area text the user sees while THUNK runs, or nil.
Prefers `current-message' (a real frame).  In --batch there IS no echo
area — `current-message' is always nil and `message' goes to stderr — so
the fallback is the LAST line the thunk appended to the message log,
which is character-for-character the string the echo area would show.

A UNIQUE sentinel is logged first: Emacs collapses a message that repeats
the previous one, so without it a keystroke echoing exactly what the
previous keystroke echoed would look like silence.  The sentinel itself
is never displayed.

Errors signalled by THUNK propagate; use `org-air-landing-test-refusal-of'
for the refusal (`user-error') text."
  (let ((log (messages-buffer))
        (message-log-max t)
        (start nil))
    (let ((inhibit-message t))
      (message "org-air-landing-test echo probe %d"
               (setq org-air-landing-test--echo-serial
                     (1+ org-air-landing-test--echo-serial))))
    (with-current-buffer log
      (setq start (point-max)))
    (let ((inhibit-message nil))
      (funcall thunk))
    (or (current-message)
        (with-current-buffer log
          (let* ((text (buffer-substring-no-properties start (point-max)))
                 (lines (split-string (string-trim text) "\n" t)))
            (car (last lines)))))))

(defun org-air-landing-test-refusal-of (thunk)
  "Return the REFUSAL text THUNK signals (`user-error'/`error'), or nil.
The user sees a refusal in the same echo area as a `message', so it is
asserted with the same instrument and in the same user-visible form."
  (condition-case err
      (progn (funcall thunk) nil)
    (error (error-message-string err))))

;;;; =====================================================================
;;;; 3. Corpora — one per view, each in a REAL window
;;;; =====================================================================

(defvar org-air-landing-test--dir nil
  "Temporary corpus directory of the running landing test.")

(defconst org-air-landing-test-frozen
  (floor (float-time (encode-time (list 0 0 10 15 6 2026 nil -1 nil))))
  "Frozen \"now\": Mon 2026-06-15 10:00 local, inside ISO week 25 2026.
The review corpus's CLOSED stamps all fall inside that week.")

(defmacro org-air-landing-test--frozen (&rest body)
  "Run BODY with the Lisp-visible clock frozen (`current-time'/`float-time').
`float-time' WITH an argument passes through, so timestamp parsing and
period arithmetic stay real."
  (declare (indent 0) (debug t))
  `(cl-letf* ((org-air-landing-test--real-ft (symbol-function 'float-time))
              ((symbol-function 'float-time)
               (lambda (&optional time)
                 (if time (funcall org-air-landing-test--real-ft time)
                   (float org-air-landing-test-frozen))))
              ((symbol-function 'current-time)
               (lambda () (seconds-to-time org-air-landing-test-frozen))))
     ,@body))

(defun org-air-landing-test--write (name content)
  "Write CONTENT into corpus file NAME (subdirectories created)."
  (let ((path (expand-file-name name org-air-landing-test--dir))
        (coding-system-for-write 'utf-8-unix)
        (file-name-handler-alist nil))
    (make-directory (file-name-directory path) t)
    (write-region (or content "") nil path nil 'silent)))

(defun org-air-landing-test--reset-tables ()
  "Clear the GLOBAL query-layer tables between corpora."
  (when (fboundp 'org-air-query-teardown)
    (ignore-errors (org-air-query-teardown))
    (clrhash org-air-query--file-meta)
    (clrhash org-air-query--visits)
    (clrhash org-air-query--denote-id-index)
    (setq org-air-query--link-graph-dirty nil)))

(defun org-air-landing-test--cleanup (names)
  "Kill the view buffers NAMES, the corpus buffers, and the corpus dir."
  (org-air-landing-test--reset-tables)
  (let ((kill-buffer-query-functions nil))
    (dolist (name names)
      (when (get-buffer name) (kill-buffer name)))
    (dolist (buf (buffer-list))
      (let ((fn (buffer-file-name buf)))
        (when (and fn org-air-landing-test--dir
                   (string-prefix-p org-air-landing-test--dir fn))
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf)))))
  (when (and org-air-landing-test--dir
             (file-directory-p org-air-landing-test--dir))
    (delete-directory org-air-landing-test--dir t)))

(defmacro org-air-landing-test--with-corpus (prefix &rest body)
  "Create a fresh temp corpus dir named PREFIX and run BODY isolated."
  (declare (indent 1) (debug t))
  `(let ((org-air-landing-test--dir (make-temp-file ,prefix t)))
     (unwind-protect
         (progn
           (org-air-landing-test--reset-tables)
           ;; Every GLOBAL a driven command may `setq' is rebound here, so
           ;; a landing / echo test can never leak view state into another
           ;; file's expectations (`org-air-filter-match' and
           ;; `org-air-project-group' are set globally by their verbs).
           (let ((find-file-hook (copy-sequence find-file-hook))
                 (create-lockfiles nil)
                 (org-tags-column 0)
                 (bookmark-alist nil)
                 (org-air-filter-match org-air-filter-match)
                 (org-air-project-group org-air-project-group)
                 (org-air-view--edit-ring nil)
                 (org-air-view--edit-redo-ring nil))
             ,@body))
       (org-air-landing-test--cleanup
        (list "*org-air-landing*" "*org-air-project*"
              org-air-revisit-buffer-name org-air-review-buffer-name
              org-air-rail-buffer-name)))))

(defun org-air-landing-test--select (buffer)
  "Select BUFFER's window and return it; assert the view is displayed."
  (let ((win (get-buffer-window buffer)))
    (should (window-live-p win))
    (select-window win)
    win))

;;;; ---------------------------------------------------------------------
;;;; BOARD
;;;; ---------------------------------------------------------------------

(defconst org-air-landing-test-board-count 60
  "TODO rows in the board corpus — taller than the batch window.")

(defmacro org-air-landing-test-with-board (&rest body)
  "Open the BOARD over a 60-row corpus in a real window; run BODY in it.
The `Needs attention' section is expanded so the board outgrows the
window, which is the only interesting geometry for a viewport rule.

R93: every task carries an inactive stamp in its OWN body, 60 days
before the frozen clock.  Needs attention is now an AGING rule (quiet
for >= the priority's `org-air-attention-days' threshold, 30 by default
for a cookie-less heading), so a corpus written a millisecond ago holds
no rows at all and the section this macro expands would be EMPTY -- the
board would then be shorter than the window and every viewport law below
would fail on its precondition instead of on its subject.  A stamp per
heading is what a real user's files carry (`org-log-into-drawer'), and
it is the heading's OWN clock, so it survives the writes these suites
drive."
  (declare (indent 0) (debug t))
  `(org-air-landing-test--with-corpus "org-air-lb-"
     (org-air-landing-test--write
      "tasks.org"
      (let ((quiet (org-air-test-quiet-stamp)))
        (mapconcat (lambda (i)
                     (if (zerop (% i 2))
                         (format "* TODO Task number %02d :focus:\n%s\n" i quiet)
                       (format "* TODO Task number %02d\n%s\n" i quiet)))
                   (number-sequence 0 (1- org-air-landing-test-board-count)) "")))
     (org-air-landing-test--write "inbox.org" "")
     (let ((org-air-files (list org-air-landing-test--dir))
           (org-air-inbox-file
            (expand-file-name "inbox.org" org-air-landing-test--dir))
           (org-air-cache-file
            (expand-file-name ".cache/board.eld" org-air-landing-test--dir))
           (org-air-view-buffer-name "*org-air-landing*")
           (org-air-backlog-tag "backlog")
           (org-air-plain-heading-type 'task)
           (org-air-landing-test-id-regexp "Task number [0-9]+")
           (inhibit-message t))
       (org-air-landing-test--frozen
         (save-window-excursion
           (org-air)
           (let* ((buf (get-buffer org-air-view-buffer-name)))
             (should buf)
             (org-air-landing-test--select buf)
             (with-current-buffer buf
               (let ((pos (org-air-view--find-property
                           'org-air-section 'attention)))
                 (should pos)
                 (goto-char pos)
                 (org-air-toggle-section))
               (should (> (line-number-at-pos (point-max))
                          (window-body-height (selected-window))))
               ,@body)))))))

;;;; ---------------------------------------------------------------------
;;;; PROJECT
;;;; ---------------------------------------------------------------------

(defconst org-air-landing-test-project-count 40
  "Air docs in the project corpus (every third one dropped).")

(defmacro org-air-landing-test-with-project (&rest body)
  "Open the PROJECT view over a 40-doc Air tree in a real window.
Every third doc is `dropped', so the view renders a collapsed
`… N dropped — TAB to show' fold row: the row whose TAB threw the
cursor 41 screen lines off in the reviewer's measurement."
  (declare (indent 0) (debug t))
  `(org-air-landing-test--with-corpus "org-air-lp-"
     (org-air-landing-test--write "air-config.toml" "")
     (dotimes (i org-air-landing-test-project-count)
       (org-air-landing-test--write
        (format "air/v0.1/doc-%02d.org" i)
        (format "#+title: Document number %02d\n#+state: %s\n#+FILETAGS: :%s:\n* Notes\n"
                i (nth (% i 3) '("ready" "dropped" "draft"))
                (if (zerop (% i 2)) "even" "odd"))))
     (let ((org-air-sources (list (list :air org-air-landing-test--dir)))
           (org-air-files (list org-air-landing-test--dir))
           (org-air-landing-test-id-regexp "Document number [0-9]+")
           (inhibit-message t))
       (org-air-landing-test--frozen
         (save-window-excursion
           (org-air-project org-air-landing-test--dir)
           (let ((buf (get-buffer "*org-air-project*")))
             (should buf)
             (org-air-landing-test--select buf)
             (with-current-buffer buf
               (should (derived-mode-p 'org-air-project-mode))
               ,@body)))))))

;;;; ---------------------------------------------------------------------
;;;; REVISIT
;;;; ---------------------------------------------------------------------

(defconst org-air-landing-test-revisit-count 30
  "Prose (knowledge) notes in the revisit corpus.")

(defmacro org-air-landing-test-with-revisit (&rest body)
  "Open the REVISIT view over 30 prose notes in a real window.
`org-air-revisit-page-limit' is bound to 12 so the view carries a real
`…and N more' fold row and the paging verb has something to do."
  (declare (indent 0) (debug t))
  `(org-air-landing-test--with-corpus "org-air-lv-"
     (dotimes (i org-air-landing-test-revisit-count)
       (org-air-landing-test--write
        (format "note-%02d.org" i)
        (format "#+title: Note number %02d\n#+filetags: :%s:\n\n* Body\nProse only.\n"
                i (if (zerop (% i 2)) "even" "odd"))))
     (org-air-landing-test--write "inbox.org" "")
     (let ((org-air-files (list org-air-landing-test--dir))
           (org-air-inbox-file
            (expand-file-name "inbox.org" org-air-landing-test--dir))
           (org-air-cache-file
            (expand-file-name ".cache/board.eld" org-air-landing-test--dir))
           (org-air-revisit-page-limit 12)
           (org-air-landing-test-id-regexp "Note number [0-9]+")
           (inhibit-message t))
       (org-air-landing-test--frozen
         (save-window-excursion
           (org-air-revisit)
           (let ((buf (get-buffer org-air-revisit-buffer-name)))
             (should buf)
             (org-air-landing-test--select buf)
             (with-current-buffer buf
               (should (derived-mode-p 'org-air-revisit-mode))
               ,@body)))))))

;;;; ---------------------------------------------------------------------
;;;; REVIEW
;;;; ---------------------------------------------------------------------

(defconst org-air-landing-test-review-count 30
  "CLOSED items in the review corpus, all inside the frozen week.")

(defmacro org-air-landing-test-with-review (&rest body)
  "Open the REVIEW view over a frozen week of finished work.
30 CLOSED items land in `Completed'; 12 open TODOs with an in-week
SCHEDULED stamp keep `Started' / `Carried over' populated, so the
section headers TAB toggles are real rows and the shared `b' backlog
verb has an item to act on."
  (declare (indent 0) (debug t))
  `(org-air-landing-test--with-corpus "org-air-lw-"
     (dotimes (i org-air-landing-test-review-count)
       (org-air-landing-test--write
        (format "done-%02d.org" i)
        (format "#+title: Done file %02d\n\n* DONE Task number %02d :%s:\nCLOSED: [2026-06-%02d Tue 09:%02d]\n"
                i i (if (zerop (% i 2)) "even" "odd") (+ 15 (% i 5)) i)))
     (dotimes (i 12)
       (org-air-landing-test--write
        (format "open-%02d.org" i)
        (format "#+title: Open file %02d\n\n* TODO Open item %02d :%s:\nSCHEDULED: <2026-06-%02d Tue>\n"
                i i (if (zerop (% i 2)) "even" "odd") (+ 15 (% i 5)))))
     (org-air-landing-test--write "inbox.org" "")
     (let ((org-air-files (list org-air-landing-test--dir))
           (org-air-inbox-file
            (expand-file-name "inbox.org" org-air-landing-test--dir))
           (org-air-cache-file
            (expand-file-name ".cache/board.eld" org-air-landing-test--dir))
           (org-air-backlog-tag "backlog")
           (org-air-landing-test-id-regexp
            "\\(?:Task number\\|Open item\\) [0-9]+")
           (inhibit-message t))
       (org-air-landing-test--frozen
         (save-window-excursion
           (org-air-review)
           (let ((buf (get-buffer org-air-review-buffer-name)))
             (should buf)
             (org-air-landing-test--select buf)
             (with-current-buffer buf
               (should (derived-mode-p 'org-air-review-mode))
               ,@body)))))))

;;;; =====================================================================
;;;; 4. Chrome — what the view's own header band looks like
;;;; =====================================================================

(defconst org-air-landing-test-chrome
  '((org-air-view-mode    . "org-air ∙")
    (org-air-project-mode . "org-air ∙ project")
    (org-air-revisit-mode . "org-air ∙ revisit")
    (org-air-review-mode  . "org-air ∙ review"))
  "The BANNER line of each view — the first thing a user reads.
The R91 regression scrolled exactly this line, plus the state-summary
line under it, off the top of the window; no assertion in any of the 95
test files said it had to be visible.")

(defun org-air-landing-test-banner (mode)
  "Return the banner regexp of MODE."
  (regexp-quote (or (cdr (assq mode org-air-landing-test-chrome))
                    "org-air ∙")))

(provide 'org-air-landing-helpers)
;;; org-air-landing-helpers.el ends here
