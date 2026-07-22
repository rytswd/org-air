;;; org-air-round68-test.el --- executing ERTs for round-68 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for round-68 (air/v0.1/org-air-round68-design.org):
;; the board's `T' (`org-air-item-cycle-todo') silently no-oped under
;; fast-selection todo keywords — a bare nil-arg `(org-todo)' inside
;; the undisplayed-buffer macro `org-air-view--at-item-source' routed
;; into `org-fast-todo-selection' even from pure Lisp, a synchronous
;; key read against a buffer the user never saw; a `DROPPED(x@)'-style
;; log note was a second trap on the same path.  R68-1 reframes `T' to
;; SELECT-WITH-COMPLETION through the extracted shared reader
;; `org-air-inbox--read-todo-keyword' (completing over the item's OWN
;; file vocabulary — R57) + an explicit-string `(org-todo CHOICE)';
;; R68-2 installs the board-context logging discipline
;; (`org-inhibit-logging' 'note + reschedule/redeadline 'note→'time +
;; the synchronous pre-save `org-air-inbox--flush-pending-log-note');
;; R68-3 puts the discipline in the shared macro itself (class fix)
;; and mirrors it into `org-air-inbox--apply-item-edits' and the
;; `org-air-refile-item' engine.  All BATCH/headless: the board is
;; built over a temp corpus (the house harness idiom) and
;; `completing-read' / `org-fast-todo-selection' are stubbed via
;; `cl-letf' — no interactive loop anywhere.  The spec's ten seams map
;; onto the ten ERTs (r68-1 is the bare-`(org-todo)' revert-RED
;; anchor).
;;
;; GUI residue (screenshot-confirm, not ERT-able): the minibuffer
;; completion rendering itself.  Goldens: only the `C-c' legend string
;; ("cycle todo state" → "set todo state") shifted — string-level only.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'org-air-test-helpers)

(when (locate-library "org-air")
  (require 'org-air))

;;;; -------------------------------------------------------------------
;;;; Fixture: the spec corpus (the user's EXACT fast-key vocabulary)
;;;; -------------------------------------------------------------------

(defvar org-air-r68--dir nil
  "The temp corpus directory of the current `org-air-r68--with-corpus'.")

(defconst org-air-r68--default-specs
  '(("inbox.org" . "#+title: inbox\n")
    ("vocab.org" . "#+TODO: TODO(t) DRAFT(d) READY(r) WIP(w) | COMP(c!) DROPPED(x@)\n\n* TODO Fix the widget :inbox:\n  body\n")
    ("wait.org" . "#+TODO: WAIT | ARCHIVED\n\n* TODO Jot\n  body\n")
    ("atdone.org" . "#+TODO: TODO | DONE(d@)\n\n* TODO At-done thing\n  body\n")
    ("sched.org" . "#+title: sched\n\n* TODO Dated thing :inbox:\nSCHEDULED: <2026-07-25 Sat>\n  body\n")
    ("target.org" . "#+TODO: TODO(t) DRAFT(d) READY(r) WIP(w) | COMP(c!) DROPPED(x@)\n\n* Existing\n"))
  "The spec fixture: `vocab.org' carries the user's EXACT fast-key
`#+TODO:' line as a file-local declaration; `wait.org' a disjoint
`WAIT | ARCHIVED' vocabulary (its `* TODO Jot' heading is keyword-LESS
there — TODO is not in that file's vocabulary); `atdone.org' an
`@'-carrying FIRST done keyword; `sched.org' an already-SCHEDULED item
for the reschedule-note seam; `target.org' a refile target declaring
the same fast-key vocabulary.")

(defmacro org-air-r68--with-corpus (specs &rest body)
  "Create a temp Org corpus from SPECS and run BODY against it.
SPECS is a list of (NAME . CONTENT) files (nil = the default spec
corpus).  Binds `org-air-files' to the directory, `org-air-inbox-file'
to its inbox.org, a temp `org-air-cache-file', a round-local board
buffer name, fresh form/last state, `org-tags-column' 0 (the r67
byte-stability shape) and `org-air-plain-heading-type' `task' (a
documented defcustom choice) so `wait.org's keyword-less heading is
board material and can carry a row.  Kills every corpus-visiting
buffer and deletes the directory afterwards."
  (declare (indent 1) (debug t))
  `(let* ((org-air-r68--dir (make-temp-file "org-air-r68-" t)))
     (unwind-protect
         (progn
           (pcase-dolist (`(,name . ,content)
                          (or ,specs org-air-r68--default-specs))
             (let ((file-name-handler-alist nil)
                   (coding-system-for-write 'utf-8-unix))
               (write-region (or content "") nil
                             (expand-file-name name org-air-r68--dir)
                             nil 'silent)))
           (let ((org-air-files (list org-air-r68--dir))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-r68--dir))
                 (org-air-cache-file
                  (expand-file-name ".cache/board.eld" org-air-r68--dir))
                 (org-air-view-buffer-name "*org-air-r68*")
                 (org-air-inbox--refile-form nil)
                 (org-air-inbox--refile-last nil)
                 (org-air-plain-heading-type 'task)
                 (org-tags-column 0)
                 (create-lockfiles nil)
                 (inhibit-message t))
             ,@body))
       (when (fboundp 'org-air-query-teardown)
         (org-air-query-teardown))
       (let ((kill-buffer-query-functions nil))
         (dolist (buf (buffer-list))
           (let ((fn (buffer-file-name buf)))
             (when (and fn (string-prefix-p org-air-r68--dir fn))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r68--dir t))))

(defmacro org-air-r68--with-board (specs &rest body)
  "Render the real board over the SPECS corpus; run BODY in its buffer.
The house harness idiom: `org-air' renders synchronously in batch; the
round-local board buffer is killed afterwards."
  (declare (indent 1) (debug t))
  `(org-air-r68--with-corpus ,specs
     (unwind-protect
         (progn
           (org-air)
           (let ((buf (get-buffer org-air-view-buffer-name)))
             (should buf)
             (with-current-buffer buf
               ,@body)))
       (let ((kill-buffer-query-functions nil)
             (buf (get-buffer org-air-view-buffer-name)))
         (when buf (kill-buffer buf))))))

(defun org-air-r68--goto-row (title)
  "Move point onto the board row whose item title contains TITLE.
Line-walks the render for the `org-air-item' text property (rows carry
it past the leading margin — the R22-2 shape).  Returns the row's
item; fails the test when no such row renders."
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (not (eobp)))
      (let ((p (line-beginning-position))
            (eol (line-end-position)))
        (while (and (not found) (< p eol))
          (let ((it (get-text-property p 'org-air-item)))
            (when (and it
                       (string-match-p (regexp-quote title)
                                       (or (org-air-item-title it) "")))
              (goto-char p)
              (setq found it)))
          (setq p (1+ p))))
      (unless found (forward-line 1)))
    (should found)
    found))

(defun org-air-r68--file (name)
  "Return the corpus file NAME's absolute path."
  (expand-file-name name org-air-r68--dir))

(defun org-air-r68--text (name)
  "Return corpus file NAME's on-disk content as a string."
  (with-temp-buffer
    (insert-file-contents (org-air-r68--file name))
    (buffer-string)))

(defun org-air-r68--item (name text)
  "Build an item for the heading containing TEXT in corpus file NAME.
Tags/todo read at the heading in the file's own buffer (its `#+TODO:'
vocabulary applies), the marker freshly positioned — the r67 idiom for
driving the non-board appliers directly."
  (let ((file (org-air-r68--file name)))
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (re-search-forward (regexp-quote text))
       (org-back-to-heading t)
       (org-air-item-create
        :title (substring-no-properties (org-get-heading t t t t))
        :tags (org-get-tags nil t)
        :todo (org-get-todo-state)
        :file file
        :marker (point-marker))))))

(defun org-air-r68--log-pending-p ()
  "Non-nil when an Org log record is still pending on `post-command-hook'."
  (and (memq 'org-add-log-note post-command-hook) t))

;;;; -------------------------------------------------------------------
;;;; r68-1 — THE REPRO: T applies an explicit keyword, never fast-selection
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r68-1-board-T-never-enters-fast-selection ()
  "Point on the `vocab.org' row (the user's exact fast-key vocabulary):
with `org-fast-todo-selection' stubbed to SIGNAL and `completing-read'
→ \"READY\", `org-air-item-cycle-todo' applies `* READY …' at the
source heading, raises no signal, and the file is SAVED (the visiting
buffer carries no dirty residue).  RED on revert: the pre-fix bare
`(org-todo)' routes into fast-selection even from batch Lisp under a
fast-key vocabulary — restoring the nil-arg call makes the stub
signal."
  (skip-unless (locate-library "org-air"))
  (org-air-r68--with-board nil
    (org-air-r68--goto-row "Fix the widget")
    (cl-letf (((symbol-function 'org-fast-todo-selection)
               (lambda (&rest _) (error "fast-selection entered")))
              ((symbol-function 'completing-read)
               (lambda (&rest _) "READY")))
      (org-air-item-cycle-todo))
    (should (string-match-p "^\\* READY Fix the widget :inbox:$"
                            (org-air-r68--text "vocab.org")))
    (should-not (buffer-modified-p
                 (find-file-noselect (org-air-r68--file "vocab.org"))))))

;;;; -------------------------------------------------------------------
;;;; r68-2 — the vocabulary offered is the file's OWN, and it is honoured
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r68-2-vocabulary-offered-and-honoured ()
  "The COLLECTION handed to `completing-read' is the item's own
file-local merged vocabulary: on the `wait.org' item it contains WAIT
and ARCHIVED (the file-local declaration won over the global — the
global default done keyword is absent), and choosing \"ARCHIVED\"
applies it; on the `vocab.org' item it carries the fast-key
vocabulary (READY, DROPPED, …).  RED today: no `completing-read' ever
runs on the `T' path."
  (skip-unless (locate-library "org-air"))
  (org-air-r68--with-board nil
    ;; wait.org: the disjoint file-local vocabulary
    (let ((colls nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt coll &rest _)
                   (push coll colls) "ARCHIVED")))
        (org-air-r68--goto-row "Jot")
        (org-air-item-cycle-todo))
      (should (= 1 (length colls)))
      (should (member "WAIT" (car colls)))
      (should (member "ARCHIVED" (car colls)))
      (should-not (member "DONE" (car colls)))
      ;; …and the choice is honoured (TODO is title text in this file,
      ;; so the keyword lands BEFORE it).
      (should (string-match-p "^\\* ARCHIVED TODO Jot"
                              (org-air-r68--text "wait.org"))))
    ;; vocab.org: the fast-key vocabulary is what completion offers
    (let ((colls nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt coll &rest _)
                   (push coll colls) "")))
        (org-air-r68--goto-row "Fix the widget")
        (org-air-item-cycle-todo))
      (should (member "READY" (car colls)))
      (should (member "DROPPED" (car colls))))))

;;;; -------------------------------------------------------------------
;;;; r68-3 — the @ keyword neither hangs nor traps; the record is SAVED
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r68-3-at-keyword-no-trap-record-saved ()
  "Choosing \"DROPPED\" (the `x@' keyword): the command RETURNS,
`post-command-hook' no longer carries `org-add-log-note', the SAVED
FILE bytes contain the downgraded `- State \"DROPPED\"' line, the
visiting buffer is unmodified, and no `*Org Note*' buffer exists.
RED today: the record pends on the hook against the undisplayed
buffer and the line is absent from disk."
  (skip-unless (locate-library "org-air"))
  (org-air-r68--with-board nil
    (org-air-r68--goto-row "Fix the widget")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "DROPPED")))
      (org-air-item-cycle-todo))
    (should-not (org-air-r68--log-pending-p))
    (should-not (get-buffer "*Org Note*"))
    (let ((new (org-air-r68--text "vocab.org")))
      (should (string-match-p "^\\* DROPPED Fix the widget" new))
      (should (string-match-p
               "^- State \"DROPPED\" +from +\"TODO\" +\\[" new)))
    (should-not (buffer-modified-p
                 (find-file-noselect (org-air-r68--file "vocab.org"))))))

;;;; -------------------------------------------------------------------
;;;; r68-4 — the ! keyword's time record lands in the SAME save
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r68-4-timestamp-keyword-records-in-same-save ()
  "Choosing \"COMP\" (the `c!' keyword): the `- State \"COMP\"' line is
in the saved bytes, the hook is clear, and the visiting buffer is
unmodified — the record was flushed BEFORE the macro's save, not
deferred past it.  RED today (record deferred to `post-command-hook',
landing after the save at best)."
  (skip-unless (locate-library "org-air"))
  (org-air-r68--with-board nil
    (org-air-r68--goto-row "Fix the widget")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "COMP")))
      (org-air-item-cycle-todo))
    (should-not (org-air-r68--log-pending-p))
    (let ((new (org-air-r68--text "vocab.org")))
      (should (string-match-p "^\\* COMP Fix the widget" new))
      (should (string-match-p
               "^- State \"COMP\" +from +\"TODO\" +\\[" new)))
    (should-not (buffer-modified-p
                 (find-file-noselect (org-air-r68--file "vocab.org"))))))

;;;; -------------------------------------------------------------------
;;;; r68-5 — the empty choice (and a re-pick) is a graceful no-op
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r68-5-empty-choice-graceful-noop ()
  "An empty completion choice writes NOTHING: the source bytes are
IDENTICAL, the message matches \"unchanged\", no error.  Companion:
re-picking the CURRENT keyword is the same gentle no-op.  RED today
(the bare nil-arg call cycles or traps instead)."
  (skip-unless (locate-library "org-air"))
  (org-air-r68--with-board nil
    (let ((old (org-air-r68--text "vocab.org"))
          (msgs nil))
      ;; empty choice
      (org-air-r68--goto-row "Fix the widget")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) ""))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when fmt (push (apply #'format fmt args) msgs))
                   nil)))
        (org-air-item-cycle-todo))
      (should (seq-some (lambda (m) (string-match-p "unchanged" m)) msgs))
      (should (equal (org-air-r68--text "vocab.org") old))
      ;; re-picking the current keyword
      (setq msgs nil)
      (org-air-r68--goto-row "Fix the widget")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "TODO"))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when fmt (push (apply #'format fmt args) msgs))
                   nil)))
        (org-air-item-cycle-todo))
      (should (seq-some (lambda (m) (string-match-p "unchanged" m)) msgs))
      (should (equal (org-air-r68--text "vocab.org") old)))))

;;;; -------------------------------------------------------------------
;;;; r68-6 — -done under an @-first done vocabulary (and the c! companion)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r68-6-done-under-at-first-done-vocab ()
  "`org-air-item-done' on the `#+TODO: TODO | DONE(d@)' item: DONE is
applied, the hook is clear, and the (downgraded) state line is in the
saved bytes — the explicit-symbol `(org-todo \\='done)' never entered
fast-selection, and the `@' note no longer traps.  Companion: under
the user's vocabulary `-done' lands COMP with its `c!' time record
saved.  RED today (probed: a pending \\='note record against the
undisplayed buffer)."
  (skip-unless (locate-library "org-air"))
  (org-air-r68--with-board nil
    ;; the @-first done keyword
    (org-air-r68--goto-row "At-done thing")
    (org-air-item-done)
    (should-not (org-air-r68--log-pending-p))
    (should-not (get-buffer "*Org Note*"))
    (let ((new (org-air-r68--text "atdone.org")))
      (should (string-match-p "^\\* DONE At-done thing" new))
      (should (string-match-p
               "^- State \"DONE\" +from +\"TODO\" +\\[" new)))
    ;; companion: the user's vocab lands COMP + its time record
    (org-air-r68--goto-row "Fix the widget")
    (org-air-item-done)
    (should-not (org-air-r68--log-pending-p))
    (let ((new (org-air-r68--text "vocab.org")))
      (should (string-match-p "^\\* COMP Fix the widget" new))
      (should (string-match-p "^- State \"COMP\" +from +\"TODO\" +\\[" new)))))

;;;; -------------------------------------------------------------------
;;;; r68-7 — a lognotereschedule config: downgraded, flushed, saved
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r68-7-reschedule-note-config-downgraded ()
  "With `org-log-reschedule' let-bound to \\='note (the
`lognotereschedule' user), `org-air-item-schedule' on an item WITH an
existing SCHEDULED lands the new stamp, leaves the hook clear, and
the downgraded TIMESTAMPED reschedule record is in the saved bytes —
`org--deadline-or-schedule' ignores `org-inhibit-logging', so the
macro's own knob downgrade is what saves this.  RED today (a pending
note against the undisplayed buffer)."
  (skip-unless (locate-library "org-air"))
  (org-air-r68--with-board nil
    (let ((org-log-reschedule 'note))
      (org-air-r68--goto-row "Dated thing")
      (org-air-item-schedule "2026-08-01"))
    (should-not (org-air-r68--log-pending-p))
    (should-not (get-buffer "*Org Note*"))
    (let ((new (org-air-r68--text "sched.org")))
      (should (string-match-p "SCHEDULED: <2026-08-01" new))
      (should (string-match-p
               "^- Rescheduled from \"\\[2026-07-25 Sat\\]\" on \\[" new)))))

;;;; -------------------------------------------------------------------
;;;; r68-8 — the R67 in-place leg shares the fix
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r68-8-apply-item-edits-shares-the-fix ()
  "`org-air-inbox--apply-item-edits' with `:todo \"DROPPED\"' on the
`vocab.org' item: applied, hook clear, and the state line is inside
the SAME save (asserted on DISK bytes) — the R67 transient's in-place
leg no longer pends a note against an undisplayed buffer.  RED today
(pending hook + line missing from disk)."
  (skip-unless (locate-library "org-air"))
  (org-air-r68--with-corpus nil
    (let ((item (org-air-r68--item "vocab.org" "Fix the widget")))
      (should (equal (org-air-inbox--apply-item-edits item '(:todo "DROPPED"))
                     '(todo)))
      (should-not (org-air-r68--log-pending-p))
      (should-not (get-buffer "*Org Note*"))
      (let ((new (org-air-r68--text "vocab.org")))
        (should (string-match-p "^\\* DROPPED Fix the widget" new))
        (should (string-match-p
                 "^- State \"DROPPED\" +from +\"TODO\" +\\[" new)))
      (should-not (buffer-modified-p
                   (find-file-noselect (org-air-r68--file "vocab.org")))))))

;;;; -------------------------------------------------------------------
;;;; r68-9 — the refile engine leg shares the fix
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r68-9-refile-engine-shares-the-fix ()
  "`org-air-refile-item' with todo \"DROPPED\" into a target file
declaring the same vocabulary: the item MOVES, the hook is clear, and
the state line is in the TARGET's saved bytes — the engine's metadata
mutators run in a target buffer the user never sees, the same
exposure.  RED today."
  (skip-unless (locate-library "org-air"))
  (org-air-r68--with-corpus nil
    (let ((item (org-air-r68--item "vocab.org" "Fix the widget")))
      (org-air-refile-item item (org-air-r68--file "target.org")
                           nil :none nil nil "DROPPED" nil nil)
      (should-not (org-air-r68--log-pending-p))
      (should-not (get-buffer "*Org Note*"))
      (should-not (string-match-p "Fix the widget"
                                  (org-air-r68--text "vocab.org")))
      (let ((target (org-air-r68--text "target.org")))
        (should (string-match-p "^\\* DROPPED Fix the widget" target))
        (should (string-match-p
                 "^- State \"DROPPED\" +from +\"TODO\" +\\[" target))))))

;;;; -------------------------------------------------------------------
;;;; r68-10 — ONE reader, not two
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r68-10-one-shared-reader ()
  "Both the board `T' body and `org-air-refile-form-todo' route
through the ONE extracted reader `org-air-inbox--read-todo-keyword':
with the reader stubbed (and `completing-read' stubbed to SIGNAL —
proof against a forked second reader), the form's `:todo' becomes the
sentinel and `T' applies the stub's choice.  RED against a forked
second reader."
  (skip-unless (locate-library "org-air"))
  (org-air-r68--with-board nil
    ;; the form suffix routes through the reader
    (let ((item (org-air-r68--goto-row "Fix the widget")))
      (cl-letf (((symbol-function 'org-air-inbox--read-todo-keyword)
                 (lambda (&rest _) "R68-SENTINEL"))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) (error "forked second reader"))))
        (org-air-inbox--form-init item)
        (call-interactively 'org-air-refile-form-todo)
        (should (equal (org-air-inbox--form-get :todo) "R68-SENTINEL"))))
    ;; …and so does the board T body
    (org-air-r68--goto-row "Fix the widget")
    (cl-letf (((symbol-function 'org-air-inbox--read-todo-keyword)
               (lambda (&rest _) "READY"))
              ((symbol-function 'completing-read)
               (lambda (&rest _) (error "forked second reader"))))
      (org-air-item-cycle-todo))
    (should (string-match-p "^\\* READY Fix the widget"
                            (org-air-r68--text "vocab.org")))))

(provide 'org-air-round68-test)
;;; org-air-round68-test.el ends here
