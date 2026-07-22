;;; org-air-round67-test.el --- executing ERTs for round-67 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for round-67 (air/v0.1/org-air-round67-design.org):
;; the refile transient becomes a general item EDITOR — the destination
;; is OPTIONAL (`org-air-refile-form-execute' dispatches: `:file' set →
;; today's R64/R66 engine byte-for-byte; `:file' nil + ≥1 changed field
;; → `org-air-inbox--apply-item-edits' IN PLACE, one
;; `atomic-change-group' + one `save-buffer'; nothing changed → a
;; gentle no-op), `:tags-dirty'/`:tags-stripped' scope the inbox-tag
;; strip to actual refiles (an in-place tag edit never graduates an
;; inbox item), a `d' DEADLINE field mirrors `s' minus `someday' with a
;; trailing DEADLINE engine parameter, and the `k'/`,' vocabulary
;; follows the WRITE TARGET (`(or :file <item's own file>)' — R57).
;; All BATCH/headless through the r19/r64 idiom: `--form-init' on a
;; fixture item + `--form-put' + `(call-interactively
;; 'org-air-refile-form-execute)' — no transient event loop.  The
;; spec's nine seams T1-T9 map onto the nine ERTs:
;;
;;   r67-1 THE REPRO (T1) — `:priority' ?A, `:file' nil ⇒ the SOURCE
;;         heading gains `[#A]' in place, no move, the file otherwise
;;         byte-stable, `--refile-last' nil.  RED today: `user-error'
;;         "No destination yet — pick a file with f".
;;   r67-2 EVERY FIELD IN PLACE (T2) — separate legs: scheduled,
;;         deadline, todo, dirty tags (non-inbox item), category —
;;         each lands at the source heading, no move.  RED today
;;         (same user-error); also pins the `d' key + option list.
;;   r67-3 THE NO-OP (T3) — an untouched form executes with NO error,
;;         source bytes IDENTICAL, message "Nothing to change…",
;;         `--refile-last' nil, form state cleared.  RED today.
;;   r67-4 REFILE REGRESSION PIN (T4) — `:file' set + todo + dirty
;;         tags ⇒ the item MOVES with the metadata and `--refile-last'
;;         records the destination — the engine leg is untouched by
;;         the dispatch.  RED against a hijacked destination case.
;;   r67-5 DEADLINE THROUGH THE ENGINE (T5) — the trailing ninth
;;         parameter: a spec string stamps DEADLINE on the moved
;;         heading; "" CLEARS a source deadline through a refile.
;;         RED today: wrong-number-of-arguments.
;;   r67-6 THE INBOX TAG STAYS PUT (T6) — an in-place dirty tag edit
;;         re-attaches the stripped `inbox' at the END (the item
;;         REMAINS a dweller per `--inbox-dweller-p'); the SAME edit
;;         WITH a destination drops it (refile semantics kept).  RED
;;         against applying the collected list verbatim in place.
;;   r67-7 NON-INBOX + VOCABULARY SOURCE (T7) — (a) nothing stripped
;;         on a non-inbox item and a priority-only edit leaves the
;;         tags bytes alone; (b) with `:file' nil the `k' collection
;;         carries the item's OWN file-local `#+TODO:' vocabulary
;;         (WAIT) and the in-place apply of "WAIT" succeeds.  RED
;;         against the global-vocab fallback.
;;   r67-8 IN-PLACE ATOMICITY (T8) — a mid-write signal (stubbed
;;         `org-set-property') propagates and the source file's DISK
;;         bytes stay byte-identical (rollback + no save).  RED
;;         against mutate-and-save-per-field.
;;   r67-9 TAGS-DIRTY PROPAGATION (T9) — the `t' suffix, the `c'
;;         suffix's extras-merge leg and the `s' suffix's `someday'
;;         leg all raise `:tags-dirty'; a someday-only in-place edit
;;         lands the `someday' tag AND a cleared schedule with `inbox'
;;         still present.  RED against a `t'-suffix-only dirty flag.
;;
;; The audit seat (round-67 test audit) adds four hardening ERTs for
;; the spec's uncovered seams:
;;
;;   r67-10 MULTI-FIELD IN ONE EXECUTE — all six metadata fields in a
;;          single destination-less confirm: every field lands at the
;;          source heading in one write, the completion message
;;          enumerates the applied fields in the engine's order, the
;;          item does not move, `inbox' re-attached, `--refile-last'
;;          still nil.  RED today (the destination guard).
;;   r67-11 CLEAR+SET MIX IN PLACE — one execute clears SCHEDULED
;;          ("") while setting a new DEADLINE; and a deadline-only
;;          "" clear removes DEADLINE while the untouched SCHEDULED
;;          survives byte-for-byte.  The in-place ""-clear leg was
;;          otherwise only reachable through T9's `someday'.  RED
;;          today (the destination guard).
;;   r67-12 READ-ONLY SOURCE FILE — a cache-hydrated (FILE . POS)
;;          item whose file is chmod 444 BEFORE any visit: the
;;          in-place execute signals `buffer-read-only' from inside
;;          the atomic group, the disk bytes stay identical, and the
;;          freshly-visited buffer is unmodified with identical bytes
;;          (no dirty residue).  RED today (the destination guard).
;;   r67-13 NEW-FILE REFILE + DEADLINE (R66 × R67) — the FORM's
;;          refile leg into a brand-new file under an Air tree with a
;;          deadline set: frontmatter synthesis still fires
;;          (`#+title:' + `#+state: draft') AND the trailing DEADLINE
;;          engine argument stamps the moved heading; `--refile-last'
;;          records the destination.  RED today: the pre-R67 execute
;;          never reads `:deadline', so the stamp is missing.
;;
;; GUI residue (screenshot-confirm, not ERT-able as pixels): the
;; reframed transient RENDERING.  The string-level halves ARE pinned
;; here — the `Edit "<title>"' heading (R70-1 reframe), the `(in place — f to
;; refile)' placeholder and the dynamic RET label (via the suffix
;; object's description slot).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'transient)
(require 'org-air-test-helpers)

(when (locate-library "org-air")
  (require 'org-air))

;;;; -------------------------------------------------------------------
;;;; Fixture: inbox / notes / wait-vocab / projects (the spec corpus)
;;;; -------------------------------------------------------------------

(defvar org-air-r67--dir nil
  "The temp corpus directory of the current `org-air-r67--with-corpus'.")

(defconst org-air-r67--default-specs
  '(("inbox.org" . "#+title: inbox\n\n* TODO Capture me :inbox:x:\n  body\n")
    ("notes.org" . "#+title: notes\n\n* Jot :keep:\n  body\n")
    ("wait.org" . "#+TODO: WAIT | ARCHIVED\n\n* Blocked thing\n  body\n")
    ("projects.org" . "#+title: projects\n\n* Existing\n"))
  "The spec fixture: an inbox capture, a non-inbox note, a file-local
`#+TODO: WAIT | ARCHIVED' variant, and a titled refile target.")

(defmacro org-air-r67--with-corpus (specs &rest body)
  "Create a temp Org corpus from SPECS and run BODY against it.
SPECS is a list of (NAME . CONTENT) files (nil = the default spec
corpus).  Binds `org-air-files' to the directory, `org-air-inbox-file'
to its inbox.org, a temp `org-air-cache-file', a DEAD board buffer
name, fresh form/last state and `org-tags-column' 0 (tags one space
after the title — the fixture shape, so the byte-stability asserts see
ONLY the intended change, never an alignment shift).  Kills every
corpus-visiting buffer and deletes the directory afterwards."
  (declare (indent 1) (debug t))
  `(let* ((org-air-r67--dir (make-temp-file "org-air-r67-" t)))
     (unwind-protect
         (progn
           (pcase-dolist (`(,name . ,content)
                          (or ,specs org-air-r67--default-specs))
             (let ((file-name-handler-alist nil)
                   (coding-system-for-write 'utf-8-unix))
               (write-region (or content "") nil
                             (expand-file-name name org-air-r67--dir)
                             nil 'silent)))
           (let ((org-air-files (list org-air-r67--dir))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-r67--dir))
                 (org-air-cache-file
                  (expand-file-name ".cache/board.eld" org-air-r67--dir))
                 (org-air-view-buffer-name "*org-air-r67-no-board*")
                 (org-air-inbox--refile-form nil)
                 (org-air-inbox--refile-last nil)
                 (org-tags-column 0)
                 (create-lockfiles nil)
                 (inhibit-message t))
             ,@body))
       (when (fboundp 'org-air-query-teardown)
         (org-air-query-teardown))
       (let ((kill-buffer-query-functions nil))
         (dolist (buf (buffer-list))
           (let ((fn (buffer-file-name buf)))
             (when (and fn (string-prefix-p org-air-r67--dir fn))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r67--dir t))))

(defun org-air-r67--file (name)
  "Return the corpus file NAME's absolute path."
  (expand-file-name name org-air-r67--dir))

(defun org-air-r67--item (name text)
  "Build an editor item for the heading containing TEXT in corpus file NAME.
Tags/todo read at the heading in the file's own buffer (its `#+TODO:'
vocabulary applies), the marker freshly positioned."
  (let ((file (org-air-r67--file name)))
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

(defun org-air-r67--text (name)
  "Return corpus file NAME's on-disk content as a string."
  (with-temp-buffer
    (insert-file-contents (org-air-r67--file name))
    (buffer-string)))

(defun org-air-r67--count (needle text)
  "Count the non-overlapping literal occurrences of NEEDLE in TEXT."
  (let ((n 0) (start 0) (re (regexp-quote needle)))
    (while (string-match re text start)
      (setq start (match-end 0))
      (cl-incf n))
    n))

(defun org-air-r67--execute ()
  "Drive the execute suffix the r19/r64 batch way."
  (call-interactively 'org-air-refile-form-execute))

(defun org-air-r67--heading-tags (name text)
  "Return the on-disk tag list of the heading containing TEXT in NAME."
  (with-current-buffer (find-file-noselect (org-air-r67--file name))
    (revert-buffer nil t)
    (org-with-wide-buffer
     (goto-char (point-min))
     (re-search-forward (regexp-quote text))
     (org-back-to-heading t)
     (org-get-tags nil t))))

;;;; -------------------------------------------------------------------
;;;; r67-1 (T1) — the repro: a priority-only form applies IN PLACE
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r67-1-in-place-priority-repro ()
  "The user's exact form — open the editor, set a priority, RET — with
NO destination applies `[#A]' IN PLACE at the source heading: the item
does not move (still exactly one match, in inbox.org), the file is
otherwise byte-stable (new bytes = old bytes with only the cookie
inserted on the heading line), and `org-air-inbox--refile-last' stays
nil (no destination was used — `l' keeps recalling the last actual
refile).  Also pins the R67-4 string reframes (heading re-reframed by
R70-1): the `Edit \"<title>\"' heading and the `(in place — f to
refile)' placeholder.
RED today: `user-error' \"No destination yet — pick a file with f\"."
  (skip-unless (locate-library "org-air"))
  (org-air-r67--with-corpus nil
    (let ((old (org-air-r67--text "inbox.org"))
          (item (org-air-r67--item "inbox.org" "Capture me")))
      (org-air-inbox--form-init item)
      ;; the R67-4 reframed strings (string-level, ERT-able half).
      (should (equal (org-air-inbox--form-heading)
                     "Edit \"Capture me\""))
      (should (string-match-p (regexp-quote "(in place — f to refile)")
                              (org-air-inbox--form-preview)))
      (org-air-inbox--form-put :priority ?A)
      (should (null (org-air-inbox--form-get :file)))
      (org-air-r67--execute)
      (let ((new (org-air-r67--text "inbox.org")))
        ;; the cookie landed on the heading line, IN PLACE…
        (should (string-match-p "^\\* TODO \\[#A\\] Capture me :inbox:x:$"
                                new))
        ;; …and it is the ONLY change: byte-stability modulo the cookie.
        (should (equal new
                       (replace-regexp-in-string
                        (regexp-quote "* TODO Capture me")
                        "* TODO [#A] Capture me" old t t)))
        ;; no move: exactly one match, still in inbox.org, nowhere else.
        (should (= 1 (org-air-r67--count "Capture me" new)))
        (should-not (string-match-p "Capture me"
                                    (org-air-r67--text "projects.org"))))
      (should (null org-air-inbox--refile-last)))))

;;;; -------------------------------------------------------------------
;;;; r67-2 (T2) — every field lands in place, each on its own leg
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r67-2-every-field-in-place ()
  "Each metadata field applies IN PLACE on a destination-less execute:
`:scheduled' ⇒ a SCHEDULED stamp, `:deadline' ⇒ a DEADLINE stamp,
`:todo' ⇒ the keyword, dirty `:tags' on the NON-inbox item ⇒ the
replacement list, `:category' ⇒ the `:CATEGORY:' drawer property.
Each leg runs on a fresh corpus, each item stays in its source file.
Also pins the R67-3 shape: the `d' key is bound to
`org-air-refile-form-deadline' in the Metadata group and the option
list is the `s' list minus `someday'.  RED today (the destination
guard user-errors every leg)."
  (skip-unless (locate-library "org-air"))
  ;; the `d' binding + option-list pins (no corpus needed).
  (let ((suffix (transient-get-suffix 'org-air-refile-transient '("d"))))
    (should suffix)
    (should (eq (plist-get (nth 2 suffix) :command)
                'org-air-refile-form-deadline)))
  (should (equal org-air-inbox--deadline-options
                 '(("today" . ".") ("tomorrow" . "+1d")
                   ("this week" . "+1w")
                   ("other date…" . other) ("clear" . ""))))
  (should-not (assoc "someday" org-air-inbox--deadline-options))
  ;; scheduled
  (org-air-r67--with-corpus nil
    (org-air-inbox--form-init (org-air-r67--item "inbox.org" "Capture me"))
    (org-air-inbox--form-put :scheduled "2026-08-01")
    (org-air-r67--execute)
    (let ((new (org-air-r67--text "inbox.org")))
      (should (string-match-p "^\\* TODO Capture me :inbox:x:\nSCHEDULED: <2026-08-01" new))
      (should (= 1 (org-air-r67--count "Capture me" new)))))
  ;; deadline (the new `d' field, in-place leg)
  (org-air-r67--with-corpus nil
    (org-air-inbox--form-init (org-air-r67--item "inbox.org" "Capture me"))
    (org-air-inbox--form-put :deadline "2026-08-01")
    (org-air-r67--execute)
    (let ((new (org-air-r67--text "inbox.org")))
      (should (string-match-p "^\\* TODO Capture me :inbox:x:\nDEADLINE: <2026-08-01" new))
      (should (= 1 (org-air-r67--count "Capture me" new)))))
  ;; todo
  (org-air-r67--with-corpus nil
    (org-air-inbox--form-init (org-air-r67--item "inbox.org" "Capture me"))
    (org-air-inbox--form-put :todo "DONE")
    (org-air-r67--execute)
    (let ((new (org-air-r67--text "inbox.org")))
      (should (string-match-p "^\\* DONE Capture me" new))
      (should (= 1 (org-air-r67--count "Capture me" new)))))
  ;; dirty tags, non-inbox item (nothing stripped, nothing re-attached)
  (org-air-r67--with-corpus nil
    (org-air-inbox--form-init (org-air-r67--item "notes.org" "Jot"))
    (org-air-inbox--form-put :tags '("alpha"))
    (org-air-inbox--form-put :tags-dirty t)
    (org-air-r67--execute)
    (let ((new (org-air-r67--text "notes.org")))
      (should (string-match-p "^\\* Jot :alpha:$" new))
      (should-not (string-match-p ":keep:" new))
      (should-not (string-match-p ":inbox:" new))))
  ;; category
  (org-air-r67--with-corpus nil
    (org-air-inbox--form-init (org-air-r67--item "inbox.org" "Capture me"))
    (org-air-inbox--form-put :category "work")
    (org-air-r67--execute)
    (let ((new (org-air-r67--text "inbox.org")))
      (should (string-match-p ":CATEGORY: +work" new))
      (should (= 1 (org-air-r67--count "Capture me" new))))))

;;;; -------------------------------------------------------------------
;;;; r67-3 (T3) — the untouched form is a gentle no-op
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r67-3-untouched-form-is-gentle-noop ()
  "Execute on a completely untouched, destination-less form: NO error,
the source bytes IDENTICAL, the message matches \"Nothing to change\",
`--refile-last' stays nil and the form state is cleared.  RED today
\(the destination guard user-errors)."
  (skip-unless (locate-library "org-air"))
  (org-air-r67--with-corpus nil
    (let ((old (org-air-r67--text "inbox.org"))
          (captured nil))
      (org-air-inbox--form-init (org-air-r67--item "inbox.org" "Capture me"))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when fmt (push (apply #'format fmt args) captured))
                   nil)))
        (org-air-r67--execute))
      (should (seq-some (lambda (m)
                          (string-match-p "Nothing to change" m))
                        captured))
      (should (equal (org-air-r67--text "inbox.org") old))
      (should (null org-air-inbox--refile-last))
      (should (null org-air-inbox--refile-form)))))

;;;; -------------------------------------------------------------------
;;;; r67-4 (T4) — the destination leg is byte-for-byte today's engine
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r67-4-refile-leg-regression-pin ()
  "With `:file' set the dispatch takes TODAY'S refile path: the item
MOVES (gone from the source, present in the target with the collected
todo + tags applied verbatim — the `inbox' strip IS the graduation)
and `--refile-last' records the destination for the `l' recall.  The
dynamic RET label reads \"refile\" here and \"edit in place\" without
a destination.  RED against an in-place leg that hijacks the
destination case."
  (skip-unless (locate-library "org-air"))
  (org-air-r67--with-corpus nil
    (let ((projects (org-air-r67--file "projects.org")))
      (org-air-inbox--form-init (org-air-r67--item "inbox.org" "Capture me"))
      ;; the dynamic RET label, both ways (the suffix object's slot).
      (let ((desc (oref (get 'org-air-refile-form-execute 'transient--suffix)
                        description)))
        (should (functionp desc))
        (should (equal (funcall desc) "edit in place"))
        (org-air-inbox--form-put :file projects)
        (should (equal (funcall desc) "refile")))
      (org-air-inbox--form-put :todo "DONE")
      (org-air-inbox--form-put :tags '("x" "moved"))
      (org-air-inbox--form-put :tags-dirty t)
      (org-air-r67--execute)
      (should-not (string-match-p "Capture me" (org-air-r67--text "inbox.org")))
      (let ((target (org-air-r67--text "projects.org")))
        (should (string-match-p "^\\* DONE Capture me :x:moved:$" target))
        (should-not (string-match-p ":inbox:" target)))
      (should (equal org-air-inbox--refile-last (cons projects nil)))
      (should (null org-air-inbox--refile-form)))))

;;;; -------------------------------------------------------------------
;;;; r67-5 (T5) — DEADLINE through the engine (the trailing parameter)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r67-5-deadline-through-the-engine ()
  "The engine's trailing optional DEADLINE: a direct ninth-argument
call stamps DEADLINE on the MOVED heading, and \"\" CLEARS a source
deadline through a refile.  RED today: the engine has no ninth
parameter (wrong-number-of-arguments)."
  (skip-unless (locate-library "org-air"))
  ;; stamp
  (org-air-r67--with-corpus nil
    (let ((projects (org-air-r67--file "projects.org")))
      (org-air-refile-item (org-air-r67--item "inbox.org" "Capture me")
                           projects nil :none nil nil nil nil "2026-08-01")
      (let ((target (org-air-r67--text "projects.org")))
        (should (string-match-p "^\\* TODO Capture me\nDEADLINE: <2026-08-01"
                                target)))
      (should-not (string-match-p "Capture me"
                                  (org-air-r67--text "inbox.org")))))
  ;; clear
  (org-air-r67--with-corpus
      '(("inbox.org" . "#+title: inbox\n\n* TODO Dated :inbox:\nDEADLINE: <2026-08-05 Wed>\n  body\n")
        ("projects.org" . "#+title: projects\n\n* Existing\n"))
    (org-air-refile-item (org-air-r67--item "inbox.org" "Dated")
                         (org-air-r67--file "projects.org")
                         nil :none nil nil nil nil "")
    (let ((target (org-air-r67--text "projects.org")))
      (should (string-match-p "Dated" target))
      (should-not (string-match-p "DEADLINE:" target)))))

;;;; -------------------------------------------------------------------
;;;; r67-6 (T6) — the inbox tag stays put in place, leaves on refile
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r67-6-inbox-tag-stays-put-in-place ()
  "The form seeds `:tags' MINUS `inbox' and records the strip; an
in-place dirty edit to (\"x\" \"y\") re-attaches `inbox' at the END —
the heading's tags are exactly {x y inbox}, the item REMAINS an inbox
dweller per `org-air-classify--inbox-dweller-p'.  Companion pin: the
SAME edit WITH a destination drops `inbox' (today's refile semantics
kept).  RED against applying the collected list verbatim in place."
  (skip-unless (locate-library "org-air"))
  ;; in place: re-attach
  (org-air-r67--with-corpus nil
    (org-air-inbox--form-init (org-air-r67--item "inbox.org" "Capture me"))
    (should (equal (org-air-inbox--form-get :tags) '("x")))
    (should (equal (org-air-inbox--form-get :tags-stripped) '("inbox")))
    (org-air-inbox--form-put :tags '("x" "y"))
    (org-air-inbox--form-put :tags-dirty t)
    (org-air-r67--execute)
    ;; exactly {x y inbox}, `inbox' re-attached at the END.
    (should (string-match-p "^\\* TODO Capture me :x:y:inbox:$"
                            (org-air-r67--text "inbox.org")))
    (should (equal (org-air-r67--heading-tags "inbox.org" "Capture me")
                   '("x" "y" "inbox")))
    ;; the item REMAINS an inbox dweller.
    (should (org-air-classify--inbox-dweller-p
             (org-air-r67--item "inbox.org" "Capture me"))))
  ;; refile: the strip is the graduation
  (org-air-r67--with-corpus nil
    (org-air-inbox--form-init (org-air-r67--item "inbox.org" "Capture me"))
    (org-air-inbox--form-put :file (org-air-r67--file "projects.org"))
    (org-air-inbox--form-put :tags '("x" "y"))
    (org-air-inbox--form-put :tags-dirty t)
    (org-air-r67--execute)
    (let ((target (org-air-r67--text "projects.org")))
      (should (string-match-p "^\\* TODO Capture me :x:y:$" target))
      (should-not (string-match-p ":inbox:" target)))))

;;;; -------------------------------------------------------------------
;;;; r67-7 (T7) — non-inbox neutrality + the write-target vocabulary
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r67-7-non-inbox-item-and-vocabulary-source ()
  "(a) Form-init on the `:keep:'-tagged notes item strips NOTHING
\(`:tags-stripped' nil) and a priority-only in-place edit leaves the
tags bytes identical (only the cookie changes).  (b) With `:file' nil
the `k' field completes over the item's OWN merged file vocabulary —
the collection contains WAIT for an item whose file declares `#+TODO:
WAIT | ARCHIVED' — and the in-place apply of \"WAIT\" succeeds
\(probed: completion and apply agree by construction, `org-todo'
user-errors on an undeclared keyword).  RED against the current
global-vocab fallback."
  (skip-unless (locate-library "org-air"))
  ;; (a) non-inbox: nothing stripped, tags bytes untouched
  (org-air-r67--with-corpus nil
    (let ((old (org-air-r67--text "notes.org")))
      (org-air-inbox--form-init (org-air-r67--item "notes.org" "Jot"))
      (should (equal (org-air-inbox--form-get :tags) '("keep")))
      (should (null (org-air-inbox--form-get :tags-stripped)))
      (org-air-inbox--form-put :priority ?B)
      (org-air-r67--execute)
      (let ((new (org-air-r67--text "notes.org")))
        (should (string-match-p "^\\* \\[#B\\] Jot :keep:$" new))
        ;; ONLY the cookie moved — the tags bytes identical.
        (should (equal new
                       (replace-regexp-in-string
                        (regexp-quote "* Jot") "* [#B] Jot" old t t))))))
  ;; (b) the `k' vocabulary follows the WRITE TARGET (= the item's file)
  (org-air-r67--with-corpus nil
    (let ((captured-coll nil))
      (org-air-inbox--form-init (org-air-r67--item "wait.org" "Blocked thing"))
      (should (null (org-air-inbox--form-get :file)))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt coll &rest _)
                   (setq captured-coll coll)
                   "WAIT")))
        (call-interactively 'org-air-refile-form-todo))
      (should (member "WAIT" captured-coll))
      (should (equal (org-air-inbox--form-get :todo) "WAIT"))
      ;; …and the in-place apply of that keyword SUCCEEDS.
      (org-air-r67--execute)
      (should (string-match-p "^\\* WAIT Blocked thing$"
                              (org-air-r67--text "wait.org"))))))

;;;; -------------------------------------------------------------------
;;;; r67-8 (T8) — in-place atomicity: rollback + no save
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r67-8-in-place-atomicity ()
  "A signal mid-write (stubbed `org-set-property') rolls back EVERY
in-buffer change (the priority applied before it) and never saves: the
error propagates and the source file's DISK bytes are byte-identical.
RED against mutate-and-save-per-field."
  (skip-unless (locate-library "org-air"))
  (org-air-r67--with-corpus nil
    (let ((old (org-air-r67--text "inbox.org")))
      (org-air-inbox--form-init (org-air-r67--item "inbox.org" "Capture me"))
      (org-air-inbox--form-put :priority ?B)
      (org-air-inbox--form-put :category "work")
      (cl-letf (((symbol-function 'org-set-property)
                 (lambda (&rest _) (error "boom (r67-8 fault)"))))
        (let ((err (should-error (org-air-r67--execute))))
          ;; the INJECTED fault propagates unchanged — never the retired
          ;; destination guard's user-error (RED against trunk too).
          (should (string-match-p "boom (r67-8 fault)"
                                  (error-message-string err)))))
      ;; disk byte-identical (rollback + the ONE save never ran)…
      (should (equal (org-air-r67--text "inbox.org") old))
      ;; …and the live buffer rolled back byte-exactly too (probed).
      (should (equal (with-current-buffer
                         (find-file-noselect (org-air-r67--file "inbox.org"))
                       (buffer-substring-no-properties (point-min) (point-max)))
                     old)))))

;;;; -------------------------------------------------------------------
;;;; r67-9 (T9) — :tags-dirty propagation from every tag-mutating path
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r67-9-tags-dirty-propagation ()
  "Every code path that mutates `:tags' raises `:tags-dirty': the `t'
suffix, the `c' suffix's extras-merge leg, and the `s' suffix's
`someday' leg — and a someday-only edit with `:file' nil lands the
`someday' tag AND a cleared schedule IN PLACE with `inbox' still
present (R20-4 semantics in the new leg).  RED against a
`t'-suffix-only dirty flag."
  (skip-unless (locate-library "org-air"))
  ;; the `t' suffix
  (org-air-r67--with-corpus nil
    (org-air-inbox--form-init (org-air-r67--item "inbox.org" "Capture me"))
    (should (null (org-air-inbox--form-get :tags-dirty)))
    (cl-letf (((symbol-function 'completing-read-multiple)
               (lambda (&rest _) '("x" "z"))))
      (call-interactively 'org-air-refile-form-tags))
    (should (equal (org-air-inbox--form-get :tags) '("x" "z")))
    (should (org-air-inbox--form-get :tags-dirty)))
  ;; the `c' suffix's extras-merge leg
  (org-air-r67--with-corpus nil
    (org-air-inbox--form-init (org-air-r67--item "inbox.org" "Capture me"))
    (should (null (org-air-inbox--form-get :tags-dirty)))
    (cl-letf (((symbol-function 'completing-read-multiple)
               (lambda (&rest _) '("work" "q3"))))
      (call-interactively 'org-air-refile-form-category))
    (should (equal (org-air-inbox--form-get :category) "work"))
    (should (member "q3" (org-air-inbox--form-get :tags)))
    (should (org-air-inbox--form-get :tags-dirty)))
  ;; the `s' suffix's `someday' leg + the in-place landing
  (org-air-r67--with-corpus
      '(("inbox.org" . "#+title: inbox\n\n* TODO Someday thing :inbox:\nSCHEDULED: <2026-07-25 Sat>\n  body\n")
        ("projects.org" . "#+title: projects\n\n* Existing\n"))
    (org-air-inbox--form-init (org-air-r67--item "inbox.org" "Someday thing"))
    (should (null (org-air-inbox--form-get :tags-dirty)))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "someday")))
      (call-interactively 'org-air-refile-form-schedule))
    (should (org-air-inbox--form-get :tags-dirty))
    (should (equal (org-air-inbox--form-get :scheduled) ""))
    (should (member "someday" (org-air-inbox--form-get :tags)))
    (org-air-r67--execute)
    (let ((new (org-air-r67--text "inbox.org")))
      ;; the tag + the cleared schedule, in place, `inbox' re-attached.
      (should (string-match-p "^\\* TODO Someday thing :someday:inbox:$" new))
      (should-not (string-match-p "SCHEDULED:" new))
      (should (= 1 (org-air-r67--count "Someday thing" new))))))

;;;; -------------------------------------------------------------------
;;;; r67-10 (audit) — every field at once, ONE in-place execute
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r67-10-multi-field-single-execute ()
  "All six metadata fields collected in ONE destination-less form and
applied by ONE execute: the source heading carries todo + priority +
the dirty tags (with `inbox' re-attached at the END), the planning
line carries BOTH stamps (org's canonical DEADLINE-then-SCHEDULED
order), the `:CATEGORY:' drawer lands — all IN PLACE (one match, no
move), the completion message enumerates the applied fields in the
engine's order (todo → priority → tags → category → scheduled →
deadline), and `--refile-last' stays nil.  RED today: the destination
guard user-errors."
  (skip-unless (locate-library "org-air"))
  (org-air-r67--with-corpus nil
    (let ((msgs nil))
      (org-air-inbox--form-init (org-air-r67--item "inbox.org" "Capture me"))
      (org-air-inbox--form-put :todo "DONE")
      (org-air-inbox--form-put :priority ?B)
      (org-air-inbox--form-put :tags '("x" "y"))
      (org-air-inbox--form-put :tags-dirty t)
      (org-air-inbox--form-put :category "work")
      (org-air-inbox--form-put :scheduled "2026-08-01")
      (org-air-inbox--form-put :deadline "2026-08-15")
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when fmt (push (apply #'format fmt args) msgs))
                   nil)))
        (org-air-r67--execute))
      (let ((new (org-air-r67--text "inbox.org")))
        (should (string-match-p "^\\* DONE \\[#B\\] Capture me :x:y:inbox:$"
                                new))
        (should (string-match-p
                 "^DEADLINE: <2026-08-15[^>]*> SCHEDULED: <2026-08-01[^>]*>$"
                 new))
        (should (string-match-p ":CATEGORY: +work" new))
        (should (= 1 (org-air-r67--count "Capture me" new)))
        (should-not (string-match-p "Capture me"
                                    (org-air-r67--text "projects.org"))))
      ;; the completion message enumerates in application order…
      (should (seq-some
               (lambda (m)
                 (string-match-p
                  "todo, priority, tags, category, scheduled, deadline" m))
               msgs))
      ;; …and no destination was used.
      (should (null org-air-inbox--refile-last))
      (should (null org-air-inbox--refile-form)))))

;;;; -------------------------------------------------------------------
;;;; r67-11 (audit) — clear+set mix in place; "" clears each stamp
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r67-11-clear-set-mix-in-place ()
  "The \"\"-clear semantics compose with a set in ONE in-place
execute: clearing SCHEDULED while setting a NEW deadline removes the
schedule stamp and replaces the old deadline; and a deadline-only
\"\" clear removes DEADLINE while the untouched SCHEDULED stamp
survives byte-for-byte (the in-place `'(4)' prefix leg for BOTH date
fields — otherwise reachable only through T9's `someday').  RED
today: the destination guard user-errors."
  (skip-unless (locate-library "org-air"))
  ;; clear SCHEDULED + set a new DEADLINE, one execute
  (org-air-r67--with-corpus
      '(("inbox.org" . "#+title: inbox\n\n* TODO Mixed :inbox:\nSCHEDULED: <2026-07-25 Sat> DEADLINE: <2026-08-05 Wed>\n  body\n")
        ("projects.org" . "#+title: projects\n\n* Existing\n"))
    (org-air-inbox--form-init (org-air-r67--item "inbox.org" "Mixed"))
    (org-air-inbox--form-put :scheduled "")
    (org-air-inbox--form-put :deadline "2026-09-01")
    (org-air-r67--execute)
    (let ((new (org-air-r67--text "inbox.org")))
      (should-not (string-match-p "SCHEDULED:" new))
      (should (string-match-p "^DEADLINE: <2026-09-01" new))
      (should-not (string-match-p "2026-08-05" new))
      (should (= 1 (org-air-r67--count "Mixed" new)))))
  ;; deadline-only "" clear: the untouched SCHEDULED survives
  (org-air-r67--with-corpus
      '(("inbox.org" . "#+title: inbox\n\n* TODO Mixed :inbox:\nSCHEDULED: <2026-07-25 Sat> DEADLINE: <2026-08-05 Wed>\n  body\n")
        ("projects.org" . "#+title: projects\n\n* Existing\n"))
    (org-air-inbox--form-init (org-air-r67--item "inbox.org" "Mixed"))
    (org-air-inbox--form-put :deadline "")
    (org-air-r67--execute)
    (let ((new (org-air-r67--text "inbox.org")))
      (should-not (string-match-p "DEADLINE:" new))
      (should (string-match-p "^SCHEDULED: <2026-07-25 Sat>$" new)))))

;;;; -------------------------------------------------------------------
;;;; r67-12 (audit) — a read-only source file: clean signal, zero residue
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r67-12-read-only-source-file ()
  "An in-place edit on an item whose file is READ-ONLY (chmod 444
before any visit; the item is R26-8 cache-hydrated — `(FILE . POS)',
no marker, no pre-existing buffer) fails CLEANLY: the first mutator
signals `buffer-read-only' inside the atomic group, the error
propagates, the disk bytes stay identical, and the freshly-visited
buffer is UNMODIFIED with identical bytes — no dirty residue for a
later save to flush.  RED today: the destination guard user-errors
instead."
  (skip-unless (locate-library "org-air"))
  (skip-unless (not (zerop (user-uid))))   ; root ignores file modes
  (org-air-r67--with-corpus nil
    (let* ((file (org-air-r67--file "inbox.org"))
           (old (org-air-r67--text "inbox.org"))
           (pos (1+ (string-match "\\* TODO" old)))
           (item (org-air-item-create
                  :title "Capture me" :tags '("inbox" "x")
                  :todo "TODO" :file file :marker (cons file pos))))
      (should (null (get-file-buffer file))) ; genuinely unvisited
      (set-file-modes file #o444)
      (unwind-protect
          (progn
            (org-air-inbox--form-init item)
            (org-air-inbox--form-put :priority ?A)
            (should-error (org-air-r67--execute) :type 'buffer-read-only)
            ;; disk untouched…
            (should (equal (org-air-r67--text "inbox.org") old))
            ;; …and the visited buffer carries NO residue.
            (let ((buf (get-file-buffer file)))
              (should buf)
              (should-not (buffer-modified-p buf))
              (should (equal (with-current-buffer buf
                               (buffer-substring-no-properties
                                (point-min) (point-max)))
                             old))))
        (set-file-modes file #o644)))))

;;;; -------------------------------------------------------------------
;;;; r67-13 (audit) — R66 × R67: new-file refile with a deadline set
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r67-13-new-file-refile-with-deadline ()
  "The FORM's refile leg into a BRAND-NEW file under an Air tree with
a deadline collected: R66 frontmatter synthesis still fires (derived
`#+title:' + `#+state: draft' at the target top) AND the R67-3
trailing DEADLINE engine argument stamps the moved heading — the two
rounds compose inside the ONE transactional save.  The source no
longer holds the item and `--refile-last' records the destination.
RED today: the pre-R67 execute never reads `:deadline', so the moved
heading lacks the stamp."
  (skip-unless (locate-library "org-air"))
  (org-air-r67--with-corpus
      '(("air-config.toml" . "")      ; the Air-tree marker (R66-2 gate)
        ("inbox.org" . "#+title: inbox\n\n* TODO Fresh idea :inbox:\n  body\n")
        ("projects.org" . "#+title: projects\n\n* Existing\n"))
    (let ((org-air-refile-synthesize-frontmatter t)
          (target (org-air-r67--file "notes/new-idea.org")))
      (org-air-inbox--form-init (org-air-r67--item "inbox.org" "Fresh idea"))
      (org-air-inbox--form-put :file target)
      (org-air-inbox--form-put :deadline "2026-08-20")
      (org-air-r67--execute)
      (let ((new (org-air-r67--text "notes/new-idea.org")))
        (should (string-match-p "\\`#\\+title: Fresh idea\n#\\+state: draft\n"
                                new))
        (should (string-match-p "^\\* TODO Fresh idea\nDEADLINE: <2026-08-20"
                                new)))
      (should-not (string-match-p "Fresh idea" (org-air-r67--text "inbox.org")))
      (should (equal org-air-inbox--refile-last (cons target nil)))
      (should (null org-air-inbox--refile-form)))))

(provide 'org-air-round67-test)
;;; org-air-round67-test.el ends here
