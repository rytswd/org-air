;;; org-air-round75-test.el --- executing ERTs for round-75 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for round-75 (air/v0.1/org-air-round75-design.org):
;; REDO for the recent-edits ring — `U' (`org-air-edit-redo', the
;; shift-pair inverse of `u') re-applies the edit `u' just reverted via
;; ONE buffer-level `undo-redo' step in the record's own buffer + save +
;; refresh + resync, then moves the record BACK onto the undo ring.
;; `u''s SUCCESS branch feeds the new global
;; `org-air-view--edit-redo-ring' (same record shape, tick re-stamped
;; post-undo); every FRESH edit CLEARS the redo side by construction
;; inside the one push choke point `org-air-view--edit-ring-push';
;; refile/archive stay not-redoable BY CONSTRUCTION (consumed without
;; reversal, never enter the redo ring); guards mirror R73 (liveness +
;; chars-tick, consumed + message, never an error) with the restamp
;; widened TWO-SIDED; bounded by conservation under the one existing
;; `org-air-view--edit-ring-max' defconst.
;;
;; All BATCH/headless: the board renders over a temp corpus (the r73
;; house idiom); spies via `cl-letf'; file bytes compared before/after
;; at every station.  The spec's eleven seams r75-1..r75-11 map onto
;; the ERTs below (revert-RED where the spec marks them).
;;
;; GUI residue (screenshot/user-confirm, not ERT-able): the visible
;; board/pane repaint right after `U' (the same R73-1 resync class) and
;; the process-inbox prompt readability with the new `[U]redo' token.

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
;;;; Fixture corpus + harness (the r73 house idiom)
;;;; -------------------------------------------------------------------

(defvar org-air-r75--dir nil
  "The temp corpus directory of the current `org-air-r75--with-corpus'.")

(defconst org-air-r75--default-specs
  '(("inbox.org" . "#+title: inbox\n\n* TODO Alpha task :inbox:\n  alpha body\n* TODO Beta task :inbox:\n  beta body\n")
    ("other.org" . "#+title: other\n\n* TODO Gamma chore\nSCHEDULED: <2026-01-05 Mon>\n  gamma body\n")
    ("target.org" . "#+title: target\n\n* Existing\n"))
  "Default corpus: two inbox items, one dated item in a second file,
and a refile target (the r73 shape).")

(defmacro org-air-r75--with-corpus (specs &rest body)
  "Create a temp Org corpus from SPECS and run BODY against it.
Binds the org-air roots, a temp cache, a round-local board buffer name,
FRESH `org-air-view--edit-ring' AND `org-air-view--edit-redo-ring'
\(test isolation — both sides of the pair are global), `org-tags-column'
0 and lockfile/message quiet.  Kills every corpus-visiting buffer and
deletes the directory afterwards."
  (declare (indent 1) (debug t))
  `(let* ((org-air-r75--dir (make-temp-file "org-air-r75-" t)))
     (unwind-protect
         (progn
           (pcase-dolist (`(,name . ,content)
                          (or ,specs org-air-r75--default-specs))
             (let ((file-name-handler-alist nil)
                   (coding-system-for-write 'utf-8-unix))
               (write-region (or content "") nil
                             (expand-file-name name org-air-r75--dir)
                             nil 'silent)))
           (let ((org-air-files (list org-air-r75--dir))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-r75--dir))
                 (org-air-cache-file
                  (expand-file-name ".cache/board.eld" org-air-r75--dir))
                 (org-air-view-buffer-name "*org-air-r75*")
                 (org-air-view--edit-ring nil)
                 (org-air-view--edit-redo-ring nil)
                 (org-air-view--triage-source-buffer nil)
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
             (when (and fn (string-prefix-p org-air-r75--dir fn))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r75--dir t))))

(defmacro org-air-r75--with-board (specs &rest body)
  "Render the real board over the SPECS corpus; run BODY in its buffer."
  (declare (indent 1) (debug t))
  `(org-air-r75--with-corpus ,specs
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

(defun org-air-r75--goto-row (title)
  "Move point onto the board row whose item title contains TITLE.
Returns the row's item; fails the test when no such row renders."
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

(defun org-air-r75--file (name)
  "Return the corpus file NAME's absolute path."
  (expand-file-name name org-air-r75--dir))

(defun org-air-r75--text (name)
  "Return corpus file NAME's on-disk content as a string."
  (with-temp-buffer
    (insert-file-contents (org-air-r75--file name))
    (buffer-string)))

(defun org-air-r75--item (name text)
  "Build an item for the heading containing TEXT in corpus file NAME."
  (let ((file (org-air-r75--file name)))
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

(defmacro org-air-r75--recording-messages (var &rest body)
  "Run BODY with `message' recorded (formatted strings) into VAR."
  (declare (indent 1) (debug t))
  `(let ((,var nil))
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args)
                  (when fmt (push (apply #'format fmt args) ,var))
                  nil)))
       ,@body)))

(defun org-air-r75--count (needle hay)
  "Return the number of NEEDLE occurrences in the string HAY."
  (let ((n 0) (start 0))
    (while (string-match (regexp-quote needle) hay start)
      (setq n (1+ n))
      (setq start (match-end 0)))
    n))

;;;; -------------------------------------------------------------------
;;;; r75-1 — the round trip: u then U is byte-exact (revert-RED)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r75-1-round-trip ()
  "Schedule an item (snapshot the post-edit bytes), `u' (bytes =
pre-edit), `U': buffer AND disk byte-equal the post-edit snapshot
\(including the saved planning line); the record is back on the undo
ring head; the echo names the desc; the R73-1 resync spies fire from
the redo's refresh tail with a stationary point.  Revert-RED: no `U'
binding, no command."
  (skip-unless (locate-library "org-air"))
  (org-air-r75--with-board nil
    (org-air-r75--goto-row "Alpha task")
    (let ((before (org-air-r75--text "inbox.org")))
      (org-air-item-schedule "2026-08-01")
      (let ((edited (org-air-r75--text "inbox.org")))
        (should-not (equal before edited))
        (org-air-edit-undo)
        (should (equal before (org-air-r75--text "inbox.org")))
        (should (null org-air-view--edit-ring))
        (should (= 1 (length org-air-view--edit-redo-ring)))
        (setq last-command 'ignore)
        (let ((pane-calls nil) (insp-calls nil) (msgs nil))
          (cl-letf* (((symbol-function 'org-air-view--view-pane-update-now)
                      (lambda (buf) (push buf pane-calls)))
                     ((symbol-function 'org-air-view--inspector-update-now)
                      (lambda (buf) (push buf insp-calls)))
                     ((symbol-function 'message)
                      (lambda (fmt &rest args)
                        (when fmt (push (apply #'format fmt args) msgs))
                        nil)))
            (org-air-edit-redo))
          ;; buffer AND disk byte-equal the post-edit snapshot.
          (should (equal edited (org-air-r75--text "inbox.org")))
          (let ((buf (find-file-noselect (org-air-r75--file "inbox.org"))))
            (should (equal edited (with-current-buffer buf (buffer-string))))
            (should-not (buffer-modified-p buf)))
          ;; the record moved back onto the undo ring head; redo empty.
          (should (= 1 (length org-air-view--edit-ring)))
          (should (null org-air-view--edit-redo-ring))
          (should (string-match-p
                   "\\`schedule \"Alpha task\" → 2026-08-01\\'"
                   (plist-get (car org-air-view--edit-ring) :desc)))
          ;; the echo names the desc + the redoable count.
          (should (seq-find
                   (lambda (m)
                     (string-match-p
                      "\\`Redid: schedule \"Alpha task\" → 2026-08-01 (0 more redoable)\\'"
                      m))
                   msgs))
          ;; the R73-1 resync rode the redo's refresh tail.
          (should pane-calls)
          (should insp-calls)
          (should (memq (current-buffer) pane-calls))
          ;; stationary point: still the SAME row.
          (let ((it (org-air-view--row-property 'org-air-item)))
            (should it)
            (should (equal "Alpha task" (org-air-item-title it)))))))))

;;;; -------------------------------------------------------------------
;;;; r75-2 — a fresh edit clears (all three push sites; the choke point)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r75-2-fresh-edit-clears ()
  "Edit A, `u', fresh edit B ⇒ the redo ring is nil; `U' ⇒ a gentle
`user-error' with zero bytes moved anywhere.  All three push sites
clear — the macro verb, a transient-leg `--apply-item-edits', and the
refile engine (the choke-point law: they all route through
`org-air-view--edit-ring-push')."
  (skip-unless (locate-library "org-air"))
  (org-air-r75--with-board nil
    ;; site 1: the macro verb.
    (org-air-r75--goto-row "Alpha task")
    (org-air-item-schedule "2026-08-01")
    (org-air-edit-undo)
    (should (= 1 (length org-air-view--edit-redo-ring)))
    (org-air-r75--goto-row "Beta task")
    (org-air-item-schedule "2026-08-03")          ; the fresh edit
    (should (null org-air-view--edit-redo-ring))
    (let ((inbox (org-air-r75--text "inbox.org"))
          (other (org-air-r75--text "other.org"))
          (err (should-error (org-air-edit-redo) :type 'user-error)))
      (should (string-match-p "Nothing to redo" (cadr err)))
      (should (equal inbox (org-air-r75--text "inbox.org")))
      (should (equal other (org-air-r75--text "other.org"))))
    ;; site 2: a transient-leg --apply-item-edits.
    (setq last-command 'ignore)
    (org-air-edit-undo)                           ; repopulate the redo side
    (should (= 1 (length org-air-view--edit-redo-ring)))
    (let ((item (org-air-r75--item "inbox.org" "Alpha task")))
      (org-air-inbox--apply-item-edits item '(:priority "A")))
    (should (null org-air-view--edit-redo-ring))
    ;; site 3: the refile engine.
    (setq last-command 'ignore)
    (org-air-edit-undo)                           ; undo the priority edit
    (should (= 1 (length org-air-view--edit-redo-ring)))
    (let ((item (org-air-r75--item "inbox.org" "Beta task")))
      (org-air-refile-item item (org-air-r75--file "target.org") "Existing"))
    (should (null org-air-view--edit-redo-ring))))

;;;; -------------------------------------------------------------------
;;;; r75-3 — walk both ways + the TWO-SIDED restamp (revert-RED)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r75-3-walk-both-ways-two-sided-restamp ()
  "Three same-buffer edits (schedule, deadline, the converted tag),
`u'×3 (bytes = each earlier station), `U'×3 (bytes = each later
station, ending at the final), then `u',`U' once more (the probe's
mixed interleave class) — byte-exact at EVERY station, ring/redo
lengths correct throughout, never a Cannot-undo/redo.  The `U'×2/×3
legs are the two-sided-restamp teeth: revert-RED with a one-sided
\(undo-ring-only) restamp."
  (skip-unless (locate-library "org-air"))
  (org-air-r75--with-board nil
    (let ((s0 (org-air-r75--text "inbox.org")))
      (org-air-r75--goto-row "Alpha task")
      (org-air-item-schedule "2026-08-01")
      (let ((s1 (org-air-r75--text "inbox.org")))
        (org-air-r75--goto-row "Beta task")
        (org-air-view--apply-date (quote deadline) "2026-09-01")
        (let ((s2 (org-air-r75--text "inbox.org")))
          (org-air-r75--goto-row "Alpha task")
          (cl-letf (((symbol-function 'read-string)
                     (lambda (&rest _) "later")))
            (org-air-set-tag))
          (let ((s3 (org-air-r75--text "inbox.org")))
            (should (= 3 (length org-air-view--edit-ring)))
            (org-air-r75--recording-messages msgs
              ;; u ×3 — down the ring.
              (setq last-command 'ignore) (org-air-edit-undo)
              (should (equal s2 (org-air-r75--text "inbox.org")))
              (should (= 2 (length org-air-view--edit-ring)))
              (should (= 1 (length org-air-view--edit-redo-ring)))
              (setq last-command 'ignore) (org-air-edit-undo)
              (should (equal s1 (org-air-r75--text "inbox.org")))
              (should (= 1 (length org-air-view--edit-ring)))
              (should (= 2 (length org-air-view--edit-redo-ring)))
              (setq last-command 'ignore) (org-air-edit-undo)
              (should (equal s0 (org-air-r75--text "inbox.org")))
              (should (= 0 (length org-air-view--edit-ring)))
              (should (= 3 (length org-air-view--edit-redo-ring)))
              ;; U ×3 — back up the ring.
              (setq last-command 'ignore) (org-air-edit-redo)
              (should (equal s1 (org-air-r75--text "inbox.org")))
              (should (= 1 (length org-air-view--edit-ring)))
              (should (= 2 (length org-air-view--edit-redo-ring)))
              (setq last-command 'ignore) (org-air-edit-redo)
              (should (equal s2 (org-air-r75--text "inbox.org")))
              (should (= 2 (length org-air-view--edit-ring)))
              (should (= 1 (length org-air-view--edit-redo-ring)))
              (setq last-command 'ignore) (org-air-edit-redo)
              (should (equal s3 (org-air-r75--text "inbox.org")))
              (should (= 3 (length org-air-view--edit-ring)))
              (should (= 0 (length org-air-view--edit-redo-ring)))
              ;; the mixed interleave once more: u then U.
              (setq last-command 'ignore) (org-air-edit-undo)
              (should (equal s2 (org-air-r75--text "inbox.org")))
              (setq last-command 'ignore) (org-air-edit-redo)
              (should (equal s3 (org-air-r75--text "inbox.org")))
              (should (= 3 (length org-air-view--edit-ring)))
              (should (null org-air-view--edit-redo-ring))
              ;; every press succeeded — no guard trip anywhere.
              (should-not (seq-find
                           (lambda (m)
                             (string-match-p "\\`Cannot \\(un\\|re\\)do" m))
                           msgs)))))))))

;;;; -------------------------------------------------------------------
;;;; r75-4 — structural never enters redo (revert-RED)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r75-4-structural-never-enters-redo ()
  "In-place edit, then refile; `u' (refile consumed with the R73
message, both files byte-untouched) ⇒ the redo ring is EMPTY; `u'
again (the in-place edit reverts), `U' re-applies IT; at no station
does either file gain a duplicate heading (occurrence-counted).
Archive same shape.  Revert-RED against a push-structural-to-redo
variant."
  (skip-unless (locate-library "org-air"))
  (org-air-r75--with-board nil
    ;; the in-place edit BENEATH (other.org)...
    (org-air-r75--goto-row "Gamma chore")
    (org-air-item-schedule "2026-08-02")
    (let ((other-edited (org-air-r75--text "other.org")))
      ;; ...then the refile on top (inbox.org -> target.org).
      (let ((item (org-air-r75--item "inbox.org" "Beta task")))
        (org-air-refile-item item (org-air-r75--file "target.org")
                             "Existing"))
      (should (= 2 (length org-air-view--edit-ring)))
      (let ((beta-count
             (lambda ()
               (+ (org-air-r75--count "Beta task"
                                      (org-air-r75--text "inbox.org"))
                  (org-air-r75--count "Beta task"
                                      (org-air-r75--text "target.org"))))))
        (should (= 1 (funcall beta-count)))
        (let ((inbox-after (org-air-r75--text "inbox.org"))
              (target-after (org-air-r75--text "target.org")))
          (org-air-r75--recording-messages msgs
            (org-air-edit-undo)
            (should (seq-find
                     (lambda (m)
                       (string-match-p
                        "\\`Not simply undoable: refile \"Beta task\"" m))
                     msgs)))
          ;; consumed WITHOUT reversal ⇒ never enters the redo ring.
          (should (equal inbox-after (org-air-r75--text "inbox.org")))
          (should (equal target-after (org-air-r75--text "target.org")))
          (should (null org-air-view--edit-redo-ring))
          (should (= 1 (funcall beta-count))))
        ;; the NEXT u reverts the in-place edit; U re-applies IT.
        (setq last-command 'ignore)
        (org-air-edit-undo)
        (should-not (equal other-edited (org-air-r75--text "other.org")))
        (should (= 1 (length org-air-view--edit-redo-ring)))
        (should (eq 'in-place
                    (plist-get (car org-air-view--edit-redo-ring) :kind)))
        (setq last-command 'ignore)
        (org-air-edit-redo)
        (should (equal other-edited (org-air-r75--text "other.org")))
        (should (= 1 (funcall beta-count))))))
  ;; archive: same consumed-without-reversal shape (fresh fixture — the
  ;; structural record on top of a cross-buffer in-place edit).
  (org-air-r75--with-board nil
    (org-air-r75--goto-row "Alpha task")
    (org-air-item-schedule "2026-08-05")       ; in-place beneath (inbox)
    (let ((inbox-edited (org-air-r75--text "inbox.org")))
      (org-air-r75--goto-row "Gamma chore")
      (org-air-item-archive)                   ; structural on top (other)
      (should (eq 'archive
                  (plist-get (car org-air-view--edit-ring) :kind)))
      (let ((other-archived (org-air-r75--text "other.org")))
        (org-air-r75--recording-messages msgs
          (org-air-edit-undo)
          (should (seq-find
                   (lambda (m)
                     (string-match-p "\\`Not simply undoable: archive" m))
                   msgs)))
        (should (equal other-archived (org-air-r75--text "other.org")))
        (should (null org-air-view--edit-redo-ring))
        ;; u reverts the in-place edit beneath; U re-applies it — the
        ;; archived heading never resurrects beside its moved copy.
        (setq last-command 'ignore)
        (org-air-edit-undo)
        (should (= 1 (length org-air-view--edit-redo-ring)))
        (should-not (equal inbox-edited (org-air-r75--text "inbox.org")))
        (setq last-command 'ignore)
        (org-air-edit-redo)
        (should (equal inbox-edited (org-air-r75--text "inbox.org")))
        (should (equal other-archived (org-air-r75--text "other.org")))
        (should (= 0 (org-air-r75--count
                      "Gamma chore" (org-air-r75--text "other.org"))))))))

;;;; -------------------------------------------------------------------
;;;; r75-5 — dead + stale degrades (consumed + message, never an error)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r75-5-dead-and-stale-degrades ()
  "Undo, kill the source buffer, `U' ⇒ the gone-message, consumed, no
error; separately undo, a manual insertion in the source buffer, `U'
⇒ the tick-guard message, consumed, bytes untouched."
  (skip-unless (locate-library "org-air"))
  ;; dead: source buffer gone.
  (org-air-r75--with-board nil
    (org-air-r75--goto-row "Alpha task")
    (org-air-item-schedule "2026-08-01")
    (org-air-edit-undo)
    (should (= 1 (length org-air-view--edit-redo-ring)))
    (let ((snap (org-air-r75--text "inbox.org"))
          (buf (find-file-noselect (org-air-r75--file "inbox.org"))))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buf))
      (org-air-r75--recording-messages msgs
        (org-air-edit-redo)
        (should (seq-find
                 (lambda (m)
                   (string-match-p
                    "\\`Cannot redo: schedule \"Alpha task\".* — source buffer gone\\'"
                    m))
                 msgs)))
      (should (null org-air-view--edit-redo-ring))
      (should (equal snap (org-air-r75--text "inbox.org")))))
  ;; stale: a manual edit after the undo trips the tick guard.
  (org-air-r75--with-board nil
    (org-air-r75--goto-row "Alpha task")
    (org-air-item-schedule "2026-08-01")
    (org-air-edit-undo)
    (let ((buf (find-file-noselect (org-air-r75--file "inbox.org"))))
      (with-current-buffer buf
        (save-excursion (goto-char (point-max)) (insert "manual\n")))
      (let ((snap (org-air-r75--text "inbox.org")))
        (org-air-r75--recording-messages msgs
          (org-air-edit-redo)
          (should (seq-find
                   (lambda (m)
                     ;; the buffer name may be uniquified (inbox.org<dir>).
                     (string-match-p
                      "\\`Cannot redo: schedule \"Alpha task\".* — inbox\\.org.* changed since\\'"
                      m))
                   msgs)))
        (should (null org-air-view--edit-redo-ring))
        ;; the redo did NOT run: disk untouched, the manual edit lives.
        (should (equal snap (org-air-r75--text "inbox.org")))
        (should (string-match-p "manual"
                                (with-current-buffer buf (buffer-string))))
        (with-current-buffer buf (set-buffer-modified-p nil))))))

;;;; -------------------------------------------------------------------
;;;; r75-6 — bounded by conservation under the ONE existing defconst
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r75-6-bounded-by-conservation ()
  "Edits×(max+5) (undo ring capped at max), `u'×5 → redo ring exactly
5, `U'×5 → undo ring back to its exact pre-dance head-20 content (the
very same record objects, same order), redo empty; at NO step does
either ring exceed `org-air-view--edit-ring-max'."
  (skip-unless (locate-library "org-air"))
  (org-air-r75--with-board nil
    (let ((item (org-air-r75--item "inbox.org" "Alpha task"))
          (max org-air-view--edit-ring-max))
      (dotimes (i (+ max 5))
        (org-air-view--at-item-source item
          (format "probe %d" i)
          (end-of-line)
          (insert " x"))
        (should (<= (length org-air-view--edit-ring) max))
        ;; every fresh edit keeps the redo side EMPTY (the clear).
        (should (null org-air-view--edit-redo-ring)))
      (should (= max (length org-air-view--edit-ring)))
      (let ((pre (copy-sequence org-air-view--edit-ring)))
        (dotimes (_ 5)
          (setq last-command 'ignore)
          (org-air-edit-undo)
          (should (<= (length org-air-view--edit-ring) max))
          (should (<= (length org-air-view--edit-redo-ring) max)))
        (should (= 5 (length org-air-view--edit-redo-ring)))
        (should (= (- max 5) (length org-air-view--edit-ring)))
        (org-air-r75--recording-messages msgs
          (dotimes (_ 5)
            (setq last-command 'ignore)
            (org-air-edit-redo)
            (should (<= (length org-air-view--edit-ring) max))
            (should (<= (length org-air-view--edit-redo-ring) max)))
          (should-not (seq-find
                       (lambda (m) (string-match-p "\\`Cannot redo" m))
                       msgs)))
        (should (null org-air-view--edit-redo-ring))
        (should (= max (length org-air-view--edit-ring)))
        ;; conservation: the SAME record objects, the same order.
        (should (cl-every #'eq pre org-air-view--edit-ring))))))

;;;; -------------------------------------------------------------------
;;;; r75-7 — the caught backstop (R53: no signal escapes)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r75-7-caught-backstop ()
  "A forged redo record whose `:tick' matches the buffer's current
tick but whose buffer's last change was a FRESH edit (not an undo):
`U' degrades with the no-redo-step-left message, consumed, zero
bytes, no signal escapes (the buffer-level `undo-redo' refusal is the
independent second layer under the ring clear)."
  (skip-unless (locate-library "org-air"))
  (org-air-r75--with-board nil
    (org-air-r75--goto-row "Alpha task")
    (org-air-item-schedule "2026-08-01")          ; the fresh edit
    (let* ((buf (find-file-noselect (org-air-r75--file "inbox.org")))
           (snap (org-air-r75--text "inbox.org"))
           (bufsnap (with-current-buffer buf (buffer-string))))
      ;; forge: tick passes, but the last change is the edit itself.
      (setq org-air-view--edit-redo-ring
            (list (list :desc "forged redo"
                        :buffer buf
                        :file (buffer-file-name buf)
                        :kind 'in-place
                        :tick (buffer-chars-modified-tick buf)
                        :time (current-time))))
      (org-air-r75--recording-messages msgs
        (org-air-edit-redo)                       ; must NOT signal
        (should (seq-find
                 (lambda (m)
                   (string-match-p
                    "\\`Cannot redo: forged redo — no redo step left in inbox\\.org"
                    m))
                 msgs)))
      (should (null org-air-view--edit-redo-ring))
      (should (equal snap (org-air-r75--text "inbox.org")))
      (should (equal bufsnap (with-current-buffer buf (buffer-string)))))))

;;;; -------------------------------------------------------------------
;;;; r75-8 — empty redo: the gentle user-error (the r73-14 shape)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r75-8-empty-redo-gentle ()
  "Fresh state ⇒ `U' is a gentle `user-error' \"Nothing to redo\",
zero bytes moved anywhere."
  (skip-unless (locate-library "org-air"))
  (org-air-r75--with-corpus nil
    (should (null org-air-view--edit-redo-ring))
    (let ((before (org-air-r75--text "inbox.org"))
          (err (should-error (org-air-edit-redo) :type 'user-error)))
      (should (string-match-p "Nothing to redo" (cadr err)))
      (should (null org-air-view--edit-redo-ring))
      (should (equal before (org-air-r75--text "inbox.org"))))))

;;;; -------------------------------------------------------------------
;;;; r75-9 — key + loop surface (revert-RED)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r75-9-key-and-loop-surface ()
  "The board map resolves `U' to `org-air-edit-redo' and `u' stays
`org-air-edit-undo'; `org-air-process-inbox' driven with
`read-char-exclusive' stubbed to [?U ?q] reaches a spied
`org-air-edit-redo' exactly once; the prompt string contains
`[U]redo' (and keeps `[u]ndo')."
  (skip-unless (locate-library "org-air"))
  (should (commandp 'org-air-edit-redo))
  (should (eq (lookup-key org-air-view-mode-map (kbd "U"))
              'org-air-edit-redo))
  (should (eq (lookup-key org-air-view-mode-map (kbd "u"))
              'org-air-edit-undo))
  (org-air-r75--with-board nil
    (let ((keys (list ?U ?q))
          (prompts nil)
          (redos 0))
      (cl-letf (((symbol-function 'read-char-exclusive)
                 (lambda (&optional prompt &rest _)
                   (push prompt prompts)
                   (pop keys)))
                ((symbol-function 'org-air-edit-redo)
                 (lambda () (cl-incf redos))))
        (org-air-process-inbox))
      (should (= 1 redos))
      (should (seq-find (lambda (p)
                          (and p (string-match-p "\\[U\\]redo" p)))
                        prompts))
      (should (seq-find (lambda (p)
                          (and p (string-match-p "\\[u\\]ndo" p)))
                        prompts)))))

;;;; -------------------------------------------------------------------
;;;; r75-10 — help surfaces: the either-ring gate + the redo sub-list
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r75-10-help-surfaces ()
  "Undo+redo both populated ⇒ the Recent-edits block shows BOTH
sub-lists; BOTH empty ⇒ the block is absent (goldens clean);
redo-only (the single edit undone) ⇒ the block still renders with the
redo sub-list; the Triage group always contains the new `U' row."
  (skip-unless (locate-library "org-air"))
  (org-air-r75--with-board nil
    (unwind-protect
        (progn
          ;; BOTH empty: block absent; the Triage U row always renders.
          (setq org-air-view--edit-ring nil
                org-air-view--edit-redo-ring nil)
          (let ((help (org-air-help--render
                       (get-buffer-create "*r75-help*") 'board
                       (current-buffer))))
            (with-current-buffer help
              (let ((text (buffer-substring-no-properties (point-min)
                                                          (point-max))))
                (should-not (string-match-p "Recent edits" text))
                (should (string-match-p
                         "^  U +redo last undo (a new edit clears redo)"
                         text))
                (should (string-match-p "^  u +undo last edit" text)))))
          ;; both populated: both sub-lists, undo lead first.
          (let ((live (find-file-noselect (org-air-r75--file "inbox.org")))
                (dead (generate-new-buffer "r75-dead")))
            (org-air-view--edit-ring-push
             "schedule \"Alpha task\" → 2026-08-01" live)
            ;; seed the redo side AFTER the push (the push clears it);
            ;; a dead record proves the [gone] suffix on the redo side.
            (kill-buffer dead)
            (setq org-air-view--edit-redo-ring
                  (list (list :desc "deadline \"Beta task\" → 2026-09-01"
                              :buffer live :file (buffer-file-name live)
                              :kind 'in-place
                              :tick (buffer-chars-modified-tick live)
                              :time (current-time))
                        (list :desc "old redo gone"
                              :buffer dead :file nil :kind 'in-place
                              :tick 0 :time (current-time))))
            (let ((help (org-air-help--render
                         (get-buffer-create "*r75-help*") 'board
                         (current-buffer))))
              (with-current-buffer help
                (let ((text (buffer-substring-no-properties (point-min)
                                                            (point-max))))
                  (should (string-match-p "Recent edits" text))
                  (should (string-match-p "u undoes the top" text))
                  (should (string-match-p "U redoes the top" text))
                  (should (string-match-p
                           "1\\. deadline \"Beta task\" → 2026-09-01" text))
                  (should (string-match-p
                           "2\\. old redo gone \\[gone\\]" text))
                  ;; the undo sub-list precedes the redo sub-list.
                  (should (< (string-match "u undoes the top" text)
                             (string-match "U redoes the top" text))))))
            ;; redo-only: the block still renders (the either-ring gate).
            (setq org-air-view--edit-ring nil)
            (let ((help (org-air-help--render
                         (get-buffer-create "*r75-help*") 'board
                         (current-buffer))))
              (with-current-buffer help
                (let ((text (buffer-substring-no-properties (point-min)
                                                            (point-max))))
                  (should (string-match-p "Recent edits" text))
                  (should-not (string-match-p "u undoes the top" text))
                  (should (string-match-p "U redoes the top" text)))))))
      (when (get-buffer "*r75-help*")
        (kill-buffer "*r75-help*")))))

;;;; -------------------------------------------------------------------
;;;; r75-11 — cross-buffer both ways
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r75-11-cross-buffer-both-ways ()
  "Edits in two files, `u'×2 (reverse chronological, each in its own
buffer — the r73-8 shape), `U'×2 re-applies in redo-stack order
\(forward chronological — the opposite walk), bytes exact in both
files at every station."
  (skip-unless (locate-library "org-air"))
  (org-air-r75--with-board nil
    (let ((inbox0 (org-air-r75--text "inbox.org"))
          (other0 (org-air-r75--text "other.org")))
      (org-air-r75--goto-row "Alpha task")
      (org-air-item-schedule "2026-08-01")          ; edit 1: inbox.org
      (let ((inbox1 (org-air-r75--text "inbox.org")))
        (org-air-r75--goto-row "Gamma chore")
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) "urgent")))
          (org-air-set-tag))                        ; edit 2: other.org
        (let ((other1 (org-air-r75--text "other.org")))
          (should (= 2 (length org-air-view--edit-ring)))
          ;; u ×2: reverse chronological, each in its own buffer.
          (org-air-edit-undo)
          (should (equal other0 (org-air-r75--text "other.org")))
          (should (equal inbox1 (org-air-r75--text "inbox.org")))
          (setq last-command 'ignore)
          (org-air-edit-undo)
          (should (equal inbox0 (org-air-r75--text "inbox.org")))
          (should (equal other0 (org-air-r75--text "other.org")))
          (should (= 2 (length org-air-view--edit-redo-ring)))
          ;; U ×2: redo-stack order — FORWARD chronological.
          (setq last-command 'ignore)
          (org-air-edit-redo)                       ; edit 1 back (inbox)
          (should (equal inbox1 (org-air-r75--text "inbox.org")))
          (should (equal other0 (org-air-r75--text "other.org")))
          (setq last-command 'ignore)
          (org-air-edit-redo)                       ; edit 2 back (other)
          (should (equal other1 (org-air-r75--text "other.org")))
          (should (equal inbox1 (org-air-r75--text "inbox.org")))
          (should (null org-air-view--edit-redo-ring))
          (should (= 2 (length org-air-view--edit-ring))))))))

(provide 'org-air-round75-test)
;;; org-air-round75-test.el ends here
