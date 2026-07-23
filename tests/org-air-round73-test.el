;;; org-air-round73-test.el --- executing ERTs for round-73 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for round-73 (air/v0.1/org-air-round73-design.org):
;; post-edit UX — R73-1 the refresh FORCES a pane/inspector resync at the
;; TWO swap tails (`org-air-view--panes-resync-now': direct timer-free
;; calls to `--view-pane-update-now' + `--inspector-update-now',
;; self-limited by struct IDENTITY — rescanned files mint fresh structs,
;; retained files keep `eq' ones; the Decision 2 empty degrade closes the
;; pane / nils the inspector, resync-scoped) and R73-2/-3 `u' generalises
;; to a bounded depth-20 recent-edits ring (recording by construction via
;; the `org-air-view--at-item-source' DESC/:structural upgrade +
;; undo-boundary + atomic-change-group; `--apply-item-edits' and the
;; refile engine push directly; `org-air-set-tag' converted;
;; `org-air-edit-undo' = undo-only + chars-tick guard with post-undo
;; re-stamp; refile/archive honestly structural — named, never a
;; duplicate-making partial undo; Recent-edits block in `?' help,
;; empty-suppressed).
;;
;; All BATCH/headless: the board renders over a temp corpus (the house
;; harness idiom); the batch `org-air-refresh' takes the fully-sync
;; branch whose tail carries the R73-1 resync, and the WARM identity
;; seam drives `org-air-view--refresh-start's sync fast path directly.
;; Spies via `cl-letf'; file bytes compared before/after.  The spec's
;; thirteen seams r73-1..r73-13 map onto the ERTs below (revert-RED
;; where the spec marks them).
;;
;; GUI residue (screenshot/user-confirm, not ERT-able): the LIVE pane
;; window visibly repainting right after an edit with no cursor nudge;
;; the pane window closing when the last item graduates; the rail
;; inspector region repaint timing.

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
;;;; Fixture corpus + harness (the r68 house idiom)
;;;; -------------------------------------------------------------------

(defvar org-air-r73--dir nil
  "The temp corpus directory of the current `org-air-r73--with-corpus'.")

(defconst org-air-r73--default-specs
  '(("inbox.org" . "#+title: inbox\n\n* TODO Alpha task :inbox:\n  alpha body\n* TODO Beta task :inbox:\n  beta body\n")
    ("other.org" . "#+title: other\n\n* TODO Gamma chore\nSCHEDULED: <2026-01-05 Mon>\n  gamma body\n")
    ("target.org" . "#+title: target\n\n* Existing\n"))
  "Default corpus: two inbox items, one OVERDUE item in a second file
\(the untouched-file identity probe), and a refile target.")

(defconst org-air-r73--single-specs
  '(("inbox.org" . "#+title: inbox\n\n* TODO Only task :inbox:\n  body\n"))
  "Single-item corpus for the r73-4 empty degrade.")

(defconst org-air-r73--coverage-specs
  '(("inbox.org" . "#+title: inbox\n\n* TODO Alpha task :inbox:\n  a\n* TODO Beta task :inbox:\n  b\n* TODO Ceta task :inbox:\n  c\n* TODO Delta task :inbox:\n  d\n* TODO Epsilon task :inbox:\n  e\n* TODO Zeta task :inbox:\n  z\n")
    ("target.org" . "#+title: target\n\n* Existing\n"))
  "Six-item corpus: one item per destructive verb for r73-11.")

(defmacro org-air-r73--with-corpus (specs &rest body)
  "Create a temp Org corpus from SPECS and run BODY against it.
Binds the org-air roots, a temp cache, a round-local board buffer name,
a FRESH `org-air-view--edit-ring' (test isolation), `org-tags-column' 0
and lockfile/message quiet.  Kills every corpus-visiting buffer and
deletes the directory afterwards."
  (declare (indent 1) (debug t))
  `(let* ((org-air-r73--dir (make-temp-file "org-air-r73-" t)))
     (unwind-protect
         (progn
           (pcase-dolist (`(,name . ,content)
                          (or ,specs org-air-r73--default-specs))
             (let ((file-name-handler-alist nil)
                   (coding-system-for-write 'utf-8-unix))
               (write-region (or content "") nil
                             (expand-file-name name org-air-r73--dir)
                             nil 'silent)))
           (let ((org-air-files (list org-air-r73--dir))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-r73--dir))
                 (org-air-cache-file
                  (expand-file-name ".cache/board.eld" org-air-r73--dir))
                 (org-air-view-buffer-name "*org-air-r73*")
                 (org-air-view--edit-ring nil)
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
             (when (and fn (string-prefix-p org-air-r73--dir fn))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r73--dir t))))

(defmacro org-air-r73--with-board (specs &rest body)
  "Render the real board over the SPECS corpus; run BODY in its buffer."
  (declare (indent 1) (debug t))
  `(org-air-r73--with-corpus ,specs
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

(defun org-air-r73--goto-row (title)
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

(defun org-air-r73--file (name)
  "Return the corpus file NAME's absolute path."
  (expand-file-name name org-air-r73--dir))

(defun org-air-r73--text (name)
  "Return corpus file NAME's on-disk content as a string."
  (with-temp-buffer
    (insert-file-contents (org-air-r73--file name))
    (buffer-string)))

(defun org-air-r73--item (name text)
  "Build an item for the heading containing TEXT in corpus file NAME.
The r67/r68 idiom for driving the non-board appliers/engine directly."
  (let ((file (org-air-r73--file name)))
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

(defmacro org-air-r73--recording-messages (var &rest body)
  "Run BODY with `message' recorded (formatted strings) into VAR."
  (declare (indent 1) (debug t))
  `(let ((,var nil))
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args)
                  (when fmt (push (apply #'format fmt args) ,var))
                  nil)))
       ,@body)))

(defun org-air-r73--fake-inspector (stale-item)
  "Fabricate live inline-inspector locals in the current (board) buffer.
STALE-ITEM seeds `org-air-view--inspector-item' so an identity-driven
redraw is observable.  Returns nothing; pair with a
`org-air-view--render-inspector-region' stub."
  (setq-local org-air-view--inspector-active t)
  (setq-local org-air-view--inspector-target-buffer nil)
  (setq-local org-air-view--inspector-beg (copy-marker (point-min)))
  (setq-local org-air-view--inspector-end (copy-marker (point-max)))
  (setq-local org-air-view--inspector-item stale-item))

;;;; -------------------------------------------------------------------
;;;; r73-1 — the resync fires at the refresh tail (revert-RED)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r73-1-resync-fires-at-the-refresh-tail ()
  "After a non-interactive schedule on the board, BOTH update-now
functions fire from the refresh tail with point on the SAME row (no
cursor motion in the test); the `--refresh-repaint' tail (the
machine-path shape) fires them too.  RED on revert: today neither
fires — both panes sync only via `post-command-hook' point-tracking,
which a refresh under a stationary cursor never triggers."
  (skip-unless (locate-library "org-air"))
  (org-air-r73--with-board nil
    (org-air-r73--goto-row "Alpha task")
    (let ((pane-calls nil) (insp-calls nil))
      (cl-letf (((symbol-function 'org-air-view--view-pane-update-now)
                 (lambda (buf) (push buf pane-calls)))
                ((symbol-function 'org-air-view--inspector-update-now)
                 (lambda (buf) (push buf insp-calls))))
        (org-air-item-schedule "2026-08-01"))
      (should pane-calls)
      (should insp-calls)
      (should (memq (current-buffer) pane-calls))
      (should (memq (current-buffer) insp-calls))
      ;; no cursor motion: point still reads the SAME row.
      (let ((it (org-air-view--row-property 'org-air-item)))
        (should it)
        (should (equal "Alpha task" (org-air-item-title it)))))
    ;; the `--refresh-repaint' tail driven directly.
    (let ((pane-calls nil) (insp-calls nil))
      (cl-letf (((symbol-function 'org-air-view--view-pane-update-now)
                 (lambda (buf) (push buf pane-calls)))
                ((symbol-function 'org-air-view--inspector-update-now)
                 (lambda (buf) (push buf insp-calls))))
        (org-air-view--refresh-repaint))
      (should pane-calls)
      (should insp-calls))))

;;;; -------------------------------------------------------------------
;;;; r73-2 — identity is the signal
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r73-2-identity-is-the-signal ()
  "The WARM sync fast path preserves the Decision 1 invariant: a
rescanned file's items are fresh structs (never `eq' to their
predecessors) while an untouched file's items are RETAINED — the very
same `eq' objects; with the inspector active the resync leaves
`org-air-view--inspector-item' `eq' to the FRESH struct at point,
never the stale one."
  (skip-unless (locate-library "org-air"))
  (org-air-r73--with-board nil
    ;; establish the mtime baseline (the batch sync refresh writes it).
    (org-air-refresh)
    (org-air-r73--goto-row "Alpha task")
    (let ((alpha-before (org-air-view--row-property 'org-air-item))
          (gamma-before (org-air-test-find-item "Gamma chore"
                                                org-air-view--items)))
      (should alpha-before)
      (should gamma-before)
      ;; touch ONLY inbox.org, then drive the WARM sync fast path.
      (set-file-times (org-air-r73--file "inbox.org")
                      (time-add (current-time) 5))
      (org-air-view--refresh-start)
      ;; the machine must be idle again (the sync path never strands).
      (should-not (eq org-air-view--refresh-state 'refreshing))
      (let ((alpha-after (org-air-view--row-property 'org-air-item))
            (gamma-after (org-air-test-find-item "Gamma chore"
                                                 org-air-view--items)))
        (should alpha-after)
        (should (equal "Alpha task" (org-air-item-title alpha-after)))
        ;; rescanned file => fresh structs.
        (should-not (eq alpha-before alpha-after))
        ;; untouched file => retained, the same eq struct.
        (should (eq gamma-before gamma-after))
        ;; inspector: identity-driven redraw through the resync helper.
        (let ((rendered nil))
          (org-air-r73--fake-inspector alpha-before)
          (cl-letf (((symbol-function 'org-air-view--render-inspector-region)
                     (lambda (thing _target) (push thing rendered))))
            (org-air-view--panes-resync-now))
          (should (eq alpha-after org-air-view--inspector-item))
          (should (eq alpha-after (car rendered))))))))

;;;; -------------------------------------------------------------------
;;;; r73-3 — done lands on the neighbour
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r73-3-done-lands-on-the-neighbour ()
  "Two-item fixture, `org-air-item-done' on the first: the resync's
update-now calls fire with the SURVIVING item now at point (never the
completed struct), and the inspector ends on the survivor."
  (skip-unless (locate-library "org-air"))
  (org-air-r73--with-board nil
    (let ((alpha (org-air-r73--goto-row "Alpha task"))
          (pane-things nil) (insp-things nil))
      (cl-letf (((symbol-function 'org-air-view--view-pane-update-now)
                 (lambda (buf)
                   (push (with-current-buffer buf
                           (org-air-view--row-property 'org-air-item))
                         pane-things)))
                ((symbol-function 'org-air-view--inspector-update-now)
                 (lambda (buf)
                   (push (with-current-buffer buf
                           (org-air-view--row-property 'org-air-item))
                         insp-things))))
        (org-air-item-done))
      (should pane-things)
      (should insp-things)
      ;; the FINAL resync read the survivor, not the completed struct.
      (should (car pane-things))
      (should (equal "Beta task" (org-air-item-title (car pane-things))))
      (should-not (eq alpha (car pane-things)))
      (should (equal "Beta task" (org-air-item-title (car insp-things))))
      ;; the inspector converges on the survivor through the real helper.
      (let ((rendered nil))
        (org-air-r73--fake-inspector alpha)
        (cl-letf (((symbol-function 'org-air-view--render-inspector-region)
                   (lambda (thing _target) (push thing rendered))))
          (org-air-view--panes-resync-now))
        (should org-air-view--inspector-item)
        (should (equal "Beta task"
                       (org-air-item-title org-air-view--inspector-item)))))))

;;;; -------------------------------------------------------------------
;;;; r73-4 — the empty degrade: pane closes, inspector nils
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r73-4-empty-degrade-closes-pane-nils-inspector ()
  "Single-item fixture, done: the resync detects no context —
`org-air-view-pane--hide' fires (window-live faked) and the inspector
renders its nil placeholder; no error, no dead-item render."
  (skip-unless (locate-library "org-air"))
  (org-air-r73--with-board org-air-r73--single-specs
    (let ((only (org-air-r73--goto-row "Only task"))
          (hide-calls 0))
      (cl-letf (((symbol-function 'org-air-view-pane--window-live-p)
                 (lambda () t))
                ((symbol-function 'org-air-view-pane--hide)
                 (lambda () (cl-incf hide-calls))))
        (org-air-item-done))
      (should (> hide-calls 0))
      ;; the board really is empty under point.
      (should-not (org-air-view--row-property 'org-air-item))
      (should-not (org-air-view-pane--context-at-point))
      ;; inspector: the nil placeholder passes through the resync.
      (let ((rendered 'unset))
        (org-air-r73--fake-inspector only)
        (cl-letf (((symbol-function 'org-air-view--render-inspector-region)
                   (lambda (thing _target) (setq rendered thing))))
          (org-air-view--panes-resync-now))
        (should (null rendered))
        (should (null org-air-view--inspector-item))))))

;;;; -------------------------------------------------------------------
;;;; r73-5 — no rescan, no timers; pending debounces cancelled
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r73-5-resync-no-rescan-no-timers ()
  "The resync helper is batch-safe and cache-first (R53): zero
`org-air-query--scan-file' calls, zero `find-file-noselect' calls,
zero idle timers armed — and dummy pending debounce timers are
CANCELLED by the helper."
  (skip-unless (locate-library "org-air"))
  (org-air-r73--with-board nil
    (org-air-r73--goto-row "Alpha task")
    (let ((scans 0) (visits 0) (idles 0)
          (dummy-insp (run-with-timer 1000 nil #'ignore))
          (dummy-pane (run-with-timer 1000 nil #'ignore)))
      (setq org-air-view--inspector-timer dummy-insp)
      (setq-local org-air-view--view-pane-timer dummy-pane)
      (cl-letf (((symbol-function 'org-air-query--scan-file)
                 (lambda (&rest _) (cl-incf scans) nil))
                ((symbol-function 'find-file-noselect)
                 (lambda (&rest _) (cl-incf visits) nil))
                ((symbol-function 'run-with-idle-timer)
                 (lambda (&rest _) (cl-incf idles) nil)))
        (org-air-view--panes-resync-now))
      (should (zerop scans))
      (should (zerop visits))
      (should (zerop idles))
      ;; the dummy debounces are gone from the timer wheel + the slots.
      (should-not (memq dummy-insp timer-list))
      (should-not (memq dummy-pane timer-list))
      (should-not org-air-view--inspector-timer)
      (should-not org-air-view--view-pane-timer)
      ;; R20-3b bookkeeping stays truthful.
      (should (eql (point) org-air-view--view-pane-last-pos)))))

;;;; -------------------------------------------------------------------
;;;; r73-6 — u undoes the last in-place edit, byte-exact (revert-RED)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r73-6-u-undoes-last-in-place-edit ()
  "Schedule an item, `u': the source file bytes byte-equal the
pre-edit snapshot, the buffer is saved, the board refreshed, the ring
popped, the echo names the desc — and the resync spies fire from the
undo's refresh tail.  Plus the R73fix Minor-2 pin:
`org-air-inbox--apply-item-edits' places the macro's LEADING
`undo-boundary', so a preceding UNBOUNDARIED same-buffer Lisp edit
\(the batch/API shape) never merges into the apply's undo group — `u'
reverts EXACTLY the apply edit and the preceding edit survives,
byte-exact (revert-RED: without the boundary `undo-only' eats both)."
  (skip-unless (locate-library "org-air"))
  (org-air-r73--with-board nil
    (org-air-r73--goto-row "Alpha task")
    (let ((before (org-air-r73--text "inbox.org")))
      (org-air-item-schedule "2026-08-01")
      (should-not (equal before (org-air-r73--text "inbox.org")))
      (should (= 1 (length org-air-view--edit-ring)))
      (let ((rec (car org-air-view--edit-ring)))
        (should (eq 'in-place (plist-get rec :kind)))
        (should (string-match-p "\\`schedule \"Alpha task\" → 2026-08-01\\'"
                                (plist-get rec :desc))))
      (let ((pane-calls nil) (msgs nil))
        (cl-letf* (((symbol-function 'org-air-view--view-pane-update-now)
                    (lambda (buf) (push buf pane-calls)))
                   ((symbol-function 'message)
                    (lambda (fmt &rest args)
                      (when fmt (push (apply #'format fmt args) msgs))
                      nil)))
          (org-air-edit-undo))
        ;; byte-exact revert, saved, ring popped, desc named.
        (should (equal before (org-air-r73--text "inbox.org")))
        (should-not (buffer-modified-p
                     (find-file-noselect (org-air-r73--file "inbox.org"))))
        (should (null org-air-view--edit-ring))
        (should (seq-find
                 (lambda (m)
                   (string-match-p
                    "\\`Undid: schedule \"Alpha task\" → 2026-08-01 (0 more)\\'"
                    m))
                 msgs))
        ;; the R73-1 resync rode the undo's refresh tail.
        (should pane-calls))))
  ;; the R73fix Minor-2 pin: an unboundaried preceding Lisp edit must
  ;; NOT ride the apply's undo group.
  (org-air-r73--with-board nil
    (let ((before (org-air-r73--text "inbox.org"))
          (item (org-air-r73--item "inbox.org" "Alpha task"))
          (buf (find-file-noselect (org-air-r73--file "inbox.org"))))
      (with-current-buffer buf
        ;; the unboundaried same-buffer Lisp edit immediately before
        ;; the apply — no command loop closes its group in batch.
        (save-excursion (goto-char (point-max)) (insert "manual line\n")))
      (org-air-inbox--apply-item-edits item '(:priority "A"))
      (should (= 1 (length org-air-view--edit-ring)))
      (should (string-match-p "\\[#A\\]" (org-air-r73--text "inbox.org")))
      (should (string-match-p "manual line" (org-air-r73--text "inbox.org")))
      (setq last-command 'ignore)
      (org-air-r73--recording-messages msgs
        (org-air-edit-undo)
        (should-not (seq-find (lambda (m) (string-match-p "Cannot undo" m))
                              msgs)))
      ;; EXACTLY the apply edit reverted: the priority is gone, the
      ;; preceding manual edit survives — byte-exact on disk.
      (should (equal (concat before "manual line\n")
                     (org-air-r73--text "inbox.org")))
      (should (null org-air-view--edit-ring)))))

;;;; -------------------------------------------------------------------
;;;; r73-7 — undo-only, never toggle (revert-RED)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r73-7-undo-only-never-toggle ()
  "Two edits, `u', an unrelated command, `u': BOTH edits reverted
\(bytes = original).  RED against a plain-`undo' impl, whose second
non-consecutive press REDOES the first undo (the today-toggle)."
  (skip-unless (locate-library "org-air"))
  (org-air-r73--with-board nil
    (let ((before (org-air-r73--text "inbox.org")))
      (org-air-r73--goto-row "Alpha task")
      (org-air-item-schedule "2026-08-01")
      (org-air-r73--goto-row "Alpha task")
      (org-air-view--apply-date (quote deadline) "2026-09-01")
      (should (= 2 (length org-air-view--edit-ring)))
      (org-air-edit-undo)
      ;; an unrelated command between the presses (breaks any
      ;; last-command-based undo continuation — the toggle trap).
      (setq last-command 'ignore)
      (org-air-edit-undo)
      (should (equal before (org-air-r73--text "inbox.org")))
      (should (null org-air-view--edit-ring)))))

;;;; -------------------------------------------------------------------
;;;; r73-8 — cross-buffer order: reverse chronological, own buffers
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r73-8-cross-buffer-reverse-chronological ()
  "Edits in two different files undo in reverse chronological order
across two `u' presses, each in its own buffer."
  (skip-unless (locate-library "org-air"))
  (org-air-r73--with-board nil
    (let ((inbox-before (org-air-r73--text "inbox.org"))
          (other-before (org-air-r73--text "other.org")))
      (org-air-r73--goto-row "Alpha task")
      (org-air-item-schedule "2026-08-01")      ; edit 1: inbox.org
      (let ((inbox-edited (org-air-r73--text "inbox.org")))
        (org-air-r73--goto-row "Gamma chore")
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) "urgent")))
          (org-air-set-tag))                    ; edit 2: other.org
        (should-not (equal other-before (org-air-r73--text "other.org")))
        (should (= 2 (length org-air-view--edit-ring)))
        ;; press 1: the NEWEST edit (other.org) reverts; inbox untouched.
        (org-air-edit-undo)
        (should (equal other-before (org-air-r73--text "other.org")))
        (should (equal inbox-edited (org-air-r73--text "inbox.org")))
        ;; press 2: the older edit (inbox.org) reverts.
        (setq last-command 'ignore)
        (org-air-edit-undo)
        (should (equal inbox-before (org-air-r73--text "inbox.org")))
        (should (null org-air-view--edit-ring))))))

;;;; -------------------------------------------------------------------
;;;; r73-9 — structural honesty: refile + archive (revert-RED)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r73-9-structural-honesty-refile-archive ()
  "Refile then `u': BOTH files' bytes unchanged by the press, the
record consumed, the message names the refile + target (+ the next
desc); the NEXT `u' undoes the in-place edit beneath.  An archive
record behaves identically.  RED on revert: today `u' after a refile
silently undoes the older disposition."
  (skip-unless (locate-library "org-air"))
  (org-air-r73--with-board nil
    (let ((other-before (org-air-r73--text "other.org")))
      ;; the in-place edit BENEATH (other.org)...
      (org-air-r73--goto-row "Gamma chore")
      (org-air-item-schedule "2026-08-02")
      ;; ...then the refile on top (inbox.org -> target.org).
      (let ((item (org-air-r73--item "inbox.org" "Beta task")))
        (org-air-refile-item item (org-air-r73--file "target.org")
                             "Existing"))
      (should (= 2 (length org-air-view--edit-ring)))
      (should (eq 'refile (plist-get (car org-air-view--edit-ring) :kind)))
      (should (string-match-p
               "\\`refile \"Beta task\" → target\\.org › Existing\\'"
               (plist-get (car org-air-view--edit-ring) :desc)))
      (let ((inbox-after (org-air-r73--text "inbox.org"))
            (target-after (org-air-r73--text "target.org")))
        (org-air-r73--recording-messages msgs
          (org-air-edit-undo)
          ;; honestly NOT undone: both files byte-identical.
          (should (equal inbox-after (org-air-r73--text "inbox.org")))
          (should (equal target-after (org-air-r73--text "target.org")))
          (should (= 1 (length org-air-view--edit-ring)))
          (should (seq-find
                   (lambda (m)
                     (string-match-p
                      "\\`Not simply undoable: refile \"Beta task\" → target\\.org › Existing — it moved; refile it back (e) from there\\.  Next u: schedule \"Gamma chore\" → 2026-08-02\\'"
                      m))
                   msgs)))
        ;; the NEXT u undoes the in-place edit beneath.
        (setq last-command 'ignore)
        (org-air-edit-undo)
        (should (equal other-before (org-air-r73--text "other.org")))
        (should (null org-air-view--edit-ring)))
      ;; archive: same consumed-with-message treatment, archive file named.
      (org-air-r73--goto-row "Gamma chore")
      (org-air-item-archive)
      (should (= 1 (length org-air-view--edit-ring)))
      (should (eq 'archive (plist-get (car org-air-view--edit-ring) :kind)))
      (let ((other-archived (org-air-r73--text "other.org")))
        (org-air-r73--recording-messages msgs
          (org-air-edit-undo)
          (should (equal other-archived (org-air-r73--text "other.org")))
          (should (null org-air-view--edit-ring))
          (should (seq-find
                   (lambda (m)
                     (string-match-p
                      "\\`Not simply undoable: archive \"Gamma chore\" → other\\.org_archive — it moved; restore it from the archive file\\.\\'"
                      m))
                   msgs)))))))

;;;; -------------------------------------------------------------------
;;;; r73-10 — dead + stale guards (+ the structural duplicate guard)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r73-10-dead-and-stale-guards ()
  "A dead source buffer degrades with the gone-message (consumed, no
error); a manual post-edit change trips the tick guard (consumed,
bytes untouched); a successful ring undo does NOT trip the guard for
the next same-buffer record (the re-stamp); and an in-place record
recorded BEFORE a same-buffer STRUCTURAL move trips the guard instead
of resurrecting a duplicate."
  (skip-unless (locate-library "org-air"))
  ;; dead: source buffer gone.
  (org-air-r73--with-board nil
    (org-air-r73--goto-row "Alpha task")
    (org-air-item-schedule "2026-08-01")
    (let ((edited (org-air-r73--text "inbox.org"))
          (buf (find-file-noselect (org-air-r73--file "inbox.org"))))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buf))
      (org-air-r73--recording-messages msgs
        (org-air-edit-undo)
        (should (seq-find
                 (lambda (m)
                   (string-match-p "\\`Cannot undo: schedule \"Alpha task\".* — source buffer gone\\'" m))
                 msgs)))
      (should (null org-air-view--edit-ring))
      ;; nothing reverted.
      (should (equal edited (org-air-r73--text "inbox.org")))))
  ;; stale: a manual user edit since the record trips the tick guard.
  (org-air-r73--with-board nil
    (org-air-r73--goto-row "Alpha task")
    (org-air-view--apply-date (quote deadline) "2026-09-01")
    (let ((buf (find-file-noselect (org-air-r73--file "inbox.org")))
          (edited (org-air-r73--text "inbox.org")))
      (with-current-buffer buf
        (save-excursion (goto-char (point-max)) (insert "manual\n")))
      (org-air-r73--recording-messages msgs
        (org-air-edit-undo)
        (should (seq-find
                 (lambda (m)
                   ;; the buffer name may be uniquified (inbox.org<dir>)
                   ;; when another suite left an inbox.org buffer behind.
                   (string-match-p "\\`Cannot undo: deadline \"Alpha task\".* — inbox\\.org.* changed since\\'" m))
                 msgs)))
      (should (null org-air-view--edit-ring))
      ;; the undo did NOT run: disk untouched, the manual edit survives.
      (should (equal edited (org-air-r73--text "inbox.org")))
      (should (string-match-p "manual"
                              (with-current-buffer buf (buffer-string))))
      (with-current-buffer buf (set-buffer-modified-p nil))))
  ;; re-stamp: a ring undo never trips the next same-buffer record —
  ;; and a same-buffer STRUCTURAL record between them stays honest:
  ;; consuming it must NOT let the older record undo the structural cut.
  (org-air-r73--with-board nil
    (org-air-r73--goto-row "Alpha task")
    (org-air-item-schedule "2026-08-01")
    (org-air-r73--goto-row "Beta task")
    (org-air-item-archive)                  ; STRUCTURAL, same buffer
    (should (= 2 (length org-air-view--edit-ring)))
    (org-air-edit-undo)                     ; consume the archive record
    (setq last-command 'ignore)
    (let ((after-archive (org-air-r73--text "inbox.org")))
      (org-air-r73--recording-messages msgs
        (org-air-edit-undo)
        ;; the tick guard trips (the archive cut changed the buffer) —
        ;; NEVER a duplicate-making partial undo of the cut.
        (should (seq-find
                 (lambda (m)
                   (string-match-p "changed since\\'" m))
                 msgs)))
      (should (equal after-archive (org-air-r73--text "inbox.org")))
      (should-not (string-match-p "Beta task"
                                  (org-air-r73--text "inbox.org")))))
  ;; the pure re-stamp seam: two in-place edits, two undos, no trip.
  (org-air-r73--with-board nil
    (let ((before (org-air-r73--text "inbox.org")))
      (org-air-r73--goto-row "Alpha task")
      (org-air-item-schedule "2026-08-01")
      (org-air-r73--goto-row "Beta task")
      (org-air-item-schedule "2026-08-03")
      (org-air-edit-undo)
      (setq last-command 'ignore)
      (org-air-r73--recording-messages msgs
        (org-air-edit-undo)
        (should-not (seq-find
                     (lambda (m) (string-match-p "Cannot undo" m))
                     msgs)))
      (should (equal before (org-air-r73--text "inbox.org"))))))

;;;; -------------------------------------------------------------------
;;;; r73-11 — coverage: every verb records once; the bound holds
;;;; -------------------------------------------------------------------

(defun org-air-r73--ring-single (kind desc-re)
  "Assert the ring holds exactly ONE record of KIND matching DESC-RE."
  (should (= 1 (length org-air-view--edit-ring)))
  (let ((rec (car org-air-view--edit-ring)))
    (should (eq kind (plist-get rec :kind)))
    (should (string-match-p desc-re (plist-get rec :desc))))
  (setq org-air-view--edit-ring nil))

(ert-deftest org-air-r73-11-coverage-and-bound ()
  "Each verb — schedule, deadline, todo, tag (the converted
`org-air-set-tag'), done, kill, archive, refile, and a transient-leg
`--apply-item-edits' with priority + note — leaves exactly ONE record
with the right kind and a desc naming verb + title; pushing max+5
records leaves exactly `org-air-view--edit-ring-max'."
  (skip-unless (locate-library "org-air"))
  (should (= 20 org-air-view--edit-ring-max))
  (org-air-r73--with-board org-air-r73--coverage-specs
    (org-air-r73--goto-row "Alpha task")
    (org-air-item-schedule "2026-08-01")
    (org-air-r73--ring-single 'in-place "\\`schedule \"Alpha task\" → ")
    (org-air-r73--goto-row "Alpha task")
    (org-air-view--apply-date (quote deadline) "2026-09-01")
    (org-air-r73--ring-single 'in-place "\\`deadline \"Alpha task\" → ")
    (org-air-r73--goto-row "Alpha task")
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "zeta")))
      (org-air-set-tag))
    (org-air-r73--ring-single 'in-place "\\`tag \"Alpha task\" \\+zeta\\'")
    ;; the transient in-place leg, driven directly (batch shape).
    (let ((item (org-air-r73--item "inbox.org" "Alpha task")))
      (org-air-inbox--apply-item-edits item '(:priority "A" :note "hello")))
    (org-air-r73--ring-single 'in-place
                              "\\`edit \"Alpha task\": priority, note\\'")
    ;; todo (the R68 completion shape).
    (org-air-r73--goto-row "Beta task")
    (cl-letf (((symbol-function 'org-air-inbox--read-todo-keyword)
               (lambda (&rest _) "DONE")))
      (org-air-item-cycle-todo))
    (org-air-r73--ring-single 'in-place "\\`todo \"Beta task\" → DONE\\'")
    (org-air-r73--goto-row "Ceta task")
    (org-air-item-done)
    (org-air-r73--ring-single 'in-place "\\`done \"Ceta task\"\\'")
    (org-air-r73--goto-row "Delta task")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (org-air-item-kill))
    (org-air-r73--ring-single 'in-place "\\`kill \"Delta task\"\\'")
    (org-air-r73--goto-row "Epsilon task")
    (org-air-item-archive)
    (org-air-r73--ring-single
     'archive "\\`archive \"Epsilon task\" → inbox\\.org_archive\\'")
    ;; refile: the engine call (the transient's execute leg shape).
    (let ((item (org-air-r73--item "inbox.org" "Zeta task")))
      (org-air-refile-item item (org-air-r73--file "target.org") "Existing"))
    (org-air-r73--ring-single
     'refile "\\`refile \"Zeta task\" → target\\.org › Existing\\'")
    ;; the bound: max+5 pushes leave exactly max, newest at the head.
    (with-temp-buffer
      (dotimes (i (+ org-air-view--edit-ring-max 5))
        (org-air-view--edit-ring-push (format "push %d" i) (current-buffer))))
    (should (= org-air-view--edit-ring-max
               (length org-air-view--edit-ring)))
    (should (equal (format "push %d" (+ org-air-view--edit-ring-max 4))
                   (plist-get (car org-air-view--edit-ring) :desc)))))

;;;; -------------------------------------------------------------------
;;;; r73-12 — the atomic macro (revert-RED)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r73-12-atomic-macro-signal-rolls-back ()
  "A signalling BODY through `org-air-view--at-item-source' leaves the
source bytes identical, the buffer unmodified, and NO ring record —
and the R68 flush still lands INSIDE the group on the success path
\(order pinned: body → flush → save; never on the error path)."
  (skip-unless (locate-library "org-air"))
  (org-air-r73--with-corpus nil
    (let* ((item (org-air-r73--item "inbox.org" "Alpha task"))
           (before (org-air-r73--text "inbox.org"))
           (buf (find-file-noselect (org-air-r73--file "inbox.org"))))
      (setq org-air-view--edit-ring nil)
      (should-error (org-air-view--at-item-source item
                      (format "edit \"%s\"" (org-air-item-title item))
                      (org-todo 'done)
                      (error "boom")))
      ;; rolled back: bytes identical on disk AND in-buffer, unmodified,
      ;; nothing recorded.
      (should (equal before (org-air-r73--text "inbox.org")))
      (should (equal before (with-current-buffer buf (buffer-string))))
      (should-not (buffer-modified-p buf))
      (should (null org-air-view--edit-ring))
      ;; order pin (success path): body → flush → save.
      (let ((events nil))
        (cl-letf (((symbol-function 'org-air-inbox--flush-pending-log-note)
                   (lambda () (push 'flush events)))
                  ((symbol-function 'save-buffer)
                   (lambda (&rest _) (push 'save events))))
          (org-air-view--at-item-source item
            "probe"
            (push 'body events)))
        (should (equal '(body flush save) (nreverse events))))
      (setq org-air-view--edit-ring nil)
      ;; error path: the flush never runs (it sits after BODY inside the
      ;; group; the signal propagates first).
      (let ((events nil))
        (cl-letf (((symbol-function 'org-air-inbox--flush-pending-log-note)
                   (lambda () (push 'flush events))))
          (should-error (org-air-view--at-item-source item
                          (error "boom"))))
        (should-not (memq 'flush events))))))

;;;; -------------------------------------------------------------------
;;;; r73-13 — help block + wording + the kept alias
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r73-13-help-block-and-wording ()
  "With a non-empty ring the help buffer carries the Recent-edits
block (newest first, structural/[gone] suffixes, `u undoes the top');
with an EMPTY ring the block is absent (help goldens stay clean); the
`u' row reads the new wording; `org-air-triage-undo' remains fboundp
and is definition-wise `org-air-edit-undo'."
  (skip-unless (locate-library "org-air"))
  (org-air-r73--with-board nil
    (unwind-protect
        (progn
          ;; empty ring: block ABSENT, wording present.
          (setq org-air-view--edit-ring nil)
          (let ((help (org-air-help--render
                       (get-buffer-create "*r73-help*") 'board
                       (current-buffer))))
            (with-current-buffer help
              (let ((text (buffer-substring-no-properties (point-min)
                                                          (point-max))))
                (should-not (string-match-p "Recent edits" text))
                (should (string-match-p
                         "undo last edit (ring; \\? shows recent)" text)))))
          ;; non-empty ring: newest first + suffixes.
          (let ((dead (generate-new-buffer "r73-dead"))
                (live (find-file-noselect (org-air-r73--file "inbox.org"))))
            (org-air-view--edit-ring-push
             "schedule \"Alpha task\" → 2026-08-01" live)
            (org-air-view--edit-ring-push "old gone edit" dead)
            (kill-buffer dead)
            (org-air-view--edit-ring-push
             "refile \"Beta task\" → target.org" live 'refile)
            (let ((help (org-air-help--render
                         (get-buffer-create "*r73-help*") 'board
                         (current-buffer))))
              (with-current-buffer help
                (let ((text (buffer-substring-no-properties (point-min)
                                                            (point-max))))
                  (should (string-match-p "Recent edits" text))
                  (should (string-match-p "u undoes the top" text))
                  (should (string-match-p
                           "1\\. refile \"Beta task\" → target\\.org \\[not simply undoable\\]"
                           text))
                  (should (string-match-p "2\\. old gone edit \\[gone\\]" text))
                  (should (string-match-p
                           "3\\. schedule \"Alpha task\" → 2026-08-01" text))
                  ;; newest first: the refile row precedes the schedule row.
                  (should (< (string-match "refile \"Beta task\"" text)
                             (string-match "schedule \"Alpha task\"" text)))))))
          ;; the kept alias.
          (should (fboundp 'org-air-triage-undo))
          (should (eq (symbol-function 'org-air-triage-undo)
                      'org-air-edit-undo))
          (should (commandp 'org-air-edit-undo)))
      (when (get-buffer "*r73-help*")
        (kill-buffer "*r73-help*")))))

;;;; -------------------------------------------------------------------
;;;; r73-14 — AUDIT gap: the empty ring + the dead-buffer PUSH side
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r73-14-empty-ring-and-dead-push ()
  "AUDIT gap seams: `u' with an EMPTY ring is a gentle `user-error'
\(never a crash, never a stray undo in some buffer) — through the kept
alias too; and `org-air-view--edit-ring-push' with a KILLED buffer
records nothing and never signals (the push-side dead-buffer guard —
r73-10 covers the POP side, where the buffer dies after the record)."
  (skip-unless (locate-library "org-air"))
  (org-air-r73--with-corpus nil
    ;; empty ring: user-error, nothing consumed, no stray undo anywhere.
    (should (null org-air-view--edit-ring))
    (let ((before (org-air-r73--text "inbox.org"))
          (err (should-error (org-air-edit-undo) :type 'user-error)))
      (should (string-match-p "Nothing to undo" (cadr err)))
      (should-error (org-air-triage-undo) :type 'user-error)
      (should (null org-air-view--edit-ring))
      (should (equal before (org-air-r73--text "inbox.org"))))
    ;; dead-buffer PUSH: no record, no signal (the ring never holds a
    ;; born-dead tombstone).
    (let ((dead (generate-new-buffer "r73-dead-push")))
      (kill-buffer dead)
      (org-air-view--edit-ring-push "never recorded" dead)
      (should (null org-air-view--edit-ring)))))

;;;; -------------------------------------------------------------------
;;;; r73-15 — AUDIT gap: a bucket graduates EMPTY on a still-populated
;;;;          board (+ the chrome keep-last RULING pin)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r73-15-bucket-graduates-empty-board-still-populated ()
  "AUDIT gap seam between r73-3 (neighbour in the SAME bucket) and
r73-4 (whole board empty): the LAST item of one bucket graduates while
OTHER buckets still hold items — context resolves onto a neighbour
from another bucket, the pane is NOT closed (the Decision 2 degrade is
reserved for a truly empty board), and the inspector converges on the
survivor.  Plus the impl-RULING pin nothing else covers: a nil-thing
CHROME row with items still resolvable below SKIPS the inspector nudge
\(keep-last) — the region keeps its render instead of degrading to the
placeholder on every refresh.  Plus the R73fix Minor-1 pin: point
parked BELOW the last row (the pad tail, `M->') on a still-populated
board — ctx is nil (the R24-4 fall-forward never looks backward) but
the board is NOT empty, so the Decision 2 degrade must NOT fire: the
pane stays open (no hide) and the inspector keeps its render, never
the nil placeholder (revert-RED against the bare `(null ctx)' gate)."
  (skip-unless (locate-library "org-air"))
  (org-air-r73--with-board nil
    ;; Gamma is the ONLY dated item in the corpus — done empties its
    ;; bucket while the inbox items (and the plain heading) remain.
    (let ((gamma (org-air-r73--goto-row "Gamma chore"))
          (hide-calls 0))
      (cl-letf (((symbol-function 'org-air-view-pane--window-live-p)
                 (lambda () t))
                ((symbol-function 'org-air-view-pane--hide)
                 (lambda () (cl-incf hide-calls))))
        (org-air-item-done))
      ;; NOT the r73-4 degrade: the board still has items, so the pane
      ;; stays (hide would have fired — the window-live guard is faked).
      (should (zerop hide-calls))
      (let ((survivor (org-air-view--row-property 'org-air-item)))
        ;; point landed on a NEIGHBOUR item row from another bucket.
        (should survivor)
        (should-not (eq gamma survivor))
        (should-not (equal "Gamma chore" (org-air-item-title survivor)))
        (should (org-air-view-pane--context-at-point))
        ;; the inspector converges on the survivor through the real helper.
        (let ((rendered nil))
          (org-air-r73--fake-inspector gamma)
          (cl-letf (((symbol-function 'org-air-view--render-inspector-region)
                     (lambda (thing _target) (push thing rendered))))
            (org-air-view--panes-resync-now))
          (should rendered)
          (should (eq survivor (car rendered)))
          (should (eq survivor org-air-view--inspector-item))))
      ;; the CHROME keep-last ruling: a nil-thing row (the banner) with
      ;; items below — the inspector nudge is SKIPPED, the seed kept.
      (goto-char (point-min))
      (should-not (get-text-property (point)
                                     org-air-view--inspector-property))
      (should (org-air-view-pane--context-at-point))
      (let ((rendered 'unset))
        (org-air-r73--fake-inspector gamma)
        (cl-letf (((symbol-function 'org-air-view--render-inspector-region)
                   (lambda (thing _target) (setq rendered thing))))
          (org-air-view--panes-resync-now))
        (should (eq rendered 'unset))
        (should (eq gamma org-air-view--inspector-item)))
      ;; the R73fix Minor-1 pin: point parked BELOW the last row — the
      ;; pad tail (`M->').  ctx is nil, but the board still has items:
      ;; the resync must keep the pane OPEN (no hide) and skip the
      ;; inspector nudge (keep-last), never the empty degrade.
      (goto-char (point-max))
      (should-not (get-text-property (point)
                                     org-air-view--inspector-property))
      (should-not (org-air-view-pane--context-at-point))
      ;; the board is NOT empty — item rows remain above point.
      (should (text-property-not-all (point-min) (point-max)
                                     'org-air-item nil))
      (let ((tail-hides 0) (rendered 'unset))
        (org-air-r73--fake-inspector gamma)
        (cl-letf (((symbol-function 'org-air-view-pane--window-live-p)
                   (lambda () t))
                  ((symbol-function 'org-air-view-pane--hide)
                   (lambda () (cl-incf tail-hides)))
                  ((symbol-function 'org-air-view--render-inspector-region)
                   (lambda (thing _target) (setq rendered thing))))
          (org-air-view--panes-resync-now))
        (should (zerop tail-hides))
        (should (eq rendered 'unset))
        (should (eq gamma org-air-view--inspector-item))))))

;;;; -------------------------------------------------------------------
;;;; r73-16 — AUDIT compose: undo → fresh edit → undo; a depth-3 walk
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r73-16-compose-undo-fresh-edit-and-deep-walk ()
  "AUDIT compose seams: (a) undo → FRESH edit → undo — the new record
is stamped against the post-undo state and undoes byte-exact (no guard
trip: the undo-side re-stamp and the fresh push compose); (b) THREE
same-buffer edits (schedule, deadline, the converted tag) walk fully
back down the ring across three `u' presses — the re-stamp chain holds
at depth 3 — ending byte-identical with an empty ring and never a
`Cannot undo'."
  (skip-unless (locate-library "org-air"))
  (org-air-r73--with-board nil
    (let ((before (org-air-r73--text "inbox.org")))
      ;; (a) undo, then a fresh edit, then undo.
      (org-air-r73--goto-row "Alpha task")
      (org-air-item-schedule "2026-08-01")
      (org-air-edit-undo)
      (should (equal before (org-air-r73--text "inbox.org")))
      (org-air-r73--goto-row "Alpha task")
      (org-air-view--apply-date (quote deadline) "2026-09-01")
      (should (= 1 (length org-air-view--edit-ring)))
      (setq last-command 'ignore)
      (org-air-r73--recording-messages msgs
        (org-air-edit-undo)
        (should-not (seq-find (lambda (m) (string-match-p "Cannot undo" m))
                              msgs)))
      (should (equal before (org-air-r73--text "inbox.org")))
      (should (null org-air-view--edit-ring))
      ;; (b) three same-buffer edits; three presses walk the whole ring.
      (org-air-r73--goto-row "Alpha task")
      (org-air-item-schedule "2026-08-01")
      (org-air-r73--goto-row "Beta task")
      (org-air-view--apply-date (quote deadline) "2026-09-01")
      (org-air-r73--goto-row "Alpha task")
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "later")))
        (org-air-set-tag))
      (should (= 3 (length org-air-view--edit-ring)))
      (org-air-r73--recording-messages msgs
        (dotimes (_ 3)
          (setq last-command 'ignore)
          (org-air-edit-undo))
        (should-not (seq-find (lambda (m) (string-match-p "Cannot undo" m))
                              msgs)))
      (should (equal before (org-air-r73--text "inbox.org")))
      (should (null org-air-view--edit-ring)))))

(provide 'org-air-round73-test)
;;; org-air-round73-test.el ends here
