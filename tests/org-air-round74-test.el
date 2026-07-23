;;; org-air-round74-test.el --- executing ERTs for round-74 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for round-74 (air/v0.1/org-air-round74-design.org):
;; the inspector "Updated" line — last activity derived at RENDER time
;; from the cached R61 slot heads (`logs' head note/state, the newest
;; clock's END, `created'; strict MAX with the logs > clocks > created
;; tie order; labels note/done/state/clock/created — the CLASS the scan
;; cached, never a guessed keyword), one KV line after Created in the
;; Created byte-shape; slotless items fall back to ONE bounded live
;; file-mtime stat marked "~file", always-on (zero knobs) and
;; FUTURE-CLAMPED (the frozen-clock golden-determinism lemma).  PLUS
;; the R69-2 sibling site folded in: the dead ✕ clear glyph dropped
;; from the HEADER/BANNER filter render (the rail's `\\ clears' hint is
;; the teaching surface; the project header keeps its glyph).
;;
;; All BATCH/headless: temp org corpora scanned through the REAL
;; pipeline (`org-air-query-items' — the R61 slots come from the
;; harvest, never hand-built, except where a seam says slot-injected);
;; NOW frozen to `org-air-test-now' (Mon 2026-06-15 10:00); mtimes set
;; deterministically via `set-file-times'; spies via `cl-letf' on
;; `org-air-query--scan-file' / `find-file-noselect' /
;; `file-attributes'.  The spec's ten seams r74-1..r74-10 map onto the
;; ERTs below (revert-RED where the spec marks them).
;;
;; GUI residue (screenshot/user-confirm, not ERT-able): the Updated
;; line visible in the live rail and its repaint on the R73 resync
;; after an `n' note.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)

(when (locate-library "org-air")
  (require 'org-air))

(defvar org-air-log-cap)
(defvar org-air-cache-file)
(defvar org-air-exclude-regexps)

;;;; -------------------------------------------------------------------
;;;; Corpus scaffolding (the r61/r73 house idiom)
;;;; -------------------------------------------------------------------

(defvar org-air-r74--dir nil
  "The temp corpus directory of the current `org-air-r74--with-corpus'.")

(defun org-air-r74--reset-tables ()
  "Clear the GLOBAL query-layer tables so temp paths never leak."
  (clrhash org-air-query--file-meta)
  (clrhash org-air-query--visits)
  (clrhash org-air-query--denote-id-index)
  (setq org-air-query--link-graph-dirty nil))

(defmacro org-air-r74--with-corpus (specs &rest body)
  "Create a temp Org corpus from SPECS and run BODY against it.
SPECS is a list of (NAME . CONTENT) files written into a fresh temp
directory (root TRUENAMED so path spellings are stable).  Binds the
org-air roots, a temp cache, `org-tags-column' 0 and lockfile/message
quiet; starts from EMPTY query tables and cleans up the tables, every
org-air view buffer, every corpus-visiting buffer and the directory."
  (declare (indent 1) (debug t))
  `(let* ((org-air-r74--dir (file-truename (make-temp-file "org-air-r74-" t))))
     (unwind-protect
         (progn
           (org-air-r74--reset-tables)
           (pcase-dolist (`(,name . ,content) ,specs)
             (let ((path (expand-file-name name org-air-r74--dir))
                   (coding-system-for-write 'utf-8-unix))
               (make-directory (file-name-directory path) t)
               (write-region (or content "") nil path nil 'silent)))
           (let ((org-air-files (list org-air-r74--dir))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-r74--dir))
                 (org-air-exclude-regexps nil)
                 (org-air-cache-file
                  (expand-file-name ".cache/board.eld" org-air-r74--dir))
                 (org-air-plain-heading-type 'task)
                 (org-tags-column 0)
                 (create-lockfiles nil)
                 (inhibit-message t))
             (save-window-excursion
               ,@body)))
       (when (fboundp 'org-air-query-teardown)
         (org-air-query-teardown))
       (org-air-r74--reset-tables)
       (let ((kill-buffer-query-functions nil))
         (dolist (name (list org-air-view-buffer-name
                             org-air-rail-buffer-name))
           (when (get-buffer name)
             (kill-buffer name)))
         (dolist (buf (buffer-list))
           (let ((fn (buffer-file-name buf)))
             (when (and fn (string-prefix-p org-air-r74--dir fn))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r74--dir t))))

(defmacro org-air-r74--with-board (specs &rest body)
  "Render the real 160x50 board over SPECS (frozen clock); run BODY in it.
160x50 is an inspector-bearing geometry (the x50 golden tier) whose
42-col rail fits the full Updated value untruncated, so the inline
inspector region exists, its markers are live and the byte-shape
asserts read whole."
  (declare (indent 1) (debug t))
  `(org-air-r74--with-corpus ,specs
     (org-air-viewport-test--with-frozen-now
       (let ((org-air-view-width 160)
             (org-air-view-height 50))
         (org-air)
         (let ((buf (get-buffer org-air-view-buffer-name)))
           (should buf)
           (with-current-buffer buf
             ,@body))))))

(defun org-air-r74--file (name)
  "Return the absolute path of corpus file NAME."
  (expand-file-name name org-air-r74--dir))

(defun org-air-r74--epoch (y m d &optional hh mm)
  "Return the LOCAL integer epoch of Y-M-D HH:MM (defaults midnight).
The tests' independent oracle, built with `encode-time' on local
calendar dates — TZ-independent comparisons against the harvest."
  (floor (float-time (encode-time (list 0 (or mm 0) (or hh 0)
                                        d m y nil -1 nil)))))

(defun org-air-r74--item (title items)
  "Return the item in ITEMS whose title contains TITLE; assert it exists."
  (let ((item (org-air-test-find-item title items)))
    (should item)
    item))

(defun org-air-r74--line (item)
  "Render ITEM's Updated line at inset \"\" against the frozen NOW.
Returns the property-stripped string, or nil."
  (when-let* ((line (org-air-view--item-updated-line
                     item "" org-air-test-now)))
    (substring-no-properties line)))

(defun org-air-r74--goto-row (title)
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

(defun org-air-r74--inspector-text ()
  "Return the board's inspector-region text (property-stripped)."
  (should (and org-air-view--inspector-beg org-air-view--inspector-end
               (marker-buffer org-air-view--inspector-beg)))
  (buffer-substring-no-properties org-air-view--inspector-beg
                                  org-air-view--inspector-end))

(defmacro org-air-r74--spied (counts &rest body)
  "Run BODY with the three R74 I/O spies counting into COUNTS.
COUNTS is a symbol naming a 3-slot vector [STATS SCANS FFNS]; each spy
DELEGATES to the real function after counting, so behaviour is real."
  (declare (indent 1) (debug t))
  `(let* ((real-attrs (symbol-function 'file-attributes))
          (real-scan (symbol-function 'org-air-query--scan-file))
          (real-ffns (symbol-function 'find-file-noselect)))
     (cl-letf (((symbol-function 'file-attributes)
                (lambda (&rest args)
                  (cl-incf (aref ,counts 0))
                  (apply real-attrs args)))
               ((symbol-function 'org-air-query--scan-file)
                (lambda (&rest args)
                  (cl-incf (aref ,counts 1))
                  (apply real-scan args)))
               ((symbol-function 'find-file-noselect)
                (lambda (&rest args)
                  (cl-incf (aref ,counts 2))
                  (apply real-ffns args))))
       ,@body)))

;;;; -------------------------------------------------------------------
;;;; r74-1 — a recent note wins (revert-RED: no Updated line today)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r74-1-a-recent-note-wins ()
  "A `- Note taken on' stamp newer than a state change wins the MAX.
The pure helper returns the note's epoch + `note'; the rendered line
reads EXACTLY \"Updated 2026-06-14  (1d ago · note)\" against the
frozen NOW (key padded to `org-air-inspector-key-w' 8, Created's
byte-shape).  RED on revert: neither helper exists pre-R74."
  (skip-unless (locate-library "org-air"))
  (org-air-r74--with-corpus
      '(("inbox.org" . "#+title: inbox\n\n* TODO Notey :inbox:\n- State \"DONE\" from \"TODO\" [2026-06-10 Wed 09:00]\n- Note taken on [2026-06-14 Sun 18:00] \\\\\n  the note text\n  body\n"))
    (let* ((items (org-air-query-items))
           (item (org-air-r74--item "Notey" items))
           (u (org-air-view--item-updated item)))
      (should u)
      (should (= (car u) (org-air-r74--epoch 2026 6 14 18 0)))
      (should (eq (cdr u) 'note))
      (should (equal "Updated 2026-06-14  (1d ago · note)"
                     (org-air-r74--line item))))))

;;;; -------------------------------------------------------------------
;;;; r74-2 — state kinds via the FILE's own vocabulary (R57-1)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r74-2-state-kinds-via-file-vocabulary ()
  "A newest `- State \"DONE\"' labels \"done\"; a newest `- State
\"WAITING\"' (declared NOT-done in the file's own `#+TODO:') labels
\"state\" — the KIND came classified from the scan (R57-1), the
renderer never guesses a keyword."
  (skip-unless (locate-library "org-air"))
  (org-air-r74--with-corpus
      '(("inbox.org" . "#+title: inbox\n#+TODO: TODO WAITING | DONE\n\n* TODO Donely :inbox:\n- State \"DONE\" from \"TODO\" [2026-06-12 Fri 09:00]\n  body\n* TODO Waity :inbox:\n- State \"WAITING\" from \"TODO\" [2026-06-12 Fri 09:00]\n  body\n"))
    (let* ((items (org-air-query-items))
           (donely (org-air-r74--item "Donely" items))
           (waity (org-air-r74--item "Waity" items)))
      (should (eq (cdr (org-air-view--item-updated donely)) 'done))
      (should (eq (cdr (org-air-view--item-updated waity)) 'state))
      (should (equal "Updated 2026-06-12  (3d ago · done)"
                     (org-air-r74--line donely)))
      (should (equal "Updated 2026-06-12  (3d ago · state)"
                     (org-air-r74--line waity))))))

;;;; -------------------------------------------------------------------
;;;; r74-3 — the MAX + the head-END clock contract + the tie order
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r74-3-max-head-end-and-tie-order ()
  "Clock END beats an older log and created; created-only floors; ties
resolve logs > clocks.  The clock candidate is the head's END (the
interval spans midnight, so the date proves END, not START)."
  (skip-unless (locate-library "org-air"))
  (org-air-r74--with-corpus
      '(("inbox.org" . "#+title: inbox\n\n* TODO Clocky :inbox:\n:PROPERTIES:\n:CREATED: [2026-06-01 Mon 08:00]\n:END:\nCLOCK: [2026-06-11 Thu 22:00]--[2026-06-12 Fri 01:00] =>  3:00\n- State \"DONE\" from \"TODO\" [2026-06-10 Wed 09:00]\n  body\n* TODO Createdonly :inbox:\n:PROPERTIES:\n:CREATED: [2026-06-01 Mon 08:00]\n:END:\n  body\n* TODO Tie :inbox:\nCLOCK: [2026-06-11 Thu 22:00]--[2026-06-12 Fri 01:00] =>  3:00\n- Note taken on [2026-06-12 Fri 01:00]\n  body\n"))
    (let* ((items (org-air-query-items))
           (clocky (org-air-r74--item "Clocky" items))
           (conly (org-air-r74--item "Createdonly" items))
           (tie (org-air-r74--item "Tie" items))
           (uc (org-air-view--item-updated clocky)))
      ;; the clock wins with the head's END — 06-12 01:00, NOT the
      ;; 06-11 22:00 START.
      (should (= (car uc) (org-air-r74--epoch 2026 6 12 1 0)))
      (should (eq (cdr uc) 'clock))
      (should (equal "Updated 2026-06-12  (3d ago · clock)"
                     (org-air-r74--line clocky)))
      ;; created-only heading -> the floor.
      (should (eq (cdr (org-air-view--item-updated conly)) 'created))
      (should (equal "Updated 2026-06-01  (14d ago · created)"
                     (org-air-r74--line conly)))
      ;; equal-epoch note vs clock-end -> the note label (A > B).
      (let ((ut (org-air-view--item-updated tie)))
        (should (= (car ut) (org-air-r74--epoch 2026 6 12 1 0)))
        (should (eq (cdr ut) 'note))))))

;;;; -------------------------------------------------------------------
;;;; r74-4 — placement + shape on the real surface
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r74-4-placement-and-shape-on-the-surface ()
  "Full board render, inspector active: the inspector region orders
Created THEN Updated; the Updated key is padded to
`org-air-inspector-key-w' (one pad space between the 7-char key and the
date) and the value carries the faded face."
  (skip-unless (locate-library "org-air"))
  (org-air-r74--with-board
      '(("inbox.org" . "#+title: inbox\n\n* TODO Full item :inbox:\n:PROPERTIES:\n:CREATED: [2026-06-01 Mon 08:00]\n:END:\n- Note taken on [2026-06-14 Sun 18:00] \\\\\n  a note\n  body\n"))
    (let ((text (org-air-r74--inspector-text)))
      (should (string-match-p "Created 2026-06-01" text))
      ;; the padded key: EXACTLY one pad space (7-char key, key-w 8),
      ;; the Created byte-shape value after it.
      (should (string-match-p "Updated 2026-06-14  (1d ago · note)" text)))
    ;; ordering on the surface: Created's line precedes Updated's.
    (let ((created-at (progn (goto-char (point-min))
                             (search-forward "Created 2026-06-01")))
          (updated-at (progn (goto-char (point-min))
                             (search-forward "Updated 2026-06-14"))))
      (should (< created-at updated-at)))
    ;; the value carries the faded face, and the line sits in the
    ;; inspector region proper.
    (goto-char (point-min))
    (search-forward "Updated 2026-06-14")
    (let ((date-beg (- (point) (length "2026-06-14"))))
      (should (get-text-property date-beg 'org-air-inspector))
      (let ((face (get-text-property date-beg 'face)))
        (should (memq 'org-air-face-faded
                      (if (listp face) face (list face))))))))

;;;; -------------------------------------------------------------------
;;;; r74-5 — the fallback: ONE bounded live stat, marked "~file"
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r74-5-fallback-one-bounded-stat ()
  "A slotless heading (no LOGBOOK, no clock, no `:CREATED:') falls back
to ONE `file-attributes' stat on its file: mtime 2026-06-10 renders
\"Updated 2026-06-10  (5d ago · ~file)\"; exactly ONE stat from the
line helper, zero scans, zero `find-file-noselect'."
  (skip-unless (locate-library "org-air"))
  (org-air-r74--with-corpus
      '(("inbox.org" . "#+title: inbox\n\n* Bare heading :inbox:\n  just text\n"))
    (let* ((items (org-air-query-items))
           (item (org-air-r74--item "Bare heading" items))
           (counts (vector 0 0 0)))
      ;; the R61 slots really are empty (the harvest ran, found nothing).
      (should (null (org-air-item-logs item)))
      (should (null (org-air-item-clocks item)))
      (should (null (org-air-item-created item)))
      (set-file-times (org-air-r74--file "inbox.org")
                      (encode-time (list 0 0 12 10 6 2026 nil -1 nil)))
      (let (line)
        (org-air-r74--spied counts
          (setq line (org-air-view--item-updated-line
                      item "" org-air-test-now)))
        (should (equal "Updated 2026-06-10  (5d ago · ~file)"
                       (substring-no-properties line)))
        (should (= 1 (aref counts 0)))   ; exactly ONE stat
        (should (= 0 (aref counts 1)))   ; zero scans
        (should (= 0 (aref counts 2))))))) ; zero find-file-noselect

;;;; -------------------------------------------------------------------
;;;; r74-6 — the future clamp: the golden-determinism lemma
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r74-6-future-clamp-golden-lemma ()
  "A slotless heading whose file mtime post-dates the frozen NOW
renders NO Updated line, no error — the exact configuration of every
inspector-bearing x50 golden (the fixture copy's mtime at render is the
real wall clock, permanently AFTER frozen 2026-06-15), pinned here so a
future regen surprise has a named cause.  The standard-fixture board
leg re-creates the golden configuration end to end."
  (skip-unless (locate-library "org-air"))
  (org-air-r74--with-corpus
      '(("inbox.org" . "#+title: inbox\n\n* Bare heading :inbox:\n  just text\n"))
    (let* ((items (org-air-query-items))
           (item (org-air-r74--item "Bare heading" items)))
      (set-file-times (org-air-r74--file "inbox.org")
                      (encode-time (list 0 0 12 20 6 2026 nil -1 nil)))
      (should-not (org-air-view--item-updated-line
                   item "" org-air-test-now))))
  ;; the golden leg: the x50 mockup configuration — the inspected
  ;; "Quick note about org-air idea" is slotless, its scratch-copy
  ;; mtime is the REAL clock (after the frozen NOW), so the clamp
  ;; suppresses the line and the board never shows "Updated".
  (org-air-viewport-test-with-dashboard (cons 100 50)
    (should (string-match-p "Quick note" (buffer-string)))
    (should-not (string-match-p "Updated" (buffer-string)))))

;;;; -------------------------------------------------------------------
;;;; r74-7 — rtrunc invariance: the cap keeps NEWEST, heads unmoved
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r74-7-rtrunc-invariance ()
  "Under `org-air-log-cap' 3 a heading with 6 state stamps truncates
\(`rtrunc' t) yet the Updated line shows the TRUE newest stamp — the
cap keeps the NEWEST entries, so the head is cap-invariant; no extra
marker appears on the line."
  (skip-unless (locate-library "org-air"))
  (org-air-r74--with-corpus
      '(("inbox.org" . "#+title: inbox\n\n* TODO Stampy :inbox:\n- State \"DONE\" from \"TODO\" [2026-06-06 Sat 09:00]\n- State \"TODO\" from \"DONE\" [2026-06-05 Fri 09:00]\n- State \"DONE\" from \"TODO\" [2026-06-04 Thu 09:00]\n- State \"TODO\" from \"DONE\" [2026-06-03 Wed 09:00]\n- State \"DONE\" from \"TODO\" [2026-06-02 Tue 09:00]\n- State \"TODO\" from \"DONE\" [2026-06-01 Mon 09:00]\n  body\n"))
    (let ((org-air-log-cap 3))
      (let* ((items (org-air-query-items))
             (item (org-air-r74--item "Stampy" items))
             (u (org-air-view--item-updated item))
             (line (org-air-r74--line item)))
        (should (org-air-item-rtrunc item))
        (should (= 3 (length (org-air-item-logs item))))
        (should (= (car u) (org-air-r74--epoch 2026 6 6 9 0)))
        (should (eq (cdr u) 'done))
        (should (equal "Updated 2026-06-06  (9d ago · done)" line))
        ;; no ⚠/~ marker for rtrunc — the heads are exact.
        (should-not (string-match-p "⚠" line))
        (should-not (string-match-p "~" line))))))

;;;; -------------------------------------------------------------------
;;;; r74-8 — the no-I/O law: slot-answered items make ZERO I/O calls
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r74-8-no-io-for-slot-answered-items ()
  "For a slot-answered item BOTH R74 helpers make ZERO
`file-attributes' / `find-file-noselect' / scan calls (scoped to the
helpers, not the whole fields pass — the Created line's own sanctioned
hydration is out of frame)."
  (skip-unless (locate-library "org-air"))
  (org-air-r74--with-corpus
      '(("inbox.org" . "#+title: inbox\n\n* TODO Notey :inbox:\n- Note taken on [2026-06-14 Sun 18:00] \\\\\n  the note text\n  body\n"))
    (let* ((items (org-air-query-items))
           (item (org-air-r74--item "Notey" items))
           (counts (vector 0 0 0)))
      (org-air-r74--spied counts
        (should (org-air-view--item-updated item))
        (should (org-air-view--item-updated-line
                 item "" org-air-test-now)))
      (should (equal [0 0 0] counts)))))

;;;; -------------------------------------------------------------------
;;;; r74-9 — R71 + R73 compose, merge-order-proof
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r74-9-r71-r73-compose ()
  "A note added through `org-air-inbox--apply-item-edits' (the R71
`:note' leg) + a rescan re-renders the Updated line to \"(today ·
note)\".  Base leg drives `org-air-view--inspector-update-now' directly
\(exists today); when `org-air-view--panes-resync-now' is `fboundp'
\(R73 landed) the SAME assert additionally rides the refresh tail with
a stationary cursor — merge-order-proof either way."
  (skip-unless (locate-library "org-air"))
  (org-air-r74--with-board
      '(("inbox.org" . "#+title: inbox\n\n* TODO Alpha task :inbox:\n  body\n"))
    (let ((item (org-air-r74--goto-row "Alpha task")))
      ;; the pre-note baseline: no note-labelled Updated line yet.
      (should-not (string-match-p "· note)" (org-air-r74--inspector-text)))
      (org-air-inbox--apply-item-edits item '(:note "compose note"))
      ;; the note is on disk with the frozen stamp.
      (with-temp-buffer
        (insert-file-contents (org-air-r74--file "inbox.org"))
        (should (string-match-p "- Note taken on \\[2026-06-15"
                                (buffer-string))))
      ;; rescan: the batch `org-air-refresh' is fully sync; its tail
      ;; carries the R73 resync when R73 has landed.
      (org-air-refresh)
      ;; stationary cursor: point still reads the SAME row.
      (let ((it (org-air-view--row-property 'org-air-item)))
        (should it)
        (should (equal "Alpha task" (org-air-item-title it))))
      ;; R73-landed leg: the refresh tail ALONE already re-rendered.
      (when (fboundp 'org-air-view--panes-resync-now)
        (should (string-match-p "Updated 2026-06-15  (today · note)"
                                (org-air-r74--inspector-text))))
      ;; base leg (merge-order-proof): drive the update-now directly.
      (org-air-view--inspector-update-now (current-buffer))
      (should (string-match-p "Updated 2026-06-15  (today · note)"
                              (org-air-r74--inspector-text))))))

;;;; -------------------------------------------------------------------
;;;; r74-10 — uniform degrade: 'file items + a deleted file
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r74-10-uniform-degrade ()
  "A `kind' `file' item (nil review slots) renders the fallback line
with the ~file marker (the mtime is item-precise there; the rule stays
uniform — no special case); a slotless item whose file is DELETED
renders no line and signals nothing."
  (skip-unless (locate-library "org-air"))
  (org-air-r74--with-corpus
      '(("inbox.org" . "#+title: inbox\n\n* TODO Anchor :inbox:\n  body\n")
        ("note.org" . "#+title: Loose note\nSome text, no heading.\n"))
    (let* ((items (org-air-query-items))
           (blob (seq-find (lambda (it) (eq (org-air-item-kind it) 'file))
                           items)))
      (should blob)
      (should (null (org-air-item-logs blob)))
      (should (null (org-air-item-clocks blob)))
      (should (null (org-air-item-created blob)))
      (set-file-times (org-air-r74--file "note.org")
                      (encode-time (list 0 0 12 10 6 2026 nil -1 nil)))
      (should (equal "Updated 2026-06-10  (5d ago · ~file)"
                     (org-air-r74--line blob)))
      ;; DELETED file: no line, no signal (the R53 degrade register).
      (let ((gone (org-air-item-create
                   :title "gone"
                   :file (org-air-r74--file "deleted.org"))))
        (should-not (org-air-r74--line gone)))
      ;; nil file: same degrade.
      (let ((nofile (org-air-item-create :title "nofile" :file nil)))
        (should-not (org-air-r74--line nofile))))))

(provide 'org-air-round74-test)
;;; org-air-round74-test.el ends here
