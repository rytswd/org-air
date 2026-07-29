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
  "A slotless heading falls back to the file mtime, marked \"~file\".
mtime 2026-06-10 renders \"Updated 2026-06-10  (5d ago · ~file)\", zero
scans, zero `find-file-noselect'.

R94 RE-BLESS — the rule the round CHANGED, encoded; not the assertion
relaxed.  R74 read the mtime with ONE bounded LIVE `file-attributes'
call.  Classify, meanwhile, reads the SCAN-time `:mtime' out of
`org-air-query-file-meta'.  Those are two different numbers the moment
the file changes without a rescan, and the R93 review measured the
drift: the rail said \"1d ago\" about the same heading the board had
bucketed at 165d.  Decision 13 — the number the inspector shows and the
number the bucket used cannot drift — held for the slot path and not for
this one.

So the ORDER of the setup moved, which is the whole point: the mtime is
now set BEFORE the scan, where a real user's file age lives, and the
assertions are strictly MORE than R74 made:

  1. the same rendered string, from the scan's own knowledge;
  2. ZERO live stats for a scanned item — the fallback is the same hash
     lookup classify makes, so the rail cannot cost I/O the bucket did
     not;
  3. NO DRIFT: the number the rail prints EQUALS
     `org-air-classify-quiet-floor-days', the number the Untracked
     bucket ranks and prints, and an on-disk edit with NO rescan moves
     NEITHER (they only ever learn together, on the next scan);
  4. the R74 bounded-stat rule survives exactly where it is still the
     only answer: an item the scan never saw (built outside it) still
     costs EXACTLY ONE `file-attributes' call."
  (skip-unless (locate-library "org-air"))
  (org-air-r74--with-corpus
      '(("inbox.org" . "#+title: inbox\n\n* Bare heading :inbox:\n  just text\n"))
    ;; R94: the file is already 5 days old when the scan runs — the
    ;; ordering a real corpus has, and the ordering that lets the rail
    ;; and the bucket read ONE fact.
    (set-file-times (org-air-r74--file "inbox.org")
                    (encode-time (list 0 0 12 10 6 2026 nil -1 nil)))
    (let* ((items (org-air-query-items))
           (item (org-air-r74--item "Bare heading" items))
           (counts (vector 0 0 0)))
      ;; the R61 slots really are empty (the harvest ran, found nothing).
      (should (null (org-air-item-logs item)))
      (should (null (org-air-item-clocks item)))
      (should (null (org-air-item-created item)))
      ;; ...and R94: no measured recency either, so this really is the
      ;; file path and not the slot path wearing its clothes.
      (should (null (org-air-classify-updated item)))
      (should (eq 'file (org-air-classify-updated-source item)))
      (let (line)
        (org-air-r74--spied counts
          (setq line (org-air-view--item-updated-line
                      item "" org-air-test-now)))
        (should (equal "Updated 2026-06-10  (5d ago · ~file)"
                       (substring-no-properties line)))
        (should (= 0 (aref counts 0)))   ; R94: ZERO live stats
        (should (= 0 (aref counts 1)))   ; zero scans
        (should (= 0 (aref counts 2))))  ; zero find-file-noselect
      ;; 3. the rail's number IS the bucket's number.
      (should (= 5 (org-air-classify-quiet-floor-days item org-air-test-now)))
      ;; ...and an on-disk edit with NO rescan moves neither of them.
      (set-file-times (org-air-r74--file "inbox.org")
                      (time-subtract org-air-test-now (days-to-time 1)))
      (should (= 5 (org-air-classify-quiet-floor-days item org-air-test-now)))
      (let ((counts2 (vector 0 0 0)) line2)
        (org-air-r74--spied counts2
          (setq line2 (org-air-view--item-updated-line
                       item "" org-air-test-now)))
        (should (equal "Updated 2026-06-10  (5d ago · ~file)"
                       (substring-no-properties line2)))
        (should (= 0 (aref counts2 0))))
      ;; 4. the R74 rule survives where it is still the only answer: an
      ;; item the scan never saw pays EXACTLY ONE bounded stat.
      (let* ((outsider (org-air-item-create
                        :title "Outside the scan"
                        :file (org-air-r74--file "inbox.org")
                        :marker (cons (org-air-r74--file "inbox.org") 1)
                        :kind 'heading :todo "TODO"))
             (counts3 (vector 0 0 0))
             line3)
        ;; no scan entry at all: the floor cannot answer for it.
        (org-air-query-teardown)
        (clrhash org-air-query--file-meta)
        (should (null (org-air-classify-updated-floor outsider)))
        (should (null (org-air-classify-updated-source outsider)))
        (set-file-times (org-air-r74--file "inbox.org")
                        (encode-time (list 0 0 12 10 6 2026 nil -1 nil)))
        (org-air-r74--spied counts3
          (setq line3 (org-air-view--item-updated-line
                       outsider "" org-air-test-now)))
        (should (equal "Updated 2026-06-10  (5d ago · ~file)"
                       (substring-no-properties line3)))
        (should (= 1 (aref counts3 0)))   ; exactly ONE stat
        (should (= 0 (aref counts3 1)))
        (should (= 0 (aref counts3 2)))))))

;;;; -------------------------------------------------------------------
;;;; r74-6 — the future clamp: the golden-determinism lemma
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r74-6-future-clamp-golden-lemma ()
  "The Updated line is derived from FIXTURE BYTES, never the wall clock.
The golden-determinism lemma, in two legs.

Leg 1 (unchanged): a heading with NO recency of its own -- no LOGBOOK,
no clock, no `:CREATED:', no body stamp -- and a file mtime that
post-dates the frozen NOW renders NO Updated line and no error.  The
FUTURE CLAMP is what keeps a scratch copy's mtime out of a golden.

Leg 2 (R93 re-bless).  This leg used to assert that the x50 inspector
goldens carry NO `Updated' line at all, because the inspected row --
\"Quick note about org-air idea\" -- was slotless and its only candidate
was that clamped-away mtime.  R93 gave the scan an `updated' slot (the
newest non-future INACTIVE stamp in the heading's own body), and that
fixture heading carries `[2026-06-14 Sun]' in its body, so the line now
renders -- from the fixture's own BYTES, at a fixed date, labelled
`stamp'.  The test's PURPOSE is therefore strengthened, not weakened:
the golden line is now derived from text under version control instead
of being absent because a wall-clock value had to be suppressed.  Both
halves are pinned: the deterministic line for the stamped row, and the
surviving clamp for a slotless one (r74-5's `~file' leg still covers
the coarse fallback itself)."
  (skip-unless (locate-library "org-air"))
  (org-air-r74--with-corpus
      '(("inbox.org" . "#+title: inbox\n\n* Bare heading :inbox:\n  just text\n"))
    (let* ((items (org-air-query-items))
           (item (org-air-r74--item "Bare heading" items)))
      (should-not (org-air-item-updated item))
      (set-file-times (org-air-r74--file "inbox.org")
                      (encode-time (list 0 0 12 20 6 2026 nil -1 nil)))
      (should-not (org-air-view--item-updated-line
                   item "" org-air-test-now))))
  ;; the golden leg: the x50 mockup configuration -- the inspected
  ;; "Quick note about org-air idea" carries a body inactive stamp, so
  ;; the board shows exactly ONE Updated line and its bytes are fixed.
  (org-air-viewport-test-with-dashboard (cons 100 50)
    (should (string-match-p "Quick note" (buffer-string)))
    (let ((text (buffer-string)))
      (should (string-match-p "Updated 2026-06-14  (1d" text))
      (should (= 1 (cl-count-if (lambda (line)
                                  (string-match-p "Updated " line))
                                (split-string text "\n"))))))
  ;; and the fixture heading really is the source of that date.
  (org-air-test-with-fixtures
    (let* ((items (org-air-query-items))
           (note (org-air-test-find-item "Quick note about org-air idea" items)))
      (should note)
      (should (equal '(2026 6 14)
                     (let ((d (decode-time (org-air-item-updated note))))
                       (list (decoded-time-year d) (decoded-time-month d)
                             (decoded-time-day d)))))
      (should (eq 'stamp (cdr (org-air-view--item-updated note)))))))

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
renders no line and signals nothing.

R94 RE-BLESS, same shape as `org-air-r74-5-fallback-one-bounded-stat':
the mtime is set BEFORE the scan, because the fallback now reads the
SCAN's `:mtime' (`org-air-classify-updated-floor') rather than stat'ing
the file live.  The uniformity claim is what this test is FOR, so it is
now asserted where it bites hardest: a `kind' `file' blob — the one item
kind whose mtime really is item-precise — obeys the SAME no-drift rule
as a heading.  Its rail number equals `org-air-classify-quiet-floor-days'
and an on-disk edit with no rescan moves neither.  No special case, in
either direction."
  (skip-unless (locate-library "org-air"))
  (org-air-r74--with-corpus
      '(("inbox.org" . "#+title: inbox\n\n* TODO Anchor :inbox:\n  body\n")
        ("note.org" . "#+title: Loose note\nSome text, no heading.\n"))
    (set-file-times (org-air-r74--file "note.org")
                    (encode-time (list 0 0 12 10 6 2026 nil -1 nil)))
    (let* ((items (org-air-query-items))
           (blob (seq-find (lambda (it) (eq (org-air-item-kind it) 'file))
                           items)))
      (should blob)
      (should (null (org-air-item-logs blob)))
      (should (null (org-air-item-clocks blob)))
      (should (null (org-air-item-created blob)))
      (should (equal "Updated 2026-06-10  (5d ago · ~file)"
                     (org-air-r74--line blob)))
      ;; R94 uniformity: the rail's number IS the bucket's bound, for a
      ;; `kind' `file' item exactly as for a heading...
      (should (eq 'file (org-air-classify-updated-source blob)))
      (should (= 5 (org-air-classify-quiet-floor-days blob org-air-test-now)))
      ;; ...and an on-disk edit with NO rescan moves neither number.
      (set-file-times (org-air-r74--file "note.org")
                      (time-subtract org-air-test-now (days-to-time 1)))
      (should (= 5 (org-air-classify-quiet-floor-days blob org-air-test-now)))
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

;;;; -------------------------------------------------------------------
;;;; r74-11 — AUDIT GAP: the header/banner ✕ clear glyph is GONE
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r74-11-header-banner-clear-glyph-gone ()
  "The R69-2 sibling site: the banner filter segment ends at the token
join — NO trailing ✕ clear glyph (it carried no keymap/button action).
With no scope and the default sort the filter segment is the LAST
status segment, so the banner line must end EXACTLY with the joined
tokens; a restored `(org-air-view--glyph \='clear)' would trail as
\" ✕\" (GUI tier) or \" x\" (batch tier) and break the suffix — the pin
is glyph-tier-immune, like r69-2's exact-line idiom (a bare \"no ✕\"
grep would be VACUOUS in batch, where the tier renders `x').  The rail
chips line (R69-2) is asserted clean in the same shape — BOTH surfaces
— and the `\\ clears' hint stays (the teaching surface).  Revert-RED:
verified against the glyph-restored banner (the pre-R74 site)."
  (skip-unless (locate-library "org-air"))
  (org-air-r74--with-board
      '(("inbox.org" . "#+title: inbox\n\n* TODO Tagged task :work:home:\n  body\n"))
    (let ((org-air-filter-match 'all))
      (setq org-air-view--tag-filter '("#work" "#home"))
      (org-air-view--render-current)
      (let* ((banner (buffer-substring-no-properties
                      (point-min) (progn (goto-char (point-min))
                                         (line-end-position))))
             (banner (string-trim-right banner)))
        ;; the segment renders at all (anti-vacuity: 160w never sheds it) …
        (should (string-match-p "#work AND #home" banner))
        ;; … and the line ENDS at the token join: no trailing clear glyph
        ;; in EITHER tier, no trailing spacer.
        (should (string-suffix-p "#work AND #home" banner))
        ;; belt: neither tier's glyph trails the join anywhere on the line.
        (should-not (string-match-p "#home [✕x×]" banner)))
      ;; the teaching surface survives: the rail's hint names the verb.
      (should (string-match-p (regexp-quote "\\ clears") (buffer-string))))
    ;; the rail chips line (the R69-2 site) is clean in the same shape.
    (with-temp-buffer
      (let ((org-air-show-rail-filters t)
            (org-air-view--tag-filter '("#work" "#home"))
            (org-air-view--scope nil)
            (org-air-filter-match 'all))
        (org-air-view--insert-rail-filters 32)
        (let* ((lines (split-string (buffer-substring-no-properties
                                     (point-min) (point-max))
                       "\n"))
               (chips (string-trim-right (nth 1 lines))))
          (should (string-suffix-p "#work AND #home" chips))
          (should (string-match-p "M-/ toggles" (nth 2 lines))))))))

;;;; -------------------------------------------------------------------
;;;; r74-12 — AUDIT GAP: the SLOT path is NOT clamped (Decision 2)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r74-12-slot-path-not-clamped ()
  "A forged FUTURE logbook stamp renders HONESTLY as \"(in Nd · note)\"
— the Decision 3 future-clamp applies ONLY to the machine-derived mtime
fallback (a machine signal with a machine failure mode); the slot path
trusts the drawer.  Pinned so a well-meaning `clamp everything' cleanup
goes RED here instead of silently hiding user-written data."
  (skip-unless (locate-library "org-air"))
  (org-air-r74--with-corpus
      '(("inbox.org" . "#+title: inbox\n\n* TODO Forged :inbox:\n- Note taken on [2026-06-20 Sat 09:00] \\\\\n  a stamp after the frozen NOW\n  body\n"))
    (let* ((items (org-air-query-items))
           (item (org-air-r74--item "Forged" items))
           (u (org-air-view--item-updated item)))
      (should u)
      (should (= (car u) (org-air-r74--epoch 2026 6 20 9 0)))
      (should (eq (cdr u) 'note))
      ;; frozen NOW is Mon 2026-06-15 → the stamp is 5d in the future.
      (should (equal "Updated 2026-06-20  (in 5d · note)"
                     (org-air-r74--line item))))))

(provide 'org-air-round74-test)
;;; org-air-round74-test.el ends here
