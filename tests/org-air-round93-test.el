;;; org-air-round93-test.el --- R93: recency x priority attention -*- lexical-binding: t; -*-

;;; Commentary:
;; Permanent coverage for the R93 planning surface — the round that
;; deleted "a board item with no date needs attention" and rebuilt Needs
;; attention as an AGING rule.
;;
;; The user's report, verbatim, is the reason this file exists:
;;
;;   "The current logic would mean everything needs to be scheduled,
;;    otherwise it would be part of Needs attention — but that's not
;;    practical; everything would be sent to backlog because that would
;;    be the easiest to stop seeing them.  Instead, Needs attention
;;    should show items based on the last update date and priority — if
;;    the priority is highest it should show up all the time, if lower it
;;    should come up after some time of no update.  Overdue may be better
;;    handled separately.  Not sure what stale is about."
;;
;; The invariants the product now rests on, one ERT each:
;;
;;   r93-1   the threshold TABLE is the user's ruling (#A 3, #B 7, #C 14,
;;           #D/#E/no-cookie 30) and an unlisted priority falls back.
;;   r93-2   the aging predicate at every BOUNDARY, per priority letter:
;;           the day before does not surface, the day OF does, the day
;;           after does — as a predicate AND as a bucket.
;;   r93-3   NO threshold in the default table surfaces an item org-air
;;           cannot date; threshold 0 is the kept, opt-in escape hatch.
;;   r93-4   recency EXTRACTION: LOGBOOK state changes, LOGBOOK notes,
;;           CLOCK-out ends, `CLOSED:', `:CREATED:' and free-form body
;;           stamps all count, and the NEWEST one wins.
;;   r93-5   ACTIVE <timestamps> NEVER count — a plan is not an update.
;;   r93-6   a FUTURE inactive stamp never counts.
;;   r93-7   OWN BODY only: a child's stamp must not refresh its parent.
;;   r93-8   the file-mtime FLOOR: consulted only when a heading has no
;;           stamp at all, never a file read at classify time, and it
;;           self-heals on the first real stamp.
;;   r93-9   LIVE retuning: `setq'-ing `org-air-attention-days' changes
;;           the next repaint, with zero rescans.
;;   r93-10  Overdue is its own bucket, its own section and its own
;;           predicate, independent of Needs attention in both
;;           directions.
;;   r93-11  `is:overdue' / `is:attention' agree with their sections by
;;           construction; `is:stale' parses as the alias and completion
;;           no longer offers it.
;;   r93-12  the section ORDER is exactly Inbox, Overdue, Upcoming, High
;;           priority, Needs attention, with Notes conditional and
;;           Backlog last.
;;   r93-13  cache v6 -> v7: a v6 record is a clean cold miss, never a
;;           silent hydration with a missing `updated' slot.
;;   r93-14  the row/inspector REASON labels quote the same two public
;;           helpers the bucket used, so they cannot drift.
;;   r93-15  the overlap is EARNED, not granted: with `#A' at 3 a fresh
;;           High-priority row is NOT a Needs-attention row, and the
;;           superset returns exactly when `(?A . 0)' is set back.
;;   r93-16  the INVERTED audit tripwire: Overdue renders worst-first and
;;           Needs attention longest-silent-first, on the real fixture.
;;   r93-17  the `#A' = 3 default AT ITS BOUNDARY (day 2 / 3 / 4), and
;;           `(?A . 0)' restoring unconditional surfacing, unknown age
;;           included — the mechanism outliving its default.
;;   r93-18  the unknown-age ruling, SWEPT: no positive threshold on any
;;           priority ever surfaces an undatable item, and High priority
;;           still shows an undatable `#A'.
;;   r93-19  the per-section SORT table: Overdue worst-first, Needs
;;           attention longest-silent-first (unknown last, stable
;;           tiebreak), Upcoming soonest-first, the rest query order.
;;   r93-20  a CAPPED section shows the WORST N, never N arbitrary rows
;;           (end-to-end, on a corpus written in exactly the wrong order).
;;   r93-21  the sort key is the number the user can SEE — the order of
;;           the RENDERED date cells IS the order of the rows.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)

(when (locate-library "org-air")
  (require 'org-air))

;;;; -------------------------------------------------------------------
;;;; Scaffolding
;;;; -------------------------------------------------------------------

(defvar org-air-r93--dir nil
  "Temp corpus directory of the running round-93 test.")

(defconst org-air-r93--file "/tmp/org-air-r93-synthetic.org"
  "A FICTION: a file with no entry in the scan's file-meta table.
An item carrying it has no `updated' slot AND no mtime floor, which is
the R93 definition of an UNKNOWN age.")

(defun org-air-r93--epoch (days)
  "Return the epoch integer DAYS from the frozen now (negative = past)."
  (floor (float-time (time-add org-air-test-now (days-to-time days)))))

(defun org-air-r93--cookie (char)
  "Return the Org priority cookie VALUE for priority letter CHAR, or nil."
  (and char (* 1000 (- org-priority-lowest char))))

(cl-defun org-air-r93--item (&key priority updated scheduled deadline
                                  (title "R93 probe") file todo tags)
  "Build a cache-hydrated-shape `org-air-item' for the aging rules.
PRIORITY is a priority LETTER (?A ...); UPDATED / SCHEDULED / DEADLINE
are day offsets from the frozen now.  FILE defaults to
`org-air-r93--file' (see its docstring: an UNKNOWN age)."
  (org-air-item-create
   :title title
   :tags tags
   :file (or file org-air-r93--file)
   :marker (cons (or file org-air-r93--file) 1)
   :kind 'heading
   :todo (or todo "TODO")
   :priority (org-air-r93--cookie priority)
   :updated (and updated (org-air-r93--epoch updated))
   :scheduled (and scheduled
                   (org-timestamp-from-string
                    (format-time-string
                     "<%Y-%m-%d %a>"
                     (time-add org-air-test-now (days-to-time scheduled)))))
   :deadline (and deadline
                  (org-timestamp-from-string
                   (format-time-string
                    "<%Y-%m-%d %a>"
                    (time-add org-air-test-now (days-to-time deadline)))))))

(defmacro org-air-r93--with-corpus (specs &rest body)
  "Write SPECS into a fresh temp corpus and run BODY against it."
  (declare (indent 1) (debug t))
  `(let ((org-air-r93--dir (make-temp-file "org-air-r93-" t)))
     (unwind-protect
         (progn
           (when (fboundp 'org-air-query-teardown)
             (org-air-query-teardown)
             (clrhash org-air-query--file-meta)
             (clrhash org-air-query--visits)
             (clrhash org-air-query--denote-id-index))
           (pcase-dolist (`(,name . ,content) ,specs)
             (let ((path (expand-file-name name org-air-r93--dir))
                   (coding-system-for-write 'utf-8-unix)
                   (file-name-handler-alist nil))
               (make-directory (file-name-directory path) t)
               (write-region (or content "") nil path nil 'silent)))
           (let ((org-air-files (list org-air-r93--dir))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-r93--dir))
                 (org-air-cache-file
                  (expand-file-name ".cache/board.eld" org-air-r93--dir))
                 (org-air-view-buffer-name "*org-air-r93*")
                 (org-air-plain-heading-type 'task)
                 (create-lockfiles nil)
                 (inhibit-message t))
             ,@body))
       (when (fboundp 'org-air-query-teardown) (org-air-query-teardown))
       (let ((kill-buffer-query-functions nil))
         (when (get-buffer "*org-air-r93*") (kill-buffer "*org-air-r93*"))
         (dolist (buf (buffer-list))
           (let ((fn (buffer-file-name buf)))
             (when (and fn (string-prefix-p org-air-r93--dir fn))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r93--dir t))))

(defun org-air-r93--stamp (days)
  "Return an INACTIVE Org timestamp DAYS from the frozen now (R93 FIX-2/3).
Written into a heading's OWN body it is that heading's recency clock,
which is what `org-air-classify-quiet-days' measures."
  (format-time-string "[%Y-%m-%d %a 09:00]"
                      (time-add org-air-test-now (days-to-time days))))

(defun org-air-r93--date (days)
  "Return an ACTIVE Org date DAYS from the frozen now (a PLAN, not an update)."
  (format-time-string "<%Y-%m-%d %a>"
                      (time-add org-air-test-now (days-to-time days))))

(defmacro org-air-r93--render-board (size &rest body)
  "Render the REAL board over the bound corpus at SIZE; run BODY in its buffer.
SIZE is (WIDTH . HEIGHT).  Frozen clock (`org-air-test-now'), the
anti-tautology render guards active, and the board buffer killed
afterwards so a warm in-buffer item cache never leaks between renders."
  (declare (indent 1) (debug t))
  `(org-air-viewport-test--with-frozen-now
     (unwind-protect
         (org-air-viewport-test--with-render-guards
           (let ((org-air-view-width (car ,size))
                 (org-air-view-height (cdr ,size)))
             (org-air)
             (with-current-buffer org-air-view-buffer-name
               ,@body)))
       (when (get-buffer org-air-view-buffer-name)
         (let ((kill-buffer-query-functions nil))
           (kill-buffer org-air-view-buffer-name))))))

(defun org-air-r93--cell-number (span)
  "Return the INTEGER a rendered row SPAN prints in its date cell, or nil.
`OVERDUE 7d' => 7, `273d quiet' => 273.  Deliberately reads the PAINTED
text and nothing else: it is the number the user can SEE, which is the
only key a per-section sort is allowed to use (R93 FIX-2)."
  (cond ((string-match "OVERDUE \\([0-9]+\\)d" span)
         (string-to-number (match-string 1 span)))
        ((string-match "\\([0-9]+\\)d quiet" span)
         (string-to-number (match-string 1 span)))))

(defun org-air-r93--rendered-sections ()
  "Return an alist (BUCKET . ROWS) for the board in the current buffer.
ROWS are in RENDER order; each is a list (TITLE NUMBER ITEM) where
NUMBER is `org-air-r93--cell-number' of the row's own painted span (nil
when that cell prints no number).  Property-driven — `org-air-section'
opens a section, each `org-air-item' run inside it is one row — so
chrome text can never be mistaken for a row."
  (let ((pos (point-min)) (bucket nil) (out ()))
    (while (< pos (point-max))
      (let ((sec (get-text-property pos 'org-air-section))
            (item (get-text-property pos 'org-air-item)))
        (cond
         (sec
          (setq bucket sec)
          (unless (assq bucket out) (push (cons bucket nil) out))
          (setq pos (or (next-single-property-change pos 'org-air-section)
                        (point-max))))
         (item
          (let* ((end (or (next-single-property-change pos 'org-air-item)
                          (point-max)))
                 (span (buffer-substring-no-properties pos end)))
            (push (list (org-air-item-title item)
                        (org-air-r93--cell-number span)
                        item)
                  (cdr (assq bucket out)))
            (setq pos end)))
         (t (setq pos (1+ pos))))))
    (mapcar (lambda (entry) (cons (car entry) (nreverse (cdr entry))))
            (nreverse out))))

(defun org-air-r93--fold-counts ()
  "Return an alist (BUCKET . N) read off the rendered \"and N more\" rows."
  (let ((pos (point-min)) (out ()))
    (while (setq pos (text-property-not-all pos (point-max)
                                            'org-air-more-row nil))
      (let* ((bucket (get-text-property pos 'org-air-more-row))
             (end (or (next-single-property-change pos 'org-air-more-row)
                      (point-max)))
             (text (buffer-substring-no-properties pos end)))
        (when (string-match "and \\([0-9]+\\) more" text)
          (push (cons bucket (string-to-number (match-string 1 text))) out))
        (setq pos end)))
    (nreverse out)))

(defun org-air-r93--path (name)
  "Return corpus file NAME's absolute path."
  (expand-file-name name org-air-r93--dir))

(defun org-air-r93--scanned (title &optional items)
  "Return the scanned item whose title contains TITLE."
  (let ((item (org-air-test-find-item title (or items (org-air-query-items)))))
    (should item)
    item))

(defun org-air-r93--updated-day (item)
  "Return ITEM's `updated' slot as a (YEAR MONTH DAY) list, or nil."
  (when-let* ((epoch (org-air-item-updated item)))
    (let ((d (decode-time epoch)))
      (list (decoded-time-year d) (decoded-time-month d)
            (decoded-time-day d)))))

;;;; -------------------------------------------------------------------
;;;; r93-1 — the threshold table IS the user's ruling
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r93-1-threshold-table-is-the-user-ruling ()
  "`org-air-attention-days' says exactly what the user asked for.
\"If the priority is highest it should show up all the time, if lower it
should come up after some time of no update\": `#A' the SHORTEST
patience, `#B' a week, `#C' a fortnight, `#D' / `#E' / no cookie a
month.  `#D' and `#E' are listed EXPLICITLY even though they equal the
nil fallback, so a user reading the alist sees the whole table instead
of inferring it.

R93 FIX-3 RE-BLESS — a USER DECISION, encoded, not a test made to pass.
`#A' is *3*, not 0.  \"Show up all the time\" is the job of the HIGH
PRIORITY section, and High priority IS the `#A' set; at 0 every `#A'
row was therefore printed twice on every board and its reason cell had
to read the excuse `always' instead of a number.  Each section now owns
exactly one job — High priority: \"always visible\"; Needs attention:
\"has gone quiet\" — and `#A' keeps the sharpest patience in the table,
which is the half of the user's sentence THIS alist is responsible for.

Pinned as a TABLE and as a SHAPE, so a later edit can neither make `#A'
laxer than `#B' nor slide a DEFAULT back onto the unconditional
(threshold 0) arm without reddening this test."
  (skip-unless (locate-library "org-air"))
  (let ((org-priority-lowest ?E))       ; so ?D / ?E are real cookies
    (pcase-dolist (`(,letter . ,days) '((?A . 3) (?B . 7) (?C . 14)
                                        (?D . 30) (?E . 30) (nil . 30)))
      (ert-info ((format "priority %s => %d days"
                         (if letter (string letter) "none") days))
        (should (equal days (cdr (assq letter org-air-attention-days))))
        (should (= days (org-air-classify-attention-threshold
                         (org-air-r93--item :priority letter))))))
    ;; The SHAPE of the table, independent of the exact numbers:
    ;; patience never SHRINKS as priority drops, and every default is
    ;; POSITIVE — no priority ships on the unconditional arm any more.
    (let* ((letters '(?A ?B ?C ?D ?E nil))
           (thresholds (mapcar (lambda (l)
                                 (org-air-classify-attention-threshold
                                  (org-air-r93--item :priority l)))
                               letters)))
      (should (equal '(3 7 14 30 30 30) thresholds))
      (should (equal thresholds (sort (copy-sequence thresholds) #'<)))
      (dolist (days thresholds)
        (should (> days 0)))
      ;; `#A' is strictly the most impatient row in the table.
      (should (< (car thresholds) (apply #'min (cdr thresholds))))))
  ;; A priority the alist does not list falls back to the nil entry…
  (let ((org-air-attention-days '((?A . 3) (nil . 30))))
    (should (= 30 (org-air-classify-attention-threshold
                   (org-air-r93--item :priority ?C)))))
  ;; …and with no nil entry either, to the documented default.
  (let ((org-air-attention-days '((?A . 3)))
        (org-air-attention-default-days 30))
    (should (= 30 (org-air-classify-attention-threshold
                   (org-air-r93--item :priority ?C)))))
  ;; Never negative: 0 means "always", and nothing means "less than 0".
  (let ((org-air-attention-days '((nil . -5))))
    (should (= 0 (org-air-classify-attention-threshold
                  (org-air-r93--item))))))

;;;; -------------------------------------------------------------------
;;;; r93-2 — the boundary, per priority letter
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r93-2-aging-boundary-per-priority ()
  "The aging rule at every THRESHOLD BOUNDARY, for every priority letter.
The day BEFORE the threshold does not surface; the day OF it does (the
rule is `>=', not `>'); the day after still does.  Asserted twice for
each case — on the predicate `org-air-classify--attention-p' and on the
BUCKET `org-air-classify-item' — so a rule that drifts between the
section body and its token cannot pass.  Includes the no-cookie row,
which is the case most users live in."
  (skip-unless (locate-library "org-air"))
  (let ((org-priority-lowest ?E))
    (pcase-dolist (`(,letter . ,threshold) '((?B . 7) (?C . 14) (?D . 30)
                                             (?E . 30) (nil . 30)))
      (pcase-dolist (`(,offset . ,expected) `((,(1- threshold) . nil)
                                              (,threshold . t)
                                              (,(1+ threshold) . t)))
        (let* ((item (org-air-r93--item :priority letter :updated (- offset)))
               (buckets (org-air-classify-item item org-air-test-now)))
          (ert-info ((format "priority %s quiet %dd of %dd => %S"
                             (if letter (string letter) "none")
                             offset threshold buckets))
            (should (= offset (org-air-classify-quiet-days
                               item org-air-test-now)))
            (should (eq expected
                        (and (org-air-classify--attention-p
                              item org-air-test-now)
                             t)))
            (should (eq expected (and (memq 'attention buckets) t)))))))))

;;;; -------------------------------------------------------------------
;;;; r93-3 — an unknown age nags nobody; threshold 0 is the opt-in
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r93-3-hipri-always-unknown-age-never ()
  "NO default threshold surfaces an item org-air cannot date; 0 still does.
R93 FIX-3 RE-BLESS — the ruling encoded, not the assertion relaxed.
Before FIX-3 `#A' sat on threshold 0, so it was the ONE priority that
surfaced with an age org-air simply does not know (an item built
outside the scan: no `updated' slot and no file in the scan's meta
table).  FIX-3 moved `#A' to 3, so `#A' now answers like every other
priority — and that is the rule the round ALREADY stated reaching one
more case, not a new rule:

  a POSITIVE threshold is a claim about ELAPSED TIME, and org-air will
  not assert elapsed time it never measured.

Both alternative readings stay rejected: \"unknown means fresh\" hides
real work (the user's original complaint); \"unknown means infinitely
old\" nags on day one of every fresh corpus.

Nothing is LOST for the `#A': High priority is the section that never
needed a clock and it still shows the row — asserted here, so the
refusal above can never be misread as the item disappearing.

And the threshold-0 MECHANISM outlived its default: it is a default
nowhere now, it is an OPT-IN, and it still means UNCONDITIONAL (unknown
age included) for whichever priority a user points it at."
  (skip-unless (locate-library "org-air"))
  (let ((org-priority-lowest ?E))
    ;; EVERY priority in the DEFAULT table refuses an unknown age — `#A'
    ;; included, since FIX-3 gave it a positive threshold.
    (dolist (letter '(?A ?B ?C ?D ?E nil))
      (let ((item (org-air-r93--item :priority letter)))
        (ert-info ((format "priority %s with an unknown age"
                           (if letter (string letter) "none")))
          ;; the age really is unknown — nothing invented one.
          (should-not (org-air-classify-updated item))
          (should-not (org-air-classify-quiet-days item org-air-test-now))
          ;; …and the threshold really is positive (why it refuses).
          (should (> (org-air-classify-attention-threshold item) 0))
          (should-not (org-air-classify--attention-p item org-air-test-now))
          (should-not (memq 'attention
                            (org-air-classify-item item org-air-test-now))))))
    ;; The undatable `#A' is NOT lost: High priority still shows it, and
    ;; that is the whole of its bucket list.
    (should (equal '(high-priority)
                   (org-air-classify-item (org-air-r93--item :priority ?A)
                                          org-air-test-now)))
    ;; Give that same `#A' a MEASURED age at its threshold and it earns
    ;; the second row: membership is earned by silence, never granted by
    ;; the cookie.
    (should (equal '(high-priority attention)
                   (org-air-classify-item
                    (org-air-r93--item :priority ?A :updated -3)
                    org-air-test-now)))
    ;; THRESHOLD 0 — kept, documented, opt-in — restores the
    ;; unconditional arm for whichever priority a user points it at,
    ;; unknown age included.  The mechanism EXERCISED, not asserted.
    (dolist (letter '(?A ?B nil))
      (let ((org-air-attention-days (list (cons letter 0) (cons nil 30))))
        (dolist (updated '(0 -1 -1000 nil))
          (let ((item (org-air-r93--item :priority letter :updated updated)))
            (ert-info ((format "opt-in (%s . 0) updated=%S"
                               (if letter (string letter) "nil") updated))
              (should (= 0 (org-air-classify-attention-threshold item)))
              (should (org-air-classify--attention-p item org-air-test-now))
              (should (memq 'attention
                            (org-air-classify-item item org-air-test-now))))))))))

;;;; -------------------------------------------------------------------
;;;; r93-4 — every shape Org writes when something HAPPENED
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r93-4-recency-reads-every-happened-shape ()
  "LOGBOOK states, notes, clock-outs, CLOSED, CREATED and body stamps all count.
One heading per shape, each carrying exactly ONE inactive stamp, so the
`updated' slot can only have come from that shape.  Plus the composite
case: with several shapes in one entry the NEWEST wins, whichever shape
it happens to be."
  (skip-unless (locate-library "org-air"))
  (org-air-r93--with-corpus
      '(("shapes.org" . "\
* TODO State change
:LOGBOOK:
- State \"DONE\" from \"TODO\" [2026-06-05 Fri 09:00]
:END:
* TODO Logbook note
:LOGBOOK:
- Note taken on [2026-06-04 Thu 09:00] \\\\
  the note body
:END:
* TODO Clock out
:LOGBOOK:
CLOCK: [2026-06-03 Wed 10:00]--[2026-06-03 Wed 11:00] =>  1:00
:END:
* DONE Completion stamp
CLOSED: [2026-06-02 Tue 09:00]
* TODO Capture stamp
:PROPERTIES:
:CREATED: [2026-06-01 Mon 09:00]
:END:
* TODO Free body stamp
Some prose and a stamp the user typed.
[2026-05-31 Sun 09:00]
* TODO Newest of several
:PROPERTIES:
:CREATED: [2026-01-01 Thu 09:00]
:END:
:LOGBOOK:
- State \"TODO\" from \"WAIT\" [2026-06-06 Sat 09:00]
- Note taken on [2026-03-03 Tue 09:00]
:END:
CLOSED: [2026-02-02 Mon 09:00]
An older body stamp [2026-04-04 Sat 09:00] too.
")
        ("inbox.org" . "#+title: inbox\n"))
    (let ((items (org-air-query-items)))
      (pcase-dolist (`(,title . ,day)
                     '(("State change"     . (2026 6 5))
                       ("Logbook note"     . (2026 6 4))
                       ("Clock out"        . (2026 6 3))
                       ("Completion stamp" . (2026 6 2))
                       ("Capture stamp"    . (2026 6 1))
                       ("Free body stamp"  . (2026 5 31))
                       ;; the newest of six stamps, of four shapes.
                       ("Newest of several" . (2026 6 6))))
        (let ((item (org-air-r93--scanned title items)))
          (ert-info ((format "%s => %S" title (org-air-r93--updated-day item)))
            (should (equal day (org-air-r93--updated-day item)))))))))

;;;; -------------------------------------------------------------------
;;;; r93-5 — a PLAN is not an update
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r93-5-active-timestamps-never-count ()
  "ACTIVE <timestamps> never move the recency clock, in any position.
SCHEDULED, DEADLINE and a bare active stamp in the body are all
invisible to the probe (`org-ts-regexp-inactive' does not match them at
all), so each heading below has NO history of its own.  That is the
mechanical guarantee behind the R93 ruling: nothing has to be scheduled
to stay off Needs attention, and scheduling something buys no exemption
either \u2014 the paired heading, with the SAME plan dates plus one inactive
stamp, ages exactly as if the plan were not there."
  (skip-unless (locate-library "org-air"))
  (org-air-r93--with-corpus
      '(("plans.org" . "\
* TODO Only scheduled
SCHEDULED: <2026-06-20 Sat>
* TODO Only deadline
DEADLINE: <2026-06-21 Sun>
* TODO Only a bare active stamp
Meeting <2026-06-19 Fri 10:00> in the body.
* TODO Planned and quiet
SCHEDULED: <2026-06-20 Sat>
DEADLINE: <2026-06-21 Sun>
A plan, plus one thing that actually happened.
[2026-04-16 Thu 09:00]
")
        ("inbox.org" . "#+title: inbox\n"))
    (let ((items (org-air-query-items)))
      (dolist (title '("Only scheduled" "Only deadline"
                       "Only a bare active stamp"))
        (let ((item (org-air-r93--scanned title items)))
          (ert-info ((format "%s" title))
            ;; the plan exists...
            (should (or (org-air-item-scheduled item)
                        (org-air-item-deadline item)
                        (org-air-item-active-ts item)))
            ;; ...and the recency clock has never heard of it.
            (should-not (org-air-item-updated item)))))
      ;; The paired heading: same plan, one real update, aged by THAT.
      (let ((quiet (org-air-r93--scanned "Planned and quiet" items)))
        (should (org-air-item-scheduled quiet))
        (should (equal '(2026 4 16) (org-air-r93--updated-day quiet)))
        (should (= 60 (org-air-classify-quiet-days quiet org-air-test-now)))
        (should (memq 'attention
                      (org-air-classify-item quiet org-air-test-now)))))))

;;;; -------------------------------------------------------------------
;;;; r93-6 — a note ABOUT the future is not an update
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r93-6-future-inactive-stamp-never-counts ()
  "An inactive stamp dated after the scan day is skipped, never taken as new.
Letting one win would silence the heading until that date arrived \u2014 the
one shape of \"stamp\" that is a note about the future rather than a
record of the past.  Both halves are pinned: a future stamp ALONE leaves
the heading historyless, and a future stamp beside an old one leaves the
OLD one as the clock."
  (skip-unless (locate-library "org-air"))
  (let* ((future (format-time-string
                  "[%Y-%m-%d %a 09:00]"
                  (time-add (current-time) (days-to-time 30))))
         (specs (list (cons "future.org"
                            (concat "* TODO Only a future stamp\n" future "\n"
                                    "* TODO Future beside an old one\n"
                                    "[2026-04-16 Thu 09:00]\n" future "\n"))
                      (cons "inbox.org" "#+title: inbox\n"))))
    (org-air-r93--with-corpus specs
      (let ((items (org-air-query-items)))
        (should-not (org-air-item-updated
                     (org-air-r93--scanned "Only a future stamp" items)))
        (should (equal '(2026 4 16)
                       (org-air-r93--updated-day
                        (org-air-r93--scanned "Future beside an old one"
                                              items))))))))

;;;; -------------------------------------------------------------------
;;;; r93-7 — the heading's OWN body, never its children's
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r93-7-own-body-only-child-never-refreshes-parent ()
  "A child's stamp belongs to the child's row and never refreshes its parent.
Crediting a subtree's newest stamp to its parent would let one busy
child silence a whole project heading forever, which is the R61 rollup
reasoning applied to the recency clock.  The corpus makes the trap live:
the parent's own body holds an OLD stamp, the child's holds a fresh one,
and the parent must still age on its own."
  (skip-unless (locate-library "org-air"))
  (org-air-r93--with-corpus
      '(("tree.org" . "\
* TODO Quiet parent
The parent's own body, last touched in April.
[2026-04-16 Thu 09:00]
** TODO Busy child
:LOGBOOK:
- State \"TODO\" from \"WAIT\" [2026-06-14 Sun 09:00]
:END:
* TODO Historyless parent
** TODO Busy child two
:LOGBOOK:
- State \"TODO\" from \"WAIT\" [2026-06-14 Sun 09:00]
:END:
")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((items (org-air-query-items))
           (parent (org-air-r93--scanned "Quiet parent" items))
           (child (org-air-r93--scanned "Busy child" items))
           (bare (org-air-r93--scanned "Historyless parent" items)))
      ;; The child IS fresh, on its own stamp.
      (should (equal '(2026 6 14) (org-air-r93--updated-day child)))
      (should (= 1 (org-air-classify-quiet-days child org-air-test-now)))
      (should-not (memq 'attention
                        (org-air-classify-item child org-air-test-now)))
      ;; The parent keeps its OWN April clock and surfaces.
      (should (equal '(2026 4 16) (org-air-r93--updated-day parent)))
      (should (= 60 (org-air-classify-quiet-days parent org-air-test-now)))
      (should (memq 'attention
                    (org-air-classify-item parent org-air-test-now)))
      ;; And a parent with NO body of its own does not inherit one.
      (should-not (org-air-item-updated bare)))))

;;;; -------------------------------------------------------------------
;;;; r93-8 — the coarse floor: last resort, no I/O, self-healing
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r93-8-mtime-floor-is-a-last-resort-and-self-heals ()
  "The file mtime is the floor for a HISTORYLESS heading only, and it self-heals.
Three properties, each of which the design commits to out loud:

  1. A heading with no stamp at all ages off its FILE's scan-time mtime
     (deliberately coarse: one edit anywhere refreshes every historyless
     heading in that file).
  2. A heading WITH a stamp never consults the floor \u2014 in either
     direction, so an old file cannot age a fresh heading and a fresh
     file cannot rejuvenate a quiet one.
  3. Classify makes NO file access: the floor is a hash lookup on the
     table the cache hydrates, never a `file-attributes' call."
  (skip-unless (locate-library "org-air"))
  (org-air-r93--with-corpus
      '(("old.org" . "\
* TODO Historyless in an old file
* TODO Fresh heading in an old file
:LOGBOOK:
- State \"TODO\" from \"WAIT\" [2026-06-14 Sun 09:00]
:END:
")
        ("new.org" . "\
* TODO Historyless in a new file
* TODO Quiet heading in a new file
[2026-04-16 Thu 09:00]
")
        ("inbox.org" . "#+title: inbox\n"))
    ;; old.org: 90 days before the frozen now.  new.org: the day after
    ;; it (the shape a scratch copy has, and a future mtime floors to 0).
    (set-file-times (org-air-r93--path "old.org")
                    (time-subtract org-air-test-now (days-to-time 90)))
    (set-file-times (org-air-r93--path "new.org")
                    (time-add org-air-test-now (days-to-time 1)))
    (let* ((items (org-air-query-items))
           (stats 0))
      (cl-letf* ((orig (symbol-function 'file-attributes))
                 ((symbol-function 'file-attributes)
                  (lambda (&rest args) (cl-incf stats) (apply orig args))))
        ;; 1. historyless => the file's clock.
        (let ((item (org-air-r93--scanned "Historyless in an old file" items)))
          (should-not (org-air-item-updated item))
          (should (= 90 (org-air-classify-quiet-days item org-air-test-now)))
          (should (memq 'attention
                        (org-air-classify-item item org-air-test-now))))
        (let ((item (org-air-r93--scanned "Historyless in a new file" items)))
          (should-not (org-air-item-updated item))
          ;; floored at 0: a clock skew can never read as negative age.
          (should (= 0 (org-air-classify-quiet-days item org-air-test-now)))
          (should-not (memq 'attention
                            (org-air-classify-item item org-air-test-now))))
        ;; 2. a stamp WINS the floor, both ways round.
        (let ((item (org-air-r93--scanned "Fresh heading in an old file" items)))
          (should (org-air-item-updated item))
          (should (= 1 (org-air-classify-quiet-days item org-air-test-now)))
          (should-not (memq 'attention
                            (org-air-classify-item item org-air-test-now))))
        (let ((item (org-air-r93--scanned "Quiet heading in a new file" items)))
          (should (= 60 (org-air-classify-quiet-days item org-air-test-now)))
          (should (memq 'attention
                        (org-air-classify-item item org-air-test-now))))
        ;; 3. none of that opened, stat'ed or read anything.
        (should (= 0 stats))))
    ;; 4. self-healing: give the historyless heading one state change and
    ;; rescan \u2014 it stops answering to the file and gets its own clock.
    (with-temp-buffer
      (insert-file-contents (org-air-r93--path "old.org"))
      (goto-char (point-min))
      (should (re-search-forward "^\\* TODO Historyless in an old file$" nil t))
      (insert "\n:LOGBOOK:\n- State \"TODO\" from \"WAIT\" [2026-06-14 Sun 09:00]\n:END:")
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region (point-min) (point-max)
                      (org-air-r93--path "old.org") nil 'silent)))
    (set-file-times (org-air-r93--path "old.org")
                    (time-subtract org-air-test-now (days-to-time 90)))
    (when (fboundp 'org-air-query-teardown) (org-air-query-teardown))
    (let ((healed (org-air-r93--scanned "Historyless in an old file")))
      (should (equal '(2026 6 14) (org-air-r93--updated-day healed)))
      (should (= 1 (org-air-classify-quiet-days healed org-air-test-now)))
      (should-not (memq 'attention
                        (org-air-classify-item healed org-air-test-now))))))

;;;; -------------------------------------------------------------------
;;;; r93-9 — retuning is LIVE and rescan-free
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r93-9-retune-is-live-and-never-rescans ()
  "`setq'-ing `org-air-attention-days' changes the NEXT repaint, with zero scans.
The thresholds join the render-time classify memo key, so a retune
self-invalidates the memo on the next repaint \u2014 a slot-fold rebuild.
They are deliberately NOT a scan-cache key input: no threshold changes
what a FILE means, so retuning must never cost a rescan.  Both halves
are asserted, plus the memo behaviour itself (same key => the memo is
kept; different key => rebuilt)."
  (skip-unless (locate-library "org-air"))
  (org-air-r93--with-corpus
      '(("tasks.org" . "\
* TODO Quiet ten days
[2026-06-05 Fri 09:00]
")
        ("inbox.org" . "#+title: inbox\n"))
    (org-air-viewport-test--with-frozen-now
      (with-current-buffer (get-buffer-create "*org-air-r93*")
        (unless (derived-mode-p 'org-air-view-mode) (org-air-view-mode))
        (let ((files (org-air-query-files)))
          (setq org-air-view--items (org-air-query-items)
                org-air-view--items-key (org-air-view--cache-key)
                org-air-view--items-mtimes (org-air-view--mtimes-snapshot files)
                org-air-view--classify-cache nil))
        (let ((item (org-air-r93--scanned "Quiet ten days" org-air-view--items))
              (scan-key (org-air-view--cache-key))
              (scans 0))
          (should (= 10 (org-air-classify-quiet-days item org-air-test-now)))
          (cl-letf (((symbol-function 'org-air-query--scan-file)
                     (lambda (&rest _) (cl-incf scans) nil))
                    ((symbol-function 'org-air-query-items)
                     (lambda (&rest _) (cl-incf scans) nil))
                    ((symbol-function 'org-air-query-items-in-files)
                     (lambda (&rest _) (cl-incf scans) nil)))
            ;; Default thresholds: 10 days quiet, 30 to go => no row.
            (org-air-view--render org-air-view--items nil)
            (let ((memo-key org-air-view--classify-cache-day))
              (should-not (memq 'attention
                                (org-air-view--classify-cached
                                 item org-air-test-now)))
              (should-not (string-match-p "Quiet ten days" (buffer-string)))
              ;; A repaint at the SAME settings keeps the memo table.
              (let ((table org-air-view--classify-cache))
                (org-air-view--render org-air-view--items nil)
                (should (eq table org-air-view--classify-cache))
                (should (equal memo-key org-air-view--classify-cache-day)))
              ;; The user retunes: a week is enough now.
              (let ((org-air-attention-days '((?A . 0) (nil . 7))))
                (org-air-view--render org-air-view--items nil)
                ;; the memo key MOVED (so the memo rebuilt)…
                (should-not (equal memo-key org-air-view--classify-cache-day))
                ;; …the row is on the board…
                (should (memq 'attention
                              (org-air-view--classify-cached
                               item org-air-test-now)))
                (should (string-match-p "Quiet ten days" (buffer-string)))
                (should (string-match-p "10d quiet" (buffer-string)))
                ;; …the SCAN key never moved (no file changed meaning)…
                (should (equal scan-key (org-air-view--cache-key))))
              ;; …and putting the threshold back puts the board back.
              (org-air-view--render org-air-view--items nil)
              (should (equal memo-key org-air-view--classify-cache-day))
              (should-not (string-match-p "Quiet ten days" (buffer-string))))
            ;; Nothing rescanned, at any point.
            (should (= 0 scans))))))))

;;;; -------------------------------------------------------------------
;;;; r93-10 — Overdue is its own thing
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r93-10-overdue-is-its-own-bucket-and-section ()
  "Overdue is its own bucket, section and predicate, independent of attention.
The split, in both directions: an overdue item that was touched today is
in Overdue and NOT in Needs attention (being LATE is not being QUIET),
and a quiet item with no date at all is in Needs attention and NOT in
Overdue.  An item that is both appears in both, which is the standing
overlap rule.  The section descriptor exists with its own empty state,
and the bucket is the `--overdue-p' predicate itself."
  (skip-unless (locate-library "org-air"))
  (let ((fresh (org-air-r93--item :title "Overdue, freshly touched"
                                  :deadline -3 :updated 0))
        (quiet (org-air-r93--item :title "Quiet, no date" :updated -60))
        (both (org-air-r93--item :title "Overdue and quiet"
                                 :deadline -3 :updated -60)))
    (should (equal '(overdue) (org-air-classify-item fresh org-air-test-now)))
    (should (equal '(attention) (org-air-classify-item quiet org-air-test-now)))
    (should (equal '(overdue attention)
                   (org-air-classify-item both org-air-test-now)))
    ;; The bucket body IS the predicate (one definition, shared).
    (dolist (item (list fresh quiet both))
      (should (eq (and (memq 'overdue (org-air-classify-item
                                       item org-air-test-now))
                       t)
                  (and (org-air-classify--overdue-p item org-air-test-now) t)))))
  ;; The section descriptor, with the empty-state line that always
  ;; belonged to it (it used to sit on Needs attention).
  (let ((descriptor (assq 'overdue org-air-view--sections)))
    (should descriptor)
    (should (equal "Overdue" (nth 1 descriptor)))
    (should (equal "Nothing overdue. Nice." (nth 2 descriptor))))
  ;; …and Needs attention teaches the new vocabulary in its own.
  (let ((descriptor (assq 'attention org-air-view--sections)))
    (should descriptor)
    (should (equal "Needs attention" (nth 1 descriptor)))
    (should (equal "Nothing has gone quiet." (nth 2 descriptor))))
  ;; Stale left no residue.
  (should-not (assq 'stale org-air-view--sections))
  (should-not (assq 'stale org-air-glyphs)))

;;;; -------------------------------------------------------------------
;;;; r93-11 — tokens agree with sections; `is:stale' is a parse-only alias
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r93-11-tokens-agree-and-stale-is-parse-only ()
  "`is:overdue' / `is:attention' select exactly their sections; `is:stale' aliases.
Agreement BY CONSTRUCTION over a real scanned corpus: for every item,
token membership and bucket membership are the same bit.  The retired
`is:stale' still parses \u2014 to the current symbol, once, at the parser \u2014
so a saved bookmark degrades honestly instead of decaying into a bare
substring search for the literal text \"is:stale\"; it is simply never
OFFERED, so the vocabulary org-air teaches is the current one.  A
bookmark that carries it still prints back verbatim in the lens."
  (skip-unless (locate-library "org-air"))
  (org-air-r93--with-corpus
      '(("tasks.org" . "\
* TODO Overdue and fresh
DEADLINE: <2026-06-10 Wed>
[2026-06-15 Mon 09:00]
* TODO Quiet and dateless
[2026-01-05 Mon 09:00]
* TODO [#A] Top priority, touched today
[2026-06-15 Mon 09:00]
* TODO Upcoming and fresh
SCHEDULED: <2026-06-16 Tue>
[2026-06-15 Mon 09:00]
")
        ("inbox.org" . "#+title: inbox\n"))
    (let ((items (org-air-query-items)))
      (dolist (pair '(("is:overdue" . overdue)
                      ("is:upcoming" . upcoming)
                      ("is:hipri" . high-priority)
                      ("is:attention" . attention)
                      ;; the alias selects the SAME set as its target.
                      ("is:stale" . attention)))
        (let ((org-air-view--tag-filter (list (car pair)))
              (org-air-filter-match 'all)
              (org-air-view--filter-now org-air-test-now)
              (org-air-view--scope nil)
              (org-air-view--render-partition nil))
          (dolist (item items)
            (let ((buckets (org-air-classify-item item org-air-test-now)))
              (ert-info ((format "%s vs %S on %s" (car pair) buckets
                                 (org-air-item-title item)))
                (should (eq (and (org-air-view--passes-filter-p item) t)
                            (and (memq (cdr pair) buckets) t))))))))
      ;; Anti-vacuity: each token really selected something.
      (should (= 4 (length items))))
    ;; Parse-only: it resolves at the parser and is never offered.
    (should (equal '(is . attention)
                   (org-air-view--filter-token-parse "is:stale")))
    (should (equal '(is . attention)
                   (org-air-view--filter-token-parse "IS:STALE")))
    (should-not (member "is:stale" (org-air-view--filter-vocabulary)))
    (should (member "is:attention" (org-air-view--filter-vocabulary)))
    ;; A bookmarked filter prints back verbatim (nothing rewrites it).
    (let ((org-air-view--tag-filter '("is:stale")))
      (should (equal '("is:stale") (org-air-view--filter-tags))))))

;;;; -------------------------------------------------------------------
;;;; r93-12 — the section order IS the sentence
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r93-12-section-order-notes-conditional-backlog-last ()
  "Inbox, Overdue, Upcoming, High priority, Needs attention \u2014 then Notes, then Backlog.
The order reads as a sentence: process, repair, plan, choose, sweep.
Needs attention sits at the BOTTOM of the task sections deliberately \u2014
it is the slow signal, and the three time-critical sections belong above
it.  The two conditional lenses keep their places: Notes only when the
board holds notes, and Backlog ALWAYS last."
  (skip-unless (locate-library "org-air"))
  (should (equal '(inbox overdue upcoming high-priority attention)
                 (mapcar #'car org-air-view--sections)))
  (should (equal '("Inbox" "Overdue" "Upcoming" "High priority"
                   "Needs attention")
                 (mapcar #'cadr org-air-view--sections)))
  (org-air-r93--with-corpus
      '(("tasks.org" . "\
* TODO Quiet task
[2026-01-05 Mon 09:00]
* TODO Deferred task :backlog:
[2026-01-05 Mon 09:00]
")
        ("notes.org" . "#+title: A knowledge note\n\nProse only, no tasks.\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((items (org-air-query-items))
           (order (mapcar #'car (org-air-view--section-descriptors items))))
      (should (equal '(inbox overdue upcoming high-priority attention
                             notes backlog)
                     order))
      ;; Backlog is last, and Notes is conditional: drop the notes and
      ;; the lens goes with them, Backlog still bringing up the rear.
      (let* ((taskish (seq-remove
                       (lambda (it) (memq 'notes (org-air-classify-item
                                                  it org-air-test-now)))
                       items))
             (order2 (mapcar #'car (org-air-view--section-descriptors taskish))))
        (should-not (memq 'notes order2))
        (should (eq 'backlog (car (last order2))))))))

;;;; -------------------------------------------------------------------
;;;; r93-13 — cache v6 -> v7
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r93-13-cache-v7-and-v6-clean-cold-miss ()
  "A v6 cache is a clean cold miss \u2014 never a hydration with no `updated'.
The bump is REQUIRED, not defensive.  A v6 record is one slot short, so
`read' would hand back an item whose `updated' is simply absent, and
EVERY heading would then age off the coarse file mtime: a whole board of
wrong reasons, silently, instead of one honest refill.  This test writes
a GENUINE v6 record \u2014 the current serialisation with the trailing
recency slot removed \u2014 and proves the version guard catches it before
the record shape ever matters."
  (skip-unless (locate-library "org-air"))
  (should (= 7 org-air-view--cache-version))
  (org-air-r93--with-corpus
      '(("tasks.org" . "\
* TODO Quiet task
[2026-01-05 Mon 09:00]
")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((files (org-air-query-files))
           (items (org-air-query-items))
           (mtimes (org-air-view--mtimes-snapshot files))
           (v7 (mapcar #'org-air-view--item-serialise items)))
      ;; The slot really is the LAST one, and it really is populated.
      (should (org-air-item-updated (car items)))
      (should (equal (org-air-item-updated (car items))
                     (aref (car v7) (1- (length (car v7))))))
      ;; A real v7 cache round-trips with the recency intact.
      (org-air-view--cache-write items mtimes)
      (let ((hydrated (plist-get (org-air-view--cache-read) :items)))
        (should hydrated)
        (should (equal (org-air-item-updated (car items))
                       (org-air-item-updated (car hydrated)))))
      ;; A genuine v6 record: the same payload, one slot shorter.
      (let ((v6 (mapcar (lambda (rec)
                          (apply #'record
                                 (butlast
                                  (cl-loop for i from 0 below (length rec)
                                           collect (aref rec i)))))
                        v7))
            (print-length nil) (print-level nil) (print-circle t))
        (should (= (1- (length (car v7))) (length (car v6))))
        (write-region
         (prin1-to-string (list :version 6 :key (org-air-view--cache-key)
                                :mtimes mtimes :file-meta nil :visits nil
                                :items v6))
         nil org-air-cache-file nil 'silent))
      ;; …refused at the version gate: no hydration, no error, no hang.
      (should-not (org-air-view--cache-read))
      (should-not (org-air-view--cache-load)))))

;;;; -------------------------------------------------------------------
;;;; r93-14 — the reason the user reads is the reason the bucket used
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r93-14-reason-labels-cannot-drift-from-the-bucket ()
  "The row cell and the rail Bucket line quote the SAME two public helpers.
`org-air-classify-attention-threshold' and `org-air-classify-quiet-days'
are public precisely so the label, the bucket and the inspector cannot
disagree.  The row's date cell carries `Nd quiet' (the measured age),
the bare word `quiet' (age unknown — the cell is never blank and
org-air never invents a number) or `always' (a threshold-0 row); the
inspector spells the same fact in words.

R93 FIX-3 RE-BLESS.  All three arms still exist and all three are still
covered — but the DEFAULT board no longer reaches `always', because no
priority ships on threshold 0 any more.  So the `always' arm is pinned
where it now lives: under an explicit `(?A . 0)' opt-in.  The row that
used to produce it — an `#A' with an UNKNOWN age — is pinned in its new
place too: it reads `quiet', and it is not in the section at all."
  (skip-unless (locate-library "org-air"))
  (let ((quiet (org-air-r93--item :title "Quiet" :updated -12 :priority ?B))
        (hipri-quiet (org-air-r93--item :title "Aged A" :updated -5 :priority ?A))
        (unknown (org-air-r93--item :title "Undatable" :priority ?A)))
    ;; the row cell (the label reads the LIVE clock, so freeze it)
    (org-air-viewport-test--with-frozen-now
      (should (equal (cons "12d quiet" 'org-air-face-date)
                     (org-air-view--date-label quiet 'attention)))
      (should (equal (cons "5d quiet" 'org-air-face-date)
                     (org-air-view--date-label hipri-quiet 'attention)))
      ;; unknown age at a POSITIVE threshold: the honest bare word.
      (should (equal (cons "quiet" 'org-air-face-date)
                     (org-air-view--date-label unknown 'attention)))
      ;; and `always' is exactly the threshold-0 arm, nothing else.
      (let ((org-air-attention-days '((?A . 0) (nil . 30))))
        (should (equal (cons "always" 'org-air-face-date)
                       (org-air-view--date-label unknown 'attention)))))
    ;; the number in the cell IS the number the rule applied
    (should (= 12 (org-air-classify-quiet-days quiet org-air-test-now)))
    (should (= 7 (org-air-classify-attention-threshold quiet)))
    (should (= 5 (org-air-classify-quiet-days hipri-quiet org-air-test-now)))
    (should (= 3 (org-air-classify-attention-threshold hipri-quiet)))
    ;; the unknown-age row is not in the section it would label
    (should-not (org-air-classify-quiet-days unknown org-air-test-now))
    (should-not (memq 'attention
                      (org-air-classify-item unknown org-air-test-now)))
    ;; the rail says it in words, from the same helpers
    (with-temp-buffer
      (org-air-view-mode)
      (setq org-air-view--classify-cache nil)
      (let ((line (org-air-view--inspector-bucket-line
                   quiet "" org-air-test-now)))
        (should (string-match-p "Attention" line))
        (should (string-match-p "quiet 12d / 7d" line)))
      (setq org-air-view--classify-cache nil)
      (let ((line (org-air-view--inspector-bucket-line
                   hipri-quiet "" org-air-test-now)))
        (should (string-match-p "High-priority" line))
        (should (string-match-p "Attention" line))
        (should (string-match-p "quiet 5d / 3d" line)))
      ;; an undatable `#A': High priority only, no attention reason.
      (setq org-air-view--classify-cache nil)
      (let ((line (org-air-view--inspector-bucket-line
                   unknown "" org-air-test-now)))
        (should (string-match-p "High-priority" line))
        (should-not (string-match-p "Attention" line))
        (should-not (string-match-p "always surfaces" line)))
      ;; …and the kept threshold-0 wording, under the opt-in.
      (setq org-air-view--classify-cache nil)
      (let* ((org-air-attention-days '((?A . 0) (nil . 30)))
             (line (org-air-view--inspector-bucket-line
                    unknown "" org-air-test-now)))
        (should (string-match-p "always surfaces" line))))))

;;;; -------------------------------------------------------------------
;;;; r93-15 — the overlap is EARNED, not granted
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r93-15-attention-overlap-is-earned-not-granted ()
  "High priority and Needs attention are INDEPENDENT sections at the defaults.
R93 FIX-3 RE-BLESS, and an INVERSION: this test used to pin the
opposite.  With `#A' on threshold 0 Needs attention was a permanent
strict SUPERSET of High priority — every `#A' row printed twice, forever
— and the old docstring said in as many words that raising `#A' above 0
is the change that must re-bless this test and have the conversation.
The conversation happened; the user raised it to 3.  So the invariant
that the round earned is pinned here instead:

  a FRESH `#A' is in High priority and NOT in Needs attention;
  a QUIET `#A' is in BOTH — the overlap is EARNED by silence.

Three things are pinned so this cannot be mistaken for a weakening:
the fixture board's measured numbers, the strict NON-containment in
both directions, and the fact that `(?A . 0)' restores the old superset
EXACTLY — proving FIX-3 changed one default and no mechanism."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (cl-flet ((split ()
                (let ((hipri nil) (attention nil))
                  (dolist (item (org-air-query-items))
                    (let ((buckets (org-air-classify-item
                                    item org-air-test-now)))
                      (when (memq 'high-priority buckets) (push item hipri))
                      (when (memq 'attention buckets) (push item attention))))
                  (cons (nreverse hipri) (nreverse attention)))))
      ;; DEFAULTS: the two sections are disjoint on this corpus, and the
      ;; measured board numbers are 3 and 2 (was 3 and 5 — three of the
      ;; five were restatements of the whole High priority section).
      (pcase-let ((`(,hipri . ,attention) (split)))
        (should (= 3 (length hipri)))
        (should (= 2 (length attention)))
        (should-not (seq-intersection hipri attention))
        ;; neither section contains the other — stated in both
        ;; directions, so a future "superset" cannot creep back either way
        (dolist (item hipri) (should-not (memq item attention)))
        (dolist (item attention) (should-not (memq item hipri)))
        ;; every High-priority row here is genuinely FRESH: it is absent
        ;; from Needs attention because it was touched, not by fiat.
        (dolist (item hipri)
          (let ((age (org-air-classify-quiet-days item org-air-test-now)))
            (should age)
            (should (< age (org-air-classify-attention-threshold item))))))
      ;; The overlap is EARNED: the same `#A' shape, silent past its
      ;; three days, is in BOTH sections.
      (should (equal '(high-priority attention)
                     (org-air-classify-item
                      (org-air-r93--item :priority ?A :updated -3)
                      org-air-test-now)))
      (should (equal '(high-priority)
                     (org-air-classify-item
                      (org-air-r93--item :priority ?A :updated -2)
                      org-air-test-now)))
      ;; ONE DEFAULT, NO MECHANISM: set `(?A . 0)' back and the old
      ;; superset returns exactly, on the same corpus, same clock.
      (let ((org-air-attention-days '((?A . 0) (?B . 7) (?C . 14)
                                      (?D . 30) (?E . 30) (nil . 30))))
        (pcase-let ((`(,hipri . ,attention) (split)))
          (should (= 3 (length hipri)))
          (should (= 5 (length attention)))
          (dolist (item hipri) (should (memq item attention)))
          (should (< (length hipri) (length attention))))))))

;;;; -------------------------------------------------------------------
;;;; r93-16 — the INVERTED audit tripwire: worst-first, per section
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r93-16-overdue-worst-first-attention-longest-silent-first ()
  "Overdue renders WORST-FIRST and Needs attention LONGEST-SILENT-FIRST.
THE INVERTED TRIPWIRE.  This test was born red, on purpose: the R93
audit found that splitting Overdue out of Needs attention had left the
new `overdue' bucket off the `date' arm of `org-air-view--sort-items',
so the ALARM section rendered in file/query order — least urgent first
— and 28 regenerated goldens had already shipped that order as if it
were the design.  R93 FIX-2 added the arm; the tripwire is now inverted
into the permanent assertion it always said it should become, on the
same corpus, against the same measured rows:

  parent (R92 FIX)   Chase missing invoice  OVERDUE 7d
                     Fix production runbook OVERDUE 5d
                     Book dentist           OVERDUE 3d
  R93 (the defect)   Book dentist           OVERDUE 3d   <- least urgent first
                     Fix production runbook OVERDUE 5d
                     Chase missing invoice  OVERDUE 7d
  R93 FIX-2 (now)    Chase missing invoice  OVERDUE 7d   <- worst first again

FIX-2 also found the SAME class of defect one section down: Needs
attention had kept a date sort it no longer had any use for (its date
cell prints a REASON, not a date), so its dateless half sorted in SCAN
order and the goldens shipped `225d quiet' ABOVE `273d quiet'.  The
equivalent assertion for that section is pinned here beside it.

Both halves are stated ANTI-TAUTOLOGICALLY: the query order the scan
hands over is captured and asserted to DIFFER from the rendered order,
so a sort that silently stopped running could not pass by accident."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (let* ((items (org-air-query-items))
           (members (lambda (bucket)
                      (seq-filter
                       (lambda (it) (memq bucket (org-air-classify-item
                                                  it org-air-test-now)))
                       items)))
           (overdue (funcall members 'overdue))
           (attention (funcall members 'attention)))
      (should (= 3 (length overdue)))
      (should (= 2 (length attention)))
      (with-temp-buffer
        (org-air-view-mode)
        (org-air-viewport-test--with-frozen-now
          ;; ---- Overdue: worst (most days past) FIRST ----------------
          (let ((sorted (org-air-view--sort-items overdue 'overdue)))
            (should (equal '("Chase missing invoice"
                             "Fix production outage runbook"
                             "Book dentist appointment")
                           (mapcar #'org-air-item-title sorted)))
            ;; the printed cells, strictly decreasing
            (should (equal '("OVERDUE 7d" "OVERDUE 5d" "OVERDUE 3d")
                           (mapcar (lambda (it)
                                     (car (org-air-view--date-label
                                           it 'overdue)))
                                   sorted)))
            ;; anti-tautology: the scan handed over a DIFFERENT order
            (should-not (equal (mapcar #'org-air-item-title overdue)
                               (mapcar #'org-air-item-title sorted))))
          ;; ---- Needs attention: longest SILENT first ----------------
          (let ((sorted (org-air-view--sort-items attention 'attention)))
            (should (equal '("Learn lute" "Dust off old archive project")
                           (mapcar #'org-air-item-title sorted)))
            (should (equal '("273d quiet" "225d quiet")
                           (mapcar (lambda (it)
                                     (car (org-air-view--date-label
                                           it 'attention)))
                                   sorted)))
            (should-not (equal (mapcar #'org-air-item-title attention)
                               (mapcar #'org-air-item-title sorted))))
          ;; ---- the two keys are DIFFERENT keys ----------------------
          ;; Needs attention must NOT be ordered by the date key it used
          ;; to share with Overdue: these rows have no dates at all, so
          ;; that key would return them in scan order (the FIX-2 defect).
          (should (equal (mapcar #'org-air-item-title attention)
                         (mapcar #'org-air-item-title
                                 (org-air-view--sort-by-date attention))))
          (should-not
           (equal (mapcar #'org-air-item-title
                          (org-air-view--sort-by-date attention))
                  (mapcar #'org-air-item-title
                          (org-air-view--sort-items attention 'attention)))))))))

;;;; -------------------------------------------------------------------
;;;; r93-17 — the `#A' = 3 default AT ITS BOUNDARY, and the 0 opt-in
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r93-17-hipri-three-day-boundary-and-zero-opt-in ()
  "`#A' waits exactly three days, and `(?A . 0)' still means never wait.
R93 FIX-3's number, pinned where a number must be pinned: at its
BOUNDARY.  Day 2 does not surface, day 3 does (the rule is `>=', not
`>'), day 4 still does — asserted on the predicate AND on the bucket, so
a rule that drifts between the section body and its `is:attention' token
cannot pass, and asserted on the ROW CELL, so the number the user reads
is the number the rule applied.

Two things are pinned around the boundary so the change cannot be
misread.  First: on BOTH sides of it the `#A' is on the board — High
priority is unconditional and has no clock — so the boundary decides
whether the row is shown TWICE, never whether it is shown.  Second:
`#A' really is the sharpest threshold, i.e. the same three days of
silence surfaces an `#A' and no other priority.

And the mechanism outlives its default: `(?A . 0)' restores
unconditional surfacing — day 0, day 1000 and the UNKNOWN age — and the
row cell goes back to the fixed literal `always'.  Only ONE number
moved in FIX-3; this is the test that proves it."
  (skip-unless (locate-library "org-air"))
  (let ((org-priority-lowest ?E))
    ;; ---- the boundary, day by day -------------------------------------
    (pcase-dolist (`(,quiet . ,expected) '((0 . nil) (1 . nil) (2 . nil)
                                           (3 . t) (4 . t) (400 . t)))
      (let* ((item (org-air-r93--item :priority ?A :updated (- quiet)))
             (buckets (org-air-classify-item item org-air-test-now)))
        (ert-info ((format "#A quiet %dd => %S" quiet buckets))
          (should (= 3 (org-air-classify-attention-threshold item)))
          (should (= quiet (org-air-classify-quiet-days item org-air-test-now)))
          (should (eq expected (and (org-air-classify--attention-p
                                     item org-air-test-now)
                                    t)))
          (should (eq expected (and (memq 'attention buckets) t)))
          ;; on BOTH sides of the boundary the row is on the board
          (should (memq 'high-priority buckets))
          ;; and the cell shows the very number the rule measured
          (org-air-viewport-test--with-frozen-now
            (should (equal (format "%dd quiet" quiet)
                           (car (org-air-view--date-label item 'attention))))))))
    ;; ---- `#A' is the sharpest threshold in the table -------------------
    (dolist (letter '(?B ?C ?D ?E nil))
      (ert-info ((format "priority %s quiet 3d" (if letter (string letter) "none")))
        (should-not (memq 'attention
                          (org-air-classify-item
                           (org-air-r93--item :priority letter :updated -3)
                           org-air-test-now)))))
    ;; ---- `(?A . 0)': the kept, opt-in unconditional arm ----------------
    (let ((org-air-attention-days '((?A . 0) (?B . 7) (?C . 14)
                                    (?D . 30) (?E . 30) (nil . 30))))
      (dolist (updated '(0 -1 -2 -1000 nil))
        (let ((item (org-air-r93--item :priority ?A :updated updated)))
          (ert-info ((format "(?A . 0) updated=%S" updated))
            (should (= 0 (org-air-classify-attention-threshold item)))
            (should (org-air-classify--attention-p item org-air-test-now))
            (should (equal '(high-priority attention)
                           (org-air-classify-item item org-air-test-now)))
            (org-air-viewport-test--with-frozen-now
              (should (equal "always"
                             (car (org-air-view--date-label
                                   item 'attention))))))))
      ;; the retune moved ONE row of the table: `#B' still waits a week.
      (should-not (memq 'attention
                        (org-air-classify-item
                         (org-air-r93--item :priority ?B :updated -3)
                         org-air-test-now)))
      (should (memq 'attention
                    (org-air-classify-item
                     (org-air-r93--item :priority ?B :updated -7)
                     org-air-test-now))))))

;;;; -------------------------------------------------------------------
;;;; r93-18 — the unknown-age ruling, swept over every threshold
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r93-18-no-positive-threshold-surfaces-an-undatable-item ()
  "SWEPT: no positive threshold on any priority ever nags about an unknown age.
The R93 ruling stated as a law rather than a case list: a POSITIVE
threshold is a claim about ELAPSED TIME, and org-air will not assert
elapsed time it never measured.  Swept over every priority letter
(including the no-cookie row) crossed with ten thresholds from 1 day to
10000, all against an item org-air genuinely cannot date — no `updated'
slot and a file with no entry in the scan's meta table.

Three complements make the law falsifiable rather than merely negative:

  - it is the SIGN of the threshold that decides, not the letter and not
    the section: the same undatable item at threshold 0 surfaces every
    time;
  - it is the UNKNOWN AGE that is refused, not the item: give it any
    measured age past its threshold and it surfaces;
  - and NOTHING IS LOST — High priority still shows the undatable `#A'
    at every threshold in the sweep, because that section has no clock
    to be unable to read."
  (skip-unless (locate-library "org-air"))
  (let ((org-priority-lowest ?E)
        (letters '(?A ?B ?C ?D ?E nil))
        (thresholds '(1 2 3 7 14 21 30 60 365 10000)))
    ;; ---- the law -------------------------------------------------------
    (dolist (letter letters)
      (dolist (threshold thresholds)
        (let* ((org-air-attention-days (list (cons letter threshold)
                                             (cons nil threshold)))
               (item (org-air-r93--item :priority letter)))
          (ert-info ((format "priority %s threshold %dd, age UNKNOWN"
                             (if letter (string letter) "none") threshold))
            (should (= threshold (org-air-classify-attention-threshold item)))
            (should-not (org-air-classify-updated item))
            (should-not (org-air-classify-quiet-days item org-air-test-now))
            (should-not (org-air-classify--attention-p item org-air-test-now))
            (should-not (memq 'attention (org-air-classify-item
                                          item org-air-test-now)))))))
    ;; ---- the SIGN of the threshold is what decides ---------------------
    (dolist (letter letters)
      (let ((org-air-attention-days (list (cons letter 0) (cons nil 0))))
        (ert-info ((format "priority %s threshold 0, age UNKNOWN"
                           (if letter (string letter) "none")))
          (should (org-air-classify--attention-p
                   (org-air-r93--item :priority letter) org-air-test-now)))))
    ;; ---- it is the unknown AGE that is refused, not the item -----------
    (dolist (letter letters)
      (let* ((org-air-attention-days (list (cons letter 5) (cons nil 5)))
             (item (org-air-r93--item :priority letter :updated -5)))
        (should (= 5 (org-air-classify-quiet-days item org-air-test-now)))
        (should (memq 'attention (org-air-classify-item
                                  item org-air-test-now)))))
    ;; ---- and the undatable `#A' is never LOST --------------------------
    (dolist (threshold '(0 1 3 30 10000))
      (let* ((org-air-attention-days (list (cons ?A threshold) (cons nil 30)))
             (buckets (org-air-classify-item (org-air-r93--item :priority ?A)
                                             org-air-test-now)))
        (ert-info ((format "undatable #A at threshold %dd => %S"
                           threshold buckets))
          (should (memq 'high-priority buckets))
          ;; …and at a positive threshold High priority is ALL it is.
          (should (eq (and (memq 'attention buckets) t)
                      (= threshold 0))))))))

;;;; -------------------------------------------------------------------
;;;; r93-19 — the per-section sort table (R93 FIX-2)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r93-19-per-section-sort-table ()
  "Each section is ordered by ITS OWN printed number, worst first.
The R93 FIX-2 table, executed:

  overdue     earliest date first  = MOST overdue first (the alarm
              section leads with the worst broken promise);
  upcoming    earliest date first  = soonest first, undated LAST, so the
              cap shows the NEXT N — the only cap that means anything;
  attention   longest SILENT first, UNKNOWN age LAST (mirroring how the
              date key trails undated rows: org-air never invents a
              number and never lets an unknown outrank a measured one);
  inbox / high-priority / backlog
              QUERY order, untouched — they have no per-row number to be
              worst-first BY.

Plus the two properties that make the attention key trustworthy: it is
STABLE (equal ages keep their incoming order, so a repaint never
reshuffles rows) and a threshold-0 row is sorted by its REAL age rather
than parked (its label suppresses the number, its silence still ranks
it).  `O' reverses whatever the section produced."
  (skip-unless (locate-library "org-air"))
  (with-temp-buffer
    (org-air-view-mode)
    (org-air-viewport-test--with-frozen-now
      ;; ---- Overdue: most overdue first --------------------------------
      (let* ((mild (org-air-r93--item :title "late-2" :deadline -2))
             (worst (org-air-r93--item :title "late-9" :scheduled -9))
             (mid (org-air-r93--item :title "late-5" :deadline -5))
             (input (list mild worst mid)))
        (should (equal '("late-9" "late-5" "late-2")
                       (mapcar #'org-air-item-title
                               (org-air-view--sort-items input 'overdue))))
        ;; the mixed slot really is exercised: the worst row is a
        ;; SCHEDULED one, so an implementation that read only deadlines
        ;; would not produce this order.
        (should (org-air-item-scheduled worst)))
      ;; ---- Upcoming: soonest first, undated last ----------------------
      (let* ((far (org-air-r93--item :title "in-5" :scheduled 5))
             (now (org-air-r93--item :title "in-0" :deadline 0))
             (soon (org-air-r93--item :title "in-2" :scheduled 2))
             (undated (org-air-r93--item :title "no-date"))
             (input (list far undated now soon)))
        (should (equal '("in-0" "in-2" "in-5" "no-date")
                       (mapcar #'org-air-item-title
                               (org-air-view--sort-items input 'upcoming)))))
      ;; ---- Needs attention: longest silent first, unknown LAST --------
      (let* ((q40 (org-air-r93--item :title "quiet-40" :updated -40))
             (q300 (org-air-r93--item :title "quiet-300" :updated -300))
             (q100 (org-air-r93--item :title "quiet-100" :updated -100))
             (tie-a (org-air-r93--item :title "tie-a" :updated -40))
             (tie-b (org-air-r93--item :title "tie-b" :updated -40))
             (unknown (org-air-r93--item :title "unknown-age")))
        (should (equal '("quiet-300" "quiet-100" "quiet-40")
                       (mapcar #'org-air-item-title
                               (org-air-view--sort-items
                                (list q40 q300 q100) 'attention))))
        ;; unknown age LAST, and equal ages keep their INCOMING order
        ;; (stable: a repaint never reshuffles the section).
        (should (equal '("quiet-300" "quiet-40" "tie-a" "tie-b" "unknown-age")
                       (mapcar #'org-air-item-title
                               (org-air-view--sort-items
                                (list q40 tie-a unknown tie-b q300)
                                'attention))))
        (should (equal '("quiet-300" "tie-a" "tie-b" "quiet-40" "unknown-age")
                       (mapcar #'org-air-item-title
                               (org-air-view--sort-items
                                (list tie-a tie-b q40 unknown q300)
                                'attention))))
        ;; a threshold-0 row is ranked by its REAL silence, not parked:
        ;; its label hides the number, the sort still uses it.
        (let ((org-air-attention-days '((?A . 0) (nil . 30))))
          (let ((always-old (org-air-r93--item :title "always-200"
                                               :priority ?A :updated -200))
                (always-new (org-air-r93--item :title "always-2"
                                               :priority ?A :updated -2)))
            (should (equal "always" (car (org-air-view--date-label
                                          always-old 'attention))))
            (should (equal '("always-200" "quiet-100" "always-2")
                           (mapcar #'org-air-item-title
                                   (org-air-view--sort-items
                                    (list always-new q100 always-old)
                                    'attention))))))
        ;; ---- the sections with NO per-row number keep query order -----
        (let ((input (list q40 q300 q100 unknown)))
          (dolist (bucket '(inbox high-priority backlog))
            (ert-info ((format "bucket %s keeps query order" bucket))
              (should (equal (mapcar #'org-air-item-title input)
                             (mapcar #'org-air-item-title
                                     (org-air-view--sort-items
                                      input bucket)))))))
        ;; ---- `O' reverses whatever the section produced ---------------
        (let ((input (list q40 q300 q100)))
          (setq-local org-air-view--sort-direction 'descending)
          (should (equal '("quiet-40" "quiet-100" "quiet-300")
                         (mapcar #'org-air-item-title
                                 (org-air-view--sort-items input 'attention))))
          (setq-local org-air-view--sort-direction 'ascending))))))

;;;; -------------------------------------------------------------------
;;;; r93-20 — a capped section shows the WORST N, never N arbitrary rows
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r93-20-capped-sections-show-the-worst-n ()
  "Overdue/Needs attention/Upcoming cap at 6/6/5 — and the cap keeps the WORST.
The corpus is written in exactly the WRONG order: the mildest heading
first in the file, in every one of the three capped sections.  A section
that capped BEFORE sorting — or never sorted — would therefore render
the mildest rows and still satisfy every count, badge and fold-row
assertion while telling the user the opposite of the truth.  That is the
failure mode this test exists for, so the assertions are made three
ways: the exact rows shown, the exact rows DROPPED, and the ordering
relation between the two sets (every dropped row is strictly milder than
every shown row).

End-to-end through the real renderer with the anti-tautology guards
active: the titles and numbers asserted are read back off the PAINTED
rows, never recomputed."
  (skip-unless (locate-library "org-air"))
  (let* ((late '(1 2 3 4 5 6 7 8 9))            ; written mildest first
         (silent '(31 35 40 50 60 80 100 150 300))
         (soon '(7 6 5 4 3 2 1 0))              ; written latest first
         (text (concat
                (mapconcat
                 (lambda (d) (format "* TODO Late %02d\nDEADLINE: %s\n%s\n"
                                     d (org-air-r93--date (- d))
                                     (org-air-r93--stamp 0)))
                 late "")
                (mapconcat
                 (lambda (d) (format "* TODO Silent %03d\n%s\n"
                                     d (org-air-r93--stamp (- d))))
                 silent "")
                (mapconcat
                 (lambda (d) (format "* TODO Soon %d\nSCHEDULED: %s\n%s\n"
                                     d (org-air-r93--date d)
                                     (org-air-r93--stamp 0)))
                 soon ""))))
    (org-air-r93--with-corpus
        (list (cons "tasks.org" text)
              (cons "inbox.org" "#+title: inbox\n"))
      (org-air-r93--render-board '(160 . 60)
        (let* ((sections (org-air-r93--rendered-sections))
               (folds (org-air-r93--fold-counts))
               (rows (lambda (bucket) (alist-get bucket sections))))
          ;; anti-vacuity: the corpus really is bigger than every cap.
          (should (= 9 (length late)))
          (should (= 9 (length silent)))
          (should (= 8 (length soon)))
          ;; ---- Overdue: the 6 WORST, worst first --------------------
          (let ((shown (funcall rows 'overdue)))
            (should (= 6 (length shown)))
            (should (equal '("Late 09" "Late 08" "Late 07" "Late 06"
                             "Late 05" "Late 04")
                           (mapcar #'car shown)))
            ;; the numbers the rows PRINT, strictly decreasing
            (should (equal '(9 8 7 6 5 4) (mapcar #'cadr shown)))
            ;; the mildest rows — the ones a first-N cap would have
            ;; shown — are exactly the ones dropped…
            (dolist (title '("Late 01" "Late 02" "Late 03"))
              (should-not (member title (mapcar #'car shown))))
            ;; …and every dropped row is strictly milder than every
            ;; shown row (the WORST-N statement, not a title list).
            (should (< (apply #'max '(1 2 3))
                       (apply #'min (mapcar #'cadr shown))))
            (should (equal 3 (alist-get 'overdue folds))))
          ;; ---- Needs attention: the 6 LONGEST silent ----------------
          (let ((shown (funcall rows 'attention)))
            (should (= 6 (length shown)))
            (should (equal '("Silent 300" "Silent 150" "Silent 100"
                             "Silent 080" "Silent 060" "Silent 050")
                           (mapcar #'car shown)))
            (should (equal '(300 150 100 80 60 50) (mapcar #'cadr shown)))
            (dolist (title '("Silent 031" "Silent 035" "Silent 040"))
              (should-not (member title (mapcar #'car shown))))
            (should (< (apply #'max '(31 35 40))
                       (apply #'min (mapcar #'cadr shown))))
            (should (equal 3 (alist-get 'attention folds))))
          ;; ---- Upcoming: the 5 SOONEST ------------------------------
          (let ((shown (funcall rows 'upcoming)))
            (should (= 5 (length shown)))
            (should (equal '("Soon 0" "Soon 1" "Soon 2" "Soon 3" "Soon 4")
                           (mapcar #'car shown)))
            (dolist (title '("Soon 5" "Soon 6" "Soon 7"))
              (should-not (member title (mapcar #'car shown))))
            (should (equal 3 (alist-get 'upcoming folds))))
          ;; the badges still count every MEMBER, not the shown subset,
          ;; so the cap hides rows and never facts.
          (let ((counts (org-air-viewport-test-section-counts)))
            (should (equal 9 (alist-get 'overdue counts)))
            (should (equal 9 (alist-get 'attention counts)))
            (should (equal 8 (alist-get 'upcoming counts)))))))))

;;;; -------------------------------------------------------------------
;;;; r93-21 — the sort key is the number the row PRINTS
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r93-21-the-sort-key-is-the-number-the-row-prints ()
  "Re-sorting the PAINTED rows by their own printed number reproduces the paint.
The R93 FIX-2 defect in one sentence: a section can be ordered by a key
the row does not show.  Needs attention was ordered by DATE while its
cell printed an AGE, so the board shipped `225d quiet' ABOVE `273d
quiet' — visibly wrong to the only person who can check it, and
invisible to every assertion that re-derived the key instead of reading
it.  The countermeasure is this test: every number is read back out of
the PAINTED row on the real fixture board, and sorting those rows by
their own printed number must reproduce the painted order exactly.

Also pinned here: the painted number is the number the PUBLIC classify
helper computed (so the cell cannot drift from the rule), and no row on
a DEFAULT board prints the threshold-0 excuse `always' any more (R93
FIX-3) — every Needs-attention row carries a real, actionable age."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 120
    (let* ((sections (org-air-r93--rendered-sections))
           (overdue (alist-get 'overdue sections))
           (attention (alist-get 'attention sections)))
      ;; anti-vacuity: the board really painted both sections.
      (should (= 3 (length overdue)))
      (should (= 2 (length attention)))
      ;; every row in both sections printed a NUMBER (never a blank cell).
      (dolist (row (append overdue attention))
        (should (integerp (cadr row))))
      ;; the printed numbers, worst first.
      (should (equal '(7 5 3) (mapcar #'cadr overdue)))
      (should (equal '(273 225) (mapcar #'cadr attention)))
      ;; THE LAW: the painted order IS the order of the painted numbers.
      (dolist (rows (list overdue attention))
        (should (equal rows
                       (sort (copy-sequence rows)
                             (lambda (a b) (> (cadr a) (cadr b)))))))
      ;; the painted number is the number the rule computed.
      (dolist (row attention)
        (should (= (cadr row)
                   (org-air-classify-quiet-days (nth 2 row) org-air-test-now)))
        (should (equal (format "%dd quiet" (cadr row))
                       (car (org-air-view--date-label (nth 2 row) 'attention)))))
      (dolist (row overdue)
        (should (equal (format "OVERDUE %dd" (cadr row))
                       (car (org-air-view--date-label (nth 2 row) 'overdue)))))
      ;; no DEFAULT board row prints the threshold-0 excuse any more.
      (let ((pos (point-min)))
        (while (setq pos (text-property-not-all pos (point-max)
                                                'org-air-item nil))
          (let ((end (or (next-single-property-change pos 'org-air-item)
                         (point-max))))
            (should-not (string-match-p
                         "always" (buffer-substring-no-properties pos end)))
            (setq pos end)))))))

(provide 'org-air-round93-test)
;;; org-air-round93-test.el ends here
