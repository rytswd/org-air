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
;;   r93-1   the threshold TABLE is the user's ruling (#A 0, #B 7, #C 14,
;;           #D/#E/no-cookie 30) and an unlisted priority falls back.
;;   r93-2   the aging predicate at every BOUNDARY, per priority letter:
;;           the day before does not surface, the day OF does, the day
;;           after does — as a predicate AND as a bucket.
;;   r93-3   `#A' surfaces unconditionally, INCLUDING with an unknown
;;           age; every other threshold does NOT surface on unknown age.
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
;;   r93-15  the OVERLAP the user chose to keep: with `#A' at threshold 0
;;           Needs attention is a strict SUPERSET of High priority.
;;   r93-16  AUDIT FINDING, pinned deliberately: the Overdue section is
;;           NOT date-sorted.  See its docstring — this test is a
;;           tripwire, not a blessing.

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
should come up after some time of no update\": `#A' 0 (always), `#B' a
week, `#C' a fortnight, `#D' / `#E' / no cookie a month.  `#D' and `#E'
are listed EXPLICITLY even though they equal the nil fallback, so a user
reading the alist sees the whole table instead of inferring it."
  (skip-unless (locate-library "org-air"))
  (let ((org-priority-lowest ?E))       ; so ?D / ?E are real cookies
    (pcase-dolist (`(,letter . ,days) '((?A . 0) (?B . 7) (?C . 14)
                                        (?D . 30) (?E . 30) (nil . 30)))
      (ert-info ((format "priority %s => %d days"
                         (if letter (string letter) "none") days))
        (should (equal days (cdr (assq letter org-air-attention-days))))
        (should (= days (org-air-classify-attention-threshold
                         (org-air-r93--item :priority letter)))))))
  ;; A priority the alist does not list falls back to the nil entry…
  (let ((org-air-attention-days '((?A . 0) (nil . 30))))
    (should (= 30 (org-air-classify-attention-threshold
                   (org-air-r93--item :priority ?C)))))
  ;; …and with no nil entry either, to the documented default.
  (let ((org-air-attention-days '((?A . 0)))
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
;;;; r93-3 — `#A' is unconditional; unknown age nags nobody else
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r93-3-hipri-always-unknown-age-never ()
  "`#A' surfaces unconditionally; every other threshold refuses an unknown age.
Two halves of one ruling.  `#A' has threshold 0, so it surfaces on day
zero, on day one thousand and \u2014 the case that matters \u2014 with an age
org-air simply does not know (an item built outside the scan: no
`updated' slot and no file in the scan's meta table).  Every other
priority does the opposite with the same unknown age: org-air refuses to
nag about something it cannot date, rather than treating unknown as
either \"fresh\" (never surfaces, the failure the user reported) or
\"infinitely old\" (every fresh corpus nags on day one)."
  (skip-unless (locate-library "org-air"))
  (let ((org-priority-lowest ?E))
    ;; `#A': always, at any age and at NO age.
    (dolist (updated '(0 -1 -1000 nil))
      (let ((item (org-air-r93--item :priority ?A :updated updated)))
        (ert-info ((format "#A updated=%S" updated))
          (should (org-air-classify--attention-p item org-air-test-now))
          (should (memq 'attention
                        (org-air-classify-item item org-air-test-now))))))
    ;; The unknown age really is unknown (nothing invented it).
    (let ((item (org-air-r93--item :priority ?A)))
      (should-not (org-air-classify-updated item))
      (should-not (org-air-classify-quiet-days item org-air-test-now)))
    ;; Every other threshold: unknown age => no row.
    (dolist (letter '(?B ?C ?D ?E nil))
      (let ((item (org-air-r93--item :priority letter)))
        (ert-info ((format "priority %s with an unknown age"
                           (if letter (string letter) "none")))
          (should-not (org-air-classify-quiet-days item org-air-test-now))
          (should-not (org-air-classify--attention-p item org-air-test-now))
          (should-not (memq 'attention
                            (org-air-classify-item item org-air-test-now))))))
    ;; The threshold, not the letter, is what does it: retune `#B' to 0
    ;; and it surfaces on an unknown age too.
    (let ((org-air-attention-days '((?A . 0) (?B . 0) (nil . 30))))
      (should (org-air-classify--attention-p
               (org-air-r93--item :priority ?B) org-air-test-now)))))

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
disagree.  The row's date cell carries `Nd quiet' or `always'; the
inspector spells the same fact in words."
  (skip-unless (locate-library "org-air"))
  (let ((quiet (org-air-r93--item :title "Quiet" :updated -12 :priority ?B))
        (always (org-air-r93--item :title "Always" :priority ?A)))
    ;; the row cell (the label reads the LIVE clock, so freeze it)
    (org-air-viewport-test--with-frozen-now
      (should (equal (cons "12d quiet" 'org-air-face-date)
                     (org-air-view--date-label quiet 'attention)))
      (should (equal (cons "always" 'org-air-face-date)
                     (org-air-view--date-label always 'attention))))
    ;; the number in the cell IS the number the rule applied
    (should (= 12 (org-air-classify-quiet-days quiet org-air-test-now)))
    (should (= 7 (org-air-classify-attention-threshold quiet)))
    (should (= 0 (org-air-classify-attention-threshold always)))
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
                   always "" org-air-test-now)))
        (should (string-match-p "always surfaces" line))))))

;;;; -------------------------------------------------------------------
;;;; r93-15 — the overlap the user chose to keep
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r93-15-attention-is-a-superset-of-high-priority ()
  "With `#A' at threshold 0, Needs attention CONTAINS High priority, always.
A consequence of two rulings meeting: High priority is exactly the `#A'
set, and `#A' has threshold 0, so every High-priority row is also a
Needs-attention row \u2014 permanently, not occasionally.  The user was shown
this and chose to keep the overlap (buckets are non-exclusive by
decision), so it is pinned here as an INVARIANT rather than left as a
surprise: if a later round excludes rows already shown above, or raises
the `#A' threshold above 0, this test is the one that must be re-blessed
and the conversation that must be had."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (let* ((items (org-air-query-items))
           (hipri nil) (attention nil))
      (dolist (item items)
        (let ((buckets (org-air-classify-item item org-air-test-now)))
          (when (memq 'high-priority buckets) (push item hipri))
          (when (memq 'attention buckets) (push item attention))))
      (should hipri)
      ;; every High-priority row is also a Needs-attention row…
      (dolist (item hipri)
        (should (memq item attention)))
      ;; …and the containment is STRICT on this corpus (two quiet rows
      ;; carry no cookie at all), so the sections are not simply equal.
      (should (< (length hipri) (length attention)))
      ;; the fixture board's numbers, as measured: 3 of the 5
      ;; Needs-attention rows are duplicates of the whole High priority
      ;; section.
      (should (= 3 (length hipri)))
      (should (= 5 (length attention))))))

;;;; -------------------------------------------------------------------
;;;; r93-16 — AUDIT FINDING (tripwire, not a blessing)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r93-16-audit-overdue-section-lost-its-date-sort ()
  "AUDIT FINDING (R93 audit seat): Overdue rows render in QUERY order.
This test does NOT bless the behaviour it pins \u2014 it exists so the
regression cannot hide inside a golden re-bless, and it is expected to
be DELETED (or inverted) by the round that fixes it.

WHAT: `org-air-view--sort-items' date-sorts only `(attention upcoming)'.
Before R93 the overdue rows lived inside `attention', so the board's
default `date' sort showed them WORST-FIRST.  R93 moved them into the
new `overdue' bucket and did not add it to that list, so the Overdue
section \u2014 the alarm section \u2014 now renders in file/query order.  Measured
on the standard fixture at width 120, against the parent commit:

  parent (R92 FIX)   Chase missing invoice  OVERDUE 7d
                     Fix production runbook OVERDUE 5d
                     Book dentist           OVERDUE 3d
  R93                Book dentist           OVERDUE 3d   <- least urgent first
                     Fix production runbook OVERDUE 5d
                     Chase missing invoice  OVERDUE 7d

WHY IT IS PINNED AND NOT FIXED: the audit seat may not edit production
`.el'.  The 28 regenerated board goldens now contain the query order, so
without this test the next reader would take it for the intended design.
FIX: add `overdue' to the `date' arm of `org-air-view--sort-items'; the
board goldens move again and this test must be re-blessed to assert
worst-first."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (let* ((items (org-air-query-items))
           (overdue (seq-filter
                     (lambda (it) (memq 'overdue (org-air-classify-item
                                                  it org-air-test-now)))
                     items)))
      (should (= 3 (length overdue)))
      (with-temp-buffer
        (org-air-view-mode)
        (let* ((sorted (org-air-view--sort-items overdue 'overdue))
               (dated (org-air-view--sort-items overdue 'attention)))
          ;; THE FINDING: the Overdue bucket is left in query order…
          (should (equal (mapcar #'org-air-item-title overdue)
                         (mapcar #'org-air-item-title sorted)))
          ;; …while the very same rows under the bucket they used to
          ;; live in ARE date-sorted, so this is a wiring gap and not a
          ;; missing capability.
          (should-not (equal (mapcar #'org-air-item-title sorted)
                             (mapcar #'org-air-item-title dated)))
          (should (equal '("Chase missing invoice"
                           "Fix production outage runbook"
                           "Book dentist appointment")
                         (mapcar #'org-air-item-title dated))))))))

(provide 'org-air-round93-test)
;;; org-air-round93-test.el ends here
