;;; org-air-round94-test.el --- R94: the measured-only clock + Untracked -*- lexical-binding: t; -*-

;;; Commentary:
;; Permanent coverage for the R94 planning surface — the round that took
;; the SOURCE FILE'S mtime out of the Needs-attention clock and gave the
;; work that removal would otherwise strand a home of its own.
;;
;; The rule the whole round turns on, stated once:
;;
;;   Any edit to a heading is an edit to its file, so the heading's last
;;   change is never LATER than the file's mtime.  The age derived from
;;   the mtime is therefore a sound LOWER BOUND — it is SOUND WHEN IT
;;   ACCUSES ("this file has not changed in 210 days, so this heading has
;;   been quiet at LEAST that long") and UNSOUND WHEN IT EXCUSES ("this
;;   file changed today, so this heading is fresh").  R93 used it for
;;   both.
;;
;; The excuse direction is what the R93 review measured: a `tasks.org'
;; edited today hid 15 of its 20 headings, org-air's OWN writes reset the
;; clock, a one-file setup was all-or-nothing, and recovery was a BURST
;; where ten headings arrived on the same day with the same number and
;; outranked every measured age.  R94 keeps the accusation, deletes the
;; excuse, and gives no-plan-no-record work the `untracked' bucket,
;; section and token.
;;
;; The ERTs below, one invariant each:
;;
;;   r94-1   FIX-1 unit: `--overdue-time' is the slot the row's own label
;;           chose, and the Overdue sort keys on it — the mixed-slot
;;           shape (overdue by SCHEDULE, deadline still FUTURE) that the
;;           FIX-2 law always claimed and no corpus contained.
;;   r94-2   FIX-1 end to end: the PAINTED Overdue cells are worst-first,
;;           on a real render, with the mixed-slot row present.
;;   r94-3   FIX-2: an INACTIVE `SCHEDULED:'/`DEADLINE:' value is a plan,
;;           not an update; `CLOSED:' still counts; keyword-by-keyword on
;;           one mixed planning line.
;;   r94-4   FIX-3: a DOUBLE-QUOTED stamp is the abandoned plan, never
;;           the log's own moment.
;;   r94-5   FIX-4: the rail's `~file' number IS the bucket's bound, and
;;           an on-disk edit with no rescan moves neither.
;;   r94-6   MEASURED-ONLY: org-air's own writes (`t', `b', refile,
;;           archive) cannot change who is in Needs attention.
;;   r94-7   MEASURED-ONLY: the time series — file edited today / 5d /
;;           31d give IDENTICAL section membership.  No silence, no burst.
;;   r94-8   THE COVERAGE THEOREM, as a property over a generated corpus.
;;   r94-9   the `untracked' predicate at its boundaries: any plan or any
;;           record takes a heading out of it.
;;   r94-10  `is:untracked' agrees with the section (the R72 law), parses,
;;           is offered, and is a STRICT SUBSET of `is:nodate'.
;;   r94-11  the section is CONDITIONAL: a board with nothing untracked
;;           renders exactly the R93 five.
;;   r94-12  the cap is 4, and the fold row carries the true count.
;;   r94-13  never attention-coloured, and the glyph is the quiet one.
;;   r94-14  `~' PROVENANCE: Untracked prints a marked bound or
;;           `no history'.
;;   r94-15  Needs attention prints NO estimates at all — every number in
;;           that section is a measured heading fact.
;;   r94-16  `--sort-by-floor': longest bound first, `no history' last.
;;   r94-17  `--sort-by-quiet' breaks ties toward MEASURED, dormant under
;;           the defaults and still correct.
;;   r94-18  section ORDER and the Summary row: Untracked last of the
;;           task sections, above Notes and Backlog.
;;   r94-19  the rail inspector names the bucket and its reason.
;;   r94-20  `--first-actionable-item' reaches Untracked only when every
;;           fixed section is empty.
;;   r94-21  the floor is a HASH LOOKUP: classify still does zero I/O.
;;   r94-22  cache v7 -> v8: a v7 record is a clean cold miss.

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

;;;; -------------------------------------------------------------------
;;;; Scaffolding
;;;; -------------------------------------------------------------------

(defvar org-air-r94--dir nil
  "Temp corpus directory of the running round-94 test.")

(defconst org-air-r94--outside-file "/tmp/org-air-r94-outside.org"
  "A FICTION: a path with no entry in the scan's file-meta table.
An item carrying it has no `updated' slot AND no mtime floor, which is
the R94 definition of a number org-air simply does not have.")

(defun org-air-r94--path (name)
  "Return corpus file NAME's absolute path."
  (expand-file-name name org-air-r94--dir))

(defun org-air-r94--epoch (days)
  "Return the epoch integer DAYS from the frozen now (negative = past)."
  (floor (float-time (time-add org-air-test-now (days-to-time days)))))

(defun org-air-r94--date (days)
  "Return an ACTIVE Org date DAYS from the frozen now (a PLAN)."
  (format-time-string "<%Y-%m-%d %a>"
                      (time-add org-air-test-now (days-to-time days))))

(defun org-air-r94--stamp (days)
  "Return an INACTIVE Org stamp DAYS from the frozen now (a RECORD)."
  (format-time-string "[%Y-%m-%d %a 09:00]"
                      (time-add org-air-test-now (days-to-time days))))

(defun org-air-r94--timestamp (days)
  "Return an Org timestamp OBJECT DAYS from the frozen now."
  (org-timestamp-from-string (org-air-r94--date days)))

(cl-defun org-air-r94--item (&key (title "R94 probe") priority updated
                                  scheduled deadline file tags (todo "TODO")
                                  active-ts)
  "Build a cache-hydrated-shape `org-air-item' for the R94 rules.
PRIORITY is a priority LETTER; UPDATED / SCHEDULED / DEADLINE /
ACTIVE-TS are day offsets from the frozen now.  FILE defaults to
`org-air-r94--outside-file' (see its docstring).

R95: ACTIVE-TS fills BOTH the subtree `active-ts' slot and the heading's
OWN `own-active-ts', which is the shape a real scan produces for a bare
`<timestamp>' written in a heading's own body — the plan SPELLING the
R94 generator had no cell for."
  (org-air-item-create
   :title title
   :tags tags
   :file (or file org-air-r94--outside-file)
   :marker (cons (or file org-air-r94--outside-file) 1)
   :kind 'heading
   :todo todo
   :priority (and priority (* 1000 (- org-priority-lowest priority)))
   :updated (and updated (org-air-r94--epoch updated))
   :active-ts (and active-ts
                   (float-time (time-add org-air-test-now
                                         (days-to-time active-ts))))
   :own-active-ts (and active-ts
                       (float-time (time-add org-air-test-now
                                             (days-to-time active-ts))))
   :scheduled (and scheduled (org-air-r94--timestamp scheduled))
   :deadline (and deadline (org-air-r94--timestamp deadline))))

(defmacro org-air-r94--with-corpus (specs &rest body)
  "Write SPECS into a fresh temp corpus and run BODY against it."
  (declare (indent 1) (debug t))
  `(let ((org-air-r94--dir (make-temp-file "org-air-r94-" t)))
     (unwind-protect
         (progn
           (when (fboundp 'org-air-query-teardown)
             (org-air-query-teardown)
             (clrhash org-air-query--file-meta)
             (clrhash org-air-query--visits)
             (clrhash org-air-query--denote-id-index))
           (pcase-dolist (`(,name . ,content) ,specs)
             (let ((path (expand-file-name name org-air-r94--dir))
                   (coding-system-for-write 'utf-8-unix)
                   (file-name-handler-alist nil))
               (make-directory (file-name-directory path) t)
               (write-region (or content "") nil path nil 'silent)))
           (let ((org-air-files (list org-air-r94--dir))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-r94--dir))
                 (org-air-cache-file
                  (expand-file-name ".cache/board.eld" org-air-r94--dir))
                 (org-air-view-buffer-name "*org-air-r94*")
                 (org-air-plain-heading-type 'task)
                 (create-lockfiles nil)
                 (inhibit-message t))
             ,@body))
       (when (fboundp 'org-air-query-teardown) (org-air-query-teardown))
       (let ((kill-buffer-query-functions nil))
         (when (get-buffer "*org-air-r94*") (kill-buffer "*org-air-r94*"))
         (dolist (buf (buffer-list))
           (let ((fn (buffer-file-name buf)))
             (when (and fn (string-prefix-p org-air-r94--dir fn))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r94--dir t))))

(defmacro org-air-r94--render-board (size &rest body)
  "Render the REAL board over the bound corpus at SIZE; run BODY in its buffer.
SIZE is (WIDTH . HEIGHT).  Frozen clock, anti-tautology render guards
active, board buffer killed afterwards."
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

(defun org-air-r94--scanned (title &optional items)
  "Return the scanned item whose title contains TITLE."
  (let ((item (org-air-test-find-item title (or items (org-air-query-items)))))
    (should item)
    item))

(defun org-air-r94--rendered-sections ()
  "Return an alist (BUCKET . ROWS) for the board in the current buffer.
ROWS are in RENDER order; each is (TITLE CELL ITEM), where CELL is the
row's own painted span.  Property-driven, so chrome can never be
mistaken for a row."
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
            (push (list (org-air-item-title item) span item)
                  (cdr (assq bucket out)))
            (setq pos end)))
         (t (setq pos (1+ pos))))))
    (mapcar (lambda (entry) (cons (car entry) (nreverse (cdr entry))))
            (nreverse out))))

(defun org-air-r94--rows (bucket)
  "Return the rendered rows of BUCKET in the current board buffer."
  (cdr (assq bucket (org-air-r94--rendered-sections))))

(defun org-air-r94--cell-number (span)
  "Return the INTEGER a rendered row SPAN prints, or nil.
`OVERDUE 40d' => 40; `~210d quiet' => 210; `273d quiet' => 273."
  (cond ((string-match "OVERDUE \\([0-9]+\\)d" span)
         (string-to-number (match-string 1 span)))
        ((string-match "~?\\([0-9]+\\)d quiet" span)
         (string-to-number (match-string 1 span)))))

(defun org-air-r94--buckets-of (items)
  "Return an alist (TITLE . BUCKETS) over ITEMS at the frozen now."
  (mapcar (lambda (item)
            (cons (org-air-item-title item)
                  (org-air-classify-item item org-air-test-now)))
          items))

(defun org-air-r94--members (bucket &optional items)
  "Return the sorted TITLES of BUCKET's members among ITEMS."
  (sort (delq nil
              (mapcar (lambda (item)
                        (and (memq bucket (org-air-classify-item
                                           item org-air-test-now))
                             (org-air-item-title item)))
                      (or items (org-air-query-items))))
        #'string<))

(defun org-air-r94--section-headings ()
  "Return (BUCKET BADGE ATTENTION-FACED-P) for every rendered section heading.
Property-driven: a heading is a run of `org-air-count-badge', and
ATTENTION-FACED-P is whether `org-air-face-count-attention' is applied
anywhere inside it — i.e. whether that section's badge SHOUTS."
  (let ((pos (point-min)) (out ()))
    (while (setq pos (text-property-not-all pos (point-max)
                                            'org-air-count-badge nil))
      (let* ((end (or (next-single-property-change pos 'org-air-count-badge)
                      (point-max)))
             (bucket (get-text-property pos 'org-air-section))
             (badge (get-text-property pos 'org-air-count-badge))
             (faced nil)
             (p pos))
        (while (< p end)
          (let ((face (get-text-property p 'face)))
            (when (memq 'org-air-face-count-attention
                        (if (listp face) face (list face)))
              (setq faced t)))
          (setq p (or (next-single-property-change p 'face nil end) end)))
        (push (list bucket badge faced) out)
        (setq pos end)))
    (nreverse out)))

(defun org-air-r94--fold-counts ()
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

;;;; -------------------------------------------------------------------
;;;; r94-1 — FIX-1: the Overdue key is the slot the LABEL chose
;;;; -------------------------------------------------------------------

(defconst org-air-r94--mixed-slot-shapes
  '(("A schedule slipped"        -4  nil)
    ("B deadline just missed"    nil  -2)
    ("C late by SCHEDULE only"   -40   5)
    ("D deadline three weeks"    nil -20)
    ("E deadline a month"        nil -30))
  "The mixed-slot Overdue corpus: (TITLE SCHEDULED-OFFSET DEADLINE-OFFSET).
Row C is the shape the FIX-2 law always claimed and no corpus in the
suite contained — overdue by its SCHEDULE (40 days) while its DEADLINE
is still in the FUTURE (+5).  `org-air-view--item-sort-time' keys on
\(or deadline scheduled) unconditionally, so C sorted by a future date:
LAST, printing the biggest number in the section, where the 6-row cap
can hide it behind \"and N more\".")

(defun org-air-r94--mixed-slot-items ()
  "Return the mixed-slot Overdue probes, in the order written above."
  (mapcar (pcase-lambda (`(,title ,sched ,dl))
            (org-air-r94--item :title title :scheduled sched :deadline dl))
          org-air-r94--mixed-slot-shapes))

(ert-deftest org-air-r94-1-overdue-key-is-the-slot-the-label-chose ()
  "The Overdue sort keys on the slot its own row LABEL chose (R94 FIX-1).
THE HOLE IN THE FIX-2 LAW.  R93 FIX-2 elevated \"each section is ordered
worst-first by the very number its own rows print\" to a stated rule and
`r93-21' claims to fence it — but no corpus in the suite carried the one
shape where the two disagree: a heading overdue by its SCHEDULE while
its DEADLINE is still in the future.  `org-air-view--date-label' prints
`OVERDUE Nd' from whichever slot is actually PAST (deadline first);
`org-air-view--item-sort-time' keyed on `(or deadline scheduled)'
unconditionally.  Measured through the real renderer by the R93 review:

  main (R93)  OVERDUE 30d  OVERDUE 20d  OVERDUE 4d  OVERDUE 2d  OVERDUE 40d
  R94         OVERDUE 40d  OVERDUE 30d  OVERDUE 20d  OVERDUE 4d  OVERDUE 2d

The worst row in the ALARM section sorted LAST.  R94 hoists the label's
own arm order into `org-air-view--overdue-time' and keys the sort on it,
so the law is true by construction rather than by coincidence of slot
order.

Four legs: the helper's own arm order; the label composed FROM it; the
sorted order; and the ANTI-TAUTOLOGY — the pre-R94 key
\(`--item-sort-time', still shipped and still right for Upcoming) is
asserted to produce a DIFFERENT order on this very corpus, so a sort
that silently stopped keying on the bucket cannot pass by accident."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test--with-frozen-now
    (let* ((items (org-air-r94--mixed-slot-items))
           (by-title (lambda (title)
                       (seq-find (lambda (i)
                                   (string-prefix-p title
                                                    (org-air-item-title i)))
                                 items)))
           (mixed (funcall by-title "C"))
           (plain (funcall by-title "D")))
      ;; 1. the helper: the PAST slot, deadline first.
      (should (time-equal-p
               (org-air-view--overdue-time mixed org-air-test-now)
               (org-air-view--timestamp-time (org-air-item-scheduled mixed))))
      (should (time-equal-p
               (org-air-view--overdue-time plain org-air-test-now)
               (org-air-view--timestamp-time (org-air-item-deadline plain))))
      ;; a heading with NO past slot has no overdue time at all.
      (should-not (org-air-view--overdue-time
                   (org-air-r94--item :title "future" :deadline 5)
                   org-air-test-now))
      ;; 2. the LABEL is composed from that same slot.
      (should (equal '("OVERDUE 4d" "OVERDUE 2d" "OVERDUE 40d"
                       "OVERDUE 20d" "OVERDUE 30d")
                     (mapcar (lambda (i)
                               (car (org-air-view--date-label i 'overdue)))
                             items)))
      (with-temp-buffer
        (org-air-view-mode)
        ;; 3. the SORT is worst-first by that same number.
        (let* ((sorted (org-air-view--sort-items items 'overdue))
               (labels (mapcar (lambda (i)
                                 (car (org-air-view--date-label i 'overdue)))
                               sorted))
               (numbers (mapcar #'org-air-r94--cell-number labels)))
          (should (equal '("C late by SCHEDULE only" "E deadline a month"
                           "D deadline three weeks" "A schedule slipped"
                           "B deadline just missed")
                         (mapcar #'org-air-item-title sorted)))
          (should (equal '(40 30 20 4 2) numbers))
          (should (equal numbers (sort (copy-sequence numbers) #'>)))
          ;; 4. ANTI-TAUTOLOGY: the pre-R94 key really does disagree here.
          (let ((old (mapcar #'org-air-item-title
                             (org-air-view--sort-by-date items))))
            (should-not (equal old (mapcar #'org-air-item-title sorted)))
            ;; and it is exactly the defect the review measured: the
            ;; worst row LAST.
            (should (equal "C late by SCHEDULE only" (car (last old))))))
        ;; The bucket argument is what selects the key: Upcoming (and
        ;; every other caller) keeps `--item-sort-time' verbatim.
        (should (equal (mapcar #'org-air-item-title
                               (org-air-view--sort-by-date items))
                       (mapcar #'org-air-item-title
                               (org-air-view--sort-by-date items 'upcoming))))
        (should-not (equal (mapcar #'org-air-item-title
                                   (org-air-view--sort-by-date items))
                           (mapcar #'org-air-item-title
                                   (org-air-view--sort-by-date
                                    items 'overdue))))))))

;;;; -------------------------------------------------------------------
;;;; r94-2 — FIX-1 end to end: the PAINTED cells are worst-first
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r94-2-overdue-paints-worst-first-with-a-mixed-slot-row ()
  "On a REAL render the Overdue cells descend, mixed-slot row included.
The end-to-end half of r94-1: not the comparator in isolation but the
numbers a user actually reads, off a board painted by the real renderer
from a real scanned corpus.  The mixed-slot heading carries BOTH slots
with only the scheduled one past, so a renderer that keyed the alarm
section on `(or deadline scheduled)' paints its biggest number last.

The section's own cap makes this load-bearing rather than cosmetic: with
six rows and a 6-row budget the worst row merely looked wrong, but one
more overdue row and it would be BEHIND the fold, which is the exact
failure R93 FIX-2 existed to remove."
  (skip-unless (locate-library "org-air"))
  (org-air-r94--with-corpus
      (list
       (cons "tasks.org"
             (concat
              "* TODO A schedule slipped\nSCHEDULED: " (org-air-r94--date -4)
              "\n" (org-air-r94--stamp -1) "\n"
              "* TODO B deadline just missed\nDEADLINE: " (org-air-r94--date -2)
              "\n" (org-air-r94--stamp -1) "\n"
              ;; ONE planning line: Org parses only the line directly
              ;; under the heading, so a `DEADLINE:' on a second line is
              ;; inert and this corpus would silently lose the shape it
              ;; exists for.  Both slots, one line, as Org writes them.
              "* TODO C late by SCHEDULE only\nSCHEDULED: "
              (org-air-r94--date -40) " DEADLINE: " (org-air-r94--date 5)
              "\n" (org-air-r94--stamp -1) "\n"
              "* TODO D deadline three weeks\nDEADLINE: " (org-air-r94--date -20)
              "\n" (org-air-r94--stamp -1) "\n"
              "* TODO E deadline a month\nDEADLINE: " (org-air-r94--date -30)
              "\n" (org-air-r94--stamp -1) "\n"))
       (cons "inbox.org" "#+title: inbox\n"))
    (org-air-r94--render-board '(120 . 70)
      (let* ((rows (org-air-r94--rows 'overdue))
             (numbers (mapcar (lambda (row)
                                (org-air-r94--cell-number (nth 1 row)))
                              rows)))
        (ert-info ((format "overdue rows: %S"
                           (mapcar (lambda (r) (cons (car r) (nth 1 r))) rows)))
          ;; the corpus really did produce five overdue rows...
          (should (= 5 (length rows)))
          ;; ...the mixed-slot row is one of them, and it really does
          ;; carry BOTH slots after the scan (a `DEADLINE:' written on a
          ;; second line would be inert, and the shape would evaporate).
          (should (member "C late by SCHEDULE only" (mapcar #'car rows)))
          (let ((mixed (nth 2 (assoc "C late by SCHEDULE only" rows))))
            (should (org-air-item-scheduled mixed))
            (should (org-air-item-deadline mixed))
            (should (time-less-p org-air-test-now
                                 (org-air-view--timestamp-time
                                  (org-air-item-deadline mixed)))))
          (should (equal '(40 30 20 4 2) numbers))
          ;; ...and it leads the section, which is the whole point.
          (should (equal "C late by SCHEDULE only" (car (car rows))))
          ;; the law, stated as a law: strictly descending.
          (should (equal numbers (sort (copy-sequence numbers) #'>))))))))

;;;; -------------------------------------------------------------------
;;;; r94-3 — FIX-2: an inactive planning stamp is a PLAN
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r94-3-inactive-plan-stamps-never-move-the-clock ()
  "An INACTIVE `SCHEDULED:'/`DEADLINE:' value is a plan; `CLOSED:' is a record.
Org accepts a planning date written with brackets
\(`DEADLINE: [2026-06-14 Sun]'), and `org-ts-regexp-inactive' matches it
exactly like a log stamp.  R93 therefore read ONE stamp as BOTH a
deadline (the row went to Overdue) and an update (age 1) — while the R93
design and README both claimed the exclusion was MECHANICAL: \"a
SCHEDULED, a DEADLINE or a bare plan date can never move this clock\".
That was true only of the `<...>' spelling.

R94's `org-air-query--plan-stamp-p' resolves it KEYWORD BY KEYWORD: the
nearest planning keyword to the LEFT of the match on the same line owns
the stamp.  `SCHEDULED:'/`DEADLINE:' own it (a plan — skip);
`CLOSED:' owns it (a completion — keep, it is the one planning keyword
that records something that HAPPENED); no keyword means an ordinary body
or LOGBOOK stamp (keep).

Four rows, exactly the review's table, plus the two directions that
prove the guard is a guard and not a blanket:

  DEADLINE: [..]                       main 1   R94 UNKNOWN
  SCHEDULED: [..]                      main 1   R94 UNKNOWN
  CLOSED: [..]                         main 1   R94 1
  CLOSED: [..] DEADLINE: [..]          main 1   R94 14  (keyword by keyword)

Both inactive-plan rows stay in Overdue, as they should: they simply
stop claiming to be updates."
  (skip-unless (locate-library "org-air"))
  (org-air-r94--with-corpus
      '(("tasks.org" . "\
* TODO Inactive deadline is a plan
DEADLINE: [2026-06-14 Sun]
* TODO Inactive schedule is a plan
SCHEDULED: [2026-06-14 Sun]
* TODO Closed is a record
CLOSED: [2026-06-14 Sun 09:00]
* TODO Mixed planning line
CLOSED: [2026-06-01 Mon 09:00] DEADLINE: [2026-06-14 Sun]
* TODO Ordinary body stamp
[2026-06-14 Sun 09:00]
* TODO Logbook state change
:LOGBOOK:
- State \"TODO\" from \"WAIT\" [2026-06-14 Sun 09:00]
:END:
")
        ("inbox.org" . "#+title: inbox\n"))
    (let ((items (org-air-query-items)))
      (pcase-dolist (`(,title . ,expected)
                     '(("Inactive deadline is a plan" . nil)
                       ("Inactive schedule is a plan" . nil)
                       ("Closed is a record" . 1)
                       ("Mixed planning line" . 14)
                       ("Ordinary body stamp" . 1)
                       ("Logbook state change" . 1)))
        (let ((item (org-air-r94--scanned title items)))
          (ert-info ((format "%s => %S" title
                             (org-air-classify-quiet-days
                              item org-air-test-now)))
            (should (equal expected
                           (org-air-classify-quiet-days
                            item org-air-test-now))))))
      ;; The plan rows are still PLANS: they reach Overdue on the very
      ;; stamp that no longer moves their clock.  (The whole point: one
      ;; stamp, ONE meaning, and it is the planning keyword's.)
      (dolist (title '("Inactive deadline is a plan"
                       "Inactive schedule is a plan"))
        (let ((item (org-air-r94--scanned title items)))
          (should (memq 'overdue (org-air-classify-item
                                  item org-air-test-now)))
          ;; ...and having a plan, they are not untracked either.
          (should-not (org-air-classify--untracked-p item))))
      ;; The guard is a GUARD, not a blanket: the same bracket spelling
      ;; in an ordinary body line, in a LOGBOOK entry and after
      ;; `CLOSED:' all still count.
      (dolist (title '("Closed is a record" "Ordinary body stamp"
                       "Logbook state change"))
        (should (eq 'measured
                    (org-air-classify-updated-source
                     (org-air-r94--scanned title items))))))))

;;;; -------------------------------------------------------------------
;;;; r94-4 — FIX-3: a quoted stamp is the ABANDONED plan
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r94-4-quoted-stamps-are-never-the-clock-witness ()
  "The old date quoted inside a reschedule log line is not that log's moment.
Org's DEFAULT `org-log-note-headings' quote the OLD value with `%S':

  - Rescheduled from \"[2026-06-14 Sun]\" on [2026-06-01 Mon 08:00]
  - New deadline from \"[2026-06-13 Sat]\" on [2026-05-01 Fri 08:00]

The quoted date is the PLAN that was abandoned, not the moment the note
was written — and it is NEWER than the log's own stamp whenever a task
is moved EARLIER, which is the common direction.  The naive newest-wins
walk therefore read the first line above as age 1 instead of 14, i.e.
it dated the heading by a plan the user had just thrown away.

`org-air-query--quoted-stamp-p' states the rule as PUNCTUATION rather
than wording — a stamp wrapped in double quotes is never this clock's
witness — so it is independent of the note-heading format string
\(`%s'/`%S' are quoted in every default heading, `%t' never is).

The unquoted twin on the same line is asserted to still count, so the
guard cannot be passing by refusing the whole line."
  (skip-unless (locate-library "org-air"))
  (org-air-r94--with-corpus
      '(("tasks.org" . "\
* TODO Rescheduled earlier
:LOGBOOK:
- Rescheduled from \"[2026-06-14 Sun]\" on [2026-06-01 Mon 08:00]
:END:
* TODO Redeadlined earlier
:LOGBOOK:
- New deadline from \"[2026-06-13 Sat]\" on [2026-05-01 Fri 08:00]
:END:
* TODO Rescheduled later
:LOGBOOK:
- Rescheduled from \"[2026-04-01 Wed]\" on [2026-06-14 Sun 08:00]
:END:
* TODO Quoted alone
:LOGBOOK:
- Note taken on \"[2026-06-14 Sun 08:00]\"
:END:
")
        ("inbox.org" . "#+title: inbox\n"))
    (let ((items (org-air-query-items)))
      (pcase-dolist (`(,title . ,expected)
                     '(;; moved EARLIER: the quoted plan is NEWER and must lose
                       ("Rescheduled earlier" . 14)
                       ("Redeadlined earlier" . 45)
                       ;; moved LATER: the log's own stamp was already the
                       ;; newest, so the answer is unchanged from R93 —
                       ;; the guard costs nothing where it is not needed
                       ("Rescheduled later" . 1)
                       ;; nothing BUT a quoted stamp: no witness at all,
                       ;; and org-air invents none
                       ("Quoted alone" . nil)))
        (let ((item (org-air-r94--scanned title items)))
          (ert-info ((format "%s => %S" title
                             (org-air-classify-quiet-days
                              item org-air-test-now)))
            (should (equal expected (org-air-classify-quiet-days
                                     item org-air-test-now))))))
      ;; The UNQUOTED stamp on the very same line is still the witness —
      ;; the guard skips a stamp, never a line.
      (should (eq 'measured
                  (org-air-classify-updated-source
                   (org-air-r94--scanned "Rescheduled earlier" items))))
      ;; ...and a heading whose ONLY stamp is quoted has no record, so
      ;; it lands in the section for exactly that.
      (should (org-air-classify--untracked-p
               (org-air-r94--scanned "Quoted alone" items))))))

;;;; -------------------------------------------------------------------
;;;; r94-5 — FIX-4: the rail number IS the bucket number
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r94-5-rail-and-bucket-quote-one-file-number ()
  "One number everywhere for the FILE path too (R94 FIX-4, decision 13).
Classify read the SCAN-time `:mtime' out of `org-air-query-file-meta';
the rail's `~file' Updated line did a LIVE `file-attributes' stat.  Two
different facts, and the R93 review measured them disagreeing:

  AFTER SCAN (mtime 2026-01-01)   bucket 165d   rail 165d
  AFTER an on-disk edit, NO rescan bucket 165d  rail   1d

Decision 13 says the number the inspector shows and the number the
bucket used cannot drift.  It held for the slot path and not for this
one.  R94 points the fallback at `org-air-classify-updated-floor', so
both numbers come from the same hash lookup and both learn at the same
moment — the next rescan.

Asserted on a REAL BOARD, through the rendered row cell as well as the
rail line, because those are the two places the user reads it: the row's
`~210d quiet', the rail's `(210d ago \u00b7 ~file)' and
`org-air-classify-quiet-floor-days' are ONE number before and after an
on-disk edit that no rescan has seen."
  (skip-unless (locate-library "org-air"))
  (org-air-r94--with-corpus
      '(("tasks.org" . "* TODO Historyless and dateless\nJust prose.\n")
        ("inbox.org" . "#+title: inbox\n"))
    (set-file-times (org-air-r94--path "tasks.org")
                    (time-subtract org-air-test-now (days-to-time 210)))
    (org-air-r94--render-board '(120 . 70)
      (let* ((rows (org-air-r94--rows 'untracked))
             (row (car rows))
             (item (nth 2 row)))
        (should (= 1 (length rows)))
        (should (equal "Historyless and dateless" (car row)))
        ;; the ROW prints the marked bound...
        (should (string-match-p "~210d quiet" (nth 1 row)))
        (should (= 210 (org-air-r94--cell-number (nth 1 row))))
        ;; ...the BUCKET's own reader answers the same number...
        (should (= 210 (org-air-classify-quiet-floor-days
                        item org-air-test-now)))
        (should (eq 'file (org-air-classify-updated-source item)))
        ;; ...and so does the RAIL, with no live stat at all.
        (let ((stats 0)
              (orig (symbol-function 'file-attributes))
              line)
          (cl-letf (((symbol-function 'file-attributes)
                     (lambda (&rest args) (cl-incf stats) (apply orig args))))
            (setq line (substring-no-properties
                        (org-air-view--item-updated-line
                         item "" org-air-test-now))))
          (should (string-match-p "(210d ago \u00b7 ~file)" line))
          (should (= 0 stats))
          ;; NO DRIFT: touch the file on disk, do NOT rescan.  Every one
          ;; of the three numbers must stay put — they only ever learn
          ;; together, at the next scan.
          (set-file-times (org-air-r94--path "tasks.org")
                          (time-subtract org-air-test-now (days-to-time 1)))
          (should (= 210 (org-air-classify-quiet-floor-days
                          item org-air-test-now)))
          (should (equal line
                         (substring-no-properties
                          (org-air-view--item-updated-line
                           item "" org-air-test-now))))
          (should (equal (cons "~210d quiet" 'org-air-face-date)
                         (org-air-view--untracked-reason
                          item org-air-test-now))))))))

;;;; -------------------------------------------------------------------
;;;; r94-6 — MEASURED-ONLY: org-air's own writes change nobody
;;;; -------------------------------------------------------------------

(defconst org-air-r94--amplifier-corpus
  (concat
   "#+title: tasks\n\n"
   ;; three MEASURED-quiet headings: Needs attention, on their own clocks
   "* TODO Quiet alpha\n[2026-01-05 Mon 09:00]\n"
   "* TODO Quiet bravo\n[2026-01-06 Tue 09:00]\n"
   "* TODO Quiet charlie\n[2026-01-07 Wed 09:00]\n"
   ;; three historyless, dateless headings: Untracked
   "* TODO Bare delta\nNo dates, no history.\n"
   "* TODO Bare echo\nNo dates, no history.\n"
   "* TODO Bare foxtrot\nNo dates, no history.\n"
   ;; the row org-air will be told to act on
   "* TODO Victim golf\n[2026-01-08 Thu 09:00]\n")
  "The R93 review's AMPLIFIER shape, in one file.
Every heading lives in ONE file, which is the common org-mode GTD setup
\(`tasks.org', `gtd.org', `todo.org').  Under R93 acting on ANY row here
rewrote the file, bumped its mtime, and re-hid every HISTORYLESS heading
in it for another full threshold — \"acting on one row silences every
other\", the amplifier the R93 design never named.")

(ert-deftest org-air-r94-6-org-airs-own-writes-cannot-change-attention ()
  "`t', `b', refile and archive cannot change WHO is in Needs attention.
THE AMPLIFIER, REMOVED BY CONSTRUCTION.  Under R93 the attention clock
fell back to the source file's mtime, and org-air's own writes are edits
to that file — so pressing `t' on one row re-hid every historyless
heading in the same file for another full threshold, and pressing it
again reset the clock again.  The R90 harness had to pin
`org-air-attention-days' to `((nil . 0))' for exactly this reason; the
discovery reached a test comment and never the README.

R94 removes it rather than tracking it: membership is decided by each
heading's OWN recorded history, so a write to a NEIGHBOUR is not a fact
about this heading and cannot move it.

The test drives the REAL commands over a REAL board and rescans between
each, comparing the FULL bucket map of every untouched heading.  Two
anti-vacuity legs make the comparison mean something: the file's mtime
is asserted to have really jumped past the scan's, and the corpus is
asserted to contain both a Needs-attention and an Untracked population
before the first write."
  (skip-unless (locate-library "org-air"))
  (dolist (verb '(done backlog archive))
    (ert-info ((format "verb %S" verb))
      (org-air-r94--with-corpus
          (list (cons "tasks.org" org-air-r94--amplifier-corpus)
                (cons "inbox.org" "#+title: inbox\n"))
        ;; A believable working file: last touched a season ago.
        (set-file-times (org-air-r94--path "tasks.org")
                        (time-subtract org-air-test-now (days-to-time 210)))
        (org-air-r94--render-board '(120 . 70)
          (let* ((subject "Victim golf")
                 (untouched (lambda (map)
                              (seq-remove (lambda (e) (equal (car e) subject))
                                          map)))
                 (before (funcall untouched
                                  (org-air-r94--buckets-of org-air-view--items)))
                 (mtime-before (file-attribute-modification-time
                                (file-attributes
                                 (org-air-r94--path "tasks.org")))))
            ;; anti-vacuity: BOTH populations are present among the rows
            ;; org-air is NOT about to touch, so the comparison below has
            ;; something real to compare.
            (should (= 3 (seq-count (lambda (e) (memq 'attention (cdr e)))
                                    before)))
            (should (= 3 (seq-count (lambda (e) (memq 'untracked (cdr e)))
                                    before)))
            ;; org-air writes the file, through its own command.
            (goto-char (point-min))
            (let ((found nil))
              (while (and (not found) (not (eobp)))
                (when-let* ((it (org-air-view--row-property 'org-air-item))
                            ((equal subject (org-air-item-title it))))
                  (setq found t))
                (unless found (forward-line 1)))
              (should found))
            (org-air-view--goto-row-title)
            (pcase verb
              ('done (org-air-item-done))
              ('backlog (org-air-item-backlog))
              ('archive (org-air-item-archive)))
            ;; anti-vacuity: the write really did bump the mtime past the
            ;; 210-day scan value, i.e. the R93 amplifier really was armed.
            (let ((mtime-after (file-attribute-modification-time
                                (file-attributes
                                 (org-air-r94--path "tasks.org")))))
              (should (time-less-p mtime-before mtime-after))
              (should (time-less-p (time-subtract org-air-test-now
                                                  (days-to-time 1))
                                   mtime-after)))
            ;; a FRESH scan, so the new mtime is genuinely in play.
            (org-air-query-teardown)
            (let ((after (funcall untouched
                                  (org-air-r94--buckets-of
                                   (org-air-query-items)))))
              (ert-info ((format "before=%S after=%S" before after))
                (should (equal before after))))))))))

;;;; -------------------------------------------------------------------
;;;; r94-7 — MEASURED-ONLY: no silence, no burst
;;;; -------------------------------------------------------------------

(defconst org-air-r94--time-series-corpus
  (concat
   "#+title: tasks\n\n"
   (mapconcat (lambda (n)
                (format "* TODO Worked %d\n[2026-06-13 Sat 09:00]\n" n))
              (number-sequence 1 5) "")
   (mapconcat (lambda (n)
                (format "* TODO Quiet %d\n[2026-03-01 Sun 09:00]\n" n))
              (number-sequence 1 5) "")
   (mapconcat (lambda (n) (format "* TODO Bare %d\nNo dates, no history.\n" n))
              (number-sequence 1 10) ""))
  "The R93 review's time-series corpus: 20 headings in ONE file.
Five actively worked (2 days quiet), five genuinely quiet (106 days),
ten bare and historyless.  Read across three file ages it is the whole
R93 failure mode in one table:

  file edited TODAY    attention 5   in NO section 15
  file edited 5d ago   attention 5   in NO section 15
  file edited 31d ago  attention 15  in NO section 5

Silence, silence, then a BURST — ten headings arriving on the same day
with the same number, outranking every measured age.")

(ert-deftest org-air-r94-7-file-age-moves-nothing-no-silence-no-burst ()
  "The same 20 headings classify IDENTICALLY at three file ages.
The R93 review read its three corpora as a TIME SERIES for one user and
found the round's worst behaviour: while the file is being worked in,
every historyless heading is invisible; the day the whole file goes
quiet, all ten arrive at once with the same number and outrank
everything measured.  For a one-file user — `tasks.org', `gtd.org',
`todo.org', the canonical names — the floor was not coarse, it was
BINARY.

R94's answer is that the file clock moves nothing at all.  This test is
that sentence as an assertion: the same bytes, scanned three times with
only the FILE'S mtime differing (today / 5 days / 31 days — the review's
own three rows), must produce the IDENTICAL bucket map, the identical
per-section counts and the identical Needs-attention ORDER.

The floor is asserted to genuinely differ across the three, so the
equality above is an invariance and not an artefact of nothing moving."
  (skip-unless (locate-library "org-air"))
  (let (maps counts orders floors)
    (dolist (age '(0 5 31))
      (org-air-r94--with-corpus
          (list (cons "tasks.org" org-air-r94--time-series-corpus)
                (cons "inbox.org" "#+title: inbox\n"))
        (set-file-times (org-air-r94--path "tasks.org")
                        (time-subtract org-air-test-now (days-to-time age)))
        (let* ((items (org-air-query-items))
               (bare (org-air-r94--scanned "Bare 1" items)))
          (push (sort (mapcar (lambda (e) (format "%s=%S" (car e) (cdr e)))
                              (org-air-r94--buckets-of items))
                      #'string<)
                maps)
          (push (list (length (org-air-r94--members 'attention items))
                      (length (org-air-r94--members 'untracked items))
                      (length (org-air-r94--members 'overdue items)))
                counts)
          (push (org-air-viewport-test--with-frozen-now
                  (with-temp-buffer
                    (org-air-view-mode)
                    (mapcar #'org-air-item-title
                            (org-air-view--sort-items
                             (seq-filter
                              (lambda (i)
                                (memq 'attention (org-air-classify-item
                                                  i org-air-test-now)))
                              items)
                             'attention))))
                orders)
          (push (org-air-classify-quiet-floor-days bare org-air-test-now)
                floors))))
    ;; The three runs are indistinguishable, in every way a user sees.
    (should (equal (nth 0 maps) (nth 1 maps)))
    (should (equal (nth 1 maps) (nth 2 maps)))
    (should (equal (nth 0 counts) (nth 1 counts)))
    (should (equal (nth 1 counts) (nth 2 counts)))
    (should (equal (nth 0 orders) (nth 1 orders)))
    (should (equal (nth 1 orders) (nth 2 orders)))
    ;; The measured populations really are what the review measured:
    ;; 5 quiet, 10 untracked, ALWAYS — never 5 then 15.
    (should (equal '(5 10 0) (car counts)))
    ;; ANTI-VACUITY: the file clock genuinely differed across the three
    ;; runs; it simply decided nothing.  (Reversed: 31, 5, 0.)
    (should (equal '(31 5 0) floors))))

;;;; -------------------------------------------------------------------
;;;; r94-8 — THE COVERAGE THEOREM, as a property
;;;; -------------------------------------------------------------------

(defconst org-air-r94--plan-shapes
  '((none         nil  nil  nil)
    (sched-past    -5  nil  nil)
    (sched-today    0  nil  nil)
    (sched-soon     3  nil  nil)
    (sched-far     30  nil  nil)
    (dl-past      nil   -5  nil)
    (dl-today     nil    0  nil)
    (dl-soon      nil    3  nil)
    (dl-far       nil   30  nil)
    (both-mixed   -40    5  nil)
    (both-far      30   45  nil)
    (body-ts-past nil  nil   -5)
    (body-ts-soon nil  nil    1)
    (body-ts-far  nil  nil   92))
  "Every PLAN a heading can carry, by SPELLING and by DISTANCE.
Each row is (NAME SCHEDULED-OFFSET DEADLINE-OFFSET ACTIVE-TS-OFFSET);
offsets are days from the frozen now.  The `far' rows are the theorem's
plan exemption — a plan beyond the Upcoming horizon — and they are in
the corpus precisely so the exemption is exercised rather than assumed.

R95 ADDED THE THIRD SPELLING, and it is the reason a sound property
passed over an unsound space.  The R94 generator built every heading
from `:scheduled' and `:deadline' ONLY, so the matrix had no cell for a
plan written as a bare active `<timestamp>' in the body — the exact
shape that had no row anywhere at any file age.  99 headings could not
see it.  With the axis added the matrix is plan-SPELLING x plan-DISTANCE
x record x priority, and the `body-ts-*' rows are the cell R94 got
wrong: they are DATED, they are NOT PLANNED, and the catch-all must take
them.

Note which column those rows leave EMPTY.  A body `<ts>' is not a plan
any date section reads, so `org-air-r94--coverage-corpus' reports NO
plan for them and the theorem must cover them under `no plan, no
record' — which is the whole of FU1, stated in the generator rather than
in an assertion.")

(defconst org-air-r94--record-shapes
  '((no-record  nil)
    (fresh       -1)
    (quiet      -60))
  "Every RECORD a heading can carry: (NAME UPDATED-OFFSET).")

(defun org-air-r94--coverage-corpus ()
  "Return (ITEM NAME PLAN-OFFSETS RECORD-OFFSET HOME) over the whole matrix.
PLAN-OFFSETS and RECORD-OFFSET are the GENERATOR's own numbers, so the
coverage property can decide its exemptions without ever asking the
classifier — which is what keeps the disjunction from being circular.
HOME is the bucket that MUST cover a heading with no plan and no record:
`untracked' for ordinary work, `inbox' for a queue dweller.

PLAN-OFFSETS deliberately excludes the `:active-ts' axis: a body
`<timestamp>' is not read by `org-air-classify--overdue-p' or
`org-air-classify--due-within-p', so it can never earn the theorem's
plan exemption and the catch-all is what must hold it (R95 FU1).

TWO CELLS THE MATRIX CANNOT REACH ON ITS OWN are appended:

  * an INBOX DWELLER — FU6's clause is invisible to a generator that
    never puts a heading in the queue, and its catch-all is `inbox';
  * a `#A' inbox dweller, so the overlap rule is exercised on the same
    axis (High priority AND Inbox, never Untracked)."
  (let (out)
    (pcase-dolist (`(,plan ,sched ,dl ,ts) org-air-r94--plan-shapes)
      (pcase-dolist (`(,record ,updated) org-air-r94--record-shapes)
        (dolist (priority '(nil ?A ?C))
          (let ((name (format "%s/%s/%s" plan record
                              (if priority (string priority) "none"))))
            (push (list (org-air-r94--item :title name :priority priority
                                           :updated updated
                                           :scheduled sched :deadline dl
                                           :active-ts ts)
                        name
                        (delq nil (list sched dl))
                        updated
                        'untracked)
                  out)))))
    ;; R95: the queue axis.  An inbox dweller's catch-all is `inbox', and
    ;; it is pushed UNCONDITIONALLY, which is why FU6 cannot strand one.
    (pcase-dolist (`(,record ,updated) org-air-r94--record-shapes)
      (dolist (priority '(nil ?A))
        (let ((name (format "inbox/%s/%s" record
                            (if priority (string priority) "none"))))
          (push (list (org-air-r94--item :title name :priority priority
                                         :updated updated :tags '("inbox"))
                      name nil updated 'inbox)
                out))))
    (nreverse out)))

(defconst org-air-r94--scanned-coverage-shapes
  '(("Body stamp only" "* TODO Body stamp only\nSeen ACTIVE-PAST in the body.\n"
     nil nil untracked)
    ("Body stamp tomorrow" "* TODO Body stamp tomorrow\nDrinks ACTIVE-SOON.\n"
     nil nil untracked)
    ("Last deadline property"
     "* TODO Last deadline property\n:PROPERTIES:\n:LAST_DEADLINE: STAMP-14\n:END:\n"
     nil -14 untracked)
    ("Orig scheduled property"
     "* TODO Orig scheduled property\n:PROPERTIES:\n:ORIG_SCHEDULED: STAMP-14\n:END:\n"
     nil -14 untracked)
    ("Plan beyond the horizon"
     "* TODO Plan beyond the horizon\nSCHEDULED: ACTIVE-FAR\n" (30) nil untracked)
    ("Bare and homeless" "* TODO Bare and homeless\nNothing at all.\n"
     nil nil untracked))
  "Shapes only a real SCAN can produce: (TITLE ORG-TEXT PLAN RECORD HOME).
PLAN and RECORD are again the GENERATOR's own numbers.  These widen the
coverage space past what `org-air-r94--item' can express — in particular
the two PROPERTY spellings, whose stamps R94's unanchored planning
regexp swallowed (R95 FU3), so a heading with a perfectly good 14-day
record read as having none.")

(defun org-air-r94--scanned-coverage-text (text)
  "Expand the date placeholders in a `org-air-r94--scanned-coverage-shapes' TEXT."
  (let ((out text))
    (setq out (replace-regexp-in-string "ACTIVE-PAST" (org-air-r94--date -5) out t t))
    (setq out (replace-regexp-in-string "ACTIVE-SOON" (org-air-r94--date 1) out t t))
    (setq out (replace-regexp-in-string "ACTIVE-FAR" (org-air-r94--date 30) out t t))
    (setq out (replace-regexp-in-string "STAMP-14" (org-air-r94--stamp -14) out t t))
    out))

(ert-deftest org-air-r94-8-coverage-theorem-holds-over-the-whole-matrix ()
  "Every board-active non-deferred task has a row, unless IT SAID OTHERWISE.

  THEOREM (as the test seat had to state it).  Every board-active,
  non-deferred task heading has a row somewhere — UNLESS the heading's
  OWN facts defer it, which happens in exactly two ways and no others:

    (a) its PLAN puts it beyond the Upcoming horizon; or
    (b) org-air MEASURED activity on it more recently than its
        priority's threshold.

  Both are promises org-air keeps rather than hiding places: (a)
  resurfaces on its own date, (b) resurfaces when the heading goes
  quiet.  Crucially there is NO THIRD CASE — in particular nothing is
  hidden by a fact about its FILE, which is the whole of R94.

A NOTE ON THE DESIGN'S WORDING (reported, not a defect).  The R94 design
states the theorem with clause (a) only.  Its own proof-by-exhaustion
table carries clause (b) — the `no plan / yes record' row reads \"Needs
attention, ONCE IT CROSSES ITS PRIORITY'S THRESHOLD\" — so the omission
is in the one-line summary, not in the model.  Measured directly here:
`none/fresh/none' (no plan, touched yesterday, no cookie) holds NO
bucket, and correctly so; it was worked on yesterday.  The property is
encoded with BOTH clauses, and clause (b) gets its own promise leg, so
the test states the model rather than the sentence.

Stated as a PROPERTY over a generated corpus rather than as an example,
because the theorem is about the SHAPE of the space and an example can
only ever witness one cell of it.

R95 WIDENED THE SPACE, AND THAT IS THE POINT OF THIS RE-BLESS.  The R94
version generated every heading from `:scheduled' and `:deadline' only.
The property it asserted was sound; the SPACE it asserted it over was
one axis short, and the missing axis held a real defect — a `TODO' whose
only date is a bare active `<timestamp>' in its body was excused from
the catch-all (it is `--dated-p') and read by no date section (they
consult the two PLAN SLOTS), so it had NO ROW ANYWHERE, at any file age,
with the stamp yesterday or tomorrow.  99 headings could not see it.
The corpus is now:

  plan SPELLING   none / SCHEDULED / DEADLINE / both / BODY `<ts>'
  plan DISTANCE   past / today / inside the horizon / beyond it
  record          none / fresh / quiet
  priority        none / #A / #C
  queue           ordinary file / INBOX DWELLER (R95 FU6)

— 14 plans x 3 records x 3 priorities = 126 generated headings, plus 6
inbox-dwelling cells, plus 6 SCANNED shapes that no in-memory builder
can express (the two `:LAST_DEADLINE:'/`:ORIG_SCHEDULED:' property
spellings among them, whose stamps R94's unanchored regexp swallowed).

Seven legs:

  1. THE THEOREM.  Every generated heading holds a bucket, or is
     exempted by (a) or by (b) — computed from the GENERATOR's own
     offsets, never from the classifier, so the disjunction cannot be
     circular.
  2. (a) IS A PROMISE.  Every plan-exempted heading, re-classified at a
     clock advanced to the edge of the horizon around its own date,
     holds a bucket.
  3. (b) IS A PROMISE.  Every record-exempted heading, re-classified at
     a clock advanced past its own threshold, holds a bucket.
  4. NO THIRD CASE: every heading with NO plan and NO record holds a
     bucket — unconditionally, at every priority, in every plan
     spelling, in or out of the queue — and it is the RIGHT bucket
     (`untracked' for ordinary work, `inbox' for a dweller).  Counted
     explicitly, and the count of third cases is asserted to be ZERO.
  5. THE NEW AXIS IS REAL: every `body-ts-*' heading is DATED and NOT
     PLANNED, so it is exactly the cell R94 lost, and it is now covered.
  6. THE SCANNED SHAPES: the same theorem re-proved over headings a real
     scan produced, including the two property spellings.
  7. ANTI-VACUITY.  Both exempted sets are non-empty, their union is a
     STRICT subset of the corpus, and the corpus is the size claimed.

Reverting the `untracked' bucket arm reddens legs 1 and 4.  Reverting
`--untracked-p' to `--dated-p' reddens legs 1, 4 and 5 on the 27
`body-ts-*' rows — which under R94 it could not, because those rows did
not exist.  Dropping the FU6 clause reddens leg 4's bucket identity."
  (skip-unless (locate-library "org-air"))
  (let ((org-air-upcoming-days 7)
        (org-priority-lowest ?E)
        (plan-exempt 0)
        (record-exempt 0)
        (homeless 0)
        (body-ts 0)
        (queued 0)
        (third-case 0)
        (total 0))
    (cl-flet
        ((check
           (item name plan record home)
           (let* ((buckets (org-air-classify-item item org-air-test-now))
                  (threshold (org-air-classify-attention-threshold item))
                  ;; (a) and (b), computed from the OFFSETS the generator
                  ;; used — never from the classifier.
                  (beyond (and plan (cl-every (lambda (d)
                                                (> d org-air-upcoming-days))
                                              plan)))
                  (recent (and record (< (- record) threshold))))
             (ert-info ((format "%s plan=%S record=%S thr=%d home=%S => %S"
                                name plan record threshold home buckets))
               ;; the corpus really is board-active, non-deferred material
               (should (org-air-classify--board-active-p item))
               (should-not (memq 'backlog buckets))
               ;; 1. THE THEOREM
               (should (or buckets beyond recent))
               ;; ...counted, so leg 4 can assert the count is zero
               (unless (or buckets beyond recent) (cl-incf third-case))
               ;; 4. NO THIRD CASE: no plan AND no record is ALWAYS
               ;; covered, and by the RIGHT bucket.
               (when (and (null plan) (null record))
                 (cl-incf homeless)
                 (should buckets)
                 (should (memq home buckets))
                 (when (eq home 'inbox)
                   (cl-incf queued)
                   ;; FU6: the dweller left Untracked and lost nothing.
                   (should-not (memq 'untracked buckets))))
               (when (null buckets)
                 (when beyond
                   (cl-incf plan-exempt)
                   ;; 2. the plan exemption is a PROMISE: advance the
                   ;; clock to the edge of the horizon around its date.
                   (let* ((soonest (apply #'min plan))
                          (later (time-add
                                  org-air-test-now
                                  (days-to-time
                                   (- soonest org-air-upcoming-days)))))
                     (should (org-air-classify-item item later))))
                 (when (and recent (not beyond))
                   (cl-incf record-exempt)
                   ;; 3. the record exemption is a PROMISE: advance the
                   ;; clock past its own threshold and it surfaces.
                   (let ((later (time-add
                                 org-air-test-now
                                 (days-to-time (+ threshold record 1)))))
                     (should (memq 'attention
                                   (org-air-classify-item item later))))))
               buckets))))
      ;; ---- the GENERATED matrix -------------------------------------
      (pcase-dolist (`(,item ,name ,plan ,record ,home)
                     (org-air-r94--coverage-corpus))
        (cl-incf total)
        (let ((buckets (check item name plan record home)))
          ;; 5. THE NEW AXIS IS REAL: a body `<ts>' is DATED, is NOT
          ;; PLANNED, and the catch-all holds it.
          (when (string-prefix-p "body-ts" name)
            (cl-incf body-ts)
            (ert-info ((format "body-ts cell %s => %S" name buckets))
              (should (org-air-classify--dated-p item))
              (should-not (org-air-classify--planned-p item))
              (should (null plan))
              (when (null record)
                (should (memq 'untracked buckets)))))))
      ;; ---- the SCANNED shapes ---------------------------------------
      ;; 6.  Shapes an in-memory builder cannot express: the two PROPERTY
      ;; spellings whose stamps R94's unanchored regexp swallowed, plus a
      ;; real body stamp and a real inbox dweller.
      (let ((scanned 0))
        (org-air-r94--with-corpus
            (append
             (mapcar (lambda (shape)
                       (cons (format "%s.org"
                                     (replace-regexp-in-string
                                      " " "-" (downcase (nth 0 shape))))
                             (org-air-r94--scanned-coverage-text (nth 1 shape))))
                     org-air-r94--scanned-coverage-shapes)
             (list (cons "inbox.org"
                         "#+title: inbox\n\n* TODO Queued and homeless\nNothing.\n")))
          (let ((items (org-air-query-items)))
            (pcase-dolist (`(,title ,_text ,plan ,record ,home)
                           org-air-r94--scanned-coverage-shapes)
              (cl-incf scanned)
              (cl-incf total)
              (check (org-air-r94--scanned title items) title plan record home))
            ;; the queue dweller, scanned rather than tagged
            (cl-incf scanned)
            (cl-incf total)
            (check (org-air-r94--scanned "Queued and homeless" items)
                   "Queued and homeless" nil nil 'inbox)
            ;; the two PROPERTY shapes really did keep their stamps (FU3),
            ;; which is what makes their `record' column honest above.
            (dolist (title '("Last deadline property" "Orig scheduled property"))
              (let ((item (org-air-r94--scanned title items)))
                (should (org-air-classify-updated item))
                (should (= 14 (org-air-classify-quiet-days
                               item org-air-test-now)))))))
        (should (= 7 scanned))))
    ;; 7. ANTI-VACUITY: every exemption fired, every new axis is
    ;; populated, and the exemptions are a STRICT subset of the corpus.
    (should (> plan-exempt 0))
    (should (> record-exempt 0))
    (should (> homeless 0))
    (should (= 27 body-ts))
    (should (> queued 0))
    (should (< (+ plan-exempt record-exempt) total))
    (should (= 126 (* 14 3 3)))
    (should (= 139 total))
    ;; ...AND NO THIRD CASE, as a COUNT rather than as a hope.
    (should (= 0 third-case))))

;;;; -------------------------------------------------------------------
;;;; r94-9 — the `untracked' predicate at its boundaries
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r94-9-untracked-is-no-plan-and-no-record ()
  "`untracked' is a CONJUNCTION: any plan or any record takes a heading out.
The bucket's whole claim is \"org-air knows nothing about this heading\",
so it must be exactly the conjunction of the two negations and nothing
looser.  Asserted over a REAL scanned corpus — one heading per way Org
can carry a plan, one per shape Org writes when something happens, and
the one heading that carries neither:

  a plan     SCHEDULED or DEADLINE — the `org-air-classify--planned-p'
             axis, which is exactly what Overdue and Upcoming read
  a record   a LOGBOOK state change or note, a clock-out, `CLOSED:',
             `:CREATED:', or any inactive body stamp

Also pinned: a `#A' shows in its own section AND here (the standing
overlap rule), and a `:backlog:' heading routes OUT of this bucket with
all the others.

R95 RE-BLESS, TWO LEGS, BOTH BEHAVIOUR CHANGES THIS ROUND MADE ON
PURPOSE.

1. THE PLAN AXIS IS NARROWER, AND THE TEST NOW SAYS WHICH.  R94 looped
   `SCHEDULED / DEADLINE / a bare active <ts>' through one `should-not'
   and called the result \"any plan\".  It was not: `--dated-p' counts a
   body `<ts>' that NO DATE SECTION READS, so a heading whose only date
   was a body stamp was excused from the catch-all and admitted by
   nothing else — no row anywhere, at any file age (the R94 review's
   §3).  R95 asks `--planned-p' instead, so `Has an active stamp' leaves
   the loop and is asserted POSITIVELY as the discriminating triple:
   DATED, NOT PLANNED, and therefore UNTRACKED.  That cell is the whole
   of FU1, and it is worth more here than one more `should-not'.

2. AN INBOX DWELLER IS NOT UNTRACKED.  \"No plan, no record\" is a
   tautology about an unprocessed capture, and Inbox already holds it
   — UNCONDITIONALLY, which is why the exclusion cannot strand anything.
   The R94 leg asserted the overlap; the R95 leg asserts the exclusion
   AND the reason it is safe: the same heading still has a row, one
   section up, and the bare `#A' twin in an ordinary file still shows
   the overlap rule alive."
  (skip-unless (locate-library "org-air"))
  (org-air-r94--with-corpus
      (list
       (cons "tasks.org"
             (concat
              "* TODO Nothing at all\nJust prose.\n"
              ;; plans
              "* TODO Has a schedule\nSCHEDULED: " (org-air-r94--date 30) "\n"
              "* TODO Has a deadline\nDEADLINE: " (org-air-r94--date 30) "\n"
              "* TODO Has an active stamp\nMeeting " (org-air-r94--date 30)
              " in the body.\n"
              ;; records
              "* TODO Has a logbook state\n:LOGBOOK:\n- State \"TODO\" from"
              " \"WAIT\" [2026-01-05 Mon 09:00]\n:END:\n"
              "* TODO Has a logbook note\n:LOGBOOK:\n- Note taken on"
              " [2026-01-05 Mon 09:00] \\\\\n  a note\n:END:\n"
              "* TODO Has a clock\n:LOGBOOK:\nCLOCK: [2026-01-05 Mon 09:00]--"
              "[2026-01-05 Mon 10:00] =>  1:00\n:END:\n"
              "* DONE Has a closed stamp\nCLOSED: [2026-01-05 Mon 09:00]\n"
              "* TODO Has a created property\n:PROPERTIES:\n"
              ":CREATED: [2026-01-05 Mon 09:00]\n:END:\n"
              "* TODO Has a body stamp\n[2026-01-05 Mon 09:00]\n"
              ;; overlaps
              "* TODO [#A] Top and untracked\nNo plan, no record.\n"
              "* TODO Deferred and untracked :backlog:\nNo plan, no record.\n"))
       (cons "inbox.org"
             "#+title: inbox\n\n* Captured and untracked\nNo plan, no record.\n"))
    (let ((items (org-air-query-items)))
      ;; the ONE heading that carries neither
      (let ((bare (org-air-r94--scanned "Nothing at all" items)))
        (should-not (org-air-classify--dated-p bare))
        (should-not (org-air-classify-updated bare))
        (should (org-air-classify--untracked-p bare))
        (should (equal '(untracked)
                       (org-air-classify-item bare org-air-test-now))))
      ;; ANY PLAN takes it out — and "plan" means the two slots the date
      ;; sections read (R95 `--planned-p'), not the broader `--dated-p'.
      (dolist (title '("Has a schedule" "Has a deadline"))
        (let ((item (org-air-r94--scanned title items)))
          (ert-info ((format "plan: %s" title))
            (should (org-air-classify--dated-p item))
            (should (org-air-classify--planned-p item))
            (should-not (org-air-classify--untracked-p item))
            (should-not (memq 'untracked (org-air-classify-item
                                          item org-air-test-now))))))
      ;; ...and a body `<ts>' is NOT one of them: DATED, NOT PLANNED, and
      ;; therefore UNTRACKED.  R94 excused this heading from the
      ;; catch-all and no date section ever read its stamp, so it had no
      ;; row at all — the coverage hole FU1 closed.
      (let ((active (org-air-r94--scanned "Has an active stamp" items)))
        (should (org-air-classify--dated-p active))
        (should-not (org-air-classify--planned-p active))
        (should-not (org-air-classify-updated active))
        (should (org-air-classify--untracked-p active))
        (should (memq 'untracked (org-air-classify-item
                                  active org-air-test-now)))
        ;; and it really was invisible before: the date sections still
        ;; do not read that stamp, so `untracked' is its ONLY row.
        (should (equal '(untracked)
                       (org-air-classify-item active org-air-test-now))))
      ;; ...and so does ANY record, whichever shape Org wrote it in.
      (dolist (title '("Has a logbook state" "Has a logbook note"
                       "Has a clock" "Has a created property"
                       "Has a body stamp"))
        (let ((item (org-air-r94--scanned title items)))
          (ert-info ((format "record: %s" title))
            (should-not (org-air-classify--dated-p item))
            (should (org-air-classify-updated item))
            (should-not (org-air-classify--untracked-p item))
            ;; the discriminating pair: undated, yet NOT untracked.
            (should-not (memq 'untracked (org-air-classify-item
                                          item org-air-test-now))))))
      ;; The overlap rule applies wherever the two rows say DIFFERENT
      ;; things: an untracked `#A' is in High priority AND here.
      (let ((hipri (org-air-r94--scanned "Top and untracked" items)))
        (should (equal '(high-priority untracked)
                       (org-air-classify-item hipri org-air-test-now))))
      ;; R95: for an unprocessed CAPTURE the two rows say the SAME thing,
      ;; so only the one with a verb is kept — and nothing is stranded,
      ;; because the `inbox' push is unconditional for that heading.
      (let ((capture (org-air-r94--scanned "Captured and untracked" items)))
        (should-not (org-air-classify--planned-p capture))
        (should-not (org-air-classify-updated capture))
        (should-not (org-air-classify--untracked-p capture))
        (should-not (memq 'untracked (org-air-classify-item
                                      capture org-air-test-now)))
        (should (memq 'inbox (org-air-classify-item capture org-air-test-now)))
        ;; it still HAS a row, which is the reason the exclusion is safe
        (should (org-air-classify-item capture org-air-test-now)))
      ;; ...except the ONE documented exception every sibling bucket
      ;; shares: a deferred heading routes to `backlog' ALONE.
      (let ((deferred (org-air-r94--scanned "Deferred and untracked" items)))
        (should (org-air-classify--untracked-p deferred)) ; the raw predicate
        (should (equal '(backlog)
                       (org-air-classify-item deferred org-air-test-now)))))))

;;;; -------------------------------------------------------------------
;;;; r94-10 — the R72 law for `is:untracked'
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r94-10-is-untracked-agrees-with-its-section ()
  "`is:untracked' selects EXACTLY the Untracked section, by construction.
The R72 law: each section body IS its token's predicate, one definition
\(`org-air-classify--untracked-p') shared by both, so the filter and the
board can never disagree.  Asserted per item over a real scanned corpus,
which is the only form of this law worth having.

Three further legs:

  * the token PARSES (case-insensitively) to the bucket symbol and is
    OFFERED by completion, so the vocabulary org-air teaches includes
    the one place it admits it cannot rank something;
  * `is:untracked' and `is:nodate' are INDEPENDENT — the corpus
    contains an undated heading WITH a record, which answers `is:nodate'
    and not `is:untracked', AND a heading whose only date is a body
    `<ts>', which answers `is:untracked' and not `is:nodate'.

    R95 RE-BLESS ON THIS LEG.  Under R94 the relation was a STRICT
    SUBSET, and this test said so.  FU1 changed the plan clause from
    `--dated-p' to `--planned-p' — the two slots the date sections
    actually read — and the two sets became genuinely INDEPENDENT:
    neither contains the other, and the separating heading in the new
    direction is exactly the coverage hole R95 closed.  Both separating
    rows are now IN THE CORPUS, so the relation is discriminated rather
    than asserted, and a revert to `--dated-p' reddens this leg as well
    as the per-item law above;
  * the pre-existing `:backlog:' exception is unchanged and shared with
    every sibling token (a deferred heading answers the raw token on its
    own merits while classifying to `(backlog)' alone)."
  (skip-unless (locate-library "org-air"))
  (org-air-r94--with-corpus
      (list
       (cons "tasks.org"
             (concat
              "* TODO Bare and homeless\nNo plan, no record.\n"
              "* TODO Also bare\nNo plan, no record.\n"
              "* TODO Undated but logged\n[2026-06-14 Sun 09:00]\n"
              "* TODO Dated and historyless\nSCHEDULED: "
              (org-air-r94--date 30) "\n"
              "* TODO Overdue and fresh\nDEADLINE: " (org-air-r94--date -3)
              "\n[2026-06-15 Mon 09:00]\n"
              ;; R95: the OTHER separating row — dated by a body `<ts>',
              ;; so `is:untracked' takes it and `is:nodate' refuses it.
              "* TODO Body stamp and homeless\nSeen " (org-air-r94--date -3)
              " in the body.\n"))
       (cons "inbox.org" "#+title: inbox\n"))
    (let ((items (org-air-query-items)))
      ;; THE LAW, per item.
      (let ((org-air-view--tag-filter '("is:untracked"))
            (org-air-filter-match 'all)
            (org-air-view--filter-now org-air-test-now)
            (org-air-view--scope nil)
            (org-air-view--render-partition nil))
        (dolist (item items)
          (let ((buckets (org-air-classify-item item org-air-test-now)))
            (ert-info ((format "is:untracked vs %S on %s" buckets
                               (org-air-item-title item)))
              (should (eq (and (org-air-view--passes-filter-p item) t)
                          (and (memq 'untracked buckets) t)))))))
      ;; anti-vacuity: the token really selected some rows and rejected
      ;; some others.
      (should (equal '("Also bare" "Bare and homeless"
                       "Body stamp and homeless")
                     (org-air-r94--members 'untracked items)))
      (should (= 6 (length items)))
      ;; INDEPENDENT of `is:nodate' — BOTH separating rows are present.
      (let ((nodate nil) (untracked nil))
        (dolist (item items)
          (let ((org-air-view--tag-filter '("is:nodate"))
                (org-air-filter-match 'all)
                (org-air-view--filter-now org-air-test-now)
                (org-air-view--scope nil)
                (org-air-view--render-partition nil))
            (when (org-air-view--passes-filter-p item)
              (push (org-air-item-title item) nodate)))
          (when (org-air-classify--untracked-p item)
            (push (org-air-item-title item) untracked)))
        ;; nodate-not-untracked: a record silences the catch-all
        (should (member "Undated but logged" nodate))
        (should-not (member "Undated but logged" untracked))
        ;; untracked-not-nodate: a body `<ts>' IS a date and is NOT a plan
        (should (member "Body stamp and homeless" untracked))
        (should-not (member "Body stamp and homeless" nodate))
        ;; ...so NEITHER set contains the other, and they still overlap.
        (should-not (cl-subsetp untracked nodate :test #'equal))
        (should-not (cl-subsetp nodate untracked :test #'equal))
        (should (member "Bare and homeless" nodate))
        (should (member "Bare and homeless" untracked))
        (should-not (equal (sort nodate #'string<)
                           (sort untracked #'string<)))))
    ;; the token parses and is TAUGHT.
    (should (equal '(is . untracked)
                   (org-air-view--filter-token-parse "is:untracked")))
    (should (equal '(is . untracked)
                   (org-air-view--filter-token-parse "IS:UNTRACKED")))
    (should (member "untracked" org-air-view--filter-is-values))
    (should (member "is:untracked" (org-air-view--filter-vocabulary)))))

;;;; -------------------------------------------------------------------
;;;; r94-11 — the section is CONDITIONAL
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r94-11-untracked-section-is-conditional ()
  "A board with nothing untracked renders exactly the R93 five sections.
The section is an ADMISSION, so it must not be a permanent fixture: a
user whose headings all carry a date or a stamp should never learn the
word.  Both directions on the SAME renderer:

  every heading dated or stamped  ->  the fixed five, and the string
                                      \"Untracked\" appears NOWHERE
  one heading with neither        ->  six sections, the new one last of
                                      the task sections

Asserted at the descriptor layer (where the conditional lives), at the
Summary layer (which grows its own row) and on the painted text (which
is what the user actually sees)."
  (skip-unless (locate-library "org-air"))
  ;; (a) nothing untracked: the fixed five, byte for byte.
  (org-air-r94--with-corpus
      (list (cons "tasks.org"
                  (concat "* TODO Dated task\nSCHEDULED: "
                          (org-air-r94--date 3) "\n"
                          "* TODO Logged task\n[2026-01-05 Mon 09:00]\n"))
            (cons "inbox.org" "#+title: inbox\n"))
    (let ((items (org-air-query-items)))
      (should-not (org-air-r94--members 'untracked items))
      (should (equal '(inbox overdue upcoming high-priority attention)
                     (mapcar #'car (org-air-view--section-descriptors items))))
      (should (equal '(inbox overdue upcoming high-priority attention)
                     (mapcar #'car (org-air-view--summary-buckets items)))))
    (org-air-r94--render-board '(120 . 70)
      (let ((text (buffer-string)))
        (should-not (string-match-p "Untracked" text))
        (should-not (assq 'untracked (org-air-r94--rendered-sections))))))
  ;; (b) one heading with neither: the section appears, and only then.
  (org-air-r94--with-corpus
      (list (cons "tasks.org"
                  (concat "* TODO Dated task\nSCHEDULED: "
                          (org-air-r94--date 3) "\n"
                          "* TODO Logged task\n[2026-01-05 Mon 09:00]\n"
                          "* TODO Bare task\nNo plan, no record.\n"))
            (cons "inbox.org" "#+title: inbox\n"))
    (let ((items (org-air-query-items)))
      (should (equal '("Bare task") (org-air-r94--members 'untracked items)))
      (should (equal '(inbox overdue upcoming high-priority attention untracked)
                     (mapcar #'car (org-air-view--section-descriptors items))))
      (should (equal '(inbox overdue upcoming high-priority attention untracked)
                     (mapcar #'car (org-air-view--summary-buckets items)))))
    (org-air-r94--render-board '(120 . 70)
      (should (string-match-p "Untracked" (buffer-string)))
      (should (equal '("Bare task")
                     (mapcar #'car (org-air-r94--rows 'untracked)))))))

;;;; -------------------------------------------------------------------
;;;; r94-12 — the cap is 4
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r94-12-untracked-cap-is-four-and-the-fold-is-honest ()
  "Untracked shows at most FOUR rows, and the fold row carries the true count.
The smallest budget of any task section, deliberately: it is a standing
statement, not a queue to work down.  Overdue and Needs attention get 6,
Upcoming 5, Untracked 4 — pinned as a TABLE so the relation is visible
and a future edit cannot make the admission louder than the alarm.

End to end on a real render: seven untracked headings produce a badge of
7, four painted rows and an \"and 3 more\" fold row that is itself the
section's toggle target.  The fold arithmetic (badge = rows + more) is
the S4 invariant; what is new here is that it holds for a CONDITIONAL
section too."
  (skip-unless (locate-library "org-air"))
  ;; the table, so the relation between the sections is legible
  (should (= 4 (org-air-view--section-limit 'untracked)))
  (should (= 6 (org-air-view--section-limit 'overdue)))
  (should (= 6 (org-air-view--section-limit 'attention)))
  (should (= 5 (org-air-view--section-limit 'upcoming)))
  (should (< (org-air-view--section-limit 'untracked)
             (org-air-view--section-limit 'attention)))
  (org-air-r94--with-corpus
      (list (cons "tasks.org"
                  (mapconcat (lambda (n)
                               (format "* TODO Bare %d\nNo plan, no record.\n" n))
                             (number-sequence 1 7) ""))
            (cons "inbox.org" "#+title: inbox\n"))
    (org-air-r94--render-board '(120 . 70)
      (let* ((rows (org-air-r94--rows 'untracked))
             (badge (nth 1 (assq 'untracked (org-air-r94--section-headings))))
             (more (cdr (assq 'untracked (org-air-r94--fold-counts)))))
        (ert-info ((format "badge=%S rows=%S more=%S"
                           badge (mapcar #'car rows) more))
          (should (= 7 badge))
          (should (= 4 (length rows)))
          (should (= 3 more))
          ;; the S4 invariant, for a CONDITIONAL section
          (should (= badge (+ (length rows) more))))))))

;;;; -------------------------------------------------------------------
;;;; r94-13 — never attention-coloured
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r94-13-untracked-is-never-attention-coloured ()
  "The Untracked count is never painted with the attention badge face.
The design's whole distinction from the deleted \"undated means
attention\" rule is that this section makes NO claim of neglect: it says
org-air cannot rank the work, not that the user is failing it.  That is
a promise about PAINT as much as about routing, so it is asserted on the
paint: Inbox, Overdue and Needs attention carry
`org-air-face-count-attention'; Untracked carries the ordinary
`org-air-face-count', on the same board, with a non-zero badge.

The glyph is pinned beside it: `\u25cc' (`.' in the ASCII tier) — attention's
`\u25cb' with its outline broken, which is exactly the relation between the
two sections: one has a clock that ran out, the other has no clock at
all.  It has no SAFE middle tier, like every other section icon."
  (skip-unless (locate-library "org-air"))
  ;; the glyph, both tiers, and its relation to attention's
  (should (equal '("\u25cc" . ".") (cdr (assq 'untracked org-air-glyphs))))
  (should-not (eq (org-air-view--glyph 'untracked)
                  (org-air-view--glyph 'attention)))
  (org-air-r94--with-corpus
      (list (cons "tasks.org"
                  (concat "* TODO Bare task\nNo plan, no record.\n"
                          "* TODO Quiet task\n[2026-01-05 Mon 09:00]\n"
                          "* TODO Late task\nDEADLINE: " (org-air-r94--date -3)
                          "\n[2026-06-15 Mon 09:00]\n"))
            (cons "inbox.org"
                  "#+title: inbox\n\n* Captured\n[2026-06-15 Mon 09:00]\n"))
    (org-air-r94--render-board '(120 . 70)
      (let ((headings (org-air-r94--section-headings)))
        (ert-info ((format "headings=%S" headings))
          (cl-flet ((badge (b) (nth 1 (assq b headings)))
                    (faced (b) (nth 2 (assq b headings))))
            ;; every section that should shout, does — with a non-zero
            ;; badge, so the face is a decision and not an empty section.
            (dolist (bucket '(inbox overdue attention))
              (should (= 1 (badge bucket)))
              (should (faced bucket)))
            ;; ...and the one that must NOT shout, does not, with an
            ;; equally non-zero badge.
            (should (= 1 (badge 'untracked)))
            (should-not (faced 'untracked))
            ;; Upcoming is the control: it has never shouted either.
            (should-not (faced 'upcoming))))))))

;;;; -------------------------------------------------------------------
;;;; r94-14 — `~' provenance in the Untracked row
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r94-14-untracked-prints-a-marked-bound-or-no-history ()
  "An Untracked row prints `~Nd quiet' or `no history' — never a bare number.
There is no heading-level number to print here, so the cell prints the
only fact org-air has and MARKS it as the file-level bound it is.  The
leading `~' is load-bearing: it says the number is about the FILE, and
that the real age can only be LARGER.  It is the same idiom the rail
already uses for its `~file' Updated line (R74).

  `~210d quiet'  the bound, N > 0
  `no history'   the bound is 0 or unknown — the file changed today, or
                 the item was built outside the scan.  A bound of zero
                 says nothing at all, so org-air prints the fact it does
                 have instead of a number that means nothing.

Both arms on the unit helper AND on a painted board, plus the face (the
quiet date face in both cases: a missing record is not an alarm)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test--with-frozen-now
    ;; an item the scan never saw: no bound at all.
    (let ((outsider (org-air-r94--item :title "Outside")))
      (should-not (org-air-classify-quiet-floor-days outsider org-air-test-now))
      (should (equal (cons "no history" 'org-air-face-date)
                     (org-air-view--untracked-reason outsider org-air-test-now)))
      ;; and the row cell routes through it for the `untracked' bucket.
      (should (equal (cons "no history" 'org-air-face-date)
                     (org-air-view--date-label outsider 'untracked)))))
  ;; a real corpus: one file old, one file fresh.
  (org-air-r94--with-corpus
      '(("old.org" . "* TODO Bare in an old file\nNo plan, no record.\n")
        ("new.org" . "* TODO Bare in a fresh file\nNo plan, no record.\n")
        ("inbox.org" . "#+title: inbox\n"))
    (set-file-times (org-air-r94--path "old.org")
                    (time-subtract org-air-test-now (days-to-time 210)))
    (set-file-times (org-air-r94--path "new.org") org-air-test-now)
    (org-air-r94--render-board '(120 . 70)
      (let* ((rows (org-air-r94--rows 'untracked))
             (cells (mapcar (lambda (r) (cons (car r) (nth 1 r))) rows)))
        (ert-info ((format "untracked cells: %S" cells))
          (should (= 2 (length rows)))
          ;; the bound, MARKED...
          (let ((old (assoc-default "Bare in an old file" cells)))
            (should old)
            (should (string-match-p "~210d quiet" old))
            ;; the `~' is really there: the bare spelling must not be.
            (should-not (string-match-p "[^~]210d quiet" old)))
          ;; ...and the honest refusal when the bound says nothing.
          (let ((new (assoc-default "Bare in a fresh file" cells)))
            (should new)
            (should (string-match-p "no history" new))
            (should-not (string-match-p "0d quiet" new))))))))

;;;; -------------------------------------------------------------------
;;;; r94-15 — Needs attention prints NO estimates
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r94-15-needs-attention-prints-no-estimates-at-all ()
  "Every number Needs attention prints is a MEASURED heading fact.
The R93 review's headline finding, inverted into an invariant: 52 % of
that section's members and 83 % of the rows visible under its cap were
file-derived guesses wearing exactly the same clothes as facts, all with
the identical number, outranking every measured age below them.

R94 does not fix that by annotating the guesses — it removes them from
the section by construction, so the section needs no marker.  Asserted
as a PROPERTY over a real board, on a corpus deliberately built to be
the review's worst case (an old file full of historyless headings beside
a handful of measured ones):

  * every member's `org-air-classify-updated-source' is `measured';
  * every painted cell in the section carries NO `~';
  * the number in every cell equals `org-air-classify-quiet-days' for
    that row — the bucket's own number, not the file's;
  * and the file's bound is genuinely LARGER than several of those
    numbers, so the invariance is not an accident of a fresh corpus."
  (skip-unless (locate-library "org-air"))
  (org-air-r94--with-corpus
      (list (cons "tasks.org"
                  (concat
                   "#+title: tasks\n\n"
                   "* TODO Measured forty\n[2026-05-06 Wed 09:00]\n"
                   "* TODO Measured ninety\n[2026-03-17 Tue 09:00]\n"
                   (mapconcat
                    (lambda (n)
                      (format "* TODO Bare %d\nNo plan, no record.\n" n))
                    (number-sequence 1 6) "")))
            (cons "inbox.org" "#+title: inbox\n"))
    ;; the review's shape: the file is much older than either measured age.
    (set-file-times (org-air-r94--path "tasks.org")
                    (time-subtract org-air-test-now (days-to-time 210)))
    (org-air-r94--render-board '(120 . 70)
      (let ((rows (org-air-r94--rows 'attention)))
        (should (= 2 (length rows)))
        (pcase-dolist (`(,title ,span ,item) rows)
          (ert-info ((format "attention row %s: %S" title span))
            ;; measured, and only measured
            (should (eq 'measured (org-air-classify-updated-source item)))
            ;; no estimate marker anywhere in the row
            (should-not (string-match-p "~" span))
            ;; the printed number IS the bucket's number
            (should (= (org-air-classify-quiet-days item org-air-test-now)
                       (org-air-r94--cell-number span)))
            ;; ANTI-VACUITY: the file bound is bigger, and was available
            ;; the whole time — it simply never reaches this section.
            (should (= 210 (org-air-classify-quiet-floor-days
                            item org-air-test-now)))
            (should (> 210 (org-air-classify-quiet-days
                            item org-air-test-now)))))
        ;; the six historyless headings are not lost for it: they are on
        ;; the board, in the section that claims nothing about them.
        (should (= 6 (length (org-air-r94--members
                              'untracked org-air-view--items))))
        (should (= 4 (length (org-air-r94--rows 'untracked)))) ; the cap
        ;; ...and NONE of them reached the section this test is about.
        (should-not (seq-intersection
                     (org-air-r94--members 'attention org-air-view--items)
                     (org-air-r94--members 'untracked org-air-view--items)))))))

;;;; -------------------------------------------------------------------
;;;; r94-16 — `--sort-by-floor'
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r94-16-untracked-sorts-by-the-number-it-prints ()
  "Untracked is ordered longest-bound-first, `no history' rows last.
The FIX-2 law — each section ordered worst-first by the very number its
own rows print — reaching the one section R94 added.  The key is
`org-air-classify-quiet-floor-days', which is exactly what
`org-air-view--untracked-reason' prints as `~210d quiet', so the order a
user sees and the order the comparator computed are the same list by
construction.

Rows whose bound is 0 or unknown print `no history' and sort LAST in
stable incoming (query) order: a file that changed today says nothing at
all about a heading with no history in it, and org-air ranks nothing on a
fact it does not have.

Asserted on the comparator (three files, three ages, plus an item the
scan never saw) and end to end on a painted board, where the sequence of
printed numbers must itself be descending."
  (skip-unless (locate-library "org-air"))
  (org-air-r94--with-corpus
      '(("ancient.org" . "* TODO Bare ancient\nNo plan, no record.\n")
        ("middling.org" . "* TODO Bare middling\nNo plan, no record.\n")
        ("recent.org" . "* TODO Bare recent\nNo plan, no record.\n")
        ("today.org" . "* TODO Bare today\nNo plan, no record.\n")
        ("inbox.org" . "#+title: inbox\n"))
    (set-file-times (org-air-r94--path "ancient.org")
                    (time-subtract org-air-test-now (days-to-time 300)))
    (set-file-times (org-air-r94--path "middling.org")
                    (time-subtract org-air-test-now (days-to-time 120)))
    (set-file-times (org-air-r94--path "recent.org")
                    (time-subtract org-air-test-now (days-to-time 9)))
    (set-file-times (org-air-r94--path "today.org") org-air-test-now)
    (let* ((items (org-air-query-items))
           (untracked (seq-filter #'org-air-classify--untracked-p items)))
      (should (= 4 (length untracked)))
      (org-air-viewport-test--with-frozen-now
        (with-temp-buffer
          (org-air-view-mode)
          (let ((sorted (org-air-view--sort-items untracked 'untracked)))
            (should (equal '("Bare ancient" "Bare middling" "Bare recent"
                             "Bare today")
                           (mapcar #'org-air-item-title sorted)))
            ;; the printed cells match the order, and the last one is the
            ;; honest refusal rather than a `0d' number.
            (should (equal '("~300d quiet" "~120d quiet" "~9d quiet"
                             "no history")
                           (mapcar (lambda (i)
                                     (car (org-air-view--date-label
                                           i 'untracked)))
                                   sorted))))
          ;; an item the scan never saw joins the `no history' tail, in
          ;; stable incoming order, and never outranks a real bound.
          (let* ((outsider (org-air-r94--item :title "Outside the scan"))
                 (sorted (org-air-view--sort-items
                          (cons outsider untracked) 'untracked)))
            (should (equal "Bare ancient" (org-air-item-title (car sorted))))
            (should (member (org-air-item-title (car (last sorted)))
                            '("Bare today" "Outside the scan")))))))
    (org-air-r94--render-board '(120 . 70)
      (let* ((rows (org-air-r94--rows 'untracked))
             (numbers (delq nil (mapcar (lambda (r)
                                          (org-air-r94--cell-number (nth 1 r)))
                                        rows))))
        (should (equal '("Bare ancient" "Bare middling" "Bare recent"
                         "Bare today")
                       (mapcar #'car rows)))
        (should (equal '(300 120 9) numbers))
        (should (equal numbers (sort (copy-sequence numbers) #'>)))))))

;;;; -------------------------------------------------------------------
;;;; r94-17 — `--sort-by-quiet' breaks ties toward MEASURED
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r94-17-quiet-sort-breaks-ties-toward-measured ()
  "Equal ages order MEASURED first — dormant under the defaults, still correct.
R94 also implements the review's follow-up 4b, and keeps it even though
the board can no longer reach it: because the file floor left the
attention clock, EVERY age that section sorts on is already a measured
heading fact, so the tie-break arm never fires on a default board.

It is kept because the ORDERING LAW must not depend on the current
bucket wiring: a caller handing this comparator a mixed list must still
get \"a fact outranks a bound\" rather than whatever the incoming order
happened to be.  Tested by calling the comparator directly with such a
list, which is the only way to reach a dormant arm honestly.

Where this seat DISAGREES with the review, and why the disagreement is
in the code: \u00a79.2 asked that a measured age ALWAYS outrank a bound
\(\"a measured 87d above a guessed 210d\").  It must not.  `~210d' means
\"at least 210 days\", which is a STRONGER claim of neglect than a
measured 87, and systematically demoting bounds would re-hide exactly
the oldest work.  So the tie-break is a TIE-break: equal numbers only.
That is asserted here in both directions — 210 still outranks 87
whichever provenance each carries."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test--with-frozen-now
    (with-temp-buffer
      (org-air-view-mode)
      ;; A MIXED list: same age, different provenance.  (Constructed by
      ;; hand because the default board can no longer produce one.)
      (org-air-r94--with-corpus
          '(("bound.org" . "* TODO Bounded row\nNo plan, no record.\n")
            ("inbox.org" . "#+title: inbox\n"))
        (set-file-times (org-air-r94--path "bound.org")
                        (time-subtract org-air-test-now (days-to-time 40)))
        (let* ((items (org-air-query-items))
               (bounded (org-air-r94--scanned "Bounded row" items))
               (measured (org-air-r94--item :title "Measured row" :updated -40)))
          ;; the shape really is a TIE with different provenance...
          (should (eq 'file (org-air-classify-updated-source bounded)))
          (should (eq 'measured (org-air-classify-updated-source measured)))
          (should (= 40 (org-air-classify-quiet-floor-days
                         bounded org-air-test-now)))
          (should (= 40 (org-air-classify-quiet-days
                         measured org-air-test-now)))
          ;; ...and the measured row wins it, from EITHER incoming order.
          ;; (`--sort-by-quiet' keys on the measured age, so the bounded
          ;; row's key is nil and it trails — the unknown-last rule and
          ;; the provenance rule agree here, which is the point: a fact
          ;; never sits below a number derived from something else.)
          (dolist (incoming (list (list bounded measured)
                                  (list measured bounded)))
            (should (equal '("Measured row" "Bounded row")
                           (mapcar #'org-air-item-title
                                   (org-air-view--sort-by-quiet incoming)))))))
      ;; A TIE-BREAK, not a demotion: a LARGER number still wins, and the
      ;; comparator is stable on genuine ties.
      (let* ((old (org-air-r94--item :title "Older measured" :updated -210))
             (new (org-air-r94--item :title "Newer measured" :updated -87))
             (tie-a (org-air-r94--item :title "Tie A" :updated -50))
             (tie-b (org-air-r94--item :title "Tie B" :updated -50)))
        (should (equal '("Older measured" "Newer measured")
                       (mapcar #'org-air-item-title
                               (org-air-view--sort-by-quiet (list new old)))))
        (should (equal '("Tie A" "Tie B")
                       (mapcar #'org-air-item-title
                               (org-air-view--sort-by-quiet
                                (list tie-a tie-b)))))
        (should (equal '("Tie B" "Tie A")
                       (mapcar #'org-air-item-title
                               (org-air-view--sort-by-quiet
                                (list tie-b tie-a)))))))))

;;;; -------------------------------------------------------------------
;;;; r94-18 — section ORDER and the Summary row
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r94-18-untracked-is-last-of-the-task-sections ()
  "Untracked sits under Needs attention, above Notes and Backlog.
The R93 order reads as a sentence — process (Inbox), repair (Overdue),
plan (Upcoming), choose (High priority), sweep (Needs attention) — and
R94 continues it: \"and here is the work I cannot rank at all\".  It
belongs with the TASK sections it is about, so it goes ahead of the two
lenses (Notes, Backlog), and it goes LAST of them because it is the
quietest thing on the board.

Pinned on the descriptor list, the Summary list and the painted board,
with all four conditional sections live at once so their relative order
is a real ordering and not a pair of independent facts."
  (skip-unless (locate-library "org-air"))
  (org-air-r94--with-corpus
      (list (cons "tasks.org"
                  (concat "* TODO Quiet task\n[2026-01-05 Mon 09:00]\n"
                          "* TODO Bare task\nNo plan, no record.\n"
                          "* TODO Deferred task :backlog:\n"
                          "[2026-01-05 Mon 09:00]\n"))
            (cons "note.org" "#+title: A loose note\n\nProse, no headings.\n")
            (cons "inbox.org" "#+title: inbox\n"))
    (let ((items (org-air-query-items))
          (org-air-show-notes-section t)
          (org-air-show-backlog-section t))
      (should (equal '(inbox overdue upcoming high-priority attention
                             untracked notes backlog)
                     (mapcar #'car (org-air-view--section-descriptors items))))
      ;; the Summary grows the same row, in the same place (Notes stay OUT
      ;; of the Summary, as they always have).
      (should (equal '(inbox overdue upcoming high-priority attention
                             untracked backlog)
                     (mapcar #'car (org-air-view--summary-buckets items))))
      ;; the title resolves through the conditional descriptor
      (should (equal "Untracked" (org-air-view--bucket-title 'untracked)))
      (should (equal "Untracked"
                     (org-air-view--inspector-bucket-name 'untracked))))
    (org-air-r94--render-board '(120 . 70)
      (let ((order (mapcar #'car (org-air-r94--rendered-sections))))
        (should (equal '(inbox overdue upcoming high-priority attention
                               untracked notes backlog)
                       order))))))

;;;; -------------------------------------------------------------------
;;;; r94-19 — the rail names the bucket and its reason
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r94-19-inspector-names-untracked-and-its-reason ()
  "The rail says in WORDS what the row says in a cell, from the same helper.
The R93 discipline (r93-14) extended to the new section: the row cell and
the rail Bucket line must quote the SAME public helper, so they cannot
drift.  The rail spells the bucket by name and adds the reason — what
org-air does not have, and the file-level bound it does, marked `~'
exactly as the row marks it.

  no plan, no record \u00b7 file quiet ~210d   when the bound says something
  no plan, no record                        when it does not

And the negative that keeps the wording honest: a heading with a record
is NOT described this way, however undated it is."
  (skip-unless (locate-library "org-air"))
  (org-air-r94--with-corpus
      '(("old.org" . "* TODO Bare in an old file\nNo plan, no record.\n")
        ("new.org" . "* TODO Bare in a fresh file\nNo plan, no record.\n\
* TODO Undated but logged\n[2026-01-05 Mon 09:00]\n")
        ("inbox.org" . "#+title: inbox\n"))
    (set-file-times (org-air-r94--path "old.org")
                    (time-subtract org-air-test-now (days-to-time 210)))
    (set-file-times (org-air-r94--path "new.org") org-air-test-now)
    (let ((items (org-air-query-items)))
      (with-temp-buffer
        (org-air-view-mode)
        (setq org-air-view--classify-cache nil)
        (let ((line (org-air-view--inspector-bucket-line
                     (org-air-r94--scanned "Bare in an old file" items)
                     "" org-air-test-now)))
          (should (string-match-p "Untracked" line))
          (should (string-match-p "no plan, no record" line))
          (should (string-match-p "file quiet ~210d" line))
          ;; never the accusation
          (should-not (string-match-p "Attention" line)))
        (setq org-air-view--classify-cache nil)
        (let ((line (org-air-view--inspector-bucket-line
                     (org-air-r94--scanned "Bare in a fresh file" items)
                     "" org-air-test-now)))
          (should (string-match-p "Untracked" line))
          (should (string-match-p "no plan, no record" line))
          ;; a bound of 0 says nothing, so nothing is claimed
          (should-not (string-match-p "file quiet" line)))
        (setq org-air-view--classify-cache nil)
        (let ((line (org-air-view--inspector-bucket-line
                     (org-air-r94--scanned "Undated but logged" items)
                     "" org-air-test-now)))
          (should-not (string-match-p "Untracked" line))
          (should-not (string-match-p "no plan, no record" line)))))))

;;;; -------------------------------------------------------------------
;;;; r94-20 — the landing helper reaches Untracked only as a last resort
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r94-20-first-actionable-item-reaches-untracked-last ()
  "`--first-actionable-item' prefers every fixed section, then Untracked.
The inspector's seed must land on a real row.  Untracked is appended
LAST, so a board that has anything else lands there and a board that is
NOTHING BUT untracked work still gets a seeded inspector instead of a
placeholder.  Both directions, on real scanned corpora."
  (skip-unless (locate-library "org-air"))
  ;; (a) something else exists: Untracked is not reached.
  (org-air-r94--with-corpus
      (list (cons "tasks.org"
                  (concat "* TODO Bare task\nNo plan, no record.\n"
                          "* TODO Due soon\nSCHEDULED: " (org-air-r94--date 1)
                          "\n[2026-06-15 Mon 09:00]\n"))
            (cons "inbox.org" "#+title: inbox\n"))
    (let* ((items (org-air-query-items))
           (first (org-air-view--first-actionable-item items)))
      (should first)
      (should (equal "Due soon" (org-air-item-title first)))))
  ;; (b) nothing but untracked work: the helper still finds a row.
  (org-air-r94--with-corpus
      '(("tasks.org" . "* TODO Only bare task\nNo plan, no record.\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((items (org-air-query-items))
           (first (org-air-view--first-actionable-item items)))
      (should first)
      (should (equal "Only bare task" (org-air-item-title first)))
      (should (memq 'untracked (org-air-classify-item first org-air-test-now))))))

;;;; -------------------------------------------------------------------
;;;; r94-21 — the floor is a HASH LOOKUP: classify does zero I/O
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r94-21-the-floor-costs-no-io ()
  "Classifying a whole board opens, stats and reads NOTHING (R53, kept).
R94 added a predicate (`--untracked-p') and a public reader
\(`--updated-floor') to the classify path, and the render-purity law is
exactly the kind of thing a new reader breaks: the floor is a FILE fact,
and the obvious implementation of a file fact is a `file-attributes'
call.  It is a hash lookup on the table the cache hydrates, and this
pins it.

Every R53 spy at zero across a full classification of every item, the
floor and the provenance helper both exercised on every one of them, and
the answers asserted non-trivial so the zero cannot come from doing
nothing."
  (skip-unless (locate-library "org-air"))
  (org-air-r94--with-corpus
      (list (cons "tasks.org"
                  (concat "* TODO Bare one\nNo plan, no record.\n"
                          "* TODO Bare two\nNo plan, no record.\n"
                          "* TODO Quiet one\n[2026-01-05 Mon 09:00]\n"
                          "* TODO Dated one\nSCHEDULED: " (org-air-r94--date -3)
                          "\n"))
            (cons "inbox.org" "#+title: inbox\n"))
    (set-file-times (org-air-r94--path "tasks.org")
                    (time-subtract org-air-test-now (days-to-time 210)))
    (let* ((items (org-air-query-items))
           (stats 0) (opens 0) (reads 0)
           (orig-attrs (symbol-function 'file-attributes))
           (orig-ffns (symbol-function 'find-file-noselect))
           (orig-ifc (symbol-function 'insert-file-contents))
           (floors nil))
      (cl-letf (((symbol-function 'file-attributes)
                 (lambda (&rest a) (cl-incf stats) (apply orig-attrs a)))
                ((symbol-function 'find-file-noselect)
                 (lambda (&rest a) (cl-incf opens) (apply orig-ffns a)))
                ((symbol-function 'insert-file-contents)
                 (lambda (&rest a) (cl-incf reads) (apply orig-ifc a))))
        (dolist (item items)
          (org-air-classify-item item org-air-test-now)
          (org-air-classify--untracked-p item)
          (org-air-classify-updated-source item)
          (push (org-air-classify-quiet-floor-days item org-air-test-now)
                floors)))
      (should (= 0 stats))
      (should (= 0 opens))
      (should (= 0 reads))
      ;; anti-vacuity: the floor really answered, with the real number.
      (should (member 210 floors))
      (should (= 2 (length (org-air-r94--members 'untracked items)))))))

;;;; -------------------------------------------------------------------
;;;; r94-22 — cache v7 -> v8
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r94-22-cache-v8-and-v7-clean-cold-miss ()
  "v7 is discarded even though its SHAPE is identical: the bump is about MEANING.
The one seat in the suite that names the shipped cache version, so a
bump has exactly one test to update and every other cache test reads the
constant.

This bump is unusual and the test is built around why.  There is NO new
slot: a v7 record and a v8 record are byte-identical in shape, and
`read' would hand back a perfectly well-formed item.  What changed is
what `updated' is ALLOWED to contain — R94 narrowed it twice, so that an
inactive `SCHEDULED:'/`DEADLINE:' value and a quoted old plan date
inside a reschedule log line are no longer updates.  A v7 record was
harvested under the OLD rule, so hydrating it would keep a heading
looking fresher than it is and, because `--untracked-p' reads the same
slot, would also keep it out of the Untracked section it belongs in.
Wrong answers that look right are worse than a cold miss.

Four legs: the shipped version is 8; a v8 cache round-trips; a v7 record
of the CURRENT shape is refused at the version gate (the shape is
asserted identical first, so the refusal can only be the version); and
the refusal is CLEAN — nil read, nil load, no error, no hang."
  (skip-unless (locate-library "org-air"))
  (should (= 8 org-air-view--cache-version))
  (org-air-r94--with-corpus
      '(("tasks.org" . "\
* TODO Quoted reschedule
:LOGBOOK:
- Rescheduled from \"[2026-06-14 Sun]\" on [2026-06-01 Mon 08:00]
:END:
* TODO Inactive deadline
DEADLINE: [2026-06-14 Sun]
")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((files (org-air-query-files))
           (items (org-air-query-items))
           (mtimes (org-air-view--mtimes-snapshot files))
           (records (mapcar #'org-air-view--item-serialise items)))
      ;; The two R94 guards really did narrow this corpus's `updated'.
      (should (= 14 (org-air-classify-quiet-days
                     (org-air-r94--scanned "Quoted reschedule" items)
                     org-air-test-now)))
      (should-not (org-air-classify-updated
                   (org-air-r94--scanned "Inactive deadline" items)))
      ;; A v8 cache round-trips.
      (org-air-view--cache-write items mtimes)
      (let ((data (org-air-view--cache-read)))
        (should data)
        (should (= 8 (plist-get data :version)))
        (let ((hydrated (plist-get data :items)))
          (should hydrated)
          (should (equal (mapcar #'org-air-item-updated items)
                         (mapcar #'org-air-item-updated hydrated)))))
      ;; A v7 record of the CURRENT shape: same bytes, older meaning.
      (let ((print-length nil) (print-level nil) (print-circle t))
        (write-region
         (prin1-to-string (list :version 7 :key (org-air-view--cache-key)
                                :mtimes mtimes :file-meta nil :visits nil
                                :items records))
         nil org-air-cache-file nil 'silent))
      ;; the SHAPE is identical — so the refusal below is the VERSION and
      ;; nothing else.
      (let ((v8 (mapcar #'org-air-view--item-serialise items)))
        (should (= (length (car v8)) (length (car records)))))
      ;; ...refused, cleanly.
      (should-not (org-air-view--cache-read))
      (should-not (org-air-view--cache-load)))))

(provide 'org-air-round94-test)
;;; org-air-round94-test.el ends here
