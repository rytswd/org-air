;;; org-air-classify.el --- Classification for org-air items -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: org-air contributors
;; Keywords: outlines, calendar
;; Version: 0.1.0
;; URL: https://github.com/rytswd/org-air
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Pure bucket classification for `org-air-item' records.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-air-query)

(defvar org-air-inbox-file)

(defcustom org-air-stale-days 21
  "Number of days without activity before an item is stale."
  :type 'integer
  :group 'org-air)

(defcustom org-air-upcoming-days 7
  "Number of calendar days ahead considered upcoming."
  :type 'integer
  :group 'org-air)

(defun org-air-classify--time (timestamp)
  "Convert Org TIMESTAMP object to an Emacs time value."
  (when timestamp
    (ignore-errors (org-timestamp-to-time timestamp))))

(defun org-air-classify--days-between (then now)
  "Return calendar days between THEN and NOW."
  (- (time-to-days now) (time-to-days then)))

(defun org-air-classify--item-source (item)
  "Return (BUFFER . POS) for ITEM's marker slot, or nil.
R53 P2: only a LIVE marker resolves — a cache-hydrated (FILE . POS) cons
returns nil, because everything classify/render needs now lives in the
item's scan-time slots (`donep'/`activity'/`body-deadline').  RENDER
NEVER OPENS A FILE: the old cons branch was a per-item
`find-file-noselect' — the measured 186s warm first paint at 15.9k
items."
  (let ((m (org-air-item-marker item)))
    (when (and (markerp m) (marker-buffer m))
      (cons (marker-buffer m) (marker-position m)))))

(defun org-air-classify--done-keywords (item)
  "Return done TODO keywords applicable to ITEM WITHOUT opening a file.
R53 P2: the scan records `donep' at scan time, so this is only the
fallback vocabulary for items built OUTSIDE the scan (a live capture
buffer): the live marker's buffer keywords (a real buffer still knows
best), else the MERGED scan vocabulary's done set
\(`org-air-query-merged-done-keywords', R57-1: the user's global done
keywords — CLOSED, DROPPED — plus org-air's supplement, replacing the
pre-R57 hard-wired (\"DONE\"))."
  (or (when-let* ((src (org-air-classify--item-source item)))
        (with-current-buffer (car src)
          (or org-done-keywords (default-value 'org-done-keywords))))
      (default-value 'org-done-keywords)
      (org-air-query-merged-done-keywords)))

(defun org-air-classify--done-p (item)
  "Return non-nil if ITEM has a done TODO state.
R53 P2: data-pure — the scan-time `donep' slot (todo ∈ the file's own
`org-done-keywords' as known in the scan buffer) answers without any file
access; items built outside the scan fall back to
`org-air-classify--done-keywords' (live buffer or global default)."
  (or (org-air-item-donep item)
      (when-let* ((todo (org-air-item-todo item)))
        (member todo (org-air-classify--done-keywords item)))))

(defun org-air-classify--future-or-today-p (timestamp now &optional days)
  "Return non-nil when TIMESTAMP is within the upcoming window from NOW.
DAYS is the window horizon in calendar days, defaulting to
`org-air-upcoming-days' (every pre-R72 caller unchanged).  The window is
0 <= d <= DAYS: today is in, the past is out — the ONE inequality shared
by the Upcoming bucket and the R72 `due:Nd' filter windows, so they agree
by construction."
  (when-let* ((time (org-air-classify--time timestamp)))
    (let ((d (org-air-classify--days-between now time)))
      (and (>= d 0) (<= d (or days org-air-upcoming-days))))))

(defun org-air-classify--past-p (timestamp now)
  "Return non-nil when TIMESTAMP is before today relative to NOW."
  (when-let* ((time (org-air-classify--time timestamp)))
    (> (org-air-classify--days-between time now) 0)))

(defun org-air-classify--marker-timestamp-time (item)
  "Return the first timestamp time found in ITEM's subtree.
R26-8: works over a live marker or a cache-hydrated (FILE . POS) cons;
any positional error (a stale position mid-refresh) degrades to nil, so
the caller's file-mtime fallback takes over instead of a crash."
  (when-let* ((src (org-air-classify--item-source item)))
    (with-current-buffer (car src)
      (ignore-errors
        (save-excursion
          (save-restriction
            (goto-char (cdr src))
            (org-back-to-heading t)
            (let ((end (save-excursion (org-end-of-subtree t t))))
              (when (re-search-forward org-ts-regexp-both end t)
                (ignore-errors
                  (org-timestamp-to-time
                   (org-timestamp-from-string
                    (match-string-no-properties 0))))))))))))

(defun org-air-classify--marker-active-ts (item)
  "Return the first ACTIVE timestamp time in ITEM's subtree, or nil.
R54-1: the live-marker fallback for items built OUTSIDE the scan (a
capture buffer, a unit test) — the `org-air-classify--marker-timestamp-
time' walk with `org-ts-regexp' (active <ts> only, planning lines in)
instead of `org-ts-regexp-both'.  A cons (FILE . POS) marker returns nil
\(data-pure render law — scanned items always answer from the
`active-ts' slot)."
  (when-let* ((src (org-air-classify--item-source item)))
    (with-current-buffer (car src)
      (ignore-errors
        (save-excursion
          (save-restriction
            (goto-char (cdr src))
            (org-back-to-heading t)
            (let ((end (save-excursion (org-end-of-subtree t t))))
              (when (re-search-forward org-ts-regexp end t)
                (ignore-errors
                  (org-timestamp-to-time
                   (org-timestamp-from-string
                    (match-string-no-properties 0))))))))))))

(defun org-air-classify--stale-eligible-p (item)
  "Non-nil when ITEM carries an actionable date (R54-1).
Scheduled, deadline, or an active timestamp in the subtree; a live-marker
item built outside the scan probes its buffer with `org-ts-regexp'
\(bounded, mirroring `org-air-classify--marker-timestamp-time').  No
date => not a task => never Stale."
  (or (org-air-item-scheduled item)
      (org-air-item-deadline item)
      (org-air-item-active-ts item)
      (org-air-classify--marker-active-ts item)))

(defvar org-air-classify--truename-cache (make-hash-table :test #'equal)
  "Memo FILE -> truename for the inbox-membership test (R53 P2).
Bounded by the configured file count; avoids a `file-truename' component
walk per item at 15k items.")

(defun org-air-classify--truename (file)
  "Return FILE's memoised truename (R53 P2)."
  (or (gethash file org-air-classify--truename-cache)
      (puthash file
               (or (ignore-errors (file-truename (expand-file-name file)))
                   (expand-file-name file))
               org-air-classify--truename-cache)))

(defun org-air-classify--inbox-file-p (item)
  "Return non-nil when ITEM lives in `org-air-inbox-file'.
R53 P2: both truenames are memoised (`org-air-classify--truename-cache'),
so the per-item cost is a hash lookup, not a filesystem walk."
  (and (boundp 'org-air-inbox-file)
       org-air-inbox-file
       (org-air-item-file item)
       (equal (org-air-classify--truename (org-air-item-file item))
              (org-air-classify--truename org-air-inbox-file))))

(defun org-air-classify--last-activity (item)
  "Return the best available activity time for ITEM.
R53 P2: the scan-time `activity' slot (an epoch float: closed ‖ scheduled
‖ deadline ‖ first subtree timestamp ‖ file mtime) answers directly for
every scanned item — no file access.  The old chain survives only as the
fallback for items built outside the scan (live-marker probes still
work; a cons marker degrades to the file-mtime fallback)."
  (or (org-air-item-activity item)
      (org-air-classify--time (org-air-item-closed item))
      (org-air-classify--time (org-air-item-scheduled item))
      (org-air-classify--time (org-air-item-deadline item))
      (org-air-classify--marker-timestamp-time item)
      (when-let* ((file (org-air-item-file item))
                  ((file-exists-p file)))
        (file-attribute-modification-time (file-attributes file)))))

(defun org-air-classify--inbox-dweller-p (item)
  "Non-nil when ITEM lives in the inbox (file or `inbox' tag).
The memoised-truename inbox test, hoisted so the R54-2 routing layer in
`org-air-classify-item' and the bucket pass share one definition."
  (or (org-air-classify--inbox-file-p item)
      (member "inbox" (mapcar #'downcase (org-air-item-tags item)))))

(defun org-air-classify--board-active-p (item)
  "Non-nil when ITEM is live board material: not done, not archived (R72).
The factored top gate of `org-air-classify--heading-buckets' — a DONE or
`org-archive-tag'-tagged item classifies into ZERO task buckets, so it
renders nowhere on the board.  Also the GATE every R72 date/status filter
token conjoins (`org-air-view--filter-date-token-match-p'): the filter
never resurrects what the board buries."
  (not (or (org-air-classify--done-p item)
           ;; R57-1 audit #5: an ARCHIVED heading (`org-archive-tag',
           ;; already inherited into `tags' per the user's tag-inheritance
           ;; settings, so whole ARCHIVE trees drop with default Org
           ;; config) is history, never board material — mirrors
           ;; `org-agenda-skip-archived-trees''s default.
           (member org-archive-tag (org-air-item-tags item)))))

(defun org-air-classify--overdue-p (item now)
  "Non-nil when ITEM's scheduled or deadline lies before today (NOW).
The Needs-attention bucket's overdue disjunct, hoisted (R72) — also the
board filter's `is:overdue' predicate, so filter and bucket share ONE
definition of \"overdue\"."
  (or (org-air-classify--past-p (org-air-item-scheduled item) now)
      (org-air-classify--past-p (org-air-item-deadline item) now)))

(defun org-air-classify--due-within-p (item now days &optional slot)
  "Non-nil when ITEM has a date within [NOW .. NOW+DAYS] in SLOT.
SLOT is `due' (the default: scheduled OR deadline), `scheduled' or
`deadline'.  Overdue never counts (0 <= d <= DAYS — the Upcoming bucket's
own inequality via `org-air-classify--future-or-today-p').  The Upcoming
bucket IS (org-air-classify--due-within-p ITEM NOW `org-air-upcoming-days')
— also the board filter's `is:upcoming' / `due:Nd' / `scheduled:Nd' /
`deadline:Nd' predicate (R72), so filter and bucket agree by construction."
  (pcase (or slot 'due)
    ('scheduled (org-air-classify--future-or-today-p
                 (org-air-item-scheduled item) now days))
    ('deadline (org-air-classify--future-or-today-p
                (org-air-item-deadline item) now days))
    (_ (or (org-air-classify--future-or-today-p
            (org-air-item-scheduled item) now days)
           (org-air-classify--future-or-today-p
            (org-air-item-deadline item) now days)))))

(defun org-air-classify--stale-p (item now)
  "Non-nil when ITEM is stale as of NOW: R54-1 eligibility + the clock.
The Stale bucket body hoisted whole (R72): eligible (scheduled ∨ deadline
∨ active <ts> — `org-air-classify--stale-eligible-p') AND the activity gap
\(`org-air-classify--last-activity') is >= `org-air-stale-days'.  Also the
board filter's `is:stale' predicate."
  (and (org-air-classify--stale-eligible-p item)
       (when-let* ((activity (org-air-classify--last-activity item)))
         (>= (org-air-classify--days-between activity now)
             org-air-stale-days))
       t))

(defun org-air-classify--hipri-p (item)
  "Non-nil when ITEM carries the highest priority (R72).
The High-priority bucket test hoisted (priority >= `org-priority-highest')
— also the board filter's `is:hipri' predicate."
  (and (org-air-item-priority item)
       (>= (org-air-item-priority item)
           (org-get-priority (format "[#%c]" org-priority-highest)))
       t))

(defun org-air-classify--container-p (item)
  "Non-nil when ITEM is a pure CONTAINER heading (R59).
A thin alias over `org-air-query-container-item-p' (layer symmetry with
the other classify predicates).  Deliberately NO live-marker fallback
probe (unlike `org-air-classify--marker-active-ts'): items built outside
the scan carry nil signal slots and are never containers — the safe
answer for an unknown is \"render it\"."
  (org-air-query-container-item-p item))

(defun org-air-classify--task-routed-p (item)
  "Non-nil when ITEM routes to the task buckets (the R54-2 routing layer).
The routing half of the R72 agreement law (R77): date/status filter
tokens are TASK vocabulary, so they must not match an item the routing
layer sends to a note bucket (`knowledge'/`journal'/`container'/
`notes').  Mirrors `org-air-classify-item' order exactly — keep the two
in lockstep; nil-ntype items (built outside the scan) pass — they take
the full task treatment there too."
  (and (not (eq (org-air-item-kind item) 'file))
       (not (org-air-classify--container-p item))
       (or (org-air-classify--inbox-dweller-p item)
           (not (memq (org-air-item-ntype item) '(journal knowledge))))))

;;;###autoload
(defun org-air-classify-item (item &optional now)
  "Return bucket symbols for ITEM relative to NOW.

Buckets are `upcoming', `stale', `attention', `high-priority', `inbox',
plus the non-board `notes', `container', `knowledge' and `journal'.
R53 P3: a `kind' `file' item (a headingless note synthesised by the scan)
routes to the dedicated `notes' bucket FIRST and never enters the task
buckets — the GTD board stays a GTD board.
R59: a pure CONTAINER heading (has children, no TODO keyword, no own
date — `org-air-classify--container-p') routes to the `container'
bucket, which has NO board section — skipped as a row EVERYWHERE,
including the Inbox.  The branch sits BEFORE the inbox bypass, which is
exactly where the container used to leak in: only container PARENTS
skip — a keyword-less dateless inbox LEAF (a bare captured note) still
rides the bypass into the `inbox' bucket (a real triage unit).
R54-2 routing layer (pure, slot-only): an inbox-dweller BYPASSES the type
signals into the task buckets (a schedule-less capture is an unfiled
task-to-be — the xsqrnoyn inbox semantics are unchanged); a `journal' /
`knowledge' `ntype' item lands in its own bucket, which has NO board
section — invisible on the GTD board, countable by the note surfaces.
A nil `ntype' (an item built outside the scan) keeps the full task
treatment.
R77: `org-air-classify--task-routed-p' MIRRORS this routing order for
the R72 date/status filter gate — any change here must land there too
\(seam r77-4's agreement theorem is the drift fence)."
  (cond
   ((eq (org-air-item-kind item) 'file) (list 'notes))
   ((org-air-classify--container-p item) (list 'container))
   ((org-air-classify--inbox-dweller-p item)
    (org-air-classify--heading-buckets item now))
   ((eq (org-air-item-ntype item) 'journal) (list 'journal))
   ((eq (org-air-item-ntype item) 'knowledge) (list 'knowledge))
   (t (org-air-classify--heading-buckets item now))))

(defun org-air-classify--heading-buckets (item now)
  "Return the task-bucket symbols for a heading ITEM relative to NOW.
R72: rewritten onto the hoisted named predicates — the top gate
\(`org-air-classify--board-active-p'), Upcoming
\(`org-air-classify--due-within-p'), overdue
\(`org-air-classify--overdue-p'), high priority
\(`org-air-classify--hipri-p') and Stale (`org-air-classify--stale-p') —
behaviour-neutral, so the buckets and the R72 date/status filter tokens
share ONE definition of every date word (agreement by construction)."
  (let* ((now (or now (current-time)))
         (buckets nil)
         (scheduled (org-air-item-scheduled item))
         (deadline (org-air-item-deadline item))
         (inbox-p (org-air-classify--inbox-dweller-p item)))
    (when (org-air-classify--board-active-p item)
      (when (org-air-classify--due-within-p item now org-air-upcoming-days)
        (push 'upcoming buckets))
      ;; Real-signal membership ruling (xsqrnoyn): an overdue item needs
      ;; attention, but the NO-DATE attention default is suppressed for
      ;; inbox-dwellers — a schedule-less inbox capture is unfiled, not
      ;; "needs attention" (it stays in Inbox).  Real scheduled/deadline/
      ;; priority membership is still honoured everywhere.
      (when (or (org-air-classify--overdue-p item now)
                (and (null scheduled) (null deadline) (not inbox-p)))
        (push 'attention buckets))
      (when (org-air-classify--hipri-p item)
        (push 'high-priority buckets))
      (when inbox-p
        (push 'inbox buckets))
      ;; R54-1: the stale-ELIGIBILITY gate is the FIRST conjunct — an item
      ;; with no actionable date is not a task and can never go Stale.
      ;; The stale CLOCK (`org-air-classify--last-activity') is unchanged,
      ;; so a dated-but-quiet item classifies byte-identically to before.
      (when (org-air-classify--stale-p item now)
        (push 'stale buckets)))
    (nreverse buckets)))

(provide 'org-air-classify)

;; Local Variables:
;; package-lint-main-file: "org-air.el"
;; End:
;;; org-air-classify.el ends here
