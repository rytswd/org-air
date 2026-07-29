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

(defcustom org-air-attention-days
  '((?A . 3) (?B . 7) (?C . 14) (?D . 30) (?E . 30) (nil . 30))
  "Days of NO UPDATE before a board item needs attention, by priority (R93).
An alist mapping a priority CHARACTER (the letter inside `[#A]') to the
number of calendar days the item may stay quiet before it surfaces in
the Needs-attention section; the NIL key is the threshold for a
heading carrying NO priority cookie.

The defaults give each priority its own patience: `#A' after three days,
`#B' after a week, `#C' after a fortnight, and `#D' / `#E' /
no-priority after a month.

R93 FIX-3 raised the `#A' default from 0 to 3 (user ruling).  At 0,
High priority — which IS the `#A' set — was a permanent SUBSET of
Needs attention: every `#A' row was printed twice on every board, and
the reason cell had to print the word `always' instead of a number.
Each section now owns one job: High priority means \"always visible\",
Needs attention means \"has gone quiet\".  An `#A' is still seen the
instant it exists (High priority), and it ADDITIONALLY surfaces in
Needs attention once it has genuinely been ignored for three days.

A threshold of 0 is still fully supported and still means UNCONDITIONAL
\(`org-air-classify--attention-p'): set `(?A . 0)' back deliberately and
every `#A' surfaces the moment it exists, unknown age included.  Only
the DEFAULT moved; the mechanism did not.

The clock is the item's `updated' fact (`org-air-item-updated' — the
newest INACTIVE Org timestamp in the heading's own body: LOGBOOK state
changes and notes, clock-outs, CLOSED, CREATED; see
`org-air-query--newest-inactive-stamp').  A SCHEDULED or DEADLINE date is
a PLAN, not an update: it neither starts nor stops this clock, so nothing
has to be scheduled to stay off this section — that rule (R93) replaced
the pre-R93 \"dateless items need attention\" default, which pushed users
to tag everything `:backlog:' just to stop seeing it.

R94: the clock is MEASURED ONLY.  It no longer falls back to the source
file's mtime, because that is a file fact answering a heading question —
it moved whenever org-air itself wrote the file, it made a one-file setup
all-or-nothing, and it made recovery a burst.  A heading with no recorded
history of its own is never aged here at all; if it also has no date it
gets its own row in the Untracked section instead
\(`org-air-classify--untracked-p').

A priority not listed falls back to the NIL entry, then to
`org-air-attention-default-days'.  Read LIVE at classify time and part
of the render memo key, so a `setq' takes effect on the next repaint,
never a rescan."
  :type '(alist :key-type (choice (character :tag "Priority letter")
                                  (const :tag "No priority" nil))
                :value-type (integer :tag "Days"))
  :group 'org-air)

(defcustom org-air-attention-default-days 30
  "Fallback quiet period for a priority missing from `org-air-attention-days'.
Only consulted when that alist has neither the item's priority letter
nor a NIL entry (R93)."
  :type 'integer
  :group 'org-air)

(defcustom org-air-upcoming-days 7
  "Number of calendar days ahead considered upcoming."
  :type 'integer
  :group 'org-air)

(defcustom org-air-backlog-tag "backlog"
  "Org tag that defers a board item onto the Backlog lens (R83).
A heading carrying this tag routes OUT of the task sections
\(Overdue / Upcoming / High priority / Needs attention) and the Inbox into
the single `backlog' bucket -- off the \"needs attention\" surfaces while
staying trackable (a Backlog section + a rail Summary count) and fully
reachable elsewhere (Notes, all-items, the day view, the calendar).  The
board key `b' (`org-air-item-backlog') toggles the tag on the item at
point; `is:backlog' filters to exactly the backlog set; `#<tag>' filters
by raw tag membership (its superset).  The tag is written to the source
heading and re-scanned like any tag; the NAME is a live classify input
\(the render memo carries it -- org-air-view.el), so a mid-session change
takes effect on the next repaint, never a file rescan."
  :type 'string
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

(defun org-air-classify--dated-p (item)
  "Non-nil when ITEM carries an actionable date (R54-1, renamed R93).
Scheduled, deadline, or an active timestamp in the subtree; a live-marker
item built outside the scan probes its buffer with `org-ts-regexp'
\(bounded, mirroring `org-air-classify--marker-timestamp-time').
R93: Stale is retired, so this is no longer a bucket gate — it survives
as the ONE definition of \"has a date\" behind the `is:nodate' filter
token (its negation).  Renamed from `org-air-classify--stale-eligible-p',
which is kept as a deprecated alias below."
  (or (org-air-item-scheduled item)
      (org-air-item-deadline item)
      (org-air-item-active-ts item)
      (org-air-classify--marker-active-ts item)))

(defalias 'org-air-classify--stale-eligible-p #'org-air-classify--dated-p
  "Deprecated R54-1 spelling of `org-air-classify--dated-p' (R93).
The predicate is unchanged — only Stale, the bucket it used to gate, is
retired.  Kept so existing callers keep working rather than signalling.")

(defun org-air-classify--planned-p (item)
  "Non-nil when ITEM carries a PLAN: a SCHEDULED or a DEADLINE (R95).
The ONE definition of \"has a plan\" that the DATE SECTIONS actually
consume — `org-air-classify--overdue-p' and
`org-air-classify--due-within-p' read exactly these two slots and nothing
else — hoisted so `org-air-classify--untracked-p' can ask the same
question they answer.

WHY IT IS NOT `org-air-classify--dated-p'.  That predicate is BROADER: it
also counts an active `<timestamp>' anywhere in the subtree, because it
answers the calendar's question (\"does this heading put a mark on a
day?\") behind the `is:nodate' token.  R94 built Untracked on the broad
notion and the R94 review measured the gap between the two: a `TODO'
whose ONLY date is a bare active stamp in its BODY was excluded from
Untracked for being dated, while Overdue and Upcoming never looked at
that stamp — so it had NO ROW ANYWHERE, at any file age, even with the
stamp yesterday or tomorrow.  Untracked is the catch-all for the date
sections, so it must be the negation of what the date sections read.

The two notions stay separate on purpose: `is:nodate' is still the
`--dated-p' negation (R54-1), so `is:untracked' is no longer a subset of
it in either direction — they answer different questions."
  (or (org-air-item-scheduled item)
      (org-air-item-deadline item)))

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
R93: this is now the `overdue' BUCKET's own body — Overdue is its own
section, no longer a disjunct of Needs-attention — as well as the board
filter's `is:overdue' predicate, so section and token share ONE
definition of \"overdue\".  A missed date is a different fact from a
quiet one and deserves its own place on the board."
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

(defun org-air-classify--priority-char (item)
  "Return ITEM's priority LETTER as a character, or nil (R93).
The classify-layer twin of `org-air-view--priority-char': Org stores a
cookie as (1000 × (`org-priority-lowest' - LETTER)), so the letter is
recovered by inverting that.  A cookie-less heading answers nil, which
is the `org-air-attention-days' NIL key."
  (when-let* ((priority (org-air-item-priority item)))
    (let ((char (- org-priority-lowest (/ priority 1000))))
      (and (characterp char) char))))

(defun org-air-classify-attention-threshold (item)
  "Return ITEM's quiet period in days before it needs attention (R93).
`org-air-attention-days' keyed by ITEM's priority letter, falling back
to that alist's NIL (no-priority) entry and then to
`org-air-attention-default-days'.  Never negative: 0 means \"always
surfaces\" — no longer a default anywhere (R93 FIX-3 moved `#A' to 3),
but an opt-in a user can still set.  Public because the row/inspector
reason labels must show the SAME number the bucket applied."
  (let* ((char (org-air-classify--priority-char item))
         (cell (or (assq char org-air-attention-days)
                   (assq nil org-air-attention-days))))
    (max 0 (or (cdr cell) org-air-attention-default-days))))

(defun org-air-classify-updated (item)
  "Return the epoch of ITEM's own last UPDATE, or nil (R93; R94).  No I/O.
The scan-time `updated' slot: the newest non-future INACTIVE timestamp
in the HEADING'S OWN BODY — LOGBOOK state changes and notes, clock-outs,
`CLOSED:', `:CREATED:', any body `[stamp]'.  A MEASURED, per-heading
fact or nothing.  nil means org-air has no record of anything ever
happening to this heading (see `org-air-classify--untracked-p').

R94 REMOVED the file-mtime fallback from this function.  It is a
FILE-level fact and it was silently answering a HEADING-level question:
because the mtime moves whenever ANY byte of the file moves — including
when org-air itself writes it (the board's state-change and backlog
keys, refile, capture, archive) — a
historyless heading in a file you are working in read as age 0 and
vanished from every clock-driven surface.  Measured by the R93 review: a
`tasks.org' edited today hid 15 of its 20 headings, and the project's own
demo fixture lost 2 of 23.  The floor survives, honestly scoped, as
`org-air-classify-updated-floor'.

SCHEDULED / DEADLINE / active `<timestamps>' are excluded by
construction: a plan is not an update (the R93 recency ruling, made
mechanical for the inactive spelling too by R94's
`org-air-query--plan-stamp-p')."
  (org-air-item-updated item))

(defun org-air-classify-updated-floor (item)
  "Return ITEM's FILE-level quiet floor as an epoch, or nil (R94).  No I/O.
The source file's scan-time mtime, read out of `org-air-query-file-meta'
\(a hash lookup on the table the cache hydrates — never a
`file-attributes' call).

WHAT THIS NUMBER IS.  Any edit to the heading is an edit to the file, so
the heading's last change is never LATER than the file's mtime: the age
derived from it is a sound LOWER BOUND on the heading's real age (\"quiet
for at LEAST N days\").  It is sound when it ACCUSES and unsound when it
EXCUSES — a fresh mtime proves nothing whatsoever about a heading that
has no history of its own.

WHERE org-air USES IT (R94).  Only where a lower bound is meaningful:
ranking and labelling the Untracked section, where it prints with a
leading `~' to say it is file-level, never heading-level.  It no longer
decides membership of ANY section, which is what makes org-air's own
writes, and the single-file all-or-nothing behaviour, harmless."
  (when-let* ((file (org-air-item-file item))
              (meta (org-air-query-file-meta file)))
    (plist-get meta :mtime)))

(defun org-air-classify-updated-source (item)
  "Return WHERE ITEM's recency number comes from: `measured', `file' or nil.
R94, public: the row label, the sort and the rail inspector must all be
able to say whether a number is a HEADING fact or a FILE bound, and they
must agree, so they ask this one function instead of each re-deriving it.

  `measured'  the heading's own `updated' slot answered
              \(`org-air-classify-updated');
  `file'      only the file-level floor answered
              \(`org-air-classify-updated-floor') — a LOWER BOUND,
              printed with a leading `~';
  nil         neither — UNKNOWN, and org-air never invents a number."
  (cond ((org-air-classify-updated item) 'measured)
        ((org-air-classify-updated-floor item) 'file)))

(defun org-air-classify-quiet-days (item now)
  "Return calendar days since ITEM's OWN last update as of NOW, or nil.
R93, narrowed by R94 to the MEASURED clock: the number this returns is
always a fact about the heading, never about its file, so the Needs
attention section's rows print heading history and nothing else.
Floored at 0 so a clock skew or a stamp written slightly ahead can never
read as negative age (which would silently suppress a `#A' item).  nil
when the heading has no recorded history at all — UNKNOWN, never
\"fresh\": see `org-air-classify--attention-p' and
`org-air-classify--untracked-p'."
  (when-let* ((epoch (org-air-classify-updated item)))
    (max 0 (org-air-classify--days-between epoch now))))

(defun org-air-classify-quiet-floor-days (item now)
  "Return the FILE-level lower bound on ITEM's quiet days as of NOW, or nil.
R94: `org-air-classify-updated-floor' in days, floored at 0.  Read as
\"quiet for at LEAST this long\" — it is the Untracked section's ranking
and label fact and nothing else's.  nil when the item has no file meta
\(an item built outside the scan)."
  (when-let* ((epoch (org-air-classify-updated-floor item)))
    (max 0 (org-air-classify--days-between epoch now))))

(defun org-air-classify--attention-p (item now)
  "Non-nil when ITEM has been quiet long enough, as of NOW, to need attention.
The R93 Needs-attention bucket body AND the `is:attention' filter predicate
\(with `is:stale' kept as a deprecated alias) — ONE definition, so
section and token agree by construction.

ITEM needs attention when its quiet period (`org-air-classify-quiet-days')
has reached its priority threshold (`org-air-classify-attention-threshold').
A threshold of 0 surfaces UNCONDITIONALLY, including when the age is
unknown: at 0 the user has said \"never make this earn its row\", so
there is nothing left to measure.  An UNKNOWN age at any POSITIVE
threshold does NOT surface: org-air refuses to nag about something it
cannot date.

R94 — THE CLOCK IS MEASURED ONLY.  `org-air-classify-quiet-days' no
longer falls back to the source file's mtime, so membership of this
section is decided entirely by the heading's OWN recorded history.  Three
consequences, all of them the point:

  * org-air's own writes (the board's state-change and backlog keys,
    refile, capture, archive) can no
    longer change who is in this section — acting on one row cannot
    silence another;
  * a one-file user no longer gets all-or-nothing behaviour, and there is
    no BURST when a file finally goes quiet (ten headings arriving on the
    same day carrying the same number);
  * every number this section prints is a fact about the heading it is
    printed next to.

Work with no history of its own is not silently dropped for it: it is
exactly `org-air-classify--untracked-p', which gives it a permanent,
non-accusing home of its own.

R93 FIX-3 note — since the `#A' default is now 3, not 0, an `#A' whose
age is UNKNOWN no longer surfaces here.  That is DELIBERATE and follows
from the same rule, not from an accident of the default: a positive
threshold is a claim about elapsed time, and org-air will not assert
elapsed time it never measured.  Such an item is not lost — it is a
`#A', so High priority shows it on every repaint.  Setting `(?A . 0)'
back restores the unconditional behaviour, unknown ages included.

What this rule deliberately does NOT do (the R93 problem statement): it
never asks whether the item is scheduled.  The pre-R93 bucket surfaced
EVERY dateless board item, so the only way to stop seeing a real-but-
not-yet-plannable task was to date it (a lie) or tag it `:backlog:'
\(gaming the board).  Overdue moved OUT to its own bucket
\(`org-air-classify--overdue-p'); a date now grants no exemption and
costs no penalty."
  (let ((threshold (org-air-classify-attention-threshold item)))
    (and (or (<= threshold 0)
             (when-let* ((age (org-air-classify-quiet-days item now)))
               (>= age threshold)))
         t)))

(defun org-air-classify--untracked-p (item)
  "Non-nil when org-air knows NOTHING about ITEM: no plan and no record (R94).
The Untracked bucket's body AND the `is:untracked' filter predicate — ONE
definition, so section and token agree by construction.

  no plan    `org-air-classify--planned-p' is nil: no SCHEDULED and no
             DEADLINE — the two slots Overdue and Upcoming read, so this
             is exactly the negation of \"some date section can show it\";
  no record  `org-air-classify-updated' is nil: no LOGBOOK state change
             or note, no clock-out, no `CLOSED:', no `:CREATED:', no body
             stamp — nothing Org writes when something happens;
  no queue   the heading is not an inbox dweller
             \(`org-air-classify--inbox-dweller-p'): Inbox already holds
             it, and holds it unconditionally.

R95 CHANGED TWO THINGS HERE, both of them coverage.

1. THE PLAN CLAUSE IS NOW THE ONE THE DATE SECTIONS READ.  R94 asked
   `org-air-classify--dated-p', which also counts an active `<timestamp>'
   anywhere in the subtree — a broader notion of \"planned\" than Overdue
   and Upcoming consume.  Every heading in the gap fell out of the
   catch-all without ever reaching a date section: a `TODO' whose only
   date is a bare `<2026-06-14 Sun>' in its body had NO ROW ANYWHERE and
   answered no `is:' token at all (`is:nodate' refused it — it IS
   dated).  Measured by the R94 review through the real renderer, with
   the stamp yesterday, tomorrow and three months out, at both file
   ages.  Asking `org-air-classify--planned-p' closes it: the catch-all
   is now the exact negation of the sections it backstops.

2. AN INBOX DWELLER IS NOT UNTRACKED.  An unprocessed capture has no
   plan and no record BY DEFINITION, so the Untracked row said nothing
   the Inbox row had not already said, in a section capped at four rows
   \(1 of the 3 rows on the shipped demo board; 2 of 21 on the review's
   corpus).  This is NOT the R93 inbox carve-out coming back — that one
   was about AGING and it HID work.  This one hides nothing and cannot:
   `org-air-classify--heading-buckets' gives every board-active,
   non-deferred inbox dweller the `inbox' bucket UNCONDITIONALLY, so the
   coverage theorem holds by construction.  Inbox is the section that
   already names what to do with an undated, unrecorded heading —
   process it — and Untracked is for work that has no such home.

WHY THE SECTION EXISTS.  R93 replaced \"a dateless item needs attention\"
with an aging rule, which was right, and R94 took the file mtime out of
that rule, which was also right — but between them they leave a set of
headings with no clock and no date, and therefore no row anywhere.  The
R93 review measured the cost: a `tasks.org' edited today hid 15 of 20
headings; the shipped demo board lost 2 of 23.  This bucket is that set,
named.

WHY IT IS NOT THE DELETED RULE COMING BACK.  It makes no claim of
neglect: it does not say the work is late, it says org-air cannot rank it
\(quiet `◌' glyph, last of the task sections, never an attention badge).
It cannot grow over time — only shrink — and one Org stamp (any state
change with `org-log-done' / `org-log-into-drawer' on) or one date empties
a heading out of it permanently.  The pre-R93 rule did the opposite: it
put undated work in the ALARM lane, ranked above real overdue work, with
no exit except inventing a date or tagging `:backlog:'.

THE OVERLAP RULE STILL STANDS (R93 decision 3) wherever the two rows say
different things: an untracked `#A' shows in High priority AND here, and
both statements are true and distinct (\"this matters\" / \"org-air cannot
rank it\").  The R95 inbox clause is not an exception to that rule but an
application of it — for an unprocessed capture the two rows say the SAME
thing, so only the one with a verb is kept.  A `:backlog:' heading routes
out of this bucket with all the others
\(`org-air-classify--heading-buckets')."
  (and (null (org-air-classify--planned-p item))
       (null (org-air-classify-updated item))
       (not (org-air-classify--inbox-dweller-p item))
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

(defun org-air-classify--backlog-p (item)
  "Non-nil when ITEM carries `org-air-backlog-tag' — the deferred flag (R83).
A live membership test over the already-scanned `tags' slot (the tag NAME
is a classify input; the tag VALUE is re-scanned).  Consulted by
`org-air-classify--heading-buckets' (routes off the four task buckets +
Inbox into `backlog') and by the `is:backlog' filter token — the ONE
definition both share, so filter⇔bucket agreement holds by construction."
  (and org-air-backlog-tag
       (member org-air-backlog-tag (org-air-item-tags item))
       t))

;;;###autoload
(defun org-air-classify-item (item &optional now)
  "Return bucket symbols for ITEM relative to NOW.

Buckets are `upcoming', `overdue', `attention', `high-priority', `inbox',
the R94 `untracked', the R83 `backlog' (a board-active deferred item, off
the task buckets), plus the non-board `notes', `container', `knowledge'
and `journal'.
R93: `overdue' is a bucket of its own and `stale' is RETIRED — its
\"dated but quiet\" rule is subsumed by the aging `attention' rule.
R94: `untracked' — no plan and no recorded history — joins, so that
removing the file-mtime floor from the aging rule cannot make work
invisible.
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
\(`org-air-classify--due-within-p'), Overdue
\(`org-air-classify--overdue-p'), high priority
\(`org-air-classify--hipri-p') — so the buckets and the R72 date/status
filter tokens share ONE definition of every date word (agreement by
construction).
R93: Overdue is its own bucket; Needs attention is the AGING rule
\(`org-air-classify--attention-p'); Stale is gone.  Buckets stay
NON-EXCLUSIVE, exactly as before: an item may hold several at once (an
overdue `#A' that has been quiet a month is in Overdue, High priority
AND Needs attention), and each section shows it.
R94: `untracked' (`org-air-classify--untracked-p') closes the covering.
R95 states the covering as it is actually true — THE COVERAGE THEOREM:

  Every board-active, non-deferred task heading has a row somewhere,
  UNLESS its own facts defer it, which happens in exactly two ways:
    (a) its PLAN (SCHEDULED/DEADLINE) puts it beyond the Upcoming
        horizon; or
    (b) org-air MEASURED activity on it more recently than its
        priority's threshold.
  There is no third case.

Both exemptions are promises rather than hiding places: (a) resurfaces on
its own date, (b) resurfaces when the heading goes quiet.  R94 shipped
the sentence with clause (a) alone and with a third case it did not know
about (a plan written as a body `<timestamp>' reached neither Untracked
nor a date section); R95's `org-air-classify--planned-p' closes that case
and the sentence above is now true without an asterisk."
  (let* ((now (or now (current-time)))
         (buckets nil)
         (inbox-p (org-air-classify--inbox-dweller-p item)))
    (when (org-air-classify--board-active-p item)
      (if (org-air-classify--backlog-p item)
          ;; R83: a board-active DEFERRED item routes OFF the four task
          ;; buckets + Inbox into the SINGLE `backlog' home — off the
          ;; attention surfaces, still trackable (a Backlog section + a
          ;; rail count).  The gate sits INSIDE the board-active branch,
          ;; so a DONE / archived backlog heading still classifies into
          ;; ZERO buckets (backlog never resurrects history).  Mirrors
          ;; R77's subtractive shape but keeps the item a TASK (a tag
          ;; overlay, not a re-typed note).
          (push 'backlog buckets)
        ;; R93 board order: Overdue, Upcoming, High priority, Needs
        ;; attention (Inbox is rendered first but classified here).
        (when (org-air-classify--overdue-p item now)
          (push 'overdue buckets))
        (when (org-air-classify--due-within-p item now org-air-upcoming-days)
          (push 'upcoming buckets))
        (when (org-air-classify--hipri-p item)
          (push 'high-priority buckets))
        ;; R93: the AGING rule, total and date-free.  Deliberately NO
        ;; inbox carve-out: the pre-R93 exemption existed only to keep
        ;; schedule-less captures out of the NO-DATE default, and that
        ;; default is gone.  A capture that has sat untouched past its
        ;; threshold genuinely needs attention, and overlap is the
        ;; standing rule — it simply shows in Inbox AND here.
        (when (org-air-classify--attention-p item now)
          (push 'attention buckets))
        ;; R94: the home of last resort.  A heading with no plan AND no
        ;; recorded history has no clock to age off and no date to be due
        ;; on, so before R94 it had no row anywhere once the file-mtime
        ;; floor stopped excusing it.  It gets one here — a statement, not
        ;; an accusation.  R95: "plan" here is the SCHEDULED/DEADLINE pair
        ;; the date sections read (`org-air-classify--planned-p'), so the
        ;; catch-all is the exact negation of the sections it backstops;
        ;; and an inbox dweller is excluded, because the `inbox' push
        ;; below is UNCONDITIONAL for exactly the same heading — the two
        ;; rows would have said the same thing.
        (when (org-air-classify--untracked-p item)
          (push 'untracked buckets))
        (when inbox-p
          (push 'inbox buckets))))
    (nreverse buckets)))

(provide 'org-air-classify)

;; Local Variables:
;; package-lint-main-file: "org-air.el"
;; End:
;;; org-air-classify.el ends here
