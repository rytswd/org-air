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
  "Days of NO UPDATE before a board item needs attention, by priority.
An alist mapping a priority CHARACTER (the letter inside `[#A]') to the
number of calendar days the item may stay quiet before it surfaces in
the Needs-attention section; the NIL key is the threshold for a heading
carrying NO priority cookie.  A priority missing from the alist falls
back to the NIL entry, then to `org-air-attention-default-days'.

The defaults give each priority its own patience: `#A' after three days,
`#B' after a week, `#C' after a fortnight, and `#D' / `#E' /
no-priority after a month.  So each section owns one job: High priority
means \"always visible\", Needs attention means \"has gone quiet\".

A threshold of 0 means UNCONDITIONAL
\(`org-air-classify--attention-p'): every item of that priority surfaces
the moment it exists, unknown age included.  That is supported but not a
default, because at `(?A . 0)' High priority — which IS the `#A' set —
becomes a permanent SUBSET of Needs attention: every `#A' row prints
twice and its reason cell reads `always' instead of a number.

The clock is the item's `updated' fact (`org-air-item-updated' — the
newest INACTIVE Org timestamp in the heading's own body: LOGBOOK state
changes and notes, clock-outs, CLOSED, CREATED; see
`org-air-query--newest-inactive-stamp').  A SCHEDULED or DEADLINE date is
a PLAN, not an update: it neither starts nor stops this clock, so nothing
has to be scheduled to stay off this section.

The clock is MEASURED ONLY.  It deliberately does NOT fall back to the
source file's mtime: that is a file fact answering a heading question,
it moves whenever org-air itself writes the file, it makes a one-file
setup all-or-nothing, and it makes recovery a burst.  A heading with no
recorded history of its own is never aged here at all; if it also has no
date it gets its own row in the Untracked section instead
\(`org-air-classify--untracked-p').

Read LIVE at classify time and part of the render memo key, so a `setq'
takes effect on the next repaint, never a rescan."
  :type '(alist :key-type (choice (character :tag "Priority letter")
                                  (const :tag "No priority" nil))
                :value-type (integer :tag "Days"))
  :group 'org-air)

(defcustom org-air-attention-default-days 30
  "Fallback quiet period for a priority missing from `org-air-attention-days'.
Only consulted when that alist has neither the item's priority letter
nor a NIL entry."
  :type 'integer
  :group 'org-air)

(defcustom org-air-upcoming-days 7
  "Number of calendar days ahead considered upcoming."
  :type 'integer
  :group 'org-air)

(defcustom org-air-backlog-tag "backlog"
  "Org tag that defers a board item onto the Backlog lens.
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
Only a LIVE marker resolves; a cache-hydrated (FILE . POS) cons returns
nil, because everything classify and render need lives in the item's
scan-time slots (`donep'/`activity'/`body-deadline').  RENDER NEVER
OPENS A FILE — resolving the cons here costs a `find-file-noselect' per
item, measured as a 186s warm first paint over 15.9k items."
  (let ((m (org-air-item-marker item)))
    (when (and (markerp m) (marker-buffer m))
      (cons (marker-buffer m) (marker-position m)))))

(defun org-air-classify--done-keywords (item)
  "Return done TODO keywords applicable to ITEM WITHOUT opening a file.
The scan records `donep' at scan time, so this is only the fallback
vocabulary for items built OUTSIDE the scan (a live capture buffer):
the live marker's buffer keywords (a real buffer still knows best),
else the MERGED scan vocabulary's done set
\(`org-air-query-merged-done-keywords' — the user's own global done
keywords plus org-air's supplement, never a hard-wired \"DONE\")."
  (or (when-let* ((src (org-air-classify--item-source item)))
        (with-current-buffer (car src)
          (or org-done-keywords (default-value 'org-done-keywords))))
      (default-value 'org-done-keywords)
      (org-air-query-merged-done-keywords)))

(defun org-air-classify--done-p (item)
  "Return non-nil if ITEM has a done TODO state.
Data-pure: the scan-time `donep' slot (todo ∈ the file's own
`org-done-keywords' as known in the scan buffer) answers without any file
access; items built outside the scan fall back to
`org-air-classify--done-keywords' (live buffer or global default)."
  (or (org-air-item-donep item)
      (when-let* ((todo (org-air-item-todo item)))
        (member todo (org-air-classify--done-keywords item)))))

(defun org-air-classify--future-or-today-p (timestamp now &optional days)
  "Return non-nil when TIMESTAMP is within the upcoming window from NOW.
DAYS is the window horizon in calendar days, defaulting to
`org-air-upcoming-days'.  The window is 0 <= d <= DAYS: today is in, the
past is out — the ONE inequality shared by the Upcoming bucket and the
`due:Nd' filter windows, so they agree by construction."
  (when-let* ((time (org-air-classify--time timestamp)))
    (let ((d (org-air-classify--days-between now time)))
      (and (>= d 0) (<= d (or days org-air-upcoming-days))))))

(defun org-air-classify--past-p (timestamp now)
  "Return non-nil when TIMESTAMP is before today relative to NOW."
  (when-let* ((time (org-air-classify--time timestamp)))
    (> (org-air-classify--days-between time now) 0)))

(defun org-air-classify--marker-timestamp-time (item)
  "Return the first timestamp time found in ITEM's subtree.
Works over a live marker or a cache-hydrated (FILE . POS) cons;
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
The live-marker fallback for items built OUTSIDE the scan (a capture
buffer, a unit test): the `org-air-classify--marker-timestamp-time'
walk with `org-ts-regexp' (active <ts> only, planning lines in)
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
  "Non-nil when ITEM carries an actionable date.
Scheduled, deadline, or an active timestamp in the subtree; a live-marker
item built outside the scan probes its buffer with `org-ts-regexp'
\(bounded, mirroring `org-air-classify--marker-timestamp-time').

This gates no bucket.  It is the ONE definition of \"has a date\" behind
the `is:nodate' filter token (its negation).  Not to be confused with
`org-air-classify--planned-p', which is narrower."
  (or (org-air-item-scheduled item)
      (org-air-item-deadline item)
      (org-air-item-active-ts item)
      (org-air-classify--marker-active-ts item)))

(defalias 'org-air-classify--stale-eligible-p #'org-air-classify--dated-p
  "Deprecated spelling of `org-air-classify--dated-p'.
Kept so existing callers keep working rather than signalling; the
Stale bucket it once gated is retired.")

(defun org-air-classify--planned-p (item)
  "Non-nil when ITEM carries a PLAN: a SCHEDULED or a DEADLINE.
The ONE definition of \"has a plan\" that the DATE SECTIONS actually
consume — `org-air-classify--overdue-p' and
`org-air-classify--due-within-p' read exactly these two slots and nothing
else — hoisted so `org-air-classify--untracked-p' can ask the same
question they answer.

WHY IT IS NOT `org-air-classify--dated-p'.  That predicate is BROADER:
it also counts an active `<timestamp>' anywhere in the subtree, because
it answers the calendar's question (\"does this heading put a mark on a
day?\") behind the `is:nodate' token.  Build Untracked on the broad
notion and a `TODO' whose ONLY date is a bare active stamp in its BODY
falls through every section: excluded from Untracked for being dated,
ignored by Overdue and Upcoming because they never read that stamp — NO
ROW ANYWHERE, at any file age.  Untracked is the catch-all for the date
sections, so it must be the negation of what the date sections read.

The two notions stay separate on purpose: `is:nodate' is the `--dated-p'
negation, so `is:untracked' is a subset of it in neither direction —
they answer different questions."
  (or (org-air-item-scheduled item)
      (org-air-item-deadline item)))

(defvar org-air-classify--truename-cache (make-hash-table :test #'equal)
  "Memo FILE -> truename for the inbox-membership test.
Bounded by the configured file count; avoids a `file-truename' component
walk per item at 15k items.")

(defun org-air-classify--truename (file)
  "Return FILE's memoised truename."
  (or (gethash file org-air-classify--truename-cache)
      (puthash file
               (or (ignore-errors (file-truename (expand-file-name file)))
                   (expand-file-name file))
               org-air-classify--truename-cache)))

(defun org-air-classify--inbox-file-p (item)
  "Return non-nil when ITEM lives in `org-air-inbox-file'.
Both truenames are memoised (`org-air-classify--truename-cache'),
so the per-item cost is a hash lookup, not a filesystem walk."
  (let ((inbox (org-air-inbox-effective-file)))
    (and inbox
         (org-air-item-file item)
         (equal (org-air-classify--truename (org-air-item-file item))
                (org-air-classify--truename inbox)))))

(defun org-air-classify--last-activity (item)
  "Return the best available activity time for ITEM.
The scan-time `activity' slot (an epoch float: closed ‖ scheduled ‖
deadline ‖ first subtree timestamp ‖ file mtime) answers directly for
every scanned item — no file access.  The chain below it survives only
as the fallback for items built outside the scan (live-marker probes
still work; a cons marker degrades to the file-mtime fallback)."
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
The memoised-truename inbox test, hoisted so the routing layer in
`org-air-classify-item' and the bucket pass share one definition."
  (or (org-air-classify--inbox-file-p item)
      (member "inbox" (mapcar #'downcase (org-air-item-tags item)))))

(defun org-air-classify--board-active-p (item)
  "Non-nil when ITEM is live board material: not done, not archived.
The factored top gate of `org-air-classify--heading-buckets' — a DONE or
`org-archive-tag'-tagged item classifies into ZERO task buckets, so it
renders nowhere on the board.  Also the GATE every date/status filter
token conjoins (`org-air-view--filter-date-token-match-p'): the filter
never resurrects what the board buries."
  (not (or (org-air-classify--done-p item)
           ;; An ARCHIVED heading (`org-archive-tag', already inherited
           ;; into `tags' per the user's tag-inheritance settings, so
           ;; whole ARCHIVE trees drop with default Org config) is
           ;; history, never board material — mirrors
           ;; `org-agenda-skip-archived-trees''s default.
           (member org-archive-tag (org-air-item-tags item)))))

(defun org-air-classify--overdue-p (item now)
  "Non-nil when ITEM's scheduled or deadline lies before today (NOW).
The `overdue' BUCKET's own body AND the board filter's `is:overdue'
predicate, so section and token share ONE definition of \"overdue\".  A
missed date is a different fact from a quiet one, which is why Overdue
is a section of its own rather than a disjunct of Needs attention."
  (or (org-air-classify--past-p (org-air-item-scheduled item) now)
      (org-air-classify--past-p (org-air-item-deadline item) now)))

(defun org-air-classify--due-within-p (item now days &optional slot)
  "Non-nil when ITEM has a date within [NOW .. NOW+DAYS] in SLOT.
SLOT is `due' (the default: scheduled OR deadline), `scheduled' or
`deadline'.  Overdue never counts (0 <= d <= DAYS — the Upcoming bucket's
own inequality via `org-air-classify--future-or-today-p').  The Upcoming
bucket IS (org-air-classify--due-within-p ITEM NOW `org-air-upcoming-days')
— also the board filter's `is:upcoming' / `due:Nd' / `scheduled:Nd' /
`deadline:Nd' predicate, so filter and bucket agree by construction."
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
  "Return ITEM's priority LETTER as a character, or nil.
The classify-layer twin of `org-air-view--priority-char': Org stores a
cookie as (1000 × (`org-priority-lowest' - LETTER)), so the letter is
recovered by inverting that.  A cookie-less heading answers nil, which
is the `org-air-attention-days' NIL key."
  (when-let* ((priority (org-air-item-priority item)))
    (let ((char (- org-priority-lowest (/ priority 1000))))
      (and (characterp char) char))))

(defun org-air-classify-attention-threshold (item)
  "Return ITEM's quiet period in days before it needs attention.
`org-air-attention-days' keyed by ITEM's priority letter, falling back
to that alist's NIL (no-priority) entry and then to
`org-air-attention-default-days'.  Never negative; 0 means \"always
surfaces\".  Public because the row and inspector reason labels must
show the SAME number the bucket applied."
  (let* ((char (org-air-classify--priority-char item))
         (cell (or (assq char org-air-attention-days)
                   (assq nil org-air-attention-days))))
    (max 0 (or (cdr cell) org-air-attention-default-days))))

(defun org-air-classify-updated (item)
  "Return the epoch of ITEM's own last UPDATE, or nil.  No I/O.
The scan-time `updated' slot: the newest non-future INACTIVE timestamp
in the HEADING'S OWN BODY — LOGBOOK state changes and notes, clock-outs,
`CLOSED:', `:CREATED:', any body `[stamp]'.  A MEASURED, per-heading
fact or nothing.  nil means org-air has no record of anything ever
happening to this heading (see `org-air-classify--untracked-p').

There is deliberately NO file-mtime fallback here.  The mtime is a
FILE-level fact answering a HEADING-level question: it moves whenever
ANY byte of the file moves — including when org-air itself writes it
\(the state-change and backlog keys, refile, capture, archive) — so a
historyless heading in a file you are working in reads as age 0 and
vanishes from every clock-driven surface.  Measured: a `tasks.org'
edited today hid 15 of its 20 headings.  The floor survives, honestly
scoped, as `org-air-classify-updated-floor'.

SCHEDULED / DEADLINE / active `<timestamps>' are excluded by
construction: a plan is not an update.  The inactive spelling of a plan
is excluded too, by `org-air-query--plan-stamp-p'."
  (org-air-item-updated item))

(defun org-air-classify-updated-floor (item)
  "Return ITEM's FILE-level quiet floor as an epoch, or nil.  No I/O.
The source file's scan-time mtime, read out of `org-air-query-file-meta'
\(a hash lookup on the table the cache hydrates — never a
`file-attributes' call).

WHAT THIS NUMBER IS.  Any edit to the heading is an edit to the file, so
the heading's last change is never LATER than the file's mtime: the age
derived from it is a sound LOWER BOUND on the heading's real age (\"quiet
for at LEAST N days\").  It is sound when it ACCUSES and unsound when it
EXCUSES — a fresh mtime proves nothing whatsoever about a heading that
has no history of its own.

WHERE org-air USES IT.  Only where a lower bound is meaningful: ranking
and labelling the Untracked section, where it prints with a leading `~'
to say it is file-level, never heading-level.  It decides membership of
NO section — that is what keeps org-air's own writes, and the
single-file all-or-nothing behaviour, harmless."
  (when-let* ((file (org-air-item-file item))
              (meta (org-air-query-file-meta file)))
    (plist-get meta :mtime)))

(defun org-air-classify-updated-source (item)
  "Return WHERE ITEM's recency number comes from: `measured', `file' or nil.
Public: the row label, the sort and the rail inspector must all be able
to say whether a number is a HEADING fact or a FILE bound, and they must
agree, so they ask this one function instead of each re-deriving it.

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
The MEASURED clock: the number this returns is always a fact about the
heading, never about its file, so the Needs attention section's rows
print heading history and nothing else.  Floored at 0 so a clock skew or
a stamp written slightly ahead can never read as negative age (which
would silently suppress a `#A' item).  nil
when the heading has no recorded history at all — UNKNOWN, never
\"fresh\": see `org-air-classify--attention-p' and
`org-air-classify--untracked-p'."
  (when-let* ((epoch (org-air-classify-updated item)))
    (max 0 (org-air-classify--days-between epoch now))))

(defun org-air-classify-quiet-floor-days (item now)
  "Return the FILE-level lower bound on ITEM's quiet days as of NOW, or nil.
`org-air-classify-updated-floor' in days, floored at 0.  Read as
\"quiet for at LEAST this long\" — it is the Untracked section's ranking
and label fact and nothing else's.  nil when the item has no file meta
\(an item built outside the scan)."
  (when-let* ((epoch (org-air-classify-updated-floor item)))
    (max 0 (org-air-classify--days-between epoch now))))

(defun org-air-classify--attention-p (item now)
  "Non-nil when ITEM has been quiet long enough, as of NOW, to need attention.
The Needs-attention bucket body AND the `is:attention' filter predicate
\(with `is:stale' kept as a deprecated alias) — ONE definition, so
section and token agree by construction.

ITEM needs attention when its quiet period (`org-air-classify-quiet-days')
has reached its priority threshold (`org-air-classify-attention-threshold').
A threshold of 0 surfaces UNCONDITIONALLY, including when the age is
unknown: at 0 the user has said \"never make this earn its row\", so
there is nothing left to measure.  An UNKNOWN age at any POSITIVE
threshold does NOT surface: org-air refuses to nag about something it
cannot date.

THE CLOCK IS MEASURED ONLY.  `org-air-classify-quiet-days' does not fall
back to the source file's mtime, so membership of this section is
decided entirely by the heading's OWN recorded history.  Three
consequences, all of them the point:

  * org-air's own writes (the state-change and backlog keys, refile,
    capture, archive) cannot change who is in this section — acting on
    one row cannot silence another;
  * a one-file user gets no all-or-nothing behaviour, and there is no
    BURST when a file finally goes quiet (ten headings arriving on the
    same day carrying the same number);
  * every number this section prints is a fact about the heading it is
    printed next to.

Work with no history of its own is not silently dropped for it: it is
exactly `org-air-classify--untracked-p', which gives it a permanent,
non-accusing home of its own.  So an `#A' of UNKNOWN age does not
surface here at the default threshold of 3, and that is deliberate: a
positive threshold is a claim about elapsed time, and org-air will not
assert elapsed time it never measured.  Such an item is not lost — it is
an `#A', so High priority shows it on every repaint.

What this rule deliberately does NOT do: it never asks whether the item
is scheduled.  A bucket that surfaces EVERY dateless board item leaves
only two ways to stop seeing a real-but-not-yet-plannable task — date it
\(a lie) or tag it `:backlog:' (gaming the board).  Overdue is its own
bucket (`org-air-classify--overdue-p'); a date grants no exemption here
and costs no penalty."
  (let ((threshold (org-air-classify-attention-threshold item)))
    (and (or (<= threshold 0)
             (when-let* ((age (org-air-classify-quiet-days item now)))
               (>= age threshold)))
         t)))

(defun org-air-classify--untracked-p (item)
  "Non-nil when org-air knows NOTHING about ITEM: no plan and no record.
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

WHY THE SECTION EXISTS.  The aging rule replaced \"a dateless item needs
attention\", and the measured-only clock took the file mtime out of that
rule; between them they leave a set of headings with no clock and no
date, and therefore no row anywhere.  This bucket is that set, named.

WHY THE PLAN CLAUSE IS `--planned-p' AND NOT `--dated-p'.  `--dated-p'
also counts an active `<timestamp>' anywhere in the subtree — a broader
notion of \"planned\" than Overdue and Upcoming consume.  Use it here and
every heading in the gap falls out of the catch-all without ever
reaching a date section: a `TODO' whose only date is a bare
`<2026-06-14 Sun>' in its body gets NO ROW ANYWHERE and answers no `is:'
token at all (`is:nodate' refuses it — it IS dated).  Asking
`--planned-p' makes the catch-all the exact negation of the sections it
backstops.

WHY AN INBOX DWELLER IS NOT UNTRACKED.  An unprocessed capture has no
plan and no record BY DEFINITION, so an Untracked row would repeat what
the Inbox row already says, in a section capped at four rows.  This
hides nothing and cannot: `org-air-classify--heading-buckets' gives
every board-active, non-deferred inbox dweller the `inbox' bucket
UNCONDITIONALLY.  Inbox already names what to do with an undated,
unrecorded heading — process it — and Untracked is for work that has no
such home.

WHY THIS IS NOT THE DELETED \"DATELESS = NEGLECTED\" RULE COMING BACK.
It makes no claim of neglect: it does not say the work is late, it says
org-air cannot rank it (quiet `◌' glyph, last of the task sections,
never an attention badge).  It cannot grow over time — only shrink — and
one Org stamp (any state change with `org-log-done' /
`org-log-into-drawer' on) or one date empties a heading out of it
permanently.  The deleted rule put undated work in the ALARM lane,
ranked above real overdue work, with no exit except inventing a date or
tagging `:backlog:'.

THE OVERLAP RULE STILL STANDS wherever the two rows say different
things: an untracked `#A' shows in High priority AND here, and both
statements are true and distinct (\"this matters\" / \"org-air cannot
rank it\").  The inbox clause above is an application of that rule, not
an exception to it — the two rows would say the SAME thing, so only the
one with a verb is kept.  A `:backlog:' heading routes out of this
bucket with all the others (`org-air-classify--heading-buckets')."
  (and (null (org-air-classify--planned-p item))
       (null (org-air-classify-updated item))
       (not (org-air-classify--inbox-dweller-p item))
       t))

(defun org-air-classify--hipri-p (item)
  "Non-nil when ITEM carries the highest priority.
The High-priority bucket test hoisted (priority >= `org-priority-highest')
— also the board filter's `is:hipri' predicate."
  (and (org-air-item-priority item)
       (>= (org-air-item-priority item)
           (org-get-priority (format "[#%c]" org-priority-highest)))
       t))

(defun org-air-classify--container-p (item)
  "Non-nil when ITEM is a pure CONTAINER heading.
A thin alias over `org-air-query-container-item-p' (layer symmetry with
the other classify predicates).  Deliberately NO live-marker fallback
probe (unlike `org-air-classify--marker-active-ts'): items built outside
the scan carry nil signal slots and are never containers — the safe
answer for an unknown is \"render it\"."
  (org-air-query-container-item-p item))

(defun org-air-classify--task-routed-p (item)
  "Non-nil when ITEM routes to the task buckets.
The routing half of the filter/bucket agreement law: date and status
filter tokens are TASK vocabulary, so they must not match an item the
routing layer sends to a note bucket (`knowledge'/`journal'/`container'/
`notes').  Mirrors `org-air-classify-item' order exactly — keep the two
in lockstep.  A nil `ntype' (an item built outside the scan) passes; it
takes the full task treatment there too."
  (and (not (eq (org-air-item-kind item) 'file))
       (not (org-air-classify--container-p item))
       (or (org-air-classify--inbox-dweller-p item)
           (not (memq (org-air-item-ntype item) '(journal knowledge))))))

(defun org-air-classify--backlog-p (item)
  "Non-nil when ITEM carries `org-air-backlog-tag' — the deferred flag.
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

Buckets are `upcoming', `overdue', `attention', `high-priority',
`inbox', `untracked', `backlog' (a board-active deferred item, off the
task buckets), plus the non-board `notes', `container', `knowledge' and
`journal'.  They are NON-EXCLUSIVE: an item may hold several at once.

The routing order is pure and slot-only:

  * a `kind' `file' item (a headingless note synthesised by the scan)
    routes to the dedicated `notes' bucket FIRST and never enters the
    task buckets — the GTD board stays a GTD board;
  * a pure CONTAINER heading (has children, no TODO keyword, no own
    date — `org-air-classify--container-p') routes to `container',
    which has NO board section, so it is skipped as a row EVERYWHERE,
    the Inbox included.  This branch sits BEFORE the inbox bypass on
    purpose: only container PARENTS skip, while a keyword-less dateless
    inbox LEAF (a bare captured note) still rides the bypass into
    `inbox', because that is a real triage unit;
  * an inbox dweller BYPASSES the type signals into the task buckets (a
    schedule-less capture is an unfiled task-to-be);
  * a `journal' / `knowledge' `ntype' item lands in its own bucket,
    which has NO board section — invisible on the GTD board, countable
    by the note surfaces;
  * a nil `ntype' (an item built outside the scan) keeps the full task
    treatment.

`org-air-classify--task-routed-p' MIRRORS this routing order for the
date/status filter gate — any change here must land there too."
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
Built entirely from the hoisted named predicates — the top gate
\(`org-air-classify--board-active-p'), Upcoming
\(`org-air-classify--due-within-p'), Overdue
\(`org-air-classify--overdue-p'), high priority
\(`org-air-classify--hipri-p'), Needs attention
\(`org-air-classify--attention-p'), Untracked
\(`org-air-classify--untracked-p') — so the buckets and the date/status
filter tokens share ONE definition of every date word (agreement by
construction).

Buckets are NON-EXCLUSIVE: an item may hold several at once (an overdue
`#A' that has been quiet a month is in Overdue, High priority AND Needs
attention), and each section shows it.

THE COVERAGE THEOREM:

  Every board-active, non-deferred task heading has a row somewhere,
  UNLESS its own facts defer it, which happens in exactly two ways:
    (a) its PLAN (SCHEDULED/DEADLINE) puts it beyond the Upcoming
        horizon; or
    (b) org-air MEASURED activity on it more recently than its
        priority's threshold.
  There is no third case.

Both exemptions are promises rather than hiding places: (a) resurfaces
on its own date, (b) resurfaces when the heading goes quiet.  The
theorem holds only because Untracked negates exactly what the date
sections read (`org-air-classify--planned-p'); widen that clause and a
plan written as a body `<timestamp>' reaches neither Untracked nor a
date section."
  (let* ((now (or now (current-time)))
         (buckets nil)
         (inbox-p (org-air-classify--inbox-dweller-p item)))
    (when (org-air-classify--board-active-p item)
      (if (org-air-classify--backlog-p item)
          ;; A board-active DEFERRED item routes OFF the four task
          ;; buckets and Inbox into the SINGLE `backlog' home: off the
          ;; attention surfaces, still trackable (a Backlog section and
          ;; a rail count).  The gate sits INSIDE the board-active
          ;; branch, so a DONE or archived backlog heading still
          ;; classifies into ZERO buckets — backlog never resurrects
          ;; history.  The item stays a TASK: this is a tag overlay, not
          ;; a re-typed note.
          (push 'backlog buckets)
        ;; Board order: Overdue, Upcoming, High priority, Needs
        ;; attention (Inbox is rendered first but classified here).
        (when (org-air-classify--overdue-p item now)
          (push 'overdue buckets))
        (when (org-air-classify--due-within-p item now org-air-upcoming-days)
          (push 'upcoming buckets))
        (when (org-air-classify--hipri-p item)
          (push 'high-priority buckets))
        ;; The AGING rule, total and date-free.  Deliberately NO inbox
        ;; carve-out: a capture that has sat untouched past its
        ;; threshold genuinely needs attention, and overlap is the
        ;; standing rule — it simply shows in Inbox AND here.
        (when (org-air-classify--attention-p item now)
          (push 'attention buckets))
        ;; The home of last resort: a heading with no plan AND no
        ;; recorded history has no clock to age off and no date to be
        ;; due on, so without this it has no row anywhere.  "Plan" is
        ;; the SCHEDULED/DEADLINE pair the date sections read
        ;; (`org-air-classify--planned-p'), so the catch-all is the
        ;; exact negation of the sections it backstops.  An inbox
        ;; dweller is excluded because the `inbox' push below is
        ;; UNCONDITIONAL for the same heading.
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
