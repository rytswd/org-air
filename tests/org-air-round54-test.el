;;; org-air-round54-test.el --- executing ERTs for v0.5 round-54 part 1 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.5 round-54 part 1 (air/v0.5/org-air-round54-
;; design.org): R54-1 staleness gated on actionable dates + R54-2 the
;; content-derived note-type model (the R54-3 Revisit view is HELD for a
;; follow-on and has no tests here).  All BATCH/headless; each test
;; drives the spec's semantics table / precedence chain through the real
;; scan (`org-air-query-items' over a temp corpus) so reverting the
;; matching impl seam fails it:
;;
;;   R54-1a  dateless prose heading => NEVER Stale (the flood fix).
;;           Scanned under the legacy `org-air-plain-heading-type' 'task
;;           so the item routes through the task buckets and ONLY the
;;           eligibility gate stands between it and 'stale — reverting
;;           the gate (pre-R54: mtime quiet => stale) fails.
;;   R54-1b  dateless TODO => Attention, never Stale (the intended
;;           reading: Attention is where "needs a decision/date" lives).
;;   R54-1c  dated-but-quiet IS Stale — SCHEDULED-quiet, DEADLINE-quiet
;;           and bare-active-<ts>-quiet each classify 'stale (the clock
;;           is unchanged for dated items; guards against over-gating),
;;           and SCHEDULED-yesterday stays un-stale.
;;   R54-1d  inactive-[ts]-only (a CREATED drawer) => never Stale; the
;;           `active-ts' slot stays nil while `subtree-ts' (regexp-both,
;;           the day view's key) still records the stamp — loosening the
;;           active probe to `org-ts-regexp-both' fails.
;;   R54-1e  `org-air-classify--stale-eligible-p' is the FIRST conjunct:
;;           an ineligible item never consults the stale clock
;;           (`--last-activity' call count 0), while the clock itself is
;;           UNCHANGED — it still answers mtime for the dateless item
;;           (the fix is the gate, not a clock that learned to say no).
;;   R54-1f  `--marker-active-ts' live-marker fallback: an active <ts>
;;           under a live marker answers; an inactive [ts] answers NIL
;;           (regexp-both revert fails); a (FILE . POS) cons answers NIL
;;           even when the file HAS an active stamp (data-pure law).
;;   R54-2g  type derivation: TODO (done or not) or scheduled/deadline
;;           => task; a bare active <ts> is a note fact, NOT a task;
;;           date-titled / journal-dir / date-shaped-#+title => journal;
;;           prose+tags => knowledge.
;;   R54-2h  the GTD board shows TASKS only: knowledge/journal buckets
;;           carry no board section (titles absent from the rendered
;;           board); the inbox BYPASS keeps a schedule-less capture in
;;           the `inbox' bucket exactly (xsqrnoyn semantics unchanged).
;;   R54-2i  overrides force the type both directions: `ORG_AIR_TYPE'
;;           property (inherited) beats the file keyword; the namespaced
;;           `#+org_air_type:' beats `#+type:'; override tags
;;           (`org-air-note-type-tag-alist') beat the task signal;
;;           invalid values fall through, never error.
;;   R54-2j  denote READ compat without denote: `#+title'/`#+filetags'
;;           win; a front-matter-less denote-style name falls back to
;;           the filename slug + `__tags'; the ID parses to `:created';
;;           an ID-only name types journal.  `(featurep 'denote)' is
;;           asserted nil around the whole scan — zero denote-* calls.

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
;;;; Corpus scaffolding
;;;; -------------------------------------------------------------------

(defvar org-air-r54--dir nil
  "The temp corpus directory of the current `org-air-r54--with-corpus'.")

(defmacro org-air-r54--with-corpus (specs &rest body)
  "Create a temp Org corpus from SPECS and run BODY against it.
SPECS is a list of (NAME . CONTENT) files written into a fresh temp
directory; NAME may carry subdirectories (created).  Binds
`org-air-files' to the directory, `org-air-inbox-file' to its inbox.org
and a temp `org-air-cache-file'.  Cleans up the scan work buffer, every
corpus-visiting buffer and the directory."
  (declare (indent 1) (debug t))
  `(let* ((org-air-r54--dir (make-temp-file "org-air-r54-" t)))
     (unwind-protect
         (progn
           (pcase-dolist (`(,name . ,content) ,specs)
             (let ((path (expand-file-name name org-air-r54--dir))
                   (coding-system-for-write 'utf-8-unix))
               (make-directory (file-name-directory path) t)
               (write-region (or content "") nil path nil 'silent)))
           (let ((org-air-files (list org-air-r54--dir))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-r54--dir))
                 (org-air-cache-file
                  (expand-file-name ".cache/board.eld" org-air-r54--dir)))
             ,@body))
       (org-air-query-teardown)
       (let ((kill-buffer-query-functions nil))
         (dolist (buf (buffer-list))
           (let ((fn (buffer-file-name buf)))
             (when (and fn (string-prefix-p org-air-r54--dir fn))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r54--dir t))))

(defun org-air-r54--age (name days)
  "Set corpus file NAME's mtime to DAYS days before `org-air-test-now'.
The R54-1 seams force the `activity' slot's mtime fallback old without
touching file CONTENT (the pre-R54 stale-flood path)."
  (set-file-times (expand-file-name name org-air-r54--dir)
                  (time-subtract org-air-test-now (days-to-time days))))

(defun org-air-r54--item (title items)
  "Return the item in ITEMS whose title contains TITLE; assert it exists."
  (let ((item (org-air-test-find-item title items)))
    (should item)
    item))

(defun org-air-r54--buckets (title items)
  "Classify the TITLE item from ITEMS at the frozen `org-air-test-now'."
  (org-air-classify-item (org-air-r54--item title items) org-air-test-now))

;;;; -------------------------------------------------------------------
;;;; R54-1 — Stale only for dated items
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r54-1a-dateless-prose-never-stale ()
  "A dateless prose heading carries no date and no Stale bucket (R54-1/R93).
Scanned with the LEGACY `org-air-plain-heading-type' 'task so the plain
heading still types task and routes through the task buckets.  R93
re-bless: the Stale bucket the seam was built against is RETIRED, so
what survives here is the surviving half -- the item is not DATED
\(`org-air-classify--dated-p', the R54-1 predicate under its honest
name, still the `is:nodate' axis) -- plus the R93 rule that replaced it:
a 60-day-old file with no per-heading history ages off the COARSE mtime
floor and surfaces in Needs attention on the clock alone, with no date
anywhere in the entry."
  (skip-unless (locate-library "org-air"))
  (org-air-r54--with-corpus
      '(("notes.org" . "* Evergreen prose note\nNo dates anywhere.\n")
        ("inbox.org" . "#+title: inbox\n"))
    (org-air-r54--age "notes.org" 60)
    (let* ((org-air-plain-heading-type 'task) ; legacy knob: board material
           (items (org-air-query-items))
           (item (org-air-r54--item "Evergreen prose note" items))
           (buckets (org-air-classify-item item org-air-test-now)))
      ;; The legacy knob typed it a task (so it DID reach the buckets)...
      (should (eq (org-air-item-ntype item) 'task))
      ;; ...it has NO per-heading history, so the R93 clock is the file...
      (should-not (org-air-item-updated item))
      (should (>= (org-air-classify-quiet-days item org-air-test-now)
                  (org-air-classify-attention-threshold item)))
      ;; ...it carries no date at all (the R54-1 predicate, renamed)...
      (should-not (org-air-classify--dated-p item))
      (should-not (org-air-classify--stale-eligible-p item)) ; kept alias
      ;; ...`stale' is retired, and the quiet clock alone surfaces it.
      (should-not (memq 'stale buckets))
      (should (memq 'attention buckets)))))

(ert-deftest org-air-r54-1b-dateless-todo-attention-never-stale ()
  "A dateless TODO surfaces via Attention only, never Stale (seam 1a/6).
Attention is where \"this needs a decision/date\" lives; reverting the
eligibility gate re-adds 'stale (the item's mtime activity is 60 days
old) and fails."
  (skip-unless (locate-library "org-air"))
  (org-air-r54--with-corpus
      '(("tasks.org" . "* TODO Decide on the venue\nNo dates yet.\n")
        ("inbox.org" . "#+title: inbox\n"))
    (org-air-r54--age "tasks.org" 60)
    (let* ((items (org-air-query-items))
           (item (org-air-r54--item "Decide on the venue" items))
           (buckets (org-air-classify-item item org-air-test-now)))
      (should (eq (org-air-item-ntype item) 'task)) ; TODO keyword => task
      (should (memq 'attention buckets))
      (should-not (org-air-classify--stale-eligible-p item))
      (should-not (memq 'stale buckets)))))

(ert-deftest org-air-r54-1c-dated-quiet-is-stale ()
  "SCHEDULED-quiet, DEADLINE-quiet and active-<ts>-quiet surface (1b/1c, R93).
R93 re-bless: the Stale bucket is retired and the aging Needs-attention
rule subsumes it, so the three quiet DATED rows still surface -- as
`attention' -- and the fresh dated companion still does not.  The corpus
moved with the rule: \"quiet\" is now the heading's RECENCY (an inactive
`[stamp]' in its own body), never its plan date, so each row states its
own last-touched date instead of leaning on the file's mtime.  That is
the R93 point restated from the other side: the SCHEDULED/DEADLINE dates
below neither start nor stop this clock."
  (skip-unless (locate-library "org-air"))
  (org-air-r54--with-corpus
      '(("tasks.org" .
         "* TODO Quiet scheduled chore\nSCHEDULED: <2026-04-16 Thu>\n\
Two months quiet.\n[2026-04-16 Thu 09:00]\n\
* TODO Quiet deadline chore\nDEADLINE: <2026-04-16 Thu>\n\
Two months quiet.\n[2026-04-16 Thu 09:00]\n\
* TODO Quiet stamped chore\nLast touched <2026-03-15 Sun>, active stamp.\n\
[2026-03-15 Sun 20:00]\n\
* TODO Fresh scheduled chore\nSCHEDULED: <2026-06-14 Sun>\nYesterday.\n\
[2026-06-14 Sun 18:00]\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let ((items (org-air-query-items)))
      (dolist (title '("Quiet scheduled chore" "Quiet deadline chore"))
        (let ((buckets (org-air-r54--buckets title items)))
          (ert-info ((format "%s => %S" title buckets))
            (should (memq 'attention buckets))
            (should-not (memq 'stale buckets)))))
      ;; The bare active <ts> still grants DATE-eligibility (the R54-1
      ;; predicate under its R93 name); the quiet clock comes from the
      ;; inactive stamp beside it, never from the active one.
      (let ((item (org-air-r54--item "Quiet stamped chore" items)))
        (should (org-air-item-active-ts item))
        (should (org-air-classify--dated-p item))
        (should (memq 'attention (org-air-classify-item item org-air-test-now))))
      ;; Fresh dated: dated, and touched yesterday — no nag.
      (let ((fresh (org-air-r54--item "Fresh scheduled chore" items)))
        (should (org-air-classify--stale-eligible-p fresh))
        (should (= 1 (org-air-classify-quiet-days fresh org-air-test-now)))
        (should-not (memq 'attention
                          (org-air-classify-item fresh org-air-test-now)))))))

(ert-deftest org-air-r54-1d-inactive-created-drawer-never-stale ()
  "An inactive-[ts]-only item (CREATED drawer) is never Stale (seam 1c).
The archival stamp still feeds `subtree-ts' (the day view's regexp-both
key) but NOT `active-ts' — reverting the active probe to
`org-ts-regexp-both' (or to `subtree-ts') repopulates the slot, makes
the item eligible with a 161-day clock, and fails."
  (skip-unless (locate-library "org-air"))
  (org-air-r54--with-corpus
      '(("archive.org" .
         "* TODO Archived reference sweep\n:PROPERTIES:\n\
:CREATED: [2026-01-05 Mon]\n:END:\nOnly archival metadata here.\n")
        ("inbox.org" . "#+title: inbox\n"))
    (org-air-r54--age "archive.org" 60)
    (let* ((items (org-air-query-items))
           (item (org-air-r54--item "Archived reference sweep" items))
           (buckets (org-air-classify-item item org-air-test-now)))
      ;; The probes stay DISTINCT: both-regexp key filled, active key nil.
      (should (org-air-item-subtree-ts item))
      (should-not (org-air-item-active-ts item))
      (should-not (org-air-classify--stale-eligible-p item))
      (should-not (memq 'stale buckets))
      (should (memq 'attention buckets)))))

(ert-deftest org-air-r54-1e-eligibility-first-conjunct-clock-unchanged ()
  "The date predicate survives; the R22 activity chain is NOT the clock (1e/R93).
R93 re-bless.  The seam this test was built for -- \"eligibility is the
first conjunct of the Stale rule\" -- went with the Stale rule, but its
two halves both have R93 successors that are worth more:

  1. `org-air-classify--dated-p' (the R54-1 predicate, renamed, with the
     old spelling kept as a working alias) still answers exactly as it
     did: it is now the `is:nodate' axis rather than a bucket gate.
  2. The aging rule reads `org-air-classify-updated' and NOTHING else.
     `org-air-classify--last-activity' -- the broad \"what has this item
     got going on\" chain that also counts SCHEDULED and DEADLINE -- is
     never consulted while classifying, for either item.  Wiring the
     attention clock to that chain (the obvious cheap implementation)
     would make a PLAN silence the nag, which is the exact inversion R93
     exists to remove; the call count pins it at zero."
  (skip-unless (locate-library "org-air"))
  (should (fboundp 'org-air-classify--stale-eligible-p))
  (org-air-r54--with-corpus
      '(("tasks.org" .
         "* TODO Dateless dawdler\nNo dates.\n\
* TODO Quiet scheduled chore\nSCHEDULED: <2026-04-16 Thu>\n")
        ("inbox.org" . "#+title: inbox\n"))
    (org-air-r54--age "tasks.org" 60)
    (let* ((items (org-air-query-items))
           (dateless (org-air-r54--item "Dateless dawdler" items))
           (dated (org-air-r54--item "Quiet scheduled chore" items))
           (orig (symbol-function 'org-air-classify--last-activity))
           (calls 0))
      (should-not (org-air-classify--stale-eligible-p dateless))
      (should-not (org-air-classify--dated-p dateless))
      (should (org-air-classify--stale-eligible-p dated))
      (should (org-air-classify--dated-p dated))
      (cl-letf (((symbol-function 'org-air-classify--last-activity)
                 (lambda (item) (cl-incf calls) (funcall orig item))))
        ;; Neither item's classification consults the R22 activity chain.
        (org-air-classify-item dateless org-air-test-now)
        (should (= calls 0))
        (should (memq 'attention (org-air-classify-item dated org-air-test-now)))
        (should (= calls 0)))
      ;; And the chain itself is unchanged: it still answers the 60-day
      ;; mtime for the dateless item (it never learned to say no; it is
      ;; simply not what Needs attention asks any more).
      (let ((activity (org-air-classify--last-activity dateless)))
        (should activity)
        (should (>= (- (time-to-days org-air-test-now)
                       (time-to-days activity))
                    (org-air-classify-attention-threshold dateless)))))))

(ert-deftest org-air-r54-1f-marker-active-ts-live-fallback-cons-nil ()
  "`--marker-active-ts': live active <ts> answers; inactive and cons nil (1f).
The live-marker fallback for items built OUTSIDE the scan mirrors
`--marker-timestamp-time' but with `org-ts-regexp' — an inactive-only
subtree answers NIL (a regexp-both revert fails) while the both-regexp
walk still sees the stamp (the plumbing is fine; only the active filter
differs).  A (FILE . POS) cons answers NIL even though the FILE carries
an active stamp — the data-pure render law: scanned items answer from
the `active-ts' slot, never a file open."
  (skip-unless (locate-library "org-air"))
  (org-air-r54--with-corpus
      '(("stamped.org" . "* Cons marker probe\nBody <2026-03-15 Sun>.\n")
        ("inbox.org" . "#+title: inbox\n"))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "* Live active note\nBody <2026-03-15 Sun>.\n"
              "* Live inactive note\nBody [2026-03-15 Sun].\n")
      (goto-char (point-min))
      (let* ((active-pos (point-min))
             (inactive-pos (progn (search-forward "* Live inactive note")
                                  (match-beginning 0)))
             (live-active (org-air-item-create
                           :title "Live active note" :file ""
                           :marker (copy-marker active-pos)))
             (live-inactive (org-air-item-create
                             :title "Live inactive note" :file ""
                             :marker (copy-marker inactive-pos)))
             (cons-item (org-air-item-create
                         :title "Cons marker probe"
                         :file (expand-file-name "stamped.org" org-air-r54--dir)
                         :marker (cons (expand-file-name "stamped.org"
                                                         org-air-r54--dir)
                                       1))))
        ;; Live marker + active <ts>: the fallback answers the stamp, so
        ;; the item is stale-ELIGIBLE without any slot.
        (let ((ts (org-air-classify--marker-active-ts live-active)))
          (should ts)
          (should (= (time-to-days ts)
                     (time-to-days (org-time-string-to-time "2026-03-15")))))
        (should (org-air-classify--stale-eligible-p live-active))
        ;; Live marker + inactive [ts]: the both-regexp walk still sees
        ;; the stamp (plumbing OK), the ACTIVE walk answers nil.
        (should (org-air-classify--marker-timestamp-time live-inactive))
        (should-not (org-air-classify--marker-active-ts live-inactive))
        (should-not (org-air-classify--stale-eligible-p live-inactive))
        ;; Cons marker: nil, even though the file HAS an active stamp —
        ;; a file-opening revert (pre-R53 resolve) fails here.
        (should-not (org-air-classify--marker-active-ts cons-item))
        (should-not (org-air-classify--stale-eligible-p cons-item))))))

;;;; -------------------------------------------------------------------
;;;; R54-2 — the content-derived note-type model
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r54-2g-note-type-derivation ()
  "Type derivation (USER-RULED): task / journal / knowledge (seam 2a).
TODO (done or not) or scheduled/deadline => task; a bare active <ts> is
a note fact, NOT a task signal; date-shaped file name, journal/ path
component or date-shaped `#+title' => journal; everything else —
prose+links+tags — => knowledge, with zero manual tagging."
  (skip-unless (locate-library "org-air"))
  (org-air-r54--with-corpus
      '(("tasks.org" .
         "* TODO Write the report\n* DONE Shipped last week\n\
* Sched-only errand\nSCHEDULED: <2026-06-20 Sat>\n\
* Deadline-only errand\nDEADLINE: <2026-06-22 Mon>\n")
        ("evergreen.org" .
         "#+title: Evergreen ideas\n#+filetags: :kb:\n\n\
* Emacs configuration ideas :emacs:\nProse and [[https://example.com][links]].\n\
* Seminar date fact\nHappens on <2026-06-20 Sat> — a fact, not a task.\n")
        ("2026-07-14.org" . "* Monday reflections\nDear diary.\n")
        ("journal/morning-pages.org" . "* Three pages\nWritten longhand.\n")
        ("titled.org" . "#+title: 2026-07-13\n\n* Entry heading\nProse.\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let ((items (org-air-query-items)))
      ;; TASK: TODO keyword (done or not) OR scheduled/deadline.
      (dolist (title '("Write the report" "Shipped last week"
                       "Sched-only errand" "Deadline-only errand"))
        (should (eq (org-air-item-ntype (org-air-r54--item title items))
                    'task)))
      ;; KNOWLEDGE: prose+tags, and a bare active <ts> is NOT a task.
      (should (eq (org-air-item-ntype
                   (org-air-r54--item "Emacs configuration ideas" items))
                  'knowledge))
      (should (eq (org-air-item-ntype
                   (org-air-r54--item "Seminar date fact" items))
                  'knowledge))
      ;; JOURNAL: date-shaped name / journal dir / date-shaped #+title.
      (should (eq (org-air-item-ntype
                   (org-air-r54--item "Monday reflections" items))
                  'journal))
      (should (eq (org-air-item-ntype
                   (org-air-r54--item "Three pages" items))
                  'journal))
      (should (eq (org-air-item-ntype
                   (org-air-r54--item "Entry heading" items))
                  'journal)))))

(ert-deftest org-air-r54-2h-board-shows-tasks-only ()
  "Knowledge/journal never enter the task buckets or the board (2b/2c).
A dateless prose heading classifies to exactly (knowledge) — no
attention, no stale (reverting the routing layer fails: pre-R54 it was
attention); a journal entry to exactly (journal); the rendered GTD
board carries NEITHER title while the task and the inbox capture render.
The inbox BYPASS keeps a schedule-less prose capture in the `inbox'
bucket exactly (not knowledge) — capture flows unchanged."
  (skip-unless (locate-library "org-air"))
  (org-air-r54--with-corpus
      '(("board.org" .
         "* TODO Real board task\nSCHEDULED: <2026-06-16 Tue>\n\
* Evergreen pruning wisdom :garden:\nProse notes on pruning.\n")
        ("2026-06-14.org" . "* Yesterday journal entry\nDear diary.\n")
        ("inbox.org" . "#+title: inbox\n\n* Half-formed capture\nNo dates.\n"))
    (let ((items (org-air-query-items)))
      ;; Classify layer: own buckets, no board section, no task buckets.
      (should (equal (org-air-r54--buckets "Evergreen pruning wisdom" items)
                     '(knowledge)))
      (should (equal (org-air-r54--buckets "Yesterday journal entry" items)
                     '(journal)))
      ;; Inbox bypass: the schedule-less capture is an unfiled task-to-be.
      (should (equal (org-air-r54--buckets "Half-formed capture" items)
                     '(inbox)))
      ;; The task keeps the full treatment.
      (should (memq 'upcoming (org-air-r54--buckets "Real board task" items))))
    ;; Board layer: the rendered GTD board shows TASKS (+ inbox) only.
    (org-air-viewport-test--with-frozen-now
      (unwind-protect
          (org-air-viewport-test--with-render-guards
            (let ((org-air-view-width 120))
              (org-air)
              (with-current-buffer "*org-air*"
                (let ((text (buffer-string)))
                  (should (string-match-p "Real board task" text))
                  (should (string-match-p "Half-formed capture" text))
                  (should-not (string-match-p "Evergreen pruning wisdom" text))
                  (should-not (string-match-p "Yesterday journal entry" text))))))
        (when (get-buffer "*org-air*")
          (kill-buffer "*org-air*"))))))

(ert-deftest org-air-r54-2i-overrides-force-type ()
  "Optional overrides force the type BOTH directions (seam 2a).
`ORG_AIR_TYPE' property (inherited) beats the file keyword; the
namespaced `#+org_air_type:' is authoritative over `#+type:'; override
tags beat the task signal (a `:note:' TODO types knowledge, a `:task:'
prose heading types task, `:journal:' types journal); invalid values
fall through the chain, never an error."
  (skip-unless (locate-library "org-air"))
  (org-air-r54--with-corpus
      '(("keyword-note.org" .
         "#+type: note\n\n* TODO Looks like a task\nKeyword-forced note.\n")
        ("keyword-both.org" .
         "#+type: note\n#+org_air_type: task\n\n\
* Plain prose heading\nNamespaced wins.\n")
        ("tag-overrides.org" .
         "* Prose forced task :task:\n* TODO Forced note :note:\n\
* Prose journal flavour :journal:\n")
        ("prop.org" .
         "#+type: note\n\n* Prose with property\n:PROPERTIES:\n\
:ORG_AIR_TYPE: task\n:END:\nProperty beats keyword.\n\
* Parent typed journal\n:PROPERTIES:\n:ORG_AIR_TYPE: journal\n:END:\n\
** TODO Child inherits journal\n")
        ("invalid.org" .
         "#+type: someday\n\n* TODO Falls through to task\n\
* Prose falls through\n:PROPERTIES:\n:ORG_AIR_TYPE: banana\n:END:\n"))
    (let ((items (org-air-query-items)))
      ;; File keyword override: note-forced TODO.
      (should (eq (org-air-item-ntype
                   (org-air-r54--item "Looks like a task" items))
                  'knowledge))
      ;; Namespaced keyword authoritative over #+type:.
      (should (eq (org-air-item-ntype
                   (org-air-r54--item "Plain prose heading" items))
                  'task))
      ;; Tag overrides both directions + journal.
      (should (eq (org-air-item-ntype
                   (org-air-r54--item "Prose forced task" items))
                  'task))
      (should (eq (org-air-item-ntype
                   (org-air-r54--item "Forced note" items))
                  'knowledge))
      (should (eq (org-air-item-ntype
                   (org-air-r54--item "Prose journal flavour" items))
                  'journal))
      ;; Property beats the file keyword; a parent property types its
      ;; subtree (inheritance) even against the child's TODO signal.
      (should (eq (org-air-item-ntype
                   (org-air-r54--item "Prose with property" items))
                  'task))
      (should (eq (org-air-item-ntype
                   (org-air-r54--item "Child inherits journal" items))
                  'journal))
      ;; Invalid values are ignored — the chain falls through.
      (should (eq (org-air-item-ntype
                   (org-air-r54--item "Falls through to task" items))
                  'task))
      (should (eq (org-air-item-ntype
                   (org-air-r54--item "Prose falls through" items))
                  'knowledge)))))

(ert-deftest org-air-r54-2j-denote-read-fallbacks-without-denote ()
  "Denote-style titles/tags/IDs read from pure file conventions (2a/2d).
`#+title'/`#+filetags' front matter wins; a front-matter-less
denote-style file falls back to the filename SLUG and `__tags'; the
leading ID parses to file-meta `:created'; an ID-only name is
date-shaped and types journal.  All of it with `(featurep 'denote)'
NIL before and after — org-air never calls a denote-* function."
  (skip-unless (locate-library "org-air"))
  (should-not (featurep 'denote))
  (org-air-r54--with-corpus
      '(("20260101T120000--evergreen-idea__emacs_zettel.org" .
         "Prose only: no front matter, no headings.\n")
        ("20260103T080000--titled__ignored.org" .
         "#+title: The Real Title\n#+filetags: :front:matter:\n\nProse.\n")
        ("20260715T000000.org" . "ID-only journal prose.\n")
        ("inbox.org" . "#+title: inbox\n"))
    (org-air-query-items)
    (let ((slugged (org-air-query-file-meta
                    (expand-file-name
                     "20260101T120000--evergreen-idea__emacs_zettel.org"
                     org-air-r54--dir)))
          (titled (org-air-query-file-meta
                   (expand-file-name "20260103T080000--titled__ignored.org"
                                     org-air-r54--dir)))
          (id-only (org-air-query-file-meta
                    (expand-file-name "20260715T000000.org"
                                      org-air-r54--dir))))
      ;; Filename fallbacks: slug title, __tag_tag tags, ID => :created.
      (should slugged)
      (should (equal (plist-get slugged :title) "evergreen-idea"))
      (should (equal (plist-get slugged :tags) '("emacs" "zettel")))
      (should (equal (plist-get slugged :created)
                     (org-air-query--denote-id-time "20260101T120000")))
      (should (eq (plist-get slugged :ntype) 'knowledge))
      ;; Front matter WINS over the filename conventions.
      (should (equal (plist-get titled :title) "The Real Title"))
      (should (equal (plist-get titled :tags) '("front" "matter")))
      (should (equal (plist-get titled :created)
                     (org-air-query--denote-id-time "20260103T080000")))
      ;; ID-only name: created parses; date-shaped => journal.
      (should (equal (plist-get id-only :created)
                     (org-air-query--denote-id-time "20260715T000000")))
      (should (eq (plist-get id-only :ntype) 'journal))))
  (should-not (featurep 'denote)))

(provide 'org-air-round54-test)
;;; org-air-round54-test.el ends here
