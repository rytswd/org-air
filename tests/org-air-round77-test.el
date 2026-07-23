;;; org-air-round77-test.el --- executing ERTs for round-77 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for round-77 (air/v0.1/org-air-round77-design.org):
;; `org-air-task-requires-todo' — optionally require a TODO keyword for
;; board tasks.  When non-nil the R54-2 task signal narrows to the
;; keyword alone: a keyword-less scheduled/deadline routine demotes to
;; knowledge (off the Upcoming/Needs-attention/High-priority/Stale task
;; buckets) while keeping its day-view row and calendar mark and staying
;; reachable via the note surfaces.  Default nil is byte-identical R54.
;; The spec's twelve seams r77-1..r77-12 map onto the ERTs below:
;;
;;   r77-1   knob ON demotes the routine: ntype knowledge, exactly the
;;           (knowledge) bucket, the rendered board lacks the row.
;;   r77-2   the keyworded twin stays: ntype task, 'upcoming, renders.
;;   r77-3   knob OFF (the DEFAULT) is R54 verbatim: the routine types
;;           task and is 'upcoming; the standard value is nil.
;;   r77-4   filter agrees with the gated bucket: the demoted routine
;;           fails is:upcoming AND due:7d through the real
;;           `org-air-view--passes-filter-p' fold; the twin passes;
;;           the r72-3 agreement theorem re-asserted over the knob-on
;;           corpus (token <=> bucket for every scanned item).
;;   r77-5   the pre-existing override hole closes at knob NIL: a
;;           `#+type: note' scheduled heading classifies knowledge and
;;           now ALSO fails is:upcoming (the hoisted routing gate).
;;   r77-6   not-done by COMPOSITION: a DONE heading still types task
;;           (never knowledge), classifies into ZERO buckets, and a
;;           pure done-archive file votes :ntype task — OUT of Revisit.
;;   r77-7   date surfaces keep the routine: the day view's "Scheduled"
;;           group on its day + the calendar day mark.
;;   r77-8   Notes/Revisit reachability: a pure-routines file votes
;;           :ntype knowledge and enters the Revisit scope — not lost.
;;   r77-9   the inbox bypass survives: a keyword-less SCHEDULED capture
;;           stays an 'inbox (+ 'upcoming) triage unit, never knowledge.
;;   r77-10  overrides outrank the knob: ORG_AIR_TYPE / a :task: tag
;;           force a routine back to ntype task + 'upcoming.
;;   r77-11  step-6 subsumption: knob t + the legacy
;;           `org-air-plain-heading-type' 'task still demotes the
;;           keyword-less routine to knowledge (the knob wins).
;;   r77-12  the knob is the SEVENTH `org-air-view--cache-key' element;
;;           a cache written under nil is a clean cold miss under t,
;;           and a pre-R77 6-element key misses on length.
;;
;; Test-seat AUDIT GAPS (round-77 closeout — seams the twelve above
;; left undriven):
;;
;;   r77-13  the step-4 gate's DEADLINE disjunct (r77-1 drove only
;;           SCHEDULED) + the D2 step-5 journal flavour (a routine in
;;           a journal-typed file demotes to `journal', off Revisit)
;;           + the routed filter gate's JOURNAL leg.
;;   r77-14  "donep/archived unaffected" — the ARCHIVED half (r77-6
;;           drove only donep): an archived keyworded heading is
;;           buried identically under the knob ON and OFF.
;;   r77-15  the r77-8 reachability at the actual SURFACE: the real
;;           `org-air-revisit' view renders the demoted routines file
;;           as a row; the D7 mixed-file wrinkle pinned as specced.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)

(when (locate-library "org-air")
  (require 'org-air)
  (require 'org-air-revisit))

;;;; -------------------------------------------------------------------
;;;; Corpus scaffolding
;;;; -------------------------------------------------------------------

(defvar org-air-r77--dir nil
  "The temp corpus directory of the current `org-air-r77--with-corpus'.")

(defun org-air-r77--reset-tables ()
  "Clear the GLOBAL query-layer tables the note surfaces read.
File-meta, the visit ledger and the denote-ID index are session globals
\(never cleared by the scan), so every test starts and ends empty —
entries from another test's absolute temp paths must never leak into
`org-air-revisit--scope-entries'."
  (clrhash org-air-query--file-meta)
  (clrhash org-air-query--visits)
  (clrhash org-air-query--denote-id-index)
  (setq org-air-query--link-graph-dirty nil))

(defmacro org-air-r77--with-corpus (specs &rest body)
  "Create a temp Org corpus from SPECS and run BODY against it.
SPECS is a list of (NAME . CONTENT) files written into a fresh temp
directory; NAME may carry subdirectories (created).  Binds
`org-air-files' to the directory, `org-air-inbox-file' to its inbox.org
and a temp `org-air-cache-file'.  Starts from EMPTY query tables and
cleans up the tables, the scan work buffer, every corpus-visiting
buffer and the directory."
  (declare (indent 1) (debug t))
  `(let* ((org-air-r77--dir (make-temp-file "org-air-r77-" t)))
     (unwind-protect
         (progn
           (org-air-r77--reset-tables)
           (pcase-dolist (`(,name . ,content) ,specs)
             (let ((path (expand-file-name name org-air-r77--dir))
                   (coding-system-for-write 'utf-8-unix))
               (make-directory (file-name-directory path) t)
               (write-region (or content "") nil path nil 'silent)))
           (let ((org-air-files (list org-air-r77--dir))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-r77--dir))
                 (org-air-cache-file
                  (expand-file-name ".cache/board.eld" org-air-r77--dir)))
             ,@body))
       (org-air-query-teardown)
       (org-air-r77--reset-tables)
       (let ((kill-buffer-query-functions nil))
         (dolist (buf (buffer-list))
           (let ((fn (buffer-file-name buf)))
             (when (and fn (string-prefix-p org-air-r77--dir fn))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r77--dir t))))

(defun org-air-r77--item (title items)
  "Return the item in ITEMS whose title contains TITLE; assert it exists."
  (let ((item (org-air-test-find-item title items)))
    (should item)
    item))

(defun org-air-r77--buckets (title items)
  "Classify the TITLE item from ITEMS at the frozen `org-air-test-now'."
  (org-air-classify-item (org-air-r77--item title items) org-air-test-now))

(defun org-air-r77--passes-p (item tokens)
  "Non-nil when ITEM passes filter TOKENS under `all' at the frozen now.
Drives the REAL fold — `org-air-view--passes-filter-p' with
`org-air-view--tag-filter' bound to TOKENS — the exact board path."
  (let ((org-air-view--tag-filter tokens)
        (org-air-filter-match 'all)
        (org-air-view--filter-now org-air-test-now)
        (org-air-view--scope nil)
        (org-air-view--render-partition nil)
        (org-air-upcoming-days 7)
        (org-air-stale-days 21))
    (and (org-air-view--passes-filter-p item) t)))

(defconst org-air-r77--routine
  "* Bi-weekly: Water plants\nSCHEDULED: <2026-06-16 Tue ++2w>\nRoutine.\n"
  "The round's verbatim routine: SCHEDULED tomorrow, repeater, NO keyword.")

;;;; -------------------------------------------------------------------
;;;; r77-1 / r77-2 — the knob demotes the routine; the twin stays.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r77-1-knob-on-demotes-routine ()
  "Knob ON: the keyword-less scheduled routine demotes to knowledge (r77-1).
ntype `knowledge', exactly the (knowledge) bucket — no upcoming, no
attention, no stale, no high-priority — and the rendered board lacks
the row while the keyworded twin renders.  Reverting the step-4 gate in
`org-air-query--note-type' fails."
  (skip-unless (locate-library "org-air"))
  (org-air-r77--with-corpus
      `(("routines.org" . ,org-air-r77--routine)
        ("tasks.org" . "* TODO Weekly review\nSCHEDULED: <2026-06-16 Tue>\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((org-air-task-requires-todo t)
           (items (org-air-query-items))
           (routine (org-air-r77--item "Water plants" items))
           (buckets (org-air-classify-item routine org-air-test-now)))
      (should (eq (org-air-item-ntype routine) 'knowledge))
      (should (equal buckets '(knowledge)))
      (dolist (bucket '(upcoming attention stale high-priority inbox))
        (should-not (memq bucket buckets)))
      ;; Board layer: the routine's row is GONE, the twin's renders.
      (org-air-viewport-test--with-frozen-now
        (unwind-protect
            (org-air-viewport-test--with-render-guards
              (let ((org-air-view-width 120))
                (org-air)
                (with-current-buffer "*org-air*"
                  (let ((text (buffer-string)))
                    (should (string-match-p "Weekly review" text))
                    (should-not (string-match-p "Water plants" text))))))
          (when (get-buffer "*org-air*")
            (kill-buffer "*org-air*")))))))

(ert-deftest org-air-r77-2-keyworded-twin-stays ()
  "Knob ON: the TODO-keyworded scheduled twin keeps full task treatment
\(r77-2).  ntype `task', 'upcoming present — guards against over-gating."
  (skip-unless (locate-library "org-air"))
  (org-air-r77--with-corpus
      `(("routines.org" . ,org-air-r77--routine)
        ("tasks.org" . "* TODO Weekly review\nSCHEDULED: <2026-06-16 Tue>\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((org-air-task-requires-todo t)
           (items (org-air-query-items))
           (twin (org-air-r77--item "Weekly review" items)))
      (should (eq (org-air-item-ntype twin) 'task))
      (should (memq 'upcoming
                    (org-air-classify-item twin org-air-test-now))))))

;;;; -------------------------------------------------------------------
;;;; r77-3 — the DEFAULT (nil) is R54 verbatim.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r77-3-knob-off-is-r54-verbatim ()
  "Knob OFF (the DEFAULT): the routine types task and is 'upcoming (r77-3).
The standard value IS nil (the R54 D1 USER-RULED signal preserved); the
same corpus scanned WITHOUT any binding gives the routine the full R54
task treatment — the default's byte-identity, in miniature (the whole
untouched r54/r59/r72 suite is the full assertion)."
  (skip-unless (locate-library "org-air"))
  ;; The defcustom's standard value is nil — a `t' default flip fails.
  (should (null (eval (car (get 'org-air-task-requires-todo
                                'standard-value)))))
  (should-not org-air-task-requires-todo)
  (org-air-r77--with-corpus
      `(("routines.org" . ,org-air-r77--routine)
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((items (org-air-query-items))
           (routine (org-air-r77--item "Water plants" items))
           (buckets (org-air-classify-item routine org-air-test-now)))
      (should (eq (org-air-item-ntype routine) 'task))
      (should (memq 'upcoming buckets))
      (should-not (memq 'knowledge buckets)))))

;;;; -------------------------------------------------------------------
;;;; r77-4 — the filter agrees with the gated buckets (R72 extended).
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r77-4-filter-agrees-with-gated-buckets ()
  "Knob ON: a demoted routine stops matching the date tokens too (r77-4).
The scanned routine fails `is:upcoming' AND `due:7d' through the real
`org-air-view--passes-filter-p' fold (a keyword-less OVERDUE routine
fails `is:overdue' likewise); the keyworded twin passes both; and the
r72-3 agreement theorem holds over the whole knob-on corpus — for EVERY
scanned item, token <=> bucket.  Reverting the
`org-air-classify--task-routed-p' conjunct in
`org-air-view--filter-date-token-match-p' fails."
  (skip-unless (locate-library "org-air"))
  (org-air-r77--with-corpus
      `(("routines.org" .
         ,(concat org-air-r77--routine
                  "* Fortnightly: backups\nSCHEDULED: <2026-06-01 Mon ++2w>\n"))
        ("tasks.org" . "* TODO Weekly review\nSCHEDULED: <2026-06-16 Tue>\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((org-air-task-requires-todo t)
           (items (org-air-query-items))
           (routine (org-air-r77--item "Water plants" items))
           (overdue-routine (org-air-r77--item "backups" items))
           (twin (org-air-r77--item "Weekly review" items)))
      ;; The demoted routine fails every task-vocabulary token.
      (should-not (org-air-r77--passes-p routine '("is:upcoming")))
      (should-not (org-air-r77--passes-p routine '("due:7d")))
      (should-not (org-air-r77--passes-p overdue-routine '("is:overdue")))
      (should-not (org-air-r77--passes-p overdue-routine '("is:stale")))
      ;; The twin passes both — no over-gating.
      (should (org-air-r77--passes-p twin '("is:upcoming")))
      (should (org-air-r77--passes-p twin '("due:7d")))
      ;; The r72-3 agreement theorem over the knob-on corpus: for EVERY
      ;; scanned item, filter token <=> gated bucket, exactly.
      (dolist (item items)
        (let ((buckets (org-air-classify-item item org-air-test-now)))
          (should (eq (org-air-r77--passes-p item '("is:upcoming"))
                      (not (null (memq 'upcoming buckets)))))
          (should (eq (org-air-r77--passes-p item '("due:7d"))
                      (not (null (memq 'upcoming buckets)))))
          (should (eq (org-air-r77--passes-p item '("is:stale"))
                      (not (null (memq 'stale buckets)))))
          (should (eq (org-air-r77--passes-p item '("is:hipri"))
                      (not (null (memq 'high-priority buckets)))))
          ;; is:overdue isolates the attention bucket's overdue disjunct.
          (should (eq (org-air-r77--passes-p item '("is:overdue"))
                      (not (null (and (memq 'attention buckets)
                                      (org-air-classify--overdue-p
                                       item org-air-test-now)))))))))))

;;;; -------------------------------------------------------------------
;;;; r77-5 — the override hole closes at knob NIL.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r77-5-override-hole-closed-at-knob-nil ()
  "Knob NIL: a `#+type: note' scheduled heading fails is:upcoming (r77-5).
It classifies `knowledge' (the R54-2 override, unchanged) and — new
under the hoisted routing gate — no longer matches `is:upcoming' /
`due:7d' / `is:nodate' either: the pre-existing filter⇔bucket
disagreement the knob would have widened, closed at the default.
Reverting D5 fails (pre-R77 it matched)."
  (skip-unless (locate-library "org-air"))
  (org-air-r77--with-corpus
      '(("noted.org" .
         "#+type: note\n\n* Facilitation workshop\nSCHEDULED: <2026-06-16 Tue>\n")
        ("inbox.org" . "#+title: inbox\n"))
    (should-not org-air-task-requires-todo) ; the DEFAULT path
    (let* ((items (org-air-query-items))
           (item (org-air-r77--item "Facilitation workshop" items)))
      (should (eq (org-air-item-ntype item) 'knowledge))
      (should (equal (org-air-classify-item item org-air-test-now)
                     '(knowledge)))
      (should-not (org-air-r77--passes-p item '("is:upcoming")))
      (should-not (org-air-r77--passes-p item '("due:7d")))
      (should-not (org-air-r77--passes-p item '("is:nodate"))))))

;;;; -------------------------------------------------------------------
;;;; r77-6 — not-done by COMPOSITION, never by re-typing DONE.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r77-6-not-done-by-composition ()
  "Knob ON: a DONE heading stays ntype task with ZERO buckets (r77-6).
The knob's ntype rule is \"carries a keyword\" (done or not) — done-ness
is enforced by the existing `org-air-classify--board-active-p' gate, so
the user-visible contract (a task section row implies a NOT-DONE
keyword) holds by composition while a pure done-archive file still
votes :ntype task and stays OUT of the Revisit scope.  Re-typing DONE
to knowledge fails the Revisit half — the D3 trap, locked."
  (skip-unless (locate-library "org-air"))
  (org-air-r77--with-corpus
      '(("archive.org" .
         "* DONE Old chore\nSCHEDULED: <2026-06-01 Mon>\n\
* DONE Shipped feature\nDEADLINE: <2026-05-20 Wed>\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((org-air-task-requires-todo t)
           (items (org-air-query-items))
           (chore (org-air-r77--item "Old chore" items)))
      ;; A done task is a TASK (never Revisit knowledge)...
      (should (eq (org-air-item-ntype chore) 'task))
      (should (org-air-item-donep chore))
      ;; ...classifying into ZERO buckets (the board-active gate).
      (should (equal (org-air-classify-item chore org-air-test-now) '()))
      ;; ...and the token side agrees: buried, not resurrected.
      (should-not (org-air-r77--passes-p chore '("is:overdue")))
      ;; The pure done-archive FILE votes task — OUT of Revisit scope.
      (let ((archive (expand-file-name "archive.org" org-air-r77--dir)))
        (should (eq (plist-get (org-air-query-file-meta archive) :ntype)
                    'task))
        (should-not (assoc archive (org-air-revisit--scope-entries)))))))

;;;; -------------------------------------------------------------------
;;;; r77-7 — the day view and the calendar keep the routine.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r77-7-date-surfaces-keep-routine ()
  "Knob ON: the demoted routine keeps its day-view row + calendar mark
\(r77-7).  `org-air-view--day-groups' files it under \"Scheduled\" on its
day and `org-air-calendar--marked-days' marks the day — those surfaces
read planning SLOTS, not types.  Gating them on the routing layer fails."
  (skip-unless (locate-library "org-air"))
  (org-air-r77--with-corpus
      `(("routines.org" . ,org-air-r77--routine)
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((org-air-task-requires-todo t)
           (items (org-air-query-items))
           (routine (org-air-r77--item "Water plants" items))
           (day (encode-time '(0 0 12 16 6 2026 nil -1 nil))))
      (should (eq (org-air-item-ntype routine) 'knowledge)) ; demoted...
      ;; ...but its day still lists it under the Scheduled group.
      (with-temp-buffer
        (let* ((org-air-view--scope nil)
               (org-air-view--render-partition nil)
               (groups (org-air-view--day-groups items day)))
          (should (memq routine (cdr (assoc "Scheduled" groups))))))
      ;; ...and its calendar day still carries the scheduled mark.
      (should (eq (gethash "2026-06-16"
                           (org-air-calendar--marked-days items))
                  'scheduled)))))

;;;; -------------------------------------------------------------------
;;;; r77-8 — Notes/Revisit reachability: demoted, never LOST.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r77-8-notes-revisit-reachability ()
  "Knob ON: a pure-routines file votes knowledge and enters Revisit (r77-8).
Every non-container heading demotes, so the F7 file vote follows at the
SAME scan: file-meta :ntype `knowledge', ONE Revisit-scope entry (the
per-FILE denote unit — the note surfaces' doorway, so the routine is
demoted but NOT lost).  Reverting the F7 follow-through (freezing the
vote to the un-gated signal) fails."
  (skip-unless (locate-library "org-air"))
  (org-air-r77--with-corpus
      `(("routines.org" .
         ,(concat org-air-r77--routine
                  "* Weekly: Review Goals\nSCHEDULED: <2026-06-21 Sun ++1w>\n"))
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((org-air-task-requires-todo t)
           (items (org-air-query-items))
           (routines (expand-file-name "routines.org" org-air-r77--dir)))
      ;; Both routines demote — the file's voters are all knowledge.
      (should (equal (org-air-r77--buckets "Water plants" items)
                     '(knowledge)))
      (should (equal (org-air-r77--buckets "Review Goals" items)
                     '(knowledge)))
      ;; The F7 vote follows: the FILE is knowledge...
      (should (eq (plist-get (org-air-query-file-meta routines) :ntype)
                  'knowledge))
      ;; ...and is IN the Revisit scope (`org-air-revisit-types' default
      ;; '(knowledge)) — one per-FILE row, the routine is reachable.
      (should (assoc routines (org-air-revisit--scope-entries))))))

;;;; -------------------------------------------------------------------
;;;; r77-9 — the inbox bypass is knob-immune.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r77-9-inbox-bypass-survives ()
  "Knob ON: a keyword-less SCHEDULED capture stays Inbox triage (r77-9).
The R54-2 inbox BYPASS precedes the type routing, so the capture rides
into the task buckets ('inbox + 'upcoming) — never 'knowledge — and the
filter's routed gate passes inbox-dwellers too (is:upcoming still
matches).  Capture flows are knob-immune."
  (skip-unless (locate-library "org-air"))
  (org-air-r77--with-corpus
      '(("inbox.org" .
         "#+title: inbox\n\n* Capture: renew passport\nSCHEDULED: <2026-06-16 Tue>\n"))
    (let* ((org-air-task-requires-todo t)
           (items (org-air-query-items))
           (capture (org-air-r77--item "renew passport" items))
           (buckets (org-air-classify-item capture org-air-test-now)))
      (should (memq 'inbox buckets))
      (should (memq 'upcoming buckets))
      (should-not (memq 'knowledge buckets))
      (should (org-air-r77--passes-p capture '("is:upcoming"))))))

;;;; -------------------------------------------------------------------
;;;; r77-10 — the overrides outrank the knob (per-item force-back).
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r77-10-overrides-outrank-knob ()
  "Knob ON: ORG_AIR_TYPE / a :task: tag force a routine back (r77-10).
The R54-2 overrides (steps 1–3) precede the gated task signal, so one
chosen keyword-less routine returns to ntype task + 'upcoming — the
right per-item granularity."
  (skip-unless (locate-library "org-air"))
  (org-air-r77--with-corpus
      '(("props.org" .
         "* Water plants\nSCHEDULED: <2026-06-16 Tue ++2w>\n:PROPERTIES:\n\
:ORG_AIR_TYPE: task\n:END:\n")
        ("tagged.org" .
         "* Feed sourdough starter :task:\nSCHEDULED: <2026-06-16 Tue ++1w>\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((org-air-task-requires-todo t)
           (items (org-air-query-items)))
      (dolist (title '("Water plants" "Feed sourdough starter"))
        (let ((item (org-air-r77--item title items)))
          (should (eq (org-air-item-ntype item) 'task))
          (should (memq 'upcoming
                        (org-air-classify-item item org-air-test-now))))))))

;;;; -------------------------------------------------------------------
;;;; r77-11 — step-6 subsumption: the knob wins the fall-through.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r77-11-step-6-subsumption ()
  "Knob ON + legacy `org-air-plain-heading-type' 'task: STILL knowledge
\(r77-11).  \"Task requires a keyword\" and \"every keyword-less heading
is a task\" are contradictory; the explicit knob wins, so its contract
\(no keyword => never a board task, overrides excepted) is TOTAL.
Reverting the step-6 change in `org-air-query--note-type' fails."
  (skip-unless (locate-library "org-air"))
  (org-air-r77--with-corpus
      `(("routines.org" . ,org-air-r77--routine)
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((org-air-task-requires-todo t)
           (org-air-plain-heading-type 'task) ; the legacy GTD-purist knob
           (items (org-air-query-items))
           (routine (org-air-r77--item "Water plants" items)))
      (should (eq (org-air-item-ntype routine) 'knowledge))
      (should (equal (org-air-classify-item routine org-air-test-now)
                     '(knowledge))))))

;;;; -------------------------------------------------------------------
;;;; r77-12 — the SEVENTH cache-key element.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r77-12-cache-key-seventh-element ()
  "The knob is the SEVENTH `org-air-view--cache-key' element (r77-12).
The key tracks the live value (a flip changes the key); a cache WRITTEN
under nil is a clean cold miss under t — nil from both
`org-air-view--cache-read' and `--cache-load', no error, no hydrate
\(the R53 skeleton + paced rescan law) — while the original setting
still hydrates (the miss is the KEY); and a crafted pre-R77 6-element
`:key' misses on length inequality.  Omitting the key element fails."
  (skip-unless (locate-library "org-air"))
  (org-air-r77--with-corpus
      `(("routines.org" . ,org-air-r77--routine)
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((items (org-air-query-items))
           (key (org-air-view--cache-key)))
      ;; Seventh element, tracking the live knob (nil here — the default).
      (should (= (length key) 7))
      (should (eq (nth 6 key) org-air-task-requires-todo))
      (should (null (nth 6 key)))
      ;; Write under nil; it round-trips under nil...
      (org-air-view--cache-write
       items (org-air-view--mtimes-snapshot (org-air-query-files)))
      (should (org-air-view--cache-read))
      ;; ...and is a CLEAN cold miss under t (no error, no hydrate).
      (let ((org-air-task-requires-todo t))
        (should (eq (nth 6 (org-air-view--cache-key)) t))
        (should-not (equal (org-air-view--cache-key) key))
        (should-not (org-air-view--cache-read))
        (should-not (org-air-view--cache-load)))
      ;; Back at nil the same file still hydrates — the miss IS the key.
      (should (org-air-view--cache-read))
      ;; A crafted pre-R77 6-element key misses on length inequality.
      (let ((print-length nil) (print-level nil))
        (write-region
         (prin1-to-string
          (list :version org-air-view--cache-version
                :key (butlast (org-air-view--cache-key))
                :mtimes nil :file-meta nil :visits nil :items nil))
         nil (expand-file-name org-air-cache-file) nil 'silent))
      (should-not (org-air-view--cache-read))
      (should-not (org-air-view--cache-load)))))

;;;; -------------------------------------------------------------------
;;;; r77-13 — AUDIT GAP: the deadline disjunct + the journal flavour.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r77-13-deadline-routine-and-journal-flavour ()
  "Knob ON: DEADLINE-only routines demote too; journal files flavour it.
Test-seat audit gap: r77-1 drove the step-4 gate's SCHEDULED disjunct
only — reverting the DEADLINE half alone went uncaught.  A keyword-less
DEADLINE heading demotes to `knowledge' exactly like the scheduled
routine (and fails `due:7d'/`is:upcoming' through the routed gate).
And per D2 step 5, a keyword-less routine in a JOURNAL-typed file falls
through to `journal' — equally off-board, the right flavour — whose
FILE stays OUT of the Revisit scope under the default
`org-air-revisit-types' '(knowledge) (the R54 F3 consistency in D7);
the routed filter gate covers the journal leg too (r77-4/5 drove only
knowledge)."
  (skip-unless (locate-library "org-air"))
  (org-air-r77--with-corpus
      '(("chores.org" .
         "* Quarterly: file taxes\nDEADLINE: <2026-06-16 Tue ++3m>\n")
        ("journal/routines.org" .
         "* Morning pages\nSCHEDULED: <2026-06-16 Tue ++1d>\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((org-air-task-requires-todo t)
           (items (org-air-query-items))
           (taxes (org-air-r77--item "file taxes" items))
           (pages (org-air-r77--item "Morning pages" items)))
      ;; The DEADLINE disjunct is gated exactly like SCHEDULED.
      (should (eq (org-air-item-ntype taxes) 'knowledge))
      (should (equal (org-air-classify-item taxes org-air-test-now)
                     '(knowledge)))
      (should-not (org-air-r77--passes-p taxes '("is:upcoming")))
      (should-not (org-air-r77--passes-p taxes '("due:7d")))
      ;; The journal-file routine takes the step-5 flavour…
      (should (eq (org-air-item-ntype pages) 'journal))
      (should (equal (org-air-classify-item pages org-air-test-now)
                     '(journal)))
      ;; …the routed gate's JOURNAL leg agrees on the token side…
      (should-not (org-air-r77--passes-p pages '("is:upcoming")))
      (should-not (org-air-r77--passes-p pages '("due:7d")))
      ;; …and the journal FILE stays out of the (knowledge-only)
      ;; Revisit scope — D7's third bullet.
      (let ((journal (expand-file-name "journal/routines.org"
                                       org-air-r77--dir)))
        (should (eq (plist-get (org-air-query-file-meta journal) :ntype)
                    'journal))
        (should-not (assoc journal (org-air-revisit--scope-entries)))))))

;;;; -------------------------------------------------------------------
;;;; r77-14 — AUDIT GAP: ARCHIVED handling is knob-immune.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r77-14-archived-unaffected ()
  "Archived items are buried identically under the knob ON and OFF.
Test-seat audit gap: the round ask's \"donep/archived unaffected\" —
r77-6 locked the donep half only.  An `org-archive-tag'-tagged
keyworded heading types `task' under BOTH knob values (the keyword is
the signal either way), classifies into ZERO buckets both ways (the
`org-air-classify--board-active-p' archive gate, untouched by R77) and
never matches `is:overdue' either way — the knob neither resurrects
nor re-types history."
  (skip-unless (locate-library "org-air"))
  (dolist (knob '(t nil))
    (org-air-r77--with-corpus
        '(("history.org" .
           "* TODO Old idea :ARCHIVE:\nSCHEDULED: <2026-06-01 Mon>\n")
          ("inbox.org" . "#+title: inbox\n"))
      (let* ((org-air-task-requires-todo knob)
             (items (org-air-query-items))
             (old (org-air-r77--item "Old idea" items)))
        (should (member org-archive-tag (org-air-item-tags old)))
        (should (eq (org-air-item-ntype old) 'task))
        (should (equal (org-air-classify-item old org-air-test-now) '()))
        (should-not (org-air-r77--passes-p old '("is:overdue")))))))

;;;; -------------------------------------------------------------------
;;;; r77-15 — AUDIT GAP: the Revisit SURFACE renders the demoted file.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r77-15-revisit-surface-renders-demoted-file ()
  "Knob ON: the REAL Revisit view renders the pure-routines file (r77-8+).
Test-seat audit gap: r77-8 stopped at `org-air-revisit--scope-entries';
the \"not lost\" claim deserves the actual surface.  Under the knob the
real `org-air-revisit' entry point renders the routines FILE as a row
\(the `org-air-revisit' row property carries its path — RET would open
it).  The D7 MIXED-file wrinkle is pinned as SPECCED, eyes open: ONE
demoted routine among keyworded tasks flips the file's F7 every-heading
vote to `knowledge', so the mixed file enters Revisit too — the
standing R54 F7 fork, flagged in D7, deliberately NOT re-ruled by R77."
  (skip-unless (locate-library "org-air"))
  (org-air-r77--with-corpus
      `(("routines.org" . ,org-air-r77--routine)
        ("mixed.org" .
         "* TODO Real work\nSCHEDULED: <2026-06-16 Tue>\n\
* Weekly: Review Goals\nSCHEDULED: <2026-06-21 Sun ++1w>\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((org-air-task-requires-todo t)
           (org-air-view-width 120)
           (org-air-view-height 50)
           (routines (expand-file-name "routines.org" org-air-r77--dir))
           (mixed (expand-file-name "mixed.org" org-air-r77--dir)))
      (unwind-protect
          (save-window-excursion
            (org-air-revisit)
            (with-current-buffer org-air-revisit-buffer-name
              ;; The rendered rows carry both files' identities — the
              ;; demoted routine is reachable ON the surface.
              (let ((row-files nil)
                    (pos (point-min)))
                (while (setq pos (text-property-not-all
                                  pos (point-max) 'org-air-revisit nil))
                  (push (car (get-text-property pos 'org-air-revisit))
                        row-files)
                  (setq pos (next-single-property-change
                             pos 'org-air-revisit nil (point-max))))
                (should (member routines row-files))
                ;; The D7 mixed-file wrinkle, as specced (F7 standing).
                (should (member mixed row-files)))))
        (let ((kill-buffer-query-functions nil))
          (when (get-buffer org-air-revisit-buffer-name)
            (kill-buffer org-air-revisit-buffer-name))))
      ;; The F7 votes behind the surface, for the record.
      (should (eq (plist-get (org-air-query-file-meta routines) :ntype)
                  'knowledge))
      (should (eq (plist-get (org-air-query-file-meta mixed) :ntype)
                  'knowledge)))))

(provide 'org-air-round77-test)
;;; org-air-round77-test.el ends here
