;;; org-air-round72-test.el --- executing ERTs for round-72 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for round-72 (air/v0.1/org-air-round72-design.org):
;; date/status filter tokens join the `/' mini-language — the R72-1
;; `qualifier:value' grammar (`is:overdue' / `is:upcoming' / `is:stale' /
;; `is:nodate' / `is:hipri' + `due:Nd/Nw' windows with `scheduled:' /
;; `deadline:' slot twins, `org-air-view--filter-token-parse'), R72-2
;; agreement by construction (the predicates ARE the hoisted classify
;; helpers, gated not-done/not-archived; an active window token WIDENS
;; the Upcoming horizon with the (DAY . HORIZON) memo key), and R72-3
;; discoverability/inheritance (the `/' completion offers the vocabulary
;; on the board + review; project/revisit pass no item and no vocab).
;;
;; All BATCH/headless: the fixture is hand-built `org-air-item' structs
;; with timestamp objects minted relative to the frozen instant
;; `org-air-test-now' (Mon 2026-06-15 10:00), (FILE . POS) cons markers
;; throughout (the cache-hydrated shape); filters are driven by binding
;; `org-air-view--tag-filter' directly and folding
;; `org-air-view--passes-filter-p' / reading the partition;
;; `org-air-view--filter-now' is let-bound to the frozen instant.  The
;; spec's twelve seams r72-1..r72-12 map onto the ERTs below.
;;
;; GUI residue (screenshot-confirm, not ERT-able): the CRM completion
;; popup showing the vocabulary; the rail Filter line with
;; `is:overdue AND #work' live.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)

(require 'org-air)
(require 'org-air-view)
(require 'org-air-classify)
(require 'org-air-project)
(require 'org-air-review)
(require 'org-air-revisit)
(require 'org-air-test-helpers)

;;;; ---------------------------------------------------------------------
;;;; Fixture — the probe-3 corpus, frozen on `org-air-test-now'.
;;;; ---------------------------------------------------------------------

(defconst org-air-r72--file "/tmp/org-air-r72-fixture/tasks.org"
  "Fixture file path (never exists — every predicate must answer from slots).")

(defun org-air-r72--ts (days)
  "Return an active org timestamp object DAYS from the frozen now."
  (org-timestamp-from-string
   (format-time-string "<%Y-%m-%d %a>"
                       (time-add org-air-test-now (days-to-time days)))))

(defun org-air-r72--epoch (days)
  "Return the epoch float DAYS from the frozen now."
  (float-time (time-add org-air-test-now (days-to-time days))))

(cl-defun org-air-r72--item (title &key scheduled deadline tags priority
                                   todo donep active-ts activity updated)
  "Build a cache-hydrated-shape `org-air-item' relative to the frozen now.
SCHEDULED / DEADLINE are day offsets (timestamp objects minted);
ACTIVE-TS / ACTIVITY / UPDATED are day offsets (epoch floats).  The
marker is a (FILE . POS) cons — the R53 cache-hydrated shape, so any
live-marker fallback must resolve to nil without opening a file.

UPDATED is the R93 recency slot.  It is left nil on purpose for every
item that is not deliberately quiet: this fixture's FILE is a fiction
with no entry in the scan's file-meta table, so a nil slot means the
age is UNKNOWN — and org-air refuses to nag about an item it cannot
date, at every threshold except `#A''s 0."
  (org-air-item-create
   :title title
   :tags tags
   :file org-air-r72--file
   :marker (cons org-air-r72--file 1)
   :kind 'heading
   :todo (or todo "TODO")
   :donep donep
   :priority priority
   :scheduled (and scheduled (org-air-r72--ts scheduled))
   :deadline (and deadline (org-air-r72--ts deadline))
   :active-ts (and active-ts (org-air-r72--epoch active-ts))
   :updated (and updated (org-air-r72--epoch updated))
   :activity (org-air-r72--epoch (or activity 0))))

(defun org-air-r72--fixture ()
  "Return the probe-3 corpus as an alist (KEY . ITEM), fresh structs."
  (list
   (cons 'past-sched (org-air-r72--item "Fix production release runbook"
                                        :scheduled -5 :tags '("work")))
   (cons 'past-dl (org-air-r72--item "Book dentist appointment"
                                     :deadline -3))
   (cons 'today (org-air-r72--item "Standup today" :scheduled 0))
   (cons 'dl3 (org-air-r72--item "Ship release notes"
                                 :deadline 3 :tags '("work")))
   (cons 'd7 (org-air-r72--item "Plan next sprint" :scheduled 7))
   (cons 'd8 (org-air-r72--item "Quarterly planning" :scheduled 8))
   (cons 'd10 (org-air-r72--item "Renew certificate" :scheduled 10))
   (cons 'dateless (org-air-r72--item "Untracked idea"))
   ;; R93: quiet for 30 days by its OWN recency stamp (the aging rule's
   ;; clock), on top of the dated-ness that used to be the Stale gate.
   (cons 'stale-active (org-air-r72--item "Quiet dated task"
                                          :active-ts -30 :activity -30
                                          :updated -30))
   (cons 'done-past (org-air-r72--item "Old done chore"
                                       :scheduled -5 :todo "DONE" :donep t))
   (cons 'archived-past (org-air-r72--item "Archived history"
                                           :deadline -5
                                           :tags (list org-archive-tag)))
   (cons 'hipri-dateless (org-air-r72--item "Urgent someday"
                                            :priority (org-get-priority "[#A]")))
   (cons 'tagged-overdue (org-air-r72--item "Tag chip owner"
                                            :scheduled 2 :tags '("overdue")))))

(defun org-air-r72--items (fixture)
  "Return FIXTURE's items in corpus order."
  (mapcar #'cdr fixture))

(defun org-air-r72--pass-keys (fixture tokens &optional match now)
  "Return FIXTURE keys passing TOKENS under MATCH (default `all') at NOW.
Drives the REAL fold: `org-air-view--tag-filter' set to TOKENS,
`org-air-view--filter-now' frozen, `org-air-view--passes-filter-p' per
item — the exact board path (R53: pure slot work, no rescan)."
  (let ((org-air-view--tag-filter tokens)
        (org-air-filter-match (or match 'all))
        (org-air-view--filter-now (or now org-air-test-now))
        (org-air-view--scope nil)
        (org-air-view--render-partition nil)
        (org-air-upcoming-days 7))
    (mapcar #'car
            (seq-filter (lambda (pair)
                          (org-air-view--passes-filter-p (cdr pair)))
                        fixture))))

;;;; ---------------------------------------------------------------------
;;;; r72-1 — the grammar: the probe-2 parse table pinned.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r72-1-token-grammar ()
  "R72-1: the closed grammar parses; every near-miss returns nil.
Case-insensitive qualifiers AND values, `2w' -> 14 days, `0d' -> 0, slot
symbols right; `#'-prefixed tokens refused outright (the tag branch owns
them forever)."
  ;; The grammar tokens parse.
  (should (equal '(is . overdue) (org-air-view--filter-token-parse "is:overdue")))
  (should (equal '(is . overdue) (org-air-view--filter-token-parse "IS:Overdue")))
  (should (equal '(is . upcoming) (org-air-view--filter-token-parse "is:upcoming")))
  (should (equal '(is . attention) (org-air-view--filter-token-parse "is:attention")))
  (should (equal '(is . attention) (org-air-view--filter-token-parse "IS:Attention")))
  ;; R93: `is:stale' is RETIRED as vocabulary but kept as a parse-only
  ;; ALIAS, so a saved bookmark or muscle memory resolves to the current
  ;; symbol -- once, here -- instead of falling through to a bare
  ;; substring search for the literal text "is:stale" (which would match
  ;; nothing and read like a bug).
  (should (equal '(is . attention) (org-air-view--filter-token-parse "is:stale")))
  (should (equal '(is . attention) (org-air-view--filter-token-parse "IS:Stale")))
  ;; ...and the retired spelling is NOT offered, so the vocabulary
  ;; org-air TEACHES is only ever the current one.
  (should (member "is:attention" (org-air-view--filter-vocabulary)))
  (should-not (member "is:stale" (org-air-view--filter-vocabulary)))
  (should (member "attention" org-air-view--filter-is-values))
  (should-not (member "stale" org-air-view--filter-is-values))
  (should (equal '(is . nodate) (org-air-view--filter-token-parse "is:nodate")))
  (should (equal '(is . hipri) (org-air-view--filter-token-parse "is:hipri")))
  (should (equal '(due . 7) (org-air-view--filter-token-parse "due:7d")))
  (should (equal '(due . 14) (org-air-view--filter-token-parse "DUE:2W")))
  (should (equal '(due . 0) (org-air-view--filter-token-parse "due:0d")))
  (should (equal '(scheduled . 3) (org-air-view--filter-token-parse "scheduled:3d")))
  (should (equal '(deadline . 7) (org-air-view--filter-token-parse "deadline:1w")))
  ;; The near-misses all return nil — the fall-through set (never-error).
  (dolist (miss '("overdue" "#overdue" "#is:overdue" "is:urgent"
                  "is:overdues" "due:7" "due:7x" "due:-1d" "due:"
                  "due:7d " "xdue:7d" "is:overdue,"
                  "is:stales" "is:attentions"))
    (should-not (org-air-view--filter-token-parse miss))))

;;;; ---------------------------------------------------------------------
;;;; r72-2 — is:overdue isolates; the gate buries DONE/ARCHIVED.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r72-2-is-overdue-isolates ()
  "R72-2: `is:overdue' passes ONLY the two live past-due items.
DONE-past and ARCHIVED-past are OUT (the `--board-active-p' gate — they
classify into no bucket either); today/+7d/dateless OUT."
  (let ((fixture (org-air-r72--fixture)))
    (should (equal '(past-sched past-dl)
                   (org-air-r72--pass-keys fixture '("is:overdue"))))))

;;;; ---------------------------------------------------------------------
;;;; r72-3 — the agreement theorem: filter <=> bucket, EXACT.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r72-3-filter-bucket-agreement ()
  "R72-3: over EVERY fixture item the filter and the buckets agree EXACTLY.
`due:7d' <=> `is:upcoming' <=> 'upcoming in `org-air-classify-item';
`is:attention' <=> 'attention; `is:overdue' <=> 'overdue;
`is:hipri' <=> 'high-priority.  Boundaries pinned:
day 0 IN, day 7 IN, day 8 OUT, past OUT.

R93 re-bless (honest — the theorem is STRONGER, not weaker): the
`is:stale' <=> 'stale leg is dead with its bucket, and its natural
successor `is:attention' <=> 'attention takes its seat.  `is:overdue'
<=> 'overdue joins as a fourth leg for free, because Overdue is a
bucket of its own now.  The retired spelling `is:stale' must select the
SAME set as `is:attention' (it parses to that predicate), which is what
makes the alias honest rather than a silent no-op.

R93 FIX-3 re-bless: the MEMBERS of the attention set moved, the theorem
did not.  `hipri-dateless' is a `#A' with an UNKNOWN age; at the old
`(?A . 0)' it surfaced unconditionally, at the new `(?A . 3)' it does
not — a positive threshold is a claim about elapsed time org-air never
measured.  The agreement LAW is what this test asserts, and it is
asserted per item over the whole fixture, so it holds either way; the
pinned set is re-measured, and the OLD set is pinned too, under the
`(?A . 0)' opt-in, so the equivalence is shown to track the knob rather
than a literal."
  (let* ((fixture (org-air-r72--fixture))
         (org-air-upcoming-days 7)
         (due7 (org-air-r72--pass-keys fixture '("due:7d")))
         (upcoming (org-air-r72--pass-keys fixture '("is:upcoming")))
         (attention (org-air-r72--pass-keys fixture '("is:attention")))
         (aliased (org-air-r72--pass-keys fixture '("is:stale")))
         (overdue (org-air-r72--pass-keys fixture '("is:overdue")))
         (hipri (org-air-r72--pass-keys fixture '("is:hipri"))))
    (should (equal due7 upcoming))
    (should (equal attention aliased))
    (pcase-dolist (`(,key . ,item) fixture)
      (let ((buckets (org-air-classify-item item org-air-test-now)))
        (should (eq (not (null (memq key due7)))
                    (not (null (memq 'upcoming buckets)))))
        (should (eq (not (null (memq key attention)))
                    (not (null (memq 'attention buckets)))))
        (should (eq (not (null (memq key overdue)))
                    (not (null (memq 'overdue buckets)))))
        (should (eq (not (null (memq key hipri)))
                    (not (null (memq 'high-priority buckets)))))))
    ;; Boundary table inside the same test.
    (should (memq 'today due7))          ; day 0 IN
    (should (memq 'd7 due7))             ; day 7 IN
    (should-not (memq 'd8 due7))         ; day 8 OUT
    (should-not (memq 'past-sched due7)) ; past OUT of every window
    (should-not (memq 'past-dl due7))
    ;; The quiet DATED item is the whole attention set: nothing is
    ;; nagged for merely lacking a date, and (FIX-3) nothing is nagged
    ;; for carrying a cookie either — the `#A' here has an unknown age.
    (should (equal '(stale-active) attention))
    (should (equal '(past-sched past-dl) overdue))
    (should (equal '(hipri-dateless) hipri))
    ;; The token tracks the KNOB, not a literal: point `#A' back at the
    ;; unconditional threshold and `is:attention' picks the `#A' up
    ;; again — filter and bucket still agreeing, item for item.
    (let ((org-air-attention-days '((?A . 0) (?B . 7) (?C . 14)
                                    (?D . 30) (?E . 30) (nil . 30))))
      (let ((attention-0 (org-air-r72--pass-keys fixture '("is:attention"))))
        (should (equal '(stale-active hipri-dateless) attention-0))
        (pcase-dolist (`(,key . ,item) fixture)
          (should (eq (not (null (memq key attention-0)))
                      (not (null (memq 'attention
                                       (org-air-classify-item
                                        item org-air-test-now)))))))))))

;;;; ---------------------------------------------------------------------
;;;; r72-4 — windows and slot scoping.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r72-4-windows-and-slots ()
  "R72-4: `due:2w' superset of `due:7d' (+8d/+10d in the former only);
`scheduled:3d' misses the +3d DEADLINE item and `deadline:3d' hits it
\(slot scoping); `due:0d' passes today only."
  (let* ((fixture (org-air-r72--fixture))
         (due7 (org-air-r72--pass-keys fixture '("due:7d")))
         (due2w (org-air-r72--pass-keys fixture '("due:2w"))))
    ;; due:2w ⊃ due:7d — every 7d member is a 2w member...
    (dolist (k due7) (should (memq k due2w)))
    ;; ...and the +8d/+10d items live in the wider window ONLY.
    (should (memq 'd8 due2w))
    (should (memq 'd10 due2w))
    (should-not (memq 'd8 due7))
    (should-not (memq 'd10 due7))
    ;; Slot scoping: the +3d item is DEADLINE-dated.
    (let ((sched3 (org-air-r72--pass-keys fixture '("scheduled:3d")))
          (dl3 (org-air-r72--pass-keys fixture '("deadline:3d"))))
      (should-not (memq 'dl3 sched3))
      (should (memq 'dl3 dl3))
      ;; today is SCHEDULED-dated: in scheduled:3d, out of deadline:3d.
      (should (memq 'today sched3))
      (should-not (memq 'today dl3)))
    ;; due:0d = today only.
    (should (equal '(today) (org-air-r72--pass-keys fixture '("due:0d"))))))

;;;; ---------------------------------------------------------------------
;;;; r72-5 — is:nodate is the R54-1 eligibility negation.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r72-5-is-nodate ()
  "R72-5: `is:nodate' passes the dateless + hipri-dateless items ONLY.
The active-ts-only item is OUT (dated under R54-1 — it feeds the stale
clock); equivalence with (not `org-air-classify--stale-eligible-p') ∧ the
gate pinned over the whole fixture."
  (let* ((fixture (org-air-r72--fixture))
         (nodate (org-air-r72--pass-keys fixture '("is:nodate"))))
    (should (equal '(dateless hipri-dateless) nodate))
    (should-not (memq 'stale-active nodate))
    (pcase-dolist (`(,key . ,item) fixture)
      (should (eq (not (null (memq key nodate)))
                  (not (null (and (org-air-classify--board-active-p item)
                                  (not (org-air-classify--stale-eligible-p
                                        item))))))))))

;;;; ---------------------------------------------------------------------
;;;; r72-6 — composition: one more predicate under the untouched fold.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r72-6-composition ()
  "R72-6: date/status tokens compose with #tag/text under M-/ (all/any).
`is:overdue,#work' under `all' = intersection; `is:overdue,due:7d' under
`any' = union (the user's two asks verbatim); `is:overdue,#work,release'
under `all' mixes all three token kinds through the ONE fold; the M-/
flip re-partitions with no special-casing (same matcher, spied to one
code path)."
  (let ((fixture (org-air-r72--fixture)))
    ;; Intersection: overdue AND #work — only the work-tagged past item.
    (should (equal '(past-sched)
                   (org-air-r72--pass-keys fixture '("is:overdue" "#work") 'all)))
    ;; Union: past OR next week — two tokens, one filter.
    (should (equal '(past-sched past-dl today dl3 d7 tagged-overdue)
                   (org-air-r72--pass-keys fixture '("is:overdue" "due:7d") 'any)))
    ;; All three token kinds under `all': date + tag + free text.
    (should (equal '(past-sched)
                   (org-air-r72--pass-keys fixture
                                           '("is:overdue" "#work" "release")
                                           'all)))
    ;; The M-/ flip runs the SAME matcher — spied to one code path.
    (let* ((orig (symbol-function 'org-air-view--filter-token-match-p))
           (calls 0))
      (cl-letf (((symbol-function 'org-air-view--filter-token-match-p)
                 (lambda (&rest args)
                   (cl-incf calls)
                   (apply orig args))))
        (let ((all-keys (org-air-r72--pass-keys
                         fixture '("is:overdue" "#work" "release") 'all))
              (all-calls calls))
          (should (> all-calls 0))
          (let ((any-keys (org-air-r72--pass-keys
                           fixture '("is:overdue" "#work" "release") 'any)))
            (should (> calls all-calls))
            ;; The flip genuinely re-partitions: any ⊃ all here.
            (should (equal '(past-sched) all-keys))
            (should (memq 'dl3 any-keys))
            (should (memq 'past-dl any-keys))))))))

;;;; ---------------------------------------------------------------------
;;;; r72-7 — disambiguation: chips, #tags and the quoted-label tell.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r72-7-disambiguation ()
  "R72-7: a literal `:overdue:' tag never collides with the keyword.
Bare `overdue' (the chip shape) substring-matches the TAGGED item
\(legacy, unchanged); `#overdue' tag-matches it and NOT the past-due
item; `is:overdue' matches the past-due items and NOT the tagged one;
`is:urgent' (unparsed) falls through to substring and its label renders
QUOTED while `is:overdue''s renders unquoted."
  (let ((fixture (org-air-r72--fixture)))
    ;; The chip's bare-name token: text/tag substring, legacy behaviour.
    (should (equal '(tagged-overdue)
                   (org-air-r72--pass-keys fixture '("overdue"))))
    ;; The #tag token: tag membership only.
    (should (equal '(tagged-overdue)
                   (org-air-r72--pass-keys fixture '("#overdue"))))
    ;; The date token: the past-due items only.
    (should (equal '(past-sched past-dl)
                   (org-air-r72--pass-keys fixture '("is:overdue"))))
    ;; The unparsed near-miss degrades to substring — matches nothing
    ;; here, errors nowhere.
    (should (equal '() (org-air-r72--pass-keys fixture '("is:urgent"))))
    ;; The label quoting is the tell: parsed verbatim-unquoted, typo quoted.
    (should (equal "is:overdue" (org-air-view--filter-token-label "is:overdue")))
    (should (equal "due:7d" (org-air-view--filter-token-label "due:7d")))
    (should (equal "\"is:urgent\"" (org-air-view--filter-token-label "is:urgent")))
    (should (equal "#overdue" (org-air-view--filter-token-label "#overdue")))))

;;;; ---------------------------------------------------------------------
;;;; r72-8 — no rescan (R53): every predicate answers from slots.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r72-8-no-rescan ()
  "R72-8: filter apply over the cons-marker fixture opens NOTHING.
`org-air-query--scan-file' / `org-air-query-items' (+ `-in-files') and
`find-file-noselect' spied at ZERO calls across every token kind; the
`--stale-eligible-p' live-marker fallback is provably nil-safe on cons
markers (the R53 P2 law)."
  (let ((fixture (org-air-r72--fixture))
        (scans 0))
    (cl-letf (((symbol-function 'org-air-query--scan-file)
               (lambda (&rest _) (cl-incf scans) nil))
              ((symbol-function 'org-air-query-items)
               (lambda (&rest _) (cl-incf scans) nil))
              ((symbol-function 'org-air-query-items-in-files)
               (lambda (&rest _) (cl-incf scans) nil))
              ((symbol-function 'find-file-noselect)
               (lambda (&rest _) (cl-incf scans) nil)))
      (dolist (tokens '(("is:overdue") ("is:upcoming") ("is:stale")
                        ("is:nodate") ("is:hipri") ("due:14d")
                        ("scheduled:3d") ("deadline:1w")
                        ("is:overdue" "#work" "release")))
        (org-air-r72--pass-keys fixture tokens 'any))
      (should (= 0 scans)))
    ;; The live-marker fallback resolves nil on a cons marker — no probe.
    (let ((dateless (cdr (assq 'dateless fixture))))
      (should (consp (org-air-item-marker dateless)))
      (should-not (org-air-classify--marker-active-ts dateless))
      (should-not (org-air-classify--stale-eligible-p dateless)))))

;;;; ---------------------------------------------------------------------
;;;; r72-9 — an active window token WIDENS the Upcoming horizon.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r72-9-widened-horizon ()
  "R72-9: `due:14d' widens the Upcoming horizon so +10d has a home row.
The +10d item classifies 'upcoming through `--classify-cached' and lands
in the partition's bucket table; clearing the filter (or `due:7d')
restores the 7-day membership; the memo KEY differs between the two
states and is IDENTICAL between no-filter and `due:7d' at defaults (no
gratuitous rebuild); the Upcoming empty-state string names the effective
horizon."
  (let* ((fixture (org-air-r72--fixture))
         (items (org-air-r72--items fixture))
         (d10 (cdr (assq 'd10 fixture)))
         (d8 (cdr (assq 'd8 fixture)))
         (org-air-upcoming-days 7)
         (org-air-filter-match 'all)
         (org-air-view--scope nil))
    (with-temp-buffer
      (setq-local org-air-view--items items)
      ;; Phase 1: due:14d active — the horizon widens to 14.
      (setq-local org-air-view--tag-filter '("due:14d"))
      (should (= 14 (org-air-view--filter-effective-horizon)))
      (should (equal "Nothing scheduled in the next 14 days."
                     (org-air-view--empty-upcoming)))
      (let* ((part (org-air-view--compute-partition items org-air-test-now))
             (upcoming (gethash 'upcoming (cddr part)))
             (key-14 org-air-view--classify-cache-day))
        ;; Every window-selected row has a home: +8d and +10d are Upcoming.
        (should (memq d10 upcoming))
        (should (memq d8 upcoming))
        (should (memq d10 (cadr part)))    ; visible under the filter
        ;; R83: the memo key is the LIST (DAY HORIZON BACKLOG-TAG); the
        ;; horizon element moved from `cdr' to the second slot.
        (should (equal 14 (nth 1 key-14)))
        ;; Phase 2: due:7d — the 7-day membership restored, key = default.
        (setq-local org-air-view--tag-filter '("due:7d"))
        (org-air-view--compute-partition items org-air-test-now)
        (let ((key-7 org-air-view--classify-cache-day))
          (should-not (equal key-14 key-7))
          (should-not (memq 'upcoming (org-air-view--classify-cached
                                       d10 org-air-test-now)))
          (should (equal "Nothing scheduled in the next 7 days."
                         (org-air-view--empty-upcoming)))
          ;; Phase 3: no filter — the memo key is IDENTICAL to due:7d at
          ;; defaults (no gratuitous rebuild) and +10d stays out.
          (setq-local org-air-view--tag-filter nil)
          (let ((table (org-air-view--classify-cache-ensure org-air-test-now)))
            (ignore table)
            (should (equal key-7 org-air-view--classify-cache-day)))
          (should-not (memq 'upcoming (org-air-view--classify-cached
                                       d10 org-air-test-now))))))))

;;;; ---------------------------------------------------------------------
;;;; r72-10 — the classify hoist is behaviour-neutral.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r72-10-hoist-is-neutral ()
  "R72-10: the hoisted predicates answer exactly as the pre-R72 bodies.
`--future-or-today-p' with no DAYS keeps the knob-window boundary table;
the FULL fixture's `org-air-classify-item' bucket lists are pinned
byte-equal to the pre-hoist answers; `--board-active-p' <=> the old
`unless' gate."
  (let ((org-air-upcoming-days 7)
        (fixture (org-air-r72--fixture)))
    ;; The no-DAYS boundary table (the pre-R72 signature's answers).
    (dolist (row '((-1 . nil) (0 . t) (3 . t) (7 . t) (8 . nil)))
      (should (eq (cdr row)
                  (and (org-air-classify--future-or-today-p
                        (org-air-r72--ts (car row)) org-air-test-now)
                       t))))
    ;; The full fixture's bucket lists, pinned (golden-in-test).
    ;; R93 re-bless: the pinned answers move with the rule, in SECTION
    ;; order (overdue, upcoming, high-priority, attention).  Read as a
    ;; table this IS the round's product change:
    ;;   past-sched / past-dl  overdue is its own bucket, and a missed
    ;;                         date alone no longer nags.
    ;;   dateless              the deleted no-date default: an undated
    ;;                         item of unknown age is NOT nagged.
    ;;   stale-active          quiet 30 days => attention (Stale's rule,
    ;;                         subsumed).
    ;;   hipri-dateless        R93 FIX-3 re-bless: a `#A' whose age is
    ;;                         UNKNOWN is High priority ONLY.  At the old
    ;;                         `(?A . 0)' it was in both sections; a
    ;;                         POSITIVE threshold is a claim about
    ;;                         elapsed time org-air never measured, so
    ;;                         the aging section declines while the
    ;;                         section with no clock still shows the row.
    ;;                         The `(?A . 0)' answers are pinned below,
    ;;                         so the hoist is neutral under BOTH.
    (dolist (expected '((past-sched . (overdue))
                        (past-dl . (overdue))
                        (today . (upcoming))
                        (dl3 . (upcoming))
                        (d7 . (upcoming))
                        (d8 . ())
                        (d10 . ())
                        (dateless . ())
                        (stale-active . (attention))
                        (done-past . ())
                        (archived-past . ())
                        (hipri-dateless . (high-priority))
                        (tagged-overdue . (upcoming))))
      (should (equal (cdr expected)
                     (org-air-classify-item (cdr (assq (car expected) fixture))
                                            org-air-test-now))))
    ;; The hoist is neutral under the OTHER setting of the one default
    ;; FIX-3 moved, too: the whole table re-asserted at `(?A . 0)',
    ;; where the only row that differs is the `#A' with the unknown age.
    (let ((org-air-attention-days '((?A . 0) (?B . 7) (?C . 14)
                                    (?D . 30) (?E . 30) (nil . 30))))
      (dolist (expected '((past-sched . (overdue))
                          (past-dl . (overdue))
                          (today . (upcoming))
                          (dl3 . (upcoming))
                          (d7 . (upcoming))
                          (d8 . ())
                          (d10 . ())
                          (dateless . ())
                          (stale-active . (attention))
                          (done-past . ())
                          (archived-past . ())
                          (hipri-dateless . (high-priority attention))
                          (tagged-overdue . (upcoming))))
        (should (equal (cdr expected)
                       (org-air-classify-item
                        (cdr (assq (car expected) fixture))
                        org-air-test-now)))))
    ;; --board-active-p <=> the old unless gate.
    (pcase-dolist (`(,_key . ,item) fixture)
      (should (eq (org-air-classify--board-active-p item)
                  (not (or (org-air-classify--done-p item)
                           (and (member org-archive-tag
                                        (org-air-item-tags item))
                                t))))))))

;;;; ---------------------------------------------------------------------
;;;; r72-11 — inheritance: board + review yes, project + revisit no.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r72-11-inheritance-edges ()
  "R72-11: a date token with NO item is vacuously false; review inherits;
the board's and review's interactive `/' candidate lists CONTAIN the
vocabulary (tracking a let-bound `org-air-upcoming-days' in the `due:'
example) while project's and revisit's do NOT."
  (let ((fixture (org-air-r72--fixture)))
    ;; NIL item ⇒ nil (the project/revisit shape: no slots, no claim).
    (let ((org-air-view--tag-filter '("is:overdue"))
          (org-air-filter-match 'all)
          (org-air-view--filter-now org-air-test-now))
      (should-not (org-air-view--tokens-pass-filter-p "anything" nil))
      ;; The legacy tags-only wrapper passes no item either.
      (should-not (org-air-view--tags-pass-filter-p '("work"))))
    ;; Review inherits: `--visible-items' keeps ONLY the past-due snapshots.
    (with-temp-buffer
      (setq-local org-air-review--items (org-air-r72--items fixture))
      (setq-local org-air-view--tag-filter '("is:overdue"))
      (let ((org-air-filter-match 'all)
            (org-air-view--filter-now org-air-test-now)
            (org-air-view--scope nil))
        (should (equal '("Fix production release runbook"
                         "Book dentist appointment")
                       (mapcar #'org-air-item-title
                               (org-air-review--visible-items))))))
    ;; The candidate lists: board + review OFFER, project + revisit do not.
    (let* ((captured nil)
           (org-air-upcoming-days 12))
      (cl-letf (((symbol-function 'completing-read-multiple)
                 (lambda (_prompt collection &rest _)
                   (setq captured collection)
                   nil))
                ((symbol-function 'org-air-view--render-current) #'ignore)
                ((symbol-function 'org-air-review--render-current) #'ignore)
                ((symbol-function 'org-air-project-refresh) #'ignore)
                ((symbol-function 'org-air-revisit--render-current) #'ignore))
        (with-temp-buffer
          (setq-local org-air-view--items (org-air-r72--items fixture))
          (call-interactively #'org-air-filter)
          (should (member "is:overdue" captured))
          (should (member "is:hipri" captured))
          (should (member "due:12d" captured))       ; knob-tracking example
          (should (member "scheduled:12d" captured))
          (should (member "deadline:12d" captured))
          (should-not (member "due:7d" captured)))
        (with-temp-buffer
          (setq-local org-air-review--items (org-air-r72--items fixture))
          (call-interactively #'org-air-review-filter)
          (should (member "is:overdue" captured))
          (should (member "due:12d" captured)))
        (with-temp-buffer
          (call-interactively #'org-air-project-filter)
          (should-not (member "is:overdue" captured))
          (should-not (member "due:12d" captured)))
        (with-temp-buffer
          (call-interactively #'org-air-revisit-filter)
          (should-not (member "is:overdue" captured))
          (should-not (member "due:12d" captured)))))))

;;;; ---------------------------------------------------------------------
;;;; r72-12 — one NOW: `org-air-view--filter-now' rules every predicate.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r72-12-one-now ()
  "R72-12: every date predicate honours a let-bound `--filter-now'.
The fixture answers differently under two frozen instants, and
`--compute-partition' binds the var to the SAME now it passes classify
\(spied equal)."
  (let ((fixture (org-air-r72--fixture)))
    ;; Two frozen instants, two answers: at NOW-10d the -5d item is
    ;; five days in the FUTURE — a window hit, not overdue.
    (let ((then (time-subtract org-air-test-now (days-to-time 10))))
      (should (memq 'past-sched
                    (org-air-r72--pass-keys fixture '("is:overdue")
                                            'all org-air-test-now)))
      (should-not (memq 'past-sched
                        (org-air-r72--pass-keys fixture '("is:overdue")
                                                'all then)))
      (should (memq 'past-sched
                    (org-air-r72--pass-keys fixture '("due:7d") 'all then)))
      (should-not (memq 'past-sched
                        (org-air-r72--pass-keys fixture '("due:7d")
                                                'all org-air-test-now))))
    ;; --compute-partition binds --filter-now to the SAME now it classifies
    ;; with (one instant per render).
    (let* ((items (org-air-r72--items fixture))
           (orig (symbol-function 'org-air-classify-item))
           (records nil))
      (cl-letf (((symbol-function 'org-air-classify-item)
                 (lambda (item &optional now)
                   (push (cons now org-air-view--filter-now) records)
                   (funcall orig item now))))
        (with-temp-buffer
          (setq-local org-air-view--items items)
          (setq-local org-air-view--tag-filter '("is:overdue"))
          (let ((org-air-filter-match 'all)
                (org-air-view--scope nil))
            (org-air-view--compute-partition items org-air-test-now))))
      (should records)
      (pcase-dolist (`(,now . ,filter-now) records)
        (should (equal now org-air-test-now))
        (should (equal filter-now org-air-test-now))))))

;;;; ---------------------------------------------------------------------
;;;; Test-seat AUDIT GAP ERTs (round-72 closeout).  The independent audit
;;;; of the twelve impl seams found four uncovered edges; each ERT below
;;;; drives one of them.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r72-13-overdue-agrees-with-attention-members ()
  "AUDIT GAP: `is:overdue' <=> the OVERDUE bucket, exactly and only.
r72-3 drove the agreement theorem for upcoming/attention/hipri; the
overdue token's bucket home was only asserted as a SET (r72-2), never
against the bucket itself.

R93 re-bless (honest — the law got SHARPER).  Pre-R93 overdue was a
disjunct of Needs attention, so this test could only say \"every
`is:overdue' hit has a home row in Needs attention, and the attention
members it does not select are the no-date/stale/hipri remainder\".
R93 split Overdue into its own bucket and its own section, so the
statement is now an EXACT equivalence with no remainder to explain:
`is:overdue' <=> 'overdue, member for member.  The two sections are
independent — a freshly-touched overdue item is in Overdue and NOT in
Needs attention, and a quiet item with no date is the reverse — which
is the whole point of the split, so both directions are pinned.

R93 FIX-3 re-bless: the attention MEMBER list below drops
`hipri-dateless' (a `#A' with an unknown age — no longer surfaced by a
cookie alone), which makes the independence claim STRONGER, not weaker:
the two sets are disjoint here with no `#A' overlap left to excuse.  The
`is:overdue' <=> 'overdue equivalence this test owns is untouched."
  (let* ((fixture (org-air-r72--fixture))
         (org-air-upcoming-days 7)
         (overdue (org-air-r72--pass-keys fixture '("is:overdue")))
         (attention (org-air-r72--pass-keys fixture '("is:attention"))))
    (pcase-dolist (`(,key . ,item) fixture)
      (let ((buckets (org-air-classify-item item org-air-test-now)))
        ;; token <=> bucket, by construction (one shared predicate).
        (should (eq (not (null (memq key overdue)))
                    (not (null (memq 'overdue buckets)))))
        ;; and the bucket <=> the predicate the section body calls,
        ;; under the two enclosing gates every sibling bucket shares
        ;; (the R72 board-active top gate, the R83 backlog routing).
        (should (eq (not (null (memq 'overdue buckets)))
                    (and (org-air-classify--board-active-p item)
                         (not (org-air-classify--backlog-p item))
                         (org-air-classify--overdue-p item org-air-test-now)
                         t)))
        ;; every `is:overdue' hit renders somewhere: Overdue is its home.
        (when (memq key overdue)
          (should (memq 'overdue buckets)))))
    ;; The two sections are INDEPENDENT: neither contains the other.
    (should (equal '(past-sched past-dl) overdue))
    (should (equal '(stale-active) attention))
    (should-not (seq-intersection overdue attention))
    ;; Overdue and freshly touched => Overdue only.  That is the R93
    ;; ruling in one row: being LATE is not the same as having gone
    ;; QUIET, and the board says so in two different places.
    (let ((touched (org-air-r72--item "Overdue, freshly touched"
                                      :deadline -3 :updated 0)))
      (should (org-air-classify--overdue-p touched org-air-test-now))
      (should (equal '(overdue)
                     (org-air-classify-item touched org-air-test-now))))))

(ert-deftest org-air-r72-14-due-2w-widens-and-shows ()
  "AUDIT GAP: the user's literal ask with the literal `due:2w' TOKEN.
r72-9 drove the widening with `due:14d'; the `w' spelling only ever met
the parser (r72-1).  Pinned end-to-end here: `due:2w' selects the SAME
set as `due:14d'; overdue and the DONE/ARCHIVED buried items stay OUT of
the wide window; the +10d item is both MATCHED (visible under the
filter) and SHOWN (an Upcoming member of the partition's bucket table)
under the widened 14-day horizon; an UNPARSED near-miss (`due:99x')
widens NOTHING; and a mixed `is:upcoming,due:2w' any-filter still widens
to the window's span (`is:upcoming' itself keeps the knob meaning — its
matches are a subset of the wider window, so every selected row has a
home)."
  (let* ((fixture (org-air-r72--fixture))
         (items (org-air-r72--items fixture))
         (d10 (cdr (assq 'd10 fixture)))
         (org-air-upcoming-days 7)
         (org-air-stale-days 21)
         (org-air-filter-match 'all)
         (org-air-view--scope nil)
         (due2w (org-air-r72--pass-keys fixture '("due:2w"))))
    ;; due:2w == due:14d, the SAME set (w = 7×N by grammar AND by fold).
    (should (equal due2w (org-air-r72--pass-keys fixture '("due:14d"))))
    ;; the wide window still excludes the past (is:overdue owns it)…
    (should-not (memq 'past-sched due2w))
    (should-not (memq 'past-dl due2w))
    ;; …and never resurrects what the board buries (the gate).
    (should-not (memq 'done-past due2w))
    (should-not (memq 'archived-past due2w))
    ;; matched AND shown: +10d is visible under the filter and lives in
    ;; the widened Upcoming bucket of the REAL partition.
    (with-temp-buffer
      (setq-local org-air-view--items items)
      (setq-local org-air-view--tag-filter '("due:2w"))
      (should (= 14 (org-air-view--filter-effective-horizon)))
      (should (equal "Nothing scheduled in the next 14 days."
                     (org-air-view--empty-upcoming)))
      (let* ((part (org-air-view--compute-partition items org-air-test-now))
             (upcoming (gethash 'upcoming (cddr part))))
        (should (memq d10 (cadr part)))      ; matched (visible)
        (should (memq d10 upcoming))         ; shown (a home row)
        ;; R83: horizon is the second element of (DAY HORIZON BACKLOG-TAG).
        (should (equal 14 (nth 1 org-air-view--classify-cache-day))))
      ;; an unparsed near-miss is NOT a window: no widening.
      (setq-local org-air-view--tag-filter '("due:99x"))
      (should (= 7 (org-air-view--filter-effective-horizon)))
      ;; the mixed any-filter: `is:upcoming' widens nothing itself, the
      ;; window token widens for both — the union is the wider set.
      (setq-local org-air-view--tag-filter '("is:upcoming" "due:2w"))
      (should (= 14 (org-air-view--filter-effective-horizon))))
    (let ((org-air-filter-match 'any))
      (should (equal due2w (org-air-r72--pass-keys fixture
                                                   '("is:upcoming" "due:2w")
                                                   'any))))))

(ert-deftest org-air-r72-15-rail-filter-line-renders-tokens ()
  "AUDIT GAP: the rail Filter line + `Match:' render the R72 tokens.
r72-7 pinned `--filter-token-label' as a FUNCTION; nothing drove the
rendered rail surface.  Pinned here on `--insert-rail-filters' output:
`is:overdue,#work' under `all' reads `is:overdue AND #work' (the parsed
token verbatim-unquoted beside the tag chip) with the `Match: AND' cue;
the M-/ flip reads `is:overdue OR #work' + `Match: OR'; a lone window
token reads `due:2w' unquoted; and the unparsed near-miss `is:urgent'
renders QUOTED in the same line — the tell, on the rendered surface."
  (with-temp-buffer
    (let ((org-air-show-rail-filters t)
          (org-air-view--tag-filter '("is:overdue" "#work"))
          (org-air-view--scope nil)
          (org-air-filter-match 'all))
      (org-air-view--insert-rail-filters 40)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "is:overdue AND #work" text))
        (should (string-match-p "Match: AND" text))
        ;; parsed = verbatim-unquoted on the surface.
        (should-not (string-match-p "\"is:overdue\"" text))))
    (erase-buffer)
    (let ((org-air-show-rail-filters t)
          (org-air-view--tag-filter '("is:overdue" "#work"))
          (org-air-view--scope nil)
          (org-air-filter-match 'any))
      (org-air-view--insert-rail-filters 40)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "is:overdue OR #work" text))
        (should (string-match-p "Match: OR" text))))
    (erase-buffer)
    (let ((org-air-show-rail-filters t)
          (org-air-view--tag-filter '("due:2w"))
          (org-air-view--scope nil)
          (org-air-filter-match 'all))
      (org-air-view--insert-rail-filters 40)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "due:2w" text))
        (should-not (string-match-p "\"due:2w\"" text))))
    (erase-buffer)
    (let ((org-air-show-rail-filters t)
          (org-air-view--tag-filter '("is:urgent" "#work"))
          (org-air-view--scope nil)
          (org-air-filter-match 'all))
      (org-air-view--insert-rail-filters 40)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        ;; the typo is text-search, and the rail SAYS so: quoted.
        (should (string-match-p "\"is:urgent\" AND #work" text))))))

(ert-deftest org-air-r72-16-interactive-filter-apply-no-rescan ()
  "AUDIT GAP: the R53 no-rescan law at the COMMAND seam.
r72-8 spied the bare predicate fold; nothing drove the interactive apply
path (`org-air-filter' -> `--render-current' -> the real board render).
Pinned here on a WARM fully-scanned board: applying `is:overdue,due:2w'
through the command re-renders over the CACHED items with the scan layer
\(`org-air-query--scan-file' / `org-air-query-items' / `-in-files') and
`find-file-noselect' spied at ZERO calls — and the render is REAL: the
repaint tick advances and the widened-horizon memo key (DAY . 14) lands;
clearing through the command restores the default (DAY . knob) key,
still scan-free.  Revert-red: the pre-R72 memo key was the bare day
integer, so the key conjuncts fail there.

R93 re-bless (honest — no conjunct weakened): the memo key gained the
two aging-threshold knobs, so the literal key shape below grew by two
elements.  The SUBJECT — a filter apply repaints and never rescans — is
untouched, and the widened-horizon element is still asserted in place.

`org-air-show-inspector' is bound nil: the rail inspector's CREATED
hydration is the ONE design-sanctioned bounded file probe (one file,
for the single inspected item, user-driven — pre-R72 behaviour, fires
on ANY render with an item at point) and is not the filter's doing; with
it out of frame the ZERO-opens assert pins the R72 contract exactly."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (let ((org-air-view-width 120)
          (org-air-view-height 50)
          (org-air-view-buffer-name "*org-air-r72*")
          (org-air-cache-file
           (expand-file-name "cache/board-r72.eld" org-air-test--dir))
          (org-air-show-inspector nil)
          (org-air-filter-match 'all))
      (unwind-protect
          (with-current-buffer (get-buffer-create org-air-view-buffer-name)
            (unless (derived-mode-p 'org-air-view-mode) (org-air-view-mode))
            ;; warm board: ONE full synchronous scan, then paint (the r42
            ;; warm-board shape).
            (let ((files (org-air-query-files)))
              (setq org-air-view--items (org-air-query-items)
                    org-air-view--items-key (list org-air-files
                                                  org-air-inbox-file)
                    org-air-view--classify-cache nil
                    org-air-view--items-mtimes
                    (org-air-view--mtimes-snapshot files))
              (org-air-view--render org-air-view--items nil))
            (let ((scans 0) (opens 0))
              (cl-letf (((symbol-function 'org-air-query--scan-file)
                         (lambda (&rest _) (cl-incf scans) nil))
                        ((symbol-function 'org-air-query-items)
                         (lambda (&rest _) (cl-incf scans) nil))
                        ((symbol-function 'org-air-query-items-in-files)
                         (lambda (&rest _) (cl-incf scans) nil))
                        ((symbol-function 'find-file-noselect)
                         (lambda (&rest _) (cl-incf opens) nil)))
                (let ((tick0 (buffer-chars-modified-tick))
                      (today (time-to-days (current-time))))
                  ;; the user's two asks, applied through the COMMAND.
                  (org-air-filter '("is:overdue" "due:2w"))
                  (should (equal '("is:overdue" "due:2w")
                                 (org-air-view--filter-tags)))
                  ;; the render was REAL…
                  (should (> (buffer-chars-modified-tick) tick0))
                  ;; …the widened-horizon memo key landed (R83: the key is
                  ;; the LIST (DAY HORIZON BACKLOG-TAG), not a bare cons)…
                  (should (equal (list today 14 org-air-backlog-tag
                                       org-air-attention-days
                                       org-air-attention-default-days)
                                 org-air-view--classify-cache-day))
                  ;; …and NOTHING was scanned or opened (R53).
                  (should (= 0 scans))
                  (should (= 0 opens))
                  ;; clearing restores the default key, still scan-free.
                  (let ((tick1 (buffer-chars-modified-tick)))
                    (org-air-filter nil)
                    (should-not (org-air-view--filter-tags))
                    (should (> (buffer-chars-modified-tick) tick1))
                    (should (equal (list today org-air-upcoming-days
                                          org-air-backlog-tag
                                          org-air-attention-days
                                          org-air-attention-default-days)
                                   org-air-view--classify-cache-day))
                    (should (= 0 scans))
                    (should (= 0 opens)))))))
        (when (get-buffer "*org-air-r72*")
          (let ((kill-buffer-query-functions nil))
            (kill-buffer "*org-air-r72*")))))))

(provide 'org-air-round72-test)
;;; org-air-round72-test.el ends here
