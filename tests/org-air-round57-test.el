;;; org-air-round57-test.el --- executing ERTs for v0.5 round-57 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.5 round-57 (air/v0.5/org-air-round57-design.org):
;; respect the user's `org-todo-keywords' — org-air's vocabulary is only
;; ever a SUPPLEMENT, never a replacement (the screenshot-confirmed
;; CLOSED/DROPPED flood fix), plus the R57-2 Air-aligned READY/WIP/COMP/
;; DROP keywords and the defensive Org-customisation-respect audit fixes
;; (cache-key vocabulary coherence, the merged classify fallback done
;; set, the donep-aware face fallback, archived trees off the board).
;;
;; All BATCH/headless, driven through the spec's named ERT seams T1-T11.
;; The scan tests bind the reporting user's EXACT global
;;   ((sequence "TODO(t)" "HOLD(h)" "|" "DONE(d!)" "DROPPED(x@)" "CLOSED"))
;; around `org-air-query--scan-file' on temp fixtures — a `let' on
;; `org-todo-keywords' (no buffer-local binding exists in the work
;; buffer) IS the default binding `org-set-regexps-and-options' consults
;; when a file declares no `#+TODO:' of its own.
;;
;;   r57-1-closed-flood-fix (T1) — `* CLOSED Foo' in a keyword-less file
;;     parses todo="CLOSED", title="Foo" (keyword NOT swallowed into the
;;     title), donep t, and classifies into ZERO task buckets.  Reverting
;;     the R57-1 merge (the old replace-with-org-air-vocab scan) FAILS
;;     (todo nil, title "CLOSED Foo", donep nil, `attention' — the flood).
;;   r57-2-user-done-twin-and-hold (T2+T3) — `* DROPPED Bar' (the user's
;;     other done keyword) => donep t, off the board (revert FAILS); and
;;     `* HOLD Baz' stays a recognised NOT-done task (the USER put HOLD
;;     before the bar — passes pre-R57 by accident of vocabulary overlap;
;;     locked here as a fence against future re-stomping).
;;   r57-3-air-supplement-keywords (T4) — `* READY Qux' / `* WIP Quux'
;;     => not-done tasks; `* COMP …' / `* DROP …' => done, off the board
;;     — in a keyword-less fixture under the user's global (org-air's
;;     supplement).  Reverting R57-2 (keyword removal) FAILS; reverting
;;     the merge FAILS (title swallow).
;;   r57-4-merge-dedup-and-preservation (T5) — the merged value's car is
;;     the user's sequence `equal'-VERBATIM (fast keys (t)/(d!)/(x@) and
;;     logging specs intact); the ONE supplement sequence carries an
;;     explicit "|", READY/WIP before it, COMP/DROP after it, and nothing
;;     the user declared at bare-name level (no double DONE anywhere).
;;     Revert FAILS (no base, no such shape).
;;   r57-5-merge-shapes-never-signal (T6) — multiple sequences + (type …)
;;     bases dedup across ALL of them; a legacy flat string list
;;     normalises to one interpreted sequence + supplement; nil and
;;     malformed (non-list) globals degrade to Org's own default base,
;;     signalling nothing; a fully-declared global returns UNCHANGED (no
;;     empty supplement); a keyword in both supplement halves dedups to
;;     the :not-done side.  Revert FAILS.
;;   r57-6-file-own-declaration-wins (T7) — in a `#+TODO: OPEN | SHUT'
;;     file under the user's global, `* OPEN A' / `* SHUT C' parse by the
;;     FILE's vocabulary and `* NEXT B' / `* CLOSED E' are title-swallowed
;;     (in-buffer keywords REPLACE the default; org-air's merge lives in
;;     the let-bound DEFAULT, never injected into declared files) — an
;;     implementation that appends to in-buffer keywords FAILS here.  The
;;     same test scans a keyword-less sibling file (`* CLOSED Gone') to
;;     prove the merged default reaches undeclared files — which makes
;;     the whole ERT revert-RED, not just a fence.
;;   r57-7-cache-key-carries-vocabulary (T8) — a cache written under
;;     vocabulary A does not hydrate under vocabulary B
;;     (`org-air-view--cache-read' => nil on the `:key' mismatch), and a
;;     crafted pre-R57 2-element `:key' cache also misses.  Reverting the
;;     `org-air-view--cache-key' extension FAILS (stale hydration).
;;   r57-8-classify-fallback-merged-done-set (T9) —
;;     `org-air-query-merged-done-keywords' derives the bare done set
;;     from the merged vocabulary exactly as Org does per sequence, and
;;     an item built OUTSIDE the scan (no live marker) with todo "CLOSED"
;;     is `org-air-classify--done-p' via that fallback.  Reverting to the
;;     hard-wired ("DONE") FAILS.
;;   r57-9-donep-aware-todo-face (T10) — (org-air-view--todo-face
;;     "CLOSED" t) => `org-air-face-done'; nil DONEP keeps
;;     `org-air-face-todo'; READY/WIP wear `org-air-face-todo-next' and
;;     COMP/DROP `org-air-face-done' (the R57-2 face table).  Revert
;;     FAILS (wrong-number-of-args / fallback face).
;;   r57-10-archived-trees-off-board (T11) — a heading tagged
;;     `org-archive-tag' AND a child under it (default tag inheritance:
;;     the scan's `org-get-tags' carries inherited tags) classify into
;;     ZERO task buckets while an unarchived control sibling stays on the
;;     board.  Reverting the classify exclusion FAILS.
;;
;; REVERT-FAIL verified against the pre-impl trunk in a scratch
;; workspace: r57-1, -2, -3, -4, -5, -6, -7, -8, -9 and -10 all fail
;; there (r57-2's HOLD half and r57-6's file-wins half pass pre-R57 by
;; design — the spec's own T3/T7 fence rulings — but each shares its ERT
;; with a revert-red conjunct, so every ERT goes RED on revert).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-round53-test)          ; temp-corpus scan helpers

(when (locate-library "org-air")
  (require 'org-air))

(defconst org-air-r57--user-kws
  '((sequence "TODO(t)" "HOLD(h)" "|" "DONE(d!)" "DROPPED(x@)" "CLOSED"))
  "The reporting user's exact global `org-todo-keywords' (round-57 spec).
Fast-access keys and `!'/`@' logging specs included — the merge must
preserve them VERBATIM.")

(defconst org-air-r57--task-buckets
  '(attention upcoming stale high-priority inbox)
  "The GTD board task buckets a done/archived item must never enter.")

(defun org-air-r57--task-buckets-of (item)
  "Return the task buckets `org-air-classify-item' puts ITEM in.
Classified against the frozen `org-air-test-now'; the non-board buckets
\(notes/knowledge/journal) are filtered out, so nil means \"not on the
GTD board\"."
  (seq-intersection (org-air-classify-item item org-air-test-now)
                    org-air-r57--task-buckets))

(defun org-air-r57--scan-under-user-kws (name)
  "Scan corpus file NAME with the user's global bound; return the items.
Binds `org-todo-keywords' to `org-air-r57--user-kws' around
`org-air-query--scan-file' — the spec's seam: with no buffer-local
binding in the work buffer, the `let' IS the default binding
`org-set-regexps-and-options' consults for a keyword-less file."
  (let ((org-todo-keywords org-air-r57--user-kws))
    (org-air-query--scan-file (expand-file-name name org-air-r53--dir))))

(defun org-air-r57--bare-names (kws)
  "Return the bare keyword names of KWS, \"|\" separators dropped."
  (mapcar #'org-air-query--todo-keyword-name (remove "|" kws)))

;;;; -------------------------------------------------------------------
;;;; T1 — THE FLOOD FIX: `* CLOSED Foo' in a keyword-less scanned file
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r57-1-closed-flood-fix ()
  "T1: a `* CLOSED Foo' heading in a file with NO `#+TODO:' line, scanned
under the user's global, parses todo=\"CLOSED\" with title \"Foo\" (the
keyword NOT swallowed into the title), donep t, and is EXCLUDED from
every board bucket — the CLOSED-flood fix.  Reverting the R57-1 merge
\(the old replace-with-org-air-vocab scan, CLOSED unknown) FAILS: todo
nil, title \"CLOSED Foo\", donep nil, and the years-old SCHEDULED lands
it in Needs attention as overdue 1000+ days."
  (skip-unless (locate-library "org-air"))
  (org-air-r53--with-corpus
      '(("history.org" . "* CLOSED Foo\nSCHEDULED: <2023-01-01 Sun>\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((items (org-air-r57--scan-under-user-kws "history.org"))
           (it (org-air-test-find-item "Foo" items)))
      (should it)
      (should (equal (org-air-item-todo it) "CLOSED"))
      (should (equal (org-air-item-title it) "Foo"))
      (should (org-air-item-donep it))
      ;; A done task, not a knowledge note (R54 constraint in the spec).
      (should (eq (org-air-item-ntype it) 'task))
      ;; OFF the board: no Needs-attention, no anything.
      (should-not (org-air-r57--task-buckets-of it)))))

;;;; -------------------------------------------------------------------
;;;; T2 + T3 — the user's other done keyword; the user's HOLD placement
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r57-2-user-done-twin-and-hold ()
  "T2: `* DROPPED Bar' (the user's OTHER custom done keyword) => donep t
and off the board; revert FAILS exactly like T1.  T3: `* HOLD Baz' stays
a recognised NOT-done task — the USER put HOLD before the bar, and
org-air's own vocabulary must never override that placement (passes
pre-R57 by accident of overlap; locked as a fence against a future
re-stomp)."
  (skip-unless (locate-library "org-air"))
  (org-air-r53--with-corpus
      '(("history.org" . "* DROPPED Bar\nDEADLINE: <2022-06-01 Wed>\n* HOLD Baz\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((items (org-air-r57--scan-under-user-kws "history.org"))
           (dropped (org-air-test-find-item "Bar" items))
           (held (org-air-test-find-item "Baz" items)))
      ;; T2 — the done twin.
      (should dropped)
      (should (equal (org-air-item-todo dropped) "DROPPED"))
      (should (equal (org-air-item-title dropped) "Bar"))
      (should (org-air-item-donep dropped))
      (should-not (org-air-r57--task-buckets-of dropped))
      ;; T3 — the not-done semantics the user declared.
      (should held)
      (should (equal (org-air-item-todo held) "HOLD"))
      (should-not (org-air-item-donep held))
      (should (eq (org-air-item-ntype held) 'task))
      ;; a live not-done task IS board material.
      (should (memq 'attention (org-air-r57--task-buckets-of held))))))

;;;; -------------------------------------------------------------------
;;;; T4 — org-air's supplement stays alive: the R57-2 Air keywords
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r57-3-air-supplement-keywords ()
  "T4: in the same keyword-less fixture under the user's global, org-air's
SUPPLEMENT still applies — `* READY Qux' / `* WIP Quux' parse as
not-done tasks and `* COMP …' / `* DROP …' as done, off the board (the
R57-2 Air-state keywords).  Reverting R57-2 (the keywords absent from
`org-air-todo-keywords') FAILS; reverting the R57-1 merge FAILS too (the
old fixed vocabulary had none of them — title swallow)."
  (skip-unless (locate-library "org-air"))
  (org-air-r53--with-corpus
      '(("air.org" . "* READY Qux\n* WIP Quux\n* COMP Shipped widget\n* DROP Abandoned widget\nDEADLINE: <2023-02-03 Fri>\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((items (org-air-r57--scan-under-user-kws "air.org"))
           (ready (org-air-test-find-item "Qux" items))
           (wip (org-air-test-find-item "Quux" items))
           (comp (org-air-test-find-item "Shipped" items))
           (drop (org-air-test-find-item "Abandoned" items)))
      ;; READY / WIP: not-done tasks, on the board.
      (should ready)
      (should (equal (org-air-item-todo ready) "READY"))
      (should (equal (org-air-item-title ready) "Qux"))
      (should-not (org-air-item-donep ready))
      (should (eq (org-air-item-ntype ready) 'task))
      (should (memq 'attention (org-air-r57--task-buckets-of ready)))
      (should wip)
      (should (equal (org-air-item-todo wip) "WIP"))
      (should-not (org-air-item-donep wip))
      ;; COMP / DROP: done, off the board.
      (should comp)
      (should (equal (org-air-item-todo comp) "COMP"))
      (should (org-air-item-donep comp))
      (should-not (org-air-r57--task-buckets-of comp))
      (should drop)
      (should (equal (org-air-item-todo drop) "DROP"))
      (should (org-air-item-donep drop))
      (should-not (org-air-r57--task-buckets-of drop)))))

;;;; -------------------------------------------------------------------
;;;; T5 — merge unit: dedup + verbatim preservation
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r57-4-merge-dedup-and-preservation ()
  "T5: `org-air-query--scan-todo-keywords' under the user's global returns
the user's sequence as its car `equal'-VERBATIM — fast-access keys (t) /
\(d!) / (x@) and logging specs intact — plus exactly ONE supplement
sequence with an explicit \"|\", READY/WIP before it, COMP/DROP after
it, and NOTHING the user already declared at bare-name level (a user's
\"DONE(d!)\" dedups org-air's \"DONE\": no double DONE anywhere in the
merged value).  Reverting the merge FAILS (no user base, no such shape)."
  (skip-unless (locate-library "org-air"))
  (let ((merged (let ((org-todo-keywords org-air-r57--user-kws))
                  (org-air-query--scan-todo-keywords))))
    (should (= (length merged) 2))
    ;; the base: the user's sequence, byte-for-byte.
    (should (equal (car merged) (car org-air-r57--user-kws)))
    (let* ((supp (cadr merged))
           (kws (cdr supp))
           (before-bar (seq-take-while (lambda (k) (not (equal k "|"))) kws))
           (after-bar (cdr (member "|" kws))))
      (should (eq (car supp) 'sequence))
      ;; the ALWAYS-explicit bar, exactly once.
      (should (= (seq-count (lambda (k) (equal k "|")) kws) 1))
      ;; dedup at bare-name level: nothing the user declared supplements.
      (dolist (name '("TODO" "HOLD" "DONE" "DROPPED" "CLOSED"))
        (should-not (member name (org-air-r57--bare-names kws))))
      ;; the Air keywords sit on the correct side of the bar.
      (should (member "READY" before-bar))
      (should (member "WIP" before-bar))
      (should (member "COMP" after-bar))
      (should (member "DROP" after-bar))
      ;; supplement keywords carry NO invented fast keys.
      (dolist (kw kws)
        (should (equal kw (org-air-query--todo-keyword-name kw)))))
    ;; no double DONE across the WHOLE merged value.
    (let ((all (cl-loop for seq in merged
                        append (org-air-r57--bare-names (cdr seq)))))
      (should (= (seq-count (lambda (n) (equal n "DONE")) all) 1)))))

;;;; -------------------------------------------------------------------
;;;; T6 — merge unit: shapes, degradation, never-signal
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r57-5-merge-shapes-never-signal ()
  "T6: the merge handles every global shape without signalling.  Multiple
sequences + (type …) entries dedup across ALL of them, the base passing
through verbatim; a legacy flat string list normalises to ONE sequence
under `org-todo-interpretation'; nil and malformed (non-list) globals
degrade to Org's own default base + the full supplement; a global that
already declares every org-air keyword returns UNCHANGED (no empty
supplement appended); a keyword in both supplement halves dedups to the
:not-done side.  Reverting the merge FAILS."
  (skip-unless (locate-library "org-air"))
  ;; (a) multiple sequences + (type …): dedup consults ALL base sequences.
  (let* ((user '((sequence "TODO(t)" "|" "DONE(d)")
                 (type "FRED(f)" "SARA" "|" "VERIFIED")
                 (sequence "WAIT" "CANCELLED")))
         (merged (let ((org-todo-keywords user))
                   (org-air-query--scan-todo-keywords)))
         (supp (car (last merged)))
         (names (org-air-r57--bare-names (cdr supp))))
    (should (equal (butlast merged) user))
    (should (eq (car supp) 'sequence))
    (dolist (n '("TODO" "DONE" "FRED" "SARA" "VERIFIED" "WAIT" "CANCELLED"))
      (should-not (member n names)))
    ;; undeclared org-air keywords still supplement.
    (dolist (n '("NEXT" "READY" "CANCELED"))
      (should (member n names))))
  ;; (b) legacy flat list: ONE sequence under `org-todo-interpretation'.
  (let ((merged (let ((org-todo-keywords '("TODO" "VERIFY" "DONE")))
                  (org-air-query--scan-todo-keywords))))
    (should (= (length merged) 2))
    (should (equal (car merged)
                   (cons org-todo-interpretation '("TODO" "VERIFY" "DONE")))))
  ;; (c) nil global: Org's own default base + the full supplement.
  (let ((merged (let ((org-todo-keywords nil))
                  (org-air-query--scan-todo-keywords))))
    (should (equal (car merged) '(sequence "TODO" "DONE")))
    (should (member "READY" (cadr merged)))
    (should (member "DROP" (cadr merged))))
  ;; (d) malformed (non-list) globals: same degradation, NO signal.
  (dolist (bad '("OOPS" 42 :nonsense))
    (let ((merged (let ((org-todo-keywords bad))
                    (org-air-query--scan-todo-keywords))))
      (should (equal (car merged) '(sequence "TODO" "DONE")))))
  ;; (e) fully-declared global: returned unchanged, no empty supplement.
  (let ((user '((sequence "TODO" "NEXT" "STARTED" "READY" "WIP"
                          "WAIT" "WAITING" "HOLD" "BLOCKED"
                          "|" "DONE" "COMP" "CANCELLED" "CANCELED"
                          "KILL" "DROP"))))
    (should (equal (let ((org-todo-keywords user))
                     (org-air-query--scan-todo-keywords))
                   user)))
  ;; (f) cross-plist dedup: a doubled keyword supplements ONCE, not-done.
  (let ((org-air-todo-keywords '(:not-done ("FOO") :done ("FOO" "BAR"))))
    (should (equal (let ((org-todo-keywords nil))
                     (org-air-query--scan-todo-keywords))
                   '((sequence "TODO" "DONE")
                     (sequence "FOO" "|" "BAR"))))))

;;;; -------------------------------------------------------------------
;;;; T7 — a file's own `#+TODO:' declaration still wins
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r57-6-file-own-declaration-wins ()
  "T7: a `#+TODO: OPEN | SHUT' file under the user's global parses with
ITS keywords — `* OPEN A' todo \"OPEN\", `* SHUT C' donep t — and
NEITHER org-air's vocabulary (`* NEXT B') NOR the user's global
\(`* CLOSED E') is injected: both stay title-swallowed, exactly Org's
in-buffer-keywords-REPLACE-the-default semantics.  Fences the merge to
the let-bound DEFAULT — an implementation appending to in-buffer
keywords FAILS here.  A keyword-less sibling file in the same corpus
\(`* CLOSED Gone') proves the merged default still reaches undeclared
files, so the whole ERT is revert-RED, not only a fence."
  (skip-unless (locate-library "org-air"))
  (org-air-r53--with-corpus
      '(("declared.org" . "#+TODO: OPEN | SHUT\n\n* OPEN A\n* NEXT B\n* SHUT C\n* CLOSED E\n")
        ("bare.org" . "* CLOSED Gone\n")
        ("inbox.org" . "#+title: inbox\n"))
    ;; the DECLARED file: its own vocabulary, nothing merged in.
    (let ((items (org-air-r57--scan-under-user-kws "declared.org")))
      (should (= (length items) 4))
      (let ((a (nth 0 items)) (b (nth 1 items))
            (c (nth 2 items)) (e (nth 3 items)))
        (should (equal (org-air-item-todo a) "OPEN"))
        (should (equal (org-air-item-title a) "A"))
        (should-not (org-air-item-donep a))
        ;; org-air's vocabulary NOT injected into declared files…
        (should-not (org-air-item-todo b))
        (should (equal (org-air-item-title b) "NEXT B"))
        (should (equal (org-air-item-todo c) "SHUT"))
        (should (org-air-item-donep c))
        ;; …and neither is the user's global (in-buffer REPLACES).
        (should-not (org-air-item-todo e))
        (should (equal (org-air-item-title e) "CLOSED E"))
        (should-not (org-air-item-donep e))))
    ;; the KEYWORD-LESS sibling: the merged default applies (revert-red).
    (let* ((items (org-air-r57--scan-under-user-kws "bare.org"))
           (gone (org-air-test-find-item "Gone" items)))
      (should gone)
      (should (equal (org-air-item-todo gone) "CLOSED"))
      (should (equal (org-air-item-title gone) "Gone"))
      (should (org-air-item-donep gone)))))

;;;; -------------------------------------------------------------------
;;;; T8 — the cache key carries the vocabulary
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r57-7-cache-key-carries-vocabulary ()
  "T8: `todo'/`title'/`donep' are cache slots parsed UNDER a vocabulary,
so the persistent cache's `:key' must carry the merged vocabulary: a
cache written under vocabulary A hydrates under A but NOT under
vocabulary B (`org-air-view--cache-read' => nil on the `:key' mismatch),
and a crafted pre-R57 2-element `:key' cache also misses (the documented
one-time cold path that flushes the wrong parses the user is staring
at).  Reverting the `org-air-view--cache-key' extension FAILS: the
stale/foreign-vocabulary caches hydrate again."
  (skip-unless (locate-library "org-air"))
  (org-air-r53--with-corpus
      '(("tasks.org" . "* TODO Alpha\n")
        ("inbox.org" . "#+title: inbox\n"))
    ;; write under vocabulary A (the user's)…
    (let ((org-todo-keywords org-air-r57--user-kws))
      (org-air-view--cache-write nil nil)
      (should (file-exists-p (expand-file-name org-air-cache-file)))
      ;; …and the SAME vocabulary hydrates (the miss below is not vacuous).
      (should (org-air-view--cache-read)))
    ;; a DIFFERENT vocabulary must miss.
    (let ((org-todo-keywords '((sequence "TODO" "|" "DONE"))))
      (should-not (org-air-view--cache-read)))
    ;; pre-R57 2-element :key data misses under any vocabulary.
    (let ((print-length nil) (print-level nil))
      (write-region
       (prin1-to-string
        (list :version org-air-view--cache-version
              :key (list org-air-files org-air-inbox-file)
              :mtimes nil :file-meta nil :visits nil :items nil))
       nil (expand-file-name org-air-cache-file) nil 'silent))
    (let ((org-todo-keywords org-air-r57--user-kws))
      (should-not (org-air-view--cache-read)))))

;;;; -------------------------------------------------------------------
;;;; T9 — classify's fallback done set: merged, not hard-wired
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r57-8-classify-fallback-merged-done-set ()
  "T9: `org-air-query-merged-done-keywords' derives the BARE done names
from the merged vocabulary exactly as Org does per sequence (after the
\"|\"; keys stripped), and an item built OUTSIDE the scan (no live
marker) with todo \"CLOSED\" is `org-air-classify--done-p' via that
fallback — off the board.  Reverting to the pre-R57 hard-wired
\(\"DONE\") FAILS: CLOSED not-done, `attention' bucket."
  (skip-unless (locate-library "org-air"))
  (with-temp-buffer                      ; no org buffer-locals in sight
    (let ((org-todo-keywords org-air-r57--user-kws)
          (org-done-keywords nil))       ; the defvar default: nil floor
      ;; the merged done set: user's done keywords + org-air's, bare names.
      (let ((done (org-air-query-merged-done-keywords)))
        (dolist (kw '("DONE" "DROPPED" "CLOSED" "COMP" "CANCELLED"
                      "CANCELED" "KILL" "DROP"))
          (should (member kw done)))
        (dolist (kw '("TODO" "HOLD" "READY" "WIP" "|" "DONE(d!)" "DROPPED(x@)"))
          (should-not (member kw done))))
      ;; the fallback in action: a live-capture-style item, no marker.
      (let ((closed (org-air-item-create :title "Foo" :todo "CLOSED"))
            (held (org-air-item-create :title "Baz" :todo "HOLD")))
        (should (org-air-classify--done-p closed))
        (should-not (org-air-r57--task-buckets-of closed))
        ;; a not-done keyword must NOT ride the fallback into done.
        (should-not (org-air-classify--done-p held))))))

;;;; -------------------------------------------------------------------
;;;; T10 — donep-aware keyword face fallback (+ R57-2 face table)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r57-9-donep-aware-todo-face ()
  "T10: a keyword absent from `org-air-todo-keyword-faces' falls back
donep-aware — `org-air-face-done' when DONEP, else `org-air-face-todo'
\(the user's CLOSED never wears an active-looking badge where done items
render).  The R57-2 table maps READY/WIP to `org-air-face-todo-next'.

R79 RE-BLESS (test seat; impl `orrtuvtlqsvl'): the DONE family SPLITS —
the cancelled/abandoned set now carries the new `org-air-face-dropped'
\(terracotta) instead of collapsing onto `org-air-face-done' (faded
blue).  So the two pre-R79 assertions that hard-wired the collapse move:
DROPPED (donep) and DROP now resolve to `org-air-face-dropped', while
COMP/DONE stay `org-air-face-done' and the donep-aware fallback for a
keyword OUTSIDE both the alist and the merged scan vocabulary (CLOSED)
is unchanged.  Reverting the DONEP thread, the R57-2 face-table
additions, or the R79 done-family split FAILS."
  (skip-unless (locate-library "org-air"))
  ;; the donep-aware fallback for keywords neither the table NOR the
  ;; merged scan vocabulary knows (CLOSED is not in either at defaults).
  (should (eq (org-air-view--todo-face "CLOSED" t) 'org-air-face-done))
  (should (eq (org-air-view--todo-face "CLOSED" nil) 'org-air-face-todo))
  (should (eq (org-air-view--todo-face "CLOSED") 'org-air-face-todo))
  ;; R79: the cancelled/abandoned family now reads `org-air-face-dropped'
  ;; (was `org-air-face-done' pre-R79 — the collapse this round cures).
  (should (eq (org-air-view--todo-face "DROPPED" t) 'org-air-face-dropped))
  (should (eq (org-air-view--todo-face "DROP") 'org-air-face-dropped))
  ;; the R57-2 face-table entries (unchanged by R79).
  (should (eq (org-air-view--todo-face "READY") 'org-air-face-todo-next))
  (should (eq (org-air-view--todo-face "WIP") 'org-air-face-todo-next))
  ;; the completion family stays `org-air-face-done' — distinct from
  ;; DROPPED/DROP above (that distinction is the R79 point).
  (should (eq (org-air-view--todo-face "COMP") 'org-air-face-done))
  (should (eq (org-air-view--todo-face "DONE") 'org-air-face-done))
  ;; a KNOWN keyword keeps its table face regardless of DONEP.
  (should (eq (org-air-view--todo-face "TODO" t) 'org-air-face-todo)))

;;;; -------------------------------------------------------------------
;;;; T11 — audit fix #5: archived trees are history, not board material
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r57-10-archived-trees-off-board ()
  "T11: a heading tagged `org-archive-tag' — and a CHILD under it, whose
scanned tags carry the inherited ARCHIVE tag under default Org tag
inheritance — classifies into ZERO task buckets, while an unarchived
control sibling with the same years-old SCHEDULED stays on the board
\(the same flood class as T1, mirrored on
`org-agenda-skip-archived-trees''s default).  Reverting the
`org-air-classify--heading-buckets' exclusion FAILS."
  (skip-unless (locate-library "org-air"))
  (org-air-r53--with-corpus
      '(("arch.org" . "* TODO Old archived parent :ARCHIVE:\nSCHEDULED: <2023-01-01 Sun>\n** TODO Archived child task\n* TODO Live control task\nSCHEDULED: <2023-01-01 Sun>\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((items (org-air-query--scan-file
                   (expand-file-name "arch.org" org-air-r53--dir)))
           (parent (org-air-test-find-item "archived parent" items))
           (child (org-air-test-find-item "Archived child" items))
           (control (org-air-test-find-item "Live control" items)))
      (should parent)
      (should (member org-archive-tag (org-air-item-tags parent)))
      (should-not (org-air-r57--task-buckets-of parent))
      ;; the child never wrote :ARCHIVE: itself — inheritance carries it.
      (should child)
      (should (member org-archive-tag (org-air-item-tags child)))
      (should-not (org-air-r57--task-buckets-of child))
      ;; the unarchived control stays board material (anti-tautology).
      (should control)
      (should (memq 'attention (org-air-r57--task-buckets-of control))))))

(provide 'org-air-round57-test)
;;; org-air-round57-test.el ends here
