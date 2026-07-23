;;; org-air-round79-test.el --- executing + audit ERTs for round-79 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance + audit ERTs for round-79 (air/v0.1/org-air-round79-design.org):
;; the single-day view (`org-air-view-day', the R55-1 owner-routed pane) is
;; repaired on four seams, each REUSING a board primitive at the right
;; setting rather than a new subsystem:
;;
;;   D1  distinct keyword-badge FACES.  `org-air-view--todo-face' becomes a
;;       resolver: the DONE family splits (completions DONE/COMP/COMPLETED
;;       keep `org-air-face-done'; the cancelled/abandoned set DROPPED/DROP/
;;       CANCELLED/CANCELED/KILL/KILLED reads the new `org-air-face-dropped')
;;       and an unknown keyword resolves through the R57 merged scan
;;       vocabulary (`org-air-view--merged-vocab-face') by POSITION, not the
;;       blanket donep fallback; `org-air-keyword-face-source' opt-in reads
;;       the user's own `org-todo-keyword-faces'.  So COMP/DROPPED/TODO/
;;       READY/WIP each read a DISTINCT face.  (Exact SVG/hex is GUI-confirm-
;;       only; the ERT asserts the FACE, never the pixel.)
;;   D2  fixed badge column ALIGNMENT.  `org-air-view--insert-day-pane' sizes
;;       the badge/tags/origin columns to the day's widest SHOWN value
;;       (`org-air-view--day-meta-widths', the board rule) instead of the
;;       stale R15 per-row nil widths, so a mixed COMP(4)/DROPPED(7) day
;;       group starts every TITLE at ONE fixed offset.
;;   D3  keyword FILTER axis.  The shared `/' grammar gains `todo:KEYWORD'
;;       and `is:done' (keyword-identity, NO board-active gate) so the day
;;       pane's DONE rows can be selected; conjoins with #tag / is: / due:;
;;       offered in completion; an unknown todo:ZZZ parses and falls through.
;;   D4  o/O day SORT.  Each day group flows through the shared R22-3 sort
;;       core (`org-air-view--sort-day-items') with a day key vocabulary
;;       (time keyword priority title), default `time' (chronological within
;;       the group); the day<->board boundary swaps the key list + coerces
;;       the active key; the banner indicator is view-aware.
;;
;; The spec's ten seams E1..E10 map onto r79-1..r79-10 below.  Cross-seam
;; invariants re-asserted: R53 (no rescan on day render/sort/filter), R55-1
;; (owner routing, locked in round55-test), R57-1 (own palette DEFAULT,
;; board goldens byte-identical), R72 (is:/due: agreement kept; the keyword
;; axis is orthogonal), R77 (day pane stays planning-slot keyed \u2014 a demoted
;; routine still shows), R22-3 (one sort core, no fork).
;;
;; HONEST AUDIT DIVERGENCE from the spec sketch (E6 label leg): the spec
;; prose claimed a typo `todo:zzz' would render QUOTED.  The shipped
;; grammar `todo:\\(.+\\)' parses ANY name (an OPEN keyword vocabulary), so
;; `todo:zzz' PARSES to `(todo . \"zzz\")' and therefore renders UNQUOTED
;; (like `todo:DROPPED') \u2014 it simply matches no items and falls through
;; cleanly.  The quoting-is-the-tell rule still holds for the CLOSED is:
;; axis (a near-miss `is:zzz' does not parse \u2192 quoted).  r79-6 asserts the
;; TRUE behaviour, not the sketch.
;;
;; RE-BLESS (this seat): `org-air-r57-9-donep-aware-todo-face' (round57-
;; test) updated to the R79 done-family split; the impl's known-failures
;; entry for it DELETED (manifest EMPTY).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'org-air-test-helpers)

(when (locate-library "org-air")
  (require 'org-air)
  (require 'org-air-view))

;;;; -------------------------------------------------------------------
;;;; Corpus scaffolding (self-contained; mirrors the r53/r77 pattern)
;;;; -------------------------------------------------------------------

(defvar org-air-r79--dir nil
  "The temp corpus directory of the current `org-air-r79--with-corpus'.")

(defconst org-air-r79--width 96
  "The fixed render width for the day-pane byte assertions.")

(defconst org-air-r79--day
  (encode-time '(0 0 12 23 7 2026 nil -1 nil))
  "The fixed focused day <2026-07-23 Thu> the fixture is built around.")

(defconst org-air-r79--fixture
  "#+TITLE: r79 day fixture
#+TODO: TODO WIP READY DELEGATED | COMP DROPPED ARCHIVED

* TODO Alpha deadline task
DEADLINE: <2026-07-23 Thu 09:00>
* WIP Beta scheduled work :work:
SCHEDULED: <2026-07-23 Thu 14:00>
* Gamma routine sched
SCHEDULED: <2026-07-23 Thu 08:00>
* COMP Zulu comp late
Body ts [2026-07-23 Thu 15:00]
* DROPPED Alpha dropped early :work:
Body ts [2026-07-23 Thu 11:00]
* COMP [#A] Mike comp early
Body ts [2026-07-23 Thu 07:00]
* DROPPED Bravo dropped late
Body ts [2026-07-23 Thu 16:00]
"
  "A single day <2026-07-23 Thu>: a TODO Deadline, a WIP + a keyword-less
routine under Scheduled, and a Logged/created group of two COMP and two
DROPPED headings whose FILE order (Zulu Alpha-dropped Mike Bravo)
deliberately DIFFERS from every sort order, so a reorder is never a
tautology.  The `#+TODO:' line names DELEGATED (not-done) and ARCHIVED
(done) for the merged-vocabulary leg.")

(defmacro org-air-r79--with-corpus (&rest body)
  "Scan the R79 fixture into a temp corpus and run BODY with the item list.
Binds `org-air-r79--items' to the scanned items and `org-air-r79--day'
to the focused day.  Cleans up the corpus buffers + directory."
  (declare (indent 0) (debug t))
  `(let ((org-air-r79--dir (make-temp-file "org-air-r79-" t)))
     (unwind-protect
         (progn
           (pcase-dolist (`(,name . ,content)
                          `(("inbox.org" . "#+title: inbox\n")
                            ("day.org" . ,org-air-r79--fixture)))
             (let ((file-name-handler-alist nil)
                   (coding-system-for-write 'utf-8-unix))
               (write-region content nil
                             (expand-file-name name org-air-r79--dir)
                             nil 'silent)))
           (let* ((org-air-files (list org-air-r79--dir))
                  (org-air-inbox-file
                   (expand-file-name "inbox.org" org-air-r79--dir))
                  (org-air-cache-file
                   (expand-file-name ".cache/board.eld" org-air-r79--dir))
                  (org-air-view-width org-air-r79--width)
                  (org-air-view-height 50)
                  (org-air-r79--items (org-air-query-items)))
             ,@body))
       (org-air-query-teardown)
       (let ((kill-buffer-query-functions nil))
         (dolist (buf (buffer-list))
           (let ((fn (buffer-file-name buf)))
             (when (and fn (string-prefix-p org-air-r79--dir fn))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r79--dir t))))

(defmacro org-air-r79--with-corpus-knob (knob &rest body)
  "Like `org-air-r79--with-corpus' but with `org-air-task-requires-todo'
bound to KNOB around the SCAN, so ntype demotion (R77) is in effect."
  (declare (indent 1) (debug t))
  `(let ((org-air-task-requires-todo ,knob))
     (org-air-r79--with-corpus ,@body)))

(defun org-air-r79--item (title items)
  "Return the item in ITEMS whose title contains TITLE."
  (cl-find-if (lambda (it) (string-match-p (regexp-quote title)
                                           (or (org-air-item-title it) "")))
              items))

(defun org-air-r79--groups (items)
  "Return the day groups of ITEMS for the fixed day (view locals unset)."
  (let ((org-air-view--scope nil)
        (org-air-view--render-partition nil))
    (org-air-view--day-groups items org-air-r79--day)))

(defun org-air-r79--render (items &optional filter key direction now)
  "Render the day pane of ITEMS to a propertized string (R79 harness).
FILTER seeds `org-air-view--tag-filter'; KEY/DIRECTION seed the active
day sort AFTER `org-air-view--enter-day-sort' coerces; NOW seeds
`org-air-view--filter-now' for the date/status axis.  Renders at the
fixed width via the real `org-air-view--insert-day-pane'."
  (with-temp-buffer
    (let ((org-air-view--day org-air-r79--day)
          (org-air-view--scope nil)
          (org-air-view--render-partition nil)
          (org-air-view--tag-filter filter)
          (org-air-view--filter-now now)
          (org-air-view--items items))
      (org-air-view--enter-day-sort)
      (when key (setq org-air-view--sort-key key))
      (when direction (setq org-air-view--sort-direction direction))
      (org-air-view--insert-day-pane items org-air-r79--width)
      (buffer-string))))

(defun org-air-r79--title-col (render title)
  "Return the 0-based column where TITLE begins in RENDER, or nil."
  (cl-loop for ln in (split-string (substring-no-properties render) "\n")
           for pos = (string-match (regexp-quote title) ln)
           when pos return pos))

(defun org-air-r79--badge-face (render keyword)
  "Return the `face' text property on the first KEYWORD badge cell in RENDER.
The keyword badge cell carries a `face' property = the resolved
`org-air-view--todo-face'; titles never contain an upcased keyword, so
the first occurrence is the cell."
  (let ((p (string-search keyword render)))
    (and p (get-text-property p 'face render))))

(defun org-air-r79--logged-order (render)
  "Return the ordered Logged-group items in RENDER as short symbols."
  (cl-loop for ln in (split-string (substring-no-properties render) "\n")
           for m = (cond ((string-match-p "Mike comp early" ln) 'mike)
                         ((string-match-p "Zulu comp late" ln) 'zulu)
                         ((string-match-p "Alpha dropped early" ln) 'alpha)
                         ((string-match-p "Bravo dropped late" ln) 'bravo))
           when m collect m))

(defun org-air-r79--present-p (render title)
  "Non-nil when a row for TITLE is rendered in RENDER."
  (string-match-p (regexp-quote title) (substring-no-properties render)))

;;;; -------------------------------------------------------------------
;;;; E1 / r79-1 \u2014 D1: a COMP badge and a DROPPED badge carry DIFFERENT faces
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r79-1-comp-vs-dropped-distinct-face ()
  "D1: distinct keyword-badge FACES, not one uniform blue.
`org-air-view--todo-face' resolves COMP and DROPPED to DIFFERENT,
non-nil faces \u2014 DROPPED to the new `org-air-face-dropped', COMP to
`org-air-face-done' \u2014 and the active family stays distinct too
(TODO/READY differ from each other and from the done pair).  Rendered:
the COMP badge cell and the DROPPED badge cell in the day pane's
Logged/created group carry different `face' properties.  Reverting the
DONE-family split (collapsing DROPPED onto `org-air-face-done') FAILS.
The exact SVG colour is GUI-confirm-only; this asserts the FACE."
  (skip-unless (locate-library "org-air"))
  ;; anti-vacuity: BOTH faces resolve to something non-nil first.
  (let ((comp (org-air-view--todo-face "COMP" t))
        (dropped (org-air-view--todo-face "DROPPED" t)))
    (should comp)
    (should dropped)
    (should-not (eq comp dropped))
    (should (eq comp 'org-air-face-done))
    (should (eq dropped 'org-air-face-dropped)))
  ;; TODO / READY / WIP are distinct active faces (not the done pair).
  (should (eq (org-air-view--todo-face "TODO") 'org-air-face-todo))
  (should (eq (org-air-view--todo-face "READY") 'org-air-face-todo-next))
  (should-not (eq (org-air-view--todo-face "TODO")
                  (org-air-view--todo-face "READY")))
  (should-not (eq (org-air-view--todo-face "TODO")
                  (org-air-view--todo-face "COMP")))
  ;; rendered: the badge cells differ ON THE DAY PANE.
  (org-air-r79--with-corpus
    (let* ((render (org-air-r79--render org-air-r79--items))
           (comp-face (org-air-r79--badge-face render "COMP"))
           (dropped-face (org-air-r79--badge-face render "DROPPED")))
      (should comp-face)
      (should dropped-face)
      (should-not (eq comp-face dropped-face))
      (should (eq dropped-face 'org-air-face-dropped)))))

;;;; -------------------------------------------------------------------
;;;; E2 / r79-2 \u2014 D2: titles align to ONE fixed badge column
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r79-2-titles-align-fixed-badge-column ()
  "D2: across a mixed COMP(4)/DROPPED(7)/WIP(3) day the TITLE column is
FIXED.  `org-air-view--day-meta-widths' pads the badge cell to the day's
widest SHOWN keyword (DROPPED => 7), so a COMP row, a DROPPED row and a
WIP row all start their title at the SAME offset.  Reverting to the
stale per-row nil widths mis-aligns them (COMP starts 3 cols left of
DROPPED) \u2014 FAILS."
  (skip-unless (locate-library "org-air"))
  (org-air-r79--with-corpus
    ;; the width computation itself: widest keyword is DROPPED = 7.
    (let ((dw (org-air-view--day-meta-widths
               (org-air-r79--groups org-air-r79--items) org-air-r79--width)))
      (should (= (nth 0 dw) 7)))
    (let* ((render (org-air-r79--render org-air-r79--items))
           (comp (org-air-r79--title-col render "Zulu comp late"))
           (dropped (org-air-r79--title-col render "Alpha dropped early"))
           (wip (org-air-r79--title-col render "Beta scheduled work")))
      (should comp)
      (should dropped)
      (should wip)
      ;; the crux: one fixed left edge across mixed-width keywords.
      (should (= comp dropped))
      (should (= comp wip))
      ;; and the priority row (#A square) lands on the same edge too.
      (should (= comp (org-air-r79--title-col render "Mike comp early"))))))

;;;; -------------------------------------------------------------------
;;;; E3 / r79-3 \u2014 D1: unknown keywords via the R57 merged scan vocabulary
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r79-3-merged-vocab-fallback ()
  "D1: an unknown keyword resolves through the R57 merged scan vocabulary
by POSITION, not the blanket donep fallback.  With DELEGATED (not-done)
and ARCHIVED (done) declared in the scanned `#+TODO:', DELEGATED reads
the ACTIVE family EVEN when the caller passes DONEP=t, and ARCHIVED
reads the DONE family EVEN when DONEP=nil \u2014 the vocabulary position wins.
Reverting to `(if donep done todo)' flips both (DELEGATED=>done,
ARCHIVED=>todo) and FAILS."
  (skip-unless (locate-library "org-air"))
  (let ((org-todo-keywords
         '((sequence "TODO" "WIP" "READY" "DELEGATED" "|"
                     "DONE" "COMP" "DROPPED" "ARCHIVED"))))
    ;; the vocabulary really carries both bare names.
    (should (member "DELEGATED" (org-air-view--scan-keyword-names)))
    (should (member "ARCHIVED" (org-air-view--scan-keyword-names)))
    ;; position beats the donep flag both ways.
    (should (eq (org-air-view--todo-face "DELEGATED" t) 'org-air-face-todo))
    (should (eq (org-air-view--todo-face "ARCHIVED" nil) 'org-air-face-done))
    ;; a name in NEITHER the alist NOR the vocabulary falls back on donep.
    (should (eq (org-air-view--todo-face "ZZZUNKNOWN" t) 'org-air-face-done))
    (should (eq (org-air-view--todo-face "ZZZUNKNOWN" nil) 'org-air-face-todo))))

;;;; -------------------------------------------------------------------
;;;; E4 / r79-4 \u2014 D1: `org-air-keyword-face-source' opt-in
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r79-4-org-face-source-optin ()
  "D1: `org-air-keyword-face-source' honours the user's own faces on opt-in.
With a user `org-todo-keyword-faces' naming COMP, the DEFAULT `own'
source keeps org-air's palette (`org-air-face-done'); flipping to `org'
reads the USER's face verbatim, wraps a bare colour string to a
`(:foreground ...)' plist, and falls back to the `own' mapping wherever
the user named none.  Reverting the resolver's `org' arm (always `own')
FAILS the `org' legs."
  (skip-unless (locate-library "org-air"))
  (let ((org-todo-keyword-faces
         (list (cons "COMP" 'font-lock-warning-face)
               (cons "TODO" "#123456"))))
    ;; DEFAULT own: no import \u2014 org-air's own palette (proves R57-1 holds).
    (let ((org-air-keyword-face-source 'own))
      (should (eq (org-air-view--todo-face "COMP" t) 'org-air-face-done)))
    ;; opt-in org: the user's face symbol, a wrapped colour string, and a
    ;; fall-through to `own' where the user named nothing.
    (let ((org-air-keyword-face-source 'org))
      (should (eq (org-air-view--todo-face "COMP" t) 'font-lock-warning-face))
      (should (equal (org-air-view--todo-face "TODO" nil) '(:foreground "#123456")))
      (should (eq (org-air-view--todo-face "WIP" nil) 'org-air-face-todo-next)))))

;;;; -------------------------------------------------------------------
;;;; E5 / r79-5 \u2014 D3: the day pane ALREADY honours tag/text/is:/due:
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r79-5-day-filter-tag-text-is-due ()
  "D3 (extend, not add): the pre-existing filter axes already narrow the
day pane through `org-air-view--visible-items'.  A #tag token, a bare
text token and an R72 `is:overdue' token each narrow the rendered day
groups \u2014 and `is:overdue' (which conjoins `board-active-p') DROPS the
DONE COMP/DROPPED rows, the exact contrast the r79-6 keyword axis is
added to overcome.  Asserted here so r79-6 is a genuine ADD."
  (skip-unless (locate-library "org-air"))
  (org-air-r79--with-corpus
    (let ((items org-air-r79--items)
          (fnow (encode-time '(0 0 12 24 7 2026 nil -1 nil))))
      ;; #work: only the two work-tagged rows.
      (let ((r (org-air-r79--render items '("#work"))))
        (should (org-air-r79--present-p r "Beta scheduled work"))
        (should (org-air-r79--present-p r "Alpha dropped early"))
        (should-not (org-air-r79--present-p r "Zulu comp late"))
        (should-not (org-air-r79--present-p r "Alpha deadline task")))
      ;; a bare text token: only the matching title.
      (let ((r (org-air-r79--render items '("beta"))))
        (should (org-air-r79--present-p r "Beta scheduled work"))
        (should-not (org-air-r79--present-p r "Zulu comp late")))
      ;; is:overdue (R72 date/status axis) narrows AND drops the done rows.
      (let ((r (org-air-r79--render items '("is:overdue") nil nil fnow)))
        (should (org-air-r79--present-p r "Alpha deadline task"))
        (should-not (org-air-r79--present-p r "Zulu comp late"))
        (should-not (org-air-r79--present-p r "Alpha dropped early"))))))

;;;; -------------------------------------------------------------------
;;;; E6 / r79-6 \u2014 D3: the keyword-identity axis (todo:KEYWORD / is:done)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r79-6-day-filter-keyword-token ()
  "D3: the keyword axis selects DONE rows with NO board-active gate.
`org-air-view--filter-token-parse' yields `(todo . \"DROPPED\")' /
`(status . done)'; `todo:DROPPED' shows ONLY the DROPPED rows in the day
pane (COMP gone \u2014 a DONE item is selected, impossible for any R72
token); `is:done' shows the whole done-set and drops the active
TODO/WIP; the SAME `todo:DROPPED' narrows via the shared
`org-air-view--passes-filter-p' (board consistency); the axis is offered
in `/' completion; an unknown `todo:ZZZ' parses and matches nothing
(falls through cleanly).  Reverting the keyword axis (or gating it on
board-active) FAILS.

Audit note: the label leg asserts the TRUE behaviour \u2014 `todo:zzz' PARSES
(open vocabulary) so it renders UNQUOTED; the quoting-tell survives on
the CLOSED is: axis (`is:zzz' does not parse => quoted)."
  (skip-unless (locate-library "org-air"))
  ;; grammar.
  (should (equal (org-air-view--filter-token-parse "todo:DROPPED")
                 '(todo . "DROPPED")))
  (should (equal (org-air-view--filter-token-parse "is:done")
                 '(status . done)))
  (should (equal (org-air-view--filter-token-parse "todo:ZZZ")
                 '(todo . "ZZZ")))
  ;; completion vocabulary teaches the axis.
  (let ((org-todo-keywords
         '((sequence "TODO" "WIP" "|" "DONE" "COMP" "DROPPED"))))
    (let ((vocab (org-air-view--filter-vocabulary)))
      (should (member "is:done" vocab))
      (should (member "todo:COMP" vocab))
      (should (member "todo:DROPPED" vocab))))
  ;; label: parsed tokens render unquoted; a non-parsing near-miss quotes.
  (should (equal (org-air-view--filter-token-label "todo:DROPPED") "todo:DROPPED"))
  (should (equal (org-air-view--filter-token-label "todo:zzz") "todo:zzz"))
  (should (equal (org-air-view--filter-token-label "is:zzz") "\"is:zzz\""))
  (should (equal (org-air-view--filter-token-label "git") "\"git\""))
  (org-air-r79--with-corpus
    (let ((items org-air-r79--items))
      ;; todo:DROPPED \u2014 ONLY the dropped rows (done items selected!).
      (let ((r (org-air-r79--render items '("todo:DROPPED"))))
        (should (org-air-r79--present-p r "Alpha dropped early"))
        (should (org-air-r79--present-p r "Bravo dropped late"))
        (should-not (org-air-r79--present-p r "Zulu comp late"))
        (should-not (org-air-r79--present-p r "Mike comp early"))
        (should-not (org-air-r79--present-p r "Beta scheduled work")))
      ;; is:done \u2014 the whole done set; active rows drop.
      (let ((r (org-air-r79--render items '("is:done"))))
        (should (org-air-r79--present-p r "Zulu comp late"))
        (should (org-air-r79--present-p r "Alpha dropped early"))
        (should-not (org-air-r79--present-p r "Beta scheduled work"))
        (should-not (org-air-r79--present-p r "Alpha deadline task")))
      ;; unknown todo:ZZZ parses, matches nothing \u2014 empty, no crash.
      (let ((r (org-air-r79--render items '("todo:ZZZ"))))
        (should-not (org-air-r79--present-p r "Zulu comp late"))
        (should-not (org-air-r79--present-p r "Alpha dropped early")))
      ;; board consistency: the same axis over the shared predicate, NO
      ;; board-active gate (a DONE dropped item passes; comp/active fail).
      (let ((org-air-view--tag-filter '("todo:DROPPED")))
        (should (org-air-view--passes-filter-p
                 (org-air-r79--item "Alpha dropped early" items)))
        (should-not (org-air-view--passes-filter-p
                     (org-air-r79--item "Zulu comp late" items)))
        (should-not (org-air-view--passes-filter-p
                     (org-air-r79--item "Beta scheduled work" items)))))))

;;;; -------------------------------------------------------------------
;;;; E7 / r79-7 \u2014 D4: o/O re-sort the day items via the shared core
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r79-7-day-sort-o-cycles-and-reorders ()
  "D4: each day sort key orders the Logged group correctly; default `time'.
The fixture's FILE order (zulu alpha mike bravo) is the `--day-groups'
scan baseline (asserted first \u2014 anti-tautology).  Rendered: default
`time' is chronological by body stamp (mike alpha zulu bravo); `keyword'
clusters all COMP before all DROPPED, secondary by time (mike zulu alpha
bravo); `priority' puts #A first then the equal-rank rest by title (mike
alpha bravo zulu); `time' DESC reverses (bravo zulu alpha mike).  Every
order is distinct from the scan baseline, so a dead sort (nreverse only)
FAILS."
  (skip-unless (locate-library "org-air"))
  (org-air-r79--with-corpus
    (let* ((items org-air-r79--items)
           (logged (cdr (assoc "Logged / created" (org-air-r79--groups items))))
           (scan (mapcar (lambda (it)
                           (cond ((string-match-p "Mike" (org-air-item-title it)) 'mike)
                                 ((string-match-p "Zulu" (org-air-item-title it)) 'zulu)
                                 ((string-match-p "dropped early" (org-air-item-title it)) 'alpha)
                                 ((string-match-p "Bravo" (org-air-item-title it)) 'bravo)))
                         logged)))
      ;; anti-tautology: the RAW group order is file order, not sorted.
      (should (equal scan '(zulu alpha mike bravo)))
      ;; default time (no key arg => enter-day-sort seeds `time').
      (should (equal (org-air-r79--logged-order (org-air-r79--render items))
                     '(mike alpha zulu bravo)))
      (should (equal (org-air-r79--logged-order (org-air-r79--render items nil 'keyword))
                     '(mike zulu alpha bravo)))
      (should (equal (org-air-r79--logged-order (org-air-r79--render items nil 'priority))
                     '(mike alpha bravo zulu)))
      (should (equal (org-air-r79--logged-order
                      (org-air-r79--render items nil 'time 'descending))
                     '(bravo zulu alpha mike))))))

;;;; -------------------------------------------------------------------
;;;; E8 / r79-8 \u2014 D4: view-aware sort keys, coercion + indicator
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r79-8-sort-indicator-and-keys-view-aware ()
  "D4: the sort key vocabulary + default + indicator are VIEW-AWARE.
Entering the day view swaps `org-air-view--sort-keys' to the day list
and coerces the active key (a shared `priority'/`title' carries; a board
`date' becomes `time'); leaving restores the board list and coerces back
(`time'/`keyword' hand to `date'; shared keys survive).
`org-air-view--sort-default-p' keys on `time' in the day view and `date'
on the board \u2014 so the REAL banner shows no sort segment at each view's
own default and the key name the moment it deviates.  Reverting the
view-aware default (fixed `date') mis-fires the indicator at the day
default \u2014 FAILS."
  (skip-unless (locate-library "org-air"))
  ;; view-aware default (the crux; pre-R79 only knew `date').
  (with-temp-buffer
    (setq-local org-air-view--sort-direction 'ascending)
    (setq-local org-air-view--day org-air-r79--day)
    (setq-local org-air-view--sort-key 'time)
    (should (org-air-view--sort-default-p))       ; day default = time
    (setq-local org-air-view--sort-key 'date)
    (should-not (org-air-view--sort-default-p))    ; date is NOT the day default
    (setq-local org-air-view--day nil)
    (setq-local org-air-view--sort-key 'time)
    (should-not (org-air-view--sort-default-p))    ; time is NOT the board default
    (setq-local org-air-view--sort-key 'date)
    (should (org-air-view--sort-default-p)))       ; board default = date
  ;; coercion at the boundary.
  (with-temp-buffer
    (setq-local org-air-view--sort-keys '(date priority title recency))
    (setq-local org-air-view--sort-key 'date)
    (setq-local org-air-view--day org-air-r79--day)
    (org-air-view--enter-day-sort)
    (should (equal org-air-view--sort-keys org-air-view--day-sort-keys))
    (should (eq org-air-view--sort-key 'time))     ; date -> time
    ;; shared priority carries both ways.
    (setq-local org-air-view--sort-key 'priority)
    (setq-local org-air-view--day nil)
    (org-air-view--leave-day-sort)
    (should (equal org-air-view--sort-keys '(date priority title recency)))
    (should (eq org-air-view--sort-key 'priority)) ; shared survives
    ;; a day-only key hands back to the board default.
    (setq-local org-air-view--day org-air-r79--day)
    (org-air-view--enter-day-sort)
    (setq-local org-air-view--sort-key 'keyword)
    (setq-local org-air-view--day nil)
    (org-air-view--leave-day-sort)
    (should (eq org-air-view--sort-key 'date)))    ; keyword -> date
  ;; the REAL banner segment appears/disappears through --sort-default-p.
  (cl-flet ((banner (key)
              (with-temp-buffer
                (let ((org-air-view--line-width org-air-r79--width)
                      (org-air-view--render-width org-air-r79--width)
                      (org-air-view--day org-air-r79--day)
                      (org-air-view--scope nil))
                  (org-air-view--enter-day-sort)
                  (when key (setq org-air-view--sort-key key))
                  (org-air-view--insert-banner nil)
                  (substring-no-properties (buffer-string))))))
    ;; day default (time): NO sort key name in the banner.
    (should-not (string-match-p "keyword" (banner nil)))
    (should-not (string-match-p "priority" (banner nil)))
    ;; after o -> keyword: the R22-3 indicator names the active key.
    (should (string-match-p "keyword" (banner 'keyword)))))

;;;; -------------------------------------------------------------------
;;;; E8-live / r79-8b \u2014 o on a LIVE board actually re-renders the day pane
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r79-8b-live-o-reorders-and-shows-indicator ()
  "D4 (live wiring): on a LIVE owner board focused on the day, `o'
\(`org-air-view-sort-cycle') advances the key over the DAY key list, the
shared refresh re-renders the day pane into the new order, and the
banner grows the R22-3 sort indicator.  Proves the o/O commands + refresh
+ banner are wired end-to-end, not just the pure core."
  (skip-unless (locate-library "org-air"))
  (org-air-r79--with-corpus
    (let ((buf (get-buffer-create org-air-view-buffer-name)))
      (unwind-protect
          (with-current-buffer buf
            (org-air-view-mode)
            (setq-local org-air-view--items org-air-r79--items)
            (setq-local org-air-view--day org-air-r79--day)
            (setq-local org-air-view--cal-month org-air-r79--day)
            (org-air-view--enter-day-sort)
            (org-air-view--render org-air-r79--items nil)
            ;; day default = time: chronological, no indicator.
            (should (eq org-air-view--sort-key 'time))
            (should (equal (org-air-r79--logged-order (buffer-string))
                           '(mike alpha zulu bravo)))
            (should-not (string-match-p "keyword"
                                        (substring-no-properties (buffer-string))))
            ;; press o -> next day key (keyword): re-rendered + indicator.
            (org-air-view-sort-cycle)
            (should (eq org-air-view--sort-key 'keyword))
            (should (memq org-air-view--sort-key org-air-view--day-sort-keys))
            (should (equal (org-air-r79--logged-order (buffer-string))
                           '(mike zulu alpha bravo)))
            (should (string-match-p "keyword"
                                    (substring-no-properties (buffer-string)))))
        (when (buffer-live-p buf)
          (let ((kill-buffer-query-functions nil)) (kill-buffer buf)))))))

;;;; -------------------------------------------------------------------
;;;; E9 / r79-9 \u2014 R77: a demoted routine still shows, aligned, in the day
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r79-9-r77-routine-still-shown ()
  "R77 carve-out survives R79: a keyword-less scheduled routine, DEMOTED
off the board task buckets under `org-air-task-requires-todo' t (ntype
knowledge), STILL appears under the day pane's Scheduled group (grouping
keys on planning SLOTS, not ntype) and \u2014 with a BLANK badge cell \u2014 pads
to the SAME fixed title column as the keyworded rows.  Reverting the
slot-keyed grouping (gating on ntype) or the fixed badge column FAILS."
  (skip-unless (locate-library "org-air"))
  (org-air-r79--with-corpus-knob t
    (let* ((items org-air-r79--items)
           (routine (org-air-r79--item "Gamma routine sched" items)))
      (should routine)
      ;; precondition: the knob demoted it off the task buckets.
      (should (eq (org-air-item-ntype routine) 'knowledge))
      (should-not (org-air-item-todo routine))
      ;; yet it is still filed under Scheduled on its day.
      (should (memq routine
                    (cdr (assoc "Scheduled" (org-air-r79--groups items)))))
      ;; and its keyword-less row aligns with the keyworded rows.
      (let* ((render (org-air-r79--render items))
             (routine-col (org-air-r79--title-col render "Gamma routine sched"))
             (comp-col (org-air-r79--title-col render "Zulu comp late")))
        (should routine-col)
        (should comp-col)
        (should (= routine-col comp-col))))))

;;;; -------------------------------------------------------------------
;;;; E10 / r79-10 \u2014 R53: no rescan on day render/sort/filter; defaults
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r79-10-no-rescan-and-defaults ()
  "R53 + R57-1 preserved: rendering the day pane \u2014 and re-rendering it
under every sort key and a keyword filter \u2014 issues ZERO
`org-air-query-items' calls (cached items only, no rescan).  And the
defaults hold: `org-air-keyword-face-source' is `own' (no user-face
import; the board goldens stay byte-identical, `make regen-mockups'
churn = 0), the day default key is `time'.  A rescan on filter/sort
FAILS the counter."
  (skip-unless (locate-library "org-air"))
  ;; defaults: R57-1 own palette, chronological day default.
  (should (eq (default-value 'org-air-keyword-face-source) 'own))
  (should (eq (car org-air-view--day-sort-keys) 'time))
  (org-air-r79--with-corpus
    (let* ((items org-air-r79--items)
           (n 0)
           (real (symbol-function 'org-air-query-items)))
      (cl-letf (((symbol-function 'org-air-query-items)
                 (lambda (&rest args) (cl-incf n) (apply real args))))
        (org-air-r79--render items)                       ; default time
        (dolist (k '(keyword priority title time))
          (org-air-r79--render items nil k))              ; every sort key
        (org-air-r79--render items '("todo:DROPPED"))     ; a keyword filter
        (org-air-r79--render items '("is:done")))          ; another
      (should (= n 0)))))

(provide 'org-air-round79-test)
;;; org-air-round79-test.el ends here
