;;; org-air-round86-test.el --- executing + audit ERTs for round-86 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance + audit ERTs for round-86 (air/v0.1/org-air-round86-design.org):
;; a LOCATION filter token `path:VALUE' joins the `/' mini-language — show
;; ONLY items whose SOURCE FILE path matches, composable with every existing
;; axis (#tag / is: / due: / todo: / is:backlog) under the SAME M-/ AND/OR
;; fold, on the board + day view + review.
;;
;; Two flagged decisions carry the design:
;;   (A) SEGMENT-aware — VALUE is a contiguous run of WHOLE path components,
;;       so `path:re' hits `…/tasks/re/…' but NOT a sibling `…/tasks/restore'
;;       (the one lens the bare-substring token structurally can't do, its
;;       origin text being the filename LEAF only).
;;   (B) org-root-RELATIVE — matched against the file made relative to its
;;       `org-air-files' source root's PARENT (machine `/home/…' prefix
;;       dropped, root basename kept — nicer AND safer than absolute;
;;       absolute fallback for an item under no source).
;;
;; The mechanism (spec D1-D5): one parse arm -> `(path . VALUE)'; four pure
;; helpers (`org-air-view--path-segments' / `--path-run-match-p' /
;; `--path-relative' / `--filter-path-token-match-p'); one top-level `pcase'
;; clause in `--filter-token-match-p' beside the R79 keyword axis (GATE-FREE
;; — a location is orthogonal to planning state); one `path:SEG' completion
;; offer per distinct directory segment.  Reads the cached
;; `org-air-item-file' slot + the `org-air-files' knob — NO file access, NO
;; scan slot, NO cache-version bump (R53, spy=0).
;;
;; The spec's sixteen seams r86-1..r86-16 map onto the ERTs below:
;;
;;   r86-1   (parse) the grammar admits `path:VALUE' -> (path . VALUE);
;;           case-fold; empty `path:' does not parse (bare-substring).
;;   r86-2   (core, segment-aware) `path:tasks/re' shows ONLY items under
;;           tasks/re — a sibling tasks/restore FAILS (segment boundary).
;;   r86-3   (segment boundary, single) `path:re' hits `re/' NOT
;;           `restore.org' NOR a `research/' dir (partial component fails).
;;   r86-4   (leaf is a whole segment) `path:foo.org' targets the file,
;;           `path:foo' does NOT (whole-or-nothing, no file: alias needed).
;;   r86-5   (relative, Decision B) machine prefix dropped, root basename
;;           kept; a segment ABOVE the root FAILS.
;;   r86-6   (absolute fallback) an item under NO source still matches by
;;           absolute segments — relative narrows noise, never loses a hit.
;;   r86-7   (case) `path:AIR' / `PATH:Air' match `air/'.
;;   r86-8   (unmatched => EMPTY, not all) a non-existent path yields the
;;           EMPTY visible set (defaults-true-on-no-match would fail).
;;   r86-9   (compose, AND) `path:tasks/re' + `#work' + `is:overdue' under
;;           `all' selects the one item that is all three.
;;   r86-10  (compose, OR) the SAME three tokens under `any' broaden; a
;;           path-only item appears via the `path:' disjunct.
;;   r86-11  (gate-free) a DONE / note-typed item under `tasks/re' STILL
;;           matches `path:tasks/re' (no board-active / task-routed gate).
;;   r86-12  (completion) `path:SEG' offered per distinct dir segment, no
;;           leaf filenames, SORTED (byte-stable).
;;   r86-13  (surfaces inherit) board + day + review all filter by `path:';
;;           project/revisit (ITEM=nil) treat it as vacuously false.
;;   r86-14  (NO rescan, spy=0) toggling `path:' never re-queries (R53).
;;   r86-15  (label) a `path:' token reads verbatim; the empty near-miss
;;           keeps its quotes (the quoting is the tell).
;;   r86-16  (`#' precedence) `#path:x' stays a TAG, never a location token.
;;
;; AUDIT ADDITIONS (test seat — revert-RED / gap-closing over the impl's
;; 16 seams; the impl's `.el' is UNTOUCHED):
;;
;;   r86-14  STRENGTHENED in place: the no-rescan spy also pins the
;;           narrowed SET to the SEGMENT-AWARE membership (Alpha/Beta/
;;           Epsilon under tasks/re, NOT the restore sibling) so it can no
;;           longer pass VACUOUSLY with a `path:' that matches nothing (the
;;           pre-R86 bare-substring reading narrows to the empty set —
;;           `> full 0' would still hold; the membership assert reddens it).
;;   r86-17  (compose, the OTHER axes) `path:' also composes under `all'
;;           with `todo:' / `is:backlog' / `due:' (r86-9/r86-10 only cover
;;           `#tag' + `is:overdue'); and under `any' a sibling rides the
;;           `todo:' disjunct — the three axes coexist, first-class.
;;   r86-18  (no scan-key / no cache-version bump, R53) the serialisation
;;           version stays 6, the coherence key stays a SEVEN-element list,
;;           and the key is INDEPENDENT of `org-air-view--tag-filter' (a
;;           `path:' flip repaints, never invalidates the scan cache).
;;   r86-19  (edge cases, never-error) a leading/trailing slash, a value
;;           matching the org ROOT itself, an INBOX-origin item and a
;;           nil-file item are all SANE (segment-aware, vacuously false,
;;           never signalling).
;;   r86-20  (segment boundary at the DIRECTORY level) `path:re' excludes a
;;           SIBLING `rest/' DIRECTORY (`re' is a strict PREFIX of the dir
;;           segment `rest') — r86-3 only pinned a `restore.org' LEAF and a
;;           `research/' dir; this pins the prefix-of-a-dir-component case.
;;
;; Full-revert audit vs pre-R86 `org-air-view.el': 14/16 reddened.  Two
;; correctly HELD, both INHERITED invariants R86 preserves (not introduces):
;;   * r86-14 — the R53 no-rescan property predates R86 (a bare-substring
;;     `path:' also never re-queries); STRENGTHENED above so its NARROWING
;;     claim is now genuinely R86-driven, and it reddens under a targeted
;;     substring/gate mutation.
;;   * r86-16 — the `#'-first precedence is STRUCTURAL, protected by three
;;     layers (parser `#'-refusal + matcher `#'-first + anchored `path:'
;;     regex); proven non-vacuous — the location axis WOULD match a
;;     `path/x' directory item, the `#' routing correctly wins.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)

(when (locate-library "org-air")
  (require 'org-air)
  (require 'org-air-review)
  (require 'org-air-calendar))

;;;; -------------------------------------------------------------------
;;;; Pure-predicate scaffolding (Decision B synthetic root — no disk)
;;;; -------------------------------------------------------------------
;;;; The path predicate is pure string/list work over the cached `:file'
;;;; slot + `org-air-files' — NO file access — so the segment/relative
;;;; seams need no on-disk .org (spec "Byte-golden / fixture impact").

(defconst org-air-r86--root '("/home/u/org")
  "A synthetic `org-air-files' source root for the pure predicate seams.
Its PARENT is `/home/u'; a file `/home/u/org/tasks/re/air/foo.org' is
made root-relative to `org/tasks/re/air/foo.org' — the machine `/home/u'
prefix dropped, the root basename `org' kept (Decision B).")

(defun org-air-r86--make (file &rest props)
  "Build an `org-air-item' with source FILE and PROPS (a plist).
PROPS pass through to `org-air-item-create' (`:tags', `:donep', `:todo',
`:kind', `:ntype', `:deadline', `:scheduled')."
  (apply #'org-air-item-create :file file :kind 'heading props))

(defun org-air-r86--passes (item tokens &optional match root)
  "Non-nil when ITEM passes filter TOKENS under MATCH at the frozen now.
Binds `org-air-files' to ROOT (default `org-air-r86--root') so the
relative transform is exercised, and drives the REAL board fold
`org-air-view--passes-filter-p'."
  (let ((org-air-files (or root org-air-r86--root))
        (org-air-filter-match (or match 'all))
        (org-air-view--tag-filter tokens)
        (org-air-view--filter-now org-air-test-now)
        (org-air-view--scope nil)
        (org-air-view--render-partition nil)
        (org-air-upcoming-days 7)
        (org-air-stale-days 21))
    (and (org-air-view--passes-filter-p item) t)))

;;;; -------------------------------------------------------------------
;;;; Live-corpus scaffolding (the r83 house idiom) for the compose /
;;;; surface / spy seams, which need genuine overdue + a real scan.
;;;; -------------------------------------------------------------------

(defvar org-air-r86--dir nil
  "The temp corpus directory of the current `org-air-r86--with-corpus'.")

(defun org-air-r86--reset-tables ()
  "Clear the GLOBAL query-layer tables the note surfaces read."
  (when (fboundp 'org-air-query-teardown)
    (clrhash org-air-query--file-meta)
    (clrhash org-air-query--visits)
    (clrhash org-air-query--denote-id-index)
    (setq org-air-query--link-graph-dirty nil)))

(defmacro org-air-r86--with-corpus (specs &rest body)
  "Create a temp Org corpus from SPECS (name . content) and run BODY.
Subdirectory names in SPECS (e.g. \"tasks/re/air/foo.org\") are created
under the root, so the recursive scan surfaces a real directory tree for
the `path:' segment matcher.  Binds the org-air roots, a temp cache, a
round-local board buffer name and quiets lockfiles/messages."
  (declare (indent 1) (debug t))
  `(let* ((org-air-r86--dir (make-temp-file "org-air-r86-" t)))
     (unwind-protect
         (progn
           (org-air-r86--reset-tables)
           (pcase-dolist (`(,name . ,content) ,specs)
             (let ((path (expand-file-name name org-air-r86--dir))
                   (coding-system-for-write 'utf-8-unix)
                   (file-name-handler-alist nil))
               (make-directory (file-name-directory path) t)
               (write-region (or content "") nil path nil 'silent)))
           (let ((org-air-files (list org-air-r86--dir))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-r86--dir))
                 (org-air-cache-file
                  (expand-file-name ".cache/board.eld" org-air-r86--dir))
                 (org-air-view-buffer-name "*org-air-r86*")
                 (org-air-upcoming-days 7)
                 (org-air-stale-days 21)
                 (org-tags-column 0)
                 (create-lockfiles nil)
                 (inhibit-message t))
             ,@body))
       (when (fboundp 'org-air-query-teardown)
         (org-air-query-teardown))
       (org-air-r86--reset-tables)
       (let ((kill-buffer-query-functions nil))
         (dolist (buf (buffer-list))
           (let ((fn (buffer-file-name buf)))
             (when (and fn (string-prefix-p org-air-r86--dir fn))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r86--dir t))))

(defmacro org-air-r86--with-board (specs &rest body)
  "Render the real board over the SPECS corpus (clock frozen); run BODY."
  (declare (indent 1) (debug t))
  `(org-air-r86--with-corpus ,specs
     (org-air-viewport-test--with-frozen-now
       (unwind-protect
           (org-air-viewport-test--with-render-guards
             (let ((org-air-view-width 120)
                   (org-air-view-height 60))
               (org-air)
               (let ((buf (get-buffer org-air-view-buffer-name)))
                 (should buf)
                 (with-current-buffer buf
                   ,@body))))
         (let ((kill-buffer-query-functions nil)
               (buf (get-buffer org-air-view-buffer-name)))
           (when buf (kill-buffer buf)))))))

(defun org-air-r86--item (title items)
  "Return the item in ITEMS whose title contains TITLE; assert it exists."
  (let ((item (org-air-test-find-item title items)))
    (should item)
    item))

;; A compose/surface corpus reused by r86-9/r86-10/r86-13/r86-14: a real
;; directory tree tasks/re/air, a SIBLING tasks/restore, tasks/re leaves
;; and a notes/ dir, with genuine overdue/upcoming dates.
(defconst org-air-r86--compose-corpus
  '(("tasks/re/air/alpha.org" . "#+title: alpha\n\n\
* TODO Alpha work :work:\nDEADLINE: <2026-06-10 Wed>\n")
    ("tasks/re/air/beta.org" . "#+title: beta\n\n\
* TODO Beta work :work:\nSCHEDULED: <2026-06-16 Tue>\n")
    ("tasks/restore/gamma.org" . "#+title: gamma\n\n\
* TODO Gamma work :work:\nDEADLINE: <2026-06-10 Wed>\n")
    ("tasks/re/epsilon.org" . "#+title: epsilon\n\n\
* TODO Epsilon plain\n")
    ("notes/delta.org" . "#+title: delta\n\n\
* Delta note\n")
    ("inbox.org" . "#+title: inbox\n"))
  "A four-directory corpus: tasks/re/air, a sibling tasks/restore,
tasks/re leaves and notes/, with a genuine overdue Alpha/Gamma.")

;;;; -------------------------------------------------------------------
;;;; r86-1 — the grammar admits `path:VALUE' (parse).
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r86-1-parse-path-token ()
  "`org-air-view--filter-token-parse' admits `path:VALUE' (r86-1).
`path:tasks/re' => (path . \"tasks/re\") — the RAW value atom, kept
verbatim for the lens; `PATH:air' parses too (case-fold); an empty
`path:' does NOT parse (nil => bare-substring).  Reverting the D1 arm
fails.  The `path:' arm sits AFTER the R72/R79 arms and is disjoint from
them, so no existing token is stolen."
  (skip-unless (locate-library "org-air"))
  (should (equal '(path . "tasks/re")
                 (org-air-view--filter-token-parse "path:tasks/re")))
  (should (equal '(path . "tasks/re/air")
                 (org-air-view--filter-token-parse "path:tasks/re/air")))
  ;; case-fold on the QUALIFIER; the value is kept verbatim (lower-cased
  ;; only at match time by `--path-segments').
  (should (equal '(path . "air")
                 (org-air-view--filter-token-parse "PATH:air")))
  ;; an empty value does not parse — falls through to bare-substring.
  (should (null (org-air-view--filter-token-parse "path:")))
  ;; the disjoint prefixes: the existing axes still parse to themselves.
  (should (equal '(is . overdue) (org-air-view--filter-token-parse "is:overdue")))
  (should (equal '(status . done) (org-air-view--filter-token-parse "is:done")))
  (should (equal '(todo . "TODO") (org-air-view--filter-token-parse "todo:TODO")))
  (should (equal '(due . 7) (org-air-view--filter-token-parse "due:7d"))))

;;;; -------------------------------------------------------------------
;;;; r86-2 — `path:tasks/re' shows ONLY items under tasks/re (segment).
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r86-2-core-segment-aware ()
  "`path:tasks/re' matches under tasks/re, NOT a sibling tasks/restore (r86-2).
An item whose `:file' is `…/tasks/re/air/foo.org' PASSES `path:tasks/re'
while a SIBLING item `…/tasks/restore/bar.org' FAILS it — the segment
boundary (`tasks' is followed by `restore', not `re').  Substringing the
predicate (matching `restore') would pass the sibling — fails."
  (skip-unless (locate-library "org-air"))
  (let ((under (org-air-r86--make "/home/u/org/tasks/re/air/foo.org"))
        (sibling (org-air-r86--make "/home/u/org/tasks/restore/bar.org")))
    (should (org-air-r86--passes under '("path:tasks/re")))
    (should-not (org-air-r86--passes sibling '("path:tasks/re")))
    ;; the multi-segment value binds `tasks' IMMEDIATELY to `re'.
    (should (org-air-r86--passes under '("path:tasks/re/air")))
    (should-not (org-air-r86--passes sibling '("path:tasks/re/air")))))

;;;; -------------------------------------------------------------------
;;;; r86-3 — `path:re' hits `re/' NOT `restore.org' NOR `research/'.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r86-3-segment-boundary-single ()
  "`path:re' hits the `re/' component, not a partial one (r86-3).
An item under `…/tasks/re/…' passes `path:re'; an item whose only
near-match is a `restore.org' LEAF, or a `research/' DIR, FAILS `path:re'
— a partial-component match never hits.  The single-segment sibling of
r86-2, pinning the boundary at the atomic component level."
  (skip-unless (locate-library "org-air"))
  (let ((in-re (org-air-r86--make "/home/u/org/tasks/re/air/foo.org"))
        (restore-leaf (org-air-r86--make "/home/u/org/tasks/restore.org"))
        (research-dir (org-air-r86--make "/home/u/org/research/notes.org")))
    (should (org-air-r86--passes in-re '("path:re")))
    (should-not (org-air-r86--passes restore-leaf '("path:re")))
    (should-not (org-air-r86--passes research-dir '("path:re")))))

;;;; -------------------------------------------------------------------
;;;; r86-4 — a filename is a whole segment (`path:foo.org', not `path:foo').
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r86-4-leaf-whole-segment ()
  "`path:foo.org' targets the file; `path:foo' does NOT (r86-4).
A single filename IS itself a whole path component, so `path:foo.org'
(the leaf, extension included) still targets one file — but `path:foo'
does NOT match `foo.org' (a partial component).  Whole-or-nothing
(Decision A), and the reason no `file:' alias is needed (Decision C)."
  (skip-unless (locate-library "org-air"))
  (let ((item (org-air-r86--make "/home/u/org/tasks/re/foo.org")))
    (should (org-air-r86--passes item '("path:foo.org")))
    (should-not (org-air-r86--passes item '("path:foo")))
    ;; the directory run still targets it, of course.
    (should (org-air-r86--passes item '("path:re/foo.org")))))

;;;; -------------------------------------------------------------------
;;;; r86-5 — org-root-relative: prefix dropped, root basename kept.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r86-5-relative-decision-b ()
  "The machine prefix is dropped, the root basename kept (r86-5, Decision B).
For `org-air-files' = (\"/home/u/org\") and item
`/home/u/org/tasks/re/foo.org': `path:tasks/re' PASSES even though the
absolute path carries `/home/u'; `path:org' PASSES (the root basename is
typeable); a segment ABOVE the root (`path:home' / `path:u') FAILS
(dropped).  Switching `--path-relative' to the absolute path would pass
`path:home' — fails.  The pure `--path-relative' transform is also
asserted directly."
  (skip-unless (locate-library "org-air"))
  (let* ((org-air-files '("/home/u/org"))
         (rel (org-air-view--path-relative "/home/u/org/tasks/re/foo.org")))
    ;; the transform drops the machine prefix, keeps the root basename.
    (should (equal "org/tasks/re/foo.org" rel)))
  (let ((item (org-air-r86--make "/home/u/org/tasks/re/foo.org")))
    (should (org-air-r86--passes item '("path:tasks/re")))
    ;; the root basename is a typeable first segment.
    (should (org-air-r86--passes item '("path:org")))
    ;; everything ABOVE the root is dropped and never matches.
    (should-not (org-air-r86--passes item '("path:home")))
    (should-not (org-air-r86--passes item '("path:u")))
    ;; the shallowest source wins the tie-break (most segments kept).
    (should (org-air-r86--passes item '("path:tasks/re")
                                 'all '("/home/u/org" "/home/u/org/tasks")))
    (should (org-air-r86--passes item '("path:org")
                                 'all '("/home/u/org" "/home/u/org/tasks")))))

;;;; -------------------------------------------------------------------
;;;; r86-6 — absolute fallback: an item under NO source still matches.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r86-6-absolute-fallback ()
  "An item under no configured source matches by absolute segments (r86-6).
With `org-air-files' empty (or not containing the item), an item
`/x/tasks/re/foo.org' still PASSES `path:tasks/re' (absolute fallback),
so the relative rule NARROWS noise without LOSING a real match.  A
segment from the absolute path (`path:x') matches too — no root is
dropped when there is no root."
  (skip-unless (locate-library "org-air"))
  (let ((item (org-air-r86--make "/x/tasks/re/foo.org")))
    ;; no source at all: the absolute path is segment-matched whole.
    (should (org-air-r86--passes item '("path:tasks/re") 'all nil))
    (should (org-air-r86--passes item '("path:x") 'all nil))
    ;; a source that does NOT contain the item: still the absolute branch.
    (should (org-air-r86--passes item '("path:tasks/re") 'all '("/other/root")))
    ;; the pure transform returns the absolute path when unrooted.
    (let ((org-air-files nil))
      (should (equal "/x/tasks/re/foo.org"
                     (org-air-view--path-relative "/x/tasks/re/foo.org"))))))

;;;; -------------------------------------------------------------------
;;;; r86-7 — case-insensitive: `path:AIR' matches `air/'.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r86-7-case-insensitive ()
  "`path:AIR' / `PATH:Air' match an item under `air/' (r86-7).
The predicate lower-cases both sides (`--path-segments'), so the axis is
case-insensitive throughout — like the rest of the `/' grammar.  Making
the comparison case-sensitive fails."
  (skip-unless (locate-library "org-air"))
  (let ((item (org-air-r86--make "/home/u/org/tasks/re/air/foo.org")))
    (should (org-air-r86--passes item '("path:AIR")))
    (should (org-air-r86--passes item '("path:Air")))
    (should (org-air-r86--passes item '("path:TASKS/RE")))
    ;; a MIXED-case path on disk still matches a lower value.
    (let ((mixed (org-air-r86--make "/home/u/org/Tasks/Re/Air/foo.org")))
      (should (org-air-r86--passes mixed '("path:tasks/re"))))))

;;;; -------------------------------------------------------------------
;;;; r86-8 — an unmatched path yields the EMPTY visible set.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r86-8-unmatched-empty-not-all ()
  "A non-existent path leaves the visible set EMPTY (r86-8).
`path:nope/zzz' over a corpus with no such segment run leaves the visible
set EMPTY (NOT the full board).  A predicate that defaults TRUE on
no-match would return everything — fails.  Driven through the shared
`org-air-view--visible-items' fold."
  (skip-unless (locate-library "org-air"))
  (let* ((org-air-files org-air-r86--root)
         (org-air-view--scope nil)
         (org-air-view--render-partition nil)
         (org-air-view--filter-now org-air-test-now)
         (org-air-filter-match 'all)
         (items (list (org-air-r86--make "/home/u/org/tasks/re/air/a.org" :title "A")
                      (org-air-r86--make "/home/u/org/notes/b.org" :title "B"))))
    ;; a matching path narrows to its members…
    (let ((org-air-view--tag-filter '("path:tasks/re")))
      (should (equal 1 (length (org-air-view--visible-items items)))))
    ;; …a non-existent path narrows to NOTHING (not the full set).
    (let ((org-air-view--tag-filter '("path:nope/zzz")))
      (should (null (org-air-view--visible-items items))))
    ;; the empty filter still passes everything (baseline).
    (let ((org-air-view--tag-filter nil))
      (should (equal 2 (length (org-air-view--visible-items items)))))))

;;;; -------------------------------------------------------------------
;;;; r86-9 — compose under M-/ (AND): path: + #tag + is:overdue.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r86-9-compose-and ()
  "`path:tasks/re' AND `#work' AND `is:overdue' under `all' (r86-9).
Over a real corpus: Alpha (tasks/re/air, #work, overdue) PASSES all
three; Beta (tasks/re/air, #work, NOT overdue) FAILS (no is:overdue);
Gamma (tasks/restore, #work, overdue) FAILS (wrong path); Epsilon
(tasks/re, untagged, not overdue) FAILS.  Reverting the D3 `path' clause
collapses the AND to empty — fails."
  (skip-unless (locate-library "org-air"))
  (org-air-r86--with-corpus org-air-r86--compose-corpus
    (org-air-viewport-test--with-frozen-now
      (let* ((items (org-air-query-items))
             (alpha (org-air-r86--item "Alpha work" items))
             (beta (org-air-r86--item "Beta work" items))
             (gamma (org-air-r86--item "Gamma work" items))
             (epsilon (org-air-r86--item "Epsilon plain" items))
             (tokens '("path:tasks/re" "#work" "is:overdue")))
        ;; the corpus is as intended: Alpha/Gamma overdue, Beta upcoming.
        (should (org-air-classify--overdue-p alpha org-air-test-now))
        (should (org-air-classify--overdue-p gamma org-air-test-now))
        (should-not (org-air-classify--overdue-p beta org-air-test-now))
        ;; AND: only the item that is all three passes.
        (should (org-air-r86--passes alpha tokens 'all (list org-air-r86--dir)))
        (should-not (org-air-r86--passes beta tokens 'all (list org-air-r86--dir)))
        (should-not (org-air-r86--passes gamma tokens 'all (list org-air-r86--dir)))
        (should-not (org-air-r86--passes epsilon tokens 'all (list org-air-r86--dir)))
        ;; the path conjunct alone excludes the restore sibling.
        (should (org-air-r86--passes alpha '("path:tasks/re") 'all (list org-air-r86--dir)))
        (should-not (org-air-r86--passes gamma '("path:tasks/re") 'all (list org-air-r86--dir)))))))

;;;; -------------------------------------------------------------------
;;;; r86-10 — compose under M-/ (OR): a path-only item still appears.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r86-10-compose-or ()
  "The SAME three tokens under `any' broaden (r86-10).
`path:tasks/re' OR `#work' OR `is:overdue' under `any': Alpha (all
three), Beta (#work), Gamma (#work + overdue) and Epsilon (path only,
untagged, not overdue) ALL pass; Delta (notes/, untagged, not overdue)
is the negative control (none of the three).  Epsilon appearing via the
`path:' disjunct confirms `path:' is a first-class token in the OR fold."
  (skip-unless (locate-library "org-air"))
  (org-air-r86--with-corpus org-air-r86--compose-corpus
    (org-air-viewport-test--with-frozen-now
      (let* ((items (org-air-query-items))
             (alpha (org-air-r86--item "Alpha work" items))
             (beta (org-air-r86--item "Beta work" items))
             (gamma (org-air-r86--item "Gamma work" items))
             (epsilon (org-air-r86--item "Epsilon plain" items))
             (delta (org-air-r86--item "Delta note" items))
             (tokens '("path:tasks/re" "#work" "is:overdue"))
             (root (list org-air-r86--dir)))
        (should (org-air-r86--passes alpha tokens 'any root))
        (should (org-air-r86--passes beta tokens 'any root))
        (should (org-air-r86--passes gamma tokens 'any root))
        ;; the path-only item: no #work, not overdue — appears via path:.
        (should (org-air-r86--passes epsilon tokens 'any root))
        (should-not (org-air-r86--passes epsilon '("#work" "is:overdue") 'any root))
        ;; Delta (notes/): under none of the three disjuncts.
        (should-not (org-air-r86--passes delta tokens 'any root))))))

;;;; -------------------------------------------------------------------
;;;; r86-11 — gate-free: a DONE / note item under tasks/re still matches.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r86-11-gate-free ()
  "A DONE / note-typed item under `tasks/re' STILL matches `path:tasks/re'
\(r86-11).  The location axis carries NO board-active / task-routed gate
— a completed note under `tasks/re' still LIVES under `tasks/re'.  A DONE
item and a `knowledge'-typed note both pass `path:tasks/re'.  Routing
`path:' through the gated `--filter-date-token-match-p' would drop the
DONE item (board-active-p is false) — fails.  The contrast: the same DONE
item does NOT pass the gated `is:overdue'."
  (skip-unless (locate-library "org-air"))
  (let ((done (org-air-r86--make "/home/u/org/tasks/re/done.org"
                                 :title "Done note" :donep t :todo "DONE"))
        (note (org-air-r86--make "/home/u/org/tasks/re/knowledge.org"
                                 :title "Kn" :kind 'file :ntype 'knowledge))
        (archived (org-air-r86--make "/home/u/org/tasks/re/arch.org"
                                     :title "Arch" :tags (list org-archive-tag))))
    ;; the gate-free location axis matches all three under tasks/re.
    (should (org-air-r86--passes done '("path:tasks/re")))
    (should (org-air-r86--passes note '("path:tasks/re")))
    (should (org-air-r86--passes archived '("path:tasks/re")))
    ;; contrast: a GATED token (is:overdue) sees the DONE item as history.
    (should-not (org-air-r86--passes done '("is:overdue")))
    ;; and the pure predicate is orthogonal to done-ness.
    (should (org-air-view--filter-path-token-match-p "tasks/re" done))))

;;;; -------------------------------------------------------------------
;;;; r86-12 — completion offers `path:SEG' per distinct dir segment.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r86-12-completion-offer ()
  "`path:SEG' is offered per distinct DIRECTORY segment (r86-12).
`org-air-view--filter-vocabulary' over a corpus spanning `tasks/re/air'
and `notes/' contains `path:tasks', `path:re', `path:air', `path:notes'
— and does NOT offer the leaf filenames (`path:foo.org'); the `path:'
entries are SORTED (byte-stable).  Reverting the D4 append fails."
  (skip-unless (locate-library "org-air"))
  (let* ((org-air-files org-air-r86--root)
         (org-air-view--items
          (list (org-air-r86--make "/home/u/org/tasks/re/air/foo.org" :title "F")
                (org-air-r86--make "/home/u/org/notes/bar.org" :title "B"))))
    ;; the pure segment collector: distinct DIRECTORY segments, sorted,
    ;; leaves dropped.
    (should (equal '("air" "notes" "org" "re" "tasks")
                   (org-air-view--filter-path-segments)))
    (let ((vocab (org-air-view--filter-vocabulary)))
      (dolist (seg '("path:tasks" "path:re" "path:air" "path:notes" "path:org"))
        (should (member seg vocab)))
      ;; no leaf filename is offered.
      (should-not (member "path:foo.org" vocab))
      (should-not (member "path:bar.org" vocab))
      ;; the path: entries appear in sorted order within the vocab.
      (let ((paths (seq-filter (lambda (v) (string-prefix-p "path:" v)) vocab)))
        (should (equal paths (sort (copy-sequence paths) #'string<)))))))

;;;; -------------------------------------------------------------------
;;;; r86-13 — board + day + review all inherit `path:'.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r86-13-surfaces-inherit ()
  "The board, day view and review all filter by `path:' (r86-13).
The same corpus filtered by `path:tasks/re' yields the SAME membership
decision on the board (`--passes-filter-p'), the day view (R6, shared
`--visible-items' via `--day-groups') and the review view
(`org-air-review--visible-items', ITEM threaded); the project/revisit
surfaces (ITEM=nil) treat `path:' as vacuously false (unchanged)."
  (skip-unless (locate-library "org-air"))
  (org-air-r86--with-corpus org-air-r86--compose-corpus
    (org-air-viewport-test--with-frozen-now
      (let* ((items (org-air-query-items))
             (alpha (org-air-r86--item "Alpha work" items))
             (gamma (org-air-r86--item "Gamma work" items))
             (day (encode-time '(0 0 12 10 6 2026 nil -1 nil))) ; the DEADLINE day
             (org-air-files (list org-air-r86--dir))
             (org-air-view--scope nil)
             (org-air-view--render-partition nil)
             (org-air-view--filter-now org-air-test-now)
             (org-air-filter-match 'all))
        ;; BOARD: the shared board fold.
        (let ((org-air-view--tag-filter '("path:tasks/re")))
          (should (org-air-view--passes-filter-p alpha))
          (should-not (org-air-view--passes-filter-p gamma))
          ;; DAY view: `--day-groups' filters through `--visible-items';
          ;; Alpha's DEADLINE day lists Alpha, never the restore sibling.
          (let ((groups (org-air-view--day-groups items day)))
            (should (memq alpha (cdr (assoc "Deadline" groups))))
            (should-not (memq gamma (cdr (assoc "Deadline" groups))))))
        ;; REVIEW: the ITEM-threaded review fold.
        (let ((org-air-review--items items)
              (org-air-view--tag-filter '("path:tasks/re")))
          (let ((visible (org-air-review--visible-items)))
            (should (memq alpha visible))
            (should-not (memq gamma visible))))
        ;; PROJECT / REVISIT: ITEM=nil => a location token is vacuously
        ;; false (no slot, no claim) — the R72 Decision 8 law extended.
        (let ((org-air-view--tag-filter '("path:tasks/re")))
          (should-not (org-air-view--tokens-pass-filter-p
                       "tasks/re/air/alpha.org" '() nil)))))))

;;;; -------------------------------------------------------------------
;;;; r86-14 — toggling `path:' never re-queries (R53, spy=0).
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r86-14-no-rescan-spy-zero ()
  "Setting / composing / clearing `path:' never re-queries (r86-14, R53).
A spy on `org-air-query-items' stays at ZERO across setting, composing
and clearing a `path:' filter while the VISIBLE set changes — the token
is a pure slot/string predicate.  Any accidental rescan fails.

AUDIT STRENGTHEN (test seat): the narrowed SET is pinned to the
SEGMENT-AWARE membership (Alpha/Beta/Epsilon under tasks/re, NOT the
restore sibling Gamma), so the seam can no longer pass VACUOUSLY with a
`path:' that matches nothing — the pre-R86 bare-substring reading of
`path:tasks/re' narrows to the EMPTY set (`> full 0' still holds), which
the membership + `> 0' asserts below now redden."
  (skip-unless (locate-library "org-air"))
  (org-air-r86--with-board org-air-r86--compose-corpus
    (let* ((items org-air-view--items)
           (org-air-view--scope nil)
           (org-air-view--render-partition nil)
           (org-air-view--filter-now org-air-test-now)
           (org-air-filter-match 'all)
           (alpha (org-air-r86--item "Alpha work" items))
           (beta (org-air-r86--item "Beta work" items))
           (gamma (org-air-r86--item "Gamma work" items))
           (epsilon (org-air-r86--item "Epsilon plain" items))
           (queries 0)
           (full nil) (narrowed nil) (composed nil) (cleared nil))
      (cl-letf* ((orig (symbol-function 'org-air-query-items))
                 ((symbol-function 'org-air-query-items)
                  (lambda (&rest a) (cl-incf queries) (apply orig a))))
        ;; baseline: no filter.
        (let ((org-air-view--tag-filter nil))
          (setq full (org-air-view--visible-items items)))
        ;; set a path: filter — the set narrows.
        (let ((org-air-view--tag-filter '("path:tasks/re")))
          (setq narrowed (org-air-view--visible-items items)))
        ;; compose it with #work — narrows further.
        (let ((org-air-view--tag-filter '("path:tasks/re" "#work")))
          (setq composed (org-air-view--visible-items items)))
        ;; clear it — back to the full set.
        (let ((org-air-view--tag-filter nil))
          (setq cleared (org-air-view--visible-items items))))
      ;; NOT ONE re-query fired across the whole dance.
      (should (= 0 queries))
      ;; …yet the visible set genuinely moved — by MONOTONE COUNT…
      (should (> (length full) (length narrowed)))
      (should (>= (length narrowed) (length composed)))
      (should (= (length full) (length cleared)))
      ;; …and — the anti-vacuity teeth — to the SEGMENT-AWARE membership,
      ;; not merely "something smaller" (a broken path: matches nothing).
      (should (> (length narrowed) 0))
      (should (memq alpha narrowed))
      (should (memq beta narrowed))
      (should (memq epsilon narrowed))
      (should-not (memq gamma narrowed))
      ;; the #work conjunct keeps the tagged tasks/re items, drops the
      ;; untagged Epsilon — composition still runs rescan-free.
      (should (memq alpha composed))
      (should-not (memq epsilon composed)))))

;;;; -------------------------------------------------------------------
;;;; r86-15 — the lens label reads verbatim; the near-miss keeps quotes.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r86-15-label-verbatim ()
  "A `path:' token reads verbatim; the empty near-miss keeps quotes (r86-15).
`org-air-view--filter-token-label' returns `path:tasks/re' UNQUOTED for
the real token (it parses) and `\"path:\"' (quoted) for the empty
near-miss (it does not) — the quoting is the tell (D5).  A `#path:x'
token reads verbatim (it is a tag)."
  (skip-unless (locate-library "org-air"))
  (should (equal "path:tasks/re"
                 (org-air-view--filter-token-label "path:tasks/re")))
  (should (equal "path:tasks/re/air"
                 (org-air-view--filter-token-label "path:tasks/re/air")))
  ;; the empty near-miss does not parse => it quotes (text, not a token).
  (should (equal "\"path:\"" (org-air-view--filter-token-label "path:")))
  ;; a `#'-spelled token reads verbatim (a tag).
  (should (equal "#path:x" (org-air-view--filter-token-label "#path:x"))))

;;;; -------------------------------------------------------------------
;;;; r86-16 — `#path:x' stays a TAG (the `#' branch is first).
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r86-16-hash-precedence ()
  "`#path:x' is a TAG match, never a location token (r86-16).
The `#' branch of `--filter-token-match-p' is FIRST, so a `#path:x' token
matches a heading tagged `path:x' (via the tag membership rule) and is
NEVER parsed as a location token — `--filter-token-parse' refuses any
`#'-prefixed token outright.  Moving the `path:' arm ahead of the `#'
guard would break this (the guard is structural, but the seam pins the
invariant)."
  (skip-unless (locate-library "org-air"))
  ;; the parser refuses the `#'-prefixed token.
  (should (null (org-air-view--filter-token-parse "#path:x")))
  ;; and the matcher routes it through the `#' tag branch: a heading
  ;; tagged `path:x' is HIT by `#path:x' regardless of its file path.
  (let ((tagged (org-air-r86--make "/home/u/org/elsewhere/foo.org"
                                   :title "T" :tags '("path:x"))))
    (should (org-air-view--filter-token-match-p
             "#path:x" "" (org-air-item-tags tagged) tagged))
    ;; an item under a `path/x' DIRECTORY but WITHOUT the tag is NOT hit
    ;; by `#path:x' (it is a tag token, not a location token).
    (let ((untagged (org-air-r86--make "/home/u/org/path/x/bar.org"
                                       :title "U")))
      (should-not (org-air-view--filter-token-match-p
                   "#path:x" "" (org-air-item-tags untagged) untagged)))))

;;;; -------------------------------------------------------------------
;;;; r86-17 (AUDIT gap) — `path:' composes with `todo:' / `is:backlog' /
;;;; `due:' too (the axes r86-9/r86-10 leave uncovered).
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r86-17-compose-todo-backlog-due ()
  "`path:' composes with `todo:' / `is:backlog' / `due:' too (r86-17).
The drive brief names FIVE axes `path:' must coexist with — r86-9/r86-10
cover `#tag' + `is:overdue'; this seam closes the gap on the remaining
THREE.  Over a tasks/re-vs-sibling-tasks/restore corpus, under `all':
  * `path:tasks/re' AND `todo:TODO' selects the TODO items under tasks/re
    (Keyword/Deferred/Soon) and EXCLUDES the restore sibling (Sibling kw);
  * `path:tasks/re' AND `is:backlog' selects ONLY the :backlog:-tagged
    item under tasks/re (Deferred), excluding the untagged siblings AND
    the restore backlog item (Sib defer — the path conjunct);
  * `path:tasks/re' AND `due:7d' selects ONLY the soon-deadline item
    (Soon), excluding the dateless ones (Keyword).
Under `any', the restore sibling still appears via the `todo:' disjunct
even though `path:notes' misses it — the three axes coexist, first-class
in BOTH folds.  (`org-timestamp'-slot dates need a real scan, so this
seam uses the live corpus, not the synthetic-item helper.)"
  (skip-unless (locate-library "org-air"))
  (org-air-r86--with-corpus
      '(("tasks/re/kw.org" . "* TODO Keyword task :work:\n")
        ("tasks/re/back.org" . "* TODO Deferred item :backlog:\n")
        ("tasks/re/soon.org" . "* TODO Soon thing\nDEADLINE: <2026-06-17 Wed>\n")
        ("tasks/restore/kwx.org" . "* TODO Sibling kw :work:\n")
        ("tasks/restore/backx.org" . "* TODO Sib defer :backlog:\n")
        ("inbox.org" . "#+title: inbox\n"))
    (org-air-viewport-test--with-frozen-now
      (let* ((items (org-air-query-items))
             (root (list org-air-r86--dir))
             (kw (org-air-r86--item "Keyword task" items))
             (back (org-air-r86--item "Deferred item" items))
             (soon (org-air-r86--item "Soon thing" items))
             (kwx (org-air-r86--item "Sibling kw" items))
             (backx (org-air-r86--item "Sib defer" items)))
        ;; sanity: the corpus dates/tags are as intended.
        (should (org-air-classify--backlog-p back))
        (should (org-air-classify--due-within-p soon org-air-test-now 7))
        ;; path: AND todo:  (keyword axis)
        (should (org-air-r86--passes kw '("path:tasks/re" "todo:TODO") 'all root))
        (should (org-air-r86--passes back '("path:tasks/re" "todo:TODO") 'all root))
        (should (org-air-r86--passes soon '("path:tasks/re" "todo:TODO") 'all root))
        (should-not (org-air-r86--passes kwx '("path:tasks/re" "todo:TODO") 'all root))
        ;; path: AND is:backlog  (R83 backlog axis)
        (should (org-air-r86--passes back '("path:tasks/re" "is:backlog") 'all root))
        (should-not (org-air-r86--passes backx '("path:tasks/re" "is:backlog") 'all root))
        (should-not (org-air-r86--passes kw '("path:tasks/re" "is:backlog") 'all root))
        ;; path: AND due:  (R72 date axis)
        (should (org-air-r86--passes soon '("path:tasks/re" "due:7d") 'all root))
        (should-not (org-air-r86--passes kw '("path:tasks/re" "due:7d") 'all root))
        ;; OR: the restore sibling rides the todo: disjunct; neither
        ;; disjunct hits it when both miss (path:notes + is:backlog).
        (should (org-air-r86--passes kwx '("path:notes" "todo:TODO") 'any root))
        (should-not (org-air-r86--passes kwx '("path:notes" "is:backlog") 'any root))))))

;;;; -------------------------------------------------------------------
;;;; r86-18 (AUDIT gap) — NO scan-key element, NO cache-version bump (R53).
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r86-18-no-cache-version-bump ()
  "`path:' adds NO scan-key element and NO cache-version bump (r86-18, R53).
The spec forbids a rescan slot or an `org-air-view--cache-version' bump:
`path:' reads the ALREADY-cached `org-air-item-file' slot + the ALREADY-
keyed `org-air-files' knob.  So (a) the serialisation version stays at
its pre-R86 value 6; (b) `org-air-view--cache-key' stays a SEVEN-element
list (the R77 element count — R86 added none); (c) the key is INDEPENDENT
of `org-air-view--tag-filter' AND `org-air-filter-match' — setting or
composing a `path:' token does NOT change the coherence key (a `path:'
flip repaints, it never invalidates the scan cache).  Bumping the version
or keying the filter into the scan key fails; the closing check confirms
the key is NOT inert (a real source change DOES move it)."
  (skip-unless (locate-library "org-air"))
  ;; (a) no version bump.
  (should (= 6 org-air-view--cache-version))
  (let ((org-air-files '("/home/u/org"))
        (org-air-inbox-file "/home/u/org/inbox.org"))
    ;; (b) still a SEVEN-element key (no path element added).
    (should (= 7 (length (org-air-view--cache-key))))
    (let ((base (org-air-view--cache-key)))
      ;; (c) a path: token in the live filter must not perturb the key…
      (let ((org-air-view--tag-filter '("path:tasks/re")))
        (should (equal base (org-air-view--cache-key))))
      ;; …nor a composed filter, nor the AND/OR combinator.
      (let ((org-air-view--tag-filter '("path:tasks/re" "#work"))
            (org-air-filter-match 'any))
        (should (equal base (org-air-view--cache-key))))
      ;; anti-inertness: a genuine scan input (the source set) STILL moves
      ;; the key — so the equalities above are a real invariance, not a
      ;; constant-key artefact.
      (let ((org-air-files '("/home/u/org" "/home/u/other")))
        (should-not (equal base (org-air-view--cache-key)))))))

;;;; -------------------------------------------------------------------
;;;; r86-19 (AUDIT gap) — edge cases: leading/trailing slash, the org root
;;;; itself, an INBOX-origin item, a nil-file item — sane, never-error.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r86-19-edge-cases-never-error ()
  "`path:' edge cases are all sane and never signal (r86-19).
The drive brief's edge list: a LEADING/TRAILING slash, a MULTI-segment
value, a value matching the org ROOT itself, an item whose origin is the
INBOX, and (defensively) a nil-file item.  `--path-segments' drops empty
runs (`split-string' OMIT-NULLS), so surrounding/duplicated slashes are
inert; the root basename is a typeable segment; an inbox item scopes by
its leaf/`path:org' and never by a task dir; a nil-file item is
vacuously false via the `when-let*' guard — all WITHOUT error."
  (skip-unless (locate-library "org-air"))
  (let ((org-air-files '("/home/u/org"))
        (org-air-inbox-file "/home/u/org/inbox.org"))
    (let ((it (org-air-r86--make "/home/u/org/tasks/re/air/foo.org")))
      ;; a LEADING slash is inert.
      (should (org-air-r86--passes it '("path:/tasks/re")))
      ;; a TRAILING slash is inert.
      (should (org-air-r86--passes it '("path:tasks/re/")))
      ;; both at once, multi-segment.
      (should (org-air-r86--passes it '("path:/tasks/re/air/")))
      ;; a value matching the org ROOT itself (its basename segment).
      (should (org-air-r86--passes it '("path:org")))
      (should (org-air-r86--passes it '("path:org/tasks/re")))
      ;; a value that is ONLY slashes: no segments => no match, no error.
      (should-not (org-air-r86--passes it '("path:/")))
      (should-not (org-air-r86--passes it '("path:///"))))
    ;; an INBOX-origin item is sane: matches its leaf + the root, never a
    ;; task subtree.
    (let ((inbox (org-air-r86--make "/home/u/org/inbox.org" :title "Captured")))
      (should (org-air-r86--passes inbox '("path:inbox.org")))
      (should (org-air-r86--passes inbox '("path:org")))
      (should-not (org-air-r86--passes inbox '("path:tasks/re")))
      (should (org-air-view--filter-path-token-match-p "inbox.org" inbox)))
    ;; a nil-file item never errors and is vacuously false (R72 Decision 8
    ;; extended to the location axis).
    (let ((nofile (org-air-r86--make nil :title "No file")))
      (should-not (org-air-view--filter-path-token-match-p "tasks/re" nofile))
      (should (org-air-r86--passes nofile nil))
      (should-not (org-air-r86--passes nofile '("path:tasks/re"))))
    ;; the pure transform is total (a rooted AND an unrooted path).
    (should (stringp (org-air-view--path-relative "/home/u/org/a.org")))
    (should (stringp (org-air-view--path-relative "relative/no/root.org")))))

;;;; -------------------------------------------------------------------
;;;; r86-20 (AUDIT gap) — segment boundary at the DIRECTORY level: a
;;;; sibling `rest/' dir (re is a strict PREFIX of the dir segment).
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r86-20-segment-boundary-dir-prefix ()
  "`path:re' excludes a SIBLING `rest/' DIRECTORY (r86-20).
r86-3 pinned the boundary against a `restore.org' LEAF and a `research/'
dir; this pins the remaining case — a directory segment of which `re' is a
strict PREFIX (`rest').  An item under `tasks/re/…' passes `path:re'; a
sibling under `tasks/rest/…' FAILS it (the component is `rest', not
`re'), and `path:tasks/re' likewise excludes `tasks/rest/…' (tasks is
followed by `rest', not `re').  A substring predicate would leak `rest'."
  (skip-unless (locate-library "org-air"))
  (let ((in-re    (org-air-r86--make "/home/u/org/tasks/re/air/foo.org"))
        (rest-dir (org-air-r86--make "/home/u/org/tasks/rest/bar.org")))
    (should (org-air-r86--passes in-re '("path:re")))
    (should-not (org-air-r86--passes rest-dir '("path:re")))
    (should-not (org-air-r86--passes rest-dir '("path:tasks/re")))
    ;; and the pure contiguous-run predicate agrees at the list level.
    (should (org-air-view--path-run-match-p '("re") '("tasks" "re" "air")))
    (should-not (org-air-view--path-run-match-p '("re") '("tasks" "rest")))))

(provide 'org-air-round86-test)
;;; org-air-round86-test.el ends here
