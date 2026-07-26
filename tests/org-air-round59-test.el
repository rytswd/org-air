;;; org-air-round59-test.el --- executing ERTs for v0.5 round-59 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.5 round-59 (air/v0.5/org-air-round59-design.org):
;; container headings are STRUCTURE, not items — a heading with child
;; headings and NO actionable signal of its OWN (no TODO keyword per the
;; R57 merged vocabulary, no own-body scheduled/deadline/active timestamp
;; per the R54 date model) is skipped as a board row EVERYWHERE, the
;; Inbox included; its children represent the content.  Inbox LEAVES (no
;; children) still surface; `org-air-skip-container-headings' (default t)
;; toggles the whole behaviour; cache v4 -> v5 with the knob as the
;; fourth cache-key element.
;;
;; All BATCH/headless over temp-dir corpora through the REAL synchronous
;; scan (`org-air-query-items'); classify assertions are pure
;; struct-in/buckets-out at the frozen `org-air-test-now'; renders go
;; through the real `(org-air)' entry under the anti-tautology render
;; guards.  The spec's ERT seams T1-T14 map one-to-one:
;;
;;   r59-1  (T1) the reported shape: the user's inbox verbatim — `* New'
;;          (PROPERTIES drawer + description) grouping two `** TODO …
;;          :inbox:' children.  `New' scans childp t and classifies
;;          exactly (container) — NO 'inbox — while both children keep
;;          'inbox; the rendered board's Inbox section holds the two
;;          child titles and NOT "New"; the Inbox badge reads 2.
;;          Reverting the classify branch FAILS (pre-R59: `New' rode the
;;          R54-2 bypass into 'inbox and rendered).
;;   r59-2  (T2) general-board twin: the same shape in a NON-inbox file
;;          classifies (container) where it was (knowledge) — both
;;          off-board (the in-test knob-nil twin proves the old routing
;;          is still reachable); parent absent from the render, TODO
;;          children present ('attention).
;;   r59-3  (T3) task parent kept: `* TODO Ship v1' WITH TODO children
;;          still classifies into task buckets and renders — an impl
;;          skipping ALL parents FAILS here.
;;   r59-4  (T4) dated parent kept: keyword-less `* Milestone review'
;;          with its OWN SCHEDULED and children => ntype task,
;;          'upcoming, rendered.
;;   r59-5  (T5) the child-date trap: a container whose CHILD carries
;;          SCHEDULED — the parent's subtree-wide `active-ts' IS non-nil
;;          and `--stale-eligible-p' IS t (the trap proven live) while
;;          `own-active-ts' is nil and the parent still classifies
;;          (container).  Testing the subtree-wide slot (or reusing
;;          `--stale-eligible-p') FAILS here.
;;   r59-6  (T6) own active date kept: a keyword-less parent with an
;;          active <ts> in its OWN body above the first child =>
;;          `own-active-ts' non-nil => NOT a container — the inbox
;;          variant renders as an Inbox row, the non-inbox variant
;;          routes 'knowledge (its R54 treatment), never 'container.
;;   r59-7  (T7) the inbox leaf rule: a keyword-less dateless LEAF in
;;          the inbox still classifies 'inbox and renders (a real triage
;;          unit) while a sibling container skips — an impl skipping
;;          keyword-less inbox LEAVES too FAILS here.
;;   r59-8  (T8) the defcustom: knob nil re-classifies the SAME scanned
;;          `New' item 'inbox (live knob over persisted slots), renders
;;          it again (badge 3 — the pre-R59 board), and a knob-nil
;;          re-scan reverts the F7 file vote to knowledge.  Reverting
;;          the knob gate FAILS.
;;   r59-9  (T9) F7 abstention: `* Projects' + `** TODO …' types the
;;          FILE task (off Revisit scope; `--scope-entries' excludes
;;          it); a prose file with sectioned subsections stays
;;          knowledge (in scope); a KB file with one TODO leaf stays
;;          knowledge; container-only quantification (zero non-container
;;          voters) never yields task.  Reverting the vote filter FAILS
;;          (the GTD file types knowledge, back into Revisit).
;;   r59-10 (T10) override wins: the container shape plus
;;          `:ORG_AIR_TYPE: task' (and the `:task:'-tag twin) => ntype
;;          task => NOT a container, full task treatment ('attention,
;;          rendered).  Reverting the ntype conjunct FAILS.
;;   r59-11 (T11) day view: a container whose child carries a
;;          `:CREATED: [today]' drawer INHERITS the stamp via
;;          `subtree-ts' (proven non-nil, keyed today) yet Logged/created
;;          holds the child and NOT the parent; knob nil restores the
;;          duplicate parent row.  Reverting the `--day-groups' filter
;;          FAILS.
;;   r59-12 (T12) capture guard: `org-air-capture' items (TODO + leaf)
;;          are unaffected by construction — 'inbox, rendered; an item
;;          from the at-point constructor (nil signal slots) is NEVER a
;;          container (the conservative default).
;;   r59-13 (T13) cache v5 + key: version is 5; the knob is the FOURTH
;;          key element (live value both ways); a crafted v4 cache with
;;          the CURRENT key is a clean cold miss (no error, no hang);
;;          v5 roundtrips `childp'/`own-active-ts' write->read->classify
;;          with scan-identical buckets; a cache written under knob t
;;          does NOT hydrate under knob nil.  Reverting the version bump
;;          or the key element FAILS (stale hydration).  R60 re-bless
;;          (air/v0.5/org-air-round60-design.org R60-3): the key is FIVE
;;          elements now — `org-air-exclude-regexps' joins as the FIFTH;
;;          the shape conjunct tracks it both ways (includes + detects
;;          the exclude set) and a knob-parallel hydration conjunct pins
;;          that an exclude-set flip is a clean cold miss too.
;;   r59-14 (T14) data purity: full board render + day grouping over
;;          CACHE-HYDRATED items with a live container present =>
;;          `find-file-noselect' called ZERO times, the visible
;;          `buffer-list' unchanged, no buffer visiting any corpus file
;;          — the predicate is slot reads only.
;;
;; REVERT-FAIL verified against the pre-impl trunk (lomzomup's parent)
;; in a scratch workspace: every ERT above goes RED there — the
;; `childp'/`own-active-ts' accessors and
;; `org-air-query-container-item-p' are void, `* New' classifies 'inbox
;; and renders, the day view duplicates the parent, the GTD file types
;; knowledge and the cache key has three elements.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)

(when (locate-library "org-air")
  (require 'org-air))

(defvar org-air-log-cap)

;;;; -------------------------------------------------------------------
;;;; Corpus scaffolding
;;;; -------------------------------------------------------------------

(defvar org-air-r59--dir nil
  "The temp corpus directory of the current `org-air-r59--with-corpus'.")

(defun org-air-r59--reset-tables ()
  "Clear the GLOBAL query-layer tables (file-meta / visits / denote ids).
Session globals are never cleared by a scan; the T9/T8 file-ntype
assertions read `org-air-query--file-meta' directly, so every test
starts and ends empty — absolute temp paths from another test can never
leak into a vote or a Revisit scope walk."
  (clrhash org-air-query--file-meta)
  (clrhash org-air-query--visits)
  (clrhash org-air-query--denote-id-index)
  (setq org-air-query--link-graph-dirty nil))

(defmacro org-air-r59--with-corpus (specs &rest body)
  "Create a temp Org corpus from SPECS and run BODY against it.
SPECS is a list of (NAME . CONTENT) files written into a fresh temp
directory; NAME may carry subdirectories (created).  Binds
`org-air-files' to the directory, `org-air-inbox-file' to its
inbox.org, a temp `org-air-cache-file' and the 120x50 batch viewport.
Starts from EMPTY query tables and cleans up the tables, the scan work
buffer, the board buffer, every corpus-visiting buffer and the
directory."
  (declare (indent 1) (debug t))
  `(let* ((org-air-r59--dir (make-temp-file "org-air-r59-" t)))
     (unwind-protect
         (progn
           (org-air-r59--reset-tables)
           (pcase-dolist (`(,name . ,content) ,specs)
             (let ((path (expand-file-name name org-air-r59--dir))
                   (coding-system-for-write 'utf-8-unix))
               (make-directory (file-name-directory path) t)
               (write-region (or content "") nil path nil 'silent)))
           (let ((org-air-files (list org-air-r59--dir))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-r59--dir))
                 (org-air-cache-file
                  (expand-file-name ".cache/board.eld" org-air-r59--dir))
                 (org-air-view-width 120)
                 (org-air-view-height 50))
             (save-window-excursion
               ,@body)))
       (org-air-query-teardown)
       (org-air-r59--reset-tables)
       (when (get-buffer org-air-view-buffer-name)
         (let ((kill-buffer-query-functions nil))
           (kill-buffer org-air-view-buffer-name)))
       (let ((kill-buffer-query-functions nil))
         (dolist (buf (buffer-list))
           (let ((fn (buffer-file-name buf)))
             (when (and fn (string-prefix-p org-air-r59--dir fn))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r59--dir t))))

(defun org-air-r59--file (name)
  "Return the absolute path of corpus file NAME."
  (expand-file-name name org-air-r59--dir))

(defun org-air-r59--item (title items)
  "Return the item in ITEMS whose title contains TITLE; assert it exists."
  (let ((item (org-air-test-find-item title items)))
    (should item)
    item))

(defun org-air-r59--buckets (title items)
  "Classify the TITLE item from ITEMS at the frozen `org-air-test-now'."
  (org-air-classify-item (org-air-r59--item title items) org-air-test-now))

(defmacro org-air-r59--render-board (&rest body)
  "Render the real board over the bound corpus and run BODY in its buffer.
Frozen clock (`org-air-test-now'), the anti-tautology render guards and
the corpus 120x50 viewport; the board buffer is killed afterwards so a
warm in-buffer item cache never leaks between renders."
  (declare (indent 0) (debug t))
  `(org-air-viewport-test--with-frozen-now
     (unwind-protect
         (org-air-viewport-test--with-render-guards
           (org-air)
           (with-current-buffer org-air-view-buffer-name
             ,@body))
       (when (get-buffer org-air-view-buffer-name)
         (let ((kill-buffer-query-functions nil))
           (kill-buffer org-air-view-buffer-name))))))

(defun org-air-r59--board-titles ()
  "Return the titles of every rendered item ROW (the `org-air-item' walk).
Property-based, not substring-based, so chrome text can never mask a
missing/present row assertion."
  (let ((pos (point-min)) titles)
    (while (setq pos (text-property-not-all pos (point-max)
                                            'org-air-item nil))
      (let ((item (get-text-property pos 'org-air-item)))
        (cl-pushnew (org-air-item-title item) titles :test #'equal))
      (setq pos (next-single-property-change pos 'org-air-item
                                             nil (point-max))))
    (nreverse titles)))

(defun org-air-r59--badge (bucket)
  "Return the rendered count badge of BUCKET's section heading."
  (cdr (assq bucket (org-air-viewport-test-section-counts))))

(defconst org-air-r59--inbox-specs
  '(("inbox.org" .
     "#+title: Inbox\n\n\
* New\n\
:PROPERTIES:\n\
:CREATED: [2026-06-01 Mon 09:00]\n\
:END:\n\
Captured items get grouped under this heading.\n\
** TODO Set up syncthing :inbox:\n\
** TODO Set up n8n :inbox:\n"))
  "The reported shape, verbatim: a keyword-less dateless PARENT (with a
PROPERTIES drawer and a description paragraph — and an INACTIVE stamp
that must never count as an own active date) grouping two TODO captures
inside `org-air-inbox-file'.")

;;;; -------------------------------------------------------------------
;;;; r59-1 — T1: the reported inbox shape
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r59-1-inbox-container-skipped ()
  "T1: the user's `* New' grouping heading never renders in the Inbox.
The scan records `childp' t (and `own-active-ts' nil — the CREATED
drawer's inactive [ts] never counts); classify routes exactly
\(container) — NO 'inbox — while both TODO children keep 'inbox; the
rendered board's Inbox rows are the two child titles WITHOUT \"New\"
and the Inbox badge reads 2.  Reverting the container branch (the item
rides the R54-2 bypass into 'inbox again) FAILS."
  (skip-unless (locate-library "org-air"))
  (org-air-r59--with-corpus org-air-r59--inbox-specs
    (let* ((items (org-air-query-items))
           (new (org-air-r59--item "New" items)))
      ;; The scan-time container signals on the reported shape.
      (should (eq (org-air-item-kind new) 'heading))
      (should (org-air-item-childp new))
      (should-not (org-air-item-todo new))
      (should-not (org-air-item-scheduled new))
      (should-not (org-air-item-deadline new))
      (should-not (org-air-item-own-active-ts new))
      (should (org-air-query-container-item-p new))
      ;; Classify: the container bucket, nothing else — no 'inbox.
      (should (equal (org-air-classify-item new org-air-test-now)
                     '(container)))
      ;; The children ARE the content: both keep the 'inbox bucket.
      (should (memq 'inbox (org-air-r59--buckets "Set up syncthing" items)))
      (should (memq 'inbox (org-air-r59--buckets "Set up n8n" items)))
      (dolist (title '("Set up syncthing" "Set up n8n"))
        (should-not (org-air-query-container-item-p
                     (org-air-r59--item title items)))))
    ;; The rendered board: child rows in, the parent row out, badge 2.
    (org-air-r59--render-board
      (let ((titles (org-air-r59--board-titles)))
        (should (member "Set up syncthing" titles))
        (should (member "Set up n8n" titles))
        (should-not (member "New" titles)))
      (should (equal (org-air-r59--badge 'inbox) 2)))))

;;;; -------------------------------------------------------------------
;;;; r59-2 — T2: the general-board twin (outside the inbox)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r59-2-general-board-twin ()
  "T2: the same container shape in a NON-inbox file routes 'container.
It used to route 'knowledge — BOTH off-board, so the general board's
pixels never change (the knob-nil twin inside this test proves the old
'knowledge routing is exactly what nil restores).  The parent is absent
from the render while the TODO children keep their 'attention rows."
  (skip-unless (locate-library "org-air"))
  (org-air-r59--with-corpus
      '(("projects.org" .
         "* Paperwork pile\n\
:PROPERTIES:\n\
:CREATED: [2026-06-01 Mon 09:00]\n\
:END:\n\
Structure only — the children are the tasks.\n\
** TODO File the taxes\n\
** TODO Shred the papers\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((items (org-air-query-items))
           (pile (org-air-r59--item "Paperwork pile" items)))
      (should (org-air-item-childp pile))
      (should (equal (org-air-classify-item pile org-air-test-now)
                     '(container)))
      ;; Knob nil: the pre-R59 routing — 'knowledge, equally off-board.
      (let ((org-air-skip-container-headings nil))
        (should (equal (org-air-classify-item pile org-air-test-now)
                       '(knowledge))))
      ;; The children keep the full task treatment (dateless => attention).
      (should (memq 'attention (org-air-r59--buckets "File the taxes" items)))
      (should (memq 'attention (org-air-r59--buckets "Shred the papers" items))))
    (org-air-r59--render-board
      (let ((titles (org-air-r59--board-titles)))
        (should (member "File the taxes" titles))
        (should (member "Shred the papers" titles))
        (should-not (member "Paperwork pile" titles))))))

;;;; -------------------------------------------------------------------
;;;; r59-3 — T3: a TODO parent with children is still a task
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r59-3-task-parent-kept ()
  "T3: `* TODO Ship v1' WITH TODO children still classifies and renders.
`childp' is t, but the TODO keyword makes it a task, not a container —
an implementation that skips ALL parents FAILS here."
  (skip-unless (locate-library "org-air"))
  (org-air-r59--with-corpus
      '(("release.org" .
         "* TODO Ship v1\n\
** TODO Write the changelog\n\
** TODO Tag the release\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((items (org-air-query-items))
           (parent (org-air-r59--item "Ship v1" items)))
      (should (org-air-item-childp parent))
      (should (equal (org-air-item-todo parent) "TODO"))
      (should-not (org-air-query-container-item-p parent))
      (should (memq 'attention (org-air-classify-item parent org-air-test-now)))
      (should-not (memq 'container
                        (org-air-classify-item parent org-air-test-now))))
    (org-air-r59--render-board
      (let ((titles (org-air-r59--board-titles)))
        (should (member "Ship v1" titles))
        (should (member "Write the changelog" titles))))))

;;;; -------------------------------------------------------------------
;;;; r59-4 — T4: a parent with its OWN date is still a task
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r59-4-dated-parent-kept ()
  "T4: a keyword-less parent with its OWN SCHEDULED and children is kept.
The R54 date signal types it task, it classifies 'upcoming (scheduled
two days after the frozen now) and renders — fences \"TODO or a date =>
still a task\"."
  (skip-unless (locate-library "org-air"))
  (org-air-r59--with-corpus
      '(("milestones.org" .
         "* Milestone review\n\
SCHEDULED: <2026-06-17 Wed>\n\
** TODO Draft the agenda\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((items (org-air-query-items))
           (parent (org-air-r59--item "Milestone review" items)))
      (should (org-air-item-childp parent))
      (should (org-air-item-scheduled parent))
      (should (eq (org-air-item-ntype parent) 'task))
      (should-not (org-air-query-container-item-p parent))
      (should (memq 'upcoming (org-air-classify-item parent org-air-test-now))))
    (org-air-r59--render-board
      (should (member "Milestone review" (org-air-r59--board-titles))))))

;;;; -------------------------------------------------------------------
;;;; r59-5 — T5: the child-date trap (subtree vs own scoping)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r59-5-child-date-trap ()
  "T5: a child's SCHEDULED must not make the container test \"dated\".
The parent's subtree-wide `active-ts' IS non-nil and
`org-air-classify--stale-eligible-p' IS t — the trap proven LIVE — while
the own-scoped `own-active-ts' is nil and the parent still classifies
\(container).  An implementation testing `active-ts' (or reusing
`--stale-eligible-p') as the \"no date\" conjunct FAILS here."
  (skip-unless (locate-library "org-air"))
  (org-air-r59--with-corpus
      '(("trap.org" .
         "* New group\n\
Grouping prose only, no dates of its own.\n\
** TODO Child chore\n\
SCHEDULED: <2026-01-01 Thu>\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((items (org-air-query-items))
           (parent (org-air-r59--item "New group" items))
           (child (org-air-r59--item "Child chore" items)))
      ;; The trap is live: the SUBTREE-wide R54 slot sees the child's date…
      (should (org-air-item-active-ts parent))
      (should (org-air-classify--stale-eligible-p parent))
      ;; …the OWN-scoped twin does not, and the parent stays a container.
      (should-not (org-air-item-own-active-ts parent))
      (should (org-air-query-container-item-p parent))
      (should (equal (org-air-classify-item parent org-air-test-now)
                     '(container)))
      ;; The child's date belongs to the child's row (overdue => attention);
      ;; for a LEAF, own-active-ts is the active-ts value itself.
      (should (memq 'attention (org-air-classify-item child org-air-test-now)))
      (should (equal (org-air-item-own-active-ts child)
                     (org-air-item-active-ts child))))))

;;;; -------------------------------------------------------------------
;;;; r59-6 — T6: an OWN active date keeps the parent
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r59-6-own-active-date-kept ()
  "T6: an active <ts> in the parent's OWN body (above its first child)
fills `own-active-ts' and vetoes containerness.  The inbox variant rides
the R54-2 bypass and renders as an Inbox row; the non-inbox variant
routes 'knowledge (its R54 treatment) — neither is 'container."
  (skip-unless (locate-library "org-air"))
  (org-air-r59--with-corpus
      '(("inbox.org" .
         "#+title: Inbox\n\n\
* Waiting on the window\n\
Review window opens <2026-08-01 Sat>.\n\
** TODO Nested errand :inbox:\n")
        ("dated.org" .
         "* Dated section\n\
Kickoff <2026-08-01 Sat>.\n\
** Prose child\n\
Body.\n"))
    (let* ((items (org-air-query-items))
           (waiting (org-air-r59--item "Waiting on the window" items))
           (dated (org-air-r59--item "Dated section" items)))
      ;; Own-body active stamp => own-active-ts filled (= the subtree's).
      (should (org-air-item-childp waiting))
      (should (floatp (org-air-item-own-active-ts waiting)))
      (should (equal (org-air-item-own-active-ts waiting)
                     (org-air-item-active-ts waiting)))
      (should-not (org-air-query-container-item-p waiting))
      (should (memq 'inbox (org-air-classify-item waiting org-air-test-now)))
      ;; The non-inbox twin: NOT a container; its R54 type treatment.
      (should (floatp (org-air-item-own-active-ts dated)))
      (should-not (org-air-query-container-item-p dated))
      (should (equal (org-air-classify-item dated org-air-test-now)
                     '(knowledge))))
    (org-air-r59--render-board
      (should (member "Waiting on the window" (org-air-r59--board-titles))))))

;;;; -------------------------------------------------------------------
;;;; r59-7 — T7: the inbox LEAF rule
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r59-7-inbox-leaf-surfaces ()
  "T7: a keyword-less dateless LEAF in the inbox is a REAL triage item.
`childp' nil fails the container test, so the R54-2 bypass carries the
bare captured note into 'inbox exactly as before, and it renders as an
Inbox row — while a sibling container in the SAME file skips (badge 2:
the leaf + the container's TODO child).  An implementation that skips
keyword-less inbox LEAVES too FAILS here."
  (skip-unless (locate-library "org-air"))
  (org-air-r59--with-corpus
      '(("inbox.org" .
         "#+title: Inbox\n\n\
* Random thought\n\
Just a note-to-self to triage later.\n\
* New\n\
Grouping only.\n\
** TODO Grouped child :inbox:\n"))
    (let* ((items (org-air-query-items))
           (leaf (org-air-r59--item "Random thought" items)))
      (should-not (org-air-item-childp leaf))
      (should-not (org-air-query-container-item-p leaf))
      (should (equal (org-air-classify-item leaf org-air-test-now)
                     '(inbox)))
      (should (equal (org-air-r59--buckets "New" items) '(container))))
    (org-air-r59--render-board
      (let ((titles (org-air-r59--board-titles)))
        (should (member "Random thought" titles))
        (should (member "Grouped child" titles))
        (should-not (member "New" titles)))
      (should (equal (org-air-r59--badge 'inbox) 2)))))

;;;; -------------------------------------------------------------------
;;;; r59-8 — T8: the defcustom restores the pre-R59 behaviour
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r59-8-knob-restores-pre-r59 ()
  "T8: `org-air-skip-container-headings' nil restores today's rendering.
The SAME scanned `New' item re-classifies 'inbox under knob nil (the
predicate evaluates the knob LIVE over persisted slots — no rescan
needed), the rendered Inbox regains the `New' row (badge 3), and a
knob-nil re-scan reverts the F7 file vote: the GTD file types knowledge
again.  Reverting the knob gate FAILS every conjunct here."
  (skip-unless (locate-library "org-air"))
  (org-air-r59--with-corpus
      (append org-air-r59--inbox-specs
              '(("gtd.org" .
                 "* Projects\n\
** TODO Set up backups\n\
** TODO Rotate the keys\n")))
    (let* ((items (org-air-query-items))
           (new (org-air-r59--item "New" items))
           (gtd (org-air-r59--file "gtd.org")))
      ;; Knob t (default): the R59 behaviour — asserted non-vacuously.
      (should (equal (org-air-classify-item new org-air-test-now)
                     '(container)))
      (should (eq (plist-get (org-air-query-file-meta gtd) :ntype) 'task))
      (let ((org-air-skip-container-headings nil))
        ;; The SAME item object re-routes 'inbox — pre-R59 byte-for-byte.
        (should-not (org-air-query-container-item-p new))
        (should (equal (org-air-classify-item new org-air-test-now)
                       '(inbox)))
        ;; A knob-nil re-scan reverts the baked F7 vote too.
        (org-air-query-items)
        (should (eq (plist-get (org-air-query-file-meta gtd) :ntype)
                    'knowledge))))
    ;; The knob-nil RENDER: `New' is an Inbox row again, badge 3.
    (let ((org-air-skip-container-headings nil))
      (org-air-r59--render-board
        (let ((titles (org-air-r59--board-titles)))
          (should (member "New" titles))
          (should (member "Set up syncthing" titles))
          (should (member "Set up n8n" titles)))
        (should (equal (org-air-r59--badge 'inbox) 3))))))

;;;; -------------------------------------------------------------------
;;;; r59-9 — T9: the F7 file-ntype vote — containers abstain
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r59-9-f7-containers-abstain ()
  "T9: structure never votes in the F7 file-type rule.
A GTD file organised `* Projects' / `** TODO …' types TASK (off the
Revisit scope — `org-air-revisit--scope-entries' excludes it); a prose
file with sectioned subsections stays KNOWLEDGE (in scope); a KB file
containing one TODO leaf stays knowledge (the mixed-file rule intact);
and container-only quantification — zero non-container voters — never
yields task.  Reverting the abstention filter FAILS (the GTD file types
knowledge and re-enters Revisit)."
  (skip-unless (locate-library "org-air"))
  (org-air-r59--with-corpus
      '(("gtd.org" .
         "* Projects\n\
** TODO Set up backups\n\
** TODO Rotate the keys\n")
        ("prose.org" .
         "#+title: Field notes\n\n\
* Observations\n\
** Morning fog\n\
Prose paragraph.\n")
        ("kb.org" .
         "#+title: Kitchen sink\n\n\
* TODO One task leaf\n\
* Prose leaf\n\
Body.\n")
        ("inbox.org" . "#+title: inbox\n"))
    (org-air-query-items)
    (let ((gtd (org-air-r59--file "gtd.org"))
          (prose (org-air-r59--file "prose.org"))
          (kb (org-air-r59--file "kb.org")))
      ;; The vote: containers abstain; leaves decide.
      (should (eq (plist-get (org-air-query-file-meta gtd) :ntype) 'task))
      (should (eq (plist-get (org-air-query-file-meta prose) :ntype)
                  'knowledge))
      (should (eq (plist-get (org-air-query-file-meta kb) :ntype)
                  'knowledge))
      ;; Revisit scope follows the vote: GTD out, the notes in.
      (let ((scoped (mapcar #'car (org-air-revisit--scope-entries))))
        (should-not (member gtd scoped))
        (should (member prose scoped))
        (should (member kb scoped))))
    ;; Zero non-container voters can never quantify vacuously to 'task.
    (let ((container-only
           (org-air-item-create :title "Structure" :kind 'heading
                                :childp t)))
      (should (org-air-query-container-item-p container-only))
      (should (eq (org-air-query--file-ntype nil (list container-only))
                  'knowledge)))))

;;;; -------------------------------------------------------------------
;;;; r59-10 — T10: an explicit type override wins
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r59-10-override-wins ()
  "T10: the container shape plus an explicit task override stays a TASK.
`:ORG_AIR_TYPE: task' (and the `org-air-note-type-tag-alist' `:task:'
tag twin) force ntype 'task with nil todo/scheduled/deadline — exactly
the predicate's last conjunct — so the parent keeps the full task
treatment ('attention, rendered).  Reverting the ntype conjunct FAILS."
  (skip-unless (locate-library "org-air"))
  (org-air-r59--with-corpus
      '(("override-prop.org" .
         "* Prop forced group\n\
:PROPERTIES:\n\
:ORG_AIR_TYPE: task\n\
:END:\n\
Structure the user SAYS is a task.\n\
** TODO Prop child\n")
        ("override-tag.org" .
         "* Tag forced group :task:\n\
Prose.\n\
** TODO Tag child\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let ((items (org-air-query-items)))
      (dolist (title '("Prop forced group" "Tag forced group"))
        (let ((parent (org-air-r59--item title items)))
          (should (org-air-item-childp parent))
          (should-not (org-air-item-todo parent))
          (should (eq (org-air-item-ntype parent) 'task))
          (should-not (org-air-query-container-item-p parent))
          (should (memq 'attention
                        (org-air-classify-item parent org-air-test-now))))))
    (org-air-r59--render-board
      (let ((titles (org-air-r59--board-titles)))
        (should (member "Prop forced group" titles))
        (should (member "Tag forced group" titles))))))

;;;; -------------------------------------------------------------------
;;;; r59-11 — T11: the day view's Logged/created group
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r59-11-day-view-skips-container ()
  "T11: a container no longer duplicates its child under Logged/created.
The parent INHERITS the child's `:CREATED:' stamp through the
subtree-wide `subtree-ts' probe (proven: the slot is non-nil and keyed
to today) yet `org-air-view--day-groups' files ONLY the child; the
parent appears in NO day group.  Knob nil restores the duplicate parent
row (pre-R59).  Reverting the `--day-groups' filter FAILS."
  (skip-unless (locate-library "org-air"))
  (let ((today-drawer (format-time-string "[%Y-%m-%d %a 09:15]")))
    (org-air-r59--with-corpus
        `(("log.org" .
           ,(concat "* Capture group\n"
                    "Structure only.\n"
                    "** TODO Captured thing :log:\n"
                    ":PROPERTIES:\n"
                    ":CREATED: " today-drawer "\n"
                    ":END:\n"))
          ("inbox.org" . "#+title: inbox\n"))
      (let* ((items (org-air-query-items))
             (parent (org-air-test-find-item "Capture group" items))
             (child (org-air-test-find-item "Captured thing" items))
             (today-key (org-air-view--day-key (current-time))))
        (should parent)
        (should child)
        ;; The leak mechanism is live: the parent inherits the stamp.
        (should (org-air-item-subtree-ts parent))
        (should (equal (org-air-view--day-key
                        (org-air-item-subtree-ts parent))
                       today-key))
        (should (org-air-query-container-item-p parent))
        ;; The day view: the child files, the parent is in NO group.
        (let* ((groups (org-air-view--day-groups items (current-time)))
               (created (cdr (assoc "Logged / created" groups))))
          (should (memq child created))
          (should-not (memq parent created))
          (should-not (memq parent (cdr (assoc "Deadline" groups))))
          (should-not (memq parent (cdr (assoc "Scheduled" groups)))))
        ;; Knob nil: the pre-R59 duplicate parent row returns.
        (let ((org-air-skip-container-headings nil))
          (let* ((groups (org-air-view--day-groups items (current-time)))
                 (created (cdr (assoc "Logged / created" groups))))
            (should (memq child created))
            (should (memq parent created))))))))

;;;; -------------------------------------------------------------------
;;;; r59-12 — T12: capture + at-point items are unaffected
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r59-12-capture-and-at-point-unaffected ()
  "T12: `org-air-capture' items can never test as containers.
The capture writes `* TODO <title> :inbox:' + a CREATED drawer — always
a TODO keyword, always a leaf — so the captured item classifies 'inbox
and renders exactly as before.  An item built OUTSIDE the scan (the
at-point constructor, nil signal slots) is NOT a container even when
its real heading HAS children — the conservative default: when in
doubt, render."
  (skip-unless (locate-library "org-air"))
  (org-air-r59--with-corpus
      '(("inbox.org" . "#+title: org-air inbox\n"))
    (org-air-capture "Sync the drives")
    (let* ((items (org-air-query-items))
           (captured (org-air-r59--item "Sync the drives" items)))
      (should (equal (org-air-item-todo captured) "TODO"))
      (should-not (org-air-item-childp captured))
      (should-not (org-air-query-container-item-p captured))
      (should (memq 'inbox (org-air-classify-item captured org-air-test-now))))
    (org-air-r59--render-board
      (should (member "Sync the drives" (org-air-r59--board-titles)))))
  ;; The at-point constructor: nil slots => never a container, even on a
  ;; heading that HAS children in its buffer.
  (with-temp-buffer
    (delay-mode-hooks (org-mode))
    (insert "* Group parent\n** Child heading\nBody.\n")
    (goto-char (point-min))
    (let ((item (org-air-inbox--interactive-item)))
      (should (equal (org-air-item-title item) "Group parent"))
      (should-not (org-air-item-childp item))
      (should-not (org-air-item-ntype item))
      (should-not (org-air-query-container-item-p item))
      ;; nil ntype keeps the full task treatment (the R54 precedent).
      (should (memq 'attention
                    (org-air-classify-item item org-air-test-now))))))

;;;; -------------------------------------------------------------------
;;;; r59-13 — T13: current cache schema + the knob in the cache key
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r59-13-cache-v6-and-key ()
  "T13: the current schema and knob-bearing key remain revert-fenced.
`org-air-view--cache-version' is 6 (the native R90 re-bless below); the knob is
the FOURTH `org-air-view--cache-key' element (tracking the live value
both ways); a crafted v4 cache with the CURRENT key is a clean cold miss
\(nil read, nil load, no error — reverting the version bump hydrates it
and FAILS); a real live-version cache roundtrips `childp'/`own-active-ts'
write->read->classify with scan-identical buckets; a cache written under
knob t does NOT hydrate under knob nil (reverting the key element
hydrates it and FAILS — the baked F7 vote would go stale across a flip).
R60 re-bless (air/v0.5/org-air-round60-design.org R60-3, honest — no
conjunct weakened): `org-air-exclude-regexps' is the FIFTH key element.
The re-blessed shape assertion still MEANS something: the fifth element
must TRACK the live exclude set both ways (nil default and a let-bound
set) and DETECT a flip (different exclude sets compare un-`equal'), and
a cache written under nil excludes must NOT hydrate under a non-nil set
\(the exact mirror of the R59 knob conjunct — reverting the R60 key
extension hydrates it and FAILS).
R61 re-bless (air/v0.5/org-air-round61-design.org R61-2, honest — no
conjunct weakened): the version is 6 (the `org-air-item' struct gained
the four review harvest slots clocks/logs/created/rtrunc — a v5 record
has the wrong record length) and the key is SIX elements —
`org-air-log-cap' is the SIXTH.  The sixth element must TRACK the live
cap both ways (the default and a let-bound value) and DETECT a flip
\(different caps compare un-`equal'), and a cache written under the
default cap must NOT hydrate under a changed cap (the cap shapes the
scanned-and-persisted clocks/logs slots — reverting the R61 key
extension hydrates it and FAILS).  The exclude-set conjuncts above are
all KEPT: exclude is still detected, at the same (fifth) seat.
R77 re-bless (air/v0.1/org-air-round77-design.org, honest — no
conjunct weakened): the key is SEVEN elements now —
`org-air-task-requires-todo' is the SEVENTH.  The seventh element must
TRACK the live knob both ways (the nil default and a let-bound t) and
DETECT a flip (different knob values compare un-`equal'), and a cache
written under the nil default must NOT hydrate under the knob (the
knob shapes scan-time `ntype' and the baked file-meta `:ntype' —
reverting the R77 key extension hydrates it and FAILS).  The R59-knob,
exclude-set and log-cap conjuncts above are all KEPT, each at its same
\(fourth/fifth/sixth) seat.
R90 final re-bless: version 6 remains the released native title/tag contract.
The experimental v7 broad projection was discarded before integration; the
dedicated R90 cache test pins native roundtrip plus a clean miss for that
unshipped payload.  This historical test keeps every older shape/key fence
unchanged."
  (skip-unless (locate-library "org-air"))
  (org-air-r59--with-corpus
      (append org-air-r59--inbox-specs
              '(("dated.org" .
                 "* Dated section\n\
Kickoff <2026-08-01 Sat>.\n\
** Prose child\n\
Body.\n")))
    ;; The version and the key shape (R60: the exclude set is the FIFTH
    ;; element; R61: `org-air-log-cap' is the SIXTH; R77:
    ;; `org-air-task-requires-todo' is the SEVENTH; this corpus runs at
    ;; the nil-exclude / default-cap / nil-knob baseline).
    (should (= org-air-view--cache-version 6))
    (let ((key (org-air-view--cache-key)))
      (should (= (length key) 7))
      (should (eq (nth 3 key) t))
      ;; The fifth element IS the live exclude set (nil here)…
      (should (eq (nth 4 key) org-air-exclude-regexps))
      ;; …tracks a let-bound set, and DETECTS the flip: different
      ;; exclude sets are different keys (the R57 "the key IS the
      ;; detector" discipline the R60 element extends).
      (let ((org-air-exclude-regexps '("/archive/")))
        (should (equal (nth 4 (org-air-view--cache-key)) '("/archive/")))
        (should-not (equal (org-air-view--cache-key) key)))
      ;; R61: the sixth element IS the live `org-air-log-cap'…
      (should (eq (nth 5 key) org-air-log-cap))
      ;; …tracks a let-bound cap, and DETECTS the flip the same way.
      (let ((org-air-log-cap 123))
        (should (equal (nth 5 (org-air-view--cache-key)) 123))
        (should-not (equal (org-air-view--cache-key) key)))
      ;; R77: the seventh element IS the live `org-air-task-requires-todo'
      ;; (nil here — the default)…
      (should (eq (nth 6 key) org-air-task-requires-todo))
      (should (null (nth 6 key)))
      ;; …tracks a let-bound knob, and DETECTS the flip the same way.
      (let ((org-air-task-requires-todo t))
        (should (eq (nth 6 (org-air-view--cache-key)) t))
        (should-not (equal (org-air-view--cache-key) key))))
    (let ((org-air-skip-container-headings nil))
      (should (eq (nth 3 (org-air-view--cache-key)) nil)))
    ;; Scan, snapshot the ground truth, persist.
    (let* ((files (org-air-query-files))
           (items (org-air-query-items))
           (new (org-air-r59--item "New" items))
           (dated (org-air-r59--item "Dated section" items))
           (own-ts (org-air-item-own-active-ts dated)))
      (should (equal (org-air-classify-item new org-air-test-now)
                     '(container)))
      (should (floatp own-ts))
      (org-air-view--cache-write items (org-air-view--mtimes-snapshot files))
      ;; Live-version (v6) roundtrip: the container signals survive
      ;; write->read->classify.
      (let* ((data (org-air-view--cache-read))
             (hydrated (plist-get data :items)))
        (should data)
        (should (consp hydrated))
        (let ((new-h (org-air-r59--item "New" hydrated))
              (dated-h (org-air-r59--item "Dated section" hydrated)))
          (should (org-air-item-childp new-h))
          (should-not (org-air-item-own-active-ts new-h))
          (should (equal (org-air-classify-item new-h org-air-test-now)
                         '(container)))
          (should (equal (org-air-item-own-active-ts dated-h) own-ts))
          (should-not (org-air-query-container-item-p dated-h)))
        ;; Every hydrated item classifies scan-identically (T1-identical).
        (dolist (item items)
          (let ((twin (org-air-r59--item (org-air-item-title item) hydrated)))
            (should (equal (org-air-classify-item twin org-air-test-now)
                           (org-air-classify-item item org-air-test-now))))))
      ;; The knob joins the key: a knob-t cache never hydrates under nil…
      (let ((org-air-skip-container-headings nil))
        (should-not (org-air-view--cache-read))
        (should-not (org-air-view--cache-load)))
      ;; …while the same knob still hydrates (the miss above is the KEY).
      (should (org-air-view--cache-read))
      ;; R60: the exclude set detects the same way — this cache was
      ;; written under nil excludes, so it never hydrates under a
      ;; non-nil set (toggling the knob can never serve a stale set)…
      (let ((org-air-exclude-regexps '("/archive/")))
        (should-not (org-air-view--cache-read))
        (should-not (org-air-view--cache-load)))
      ;; …while the original (nil) exclude set still hydrates.
      (should (org-air-view--cache-read))
      ;; R61: `org-air-log-cap' detects the same way — this cache was
      ;; written under the default cap, so it never hydrates under a
      ;; changed cap (the cap shapes the persisted clocks/logs slots)…
      (let ((org-air-log-cap 123))
        (should-not (org-air-view--cache-read))
        (should-not (org-air-view--cache-load)))
      ;; …while the original cap still hydrates.
      (should (org-air-view--cache-read))
      ;; R77: `org-air-task-requires-todo' detects the same way — this
      ;; cache was written under the nil default, so it never hydrates
      ;; under the knob (the baked ntype/file-meta :ntype task/knowledge
      ;; split would go stale across a flip)…
      (let ((org-air-task-requires-todo t))
        (should-not (org-air-view--cache-read))
        (should-not (org-air-view--cache-load)))
      ;; …while the original (nil) knob still hydrates.
      (should (org-air-view--cache-read))
      ;; A v4 cache (the pre-R59 struct shape) is a clean cold miss even
      ;; with the CURRENT key: no hydration, no error, no hang.
      (let ((print-length nil) (print-level nil))
        (write-region
         (prin1-to-string
          (list :version 4
                :key (org-air-view--cache-key)
                :mtimes nil :file-meta nil :visits nil :items nil))
         nil (expand-file-name org-air-cache-file) nil 'silent))
      (should-not (org-air-view--cache-read))
      (should-not (org-air-view--cache-load)))))

;;;; -------------------------------------------------------------------
;;;; r59-14 — T14: data purity over cache-hydrated containers
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r59-14-data-pure-render ()
  "T14: the container predicate is slot reads — rendering opens NOTHING.
A full board render + a day grouping over CACHE-HYDRATED items (every
marker a durable (FILE . POS) cons) with a live container present calls
`find-file-noselect' ZERO times, leaves the visible `buffer-list'
unchanged and no buffer visiting any corpus file — while the render IS
the R59 render (children rows in, the container row out)."
  (skip-unless (locate-library "org-air"))
  (org-air-r59--with-corpus org-air-r59--inbox-specs
    ;; Previous session: scan + persist, then vanish.
    (let* ((files (org-air-query-files))
           (items (org-air-query-items)))
      (org-air-view--cache-write items (org-air-view--mtimes-snapshot files)))
    (org-air-query-teardown)
    ;; Fresh session: hydrate and paint under the spies.
    (let* ((data (org-air-view--cache-read))
           (hydrated (plist-get data :items)))
      (should (consp hydrated))
      (dolist (item hydrated)
        (should (consp (org-air-item-marker item))))
      ;; Anti-vacuous: a live container is in the hydrated set.
      (should (org-air-query-container-item-p
               (org-air-r59--item "New" hydrated)))
      (let ((org-air-view-buffer-name "*org-air-r59*")
            (ffns-calls 0))
        (unwind-protect
            (with-current-buffer (get-buffer-create "*org-air-r59*")
              (unless (derived-mode-p 'org-air-view-mode)
                (org-air-view-mode))
              (setq org-air-view--items hydrated)
              (let ((visible-before
                     (seq-remove (lambda (b)
                                   (string-prefix-p " " (buffer-name b)))
                                 (buffer-list))))
                (cl-letf* ((orig (symbol-function 'find-file-noselect))
                           ((symbol-function 'find-file-noselect)
                            (lambda (&rest args)
                              (cl-incf ffns-calls)
                              (apply orig args))))
                  (let ((org-air-show-inspector nil))
                    (org-air-view--render hydrated nil))
                  (org-air-view--day-groups hydrated (current-time))
                  (should (= ffns-calls 0)))
                ;; The paint IS the R59 board: children in, container out.
                (let ((titles (org-air-r59--board-titles)))
                  (should (member "Set up syncthing" titles))
                  (should-not (member "New" titles)))
                ;; No visible buffer appeared; no corpus file is visited.
                (should (equal (seq-remove
                                (lambda (b)
                                  (string-prefix-p " " (buffer-name b)))
                                (buffer-list))
                               visible-before))
                (dolist (f (org-air-query-files))
                  (should-not (get-file-buffer f)))))
          (when (get-buffer "*org-air-r59*")
            (let ((kill-buffer-query-functions nil))
              (kill-buffer "*org-air-r59*"))))))))

(provide 'org-air-round59-test)
;;; org-air-round59-test.el ends here
