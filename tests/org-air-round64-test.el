;;; org-air-round64-test.el --- executing ERTs for v0.5 round-64 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.5 round-64 (air/v0.5/org-air-round64-design.org):
;; the ONE-SHOT inbox refile — the superset non-interactive engine
;; (`org-air-refile-item' with outline-PATH targets, missing parents
;; created root-down via `org-refile-new-child', the `org-paste-subtree'
;; re-leveled insert, TODO/PRIORITY args), the cached two-stage f/p
;; destination picker (creation DEFERRED to execute), and the batch-safe
;; parts of the transient shell.  All BATCH/headless: every test drives
;; the NON-INTERACTIVE `org-air-refile-item' and the named reader
;; functions — the transient UI polish itself is the round's one
;; screenshot-confirm residue and is deliberately NOT event-looped here.
;; Reverting the R64 impl fails each (all eight verified RED against the
;; pre-impl trunk kklxowyq in a scratch workspace):
;;
;;   r64-1 NESTED CREATION (T1/T2) — an outline path whose parents are
;;         ABSENT creates them (`* Infra' › `** Cloud' under the target
;;         file, exactly ONE of each, keyword-less R59 containers) and
;;         files the item under the deepest; the same path AGAIN creates
;;         nothing (no duplicates, last-child placement); a THIRD call
;;         files a CHILD under a heading a PRIOR call moved into place
;;         (n8n › daily automation).  The on-disk tree is asserted.
;;   r64-2 THE WORKED SCENARIO (T5) — the spec's three acceptance calls
;;         verbatim: syncthing → Infra/Cloud (created) with tag +
;;         schedule, n8n → the same place, daily automation → CHILD of
;;         n8n; the exact final nested structure + tags + the ONE
;;         SCHEDULED stamp asserted on disk, the `inbox' tag gone from
;;         all three, the emptied `* New' container still in the inbox.
;;   r64-3 ONE-CALL METADATA (T4) — a SINGLE call sets olp + tags +
;;         category + schedule + todo + priority together (the superset
;;         signature); all six asserted on disk (keyword, `[#B]', tags,
;;         `:CATEGORY:', SCHEDULED, ancestry + level) — nothing dropped.
;;   r64-4 SIBLING-INSERT BUG (T3) — the measured defect: a level-2
;;         inbox item refiled "under" the level-2 `Cloud' heading lands
;;         as Cloud's CHILD (level 3, last), never its sibling; an item
;;         that ITSELF has a child re-levels both (2/3 → 4/5) under a
;;         level-3 parent — the `org-paste-subtree' path files at the
;;         correct DEPTH for all callers.
;;   r64-5 NO-SCAN (T6, the R53 re-pin) — with a seeded board-items
;;         cache the `f' candidates, the tag/category vocabularies and
;;         the R64-2 path table build under spies on
;;         `org-air-query--scan-file' / `org-air-query-items' /
;;         `org-air-query-items-in-files' / `org-air-query-files' at
;;         ZERO calls; the path stage opens exactly ONE file — the
;;         destination (`find-file-noselect' spy, distinct files); the
;;         cache fallback (no in-memory items) still scans nothing.
;;   r64-6 SIGNATURE COMPATIBILITY (T7) — `(item file)' ⇒ file end
;;         (the R53-6 headingless-note call shape re-run; the nil paste
;;         level is 1, the called-out promotion); `(item file
;;         "Existing")' ⇒ filed under it AT child level (string ≡
;;         one-segment path — the level fix applies); `"Missing"' ⇒
;;         CREATED at top level, never the silent file-end fallback.
;;   r64-7 THE SHELL, batch-safe parts (T8) — `org-air-refile-item'
;;         remains a command and the board's `r' binding;
;;         `org-air-refile-transient' is a command; the `p' reader
;;         parses "A/B" → (:olp ("A" "B") :new 2) against a fixture
;;         table, tolerates a trailing `/', answers `:olp' nil for
;;         empty input, and matches a `/'-NAMED existing heading as a
;;         TABLE entry first; the `f' reader resolves a DISAMBIGUATED
;;         `⌂' candidate to the real file (the R19-2 guarantee,
;;         re-homed — the retired action menu's residue).
;;   r64-8 R57 TODO VOCAB (T9) — a destination file with `#+TODO: TODO
;;         HOLD | CLOSED' yields exactly ITS merged keywords from the
;;         `k' vocabulary reader; a TODO arg of "CLOSED" round-trips
;;         on-disk with `donep' semantics intact (the file's own done
;;         set, never org-air's).
;;   r64-9 CRASH-SAFE MOVE (harden) — a paste-step failure AFTER the
;;         destructive cut restores the source subtree byte-identically
;;         (item never lost) and leaves the target file untouched on
;;         disk (not half-written); the error still propagates and the
;;         same refile succeeds afterwards.
;;
;; RETIREMENT LEDGER (the four flagged legacy ERTs, spec R64-3): the
;; R19-2 stub-chain test and the two `--decode-target' tests are
;; RE-BLESSED in place (tests/org-air-round19-test.el /
;; tests/org-air-round20-test.el) to the surviving reader/form seams;
;; `org-air-r20-4-refile-menu-is-action-first-move-leads' is REMOVED —
;; the action-first menu SHAPE it pinned has no successor (the form
;; shows every field at once), and each of its surviving guarantees is
;; asserted at its new home (see the comment left in its place).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'org-air-test-helpers)

(when (locate-library "org-air")
  (require 'org-air))

;;;; -------------------------------------------------------------------
;;;; Corpus / board scaffolding (the R53 harness shape)
;;;; -------------------------------------------------------------------

(defvar org-air-r64--dir nil
  "The temp corpus directory of the current `org-air-r64--with-corpus'.")

(defmacro org-air-r64--with-corpus (specs &rest body)
  "Create a temp Org corpus from SPECS and run BODY against it.
SPECS is a list of (NAME . CONTENT) files written into a fresh temp
directory.  Binds `org-air-files' to the directory, `org-air-inbox-file'
to its inbox.org, a temp `org-air-cache-file', a DEAD
`org-air-view-buffer-name' (so picker data never leaks in from another
suite's live board — `org-air-r64--with-board' rebinds it live) and the
standard batch board geometry.  Cleans up every corpus-visiting buffer,
the scan work buffer and the directory."
  (declare (indent 1) (debug t))
  `(let* ((org-air-r64--dir (make-temp-file "org-air-r64-" t)))
     (unwind-protect
         (progn
           (pcase-dolist (`(,name . ,content) ,specs)
             (let ((file-name-handler-alist nil)
                   (coding-system-for-write 'utf-8-unix))
               (write-region (or content "") nil
                             (expand-file-name name org-air-r64--dir)
                             nil 'silent)))
           (let ((org-air-files (list org-air-r64--dir))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-r64--dir))
                 (org-air-cache-file
                  (expand-file-name ".cache/board.eld" org-air-r64--dir))
                 (org-air-view-buffer-name "*org-air-r64-no-board*")
                 (org-air-view-width 120)
                 (org-air-view-height 50))
             ,@body))
       (org-air-query-teardown)
       (let ((kill-buffer-query-functions nil))
         (dolist (buf (buffer-list))
           (let ((fn (buffer-file-name buf)))
             (when (and fn (string-prefix-p org-air-r64--dir fn))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r64--dir t))))

(defmacro org-air-r64--with-board (&rest body)
  "Run BODY in a fresh `org-air-view-mode' board buffer (killed after)."
  (declare (indent 0) (debug t))
  `(let ((org-air-view-buffer-name "*org-air-r64*"))
     (unwind-protect
         (with-current-buffer (get-buffer-create org-air-view-buffer-name)
           (unless (derived-mode-p 'org-air-view-mode) (org-air-view-mode))
           ,@body)
       (when (get-buffer "*org-air-r64*")
         (let ((kill-buffer-query-functions nil))
           (kill-buffer "*org-air-r64*"))))))

(defvar org-air-r64--scan-calls 0
  "Calls to `org-air-query--scan-file' while counting (the T6 spy).")
(defvar org-air-r64--items-calls 0
  "Calls to `org-air-query-items' / `-in-files' while counting.")
(defvar org-air-r64--enum-calls 0
  "Calls to `org-air-query-files' (a full enumeration) while counting.")
(defvar org-air-r64--ffns-files nil
  "FILE arguments handed to `find-file-noselect' while counting.")

(defmacro org-air-r64--counting (&rest body)
  "Run BODY with the T6 scan / enumeration / file-open spies installed."
  (declare (indent 0) (debug t))
  `(let ((org-air-r64--scan-calls 0)
         (org-air-r64--items-calls 0)
         (org-air-r64--enum-calls 0)
         (org-air-r64--ffns-files nil))
     (cl-letf* ((scan-orig (symbol-function 'org-air-query--scan-file))
                ((symbol-function 'org-air-query--scan-file)
                 (lambda (&rest args)
                   (cl-incf org-air-r64--scan-calls)
                   (apply scan-orig args)))
                (items-orig (symbol-function 'org-air-query-items))
                ((symbol-function 'org-air-query-items)
                 (lambda (&rest args)
                   (cl-incf org-air-r64--items-calls)
                   (apply items-orig args)))
                (inf-orig (symbol-function 'org-air-query-items-in-files))
                ((symbol-function 'org-air-query-items-in-files)
                 (lambda (&rest args)
                   (cl-incf org-air-r64--items-calls)
                   (apply inf-orig args)))
                (files-orig (symbol-function 'org-air-query-files))
                ((symbol-function 'org-air-query-files)
                 (lambda (&rest args)
                   (cl-incf org-air-r64--enum-calls)
                   (apply files-orig args)))
                (ffns-orig (symbol-function 'find-file-noselect))
                ((symbol-function 'find-file-noselect)
                 (lambda (file &rest args)
                   (push file org-air-r64--ffns-files)
                   (apply ffns-orig file args))))
       ,@body)))

(defun org-air-r64--item (file title)
  "Build a minimal refile item for the heading titled TITLE in FILE.
Positions a fresh marker on the heading (first match in buffer order),
so calls in a sequence always see the CURRENT on-disk layout."
  (with-current-buffer (find-file-noselect file)
    (org-with-wide-buffer
     (goto-char (point-min))
     (re-search-forward (format org-complex-heading-regexp-format
                                (regexp-quote title)))
     (goto-char (match-beginning 0))
     (org-air-item-create
      :title title
      :tags (org-get-tags nil t)
      :todo (org-get-todo-state)
      :file file
      :marker (point-marker)))))

(defun org-air-r64--headings (file)
  "Return FILE's on-disk headings as (LEVEL . HEADLINE), buffer order.
HEADLINE is the raw headline text after the stars with tag-alignment
runs collapsed to single spaces and trailing whitespace dropped — so a
tree can be asserted with plain `equal', alignment-independent."
  (with-temp-buffer
    (insert-file-contents file)
    (let (out)
      (goto-char (point-min))
      (while (re-search-forward "^\\(\\*+\\) +\\(.*?\\)[ \t]*$" nil t)
        (push (cons (length (match-string 1))
                    (replace-regexp-in-string "[ \t]+" " " (match-string 2)))
              out))
      (nreverse out))))

(defun org-air-r64--text (file)
  "Return FILE's on-disk content as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defconst org-air-r64--acceptance-inbox
  "#+title: Inbox\n* New\n** TODO Set up syncthing :inbox:\n** TODO Set up n8n :inbox:\n** TODO some daily automation with n8n :inbox:\n"
  "The R59 user's real inbox shape: level-2 captures under `* New'.")

(defconst org-air-r64--acceptance-projects
  "#+title: projects\n* Website relaunch\n** TODO Fix nav\n"
  "The acceptance target file: no `Infra' anywhere.")

;;;; -------------------------------------------------------------------
;;;; r64-1 — nested parents created when absent, then chained (T1/T2)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r64-1-nested-parents-created-and-chained ()
  "Non-interactive `org-air-refile-item' with TARGET-HEADING
'(\"Infra\" \"Cloud\") into a file containing NEITHER creates both
(`* Infra' › `** Cloud', exactly one of each — keyword-less bare
headlines, the R59 container guarantee, no timestamp anywhere) and
files the item under the deepest at parent-level+1.  The same path in a
SECOND call creates NOTHING (still one Infra, one Cloud) and places the
second item as a sibling AFTER the first (the last-child rule).  A
THIRD call files a CHILD under the heading the second call moved into
place (n8n › daily automation) — a just-filed item is a real path
target immediately.  The full on-disk tree is asserted; the emptied
`* New' container stays in the inbox.  RED pre-R64: the list target
signals `wrong-type-argument' against the string-only resolver."
  (skip-unless (locate-library "org-air"))
  (org-air-r64--with-corpus
      `(("inbox.org" . ,org-air-r64--acceptance-inbox)
        ("projects.org" . ,org-air-r64--acceptance-projects))
    (let ((projects (expand-file-name "projects.org" org-air-r64--dir))
          (inhibit-message t))
      ;; call 1: neither parent exists — both created, item under Cloud.
      (org-air-refile-item
       (org-air-r64--item org-air-inbox-file "Set up syncthing")
       projects '("Infra" "Cloud"))
      (should (equal (org-air-r64--headings projects)
                     '((1 . "Website relaunch")
                       (2 . "TODO Fix nav")
                       (1 . "Infra")
                       (2 . "Cloud")
                       (3 . "TODO Set up syncthing :inbox:"))))
      ;; created parents are R59 containers: the bare headlines above
      ;; carry no keyword; no timestamp was written anywhere.
      (should-not (string-match-p "<2[0-9]\\{3\\}-" (org-air-r64--text projects)))
      ;; call 2: the SAME path — no duplicates, last-child placement.
      (org-air-refile-item
       (org-air-r64--item org-air-inbox-file "Set up n8n")
       projects '("Infra" "Cloud"))
      ;; call 3: a CHILD under the heading call 2 filed (n8n › daily).
      (org-air-refile-item
       (org-air-r64--item org-air-inbox-file "some daily automation with n8n")
       projects '("Infra" "Cloud" "Set up n8n"))
      (should (equal (org-air-r64--headings projects)
                     '((1 . "Website relaunch")
                       (2 . "TODO Fix nav")
                       (1 . "Infra")
                       (2 . "Cloud")
                       (3 . "TODO Set up syncthing :inbox:")
                       (3 . "TODO Set up n8n :inbox:")
                       (4 . "TODO some daily automation with n8n :inbox:"))))
      ;; the source items are gone; the emptied container remains.
      (should (equal (org-air-r64--headings org-air-inbox-file)
                     '((1 . "New")))))))

;;;; -------------------------------------------------------------------
;;;; r64-2 — the acceptance mirror: the worked scenario end-to-end (T5)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r64-2-acceptance-scenario-on-disk ()
  "The spec's worked example as its THREE engine calls (the exact args
the transient's one confirm collects): syncthing → Infra › Cloud
(created) with '(\"selfhosted\") + \"+1w\"; n8n → the same place; daily
automation → CHILD of the n8n heading just filed.  The final
projects.org tree is asserted verbatim modulo the timestamp — every
item re-leveled to parent+1 (2→3, 2→4 for the child move), created
parents keyword-less, the `inbox' tag gone from all three (replace /
`:none' clear semantics) — plus exactly ONE SCHEDULED stamp, sitting
under syncthing.  The inbox keeps its emptied `* New' container."
  (skip-unless (locate-library "org-air"))
  (org-air-r64--with-corpus
      `(("inbox.org" . ,org-air-r64--acceptance-inbox)
        ("projects.org" . ,org-air-r64--acceptance-projects))
    (let ((projects (expand-file-name "projects.org" org-air-r64--dir))
          (inhibit-message t))
      ;; move 1: r · f proj · p Infra/Cloud · t selfhosted · s this week · RET
      (org-air-refile-item
       (org-air-r64--item org-air-inbox-file "Set up syncthing")
       projects '("Infra" "Cloud") '("selfhosted") "+1w" nil)
      ;; move 2: r l RET (same destination; the form's tag pre-fill
      ;; minus `inbox' executes as a clear).
      (org-air-refile-item
       (org-air-r64--item org-air-inbox-file "Set up n8n")
       projects '("Infra" "Cloud") :none)
      ;; move 3: r · l · p …/Set up n8n · RET — child of the new heading.
      (org-air-refile-item
       (org-air-r64--item org-air-inbox-file "some daily automation with n8n")
       projects '("Infra" "Cloud" "Set up n8n") :none)
      ;; the exact final tree (T5), modulo tag alignment + the stamp.
      (should (equal (org-air-r64--headings projects)
                     '((1 . "Website relaunch")
                       (2 . "TODO Fix nav")
                       (1 . "Infra")
                       (2 . "Cloud")
                       (3 . "TODO Set up syncthing :selfhosted:")
                       (3 . "TODO Set up n8n")
                       (4 . "TODO some daily automation with n8n"))))
      (let ((text (org-air-r64--text projects)))
        ;; exactly ONE schedule, directly under syncthing.
        (should (string-match
                 "^\\*\\*\\* TODO Set up syncthing.*\n[ \t]*SCHEDULED: <2[0-9]\\{3\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]\\{2,3\\}>"
                 text))
        (should (= 1 (with-temp-buffer
                       (insert text)
                       (count-matches "SCHEDULED:" (point-min) (point-max)))))
        ;; the `inbox' tag left with the inbox.
        (should-not (string-match-p ":inbox:" text)))
      (should (equal (org-air-r64--headings org-air-inbox-file)
                     '((1 . "New")))))))

;;;; -------------------------------------------------------------------
;;;; r64-3 — one call sets everything (T4, the superset signature)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r64-3-one-call-sets-everything ()
  "A SINGLE `org-air-refile-item' call with olp + tags + category +
schedule + todo + priority applies ALL SIX on the moved heading —
keyword DONE, `[#B]', both tags, `:CATEGORY:', the SCHEDULED date and
the (\"Ops\" \"Weekly\") ancestry at level 3 — asserted on disk through
a fresh `org-mode' parse.  Nothing is dropped and nothing needs a
second invocation (the 3-4-invocations class is dead at the engine
level).  RED pre-R64: no TODO/PRIORITY parameters exist
(`wrong-number-of-arguments')."
  (skip-unless (locate-library "org-air"))
  (org-air-r64--with-corpus
      '(("inbox.org" . "#+title: Inbox\n* New\n** TODO Pay invoice :inbox:\n")
        ("ops.org" . "#+title: ops\n"))
    (let ((ops (expand-file-name "ops.org" org-air-r64--dir))
          (inhibit-message t))
      (org-air-refile-item
       (org-air-r64--item org-air-inbox-file "Pay invoice")
       ops '("Ops" "Weekly") '("alpha" "beta") "2026-08-01" "opscat"
       "DONE" "B")
      (with-temp-buffer
        (insert-file-contents ops)
        (org-mode)
        (goto-char (point-min))
        (re-search-forward "Pay invoice")
        (org-back-to-heading t)
        ;; keyword + priority cookie + title on the headline itself.
        (should (looking-at "^\\*\\*\\* DONE \\[#B\\] Pay invoice"))
        (should (equal (org-get-todo-state) "DONE"))
        (should (equal (sort (org-get-tags nil t) #'string<)
                       '("alpha" "beta")))
        (should (equal (org-entry-get nil "CATEGORY") "opscat"))
        (let ((sched (org-entry-get nil "SCHEDULED")))
          (should sched)
          (should (string-match-p "2026-08-01" sched)))
        (should (equal (org-get-outline-path) '("Ops" "Weekly")))
        (should (= (org-outline-level) 3)))
      ;; the created ancestry is keyword-less container structure.
      (should (equal (seq-take (org-air-r64--headings ops) 2)
                     '((1 . "Ops") (2 . "Weekly")))))))

;;;; -------------------------------------------------------------------
;;;; r64-4 — the measured sibling-level insert bug is dead (T3)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r64-4-sibling-level-bug-is-dead ()
  "The measured pre-R64 defect (probe-level.el, spec Summary): a level-2
inbox item (the R59 `* New' container shape) refiled \"under\" the
level-2 `Cloud' heading kept its SOURCE stars and landed as Cloud's
level-2 SIBLING.  The `org-paste-subtree' re-level path files at the
correct DEPTH: the item lands at level 3 as Cloud's LAST child (after
the existing task), and an item that ITSELF has a child re-levels both
together (2/3 → 4/5) when filed under a level-3 parent — a child
target lands as a CHILD, never a sibling, for string AND path targets."
  (skip-unless (locate-library "org-air"))
  (org-air-r64--with-corpus
      '(("inbox.org" . "#+title: Inbox\n* New\n** TODO Set up syncthing :inbox:\n** TODO automation parent :inbox:\n*** child note\n")
        ("infra.org" . "#+title: infra\n* Infra\n** Cloud\n*** Existing task\n"))
    (let ((infra (expand-file-name "infra.org" org-air-r64--dir))
          (inhibit-message t))
      ;; the exact measured shape: level-2 item, STRING target "Cloud".
      (org-air-refile-item
       (org-air-r64--item org-air-inbox-file "Set up syncthing")
       infra "Cloud")
      (should (equal (org-air-r64--headings infra)
                     '((1 . "Infra")
                       (2 . "Cloud")
                       (3 . "Existing task")
                       (3 . "TODO Set up syncthing :inbox:"))))
      ;; an item WITH its own child under a level-3 parent: 2/3 → 4/5.
      (org-air-refile-item
       (org-air-r64--item org-air-inbox-file "automation parent")
       infra '("Infra" "Cloud" "Existing task"))
      (should (equal (org-air-r64--headings infra)
                     '((1 . "Infra")
                       (2 . "Cloud")
                       (3 . "Existing task")
                       (4 . "TODO automation parent :inbox:")
                       (5 . "child note")
                       (3 . "TODO Set up syncthing :inbox:")))))))

;;;; -------------------------------------------------------------------
;;;; r64-5 — pickers never scan (T6, the R53 re-pin)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r64-5-pickers-never-scan ()
  "With a seeded board-items cache, building the `f' candidates, the
tag/category vocabularies (through the REAL CRM editors, readers
stubbed) and the R64-2 path table runs ZERO fresh scans — spies on
`org-air-query--scan-file', `org-air-query-items',
`org-air-query-items-in-files' AND `org-air-query-files' all count 0 at
menu time — and the path-table build opens exactly ONE file: the
destination (`find-file-noselect' spy, distinct files).  The
vocabularies are REAL (the cached corpus tags surface — anti-tautology)
and the path resolves against the destination's REAL outline table.
The cache fallback (a board with NO in-memory items) still reads the
persisted items with zero scans."
  (skip-unless (locate-library "org-air"))
  (org-air-r64--with-corpus
      '(("dest.org" . "#+title: dest\n* Infra\n** Cloud\n")
        ("other.org" . "* TODO Other task :work:\n")
        ("inbox.org" . "* TODO Move me :inbox:\n"))
    (let* ((dest (expand-file-name "dest.org" org-air-r64--dir))
           (files (org-air-query-files))
           (items (org-air-query-items))
           (snapshot (org-air-view--mtimes-snapshot files)))
      (org-air-view--cache-write items snapshot)
      (org-air-r64--with-board
        (setq org-air-view--items items
              org-air-view--items-key (list org-air-files org-air-inbox-file)
              org-air-view--items-mtimes snapshot)
        (let ((item (org-air-test-find-item "Move me" items))
              (tag-vocab nil) (path-coll nil) (resolved nil))
          (should item)
          (org-air-r64--counting
            ;; stage 1: the `f' candidates from the board's index.
            (let* ((targets (org-air-inbox--target-files item))
                   (cands (org-air-inbox--file-candidates targets)))
              (should (member dest targets))
              (should (member "⌂ dest.org" cands)))
            ;; the tag/category vocabularies through the REAL editors.
            (cl-letf (((symbol-function 'completing-read-multiple)
                       (lambda (_prompt coll &rest _)
                         (setq tag-vocab coll)
                         nil)))
              (org-air-inbox--edit-tags item))
            (cl-letf (((symbol-function 'completing-read-multiple)
                       (lambda (&rest _) nil)))
              (org-air-inbox--edit-categories item))
            ;; stage 2: the path table over the ONE destination file.
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt coll &rest _)
                         (setq path-coll coll)
                         "Infra/Cloud")))
              (setq resolved (org-air-inbox--read-target-path dest)))
            ;; ZERO fresh scans / enumerations at menu time.
            (should (= org-air-r64--scan-calls 0))
            (should (= org-air-r64--items-calls 0))
            (should (= org-air-r64--enum-calls 0))
            ;; the path stage opened exactly ONE file: the destination.
            (should (equal (delete-dups
                            (mapcar #'file-truename org-air-r64--ffns-files))
                           (list (file-truename dest)))))
          ;; the vocab/table are REAL cached data — anti-tautology.
          (should (member "work" tag-vocab))
          (should (member "inbox" tag-vocab))
          (should (member "Infra/Cloud" path-coll))
          (should (equal resolved '(:olp ("Infra" "Cloud") :new 0)))
          ;; cache fallback: no in-memory items — still zero scans.
          (let ((org-air-view--items nil))
            (org-air-r64--counting
              (should (org-air-test-find-item
                       "Move me" (org-air-inbox--board-items)))
              (should (= org-air-r64--scan-calls 0))
              (should (= org-air-r64--items-calls 0)))))))))

;;;; -------------------------------------------------------------------
;;;; r64-6 — signature compatibility (T7)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r64-6-signature-compatibility ()
  "Every pre-R64 call shape keeps its meaning: `(item file)' appends at
file END under the `#+title' prose (the R53-6 headingless-note contract
re-run; the nil paste level is 1 — the called-out promotion of a
level-2 capture to a top-level heading); `(item file \"Existing\")'
files under the existing heading AT CHILD level (string ≡ one-segment
path — the level fix applies to string callers too); a MISSING string
is CREATED as a bare top-level heading with the item under it — the
silent file-end fallback is retired as a defect."
  (skip-unless (locate-library "org-air"))
  (org-air-r64--with-corpus
      '(("inbox.org" . "#+title: Inbox\n* New\n** TODO First :inbox:\n** TODO Second :inbox:\n** TODO Third :inbox:\n")
        ("note.org" . "#+title: R64 note\n\nSome prose only.\n")
        ("dest.org" . "#+title: dest\n* Existing\n"))
    (let ((note (expand-file-name "note.org" org-air-r64--dir))
          (dest (expand-file-name "dest.org" org-air-r64--dir))
          (inhibit-message t))
      ;; (item file): file end, top level, after the prose.
      (org-air-refile-item
       (org-air-r64--item org-air-inbox-file "First") note)
      (let ((text (org-air-r64--text note)))
        (should (string-match "Some prose only\\." text))
        (should (string-match "^\\* TODO First" text))
        (should (< (string-match "Some prose only\\." text)
                   (string-match "^\\* TODO First" text))))
      (should (equal (org-air-r64--headings note)
                     '((1 . "TODO First :inbox:"))))
      ;; (item file "Existing"): filed under it AT child level.
      (org-air-refile-item
       (org-air-r64--item org-air-inbox-file "Second") dest "Existing")
      ;; "Missing": created at top level — never the silent file end.
      (org-air-refile-item
       (org-air-r64--item org-air-inbox-file "Third") dest "Missing")
      (should (equal (org-air-r64--headings dest)
                     '((1 . "Existing")
                       (2 . "TODO Second :inbox:")
                       (1 . "Missing")
                       (2 . "TODO Third :inbox:")))))))

;;;; -------------------------------------------------------------------
;;;; r64-7 — the shell's batch-safe parts (T8)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r64-7-shell-binding-and-pure-readers ()
  "`org-air-refile-item' remains a command and the board's `r' binding
(the four legacy binding suites' contract, unchanged);
`org-air-refile-transient' is a command.  The `p' reader's resolver is
PURE over a fixture table: \"A/B\" with neither existing →
(:olp (\"A\" \"B\") :new 2), a trailing `/' is tolerated, a partial
chain counts only the missing tail, a full TABLE entry resolves with
`:new' 0, empty input answers `:olp' nil (file end), and an existing
heading NAMED with `/' matches as a table entry FIRST (split only as
the fallback).  The `f' reader still resolves a DISAMBIGUATED `⌂'
candidate (two files sharing a basename) to the REAL file — the R19-2
guarantee re-homed from the retired action menu."
  (skip-unless (locate-library "org-air"))
  ;; commands + the `r' binding on a live board keymap.
  (should (commandp 'org-air-refile-item))
  (should (commandp 'org-air-refile-transient))
  (org-air-r64--with-corpus '(("inbox.org" . "* TODO I :inbox:\n"))
    (org-air-r64--with-board
      (should (eq (key-binding (kbd "r")) 'org-air-refile-item))))
  ;; the `p' resolver, pure against fixture tables.
  (should (equal (org-air-inbox--resolve-path "A/B" '("X"))
                 '(:olp ("A" "B") :new 2)))
  (should (equal (org-air-inbox--resolve-path "A/B/" '("X"))
                 '(:olp ("A" "B") :new 2)))
  (should (equal (org-air-inbox--resolve-path "A/B" '("A"))
                 '(:olp ("A" "B") :new 1)))
  (should (equal (org-air-inbox--resolve-path "A/B"
                                              '(("A/B" . ("A" "B"))))
                 '(:olp ("A" "B") :new 0)))
  (should (equal (org-air-inbox--resolve-path "" '("A"))
                 '(:olp nil :new 0)))
  (should (equal (org-air-inbox--resolve-path
                  "a/b heading" '(("a/b heading" . ("a/b heading"))))
                 '(:olp ("a/b heading") :new 0)))
  ;; the `f' reader: a disambiguated `⌂' candidate → the REAL file.
  (org-air-r64--with-corpus '(("x.org" . "* A\n")
                              ("inbox.org" . "* TODO I :inbox:\n"))
    (make-directory (expand-file-name "sub" org-air-r64--dir))
    (write-region "* B\n" nil (expand-file-name "sub/x.org" org-air-r64--dir)
                  nil 'silent)
    (let* ((subx (expand-file-name "sub/x.org" org-air-r64--dir))
           (item (org-air-r64--item org-air-inbox-file "I"))
           (sub-cand "⌂ sub/x.org")
           (offered nil)
           (file (cl-letf (((symbol-function 'completing-read)
                            (lambda (_prompt coll &rest _)
                              (setq offered coll)
                              sub-cand)))
                   (org-air-inbox--read-target-file item))))
      ;; both basename twins were offered, disambiguated by parent dir
      ;; (the `dir/x.org' tail — `inbox.org' must not count as a twin).
      (should (member sub-cand offered))
      (should (= 2 (seq-count (lambda (c) (string-match-p "/x\\.org\\'" c))
                              offered)))
      (should (equal (file-truename file) (file-truename subx))))))

;;;; -------------------------------------------------------------------
;;;; r64-8 — the todo vocabulary is the destination file's OWN (T9)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r64-8-todo-vocab-is-destination-files-own ()
  "A destination file declaring `#+TODO: TODO HOLD | CLOSED' answers the
`k' vocabulary reader (`org-air-inbox--target-todo-keywords') with
exactly ITS merged keywords — the R57 law: the user's globals + the
per-file line win, org-air's supplement never replaces them.  A TODO
arg of \"CLOSED\" round-trips on-disk on the moved heading, and `donep'
semantics stay intact: the scan reads the file's OWN done set, so the
moved item is done while the HOLD sibling is not."
  (skip-unless (locate-library "org-air"))
  (org-air-r64--with-corpus
      '(("inbox.org" . "#+title: Inbox\n* New\n** TODO Pay invoice :inbox:\n")
        ("tasks.org" . "#+title: tasks\n#+TODO: TODO HOLD | CLOSED\n\n* HOLD Seed task\n"))
    (let ((tasks (expand-file-name "tasks.org" org-air-r64--dir))
          (inhibit-message t))
      ;; the vocabulary is the file's OWN merged keyword set, exactly.
      (should (equal (org-air-inbox--target-todo-keywords tasks)
                     '("TODO" "HOLD" "CLOSED")))
      ;; the engine round-trips the file's own done keyword.
      (org-air-refile-item
       (org-air-r64--item org-air-inbox-file "Pay invoice")
       tasks "Seed task" :none nil nil "CLOSED")
      (should (equal (org-air-r64--headings tasks)
                     '((1 . "HOLD Seed task")
                       (2 . "CLOSED Pay invoice"))))
      ;; donep semantics: the file's own `| CLOSED' set decides.
      (let* ((items (org-air-query--scan-file tasks))
             (moved (org-air-test-find-item "Pay invoice" items))
             (seed (org-air-test-find-item "Seed task" items)))
        (should moved)
        (should (org-air-item-donep moved))
        (should seed)
        (should-not (org-air-item-donep seed))))))

;;;; -------------------------------------------------------------------
;;;; r64-9 — a failed paste never loses the item (the harden guard)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r64-9-refile-crash-safe ()
  "The destructive window is guarded: `org-air-refile-item' cuts the
source subtree (delete + save, the text held only in a local) BEFORE
pasting into the target.  If `org-paste-subtree' (or any later step
before the target's save) SIGNALS, the source subtree is RESTORED at
its original position and saved — the inbox file is BYTE-IDENTICAL to
before the call — and the target file on disk is untouched (never
half-written: the pre-cut created parents live only in the unsaved
buffer).  The error itself still propagates to the caller, and the
SAME refile succeeds once the fault is gone — the failed attempt left
a fully working state.  Reverting the guard loses the item from the
saved inbox and fails the byte-identity assertion."
  (skip-unless (locate-library "org-air"))
  (org-air-r64--with-corpus
      `(("inbox.org" . ,org-air-r64--acceptance-inbox)
        ("projects.org" . ,org-air-r64--acceptance-projects))
    (let* ((projects (expand-file-name "projects.org" org-air-r64--dir))
           (inbox-before (org-air-r64--text org-air-inbox-file))
           (projects-before (org-air-r64--text projects))
           (inhibit-message t))
      ;; force the paste step to crash AFTER the cut has happened.
      (cl-letf (((symbol-function 'org-paste-subtree)
                 (lambda (&rest _)
                   (error "r64-9: simulated paste crash"))))
        (should-error
         (org-air-refile-item
          (org-air-r64--item org-air-inbox-file "Set up syncthing")
          projects '("Infra" "Cloud"))
         :type 'error))
      ;; the SOURCE is byte-identical on disk: the cut was rolled back.
      (should (equal (org-air-r64--text org-air-inbox-file) inbox-before))
      ;; the TARGET file was not half-written: on-disk bytes untouched.
      (should (equal (org-air-r64--text projects) projects-before))
      ;; the restored state is fully working: the SAME call (real paste)
      ;; now lands the item, and the source container empties normally.
      (org-air-refile-item
       (org-air-r64--item org-air-inbox-file "Set up syncthing")
       projects '("Infra" "Cloud"))
      (should (equal (org-air-r64--headings projects)
                     '((1 . "Website relaunch")
                       (2 . "TODO Fix nav")
                       (1 . "Infra")
                       (2 . "Cloud")
                       (3 . "TODO Set up syncthing :inbox:"))))
      (should-not (string-match-p "Set up syncthing"
                                  (org-air-r64--text org-air-inbox-file))))))

(provide 'org-air-round64-test)
;;; org-air-round64-test.el ends here
