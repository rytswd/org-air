;;; org-air-inbox.el --- Inbox capture and refile for org-air -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: org-air contributors
;; Keywords: outlines, calendar
;; Version: 0.1.0
;; URL: https://github.com/rytswd/org-air
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Inbox-first capture and the ONE-SHOT refile for org-air (R64): a
;; transient destination+metadata form (single confirm, live preview)
;; over a non-interactive engine that sets destination AND tags/
;; category/schedule/todo/priority in ONE call, with NESTED outline-path
;; targets whose missing parents are created on the fly by Org's own
;; refile machinery (org-refile.el / org.el — never org-agenda).
;;
;; R66: refiling to a brand-new or frontmatter-less target under an Air
;; tree synthesises Air frontmatter (a derived `#+title:', `#+state:'
;; from `org-air-refile-new-file-state', `#+FILETAGS:' from the moved
;; heading's effective tags) at the file top before the paste, and a
;; brand-new target's missing directory chain is created on execute —
;; both inside the R64 disk-atomic transaction: a failed refile leaves
;; no half-written file and mints no new file at all.
;;
;; R67: the transient is the board's general per-item EDITOR — the
;; destination is one OPTIONAL field.  With a destination, execute is
;; today's ONE engine call; without one, the changed metadata applies
;; IN PLACE at the item's source heading (`org-air-inbox--apply-item-
;; edits': one `atomic-change-group', one `save-buffer', no move); a
;; fully untouched form is a gentle no-op.  A `d' DEADLINE field joins
;; the metadata group and the engine (trailing optional argument).

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-refile)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'org-air-query)

(defvar org-air-inbox-file)
(defvar org-air-view-buffer-name)
(defvar org-air-view--items)
(defvar org-air-view--items-mtimes)
(defvar org-air-view--triage-source-buffer)

(declare-function org-air-view--cache-read "org-air-view")
(declare-function org-air-view--refresh-stale-item-guard "org-air-view" (item))

(defun org-air-inbox--board-buffer ()
  "Return the live board buffer, or nil (R53 P4)."
  (and (boundp 'org-air-view-buffer-name)
       (get-buffer org-air-view-buffer-name)))

(defun org-air-inbox--board-files ()
  "Return the board's last-enumerated file list, or nil (R53 P4).
The refile pickers must never re-walk 5000 files at menu time: the board
already holds the enumeration as its mtime baseline
\(`org-air-view--items-mtimes', hydrated from the persisted cache's
`:mtimes' on a warm start)."
  (when-let* ((board (org-air-inbox--board-buffer)))
    (mapcar #'car (buffer-local-value 'org-air-view--items-mtimes board))))

(defun org-air-inbox--board-items ()
  "Return the in-memory board items, else the persisted cache's (R53 P4).
NEVER a fresh `org-air-query-items' — the Tags…/Category… vocabularies
used to trigger a FULL synchronous rescan at menu time (the 271s class
when cold).  nil when neither the board nor the cache has items; the
completion then simply offers no pre-seeded vocabulary."
  (or (when-let* ((board (org-air-inbox--board-buffer)))
        (buffer-local-value 'org-air-view--items board))
      (when (fboundp 'org-air-view--cache-read)
        (plist-get (ignore-errors (org-air-view--cache-read)) :items))))

(defun org-air-inbox--ensure-file ()
  "Ensure `org-air-inbox-file' exists and return it."
  (let ((file (expand-file-name org-air-inbox-file)))
    (make-directory (file-name-directory file) t)
    (unless (file-exists-p file)
      (with-temp-file file
        (insert "#+title: org-air inbox\n\n")))
    file))

;;;###autoload
(defun org-air-capture (&optional title body)
  "Capture a new inbox item with TITLE and optional BODY."
  (interactive)
  (let* ((title (or title (read-string "Capture title: ")))
         (body (or body
                   (when current-prefix-arg
                     (read-string "Note: "))))
         (file (org-air-inbox--ensure-file)))
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "* TODO " title " :inbox:\n")
      (insert "  :PROPERTIES:\n  :CREATED: " (format-time-string "[%Y-%m-%d %a %H:%M]") "\n  :END:\n")
      (when (and body (not (string-empty-p body)))
        (insert body "\n"))
      (save-buffer))
    (message "Captured to %s" file)
    file))

(defun org-air-inbox--target-position (file heading)
  "Return insertion point for FILE under optional HEADING.
R53 P3 contract (\"refile to file top\"): with HEADING nil — which is
what `org-air-inbox--read-heading' yields for a HEADINGLESS note file —
the insertion point is the file end, i.e. directly under the `#+title'
content, so headingless notes are structurally valid refile targets.
R64: the refile path no longer inserts at this point verbatim (it
re-levels via `org-paste-subtree'); this resolver stays for its
remaining callers."
  (with-current-buffer (find-file-noselect file)
    (org-with-wide-buffer
     (goto-char (point-min))
     (if (and heading (not (string-empty-p heading))
              (re-search-forward (format org-complex-heading-regexp-format (regexp-quote heading)) nil t))
         (progn
           (org-end-of-subtree t t)
           (unless (bolp) (insert "\n"))
           (point-marker))
       (goto-char (point-max))
       (unless (bolp) (insert "\n"))
       (point-marker)))))

(defun org-air-inbox--file-headings (file)
  "Return FILE's heading titles (top-level + nested) as plain strings (R24-1).
Plain text (no fontification) so the completion vocabulary never carries an
org heading face; the order is buffer order so the list reads top-to-bottom."
  (when (and file (file-readable-p (expand-file-name file)))
    (with-current-buffer (find-file-noselect (expand-file-name file))
      (org-with-wide-buffer
       (let (out)
         (goto-char (point-min))
         (while (re-search-forward (concat "^" org-outline-regexp) nil t)
           (push (substring-no-properties (org-get-heading t t t t)) out))
         (nreverse out))))))

(defun org-air-inbox--read-heading (file)
  "Read an optional target HEADING in FILE via completion (R24-1, legacy).
Candidates are FILE's real headings plus a leading `(file end)' default;
`(file end)' / empty / RET => nil (append at file end).  Returns nil with
NO prompt when FILE has no headings.  R64: the board's refile form reads
a nested PATH instead (`org-air-inbox--read-target-path'); this flat
picker survives for its remaining callers."
  (let ((headings (org-air-inbox--file-headings file)))
    (when headings
      (let* ((file-end "(file end)")
             (choice (completing-read
                      "Under heading (default file end): "
                      (cons file-end headings) nil nil nil nil file-end)))
        (unless (or (string-empty-p choice) (string= choice file-end))
          choice)))))

(defun org-air-inbox--source-buffer (item)
  "Return source buffer for ITEM."
  (find-file-noselect (org-air-item-file item)))

(defun org-air-inbox--interactive-item ()
  "Return an org-air item at point in either dashboard or Org buffer."
  (or (get-text-property (point) 'org-air-item)
      (org-air-item-create
       ;; R23-1: strip properties off the at-point title (this item is built
       ;; inside a fontified Org buffer, so `org-get-heading' carries `face
       ;; org-level-1') so the moved item's row stays calm post-refile.
       :title (substring-no-properties (org-get-heading t t t t))
       :tags (org-get-tags nil nil)
       :file (or (buffer-file-name) "")
       :marker (copy-marker (point-marker))
       :todo (org-get-todo-state)
       :priority nil
       :scheduled nil
       :deadline nil
       :group nil)))

(defun org-air-inbox--target-files (item)
  "Return the real, expanded Org files for refile targets (R19-2).
Uses `org-air-query-files' (which RECURSES configured directories), so a
`⌂' candidate is always an actual file — the move bug was that
`org-air-files' may hold DIRECTORIES that never match a basename.  Falls
back to ITEM's own file when nothing is configured."
  (or (org-air-inbox--board-files)
      (ignore-errors (org-air-query-files))
      (list (org-air-item-file item))))

(defun org-air-inbox--file-candidates (files)
  "Return `⌂ <name>' refile candidates for FILES, disambiguating clashes (R19-2).
When two files share a basename, the candidate shows a parent-dir/name tail
so each `⌂' entry maps to exactly one file.  R53 P4: basenames are counted
in ONE hash pass (the old per-file `seq-count' was O(n²) — measured 3.0s
at 5006 files, now 0.013s), so the picker opens in <100ms."
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (file files)
      (let ((base (file-name-nondirectory file)))
        (puthash base (1+ (gethash base counts 0)) counts)))
    (mapcar (lambda (file)
              (let ((base (file-name-nondirectory file)))
                (concat "⌂ "
                        (if (> (gethash base counts 0) 1)
                            (concat (file-name-nondirectory
                                     (directory-file-name
                                      (file-name-directory file)))
                                    "/" base)
                          base))))
            files)))

(defun org-air-inbox--edit-tags (item)
  "Read a REPLACEMENT tag list for ITEM, pre-filled with its current tags (R19-2).
Uses `completing-read-multiple' over the tag vocabulary seeded with the
item's existing tags (joined by `,') so the user SEES the full set and can
add OR remove; the returned list replaces the tags.  R53 P4: the
vocabulary reads the IN-MEMORY board items (or the persisted cache) —
never a fresh scan at menu time."
  (let ((current (org-air-item-tags item))
        (vocab (delete-dups (seq-mapcat #'org-air-item-tags
                                        (org-air-inbox--board-items)))))
    (completing-read-multiple
     "Tags: " vocab nil nil
     (when current (mapconcat #'identity current ",")))))

(defun org-air-inbox--edit-categories (item)
  "Read a pre-filled category list for ITEM (R20-4a).
Uses `completing-read-multiple' seeded with the item's current category (its
`org-air-item-group') over the group vocabulary so a single pick is the
common case (add/remove from there).  Multiple
picks are allowed: the caller makes the FIRST the `:CATEGORY:' and adds any
extras as tags, so nothing the user typed is lost.  R53 P4: the vocabulary
reads the IN-MEMORY board items (or the persisted cache) — never a fresh
scan at menu time."
  (let ((current (org-air-item-group item))
        (vocab (delete-dups (delq nil (mapcar #'org-air-item-group
                                              (org-air-inbox--board-items))))))
    (completing-read-multiple
     "Category: " vocab nil nil
     (when (and current (not (string-empty-p current))) current))))

(defun org-air-inbox--decode-file-choice (choice item)
  "Resolve a `⌂ …' refile CHOICE for ITEM to a real target file path (R19-2).
`⌂ other file…' prompts via `read-file-name'; otherwise CHOICE is matched
against the same disambiguated `org-air-inbox--target-files' candidates the
prompt offered, so the chosen entry maps back to the actual expanded file
rather than the item's own file by accident — the original move bug."
  (if (string= choice "⌂ other file…")
      (read-file-name "Refile to file: ")
    (let* ((files (org-air-inbox--target-files item))
           (cands (org-air-inbox--file-candidates files))
           (idx (seq-position cands choice #'equal)))
      (or (and idx (nth idx files))
          (org-air-item-file item)))))

;;;; ---------------------------------------------------------------------
;;;; R64-2 — nested destinations: pick, complete, create-on-execute.
;;;; ---------------------------------------------------------------------

(defvar org-air-inbox--refile-last nil
  "Cons (FILE . OLP) of the last EXECUTED refile destination (session).
The transient form's `l' recall and the `f' picker's default read it —
the \"file the sibling too\" case is three keys.")

(defun org-air-inbox--read-target-file (item)
  "Read the destination FILE for ITEM — stage 1 of the R64-2 picker.
The R19-2/R53 cached `⌂' picker reused verbatim: candidates come from
`org-air-inbox--target-files' (board enumeration or persisted cache,
NEVER a fresh scan at menu time) through the one-pass
`org-air-inbox--file-candidates' disambiguation, plus the
`⌂ other file…' escape hatch; resolution reuses
`org-air-inbox--decode-file-choice' unchanged.  The prompt's default is
the last EXECUTED destination's file, so `f RET' re-picks it."
  (let* ((files (org-air-inbox--target-files item))
         (cands (append (org-air-inbox--file-candidates files)
                        '("⌂ other file…")))
         (last (car-safe org-air-inbox--refile-last))
         (idx (and last (seq-position files last #'equal)))
         (default (and idx (nth idx cands)))
         (choice (completing-read "Move to file: " cands nil t nil nil
                                  default)))
    (org-air-inbox--decode-file-choice choice item)))

(defun org-air-inbox--read-move-target (item)
  "Read a (FILE . HEADING) move target for ITEM (legacy two-step shape).
Stage 1 is `org-air-inbox--read-target-file' (the R64-3 `f' infix
reader); the optional flat `Under heading:' completion survives for its
remaining callers — the board's refile form reads a nested PATH instead
\(R64-2)."
  (let* ((file (org-air-inbox--read-target-file item))
         (heading (org-air-inbox--read-heading file)))
    (cons file (unless (and heading (string-empty-p heading)) heading))))

(defun org-air-inbox--path-table (file)
  "Return FILE's outline-path table as an alist ((PATH . SEGMENTS) …).
Built from `org-refile-get-targets' with `org-refile-targets' let-bound
to FILE alone (`:maxlevel' 9) and `org-refile-use-outline-path' t —
Org's proven path builder, scoped so it opens exactly ONE buffer (the
destination file the refile is about to open anyway).  R53 holds: the
5000-file world is never walked at menu time.  nil when FILE is not
readable."
  (when (and file (file-readable-p (expand-file-name file)))
    (let* ((file (expand-file-name file))
           (org-refile-targets `((,file :maxlevel . 9)))
           (org-refile-use-outline-path t)
           (org-refile-target-verify-function nil)
           (org-refile-use-cache nil)
           (inhibit-message t))
      (with-current-buffer (find-file-noselect file)
        (mapcar (lambda (target)
                  (cons (substring-no-properties (car target))
                        (org-with-wide-buffer
                         (goto-char (nth 3 target))
                         (mapcar #'substring-no-properties
                                 (org-get-outline-path t t)))))
                (org-refile-get-targets))))))

(defun org-air-inbox--path-table-normalize (table)
  "Return TABLE as ((PATH . SEGMENTS) …), splitting plain-string entries."
  (mapcar (lambda (entry)
            (if (consp entry) entry
              (cons entry (split-string entry "/" t))))
          table))

(defun org-air-inbox--path-new-count (table olp)
  "Return how many trailing OLP segments are missing from TABLE.
TABLE is a `org-air-inbox--path-table' alist (plain path strings are
tolerated).  The longest EXISTING prefix chain of OLP anchors the count:
the segments past it are the ones a refile will create, in order."
  (let ((table (org-air-inbox--path-table-normalize table))
        (n (length olp))
        (existing 0))
    (dotimes (j n)
      (let ((prefix (seq-take olp (1+ j))))
        (when (and (= existing j)
                   (seq-find (lambda (entry) (equal (cdr entry) prefix))
                             table))
          (setq existing (1+ j)))))
    (- n existing)))

(defun org-air-inbox--resolve-path (input table)
  "Parse the typed destination path INPUT against TABLE (R64-2).
TABLE is the one-file `org-air-inbox--path-table' alist (plain path
strings are tolerated).  Returns a plist (:olp SEGMENTS :new N) — the
segments to file under and the count of segments a refile will CREATE.
Empty INPUT means file end (:olp nil), the R53 headingless-note answer;
a trailing `/' is tolerated (the `org-refile--get-location'
normalization).  INPUT is matched as a TABLE entry FIRST — so an
existing heading whose NAME contains `/' stays addressable — and split
on `/' only as the fallback.  Nothing mutates at prompt time; creation
is deferred to execute."
  (let* ((table (org-air-inbox--path-table-normalize table))
         (norm (string-remove-suffix "/" (string-trim (or input "")))))
    (if (string-empty-p norm)
        (list :olp nil :new 0)
      (let ((entry (assoc norm table)))
        (if entry
            (list :olp (cdr entry) :new 0)
          (let ((segments (delq nil
                                (mapcar (lambda (seg)
                                          (let ((seg (string-trim seg)))
                                            (unless (string-empty-p seg) seg)))
                                        (split-string norm "/")))))
            (list :olp segments
                  :new (org-air-inbox--path-new-count table segments))))))))

(defun org-air-inbox--read-target-path (file)
  "Read the outline PATH within FILE — stage 2 of the R64-2 picker.
A single-shot `completing-read' (`require-match' nil,
`completion-ignore-case' t — Org's own choice) over FILE's real target
table; typing beyond the existing tree means CREATE, empty input / RET
means file end.  Returns the `org-air-inbox--resolve-path' plist
\(:olp SEGMENTS :new N)."
  (let* ((table (org-air-inbox--path-table file))
         (completion-ignore-case t)
         (input (completing-read
                 (format "Path in %s (RET = file end, / nests): "
                         (file-name-nondirectory (or file "")))
                 (mapcar #'car table))))
    (org-air-inbox--resolve-path input table)))

;;;; ---------------------------------------------------------------------
;;;; R66 — step 0: Air frontmatter synthesis for new/frontmatter-less
;;;; targets, plus the v0.2 target-directory creation fold-in.
;;;; ---------------------------------------------------------------------

(defcustom org-air-refile-synthesize-frontmatter t
  "When the refile engine synthesises Air frontmatter into a target (R66-2).
Fires only for a target buffer with no `#+title:' keyword before its
first heading — which a brand-new file trivially is; an already-titled
target is always left byte-for-byte as-is.  t (the default) gates the
synthesis on the target lying under an Air-managed tree
\(`org-air-inbox--air-tree-p'), so ordinary org notes outside Air stay
bare; `always' synthesises for every new/frontmatter-less target; nil
never synthesises (the bare pre-R66 write everywhere)."
  :type '(choice (const :tag "In Air trees (default)" t)
                 (const :tag "Every new/frontmatter-less target" always)
                 (const :tag "Never" nil))
  :group 'org-air)

(defcustom org-air-refile-new-file-state "draft"
  "The `#+state:' value a synthesised refile target receives (R66-1).
A freshly refiled item is un-triaged planning material, hence the
\"draft\" default; users who treat refile-out-of-inbox as commitment
set \"ready\"."
  :type '(choice (const "draft") (const "ready")
                 (const "work-in-progress") (const "complete")
                 (const "dropped") (string :tag "Custom"))
  :group 'org-air)

(defun org-air-inbox--air-tree-p (file)
  "Return non-nil when FILE lies under an Air-managed tree (R66-2).
Cheap O(path-depth) stats, run once per refile EXECUTE and never at
prompt time (R53 — no scan, no enumeration):
`locate-dominating-file' over exactly the `org-air-detect-air-project'
marker test (an `air-config.toml' file or an `air/' subdirectory —
inlined so this file grows no hard org-air-project.el requirement),
ORed with membership under a configured Air root when org-air-project
IS loaded (covers an explicit `org-air-projects' entry whose root
carries neither marker).  nil on any failure — the refile degrades to
the bare write, never an error."
  (ignore-errors
    (let ((file (expand-file-name file)))
      (and (or (locate-dominating-file
                file
                (lambda (dir)
                  (or (file-exists-p (expand-file-name "air-config.toml" dir))
                      (file-directory-p (expand-file-name "air" dir)))))
               (and (fboundp 'org-air-project-roots)
                    (seq-some (lambda (root)
                                (and (stringp root)
                                     (file-in-directory-p file root)))
                              (org-air-project-roots))))
           t))))

(defun org-air-inbox--derive-title ()
  "Return the Air `#+title:' derived from the heading at point (R66-1).
Org's OWN parsers, no hand-rolled heading regexp: `org-get-heading'
strips the TODO keyword (THIS buffer's merged R57 vocabulary — the
user's globals + the file's own `#+TODO:' line win), the `[#A]'
priority cookie, the trailing tag list and the COMMENT keyword in one
call; the statistics cookies are then removed via org-element
\(`org-element-parse-secondary-string' under the `headline'
restriction, `org-element-extract-element', re-interpreted), so
non-cookie bracketed text — org link markup — survives verbatim.
`string-clean-whitespace' collapses the doubled space a removed inline
cookie leaves.  May return the empty string (degenerate headings like
\"* TODO [1/2]\"); the caller falls back to the target's
`file-name-base'."
  (let* ((raw (substring-no-properties (org-get-heading t t t t)))
         (dummy (org-element-create 'headline (list :secondary '(:title))))
         (title (org-element-parse-secondary-string
                 raw (org-element-restriction 'headline) dummy)))
    (org-element-put-property dummy :title title)
    (dolist (cookie (org-element-map title 'statistics-cookie #'identity))
      (org-element-extract-element cookie))
    (string-clean-whitespace
     (substring-no-properties
      (org-element-interpret-data (org-element-property :title dummy))))))

(defun org-air-inbox--item-derived-title (item)
  "Derive the synthesised `#+title:' for ITEM in its SOURCE buffer (R66-1).
The R57 point: the TODO strip must read the source buffer's own merged
vocabulary, so the derivation runs at the item's heading in its own
file — the same `org-back-to-heading' resolution the cut path performs
\(one extra read, zero extra file visits)."
  (with-current-buffer (org-air-inbox--source-buffer item)
    (org-with-wide-buffer
     (goto-char (let ((m (org-air-item-marker item)))
                  (if (markerp m) (marker-position m) (or (cdr-safe m) 1))))
     (org-back-to-heading t)
     (org-air-inbox--derive-title))))

(defun org-air-inbox--synthesis-filetags (item tags)
  "Return the `#+FILETAGS:' tag list for a synthesised target, or nil.
The moved heading's EFFECTIVE tags — the refile TAGS argument when
non-nil (`:none' means empty), else ITEM's own tags (what the
transient preview shows) — MINUS `inbox': leaving the inbox is what
refiling is (the same rule as the R64 form pre-fill).  Order is
preserved; nil (the empty set) means the line is omitted entirely."
  (remove "inbox"
          (cond ((eq tags :none) nil)
                (tags tags)
                (t (org-air-item-tags item)))))

(defun org-air-inbox--target-titled-p ()
  "Return non-nil when the current buffer already carries a `#+title:'.
The idempotence check of the R66 synthesis, against the BUFFER — not
the disk, so a retry after a failed refile finds its own unsaved
residue and never writes the block twice: the
`org-air-project--read-keyword' regexp idiom, wide buffer, bounded to
BEFORE the first heading (a keyword below a heading is not file
frontmatter)."
  (org-with-wide-buffer
   (goto-char (point-min))
   (let ((bound (save-excursion
                  (if (re-search-forward org-outline-regexp-bol nil t)
                      (match-beginning 0)
                    (point-max))))
         (case-fold-search t))
     (and (re-search-forward "^[ \t]*#\\+title:" bound t) t))))

(defun org-air-inbox--frontmatter-insert-point ()
  "Return the insertion position for the synthesised frontmatter block.
`point-min', EXCEPT when the buffer's first element is a file-level
`:PROPERTIES:' drawer (org requires it to come first; probed:
`org-element-at-point' at bob reports `property-drawer') — then that
drawer's `:end'."
  (org-with-wide-buffer
   (goto-char (point-min))
   (let ((first (org-element-at-point)))
     (if (eq (org-element-type first) 'property-drawer)
         (org-element-property :end first)
       (point-min)))))

(defun org-air-inbox--synthesize-frontmatter (item target-file tags)
  "Step 0 of the refile engine: give TARGET-FILE Air frontmatter (R66-1).
When the synthesis rule fires — `org-air-refile-synthesize-frontmatter'
non-nil, TARGET-FILE's buffer has no `#+title:' yet (a brand-new file
trivially so), and the value is `always' or the target lies under an
Air tree (`org-air-inbox--air-tree-p') — insert at the file top:
`#+title:' derived from ITEM's heading (falling back to the file-name
base when the derivation is empty), `#+state:' from
`org-air-refile-new-file-state', and `#+FILETAGS:' from ITEM's
effective TAGS (omitted when empty), followed by ONE blank line (the
`--ensure-file' shape).  BUFFER ONLY, never a save: the target's disk
state still changes solely via the ONE `save-buffer' at the end of the
successful R64 transaction, so a failed refile to a brand-new target
creates NO file at all.  Return non-nil when a block was written."
  (when (and org-air-refile-synthesize-frontmatter
             (or (eq org-air-refile-synthesize-frontmatter 'always)
                 (org-air-inbox--air-tree-p target-file)))
    (with-current-buffer (find-file-noselect target-file)
      (unless (org-air-inbox--target-titled-p)
        (let* ((derived (org-air-inbox--item-derived-title item))
               (title (if (string-empty-p derived)
                          (file-name-base target-file)
                        derived))
               (ftags (org-air-inbox--synthesis-filetags item tags)))
          (org-with-wide-buffer
           (goto-char (org-air-inbox--frontmatter-insert-point))
           (insert "#+title: " title "\n"
                   "#+state: " org-air-refile-new-file-state "\n")
           (when ftags
             (insert "#+FILETAGS: :" (mapconcat #'identity ftags ":") ":\n"))
           (insert "\n")
           t))))))

;;;; ---------------------------------------------------------------------
;;;; R64-1 — the non-interactive engine: ensure-olp + re-leveled paste.
;;;; ---------------------------------------------------------------------

(defun org-air-inbox--heading-stars (pos)
  "Return the raw star count of the heading at POS in the current buffer."
  (save-excursion
    (goto-char pos)
    (if (looking-at "\\*+") (length (match-string 0)) 0)))

(defun org-air-inbox--olp-child (title parent-pos parent-level)
  "Find the child heading TITLE directly under the parent at PARENT-POS.
Searches WITHIN the parent's subtree bounds (the whole buffer when
PARENT-POS is nil) for a heading titled exactly TITLE — the
`org-complex-heading-regexp-format' idiom — at exactly the child star
depth of PARENT-LEVEL (`org-get-valid-level').  First match in buffer
order wins.  Returns the heading position, or nil."
  (org-with-wide-buffer
   (let* ((beg (or parent-pos (point-min)))
          (end (if parent-pos
                   (save-excursion (goto-char parent-pos)
                                   (org-end-of-subtree t t)
                                   (point))
                 (point-max)))
          (want (if parent-pos (org-get-valid-level parent-level 1) 1))
          (re (format org-complex-heading-regexp-format
                      (regexp-quote title)))
          (found nil))
     (goto-char beg)
     (while (and (not found) (re-search-forward re end t))
       (when (= (length (match-string 1)) want)
         (setq found (match-beginning 0))))
     found)))

(defun org-air-inbox--ensure-olp (file olp)
  "Ensure the outline path OLP (list of heading titles) exists in FILE.
R64-1: resolved root-down — each segment must sit directly under its
parent (first match in buffer order); a MISSING segment is created by
Org's own `org-refile-new-child', handed a synthetic (NAME FILE RE POS)
parent target, so Org does the level math (`org-get-valid-level'),
end-of-subtree placement and blank-line handling — org-air writes no
star arithmetic of its own.  Created parents are plain headings (no
TODO keyword, no timestamp): R59 containers, never board rows.  Returns
\(MARKER . LEVEL) of the final segment's heading in FILE's buffer, or
nil when OLP is nil."
  (when olp
    (let ((file (expand-file-name file)))
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (let ((parent-pos nil) (parent-level 0) (parent-name nil))
           (dolist (segment olp)
             (let ((pos (org-air-inbox--olp-child segment parent-pos
                                                  parent-level)))
               (unless pos
                 (setq pos (nth 3 (org-refile-new-child
                                   (list parent-name file nil parent-pos)
                                   segment))))
               (setq parent-pos pos
                     parent-level (org-air-inbox--heading-stars pos)
                     parent-name segment)))
           (cons (copy-marker parent-pos) parent-level)))))))

(defun org-air-inbox--resolve-target (file target-heading)
  "Resolve TARGET-HEADING in FILE to a (MARKER . LEVEL) parent, or nil.
nil (or empty-string) TARGET-HEADING returns nil: the item appends at
file END — the R53 P3 headingless-note contract.  A STRING resolves
like the pre-R64 refile (first `org-complex-heading-regexp-format'
match in buffer order, any depth); a MISSING string is CREATED at top
level as a one-segment path — the silent file-end fallback is retired
as a defect.  A LIST of strings is an outline path handed to
`org-air-inbox--ensure-olp' (missing segments created root-down)."
  (cond
   ((null target-heading) nil)
   ((stringp target-heading)
    (unless (string-empty-p target-heading)
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (if (re-search-forward
              (format org-complex-heading-regexp-format
                      (regexp-quote target-heading))
              nil t)
             (cons (copy-marker (match-beginning 0))
                   (length (match-string 1)))
           (org-air-inbox--ensure-olp file (list target-heading)))))))
   (t (org-air-inbox--ensure-olp file target-heading))))

;;;###autoload
(defun org-air-refile-item (&optional item target-file target-heading tags
                                      scheduled category todo priority
                                      deadline note)
  "Move ITEM to TARGET-FILE — the one-shot refile engine (R64-1).

Interactively (the board's `e') this opens the transient
destination+metadata form `org-air-refile-transient': one interaction
sets destination AND tags/category/schedule/todo/priority, one confirm
executes ONE call of this engine.  Non-interactively ITEM and
TARGET-FILE are required.

TARGET-HEADING accepts three shapes: nil appends at file end (top
level, the R53 headingless-note contract); a STRING is a one-segment
path (existing headings resolve first-match as before, a missing one is
now CREATED at top level); a LIST of strings is an outline PATH whose
missing segments are created root-down via `org-refile-new-child' —
\"Infra\" › \"Cloud\" on an empty file yields `* Infra' › `** Cloud'.
The item is pasted with `org-paste-subtree' at parent-level+1 (the
`org-refile' idiom), so the item AND its own children re-level together
— refiled-under never lands as a sibling again.

TAGS replaces the item's tags when non-nil (the keyword `:none' clears
them); CATEGORY sets the moved heading's `:CATEGORY:' property;
SCHEDULED is an Org timestamp/shift string (empty clears the schedule);
TODO (a keyword string) and PRIORITY (a character, or string of one)
are applied via `org-todo' / `org-priority' — nil leaves each
untouched.  DEADLINE (R67-3) mirrors SCHEDULED via `org-deadline':
an Org timestamp/shift string stamps a deadline, the empty string
clears one, nil leaves it untouched — a trailing additive parameter,
so every pre-R67 caller passes unchanged.  NOTE (R71-2) replays that
precedent: a non-empty string is appended as a dated Org log note at
the MOVED heading in the TARGET buffer (after the metadata block,
inside the same transaction, before the ONE transactional save) via
`org-air-inbox--append-log-note' — the drawer decision is the WRITE
TARGET file's own `org-log-into-drawer' / `#+STARTUP: logdrawer'
\(the R67-4 write-target law); nil or the empty string writes no
note, so every pre-R71 caller passes unchanged.

R66: step 0 first ensures TARGET-FILE's directory chain exists, and —
gated by `org-air-refile-synthesize-frontmatter' — a brand-new or
`#+title:'-less target gets Air frontmatter synthesised at its top
\(`org-air-inbox--synthesize-frontmatter': derived `#+title:',
`#+state:' from `org-air-refile-new-file-state', `#+FILETAGS:' from
the effective tags) before the paste; an already-titled target is left
byte-for-byte as-is, and a failed refile still creates no file."
  (interactive (list 'org-air-inbox--form-dispatch))
  (if (eq item 'org-air-inbox--form-dispatch)
      (call-interactively #'org-air-refile-transient)
    (unless (and item target-file)
      (error "ITEM and TARGET-FILE are required (org-air-refile-item)"))
    (let* ((target-file (expand-file-name target-file))
           ;; R66 step 0, BEFORE `--resolve-target' (the pos-1 marker
           ;; hazard: a marker does not advance past an insertion AT its
           ;; own position, so frontmatter written after resolution
           ;; would leave a fresh file's parent marker on the `#+title:'
           ;; line).  First the v0.2 directory fold-in (R66-3, mirrors
           ;; `--ensure-file'; EXECUTE-time only — a failed refile's
           ;; empty chain is inert residue, spec Decision 4), then the
           ;; frontmatter synthesis (R66-1/-2; buffer only, no save —
           ;; disk atomicity stays with the ONE transactional
           ;; `save-buffer' below).  Both run before the cut, so any
           ;; failure here aborts with the item untouched (R64 safety).
           (parent (progn
                     (make-directory (file-name-directory target-file) t)
                     (org-air-inbox--synthesize-frontmatter
                      item target-file tags)
                     ;; ensure-olp NEXT (R64 spec order): creation
                     ;; re-resolves by NAME, the (MARKER . LEVEL) parent
                     ;; survives the same-file cut, and OLP parents
                     ;; created on a fresh file land BELOW the block.
                     (org-air-inbox--resolve-target target-file
                                                    target-heading)))
           (text nil)
           (src-buf nil)
           (src-beg nil))
      ;; cut (R26-8: a cache-hydrated item carries (FILE . POS), not a marker)
      (with-current-buffer (org-air-inbox--source-buffer item)
        (save-excursion
          (goto-char (let ((m (org-air-item-marker item)))
                       (if (markerp m) (marker-position m) (or (cdr-safe m) 1))))
          (org-back-to-heading t)
          (let ((begin (point))
                (end (save-excursion (org-end-of-subtree t t) (point))))
            (setq text (buffer-substring begin end)
                  src-buf (current-buffer)
                  ;; a MARKER, never a stale integer (r64fix2): a
                  ;; SAME-FILE paste shifts every position after it, and
                  ;; the marker rides the shift (and the atomic rollback
                  ;; below), so a failure restore lands EXACTLY where
                  ;; the subtree was cut.
                  src-beg (copy-marker begin))
            (delete-region begin end)
            (save-buffer))))
      ;; paste, re-leveled: last child of the parent (or file end, level 1).
      ;; TRANSACTIONAL (R64 harden + fix2): between the cut above and the
      ;; target's `save-buffer' the item exists ONLY in TEXT.  The whole
      ;; post-cut window — paste + EVERY metadata step (todo / priority /
      ;; tags / category / schedule / deadline) + the save — runs inside ONE
      ;; `atomic-change-group' on the target buffer, so ANY signal (or
      ;; quit) first rolls back EVERY target-side change in-buffer: no
      ;; half-paste, no dirty pasted subtree left for a retry (or any
      ;; later save) to double-write — and in the SAME-FILE case the
      ;; rollback restores the shared buffer's post-cut geometry.  Only
      ;; then does the unwind handler re-insert the RAW capture at the
      ;; SRC-BEG marker and save: the source is byte-identical on disk,
      ;; the error propagates unchanged, a retry starts from a clean
      ;; slate.  The paste-local newline keeps TEXT itself un-mutated so
      ;; a no-final-newline source restores without growing a byte.
      (let ((paste-text (if (string-suffix-p "\n" text)
                            text
                          (concat text "\n")))
            (landed nil))
        (unwind-protect
            (progn
              (with-current-buffer (find-file-noselect target-file)
                (org-with-wide-buffer
                 (atomic-change-group
                   (let ((level (if parent (org-get-valid-level (cdr parent) 1) 1)))
                     (if parent
                         (progn (goto-char (car parent))
                                (org-end-of-subtree t t))
                       (goto-char (point-max)))
                     (unless (bolp) (insert "\n"))
                     (let ((insert-pos (point-marker)))
                       (unwind-protect
                           (progn
                             (org-paste-subtree level paste-text)
                             (goto-char insert-pos)
                             (org-back-to-heading t)
                             ;; R68-3: the board-context logging
                             ;; discipline around the metadata block —
                             ;; the todo/schedule/deadline mutators run
                             ;; in a TARGET buffer the user never sees,
                             ;; the same `@'-note exposure as the board
                             ;; verbs (see `org-air-view--at-item-source').
                             (let ((org-inhibit-logging
                                    (or org-inhibit-logging 'note))
                                   (org-log-reschedule
                                    (if (eq org-log-reschedule 'note) 'time
                                      org-log-reschedule))
                                   (org-log-redeadline
                                    (if (eq org-log-redeadline 'note) 'time
                                      org-log-redeadline)))
                               (when todo (org-todo todo))
                               (when priority
                                 (org-priority (if (stringp priority)
                                                   (aref priority 0)
                                                 priority)))
                               (when tags
                                 (org-set-tags (if (eq tags :none) nil tags)))
                               (when (and category
                                          (not (string-empty-p category)))
                                 (org-set-property "CATEGORY" category))
                               (when scheduled
                                 (if (string-empty-p scheduled)
                                     (org-schedule '(4))
                                   (org-schedule nil scheduled)))
                               (when deadline
                                 (if (string-empty-p deadline)
                                     (org-deadline '(4))
                                   (org-deadline nil deadline))))
                             ;; R68-2: flush the pending (downgraded)
                             ;; log record INSIDE the transaction,
                             ;; before the ONE save — the state line
                             ;; lands in the saved bytes and rolls back
                             ;; with everything else on a signal.
                             ;; Relocated here (R71 Decision 4,
                             ;; behaviour-neutral: same transaction,
                             ;; still before the save) so it provably
                             ;; runs BEFORE the note — the probed
                             ;; globals-overwrite hazard:
                             ;; `org-add-log-setup' would clobber the
                             ;; shared `org-log-note-*' globals of a
                             ;; pending downgraded record, and the note
                             ;; writer's own dequeue would silently
                             ;; drop it.
                             (org-air-inbox--flush-pending-log-note)
                             ;; R71-2: the explicit NOTE at the MOVED
                             ;; heading — the write-target buffer's own
                             ;; `org-log-into-drawer' governs (R67-4);
                             ;; still inside the atomic-change-group,
                             ;; so a signal rolls the note back with
                             ;; everything else.
                             (when (and note (not (string-empty-p note)))
                               (goto-char insert-pos)
                               (org-back-to-heading t)
                               (org-air-inbox--append-log-note note)))
                         (set-marker insert-pos nil))))
                   (save-buffer))))
              (setq landed t))
          ;; `inhibit-quit': a second C-g must not abort the restore (or
          ;; the marker cleanup) half-way.
          (let ((inhibit-quit t))
            (unless landed
              (with-current-buffer src-buf
                (save-excursion
                  (goto-char src-beg)
                  (insert text)
                  (save-buffer))))
            (set-marker src-beg nil)
            (when (car-safe parent) (set-marker (car parent) nil)))))
      (message "Refiled → %s%s"
               (file-name-nondirectory target-file)
               (cond ((consp target-heading)
                      (concat " › " (mapconcat #'identity target-heading " › ")))
                     ((and (stringp target-heading)
                           (not (string-empty-p target-heading)))
                      (concat " › " target-heading))
                     (t "")))
      (when (derived-mode-p 'org-air-view-mode)
        (when (fboundp 'org-air-refresh)
          (org-air-refresh))))))

;;;; ---------------------------------------------------------------------
;;;; R64-3 / R67 — the transient editor: metadata + OPTIONAL destination,
;;;; one confirm.
;;;; ---------------------------------------------------------------------

(defvar org-air-inbox--refile-form nil
  "The transient editor form state (R64-3 / R67), a plist.
Keys: `:item' (the org-air item being edited), `:file' / `:olp' /
`:new' (the OPTIONAL destination + to-create count — set means execute
REFILES, nil means it edits IN PLACE), `:tags' (pre-filled from the
item MINUS `inbox') with the R67-2 companions `:tags-dirty' (set by
every path that mutates `:tags' — an untouched field writes nothing in
place) and `:tags-stripped' (the recorded `inbox' strip, re-attached by
the in-place leg so an in-place edit never graduates an inbox item),
and the dirty-only fields `:category', `:scheduled'
\(+ `:schedule-label'), `:deadline' (+ `:deadline-label'), `:todo',
`:priority' \(nil = leave the item's own value untouched), and
`:note' (R71-1 — the drafted dated-log-note text; nil = no note,
the form never holds \"\").")

(defun org-air-inbox--form-get (key)
  "Return KEY's value from the transient refile form state."
  (plist-get org-air-inbox--refile-form key))

(defun org-air-inbox--form-put (key value)
  "Set KEY to VALUE in the transient refile form state."
  (setq org-air-inbox--refile-form
        (plist-put org-air-inbox--refile-form key value)))

(defun org-air-inbox--form-init (item)
  "Seed the transient editor form state from ITEM (R64-3 / R67 pre-fills).
Tags pre-fill MINUS `inbox' — leaving the inbox is what refiling is,
and the strip is RECORDED on `:tags-stripped' so the R67-1 in-place
leg can re-attach it (an in-place tag edit never silently graduates an
inbox item).  Destination starts EMPTY (the last-used file would be a
silent wrong default — `l' recalls it); an empty destination means
execute applies the changed metadata IN PLACE."
  (setq org-air-inbox--refile-form
        (list :item item
              :file nil :olp nil :new 0
              :tags (remove "inbox" (org-air-item-tags item))
              :tags-dirty nil
              :tags-stripped (and (member "inbox" (org-air-item-tags item))
                                  '("inbox"))
              :category nil :scheduled nil :schedule-label nil
              :deadline nil :deadline-label nil
              :todo nil :priority nil :note nil)))

(defun org-air-inbox--item-priority-char (item)
  "Return ITEM's priority cookie as a character, or nil.
The scan records `org-get-priority' NUMBERS (1000 × (lowest − char));
invert that scale back to the letter; a raw character passes through."
  (let ((p (org-air-item-priority item)))
    (cond ((null p) nil)
          ((stringp p) (and (> (length p) 0) (aref p 0)))
          ((and (integerp p) (<= org-priority-highest p org-priority-lowest))
           p)
          ((integerp p)
           (let ((c (- org-priority-lowest (/ p 1000))))
             (and (<= org-priority-highest c org-priority-lowest) c)))
          (t nil))))

(defun org-air-inbox--target-todo-keywords (file)
  "Return the DESTINATION FILE's own merged todo vocabulary (R57).
Read inside that file's buffer, so the user's globals + per-file
`#+TODO:' win; when the buffer is first opened here, the merged
scan-time default (`org-air-query--scan-todo-keywords') is let-bound so
an undeclared file still sees org-air's supplement — the user's global
`org-todo-keywords' is never rebound."
  (when (and file (file-readable-p (expand-file-name file)))
    (let ((org-todo-keywords
           (if (fboundp 'org-air-query--scan-todo-keywords)
               (org-air-query--scan-todo-keywords)
             org-todo-keywords)))
      (with-current-buffer (find-file-noselect (expand-file-name file))
        (copy-sequence org-todo-keywords-1)))))

(defun org-air-inbox--read-todo-keyword (file &optional current)
  "Complete a TODO keyword over FILE's own merged vocabulary (R68-1).
The R67 `k'-field reader EXTRACTED, not forked — the ONE shared
completion-over-target-vocab path behind both the board's `T'
\(`org-air-item-cycle-todo') and `org-air-refile-form-todo'.  The
collection is FILE's buffer-local `org-todo-keywords-1' via
`org-air-inbox--target-todo-keywords' (R57: the user's globals + the
file's `#+TODO:' line win; dir-locals apply because that helper reads
inside `find-file-noselect's fully-initialised buffer), with the
global `org-todo-keywords-1' as the fallback when FILE is unreadable.
CURRENT pre-fills as the completion default.  Returns the chosen
keyword string, or nil for an empty choice.  `require-match' stays
nil (the R67 field's shape): a free-typed keyword the file never
declares is rejected by `org-todo' itself with an honest `user-error'
\(\"State X not valid in this file\") — an error message, not a trap."
  (let* ((vocab (or (org-air-inbox--target-todo-keywords file)
                    org-todo-keywords-1))
         (choice (completing-read
                  "Todo (empty leaves untouched): " vocab nil nil nil nil
                  current)))
    (unless (string-empty-p choice) choice)))

(defun org-air-inbox--target-priority-range (file)
  "Return (HIGHEST . LOWEST) priority chars for destination FILE."
  (if (and file (file-readable-p (expand-file-name file)))
      (with-current-buffer (find-file-noselect (expand-file-name file))
        (cons org-priority-highest org-priority-lowest))
    (cons org-priority-highest org-priority-lowest)))

(defconst org-air-inbox--schedule-options
  '(("today" . ".") ("tomorrow" . "+1d") ("this week" . "+1w")
    ("someday" . someday) ("other date…" . other) ("clear" . ""))
  "The R64-3 `s' quick-pick: label → Org shift string or action symbol.
`someday' keeps its R20-4 meaning (adds the `someday' tag + clears the
schedule); `other date…' runs `org-read-date'.")

(defconst org-air-inbox--deadline-options
  '(("today" . ".") ("tomorrow" . "+1d") ("this week" . "+1w")
    ("other date…" . other) ("clear" . ""))
  "The R67-3 `d' quick-pick: label → Org shift string or action symbol.
The `s' list minus its schedule-specific `someday' leg (R20-4 is
schedule vocabulary — tag + cleared SCHEDULE — and does not transfer
to deadlines); `other date…' runs `org-read-date'.")

(defun org-air-inbox--form-write-target ()
  "Return the file the form's EXECUTE will write in (R67-4).
The chosen destination when set; otherwise the item's OWN file — the
in-place leg writes there, so the `k'/`,' vocabulary must read the
same buffer the apply-time `org-todo'/`org-priority' will run in
\(probed: `org-todo' user-errors on a keyword the buffer never
declared — completing over the wrong file's vocabulary manufactures
apply-time failures)."
  (or (org-air-inbox--form-get :file)
      (let* ((item (org-air-inbox--form-get :item))
             (file (and item (org-air-item-file item))))
        (and file (not (string-empty-p file)) file))))

(defun org-air-inbox--form-effective-tags ()
  "Return (WRITE-P . TAGS) — the ONE R67-2 effective-tags rule.
Used by the preview AND both execute legs (WYSIWYG by construction):
with a destination the collected `:tags' apply verbatim (today's
refile semantics — the `inbox' strip IS the graduation); without one,
a dirty tag edit applies the collected list PLUS the recorded
`:tags-stripped' `inbox' tag appended at the END (an in-place edit
never graduates an inbox item; `delete-dups' covers the user re-adding
`inbox' by hand), and an untouched field writes nothing — WRITE-P nil,
TAGS then previews the item's own list."
  (let ((file (org-air-inbox--form-get :file))
        (tags (org-air-inbox--form-get :tags)))
    (cond
     (file (cons t tags))
     ((org-air-inbox--form-get :tags-dirty)
      ;; the trailing nil forces `append' to COPY `:tags-stripped' too,
      ;; so the destructive `delete-dups' never mutates the form state.
      (cons t (delete-dups
               (append tags (org-air-inbox--form-get :tags-stripped) nil))))
     (t (cons nil (let ((item (org-air-inbox--form-get :item)))
                    (and item (org-air-item-tags item))))))))

(defun org-air-inbox--schedule-resolved (spec)
  "Return SPEC (an Org date/shift string) resolved to `Fri Jul 24', or nil."
  (ignore-errors
    (format-time-string
     "%a %b %d"
     (org-time-string-to-time (org-read-date nil nil spec)))))

(defun org-air-inbox--form-note-label (note)
  "Return the drafted NOTE's one-line field/preview label (R71-1).
The FIRST line of NOTE, truncated to width 24 via
`truncate-string-to-width' with an \"…\" ellipsis; the ellipsis is
ALSO appended when further lines follow untruncated, so a short first
line never masquerades as the whole note.  Pure string formatting —
used by BOTH the `n' field row and the Preview extras segment, so the
two render identically by construction (WYSIWYG)."
  (let* ((first (car (split-string note "\n")))
         (multi (string-match-p "\n" note))
         (label (truncate-string-to-width first 24 nil nil "…")))
    (if (and multi (equal label first))
        (concat label "…")
      label)))

(defun org-air-inbox--form-field (label value)
  "Format one transient field row: LABEL, then VALUE (`–' when unset)."
  (format "%-9s%s" label (or value "–")))

(defun org-air-inbox--form-creates ()
  "Return the create-list annotation for the form's destination, or nil.
The \"(creates: …)\" suffix lists what EXECUTE will mint: `new file'
when the chosen `:file' does not exist yet (R66-3 — one `file-exists-p'
stat over the collected value; no buffer, no mutation, the R64
defer-everything rule holds), then exactly the missing path segments in
creation order; nil (no annotation) means everything exists."
  (let* ((file (org-air-inbox--form-get :file))
         (olp (org-air-inbox--form-get :olp))
         (new (or (org-air-inbox--form-get :new) 0))
         (parts (delq nil
                      (list (and file (not (file-exists-p file)) "new file")
                            (and olp (> new 0)
                                 (mapconcat #'identity
                                            (nthcdr (- (length olp) new) olp)
                                            " › "))))))
    (when parts
      (format "  (creates: %s)" (mapconcat #'identity parts ", ")))))

(defun org-air-inbox--form-heading ()
  "Return the transient's header: the short truncated editor prompt (R67-4)."
  (let ((item (org-air-inbox--form-get :item)))
    (if item
        (format "Edit \"%s\""
                (truncate-string-to-width
                 (org-air-item-title item) 40 nil nil "…"))
      "Edit")))

(defun org-air-inbox--form-preview ()
  "Render the live preview group (R64-3 / R67-4).
Pure string formatting over the collected values — no buffer access, so
the form stays instant.  Line 1: basename › path (or the in-place
placeholder) › the heading line as it will be written (todo, priority,
title, the R67-2 EFFECTIVE tags) + the `(creates: …)' annotation;
line 2: SCHEDULED / DEADLINE / `:CATEGORY:' when set, plus the R71-1
`note: <first line>…' trailing segment when a note is drafted (the
shared `org-air-inbox--form-note-label' truncation — the field row
and this segment render the same label by construction)."
  (let ((item (org-air-inbox--form-get :item)))
    (if (not item)
        "Preview"
      (let* ((file (org-air-inbox--form-get :file))
             (olp (org-air-inbox--form-get :olp))
             (tags (cdr (org-air-inbox--form-effective-tags)))
             (todo (or (org-air-inbox--form-get :todo)
                       (org-air-item-todo item)))
             (pri (or (org-air-inbox--form-get :priority)
                      (org-air-inbox--item-priority-char item)))
             (sched (org-air-inbox--form-get :schedule-label))
             (dead (org-air-inbox--form-get :deadline-label))
             (cat (org-air-inbox--form-get :category))
             (note (org-air-inbox--form-get :note))
             (dest (if file
                       (concat (file-name-nondirectory file)
                               (and olp
                                    (concat " › "
                                            (mapconcat #'identity olp " › "))))
                     "(in place — f to refile)"))
             (head (concat "* "
                           (and todo (concat todo " "))
                           (and pri (format "[#%c] " pri))
                           (org-air-item-title item)
                           (and tags
                                (concat " :" (mapconcat #'identity tags ":")
                                        ":"))))
             (extra (mapconcat
                     #'identity
                     (delq nil
                           (list (and sched (concat "SCHEDULED: " sched))
                                 (and dead (concat "DEADLINE: " dead))
                                 (and cat (concat ":CATEGORY: " cat))
                                 (and note
                                      (concat
                                       "note: "
                                       (org-air-inbox--form-note-label
                                        note)))))
                     "  ·  ")))
        (concat "Preview\n " dest " › " head
                (or (org-air-inbox--form-creates) "")
                (unless (string-empty-p extra) (concat "\n " extra)))))))

(transient-define-suffix org-air-refile-form-file ()
  "Pick the destination FILE (the cached R19-2/R53 `⌂' picker)."
  :transient t
  :description (lambda ()
                 (org-air-inbox--form-field
                  "file"
                  (when-let* ((file (org-air-inbox--form-get :file)))
                    (file-name-nondirectory file))))
  (interactive)
  (let* ((item (org-air-inbox--form-get :item))
         (file (org-air-inbox--read-target-file item)))
    (unless (equal file (org-air-inbox--form-get :file))
      (org-air-inbox--form-put :olp nil)
      (org-air-inbox--form-put :new 0))
    (org-air-inbox--form-put :file file)))

(transient-define-suffix org-air-refile-form-path ()
  "Pick the outline PATH within the chosen file (typing beyond creates)."
  :transient t
  :description (lambda ()
                 (org-air-inbox--form-field
                  "path"
                  (when-let* ((olp (org-air-inbox--form-get :olp)))
                    (concat (mapconcat #'identity olp " › ")
                            (or (org-air-inbox--form-creates) "")))))
  (interactive)
  (let ((file (org-air-inbox--form-get :file)))
    (if (null file)
        (message "Pick a destination file first (f)")
      (let ((resolved (org-air-inbox--read-target-path file)))
        (org-air-inbox--form-put :olp (plist-get resolved :olp))
        (org-air-inbox--form-put :new (plist-get resolved :new))))))

(transient-define-suffix org-air-refile-form-last ()
  "Recall the last EXECUTED destination (file + path) in one key."
  :transient t
  :description (lambda ()
                 (org-air-inbox--form-field
                  "last destination"
                  (when-let* ((last org-air-inbox--refile-last))
                    (concat (file-name-nondirectory (car last))
                            (and (cdr last)
                                 (concat " › "
                                         (mapconcat #'identity (cdr last)
                                                    " › ")))))))
  (interactive)
  (if (null org-air-inbox--refile-last)
      (message "No refile executed this session yet")
    (let ((file (car org-air-inbox--refile-last))
          (olp (cdr org-air-inbox--refile-last)))
      (org-air-inbox--form-put :file file)
      (org-air-inbox--form-put :olp olp)
      (org-air-inbox--form-put
       :new (if olp
                (org-air-inbox--path-new-count
                 (org-air-inbox--path-table file) olp)
              0)))))

(transient-define-suffix org-air-refile-form-tags ()
  "Edit the replacement tag list (CRM over the R53 cached vocabulary)."
  :transient t
  :description (lambda ()
                 (org-air-inbox--form-field
                  "tags"
                  (when-let* ((tags (org-air-inbox--form-get :tags)))
                    (concat ":" (mapconcat #'identity tags ":") ":"))))
  (interactive)
  (let* ((item (org-air-inbox--form-get :item))
         (probe (org-air-item-create
                 :title (org-air-item-title item)
                 :file (org-air-item-file item)
                 :tags (org-air-inbox--form-get :tags))))
    (org-air-inbox--form-put :tags (org-air-inbox--edit-tags probe))
    (org-air-inbox--form-put :tags-dirty t)))

(transient-define-suffix org-air-refile-form-category ()
  "Edit the category (CRM; first pick is `:CATEGORY:', extras are tags)."
  :transient t
  :description (lambda ()
                 (org-air-inbox--form-field
                  "category"
                  (let ((item (org-air-inbox--form-get :item)))
                    (or (org-air-inbox--form-get :category)
                        (and item (org-air-item-group item))))))
  (interactive)
  (let* ((item (org-air-inbox--form-get :item))
         (probe (org-air-item-create
                 :title (org-air-item-title item)
                 :file (org-air-item-file item)
                 :group (or (org-air-inbox--form-get :category)
                            (org-air-item-group item))))
         (picks (org-air-inbox--edit-categories probe)))
    (org-air-inbox--form-put :category (car picks))
    (when (cdr picks)
      (org-air-inbox--form-put
       :tags (delete-dups (append (org-air-inbox--form-get :tags)
                                  (cdr picks))))
      (org-air-inbox--form-put :tags-dirty t))))

(transient-define-suffix org-air-refile-form-schedule ()
  "Pick the schedule: today / tomorrow / this week / someday / date / clear."
  :transient t
  :description (lambda ()
                 (org-air-inbox--form-field
                  "schedule"
                  (or (org-air-inbox--form-get :schedule-label)
                      (let ((item (org-air-inbox--form-get :item)))
                        (and item
                             (stringp (org-air-item-scheduled item))
                             (org-air-item-scheduled item))))))
  (interactive)
  (let* ((choice (completing-read
                  "Schedule: "
                  (mapcar #'car org-air-inbox--schedule-options) nil t))
         (spec (cdr (assoc choice org-air-inbox--schedule-options))))
    (cond
     ((eq spec 'someday)
      ;; R20-4 semantics kept: the `someday' tag + a cleared schedule;
      ;; the preview shows both effects.
      (org-air-inbox--form-put :scheduled "")
      (org-air-inbox--form-put :schedule-label "someday (+ #someday, cleared)")
      (org-air-inbox--form-put
       :tags (delete-dups (append (org-air-inbox--form-get :tags)
                                  (list "someday"))))
      (org-air-inbox--form-put :tags-dirty t))
     ((eq spec 'other)
      (let ((date (org-read-date)))
        (org-air-inbox--form-put :scheduled date)
        (org-air-inbox--form-put :schedule-label date)))
     ((string-empty-p spec)
      (org-air-inbox--form-put :scheduled "")
      (org-air-inbox--form-put :schedule-label "clear"))
     (t
      (org-air-inbox--form-put :scheduled spec)
      (org-air-inbox--form-put
       :schedule-label
       (let ((resolved (org-air-inbox--schedule-resolved spec)))
         (if resolved (format "%s (%s)" choice resolved) choice)))))))

(transient-define-suffix org-air-refile-form-deadline ()
  "Pick the deadline: today / tomorrow / this week / date / clear (R67-3).
Mirrors the `s' schedule field minus its schedule-specific `someday'
leg; applied via `org-deadline' in BOTH execute legs (refile and
in-place)."
  :transient t
  :description (lambda ()
                 (org-air-inbox--form-field
                  "deadline"
                  (or (org-air-inbox--form-get :deadline-label)
                      (let ((item (org-air-inbox--form-get :item)))
                        ;; a STATIC `org-air-item-deadline' accessor
                        ;; call — safe: cl-defstruct's compiler macro
                        ;; inlines the slot access at expansion time,
                        ;; so the view.el COMMAND of the same name
                        ;; (the documented R67 Decision-4 collision)
                        ;; is never consulted.  Keep it static.
                        (and item
                             (stringp (org-air-item-deadline item))
                             (org-air-item-deadline item))))))
  (interactive)
  (let* ((choice (completing-read
                  "Deadline: "
                  (mapcar #'car org-air-inbox--deadline-options) nil t))
         (spec (cdr (assoc choice org-air-inbox--deadline-options))))
    (cond
     ((eq spec 'other)
      (let ((date (org-read-date)))
        (org-air-inbox--form-put :deadline date)
        (org-air-inbox--form-put :deadline-label date)))
     ((string-empty-p spec)
      (org-air-inbox--form-put :deadline "")
      (org-air-inbox--form-put :deadline-label "clear"))
     (t
      (org-air-inbox--form-put :deadline spec)
      (org-air-inbox--form-put
       :deadline-label
       (let ((resolved (org-air-inbox--schedule-resolved spec)))
         (if resolved (format "%s (%s)" choice resolved) choice)))))))

(transient-define-suffix org-air-refile-form-todo ()
  "Pick the TODO keyword from the WRITE TARGET's own vocabulary (R57/R67-4).
The destination file when one is set, else the item's OWN file — the
file the write will land in, so completion and the apply-time
`org-todo' agree by construction."
  :transient t
  :description (lambda ()
                 (org-air-inbox--form-field
                  "todo"
                  (or (org-air-inbox--form-get :todo)
                      (let ((item (org-air-inbox--form-get :item)))
                        (and item (org-air-item-todo item))))))
  (interactive)
  (let ((item (org-air-inbox--form-get :item)))
    ;; R68-1: behaviour byte-for-byte through the extracted shared
    ;; reader (the r67-7 vocabulary pin holds) — the board `T' and this
    ;; suffix are ONE completion-over-target-vocab path now.
    (org-air-inbox--form-put
     :todo (org-air-inbox--read-todo-keyword
            (org-air-inbox--form-write-target)
            (or (org-air-inbox--form-get :todo)
                (org-air-item-todo item))))))

(transient-define-suffix org-air-refile-form-priority ()
  "Cycle the priority: – → A → … → lowest → – (the WRITE TARGET's range).
The destination file when one is set, else the item's OWN file
\(R67-4)."
  :transient t
  :description (lambda ()
                 (org-air-inbox--form-field
                  "priority"
                  (when-let* ((c (or (org-air-inbox--form-get :priority)
                                     (let ((item (org-air-inbox--form-get
                                                  :item)))
                                       (and item
                                            (org-air-inbox--item-priority-char
                                             item))))))
                    (string c))))
  (interactive)
  (let* ((range (org-air-inbox--target-priority-range
                 (org-air-inbox--form-write-target)))
         (current (or (org-air-inbox--form-get :priority)
                      (let ((item (org-air-inbox--form-get :item)))
                        (and item (org-air-inbox--item-priority-char item)))))
         (next (cond ((null current) (car range))
                     ((>= current (cdr range)) nil)
                     (t (1+ current)))))
    (org-air-inbox--form-put :priority next)))

(defun org-air-inbox--flush-pending-log-note ()
  "Synchronously store a pending timestamp-style Org log record (R68-2).
`org-add-log-setup' DEFERS its record to `post-command-hook' — wrong
twice over for a board-context write: inside `org-air-process-inbox'
the whole guided loop is ONE command, so the hook cannot run between
iterations and a second `org-todo' OVERWRITES the shared
`org-log-note-*' globals (silently losing the earlier record); and a
deferred record lands AFTER the verb's `save-buffer'.  So: when a
`time'/`state' record is pending, run `org-add-log-note' NOW with
`this-command' let-bound to `org-log-note-this-command' (satisfying
its identity gate; the `recursion-depth' gate holds trivially — any
minibuffer read has exited by apply time).  For those hows the hook
function stores IMMEDIATELY: `org-store-log-note' inserts via MARKER
\(display-independent, probed unmocked in batch) and its
window-configuration save/restore brackets the call, leaving an
interactive frame exactly as it was.  The `how' gate is
belt-and-braces: a genuinely interactive pending `note' (impossible
under the R68 downgrade, conceivable from an outer context) is LEFT
for the command loop — this helper never opens an interaction and
never hijacks one.  Callers run it AFTER the mutators and BEFORE the
save, so the log line is part of the SAME saved bytes (and the R53
scan sees the complete edit at the next refresh)."
  (when (and (memq 'org-add-log-note post-command-hook)
             (memq org-log-note-how '(time state)))
    (let ((this-command org-log-note-this-command))
      (org-add-log-note))))

(defun org-air-inbox--append-log-note (text)
  "Append TEXT as a dated Org log note at the heading at point (R70-2).
The synchronous emulation of `org-add-log-note's finishing branch,
with the note buffer PRE-FILLED instead of user-edited — org still
owns the formatting (the dated `- Note taken on [ts] \\\\' line +
indented continuation lines, from `org-log-note-headings'), the
timestamp (`org-log-note-effective-time') and the placement
\(`org-log-beginning' via MARKER — display-independent, honouring the
buffer's own `org-log-into-drawer' / `#+STARTUP: logdrawer' /
`LOG_INTO_DRAWER').  `org-add-log-setup' is the org-owned seam: it
records the marker, the `note' purpose (selecting the \"Note taken
on %t\" template) and the effective time, and queues
`org-add-log-note' on `post-command-hook' — immediately dequeued here
\(+ `org-log-setup' cleared), taking over its ONLY remaining job, so
the full-frame `*Org Note*' buffer can never trap against an
undisplayed source (the R68 trap class).
`org-log-note-window-configuration' / `org-log-note-return-to' are
pre-set so `org-store-log-note's unconditional epilogue restore is a
no-op bracket; `current-prefix-arg' / `org-note-abort' are its two
abort gates, let-bound nil so a stray prefix argument can never
silently drop the note.  The note buffer is a `generate-new-buffer'
— `org-store-log-note' KILLS it, so `with-temp-buffer' would
double-kill.  Disjoint from the R68 logging discipline by
construction: an explicit `org-add-log-setup' never consults
`org-inhibit-logging', and the hook is already clean afterwards, so
the R68 flush no-ops — an EXPLICIT user note is applied, never
downgraded or suppressed."
  (org-add-log-setup 'note nil nil 'note)
  (remove-hook 'post-command-hook #'org-add-log-note)
  (setq org-log-setup nil)
  (setq org-log-note-window-configuration (current-window-configuration))
  (move-marker org-log-note-return-to (point))
  (let ((buf (generate-new-buffer " *org-air-note*"))
        (current-prefix-arg nil)
        (org-note-abort nil))
    (with-current-buffer buf (insert text))
    (with-current-buffer buf (org-store-log-note))))

(defun org-air-inbox--apply-item-edits (item edits)
  "Apply EDITS to ITEM's source heading IN PLACE — the R67-1 editor leg.
EDITS is a plist of exactly the CHANGED fields: `:todo', `:priority'
\(char or one-char string), `:tags' (guarded by `:tags-p' t — the
value may be nil, which CLEARS), `:scheduled' / `:deadline' (Org
date/shift strings; \"\" clears via the \='(4) prefix), `:category',
and `:note' (R71-2 — a dated Org log note appended at the heading via
`org-air-inbox--append-log-note', drawer per the SOURCE file's own
`org-log-into-drawer'; nil or \"\" writes no note).  Returns the list
of applied field symbols in application order (the completion message
enumerates them).

Inlines `org-air-view--at-item-source's semantics — its home file
requires this one, a hard require back would be circular: the
mid-refresh stale guard when loaded, the R26-8 marker-or-(FILE . POS)
position, `org-back-to-heading' under `org-with-wide-buffer', and the
triage-undo source recording (the board's `u' covers an in-place edit
like every other single-field verb).  NOT the refile engine: no cut,
no paste, no target resolution, no frontmatter synthesis, no directory
creation — the mutators run at the source heading in the engine's
order (todo → priority → tags → category → schedule → deadline)
inside ONE `atomic-change-group' with ONE `save-buffer' after (the
R64 discipline scaled down): any signal rolls back every in-buffer
change and propagates — the file is never saved, bytes identical.
The R70 standalone note wrapper (`org-air-inbox--add-item-note')
folded into this function's `:note' leg (R71 Decision 3): a
note-only edit is `(:note \"…\")' through the same path, same
discipline — one code shape for every in-place confirm.

R68-3: mirrors the board-context logging discipline of
`org-air-view--at-item-source' (whose semantics this function inlines
by design) — `org-inhibit-logging' `note' + the reschedule/redeadline
`note'→`time' downgrade around the mutators, and the synchronous
`org-air-inbox--flush-pending-log-note' AFTER the mutators, before
the save — so a `@'-note keyword (or a `lognotereschedule' config)
records a timestamped state line in the same save instead of trapping
an `*Org Note*' prompt against the undisplayed source buffer.  The
intra-transaction ORDER is part of the contract (R71 Decision 4,
probed): mutators → flush → `:note' — the explicit note displaces the
flush as the change group's LAST form; running the note's
`org-add-log-setup' BEFORE the flush would overwrite the shared
`org-log-note-*' globals of a pending downgraded record and its
dequeue would silently drop that record.  Rollback covers metadata,
record and note together (probed byte-exact)."
  (when (fboundp 'org-air-view--refresh-stale-item-guard)
    (org-air-view--refresh-stale-item-guard item))
  (let ((applied nil)
        (buf (org-air-inbox--source-buffer item)))
    (with-current-buffer buf
      (org-with-wide-buffer
       (goto-char (let ((m (org-air-item-marker item)))
                    (if (markerp m) (marker-position m) (or (cdr-safe m) 1))))
       (org-back-to-heading t)
       (atomic-change-group
         ;; R68-3: the logging discipline around the mutator block —
         ;; `(or … 'note)' preserves an outer t (full inhibition stays
         ;; full); the reschedule/redeadline knobs are read DIRECTLY by
         ;; `org--deadline-or-schedule' (it ignores `org-inhibit-logging'),
         ;; so their `note' value downgrades here too.  Dynamic bindings
         ;; only — the user's customs are never modified.
         (let ((org-inhibit-logging (or org-inhibit-logging 'note))
               (org-log-reschedule (if (eq org-log-reschedule 'note) 'time
                                     org-log-reschedule))
               (org-log-redeadline (if (eq org-log-redeadline 'note) 'time
                                     org-log-redeadline)))
           (when-let* ((todo (plist-get edits :todo)))
             (org-todo todo)
             (push 'todo applied))
           (when-let* ((p (plist-get edits :priority)))
             (org-priority (if (stringp p) (aref p 0) p))
             (push 'priority applied))
           (when (plist-get edits :tags-p)
             (org-set-tags (plist-get edits :tags))
             (push 'tags applied))
           (when-let* ((category (plist-get edits :category)))
             (org-set-property "CATEGORY" category)
             (push 'category applied))
           (when-let* ((scheduled (plist-get edits :scheduled)))
             (if (string-empty-p scheduled)
                 (org-schedule '(4))
               (org-schedule nil scheduled))
             (push 'scheduled applied))
           (when-let* ((deadline (plist-get edits :deadline)))
             (if (string-empty-p deadline)
                 (org-deadline '(4))
               (org-deadline nil deadline))
             (push 'deadline applied)))
         ;; R68-2: the flush AFTER the mutators — the log line rides
         ;; the same rollback AND the same save below.  R71 Decision 4
         ;; pins it BEFORE the note (the probed globals-overwrite
         ;; hazard: `org-add-log-setup' clobbers a pending downgraded
         ;; record's shared globals and the note writer's dequeue
         ;; drops it).
         (org-air-inbox--flush-pending-log-note)
         ;; R71-2: the explicit note is the change group's LAST form —
         ;; re-anchored first (the mutators may drift point); drawer
         ;; per this SOURCE buffer's own `org-log-into-drawer'.
         (let ((note (plist-get edits :note)))
           (when (and note (not (string-empty-p note)))
             (org-back-to-heading t)
             (org-air-inbox--append-log-note note)
             (push 'note applied)))))
      (save-buffer)
      (when (boundp 'org-air-view--triage-source-buffer)
        (setq org-air-view--triage-source-buffer buf)))
    (nreverse applied)))

(transient-define-suffix org-air-refile-form-note ()
  "Draft the dated log-note FIELD — the R71-1 action→field repurpose.
A transient FIELD like its Metadata siblings, not the R70 immediate
action: reads the note text from the MINIBUFFER (multi-line ok —
yank, \\`C-q C-j'; never org's interactive `*Org Note*' buffer, the
exact R68 trap shape) PRE-FILLED with the pending value, so a
re-press RE-EDITS the same draft and EMPTY input CLEARS the field
\(initial-input, deliberately NOT a default — a default would
re-assert itself on empty RET, leaving the field no way out; the `k'
todo field's empty-clears shape applied to free text).  Stores the
dirty `:note' — the form never holds \"\", empty IS nil IS no note —
and writes NOTHING at read time (the R64-2 prompt-time no-mutation
contract covers the note; \\`C-g' at the prompt keeps the previous
value, \\`C-g'/`q' on the form ABANDON a drafted note).  The field
REPLACES rather than journals: one RET writes at most ONE dated note
\(RET then `e' again journals); on execute the note rides whichever
leg RET's label announces — in place via
`org-air-inbox--apply-item-edits', or at the MOVED heading through
the engine's trailing NOTE parameter."
  :transient t
  :description (lambda ()
                 (org-air-inbox--form-field
                  "note"
                  (when-let* ((note (org-air-inbox--form-get :note)))
                    (org-air-inbox--form-note-label note))))
  (interactive)
  (let ((text (read-string "Note (empty clears): "
                           (org-air-inbox--form-get :note))))
    (org-air-inbox--form-put :note (unless (string-empty-p text) text))))

(transient-define-suffix org-air-refile-form-execute ()
  "Execute the collected editor form — the R67-1 two-way dispatch.
With a destination (`:file' set): today's ONE `org-air-refile-item'
engine call, byte-for-byte (R64/R66 — cut, paste, frontmatter
synthesis, transactional save), plus the R67-3 trailing DEADLINE and
the R71-2 trailing NOTE (applied at the MOVED heading; never part of
the `l' recall — per-confirm payload).  Without one: the changed
fields apply IN PLACE at the item's source via
`org-air-inbox--apply-item-edits' — a drafted `:note' is a REAL edit
like any field, so a note-only form runs the applier (no move, no
engine, nothing recorded for the `l' recall); a fully untouched form
is a gentle no-op message — never an error, never a mutation.  ONE
RET confirms edit + note together in BOTH legs (R71)."
  :description (lambda ()
                 (if (org-air-inbox--form-get :file) "refile" "edit in place"))
  (interactive)
  (let* ((item (org-air-inbox--form-get :item))
         (file (org-air-inbox--form-get :file))
         (olp (org-air-inbox--form-get :olp))
         (eff (org-air-inbox--form-effective-tags))
         (scheduled (org-air-inbox--form-get :scheduled))
         (deadline (org-air-inbox--form-get :deadline))
         (category (org-air-inbox--form-get :category))
         (todo (org-air-inbox--form-get :todo))
         (priority (org-air-inbox--form-get :priority))
         (note (org-air-inbox--form-get :note)))
    (cond
     (file
      ;; the refile leg — R64/R66 unchanged: same `(or tags :none)'
      ;; call shape, same `--refile-last' recording (+ DEADLINE, and
      ;; the R71-2 trailing NOTE — landed at the moved heading, never
      ;; recorded for `l').
      (org-air-refile-item item file olp
                           (or (cdr eff) :none)
                           scheduled category todo priority deadline note)
      (setq org-air-inbox--refile-last (cons file olp))
      (setq org-air-inbox--refile-form nil))
     (t
      ;; the in-place leg — collect ONLY the dirty fields (R67-2).
      (let ((edits nil))
        (when todo (setq edits (plist-put edits :todo todo)))
        (when priority (setq edits (plist-put edits :priority priority)))
        (when (car eff)                 ; `:tags-dirty' (no destination)
          (setq edits (plist-put edits :tags (cdr eff)))
          (setq edits (plist-put edits :tags-p t)))
        (when (and category (not (string-empty-p category)))
          (setq edits (plist-put edits :category category)))
        (when scheduled (setq edits (plist-put edits :scheduled scheduled)))
        (when deadline (setq edits (plist-put edits :deadline deadline)))
        ;; R71-2: a drafted note is a REAL edit — a note-only form
        ;; runs the applier; "Nothing to change" now fires only when
        ;; the form is truly untouched.
        (when note (setq edits (plist-put edits :note note)))
        (if (null edits)
            (progn
              (setq org-air-inbox--refile-form nil)
              (message "Nothing to change — f picks a destination, any metadata key edits in place"))
          (let ((applied (org-air-inbox--apply-item-edits item edits)))
            (setq org-air-inbox--refile-form nil)
            (message "Edited \"%s\" — %s"
                     (org-air-item-title item)
                     (mapconcat #'symbol-name applied ", "))
            (when (derived-mode-p 'org-air-view-mode)
              (when (fboundp 'org-air-refresh)
                (org-air-refresh))))))))))

;;;###autoload (autoload 'org-air-refile-transient "org-air-inbox" nil t)
(transient-define-prefix org-air-refile-transient ()
  "The per-item EDITOR with an OPTIONAL destination (R64-3 / R67).
Every field is visible with its current value, editable in any order;
the Preview group re-renders live; RET means what its dynamic label
says — with a destination it executes ONE `org-air-refile-item' call,
without one it applies the changed metadata IN PLACE at the item's
source (an untouched form is a gentle no-op); \\`C-g' / q abandon
everything (no buffer was touched — every write is deferred to
execute, R64-2, INCLUDING the `n' note field: the drafted note is
stored, previewed, and only written by RET, so abandoning the form
abandons the draft too).  Fields: tags / category / schedule /
deadline / todo / priority / note (a dated LOGBOOK note, drawer per
the write target) — ONE RET confirms edit + note together, in place
or at the refiled heading (R71).  The `e' binding and the
`org-air-refile-*' names stay — refiling is one optional field of
the editor, not a separate mode."
  [:description org-air-inbox--form-heading
   ["Destination"
    ("f" org-air-refile-form-file)
    ("p" org-air-refile-form-path)
    ("l" org-air-refile-form-last)]
   ["Metadata"
    ("t" org-air-refile-form-tags)
    ("c" org-air-refile-form-category)
    ("s" org-air-refile-form-schedule)
    ("d" org-air-refile-form-deadline)
    ("k" org-air-refile-form-todo)
    ("," org-air-refile-form-priority)
    ("n" org-air-refile-form-note)]]
  [:description org-air-inbox--form-preview
   ("RET" org-air-refile-form-execute)
   ("q" "quit" transient-quit-one)]
  (interactive)
  (when noninteractive
    (user-error "The editor form is interactive-only; call `org-air-refile-item' / `org-air-inbox--apply-item-edits' with arguments in batch"))
  (org-air-inbox--form-init (org-air-inbox--interactive-item))
  (transient-setup 'org-air-refile-transient))

(provide 'org-air-inbox)

;; Local Variables:
;; package-lint-main-file: "org-air.el"
;; End:
;;; org-air-inbox.el ends here
