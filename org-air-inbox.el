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

;;; Code:

(require 'org)
(require 'org-refile)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'org-air-query)

(defvar org-air-inbox-file)
(defvar org-air-view-buffer-name)
(defvar org-air-view--items)
(defvar org-air-view--items-mtimes)

(declare-function org-air-view--cache-read "org-air-view")

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
                                      scheduled category todo priority)
  "Move ITEM to TARGET-FILE — the one-shot refile engine (R64-1).

Interactively (the board's `r') this opens the transient
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
untouched."
  (interactive (list 'org-air-inbox--form-dispatch))
  (if (eq item 'org-air-inbox--form-dispatch)
      (call-interactively #'org-air-refile-transient)
    (unless (and item target-file)
      (error "ITEM and TARGET-FILE are required (org-air-refile-item)"))
    (let* ((target-file (expand-file-name target-file))
           ;; ensure-olp FIRST (spec order): creation re-resolves by NAME,
           ;; and the (MARKER . LEVEL) parent survives the same-file cut.
           (parent (org-air-inbox--resolve-target target-file target-heading))
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
      ;; tags / category / schedule) + the save — runs inside ONE
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
                             (when todo (org-todo todo))
                             (when priority
                               (org-priority (if (stringp priority)
                                                 (aref priority 0)
                                               priority)))
                             (when tags
                               (org-set-tags (if (eq tags :none) nil tags)))
                             (when (and category (not (string-empty-p category)))
                               (org-set-property "CATEGORY" category))
                             (when scheduled
                               (if (string-empty-p scheduled)
                                   (org-schedule '(4))
                                 (org-schedule nil scheduled))))
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
;;;; R64-3 — the transient form: destination + metadata, one confirm.
;;;; ---------------------------------------------------------------------

(defvar org-air-inbox--refile-form nil
  "The transient refile form state (R64-3), a plist.
Keys: `:item' (the org-air item being refiled), `:file' / `:olp' /
`:new' (destination + to-create count), `:tags' (ALWAYS applied —
pre-filled from the item MINUS `inbox'), and the dirty-only fields
`:category', `:scheduled' (+ `:schedule-label'), `:todo', `:priority'
\(nil = leave the item's own value untouched).")

(defun org-air-inbox--form-get (key)
  "Return KEY's value from the transient refile form state."
  (plist-get org-air-inbox--refile-form key))

(defun org-air-inbox--form-put (key value)
  "Set KEY to VALUE in the transient refile form state."
  (setq org-air-inbox--refile-form
        (plist-put org-air-inbox--refile-form key value)))

(defun org-air-inbox--form-init (item)
  "Seed the transient refile form state from ITEM (R64-3 pre-fills).
Tags pre-fill MINUS `inbox' (leaving the inbox is what refiling is; the
tag is re-addable in one CRM keystroke); destination starts EMPTY (the
last-used file would be a silent wrong default — `l' recalls it)."
  (setq org-air-inbox--refile-form
        (list :item item
              :file nil :olp nil :new 0
              :tags (remove "inbox" (org-air-item-tags item))
              :category nil :scheduled nil :schedule-label nil
              :todo nil :priority nil)))

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

(defun org-air-inbox--schedule-resolved (spec)
  "Return SPEC (an Org date/shift string) resolved to `Fri Jul 24', or nil."
  (ignore-errors
    (format-time-string
     "%a %b %d"
     (org-time-string-to-time (org-read-date nil nil spec)))))

(defun org-air-inbox--form-field (label value)
  "Format one transient field row: LABEL, then VALUE (`–' when unset)."
  (format "%-9s%s" label (or value "–")))

(defun org-air-inbox--form-creates ()
  "Return the create-list annotation for the form's path, or nil.
The \"(creates: …)\" suffix lists exactly the missing segments, in
creation order; nil (no annotation) means every segment exists."
  (let ((olp (org-air-inbox--form-get :olp))
        (new (or (org-air-inbox--form-get :new) 0)))
    (when (and olp (> new 0))
      (format "  (creates: %s)"
              (mapconcat #'identity (nthcdr (- (length olp) new) olp)
                         " › ")))))

(defun org-air-inbox--form-heading ()
  "Return the transient's header: the short truncated refile prompt."
  (let ((item (org-air-inbox--form-get :item)))
    (if item
        (format "Refile \"%s\""
                (truncate-string-to-width
                 (org-air-item-title item) 40 nil nil "…"))
      "Refile")))

(defun org-air-inbox--form-preview ()
  "Render the live preview group (R64-3).
Pure string formatting over the collected values — no buffer access, so
the form stays instant.  Line 1: basename › path › the heading line as
it will be written (todo, priority, title, tags) + the `(creates: …)'
annotation; line 2: SCHEDULED / `:CATEGORY:' when set."
  (let ((item (org-air-inbox--form-get :item)))
    (if (not item)
        "Preview"
      (let* ((file (org-air-inbox--form-get :file))
             (olp (org-air-inbox--form-get :olp))
             (tags (org-air-inbox--form-get :tags))
             (todo (or (org-air-inbox--form-get :todo)
                       (org-air-item-todo item)))
             (pri (or (org-air-inbox--form-get :priority)
                      (org-air-inbox--item-priority-char item)))
             (sched (org-air-inbox--form-get :schedule-label))
             (cat (org-air-inbox--form-get :category))
             (dest (if file
                       (concat (file-name-nondirectory file)
                               (and olp
                                    (concat " › "
                                            (mapconcat #'identity olp " › "))))
                     "(no destination — f)"))
             (head (concat "* "
                           (and todo (concat todo " "))
                           (and pri (format "[#%c] " pri))
                           (org-air-item-title item)
                           (and tags
                                (concat " :" (mapconcat #'identity tags ":")
                                        ":"))))
             (extra (concat (and sched (concat "SCHEDULED: " sched))
                            (and sched cat "  ·  ")
                            (and cat (concat ":CATEGORY: " cat)))))
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
    (org-air-inbox--form-put :tags (org-air-inbox--edit-tags probe))))

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
                                  (cdr picks)))))))

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
                                  (list "someday")))))
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

(transient-define-suffix org-air-refile-form-todo ()
  "Pick the TODO keyword from the DESTINATION file's own vocabulary (R57)."
  :transient t
  :description (lambda ()
                 (org-air-inbox--form-field
                  "todo"
                  (or (org-air-inbox--form-get :todo)
                      (let ((item (org-air-inbox--form-get :item)))
                        (and item (org-air-item-todo item))))))
  (interactive)
  (let* ((item (org-air-inbox--form-get :item))
         (vocab (or (org-air-inbox--target-todo-keywords
                     (org-air-inbox--form-get :file))
                    org-todo-keywords-1))
         (choice (completing-read
                  "Todo (empty leaves untouched): " vocab nil nil nil nil
                  (or (org-air-inbox--form-get :todo)
                      (org-air-item-todo item)))))
    (org-air-inbox--form-put :todo (unless (string-empty-p choice) choice))))

(transient-define-suffix org-air-refile-form-priority ()
  "Cycle the priority: – → A → … → lowest → – (destination file's range)."
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
                 (org-air-inbox--form-get :file)))
         (current (or (org-air-inbox--form-get :priority)
                      (let ((item (org-air-inbox--form-get :item)))
                        (and item (org-air-inbox--item-priority-char item)))))
         (next (cond ((null current) (car range))
                     ((>= current (cdr range)) nil)
                     (t (1+ current)))))
    (org-air-inbox--form-put :priority next)))

(transient-define-suffix org-air-refile-form-execute ()
  "Execute ONE `org-air-refile-item' call from the collected form."
  (interactive)
  (let* ((item (org-air-inbox--form-get :item))
         (file (org-air-inbox--form-get :file))
         (olp (org-air-inbox--form-get :olp))
         (tags (org-air-inbox--form-get :tags)))
    (unless file
      (user-error "No destination yet — pick a file with f"))
    (org-air-refile-item item file olp
                         (or tags :none)
                         (org-air-inbox--form-get :scheduled)
                         (org-air-inbox--form-get :category)
                         (org-air-inbox--form-get :todo)
                         (org-air-inbox--form-get :priority))
    (setq org-air-inbox--refile-last (cons file olp))
    (setq org-air-inbox--refile-form nil)))

;;;###autoload (autoload 'org-air-refile-transient "org-air-inbox" nil t)
(transient-define-prefix org-air-refile-transient ()
  "One-shot refile form for the item at point (R64-3).
Every field is visible with its current value, editable in any order;
the Preview group re-renders live; RET executes ONE
`org-air-refile-item' call; \\`C-g' / q abandon everything (no buffer
was touched — creation is deferred to execute, R64-2)."
  [:description org-air-inbox--form-heading
   ["Destination"
    ("f" org-air-refile-form-file)
    ("p" org-air-refile-form-path)
    ("l" org-air-refile-form-last)]
   ["Metadata"
    ("t" org-air-refile-form-tags)
    ("c" org-air-refile-form-category)
    ("s" org-air-refile-form-schedule)
    ("k" org-air-refile-form-todo)
    ("," org-air-refile-form-priority)]]
  [:description org-air-inbox--form-preview
   ("RET" "refile" org-air-refile-form-execute)
   ("q" "quit" transient-quit-one)]
  (interactive)
  (when noninteractive
    (user-error "The refile form is interactive-only; call `org-air-refile-item' with arguments in batch"))
  (org-air-inbox--form-init (org-air-inbox--interactive-item))
  (transient-setup 'org-air-refile-transient))

(provide 'org-air-inbox)

;; Local Variables:
;; package-lint-main-file: "org-air.el"
;; End:
;;; org-air-inbox.el ends here
