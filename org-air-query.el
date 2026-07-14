;;; org-air-query.el --- Org-QL data layer for org-air -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: org-air contributors
;; Keywords: outlines, calendar
;; Version: 0.1.0
;; URL: https://github.com/rytswd/org-air
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Normalise Org headings from `org-air-files' into `org-air-item' records.
;;
;; R53: the scan is a WORK-BUFFER scan — org-ql stays the only query
;; engine, but org-air now hands it buffers IT manages (one reused work
;; buffer per session, or a user's live buffer) instead of letting it
;; `find-file-noselect' every file.  That kills the measured O(n^2)
;; `buffer-list' cost (271.8s -> 3.41s at 5006 files), retains ZERO source
;; buffers, and lets the per-file body be wrapped in the never-error law:
;; a signalling file (encrypted, unreadable, binary, vanished) contributes
;; 0 items and one skip-log entry — it can NEVER abort a whole scan.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-ql)
(require 'seq)

(defvar org-air-files)
(defvar org-air-inbox-file)

(defcustom org-air-todo-keywords
  '(:not-done ("TODO" "NEXT" "STARTED" "WAIT" "WAITING" "HOLD" "BLOCKED")
    :done     ("DONE" "CANCELLED" "CANCELED" "KILL"))
  "TODO keyword vocabulary org-air recognises when a file declares none.
The active (:not-done) and :done keyword sets org-air falls back to so a
heading like `* NEXT Foo' is parsed as a NEXT task even in a file without
a `#+TODO:' line.  A file's OWN `#+TODO:'/`#+SEQ_TODO:' always wins; this
only fills the gap.  Defaults mirror the keys of
`org-air-todo-keyword-faces' plus the standard done keywords (R21-3)."
  :type '(plist :key-type symbol :value-type (repeat string))
  :group 'org-air)

(defcustom org-air-note-type-tag-alist
  '(("task" . task) ("note" . knowledge) ("journal" . journal))
  "Tags that OVERRIDE the derived note type (R54-2, optional).
Alist of TAG (string, matched case-insensitively) to TYPE (`task',
`knowledge' — `note' accepted as a synonym — or `journal').  Matched
against an item's tags, which already include inherited `#+filetags', so
a `:note:' file tag (or denote-journal's `journal' keyword) types every
heading with zero mechanics.  Users who tag with `kb'/`evergreen' add one
entry.  Purely an escape hatch: the content-derived model needs no
tagging at all."
  :type '(alist :key-type string :value-type symbol)
  :group 'org-air)

(defcustom org-air-journal-directory-regexp
  "\\`\\(?:journal\\|diary\\|daily\\)\\'"
  "Regexp a PATH COMPONENT must match for a file to type `journal' (R54-2).
One of the journal sub-heuristic's three signals (with a date-shaped file
name and a date-shaped `#+title'); matched case-insensitively against
each directory component of the scanned file's path."
  :type 'regexp
  :group 'org-air)

(defcustom org-air-plain-heading-type 'knowledge
  "Type derived for a plain heading with no task signal (R54-2 step 6).
The USER-RULED default `knowledge' keeps dateless prose off the GTD board
\(everything else is a KNOWLEDGE note).  The legacy value `task'
restores the pre-R54 behaviour where every dateless heading was board
material (Needs attention by default) — for GTD purists whose bare
section headings must stay tasks."
  :type '(choice (const knowledge) (const task))
  :group 'org-air)

(defcustom org-air-max-file-size (* 4 1024 1024)
  "Largest file (bytes) the background scan will read; nil = no limit (R53).
A file over the limit is skipped with a `too-large' entry in the scan
report (`org-air-scan-report') instead of stalling a slice — the generic
monster-file valve of the never-hang contract."
  :type '(choice (const :tag "No limit" nil) integer)
  :group 'org-air)

(defun org-air-query--scan-todo-keywords ()
  "Return an `org-todo-keywords' value merging org-air's vocabulary (R21-3).
One sequence: the :not-done keywords, then `|', then the :done keywords.
Let-bound around the org-ql scan so a file WITHOUT its own `#+TODO:'
inherits org-air's NEXT/WAIT/... vocabulary (otherwise the keyword is
swallowed into the title), while a file WITH a `#+TODO:'/`#+SEQ_TODO:'
line still parses with its own (Org's per-file keywords win over the
default)."
  `((sequence
     ,@(plist-get org-air-todo-keywords :not-done)
     "|"
     ,@(plist-get org-air-todo-keywords :done))))

(cl-defstruct (org-air-item
               (:constructor org-air-item-create)
               (:copier nil))
  "A normalised Org heading (or R53 note file) for org-air views.
R53 P2 (cache v2): `kind', `donep', `activity' and `body-deadline' are
SCAN-TIME slots — everything classify/render needs lives in the struct,
so painting a cache-hydrated board never opens a file."
  title tags file marker todo priority scheduled deadline group closed
  ;; R53 scan-time slots (data-pure render):
  kind          ; 'heading | 'file (P3 headingless note file-item)
  donep         ; non-nil when todo ∈ the file's own `org-done-keywords'
  activity      ; epoch float: closed‖scheduled‖deadline‖first subtree ts‖mtime
  subtree-ts    ; epoch float of the first timestamp in the subtree BODY,
                ; or nil (R53fix B1: the day view's Logged/created key —
                ; distinct from `activity', whose mtime fallback must
                ; never fill that group)
  body-deadline ; epoch float of the first subtree DEADLINE: when the
                ; heading itself has none (the calendar's origin check)
  ;; R54 scan-time slots (cache v4):
  active-ts     ; epoch float of the first ACTIVE <ts> in the subtree
                ; (`org-ts-regexp': planning lines in, inactive [..] out)
                ; — the R54-1 stale-eligibility signal, distinct from
                ; `subtree-ts' (regexp-both, the day view's key)
  ntype)        ; 'task | 'journal | 'knowledge — the R54-2 content-
                ; derived note type; nil on items built outside the scan
                ; (treated as task by the classify routing)

(defun org-air-query--org-file-p (file)
  "Return non-nil when FILE is an Org file."
  (and (stringp file)
       (file-regular-p file)
       (string-match-p "\\.org\\(?:\\.gpg\\)?\\'" file)))

(defun org-air-query--expand-source (source)
  "Expand SOURCE, which may be a file or directory, to Org files."
  (let ((path (expand-file-name source)))
    (cond
     ((file-directory-p path)
      (directory-files-recursively path "\\.org\\(?:\\.gpg\\)?\\'" nil))
     ((org-air-query--org-file-p path) (list path))
     (t nil))))

(defun org-air-query-files ()
  "Return all existing Org files configured in `org-air-files'.
R53 P1d: order-preserving hash-table dedupe; `file-truename' is paid ONLY
for actual symlinks (`file-symlink-p' pre-check) so a 5000-file tree
enumerates in milliseconds while a symlinked duplicate still dedupes to
its target (measured 0.647s -> 0.044s at 5006 files)."
  (let ((seen (make-hash-table :test #'equal))
        (out nil))
    (dolist (file (seq-mapcat #'org-air-query--expand-source org-air-files))
      (let ((path (if (file-symlink-p file)
                      (or (ignore-errors (file-truename file)) file)
                    file)))
        (unless (gethash path seen)
          (puthash path t seen)
          (push path out))))
    (nreverse out)))

(defun org-air-query--timestamp (property)
  "Return Org timestamp object for PROPERTY at point, or nil."
  (when-let* ((value (org-entry-get (point) property)))
    (ignore-errors (org-timestamp-from-string value))))

(defun org-air-query--time-float (timestamp)
  "Return TIMESTAMP (an Org timestamp object) as an epoch float, or nil."
  (when timestamp
    (ignore-errors (float-time (org-timestamp-to-time timestamp)))))

(defun org-air-query--group (file)
  "Return display group for heading in FILE."
  (or (org-entry-get (point) "CATEGORY")
      (file-name-base file)))

(defvar org-air-query--scan-mtime nil
  "The scanned file's modification time, bound per file by the scan (R53).
Lets `org-air-query--item-at-point' seed the `activity' slot's mtime
fallback from the stat the scan already paid, instead of a per-item
re-stat.")

;;;; ---------------------------------------------------------------------
;;;; R54-2 — the content-derived note-type model + denote READ compat.
;;;; org-air reads denote's ON-DISK conventions only (ID file names,
;;;; `#+title'/`#+filetags' front matter, `denote:' links); it NEVER calls
;;;; a denote-* function and works with denote absent.
;;;; ---------------------------------------------------------------------

(defconst org-air-query--denote-id-regexp
  "\\`\\([0-9]\\{8\\}T[0-9]\\{6\\}\\)"
  "Anchored regexp capturing the denote identifier of a file NAME (R54-2).
The view's F1 regexp promoted into the query layer, loosened to not
require the `--' separator so a date-only journal name
\(`20260715T000000.org') matches too.")

(defun org-air-query--denote-file-id (file)
  "Return FILE's denote identifier (\"YYYYMMDDTHHMMSS\"), or nil (R54-2)."
  (let ((base (file-name-nondirectory (or file ""))))
    (when (string-match org-air-query--denote-id-regexp base)
      (match-string 1 base))))

(defun org-air-query--denote-id-time (id)
  "Parse denote identifier ID to an epoch float, or nil (R54-2)."
  (when (and (stringp id) (string-match-p "\\`[0-9]\\{8\\}T[0-9]\\{6\\}\\'" id))
    (ignore-errors
      (float-time
       (encode-time (string-to-number (substring id 13 15))
                    (string-to-number (substring id 11 13))
                    (string-to-number (substring id 9 11))
                    (string-to-number (substring id 6 8))
                    (string-to-number (substring id 4 6))
                    (string-to-number (substring id 0 4)))))))

(defun org-air-query--denote-slug (file)
  "Return FILE's raw denote title slug (hyphens kept), or nil (R54-2).
Data-layer twin of the view's F1 de-slug: the part between the `--'
separator and any `__tag' signature."
  (let ((base (file-name-nondirectory (or file ""))))
    (when (string-match (concat org-air-query--denote-id-regexp "--") base)
      (let* ((rest (file-name-sans-extension (substring base (match-end 0))))
             (slug (if (string-match "__" rest)
                       (substring rest 0 (match-beginning 0))
                     rest)))
        (unless (string-empty-p slug) slug)))))

(defun org-air-query--denote-filename-tags (file)
  "Return the `__tag_tag' keywords of FILE's denote name, or nil (R54-2).
Only the FALLBACK for files missing `#+filetags' front matter (denote
keeps the two in sync, so this is rare)."
  (let ((base (file-name-sans-extension
               (file-name-nondirectory (or file "")))))
    (when (and (string-match-p org-air-query--denote-id-regexp base)
               (string-match "__" base))
      (split-string (substring base (match-end 0)) "_" t))))

(defun org-air-query--parse-type (value)
  "Normalise VALUE (a string or symbol) to a note type symbol, or nil.
`note' and `knowledge' are synonyms; invalid values are IGNORED (fall
through the R54-2 precedence chain), never an error."
  (let ((sym (cond ((symbolp value) value)
                   ((stringp value) (intern (downcase (string-trim value))))
                   (t nil))))
    (pcase sym
      ('task 'task)
      ((or 'note 'knowledge) 'knowledge)
      ('journal 'journal)
      (_ nil))))

(defun org-air-query--tag-type (tags)
  "Return the type a tag in TAGS overrides to, or nil (R54-2 step 3).
First match in `org-air-note-type-tag-alist' wins (case-insensitive)."
  (cl-some (lambda (tag)
             (org-air-query--parse-type
              (cdr (assoc-string tag org-air-note-type-tag-alist t))))
           tags))

(defun org-air-query--date-shaped-name-p (base)
  "Non-nil when file name BASE is date-shaped (R54-2 journal heuristic).
ISO (`2026-07-15'), compact (`20260715'), or a denote ID-only name."
  (or (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" base)
      (string-match-p "\\`[0-9]\\{8\\}\\'" base)
      (string-match-p "\\`[0-9]\\{8\\}T[0-9]\\{6\\}\\'" base)))

(defun org-air-query--date-shaped-title-p (title)
  "Non-nil when TITLE is date-shaped (R54-2 journal heuristic).
ISO (`2026-07-15') or the denote-journal long form (`Tuesday 15 July
2026')."
  (and (stringp title)
       (let ((case-fold-search t))
         (or (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" title)
             (string-match-p
              "\\`[[:alpha:]]+day,?[ \t]+[0-9]\\{1,2\\}[ \t]+[[:alpha:]]+[ \t]+[0-9]\\{4\\}\\'"
              title)))))

(defun org-air-query--journal-file-p (file title)
  "Return non-nil when FILE (with `#+title' TITLE) is a journal (R54-2).
File-level, computed once per file: a date-shaped file name, a path
component matching `org-air-journal-directory-regexp', or a date-shaped
TITLE."
  (let ((case-fold-search t))
    (or (org-air-query--date-shaped-name-p (file-name-base (or file "")))
        (cl-some (lambda (component)
                   (string-match-p org-air-journal-directory-regexp component))
                 (split-string (or (file-name-directory (or file "")) "")
                               "/" t))
        (org-air-query--date-shaped-title-p title))))

(defvar org-air-query--scan-file-signals nil
  "The scanned file's per-FILE type signals, bound per file by the scan.
A plist (:override TYPE :journal FLAG :title TITLE :tags TAGS :created
FLOAT :ids IDS :links LINKS) computed ONCE per file by
`org-air-query--file-signals' — not per heading — and threaded to
`org-air-query--note-type' (R54-2; :ids/:links feed the R54-3 link
graph).")

(defun org-air-query--note-link (target)
  "Normalise raw bracket-link TARGET to a note-link string, or nil (R54-3).
Only the three note-link kinds survive: `denote:ID', `id:UUID' and
`file:' links landing on `.org' files (explicit `file:' prefix, or a
bare untyped bracket target naming an .org file — Org's own file-link
fallback); `https:' and every other link type returns nil (a web-linked
note can still be an orphan in the garden sense).  A `::search' suffix
is tolerated and stripped."
  (let ((target (car (split-string (or target "") "::"))))
    (cond
     ((string-prefix-p "denote:" target) target)
     ((string-prefix-p "id:" target) target)
     ((string-prefix-p "file:" target)
      (and (string-match-p "\\.org\\'" target) target))
     ((and (not (string-match-p "\\`[[:alpha:]][[:alnum:]+.-]*:" target))
           (string-match-p "\\.org\\'" target))
      (concat "file:" target))
     (t nil))))

(defun org-air-query--file-signals (file)
  "Compute FILE's per-file type signals in the current scan buffer (R54-2).
Bounded head-of-buffer regexps only (first 4KB): the `#+org_air_type:' /
`#+type:' keyword override (the namespaced spelling authoritative when
both are present), `#+title', `#+filetags', the journal heuristic and
the created date (denote filename ID, else `#+date:').  Returns the
plist documented on `org-air-query--scan-file-signals'."
  (org-with-wide-buffer
   (goto-char (point-min))
   (let* ((case-fold-search t)
          (bound (min (point-max) 4096))
          (title (save-excursion
                   (when (re-search-forward
                          "^#\\+title:[ \t]*\\(.+?\\)[ \t]*$" bound t)
                     (match-string-no-properties 1))))
          (tags (save-excursion
                  (when (re-search-forward
                         "^#\\+filetags:[ \t]*\\(.+?\\)[ \t]*$" bound t)
                    (split-string (match-string-no-properties 1)
                                  "[: \t]+" t))))
          (override
           (or (save-excursion
                 (when (re-search-forward
                        "^#\\+org_air_type:[ \t]*\\([^ \t\n]+\\)" bound t)
                   (org-air-query--parse-type
                    (match-string-no-properties 1))))
               (save-excursion
                 (when (re-search-forward
                        "^#\\+type:[ \t]*\\([^ \t\n]+\\)" bound t)
                   (org-air-query--parse-type
                    (match-string-no-properties 1))))))
          (created
           (or (org-air-query--denote-id-time
                (org-air-query--denote-file-id file))
               (save-excursion
                 (when (re-search-forward
                        "^#\\+date:[ \t]*\\(.+?\\)[ \t]*$" bound t)
                   (ignore-errors
                     (float-time
                      (org-timestamp-to-time
                       (org-timestamp-from-string
                        (match-string-no-properties 1)))))))))
          ;; R54-3 link graph: ONE bounded whole-buffer pass while the
          ;; file is already in the work buffer — `:ID:' property values
          ;; (the id: link resolution targets) and the note-to-note
          ;; outbound links.  Scan-TIME extraction only; resolution is a
          ;; finish-time PURE pass over the in-memory table
          ;; (`org-air-query--link-graph-finish') — never at render time,
          ;; never a file open.
          (ids (save-excursion
                 (let (acc)
                   (while (re-search-forward
                           "^[ \t]*:ID:[ \t]+\\([^ \t\n]+\\)" nil t)
                     (push (match-string-no-properties 1) acc))
                   (nreverse acc))))
          (links (save-excursion
                   (let (acc)
                     (while (re-search-forward org-link-bracket-re nil t)
                       (when-let* ((link (org-air-query--note-link
                                          (match-string-no-properties 1))))
                         (push link acc)))
                     (nreverse acc)))))
     (list :override override
           :journal (and (org-air-query--journal-file-p file title) t)
           :title title
           :tags tags
           :created created
           :ids ids
           :links links))))

(defun org-air-query--note-type (todo scheduled deadline tags)
  "Derive the note type for the heading at point (R54-2, USER-RULED).
Precedence: the inherited `ORG_AIR_TYPE' property, the file keyword
override, an override tag (`org-air-note-type-tag-alist'), the TASK
signal (a TODO keyword — done or not — OR scheduled OR deadline;
nothing else — a bare active <ts> is a note fact, not a task), the
journal file heuristic, else `org-air-plain-heading-type'.  TODO,
SCHEDULED, DEADLINE and TAGS are the already-parsed heading signals."
  (or (org-air-query--parse-type
       (org-entry-get (point) "ORG_AIR_TYPE" t))
      (plist-get org-air-query--scan-file-signals :override)
      (org-air-query--tag-type tags)
      (and (or todo scheduled deadline) 'task)
      (and (plist-get org-air-query--scan-file-signals :journal) 'journal)
      org-air-plain-heading-type))

(defun org-air-query--file-ntype (signals items)
  "Return the FILE-level type from SIGNALS and its heading ITEMS (R54-2).
Override → tag override → journal → `task' iff EVERY heading item is a
task (and there is at least one — the F7 mixed-file rule: a pure GTD
file stays off the note surfaces while a KB note containing one TODO
stays a knowledge FILE) → else `knowledge'."
  (or (plist-get signals :override)
      (org-air-query--tag-type (plist-get signals :tags))
      (and (plist-get signals :journal) 'journal)
      (and items
           (cl-every (lambda (item)
                       (eq (org-air-item-ntype item) 'task))
                     items)
           'task)
      'knowledge))

(defvar org-air-query--file-meta (make-hash-table :test #'equal)
  "The per-file fact table: FILE → plist (R54-2, cache v4).
Keys: `:title' (`#+title', else the denote slug, else nil — `:org-title'
holds the raw `#+title' alone so display fallbacks stay exact), `:tags'
\(`#+filetags', else the filename `__tags' fallback), `:ntype' (the
FILE's type, F7 rule), `:mtime' and `:created' (epoch floats).  R54-3
link-graph keys: `:ids' (the file's `:ID:' property values), `:links-raw'
\(scan-time outbound note links, unresolved), `:links-out' (the resolved
outbound list — FILE paths where resolvable, the raw link string where
not: unresolvable intent counts outbound but creates no inbound) and
`:links-in' (the inbound count, one pure inversion pass at scan finish).
Updated per scanned file; persisted in the cache as `:file-meta' and
hydrated back on cache load, so a warm board answers file-level
questions with ZERO file opens.")

(defvar org-air-query--link-graph-dirty nil
  "Non-nil when file-meta gained scan entries since the last resolution.
Set by `org-air-query--file-meta-record'; cleared by the pure
`org-air-query--link-graph-finish' pass (R54-3).")

(defvar org-air-query--denote-id-index (make-hash-table :test #'equal)
  "Index denote ID → FILE for the read-only `denote:' link shim (R54-2).
Pure filename derivation, populated as the scan enumerates files and
re-derived from the cache's `:file-meta' keys on hydration.")

(defun org-air-query-file-meta (file)
  "Return the recorded per-file plist for FILE, or nil (R54-2)."
  (gethash file org-air-query--file-meta))

(defun org-air-query--index-denote-id (file)
  "Record FILE under its denote identifier, when it carries one (R54-2)."
  (when-let* ((id (org-air-query--denote-file-id file)))
    (puthash id file org-air-query--denote-id-index)))

(defun org-air-query--file-meta-record (file signals items)
  "Record FILE's per-file facts from SIGNALS and its heading ITEMS (R54-2)."
  (puthash file
           (list :title (or (plist-get signals :title)
                            (org-air-query--denote-slug file))
                 :org-title (plist-get signals :title)
                 :tags (or (plist-get signals :tags)
                           (org-air-query--denote-filename-tags file))
                 :ntype (org-air-query--file-ntype signals items)
                 :mtime (when-let* ((mtime
                                     (or org-air-query--scan-mtime
                                         (and (file-exists-p file)
                                              (file-attribute-modification-time
                                               (file-attributes file))))))
                          (float-time mtime))
                 :created (plist-get signals :created)
                 ;; R54-3: the raw link-graph facts; resolution is the
                 ;; finish-time pure pass (`--link-graph-finish').
                 :ids (plist-get signals :ids)
                 :links-raw (plist-get signals :links))
           org-air-query--file-meta)
  (setq org-air-query--link-graph-dirty t))

(defun org-air-query--link-graph-finish ()
  "Resolve the note-link graph over the in-memory file-meta table (R54-3).
A PURE pass — zero file opens: builds the denote-ID / `:ID:' / normalised
path indexes from the table itself, resolves every file's `:links-raw'
into `:links-out' (resolved targets become FILE paths; an unresolvable
note link keeps its raw string — outbound intent with no inbound edge;
self-links are dropped) and computes `:links-in' by one in-memory
inversion.  Never called at render time per row — the Revisit view runs
it at most once per dirty table (`org-air-query-link-graph-ensure')."
  (let ((ids (make-hash-table :test #'equal))
        (paths (make-hash-table :test #'equal))
        (inbound (make-hash-table :test #'equal)))
    (maphash (lambda (file meta)
               (puthash (expand-file-name file) file paths)
               (dolist (id (plist-get meta :ids))
                 (puthash id file ids))
               (org-air-query--index-denote-id file))
             org-air-query--file-meta)
    (maphash
     (lambda (file meta)
       (let (out)
         (dolist (raw (plist-get meta :links-raw))
           (let* ((resolved
                   (cond
                    ((string-prefix-p "denote:" raw)
                     (gethash (substring raw (length "denote:"))
                              org-air-query--denote-id-index))
                    ((string-prefix-p "id:" raw)
                     (gethash (substring raw (length "id:")) ids))
                    ((string-prefix-p "file:" raw)
                     (let ((path (substring raw (length "file:"))))
                       (gethash (expand-file-name
                                 path (file-name-directory file))
                                paths)))))
                  (target (or resolved raw)))
             (unless (or (equal target file) (member target out))
               (push target out)
               (when resolved
                 (cl-incf (gethash resolved inbound 0))))))
         (puthash file (plist-put meta :links-out (nreverse out))
                  org-air-query--file-meta)))
     org-air-query--file-meta)
    (maphash (lambda (file meta)
               (puthash file
                        (plist-put meta :links-in (gethash file inbound 0))
                        org-air-query--file-meta))
             org-air-query--file-meta))
  (setq org-air-query--link-graph-dirty nil))

(defun org-air-query-link-graph-ensure ()
  "Run the link-graph resolution iff the table gained scans (R54-3).
Idempotent and cheap (one in-memory pass over the file-meta table);
safe to call before any consumer read (the Revisit ORPHANS mode, the
cache serialisation)."
  (when org-air-query--link-graph-dirty
    (org-air-query--link-graph-finish)))

(defun org-air-query-file-meta-alist (files)
  "Return the file-meta entries for FILES as a printable alist (R54-2).
The cache serialisation form: pruned to FILES, so vanished files never
persist.  R54-3: the link graph is resolved first, so the persisted
entries carry `:links-out'/`:links-in' and a warm ORPHANS render is
data-pure with no resolution pass."
  (org-air-query-link-graph-ensure)
  (let (out)
    (dolist (file files)
      (when-let* ((meta (gethash file org-air-query--file-meta)))
        (push (cons file meta) out)))
    (nreverse out)))

(defun org-air-query-file-meta-hydrate (alist)
  "Hydrate the file-meta table (and denote index) from cache ALIST (R54-2)."
  (pcase-dolist (`(,file . ,meta) alist)
    (when (and (stringp file) (listp meta))
      (puthash file meta org-air-query--file-meta)
      (org-air-query--index-denote-id file))))

;;;; ---------------------------------------------------------------------
;;;; R54-3 — the bounded VISIT LEDGER (opt-in after the D2 ruling).
;;;; org-air records opens IT initiates (board S-RET / g RET, the pane
;;;; RET, revisit RET) — NEVER a global `find-file' hook: org-air does
;;;; not instrument buffers it does not own.
;;;; ---------------------------------------------------------------------

(defvar org-air-query--visits (make-hash-table :test #'equal)
  "The visit ledger: FILE → epoch float of the last org-air open (R54-3).
Written only when `org-air-revisit-visit-ledger' is non-nil (the D2
ruling demoted the ledger to OPT-IN; age is pure mtime by default).
Persisted in the cache as `:visits' (alist) and hydrated back; BOUNDED:
pruned to the enumerated file set at cache write, so its size can never
exceed the configured file count.")

(defun org-air--note-visited (file)
  "Record an org-air-initiated open of FILE in the visit ledger (R54-3).
A no-op unless `org-air-revisit-visit-ledger' is non-nil (USER-RULED D2:
last-modified is the default attention-age signal; the ledger is the
opt-in refinement).  Called only from org-air's OWN open paths."
  (when (and (bound-and-true-p org-air-revisit-visit-ledger)
             (stringp file) (not (string-empty-p file)))
    (puthash file (float-time) org-air-query--visits)))

(defun org-air-query-note-visit (file)
  "Return the ledger epoch float of FILE's last org-air open, or nil."
  (gethash file org-air-query--visits))

(defun org-air-query-visits-alist (files)
  "Return the visit ledger pruned to FILES as a printable alist (R54-3).
The cache serialisation form — the prune IS the bound: entries for files
no longer enumerated are dropped here and, since this feeds the write,
never persist."
  (let (out)
    (dolist (file files)
      (when-let* ((time (gethash file org-air-query--visits)))
        (push (cons file time) out)))
    (nreverse out)))

(defun org-air-query-visits-hydrate (alist)
  "Hydrate the visit ledger from cache ALIST (R54-3)."
  (pcase-dolist (`(,file . ,time) alist)
    (when (and (stringp file) (numberp time))
      (puthash file time org-air-query--visits))))

(defun org-air-query--denote-resolve (id)
  "Resolve denote identifier ID to a configured file, or nil (R54-2).
An O(1) hit on the scan's ID index, else one bounded pass over the
enumerated file list (a cold Emacs following a link before any scan)."
  (or (gethash id org-air-query--denote-id-index)
      (cl-find-if (lambda (file)
                    (string-prefix-p id (file-name-nondirectory file)))
                  (ignore-errors (org-air-query-files)))))

(defun org-air-query--denote-follow (link &optional _prefix)
  "Follow a `denote:' LINK read-only against `org-air-files' (R54-2).
Resolves the ID by filename convention (no denote required, no DB); a
`::search' suffix is tolerated but ignored.  Authoring (creation,
renaming, completion) stays denote's — this shim only keeps existing
links alive in a denote-less Emacs."
  (let* ((id (car (split-string (or link "") "::")))
         (file (org-air-query--denote-resolve id)))
    (if file
        (find-file file)
      (user-error "No note with denote ID %s under `org-air-files'" id))))

(defun org-air-query-register-denote-link ()
  "Register the read-only `denote:' follower IFF none exists (R54-2).
When denote (or anything else) already claims the link type, org-air
leaves it alone — never a `denote-*' call, works with denote absent."
  (unless (org-link-get-parameter "denote" :follow)
    (org-link-set-parameters "denote"
                             :follow #'org-air-query--denote-follow)))

(org-air-query-register-denote-link)

(defun org-air-query--item-at-point ()
  "Build an `org-air-item' for the heading at point.
R53 P2: also records the scan-time slots (`kind'/`donep'/`activity'/
`body-deadline') so classify/render never open the file again, and the
marker slot is the durable (FILE . POS) cons (source buffers are never
retained by scanning; live positions resolve on demand)."
  (let* ((file (or (buffer-file-name) ""))
         ;; R23-1: `org-get-heading' returns a FONTIFIED title (with `face
         ;; org-level-1') once the source Org buffer is live + fontified
         ;; (e.g. after a refile).  Strip all text-properties at the data
         ;; layer so the struct title is a plain string and no caller leaks
         ;; org heading faces into the calm one-line row (V6 pixel-lock).
         (title (substring-no-properties (org-get-heading t t t t)))
         (todo (org-get-todo-state))
         (tags (org-get-tags nil nil))
         (scheduled (org-air-query--timestamp "SCHEDULED"))
         (deadline (org-air-query--timestamp "DEADLINE"))
         (closed (org-air-query--timestamp "CLOSED"))
         (subtree-ts nil)
         (active-ts nil)
         (body-deadline nil))
    ;; R53 P2: the two bounded subtree probes, run HERE in the already-
    ;; positioned scan buffer (they used to be per-item render-time file
    ;; opens — the 186s warm-paint hang).
    (save-excursion
      (let ((end (save-excursion (ignore-errors (org-end-of-subtree t t))
                                 (point))))
        (save-excursion
          (when (re-search-forward org-ts-regexp-both end t)
            (setq subtree-ts
                  (ignore-errors
                    (float-time
                     (org-timestamp-to-time
                      (org-timestamp-from-string
                       (match-string-no-properties 0))))))))
        ;; R54-1: the ACTIVE-only twin probe (`org-ts-regexp': planning
        ;; lines included, inactive [..] excluded) — the stale-eligibility
        ;; signal.  Distinct from `subtree-ts' (regexp-both), which the
        ;; day view's Logged/created group needs and which must keep
        ;; matching inactive stamps.
        (save-excursion
          (when (re-search-forward org-ts-regexp end t)
            (setq active-ts
                  (ignore-errors
                    (float-time
                     (org-timestamp-to-time
                      (org-timestamp-from-string
                       (match-string-no-properties 0))))))))
        (unless deadline
          (save-excursion
            (when (re-search-forward org-deadline-time-regexp end t)
              (setq body-deadline
                    (ignore-errors
                      (float-time
                       (org-timestamp-to-time
                        (org-timestamp-from-string
                         (format "<%s>"
                                 (match-string-no-properties 1))))))))))))
    (org-air-item-create
     :title title
     :tags tags
     :file file
     ;; R53 P1: (FILE . POS), first-class everywhere since R26-8 — the
     ;; scan retains NO buffer.  A file-less buffer (a test temp buffer)
     ;; keeps the live marker so at-point flows still resolve.
     :marker (if (string-empty-p file)
                 (copy-marker (point-marker))
               (cons file (point)))
     :todo todo
     ;; R22-1: detect an EXPLICIT [#X] cookie via `org-priority-regexp'
     ;; (group 2 = the letter, A..E), so [#B] is recorded even though its
     ;; value equals `org-default-priority' (=?B); a cookie-LESS heading
     ;; stays nil.  The old value-equals-default test dropped explicit [#B].
     :priority (let ((heading (org-get-heading t t nil t)))
                 (when (string-match org-priority-regexp heading)
                   (org-get-priority heading)))
     :scheduled scheduled
     :deadline deadline
     :closed closed
     :group (org-air-query--group file)
     :kind 'heading
     :donep (and todo (member todo org-done-keywords) t)
     :subtree-ts subtree-ts
     :activity (or (org-air-query--time-float closed)
                   (org-air-query--time-float scheduled)
                   (org-air-query--time-float deadline)
                   subtree-ts
                   (when-let* ((mtime
                                (or org-air-query--scan-mtime
                                    (and (not (string-empty-p file))
                                         (file-exists-p file)
                                         (file-attribute-modification-time
                                          (file-attributes file))))))
                     (float-time mtime)))
     :body-deadline body-deadline
     :active-ts active-ts
     ;; R54-2: the content-derived note type, computed here in the scan
     ;; buffer over signals already in hand (the per-FILE signals are
     ;; computed once per file, not per heading).
     :ntype (org-air-query--note-type todo scheduled deadline tags))))

;;;; ---------------------------------------------------------------------
;;;; R53 P1/P1b — the never-error work-buffer scan.
;;;; ---------------------------------------------------------------------

(defvar org-air-query--skip-log nil
  "Per-scan list of (FILE . REASON) entries the scan skipped (R53 P1b).
Cleared at the start of every full scan (`org-air-query-skip-log-reset');
listed by `org-air-scan-report'.  REASON is a symbol (`encrypted',
`too-large', `slow') or an error string.")

(defun org-air-query-skip-log-reset ()
  "Clear the per-scan skip log (R53 P1b).  Called once per scan start."
  (setq org-air-query--skip-log nil))

(defun org-air-query--skip (file reason)
  "Record FILE as skipped for REASON in the scan's skip log; return nil.
Never messages per file — the scan reports ONE summary line itself and
`org-air-scan-report' lists the details (no echo spam at 5000 files)."
  (push (cons file reason) org-air-query--skip-log)
  nil)

;;;###autoload
(defun org-air-scan-report ()
  "List the files the last org-air scan skipped, and why (R53 P1b)."
  (interactive)
  (if (null org-air-query--skip-log)
      (message "org-air: the last scan skipped no files")
    (let ((entries (reverse org-air-query--skip-log)))
      (with-current-buffer (get-buffer-create "*org-air scan report*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "org-air scan report — %d file(s) skipped\n\n"
                          (length entries)))
          (pcase-dolist (`(,file . ,reason) entries)
            (insert (format "  %-12s %s\n"
                            (if (symbolp reason) (symbol-name reason)
                              (format "%s" reason))
                            file)))
          (goto-char (point-min)))
        (special-mode)
        (pop-to-buffer (current-buffer))))))

(defvar org-air-query--work-buffer nil
  "The single reused scan work buffer, or nil (R53 P1).
A NORMAL-named buffer (org-ql drops space-prefixed ones), the mode
`org-mode' initialised ONCE per session under the
symbol `delay-mode-hooks' (mode hooks never run), element cache ON but
`org-element-cache-persistent' nil buffer-locally (org-persist stays out
of the shared buffer).  Killed by `org-air-query-teardown'.")

(defun org-air-query--work-buffer ()
  "Return the live scan work buffer, creating it on first use (R53 P1)."
  (unless (buffer-live-p org-air-query--work-buffer)
    (setq org-air-query--work-buffer (generate-new-buffer "*org-air scan*"))
    (with-current-buffer org-air-query--work-buffer
      (delay-mode-hooks (org-mode))
      (setq-local org-element-cache-persistent nil)))
  org-air-query--work-buffer)

(defun org-air-query-teardown ()
  "Kill the session's scan work buffer, if any (R53 P1)."
  (when (buffer-live-p org-air-query--work-buffer)
    (kill-buffer org-air-query--work-buffer))
  (setq org-air-query--work-buffer nil))

(defun org-air-query--inbox-file-p (file)
  "Return non-nil when FILE is `org-air-inbox-file' (P3 exclusion)."
  (and (boundp 'org-air-inbox-file)
       org-air-inbox-file
       (equal (or (ignore-errors (file-truename (expand-file-name file)))
                  (expand-file-name file))
              (or (ignore-errors
                    (file-truename (expand-file-name org-air-inbox-file)))
                  (expand-file-name org-air-inbox-file)))))

(defun org-air-query--file-item (file)
  "Return a one-item list for FILE as a headingless note, or nil (R53 P3).
Called with the scanned content in the current buffer AFTER the heading
scan yielded nothing.  A REAL note — no headings, some non-blank content,
no NUL byte in the first 1KB (binary junk never becomes a row), and not
the inbox file (its emptiness is chrome, not content) — synthesises ONE
openable item: `kind' `file', title from `#+title' (file name base
fallback), tags from `#+filetags', group = parent directory name, marker
\(FILE . 1) so RET opens the file at the top."
  (let ((case-fold-search t))
    (org-with-wide-buffer
     (goto-char (point-min))
     (unless (or (string-empty-p file)
                 (re-search-forward org-outline-regexp-bol nil t)
                 (save-excursion
                   (goto-char (point-min))
                   (search-forward "\0" (min (point-max) 1024) t))
                 (not (save-excursion
                        (goto-char (point-min))
                        (re-search-forward "[^ \t\r\n]" nil t)))
                 (org-air-query--inbox-file-p file))
       (goto-char (point-min))
       (let ((title (when (re-search-forward
                           "^#\\+title:[ \t]*\\(.+?\\)[ \t]*$" nil t)
                      (match-string-no-properties 1)))
             (tags (save-excursion
                     (goto-char (point-min))
                     (when (re-search-forward
                            "^#\\+filetags:[ \t]*\\(.+?\\)[ \t]*$" nil t)
                       (split-string (match-string-no-properties 1)
                                     "[: \t]+" t)))))
         (list
          (org-air-item-create
           :title (if (and title (not (string-empty-p title)))
                      title
                    (file-name-base file))
           :tags tags
           :file file
           :marker (cons file 1)
           :todo nil :priority nil
           :scheduled nil :deadline nil :closed nil
           :group (file-name-nondirectory
                   (directory-file-name (file-name-directory file)))
           :kind 'file
           :donep nil
           :activity (when-let* ((mtime
                                  (or org-air-query--scan-mtime
                                      (and (file-exists-p file)
                                           (file-attribute-modification-time
                                            (file-attributes file))))))
                       (float-time mtime))
           :body-deadline nil
           :active-ts nil
           ;; R54-2: 'file items type from the FILE-level signals alone
           ;; (keyword/tag override → journal → knowledge); they route to
           ;; the 'notes bucket regardless, so this feeds the note
           ;; surfaces, not the board.
           :ntype (org-air-query--file-ntype
                   org-air-query--scan-file-signals nil))))))))

(defun org-air-query--scan-live-buffer (buffer file query)
  "Scan the live user BUFFER visiting FILE with org-ql QUERY (R53 P1 rule 1).
Unsaved edits are respected; every item's marker/file slot is rewritten to
FILE so the (FILE . POS) contract and the mtime bookkeeping stay coherent
even when the buffer's own name differs (a symlinked visit)."
  (let* (;; R53fix M2: same echo hygiene as the work-buffer path — a live
         ;; headingless buffer must not re-spam org-ql's "No headings in
         ;; buffer" message on every refresh.
         (inhibit-message t)
         (message-log-max nil)
         ;; R54-2: the per-FILE type signals, computed ONCE per file.
         (org-air-query--scan-file-signals
          (with-current-buffer buffer
            (org-air-query--file-signals file)))
         (items (copy-sequence
                 (org-ql-select buffer (or query '(heading))
                   :action #'org-air-query--item-at-point))))
    (dolist (item items)
      (setf (org-air-item-file item) file)
      (let ((m (org-air-item-marker item)))
        (setf (org-air-item-marker item)
              (cons file (cond ((consp m) (or (cdr m) 1))
                               ((markerp m) (or (marker-position m) 1))
                               (t 1))))))
    (org-air-query--file-meta-record file org-air-query--scan-file-signals
                                     items)
    (or items
        (with-current-buffer buffer
          (org-air-query--file-item file)))))

(defun org-air-query--scan-work-buffer (file query)
  "Scan FILE in the reused work buffer with org-ql QUERY (R53 P1 rule 2).
One `erase-buffer' + `insert-file-contents' per file into the session's
single `org-mode' work buffer; the
variable `buffer-file-name' is set for the file's extent (so Org's
file-relative logic behaves) and always cleared again;
`org-set-regexps-and-options' makes the file's own
`#+TODO:' win, with the R21-3 default vocabulary otherwise.  Known,
accepted difference: file-local variable BLOCKS are not processed here
\(`#+…' keywords ARE); a file whose parsing genuinely depends on local
variables scans like the same Org file without them."
  (with-current-buffer (org-air-query--work-buffer)
    (let ((buffer-undo-list t)
          (create-lockfiles nil)
          ;; Kills org-ql's per-file "No headings in buffer" echo spam ×N;
          ;; the scan reports ONE summary line itself (R53 P1b).
          (inhibit-message t)
          (message-log-max nil)
          (org-todo-keywords (org-air-query--scan-todo-keywords))
          (org-air-query--scan-mtime
           (file-attribute-modification-time (file-attributes file))))
      (unwind-protect
          (progn
            (erase-buffer)
            (insert-file-contents file)
            (setq buffer-file-name file)
            (org-set-regexps-and-options)
            ;; R54-2: the per-FILE type signals, computed ONCE per file
            ;; and threaded to the per-heading action via the scan-scoped
            ;; binding (like `org-air-query--scan-mtime').
            (let* ((org-air-query--scan-file-signals
                    (org-air-query--file-signals file))
                   (items (copy-sequence
                           (org-ql-select (current-buffer)
                             (or query '(heading))
                             :action #'org-air-query--item-at-point))))
              (org-air-query--file-meta-record
               file org-air-query--scan-file-signals items)
              (or items (org-air-query--file-item file))))
        (setq buffer-file-name nil)
        (set-buffer-modified-p nil)))))

(defun org-air-query--scan-file (file &optional query)
  "Return `org-air-item' records for FILE; NEVER signals (R53 P1/P1b).
The one per-file scan entry: a live user buffer visiting FILE is scanned
in place (rule 1, cheap `get-file-buffer' — never a `buffer-list' walk);
otherwise the file scans in the single reused work buffer (rule 2).  The
policy table applies BEFORE any read: an `.org.gpg' with no live buffer
is skipped `encrypted' (the background scan NEVER decrypts or prompts; a
live already-decrypted buffer scans normally), an over-
`org-air-max-file-size' file is skipped `too-large', an unreadable /
vanished / dangling-symlink file is skipped with its `file-error'.  ANY
signal inside the body degrades to 0 items + one skip-log entry — a bad
file can never abort the whole scan (the P1b never-error law).  QUERY is
the optional org-ql query (default: all headings).  A `quit' is NOT
swallowed: aborting always works."
  (org-air-query--index-denote-id file)
  (condition-case err
      (let ((live (get-file-buffer file)))
        (cond
         (live (org-air-query--scan-live-buffer live file query))
         ((string-match-p "\\.gpg\\'" file)
          (org-air-query--skip file 'encrypted))
         ((not (file-readable-p file))
          (org-air-query--skip file 'unreadable))
         ((let ((size (file-attribute-size (file-attributes file))))
            (and org-air-max-file-size size
                 (> size org-air-max-file-size)))
          (org-air-query--skip file 'too-large))
         (t (org-air-query--scan-work-buffer file query))))
    (error (org-air-query--skip file (error-message-string err)))))

;;;###autoload
(defun org-air-query-items (&optional query)
  "Return `org-air-item' records matching org-ql QUERY.

When QUERY is nil, return all headings from `org-air-files' (plus one
bounded file-item per headingless note file, R53 P3).  R53 P1: the scan
loops `org-air-query--scan-file' over the configured files — org-ql stays
the only query engine, but it runs over buffers org-air manages, so no
source buffer is ever retained and one bad file can never abort the scan.
Item order is file order × buffer order, exactly as before."
  (let ((files (org-air-query-files)))
    (org-air-query-skip-log-reset)
    (let (items)
      (dolist (file files)
        (setq items (nconc items (org-air-query--scan-file file query))))
      items)))

(defun org-air-query-items-in-files (files &optional query)
  "Return `org-air-item' records for FILES, a subset of the configured set.

Like `org-air-query-items' but restricted to FILES (already-expanded Org
file paths), so the query can be split into batches and run on an idle
timer without blocking the frame (R19-1).  QUERY defaults to all
headings.  Does NOT reset the skip log — the caller (the refresh machine)
owns the per-scan log across its slices."
  (let (items)
    (dolist (file files)
      (setq items (nconc items (org-air-query--scan-file file query))))
    items))

(provide 'org-air-query)

;; Local Variables:
;; package-lint-main-file: "org-air.el"
;; End:
;;; org-air-query.el ends here
