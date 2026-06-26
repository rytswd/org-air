;;; org-air-project.el --- Air-docs project tree view for org-air -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: org-air contributors
;; Keywords: outlines, files
;; Version: 0.1.0
;; URL: https://github.com/rytswd/org-air
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; F5: render an Air-managed documentation tree (like `airctl status') as
;; a separate view from the GTD task board.  Air docs carry a state
;; (draft/ready/work-in-progress/review/complete/dropped), tags and a
;; title; this module reads them and (D-P5) renders them through
;; org-air-view's shared row primitive (`org-air-view--insert-row'): state
;; buckets become sections (icon + title + count badge), each doc a row
;; with the title LEFT and a date / tags / path cluster (the same D-P1
;; svg pills as the board).  Group by state / directory / tag.  The view
;; is buffer TEXT so it is fully byte-testable; the state badge degrades
;; from emoji to a `[D]'-style label off a graphical frame.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-air-faces)
(require 'org-air-layout)
(require 'org-air-view)
(require 'org-air-calendar)

;;;; ---------------------------------------------------------------------
;;;; Configuration (F5a)
;;;; ---------------------------------------------------------------------

(defcustom org-air-sources nil
  "Where org-air finds content.
A list of entries, each either:
 - a STRING directory of Org files (a GTD task board), or
 - (:air ROOT)  an Air project root rendered as a doc tree, or
 - (:files (F...)) explicit files.
Air projects are also AUTO-DETECTED: a source directory that contains
`air-config.toml' or an `air/' subdirectory is treated as (:air ...).
The existing `org-air-files'/`org-air-inbox-file' (the GTD board) remain;
`org-air-sources' is the newer unified entry point feeding both the task
board (Org dirs) and the project view (Air roots)."
  :type '(repeat sexp)
  :group 'org-air)

(defcustom org-air-projects nil
  "Explicit list of Air project roots for `org-air-project'.
When nil the roots are derived from `org-air-sources' (its `:air' entries
and auto-detected directories)."
  :type '(repeat directory)
  :group 'org-air)

(defcustom org-air-project-view-width nil
  "Fixed render width for the Air project view, or nil to use the window.
Integer pins an exact composition width (the batch/test seam, mirroring
`org-air-view-width'); nil derives the width from the live window."
  :type '(choice (const :tag "Live window" nil) integer)
  :group 'org-air)

(defcustom org-air-project-show-inspector t
  "When non-nil, the project view hosts a mid-rail inspector (R14 D-P1.B).
Mirrors `org-air-show-inspector' for the board: above
`org-air-rail-min-width' the view is two-pane (doc sections + a project
rail of Summary + Inspector); below it the view is board-only."
  :type 'boolean
  :group 'org-air)

(defcustom org-air-project-group 'directory
  "Default grouping for the Air project view: `state', `directory' or `tag'.
Mirrors `airctl status' -a / -Da / -Ta.  R20-5: the default is `directory'
— the NESTED directory tree that matches `airctl status -Da' (the most
useful view).  The `state' / `tag' modes stay reachable via the commands
`org-air-project-group-by-state' / `org-air-project-group-by-tag' (no key),
so they never shadow the shared board keys s / d / t."
  :type '(choice (const state) (const directory) (const tag))
  :group 'org-air)

(defconst org-air-project--state-display-order
  '("ready" "work-in-progress" "review" "complete" "dropped" "draft")
  "Canonical state order for the directory tree (R20-5), matching `airctl -Da'.
Drives BOTH the per-dir count badges (Ready · Work-In-Progress · Review ·
Complete · Dropped · Draft, only those present) AND the state-first
ordering of a directory's own docs, so the two can never drift.")

(defcustom org-air-project-sort-key 'name
  "INITIAL sort key for the Air project view (R16 D-P4).
Keys: `name', `created', `updated' (reserved for later: scheduled,
deadline).  The runtime commands (`o' cycle, `g s' select) override this
per-buffer via `org-air-project--sort-key'.  The resulting doc order is
the byte contract."
  :type '(choice (const name) (const created) (const updated))
  :group 'org-air)

(defcustom org-air-project-sort-direction 'ascending
  "INITIAL sort direction for the Air project view (R16 D-P4).
`O' toggles it per-buffer via `org-air-project--sort-direction'."
  :type '(choice (const ascending) (const descending))
  :group 'org-air)

(defcustom org-air-project-states
  '("draft" "ready" "work-in-progress" "review" "complete" "dropped")
  "Air doc states in display order.
`review' (D-P5.C) is an optional state between Work-In-Progress and
Complete."
  :type '(repeat string)
  :group 'org-air)

(defcustom org-air-project-sections
  '("draft" "ready" "work-in-progress" "review" "complete" "dropped")
  "State buckets, in order, rendered as project-view SECTIONS (D-P5.C).
Each present bucket becomes a section heading (badge icon + title + count
badge) with its doc rows beneath; empty buckets are omitted, exactly like
the board's empty sections."
  :type '(repeat string)
  :group 'org-air)

(defcustom org-air-project-state-badges
  '(("draft"            . ("\N{MEMO}"               . "[D]"))
    ("ready"            . ("\N{DIRECT HIT}"         . "[R]"))
    ("work-in-progress" . ("\N{GEAR}"               . "[W]"))
    ("review"           . ("\N{LEFT-POINTING MAGNIFYING GLASS}" . "[V]"))
    ("complete"         . ("\N{WHITE HEAVY CHECK MARK}" . "[C]"))
    ("dropped"          . ("\N{WASTEBASKET}"        . "[X]")))
  "Per-state badge as (STATE . (EMOJI . TTY)).
The GUI shows EMOJI; the byte gate (no graphical frame) shows TTY."
  :type '(alist :key-type string :value-type (cons string string))
  :group 'org-air)

;;;; ---------------------------------------------------------------------
;;;; Detection + source resolution
;;;; ---------------------------------------------------------------------

(defun org-air-detect-air-project (root)
  "Return non-nil when ROOT is an Air project root.
That is, when ROOT contains `air-config.toml' or an `air/' subdirectory."
  (and (stringp root)
       (file-directory-p root)
       (or (file-exists-p (expand-file-name "air-config.toml" root))
           (file-directory-p (expand-file-name "air" root)))))

(defun org-air-project-roots ()
  "Return the list of Air project roots from config.
`org-air-projects' wins; otherwise derive from `org-air-sources' (explicit
`:air' entries and auto-detected source directories)."
  (or org-air-projects
      (delete-dups
       (delq nil
             (mapcar (lambda (entry)
                       (cond
                        ((and (consp entry) (eq (car entry) :air))
                         (cadr entry))
                        ((and (stringp entry)
                              (org-air-detect-air-project entry))
                         entry)))
                     org-air-sources)))))

;;;; ---------------------------------------------------------------------
;;;; Data model
;;;; ---------------------------------------------------------------------

(cl-defstruct (org-air-doc (:constructor org-air-doc-create))
  "A single Air document read from its `.org' file."
  name file state tags updated created relpath)

(defun org-air-project--air-dir (root)
  "Return the directory under ROOT containing the Air docs.
The `air/' subdirectory when present, else ROOT itself."
  (let ((air (expand-file-name "air" root)))
    (if (file-directory-p air) air root)))

(defconst org-air-project--non-tracked-file-stems '("readme" "overview" "skill")
  "Reserved summary/metadata file stems Air EXCLUDES from document tracking.
Mirrors airctl's `DocumentScanner::is_overview_file' (air-core scanner): a
file whose stem (case-insensitive, sans any supported extension) is one of
these is a directory-level summary (e.g. OVERVIEW.org) or tool metadata
such as README or SKILL, NOT a trackable work item -- so it must never
reach the per-dir state counts.")

(defun org-air-project--overview-file-p (file)
  "Non-nil when FILE is a non-trackable summary/metadata file (R20-5 fix).
Matches airctl exactly: the file stem (sans extension), case-folded, is
README, OVERVIEW or SKILL.  Air filters these directory-summary docs out
of document scanning, so org-air must not count their (stateless) bodies
as `draft' the way `org-air-project--collect-docs' silently did before."
  (and file
       (member (downcase (file-name-base file))
               org-air-project--non-tracked-file-stems)))

(defun org-air-project--read-keyword (key)
  "Return the first in-buffer #+KEY value as a string, or nil.
Point-independent; scans from the top of the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           (format "^[ \t]*#\\+%s:[ \t]*\\(.*\\)$" (regexp-quote key))
           nil t)
      (let ((v (string-trim (match-string 1))))
        (unless (string-empty-p v) v)))))

(defun org-air-project--parse-tags (filetags)
  "Return a list of tag strings from a FILETAGS value like \":a:b:\"."
  (when filetags
    (seq-remove #'string-empty-p (split-string filetags ":" t "[ \t]+"))))

(defun org-air-project--read-doc (file root)
  "Read FILE (under air dir of ROOT) into an `org-air-doc'."
  (let* ((air (org-air-project--air-dir root))
         (relpath (file-relative-name file air))
         state tags title)
    (with-temp-buffer
      (insert-file-contents file)
      ;; R20-5 fix: a doc WITHOUT a #+state: keyword is `unknown', exactly as
      ;; airctl does (`extracted.state.unwrap_or(DocumentState::Unknown)') --
      ;; never silently `draft'.  `unknown' ranks last and renders faded.
      (setq state (downcase (or (org-air-project--read-keyword "state") "unknown"))
            tags (org-air-project--parse-tags
                  (org-air-project--read-keyword "FILETAGS"))
            title (org-air-project--read-keyword "title")))
    (org-air-doc-create
     :name (or title (file-name-base file))
     :file file
     :state state
     :tags tags
     :updated (file-attribute-modification-time (file-attributes file))
     :created (org-air-project--doc-created file)
     :relpath relpath)))

(defun org-air-project--doc-created (file)
  "Return FILE's creation time (R14 D-P1.A): #+created:/#+date: else ctime.
Reads an in-buffer `#+created:' or `#+date:' keyword as an Org timestamp
when present, else the file's status-change (ctime) attribute."
  (or (ignore-errors
        (with-temp-buffer
          (insert-file-contents file nil 0 4096)
          (when-let* ((v (or (org-air-project--read-keyword "created")
                             (org-air-project--read-keyword "date")))
                      (ts (org-timestamp-from-string
                           (if (string-prefix-p "[" (string-trim v))
                               (string-trim v)
                             (format "[%s]" (string-trim v))))))
            (org-timestamp-to-time ts))))
      (file-attribute-status-change-time (file-attributes file))))

(defun org-air-project--collect-docs (root)
  "Return the list of `org-air-doc' under ROOT's Air directory.
Non-trackable summary/metadata files (OVERVIEW/README/SKILL) are EXCLUDED
exactly as `airctl status' excludes them (R20-5 fix,
`org-air-project--overview-file-p'), so the total doc count and every
per-dir state badge match `airctl status -Da' instead of inflating Draft
with the stateless directory-summary bodies."
  (let ((air (org-air-project--air-dir root)))
    (when (file-directory-p air)
      (mapcar (lambda (f) (org-air-project--read-doc f root))
              (seq-remove #'org-air-project--overview-file-p
                          (sort (directory-files-recursively air "\\.org\\'")
                                #'string-lessp))))))

;;;; ---------------------------------------------------------------------
;;;; Badges / glyphs
;;;; ---------------------------------------------------------------------

(defun org-air-project--state-face (state)
  "Return the face for STATE's badge."
  (pcase state
    ("draft" 'org-air-face-air-state-draft)
    ("ready" 'org-air-face-air-state-ready)
    ("work-in-progress" 'org-air-face-air-state-wip)
    ("review" 'org-air-face-air-state-review)
    ("complete" 'org-air-face-air-state-complete)
    ("dropped" 'org-air-face-air-state-dropped)
    (_ 'org-air-face-faded)))

(defun org-air-project--state-title (state)
  "Return a human title for STATE (e.g. \"Work In Progress\")."
  (mapconcat #'capitalize (split-string state "-") " "))

;;;; ---------------------------------------------------------------------
;;;; Row cells (D-P5.B — Air docs mapped onto the shared row primitive)
;;;; ---------------------------------------------------------------------

(defun org-air-project--doc-date-text (doc)
  "Return the faded updated-stamp date cell for DOC (D-P5.B): \"↻ YYYY-MM-DD\"."
  (propertize (concat (org-air-layout-glyph 'updated) " "
                      (format-time-string "%F" (org-air-doc-updated doc)))
              'face 'org-air-face-faded))

(defun org-air-project--doc-tagstr (doc)
  "Return DOC's #tags as the shared accent-faced svg pill string (D-P5.B).
Reuses `org-air-view--item-tagstr' so the project view's tag pills are the
SAME D-P1 pills as the board's (accent face + pad cols + svg overlay)."
  (let* ((tags (org-air-doc-tags doc))
         (n (length tags)))
    (org-air-view--item-tagstr tags n n)))

(defun org-air-project--doc-origin-text (doc)
  "Return DOC's origin cell (D-P5.B): \"⌂ relpath\" (the dir grouping lives here)."
  (concat (org-air-layout-glyph 'origin) " " (org-air-doc-relpath doc)))

(defun org-air-project--doc-widths (docs)
  "Return the fixed (DCOL TCOL OCOL) metadata column widths over DOCS.
Mirrors `org-air-view--compute-meta-widths' so the date / tags / path
columns line up exactly down the project list (board parity)."
  (let ((dw 0) (tw 0) (ow 0))
    (dolist (doc docs)
      (setq dw (max dw (string-width (org-air-project--doc-date-text doc)))
            tw (max tw (string-width (org-air-project--doc-tagstr doc)))
            ow (max ow (string-width (org-air-project--doc-origin-text doc)))))
    (list dw tw ow)))

;; R21-5: the per-render fixed metadata column widths over the DISPLAYED
;; docs, bound in `org-air-project--render' so the project's one-line rows
;; line up exactly like the board's `org-air-view--meta-*-w' (V6 + R20-6
;; "measure only what is shown").
(defvar org-air-project--meta-date-w 0
  "Per-render fixed date-cell width for the project rows (R21-5).")
(defvar org-air-project--meta-tags-w 0
  "Per-render fixed tags-cell width for the project rows (R21-5).")
(defvar org-air-project--meta-origin-w 0
  "Per-render fixed origin-cell width for the project rows (R21-5).")

(defconst org-air-project--state-cell-w 3
  "Reserved width of the project state token cell ([R]/[C]/...; R21-5).")

(defun org-air-project--fit-meta-widths (docs width)
  "Return the fitted (DCOL TCOL OCOL) project column widths at WIDTH (R21-5).
Mirrors `org-air-view--compute-meta-widths' for the project rows: measures
the displayed DOCS, caps the origin at `org-air-origin-max-width', then
reclaims columns for the flex title (origin toward `org-air-origin-min',
then tags toward a 1-col floor) so the title keeps at least
`org-air-title-min-width'.  The left reserve mirrors the row prefix
\(margin + the fixed state cell + its separator) so a doc row and a board
task row share column positions (board parity, invariant #4)."
  (let* ((raw (org-air-project--doc-widths docs))
         (dw (nth 0 raw))
         (tw (nth 1 raw))
         (ow (min (nth 2 raw) org-air-origin-max-width))
         (gap 2)
         (left-reserve (+ (string-width (org-air-view--item-margin))
                          (1+ org-air-project--state-cell-w)))
         (cluster
          (lambda (o)
            (let ((cells (delq nil (list (and (> dw 0) dw)
                                         (and (> tw 0) tw)
                                         (and (> o  0) o)))))
              (+ (apply #'+ cells) (max 0 (1- (length cells)))))))
         (budget (lambda (o) (- width left-reserve gap (funcall cluster o)))))
    ;; 1) shrink the origin toward its floor until the title reaches min.
    (while (and (> ow org-air-origin-min)
                (< (funcall budget ow) org-air-title-min-width))
      (setq ow (1- ow)))
    ;; 2) still starved? shrink tags toward a 1-col floor.
    (let ((tw-floor (if (> tw 0) 1 0)))
      (while (and (> tw tw-floor)
                  (< (funcall budget ow) org-air-title-min-width))
        (setq tw (1- tw))))
    ;; 3) the date column is held (small, uniform); the title floor in
    ;;    `org-air-view--insert-row' (max 1) takes over on a board-only
    ;;    narrow tier -- never crash, never overflow.
    (list dw tw ow)))

(defun org-air-project--state-token (state)
  "Return the terse TTY/byte state token for STATE (e.g. \"[R]\"; R21-5).
This is the bracket token the board/project byte contract shows; R21-4
overlays the svg keyword/state badge on it for GUI; it never returns an
emoji (R21.1 retired the GUI state-emoji path entirely)."
  (let ((pair (cdr (assoc state org-air-project-state-badges))))
    (if pair (cdr pair)
      (format "[%s]" (upcase (substring state 0 1))))))

(defun org-air-project--state-badge-cell (state)
  "Return STATE's token faced + the shared svg keyword/state badge (R21-4).
The terse token text (`[R]'...) is the byte/TTY contract; on GUI the
shared `org-air-view--svg-keyword-badge' overlays a small coloured chip
\(state colour from `org-air-project--state-face'), the SAME badge idiom
as the board keyword cell -- retiring the project emoji on GUI."
  (org-air-view--svg-keyword-badge
   (propertize (org-air-project--state-token state)
               'face (org-air-project--state-face state))
   (org-air-project--state-face state)))

(defun org-air-project--state-cell (state)
  "Return a FIXED-width reserved STATE cell for the project row (R21-5).
Mirrors the board's `org-air-view--todo-cell': the state TOKEN in its
state face, left-justified and padded to `org-air-project--state-cell-w'
plus a single trailing separator, so every doc title shares one left
edge.  The token text is the byte/TTY contract; R21-4 overlays the svg
keyword/state badge on GUI."
  (concat (org-air-view--pad-to
           (org-air-project--state-badge-cell state)
           org-air-project--state-cell-w)
          " "))

;;;; ---------------------------------------------------------------------
;;;; Sections (D-P5.C — state buckets become sections; parity with board)
;;;; ---------------------------------------------------------------------

(defvar org-air-project--width 80
  "Effective render width for the current project-view render.")

(defun org-air-project--render-width ()
  "Return the width to render the project view at."
  (or org-air-project-view-width
      (and (get-buffer-window (current-buffer) t)
           (org-air-layout-current-width (current-buffer)))
      80))

(defconst org-air-project--attention-states '("ready" "work-in-progress")
  "States whose non-empty section count uses the attention badge (D-P5.C).")

(defun org-air-project--sections (docs)
  "Return the ordered render sections for DOCS under the current group mode.
Each section is a plist: :icon :icon-face :title :title-face :docs
:attention :show-state.  R16 D-P4/D-P5: every section's members are ordered
through the single comparator (`org-air-project--sort-section-docs') so
state-then-sort-key composition is uniform across all group modes."
  (let ((sections (pcase org-air-project-group
                    ('tag (org-air-project--sections-by-tag docs))
                    (_ (org-air-project--sections-by-state docs)))))
    (dolist (section sections sections)
      (plist-put section :docs
                 (org-air-project--sort-section-docs
                  (plist-get section :docs))))))

(defun org-air-project--sections-by-state (docs)
  "Return state-bucket sections for DOCS in `org-air-project-sections' order.
Buckets with zero docs are omitted; any state not listed is appended."
  (let ((order (append org-air-project-sections
                       (seq-remove (lambda (s) (member s org-air-project-sections))
                                   (seq-uniq (mapcar #'org-air-doc-state docs))))))
    (delq nil
          (mapcar
           (lambda (state)
             (let ((members (seq-filter
                             (lambda (d) (equal (org-air-doc-state d) state))
                             docs)))
               (when members
                 ;; R21.1: route the section icon through the SAME shared
                 ;; svg keyword/state badge as the doc rows (svg chip on
                 ;; GUI, terse `[R]' token on TTY) -- no GUI emoji.
                 (list :icon (org-air-project--state-badge-cell state)
                       :icon-face (org-air-project--state-face state)
                       :title (org-air-project--state-title state)
                       :title-face 'org-air-face-section
                       :docs members
                       :attention (member state org-air-project--attention-states)
                       :show-state nil))))
           order))))

;;;; ---------------------------------------------------------------------
;;;; Nested directory tree (R20-5 — match airctl status -Da)
;;;; ---------------------------------------------------------------------

(defun org-air-project--doc-dir-segments (doc)
  "Return DOC's directory path as a list of segments (R20-5).
\"v0.1/air-context/x.org\" -> (\"v0.1\" \"air-context\"); a root doc -> nil."
  (let ((dir (file-name-directory (or (org-air-doc-relpath doc) ""))))
    (and dir (split-string (directory-file-name dir) "/" t))))

(defun org-air-project--state-display-rank (state)
  "Return STATE's rank in `org-air-project--state-display-order' (unknown last)."
  (or (seq-position org-air-project--state-display-order state #'equal)
      (length org-air-project--state-display-order)))

(defun org-air-project--state-first-lessp (a b)
  "Non-nil when doc A precedes B state-first (display order) then by name (R20-5)."
  (let ((ra (org-air-project--state-display-rank (org-air-doc-state a)))
        (rb (org-air-project--state-display-rank (org-air-doc-state b))))
    (if (/= ra rb) (< ra rb)
      (org-air-project--doc-tiebreak-lessp a b))))

(defun org-air-project--sort-own-docs (docs)
  "Return DOCS state-first (display order) then by name (R20-5)."
  (sort (copy-sequence docs) #'org-air-project--state-first-lessp))

(defun org-air-project--count-by-state (docs)
  "Return an alist STATE -> count over DOCS."
  (let (table)
    (dolist (d docs table)
      (let ((cell (assoc (org-air-doc-state d) table)))
        (if cell (setcdr cell (1+ (cdr cell)))
          (push (cons (org-air-doc-state d) 1) table))))))

(defun org-air-project--counts-add (a b)
  "Return the per-state sum of count alists A and B."
  (let ((out (copy-alist a)))
    (dolist (cell b out)
      (let ((o (assoc (car cell) out)))
        (if o (setcdr o (+ (cdr o) (cdr cell)))
          (push (cons (car cell) (cdr cell)) out))))))

(defun org-air-project--make-dir-node (path depth subtree-docs)
  "Build the tree node for the directory PATH (a list of segments) at DEPTH.
SUBTREE-DOCS are all docs whose dir-segments have PATH as a prefix (or
equal it).  A node is a plist (see `org-air-project--directory-tree').
Counts are computed bottom-up: :direct-counts over the dir's OWN docs,
:desc-counts summed over descendant dirs, :total-counts = direct+desc."
  (let* ((plen (length path))
         (own (seq-filter
               (lambda (d)
                 (= (length (org-air-project--doc-dir-segments d)) plen))
               subtree-docs))
         (deeper (seq-filter
                  (lambda (d)
                    (> (length (org-air-project--doc-dir-segments d)) plen))
                  subtree-docs))
         (groups nil))                  ; alist next-seg -> docs (reversed)
    (dolist (d deeper)
      (let* ((seg (nth plen (org-air-project--doc-dir-segments d)))
             (cell (assoc seg groups)))
        (if cell (setcdr cell (cons d (cdr cell)))
          (push (cons seg (list d)) groups))))
    (setq groups (sort groups (lambda (a b) (string-lessp (car a) (car b)))))
    (let* ((children (mapcar
                      (lambda (g)
                        (org-air-project--make-dir-node
                         (append path (list (car g)))
                         (1+ depth)
                         (nreverse (cdr g))))
                      groups))
           (direct (org-air-project--count-by-state own))
           (desc (seq-reduce
                  (lambda (acc c)
                    (org-air-project--counts-add
                     acc (org-air-project--counts-add
                          (plist-get c :direct-counts)
                          (plist-get c :desc-counts))))
                  children nil)))
      (list :dir (car (last path))
            :depth depth
            :path (string-join path "/")
            :own-docs (org-air-project--sort-own-docs own)
            :children children
            :direct-counts direct
            :desc-counts desc
            :total-counts (org-air-project--counts-add direct desc)))))

(defun org-air-project--directory-tree (docs)
  "Return the ordered list of TOP-dir nodes for DOCS (R20-5).
Groups DOCS by their first path segment (top dirs, name-sorted); root
docs with no directory fold into a leading node with an empty :path."
  (let (groups root-docs)
    (dolist (d docs)
      (let ((segs (org-air-project--doc-dir-segments d)))
        (if (null segs)
            (push d root-docs)
          (let* ((seg (car segs)) (cell (assoc seg groups)))
            (if cell (setcdr cell (cons d (cdr cell)))
              (push (cons seg (list d)) groups))))))
    (setq groups (sort groups (lambda (a b) (string-lessp (car a) (car b)))))
    (let ((nodes (mapcar
                  (lambda (g)
                    (org-air-project--make-dir-node
                     (list (car g)) 0 (nreverse (cdr g))))
                  groups)))
      (if root-docs
          (cons (org-air-project--make-dir-node nil 0 (nreverse root-docs))
                nodes)
        nodes))))

(defun org-air-project--marker ()
  "Return the propertized rail-marker glyph (svg accent bar on GUI)."
  (let* ((img (org-air-layout-marker-image))
         (mk (org-air-layout-glyph 'rail-marker))
         (marker (propertize mk 'face 'org-air-face-rail-marker)))
    (if img (propertize marker 'display img) marker)))

(defun org-air-project--state-letter (state)
  "Return STATE's single-letter token (R/W/V/C/X/D) for the count summary (R22-6).
Derived from `org-air-project--state-token' (the `[R]'... TTY token) so it
tracks `org-air-project-state-badges'; falls back to the upcased initial."
  (let ((tok (org-air-project--state-token state)))
    (if (string-match "\\[\\(.\\)\\]" tok)
        (match-string 1 tok)
      (upcase (substring state 0 1)))))

(defun org-air-project--dir-count-summary (direct desc)
  "Return the calm `R4(+1) C14(+14) ...' count summary for a dir header (R22-6).
DIRECT is the dir's OWN per-state counts, DESC its descendants' rollup.
State as a quiet faded LETTER (not the coloured badge), own count, faded
`(+M)' nested rollup; states absent from BOTH are omitted; display order =
`org-air-project--state-display-order'.  Numerically identical to the old
`--count-badges' / `airctl -Da' (own N + nested +M)."
  (let (cells)
    (dolist (state org-air-project--state-display-order)
      (let ((n (or (cdr (assoc state direct)) 0))
            (m (or (cdr (assoc state desc)) 0)))
        (when (or (> n 0) (> m 0))
          (push (concat
                 (propertize (org-air-project--state-letter state)
                             'face (org-air-project--state-face state))
                 (when (> n 0) (propertize (number-to-string n)
                                           'face 'org-air-face-count))
                 (when (> m 0) (propertize (format "(+%d)" m)
                                           'face 'org-air-face-faded)))
                cells))))
    (mapconcat #'identity (nreverse cells) " ")))

(defun org-air-project--insert-dir-node (node width _topp)
  "Insert NODE (a dir tree node) and its subtree into the buffer at WIDTH.
ONE header per directory (R22-6): the depth-indented marker + `dir/' name on
the LEFT, a quiet right-aligned letter-count summary (`R4(+1) C14(+14) ...')
on the RIGHT; then the dir's OWN docs (state-first, indented one level
DEEPER than the header), then recursion into the name-sorted children.  The
old doubled header (a rolled-up box header + a per-dir count heading) is
gone — the single header's own/+nested counts already encode the subtree
totals.  _TOPP is accepted for the caller's uniform call but no longer
special-cases a separate header."
  (let* ((depth (plist-get node :depth))
         (indent (make-string (* 2 depth) ?\s))
         (dir (plist-get node :dir))
         (path (plist-get node :path))
         (name (if (and dir (not (string-empty-p dir))) (concat dir "/") "·"))
         (start (point))
         ;; Indent the WHOLE header (marker included) by depth so the tree
         ;; reads; the accent marker is the quiet section bullet, printed
         ;; ONCE per header, never repeated mid-line.
         (left (concat "  " indent (org-air-project--marker) " "
                       (propertize name 'face 'org-air-face-section)))
         ;; Quiet, right-aligned count summary (the badge wall is gone).
         (summary (org-air-project--dir-count-summary
                   (plist-get node :direct-counts)
                   (plist-get node :desc-counts)))
         (header (if (string-empty-p summary)
                     left
                   (org-air-view--justify left summary width))))
    (insert header "\n")
    (add-text-properties start (point) (list 'org-air-section path))
    ;; Own docs, state-first, indented one level UNDER this dir's header so a
    ;; doc clearly hangs beneath its directory (R21-5 one board-style row).
    (dolist (doc (plist-get node :own-docs))
      (org-air-project--insert-doc-row doc width (* 2 (1+ depth))))
    ;; Recurse into the children (name-sorted).
    (dolist (child (plist-get node :children))
      (org-air-project--insert-dir-node child width nil))))

(defun org-air-project--insert-directory-tree (nodes width)
  "Insert the nested directory TREE (NODES) at content WIDTH (R20-5)."
  (dolist (node nodes)
    (org-air-project--insert-dir-node node width t)
    (insert "\n")))

(defun org-air-project--sections-by-tag (docs)
  "Return tag sections for DOCS (a doc may appear under several tags)."
  (let ((tags (seq-uniq (apply #'append (mapcar #'org-air-doc-tags docs)))))
    (mapcar
     (lambda (tag)
       (list :icon nil
             :icon-face nil
             :title (concat "#" tag)
             :title-face (org-air-faces-tag-face tag)
             :docs (seq-filter (lambda (d) (member tag (org-air-doc-tags d))) docs)
             :attention nil
             :show-state t))
     (sort tags #'string-lessp))))

(defun org-air-project--insert-section-heading (section)
  "Insert SECTION's heading (R14 D-P1.A): ▌ marker + icon + title + count.
Adopts the round-11 prefix-svg `▌' header marker for parity with the rail
headers; the marker carries the GUI svg accent bar via
`org-air-layout-marker-image'."
  (let* ((start (point))
         (icon (plist-get section :icon))
         (count (length (plist-get section :docs)))
         (img (org-air-layout-marker-image))
         (mk (org-air-layout-glyph 'rail-marker))
         (marker (propertize mk 'face 'org-air-face-rail-marker))
         (marker (if img (propertize marker 'display img) marker)))
    (insert "  " marker " "
            (if icon
                (concat (propertize icon 'face (plist-get section :icon-face)) " ")
              "")
            (propertize (plist-get section :title)
                        'face (plist-get section :title-face))
            " "
            (propertize (format "%d" count)
                        'face (if (plist-get section :attention)
                                  'org-air-face-count-attention
                                'org-air-face-count))
            "\n")
    (add-text-properties start (point)
                         (list 'org-air-section (plist-get section :title)
                               'org-air-count-badge count))))

(defun org-air-project--deslug-relpath (relpath)
  "Return RELPATH with its Denote LEAF de-slugged (R17 D-P2).
Keeps the directory prefix and replaces only the leaf with its de-slugged
Denote title (`org-air-view--denote-title'), falling back to the raw leaf
for a non-Denote name.  So a long Denote filename reads as its title --
e.g. \"v0.1/weekly-invalidation-rate-upgr\" -- instead of the raw
identifier--slug__tags.org; `org-air-view--pad-to' still bounds line 2."
  (let* ((dir (file-name-directory relpath))
         (leaf (file-name-nondirectory relpath))
         (title (or (org-air-view--denote-title leaf) leaf)))
    (concat (or dir "") title)))

(defun org-air-project--insert-doc-row (doc _width &optional indent-cols)
  "Insert DOC as ONE board-style row via the shared primitive (R21-5).
Maps DOC onto `org-air-view--insert-row' exactly as the board maps a task
\(invariant #4: parameterise the shared primitive, do not fork): the doc
STATE is the row PREFIX as a fixed-width cell, the updated stamp the DATE
cell, the #tags the SAME svg pills as the board, and the relpath the
right-justified ORIGIN cell.  INDENT-COLS nests the row under its
directory in the tree.  The whole row carries `org-air-doc' +
`org-air-marker' so point on ANY cell identifies the doc (RET/visit still
resolve).  Replaced the old two-line doc block (R21-5) so the project rows
match the board's clean one-line table."
  (let* ((state  (org-air-doc-state doc))
         (prefix (concat (org-air-view--item-margin)
                         (make-string (max 0 (or indent-cols 0)) ?\s)
                         (org-air-project--state-cell state)))
         (date   (org-air-project--doc-date-text doc))
         (tags   (org-air-project--doc-tagstr doc))
         (origin (org-air-project--doc-origin-text doc)))
    (org-air-view--insert-row
     :prefix prefix
     :title (org-air-doc-name doc)
     :date-text date
     :tags tags
     :origin-text origin
     :origin-face 'org-air-face-group
     :widths (list org-air-project--meta-date-w
                   org-air-project--meta-tags-w
                   org-air-project--meta-origin-w)
     :props (list 'org-air-doc doc
                  'org-air-marker (org-air-doc-file doc)
                  'mouse-face 'org-air-face-cursor)
     :face 'org-air-face-title)))

;;;; ---------------------------------------------------------------------
;;;; View
;;;; ---------------------------------------------------------------------

(defvar-local org-air-project--root nil
  "Air root rendered in this project-view buffer.")

(defvar-local org-air-project--doc-count nil
  "Cached doc count for the calm status mode-line (R20-2); set per render.")

(defvar-local org-air-project--rendered-width nil
  "Width of the most recent project-view render (R14 D-P1.B resize guard).")

(defvar-local org-air-project--sort-key nil
  "Active per-buffer sort key (R16 D-P4); seeded from `org-air-project-sort-key'.")
(defvar-local org-air-project--sort-direction nil
  "Active per-buffer sort direction (R16 D-P4); seeded from the defcustom.")

(defun org-air-project--state-rank (state)
  "Return the canonical rank of STATE (R16 D-P5).
Uses `org-air-project-sections' (Draft/Ready/WIP/Review/Complete/...) as
the single source of truth for state order: a known state gets its index;
any unknown state shares one rank just past the known ones (so it sorts
AFTER them).  Unknown states are then ordered among themselves by their
state string in `org-air-project--doc-compare', not by this integer."
  (or (seq-position org-air-project-sections state #'equal)
      (length org-air-project-sections)))

(defun org-air-project--sort-key-active ()
  "Return the active sort key (R22-3: the SHARED sort state wins).
The project now drives `o'/`O' through `org-air-view--sort-key' (the shared
core); the project-local var remains a fallback (let-bindable in tests),
then the defcustom."
  (or org-air-view--sort-key org-air-project--sort-key org-air-project-sort-key))

(defun org-air-project--sort-direction-active ()
  "Return the active sort direction (R22-3: the SHARED sort state wins)."
  (or org-air-view--sort-direction org-air-project--sort-direction
      org-air-project-sort-direction))

(defun org-air-project--doc-key-value (doc key)
  "Return DOC's value for sort KEY (`name'/`created'/`updated') (R16 D-P4)."
  (pcase key
    ('created (org-air-doc-created doc))
    ('updated (org-air-doc-updated doc))
    (_ (org-air-doc-name doc))))

(defun org-air-project--doc-tiebreak-lessp (a b)
  "Return non-nil when doc A precedes doc B by the byte-stable tiebreak.
Name ascending, then relpath ascending."
  (let ((na (or (org-air-doc-name a) ""))
        (nb (or (org-air-doc-name b) "")))
    (if (not (string-equal na nb))
        (string-lessp na nb)
      (string-lessp (or (org-air-doc-relpath a) "")
                    (or (org-air-doc-relpath b) "")))))

(defun org-air-project--doc-compare (a b)
  "Strict total order over docs A and B (R16 D-P4/D-P5).
1. state-rank ascending (D-P5 — within-group state primary; constant within
   a state group so the key drives order there);
2. the active sort key in the active direction (a nil date sorts LAST in
   BOTH directions — the partition rule);
3. tiebreak: name then relpath ascending (byte-stable equal keys)."
  (let* ((sa (org-air-doc-state a))
         (sb (org-air-doc-state b))
         (ra (org-air-project--state-rank sa))
         (rb (org-air-project--state-rank sb))
         (unknown (length org-air-project-sections)))
    (cond
     ((/= ra rb) (< ra rb))
     ;; Both UNKNOWN (same tail rank) but different states -> order by the
     ;; state string (matches the docstring; byte-stable).
     ((and (= ra unknown) (not (equal sa sb)))
      (string-lessp (or sa "") (or sb "")))
     (t (org-air-project--doc-compare-key a b)))))

(defun org-air-project--doc-compare-key (a b)
  "Strict order over docs A and B by the active sort key only (R16 D-P4).
The D-P5 state-rank primary is applied by `org-air-project--doc-compare'."
  (let* ((key (org-air-project--sort-key-active))
         (desc (eq (org-air-project--sort-direction-active) 'descending))
         (va (org-air-project--doc-key-value a key))
         (vb (org-air-project--doc-key-value b key)))
    (cond
     ;; Nil-key partition: missing dates always sort LAST, never
     ;; flipping to the top under descending.
     ((and (null va) (null vb))
      (org-air-project--doc-tiebreak-lessp a b))
     ((null va) nil)
     ((null vb) t)
     (t
      (let ((lt (if (memq key '(created updated))
                    (time-less-p va vb)
                  (string-lessp (or va "") (or vb ""))))
            (gt (if (memq key '(created updated))
                    (time-less-p vb va)
                  (string-lessp (or vb "") (or va "")))))
        (cond
         ((and (not lt) (not gt))
          ;; equal key → byte-stable tiebreak (never reversed).
          (org-air-project--doc-tiebreak-lessp a b))
         (desc gt)
         (t lt)))))))

(defun org-air-project--sort-section-docs (docs)
  "Return DOCS ordered by the single comparator (R16 D-P4/D-P5).
Every group mode funnels its members through here so the comparator is
the single source of truth for display order."
  (sort (copy-sequence docs) #'org-air-project--doc-compare))

(defun org-air-project--sort-indicator ()
  "Return the active-sort badge text `↕ <key> <dir>' (R16 D-P4).
R22-3: delegates to the shared `org-air-view--sort-indicator-text' builder
so the board and the project show one indicator; byte-identical to the old
local builder (same glyphs + faces)."
  (org-air-view--sort-indicator-text
   (org-air-project--sort-key-active)
   (org-air-project--sort-direction-active)))

(defun org-air-project--filter-segment ()
  "Return the active filter + combinator as a header segment, or empty string.
R18 D-P3: mirrors the board banner (`#a AND #b' / `#a OR #b', single tag
shows no combinator) so the two views read identically.  Empty when no
filter is active, keeping the existing project goldens byte-identical."
  (let* ((filters (org-air-view--filter-tags))
         (sep (if (> (length filters) 1)
                  (concat " " (org-air-view--filter-combinator-word) " ")
                " ")))
    (if filters
        (propertize (concat " · "
                            (mapconcat (lambda (tag) (concat "#" tag)) filters sep)
                            " " (org-air-view--glyph 'clear))
                    'face 'org-air-face-faded)
      "")))

(defun org-air-project--header-line (width)
  "Return the project header line for WIDTH: title left, sort badge right.
The badge order is part of the byte contract (R16 D-P4).  R18 D-P3: an
active tag filter + combinator is surfaced beside the title (empty when
none, so the no-filter goldens are byte-identical)."
  (let* ((title (concat (propertize "  org-air · project" 'face 'org-air-face-title)
                        (org-air-project--filter-segment)))
         (badge (org-air-project--sort-indicator))
         (lw (string-width title))
         (bw (string-width badge))
         ;; right-cluster the badge, leaving a trailing column like the rest.
         (pad (max 1 (- width lw bw 2))))
    (concat title (make-string pad ?\s) badge)))

(defun org-air-project--insert-doc-sections (sections width)
  "Insert all SECTIONS (headings + two-line doc blocks) at content WIDTH."
  (dolist (section sections)
    (org-air-project--insert-section-heading section)
    ;; R21-5: one board-style row per doc.  The row ALWAYS carries the
    ;; state cell now, so the per-section SHOW-STATE conditional is gone
    ;; (a doc under a state section reads identically to one under a dir).
    (dolist (doc (plist-get section :docs))
      (org-air-project--insert-doc-row doc width))
    (insert "\n")))

(defun org-air-project--insert-state-summary-line (docs)
  "Insert the compact one-line state-count summary for DOCS (board-only)."
  (insert "  "
          (mapconcat
           (lambda (state)
             (let ((n (seq-count (lambda (d) (equal (org-air-doc-state d) state))
                                 docs)))
               ;; R21.1: shared svg badge (no GUI emoji); `[R]' token on TTY.
               (concat (org-air-project--state-badge-cell state)
                       " " (number-to-string n))))
           org-air-project-states "   ")
          "\n\n"))

(defun org-air-project--insert-summary (docs width)
  "Insert the project rail Summary block for DOCS at rail WIDTH (R14 D-P1.B).
A `▌ Summary' header + a per-state count row (the board's top-line state
summary moved into the rail)."
  (org-air-view--rail-header "Summary" width)
  (let ((inset (org-air-view--rail-inset-str width)))
    (dolist (state org-air-project-states)
      (let ((n (seq-count (lambda (d) (equal (org-air-doc-state d) state)) docs)))
        (insert inset
                (propertize (format "%3d" n)
                            'face (if (= n 0) 'org-air-face-faded
                                    'org-air-face-summary-number))
                "  "
                (propertize (org-air-project--state-title state)
                            'face 'org-air-face-summary-label)
                "\n")))))

(defun org-air-project--calendar-marks (docs)
  "Return a date-key -> `created mark table over DOCS' updated stamps (R20-5).
The shared rail's Calendar marks each doc on the day it was last updated
with a quiet `created'-style dot, so the project Calendar reads like the
board's without needing Org deadline/scheduled timestamps."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (doc docs table)
      (when-let* ((u (org-air-doc-updated doc)))
        (let ((d (decode-time u)))
          (puthash (org-air-calendar--date-key (decoded-time-month d)
                                               (decoded-time-day d)
                                               (decoded-time-year d))
                   'created table))))))

(defun org-air-project--insert-actions (width)
  "Insert the project rail Actions block fitted to rail content WIDTH (R20-5).
Same SHAPE + keycap idiom as the board's Actions, with the project's verbs
— open / filter / refresh, then visit / clear / quit — on the SAME keys a
board user already knows."
  (org-air-view--rail-header "Actions" width)
  (let* ((inset (org-air-view--rail-inset-str width))
         (c1 (max (+ 4 (length "open")) (+ 6 (length "visit"))))
         (c2 (max (+ 2 (length "filter")) (+ 2 (length "clear"))))
         (gap (if (>= width 38) "    " " ")))
    (insert (org-air-view--pad-to
             (concat inset
                     (org-air-view--verb-cell "RET" "open" c1) gap
                     (org-air-view--verb-cell "/" "filter" c2) gap
                     (org-air-view--verb-cell "g" "refresh" 0))
             width)
            "\n")
    (insert (org-air-view--pad-to
             (concat inset
                     (org-air-view--verb-cell "S-RET" "visit" c1) gap
                     (org-air-view--verb-cell "\\" "clear" c2) gap
                     (org-air-view--verb-cell "q" "quit" 0))
             width)
            "\n")))

(defun org-air-project--two-pane-body (docs left-fn width)
  "Return (BODY-LINES . FILL-ROW) composing the LEFT pane | project-rail.
LEFT-FN is a one-arg closure that inserts the left pane content at a given
width (state/tag sections OR the R20-5 directory tree); the RIGHT rail is
the project rail (Summary + Inspector) for DOCS.  The rail is sized to the
doc-pane height so the divider runs the full body and the layout is
deterministic."
  (let* ((rail-width (org-air-view--rail-width width))
         (divider (org-air-view--divider))
         (item-width (max 20 (- width rail-width (string-width divider))))
         (doc-lines (org-air-view--render-lines
                     item-width
                     (lambda () (funcall left-fn item-width))))
         (doc-h (max 1 (length doc-lines)))
         ;; R20-5(b): render the SHARED board rail, sized (via the
         ;; descriptor's :rail-target-height) to MAX(doc pane, window body)
         ;; so the inspector fills the rail in a real (tall) window while
         ;; the divider still spans a long doc list (doc-h > the window).
         ;; `org-air-show-inspector' follows the project's own toggle.
         (target-h (max doc-h (max 1 (- (org-air-view--render-height) 3))))
         (rail-lines
          (let ((org-air-view--rail-descriptor
                 (plist-put (copy-sequence org-air-view--rail-descriptor)
                            :rail-target-height target-h))
                (org-air-show-inspector org-air-project-show-inspector))
            (mapcar
             (lambda (l) (org-air-view--pad-to l rail-width))
             (org-air-view--render-lines
              rail-width
              (lambda () (org-air-view--insert-rail docs rail-width)))))))
    (setq org-air-view--inspector-geom
          (list :item-width item-width :divider divider :rail-width rail-width
                :region-height org-air-view--inspector-region-height))
    (cons (org-air-view--compose-columns
           (list (cons doc-lines item-width) (cons rail-lines rail-width))
           divider)
          (concat (make-string item-width ?\s) divider
                  (make-string rail-width ?\s)))))

(defun org-air-project--inspector-doc-fields (doc inset content-w now)
  "Return the project DOC's inspector body lines (forward order) (R14 D-P1.B).
Title / State / Path (full) / tags / Group / Created / Updated, the same KV
layout + breathing as the board (`org-air-view--inspector-fields-function').
INSET is the spine prefix, CONTENT-W the wrap width, NOW the render clock."
  (let ((state (org-air-doc-state doc))
        lines)
    (dolist (tl (org-air-view--inspector-title-lines
                 (or (org-air-doc-name doc) "") content-w
                 org-air-inspector-max-title-lines))
      (push (concat inset (propertize tl 'face 'org-air-face-title)) lines))
    (push (org-air-view--inspector-kv
           "State"
           ;; R21.1: shared svg badge (no GUI emoji); `[R]' token on TTY.
           (concat (org-air-project--state-badge-cell state)
                   " "
                   (propertize (org-air-project--state-title state)
                               'face (org-air-project--state-face state)))
           inset)
          lines)
    (push (org-air-view--inspector-kv
           "Path"
           (propertize (abbreviate-file-name (org-air-doc-file doc))
                       'face 'org-air-face-faded)
           inset)
          lines)
    (let ((tagstr (mapconcat
                   (lambda (tg) (propertize (concat "#" tg)
                                            'face (org-air-faces-tag-face tg)))
                   (org-air-doc-tags doc) " ")))
      (unless (string-empty-p tagstr)
        (dolist (tl (org-air-view--word-wrap tagstr content-w))
          (push (concat inset tl) lines))))
    (let ((grp (car (split-string (org-air-doc-relpath doc) "/"))))
      (when (and grp (not (string-empty-p grp))
                 (string-match-p "/" (org-air-doc-relpath doc)))
        (push (org-air-view--inspector-kv
               "Group" (propertize grp 'face 'org-air-face-faded) inset)
              lines)))
    (push "" lines)
    (when-let* ((c (org-air-doc-created doc)))
      (push (org-air-view--inspector-kv
             "Created"
             (concat (propertize (format-time-string "%F" c) 'face 'org-air-face-faded)
                     "  "
                     (propertize (format "(%s)" (org-air-view--inspector-relative c now))
                                 'face 'org-air-face-faded))
             inset)
            lines))
    (when-let* ((u (org-air-doc-updated doc)))
      (push (org-air-view--inspector-kv
             "Updated"
             (concat (propertize (format-time-string "%F" u) 'face 'org-air-face-faded)
                     "  "
                     (propertize (format "(%s)" (org-air-view--inspector-relative u now))
                                 'face 'org-air-face-faded))
             inset)
            lines))
    (nreverse lines)))

(defun org-air-project--render (root)
  "Render the Air project view for ROOT into the current buffer (R14 D-P1).
Two-line doc blocks in state-bucket sections; two-pane (docs + a Summary/
Inspector rail) above `org-air-rail-min-width', board-only below it."
  (let* ((inhibit-read-only t)
         (width (org-air-project--render-width))
         (org-air-project--width width)
         ;; drive the shared row primitive's width seam.
         (org-air-view-width width)
         (dims (org-air-view--char-dimensions))
         (org-air-view--pill-char-w (car dims))
         (org-air-view--pill-char-h (cdr dims))
         ;; R18 D-P3: the shared filter core thins the docs by tag exactly
         ;; as it thins board items (doc-aware `--tags-pass-filter-p').
         (docs (seq-filter
                (lambda (d) (org-air-view--tags-pass-filter-p
                             (org-air-doc-tags d)))
                (org-air-project--collect-docs root)))
         ;; R20-5: `directory' renders the NESTED tree (matching airctl
         ;; -Da); state/tag stay the flat state-bucket / tag sections.
         (directoryp (eq org-air-project-group 'directory))
         (tree (when directoryp (org-air-project--directory-tree docs)))
         (sections (unless directoryp (org-air-project--sections docs)))
         ;; R21-5: compute the fixed metadata column widths over the
         ;; DISPLAYED docs at the ACTUAL render width W (board parity:
         ;; cap + title-protecting fit), and bind them for the row pass so
         ;; the one-line rows line up exactly like the board's V6 table.
         (left-fn
          (lambda (w)
            (let* ((mw (org-air-project--fit-meta-widths docs w))
                   (org-air-project--meta-date-w (nth 0 mw))
                   (org-air-project--meta-tags-w (nth 1 mw))
                   (org-air-project--meta-origin-w (nth 2 mw)))
              (if directoryp
                  (org-air-project--insert-directory-tree tree w)
                (org-air-project--insert-doc-sections sections w))))))
    ;; R20-2: cache the doc count for the status mode-line :eval.
    (setq-local org-air-project--doc-count (length docs))
    ;; R22-5: expose the docs as the shared `org-air-view--items' so a
    ;; POPPED-OUT project rail (the shared `org-air-rail--render', which
    ;; reads this back-pointer) renders the project's docs in the side
    ;; window — the same primitive the board uses.
    (setq-local org-air-view--items docs)
    ;; R14 D-P1.B: this buffer hosts the SHARED mid-rail inspector with the
    ;; project's property + fields function.
    (setq-local org-air-view--inspector-active (and org-air-project-show-inspector t)
                org-air-view--inspector-property 'org-air-doc
                org-air-view--inspector-fields-function
                #'org-air-project--inspector-doc-fields)
    ;; R20-5(b): drive the SHARED board rail (Calendar/Filter/Scope/Summary/
    ;; Inspector/Actions) via a view descriptor, so the project rail is the
    ;; board rail — no bespoke parallel rail.
    (setq-local org-air-view--rail-descriptor
                (list :visible-fn (lambda (ds) ds)
                      :calendar-fn
                      (lambda (ds w inset)
                        (org-air-calendar-insert-month
                         org-air-view--cal-month ds w inset
                         (org-air-project--calendar-marks ds)))
                      :summary-fn #'org-air-project--insert-summary
                      ;; The inspector fills from point (`--setup-inspector');
                      ;; seed it on nothing, exactly as the old project rail.
                      :first-thing-fn (lambda (_ds) nil)
                      :actions-fn #'org-air-project--insert-actions))
    ;; R16 D-P4: seed the per-buffer sort state from the defcustoms once.
    (unless org-air-project--sort-key
      (setq-local org-air-project--sort-key org-air-project-sort-key))
    (unless org-air-project--sort-direction
      (setq-local org-air-project--sort-direction org-air-project-sort-direction))
    (erase-buffer)
    ;; R22-5: when the rail is POPPED OUT, render the doc pane LEFT-ONLY and
    ;; push the project rail into the shared `*org-air-rail*' side window
    ;; (reusing the board's side-window primitives).  `unset' (the initial
    ;; sentinel) is NOT popped out.
    (setq org-air-view--orientation
          (cond
           ((eq org-air-view--rail-popped-out t) 'side-window)
           ((and org-air-project-show-inspector
                 (not (org-air-view--board-only-p width)))
            'two-pane)
           (t 'board-only)))
    (insert (org-air-project--header-line width) "\n\n")
    (cond
     ((null docs)
      (insert "  "
              (propertize "No Air documents found here." 'face 'org-air-face-empty)
              "\n"))
     ((eq org-air-view--orientation 'two-pane)
      (let ((body (car (org-air-project--two-pane-body docs left-fn width))))
        (org-air-view--insert-lines body)))
     ((eq org-air-view--orientation 'side-window)
      ;; R22-5: the rail lives in the side window now — doc pane only.
      (setq org-air-view--inspector-region-height nil)
      (funcall left-fn width))
     (t
      ;; board-only: the state summary + the full-width left pane.
      (setq org-air-view--inspector-region-height nil)
      (org-air-project--insert-state-summary-line docs)
      (funcall left-fn width)))
    ;; Drop the trailing newline so the buffer is exactly its line count.
    (goto-char (point-max))
    (when (and (bolp) (> (point-max) (point-min))) (delete-char -1))
    (goto-char (point-min))
    (org-air-project--next-doc)
    (setq org-air-project--rendered-width width)
    ;; Locate + fill the inspector region (real buffer; buffer-locals set).
    (org-air-view--setup-inspector)
    ;; R22-5: side-window rail lifecycle, mirroring the board: show/refresh
    ;; the popped-out rail; a responsive narrow teardown hides it.  Two-pane
    ;; (inline rail) leaves any rail buffer untouched — the pop-IN path in
    ;; `org-air-rail-toggle' hides the side window before re-rendering.
    (cond
     ((eq org-air-view--orientation 'side-window)
      (org-air-rail--show (current-buffer) width))
     ((eq org-air-view--orientation 'board-only)
      (org-air-rail--hide (current-buffer))))))

(defun org-air-project--resize-refresh ()
  "Re-render the project view when the displaying window changed (R14 D-P1.B).
Rides the round-9 C1 resize path so widening/narrowing the window flips
between two-pane and board-only."
  (let ((width (org-air-project--render-width)))
    (unless (eql width org-air-project--rendered-width)
      (when org-air-project--root
        (org-air-project--render org-air-project--root)))))

(defun org-air-project--next-doc ()
  "Move point to the next doc row, if any (landing on its title; R21-2)."
  (let ((pos (next-single-property-change (point) 'org-air-doc)))
    (when pos
      (goto-char pos)
      (org-air-view--goto-row-title))))


;;;; ---------------------------------------------------------------------
;;;; Commands + mode
;;;; ---------------------------------------------------------------------

(defun org-air-project-refresh ()
  "Re-render the current Air project view."
  (interactive)
  (when org-air-project--root
    (org-air-project--render org-air-project--root)))

(defun org-air-project--render-current ()
  "Re-render the current project view (R22-5: the rail-toggle dispatch target).
Non-interactive sibling of `org-air-project-refresh' used by the shared
`org-air-view--refresh-current' so the rail toggle never forks."
  (when org-air-project--root
    (org-air-project--render org-air-project--root)))

(defun org-air-project-group-by-state ()
  "Group the project view by state (airctl -a)."
  (interactive)
  (setq org-air-project-group 'state)
  (org-air-project-refresh))

(defun org-air-project-group-by-directory ()
  "Group the project view by directory (airctl -Da)."
  (interactive)
  (setq org-air-project-group 'directory)
  (org-air-project-refresh))

(defun org-air-project-group-by-tag ()
  "Group the project view by tag (airctl -Ta)."
  (interactive)
  (setq org-air-project-group 'tag)
  (org-air-project-refresh))

(defun org-air-project-sort-cycle ()
  "Cycle the project sort key and refresh (R22-3: shared sort core).
Thin alias of `org-air-view-sort-cycle' (the inherited `o'); the project
mode seeds the shared spec (name/created/updated + refresh)."
  (interactive)
  (org-air-view-sort-cycle))

(defun org-air-project-sort-reverse ()
  "Toggle the project sort direction and refresh (R22-3: shared sort core).
Thin alias of `org-air-view-sort-reverse' (the inherited `O')."
  (interactive)
  (org-air-view-sort-reverse))

(defun org-air-project-sort-set (key)
  "Set the sort KEY directly (name/created/updated) and refresh (R16 D-P4).
R22-3: writes the SHARED `org-air-view--sort-key' the comparator reads."
  (interactive
   (list (intern (completing-read "Sort by: " '("name" "created" "updated")
                                  nil t))))
  (setq-local org-air-view--sort-key key)
  (org-air-project-refresh)
  (message "org-air project: sort by %s" key))

(defun org-air-project-next ()
  "Move point to the next doc row, landing on its title (R21-2)."
  (interactive)
  (let ((pos (next-single-property-change
              (line-end-position) 'org-air-doc)))
    (when pos
      (goto-char pos)
      (org-air-view--goto-row-title))))

(defun org-air-project-prev ()
  "Move point to the previous doc row, landing on its title (R21-2)."
  (interactive)
  (let ((pos (previous-single-property-change
              (line-beginning-position) 'org-air-doc)))
    (when pos
      (goto-char pos)
      (goto-char (or (previous-single-property-change pos 'org-air-doc)
                     (line-beginning-position)))
      (org-air-view--goto-row-title))))

(defun org-air-project-visit ()
  "Visit the Air doc on the current row."
  (interactive)
  (let ((doc (get-text-property (point) 'org-air-doc)))
    (if doc
        (find-file-other-window (org-air-doc-file doc))
      (user-error "No Air document on this line"))))

(defun org-air-project-filter (tags)
  "Filter the project doc tree to TAGS (R18 D-P3, shares the board core).
The prompt is PRE-FILLED with the active filter and the chosen terms
combine with the shared `org-air-filter-match' combinator (AND by default,
`M-/' toggles) — the same filter core the board uses, applied to
`org-air-doc-tags'."
  (interactive
   (list (org-air-view--read-filter
          (delete-dups
           (sort (apply #'append
                        (mapcar #'org-air-doc-tags
                                (if org-air-project--root
                                    (org-air-project--collect-docs
                                     org-air-project--root)
                                  nil)))
                 #'string<)))))
  (setq org-air-view--tag-filter (unless (null tags) tags))
  (org-air-project-refresh))

(defun org-air-project-quit ()
  "Quit the project view and restore the previous window."
  (interactive)
  (quit-window))

(defvar org-air-project-mode-map
  (let ((map (make-sparse-keymap)))
    ;; R20-5(b): a THIN child of the shared view-core map.  Every shared
    ;; board key keeps the board's meaning by INHERITANCE (RET pane,
    ;; mouse-1, v/V pane open/close, \ filter-clear, M-/ AND/OR toggle); the
    ;; old domain verbs that SHADOWED s / d / t / o / O are GONE — a user
    ;; who knows the board drives the project with no relearning.  The
    ;; state / tag groupings and the sort cycle stay reachable via
    ;; `M-x org-air-project-group-by-state' / `-by-tag' / `-sort-cycle', so
    ;; the airctl -a / -Ta parity exists without stealing the shared keys.
    (set-keymap-parent map org-air-view-core-map)
    (define-key map (kbd "n") #'org-air-project-next)
    (define-key map (kbd "p") #'org-air-project-prev)
    ;; RET (inherited) opens the pane; S-RET visits the doc (the project's
    ;; visit target), mirroring the board's S-RET visit.
    (define-key map (kbd "<S-return>") #'org-air-project-visit)
    (define-key map (kbd "S-RET") #'org-air-project-visit)
    ;; The per-mode doc-tag filter (shares the board's pre-fill + AND
    ;; default + M-/ toggle core); `g' refreshes, `q' quits.
    (define-key map (kbd "/") #'org-air-project-filter)
    (define-key map (kbd "g") #'org-air-project-refresh)
    (define-key map (kbd "q") #'org-air-project-quit)
    map)
  "Keymap for `org-air-project-mode'.")

(define-derived-mode org-air-project-mode special-mode "Org-Air-Project"
  "Major mode for the Air-docs project tree view (F5)."
  (setq-local truncate-lines t)
  (setq-local cursor-type 'box)
  (setq-local line-spacing org-air-line-spacing)
  ;; R18 D-P5.1: the calm nano-style mode-line (status lives in the header).
  (org-air-view--install-modeline)
  ;; R14 D-P1.B: responsive re-render (two-pane <-> board-only) on resize,
  ;; riding the round-9 C1 window-size path.
  (setq-local org-air-layout-refresh-function #'org-air-project--resize-refresh)
  ;; R22-3: seed the SHARED sort spec so the inherited o/O cycle/reverse
  ;; drive the project's name/created/updated sort (one core, no fork).
  (setq-local org-air-view--sort-keys '(name created updated))
  (setq-local org-air-view--sort-refresh #'org-air-project-refresh)
  (unless org-air-view--sort-key
    (setq-local org-air-view--sort-key org-air-project-sort-key))
  (unless org-air-view--sort-direction
    (setq-local org-air-view--sort-direction org-air-project-sort-direction))
  ;; R14 D-P1.B: the project view hosts the shared mid-rail inspector; the
  ;; debounced point-tracking hook is INERT under batch (P0 contract).
  (unless noninteractive
    (add-hook 'post-command-hook #'org-air-view--inspector-post-command nil t)
    ;; R18 D-P3/D-P4: the bottom view pane auto-follows here too (same hook
    ;; as the board; guarded on a live pane window, inert under batch).
    (add-hook 'post-command-hook #'org-air-view--view-pane-post-command nil t)
    ;; R22-2b: snap point off the dead margin/rail/pad columns onto the doc
    ;; row title (project rows carry `org-air-doc' via the shared
    ;; `--insert-row'); idempotent on a propertized column, inert in batch.
    (add-hook 'post-command-hook #'org-air-view--normalize-point nil t))
  ;; R22-5: tear down a popped-out rail side window + buffer when the
  ;; project buffer is killed (it must not outlive its host), mirroring the
  ;; board's kill-buffer-hook.
  (add-hook 'kill-buffer-hook #'org-air-rail--teardown nil t)
  (org-air-layout-install-window-size-hook)
  (buffer-disable-undo))

;;;###autoload
(defun org-air-project (&optional root)
  "Open the Air project doc-tree view for ROOT.
With several configured projects, prompt for one (`org-air-projects' /
`org-air-sources'); ROOT non-nil opens it directly."
  (interactive)
  (let* ((roots (org-air-project-roots))
         (root (or root
                   (cond
                    ((null roots)
                     (read-directory-name "Air project root: "))
                    ((= (length roots) 1) (car roots))
                    (t (completing-read "Air project: " roots nil t)))))
         (buffer (get-buffer-create "*org-air-project*")))
    (with-current-buffer buffer
      (org-air-project-mode)
      (setq org-air-project--root (expand-file-name root))
      (org-air-project--render org-air-project--root))
    (pop-to-buffer buffer)))

(provide 'org-air-project)

;;; org-air-project.el ends here
