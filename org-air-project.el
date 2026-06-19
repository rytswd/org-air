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

(defcustom org-air-project-line2-indent 2
  "Extra indent (cols) of a doc block's second line under its title (R14 D-P1.A).
The `▤ relpath  created… updated…' detail line sits this many columns to
the right of the title so it reads as a secondary detail of line 1."
  :type 'integer
  :group 'org-air)

(defcustom org-air-project-show-inspector t
  "When non-nil, the project view hosts a mid-rail inspector (R14 D-P1.B).
Mirrors `org-air-show-inspector' for the board: above
`org-air-rail-min-width' the view is two-pane (doc sections + a project
rail of Summary + Inspector); below it the view is board-only."
  :type 'boolean
  :group 'org-air)

(defcustom org-air-project-group 'state
  "Default grouping for the Air project view: `state', `directory' or `tag'.
Mirrors `airctl status' -a / -Da / -Ta; toggled in-view with s / d / t."
  :type '(choice (const state) (const directory) (const tag))
  :group 'org-air)

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
      (setq state (downcase (or (org-air-project--read-keyword "state") "draft"))
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
  "Return the list of `org-air-doc' under ROOT's Air directory."
  (let ((air (org-air-project--air-dir root)))
    (when (file-directory-p air)
      (mapcar (lambda (f) (org-air-project--read-doc f root))
              (sort (directory-files-recursively air "\\.org\\'")
                    #'string-lessp)))))

;;;; ---------------------------------------------------------------------
;;;; Badges / glyphs
;;;; ---------------------------------------------------------------------

(defun org-air-project--state-badge (state)
  "Return the display badge string for STATE (emoji on GUI, else TTY text)."
  (let ((pair (cdr (assoc state org-air-project-state-badges))))
    (cond
     ((null pair) (format "[%s]" (upcase (substring state 0 1))))
     ((display-graphic-p) (car pair))
     (t (cdr pair)))))

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

(defun org-air-project--state-chip (doc)
  "Return a small leading state chip for DOC (dir/tag modes; D-P5.C).
When grouping by something other than state, the state is no longer the
section, so each row's prefix carries the doc's state as a small chip."
  (concat (propertize (org-air-project--state-badge (org-air-doc-state doc))
                      'face (org-air-project--state-face (org-air-doc-state doc)))
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
                    ('directory (org-air-project--sections-by-directory docs))
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
                 (list :icon (org-air-project--state-badge state)
                       :icon-face (org-air-project--state-face state)
                       :title (org-air-project--state-title state)
                       :title-face 'org-air-face-section
                       :docs members
                       :attention (member state org-air-project--attention-states)
                       :show-state nil))))
           order))))

(defun org-air-project--sections-by-directory (docs)
  "Return folder sections for DOCS, the dir hierarchy living in the path col."
  (let ((dirs (seq-uniq
               (mapcar (lambda (d)
                         (car (split-string (org-air-doc-relpath d) "/")))
                       docs))))
    (mapcar
     (lambda (dir)
       (list :icon (org-air-layout-glyph 'origin)
             :icon-face 'org-air-face-faded
             :title (concat dir "/")
             :title-face 'org-air-face-section
             :docs (seq-filter
                    (lambda (d)
                      (equal dir (car (split-string (org-air-doc-relpath d) "/"))))
                    docs)
             :attention nil
             :show-state t))
     (sort dirs #'string-lessp))))

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

(defun org-air-project--doc-dates-str (doc)
  "Return the quiet `created …    updated …' detail for DOC (R14 D-P1.A)."
  (let* ((created (org-air-doc-created doc))
         (updated (org-air-doc-updated doc))
         (parts (delq nil
                      (list (when created
                              (format "created %s" (format-time-string "%F" created)))
                            (when updated
                              (format "updated %s" (format-time-string "%F" updated)))))))
    (propertize (mapconcat #'identity parts "    ") 'face 'org-air-face-faded)))

(defun org-air-project--insert-doc-block (doc width &optional show-state)
  "Insert DOC as a TWO-LINE left-flowing block (R14 D-P1.A) fitted to WIDTH.
Line 1 = <indent><state-chip?>Title  #tag #tag (calm pills, NOT
right-pinned; the title truncates LAST).  Line 2 = <indent+line2-indent>
▤ relpath    created …    updated … (quieter, the document svg-file-icon
overlays the ▤ cell; the filename is NOT right-aligned).  Both lines carry
`org-air-doc' + `org-air-marker' so point on EITHER identifies the doc.
SHOW-STATE adds the leading state chip (dir/tag grouping modes)."
  (let* ((start (point))
         (indent "  ")
         (l2-indent (concat indent (make-string (max 0 org-air-project-line2-indent) ?\s)))
         ;; line 1: indent + state-chip? + title + inline tags.
         (chip (if show-state (org-air-project--state-chip doc) ""))
         (tagstr (org-air-project--doc-tagstr doc))
         (gap (if (string-empty-p tagstr) "" "  "))
         (l1-prefix (concat indent chip))
         (avail (max 1 (- width (string-width l1-prefix)
                          (string-width gap) (string-width tagstr))))
         (title (org-air-doc-name doc))
         (title (if (<= (string-width title) avail)
                    title
                  (truncate-string-to-width title avail nil nil
                                            (org-air-view--glyph 'more))))
         (line1 (concat l1-prefix (propertize title 'face 'org-air-face-title)
                        gap tagstr))
         ;; line 2: indented ▤ relpath + created/updated.
         (origin-glyph (org-air-layout-glyph 'origin))
         (file-cell (org-air-view--svg-file-icon origin-glyph))
         (relpath (propertize (org-air-doc-relpath doc) 'face 'org-air-face-faded))
         (line2 (concat l2-indent file-cell " " relpath "    "
                        (org-air-project--doc-dates-str doc))))
    (insert (org-air-view--pad-to line1 width) "\n")
    (insert (org-air-view--pad-to line2 width) "\n")
    (add-text-properties start (point)
                         (list 'org-air-doc doc
                               'org-air-marker (org-air-doc-file doc)
                               'mouse-face 'org-air-face-cursor
                               'font-lock-face 'org-air-face-title))))

;;;; ---------------------------------------------------------------------
;;;; View
;;;; ---------------------------------------------------------------------

(defvar-local org-air-project--root nil
  "Air root rendered in this project-view buffer.")

(defvar-local org-air-project--rendered-width nil
  "Width of the most recent project-view render (R14 D-P1.B resize guard).")

(defvar-local org-air-project--sort-key nil
  "Active per-buffer sort key (R16 D-P4); seeded from `org-air-project-sort-key'.")
(defvar-local org-air-project--sort-direction nil
  "Active per-buffer sort direction (R16 D-P4); seeded from the defcustom.")

(defun org-air-project--sort-key-active ()
  "Return the active sort key, seeding from the defcustom when unset."
  (or org-air-project--sort-key org-air-project-sort-key))

(defun org-air-project--sort-direction-active ()
  "Return the active sort direction, seeding from the defcustom when unset."
  (or org-air-project--sort-direction org-air-project-sort-direction))

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
  "Strict total order over docs A and B (R16 D-P4).
1. the active sort key in the active direction (a nil date sorts LAST in
   BOTH directions — the partition rule);
2. tiebreak: name then relpath ascending (byte-stable equal keys).
D-P5 prepends a state-rank primary step ahead of the key."
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
Plain text (svg-free) so it is part of every project fixture's byte
contract; quiet faces."
  (let* ((key (symbol-name (org-air-project--sort-key-active)))
         (dir (org-air-project--sort-direction-active))
         (mk (org-air-layout-glyph 'sort-key))
         (arrow (org-air-layout-glyph (if (eq dir 'descending) 'sort-desc 'sort-asc))))
    (concat (propertize mk 'face 'org-air-face-faded)
            " "
            (propertize key 'face 'org-air-face-summary-label)
            " "
            (propertize arrow 'face 'org-air-face-faded))))

(defun org-air-project--header-line (width)
  "Return the project header line for WIDTH: title left, sort badge right.
The badge order is part of the byte contract (R16 D-P4)."
  (let* ((title (propertize "  org-air · project" 'face 'org-air-face-title))
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
    (let ((show-state (plist-get section :show-state)))
      (dolist (doc (plist-get section :docs))
        (org-air-project--insert-doc-block doc width show-state)))
    (insert "\n")))

(defun org-air-project--insert-state-summary-line (docs)
  "Insert the compact one-line state-count summary for DOCS (board-only)."
  (insert "  "
          (mapconcat
           (lambda (state)
             (let ((n (seq-count (lambda (d) (equal (org-air-doc-state d) state))
                                 docs)))
               (concat (propertize (org-air-project--state-badge state)
                                   'face (org-air-project--state-face state))
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

(defun org-air-project--insert-rail (docs rail-width target-h)
  "Insert the project rail (Summary + Inspector) for DOCS at RAIL-WIDTH (D-P1.B).
The inspector fills the fixed reserved region to TARGET-H (the doc pane
height) so the divider spans the docs; there are no Filters/Actions here.
The reserved region is rendered with a nil thing (just reserves height +
the `Inspector' header); `org-air-view--setup-inspector' fills it with the
first doc in the real buffer (buffer-locals set there)."
  (org-air-project--insert-summary docs rail-width)
  (insert "\n")
  (if org-air-project-show-inspector
      (let* ((top-used (count-lines (point-min) (point)))
             (reserved (max 1 (- target-h top-used))))
        (setq org-air-view--inspector-region-height reserved)
        (dolist (l (org-air-view--inspector-rail-lines nil rail-width reserved))
          (insert l "\n")))
    (setq org-air-view--inspector-region-height nil)))

(defun org-air-project--two-pane-body (docs sections width)
  "Return (BODY-LINES . FILL-ROW) composing DOCS | project-rail at WIDTH.
LEFT = the two-line doc SECTIONS; RIGHT = the project rail (Summary +
Inspector).  The rail is sized to the doc-pane height so the divider runs
the full body and the layout is deterministic (no `window-height' seam)."
  (let* ((rail-width (org-air-view--rail-width width))
         (divider (org-air-view--divider))
         (item-width (max 20 (- width rail-width (string-width divider))))
         (doc-lines (org-air-view--render-lines
                     item-width
                     (lambda ()
                       (org-air-project--insert-doc-sections sections item-width))))
         (doc-h (max 1 (length doc-lines)))
         (rail-lines (mapcar
                      (lambda (l) (org-air-view--pad-to l rail-width))
                      (org-air-view--render-lines
                       rail-width
                       (lambda ()
                         (org-air-project--insert-rail docs rail-width doc-h))))))
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
           (concat (propertize (org-air-project--state-badge state)
                               'face (org-air-project--state-face state))
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
         (docs (org-air-project--collect-docs root))
         (sections (org-air-project--sections docs)))
    ;; R14 D-P1.B: this buffer hosts the SHARED mid-rail inspector with the
    ;; project's property + fields function.
    (setq-local org-air-view--inspector-active (and org-air-project-show-inspector t)
                org-air-view--inspector-property 'org-air-doc
                org-air-view--inspector-fields-function
                #'org-air-project--inspector-doc-fields)
    ;; R16 D-P4: seed the per-buffer sort state from the defcustoms once.
    (unless org-air-project--sort-key
      (setq-local org-air-project--sort-key org-air-project-sort-key))
    (unless org-air-project--sort-direction
      (setq-local org-air-project--sort-direction org-air-project-sort-direction))
    (erase-buffer)
    (setq org-air-view--orientation
          (if (and org-air-project-show-inspector
                   (not (org-air-view--board-only-p width)))
              'two-pane 'board-only))
    (insert (org-air-project--header-line width) "\n\n")
    (cond
     ((null docs)
      (insert "  "
              (propertize "No Air documents found here." 'face 'org-air-face-empty)
              "\n"))
     ((eq org-air-view--orientation 'two-pane)
      (let ((body (car (org-air-project--two-pane-body docs sections width))))
        (org-air-view--insert-lines body)))
     (t
      ;; board-only: the state summary + the full-width doc sections.
      (setq org-air-view--inspector-region-height nil)
      (org-air-project--insert-state-summary-line docs)
      (org-air-project--insert-doc-sections sections width)))
    ;; Drop the trailing newline so the buffer is exactly its line count.
    (goto-char (point-max))
    (when (and (bolp) (> (point-max) (point-min))) (delete-char -1))
    (goto-char (point-min))
    (org-air-project--next-doc)
    (setq org-air-project--rendered-width width)
    ;; Locate + fill the inspector region (real buffer; buffer-locals set).
    (org-air-view--setup-inspector)))

(defun org-air-project--resize-refresh ()
  "Re-render the project view when the displaying window changed (R14 D-P1.B).
Rides the round-9 C1 resize path so widening/narrowing the window flips
between two-pane and board-only."
  (let ((width (org-air-project--render-width)))
    (unless (eql width org-air-project--rendered-width)
      (when org-air-project--root
        (org-air-project--render org-air-project--root)))))

(defun org-air-project--next-doc ()
  "Move point to the next doc row, if any."
  (let ((pos (next-single-property-change (point) 'org-air-doc)))
    (when pos (goto-char pos))))


;;;; ---------------------------------------------------------------------
;;;; Commands + mode
;;;; ---------------------------------------------------------------------

(defun org-air-project-refresh ()
  "Re-render the current Air project view."
  (interactive)
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
  "Cycle the sort key name -> created -> updated -> name and refresh (R16 D-P4)."
  (interactive)
  (setq-local org-air-project--sort-key
              (pcase (org-air-project--sort-key-active)
                ('name 'created)
                ('created 'updated)
                (_ 'name)))
  (org-air-project-refresh)
  (message "org-air project: sort by %s" org-air-project--sort-key))

(defun org-air-project-sort-reverse ()
  "Toggle the sort direction ascending <-> descending and refresh (R16 D-P4)."
  (interactive)
  (setq-local org-air-project--sort-direction
              (if (eq (org-air-project--sort-direction-active) 'descending)
                  'ascending 'descending))
  (org-air-project-refresh)
  (message "org-air project: %s" org-air-project--sort-direction))

(defun org-air-project-sort-set (key)
  "Set the sort KEY directly (name/created/updated) and refresh (R16 D-P4)."
  (interactive
   (list (intern (completing-read "Sort by: " '("name" "created" "updated")
                                  nil t))))
  (setq-local org-air-project--sort-key key)
  (org-air-project-refresh)
  (message "org-air project: sort by %s" key))

(defun org-air-project-next ()
  "Move point to the next doc row."
  (interactive)
  (let ((pos (next-single-property-change
              (line-end-position) 'org-air-doc)))
    (when pos (goto-char pos))))

(defun org-air-project-prev ()
  "Move point to the previous doc row."
  (interactive)
  (let ((pos (previous-single-property-change
              (line-beginning-position) 'org-air-doc)))
    (when pos
      (goto-char pos)
      (goto-char (or (previous-single-property-change pos 'org-air-doc)
                     (line-beginning-position))))))

(defun org-air-project-visit ()
  "Visit the Air doc on the current row."
  (interactive)
  (let ((doc (get-text-property (point) 'org-air-doc)))
    (if doc
        (find-file-other-window (org-air-doc-file doc))
      (user-error "No Air document on this line"))))

(defun org-air-project-quit ()
  "Quit the project view and restore the previous window."
  (interactive)
  (quit-window))

(defvar org-air-project-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") #'org-air-project-group-by-state)
    (define-key map (kbd "d") #'org-air-project-group-by-directory)
    (define-key map (kbd "t") #'org-air-project-group-by-tag)
    (define-key map (kbd "n") #'org-air-project-next)
    (define-key map (kbd "p") #'org-air-project-prev)
    (define-key map (kbd "RET") #'org-air-project-visit)
    ;; R16 D-P4: sort the doc rows — `o' cycles the key, `O' flips direction.
    ;; `org-air-project-sort-set' selects a key directly (M-x; `g' here is a
    ;; single-key refresh, so it cannot also host a `g s' prefix).
    (define-key map (kbd "o") #'org-air-project-sort-cycle)
    (define-key map (kbd "O") #'org-air-project-sort-reverse)
    (define-key map (kbd "g") #'org-air-project-refresh)
    ;; R16 D-P3: the bottom source view pane works here too (one pane, both
    ;; views) — the doc rows carry `org-air-marker' = the doc file.
    (define-key map (kbd "v") #'org-air-view-pane)
    (define-key map (kbd "V") #'org-air-view-pane-close)
    (define-key map (kbd "q") #'org-air-project-quit)
    map)
  "Keymap for `org-air-project-mode'.")

(define-derived-mode org-air-project-mode special-mode "Org-Air-Project"
  "Major mode for the Air-docs project tree view (F5)."
  (setq-local truncate-lines t)
  (setq-local cursor-type 'box)
  (setq-local line-spacing org-air-line-spacing)
  ;; R14 D-P1.B: responsive re-render (two-pane <-> board-only) on resize,
  ;; riding the round-9 C1 window-size path.
  (setq-local org-air-layout-refresh-function #'org-air-project--resize-refresh)
  ;; R14 D-P1.B: the project view hosts the shared mid-rail inspector; the
  ;; debounced point-tracking hook is INERT under batch (P0 contract).
  (unless noninteractive
    (add-hook 'post-command-hook #'org-air-view--inspector-post-command nil t))
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
