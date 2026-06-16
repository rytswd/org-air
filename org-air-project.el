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

(defcustom org-air-project-group 'state
  "Default grouping for the Air project view: `state', `directory' or `tag'.
Mirrors `airctl status' -a / -Da / -Ta; toggled in-view with s / d / t."
  :type '(choice (const state) (const directory) (const tag))
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
  name file state tags updated relpath)

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
     :relpath relpath)))

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
:attention :show-state."
  (pcase org-air-project-group
    ('directory (org-air-project--sections-by-directory docs))
    ('tag (org-air-project--sections-by-tag docs))
    (_ (org-air-project--sections-by-state docs))))

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
  "Insert SECTION's heading (D-P5.C): icon + title + count badge.
Reuses the board's section-heading styling for parity."
  (let ((start (point))
        (icon (plist-get section :icon))
        (count (length (plist-get section :docs))))
    (insert "  "
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

(defun org-air-project--insert-doc-row (doc widths show-state)
  "Insert DOC as a shared-row (D-P5.B) using `org-air-view--insert-row'.
WIDTHS is the fixed (DCOL TCOL OCOL).  SHOW-STATE adds a leading state
chip in the prefix (dir/tag modes, where the state is not the section)."
  (org-air-view--insert-row
   :prefix (concat "  " (when show-state (org-air-project--state-chip doc)))
   :title (org-air-doc-name doc)
   :date-text (org-air-project--doc-date-text doc)
   :tags (org-air-project--doc-tagstr doc)
   :origin-text (org-air-project--doc-origin-text doc)
   :origin-face 'org-air-face-group
   :widths widths
   :props (list 'org-air-doc doc
                'org-air-marker (org-air-doc-file doc)
                'mouse-face 'org-air-face-cursor)
   :face 'org-air-face-title))

;;;; ---------------------------------------------------------------------
;;;; View
;;;; ---------------------------------------------------------------------

(defvar-local org-air-project--root nil
  "Air root rendered in this project-view buffer.")

(defun org-air-project--render (root)
  "Render the Air project view for ROOT into the current buffer (D-P5).
A section loop over the shared row primitive: no box-tree glyphs."
  (let* ((inhibit-read-only t)
         (org-air-project--width (org-air-project--render-width))
         ;; drive the shared row primitive's width seam.
         (org-air-view-width org-air-project--width)
         (docs (org-air-project--collect-docs root))
         (sections (org-air-project--sections docs))
         (widths (org-air-project--doc-widths docs)))
    (erase-buffer)
    (insert (propertize "  org-air · project" 'face 'org-air-face-title)
            "\n\n")
    ;; Compact state-count summary (F5e).
    (insert "  "
            (mapconcat
             (lambda (state)
               (let ((n (seq-count (lambda (d) (equal (org-air-doc-state d) state))
                                   docs)))
                 (concat (propertize (org-air-project--state-badge state)
                                     'face (org-air-project--state-face state))
                         " " (number-to-string n))))
             org-air-project-states "   ")
            "\n\n")
    (if (null docs)
        (insert "  "
                (propertize "No Air documents found here." 'face 'org-air-face-empty)
                "\n")
      (dolist (section sections)
        (org-air-project--insert-section-heading section)
        (let ((show-state (plist-get section :show-state)))
          (dolist (doc (plist-get section :docs))
            (org-air-project--insert-doc-row doc widths show-state)))
        (insert "\n")))
    (goto-char (point-min))
    (org-air-project--next-doc)))

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
    (define-key map (kbd "g") #'org-air-project-refresh)
    (define-key map (kbd "q") #'org-air-project-quit)
    map)
  "Keymap for `org-air-project-mode'.")

(define-derived-mode org-air-project-mode special-mode "Org-Air-Project"
  "Major mode for the Air-docs project tree view (F5)."
  (setq-local truncate-lines t)
  (setq-local cursor-type 'box)
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
