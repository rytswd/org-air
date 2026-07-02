;;; org-air-inbox.el --- Inbox capture and refile for org-air -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: org-air contributors
;; Keywords: outlines, calendar
;; Version: 0.1.0
;; URL: https://github.com/rytswd/org-air
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Inbox-first capture and lightweight refile helpers for org-air.

;;; Code:

(require 'org)
(require 'seq)
(require 'subr-x)
(require 'org-air-query)

(defvar org-air-inbox-file)

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
  "Return insertion point for FILE under optional HEADING."
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
  "Read an optional target HEADING in FILE via completion (R24-1).
Candidates are FILE's real headings plus a leading `(file end)' default;
`(file end)' / empty / RET => nil (append at file end, the current
behaviour).  Returns nil with NO prompt when FILE has no headings."
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
  (or (ignore-errors (org-air-query-files))
      (list (org-air-item-file item))))

(defun org-air-inbox--file-candidates (files)
  "Return `⌂ <name>' refile candidates for FILES, disambiguating clashes (R19-2).
When two files share a basename, the candidate shows a parent-dir/name tail
so each `⌂' entry maps to exactly one file."
  (let ((bases (mapcar #'file-name-nondirectory files)))
    (mapcar (lambda (file)
              (let ((base (file-name-nondirectory file)))
                (concat "⌂ "
                        (if (> (seq-count (lambda (b) (equal b base)) bases) 1)
                            (concat (file-name-nondirectory
                                     (directory-file-name
                                      (file-name-directory file)))
                                    "/" base)
                          base))))
            files)))

(defun org-air-inbox--refile-candidates (item)
  "Return the action-first refile menu for ITEM (R20-4c).
Leads with the dedicated `⌂ Move to file…' action (the single most
important move-this-item-to-a-real-file path, with its own focused
picker), then `Tags…' and `Category…' (both `completing-read-multiple',
add/remove), then the spelled-out `Schedule: …' quicks.  The named
actions are the contract — no flat tag/group/file soup, so the move is a
first-class entry rather than a needle in a candidate haystack."
  (ignore item)
  (list "⌂ Move to file…"
        "Tags…"
        "Category…"
        "Schedule: today"
        "Schedule: tomorrow"
        "Schedule: this week"
        "Schedule: someday"))

(defun org-air-inbox--edit-tags (item)
  "Read a REPLACEMENT tag list for ITEM, pre-filled with its current tags (R19-2).
Uses `completing-read-multiple' over the tag vocabulary seeded with the
item's existing tags (joined by `,') so the user SEES the full set and can
add OR remove; the returned list replaces the tags."
  (let ((current (org-air-item-tags item))
        (vocab (delete-dups (seq-mapcat #'org-air-item-tags
                                        (ignore-errors (org-air-query-items))))))
    (completing-read-multiple
     "Tags: " vocab nil nil
     (when current (mapconcat #'identity current ",")))))

(defun org-air-inbox--edit-categories (item)
  "Read a pre-filled category list for ITEM (R20-4a).
Uses `completing-read-multiple' seeded with the item's current category (its
`org-air-item-group') over the group vocabulary so a single pick is the
common case (add/remove from there).  Multiple
picks are allowed: the caller makes the FIRST the `:CATEGORY:' and adds any
extras as tags, so nothing the user typed is lost."
  (let ((current (org-air-item-group item))
        (vocab (delete-dups (delq nil (mapcar #'org-air-item-group
                                              (ignore-errors (org-air-query-items)))))))
    (completing-read-multiple
     "Category: " vocab nil nil
     (when (and current (not (string-empty-p current))) current))))

(defun org-air-inbox--read-move-target (item)
  "Read the DEDICATED `⌂ Move to file…' target for ITEM (R20-4c).
A focused picker over the REAL expanded Org files
\(`org-air-inbox--file-candidates', disambiguated) plus `⌂ other file…'
\(`read-file-name'), then an optional `Under heading: '.  Resolution reuses
the R19-2 `org-air-inbox--decode-file-choice' move-bug hardening UNCHANGED,
so a chosen candidate maps to the actual file.  Returns (FILE . HEADING)."
  (let* ((files (org-air-inbox--target-files item))
         (cands (append (org-air-inbox--file-candidates files)
                        '("⌂ other file…")))
         (choice (completing-read "Move to file: " cands nil t))
         (file (org-air-inbox--decode-file-choice choice item))
         ;; R24-1: complete the sub-heading over the TARGET FILE's real
         ;; headings (default `(file end)'; skipped entirely when none),
         ;; instead of a blind `read-string'.
         (heading (org-air-inbox--read-heading file)))
    (cons file (unless (and heading (string-empty-p heading)) heading))))

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

(defun org-air-inbox--decode-target (choice item)
  "Decode the named refile CHOICE for ITEM into `org-air-refile-item' args.
Dispatches the R20-4 action-first menu: `⌂ Move to file…' opens the
dedicated `org-air-inbox--read-move-target' picker; `Tags…' / `Category…'
run the `completing-read-multiple' editors; the `Schedule: …' quicks map to
Org shift strings.  Returns (ITEM FILE HEADING TAGS SCHEDULED CATEGORY)."
  (cond
   ((string= choice "⌂ Move to file…")
    (let ((target (org-air-inbox--read-move-target item)))
      (list item (car target) (cdr target) nil nil nil)))
   ((string= choice "Tags…")
    (list item (org-air-item-file item) nil
          (org-air-inbox--edit-tags item) nil nil))
   ((string= choice "Category…")
    (let* ((cats (org-air-inbox--edit-categories item))
           (cat (car cats))
           (extra (cdr cats)))
      (list item (org-air-item-file item) nil
            (and extra (delete-dups (append extra (org-air-item-tags item))))
            nil cat)))
   ((string= choice "Schedule: today")
    (list item (org-air-item-file item) nil nil "." nil))
   ((string= choice "Schedule: tomorrow")
    (list item (org-air-item-file item) nil nil "+1d" nil))
   ((string= choice "Schedule: this week")
    (list item (org-air-item-file item) nil nil "+1w" nil))
   ((string= choice "Schedule: someday")
    (list item (org-air-item-file item) nil
          (delete-dups (cons "someday" (org-air-item-tags item))) "" nil))
   (t
    ;; free-text / unmatched fallback: treat as a move-to-file target.
    (let ((target (org-air-inbox--read-move-target item)))
      (list item (car target) (cdr target) nil nil nil)))))

;;;###autoload
(defun org-air-refile-item (item target-file &optional target-heading tags scheduled category)
  "Move ITEM to TARGET-FILE under TARGET-HEADING.

Interactively, dashboard items use the R20-4 action-first refile menu: a
short, truncated `Refile \"<title…>\" → ' prompt leading with the dedicated
`⌂ Move to file…' picker, then `Tags…' / `Category…' (both
`completing-read-multiple', add/remove) and the spelled-out `Schedule: …'
quicks.  TAGS replaces the item's tags when non-nil; CATEGORY sets the
moved heading's `:CATEGORY:' property; SCHEDULED is an Org timestamp string
\(empty clears the schedule)."
  (interactive
   (let* ((item (org-air-inbox--interactive-item))
          (choice (completing-read
                   (format "Refile \"%s\" → "
                           (truncate-string-to-width
                            (org-air-item-title item) 24 nil nil "…"))
                   (org-air-inbox--refile-candidates item)
                   nil t)))
     (org-air-inbox--decode-target choice item)))
  (let ((text nil))
    (with-current-buffer (org-air-inbox--source-buffer item)
      (save-excursion
        ;; R26-8: a cache-hydrated item carries (FILE . POS), not a marker.
        (goto-char (let ((m (org-air-item-marker item)))
                     (if (markerp m) (marker-position m) (or (cdr-safe m) 1))))
        (org-back-to-heading t)
        (let ((begin (point))
              (end (save-excursion (org-end-of-subtree t t) (point))))
          (setq text (buffer-substring begin end))
          (delete-region begin end)
          (save-buffer))))
    (with-current-buffer (find-file-noselect (expand-file-name target-file))
      (let ((insert-marker (org-air-inbox--target-position target-file target-heading)))
        (goto-char insert-marker)
        (insert text)
        (unless (bolp) (insert "\n"))
        (save-excursion
          (goto-char insert-marker)
          (org-back-to-heading t)
          (when tags (org-set-tags tags))
          (when (and category (not (string-empty-p category)))
            (org-set-property "CATEGORY" category))
          (when scheduled
            (org-schedule nil (unless (string-empty-p scheduled) scheduled)))))
      (save-buffer)))
  (message "Refiled → %s"
           (file-name-nondirectory (expand-file-name target-file)))
  (when (derived-mode-p 'org-air-view-mode)
    (when (fboundp 'org-air-refresh)
      (org-air-refresh))))

(provide 'org-air-inbox)

;; Local Variables:
;; package-lint-main-file: "org-air.el"
;; End:
;;; org-air-inbox.el ends here
