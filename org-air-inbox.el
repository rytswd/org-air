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

(defun org-air-inbox--source-buffer (item)
  "Return source buffer for ITEM."
  (find-file-noselect (org-air-item-file item)))

(defun org-air-inbox--interactive-item ()
  "Return an org-air item at point in either dashboard or Org buffer."
  (or (get-text-property (point) 'org-air-item)
      (org-air-item-create
       :title (org-get-heading t t t t)
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
  "Return design-style refile candidates for ITEM (R19-2).
A `# edit tags…' candidate opens the FULL tag set pre-filled (add OR
remove); the quick `#tag' candidates still ADD on top of the now-visible
set.  The `⌂' file targets are the REAL expanded Org files (so move-to-
another-file actually works), and `⌂ other file…' reaches an arbitrary
`read-file-name' target."
  (let* ((groups (delete-dups (delq nil (mapcar #'org-air-item-group
                                                (ignore-errors (org-air-query-items))))))
         (tags (delete-dups (seq-mapcat #'org-air-item-tags
                                        (ignore-errors (org-air-query-items)))))
         (file-cands (org-air-inbox--file-candidates
                      (org-air-inbox--target-files item))))
    (append '("# edit tags…")
            (mapcar (lambda (group) (concat "@" group)) groups)
            '(">today" ">tomorrow" ">week" ">someday")
            (mapcar (lambda (tag) (concat "#" tag)) tags)
            file-cands
            '("⌂ other file…"))))

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
  "Decode refile CHOICE for ITEM into argument values."
  (cond
   ((string= choice "# edit tags…")
    (list item (org-air-item-file item) nil
          (org-air-inbox--edit-tags item) nil))
   ((string= choice "⌂ other file…")
    (list item (org-air-inbox--decode-file-choice choice item) nil nil nil))
   ((string-prefix-p "#" choice)
    (list item (org-air-item-file item) nil
          (delete-dups (cons (substring choice 1) (org-air-item-tags item))) nil))
   ((string-prefix-p ">" choice)
    (pcase (substring choice 1)
      ("today" (list item (org-air-item-file item) nil nil "."))
      ("tomorrow" (list item (org-air-item-file item) nil nil "+1d"))
      ("week" (list item (org-air-item-file item) nil nil "+1w"))
      ("someday" (list item (org-air-item-file item) nil
                       (delete-dups (cons "someday" (org-air-item-tags item))) ""))
      (_ (list item (org-air-item-file item) nil nil nil))))
   ((string-prefix-p "@" choice)
    (list item (org-air-item-file item) (substring choice 1) nil nil))
   ((string-prefix-p "⌂ " choice)
    (list item (org-air-inbox--decode-file-choice choice item) nil nil nil))
   (t (list item (read-file-name "Refile to file: ")
            (read-string "Under heading (empty for file end): ") nil nil))))

;;;###autoload
(defun org-air-refile-item (item target-file &optional target-heading tags scheduled)
  "Move ITEM to TARGET-FILE under TARGET-HEADING.

Interactively, dashboard items use the single org-air refile prompt, whose
title shows the item's CURRENT tags, with category (@), timeline (>), quick
tag-add (#), a `# edit tags…' step (the full set, pre-filled, add OR
remove), real file targets (⌂), and `⌂ other file…' candidates (R19-2).
TAGS replaces the item's tags when non-nil.  SCHEDULED is an Org timestamp
string; empty clears the schedule."
  (interactive
   (let* ((item (org-air-inbox--interactive-item))
          (current-tags (org-air-item-tags item))
          (choice (completing-read
                   (format "Refile \"%s\"%s → "
                           (org-air-item-title item)
                           (if current-tags
                               (concat " ["
                                       (mapconcat (lambda (tg) (concat "#" tg))
                                                  current-tags " ")
                                       "]")
                             ""))
                   (org-air-inbox--refile-candidates item)
                   nil nil)))
     (org-air-inbox--decode-target choice item)))
  (let ((text nil))
    (with-current-buffer (org-air-inbox--source-buffer item)
      (save-excursion
        (goto-char (marker-position (org-air-item-marker item)))
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
          (when scheduled
            (org-schedule nil (unless (string-empty-p scheduled) scheduled)))))
      (save-buffer)))
  (message "Refiled %s" (org-air-item-title item))
  (when (derived-mode-p 'org-air-view-mode)
    (when (fboundp 'org-air-refresh)
      (org-air-refresh))))

(provide 'org-air-inbox)

;; Local Variables:
;; package-lint-main-file: "org-air.el"
;; End:
;;; org-air-inbox.el ends here
