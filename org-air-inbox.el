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

(defun org-air-inbox--refile-candidates (item)
  "Return design-style refile candidates for ITEM."
  (let* ((files (and (boundp 'org-air-files) org-air-files))
         (groups (delete-dups (delq nil (mapcar #'org-air-item-group
                                                (ignore-errors (org-air-query-items))))))
         (tags (delete-dups (seq-mapcat #'org-air-item-tags
                                        (ignore-errors (org-air-query-items)))))
         (file-cands (mapcar (lambda (file)
                               (concat "⌂ " (file-name-nondirectory file)))
                             (or files (list (org-air-item-file item))))))
    (append (mapcar (lambda (group) (concat "@" group)) groups)
            '(">today" ">tomorrow" ">week" ">someday")
            (mapcar (lambda (tag) (concat "#" tag)) tags)
            file-cands)))

(defun org-air-inbox--decode-target (choice item)
  "Decode refile CHOICE for ITEM into argument values."
  (cond
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
    (let* ((name (substring choice 2))
           (file (seq-find (lambda (file)
                             (equal name (file-name-nondirectory file)))
                           (if (and (boundp 'org-air-files) org-air-files)
                               org-air-files
                             (list (org-air-item-file item))))))
      (list item (or file (org-air-item-file item)) nil nil nil)))
   (t (list item (read-file-name "Refile to file: ")
            (read-string "Under heading (empty for file end): ") nil nil))))

;;;###autoload
(defun org-air-refile-item (item target-file &optional target-heading tags scheduled)
  "Move ITEM to TARGET-FILE under TARGET-HEADING.

Interactively, dashboard items use the single org-air refile prompt with
category (@), timeline (>), tag (#), and file (⌂) candidates.  TAGS replaces
the item's tags when non-nil.  SCHEDULED is an Org timestamp string; empty
clears the schedule."
  (interactive
   (let* ((item (org-air-inbox--interactive-item))
          (choice (completing-read
                   (format "Refile \"%s\" → " (org-air-item-title item))
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
