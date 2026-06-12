;;; org-air-inbox.el --- Inbox capture and refile for org-air -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: org-air contributors
;; Keywords: outlines, calendar
;; Package-Requires: ((emacs "29.1") (org "9.6"))

;;; Commentary:

;; Inbox-first capture and lightweight refile helpers for org-air.

;;; Code:

(require 'org)
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

;;;###autoload
(defun org-air-refile-item (item target-file &optional target-heading tags scheduled)
  "Move ITEM to TARGET-FILE under TARGET-HEADING.

Interactively, ITEM defaults to the heading at point.  TAGS replaces the item's
tags when non-nil.  SCHEDULED is an Org timestamp string, or empty to leave the
schedule unchanged."
  (interactive
   (let* ((item (org-air-item-create
                 :title (org-get-heading t t t t)
                 :tags (org-get-tags nil t)
                 :file (or (buffer-file-name) "")
                 :marker (copy-marker (point-marker))
                 :todo (org-get-todo-state)
                 :priority nil
                 :scheduled nil
                 :deadline nil
                 :group nil))
          (file (read-file-name "Refile to file: "))
          (heading (read-string "Under heading (empty for file end): "))
          (tags (let ((value (read-string "Tags (colon or comma separated, empty unchanged): ")))
                  (unless (string-empty-p value)
                    (split-string value "[:,[:space:]]+" t))))
          (scheduled (read-string "Schedule timestamp (empty unchanged): ")))
     (list item file heading tags scheduled)))
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
          (when (and scheduled (not (string-empty-p scheduled)))
            (org-schedule nil scheduled))))
      (save-buffer)))
  (message "Refiled %s" (org-air-item-title item)))

(provide 'org-air-inbox)
;;; org-air-inbox.el ends here
