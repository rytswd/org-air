;;; org-air-query.el --- org-ql data layer for org-air -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: org-air contributors
;; Keywords: outlines, calendar
;; Package-Requires: ((emacs "29.1") (org "9.6") (org-ql "0.8"))

;;; Commentary:

;; Normalise Org headings from `org-air-files' into `org-air-item' records.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-ql)
(require 'seq)

(defvar org-air-files)

(cl-defstruct (org-air-item
               (:constructor org-air-item-create)
               (:copier nil))
  "A normalised Org heading for org-air views."
  title tags file marker todo priority scheduled deadline group closed)

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
  "Return all existing Org files configured in `org-air-files'."
  (delete-dups
   (seq-map #'file-truename
            (seq-mapcat #'org-air-query--expand-source org-air-files))))

(defun org-air-query--timestamp (property)
  "Return Org timestamp object for PROPERTY at point, or nil."
  (when-let* ((value (org-entry-get (point) property)))
    (ignore-errors (org-timestamp-from-string value))))

(defun org-air-query--group (file)
  "Return display group for heading in FILE."
  (or (org-entry-get (point) "CATEGORY")
      (file-name-base file)))

(defun org-air-query--item-at-point ()
  "Build an `org-air-item' for the heading at point."
  (let ((file (or (buffer-file-name) "")))
    (org-air-item-create
     :title (org-get-heading t t t t)
     :tags (org-get-tags nil nil)
     :file file
     :marker (copy-marker (point-marker))
     :todo (org-get-todo-state)
     :priority (let* ((heading (org-get-heading t t nil t))
                      (priority (org-get-priority heading))
                      (default-priority (org-get-priority "")))
                 (and priority (/= priority default-priority) priority))
     :scheduled (org-air-query--timestamp "SCHEDULED")
     :deadline (org-air-query--timestamp "DEADLINE")
     :closed (org-air-query--timestamp "CLOSED")
     :group (org-air-query--group file))))

;;;###autoload
(defun org-air-query-items (&optional query)
  "Return `org-air-item' records matching org-ql QUERY.

When QUERY is nil, return all headings from `org-air-files'.  The scan is a
single `org-ql-select' pass over the configured files."
  (let ((files (org-air-query-files)))
    (when files
      (org-ql-select files (or query '(heading))
        :action #'org-air-query--item-at-point))))

(provide 'org-air-query)
;;; org-air-query.el ends here
