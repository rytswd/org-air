;;; org-air-query.el --- Org-QL data layer for org-air -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: org-air contributors
;; Keywords: outlines, calendar
;; Version: 0.1.0
;; URL: https://github.com/rytswd/org-air
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Normalise Org headings from `org-air-files' into `org-air-item' records.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-ql)
(require 'seq)

(defvar org-air-files)

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
     ;; R22-1: detect an EXPLICIT [#X] cookie via `org-priority-regexp'
     ;; (group 2 = the letter, A..E), so [#B] is recorded even though its
     ;; value equals `org-default-priority' (=?B); a cookie-LESS heading
     ;; stays nil.  The old value-equals-default test dropped explicit [#B].
     :priority (let ((heading (org-get-heading t t nil t)))
                 (when (string-match org-priority-regexp heading)
                   (org-get-priority heading)))
     :scheduled (org-air-query--timestamp "SCHEDULED")
     :deadline (org-air-query--timestamp "DEADLINE")
     :closed (org-air-query--timestamp "CLOSED")
     :group (org-air-query--group file))))

;;;###autoload
(defun org-air-query-items (&optional query)
  "Return `org-air-item' records matching org-ql QUERY.

When QUERY is nil, return all headings from `org-air-files'.  The scan is a
single `org-ql-select' pass over the configured files."
  (let ((files (org-air-query-files))
        ;; R21-3: recognise org-air's NEXT/WAIT/... vocabulary for files
        ;; with no `#+TODO:' line (a file's own `#+TODO:' still wins).
        (org-todo-keywords (org-air-query--scan-todo-keywords)))
    (when files
      (org-ql-select files (or query '(heading))
        :action #'org-air-query--item-at-point))))

(defun org-air-query-items-in-files (files &optional query)
  "Return `org-air-item' records for FILES, a subset of the configured set.

Like `org-air-query-items' but restricted to FILES (already-expanded Org
file paths), so the cold first-load query can be split into batches and
run on an idle timer without blocking the frame (R19-1).  QUERY defaults
to all headings."
  (when files
    ;; R21-3: same keyword recognition as `org-air-query-items'.
    (let ((org-todo-keywords (org-air-query--scan-todo-keywords)))
      (org-ql-select files (or query '(heading))
        :action #'org-air-query--item-at-point))))

(provide 'org-air-query)

;; Local Variables:
;; package-lint-main-file: "org-air.el"
;; End:
;;; org-air-query.el ends here
