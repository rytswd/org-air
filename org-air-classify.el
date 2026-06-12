;;; org-air-classify.el --- Classification for org-air items -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: org-air contributors
;; Keywords: outlines, calendar
;; Version: 0.1.0
;; URL: https://github.com/rytswd/org-air
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Pure bucket classification for `org-air-item' records.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-air-query)

(defvar org-air-inbox-file)

(defcustom org-air-stale-days 21
  "Number of days without activity before an item is stale."
  :type 'integer
  :group 'org-air)

(defcustom org-air-upcoming-days 7
  "Number of calendar days ahead considered upcoming."
  :type 'integer
  :group 'org-air)

(defun org-air-classify--time (timestamp)
  "Convert Org TIMESTAMP object to an Emacs time value."
  (when timestamp
    (ignore-errors (org-timestamp-to-time timestamp))))

(defun org-air-classify--days-between (then now)
  "Return calendar days between THEN and NOW."
  (- (time-to-days now) (time-to-days then)))

(defun org-air-classify--done-keywords (item)
  "Return done TODO keywords applicable to ITEM."
  (or (when-let* ((marker (org-air-item-marker item))
                  (buffer (marker-buffer marker)))
        (with-current-buffer buffer
          (or org-done-keywords (default-value 'org-done-keywords))))
      (when-let* ((file (org-air-item-file item))
                  ((file-exists-p file)))
        (with-current-buffer (find-file-noselect file)
          (or org-done-keywords (default-value 'org-done-keywords))))
      (default-value 'org-done-keywords)
      '("DONE")))

(defun org-air-classify--done-p (item)
  "Return non-nil if ITEM has a done TODO state."
  (when-let* ((todo (org-air-item-todo item)))
    (member todo (org-air-classify--done-keywords item))))

(defun org-air-classify--future-or-today-p (timestamp now)
  "Return non-nil when TIMESTAMP is within the upcoming window from NOW."
  (when-let* ((time (org-air-classify--time timestamp)))
    (let ((days (org-air-classify--days-between now time)))
      (and (>= days 0) (<= days org-air-upcoming-days)))))

(defun org-air-classify--past-p (timestamp now)
  "Return non-nil when TIMESTAMP is before today relative to NOW."
  (when-let* ((time (org-air-classify--time timestamp)))
    (> (org-air-classify--days-between time now) 0)))

(defun org-air-classify--marker-timestamp-time (item)
  "Return the first timestamp time found in ITEM's subtree."
  (when-let* ((marker (org-air-item-marker item))
              (buffer (marker-buffer marker)))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (goto-char marker)
          (org-back-to-heading t)
          (let ((end (save-excursion (org-end-of-subtree t t))))
            (when (re-search-forward org-ts-regexp-both end t)
              (ignore-errors
                (org-timestamp-to-time
                 (org-timestamp-from-string (match-string-no-properties 0)))))))))))

(defun org-air-classify--inbox-file-p (item)
  "Return non-nil when ITEM lives in `org-air-inbox-file'."
  (and (boundp 'org-air-inbox-file)
       org-air-inbox-file
       (org-air-item-file item)
       (equal (file-truename (expand-file-name (org-air-item-file item)))
              (file-truename (expand-file-name org-air-inbox-file)))))

(defun org-air-classify--last-activity (item)
  "Return the best available activity time for ITEM."
  (or (org-air-classify--time (org-air-item-closed item))
      (org-air-classify--time (org-air-item-scheduled item))
      (org-air-classify--time (org-air-item-deadline item))
      (org-air-classify--marker-timestamp-time item)
      (when-let* ((file (org-air-item-file item))
                  ((file-exists-p file)))
        (file-attribute-modification-time (file-attributes file)))))

;;;###autoload
(defun org-air-classify-item (item &optional now)
  "Return bucket symbols for ITEM relative to NOW.

Buckets are `upcoming', `stale', `attention', `high-priority', and `inbox'."
  (let* ((now (or now (current-time)))
         (buckets nil)
         (scheduled (org-air-item-scheduled item))
         (deadline (org-air-item-deadline item)))
    (unless (org-air-classify--done-p item)
      (when (or (org-air-classify--future-or-today-p scheduled now)
                (org-air-classify--future-or-today-p deadline now))
        (push 'upcoming buckets))
      (when (or (org-air-classify--past-p scheduled now)
                (org-air-classify--past-p deadline now)
                (and (null scheduled) (null deadline)))
        (push 'attention buckets))
      (when (and (org-air-item-priority item)
                 (>= (org-air-item-priority item)
                     (org-get-priority (format "[#%c]" org-priority-highest))))
        (push 'high-priority buckets))
      (when (or (org-air-classify--inbox-file-p item)
                (member "inbox" (mapcar #'downcase (org-air-item-tags item))))
        (push 'inbox buckets))
      (when-let* ((activity (org-air-classify--last-activity item)))
        (when (>= (org-air-classify--days-between activity now) org-air-stale-days)
          (push 'stale buckets))))
    (nreverse buckets)))

(provide 'org-air-classify)

;; Local Variables:
;; package-lint-main-file: "org-air.el"
;; End:
;;; org-air-classify.el ends here
