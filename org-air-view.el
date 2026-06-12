;;; org-air-view.el --- Dashboard renderer for org-air -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: org-air contributors
;; Keywords: outlines, calendar
;; Package-Requires: ((emacs "29.1") (org "9.6") (org-ql "0.8"))

;;; Commentary:

;; Minimal interactive dashboard for org-air.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org-air-query)
(require 'org-air-classify)
(require 'org-air-calendar)

(defface org-air-face-default '((t :inherit default))
  "Fallback default face for org-air."
  :group 'org-air)
(defface org-air-face-faded '((t :inherit shadow))
  "Fallback faded face for org-air."
  :group 'org-air)
(defface org-air-face-salient '((t :inherit font-lock-keyword-face :weight bold))
  "Fallback salient face for org-air."
  :group 'org-air)
(defface org-air-face-popout '((t :inherit highlight :weight bold))
  "Fallback popout face for org-air."
  :group 'org-air)
(defface org-air-face-critical '((t :inherit error :weight bold))
  "Fallback critical face for org-air."
  :group 'org-air)
(defface org-air-face-subtle '((t :inherit shadow :height 0.9))
  "Fallback subtle face for org-air."
  :group 'org-air)

(defvar-local org-air-view--items nil)
(defvar-local org-air-view--tag-filter nil)

(defconst org-air-view-buffer-name "*org-air*")

(defvar org-air-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-air-visit-item)
    (define-key map (kbd "g") #'org-air-refresh)
    (define-key map (kbd "/") #'org-air-filter-by-tag)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `org-air-view-mode'.")

(define-derived-mode org-air-view-mode special-mode "org-air"
  "Major mode for the org-air dashboard.")

(defun org-air-view--timestamp-label (timestamp)
  "Return a compact display string for TIMESTAMP."
  (when timestamp
    (ignore-errors (org-format-timestamp timestamp "%Y-%m-%d"))))

(defun org-air-view--item-date (item)
  "Return a compact date label for ITEM."
  (string-join
   (delq nil (list (when-let* ((scheduled (org-air-view--timestamp-label (org-air-item-scheduled item))))
                     (concat "S:" scheduled))
                   (when-let* ((deadline (org-air-view--timestamp-label (org-air-item-deadline item))))
                     (concat "D:" deadline))))
   " "))

(defun org-air-view--passes-filter-p (item)
  "Return non-nil when ITEM passes `org-air-view--tag-filter'."
  (or (null org-air-view--tag-filter)
      (member org-air-view--tag-filter (org-air-item-tags item))))

(defun org-air-view--items-for-bucket (bucket items)
  "Return ITEMS classified into BUCKET and passing filters."
  (seq-filter (lambda (item)
                (and (org-air-view--passes-filter-p item)
                     (memq bucket (org-air-classify-item item))))
              items))

(defun org-air-view--insert-heading (title &optional face)
  "Insert section TITLE with FACE."
  (insert "\n" (propertize title 'face (or face 'org-air-face-salient)) "\n")
  (insert (propertize (make-string (length title) ?─) 'face 'org-air-face-subtle) "\n"))

(defun org-air-view--insert-item (item)
  "Insert ITEM as an interactive row."
  (let ((start (point))
        (todo (org-air-item-todo item))
        (date (org-air-view--item-date item))
        (tags (org-air-item-tags item)))
    (insert-text-button (format "  %s" (org-air-item-title item))
                        'follow-link t
                        'action (lambda (button)
                                  (org-air-visit-item (button-get button 'org-air-item)))
                        'org-air-item item
                        'face 'org-air-face-default)
    (when todo
      (insert " " (propertize todo 'face 'org-air-face-popout)))
    (unless (string-empty-p date)
      (insert " " (propertize date 'face 'org-air-face-subtle)))
    (when tags
      (insert " " (propertize (concat ":" (string-join tags ":") ":")
                              'face 'org-air-face-faded)))
    (insert "\n")
    (add-text-properties start (point) `(org-air-item ,item))))

(defun org-air-view--insert-section (bucket title items &optional face)
  "Insert BUCKET section titled TITLE from ITEMS."
  (let ((bucket-items (org-air-view--items-for-bucket bucket items)))
    (org-air-view--insert-heading (format "%s (%d)" title (length bucket-items)) face)
    (if bucket-items
        (dolist (item bucket-items)
          (org-air-view--insert-item item))
      (insert (propertize "  Nothing here.\n" 'face 'org-air-face-faded)))))

(defun org-air-view--render (items tag-filter)
  "Render dashboard for ITEMS with TAG-FILTER."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq org-air-view--items items
          org-air-view--tag-filter tag-filter)
    (insert (propertize "org-air" 'face 'org-air-face-popout) "\n")
    (insert (propertize (format "%d items" (length items)) 'face 'org-air-face-subtle))
    (when tag-filter
      (insert (propertize (format "  filter: :%s:" tag-filter) 'face 'org-air-face-salient)))
    (insert "\n\n")
    (org-air-calendar-insert-month nil items)
    (org-air-view--insert-section 'inbox "Inbox" items 'org-air-face-popout)
    (org-air-view--insert-section 'high-priority "High priority" items 'org-air-face-critical)
    (org-air-view--insert-section 'attention "Needs attention" items 'org-air-face-critical)
    (org-air-view--insert-section 'upcoming "Upcoming" items 'org-air-face-salient)
    (org-air-view--insert-section 'stale "Stale" items 'org-air-face-faded)
    (goto-char (point-min))))

;;;###autoload
(defun org-air-view ()
  "Open the org-air dashboard buffer."
  (interactive)
  (let ((buffer (get-buffer-create org-air-view-buffer-name)))
    (with-current-buffer buffer
      (org-air-view-mode)
      (org-air-view--render (org-air-query-items) org-air-view--tag-filter))
    (pop-to-buffer buffer)))

(defun org-air-refresh ()
  "Refresh the current org-air dashboard."
  (interactive)
  (let ((filter org-air-view--tag-filter))
    (org-air-view--render (org-air-query-items) filter)))

(defun org-air-filter-by-tag (tag)
  "Filter dashboard to TAG.  Empty TAG clears the filter."
  (interactive (list (read-string "Tag filter (empty clears): " org-air-view--tag-filter)))
  (setq org-air-view--tag-filter (unless (string-empty-p tag) tag))
  (org-air-refresh))

;;;###autoload
(defun org-air-visit-item (&optional item)
  "Visit ITEM's original Org heading.

When ITEM is nil, use the item at point in an org-air dashboard."
  (interactive)
  (let ((item (or item (get-text-property (point) 'org-air-item))))
    (unless item
      (user-error "No org-air item at point"))
    (let* ((marker (org-air-item-marker item))
           (buffer (or (marker-buffer marker)
                       (find-file-noselect (org-air-item-file item)))))
      (switch-to-buffer buffer)
      (goto-char marker)
      (funcall (if (fboundp 'org-fold-show-context)
                   #'org-fold-show-context
                 (intern "org-show-context"))))))

(provide 'org-air-view)
;;; org-air-view.el ends here
