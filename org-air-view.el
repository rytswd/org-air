;;; org-air-view.el --- Dashboard renderer for org-air -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: org-air contributors
;; Keywords: outlines, calendar
;; Version: 0.1.0
;; URL: https://github.com/rytswd/org-air
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Minimal, nano-inspired interactive dashboard for org-air.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'org)
(require 'seq)
(require 'subr-x)
(require 'org-air-faces)
(require 'org-air-query)
(require 'org-air-classify)
(require 'org-air-calendar)
(require 'org-air-inbox)

(defvar org-air-files)
(defvar org-air-inbox-file)

(defcustom org-air-margin 2
  "Left margin for org-air content lines."
  :type 'integer
  :group 'org-air)

(defcustom org-air-item-indent 4
  "Column where item rows start."
  :type 'integer
  :group 'org-air)

(defcustom org-air-section-max 12
  "Maximum visible items per section before an overflow note."
  :type 'integer
  :group 'org-air)

(defcustom org-air-tags-inline-max 3
  "Maximum number of tag chips to render on an item line."
  :type 'integer
  :group 'org-air)

(defcustom org-air-show-footer t
  "Whether to show the footer key legend."
  :type 'boolean
  :group 'org-air)

(defcustom org-air-show-group nil
  "When non-nil, show item group instead of leaf filename as origin."
  :type 'boolean
  :group 'org-air)

(defcustom org-air-section-rule nil
  "When non-nil, draw a faint rule under section titles."
  :type 'boolean
  :group 'org-air)

(defcustom org-air-visit-display 'other-window
  "How `org-air-visit-item' displays an item's source.
Choices are `other-window', `same', and `frame'."
  :type '(choice (const other-window) (const same) (const frame))
  :group 'org-air)

(defcustom org-air-priority-show '(?A)
  "Priority cookies shown in item rows."
  :type '(repeat character)
  :group 'org-air)

(defcustom org-air-filter-match 'any
  "How multiple tag filters match items: `any' or `all'."
  :type '(choice (const any) (const all))
  :group 'org-air)

(defcustom org-air-glyphs
  '((origin . ("⌂" . "H"))
    (inbox . ("▤" . "I"))
    (attention . ("▲" . "!"))
    (upcoming . ("◆" . ">"))
    (high-priority . ("★" . "*"))
    (stale . ("◷" . "~"))
    (clear . ("✕" . "x"))
    (more . ("…" . "...")))
  "Glyph pairs used by org-air as (GUI . TTY) fallbacks."
  :type 'alist
  :group 'org-air)

(defvar-local org-air-view--items nil)
(defvar-local org-air-view--items-key nil)
(defvar-local org-air-view--tag-filter nil)
(defvar-local org-air-view--scope nil)
(defvar-local org-air-view--expanded-sections nil)

(defconst org-air-view-buffer-name "*org-air*")

(defconst org-air-view--sections
  '((inbox "Inbox" "Inbox zero — nothing to process.")
    (attention "Needs attention" "Nothing overdue. Nice.")
    (upcoming "Upcoming" org-air-view--empty-upcoming)
    (high-priority "High priority" "No #A items.")
    (stale "Stale" "Nothing has gone stale."))
  "Section descriptors in display order.")

(defvar org-air-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-air-visit-item)
    (define-key map (kbd "<mouse-1>") #'org-air-visit-item)
    (define-key map (kbd "n") #'org-air-next-item)
    (define-key map (kbd "p") #'org-air-prev-item)
    (define-key map (kbd "TAB") #'org-air-next-section)
    (define-key map (kbd "<backtab>") #'org-air-prev-section)
    (define-key map (kbd "M-n") #'org-air-forward-section)
    (define-key map (kbd "M-p") #'org-air-back-section)
    (define-key map (kbd "SPC") #'org-air-peek-item)
    (define-key map (kbd "o") #'org-air-visit-item-stay)
    (define-key map (kbd "c") #'org-air-capture)
    (define-key map (kbd "r") #'org-air-refile-item)
    (define-key map (kbd "m") #'org-air-toggle-mark)
    (define-key map (kbd "t") #'org-air-set-tag)
    (define-key map (kbd "d") #'org-air-set-schedule)
    (define-key map (kbd "/") #'org-air-filter)
    (define-key map (kbd "\\") #'org-air-filter-clear)
    (define-key map (kbd "s") #'org-air-scope)
    (define-key map (kbd "S") #'org-air-scope-clear)
    (define-key map (kbd "g") #'org-air-refresh)
    (define-key map (kbd "G") #'org-air-refresh-all)
    (define-key map (kbd "<") #'org-air-calendar-prev)
    (define-key map (kbd ">") #'org-air-calendar-next)
    (define-key map (kbd ".") #'org-air-calendar-today)
    (define-key map (kbd "?") #'org-air-help)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `org-air-view-mode'.")

(defalias 'org-air-mode-map 'org-air-view-mode-map)

(define-derived-mode org-air-view-mode special-mode "org-air"
  "Major mode for the org-air dashboard."
  (setq-local truncate-lines t)
  (setq-local cursor-type 'bar)
  (setq-local buffer-read-only t))

(defalias 'org-air-mode #'org-air-view-mode)

(defun org-air-view--glyph (name)
  "Return glyph NAME with a TTY fallback."
  (let ((pair (cdr (assq name org-air-glyphs))))
    (if (display-graphic-p) (car pair) (cdr pair))))

(defun org-air-view--margin ()
  "Return the standard left margin string."
  (make-string org-air-margin ?\s))

(defun org-air-view--item-margin ()
  "Return the item indentation string."
  (make-string org-air-item-indent ?\s))

(defun org-air-view--date-key (time)
  "Return YYYY-MM-DD key for TIME."
  (format-time-string "%F" time))

(defun org-air-view--days-between (then now)
  "Return calendar days between THEN and NOW."
  (- (time-to-days now) (time-to-days then)))

(defun org-air-view--timestamp-time (timestamp)
  "Return Emacs time for Org TIMESTAMP."
  (when timestamp (ignore-errors (org-timestamp-to-time timestamp))))

(defun org-air-view--human-date (time &optional now)
  "Return a compact human date label for TIME relative to NOW."
  (let* ((now (or now (current-time)))
         (delta (org-air-view--days-between now time)))
    (cond
     ((= delta 0) "Today")
     ((= delta 1) "Tomorrow")
     ((and (> delta 1) (< delta 7)) (format-time-string "%a" time))
     ((= (string-to-number (format-time-string "%Y" time))
         (string-to-number (format-time-string "%Y" now)))
      (format-time-string "%d %b" time))
     (t (format-time-string "%d %b %y" time)))))

(defun org-air-view--marker-timestamp-time (item)
  "Return first timestamp in ITEM subtree, if any."
  (when-let* ((marker (org-air-item-marker item))
              (buffer (marker-buffer marker)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char marker)
        (org-back-to-heading t)
        (let ((end (save-excursion (org-end-of-subtree t t))))
          (when (re-search-forward org-ts-regexp-both end t)
            (ignore-errors
              (org-timestamp-to-time
               (org-timestamp-from-string (match-string-no-properties 0))))))))))

(defun org-air-view--date-label (item bucket)
  "Return (LABEL . FACE) date metadata for ITEM in BUCKET."
  (let* ((now (current-time))
         (scheduled (org-air-view--timestamp-time (org-air-item-scheduled item)))
         (deadline (org-air-view--timestamp-time (org-air-item-deadline item))))
    (cond
     ((and deadline (> (org-air-view--days-between deadline now) 0))
      (cons (format "OVERDUE %dd" (abs (org-air-view--days-between deadline now)))
            'org-air-face-overdue))
     ((and scheduled (> (org-air-view--days-between scheduled now) 0))
      (cons (format "OVERDUE %dd" (abs (org-air-view--days-between scheduled now)))
            'org-air-face-overdue))
     (deadline (cons (org-air-view--human-date deadline now) 'org-air-face-deadline))
     (scheduled (cons (org-air-view--human-date scheduled now) 'org-air-face-scheduled))
     ((eq bucket 'attention) (cons "no date" 'org-air-face-date))
     ((eq bucket 'stale)
      (when-let* ((activity (or (org-air-view--marker-timestamp-time item)
                                (when-let* ((file (org-air-item-file item))
                                            ((file-exists-p file)))
                                  (file-attribute-modification-time
                                   (file-attributes file))))))
        (cons (format "· %dd quiet" (org-air-view--days-between activity now))
              'org-air-face-date))))))

(defun org-air-view--priority-char (item)
  "Return ITEM priority character, or nil."
  (when-let* ((priority (org-air-item-priority item)))
    (let ((char (- org-priority-lowest (/ priority 1000))))
      (and (characterp char) char))))

(defun org-air-view--origin (item)
  "Return origin breadcrumb for ITEM."
  (if org-air-show-group
      (or (org-air-item-group item) "")
    (file-name-nondirectory (org-air-item-file item))))

(defun org-air-view--filter-tags ()
  "Return active tag filters as a list."
  (cond
   ((null org-air-view--tag-filter) nil)
   ((listp org-air-view--tag-filter) org-air-view--tag-filter)
   ((stringp org-air-view--tag-filter) (list org-air-view--tag-filter))))

(defun org-air-view--passes-filter-p (item)
  "Return non-nil when ITEM passes active tag filters."
  (let ((filters (org-air-view--filter-tags))
        (tags (mapcar #'downcase (org-air-item-tags item))))
    (or (null filters)
        (let ((filters (mapcar #'downcase filters)))
          (if (eq org-air-filter-match 'all)
              (seq-every-p (lambda (tag) (member tag tags)) filters)
            (seq-some (lambda (tag) (member tag tags)) filters))))))

(defun org-air-view--passes-scope-p (item)
  "Return non-nil when ITEM passes the active scope."
  (pcase org-air-view--scope
    (`nil t)
    (`(:tag ,tag) (member tag (org-air-item-tags item)))
    (`(:group ,group) (equal group (org-air-item-group item)))
    (`(:file ,file) (equal (file-truename file)
                           (file-truename (org-air-item-file item))))
    (_ t)))

(defun org-air-view--visible-items (items)
  "Return ITEMS after scope and filter."
  (seq-filter (lambda (item)
                (and (org-air-view--passes-scope-p item)
                     (org-air-view--passes-filter-p item)))
              items))

(defun org-air-view--items-for-bucket (bucket items)
  "Return visible ITEMS classified into BUCKET."
  (seq-filter (lambda (item)
                (memq bucket (org-air-classify-item item)))
              (org-air-view--visible-items items)))

(defun org-air-view--right (string &optional face)
  "Return STRING propertized with FACE to right-align at line end."
  (concat (propertize " " 'display `(space :align-to (- right ,(length string))))
          (if face (propertize string 'face face) string)))

(defun org-air-view--insert-tag-chip (tag &optional active)
  "Insert TAG as a deterministic colour chip.
When ACTIVE is non-nil, use the active-filter tag face."
  (let ((start (point))
        (face (if active 'org-air-face-tag-active (org-air-faces-tag-face tag))))
    (insert-text-button (concat "#" tag)
                        'follow-link t
                        'action (lambda (_button) (org-air-filter-toggle tag))
                        'face face
                        'org-air-tag tag)
    (add-text-properties start (point) `(org-air-tag ,tag))))

(defun org-air-view--insert-tags (tags)
  "Insert up to `org-air-tags-inline-max' TAGS as chips."
  (let* ((shown (seq-take tags org-air-tags-inline-max))
         (overflow (- (length tags) (length shown)))
         (filters (org-air-view--filter-tags)))
    (dolist (tag shown)
      (insert " ")
      (org-air-view--insert-tag-chip tag (member tag filters)))
    (when (> overflow 0)
      (insert " " (propertize (format "+%d" overflow) 'face 'org-air-face-count)))))

(defun org-air-view--insert-banner (items)
  "Set the sticky header line for ITEMS."
  (let* ((filters (org-air-view--filter-tags))
         (filter-text (when filters
                        (concat " · "
                                (mapconcat (lambda (tag) (concat "#" tag)) filters " ")
                                " " (org-air-view--glyph 'clear))))
         (scope-text (pcase org-air-view--scope
                       (`(:tag ,tag) (concat " · #" tag))
                       (`(:group ,group) (concat " · @" group))
                       (`(:file ,file) (concat " · " (file-name-nondirectory file)))
                       (_ "")))
         (status (format "%s · %d items%s%s"
                         (format-time-string "%a %d %b")
                         (length (org-air-view--visible-items items))
                         scope-text
                         (or filter-text ""))))
    (setq header-line-format
          (list (propertize "  org-air" 'face 'org-air-face-header)
                (org-air-view--right status 'org-air-face-faded)))))

(defun org-air-view--insert-rule ()
  "Insert a faint full-width separator."
  (insert (propertize (make-string 74 ?─) 'face 'org-air-face-separator) "\n"))

(defun org-air-view--empty-upcoming ()
  "Return upcoming empty state."
  (format "Nothing scheduled in the next %d days." org-air-upcoming-days))

(defun org-air-view--empty-message (message)
  "Resolve MESSAGE as a string or function."
  (if (functionp message) (funcall message) message))

(defun org-air-view--insert-section-heading (bucket title count attentionp)
  "Insert section heading for BUCKET TITLE with COUNT.
ATTENTIONP means the count should use the attention badge face."
  (let ((start (point)))
    (insert (propertize (org-air-view--glyph bucket) 'face 'org-air-face-section-icon)
            " "
            (propertize title 'face 'org-air-face-section)
            "  "
            (propertize (format " %d " count)
                        'face (if attentionp
                                  'org-air-face-count-attention
                                'org-air-face-count))
            "\n")
    (add-text-properties start (point) `(org-air-section ,bucket org-air-count-badge ,count))
    (when org-air-section-rule
      (org-air-view--insert-rule))))

(defun org-air-view--insert-item (item bucket)
  "Insert ITEM as an interactive row in BUCKET."
  (let* ((start (point))
         (todo (org-air-item-todo item))
         (donep (and todo (org-air-classify--done-p item)))
         (priority (org-air-view--priority-char item))
         (date (org-air-view--date-label item bucket))
         (origin (concat (org-air-view--glyph 'origin) " " (org-air-view--origin item))))
    (insert (org-air-view--item-margin))
    (when todo
      (insert (propertize todo 'face (if donep 'org-air-face-done 'org-air-face-todo)) " "))
    (when (and priority (member priority org-air-priority-show))
      (insert (propertize (format "[#%c]" priority) 'face 'org-air-face-priority) " "))
    (insert-text-button (org-air-item-title item)
                        'follow-link t
                        'action (lambda (button)
                                  (org-air-visit-item (button-get button 'org-air-item)))
                        'org-air-item item
                        'face (if donep 'org-air-face-done 'org-air-face-title)
                        'help-echo (org-air-item-title item))
    (when date
      (insert "  " (propertize (car date) 'face (cdr date))))
    (org-air-view--insert-tags (org-air-item-tags item))
    (insert (org-air-view--right origin 'org-air-face-group))
    (insert "\n")
    (add-text-properties start (point)
                         `(org-air-item ,item
                           org-air-marker ,(org-air-item-marker item)
                           mouse-face org-air-face-cursor))))

(defun org-air-view--insert-section (descriptor items)
  "Insert section DESCRIPTOR from ITEMS."
  (pcase-let ((`(,bucket ,title ,empty) descriptor))
    (let* ((bucket-items (org-air-view--items-for-bucket bucket items))
           (count (length bucket-items))
           (attentionp (memq bucket '(inbox attention)))
           (expanded (memq bucket org-air-view--expanded-sections))
           (visible (if expanded bucket-items (seq-take bucket-items org-air-section-max))))
      (insert "\n")
      (org-air-view--insert-section-heading bucket title count attentionp)
      (if bucket-items
          (progn
            (dolist (item visible)
              (org-air-view--insert-item item bucket))
            (when (> count (length visible))
              (insert (org-air-view--item-margin)
                      (propertize (format "%s and %d more — press TAB on the title to expand\n"
                                          (org-air-view--glyph 'more)
                                          (- count (length visible)))
                                  'face 'org-air-face-faded))))
        (insert (org-air-view--item-margin)
                (propertize (org-air-view--empty-message empty)
                            'face 'org-air-face-empty)
                "\n")))))

(defun org-air-view--insert-footer ()
  "Insert footer hint line."
  (when org-air-show-footer
    (insert "\n" (org-air-view--margin)
            (propertize "[c]apture  [g]refresh  [/]filter  [\\]clear  [s]cope  [TAB]next  RET visit  [?]help"
                        'face 'org-air-face-faded)
            "\n")))

(defun org-air-view--render (items tag-filter)
  "Render dashboard for cached ITEMS with TAG-FILTER."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq org-air-view--items items
          org-air-view--items-key (list org-air-files org-air-inbox-file)
          org-air-view--tag-filter tag-filter)
    (org-air-view--insert-banner items)
    (insert "\n")
    (org-air-view--insert-rule)
    (if (null (org-air-view--visible-items items))
        (insert "\n" (org-air-view--item-margin)
                (propertize "Nothing here yet. Press c to capture your first note."
                            'face 'org-air-face-empty)
                "\n")
      (dolist (descriptor org-air-view--sections)
        (org-air-view--insert-section descriptor items))
      (insert "\n")
      (org-air-view--insert-rule)
      (insert "\n" (org-air-view--item-margin))
      (org-air-calendar-insert-month nil (org-air-view--visible-items items)))
    (org-air-view--insert-footer)
    (goto-char (point-min))))

(defun org-air-view--render-current ()
  "Re-render the dashboard from `org-air-view--items' without re-querying."
  (org-air-view--render (or org-air-view--items (org-air-query-items))
                        org-air-view--tag-filter))

;;;###autoload
(defun org-air-view ()
  "Open the org-air dashboard buffer."
  (interactive)
  (let ((buffer (get-buffer-create org-air-view-buffer-name)))
    (with-current-buffer buffer
      (org-air-view-mode)
      (unless (and org-air-view--items
                   (equal org-air-view--items-key (list org-air-files org-air-inbox-file)))
        (setq org-air-view--items (org-air-query-items)))
      (org-air-view--render org-air-view--items org-air-view--tag-filter))
    (pop-to-buffer buffer)))

(defun org-air-refresh ()
  "Re-query files and refresh the current org-air dashboard."
  (interactive)
  (let ((filter org-air-view--tag-filter))
    (setq org-air-view--items (org-air-query-items))
    (org-air-view--render org-air-view--items filter)))

(defun org-air-refresh-all ()
  "Clear scope and filters, then refresh."
  (interactive)
  (setq org-air-view--tag-filter nil
        org-air-view--scope nil)
  (org-air-refresh))

(defun org-air-filter (tags)
  "Filter dashboard to TAGS, a comma-separated or list value."
  (interactive
   (let* ((tags (delete-dups (sort (seq-mapcat #'org-air-item-tags org-air-view--items)
                                   #'string<)))
          (choice (completing-read-multiple "Tags: " tags nil nil)))
     (list choice)))
  (setq org-air-view--tag-filter (unless (null tags) tags))
  (org-air-view--render-current))

(defun org-air-filter-by-tag (tag)
  "Compatibility wrapper: filter dashboard to TAG."
  (interactive (list (read-string "Tag filter (empty clears): ")))
  (setq org-air-view--tag-filter (unless (string-empty-p tag) (list tag)))
  (org-air-view--render-current))

(defun org-air-filter-toggle (tag)
  "Toggle TAG in the active filter list."
  (interactive "sTag: ")
  (let ((filters (org-air-view--filter-tags)))
    (setq org-air-view--tag-filter
          (if (member tag filters)
              (delete tag filters)
            (cons tag filters))))
  (org-air-view--render-current))

(defun org-air-filter-clear ()
  "Clear tag filters."
  (interactive)
  (setq org-air-view--tag-filter nil)
  (org-air-view--render-current))

(defun org-air-scope (scope)
  "Scope dashboard to SCOPE."
  (interactive
   (let* ((tags (delete-dups (seq-mapcat #'org-air-item-tags org-air-view--items)))
          (groups (delete-dups (delq nil (mapcar #'org-air-item-group org-air-view--items))))
          (files (delete-dups (mapcar #'org-air-item-file org-air-view--items)))
          (candidates (append '("all")
                              (mapcar (lambda (g) (concat "@" g)) groups)
                              (mapcar (lambda (tag) (concat "#" tag)) tags)
                              (mapcar (lambda (file) (concat "⌂ " (file-name-nondirectory file))) files)))
          (choice (completing-read "Scope: " candidates nil t)))
     (list choice)))
  (setq org-air-view--scope
        (cond
         ((or (null scope) (equal scope "all")) nil)
         ((string-prefix-p "#" scope) (list :tag (substring scope 1)))
         ((string-prefix-p "@" scope) (list :group (substring scope 1)))
         ((string-prefix-p "⌂ " scope)
          (let ((name (substring scope 2)))
            (list :file (seq-find (lambda (file)
                                    (equal name (file-name-nondirectory file)))
                                  (mapcar #'org-air-item-file org-air-view--items)))))
         (t nil)))
  (org-air-view--render-current))

(defun org-air-scope-clear ()
  "Clear active dashboard scope."
  (interactive)
  (setq org-air-view--scope nil)
  (org-air-view--render-current))

(defun org-air-next-item ()
  "Move point to the next item row."
  (interactive)
  (let ((pos (next-single-property-change (point) 'org-air-item nil (point-max))))
    (while (and pos (not (get-text-property pos 'org-air-item)) (< pos (point-max)))
      (setq pos (next-single-property-change pos 'org-air-item nil (point-max))))
    (when pos (goto-char pos))))

(defun org-air-prev-item ()
  "Move point to the previous item row."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'org-air-item nil (point-min))))
    (while (and pos (not (get-text-property (max (point-min) (1- pos)) 'org-air-item)) (> pos (point-min)))
      (setq pos (previous-single-property-change pos 'org-air-item nil (point-min))))
    (when pos (goto-char (max (point-min) (1- pos))))))

(defun org-air-next-section ()
  "Move point to the next section heading."
  (interactive)
  (let ((pos (next-single-property-change (point) 'org-air-section nil (point-max))))
    (when pos (goto-char pos))))

(defun org-air-prev-section ()
  "Move point to the previous section heading."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'org-air-section nil (point-min))))
    (when pos (goto-char (max (point-min) (1- pos))))))

(defalias 'org-air-forward-section #'org-air-next-section)
(defalias 'org-air-back-section #'org-air-prev-section)

(defun org-air-toggle-mark ()
  "Toggle a visual mark on the item at point."
  (interactive)
  (let ((inhibit-read-only t)
        (marked (get-text-property (point) 'org-air-marked)))
    (add-text-properties (line-beginning-position) (line-end-position)
                         `(org-air-marked ,(not marked)))
    (save-excursion
      (beginning-of-line)
      (delete-char 1)
      (insert (if marked " " "•")))))

(defun org-air-set-tag ()
  "Add TAG to the item at point."
  (interactive)
  (let* ((item (or (get-text-property (point) 'org-air-item)
                   (user-error "No org-air item at point")))
         (tag (read-string "Tag: ")))
    (with-current-buffer (find-file-noselect (org-air-item-file item))
      (goto-char (org-air-item-marker item))
      (org-back-to-heading t)
      (org-toggle-tag tag 'on)
      (save-buffer)))
  (org-air-refresh))

(defun org-air-set-schedule (date)
  "Set SCHEDULED DATE on the item at point."
  (interactive "sSchedule (empty clears): ")
  (let ((item (or (get-text-property (point) 'org-air-item)
                  (user-error "No org-air item at point"))))
    (with-current-buffer (find-file-noselect (org-air-item-file item))
      (goto-char (org-air-item-marker item))
      (org-back-to-heading t)
      (org-schedule nil (unless (string-empty-p date) date))
      (save-buffer)))
  (org-air-refresh))

(defun org-air-calendar-prev ()
  "Placeholder command for previous calendar month."
  (interactive)
  (message "org-air: calendar month paging is not persistent in this view yet"))

(defun org-air-calendar-next ()
  "Placeholder command for next calendar month."
  (interactive)
  (message "org-air: calendar month paging is not persistent in this view yet"))

(defun org-air-calendar-today ()
  "Refresh the calendar around today."
  (interactive)
  (org-air-refresh))

(defun org-air-peek-item ()
  "Preview the source item in another window while keeping dashboard focus."
  (interactive)
  (let ((dashboard (selected-window)))
    (org-air-visit-item nil 'other-window)
    (select-window dashboard)))

(defun org-air-visit-item-stay ()
  "Visit the source item without moving focus away from dashboard."
  (interactive)
  (org-air-peek-item))

(defun org-air-help ()
  "Show org-air key bindings."
  (interactive)
  (message "org-air: n/p items, TAB sections, RET visit, c capture, r refile, / filter, \\ clear, s scope, g refresh, q quit"))

;;;###autoload
(defun org-air-visit-item (&optional item display)
  "Visit ITEM's original Org heading.
When ITEM is nil, use the item at point in an org-air dashboard.  DISPLAY
controls window choice and defaults to `org-air-visit-display'."
  (interactive)
  (let ((item (or item (get-text-property (point) 'org-air-item))))
    (unless item
      (user-error "No org-air item at point"))
    (let* ((marker (org-air-item-marker item))
           (buffer (or (marker-buffer marker)
                       (find-file-noselect (org-air-item-file item))))
           (display (or display org-air-visit-display)))
      (pcase display
        ('other-window (switch-to-buffer-other-window buffer))
        ('frame (switch-to-buffer-other-frame buffer))
        (_ (switch-to-buffer buffer)))
      (goto-char marker)
      (funcall (if (fboundp 'org-fold-show-context)
                   #'org-fold-show-context
                 (intern "org-show-context")))
      (recenter)
      (when (fboundp 'pulse-momentary-highlight-one-line)
        (pulse-momentary-highlight-one-line (point))))))

(provide 'org-air-view)

;; Local Variables:
;; package-lint-main-file: "org-air.el"
;; End:
;;; org-air-view.el ends here
