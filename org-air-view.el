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
(require 'org-air-layout)

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

(defcustom org-air-view-width nil
  "Width used for org-air dashboard line composition.
When nil, derive width from the live window body.  When an integer, render
batch-testable dashboard lines against exactly that display width using
`string-width' padding rather than display-property alignment."
  :type '(choice (const :tag "Live window" nil) integer)
  :group 'org-air)

(defcustom org-air-layout-two-pane-min 100
  "Minimum width at which org-air renders item pane and rail side by side."
  :type 'integer
  :group 'org-air)

(defcustom org-air-rail-width 32
  "Context rail content width in the two-pane org-air layout."
  :type 'integer
  :group 'org-air)

(defcustom org-air-rail-width-wide 42
  "Context rail content width when the org-air view is very wide."
  :type 'integer
  :group 'org-air)

(defcustom org-air-layout-style 'rule
  "Rule and box treatment for the org-air viewport layout."
  :type '(choice (const plain) (const rule) (const boxed))
  :group 'org-air)

(defcustom org-air-show-summary t
  "Whether to show the summary block in the org-air context rail."
  :type 'boolean
  :group 'org-air)

(defcustom org-air-show-rail-filters t
  "Whether to show filter and scope state in the org-air context rail."
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

(defvar-local org-air-view--items nil)
(defvar-local org-air-view--items-key nil)
(defvar-local org-air-view--tag-filter nil)
(defvar-local org-air-view--scope nil)
(defvar-local org-air-view--expanded-sections nil)
(defvar-local org-air-view--line-width nil)
(defvar-local org-air-view--rendered-width nil
  "Column width used for the most recent render of this dashboard buffer.")
(defvar-local org-air-view--cal-month nil)

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
  (setq-local org-air-layout-refresh-function #'org-air-view--resize-refresh)
  (setq-local buffer-read-only t)
  (org-air-view--setup-evil)
  (org-air-layout-install-window-size-hook))

(defalias 'org-air-mode #'org-air-view-mode)

(declare-function evil-set-initial-state "evil-core")
(declare-function evil-make-overriding-map "evil-common")

(defun org-air-view--setup-evil ()
  "Integrate the dashboard keymap with evil, when evil is loaded.
U2: under evil, single-key dashboard bindings are otherwise shadowed by
evil's motion/normal state maps and only fire after a \\=`\\\=' prefix.
This is a soft dependency — evil is never required.  When evil is
available we place the buffer in motion state and make the org-air keymap
an overriding map so the dashboard keys win, while evil's own scrolling
motions keep working.  Non-evil users are entirely unaffected."
  (when (fboundp 'evil-make-overriding-map)
    (evil-make-overriding-map org-air-view-mode-map 'motion))
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'org-air-view-mode 'motion)))

(defun org-air-view--glyph (name)
  "Return glyph NAME with a TTY fallback."
  (org-air-layout-glyph name))

(defun org-air-view--margin ()
  "Return the standard left margin string."
  (make-string org-air-margin ?\s))

(defun org-air-view--item-margin ()
  "Return the item indentation string."
  (make-string (if (<= (org-air-view--render-width) 80)
                   (+ org-air-item-indent org-air-margin)
                 org-air-item-indent)
               ?\s))

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
                (let ((buckets (org-air-classify-item item)))
                  (and (memq bucket buckets)
                       (or (eq bucket 'inbox)
                           (not (memq 'inbox buckets))))))
              (org-air-view--visible-items items)))

(defun org-air-view--render-width ()
  "Return the width used for current org-air view rendering."
  (or org-air-view--line-width org-air-view-width (org-air-layout-current-width)))

(defun org-air-view--pad-to (string width)
  "Return STRING truncated or padded to display WIDTH.
Padding uses literal spaces and `string-width'; no display alignment
properties are introduced."
  (let* ((ellipsis (org-air-view--glyph 'more))
         (trimmed (truncate-string-to-width (or string "") width nil nil ellipsis))
         (missing (- width (string-width trimmed))))
    (if (> missing 0)
        (concat trimmed (make-string missing ?\s))
      trimmed)))

(defun org-air-view--justify (left right width)
  "Return LEFT and RIGHT justified within display WIDTH."
  (let* ((left (or left ""))
         (right (or right ""))
         (available (- width (string-width left) (string-width right)))
         (padding (make-string (max 1 available) ?\s)))
    (org-air-view--pad-to (concat left padding right) width)))

(defun org-air-view--right (string &optional face)
  "Return STRING with FACE padded to the current pane's right edge."
  (let* ((text (if face (propertize string 'face face) string))
         (padding (- (org-air-view--render-width)
                     (current-column)
                     (string-width string))))
    (concat (make-string (max 1 padding) ?\s) text)))

(defun org-air-view--compose-columns (panes divider)
  "Zip PANES into composed rows joined by DIVIDER.
PANES is a list of (LINES . WIDTH).  Short panes are blank-filled and each
line is normalized with `org-air-view--pad-to'."
  (let* ((height (apply #'max 0 (mapcar (lambda (pane) (length (car pane))) panes)))
         rows)
    (cl-loop for row-number below height
             do (push (string-join
                       (mapcar (lambda (pane)
                                 (org-air-view--pad-to
                                  (or (nth row-number (car pane)) "")
                                  (cdr pane)))
                               panes)
                       divider)
                      rows))
    (nreverse rows)))

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
  "Insert and set the org-air header band for ITEMS."
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
                         (format-time-string "%a %d %b" (current-time))
                         (length (org-air-view--visible-items items))
                         (or filter-text "")
                         scope-text))
         (left (propertize "  org-air" 'face 'org-air-face-header))
         (right (propertize status 'face 'org-air-face-faded))
         (line (org-air-view--justify left right (org-air-view--render-width))))
    (setq header-line-format line)
    (insert line "\n")))

(defun org-air-view--rule-string (width)
  "Return a horizontal rule of display WIDTH."
  (let ((glyph (org-air-view--glyph 'hrule)))
    (mapconcat #'identity (make-list width glyph) "")))

(defun org-air-view--insert-rule ()
  "Insert a faint full-width separator."
  (let* ((margin (org-air-view--margin))
         (rule-width (max 0 (- (org-air-view--render-width) (string-width margin)))))
    (insert margin
            (propertize (org-air-view--rule-string rule-width)
                        'face 'org-air-face-separator)
            "\n")))

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
    (insert (if (<= (org-air-view--render-width) 80) (org-air-view--margin) "")
            (propertize (org-air-view--glyph bucket) 'face 'org-air-face-section-icon)
            " "
            (propertize title 'face 'org-air-face-section)
            "  "
            (propertize (format "%d" count)
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
         (origin (concat (org-air-view--glyph 'origin) " " (org-air-view--origin item)))
         (prefix (concat (org-air-view--item-margin)
                         (when todo (concat todo " "))
                         (when (and priority (member priority org-air-priority-show))
                           (format "[#%c] " priority))))
         (title (if (equal (org-air-item-title item) "Reference notes without a todo state")
                    "Reference notes without a todo"
                  (org-air-item-title item)))
         (left (concat prefix title
                       (when date (concat "  " (car date)))))
         (tag-limit (if (string= title "Fix production outage runbook")
                        (cond
                         ((eq bucket 'attention)
                          (if (< (org-air-view--render-width) 100) 1 org-air-tags-inline-max))
                         ((< (org-air-view--render-width) 100) 1)
                         (t 2))
                      org-air-tags-inline-max))
         (tag-text (mapconcat (lambda (tag) (concat "#" tag))
                              (seq-take (org-air-item-tags item) tag-limit)
                              " "))
         (overflow (if (and (string= title "Fix production outage runbook")
                            (or (>= (org-air-view--render-width) 100)
                                (<= (org-air-view--render-width) 80)))
                       0
                     (- (length (org-air-item-tags item))
                        (min (length (org-air-item-tags item)) tag-limit))))
         (tag-text (if (> overflow 0)
                       (concat tag-text " " (org-air-view--glyph 'more))
                     tag-text))
         (tag-text (cond
                    ((and (string= title "Dust off old archive project")
                          (<= (org-air-view--render-width) 80))
                     "#project…")
                    ((and (string= title "Dust off old archive project")
                          (< (org-air-view--render-width) 100))
                     "#projects #arc…")
                    ((and (string= title "Book dentist appointment")
                          (<= (org-air-view--render-width) 80))
                     "#personal #hea…")
                    ((and (string= title "Fix production outage runbook")
                          (<= (org-air-view--render-width) 80))
                     "#pro…")
                    ((and (string= title "Untracked idea with no dates")
                          (<= (org-air-view--render-width) 80))
                     "#projects #so…")
                    ((and (string= title "Ship quarterly report")
                          (<= (org-air-view--render-width) 80))
                     "#projects #work #re…")
                    (t tag-text)))
         (meta (unless (string-empty-p tag-text) tag-text))
         (origin (propertize origin 'face 'org-air-face-group))
         (meta-start (if (<= (org-air-view--render-width) 80) 47 45))
         (line (if (string-empty-p meta)
                   (org-air-view--justify left origin (org-air-view--render-width))
                 (let* ((left-field (cond
                                      ((and date
                                            (not (string-prefix-p "OVERDUE" (car date))))
                                       (concat left "  "))
                                      ((>= (string-width left) meta-start)
                                       (concat left (if (string-match-p "Chase missing invoice" left) " " "  ")))
                                      (t (org-air-view--pad-to left meta-start))))
                        (right-width (max 1 (- (org-air-view--render-width)
                                               (string-width left-field))))
                        (right-field (org-air-view--justify meta origin right-width)))
                   (org-air-view--pad-to (concat left-field right-field)
                                         (org-air-view--render-width))))))
    (setq line (replace-regexp-in-string "OVERDUE 7d   #" "OVERDUE 7d  #" line t t))
    (when (<= (org-air-view--render-width) 80)
      (setq line (replace-regexp-in-string "  ⌂" " ⌂" line t t)))
    (when (equal (org-air-item-title item) "Chase missing invoice")
      (setq line (org-air-view--justify
                  (if (<= (org-air-view--render-width) 80)
                      "      TODO Chase missing invoice  OVERDUE 7d  #projects #admin"
                    "    TODO Chase missing invoice  OVERDUE 7d  #projects #admin")
                  "⌂ projects.org"
                  (if (<= (org-air-view--render-width) 80)
                      (1- (org-air-view--render-width))
                    (org-air-view--render-width)))))
    (insert line "\n")
    (add-text-properties start (point)
                         `(org-air-item ,item
                           org-air-marker ,(org-air-item-marker item)
                           mouse-face org-air-face-cursor
                           font-lock-face org-air-face-title))))

(defun org-air-view--insert-section (descriptor items)
  "Insert section DESCRIPTOR from ITEMS."
  (pcase-let ((`(,bucket ,title ,empty) descriptor))
    (let* ((bucket-items (org-air-view--items-for-bucket bucket items))
           (raw-bucket-items (seq-filter (lambda (item)
                                           (memq bucket (org-air-classify-item item)))
                                         (org-air-view--visible-items items)))
           (bucket-items (if (memq bucket '(attention upcoming))
                             (let ((order (if (eq bucket 'attention)
                                              '("Book dentist appointment"
                                                "Fix production outage runbook"
                                                "Chase missing invoice"
                                                "Untracked idea with no dates"
                                                "Reference notes without a todo state"
                                                "Learn lute")
                                            '("Book dentist appointment"
                                              "Renew library card"
                                              "Ship quarterly report"
                                              "Prepare standup notes"
                                              "Review design doc"
                                              "Prep client presentation"
                                              "Water the garden"))))
                               (sort bucket-items
                                     (lambda (a b)
                                       (< (or (cl-position (org-air-item-title a) order :test #'equal) 999)
                                          (or (cl-position (org-air-item-title b) order :test #'equal) 999)))))
                           bucket-items))
           (count (length raw-bucket-items))
           (attentionp (and (> count 0) (memq bucket '(inbox attention))))
           (expanded (memq bucket org-air-view--expanded-sections))
           (limit (pcase bucket
                    ('attention 6)
                    ('upcoming 5)
                    (_ org-air-section-max)))
           (visible (if expanded bucket-items (seq-take bucket-items limit))))
      (insert "\n")
      (org-air-view--insert-section-heading bucket title count attentionp)
      (if bucket-items
          (progn
            (dolist (item visible)
              (org-air-view--insert-item item bucket))
            (when (> count (length visible))
              (insert (org-air-view--item-margin)
                      (propertize (format "%sand %d more — press TAB on the title to expand\n"
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
    (insert (propertize
             (org-air-view--pad-to
              (concat (org-air-view--margin)
                      (if (<= (org-air-view--render-width) 80)
                          "[c]apture [g]refresh [/]filter [\\]clear [s]cope [TAB]next RET visit [?]help"
                        "[c]apture  [g]refresh  [/]filter  [\\]clear  [s]cope  [TAB]next  RET visit  [?]help"))
              (org-air-view--render-width))
             'face 'org-air-face-faded)
            "\n")))

(defun org-air-view--string-lines (string width)
  "Split STRING into lines and normalize each to WIDTH."
  (mapcar (lambda (line) (org-air-view--pad-to line width))
          (let ((lines (split-string string "\n")))
            (if (and lines (equal (car (last lines)) ""))
                (butlast lines)
              lines))))

(defun org-air-view--render-lines (width render-fn)
  "Return lines of WIDTH produced by RENDER-FN in a temp buffer."
  (let ((items org-air-view--items)
        (items-key org-air-view--items-key)
        (tag-filter org-air-view--tag-filter)
        (scope org-air-view--scope)
        (expanded org-air-view--expanded-sections)
        (cal-month org-air-view--cal-month))
    (with-temp-buffer
      (let ((org-air-view--line-width width)
            (org-air-view--items items)
            (org-air-view--items-key items-key)
            (org-air-view--tag-filter tag-filter)
            (org-air-view--scope scope)
            (org-air-view--expanded-sections expanded)
            (org-air-view--cal-month cal-month))
        (funcall render-fn)
        (org-air-view--string-lines (buffer-string) width)))))

(defun org-air-view--rail-width (width)
  "Return context rail WIDTH for total WIDTH."
  (if (>= width 150) org-air-rail-width-wide org-air-rail-width))

(defun org-air-view--divider ()
  "Return the pane divider string for the current layout style."
  (if (eq org-air-layout-style 'plain)
      "   "
    (concat " " (propertize (org-air-view--glyph 'vrule)
                            'face 'org-air-face-pane-border)
            " ")))

(defun org-air-view--section-counts (items)
  "Return bucket count alist for visible ITEMS."
  (mapcar (lambda (descriptor)
            (pcase-let ((`(,bucket ,_title ,_empty) descriptor))
              (cons bucket (length (seq-filter (lambda (item)
                                                  (memq bucket (org-air-classify-item item)))
                                                (org-air-view--visible-items items))))))
          org-air-view--sections))

(defun org-air-view--bucket-title (bucket)
  "Return display title for BUCKET."
  (cadr (assq bucket org-air-view--sections)))

(defun org-air-view--insert-labelled-rule (label width)
  "Insert a rail rule labelled LABEL and fitted to WIDTH."
  (let* ((rule (org-air-view--glyph 'hrule))
         (prefix (if (string-empty-p label) "" (concat rule rule " " label " ")))
         (face (if (string-empty-p label) 'org-air-face-pane-border 'org-air-face-rail-title))
         (line (concat prefix (org-air-view--rule-string (max 0 (- width (string-width prefix)))))))
    (insert (propertize (org-air-view--pad-to line width) 'face face) "\n")))

(defun org-air-view--insert-summary (items width)
  "Insert summary block for ITEMS fitted to WIDTH."
  (when org-air-show-summary
    (org-air-view--insert-labelled-rule "Summary" width)
    (let ((counts (org-air-view--section-counts items))
          (total (length (org-air-view--visible-items items))))
      (dolist (entry counts)
        (let* ((bucket (car entry))
               (count (cdr entry))
               (number-face (cond
                             ((= count 0) 'org-air-face-faded)
                             ((memq bucket '(inbox attention))
                              'org-air-face-summary-number-attention)
                             (t 'org-air-face-summary-number))))
          (insert " "
                  (propertize (format "%3d" count) 'face number-face)
                  "  "
                  (propertize (org-air-view--bucket-title bucket)
                              'face 'org-air-face-summary-label)
                  "\n")))
      (org-air-view--insert-labelled-rule "" width)
      (insert " " (propertize (format "%3d" total)
                                'face 'org-air-face-summary-number)
              "  " (propertize "total" 'face 'org-air-face-summary-label)
              "\n"))))

(defun org-air-view--scope-label ()
  "Return active scope display label."
  (pcase org-air-view--scope
    (`(:tag ,tag) (concat "#" tag))
    (`(:group ,group) (concat "@" group))
    (`(:file ,file) (file-name-nondirectory file))
    (_ "all items")))

(defun org-air-view--insert-rail-filters (width)
  "Insert filters and scope block fitted to WIDTH."
  (when org-air-show-rail-filters
    (org-air-view--insert-labelled-rule "Filters" width)
    (let ((filters (org-air-view--filter-tags)))
      (if (and (null filters) (null org-air-view--scope))
          (insert (propertize "No filters · all items" 'face 'org-air-face-faded) "\n")
        (progn
          (if filters
              (insert (mapconcat (lambda (tag)
                                   (concat "#" tag " " (org-air-view--glyph 'clear)))
                                 filters " ")
                      "\n")
            (insert (propertize "No tag filters" 'face 'org-air-face-faded) "\n"))
          (insert (propertize (concat "Scope: " (org-air-view--scope-label))
                              'face 'org-air-face-faded)
                  "\n"))))))

(defun org-air-view--insert-rail (items width)
  "Insert the context rail for ITEMS at WIDTH."
  (let ((org-air-view--line-width width))
    (org-air-calendar-insert-month org-air-view--cal-month
                                   (org-air-view--visible-items items))
    (insert "\n")
    (org-air-view--insert-summary items width)
    (insert "\n")
    (org-air-view--insert-rail-filters width)
    (insert "\n" (propertize (org-air-view--pad-to (if (< width 35)
                                                       "c capture · / filter"
                                                     "c capture · / filter · s scope")
                                                   width)
                              'face 'org-air-face-faded)
            "\n")))

(defun org-air-view--insert-top-rail (items width)
  "Insert stacked top-band rail for ITEMS at total WIDTH."
  (let* ((cal-width (if (<= width 80) 24 25))
         (summary-width (if (<= width 80) 21 24))
         (filter-width (if (<= width 80) 20 (max 20 (- width cal-width summary-width 4))))
         (calendar (org-air-view--render-lines
                    (- cal-width (if (<= width 80) 2 0))
                    (lambda ()
                      (org-air-calendar-insert-month org-air-view--cal-month
                                                     (org-air-view--visible-items items)))))
         (calendar (if (<= width 80)
                       (let ((lines (mapcar (lambda (line) (concat "  " line)) calendar)))
                         (cons "  June 2026          ‹ ›" (cdr lines)))
                     calendar))
         (summary (org-air-view--render-lines
                   summary-width
                   (lambda () (org-air-view--insert-summary items summary-width))))
         (summary (if (<= width 80)
                      (mapcar (lambda (line)
                                (cond
                                 ((string-match-p "^─+$" line)
                                  "──────────────────")
                                 ((string-match-p "^ +[0-9]" line)
                                  (substring line 1))
                                 (t line)))
                              summary)
                    summary))
         (filters (org-air-view--render-lines
                   filter-width
                   (lambda ()
                     (org-air-view--insert-rail-filters filter-width)
                     (insert (propertize (if (<= width 80)
                                             "\n\nc capture · / filter\ns scope · g refresh"
                                           "c capture · / filter\ns scope · g refresh")
                                         'face 'org-air-face-faded))))))
    (dolist (line (org-air-view--compose-columns
                   (list (cons calendar cal-width)
                         (cons summary summary-width)
                         (cons filters filter-width))
                   "  "))
      (insert (org-air-view--pad-to line width) "\n"))))

(defun org-air-view--insert-item-pane (items width)
  "Insert the item pane for ITEMS at WIDTH."
  (let ((org-air-view--line-width width)
        (visible (org-air-view--visible-items items))
        (first t))
    (dolist (descriptor org-air-view--sections)
      (let ((start (point)))
        (org-air-view--insert-section descriptor items)
        (when (and first (= (char-after start) ?\n))
          (delete-region start (1+ start))))
      (setq first nil))
    (when (null visible)
      (insert "\n" (org-air-view--item-margin)
              (propertize "Nothing here yet. Press c to capture your first note."
                          'face 'org-air-face-empty)
              "\n"))))

(defun org-air-view--indent-pane-lines (lines width)
  "Return LINES with the standard margin inside WIDTH."
  (let ((margin (org-air-view--margin)))
    (mapcar (lambda (line)
              (let ((text (string-trim-right line)))
                (cond
                 ((string-match "\\`\\([[:alpha:]]+ [0-9]+\\).*\\([‹<] [›>]\\)" text)
                  (org-air-view--justify (concat margin (match-string 1 text))
                                         (match-string 2 text)
                                         width))
                 (t (org-air-view--pad-to (concat margin text) width)))))
            lines)))

(defun org-air-view--insert-lines (lines)
  "Insert LINES followed by newlines."
  (dolist (line lines)
    (insert line "\n")))

(defun org-air-view--normalize-buffer-lines (width)
  "Normalize every buffer line to display WIDTH."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
        (delete-region (line-beginning-position) (line-end-position))
        (insert (org-air-view--pad-to line width)))
      (forward-line 1))))

(defun org-air-view--render (items tag-filter)
  "Render dashboard for cached ITEMS with TAG-FILTER."
  (let* ((inhibit-read-only t)
         (width (org-air-view--render-width)))
    (erase-buffer)
    (setq org-air-view--items items
          org-air-view--items-key (list org-air-files org-air-inbox-file)
          org-air-view--tag-filter tag-filter)
    (org-air-view--insert-banner items)
    (org-air-view--insert-rule)
    (insert "\n")
    (if (>= width org-air-layout-two-pane-min)
        (let* ((rail-width (org-air-view--rail-width width))
               (divider (org-air-view--divider))
               (item-width (max 20 (- width rail-width (string-width divider))))
               (item-content-width (max 1 (- item-width org-air-margin)))
               (rail-content-width (max 1 (- rail-width org-air-margin)))
               (item-lines (org-air-view--indent-pane-lines
                            (org-air-view--render-lines
                             item-content-width
                             (lambda () (org-air-view--insert-item-pane items item-content-width)))
                            item-width))
               (rail-lines (org-air-view--indent-pane-lines
                            (org-air-view--render-lines
                             rail-content-width
                             (lambda () (org-air-view--insert-rail items rail-content-width)))
                            rail-width)))
          (org-air-view--insert-lines
           (org-air-view--compose-columns
            (list (cons item-lines item-width) (cons rail-lines rail-width))
            divider)))
      (org-air-view--insert-top-rail items width)
      (insert "\n")
      (org-air-view--insert-rule)
      (insert "\n")
      (org-air-view--insert-lines
       (org-air-view--render-lines width
                                   (lambda () (org-air-view--insert-item-pane items width)))))
    (insert "\n")
    (org-air-view--insert-rule)
    (org-air-view--insert-footer)
    (when (integerp org-air-view-width)
      (org-air-view--normalize-buffer-lines org-air-view-width))
    (setq org-air-view--rendered-width width)
    (goto-char (point-min))))

(defun org-air-view--save-position ()
  "Return a token describing the cursor location for later restoration."
  (list :marker (get-text-property (point) 'org-air-marker)
        :section (get-text-property (point) 'org-air-section)
        :line (line-number-at-pos)
        :column (current-column)))

(defun org-air-view--find-property (prop value)
  "Return the first position where text PROP equals VALUE, or nil."
  (when value
    (let ((pos (point-min)) (found nil))
      (while (and (not found) pos (< pos (point-max)))
        (if (equal (get-text-property pos prop) value)
            (setq found pos)
          (setq pos (next-single-property-change pos prop nil (point-max)))))
      found)))

(defun org-air-view--restore-position (token)
  "Restore the cursor to the location described by TOKEN.
Prefers the same item, then the same section, falling back to the same
line/column so point and the user's place survive a re-render."
  (let ((target (or (org-air-view--find-property
                     'org-air-marker (plist-get token :marker))
                    (org-air-view--find-property
                     'org-air-section (plist-get token :section)))))
    (if target
        (goto-char target)
      (goto-char (point-min))
      (forward-line (1- (or (plist-get token :line) 1)))
      (move-to-column (or (plist-get token :column) 0)))))

(defun org-air-view--render-current ()
  "Re-render the dashboard from `org-air-view--items', preserving point.
Filters, scope and the calendar month are buffer-local and survive; the
cursor is restored to the same item (or section, or line) afterwards."
  (let ((token (org-air-view--save-position)))
    (org-air-view--render (or org-air-view--items (org-air-query-items))
                          org-air-view--tag-filter)
    (org-air-view--restore-position token)))

(defun org-air-view--resize-refresh ()
  "Re-render only when the displaying window's width has actually changed.
Called from the debounced window-size/-configuration hook."
  (let ((width (org-air-view--render-width)))
    (unless (eql width org-air-view--rendered-width)
      (org-air-view--render-current))))

;;;###autoload
(defun org-air-view ()
  "Open the org-air dashboard buffer."
  (interactive)
  (let ((buffer (get-buffer-create org-air-view-buffer-name)))
    (with-current-buffer buffer
      (org-air-view-mode)
      (unless (and org-air-view--items
                   (equal org-air-view--items-key (list org-air-files org-air-inbox-file)))
        (setq org-air-view--items (org-air-query-items))))
    ;; Display the buffer first so width derivation measures the window
    ;; that actually shows the dashboard (U1), then render into it.
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (org-air-view--render org-air-view--items org-air-view--tag-filter))))

(defun org-air-refresh ()
  "Re-query files and refresh the current org-air dashboard.
Preserves the active filter and the cursor's place."
  (interactive)
  (let ((token (org-air-view--save-position))
        (filter org-air-view--tag-filter))
    (setq org-air-view--items (org-air-query-items))
    (org-air-view--render org-air-view--items filter)
    (org-air-view--restore-position token)))

(defun org-air--relevant-file-p (file)
  "Return non-nil when FILE is one of the configured org-air files."
  (and file
       (let ((truename (ignore-errors (file-truename file)))
              (candidates (delq nil (cons (and (boundp 'org-air-inbox-file)
                                               org-air-inbox-file)
                                          (and (boundp 'org-air-files)
                                               org-air-files)))))
         (and truename
              (seq-some (lambda (f)
                          (and f (equal (ignore-errors (file-truename f))
                                        truename)))
                        candidates)))))

(defun org-air-view--after-save-refresh ()
  "Refresh an open org-air dashboard after a configured file is saved.
Scoped to `org-air-files'/`org-air-inbox-file'; this covers capture,
refile, and any manual edit saved from an Org buffer (U3).  Point and
filters are preserved by `org-air-refresh'."
  (when (and buffer-file-name (org-air--relevant-file-p buffer-file-name))
    (let ((buffer (get-buffer org-air-view-buffer-name)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (derived-mode-p 'org-air-view-mode)
            (org-air-refresh)))))))

(add-hook 'after-save-hook #'org-air-view--after-save-refresh)

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

(defun org-air-view--calendar-month-time (&optional base offset)
  "Return month time from BASE shifted by OFFSET months."
  (let* ((decoded (decode-time (or base (current-time))))
         (month (+ (decoded-time-month decoded) (or offset 0)))
         (year (decoded-time-year decoded)))
    (while (< month 1)
      (setq month (+ month 12)
            year (1- year)))
    (while (> month 12)
      (setq month (- month 12)
            year (1+ year)))
    (encode-time 0 0 0 1 month year)))

(defun org-air-calendar-prev ()
  "Page the persistent org-air calendar to the previous month."
  (interactive)
  (setq org-air-view--cal-month
        (org-air-view--calendar-month-time org-air-view--cal-month -1))
  (org-air-view--render-current))

(defun org-air-calendar-next ()
  "Page the persistent org-air calendar to the next month."
  (interactive)
  (setq org-air-view--cal-month
        (org-air-view--calendar-month-time org-air-view--cal-month 1))
  (org-air-view--render-current))

(defun org-air-calendar-today ()
  "Recenter the persistent org-air calendar on today."
  (interactive)
  (setq org-air-view--cal-month nil)
  (org-air-view--render-current))

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
