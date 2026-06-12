;;; org-air-calendar.el --- Calendar pane for org-air -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: org-air contributors
;; Keywords: outlines, calendar
;; Package-Requires: ((emacs "29.1") (org "9.6"))

;;; Commentary:

;; Month calendar rendering for org-air dashboards.

;;; Code:

(require 'calendar)
(require 'org)
(require 'org-air-query)

(declare-function org-ql--normalize-query "org-ql" t t)

(defun org-air-calendar--date-key (month day year)
  "Return sortable key for MONTH DAY YEAR."
  (format "%04d-%02d-%02d" year month day))

(defun org-air-calendar--timestamp-key (timestamp)
  "Return date key for Org TIMESTAMP."
  (when-let* ((time (ignore-errors (org-timestamp-to-time timestamp))))
    (let ((decoded (decode-time time)))
      (org-air-calendar--date-key (decoded-time-month decoded)
                                  (decoded-time-day decoded)
                                  (decoded-time-year decoded)))))

(defun org-air-calendar--marked-days (items)
  "Return hash table of scheduled date keys in ITEMS."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (item items table)
      (when-let* ((key (org-air-calendar--timestamp-key (org-air-item-scheduled item))))
        (puthash key t table)))))

;;;###autoload
(defun org-air-calendar-insert-month (&optional date items)
  "Insert a simple month calendar for DATE marking scheduled ITEMS."
  (let* ((decoded (decode-time (or date (current-time))))
         (month (decoded-time-month decoded))
         (year (decoded-time-year decoded))
         (today (decode-time (current-time)))
         (today-key (org-air-calendar--date-key (decoded-time-month today)
                                                (decoded-time-day today)
                                                (decoded-time-year today)))
         (marks (org-air-calendar--marked-days items))
         (first-day (calendar-day-of-week (list month 1 year)))
         (last-day (calendar-last-day-of-month month year))
         (day 1))
    (insert (propertize (format "%s %d\n" (calendar-month-name month) year)
                        'face 'org-air-face-salient))
    (insert (propertize "Su Mo Tu We Th Fr Sa\n" 'face 'org-air-face-subtle))
    (dotimes (_ first-day)
      (insert "   "))
    (while (<= day last-day)
      (let* ((key (org-air-calendar--date-key month day year))
             (face (cond
                    ((equal key today-key) 'org-air-face-popout)
                    ((gethash key marks) 'org-air-face-salient)
                    (t 'org-air-face-default))))
        (insert (propertize (format "%2d" day) 'face face))
        (insert (if (gethash key marks) "•" " "))
        (when (= (calendar-day-of-week (list month day year)) 6)
          (insert "\n")))
      (setq day (1+ day)))
    (unless (bolp) (insert "\n"))))

(provide 'org-air-calendar)
;;; org-air-calendar.el ends here
