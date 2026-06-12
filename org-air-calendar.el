;;; org-air-calendar.el --- Calendar pane for org-air -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: org-air contributors
;; Keywords: outlines, calendar
;; Version: 0.1.0
;; URL: https://github.com/rytswd/org-air
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Month calendar rendering for org-air dashboards.

;;; Code:

(require 'calendar)
(require 'org)
(require 'org-air-query)
(require 'org-air-faces)

(declare-function org-ql--normalize-query "org-ql" t t)

(defcustom org-air-calendar-week-start 1
  "First day of week for the org-air calendar.
0 means Sunday, 1 means Monday."
  :type '(choice (const :tag "Sunday" 0) (const :tag "Monday" 1))
  :group 'org-air)

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
  "Return hash table of date keys with scheduled/deadline ITEMS."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (item items table)
      (dolist (timestamp (list (org-air-item-scheduled item)
                               (org-air-item-deadline item)))
        (when-let* ((key (org-air-calendar--timestamp-key timestamp)))
          (puthash key t table))))))

(defun org-air-calendar--glyph (gui tty)
  "Return GUI glyph or TTY fallback."
  (if (display-graphic-p) gui tty))

(defun org-air-calendar--weekdays ()
  "Return weekday labels according to `org-air-calendar-week-start'."
  (if (= org-air-calendar-week-start 1)
      '("Mo" "Tu" "We" "Th" "Fr" "Sa" "Su")
    '("Su" "Mo" "Tu" "We" "Th" "Fr" "Sa")))

(defun org-air-calendar--column (calendar-dow)
  "Return zero-based display column for CALENDAR-DOW."
  (mod (- calendar-dow org-air-calendar-week-start) 7))

;;;###autoload
(defun org-air-calendar-insert-month (&optional date items)
  "Insert a compact month calendar for DATE marking dashboard ITEMS."
  (let* ((decoded (decode-time (or date (current-time))))
         (month (decoded-time-month decoded))
         (year (decoded-time-year decoded))
         (today (decode-time (current-time)))
         (today-key (org-air-calendar--date-key (decoded-time-month today)
                                                (decoded-time-day today)
                                                (decoded-time-year today)))
         (marks (org-air-calendar--marked-days items))
         (first-day (org-air-calendar--column
                     (calendar-day-of-week (list month 1 year))))
         (last-day (calendar-last-day-of-month month year))
         (day 1))
    (insert (propertize (format "%s %d                 %s %s\n"
                              (calendar-month-name month) year
                              (org-air-calendar--glyph "‹" "<")
                              (org-air-calendar--glyph "›" ">"))
                      'face 'org-air-face-calendar-header))
    (insert (propertize (string-join (org-air-calendar--weekdays) " ")
                        'face 'org-air-face-calendar-day-name)
            "\n")
    (dotimes (_ first-day)
      (insert "   "))
    (while (<= day last-day)
      (let* ((key (org-air-calendar--date-key month day year))
             (calendar-dow (calendar-day-of-week (list month day year)))
             (weekend (memq calendar-dow '(0 6)))
             (marked (gethash key marks))
             (todayp (equal key today-key))
             (face (cond
                    (todayp 'org-air-face-calendar-today)
                    (marked 'org-air-face-calendar-event)
                    (weekend 'org-air-face-calendar-weekend)
                    (t 'org-air-face-calendar-day))))
        (insert (propertize (format "%2d" day) 'face face))
        (insert (if marked (org-air-calendar--glyph "●" "o") " "))
        (when (= (org-air-calendar--column calendar-dow) 6)
          (insert "\n")))
      (setq day (1+ day)))
    (unless (bolp) (insert "\n"))
    (insert (propertize (format "%s has items   today is underlined\n"
                              (org-air-calendar--glyph "●" "o"))
                      'face 'org-air-face-faded))))

(provide 'org-air-calendar)

;; Local Variables:
;; package-lint-main-file: "org-air.el"
;; End:
;;; org-air-calendar.el ends here
