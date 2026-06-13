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

(defcustom org-air-calendar-day-spacing 'auto
  "Width of each calendar day cell (T3a, responsive per ruling pxvlzyov).
`auto' (default) uses a spaced 4-column cell only when the available rail
width is wide enough (>= 30 cols, i.e. windows >= 120); otherwise it
falls back to the compact 3-column cell so the narrow rail tier (95-119)
never clips the Sunday column.  An integer 3 or 4 forces that cell width."
  :type '(choice (const :tag "Responsive" auto)
                 (const :tag "Compact (3)" 3)
                 (const :tag "Spaced (4)" 4))
  :group 'org-air)

(defconst org-air-calendar--spacing-threshold 30
  "Available width at/above which `auto' spacing uses the 4-column cell.")

(defun org-air-calendar--cell-width (avail)
  "Return the day-cell width (3 or 4) for AVAIL columns."
  (pcase org-air-calendar-day-spacing
    (4 4)
    (3 3)
    (_ (if (and (integerp avail)
                (>= avail org-air-calendar--spacing-threshold))
           4
         3))))

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

(defun org-air-calendar--item-deadline (item)
  "Return ITEM deadline, checking the origin when needed."
  (or (org-air-item-deadline item)
      (when-let* ((marker (org-air-item-marker item))
                  (buffer (marker-buffer marker)))
        (with-current-buffer buffer
          (save-excursion
            (goto-char marker)
            (org-back-to-heading t)
            (let ((end (save-excursion (org-end-of-subtree t t))))
              (when (re-search-forward org-deadline-time-regexp end t)
                (org-timestamp-from-string
                 (format "<%s>" (match-string-no-properties 1))))))))))

(defun org-air-calendar--marked-days (items)
  "Return hash of date key -> strongest mark kind for ITEMS (T3b).
Kinds are `deadline' or `scheduled'; precedence is deadline > scheduled
so a day carrying both reads as a deadline."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (item items table)
      (when-let* ((ts (org-air-calendar--item-deadline item))
                  (key (org-air-calendar--timestamp-key ts)))
        (puthash key 'deadline table))
      (when-let* ((ts (org-air-item-scheduled item))
                  (key (org-air-calendar--timestamp-key ts)))
        (unless (eq (gethash key table) 'deadline)
          (puthash key 'scheduled table))))))

(defun org-air-calendar--mark (kind)
  "Return (GLYPH . FACE) for mark KIND (T3b), or nil for no mark."
  (pcase kind
    ('deadline (cons (org-air-calendar--glyph "◆" "!")
                     'org-air-face-calendar-deadline))
    ('scheduled (cons (org-air-calendar--glyph "●" "o")
                      'org-air-face-calendar-scheduled))
    ('created (cons (org-air-calendar--glyph "·" ".")
                    'org-air-face-calendar-created))
    (_ nil)))

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
(defun org-air-calendar-insert-month (&optional date items width)
  "Insert a compact month calendar for DATE marking dashboard ITEMS.
WIDTH is the available content width; it selects the responsive day-cell
width (3 vs 4 columns) when `org-air-calendar-day-spacing' is `auto'."
  (let* ((cell (org-air-calendar--cell-width width))
         (gap (if (>= cell 4) " " ""))
         (decoded (decode-time (or date (current-time))))
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
    (let* ((weekday-row (if (>= cell 4)
                            (mapconcat (lambda (wd) (format "%-4s" wd))
                                       (org-air-calendar--weekdays) "")
                          ;; Compact tier: exactly the pre-T3a 20-col row.
                          (string-join (org-air-calendar--weekdays) " ")))
           (row-width (string-width weekday-row))
           (nav (concat (org-air-calendar--glyph "‹" "<") " "
                        (org-air-calendar--glyph "›" ">")))
           (label (format "%s %d" (calendar-month-name month) year))
           ;; Right-align the month-nav within the weekday-row width so the
           ;; ‹ › affordance never truncates, abbreviating the month name
           ;; before dropping the nav (D3).
           (label (if (> (+ (string-width label) 1 (string-width nav)) row-width)
                      (format "%s %d"
                              (substring (calendar-month-name month) 0 3) year)
                    label))
           (pad (max 1 (- row-width (string-width label) (string-width nav)))))
      (insert (propertize (concat label (make-string pad ?\s) nav)
                          'face 'org-air-face-calendar-header)
              "\n")
      (insert (propertize weekday-row 'face 'org-air-face-calendar-day-name)
              "\n"))
    ;; T3a: per-day cell = "%2d" + marker (+ one breathing space when the
    ;; rail is wide enough; responsive per `org-air-calendar-day-spacing').
    (dotimes (_ first-day)
      (insert (make-string cell ?\s)))
    (while (<= day last-day)
      (let* ((key (org-air-calendar--date-key month day year))
             (calendar-dow (calendar-day-of-week (list month day year)))
             (weekend (memq calendar-dow '(0 6)))
             (kind (gethash key marks))
             (mark (org-air-calendar--mark kind))
             (todayp (equal key today-key))
             (face (cond
                    (todayp 'org-air-face-calendar-today)
                    (mark (cdr mark))
                    (weekend 'org-air-face-calendar-weekend)
                    (t 'org-air-face-calendar-day))))
        (insert (propertize (format "%2d" day) 'face face))
        (insert (cond
                 (todayp (propertize (org-air-calendar--glyph "■" "#")
                                     'face 'org-air-face-calendar-today))
                 (mark (propertize (car mark) 'face (cdr mark)))
                 (t " "))
                gap)
        (when (= (org-air-calendar--column calendar-dow) 6)
          (insert "\n")))
      (setq day (1+ day)))
    (unless (bolp) (insert "\n"))
    ;; T3b/S9 (ruling tynxttsz): a per-tier SINGLE-LINE legend that doubles
    ;; as a key, the glyph hugging its label.  Narrow tier (compact cell)
    ;; drops `created' from the cramped key (the · mark still renders on
    ;; the grid); the wide tier names all three plus today.  Markers come
    ;; from `org-air-calendar--glyph', the same source as the cells.
    (insert (org-air-calendar--legend (>= cell 4)) "\n")))

(defun org-air-calendar--legend-entry (gui tty face word)
  "Return a legend key entry: GUI/TTY glyph in FACE, hugging WORD."
  (concat (propertize (org-air-calendar--glyph gui tty) 'face face)
          (propertize word 'face 'org-air-face-calendar-legend)))

(defun org-air-calendar--legend (wide)
  "Return the single-line calendar legend; WIDE names `created' too (T3b)."
  (let ((due (org-air-calendar--legend-entry
              "◆" "!" 'org-air-face-calendar-deadline "due"))
        (sched (org-air-calendar--legend-entry
                "●" "o" 'org-air-face-calendar-scheduled "sched"))
        (created (org-air-calendar--legend-entry
                  "·" "." 'org-air-face-calendar-created "created"))
        (today (org-air-calendar--legend-entry
                "■" "#" 'org-air-face-calendar-today "today")))
    (if wide
        (string-join (list due sched created today) " ")
      (string-join (list due sched today) " "))))

(provide 'org-air-calendar)

;; Local Variables:
;; package-lint-main-file: "org-air.el"
;; End:
;;; org-air-calendar.el ends here
