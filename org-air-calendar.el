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
(require 'org-air-layout)

(declare-function org-ql--normalize-query "org-ql" t t)
(declare-function org-air-view--svg-image-cached "org-air-view")
(declare-function svg-create "svg")
(declare-function svg-rectangle "svg")
(declare-function svg-text "svg")
(declare-function svg-image "svg")

(defcustom org-air-calendar-day-spacing 'auto
  "Width of each calendar day cell.
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

(defcustom org-air-calendar-week-start 0
  "First day of week for the org-air calendar.
0 means Sunday (the default), 1 means Monday."
  :type '(choice (const :tag "Sunday" 0) (const :tag "Monday" 1))
  :group 'org-air)

(defcustom org-air-calendar-center t
  "When non-nil, centre the calendar grid block within the rail.
The whole grid block (header, weekday row, day rows, legend) shares one
block width (the weekday-row width) and is prefixed by
`lead = (max 0 (/ (- width row-width) 2))' spaces so it sits centred in
the available rail WIDTH.  Floors at 0 when WIDTH <= row-width (narrow rail
-> no centring, no clip).  Nil restores the left-packed spine layout."
  :type 'boolean
  :group 'org-air)

(defun org-air-calendar--date-key (month day year)
  "Return sortable key for MONTH DAY YEAR."
  (format "%04d-%02d-%02d" year month day))

(defun org-air-calendar--time-key (time)
  "Return date key for the Emacs TIME value (epoch floats included)."
  (when time
    (let ((decoded (ignore-errors (decode-time time))))
      (when decoded
        (org-air-calendar--date-key (decoded-time-month decoded)
                                    (decoded-time-day decoded)
                                    (decoded-time-year decoded))))))

(defun org-air-calendar--timestamp-key (timestamp)
  "Return date key for Org TIMESTAMP."
  (when-let* ((time (ignore-errors (org-timestamp-to-time timestamp))))
    (org-air-calendar--time-key time)))

(defun org-air-calendar--item-deadline-time (item)
  "Return ITEM's effective deadline as an Emacs time value, or nil.
The heading's own deadline first, else the scan-time `body-deadline'
slot (an epoch float: the first `DEADLINE:' in the subtree, recorded by
the scan so the calendar NEVER opens a file).  Probing the subtree here
instead costs a `find-file-noselect' for every item WITHOUT a deadline
— nearly every item on a journal corpus.  A live-marker item built
outside the scan keeps the bounded subtree probe."
  (or (when-let* ((ts (org-air-item-deadline item)))
        (ignore-errors (org-timestamp-to-time ts)))
      (org-air-item-body-deadline item)
      (let ((marker (org-air-item-marker item)))
        (when (and (markerp marker) (marker-buffer marker))
          (with-current-buffer (marker-buffer marker)
            (ignore-errors
              (save-excursion
                (goto-char (marker-position marker))
                (org-back-to-heading t)
                (let ((end (save-excursion (org-end-of-subtree t t))))
                  (when (re-search-forward org-deadline-time-regexp end t)
                    (org-timestamp-to-time
                     (org-timestamp-from-string
                      (format "<%s>"
                              (match-string-no-properties 1)))))))))))))

(defun org-air-calendar--marked-days (items)
  "Return hash of date key -> strongest mark kind for ITEMS.
Kinds are `deadline' or `scheduled'; precedence is deadline > scheduled
so a day carrying both reads as a deadline."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (item items table)
      (when-let* ((time (org-air-calendar--item-deadline-time item))
                  (key (org-air-calendar--time-key time)))
        (puthash key 'deadline table))
      (when-let* ((ts (org-air-item-scheduled item))
                  (key (org-air-calendar--timestamp-key ts)))
        (unless (eq (gethash key table) 'deadline)
          (puthash key 'scheduled table))))))

(defun org-air-calendar--mark (kind)
  "Return (GLYPH . FACE) for mark KIND, or nil for no mark."
  (pcase kind
    ('deadline (cons (org-air-calendar--glyph "◆" "!")
                     'org-air-face-calendar-deadline))
    ('scheduled (cons (org-air-calendar--glyph "●" "o")
                      'org-air-face-calendar-scheduled))
    ('created (cons (org-air-calendar--glyph "∙" ".")
                    'org-air-face-calendar-created))
    ;; The review-period marks: `period' highlights the day NUMBER (the
    ;; face) with a SPACE glyph so the mark column carries no noise;
    ;; `period-done' adds the quiet completion dot in the same face.
    ('period (cons " " 'org-air-face-calendar-period))
    ('period-done (cons (org-air-calendar--glyph "∙" ".")
                        'org-air-face-calendar-period))
    (_ nil)))

(declare-function org-air-view-day "org-air-view")

(defvar org-air-calendar-day-keymap
  (make-sparse-keymap)
  "Keymap active on a calendar day cell to focus its single-day view.
Keys (RET / mouse-1 -> `org-air-view-day') are installed by
`org-air--install-default-keybindings', so the
`org-air-use-default-keybindings' knob gates the calendar day cells too;
registered from `org-air-view.el' (which requires this file).")

(defun org-air-calendar--glyph (gui tty)
  "Return GUI glyph or TTY fallback."
  (if (display-graphic-p) gui tty))

(defun org-air-calendar--today-svg (text)
  "Return TEXT with a soft rounded svg today-cell background.
A tiny rounded-rect (\=:rx ch/6, the same box-fit discipline as the item
pills) is drawn in the `org-air-face-calendar-today' background behind the
day number.  On a non-graphical frame or when SVG is unavailable TEXT is
returned unchanged so the existing today-cell BACKGROUND face is the
mandatory fallback; the buffer text (the bytes) is never touched."
  (if (not (and (display-graphic-p) (require 'svg nil t)))
      text
    (or (ignore-errors
          (let* ((cw (frame-char-width))
                 (ch (frame-char-height))
                 (n (max 1 (string-width text)))
                 (w (* n cw))
                 (h ch)
                 (bg (or (face-background 'org-air-face-calendar-today nil t)
                         "#88C0D0"))
                 (fg (or (face-foreground 'org-air-face-calendar-today nil t)
                         (face-foreground 'default nil t)
                         "black"))
                 (radius (/ ch 6.0))
                 (fs (max 7 (round (* ch 0.7))))
                 ;; The today cell is a pure function of
                 ;; (text, bg, fg, cw, ch); memoise the image via the
                 ;; view layer's svg cache when that is loaded (the
                 ;; dashboard always loads it), else build it directly.
                 (build (lambda ()
                          (let ((svg (svg-create w h)))
                            (svg-rectangle svg 0.5 0.5
                                           (max 0 (- w 1.0)) (max 0 (- h 1.0))
                                           :rx radius :ry radius :fill bg)
                            (svg-text svg (string-trim text)
                                      :x (/ w 2.0) :y (round (* ch 0.72))
                                      :text-anchor "middle" :fill fg
                                      :font-size fs)
                            (svg-image svg :ascent 'center :width w :height h))))
                 (image (if (fboundp 'org-air-view--svg-image-cached)
                            (org-air-view--svg-image-cached
                             (list 'today text bg fg cw ch) build)
                          (funcall build))))
            ;; INVARIANT: no buffer position may carry both `mouse-face'
            ;; and an image `display' — Emacs 30's
            ;; DRAW_MOUSE_FACE SVG re-lookup (e69fafdb, bug#67794) would
            ;; synchronously re-rasterize this cell on every hover crossing
            ;; (and the hover face can't tint an image anyway).  The day
            ;; cell's `org-air-day' + keymap stay, so click/RET still focus
            ;; today; the TTY/no-svg fallback below keeps its text
            ;; `mouse-face' untouched.
            (let ((cell (propertize text 'display image)))
              (remove-text-properties 0 (length cell) '(mouse-face nil) cell)
              cell)))
        text)))

(defun org-air-calendar--weekdays ()
  "Return weekday labels according to `org-air-calendar-week-start'."
  (if (= org-air-calendar-week-start 1)
      '("Mo" "Tu" "We" "Th" "Fr" "Sa" "Su")
    '("Su" "Mo" "Tu" "We" "Th" "Fr" "Sa")))

(defun org-air-calendar--column (calendar-dow)
  "Return zero-based display column for CALENDAR-DOW."
  (mod (- calendar-dow org-air-calendar-week-start) 7))

;;;###autoload
(defun org-air-calendar-insert-month (&optional date items width inset marks)
  "Insert a compact month calendar for DATE marking dashboard ITEMS.
WIDTH is the available content width; it selects the responsive day-cell
width (3 vs 4 columns) when `org-air-calendar-day-spacing' is `auto'.
INSET is the content-spine indent in columns applied to the weekday row,
the day grid and the legend so the calendar shares the rail's single
left edge; the header itself spans the full WIDTH.
MARKS, when non-nil, is a precomputed date-key -> mark-kind table used
instead of deriving the marks from ITEMS (so a non-board view can mark
by its own dates, e.g. the project's per-doc updated stamp)."
  (let* ((inset (or inset 0))
         ;; The content spine eats INSET columns, so the day-cell width
         ;; must be chosen from the width that REMAINS for the grid —
         ;; otherwise a spaced (4-col) grid plus the inset overflows the
         ;; rail.
         (cell (org-air-calendar--cell-width (- (or width 0) inset)))
         (gap (if (>= cell 4) " " ""))
         (decoded (decode-time (or date (current-time))))
         (month (decoded-time-month decoded))
         (year (decoded-time-year decoded))
         (today (decode-time (current-time)))
         (today-key (org-air-calendar--date-key (decoded-time-month today)
                                                (decoded-time-day today)
                                                (decoded-time-year today)))
         (marks (or marks (org-air-calendar--marked-days items)))
         (first-day (org-air-calendar--column
                     (calendar-day-of-week (list month 1 year))))
         (last-day (calendar-last-day-of-month month year))
         (day 1)
         (weekday-row (if (>= cell 4)
                          (mapconcat (lambda (wd) (format "%-4s" wd))
                                     (org-air-calendar--weekdays) "")
                        ;; Compact tier: a 20-column row.
                        (string-join (org-air-calendar--weekdays) " ")))
         (row-width (string-width weekday-row))
         ;; When centring, the whole block (header included) shares the
         ;; row-width and is prefixed by LEAD spaces; otherwise the spine
         ;; INSET left-packs it.
         (center (and org-air-calendar-center width (> width row-width)))
         (lead (if center (max 0 (/ (- width row-width) 2)) inset))
         (pad-str (make-string (max 0 lead) ?\s))
         ;; The calendar header spans the FULL rail width (like every
         ;; other section header), not the centred grid width.
         (header-width (or width row-width)))
    (let* ((nav (concat (org-air-calendar--glyph "‹" "<") " "
                        (org-air-calendar--glyph "›" ">")))
           (full-label (format "%s %d" (calendar-month-name month) year))
           ;; Abbreviate the month before the nav truncates; measured
           ;; against the full header width.
           (label (if (> (+ 4 (string-width full-label) 2 (string-width nav))
                         header-width)
                      (format "%s %d"
                              (substring (calendar-month-name month) 0 3) year)
                    full-label)))
      ;; The header is full rail width with the nav right-anchored; the
      ;; grid, weekday row and legend below stay CENTRED (LEAD).
      (if (eq org-air-rail-header-style 'rule)
          (insert (org-air-layout-labelled-rule
                   label header-width
                   :suffix nav :suffix-face 'org-air-face-calendar-header)
                  "\n")
        (insert (org-air-layout-rail-header-string
                 label header-width
                 :suffix nav :suffix-face 'org-air-face-calendar-header)
                "\n"))
      (insert pad-str
              (propertize weekday-row 'face 'org-air-face-calendar-day-name)
              "\n"))
    ;; Per-day cell = "%2d" + marker (+ one breathing space when the rail
    ;; is wide enough; see `org-air-calendar-day-spacing').  Each grid
    ;; row opens at the content spine (INSET).
    (insert pad-str)
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
        ;; Each day cell carries its date and a click/RET keymap so it
        ;; can be focused into the single-day view.  Today also gets a
        ;; soft rounded svg bg (GUI only; TTY keeps the bg face).
        (let ((cell-text (propertize (format "%2d" day)
                                     'face face
                                     'org-air-day (encode-time 0 0 0 day month year)
                                     'mouse-face 'org-air-face-calendar-selected
                                     'keymap org-air-calendar-day-keymap)))
          (insert (if todayp
                      (org-air-calendar--today-svg cell-text)
                    cell-text)))
        ;; Today is a filled background on the day number — no ■ glyph.
        (insert (if mark (propertize (car mark) 'face (cdr mark)) " ")
                gap)
        (when (= (org-air-calendar--column calendar-dow) 6)
          (insert "\n")
          ;; Open the next grid row at the content spine.
          (when (< day last-day) (insert pad-str))))
      (setq day (1+ day)))
    (unless (bolp) (insert "\n"))
    ;; Separate the legend from the grid with one blank line, indent it
    ;; to the spine, and space each glyph from its word.
    (insert "\n" pad-str)
    ;; A per-tier SINGLE-LINE legend that doubles as a key.  The narrow
    ;; tier (compact cell) drops `created' from the cramped key (the ·
    ;; mark still renders on the grid); the wide tier names all three.
    ;; Markers come from `org-air-calendar--glyph', the same source as
    ;; the cells.
    (insert (org-air-calendar--legend (>= cell 4)) "\n")))

(defun org-air-calendar--legend-entry (gui tty face word)
  "Return a legend key entry: GUI/TTY glyph in FACE, a space, then WORD."
  (concat (propertize (org-air-calendar--glyph gui tty) 'face face)
          " "
          (propertize word 'face 'org-air-face-calendar-legend)))

(defun org-air-calendar--legend (wide)
  "Return the single-line calendar legend; WIDE names `created' too.
Each glyph is spaced from its word (\"◆ due\") and the entries are
separated by a wider 4-space gap.  Today is deliberately absent — the
filled today cell is its own unmistakable cue."
  (let ((due (org-air-calendar--legend-entry
              "◆" "!" 'org-air-face-calendar-deadline "due"))
        (sched (org-air-calendar--legend-entry
                "●" "o" 'org-air-face-calendar-scheduled "sched"))
        (created (org-air-calendar--legend-entry
                  "∙" "." 'org-air-face-calendar-created "created")))
    (if wide
        (string-join (list due sched created) "    ")
      (string-join (list due sched) "    "))))

(provide 'org-air-calendar)

;; Local Variables:
;; package-lint-main-file: "org-air.el"
;; End:
;;; org-air-calendar.el ends here
