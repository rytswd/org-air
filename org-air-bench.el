;;; org-air-bench.el --- Frame-real performance fence for org-air -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: org-air contributors
;; Keywords: outlines, calendar
;; Version: 0.1.0
;; URL: https://github.com/rytswd/org-air
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; WHY THIS FILE REFUSES TO RUN IN BATCH.
;;
;; A user reported "toggling a 191-item section takes about a second".
;; Every batch benchmark this project had said the same operation took
;; 0.054 s.  Both numbers were true: they were measuring different
;; machines.
;;
;;   Emacs in batch mode        gc-cons-percentage = 1.0
;;   a real interactive frame  gc-cons-percentage = 0.1
;;
;; Emacs lowers `gc-cons-percentage' out of its startup value only for an
;; interactive session; `--batch' keeps 1.0, which means "collect after the
;; heap doubles" -- i.e. essentially never, for the duration of a test.  A
;; repaint that allocates a few MB therefore collects ZERO times in batch
;; and two to six times in a frame, and in a session with Org, org-ql and a
;; loaded board ONE collection costs ~55 ms.  That single configuration
;; difference was the whole of the 0.054 s vs 1.000 s gap, and no amount of
;; batch profiling could ever have shown it.
;;
;; The second half of the same blindness: `string-width' is a different
;; function on a graphical frame.  On a pure-ASCII string it is free
;; everywhere; on a string containing ANY non-ASCII character -- and every
;; org-air row carries the approved glyph vocabulary -- it costs ~0 conses
;; in batch and ~4 conses per column in a frame.  Measuring the rows was
;; 40% of the allocation, and batch charged nothing for it.
;;
;; So this file is a benchmark that REFUSES to run in batch.  It expands a
;; realistic section with the shipped default styles in a real frame, with
;; `redisplay' inside the measured window, and asserts a wall-clock
;; ceiling.  It is wired into `make check-gui' (never `make check', which
;; cannot have a display everywhere), and `make check' prints a loud notice
;; that the fence did NOT run, so a green batch gate can never be mistaken
;; for a performance verdict.
;;
;; Entry points:
;;
;;   M-x org-air-bench-expand-report   measure here, in THIS frame, and
;;                                     show the numbers in a buffer.
;;   org-air-bench-batch-fence         the `make check-gui' driver: arms
;;                                     itself on a timer (a frame is not
;;                                     real until Emacs reaches its command
;;                                     loop), measures, prints, and exits
;;                                     0 pass / 1 breach / 2 NO DISPLAY.
;;
;; Exit code 2 is the important one: a fence that silently "passes" when it
;; could not run is worse than no fence.

;;; Code:

(require 'org-air)
(require 'org-air-view)

(declare-function org-ql--normalize-query "org-ql" t t)

(defgroup org-air-bench nil
  "Frame-real performance fence for org-air."
  :group 'org-air
  :prefix "org-air-bench-")

(defcustom org-air-bench-items 191
  "Number of items the fence corpus holds in one section.
191 is the size the user reported: a single \"Needs attention\" section
that used to take about a second to open."
  :type 'integer
  :group 'org-air-bench)

(defcustom org-air-bench-expand-ceiling 0.250
  "Wall-clock ceiling, in seconds, for one section expand in a real frame.
Measured end to end: `org-air-toggle-section' plus a forced `redisplay',
on `org-air-bench-items' rows with the SHIPPED default styles.

Why this number.  The reported defect measured 1.000 s; the same expand
now measures 0.07-0.11 s depending on the rail orientation, of which
~55 ms is ONE garbage collection whose cost is a property of the
session's live heap rather than of org-air.  A ceiling of 0.250 s is far
enough above the measurement not to flap on a loaded CI box, and far
enough below the defect to catch any regression worth the name: it fails
on anything slower than ~2.5x today, and it would have failed the
reported defect by 4x."
  :type 'number
  :group 'org-air-bench)

(defcustom org-air-bench-repeats 5
  "How many expand/collapse cycles the fence times.
The reported figure is the MEDIAN; the best and worst are reported too, so
a single unlucky collection cannot pass or fail the gate on its own."
  :type 'integer
  :group 'org-air-bench)

(defcustom org-air-bench-frame-size '(200 . 50)
  "Frame size (COLUMNS . LINES) the fence measures at, or nil to leave it.
A default Emacs frame started with -Q is 80 columns, BELOW the rail
threshold, so the board would render board-only and the fence would not
exercise the layout the report is about.  The default here is a plain wide
frame: rail on, real column composition."
  :type '(choice (const :tag "Leave the frame alone" nil)
                 (cons integer integer))
  :group 'org-air-bench)

(defcustom org-air-bench-min-rows 100
  "Fewest rows an expand must ADD for the fence to count as a measurement.
A benchmark that silently toggled an empty section would report a
beautiful number and prove nothing; below this the fence FAILS instead."
  :type 'integer
  :group 'org-air-bench)

(defun org-air-bench--corpus (dir n)
  "Write N realistic undated task headings into DIR and return DIR.
Every item lands in one section, which is the shape of the report: a
single large fold, not many small ones."
  (with-temp-file (expand-file-name "bench.org" dir)
    (dotimes (i n)
      (insert (format "* TODO [#%c] Task %03d with a reasonably long \
realistic title :work:proj:\n:PROPERTIES:\n:CREATED: [2024-03-%02d Mon]\n\
:END:\n"
                      (aref "ABC" (mod i 3)) i (1+ (mod i 27))))))
  dir)

(defun org-air-bench--section-positions ()
  "Return one buffer position per distinct `org-air-section' on the board."
  (let ((pos (point-min))
        (seen nil)
        (out nil))
    (while (and pos (< pos (point-max)))
      (let ((section (get-text-property pos 'org-air-section)))
        (when (and section (not (memq section seen)))
          (push section seen)
          (push pos out)))
      (setq pos (next-single-property-change
                 pos 'org-air-section nil (point-max))))
    (nreverse out)))

(defun org-air-bench--biggest-section ()
  "Move point to the section whose expansion reveals most rows; return the gain.
The corpus is synthetic but the classifier owns which bucket it lands in,
so the fence DISCOVERS the large fold rather than assuming one.  Leaves
every section collapsed again."
  (let ((best nil)
        (gain 0))
    (dolist (pos (org-air-bench--section-positions))
      (goto-char pos)
      (let ((before (count-lines (point-min) (point-max))))
        (org-air-toggle-section)
        (let ((added (- (count-lines (point-min) (point-max)) before)))
          (when (> added gain)
            (setq gain added best (get-text-property (point) 'org-air-section))))
        (goto-char pos)
        (org-air-toggle-section)))
    (when best
      (goto-char (or (car (seq-filter
                           (lambda (p) (eq (get-text-property p 'org-air-section)
                                           best))
                           (org-air-bench--section-positions)))
                     (point-min))))
    gain))

(defun org-air-bench--median (numbers)
  "Return the median of NUMBERS, which must be non-empty."
  (let* ((sorted (sort (copy-sequence numbers) #'<))
         (len (length sorted)))
    (nth (/ len 2) sorted)))

(defun org-air-bench-expand (&optional items repeats)
  "Measure one section expand in the CURRENT frame and return a plist.
ITEMS defaults to `org-air-bench-items' and REPEATS to
`org-air-bench-repeats'.  The corpus is synthetic and lives in a temporary
directory; the caller's `org-air-files' is never consulted and no user
file is opened.  Styles are left at their shipped defaults on purpose --
the point of the fence is the default the user actually sees.

The measured window is `org-air-toggle-section' PLUS a forced
\(redisplay t), because a repaint that only builds strings has not yet
cost the user anything.  Returned plist keys: `:graphic', `:svg',
`:orientation', `:items', `:best', `:median', `:worst', `:collections',
`:conses', `:gc-cons-percentage', `:ceiling', `:pass'."
  (let* ((items (or items org-air-bench-items))
         (repeats (max 1 (or repeats org-air-bench-repeats)))
         (dir (org-air-bench--corpus (make-temp-file "org-air-bench" t) items))
         (org-air-files (list dir))
         (times nil)
         (collections 0)
         (conses 0)
         (rows 0)
         (orientation nil))
    (when (consp org-air-bench-frame-size)
      (ignore-errors (set-frame-size (selected-frame)
                                     (car org-air-bench-frame-size)
                                     (cdr org-air-bench-frame-size))))
    (unwind-protect
        (progn
          (org-air)
          (with-current-buffer (get-buffer org-air-view-buffer-name)
            (setq rows (org-air-bench--biggest-section))
            (setq orientation org-air-view--orientation)
            ;; One discarded warm-up cycle: the first expand also pays for
            ;; the svg image cache and the classify cache, which a steady
            ;; state does not.  Both are measured elsewhere; the fence is
            ;; about the repeated operation the user complained about.
            (org-air-toggle-section)
            (redisplay t)
            (org-air-toggle-section)
            (redisplay t)
            (dotimes (_ repeats)
              (let ((gcs gcs-done)
                    (cons0 (car (memory-use-counts)))
                    (start (float-time)))
                (org-air-toggle-section)
                (redisplay t)
                (push (- (float-time) start) times)
                (setq collections (+ collections (- gcs-done gcs))
                      conses (+ conses (- (car (memory-use-counts)) cons0))))
              (org-air-toggle-section)
              (redisplay t))))
      (ignore-errors (delete-directory dir t)))
    (let ((median (org-air-bench--median times)))
      (list :graphic (display-graphic-p)
            :svg (image-type-available-p 'svg)
            :orientation orientation
            :items items
            :rows rows
            :best (apply #'min times)
            :median median
            :worst (apply #'max times)
            :collections (/ (float collections) repeats)
            :conses (/ conses repeats)
            :gc-cons-percentage gc-cons-percentage
            :ceiling org-air-bench-expand-ceiling
            :pass (and (>= rows org-air-bench-min-rows)
                       (<= median org-air-bench-expand-ceiling))))))

(defun org-air-bench-format (result)
  "Return RESULT, a `org-air-bench-expand' plist, as a report string."
  (format "org-air expand fence: %s
  frame          graphic=%s svg=%s orientation=%s Emacs %s
  collector      gc-cons-percentage=%s (batch would be 1.0 -- see \
org-air-bench.el)
  corpus         %d items, shipped default styles, %d rows revealed \
\(floor %d)
  expand+redisplay  best %.3fs  median %.3fs  worst %.3fs
  per expand     %.1f collections, %d conses
  ceiling        %.3fs"
          (if (plist-get result :pass) "PASS" "FAIL")
          (plist-get result :graphic) (plist-get result :svg)
          (plist-get result :orientation) emacs-version
          (plist-get result :gc-cons-percentage)
          (plist-get result :items) (plist-get result :rows)
          org-air-bench-min-rows
          (plist-get result :best) (plist-get result :median)
          (plist-get result :worst)
          (plist-get result :collections) (plist-get result :conses)
          (plist-get result :ceiling)))

;;;###autoload
(defun org-air-bench-expand-report ()
  "Measure a section expand in this frame and show the numbers.
Interactive counterpart of the `make check-gui' fence.  Refuses to report
a number from a non-graphical frame, where the measurement would be a
different machine's (see the Commentary)."
  (interactive)
  (unless (display-graphic-p)
    (user-error "No graphical frame: an org-air-bench number would be \
meaningless here"))
  (let ((result (org-air-bench-expand)))
    (with-current-buffer (get-buffer-create "*org-air bench*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (org-air-bench-format result) "\n")
        (goto-char (point-min)))
      (special-mode)
      (display-buffer (current-buffer)))
    (message "%s" (org-air-bench-format result))
    result))

(defun org-air-bench--emit (string)
  "Print STRING to real stderr, which exists with or without a frame."
  (princ (concat string "\n") #'external-debugging-output))

(defun org-air-bench--run-and-exit ()
  "Run the fence, print the transcript and exit with its verdict.
Exit 0 pass, 1 ceiling breached or error, 2 NO GRAPHICAL DISPLAY."
  (let ((exit 1))
    (unwind-protect
        (condition-case err
            (if (not (display-graphic-p))
                (progn
                  (org-air-bench--emit "\
================================================================
org-air expand fence: SKIPPED -- NO GRAPHICAL DISPLAY.
This is NOT a pass.  In batch Emacs keeps gc-cons-percentage at
1.0, so the collector -- which was 87% of the reported defect --
never runs, and the fence would measure a machine that does not
exist.  Run under X/Wayland or Xvfb:  DISPLAY=:99 make check-gui
================================================================")
                  (setq exit 2))
              (let ((result (org-air-bench-expand)))
                (org-air-bench--emit (org-air-bench-format result))
                (setq exit (if (plist-get result :pass) 0 1))
                (unless (plist-get result :pass)
                  (org-air-bench--emit "\
org-air expand fence: FAILED -- a section expand got slower than the
ceiling.  This is the class that batch benchmarking cannot see; profile
in a FRAME (allocation and collections, not just CPU) before touching
the ceiling."))))
          (error
           (org-air-bench--emit (format "org-air expand fence: ERROR %S" err))
           (setq exit 1)))
      (kill-emacs exit))))

;;;###autoload
(defun org-air-bench-batch-fence ()
  "Arm the `make check-gui' performance fence and return.
Deliberately does NOT measure at load time.  A frame created by Emacs
with -Q and -l FILE is not yet mapped, and no timer fires while FILE is
still loading, so anything measured there measures the wrong machine
\(the same trap `make check-gui' documents).  This arms a one-shot timer
and returns, so `-l' finishes, Emacs reaches its command loop with a real
frame, and only then does the fence run.  A second timer is a hard
watchdog so the target can never hang a job."
  (run-with-timer 2 nil #'org-air-bench--run-and-exit)
  (run-with-timer 180 nil
                  (lambda ()
                    (org-air-bench--emit
                     "org-air expand fence: TIMEOUT -- not measured")
                    (kill-emacs 3))))

(provide 'org-air-bench)
;;; org-air-bench.el ends here
