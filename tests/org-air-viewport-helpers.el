;;; org-air-viewport-helpers.el --- batch viewport harness for org-air -*- lexical-binding: t; -*-

;;; Commentary:
;; Batch-render the org-air dashboard at controlled widths and assert
;; layout invariants.  `window-body-width' is meaningless in --batch
;; (always the default 80-col fake frame), so the harness relies on a
;; WIDTH SEAM in the renderer:
;;
;;   `org-air-view-width' — dynamic variable consulted by the renderer.
;;     nil (default)  -> derive from the live window as today.
;;     integer        -> compose every line to exactly that many columns
;;                       (`string-width', explicit padding — NOT
;;                       `:align-to' display properties, which cannot be
;;                       measured in batch).
;;
;; The variable is declared special here so tests can bind it before the
;; implementation defines it; the implementation owns the real
;; definition (defcustom) once the v0.2 layout lands.
;;
;; All renders freeze the Lisp-visible clock to `org-air-test-now'
;; (Mon 2026-06-15) so the calendar pane is deterministic: "June 2026".

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-air-test-helpers)

(defvar org-air-view-width nil
  "Width seam: explicit render width for the org-air dashboard.
Declared in the test harness so suites can bind it; the implementation
owns the canonical definition.")

(defvar org-air-view-height nil
  "Proposed HEIGHT seam (S6 full-height composition), mirroring the
width seam: nil (default) = derive from the displaying window
(`window-body-height'); integer = compose the buffer to exactly that
many rows in batch — header band first, footer band on the last row,
the body band blank-padded (full-width plain-space rows) in between.
Declared special here so suites can bind it before the implementation
defines the canonical defcustom.")

(defconst org-air-viewport-test-widths '(80 120 160)
  "Canonical render widths exercised by the viewport suites.")

(defconst org-air-viewport-test-narrow-width 80
  "Representative width below the responsive two-pane threshold (~100).")

(defconst org-air-viewport-test-wide-width 120
  "Representative width at/above the responsive two-pane threshold.")

(defmacro org-air-viewport-test--with-frozen-now (&rest body)
  "Run BODY with `current-time' frozen to `org-air-test-now'."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'current-time)
              (lambda () org-air-test-now)))
     ,@body))

;;;; Anti-tautology render guards.
;;
;; The gate asserts bytes the renderer PRODUCES.  A renderer that
;; sources the dashboard from the embedded mockup fixture files (as a
;; rejected impl change did via `org-air-view--maybe-insert-mockup-
;; fixture' keyed on GUI + canonical width + unfiltered board — exactly
;; this harness's render conditions) turns the byte-precise comparison
;; into fixture-vs-fixture.  Every harness render therefore runs with:
;;   1. any known file-insertion shim overridden to signal an error;
;;   2. `insert-file-contents' refusing tests/fixtures/layout-mockup-*
;;      unless the read comes from the mockup LOADER itself
;;      (`org-air-viewport-test--allow-mockup-read').

(defconst org-air-viewport-test--render-shims
  '(org-air-view--maybe-insert-mockup-fixture
    org-air-view--mockup-fixture-file)
  "Renderer functions that must never run inside the gate.")

(defvar org-air-viewport-test--allow-mockup-read nil
  "Non-nil while the test harness itself loads a mockup fixture.")

(defun org-air-viewport-test--shim-trap (&rest _)
  "Trap for renderer shims: the gate render path must be the real renderer."
  (error "org-air gate: render shim called during a gate render (tautology guard)"))

(defun org-air-viewport-test--call-with-render-guards (thunk)
  "Call THUNK with the anti-tautology render guards installed."
  (let ((real-ifc (symbol-function 'insert-file-contents))
        (trapped ()))
    (unwind-protect
        (progn
          (dolist (shim org-air-viewport-test--render-shims)
            (when (fboundp shim)
              (advice-add shim :override #'org-air-viewport-test--shim-trap)
              (push shim trapped)))
          (cl-letf (((symbol-function 'insert-file-contents)
                     (lambda (filename &rest args)
                       (when (and (stringp filename)
                                  (string-match-p "layout-mockup-[0-9]+\\.txt\\'"
                                                  filename)
                                  (not org-air-viewport-test--allow-mockup-read))
                         (error "org-air gate: render read mockup fixture %s (tautology guard)"
                                filename))
                       (apply real-ifc filename args))))
            (funcall thunk)))
      (dolist (shim trapped)
        (advice-remove shim #'org-air-viewport-test--shim-trap)))))

(defmacro org-air-viewport-test--with-render-guards (&rest body)
  "Run BODY with the anti-tautology render guards installed."
  (declare (indent 0) (debug t))
  `(org-air-viewport-test--call-with-render-guards (lambda () ,@body)))

(defmacro org-air-viewport-test-with-dashboard (width &rest body)
  "Render the dashboard at WIDTH over the fixtures; run BODY in its buffer.
WIDTH is bound to `org-air-view-width' for the render; WIDTH may also be
a cons (WIDTH . HEIGHT) to bind the height seam too.  The clock is
frozen to `org-air-test-now'.  The buffer is killed afterwards so the
per-buffer item cache never leaks between renders."
  (declare (indent 1) (debug t))
  `(org-air-test-with-fixtures
     (org-air-viewport-test--with-frozen-now
       (unwind-protect
           (org-air-viewport-test--with-render-guards
             (let* ((org-air-viewport-test--size ,width)
                    (org-air-view-width (if (consp org-air-viewport-test--size)
                                            (car org-air-viewport-test--size)
                                          org-air-viewport-test--size))
                    (org-air-view-height (if (consp org-air-viewport-test--size)
                                             (cdr org-air-viewport-test--size)
                                           org-air-view-height)))
               (org-air)
               (let ((buf (get-buffer "*org-air*")))
                 (should buf)
                 (with-current-buffer buf
                   ,@body))))
         (when (get-buffer "*org-air*")
           (kill-buffer "*org-air*"))))))

(defmacro org-air-viewport-test-with-empty-dashboard (width &rest body)
  "Render the dashboard at WIDTH with ZERO items; run BODY in its buffer.
Binds `org-air-files' to an empty scratch directory and
`org-air-inbox-file' to a not-yet-created file inside it, so every
section — and the view as a whole — is empty."
  (declare (indent 1) (debug t))
  `(let ((org-air-viewport-test--dir (make-temp-file "org-air-empty-" t)))
     (unwind-protect
         (let ((org-air-files (list org-air-viewport-test--dir))
               (org-air-inbox-file
                (expand-file-name "inbox.org" org-air-viewport-test--dir)))
           (org-air-viewport-test--with-frozen-now
             (unwind-protect
                 (org-air-viewport-test--with-render-guards
                   (let* ((org-air-viewport-test--size ,width)
                          (org-air-view-width
                           (if (consp org-air-viewport-test--size)
                               (car org-air-viewport-test--size)
                             org-air-viewport-test--size))
                          (org-air-view-height
                           (if (consp org-air-viewport-test--size)
                               (cdr org-air-viewport-test--size)
                             org-air-view-height)))
                     (org-air)
                     (let ((buf (get-buffer "*org-air*")))
                       (should buf)
                       (with-current-buffer buf
                         ,@body))))
               (when (get-buffer "*org-air*")
                 (kill-buffer "*org-air*")))))
       (delete-directory org-air-viewport-test--dir t))))

(defun org-air-viewport-test-lines ()
  "Return current buffer contents as a list of lines (no newlines).
Text properties are preserved; a trailing final newline does not
produce a phantom empty line."
  (let ((lines (split-string (buffer-string) "\n")))
    (if (and lines (equal (car (last lines)) ""))
        (butlast lines)
      lines)))

(defun org-air-viewport-test--align-to-p (line)
  "Return non-nil when LINE carries an `:align-to' display property.
Such lines have no measurable batch width; the v0.2 composition
primitive must pad explicitly instead."
  (let ((pos 0) (len (length line)) found)
    (while (and (< pos len) (not found))
      (let ((display (get-text-property pos 'display line)))
        (when (and (consp display)
                   (eq (car-safe display) 'space)
                   (plist-member (cdr display) :align-to))
          (setq found t)))
      (setq pos (or (next-single-property-change pos 'display line) len)))
    found))

(defun org-air-viewport-test-misaligned-lines (width)
  "Return (LINENO WIDTH-OR-SYMBOL LINE) for lines not composed to WIDTH.
A line conforms when it is blank or its `string-width' is exactly
WIDTH.  Lines relying on `:align-to' display properties are reported
with the symbol `align-to' since their batch width is unmeasurable."
  (let ((lineno 0) (bad ()))
    (dolist (line (org-air-viewport-test-lines))
      (setq lineno (1+ lineno))
      (cond
       ((string-empty-p line))
       ((org-air-viewport-test--align-to-p line)
        (push (list lineno 'align-to line) bad))
       ((/= (string-width line) width)
        (push (list lineno (string-width line) line) bad))))
    (nreverse bad)))

(defun org-air-viewport-test-assert-aligned (width)
  "Assert every non-blank buffer line is composed to exactly WIDTH columns."
  (let ((bad (org-air-viewport-test-misaligned-lines width)))
    (when bad
      (ert-fail
       (format "%d/%d line(s) not composed to width %d; first offenders:\n%s"
               (length bad)
               (length (org-air-viewport-test-lines))
               width
               (mapconcat
                (pcase-lambda (`(,lineno ,w ,line))
                  (format "  line %3d [%s]: %S"
                          lineno w (substring-no-properties line)))
                (seq-take bad 8)
                "\n"))))))

(defun org-air-viewport-test-trailing-whitespace-lines ()
  "Return (LINENO . LINE) for buffer lines ending in literal whitespace."
  (let ((lineno 0) (bad ()))
    (dolist (line (org-air-viewport-test-lines))
      (setq lineno (1+ lineno))
      (when (string-match-p "[ \t]\\'" line)
        (push (cons lineno line) bad)))
    (nreverse bad)))

;; `org-air-viewport-test-assert-no-trailing-whitespace' was retired
;; with its test (spec rev orwonzvz: plain-space full-width padding is
;; the contract; comparisons are right-trimmed).  The measuring
;; primitive above is kept for the harness self-test.

(defun org-air-viewport-test-calendar-present-p ()
  "Return non-nil when the calendar pane is visible in the buffer.
With the clock frozen to `org-air-test-now', the month header is
\"June 2026\" and a weekday label row follows."
  (let ((text (buffer-string)))
    (and (string-match-p "June 2026" text)
         (string-match-p "Mo Tu We Th Fr Sa Su\\|Su Mo Tu We Th Fr Sa" text)
         t)))

;;;; Phase-2 primitives — pane geometry, rail content, face application.

(defconst org-air-viewport-test-divider-regexp "[│|]"
  "Pane divider glyph: GUI │ or its TTY fallback |.")

(defconst org-air-viewport-test-section-titles
  '((inbox . "Inbox")
    (attention . "Needs attention")
    (upcoming . "Upcoming")
    (high-priority . "High priority")
    (stale . "Stale"))
  "Spec-frozen bucket display titles (design §3 mockups).")

(defun org-air-viewport-test-divider-positions ()
  "Return (LINENO . COLUMN) for each line containing a pane divider.
COLUMN is the display column (`string-width' of the text before the
first divider glyph on that line)."
  (let ((lineno 0) (hits ()))
    (dolist (line (org-air-viewport-test-lines))
      (setq lineno (1+ lineno))
      (when (string-match org-air-viewport-test-divider-regexp line)
        (push (cons lineno
                    (string-width (substring line 0 (match-beginning 0))))
              hits)))
    (nreverse hits)))

(defun org-air-viewport-test-divider-run ()
  "Return (COLUMN START-LINE LENGTH) for the longest unbroken divider run.
A run is a maximal sequence of CONSECUTIVE lines whose first divider
glyph sits at the SAME display column — the v0.2 body-band divider
running unbroken through the zipped panes.  Return nil when no line
carries a divider."
  (let ((best nil) (cur nil))
    (dolist (hit (org-air-viewport-test-divider-positions))
      (pcase-let ((`(,lineno . ,col) hit))
        (if (and cur
                 (= col (nth 0 cur))
                 (= lineno (+ (nth 1 cur) (nth 2 cur))))
            (setf (nth 2 cur) (1+ (nth 2 cur)))
          (setq cur (list col lineno 1)))
        (when (or (null best) (> (nth 2 cur) (nth 2 best)))
          (setq best (copy-sequence cur)))))
    best))

(defun org-air-viewport-test-face-applied-p (face)
  "Return non-nil when FACE is actually applied somewhere in the buffer.
Checks both `face' and `font-lock-face' properties, accepting FACE as
the value itself or as a member of an anonymous face list."
  (let ((pos (point-min)) found)
    (while (and (not found) (< pos (point-max)))
      (dolist (prop '(face font-lock-face))
        (let ((value (get-text-property pos prop)))
          (when (or (eq value face)
                    (and (listp value) (memq face value)))
            (setq found t))))
      (setq pos (1+ pos)))
    found))

(defun org-air-viewport-test-section-counts ()
  "Return alist of (BUCKET . COUNT) read from the rendered count badges.
Walks the `org-air-section' / `org-air-count-badge' text properties the
section headings carry."
  (let ((pos (point-min)) (counts ()))
    (while (< pos (point-max))
      (let ((bucket (get-text-property pos 'org-air-section)))
        (when (and bucket (not (assq bucket counts)))
          (push (cons bucket (get-text-property pos 'org-air-count-badge))
                counts)))
      (setq pos (or (next-single-property-change pos 'org-air-section)
                    (point-max))))
    (nreverse counts)))

;;;; Data-variation board — anti-hardcoding guards.
;;
;; Design's fidelity review (reviews/v0.2-fidelity-review.org, F1/F2)
;; found fixture DATA hardcoded in core (calendar mark date literals +
;; ~20 item-title branches) — invisible to the file-level render guards.
;; Countermeasure: a SECOND board whose org files are generated at test
;; time from the data constant below, with every expectation (calendar
;; day union, titles, origins, total) computed from that same constant.
;; The dates deliberately fall outside F1's [-6..+3] age window (Jun 5 =
;; 10d overdue, Jun 25 = 10d out) and the board has no item on Jun 19
;; (F1's fabricated literal), so any data hardcoding diverges.

(defconst org-air-viewport-test-alt-items
  '((:file "alpha.org" :todo "TODO" :title "Water the bonsai garden"
     :scheduled "2026-06-21 Sun" :tags ("garden"))
    (:file "alpha.org" :todo "TODO" :title "File expense reimbursement"
     :deadline "2026-06-05 Fri" :tags ("money"))
    ;; Far-future schedule: beyond the upcoming window, so it appears in
    ;; NO section — but it MUST still be marked on the calendar (the
    ;; calendar maps time, not buckets) and counted in the visible total.
    (:file "alpha.org" :todo "TODO" :title "Plan midsummer party"
     :scheduled "2026-06-25 Thu" :tags ("social") :sectionless t)
    (:file "beta.org" :todo "TODO" :title "Sharpen kitchen knives"
     :deadline "2026-06-11 Thu" :tags ("kitchen"))
    (:file "beta.org" :title "Reference clipping without dates"
     :tags ("note"))
    (:file "inbox-alt.org" :todo "TODO" :title "Sort the seed packets")
    (:file "inbox-alt.org" :title "Half-formed thought to triage"))
  "Source of truth for the data-variation board (frozen now: Mon 15 Jun 2026).
The org files AND every test expectation derive from this list.")

(defun org-air-viewport-test-alt-titles (&optional sectioned-only)
  "All titles on the data-variation board.
With SECTIONED-ONLY, exclude items annotated :sectionless (they have no
bucket row; they exist for calendar/total ground truth)."
  (mapcar (lambda (item) (plist-get item :title))
          (if sectioned-only
              (seq-remove (lambda (item) (plist-get item :sectionless))
                          org-air-viewport-test-alt-items)
            org-air-viewport-test-alt-items)))

(defun org-air-viewport-test-alt-june-days ()
  "Sorted union of June 2026 day numbers carrying a SCHEDULED/DEADLINE."
  (let (days)
    (dolist (item org-air-viewport-test-alt-items)
      (dolist (key '(:scheduled :deadline))
        (let ((date (plist-get item key)))
          (when (and date (string-match "\\`2026-06-\\([0-9]+\\)" date))
            (cl-pushnew (string-to-number (match-string 1 date)) days)))))
    (sort days #'<)))

(defun org-air-viewport-test--write-alt-board (dir)
  "Write the data-variation board org files into DIR."
  (let ((by-file (seq-group-by (lambda (item) (plist-get item :file))
                               org-air-viewport-test-alt-items)))
    (pcase-dolist (`(,file . ,items) by-file)
      (with-temp-file (expand-file-name file dir)
        (dolist (item items)
          (insert "* "
                  (if-let* ((todo (plist-get item :todo))) (concat todo " ") "")
                  (plist-get item :title))
          (when-let* ((tags (plist-get item :tags)))
            (insert "  :" (mapconcat #'identity tags ":") ":"))
          (insert "\n")
          (when-let* ((s (plist-get item :scheduled)))
            (insert (format "SCHEDULED: <%s>\n" s)))
          (when-let* ((d (plist-get item :deadline)))
            (insert (format "DEADLINE: <%s>\n" d))))))))

(defmacro org-air-viewport-test-with-alt-dashboard (width &rest body)
  "Render the dashboard at WIDTH over the GENERATED data-variation board.
Same guarantees as `org-air-viewport-test-with-dashboard' (frozen clock,
render guards, buffer killed afterwards)."
  (declare (indent 1) (debug t))
  `(let ((org-air-viewport-test--dir (make-temp-file "org-air-alt-" t)))
     (unwind-protect
         (progn
           (org-air-viewport-test--write-alt-board org-air-viewport-test--dir)
           (let ((org-air-files (list org-air-viewport-test--dir))
                 (org-air-inbox-file
                  (expand-file-name "inbox-alt.org" org-air-viewport-test--dir)))
             (org-air-viewport-test--with-frozen-now
               (unwind-protect
                   (org-air-viewport-test--with-render-guards
                     (let ((org-air-view-width ,width))
                       (org-air)
                       (let ((buf (get-buffer "*org-air*")))
                         (should buf)
                         (with-current-buffer buf
                           ,@body))))
                 (when (get-buffer "*org-air*")
                   (kill-buffer "*org-air*"))))))
       (delete-directory org-air-viewport-test--dir t))))

(defun org-air-viewport-test--glyph (name which)
  "Return the WHICH (`gui' or `tty') variant of glyph NAME from the table.
Derived from `org-air-glyphs' so glyph-default respecs (e.g. S5) never
break the parsers.  Understands the 3-tier (NAME PREFERRED SAFE ASCII)
format (gui -> PREFERRED, tty -> ASCII) and the legacy (GUI . TTY)
cons; falls back to historical defaults when the entry is absent."
  (let ((entry (and (boundp 'org-air-glyphs)
                    (cdr (assq name org-air-glyphs)))))
    (or (cond
         ((and (consp entry) (stringp (cdr entry)))   ; legacy cons
          (if (eq which 'gui) (car entry) (cdr entry)))
         ((consp entry)                               ; 3-tier list
          (if (eq which 'gui) (nth 0 entry) (car (last entry)))))
        (cdr (assq name '((calendar-item . "●") (today . "▮")))))))

(defun org-air-viewport-test-calendar-marks ()
  "Parse the rendered GUI calendar grid: return (MARKED-DAYS . TODAY-DAYS).
MARKED-DAYS are day numbers followed by the `calendar-item' glyph;
TODAY-DAYS by the `today' glyph (both read from the live glyph table,
GUI variants).  Sorted.  Digit-prefixed glyphs only, so the legend line
never matches."
  (let* ((mark (org-air-viewport-test--glyph 'calendar-item 'gui))
         (today-glyph (org-air-viewport-test--glyph 'today 'gui))
         (rx (format "\\([0-9]+\\)\\(%s\\|%s\\)"
                     (regexp-quote mark) (regexp-quote today-glyph)))
         (text (buffer-string)) (marked ()) (today ()) (pos 0))
    (while (string-match rx text pos)
      (let ((day (string-to-number (match-string 1 text)))
            (glyph (match-string 2 text)))
        (if (equal glyph mark)
            (cl-pushnew day marked)
          (cl-pushnew day today)))
      (setq pos (match-end 0)))
    (cons (sort marked #'<) (sort today #'<))))

(defun org-air-viewport-test-assert-fills-height (height)
  "Assert the buffer composes to exactly HEIGHT rows (S6 contract).
Blank padding rows count; they are full-width plain-space rows that
right-trim to empty in the line list."
  (let ((lines (org-air-viewport-test-lines)))
    (unless (= (length lines) height)
      (ert-fail (format "composed to %d rows, expected exactly %d"
                        (length lines) height)))))

(defmacro org-air-viewport-test-as-gui (&rest body)
  "Run BODY with `display-graphic-p' stubbed non-nil.
The §3 mockups use the GUI glyph set (│ ─ ‹ › ● ▮ …); --batch is a
TTY, so byte-precise mockup comparisons stub the display check while
the dedicated TTY-fallback test keeps the real (nil) answer."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'display-graphic-p)
              (lambda (&optional _display) t)))
     ,@body))

(defun org-air-viewport-test-mockup-lines (width)
  "Return the embedded §3 mockup for WIDTH as right-trimmed lines.
The mockups live in tests/fixtures/layout-mockup-WIDTH.txt, extracted
verbatim from air/v0.2/org-air-layout-design.org §3 (with the
filter/scope chips normalised to the spec'd no-filter state — the
asserted board is the unfiltered fixture set)."
  (let ((file (expand-file-name
               (cond ((stringp width) (format "layout-mockup-%s.txt" width))
                     ((consp width) (format "layout-mockup-%dx%d.txt"
                                            (car width) (cdr width)))
                     (t (format "layout-mockup-%d.txt" width)))
               org-air-test-fixture-dir)))
    (unless (file-readable-p file)
      (error "Missing mockup fixture: %s" file))
    (let* ((org-air-viewport-test--allow-mockup-read t)
           (lines (split-string
                   (with-temp-buffer
                     (insert-file-contents file)
                     (buffer-string))
                   "\n")))
      (mapcar #'string-trim-right lines))))

(defun org-air-viewport-test--drop-trailing-blanks (lines)
  "Return LINES without trailing empty strings."
  (let ((lines (copy-sequence lines)))
    (while (and lines (string-empty-p (car (last lines))))
      (setq lines (butlast lines)))
    lines))

(defun org-air-viewport-test-assert-matches-mockup (width)
  "Assert the current buffer equals the mockup for WIDTH, line for line.
WIDTH may be an integer (natural height), a (WIDTH . HEIGHT) cons, or a
string fixture suffix such as \"empty-120x50\".
Both sides are right-trimmed per line and stripped of trailing blank
lines, per design §3/§9.1.  On mismatch, fail with the first divergent
line so the impl grind gets a precise punch list."
  (let ((expected (org-air-viewport-test--drop-trailing-blanks
                   (org-air-viewport-test-mockup-lines width)))
        (actual (org-air-viewport-test--drop-trailing-blanks
                 (mapcar (lambda (line)
                           (string-trim-right (substring-no-properties line)))
                         (org-air-viewport-test-lines)))))
    (unless (equal actual expected)
      (let ((i 0))
        (while (and (< i (length expected)) (< i (length actual))
                    (equal (nth i expected) (nth i actual)))
          (setq i (1+ i)))
        (ert-fail
         (format "render diverges from the %s mockup at line %d (%d expected / %d actual lines)\nexpected: %S\nactual:   %S"
                 width (1+ i) (length expected) (length actual)
                 (nth i expected) (nth i actual)))))))

(provide 'org-air-viewport-helpers)
;;; org-air-viewport-helpers.el ends here
