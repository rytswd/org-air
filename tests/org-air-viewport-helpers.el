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
  "HEIGHT seam (S6 full-height composition; contract impl-accepted),
mirroring the width seam: nil (default) = derive ROWS from the
displaying window (`window-body-height'); integer = compose the buffer
to exactly that many rows in batch — header band first, footer band
pinned to the last row, the body band fill-padded in between.  Fill
rows are full-width: plain spaces in the STACKED layout (right-trim to
empty), but in TWO-PANE they carry the framed divider on every body
row (a framed sidebar, not a void).  Canonical definition is owned by
the implementation; declared special here for binding.")

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

(defconst org-air-project-test-frozen-mtime
  (encode-time 0 0 12 1 5 2026)
  "Frozen file modification time (2026-05-01) for project-view fixtures.
The F5 doc ↻ date is the file mtime, which a jj checkout resets to the
checkout time — pinning it keeps the project-view fixtures byte-stable.")

(defconst org-air-project-test-frozen-ctime
  (encode-time 0 0 12 1 4 2026)
  "Frozen file status-change (ctime) time (2026-04-01) for fixtures.
`org-air-project--doc-created' falls back to the file CTIME when a doc
carries no `#+created:'/`#+date:' keyword.  A jj checkout resets ctime to
the checkout time, so the rendered `created YYYY-MM-DD' and its relative
term `(Nd ago)' DRIFT day-to-day unless ctime is pinned too (the mtime
pin alone left `created' computed from REAL today).  Distinct from — and
earlier than — the mtime so created < updated reads naturally.")

(defmacro org-air-project-test--with-frozen-mtime (&rest body)
  "Run BODY with every file's modification + status-change times pinned.
Mocks `file-attributes' (a C subr, so the override survives byte-
compilation — unlike `file-attribute-modification-time', which the
compiler inlines) and rewrites BOTH the mtime slot (index 5) and the
ctime slot (index 6).  Pinning ctime keeps the project-view doc
`created' date (which falls back to ctime) byte-stable: without it the
created date and its relative `(Nd ago)' term track REAL today, so the
fixtures drift across midnight / across a jj checkout."
  (declare (indent 0) (debug t))
  `(let ((org-air-project-test--orig-file-attributes
          (symbol-function 'file-attributes)))
     (cl-letf (((symbol-function 'file-attributes)
                (lambda (file &rest args)
                  (let ((attrs (apply org-air-project-test--orig-file-attributes
                                      file args)))
                    (when (and attrs (> (length attrs) 5))
                      (setf (nth 5 attrs) org-air-project-test-frozen-mtime))
                    (when (and attrs (> (length attrs) 6))
                      (setf (nth 6 attrs) org-air-project-test-frozen-ctime))
                    attrs))))
       ,@body)))

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
         ;; T3a: the weekday row is now "%-4s"-padded (4 cols/day), so the
         ;; labels are separated by one-or-more spaces, not a single space.
         (string-match-p
          "Mo +Tu +We +Th +Fr +Sa +Su\\|Su +Mo +Tu +We +Th +Fr +Sa" text)
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
;; The dates deliberately diverge from F1's fabricated literals (Jun 5 =
;; 10d overdue, no item on Jun 19), so any data hardcoding diverges.
;; Ruling xsqrnoyn: every calendar mark is backed by a visible date-bucket
;; row, so each dated item also lands in a section (none are marked-but-
;; row-less).

(defconst org-air-viewport-test-alt-items
  '((:file "alpha.org" :todo "TODO" :title "Water the bonsai garden"
     :scheduled "2026-06-21 Sun" :tags ("garden"))
    (:file "alpha.org" :todo "TODO" :title "File expense reimbursement"
     :deadline "2026-06-05 Fri" :tags ("money"))
    ;; Window-edge schedule (Mon 22 Jun, 7d out): inside the upcoming
    ;; window, so it carries BOTH a calendar mark and an Upcoming row
    ;; (ruling xsqrnoyn — no marked-but-row-less day).
    (:file "alpha.org" :todo "TODO" :title "Plan midsummer party"
     :scheduled "2026-06-22 Mon" :tags ("social"))
    (:file "beta.org" :todo "TODO" :title "Sharpen kitchen knives"
     :deadline "2026-06-11 Thu" :tags ("kitchen"))
    (:file "beta.org" :title "Reference clipping without dates"
     :tags ("note"))
    (:file "inbox-alt.org" :todo "TODO" :title "Sort the seed packets")
    (:file "inbox-alt.org" :title "Half-formed thought to triage")
    ;; DATED INBOX item — the dual-membership surface (screenshot-3
    ;; finding 1, ruling xsqrnoyn): scheduled inside the upcoming window
    ;; (Wed 17 Jun), it appears in BOTH Inbox and Upcoming and dots its
    ;; calendar day, so calendar / Upcoming / Inbox tell one story.
    (:file "inbox-alt.org" :todo "TODO" :title "Dated inbox capture"
     :scheduled "2026-06-17 Wed"))
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

(defconst org-air-viewport-test-calendar-mark-glyphs
  ;; T3b: the calendar distinguishes three mark KINDS, each its own glyph
  ;; (GUI . TTY), hardcoded in org-air-calendar.el's `org-air-calendar--mark'
  ;; rather than `org-air-glyphs' — mirror those literals here.  Deadline
  ;; moved to ◆ (was ●); ● is now SCHEDULED; ∙ is CREATED (R33-1: the
  ;; created glyph swapped U+00B7 ambiguous -> U+2219 neutral single-width).
  '((deadline  . ("◆" . "!"))
    (scheduled . ("●" . "o"))
    (created   . ("∙" . ".")))
  "Alist of calendar mark KIND -> (GUI . TTY) glyph (T3b).")

(defun org-air-viewport-test--calendar-today-glyph (which)
  "Return the WHICH (`gui'/`tty') calendar TODAY glyph (■ / #)."
  ;; org-air-calendar.el draws today as a literal ■ / #; it matches the
  ;; `today' entry in `org-air-glyphs', so derive it from the table.
  (org-air-viewport-test--glyph 'today which))

(defun org-air-viewport-test-calendar-legend-expected (tier &optional which)
  "Return the expected calendar legend string for TIER.
TIER is `narrow' (the 95-119 / stacked form, no `created' word) or
`wide' (the >=120 form).  WHICH selects the glyph set (`gui', default,
or `tty').  R7 (design vlpzyquw) drops the today token — today is a
background highlight on its cell, so the legend needs no entry for it:
narrow ◆ due  ●  sched / wide adds ∙  created.  GLYPHS are derived from
the mark table so a respec never rots the assertion.  Format (D5c): each
GLYPH is spaced from its WORD (glyph, space, word) and entries are
joined by a wider 4-space gap."
  (let* ((which (or which 'gui))
         (g (lambda (kind)
              (let ((cell (cdr (assq kind
                                     org-air-viewport-test-calendar-mark-glyphs))))
                (if (eq which 'tty) (cdr cell) (car cell)))))
         (parts (append (list (concat (funcall g 'deadline) " due")
                              (concat (funcall g 'scheduled) " sched"))
                        (when (eq tier 'wide)
                          (list (concat (funcall g 'created) " created"))))))
    (mapconcat #'identity parts "    ")))

(defun org-air-viewport-test--face-list-at (pos)
  "Return the `face' property at POS as a list (it may be a symbol)."
  (let ((f (get-text-property pos 'face)))
    (cond ((null f) nil) ((listp f) f) (t (list f)))))

(defun org-air-viewport-test--calendar-today-days ()
  "Return day numbers whose cell carries `org-air-face-calendar-today'.
FACE-based, not glyph-based: round-7 (R7) drops the ■ today marker and
highlights the today cell with a background face instead, so the only
durable signal is the face on the day-number digits.  Works on round-6
too (the ■ cell already carries the face on its digits)."
  (let ((days ()) (pos (point-min)) (end (point-max)))
    (save-excursion
      (while (< pos end)
        (when (and (<= ?0 (or (char-after pos) 0) ?9)
                   (memq 'org-air-face-calendar-today
                         (org-air-viewport-test--face-list-at pos))
                   ;; start of the number (previous char is not a digit)
                   (not (and (> pos (point-min))
                             (<= ?0 (or (char-before pos) 0) ?9))))
          (goto-char pos)
          (when (looking-at "[0-9]+")
            (cl-pushnew (string-to-number (match-string 0)) days)))
        (setq pos (1+ pos))))
    (sort days #'<)))

(defun org-air-viewport-test-calendar-marks-by-kind (&optional which)
  "Parse the calendar grid into an alist of KIND -> sorted day numbers.
KIND is one of `deadline', `scheduled', `created' (T3b mark kinds, by
glyph) or `today' (by the `org-air-face-calendar-today' text property,
so it survives R7's removal of the ■ today glyph).  WHICH selects the
glyph set (`gui', default, or `tty').  Digit-prefixed glyphs only, so
the legend line never matches."
  (let* ((which (or which 'gui))
         (specs (mapcar (lambda (e)
                          (cons (car e)
                                (if (eq which 'tty) (cddr e) (cadr e))))
                        org-air-viewport-test-calendar-mark-glyphs))
         (alt (mapconcat (lambda (s) (regexp-quote (cdr s))) specs "\\|"))
         (rx (format "\\([0-9]+\\)\\(%s\\)" alt))
         (text (buffer-string)) (acc ()) (pos 0))
    (while (string-match rx text pos)
      (let* ((day (string-to-number (match-string 1 text)))
             (glyph (match-string 2 text))
             (kind (car (cl-find glyph specs :key #'cdr :test #'equal))))
        (when kind (cl-pushnew day (alist-get kind acc))))
      (setq pos (match-end 0)))
    (let ((today (org-air-viewport-test--calendar-today-days)))
      (when today (setf (alist-get 'today acc) today)))
    (mapcar (lambda (cell) (cons (car cell) (sort (cdr cell) #'<))) acc)))

(defun org-air-viewport-test-calendar-marks (&optional which)
  "Parse the rendered calendar grid: return (MARKED-DAYS . TODAY-DAYS).
MARKED-DAYS are day numbers carrying ANY date mark (deadline ◆,
scheduled ● or created ∙ — T3b); TODAY-DAYS carry the
`org-air-face-calendar-today' face (R7: the ■ glyph is gone, today is a
background highlight).  WHICH selects the glyph set (`gui'/`tty').  Sorted."
  (let* ((by-kind (org-air-viewport-test-calendar-marks-by-kind which))
         (today (alist-get 'today by-kind))
         (marked (cl-loop for (kind . days) in by-kind
                          unless (eq kind 'today) append days)))
    (cons (sort (delete-dups marked) #'<) (sort (copy-sequence today) #'<))))

(defun org-air-viewport-test-assert-fills-height (height)
  "Assert the buffer composes to exactly HEIGHT rows (S6 contract).
Fill rows count: full-width plain-space rows in the stacked layout
(right-trim to empty), divider-framed rows in two-pane."
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

;;;; -------------------------------------------------------------------
;;;; R17 long-Denote origin board (shared by the byte test + the regen).
;;;;
;;;; An ISOLATED mini-board (one short-named item + one ~90-char Denote
;;;; item) used by both `org-air-r17-denote-origin-byte-mockup' and
;;;; `org-air-regen--write-denote' so the asserted bytes and the blessed
;;;; fixture are produced by the SAME render path.  It is NOT part of the
;;;; GTD board *.org set, so the 25 layout mockups stay byte-identical.

(defconst org-air-viewport-test-denote-long-title
  "Reconcile the quarterly invalidation report"
  "The long-Denote item's heading (43 cols > `org-air-title-min-width').
Used by the title-floor guard so the ERT exercises the `>= title-min'
clause (the visible title must not collapse to a bare TODO ellipsis).")

(defconst org-air-viewport-test-denote-fixture-specs
  (list
   (cons "inbox.org"
         "* TODO File the receipts  :inbox:\nSCHEDULED: <2026-06-16 Tue>\n")
   (cons "20260614T170000--weekly-invalidation-rate-upgrade-with-a-long-denote-slug__work_admin.org"
         (format "* TODO %s  :work:admin:\nSCHEDULED: <2026-06-16 Tue>\n"
                 org-air-viewport-test-denote-long-title)))
  "Fixed mini-board for the R17 long-Denote origin goldens.
One short non-Denote name + one ~90-char Denote name (its de-slugged
title exceeds the origin cap).  Both carry tags so the right cluster is
wide enough that the title-min fit pass genuinely shrinks the origin at
the narrow tier.  Each value is the full org body.")

(defun org-air-viewport-test-denote-board-lines (width)
  "Render the R17 isolated long-Denote board at WIDTH; return trimmed lines.
Frozen clock, GUI glyphs and the anti-tautology render guards are all
active, so the bytes come from the REAL renderer.  Right-trimmed, with
trailing blank lines dropped (the regen + byte-test contract)."
  (let ((dir (make-temp-file "org-air-r17-denote-" t)))
    (unwind-protect
        (progn
          (pcase-dolist (`(,name . ,content)
                         org-air-viewport-test-denote-fixture-specs)
            (with-temp-file (expand-file-name name dir) (insert content)))
          (let ((org-air-files (directory-files dir t "\\.org\\'"))
                (org-air-inbox-file (expand-file-name "inbox.org" dir)))
            (org-air-viewport-test-as-gui
              (org-air-viewport-test--with-frozen-now
                (org-air-viewport-test--with-render-guards
                  (let ((org-air-view-width width)
                        ;; R30-3: the denote board is the ORIGIN-ON golden —
                        ;; its whole purpose is to pin the long-Denote origin
                        ;; de-slug/cap, so it renders with the origin column
                        ;; shown (the default board hides it now).
                        (org-air-show-origin t))
                    (org-air)
                    (unwind-protect
                        (with-current-buffer "*org-air*"
                          (org-air-viewport-test--drop-trailing-blanks
                           (mapcar (lambda (l)
                                     (string-trim-right
                                      (substring-no-properties l)))
                                   (org-air-viewport-test-lines))))
                      (when (get-buffer "*org-air*")
                        (kill-buffer "*org-air*")))))))))
      (delete-directory dir t))))

(provide 'org-air-viewport-helpers)
;;; org-air-viewport-helpers.el ends here
