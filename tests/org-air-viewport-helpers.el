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
  "Proposed width seam: explicit render width for the org-air dashboard.
Declared in the test harness so suites can bind it; the implementation
owns the canonical definition.")

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

(defmacro org-air-viewport-test-with-dashboard (width &rest body)
  "Render the dashboard at WIDTH over the fixtures; run BODY in its buffer.
WIDTH is bound to `org-air-view-width' for the render.  The clock is
frozen to `org-air-test-now'.  The buffer is killed afterwards so the
per-buffer item cache never leaks between renders."
  (declare (indent 1) (debug t))
  `(org-air-test-with-fixtures
     (org-air-viewport-test--with-frozen-now
       (unwind-protect
           (let ((org-air-view-width ,width))
             (org-air)
             (let ((buf (get-buffer "*org-air*")))
               (should buf)
               (with-current-buffer buf
                 ,@body)))
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
                 (let ((org-air-view-width ,width))
                   (org-air)
                   (let ((buf (get-buffer "*org-air*")))
                     (should buf)
                     (with-current-buffer buf
                       ,@body)))
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

(defun org-air-viewport-test-assert-no-trailing-whitespace ()
  "Assert no buffer line ends in literal spaces or tabs.
Full-width composition must pad with propertized fill characters or
faces, not bare trailing blanks, so alignment failures stay visible."
  (let ((bad (org-air-viewport-test-trailing-whitespace-lines)))
    (when bad
      (ert-fail
       (format "%d line(s) end in whitespace; first offenders:\n%s"
               (length bad)
               (mapconcat
                (pcase-lambda (`(,lineno . ,line))
                  (format "  line %3d: %S"
                          lineno (substring-no-properties line)))
                (seq-take bad 8)
                "\n"))))))

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

(defun org-air-viewport-test-banner-item-count ()
  "Return N from the banner's \"N items\" status, or nil.
Walks the `header-line-format' construct strings directly —
`format-mode-line' returns \"\" in --batch."
  (let ((parts ()))
    (cl-labels ((walk (x)
                  (cond ((stringp x) (push x parts))
                        ((consp x) (walk (car x)) (walk (cdr x))))))
      (walk header-line-format))
    (let ((text (mapconcat #'identity (nreverse parts) "")))
      (when (string-match "\\([0-9]+\\) items" text)
        (string-to-number (match-string 1 text))))))

(provide 'org-air-viewport-helpers)
;;; org-air-viewport-helpers.el ends here
