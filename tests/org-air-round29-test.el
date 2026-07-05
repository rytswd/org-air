;;; org-air-round29-test.el --- executing ERTs for v0.5 round-29 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.5 round-29 (air/v0.5/org-air-round29-design.org).
;;
;;   R29-1  FRINGE-LESS GUI X-OVERFLOW — the R27-2 host-width seam (and the
;;          rail's own paint + the doc-session host) measured raw
;;          `window-body-width' while every other live path measured
;;          `org-air-layout--usable-columns' (GUI: `window-max-chars-per-line',
;;          which reserves the continuation-glyph column when the right
;;          fringe is absent).  In a fringe-less GUI the two disagree by ONE,
;;          so with the rail popped every composed line ran one column past
;;          the window edge.  The fringe-less GUI is simulated per the Emacs
;;          contract: `display-graphic-p' -> t and
;;          `window-max-chars-per-line' -> body - 1 (zero right fringe), with
;;          LIVE windows and NO width seam — exactly the round's measured
;;          reproduction harness.
;;
;;   R29-2  EVIL CURSOR SNAP — command-agnostic, line-motion-gated title
;;          snap; j/k in the shared core map; entry/restore normalize; late
;;          evil registration replay.  Driven with the REAL evil from .deps.
;;
;; Batch contract: BOTH items are byte-invisible — every golden identical
;; (the batch width seams bypass the host-width helper; in a TTY/batch frame
;; `org-air-layout--usable-columns' == `window-body-width').

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-project-test)            ; project fixture root
(require 'org-air-viewport-helpers)
(require 'org-air-round27-test)            ; live-board/-project harness

(when (locate-library "org-air")
  (require 'org-air))

;;;; =====================================================================
;;;; R29-1 — fringe-less GUI simulation harness.
;;;; =====================================================================

(defmacro org-air-r29--with-fringeless-gui (&rest body)
  "Run BODY under a simulated FRINGE-LESS GUI (R29-1 reproduction harness).
Two stubs, per the documented Emacs contract: `display-graphic-p' -> t
\(so `org-air-layout--usable-columns' takes its graphical branch) and
`window-max-chars-per-line' -> `window-body-width' - 1 (a zero right
fringe reserves the continuation-glyph column).  Live windows, no width
seam — exactly the round's measured reproduction."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'display-graphic-p)
              (lambda (&optional _display) t))
             ((symbol-function 'window-max-chars-per-line)
              (lambda (&optional window _face)
                (1- (window-body-width (or window (selected-window)))))))
     ,@body))

(defmacro org-air-r29--with-fringed-gui (&rest body)
  "Run BODY under a simulated GUI WITH fringes (R29-1 no-op variant).
`display-graphic-p' -> t but `window-max-chars-per-line' returns the full
body width (fringes host the continuation glyph), so usable == body and
the R29-1 fix must change nothing."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'display-graphic-p)
              (lambda (&optional _display) t))
             ((symbol-function 'window-max-chars-per-line)
              (lambda (&optional window _face)
                (window-body-width (or window (selected-window))))))
     ,@body))

(defun org-air-r29--assert-lines-fit (win)
  "Assert every line of WIN's buffer fits WIN's usable columns."
  (let ((usable (org-air-layout--usable-columns win)))
    (with-current-buffer (window-buffer win)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let ((w (string-width
                    (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position)))))
            (unless (<= w usable)
              (ert-fail (format "line %d is %d cols wide > usable %d: %S"
                                (line-number-at-pos) w usable
                                (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position))))))
          (forward-line 1))))))

(defun org-air-r29--header-widths ()
  "Return (RAW . TRIMMED) display widths of the first buffer line."
  (save-excursion
    (goto-char (point-min))
    (let ((line (buffer-substring-no-properties (point) (line-end-position))))
      (cons (string-width line)
            (string-width (string-trim-right line))))))

;;;; =====================================================================
;;;; R29-1 — no composed line exceeds the usable columns; header contract.
;;;; =====================================================================

(ert-deftest org-air-r29-1-fringeless-no-line-exceeds-usable ()
  "Fringe-less GUI, all three rail modes, odd AND even host widths: after
render + refresh, every buffer line's `string-width' is <= the displaying
window's `org-air-layout--usable-columns'.  Trunk FAILED under the popped
side-window rail (every line one column past the usable area: 51 > 50)."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-board
    (org-air-r29--with-fringeless-gui
      ;; --- SIDE-WINDOW (popped rail): the trunk overflow. ---
      (org-air-r27--pop-rail)
      (org-air-view--refresh-current)
      (should (eq org-air-view--orientation 'side-window))
      (let ((bwin (get-buffer-window (current-buffer))))
        (should (window-live-p bwin))
        (should (eql org-air-view--rendered-width
                     (org-air-layout--usable-columns bwin)))
        (org-air-r29--assert-lines-fit bwin))
      ;; --- INLINE (two-pane) at the full frame width. ---
      (setq-local org-air-view--rail-popped-out nil)
      (org-air-rail--hide (current-buffer))
      (org-air-view--refresh-current)
      (should (eq org-air-view--orientation 'two-pane))
      (org-air-r29--assert-lines-fit (get-buffer-window (current-buffer)))
      ;; --- BOARD-ONLY at MULTIPLE widths, odd and even, via splits. ---
      (let ((org-air-rail-min-width 500)   ; force board-only at any width
            (widths nil))
        (dolist (size '(29 30))
          (let ((other (split-window (get-buffer-window (current-buffer))
                                     (- size) 'right)))
            (unwind-protect
                (let ((bwin (get-buffer-window (current-buffer))))
                  (with-selected-window bwin
                    (org-air-view--refresh-current))
                  (should (eq org-air-view--orientation 'board-only))
                  (push (org-air-layout--usable-columns bwin) widths)
                  (org-air-r29--assert-lines-fit bwin))
              (delete-window other))))
        ;; the split pair really exercised BOTH parities.
        (should (cl-find-if #'cl-oddp widths))
        (should (cl-find-if #'cl-evenp widths))))))

(ert-deftest org-air-r29-1-header-ends-at-contract-column ()
  "Fringe-less GUI: the S7 header contract holds where the user looks —
the right-trimmed header width equals compose-width - 1, the compose
width equals the window's USABLE columns, and the header's final column
is blank (the S7 margin column sits INSIDE the displayable area).  Trunk
FAILED under side-window: compose width = usable + 1, so the status glyph
sat flush at the window edge."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-board
    (org-air-r29--with-fringeless-gui
      (org-air-r27--pop-rail)
      (org-air-view--refresh-current)
      (should (eq org-air-view--orientation 'side-window))
      (let* ((bwin (get-buffer-window (current-buffer)))
             (usable (org-air-layout--usable-columns bwin))
             (hw (org-air-r29--header-widths)))
        ;; compose width == usable columns (the R29-1 fix).
        (should (eql org-air-view--rendered-width usable))
        ;; status ends at W-1 of the compose width (S7, unchanged).
        (should (= (cdr hw) (1- org-air-view--rendered-width)))
        ;; the final column of the header is blank (right-trimmed line is
        ;; strictly narrower than the compose width).
        (should (< (car hw) org-air-view--rendered-width)))
      ;; the INLINE header keeps the same contract (regression guard).
      (setq-local org-air-view--rail-popped-out nil)
      (org-air-rail--hide (current-buffer))
      (org-air-view--refresh-current)
      (let* ((bwin (get-buffer-window (current-buffer)))
             (usable (org-air-layout--usable-columns bwin))
             (hw (org-air-r29--header-widths)))
        (should (eql org-air-view--rendered-width usable))
        (should (= (cdr hw) (1- org-air-view--rendered-width)))))))

(ert-deftest org-air-r29-1-rail-buffer-fits ()
  "Fringe-less GUI: the popped rail buffer's OWN lines fit its side
window's usable columns.  Trunk FAILED (composed at the raw body width:
28 > 27)."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-board
    (org-air-r29--with-fringeless-gui
      (org-air-r27--pop-rail)
      (org-air-view--refresh-current)
      (let ((rwin (org-air-rail--side-window)))
        (should (window-live-p rwin))
        (should (eq (window-buffer rwin)
                    (get-buffer org-air-rail-buffer-name)))
        (org-air-r29--assert-lines-fit rwin)))))

(ert-deftest org-air-r29-1-project-fits-fringeless ()
  "Fringe-less GUI: the PROJECT view (default side-window placement,
R26-5) composes at its host window's usable columns — every line fits,
every doc row is composed to exactly the rendered width, and
`org-air-project--rendered-width' equals the usable columns.  Trunk
FAILED (the shared host-width helper measured raw body width)."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-project
    (should (org-air-rail--popped-p))
    (org-air-r29--with-fringeless-gui
      (org-air-view--refresh-current)
      (let* ((pwin (get-buffer-window (current-buffer)))
             (usable (org-air-layout--usable-columns pwin)))
        (should (window-live-p pwin))
        (should (eql org-air-project--rendered-width usable))
        (org-air-r29--assert-lines-fit pwin)
        (dolist (bol (org-air-r27--doc-row-bols))
          (save-excursion
            (goto-char bol)
            (should (= (string-width
                        (buffer-substring-no-properties
                         bol (line-end-position)))
                       org-air-project--rendered-width))))))))

(ert-deftest org-air-r29-1-fringed-gui-unchanged ()
  "A GUI WITH fringes has `window-max-chars-per-line' == body width, so
the R29-1 fix is a no-op there: with the rail popped, the rendered width
and the longest composed line are exactly the values of the plain
\(TTY-measured) render in the same window geometry."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-board
    ;; plain render (TTY tier: usable == body) — the trunk values.
    (org-air-r27--pop-rail)
    (org-air-view--refresh-current)
    (let ((plain-width org-air-view--rendered-width)
          (plain-longest (org-air-r27--longest-line))
          (bwin (get-buffer-window (current-buffer))))
      (should (eql plain-width (window-body-width bwin)))
      ;; fringed GUI: usable == body, so nothing moves.
      (org-air-r29--with-fringed-gui
        (org-air-view--refresh-current)
        (should (eql org-air-view--rendered-width plain-width))
        (should (= (org-air-r27--longest-line) plain-longest))
        (should (eql org-air-view--rendered-width
                     (window-body-width
                      (get-buffer-window (current-buffer)))))))))

(provide 'org-air-round29-test)
;;; org-air-round29-test.el ends here
