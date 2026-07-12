;;; org-air-round44-test.el --- executing ERT for v0.5 round-44 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERT for v0.5 round-44 (air/v0.5/org-air-round44-design.org):
;; the board<->rail pane divider is PIXEL-broken though the BUFFER is column-
;; exact — an SVG pill split-brain.  R43 landed char-column 146 on every board
;; row, yet the user still SEES a zig-zag divider on the GUI: character
;; column 146 lands at a DIFFERENT pixel-X on different rows.
;;
;; ROOT (R44-1, confirmed + quantified): a board svg pill spanning N text
;; cells is baked as a `display' image of width N * `org-air-view--pill-char-w'
;; where that metric is seeded from `org-air-view--char-dimensions', which —
;; on the first / async / not-yet-displayed render (no live board window) —
;; FELL BACK to `frame-char-width'.  The redisplay engine advances the divider
;; column and the plain text at the window's REAL default-face advance
;; (`window-font-width').  On a font where the two disagree (here Wtext=9 vs
;; frame-char-width=8) every pill is NARROWER than its cells by
;; N*(Wtext-cw)=N*1px, so each pill drags the `|' glyph LEFT and col 146 lands
;; anywhere across a ~20px swing — the zig-zag.  Character goldens are blind to
;; this: `display'/image props are golden-invisible.
;;
;; R44-2 fix (Hybrid C, B load-bearing):
;;   B  `org-air-view--char-dimensions' resolves the metric off the DESTINATION
;;      window (window-font-width) when no live board window exists yet, NEVER
;;      the frame-char-width fallback while the two disagree — so a pill is
;;      exactly N * Wtext px and stops moving the divider.
;;   A  the LIVE finalize tail pins the divider column with
;;      `display (space :align-to (COL . width))' on the leading space before
;;      the `|' so it lands at ONE font-relative pixel-X on every board row.
;;
;; The DECISIVE fences (each REVERT-FAILS):
;;
;;   1  PILL SIZED AT THE DIVIDER METRIC.  With the split-brain reproduced
;;      (display-graphic-p t, window-font-width 9, frame-char-width 8) and NO
;;      live board window, `org-air-view--char-dimensions' returns Wtext=9 (the
;;      divider advance), so every pill image :width == ncols*9.  Reverted
;;      (frame-char-width fallback) it returns 8 and bakes ncols*8 — off by the
;;      measured -3/-6/-11 px.
;;
;;   2  DRIFT = 0.  box-w - ncols*Wtext == 0 for state badge, date pill AND tag
;;      pill under the fix; reverted it is negative (the pill under-fills).
;;
;;   3  DIVIDER PINNED ON EVERY BOARD ROW.  On the LIVE render every pane-
;;      divider row carries `display (space :align-to (COL . width))'
;;      immediately before the `|' at the shared column, and the align-to value
;;      is IDENTICAL on every row (one pixel-X, no swing).  Reverted (no pin)
;;      the spec is absent on every row.
;;
;;   4  GOLDENS / HEADER untouched.  `string-width' of every divider row is
;;      still the full render width (char column 146 preserved, pixel-only
;;      change); the banner + header rule carry NO align-to and NO divider.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)

(when (locate-library "org-air")
  (require 'org-air))

;;;; -------------------------------------------------------------------
;;;; The split-brain reproduced: Wtext (divider advance) != frame-char-width,
;;;; and NO live window shows the board buffer (the async / first render).
;;;; -------------------------------------------------------------------

(defconst org-air-r44--wtext 9  "Stubbed window-font-width (the divider advance).")
(defconst org-air-r44--wtext-h 18 "Stubbed window-font-height.")
(defconst org-air-r44--frame-w 8 "Stubbed frame-char-width (the WRONG fallback).")
(defconst org-air-r44--frame-h 16 "Stubbed frame-char-height.")

(defmacro org-air-r44--with-split-brain (&rest body)
  "Run BODY with the pixel split-brain reproduced and NO live board window.
`display-graphic-p' is t, `window-font-width'/`-height' are the REAL
default-face advance the divider column uses (9/18), `frame-char-width'/
`-height' are the DIFFERENT rounded frame metric (8/16), and
`get-buffer-window' returns nil so `org-air-view--char-dimensions' takes
its no-live-window path (the async first render) — exactly the state that
baked the wrong metric pre-R44."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
             ((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
             ((symbol-function 'window-font-width)
              (lambda (&rest _) org-air-r44--wtext))
             ((symbol-function 'window-font-height)
              (lambda (&rest _) org-air-r44--wtext-h))
             ((symbol-function 'frame-char-width)
              (lambda (&rest _) org-air-r44--frame-w))
             ((symbol-function 'frame-char-height)
              (lambda (&rest _) org-air-r44--frame-h)))
     ,@body))

(defun org-air-r44--pill-width (text &optional label)
  "Return the :width of the svg pill for TEXT (LABEL overrides the glyph).
Sizes the pill EXACTLY as `org-air-view--render' does: the char metric is
seeded from `org-air-view--char-dimensions' (the live/destination window
font), then bound into `org-air-view--pill-char-w/-h'."
  (let* ((dims (org-air-view--char-dimensions))
         (org-air-view--pill-char-w (car dims))
         (org-air-view--pill-char-h (cdr dims))
         (org-air-view--pill-style-sig
          (list org-air-pill-pad-cols org-air-pill-radius
                org-air-pill-fill-alpha org-air-pill-font-scale
                org-air-pill-border-opacity org-air-pill-vinset))
         (pill (org-air-view--svg-pillify text 'org-air-face-faded
                                          :label label))
         (img (get-text-property 0 'display pill)))
    (and img (eq (car-safe img) 'image) (image-property img :width))))

;;;; -------------------------------------------------------------------
;;;; 1. Every pill image is a whole number of char cells at the DIVIDER metric.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r44-char-dimensions-uses-divider-metric ()
  "R44-2 Fix B: with no live board window, `org-air-view--char-dimensions'
resolves the metric off the destination window (window-font-width, the
SAME advance the divider column uses), NEVER the frame-char-width fallback.

REVERT-FAILS: the pre-R44 fallback returns `frame-char-width' (8), so the
metric is 8, not the divider's 9 — the split-brain."
  (skip-unless (locate-library "org-air"))
  (should (fboundp 'org-air-view--char-dimensions))
  (org-air-r44--with-split-brain
    (let ((dims (org-air-view--char-dimensions)))
      (ert-info ((format "dims=%S" dims))
        ;; the char advance is the DIVIDER's real metric, not the frame one.
        (should (= (car dims) org-air-r44--wtext))
        (should (= (cdr dims) org-air-r44--wtext-h))
        (should-not (= (car dims) org-air-r44--frame-w))))))

(ert-deftest org-air-r44-pill-width-is-ncols-times-wtext ()
  "R44-2 Fix B / fence 1: every board pill image is a whole number of char
cells at the DIVIDER metric — :width == ncols * Wtext — for the state
badge, the date pills AND the tag pill.

REVERT-FAILS: the pre-R44 metric bakes ncols * frame-char-width (ncols*8),
off by the measured -3 (badge) / -5 (Today) / -6 (#infra) / -11 (OVERDUE
12d) px."
  (skip-unless (locate-library "org-air"))
  (should (fboundp 'org-air-view--svg-pillify))
  (org-air-r44--with-split-brain
    ;; (text . drawn-label): the badge draws a single letter but its cell
    ;; text (the pixel-lock box) stays the 3-col `[D]' token, per R25-2.
    (dolist (case '(("[D]" . "D") ("[N]" . "N") ("[W]" . "W")
                    ("OVERDUE 12d" . nil) ("Today" . nil) ("#infra" . nil)))
      (let* ((text (car case))
             (label (cdr case))
             (ncols (string-width text))
             (width (org-air-r44--pill-width text label)))
        (ert-info ((format "text=%S ncols=%d width=%S" text ncols width))
          (should (integerp width))
          ;; a whole number of cells AT THE DIVIDER METRIC.
          (should (= width (* ncols org-air-r44--wtext)))
          (should (zerop (% width org-air-r44--wtext)))
          ;; and NOT the split-brain frame metric.
          (should-not (= width (* ncols org-air-r44--frame-w))))))))

(ert-deftest org-air-r44-pill-drift-is-zero ()
  "R44-2 fence 2 (drift = 0): for each pill the drift box-w - ncols*Wtext is
EXACTLY 0 under the fix; reverted it is the measured NEGATIVE deficit
(pill NARROWER than its cells), which is precisely what dragged the divider
left row-by-row."
  (skip-unless (locate-library "org-air"))
  (org-air-r44--with-split-brain
    (dolist (case '(("[D]" . "D") ("OVERDUE 12d" . nil)
                    ("Today" . nil) ("#infra" . nil)))
      (let* ((text (car case))
             (ncols (string-width text))
             (box-w (org-air-r44--pill-width text (cdr case)))
             (drift (- box-w (* ncols org-air-r44--wtext)))
             ;; the reverted metric would have produced this NEGATIVE drift.
             (reverted-drift (- (* ncols org-air-r44--frame-w)
                                (* ncols org-air-r44--wtext))))
        (ert-info ((format "text=%S drift=%d reverted=%d"
                           text drift reverted-drift))
          (should (= drift 0))
          (should (< reverted-drift 0)))))))

;;;; -------------------------------------------------------------------
;;;; 3. The divider is pixel-pinned on EVERY board row (LIVE render).
;;;; -------------------------------------------------------------------

(defconst org-air-r44--item-count 160
  "Item count for the synthetic tall board (board rows >> rail rows).")

(defmacro org-air-r44--with-tall-two-pane (&rest body)
  "Render a TALL two-pane board through the LIVE finalize tail; run BODY.
Mirrors the R43 harness: GUI stub, clock frozen, anti-tautology guards,
sections EXPANDED, `org-air-view-width' NIL (the LIVE
`org-air-view--finalize-buffer-lines' tail), 191x60 so two-pane engages and
the board is far taller than the rail.  Items carry a tag so the board has
badge + date + tag pills before the divider (the drift payload)."
  (declare (indent 0) (debug t))
  `(let ((org-air-r44--dir (make-temp-file "org-air-r44-" t)))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name "many.org" org-air-r44--dir)
             (dotimes (i org-air-r44--item-count)
               (insert (format "* TODO Task number %d needing attention   :infra:\nSCHEDULED: <2026-06-1%d>\n"
                               i (% i 9)))))
           (with-temp-file (expand-file-name "inbox.org" org-air-r44--dir)
             (insert "* TODO Inbox capture\n"))
           (let ((org-air-files (directory-files org-air-r44--dir t "\\.org\\'"))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-r44--dir)))
             (org-air-viewport-test--with-frozen-now
               (org-air-viewport-test-as-gui
                 (org-air-viewport-test--with-render-guards
                   (cl-letf (((symbol-function 'org-air-layout-current-width)
                              (lambda (&rest _) 191))
                             ((symbol-function 'org-air-layout-current-height)
                              (lambda (&rest _) 60)))
                     (let ((org-air-view-width nil)
                           (org-air-view-height nil)
                           (org-air-view--expanded-sections
                            '(inbox attention upcoming high-priority stale)))
                       (org-air)
                       (unwind-protect
                           (with-current-buffer "*org-air*" ,@body)
                         (when (get-buffer "*org-air*")
                           (kill-buffer "*org-air*"))))))))))
       (delete-directory org-air-r44--dir t))))

(defun org-air-r44--divider-align-to (line col)
  "Return the `:align-to' spec on the leading space before the `|' at COL.
Nil when the pane-divider row is not pinned (the reverted shape)."
  (let ((idx (org-air-view--pane-divider-glyph-index line col)))
    (and idx (> idx 0)
         (let ((d (get-text-property (1- idx) 'display line)))
           (and (consp d) (eq (car d) 'space)
                (plist-get (cdr d) :align-to))))))

(ert-deftest org-air-r44-divider-pinned-every-row ()
  "R44-2 Fix A / fence 3: on the LIVE render EVERY pane-divider row carries
`display (space :align-to (COL . width))' on the leading space immediately
before the `|', and that align-to value is IDENTICAL on every divider row
\(one pixel-X — no zig-zag).

REVERT-FAILS: without the pin no divider row carries an `:align-to' spec at
all, so the divider's pixel-X is left to the per-row pill sum."
  (skip-unless (locate-library "org-air"))
  (should (fboundp 'org-air-view--pin-pane-divider))
  (org-air-r44--with-tall-two-pane
    (let* ((col org-air-view--pane-divider-col)
           (rows (cl-remove-if-not
                  (lambda (l) (org-air-view--pane-divider-line-p l col))
                  (org-air-viewport-test-lines)))
           (specs '())
           (unpinned 0))
      (should (eq org-air-view--orientation 'two-pane))
      (should (integerp col))
      (should (>= (length rows) 40))
      (dolist (line rows)
        (let ((spec (org-air-r44--divider-align-to line col)))
          (if spec (push spec specs) (setq unpinned (1+ unpinned)))))
      (ert-info ((format "rows=%d unpinned=%d distinct=%d"
                         (length rows) unpinned
                         (length (delete-dups (copy-sequence specs)))))
        ;; every divider row is pinned …
        (should (= unpinned 0))
        (should (= (length specs) (length rows)))
        ;; … the pin measures the SHARED divider column in face-width units
        ;; (font-relative, so it lands at the exact text stop) …
        (should (equal (car specs) (cons col 'width)))
        ;; … and it is the SAME value on every row -> ONE pixel-X (no swing).
        (should (= 1 (length (delete-dups (copy-sequence specs)))))))))

;;;; -------------------------------------------------------------------
;;;; 4. Goldens / header untouched (char column preserved; pixel-only).
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r44-divider-rows-full-width-header-clean ()
  "R44-2 fence 4: the pixel pin is golden-invisible — every pane-divider row
still measures to the full render width (char column preserved), and the
header banner + rule carry NO align-to and NO pane divider (the pin is
scoped to two-pane divider rows only)."
  (skip-unless (locate-library "org-air"))
  (org-air-r44--with-tall-two-pane
    (let* ((col org-air-view--pane-divider-col)
           (w (org-air-view--render-width))
           (lines (org-air-viewport-test-lines))
           (banner (nth 0 lines))
           (rule (nth 1 lines))
           (rows (cl-remove-if-not
                  (lambda (l) (org-air-view--pane-divider-line-p l col))
                  lines)))
      (should (>= (length rows) 40))
      ;; char column 146 preserved: every divider row is full render width
      ;; (the `:align-to' pin sits on an existing space; string-width is
      ;; blind to the display property).
      (dolist (line rows)
        (should (= (string-width line) w)))
      ;; header carries NO pane divider and NO align-to pin.
      (should-not (org-air-view--pane-divider-line-p banner col))
      (should-not (org-air-view--pane-divider-line-p rule col))
      (should-not (org-air-r44--divider-align-to banner col))
      (should-not (org-air-r44--divider-align-to rule col))
      (should-not (org-air-viewport-test--align-to-p banner))
      (should-not (org-air-viewport-test--align-to-p rule)))))

(provide 'org-air-round44-test)
;;; org-air-round44-test.el ends here
