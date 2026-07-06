;;; org-air-round31-test.el --- executing ERTs for v0.5 round-31 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.5 round-31 (air/v0.5/org-air-round31-design.org).
;;
;;   R31-1  INLINE two-pane + header x-overflow (the mode R29-1 was said to
;;          miss).  ROOT-CAUSED on this checkout: the INLINE two-pane and the
;;          header are ALREADY correct — every composed line ==
;;          `org-air-view--render-width' = `org-air-layout--usable-columns'
;;          (the R29-1 primitive the inline path already flows through), and
;;          the divider column is constant on every row (no alignment bug).
;;          What R31-1 ships:
;;            - SEAM A (byte-invisible): the LAST raw-column term,
;;              `org-air-layout-current-width''s `(frame-width)' fallback, now
;;              routes through `org-air-layout--usable-frame-columns' so a
;;              fringe-less graphic frame reserves the continuation-glyph
;;              column there too (TTY/batch reserve none -> byte-identical).
;;            - the INLINE ERT matrix (header at the contract column; every
;;              two-pane row == rendered width; divider column identical),
;;              extending R29-1's matrix, LOCKING the already-correct
;;              behaviour so a future refactor cannot silently drift.
;;            - a SEAM B glyph-advance guard: every status/separator glyph is
;;              single-`char-width', and the header composes self-consistently
;;              even when `char-width-table' marks the ambiguous set WIDE.
;;
;;   R31-2  No separate column-alignment bug — folded into R31-1 (the divider
;;          column is constant on every composed row, measured; locked by
;;          `org-air-r31-1-inline-rows-fit-and-align' / `-inline-width-sweep').
;;
;; Batch contract: byte-INVISIBLE.  The one source change is reached only
;; when there is no live window/selected-window, and in a TTY/batch frame
;; `org-air-layout--usable-frame-columns' == `frame-width'; every golden and
;; every live-terminal value is unchanged (NO manifest, NO re-bless).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-project-test)            ; project fixture root
(require 'org-air-viewport-helpers)
(require 'org-air-round27-test)            ; live-board/-project harness
(require 'org-air-round29-test)            ; fringe-less / fringed GUI sim macros

(when (locate-library "org-air")
  (require 'org-air))

;;;; =====================================================================
;;;; Shared helpers.
;;;; =====================================================================

(defconst org-air-r31--ambiguous-glyphs
  '(clear more sep-dot sort-asc sort-desc arrow updated)
  "Status/separator/legend glyph names whose PREFERRED forms are the
East-Asian *Ambiguous-width* set the round audits (Seam B): ✕ … · ↑ ↓ → ↻.")

(defun org-air-r31--preferred-glyph (name)
  "Return the PREFERRED (GUI) glyph string for NAME from the glyph table."
  (car (alist-get name org-air-glyphs)))

(defun org-air-r31--face-carries-p (face)
  "Non-nil when FACE is (or contains) `org-air-face-pane-border'."
  (or (eq face 'org-air-face-pane-border)
      (and (listp face) (memq 'org-air-face-pane-border face))))

(defun org-air-r31--line-divider-col ()
  "Return the display column of the pane-border │ on point's line, or nil.
Scans for the `vrule' glyph carrying `org-air-face-pane-border' (the
divider cell) so item/rail content that happens to contain a │ is never
mistaken for the divider."
  (save-excursion
    (let ((beg (line-beginning-position))
          (end (line-end-position))
          (vrule (org-air-view--glyph 'vrule))
          col)
      (goto-char beg)
      (while (and (not col) (< (point) end))
        (when (and (equal (char-to-string (char-after)) vrule)
                   (org-air-r31--face-carries-p (get-text-property (point) 'face)))
          (setq col (string-width (buffer-substring-no-properties beg (point)))))
        (forward-char 1))
      col)))

(defun org-air-r31--string-divider-col (s)
  "Return the display column of the pane-border │ in composed row S, or nil."
  (let ((vrule (org-air-view--glyph 'vrule))
        (i 0) (n (length s)) col)
    (while (and (not col) (< i n))
      (when (and (equal (char-to-string (aref s i)) vrule)
                 (org-air-r31--face-carries-p (get-text-property i 'face s)))
        (setq col (string-width (substring-no-properties s 0 i))))
      (setq i (1+ i)))
    col))

(defun org-air-r31--render-inline ()
  "Drop the rail inline (two-pane) on the current live board + refresh."
  (setq-local org-air-view--rail-popped-out nil)
  (org-air-rail--hide (current-buffer))
  (org-air-view--refresh-current)
  (should (eq org-air-view--orientation 'two-pane)))

(defun org-air-r31--banner-width (items)
  "Compose `org-air-view--insert-banner' for ITEMS in a temp buffer.
Returns (RAW . TRIMMED) display widths of the header line, measured with
the AMBIENT `string-width' (so a WIDE `char-width-table' in scope is
honoured — the Seam-B self-consistency probe)."
  (with-temp-buffer
    (org-air-view--insert-banner items)
    (goto-char (point-min))
    (let ((line (buffer-substring-no-properties (point) (line-end-position))))
      (cons (string-width line)
            (string-width (string-trim-right line))))))

;;;; =====================================================================
;;;; R31-1 #1 — INLINE header ends at the contract column (fringe-less).
;;;; =====================================================================

(ert-deftest org-air-r31-1-inline-header-ends-at-contract-column ()
  "INLINE two-pane (rail NOT popped), fringe-less GUI, at odd AND even
host widths: the compose width == the window's usable columns, the
right-trimmed header width == compose-width - 1 (S7), and the header's
final column is blank (the S7 margin sits INSIDE the displayable area, so
a zero-fringe GUI never draws a continuation glyph over the status).
Locks the header contract for the inline mode the user dogfooded."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-board
    (org-air-r29--with-fringeless-gui
      ;; drive BOTH parities via two narrow right-splits that still leave
      ;; the board window WIDE enough for two-pane (usable >= boundary):
      ;; a 15- and a 16-col right window yield adjacent-parity usables.
      (let ((widths nil))
        (dolist (trim '(15 16))
          (let ((other (split-window (get-buffer-window (current-buffer))
                                     (- trim) 'right)))
            (unwind-protect
                (let ((bwin (get-buffer-window (current-buffer))))
                  (with-selected-window bwin
                    (org-air-r31--render-inline))
                  (let ((usable (org-air-layout--usable-columns bwin))
                        (hw (org-air-r29--header-widths)))
                    (push usable widths)
                    ;; compose width IS the usable columns (R29-1 primitive).
                    (should (eql org-air-view--rendered-width usable))
                    ;; status ends at W-1 of the compose width (S7).
                    (should (= (cdr hw) (1- org-air-view--rendered-width)))
                    ;; the final column of the header is blank.
                    (should (< (car hw) org-air-view--rendered-width))))
              (when (window-live-p other) (delete-window other)))))
        ;; the split pair really exercised BOTH parities.
        (should (cl-find-if #'cl-oddp widths))
        (should (cl-find-if #'cl-evenp widths))))))

;;;; =====================================================================
;;;; R31-1 #2 — every INLINE row fits usable, == rendered width, and the
;;;;            divider column is identical on every row (alignment lock).
;;;; =====================================================================

(ert-deftest org-air-r31-1-inline-rows-fit-and-align ()
  "INLINE two-pane, fringe-less GUI: EVERY composed row's `string-width'
is <= usable (the user-visible right edge is honoured — live insertion
right-trims trailing pad, so the EXACT == rendered-width invariant is
locked by the pure sweep in `org-air-r31-1-inline-width-sweep'), and the
pane divider column is IDENTICAL on every row that carries it (the R31-2
alignment lock).  Anti-tautology: the body really contains multiple
divider rows AND an item row (so the assertion is not vacuously true)."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-board
    (org-air-r29--with-fringeless-gui
      (org-air-r31--render-inline)
      (let* ((bwin (get-buffer-window (current-buffer)))
             (usable (org-air-layout--usable-columns bwin))
             (divider-cols nil)
             (item-rows 0))
        (should (eql org-air-view--rendered-width usable))
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (let* ((beg (line-beginning-position))
                   (end (line-end-position))
                   (w (string-width (buffer-substring-no-properties beg end)))
                   (dcol (org-air-r31--line-divider-col)))
              ;; the user-visible right edge: no line exceeds usable.
              (should (<= w usable))
              (when dcol (push dcol divider-cols))
              (when (text-property-not-all beg end 'org-air-item nil)
                (cl-incf item-rows)))
            (forward-line 1)))
        ;; anti-tautology: real divider rows AND real item rows were seen.
        (should (> (length divider-cols) 3))
        (should (> item-rows 0))
        ;; the divider column is CONSTANT across every row that carries it.
        (should (= 1 (length (delete-dups (copy-sequence divider-cols)))))))))

;;;; =====================================================================
;;;; R31-1 #3 — pure two-pane compose sweep, widths 61..200 (odd+even).
;;;; =====================================================================

(ert-deftest org-air-r31-1-inline-width-sweep ()
  "PURE compose over widths 61..200 (odd AND even): for each W, every
`org-air-view--two-pane-body' row == W, the composed header == W, and the
divider column is constant across all rows.  Locks the width-math
invariant independent of any live window (the batch width seam)."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-board
    (let ((items org-air-view--items)
          (saw-odd nil) (saw-even nil))
      (should items)
      (cl-loop for w from 61 to 200 do
        (let ((org-air-view--line-width w))
          (should (= (org-air-view--render-width) w))
          (if (cl-oddp w) (setq saw-odd t) (setq saw-even t))
          (let* ((rows (car (org-air-view--two-pane-body items w)))
                 (dcols nil))
            (should rows)
            (dolist (row rows)
              (should (= (string-width (substring-no-properties row)) w))
              (let ((dcol (org-air-r31--string-divider-col row)))
                (when dcol (push dcol dcols))))
            ;; the divider is present and lands on ONE column, every row.
            (should dcols)
            (should (= 1 (length (delete-dups (copy-sequence dcols)))))
            ;; the header composed at this width is exactly W wide.
            (should (= (car (org-air-r31--banner-width items)) w)))))
      (should saw-odd)
      (should saw-even))))

;;;; =====================================================================
;;;; R31-1 #4 — SEAM A: the last frame-width fallback reserves the
;;;;            continuation column on a graphic frame (revert-guard).
;;;; =====================================================================

(ert-deftest org-air-r31-1-seam-a-frame-fallback-fits ()
  "The LAST raw-column term: with NO window showing the buffer and the
selected window stubbed non-live, `org-air-layout-current-width' takes
the `t' fallback branch.  On a fringe-less GRAPHIC frame it returns
usable = frame-width - 1 (the continuation column reserved), NOT the raw
frame width; on a TTY/batch frame it returns `frame-width' unchanged
\(byte-identical).  Reverting the branch to raw `(frame-width)' makes the
graphic assertion FAIL (revert-guard)."
  (skip-unless (locate-library "org-air"))
  (should (fboundp 'org-air-layout--usable-frame-columns))
  (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
            ((symbol-function 'window-live-p) (lambda (&rest _) nil)))
    ;; GRAPHIC, fringe-less: no window to query -> mirror the -1 reserve.
    (org-air-r29--with-fringeless-gui
      (should (= (org-air-layout-current-width)
                 (max 1 (1- (frame-width)))))
      (should (= (org-air-layout--usable-frame-columns)
                 (max 1 (1- (frame-width)))))
      ;; the reserve is strictly below the raw frame width (revert-guard):
      ;; a raw `(frame-width)' fallback would return frame-width and fail.
      (should (< (org-air-layout-current-width) (frame-width))))
    ;; TTY/batch: no reserve -> plain frame-width (goldens byte-identical).
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
      (should (= (org-air-layout-current-width) (frame-width)))
      (should (= (org-air-layout--usable-frame-columns) (frame-width))))))

;;;; =====================================================================
;;;; R31-1 #5 — regression: side-window + board-only still fit (R29-1).
;;;; =====================================================================

(ert-deftest org-air-r31-1-side-window-and-board-only-still-fit ()
  "Regression (R29-1 preserved): side-window (popped) and board-only,
fringe-less, still fit usable with the header at the contract column."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-board
    (org-air-r29--with-fringeless-gui
      ;; SIDE-WINDOW (popped rail).
      (org-air-r27--pop-rail)
      (org-air-view--refresh-current)
      (should (eq org-air-view--orientation 'side-window))
      (let ((bwin (get-buffer-window (current-buffer))))
        (should (eql org-air-view--rendered-width
                     (org-air-layout--usable-columns bwin)))
        (org-air-r29--assert-lines-fit bwin)
        (let ((hw (org-air-r29--header-widths)))
          (should (= (cdr hw) (1- org-air-view--rendered-width)))))
      ;; BOARD-ONLY (rail forced off).
      (setq-local org-air-view--rail-popped-out nil)
      (org-air-rail--hide (current-buffer))
      (let ((org-air-rail-min-width 500))
        (org-air-view--refresh-current)
        (should (eq org-air-view--orientation 'board-only))
        (let ((bwin (get-buffer-window (current-buffer))))
          (should (eql org-air-view--rendered-width
                       (org-air-layout--usable-columns bwin)))
          (org-air-r29--assert-lines-fit bwin))))))

;;;; =====================================================================
;;;; R31-1 #6 — fringed GUI unchanged (Seam-A change is a no-op there).
;;;; =====================================================================

(ert-deftest org-air-r31-1-fringed-gui-unchanged ()
  "A GUI WITH fringes (usable == body): the INLINE rendered width and the
longest composed line are byte-for-byte the plain (TTY-measured) values.
Where a live window exists the Seam-A branch is never reached; the fix is
a no-op with fringes."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-board
    ;; plain render (TTY tier: usable == body) — the trunk values.
    (org-air-r31--render-inline)
    (let ((plain-width org-air-view--rendered-width)
          (plain-longest (org-air-r27--longest-line))
          (bwin (get-buffer-window (current-buffer))))
      (should (eql plain-width (window-body-width bwin)))
      ;; fringed GUI: usable == body, so nothing moves.
      (org-air-r29--with-fringed-gui
        (org-air-r31--render-inline)
        (should (eql org-air-view--rendered-width plain-width))
        (should (= (org-air-r27--longest-line) plain-longest))
        (should (eql org-air-view--rendered-width
                     (window-body-width
                      (get-buffer-window (current-buffer)))))))))

;;;; =====================================================================
;;;; R31-1 #7 — SEAM B guard: status/separator glyphs are single-width,
;;;;            and the header composes self-consistently under ambiguous
;;;;            glyphs forced WIDE.
;;;; =====================================================================

(ert-deftest org-air-r31-1-status-glyphs-single-width ()
  "Seam B guard.  (a) Every status/separator/legend glyph org-air emits
\(the PREFERRED forms of clear/more/sep-dot/sort-asc/sort-desc/arrow/
updated) has `string-width' 1 and each of its chars `char-width' 1, so
the renderer's budget can never disagree with a correctly-configured
frame.  (b) Self-consistency: even with `char-width-table' forcing the
ambiguous set WIDE, `org-air-view--insert-banner' STILL composes to
exactly the render width (the padding compensates — the renderer never
assumes ambiguous == 1)."
  (skip-unless (locate-library "org-air"))
  ;; (a) single-width preferred glyphs.
  (dolist (name org-air-r31--ambiguous-glyphs)
    (let ((g (org-air-r31--preferred-glyph name)))
      (should (stringp g))
      (should (= (string-width g) 1))
      (mapc (lambda (ch)
              (should (= (char-width ch) 1)))
            (append g nil))))
  ;; (b) compose self-consistency with the ambiguous set forced WIDE.
  (org-air-r27--with-live-board
    (let* ((items org-air-view--items)
           (chars (mapcar (lambda (name)
                            (string-to-char (org-air-r31--preferred-glyph name)))
                          org-air-r31--ambiguous-glyphs))
           (org-air-view--line-width 80)
           ;; baseline: header composes to exactly the render width.
           (base (car (org-air-r31--banner-width items))))
      (should (= base 80))
      (let ((char-width-table (copy-sequence char-width-table)))
        (dolist (ch chars) (aset char-width-table ch 2))
        ;; sanity: the table really marks them wide now.
        (should (= (char-width (car chars)) 2))
        ;; the composed header STILL measures exactly the render width
        ;; (measured with the same WIDE table — self-consistent).
        (should (= (car (org-air-r31--banner-width items)) 80))))))

(provide 'org-air-round31-test)
;;; org-air-round31-test.el ends here
