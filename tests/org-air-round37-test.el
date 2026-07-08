;;; org-air-round37-test.el --- R37 acceptance ERTs (folded back) -*- lexical-binding: t; -*-

;;; Commentary:
;; Round-37 acceptance tests (air/v0.5/org-air-round37-design.org), RE-AUTHORED
;; from the design spec's "Testability" section after the file was lost between
;; reboots (it was never committed to the tree).  EXECUTING and deterministic
;; in batch.
;;
;; R37-1  USABLE WIDTH WAS ONE TOO HIGH on fringed + right-scrollbar frames.
;;        R34 set usable = `window-body-width' and reserved the continuation
;;        column ONLY when `right-fringe == 0'.  On a frame with a RIGHT scroll
;;        bar (and/or a partial last column when body pixels are not an exact
;;        multiple of the char advance) the LAST body column is clipped, so
;;        R36's flush content lost its last glyph ("… files" -> "… file").
;;        R37 adopts a UNIVERSAL 1-column right safety margin: on ANY graphical
;;        frame usable = `window-body-width' - 1 (window tier AND frame tier
;;        agree); on a TTY/batch frame usable = `window-body-width' exactly, so
;;        every batch golden is byte-identical (the graphical branch never runs
;;        under `noninteractive').
;;
;; Guards here: the pure width model reserves exactly one column on every
;; graphical geometry and none on a TTY; a right-scroll-bar SIMULATION (live,
;; skipped in batch) confirms usable = body-1; the composed header + rule fit
;; body-1 with the last glyph at body-2 and the last column EMPTY; and a REVERT
;; guard restores R34's `right-fringe > 0 => body' rule and shows the composed
;; header's last glyph then lands on the CLIPPED last column (body-1) — the
;; exact regression that clipped the "s".

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-air)

(defconst org-air-r37--banner-left "  org-air"
  "The literal left token `org-air-view--insert-banner' emits.")

(defun org-air-r37--compose-header (width &optional loading)
  "Return the ACTUAL composed banner line at WIDTH (no text properties), via
the real `org-air-view--insert-banner' choke point (LOADING = skeleton)."
  (with-temp-buffer
    (let ((org-air-view--line-width width)
          (org-air-view--loading loading))
      (org-air-view--insert-banner nil)
      (goto-char (point-min))
      (buffer-substring-no-properties (line-beginning-position)
                                      (line-end-position)))))

;;;; =====================================================================
;;;; R37-1 — the pure width model reserves ONE column on every graphical
;;;;          geometry, and NONE on a TTY (batch parity).
;;;; =====================================================================

(ert-deftest org-air-r37-1-usable-reserves-one-graphical-column ()
  "`org-air-layout--usable-columns-for' reserves exactly ONE right column on
every graphical geometry (fringed+scrollbar, fringe-less, fringed-no-
scrollbar all collapse to body-1) and reserves NOTHING on a TTY (body, batch
parity).  Monotone invariant: 1 <= result <= body for any body/fringe."
  (skip-unless (locate-library "org-air"))
  (should (fboundp 'org-air-layout--usable-columns-for))
  ;; graphical: body-1 regardless of the (now advisory) fringe/scroll-bar arg.
  (should (= (org-air-layout--usable-columns-for t 191 8) 190))   ; fringed+scrollbar
  (should (= (org-air-layout--usable-columns-for t 191 0) 190))   ; fringe-less
  (should (= (org-air-layout--usable-columns-for t 191 nil) 190)) ; fringed-no-sb
  (should (= (org-air-layout--usable-columns-for t 80 8) 79))
  ;; TTY / mock: the plain body width (byte-identical golden path).
  (should (= (org-air-layout--usable-columns-for nil 191 0) 191))
  (should (= (org-air-layout--usable-columns-for nil 191 8) 191))
  (should (= (org-air-layout--usable-columns-for nil 80 8) 80))
  ;; invariant sweep.
  (dolist (graphic '(t nil))
    (dolist (body '(1 2 40 80 120 191 200))
      (dolist (fr '(nil 0 1 8))
        (let ((u (org-air-layout--usable-columns-for graphic body fr)))
          (should (<= u body))
          (should (>= u 1)))))))

(ert-deftest org-air-r37-1-frame-tier-agrees-with-window-tier ()
  "The frame-tier seam (`org-air-layout--usable-frame-columns') applies the
SAME universal -1 reserve on a graphical frame, so the window tier can never
return one MORE than the frame tier (the R34 divergence that could exceed the
text area).  On a TTY it returns the plain `frame-width'."
  (skip-unless (locate-library "org-air"))
  (should (fboundp 'org-air-layout--usable-frame-columns))
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
            ((symbol-function 'frame-width) (lambda (&rest _) 191)))
    (should (= (org-air-layout--usable-frame-columns) 190)))
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil))
            ((symbol-function 'frame-width) (lambda (&rest _) 191)))
    (should (= (org-air-layout--usable-frame-columns) 191))))

;;;; =====================================================================
;;;; R37-1 — right-scroll-bar SIMULATION (live, skipped in batch).
;;;; =====================================================================

(ert-deftest org-air-r37-1-right-scrollbar-window-usable-is-body-1 ()
  "GUI SIMULATION (skipped in batch).  On a graphical frame with default
fringes and a RIGHT scroll bar, `org-air-layout--usable-columns' returns
body-1 (the last body column reserved), NOT the full body."
  (skip-unless (display-graphic-p))
  (let ((frame (make-frame '((width . 100) (height . 24)
                             (vertical-scroll-bars . right)
                             (minibuffer . nil)))))
    (unwind-protect
        (let ((win (frame-selected-window frame)))
          (set-window-scroll-bars win nil 'right)
          (let* ((body (window-body-width win))
                 (usable (org-air-layout--usable-columns win)))
            (ert-info ((format "body=%s usable=%s" body usable))
              (should (= usable (max 1 (1- body)))))))
      (delete-frame frame))))

;;;; =====================================================================
;;;; R37-1 — composed header fits body-1, last glyph at body-2, last
;;;;          column EMPTY; revert to R34 clips the last glyph.
;;;; =====================================================================

(ert-deftest org-air-r37-1-header-fits-body-1-last-glyph-at-body-2 ()
  "With the render width pinned to the R37 usable = body-1, the ACTUAL
composed header (both flavours, bodies incl. 191) fits usable, is FLUSH (no
trailing whitespace past its right content), and its last VISIBLE glyph sits
at column body-2 (usable-1) — leaving the clipped last body column EMPTY."
  (skip-unless (locate-library "org-air"))
  (dolist (body '(40 80 120 191))
    (let ((usable (max 1 (1- body))))         ; body-1
      (dolist (loading '(nil t))
        (let ((line (org-air-r37--compose-header usable loading)))
          (ert-info ((format "body %d usable %d loading %s: %S"
                             body usable loading line))
            ;; fits the reserved width…
            (should (<= (string-width line) usable))
            ;; …no trailing whitespace past the content (R36 flush preserved)…
            (should-not (string-match-p "[ \t]+$" line))
            ;; …R39-1: last glyph ends banner-indent columns before the last
            ;; usable column (symmetric right gutter), so trim == usable-indent.
            (should (= (string-width (string-trim-right line))
                       (- usable org-air-view--banner-indent)))
            ;; the last body column (body-1) is NOT occupied by content.
            (should (< (string-width line) body))))))))

(ert-deftest org-air-r37-1-reverting-to-r34-rule-clips-last-glyph ()
  "NON-TAUTOLOGY / REVERT GUARD.  Restoring R34's rule (graphical branch
returns `body' when a right fringe is present) makes usable = body = 191 on
the scroll-bar frame; the flush composer then places the header's last glyph
on column body-1 — the CLIPPED last column.  The R37 width (body-1) places it
one column earlier (body-2), off the clip.  The reverted target width is the
regression that clipped the \"s\"."
  (skip-unless (locate-library "org-air"))
  (let* ((body 191)
         ;; R34 (reverted) rule: with a right fringe present, use the whole body.
         (r34 (lambda (graphic body fringe)
                (if graphic (if (and (numberp fringe) (> fringe 0))
                                body (max 1 (1- body)))
                  body)))
         (usable-r37 (org-air-layout--usable-columns-for t body 8))
         (usable-r34 (funcall r34 t body 8)))
    ;; the two rules DISAGREE on the fringed+scrollbar geometry.
    (should (= usable-r37 190))
    (should (= usable-r34 191))
    (let ((line-r37 (org-air-r37--compose-header usable-r37))
          (line-r34 (org-air-r37--compose-header usable-r34)))
      ;; R39-1: each header ends banner-indent columns before the width it was
      ;; composed at (symmetric right gutter).  The R34 (reverted) width is one
      ;; column WIDER, so its content still lands one column further right than
      ;; the R37 width — the reverted-width regression signal survives.
      (should (= (string-width (string-trim-right line-r37))
                 (- usable-r37 org-air-view--banner-indent)))   ; 190-indent = 188
      (should (< (string-width line-r37) body))
      (should (= (string-width (string-trim-right line-r34))
                 (- usable-r34 org-air-view--banner-indent)))   ; 191-indent = 189
      (should (= (string-width line-r34)
                 (- usable-r34 org-air-view--banner-indent)))
      ;; the reverted (wider) width pushes the content one column right.
      (should (= (1+ (string-width (string-trim-right line-r37)))
                 (string-width (string-trim-right line-r34)))))))

(provide 'org-air-round37-test)
;;; org-air-round37-test.el ends here
