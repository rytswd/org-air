;;; org-air-round38-test.el --- R38 regression fence -*- lexical-binding: t; -*-

;;; Commentary:
;; Round-38 regression fence (air/v0.5/org-air-round38-design.org).  The
;; header-overflow hunt ran R29->R37 on the CHARACTER/string layer and never
;; closed because the real root cause is PIXEL-level: the banner's left token
;; `(propertize "  org-air" 'face 'org-air-face-header)' carries the ONLY
;; height-scaled chrome face in the package (`:height 1.2', org-air-faces.el
;; ~173).  Every width budget counted that 9-char token as `string-width' = 9
;; columns, but at 1.2x it PAINTS ~11 canonical pixel-columns, so on a
;; graphical frame with `truncate-lines' the header row ran ~2 pixel-columns
;; past the text area (truncation arrow + overhang, header row ONLY).  R38-1
;; charges the token its TRUE pixel width in the banner composer
;; (`org-air-view--banner-left-cols' / `org-air-view--justify'); R38-2 stops
;; the inspector inline refill re-emitting off-edge trailing whitespace from a
;; stale cached geometry.
;;
;; This fence adds the PIXEL-layer guard that was missing for five rounds, on
;; top of the STRING-layer guard (kept green), a FACE-HYGIENE guard (the class
;; catcher that would have caught this in R29), and the R38-2 inspector guard.
;; All are EXECUTING and deterministic in batch: the graphical frame is
;; simulated by pinning `display-graphic-p'/`frame-char-width'/
;; `string-pixel-width' so the pixel arithmetic runs headless; the GUI-
;; definitive variants carry `(skip-unless (display-graphic-p))' and are
;; skipped under batch.  Every revert guard reconstructs the pre-R38 contract
;; in-process and proves the assertion FAILS on it (non-tautological).
;;
;; BATCH BYTE-STABILITY: R38-1 is graphical-only.  Under `noninteractive' the
;; banner composes exactly as before (`org-air-view--banner-left-cols' returns
;; `string-width' on a TTY), so NO golden/mockup byte moves — `make regen-
;; mockups' is confirmed zero-churn.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-air)

(defconst org-air-r38--banner-left "  org-air"
  "The literal left token `org-air-view--insert-banner' emits (S7 margin + name).")

(defconst org-air-r38--cw 10
  "Simulated canonical frame char width (pixels) for the headless pixel model.")

(defun org-air-r38--mock-pixel-width (string)
  "Pixel width of STRING under the headless model: chars wearing the height-
scaled `org-air-face-header' (:height 1.2) paint 1.2x the canonical cell
`org-air-r38--cw'; every other glyph paints exactly one cell.  This mirrors
what the display engine does to the banner's enlarged title, so a `string-
pixel-width'-based reconstruction of the composed row is computable in batch."
  (let ((s (if (stringp string) string (format "%s" string)))
        (px 0))
    (dotimes (i (length s))
      (let* ((face (get-text-property i 'face s))
             (scaled (or (eq face 'org-air-face-header)
                         (and (listp face) (memq 'org-air-face-header face)))))
        (setq px (+ px (round (* org-air-r38--cw (if scaled 1.2 1.0)))))))
    px))

(defmacro org-air-r38--with-graphical-frame (&rest body)
  "Run BODY with a SIMULATED graphical frame: `display-graphic-p' => t,
`frame-char-width' => `org-air-r38--cw', and `string-pixel-width' => the
face-aware headless model.  Lets the R38-1 pixel path (`org-air-view--
banner-left-cols') execute deterministically under batch."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
             ((symbol-function 'frame-char-width) (lambda (&rest _) org-air-r38--cw))
             ((symbol-function 'string-pixel-width)
              (lambda (s) (org-air-r38--mock-pixel-width s))))
     ,@body))

(defun org-air-r38--compose-banner (width &optional loading)
  "Return (LINE . LEFT): the ACTUAL composed banner LINE at WIDTH (with text
properties preserved) and the propertized LEFT token, via the real
`org-air-view--insert-banner' choke point (LOADING selects the skeleton)."
  (with-temp-buffer
    (let ((org-air-view--line-width width)
          (org-air-view--loading loading))
      (org-air-view--insert-banner nil)
      (goto-char (point-min))
      (cons (buffer-substring (line-beginning-position) (line-end-position))
            (propertize org-air-r38--banner-left 'face 'org-air-face-header)))))

;;;; =====================================================================
;;;; 2a. STRING layer (headless, KEEP GREEN) — banner through the live
;;;;     finalize path at the pinned usable = body-1 width.
;;;; =====================================================================

(ert-deftest org-air-r38-string-banner-fits-usable-no-trailing-ws ()
  "Render the banner via the LIVE finalize path (`org-air-view--insert-banner'
+ `org-air-view--finalize-buffer-lines') with the width getter pinned to the
R37 usable = body-1, across several bodies incl. the user's 191.  Assert:
NO composed line's `string-width' exceeds usable, and the banner (line 0)
does NOT end in whitespace (it is flush to its last content glyph).  This is
the character-layer contract that was already correct; it must stay green."
  (skip-unless (locate-library "org-air"))
  (dolist (body '(40 63 80 96 100 120 160 191 200))
    (let ((usable (max 1 (1- body))))         ; R37 universal reserve: body-1
      (dolist (loading '(nil t))
        (with-temp-buffer
          (let ((org-air-view--line-width usable)
                (org-air-view--loading loading))
            (org-air-view--insert-banner nil)
            (org-air-view--insert-rule)
            ;; the LIVE finalize path: cap to usable + right-trim.
            (org-air-view--finalize-buffer-lines usable))
          (goto-char (point-min))
          (let ((n 0))
            (while (not (eobp))
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position))))
                (ert-info ((format "body %d usable %d loading %s line %d: %S"
                                   body usable loading n line))
                  (should (<= (string-width line) usable))
                  (when (= n 0)
                    ;; the banner row must not end in whitespace…
                    (should-not (string-match-p "[ \t]+$" line))
                    ;; …and it is flush to its last content glyph.
                    (should (= (string-width (string-trim-right line))
                               (string-width line))))))
              (setq n (1+ n))
              (forward-line 1))))))))

;;;; =====================================================================
;;;; 2b. PIXEL layer (THE guard missing for five rounds).
;;;; =====================================================================

(ert-deftest org-air-r38-1-banner-left-cols-charges-pixel-true-width ()
  "On a (simulated) GRAPHICAL frame the banner composer charges the height-
scaled left token its TRUE pixel-column cost, not `string-width': `org-air-
view--banner-left-cols' >= `string-width' of the token, and STRICTLY greater
for the 1.2x title (it paints ~11 columns, string-width counts 9).  Under a
TTY it collapses to `string-width' exactly (so the golden path is byte-
identical)."
  (skip-unless (locate-library "org-air"))
  (should (fboundp 'org-air-view--banner-left-cols))
  (let ((left (propertize org-air-r38--banner-left 'face 'org-air-face-header)))
    ;; TTY / batch: exactly string-width (byte-identical golden path).
    (should (= (org-air-view--banner-left-cols left) (string-width left)))
    ;; graphical: pixel-true, strictly wider than string-width for :height 1.2.
    (org-air-r38--with-graphical-frame
      (let ((lc (org-air-view--banner-left-cols left)))
        (should (>= lc (string-width left)))
        (should (> lc (string-width left)))
        ;; == ceil(string-pixel-width / frame-char-width).
        (should (= lc (ceiling (/ (float (org-air-r38--mock-pixel-width left))
                                  (float org-air-r38--cw)))))))))

(ert-deftest org-air-r38-1-banner-row-pixel-extent-within-usable ()
  "PIXEL reconstruction of the composed banner row is <= (* usable
`frame-char-width') pixels on a graphical frame — the row NEVER paints past
the text area even though its enlarged title over-paints `string-width'.
Checked across bodies incl. the user's 191, both header flavours."
  (skip-unless (locate-library "org-air"))
  (org-air-r38--with-graphical-frame
    (dolist (usable '(40 80 120 190))
      (dolist (loading '(nil t))
        (let* ((composed (org-air-r38--compose-banner usable loading))
               (line (car composed)) (left (cdr composed))
               (budget-px (* usable org-air-r38--cw)))
          (ert-info ((format "usable %d loading %s row=%S" usable loading line))
            ;; the STRING still fits usable characters (flush layout intact)…
            (should (<= (string-width line) usable))
            ;; …and its PIXEL extent fits the text area.
            (should (<= (org-air-r38--mock-pixel-width line) budget-px))
            ;; the left token is charged more columns than string-width…
            (should (> (org-air-view--banner-left-cols left)
                       (string-width left)))))))))

(ert-deftest org-air-r38-1-reverting-to-string-width-budget-fails ()
  "NON-TAUTOLOGY / REVERT GUARD.  Reverting R38-1 (charge the GUI budget by
`string-width' instead of the pixel-true cost) re-composes a banner row whose
PIXEL extent is `excess' columns WIDER than the pixel-true row.  R39-1 added a
symmetric right gutter of `org-air-view--banner-indent' columns: the fixed
(pixel-true) row fits INSIDE that gutter (pixel extent <= (usable-indent)*cw),
but the reverted row eats into it and OVERHANGS the gutter-reserved area —
exactly the five-round header artifact, one gutter deeper in.  The 2-col
gutter no longer masks it once the target is the gutter-reserved width."
  (skip-unless (locate-library "org-air"))
  (org-air-r38--with-graphical-frame
    (dolist (usable '(40 80 120 190))
      (let* ((fixed (car (org-air-r38--compose-banner usable nil)))
             ;; R39-1: the header targets the gutter-reserved width
             ;; (usable - banner-indent), so its pixel budget is that in pixels.
             (gutter-px (* (- usable org-air-view--banner-indent) org-air-r38--cw))
             ;; revert: the GUI budget uses string-width (pre-R38-1).
             (reverted (cl-letf (((symbol-function 'org-air-view--banner-left-cols)
                                  (lambda (l) (string-width l))))
                         (car (org-air-r38--compose-banner usable nil)))))
        (ert-info ((format "usable %d fixed=%S reverted=%S" usable fixed reverted))
          ;; the FIXED (pixel-true) row's pixel extent fits within the
          ;; gutter-reserved width…
          (should (<= (org-air-r38--mock-pixel-width fixed) gutter-px))
          ;; …the REVERTED row's pixel extent OVERHANGS the gutter (it is
          ;; `excess' columns wider, so it eats into the reserved margin).
          (should (> (org-air-r38--mock-pixel-width reverted) gutter-px))
          ;; and the reverted row is strictly wider than the fixed one
          ;; (the pixel excess R38-1 charges is real).
          (should (> (org-air-r38--mock-pixel-width reverted)
                     (org-air-r38--mock-pixel-width fixed))))))))

(ert-deftest org-air-r38-1-banner-row-pixel-fits-gui ()
  "GUI-DEFINITIVE variant (skipped in batch).  On a real graphical frame the
banner row's measured `window-text-pixel-size' does not exceed the window's
text-area pixel width (`window-body-width' WIN t)."
  (skip-unless (display-graphic-p))
  (let ((buf (get-buffer-create " *org-air-r38-banner*")))
    (unwind-protect
        (with-current-buffer buf
          (let ((inhibit-read-only t)) (erase-buffer))
          (setq-local truncate-lines t)
          (let ((win (display-buffer buf '(display-buffer-pop-up-window))))
            (skip-unless (window-live-p win))
            (with-selected-window win
              (let ((org-air-view--line-width
                     (max 1 (1- (window-body-width win)))))
                (let ((inhibit-read-only t))
                  (org-air-view--insert-banner nil)))
              (goto-char (point-min))
              (let ((px (car (window-text-pixel-size
                              win (line-beginning-position) (line-end-position))))
                    (avail (window-body-width win t)))
                (ert-info ((format "banner row px=%s avail=%s" px avail))
                  (should (<= px avail)))))))
      (kill-buffer buf))))

;;;; =====================================================================
;;;; 2c. FACE-HYGIENE fence — the whole-class catcher (would have caught
;;;;     this in R29).
;;;; =====================================================================

(defun org-air-r38--scaled-chrome-faces ()
  "Return the org-air-face-* faces whose OWN `:height' is a scale != 1.0.
Uses the face's own attribute (no inheritance), so the batch default's
resolved `:height 1' does not leak in.  These are the faces that paint more
pixel-columns than `string-width' counts and therefore MUST be composed only
by a pixel-aware composer."
  (let (scaled)
    (dolist (f (face-list))
      (let ((name (symbol-name f)))
        (when (string-prefix-p "org-air-face-" name)
          (let ((h (face-attribute f :height nil nil)))
            (when (and (numberp h) (not (= h 1.0)))
              (push f scaled))))))
    (nreverse scaled)))

(ert-deftest org-air-r38-face-hygiene-only-banner-consumes-scaled-height ()
  "FACE-HYGIENE FENCE.  Iterate `face-list', filter the org-air chrome faces,
and assert the ONLY one carrying a scaled `:height' (!= 1.0) is
`org-air-face-header' — the banner's left token — and that the banner
composer charges it its pixel-true width.  Any NEW height-scaled chrome face
that is not routed through the pixel-aware composer trips this guard (the
assertion that would have caught the :height 1.2 overflow back in R29)."
  (skip-unless (locate-library "org-air"))
  (let ((scaled (org-air-r38--scaled-chrome-faces)))
    (ert-info ((format "scaled chrome faces: %S" scaled))
      ;; exactly one scaled chrome face, and it is the banner title face.
      (should (equal scaled (list 'org-air-face-header))))
    ;; that face is CONSUMED by the pixel-aware banner composer: on a
    ;; graphical frame the composer charges the header-faced token strictly
    ;; more columns than string-width (i.e. it is pixel-aware for this face).
    (should (fboundp 'org-air-view--banner-left-cols))
    (org-air-r38--with-graphical-frame
      (let ((tok (propertize "  org-air" 'face 'org-air-face-header)))
        (should (> (org-air-view--banner-left-cols tok)
                   (string-width tok)))))))

;;;; =====================================================================
;;;; 2d. R38-2 — inspector inline refill emits NO off-edge trailing
;;;;     whitespace (the "never trim here" body-row artifact class).
;;;; =====================================================================

(defun org-air-r38--refill-inline (item-width divider rail-width region-height
                                             live-width &optional revert)
  "Drive `org-air-view--render-inspector-region' inline branch and return the
region's refilled lines.  A WIDE cached geometry (ITEM-WIDTH + DIVIDER +
RAIL-WIDTH) simulates the pre-narrow composed width; LIVE-WIDTH is the
narrowed live render width (as if the window shrank inside the resize
debounce).  When REVERT is non-nil, model the pre-R38-2 behaviour (pad the
refilled row to the CACHED composed width instead of clamping to the live
render width) so its off-edge overhang is observable."
  (let ((cells (mapcar (lambda (i)
                         (propertize (org-air-view--pad-to (format "rail-%d" i) rail-width)
                                     'org-air-inspector t))
                       (number-sequence 0 (1- region-height))))
        (composed (+ item-width (string-width divider) rail-width)))
    (with-temp-buffer
      (dotimes (i region-height)
        (insert (org-air-view--pad-to (format "item-%d body" i) item-width) "\n"))
      (goto-char (point-min))
      (setq-local org-air-view--inspector-beg (copy-marker (point-min) nil))
      (setq-local org-air-view--inspector-end (copy-marker (point-max) nil))
      (setq-local org-air-view--inspector-geom
                  (list :item-width item-width :divider divider
                        :rail-width rail-width :region-height region-height))
      (let ((org-air-view--line-width live-width)
            (org-air-view-width nil))
        (cl-letf (((symbol-function 'org-air-view--inspector-rail-lines)
                   (lambda (_item _w _h) cells))
                  ;; REVERT: postprocess pads to the stale composed width
                  ;; (trailing whitespace past the live edge) instead of
                  ;; clamping/trimming to the live render width.
                  ((symbol-function 'org-air-view--postprocess-line)
                   (if revert
                       (lambda (line _w) (org-air-view--pad-to line composed))
                     (symbol-function 'org-air-view--postprocess-line))))
          (org-air-view--render-inspector-region 'dummy)))
      (let (lines)
        (goto-char (point-min))
        (while (not (eobp))
          (push (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position))
                lines)
          (forward-line 1))
        (nreverse lines)))))

(ert-deftest org-air-r38-2-inspector-refill-no-off-edge-trailing-ws ()
  "R38-2.  The inspector inline refill (the \"never trim here\" branch of
`org-air-view--render-inspector-region') must NOT re-emit rows padded past
the LIVE usable width when the cached geometry is stale (window narrowed in
the resize debounce).  Every refilled line fits the live width and carries no
trailing whitespace past its content."
  (skip-unless (locate-library "org-air"))
  (dolist (live '(30 40 58))
    (let ((lines (org-air-r38--refill-inline 20 " | " 60 4 live)))
      (should (= (length lines) 4))
      (dolist (line lines)
        (ert-info ((format "live %d line: %S" live line))
          (should (<= (string-width line) live))
          (should-not (string-match-p "[ \t]+$" line)))))))

(ert-deftest org-air-r38-2-reverting-inspector-clamp-fails ()
  "NON-TAUTOLOGY / REVERT GUARD for R38-2.  Modelling the pre-fix refill (pad
to the stale CACHED composed width, no clamp to the live render width)
re-introduces rows that overhang the live edge as pure trailing whitespace —
the exact reported body-row artifact.  The fixed refill has no such row."
  (skip-unless (locate-library "org-air"))
  (let* ((live 40)
         (fixed   (org-air-r38--refill-inline 20 " | " 60 4 live nil))
         (reverted (org-air-r38--refill-inline 20 " | " 60 4 live t)))
    ;; fixed: every row fits and is trimmed.
    (dolist (line fixed)
      (should (<= (string-width line) live))
      (should-not (string-match-p "[ \t]+$" line)))
    ;; reverted: at least one row overhangs the live edge with trailing ws.
    (should (cl-some (lambda (line)
                       (and (> (string-width line) live)
                            (string-match-p "[ \t]+$" line)))
                     reverted))))

(provide 'org-air-round38-test)
;;; org-air-round38-test.el ends here
