;;; org-air-round43-test.el --- executing ERT for v0.5 round-43 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERT for v0.5 round-43 (air/v0.5/org-air-round43-design.org):
;; the board<->rail pane divider BREAKS in the lower portion of a two-pane
;; board on the LIVE render path (board rows >> rail-content rows).
;;
;; R43-1 diagnosis (headless) REFUTED the prompt's hypothesis: the composer
;; (`org-air-view--compose-columns' + the `org-air-view--two-pane-body'
;; fill-row) emits the pane divider on EVERY board row, including every row
;; PAST the rail's content end (the rail side is blank-padded, never
;; divider-skipped).  The break is introduced AFTER composition by the LIVE
;; render tail `org-air-view--finalize-buffer-lines', whose plain
;; `string-trim-right' strips the blank rail tail on every board row whose
;; rail cell is blank, DEMOTING the interior divider cell into that row's
;; TERMINAL glyph.  A terminal box-drawing | does not tile with the interior
;; | cells above/below it, so the rule reads as broken segments — exactly and
;; only where the rail cell is blank (the board >> rail gap).  The batch/
;; golden tail (`org-air-view--normalize-buffer-lines') pads to width and
;; keeps | interior, which is why NO golden ever caught it (this suite drives
;; the LIVE tail — `org-air-view-width' NIL — so it does).
;;
;; R43-2 fix: `org-air-view--finalize-buffer-lines' is DIVIDER-AWARE — when a
;; two-pane pane-divider column is in force (`org-air-view--pane-divider-col',
;; item-width + the divider's leading space) any line carrying the faced |
;; at that column is PADDED to full width instead of trimmed, so the divider
;; stays an INTERIOR cell on every board content + fill row PAST the rail
;; content end — byte-identical to the normalize/golden shape.  Header banner,
;; header rule and footer carry no divider and keep the R36-1 / R37 no-
;; trailing-pad contract; board-only / stacked / side-window pass a nil
;; divider column and are untouched.
;;
;; The DECISIVE fences (each REVERT-FAILS):
;;
;;   1  LIVE DIVIDER CONTINUITY past the rail content end.  Render a tall
;;      two-pane board LIVE (finalize tail, board >> rail) and assert EVERY
;;      board row past the rail's last content row carries the pane divider
;;      at the SAME shared column AND that | is INTERIOR (the row is padded to
;;      the full render width; the | is NOT the terminal glyph, a cell
;;      follows it).  Reverting the divider-aware pad (plain trailing-trim)
;;      turns those same rows TERMINAL / sub-width — proven decisively by
;;      running the REAL `org-air-view--finalize-buffer-lines' on those rows
;;      with the divider column NIL (exactly the reverted behaviour) and
;;      showing the | becomes terminal.
;;
;;   2  BREAK-POINT = blank-rail rows (characterization guard).  Under the OLD
;;      tail (finalize with a nil divider column = revert) the set of
;;      terminal-| rows equals exactly the set of blank-rail rows and is
;;      NON-EMPTY; the fix (finalize with the divider column) drives that set
;;      to EMPTY on the very same rows.
;;
;;   3  HEADER / RULE untouched (R36-1 / R37 held).  The banner + header rule
;;      carry NO pane divider and NO trailing pad — the divider-scoped pad did
;;      not leak into the header.
;;
;;   4  ORIENTATION SCOPE.  A board-only render (below the rail threshold)
;;      exposes a NIL `org-air-view--pane-divider-col', carries no pane
;;      divider, and its body lines are right-trimmed (no trailing pad) — the
;;      fix touches ONLY two-pane divider rows.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)

(when (locate-library "org-air")
  (require 'org-air))

;;;; -------------------------------------------------------------------
;;;; Tall two-pane board harness — the LIVE finalize tail, board >> rail.
;;;; -------------------------------------------------------------------

(defconst org-air-r43--item-count 200
  "Item count for the synthetic tall board (board rows >> rail rows).
A single file of dated TODOs so the expanded sections compose a board
far taller than the rail's content height at the render geometry below —
the very state the goldens (few items, board ~ rail) never cover.")

(defmacro org-air-r43--with-tall-two-pane (&rest body)
  "Render a TALL two-pane board through the LIVE finalize tail; run BODY.
Generates `org-air-r43--item-count' dated TODOs, then renders headless
with `display-graphic-p' stubbed t (the real GUI glyph set), the clock
frozen, the anti-tautology render guards active, sections EXPANDED, and —
crucially — `org-air-view-width' NIL so the render takes the LIVE
`org-air-view--finalize-buffer-lines' tail (NOT the batch normalize path).
The width/height are driven through `org-air-layout-current-width' /
`...-height' (191 x 60) so two-pane engages and the board is far taller
than the rail.  BODY runs in the rendered `*org-air*' buffer."
  (declare (indent 0) (debug t))
  `(let ((org-air-r43--dir (make-temp-file "org-air-r43-" t)))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name "many.org" org-air-r43--dir)
             (dotimes (i org-air-r43--item-count)
               (insert (format "* TODO Task number %d needing attention\nSCHEDULED: <2026-06-1%d>\n"
                               i (% i 9)))))
           (with-temp-file (expand-file-name "inbox.org" org-air-r43--dir)
             (insert "* TODO Inbox capture\n"))
           (let ((org-air-files (directory-files org-air-r43--dir t "\\.org\\'"))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-r43--dir)))
             (org-air-viewport-test--with-frozen-now
               (org-air-viewport-test-as-gui
                 (org-air-viewport-test--with-render-guards
                   (cl-letf (((symbol-function 'org-air-layout-current-width)
                              (lambda (&rest _) 191))
                             ((symbol-function 'org-air-layout-current-height)
                              (lambda (&rest _) 60)))
                     (let ((org-air-view-width nil)   ; LIVE finalize tail
                           (org-air-view-height nil)
                           (org-air-view--expanded-sections
                            '(inbox attention upcoming high-priority stale)))
                       (org-air)
                       (unwind-protect
                           (with-current-buffer "*org-air*" ,@body)
                         (when (get-buffer "*org-air*")
                           (kill-buffer "*org-air*"))))))))))
       (delete-directory org-air-r43--dir t))))

(defun org-air-r43--vrule ()
  "The pane-divider box-drawing glyph in force (GUI | under the stub)."
  (org-air-view--glyph 'vrule))

(defun org-air-r43--divider-rows (col)
  "Return a list of (INDEX . LINE) for body rows carrying the pane divider.
INDEX is 1-based; LINE keeps its text properties (so the faced divider is
detectable).  A row qualifies when `org-air-view--pane-divider-line-p'
finds the faced vrule at display column COL."
  (let ((idx 0) (rows ()))
    (dolist (line (org-air-viewport-test-lines))
      (setq idx (1+ idx))
      (when (org-air-view--pane-divider-line-p line col)
        (push (cons idx line) rows)))
    (nreverse rows)))

(defun org-air-r43--line-after-divider (line col)
  "Return the substring of LINE strictly AFTER the divider at column COL.
Empty string when the divider is the terminal glyph (nothing follows)."
  (let* ((vrule (org-air-r43--vrule))
         (pos (org-air-r43--divider-glyph-index line col vrule)))
    (if (and pos (< (1+ pos) (length line)))
        (substring line (1+ pos))
      "")))

(defun org-air-r43--divider-glyph-index (line col vrule)
  "Return the STRING index of the faced VRULE at display column COL in LINE."
  (let ((i 0) (w 0) (len (length line)) (glyph (string-to-char vrule)) found)
    (while (and (not found) (<= w col) (< i len))
      (when (and (= w col) (eq (aref line i) glyph))
        (setq found i))
      (setq w (+ w (char-width (aref line i))) i (1+ i)))
    found))

(defun org-air-r43--rail-content-end (col)
  "Return the 1-based INDEX of the last board row that carries RAIL content.
A rail-content row is a pane-divider row with a non-space glyph strictly
after the divider column (calendar / filter / summary / inspector field /
actions).  Board rows AFTER this index whose rail cell is blank are the
`blank-rail rows past the rail content end' the R43-2 fix must keep
interior."
  (let ((end 0))
    (dolist (row (org-air-r43--divider-rows col))
      (when (string-match-p "[^ ]" (org-air-r43--line-after-divider
                                    (cdr row) col))
        (setq end (car row))))
    end))

(defun org-air-r43--terminal-divider-p (line col)
  "Return non-nil when the faced divider at COL is the TERMINAL glyph of LINE.
That is, LINE ends exactly at the divider — no cell follows it (the broken-
rule shape the OLD `string-trim-right' tail produces on a blank-rail row)."
  (let* ((vrule (org-air-r43--vrule))
         (pos (org-air-r43--divider-glyph-index line col vrule)))
    (and pos (= (1+ pos) (length line)))))

(defun org-air-r43--finalize-line (line width divider-col)
  "Run the REAL `org-air-view--finalize-buffer-lines' on one LINE.
Returns the single finalized line.  DIVIDER-COL nil reproduces the
PRE-R43 (reverted) trailing-trim behaviour exactly; non-nil is the fix."
  (with-temp-buffer
    (insert line)
    (org-air-view--finalize-buffer-lines width divider-col)
    (buffer-substring (point-min) (point-max))))

;;;; -------------------------------------------------------------------
;;;; 1. LIVE divider continuity past the rail content end.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r43-live-divider-interior-past-rail ()
  "R43-2: on the LIVE finalize path, EVERY board row past the rail content
end carries the pane divider at the shared column as an INTERIOR cell
\(full render width; the | is not the terminal glyph).

Rendered headless (GUI stub) as a tall two-pane board (board rows >> rail
rows), driven through `org-air-view--finalize-buffer-lines' (`org-air-view-
width' NIL).  For the band of body rows AFTER the rail's last content row
\(the blank-rail gap the user sees break): every such row carries the faced
| at the SAME column `org-air-view--pane-divider-col', the row is composed
to the FULL render width, and a cell follows the | (INTERIOR, never
terminal).

REVERT-FAILS, proven with the REAL function: feeding each of those rows
back through `org-air-view--finalize-buffer-lines' with a NIL divider
column — precisely the reverted trailing-trim tail — DEMOTES the | to the
row's terminal glyph and drops the row below full width.  The fix
\(finalize WITH the divider column) keeps it interior and full width."
  (skip-unless (locate-library "org-air"))
  (should (fboundp 'org-air-view--finalize-buffer-lines))
  (should (fboundp 'org-air-view--pane-divider-line-p))
  (org-air-r43--with-tall-two-pane
    (let* ((col org-air-view--pane-divider-col)
           (w (org-air-view--render-width))
           (vrule (org-air-r43--vrule))
           (rail-end (org-air-r43--rail-content-end col))
           (rows (org-air-r43--divider-rows col))
           (past (cl-remove-if-not (lambda (r) (> (car r) rail-end)) rows)))
      ;; sanity / anti-tautology: two-pane really engaged, a real pane
      ;; divider column exists, and there IS a substantial blank-rail band
      ;; past the rail content end (board >> rail).
      (should (eq org-air-view--orientation 'two-pane))
      (should (integerp col))
      (should (> rail-end 0))
      (should (>= (length past) 40))
      (ert-info ((format "w=%d col=%d rail-end=%d past-rows=%d"
                         w col rail-end (length past)))
        (dolist (row past)
          (let ((idx (car row)) (line (cdr row)))
            (ert-info ((format "row %d width=%d" idx (string-width line)))
              ;; the divider is at the shared column, full width, INTERIOR.
              (should (org-air-view--pane-divider-line-p line col))
              (should (= (string-width line) w))
              (should-not (org-air-r43--terminal-divider-p line col))
              ;; THE FIX (finalize WITH the divider column) keeps it exactly
              ;; as rendered — interior, full width.
              (let ((fixed (org-air-r43--finalize-line line w col)))
                (should (= (string-width fixed) w))
                (should-not (org-air-r43--terminal-divider-p fixed col)))
              ;; THE REVERT (finalize with a NIL divider column = plain
              ;; trailing-trim) demotes the | to terminal and shortens the
              ;; row — the segmented rule the user saw.
              (let ((reverted (org-air-r43--finalize-line line w nil)))
                (should (org-air-r43--terminal-divider-p reverted col))
                (should (< (string-width reverted) w))
                (should (string-suffix-p vrule reverted))))))))))

;;;; -------------------------------------------------------------------
;;;; 2. Break-point = blank-rail rows (characterization guard).
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r43-break-set-equals-blank-rail-rows ()
  "R43-1/-2 characterization: under the OLD tail the terminal-| rows equal
EXACTLY the blank-rail rows (non-empty); the fix drives that set to EMPTY.

For every pane-divider row past the rail content end (blank rail cell) the
reverted tail (finalize, nil divider column) yields a TERMINAL |, while the
divider-aware tail yields an INTERIOR one — computed on the SAME rows with
the SAME function, so the guard is decisive without a live frame.  The
blank-rail set is non-empty (there IS a break to fix) and the fix empties
the terminal set over it."
  (skip-unless (locate-library "org-air"))
  (org-air-r43--with-tall-two-pane
    (let* ((col org-air-view--pane-divider-col)
           (w (org-air-view--render-width))
           (rail-end (org-air-r43--rail-content-end col))
           (past (cl-remove-if-not
                  (lambda (r) (> (car r) rail-end))
                  (org-air-r43--divider-rows col)))
           (old-terminal 0) (new-terminal 0))
      (should (> (length past) 0))
      (dolist (row past)
        (when (org-air-r43--terminal-divider-p
               (org-air-r43--finalize-line (cdr row) w nil) col)
          (setq old-terminal (1+ old-terminal)))
        (when (org-air-r43--terminal-divider-p
               (org-air-r43--finalize-line (cdr row) w col) col)
          (setq new-terminal (1+ new-terminal))))
      (ert-info ((format "past=%d old-terminal=%d new-terminal=%d"
                         (length past) old-terminal new-terminal))
        ;; OLD tail: the whole blank-rail band breaks (terminal set ==
        ;; blank-rail set, non-empty).
        (should (= old-terminal (length past)))
        (should (> old-terminal 0))
        ;; FIX: the terminal set over those rows is EMPTY.
        (should (= new-terminal 0))))))

;;;; -------------------------------------------------------------------
;;;; 3. Header / rule untouched (R36-1 / R37 held).
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r43-header-no-divider-no-trailing-pad ()
  "R36-1 / R37 held: the banner + header rule carry NO pane divider and NO
trailing pad, so the divider-scoped finalize pad did not leak into the
header.  The banner's and rule's last visible glyph sits flush — right-
trimming them is a no-op (`string-trim-right' preserves their width)."
  (skip-unless (locate-library "org-air"))
  (org-air-r43--with-tall-two-pane
    (let* ((col org-air-view--pane-divider-col)
           (lines (org-air-viewport-test-lines))
           (banner (nth 0 lines))
           (rule (nth 1 lines)))
      ;; the header carries no pane divider …
      (should-not (org-air-view--pane-divider-line-p banner col))
      (should-not (org-air-view--pane-divider-line-p rule col))
      ;; … and no trailing whitespace (R36-1 / R37 flush contract).
      (dolist (h (list banner rule))
        (should (= (string-width (string-trim-right h))
                   (string-width h)))
        (should-not (string-match-p "[ \t]\\'" h)))
      ;; anti-tautology: line 1 really is the horizontal rule.
      (should (string-match-p (regexp-quote (org-air-view--glyph 'hrule))
                              rule)))))

;;;; -------------------------------------------------------------------
;;;; 4. Orientation scope — board-only carries no pane divider (nil column).
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r43-board-only-scope-untouched ()
  "R43-2 scope: a board-only render (below the rail threshold) passes a NIL
`org-air-view--pane-divider-col' to finalize, carries no pane divider, and
its body lines are right-trimmed (no trailing pad).  So the divider-aware
pad is inert for non-two-pane orientations — the fix touches ONLY two-pane
divider rows (D6/D7 preserved)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (cl-letf (((symbol-function 'org-air-layout-current-width)
               (lambda (&rest _) 70))       ; below `org-air-rail-min-width'
              ((symbol-function 'org-air-layout-current-height)
               (lambda (&rest _) 40)))
      (org-air-test-with-fixtures
        (org-air-viewport-test--with-frozen-now
          (org-air-viewport-test--with-render-guards
            (let ((org-air-view-width nil) (org-air-view-height nil))
              (org-air)
              (unwind-protect
                  (with-current-buffer "*org-air*"
                    (should (eq org-air-view--orientation 'board-only))
                    ;; no pane divider column is in force …
                    (should (null org-air-view--pane-divider-col))
                    ;; … no line carries a pane divider …
                    (should-not
                     (cl-some (lambda (l)
                                (org-air-view--pane-divider-line-p
                                 l (or org-air-view--pane-divider-col 0)))
                              (org-air-viewport-test-lines)))
                    ;; … and no body line carries a trailing pad (the trim
                    ;; path is untouched for board-only).
                    (should-not
                     (cl-some (lambda (l) (string-match-p "[ \t]\\'" l))
                              (org-air-viewport-test-lines))))
                (when (get-buffer "*org-air*")
                  (kill-buffer "*org-air*"))))))))))

(provide 'org-air-round43-test)
;;; org-air-round43-test.el ends here
