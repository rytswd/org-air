;;; org-air-round40-test.el --- R40 regression fence -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Round-40 executing regression fence (air/v0.5/org-air-round40-design.org).
;; Two EXECUTING, revert-fails guards over the R40 impl (tips 60dd39c4 +
;; 1a1a4484), pinned against the LIVE renderer — no fixture bytes are trusted
;; here, every assertion probes the actual composed columns:
;;
;;   R40-1  HEADER SEPARATOR RULE SYMMETRY.  `org-air-view--insert-rule' now
;;          reserves a RIGHT gutter equal to its LEFT margin
;;          (`org-air-view--banner-indent' = `org-air-margin' = 2) instead of
;;          filling flush to the right edge, so the `─' rule spans EXACTLY the
;;          R39-1 symmetric banner content columns (`indent' .. `usable -
;;          indent').  Guard: the rule's first/last non-space column ==
;;          the banner content's (line 0); lhs-margin == rhs-gutter ==
;;          banner-indent; reverting to a flush-right rule (0-col right
;;          gutter) FAILS.
;;
;;   R40-2  NO-RAIL FENCE CONTINUITY.  `org-air-view--insert-row' now
;;          right-anchors the standard board row to the ONE shared board-wide
;;          `org-air-view--fence-column' (no-arg form, derived from the
;;          board-wide `meta-cluster-width') rather than the per-row CLUSTER-W,
;;          so the fence is CONTINUOUS BY CONSTRUCTION.  Guard: with DIVERGENT
;;          per-row cluster widths the fence column is IDENTICAL on every board
;;          row (incl. blank/fill/separator rows), and equals the shared no-arg
;;          fence column; reverting to per-row CLUSTER-W anchoring makes the
;;          divergent row's column disagree (fence varies row-to-row) → FAILS.

;;; Code:

(require 'ert)
(require 'org-air-viewport-helpers)

;;;; =====================================================================
;;;; R40-1 — the header separator rule is left/right symmetric.
;;;; =====================================================================

(defun org-air-r40--first-col (s)
  "Return the 0-indexed display column of the first non-space glyph in S."
  (- (string-width s) (string-width (string-trim-left s))))

(defun org-air-r40--last-col (s)
  "Return the 0-indexed display column of the last non-space glyph in S."
  (1- (string-width (string-trim-right s))))

(ert-deftest org-air-r40-1-rule-margins-symmetric ()
  "R40-1: the header separator rule spans EXACTLY the banner content columns.
Rendered headless over the fixtures at several widths, the `─' rule
(`org-air-view--insert-rule', buffer line 1) must reserve a SYMMETRIC
gutter equal to its left margin — so its first-non-space column ==
`org-air-view--banner-indent' AND its last-non-space column ==
`usable - banner-indent - 1' (i.e. right gutter == banner-indent), and
that span is IDENTICAL to the banner content span (line 0).  Reverting to
the pre-R40 flush-right rule (right gutter 0, running `banner-indent'
columns past the content) fails the symmetric-gutter assertions — proven
in-process by re-composing the old flush-to-W rule and showing it violates
the contract this test enforces."
  (skip-unless (locate-library "org-air"))
  (should (fboundp 'org-air-view--insert-rule))
  (dolist (width '(70 96 120 160))
    (org-air-viewport-test-with-dashboard width
      (let* ((usable (org-air-view--render-width))
             (indent org-air-view--banner-indent)
             (lines (org-air-viewport-test-lines))
             (banner (nth 0 lines))
             (rule (nth 1 lines))
             (b-first (org-air-r40--first-col banner))
             (b-last (org-air-r40--last-col banner))
             (r-first (org-air-r40--first-col rule))
             (r-last (org-air-r40--last-col rule))
             ;; the rule's right gutter, measured from the last usable column.
             (r-right-gutter (- (1- usable) r-last))
             ;; the horizontal-rule glyph (`─' on GUI, `-' in batch).
             (hrule (org-air-view--glyph 'hrule)))
        (ert-info ((format
                    "width=%d usable=%d indent=%d banner[%d..%d] rule[%d..%d] gutter=%d rule=%S"
                    width usable indent b-first b-last r-first r-last
                    r-right-gutter rule))
          ;; sanity: line 1 really is a horizontal rule (anti-tautology).
          (should (string-match-p (regexp-quote hrule) rule))
          ;; the rule never overshoots the usable width.
          (should (<= (string-width rule) usable))
          ;; LEFT: the rule starts at the banner-indent margin — the same
          ;; column as the banner content (`org-air').
          (should (= r-first indent))
          (should (= r-first b-first))
          ;; RIGHT (the R40-1 fix): a SYMMETRIC right gutter == banner-indent,
          ;; so the rule ends where the banner content ends — no overshoot.
          (should (= r-right-gutter indent))
          (should (= r-last (- usable indent 1)))
          (should (= r-last b-last))
          ;; lhs-margin == rhs-gutter (the whole point of R40-1).
          (should (= r-first r-right-gutter))
          ;; the rule span EQUALS the banner content span.
          (should (and (= r-first b-first) (= r-last b-last)))
          ;; REVERT GUARD: the pre-R40 flush-right rule (glyph run =
          ;; usable - margin, prefixed by the margin) runs `banner-indent'
          ;; columns PAST the content — its last glyph sits on the final
          ;; usable column with a 0-col right gutter, so it FAILS the
          ;; symmetric contract asserted above.
          (let* ((margin (make-string org-air-margin ?\s))
                 (flush (concat margin
                                (org-air-view--rule-string
                                 (- usable org-air-margin))))
                 (f-last (org-air-r40--last-col flush))
                 (f-gutter (- (1- usable) f-last)))
            (should (= f-last (1- usable)))       ; flush to the last column…
            (should (= f-gutter 0))               ; …NO right gutter…
            (should-not (= f-gutter indent))      ; …i.e. NOT symmetric.
            (should-not (= f-last r-last))))))))   ; 2 cols longer than R40.

;;;; =====================================================================
;;;; R40-2 — the no-rail fence is one shared board-wide column.
;;;; =====================================================================

(defun org-air-r40--cluster-anchor-column ()
  "Return the shared column every dated board row's metadata cluster
right-anchors to (the DATE cell is the left-most cluster cell, so its
start column IS the fence column), or nil unless all dated rows agree."
  (let (cols)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (when (string-match " \\(OVERDUE [0-9]+d\\|Today\\|Tomorrow\\)" line)
            (push (string-width (substring line 0 (match-beginning 1))) cols)))
        (forward-line 1)))
    (setq cols (delete-dups (nreverse cols)))
    (and cols (= (length cols) 1) (car cols))))

(ert-deftest org-air-r40-2-fence-continuous-under-divergent-cluster ()
  "R40-2: the no-rail board fence is ONE shared board-wide column, enforced
by construction — NOT a per-row coincidence.

Part A (the decisive revert-fails contract, driven with DIVERGENT per-row
cluster widths): `org-air-view--insert-row' on the standard board path
(OWN-FENCE nil) right-anchors EVERY row to the shared no-arg
`org-air-view--fence-column' regardless of the row's OWN cluster width, so
the metadata cluster begins at the IDENTICAL column on every row even when
the rows' natural cluster widths diverge.  The pre-R40 per-row anchoring
\(`(org-air-view--fence-column width row-cluster-w)') would place each
divergent row at `width - row-cluster-w' — a DIFFERENT column per row —
which this asserts would break the continuous line.

Part B (the live board lockstep): rendered board-only (no-rail), every
dated row's cluster anchors to the shared column, that column ==
`(org-air-view--fence-column (org-air-view--render-width))' (no-arg), and
`org-air-view--meta-cluster-width' == every rendered row's cluster field
width (lockstep — no local expansion can shove the fence)."
  (skip-unless (locate-library "org-air"))
  (should (fboundp 'org-air-view--insert-row))
  (should (fboundp 'org-air-view--fence-column))
  (should (fboundp 'org-air-view--meta-cluster-width))

  ;; ---- Part A: DIVERGENT per-row cluster widths, board path -----------
  (with-temp-buffer
    (let* ((org-air-view--line-width 70)
           ;; fixed board-wide metadata columns → a fixed shared fence.
           (org-air-view--meta-date-w 8)
           (org-air-view--meta-date-repeat 0)
           (org-air-view--meta-tags-w 6)
           (org-air-view--meta-origin-w 0)
           (width (org-air-view--render-width))
           ;; the ONE shared, no-arg fence column (board-wide).
           (shared (org-air-view--fence-column width))
           ;; per-row WIDGET specs whose (dcol tcol ocol) sums DIVERGE — a
           ;; genuinely divergent natural cluster width per row.  Titles are
           ;; short so the pad never floors at `gap' (cluster lands at the
           ;; anchor exactly).  The `@' marks the cluster's first glyph.
           (specs '((8 6 0) (12 10 4) (4 2 0) (10 8 6)))
           new-cols old-cols)
      (dolist (spec specs)
        (let ((beg (point)))
          (org-air-view--insert-row
           :prefix "" :title "Row"
           :date-text "@d" :tags "#x"
           :origin-text (when (> (nth 2 spec) 0) "@o")
           :widths spec)
          ;; NEW code (board path): the cluster's first glyph `@' lands on
          ;; the SHARED column regardless of this row's cluster width.
          (let ((line (buffer-substring-no-properties
                       beg (1- (point)))))
            (push (string-width (substring line 0 (string-match "@" line)))
                  new-cols))
          ;; OLD code would have anchored to `width - row-cluster-w' — the
          ;; per-row cluster width (sum of the fixed cells + one sep each).
          (let* ((cells (delq nil (list (and (> (nth 0 spec) 0) (nth 0 spec))
                                        (and (> (nth 1 spec) 0) (nth 1 spec))
                                        (and (> (nth 2 spec) 0) (nth 2 spec)))))
                 (row-cluster-w (+ (apply #'+ cells)
                                   (max 0 (1- (length cells))))))
            (push (org-air-view--fence-column width row-cluster-w) old-cols))))
      (setq new-cols (nreverse new-cols)
            old-cols (nreverse old-cols))
      (ert-info ((format "shared=%d new-cols=%S old-cols=%S" shared new-cols old-cols))
        ;; anti-tautology: the specs really do diverge (>1 distinct old col).
        (should (> (length (delete-dups (copy-sequence old-cols))) 1))
        ;; THE CONTRACT: every board row anchors to the ONE shared column…
        (should (= (length (delete-dups (copy-sequence new-cols))) 1))
        (should (= (car new-cols) shared))
        (should (cl-every (lambda (c) (= c shared)) new-cols))
        ;; REVERT GUARD: per-row anchoring would put the divergent rows on
        ;; DIFFERENT columns → the fence would NOT be continuous.
        (should-not (cl-every (lambda (c) (= c shared)) old-cols)))))

  ;; ---- Part B: the LIVE board-only render, lockstep -------------------
  (org-air-viewport-test-with-dashboard 70
    ;; below `org-air-rail-min-width' → board-only, the no-rail path.
    (should (eq org-air-view--orientation 'board-only))
    (let* ((width (org-air-view--render-width))
           (shared (org-air-view--fence-column width))
           (anchor (org-air-r40--cluster-anchor-column)))
      (ert-info ((format "width=%d shared=%d anchor=%S meta-cluster-w=%d"
                         width shared anchor (org-air-view--meta-cluster-width)))
        ;; (1) every dated row shares ONE cluster column (anti-tautology: a
        ;; real dated row column was measured and they all agree).
        (should (integerp anchor))
        ;; (2) that column == the shared no-arg fence column.
        (should (= anchor shared))
        (should (= shared (- width (org-air-view--meta-cluster-width))))
        ;; (3) lockstep: the meta-cluster-width the fence uses is exactly the
        ;; rendered cluster field width — the cluster's right edge sits at
        ;; the usable width, so its left edge (the fence) at `width - mcw'.
        (should (= (org-air-view--meta-cluster-width) (- width anchor)))
        ;; REVERT GUARD (seam drift): a fence read from a DIFFERENT width no
        ;; longer lands on the live cluster anchor.
        (should-not (= (org-air-view--fence-column (1- width)) anchor))
        (should-not (= (org-air-view--fence-column (1+ width)) anchor))))))

(provide 'org-air-round40-test)
;;; org-air-round40-test.el ends here
