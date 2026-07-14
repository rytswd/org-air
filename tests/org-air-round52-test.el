;;; org-air-round52-test.el --- executing ERTs for v0.5 round-52 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.5 round-52 (air/v0.5/org-air-round52-design.org):
;;   R52-1 — the PROJECT dir-header letter-count rollup (`R4(+1) C14(+14)
;;     …') moves from RIGHT-JUSTIFIED at the pane width (the R22-6
;;     `org-air-view--justify' — detached out by the date column on wide
;;     frames) to LEFT-ANCHORED: the `dir/' name, a two-space gap, then
;;     the SAME summary, clamped to WIDTH with the shared `more' ellipsis
;;     (the R48-3 fold-row clamp idiom).  Tokens, faces, `(+N)' semantics
;;     and letter ORDER are untouched (`--dir-count-summary' +
;;     `--state-display-order' — the airctl `status -Da' parity contract).
;;
;; Executing renders over the round-20 air-project fixture
;; (`org-air-project-test--render-lines' at width 100) plus direct
;; `org-air-project--insert-directory-tree' temp-buffer renders (the
;; R22-6 harness shape, frozen path/mtime) and one synthetic node (the
;; R24/R25 idiom) for the overflow clamp.  Reverting R52-1 (re-justify
;; the summary to the right edge) FAILS the adjacency + width-invariance
;; tests; the clamp + token locks guard both sides.  org-ql stays the
;; only query path — nothing here touches querying.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)     ; frozen-mtime macro
(require 'org-air-project-test)         ; fixture root + --render-lines

(when (locate-library "org-air")
  (require 'org-air))

;;;; ---------------------------------------------------------------------
;;;; Helpers.
;;;; ---------------------------------------------------------------------

(defun org-air-r52--tree-header-lines (w)
  "Direct-render the fixture directory tree at width W (no rail/compose).
Return the three group-header lines (v0.1/, air-context/, v0.2/) as
RIGHT-TRIMMED plain strings, in that order.  Frozen project path + mtime,
the R22-6 harness shape."
  (let* ((docs (org-air-project--collect-docs org-air-project-test-root))
         (tree (org-air-project--directory-tree docs))
         (lines (org-air-test-with-frozen-project-path org-air-project-test-root
                  (org-air-project-test--with-frozen-mtime
                   (with-temp-buffer
                     (org-air-project--insert-directory-tree tree w)
                     (mapcar #'substring-no-properties
                             (split-string (buffer-string) "\n")))))))
    (mapcar (lambda (rx)
              (let ((l (cl-find-if (lambda (l) (string-match-p rx l)) lines)))
                (and l (string-trim-right l))))
            '("| v0\\.1/" "\\+- air-context/" "| v0\\.2/"))))

(defun org-air-r52--gap-after-name (line name-rx)
  "Return the width of the space run between NAME-RX's end and the next
non-space in LINE, or nil when NAME-RX does not match."
  (when (string-match name-rx line)
    (let* ((e (match-end 0))
           (rest (substring line e)))
      (when (string-match "\\`\\( *\\)[^ ]" rest)
        (length (match-string 1 rest))))))

;;;; ---------------------------------------------------------------------
;;;; R52-1(a) — the rollup sits ADJACENT to the group name (full render).
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r52-1-group-header-summary-adjacent ()
  "R52-1 (FAILS on trunk): in the by-directory PROJECT view at width 100
the three group-header lines carry the EXACT adjacent forms — the `dir/'
name, a TWO-space gap, then its rollup: `| v0.1/  R1 C1 D(+1)',
`+- air-context/  D1' (the NESTED child: summary adjacent to its OWN name
at its OWN indent) and `| v0.2/  W1 X1 D1'.  Anti-revert conjunct: on each
header the gap between name-end and summary-start is exactly 2 (under the
superseded right-justify it was ~40-50 pad spaces out at the date column)."
  (skip-unless (locate-library "org-air"))
  (let* ((lines (org-air-project-test--render-lines
                 'org-air-project-group-by-directory 100))
         (pick (lambda (rx) (cl-find-if (lambda (l) (string-match-p rx l)) lines)))
         (h1 (funcall pick "| v0\\.1/"))
         (hc (funcall pick "\\+- air-context/"))
         (h2 (funcall pick "| v0\\.2/")))
    (should h1) (should hc) (should h2)
    ;; The EXACT adjacent forms (spec §Golden impact NEW bytes).
    (should (string-match-p (regexp-quote "| v0.1/  R1 C1 D(+1)") h1))
    (should (string-match-p (regexp-quote "+- air-context/  D1") hc))
    (should (string-match-p (regexp-quote "| v0.2/  W1 X1 D1") h2))
    ;; Anti-revert: the name→summary gap is exactly TWO columns on every
    ;; header — top dirs AND the nested child at its own indent.
    (should (= 2 (org-air-r52--gap-after-name h1 "v0\\.1/")))
    (should (= 2 (org-air-r52--gap-after-name hc "air-context/")))
    (should (= 2 (org-air-r52--gap-after-name h2 "v0\\.2/")))))

;;;; ---------------------------------------------------------------------
;;;; R52-1(b) — the summary column no longer tracks the pane width.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r52-1-summary-column-is-width-invariant ()
  "R52-1 (FAILS on trunk): direct tree renders of the SAME fixture docs at
width 80 AND width 120 yield byte-IDENTICAL right-trimmed group-header
lines — the summary no longer tracks the right edge, so the user's
wide-frame detachment is structurally impossible.  Each right-trimmed
header ENDS with its summary at width < W.  Trunk FAILED here by
construction: `org-air-view--justify' padded every header to exactly W and
the summary column moved with W."
  (skip-unless (locate-library "org-air"))
  (let ((h80 (org-air-r52--tree-header-lines 80))
        (h120 (org-air-r52--tree-header-lines 120)))
    (should (= 3 (length h80)))
    (dolist (h h80) (should h))
    (dolist (h h120) (should h))
    ;; Byte-identical across widths (right-trimmed).
    (should (equal h80 h120))
    ;; Each header ENDS with its summary, at width strictly < W for BOTH
    ;; renders (headers are no longer padded out to the pane width).
    (dolist (h h80)
      (should (string-match-p "[0-9)]\\'" h))
      (should (< (length h) 80))
      (should (< (length h) 120)))))

;;;; ---------------------------------------------------------------------
;;;; R52-1(c) — overflow clamps at WIDTH with the `more' ellipsis (LOCK).
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r52-1-long-name-header-clamps ()
  "R52-1 LOCK (new-behaviour fence; also held on trunk via justify's own
truncation — asserted so the clamp can never be dropped now that the
justify call is gone): a synthetic dir node (the R24/R25 synthetic-tree
idiom) whose guide + name + gap + summary exceeds a small W emits a header
of `string-width' exactly W ending with the shared `more' glyph — the
header can never spill past the pane or move the divider.  Degenerate
reading is name-first (the summary tail clips): the NAME is the identity,
the rollup the annotation."
  (skip-unless (locate-library "org-air"))
  (let* ((w 30)
         (name "an-extremely-long-directory-name-that-overflows-the-pane")
         (node (list :dir name :depth 0 :path name :own-docs nil
                     :direct-counts '(("ready" . 4) ("complete" . 14))
                     :desc-counts '(("ready" . 1) ("complete" . 14))
                     :children nil))
         (line (with-temp-buffer
                 (org-air-project--insert-dir-node node w nil t)
                 (substring-no-properties
                  (car (split-string (buffer-string) "\n"))))))
    ;; Sanity: the unclamped composition really would overflow W.
    (should (> (+ (length name) 1) w))
    ;; The emitted header is exactly W wide and ends with the ellipsis.
    (should (= (string-width line) w))
    (should (string-suffix-p (org-air-view--glyph 'more) line))))

;;;; ---------------------------------------------------------------------
;;;; R52-1(d) — the rollup TOKENS did not move with the position (LOCK).
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r52-1-rollup-tokens-unchanged ()
  "R52-1 LOCK (passes trunk; anti-scope-creep): the rollup BUILDER is
untouched — `org-air-project--dir-count-summary' over the R22-6 airctl
vector still yields exactly `R4(+1) C14(+14) X1(+9) D2(+8)': tokens,
letter ORDER (`org-air-project--state-display-order', the airctl `status
-Da' parity contract) and `(+N)' semantics did NOT move with the position."
  (skip-unless (locate-library "org-air"))
  (should (equal (substring-no-properties
                  (org-air-project--dir-count-summary
                   '(("ready" . 4) ("complete" . 14) ("dropped" . 1) ("draft" . 2))
                   '(("ready" . 1) ("complete" . 14) ("dropped" . 9) ("draft" . 8))))
                 "R4(+1) C14(+14) X1(+9) D2(+8)")))

(provide 'org-air-round52-test)
;;; org-air-round52-test.el ends here
