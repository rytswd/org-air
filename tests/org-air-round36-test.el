;;; org-air-round36-test.el --- R36 acceptance ERTs -*- lexical-binding: t; -*-

;;; Commentary:
;; Round-36 acceptance tests (air/v0.5/org-air-round36-design.org), EXECUTING
;; and deterministic in batch (no real timers, no font pixels).
;;
;; R36-1  HEADER TRAILING-MARGIN OVERFLOW.  The board header AND the loading-
;;        skeleton header share `org-air-view--insert-banner', which used to
;;        right-align the status to column W-2 and then APPEND a reserved
;;        one-column right margin (the S7 mirror of the visible left margin):
;;        `(org-air-view--justify left (concat right " ") w)' with
;;        `budget = (- w (string-width left) 2 1)'.  The composed line filled
;;        `window-body-width' EXACTLY, but its LAST column was a blank — on
;;        the user's fringed frame R34's usable-columns already gives the
;;        whole body, so that hand-reserved blank landed on the clipped last
;;        column (the "trailing empty space that's OFF the window" + the `↦'
;;        continuation arrow).  R36-1 dropped the appended " " and the
;;        reserved `1', so the status' last glyph sits on column W-1 (the
;;        true last usable column) with NO trailing pad; S7's spare column is
;;        now supplied UPSTREAM by R34's fringe-aware usable-columns.
;;
;; These are the DECISIVE headless guards: they compose the ACTUAL header
;; line (both header flavours, several body widths incl. the user's 191) and
;; assert the string fits the width, carries no trailing whitespace past the
;; right content, keeps the left margin, and sits FLUSH on the last usable
;; column.  Reconstructing the reverted (reserved-trailing-margin) contract
;; in-process shows the guard FAILS on it (non-tautological).  R34's
;; usable-columns model is re-asserted intact.

;;; Code:

(require 'ert)
(require 'org-air)

(defconst org-air-r36--banner-left "  org-air"
  "The literal left token `org-air-view--insert-banner' emits.
Two leading margin spaces (S7 visible left margin) + `org-air'.")

(defun org-air-r36--compose-header (width &optional loading)
  "Return the ACTUAL composed banner line at WIDTH (no text properties).
When LOADING is non-nil the LOADING-SKELETON flavour is composed (the
count slot shows the `loading…' cue); otherwise the normal BOARD header.
Both flavours flow through the SAME `org-air-view--insert-banner' choke
point, so this exercises the real production composer for each."
  (with-temp-buffer
    (let ((org-air-view--line-width width)
          (org-air-view--loading loading))
      (org-air-view--insert-banner nil)
      (goto-char (point-min))
      (buffer-substring-no-properties (line-beginning-position)
                                      (line-end-position)))))

(defun org-air-r36--assert-header-fits (line width flavour)
  "Assert LINE (a composed header of FLAVOUR) obeys the R39-1 contract at WIDTH.
Contract: character length <= WIDTH; display width <= WIDTH; NO trailing
whitespace past the right content; the S7 left margin is still present
INSIDE the width; and (R39-1) the right content ends `org-air-view--banner-
indent' columns before WIDTH — a symmetric right gutter mirroring the left
indent.  So the trimmed AND raw widths equal WIDTH - banner-indent (no
trailing whitespace emitted; the gutter stays inside WIDTH)."
  (ert-info ((format "%s header @ width %d: %S" flavour width line))
    ;; fits the window body width — both the raw length and the display width.
    (should (<= (length line) width))
    (should (<= (string-width line) width))
    ;; NO trailing whitespace past the right content (the R36-1 core: no
    ;; reserved trailing-margin column off the usable edge).
    (should-not (string-match-p "[ \t]+$" line))
    ;; the S7 left margin (2 spaces) is still present, inside the width.
    (should (string-prefix-p "  " line))
    (should (string-prefix-p org-air-r36--banner-left line))
    ;; R39-1: the right content ends banner-indent columns before WIDTH
    ;; (symmetric right gutter); no trailing whitespace is emitted, so the
    ;; trimmed AND raw widths both equal WIDTH - banner-indent.
    (should (= (string-width (string-trim-right line))
               (- width org-air-view--banner-indent)))
    (should (= (string-width line)
               (- width org-air-view--banner-indent)))))

;;;; =====================================================================
;;;; R36-1 — the decisive composed-line guard for BOTH headers.
;;;; =====================================================================

(ert-deftest org-air-r36-1-header-no-reserved-trailing-margin ()
  "BOARD header AND LOADING-SKELETON header, across several window-body-
widths (incl. the user's 191): the ACTUAL composed line fits the width,
carries no trailing whitespace past the right content, keeps the S7 left
margin, and sits FLUSH on the last usable column (no reserved trailing-
margin column off the usable edge)."
  (skip-unless (locate-library "org-air"))
  (dolist (width '(40 63 80 96 100 120 160 191 200))
    ;; BOARD header (loading nil).
    (org-air-r36--assert-header-fits
     (org-air-r36--compose-header width nil) width "BOARD")
    ;; LOADING-SKELETON header (loading t — the cold fast-paint chrome).
    (org-air-r36--assert-header-fits
     (org-air-r36--compose-header width t) width "LOADING-SKELETON")))

(ert-deftest org-air-r36-1-reverting-the-fix-fails-the-guard ()
  "NON-TAUTOLOGY: reconstruct the PRE-R36 reserved-trailing-margin contract
in-process — `(org-air-view--justify left (concat status \" \") w)' — and
show it VIOLATES the R36-1 guard (a trailing blank final column, so the
right content is NOT flush).  If the impl reverted to that composition the
`org-air-r36-1-header-no-reserved-trailing-margin' guard would FAIL, which
is exactly what this asserts.  For each width the ACTUAL fixed line passes."
  (skip-unless (locate-library "org-air"))
  (dolist (width '(40 80 120 191))
    (let* ((fixed (org-air-r36--compose-header width nil))
           ;; recover the composed status (everything right of the left
           ;; token and the middle gap) from the fixed line…
           (status (string-trim (substring fixed (length org-air-r36--banner-left))))
           ;; …and re-compose the OLD reserved-trailing-margin line.
           (reverted (org-air-view--justify
                      org-air-r36--banner-left (concat status " ") width)))
      (ert-info ((format "width %d: fixed=%S reverted=%S" width fixed reverted))
        ;; the FIXED line obeys the contract (sanity: same as the main guard).
        (org-air-r36--assert-header-fits fixed width "BOARD")
        ;; the REVERTED line has a trailing blank final column…
        (should (string-match-p "[ \t]+$" reverted))
        ;; …so its right content is NOT flush to the last usable column…
        (should (< (string-width (string-trim-right reverted)) width))
        ;; …i.e. the R36-1 guard rejects it (proving the guard is real).
        (should-error (org-air-r36--assert-header-fits reverted width "REVERTED")
                      :type 'ert-test-failed)))))

(ert-deftest org-air-r36-1-right-padded-chrome-lines-do-not-overshoot ()
  "The other right-padded full-width chrome lines that ride the same render
width must not overshoot: the S7 rule (`org-air-view--insert-rule') must
not run PAST the render width.  R41-1 widened the contract — the rule now
spans the FULL usable width (`0' .. `usable', flush to BOTH text-area
edges), `org-air-view--banner-indent' (2) columns wider on EACH side than
the R40-1 inset rule, while the R39-1 banner content stays inset.  This
asserts the full-width contract: no overshoot AND flush to both edges (no
leading margin, last glyph on the final usable column) — the R40 inset
rule, with a banner-indent margin each side, would fail here."
  (skip-unless (locate-library "org-air"))
  (dolist (width '(40 80 120 191 200))
    (with-temp-buffer
      (let ((org-air-view--line-width width))
        (org-air-view--insert-rule)
        (goto-char (point-min))
        (let* ((rule (buffer-substring-no-properties (line-beginning-position)
                                                     (line-end-position)))
               (margin org-air-margin)
               (rulew (string-width rule))
               ;; leading blank columns before the rule's first glyph.
               (left-margin (- rulew (string-width (string-trim-left rule))))
               ;; columns of blank between the rule's last glyph and W.
               (right-gutter (- width (string-width (string-trim-right rule)))))
          (ert-info ((format "rule @ width %d: %S" width rule))
            ;; never overshoots the render width…
            (should (<= rulew width))
            ;; …and R41-1: fills the full usable width, flush to W.
            (should (= rulew width))
            ;; LEFT: no leading margin — the rule is flush to the left edge.
            (should (= left-margin 0))
            (should-not (string-prefix-p (make-string margin ?\s) rule))
            ;; RIGHT: flush to the last usable column — no right gutter.
            (should (= right-gutter 0))
            ;; the glyph run spans the FULL usable width (0 .. usable),
            ;; 2*margin wider than the pre-R41 inset run (width - 2*margin).
            (should (= rulew width))
            (should (= (string-width (string-trim rule)) width))))))))

;;;; =====================================================================
;;;; R36-1 — R34's usable-columns model is preserved verbatim (fenced).
;;;; =====================================================================

(ert-deftest org-air-r36-1-r34-usable-columns-guard-preserved ()
  "R36-1 supplies the S7 spare column UPSTREAM via the shared
`org-air-layout--usable-columns-for'; that model must stay intact.  R37
made the reserve UNIVERSAL: EVERY graphical frame reserves exactly one
right column (the fringed+scrollbar case that clipped this user, the
fringe-less continuation column, and the partial-cell rounding case); a
TTY/mock uses the plain body — and usable NEVER exceeds body.  (Fenced so
an R36 refactor that disturbs the model trips here too.)"
  (skip-unless (locate-library "org-air"))
  (should (fboundp 'org-air-layout--usable-columns-for))
  ;; fringed graphical (R37 re-bless 191->190): the last body column clips
  ;; under the right scroll bar / is a partial cell -> reserve one.
  (should (= (org-air-layout--usable-columns-for t 191 8) 190))
  ;; fringe-less graphical: reserve exactly one column.
  (should (= (org-air-layout--usable-columns-for t 191 0) 190))
  (should (= (org-air-layout--usable-columns-for t 80 nil) 79))
  ;; TTY / mock: the plain body width regardless of the fringe.
  (should (= (org-air-layout--usable-columns-for nil 191 0) 191))
  (should (= (org-air-layout--usable-columns-for nil 80 8) 80))
  ;; the invariant: usable NEVER exceeds body, never below one column.
  (dolist (graphic '(t nil))
    (dolist (body '(1 2 40 80 120 191 200))
      (dolist (rf '(nil 0 1 8))
        (let ((u (org-air-layout--usable-columns-for graphic body rf)))
          (should (<= u body))
          (should (>= u 1)))))))

(provide 'org-air-round36-test)
;;; org-air-round36-test.el ends here
