;;; org-air-screenshot-regression-test.el --- S1/S4/glyph regression surface -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression surface for the GUI-screenshot bugs the byte gate missed
;; (screenshot round):
;;
;;   S1 — duplicate header: the header-line path was still active in
;;        GUI.  Batch cannot RENDER a header-line, but it CAN assert the
;;        variable: `header-line-format' must be nil in a rendered
;;        dashboard — the in-buffer header band is the only header.
;;   S4 — badge/section count disagreement: for every section, the
;;        count badge must equal the item rows actually rendered (plus
;;        any "…and K more" remainder), and the empty-state line may
;;        appear only when the count is 0.  Table-driven across both
;;        boards and filter states.
;;   Glyph coverage — every spec'd glyph (design §6.1 + v0.1 set) has a
;;        GUI char and a non-empty pure-ASCII TTY fallback, so no width
;;        surprises and full coverage in emacs -nw / pipes.
;;
;; All assertions are render-byte-independent (text properties, the
;; header-line variable, the glyph table) and survive visual respecs.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)

(when (locate-library "org-air")
  (require 'org-air))

;;;; S1 — single header surface.

(ert-deftest org-air-s1-no-header-line-in-dashboard ()
  "A rendered dashboard never sets `header-line-format'.
The in-buffer header band (design §1.1/§2) is the only header; a
non-nil header-line duplicates it in GUI (invisible to batch renders,
hence this variable-level assertion)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 120
    (should (null header-line-format)))
  ;; The GUI glyph path must not re-introduce it either.
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 120
      (should (null header-line-format)))))

;;;; S4 — badge/rows consistency invariant.

(defun org-air-s4--sections ()
  "Parse rendered sections: ((BUCKET BADGE ROWS MORE EMPTY-P) ...).
A section spans from a heading carrying `org-air-count-badge' to the
next such heading (or buffer end).  ROWS counts distinct
`org-air-item' regions inside; MORE is the K from \"and K more\";
EMPTY-P is whether an `org-air-face-empty' line is present."
  (let ((headings ()) (pos (point-min)))
    (while (< pos (point-max))
      (when (get-text-property pos 'org-air-count-badge)
        (push (cons pos (list (get-text-property pos 'org-air-section)
                              (get-text-property pos 'org-air-count-badge)))
              headings))
      (setq pos (or (next-single-property-change pos 'org-air-count-badge)
                    (point-max))))
    (setq headings (nreverse headings))
    (cl-loop for (start . (bucket badge)) in headings
             for rest on headings
             for end = (or (car-safe (cadr rest)) (point-max))
             collect
             (let ((rows 0) (more 0) (emptyp nil) (p start) (last-item nil))
               ;; Count distinct item regions.
               (while (< p end)
                 (let ((item (get-text-property p 'org-air-item)))
                   (when (and item (not (eq item last-item)))
                     (cl-incf rows))
                   (setq last-item item))
                 (setq p (or (next-single-property-change p 'org-air-item nil end)
                             end)))
               ;; Remainder note and empty-state line.
               (let ((text (buffer-substring start end)))
                 (when (string-match "and \\([0-9]+\\) more" text)
                   (setq more (string-to-number (match-string 1 text))))
                 (setq emptyp
                       (org-air-s4--face-in-string-p 'org-air-face-empty text)))
               (list bucket badge rows more emptyp)))))

(defun org-air-s4--face-in-string-p (face string)
  "Return non-nil when FACE is applied anywhere in STRING."
  (let ((pos 0) (len (length string)) found)
    (while (and (< pos len) (not found))
      (let ((value (get-text-property pos 'face string)))
        (when (or (eq value face) (and (listp value) (memq face value)))
          (setq found t)))
      (setq pos (or (next-single-property-change pos 'face string) len)))
    found))

(defun org-air-s4--assert-consistent ()
  "Assert the S4 invariant for every rendered section."
  (let ((sections (org-air-s4--sections)))
    (should (= (length sections) 5))
    (pcase-dolist (`(,bucket ,badge ,rows ,more ,emptyp) sections)
      (ert-info ((format "section %s: badge=%d rows=%d more=%d empty=%s"
                         bucket badge rows more emptyp))
        (if (zerop badge)
            (progn (should (zerop rows))
                   (should (zerop more))
                   (should emptyp))
          (should (= badge (+ rows more)))
          (should-not emptyp))))))

(ert-deftest org-air-s4-badge-row-consistency ()
  "Badge == rendered rows + remainder; empty state iff badge is 0.
Table-driven: canonical and data-variation boards, with and without
filters, including a filter that hides everything."
  (skip-unless (locate-library "org-air"))
  ;; Canonical board: unfiltered, narrowing filter, all-hiding filter.
  (dolist (filter '(nil ("work") ("org-air-no-such-tag")))
    (ert-info ((format "canonical board, filter %S" filter))
      (org-air-viewport-test-with-dashboard 120
        (when filter (org-air-filter filter))
        (org-air-s4--assert-consistent))))
  ;; Data-variation board: unfiltered and filtered.
  (dolist (filter '(nil ("kitchen")))
    (ert-info ((format "alt board, filter %S" filter))
      (org-air-viewport-test-with-alt-dashboard 120
        (when filter (org-air-filter filter))
        (org-air-s4--assert-consistent)))))

;;;; S5a — point lands on a visible character on EVERY point-moving path.

(defun org-air-s5a--point-on-visible-char-p ()
  "Non-nil when point sits somewhere meaningful for a user:
on an item/section property, or on a non-whitespace character."
  (or (get-text-property (point) 'org-air-item)
      (get-text-property (point) 'org-air-section)
      (let ((c (char-after)))
        (and c (not (memq c '(?\s ?\t ?\n)))))))

(ert-deftest org-air-s5a-point-on-visible-char-all-paths ()
  "Point ends on a visible char after EVERY point-moving path — first
open, refresh, item-anchored refresh, filter apply/clear, and a resize
re-render — not just the first open (screenshot-3 finding 2)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 120
    ;; (a) first open.
    (ert-info ("first open")
      (should (org-air-s5a--point-on-visible-char-p)))
    ;; (b) plain refresh from wherever point starts.
    (ert-info ("refresh from initial point")
      (org-air-refresh)
      (should (org-air-s5a--point-on-visible-char-p)))
    ;; (c) refresh anchored on an item row.
    (ert-info ("refresh anchored on item")
      (goto-char (point-min))
      (should (search-forward "Prepare standup notes" nil t))
      (goto-char (match-beginning 0))
      (org-air-refresh)
      (should (org-air-s5a--point-on-visible-char-p)))
    ;; (d) filter apply: the anchored item survives the narrowing.
    (ert-info ("filter apply")
      (org-air-filter '("work"))
      (should (org-air-s5a--point-on-visible-char-p)))
    ;; (e) filter apply that HIDES the anchored item.
    (ert-info ("filter hides anchor")
      (org-air-filter '("org-air-no-such-tag"))
      (should (org-air-s5a--point-on-visible-char-p)))
    ;; (f) filter clear.
    (ert-info ("filter clear")
      (org-air-filter-clear)
      (should (org-air-s5a--point-on-visible-char-p)))
    ;; (g) resize re-render (width change path).
    (ert-info ("resize re-render")
      (let ((org-air-view-width 100))
        (org-air-view--resize-refresh))
      (should (org-air-s5a--point-on-visible-char-p)))))

;;;; Glyph coverage.

(defconst org-air-screenshot-test--spec-glyphs
  '(origin inbox attention upcoming high-priority stale
    calendar-item today clear more
    vrule hrule cal-prev cal-next
    box-top-left box-top-right box-bottom-left box-bottom-right
    box-tee-left box-tee-right)
  "Every glyph name the design spec requires (v0.1 set + §6.1 table).")

(ert-deftest org-air-glyph-coverage-complete-with-ascii-fallbacks ()
  "Every spec'd glyph conforms to the reconciled S5b shape:
`org-air-glyphs' holds a (PREFERRED . ASCII) cons per name (both
non-empty, ASCII side pure-ASCII); the optional SAFE middle tier lives
in `org-air-layout-safe-glyphs', whose names must be a subset of the
spec'd set with non-empty string values."
  (skip-unless (boundp 'org-air-glyphs))
  (dolist (name org-air-screenshot-test--spec-glyphs)
    (ert-info ((format "glyph %s" name))
      (let ((entry (cdr (assq name org-air-glyphs))))
        (should (consp entry))
        (should (stringp (car entry)))
        (should (> (length (car entry)) 0))
        (should (stringp (cdr entry)))
        (should (> (length (cdr entry)) 0))
        ;; The fallback side must be ASCII-only: safe in every terminal.
        (should (string-match-p "\\`[[:ascii:]]+\\'" (cdr entry))))))
  (when (boundp 'org-air-layout-safe-glyphs)
    (pcase-dolist (`(,name . ,safe) org-air-layout-safe-glyphs)
      (ert-info ((format "safe glyph %s" name))
        (should (memq name org-air-screenshot-test--spec-glyphs))
        (should (stringp safe))
        (should (> (length safe) 0))))))

(provide 'org-air-screenshot-regression-test)
;;; org-air-screenshot-regression-test.el ends here
