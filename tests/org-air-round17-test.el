;;; org-air-round17-test.el --- round-17 grind suite for org-air -*- lexical-binding: t; -*-

;;; Commentary:
;; Spec-true grinds for v0.5 round-17 (air/v0.5/org-air-round17-design.org).
;; The CRITICAL break: a long Denote filename grows the UNBOUNDED origin
;; column until the flex title collapses to a bare `TODO…' on every row.
;; Round-17 bounds the origin (`org-air-origin-max-width', truncating the
;; TEXT only -- the V6 glyph/svg cell is intact) and guarantees the title
;; `org-air-title-min-width' via a width-aware fit pass that reclaims columns
;; from the origin (down to `org-air-origin-min') then tags -- INVERTING the
;; never-wired D2 origin-protected priority to a title-protected one.
;;
;;   D-P1  origin cap + title-min budget (board).
;;   D-P2  de-slug long Denote names in the inspector + project line-2.
;;
;; The old F1 test (tests/org-air-round9-test.el) asserted only that each
;; line <= width -- which the BUG already satisfied (the title collapsed but
;; the line still fit).  These tests add the title-FLOOR guard it lacked.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)
(require 'org-air)
(require 'org-air-project)

;;;; ---------------------------------------------------------------------
;;;; Helpers.
;;;; ---------------------------------------------------------------------

(defun org-air-r17--item-row (lines)
  "Return the rendered LINES row carrying the long-Denote item, or nil.
Anchored on the `weekly' prefix of the de-slugged origin -- which is
ALWAYS present, even when the capped origin cell truncates the rest."
  (seq-find (lambda (l)
              (string-match-p "weekly"
                              (substring-no-properties l)))
            lines))

(defun org-air-r17--visible-title-width (line heading more-glyph)
  "Return the visible display width of HEADING on item-row LINE.
When HEADING appears in full, its own `string-width'; when truncated,
the width of the longest HEADING prefix present, plus the ellipsis
MORE-GLYPH; 0 when absent.  Content-anchored (the heading is a known
fixed string), so it survives the prefix/cluster geometry at any tier."
  (let ((s (substring-no-properties line)))
    (if (string-match-p (regexp-quote heading) s)
        (string-width heading)
      (let ((best 0) (i (1- (length heading))))
        (while (and (> i 0) (= best 0))
          (let ((pre (substring heading 0 i)))
            (when (string-match-p (regexp-quote (concat pre more-glyph)) s)
              (setq best (+ (string-width pre) (string-width more-glyph)))))
          (setq i (1- i)))
        best))))

(defmacro org-air-r17--with-denote-board (width &rest body)
  "Render the isolated long-Denote board at WIDTH; run BODY in its buffer.
GUI glyphs + frozen clock + the anti-tautology render guards are active.
BODY runs inside the live `*org-air*' buffer (killed afterwards)."
  (declare (indent 1) (debug t))
  `(let ((dir (make-temp-file "org-air-r17-denote-" t)))
     (unwind-protect
         (progn
           (pcase-dolist (`(,name . ,content)
                          org-air-viewport-test-denote-fixture-specs)
             (with-temp-file (expand-file-name name dir) (insert content)))
           (let ((org-air-files (directory-files dir t "\\.org\\'"))
                 (org-air-inbox-file (expand-file-name "inbox.org" dir)))
             (org-air-viewport-test-as-gui
               (org-air-viewport-test--with-frozen-now
                 (org-air-viewport-test--with-render-guards
                   (let ((org-air-view-width ,width))
                     (org-air)
                     (unwind-protect
                         (with-current-buffer "*org-air*" ,@body)
                       (when (get-buffer "*org-air*")
                         (kill-buffer "*org-air*")))))))))
       (delete-directory dir t))))

;;;; ---------------------------------------------------------------------
;;;; D-P1.C — the origin cap (`org-air-view--origin-capped').
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r17-origin-capped-defcustoms ()
  "D-P1.A: the two new knobs + the reused floor exist with the spec
defaults, and the vestigial `org-air-title-min' is an obsolete alias of
`org-air-title-min-width' (same value, old configs keep working)."
  (skip-unless (locate-library "org-air"))
  (should (boundp 'org-air-origin-max-width))
  (should (= org-air-origin-max-width 26))
  (should (boundp 'org-air-title-min-width))
  (should (= org-air-title-min-width 24))
  (should (boundp 'org-air-origin-min))
  (should (= org-air-origin-min 12))
  ;; the obsolete alias resolves to the new variable.
  (should (eq (indirect-variable 'org-air-title-min) 'org-air-title-min-width))
  (should (= org-air-title-min org-air-title-min-width)))

(ert-deftest org-air-r17-origin-capped-unit ()
  "D-P1.C: `org-air-view--origin-capped' caps a long Denote slug to
`org-air-origin-max-width' minus the reserved 2-col lead, ending with the
ellipsis glyph; a short name passes through verbatim."
  (skip-unless (locate-library "org-air"))
  (let* ((more (org-air-view--glyph 'more))
         (long (org-air-item-create
                :title "x"
                :file (concat "/x/20260614T170000--"
                              "weekly-invalidation-rate-upgrade-with-a-long-denote-slug"
                              "__work_admin.org")))
         (short (org-air-item-create :title "y" :file "/x/inbox.org"))
         (capped (org-air-view--origin-capped long))
         (plain (org-air-view--origin-capped short)))
    ;; capped to <= max-width - 2 (the `▤ ' lead is reserved separately).
    (should (<= (string-width capped) (- org-air-origin-max-width 2)))
    ;; it actually truncated (the slug is far longer) and shows the glyph.
    (should (string-suffix-p more capped))
    (should (string-match-p "\\`weekly-invalidation" capped))
    ;; the id / __tags / .org never surface.
    (should-not (string-match-p "20260614T170000\\|__work\\|\\.org" capped))
    ;; a short name is verbatim (no truncation, no ellipsis).
    (should (equal plain "inbox.org"))
    (should-not (string-suffix-p more plain))))

;;;; ---------------------------------------------------------------------
;;;; D-P1.D — the title-min budget (the guard the old F1 test lacked).
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r17-long-denote-origin-keeps-title ()
  "D-P1: a board whose item lives in a ~90-char Denote file keeps a
usable title at W80/W120/W160 -- the title's visible width is at least
`org-air-title-min-width' (or its own width when shorter), NOT a bare
`TODO…'.  The origin column is bounded by `org-air-origin-max-width', the
id/__tags/.org never surface, and no line overflows.  THIS is the guard
the line-width-only F1 test could not catch.

Review nit: the title-min budget (`org-air-view--compute-meta-widths') is
computed for the MODELED row -- `org-air-priority-style' `square' (the
fixed 2-col slot) over a BARE date label (no Inbox nudge).  Rows that
DIVERGE from that model -- a badge / text priority prefix, or the
Inbox-nudged date cell (which is wider than the bare label the budget
reserves) -- can legitimately dip a hair under title-min, so this guard
deliberately asserts on the long-Denote item, which IS the modeled row
(square slot, no priority; `:work:admin:' so its date is bare, NOT the
Inbox `· file with r' nudge).  `org-air-priority-style' is pinned to
`square' so the asserted row's prefix geometry matches the budget model
exactly and the floor is the genuine guarantee, not an accident.  (R19-2(c)
clarified that nudge to `· r to file'.)"
  (skip-unless (locate-library "org-air"))
  ;; the render runs under `org-air-viewport-test-as-gui', so anchor the
  ;; truncation on the GUI ellipsis (computing it in the batch TTY context
  ;; would give the ASCII fallback and never match the rendered glyph).
  (let ((more (org-air-viewport-test--glyph 'more 'gui))
        (heading org-air-viewport-test-denote-long-title)
        ;; modeled-row contract: the fixed square slot matches the budget's
        ;; `left-reserve' (margin+indent + reserved keyword cell + 2-col
        ;; square slot), so the long-Denote row is the row the title-min
        ;; budget actually targets.
        (org-air-priority-style 'square))
    (dolist (width '(80 120 160))
      (ert-info ((format "width %d" width))
        (org-air-r17--with-denote-board width
          (let* ((lines (org-air-viewport-test-lines))
                 (row (org-air-r17--item-row lines))
                 (text (buffer-string)))
            (should row)
            ;; the anchored row is the MODELED (bare-date, non-Inbox) row,
            ;; NOT the Inbox-nudged `File the receipts' row (R19-2(c):
            ;; the nudge is now `· r to file', was `· file with r').
            (should-not (string-match-p "r to file"
                                        (substring-no-properties row)))
            ;; (a) the title survived: its visible width >= the floor.
            (let ((vis (org-air-r17--visible-title-width row heading more))
                  (floor (min (string-width heading) org-air-title-min-width)))
              (should (>= vis floor)))
            ;; (b) the origin cell is bounded by the cap.
            (should (<= org-air-view--meta-origin-w org-air-origin-max-width))
            ;; (c) the de-slugged origin surfaces (its `weekly' prefix is
            ;;     present even when the capped cell truncates the rest);
            ;;     the id / __tags machinery never does.
            (should (string-match-p "weekly"
                                    (substring-no-properties text)))
            (should-not (string-match-p "20260614T170000"
                                        (substring-no-properties text)))
            (should-not (string-match-p "__work\\|__admin"
                                        (substring-no-properties text)))
            ;; (d) nothing overflows the composed width.
            (dolist (line lines)
              (should (<= (string-width (substring-no-properties line))
                          width)))))))))

(ert-deftest org-air-r17-compute-meta-widths-title-budget ()
  "D-P1.D: at the narrow tier with a long origin the fit pass shrinks the
origin column toward `org-air-origin-min' (it stays within
\[origin-min, origin-max]) so the implied title budget reaches
`org-air-title-min-width'.  The fit reclaims FROM the origin first --
title-protected, the inversion of the dead D2 priority."
  (skip-unless (locate-library "org-air"))
  (org-air-r17--with-denote-board 80
    ;; the origin shrank below the cap but never past the floor.
    (should (<= org-air-view--meta-origin-w org-air-origin-max-width))
    (should (>= org-air-view--meta-origin-w org-air-origin-min))
    (should (< org-air-view--meta-origin-w org-air-origin-max-width))
    ;; and the title kept at least its guaranteed minimum.
    (let* ((more (org-air-view--glyph 'more))
           (heading org-air-viewport-test-denote-long-title)
           (row (org-air-r17--item-row (org-air-viewport-test-lines)))
           (vis (org-air-r17--visible-title-width row heading more)))
      (should (>= vis org-air-title-min-width)))))

;;;; ---------------------------------------------------------------------
;;;; D-P1 #2 — the long-Denote byte golden (isolated mini-board).
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r17-denote-origin-byte-mockup ()
  "D-P1 #2: the isolated long-Denote board renders byte-for-byte equal to
the blessed goldens (denote-origin-80/120.txt), right-trimmed.  The
fixture pins `the long origin is capped AND the title survives'; it is
NOT part of the GTD board *.org set, so the 25 layout mockups stay
byte-identical.  Regenerated + blessed via the frozen-clock renderer
\(`org-air-regen--write-denote')."
  (skip-unless (locate-library "org-air"))
  (dolist (width '(80 120))
    (ert-info ((format "width %d" width))
      (let* ((file (expand-file-name (format "denote-origin-%d.txt" width)
                                     org-air-test-fixture-dir)))
        (unless (file-readable-p file)
          (ert-fail (format "missing golden %s -- test track to regen + bless via org-air-regen--write-denote"
                            (file-name-nondirectory file))))
        (let* ((expected (org-air-viewport-test--drop-trailing-blanks
                          (split-string
                           (with-temp-buffer
                             (insert-file-contents file)
                             (buffer-string))
                           "\n")))
               (actual (org-air-viewport-test-denote-board-lines width)))
          (unless (equal actual expected)
            (let ((i 0))
              (while (and (< i (length expected)) (< i (length actual))
                          (equal (nth i expected) (nth i actual)))
                (setq i (1+ i)))
              (ert-fail
               (format "denote-origin-%d diverges at line %d\nexpected: %S\nactual:   %S"
                       width (1+ i) (nth i expected) (nth i actual))))))))))

;;;; ---------------------------------------------------------------------
;;;; D-P2 — long-name safety in the inspector + project view.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r17-inspector-origin-deslugs ()
  "D-P2 (+ the design-approved no-redundant-group refinement): the
inspector origin line shows the SAME de-slugged Denote title the board
shows -- not the raw identifier--slug__tags.org -- bounded by the rail
width (pad-to: no overflow).  Per design's D-P2 sign-off
(ouyqxrnt: APPROVE inspector no-redundant-group + de-slug group):
  (a) the de-slugged title shows; the id / __tags / .org never surface;
  (b) the group breadcrumb is DROPPED when the item's group is just the
      defaulted Denote file-name-base (the redundant case), and a real
      #+CATEGORY group is shown DE-SLUGGED (never the raw slug).
Driven via the batch-safe inspector-lines seam (the live hook stays inert
in --batch)."
  (skip-unless (locate-library "org-air"))
  (let ((dir (make-temp-file "org-air-r17-insp-" t)))
    (unwind-protect
        (progn
          ;; (1) a Denote file with NO #+CATEGORY -- its group defaults to
          ;;     the Denote file-name-base, so the breadcrumb is redundant
          ;;     and must be DROPPED (no `group/' prefix).
          (with-temp-file
              (expand-file-name
               "20260614T170000--weekly-invalidation-rate-upgrade-with-a-long-denote-slug__work_admin.org"
               dir)
            (insert "* TODO Long denote note\nSCHEDULED: <2026-06-16 Tue>\n"))
          ;; (2) a Denote file with a real Denote-style #+CATEGORY -- a
          ;;     genuine category distinct from the base, shown DE-SLUGGED
          ;;     (id/__tags stripped), never the raw slug.
          (with-temp-file
              (expand-file-name
               "20260614T180000--another-long-denote-note__proj.org"
               dir)
            (insert (concat "#+CATEGORY: 20250101T000000--my-project-slug__proj\n"
                            "* TODO Cat denote note\nSCHEDULED: <2026-06-16 Tue>\n")))
          (let ((org-air-files (directory-files dir t "\\.org\\'"))
                (org-air-inbox-file (expand-file-name "inbox.org" dir)))
            (org-air-viewport-test--with-frozen-now
              (let* ((items (org-air-query-items))
                     (bare (org-air-test-find-item "Long denote note" items))
                     (catd (org-air-test-find-item "Cat denote note" items)))
                (should bare) (should catd)
                (dolist (width '(30 44 60))
                  (ert-info ((format "rail width %d" width))
                    ;; --- (a) + (b: no redundant group) on the bare item ---
                    (let* ((lines (org-air-view--inspector-lines bare width))
                           (origin (seq-find
                                    (lambda (l)
                                      (string-match-p
                                       "weekly-invalidation\\|20260614T170000"
                                       (substring-no-properties l)))
                                    lines))
                           (otext (and origin (substring-no-properties origin))))
                      (should origin)
                      ;; (a) the de-slugged title shows, machinery does not.
                      (should (string-match-p "weekly-invalidation" otext))
                      (should-not (string-match-p
                                   "20260614T170000\\|__work\\|__admin" otext))
                      ;; (b) the defaulted Denote-base group is DROPPED --
                      ;;     no `group/' breadcrumb leaks.
                      (should-not (string-match-p "/" otext))
                      ;; bounded by the rail width (no overflow).
                      (should (<= (string-width otext) width)))
                    ;; --- (b: real #+CATEGORY de-slugged) on the cat item ---
                    (let* ((lines (org-air-view--inspector-lines catd width))
                           (origin (seq-find
                                    (lambda (l)
                                      (string-match-p
                                       "my-project-slug\\|another-long\\|20260614T180000"
                                       (substring-no-properties l)))
                                    lines))
                           (otext (and origin (substring-no-properties origin))))
                      (should origin)
                      ;; the real category shows as a DE-SLUGGED breadcrumb
                      ;; (`my-project-slug/...'), never the raw slug.
                      (should (string-match-p "my-project-slug/" otext))
                      (should-not (string-match-p
                                   "20250101T000000\\|__proj\\|20260614T180000"
                                   otext))
                      ;; bounded by the rail width (no overflow).
                      (should (<= (string-width otext) width)))))))))
      (delete-directory dir t))))

(ert-deftest org-air-r17-project-line2-deslugs-leaf ()
  "D-P2: `org-air-project--deslug-relpath' de-slugs only the Denote LEAF
of a relpath (keeping the directory prefix); a non-Denote leaf passes
through verbatim.  And the two-line doc block's line 2 shows the
de-slugged leaf without overflowing the width (pad-to guard)."
  (skip-unless (locate-library "org-air"))
  ;; the leaf de-slug helper.
  (should (equal (org-air-project--deslug-relpath
                  "v0.1/20260101T120000--weekly-invalidation-rate-upgrade__work.org")
                 "v0.1/weekly-invalidation-rate-upgrade"))
  (should (equal (org-air-project--deslug-relpath "v0.1/feature-a.org")
                 "v0.1/feature-a.org"))
  ;; the rendered line 2 de-slugs and never overflows.
  (let ((doc (org-air-doc-create
              :name "Weekly invalidation rate upgrade"
              :file "/r/v0.1/20260101T120000--weekly-invalidation-rate-upgrade-long__work_admin.org"
              :state "draft" :tags '("work")
              :relpath "v0.1/20260101T120000--weekly-invalidation-rate-upgrade-long__work_admin.org")))
    (dolist (width '(40 80))
      (ert-info ((format "width %d" width))
        (with-temp-buffer
          (org-air-project--insert-doc-block doc width t)
          (let* ((lines (split-string (buffer-string) "\n"))
                 (line2 (seq-find
                         (lambda (l) (string-match-p "weekly-invalidation\\|20260101"
                                                     (substring-no-properties l)))
                         lines)))
            (should line2)
            (should (string-match-p "v0.1/weekly-invalidation"
                                    (substring-no-properties line2)))
            (should-not (string-match-p "20260101T120000\\|__work\\|__admin"
                                        (substring-no-properties line2)))
            (dolist (l lines)
              (should (<= (string-width (substring-no-properties l)) width)))))))))

(provide 'org-air-round17-test)
;;; org-air-round17-test.el ends here
