;;; org-air-screenshot-regression-test.el --- S1/S4/glyph regression surface -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression surface for the GUI-screenshot bugs the byte gate missed
;; (screenshot round):
;;
;;   S1 — duplicate header: the GUI header-line must never DUPLICATE the
;;        in-buffer banner.  Original intent was "no second banner", first
;;        encoded as `header-line-format' nil.  T7 (buffer-box, design
;;        ytvztszk option A) may legitimately put the frame's TOP BORDER
;;        in `header-line-format' — chrome, not a banner.  So S1 now
;;        asserts: the in-buffer banner exists exactly once, and the
;;        header-line, IF set, is the frame border (box chrome), never a
;;        banner duplicate.  Batch cannot RENDER a header-line, so the
;;        construct is walked structurally (format-mode-line is empty in
;;        --batch).
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

;;;; S1 — single header surface (no DUPLICATE banner; T7 border allowed).

(defun org-air-s1--construct-atoms (construct)
  "Return a flat list of every string and symbol inside mode-line CONSTRUCT.
Descends conses, `:eval'/`:propertize' forms and lists alike — used to
inspect `header-line-format' structurally (format-mode-line yields the
empty string in --batch)."
  (cond
   ((stringp construct) (list (substring-no-properties construct)))
   ((symbolp construct) (if construct (list construct) nil))
   ((consp construct)
    (append (org-air-s1--construct-atoms (car construct))
            (org-air-s1--construct-atoms (cdr construct))))
   (t nil)))

(defun org-air-s1--banner-count ()
  "Number of in-buffer banner bands in the current dashboard.
The banner carries the unique \"· N items\" count badge (the filter line
reads \"· all items\", never a digit), so this counts true banners only."
  (let ((n 0) (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "·[[:space:]]*[0-9]+[[:space:]]+items\\b" nil t)
        (setq n (1+ n))))
    n))

(defun org-air-s1--header-line-duplicates-banner-p ()
  "Non-nil when `header-line-format' re-states the banner (the S1 bug).
True if any atom is a banner-builder symbol (name contains \"banner\")
or any literal string carries the \"N items\" count signature."
  (let ((atoms (org-air-s1--construct-atoms header-line-format)))
    (cl-some
     (lambda (a)
       (or (and (symbolp a)
                (string-match-p "banner" (symbol-name a)))
           (and (stringp a)
                (string-match-p "[0-9]+[[:space:]]+items\\b" a))))
     atoms)))

(defun org-air-s1--header-line-is-frame-border-p ()
  "Non-nil when `header-line-format' is the T7 frame border (chrome).
True if any atom is a frame/border builder symbol or any literal string
carries a box-drawing horizontal glyph (GUI ─ or its ASCII fallback)."
  (let ((atoms (org-air-s1--construct-atoms header-line-format)))
    (cl-some
     (lambda (a)
       (or (and (symbolp a)
                (string-match-p "frame\\|border" (symbol-name a)))
           (and (stringp a)
                (string-match-p "[─━╌┄+]" a))))
     atoms)))

(defun org-air-s1--line1 ()
  "Return the buffer's first line, properties stripped."
  (save-excursion
    (goto-char (point-min))
    (buffer-substring-no-properties
     (line-beginning-position) (line-end-position))))

(defun org-air-s1--assert-single-header ()
  "Assert the S1 contract (design pxvlzyov) in the current dashboard."
  ;; Title + status appear EXACTLY ONCE, and on buffer LINE 1.
  (should (= (org-air-s1--banner-count) 1))
  (let ((line1 (org-air-s1--line1)))
    (should (string-match-p "org-air" line1))
    (should (string-match-p "·[[:space:]]*[0-9]+[[:space:]]+items\\b" line1)))
  ;; The header-line never duplicates that banner/status...
  (should-not (org-air-s1--header-line-duplicates-banner-p))
  ;; ...and IF it is set, it is the frame border (T7 chrome), not a banner.
  (when header-line-format
    (should (org-air-s1--header-line-is-frame-border-p))))

(ert-deftest org-air-s1-no-duplicate-banner-in-header-line ()
  "The title+status banner appears exactly once, on buffer line 1; the
header-line, if set, is the T7 frame border (design pxvlzyov /
ytvztszk option A), never a banner/status duplicate.  Asserted on both
the ASCII and GUI glyph paths.  (Was s1-no-header-line-in-dashboard:
header-line nil was over-strict once T7 put the frame top border there.)"
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 120
    (org-air-s1--assert-single-header))
  ;; The GUI glyph path must not re-introduce a duplicate banner either.
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 120
      (org-air-s1--assert-single-header))))

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

;;;; T3b — responsive single-line calendar legend (design tynxttsz).

(defun org-air-t3b--legend-tier-for-width (width)
  "Expected legend TIER for WIDTH: `wide' at >=120, else `narrow'."
  (if (>= width 120) 'wide 'narrow))

(ert-deftest org-air-t3b-calendar-legend-per-tier ()
  "The calendar legend is single-line and tier-dependent (tynxttsz,
superseding the 2-line wrap): narrow (95-119 / stacked) reads
\"◆due ●sched ■today\" — no `created' word, though · still marks the
grid; wide (>=120) reads \"◆due ●sched ·created ■today\".  The opposite
tier's full legend never appears.  Asserted on GUI and TTY glyph sets."
  (skip-unless (locate-library "org-air"))
  (dolist (which '(gui tty))
    (dolist (width '(100 119 120 160))
      (let* ((tier (org-air-t3b--legend-tier-for-width width))
             (this (org-air-viewport-test-calendar-legend-expected tier which))
             (other (org-air-viewport-test-calendar-legend-expected
                     (if (eq tier 'wide) 'narrow 'wide) which))
             (run (lambda ()
                    (org-air-viewport-test-with-dashboard width
                      (let ((text (buffer-string)))
                        (ert-info ((format "width %d (%s, %s): legend %S"
                                           width tier which this))
                          (should (string-match-p (regexp-quote this) text))
                          ;; The other tier's COMPLETE legend must be absent
                          ;; (narrow vs wide are mutually exclusive strings).
                          (should-not
                           (string-match-p (regexp-quote other) text))))))))
        ;; GUI uses ◆ ● · ■; --batch is a real TTY for the ! o . # path.
        (if (eq which 'gui)
            (org-air-viewport-test-as-gui (funcall run))
          (funcall run))))))

;;;; Round-6 restraint (design ssyrlulw): de-boxed faces, inline tags, no frame.

(defconst org-air-v1--tag-faces
  '(org-air-face-tag org-air-face-tag-active
    org-air-face-tag-accent-1 org-air-face-tag-accent-2 org-air-face-tag-accent-3
    org-air-face-tag-accent-4 org-air-face-tag-accent-5 org-air-face-tag-accent-6)
  "Tag faces that V1a strips of box vocabulary.")

(defconst org-air-v1--accent-faces
  '(org-air-face-tag-accent-1 org-air-face-tag-accent-2 org-air-face-tag-accent-3
    org-air-face-tag-accent-4 org-air-face-tag-accent-5 org-air-face-tag-accent-6)
  "The six tag accent hues — foreground-only after V1a.")

(defconst org-air-v2--priority-faces
  '(org-air-face-priority-a org-air-face-priority-b org-air-face-priority-c)
  "Priority cookie faces that V2 strips of the pill.")

(ert-deftest org-air-v1a-v2-tag-priority-faces-deboxed ()
  "V1a/V2 restraint: tag and priority faces carry NO box vocabulary.
Every tag/priority face has :box, :background and :height `unspecified'
(the effective face, resolving inheritance) — colour is foreground only.
:box is GUI-invisible to the byte gate, so the face spec is the only
place de-boxing is testable.  The six accent hues keep a real
:foreground clause in their defface spec (the hue is the whole point)."
  (skip-unless (locate-library "org-air"))
  (dolist (f (append org-air-v1--tag-faces org-air-v2--priority-faces))
    (ert-info ((format "face %s" f))
      (should (facep f))
      (dolist (attr '(:box :background :height))
        (should (eq (face-attribute f attr nil t) 'unspecified)))))
  (dolist (f org-air-v1--accent-faces)
    (ert-info ((format "accent hue kept: %s" f))
      ;; The hue lives in the defface spec's colour clauses (batch has no
      ;; 256-colour display, so the effective :foreground falls back; the
      ;; spec is the display-independent source of truth).
      (should (memq :foreground (flatten-tree (get f 'face-defface-spec)))))))

(defun org-air-v1b--item-leftpane (title)
  "Return the rendered item row's LEFT pane (pre-divider) for TITLE.
Nil if TITLE is not on screen.  Right-trimmed; the rail/calendar pane
after the divider is dropped so only the item line is inspected."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward title nil t)
      (let* ((bol (line-beginning-position)) (eol (line-end-position))
             (line (buffer-substring-no-properties bol eol)))
        (string-trim-right (car (split-string line "[│|]")))))))

(ert-deftest org-air-v1b-inline-tag-placement ()
  "V1b: tags sit INLINE right after the date (single space), and the
single flex gap is between the tag cluster and the flush-right origin —
the round-5 title->tags void is gone.  Reading order:
<state> [<prio>] <title>  <date>  <#tags>  <flex>  ⌂ <origin>."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 120
      (let ((lp (org-air-v1b--item-leftpane "Chase missing invoice")))
        (should lp)
        ;; Tags immediately follow the date, single space — no void.
        (should (string-match-p "OVERDUE 7d #projects #admin" lp))
        ;; The origin is flush-right, reached across the flex gap (>=2
        ;; spaces) that now sits AFTER the tags, not before them.
        (should (string-match-p "#admin \\{2,\\}⌂ projects\\.org\\'" lp))
        ;; No large whitespace run between the date and the tags.
        (should-not (string-match-p "OVERDUE 7d \\{2,\\}#" lp))))))

(ert-deftest org-air-v1b-origin-protected-on-overflow ()
  "D2 + V1b overflow: when the row cannot fit, the inline tags drop
first toward a faded overflow marker and the title truncates, but the
origin is NEVER dropped — it stays intact and flush-right.  (Impl emits
the `more' glyph for the tag overflow, width-driven, rather than the
spec's static +N-at-inline-max; flagged for a doc reconcile.)"
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 120
      ;; This row has 3 tags and a long title; at 120 it overflows.
      (let ((lp (org-air-v1b--item-leftpane "Fix production outage runbook"))
            (more (org-air-viewport-test--glyph 'more 'gui)))
        (should lp)
        ;; Origin intact and flush-right despite the overflow (D2).
        (should (string-suffix-p "⌂ projects.org" lp))
        ;; A faded overflow marker shows content was dropped before it.
        (should (string-match-p (regexp-quote more) lp))))))

(ert-deftest org-air-v3-frame-chrome-removed ()
  "V3: the round-4 buffer-box outer frame is GONE — a half-drawn frame is
a regression, so this pins its absence.  A rendered dashboard sets no
frame margins, no line-prefix/wrap-prefix side border, and no
header-line frame border; the in-buffer hairline rules + single rail
divider carry all the structure (and stay byte-tested elsewhere)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 120
    ;; No outer-frame margins.
    (should (= (or left-margin-width 0) 0))
    (should (= (or right-margin-width 0) 0))
    ;; No side border carried by the line/wrap prefixes.
    (should (null line-prefix))
    (should (null wrap-prefix))
    ;; No header-line frame border (V3 removed it; S1 allows nil).
    (should (null header-line-format)))
  ;; The GUI glyph path must not re-introduce frame chrome either.
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 120
      (should (= (or left-margin-width 0) 0))
      (should (= (or right-margin-width 0) 0))
      (should (null line-prefix))
      (should (null wrap-prefix))
      (should (null header-line-format)))))

(provide 'org-air-screenshot-regression-test)
;;; org-air-screenshot-regression-test.el ends here
