;;; org-air-round9-test.el --- round-9 grind suite for org-air -*- lexical-binding: t; -*-

;;; Commentary:
;; Spec-true grinds for v0.4 round-9 (air/v0.4/org-air-round9-design.org,
;; design tip ymroopnp).  These are written against the FROZEN design
;; names/contracts, never the current impl, so they double as the impl
;; punch list (listed in tests/org-air-known-failures.el while red).
;;
;;   C1  narrow-width re-render: the resize-refresh seam must re-fit the
;;       view to the ACTUAL displaying-window width on split/resize — no
;;       rendered line overflows, the V6 date/tag/origin columns are
;;       recomputed, and the calendar never overlaps the origin column.
;;       (The 8b split regression the fixed-width byte gate missed.)
;;   C2  pill-vs-text geometry: a pill is a cosmetic `display' overlay; it
;;       NEVER shifts a column.  Turning tag/date pills on vs off leaves
;;       the TEXT layer byte-identical (the gate always asserts the text).
;;   C3  text-scale re-fit is consistent (extends T6): a font/scale change
;;       routes through the same resize seam and re-fits EVERY element to
;;       the new metrics — never a mix of old/new sizes.
;;   F1  Denote origin: a Denote-named file (20260614T170000--my-title__a_b
;;       .org) shows the human title slug ("my-title") in the origin, with
;;       the id/tag machinery and .org stripped; non-Denote names fall
;;       back to the plain filename.
;;   Q1  scope reset is discoverable: the rail advertises the reset and S
;;       (`org-air-scope-clear') clears an active scope.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)

;;;; ---------------------------------------------------------------------
;;;; C1 — narrow-width re-render via the resize-refresh seam.
;;;; ---------------------------------------------------------------------

(defun org-air-r9--date-token-columns ()
  "Return the start columns of every item-row date token in the buffer.
Mirrors `org-air-v6-dates-align-in-column': a V6 date cell begins at the
same screen column on every dated row."
  (let ((cols '()))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((bol (line-beginning-position))
               (eol (line-end-position))
               (item (let ((p bol) found)
                       (while (and (< p eol) (not found))
                         (when (get-text-property p 'org-air-item)
                           (setq found t))
                         (setq p (1+ p)))
                       found))
               (line (buffer-substring-no-properties bol eol)))
          ;; The trailing \b keeps a date phrase inside a TITLE (e.g.
          ;; "Untracked idea with no dates") from masquerading as the
          ;; V6 date cell.
          (when (and item
                     (string-match
                      " \\(OVERDUE [0-9]+d\\|Today\\|Tomorrow\\|no date\\)\\b"
                      line))
            (push (match-beginning 1) cols)))
        (forward-line 1)))
    cols))

(defun org-air-r9--origin-glyph ()
  "Return the GUI origin breadcrumb glyph (⌂)."
  (org-air-viewport-test--glyph 'origin 'gui))

(ert-deftest org-air-r9-c1-narrow-resize-refits ()
  "C1: after the displaying window narrows, the resize-refresh seam
re-renders to the NEW width.  Render two-pane at 160, then narrow to 100
(both the live `org-air-layout-current-width' path AND the width seam
report 100) and fire `org-air-view--resize-refresh': (a) no line exceeds
100, (b) the V6 columns are recomputed and the date tokens still share
one column, (c) the origin glyph never crosses the divider into the
calendar pane (the 8b calendar/origin overlap)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 160
      (should (eql org-air-view--rendered-width 160))
      (let* ((narrow 100)
             ;; D1 geometry: divider column = WIDTH - rail-tier - 2; the
             ;; 96-119 tier rail is `org-air-rail-width-narrow' (28).
             (divider-col (- narrow org-air-rail-width-narrow 2)))
        (cl-letf (((symbol-function 'org-air-layout-current-width)
                   (lambda (&optional _buf) narrow)))
          (let ((org-air-view-width narrow))
            (org-air-view--resize-refresh)))
        ;; The seam actually re-rendered to the new width.
        (should (eql org-air-view--rendered-width narrow))
        ;; (a) nothing overflows the narrowed width.
        (dolist (line (org-air-viewport-test-lines))
          (should (<= (string-width (substring-no-properties line)) narrow)))
        ;; (b) the V6 metadata columns were recomputed (non-nil) and the
        ;; date tokens still all start at exactly one column.
        (should org-air-view--meta-date-w)
        (should org-air-view--meta-origin-w)
        (let ((cols (org-air-r9--date-token-columns)))
          (should (> (length cols) 2))
          (should (= 1 (length (delete-dups (copy-sequence cols))))))
        ;; (c) no calendar/origin overlap: every origin glyph sits LEFT of
        ;; the recomputed divider column (origins live in the item pane,
        ;; the calendar in the rail to the right).
        (let ((glyph (org-air-r9--origin-glyph)))
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position)))
                    (start 0))
                (while (string-match (regexp-quote glyph) line start)
                  (should (< (string-width (substring line 0 (match-beginning 0)))
                             divider-col))
                  (setq start (match-end 0))))
              (forward-line 1))))))))

;;;; ---------------------------------------------------------------------
;;;; C2 — a pill is a cosmetic overlay; it never shifts a column.
;;;; ---------------------------------------------------------------------

(defun org-air-r9--render-text (tag-style date-style width)
  "Render the fixtures at WIDTH with TAG-STYLE/DATE-STYLE; return the
buffer's TEXT layer (no properties) as a list of right-trimmed lines."
  (let (out)
    (org-air-viewport-test-as-gui
      (org-air-viewport-test-with-dashboard width
        (let ((org-air-tag-style tag-style)
              (org-air-date-style date-style))
          (org-air-view--render-current))
        (setq out (mapcar (lambda (l)
                            (string-trim-right (substring-no-properties l)))
                          (org-air-viewport-test-lines)))))
    out))

(ert-deftest org-air-r9-c2-pill-text-layer-byte-identical ()
  "C2: turning the svg pills ON must not move a single column.  The TEXT
layer (what the byte gate and the TTY both see) is byte-identical with
pills on vs off — the pill is a `display' overlay sized to the cell, not
extra geometry."
  (skip-unless (locate-library "org-air"))
  (let ((pill (org-air-r9--render-text 'pill 'pill 120))
        (text (org-air-r9--render-text 'text 'text 120)))
    (should (equal pill text))))

(ert-deftest org-air-r9-c2-text-fallback-is-coloured-text ()
  "C2 fallback contract: with pills OFF (or unavailable) the tag/date
metadata is plain coloured TEXT — #tag and the date token appear in the
buffer string, never an image placeholder."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 120
    (let ((org-air-tag-style 'text)
          (org-air-date-style 'text))
      (org-air-view--render-current))
    (let ((text (buffer-string)))
      (should (string-match-p "#[a-z]" text))
      (should (string-match-p "OVERDUE [0-9]+d\\|Tomorrow\\|Today" text)))))

;;;; ---------------------------------------------------------------------
;;;; C3 — text-scale / font re-fit is consistent (extends T6).
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r9-c3-text-scale-wired-to-resize-seam ()
  "C3/T6: a text-scale change re-fits through the SAME resize path.
`org-air-view--text-scale-refresh' is on `text-scale-mode-hook' and the
buffer's `org-air-layout-refresh-function' is the resize seam, so a font
change re-renders the whole view (never a partial re-fit)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 120
    (should (memq #'org-air-view--text-scale-refresh
                  (buffer-local-value 'text-scale-mode-hook (current-buffer))))
    (should (eq org-air-layout-refresh-function
                #'org-air-view--resize-refresh))))

(ert-deftest org-air-r9-c3-text-scale-refit-consistent ()
  "C3: a text-scale change that alters how many columns fit re-fits EVERY
element to the new metrics — no mixed old/new sizes left behind.  Drive
two successive scale changes (narrower, then wider) through the resize
seam; after each the buffer is wholly consistent at the new width (every
line exactly that wide, the V6 date column still aligned)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 120
      (dolist (w '(96 140))
        (ert-info ((format "re-fit width %d" w))
          (cl-letf (((symbol-function 'org-air-layout-current-width)
                     (lambda (&optional _buf) w)))
            (let ((org-air-view-width w))
              (org-air-view--text-scale-refresh)
              ;; text-scale-refresh debounces through an idle timer that
              ;; never fires in batch; the resize seam IS that handler.
              (org-air-view--resize-refresh)))
          (should (eql org-air-view--rendered-width w))
          (dolist (line (org-air-viewport-test-lines))
            (let ((wide (string-width (substring-no-properties line))))
              (should (or (string-empty-p (string-trim-right line))
                          (= wide w)))))
          (let ((cols (org-air-r9--date-token-columns)))
            (should (> (length cols) 1))
            (should (= 1 (length (delete-dups (copy-sequence cols)))))))))))

;;;; ---------------------------------------------------------------------
;;;; F1 — Denote-style origin names (strip id/tag machinery, show title).
;;;; ---------------------------------------------------------------------

(defmacro org-air-r9--with-denote-board (specs &rest body)
  "Write SPECS (alist of FILENAME -> HEADING-CONTENT) into a temp board,
bind `org-air-files'/`org-air-inbox-file' to it, and run BODY with the
clock frozen.  Each value is the full org body for that file."
  (declare (indent 1) (debug t))
  `(let ((org-air-r9--dir (make-temp-file "org-air-denote-" t)))
     (unwind-protect
         (progn
           (pcase-dolist (`(,name . ,content) ,specs)
             (with-temp-file (expand-file-name name org-air-r9--dir)
               (insert content)))
           (let ((org-air-files
                  (directory-files org-air-r9--dir t "\\.org\\'"))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-r9--dir)))
             (org-air-viewport-test--with-frozen-now
               ,@body)))
       (delete-directory org-air-r9--dir t))))

(ert-deftest org-air-r9-f1-origin-style-defcustom ()
  "F1: `org-air-origin-style' selects how the origin renders — `auto'
(Denote-aware, the default), `filename', or `title-from-org'."
  (skip-unless (locate-library "org-air"))
  (should (boundp 'org-air-origin-style))
  (should (eq org-air-origin-style 'auto)))

(ert-deftest org-air-r9-f1-denote-origin-shows-title-slug ()
  "F1: a Denote-named file's origin shows the human title slug, stripped
of the timestamp identifier, the __tag signature and the .org extension."
  (skip-unless (locate-library "org-air"))
  (org-air-r9--with-denote-board
      '(("20260614T170000--my-title__a_b.org"
         . "* TODO Read the Denote note\nSCHEDULED: <2026-06-16 Tue>\n"))
    (let* ((items (org-air-query-items))
           (item (org-air-test-find-item "Read the Denote note" items)))
      (should item)
      (let ((origin (org-air-view--origin item)))
        (should (equal origin "my-title"))
        (should-not (string-match-p "20260614T170000" origin))
        (should-not (string-match-p "__\\|_a_b\\|:a:b" origin))
        (should-not (string-match-p "\\.org" origin))))))

(ert-deftest org-air-r9-f1-non-denote-falls-back-to-filename ()
  "F1: a plain (non-Denote) filename is shown verbatim (today's
behaviour) — Denote parsing must not mangle ordinary files."
  (skip-unless (locate-library "org-air"))
  (org-air-r9--with-denote-board
      '(("projects.org"
         . "* TODO Plain file item\nSCHEDULED: <2026-06-16 Tue>\n"))
    (let* ((items (org-air-query-items))
           (item (org-air-test-find-item "Plain file item" items)))
      (should item)
      (should (equal (org-air-view--origin item) "projects.org")))))

(ert-deftest org-air-r9-f1-denote-origin-rendered-and-truncates ()
  "F1: in a rendered board a long Denote name shows the de-slugged title
in the origin column (not the id/tags), and a too-long title truncates
with the ellipsis glyph — never overflowing the line width."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-r9--with-denote-board
        '(("20260614T170000--a-very-long-denote-note-title-that-keeps-going__x_y.org"
           . "* TODO Long denote note\nSCHEDULED: <2026-06-16 Tue>\n"))
      (let ((org-air-view-width 80))
        (org-air)
        (let ((buf (get-buffer "*org-air*")))
          (should buf)
          (unwind-protect
              (with-current-buffer buf
                (let ((text (buffer-string)))
                  ;; the title slug surfaces; the machinery never does.
                  (should (string-match-p "a-very-long-denote-note-title" text))
                  (should-not (string-match-p "20260614T170000" text))
                  (should-not (string-match-p "__x_y\\|\\.org" text)))
                ;; nothing overflows the composed width.
                (dolist (line (org-air-viewport-test-lines))
                  (should (<= (string-width (substring-no-properties line)) 80))))
            (kill-buffer buf)))))))

;;;; ---------------------------------------------------------------------
;;;; Q1 — scope reset discoverability.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r9-q1-S-clears-active-scope ()
  "Q1: S (`org-air-scope-clear') clears an active scope and re-renders to
the full board."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 120
    ;; S is bound to the clearer in the dashboard keymap.
    (should (eq (lookup-key org-air-view-mode-map (kbd "S"))
                #'org-air-scope-clear))
    (setq org-air-view--scope '(:tag "work"))
    (org-air-view--render-current)
    (org-air-scope-clear)
    (should (null org-air-view--scope))))

(ert-deftest org-air-r9-q1-rail-advertises-scope-reset ()
  "Q1: the scope reset is discoverable.  When a scope is active the rail
surfaces a reset cue tying the S key to clearing scope (design: the
Filters block shows `Scope: <name>  (S clears)' and the rail hint adds
`S reset')."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 160
      (setq org-air-view--scope '(:tag "work"))
      (org-air-view--render-current)
      (let ((text (substring-no-properties (buffer-string))))
        ;; the active scope is shown,
        (should (string-match-p "Scope: #work" text))
        ;; and a discoverable S-clears-scope cue is present.
        (should (string-match-p "S[^\n]*\\(clear\\|reset\\)" text))))))

;;;; ---------------------------------------------------------------------
;;;; D5 — sidebar / context-rail refinement
;;;; (air/v0.4/org-air-round9-design-d5.org, design tip utwrpzmx).
;;;; The rail becomes a polished nano-* sidebar: one labelled-rule family
;;;; (calendar + Actions adopt Summary/Filters' rule, each opened by a ╶
;;;; hrule-cap echoing the D1-D3 pill), a single content spine, a spaced
;;;; legend, a short ledger-sum rule under `total', and a named Actions
;;;; block with rail-key keycaps.  Mostly [byte] (rail text -> regen);
;;;; the [face] rail-key + [glyph] hrule-cap land via org-air-faces.el /
;;;; org-air-layout.el.  All grinds until the D5 impl+face parts integrate.
;;;; ---------------------------------------------------------------------

(defun org-air-r9--rail-lines ()
  "Return the rail-column text (right of the two-pane divider) per line.
Each element is the right-trimmed text after the first divider glyph;
lines with no divider are skipped."
  (let (out)
    (dolist (line (org-air-viewport-test-lines))
      (let ((s (substring-no-properties line)))
        (when (string-match "[│|]" s)
          (push (string-trim-right (substring s (match-end 0))) out))))
    (nreverse out)))

(defun org-air-r9--hrule-cap ()
  "Return the GUI hrule-cap glyph (╶) from the glyph table, or the spec
literal when the entry is absent (so the grind reports the missing cap)."
  (let ((entry (and (boundp 'org-air-glyphs)
                    (cdr (assq 'hrule-cap org-air-glyphs)))))
    (or (cond ((and (consp entry) (stringp (cdr entry))) (car entry))
              ((consp entry) (nth 0 entry))
              ((stringp entry) entry))
        "╶")))

(ert-deftest org-air-r9-d5-rail-key-face-defined ()
  "D5f: the quiet keycap face `org-air-face-rail-key' exists (inherits
the salient tone, no box) for the Actions verbs' leading key tokens."
  (skip-unless (locate-library "org-air"))
  (should (facep 'org-air-face-rail-key)))

(ert-deftest org-air-r9-d5-hrule-cap-glyph-defined ()
  "D5a: the `hrule-cap' glyph (GUI ╶ / TTY -) is registered so every rail
rule can open with the rounded stub that echoes the pill's left edge."
  (skip-unless (locate-library "org-air"))
  (should (assq 'hrule-cap org-air-glyphs))
  ;; GUI glyph from the table (batch is a TTY, so go through as-gui for
  ;; the live accessor) plus the TTY fallback.
  (should (equal (org-air-r9--hrule-cap) "╶"))
  (org-air-viewport-test-as-gui
    (should (equal (org-air-layout-glyph 'hrule-cap) "╶")))
  (should (equal (org-air-layout-glyph 'hrule-cap) "-")))

(ert-deftest org-air-r9-d5a-rail-rule-family-has-cap ()
  "D5a: Summary and Filters open with the SAME labelled rule led by the
╶ hrule-cap (one rule family across the rail, not bare ── rules)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 160
      (let ((cap (org-air-r9--hrule-cap))
            (text (substring-no-properties (buffer-string))))
        (should (string-match-p (concat (regexp-quote cap) "─ Summary") text))
        (should (string-match-p (concat (regexp-quote cap) "─ Filters") text))))))

(ert-deftest org-air-r9-d5a-actions-block-named ()
  "D5f/D5a: the floating verb hints become a named `Actions' peer block,
opened by the labelled rule (not orphan lines)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 160
      (let ((cap (org-air-r9--hrule-cap))
            (text (substring-no-properties (buffer-string))))
        (should (string-match-p (concat (regexp-quote cap) "─ Actions") text))))))

(ert-deftest org-air-r9-d5a-calendar-is-labelled-rule ()
  "D5a: the calendar month header renders as a labelled rule
(╶─ June 2026 ──…── ‹ ›): the cap + month label, the ‹ › nav anchored
AFTER the fill, in the same family as Summary/Filters."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 160
      (let* ((cap (org-air-r9--hrule-cap))
             (rail (org-air-r9--rail-lines))
             ;; the month rule line: cap, dash, "June 2026"/"Jun 2026",
             ;; fill, then the nav glyphs at the right.
             ;; the line carries the content-spine leading inset.
             (rx (concat "^ *" (regexp-quote cap)
                         "─ Ju\\(ne\\|n\\) 2026 ─.*‹ ›$")))
        (should (cl-some (lambda (l) (string-match-p rx l)) rail))))))

(ert-deftest org-air-r9-d5c-legend-separated-and-spaced ()
  "D5c: the calendar legend is separated from the grid by one blank line,
indented to the spine, with a space between each glyph and its word
(◆ due, ● sched, · created) and a wide gap between entries."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 160
      (let* ((rail (org-air-r9--rail-lines))
             (idx (cl-position-if
                   (lambda (l) (string-match-p "◆ due" l)) rail)))
        (should idx)
        ;; spaced glyph+word for all three marks.
        (let ((line (nth idx rail)))
          (should (string-match-p "◆ due" line))
          (should (string-match-p "● sched" line))
          (should (string-match-p "· created" line))
          ;; wider (>=3 space) gap between entries, not a single space.
          (should (string-match-p "due \\{3,\\}●" line)))
        ;; separated from the grid by a blank rail line above.
        (should (> idx 0))
        (should (string-empty-p (string-trim (nth (1- idx) rail))))))))

(defun org-air-r9--rail-leading-spaces (rail-line)
  "Return the count of leading spaces of RAIL-LINE (its content inset)."
  (if (string-match "\\`\\( *\\)" rail-line)
      (length (match-string 1 rail-line))
    0))

(ert-deftest org-air-r9-d5b-content-spine ()
  "D5b: every block's content snaps to ONE left-edge spine
(`org-air-rail-content-inset', default 3 at the wide/mid tiers).  The
calendar weekday row, a Summary row, the Filters text and an Actions
verb row all begin at the SAME rail column (the old rail had three
different left edges)."
  (skip-unless (locate-library "org-air"))
  (should (boundp 'org-air-rail-content-inset))
  (should (= org-air-rail-content-inset 3))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 160
      (let* ((rail (org-air-r9--rail-lines))
             (pick (lambda (rx)
                     (cl-find-if (lambda (l) (string-match-p rx l)) rail)))
             (weekday (funcall pick "Su .*Mo .*Tu"))
             (filters (funcall pick "No filters"))
             (actions (funcall pick "c capture")))
        ;; Left-aligned content across three blocks (calendar grid /
        ;; filters / actions) shares one left edge.  (Summary numbers are
        ;; right-aligned in their field, a different measurement.)
        (should (and weekday filters actions))
        (let ((insets (mapcar #'org-air-r9--rail-leading-spaces
                              (list weekday filters actions))))
          (should (= 1 (length (delete-dups (copy-sequence insets))))))))))

(ert-deftest org-air-r9-d5d-ledger-sum-rule ()
  "D5d: `total' sits under a SHORT ledger rule (a 4-char ──── over the
number field), not the old full-width empty hairline that read as debris."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 160
      (let* ((rail (org-air-r9--rail-lines))
             (tot (cl-position-if
                   (lambda (l) (string-match-p "[0-9]+ +total\\'" l)) rail)))
        (should tot)
        (should (> tot 0))
        (let ((above (string-trim (nth (1- tot) rail))))
          ;; a short ledger rule of box-drawing dashes,
          (should (string-match-p "\\`─+\\'" above))
          ;; that is SHORT (the ledger sum, ~4), not the full rail width.
          (should (<= (string-width above) 8)))))))

(ert-deftest org-air-r9-d5f-actions-aligned-no-dot-separators ()
  "D5f: the Actions verb rows are column-aligned with a wide gap and DROP
the `·' separators (the column gap does the separating, calmer prose)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 160
      (let* ((case-fold-search nil)
             (rail (org-air-r9--rail-lines))
             ;; "capture"/"refresh" appear ONLY in the Actions verb rows
             ;; (avoid "filter", which case-folds into the Filters rule).
             (verbs (seq-filter
                     (lambda (l) (string-match-p "capture\\|refresh" l))
                     rail)))
        (should verbs)
        (dolist (l verbs)
          (should-not (string-match-p " · " l))
          ;; column-aligned: a wide gap between verbs.
          (should (string-match-p "[a-z] \\{3,\\}[A-Za-z/?]" l)))))))

(ert-deftest org-air-r9-d5f-rail-key-keycap-applied ()
  "D5f: the Actions verbs' leading key tokens render in the quiet keycap
face `org-air-face-rail-key' (keys read as keys, prose recedes)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 160
      (should (org-air-viewport-test-face-applied-p 'org-air-face-rail-key)))))

(provide 'org-air-round9-test)
;;; org-air-round9-test.el ends here
