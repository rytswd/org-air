;;; org-air-view.el --- Dashboard renderer for org-air -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: org-air contributors
;; Keywords: outlines, calendar
;; Version: 0.1.0
;; URL: https://github.com/rytswd/org-air
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Minimal, nano-inspired interactive dashboard for org-air.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'org)
(require 'seq)
(require 'subr-x)
(require 'org-air-faces)
(require 'org-air-query)
(require 'org-air-classify)
(require 'org-air-calendar)
(require 'org-air-inbox)
(require 'org-air-layout)

;; V3 svg pills are GUI-only and soft-loaded at render time (`require 'svg').
(declare-function svg-create "svg")
(declare-function svg-rectangle "svg")
(declare-function svg-text "svg")
(declare-function svg-image "svg")

(defvar org-air-files)
(defvar org-air-inbox-file)

(defcustom org-air-margin 2
  "Left margin for org-air content lines."
  :type 'integer
  :group 'org-air)

(defcustom org-air-item-indent 4
  "Column where item rows start."
  :type 'integer
  :group 'org-air)

(defcustom org-air-section-max 12
  "Maximum visible items per section before an overflow note."
  :type 'integer
  :group 'org-air)

(defcustom org-air-date-style 'pill
  "How the item-row date renders (R10).
`pill' draws an svg-tag-style rounded pill on a graphical frame when SVG
is available, degrading to coloured text; `text' is always coloured text.
The byte gate only ever sees the coloured text (the pill is a GUI display
property), so fixtures are unaffected by this choice."
  :type '(choice (const pill) (const text))
  :group 'org-air)

(defcustom org-air-tags-inline-max 2
  "Maximum number of tag chips to render on an item line."
  :type 'integer
  :group 'org-air)

(defcustom org-air-tag-style 'pill
  "How tags (and the R10/V6 date) render (V3).
`pill' draws a built-in rounded svg pill on a graphical frame when SVG is
available (D-P1: own the geometry, no `svg-tag-mode'), degrading to the
quiet coloured text; `text' is always the text.  The pill is sized to the
underlying text's column width, with the D-P1.PAD reserved pad columns,
so it never shifts the V6 columns, and the byte gate only ever sees the
padded text."
  :type '(choice (const pill) (const text))
  :group 'org-air)

(defcustom org-air-pill-pad-cols 1
  "Reserved internal padding COLUMNS on each side of a pill label (D-P1.PAD).
The pill label string carries this many space columns left AND right in
the TEXT layer (svg-tag-mode's technique): the chip reads `(pad)#inbox(pad)'.
Because `org-air-view--compute-meta-widths' sizes the metadata columns from
`string-width', the reserved pad is counted automatically, so the pill box
spans `reserved-Ncols * char-width' and V6 alignment still holds (geometry
stays 100% text-layer driven).  The label is drawn centred in the inner
columns so the pad becomes genuine internal margin and the label never
reaches the rounded edge.  In TTY (pills off) the pad spaces are harmless
and keep alignment byte-identical with the pill on."
  :type 'integer
  :group 'org-air)

(defcustom org-air-pill-font-scale 0.66
  "Pill label font-size as a fraction of the line height `ch' (D-P1.FIT).
The desired label font-size is `(max 7 (round (* ch this)))'.  The label
is then WIDTH-fitted to the box's inner width so a long label can never
exceed its drawable area — this is the structural clip fix."
  :type 'number
  :group 'org-air)

(defcustom org-air-pill-radius nil
  "Corner radius in device pixels for the rounded svg pill (D-P1.LOOK).
When nil the radius is `line-height / 6' (a soft rounded corner, NOT a
stadium), tracking the current font/text-scale metrics."
  :type '(choice (const :tag "Auto (line-height/6)" nil) number)
  :group 'org-air)

(defcustom org-air-pill-fill-alpha 0
  "Per-chip fill opacity for the rounded svg pill (D-P1.LOOK).
Default 0 = a pure outline pill (no fill): the board stops being a
rainbow of filled chips.  Colour lives only in the LABEL; the capsule is
a monochrome border."
  :type 'number
  :group 'org-air)

(defcustom org-air-pill-border nil
  "Colour of the single muted pill border (D-P1.LOOK).
A colour string, or nil to derive a quiet neutral from the
`org-air-face-faded' foreground.  The same restrained neutral is used for
every chip — the per-tag hue lives in the LABEL only, never the border."
  :type '(choice (const :tag "Auto (derive from faded)" nil) string)
  :group 'org-air)

(defcustom org-air-pill-border-opacity 0.5
  "Stroke opacity for the svg pill border (D-P2, calmer capsules).
Default 0.5 draws the capsule outline as a hairline so a column of stacked
pills no longer reads as a busy grid of lines.  Display-only (an svg
attribute) — no byte change."
  :type 'number
  :group 'org-air)

(defcustom org-air-date-pill-align 'center
  "How the date label sits inside its uniform-width pill capsule (D-P1).
`center' (default) centres the label in the `meta-date-w' box; `right'
hugs the label to the right of the box (a right-aligned date column).  The
underlying TTY/byte text stays left-justified either way."
  :type '(choice (const center) (const right))
  :group 'org-air)

(defcustom org-air-line-spacing 0.15
  "Buffer-local `line-spacing' for the org-air board (D-P2 #4).
A touch of inter-row breathing space calms the stacked capsules.  nil
leaves `line-spacing' at the frame default; 0 packs rows tight (the
round-8 S8 behaviour for unbroken box-drawing rules)."
  :type '(choice (const :tag "Frame default" nil) number)
  :group 'org-air)

(defvar org-air-view--pill-char-w nil
  "Device-pixel width of one text column for the current render (C2/C3).
Bound during `org-air-view--render' from the displaying window's actual
font metrics (text-scale aware) so svg pills are sized to the exact cell.")

(defvar org-air-view--pill-char-h nil
  "Device-pixel height of one text line for the current render (C2/C3).")

(defcustom org-air-date-column 12
  "Fixed width of the date cell in the item-row metadata table (V6).
The date is left-justified in this many columns so every row's date
starts at the same position and the eye can scan the column down the
list.  Fits the longest tokens (e.g. \"OVERDUE 12d\", \"· 273d quiet\")."
  :type 'integer
  :group 'org-air)

(defvar org-air-view--meta-date-w nil
  "Computed width of the date column for the current render (V6), or nil.")

(defvar org-air-view--meta-tags-w nil
  "Computed width of the tags column for the current render (V6), or nil.")

(defvar org-air-view--meta-origin-w nil
  "Computed width of the origin column for the current render (V6), or nil.")

(defcustom org-air-show-footer nil
  "Whether to show the bottom footer key-legend band (R4: default nil).
The verbs live in the rail hint block now; the footer band is opt-in."
  :type 'boolean
  :group 'org-air)

(defcustom org-air-show-group nil
  "When non-nil, show item group instead of leaf filename as origin."
  :type 'boolean
  :group 'org-air)

(defcustom org-air-origin-style 'auto
  "How the origin column renders a file name (F1).
`auto' is Denote-aware: a file named like a Denote note
\(\"YYYYMMDDTHHMMSS--my-title__tag1_tag2.org\") shows the human title
\(\"my-title\"), stripping the timestamp id, the __tag signature and the
extension; any other file falls back to its plain leaf name.  `filename'
always shows the plain leaf name (the historic behaviour).
`title-from-org' reads the file's \"#+title:\" when present, then falls
back to the Denote title, then the leaf name."
  :type '(choice (const :tag "Denote-aware (auto)" auto)
                 (const :tag "Plain filename" filename)
                 (const :tag "Org #+title" title-from-org))
  :group 'org-air)

(defcustom org-air-origin-deslugify nil
  "When non-nil, show a Denote title with spaces instead of hyphens (F1).
The default keeps hyphens for a compact origin column."
  :type 'boolean
  :group 'org-air)

(defcustom org-air-section-rule nil
  "When non-nil, draw a faint rule under section titles."
  :type 'boolean
  :group 'org-air)

(defcustom org-air-view-width nil
  "Width used for org-air dashboard line composition.
When nil, derive width from the live window body.  When an integer, render
batch-testable dashboard lines against exactly that display width using
`string-width' padding rather than display-property alignment."
  :type '(choice (const :tag "Live window" nil) integer)
  :group 'org-air)

(defcustom org-air-view-height nil
  "Total body height used for org-air full-height composition (S6).
When nil, derive the height from the live window body.  When an integer,
fill the dashboard to exactly that many lines (the vertical analogue of
`org-air-view-width', for deterministic batch rendering)."
  :type '(choice (const :tag "Live window" nil) integer)
  :group 'org-air)

(defcustom org-air-layout-two-pane-min 100
  "Legacy fixed two-pane breakpoint (superseded by the derived rule).
Kept for back-compatibility/customisation; the live decision is made by
`org-air-view--two-pane-p', which derives engagement from
`org-air-item-pane-min' and the active rail tier (D1)."
  :type 'integer
  :group 'org-air)

(defcustom org-air-item-pane-min 64
  "Minimum item-pane content width below which two-pane is not worth it.
The derived two-pane breakpoint is `org-air-item-pane-min' plus the
divider plus the narrow rail tier, about 95 columns, so threshold-zone
windows near 100 columns stay horizontal with the calendar on-screen (D1)."
  :type 'integer
  :group 'org-air)

(defcustom org-air-rail-width-narrow 28
  "Context rail content width in the threshold zone (95–119 cols).
The narrow tier still fits the calendar grid, the longest summary row,
and the \"No filters · all items\" line, while leaving the item pane a
usable width (D1)."
  :type 'integer
  :group 'org-air)

(defcustom org-air-rail-width 32
  "Context rail content width in the mid two-pane tier (120–149 cols)."
  :type 'integer
  :group 'org-air)

(defcustom org-air-rail-width-wide 42
  "Context rail content width in the wide two-pane tier (>= 150 cols)."
  :type 'integer
  :group 'org-air)

(defcustom org-air-rail-content-inset 3
  "Content-spine indent for the context-rail blocks (D5b).
Every rail block's CONTENT (calendar grid + legend, summary rows + total,
filters/scope text, Actions verbs) is inset by this many columns so they
share one left edge directly under the labelled rules' label text.  The
labelled rules themselves start at column 0 and span the full rail width.
At the narrow rail tier the inset drops to 1 (see `org-air-view--rail-inset')
so the 20-col calendar grid never overflows."
  :type 'integer
  :group 'org-air)

(defcustom org-air-rail-anchor-actions nil
  "When non-nil, pin the rail Actions block to the bottom of the rail (D5f).
The default keeps Actions in normal flow one blank line under Filters; set
to t for the classic sidebar-footer look, padding blank lines between
Filters and Actions so the verbs sit at the foot of the fixed-height rail."
  :type 'boolean
  :group 'org-air)

(defcustom org-air-layout-hysteresis 3
  "Column dead-band around the two-pane breakpoint (D1).
Resizing within this many columns of the breakpoint keeps the current
orientation, so a window dragged across the boundary does not flap
between stacked and two-pane on every pixel."
  :type 'integer
  :group 'org-air)

(defcustom org-air-origin-min 12
  "Columns reserved for the origin breadcrumb in a two-pane item row (D2).
The origin is the item's identity and RET target; tags and then the
title shrink before the origin, which keeps at least this many columns."
  :type 'integer
  :group 'org-air)

(defcustom org-air-title-min 24
  "Floor width for a truncated item title before the origin shrinks (D2)."
  :type 'integer
  :group 'org-air)

(defcustom org-air-display-action nil
  "Optional `display-buffer' ACTION used to show the org-air dashboard.
When nil, the dashboard reuses a window or, failing that, takes the full
frame, so a narrow vertical split never pushes the rail/calendar
off-screen (D4)."
  :type '(choice (const :tag "Default (full-width)" nil) sexp)
  :group 'org-air)

(defcustom org-air-layout-style 'rule
  "Rule and box treatment for the org-air viewport layout."
  :type '(choice (const plain) (const rule) (const boxed))
  :group 'org-air)

(defcustom org-air-show-summary t
  "Whether to show the summary block in the org-air context rail."
  :type 'boolean
  :group 'org-air)

(defcustom org-air-show-rail-filters t
  "Whether to show filter and scope state in the org-air context rail."
  :type 'boolean
  :group 'org-air)

(defcustom org-air-visit-display 'other-window
  "How `org-air-visit-item' displays an item's source (T4).
Choices (design contract): `other-window' (default — keep the dashboard
visible alongside), `same' (reuse the dashboard's own window), `side' (a
reusable side window), `frame' (a new frame).  Whatever the choice, the
exact window configuration is captured at visit time and `org-air-return'
restores it, landing point back on the originating item row; the visited
buffer also gets a buffer-local `org-air-return-key'."
  :type '(choice (const other-window) (const same) (const side)
                 (const frame))
  :group 'org-air)

(defcustom org-air-return-key "C-c b"
  "Key bound (buffer-locally) in a visited buffer to return to the dashboard.
Set via `kbd' syntax.  Kept out of the way of normal Org editing (T4)."
  :type 'string
  :group 'org-air)

(defcustom org-air-priority-show '(?A)
  "Priority cookies shown in item rows."
  :type '(repeat character)
  :group 'org-air)

(defcustom org-air-todo-keyword-faces
  '(("TODO" . org-air-face-todo)
    ("NEXT" . org-air-face-todo-next)
    ("STARTED" . org-air-face-todo-next)
    ("WAIT" . org-air-face-todo-wait)
    ("WAITING" . org-air-face-todo-wait)
    ("HOLD" . org-air-face-todo-wait)
    ("BLOCKED" . org-air-face-todo-wait)
    ("DONE" . org-air-face-done)
    ("CANCELLED" . org-air-face-done)
    ("CANCELED" . org-air-face-done)
    ("KILL" . org-air-face-done))
  "Map TODO keyword strings to faces for coloured rendering (T1a).
Unknown keywords fall back to `org-air-face-todo'."
  :type '(alist :key-type string :value-type face)
  :group 'org-air)

(defcustom org-air-filter-match 'any
  "How multiple tag filters match items: `any' or `all'."
  :type '(choice (const any) (const all))
  :group 'org-air)

(defvar-local org-air-view--items nil)
(defvar-local org-air-view--items-key nil)
(defvar-local org-air-view--tag-filter nil)
(defvar-local org-air-view--scope nil)
(defvar-local org-air-view--day nil
  "When non-nil, an Emacs time focusing the single-day view (R6).")
(defvar-local org-air-view--expanded-sections nil)
(defvar-local org-air-view--line-width nil)
(defvar-local org-air-view--rendered-width nil
  "Column width used for the most recent render of this dashboard buffer.")
(defvar-local org-air-view--rendered-height nil
  "Body height used for the most recent render of this dashboard buffer.")
(defvar-local org-air-view--orientation nil
  "Last chosen layout orientation, `two-pane' or `stacked' (D1 hysteresis).")
(defvar org-air-view--pane-indented nil
  "Non-nil while rendering the two-pane item pane (indented downstream).
Lets headings and item rows use a consistent hanging indent regardless
of whether the wrapping pane margin is added later (D6).")
(defvar-local org-air-view--cal-month nil)

(defconst org-air-view-buffer-name "*org-air*")

(defconst org-air-view--sections
  '((inbox "Inbox" "Inbox zero — nothing to process.")
    (attention "Needs attention" "Nothing overdue. Nice.")
    (upcoming "Upcoming" org-air-view--empty-upcoming)
    (high-priority "High priority" "No #A items.")
    (stale "Stale" "Nothing has gone stale."))
  "Section descriptors in display order.")

(defvar org-air-g-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") #'org-air-refresh)
    (define-key map (kbd "g") #'org-air-goto-top)
    (define-key map (kbd "R") #'org-air-refresh-all)
    map)
  "Transient g-prefix map (B4): r refresh, g top of pane, R refresh+clear.")

(defvar org-air-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-air-visit-item)
    (define-key map (kbd "<mouse-1>") #'org-air-visit-item)
    (define-key map (kbd "n") #'org-air-next-item)
    (define-key map (kbd "p") #'org-air-prev-item)
    ;; R3: vim-ish line navigation; j/k are NOT destructive.
    (define-key map (kbd "j") #'org-air-next-line)
    (define-key map (kbd "k") #'org-air-prev-line)
    ;; T2: TAB toggles expand/collapse of the section at point; section
    ;; MOTION lives on M-n/M-p (and M-TAB) so both verbs are reachable.
    (define-key map (kbd "TAB") #'org-air-toggle-section)
    (define-key map (kbd "<backtab>") #'org-air-prev-section)
    (define-key map (kbd "M-TAB") #'org-air-next-section)
    (define-key map (kbd "M-n") #'org-air-forward-section)
    (define-key map (kbd "M-p") #'org-air-back-section)
    (define-key map (kbd "SPC") #'org-air-peek-item)
    (define-key map (kbd "o") #'org-air-visit-item-stay)
    (define-key map (kbd "c") #'org-air-capture)
    (define-key map (kbd "m") #'org-air-toggle-mark)
    ;; Triage disposition vocabulary (air/v0.2/org-air-triage.org).
    ;; R5: `s' is board SCOPE again (the user-facing mnemonic; the
    ;; round-5 schedule-on-s remap is reverted).  Inline scheduling lives
    ;; in the process-inbox flow.
    (define-key map (kbd "s") #'org-air-scope)
    (define-key map (kbd "d") #'org-air-item-deadline)
    (define-key map (kbd "r") #'org-air-refile-item)
    (define-key map (kbd "f") #'org-air-item-file-group)
    (define-key map (kbd "t") #'org-air-set-tag)
    (define-key map (kbd "T") #'org-air-item-cycle-todo)
    (define-key map (kbd "a") #'org-air-item-archive)
    (define-key map (kbd "D") #'org-air-item-done)
    ;; R3: the kill/delete disposition moves OFF the motion key `k' onto
    ;; the guarded `x' (it still confirms); `k' is now line-up.
    (define-key map (kbd "x") #'org-air-item-kill)
    (define-key map (kbd "u") #'org-air-triage-undo)
    (define-key map (kbd "I") #'org-air-process-inbox)
    (define-key map (kbd "/") #'org-air-filter)
    (define-key map (kbd "\\") #'org-air-filter-clear)
    ;; Scope moves off the prime key so s = schedule (the triage verb).
    (define-key map (kbd "S") #'org-air-scope-clear)
    ;; B4: vim/evil g-prefix — "g r" refresh, "g g" top of pane, "g R"
    ;; refresh+clear; "G" jumps to the bottom of the pane.
    (define-key map (kbd "g") org-air-g-prefix-map)
    (define-key map (kbd "G") #'org-air-goto-bottom)
    ;; F5: open the Air-docs project tree view.
    (define-key map (kbd "P") #'org-air-project)
    (define-key map (kbd "<") #'org-air-calendar-prev)
    (define-key map (kbd ">") #'org-air-calendar-next)
    (define-key map (kbd ".") #'org-air-calendar-today)
    (define-key map (kbd "?") #'org-air-help)
    (define-key map (kbd "q") #'org-air-quit)
    map)
  "Keymap for `org-air-view-mode'.")

(defalias 'org-air-mode-map 'org-air-view-mode-map)

(define-derived-mode org-air-view-mode special-mode "org-air"
  "Major mode for the org-air dashboard."
  (setq-local truncate-lines t)
  ;; S1: the header band is in-buffer text only; never a header line.
  (setq-local header-line-format nil)
  ;; D-P2 #4: a touch of `line-spacing' (`org-air-line-spacing', default
  ;; 0.15) calms the stacked capsules.  This reverses the round-8 S8
  ;; line-spacing 0 (which kept box-drawing rules unbroken); set
  ;; `org-air-line-spacing' to 0 to restore the tight S8 packing.
  (setq-local line-spacing org-air-line-spacing)
  (setq-local cursor-type 'bar)
  (setq-local org-air-layout-refresh-function #'org-air-view--resize-refresh)
  (setq-local buffer-read-only t)
  ;; T6: re-fit when the font/text size changes (text-scale alters how many
  ;; columns/rows fit), debounced through the same window-size path.
  (add-hook 'text-scale-mode-hook #'org-air-view--text-scale-refresh nil t)
  ;; V3: the round-4/T7 buffer-box outer frame is DROPPED — it shipped
  ;; half-drawn (partial top edge, deferred right) and a half-box reads
  ;; worse than none.  Structure comes from the in-buffer full-width
  ;; hairline rules (S2) and the single internal rail divider; no chrome
  ;; frame, so `header-line-format' stays nil (S1) and the mode-line is
  ;; the default.
  (org-air-view--setup-evil)
  (org-air-layout-install-window-size-hook))

(defun org-air-view--text-scale-refresh ()
  "Re-fit the dashboard after a text-scale/font-size change (T6).
Routes through the debounced resize handler so a rapid sequence of scale
changes coalesces into a single re-render."
  (org-air-layout--window-size-change))

(defalias 'org-air-mode #'org-air-view-mode)

(declare-function evil-set-initial-state "evil-core")
(declare-function evil-make-overriding-map "evil-common")

(defun org-air-view--setup-evil ()
  "Integrate the dashboard keymap with evil, when evil is loaded.
U2: under evil, single-key dashboard bindings are otherwise shadowed by
evil's motion/normal state maps and only fire after a \\=`\\\=' prefix.
This is a soft dependency — evil is never required.  When evil is
available we place the buffer in motion state and make the org-air keymap
an overriding map so the dashboard keys win, while evil's own scrolling
motions keep working.  Non-evil users are entirely unaffected."
  (when (fboundp 'evil-make-overriding-map)
    (evil-make-overriding-map org-air-view-mode-map 'motion))
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'org-air-view-mode 'motion)))

(defun org-air-view--glyph (name)
  "Return glyph NAME with a TTY fallback."
  (org-air-layout-glyph name))

(defun org-air-view--margin ()
  "Return the standard left margin string."
  (make-string org-air-margin ?\s))

(defun org-air-view--item-margin ()
  "Return the item-row indentation so rows hang under their heading (D6).
Item rows sit at the content margin plus `org-air-item-indent' (col 6);
inside the two-pane item pane the wrapping margin is added downstream,
so only `org-air-item-indent' is applied here.  This is width-
independent, keeping headings (col 2) hanging over items (col 6) in both
stacked and two-pane layouts."
  (make-string (if org-air-view--pane-indented
                   org-air-item-indent
                 (+ org-air-margin org-air-item-indent))
               ?\s))

(defun org-air-view--date-key (time)
  "Return YYYY-MM-DD key for TIME."
  (format-time-string "%F" time))

(defun org-air-view--days-between (then now)
  "Return calendar days between THEN and NOW."
  (- (time-to-days now) (time-to-days then)))

(defun org-air-view--timestamp-time (timestamp)
  "Return Emacs time for Org TIMESTAMP."
  (when timestamp (ignore-errors (org-timestamp-to-time timestamp))))

(defun org-air-view--human-date (time &optional now)
  "Return a compact human date label for TIME relative to NOW."
  (let* ((now (or now (current-time)))
         (delta (org-air-view--days-between now time)))
    (cond
     ((= delta 0) "Today")
     ((= delta 1) "Tomorrow")
     ((and (> delta 1) (< delta 7)) (format-time-string "%a" time))
     ((= (string-to-number (format-time-string "%Y" time))
         (string-to-number (format-time-string "%Y" now)))
      (format-time-string "%d %b" time))
     (t (format-time-string "%d %b %y" time)))))

(defun org-air-view--marker-timestamp-time (item)
  "Return first timestamp in ITEM subtree, if any."
  (when-let* ((marker (org-air-item-marker item))
              (buffer (marker-buffer marker)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char marker)
        (org-back-to-heading t)
        (let ((end (save-excursion (org-end-of-subtree t t))))
          (when (re-search-forward org-ts-regexp-both end t)
            (ignore-errors
              (org-timestamp-to-time
               (org-timestamp-from-string (match-string-no-properties 0))))))))))

(defun org-air-view--date-label (item bucket)
  "Return (LABEL . FACE) date metadata for ITEM in BUCKET."
  (let* ((now (current-time))
         (scheduled (org-air-view--timestamp-time (org-air-item-scheduled item)))
         (deadline (org-air-view--timestamp-time (org-air-item-deadline item))))
    (cond
     ((and deadline (> (org-air-view--days-between deadline now) 0))
      (cons (format "OVERDUE %dd" (abs (org-air-view--days-between deadline now)))
            'org-air-face-overdue))
     ((and scheduled (> (org-air-view--days-between scheduled now) 0))
      (cons (format "OVERDUE %dd" (abs (org-air-view--days-between scheduled now)))
            'org-air-face-overdue))
     (deadline (cons (org-air-view--human-date deadline now) 'org-air-face-deadline))
     (scheduled (cons (org-air-view--human-date scheduled now) 'org-air-face-scheduled))
     ((eq bucket 'attention) (cons "no date" 'org-air-face-date))
     ((eq bucket 'stale)
      (when-let* ((activity (or (org-air-view--marker-timestamp-time item)
                                (when-let* ((file (org-air-item-file item))
                                            ((file-exists-p file)))
                                  (file-attribute-modification-time
                                   (file-attributes file))))))
        (cons (format "· %dd quiet" (org-air-view--days-between activity now))
              'org-air-face-date))))))

(defun org-air-view--priority-char (item)
  "Return ITEM priority character, or nil."
  (when-let* ((priority (org-air-item-priority item)))
    (let ((char (- org-priority-lowest (/ priority 1000))))
      (and (characterp char) char))))

(defconst org-air-view--denote-id-regexp
  "\\`[0-9]\\{8\\}T[0-9]\\{6\\}--"
  "Anchored regexp matching the Denote identifier prefix of a file name (F1).")

(defun org-air-view--denote-title (filename)
  "Return the de-machined Denote title of FILENAME, or nil (F1).
Strips the \"YYYYMMDDTHHMMSS--\" identifier, the \"__tag_tag\" signature
and the extension, leaving the title slug; hyphens become spaces when
`org-air-origin-deslugify' is non-nil.  Returns nil when FILENAME is not a
Denote-style name."
  (let ((base (file-name-nondirectory (or filename ""))))
    (when (string-match org-air-view--denote-id-regexp base)
      (let* ((rest (file-name-sans-extension (substring base (match-end 0))))
             (slug (if (string-match "__" rest)
                       (substring rest 0 (match-beginning 0))
                     rest)))
        (unless (string-empty-p slug)
          (if org-air-origin-deslugify
              (replace-regexp-in-string "-" " " slug)
            slug))))))

(defun org-air-view--org-file-title (file)
  "Return the \"#+title:\" keyword of FILE, or nil (F1, `title-from-org')."
  (when (and file (file-readable-p file))
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents file nil 0 4096)
        (goto-char (point-min))
        (when (re-search-forward "^#\\+title:[ \t]*\\(.+?\\)[ \t]*$" nil t)
          (let ((title (match-string-no-properties 1)))
            (unless (string-empty-p title) title)))))))

(defun org-air-view--origin (item)
  "Return origin breadcrumb for ITEM, honouring `org-air-origin-style' (F1)."
  (if org-air-show-group
      (or (org-air-item-group item) "")
    (let* ((file (org-air-item-file item))
           (leaf (file-name-nondirectory (or file ""))))
      (pcase org-air-origin-style
        ('filename leaf)
        ('title-from-org (or (org-air-view--org-file-title file)
                             (org-air-view--denote-title file)
                             leaf))
        (_ (or (org-air-view--denote-title file) leaf))))))

(defun org-air-view--filter-tags ()
  "Return active tag filters as a list."
  (cond
   ((null org-air-view--tag-filter) nil)
   ((listp org-air-view--tag-filter) org-air-view--tag-filter)
   ((stringp org-air-view--tag-filter) (list org-air-view--tag-filter))))

(defun org-air-view--passes-filter-p (item)
  "Return non-nil when ITEM passes active tag filters."
  (let ((filters (org-air-view--filter-tags))
        (tags (mapcar #'downcase (org-air-item-tags item))))
    (or (null filters)
        (let ((filters (mapcar #'downcase filters)))
          (if (eq org-air-filter-match 'all)
              (seq-every-p (lambda (tag) (member tag tags)) filters)
            (seq-some (lambda (tag) (member tag tags)) filters))))))

(defun org-air-view--passes-scope-p (item)
  "Return non-nil when ITEM passes the active scope."
  (pcase org-air-view--scope
    (`nil t)
    (`(:tag ,tag) (member tag (org-air-item-tags item)))
    (`(:group ,group) (equal group (org-air-item-group item)))
    (`(:file ,file) (equal (file-truename file)
                           (file-truename (org-air-item-file item))))
    (_ t)))

(defun org-air-view--visible-items (items)
  "Return ITEMS after scope and filter."
  (seq-filter (lambda (item)
                (and (org-air-view--passes-scope-p item)
                     (org-air-view--passes-filter-p item)))
              items))

(defun org-air-view--items-for-bucket (bucket items)
  "Return visible ITEMS classified into BUCKET.
Real-signal membership (ruling xsqrnoyn): an item appears in every bucket
it genuinely qualifies for, so a dated inbox capture shows in BOTH Inbox
and its date bucket.  The no-date attention default for inbox-dwellers is
suppressed in `org-air-classify-item', not here, so no dedup is needed."
  (seq-filter (lambda (item)
                (memq bucket (org-air-classify-item item)))
              (org-air-view--visible-items items)))

(defun org-air-view--render-width ()
  "Return the width used for current org-air view rendering."
  (or org-air-view--line-width org-air-view-width (org-air-layout-current-width)))

(defun org-air-view--render-height ()
  "Return the total body height for full-height composition (S6)."
  (or org-air-view-height (org-air-layout-current-height)))

(defun org-air-view--pad-to (string width)
  "Return STRING truncated or padded to display WIDTH.
Padding uses literal spaces and `string-width'; no display alignment
properties are introduced."
  (let* ((ellipsis (org-air-view--glyph 'more))
         (trimmed (truncate-string-to-width (or string "") width nil nil ellipsis))
         (missing (- width (string-width trimmed))))
    (if (> missing 0)
        (concat trimmed (make-string missing ?\s))
      trimmed)))

(defun org-air-view--justify (left right width)
  "Return LEFT and RIGHT justified within display WIDTH."
  (let* ((left (or left ""))
         (right (or right ""))
         (available (- width (string-width left) (string-width right)))
         (padding (make-string (max 1 available) ?\s)))
    (org-air-view--pad-to (concat left padding right) width)))

(defun org-air-view--right (string &optional face)
  "Return STRING with FACE padded to the current pane's right edge."
  (let* ((text (if face (propertize string 'face face) string))
         (padding (- (org-air-view--render-width)
                     (current-column)
                     (string-width string))))
    (concat (make-string (max 1 padding) ?\s) text)))

(defun org-air-view--compose-columns (panes divider)
  "Zip PANES into composed rows joined by DIVIDER.
PANES is a list of (LINES . WIDTH).  Short panes are blank-filled and each
line is normalized with `org-air-view--pad-to'."
  (let* ((height (apply #'max 0 (mapcar (lambda (pane) (length (car pane))) panes)))
         rows)
    (cl-loop for row-number below height
             do (push (string-join
                       (mapcar (lambda (pane)
                                 (org-air-view--pad-to
                                  (or (nth row-number (car pane)) "")
                                  (cdr pane)))
                               panes)
                       divider)
                      rows))
    (nreverse rows)))

(defun org-air-view--insert-tag-chip (tag &optional active)
  "Insert TAG as a deterministic colour chip.
When ACTIVE is non-nil, use the active-filter tag face."
  (let ((start (point))
        (face (if active 'org-air-face-tag-active (org-air-faces-tag-face tag))))
    (insert-text-button (concat "#" tag)
                        'follow-link t
                        'action (lambda (_button) (org-air-filter-toggle tag))
                        'face face
                        'org-air-tag tag)
    (add-text-properties start (point) `(org-air-tag ,tag))))

(defun org-air-view--insert-tags (tags)
  "Insert up to `org-air-tags-inline-max' TAGS as chips."
  (let* ((shown (seq-take tags org-air-tags-inline-max))
         (overflow (- (length tags) (length shown)))
         (filters (org-air-view--filter-tags)))
    (dolist (tag shown)
      (insert " ")
      (org-air-view--insert-tag-chip tag (member tag filters)))
    (when (> overflow 0)
      (insert " " (propertize (format "+%d" overflow) 'face 'org-air-face-count)))))

(defcustom org-air-header-accent-count nil
  "When non-nil, the header item count is salient too (D-P3).
Default nil keeps the count faded; the date token always takes the quiet
`org-air-face-header-date' accent.  Face-only: the assembled header string
width is unchanged, so fixtures hold."
  :type 'boolean
  :group 'org-air)

(defun org-air-view--insert-banner (items)
  "Insert the org-air header band for ITEMS (S1 single in-buffer band).
The right status is justified to the displaying window width W with a
reserved one-column right margin: its last visible glyph sits at column
W-1, never W, so a zero-fringe GUI never draws a continuation glyph over
it (S7).  When the window is too narrow the status sheds tokens in
priority order — filter chips, then scope, then the item count — always
keeping the date."
  (let* ((w (org-air-view--render-width))
         (left (propertize "  org-air" 'face 'org-air-face-header))
         ;; D-P3: per-segment faces — date salient, count faded (or salient
         ;; via `org-air-header-accent-count'), filter/scope faded.  The
         ;; assembled width is unchanged (propertize never alters it).
         (date (propertize (format-time-string "%a %d %b" (current-time))
                           'face 'org-air-face-header-date))
         (count (propertize
                 (format " · %d items" (length (org-air-view--visible-items items)))
                 'face (if org-air-header-accent-count
                           'org-air-face-count 'org-air-face-faded)))
         (filter-text (let ((filters (org-air-view--filter-tags)))
                        (when filters
                          (propertize
                           (concat " · "
                                   (mapconcat (lambda (tag) (concat "#" tag)) filters " ")
                                   " " (org-air-view--glyph 'clear))
                           'face 'org-air-face-faded))))
         (scope-text (pcase org-air-view--scope
                       (`(:tag ,tag) (propertize (concat " · #" tag)
                                                 'face 'org-air-face-faded))
                       (`(:group ,group) (propertize (concat " · @" group)
                                                     'face 'org-air-face-faded))
                       (`(:file ,file) (propertize
                                        (concat " · " (file-name-nondirectory file))
                                        'face 'org-air-face-faded))
                       (_ nil)))
         ;; Budget for the status: window minus the left token, a >=2-col
         ;; gap, and the reserved one-column right margin.
         (budget (- w (string-width left) 2 1))
         (assemble (lambda (shed)
                     (concat date
                             (unless (memq :count shed) count)
                             (unless (memq :filter shed) (or filter-text ""))
                             (unless (memq :scope shed) (or scope-text "")))))
         (status (catch 'fit
                   (dolist (shed '(() (:filter) (:filter :scope)
                                   (:filter :scope :count))
                                 date)
                     (let ((s (funcall assemble shed)))
                       (when (<= (string-width s) budget)
                         (throw 'fit s))))))
         ;; D-P3: the segments already carry their faces; keep the assembled
         ;; status as-is (no blanket faded override).
         (right status)
         ;; Justify with a trailing space so the status ends at W-1 and the
         ;; final column W is always blank (the reserved margin).
         (line (org-air-view--justify left (concat right " ") w)))
    (insert line "\n")))

(defun org-air-view--rule-string (width)
  "Return a horizontal rule of display WIDTH."
  (let ((glyph (org-air-view--glyph 'hrule)))
    (mapconcat #'identity (make-list width glyph) "")))

(defun org-air-view--insert-rule ()
  "Insert a faint full-width separator."
  (let* ((margin (org-air-view--margin))
         (rule-width (max 0 (- (org-air-view--render-width) (string-width margin)))))
    (insert margin
            (propertize (org-air-view--rule-string rule-width)
                        'face 'org-air-face-separator)
            "\n")))

(defun org-air-view--empty-upcoming ()
  "Return upcoming empty state."
  (format "Nothing scheduled in the next %d days." org-air-upcoming-days))

(defun org-air-view--empty-message (message)
  "Resolve MESSAGE as a string or function."
  (if (functionp message) (funcall message) message))

(defun org-air-view--insert-section-heading (bucket title count attentionp)
  "Insert section heading for BUCKET TITLE with COUNT.
ATTENTIONP means the count should use the attention badge face."
  (let ((start (point)))
    (insert (if org-air-view--pane-indented "" (org-air-view--margin))
            (propertize (org-air-view--glyph bucket) 'face 'org-air-face-section-icon)
            " "
            (propertize title 'face 'org-air-face-section)
            ;; D6 — one space before the inverse count chip; never wraps
            ;; (single heading line, `truncate-lines' is t).
            " "
            (propertize (format "%d" count)
                        'face (if attentionp
                                  'org-air-face-count-attention
                                'org-air-face-count))
            "\n")
    (add-text-properties start (point) `(org-air-section ,bucket org-air-count-badge ,count))
    (when org-air-section-rule
      (org-air-view--insert-rule))))

(defun org-air-view--todo-face (keyword)
  "Return the face for TODO KEYWORD (T1a), defaulting to `org-air-face-todo'."
  (or (cdr (assoc keyword org-air-todo-keyword-faces))
      'org-air-face-todo))

(defun org-air-view--priority-face (char)
  "Return the boxed-pill face for priority CHAR (T1b)."
  (pcase char
    (?A 'org-air-face-priority-a)
    (?B 'org-air-face-priority-b)
    (_ 'org-air-face-priority-c)))

(defun org-air-view--svg-available-p ()
  "Return non-nil when svg pills can be drawn on this display (C2)."
  (and (display-graphic-p)
       (require 'svg nil t)))

(defun org-air-view--char-dimensions ()
  "Return (CHAR-W . CHAR-H) device pixels for the displaying window (C2/C3).
Uses `window-font-width'/`window-font-height' on the window actually
showing the org-air buffer so the metrics track the current font AND any
`text-scale-mode' adjustment (C3); falls back to the frame char metrics
when no graphical window is available."
  (let ((win (get-buffer-window (current-buffer) t)))
    (if (and win (display-graphic-p (window-frame win))
             (fboundp 'window-font-width))
        (cons (or (ignore-errors (window-font-width win)) (frame-char-width))
              (or (ignore-errors (window-font-height win)) (frame-char-height)))
      (cons (frame-char-width) (frame-char-height)))))

(defun org-air-view--string-pixel-width-available-p ()
  "Return non-nil when `string-pixel-width' can measure here (Emacs >= 29)."
  (and (fboundp 'string-pixel-width) (display-graphic-p)))

(defun org-air-view--pill-label-width (label fs cw ch)
  "Return LABEL's natural pixel width at font-size FS (device px) (D-P1.FIT).
Prefer `string-pixel-width' inside a temp face scaled to FS/CH of the
frame default; fall back to the column-width estimate `(* (string-width
LABEL) CW)' when `string-pixel-width' is unavailable (Emacs < 29)."
  (or (and (org-air-view--string-pixel-width-available-p)
           (> ch 0)
           (ignore-errors
             (string-pixel-width
              (propertize label 'face (list :height (/ fs (float ch)))))))
      (* (string-width label) cw)))

(cl-defun org-air-view--svg-pillify (text face &key (align 'center))
  "Return TEXT carrying a rounded svg-pill `display' overlay (C2/D-P1).
ALIGN places the label inside the box: `center' (default) or `right'
\(D-P1 `org-air-date-pill-align').
The pill SVG occupies EXACTLY TEXT's text-cell box —
box-w = Ncols * char-px, height = the line's pixel height — where Ncols is
TEXT's column width (INCLUDING the `org-air-pill-pad-cols' reserved pad
spaces, D-P1.PAD) and char-px/line-px are the current (text-scale aware)
metrics bound in `org-air-view--pill-char-w'/`-h'.  Because the image is
locked to that box it never adds external width, so turning pills on/off
changes zero V6 column positions (C2).

The label (TEXT trimmed) is drawn centred and WIDTH-FITTED to the box's
inner width (box minus the reserved pad columns) so the glyph run can
NEVER reach the rounded edge (D-P1.FIT) — no clipping at any tag length or
text-scale.  The capsule is a calm monochrome: a soft `org-air-pill-radius'
corner, no per-chip fill (`org-air-pill-fill-alpha' default 0) and ONE
muted `org-air-pill-border'; colour lives only in the LABEL via FACE, per
D-P1.LOOK.  On a non-graphical frame, when SVG is unavailable, the box
is degenerate, OR the label cannot be guaranteed to fit, TEXT is returned
unchanged so the byte/TTY layer keeps the plain padded coloured text as a
mandatory fallback."
  (let ((ncols (string-width text)))
    (if (or (not (org-air-view--svg-available-p))
            (string-empty-p (string-trim text))
            (<= ncols 0))
        text
      (or (ignore-errors
            (let* ((cw (or org-air-view--pill-char-w (frame-char-width)))
                   (ch (or org-air-view--pill-char-h (frame-char-height)))
                   (pad (max 0 org-air-pill-pad-cols))
                   (box-w (* ncols cw))
                   (h ch)
                   ;; the width the label is allowed to occupy = box minus
                   ;; the reserved pad columns (D-P1.FIT).
                   (inner-w (* (max 1 (- ncols (* 2 pad))) cw))
                   (radius (max 0.0 (float (or org-air-pill-radius (/ h 6.0)))))
                   (label (string-trim text))
                   (fg (or (face-foreground face nil t) "gray"))
                   (border (or org-air-pill-border
                               (face-foreground 'org-air-face-faded nil t)
                               "gray"))
                   (alpha (max 0.0 (min 1.0 (float org-air-pill-fill-alpha))))
                   (desired-fs (max 7 (round (* ch org-air-pill-font-scale))))
                   (natural-w (org-air-view--pill-label-width
                               label desired-fs cw ch))
                   ;; width fit: never exceed inner-w (the actual clip fix).
                   (font-size (if (and (> natural-w inner-w) (> natural-w 0))
                                  (max 7 (floor (* desired-fs
                                                   (/ inner-w (float natural-w)))))
                                desired-fs)))
              (if (and (not (org-air-view--string-pixel-width-available-p))
                       (> (org-air-view--pill-label-width label font-size cw ch)
                          inner-w))
                  ;; D-P1.FIT cannot guarantee a fit (no string-pixel-width
                  ;; AND the estimate already overruns) -> mandatory text
                  ;; fallback (plain padded coloured label, no pill).
                  text
                (let ((svg (svg-create box-w h))
                      (stroke-op (max 0.0 (min 1.0 (float org-air-pill-border-opacity)))))
                  (svg-rectangle svg 0.5 0.5
                                 (max 0 (- box-w 1.0)) (max 0 (- h 1.0))
                                 :rx radius :ry radius
                                 :fill (if (> alpha 0) fg "none")
                                 :fill-opacity (if (> alpha 0) alpha 0)
                                 :stroke border :stroke-width 1
                                 ;; D-P2 #1: hairline border at reduced opacity.
                                 :stroke-opacity stroke-op)
                  ;; D-P1: label placement — centred (default) or right-hugged.
                  (if (eq align 'right)
                      (svg-text svg label
                                :x (- box-w (* pad cw))
                                :y (round (* ch 0.72))
                                :text-anchor "end"
                                :fill fg
                                :font-size font-size)
                    (svg-text svg label
                              :x (/ box-w 2.0)
                              :y (round (* ch 0.72))
                              :text-anchor "middle"
                              :fill fg
                              :font-size font-size))
                  ;; Lock the displayed image to the exact cell box so the
                  ;; run of Ncols characters occupies Ncols*char-px pixels
                  ;; — no more, no less (C2); centre on the text line.
                  (propertize text 'display
                              (svg-image svg :ascent 'center
                                         :width box-w :height h))))))
          text))))

(defun org-air-view--pill-pad-label (label face)
  "Return LABEL carrying FACE and the reserved pill pad columns (D-P1.PAD).
When a pill style is active the label string carries `org-air-pill-pad-cols'
space columns on EACH side in the text layer (svg-tag-mode's technique), so
`org-air-view--compute-meta-widths' counts the pad automatically and V6
alignment holds.  The pad spaces inherit FACE so the TTY/fallback shows the
same quiet coloured breathing space."
  (let* ((pad (max 0 org-air-pill-pad-cols))
         (sp (make-string pad ?\s)))
    (propertize (concat sp label sp) 'face face)))

(defun org-air-view--item-tagstr (tags k total)
  "Return inline tag chips showing K of TOTAL TAGS, with overflow marker.
When fewer than TOTAL chips are shown the `more' glyph signals the rest.
Each chip carries its deterministic accent face (T1c); when the pill style
is active the chip text also carries the D-P1.PAD reserved pad columns so
the svg box gains genuine internal margin and the byte/V6 widths track it."
  (let ((shown (mapconcat
                (lambda (tg)
                  (let* ((face (org-air-faces-tag-face tg))
                         (pill (eq org-air-tag-style 'pill))
                         (chip (if pill
                                   (org-air-view--pill-pad-label (concat "#" tg) face)
                                 (propertize (concat "#" tg) 'face face))))
                    (if pill
                        (org-air-view--svg-pillify chip face)
                      chip)))
                (seq-take tags k) " "))
        (overflow (- total k)))
    (cond
     ((<= total 0) "")
     ;; D-P3: NEVER `string-trim' a string carrying pill display props.
     ;; Trimming strips the first chip's reserved leading pad, desyncing
     ;; its baked svg box from its column run and drifting the origin
     ;; column.  Keep every chip's pad; append the overflow marker as a
     ;; plain, unpilled 1-col token (counted in `meta-tags-w').
     ((> overflow 0)
      (concat shown " "
              (propertize (org-air-view--glyph 'more) 'face 'org-air-face-faded)))
     (t shown))))

(defun org-air-view--item-origin-raw (item)
  "Return the origin breadcrumb \"⌂ FILE\" for ITEM (unfaced)."
  (concat (org-air-view--glyph 'origin) " " (org-air-view--origin item)))

(defun org-air-view--item-date-text (item bucket)
  "Return the propertized date text for ITEM in BUCKET (V6/R10), or nil.
The date is coloured TEXT in its semantic face; a dated-but-unfiled Inbox
row also carries the quiet \"· file with r\" triage nudge (ruling
xsqrnoyn).  The GUI pill (V3) is a non-byte overlay over this same text."
  (let* ((date (org-air-view--date-label item bucket))
         (inbox-hint (and (eq bucket 'inbox)
                          (or (org-air-item-scheduled item)
                              (org-air-item-deadline item))
                          (propertize " · file with r" 'face 'org-air-face-faded))))
    (when date
      (let* ((face (or (cdr date) 'org-air-face-date))
             (pill (eq org-air-date-style 'pill))
             ;; D-P1: pad the label to the FULL date column before pillify
             ;; (left-justified text fallback) so every date pill's box =
             ;; meta-date-w × char-px — uniform capsules (V6 pixel-lock holds).
             (col (max (or org-air-view--meta-date-w org-air-date-column)
                       (string-width (car date))))
             (text (if pill
                       (propertize (org-air-view--pad-to (car date) col) 'face face)
                     (propertize (car date) 'face face))))
        (concat (if pill
                    (org-air-view--svg-pillify text face
                                               :align org-air-date-pill-align)
                  text)
                (or inbox-hint ""))))))

(defun org-air-view--compute-meta-widths (items)
  "Set the V6 metadata column widths over the rendered ITEMS.
Walks the same section buckets the pane renders and records the widest
date label (bare, no Inbox nudge), tag string and origin so date / tags /
origin each occupy a fixed-width column down the whole list and line up
vertically.  The date floor is `org-air-date-column'."
  (let ((dw org-air-date-column) (tw 0) (ow 0))
    (dolist (descriptor org-air-view--sections)
      (let* ((bucket (car descriptor))
             (bucket-items (org-air-view--items-for-bucket bucket items)))
        (dolist (item bucket-items)
          (let* ((date (org-air-view--date-label item bucket))
                 (tags (org-air-item-tags item))
                 (n (length tags))
                 (ts (org-air-view--item-tagstr
                      tags (min org-air-tags-inline-max n) n)))
            (when date
              (setq dw (max dw (+ (string-width (car date))
                                  (if (eq org-air-date-style 'pill)
                                      (* 2 (max 0 org-air-pill-pad-cols))
                                    0)))))
            (setq tw (max tw (string-width ts)))
            (setq ow (max ow (string-width
                              (org-air-view--item-origin-raw item))))))))
    (setq org-air-view--meta-date-w dw
          org-air-view--meta-tags-w tw
          org-air-view--meta-origin-w ow)))

(cl-defun org-air-view--insert-row (&key prefix title date-text tags
                                         origin-text origin-face widths
                                         props face)
  "Insert one shared V6 fixed-column row (D-P5.A; the board + project floor).
PREFIX leads the line (todo/priority markers, or a state chip); the TITLE
owns the LEFT and stays clean; the metadata is a fixed-width
right-aligned cluster of DATE-TEXT / TAGS / ORIGIN-TEXT.  WIDTHS is
\(DCOL TCOL OCOL); a cell whose column width is 0 is omitted.  DATE-TEXT
and TAGS are pre-faced/pilled strings (left-justified); ORIGIN-TEXT is
right-justified in OCOL and faced with ORIGIN-FACE.  Because every cell is
fixed width the columns line up vertically down the list (V6).  The title
flexes and truncates LAST (D2) in the single gap before the cluster.
PROPS are added as text properties over the whole row and FACE is its
`font-lock-face' (so both the board's items and the project's docs share
this one primitive, faces, truncation, alignment and svg pills)."
  (let* ((start (point))
         (width (org-air-view--render-width))
         (prefix (or prefix ""))
         (prefix-w (string-width prefix))
         (title (or title ""))
         (gap 2)
         (dcol (or (nth 0 widths) 0))
         (tcol (or (nth 1 widths) 0))
         (ocol (or (nth 2 widths) 0))
         ;; date cell: left-justified; expands locally (e.g. for the Inbox
         ;; nudge baked into DATE-TEXT) so the row alone widens, never clips.
         (date-cell (when (> dcol 0)
                      (org-air-view--pad-to
                       (or date-text "")
                       (max dcol (string-width (or date-text ""))))))
         (tags-cell (when (> tcol 0) (org-air-view--pad-to (or tags "") tcol)))
         (origin-cell (when (> ocol 0)
                        (let* ((ot (or origin-text ""))
                               (w (string-width ot)))
                          (concat (make-string (max 0 (- ocol w)) ?\s)
                                  (if origin-face
                                      (propertize ot 'face origin-face)
                                    ot)))))
         (cluster (mapconcat #'identity
                             (delq nil (list date-cell tags-cell origin-cell))
                             " "))
         (cluster-w (string-width cluster))
         ;; title flexes/truncates in the space before the fixed cluster.
         ;; V6: it floors at 1 so the cluster stays at a fixed column — a
         ;; long title or wide prefix must not push the columns out.
         (avail-title (- width prefix-w gap cluster-w))
         (title (if (<= (string-width title) avail-title)
                    title
                  (truncate-string-to-width
                   title (max 1 avail-title) nil nil
                   (org-air-view--glyph 'more))))
         (left (concat prefix title))
         ;; V6 (D-P1.PAD fix): the cluster MUST start at the fixed column
         ;; `width - cluster-w' regardless of prefix/title/pad.  At the
         ;; narrow tier a wide prefix (e.g. the [#A] priority badge) plus
         ;; the reserved pad cols can make prefix + the 1-col title floor
         ;; overrun the left budget; flooring the pad at `gap' would then
         ;; shove the cluster right and break the date/tag/origin column
         ;; alignment.  Clamp the assembled LEFT to the budget so the
         ;; cluster column never shifts (a no-op when LEFT already fits,
         ;; so the wider tiers stay byte-identical).
         (left-budget (max 0 (- width cluster-w gap)))
         (left (if (<= (string-width left) left-budget)
                   left
                 (truncate-string-to-width
                  left left-budget nil nil (org-air-view--glyph 'more))))
         (pad (max gap (- width (string-width left) cluster-w)))
         (line (concat left (make-string pad ?\s) cluster)))
    (insert line "\n")
    (when (or props face)
      (add-text-properties start (point)
                           (append props
                                   (when face (list 'font-lock-face face)))))))

(defun org-air-view--insert-item (item bucket &optional omit-date)
  "Insert ITEM as an interactive row in BUCKET (V6 fixed-column table).
A thin caller of the shared `org-air-view--insert-row' (D-P5.A): it maps
the task ITEM onto the row args (todo/priority prefix, title, date / tags
/ origin cluster).  OMIT-DATE drops the date column (R6 day view)."
  (let* ((todo (org-air-item-todo item))
         (priority (org-air-view--priority-char item))
         (date-text (unless omit-date (org-air-view--item-date-text item bucket)))
         (prefix (concat (org-air-view--item-margin)
                         (when todo
                           (concat (propertize todo 'face
                                               (org-air-view--todo-face todo))
                                   " "))
                         (when (and priority (member priority org-air-priority-show))
                           (concat (propertize (format "[#%c]" priority) 'face
                                               (org-air-view--priority-face priority))
                                   " "))))
         (tags (org-air-item-tags item))
         (n-tags (length tags))
         (tagstr (org-air-view--item-tagstr
                  tags (min org-air-tags-inline-max n-tags) n-tags))
         (origin-raw (org-air-view--item-origin-raw item))
         ;; V6 fixed column widths (computed over the whole list; fall back
         ;; to this single row when unset, e.g. the day pane).
         (dcol (if omit-date 0 (or org-air-view--meta-date-w org-air-date-column)))
         (tcol (or org-air-view--meta-tags-w (string-width tagstr)))
         (ocol (or org-air-view--meta-origin-w (string-width origin-raw))))
    (org-air-view--insert-row
     :prefix prefix
     :title (org-air-item-title item)
     :date-text date-text
     :tags tagstr
     :origin-text origin-raw
     :origin-face 'org-air-face-group
     :widths (list dcol tcol ocol)
     :props (list 'org-air-item item
                  'org-air-marker (org-air-item-marker item)
                  'mouse-face 'org-air-face-cursor)
     :face 'org-air-face-title)))

(defun org-air-view--item-sort-time (item)
  "Return the effective deadline/scheduled time for ITEM, or nil."
  (or (org-air-view--timestamp-time (org-air-item-deadline item))
      (org-air-view--timestamp-time (org-air-item-scheduled item))))

(defun org-air-view--sort-by-date (items)
  "Return ITEMS ordered by effective date, undated items last.
The order is stable so items sharing a date keep their incoming order."
  (let ((indexed (let ((i 0))
                   (mapcar (lambda (item) (prog1 (cons i item) (setq i (1+ i))))
                           items))))
    (mapcar #'cdr
            (sort indexed
                  (lambda (a b)
                    (let ((ta (org-air-view--item-sort-time (cdr a)))
                          (tb (org-air-view--item-sort-time (cdr b))))
                      (cond
                       ((and ta tb)
                        (if (time-equal-p ta tb)
                            (< (car a) (car b))
                          (time-less-p ta tb)))
                       (ta t)
                       (tb nil)
                       (t (< (car a) (car b))))))))))

(defun org-air-view--insert-section (descriptor items)
  "Insert section DESCRIPTOR from ITEMS."
  (pcase-let ((`(,bucket ,title ,empty) descriptor))
    (let* ((bucket-items (org-air-view--items-for-bucket bucket items))
           (bucket-items (if (memq bucket '(attention upcoming))
                             (org-air-view--sort-by-date bucket-items)
                           bucket-items))
           ;; S4: the badge counts exactly what the section renders
           ;; (`items-for-bucket', which keeps inbox items out of the
           ;; other buckets), so badge/summary/body always agree.
           (count (length bucket-items))
           (attentionp (and (> count 0) (memq bucket '(inbox attention))))
           (expanded (memq bucket org-air-view--expanded-sections))
           (limit (pcase bucket
                    ('attention 6)
                    ('upcoming 5)
                    (_ org-air-section-max)))
           (visible (if expanded bucket-items (seq-take bucket-items limit))))
      (insert "\n")
      (org-air-view--insert-section-heading bucket title count attentionp)
      (if bucket-items
          (progn
            (dolist (item visible)
              (org-air-view--insert-item item bucket))
            (when (> count (length visible))
              (insert (org-air-view--item-margin)
                      (propertize (format "%sand %d more — press TAB on the title to expand\n"
                                          (org-air-view--glyph 'more)
                                          (- count (length visible)))
                                  'face 'org-air-face-faded))))
        (insert (org-air-view--item-margin)
                (propertize (org-air-view--empty-message empty)
                            'face 'org-air-face-empty)
                "\n")))))

(defun org-air-view--insert-footer ()
  "Insert footer hint line."
  (when org-air-show-footer
    (insert (propertize
             (org-air-view--pad-to
              (concat (org-air-view--margin)
                      (if (<= (org-air-view--render-width) 80)
                          "[c]apture [g]refresh [/]filter [\\]clear [s]cope [TAB]next RET visit [?]help"
                        "[c]apture  [g]refresh  [/]filter  [\\]clear  [s]cope  [TAB]next  RET visit  [?]help"))
              (org-air-view--render-width))
             'face 'org-air-face-faded)
            "\n")))

(defun org-air-view--string-lines (string width)
  "Split STRING into lines and normalize each to WIDTH."
  (mapcar (lambda (line) (org-air-view--pad-to line width))
          (let ((lines (split-string string "\n")))
            (if (and lines (equal (car (last lines)) ""))
                (butlast lines)
              lines))))

(defun org-air-view--render-lines (width render-fn)
  "Return lines of WIDTH produced by RENDER-FN in a temp buffer."
  (let ((items org-air-view--items)
        (items-key org-air-view--items-key)
        (tag-filter org-air-view--tag-filter)
        (scope org-air-view--scope)
        (expanded org-air-view--expanded-sections)
        (cal-month org-air-view--cal-month)
        (day org-air-view--day))
    (with-temp-buffer
      (let ((org-air-view--line-width width)
            (org-air-view--items items)
            (org-air-view--items-key items-key)
            (org-air-view--tag-filter tag-filter)
            (org-air-view--scope scope)
            (org-air-view--expanded-sections expanded)
            (org-air-view--cal-month cal-month)
            (org-air-view--day day))
        (funcall render-fn)
        (org-air-view--string-lines (buffer-string) width)))))

(defun org-air-view--rail-width (width)
  "Return the rail content width tier for total WIDTH (D1).
Wide (>= 150) -> `org-air-rail-width-wide' (42); mid (120-149) ->
`org-air-rail-width' (32); threshold zone (< 120) ->
`org-air-rail-width-narrow' (28)."
  (cond
   ((>= width 150) org-air-rail-width-wide)
   ((>= width 120) org-air-rail-width)
   (t org-air-rail-width-narrow)))

(defun org-air-view--two-pane-boundary ()
  "Return the minimum total width at which two-pane engages (D1).
Item-pane floor + divider + the narrow rail tier (≈ 95 cols)."
  (+ org-air-item-pane-min 3 org-air-rail-width-narrow))

(defun org-air-view--two-pane-p (width)
  "Return non-nil when WIDTH should render two-pane (D1).
Engagement is derived: two-pane iff the item pane (WIDTH minus the
tier's rail and the 3-col divider) is at least `org-air-item-pane-min'.
Within `org-air-layout-hysteresis' columns of the breakpoint the current
`org-air-view--orientation' is kept to avoid flapping on resize."
  (let* ((rail (org-air-view--rail-width width))
         (item-pane (- width rail 3))
         (base (>= item-pane org-air-item-pane-min)))
    (if (and org-air-view--orientation
             (<= (abs (- width (org-air-view--two-pane-boundary)))
                 org-air-layout-hysteresis))
        (eq org-air-view--orientation 'two-pane)
      base)))

(defun org-air-view--divider ()
  "Return the pane divider string for the current layout style."
  (if (eq org-air-layout-style 'plain)
      "   "
    (concat " " (propertize (org-air-view--glyph 'vrule)
                            'face 'org-air-face-pane-border)
            " ")))

(defun org-air-view--section-counts (items)
  "Return bucket count alist for visible ITEMS.
Counts use `org-air-view--items-for-bucket' so the summary mirrors the
section badges and bodies exactly (S4) — inbox items are not also tallied
under the other buckets."
  (mapcar (lambda (descriptor)
            (pcase-let ((`(,bucket ,_title ,_empty) descriptor))
              (cons bucket (length (org-air-view--items-for-bucket bucket items)))))
          org-air-view--sections))

(defun org-air-view--bucket-title (bucket)
  "Return display title for BUCKET."
  (cadr (assq bucket org-air-view--sections)))

(defun org-air-view--rail-inset (width)
  "Return the D5b content-spine inset for a rail of content WIDTH.
Drops to 1 at the narrow tier (content width < 30, i.e. the 28-col rail)
so the calendar grid and rules never overflow; `org-air-rail-content-inset'
\(default 3) at the mid/wide tiers."
  (if (>= width 30) (max 0 org-air-rail-content-inset) 1))

(defun org-air-view--rail-inset-str (width)
  "Return the spine-inset whitespace prefix for a rail of content WIDTH."
  (make-string (org-air-view--rail-inset width) ?\s))

(cl-defun org-air-view--rail-header (label width &key suffix suffix-face)
  "Insert a D-P6 rail section header for LABEL fitted to WIDTH.
With `org-air-rail-header-style' = `marker' (default) emit the clean
prefix-marked header with SUFFIX (e.g. the calendar nav, in SUFFIX-FACE)
right-anchored — no bg/overline/rule glyphs (reverses round-10 D-P2.A).
With `rule' restore the round-10 labelled rule."
  (if (eq org-air-rail-header-style 'rule)
      (let ((start (point)))
        (insert (org-air-view--pad-to
                 (org-air-layout-labelled-rule
                  label width :suffix suffix
                  :suffix-face (or suffix-face 'org-air-face-rail-title))
                 width)
                "\n")
        (add-face-text-property start (max start (1- (point)))
                                'org-air-face-rail-card-header t))
    (insert (org-air-layout-rail-header-string
             label width :suffix suffix
             :suffix-face (or suffix-face 'org-air-face-rail-header))
            "\n")))

(defun org-air-view--insert-labelled-rule (label width)
  "Insert a D5 rail rule labelled LABEL and fitted to WIDTH.
The rule opens with the rounded `hrule-cap' stub echoing a pill's left
edge (D5a); the rule glyphs are quiet `org-air-face-pane-border' and the
label is `org-air-face-rail-title'.
D-P2.A: the line also carries `org-air-face-rail-card-header' (a subtle bg
tint + overline) layered UNDER the rule text via `add-face-text-property'
\(APPEND), so the existing labelled rule is the TTY substrate (mandatory
fallback) while a GUI frame reads it as an hl-block card header."
  (let ((start (point)))
    (insert (org-air-view--pad-to
             (org-air-layout-labelled-rule label width)
             width)
            "\n")
    (add-face-text-property start (max start (1- (point)))
                            'org-air-face-rail-card-header t)))

(defun org-air-view--insert-summary (items width)
  "Insert summary block for ITEMS fitted to WIDTH."
  (when org-air-show-summary
    (org-air-view--rail-header "Summary" width)
    (let ((counts (org-air-view--section-counts items))
          (total (length (org-air-view--visible-items items)))
          (inset (org-air-view--rail-inset-str width)))
      (dolist (entry counts)
        (let* ((bucket (car entry))
               (count (cdr entry))
               (number-face (cond
                             ((= count 0) 'org-air-face-faded)
                             ((memq bucket '(inbox attention))
                              'org-air-face-summary-number-attention)
                             (t 'org-air-face-summary-number))))
          ;; D5b/D5d: spine inset + %3d number + 3-space gutter + label.
          (insert inset
                  (propertize (format "%3d" count) 'face number-face)
                  "   "
                  (propertize (org-air-view--bucket-title bucket)
                              'face 'org-air-face-summary-label)
                  "\n")))
      ;; D5d: a short ledger rule (over the number field) replaces the old
      ;; stray full-width hairline — the universal "sum" affordance.
      (insert inset
              (propertize (make-string 4 (string-to-char (org-air-view--glyph 'hrule)))
                          'face 'org-air-face-pane-border)
              "\n")
      (insert inset (propertize (format "%3d" total)
                                'face 'org-air-face-summary-number)
              "   " (propertize "total" 'face 'org-air-face-summary-label)
              "\n"))))

(defun org-air-view--scope-label ()
  "Return active scope display label."
  (pcase org-air-view--scope
    (`(:tag ,tag) (concat "#" tag))
    (`(:group ,group) (concat "@" group))
    (`(:file ,file) (file-name-nondirectory file))
    (_ "all items")))

(defun org-air-view--insert-rail-filters (width)
  "Insert filters and scope block fitted to WIDTH."
  (when org-air-show-rail-filters
    (org-air-view--rail-header "Filters" width)
    (let ((filters (org-air-view--filter-tags))
          (inset (org-air-view--rail-inset-str width)))
      (if (and (null filters) (null org-air-view--scope))
          (insert inset
                  (propertize "No filters · all items" 'face 'org-air-face-faded)
                  "\n")
        (progn
          (if filters
              (insert inset
                      (mapconcat (lambda (tag)
                                   (concat "#" tag " " (org-air-view--glyph 'clear)))
                                 filters " ")
                      "\n")
            (insert inset
                    (propertize "No tag filters" 'face 'org-air-face-faded) "\n"))
          (insert inset
                  (propertize (concat "Scope: " (org-air-view--scope-label)
                                      (when org-air-view--scope "  (S clears)"))
                              'face 'org-air-face-faded)
                  "\n"))))))

(defun org-air-view--verb-cell (key desc width)
  "Return a rail Actions verb cell: KEY (keycap face) DESC (faded), padded.
KEY renders in `org-air-face-rail-key' so keys read as keys; DESC stays
`org-air-face-faded'.  The cell is right-padded to display WIDTH for
column alignment (D5f); a WIDTH of 0 (the trailing column) is as-is."
  (let ((cell (concat (propertize key 'face 'org-air-face-rail-key)
                      " "
                      (propertize desc 'face 'org-air-face-faded))))
    (if (> width 0)
        (concat cell (make-string (max 0 (- width (string-width cell))) ?\s))
      cell)))

(defun org-air-view--insert-actions (width)
  "Insert the named D5f Actions block fitted to rail content WIDTH.
Two column-aligned verb rows, inset to the spine, the leading key token in
the quiet keycap face; the columns do the separating (no dotted prose)."
  (org-air-view--rail-header "Actions" width)
  (let* ((inset (org-air-view--rail-inset-str width))
         ;; Round-9 Q1: when a scope is active the second row's middle verb
         ;; surfaces the scope reset (the literal "S reset" cue the design
         ;; and grind ask for) right where the user acts.
         (mid2 (if org-air-view--scope '("S" . "reset") '("TAB" . "expand")))
         ;; Column field widths = the widest "KEY DESC" cell in each column.
         (c1 (max (+ 2 (length "capture")) (+ 2 (length "refresh"))))
         (c2 (max (+ 2 (length "filter"))
                  (+ (length (car mid2)) 1 (length (cdr mid2)))))
         ;; D5f: a 4-space column gap at the wide tier; tighten to 1 at the
         ;; mid/narrow tiers so the three verbs still fit (elide only at the
         ;; very narrow rail, as the spec allows).
         (gap (if (>= width 38) "    " " ")))
    (insert (org-air-view--pad-to
             (concat inset
                     (org-air-view--verb-cell "c" "capture" c1) gap
                     (org-air-view--verb-cell "/" "filter" c2) gap
                     (org-air-view--verb-cell "s" "scope" 0))
             width)
            "\n")
    (insert (org-air-view--pad-to
             (concat inset
                     (org-air-view--verb-cell "g" "refresh" c1) gap
                     (org-air-view--verb-cell (car mid2) (cdr mid2) c2) gap
                     (org-air-view--verb-cell "?" "help" 0))
             width)
            "\n")))

(defun org-air-view--insert-rail (items width)
  "Insert the context rail for ITEMS at WIDTH (D5 polished sidebar).
Four peer blocks — calendar, Summary, Filters, Actions — each opened by
the same labelled rule and sharing one content spine (D5a/D5b)."
  (let ((org-air-view--line-width width))
    (org-air-calendar-insert-month org-air-view--cal-month
                                   (org-air-view--visible-items items)
                                   width (org-air-view--rail-inset width))
    (insert "\n")
    (org-air-view--insert-summary items width)
    (insert "\n")
    (org-air-view--insert-rail-filters width)
    ;; D5f: the verbs are a named Actions peer block, not floating text.
    ;; Optionally pin it to the rail foot (`org-air-rail-anchor-actions').
    (when org-air-rail-anchor-actions
      (let* ((have (count-lines (point-min) (point)))
             (target (max 0 (- (org-air-view--render-height) 3 have 3))))
        (when (> target 0) (insert (make-string target ?\n)))))
    (insert "\n")
    (org-air-view--insert-actions width)))

(defun org-air-view--insert-top-rail (items width)
  "Insert the stacked top-band rail for ITEMS at total WIDTH (D3).
Three fixed columns (calendar / summary / filters), left-packed with a
2-col gutter; each labelled rule is exactly its column width so no rule
balloons to the window edge, and the calendar month-nav never truncates.
When the band is too narrow for three columns the blocks stack
vertically, calendar first — always on-screen."
  (let* ((col 24)
         (gutter 2)
         (visible (org-air-view--visible-items items))
         (calendar-fn (lambda ()
                        (org-air-calendar-insert-month org-air-view--cal-month
                                                       visible col)))
         (summary-fn (lambda () (org-air-view--insert-summary items col)))
         (filters-fn (lambda ()
                       (org-air-view--insert-rail-filters col)
                       (insert (propertize "c capture · / filter"
                                           'face 'org-air-face-faded)))))
    (if (>= width (+ (* 3 col) (* 2 gutter)))
        (let ((cal-lines (org-air-view--render-lines col calendar-fn))
              (sum-lines (org-air-view--render-lines col summary-fn))
              (fil-lines (org-air-view--render-lines col filters-fn)))
          (dolist (line (org-air-view--compose-columns
                         (list (cons cal-lines col)
                               (cons sum-lines col)
                               (cons fil-lines col))
                         (make-string gutter ?\s)))
            (insert (org-air-view--pad-to line width) "\n")))
      (let ((band (min width col)))
        (dolist (block (list calendar-fn summary-fn filters-fn))
          (dolist (line (org-air-view--render-lines band block))
            (insert (org-air-view--pad-to line width) "\n"))
          (insert "\n"))))))

(defun org-air-view--day-key (time)
  "Return the YYYY-MM-DD key for TIME."
  (format-time-string "%Y-%m-%d" time))

(defun org-air-view--day-groups (items day)
  "Return ((LABEL . ITEMS)...) grouping ITEMS by relation to DAY (R6)."
  (let ((key (org-air-view--day-key day))
        (deadline nil) (scheduled nil) (created nil))
    (dolist (item (org-air-view--visible-items items))
      (let ((d (org-air-view--timestamp-time (org-air-item-deadline item)))
            (s (org-air-view--timestamp-time (org-air-item-scheduled item)))
            (a (org-air-view--marker-timestamp-time item)))
        (cond
         ((and d (equal (org-air-view--day-key d) key)) (push item deadline))
         ((and s (equal (org-air-view--day-key s) key)) (push item scheduled))
         ((and a (equal (org-air-view--day-key a) key)) (push item created)))))
    (list (cons "Deadline" (nreverse deadline))
          (cons "Scheduled" (nreverse scheduled))
          (cons "Logged / created" (nreverse created)))))

(defun org-air-view--insert-day-pane (items width)
  "Insert the single-day focus view (R6) of ITEMS, fitted to WIDTH.
The day is `org-air-view--day'; its items are grouped Deadline >
Scheduled > Logged/created and rendered with the R10 item line minus its
now-redundant date."
  (let* ((org-air-view--line-width width)
         ;; Day view omits the date column; let the tags/origin columns
         ;; size to this focused list rather than the (stale) board.
         (org-air-view--meta-date-w nil)
         (org-air-view--meta-tags-w nil)
         (org-air-view--meta-origin-w nil)
         (day org-air-view--day)
         (groups (org-air-view--day-groups items day))
         (total (apply #'+ (mapcar (lambda (g) (length (cdr g))) groups))))
    (insert (org-air-view--justify
             (concat (org-air-view--margin)
                     (propertize (format "%s  %s  %s"
                                         (org-air-view--glyph 'cal-prev)
                                         (format-time-string "%A %-d %B %Y" day)
                                         (org-air-view--glyph 'cal-next))
                                 'face 'org-air-face-day-header))
             (propertize (format "%d items" total) 'face 'org-air-face-faded)
             width)
            "\n\n")
    (dolist (g groups)
      (when (cdr g)
        (insert (org-air-view--margin)
                (propertize (car g) 'face 'org-air-face-section) "\n")
        (dolist (item (cdr g))
          (org-air-view--insert-item item 'upcoming t))
        (insert "\n")))
    (when (zerop total)
      (insert "\n" (org-air-view--item-margin)
              (propertize "Nothing on this day." 'face 'org-air-face-empty)
              "\n"))))

(defun org-air-view--insert-item-pane (items width)
  "Insert the item pane for ITEMS at WIDTH (or the day view when focused)."
  (when org-air-view--day
    (org-air-view--insert-day-pane items width))
  (unless org-air-view--day
  (org-air-view--compute-meta-widths items)
  (let ((org-air-view--line-width width)
        (visible (org-air-view--visible-items items))
        (first t))
    (dolist (descriptor org-air-view--sections)
      (let ((start (point)))
        (org-air-view--insert-section descriptor items)
        (when (and first (= (char-after start) ?\n))
          (delete-region start (1+ start))))
      (setq first nil))
    (when (null visible)
      (insert "\n" (org-air-view--item-margin)
              (propertize "Nothing here yet. Press c to capture your first note."
                          'face 'org-air-face-empty)
              "\n")))))

(defun org-air-view--indent-pane-lines (lines width)
  "Return LINES with the standard margin inside WIDTH."
  (let ((margin (org-air-view--margin)))
    (mapcar (lambda (line)
              (let ((text (string-trim-right line)))
                (cond
                 ((string-match "\\`\\([[:alpha:]]+ [0-9]+\\).*\\([‹<] [›>]\\)" text)
                  (org-air-view--justify (concat margin (match-string 1 text))
                                         (match-string 2 text)
                                         width))
                 (t (org-air-view--pad-to (concat margin text) width)))))
            lines)))

(defun org-air-view--insert-lines (lines)
  "Insert LINES followed by newlines."
  (dolist (line lines)
    (insert line "\n")))

(defun org-air-view--normalize-buffer-lines (width)
  "Normalize every buffer line to display WIDTH."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
        (delete-region (line-beginning-position) (line-end-position))
        (insert (org-air-view--pad-to line width)))
      (forward-line 1))))

(defun org-air-view--finalize-buffer-lines (width)
  "Cap each line at WIDTH and strip trailing whitespace (D7/D6, live mode).
No line may exceed the window actually displaying the dashboard so the
rail/calendar are never pushed off-screen (D7); full-width and stacked
rows carry no trailing whitespace (D6)."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((beg (line-beginning-position))
             (end (line-end-position))
             (line (buffer-substring beg end))
             (capped (if (> (string-width line) width)
                         (truncate-string-to-width line width nil nil
                                                   (org-air-view--glyph 'more))
                       line))
             (trimmed (string-trim-right capped)))
        (unless (string= trimmed line)
          (delete-region beg end)
          (insert trimmed)))
      (forward-line 1))))

(defun org-air-view--collapse-blank-lines ()
  "Collapse two or more consecutive blank lines to a single blank line (D6).
Keep exactly one blank line of rhythm between sections and rail blocks."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\n[ \t]*\n[ \t]*\n" nil t)
      (replace-match "\n\n")
      (goto-char (match-beginning 0)))))

(defun org-air-view--beginning-of-visible ()
  "Move point to the first visible (non-whitespace) char of the line (S5a).
An unfocused frame draws a hollow-box cursor; on leading indent
whitespace it reads as tofu, so park point on the first real glyph."
  (beginning-of-line)
  (skip-chars-forward " \t" (line-end-position)))

(defun org-air-view--goto-first-item ()
  "Place point on the first actionable item row (D4 / S5a).
Lands on the first `org-air-item' (first non-empty section), then the
first section heading, then `point-min' for a truly empty board — so
n/p, RET and r work on the first keystroke instead of the banner — and on
the first VISIBLE character of that row, never the indent whitespace."
  (goto-char (or (text-property-not-all (point-min) (point-max) 'org-air-item nil)
                 (text-property-not-all (point-min) (point-max) 'org-air-section nil)
                 (point-min)))
  (org-air-view--beginning-of-visible))

(defun org-air-view--collapse-line-list (lines)
  "Collapse two or more consecutive blank LINES to a single blank line (D6)."
  (let (out (prev-blank nil))
    (dolist (l lines (nreverse out))
      (let ((blank (and (string-match-p "\\`[ \t]*\\'" l) t)))
        (unless (and blank prev-blank)
          (push l out))
        (setq prev-blank blank)))))

(defun org-air-view--pad-line-list (lines target fill-row)
  "Return LINES extended to TARGET rows by appending FILL-ROW (S6)."
  (if (>= (length lines) target)
      lines
    (append lines (make-list (- target (length lines)) fill-row))))

(defun org-air-view--two-pane-body (items width)
  "Return (BODY-LINES . FILL-ROW) for ITEMS in the two-pane layout at WIDTH.
FILL-ROW is a full-width blank row carrying the divider, so the divider
spans the full body height when the body is padded out (S6)."
  (let* ((rail-width (org-air-view--rail-width width))
         (divider (org-air-view--divider))
         (item-width (max 20 (- width rail-width (string-width divider))))
         (item-content-width (max 1 (- item-width org-air-margin)))
         ;; D5b: the rail carries NO extra left margin — its labelled rules
         ;; sit flush at rail column 0 and the single content spine
         ;; (`org-air-rail-content-inset') is the only inset, aligning the
         ;; grid/summary/filters/actions under the rule labels.
         (rail-content-width rail-width)
         (item-lines (org-air-view--indent-pane-lines
                      (org-air-view--render-lines
                       item-content-width
                       (lambda ()
                         (let ((org-air-view--pane-indented t))
                           (org-air-view--insert-item-pane items item-content-width))))
                      item-width))
         (rail-lines (mapcar
                      (lambda (line) (org-air-view--pad-to line rail-width))
                      (org-air-view--render-lines
                       rail-content-width
                       (lambda () (org-air-view--insert-rail items rail-content-width))))))
    (cons (org-air-view--compose-columns
           (list (cons item-lines item-width) (cons rail-lines rail-width))
           divider)
          (concat (make-string item-width ?\s) divider
                  (make-string rail-width ?\s)))))

(defun org-air-view--render (items tag-filter)
  "Render the dashboard for cached ITEMS with TAG-FILTER, filling the window.
Three bands (S6): a fixed header (banner + rule), a body that fills the
full `org-air-view--render-height' (two-pane keeps the divider down
every body row; stacked blank-fills), and a footer pinned to the bottom."
  (let* ((inhibit-read-only t)
         (width (org-air-view--render-width))
         (height (org-air-view--render-height))
         ;; C2/C3: capture the displaying window's live char metrics here,
         ;; in the real buffer (the panes render in temp buffers with no
         ;; window), so every pill is sized to the exact text cell at the
         ;; current font/text-scale.
         (dims (org-air-view--char-dimensions))
         (org-air-view--pill-char-w (car dims))
         (org-air-view--pill-char-h (cdr dims)))
    (erase-buffer)
    (setq org-air-view--items items
          org-air-view--items-key (list org-air-files org-air-inbox-file)
          org-air-view--tag-filter tag-filter)
    (setq org-air-view--orientation
          (if (org-air-view--two-pane-p width) 'two-pane 'stacked))
    (let* ((header (org-air-view--render-lines
                    width
                    (lambda ()
                      (org-air-view--insert-banner items)
                      (org-air-view--insert-rule)
                      (insert "\n"))))
           ;; R4: with the footer band off (default), there is no bottom
           ;; rule/legend at all — the body fills to the last usable row.
           (footer (if org-air-show-footer
                       (org-air-view--render-lines
                        width
                        (lambda ()
                          (org-air-view--insert-rule)
                          (org-air-view--insert-footer)))
                     nil))
           (fill-row "")
           (body-content
            (if (eq org-air-view--orientation 'two-pane)
                (let ((pair (org-air-view--two-pane-body items width)))
                  (setq fill-row (cdr pair))
                  (car pair))
              (org-air-view--render-lines
               width
               (lambda ()
                 (org-air-view--insert-top-rail items width)
                 (insert "\n")
                 (org-air-view--insert-rule)
                 (insert "\n")
                 (org-air-view--insert-item-pane items width)))))
           (body-content (org-air-view--collapse-line-list body-content))
           (body-target (max (length body-content)
                             (- height (length header) (length footer))))
           (body (org-air-view--pad-line-list body-content body-target fill-row)))
      (org-air-view--insert-lines (append header body footer)))
    (if (integerp org-air-view-width)
        (org-air-view--normalize-buffer-lines org-air-view-width)
      ;; D7/D6 — cap every line at the displaying window and right-trim.
      (org-air-view--finalize-buffer-lines width))
    ;; T5: drop the trailing newline so the buffer is EXACTLY the filled
    ;; line count — otherwise the final \n renders one phantom blank row
    ;; below the footer, overrunning the body height by one.
    (goto-char (point-max))
    (when (and (bolp) (> (point-max) (point-min)))
      (delete-char -1))
    (setq org-air-view--rendered-width width
          org-air-view--rendered-height height)
    (org-air-view--goto-first-item)))

(defun org-air-view--save-position ()
  "Return a token describing the cursor location for later restoration."
  (list :marker (get-text-property (point) 'org-air-marker)
        :section (get-text-property (point) 'org-air-section)
        :line (line-number-at-pos)
        :column (current-column)))

(defun org-air-view--find-property (prop value)
  "Return the first position where text PROP equals VALUE, or nil."
  (when value
    (let ((pos (point-min)) (found nil))
      (while (and (not found) pos (< pos (point-max)))
        (if (equal (get-text-property pos prop) value)
            (setq found pos)
          (setq pos (next-single-property-change pos prop nil (point-max)))))
      found)))

(defun org-air-view--restore-position (token)
  "Restore the cursor to the location described by TOKEN (D5).
Prefers the same item; if it vanished (refiled/done), lands on the
nearest surviving item in the same section, then the section heading,
falling back to the same line/column — never jumping to `point-min'
unless nothing else is available."
  (let ((marker-pos (org-air-view--find-property
                     'org-air-marker (plist-get token :marker)))
        (section-pos (org-air-view--find-property
                      'org-air-section (plist-get token :section))))
    (cond
     (marker-pos
      (goto-char marker-pos)
      (org-air-view--beginning-of-visible))
     (section-pos
      ;; Nearest surviving item at/after the saved section heading.
      (goto-char (or (text-property-not-all section-pos (point-max)
                                            'org-air-item nil)
                     section-pos))
      (org-air-view--beginning-of-visible))
     (t
      ;; Item vanished and its marker no longer matches (a re-query after
      ;; auto-refresh rebuilds markers): land on the saved line but on its
      ;; first VISIBLE char, never the indent whitespace (S5a regression).
      (goto-char (point-min))
      (forward-line (1- (or (plist-get token :line) 1)))
      (org-air-view--beginning-of-visible)))))

(defun org-air-view--render-current ()
  "Re-render the dashboard from `org-air-view--items', preserving point.
Filters, scope and the calendar month are buffer-local and survive; the
cursor is restored to the same item (or section, or line) afterwards."
  (let ((token (org-air-view--save-position)))
    (org-air-view--render (or org-air-view--items (org-air-query-items))
                          org-air-view--tag-filter)
    (org-air-view--restore-position token)))

(defun org-air-view--resize-refresh ()
  "Re-render only when the displaying window's width or height changed.
Called from the debounced window-size/-configuration hook (S6 makes the
body fill the height, so a height change must re-pad too)."
  (let ((width (org-air-view--render-width))
        (height (org-air-view--render-height)))
    (unless (and (eql width org-air-view--rendered-width)
                 (eql height org-air-view--rendered-height))
      (org-air-view--render-current))))

;;;###autoload
(defun org-air-view ()
  "Open the org-air dashboard buffer."
  (interactive)
  (let ((buffer (get-buffer-create org-air-view-buffer-name)))
    (with-current-buffer buffer
      (org-air-view-mode)
      (unless (and org-air-view--items
                   (equal org-air-view--items-key (list org-air-files org-air-inbox-file)))
        (setq org-air-view--items (org-air-query-items))))
    ;; Display the buffer first so width derivation measures the window
    ;; that actually shows the dashboard (U1), in a full-width window so
    ;; the rail/calendar are never pushed off-screen (D4), then render.
    (pop-to-buffer buffer
                   (or org-air-display-action
                       '((display-buffer-reuse-window
                          display-buffer-same-window
                          display-buffer-full-frame))))
    (with-current-buffer buffer
      (org-air-view--render org-air-view--items org-air-view--tag-filter))))

(defun org-air-refresh ()
  "Re-query files and refresh the current org-air dashboard.
Preserves the active filter and the cursor's place."
  (interactive)
  (let ((token (org-air-view--save-position))
        (filter org-air-view--tag-filter))
    (setq org-air-view--items (org-air-query-items))
    (org-air-view--render org-air-view--items filter)
    (org-air-view--restore-position token)))

(defun org-air--relevant-file-p (file)
  "Return non-nil when FILE is one of the configured org-air files."
  (and file
       (let ((truename (ignore-errors (file-truename file)))
              (candidates (delq nil (cons (and (boundp 'org-air-inbox-file)
                                               org-air-inbox-file)
                                          (and (boundp 'org-air-files)
                                               org-air-files)))))
         (and truename
              (seq-some (lambda (f)
                          (and f (equal (ignore-errors (file-truename f))
                                        truename)))
                        candidates)))))

(defun org-air-view--after-save-refresh ()
  "Refresh an open org-air dashboard after a configured file is saved.
Scoped to `org-air-files'/`org-air-inbox-file'; this covers capture,
refile, and any manual edit saved from an Org buffer (U3).  Point and
filters are preserved by `org-air-refresh'."
  (when (and buffer-file-name (org-air--relevant-file-p buffer-file-name))
    (let ((buffer (get-buffer org-air-view-buffer-name)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (derived-mode-p 'org-air-view-mode)
            (org-air-refresh)))))))

(add-hook 'after-save-hook #'org-air-view--after-save-refresh)

(defun org-air-refresh-all ()
  "Clear scope and filters, then refresh."
  (interactive)
  (setq org-air-view--tag-filter nil
        org-air-view--scope nil)
  (org-air-refresh))

(defun org-air-goto-top ()
  "Move point to the top of the pane (B4): the first actionable item."
  (interactive)
  (org-air-view--goto-first-item))

(defun org-air-goto-bottom ()
  "Move point to the bottom of the pane (B4): the last item row."
  (interactive)
  (goto-char (point-max))
  (org-air-prev-item))

(defun org-air-filter (tags)
  "Filter dashboard to TAGS, a comma-separated or list value."
  (interactive
   (let* ((tags (delete-dups (sort (seq-mapcat #'org-air-item-tags org-air-view--items)
                                   #'string<)))
          (choice (completing-read-multiple "Tags: " tags nil nil)))
     (list choice)))
  (setq org-air-view--tag-filter (unless (null tags) tags))
  (org-air-view--render-current))

(defun org-air-filter-by-tag (tag)
  "Compatibility wrapper: filter dashboard to TAG."
  (interactive (list (read-string "Tag filter (empty clears): ")))
  (setq org-air-view--tag-filter (unless (string-empty-p tag) (list tag)))
  (org-air-view--render-current))

(defun org-air-filter-toggle (tag)
  "Toggle TAG in the active filter list."
  (interactive "sTag: ")
  (let ((filters (org-air-view--filter-tags)))
    (setq org-air-view--tag-filter
          (if (member tag filters)
              (delete tag filters)
            (cons tag filters))))
  (org-air-view--render-current))

(defun org-air-filter-clear ()
  "Clear tag filters."
  (interactive)
  (setq org-air-view--tag-filter nil)
  (org-air-view--render-current))

(defun org-air-scope (scope)
  "Scope dashboard to SCOPE."
  (interactive
   (let* ((tags (delete-dups (seq-mapcat #'org-air-item-tags org-air-view--items)))
          (groups (delete-dups (delq nil (mapcar #'org-air-item-group org-air-view--items))))
          (files (delete-dups (mapcar #'org-air-item-file org-air-view--items)))
          (candidates (append '("all")
                              (mapcar (lambda (g) (concat "@" g)) groups)
                              (mapcar (lambda (tag) (concat "#" tag)) tags)
                              (mapcar (lambda (file) (concat "⌂ " (file-name-nondirectory file))) files)))
          (choice (completing-read "Scope: " candidates nil t)))
     (list choice)))
  (setq org-air-view--scope
        (cond
         ((or (null scope) (equal scope "all")) nil)
         ((string-prefix-p "#" scope) (list :tag (substring scope 1)))
         ((string-prefix-p "@" scope) (list :group (substring scope 1)))
         ((string-prefix-p "⌂ " scope)
          (let ((name (substring scope 2)))
            (list :file (seq-find (lambda (file)
                                    (equal name (file-name-nondirectory file)))
                                  (mapcar #'org-air-item-file org-air-view--items)))))
         (t nil)))
  (org-air-view--render-current))

(defun org-air-scope-clear ()
  "Clear active dashboard scope."
  (interactive)
  (setq org-air-view--scope nil)
  (org-air-view--render-current))

(defun org-air-next-line ()
  "Move point down one line, landing on its first visible char (R3, vim j)."
  (interactive)
  (forward-line 1)
  (org-air-view--beginning-of-visible))

(defun org-air-prev-line ()
  "Move point up one line, landing on its first visible char (R3, vim k)."
  (interactive)
  (forward-line -1)
  (org-air-view--beginning-of-visible))

(defun org-air-next-item ()
  "Move point to the next item row."
  (interactive)
  (let ((pos (next-single-property-change (point) 'org-air-item nil (point-max))))
    (while (and pos (not (get-text-property pos 'org-air-item)) (< pos (point-max)))
      (setq pos (next-single-property-change pos 'org-air-item nil (point-max))))
    (when pos
      (goto-char pos)
      (org-air-view--beginning-of-visible))))

(defun org-air-prev-item ()
  "Move point to the previous item row."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'org-air-item nil (point-min))))
    (while (and pos (not (get-text-property (max (point-min) (1- pos)) 'org-air-item)) (> pos (point-min)))
      (setq pos (previous-single-property-change pos 'org-air-item nil (point-min))))
    (when pos
      (goto-char (max (point-min) (1- pos)))
      (org-air-view--beginning-of-visible))))

(defun org-air-view--line-section ()
  "Return the `org-air-section' bucket anywhere on the current line, or nil.
In two-pane mode the heading sits past the indent margin (and the
composed row also carries rail text), so scan the whole line rather than
only its first column."
  ;; B1: `next-single-property-change' returns LIMIT (not nil) when no
  ;; change is found before it, so guard with `< pos eol' — the old
  ;; `<= pos eol' + `(or ... (1+ eol))' parked pos at eol forever (hang).
  (let ((pos (line-beginning-position))
        (eol (line-end-position))
        (found nil))
    (while (and (not found) (< pos eol))
      (setq found (get-text-property pos 'org-air-section))
      (setq pos (next-single-property-change pos 'org-air-section nil eol)))
    found))

(defun org-air-view--section-at-point ()
  "Return the bucket of the section containing point, or nil."
  (save-excursion
    (let ((bucket (org-air-view--line-section)))
      (while (and (not bucket) (not (bobp)))
        (forward-line -1)
        (setq bucket (org-air-view--line-section)))
      bucket)))

(defun org-air-toggle-section ()
  "Toggle expand/collapse of the section HEADER at point (T2/B1).
On a section header, toggle its full vs capped preview and KEEP POINT ON
THE HEADER so it can be re-collapsed immediately.  On any non-header line
TAB is safe — it moves to the next section header and never toggles or
hangs."
  (interactive)
  (let ((bucket (org-air-view--line-section)))
    (if (not bucket)
        (org-air-next-section)
      (setq org-air-view--expanded-sections
            (if (memq bucket org-air-view--expanded-sections)
                (delq bucket org-air-view--expanded-sections)
              (cons bucket org-air-view--expanded-sections)))
      (org-air-view--render (or org-air-view--items (org-air-query-items))
                            org-air-view--tag-filter)
      (let ((pos (org-air-view--find-property 'org-air-section bucket)))
        (when pos
          (goto-char pos)
          (org-air-view--beginning-of-visible))))))

(defun org-air-next-section ()
  "Move point to the next section heading."
  (interactive)
  (let ((pos (next-single-property-change (point) 'org-air-section nil (point-max))))
    (when pos
      (goto-char pos)
      (org-air-view--beginning-of-visible))))

(defun org-air-prev-section ()
  "Move point to the previous section heading."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'org-air-section nil (point-min))))
    (when pos
      (goto-char (max (point-min) (1- pos)))
      (org-air-view--beginning-of-visible))))

(defalias 'org-air-forward-section #'org-air-next-section)
(defalias 'org-air-back-section #'org-air-prev-section)

(defun org-air-toggle-mark ()
  "Toggle a visual mark on the item at point."
  (interactive)
  (let ((inhibit-read-only t)
        (marked (get-text-property (point) 'org-air-marked)))
    (add-text-properties (line-beginning-position) (line-end-position)
                         `(org-air-marked ,(not marked)))
    (save-excursion
      (beginning-of-line)
      (delete-char 1)
      (insert (if marked " " "•")))))

(defun org-air-set-tag ()
  "Add TAG to the item at point."
  (interactive)
  (let* ((item (or (get-text-property (point) 'org-air-item)
                   (user-error "No org-air item at point")))
         (tag (read-string "Tag: ")))
    (with-current-buffer (find-file-noselect (org-air-item-file item))
      (goto-char (org-air-item-marker item))
      (org-back-to-heading t)
      (org-toggle-tag tag 'on)
      (save-buffer)))
  (org-air-refresh))

(defun org-air-set-schedule (date)
  "Set SCHEDULED DATE on the item at point."
  (interactive "sSchedule (empty clears): ")
  (let ((item (or (get-text-property (point) 'org-air-item)
                  (user-error "No org-air item at point"))))
    (with-current-buffer (find-file-noselect (org-air-item-file item))
      (goto-char (org-air-item-marker item))
      (org-back-to-heading t)
      (org-schedule nil (unless (string-empty-p date) date))
      (save-buffer)))
  (org-air-refresh))

;;;; Inbox triage — inline dispositions + process-inbox (org-air-triage.org)

(defvar org-air-view--triage-source-buffer nil
  "Source buffer of the most recent triage disposition (for `u' undo).")

(defun org-air-view--item-at-point ()
  "Return the org-air item at point, or signal a `user-error'."
  (or (get-text-property (point) 'org-air-item)
      (user-error "No org-air item at point")))

(defmacro org-air-view--at-item-source (item &rest body)
  "At ITEM's heading in its source buffer run BODY, save, and remember it."
  (declare (indent 1) (debug t))
  (let ((buf (make-symbol "buf")) (it (make-symbol "it")))
    `(let* ((,it ,item)
            (,buf (find-file-noselect (org-air-item-file ,it))))
       (with-current-buffer ,buf
         (save-excursion
           (goto-char (org-air-item-marker ,it))
           (org-back-to-heading t)
           ,@body)
         (save-buffer))
       (setq org-air-view--triage-source-buffer ,buf)
       ,buf)))

(defun org-air-view--next-dow (target)
  "Return YYYY-MM-DD of the next day-of-week TARGET (0=Sun..6=Sat)."
  (let* ((now (current-time))
         (today (string-to-number (format-time-string "%w" now)))
         (delta (mod (- target today) 7))
         (delta (if (= delta 0) 7 delta)))
    (format-time-string "%Y-%m-%d" (time-add now (* delta 86400)))))

(defun org-air-view--quick-date-string (key)
  "Return a date for quick-date KEY: a YYYY-MM-DD string, `clear', or nil."
  (let ((now (current-time)))
    (pcase key
      (?t (format-time-string "%Y-%m-%d" now))
      ((or ?m ?+) (format-time-string "%Y-%m-%d" (time-add now 86400)))
      (?w (org-air-view--next-dow 1))
      (?e (org-air-view--next-dow 6))
      (?. (org-read-date nil nil))
      (?0 'clear)
      (_ nil))))

(defun org-air-view--read-quick-date (verb)
  "Prompt VERB and return a date string, or `clear' to remove the date."
  (let* ((key (read-char-exclusive
               (format (concat "%s: [t]oday [m]orrow [w]eek [e]weekend "
                               "[+]1d [.]pick [0]clear ")
                       verb)))
         (spec (org-air-view--quick-date-string key)))
    (or spec (user-error "Unknown quick-date key: %c" key))))

(defun org-air-view--apply-date (kind date)
  "Set KIND (`scheduled' or `deadline') to DATE on the item at point.
DATE is a date string or `clear'.  Refinement only — stays in Inbox."
  (let* ((item (org-air-view--item-at-point))
         (clearp (eq date 'clear))
         (setter (if (eq kind 'deadline) #'org-deadline #'org-schedule)))
    (org-air-view--at-item-source item
      (if clearp (funcall setter '(4)) (funcall setter nil date)))
    (org-air-refresh)
    (message "%s \"%s\"%s"
             (if (eq kind 'deadline) "Deadline" "Scheduled")
             (org-air-item-title item)
             (if clearp " cleared" (format " → %s" date)))))

;;;###autoload
(defun org-air-item-schedule (&optional date)
  "Set SCHEDULED on the item at point via the quick-date sub-prompt.
DATE may be supplied non-interactively.  A refinement: the item gains
Upcoming membership and a calendar dot but stays in Inbox until filed."
  (interactive)
  (org-air-view--apply-date 'scheduled
                            (or date (org-air-view--read-quick-date "Schedule"))))

;;;###autoload
(defun org-air-item-deadline (&optional date)
  "Set DEADLINE on the item at point via the quick-date sub-prompt.
DATE may be supplied non-interactively.  A refinement: stays in Inbox."
  (interactive)
  (org-air-view--apply-date 'deadline
                            (or date (org-air-view--read-quick-date "Deadline"))))

;;;###autoload
(defun org-air-item-file-group ()
  "Fast-refile the item at point under a category/group (graduates it)."
  (interactive)
  (call-interactively #'org-air-refile-item))

;;;###autoload
(defun org-air-item-cycle-todo ()
  "Cycle/promote the TODO state of the item at point (a refinement).
Named -cycle-todo to avoid colliding with the `org-air-item-todo' struct
accessor; the triage spec's `T' key maps here."
  (interactive)
  (let ((item (org-air-view--item-at-point)))
    (org-air-view--at-item-source item (org-todo))
    (org-air-refresh)))

;;;###autoload
(defun org-air-item-archive ()
  "Archive the item at point's subtree (graduates it out of Inbox)."
  (interactive)
  (let ((item (org-air-view--item-at-point)))
    (org-air-view--at-item-source item (org-archive-subtree))
    (org-air-refresh)
    (message "Archived \"%s\"" (org-air-item-title item))))

;;;###autoload
(defun org-air-item-done ()
  "Mark the item at point DONE (graduates it out of Inbox)."
  (interactive)
  (let ((item (org-air-view--item-at-point)))
    (org-air-view--at-item-source item (org-todo 'done))
    (org-air-refresh)
    (message "Marked DONE \"%s\"" (org-air-item-title item))))

;;;###autoload
(defun org-air-item-kill ()
  "Delete the item at point's subtree, with confirmation (graduates it)."
  (interactive)
  (let ((item (org-air-view--item-at-point)))
    (when (yes-or-no-p (format "Delete \"%s\"? " (org-air-item-title item)))
      (org-air-view--at-item-source item (org-cut-subtree))
      (org-air-refresh)
      (message "Deleted \"%s\"" (org-air-item-title item)))))

(defun org-air-triage-undo ()
  "Undo the last triage disposition in its source buffer."
  (interactive)
  (if (buffer-live-p org-air-view--triage-source-buffer)
      (progn
        (with-current-buffer org-air-view--triage-source-buffer
          (undo)
          (save-buffer))
        (org-air-refresh)
        (message "Undid last disposition"))
    (user-error "No triage disposition to undo")))

(defun org-air-view--goto-first-inbox-item ()
  "Move point to the first Inbox item row, if any."
  (goto-char (point-min))
  (let (found)
    (while (and (not found) (not (eobp)))
      (if (and (eq (get-text-property (line-beginning-position) 'org-air-section)
                   nil)
               (get-text-property (line-beginning-position) 'org-air-item)
               (memq 'inbox (org-air-classify-item
                             (get-text-property (line-beginning-position)
                                                'org-air-item))))
          (setq found t)
        (forward-line 1)))
    (when found (org-air-view--beginning-of-visible))
    found))

;;;###autoload
(defun org-air-process-inbox ()
  "Walk the Inbox one item at a time with single-key dispositions.
A guided loop (mu4e/dired style) that counts down to Inbox zero: filing
dispositions (refile/file/archive/done/kill) shrink the Inbox; schedule/
deadline/tag/todo are refinements that keep the item in Inbox.  Filters
and scope are preserved; `q'/`RET' exits with partial progress kept."
  (interactive)
  (unless (derived-mode-p 'org-air-view-mode)
    (org-air-view))
  (let ((quit nil))
    (while (not quit)
      (org-air-refresh)
      (let* ((inbox (org-air-view--items-for-bucket 'inbox org-air-view--items))
             (n (length inbox)))
        (if (zerop n)
            (progn (message "Inbox zero — nice work.") (setq quit t))
          (org-air-view--goto-first-inbox-item)
          (let ((key (read-char-exclusive
                      (format (concat "Inbox %d ┆ [s]chedule [d]eadline [r]efile "
                                      "[f]ile [t]ag [T]odo [a]rchive [D]one [k]ill "
                                      "┆ [SPC]skip [p]rev [u]ndo [g]refresh [q]uit ")
                              n))))
            (pcase key
              (?s (call-interactively #'org-air-item-schedule))
              (?d (call-interactively #'org-air-item-deadline))
              (?r (call-interactively #'org-air-refile-item))
              (?f (call-interactively #'org-air-item-file-group))
              (?t (call-interactively #'org-air-set-tag))
              (?T (org-air-item-cycle-todo))
              (?a (org-air-item-archive))
              (?D (org-air-item-done))
              (?k (org-air-item-kill))
              (?u (org-air-triage-undo))
              (?g nil)
              ((or ?\s ?n) (org-air-next-item))
              (?p (org-air-prev-item))
              ((or ?q ?\r ?\e) (setq quit t))
              (_ (message "Unknown key: %c" key)))))))))

(defun org-air-view--calendar-month-time (&optional base offset)
  "Return month time from BASE shifted by OFFSET months."
  (let* ((decoded (decode-time (or base (current-time))))
         (month (+ (decoded-time-month decoded) (or offset 0)))
         (year (decoded-time-year decoded)))
    (while (< month 1)
      (setq month (+ month 12)
            year (1- year)))
    (while (> month 12)
      (setq month (- month 12)
            year (1+ year)))
    (encode-time 0 0 0 1 month year)))

(defun org-air-calendar-prev ()
  "Page to the previous month, or the previous day in the day view (R6)."
  (interactive)
  (if org-air-view--day
      (org-air-view-day (time-subtract org-air-view--day (days-to-time 1)))
    (setq org-air-view--cal-month
          (org-air-view--calendar-month-time org-air-view--cal-month -1))
    (org-air-view--render-current)))

(defun org-air-calendar-next ()
  "Page to the next month, or the next day in the day view (R6)."
  (interactive)
  (if org-air-view--day
      (org-air-view-day (time-add org-air-view--day (days-to-time 1)))
    (setq org-air-view--cal-month
          (org-air-view--calendar-month-time org-air-view--cal-month 1))
    (org-air-view--render-current)))

;;;###autoload
(defun org-air-view-day (&optional date)
  "Focus the single-day view (R6) on DATE, the calendar day at point, or today.
The item pane becomes that day's items grouped Deadline / Scheduled /
Logged.  `q' or `g' returns to the full board; `<'/`>' move to the
adjacent day; the rail calendar re-centres on the focused month."
  (interactive)
  (let ((day (or date
                 (get-text-property (point) 'org-air-day)
                 org-air-view--day
                 (current-time))))
    (setq org-air-view--day day
          org-air-view--cal-month day)
    (org-air-view--render-current)))

(defun org-air-view-board ()
  "Leave the single-day view and return to the full board (R6)."
  (interactive)
  (when org-air-view--day
    (setq org-air-view--day nil)
    (org-air-view--render-current)))

(defun org-air-quit ()
  "Return to the full board from the single-day view, else quit the window."
  (interactive)
  (if org-air-view--day
      (org-air-view-board)
    (quit-window)))

(defun org-air-calendar-today ()
  "Recenter the persistent org-air calendar on today."
  (interactive)
  (setq org-air-view--cal-month nil)
  (org-air-view--render-current))

(defun org-air-peek-item ()
  "Preview the source item in another window while keeping dashboard focus."
  (interactive)
  (let ((dashboard (selected-window)))
    (org-air-visit-item nil 'other-window)
    (select-window dashboard)))

(defun org-air-visit-item-stay ()
  "Visit the source item without moving focus away from dashboard."
  (interactive)
  (org-air-peek-item))

(defun org-air-help ()
  "Show org-air key bindings."
  (interactive)
  (message "org-air: n/p items, TAB sections, RET visit, c capture, r refile, / filter, \\ clear, s scope, g refresh, q quit"))

;;;###autoload
(defun org-air-visit-item (&optional item display)
  "Visit ITEM's original Org heading.
When ITEM is nil, use the item at point in an org-air dashboard.  DISPLAY
controls window choice and defaults to `org-air-visit-display'."
  (interactive)
  (let ((item (or item (get-text-property (point) 'org-air-item))))
    (unless item
      (user-error "No org-air item at point"))
    (let* ((marker (org-air-item-marker item))
           (buffer (or (marker-buffer marker)
                       (find-file-noselect (org-air-item-file item))))
           (display (or display org-air-visit-display))
           (dash-window (get-buffer-window
                         (get-buffer org-air-view-buffer-name) t))
           ;; T4/B2: capture the window configuration and the originating
           ;; item's SOURCE marker (stable across a dashboard re-render — a
           ;; dashboard-buffer position marker collapses to point-min when
           ;; the buffer is re-rendered).  `org-air-return' re-locates the
           ;; row by this marker.
           (config (current-window-configuration))
           (origin (org-air-item-marker item)))
      (pcase display
        ('other-window (switch-to-buffer-other-window buffer))
        ('frame (switch-to-buffer-other-frame buffer))
        ('side (pop-to-buffer
                buffer '((display-buffer-in-side-window)
                         (side . right) (window-width . 0.5)
                         (dedicated . t))))
        ('same (if (window-live-p dash-window)
                   (progn (select-window dash-window)
                          (switch-to-buffer buffer))
                 (switch-to-buffer buffer)))
        (_ (switch-to-buffer buffer)))
      (goto-char marker)
      (funcall (if (fboundp 'org-fold-show-context)
                   #'org-fold-show-context
                 (intern "org-show-context")))
      (recenter)
      ;; T4: arm the captured return contract in the visited buffer.
      (org-air-view--enable-return config origin)
      (when (fboundp 'pulse-momentary-highlight-one-line)
        (pulse-momentary-highlight-one-line (point))))))

(defvar org-air-return-mode-map (make-sparse-keymap)
  "Keymap for `org-air-return-mode'.")

(defvar-local org-air-view--visit-config nil
  "Window configuration captured when this buffer was visited from org-air.")
(defvar-local org-air-view--visit-origin nil
  "Marker on the originating dashboard item row, for `org-air-return'.")

(define-minor-mode org-air-return-mode
  "Buffer-local mode in an item buffer visited from the org-air dashboard.
Binds `org-air-return-key' to `org-air-return' so the captured window
configuration is restored with one key (T4)."
  :lighter " ↳org-air"
  :keymap org-air-return-mode-map)

(defun org-air-view--enable-return (config origin)
  "Enable `org-air-return-mode', recording window CONFIG and ORIGIN (T4)."
  (setq org-air-view--visit-config config
        org-air-view--visit-origin origin)
  (when (and (stringp org-air-return-key)
             (not (string-empty-p org-air-return-key)))
    (define-key org-air-return-mode-map (kbd org-air-return-key)
                #'org-air-return))
  (org-air-return-mode 1))

;;;###autoload
(defun org-air-return ()
  "Restore the window configuration captured when this item was visited (T4).
Lands point back on the originating dashboard item row.  The user's Org
buffer is never killed and no split is left broken."
  (interactive)
  (let ((config org-air-view--visit-config)
        (origin org-air-view--visit-origin))
    (when (window-configuration-p config)
      (set-window-configuration config))
    (let* ((dashboard (get-buffer org-air-view-buffer-name))
           (win (and (buffer-live-p dashboard)
                     (get-buffer-window dashboard t))))
      (cond
       ((window-live-p win)
        (select-window win)
        ;; B2: re-locate the originating row by its (stable) source marker
        ;; — the dashboard may have re-rendered while the item was visited.
        (when (markerp origin)
          (let ((pos (org-air-view--find-property 'org-air-marker origin)))
            (when pos
              (goto-char pos)
              (org-air-view--beginning-of-visible)))))
       (t (org-air-return-to-dashboard))))))

;;;###autoload
(defun org-air-return-to-dashboard ()
  "Return to the org-air dashboard window (T4 fallback).
If the dashboard is on screen, select its window; otherwise show it in
the current window.  Used when no captured configuration is available."
  (interactive)
  (let ((dashboard (get-buffer org-air-view-buffer-name)))
    (unless (buffer-live-p dashboard)
      (user-error "No org-air dashboard to return to"))
    (let ((win (get-buffer-window dashboard t)))
      (if (window-live-p win)
          (select-window win)
        (switch-to-buffer dashboard)))))

(provide 'org-air-view)

;; Local Variables:
;; package-lint-main-file: "org-air.el"
;; End:
;;; org-air-view.el ends here
