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
(declare-function org-air-doc-file "org-air-project")
(declare-function org-air-doc-name "org-air-project")
(declare-function org-air-doc-state "org-air-project")
(declare-function svg-create "svg")
(declare-function svg-rectangle "svg")
(declare-function svg-polygon "svg")
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

(defcustom org-air-pill-fill-alpha 0.08
  "Per-chip fill opacity for the rounded svg pill (D-P1.LOOK / R13 D-P1.B).
Round-13 raises the default 0 → 0.08: a *very* light tint of the label hue
behind the label, so a capsule reads at a glance without shouting.
Combined with the 0.85 border this makes the pill unmistakable yet calm.
Colour lives in the LABEL; the fill is just a faint wash of the same hue."
  :type 'number
  :group 'org-air)

(defcustom org-air-pill-border nil
  "Colour of the single muted pill border (D-P1.LOOK).
A colour string, or nil to derive a quiet neutral from the
`org-air-face-faded' foreground.  The same restrained neutral is used for
every chip — the per-tag hue lives in the LABEL only, never the border."
  :type '(choice (const :tag "Auto (derive from faded)" nil) string)
  :group 'org-air)

(defcustom org-air-pill-border-opacity 0.7
  "Stroke opacity for the svg pill border (R13 D-P1.B; R18 D-P5.4 calmer).
R18 D-P5.4 nudges the default 0.85 → 0.7 for a calmer nano-style hairline,
paired with the low `org-air-pill-fill-alpha' 0.08: the capsule still
reads clearly but no longer shouts.  Round-13's 0.85 sat a touch loud;
round-12's 0.5 was too faint.  Display-only (an svg attribute) — no byte
change; the round-18 D-P1a image cache invalidates once on the new
style-sig."
  :type 'number
  :group 'org-air)

(defcustom org-air-date-pill-align 'center
  "How the date label sits inside its uniform-width pill capsule (D-P1).
`center' (default) centres the label in the `meta-date-w' box; `right'
hugs the label to the right of the box (a right-aligned date column).  The
underlying TTY/byte text stays left-justified either way."
  :type '(choice (const center) (const right))
  :group 'org-air)

(defcustom org-air-line-spacing 0
  "Buffer-local `line-spacing' for the org-air board (D-P3, default 0).
Default 0 packs rows tight (the round-8 S8 behaviour) so the `│' divider
glyph is drawn per cell with NO gap below the row — a continuous,
portable (TTY + every GUI) vertical rule.  Round-11 set this to 0.15 to
calm the stacked capsules, but that opens a pixel gap below every row
that the per-cell divider glyph does not paint into, so the divider read
as dashed/broken (D-P3 symptom).  The capsule breathing now lives
INSIDE each pill via `org-air-pill-vinset', so the divider stays solid.
nil leaves `line-spacing' at the frame default; a non-zero value
re-introduces inter-row spacing (the divider then needs
`org-air-divider-style' = `svg to stay solid)."
  :type '(choice (const :tag "Frame default" nil) number)
  :group 'org-air)

(defcustom org-air-modeline-style 'default
  "Mode-line style for the org-air board / project / pane buffers (R23-2).
org-air already shows status in the in-buffer banner and keeps the
header-line nil (S1).  `default' (the default since R23-2) leaves the
mode-line UNTOUCHED — your own normal Emacs mode-line shows on every
org-air surface (board / project / rail / pane).  `calm' is the opt-in
minimal, faded nano-style `mode-line-format' — a quiet counts · filter ·
source line in `org-air-face-modeline'.  The mode-line is NOT part of the
buffer-text fixtures, so this is byte-invisible (and either way it is a
single line, so the body-height derivation is unaffected)."
  :type '(choice (const :tag "Emacs default (your own)" default)
                 (const :tag "Calm nano-style" calm))
  :group 'org-air)

(defcustom org-air-pill-vinset 1
  "Vertical inset in device pixels applied INSIDE each svg pill (R13 D-P1.B).
Each pill capsule is drawn this many pixels shorter than the full line
height at top AND bottom (box height = char-px-h - 2*vinset, vertically
centred), a hair of internal margin.  Round-13 lowers the default 2 → 1:
the capsule is essentially full height again (round-12's 2px shrank it and
made it read faint).  The divider is now kept solid by the D-P1.A image
line-height clamp (`org-air-view--svg-line-image'), NOT by shrinking the
pill, so the vinset is purely cosmetic margin.  TTY is unaffected."
  :type 'integer
  :group 'org-air)

(defcustom org-air-divider-style 'glyph
  "How the two-pane vertical divider renders (D-P3, secondary opt-in).
`glyph (default) draws the `│' box-drawing glyph per cell — continuous
at `org-air-line-spacing' 0, portable to TTY and every GUI, zero-cost.
`svg draws each divider cell as an svg vertical bar sized to the line
height PLUS the `line-spacing' gap so consecutive cells abut into a solid
line even when `org-air-line-spacing' is non-zero — for users who
re-enable inter-row spacing.  The shipped default is `glyph + spacing 0."
  :type '(choice (const glyph) (const svg))
  :group 'org-air)

(defvar org-air-view--pill-char-w nil
  "Device-pixel width of one text column for the current render (C2/C3).
Bound during `org-air-view--render' from the displaying window's actual
font metrics (text-scale aware) so svg pills are sized to the exact cell.")

(defvar org-air-view--pill-char-h nil
  "Device-pixel height of one text line for the current render (C2/C3).")

(defvar org-air-view--pill-style-sig nil
  "Snapshot of the pill-geometry defcustoms for the current render (R18 D-P1a).
Bound once in `org-air-view--render' alongside the char metrics so every
pane shares ONE style signature; folded into the svg image cache key so a
change to any pill-geometry defcustom yields fresh keys (auto-invalidation)
without advice or an epoch counter.")

(defvar org-air-view--svg-image-cache (make-hash-table :test 'equal :size 512)
  "Global memo of pixel-identical svg overlay images keyed on their inputs.
\(R18 D-P1a).  Shared across boards/panes (panes render in temp buffers, so
a buffer-local table would not persist, and the key fully captures the
environment: text + resolved colours + char-metrics + style signature).")

(defun org-air-view--svg-image-cached (key thunk)
  "Return the cached svg image for KEY, else (funcall THUNK) and cache it.
\(R18 D-P1a).  KEY must capture EVERYTHING that changes the pixels (text /
resolved colours / char-metrics / style), so a font/theme/width/text-scale
change re-keys to a fresh image automatically.  A soft cap keeps memory
bounded under font/theme churn."
  (let ((hit (gethash key org-air-view--svg-image-cache 'miss)))
    (if (eq hit 'miss)
        (progn
          (when (> (hash-table-count org-air-view--svg-image-cache) 4000)
            (clrhash org-air-view--svg-image-cache))
          (puthash key (funcall thunk) org-air-view--svg-image-cache))
      hit)))

(defun org-air-view--svg-image-cache-clear (&rest _)
  "Drop every cached svg overlay image (R18 D-P1a).
Belt-and-braces theme invalidation: although the resolved colour strings
are already in the cache key, clearing on a theme switch stops the table
accumulating dead palettes.  Safe to call from `enable-theme-functions'."
  (clrhash org-air-view--svg-image-cache))

(when (boundp 'enable-theme-functions)
  (add-hook 'enable-theme-functions #'org-air-view--svg-image-cache-clear))
(when (boundp 'disable-theme-functions)
  (add-hook 'disable-theme-functions #'org-air-view--svg-image-cache-clear))

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

(defvar org-air-view--meta-todo-w nil
  "Computed width of the reserved TODO-keyword cell for the current render.
The widest TODO keyword over ALL rendered board items (board-wide, so
every section shares one left edge).  0 when no rendered item carries a
keyword (no board has keywords -> no wasted column).  Set by
`org-air-view--compute-meta-widths' (R15 D-P1).")

(defvar org-air-view--meta-date-repeat 0
  "Extra date-column columns reserved for the R14 D-P2 repeat marker.
2 when ANY rendered item carries an Org repeater on its effective date (so
the marker `␣↻' fits without shoving the tags column on repeating rows),
else 0.  Set by `org-air-view--compute-meta-widths'.")

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

(defcustom org-air-rail-min-width 90
  "Minimum displaying-window width that still shows the context rail (R13 D-P3).
Below this width the dashboard renders BOARD-ONLY — no rail, no calendar,
no inspector — and the item pane uses the FULL window width.  At or above
it the usual `org-air-view--two-pane-p' decision applies (two-pane vs
stacked).  Opening a file in a split flips to board-only automatically via
the round-9 C1 resize re-render, and back when the window is widened."
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

(defcustom org-air-rail-width-hysteresis 2
  "Column dead-band around `org-air-rail-min-width' for a popped-out rail.
When the rail is popped into its side window the board renders board-only
width, sitting near `org-air-rail-min-width'; a 1-col hscroll/redisplay
wobble there must not flip board-only <-> side-window (each flip is a real
dimension change that drives a motion-time re-render).  Once `side-window',
stay side-window until the board width drops more than this many columns
below the threshold."
  :type 'integer
  :group 'org-air)

(defcustom org-air-rail-style 'inline
  "INITIAL popout state of the context rail when the board opens (R16 D-P1).
This is no longer a forceful render mode; it only seeds the per-board
runtime flag `org-air-view--rail-popped-out' on first render.
With `inline' (the default) the board opens with the inline two-pane rail
composed into the SAME `*org-air*' buffer beside the board (the in-buffer
`\=│' divider cell).  With `side-window' the board opens with the rail
ALREADY popped out into a dedicated `*org-air-rail*' right side window (a
power-user opt-in); the board then renders board-only style at full window
width with no in-buffer rail or divider.
Either way the user owns the side window: `org-air-rail-toggle' (`|') pops
it in/out, native window commands are always respected (closing the side
window restores the inline rail; org-air never re-creates it behind your
back), and the responsive board-only path (below `org-air-rail-min-width')
still wins (the window is deleted while narrow)."
  :type '(choice (const :tag "Inline (single buffer)" inline)
                 (const :tag "Side window (popped out initially)" side-window))
  :group 'org-air)

(defcustom org-air-rail-placement
  '((board . inline) (project . side-window))
  "Default context-rail placement per view (R26-5).
Consulted ONCE per buffer: when a view first renders with the popped flag
still `unset', it seeds t (side-window) or nil (inline).  Thereafter the
`|' toggle and the R25-6 reconciler own the flag.  `org-air-rail-style'
set to `side-window' still forces the BOARD entry (back-compat).  Batch
\(`noninteractive') renders never consult this alist — they normalise the
sentinel to nil exactly as before, keeping every byte golden untouched."
  :type '(alist :key-type (choice (const board) (const project))
                :value-type (choice (const inline) (const side-window)))
  :group 'org-air)

(defcustom org-air-divider-pixels 3
  "Pixel width of the `side-window' rail divider on GUI frames (R15 D-P2).
Used as `window-divider-default-right-width' (and the right divider width)
when `org-air-rail-style' is `side-window' on a graphical frame.  Ignored
for `inline' and on TTY (where the inter-window `vertical-border' is a
single continuous column by construction)."
  :type 'integer
  :group 'org-air)

(defcustom org-air-rail-window-width nil
  "Column width of the `side-window' rail window, or nil to derive it (R15 D-P2).
When nil the rail window width is derived from the existing rail-width tier
\(`org-air-rail-width-narrow'/`-width'/`-wide', see
`org-air-view--rail-tier') so the side window matches the inline rail's
tiers.  When an integer it is
used verbatim as the side window's column width.  Only consulted when
`org-air-rail-style' is `side-window'."
  :type '(choice (const :tag "Derive from rail-width tier" nil) integer)
  :group 'org-air)

(defcustom org-air-rail-side 'right
  "Which side the `side-window' rail occupies (R15 D-P2).
The spec targets `right'; `left' is provided for future-proofing.  Only
consulted when `org-air-rail-style' is `side-window'."
  :type '(choice (const :tag "Right" right) (const :tag "Left" left))
  :group 'org-air)

(defcustom org-air-rail-focus-on-popout nil
  "When non-nil, `org-air-rail-toggle' selects the rail side window after popout.
The default nil keeps point on the board (the \"point lives in the board\"
invariant); the rail is still `other-window'-reachable for reading (R16
D-P1)."
  :type 'boolean
  :group 'org-air)

(defcustom org-air-rail-keep-buffer t
  "When non-nil the `*org-air-rail*' buffer survives a pop-in (R16 D-P1).
Keeping the buffer makes a subsequent pop-out cheap; nil kills it on
pop-in / reconcile."
  :type 'boolean
  :group 'org-air)

(defcustom org-air-origin-min 12
  "Floor width (columns) the origin cell keeps under the title-min budget (R17).
When the width-aware fit pass in `org-air-view--compute-meta-widths'
shrinks the origin column to fund `org-air-title-min-width', the origin --
the item's identity / RET target -- never drops below this many columns,
so it can never vanish entirely.  Round-17 INVERTS the original D2
priority (which shrank the title before the origin): the title is now the
protected primary identity, the origin yields first (down to this floor),
then tags."
  :type 'integer
  :group 'org-air)

(defcustom org-air-origin-max-width 26
  "Hard cap (display columns) for the origin cell in a board item row (R17).
The cell is the `▤' glyph + its space + the file/title text; a longer
origin truncates the TEXT with the ellipsis glyph (the glyph cell and its
box-fit svg overlay are untouched).  This BOUNDS the V6 right cluster so a
long Denote slug can never starve the flex title.  Range guidance 24-28;
26 keeps a real de-slugged Denote title legible while the cluster stays
bounded at every tier."
  :type 'integer
  :group 'org-air)

(define-obsolete-variable-alias 'org-air-title-min
  'org-air-title-min-width "org-air 0.5")

(defcustom org-air-title-min-width 24
  "Guaranteed minimum display width for the flex item title (R17).
The origin (then tags) shrink so the title keeps at least this many
columns before the right cluster yields; only when the line itself is too
narrow to honour it (e.g. the board-only tier) does the title fall below.
The title is the row's primary identity, so it wins the budget."
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

(defcustom org-air-priority-show '(?A ?B ?C ?D ?E)
  "Priority cookies shown in item rows.
R22-1: show A..E by default (the user dogfooded `#A'-`#E').  The fixed 2-col
slot still renders two blanks for a row with no shown priority, so titles
stay V6-aligned."
  :type '(repeat character)
  :group 'org-air)

(defcustom org-air-priority-colors
  '((?A . ("#D32F2F" . "#BF616A"))   ; red       (hot)
    (?B . ("#E0631E" . "#D08770"))   ; orange
    (?C . ("#689F38" . "#A3BE8C"))   ; yellow-green
    (?D . ("#0097A7" . "#88C0D0"))   ; teal/cyan  (cool)
    (?E . ("#5C6BC0" . "#7E8CC0")))  ; indigo     (coolest)
  "Alist mapping a priority CHAR to its (LIGHT . DARK) badge colour (D-P4).
Lower priority is cooler/quieter, a hot->cool ramp: A = red (hot), B =
orange, C = yellow-green, D = teal/cyan, E = indigo (coolest) (R22-1).
Resolved against the frame background like the accent palette by
`org-air-view--priority-color'.  Themable; reconciled with
`org-air-face-priority-a/-b/-c/-d/-e' so the svg badge and the TTY text
fallback agree."
  :type '(alist :key-type character
                :value-type (cons string string))
  :group 'org-air)

(defcustom org-air-priority-style 'square
  "How the priority cookie renders (R13 D-P2, parallel to `org-air-tag-style').
`square (the R13 default) draws a tiny solid filled colour square — red A
/ orange B / cooler C, NO letter, NO outline — in a FIXED 2-column slot on
EVERY item row (blank slot when the row has no shown priority), so titles
stay V6-aligned.  `badge keeps the round-12 `[#A]' svg capsule; `text is
always the plain coloured cookie.  The byte gate sees the slot text (`■ '
for `square, `[#A]' for badge/text); the filled square / capsule is a GUI
display overlay."
  :type '(choice (const square) (const badge) (const text))
  :group 'org-air)

(defcustom org-air-sort-key 'date
  "Default within-bucket sort key for the board (R22-3).
One of `date' / `priority' / `title' / `recency'.  Seeds the per-buffer
`org-air-view--sort-key'; `o' cycles it.  The default `date' reproduces the
historical within-bucket order exactly, so the board byte goldens are
byte-identical out of the box."
  :type '(choice (const date) (const priority) (const title) (const recency))
  :group 'org-air)

(defcustom org-air-sort-direction 'ascending
  "Default within-bucket sort direction for the board (R22-3).
Seeds the per-buffer `org-air-view--sort-direction'; `O' toggles it."
  :type '(choice (const ascending) (const descending))
  :group 'org-air)

(defcustom org-air-keyword-style 'badge
  "How a TODO keyword / Air state renders (R21-4).
`badge' overlays the reserved keyword/state cell with a small coloured
svg chip (GUI); `text' keeps the plain coloured keyword text.  The svg is
a display overlay over the UNCHANGED cell text, so the byte/TTY layer
always shows the keyword/token text either way (`NEXT', `[R]')."
  :type '(choice (const badge) (const text))
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

(defcustom org-air-filter-match 'all
  "How multiple tag filters combine: `all' (AND) or `any' (OR).
R18 D-P2: the default is `all' so adding a second filter term NARROWS,
as both tags must match; `M-/' (`org-air-filter-toggle-match') flips it
to `any', where either tag matches.  The predicate honours both modes."
  :type '(choice (const all) (const any))
  :group 'org-air)

(defvar-local org-air-view--items nil)
(defvar-local org-air-view--items-key nil)
(defvar-local org-air-view--classify-cache nil
  "Per-board memo mapping an `org-air-item' to its cached bucket list.
An `eq' hash (R18 D-P1c).  Auto-invalidates because a re-query yields new
item objects; explicitly cleared on day-rollover and refresh.")
(defvar-local org-air-view--classify-cache-day nil
  "`time-to-days' the classify cache was built for; a rollover clears it.")
(defvar org-air-view--render-partition nil
  "Per-render compute-once memo (ITEMS VISIBLE . TABLE) (R20-6).
VISIBLE is the scope+filter visible subset of ITEMS computed ONCE; TABLE is
an `eq' hash mapping each classify bucket to its visible members (source
order).  Bound for the render's dynamic extent in `org-air-view--render'
and rebound into the `org-air-view--render-lines' temp buffers, so every
consumer (`--visible-items', `--items-for-bucket', the section/summary/
badge/calendar counts, `--compute-meta-widths') reads ONE classify pass
instead of re-deriving the visible set + per-bucket filtering O(N) times.
nil outside a render -> consumers fall back to computing fresh.  The CAR is
the ITEMS object the memo was built for; consumers use it only when the
passed ITEMS is `eq' to it, so a stray off-render call is always correct.")
(defvar org-air-view--render-displayed nil
  "Per-render memo (ITEMS . TABLE) of `org-air-view--displayed-items-for-bucket'.
TABLE is an `eq' hash bucket->the date-sorted, section-capped rows a section
ACTUALLY renders.  Bound for the render extent (R20-6) so the section pass
and the meta-width pass SHARE one sort+take per bucket instead of each
paying it; nil outside a render.  CAR is the ITEMS the memo was built for.")
(defvar-local org-air-view--loading nil
  "Non-nil during the brief synchronous fast-paint window of a cold load (R20-1).
`org-air-view' sets it around the `redisplay'-then-query body so a stray
data-dependent command in that window soft-errors via
`org-air-view--loading-guard'; it is always cleared by the body's
`unwind-protect', so the board can never wedge in a loading state.
R26-8: on the interactive COLD (no cache) path it stays set until the
chunked refresh's single swap, guarding data-dependent verbs while input
stays live over the skeleton.")

(defcustom org-air-cache-file
  (expand-file-name "org-air/board-cache.eld"
                    (or (getenv "XDG_CACHE_HOME") "~/.cache"))
  "Persisted board scan cache; nil disables persistence entirely (R26-8).
Written atomically after every completed scan; read on an interactive
cold start so the last-known board paints instantly (a stale cache shows
the `stale · refreshing…' header marker while the chunked rescan runs).
Never read or written under `noninteractive', so the byte gate and every
golden are byte-identical to the synchronous path."
  :type '(choice (const :tag "Disabled" nil) file)
  :group 'org-air)

(defcustom org-air-refresh-files-per-slice 3
  "Files scanned per idle-timer refresh slice (R26-8).
Measured ≈21ms/file on a real Air tree, so the default 3 keeps each
slice ≈60-70ms — under perception — while the board stays interactive."
  :type 'integer
  :group 'org-air)

(defvar-local org-air-view--refresh-token 0
  "Monotonic refresh token (R26-8).
Every scheduled slice carries the token current at schedule time; a
callback whose token is stale self-cancels, so `g' mid-refresh (which
bumps the token) makes every pending slice a no-op — timers can never
interleave two refreshes or touch a superseded scan.")
(defvar-local org-air-view--refresh-state nil
  "R26-8 refresh machine state: nil (fresh/idle), `refreshing', `failed'.
Drives the header count-slot marker (`stale · refreshing…' / `stale ·
refresh failed (g retries)'); only ever non-nil when the machine is
driven (interactively, or by an ERT calling the slice runner), so batch
renders never show it.")
(defvar-local org-air-view--refresh-queue nil
  "Files not yet scanned by the in-flight chunked refresh (R26-8).")
(defvar-local org-air-view--refresh-total 0
  "Total file count of the in-flight chunked refresh (R26-8).")
(defvar-local org-air-view--refresh-acc nil
  "Items accumulated PRIVATELY by the refresh slices (R26-8).
The board repaints exactly once, when the whole accumulation swaps in —
never a partial paint.")
(defvar-local org-air-view--refresh-mtimes nil
  "Alist FILE -> mtime captured per file AT SCAN TIME (R26-8).
Persisted with the cache so the next start can detect staleness.")
(defvar-local org-air-view--refresh-timer nil
  "The pending one-shot idle timer of the in-flight refresh, or nil.")
(defvar-local org-air-view--cache-stale-files nil
  "Files whose mtime diverged from the cache snapshot (R26-8).
While REFRESHING, triage verbs on an item from one of these soft-error
\(\"Still refreshing this file…\"); positions in unchanged files are valid
by construction (mtime match).")
(defvar-local org-air-view--tag-filter nil)
(defvar-local org-air-view--scope nil)
(defvar-local org-air-view--rail-descriptor nil
  "Plist of providers the SHARED rail consults; nil = the board defaults (R20-5).
When a non-board view (the project) renders the rail it sets this so the
one rail construct serves both views (invariant #4: parameterise, do not
fork).  All keys are optional; the board path is used for any that are
absent, so the board byte goldens stay byte-identical (descriptor nil).
Keys:
  :visible-fn      (THINGS) -> the scope+filter visible subset
  :calendar-fn     (VISIBLE WIDTH INSET) -> inserts the month calendar
  :summary-fn      (THINGS WIDTH) -> inserts the Summary block
  :first-thing-fn  (THINGS) -> the thing the inspector seeds on (nil ok)
  :actions-fn      (WIDTH) -> inserts the Actions block
  :rail-target-height N -> the inspector reserved-region target height
    (the project sizes the rail to its doc-pane height, not the window).")
(defvar-local org-air-view--day nil
  "When non-nil, an Emacs time focusing the single-day view (R6).")
(defvar-local org-air-view--expanded-sections nil)
(defvar-local org-air-view--line-width nil)
(defvar-local org-air-view--rendered-width nil
  "Column width used for the most recent render of this dashboard buffer.")
(defvar-local org-air-view--rendered-height nil
  "Body height used for the most recent render of this dashboard buffer.")
(defvar-local org-air-view--body-beg nil
  "Marker at the first body-band line of the last render (R18 D-P1b).
Set by `org-air-view--render'; bounds the in-place section splice so it
never touches the header.")
(defvar-local org-air-view--body-end nil
  "Marker just past the last body-band line of the last render (R18 D-P1b).")
(defvar-local org-air-view--body-target-floor nil
  "Minimum body-band row count `=height - header - footer' (R18 D-P1b).
The splice reuses the EXACT body-target formula so the spliced buffer is
byte-identical to a full render.")
(defvar-local org-air-view--body-fill-row nil
  "Fill row used to pad the body band to full height (R18 D-P1b).")
(defvar-local org-air-view--orientation nil
  "Last chosen layout orientation, `two-pane' or `stacked' (D1 hysteresis).")
(defvar-local org-air-view--rail-popped-out 'unset
  "Single source of truth: is the rail a side window right now (R16 D-P1)?
Nil = inline rail; non-nil = popped out into the `*org-air-rail*' side
window.  The sentinel `unset' means \"not yet initialised\"; first render
seeds it from `org-air-rail-style' (`side-window' -> t, else nil).  The
toggle flips it; the cooperative reconciler clears it when the user closes
the side window with a native command.")
(defvar-local org-air-view--rail-suspended nil
  "Non-nil when this view is popped-out but its side rail is HIDDEN (R25-6).
Set when another org-air view becomes the active main view and claims (or
empties) the singleton `*org-air-rail*' side window: this view keeps its
`org-air-view--rail-popped-out' flag t but its side window is suspended, so
returning to it cleanly RE-pops.  Distinguishes \"hidden for a cross-view
switch\" (re-pop on return) from \"the user natively CLOSED the rail\" (fall
back inline).  Cleared whenever this view owns the side window or goes
inline.")
(defvar org-air-rail--reconciling nil
  "Re-entrancy latch for `org-air-rail--reconcile' (R16 D-P1).
R27-1 S3: also bound t for the FULL extent of a board/project render, so
a reconcile timer nesting inside an in-flight render (org-ql yields run
pending timers) no-ops instead of mutating rail state mid-render.")
(defvar org-air-rail--reconcile-timer nil
  "The SINGLE pending deferred-reconcile timer, or nil (R27-1 S3).
Every `window-configuration-change-hook' fire routes through this one
slot: a fire while a reconcile is already pending RESCHEDULES it instead
of stacking one new 0s timer per fire (measured trunk: 5 fires -> 5 live
timers).  The timer body is the named `org-air-rail--reconcile-run' so
tests can count pending slots deterministically.")
(defvar org-air-rail--side-was-live nil
  "Non-nil when the side rail window was LIVE at the last observation (R27-1).
The reconciler's user-close branch is EDGE-TRIGGERED on an observed
live->dead transition of this flag: mere absence of the side window with
the popped flag t (a popout still in flight, mid-render) must never be
classified as a user close.  Updated by `org-air-rail--show',
`org-air-rail--hide' and each `org-air-rail--reconcile-frame' run.")
(defvar-local org-air-rail--last-stamp nil
  "Input stamp of the last rail content paint (R27-1 S4).
Local to the `*org-air-rail*' buffer.  When `org-air-rail--show' computes
an identical stamp the erase+re-insert is skipped (the output would be
byte-identical); any component change repaints.  See
`org-air-rail--input-stamp'.")
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
    ;; R22-3: o/O now drive the shared SORT (view-core), so the board's
    ;; visit verbs relocate under the g-prefix: `g RET' visits in the other
    ;; window, `g o' visits but stays.  GUI visit stays on S-RET.
    (define-key map (kbd "RET") #'org-air-visit-item)
    (define-key map (kbd "o") #'org-air-visit-item-stay)
    map)
  "Transient g-prefix map (B4): r refresh, g top of pane, R refresh+clear.
R22-3: g RET visit, g o visit-stay (o/O are the shared sort now).")

;;;; =====================================================================
;;;; R30-2 — a main-window C-c LEADER for the rail actions.  The rail/
;;;; legend advertises verbs (RET jump, `|' rail, outline nav) that only
;;;; fire when the SIDE WINDOW is focused; from the editable doc-session
;;;; org buffer single keys self-insert.  A shared `C-c' leader prefix
;;;; installed on the content buffers reuses the EXISTING commands, and
;;;; the legend derives each key context-correctly via `where-is'.
;;;; =====================================================================

(defvar org-air-view--leader-installs nil
  "List of (HOST-MAP . PREFIX-MAP) leader installs to keep synced (R30-2).
Each registered pair is re-bound whenever `org-air-leader-key' changes so
the leader prefix follows a user rebinding.")

(defvar org-air-view--leader-installed-key nil
  "The key sequence at which the leader prefix is currently installed (R30-2).")

(defcustom org-air-leader-key "C-c C-a"
  "Key sequence for the org-air main-window action leader (R30-2).
A clean, org-safe `C-c C-<letter>' prefix (mnemonic: Air) that does not
collide with org-mode's own `C-c' bindings in the doc session and is left
alone by evil in normal AND insert state.  Rebinding it re-installs the
leader prefix on every content buffer's map (the legend follows via
`where-is')."
  :type 'key-sequence
  :group 'org-air
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'org-air-view--leader-reinstall)
           (org-air-view--leader-reinstall))))

(defun org-air-view--leader-reinstall ()
  "Re-bind every registered leader prefix at the current `org-air-leader-key'.
Unbinds the previous key first (a no-op on a fresh install), so a
`org-air-leader-key' change moves the prefix without leaving the old one
bound.  The legend follows automatically (it derives keys via
`where-is')."
  (dolist (pair org-air-view--leader-installs)
    (when (and org-air-view--leader-installed-key
               (not (equal org-air-view--leader-installed-key
                           org-air-leader-key)))
      (define-key (car pair) (kbd org-air-view--leader-installed-key) nil))
    (define-key (car pair) (kbd org-air-leader-key) (cdr pair)))
  (setq org-air-view--leader-installed-key org-air-leader-key))

(defun org-air-view--leader-install (host-map prefix-map)
  "Bind PREFIX-MAP at `org-air-leader-key' in HOST-MAP (R30-2).
Registers the pair so a later `org-air-leader-key' change re-installs it
via `org-air-view--leader-reinstall'.  Returns PREFIX-MAP."
  (cl-pushnew (cons host-map prefix-map) org-air-view--leader-installs
              :test #'equal)
  (define-key host-map (kbd org-air-leader-key) prefix-map)
  (setq org-air-view--leader-installed-key org-air-leader-key)
  prefix-map)

(defun org-air-view--legend-key (command buffer &optional fallback)
  "Return the key text for COMMAND live in BUFFER, or FALLBACK (R30-2).
`where-is-internal' FIRSTONLY on COMMAND resolved in BUFFER's OWN active
keymaps (so an evil/custom rebinding shows what is really bound there),
formatted via `key-description'.  In a read-only rail this returns the
BARE key (RET, `|'); in the editable doc buffer where those keys
self-insert it returns the LEADER form (C-c C-a o, C-c C-a |).  The one
derivation every legend cell shares — the legend can never lie about
reachability."
  (or (and (buffer-live-p buffer)
           (with-current-buffer buffer
             (let ((key (where-is-internal command nil t)))
               (and key (key-description key)))))
      fallback))

(defun org-air-outline--heading-positions ()
  "Return the buffer positions of the Org headings in the current buffer.
A pure `^\\*+[ \\t]+' scan (R26-5 shape) — no Org re-parse, no Air struct;
reused by the R30-2 leader outline motions and the R30-4 outline mode."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let (ps)
        (while (re-search-forward "^\\*+[ \t]+" nil t)
          (push (match-beginning 0) ps))
        (nreverse ps)))))

(defun org-air-outline-next-heading ()
  "Move point to the next Org heading in this buffer (R30-2 leader `n')."
  (interactive)
  (let ((next (cl-find-if (lambda (p) (> p (point)))
                          (org-air-outline--heading-positions))))
    (if next (goto-char next)
      (message "org-air: no next heading"))))

(defun org-air-outline-prev-heading ()
  "Move point to the previous Org heading in this buffer (R30-2 leader `p')."
  (interactive)
  (let ((prev (cl-find-if (lambda (p) (< p (line-beginning-position)))
                          (reverse (org-air-outline--heading-positions)))))
    (if prev (goto-char prev)
      (message "org-air: no previous heading"))))

(defun org-air-outline-goto-current-heading ()
  "Jump to the Org heading enclosing point — the outline anchor (R30-2 `o').
Reuses the same heading scan as the rail outline: the `jump' verb from
the editable doc buffer, where `RET' self-inserts."
  (interactive)
  (let ((cur (cl-find-if (lambda (p) (<= p (point)))
                         (reverse (org-air-outline--heading-positions)))))
    (if cur (goto-char cur)
      (message "org-air: point is before the first heading"))))

(defvar org-air-leader-map
  (let ((map (make-sparse-keymap)))
    ;; Board/project content buffers: rail toggle, outline jump, the
    ;; shared sort, the per-view filter — all EXISTING commands, reached
    ;; from the main window under the `C-c' leader (never a fork).
    (define-key map (kbd "|") #'org-air-rail-toggle)
    (define-key map (kbd "o") #'org-air-rail-return)
    (define-key map (kbd "s") #'org-air-view-sort-cycle)
    (define-key map (kbd "/") #'org-air-filter)
    map)
  "Leader prefix map for the BOARD content buffer (R30-2).
Installed at `org-air-leader-key' on `org-air-view-mode-map'.")

(defvar org-air-view-core-map
  (let ((map (make-sparse-keymap)))
    ;; Keep the `special-mode' defaults reachable below the shared core.
    (set-keymap-parent map special-mode-map)
    ;; R18 D-P3: the unambiguous VIEW-CORE keys live here ONCE, so they can
    ;; never drift between the board and project maps (both inherit this via
    ;; `set-keymap-parent').  RET owns the bottom view pane; v/V open+close
    ;; it; \ clears the filter; M-/ toggles AND/OR.  Each child overrides
    ;; only its motion, its per-mode `/' filter, its S-RET visit target and
    ;; its DOMAIN verbs.
    (define-key map (kbd "RET") #'org-air-view-pane-return)
    (define-key map (kbd "<mouse-1>") #'org-air-view-pane-return)
    (define-key map (kbd "v") #'org-air-view-pane)
    (define-key map (kbd "V") #'org-air-view-pane-close)
    (define-key map (kbd "\\") #'org-air-filter-clear)
    (define-key map (kbd "M-/") #'org-air-filter-toggle-match)
    ;; R22-3: the shared within-view SORT — `o' cycles the key, `O' reverses
    ;; the direction — bound ONCE here so the board and the project inherit
    ;; the same UX (no fork); each seeds its own key list + refresh fn.
    (define-key map (kbd "o") #'org-air-view-sort-cycle)
    (define-key map (kbd "O") #'org-air-view-sort-reverse)
    ;; R22-5: pop the context rail in/out of a native side window — shared
    ;; so BOTH the board and the project toggle their rail with `|'.
    (define-key map (kbd "|") #'org-air-rail-toggle)
    ;; R29-2: vim-ish j/k line motion moves UP from the board map (R3) so
    ;; the PROJECT has the title-landing motions too (it bound neither, so
    ;; under evil j/k resolved to evil-next-line/evil-previous-line whose
    ;; goal-column pinned point at column 0), and the R27-4 evil overriding
    ;; map has the keys to override.  Identical bindings on the board
    ;; (byte- and behavior-identical there — inheritance, not a copy).
    (define-key map (kbd "j") #'org-air-next-line)
    (define-key map (kbd "k") #'org-air-prev-line)
    map)
  "Shared view-core keymap, parent of the board + project mode maps (R18 D-P3).
Reuse the core, override the bespoke: the keys here are identical across
both views; per-mode domain verbs stay in each child map.")

(defvar org-air-view-mode-map
  (let ((map (make-sparse-keymap)))
    ;; R18 D-P3: inherit the shared view-core keys (RET pane, v/V, \, M-/).
    (set-keymap-parent map org-air-view-core-map)
    ;; R18 D-P4: visiting the file in the other window is S-RET (and `O' as
    ;; a TTY alias since terminals can't send S-RET); RET itself opens the
    ;; pane (inherited from the core map).
    (define-key map (kbd "<S-return>") #'org-air-visit-item)
    (define-key map (kbd "S-RET") #'org-air-visit-item)
    (define-key map (kbd "n") #'org-air-next-item)
    (define-key map (kbd "p") #'org-air-prev-item)
    ;; R3/R29-2: vim-ish j/k line navigation (NOT destructive) is inherited
    ;; from `org-air-view-core-map' now — shared with the project.
    ;; T2: TAB toggles expand/collapse of the section at point; section
    ;; MOTION lives on M-n/M-p (and M-TAB) so both verbs are reachable.
    (define-key map (kbd "TAB") #'org-air-toggle-section)
    (define-key map (kbd "<backtab>") #'org-air-prev-section)
    (define-key map (kbd "M-TAB") #'org-air-next-section)
    (define-key map (kbd "M-n") #'org-air-forward-section)
    (define-key map (kbd "M-p") #'org-air-back-section)
    (define-key map (kbd "SPC") #'org-air-peek-item)
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
    ;; `/' is the per-mode filter (board item tags); `\' clear + `M-/'
    ;; toggle are inherited from the shared core map (R18 D-P3).
    (define-key map (kbd "/") #'org-air-filter)
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
    ;; R16 D-P3 / R18 D-P3: v/V open+close the bottom view pane — inherited
    ;; from `org-air-view-core-map' now (shared with the project).
    (define-key map (kbd "q") #'org-air-quit)
    map)
  "Keymap for `org-air-view-mode'.")

;; R30-2: install the main-window leader on the board map so the rail
;; actions (rail toggle, outline jump, sort, filter) are reachable from
;; the board content buffer under `C-c C-a', not only the side window.
(org-air-view--leader-install org-air-view-mode-map org-air-leader-map)

(defalias 'org-air-mode-map 'org-air-view-mode-map)

(defvar-local org-air-view--mode-line-count nil
  "Cached visible-item count for the calm status mode-line (R20-2).
Set once per board render so the redisplay :eval never re-scans all items.")

(defun org-air-view--mode-line-filter-text ()
  "Return the `filter …' / `filter none' segment from the active tag filter.
R22-4: empty reads `filter none' (not `no filter') so the FILTER segment is
named the same way whether empty or active, and never collides with the
`source ...' segment's wording."
  (let ((filters (org-air-view--filter-tags)))
    (if filters
        (concat "filter "
                (mapconcat #'org-air-view--filter-token-label filters
                           (concat " " (org-air-view--filter-combinator-word)
                                   " ")))
      "filter none")))

(defun org-air-view--mode-line-content ()
  "Return the calm status text for the current org-air buffer (R20-2).
Branches on `major-mode' so the board and project share ONE construct per
invariant #4: the board reports its visible item count · filter · scope;
the project reports its doc count · filter; rail/pane buffers fall back to
the bare mode name.  Reads only buffer-locals already on hand, so it is
cheap in the redisplay :eval (no render-path work)."
  (cond
   ((derived-mode-p 'org-air-view-mode)
    (let ((n (or org-air-view--mode-line-count
                 (length (org-air-view--visible-items org-air-view--items)))))
      ;; R22-4: `source <...>' (was `scope <...>') so the two segments no
      ;; longer both read "all items"; the filter segment is `filter none'.
      (format "org-air · %d item%s · %s · source %s · sort %s"
              n (if (= n 1) "" "s")
              (org-air-view--mode-line-filter-text)
              (org-air-view--scope-label)
              (org-air-view--sort-active-key))))
   ((derived-mode-p 'org-air-project-mode)
    (let ((n (or (and (boundp 'org-air-project--doc-count)
                      org-air-project--doc-count)
                 0)))
      (format "org-air · project · %d doc%s · %s"
              n (if (= n 1) "" "s")
              (org-air-view--mode-line-filter-text))))
   (t (format-mode-line mode-name))))

;; The back-compat alias is declared BEFORE its referent so the byte
;; compiler keeps `org-air-view--calm-mode-line' pointing at the canonical
;; `org-air-view--status-mode-line' without the "alias should be declared
;; before its referent" warning (a defvaralias whose base is already bound
;; trips that check); 0 compile warnings.
(defvaralias 'org-air-view--calm-mode-line 'org-air-view--status-mode-line
  "Back-compat alias for the renamed status construct (R20-2).")

(defconst org-air-view--status-mode-line
  '(:eval (propertize (concat "  " (org-air-view--mode-line-content) "  ")
                      'face 'org-air-face-modeline))
  "The calm nano-style STATUS mode-line construct (R18 D-P5.1 / R20-2).
A single quiet :eval that earns its row — counts · filter · scope from the
buffer-locals already on hand — in the faded `org-air-face-modeline' (which
also draws the boundary overline).  Mode-line is not part of the buffer-
text fixtures, so this is byte-invisible.")

(defun org-air-view--install-modeline ()
  "Install the calm nano-style mode-line, or restore the user's own (R23-2).
`calm' installs the minimal faded nano construct; ANY other value
\(`default'/nil) drops any buffer-local override so the user's normal
mode-line shows.  Symmetric so a runtime `calm'->`default' flip actively
restores the inherited line (not just a no-op on a fresh buffer).  Single
line either way, so the body-height derivation is unchanged; byte-invisible
\(the mode-line is not buffer text)."
  (if (eq org-air-modeline-style 'calm)
      (setq-local mode-line-format (list org-air-view--status-mode-line))
    (kill-local-variable 'mode-line-format)))

(define-derived-mode org-air-view-mode special-mode "org-air"
  "Major mode for the org-air dashboard."
  (setq-local truncate-lines t)
  ;; S1: the header band is in-buffer text only; never a header line.
  (setq-local header-line-format nil)
  ;; R18 D-P5.1: a calm, faded nano-style mode-line (status lives in the
  ;; in-buffer banner); byte-invisible (mode-line is not buffer text).
  (org-air-view--install-modeline)
  ;; D-P3: `org-air-line-spacing' default 0 keeps the `│' divider glyph an
  ;; unbroken vertical rule (no gap below the row for the per-cell glyph
  ;; to skip).  The capsule breathing now lives inside each pill via
  ;; `org-air-pill-vinset'.  A non-zero value re-introduces spacing.
  (setq-local line-spacing org-air-line-spacing)
  (setq-local cursor-type 'bar)
  (setq-local org-air-layout-refresh-function #'org-air-view--resize-refresh)
  ;; R22-3: seed the shared sort spec — the board's key list + the refresh fn
  ;; the shared `o'/`O' commands call; the default key `date' reproduces the
  ;; historical within-bucket order so the goldens stay byte-identical.
  (setq-local org-air-view--sort-keys '(date priority title recency))
  (setq-local org-air-view--sort-refresh #'org-air-view--render-current)
  (unless org-air-view--sort-key
    (setq-local org-air-view--sort-key org-air-sort-key))
  (unless org-air-view--sort-direction
    (setq-local org-air-view--sort-direction org-air-sort-direction))
  (setq-local buffer-read-only t)
  ;; T6: re-fit when the font/text size changes (text-scale alters how many
  ;; columns/rows fit), debounced through the same window-size path.
  (add-hook 'text-scale-mode-hook #'org-air-view--text-scale-refresh nil t)
  ;; D-P7 / R14 D-P1.B: this buffer hosts the mid-rail inspector; the core
  ;; reads the board defaults (`org-air-item' property, item fields).
  (setq-local org-air-view--inspector-active org-air-show-inspector)
  ;; track point to keep the rail inspector synced (debounced).
  ;; P0: INERT when noninteractive — never install the live hook under
  ;; batch (`make check' / `make regen-mockups') so nothing schedules a
  ;; timer or waits for input; the compose path stays pure/synchronous.
  (unless noninteractive
    (add-hook 'post-command-hook #'org-air-view--inspector-post-command nil t))
  ;; V3: the round-4/T7 buffer-box outer frame is DROPPED — it shipped
  ;; half-drawn (partial top edge, deferred right) and a half-box reads
  ;; worse than none.  Structure comes from the in-buffer full-width
  ;; hairline rules (S2) and the single internal rail divider; no chrome
  ;; frame, so `header-line-format' stays nil (S1) and the mode-line is
  ;; the default.
  ;; R15 D-P2: tear down the side-window rail when the board buffer is
  ;; killed (the rail buffer + side window must not outlive the board).
  (add-hook 'kill-buffer-hook #'org-air-rail--teardown nil t)
  ;; R26-8: a dying board cancels its in-flight chunked refresh outright
  ;; (token bump + timer cancel), so no slice can outlive its buffer.
  (add-hook 'kill-buffer-hook #'org-air-view--refresh-teardown nil t)
  ;; R16 D-P1: cooperative reconciler — fall back to inline when the user
  ;; closes the popped-out rail with a native window command.  Reactive
  ;; only; never re-creates a window the user closed.
  (unless noninteractive
    (add-hook 'window-configuration-change-hook #'org-air-rail--reconcile nil t))
  ;; R16 D-P3: follow-mode point-tracking for the bottom view pane (inert
  ;; in batch; a separate consumer of board point from the inspector).
  (unless noninteractive
    (add-hook 'post-command-hook #'org-air-view--view-pane-post-command nil t))
  ;; R22-2b/R29-2: snap point off the dead gutter/margin/rail/pad columns
  ;; onto the row title after any LINE-crossing command (incl. native
  ;; arrow/C-n/C-p/mouse and evil's line motions) — the pre-command line
  ;; snapshot gates the snap so in-row horizontal motion is never hijacked;
  ;; inert under batch like the other hooks.
  (unless noninteractive
    (add-hook 'pre-command-hook #'org-air-view--pre-command-snapshot nil t)
    (add-hook 'post-command-hook #'org-air-view--normalize-point nil t))
  (org-air-view--setup-evil 'org-air-view-mode org-air-view-mode-map)
  (org-air-layout-install-window-size-hook))

(defun org-air-view--text-scale-refresh ()
  "Re-fit the dashboard after a text-scale/font-size change (T6).
Routes through the debounced resize handler so a rapid sequence of scale
changes coalesces into a single re-render."
  (org-air-layout--window-size-change))

(defalias 'org-air-mode #'org-air-view-mode)

(declare-function evil-set-initial-state "evil-core")
(declare-function evil-make-overriding-map "evil-common")

(defvar org-air-view--evil-modes nil
  "Alist of (MODE . MAP) views registered via `org-air-view--setup-evil'.
R29-2 registration hardening: `org-air-view--evil-registration-replay'
walks this table when evil loads AFTER the org-air modes initialised, so
a deferred evil still gets motion state + overriding maps for every
view.")

(defun org-air-view--setup-evil (mode map)
  "Integrate the org-air special-mode MODE + MAP with evil, when loaded.
U2: under evil, single-key org-air bindings are otherwise shadowed by
evil's motion/normal state maps and only fire after a \\=`\\\=' prefix.
This is a soft dependency — evil is never required.  When evil is
available we place MODE's buffers in motion state and make MAP an
overriding map so the org-air keys win, while evil's own scrolling and
search motions keep working.  Non-evil users are entirely unaffected.
R27-4: parameterised (was board-only) and called from EVERY org-air
special-mode view — board, project, rail, entry-view pane — so no view's
keys are shadowed into evil-record-macro / evil-open-below / etc. under
evil's normal state; one shared setup, no fork.  The two minor modes
\(doc-session + return) need NOTHING: their verbs are `C-c'-prefixed or
remaps on the user's own editable file buffers, where forcing a state
would be wrong.  Idempotent; runs once per mode init.
R29-2: every MODE + MAP pair is also recorded in
`org-air-view--evil-modes' so a LATE-loading evil (deferred `use-package')
still registers the already-defined modes — see
`org-air-view--evil-registration-replay' below."
  (setf (alist-get mode org-air-view--evil-modes) map)
  (when (fboundp 'evil-make-overriding-map)
    (evil-make-overriding-map map 'motion))
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state mode 'motion)))

(defun org-air-view--evil-registration-replay (&rest _)
  "Replay evil registration for every recorded org-air view (R29-2).
Registration hardening (defense-in-depth, the R28-1 soft-dep idiom): mode
init runs `org-air-view--setup-evil' behind an fboundp gate, so an evil
loaded AFTER the first org-air buffer silently got NO registration.  This
runs from `after-load-functions' (a standard hook — no `eval-after-load'
in a package); once evil is present it replays the table and removes
itself.  Idempotent (`evil-make-overriding-map' / `evil-set-initial-state'
both are), so already-registered modes are unaffected."
  (when (featurep 'evil)
    (remove-hook 'after-load-functions
                 #'org-air-view--evil-registration-replay)
    (pcase-dolist (`(,mode . ,map) org-air-view--evil-modes)
      (org-air-view--setup-evil mode map))))

;; If evil is already present, the mode inits' fboundp gate registers
;; directly; otherwise watch for its (deferred) arrival via the standard
;; `after-load-functions' hook (package-safe — no `eval-after-load').
(if (featurep 'evil)
    (org-air-view--evil-registration-replay)
  (add-hook 'after-load-functions #'org-air-view--evil-registration-replay))

;; Declaration only (no value): the soft-dep registration below must leave
;; this VOID when dimmer is absent — the integration is provably dormant.
(defvar dimmer-buffer-exclusion-predicates)

(defun org-air-dimmer-buffer-p (buf)
  "Non-nil when BUF is an org-air-OWNED buffer (R28-1).
Matches the shipped `*org-air' naming contract — board `*org-air*', rail
`*org-air-rail*', snapshot pane `*org-air-view*', editable pane
`*org-air-pane:TITLE*', project `*org-air-project*' — with an
optional-space belt so any legacy/hidden org-air internal stays covered
no matter what the naming layer renames.  The doc-session and R20
return-mode host buffers are the USER'S file buffers: never renamed,
never matched here (dimming those stays the user's own policy)."
  (and (string-match-p "\\` ?\\*org-air" (buffer-name buf)) t))

(defun org-air-view--setup-dimmer (&optional _file)
  "Register `org-air-dimmer-buffer-p' on dimmer's exclusion seam (R28-1).
Soft-dep dimmer integration (zero config, the R27-4 evil idiom):
`dimmer-buffer-exclusion-predicates' is dimmer 0.4's per-buffer predicate
seam (called with the buffer; truthy = never dimmed) — the exact seam
dimmer's own `dimmer-configure-*' helpers use.  `add-to-list' is
idempotent and the seam is only touched when dimmer is LOADED (`boundp'
gate — dimmer is never required): without dimmer this is provably
dormant and no dimmer variable is created.  Returns non-nil once
registered (and disarms the deferred `after-load-functions' seam below).
Users with the manual exclusion regexp keep working — the R28-1(a)
naming makes their regexp true again; this makes it unnecessary."
  (when (boundp 'dimmer-buffer-exclusion-predicates)
    (add-to-list 'dimmer-buffer-exclusion-predicates
                 #'org-air-dimmer-buffer-p)
    (remove-hook 'after-load-functions #'org-air-view--setup-dimmer)
    t))

;; R28-1(b): register NOW when dimmer is already loaded, else on the load
;; that brings it in (`after-load-functions' — a one-shot boundp probe,
;; removed the moment it registers).  Either order — dimmer before or
;; after org-air — lands the same idempotent registration.
(unless (org-air-view--setup-dimmer)
  (add-hook 'after-load-functions #'org-air-view--setup-dimmer))

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
  "Return first timestamp in ITEM subtree, if any.
R26-8: resolves a live marker OR a cache-hydrated (FILE . POS) cons via
`org-air-classify--item-source' (one background file visit, shared
buffer), so a cache-painted board's stale labels are byte-identical to a
live scan's; a stale position mid-refresh degrades to nil (file-mtime
fallback), never a crash."
  (when-let* ((src (org-air-classify--item-source item)))
    (with-current-buffer (car src)
      (ignore-errors
        (save-excursion
          (goto-char (cdr src))
          (org-back-to-heading t)
          (let ((end (save-excursion (org-end-of-subtree t t))))
            (when (re-search-forward org-ts-regexp-both end t)
              (ignore-errors
                (org-timestamp-to-time
                 (org-timestamp-from-string
                  (match-string-no-properties 0)))))))))))

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
  "Return active filter tokens as a list (R24-6: tokens stored VERBATIM).
Each token is either a `#tag' (a tag match) or a bare substring token."
  (cond
   ((null org-air-view--tag-filter) nil)
   ((listp org-air-view--tag-filter) org-air-view--tag-filter)
   ((stringp org-air-view--tag-filter) (list org-air-view--tag-filter))))

(defun org-air-view--filter-token-label (token)
  "Return TOKEN as it should appear in the filter lens display (R24-6).
A `#tag' token reads verbatim; a bare substring token reads quoted
\(`\"git\"') so the lens presents it as text, not a tag."
  (if (string-prefix-p "#" token) token (format "%S" token)))

(defun org-air-view--filter-token-match-p (token text tags)
  "Non-nil when TOKEN matches TEXT/TAGS (R24-6 filter mini-language).
A `#tag' token = exact TAG membership; a BARE token = case-insensitive
SUBSTRING of TEXT (the caller builds it from the title + origin/path) plus
the tag NAMES (so a bare tag name still finds its tagged items, the legacy
behaviour as a subset).  Case-insensitive throughout."
  (if (string-prefix-p "#" token)
      (let ((tag (downcase (substring token 1))))
        (and (member tag (mapcar #'downcase tags)) t))
    (and (string-search (downcase token)
                        (downcase (concat (or text "") " "
                                          (string-join tags " "))))
         t)))

(defun org-air-view--tokens-pass-filter-p (text tags)
  "Return non-nil when TEXT/TAGS satisfy the active filter tokens + combinator.
SHARED by the board (item title+origin+tags) and the project (doc
name+relpath+tags) — the one matcher both views call (R24-6, generalising
R18 D-P3).  Empty filter passes everything; `org-air-filter-match' selects
`all' (AND) or `any' (OR) and spans MIXED #tag / bare-substring tokens."
  (let ((tokens (org-air-view--filter-tags)))
    (or (null tokens)
        (and (funcall (if (eq org-air-filter-match 'all) #'seq-every-p #'seq-some)
                      (lambda (tok)
                        (org-air-view--filter-token-match-p tok text tags))
                      tokens)
             t))))

(defun org-air-view--tags-pass-filter-p (item-tags)
  "Return non-nil when ITEM-TAGS satisfy the active filter + combinator.
R18 D-P3: the pure matcher SHARED by the board (`org-air-item-tags') and
the project (`org-air-doc-tags').  R24-6: a thin tags-only wrapper over
`org-air-view--tokens-pass-filter-p' (no searchable text) so any legacy
caller still tag-matches; the real call sites pass the title/path text."
  (org-air-view--tokens-pass-filter-p "" item-tags))

(defun org-air-view--passes-filter-p (item)
  "Return non-nil when ITEM passes the active filter (R24-6).
Passes the item's title + origin breadcrumb as the searchable TEXT so a
bare token substring-matches the title; `#tag' tokens still tag-match."
  (org-air-view--tokens-pass-filter-p
   (concat (org-air-item-title item) " " (org-air-view--origin item))
   (org-air-item-tags item)))

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
  "Return ITEMS after scope and filter.
Reads the compute-once `org-air-view--render-partition' when bound for the
SAME ITEMS (R20-6), so a render computes the visible set exactly once
instead of 21x; falls back to a fresh scan off-render."
  (if (and org-air-view--render-partition
           (eq items (car org-air-view--render-partition)))
      (cadr org-air-view--render-partition)
    (seq-filter (lambda (item)
                  (and (org-air-view--passes-scope-p item)
                       (org-air-view--passes-filter-p item)))
                items)))

(defun org-air-view--compute-partition (items &optional now)
  "Build the compute-once render partition for ITEMS as of NOW (R20-6).
Returns (ITEMS VISIBLE . TABLE): VISIBLE is `org-air-view--visible-items'
in source order; TABLE is an `eq' hash mapping each classify bucket to its
visible members in SOURCE order, byte-identical to what repeated
`org-air-view--items-for-bucket' calls produced, in ONE classify pass."
  (let* ((now (or now (current-time)))
         (org-air-view--render-partition nil) ; force a true scan, not self
         (visible (org-air-view--visible-items items))
         (table (make-hash-table :test 'eq)))
    (dolist (item visible)
      (dolist (bucket (org-air-view--classify-cached item now))
        (push item (gethash bucket table))))
    (maphash (lambda (k v) (puthash k (nreverse v) table)) table)
    (cons items (cons visible table))))

(defun org-air-view--classify-cache-ensure (&optional now)
  "Ensure this board's classify cache table exists for NOW's day (R18 D-P1c).
Called once in the MAIN board buffer at the start of a render so the temp
pane buffers (`org-air-view--render-lines') can bind the cache var to the
SAME table object; `puthash' there then persists back to this buffer.  A
day rollover (or a missing table) rebuilds it."
  (let ((day (time-to-days (or now (current-time)))))
    (unless (and org-air-view--classify-cache
                 (eql org-air-view--classify-cache-day day))
      (setq org-air-view--classify-cache (make-hash-table :test 'eq :size 700)
            org-air-view--classify-cache-day day))))

(defun org-air-view--classify-cached (item &optional now)
  "Return ITEM's bucket list, memoised per board (R18 D-P1c).
Delegates to the pure `org-air-classify-item'; caches the result keyed on
the item object (`eq').  Classify is DAY-granular (every predicate is a
day-window comparison), so the cache key is the day of NOW: a render later
the same day is a pure cache hit; a render after midnight rebuilds."
  (let ((now (or now (current-time))))
    (org-air-view--classify-cache-ensure now)
    (let ((hit (gethash item org-air-view--classify-cache 'miss)))
      (if (eq hit 'miss)
          (puthash item (org-air-classify-item item now)
                   org-air-view--classify-cache)
        hit))))

(defun org-air-view--items-for-bucket (bucket items)
  "Return visible ITEMS classified into BUCKET.
Real-signal membership (ruling xsqrnoyn): an item appears in every bucket
it genuinely qualifies for, so a dated inbox capture shows in BOTH Inbox
and its date bucket.  The no-date attention default for inbox-dwellers is
suppressed in `org-air-classify-item', not here, so no dedup is needed.
Classify is routed through `org-air-view--classify-cached' (R18 D-P1c) so
each item is classified at most once per render; one NOW is bound for the
whole call so every item classifies against a single instant."
  (if (and org-air-view--render-partition
           (eq items (car org-air-view--render-partition)))
      (gethash bucket (cddr org-air-view--render-partition))
    (let ((now (current-time)))
      (seq-filter (lambda (item)
                    (memq bucket (org-air-view--classify-cached item now)))
                  (org-air-view--visible-items items)))))

(defun org-air-view--section-limit (bucket)
  "Return the row cap a section renders for BUCKET (used when not expanded)."
  (pcase bucket
    ('attention 6)
    ('upcoming 5)
    (_ org-air-section-max)))

(defun org-air-view--displayed-for-bucket-1 (bucket items)
  "Compute (no memo) the BUCKET rows of ITEMS a section renders (R20-6).
Mirrors `org-air-view--insert-section': the bucket members (date-sorted for
attention/upcoming), capped to `org-air-view--section-limit' unless the
section is expanded."
  (let* ((bucket-items (org-air-view--items-for-bucket bucket items))
         ;; R22-3: order WITHIN the bucket by the active sort key/direction.
         ;; The default key `date' reproduces the historical order exactly
         ;; (attention/upcoming date-sorted, the rest query order).
         (bucket-items (org-air-view--sort-items bucket-items bucket)))
    (if (memq bucket org-air-view--expanded-sections)
        bucket-items
      (seq-take bucket-items (org-air-view--section-limit bucket)))))

(defun org-air-view--displayed-items-for-bucket (bucket items)
  "Return the BUCKET rows of ITEMS a section actually renders (R20-6).
Memoised per render via `org-air-view--render-displayed' so the section
pass and `org-air-view--compute-meta-widths' (which measures only THESE
displayed rows, ~35, not every member) SHARE one sort+take per bucket
instead of each paying it.  Falls back to a fresh compute off-render."
  (if (and org-air-view--render-displayed
           (eq items (car org-air-view--render-displayed)))
      (let* ((memo (cdr org-air-view--render-displayed))
             (hit (gethash bucket memo 'miss)))
        (if (eq hit 'miss)
            (puthash bucket
                     (org-air-view--displayed-for-bucket-1 bucket items)
                     memo)
          hit))
    (org-air-view--displayed-for-bucket-1 bucket items)))

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
keeping the date.  R27-3: the active-sort badge sheds LAST of the
optional segments (after filter, scope and count)."
  (let* ((w (org-air-view--render-width))
         (left (propertize "  org-air" 'face 'org-air-face-header))
         ;; D-P3: per-segment faces — date salient, count faded (or salient
         ;; via `org-air-header-accent-count'), filter/scope faded.  The
         ;; assembled width is unchanged (propertize never alters it).
         (date (propertize (format-time-string "%a %d %b" (current-time))
                           'face 'org-air-face-header-date))
         ;; R20-1: during the brief synchronous fast-paint window the count
         ;; slot shows a static `loading…' cue instead of the item count.
         ;; `org-air-view--loading' is nil on every normal render, so this
         ;; collapses to the unchanged item count (byte-identical).  R26-8:
         ;; the same slot carries the honest refresh markers — COLD slice
         ;; progress (`loading I/N files'), the CACHED-stale `stale ·
         ;; refreshing…' cue, and the failure notice.  All display-only and
         ;; transient; the machine never runs in batch, so no golden
         ;; captures them.
         (busy (or org-air-view--loading org-air-view--refresh-state))
         (count (propertize
                 (cond
                  (org-air-view--loading
                   (if (eq org-air-view--refresh-state 'refreshing)
                       (format " · loading %d/%d files"
                               (max 0 (- org-air-view--refresh-total
                                         (length org-air-view--refresh-queue)))
                               org-air-view--refresh-total)
                     " · loading…"))
                  ((eq org-air-view--refresh-state 'refreshing)
                   " · stale · refreshing…")
                  ((eq org-air-view--refresh-state 'failed)
                   " · stale · refresh failed (g retries)")
                  (t (format " · %d items"
                             (length (org-air-view--visible-items items)))))
                 'face (if (and (not busy) org-air-header-accent-count)
                           'org-air-face-count 'org-air-face-faded)))
         ;; R18 D-P2.3: with >=2 active filter tags, join them with the
         ;; combinator word (AND/OR) so the mode reads inline; a single tag
         ;; shows no combinator (irrelevant).
         (filter-text (let* ((filters (org-air-view--filter-tags))
                             (sep (if (> (length filters) 1)
                                      (concat " " (org-air-view--filter-combinator-word) " ")
                                    " ")))
                        (when filters
                          (propertize
                           (concat " · "
                                   (mapconcat (lambda (tag) (concat "#" tag)) filters sep)
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
         ;; R22-3: the within-bucket sort indicator, shown ONLY when a
         ;; non-default sort is active (default `date'/ascending -> nil ->
         ;; the default banner is byte-identical).  R27-3: whenever the
         ;; segment exists it IS the active state, so it takes the bold
         ;; high-contrast `org-air-face-sort-active' and sheds LAST under
         ;; narrow widths (see the shed order below).
         (sort-text (unless (org-air-view--sort-default-p)
                      (concat (propertize " · " 'face 'org-air-face-faded)
                              (org-air-view--sort-indicator-text
                               (org-air-view--sort-active-key)
                               (org-air-view--sort-active-direction)
                               (not (org-air-view--sort-default-p))))))
         ;; Budget for the status: window minus the left token, a >=2-col
         ;; gap, and the reserved one-column right margin.
         (budget (- w (string-width left) 2 1))
         (assemble (lambda (shed)
                     (concat date
                             (unless (memq :count shed) count)
                             (unless (memq :filter shed) (or filter-text ""))
                             (unless (memq :scope shed) (or scope-text ""))
                             (unless (memq :sort shed) (or sort-text "")))))
         (status (catch 'fit
                   ;; R27-3: the active-sort segment sheds LAST among the
                   ;; optional segments — the state the user asked for must
                   ;; not be the first casualty of a narrow window.  With no
                   ;; active sort the segment is nil, so the order change is
                   ;; unobservable and the default goldens hold.
                   (dolist (shed '(() (:filter) (:filter :scope)
                                   (:filter :scope :count)
                                   (:filter :scope :count :sort))
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
  "Return the boxed-pill face for priority CHAR (T1b; R22-1 covers D/E)."
  (pcase char
    (?A 'org-air-face-priority-a)
    (?B 'org-air-face-priority-b)
    (?C 'org-air-face-priority-c)
    (?D 'org-air-face-priority-d)
    (?E 'org-air-face-priority-e)
    (_ 'org-air-face-priority-c)))

(defun org-air-view--priority-color (char)
  "Return the badge colour string for priority CHAR (D-P4).
Resolved light/dark from `org-air-priority-colors' against the frame
background (like the accent palette); falls back to the priority face
foreground when CHAR is absent from the table."
  (let ((pair (cdr (assq char org-air-priority-colors)))
        (dark (eq (frame-parameter nil 'background-mode) 'dark)))
    (or (and (consp pair) (if dark (cdr pair) (car pair)))
        (face-foreground (org-air-view--priority-face char) nil t)
        "gray")))

(defun org-air-view--priority-token (char)
  "Return the `[#C]' priority token for CHAR, badge-pilled when enabled (D-P4).
With `org-air-priority-style' = `badge and svg available the existing
`[#A]' text cell is pillified — the calm capsule tinted by level via
`org-air-view--svg-pillify' :border-color — pixel-locked to its 4-col
cell.  Otherwise the plain coloured `[#A]' text (the TTY/byte fallback)."
  (let* ((face (org-air-view--priority-face char))
         (text (propertize (format "[#%c]" char) 'face face)))
    (if (and (eq org-air-priority-style 'badge)
             (org-air-view--svg-available-p))
        (org-air-view--svg-pillify text face
                                   :border-color (org-air-view--priority-color char))
      text)))

(defun org-air-view--svg-priority-square (char text)
  "Return TEXT (the `■' cell) carrying a tiny filled-square overlay (R13 D-P2).
Draws a small solid square — no stroke, slightly rounded — filled in the
CHAR priority colour (`org-air-view--priority-color'), sized ~62% of the
cell and centred,
box-fit to the 1-col cell and line-height-clamped so it never grows the
row.  Returns TEXT unchanged when svg is unavailable (the `■' glyph is the
fallback)."
  (if (not (org-air-view--svg-available-p))
      text
    (or (ignore-errors
          (let* ((cw (or org-air-view--pill-char-w (frame-char-width)))
                 (ch (or org-air-view--pill-char-h (frame-char-height)))
                 (color (org-air-view--priority-color char))
                 ;; R18 D-P1a: the square is a pure function of (colour, cw,
                 ;; ch); build it once and share the image.
                 (image
                  (org-air-view--svg-image-cached
                   (list 'priority-square color cw ch)
                   (lambda ()
                     (let* ((size (max 3.0 (* 0.62 (min cw ch))))
                            (x (/ (- cw size) 2.0))
                            (y (/ (- ch size) 2.0))
                            (svg (svg-create cw ch)))
                       (svg-rectangle svg x y size size :rx 1 :ry 1 :fill color)
                       (org-air-view--svg-line-image svg cw ch))))))
            (propertize text 'display image)))
        text)))

(defun org-air-view--priority-slot (char)
  "Return the FIXED 2-column priority slot for CHAR (R13 D-P2 `square style).
A coloured filled square (svg on GUI, `■' glyph in TTY) + one pad space
when CHAR is a shown priority (`org-air-priority-show'); two blanks
otherwise — so every item-row title starts at the same column (V6)."
  (if (and char (member char org-air-priority-show))
      (let* ((sq (org-air-view--glyph 'priority-square))
             (face (org-air-view--priority-face char))
             (cell (propertize sq 'face face))
             (cell (org-air-view--svg-priority-square char cell)))
        (concat cell " "))
    "  "))

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

(defun org-air-view--font-ascent ()
  "Return the default font ASCENT in device px for the displaying window (D-P1.A).
Used to baseline-align org-air svg overlays with the text line so an image
clamped to the line height never grows the row.  Falls back to ~80% of the
line height when `font-info' is unavailable."
  (let* ((win (get-buffer-window (current-buffer) t))
         (frame (and win (window-frame win)))
         (info (ignore-errors (font-info (face-font 'default frame)))))
    (if (and (vectorp info) (> (length info) 8) (numberp (aref info 8)))
        (aref info 8)
      (round (* 0.8 (or org-air-view--pill-char-h (frame-char-height)))))))

(defun org-air-view--svg-line-image (svg width height)
  "Return an `svg-image' of SVG at WIDTH x HEIGHT clamped to the line box (D-P1.A).
Displays SVG with an INTEGER :ascent derived from the font ascent ratio
\(NOT `:ascent center'), so a HEIGHT = line-height image occupies exactly
the text line box and NEVER grows the row.  Because no org-air svg row
grows, the `│' divider glyph fills every row at `line-spacing' 0 and the
divider reads solid.  This is the shared wrapper for EVERY org-air svg
overlay (pill / file-icon / priority-square / divider) so none can grow a
row."
  (let* ((asc (org-air-view--font-ascent))
         (ascent (if (and (numberp asc) (> height 0))
                     (max 0 (min 100 (round (* 100 (/ (float asc) height)))))
                   'center)))
    (svg-image svg :ascent ascent :width width :height height)))

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

(cl-defun org-air-view--svg-pillify (text face &key (align 'center) border-color
                                         label font-weight)
  "Return TEXT carrying a rounded svg-pill `display' overlay (C2/D-P1).
ALIGN places the label inside the box: `center' (default) or `right'
\(D-P1 `org-air-date-pill-align').  LABEL overrides the DRAWN glyph string
\(default: TEXT trimmed) so a chip can show a glyph DIFFERENT from its text-
cell contract (R25-2: the project state badge keeps the `[D]' cell text for
the byte/pixel-lock box but draws just `D').  FONT-WEIGHT (e.g. `bold') is
passed to `svg-text' (default: normal).  BORDER-COLOR overrides the neutral
`org-air-pill-border' for this pill (D-P4: the priority badge passes its
level colour; tags/dates pass nil = the neutral border).  When non-nil
the border draws a touch stronger (full opacity) since it is salient.
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
                   ;; R25-2: LABEL overrides the drawn glyph (default = TEXT),
                   ;; so a chip can draw a single letter while its cell text
                   ;; (the pixel-lock box) stays the 3-col `[D]' token.
                   (label (string-trim (or label text)))
                   (fg (or (face-foreground face nil t) "gray"))
                   (border (or border-color
                               org-air-pill-border
                               (face-foreground 'org-air-face-faded nil t)
                               "gray"))
                   (alpha (max 0.0 (min 1.0 (float org-air-pill-fill-alpha))))
                   (desired-fs (max 7 (round (* ch org-air-pill-font-scale))))
                   ;; R18 D-P1a: defer the (expensive) label measurement into
                   ;; a thunk so it runs ONLY on a cache miss; width-fit never
                   ;; exceeds inner-w (the clip fix).
                   (fit-font-size
                    (lambda ()
                      (let ((natural-w (org-air-view--pill-label-width
                                        label desired-fs cw ch)))
                        (if (and (> natural-w inner-w) (> natural-w 0))
                            (max 7 (floor (* desired-fs
                                             (/ inner-w (float natural-w)))))
                          desired-fs)))))
              (if (and (not (org-air-view--string-pixel-width-available-p))
                       (> (org-air-view--pill-label-width
                           label (funcall fit-font-size) cw ch)
                          inner-w))
                  ;; D-P1.FIT cannot guarantee a fit (no string-pixel-width
                  ;; AND the estimate already overruns) -> mandatory text
                  ;; fallback (plain padded coloured label, no pill).
                  text
                ;; R18 D-P1a: build the pixel-identical pill image ONCE and
                ;; share it; `propertize' returns a FRESH string so a caller
                ;; that later adds row props never mutates the shared image.
                ;; The salient-border flag joins the key because `stroke-op'
                ;; depends on whether an explicit BORDER-COLOR was passed,
                ;; not only on the resolved `border' string.
                (let ((image
                       (org-air-view--svg-image-cached
                        ;; R25-2: the cache key includes the LABEL override,
                        ;; the weight, and the effective pad/scale so a badge
                        ;; image is never confused with a same-text board pill.
                        (list 'pill text label font-weight fg border align cw ch
                              (and border-color t)
                              org-air-pill-pad-cols org-air-pill-font-scale
                              org-air-view--pill-style-sig)
                        (lambda ()
                          (let* ((font-size (funcall fit-font-size))
                                 (svg (svg-create box-w h))
                                 ;; D-P4: a salient priority border draws full
                                 ;; strength; the neutral tag/date border stays
                                 ;; a hairline.
                                 (stroke-op
                                  (if border-color 1.0
                                    (max 0.0 (min 1.0
                                                  (float org-air-pill-border-opacity)))))
                                 ;; D-P3: draw the capsule `org-air-pill-vinset'
                                 ;; px shorter top+bottom, vertically centred,
                                 ;; so the pill breathes INSIDE its cell while
                                 ;; the cell grid (and the `│' divider glyph)
                                 ;; stays continuous.
                                 (vin (max 0.0 (min (/ (- h 2.0) 2.0)
                                                    (float org-air-pill-vinset))))
                                 (box-h (max 1.0 (- h (* 2 vin)))))
                            (svg-rectangle svg 0.5 (+ vin 0.5)
                                           (max 0 (- box-w 1.0)) (max 0 (- box-h 1.0))
                                           :rx radius :ry radius
                                           :fill (if (> alpha 0) fg "none")
                                           :fill-opacity (if (> alpha 0) alpha 0)
                                           :stroke border :stroke-width 1
                                           ;; D-P2 #1: hairline border at
                                           ;; reduced opacity.
                                           :stroke-opacity stroke-op)
                            ;; D-P1: label placement — centred or right-hugged.
                            (if (eq align 'right)
                                (svg-text svg label
                                          :x (- box-w (* pad cw))
                                          :y (round (* ch 0.72))
                                          :text-anchor "end"
                                          :fill fg
                                          :font-size font-size
                                          :font-weight (or font-weight 'normal))
                              (svg-text svg label
                                        :x (/ box-w 2.0)
                                        :y (round (* ch 0.72))
                                        :text-anchor "middle"
                                        :fill fg
                                        :font-size font-size
                                        :font-weight (or font-weight 'normal)))
                            ;; Lock the image to the exact cell box (C2) with
                            ;; the D-P1.A integer-ascent clamp so it is exactly
                            ;; one line tall and never grows the row.
                            (org-air-view--svg-line-image svg box-w h))))))
                  (propertize text 'display image)))))
          text))))

(defcustom org-air-origin-icon-svg t
  "When non-nil, overlay the origin cell with a drawn document icon (D-P2).
On a capable GUI the 1-col origin glyph (`▤') carries an svg `display'
of a crisp document — a rounded rectangle with a folded top-right corner,
box-fit to the single reserved column (1 × char-px, V6 pixel-lock holds)
and stroked in `org-air-face-group' (the origin face).  Turning it off (or
on a TTY) shows the plain `▤' glyph.  The svg is a non-byte overlay over
the unchanged glyph cell, so fixtures assert `▤' either way."
  :type 'boolean
  :group 'org-air)

(defun org-air-view--svg-file-icon (glyph)
  "Return GLYPH carrying a drawn document-icon `display' overlay (D-P2).
The icon is box-fit to GLYPH's single text cell (1 × char-px), a rounded
rectangle with a folded top-right corner, stroked in `org-air-face-group'.
Returns GLYPH unchanged when the icon is off or svg is unavailable (the
glyph is then the mandatory fallback)."
  (if (or (not org-air-origin-icon-svg)
          (not (org-air-view--svg-available-p)))
      glyph
    (or (ignore-errors
          (let* ((cw (or org-air-view--pill-char-w (frame-char-width)))
                 (ch (or org-air-view--pill-char-h (frame-char-height)))
                 (color (or (face-foreground 'org-air-face-group nil t) "gray"))
                 ;; R18 D-P1a: one document icon per (glyph, colour, cw, ch);
                 ;; one per row, so the highest-count overlay — build it once.
                 (image
                  (org-air-view--svg-image-cached
                   (list 'icon glyph color cw ch)
                   (lambda ()
                     (let* ((svg (svg-create cw ch))
                            ;; margins so the page sits centred in the cell.
                            (mx (max 1.0 (* cw 0.18)))
                            (my (max 1.0 (* ch 0.18)))
                            (x0 mx) (y0 my)
                            (x1 (- cw mx)) (y1 (- ch my))
                            ;; fold size at the top-right corner.
                            (fold (max 2.0 (* (- x1 x0) 0.34))))
                       ;; page outline with a cut top-right corner (the fold).
                       (svg-polygon svg (list (cons x0 y0)
                                              (cons (- x1 fold) y0)
                                              (cons x1 (+ y0 fold))
                                              (cons x1 y1)
                                              (cons x0 y1))
                                    :fill "none" :stroke color :stroke-width 1
                                    :stroke-linejoin "round")
                       ;; the folded corner triangle.
                       (svg-polygon svg (list (cons (- x1 fold) y0)
                                              (cons (- x1 fold) (+ y0 fold))
                                              (cons x1 (+ y0 fold)))
                                    :fill "none" :stroke color :stroke-width 1
                                    :stroke-linejoin "round")
                       (org-air-view--svg-line-image svg cw ch))))))
            (propertize glyph 'display image)))
        glyph)))

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

(defun org-air-view--origin-capped (item)
  "Return ITEM's origin TEXT capped to `org-air-origin-max-width' (R17).
The 2-col `▤ ' lead is reserved separately (`org-air-view--item-origin-raw'),
so the text budget is the cap minus those 2 columns; a longer text
truncates with the ellipsis glyph.  V6: only the TEXT truncates -- the
glyph cell (and its box-fit svg overlay) is untouched."
  (let* ((text (org-air-view--origin item))
         (budget (max 1 (- org-air-origin-max-width 2))))
    (if (<= (string-width text) budget)
        text
      (truncate-string-to-width text budget nil nil
                                (org-air-view--glyph 'more)))))

(defun org-air-view--item-origin-raw (item)
  "Return the origin breadcrumb \"▤ FILE\" for ITEM (unfaced).
D-P2: the leading origin glyph carries the drawn document-icon svg overlay
via `org-air-view--svg-file-icon'; the glyph TEXT is unchanged so the byte
width/cell holds.  R17: the origin TEXT is capped via
`org-air-view--origin-capped' so a long Denote slug cannot grow the cell."
  (concat (org-air-view--svg-file-icon (org-air-view--glyph 'origin))
          " " (org-air-view--origin-capped item)))

(defun org-air-view--timestamp-repeater (timestamp)
  "Return (TYPE VALUE UNIT) of TIMESTAMP's Org repeater, or nil (R14 D-P2).
Reads the org-timestamp object's :repeater-type / :repeater-value /
:repeater-unit directly (no marker round-trip, no recomputation).  These
props are present on the `org-timestamp-from-string' object stored in
`org-air-item-scheduled' / `-deadline'."
  (when timestamp
    (let ((type (org-element-property :repeater-type timestamp))
          (value (org-element-property :repeater-value timestamp))
          (unit (org-element-property :repeater-unit timestamp)))
      (when (and type value unit (not (eq type 'none)))
        (list type value unit)))))

(defun org-air-view--repeat-string (timestamp)
  "Return a human repeat rule for TIMESTAMP like \"every 1w\", or nil (R14 D-P2).
The cookie kind (.+/++/+) is irrelevant to the displayed rule, so only the
N and the unit (day->d / week->w / month->m / year->y / hour->h) show."
  (when-let* ((rep (org-air-view--timestamp-repeater timestamp)))
    (let ((value (nth 1 rep))
          (unit (nth 2 rep)))
      (format "every %d%s" value
              (pcase unit
                ('day "d") ('week "w") ('month "m") ('year "y") ('hour "h")
                (_ (substring (symbol-name unit) 0 1)))))))

(defun org-air-view--item-repeat-timestamp (item)
  "Return ITEM's effective (deadline-or-scheduled) timestamp IF it repeats (D-P2)."
  (let ((deadline (org-air-item-deadline item))
        (scheduled (org-air-item-scheduled item)))
    (cond
     ((and deadline (org-air-view--timestamp-repeater deadline)) deadline)
     ((and scheduled (org-air-view--timestamp-repeater scheduled)) scheduled)
     (t nil))))

(defun org-air-view--item-repeat-marker (item)
  "Return the date-cluster repeat marker for ITEM (R14 D-P2), or \"\".
A leading space + the `repeat' glyph (TTY ~), faded, when the effective
date carries an Org repeater; empty otherwise."
  (if (org-air-view--item-repeat-timestamp item)
      (concat " " (propertize (org-air-view--glyph 'repeat)
                              'face 'org-air-face-faded))
    ""))

(defun org-air-view--inspector-repeat-line (item inset)
  "Return the inspector Repeat line for ITEM at INSET, or nil (R14 D-P2).
\"every 1w -> next Mon 22 Jun\": the repeating timestamp's own stored date
IS the next occurrence by Org convention, so it is read directly (no
repeat math reimplemented)."
  (when-let* ((ts (org-air-view--item-repeat-timestamp item))
              (rule (org-air-view--repeat-string ts))
              (time (org-air-view--timestamp-time ts)))
    (org-air-view--inspector-kv
     "Repeat"
     (concat (propertize rule 'face 'org-air-face-salient)
             " " (org-air-view--glyph 'arrow) " next "
             (propertize (format-time-string "%a %d %b" time)
                         'face 'org-air-face-salient))
     inset)))

(defun org-air-view--item-date-text (item bucket)
  "Return the propertized date text for ITEM in BUCKET (V6/R10), or nil.
The date is coloured TEXT in its semantic face; the GUI pill (V3) is a
non-byte overlay over this same text.  R26-6: the old \"· r to file\"
Inbox nudge is GONE from rows — it cost 12 columns per dated inbox row,
read as a path fragment, and broke that row's V6 tag/origin columns via
the local date-cell expansion; discovery lives in `?' help + the Actions
legend (`r' `org-air-refile-item' stays bound), the single teaching
surface."
  (let ((date (org-air-view--date-label item bucket)))
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
                ;; R14 D-P2: the repeat marker sits AFTER the date pill,
                ;; within the date cell (the cell is widened by
                ;; `org-air-view--meta-date-repeat' so the column stays
                ;; aligned).  R26-6: nothing follows it — the date cell is
                ;; exactly date + optional repeat marker.
                (org-air-view--item-repeat-marker item))))))

(defun org-air-view--compute-meta-widths (items width)
  "Set the V6 metadata column widths over the DISPLAYED ITEMS at WIDTH.
Walks the same section buckets the pane renders and records the widest
date label (bare, no Inbox nudge), tag string and origin so date / tags /
origin each occupy a fixed-width column down the whole list and line up
vertically.  The date floor is `org-air-date-column'.

R20-6: measures only the rows a section actually renders via
`org-air-view--displayed-items-for-bucket' (the per-bucket `seq-take' subset,
plus any expanded section), not every member.  The contract is per-render
alignment of WHAT IS SHOWN, so the columns still align; the cost drops from
O(N) over every item to O(shown) over ~35 rows -- the dominant warm re-render
win.  Widths can only be <= the all-items result (tighter where a hidden item
was widest), so the common case is byte-identical.

R17: the origin column is capped at `org-air-origin-max-width' (it is
already capped per-item at the source, this is belt-and-braces), then a
width-aware fit pass reclaims columns for the flex title so it keeps at
least `org-air-title-min-width': the origin shrinks toward
`org-air-origin-min' first, then tags toward a 1-col floor; the date
column is held.  This INVERTS the never-wired D2 origin-protected
priority -- the title is the row's primary identity and is protected
first."
  (let ((dw org-air-date-column) (tw 0) (ow 0) (rep 0) (tw-todo 0))
    (dolist (descriptor org-air-view--sections)
      (let* ((bucket (car descriptor))
             (bucket-items (org-air-view--displayed-items-for-bucket bucket items)))
        (dolist (item bucket-items)
          (when (org-air-view--item-repeat-timestamp item) (setq rep 2))
          (setq tw-todo (max tw-todo
                             (string-width (or (org-air-item-todo item) ""))))
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
    ;; R17 piece C: the per-item origin TEXT is already capped at the
    ;; source (`org-air-view--origin-capped'), so OW is inherently <= the
    ;; cap; clamp anyway (belt-and-braces -- width and rendered cell agree).
    (setq ow (min ow org-air-origin-max-width))
    ;; R17 piece D: title-min budget.  The title's left edge is board-wide
    ;; constant: margin+indent + the reserved keyword cell (+1 sep) + the
    ;; fixed priority slot (`square style only).  Mirror `insert-row''s
    ;; arithmetic EXACTLY (gap=2; cluster cells joined by single spaces).
    (let* ((gap 2)
           (left-reserve (+ (string-width (org-air-view--item-margin))
                            (if (> tw-todo 0) (1+ tw-todo) 0)
                            (if (eq org-air-priority-style 'square) 2 0)))
           (dcol (+ dw rep))
           ;; present cluster cells (width>0) -> one separator between each.
           (cluster (lambda (o)
                      (let ((cells (delq nil (list (and (> dcol 0) dcol)
                                                   (and (> tw   0) tw)
                                                   (and (> o     0) o)))))
                        (+ (apply #'+ cells) (max 0 (1- (length cells)))))))
           (budget (lambda (o)
                     (- width left-reserve gap (funcall cluster o)))))
      ;; 1) shrink the origin toward its floor until the title reaches min.
      (while (and (> ow org-air-origin-min)
                  (< (funcall budget ow) org-air-title-min-width))
        (setq ow (1- ow)))
      ;; 2) still starved? shrink tags toward 0 (keep a 1-col floor when any
      ;;    item carries tags, so the tag column doesn't disappear silently).
      (let ((tw-floor (if (> tw 0) 1 0)))
        (while (and (> tw tw-floor)
                    (< (funcall budget ow) org-air-title-min-width))
          (setq tw (1- tw))))
      ;; 3) date is held (small, uniform, semantic).  If the line is so
      ;;    narrow that even origin+tags at floor cannot fund the title min
      ;;    (board-only tier), the title floor in `insert-row' (max 1) takes
      ;;    over -- never crash, never overflow.
      )
    (setq org-air-view--meta-date-w dw
          org-air-view--meta-tags-w tw
          org-air-view--meta-origin-w ow
          org-air-view--meta-date-repeat rep
          org-air-view--meta-todo-w tw-todo)))

(defun org-air-view--svg-keyword-badge (text face)
  "Return TEXT carrying a small coloured keyword/state svg chip (R21-4).
Reuses `org-air-view--svg-pillify' with FACE's foreground as the salient
\(full-strength) border, so the chip reads as a coloured BADGE -- distinct
from the calm monochrome tag/date pills.  Shared by the board keyword
cell and the project state cell.  Returns TEXT unchanged (the plain
coloured keyword/token text) when `org-air-keyword-style' is `text', when
svg is unavailable, or when TEXT is blank -- the mandatory fallback, so
the byte/TTY layer always keeps the keyword text."
  (if (or (not (eq org-air-keyword-style 'badge))
          (not (org-air-view--svg-available-p))
          (string-empty-p (string-trim text)))
      text
    (let ((color (face-foreground face nil t)))
      (org-air-view--svg-pillify text face :border-color color))))

(defun org-air-view--todo-cell (todo width)
  "Return a fixed-width reserved TODO-keyword cell (R15 D-P1).
WIDTH is the board-wide widest keyword (`org-air-view--meta-todo-w').
When WIDTH is 0 no rendered item has a keyword, so return an empty
string (no wasted column).  Otherwise return TODO in its todo-face (or
WIDTH blanks when absent), left-justified and padded to WIDTH, plus a
single trailing space separator -- so every row contributes WIDTH+1
columns here and all titles share one left edge.  R21-4: when TODO is
present, overlay the shared svg keyword badge on the (unchanged) padded
keyword text -- a `display' overlay, so the byte/TTY layer is identical."
  (if (<= width 0)
      ""
    (concat (org-air-view--pad-to
             (if todo
                 (org-air-view--svg-keyword-badge
                  (propertize todo 'face (org-air-view--todo-face todo))
                  (org-air-view--todo-face todo))
               "")
             width)
            " ")))

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
         ;; R23-1 (defensive): normalise the incoming title to PLAIN text
         ;; before any width math, so the row is rendered purely via org-air's
         ;; own font-lock-face/propertize and never inherits a caller's
         ;; face/display/org property (e.g. the `org-level-1' that
         ;; `org-get-heading' leaks from a fontified buffer post-refile).  The
         ;; R21-2 step then re-adds only org-air's own title mark.
         (title (substring-no-properties (or title "")))
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
                        ;; R17: the fit pass can shrink OCOL below a capped
                        ;; origin's width (e.g. 13 at W80); truncate OT to
                        ;; OCOL FIRST so a wider text can't overflow the
                        ;; cell.  At the wide tiers OT already fits OCOL
                        ;; (the board-wide max), so this is a no-op there
                        ;; and the wide goldens stay byte-identical.
                        (let* ((ot (truncate-string-to-width
                                    (or origin-text "") ocol nil nil
                                    (org-air-view--glyph 'more)))
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
         ;; R21-2: mark the TITLE's first glyph so motion/open can land
         ;; point on the title (not the keyword/priority prefix).  A text
         ;; property, not visible text — byte goldens are byte-identical.
         ;; Copy first so the source item/doc title string is not mutated.
         (title (if (> (length title) 0)
                    (let ((tt (copy-sequence title)))
                      (put-text-property 0 1 'org-air-row-title t tt)
                      tt)
                  title))
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
         ;; R15 D-P1: reserve a FIXED keyword cell so keyword-less rows
         ;; render blank there and ALL titles share one left edge.  The
         ;; board-wide width comes from `org-air-view--meta-todo-w'; in a
         ;; single-row pane (R6 day view, meta widths unset) fall back to
         ;; this row's own keyword width -- it is the only row, so there is
         ;; no cross-row alignment to honour.
         (todo-w (or org-air-view--meta-todo-w
                     (string-width (or todo ""))))
         (prefix (concat (org-air-view--item-margin)
                         (org-air-view--todo-cell todo todo-w)
                         ;; R13 D-P2: `square style emits a FIXED 2-col slot
                         ;; on EVERY row (square or blank) so titles align;
                         ;; `badge/`text keep the conditional `[#A]' token.
                         (if (eq org-air-priority-style 'square)
                             (org-air-view--priority-slot priority)
                           (when (and priority (member priority org-air-priority-show))
                             (concat (org-air-view--priority-token priority) " ")))))
         (tags (org-air-item-tags item))
         (n-tags (length tags))
         (tagstr (org-air-view--item-tagstr
                  tags (min org-air-tags-inline-max n-tags) n-tags))
         (origin-raw (org-air-view--item-origin-raw item))
         ;; V6 fixed column widths (computed over the whole list; fall back
         ;; to this single row when unset, e.g. the day pane).
         ;; R14 D-P2: reserve the repeat-marker columns so a repeating
         ;; row's `␣↻' never shoves the tags column right (V6 alignment).
         (dcol (if omit-date 0
                 (+ (or org-air-view--meta-date-w org-air-date-column)
                    (or org-air-view--meta-date-repeat 0))))
         (tcol (or org-air-view--meta-tags-w (string-width tagstr)))
         (ocol (or org-air-view--meta-origin-w (string-width origin-raw))))
    (org-air-view--insert-row
     :prefix prefix
     :title (org-air-item-title item)
     :date-text date-text
     :tags tagstr
     :origin-text origin-raw
     ;; R22-7: the origin reads at AA (mid-tier) instead of sub-AA faded.
     :origin-face 'org-air-face-origin
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

;;;; ---------------------------------------------------------------------
;;;; Shared sort core (R22-3) — one cycle/reverse pair + indicator for BOTH
;;;; the board and the project; per-mode the SPEC (key list + refresh fn) is
;;;; seeded into buffer-locals so the commands never fork.
;;;; ---------------------------------------------------------------------

(defvar-local org-air-view--sort-key nil
  "Active per-buffer sort KEY for this view (R22-3).
The board seeds it from `org-air-sort-key' (date/priority/title/recency);
the project from `org-air-project-sort-key' (name/created/updated).")
(defvar-local org-air-view--sort-direction nil
  "Active per-buffer sort DIRECTION (`ascending'/`descending') (R22-3).")
(defvar-local org-air-view--sort-keys nil
  "Ordered list of sort keys for THIS view (the board / the project sets it).")
(defvar-local org-air-view--sort-refresh nil
  "The per-mode refresh fn the sort commands call after changing key/dir (R22-3).")

(defun org-air-view-sort-cycle ()
  "Cycle to the next sort key for this view and refresh (R22-3).
Bound to `o' in BOTH the board and the project via `org-air-view-core-map'."
  (interactive)
  (let* ((keys org-air-view--sort-keys)
         (cur  (or org-air-view--sort-key (car keys)))
         (next (or (cadr (memq cur keys)) (car keys))))
    (setq-local org-air-view--sort-key next)
    (when org-air-view--sort-refresh (funcall org-air-view--sort-refresh))
    (message "org-air: sort by %s" next)))

(defun org-air-view-sort-reverse ()
  "Toggle the sort direction for this view and refresh (R22-3).
Bound to `O' in BOTH views via `org-air-view-core-map'."
  (interactive)
  (setq-local org-air-view--sort-direction
              (if (eq org-air-view--sort-direction 'descending)
                  'ascending 'descending))
  (when org-air-view--sort-refresh (funcall org-air-view--sort-refresh))
  (message "org-air: sort %s" org-air-view--sort-direction))

(defun org-air-view--sort-indicator-text (key dir &optional active)
  "Return the shared `\u2195 <key> <dir>' badge text for KEY + DIR (R22-3).
Lifted from the project's builder, parameterised on key+dir so the board
and the project show one indicator.  Plain text (svg-free) + quiet faces.
R27-3: when ACTIVE is non-nil (the caller's sort differs from its view's
default) the marker glyph, the key name AND the direction arrow all take
the high-contrast bold `org-air-face-sort-active' so the state is clearly
stated at the header level; nil keeps today's quiet faces (byte- and
face-identical, so the default goldens hold)."
  (let* ((mk (org-air-layout-glyph 'sort-key))
         (arrow (org-air-layout-glyph
                 (if (eq dir 'descending) 'sort-desc 'sort-asc))))
    (concat (propertize mk 'face (if active 'org-air-face-sort-active
                                   'org-air-face-faded))
            " "
            (propertize (if (symbolp key) (symbol-name key) (format "%s" key))
                        'face (if active 'org-air-face-sort-active
                                'org-air-face-summary-label))
            " "
            (propertize arrow 'face (if active 'org-air-face-sort-active
                                      'org-air-face-faded)))))

(defun org-air-view--sort-active-key ()
  "Return the board's active sort key, seeding from the defcustom (R22-3)."
  (or org-air-view--sort-key org-air-sort-key))

(defun org-air-view--sort-active-direction ()
  "Return the board's active sort direction, seeding from the defcustom (R22-3)."
  (or org-air-view--sort-direction org-air-sort-direction))

(defun org-air-view--sort-default-p ()
  "Return non-nil when the board sort is the byte-identical default (R22-3)."
  (and (eq (org-air-view--sort-active-key) 'date)
       (eq (org-air-view--sort-active-direction) 'ascending)))

(defun org-air-view--item-priority-rank (item)
  "Return ITEM's numeric priority rank for sorting (R22-3).
Higher = more urgent (#A is highest); a cookie-less item gets the lowest
rank, so it is ordered last."
  (or (org-air-item-priority item) most-negative-fixnum))

(defun org-air-view--item-activity (item)
  "Return ITEM's last-activity time for the `recency' sort (R22-3).
Reuses `org-air-classify--last-activity' (the Stale-bucket signal); a
missing signal is treated as the oldest (epoch)."
  (or (org-air-classify--last-activity item) '(0 0)))

(defun org-air-view--sort-by (items lessp keyfn &optional desc)
  "Return ITEMS stably ordered by KEYFN under LESSP (R22-3).
Equal keys tiebreak by lowercased title then incoming order (byte-stable);
DESC reverses the whole resulting order."
  (let* ((i 0)
         (indexed (mapcar
                   (lambda (it)
                     (prog1 (list i (funcall keyfn it)
                                  (downcase (or (org-air-item-title it) "")) it)
                       (setq i (1+ i))))
                   items))
         (sorted (sort indexed
                       (lambda (a b)
                         (let ((ka (nth 1 a)) (kb (nth 1 b)))
                           (cond
                            ((funcall lessp ka kb) t)
                            ((funcall lessp kb ka) nil)
                            ((string-lessp (nth 2 a) (nth 2 b)) t)
                            ((string-lessp (nth 2 b) (nth 2 a)) nil)
                            (t (< (nth 0 a) (nth 0 b))))))))
         (result (mapcar (lambda (e) (nth 3 e)) sorted)))
    (if desc (nreverse result) result)))

(defun org-air-view--sort-items (items bucket)
  "Order ITEMS within BUCKET by the active board sort key/direction (R22-3).
Buckets are NEVER reordered, only the items inside each.  The default key
`date' reproduces the historical within-bucket order EXACTLY (only the
attention/upcoming buckets were date-sorted; the rest kept query order),
so the board byte goldens are byte-identical by default."
  (let ((key  (org-air-view--sort-active-key))
        (desc (eq (org-air-view--sort-active-direction) 'descending)))
    (pcase key
      ('date
       ;; EXACTLY today's behaviour: only attention/upcoming sort by date,
       ;; the rest keep query order.  `O' reverses the within-bucket order.
       (let ((sorted (if (memq bucket '(attention upcoming))
                         (org-air-view--sort-by-date items)
                       items)))
         (if desc (reverse sorted) sorted)))
      ('priority
       (org-air-view--sort-by items #'> #'org-air-view--item-priority-rank desc))
      ('title
       (org-air-view--sort-by items #'string-lessp
                              (lambda (it) (downcase (or (org-air-item-title it) "")))
                              desc))
      ('recency
       (org-air-view--sort-by items #'time-less-p #'org-air-view--item-activity desc))
      (_ items))))

(defun org-air-view--insert-section (descriptor items)
  "Insert section DESCRIPTOR from ITEMS."
  (pcase-let ((`(,bucket ,title ,empty) descriptor))
    (let* (;; S4: the badge counts exactly what the section renders
           ;; (`items-for-bucket', which keeps inbox items out of the
           ;; other buckets), so badge/summary/body always agree.  `length'
           ;; is order-independent, so the unsorted member list is fine here.
           (count (length (org-air-view--items-for-bucket bucket items)))
           (attentionp (and (> count 0) (memq bucket '(inbox attention))))
           ;; R20-6: the displayed subset (date-sorted, section-capped) is the
           ;; SAME memoised list the meta-width pass measured — one sort+take.
           (visible (org-air-view--displayed-items-for-bucket bucket items)))
      (insert "\n")
      (org-air-view--insert-section-heading bucket title count attentionp)
      (if (> count 0)
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
        (day org-air-view--day)
        ;; R18 D-P1c: share the board's classify cache TABLE OBJECT into the
        ;; temp pane so `puthash' persists back (buffer-local would reset to
        ;; nil here and lose every entry on each render).
        (classify-cache org-air-view--classify-cache)
        (classify-cache-day org-air-view--classify-cache-day)
        ;; R20-6: carry the compute-once partition into the pane temp buffer
        ;; so the section/summary/badge/calendar/meta-width consumers read
        ;; ONE classify pass instead of re-deriving the visible set + per-
        ;; bucket filtering O(N) times each.
        (render-partition org-air-view--render-partition)
        (render-displayed org-air-view--render-displayed)
        ;; R20-5: carry the view descriptor into the rail temp buffer so the
        ;; SHARED rail consults the project's providers (nil for the board).
        (rail-descriptor org-air-view--rail-descriptor)
        ;; R26-7: carry the buffer-local SORT state — the panes compose in
        ;; temp buffers that otherwise fall back to the GLOBAL defaults, so
        ;; `o'/`O' cycled the key while the rendered rows never moved (and
        ;; the banner indicator stayed suppressed).
        (sort-key org-air-view--sort-key)
        (sort-direction org-air-view--sort-direction)
        ;; R26-8: carry the refresh-machine state (and the loading flag) so
        ;; the banner's count slot can show the honest stale/progress marker
        ;; from inside the composing temp buffer.
        (loading org-air-view--loading)
        (refresh-state org-air-view--refresh-state)
        (refresh-queue org-air-view--refresh-queue)
        (refresh-total org-air-view--refresh-total))
    (with-temp-buffer
      (let ((org-air-view--line-width width)
            (org-air-view--items items)
            (org-air-view--items-key items-key)
            (org-air-view--tag-filter tag-filter)
            (org-air-view--scope scope)
            (org-air-view--expanded-sections expanded)
            (org-air-view--cal-month cal-month)
            (org-air-view--day day)
            (org-air-view--classify-cache classify-cache)
            (org-air-view--classify-cache-day classify-cache-day)
            (org-air-view--render-partition render-partition)
            (org-air-view--render-displayed render-displayed)
            (org-air-view--rail-descriptor rail-descriptor)
            (org-air-view--sort-key sort-key)
            (org-air-view--sort-direction sort-direction)
            (org-air-view--loading loading)
            (org-air-view--refresh-state refresh-state)
            (org-air-view--refresh-queue refresh-queue)
            (org-air-view--refresh-total refresh-total))
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

(defun org-air-view--board-only-p (width)
  "Return non-nil when WIDTH is too narrow for the rail (R13 D-P3).
Below `org-air-rail-min-width' the dashboard drops the rail and renders
board-only.  Within `org-air-layout-hysteresis' columns of the threshold
the current `org-air-view--orientation' is kept so dragging across the
boundary does not flap."
  (let ((base (< width org-air-rail-min-width)))
    (cond
     ;; R21-1: side-window orientation hysteresis.  Once the rail is popped
     ;; out and the board renders board-only-width, keep side-window until
     ;; the board width drops more than `org-air-rail-width-hysteresis' below
     ;; the threshold, so a 1-col redisplay/hscroll wobble never flips the
     ;; orientation (and so never fires a motion-time re-render).
     ((eq org-air-view--orientation 'side-window)
      (< width (- org-air-rail-min-width org-air-rail-width-hysteresis)))
     ((and org-air-view--orientation
           (<= (abs (- width org-air-rail-min-width))
               org-air-layout-hysteresis))
      (eq org-air-view--orientation 'board-only))
     (t base))))

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

(defun org-air-view--svg-divider-glyph (glyph)
  "Return GLYPH carrying an svg vertical-bar `display' overlay (D-P3 `svg).
The bar is sized to the cell width and to the line height PLUS the
`org-air-line-spacing' gap so consecutive divider cells abut into a solid
rule even when inter-row spacing is non-zero.  Returns GLYPH unchanged
when svg is unavailable (the glyph itself is the TTY/GUI fallback)."
  (if (not (org-air-view--svg-available-p))
      glyph
    (or (ignore-errors
          (let* ((cw (or org-air-view--pill-char-w (frame-char-width)))
                 (ch (or org-air-view--pill-char-h (frame-char-height)))
                 (sp (cond ((null org-air-line-spacing) 0)
                           ((floatp org-air-line-spacing)
                            (round (* ch org-air-line-spacing)))
                           (t (round org-air-line-spacing))))
                 (h (+ ch (max 0 sp)))
                 (color (or (face-foreground 'org-air-face-pane-border nil t)
                            "gray"))
                 ;; R18 D-P1a: the divider cell repeats on every body row in
                 ;; two-pane/stacked — a pure function of (colour, cw, h); build
                 ;; it once and share the image across all rows/renders.
                 (image
                  (org-air-view--svg-image-cached
                   (list 'divider color cw h)
                   (lambda ()
                     (let ((svg (svg-create cw h)))
                       (svg-rectangle svg (/ (- cw 1.0) 2.0) 0 1.0 h :fill color)
                       (svg-image svg :ascent 'center :width cw :height h))))))
            (propertize glyph 'display image)))
        glyph)))

(defun org-air-view--divider ()
  "Return the pane divider string for the current layout style.
With `org-air-divider-style' = `svg the `│' cell carries an svg bar sized
to the line+spacing so the rule stays solid even with row spacing (D-P3)."
  (if (eq org-air-layout-style 'plain)
      "   "
    (let ((bar (propertize (org-air-view--glyph 'vrule)
                           'face 'org-air-face-pane-border)))
      (concat " " (if (eq org-air-divider-style 'svg)
                      (org-air-view--svg-divider-glyph bar)
                    bar)
              " "))))

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

(defun org-air-view--rail-visible (things)
  "Return THINGS after scope+filter via the rail descriptor (R20-5).
The board's `org-air-view--visible-items' runs when the descriptor's
:visible-fn is absent; a non-board view (the project) provides its own."
  (if-let* ((f (plist-get org-air-view--rail-descriptor :visible-fn)))
      (funcall f things)
    (org-air-view--visible-items things)))

(defun org-air-view--rail-first-thing (things)
  "Return the thing the rail inspector seeds on, via the descriptor (R20-5).
Defaults to the board's `org-air-view--first-actionable-item' over THINGS;
a non-board view may return nil (the inspector is then filled from point by
`org-air-view--setup-inspector')."
  (if (plist-member org-air-view--rail-descriptor :first-thing-fn)
      (funcall (plist-get org-air-view--rail-descriptor :first-thing-fn) things)
    (org-air-view--first-actionable-item things)))

(defun org-air-view--insert-summary (items width)
  "Insert summary block for ITEMS fitted to WIDTH.
R20-5: a non-board view supplies its own Summary via the rail descriptor's
:summary-fn (e.g. the project's per-state counts); the board path runs when
the descriptor is nil so the board summary stays byte-identical."
  (if-let* ((f (plist-get org-air-view--rail-descriptor :summary-fn)))
      (funcall f items width)
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
              "\n")))))

(defun org-air-view--scope-label ()
  "Return active Source/scope display label; a file source carries ⌂ (R22-4)."
  (pcase org-air-view--scope
    (`(:tag ,tag) (concat "#" tag))
    (`(:group ,group) (concat "@" group))
    (`(:file ,file) (concat "⌂ " (file-name-nondirectory file)))
    (_ "all items")))

(defun org-air-view--scope-loaded-count ()
  "Return M = items loaded under the active Source/scope (R22-4).
The dataset size the Source selector returned: items passing the scope
\(all loaded when unscoped, since `--passes-scope-p' is t for every item)."
  (seq-count #'org-air-view--passes-scope-p org-air-view--items))

(defun org-air-view--insert-rail-filters (width)
  "Insert the Filter + Source block fitted to WIDTH (R19-4b/d; R22-4).
Names the two roles UNMISTAKABLY (the user kept reading them as the same):
 - Filter = the LIVE tag LENS (one or more tags, AND/OR); `/' edits, `M-/'
   toggles AND<->OR, `\\' clears.  Empty reads `none' (no dataset claim);
   when a filter narrows it reports `N of M shown'.
 - Source = the structural DATASET selector (all / @group / ⌂ file) with an
   `M loaded' count; `s' changes (re-runs org-ql), `S' clears.  The dataset
   face + the count make it visibly NOT a second filter."
  (when org-air-show-rail-filters
    (let* ((filters (org-air-view--filter-tags))
           (inset (org-air-view--rail-inset-str width))
           (loaded (org-air-view--scope-loaded-count)))
      (org-air-view--rail-header "Filter" width)
      (if filters
          ;; R22-4: the post-filter `N of M shown' count is a BOARD figure
          ;; (`--visible-items' reads item tags); the project filters its
          ;; docs upstream, so skip the narrowing count there (shown=loaded).
          (let ((shown (if (derived-mode-p 'org-air-view-mode)
                           (length (org-air-view--visible-items org-air-view--items))
                         loaded)))
            ;; R18 D-P2.3: join the chips with the combinator word when
            ;; >=2 are active, then teach the toggle AND the clear key.
            (insert inset
                    (mapconcat #'org-air-view--filter-token-label
                               filters
                               (if (> (length filters) 1)
                                   (concat " " (org-air-view--filter-combinator-word) " ")
                                 " "))
                    "  " (org-air-view--glyph 'clear)
                    "\n")
            (insert inset
                    (propertize
                     (concat (format "Match: %s   M-/ toggles · \\ clears"
                                     (org-air-view--filter-combinator-word))
                             ;; R22-4: when the lens removed rows, report it.
                             (when (< shown loaded)
                               (format "   %d of %d shown" shown loaded)))
                     'face 'org-air-face-faded)
                    "\n"))
        ;; R22-4: empty filter reads `none' — the dataset is the Source's job.
        (insert inset (propertize "none" 'face 'org-air-face-faded) "\n"))
      ;; R22-4: the SOURCE/DATASET selector, named + counted, on its own
      ;; labelled line; the dataset name rides the readable origin face so
      ;; it reads as a dataset chip, NOT a faded second filter.
      (org-air-view--rail-header "Source" width)
      (insert inset
              (propertize (org-air-view--scope-label) 'face 'org-air-face-origin)
              (propertize (format " · %d loaded" loaded) 'face 'org-air-face-faded)
              (if org-air-view--scope
                  (propertize "   s changes · S clears" 'face 'org-air-face-faded)
                "")
              "\n"))))

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
the quiet keycap face; the columns do the separating (no dotted prose).
R20-5: a non-board view supplies its own verb rows via the rail
descriptor's :actions-fn; the board path runs when the descriptor is nil."
  (if-let* ((f (plist-get org-air-view--rail-descriptor :actions-fn)))
      (funcall f width)
    (org-air-view--insert-actions-default width)))

(defun org-air-view--insert-actions-default (width)
  "Insert the BOARD Actions block fitted to rail content WIDTH."
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
                     ;; R22-4: `source' (was `scope') — the dataset selector.
                     (org-air-view--verb-cell "s" "source" 0))
             width)
            "\n")
    (insert (org-air-view--pad-to
             (concat inset
                     (org-air-view--verb-cell "g" "refresh" c1) gap
                     (org-air-view--verb-cell (car mid2) (cdr mid2) c2) gap
                     (org-air-view--verb-cell "?" "help" 0))
             width)
            "\n")))

;;;; ---------------------------------------------------------------------
;;;; D-P7 — item inspector (lower-rail metadata for the line at point)
;;;; ---------------------------------------------------------------------

(defcustom org-air-show-inspector t
  "When non-nil, show the rail item inspector (D-P7/D-P1).
The inspector occupies a FIXED reserved mid-rail region between Summary
and Filters (D-P1): it shows metadata for the board line at point,
updating (debounced) as point moves.  The region height is fixed per full
render so Filters/Actions stay pinned to the rail foot and the rail never
desyncs from the item pane; live updates replace ONLY the rail columns,
preserving the item rows beside the region."
  :type 'boolean
  :group 'org-air)

(defcustom org-air-inspector-key-w 8
  "Width in columns of the inspector's fixed key column (D-P7)."
  :type 'integer
  :group 'org-air)

(defcustom org-air-inspector-max-title-lines 4
  "Maximum wrapped title lines shown in the inspector (D-P7/D-P1).
D-P1 raised the default 3 → 4 now the inspector owns the freed mid-rail."
  :type 'integer
  :group 'org-air)

(defcustom org-air-inspector-debounce 0.1
  "Idle seconds before the inspector re-renders after point moves (D-P7).
Coalesces rapid n/p motion so holding a key costs nothing."
  :type 'number
  :group 'org-air)

(defcustom org-air-inspector-empty-hint "Move to an item to inspect."
  "Inspector hint shown when point is not on an item row (D-P7)."
  :type 'string
  :group 'org-air)

(defvar-local org-air-view--inspector-beg nil
  "Marker at the start of the inspector foot band, or nil (D-P7).")
(defvar-local org-air-view--inspector-end nil
  "Marker just past the inspector foot band, or nil (D-P7).")
(defvar-local org-air-view--inspector-item nil
  "The `org-air-item' currently shown in the inspector, or nil (D-P7).")
(defvar-local org-air-view--inspector-geom nil
  "Plist (:item-width :divider :rail-width :region-height) (D-P7/D-P1).
:region-height is the FIXED line-count of the reserved mid-rail region;
live updates pad/truncate the inspector to exactly this many lines and
replace ONLY the rail columns (>= :item-width + the divider).")

(defvar-local org-air-view--inspector-target-buffer nil
  "Buffer whose inspector region this buffer's point drives, or nil (R15 D-P2).
Under `org-air-rail-style' = `side-window' the BOARD buffer sets this to the
`*org-air-rail*' buffer: the board's point selects the item but the
inspector region lives + redraws in the rail buffer.  nil means the
inspector region lives in this same buffer (the inline default).")

(defvar org-air-view--inspector-region-height nil
  "Reserved mid-rail inspector region height for the current render (D-P1).
Set by `org-air-view--insert-rail' (in the rail temp buffer) and read back
by `org-air-view--two-pane-body' to stash into `org-air-view--inspector-geom'.")
(defvar org-air-view--inspector-timer nil
  "Pending debounce timer for the inspector update (D-P7).")

;; R14 D-P1.B: the inspector core is content-agnostic; these buffer-local
;; hooks let a non-board view (the project tree) host the SAME mid-rail
;; inspector.  Their defaults reproduce the board exactly, so the board's
;; inspector output is byte-identical after the generalisation.
(defvar-local org-air-view--inspector-active nil
  "Non-nil in a buffer that hosts the mid-rail inspector (R14 D-P1.B).
Replaces the board-only derived-mode-p guard so the project view's
inspector hook fires in its own mode too.")
(defvar-local org-air-view--inspector-property 'org-air-item
  "Text property identifying the thing at point for the inspector (R14 D-P1.B).
The board uses `org-air-item'; the project view sets `org-air-doc'.")
(defvar-local org-air-view--inspector-fields-function nil
  "Function (THING INSET CONTENT-W NOW) -> inspector body lines (R14 D-P1.B).
Returns the lines AFTER the `Inspector' header (the header + empty-hint
stay in the core).  nil means the board default
`org-air-view--inspector-item-fields'.")
(defvar-local org-air-view--inspector-initial-fn nil
  "Function (THINGS) -> the initial thing to show at render time (R14 D-P1.B).
nil means the board default `org-air-view--first-actionable-item'.")

(defun org-air-view--first-actionable-item (items)
  "Return the first of ITEMS the cursor lands on (section order), or nil.
Mirrors `org-air-view--goto-first-item': the first item of the first
non-empty section (D-P7)."
  (catch 'found
    (dolist (descriptor org-air-view--sections)
      (let ((bucket-items (org-air-view--items-for-bucket (car descriptor) items)))
        (when bucket-items (throw 'found (car bucket-items)))))
    nil))

(defun org-air-view--word-wrap (string width)
  "Greedily wrap STRING (props preserved) into lines of <= WIDTH columns."
  (let ((words (split-string string " " t)) lines (cur ""))
    (dolist (w words)
      (let ((cand (if (string-empty-p cur) w (concat cur " " w))))
        (if (<= (string-width cand) width)
            (setq cur cand)
          (progn (unless (string-empty-p cur) (push cur lines))
                 (setq cur w)))))
    (unless (string-empty-p cur) (push cur lines))
    (or (nreverse lines) (list ""))))

(defun org-air-view--inspector-title-lines (title width maxlines)
  "Return TITLE word-wrapped to WIDTH, capped at MAXLINES with a more glyph."
  (let ((lines (org-air-view--word-wrap title width)))
    (if (<= (length lines) maxlines)
        lines
      (append (butlast (seq-take lines maxlines))
              (list (truncate-string-to-width
                     (concat (nth (1- maxlines) lines) (org-air-view--glyph 'more))
                     width nil nil (org-air-view--glyph 'more)))))))

(defun org-air-view--inspector-kv (key value inset)
  "Return an inspector key/value line: INSET + KEY (label face) + VALUE."
  (let* ((k (truncate-string-to-width key org-air-inspector-key-w))
         (k (concat k (make-string (max 0 (- org-air-inspector-key-w
                                            (string-width k)))
                                   ?\s))))
    (concat inset
            (propertize k 'face 'org-air-face-inspector-label)
            value)))

(defun org-air-view--inspector-relative (time now)
  "Return a relative term for TIME vs NOW: `today' / `in Nd' / `Nd ago'."
  (let ((d (org-air-view--days-between now time)))
    (cond ((= d 0) "today")
          ((> d 0) (format "in %dd" d))
          (t (format "%dd ago" (- d))))))

(defun org-air-view--inspector-date-line (key timestamp face inset now &optional overdue)
  "Return an inspector date line for KEY/TIMESTAMP in FACE at INSET, or nil.
Appends the relative term computed against NOW; with OVERDUE non-nil
appends the deadline mark."
  (when-let* ((time (org-air-view--timestamp-time timestamp)))
    (org-air-view--inspector-kv
     key
     (concat (propertize (format-time-string "%F" time) 'face face)
             "  "
             (propertize (format "(%s)" (org-air-view--inspector-relative time now))
                         'face 'org-air-face-faded)
             (if overdue
                 (concat " " (propertize (if (display-graphic-p) "◆" "!") 'face face))
               ""))
     inset)))

(defun org-air-view--item-created (item)
  "Return ITEM's CREATED property as an Emacs time, or nil (D-P7).
R26-8: hydrates a cache-cold (FILE . POS) cons marker slot on demand, so
the inspector reads the same CREATED for a cache-painted item."
  (when-let* ((src (org-air-classify--item-source item)))
    (ignore-errors
      (with-current-buffer (car src)
        (save-excursion
          (goto-char (cdr src))
          (when-let* ((v (org-entry-get (point) "CREATED")))
            (org-air-view--timestamp-time (org-timestamp-from-string v))))))))

(defun org-air-view--inspector-bucket-name (bucket)
  "Return a compact display name for classify BUCKET (D-P7)."
  (pcase bucket
    ('inbox "Inbox") ('attention "Attention") ('upcoming "Upcoming")
    ('high-priority "High-priority") ('stale "Stale")
    (_ (capitalize (symbol-name bucket)))))

(defun org-air-view--inspector-bucket-line (item inset now)
  "Return the inspector derived-bucket line for ITEM at INSET, or nil.
The classification is computed against NOW (D-P7)."
  (let ((buckets (org-air-view--classify-cached item now)))
    (when buckets
      (let* ((names (mapconcat #'org-air-view--inspector-bucket-name
                               (seq-remove (lambda (b) (eq b 'stale)) buckets)
                               " · "))
             (stale (when (memq 'stale buckets)
                      (when-let* ((act (org-air-classify--last-activity item)))
                        (format "stale %dd" (org-air-view--days-between act now)))))
             (text (string-join (delq nil (list (unless (string-empty-p names) names)
                                                stale))
                                " · ")))
        (unless (string-empty-p text)
          (org-air-view--inspector-kv
           "Bucket" (propertize text 'face 'org-air-face-faded) inset))))))

(defun org-air-view--inspector-item-fields (item inset content-w now)
  "Return the board ITEM's inspector body lines (forward order) (R14 D-P1.B).
The lines AFTER the `Inspector' header: title / state+priority / origin /
tags / breathing / dates / repeat / bucket.  INSET is the spine prefix,
CONTENT-W the wrappable width, NOW the frozen render clock.  Extracted from
`org-air-view--inspector-lines' so the project view can supply its own
fields function while the core stays content-agnostic."
  (let (lines)
    ;; title (wrapped)
    (dolist (tl (org-air-view--inspector-title-lines
                 (or (org-air-item-title item) "") content-w
                 org-air-inspector-max-title-lines))
      (push (concat inset (propertize tl 'face 'org-air-face-title)) lines))
    ;; state + priority
    (let* ((todo (org-air-item-todo item))
           (prio (org-air-view--priority-char item))
           (parts (delq nil
                        (list (when todo
                                (propertize todo 'face
                                            (org-air-view--todo-face todo)))
                              (when prio
                                (org-air-view--priority-token prio))))))
      (when parts (push (concat inset (string-join parts "  ")) lines)))
    ;; origin (group/file) -- R17 D-P2: the leaf is the SAME de-slugged
    ;; Denote title the board shows (`org-air-view--origin'), not the raw
    ;; identifier--slug__tags.org, so the board and inspector agree.  Bind
    ;; `org-air-show-group' nil so this resolves the FILE leaf.  The group
    ;; defaults to the file basename (`org-air-query--group'), so keep the
    ;; `group/' breadcrumb ONLY when it is a real CATEGORY distinct from
    ;; that basename (else it is redundant with -- and, for a Denote file,
    ;; the raw slug of -- the leaf); de-slug a Denote-style group too so a
    ;; long raw basename can never leak.  `org-air-view--pad-to' in
    ;; `org-air-view--inspector-lines' still bounds the line (no overflow).
    (let* ((ifile (org-air-item-file item))
           (origin (let ((org-air-show-group nil))
                     (org-air-view--origin item)))
           (grp (org-air-item-group item))
           (grp (and grp ifile
                     (not (equal grp (file-name-base ifile)))
                     (or (org-air-view--denote-title grp) grp)))
           (org (concat (org-air-view--glyph 'origin) " "
                        (if grp (concat grp "/") "") (or origin ""))))
      (push (concat inset
                    (org-air-view--svg-file-icon (org-air-view--glyph 'origin))
                    (propertize (substring org (length (org-air-view--glyph 'origin)))
                                'face 'org-air-face-group))
            lines))
    ;; tags (all, accent, wrapped)
    (let ((tagstr (mapconcat
                   (lambda (tg) (propertize (concat "#" tg)
                                            'face (org-air-faces-tag-face tg)))
                   (org-air-item-tags item) " ")))
      (unless (string-empty-p tagstr)
        (dolist (tl (org-air-view--word-wrap tagstr content-w))
          (push (concat inset tl) lines))))
    ;; D-P1 breathing: a blank line separates the identity group
    ;; (title/state/origin/tags) from the dates group.
    (push "" lines)
    ;; dates
    (let* ((deadline (org-air-item-deadline item))
           (dl-time (org-air-view--timestamp-time deadline))
           (overdue (and dl-time (> (org-air-view--days-between dl-time now) 0))))
      (dolist (ln (delq nil
                        (list
                         (org-air-view--inspector-date-line
                          "Sched" (org-air-item-scheduled item)
                          'org-air-face-salient inset now)
                         (org-air-view--inspector-date-line
                          "Deadln" deadline
                          (if overdue 'org-air-face-critical 'org-air-face-popout)
                          inset now overdue)
                         (when-let* ((c (org-air-view--item-created item)))
                           (org-air-view--inspector-kv
                            "Created"
                            (concat (propertize (format-time-string "%F" c)
                                                'face 'org-air-face-faded)
                                    "  "
                                    (propertize
                                     (format "(%s)"
                                             (org-air-view--inspector-relative c now))
                                     'face 'org-air-face-faded))
                            inset))
                         (org-air-view--inspector-date-line
                          "Closed" (org-air-item-closed item)
                          'org-air-face-faded inset now))))
        (push ln lines)))
    ;; D-P2 repeat line (when the effective date carries an Org repeater)
    (let ((rep (org-air-view--inspector-repeat-line item inset)))
      (when rep (push rep lines)))
    ;; derived bucket (D-P1 breathing: a blank line before it)
    (let ((b (org-air-view--inspector-bucket-line item inset now)))
      (when b (push "" lines) (push b lines)))
    (nreverse lines)))

(defun org-air-view--inspector-lines (thing width)
  "Return the inspector block as a list of lines, each WIDTH wide, for THING.
THING nil yields the header + the empty hint.  The body comes from
`org-air-view--inspector-fields-function' (buffer-local; default the board
`org-air-view--inspector-item-fields'), so a non-board view supplies its
own fields while the header/empty-hint/padding stay in the core (R14
D-P1.B).  Every line is tagged with the `org-air-inspector' text property
so the region can be re-found (D-P7)."
  (let* ((inset (org-air-view--rail-inset-str width))
         (content-w (max 1 (- width (string-width inset))))
         (now (current-time))
         (header (org-air-layout-rail-header-string "Inspector" width))
         (body (if (null thing)
                   (list (concat inset (propertize org-air-inspector-empty-hint
                                                   'face 'org-air-face-faded)))
                 (funcall (or org-air-view--inspector-fields-function
                              #'org-air-view--inspector-item-fields)
                          thing inset content-w now))))
    (mapcar (lambda (l)
              (propertize (org-air-view--pad-to l width) 'org-air-inspector t))
            (cons header body))))

(defun org-air-view--inspector-rail-lines (item width height)
  "Return exactly HEIGHT rail-cell lines (each WIDTH wide) for ITEM (D-P1).
The inspector content (header + fields) is top-aligned; the remainder is
padded with blank `org-air-inspector'-tagged rail lines (the blank tail IS
the breathing the spec asks for); content is truncated to HEIGHT if it
overflows.  Every line carries the `org-air-inspector' property so the
fixed reserved region can be re-found."
  (let* ((content (org-air-view--inspector-lines item width))
         (height (max 1 height))
         (blank (propertize (org-air-view--pad-to "" width) 'org-air-inspector t)))
    (if (>= (length content) height)
        (seq-take content height)
      (append content (make-list (- height (length content)) blank)))))

(defun org-air-view--render-inspector-region (item &optional target)
  "Redraw the inspector region for ITEM in TARGET (default current) (D-P1/R15).
Two paths, chosen by the target buffer's `org-air-view--inspector-geom':
- `:style' = `whole-region' (R15 D-P2 `side-window'): the rail buffer has
  no item columns beside the inspector, so the whole reserved region is
  deleted and the fresh inspector lines (padded/truncated to
  :region-height) are re-inserted.
- otherwise (inline, COLUMN-ONLY): replace ONLY the rail columns
  \(>= :item-width + the divider) of each fixed region line, PRESERVING
  the item-row text beside it; lines are padded/truncated to
  :region-height so the region never changes height."
  (with-current-buffer (or target (current-buffer))
    (when (and org-air-view--inspector-beg org-air-view--inspector-end
               (marker-buffer org-air-view--inspector-beg)
               org-air-view--inspector-geom)
      (let* ((geom org-air-view--inspector-geom)
             (rw (plist-get geom :rail-width))
             (rh (or (plist-get geom :region-height) 1))
             (rail-cells (org-air-view--inspector-rail-lines item rw rh))
             (inhibit-read-only t))
        (if (eq (plist-get geom :style) 'whole-region)
            ;; R15 D-P2: rail buffer — delete the reserved region and
            ;; re-insert the fresh inspector lines (no item columns to
            ;; dodge).  Markers bracket exactly :region-height lines.
            (save-excursion
              (let ((beg (marker-position org-air-view--inspector-beg))
                    (end (marker-position org-air-view--inspector-end)))
                (delete-region beg end)
                (goto-char beg)
                (dolist (cell rail-cells)
                  (insert cell "\n"))))
          ;; inline column-only path (unchanged).
          (let ((iw (plist-get geom :item-width))
                (div (plist-get geom :divider)))
            (save-excursion
              (goto-char org-air-view--inspector-beg)
              (dolist (cell rail-cells)
                (when (< (point) org-air-view--inspector-end)
                  (let* ((beg (line-beginning-position))
                         (end (line-end-position))
                         (cur (buffer-substring beg end))
                         ;; preserve the item-row text in the first IW
                         ;; columns, re-pad to IW, then the divider + the
                         ;; new rail cell.  Keep the full composed width so
                         ;; the line stays exactly the board width — never
                         ;; trim here (a trimmed line would break the
                         ;; width-composition invariant the byte gate
                         ;; asserts).
                         (item-part (org-air-view--pad-to
                                     (truncate-string-to-width cur iw) iw))
                         (new (concat item-part div cell)))
                    (delete-region beg end)
                    (insert new))
                  (forward-line 1))))))))))

(defun org-air-view--setup-inspector ()
  "Bracket the fixed reserved inspector region, sync it to point (D-P1).
Uses the first `org-air-inspector'-tagged line as the region start and the
stashed :region-height as the fixed line-count so the region brackets the
WHOLE reserved block even after the blank tail's trailing spaces are
trimmed."
  (setq org-air-view--inspector-beg nil
        org-air-view--inspector-end nil
        org-air-view--inspector-item nil
        ;; Inline default: the inspector region lives in THIS buffer.  The
        ;; `side-window' path re-points this to the rail buffer afterwards
        ;; via `org-air-rail--setup-inspector' (R15 D-P2).
        org-air-view--inspector-target-buffer nil)
  (when (and org-air-view--inspector-active
             org-air-view--inspector-geom
             (eq org-air-view--orientation 'two-pane))
    (let ((rh (or (plist-get org-air-view--inspector-geom :region-height) 0))
          firstbol)
      (save-excursion
        (goto-char (point-min))
        (while (and (not firstbol) (not (eobp)))
          (when (text-property-any (line-beginning-position) (line-end-position)
                                   'org-air-inspector t)
            (setq firstbol (line-beginning-position)))
          (forward-line 1)))
      (when (and firstbol (> rh 0))
        (setq org-air-view--inspector-beg (copy-marker firstbol nil))
        (save-excursion
          (goto-char firstbol)
          (forward-line rh)
          (setq org-air-view--inspector-end (copy-marker (point) t)))
        ;; sync to the actual item the cursor landed on.
        (org-air-view--maybe-update-inspector t)))))

(defun org-air-view--maybe-update-inspector (&optional force)
  "Redraw the inspector when the thing at point changed (or FORCE) (D-P7/D-P1.B).
The thing is read via the buffer-local `org-air-view--inspector-property'
\(board `org-air-item', project `org-air-doc'); the guard is the
buffer-local `org-air-view--inspector-active' flag so this fires in either
host mode."
  (let ((target (or org-air-view--inspector-target-buffer (current-buffer))))
    (when (and org-air-view--inspector-active
               (buffer-live-p target)
               (buffer-local-value 'org-air-view--inspector-beg target)
               (marker-buffer
                (buffer-local-value 'org-air-view--inspector-beg target)))
      ;; Point lives in THIS buffer (the board); the inspector region lives
      ;; in TARGET (self for inline, the rail buffer for `side-window').
      (let ((thing (get-text-property (point) org-air-view--inspector-property)))
        (when (or force
                  (not (eq thing (buffer-local-value
                                  'org-air-view--inspector-item target))))
          (with-current-buffer target
            (setq org-air-view--inspector-item thing))
          (org-air-view--render-inspector-region thing target))))))

(defun org-air-view--inspector-update-now (buf)
  "Run the inspector update in BUF (debounce-timer callback) (D-P7)."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (org-air-view--maybe-update-inspector))))

(defun org-air-view--inspector-post-command ()
  "Buffer-local `post-command-hook': schedule a debounced update (D-P7/D-P1.B).
P0: a hard noninteractive guard — never arm an idle timer under batch.
Guarded by the buffer-local `org-air-view--inspector-active' flag so it
serves both the board and the project view."
  (when (and (not noninteractive)
             org-air-view--inspector-active)
    (when (timerp org-air-view--inspector-timer)
      (cancel-timer org-air-view--inspector-timer))
    (setq org-air-view--inspector-timer
          (run-with-idle-timer org-air-inspector-debounce nil
                               #'org-air-view--inspector-update-now
                               (current-buffer)))))

(defun org-air-view--insert-rail (items width)
  "Insert the context rail for ITEMS at WIDTH (R19-4c reordered sidebar).
Top→bottom: Calendar, Filter, Summary, Inspector, Actions.  The Filter
block moves UP (between Calendar and Summary) so the active narrowing is
seen BEFORE the Summary counts it explains; the Inspector occupies a FIXED
reserved mid-rail region (top-aligned + blank-padded) computed once per
render, and only ACTIONS is pinned to the rail foot beneath it.  When the
inspector is off the rail falls back to the four-block flow of
Calendar/Filter/Summary/Actions."
  (let ((org-air-view--line-width width))
    (if-let* ((outline-fn (plist-get org-air-view--rail-descriptor
                                     :outline-fn)))
        ;; R26-5: the DOC-CONTEXT rail (a project doc session) — the
        ;; provider inserts the doc meta + outline; only the Actions
        ;; legend follows (same descriptor seam, not a fork).
        (progn
          (funcall outline-fn width)
          (setq org-air-view--inspector-region-height nil)
          (insert "\n")
          (org-air-view--insert-actions width))
      (org-air-view--insert-rail-1 items width))))

(defun org-air-view--insert-rail-1 (items width)
  "Insert the five-block context rail for ITEMS at WIDTH (R19-4c body).
The standard Calendar/Filter/Summary/Inspector/Actions flow, split out of
`org-air-view--insert-rail' so the R26-5 doc-context descriptor can swap
the body without duplicating the foot-pinning arithmetic."
  (progn
    (if-let* ((f (plist-get org-air-view--rail-descriptor :calendar-fn)))
        (funcall f (org-air-view--rail-visible items)
                 width (org-air-view--rail-inset width))
      (org-air-calendar-insert-month org-air-view--cal-month
                                     (org-air-view--visible-items items)
                                     width (org-air-view--rail-inset width)))
    (insert "\n")
    ;; R19-4c: Filter UP, above Summary.
    (org-air-view--insert-rail-filters width)
    (insert "\n")
    (org-air-view--insert-summary items width)
    (insert "\n")
    (if org-air-show-inspector
        ;; R19-4c: Inspector in the fixed reserved middle; ACTIONS alone
        ;; pinned to the foot (Filter left the foot).  `insert-rail' is only
        ;; ever called for the two-pane rail, so the orientation is
        ;; implicitly two-pane here (the rail renders in a temp buffer where
        ;; the buffer-local orientation is not carried).
        (let* ((top-used (count-lines (point-min) (point)))
               ;; foot = the leading blank gap + Actions ONLY now.
               (foot-lines (org-air-view--render-lines
                            width
                            (lambda ()
                              (org-air-view--insert-actions width))))
               (foot-h (1+ (length foot-lines)))
               ;; R20-5: a non-board view sizes the rail to its own pane
               ;; height (the project's doc pane), not the window height.
               (target (or (plist-get org-air-view--rail-descriptor
                                      :rail-target-height)
                           (max 1 (- (org-air-view--render-height)
                                     3 (if org-air-show-footer 2 0)))))
               ;; R26-3: a height-CLAMPED side-window rail lets the
               ;; reserved region shrink to NOTHING (the inspector gives
               ;; way first) so the Actions foot stays on-screen; the
               ;; inline rail keeps its >=1 floor (goldens frozen).
               (clamped (plist-get org-air-view--rail-descriptor
                                   :rail-clamp))
               (avail (- target top-used foot-h))
               (reserved (if clamped avail (max 1 avail))))
          (if (>= reserved 1)
              (progn
                (setq org-air-view--inspector-region-height reserved)
                (dolist (l (org-air-view--inspector-rail-lines
                            (org-air-view--rail-first-thing items)
                            width reserved))
                  (insert l "\n"))
                (insert "\n")
                (org-air-view--insert-actions width))
            ;; No room: drop the inspector region AND its separator blank
            ;; — Actions follows the Summary directly (R26-3 fit rule).
            (setq org-air-view--inspector-region-height nil)
            (org-air-view--insert-actions width)))
      ;; No inspector: the four-block flow (Filter already emitted above).
      (setq org-air-view--inspector-region-height nil)
      ;; D5f: optionally pin Actions to the rail foot.
      (when org-air-rail-anchor-actions
        (let* ((have (count-lines (point-min) (point)))
               (target (max 0 (- (org-air-view--render-height) 3 have 3))))
          (when (> target 0) (insert (make-string target ?\n)))))
      (insert "\n")
      (org-air-view--insert-actions width))))

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
          ;; R19-4c: column order calendar / filter / summary (consistency).
          (dolist (line (org-air-view--compose-columns
                         (list (cons cal-lines col)
                               (cons fil-lines col)
                               (cons sum-lines col))
                         (make-string gutter ?\s)))
            (insert (org-air-view--pad-to line width) "\n")))
      (let ((band (min width col)))
        (dolist (block (list calendar-fn filters-fn summary-fn))
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
         ;; R15 D-P1: unset the board keyword width so each day row falls
         ;; back to its own keyword width (single-row, no cross-row align).
         (org-air-view--meta-todo-w nil)
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
  (org-air-view--compute-meta-widths items width)
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

(defun org-air-view--row-title-pos ()
  "Return the position of the TITLE start on the current row (R21-2).
Finds the `org-air-row-title' mark `org-air-view--insert-row' put on the
title's first glyph; falls back to the row's first visible glyph (section
headings / rows with no title), so headings keep their current landing."
  (save-excursion
    (let* ((bol (line-beginning-position))
           (eol (line-end-position))
           (pos (if (get-text-property bol 'org-air-row-title) bol
                  (next-single-property-change bol 'org-air-row-title nil eol))))
      (if (and pos (get-text-property pos 'org-air-row-title)) pos
        (progn (org-air-view--beginning-of-visible) (point))))))

(defun org-air-view--goto-row-title ()
  "Move point to the TITLE column of the current row (R21-2).
Lands on the title (the row's identity) rather than the leading keyword /
priority cell on motion and open; falls back to first-visible for rows
with no title mark (section headings, the empty board)."
  (goto-char (org-air-view--row-title-pos)))

(defun org-air-view--goto-first-item ()
  "Place point on the first actionable item row (D4 / S5a).
Lands on the first `org-air-item' (first non-empty section), then the
first section heading, then `point-min' for a truly empty board — so
n/p, RET and r work on the first keystroke instead of the banner — and on
the first VISIBLE character of that row, never the indent whitespace."
  (goto-char (or (text-property-not-all (point-min) (point-max) 'org-air-item nil)
                 (text-property-not-all (point-min) (point-max) 'org-air-section nil)
                 (point-min)))
  ;; R21-2: open lands on the first item's TITLE, not its keyword/priority.
  (org-air-view--goto-row-title))

(defun org-air-view--row-property (prop)
  "Return PROP found ANYWHERE on the current row (line), or nil (R22-2).
A row's `org-air-item'/`org-air-doc'/`org-air-marker' covers only the board
content between the leading margin and the rail; the margin, the rail columns
and the trailing pad carry no row property, so a point-ONLY lookup fails there.
Scan the whole line so an action resolves the row regardless of point's column."
  (or (get-text-property (point) prop)
      (let* ((bol (line-beginning-position))
             (eol (line-end-position)))
        (or (get-text-property bol prop)
            (let ((pos (text-property-not-all bol eol prop nil)))
              (and pos (get-text-property pos prop)))))))

(defvar-local org-air-view--pre-command-line nil
  "Line number recorded by `org-air-view--pre-command-snapshot' (R29-2).
The R29-2 line-motion snap gate: `org-air-view--normalize-point' snaps
only when the command moved point to a DIFFERENT line than this snapshot
\(or when no snapshot exists — the command ENTERED this buffer).  Consumed
\(cleared) every post-command.")

(defun org-air-view--pre-command-snapshot ()
  "Record the pre-command line for the R29-2 line-motion snap gate.
Buffer-local `pre-command-hook' in the board and project views."
  (setq org-air-view--pre-command-line (line-number-at-pos)))

(defun org-air-view--dead-zone-p ()
  "Non-nil when point sits in the DEAD ZONE of a row-owning line (R29-2).
On a line that owns a row (`org-air-item'/`org-air-doc' anywhere on it,
via `org-air-view--row-property'), the dead zone is every column BEFORE
the row's title mark (the leading margin, todo cell and priority cell —
the R21-2 `org-air-row-title' position via `org-air-view--row-title-pos')
PLUS any column carrying no row property under point (the two-pane
margin/rail/pad columns — the R22-2 clause, preserved verbatim).  R22-2's
property-only predicate was provably dead under board-only/side-window
composition and on EVERY project doc row, where the row property covers
all columns including column 0.  Lines owning no row (section headings,
banner, blanks) have no dead zone — never touched."
  (and (or (org-air-view--row-property 'org-air-item)
           (org-air-view--row-property 'org-air-doc))
       (or (and (not (get-text-property (point) 'org-air-item))
                (not (get-text-property (point) 'org-air-doc)))
           (< (point) (org-air-view--row-title-pos)))))

(defun org-air-view--normalize-point-now ()
  "Snap point onto the row title when it sits in the dead zone (R29-2).
The gate-free snap: entry/restore tails call this DIRECTLY after placing
point (the R21-1 restore tail, the pane return, the doc-session return)
so a restored dead column is corrected immediately, not on the next
keystroke.  Idempotent: on/after the title — or on a non-row line — it
does nothing, so it composes with R21-1's restored column."
  (when (and (not (window-minibuffer-p))
             (memq major-mode '(org-air-view-mode org-air-project-mode))
             (org-air-view--dead-zone-p))
    (org-air-view--goto-row-title)))

(defun org-air-view--normalize-point ()
  "Snap point onto the row title after a LINE-crossing command (R29-2).
Runs in `post-command-hook'.  Command-agnostic by construction (there is
no command whitelist): the buffer-local snapshot recorded by
`org-air-view--pre-command-snapshot' gates the snap on LINE MOTION — it
fires only when the command moved point to a DIFFERENT line
\(`evil-next-line'/`evil-previous-line'/`evil-goto-line', arrows,
`next-line'/`previous-line', any future package) or when no snapshot
exists (the command ENTERED this buffer, e.g. returning from the pane or
a doc session).  In-row horizontal char motion (h/l and friends, 0/^)
keeps the
same line, so it is NEVER hijacked — even when it moves INTO the gutter
\(evil `h' from the title's first char parks on the todo cell and STAYS).
The snapshot is consumed every post-command."
  (let ((before org-air-view--pre-command-line))
    (setq org-air-view--pre-command-line nil)
    (when (or (null before) (/= before (line-number-at-pos)))
      (org-air-view--normalize-point-now))))

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

;;;; ---------------------------------------------------------------------
;;;; Side-window rail (R15 D-P2 `side-window' rail-style)
;;;;
;;;; With `org-air-rail-style' = `side-window' the rail renders into a
;;;; dedicated `*org-air-rail*' buffer shown in a right side window; the
;;;; divider is a real window border (`window-divider-mode' on GUI,
;;;; `vertical-border' on TTY).  The board buffer renders board-only style
;;;; at full window width with no inline rail or divider.  Phase 1 renders
;;;; the calendar/Summary/Filters/Actions blocks (no inspector yet — the
;;;; rail uses the simple four-block flow; the inspector moves in at
;;;; Phase 2).
;;;; ---------------------------------------------------------------------

(defconst org-air-rail-buffer-name "*org-air-rail*"
  "Name of the `side-window' rail buffer (R15 D-P2).")

(defvar-local org-air-view--rail-buffer nil
  "The live `*org-air-rail*' buffer for this board buffer, or nil (R15 D-P2).")
(defvar-local org-air-rail--board-buffer nil
  "Back-pointer to the `*org-air*' board buffer, set in the rail buffer.
The rail reads the board's items/scope/filter/cal-month through this
pointer (R15 D-P2).")
(defvar-local org-air-rail--window nil
  "Cached side window showing the rail buffer, validated before use (R15 D-P2).")

(defvar org-air-rail-mode-map
  (let ((map (make-sparse-keymap)))
    ;; R16 D-P1: the rail is `other-window'-reachable now; `q' from inside
    ;; it pops the rail back inline on the board (cooperative).  R26-5: in
    ;; a DOC session `q' returns to the tree instead (the dispatcher), RET
    ;; jumps the main window to the outline heading at point, and `|' pops
    ;; the rail in (the legend's `| rail').
    (define-key map (kbd "q") #'org-air-rail-quit)
    (define-key map (kbd "RET") #'org-air-rail-return)
    (define-key map (kbd "|") #'org-air-rail-popin)
    map)
  "Keymap for `org-air-rail-mode' (R16 D-P1 / R26-5).")

(defun org-air-rail-quit ()
  "Quit the rail: back to the tree in a DOC session, else pop inline.
R26-5: when the rail's owner is a doc-session file buffer, `q' is the
session's back verb (the read-only side window is where a plain `q' is
legal — the doc FILE buffer stays editable); otherwise the R16 cooperative
pop-in."
  (interactive)
  (let ((owner org-air-rail--board-buffer))
    (if (and (buffer-live-p owner)
             (local-variable-p 'org-air-project--session-tree owner)
             (buffer-local-value 'org-air-project--session-tree owner)
             (fboundp 'org-air-project-back))
        (with-current-buffer owner (org-air-project-back))
      (org-air-rail-popin))))

(defun org-air-rail-return ()
  "RET inside the rail: jump the MAIN window to the outline row's heading.
R26-5: doc-context outline rows carry `org-air-doc-heading-pos'; RET moves
the session doc's window there and selects it.  A no-op elsewhere."
  (interactive)
  (let ((pos (get-text-property (point) 'org-air-doc-heading-pos))
        (owner org-air-rail--board-buffer))
    (when (and pos (buffer-live-p owner))
      (let ((win (get-buffer-window owner)))
        (when (window-live-p win)
          (set-window-point win pos)
          (select-window win))))))

(define-derived-mode org-air-rail-mode special-mode "org-air-rail"
  "Major mode for the popped-out org-air context rail (R16 D-P1).
A read-only buffer the user owns: it is `other-window'-reachable for
reading/scrolling, and `q' pops the rail back inline on the board."
  (setq-local truncate-lines t)
  (setq-local header-line-format nil)
  ;; R19-4a: the calm, faded nano-style mode-line in the rail too (mirrors
  ;; the board + pane).  `org-air-view--install-modeline' uses `setq-local',
  ;; so this is BUFFER-LOCAL to `*org-air-rail*' — it cannot bleed to other
  ;; side windows; the (separate) window-divider concern is untouched.
  ;; Byte-invisible (the mode-line is not buffer text; the rail goldens are
  ;; the rail BUFFER content, which strips it).
  (org-air-view--install-modeline)
  (setq-local line-spacing org-air-line-spacing)
  (setq-local cursor-type nil)
  (setq-local buffer-read-only t)
  ;; R27-4: the board's evil parity for the rail too — under evil, `q'/`RET'
  ;; /`|' were shadowed (evil-record-macro / evil-ret / evil-goto-column).
  (org-air-view--setup-evil 'org-air-rail-mode org-air-rail-mode-map))

(defun org-air-rail--get-buffer ()
  "Get or create the `*org-air-rail*' buffer in `org-air-rail-mode' (R15 D-P2)."
  (let ((buf (get-buffer-create org-air-rail-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-air-rail-mode)
        (org-air-rail-mode)))
    buf))

(defun org-air-rail--window-cols (&optional total-width)
  "Return the rail side-window column width (R15 D-P2 / R27-1 S1).
`org-air-rail-window-width' wins when set; else derive from the rail
width tier (`org-air-view--rail-width') fed the FRAME's total column
width — an input the rail's own existence cannot change — so every call
site (ensure, render tail, reconcile) derives the SAME cols by
construction and the tier always has a fixpoint.  (Trunk derived the
tier from the MAIN window's width, which depends on the rail: at frame
widths where tier(F-42) /= tier(F-32) the loop had no fixpoint and every
render resized the rail twice.)  TOTAL-WIDTH, when a number, substitutes
for the measured frame width (pure-function tests / batch seams); nil
measures the selected frame."
  (or org-air-rail-window-width
      (org-air-view--rail-width (or total-width (frame-width)))))

(defun org-air-rail--window-params (cols)
  "Return the `display-buffer-in-side-window' alist for a rail of COLS wide.
The side window sits on `org-air-rail-side' and is not deleted by
`delete-other-windows'.  R16 D-P1: NO `no-other-window' — the rail is the
user's own window and stays `other-window'-reachable."
  `((side . ,org-air-rail-side)
    (slot . 0)
    (window-width . ,cols)
    (window-parameters . ((no-delete-other-windows . t)))))

(defun org-air-rail--setup-divider ()
  "Enable `window-divider-mode' with the org-air right divider on GUI (R15 D-P2).
No-op in batch / on TTY, where the inter-window `vertical-border' is a
single continuous column by construction."
  (when (and (not noninteractive) (display-graphic-p))
    (setq window-divider-default-places 'right-only
          window-divider-default-right-width org-air-divider-pixels)
    (window-divider-mode 1)))

(defun org-air-rail--render (board-buffer width &optional height)
  "Render the rail buffer for BOARD-BUFFER at content WIDTH columns (R15 D-P2).
Reads the board's items/scope/filter/cal-month through the back-pointer
and emits the same blocks as the inline rail (calendar, Summary,
Inspector, Filters, Actions) full-width into `*org-air-rail*'.  HEIGHT
in rows sizes the reserved inspector region; when nil the board's render
height is used.  Stashes a whole-region inspector geom so the cross-buffer
inspector update re-finds + re-fills the region (Phase 2)."
  (let* ((rail-buf (or (buffer-local-value 'org-air-view--rail-buffer
                                           board-buffer)
                       (org-air-rail--get-buffer)))
         (state (with-current-buffer board-buffer
                  (list :items org-air-view--items
                        :items-key org-air-view--items-key
                        :tag-filter org-air-view--tag-filter
                        :scope org-air-view--scope
                        :expanded org-air-view--expanded-sections
                        :cal-month org-air-view--cal-month
                        :day org-air-view--day
                        ;; R22-5: carry the host's rail descriptor +
                        ;; inspector config so a POPPED-OUT PROJECT rail
                        ;; renders the project blocks (not the board's),
                        ;; reusing the same side-window primitives.
                        :rail-descriptor org-air-view--rail-descriptor
                        :inspector-property org-air-view--inspector-property
                        :inspector-fields-function
                        org-air-view--inspector-fields-function)))
         (dims (with-current-buffer board-buffer
                 (org-air-view--char-dimensions)))
         (rheight (or height
                      (with-current-buffer board-buffer
                        (org-air-view--render-height)))))
    (with-current-buffer rail-buf
      (setq-local org-air-rail--board-buffer board-buffer)
      (let ((inhibit-read-only t)
            (org-air-view--pill-char-w (car dims))
            (org-air-view--pill-char-h (cdr dims))
            (org-air-view--line-width width)
            (org-air-view-height rheight)
            (org-air-view--items (plist-get state :items))
            (org-air-view--items-key (plist-get state :items-key))
            (org-air-view--tag-filter (plist-get state :tag-filter))
            (org-air-view--scope (plist-get state :scope))
            (org-air-view--expanded-sections (plist-get state :expanded))
            (org-air-view--cal-month (plist-get state :cal-month))
            (org-air-view--day (plist-get state :day))
            (org-air-view--rail-descriptor
             ;; R26-3: a LIVE side window (HEIGHT non-nil) clamps the rail
             ;; to its own body height so the Actions foot is on-screen in
             ;; the side window, not pinned to the host's render height.
             ;; The batch seam path (HEIGHT nil) keeps the host height.
             (let ((d (plist-get state :rail-descriptor)))
               (if height
                   (plist-put (plist-put (copy-sequence d)
                                         :rail-target-height height)
                              :rail-clamp t)
                 d)))
            (org-air-view--inspector-fields-function
             (plist-get state :inspector-fields-function))
            (org-air-view--inspector-region-height nil))
        (erase-buffer)
        ;; Phase 2: the inspector renders here (in the rail buffer); the
        ;; board's point drives its content via the cross-buffer hook.
        (org-air-view--insert-rail (plist-get state :items) width)
        (goto-char (point-max))
        (when (and (bolp) (> (point-max) (point-min)))
          (delete-char -1))
        (goto-char (point-min))
        ;; Whole-region inspector geom: the rail buffer has no item columns
        ;; beside the inspector, so updates delete + re-insert the region.
        (setq-local org-air-view--inspector-geom
                    (when org-air-view--inspector-region-height
                      (list :style 'whole-region
                            :rail-width width
                            :region-height
                            org-air-view--inspector-region-height)))))
    rail-buf))

(defun org-air-rail--setup-inspector (board-buffer)
  "Bracket the rail inspector region + point the board's hook at it (R15 D-P2).
Finds the reserved `org-air-inspector' region in the rail buffer, sets the
rail buffer's markers, then sets the BOARD-BUFFER's inspector target to
the rail buffer and syncs once to the board's item-at-point.  Thereafter the
board's debounced `post-command-hook' redraws the rail inspector."
  (let ((rail-buf (or (buffer-local-value 'org-air-view--rail-buffer
                                          board-buffer)
                      (org-air-rail--get-buffer)))
        ;; R22-5: carry the host's inspector property + fields fn so a
        ;; popped-out PROJECT rail inspects DOCS (`org-air-doc' +
        ;; `org-air-project--inspector-doc-fields'), not board items.
        (host-prop (or (buffer-local-value 'org-air-view--inspector-property
                                           board-buffer)
                       'org-air-item))
        (host-fields (buffer-local-value 'org-air-view--inspector-fields-function
                                         board-buffer)))
    (with-current-buffer rail-buf
      (setq org-air-view--inspector-beg nil
            org-air-view--inspector-end nil
            org-air-view--inspector-item nil)
      (setq-local org-air-view--inspector-active org-air-show-inspector)
      (setq-local org-air-view--inspector-property host-prop)
      (setq-local org-air-view--inspector-fields-function host-fields)
      (when (and org-air-view--inspector-active
                 org-air-view--inspector-geom)
        (let ((rh (or (plist-get org-air-view--inspector-geom :region-height) 0))
              firstbol)
          (save-excursion
            (goto-char (point-min))
            (while (and (not firstbol) (not (eobp)))
              (when (text-property-any (line-beginning-position)
                                       (line-end-position)
                                       'org-air-inspector t)
                (setq firstbol (line-beginning-position)))
              (forward-line 1)))
          (when (and firstbol (> rh 0))
            (setq org-air-view--inspector-beg (copy-marker firstbol nil))
            (save-excursion
              (goto-char firstbol)
              (forward-line rh)
              (setq org-air-view--inspector-end (copy-marker (point) t)))))))
    ;; Wire the board's point-tracking hook to redraw the rail inspector.
    (when (buffer-live-p board-buffer)
      (with-current-buffer board-buffer
        (setq-local org-air-view--inspector-target-buffer rail-buf)
        (org-air-view--maybe-update-inspector t)))))



(defun org-air-rail--ensure-window (board-buffer &optional _width)
  "Ensure the rail side window exists for BOARD-BUFFER (R15 D-P2 / R27-1 S2).
CONVERGENT create-once: when a live rail side window already exists on
the frame it is REUSED — no `display-buffer-in-side-window' call at all —
and only a desired/actual column mismatch applies ONE `window-resize'
delta (with the frame-derived cols of R27-1 S1 the steady state is zero
resizes).  On creation the window is pinned with `window-preserve-size'
plus the dedicated/no-delete parameters, so redisplay and sibling churn
cannot drift its width between renders.  The show path never
deletes+recreates; only `org-air-rail--hide' (a real pop-in teardown)
deletes the window.  Renders NO content.  Returns the side window (or
nil)."
  (let* ((cols (org-air-rail--window-cols))
         (rail-buf (org-air-rail--get-buffer))
         (existing (get-buffer-window rail-buf (selected-frame))))
    (if (window-live-p existing)
        (progn
          ;; Converge: a no-op when desired == actual; else one resize.
          (unless (= (window-total-width existing) cols)
            (ignore-errors
              (window-resize existing
                             (- cols (window-total-width existing)) t t))
            (window-preserve-size existing t t))
          (with-current-buffer board-buffer
            (setq-local org-air-view--rail-buffer rail-buf
                        org-air-rail--window existing))
          existing)
      (org-air-rail--setup-divider)
      (let ((win (display-buffer-in-side-window
                  rail-buf (org-air-rail--window-params cols))))
        (when (window-live-p win)
          ;; R16 D-P1: do NOT set `no-other-window' — keep the rail reachable.
          (set-window-parameter win 'no-delete-other-windows t)
          (set-window-dedicated-p win t)
          ;; R27-1 S2: pin the width so redisplay and sibling churn cannot
          ;; drift it — the window is resized only through the convergent
          ;; branch above.
          (window-preserve-size win t t))
        (with-current-buffer board-buffer
          (setq-local org-air-view--rail-buffer rail-buf
                      org-air-rail--window (and (window-live-p win) win)))
        win))))

(defun org-air-rail--host-width (host-buffer width)
  "Return HOST-BUFFER's REAL compose width under the side-window rail (R27-2).
Ensures the (pinned, frame-derived) rail side window FIRST via the
convergent `org-air-rail--ensure-window' (R27-1 S2), then measures the
host window's USABLE columns (`org-air-layout--usable-columns') — the
width content must be composed at.  R29-1: usable columns, NOT raw
`window-body-width' — in a fringe-less GUI the continuation-glyph column
is reserved (`window-max-chars-per-line' = body - 1), so composing at the
raw body width overflowed every line by one; in a TTY/batch frame the two
are equal, so every batch value is unchanged.  With the frame-derived
cols (S1) \"settle\" is one step and convergent: the render tail's
`org-air-rail--show' derives the SAME cols, so no post-composition resize
can move the goalposts.  WIDTH is the fallback when HOST-BUFFER has no
live window (and the floor input: the result never drops below
`org-air-item-pane-min').  Shared by the board and the project (one
primitive, no fork); the batch width seams bypass this helper entirely."
  (org-air-rail--ensure-window host-buffer width)
  (let ((win (get-buffer-window host-buffer)))
    (if (window-live-p win)
        (max org-air-item-pane-min (org-air-layout--usable-columns win))
      width)))

(defun org-air-rail--input-stamp (board-buffer width height)
  "Return the rail content input stamp for BOARD-BUFFER at WIDTH x HEIGHT.
R27-1 S4: every input the rail paint reads through the back-pointer —
owner buffer, `org-air-view--items' identity, items key, filter, scope,
expanded sections, calendar month, cols, height, descriptor identity,
plus the calendar's current day — so an unchanged stamp proves a repaint
would be byte-identical and may be skipped."
  (with-current-buffer board-buffer
    (list board-buffer
          org-air-view--items
          org-air-view--items-key
          org-air-view--tag-filter
          org-air-view--scope
          org-air-view--expanded-sections
          org-air-view--cal-month
          width height
          org-air-view--rail-descriptor
          (format-time-string "%F"))))

(defun org-air-rail--show (board-buffer width)
  "Show + render the rail side window for BOARD-BUFFER at board WIDTH (R15 D-P2).
WIDTH is the board's total window width (the batch seam input; the side
window's own column width is the frame-derived R27-1 S1 tier).  Ensures
the window (convergent — reuses the window `org-air-view--render' may
already have created), then renders the rail content + inspector.
R27-1 S4: the content paint is STAMP-GUARDED — when every paint input
of `org-air-rail--input-stamp' matches the previous paint the erase+
re-insert is skipped (the output would be byte-identical), so the steady
state is zero rail repaints and exactly one at the R26-8 swap."
  (let* ((cols (org-air-rail--window-cols (and org-air-view-width width)))
         (win (org-air-rail--ensure-window board-buffer width)))
    ;; The render-width/-height seams (`org-air-view-width/-height', used
    ;; for deterministic batch goldens) drive the rail dimensions when set;
    ;; otherwise the live side window's body metrics do.  This keeps the
    ;; per-buffer text goldens reproducible in batch where side-window
    ;; geometry is unreliable (R15 D-P2 testability plan).
    ;; R29-1: the rail's OWN lines are composed at the side window's
    ;; USABLE columns (not raw body width) so they too fit a fringe-less
    ;; GUI window; TTY/batch values are identical.
    (let* ((rwidth (cond ((and noninteractive org-air-view-width) cols)
                         ((window-live-p win)
                          (max 1 (org-air-layout--usable-columns win)))
                         (t cols)))
           (rheight (cond (org-air-view-height nil)
                          ((window-live-p win) (window-body-height win))
                          (t nil)))
           (rail-buf (org-air-rail--get-buffer))
           (stamp (org-air-rail--input-stamp board-buffer rwidth rheight)))
      (unless (equal stamp (buffer-local-value 'org-air-rail--last-stamp
                                               rail-buf))
        (org-air-rail--render board-buffer rwidth rheight)
        (with-current-buffer rail-buf
          (setq-local org-air-rail--last-stamp stamp))))
    ;; Phase 2: bracket the rail inspector region + wire the board hook.
    (org-air-rail--setup-inspector board-buffer)
    (setq org-air-rail--side-was-live (and (window-live-p win) t))
    win))

(defun org-air-rail--hide (board-buffer)
  "Delete the rail side window and clear caches for BOARD-BUFFER (R16 D-P1).
The `*org-air-rail*' buffer survives when `org-air-rail-keep-buffer' is
non-nil (cheaper re-popout); otherwise it is killed."
  (setq org-air-rail--side-was-live nil)
  (let ((rail-buf (get-buffer org-air-rail-buffer-name)))
    (when (buffer-live-p rail-buf)
      (let ((win (get-buffer-window rail-buf)))
        (when (window-live-p win)
          (delete-window win)))
      (unless org-air-rail-keep-buffer
        (kill-buffer rail-buf))))
  (when (buffer-live-p board-buffer)
    (with-current-buffer board-buffer
      (setq-local org-air-view--rail-buffer nil
                  org-air-rail--window nil
                  ;; the rail inspector is gone; stop driving it.
                  org-air-view--inspector-target-buffer nil))))

(defun org-air-rail--teardown ()
  "Tear down the rail window + buffer for the current board buffer (R15 D-P2).
Used by `org-air-view-quit' and the board's `kill-buffer-hook'."
  (let ((org-air-rail-keep-buffer nil))
    (org-air-rail--hide (current-buffer)))
  (let ((rail-buf (get-buffer org-air-rail-buffer-name)))
    (when (buffer-live-p rail-buf)
      (kill-buffer rail-buf))))

(defun org-air-rail--popped-p (&optional buffer)
  "Non-nil when BUFFER's (default current) rail is GENUINELY popped out.
R26-5: only the explicit t counts — the `unset' first-render sentinel is
TRUTHY, and reading it raw is exactly how the re-entry wipe got a double
rail blessed.  The renderers, the toggle and BOTH reconciler branches
route through this one predicate."
  (eq (if buffer
          (buffer-local-value 'org-air-view--rail-popped-out buffer)
        org-air-view--rail-popped-out)
      t))

(defun org-air-rail--window-live-p ()
  "Return non-nil when the `*org-air-rail*' buffer is shown on this frame.
Checks the board frame so a stray rail buffer in another frame does not
count (R16 D-P1)."
  (let ((rail-buf (get-buffer org-air-rail-buffer-name)))
    (and (buffer-live-p rail-buf)
         (get-buffer-window rail-buf (selected-frame))
         t)))

;;;; ---------------------------------------------------------------------
;;;; R16 D-P1: cooperative, command-driven popout + reconciler.
;;;; ---------------------------------------------------------------------

(defun org-air-view--refresh-current ()
  "Re-render the current org-air buffer, dispatching on mode (R22-5).
The shared rail-toggle uses this so it never hard-codes the board renderer:
the board re-renders via `org-air-view--render-current'; the project via
`org-air-project--render-current'."
  (cond
   ((derived-mode-p 'org-air-view-mode) (org-air-view--render-current))
   ((derived-mode-p 'org-air-project-mode) (org-air-project--render-current))
   ;; R26-5: a doc-session buffer "refreshes" by re-showing/hiding its
   ;; DOC-context side rail per the popped flag (the buffer text is the
   ;; user's file — never re-rendered by org-air).
   ((and (bound-and-true-p org-air-project--session-tree)
         (fboundp 'org-air-project--doc-rail-refresh))
    (org-air-project--doc-rail-refresh (current-buffer)))))

(defun org-air-rail-toggle ()
  "Toggle the context rail between inline and a side window (R16 D-P1; R22-5).
Command-driven and cooperative in the board OR the project: popping out
renders the host pane-only and shows the `*org-air-rail*' side window;
popping in restores the inline two-pane rail.  Native window management
always wins — closing the side window with any native command falls back to
inline via the reconciler.  The refresh is dispatched per-mode via
`org-air-view--refresh-current' so the toggle never forks."
  (interactive)
  (unless (or (derived-mode-p 'org-air-view-mode 'org-air-project-mode)
              ;; R26-5: the toggle also works from a doc-session buffer
              ;; (its side rail is the DOC context).
              (bound-and-true-p org-air-project--session-tree))
    (user-error "Not in an org-air board or project buffer"))
  ;; The project never seeds the flag during its normal render, so it may
  ;; still be the `unset' sentinel (which is truthy) — normalise it to nil
  ;; so the FIRST toggle pops OUT, not in.
  (when (eq org-air-view--rail-popped-out 'unset)
    (setq-local org-air-view--rail-popped-out nil))
  (if (org-air-rail--popped-p)
      (progn
        (setq-local org-air-view--rail-popped-out nil
                    org-air-view--rail-suspended nil)
        (org-air-rail--hide (current-buffer))
        (org-air-view--refresh-current))
    (setq-local org-air-view--rail-popped-out t
                org-air-view--rail-suspended nil)
    (org-air-view--refresh-current)
    (when org-air-rail-focus-on-popout
      (let ((win (and (org-air-rail--window-live-p)
                      (get-buffer-window org-air-rail-buffer-name
                                         (selected-frame)))))
        (when (window-live-p win)
          (select-window win))))))

(defun org-air-rail-popout ()
  "Pop the context rail OUT into the side window if it is inline (R16 D-P1)."
  (interactive)
  (when (and (derived-mode-p 'org-air-view-mode)
             (not (org-air-rail--popped-p)))
    (org-air-rail-toggle)))

(defun org-air-rail-popin ()
  "Pop the context rail back INLINE if it is a side window (R16 D-P1).
Works from the board OR the project OR from inside the rail buffer (`q').
R24-5: dispatch the re-render per host mode via `org-air-view--refresh-
current' (the rail back-pointer points at the PROJECT buffer when the
project popped it) so a project rail falls back to inline like the board's."
  (interactive)
  (let ((board (if (derived-mode-p 'org-air-view-mode 'org-air-project-mode)
                   (current-buffer)
                 (or (and (boundp 'org-air-rail--board-buffer)
                          org-air-rail--board-buffer)
                     (get-buffer org-air-view-buffer-name)))))
    (when (buffer-live-p board)
      (with-current-buffer board
        (when (org-air-rail--popped-p)
          (setq-local org-air-view--rail-popped-out nil
                      org-air-view--rail-suspended nil)
          (org-air-rail--hide board)
          (org-air-view--refresh-current)))
      ;; If `q' was pressed inside the rail, hop focus back to the board.
      (when (not (eq (current-buffer) board))
        (let ((win (get-buffer-window board (selected-frame))))
          (when (window-live-p win)
            (select-window win)))))))

(defun org-air-rail--user-closed-p (board)
  "Return non-nil when the rail's absence on BOARD's frame is a USER close.
A genuine user close = the board window is live and wide enough to show
the rail, yet the rail buffer shows in no window.  A responsive board-only
teardown (narrow) is NOT a user close — the flag is kept so widening
re-pops the side window (R16 D-P1, design transition table)."
  (and (get-buffer-window board (selected-frame))
       (not (org-air-rail--window-live-p))
       (not (org-air-view--board-only-p (org-air-view--render-width)))))

;;;; ---------------------------------------------------------------------
;;;; R25-6: CLEAN rail dual-mode — single-owner invariant, reconciled to
;;;; the ACTIVE view.  At most ONE *org-air-rail* side window exists on the
;;;; frame; it belongs to exactly the active org-air main view and exists
;;;; IFF that view's `org-air-view--rail-popped-out' is t (and it is wide
;;;; enough).  Every other view renders its rail INLINE.  A popped-but-not-
;;;; active view is SUSPENDED (`org-air-view--rail-suspended' t) so it
;;;; re-pops on return.
;;;; ---------------------------------------------------------------------

(defun org-air-rail--host-buffer-p (buf)
  "Non-nil when BUF is an org-air board/project (rail HOST) buffer (R25-6).
R26-5: a DOC-SESSION file buffer (one carrying the back-pointer
`org-air-project--session-tree') counts as a host too, so the R25-6
suspension/re-pop sweep treats the doc half of a project session exactly
like a board<->project switch."
  (and (buffer-live-p buf)
       (or (with-current-buffer buf
             (derived-mode-p 'org-air-view-mode 'org-air-project-mode))
           (and (local-variable-p 'org-air-project--session-tree buf)
                (buffer-local-value 'org-air-project--session-tree buf)
                t))))

(defun org-air-rail--active-view (&optional frame)
  "Return the org-air HOST buffer shown in a MAIN (non-side) window on FRAME.
Prefers the selected window; else the first non-side window hosting an
org-air view.  The `*org-air-rail*' and `*org-air-view*' panes are side
windows, so they are skipped — only the BOARD/PROJECT main view counts the
rail belongs to (R25-6)."
  (setq frame (or frame (selected-frame)))
  (let* ((sel-win (frame-selected-window frame))
         (sel (window-buffer sel-win)))
    (if (and (not (window-parameter sel-win 'window-side))
             (org-air-rail--host-buffer-p sel))
        sel
      (catch 'hit
        (dolist (w (window-list frame 'no-mini))
          (unless (window-parameter w 'window-side)
            (when (org-air-rail--host-buffer-p (window-buffer w))
              (throw 'hit (window-buffer w)))))
        nil))))

(defun org-air-rail--side-window (&optional frame)
  "Return the live `*org-air-rail*' side window on FRAME, or nil (R25-6)."
  (let ((rb (get-buffer org-air-rail-buffer-name)))
    (and rb (get-buffer-window rb (or frame (selected-frame))))))

(defun org-air-rail--side-owner (&optional frame)
  "Return the OWNER (back-pointer) buffer of the side rail on FRAME, or nil.
The owner is the board/project buffer the rail currently mirrors, read
from the rail buffer's `org-air-rail--board-buffer' (R25-6)."
  (let ((win (org-air-rail--side-window frame)))
    (and win (buffer-local-value 'org-air-rail--board-buffer
                                 (window-buffer win)))))

(defun org-air-rail--evict-foreign-rail (self)
  "Hide a `*org-air-rail*' side window that does NOT belong to SELF (R25-6).
Suspends its owner (flag kept) so returning to that owner re-pops cleanly.
Called from a render tail: when SELF is popped, `org-air-rail--show' has
already re-owned the window, so the owner == SELF and this no-ops; when
SELF is inline it drops a lingering foreign rail (the cross-view sweep)."
  (let* ((frame (selected-frame))
         (side  (org-air-rail--side-window frame))
         (owner (org-air-rail--side-owner frame)))
    (when (and (window-live-p side) (not (eq owner self)))
      (when (buffer-live-p owner)
        (with-current-buffer owner
          (setq-local org-air-view--rail-suspended t)))
      (org-air-rail--hide (or owner self)))))

(defun org-air-rail--reconcile-run (frame)
  "Run the deferred reconcile for FRAME; the single timer slot's body (R27-1).
Named (not a closure) so tests can count pending reconcile timers
deterministically; clears `org-air-rail--reconcile-timer' before running."
  (setq org-air-rail--reconcile-timer nil)
  (when (frame-live-p frame)
    (org-air-rail--reconcile-frame frame)))

(defun org-air-rail--reconcile ()
  "Enforce the single-owner rail invariant for the ACTIVE view (R25-6).
Buffer-local on each board/project `window-configuration-change-hook'.
Defers the (window-mutating) reconcile to a 0s timer so it runs AFTER the
window config settles (window mutation never runs INSIDE the hook).
R27-1 S3: ONE pending timer slot — a hook fire while a reconcile is
already pending RESCHEDULES it instead of stacking one new timer per fire."
  (unless noninteractive
    (when (timerp org-air-rail--reconcile-timer)
      (cancel-timer org-air-rail--reconcile-timer))
    (setq org-air-rail--reconcile-timer
          (run-with-timer 0 nil #'org-air-rail--reconcile-run
                          (selected-frame)))))

(defun org-air-rail--reconcile-frame (frame)
  "Reconcile the singleton side rail to the ACTIVE org-air view on FRAME (R25-6).
Enforces the single-owner invariant: the side rail exists IFF the active
main view is popped (and wide enough); a view popped but not active is
suspended; a genuinely user-closed rail falls back inline.
R27-1 S3: render-latched and edge-triggered — while
`org-air-rail--reconciling' is bound (the full extent of a board/project
render) the body NO-OPS and re-arms the single timer slot for after the
render, so a timer nesting inside an in-flight render can never misread
the transient popped-but-windowless state; and the user-close branch may
fire ONLY on an observed live->dead transition of
`org-air-rail--side-was-live', never on mere absence (absence + flag t
+ was-live nil = a popout in flight or a suspended view: leave the state
alone — the render tail owns it)."
  (unless noninteractive
    (if org-air-rail--reconciling
        ;; Render latch: never mutate rail state mid-render; the single
        ;; slot re-runs this after the render extent unwinds.
        (unless (timerp org-air-rail--reconcile-timer)
          (setq org-air-rail--reconcile-timer
                (run-with-timer 0 nil #'org-air-rail--reconcile-run frame)))
      (let* ((org-air-rail--reconciling t)
             (was-live org-air-rail--side-was-live)
             (active (org-air-rail--active-view frame))
             (side   (org-air-rail--side-window frame))
             (owner  (org-air-rail--side-owner frame)))
        (cond
         ;; No active org-air main view: any side rail is an orphan -> hide
         ;; it, mark its owner suspended so re-entry re-pops.
         ((not (buffer-live-p active))
          (when (window-live-p side)
            (when (buffer-live-p owner)
              (with-current-buffer owner
                (setq-local org-air-view--rail-suspended t)))
            (org-air-rail--hide (or owner active))))
         (t
          (with-current-buffer active
            (let ((width (org-air-view--render-width)))
              (cond
               ;; (A) active WANTS the side rail.  R26-5: through the ONE
               ;; popped predicate — `unset' can never read as "wants it".
               ((org-air-rail--popped-p)
                (cond
                 ((eq owner active)             ; already ours -> consistent
                  (setq-local org-air-view--rail-suspended nil))
                 ((window-live-p side)          ; owned by another view -> re-own
                  (when (buffer-live-p owner)
                    (with-current-buffer owner
                      (setq-local org-air-view--rail-suspended t)))
                  (setq-local org-air-view--rail-suspended nil)
                  (org-air-rail--show active width))
                 ((org-air-view--board-only-p width) nil) ; narrow -> keep flag
                 (org-air-view--rail-suspended  ; hidden for a switch -> re-pop
                  (setq-local org-air-view--rail-suspended nil)
                  (org-air-rail--show active width))
                 (was-live
                  ;; Observed live->dead with the host still wide enough:
                  ;; the user CLOSED it natively -> go inline (R16 contract).
                  (setq-local org-air-view--rail-popped-out nil
                              org-air-view--rail-suspended nil)
                  (when (get-buffer-window active frame)
                    (org-air-view--refresh-current)))
                 (t
                  ;; Absence WITHOUT a live->dead edge: a popout still in
                  ;; flight (mid-render) — leave the state alone; the render
                  ;; tail owns it (R27-1 S3 edge-triggered user-close).
                  nil)))
               ;; (B) active is INLINE: no side rail may show.
               (t
                (when (window-live-p side)
                  (when (buffer-live-p owner)
                    (with-current-buffer owner
                      (setq-local org-air-view--rail-suspended t)))
                  (org-air-rail--hide (or owner active)))))))))
        ;; Record the side window's liveness for the next run's edge
        ;; detection (also updated by `org-air-rail--show'/`--hide').
        (setq org-air-rail--side-was-live
              (and (window-live-p (org-air-rail--side-window frame)) t))))))

;;;; ---------------------------------------------------------------------
;;;; R16 D-P3: mu4e-style bottom source/entry view pane (*org-air-view*).
;;;;
;;;; An optional bottom side window showing the SOURCE of the selected item
;;;; — the org entry (heading + body + drawers) at the item's marker — a
;;;; read-only snapshot in `*org-air-view*'.  Explicit-open by default (`v')
;;;; with optional follow-mode; cooperative with the rail side window and
;;;; native windows.
;;;; ---------------------------------------------------------------------

(defconst org-air-view-pane-buffer-name "*org-air-view*"
  "Name of the bottom source/entry view pane buffer (R16 D-P3/D-P2).")

(defconst org-air-view-pane--file-head-chars 4000
  "Character cap for the file-head snapshot when no heading is at point.
Used for heading-less files / before-first-heading positions, where there
is no subtree to bound the copy; `org-air-view-pane-max-lines' caps the
shown lines on top of this (R16 D-P3).")

(defcustom org-air-view-pane-height 14
  "Height of the bottom `*org-air-view*' source pane (R16 D-P3).
An integer >= 1 is read as a line count; a value < 1 (a float like 0.33)
is read as a fraction of the frame height."
  :type 'number
  :group 'org-air)

(defcustom org-air-view-pane-follow t
  "When non-nil, the bottom view pane tracks point on the board (R16 D-P3).
R18 D-P4: the default is now t so that ONCE the pane is open (RET), moving
point auto-updates it to the item at point (debounced, inert under batch)
— an auto-inspecting pane like the rail inspector.  This does NOT auto-open
the pane: the follow hook guards on a live pane window, so nothing appears
until you press RET (or `v').  nil restores explicit-open-only updates."
  :type 'boolean
  :group 'org-air)

(defcustom org-air-view-pane-on-return nil
  "Obsolete (R18 D-P4): RET now owns the pane via `org-air-view-pane-return'.
Formerly, when non-nil, RET = `org-air-visit-item' ALSO opened the pane;
that behaviour is moot now RET opens the pane directly.  No longer
consulted."
  :type 'boolean
  :group 'org-air)
(make-obsolete-variable 'org-air-view-pane-on-return nil "org-air 0.5")

(defcustom org-air-view-pane-focus nil
  "When non-nil, opening the bottom view pane selects its window (R16 D-P3).
Default nil keeps point on the board; the pane is `other-window'-reachable."
  :type 'boolean
  :group 'org-air)

(defcustom org-air-view-pane-keep-buffer t
  "When non-nil the `*org-air-view*' buffer survives a pane close (R16 D-P3)."
  :type 'boolean
  :group 'org-air)

(defcustom org-air-view-pane-max-lines nil
  "Maximum entry lines shown in the bottom view pane, or nil (R16 D-P3).
When an integer, very large entries are capped with a `…' continuation
marker; nil shows the full subtree."
  :type '(choice (const :tag "Full subtree" nil) integer)
  :group 'org-air)

(defcustom org-air-view-pane-line-spacing 0.15
  "Buffer-local `line-spacing' for the bottom `*org-air-view*' pane (R18 D-P5.2).
The pane has NO `│' divider (unlike the two-pane board), so a small
positive leading is free and gives the entry snapshot a calmer, mu4e
message-view rhythm.  Display-only — the pane is a side window, never part
of the board fixture bytes.  nil leaves the frame default; 0 packs tight."
  :type '(choice (const :tag "Frame default" nil) number)
  :group 'org-air)

(defcustom org-air-view-pane-follow-debounce 0.2
  "Idle seconds before follow redraws the bottom view pane (R16 D-P3 / R20-3b).
Mirrors `org-air-inspector-debounce': a short idle delay coalesces rapid
point motion into a single re-narrow so scrubbing a large file with
hundreds of items across dozens of files stays responsive.  R20-3b raised
the default 0.1 -> 0.2 so a scrub coalesces to ONE update at rest (the
follow itself is now cheap: same-file changes re-narrow the existing
indirect instead of rebuilding it)."
  :type 'number
  :group 'org-air)

(defcustom org-air-view-pane-editable t
  "When non-nil, the bottom view pane is a LIVE, editable Org buffer (R19-3).
The pane becomes an `org-mode' INDIRECT buffer narrowed to the source
heading, so edits write through to the file's buffer and saving persists
them to disk (the after-save hook then refreshes the board).  When nil, OR
under `noninteractive', OR when the source cannot be resolved, the pane
falls back to the unchanged READ-ONLY snapshot — so batch + every fixture
use the snapshot path and stay byte-identical."
  :type 'boolean
  :group 'org-air)

(defcustom org-air-view-pane-variable-pitch nil
  "When non-nil, enable `variable-pitch-mode' in the editable view pane (R19-3).
The pane is NOT pixel-locked (the V6 invariant governs the BOARD, where the
svg pills + `│' divider must occupy exact text cells), so a prose-like
proportional message view is allowed here.  Default off (opt-in); the round
only requires that it be ALLOWED."
  :type 'boolean
  :group 'org-air)

(defvar-local org-air-view--view-pane-item nil
  "Item last shown in the bottom view pane; the follow change-guard.")
(defvar-local org-air-view--view-pane-last-pos nil
  "Point position at the last follow fire; the cheap motion early-out (R20-3b).")
(defvar-local org-air-view--view-pane-timer nil
  "Pending debounce timer for the follow view-pane update (R16 D-P3).")
(defvar-local org-air-view--pane-indirect nil
  "The current editable-pane indirect buffer for this host buffer (R19-3).
One at a time: replaced (old killed) on follow / re-open and killed on pane
close.  Killing an indirect buffer never loses text — unsaved edits live in
the base file buffer and stay savable.")

(defvar org-air-entry-view-mode-map
  (let ((map (make-sparse-keymap)))
    ;; R20-3a: the snapshot pane is read-only, so `q' closes it (overrides
    ;; `special-mode's bury so the pane is actually torn down) instead of
    ;; merely burying the buffer and leaving the split behind.
    (define-key map (kbd "q") #'org-air-view-pane-quit)
    map)
  "Keymap for `org-air-entry-view-mode' (the read-only snapshot pane).")

(define-derived-mode org-air-entry-view-mode special-mode "org-air-view"
  "Major mode for the bottom `*org-air-view*' source/entry pane (R16 D-P3).
A read-only snapshot of the selected item's Org entry with Org font-lock,
`other-window'-reachable for reading/scrolling."
  (setq-local truncate-lines nil)
  ;; R18 D-P5.2: the pane has no `│' divider, so a small positive leading is
  ;; free and gives a calmer mu4e message-view rhythm.
  (setq-local line-spacing org-air-view-pane-line-spacing)
  ;; R18 D-P5.1: the calm nano-style mode-line here too.
  (org-air-view--install-modeline)
  (setq-local cursor-type t)
  (setq-local buffer-read-only t)
  ;; R27-4: evil parity for the read-only pane — under evil, `q' resolved
  ;; to evil-record-macro instead of closing the pane.
  (org-air-view--setup-evil 'org-air-entry-view-mode
                            org-air-entry-view-mode-map))

(defun org-air-view-pane--buffer ()
  "Get or create the `*org-air-view*' pane buffer in `org-air-entry-view-mode'."
  (let ((buf (get-buffer-create org-air-view-pane-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-air-entry-view-mode)
        (org-air-entry-view-mode)))
    buf))

(defun org-air-view-pane--window-params ()
  "Return the `display-buffer' action alist for the bottom view pane (R19-3).
The pane SPLITS the board window (`display-buffer-below-selected'), not a
frame-level bottom side window — so the rail's RIGHT side window keeps its
full frame-body height (side windows are placed around the whole main
area; only the board window is split).  The `org-air-pane' parameter tags
the window so it is reused on follow and found on close; NO `no-other-
window' — the pane is `other-window'-reachable and survives
`delete-other-windows'."
  `((display-buffer-below-selected)
    (window-height . ,org-air-view-pane-height)
    (dedicated . t)
    (window-parameters . ((no-delete-other-windows . t)
                          (org-air-pane . t)))))

(defun org-air-view-pane--find-window ()
  "Return the live org-air bottom pane window on this frame, or nil (R19-3).
Identified by the `org-air-pane' window parameter, so it works whether the
pane shows the read-only snapshot buffer or a per-heading indirect buffer."
  (seq-find (lambda (w) (window-parameter w 'org-air-pane))
            (window-list (selected-frame) 'no-mini)))

(defun org-air-view-pane--window-live-p ()
  "Return non-nil when an org-air bottom pane window is shown on this frame."
  (window-live-p (org-air-view-pane--find-window)))

(defun org-air-view--pane-host-p ()
  "Non-nil in a buffer that hosts the bottom view pane (R18 D-P3).
The board (`org-air-view-mode') AND the project (`org-air-project-mode')
both drive the same pane, so the follow hook fires in either."
  (or (derived-mode-p 'org-air-view-mode)
      (derived-mode-p 'org-air-project-mode)))

(defun org-air-view--view-pane-thing-at-point ()
  "Return the follow change-guard key for the row at point (R18 D-P3).
The board item, else the project doc, else the shared `org-air-marker' —
so the pane re-follows when the SELECTED ROW changes in either view.
R22-2: line-based so a native/mouse landing on a dead column still follows."
  (or (org-air-view--row-property 'org-air-item)
      (org-air-view--row-property 'org-air-doc)
      (org-air-view--row-property 'org-air-marker)))

(defun org-air-view-pane--row-thing-near-point ()
  "Return (PROP . VALUE) for the doc/item at or after point, else nil (R24-4).
Resolves the doc/item on THIS row, else on the NEAREST following row.  Lets
RET/click on a dir-header or blank row open the first doc beneath it instead
of erroring."
  (or (let ((d (org-air-view--row-property 'org-air-doc)))  (and d (cons 'org-air-doc d)))
      (let ((i (org-air-view--row-property 'org-air-item))) (and i (cons 'org-air-item i)))
      ;; fall forward to the next doc/item row in the buffer.
      (save-excursion
        (let ((pos (or (next-single-property-change (point) 'org-air-doc)
                       (next-single-property-change (point) 'org-air-item))))
          (when pos
            (goto-char pos)
            (or (let ((d (org-air-view--row-property 'org-air-doc)))  (and d (cons 'org-air-doc d)))
                (let ((i (org-air-view--row-property 'org-air-item))) (and i (cons 'org-air-item i)))))))))

(defun org-air-view-pane--context-at-point ()
  "Return a plist describing the source to show for the item/doc at point.
Keys: :marker (a marker or filepath string), :file, :title, :state.
Works on the board (`org-air-item') and the project view (`org-air-doc')
via the shared `org-air-marker' text property (R16 D-P3).
R22-2: resolve each row property anywhere on the line (point-independent),
so a native/mouse landing on the leading margin/rail/pad still resolves.
R24-4: when the row has NO item/doc/marker (a dir-header or blank row),
fall forward to the NEAREST following doc/item so RET/click still opens a
pane instead of erroring (shared resolver; the board section headings benefit
identically)."
  (let ((item (org-air-view--row-property 'org-air-item))
        (doc (org-air-view--row-property 'org-air-doc))
        (marker (org-air-view--row-property 'org-air-marker)))
    (unless (or item doc marker)
      (pcase (org-air-view-pane--row-thing-near-point)
        (`(org-air-doc . ,d)  (setq doc d))
        (`(org-air-item . ,i) (setq item i))))
    (cond
     (item
      (list :marker (or marker (org-air-item-marker item))
            :file (org-air-item-file item)
            :title (org-air-item-title item)
            :state (org-air-item-todo item)))
     (doc
      (list :marker (or marker (org-air-doc-file doc))
            :file (org-air-doc-file doc)
            :title (org-air-doc-name doc)
            :state (org-air-doc-state doc)))
     (marker
      (list :marker marker)))))

(defun org-air-view-pane--source-buffer-pos (marker)
  "Resolve MARKER (a live marker, filepath, or cons) to (BUFFER . POS).
Visits a file in the background (never pops it).  Returns nil when the
source is unavailable (R16 D-P3).  R26-8: a cache-hydrated (FILE . POS)
cons hydrates on demand — the pane visits FILE and lands on POS."
  (cond
   ((and (markerp marker) (marker-buffer marker)
         (buffer-live-p (marker-buffer marker)))
    (cons (marker-buffer marker) (marker-position marker)))
   ((and (stringp marker) (file-readable-p marker))
    (cons (find-file-noselect marker) nil))
   ((and (consp marker) (stringp (car marker))
         (file-readable-p (car marker)))
    (cons (find-file-noselect (car marker)) (cdr marker)))
   (t nil)))

(defun org-air-view-pane--entry-text (buffer pos)
  "Return the Org entry text at POS in BUFFER (heading + body + drawers).
When POS is nil, return the file head (heading-less files → the buffer
head).  Org font-lock is applied on a copy (R16 D-P3)."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (let ((head-end (min (point-max)
                             (+ (point-min) org-air-view-pane--file-head-chars)))
              beg end)
          (if (and pos (ignore-errors (goto-char pos)))
              (progn
                (ignore-errors (org-back-to-heading t))
                (if (org-before-first-heading-p)
                    (setq beg (point-min) end head-end)
                  (setq beg (point)
                        end (save-excursion (org-end-of-subtree t t) (point)))))
            (setq beg (point-min) end head-end))
          (org-air-view-pane--fontify
           (buffer-substring beg end)))))))

(defun org-air-view-pane--fontify (text)
  "Return TEXT fontified as Org (R16 D-P3).
Uses a temp Org buffer; degrades to the raw text when Org font-lock is
unavailable (e.g. batch)."
  (condition-case nil
      (with-temp-buffer
        (delay-mode-hooks (org-mode))
        (insert text)
        (if noninteractive
            (buffer-string)
          (font-lock-ensure)
          (buffer-string)))
    (error text)))

(defun org-air-view-pane--header-line (ctx &optional close-key)
  "Return the `*org-air-view*' header-line string for context CTX (R16 D-P3).
Text contract: `▤ <file>  ·  <title>  ·  <state>'.  R18 D-P5.2 gives it
mu4e-style chrome: the TITLE is the one salient segment, the file/state
and the `·' separators ride the faded face.  R20-3a surfaces the active
CLOSE-KEY as a trailing `· <key> close' hint when given (so the in-pane
close verb is discoverable).  The header is not buffer text (the pane byte
golden strips it), so this is byte-invisible."
  (let* ((icon (org-air-view--glyph 'view-pane))
         (dot (concat "  "
                      (propertize (org-air-view--glyph 'sep-dot)
                                  'face 'org-air-face-inspector-label)
                      "  "))
         ;; R22-7: face the filename + state (+ the separators / icon /
         ;; close-key) with the readable mid-tier `org-air-face-inspector-
         ;; label' (6.02:1 light / 8.32:1 dark) instead of `org-air-face-
         ;; faded' (2.15:1 / 2.45:1, sub-AA) so the FIRST-read filename is
         ;; legible; the TITLE stays the strongest segment (pane-title ~11:1).
         (file (let ((f (plist-get ctx :file)))
                 (and f (propertize (file-name-nondirectory f)
                                    'face 'org-air-face-inspector-label))))
         (title (let ((tt (plist-get ctx :title)))
                  (and tt (propertize tt 'face 'org-air-face-pane-title))))
         (state (let ((s (plist-get ctx :state)))
                  (and s (propertize s 'face 'org-air-face-inspector-label))))
         (parts (delq nil (list file title state))))
    (concat (propertize icon 'face 'org-air-face-inspector-label)
            " " (mapconcat #'identity parts dot)
            (if close-key
                (concat dot (propertize (concat close-key " close")
                                        'face 'org-air-face-inspector-label))
              ""))))

(defun org-air-view-pane--indirect (base pos title)
  "Return an `org-mode' indirect buffer on BASE narrowed to the subtree at POS.
Edits write through to BASE; `save-buffer' persists to disk (R19-3).  TITLE
names the (hidden) indirect buffer.  When POS is before the first heading —
a heading-less file head — the buffer is left WIDE so the file shows.
Narrowing is per-indirect-buffer — it never leaks to BASE or to the board's
own markers/classify scans of that file."
  ;; R28-1(a) naming contract: every buffer org-air creates and shows in
  ;; a window carries the `*org-air' prefix — NO leading `hidden buffer'
  ;; space, or the shipped/manual dimmer exclusions can never match the
  ;; pane.  Trade-off accepted: the transient indirect shows up in buffer
  ;; lists (killed by the R20-3 teardown / rebuilt by follow).
  (let ((ind (make-indirect-buffer
              base (generate-new-buffer-name
                    (concat "*org-air-pane:" (or title "") "*"))
              t)))                          ; CLONE = inherit org-mode
    (with-current-buffer ind
      (unless (derived-mode-p 'org-mode) (delay-mode-hooks (org-mode)))
      (widen)
      (when pos
        (goto-char pos)
        (ignore-errors (org-back-to-heading t))
        (if (org-before-first-heading-p)
            (widen)                          ; heading-less / preamble
          (org-narrow-to-subtree)))
      (goto-char (point-min)))
    ind))

(defvar-local org-air-view-pane--header-ruled nil
  "Non-nil once the pane `header-line' boundary rule has been remapped (R20-2).")

(defun org-air-view-pane--install-header-rule ()
  "Remap the pane `header-line' to the boundary-rule face once (R20-2 #2).
Gives the pane's TOP edge a quiet overline/background so the pane visibly
starts at its header; display-only (the header TEXT is untouched), so it is
byte-invisible.  Guarded so repeated chrome installs never stack remaps."
  (unless org-air-view-pane--header-ruled
    (setq-local org-air-view-pane--header-ruled t)
    (face-remap-add-relative 'header-line 'org-air-face-pane-header)))

(defun org-air-view-pane--install-chrome (ctx)
  "Install the pane header-line + leading on the current buffer for CTX (R19-3).
Keeps the existing `▤ file · title · state' header-line contract and the
R18 D-P5.2 `line-spacing'; enables `variable-pitch-mode' when
`org-air-view-pane-variable-pitch' is on (allowed — the pane is not pixel-
locked)."
  (setq-local header-line-format
              (org-air-view-pane--header-line ctx "C-c C-q"))
  (org-air-view-pane--install-header-rule)
  (org-air-view-pane--install-close-map)
  (when (numberp org-air-view-pane-line-spacing)
    (setq-local line-spacing org-air-view-pane-line-spacing))
  (when org-air-view-pane-variable-pitch
    (variable-pitch-mode 1)))

(defun org-air-view-pane--install-close-map ()
  "Install a buffer-local close map on the EDITABLE indirect pane (R20-3a).
`q' must stay self-insert in an editable Org buffer, so the close verb is a
dedicated `org-air-view-pane-quit' key (surfaced in the header-line hint);
`quit-window' is remapped so the standard quit key tears the indirect down
cleanly too.  Built on the current local map (the parent), so every binding
from `org-mode' still works underneath."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (current-local-map))
    (define-key map (kbd "C-c C-q") #'org-air-view-pane-quit)
    (define-key map [remap quit-window] #'org-air-view-pane-quit)
    (use-local-map map)))

(defun org-air-view-pane--render-snapshot (ctx src)
  "Render the READ-ONLY entry snapshot for CTX/SRC into `*org-air-view*'.
The unchanged R16 path: a fontified COPY of the subtree, dead sources show
a calm hint.  Used under `noninteractive', when `org-air-view-pane-editable'
is nil, or when the source is unresolvable — so every fixture stays
byte-identical (R19-3).  Returns the pane buffer."
  (let ((buf (org-air-view-pane--buffer)))
    ;; The editable indirect (if any) is being replaced by the snapshot;
    ;; forget it (the caller kills the now-unshown buffer).
    (when (org-air-view--pane-host-p)
      (setq-local org-air-view--pane-indirect nil))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (null src)
            (insert (propertize "(entry no longer available)"
                                'face 'org-air-face-empty))
          (let ((text (org-air-view-pane--entry-text (car src) (cdr src))))
            (insert text)
            (org-air-view-pane--apply-max-lines)))
        (setq-local header-line-format
                    (org-air-view-pane--header-line ctx "q"))
        (org-air-view-pane--install-header-rule)
        (goto-char (point-min))))
    buf))

(defun org-air-view-pane--render-editable (ctx src)
  "Build the LIVE, editable Org indirect pane for CTX/SRC (R19-3).
Returns a fresh indirect buffer on the source file narrowed to the heading;
stashes it on the host as `org-air-view--pane-indirect' (the caller kills
the previously-shown indirect AFTER the window swaps to this one, so the
pane never flickers a dead buffer)."
  (let ((ind (org-air-view-pane--indirect
              (car src) (cdr src) (plist-get ctx :title))))
    (with-current-buffer ind
      (org-air-view-pane--install-chrome ctx))
    (when (org-air-view--pane-host-p)
      (setq-local org-air-view--pane-indirect ind))
    ind))

(defun org-air-view-pane--renarrow (ind src ctx)
  "Re-narrow the existing indirect IND to SRC's heading, updating CTX's header.
The dominant per-change follow cost is REBUILDING the indirect (a fresh
`make-indirect-buffer' + `org-mode' init + font-lock); when the new item
lives in the SAME base file we instead widen + `org-narrow-to-subtree' at
the new heading and refresh the CTX header-line, skipping that cost
entirely.  Returns IND on success, nil on any error so the caller falls
back to a rebuild (R20-3b)."
  (condition-case nil
      (with-current-buffer ind
        (widen)
        (let ((pos (cdr src)))
          (when pos
            (goto-char pos)
            (ignore-errors (org-back-to-heading t))
            (if (org-before-first-heading-p)
                (widen)
              (org-narrow-to-subtree)))
          (goto-char (point-min)))
        (setq-local header-line-format
                    (org-air-view-pane--header-line ctx "C-c C-q"))
        ind)
    (error nil)))

(defun org-air-view-pane--render (ctx)
  "Show the entry described by CTX in the bottom pane; return its buffer.
With `org-air-view-pane-editable' (default t) and a resolvable source, the
pane is a LIVE, narrowed Org indirect buffer whose edits write through to
the file (R19-3); otherwise (toggle off, batch, or a dead source) it is the
unchanged read-only snapshot — so every byte fixture is byte-identical.
R20-3b: when the live indirect already shows the SAME base file, REUSE it
and re-narrow rather than rebuild."
  (let* ((marker (plist-get ctx :marker))
         (src (and marker (org-air-view-pane--source-buffer-pos marker))))
    (if (and src (not noninteractive) org-air-view-pane-editable)
        (let ((ind (and (org-air-view--pane-host-p)
                        (buffer-live-p org-air-view--pane-indirect)
                        org-air-view--pane-indirect)))
          (or (and ind
                   (eq (buffer-base-buffer ind) (car src))
                   (org-air-view-pane--renarrow ind src ctx))
              (org-air-view-pane--render-editable ctx src)))
      (org-air-view-pane--render-snapshot ctx src))))

(defun org-air-view-pane--apply-max-lines ()
  "Cap the current pane buffer at `org-air-view-pane-max-lines' (R16 D-P3).
Appends a `…' continuation marker when truncated.  No-op when nil."
  (when (integerp org-air-view-pane-max-lines)
    (save-excursion
      (goto-char (point-min))
      (when (and (> org-air-view-pane-max-lines 0)
                 (zerop (forward-line org-air-view-pane-max-lines))
                 (not (eobp)))
        (delete-region (point) (point-max))
        (insert (org-air-view--glyph 'more))))))

(defun org-air-view-pane--show (ctx)
  "Render CTX into the bottom pane and display it BELOW the board window.
The pane splits the board window (`display-buffer-below-selected'), so the
rail side window keeps its full frame-body height; a live pane window is
REUSED (`set-window-buffer') so follow/re-open never re-splits, and the
previously-shown indirect buffer is killed only AFTER the swap (no flicker)
\(R16 D-P3 / R19-3).  Respects `org-air-view-pane-focus'."
  (let* ((old (and (org-air-view--pane-host-p) org-air-view--pane-indirect))
         (host (and (org-air-view--pane-host-p) (current-buffer)))
         (buf (org-air-view-pane--render ctx)))
    (when (and (not noninteractive) (buffer-live-p buf))
      (let ((win (org-air-view-pane--find-window)))
        (if (window-live-p win)
            ;; Reuse the live pane window (follow / re-open): swap its buffer.
            (progn
              (set-window-dedicated-p win nil)
              (set-window-buffer win buf)
              (set-window-dedicated-p win t))
          ;; Open: split the BOARD window below it.  Resolve the board window
          ;; explicitly so a stray `selected-window' never splits the wrong
          ;; pane (robustness).
          (let ((host-win (or (and host (get-buffer-window host))
                              (selected-window))))
            (setq win (with-selected-window host-win
                        (display-buffer buf (org-air-view-pane--window-params))))))
        (if (window-live-p win)
            (progn
              (set-window-parameter win 'org-air-pane t)
              (set-window-parameter win 'no-delete-other-windows t)
              (set-window-dedicated-p win t)
              (when org-air-view-pane-focus
                (select-window win)))
          ;; R26-3b: `display-buffer' refused the pane (a user
          ;; `display-buffer-alist', an unsplittable/too-short host, a
          ;; dedicated window...) — say so.  Never a silent no-op.
          (message "org-air: could not display the view pane"))))
    ;; Kill the now-replaced indirect AFTER the window shows the new buffer.
    (when (and (buffer-live-p old) (not (eq old buf)))
      (kill-buffer old))
    buf))

(defun org-air-view-pane--kill-indirect ()
  "Kill the tracked pane indirect buffer; the base file buffer survives (R19-3).
Killing an indirect buffer never loses text — unsaved edits live in the
base and stay savable."
  (when (org-air-view--pane-host-p)
    (when (buffer-live-p org-air-view--pane-indirect)
      (kill-buffer org-air-view--pane-indirect))
    (setq-local org-air-view--pane-indirect nil)))

(defun org-air-view-pane--hide ()
  "Close the bottom pane window + kill its indirect buffer (R16 D-P3 / R19-3).
The base file buffer persists (and any unsaved edits with it); the snapshot
buffer is killed only when `org-air-view-pane-keep-buffer' is nil."
  (let ((win (org-air-view-pane--find-window)))
    (when (window-live-p win)
      (delete-window win)))
  (org-air-view-pane--kill-indirect)
  (let ((buf (get-buffer org-air-view-pane-buffer-name)))
    (when (and (buffer-live-p buf) (not org-air-view-pane-keep-buffer))
      (kill-buffer buf))))

(defun org-air-view-pane--teardown ()
  "Delete the bottom pane window + kill its buffer (R16 D-P3)."
  (let ((org-air-view-pane-keep-buffer nil))
    (org-air-view-pane--hide)))

(defun org-air-view-pane ()
  "Open OR refresh the bottom `*org-air-view*' source pane for the item at point.
If the pane is open it is refreshed to the current item; if closed it is
opened (R16 D-P3).  Key `v'."
  (interactive)
  (let ((ctx (org-air-view-pane--context-at-point)))
    (unless ctx
      (user-error "No org-air item at point"))
    (when (org-air-view--pane-host-p)
      (setq-local org-air-view--view-pane-item
                  (org-air-view--view-pane-thing-at-point)))
    (org-air-view-pane--show ctx)))

(defun org-air-view-pane-return ()
  "RET: open the bottom view pane for the item at point; focus it if open.
R18 D-P4: the first RET opens/refreshes the pane and KEEPS point on the
board; a second RET (RET while the pane is already open) selects the pane
window for reading/scrolling.  With `org-air-view-pane-follow' (default t)
the pane then tracks point as you move.  `q' / `other-window' return to the
board.  Visiting the file in the other window is `S-RET'
\(`org-air-visit-item') or, on a TTY that cannot send S-RET, `O'."
  (interactive)
  ;; Focus only when the pane is ALREADY live: the first RET opens (no
  ;; focus), the second RET (pane now live) focuses.  `org-air-view-pane'
  ;; honours the dynamic `org-air-view-pane-focus' binding.
  (let ((org-air-view-pane-focus (org-air-view-pane--window-live-p)))
    (org-air-view-pane)))

(defun org-air-view-pane-close ()
  "Close the bottom `*org-air-view*' source pane (R16 D-P3).  Key `V'."
  (interactive)
  (org-air-view-pane--hide))

(defun org-air-view-pane--board-window ()
  "Return a live window showing the org-air board / project host, or nil."
  (catch 'win
    (dolist (w (window-list))
      (with-current-buffer (window-buffer w)
        (when (derived-mode-p 'org-air-view-mode 'org-air-project-mode)
          (throw 'win w))))
    nil))

(defun org-air-view-pane-quit ()
  "Close the bottom view pane from WITHIN it and return to the board (R20-3a).
Bound to `q' in the read-only snapshot pane, and to a dedicated quit key
plus a `quit-window' remap in the editable indirect pane, so the pane is
closable while focused without an explicit function call.  Tears the pane
down cleanly (window deleted, indirect/snapshot killed via the existing
teardown) and re-selects the board window."
  (interactive)
  (let* ((board (org-air-view-pane--board-window))
         (host (and board (window-buffer board))))
    ;; Run the teardown in the HOST buffer's context so `--kill-indirect'
    ;; (which reads the host-local `org-air-view--pane-indirect') actually
    ;; kills the indirect; `--find-window' locates the pane window globally.
    (if (buffer-live-p host)
        (with-current-buffer host (org-air-view-pane--hide))
      (org-air-view-pane--hide))
    (when (window-live-p board)
      (select-window board)
      ;; R29-2: the pane-return entry tail normalizes explicitly — point
      ;; left in the gutter before the pane opened lands on the title.
      (with-current-buffer (window-buffer board)
        (org-air-view--normalize-point-now)))))

(defun org-air-view--quit-close-pane ()
  "Close a live bottom pane as ONE progressive quit step (R28-2).
Shared by `org-air-quit' and `org-air-project-quit' (one helper, never
forked): when the bottom pane window is live, run the R20-3 teardown
from the HOST buffer's context (so the host-local
`org-air-view--pane-indirect' really dies — exactly the discipline
`org-air-view-pane-quit' established), keep focus on the host window,
and return non-nil: the press is handled, the caller STOPS.  Return nil
when no pane is open, so the caller peels its next layer."
  (when (org-air-view-pane--window-live-p)
    (let* ((board (org-air-view-pane--board-window))
           (host (and board (window-buffer board))))
      (if (buffer-live-p host)
          (with-current-buffer host (org-air-view-pane--hide))
        (org-air-view-pane--hide))
      (when (window-live-p board) (select-window board)))
    t))

(defun org-air-view--view-pane-update-now (buf)
  "Redraw the follow view pane for BUF (debounce-timer callback).
Redraws only when the item at point CHANGED and the pane window is live
\(R16 D-P3)."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (and org-air-view-pane-follow
                 (org-air-view--pane-host-p)
                 (org-air-view-pane--window-live-p))
        (let ((item (org-air-view--view-pane-thing-at-point)))
          (unless (eq item org-air-view--view-pane-item)
            (setq-local org-air-view--view-pane-item item)
            (let ((ctx (org-air-view-pane--context-at-point)))
              (when ctx
                (let ((org-air-view-pane-focus nil))
                  (org-air-view-pane--show ctx))))))))))

(defun org-air-view--view-pane-post-command ()
  "Follow hook: schedule a DEBOUNCED bottom-pane update (R16 D-P3).
Separate from the inspector hook; inert under batch.  Mirrors the
inspector's idle-timer model so the snapshot+fontify on each item-change
does not run synchronously on every command (responsive on large files)."
  (when (and (not noninteractive)
             org-air-view-pane-follow
             (org-air-view--pane-host-p)
             (org-air-view-pane--window-live-p)
             ;; R20-3b: cheap early-out — if point has not moved since the
             ;; last fire, a non-motion command never even schedules the
             ;; timer (so `thing-at-point' is not called).
             (not (eql (point) org-air-view--view-pane-last-pos)))
    (setq-local org-air-view--view-pane-last-pos (point))
    (when (timerp org-air-view--view-pane-timer)
      (cancel-timer org-air-view--view-pane-timer))
    (setq org-air-view--view-pane-timer
          (run-with-idle-timer org-air-view-pane-follow-debounce nil
                               #'org-air-view--view-pane-update-now
                               (current-buffer)))))

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
    ;; D-P1: stash the inspector geometry (incl. the FIXED :region-height set
    ;; by `org-air-view--insert-rail' in the rail temp buffer) so the live
    ;; column-only update can re-find + re-fill the reserved region.
    (setq org-air-view--inspector-geom
          (list :item-width item-width :divider divider :rail-width rail-width
                :region-height org-air-view--inspector-region-height))
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
         ;; R27-1 S3: latch the reconciler for the FULL render extent so a
         ;; 0s reconcile timer nesting inside this render (org-ql's file IO
         ;; runs pending timers) can never misread the in-flight popout as
         ;; a user close; the nested run re-arms for after the render.
         (org-air-rail--reconciling t)
         (width (org-air-view--render-width))
         (height (org-air-view--render-height))
         ;; C2/C3: capture the displaying window's live char metrics here,
         ;; in the real buffer (the panes render in temp buffers with no
         ;; window), so every pill is sized to the exact text cell at the
         ;; current font/text-scale.
         (dims (org-air-view--char-dimensions))
         (org-air-view--pill-char-w (car dims))
         (org-air-view--pill-char-h (cdr dims))
         ;; R18 D-P1a: one pill-geometry snapshot per render, shared by every
         ;; pane (folded into the svg image cache key for auto-invalidation
         ;; on any pill-geometry defcustom change).
         (org-air-view--pill-style-sig
          (list org-air-pill-pad-cols org-air-pill-radius
                org-air-pill-fill-alpha org-air-pill-font-scale
                org-air-pill-border-opacity org-air-pill-vinset))
         ;; R20-6: set the render dynamics the partition depends on
         ;; (items/tag-filter) and ensure the classify cache table for today
         ;; (R18 D-P1c) BEFORE composing the panes, THEN compute the
         ;; compute-once partition (the visible set + the bucket->items map in
         ;; ONE classify pass) and bind it for the whole render extent.  The
         ;; pane temp buffers (`org-air-view--render-lines') rebind it, so
         ;; every consumer reads ONE pass instead of re-deriving O(N).
         (_ (progn
              (setq org-air-view--items items
                    org-air-view--items-key (list org-air-files org-air-inbox-file)
                    org-air-view--tag-filter tag-filter)
              (org-air-view--classify-cache-ensure)))
         (org-air-view--render-partition
          (org-air-view--compute-partition items))
         ;; R20-6: the per-render displayed-rows memo (shared by the section
         ;; pass + the meta-width pass so they sort+take each bucket once).
         (org-air-view--render-displayed
          (cons items (make-hash-table :test 'eq))))
    (erase-buffer)
    ;; R20-2 + R20-6: cache the visible count for the status mode-line :eval
    ;; from the compute-once visible set (redisplay never re-scans all items).
    (setq-local org-air-view--mode-line-count
                (length (cadr org-air-view--render-partition)))
    ;; R16 D-P1: seed the per-board popout flag from the INITIAL preference
    ;; on first render only (`unset' sentinel); thereafter the toggle /
    ;; reconciler own it.  The renderer never consults `org-air-rail-style'
    ;; for dispatch again.  R26-5: the per-view `org-air-rail-placement'
    ;; alist seeds too (interactive only; batch keeps `unset' -> nil, or
    ;; the explicit `org-air-rail-style' back-compat force).
    (when (eq org-air-view--rail-popped-out 'unset)
      (setq-local org-air-view--rail-popped-out
                  (or (eq org-air-rail-style 'side-window)
                      (and (not noninteractive)
                           (eq (alist-get 'board org-air-rail-placement)
                               'side-window)))))
    ;; R13 D-P3: below `org-air-rail-min-width' drop the rail entirely
    ;; (board-only); else the existing two-pane vs stacked decision.
    ;; R16 D-P1: `side-window' is now driven by the RUNTIME flag (a user
    ;; popped the rail out), never an unconditional render mode — so
    ;; `window-toggle-side-window' is respected.
    (setq org-air-view--orientation
          (cond
           ((org-air-view--board-only-p width) 'board-only)
           ((org-air-rail--popped-p) 'side-window)
           ((org-air-view--two-pane-p width) 'two-pane)
           (t 'stacked)))
    ;; R15 D-P2 / R27-2: under `side-window' create the rail side window
    ;; BEFORE composing the board body, then compose at the board window's
    ;; REAL body width (the shared `org-air-rail--host-width' helper — the
    ;; rail cols are frame-derived, so the geometry settles in one step and
    ;; the render tail cannot resize it after composition).  Skipped when
    ;; the width seam is set (deterministic batch goldens: the seam IS the
    ;; board width and window geometry is unreliable in batch).
    (when (and (eq org-air-view--orientation 'side-window)
               (not org-air-view-width))
      (setq width (org-air-rail--host-width (current-buffer) width)))
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
            (cond
             ;; R13 D-P3: board-only — full-width item pane, NO rail /
             ;; calendar / inspector.
             ((memq org-air-view--orientation '(board-only side-window))
              (org-air-view--render-lines
               width
               (lambda () (org-air-view--insert-item-pane items width))))
             ((eq org-air-view--orientation 'two-pane)
              ;; D-P1: the inspector now lives INSIDE the rail (fixed
              ;; reserved mid-rail region), not as an appended foot band.
              (let ((pair (org-air-view--two-pane-body items width)))
                (setq fill-row (cdr pair))
                (car pair)))
             (t
              (org-air-view--render-lines
               width
               (lambda ()
                 (org-air-view--insert-top-rail items width)
                 (insert "\n")
                 (org-air-view--insert-rule)
                 (insert "\n")
                 (org-air-view--insert-item-pane items width))))))
           (body-content (org-air-view--collapse-line-list body-content))
           (body-target (max (length body-content)
                             (- height (length header) (length footer))))
           (body (org-air-view--pad-line-list body-content body-target fill-row)))
      ;; R18 D-P1b: bracket the body band with markers and remember the
      ;; target floor + fill row, so the TAB-expand section splice can redraw
      ;; only the body (never the header/footer) and reproduce the EXACT
      ;; full-render body height.
      (setq-local org-air-view--body-target-floor
                  (- height (length header) (length footer))
                  org-air-view--body-fill-row fill-row)
      (org-air-view--insert-lines header)
      (setq-local org-air-view--body-beg (point-marker))
      (org-air-view--insert-lines body)
      (setq-local org-air-view--body-end (point-marker))
      (org-air-view--insert-lines footer))
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
    (org-air-view--goto-first-item)
    ;; D-P7: locate the inspector band, set its markers, sync to the item
    ;; the cursor landed on (the live hook keeps it synced thereafter).
    (org-air-view--setup-inspector)
    ;; R15 D-P2: side-window rail lifecycle.  `side-window' shows/refreshes
    ;; the rail side window; board-only deletes it (responsive teardown);
    ;; the inline paths leave any rail buffer untouched (none under inline).
    (cond
     ((eq org-air-view--orientation 'side-window)
      (org-air-rail--show (current-buffer) width))
     ((eq org-air-view--orientation 'board-only)
      (org-air-rail--hide (current-buffer))))
    ;; R25-6: an INLINE (two-pane/stacked) self-render must also drop a
    ;; stale side rail owned by ANOTHER view (the cross-view sweep).  When
    ;; SELF is popped `--show' already re-owned the window, so this no-ops.
    (org-air-rail--evict-foreign-rail (current-buffer))))

;;;; ---------------------------------------------------------------------
;;;; R18 D-P1b incremental render — redraw only the changed body / calendar.
;;;; ---------------------------------------------------------------------

(defun org-air-view--postprocess-line (line width)
  "Return LINE post-processed to match a full render's output (R18 D-P1b).
Mirror `org-air-view--normalize-buffer-lines' (pad to the fixed width seam)
or `org-air-view--finalize-buffer-lines' (cap to WIDTH + right-trim) so a
spliced line is byte-identical to the corresponding full-render line."
  (if (integerp org-air-view-width)
      (org-air-view--pad-to line org-air-view-width)
    (let ((capped (if (> (string-width line) width)
                      (truncate-string-to-width
                       line width nil nil (org-air-view--glyph 'more))
                    line)))
      (string-trim-right capped))))

(defun org-air-view--body-region ()
  "Return (BEG . END) buffer positions of the live body band, or nil (R18)."
  (when (and (markerp org-air-view--body-beg)
             (markerp org-air-view--body-end)
             (eq (marker-buffer org-air-view--body-beg) (current-buffer))
             (eq (marker-buffer org-air-view--body-end) (current-buffer))
             (marker-position org-air-view--body-beg)
             (marker-position org-air-view--body-end)
             (<= (marker-position org-air-view--body-beg)
                 (marker-position org-air-view--body-end)))
    (cons (marker-position org-air-view--body-beg)
          (marker-position org-air-view--body-end))))

(defun org-air-view--render-section (_bucket)
  "Redraw only the changed board body region after a section toggled (R18 D-P1b).
Recomposes the body the SAME way `org-air-view--render' does (cheap: the
classify + pill caches make it text-only work), then rewrites ONLY from the
first line that actually changed to the end of the body band — the header,
the footer and every earlier section stay byte-untouched, and the body
height is reproduced from the stored target floor (S6).  The result is
byte-identical to a full render with the same `org-air-view--expanded-
sections' (proved by the equivalence golden).  Falls back to the cheap full
render when the body band is unknown.  _BUCKET is accepted for API symmetry."
  (let ((region (org-air-view--body-region)))
    (if (not region)
        (org-air-view--render-current)
      (let* ((token (org-air-view--save-position))
             (width org-air-view--rendered-width)
             (items org-air-view--items)
             (dims (org-air-view--char-dimensions))
             (org-air-view--pill-char-w (car dims))
             (org-air-view--pill-char-h (cdr dims))
             (org-air-view--pill-style-sig
              (list org-air-pill-pad-cols org-air-pill-radius
                    org-air-pill-fill-alpha org-air-pill-font-scale
                    org-air-pill-border-opacity org-air-pill-vinset))
             (beg (car region))
             (end (cdr region))
             (old-lines (split-string (buffer-substring-no-properties beg end)
                                      "\n"))
             (raw (org-air-view--render-lines
                   width
                   (lambda () (org-air-view--insert-item-pane items width))))
             (body-content (org-air-view--collapse-line-list raw))
             (body-target (max (length body-content)
                               (or org-air-view--body-target-floor 0)))
             (body (org-air-view--pad-line-list
                    body-content body-target
                    (or org-air-view--body-fill-row "")))
             (new-lines (mapcar (lambda (l)
                                  (org-air-view--postprocess-line l width))
                                body))
             ;; common prefix: every byte-identical leading line (header +
             ;; earlier sections) is left untouched.
             (p 0)
             (olen (length old-lines))
             (nlen (length new-lines)))
        (org-air-view--classify-cache-ensure)
        (while (and (< p olen) (< p nlen)
                    (string= (nth p old-lines) (nth p new-lines)))
          (setq p (1+ p)))
        ;; Replace [start-of-old-line-p, body-end) with the new tail.  Buffer
        ;; positions are char-based; lines 0..p-1 each contribute len+1 chars
        ;; (text + newline) EXCEPT the body's last line carries no trailing
        ;; newline, so the running offset is clamped to END.
        (let* ((inhibit-read-only t)
               (pos beg))
          (dotimes (i p)
            (setq pos (+ pos (length (nth i old-lines)) 1)))
          (setq pos (min pos end))
          (delete-region pos end)
          (goto-char pos)
          (let ((tail (nthcdr p new-lines)))
            (when tail
              ;; appending after a kept last line (no trailing newline).
              (when (and (> pos beg) (not (eq (char-before pos) ?\n)))
                (insert "\n"))
              (insert (mapconcat #'identity tail "\n"))))
          ;; The body is the last band in board-only/side-window, so it must
          ;; not end with a newline; drop a stray one left by a pure deletion.
          (when (and (> (point) beg) (eq (char-before (point)) ?\n))
            (delete-char -1))
          (set-marker org-air-view--body-end (point)))
        (org-air-view--restore-position token)))))

(defun org-air-view--render-calendar ()
  "Re-render ONLY the side-window rail buffer for the new month (R18 D-P1b).
The board buffer is byte-untouched: month-nav changes only
`org-air-view--cal-month', which the rail reads through its back-pointer.
Falls back to the cheap full render when the rail is not a side window."
  (if (eq org-air-view--orientation 'side-window)
      (org-air-rail--show (current-buffer) (org-air-view--render-width))
    (org-air-view--render-current)))

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

(defun org-air-view--restore-to-column (token)
  "Move point to TOKEN's saved :column on the CURRENT row, clamped (R21-1).
Never lands before the row's first visible glyph (so the cursor still reads
on a real character, S5a); if the saved column runs past a now-narrower
row's content it lands on the row's last visible glyph rather than the
trailing newline.  This is what makes a point-preserving re-render keep
the column the user was on instead of snapping to the row's leftmost glyph."
  (org-air-view--beginning-of-visible)
  (let ((first (current-column))
        (want  (or (plist-get token :column) 0)))
    (move-to-column (max first want))
    (when (and (eolp) (> (current-column) first))
      (backward-char 1))))

(defun org-air-view--restore-position (token)
  "Restore the cursor to the location described by TOKEN (D5).
Prefers the same item; if it vanished (refiled/done), lands on the
nearest surviving item in the same section, then the section heading,
falling back to the same line/column — never jumping to `point-min'
unless nothing else is available.  In every branch point is restored to
TOKEN's saved column (clamped), so a re-render genuinely preserves point
\(R21-1)."
  (let ((marker-pos (org-air-view--find-property
                     'org-air-marker (plist-get token :marker)))
        (section-pos (org-air-view--find-property
                      'org-air-section (plist-get token :section))))
    (cond
     (marker-pos
      ;; SAME item survived: this is the point-preservation case.  Restore
      ;; the saved column so the user stays on the SAME glyph (R21-1), not
      ;; snapped to the row's leftmost glyph.
      (goto-char marker-pos)
      (org-air-view--restore-to-column token))
     (section-pos
      ;; Item vanished, its section survived: land on the nearest surviving
      ;; item.  No same-glyph to preserve, so use the safe first-visible
      ;; landing (R21-2 routes this to the title).
      (goto-char (or (text-property-not-all section-pos (point-max)
                                            'org-air-item nil)
                     section-pos))
      ;; R21-2: one consistent landing rule — the surviving item's TITLE.
      (org-air-view--goto-row-title))
     (t
      ;; Item AND section vanished (a re-query after auto-refresh rebuilds
      ;; markers, or a filter emptied the board): land on the saved line.
      ;; The saved column is not preserved here — it belonged to a row that
      ;; no longer exists; land on the TITLE (R21-2), falling back to the
      ;; first visible char for a heading / empty board.
      (goto-char (point-min))
      (forward-line (1- (or (plist-get token :line) 1)))
      (org-air-view--goto-row-title)))
    ;; R29-2: the restore tail normalizes EXPLICITLY (no hook timing
    ;; games) — a restored DEAD column (the gutter before the title) is
    ;; corrected immediately, not on the next keystroke.  Idempotent on
    ;; any on/after-title column, so R21-1's preserved column survives.
    (org-air-view--normalize-point-now)))

(defun org-air-view--render-current ()
  "Re-render the dashboard from `org-air-view--items', preserving point.
Filters, scope and the calendar month are buffer-local and survive; the
cursor is restored to the same item (or section, or line) afterwards.
R27-1 S4 (the single-scanner law): while the R26-8 machine is REFRESHING
this never runs the synchronous fallback query — a mid-refresh repaint
renders the items AS-IS (the swap repaints once at the end), and a COLD
refresh (`org-air-view--loading' t) repaints the loading skeleton — so
the slice machine can never be raced by a second concurrent scan (whose
`find-file-noselect'/`normal-mode' wipes the very buffer-locals the
in-flight slice's org-ql predicate is reading)."
  (cond
   ((and (eq org-air-view--refresh-state 'refreshing)
         org-air-view--loading)
    (org-air-view--render-loading))
   ((eq org-air-view--refresh-state 'refreshing)
    (org-air-view--refresh-repaint))
   (t
    (let ((token (org-air-view--save-position)))
      (org-air-view--render (or org-air-view--items (org-air-query-items))
                            org-air-view--tag-filter)
      (org-air-view--restore-position token)))))

(defun org-air-view--resize-refresh ()
  "Re-render only when the displaying window's width or height changed.
Called from the debounced window-size/-configuration hook (S6 makes the
body fill the height, so a height change must re-pad too)."
  (let ((width (org-air-view--render-width))
        (height (org-air-view--render-height)))
    (unless (and (eql width org-air-view--rendered-width)
                 (eql height org-air-view--rendered-height))
      (org-air-view--render-current))))

(defun org-air-view--render-loading (&optional _progress)
  "Paint the chrome-only loading skeleton for the cold fast-paint load (R20-1).
Reuses the banner + rule + footer bands at `board-only' orientation with a
single centred \"Loading your board…\" body line.  `org-air-view' paints
this once and forces it visible with `redisplay' so the frame appears
within one paint, BEFORE the synchronous query runs."
  (let* ((inhibit-read-only t)
         (width (org-air-view--render-width))
         (height (org-air-view--render-height))
         ;; Bind the SAME pill-metrics/style env as `org-air-view--render'
         ;; so the skeleton sizes to the live window font (no pill is drawn
         ;; here, but the bands measure consistently).
         (dims (org-air-view--char-dimensions))
         (org-air-view--pill-char-w (car dims))
         (org-air-view--pill-char-h (cdr dims))
         (org-air-view--pill-style-sig
          (list org-air-pill-pad-cols org-air-pill-radius
                org-air-pill-fill-alpha org-air-pill-font-scale
                org-air-pill-border-opacity org-air-pill-vinset)))
    (erase-buffer)
    (let* ((header (org-air-view--render-lines
                    width
                    (lambda ()
                      (org-air-view--insert-banner nil)
                      (org-air-view--insert-rule)
                      (insert "\n"))))
           (footer (if org-air-show-footer
                       (org-air-view--render-lines
                        width
                        (lambda ()
                          (org-air-view--insert-rule)
                          (org-air-view--insert-footer)))
                     nil))
           (msg (propertize "Loading your board…" 'face 'org-air-face-faded))
           (pad (max 0 (/ (- width (string-width msg)) 2)))
           (centred (concat (make-string pad ?\s) msg))
           (body-content (list "" centred))
           (body-target (max (length body-content)
                             (- height (length header) (length footer))))
           (body (org-air-view--pad-line-list body-content body-target "")))
      (org-air-view--insert-lines header)
      (org-air-view--insert-lines body)
      (org-air-view--insert-lines footer))
    (if (integerp org-air-view-width)
        (org-air-view--normalize-buffer-lines org-air-view-width)
      (org-air-view--finalize-buffer-lines width))
    (goto-char (point-max))
    (when (and (bolp) (> (point-max) (point-min)))
      (delete-char -1))
    (goto-char (point-min))))

(defun org-air-view--short-error (err)
  "Return a bounded single-line human string for the load error ERR (R20-1).
The first line of `error-message-string', capped at 160 chars.  org/org-ql
errors routinely carry large data payloads (a re-query of N items can make
`error-message-string' six figures long); this guarantees the cold-load
failure message can never become the 101 802-char echo-area dump the bare
timer path produced — a truncated, single-line, human message."
  (let* ((msg (error-message-string err))
         (line (car (split-string msg "\n"))))
    (if (> (length line) 160)
        (concat (substring line 0 160) "…")
      line)))

(defun org-air-view--loading-guard ()
  "Soft-error during the brief synchronous fast-paint window (R20-1).
Guards the data-dependent commands (filter/scope/TAB) so they never act on
an empty skeleton; navigation over the skeleton stays harmless.  Now
near-inert: the load window is a single synchronous body the user cannot
interrupt, but the guard is cheap and harmless."
  (when org-air-view--loading
    (user-error "Still loading your board…")))

;;;; ---------------------------------------------------------------------
;;;; R26-8 — cache-first async: disk cache + token-guarded chunked refresh.
;;;; ---------------------------------------------------------------------

(defconst org-air-view--cache-version 1
  "Serialisation version of `org-air-cache-file' (R26-8).  Bump = discard.")

(defun org-air-view--item-pos (item)
  "Return a position for ITEM valid inside its source file's buffer.
Accepts a live marker OR the cache-hydrated (FILE . POS) cons in the
marker slot (R26-8) — the cons is a startup-window state; the completed
refresh swap replaces cached items with live-marker ones."
  (let ((m (org-air-item-marker item)))
    (cond ((markerp m) m)
          ((consp m) (or (cdr m) 1))
          (t m))))

(defun org-air-view--item-serialise (item)
  "Return a printable copy of ITEM: the marker slot becomes (FILE . POS)."
  (let* ((copy (copy-sequence item))
         (m (org-air-item-marker item)))
    (setf (org-air-item-marker copy)
          (if (markerp m)
              (cons (org-air-item-file item) (marker-position m))
            m))
    copy))

(defun org-air-view--cache-write (items mtimes)
  "Persist ITEMS + the MTIMES snapshot to `org-air-cache-file' (R26-8).
Atomic (temp file + rename); a nil `org-air-cache-file' disables
persistence; any write error is swallowed (the cache is an optimisation,
never a failure source)."
  (when org-air-cache-file
    (ignore-errors
      (let* ((file (expand-file-name org-air-cache-file))
             (dir (file-name-directory file))
             (tmp (progn (make-directory dir t)
                         (make-temp-file (concat file ".") nil ".tmp"))))
        (let ((print-length nil) (print-level nil) (print-circle t))
          (write-region
           (prin1-to-string
            (list :version org-air-view--cache-version
                  :key (list org-air-files org-air-inbox-file)
                  :mtimes mtimes
                  :items (mapcar #'org-air-view--item-serialise items)))
           nil tmp nil 'silent))
        (rename-file tmp file t)))))

(defun org-air-view--cache-read ()
  "Read `org-air-cache-file'; return its plist, or nil.
A missing/corrupt file, a `:version' bump or a `:key' (config) mismatch
are all silently \"no cache\" — the cold path."
  (when (and org-air-cache-file
             (file-readable-p (expand-file-name org-air-cache-file)))
    (condition-case nil
        (let ((data (with-temp-buffer
                      (insert-file-contents
                       (expand-file-name org-air-cache-file))
                      (read (current-buffer)))))
          (and (listp data)
               (eql (plist-get data :version) org-air-view--cache-version)
               (equal (plist-get data :key)
                      (list org-air-files org-air-inbox-file))
               (listp (plist-get data :items))
               data))
      (error nil))))

(defun org-air-view--cache-load ()
  "Return (ITEMS . STALE-FILES) from the persisted cache, or nil.
ITEMS carry cons (FILE . POS) marker slots (hydrated on demand by
`org-air-view--item-pos').  STALE-FILES is the list of configured files
whose mtime diverged from the snapshot — new files and snapshot files now
missing included — nil when every mtime matches (FRESH: no scan at all)."
  (when-let* ((data (org-air-view--cache-read)))
    (let* ((files (org-air-query-files))
           (mtimes (plist-get data :mtimes))
           (stale (seq-remove
                   (lambda (f)
                     (equal (cdr (assoc f mtimes))
                            (file-attribute-modification-time
                             (file-attributes f))))
                   files)))
      ;; a snapshot file that vanished also invalidates (its rows linger).
      (dolist (entry mtimes)
        (unless (member (car entry) files)
          (push (car entry) stale)))
      (cons (plist-get data :items) stale))))

(defun org-air-view--refresh-repaint ()
  "Repaint the board from `org-air-view--items' as-is, preserving point.
Unlike `org-air-view--render-current' this never falls back to a
synchronous query when the items are nil (the cold/failed machine states
must not re-block the frame)."
  (let ((token (org-air-view--save-position)))
    (org-air-view--render org-air-view--items org-air-view--tag-filter)
    (org-air-view--restore-position token)))

(defun org-air-view--refresh-cancel ()
  "Invalidate any in-flight refresh: bump the token, cancel the timer.
Every pending slice callback carries the old token and self-cancels."
  (cl-incf org-air-view--refresh-token)
  (when (timerp org-air-view--refresh-timer)
    (cancel-timer org-air-view--refresh-timer))
  (setq org-air-view--refresh-timer nil
        org-air-view--refresh-queue nil
        org-air-view--refresh-acc nil))

(defun org-air-view--refresh-teardown ()
  "Cancel the in-flight refresh outright (the board buffer is dying)."
  (org-air-view--refresh-cancel)
  (setq org-air-view--refresh-state nil))

(defun org-air-view--refresh-schedule (buffer token)
  "Schedule the next refresh slice for BUFFER under TOKEN (R26-8).
One-shot idle timer, re-armed from each completed slice; relative to the
current idleness so a chain that starts while Emacs is already idle keeps
running.  Never schedules under `noninteractive' — the deterministic ERTs
call `org-air-view--refresh-run-slice' directly instead."
  (unless noninteractive
    (with-current-buffer buffer
      (setq org-air-view--refresh-timer
            (run-with-idle-timer
             (time-add (or (current-idle-time) 0) 0.05) nil
             #'org-air-view--refresh-run-slice buffer token)))))

(defun org-air-view--refresh-start ()
  "Enter REFRESHING for the current board buffer; return the new token.
Cancels any in-flight refresh (its slices go stale via the token), queues
the CURRENT file list — `g' is exactly this same path — and schedules the
first slice.  The caller paints (board or skeleton) after this so the
header marker/progress is visible from the first paint."
  (org-air-view--refresh-cancel)
  (setq org-air-view--refresh-queue (org-air-query-files)
        org-air-view--refresh-total (length org-air-view--refresh-queue)
        org-air-view--refresh-acc nil
        org-air-view--refresh-mtimes nil
        org-air-view--refresh-state 'refreshing)
  (if (null org-air-view--refresh-queue)
      (org-air-view--refresh-finish)
    (org-air-view--refresh-schedule (current-buffer)
                                    org-air-view--refresh-token))
  org-air-view--refresh-token)

(defun org-air-view--refresh-finish ()
  "All slices done: swap ONCE, re-render, clear the marker, write the cache.
The single-swap rule (no partial paints): the accumulated items replace
`org-air-view--items' in one motion, the board repaints once with point
preserved, and the machine returns to FRESH."
  (let ((items org-air-view--refresh-acc)
        (mtimes org-air-view--refresh-mtimes))
    (setq org-air-view--items items
          org-air-view--items-key (list org-air-files org-air-inbox-file)
          org-air-view--classify-cache nil
          org-air-view--refresh-state nil
          org-air-view--refresh-acc nil
          org-air-view--refresh-queue nil
          org-air-view--refresh-total 0
          org-air-view--cache-stale-files nil
          org-air-view--loading nil)
    (org-air-view--refresh-repaint)
    (org-air-view--cache-write items mtimes)))

(defun org-air-view--refresh-run-slice (buffer token)
  "Scan ONE slice of BUFFER's pending refresh queue under TOKEN (R26-8).
The named slice runner the idle timer schedules — ERTs call it directly in
a loop, so the whole machine is testable synchronously with zero timers.
Robustness rules made law: a stale TOKEN (or dead BUFFER, or a machine no
longer refreshing) is a silent no-op; slices accumulate privately and
NEVER touch windows; a slice error keeps the painted board, flips to
FAILED (header: `refresh failed (g retries)') and always clears
`org-air-view--loading' so the buffer can never wedge."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (eq token org-air-view--refresh-token)
                 (eq org-air-view--refresh-state 'refreshing))
        (setq org-air-view--refresh-timer nil)
        (condition-case err
            (let* ((slice (seq-take org-air-view--refresh-queue
                                    (max 1 org-air-refresh-files-per-slice))))
              ;; mtime captured per file AT SCAN TIME (the cache snapshot).
              (dolist (f slice)
                (push (cons f (file-attribute-modification-time
                               (file-attributes f)))
                      org-air-view--refresh-mtimes))
              (setq org-air-view--refresh-acc
                    ;; copy: org-ql may hand back a CACHED list object —
                    ;; nconc'ing it would mutate the cache (and a repeat
                    ;; scan would then build a circular list).
                    (nconc org-air-view--refresh-acc
                           (copy-sequence
                            (org-air-query-items-in-files slice)))
                    org-air-view--refresh-queue
                    (nthcdr (length slice) org-air-view--refresh-queue))
              (if org-air-view--refresh-queue
                  ;; more to do: re-arm; the buffer text is NOT touched
                  ;; between slices (single-swap rule).
                  (org-air-view--refresh-schedule buffer token)
                (org-air-view--refresh-finish)))
          (error
           (setq org-air-view--refresh-state 'failed
                 org-air-view--refresh-queue nil
                 org-air-view--refresh-acc nil
                 org-air-view--loading nil)
           (org-air-view--refresh-repaint)   ; same board + honest header
           (message "org-air: refresh failed: %s (g retries)"
                    (org-air-view--short-error err))))))))

(defun org-air-view--refresh-stale-item-guard (item)
  "Soft-error on a triage verb for ITEM while its file is mid-refresh (R26-8).
Only an item whose source file's mtime diverged from the cache snapshot is
blocked (its cached position may be wrong); positions in unchanged files
are valid by construction (mtime match), so triage there stays live."
  (when (and (eq org-air-view--refresh-state 'refreshing)
             (member (org-air-item-file item) org-air-view--cache-stale-files))
    (user-error "Still refreshing this file…")))

;;;###autoload
(defun org-air-view ()
  "Open the org-air dashboard buffer.
R26-8 cache-first async: an interactive start with a valid persisted cache
\(`org-air-cache-file') paints the FULL last-known board instantly — all
mtimes matching means FRESH (no scan at all); any divergence starts the
token-guarded chunked refresh with the `stale · refreshing…' header
marker.  An interactive COLD start (no cache) paints the chrome skeleton
and runs the same chunked refresh (header slice progress; input live; the
data-dependent verbs guarded by `org-air-view--loading-guard' until the
single swap).  A re-open with the in-buffer item cache warm, or any
`noninteractive' (batch) call, takes the EXACT synchronous path so every
byte fixture is produced exactly as before and the gate never reads or
writes the cache file.  Errors surface as a single truncated line; the
buffer can never wedge in a loading state."
  (interactive)
  (let* ((buffer (get-buffer-create org-air-view-buffer-name))
         (cached (with-current-buffer buffer
                   ;; R26-5: IDEMPOTENT entry — re-running the mode on a
                   ;; live buffer runs `kill-all-local-variables' and wipes
                   ;; the whole session (rail placement, sort, filter...).
                   ;; Initialise only when not already in the mode (the
                   ;; same guard as the project entry; one discipline).
                   (unless (derived-mode-p 'org-air-view-mode)
                     (org-air-view-mode))
                   (and org-air-view--items
                        (equal org-air-view--items-key
                               (list org-air-files org-air-inbox-file))))))
    ;; Display the buffer first so width derivation measures the window
    ;; that actually shows the dashboard (U1), in a full-width window so
    ;; the rail/calendar are never pushed off-screen (D4).
    (pop-to-buffer buffer
                   (or org-air-display-action
                       '((display-buffer-reuse-window
                          display-buffer-same-window
                          display-buffer-full-frame))))
    (with-current-buffer buffer
      (cond
       ;; Cache hit, or batch/noninteractive (the byte goldens never see the
       ;; fast-paint path): synchronous — unchanged behaviour, byte-stable.
       ((or cached noninteractive)
        (unless cached
          ;; R18 D-P1c: fresh structs from a re-query invalidate the
          ;; classify cache (old `eq' entries can never be wrongly hit, but
          ;; drop them).
          (setq org-air-view--items (org-air-query-items)
                org-air-view--classify-cache nil))
        (org-air-view--render org-air-view--items org-air-view--tag-filter))
       ;; R26-8 CACHED: a valid persisted cache paints the FULL last-known
       ;; board instantly.  All mtimes match -> FRESH, no scan at all; any
       ;; divergence -> REFRESHING (chunked slices; `stale · refreshing…'
       ;; marker in the count slot from this very first paint).  Nothing
       ;; modal remains on this path (`--loading' stays nil).
       ((when-let* ((cache (org-air-view--cache-load)))
          (setq org-air-view--items (car cache)
                org-air-view--items-key (list org-air-files
                                              org-air-inbox-file)
                org-air-view--classify-cache nil
                org-air-view--cache-stale-files (cdr cache))
          (when (cdr cache)
            (org-air-view--refresh-start))
          (org-air-view--render org-air-view--items org-air-view--tag-filter)
          t))
       ;; R26-8 COLD (no cache): honest fast paint of the chrome skeleton,
       ;; then the SAME chunked refresh — input stays live over the
       ;; skeleton (the R20-1 synchronous wait retires); `--loading' guards
       ;; the data-dependent verbs until the machine's single swap (which
       ;; always clears it, success or failure — never a wedge).  Any error
       ;; starting the machine falls back to the R20-1 discipline: a single
       ;; truncated message + the empty board.
       (t
        (setq org-air-view--loading t)
        (condition-case err
            (progn
              (org-air-view--refresh-start)  ; state first: header shows 0/N
              (org-air-view--render-loading)
              (redisplay t))
          (error
           (setq org-air-view--items nil
                 org-air-view--classify-cache nil
                 org-air-view--loading nil)
           (org-air-view--render nil org-air-view--tag-filter)
           (message "org-air: load failed: %s"
                    (org-air-view--short-error err)))))))))

(defun org-air-refresh ()
  "Re-query files and refresh the current org-air dashboard.
Preserves the active filter and the cursor's place.  R26-8: an
interactive `g' on the board IS the refresh machine — it cancels any
pending slices (token bump) and restarts the chunked scan from the
current file list, keeping the painted board (with the header marker)
until the single swap.  Under `noninteractive' (the byte gate) it is the
exact synchronous re-query it always was."
  (interactive)
  (if (and (not noninteractive) (eq major-mode 'org-air-view-mode))
      (progn
        (setq org-air-view--cache-stale-files nil)
        (org-air-view--refresh-start)
        ;; repaint so the `refreshing…' marker shows; the body is the same
        ;; items (byte-identical rows), point preserved.
        (when (eq org-air-view--refresh-state 'refreshing)
          (org-air-view--refresh-repaint)))
    (let ((token (org-air-view--save-position))
          (filter org-air-view--tag-filter))
      ;; a completed synchronous re-query supersedes any machine state
      ;; (stale slices go stale via the token; failed/stale markers clear).
      (org-air-view--refresh-teardown)
      (setq org-air-view--cache-stale-files nil)
      ;; R18 D-P1c: a re-query builds fresh item structs, so drop the
      ;; classify cache (bounds memory; picks up a changed classify-tuning
      ;; defcustom on the next refresh).
      (setq org-air-view--items (org-air-query-items)
            org-air-view--classify-cache nil)
      (org-air-view--render org-air-view--items filter)
      (org-air-view--restore-position token))))

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

(defun org-air-view--read-filter (candidate-tags)
  "Prompt for a tag filter PRE-FILLED with the active one (R18 D-P2/D-P3).
CANDIDATE-TAGS is the completion vocabulary (board item tags, or project
doc tags).  View-agnostic: shared by `org-air-filter' and
`org-air-project-filter' so the pre-fill + AND default + `M-/' toggle are
coded once."
  (completing-read-multiple
   "Filter (#tag or text): " candidate-tags nil nil
   (when (org-air-view--filter-tags)
     (mapconcat #'identity (org-air-view--filter-tags) ","))))

(defun org-air-view--rerender-current-view ()
  "Re-render whichever org-air view is current: board or project (R18 D-P3).
The shared filter commands (`org-air-filter-clear',
`org-air-filter-toggle-match') re-render through this so they work in BOTH
the board and the project view without a hard dependency on
org-air-project (resolved by `fboundp')."
  (if (and (derived-mode-p 'org-air-project-mode)
           (fboundp 'org-air-project-refresh))
      (org-air-project-refresh)
    (org-air-view--render-current)))

(defun org-air-filter (tags)
  "Filter dashboard to TAGS, a comma-separated or list value.
R18 D-P2: the prompt is PRE-FILLED with the active filter so each
invocation continues narrowing instead of restarting; edit/extend the
pre-filled value, or clear it to drop the filter."
  (interactive
   (progn
     (org-air-view--loading-guard)
     (list (org-air-view--read-filter
            (delete-dups (sort (seq-mapcat #'org-air-item-tags org-air-view--items)
                               #'string<))))))
  (setq org-air-view--tag-filter (unless (null tags) tags))
  (org-air-view--render-current))

(defun org-air-filter-by-tag (tag)
  "Compatibility wrapper: filter dashboard to TAG.
R18 D-P2: pre-fills with the first active filter tag (empty clears)."
  (interactive (list (read-string "Tag filter (empty clears): "
                                  (car (org-air-view--filter-tags)))))
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
  "Clear tag filters (shared by the board + project views, R18 D-P3)."
  (interactive)
  (setq org-air-view--tag-filter nil)
  (org-air-view--rerender-current-view))

(defun org-air-filter-toggle-match ()
  "Toggle the multi-tag filter combinator: AND (`all') <-> OR (`any').
R18 D-P2: `all' means every active tag must match (narrow); `any' means
any one matches (widen).  Re-renders the current view (board or project,
R18 D-P3) and echoes the new mode.  Bound to `M-/' in both maps; the
banner/rail/project header show the active combinator beside the chips."
  (interactive)
  (setq org-air-filter-match (if (eq org-air-filter-match 'all) 'any 'all))
  (org-air-view--rerender-current-view)
  (message "Filter match: %s" (if (eq org-air-filter-match 'all) "AND" "OR")))

(defun org-air-view--filter-combinator-word ()
  "Return the active filter combinator as the literal word AND or OR.
R18 D-P2: TTY-safe and deterministic for byte goldens."
  (if (eq org-air-filter-match 'all) "AND" "OR"))

(defun org-air-scope (scope)
  "Scope dashboard to SCOPE."
  (interactive
   (progn
     (org-air-view--loading-guard)
     (let* ((groups (delete-dups (delq nil (mapcar #'org-air-item-group org-air-view--items))))
            (files (delete-dups (mapcar #'org-air-item-file org-air-view--items)))
            ;; R19-4d: Scope is a purely STRUCTURAL lens now — all / @group /
            ;; ⌂ file.  The `#tag' option is DROPPED here (it overlapped the
            ;; live tag Filter, which does tags better: multi-tag + AND/OR +
            ;; live).  Tags belong entirely to `/' (`org-air-filter').
            (candidates (append '("all")
                                (mapcar (lambda (g) (concat "@" g)) groups)
                                (mapcar (lambda (file) (concat "⌂ " (file-name-nondirectory file))) files)))
            (choice (completing-read "Scope: " candidates nil t)))
       (list choice))))
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
  "Move point down one line, landing on its title (R3, vim j; R21-2)."
  (interactive)
  (forward-line 1)
  (org-air-view--goto-row-title))

(defun org-air-prev-line ()
  "Move point up one line, landing on its title (R3, vim k; R21-2)."
  (interactive)
  (forward-line -1)
  (org-air-view--goto-row-title))

(defun org-air-next-item ()
  "Move point to the next item row, landing on its title (R21-2)."
  (interactive)
  (let ((pos (next-single-property-change (point) 'org-air-item nil (point-max))))
    (while (and pos (not (get-text-property pos 'org-air-item)) (< pos (point-max)))
      (setq pos (next-single-property-change pos 'org-air-item nil (point-max))))
    (when pos
      (goto-char pos)
      (org-air-view--goto-row-title))))

(defun org-air-prev-item ()
  "Move point to the previous item row, landing on its title (R21-2)."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'org-air-item nil (point-min))))
    (while (and pos (not (get-text-property (max (point-min) (1- pos)) 'org-air-item)) (> pos (point-min)))
      (setq pos (previous-single-property-change pos 'org-air-item nil (point-min))))
    (when pos
      (goto-char (max (point-min) (1- pos)))
      (org-air-view--goto-row-title))))

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
  (org-air-view--loading-guard)
  (let ((bucket (org-air-view--line-section)))
    (if (not bucket)
        (org-air-next-section)
      (setq org-air-view--expanded-sections
            (if (memq bucket org-air-view--expanded-sections)
                (delq bucket org-air-view--expanded-sections)
              (cons bucket org-air-view--expanded-sections)))
      ;; R18 D-P1b: in the full-width item-pane layouts (board-only /
      ;; side-window) the toggled section is splice-replaceable in place
      ;; (the rail is a separate buffer or absent), so redraw only the body
      ;; band; the column-composed layouts fall back to the now-cheap full
      ;; render (correctness first).
      (if (memq org-air-view--orientation '(board-only side-window))
          (org-air-view--render-section bucket)
        (org-air-view--render (or org-air-view--items (org-air-query-items))
                              org-air-view--tag-filter))
      (let ((pos (org-air-view--find-property 'org-air-section bucket)))
        (when pos
          (goto-char pos)
          ;; A section header has no title mark, so this falls back to
          ;; first-visible (R21-2) — point stays on the header.
          (org-air-view--goto-row-title))))))

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
  (let* ((item (org-air-view--item-at-point))
         (tag (read-string "Tag: ")))
    (org-air-view--refresh-stale-item-guard item)
    (with-current-buffer (find-file-noselect (org-air-item-file item))
      (goto-char (org-air-view--item-pos item))
      (org-back-to-heading t)
      (org-toggle-tag tag 'on)
      (save-buffer)))
  (org-air-refresh))

(defun org-air-set-schedule (date)
  "Set SCHEDULED DATE on the item at point."
  (interactive "sSchedule (empty clears): ")
  (let ((item (org-air-view--item-at-point)))
    (org-air-view--refresh-stale-item-guard item)
    (with-current-buffer (find-file-noselect (org-air-item-file item))
      (goto-char (org-air-view--item-pos item))
      (org-back-to-heading t)
      (org-schedule nil (unless (string-empty-p date) date))
      (save-buffer)))
  (org-air-refresh))

;;;; Inbox triage — inline dispositions + process-inbox (org-air-triage.org)

(defvar org-air-view--triage-source-buffer nil
  "Source buffer of the most recent triage disposition (for `u' undo).")

(defun org-air-view--item-at-point ()
  "Return the org-air item at point, or signal a `user-error'.
R22-2: line-based so a row action resolves from ANY column on the row (the
leading margin / rail / trailing pad carry no item property; a point-only
lookup there fails)."
  (or (org-air-view--row-property 'org-air-item)
      (user-error "No org-air item at point")))

(defmacro org-air-view--at-item-source (item &rest body)
  "At ITEM's heading in its source buffer run BODY, save, and remember it.
R26-8: soft-errors first when ITEM's file is mid-refresh stale (its cached
position may be wrong); hydrates a cache-cold (FILE . POS) marker slot on
demand via `org-air-view--item-pos'."
  (declare (indent 1) (debug t))
  (let ((buf (make-symbol "buf")) (it (make-symbol "it")))
    `(let* ((,it ,item))
       (org-air-view--refresh-stale-item-guard ,it)
       (let ((,buf (find-file-noselect (org-air-item-file ,it))))
         (with-current-buffer ,buf
           (save-excursion
             (goto-char (org-air-view--item-pos ,it))
             (org-back-to-heading t)
             ,@body)
           (save-buffer))
         (setq org-air-view--triage-source-buffer ,buf)
         ,buf))))

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
               (memq 'inbox (org-air-view--classify-cached
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
    ;; R18 D-P1b: under `side-window' the calendar lives in the separate rail
    ;; buffer, so month-nav redraws only that buffer — the board is untouched.
    (org-air-view--render-calendar)))

(defun org-air-calendar-next ()
  "Page to the next month, or the next day in the day view (R6)."
  (interactive)
  (if org-air-view--day
      (org-air-view-day (time-add org-air-view--day (days-to-time 1)))
    (setq org-air-view--cal-month
          (org-air-view--calendar-month-time org-air-view--cal-month 1))
    ;; R18 D-P1b: side-window redraws only the rail buffer (see -prev).
    (org-air-view--render-calendar)))

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
  "Progressively close org-air surfaces — ONE surface per press (R28-2).
Peel order, most-recent surface first: a live bottom pane closes FIRST
\(board alive, point untouched); the single-day view returns to the full
board (R6); only then does a press quit org-air itself — rail teardown +
`quit-window'."
  (interactive)
  (cond
   ;; R28-2 layer 1: a live pane is the most-recent surface — close it, STOP.
   ((org-air-view--quit-close-pane))
   ;; Layer 2: single-day view -> the full board (R6).
   (org-air-view--day (org-air-view-board))
   (t
    ;; R16 D-P1: tear down the popped-out rail (if any) before quitting.
    (when org-air-view--rail-popped-out
      (org-air-rail--teardown))
    ;; R16 D-P3: tear down a lingering (window-less) pane buffer as well.
    (org-air-view-pane--teardown)
    (quit-window))))

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
  "Show org-air key bindings (R26-3: project-aware)."
  (interactive)
  ;; R19-4d: name the two roles distinctly — Filter is the LIVE tag
  ;; narrowing (multi-tag, AND/OR), Scope is the structural LENS.
  (if (derived-mode-p 'org-air-project-mode)
      (message "org-air project: RET open (same window), S-RET visit other window, ( flip filename↔title, o/O sort cycle/reverse, s/d/t group state/dir/tag, / filter · \\ clear · M-/ AND↔OR, | rail, v peek pane, g refresh, q quit")
    (message "org-air: n/p items, TAB sections, RET visit, c capture, r refile, / filter (tags, live) · \\ clear · M-/ AND↔OR, s scope (lens: file/group/all) · S clear, g refresh, q quit")))

;;;###autoload
(defun org-air-visit-item (&optional item display)
  "Visit ITEM's original Org heading.
When ITEM is nil, use the item at point in an org-air dashboard.  DISPLAY
controls window choice and defaults to `org-air-visit-display'."
  (interactive)
  (let ((item (or item (get-text-property (point) 'org-air-item))))
    (unless item
      (user-error "No org-air item at point"))
    ;; R18 D-P4: RET owns the pane now (`org-air-view-pane-return'); the old
    ;; opt-in `org-air-view-pane-on-return' RET-also-opens-pane behaviour is
    ;; obsolete and no longer consulted here.
    (let* ((marker (org-air-item-marker item))
           ;; R26-8: a cache-hydrated item carries a (FILE . POS) cons —
           ;; hydrate on demand (visit the file in the background).
           (buffer (or (and (markerp marker) (marker-buffer marker))
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
      (goto-char (org-air-view--item-pos item))
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
