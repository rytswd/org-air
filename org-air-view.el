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
;; R73-2: the archive verb's ring-record desc names the archive file.
(declare-function org-archive--compute-location "org-archive" (location))

(defvar org-air-files)
(defvar org-air-inbox-file)
(defvar org-air-exclude-regexps)
;; R58: `bookmark-make-record-function' is bookmark.el's (not preloaded);
;; the modes set it buffer-locally without requiring bookmark at load.
(defvar bookmark-make-record-function)
(defvar org-air-revisit-buffer-name)
(defvar org-air-review-buffer-name)

;; R54-3: the Revisit view lives in org-air-revisit.el (module split);
;; this file only NAMES its entry points (the `N' key, the Notes-heading
;; RET doorway, the shared dispatchers), all fboundp-guarded at runtime.
(declare-function org-air-revisit "org-air-revisit" ())
(declare-function org-air-revisit--render-current "org-air-revisit" ())

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

(defcustom org-air-chrome-separator "∙"
  "Single source of truth for the org-air chrome middle-dot separator.
Used to join header status segments, the rail Source line, the
Filter/Match/Actions legends, the pane header, the quiet-activity marker
and the calendar `created' key (R33-1).  The default is `∙' (U+2219
BULLET OPERATOR, East-Asian *Neutral* -> painted one column in every
font), replacing the visually similar `·' (U+00B7 MIDDLE DOT, East-Asian
*Ambiguous* -> a GUI font may PAINT it two columns while `string-width'
measures one).  Because `string-width' is identical (both 1), every
column position and the whole V6/R31 width math are byte-identical in
COLUMNS; only the glyph byte changes, so a right-filled chrome line can
no longer be painted past its usable width (Seam B)."
  :type 'string
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

(defcustom org-air-rail-placement 'side-window
  "The ONE shared default context-rail placement for every view (R49-2/3).
A symbol: `side-window' (the default) opens each view with its rail
ALREADY popped out into the dedicated `*org-air-rail*' side window;
`inline' composes the rail as buffer text beside the content.  Per-view
overrides win when non-nil: `org-air-board-rail-placement',
`org-air-project-rail-placement', `org-air-outline-rail-placement',
`org-air-revisit-rail-placement' and `org-air-review-rail-placement'
\(nil = inherit this shared default) — every view resolves through the
ONE `org-air-rail--placement' resolver.
Consulted ONCE per buffer: when a view first renders with the popped flag
still `unset', it seeds t (side-window) or nil (inline).  Thereafter the
`|' toggle and the R25-6 reconciler own the flag.  `org-air-rail-style'
set to `side-window' still forces the BOARD entry (back-compat).  Batch
\(`noninteractive') renders never consult placement — they normalise the
sentinel to nil exactly as before, keeping every byte golden untouched.
LEGACY (R26-5): the old per-view alist shape
`\\='((board . inline) (project . side-window))' is still honoured — a
`consp' value resolves per view via `alist-get', so existing
`custom-set-variables' (and harness let-binds) keep their exact per-view
behaviour with zero migration.
Note on `inline': the inline rail is buffer text composed beside the
content, so scrolling moves it off-screen with the rows — inherent to
in-buffer composition (R49-3); `side-window' keeps the calendar and the
Actions legend always visible."
  :type '(choice (const :tag "Side window (popped out initially)" side-window)
                 (const :tag "Inline (single buffer)" inline))
  :group 'org-air)

(defcustom org-air-board-rail-placement nil
  "BOARD override for `org-air-rail-placement' (R49-2).
nil (the default) inherits the shared `org-air-rail-placement'; `inline'
or `side-window' pins the board regardless of the shared default."
  :type '(choice (const :tag "Inherit `org-air-rail-placement'" nil)
                 (const inline) (const side-window))
  :group 'org-air)

(defvar org-air-project-rail-placement)  ; defcustom in org-air-project.el
(defvar org-air-revisit-rail-placement)  ; defcustom in org-air-revisit.el
(defvar org-air-review-rail-placement)   ; defcustom in org-air-review.el

(defun org-air-rail--placement (view)
  "Resolve VIEW's initial rail placement (R49-2; R62-1d).
VIEW is `board' / `project' / `outline' / `revisit' / `review'.
Per-view override first (`org-air-board-rail-placement' /
`org-air-project-rail-placement' / `org-air-outline-rail-placement' /
`org-air-revisit-rail-placement' / `org-air-review-rail-placement'),
else the shared `org-air-rail-placement'.  The R26-5 alist shape of the
shared knob is still honoured (legacy): a `consp' value resolves per view
via `alist-get'.  Falls back to `side-window' (the R49-3 default) when
nothing names VIEW."
  (or (pcase view
        ('board org-air-board-rail-placement)
        ('project (and (boundp 'org-air-project-rail-placement)
                       org-air-project-rail-placement))
        ('outline org-air-outline-rail-placement)
        ('revisit (and (boundp 'org-air-revisit-rail-placement)
                       org-air-revisit-rail-placement))
        ('review (and (boundp 'org-air-review-rail-placement)
                      org-air-review-rail-placement)))
      (let ((base org-air-rail-placement))
        (if (consp base) (alist-get view base) base))
      'side-window))

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

(defcustom org-air-show-origin nil
  "When non-nil, show the board FILENAME (origin) column (R30-3).
Default nil: the filename is noise on the board, so the origin column is
HIDDEN and reclaims its width for the flex title.  Toggled at runtime by
`org-air-toggle-origin' (the `z f' display-column key); a display-only
knob — filter/scope still read the item origin from the struct."
  :type 'boolean
  :group 'org-air)

(defcustom org-air-show-dates t
  "When non-nil (the default), show the board DATE/SCHEDULE column (R30-3).
Toggled at runtime by `org-air-toggle-dates' (`z d').  Display-only: the
date sort still orders rows when the column is hidden."
  :type 'boolean
  :group 'org-air)

(defcustom org-air-show-tags t
  "When non-nil (the default), show the board TAGS column (R30-3).
Toggled at runtime by `org-air-toggle-tags' (`z t').  Display-only:
`org-air-filter' still narrows by a hidden tag column (it reads
`org-air-item-tags' from the struct, not the rendered cell)."
  :type 'boolean
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

(defcustom org-air-keyword-badge-min-cols 5
  "Minimum column width of a keyword svg pill (R80).
A short keyword (OUT/OFF, 3 cols) pads its capsule to at least this many
columns, centring the label, so it renders at the SAME size as a DRAFT
state chip (`org-air-project--state-cell-w', also 5) instead of a tiny
pill.  The pad is symmetric-looking (the label centres); the extra
spacing on both sides is intended.  A longer keyword (WAITING, 7 cols) is
already >= this floor, so it is byte-identical.  The widening is COLUMNS
only, never a `:height' (svg-never-grows-line)."
  :type 'integer :group 'org-air)

(defcustom org-air-todo-keyword-faces
  '(("TODO" . org-air-face-todo)
    ("NEXT" . org-air-face-todo-next)
    ("STARTED" . org-air-face-todo-next)
    ("READY" . org-air-face-todo-next)
    ("WIP" . org-air-face-todo-next)
    ("WAIT" . org-air-face-todo-wait)
    ("WAITING" . org-air-face-todo-wait)
    ("HOLD" . org-air-face-todo-wait)
    ("BLOCKED" . org-air-face-todo-wait)
    ("OUT" . org-air-face-air-state-out)
    ("OFF" . org-air-face-air-state-off)
    ("DONE" . org-air-face-done)
    ("COMP" . org-air-face-done)
    ("COMPLETED" . org-air-face-done)
    ("DROPPED" . org-air-face-dropped)
    ("DROP" . org-air-face-dropped)
    ("CANCELLED" . org-air-face-dropped)
    ("CANCELED" . org-air-face-dropped)
    ("KILL" . org-air-face-dropped)
    ("KILLED" . org-air-face-dropped))
  "Map TODO keyword strings to faces for coloured rendering (T1a).
R79: the DONE family splits — completions (DONE/COMP/COMPLETED) keep
`org-air-face-done' (faded blue) while the cancelled/abandoned set
\(DROPPED/DROP/CANCELLED/CANCELED/KILL/KILLED) reads `org-air-face-dropped'
\(muted terracotta), so a completion and an abandonment are distinct.
R80: OUT/OFF map to the NEW distinct standing-out faces
\(`org-air-face-air-state-out' / `org-air-face-air-state-off') — the SAME
faces the project STATE chip uses, so a heading keyword OUT/OFF and a
`#+state: out'/`off' doc chip wear ONE colour (R80 Decision 3).
Unknown keywords fall back through the R57 merged scan vocabulary — a
not-done keyword to `org-air-face-todo', a done keyword to
`org-air-face-done' (or `org-air-face-dropped' when cancelled-named), else
by the item's DONE flag (see `org-air-view--todo-face')."
  :type '(alist :key-type string :value-type face)
  :group 'org-air)

(defcustom org-air-keyword-face-source 'own
  "Where a TODO-keyword badge takes its colour (R79).
`own' (default): org-air's own `org-air-todo-keyword-faces' plus the R57
merged scan vocabulary — the calm V6 palette (R57-1), so the board
goldens stay byte-identical out of the box.
`org': the user's own `org-todo-keyword-faces' (the SAME table Org
fontifies headings with), falling back to the `own' mapping wherever the
user names no face.  Flip this one knob to read your exact keyword
palette on the board and in the day view."
  :type '(choice (const own) (const org))
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

(defun org-air-view--cache-key ()
  "Return the ONE coherence key board items and caches are valid under.
R57-1: the file set + inbox + the MERGED scan vocabulary
\(`org-air-query--scan-todo-keywords').  `todo'/`title'/`donep' are
persisted cache slots parsed UNDER a vocabulary, so a vocabulary change
\(the user edits their global `org-todo-keywords', or org-air's
supplement changes) must invalidate items exactly like a file-set
change — the key IS the detector, both in memory
\(`org-air-view--items-key') and in the persistent cache's `:key'.
R59: `org-air-skip-container-headings' joins as the fourth element —
the classify routing and the day-view skip evaluate the knob LIVE over
persisted signal slots, but the F7 file-ntype vote is baked into
file-meta `:ntype' at scan time, so a knob flip must take the same
documented cold re-derive as a vocabulary change.
R60: `org-air-exclude-regexps' joins as the FIFTH element — the exclude
set is read at DISCOVERY time, so a flip takes effect through the key
mismatch's cold re-derive, never by live re-filtering of
already-scanned items: a cache written under exclude set A never
hydrates under set B, and a pre-R60 4-element `:key' misses on length
inequality (no `org-air-view--cache-version' bump — no serialisation
shape changed).
R61: `org-air-log-cap' joins as the SIXTH element — the cap shapes the
scanned-and-persisted `clocks'/`logs' slots, so a cap change must
invalidate exactly like a vocabulary change (the key IS the detector;
a pre-R61 5-element key also misses, on length).  The review's
RENDER-time knobs (period kind/anchor, rollup basis,
`org-air-review-suspect-clock-hours') are deliberately NOT key
elements — they fold over cached data and take effect on repaint.
R77: `org-air-task-requires-todo' joins as the SEVENTH element — the
knob shapes scan-time `ntype' and the baked file-meta `:ntype' (the
task signal narrows to the keyword alone), so a flip must invalidate
exactly like a vocabulary change: key mismatch ⇒ the documented cold
re-derive (skeleton + paced rescan), never a half-reclassified board.
A pre-R77 6-element key misses on length inequality (no
`org-air-view--cache-version' bump — no serialisation shape changed).
Plain printable list data: serialises as-is, compares with `equal'."
  (list org-air-files org-air-inbox-file
        (org-air-query--scan-todo-keywords)
        org-air-skip-container-headings
        org-air-exclude-regexps
        org-air-log-cap
        org-air-task-requires-todo))
(defvar-local org-air-view--items-mtimes nil
  "Alist FILE -> mtime of the last COMPLETED full scan (R42-1).
The in-memory baseline `org-air-view--refresh-start' diffs against so a
refresh reparses only the files that actually changed (or, when nothing
changed, nothing at all) instead of re-scanning every file.  Set wherever
`org-air-view--items' is filled from a full query (`org-air-view'
sync/cached branches, `org-air-refresh', `org-air-view--refresh-finish')
and hydrated from the persisted `:mtimes' by `org-air-view--cache-load'.")
(defvar-local org-air-view--classify-cache nil
  "Per-board memo mapping an `org-air-item' to its cached bucket list.
An `eq' hash (R18 D-P1c).  Auto-invalidates because a re-query yields new
item objects; explicitly cleared on day-rollover and refresh.")
(defvar-local org-air-view--classify-cache-day nil
  "Key the classify cache was built for; a mismatch clears it.
R72/R83: the list (DAY EFFECTIVE-HORIZON BACKLOG-TAG) — `time-to-days' of
the render's now, `org-air-view--filter-effective-horizon' and
`org-air-backlog-tag' — so a day rollover, an active-window horizon
change OR a mid-session backlog-tag rename all rebuild the memo (a cheap
slot-fold, never a file re-derive).")
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
OBSOLETE (R53 P1c): slices are now TIME-budgeted
\(`org-air-refresh-slice-budget') so a slice of cheap warm files consumes
many files per tick while a pathological file alone caps a slice — a
fixed file count was either too slow at 5000 files or too janky on slow
ones.  Kept only so existing configuration does not error; unused."
  :type 'integer
  :group 'org-air)
(make-obsolete-variable 'org-air-refresh-files-per-slice
                        'org-air-refresh-slice-budget "0.5 (R53)")

(defconst org-air-refresh-slice-budget 0.018
  "Wall-clock seconds one refresh slice may consume (R53 P1c).
≈ one frame: the budgeted slice keeps consuming queued files until the
budget is exceeded (minimum 1 file), so input latency is bounded by one
slice while a 5000-file cold fill still streams in over ~10s of idle.
An internal constant, never a defcustom.")

(defcustom org-air-cold-paint-interval 1.0
  "Seconds between progressive repaints of a still-loading cold board (R53).
The cold (no-cache) load paints real rows from the scan accumulator at
most this often — first rows ≈1s in, streaming to the full board — while
warm incremental refreshes keep the R26-8 single-swap rule."
  :type 'number
  :group 'org-air)

(defcustom org-air-scan-abort-retries 3
  "Input-aborts of the SAME file before the scan skips it as `slow' (R53).
A pathological file that keeps getting interrupted by typing is skip-
logged (see `org-air-scan-report') instead of livelocking the refresh."
  :type 'integer
  :group 'org-air)

(defcustom org-air-show-notes-section t
  "When non-nil, show the bounded Notes section for headingless files (R53 P3).
Headingless note files (a `#+title' + prose, no `*' headings) surface as
ONE collapsed count row at the bottom of the board; TAB expands the
`org-air-notes-preview-limit' most recent.  Nil removes the section from
the board entirely (the notes stay refile targets either way)."
  :type 'boolean
  :group 'org-air)

(defcustom org-air-notes-preview-limit 50
  "Rows the expanded Notes section shows (R53 P3).
The most recent notes by scan-time activity; the remainder stays behind
the standard `…and N more' fold row so the section can never reintroduce
an unbounded render at 5000 files."
  :type 'integer
  :group 'org-air)

(defcustom org-air-show-backlog-section t
  "When non-nil, show the bottom Backlog header/count for deferred items.
R90 makes the normal section header-only until TAB explicitly expands all
currently visible backlog rows.  Nil removes both its ordinary section
and ordinary Summary row; an exact `is:backlog' lens overrides that
opt-out and reveals the rows because it is an explicit request.  Raw
`#backlog' does not auto-reveal.  Items remain reachable through other
surfaces either way.  A backlog-free board renders byte-identically."
  :type 'boolean
  :group 'org-air)

(defvar-local org-air-view--refresh-token 0
  "Monotonic refresh token (R26-8).
Every scheduled slice carries the token current at schedule time; a
callback whose token is stale self-cancels, so `g r' mid-refresh (which
bumps the token) makes every pending slice a no-op — timers can never
interleave two refreshes or touch a superseded scan.")
(defvar-local org-air-view--refresh-state nil
  "R26-8 refresh machine state: nil (fresh/idle), `refreshing', `failed'.
Drives the header count-slot marker (`stale · refreshing…' / `stale ·
refresh failed (g r retries)'); only ever non-nil when the machine is
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
  "Alist FILE -> mtime captured per file AT SCAN TIME (R26-8/R42-1).
Captured by `org-air-view--refresh-run-slice' as each slice reads its
files, then consumed by `org-air-view--refresh-finish' to build the new
mtime baseline from SCAN-TIME data (never a finish-time re-stat, which
would stamp a fresh mtime over items read from an older revision — the
B1 coherence hole).")
(defvar-local org-air-view--refresh-last-paint nil
  "Float time of the last progressive cold repaint, or nil (R53 P1c).
Bounds the cold path's streaming repaints to the effective
`org-air-view--refresh-paint-interval'.  R56 P1c: nil while a STREAM
fill has not painted yet, so the first item-bearing slice (the inbox
slice, by queue construction) paints IMMEDIATELY — first meaningful
content well under a second at a 1801-file corpus.")
(defvar-local org-air-view--refresh-abort-file nil
  "Cons (FILE . COUNT) tracking input-aborts of the queue head (R53 P1c).
After `org-air-scan-abort-retries' aborts of the SAME file it is
skip-logged `slow' and dropped from the queue — no livelock.")
(defvar-local org-air-view--refresh-progressive nil
  "Non-nil while the fill runs in progressive STREAM mode (R56 P1b).
Set by `org-air-view--refresh-start' when the board has NO previous full
content to show (a true cold open) — never for a painted or cache-seeded
board, which keeps the R26-8 single-swap rule.  Gates the REPEATED
progressive paints in `org-air-view--refresh-run-slice': R53 P1c specced
a throttled stream but implemented the gate on the self-clearing
`org-air-view--loading' flag, so the first paint destroyed the stream
\(measured: exactly 1 progressive paint per cold fill, then frozen at
~1% content until the finish swap).  Also lets
`org-air-view--refresh-stale-item-guard' unblock items whose file was
ALREADY scanned this refresh (their painted positions are scan-fresh)
while still-queued files stay guarded.")
(defvar-local org-air-view--refresh-paint-items 0
  "Accumulator item count at the last progressive stream paint (R56 P1b).
A slice tail of empty/skipped files accumulates nothing new, so the
stream repaints only when this count has actually grown — no vacuous
re-renders of an unchanged board.")
(defvar-local org-air-view--refresh-render-secs nil
  "Seconds the last progressive stream repaint cost, or nil (R56 P1b).
Floors the effective paint cadence at 3x this cost
\(`org-air-view--refresh-paint-interval') so paint overhead can never
exceed ~1/3 of the fill even as the accumulated board grows expensive to
re-render (R53 measured 0.28s at 15.9k items).")
(defvar-local org-air-view--refresh-gap nil
  "The adaptive chain's LAST armed gap in seconds, or nil (R56 P2a).
The backoff memory `org-air-view--refresh-next-gap' doubles from while
input keeps aborting slices; reset by arm/cancel.")
(defvar-local org-air-view--refresh-scan-started nil
  "Non-nil while the queue-head file's scan has actually BEGUN (R56 P2c).
Raised immediately before the `org-air-query-items-in-files' call,
cleared on the per-file commit; `org-air-view--refresh-note-abort'
counts an input-abort against the head ONLY when this is up, so aborts
landing BETWEEN files (key-repeat — measured one innocent file dropped
`slow' per 0.6s of held-down arrow key) can never skip-drop files whose
scan never ran.")
(defvar-local org-air-view--refresh-banner-tick-time nil
  "Float time of the last in-place banner progress tick, or nil (R56 P3b).
Bounds `org-air-view--refresh-banner-tick' to one line-1 rewrite per
`org-air-view--refresh-banner-tick-interval'; nil at refresh start so
the first tick lands immediately.")
(defvar-local org-air-view--refresh-timer nil
  "The adaptive chain's live one-shot wall-clock pacing timer, or nil (R56 P2a).
Exactly one at a time: `org-air-view--refresh-chain-arm' cancels any
pending one-shot before arming the next, and
`org-air-view--refresh-disarm' (finish / failure / cancel) is the single
teardown.  Supersedes R34-3's \"repeating\" idle pacer — a repeating idle
timer fires ONCE per continuous idle period ((elisp) Idle Timers; R56
pty-probed 1 fire in 3s of idleness), never \"every 0.05s while idle\".")
(defvar-local org-air-view--refresh-watchdog nil
  "One-shot wall-clock safety timer for the paced refresh, or nil (R42-2).
Armed with the idle pacer; if the machine is STILL `refreshing' under the
same token when it fires, it drains the remaining queue synchronously and
finishes, so the idle pacer stranding (Emacs never going idle, or the idle
clock repeatedly reset) can never leave the state stuck at `refreshing'.")
(defvar-local org-air-view--deferred-timer nil
  "One-shot idle timer for the cache-first deferred first paint, or nil (R45-2).
The cache-HIT FRESH branch paints a pill-free skeleton instantly, then
defers the cached full-board render to this token-guarded one-shot so the
SVG pill rasterization happens OFF the launch critical path (no 2-10s
blank-frozen open on a fresh Emacs with a cold image cache).  Never armed
under `noninteractive' (the byte gate stays synchronous with no timer).")
(defvar-local org-air-view--cache-stale-files nil
  "Files whose mtime diverged from the cache snapshot (R26-8).
While REFRESHING, triage verbs on an item from one of these soft-error
\(\"Still refreshing this file…\"); positions in unchanged files are valid
by construction (mtime match).")
(defvar-local org-air-view--bookmark-locator nil
  "Armed point locator of an in-flight bookmark restore, or nil (R58).
A plist (:item (FILE . POS) :title TITLE) stashed by
`org-air-view-bookmark-jump' and consumed at the tail of every
successful full paint (`org-air-view--bookmark-consume').  One-shot: it
never survives past refresh-idle, so later user renders are untouched.")
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

;; R90: marks are live board-buffer UI state, keyed by exact source
;; identity.  They deliberately do not enter the scan cache or bookmarks.
(defvar-local org-air-view--marked-keys nil
  "Insertion-ordered exact source keys currently marked in this board.")
(defvar-local org-air-view--marked-key-table nil
  "Equal hash table mirroring `org-air-view--marked-keys' for membership.")
(defvar-local org-air-view--marked-witnesses nil
  "Equal hash of marked source key to the identity witness it was made on.
A source key is `(FILE . POSITION)' and a position is not an identity: an
edit made outside org-air shifts every later heading's offset, so the same
key can name a DIFFERENT heading in the next item generation.  The witness is
the bounded `((TITLE . SORTED-EFFECTIVE-TAGS) ORDINAL . ARITY)' fingerprint of
the heading the user really marked (`org-air-view--item-mark-witness'): the
projection `org-air-view--source-heading-exact-p' compares, PLUS the one
bounded discriminator that tells two same-projection siblings apart.  Nothing
more: no buffer text beyond that projection, no markers, no items, no undo
objects, two fixnums.  Confined to the live key set by
`org-air-view--marked-table-rebuild', so mark storage stays proportional to
the number of marks.")
(defvar-local org-air-view--marked-generation nil
  "The cached item-list object last reconciled with the marked key set.")
(defvar-local org-air-view--pending-mutation-landing nil
  "One-shot R90 local landing plan consumed by the next board render.")
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
(defvar-local org-air-view--pane-divider-col nil
  "Display column of the two-pane board/rail divider, or nil (R43-2).
`org-air-view--render' sets this to the column carrying the
`org-air-face-pane-border' vrule (`item-width' + the divider's leading
space) when the orientation is `two-pane', nil otherwise.
`org-air-view--finalize-buffer-lines' reads it so a two-pane BODY row is
padded to full width (the divider stays INTERIOR, byte-identical to the
normalize/golden shape) instead of having its blank rail tail trimmed —
which would demote the divider to the row's TERMINAL glyph and break the
rule into segments on every blank-rail row (board ≫ rail).")
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

(defconst org-air-view--notes-descriptor
  '(notes "Notes" "No notes.")
  "The bounded Notes section descriptor (R53 P3).
Rendered ONLY when headingless note file-items exist (see
`org-air-view--section-descriptors'), so a notes-free board — every
existing golden — renders exactly the pre-R53 sections.")

(defconst org-air-view--backlog-descriptor
  '(backlog "Backlog" "Nothing deferred.")
  "The conditional Backlog section descriptor (R83/R90).
A normal board shows it when the display knob is on; an exact
`is:backlog' lens forces it even when the knob is nil.  The ▽ section
icon comes from `org-air-glyphs'.")

(defun org-air-view--explicit-backlog-lens-p ()
  "Return non-nil for an exact `is:backlog' token in the active filter.
Case-insensitive exact matching mirrors the parser.  Raw `#backlog' is
intentionally not an explicit Backlog lens."
  (seq-some (lambda (token)
              (and (stringp token)
                   (string-equal-ignore-case token "is:backlog")))
            (cond ((listp org-air-view--tag-filter)
                   org-air-view--tag-filter)
                  ((stringp org-air-view--tag-filter)
                   (list org-air-view--tag-filter)))))

(defun org-air-view--backlog-section-enabled-p ()
  "Return non-nil when the ordinary knob or exact Backlog lens enables it."
  (or org-air-show-backlog-section
      (org-air-view--explicit-backlog-lens-p)))

(defun org-air-view--section-expanded-p (bucket)
  "Return non-nil when BUCKET is explicitly expanded in this live board.
This is the one R90 expansion read seam shared by row selection, fold-row
emission, TAB and width measurement."
  (and (memq bucket org-air-view--expanded-sections) t))

(defun org-air-view--ensure-explicit-backlog-lens ()
  "Persistently reveal Backlog when an exact `is:backlog' lens is applied."
  (when (org-air-view--explicit-backlog-lens-p)
    (cl-pushnew 'backlog org-air-view--expanded-sections)))

(defun org-air-view--section-descriptors (items)
  "Return the section descriptors to render for ITEMS (R53 P3, R83).
The fixed task sections, then the single collapsed Notes section when any
visible item is a `kind' `file' note and `org-air-show-notes-section' is
on, and finally the Backlog section when any visible item defers into the
`backlog' bucket and `org-air-show-backlog-section' is on.  Both trailing
sections are conditional-and-empty-suppressed, so a board with neither
renders byte-identically to the fixed five."
  (let ((descriptors org-air-view--sections))
    (when (and org-air-show-notes-section
               (org-air-view--items-for-bucket 'notes items))
      (setq descriptors
            (append descriptors (list org-air-view--notes-descriptor))))
    (when (and (org-air-view--backlog-section-enabled-p)
               (org-air-view--items-for-bucket 'backlog items))
      (setq descriptors
            (append descriptors (list org-air-view--backlog-descriptor))))
    descriptors))

;;;; =====================================================================
;;;; R35-1 — one switch to opt out of EVERY default keybinding.
;;;; `org-air-use-default-keybindings' (default t).  When nil org-air
;;;; installs NONE of its own keys — the board / project / rail /
;;;; doc-session maps, the `z' column-toggle prefix, the `g' prefix, the
;;;; `C-c C-a' leader (and its project / doc-session subsets) and the evil
;;;; overriding-map setup are all skipped.  The keymaps still EXIST and
;;;; keep their `special-mode' parent, so navigation/quit still work and
;;;; users can `define-key' their own commands into them.
;;;;
;;;; ONE installer + ONE clearer, both driven by ONE data table
;;;; (`org-air--default-key-specs' + `org-air--default-leader-specs'), so
;;;; install and clear can never drift.  The keymap OBJECTS are mutated in
;;;; place; the map variables are NEVER rebound, so every captured
;;;; reference (derived-mode map value, minor-mode :keymap, the leader
;;;; installs, the `org-air-mode-map' alias) stays valid.  `sync' is called
;;;; at LOAD (seed — byte-identical under the default t), at every org-air
;;;; MODE INIT (honours use-package `:custom' / a runtime `setq' on the
;;;; next org-air buffer) and from the defcustom `:set' (instant runtime
;;;; toggle) — the mechanism that "just works" for `:custom',
;;;; `setq'-before-`require' AND `customize'.
;;;; =====================================================================

(defvar org-air--default-key-specs nil
  "Registry of org-air default key bindings (R35-1).
Each entry is (MAP-SYMBOL KEY BINDING): MAP-SYMBOL names a shared keymap
variable; KEY is a `kbd' STRING or a raw key VECTOR (e.g. `[remap
quit-window]'); BINDING is a command symbol or a `(:prefix . PREFIX-MAP-
SYMBOL)' marker for a nested prefix map.  Populated by
`org-air--register-default-keys' right after each keymap `defvar' (in this
file and in `org-air-project.el'); consumed by
`org-air--install-default-keybindings' / `-clear-default-keybindings'.")

(defvar org-air--default-leader-specs nil
  "Registry of (HOST-MAP-SYMBOL . PREFIX-MAP-SYMBOL) leader installs (R35-1).
Each registered pair is installed at `org-air-leader-key' via
`org-air-view--leader-install' when the defaults are on, and unbound when
they are off.")

(defvar org-air--default-keybindings-state 'unset
  "Whether the org-air defaults are currently installed (R35-1).
One of `unset' (never synced), t (installed) or nil (cleared).  Guards the
`org-air-leader-key' :set reinstall (D-4): a leader re-bind is a no-op
while the defaults are not installed.")

(defcustom org-air-use-default-keybindings t
  "When non-nil (the default), install org-air's default keybindings (R35-1).
Set to nil to make org-air install NO keys of its own — in ANY org-air
buffer OR in the files you visit from org-air.  Skipped when nil: the
board / project / rail / doc-session maps, the `z' column-toggle prefix,
the `g' prefix, the `org-air-leader-key' leader (and its project /
doc-session subsets), `org-air-outline-mode's rail keys, the calendar
day-cell keys, the read-only snapshot pane's `q', the evil overriding-map
setup, AND — crucially — the keys org-air would otherwise add to YOUR OWN
files: the `org-air-return-key' in a visited source buffer and the
dedicated close key + `quit-window' remap in an editable doc-session /
indirect pane.  The org-air keymaps still EXIST and keep their `special-mode'
parent, so navigation/quit still work and you can `define-key' your own
commands into them (see `org-air--install-default-keybindings' to re-add
the full set, or bind individual command symbols).

Honoured whether set via `use-package' `:custom', a plain `setq' before
loading org-air, or a runtime `customize' — the maps re-sync on the next
org-air buffer and immediately on a `customize' set.  Does not affect the
global entry-point keys you bind yourself (e.g. `C-c a' -> `org-air');
those are your bindings, not defaults."
  :type 'boolean
  :group 'org-air
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'org-air--sync-default-keybindings)
           (org-air--sync-default-keybindings))))

(defun org-air--register-default-keys (map-symbol &rest bindings)
  "Register default BINDINGS for MAP-SYMBOL into `org-air--default-key-specs'.
BINDINGS is a flat list of KEY BINDING KEY BINDING…: KEY a `kbd' STRING or
a raw key VECTOR; BINDING a command symbol or a `:prefix' marker cons
`(:prefix . PREFIX-SYM)' (R35-1).  Data only, nothing is bound until
`sync' runs."
  (while bindings
    (let ((key (pop bindings))
          (binding (pop bindings)))
      (push (list map-symbol key binding) org-air--default-key-specs)))
  org-air--default-key-specs)

(defun org-air--register-default-leader (host-symbol prefix-symbol)
  "Register a (HOST-SYMBOL . PREFIX-SYMBOL) default leader install (R35-1)."
  (cl-pushnew (cons host-symbol prefix-symbol) org-air--default-leader-specs
              :test #'equal)
  org-air--default-leader-specs)

(defun org-air--default-key-descriptor (key)
  "Return the internal key descriptor for KEY (a `kbd' STRING or a VECTOR)."
  (if (stringp key) (kbd key) key))

(defun org-air--install-default-keybindings ()
  "Install EVERY org-air default binding into the shared maps (R35-1).
Idempotent: populates the board / view-core / project / doc-session / rail
/ calendar-day / snapshot-pane maps and the prefix maps from
`org-air--default-key-specs', and installs the `org-air-leader-key' leader
subsets from `org-air--default-leader-specs'.  The per-buffer visited-file
keys (the `org-air-return-key' and the indirect-pane close map) are gated
separately at their install call sites (R35.1), since they bind into the
user's OWN buffers at visit time, not into these shared maps.  Never
touches keymap PARENTS (set once at each `defvar').  Returns t."
  (dolist (spec (reverse org-air--default-key-specs))
    (pcase-let ((`(,map-symbol ,key ,binding) spec))
      (when (boundp map-symbol)
        (define-key (symbol-value map-symbol)
                    (org-air--default-key-descriptor key)
                    (if (and (consp binding) (eq (car binding) :prefix))
                        (symbol-value (cdr binding))
                      binding)))))
  (dolist (pair (reverse org-air--default-leader-specs))
    (when (and (boundp (car pair)) (boundp (cdr pair)))
      (org-air-view--leader-install (symbol-value (car pair))
                                    (symbol-value (cdr pair)))))
  t)

(defun org-air--clear-default-keybindings ()
  "Remove every org-air default binding, KEEPING the maps + parents (R35-1).
Each key this package installs is REMOVED (the `define-key' REMOVE arg, so
it truly falls through to the `special-mode' parent — `q' -> `quit-window',
`g' -> `revert-buffer', SPC scroll — rather than being nil-SHADOWED); a key
with no parent binding becomes unbound (self-insert).  The prefix host
keys are removed and the leader prefix unbound, leaving the submaps
dormant.  Evil registration is not undone at runtime — with the knob nil it
was never added.  Returns t."
  (dolist (spec org-air--default-key-specs)
    (pcase-let ((`(,map-symbol ,key ,_binding) spec))
      (when (boundp map-symbol)
        (define-key (symbol-value map-symbol)
                    (org-air--default-key-descriptor key)
                    nil t))))
  (dolist (pair org-air--default-leader-specs)
    (when (boundp (car pair))
      (let ((host (symbol-value (car pair))))
        (when org-air-view--leader-installed-key
          (define-key host (kbd org-air-view--leader-installed-key) nil t))
        (define-key host (kbd org-air-leader-key) nil t))))
  (setq org-air-view--leader-installs nil
        org-air-view--leader-installed-key nil)
  t)

(defun org-air--sync-default-keybindings ()
  "Install-or-clear the defaults to match `org-air-use-default-keybindings'.
Called at LOAD (seed), at every org-air mode init (so a `use-package'
`:custom' / a runtime `setq' are honoured on the next org-air buffer), and
from the defcustom `:set' (so a live `customize' set takes effect
immediately).  Acts only when the desired state differs from
`org-air--default-keybindings-state' (D-2), so it does not redundantly
re-run every mode init; idempotent either way (R35-1)."
  (let ((want (and org-air-use-default-keybindings t)))
    (unless (eq want org-air--default-keybindings-state)
      (if want
          (org-air--install-default-keybindings)
        (org-air--clear-default-keybindings))
      (setq org-air--default-keybindings-state want))))

;; R35.1: the calendar day-cell keys (installer-owned).  `org-air-calendar'
;; is required at the top of this file, so `org-air-calendar-day-keymap'
;; already exists; registering here folds it into the same knob so the
;; day cells lose RET / mouse-1 -> `org-air-view-day' when the defaults
;; are off (the cells then fall through to the major-mode map).
(org-air--register-default-keys 'org-air-calendar-day-keymap
  "RET" #'org-air-view-day
  [mouse-1] #'org-air-view-day)

(defvar org-air-g-prefix-map
  (make-sparse-keymap)
  "Transient g-prefix map (B4): r refresh, g top of pane, R refresh+clear.
R22-3: g RET visit, g o visit-stay (o/O are the shared sort now).
Keys installed by `org-air--install-default-keybindings' (R35-1).")

;; R35-1: the g-prefix default keys (installer-owned).  R22-3: o/O now
;; drive the shared SORT (view-core), so the board's visit verbs relocate
;; under the g-prefix: `g RET' visits in the other window, `g o' visits but
;; stays.  GUI visit stays on S-RET.
(org-air--register-default-keys 'org-air-g-prefix-map
  "r" #'org-air-refresh
  "g" #'org-air-goto-top
  "R" #'org-air-refresh-all
  "RET" #'org-air-visit-item
  "o" #'org-air-visit-item-stay
  "d" #'org-air-goto-date)   ; R78: "go to date" — the prompted day jump

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
`where-is').
R35-1 (D-4): a no-op while the org-air defaults are NOT installed — there
is no leader prefix to move; when the user later turns the defaults on the
installer binds the leader at the then-current `org-air-leader-key'."
  (unless (eq org-air--default-keybindings-state nil)
  (dolist (pair org-air-view--leader-installs)
    (when (and org-air-view--leader-installed-key
               (not (equal org-air-view--leader-installed-key
                           org-air-leader-key)))
      (define-key (car pair) (kbd org-air-view--leader-installed-key) nil))
    (define-key (car pair) (kbd org-air-leader-key) (cdr pair)))
  (setq org-air-view--leader-installed-key org-air-leader-key)))

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

(defun org-air--repeat-pn-commands ()
  "Return (NEXT . PREV) the live prev/next motion commands for this buffer.
Resolves the context-correct motion so ONE shared repeat map (R39-4) steps
the board items, the project rows, or the doc/outline headings, whichever
the current buffer is.  The doc/outline motion is the fallback."
  (cond
   ((derived-mode-p 'org-air-view-mode)
    (cons #'org-air-next-item #'org-air-prev-item))
   ((derived-mode-p 'org-air-project-mode)
    (cons #'org-air-project-next #'org-air-project-prev))
   (t (cons #'org-air-outline-next-heading #'org-air-outline-prev-heading))))

(defvar org-air--repeat-pn-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'org-air--repeat-next)
    (define-key map "p" #'org-air--repeat-prev)
    map)
  "Transient repeat map for the leader prev/next motions (R39-4).
Armed via `set-transient-map' after a leader (or bare) prev/next so a bare
`n'/`p' repeats the motion until ANY other key; its `n'/`p' dispatch to the
context-correct motion via `org-air--repeat-pn-commands'.")

(defun org-air--repeat-pn-arm ()
  "Install the shared p/n repeat transient map (R39-4), gated on the knob.
`set-transient-map' with KEEP-PRED = t keeps the map live as long as the
last key was a bound `n'/`p'; the first other key deactivates it AND runs
normally.  A no-op when `org-air-use-default-keybindings' is nil.  Only the
LEADER-path wrappers (`org-air--repeat-next'/`-prev') arm the map; the bare
motion primitives stay pure so nothing leaks the transient map into an
unrelated buffer."
  (when org-air-use-default-keybindings
    (set-transient-map org-air--repeat-pn-map t)))

(defun org-air--repeat-next ()
  "Leader NEXT motion made repeatable: run the context motion, arm p/n (R39-4).
Bound at the leader `n' (and in `org-air--repeat-pn-map'); it calls the SAME
context-correct motion primitive (no fork) then arms the transient map so a
bare `n'/`p' repeats until any other key."
  (interactive)
  (call-interactively (car (org-air--repeat-pn-commands)))
  (org-air--repeat-pn-arm))

(defun org-air--repeat-prev ()
  "Leader PREV motion made repeatable: run the context motion, arm p/n (R39-4).
Bound at the leader `p' (and in `org-air--repeat-pn-map'); it calls the SAME
context-correct motion primitive (no fork) then arms the transient map so a
bare `n'/`p' repeats until any other key."
  (interactive)
  (call-interactively (cdr (org-air--repeat-pn-commands)))
  (org-air--repeat-pn-arm))

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
  (make-sparse-keymap)
  "Leader prefix map for the BOARD content buffer (R30-2).
Installed at `org-air-leader-key' on `org-air-view-mode-map'.
Keys installed by `org-air--install-default-keybindings' (R35-1).")

;; R35-1: the board leader default keys (installer-owned).  Board/project
;; content buffers: rail toggle, outline jump, the shared sort, the
;; per-view filter — all EXISTING commands, reached from the main window
;; under the `C-c' leader (never a fork).
(org-air--register-default-keys 'org-air-leader-map
  "|" #'org-air-rail-toggle
  "o" #'org-air-rail-return
  "s" #'org-air-view-sort-cycle
  "/" #'org-air-filter)

;;;; =====================================================================
;;;; R30-3 — dashboard column toggles (defcustom-backed display group).
;;;; The origin/date/tag cluster columns hide/show through the EXISTING
;;;; compute-once / V6 relock; a hidden column reclaims its width for the
;;;; flex title.  A `z' display-column prefix keeps the flat board
;;;; namespace clean; filter/scope still read the hidden data.
;;;; =====================================================================

(defun org-air-view--toggle-column (var label)
  "Flip board column toggle VAR buffer-locally, re-render, echo (R30-3).
VAR is one of `org-air-show-origin' / `-dates' / `-tags'; LABEL names the
column for the echo.  Re-renders through the shared
`org-air-view--refresh-current' (compute-once partition + V6 relock
reused) so the hidden column's width reflows to the title and everything
stays aligned."
  (set (make-local-variable var) (not (symbol-value var)))
  (org-air-view--refresh-current)
  (message "org-air: %s column %s" label
           (if (symbol-value var) "shown" "hidden")))

(defun org-air-toggle-origin ()
  "Toggle the board FILENAME (origin) column (R30-3).  Key `z f'."
  (interactive)
  (org-air-view--toggle-column 'org-air-show-origin "origin"))

(defun org-air-toggle-dates ()
  "Toggle the board DATE/SCHEDULE column (R30-3).  Key `z d'."
  (interactive)
  (org-air-view--toggle-column 'org-air-show-dates "dates"))

(defun org-air-toggle-tags ()
  "Toggle the board TAGS column (R30-3).  Key `z t'."
  (interactive)
  (org-air-view--toggle-column 'org-air-show-tags "tags"))

(defvar org-air-columns-prefix-map
  (make-sparse-keymap)
  "Display-column toggle prefix map (R30-3), bound to `z' on the board.
`z f' origin (Filename), `z d' dates, `z t' tags.
Keys installed by `org-air--install-default-keybindings' (R35-1).")

;; R35-1: the `z' column-toggle default keys (installer-owned).
(org-air--register-default-keys 'org-air-columns-prefix-map
  "f" #'org-air-toggle-origin
  "d" #'org-air-toggle-dates
  "t" #'org-air-toggle-tags)

(defvar org-air-view-core-map
  (let ((map (make-sparse-keymap)))
    ;; Keep the `special-mode' defaults reachable below the shared core.
    ;; PARENT stays at defvar time — always, even with the knob nil (R35-1).
    (set-keymap-parent map special-mode-map)
    map)
  "Shared view-core keymap, parent of the board + project mode maps (R18 D-P3).
Reuse the core, override the bespoke: the keys here are identical across
both views; per-mode domain verbs stay in each child map.
Keys installed by `org-air--install-default-keybindings' (R35-1).")

;; R35-1: the shared VIEW-CORE default keys (installer-owned).  R18 D-P3:
;; the unambiguous keys live here ONCE, so they can never drift between the
;; board and project maps (both inherit via `set-keymap-parent').  RET owns
;; the bottom view pane; v/V open+close it; \ clears the filter; M-/
;; toggles AND/OR.  R22-3: o/O are the shared within-view SORT.  R22-5: `|'
;; pops the rail.  R29-2: vim-ish j/k line motion is shared here.
(org-air--register-default-keys 'org-air-view-core-map
  "RET" #'org-air-view-pane-return
  "<mouse-1>" #'org-air-view-pane-return
  "v" #'org-air-view-pane
  "V" #'org-air-view-pane-close
  "\\" #'org-air-filter-clear
  "M-/" #'org-air-filter-toggle-match
  "o" #'org-air-view-sort-cycle
  "O" #'org-air-view-sort-reverse
  "|" #'org-air-rail-toggle
  "j" #'org-air-next-line
  "k" #'org-air-prev-line)

(defvar org-air-view-mode-map
  (let ((map (make-sparse-keymap)))
    ;; R18 D-P3: inherit the shared view-core keys (RET pane, v/V, \, M-/).
    ;; PARENT stays at defvar time — always, even with the knob nil (R35-1).
    (set-keymap-parent map org-air-view-core-map)
    map)
  "Keymap for `org-air-view-mode'.
Keys installed by `org-air--install-default-keybindings' (R35-1).")

;; R35-1: the BOARD default keys (installer-owned).  R18 D-P4: S-RET visits
;; the file in the other window (and `O' via the shared core sort); RET
;; opens the pane (inherited).  T2: TAB toggles a section; motion on
;; M-n/M-p/M-TAB.  Triage verbs c/m/s/d/e/f/t/T/a/D/x/u/I.  `/' per-mode
;; filter.  `g' -> g-prefix, `z' -> columns prefix.
(org-air--register-default-keys 'org-air-view-mode-map
  "<S-return>" #'org-air-visit-item
  "S-RET" #'org-air-visit-item
  "n" #'org-air-next-item
  "p" #'org-air-prev-item
  "TAB" #'org-air-toggle-section
  "<backtab>" #'org-air-prev-section
  "M-TAB" #'org-air-next-section
  "M-n" #'org-air-forward-section
  "M-p" #'org-air-back-section
  "SPC" #'org-air-peek-item
  "c" #'org-air-capture
  "m" #'org-air-toggle-mark
  "M" #'org-air-clear-marks
  "s" #'org-air-scope
  "d" #'org-air-item-deadline
  "e" #'org-air-refile-item
  "f" #'org-air-item-file-group
  "t" #'org-air-set-tag
  "T" #'org-air-item-cycle-todo
  ;; R83: `b' toggles the backlog tag — defer/un-defer the item off the
  ;; attention surfaces (audited FREE on this map + the review map).
  "b" #'org-air-item-backlog
  "a" #'org-air-item-archive
  "D" #'org-air-item-done
  "x" #'org-air-item-kill
  "u" #'org-air-edit-undo
  "U" #'org-air-edit-redo
  "I" #'org-air-process-inbox
  "/" #'org-air-filter
  "S" #'org-air-scope-clear
  "g" '(:prefix . org-air-g-prefix-map)
  "G" #'org-air-goto-bottom
  "P" #'org-air-project
  "<" #'org-air-calendar-prev
  ">" #'org-air-calendar-next
  "." #'org-air-calendar-today
  "z" '(:prefix . org-air-columns-prefix-map)
  ;; R54-3: the symmetric view-switch pair — `P' project, `N' revisit.
  "N" #'org-air-revisit
  ;; R61-4: the third leg — `W' opens the review (week/period) surface.
  "W" #'org-air-review
  "?" #'org-air-help
  "q" #'org-air-quit)

;; R30-2/R35-1: install the main-window leader on the board map so the rail
;; actions (rail toggle, outline jump, sort, filter) are reachable from the
;; board content buffer under `C-c C-a', not only the side window.
(org-air--register-default-leader 'org-air-view-mode-map 'org-air-leader-map)

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
  ;; R35-1: reconcile the shared maps to `org-air-use-default-keybindings'
  ;; on the FIRST org-air buffer — honours use-package `:custom' / a runtime
  ;; `setq' (always run after load) before the map is consulted here.
  (org-air--sync-default-keybindings)
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
  ;; R58: the board is bookmarkable (activities.el / burly / `C-x r m') —
  ;; a FULL record: view kind, scope, filter, sort, day, plus the durable
  ;; (FILE . POS) row locator.  Restored by `org-air-view-bookmark-jump'.
  (setq-local bookmark-make-record-function
              #'org-air-view--bookmark-make-record)
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
  ;; R90 pass-2: only the canonical `*org-air*' board owns global source
  ;; tracking.  Incidental mode buffers neither install nor tear down its hook.
  (org-air-view--source-tracking-claim)
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
  ;; R35-1: the evil overriding-map setup is gated on the knob — with the
  ;; defaults off there is nothing to override, so no motion state is
  ;; forced and `org-air-view--evil-modes' stays empty.
  (when org-air-use-default-keybindings
    (org-air-view--setup-evil 'org-air-view-mode org-air-view-mode-map))
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

(defun org-air-view--sep ()
  "Return the chrome separator wrapped as \" SEP \" (R33-1).
A single source of truth for the chrome middle-dot; see
`org-air-chrome-separator'."
  (format " %s " org-air-chrome-separator))

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

(defun org-air-view--item-source-key (item)
  "Return ITEM's exact R90 source key `(ABSOLUTE-FILE . POSITION)'.
The position is read from the durable marker slot: the integer cdr of a
cache-hydrated `(FILE . POS)' cell, or `marker-position' for a live
marker.  `expand-file-name' is the only normalisation paid; titles never
participate and no file is read.  Return nil when the source is malformed."
  (when (org-air-item-p item)
    (let* ((marker (org-air-item-marker item))
           (file (or (and (consp marker) (stringp (car marker)) (car marker))
                     (org-air-item-file item)))
           (pos (cond ((markerp marker) (marker-position marker))
                      ((and (consp marker) (integerp (cdr marker)))
                       (cdr marker)))))
      (when (and (stringp file) (integerp pos))
        (cons (expand-file-name file) pos)))))

(defvar org-air-view--source-tracking-owner nil
  "The one canonical live board buffer that owns source tracking.")

(defvar-local org-air-view--source-tracked-buffers nil
  "Already-live source buffers carrying this board owner's locators.")

(defvar-local org-air-view--source-generation 'unset
  "Item-list identity represented by the board's source indexes.")

(defvar-local org-air-view--source-item-membership nil
  "Eq membership index for the current board item generation.")

(defvar-local org-air-view--source-items-by-file nil
  "Expanded source file to heading items index for the current generation.")

(defvar-local org-air-view--source-tracked-locators nil
  "Ephemeral `(ITEM . MARKER)' locators for one user-visited source buffer.")

(defvar-local org-air-view--source-locator-index nil
  "Eq item to locator index for one user-visited source buffer.")

(defvar-local org-air-view--source-locator-generation 'unset
  "Board item-list identity represented by this source's locator index.")

(defvar-local org-air-view--source-locator-complete nil
  "Non-nil when every matching item in the locator generation was hydrated.")

(defvar-local org-air-view--source-locator-owner nil
  "Canonical board buffer owning this source's tracked locators.")

(defvar-local org-air-view--source-locator-witness nil
  "O(1) liveness witness `(FIRST . LAST)' for this source's locator set.
Cloned indirect buffers copy locator buffer-locals and hooks, so a
`complete' same-generation index must never be trusted on faith: the fast
path validates this bounded witness instead of sweeping every marker.")

(defvar org-air-view--source-projection-index nil
  "Dynamic eq item to `(POSITION . PROJECTION)' hydration index.")

(defun org-air-view--source-owner-valid-p (owner)
  "Return non-nil when OWNER is the live canonical org-air board."
  (and (buffer-live-p owner)
       (eq owner (get-buffer org-air-view-buffer-name))
       (with-current-buffer owner
         (derived-mode-p 'org-air-view-mode))))

(defun org-air-view--source-generation-index-ensure (items)
  "Build current-generation source indexes for ITEMS once.
Return non-nil only when the generation identity changed."
  (unless (and (eq org-air-view--source-generation items)
               (hash-table-p org-air-view--source-item-membership)
               (hash-table-p org-air-view--source-items-by-file))
    (let ((membership (make-hash-table :test #'eq :size (length items)))
          (by-file (make-hash-table :test #'equal)))
      (dolist (item items)
        (puthash item t membership)
        (when (and (eq (org-air-item-kind item) 'heading)
                   (stringp (org-air-item-file item)))
          (let ((file (expand-file-name (org-air-item-file item))))
            (puthash file (cons item (gethash file by-file)) by-file))))
      (maphash (lambda (file matching)
                 (puthash file (nreverse matching) by-file))
               by-file)
      (setq org-air-view--source-generation items
            org-air-view--source-item-membership membership
            org-air-view--source-items-by-file by-file))
    t))

(defun org-air-view--source-canonical-buffer (buffer)
  "Return the canonical visited base buffer tracking BUFFER's source text.
An indirect buffer shares its base's text and markers but is a distinct
buffer with its own locals; source locators are owned by the base ALONE."
  (and (buffer-live-p buffer)
       (or (buffer-base-buffer buffer) buffer)))

(defun org-air-view--dehydrate-source-markers ()
  "Release this source buffer's ephemeral tracked locators and ownership.
Teardown is OWNERSHIP-EXACT.  A cloned indirect buffer copies these
buffer-locals and this local `kill-buffer-hook', and its copied locator
entries hold the very marker objects still indexed by the canonical base, so
a clone's kill or mode change may only drop its own clone-local aliases:
releasing those markers, editing the owner's roster, or resetting the base's
generation/complete facts would destroy the base's live locators."
  (let* ((source (current-buffer))
         (clone (and (buffer-base-buffer source) t)))
    (unless clone
      (dolist (entry org-air-view--source-tracked-locators)
        (set-marker (cdr entry) nil)))
    (setq org-air-view--source-tracked-locators nil
          org-air-view--source-locator-index nil
          org-air-view--source-locator-generation 'unset
          org-air-view--source-locator-complete nil
          org-air-view--source-locator-witness nil)
    (let ((owner org-air-view--source-locator-owner))
      (setq org-air-view--source-locator-owner nil)
      (unless clone
        (when (buffer-live-p owner)
          (with-current-buffer owner
            (setq org-air-view--source-tracked-buffers
                  (delq source org-air-view--source-tracked-buffers))))))))

(defun org-air-view--source-locators-live-p (source)
  "Return non-nil when SOURCE's tracked locator set is still usable.
This is the O(1) fast-path guard: a bounded first/last witness must still be
live and owned by SOURCE itself.  An empty locator set is trivially usable."
  (let ((witness (buffer-local-value 'org-air-view--source-locator-witness
                                     source)))
    (if (null witness)
        (null (buffer-local-value 'org-air-view--source-tracked-locators
                                  source))
      (let ((first (car witness))
            (last (cdr witness)))
        (and (markerp first) (markerp last)
             (marker-position first) (marker-position last)
             (eq (marker-buffer first) source)
             (eq (marker-buffer last) source))))))

(defun org-air-view--source-prune-buffer
    (source items owner &optional membership)
  "In SOURCE retain one live locator per current ITEMS member for OWNER.
MEMBERSHIP is the generation's prebuilt eq index."
  (when (buffer-live-p source)
    (with-current-buffer source
      (when (eq org-air-view--source-locator-owner owner)
        (let ((members (or membership
                           (let ((table (make-hash-table :test #'eq)))
                             (dolist (item items) (puthash item t table))
                             table)))
              (seen (make-hash-table :test #'eq)) keep)
          (dolist (entry org-air-view--source-tracked-locators)
            (let ((item (car entry)) (marker (cdr entry)))
              (if (and (gethash item members)
                       (markerp marker) (marker-position marker)
                       (not (gethash item seen)))
                  (progn (puthash item marker seen) (push entry keep))
                (when (markerp marker) (set-marker marker nil)))))
          (setq keep (nreverse keep)
                org-air-view--source-tracked-locators keep
                org-air-view--source-locator-index seen
                org-air-view--source-locator-generation items
                org-air-view--source-locator-complete nil
                org-air-view--source-locator-witness
                (when keep
                  (cons (cdr (car keep)) (cdr (car (last keep)))))))))))

(defun org-air-view--source-prune-generation (items)
  "Release stale/deduplicated owner locators before hydrating ITEMS."
  (when (eq (current-buffer) org-air-view--source-tracking-owner)
    (org-air-view--source-generation-index-ensure items)
    (let ((owner (current-buffer)) live)
      (dolist (source org-air-view--source-tracked-buffers)
        (when (buffer-live-p source)
          (push source live)
          (org-air-view--source-prune-buffer
           source items owner org-air-view--source-item-membership)))
      (setq org-air-view--source-tracked-buffers (nreverse live)))))

(defun org-air-view--hydrate-source-items (source)
  "Track cached numeric locators belonging to live SOURCE ephemerally.
Only the exact cached position is accepted; this never searches by title and
never changes the durable cons stored in an item or rendered row property.
SOURCE is canonicalized to its visited BASE buffer first: an indirect clone
shares text and markers but is a separate buffer, and only the canonical base
may be hydrated, owned, indexed or tracked.  A source already indexed for the
same generation is an O(1) no-op, validated (also in O(1)) against a bounded
liveness witness so a stale `complete' flag can never bless dead or
wrong-buffer markers."
  (setq source (org-air-view--source-canonical-buffer source))
  (when (and (eq (current-buffer) org-air-view--source-tracking-owner)
             (buffer-live-p source)
             (buffer-local-value 'buffer-file-name source))
    (org-air-view--source-generation-index-ensure org-air-view--items)
    (let* ((owner (current-buffer))
           (items org-air-view--items)
           (file (expand-file-name
                  (buffer-local-value 'buffer-file-name source)))
           (matching (gethash file org-air-view--source-items-by-file)))
      (if (null matching)
          (when (eq (buffer-local-value 'org-air-view--source-locator-owner
                                        source)
                    owner)
            (with-current-buffer source
              (org-air-view--dehydrate-source-markers)))
        (with-current-buffer source
          (unless (eq org-air-view--source-locator-owner owner)
            (org-air-view--dehydrate-source-markers))
          (setq-local org-air-view--source-locator-owner owner)
          (add-hook 'kill-buffer-hook
                    #'org-air-view--dehydrate-source-markers nil t))
        (cl-pushnew source org-air-view--source-tracked-buffers :test #'eq)
        (unless (and (eq (buffer-local-value
                          'org-air-view--source-locator-generation source)
                         items)
                     (buffer-local-value
                      'org-air-view--source-locator-complete source)
                     (hash-table-p
                      (buffer-local-value 'org-air-view--source-locator-index
                                          source))
                     (org-air-view--source-locators-live-p source))
          (unless (and (eq (buffer-local-value
                            'org-air-view--source-locator-generation source)
                           items)
                       (hash-table-p
                        (buffer-local-value
                         'org-air-view--source-locator-index source))
                       (org-air-view--source-locators-live-p source))
            (org-air-view--source-prune-buffer
             source items owner org-air-view--source-item-membership))
          (with-current-buffer source
            (org-with-wide-buffer
             ;; Project native truth in source order once.  Effective tag
             ;; inheritance can otherwise walk toward file metadata again for
             ;; a descending/random validation order.
             (let ((org-air-view--source-projection-index
                    (make-hash-table :test #'eq :size (length matching)))
                   (items-at-position
                    (make-hash-table :test #'eql :size (length matching))))
               (dolist (item matching)
                 (let ((durable (org-air-item-marker item)))
                   (when (and (consp durable) (integerp (cdr durable)))
                     (puthash (cdr durable) item items-at-position))))
               ;; Org's native mapping scanner supplies effective tag truth in
               ;; one forward pass; repeated random `org-element-at-point'
               ;; calls with deferred cache disabled are quadratic by position.
               (org-map-entries
                (lambda ()
                  (when-let* ((item (gethash (point) items-at-position)))
                    (puthash
                     item
                     (cons
                      (point)
                      (list
                       :title
                       (substring-no-properties (org-get-heading t t t t))
                       ;; `org-map-entries' documents `org-scanner-tags' as
                       ;; its O(1) native effective/inherited tag projection.
                       :effective-tags
                       (mapcar #'substring-no-properties org-scanner-tags)))
                     org-air-view--source-projection-index)))
                nil nil)
               ;; Descending positions keep Emacs' per-buffer marker-chain
               ;; insertion linear; exact checks now hit the projection index.
               (dolist (item (reverse matching))
                 (let ((durable (org-air-item-marker item)))
                   (when (and (consp durable) (integerp (cdr durable))
                              (not (gethash
                                    item org-air-view--source-locator-index)))
                     (let ((position (cdr durable)))
                       (when (org-air-view--source-heading-exact-p item position)
                         ;; This marker only observes user drift before an
                         ;; action; durable item/row identity stays numeric.
                         (let ((marker (copy-marker position t)))
                           (push (cons item marker)
                                 org-air-view--source-tracked-locators)
                           (puthash item marker
                                    org-air-view--source-locator-index))))))))
             (let* ((newest org-air-view--source-tracked-locators)
                    (entries (nreverse newest)))
               (setq org-air-view--source-tracked-locators entries
                     org-air-view--source-locator-generation items
                     org-air-view--source-locator-complete t
                     ;; NEWEST is the pre-`nreverse' head cons, which is now
                     ;; the final entry: the bounded witness costs O(1) here
                     ;; and keeps the hot repaint fast path sweep-free.
                     org-air-view--source-locator-witness
                     (when entries
                       (cons (cdr (car entries)) (cdr (car newest)))))))))))))

(defun org-air-view--source-tracking-release-owner (owner)
  "Release global tracking and all source locators owned exactly by OWNER."
  (when (eq owner org-air-view--source-tracking-owner)
    (remove-hook 'find-file-hook #'org-air-view--hydrate-open-source-markers)
    ;; Discovery by exact owner identity survives a major-mode reset that
    ;; erased OWNER's buffer-local roster.
    (dolist (source (buffer-list))
      (when (and (buffer-live-p source)
                 (eq (buffer-local-value 'org-air-view--source-locator-owner
                                         source)
                     owner))
        (with-current-buffer source
          (org-air-view--dehydrate-source-markers))))
    (when (buffer-live-p owner)
      (with-current-buffer owner
        (setq org-air-view--source-tracked-buffers nil
              org-air-view--source-generation 'unset
              org-air-view--source-item-membership nil
              org-air-view--source-items-by-file nil)))
    (setq org-air-view--source-tracking-owner nil)))

(defun org-air-view--hydrate-open-source-markers ()
  "Hydrate this newly visited source for the validated canonical owner."
  (let ((source (current-buffer))
        (owner org-air-view--source-tracking-owner))
    (if (org-air-view--source-owner-valid-p owner)
        (with-current-buffer owner
          (org-air-view--hydrate-source-items source))
      (when owner
        (org-air-view--source-tracking-release-owner owner)))))

(defun org-air-view--hydrate-live-source-markers ()
  "Hydrate already visited files once each without opening any file."
  (when (eq (current-buffer) org-air-view--source-tracking-owner)
    (org-air-view--source-generation-index-ensure org-air-view--items)
    (maphash
     (lambda (file _items)
       (when-let* ((source (find-buffer-visiting file)))
         (org-air-view--hydrate-source-items source)))
     org-air-view--source-items-by-file)))

(defun org-air-view--source-generation-synchronize (items)
  "Prune and hydrate only when ITEMS is a replacement generation."
  (when (and (eq (current-buffer) org-air-view--source-tracking-owner)
             (org-air-view--source-generation-index-ensure items))
    (org-air-view--source-prune-generation items)
    (org-air-view--hydrate-live-source-markers)))

(defun org-air-view--source-tracking-claim ()
  "Validate and claim tracking for the current canonical board entry."
  (let ((owner org-air-view--source-tracking-owner))
    (unless (org-air-view--source-owner-valid-p owner)
      (when owner (org-air-view--source-tracking-release-owner owner)))
    (when (and (eq (current-buffer) (get-buffer org-air-view-buffer-name))
               (derived-mode-p 'org-air-view-mode))
      (unless (eq (current-buffer) org-air-view--source-tracking-owner)
        (setq org-air-view--source-tracking-owner (current-buffer)
              org-air-view--source-generation 'unset
              org-air-view--source-item-membership nil
              org-air-view--source-items-by-file nil))
      (add-hook 'find-file-hook #'org-air-view--hydrate-open-source-markers)
      (add-hook 'kill-buffer-hook
                #'org-air-view--source-tracking-teardown nil t)
      (add-hook 'change-major-mode-hook
                #'org-air-view--source-tracking-teardown nil t))))

(defun org-air-view--source-tracking-teardown ()
  "Release hook and source locators owned by the current canonical board."
  (when (eq (current-buffer) org-air-view--source-tracking-owner)
    (org-air-view--source-tracking-release-owner (current-buffer))))

(defun org-air-view--marked-witness-table ()
  "Return this board's mark witness table, creating it once."
  (unless (hash-table-p org-air-view--marked-witnesses)
    (setq org-air-view--marked-witnesses (make-hash-table :test #'equal)))
  org-air-view--marked-witnesses)

(defun org-air-view--item-projection (item)
  "Return ITEM's bounded `(TITLE . SORTED-EFFECTIVE-TAGS)' projection.
Exactly the two fields `org-air-view--source-heading-exact-p' compares an item
against its source heading with, read from the item's own already-cached
fields: no file is opened, no marker is captured, and nothing unbounded or
source-side is retained.  Tags are sorted copies, so tag ORDER never decides
whether a mark survives."
  (when (org-air-item-p item)
    (cons (substring-no-properties (or (org-air-item-title item) ""))
          (sort (mapcar #'substring-no-properties (org-air-item-tags item))
                #'string<))))

(defun org-air-view--mark-projection-candidates (file items headingp)
  "Return the ITEMS-generation items that share FILE with a marked item.
HEADINGP selects the SAME candidate set the renderer's own per-generation
index holds (`org-air-view--source-generation-index-ensure': heading-kind
items with a source file), so the O(1) index and the fallback pass can never
disagree about which siblings exist — a discriminator that changed with the
index's availability would prune marks for no reason at all.  The index is
used only when it really represents ITEMS; otherwise ONE bounded pass over
the generation rebuilds the same set.  No file is opened either way.

The fallback pass memoizes `expand-file-name' per DISTINCT raw path string
rather than per item: the answer it needs is exact (the same normalisation
`org-air-view--item-source-key' pays) but a generation holds thousands of
items and only a handful of file names, so the pass costs one expansion per
file instead of one per heading."
  (if (and headingp
           (hash-table-p org-air-view--source-items-by-file)
           (eq org-air-view--source-generation items))
      (gethash file org-air-view--source-items-by-file)
    (let ((memo (make-hash-table :test #'equal))
          (absent (list 'absent))
          out)
      (dolist (item items)
        (when (or (not headingp)
                  (and (eq (org-air-item-kind item) 'heading)
                       (stringp (org-air-item-file item))))
          (let* ((marker (org-air-item-marker item))
                 (raw (or (and (consp marker) (stringp (car marker))
                               (car marker))
                          (org-air-item-file item)))
                 (same (when (stringp raw)
                         (let ((cached (gethash raw memo absent)))
                           (if (eq cached absent)
                               (puthash raw
                                        (equal (expand-file-name raw) file)
                                        memo)
                             cached)))))
            (when (and same (org-air-view--item-source-key item))
              (push item out)))))
      (nreverse out))))

(defun org-air-view--mark-sibling-table (file items headingp)
  "Return FILE's `TITLE -> ((POSITION . ITEM) ...)' table in ITEMS.
HEADINGP has the meaning documented by
`org-air-view--mark-projection-candidates'.
One entry per DISTINCT source position, so a heading rendered as several
mirror rows counts once.  Bucketing by TITLE alone keeps this a plain string
hash over the file's headings; the full projection (whose sorted tag copy is
the only real cost) is derived exclusively for the handful of items that
already share the marked title, so a unique heading pays for one bucket of
one.  Built once per file per caller and thrown away — nothing here is
retained between generations, no file is opened, and no marker is captured."
  (let ((table (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'eql)))
    (dolist (item (org-air-view--mark-projection-candidates
                   file items headingp))
      (when-let* ((key (org-air-view--item-source-key item)))
        (when (and (equal (car key) file)
                   (not (gethash (cdr key) seen)))
          (puthash (cdr key) t seen)
          (let ((title (substring-no-properties
                        (or (org-air-item-title item) ""))))
            (puthash title
                     (cons (cons (cdr key) item) (gethash title table))
                     table)))))
    table))

(defun org-air-view--item-mark-witness (item &optional items index)
  "Return ITEM's bounded mark witness, or nil when it cannot be derived.
The witness is `((TITLE . SORTED-EFFECTIVE-TAGS) ORDINAL . ARITY)'.

The projection alone — exactly the two fields
`org-air-view--source-heading-exact-p' compares — is precise about the BOARD
PROJECTION, not about the heading: two headings that share a title and
effective tags are common Org (\"Standup\", \"Weekly review\", generated or
templated entries) and are interchangeable identities under it.  Those are
also exactly the siblings an outside insert or delete can align byte for
byte, because structurally uniform blocks shift every later offset by one
whole block — so a mark could silently re-point at a sibling and spend the
user's marked `b' or tag verb on a heading they never selected, with a
complete-success echo.

DISCRIMINATOR.  ORDINAL is the item's rank among the SAME-PROJECTION headings
of its OWN file (how many of them start before it), and ARITY is how many
there are.  The pair is what makes the sibling case decidable, and it must be
the pair: a delete above the mark lowers the ordinal by exactly as much as
the offset shift raises it, so the ordinal alone re-aligns onto the wrong
sibling; the arity moves with any insert or delete of a same-projection
sibling anywhere in the file, so the two together disagree in both drift
directions.  A unique heading has `(0 . 1)' in every generation, so the
discriminator costs nothing for the overwhelmingly common case and cannot
over-prune it — a heading that merely moves, changes TODO state, gains body
text or is reformatted keeps the identical witness.

An Org `ID'/`CUSTOM_ID' would be stronger still, but no scan slot carries one
today and reading it would mean opening the user's file at mark time; org-air
also never invents or writes an `ID' property.  So the discriminator stays
within what the generation already knows.

FAIL CLOSED.  Return nil whenever the witness cannot be derived from the
generation — no source key, no resolvable position, or an item that does not
appear among its own file's candidates.  A nil witness is never adopted and
never matches, so the mark is pruned as stale through the existing bounded
message instead of being guessed at.

ITEMS defaults to this board's current generation.  INDEX, when given, is an
`equal' hash reused across one reconciliation pass so each file's sibling
table is built at most once; costs stay proportional to the marks plus one
bounded pass over the files that actually hold marks, and nothing is retained
after the call."
  (when-let* ((projection (org-air-view--item-projection item))
              (key (org-air-view--item-source-key item)))
    (let* ((file (car key))
           (position (cdr key))
           (items (or items org-air-view--items))
           (headingp (and (eq (org-air-item-kind item) 'heading) t))
           (slot (cons headingp file))
           (table (or (and index (gethash slot index))
                      (let ((built (org-air-view--mark-sibling-table
                                    file items headingp)))
                        (when index (puthash slot built index))
                        built)))
           (positions
            (delq nil
                  (mapcar
                   (lambda (cell)
                     (when (equal projection
                                  (org-air-view--item-projection (cdr cell)))
                       (car cell)))
                   (gethash (car projection) table)))))
      ;; Ambiguity or absence is a stale mark, never a guess: the item must
      ;; be one of the siblings the generation itself reports for its file.
      (when (memql position positions)
        (cons projection
              (cons (seq-count (lambda (other) (< other position)) positions)
                    (length positions)))))))

(defun org-air-view--marked-table-rebuild ()
  "Rebuild the exact-key membership table from the ordered mark list.
The witness table is confined to the same key set in the same pass, so every
path that drops, clears or reconciles marks releases their witnesses too and
mark storage stays proportional to the number of marks."
  (setq org-air-view--marked-key-table (make-hash-table :test #'equal))
  (dolist (key org-air-view--marked-keys)
    (puthash key t org-air-view--marked-key-table))
  (when (hash-table-p org-air-view--marked-witnesses)
    (let (drop)
      (maphash (lambda (key _witness)
                 (unless (gethash key org-air-view--marked-key-table)
                   (push key drop)))
               org-air-view--marked-witnesses)
      (dolist (key drop)
        (remhash key org-air-view--marked-witnesses))))
  org-air-view--marked-key-table)

(defun org-air-view--marked-key-p (key)
  "Return non-nil when exact source KEY is marked in this board."
  (when key
    (unless (hash-table-p org-air-view--marked-key-table)
      (org-air-view--marked-table-rebuild))
    (and (gethash key org-air-view--marked-key-table) t)))

(defun org-air-view--marks-active-p ()
  "Return non-nil when the current board owns at least one source mark."
  (and (derived-mode-p 'org-air-view-mode)
       org-air-view--marked-keys
       t))

(defun org-air-view--marked-remove-keys (keys)
  "Remove exact KEYS from the ordered mark set and rebuild membership."
  (when keys
    (let ((drop (make-hash-table :test #'equal)))
      (dolist (key keys) (puthash key t drop))
      (setq org-air-view--marked-keys
            (seq-remove (lambda (key) (gethash key drop))
                        org-air-view--marked-keys))
      (org-air-view--marked-table-rebuild))))

(defun org-air-view--marked-reconcile (items)
  "Reconcile the exact source-key selection with a new ITEMS generation.
Missing source keys are pruned, with no title fallback and no file read.
Ordinary cached repaints keep the identical list object and do no work.

MEMBERSHIP IS NOT IDENTITY.  A key is `(FILE . POSITION)', so an ordinary
edit made outside org-air shifts every later heading's offset and the very
same key names a DIFFERENT heading in the replacement generation this call
reconciles against.  Keeping it would silently RE-TARGET the user's mark:
the write path re-resolves key to item from the rebuilt index, so every
downstream exactness check would compare that re-resolved item against
itself, could never fire, and the marked backlog/tag write would move a
heading the user never selected while echoing complete success.  So a
surviving key must still resolve to the heading it was made on: its witness
\(`org-air-view--item-mark-witness') is compared with the newly resolved
item's, and a mismatch is pruned as stale through the same bounded message a
vanished key uses — never silently re-bound.  The witness carries the
bounded same-projection discriminator too, so two headings that share a title
and effective tags are NOT interchangeable identities and the one-block
offset shift that aligns uniform siblings prunes instead of re-targeting.
Resolution is first-item-wins, exactly like `org-air-view--bulk-preflight''s
own key lookup, so the witness gates precisely the item a write would target.
A witness that cannot be derived at all FAILS CLOSED: the mark is pruned
rather than adopted.  Costs stay proportional to the marks plus one bounded
sibling pass over each file that actually holds one (INDEX builds each such
table once and is dropped when this call returns).  Still no file read, no
title fallback, no per-render work — an unchanged generation returns above."
  (unless (eq items org-air-view--marked-generation)
    (let ((live (make-hash-table :test #'equal))
          (witnesses (org-air-view--marked-witness-table))
          (index (make-hash-table :test #'equal))
          (missing (list 'missing))
          (before (length org-air-view--marked-keys)))
      (dolist (item items)
        (when-let* ((key (org-air-view--item-source-key item)))
          (unless (gethash key live)
            (puthash key item live))))
      (setq org-air-view--marked-keys
            (seq-filter
             (lambda (key)
               (when-let* ((item (gethash key live)))
                 (let ((witness (org-air-view--item-mark-witness
                                 item items index))
                       (marked (gethash key witnesses missing)))
                   (cond
                    ;; An underivable witness is an unresolvable mark, and an
                    ;; unresolvable mark is stale — never a guess.
                    ((null witness) nil)
                    ;; A key older than the witness rule (only reachable by
                    ;; setting the mark list directly, never by `m') adopts
                    ;; the resolved identity once instead of vanishing.  A
                    ;; STORED nil is not "older": it is a mark whose identity
                    ;; could not be derived when it was made, so it is pruned.
                    ((eq marked missing) (puthash key witness witnesses) t)
                    ((equal marked witness) t)))))
             org-air-view--marked-keys)
            org-air-view--marked-generation items)
      (org-air-view--marked-table-rebuild)
      (let ((pruned (- before (length org-air-view--marked-keys))))
        (when (> pruned 0)
          (message "Pruned %d stale marked item%s"
                   pruned (if (= pruned 1) "" "s")))))))

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

(defun org-air-view--day-relative-face (time &optional now)
  "Return the R88 PROXIMITY heat-ramp face for TIME relative to NOW, else nil.
Keyed on the SAME `org-air-view--days-between' delta as `--human-date' (so
the label and the face agree by construction):
  delta = 0     -> `org-air-face-day-today'    (ORANGE)
  delta = 1     -> `org-air-face-day-tomorrow' (the today<->week blend)
  2 <= delta <= 6 -> `org-air-face-day-week'   (AMBER — this week)
  delta < 0     -> nil  (a PAST date: a deadline/scheduled is already caught
                        by the OVERDUE arm -> `org-air-face-overdue'; a
                        neutral note keeps its default `org-air-face-date')
  delta >= 7    -> nil  (BEYOND a week -> the slot's default face)
Returned to the deadline / scheduled / notes arms of `org-air-view--date-
label' (R87 rule A), so ANY date within the coming week reads warmer the
nearer it is; a date >=7 days out reads the slot default.  OVERDUE (delta<0)
is owned by the slot's OVERDUE arm, not this helper (a stale note must not
flash critical-red)."
  (let* ((now (or now (current-time)))
         (delta (org-air-view--days-between now time)))
    (cond
     ((= delta 0) 'org-air-face-day-today)
     ((= delta 1) 'org-air-face-day-tomorrow)
     ((<= 2 delta 6) 'org-air-face-day-week))))   ; delta<0 and delta>=7 => nil

(defun org-air-view--marker-timestamp-time (item)
  "Return first timestamp in ITEM subtree, if any.
R53 P2: resolves LIVE markers only (`org-air-classify--item-source'
returns nil for a (FILE . POS) cons — render never opens a file); scanned
items answer from the `activity' slot at the call sites instead.  A stale
position degrades to nil, never a crash."
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
     (deadline
      (cons (org-air-view--human-date deadline now)
            ;; R87: a deadline that falls today/tomorrow WINS its slot — it
            ;; reads teal/rose (day face) instead of the slot orange, so the
            ;; standout reaches the due-date column (text AND svg pill); a
            ;; deadline >=2 days out keeps `org-air-face-deadline'.
            (or (org-air-view--day-relative-face deadline now)
                'org-air-face-deadline)))
     (scheduled
      (cons (org-air-view--human-date scheduled now)
            ;; R87: a scheduled today/tomorrow WINS its slot too (consistency).
            (or (org-air-view--day-relative-face scheduled now)
                'org-air-face-scheduled)))
     ((eq bucket 'attention) (cons "no date" 'org-air-face-date))
     ((eq bucket 'notes)
      ;; R53 P3: a note row's date pill is its scan-time activity.
      (when-let* ((activity (org-air-item-activity item)))
        (cons (org-air-view--human-date activity now)
              ;; R85: a today/tomorrow neutral date stops reading as muted
              ;; grey; a non-today/tomorrow date keeps `org-air-face-date'.
              (or (org-air-view--day-relative-face activity now)
                  'org-air-face-date))))
     ((eq bucket 'stale)
      ;; R53 P2: the scan-time `activity' slot answers data-pure (it IS
      ;; the first-subtree-timestamp ‖ mtime value in this branch — the
      ;; dated cond arms above already caught scheduled/deadline); the
      ;; probe/mtime chain survives only for items built outside the scan.
      (when-let* ((activity (or (org-air-item-activity item)
                                (org-air-view--marker-timestamp-time item)
                                (when-let* ((file (org-air-item-file item))
                                            ((file-exists-p file)))
                                  (file-attribute-modification-time
                                   (file-attributes file))))))
        (cons (format "%s %dd quiet" org-air-chrome-separator
                      (org-air-view--days-between activity now))
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
  "Return the \"#+title:\" keyword of FILE, or nil (F1, `title-from-org').
R54-2: answered from the scan's file-meta table when FILE has an entry
\(`:org-title', the raw `#+title' alone — nil there means the scan SAW no
title, so no read happens and the denote de-slug fallback takes over);
the bounded 4KB read survives only for files the scan has not met."
  (let ((meta (org-air-query-file-meta file)))
    (if meta
        (let ((title (plist-get meta :org-title)))
          (and title (not (string-empty-p title)) title))
      (when (and file (file-readable-p file))
        (ignore-errors
          (with-temp-buffer
            (insert-file-contents file nil 0 4096)
            (goto-char (point-min))
            (when (re-search-forward "^#\\+title:[ \t]*\\(.+?\\)[ \t]*$" nil t)
              (let ((title (match-string-no-properties 1)))
                (unless (string-empty-p title) title)))))))))

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

(defvar org-air-view--filter-now nil
  "The ONE instant the R72 date/status filter tokens evaluate against.
nil (the off-partition default) means `current-time'.
`org-air-view--compute-partition' binds it to the SAME now it classifies
with, so within one render the filter and the buckets read a single
instant; ERT freezes time by let-binding it.  Day-granular predicates,
same as classify.")

(defun org-air-view--filter-tags ()
  "Return active filter tokens as a list (R24-6: tokens stored VERBATIM).
Each token is either a `#tag' (a tag match), an R72 date/status token
\(`is:overdue', `due:7d', … — `org-air-view--filter-token-parse') or a
bare substring token."
  (cond
   ((null org-air-view--tag-filter) nil)
   ((listp org-air-view--tag-filter) org-air-view--tag-filter)
   ((stringp org-air-view--tag-filter) (list org-air-view--tag-filter))))

(defconst org-air-view--filter-is-values
  '("overdue" "upcoming" "stale" "nodate" "hipri" "backlog")
  "The closed value set of the R72 `is:' status-token family.
R83: `backlog' is the bucket-exact twin of the raw-tag `#backlog' — it
selects exactly the `backlog' bucket (board-active ∧ task-routed ∧
tagged), a strict subset of `#backlog' (which also hits done / archived /
note-typed carriers of the tag).")

(defun org-air-view--filter-token-parse (token)
  "Parse TOKEN as an R72 date/status filter token, or return nil.
The closed `qualifier:value' grammar (case-insensitive throughout):
  is:overdue / is:upcoming / is:stale / is:nodate / is:hipri
    => (is . SYMBOL)
  due:Nd / due:Nw (and the scheduled:/deadline: slot twins; the unit is
  REQUIRED, `w' = 7×N days)
    => (SLOT . DAYS) with SLOT one of `due' / `scheduled' / `deadline'.
  is:done / todo:KEYWORD (R79 keyword-identity axis)
    => (status . done) / (todo . NAME).
  path:VALUE (R86 LOCATION axis; VALUE a `/'-joined run of whole path
  components, matched segment-aware + org-root-relative at match time)
    => (path . VALUE).
A `#'-prefixed token is refused outright (the `#' branch of the matcher
stays first, so `#overdue' — and any `#'-spelled literal tag that \"looks
like\" a keyword — is a tag match forever).  Anything else returns nil
and falls through to the existing #tag/substring rules VERBATIM (never
errors; the label quoting is the tell —
`org-air-view--filter-token-label')."
  (when (and (stringp token) (not (string-prefix-p "#" token)))
    (let ((case-fold-search t))
      (cond
       ((string-match
         (concat "\\`is:\\("
                 (mapconcat #'regexp-quote org-air-view--filter-is-values
                            "\\|")
                 "\\)\\'")
         token)
        (cons 'is (intern (downcase (match-string 1 token)))))
       ((string-match
         "\\`\\(due\\|scheduled\\|deadline\\):\\([0-9]+\\)\\([dw]\\)\\'"
         token)
        (cons (intern (downcase (match-string 1 token)))
              (* (string-to-number (match-string 2 token))
                 (if (string-equal-ignore-case (match-string 3 token) "w")
                     7
                   1))))
       ;; R79 keyword-identity axis, AFTER the date/status branches so no
       ;; #tag or R72 token is stolen: `is:done' (any done keyword) and
       ;; `todo:KEYWORD' (case-insensitive keyword identity).  Read the
       ;; item's OWN keyword/done slot with NO board-active gate.
       ((string-equal-ignore-case token "is:done")
        (cons 'status 'done))
       ((string-match "\\`todo:\\(.+\\)\\'" token)
        (cons 'todo (match-string 1 token)))
       ;; R86 LOCATION axis: `path:VALUE' — VALUE is a `/'-joined run of
       ;; whole path components, matched SEGMENT-aware and org-root-
       ;; RELATIVE at MATCH time (`org-air-view--filter-path-token-match-p').
       ;; Kept as the RAW VALUE atom here so the token prints back verbatim
       ;; in the lens (`org-air-view--filter-token-label'); the
       ;; split/relativise is deferred to the matcher.  An empty value
       ;; (`path:' alone) does NOT parse (`\(.+\)' requires >=1 char) and
       ;; falls through to bare-substring VERBATIM.  A `#'-prefixed token is
       ;; already refused above, so a literal `#path:x' tag is a tag forever.
       ((string-match "\\`path:\\(.+\\)\\'" token)
        (cons 'path (match-string 1 token)))))))

(defun org-air-view--filter-keyword-token-match-p (parsed item)
  "Non-nil when PARSED keyword token matches ITEM's own keyword/done slot (R79).
PARSED is a `(todo . NAME)' or `(status . done)' cell from
`org-air-view--filter-token-parse'.  Reads the item's OWN slots with NO
`org-air-classify--board-active-p' gate — the keyword axis is orthogonal
to the R72 date/status axis and MUST select done items (the day pane's
staple).  `(todo . NAME)' is a case-insensitive keyword-identity match;
`(status . done)' is the item's done flag.  Vacuously false when ITEM
carries no keyword slot (empty project/revisit records), like the R72
tokens."
  (pcase parsed
    (`(todo . ,name)
     (and (org-air-item-todo item)
          (string-equal-ignore-case (org-air-item-todo item) name)
          t))
    (`(status . done)
     (and (org-air-item-donep item) t))))

(defun org-air-view--path-segments (path)
  "Split PATH into a list of lower-cased, non-empty path components (R86).
`/'-delimited; empty runs (a leading `/', `//', a trailing `/') are
dropped.  Lower-cased so `path:' matching is case-insensitive (the
`org-air-view--filter-token-match-p' throughout-rule).  Pure string work
— no file access."
  (when path (split-string (downcase path) "/" t)))

(defun org-air-view--path-run-match-p (needle haystack)
  "Non-nil when the NEEDLE component list is a CONTIGUOUS run in HAYSTACK (R86).
Both are `org-air-view--path-segments' outputs (already lower-cased).
Segment-aware, not substring: NEEDLE `(\"re\")' matches HAYSTACK
`(… \"tasks\" \"re\" \"air\" …)' but NOT `(… \"tasks\" \"restore.org\")';
NEEDLE `(\"tasks\" \"re\")' matches only where `tasks' is IMMEDIATELY
followed by `re'.  An empty NEEDLE never matches (an empty `path:' does
not parse — D1).  Pure list work."
  (and needle
       (let ((n (length needle)) (hit nil) (tail haystack))
         (while (and tail (not hit))
           (when (and (>= (length tail) n)
                      (equal needle (seq-take tail n)))
             (setq hit t))
           (setq tail (cdr tail)))
         hit)))

(defun org-air-view--path-relative (file)
  "Return FILE relative to its `org-air-files' source root, else absolute (R86).
Decision B: FILE is made relative to the PARENT of the SHALLOWEST
`org-air-files' source that contains it, so the source's own basename
survives as the first segment (`~/org' + `…/org/tasks/re/foo.org' =>
`org/tasks/re/foo.org') while the machine-specific prefix above the root
is dropped.  A FILE under no configured source is returned ABSOLUTE (still
segment-matchable).  Reads `org-air-files' LIVE at match time; pure
`expand-file-name' string arithmetic — NO disk access (the item's file is
already the scan's truename; sources are `expand-file-name'd, not
truenamed, to stay I/O-free — a symlinked source degrades to the absolute
branch, still matchable)."
  (let* ((exp (expand-file-name (or file "")))
         (best nil) (best-depth nil))
    (dolist (src (and (boundp 'org-air-files) org-air-files))
      (let ((sx (directory-file-name (expand-file-name src))))
        (when (or (equal sx exp)
                  (string-prefix-p (file-name-as-directory sx) exp))
          (let ((depth (length (split-string sx "/" t))))
            (when (or (null best-depth) (< depth best-depth))
              (setq best sx best-depth depth))))))
    (if best
        (file-relative-name exp (file-name-directory (directory-file-name best)))
      exp)))

(defun org-air-view--filter-path-token-match-p (value item)
  "Non-nil when ITEM's source path matches the `path:' VALUE (R86).
VALUE is the raw `(path . VALUE)' payload; ITEM's `org-air-item-file' is
made root-RELATIVE (`org-air-view--path-relative', Decision B) and
SEGMENT-matched against VALUE's component run via
`org-air-view--path-run-match-p' (Decision A).  Vacuously FALSE when
ITEM carries no file (empty project/revisit records) — like the R72/R79
slot tokens.  Reads a cached slot + `org-air-files'; NO file access, NO
board-active/task-routed gate (a LOCATION is orthogonal to planning
state — a DONE note under `tasks/re' still lives there)."
  (when-let* ((file (org-air-item-file item)))
    (org-air-view--path-run-match-p
     (org-air-view--path-segments value)
     (org-air-view--path-segments (org-air-view--path-relative file)))))

(defun org-air-view--filter-date-token-match-p (parsed item now)
  "Non-nil when PARSED (a date/status token) matches ITEM as of NOW.
PARSED is `org-air-view--filter-token-parse' output.  Dispatches onto the
buckets' OWN hoisted classify predicates — the filter contains NO date
arithmetic of its own, so filter⇔bucket agreement holds by construction
\(R72 Decision 3).  Every token conjoins the buckets' top gate
\(`org-air-classify--board-active-p': not done, not archived) — the
filter never resurrects what the board buries — AND the routing gate
\(`org-air-classify--task-routed-p', R77): date/status tokens are TASK
vocabulary, so an item the R54-2 routing layer sends to a note bucket
\(a demoted routine under `org-air-task-requires-todo', a `#+type: note'
overridden scheduled heading) never matches them, extending the R72
agreement law through the routing layer."
  (and (org-air-classify--board-active-p item)
       (org-air-classify--task-routed-p item)
       (pcase parsed
         (`(is . overdue) (org-air-classify--overdue-p item now))
         (`(is . upcoming)
          ;; `is:upcoming' means the KNOB horizon (it widens nothing).
          (org-air-classify--due-within-p item now org-air-upcoming-days))
         (`(is . stale) (org-air-classify--stale-p item now))
         (`(is . nodate)
          ;; Deliberately the R54-1 eligibility NEGATION, not the attention
          ;; bucket's narrower (null sched)(null dl): an item whose only
          ;; date is a body <ts> is dated (it feeds the calendar and the
          ;; stale clock) and must not answer "nodate".
          (not (org-air-classify--stale-eligible-p item)))
         (`(is . hipri) (org-air-classify--hipri-p item))
         ;; R83: `is:backlog' is the bucket-exact twin of `#backlog'.  The
         ;; enclosing board-active ∧ task-routed conjunction already holds,
         ;; so this selects EXACTLY the `backlog' bucket — the R72
         ;; agreement law extended to the deferred set for free.
         (`(is . backlog) (org-air-classify--backlog-p item))
         (`(due . ,days) (org-air-classify--due-within-p item now days))
         (`(scheduled . ,days)
          (org-air-classify--due-within-p item now days 'scheduled))
         (`(deadline . ,days)
          (org-air-classify--due-within-p item now days 'deadline)))
       t))

(defun org-air-view--filter-window-days ()
  "Return the MAX days over the active filter's parsed window tokens.
nil when no `due:'/`scheduled:'/`deadline:' window token is active (R72
Decision 4 — the Upcoming-horizon widening input)."
  (let ((days nil))
    (dolist (tok (org-air-view--filter-tags))
      (pcase (org-air-view--filter-token-parse tok)
        (`(,(or 'due 'scheduled 'deadline) . ,n)
         (setq days (max n (or days 0))))))
    days))

(defun org-air-view--filter-effective-horizon ()
  "Return the effective Upcoming horizon in days (R72 Decision 4).
\(max `org-air-upcoming-days' ACTIVE-WINDOW-DAYS) — widening only, never
narrowing: `due:2d' still renders the full knob-wide Upcoming section
\(the FILTER does the narrowing, the bucket keeps its shape), while
`due:2w' widens the horizon so every item the window selects has a home
row (the probed +8d bucketless hole)."
  (max org-air-upcoming-days (or (org-air-view--filter-window-days) 0)))

(defun org-air-view--filter-path-segments ()
  "Return the distinct DIRECTORY segments across the loaded items (R86).
For each `org-air-view--items' file, `org-air-view--path-relative' then
`org-air-view--path-segments' MINUS the leaf filename (`butlast'); the
union, case-insensitively de-duplicated (segments are already lower-cased)
and sorted for a deterministic `/' completion + byte-stable ordering.
Pure over cached slots; no file access.  The completion teaches the axis
by the user's OWN directory names (`tasks', `re', `air', …); deeper
multi-segment values (`path:tasks/re') are typed freely (the prompt is
not require-match)."
  (let ((seen (make-hash-table :test #'equal)) (out nil))
    (dolist (item org-air-view--items)
      (when-let* ((file (org-air-item-file item)))
        (dolist (seg (butlast (org-air-view--path-segments
                               (org-air-view--path-relative file))))
          (unless (gethash seg seen)
            (puthash seg t seen)
            (push seg out)))))
    (sort out #'string<)))

(defun org-air-view--filter-vocabulary ()
  "Return the date/status + R79 keyword token offer list for `/' completion.
The five `is:' tokens plus knob-tracking window examples
\(`due:7d' / `scheduled:7d' / `deadline:7d' where 7 is the LIVE
`org-air-upcoming-days'), plus the R79 keyword axis: `is:done' and a
`todo:<KW>' for each bare name in the merged scan vocabulary
\(`org-air-view--scan-keyword-names') so `/' completion teaches the
axis by the user's real keywords, plus the R86 LOCATION axis: a
`path:SEG' for each distinct DIRECTORY segment across the loaded items'
root-relative paths (`org-air-view--filter-path-segments').  Offered by
the board and the review view; project and revisit pass nothing (their
records carry no planning slots — R72 Decision 8; the keyword and
location axes are vacuously false there too)."
  (append
   (mapcar (lambda (v) (concat "is:" v)) org-air-view--filter-is-values)
   (mapcar (lambda (q) (format "%s:%dd" q org-air-upcoming-days))
           '("due" "scheduled" "deadline"))
   (list "is:done")
   (mapcar (lambda (kw) (concat "todo:" kw))
           (org-air-view--scan-keyword-names))
   ;; R86: a `path:SEG' offer per distinct DIRECTORY segment across the
   ;; loaded items' root-relative paths (the leaf filename dropped).
   (mapcar (lambda (seg) (concat "path:" seg))
           (org-air-view--filter-path-segments))))

(defun org-air-view--filter-token-label (token)
  "Return TOKEN as it should appear in the filter lens display (R24-6).
A `#tag' token reads verbatim; a bare substring token reads quoted
\(`\"git\"') so the lens presents it as text, not a tag.  R72: a token
that PARSES as a date/status token renders verbatim-unquoted (like a
`#tag'), while an unparsed near-miss keeps its quotes — the rail/banner/
mode-line read `is:overdue' for the real thing but `\"is:urgent\"' for
the typo, so the quoting is the tell."
  (if (or (string-prefix-p "#" token)
          (org-air-view--filter-token-parse token))
      token
    (format "%S" token)))

(defun org-air-view--tag-chip-label (tag)
  "Return TAG's chip label: the `#'-prefixed tag name, prefix-DEDUPED (R69-5).
A tag whose own text already starts with `#' (a literal `:#Nix:' org tag,
kept for svg-tag-mode) is returned VERBATIM — org-air never adds a second
`#' on its chrome (`##Nix').  Only the org-air-prepended prefix collapses:
a tag literally named `##x' still renders `##x' (the user's tag text is
never rewritten).  The ONE label primitive for every tag-NAME chip surface
\(board pills/chips, scope labels, inspectors, project sections, review
rollups); filter-TOKEN surfaces use `org-air-view--filter-token-label'."
  (if (string-prefix-p "#" tag) tag (concat "#" tag)))

(defun org-air-view--filter-token-match-p (token text tags &optional item)
  "Non-nil when TOKEN matches TEXT/TAGS (R24-6 filter mini-language).
A `#tag' token = exact TAG membership of the STRIPPED name OR of the
VERBATIM token (R69-5: a literal `#'-named org tag like `:#Nix:' is hit
by the `#Nix' token its own chip toggles, so deduped chips stay
clickable/filterable — a strict superset of the old stripped-only rule);
an R72 date/status token (`is:overdue', `due:7d', … — the `#' branch
stays FIRST, so `#'-spelled tags can never be stolen) = the bucket
predicate over ITEM's cached planning slots, evaluated against
`org-air-view--filter-now' (one NOW per render) — vacuously FALSE when
no ITEM is threaded (project/revisit: no slots, no claim);
a BARE token = case-insensitive SUBSTRING of TEXT (the caller builds it
from the title + origin/path) plus the tag NAMES (so a bare tag name
still finds its tagged items, the legacy behaviour as a subset).
Case-insensitive throughout."
  (if (string-prefix-p "#" token)
      (let ((names (mapcar #'downcase tags)))
        (and (or (member (downcase (substring token 1)) names)
                 (member (downcase token) names))
             t))
    (if-let* ((parsed (org-air-view--filter-token-parse token)))
        (and item
             (pcase parsed
               ;; R79 keyword axis: read the item's OWN keyword/done slot,
               ;; NO board-active gate (must select done items).
               ((or `(todo . ,_) `(status . done))
                (org-air-view--filter-keyword-token-match-p parsed item))
               ;; R86 LOCATION axis (gate-free): the item's source path.
               ;; Orthogonal to planning state (a DONE note under
               ;; `tasks/re' still lives there), so routed here beside the
               ;; keyword axis, NOT through the gated date matcher.
               (`(path . ,value)
                (org-air-view--filter-path-token-match-p value item))
               ;; R72 date/status axis: the bucket predicate under one NOW.
               (_ (org-air-view--filter-date-token-match-p
                   parsed item (or org-air-view--filter-now (current-time))))))
      (and (string-search (downcase token)
                          (downcase (concat (or text "") " "
                                            (string-join tags " "))))
           t))))

(defun org-air-view--tokens-pass-filter-p (text tags &optional item)
  "Return non-nil when TEXT/TAGS satisfy the active filter tokens + combinator.
SHARED by the board (item title+origin+tags), the review view and the
project (doc name+relpath+tags) — the one matcher every view calls
\(R24-6, generalising R18 D-P3).  Empty filter passes everything;
`org-air-filter-match' selects `all' (AND) or `any' (OR) and spans MIXED
#tag / date-status / bare-substring tokens.  ITEM, when non-nil, is the
`org-air-item' the R72 date/status tokens read their planning slots from
\(board + review thread it; project/revisit pass none — there a date
token is vacuously false, Decision 8)."
  (let ((tokens (org-air-view--filter-tags)))
    (or (null tokens)
        (and (funcall (if (eq org-air-filter-match 'all) #'seq-every-p #'seq-some)
                      (lambda (tok)
                        (org-air-view--filter-token-match-p tok text tags item))
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
bare token substring-matches the title; `#tag' tokens still tag-match;
the ITEM itself is threaded so R72 date/status tokens read its planning
slots."
  (org-air-view--tokens-pass-filter-p
   (concat (org-air-item-title item) " " (org-air-view--origin item))
   (org-air-item-tags item)
   item))

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
`org-air-view--items-for-bucket' calls produced, in ONE classify pass.
R72: binds `org-air-view--filter-now' to the SAME now it classifies with,
so the date/status filter tokens and the buckets evaluate against a
single instant within one render."
  (let* ((now (or now (current-time)))
         (org-air-view--filter-now now)
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
day rollover (or a missing table) rebuilds it.  R72: the memo key is the
pair (DAY . EFFECTIVE-HORIZON), so toggling a window filter token that
widens the Upcoming horizon rebuilds the memo instead of serving stale
buckets — a pure slot-fold rebuild — and is a NO-OP when the horizon is
unchanged (`due:7d' at defaults keys identically to no filter).
R83: `org-air-backlog-tag' joins the key (a RENDER-time classify input,
not a scan-key input — the tag NAME never forces a file reopen), so a
mid-session `setq' of the tag name self-invalidates the memo on the next
repaint (a cheap slot-fold rebuild), never a cold file re-derive; a
backlog-free default keys identically to before."
  (let ((key (list (time-to-days (or now (current-time)))
                   (org-air-view--filter-effective-horizon)
                   org-air-backlog-tag)))
    (unless (and org-air-view--classify-cache
                 (equal org-air-view--classify-cache-day key))
      (setq org-air-view--classify-cache (make-hash-table :test 'eq :size 700)
            org-air-view--classify-cache-day key))))

(defun org-air-view--classify-cached (item &optional now)
  "Return ITEM's bucket list, memoised per board (R18 D-P1c).
Delegates to the pure `org-air-classify-item'; caches the result keyed on
the item object (`eq').  Classify is DAY-granular (every predicate is a
day-window comparison), so the cache key is the day of NOW: a render later
the same day is a pure cache hit; a render after midnight rebuilds.
R72 Decision 4: `org-air-upcoming-days' is bound to the effective horizon
\(`org-air-view--filter-effective-horizon' — widened by an active window
filter token, never narrowed) around the ONE classify choke point, so an
item `due:2w' selects always has a home row; the memo key carries the
horizon (`org-air-view--classify-cache-ensure')."
  (let ((now (or now (current-time))))
    (org-air-view--classify-cache-ensure now)
    (let ((hit (gethash item org-air-view--classify-cache 'miss)))
      (if (eq hit 'miss)
          (puthash item
                   (let ((org-air-upcoming-days
                          (org-air-view--filter-effective-horizon)))
                     (org-air-classify-item item now))
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
    ('notes org-air-notes-preview-limit)
    (_ org-air-section-max)))

(defun org-air-view--notes-by-recency (notes)
  "Return NOTES sorted most-recent-first by scan-time activity (R53 P3).
A top-K selection over precomputed floats — milliseconds at 4k notes."
  (sort (copy-sequence notes)
        (lambda (a b)
          (> (float-time (or (org-air-item-activity a) 0))
             (float-time (or (org-air-item-activity b) 0))))))

(defun org-air-view--displayed-for-bucket-1 (bucket items)
  "Compute (no memo) the BUCKET rows of ITEMS a section renders (R20-6).
Mirrors `org-air-view--insert-section'.  Notes and Backlog are header-only
while collapsed.  Expanded Notes remains preview-capped; expanded Backlog
shows every currently visible member in the active sort order."
  (cond
   ((eq bucket 'notes)
    (when (org-air-view--section-expanded-p 'notes)
      (seq-take (org-air-view--notes-by-recency
                 (org-air-view--items-for-bucket bucket items))
                (max 0 org-air-notes-preview-limit))))
   ((eq bucket 'backlog)
    (when (org-air-view--section-expanded-p 'backlog)
      (org-air-view--sort-items
       (org-air-view--items-for-bucket bucket items) bucket)))
   (t
    (let* ((bucket-items (org-air-view--items-for-bucket bucket items))
           ;; R22-3: order WITHIN the bucket by the active sort key/direction.
           ;; The default key `date' reproduces the historical order exactly.
           (bucket-items (org-air-view--sort-items bucket-items bucket)))
      (if (org-air-view--section-expanded-p bucket)
          bucket-items
        (seq-take bucket-items (org-air-view--section-limit bucket)))))))

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

(defun org-air-view--marked-shown-count (items)
  "Return unique marked source keys from ITEMS represented by rendered rows.
Hidden filtered/scoped/collapsed marks are excluded from this count but
remain in the action target set.  Duplicate bucket rows count once."
  (if (null org-air-view--marked-keys)
      0
    (let ((seen (make-hash-table :test #'equal)))
      (if org-air-view--day
          (dolist (group (org-air-view--day-groups items org-air-view--day))
            (dolist (item (cdr group))
              (when-let* ((key (org-air-view--item-source-key item))
                          ((org-air-view--marked-key-p key)))
                (puthash key t seen))))
        (dolist (descriptor (org-air-view--section-descriptors items))
          (dolist (item (org-air-view--displayed-items-for-bucket
                         (car descriptor) items))
            (when-let* ((key (org-air-view--item-source-key item))
                        ((org-air-view--marked-key-p key)))
              (puthash key t seen)))))
      (hash-table-count seen))))

(defun org-air-view--marked-count-label (items)
  "Return the conditional R90 total/shown mark label for ITEMS, or nil."
  (when org-air-view--marked-keys
    (let ((total (length org-air-view--marked-keys))
          (shown (org-air-view--marked-shown-count items)))
      (if (= total shown)
          (format "• %d marked" total)
        (format "• %d marked · %d shown" total shown)))))

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

(defun org-air-view--banner-left-cols (left)
  "Return the display column cost charged for the banner LEFT token.
On a GRAPHICAL frame the left token carries the height-scaled
`org-air-face-header' (:height 1.2), so it paints more canonical columns
than `string-width' counts; charge it its TRUE pixel width
\(`string-pixel-width' rounded up to whole columns) so the composed row's
pixel extent never exceeds W canonical columns.  In BATCH/TTY this is
exactly `string-width' so the golden path stays byte-identical."
  (if (not (display-graphic-p))
      (string-width left)
    (let* ((px (condition-case nil
                   (string-pixel-width left)
                 (error nil)))
           (cw (frame-char-width)))
      (if (and (numberp px) (> px 0) (> cw 0))
          (ceiling (/ px (float cw)))
        (string-width left)))))

(defun org-air-view--justify (left right width &optional left-cols)
  "Return LEFT and RIGHT justified within display WIDTH.
LEFT-COLS, when non-nil, is the display column cost charged for LEFT in
place of its `string-width' (the banner charges a height-scaled title its
true pixel width).  The emitted string still contains the literal LEFT
bytes; only the pad run shrinks by the pixel excess, so the row's pixel
extent becomes <= WIDTH canonical columns while the string stays a valid
flush-right layout."
  (let* ((left (or left ""))
         (right (or right ""))
         (lcols (or left-cols (string-width left)))
         (excess (- lcols (string-width left)))
         (available (- width lcols (string-width right)))
         (padding (make-string (max 1 available) ?\s)))
    (org-air-view--pad-to (concat left padding right) (- width excess))))

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
    (insert-text-button (org-air-view--tag-chip-label tag)
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

(defun org-air-view--refresh-progress-string ()
  "Return the live scan-progress banner segment text (R56 P3a).
`⟳ scanning N/M…' from the machine's own `org-air-view--refresh-total' /
`org-air-view--refresh-queue' — the ONE honest progress source,
independent of the self-clearing `org-air-view--loading' flag (whose
gating killed the old `loading N/M files' numbers at the first
progressive paint).  The glyph degrades by the S5b tier table."
  (if (> org-air-view--refresh-total 0)
      (format "%s scanning %d/%d…"
              (org-air-view--glyph 'scanning)
              (max 0 (- org-air-view--refresh-total
                        (length org-air-view--refresh-queue)))
              org-air-view--refresh-total)
    (format "%s scanning…" (org-air-view--glyph 'scanning))))

(defconst org-air-view--banner-indent 2
  "Columns of horizontal margin the banner reserves on EACH side.
The left token bakes this many leading spaces (\"  org-air\"); the right
status cluster reserves the SAME count as a trailing gutter so the header is
left/right symmetric.  One constant so the two sides can never drift.")

(defun org-air-view--insert-banner (items)
  "Insert the org-air header band for ITEMS (S1 single in-buffer band).
The right status is justified to the displaying window width W and its
last visible glyph sits at the last usable column (W-1) with NO trailing
pad past the content: the left margin is counted INSIDE W and is never
mirrored as a trailing right blank.  R36-1: the S7 spare column (so a
zero-fringe GUI never draws a continuation glyph over the content) is now
supplied UPSTREAM by R34's fringe-aware `org-air-layout--usable-columns',
not by a hand-reserved trailing space here.  When the window is too
narrow the status sheds tokens in
priority order — filter chips, then scope, then the item count — always
keeping the date.  R27-3: the active-sort badge sheds LAST of the
optional segments (after filter, scope and count)."
  (let* ((w (org-air-view--render-width))
         (left (propertize "  org-air" 'face 'org-air-face-header))
         ;; R38-1: on a GRAPHICAL frame the height-scaled title paints more
         ;; canonical pixel-columns than `string-width' counts; charge it
         ;; its true pixel cost so the row never overhangs the text area.
         ;; In batch/TTY this is `string-width', so goldens are unchanged.
         (left-cols (org-air-view--banner-left-cols left))
         ;; D-P3: per-segment faces — date salient, count faded (or salient
         ;; via `org-air-header-accent-count'), filter/scope faded.  The
         ;; assembled width is unchanged (propertize never alters it).
         (date (propertize (format-time-string "%a %d %b" (current-time))
                           'face 'org-air-face-header-date))
         ;; R20-1: during the brief synchronous fast-paint window the count
         ;; slot shows a static `loading…' cue instead of the item count.
         ;; `org-air-view--loading' is nil on every normal render, so this
         ;; collapses to the unchanged item count (byte-identical).
         ;; R56 P3a: while the machine is REFRESHING the slot is ONE
         ;; prominent progress segment — `⟳ scanning N/M…', salient-faced,
         ;; numbers from the machine's own queue/total — shown on the
         ;; skeleton, the streaming cold board AND the painted cache-stale
         ;; board alike (independent of `--loading'), replacing both the
         ;; old `loading N/M files' (whose `--loading' gate killed it at
         ;; the first progressive paint) and the faded `stale ·
         ;; refreshing…'.  `failed' keeps its text, salient face.  All
         ;; display-only and transient; the machine never runs in batch,
         ;; so no golden captures them.
         (busy (or org-air-view--loading org-air-view--refresh-state))
         (refreshing (eq org-air-view--refresh-state 'refreshing))
         (count (cond
                 (refreshing
                  (propertize
                   (concat (org-air-view--sep)
                           (org-air-view--refresh-progress-string))
                   'face 'org-air-face-progress))
                 (org-air-view--loading
                  (propertize (concat (org-air-view--sep) "loading…")
                              'face 'org-air-face-faded))
                 ((eq org-air-view--refresh-state 'failed)
                  (propertize
                   (concat (org-air-view--sep) "stale"
                           ;; R50-1: the retry key is the TRUE sequence
                           ;; `g r' (`g' alone is the B4 prefix map).
                           (org-air-view--sep)
                           "refresh failed (g r retries)")
                   'face 'org-air-face-progress))
                 (t (propertize
                     (concat (org-air-view--sep)
                             (format "%d items"
                                     (length (org-air-view--visible-items
                                              items))))
                     'face (if (and (not busy) org-air-header-accent-count)
                               'org-air-face-count
                             'org-air-face-faded)))))
         ;; R18 D-P2.3: with >=2 active filter tags, join them with the
         ;; combinator word (AND/OR) so the mode reads inline; a single tag
         ;; shows no combinator (irrelevant).
         ;; R90: source-key marks are a non-sheddable-before-filter status
         ;; segment.  Empty mark state contributes no bytes.
         (mark-text (when-let* ((label (org-air-view--marked-count-label items)))
                      (propertize (concat (org-air-view--sep) label)
                                  'face 'org-air-face-faded)))
         (filter-text (let* ((filters (org-air-view--filter-tags))
                             (sep (if (> (length filters) 1)
                                      (concat " " (org-air-view--filter-combinator-word) " ")
                                    " ")))
                        (when filters
                          (propertize
                           (concat (org-air-view--sep)
                                   ;; R69-5: route through the R24-6 token
                                   ;; primitive (verbatim `#…', quoted bare)
                                   ;; instead of hand-prepending `#'.
                                   ;; R74 (the R69-2 sibling site): the
                                   ;; trailing ✕ clear glyph is DROPPED —
                                   ;; it carried no keymap/button action (a
                                   ;; promise the banner could not honour);
                                   ;; the rail's `\\ clears' hint is the
                                   ;; teaching surface.  The glyph table
                                   ;; entry stays (the project header still
                                   ;; renders it).
                                   (mapconcat #'org-air-view--filter-token-label filters sep))
                           'face 'org-air-face-faded))))
         (scope-text (pcase org-air-view--scope
                       ;; R69-5: prefix-deduped chip label (a `#Nix' tag
                       ;; scope reads `#Nix', never `##Nix').
                       (`(:tag ,tag) (propertize (concat (org-air-view--sep)
                                                         (org-air-view--tag-chip-label tag))
                                                 'face 'org-air-face-faded))
                       (`(:group ,group) (propertize (concat (org-air-view--sep) "@" group)
                                                     'face 'org-air-face-faded))
                       (`(:file ,file) (propertize
                                        (concat (org-air-view--sep) (file-name-nondirectory file))
                                        'face 'org-air-face-faded))
                       (_ nil)))
         ;; R22-3: the within-bucket sort indicator, shown ONLY when a
         ;; non-default sort is active (default `date'/ascending -> nil ->
         ;; the default banner is byte-identical).  R27-3: whenever the
         ;; segment exists it IS the active state, so it takes the bold
         ;; high-contrast `org-air-face-sort-active' and sheds LAST under
         ;; narrow widths (see the shed order below).
         (sort-text (unless (org-air-view--sort-default-p)
                      (concat (propertize (org-air-view--sep) 'face 'org-air-face-faded)
                              (org-air-view--sort-indicator-text
                               (org-air-view--sort-active-key)
                               (org-air-view--sort-active-direction)
                               (not (org-air-view--sort-default-p))))))
         ;; Budget for the status: window minus the left token, the >=1-col
         ;; gap, and R39-1's symmetric right gutter (the same indent the left
         ;; token bakes).  R36-1: no reserved right-margin column beyond this
         ;; (R34's usable-columns already supplies the spare column upstream).
         (budget (- w left-cols org-air-view--banner-indent))
         (assemble (lambda (shed)
                     (concat date
                             (unless (memq :count shed) count)
                             (unless (memq :marks shed) (or mark-text ""))
                             (unless (memq :filter shed) (or filter-text ""))
                             (unless (memq :scope shed) (or scope-text ""))
                             (unless (memq :sort shed) (or sort-text "")))))
         (status (catch 'fit
                   ;; R27-3: the active-sort segment sheds LAST among the
                   ;; optional segments — the state the user asked for must
                   ;; not be the first casualty of a narrow window.  With no
                   ;; active sort the segment is nil, so the order change is
                   ;; unobservable and the default goldens hold.
                   ;; R56 P3a: while REFRESHING the progress segment (the
                   ;; count slot) sheds LAST of all — a narrow window drops
                   ;; decoration before it drops "it's working".
                   (dolist (shed (if refreshing
                                     '(() (:filter) (:filter :scope)
                                       (:filter :scope :sort)
                                       (:filter :scope :sort :marks)
                                       (:filter :scope :sort :marks :count))
                                   '(() (:filter) (:filter :scope)
                                     (:filter :scope :count)
                                     (:filter :scope :count :sort)
                                     (:filter :scope :count :sort :marks)))
                                 date)
                     (let ((s (funcall assemble shed)))
                       (when (<= (string-width s) budget)
                         (throw 'fit s))))))
         ;; D-P3: the segments already carry their faces; keep the assembled
         ;; status as-is (no blanket faded override).
         (right status)
         ;; R39-1: right-align the status to (usable - banner-indent) so the
         ;; header has a trailing gutter equal to the left indent — the line
         ;; is symmetric (lhs-margin == rhs-margin == banner-indent).  The
         ;; gutter stays INSIDE w (justify pads to w-indent), never emitted as
         ;; trailing whitespace past the declared width.  S7's spare column is
         ;; supplied upstream by R34's fringe-aware usable-columns.
         (line (org-air-view--justify left right
                                      (- w org-air-view--banner-indent)
                                      left-cols)))
    (insert line "\n")))

(defun org-air-view--rule-string (width)
  "Return a horizontal rule of display WIDTH."
  (let ((glyph (org-air-view--glyph 'hrule)))
    (mapconcat #'identity (make-list width glyph) "")))

(defun org-air-view--insert-rule ()
  "Insert a faint FULL-WIDTH separator flush to both text-area edges (R41-1).
The rule spans the entire usable width (`0' .. `usable', where `usable' =
`org-air-view--render-width', the R37 body-1 safety margin), so it is
`org-air-view--banner-indent' (2) columns wider on the LEFT and 2 wider
on the RIGHT than the R40-1 inset rule — a full-bleed rule under the
inset R39-1 banner content.  The rule ends exactly at the last usable
column (`usable - 1') and never overshoots past `usable'."
  (let* (;; R41-1: full usable width, no leading margin — flush left window
         ;; edge, ending at the last usable column (no right overshoot).
         (rule-width (max 0 (org-air-view--render-width))))
    (insert (propertize (org-air-view--rule-string rule-width)
                        'face 'org-air-face-separator)
            "\n")))

(defun org-air-view--empty-upcoming ()
  "Return upcoming empty state.
R72: reads the EFFECTIVE horizon (`org-air-view--filter-effective-horizon'
— the knob, widened by an active window filter token), so it can never
announce \"next 7 days\" under a 14-day lens."
  (format "Nothing scheduled in the next %d days."
          (org-air-view--filter-effective-horizon)))

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

(defconst org-air-view--dropped-keyword-names
  '("DROPPED" "DROP" "CANCELLED" "CANCELED" "KILL" "KILLED" "ABANDONED")
  "Bare (upcased) keyword names that read as cancelled/abandoned (R79).
An unknown keyword whose bare name is here resolves to
`org-air-face-dropped' even when it is not declared in the scan
vocabulary, so the user's real spelling paints terracotta, not blue.")

(defun org-air-view--dropped-keyword-p (keyword)
  "Non-nil when KEYWORD is a cancelled/abandoned spelling (R79/R84).
The ONE membership test over `org-air-view--dropped-keyword-names',
matched on the bare upcased name (`org-air-query--todo-keyword-name');
shared by the R79 face resolver (`org-air-view--merged-vocab-face') and
R84's `org-air-review--abandoned-p' — the face split and the review's
Dropped routing agree by construction.  Pure; never signals."
  (let ((name (and keyword (org-air-query--todo-keyword-name keyword))))
    (and name (member (upcase name) org-air-view--dropped-keyword-names) t)))

(defun org-air-view--scan-keyword-names ()
  "Return the flat list of bare keyword names in the merged scan vocabulary (R79).
Flattens `org-air-query--scan-todo-keywords' (the R57 merged
user+supplement sequence) to bare names, dropping the `|' separators;
never signals (a nil/malformed scan yields nil)."
  (delete-dups
   (cl-loop for seq in (ignore-errors (org-air-query--scan-todo-keywords))
            append (cl-loop for kw in (cdr seq)
                            unless (equal kw "|")
                            collect (org-air-query--todo-keyword-name kw)))))

(defun org-air-view--merged-vocab-face (keyword &optional donep)
  "Return a family face for KEYWORD via the R57 merged scan vocabulary (R79).
An unknown KEYWORD is placed in a family by its POSITION in its scan
sequence: a cancelled/abandoned spelling (`org-air-view--dropped-keyword-
names') is `org-air-face-dropped'; a scan-DONE keyword is
`org-air-face-done'; a scan not-done keyword is `org-air-face-todo'.  A
keyword absent from the whole vocabulary falls back on DONEP — done items
still never wear an active badge (R57-1).  Pure over cached list data; no
rescan, never signals."
  (let* ((name (and keyword (org-air-query--todo-keyword-name keyword)))
         (in-vocab (and name (member name (org-air-view--scan-keyword-names))))
         (scan-done (and name (member name
                                      (ignore-errors
                                        (org-air-query-merged-done-keywords)))))
         (cancelled (org-air-view--dropped-keyword-p keyword)))
    (cond
     (cancelled 'org-air-face-dropped)
     ((and in-vocab scan-done) 'org-air-face-done)
     (in-vocab 'org-air-face-todo)
     (donep 'org-air-face-done)
     (t 'org-air-face-todo))))

(defun org-air-view--org-keyword-face (keyword)
  "Return the user's own `org-todo-keyword-faces' face for KEYWORD, or nil (R79).
Consulted only when `org-air-keyword-face-source' is `org' — the literal
\"same colours Org fontifies headings with\".  A face symbol passes
through; a colour string wraps to `(:foreground COLOR)'; an anonymous
face plist passes through; anything unnamed/malformed returns nil so the
caller degrades to the `own' mapping.  Never signals."
  (when (and keyword (boundp 'org-todo-keyword-faces))
    (ignore-errors
      (let ((spec (cdr (assoc keyword org-todo-keyword-faces))))
        (cond
         ((null spec) nil)
         ((and (symbolp spec) (facep spec)) spec)
         ((stringp spec) (list :foreground spec))
         ((and (listp spec) (keywordp (car-safe spec))) spec)
         (t nil))))))

(defun org-air-view--todo-face (keyword &optional donep)
  "Return the face for TODO KEYWORD (T1a; R79 supersedes the R57-1 fallback).
Resolution order: when `org-air-keyword-face-source' is `org', the user's
own `org-todo-keyword-faces' first; then org-air's own
`org-air-todo-keyword-faces' alist; then the R57 merged scan-vocabulary
family (`org-air-view--merged-vocab-face': not-done→active, done→done
unless a cancelled spelling→dropped), finally DONEP→done / else→todo.
R79 splits the DONE family so a completion (DONE/COMP) and an abandonment
\(DROPPED/CANCELLED/KILL) read as DIFFERENT faces, and resolves unknown
keywords through the merged vocabulary instead of the blanket done
fallback — so COMP/DROPPED/READY/WIP each read distinctly.  DEFAULT
\(`own') keeps R57-1 and the board goldens byte-identical."
  (or (and (eq org-air-keyword-face-source 'org)
           (org-air-view--org-keyword-face keyword))
      (cdr (assoc keyword org-air-todo-keyword-faces))
      (org-air-view--merged-vocab-face keyword donep)))

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

(defun org-air-view--priority-cell (item)
  "Return ITEM's row-prefix priority cell (the shared board idiom; R84).
`square (default): the FIXED 2-col `org-air-view--priority-slot' (a
coloured square + pad, or two blanks — every title aligns).  `badge/
`text: the conditional `org-air-view--priority-token' + one pad, ONLY
when ITEM carries a shown priority (`org-air-priority-show'), else nil.
The ONE definition the board (`org-air-view--insert-item') and the
review row (`org-air-review--insert-body') both prepend — no fork; a
byte-golden on the board proves the refactor is inert (R84 r84-2)."
  (let ((priority (org-air-view--priority-char item)))
    (if (eq org-air-priority-style 'square)
        (org-air-view--priority-slot priority)
      (when (and priority (member priority org-air-priority-show))
        (concat (org-air-view--priority-token priority) " ")))))

(defun org-air-view--svg-available-p ()
  "Return non-nil when svg pills can be drawn on this display (C2)."
  (and (display-graphic-p)
       (require 'svg nil t)))

(defun org-air-view--char-dimensions ()
  "Return (CHAR-W . CHAR-H) device pixels for the displaying window (C2/C3).
Uses `window-font-width'/`window-font-height' on the window actually
showing the org-air buffer so the metrics track the current font AND any
`text-scale-mode' adjustment (C3).

R44-1 (pixel split-brain fix): a board svg pill spans N text cells as a
`display' image whose width is N * this CHAR-W, while the redisplay engine
advances the divider column and the surrounding plain text at the window's
REAL default-face font advance (`window-font-width').  When the two
disagree (HiDPI / fractional text-scale / a fallback glyph in the default
face) every pill is baked NARROWER than its cells and drags the divider
glyph LEFT, so col 146 lands at a different pixel-X on every row (the
zig-zag).  To keep the pill metric == the divider column's advance ALWAYS,
when no live window yet shows this buffer (async / first render, BEFORE
`pop-to-buffer' settles) the metric is resolved off the DESTINATION window
\(the selected window on its graphical frame) via `window-font-width' —
NEVER the `frame-char-width' fallback while the two disagree.  Only a
truly non-graphical context (pure batch, no frame) keeps the frame char
metrics, and there svg pills are not drawn at all (the TTY text fallback)."
  (let ((win (or (get-buffer-window (current-buffer) t)
                 ;; R44-1: no live window shows this buffer yet — resolve
                 ;; the metric off the destination window so a pill is sized
                 ;; to `window-font-width', the SAME advance the divider
                 ;; column and plain text use, not `frame-char-width'.
                 (let ((sw (selected-window)))
                   (and (window-live-p sw)
                        (display-graphic-p (window-frame sw))
                        sw)))))
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
                         ;; R69-5: prefix-deduped chip label (one shared
                         ;; primitive; a literal `#nix' tag never `##nix').
                         (chip (if pill
                                   (org-air-view--pill-pad-label
                                    (org-air-view--tag-chip-label tg) face)
                                 (propertize (org-air-view--tag-chip-label tg)
                                             'face face))))
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
legend (`e' `org-air-refile-item' stays bound), the single teaching
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
  ;; R30-3: each cluster column is GATED at the width pass by its
  ;; defcustom toggle.  A hidden column seeds/accumulates 0 width, so
  ;; `org-air-view--insert-row' skips the cell (its `(when (> col 0))'
  ;; guards already exist) and the freed columns flow to the flex title
  ;; via the SAME title-min fit pass — V6 alignment holds by construction
  ;; (the widths are recomputed every render, so the relock is automatic).
  (let ((dw (if org-air-show-dates org-air-date-column 0))
        (tw 0) (ow 0) (rep 0) (tw-todo 0))
    (dolist (descriptor (org-air-view--section-descriptors items))
      (let* ((bucket (car descriptor))
             (bucket-items (org-air-view--displayed-items-for-bucket bucket items)))
        (dolist (item bucket-items)
          (when (and org-air-show-dates
                     (org-air-view--item-repeat-timestamp item))
            (setq rep 2))
          (setq tw-todo (max tw-todo
                             (string-width (or (org-air-item-todo item) ""))))
          (let* ((date (org-air-view--date-label item bucket))
                 (tags (org-air-item-tags item))
                 (n (length tags))
                 (ts (org-air-view--item-tagstr
                      tags (min org-air-tags-inline-max n) n)))
            (when (and org-air-show-dates date)
              (setq dw (max dw (+ (string-width (car date))
                                  (if (eq org-air-date-style 'pill)
                                      (* 2 (max 0 org-air-pill-pad-cols))
                                    0)))))
            (when org-air-show-tags
              (setq tw (max tw (string-width ts))))
            (when org-air-show-origin
              (setq ow (max ow (string-width
                                (org-air-view--item-origin-raw item)))))))))
    ;; R80: floor the keyword column so a SHORT keyword (OUT/OFF, 3 cols)
    ;; reserves a cell the SAME width as a 5-col DRAFT state chip -- the
    ;; keyword badge (`org-air-view--svg-keyword-badge') pads its pill to
    ;; the same floor, so box <= cell and the pill never overflows.  Only
    ;; when the badge style is on AND some row HAS a keyword (tw-todo>0);
    ;; a keyword already >= the floor (WAITING, 7) is a no-op (byte-
    ;; identical), and an all-keywordless board reserves no column.
    (when (and (eq org-air-keyword-style 'badge) (> tw-todo 0))
      (setq tw-todo (max tw-todo org-air-keyword-badge-min-cols)))
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
cell and the project state cell.

R80: TEXT is first padded to at least `org-air-keyword-badge-min-cols'
columns (default 5 = `org-air-project--state-cell-w'), the label centring,
so a SHORT keyword (OUT/OFF, 3 cols) renders a pill the SAME size as a
5-col DRAFT state chip instead of a tiny capsule -- the widening is
COLUMNS only (never a `:height').  A keyword already >= the floor
\(WAITING, 7) keeps its natural width (the pad is a no-op).  Returns the
PADDED token unchanged (the mandatory text fallback) when
`org-air-keyword-style' is `text', when svg is unavailable, or when TEXT
is blank, so the byte/TTY layer reserves the SAME min width as a state
token."
  (let ((padded (org-air-view--pad-to
                 text (max (string-width text)
                           org-air-keyword-badge-min-cols))))
    (if (or (not (eq org-air-keyword-style 'badge))
            (not (org-air-view--svg-available-p))
            (string-empty-p (string-trim text)))
        padded
      (let ((color (face-foreground face nil t)))
        (org-air-view--svg-pillify padded face :border-color color)))))

(defun org-air-view--todo-cell (todo width &optional donep)
  "Return a fixed-width reserved TODO-keyword cell (R15 D-P1).
WIDTH is the board-wide widest keyword (`org-air-view--meta-todo-w').
When WIDTH is 0 no rendered item has a keyword, so return an empty
string (no wasted column).  Otherwise return TODO in its todo-face (or
WIDTH blanks when absent), left-justified and padded to WIDTH, plus a
single trailing space separator -- so every row contributes WIDTH+1
columns here and all titles share one left edge.  R21-4: when TODO is
present, overlay the shared svg keyword badge on the (unchanged) padded
keyword text -- a `display' overlay, so the byte/TTY layer is identical.
R57-1: DONEP is the item's done flag, threaded to
`org-air-view--todo-face' so an unknown done keyword renders faded."
  (if (<= width 0)
      ""
    (concat (org-air-view--pad-to
             (if todo
                 (org-air-view--svg-keyword-badge
                  (propertize todo 'face (org-air-view--todo-face todo donep))
                  (org-air-view--todo-face todo donep))
               "")
             width)
            " ")))

(defun org-air-view--meta-cluster-width ()
  "Return the fixed metadata cluster width from the live meta-* column globals.
Sums the present (width>0) date / tags / origin columns plus one single-space
separator between each, mirroring `org-air-view--compute-meta-widths' and
`org-air-view--insert-row' EXACTLY.  This is the fixed cluster width every
standard no-rail row reserves on its right (R34-2)."
  (let* ((dcol (+ (or org-air-view--meta-date-w 0)
                  (or org-air-view--meta-date-repeat 0)))
         (tcol (or org-air-view--meta-tags-w 0))
         (ocol (or org-air-view--meta-origin-w 0))
         (cells (delq nil (list (and (> dcol 0) dcol)
                                (and (> tcol 0) tcol)
                                (and (> ocol 0) ocol)))))
    (+ (apply #'+ cells) (max 0 (1- (length cells))))))

(defun org-air-view--fence-column (width &optional cluster-w)
  "Return the column the no-rail fence / metadata cluster right-anchors to.
WIDTH is the live `org-air-view--render-width' (post R37 usable=body-1).  BOTH
the vertical fence line and every standard row's metadata cluster right-anchor
share this ONE column so they align exactly — the column is `WIDTH minus the
cluster width'.  CLUSTER-W defaults to the live board `meta-cluster-width'
\(the no-arg form the fence renderer / test call); `org-air-view--insert-row'
passes its own row CLUSTER-W so board and project share this ONE derivation.
Deriving it here, from the one live WIDTH passed into the render pass, keeps
the fence and the cluster from desyncing (the R37 usable / R38-2 inspector
seams).  Reverting it (fence and cluster read different widths) reintroduces
the reported 1-2 col drift."
  (- width (or cluster-w (org-air-view--meta-cluster-width))))

(cl-defun org-air-view--insert-row (&key prefix title date-text tags
                                         origin-text origin-face widths
                                         props face own-fence marked)
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
this one primitive, faces, truncation, alignment and svg pills).
Exception (R47-2): a `mouse-face' in PROPS is scoped to the TEXT-ONLY
title band [title-start, cluster-start) instead of the whole row, so no
position ever carries both `mouse-face' and an image `display' — hover
never re-rasterizes the SVG pills (Emacs 30's DRAW_MOUSE_FACE SVG
re-lookup can never fire)."
  (let* ((start (point))
         (width (org-air-view--render-width))
         (prefix (or prefix ""))
         ;; R90: the mark glyph consumes one existing indentation space.
         ;; Width and every downstream V6 column remain unchanged.
         (prefix (if (and marked (string-match " " prefix))
                     (concat (substring prefix 0 (match-beginning 0))
                             "•"
                             (substring prefix (1+ (match-beginning 0))))
                   prefix))
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
         ;; date cell: left-justified, padded to EXACTLY its global column
         ;; DCOL — no local expansion past DCOL (R40-2 lockstep).  The R26-6
         ;; no-nudge contract removed the one expander (the "· r to file"
         ;; Inbox nudge that baked extra width into DATE-TEXT), so a bare
         ;; DATE-TEXT always fits DCOL and this is byte-identical to the old
         ;; `(max dcol …)' form on every fixture.  Keeping it at DCOL (not
         ;; `max') means the cluster field width can never exceed the
         ;; board-wide `meta-cluster-width', so every row's cluster field ==
         ;; `meta-cluster-width' in lockstep and the shared fence column
         ;; below is exact for all rows (a divergent wide date is truncated,
         ;; never allowed to shove the fence).
         (date-cell (when (> dcol 0)
                      (org-air-view--pad-to (or date-text "") dcol)))
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
         (had-title (> (length title) 0))
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
         ;; R46-2 (secondary): the LEFT re-truncation above can chop the
         ;; R21-2-marked title glyph on a narrow row (the ellipsis replaces
         ;; the whole title cell), leaving the row with NO title mark, so
         ;; `org-air-view--row-title-pos' fell back to the row's first
         ;; visible glyph (the keyword cell) and the R46 band start
         ;; wandered between columns down one section.  Re-apply the mark
         ;; to the title's surviving first glyph — or the ellipsis remnant
         ;; when even that is gone — so the band start is stable.  A text
         ;; property only; the visible bytes are untouched.
         (left (if (and had-title (> (length left) 0)
                        (not (text-property-not-all
                              0 (length left) 'org-air-row-title nil left)))
                   (let ((lt (copy-sequence left)))
                     (put-text-property
                      (min (length prefix) (1- (length lt)))
                      (min (1+ (length prefix)) (length lt))
                      'org-air-row-title t lt)
                     lt)
                 left))
         ;; R40-2: right-anchor the cluster to the ONE board-wide fence
         ;; column.  The standard no-rail BOARD row (OWN-FENCE nil) anchors
         ;; to the SHARED no-arg `org-air-view--fence-column' — derived from
         ;; the live WIDTH and the board-wide `meta-cluster-width' — so the
         ;; vertical fence is CONTINUOUS BY CONSTRUCTION: every board row
         ;; (and every blank/fill/separator row that reads the same helper)
         ;; lands on the identical column regardless of its OWN cluster
         ;; width, in LOCKSTEP.  This supersedes R39-2's per-row CLUSTER-W
         ;; anchoring, whose continuity was one divergent row away from
         ;; breaking.  The project view / day pane compose their OWN cluster
         ;; field (different globals) and pass OWN-FENCE t to keep anchoring
         ;; to THIS row's CLUSTER-W (documented exception — not the no-rail
         ;; board fence the user reports); with a lockstep cluster field this
         ;; is byte-identical to the board path on the current fixtures.
         (anchor (org-air-view--fence-column width (and own-fence cluster-w)))
         (pad (max gap (- anchor (string-width left))))
         (line (concat left (make-string pad ?\s) cluster)))
    (insert line "\n")
    (when (or props face)
      ;; R47-2: pop `mouse-face' OUT of the whole-extent PROPS.  Since Emacs
      ;; 30 (commit e69fafdb, bug#67794, "Respect mouse-face on SVG image
      ;; glyphs") `draw_glyphs' re-looks-up EVERY SVG image glyph drawn under
      ;; DRAW_MOUSE_FACE with the hover face; the C image cache keys on the
      ;; face's fg/bg/font, and the hover :background DIFFERS from the row
      ;; face by construction, so a full-row `mouse-face' forced a synchronous
      ;; librsvg re-rasterization of every SVG pill in the row on EVERY
      ;; crossing (the R45 cold-pill cost relocated onto the hover hot path).
      ;; Invariant: NO buffer position may carry both `mouse-face' and an
      ;; image `display'.  Every OTHER row-identity property
      ;; (`org-air-item'/`org-air-doc', marker, R21-2 title mark,
      ;; `font-lock-face') keeps the FULL row extent so click/RET resolution,
      ;; R32-3 open-target and the inspector are untouched — ONLY the
      ;; highlight span narrows.
      (let ((hover (plist-member props 'mouse-face))
            (row-props nil))
        (let ((tail props))
          (while tail
            (unless (eq (car tail) 'mouse-face)
              (setq row-props
                    (nconc row-props (list (car tail) (cadr tail)))))
            (setq tail (cddr tail))))
        (when (or row-props face)
          (add-text-properties start (point)
                               (append row-props
                                       (when face
                                         (list 'font-lock-face face)))))
        ;; R47-2: apply the popped `mouse-face' over the TEXT-ONLY TITLE
        ;; BAND [title-start, cluster-start) — from the R21-2/R46 title mark
        ;; through the flex pad, ENDING where the meta cluster begins (the
        ;; R40-2 fence).  The band is text-only BY CONSTRUCTION (the title is
        ;; `substring-no-properties' plain text per R23-1; every pill lives
        ;; in the prefix badges BEFORE it or the cluster AFTER it), so it is
        ;; ONE contiguous hover run per row that never covers an SVG pill —
        ;; a crossing re-blits only text glyph backgrounds: ZERO pill
        ;; rasterizations, ZERO `lookup_image' calls, on every Emacs
        ;; version.  It also matches R46: the highlight shows exactly the
        ;; band point is clamped to on click.  Degenerate rows (empty title
        ;; after truncation, no title mark) get NO `mouse-face' at all — NO
        ;; span is always safer than a wrong span.  The R32-1 invariant (no
        ;; hover run spans a newline, so adjacent rows never fuse) holds by
        ;; construction: the band ends at the cluster start, never reaching
        ;; the row's newline; the old explicit newline strip is vacuous now
        ;; and dropped.
        (when (cadr hover)
          (let ((title-idx (text-property-not-all
                            0 (length left) 'org-air-row-title nil left)))
            (when title-idx
              (put-text-property (+ start title-idx)
                                 (+ start (length left) pad)
                                 'mouse-face (cadr hover)))))))))

(defun org-air-view--insert-item (item bucket &optional omit-date)
  "Insert ITEM as an interactive row in BUCKET (V6 fixed-column table).
A thin caller of the shared `org-air-view--insert-row' (D-P5.A): it maps
the task ITEM onto the row args (todo/priority prefix, title, date / tags
/ origin cluster).  OMIT-DATE drops the date column (R6 day view)."
  (let* ((todo (org-air-item-todo item))
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
                         (org-air-view--todo-cell todo todo-w
                                                  (org-air-item-donep item))
                         ;; R13 D-P2 / R84 D1a: the SHARED priority cell —
                         ;; `square emits a FIXED 2-col slot on EVERY row
                         ;; (square or blank) so titles align; `badge/`text
                         ;; keep the conditional `[#A]' token.  Extracted to
                         ;; `org-air-view--priority-cell' so the review row
                         ;; prepends the SAME idiom (no fork; board inert).
                         (org-air-view--priority-cell item)))
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
         (ocol (or org-air-view--meta-origin-w (string-width origin-raw)))
         (marked (org-air-view--marked-key-p
                  (org-air-view--item-source-key item))))
    (org-air-view--insert-row
     :prefix prefix
     :title (org-air-item-title item)
     :date-text date-text
     :tags tagstr
     :origin-text origin-raw
     ;; R22-7: the origin reads at AA (mid-tier) instead of sub-AA faded.
     :origin-face 'org-air-face-origin
     :widths (list dcol tcol ocol)
     ;; R40-2: the STANDARD no-rail board row (OMIT-DATE nil) anchors to the
     ;; SHARED board-wide fence column (OWN-FENCE nil) so the vertical fence
     ;; is continuous by construction.  The R6 DAY PANE (OMIT-DATE) composes
     ;; its own focused cluster field with the board globals let-unset, so it
     ;; keeps anchoring to THIS row's cluster width (OWN-FENCE t).
     :own-fence omit-date
     :marked marked
     :props (append (list 'org-air-item item
                          'org-air-marker (org-air-item-marker item)
                          'mouse-face 'org-air-face-cursor)
                    (and marked (list 'org-air-marked t)))
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
  "Return non-nil when the sort is this view's byte-identical default (R22-3).
R79: the default key is view-aware — `time' in the single-day view
\(chronological within each group), else the board `date'.  So the R22-3
banner indicator stays hidden at each view's own default and appears the
moment `o'/`O' deviates, with zero new render code."
  (and (eq (org-air-view--sort-active-key)
           (if org-air-view--day 'time 'date))
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

;;;; ---------------------------------------------------------------------
;;;; Day-view sort (R79) — the SAME sort core, a day key vocabulary, and a
;;;; per-group dispatcher; the day↔board boundary swaps the key list and
;;;; coerces the active key (`org-air-view-day' / `org-air-view-board').
;;;; ---------------------------------------------------------------------

(defconst org-air-view--day-sort-keys '(time keyword priority title)
  "The single-day view's sort key vocabulary (R79).
Swapped into `org-air-view--sort-keys' at the day↔board boundary; default
`time' (chronological within each group).  `priority'/`title' are shared
with the board list and carry across the boundary unchanged.")

(defconst org-air-view--day-time-never
  (encode-time 0 0 0 1 1 9999)
  "Far-future sentinel time for stampless day items (R79).
A whole-day / stampless item sorts LAST under the `time' key by taking
this sentinel; `O' reverses the whole order, so it moves first there.")

(defun org-air-view--day-item-sort-time (group-label item)
  "Return ITEM's within-group planning time for GROUP-LABEL (R79 `time' sort).
The SAME signals `org-air-view--day-groups' keyed on: Deadline reads the
deadline slot, Scheduled the scheduled slot, Logged/created the live
marker stamp else the `subtree-ts' slot.  A whole-day / stampless item
returns `org-air-view--day-time-never' so it sorts last (stable).  Pure
over cached data; a (FILE . POS) item answers data-pure (no file open)."
  (or (pcase group-label
        ("Deadline" (org-air-view--timestamp-time (org-air-item-deadline item)))
        ("Scheduled" (org-air-view--timestamp-time (org-air-item-scheduled item)))
        (_ (or (org-air-view--marker-timestamp-time item)
               (let ((ts (org-air-item-subtree-ts item)))
                 (and ts (seconds-to-time ts))))))
      org-air-view--day-time-never))

(defun org-air-view--sort-day-items (group-label items)
  "Order a day GROUP-LABEL's ITEMS by the active day sort key/direction (R79).
Dispatches the shared `org-air-view--sort-active-key' through the R22-3
`org-air-view--sort-by' core — no fork:
  `time'     the group's own planning time-of-day, earliest first,
             stampless last;
  `keyword'  `org-air-item-todo' (case-insensitive), clustering e.g. all
             COMP before all DROPPED;
  `priority' `org-air-view--item-priority-rank' (#A first);
  `title'    the downcased title.
`O' reverses via the DESC arg.  Groups themselves are NEVER reordered
\(Deadline > Scheduled > Logged/created); only the items inside each."
  (let ((desc (eq (org-air-view--sort-active-direction) 'descending)))
    (pcase (org-air-view--sort-active-key)
      ('time
       (org-air-view--sort-by
        items #'time-less-p
        (lambda (it) (org-air-view--day-item-sort-time group-label it))
        desc))
      ('keyword
       (org-air-view--sort-by
        items #'string-lessp
        (lambda (it) (downcase (or (org-air-item-todo it) "")))
        desc))
      ('priority
       (org-air-view--sort-by items #'> #'org-air-view--item-priority-rank desc))
      ('title
       (org-air-view--sort-by items #'string-lessp
                              (lambda (it) (downcase (or (org-air-item-title it) "")))
                              desc))
      (_ items))))

(defun org-air-view--enter-day-sort ()
  "Swap the sort key vocabulary to the day view's on ENTERING it (R79).
Sets `org-air-view--sort-keys' to `org-air-view--day-sort-keys' and
coerces the active key: keep it when it is a day key (so the shared
`priority'/`title' carry across the board→day boundary), else default to
`time' (`date'→`time', both the chronological default).  Direction
carries unchanged.  Idempotent, so day nav (`<'/`>') re-entry is safe."
  (setq-local org-air-view--sort-keys org-air-view--day-sort-keys)
  (unless (memq (org-air-view--sort-active-key) org-air-view--day-sort-keys)
    (setq-local org-air-view--sort-key 'time)))

(defun org-air-view--leave-day-sort ()
  "Restore the board sort key vocabulary on LEAVING the day view (R79).
Sets `org-air-view--sort-keys' back to the board list and coerces the
active key: keep it when it is a board key (shared `priority'/`title'
survive), else `date' (day-only `time'/`keyword' hand back to the
chronological board default).  Direction carries unchanged."
  (setq-local org-air-view--sort-keys '(date priority title recency))
  (unless (memq (org-air-view--sort-active-key) org-air-view--sort-keys)
    (setq-local org-air-view--sort-key 'date)))

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
            (when (and (> count (length visible))
                       ;; R53/R90: collapsed Notes and Backlog are header-only
                       ;; and emit no fold row.  Expanded Notes may still have
                       ;; a capped preview; expanded Backlog shows all rows.
                       (or (not (memq bucket '(notes backlog)))
                           (org-air-view--section-expanded-p bucket)))
              ;; R51-3: the fold row is itself an actionable toggle target.
              ;; It carries `org-air-more-row' BUCKET over its FULL extent
              ;; (the dispatch handle — the board twin of the project's
              ;; `org-air-dropped-fold') and `mouse-face' over the text-only
              ;; label, so TAB/RET/mouse-1 ON the row expand the section
              ;; instead of drifting.  NO `org-air-item' — n/p item motion
              ;; and the inspector keep skipping the row by construction.
              ;; The label TEXT is byte-frozen (every board golden holds).
              (let ((start (point)))
                (insert (org-air-view--item-margin)
                        (propertize (format "%sand %d more — press TAB on the title to expand"
                                            (org-air-view--glyph 'more)
                                            (- count (length visible)))
                                    'face 'org-air-face-faded
                                    'mouse-face 'org-air-face-cursor)
                        (propertize "\n" 'face 'org-air-face-faded))
                (add-text-properties start (point)
                                     (list 'org-air-more-row bucket)))))
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
                      ;; R50-1: refresh is the SEQUENCE `g r' (`g' alone
                      ;; is the B4 prefix map) — the band must not teach a
                      ;; bare prefix.  Bracket-key idiom kept elsewhere.
                      (if (<= (org-air-view--render-width) 80)
                          "[c]apture [g r] refresh [/]filter [\\]clear [s]cope [TAB]next RET visit [?]help"
                        "[c]apture  [g r] refresh  [/]filter  [\\]clear  [s]cope  [TAB]next  RET visit  [?]help"))
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
        (marked-keys org-air-view--marked-keys)
        (marked-table org-air-view--marked-key-table)
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
        ;; R30-3: carry the buffer-local column toggles into the pane temp
        ;; buffer so the meta-width pass composes with the SAME hidden/
        ;; shown columns the user toggled (else it falls back to the
        ;; global defaults and the toggle appears to do nothing).
        (show-origin org-air-show-origin)
        (show-dates org-air-show-dates)
        (show-tags org-air-show-tags)
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
            (org-air-view--marked-keys marked-keys)
            (org-air-view--marked-key-table marked-table)
            (org-air-view--cal-month cal-month)
            (org-air-view--day day)
            (org-air-view--classify-cache classify-cache)
            (org-air-view--classify-cache-day classify-cache-day)
            (org-air-view--render-partition render-partition)
            (org-air-view--render-displayed render-displayed)
            (org-air-view--rail-descriptor rail-descriptor)
            (org-air-view--sort-key sort-key)
            (org-air-view--sort-direction sort-direction)
            (org-air-show-origin show-origin)
            (org-air-show-dates show-dates)
            (org-air-show-tags show-tags)
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

(defun org-air-view--summary-buckets (items)
  "Return the Summary section descriptors for visible ITEMS (R83).
The fixed five task/inbox sections, plus the conditional Backlog
descriptor when any visible item defers into the `backlog' bucket — so a
backlog-free board keeps the exact five Summary rows (byte-identical),
while a board with deferred items grows ONE `Backlog N' row.  Notes stay
OUT of the Summary, as before."
  (if (and (org-air-view--backlog-section-enabled-p)
           (org-air-view--items-for-bucket 'backlog items))
      (append org-air-view--sections (list org-air-view--backlog-descriptor))
    org-air-view--sections))

(defun org-air-view--section-counts (items)
  "Return bucket count alist for visible ITEMS.
Counts use `org-air-view--items-for-bucket' so the summary mirrors the
section badges and bodies exactly (S4) — inbox items are not also tallied
under the other buckets.  R83: the conditional `backlog' tally rides the
`org-air-view--summary-buckets' list."
  (mapcar (lambda (descriptor)
            (pcase-let ((`(,bucket ,_title ,_empty) descriptor))
              (cons bucket (length (org-air-view--items-for-bucket bucket items)))))
          (org-air-view--summary-buckets items)))

(defun org-air-view--bucket-title (bucket)
  "Return display title for BUCKET.
R83: `backlog' resolves to the conditional Backlog descriptor's title."
  (cadr (if (eq bucket 'backlog)
            org-air-view--backlog-descriptor
          (assq bucket org-air-view--sections))))

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
    ;; R69-5: prefix-deduped chip label (a literal `#nix' tag reads `#nix').
    (`(:tag ,tag) (org-air-view--tag-chip-label tag))
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
                    ;; R69-2: the trailing ✕ clear glyph is DROPPED — it
                    ;; carried no keymap/button action (a promise the rail
                    ;; could not honour); the `M-/ toggles ∙ \ clears' hint
                    ;; line below is the teaching surface and names BOTH
                    ;; verbs.  The glyph table entry stays (banner/project
                    ;; header still render it).
                    (mapconcat #'org-air-view--filter-token-label
                               filters
                               (if (> (length filters) 1)
                                   (concat " " (org-air-view--filter-combinator-word) " ")
                                 " "))
                    "\n")
            (insert inset
                    (propertize
                     (concat (format "Match: %s   M-/ toggles %s \\ clears"
                                     (org-air-view--filter-combinator-word)
                                     org-air-chrome-separator)
                             ;; R22-4: when the lens removed rows, report it.
                             (when (< shown loaded)
                               (format "   %d of %d shown" shown loaded)))
                     'face 'org-air-face-faded)
                    "\n"))
        ;; R22-4: empty filter reads `none' — the dataset is the Source's job.
        (insert inset (propertize "none" 'face 'org-air-face-faded) "\n"))
      ;; R69-1: one blank line between the Filter and Source sections —
      ;; the SAME inter-section spacer `--insert-rail-1' emits between
      ;; every other rail section pair (Calendar/Filter/Summary/…); the
      ;; foot arithmetic absorbs the extra line (the breathing tail
      ;; shrinks by one, R26-3 clamp rules unchanged).
      (insert "\n")
      ;; R22-4: the SOURCE/DATASET selector, named + counted, on its own
      ;; labelled line; the dataset name rides the readable origin face so
      ;; it reads as a dataset chip, NOT a faded second filter.
      (org-air-view--rail-header "Source" width)
      (insert inset
              (propertize (org-air-view--scope-label) 'face 'org-air-face-origin)
              (propertize (format " %s %d loaded" org-air-chrome-separator loaded)
                          'face 'org-air-face-faded)
              (if org-air-view--scope
                  (propertize (format "   s changes %s S clears" org-air-chrome-separator)
                              'face 'org-air-face-faded)
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

(defun org-air-view--insert-verb-rows (cells width)
  "Insert Actions verb CELLS as column-aligned rows fitted to WIDTH (R69-4).
CELLS is the flat ordered list of (KEY . DESC) conses in reading order.
The ONE shared row emitter for every rail Actions block (board, project,
review, revisit).  It picks the LARGEST column count n in {3, 2, 1} whose
grid FITS WIDTH: cells chunk in order into rows of n, each column is
sized to its widest DERIVED cell (`string-width' over \"KEY DESC\" — the
R50-1 rule: a leader/evil rebind changes the arithmetic, and when needed
the column COUNT, automatically), and the fit charges the spine inset,
the tier gap (4 columns at WIDTH >= 38, else 1) and EVERY column
including the unpadded last one (the fit is judged on the widest possible
row).  When 3 columns fit the output is byte-identical to the historical
3-column emitters BY CONSTRUCTION (same column maxima, same gap, same
unpadded-last-column rule); at narrower widths the block REFLOWS to more
rows instead of truncating — no verb dropped, no label shortened.  Floor:
at n=1 a single cell wider than WIDTH still ellipsizes via `--pad-to'
\(the documented last resort; needs a pathological rebinding — the widest
fallback cell is 11 cols, safe at every rail tier)."
  (when cells
    (let* ((inset (org-air-view--rail-inset-str width))
           (gap (if (>= width 38) "    " " "))
           (cellw (lambda (cell)
                    (+ (string-width (car cell)) 1 (string-width (cdr cell)))))
           (chunk (lambda (n)
                    (let ((rest cells) rows)
                      (while rest
                        (push (seq-take rest n) rows)
                        (setq rest (nthcdr n rest)))
                      (nreverse rows))))
           (colws (lambda (rows n)
                    (mapcar (lambda (j)
                              (apply #'max 0
                                     (mapcar (lambda (row)
                                               (if (nth j row)
                                                   (funcall cellw (nth j row))
                                                 0))
                                             rows)))
                            (number-sequence 0 (1- n)))))
           (fits (lambda (n)
                   (<= (+ (string-width inset)
                          (apply #'+ (funcall colws (funcall chunk n) n))
                          (* (string-width gap) (1- n)))
                       width)))
           (n (cond ((funcall fits 3) 3)
                    ((funcall fits 2) 2)
                    (t 1)))
           (rows (funcall chunk n))
           (ws (funcall colws rows n)))
      (dolist (row rows)
        (let ((line inset)
              (last (1- (length row)))
              (j 0))
          (dolist (cell row)
            (setq line (concat line
                               (if (zerop j) "" gap)
                               (org-air-view--verb-cell
                                (car cell) (cdr cell)
                                ;; The row's LAST cell emits unpadded (its
                                ;; colw still counted in the fit test).
                                (if (= j last) 0 (nth j ws))))
                  j (1+ j)))
          (insert (org-air-view--pad-to line width) "\n"))))))

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
  "Insert the BOARD Actions block fitted to rail content WIDTH.
R50-1: every cell's KEY text is DERIVED from the LIVE binding in the
BOARD buffer through the one shared seam `org-air-view--legend-key'
\(`where-is' in the buffer where these keys actually fire — the legend may
render in the rail side window, whose own map does not carry the board
verbs), never a hardcoded string.  The refresh cell therefore shows the
TRUE sequence `g r' (`g' alone is the B4 prefix map — pressed alone it
only waits for a second key), and the legend follows
`org-air-use-default-keybindings' (knob off -> the fallbacks keep the
legend readable), a user `define-key' rebinding, an `org-air-leader-key'
move, and evil (the R29-2 overriding map makes `where-is' return the
sequences evil actually dispatches).  Column widths are computed from the
DERIVED cell strings, so the layout follows automatically at every rail
tier (wide/mid/narrow gap rules unchanged).  R69-4: the rows emit through
the shared fit-driven `org-air-view--insert-verb-rows' (3→2→1 columns),
so a narrow rail REFLOWS instead of truncating a verb."
  (org-air-view--rail-header
   "Actions" width
   :suffix (org-air-view--marked-count-label org-air-view--items)
   :suffix-face 'org-air-face-count)
  (let* ((board (get-buffer org-air-view-buffer-name))
         (key (lambda (command fallback)
                (org-air-view--legend-key command board fallback)))
         ;; Round-9 Q1: when a scope is active the second row's middle verb
         ;; surfaces the scope reset (the literal "S reset" cue the design
         ;; and grind ask for) right where the user acts.
         (mid2 (if org-air-view--scope
                   (cons (funcall key #'org-air-scope-clear "S") "reset")
                 (cons (funcall key #'org-air-toggle-section "TAB")
                       "expand")))
         ;; R22-4: `source' (was `scope') — the dataset selector.
         (row1 (list (cons (funcall key #'org-air-capture "c") "capture")
                     (cons (funcall key #'org-air-filter "/") "filter")
                     (cons (funcall key #'org-air-scope "s") "source")))
         ;; The `g r' fallback is the TRUE sequence even when the board
         ;; buffer is dead — a legend key must never be a bare prefix.
         (row2 (list (cons (funcall key #'org-air-refresh "g r") "refresh")
                     mid2
                     (cons (funcall key #'org-air-help "?") "help")))
         (bulk (and org-air-view--marked-keys
                    (list (cons (funcall key #'org-air-item-backlog "b")
                                "backlog all")
                          (cons (funcall key #'org-air-set-tag "t")
                                "tag all")
                          (cons (funcall key #'org-air-clear-marks "M")
                                "clear marks")))))
    ;; R69-4: the shared fit-driven emitter (column widths from the
    ;; DERIVED cells via `string-width'; 3-col byte-identical when it
    ;; fits, else reflow to 2 then 1 columns — never truncate a verb).
    (org-air-view--insert-verb-rows (append row1 row2 bulk) width)))

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

(defcustom org-air-inspector-max-title-lines nil
  "Cap on wrapped title lines in the inspector, or nil = wrap fully (R30-1).
nil (the DEFAULT, R30-1) wraps the WHOLE title with NO truncation — the
title line never carries a mid-word more glyph; the reserved mid-rail
region still bounds the total inspector height, so a pathological title
simply consumes more of the region (the breathing tail shrinks first).
A positive integer caps the title at that many wrapped lines with a
trailing more glyph (the back-compat knob; D-P1 default was 4)."
  :type '(choice (const :tag "wrap fully" nil) integer)
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
  "Return TITLE word-wrapped to WIDTH (R30-1).
MAXLINES nil = NO cap: the full wrapped title, so the title never carries
a mid-word more glyph.  A positive integer caps at MAXLINES with a
trailing more glyph (the back-compat knob)."
  (let ((lines (org-air-view--word-wrap title width)))
    (if (or (null maxlines) (<= (length lines) maxlines))
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
the inspector reads the same CREATED for a cache-painted item.  R53: the
hydration lives HERE (one file, for the single inspected item — bounded
and user-driven), not in the per-item classify path, which is data-pure."
  (let* ((m (org-air-item-marker item))
         (src (cond ((and (markerp m) (marker-buffer m))
                     (cons (marker-buffer m) (marker-position m)))
                    ((and (consp m) (stringp (car m))
                          (ignore-errors (file-readable-p (car m))))
                     ;; NOWARN: a background probe must NEVER prompt
                     ;; ("changed on disk; reread?" reads stdin in batch
                     ;; and modals interactively); a stale live buffer's
                     ;; CREATED is fine for the inspector.
                     (cons (find-file-noselect (car m) t)
                           (or (cdr m) 1))))))
    (when src
      (ignore-errors
        (with-current-buffer (car src)
          (save-excursion
            (goto-char (cdr src))
            (when-let* ((v (org-entry-get (point) "CREATED")))
              (org-air-view--timestamp-time
               (org-timestamp-from-string v)))))))))

(defun org-air-view--item-updated (item)
  "Return ITEM's last activity as (EPOCH . SOURCE), or nil (R74-1).
Pure and I/O-free — three cached R61 slot reads, no file access, no
marker hydration.  Candidates: the newest LOGBOOK stamp (`logs' head;
SOURCE from its KIND — nil -> `note', `done' -> `done', `todo' ->
`state' — the CLASS the scan cached, never a guessed keyword), the
newest clock's END (`clocks' head's cdr, O(1); SOURCE `clock') and the
`created' slot (SOURCE `created' — the floor: an item never touched
since capture was last updated when it was created).  The winner is the
strict MAX; ties resolve logs > clocks > created (replace only on
strictly greater), so an equal-second note outranks a clock-out with
the more descriptive label.  Cap-invariant: `org-air-log-cap'
truncation keeps the NEWEST entries, so a `rtrunc' item's heads are
exactly the untruncated heads.  All three slots empty -> nil (the
Decision 3 fallback takes over in `org-air-view--item-updated-line')."
  (let* ((log (car (org-air-item-logs item)))
         (clock-end (cdr (car (org-air-item-clocks item))))
         (created (org-air-item-created item))
         (best (when log
                 (cons (car log)
                       (pcase (cdr log)
                         ('done 'done)
                         ('todo 'state)
                         (_ 'note))))))
    (when (and clock-end (or (null best) (> clock-end (car best))))
      (setq best (cons clock-end 'clock)))
    (when (and created (or (null best) (> created (car best))))
      (setq best (cons created 'created)))
    best))

(defun org-air-view--item-updated-line (item inset now)
  "Return the inspector Updated KV line for ITEM at INSET, or nil (R74).
Slot path first: `org-air-view--item-updated' (pure, zero I/O) supplies
the epoch and the class label.  When ALL three R61 slots are empty (no
LOGBOOK, no closed clock, no `:CREATED:' — including items built
outside the scan), ONE bounded `file-attributes' stat on the item's
file supplies a last-modified time labelled \"~file\" (file-level, not
heading-precise — one uniform rule, `kind' `file' items included).
Always-on and FUTURE-CLAMPED: a fallback mtime AFTER the render clock
NOW renders NOTHING (clock skew, NFS drift, a restored backup — an
untrustworthy signal; the clamp is also exactly what keeps the
frozen-clock goldens byte-clean, their fixture mtimes post-dating the
frozen NOW).  The SLOT path is NOT clamped — a forged future LOGBOOK
stamp renders honestly as \"(in Nd · note)\".  A nil, missing or
unreadable file degrades to nil — no line, no signal (R53).  The stat
is reachable only from HERE (the inspector line render): one item, once
per debounced render, never in the classify/paint loop."
  (let* ((slot (org-air-view--item-updated item))
         (time (car slot))
         (label (pcase (cdr slot)
                  ('note "note") ('done "done") ('state "state")
                  ('clock "clock") ('created "created"))))
    (unless slot
      ;; Decision 3: ONE bounded live stat, marked, future-clamped.
      (let* ((file (org-air-item-file item))
             (mtime (and (stringp file)
                         (ignore-errors
                           (file-attribute-modification-time
                            (file-attributes file))))))
        (when (and mtime (not (time-less-p now mtime)))
          (setq time mtime
                label "~file"))))
    (when time
      (org-air-view--inspector-kv
       "Updated"
       (concat (propertize (format-time-string "%F" time)
                           'face 'org-air-face-faded)
               "  "
               (propertize
                (format "(%s · %s)"
                        (org-air-view--inspector-relative time now)
                        label)
                'face 'org-air-face-faded))
       inset))))

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
The lines AFTER the `Inspector' header, R30-1 identity block first:
title / state+priority / tags / breathing / origin / dates / repeat /
bucket.  INSET is the spine prefix,
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
                                            (org-air-view--todo-face
                                             todo (org-air-item-donep item))))
                              (when prio
                                (org-air-view--priority-token prio))))))
      (when parts (push (concat inset (string-join parts "  ")) lines)))
    ;; R30-1 identity block: tags move UP to sit directly under
    ;; title+state — the row's IDENTITY — ABOVE the breathing blank and
    ;; the metadata KV rows (origin/dates).  tags (all, accent, wrapped).
    (let ((tagstr (mapconcat
                   ;; R69-5: prefix-deduped chip label; the face keeps
                   ;; hashing the RAW tag name (`#Nix' and `Nix' ARE
                   ;; different tags and may carry different accents).
                   (lambda (tg) (propertize (org-air-view--tag-chip-label tg)
                                            'face (org-air-faces-tag-face tg)))
                   (org-air-item-tags item) " ")))
      (unless (string-empty-p tagstr)
        (dolist (tl (org-air-view--word-wrap tagstr content-w))
          (push (concat inset tl) lines))))
    ;; R30-1 breathing: a blank line separates the title/state/tags
    ;; identity block from the metadata KV rows (origin/dates).
    (push "" lines)
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
                         ;; R74: last activity from the cached R61 slot
                         ;; heads (else the bounded ~file fallback),
                         ;; directly AFTER Created — and rendered whether
                         ;; or not Created did (the fallback case is
                         ;; precisely a Created-less item).
                         (org-air-view--item-updated-line item inset now)
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
                         ;; new rail cell.  R38-2: the composed width comes
                         ;; from the CACHED geom; if the window narrowed
                         ;; inside the resize debounce that width overhangs
                         ;; the live text area as pure trailing whitespace +
                         ;; a truncation arrow.  Route through
                         ;; `org-air-view--postprocess-line' against the
                         ;; LIVE render width so the refill never emits
                         ;; whitespace past usable.  In the batch/fixed-
                         ;; width path this pads to the seam exactly as
                         ;; before, so the width-composition byte gate holds.
                         (item-part (org-air-view--pad-to
                                     (truncate-string-to-width cur iw) iw))
                         (new (org-air-view--postprocess-line
                               (concat item-part div cell)
                               (org-air-view--render-width))))
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
                       (insert (propertize (format "c capture %s / filter" org-air-chrome-separator)
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
  "Return ((LABEL . ITEMS)...) grouping ITEMS by relation to DAY (R6).
R53fix B1: the Logged/created key is the live-marker subtree probe when
the item still carries a live marker, else the scan-time `subtree-ts'
slot — a (FILE . POS) cons item answers data-pure.  NEVER the `activity'
slot: its mtime fallback would wrongly file every undated heading of a
today-touched file under Logged/created.
R59: pure CONTAINER headings are skipped (`org-air-query-container-
item-p') — the child that actually carries the stamp still files under
Logged/created; the structural parent no longer duplicates it."
  (let ((key (org-air-view--day-key day))
        (deadline nil) (scheduled nil) (created nil))
    (dolist (item (org-air-view--visible-items items))
      ;; R59: a pure CONTAINER heading is structure, not an item — it
      ;; INHERITS its children's `:CREATED:' stamps through the
      ;; subtree-wide `subtree-ts' probe and would duplicate the child
      ;; under Logged/created.  One uniform filter (Deadline/Scheduled
      ;; key on OWN planning slots a container by definition lacks, but
      ;; the guard is uniform anyway); knob-gated inside the predicate.
      (unless (org-air-query-container-item-p item)
        (let ((d (org-air-view--timestamp-time (org-air-item-deadline item)))
              (s (org-air-view--timestamp-time (org-air-item-scheduled item)))
              (a (or (org-air-view--marker-timestamp-time item)
                     (org-air-item-subtree-ts item))))
          (cond
           ((and d (equal (org-air-view--day-key d) key)) (push item deadline))
           ((and s (equal (org-air-view--day-key s) key)) (push item scheduled))
           ((and a (equal (org-air-view--day-key a) key)) (push item created))))))
    (list (cons "Deadline" (nreverse deadline))
          (cons "Scheduled" (nreverse scheduled))
          (cons "Logged / created" (nreverse created)))))

(defun org-air-view--day-meta-widths (groups width)
  "Return (TODO-W TAGS-W ORIGIN-W) for the day pane over GROUPS at WIDTH (R79).
The widest SHOWN keyword / tags / origin across ALL day groups' items —
the board's `org-air-view--compute-meta-widths' rule applied to the day
list, minus the date column (R6).  The origin is capped at
`org-air-origin-max-width' and a title-min fit pass shrinks origin then
tags toward their floors so the flex title keeps `org-air-title-min-width'
columns, exactly like the board.  Order-independent (measures the SET),
so it may be called before the per-group sort."
  (let ((tw 0) (ow 0) (tw-todo 0))
    (dolist (g groups)
      (dolist (item (cdr g))
        (setq tw-todo (max tw-todo
                           (string-width (or (org-air-item-todo item) ""))))
        (let* ((tags (org-air-item-tags item))
               (n (length tags))
               (ts (org-air-view--item-tagstr
                    tags (min org-air-tags-inline-max n) n)))
          (when org-air-show-tags
            (setq tw (max tw (string-width ts))))
          (when org-air-show-origin
            (setq ow (max ow (string-width
                              (org-air-view--item-origin-raw item))))))))
    ;; R80: mirror the board keyword-column floor so a short keyword
    ;; (OUT/OFF) day badge is DRAFT-sized, not a tiny pill.
    (when (and (eq org-air-keyword-style 'badge) (> tw-todo 0))
      (setq tw-todo (max tw-todo org-air-keyword-badge-min-cols)))
    (setq ow (min ow org-air-origin-max-width))
    ;; title-min fit pass (no date column; mirrors --compute-meta-widths).
    (let* ((gap 2)
           (left-reserve (+ (string-width (org-air-view--item-margin))
                            (if (> tw-todo 0) (1+ tw-todo) 0)
                            (if (eq org-air-priority-style 'square) 2 0)))
           (cluster (lambda (o)
                      (let ((cells (delq nil (list (and (> tw 0) tw)
                                                   (and (> o  0) o)))))
                        (+ (apply #'+ cells) (max 0 (1- (length cells)))))))
           (budget (lambda (o) (- width left-reserve gap (funcall cluster o)))))
      (while (and (> ow org-air-origin-min)
                  (< (funcall budget ow) org-air-title-min-width))
        (setq ow (1- ow)))
      (let ((tw-floor (if (> tw 0) 1 0)))
        (while (and (> tw tw-floor)
                    (< (funcall budget ow) org-air-title-min-width))
          (setq tw (1- tw)))))
    (list tw-todo tw ow)))

(defun org-air-view--insert-day-pane (items width)
  "Insert the single-day focus view (R6) of ITEMS, fitted to WIDTH.
The day is `org-air-view--day'; its items are grouped Deadline >
Scheduled > Logged/created and rendered with the R10 item line minus its
now-redundant date.
R79: the meta-badge/tags/origin COLUMNS are sized to the widest SHOWN
value across the whole day (`org-air-view--day-meta-widths', the board
rule) so mixed-width keywords like COMP/DROPPED align their titles / dots
\/ tags / origin like board rows; and each group's items are ordered by
the shared sort core (`org-air-view--sort-day-items') so `o'/`O' act."
  (let* ((org-air-view--line-width width)
         ;; Day view omits the date column (R6).
         (org-air-view--meta-date-w nil)
         (day org-air-view--day)
         (groups (org-air-view--day-groups items day))
         ;; R79 D2: fix the badge/tags/origin columns to the day's widest
         ;; SHOWN value so titles align across mixed-width keywords — drop
         ;; the stale R15 single-row nil assumption (49 rows is not one).
         (dw (org-air-view--day-meta-widths groups width))
         (org-air-view--meta-todo-w (nth 0 dw))
         (org-air-view--meta-tags-w (nth 1 dw))
         (org-air-view--meta-origin-w (nth 2 dw))
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
        ;; R79 D4: order each group through the shared sort core before the
        ;; row loop (never reorder the Deadline > Scheduled > Logged order).
        (dolist (item (org-air-view--sort-day-items (car g) (cdr g)))
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
    (dolist (descriptor (org-air-view--section-descriptors items))
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

(defun org-air-view--pane-divider-line-p (line col)
  "Return non-nil if LINE carries the pane divider at display column COL.
The divider is the `org-air-face-pane-border'-faced vrule glyph the
two-pane composer (`org-air-view--compose-columns' /
`org-air-view--two-pane-body' fill-row) emits at COL on EVERY board row.
Returns nil when COL is nil (board-only / stacked / side-window: no pane
divider), so those orientations keep the plain trim path."
  (and col
       (let ((glyph (string-to-char (org-air-view--glyph 'vrule)))
             (i 0) (w 0) (len (length line)) (found nil))
         (while (and (not found) (<= w col) (< i len))
           (let ((ch (aref line i)))
             (when (and (= w col)
                        (eq ch glyph)
                        (let ((f (get-text-property i 'face line)))
                          (or (eq f 'org-air-face-pane-border)
                              (and (listp f)
                                   (memq 'org-air-face-pane-border f)))))
               (setq found t))
             (setq w (+ w (char-width ch)) i (1+ i))))
         found)))

(defun org-air-view--pane-divider-glyph-index (line col)
  "Return the STRING index of the faced pane-divider vrule at display COL in LINE.
Mirrors `org-air-view--pane-divider-line-p' but returns the index of the
`org-air-face-pane-border'-faced vrule glyph (nil when absent), so the
divider can be pinned relative to it (R44-2, Fix A)."
  (and col
       (let ((glyph (string-to-char (org-air-view--glyph 'vrule)))
             (i 0) (w 0) (len (length line)) (found nil))
         (while (and (not found) (<= w col) (< i len))
           (let ((ch (aref line i)))
             (when (and (= w col)
                        (eq ch glyph)
                        (let ((f (get-text-property i 'face line)))
                          (or (eq f 'org-air-face-pane-border)
                              (and (listp f)
                                   (memq 'org-air-face-pane-border f)))))
               (setq found i))
             (setq w (+ w (char-width ch)) i (1+ i))))
         found)))

(defun org-air-view--pin-pane-divider (line col)
  "Pin the pane divider at display COL to an exact pixel-stop (R44-2 Fix A).
Attaches `display (space :align-to (COL . width))' to the SINGLE leading
space immediately BEFORE the faced vrule so the `│' glyph lands at the
same font-relative pixel-X on every board row — straightening the rule even
if any residual pill sub-pixel drift remains.  `(COL . width)' measures COL
in units of the buffer default face's char advance (the UNIT the divider
column and plain text advance at), so the align-to stop always lies
AT-OR-AHEAD of the (Fix-B, exactly-ncols-cells) board run — it only ever
pads FORWARD and can NEVER collapse behind a drawn glyph.  Applied only on
a graphical display (a TTY has no pixel drift); returns LINE unchanged
otherwise or when the leading divider space is not found."
  (if (not (display-graphic-p))
      line
    (let ((idx (org-air-view--pane-divider-glyph-index line col)))
      (if (and idx (> idx 0) (eq (aref line (1- idx)) ?\s))
          (let ((out (copy-sequence line)))
            (put-text-property (1- idx) idx
                               'display (list 'space :align-to (cons col 'width))
                               out)
            out)
        line))))

(defun org-air-view--finalize-buffer-lines (width &optional divider-col)
  "Cap each line at WIDTH and strip trailing whitespace (D7/D6, live mode).
No line may exceed the window actually displaying the dashboard so the
rail/calendar are never pushed off-screen (D7); full-width and stacked
rows carry no trailing whitespace (D6).

R43-2: when DIVIDER-COL is non-nil (two-pane) any line carrying the pane
divider at that column is PADDED to WIDTH instead of trimmed, so the
blank rail tail is preserved and the divider stays an INTERIOR cell on
every board row — byte-identical to the normalize/golden shape.  Trimming
that tail would demote the divider to the row's TERMINAL glyph and break
the rule into segments on every blank-rail row (board ≫ rail).  Header
banner, header rule and footer carry no divider, so they keep the R36-1 /
R37 no-trailing-pad contract; board-only / stacked / side-window pass
DIVIDER-COL nil and are untouched."
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
             (result (if (org-air-view--pane-divider-line-p capped divider-col)
                         ;; R44-2 Fix A: pad the blank rail tail (R43-2
                         ;; interior divider) THEN pin the divider column to
                         ;; an exact pixel-stop with `:align-to' so it lands
                         ;; at ONE pixel-X on every board row.
                         (org-air-view--pin-pane-divider
                          (org-air-view--pad-to capped width) divider-col)
                       (string-trim-right capped))))
        ;; R44-2: compare INCLUDING text properties so a divider row whose
        ;; chars are already full width but now carries the `:align-to' pin
        ;; is still rewritten (a plain `string='/`equal' ignores the added
        ;; display property and would drop the pin).
        (unless (equal-including-properties result line)
          (delete-region beg end)
          (insert result)))
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

(defun org-air-view--row-pane-limit (bol eol)
  "Return the exclusive END of the current row's BOARD segment (R46-2).
On a two-pane line (`org-air-view--pane-divider-col' set) the board
segment ends at the `org-air-face-pane-border'-faced vrule glyph the
composer put on the line; every other orientation (board-only, stacked,
side-window) — and any line not carrying the faced vrule (banner, rule,
footer) — uses EOL.  BOL/EOL bound the scan.  Keeps the R46-2 title band
on the BOARD side of the divider, so a two-pane fill row (blank board,
live rail) reads as genuinely blank."
  (or (and org-air-view--pane-divider-col
           (let ((pos bol) (found nil))
             (while (and (not found) (< pos eol))
               (let ((f (get-text-property pos 'face)))
                 (if (or (eq f 'org-air-face-pane-border)
                         (and (listp f) (memq 'org-air-face-pane-border f)))
                     (setq found pos)
                   (setq pos (1+ pos)))))
             found))
      eol))

(defun org-air-view--row-band ()
  "Return the current row's TITLE BAND as (START . END), or nil (R46-2).
START/END are buffer positions on the current line; point anywhere in
[START..END] is a legitimate landing for a line-crossing motion.
Supersedes the R29-2 `dead-zone' predicate, which fired only on item/doc
rows and guarded only the LEFT edge — so every non-item row (section
headers, the `…and N more' / empty-section notes, banner) and the RIGHT
edge of item rows were unguarded and vertical motion dropped point to
column 0 or EOL (the R46 cursor jump).

- Item / doc rows (`org-air-item'/`org-air-doc' anywhere on the line,
  via `org-air-view--row-property'): START is the R21-2 title
  (`org-air-view--row-title-pos'); END is the row's LAST visible
  (non-space) glyph within the row's own property run — the board
  content, before the trailing pad / two-pane rail.
- Every other visible row (section headers, the truncation/empty-section
  notes, the banner, the header rule): START = END = the row's first
  visible glyph on the BOARD side of the divider
  (`org-air-view--row-pane-limit'), so point rides the row's own text,
  never the col-0 indent margin.
- Genuinely blank rows (spacers, two-pane fill rows): nil — column 0 is
  the only legitimate landing; never touched."
  (let* ((bol (line-beginning-position))
         (eol (line-end-position)))
    (if (or (org-air-view--row-property 'org-air-item)
            (org-air-view--row-property 'org-air-doc))
        (let* ((start (org-air-view--row-title-pos))
               ;; End of the row's own item/doc property run: the trailing
               ;; pad may still carry the property (it covers the whole
               ;; board run), but the rail columns never do (R22-2).
               (run-end (let ((pos eol))
                          (while (and (> pos bol)
                                      (not (get-text-property
                                            (1- pos) 'org-air-item))
                                      (not (get-text-property
                                            (1- pos) 'org-air-doc)))
                            (setq pos (1- pos)))
                          pos))
               ;; ...trimmed to the run's last visible glyph.
               (end (save-excursion
                      (goto-char run-end)
                      (skip-chars-backward " \t" bol)
                      (point))))
          (cons start (max start (1- end))))
      (let* ((limit (org-air-view--row-pane-limit bol eol))
             (first (save-excursion
                      (goto-char bol)
                      (skip-chars-forward " \t" limit)
                      (point))))
        (when (< first limit)
          (cons first first))))))

(defun org-air-view--normalize-point-now ()
  "Clamp point into the current row's TITLE BAND (R46-2).
The gate-free clamp: entry/restore tails call this DIRECTLY after placing
point (the R21-1 restore tail, the pane return, the doc-session return)
so a stray column is corrected immediately, not on the next keystroke.
Universal (EVERY board row) and two-edged: a col-0/gutter landing snaps
forward to the band start (subsuming the R21-2/R29-2 title snap — the
band start IS the title), a trailing-pad/EOL landing snaps BACK to the
band end, and a point already INSIDE the band is KEPT, so the goal
column of `next-line'/`evil-next-line' is respected.  Blank rows have no
band and are never touched (col 0 is their only legitimate landing).
Idempotent on/inside the band, so it composes with R21-1's restored
column."
  (when (and (not (window-minibuffer-p))
             (memq major-mode '(org-air-view-mode org-air-project-mode)))
    (let ((band (org-air-view--row-band)))
      (cond ((null band) nil)
            ((< (point) (car band)) (goto-char (car band)))
            ((> (point) (cdr band)) (goto-char (cdr band)))))))

(defun org-air-view--normalize-point ()
  "Clamp point into the row's title band after a LINE-crossing command.
Runs in `post-command-hook' (R29-2 gate, R46-2 clamp).  Command-agnostic
by construction (there is no command whitelist): the buffer-local
snapshot recorded by
`org-air-view--pre-command-snapshot' gates the clamp on LINE MOTION — it
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

;;;; =====================================================================
;;;; R30-4 — org-air-outline-mode: a generic, opt-in outline rail for ANY
;;;; org buffer, reusing the SAME rail descriptor seam + current-heading
;;;; highlight the doc session uses, with NO org-air-project dependency.
;;;; The two primitives below are the extracted generic core (the project
;;;; keeps thin wrappers over them, byte-identical outputs).
;;;; =====================================================================

(defun org-air-outline--headings (buffer)
  "Return BUFFER's Org outline as a list of (LEVEL TITLE POS) (R30-4).
A pure `^\\*+[ \\t]+' heading scan — no Air struct, no project, no airctl.
Relocated from `org-air-project--doc-outline' (which keeps a thin alias)."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let (rows)
          (while (re-search-forward "^\\(\\*+\\)[ \t]+\\(.*\\)$" nil t)
            (push (list (length (match-string 1))
                        (string-trim (match-string-no-properties 2))
                        (match-beginning 0))
                  rows))
          (nreverse rows))))))

(defvar org-air-rail--outline-overlay nil
  "The ONE current-heading overlay in the rail outline (R28-4/R30-4).
Overlay-only: overlays are not buffer text, so the rail byte goldens
\(`buffer-substring' reads) never move.  Deleted (no highlight) on any
error — graceful degrade, never flicker.  Shared by the doc session and
the generic `org-air-outline-mode' (only one rail exists at a time).")

(defun org-air-rail--outline-highlight-clear ()
  "Delete the rail-outline overlay (the no-highlight degrade) (R28-4)."
  (when (overlayp org-air-rail--outline-overlay)
    (delete-overlay org-air-rail--outline-overlay)))

(defun org-air-outline--highlight-update (source-buf rail-buf)
  "Move the single current-heading overlay in RAIL-BUF for point in SOURCE-BUF.
The generic R28-4 core (Air-free): the current heading is the LAST rail
outline row whose `org-air-doc-heading-pos' is <= point in SOURCE-BUF (a
linear scan over the rail's few rows — no Org re-parse).  Paints by
`move-overlay' of the single overlay — NO re-render, NO buffer text
change.  Wrapped in `condition-case': on ANY error the overlay is deleted
— no highlight, never a flicker or a message."
  (condition-case nil
      (let ((pt (and (buffer-live-p source-buf)
                     (with-current-buffer source-buf (point)))))
        (if (not (and pt (buffer-live-p rail-buf)))
            (org-air-rail--outline-highlight-clear)
          (with-current-buffer rail-buf
            (let (row-bol row-eol)
              (save-excursion
                (goto-char (point-min))
                (while (not (eobp))
                  (let ((hp (get-text-property
                             (point) 'org-air-doc-heading-pos)))
                    (when (and hp (<= hp pt))
                      (setq row-bol (line-beginning-position)
                            row-eol (line-end-position))))
                  (forward-line 1)))
              (if (null row-bol)
                  (org-air-rail--outline-highlight-clear)
                (if (overlayp org-air-rail--outline-overlay)
                    (move-overlay org-air-rail--outline-overlay
                                  row-bol row-eol rail-buf)
                  (setq org-air-rail--outline-overlay
                        (make-overlay row-bol row-eol rail-buf))
                  (overlay-put org-air-rail--outline-overlay
                               'face 'org-air-face-outline-current)
                  (overlay-put org-air-rail--outline-overlay
                               'evaporate t)))))))
    (error (org-air-rail--outline-highlight-clear))))

(defcustom org-air-outline-rail-placement nil
  "Where `org-air-outline-mode' hosts its outline rail (R30-4/R49-2).
OUTLINE override for `org-air-rail-placement': nil (the default) inherits
the shared knob; `side-window' pops the rail into a native side window
regardless of it.  Resolved through `org-air-rail--placement'."
  :type '(choice (const :tag "Inherit `org-air-rail-placement'" nil)
                 (const :tag "side window" side-window)
                 (const :tag "inline" inline))
  :group 'org-air)

(defun org-air-outline--buffer-title (buffer)
  "Return BUFFER's `#+title:' value, or its buffer name (R30-4)."
  (with-current-buffer buffer
    (or (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (when (re-search-forward "^#\\+title:[ \t]*\\(.*\\)$" nil t)
              (let ((s (string-trim (match-string-no-properties 1))))
                (unless (string-empty-p s) s)))))
        (buffer-name buffer))))

(defun org-air-outline--insert-context (source-buf width)
  "Insert the generic outline rail body for SOURCE-BUF at WIDTH (R30-4).
The buffer `#+title:' (or name) as the meta line — NO state badge — then
the Outline: one row per heading, indented by level, each carrying
`org-air-doc-heading-pos' so RET in the rail jumps the main window there."
  (let ((inset (org-air-view--rail-inset-str width)))
    (org-air-view--rail-header "Document" width)
    (insert (org-air-view--pad-to
             (concat inset (propertize (org-air-outline--buffer-title source-buf)
                                       'face 'org-air-face-title))
             width)
            "\n")
    (insert "\n")
    (org-air-view--rail-header "Outline" width)
    (let ((rows (org-air-outline--headings source-buf)))
      (if (null rows)
          (insert (org-air-view--pad-to
                   (concat inset (propertize "no headings"
                                             'face 'org-air-face-faded))
                   width)
                  "\n")
        (pcase-dolist (`(,level ,title ,pos) rows)
          (insert (propertize
                   (org-air-view--pad-to
                    (concat inset (make-string (* 2 (1- level)) ?\s) title)
                    width)
                   'org-air-doc-heading-pos pos)
                  "\n"))))))

(defun org-air-outline--insert-actions (width _source-buf)
  "Insert the generic outline rail Actions legend at WIDTH (R30-4).
The reachable rail keys, derived (R30-2 `org-air-view--legend-key') from
the rail buffer where the legend lives: `RET jump' and `| rail'."
  (org-air-view--rail-header "Actions" width)
  (let* ((inset (org-air-view--rail-inset-str width))
         (gap (if (>= width 38) "    " " "))
         (rail (get-buffer org-air-rail-buffer-name))
         (cells (list (org-air-view--verb-cell
                       (org-air-view--legend-key #'org-air-rail-return
                                                 rail "RET")
                       "jump" 0)
                      (org-air-view--verb-cell
                       (org-air-view--legend-key #'org-air-rail-popin
                                                 rail "|")
                       "rail" 0)))
         (line inset))
    (dolist (cell cells)
      (cond
       ((equal line inset) (setq line (concat line cell)))
       ((<= (+ (string-width line) (string-width gap) (string-width cell))
            width)
        (setq line (concat line gap cell)))
       (t (insert (org-air-view--pad-to line width) "\n")
          (setq line (concat inset cell)))))
    (unless (equal line inset)
      (insert (org-air-view--pad-to line width) "\n"))))

(defun org-air-outline--rail-descriptor (source-buf)
  "Return the generic outline rail descriptor for SOURCE-BUF (R30-4).
The SAME `:outline-fn' + `:actions-fn' seam the doc session uses — one
renderer, parameterised, never forked."
  (list :outline-fn (lambda (w)
                      (org-air-outline--insert-context source-buf w))
        :actions-fn (lambda (w)
                      (org-air-outline--insert-actions w source-buf))))

(defun org-air-outline--rail-show (buffer)
  "Show/re-render the outline side rail owned by BUFFER (R30-4).
Measures the window's USABLE columns (R29-1) so a fringe-less GUI never
composes past the displayable area."
  (let ((win (get-buffer-window buffer)))
    (org-air-rail--show buffer (if (window-live-p win)
                                   (max 40 (org-air-layout--usable-columns win))
                                 80))))

;; Forward declaration so the debounce hook (defined before the
;; `define-minor-mode') can reference the mode variable without a
;; free-variable warning; `define-minor-mode' below sets it up fully.
(defvar org-air-outline-mode nil)

(defvar org-air-outline--timer nil
  "Single debounce slot for the `org-air-outline-mode' highlight tick (R30-4).")

(defun org-air-outline--highlight-tick (buffer)
  "Timer body: re-place the outline highlight for BUFFER (R30-4)."
  (setq org-air-outline--timer nil)
  (org-air-outline--highlight-update buffer
                                     (get-buffer org-air-rail-buffer-name)))

(defun org-air-outline--post-command ()
  "Buffer-local hook: schedule the DEBOUNCED outline highlight (R30-4).
Interactive-only; ONE idle timer slot, rescheduled — never stacked."
  (when (and (not noninteractive) org-air-outline-mode)
    (when (timerp org-air-outline--timer)
      (cancel-timer org-air-outline--timer))
    (setq org-air-outline--timer
          (run-with-idle-timer 0.1 nil
                               #'org-air-outline--highlight-tick
                               (current-buffer)))))

(defun org-air-outline--teardown (buffer)
  "Tear down BUFFER's outline rail: timer, overlay, and the rail (R30-4)."
  (when (timerp org-air-outline--timer)
    (cancel-timer org-air-outline--timer))
  (setq org-air-outline--timer nil)
  (org-air-rail--outline-highlight-clear)
  (let ((rail (get-buffer org-air-rail-buffer-name)))
    (when (and (buffer-live-p rail)
               (eq (buffer-local-value 'org-air-rail--board-buffer rail)
                   buffer))
      (org-air-rail--hide buffer))))

;;;###autoload
(define-minor-mode org-air-outline-mode
  "Opt-in outline rail for ANY Org buffer (R30-4).
Enabling in an `org-mode' buffer pops the org-air context rail showing
this buffer's headings (via the SAME rail descriptor seam the doc session
uses) and follows point with the R28-4 current-heading highlight.  NO
dependency on `org-air-project' / org-ql / airctl — a light, generic
scaffold.  Off by default; a no-op outside `org-mode'."
  :lighter " ◦outline"
  :group 'org-air
  ;; R35-1: reconcile the shared rail map to the knob before the rail is
  ;; shown (honours use-package `:custom' / a runtime `setq').
  (org-air--sync-default-keybindings)
  (if org-air-outline-mode
      (if (not (derived-mode-p 'org-mode))
          ;; soft: enabling in a non-org buffer is a no-op.
          (setq org-air-outline-mode nil)
        (setq-local org-air-view--rail-descriptor
                    (org-air-outline--rail-descriptor (current-buffer)))
        ;; R49-2: the outline placement resolves through the SAME shared
        ;; resolver as the board/project (override slot:
        ;; `org-air-outline-rail-placement', nil = inherit).
        (setq-local org-air-view--rail-popped-out
                    (eq (org-air-rail--placement 'outline) 'side-window))
        (when (org-air-rail--popped-p)
          (org-air-outline--rail-show (current-buffer)))
        (unless noninteractive
          (add-hook 'post-command-hook
                    #'org-air-outline--post-command nil t)))
    (remove-hook 'post-command-hook #'org-air-outline--post-command t)
    (org-air-outline--teardown (current-buffer))
    (kill-local-variable 'org-air-view--rail-descriptor)
    (kill-local-variable 'org-air-view--rail-popped-out)))

(defvar org-air-rail-mode-map
  (let ((map (make-sparse-keymap)))
    ;; PARENT stays at defvar time — always, even with the knob nil, so a
    ;; key-less rail (outline mode / defaults off) still quits/scrolls via
    ;; `special-mode' (R35-1).
    (set-keymap-parent map special-mode-map)
    map)
  "Keymap for `org-air-rail-mode' (R16 D-P1 / R26-5).
Keys installed by `org-air--install-default-keybindings' (R35-1).")

;; R35-1: the rail default keys (installer-owned).  R16 D-P1: `q' pops the
;; rail back inline (or, in a DOC session, returns to the tree); RET jumps
;; the main window to the outline heading at point; `|' pops the rail in.
(org-air--register-default-keys 'org-air-rail-mode-map
  "q" #'org-air-rail-quit
  "RET" #'org-air-rail-return
  "|" #'org-air-rail-popin)

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
  ;; R58: the rail is a dependent buffer — its bookmark record DELEGATES
  ;; to the host view (board/project/revisit) so an activities.el layout
  ;; holding the rail restores it beside its host, order-independently.
  (setq-local bookmark-make-record-function
              #'org-air-rail--bookmark-make-record)
  ;; R27-4: the board's evil parity for the rail too — under evil, `q'/`RET'
  ;; /`|' were shadowed (evil-record-macro / evil-ret / evil-goto-column).
  ;; R35-1: gated on the knob (skipped with the defaults off).
  (when org-air-use-default-keybindings
    (org-air-view--setup-evil 'org-air-rail-mode org-air-rail-mode-map)))

(defun org-air-rail--get-buffer ()
  "Get or create the `*org-air-rail*' buffer in `org-air-rail-mode' (R15 D-P2)."
  (let ((buf (get-buffer-create org-air-rail-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-air-rail-mode)
        (org-air-rail-mode)))
    buf))

(defun org-air-rail--undisplayed-host-p (buffer)
  "Non-nil when host BUFFER renders windowless in an interactive session (R58).
A bookmark/activities restore rebuilds views UNDISPLAYED — the restorer
owns the window layout, so a render for a host no window shows must not
create, resize or sweep side windows (the R9/C1 resize-refresh re-fits —
and the rail lifecycle re-runs — the moment the restorer displays it).
Inert in batch: `noninteractive' keeps every golden's rail lifecycle
exactly as before."
  (and (not noninteractive)
       (not (get-buffer-window buffer t))))

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
                        :marked-keys org-air-view--marked-keys
                        :marked-table org-air-view--marked-key-table
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
            (org-air-view--marked-keys (plist-get state :marked-keys))
            (org-air-view--marked-key-table (plist-get state :marked-table))
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
  ;; R63-1a: never ensure (create/resize) the side window from a tail
  ;; that does not hold the rail claim — the width measure below falls
  ;; through to the live-window/fallback branches unchanged (the host
  ;; window's usable columns are already rail-adjusted, since the rail
  ;; lives on the frame under its real owner).
  ;; R58: never ensure the side window for an undisplayed host (bookmark
  ;; restore) — the measure below then falls back to WIDTH as always.
  (unless (or (not (org-air-rail--tail-owner-p host-buffer))
              (org-air-rail--undisplayed-host-p host-buffer))
    (org-air-rail--ensure-window host-buffer width))
  (let ((win (get-buffer-window host-buffer)))
    (if (window-live-p win)
        (max org-air-item-pane-min (org-air-layout--usable-columns win))
      width)))

(defun org-air-rail--input-stamp (board-buffer width height)
  "Return the rail content input stamp for BOARD-BUFFER at WIDTH x HEIGHT.
R27-1 S4: every input the rail paint reads through the back-pointer —
owner buffer, `org-air-view--items' identity, items key, filter, the
AND/OR combinator (R69-3: `org-air-filter-match' feeds the chip join
word, the `Match:' line and the `N of M shown' count, so M-/ must bust
the stamp exactly like `/' and `\\' do), scope, expanded sections,
calendar month, cols, height, descriptor identity, plus the calendar's
current day — so an unchanged stamp proves a repaint would be
byte-identical and may be skipped."
  (with-current-buffer board-buffer
    (list board-buffer
          org-air-view--items
          org-air-view--items-key
          org-air-view--tag-filter
          ;; R69-3: the AND/OR combinator IS a paint input (the chip join
          ;; word, the `Match: %s' line, and the `N of M shown' count all
          ;; read it) — without it the stamp guard proves an M-/ repaint
          ;; "byte-identical" and wrongly skips it.
          org-air-filter-match
          org-air-view--scope
          org-air-view--expanded-sections
          org-air-view--marked-keys
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
state is zero rail repaints and exactly one at the R26-8 swap.
R63-1: the ONE render tail every rail-showing surface routes through,
now governed by the deterministic two-belt single-owner rule — the
`org-air-rail--tail-owner-p' gate (a non-owner tail is a FULL no-op:
no window ensure, no content render, returns nil, and SELF
self-suspends so every later background render during a load is skipped
by the flag alone) plus the ownership-transfer suspension (a passing
tail whose host differs from the live rail's owner suspends the
PREVIOUS owner synchronously, in the same call that takes the rail —
mirroring the reconciler's re-own branch, so belt 2's hook-selection
blind spot is closed BEFORE any C1 resize render can fire).  The gate
takes precedence over the R58 undisplayed-host content carve (R63-1c):
in the bookmark-restore flow no other org-air host is active (active =
nil, gate passes) so R58 behaviour is unchanged there; in the mid-fill
flow the carve was the content-flip vector and must lose."
  (if (not (org-air-rail--tail-owner-p board-buffer))
      ;; R63-1a: gate failed — full no-op + self-suspend.  The suspended
      ;; flag's falling edge belongs to the reconciler alone (R63-1d /
      ;; R25-6): a suspended view re-pops ONLY via the reconciler's
      ;; suspended branch (settled active view, 0s timer) or an explicit
      ;; user toggle, both of which clear the flag first.
      (progn
        (when (buffer-live-p board-buffer)
          (with-current-buffer board-buffer
            (setq-local org-air-view--rail-suspended t)))
        nil)
    ;; R63-1b: ownership-transfer suspension — taking the rail from a
    ;; live previous owner suspends that owner SYNCHRONOUSLY, exactly
    ;; what the reconciler's re-own branch does, so the transfer is
    ;; deterministic and never inferred from transient window selection.
    (let ((prev (org-air-rail--side-owner)))
      (when (and (buffer-live-p prev) (not (eq prev board-buffer)))
        (with-current-buffer prev
          (setq-local org-air-view--rail-suspended t))))
    (org-air-rail--show-1 board-buffer width)))

(defun org-air-rail--show-1 (board-buffer width)
  "The ungoverned `org-air-rail--show' body for BOARD-BUFFER at WIDTH.
R63-1 split: only `org-air-rail--show' (which owns the tail-owner gate
and the ownership-transfer suspension) may call this."
  (let* ((cols (org-air-rail--window-cols (and org-air-view-width width)))
         ;; R58: a host no window shows (a bookmark/activities restore
         ;; rebuilding views undisplayed) must not create the side window
         ;; — the restorer owns the layout.  Rail CONTENT still renders
         ;; below, so the buffer is fresh when the restorer shows it.
         (win (unless (org-air-rail--undisplayed-host-p board-buffer)
                (org-air-rail--ensure-window board-buffer width))))
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
`org-air-project--render-current'; the revisit view via
`org-air-revisit--render-current' (R54-3)."
  (cond
   ((derived-mode-p 'org-air-view-mode) (org-air-view--render-current))
   ((derived-mode-p 'org-air-project-mode) (org-air-project--render-current))
   ((and (derived-mode-p 'org-air-revisit-mode)
         (fboundp 'org-air-revisit--render-current))
    (org-air-revisit--render-current))
   ;; R61-4: the review view re-renders in place (data untouched).
   ((and (derived-mode-p 'org-air-review-mode)
         (fboundp 'org-air-review--render-current))
    (org-air-review--render-current))
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
  (unless (or (derived-mode-p 'org-air-view-mode 'org-air-project-mode
                              'org-air-revisit-mode 'org-air-review-mode)
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
  "Non-nil when BUF is an org-air rail HOST buffer (R25-6; R62-1a).
The hosts are the board, the project, the revisit AND the review views
\(every main view that pops the shared `*org-air-rail*' side window —
omitting one here is exactly the R62-1 bug: the reconciler treated the
review view as a foreign window and evicted its rail).
R26-5: a DOC-SESSION file buffer (one carrying the back-pointer
`org-air-project--session-tree') counts as a host too, so the R25-6
suspension/re-pop sweep treats the doc half of a project session exactly
like a board<->project switch."
  (and (buffer-live-p buf)
       (or (with-current-buffer buf
             (derived-mode-p 'org-air-view-mode 'org-air-project-mode
                             'org-air-revisit-mode 'org-air-review-mode))
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

(defun org-air-rail--tail-owner-p (self &optional frame)
  "Non-nil when SELF's render tail may mutate the shared rail (R63-1a).
The deterministic single-owner rule's GATE, consulted at the three rail
choke points (`org-air-rail--show', `org-air-rail--host-width''s window
ensure, `org-air-rail--evict-foreign-rail'): the shared `*org-air-rail*'
may be mutated ONLY by (1) the reconciler — which resolves the SETTLED
active view from a 0s timer and is exempt by construction: its re-own /
re-pop branches first clear the target's suspended flag and run for the
active view, so this gate passes — and (2) a render tail whose view
currently holds the rail claim.  Two conjuncts, both load-bearing
\(measured: either alone is insufficient):

  non-suspended?  SELF's buffer-local `org-air-view--rail-suspended' is
    nil.  Belt 1: ownership transfer marks the previous owner suspended
    SYNCHRONOUSLY (`org-air-rail--show', R63-1b), so a background render
    of the dispossessed view is blocked here even inside the C1
    resize-hook window where the view's window is HOOK-SELECTED and any
    instantaneous active check misreads (the measured +0.01s steal).
  active?  `org-air-rail--active-view' on FRAME is nil or SELF.  Belt 2:
    blocks the timer-driven R56 progressive/finish repaints of a view
    that never owned the rail (selection settled by then).

A dead SELF never holds a claim.  Pure reads — no window mutation."
  (and (buffer-live-p self)
       (not (buffer-local-value 'org-air-view--rail-suspended self))
       (let ((active (org-air-rail--active-view frame)))
         (or (null active) (eq active self)))))

(defun org-air-rail--evict-foreign-rail (self)
  "Hide a `*org-air-rail*' side window that does NOT belong to SELF (R25-6).
Suspends its owner (flag kept) so returning to that owner re-pops cleanly.
Called from a render tail: when SELF is popped, `org-air-rail--show' has
already re-owned the window, so the owner == SELF and this no-ops; when
SELF is inline it drops a lingering foreign rail (the cross-view sweep)."
  (let* ((frame (selected-frame))
         (side  (org-air-rail--side-window frame))
         (owner (org-air-rail--side-owner frame)))
    (when (and (window-live-p side) (not (eq owner self))
               ;; R63-1a: sweeping a foreign rail is an owner/active
               ;; privilege — without this gate the GATED board tail
               ;; (owner /= self, no `--show' re-own) would fall through
               ;; here and DELETE the active view's rail: the same bug
               ;; with the opposite sign.
               (org-air-rail--tail-owner-p self frame)
               ;; R58: an undisplayed self-render (a bookmark restore) has
               ;; no layout claim — never sweep the displayed view's rail.
               (not (org-air-rail--undisplayed-host-p self)))
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
alone — the render tail owns it).
R63-1d: the reconciler OWNS the suspension flag's falling edge (the
R25-6 contract, now stated and pinned): a view suspended by the tail
gate or the transfer belt re-pops ONLY through the suspended branch
below — settled active view, 0s timer — or an explicit user toggle;
no render tail may clear the flag for itself."
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

(defvar-local org-air-view-pane--bookmark-ctx nil
  "Printable (FILE POS TITLE HOST) of the snapshot this pane shows (R58).
Local to the `*org-air-view*' snapshot buffer; written by the one
snapshot writer (`org-air-view-pane--render-snapshot') so the pane's
bookmark record producer is a pure buffer-local read.  Every element is
plain readable data — never a marker or buffer.")

(defun org-air-view-pane--bookmark-stash (ctx)
  "Return the printable (FILE POS TITLE HOST) bookmark stash for CTX (R58).
Called in the HOST buffer (the snapshot render's caller context) so HOST
names the view that drove the pane.  A live marker degrades to its
position at stash time (R58 rule 5: a marker must never enter a bookmark
record); never signals — an odd CTX yields nil."
  (condition-case nil
      (let* ((m (plist-get ctx :marker))
             (file (or (plist-get ctx :file)
                       (cond ((stringp m) m)
                             ((consp m) (car-safe m))
                             ((and (markerp m) (marker-buffer m))
                              (buffer-file-name (marker-buffer m))))))
             (pos (cond ((consp m) (cdr m))
                        ((markerp m) (marker-position m))))
             (title (plist-get ctx :title)))
        (list (and (stringp file) (substring-no-properties file))
              (if (integerp pos) pos 1)
              (and (stringp title) (substring-no-properties title))
              (cond ((derived-mode-p 'org-air-project-mode) 'project)
                    ((derived-mode-p 'org-air-revisit-mode) 'revisit)
                    ;; R62-1c: the review-driven pane stamps its real
                    ;; host (a sibling of the R62-1a roster omission).
                    ((derived-mode-p 'org-air-review-mode) 'review)
                    (t 'board))))
    (error nil)))

(defvar org-air-entry-view-mode-map
  (let ((map (make-sparse-keymap)))
    ;; PARENT stays at defvar time — always, even with the knob nil, so a
    ;; key-less snapshot pane still buries/scrolls via `special-mode'
    ;; (R35.1).
    (set-keymap-parent map special-mode-map)
    map)
  "Keymap for `org-air-entry-view-mode' (the read-only snapshot pane).
Keys installed by `org-air--install-default-keybindings' (R35-1 / R35.1).")

;; R35.1: the snapshot-pane close key (installer-owned).  R20-3a: the pane
;; is read-only, so `q' closes it (overrides `special-mode's bury so the
;; pane is actually torn down) instead of merely burying the buffer.
(org-air--register-default-keys 'org-air-entry-view-mode-map
  "q" #'org-air-view-pane-quit)

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
  ;; R58: the snapshot pane delegates to its host + the entry's (FILE .
  ;; POS) source, so a saved layout holding the pane restores cleanly.
  (setq-local bookmark-make-record-function
              #'org-air-view-pane--bookmark-make-record)
  ;; R27-4: evil parity for the read-only pane — under evil, `q' resolved
  ;; to evil-record-macro instead of closing the pane.
  ;; R35-1: gated on the knob (skipped with the defaults off).
  (when org-air-use-default-keybindings
    (org-air-view--setup-evil 'org-air-entry-view-mode
                              org-air-entry-view-mode-map)))

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

(defun org-air-view-pane--reveal (&optional wide)
  "Reveal the pane's content, then re-fold its drawers (R65-1).
Runs in the current (PANE) buffer only: `org-fold-show-subtree' reveals
the narrowed heading's ENTIRE subtree — body, sub-headings and their
bodies (WIDE non-nil — the heading-less / pos-nil file-head case — uses
`org-fold-show-all' instead: there is no subtree to bound); THEN the
zero-arg `org-fold-hide-drawer-all' (the Org 9.6-compatible call shape,
bounded by the narrow) re-folds the `:PROPERTIES:' (and other) drawers,
matching Org's own cycle idiom.  Pane-LOCAL by construction: on
org-air's platform floor (Emacs 29.1 / Org 9.6) `make-indirect-buffer'
runs `clone-indirect-buffer-hook', where org-fold DECOUPLES the pane's
fold state at creation (text-properties style), and overlay folds are
private itree clone copies — so nothing here can mutate the SOURCE
buffer's folds.  Each call rides an `fboundp' ladder (the
`org-show-context' precedent) so an exotic pre-org-fold Org degrades
sanely.  Never errors (R53): any failure degrades to today's folded
pane, never a broken `v'."
  (condition-case nil
      (progn
        (if wide
            (funcall (if (fboundp 'org-fold-show-all)
                         #'org-fold-show-all
                       (intern "org-show-all")))
          (funcall (if (fboundp 'org-fold-show-subtree)
                       #'org-fold-show-subtree
                     (intern "org-show-subtree"))))
        (if (fboundp 'org-fold-hide-drawer-all)
            (org-fold-hide-drawer-all)       ; zero-arg: the Org 9.6 shape
          (funcall (intern "org-cycle-hide-drawers") 'all)))
    (error nil)))

(defun org-air-view-pane--indirect (base pos title)
  "Return an `org-mode' indirect buffer on BASE narrowed to the subtree at POS.
Edits write through to BASE; `save-buffer' persists to disk (R19-3).  TITLE
names the (hidden) indirect buffer.  When POS is before the first heading —
a heading-less file head — the buffer is left WIDE so the file shows.
Narrowing is per-indirect-buffer — it never leaks to BASE or to the board's
own markers/classify scans of that file.  R65-1: the pane's content is
REVEALED (body + sub-headings visible) with the drawers re-folded —
pane-locally, the source buffer's fold state is never touched."
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
      (let ((wide t))
        (when pos
          (goto-char pos)
          (ignore-errors (org-back-to-heading t))
          (if (org-before-first-heading-p)
              (widen)                        ; heading-less / preamble
            (org-narrow-to-subtree)
            (setq wide nil)))
        (goto-char (point-min))
        ;; R65-1: reveal the item's body/sub-headings, re-fold drawers —
        ;; pane-local (the clone inherited the base's FOLDED state; the
        ;; base's own folds are decoupled and stay untouched).
        (org-air-view-pane--reveal wide)))
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
from `org-mode' still works underneath.
R35.1: gated on `org-air-use-default-keybindings' — with the knob nil
org-air installs NO close map in this editable indirect pane (which is the
user's OWN file content); the dedicated close key + the `quit-window'
remap are then absent and the buffer keeps its plain `org-mode' local map."
  (when org-air-use-default-keybindings
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map (current-local-map))
      (define-key map (kbd "C-c C-q") #'org-air-view-pane-quit)
      (define-key map [remap quit-window] #'org-air-view-pane-quit)
      (use-local-map map))))

(defun org-air-view-pane--snapshot-fold-drawers ()
  "Fold `:PROPERTIES:' drawers in the snapshot pane, DISPLAY-ONLY (R65-2).
Walks the current (pane) buffer for property drawers and puts the
`org-air-pane-drawer' `invisible' text property from the end of each
`:PROPERTIES:' line through the end of its matching `:END:' line, with
the invisibility-spec entry registered for an ellipsis — the folded
look, with buffer BYTES untouched (every pane golden and the R58
bookmark stash stay byte-identical; string `equal' ignores text
properties).  Unterminated drawers are skipped; never errors (R53)."
  (condition-case nil
      (progn
        (unless (and (listp buffer-invisibility-spec)
                     (member '(org-air-pane-drawer . t)
                             buffer-invisibility-spec))
          (add-to-invisibility-spec '(org-air-pane-drawer . t)))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^[ \t]*:PROPERTIES:[ \t]*$" nil t)
            (let ((beg (point)))             ; end of the :PROPERTIES: line
              (when (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
                (put-text-property beg (line-end-position)
                                   'invisible 'org-air-pane-drawer))))))
    (error nil)))

(defun org-air-view-pane--render-snapshot (ctx src)
  "Render the READ-ONLY entry snapshot for CTX/SRC into `*org-air-view*'.
The unchanged R16 path: a fontified COPY of the subtree, dead sources show
a calm hint.  Used under `noninteractive', when `org-air-view-pane-editable'
is nil, or when the source is unresolvable — so every fixture stays
byte-identical (R19-3).  R65-2: the body stays visible (a copy carries no
live folds) and the `:PROPERTIES:' drawer(s) are folded DISPLAY-ONLY —
buffer bytes unchanged.  Returns the pane buffer."
  (let ((buf (org-air-view-pane--buffer))
        ;; R58: capture the printable bookmark stash HERE, in the caller's
        ;; (host) buffer — the one writer of the snapshot — so the pane's
        ;; record producer never re-derives sources.
        (bm (org-air-view-pane--bookmark-stash ctx)))
    ;; The editable indirect (if any) is being replaced by the snapshot;
    ;; forget it (the caller kills the now-unshown buffer).
    (when (org-air-view--pane-host-p)
      (setq-local org-air-view--pane-indirect nil))
    (with-current-buffer buf
      (setq-local org-air-view-pane--bookmark-ctx bm)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (null src)
            (insert (propertize "(entry no longer available)"
                                'face 'org-air-face-empty))
          (let ((text (org-air-view-pane--entry-text (car src) (cdr src))))
            (insert text)
            (org-air-view-pane--apply-max-lines)
            ;; R65-2: display-only fold of the raw `:PROPERTIES:' dump —
            ;; applied AFTER the truncation, bytes unchanged (goldens
            ;; stay byte-identical).
            (org-air-view-pane--snapshot-fold-drawers)))
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
back to a rebuild (R20-3b).  R65-1: the reuse path re-reveals per item
\(body visible, drawers re-folded) — pane-local, like the build path;
stale reveal state outside the new narrow is invisible + pane-private."
  (condition-case nil
      (with-current-buffer ind
        (widen)
        (let ((pos (cdr src))
              (wide t))
          (when pos
            (goto-char pos)
            (ignore-errors (org-back-to-heading t))
            (if (org-before-first-heading-p)
                (widen)
              (org-narrow-to-subtree)
              (setq wide nil)))
          (goto-char (point-min))
          ;; R65-1: same reveal as the build path (the second call site).
          (org-air-view-pane--reveal wide))
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
\(`org-air-visit-item') or, on a TTY that cannot send S-RET, `O'.
R51-3: on the `…and N more' fold row RET (and <mouse-1> — this same
command) dispatches to `org-air-toggle-section' and does ONLY that —
nothing opens, no pane (the exact shape of the R48-3 fold branch in
`org-air-project-open').
R54-3 (fork F4): on the NOTES section heading — the count row that
advertises the knowledge corpus — RET opens the Revisit view instead:
the count row is the doorway to the full resurfacing surface (TAB still
expands the bounded preview in place)."
  (interactive)
  (cond
   ((org-air-view--row-property 'org-air-more-row)
    (org-air-toggle-section))
   ;; R54-3 F4: the Notes section HEADING (never an item row — those
   ;; carry `org-air-item' and keep the pane) answers RET with Revisit.
   ((and (eq (org-air-view--line-section) 'notes)
         (not (org-air-view--row-property 'org-air-item))
         (fboundp 'org-air-revisit))
    (org-air-revisit))
   (t
    ;; R54-3: an org-air-initiated open — feed the opt-in visit ledger
    ;; (a no-op at the default; never a global hook).
    (when-let* ((item (org-air-view--row-property 'org-air-item)))
      (org-air--note-visited (org-air-item-file item)))
    ;; Focus only when the pane is ALREADY live: the first RET opens (no
    ;; focus), the second RET (pane now live) focuses.  `org-air-view-pane'
    ;; honours the dynamic `org-air-view-pane-focus' binding.
    (let ((org-air-view-pane-focus (org-air-view-pane--window-live-p)))
      (org-air-view-pane)))))

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

(defun org-air-view--panes-resync-now ()
  "Force a pane/inspector resync for the item NOW at point (R73-1).
Called from the board's TWO swap tails — the `org-air-view--refresh-repaint'
tail (which covers the machine's `--refresh-finish' swap, the ≤budget
sync fast path, the no-change path, and every failure/cancel repaint)
and the fully-synchronous `org-air-refresh' else-branch tail — AFTER
`org-air-view--restore-position', so the resync reads the FINAL landing.
Position tracking (`post-command-hook') answers \"did the cursor move?\",
never \"did the board change under the cursor?\" — this is the missing
half: direct, timer-free calls (batch visible, no idle wait) to
`org-air-view--inspector-update-now' and
`org-air-view--view-pane-update-now', SELF-LIMITED by struct
identity (Decision 1): a rescanned file's items are FRESH structs (the `eq'
guards redraw — the content-changed case), a retained file's items are
the very same `eq' structs (skip — provably current, no FORCE churn),
and a removed item leaves a DIFFERENT struct at point (redraw to the
new item — the done/drop case).  First both pending debounce timers are
cancelled (a pre-refresh debounce must not fire later against
superseded state) and the R20-3b `--view-pane-last-pos' bookkeeping is
re-stamped (the pane is now correct FOR this position).

Decision 2 — the empty degrade, RESYNC-SCOPED only: when NO context
resolves at-or-after point (`org-air-view-pane--context-at-point' with
its R24-4 fall-forward) AND the board is TRULY empty — no item rows
anywhere (the fall-forward never looks BACKWARD, so a nil context
alone also covers point merely parked below the last row of a
POPULATED board, where keep-last must hold instead) — the last item
graduated, the board is empty: with the pane window live, the pane is
CLOSED via
`org-air-view-pane--hide' (the R20-3 teardown — base buffer and its
unsaved state survive) and the inspector renders its nil placeholder —
never a stale/dead-item pane.  Deliberately NOT folded into
`--view-pane-update-now' itself: in normal FOLLOW motion a nil-thing
row keeping the last item visible is long-standing, harmless behaviour.
The same keep-last scoping bounds the INSPECTOR here: a nil-thing
CHROME row (the banner after a cold scan, a parked section heading)
with items still resolvable below skips the inspector nudge — the
region keeps its render instead of degrading to the placeholder on
every refresh (and the swap goldens stay byte-clean); the placeholder
is reserved for the true empty degrade, where it is the honest render.

Never signals, arms NO timers, re-queries NOTHING (R53 — the renders
work from cached structs; the pane's own source visit is its existing
R16 render path, unchanged).  The post-command trackers and their
debounces are byte-untouched — this is an ADDITIONAL synchronous nudge
at the two swap tails only."
  (condition-case nil
      (when (derived-mode-p 'org-air-view-mode)
        (let ((buf (current-buffer)))
          ;; Cancel superseded pre-refresh debounces + keep the R20-3b
          ;; early-out's bookkeeping truthful.
          (when (timerp org-air-view--inspector-timer)
            (cancel-timer org-air-view--inspector-timer)
            (setq org-air-view--inspector-timer nil))
          (when (timerp org-air-view--view-pane-timer)
            (cancel-timer org-air-view--view-pane-timer))
          (setq-local org-air-view--view-pane-timer nil)
          (setq-local org-air-view--view-pane-last-pos (point))
          ;; Direct, identity-limited updates for the item NOW at point.
          (let* ((thing (get-text-property (point)
                                           org-air-view--inspector-property))
                 (ctx (org-air-view-pane--context-at-point))
                 ;; R73fix: the degrade legs below require the board
                 ;; TRULY empty — the R24-4 fall-forward never looks
                 ;; BACKWARD, so a nil ctx alone also matches point
                 ;; parked BELOW the last row of a populated board (the
                 ;; pad tail), where keep-last must hold.
                 (board-empty
                  (and (null ctx)
                       (not (text-property-not-all
                             (point-min) (point-max)
                             org-air-view--inspector-property nil)))))
            ;; The inspector nudge: for a real thing (the eq guard decides
            ;; redraw-vs-skip) or the true empty degrade (nil thing, board
            ;; truly empty — the nil placeholder is the honest render).
            ;; A nil-thing CHROME row or the pad tail with items still
            ;; on the board keeps the last render (Decision 2's keep-last
            ;; scoping).
            (when (or thing board-empty)
              (org-air-view--inspector-update-now buf))
            (org-air-view--view-pane-update-now buf)
            ;; Decision 2: the empty degrade — no items remain anywhere
            ;; after the swap: close the pane rather than paint a
            ;; removed item.
            (when (and board-empty
                       (org-air-view-pane--window-live-p))
              (setq-local org-air-view--view-pane-item nil)
              (org-air-view-pane--hide)))))
    (error nil)))

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

(defun org-air-view--rendered-item-occurrences ()
  "Return rendered item occurrences as `(KEY SECTION POSITION)' in row order."
  (save-excursion
    (goto-char (point-min))
    (let ((section nil) out)
      (while (not (eobp))
        (when-let* ((heading (org-air-view--line-section)))
          (setq section heading))
        (when-let* ((item (org-air-view--row-property 'org-air-item))
                    (key (org-air-view--item-source-key item)))
          (push (list key section (line-beginning-position)) out))
        (forward-line 1))
      (nreverse out))))

(defun org-air-view--mutation-landing-capture ()
  "Capture the R90 section-local landing plan for a cached mutation."
  (let* ((item (org-air-view--row-property 'org-air-item))
         (key (and item (org-air-view--item-source-key item)))
         (section (org-air-view--section-at-point))
         (line (line-beginning-position))
         (occurrences (org-air-view--rendered-item-occurrences))
         (section-occ (seq-filter (lambda (entry)
                                    (eq (nth 1 entry) section))
                                  occurrences))
         (section-index (or (cl-position line section-occ
                                         :key (lambda (entry) (nth 2 entry)))
                            0))
         (full-index (or (cl-position line occurrences
                                      :key (lambda (entry) (nth 2 entry)))
                         0)))
    (list :key key
          :section section
          :title-column (save-excursion
                          (org-air-view--goto-row-title)
                          (current-column))
          :section-keys (mapcar #'car section-occ)
          :section-index section-index
          :full-keys (mapcar #'car occurrences)
          :full-index full-index
          :line (line-number-at-pos)
          :excluded nil)))

(defun org-air-view--mutation-landing-exclude (plan keys)
  "Set PLAN's exact moved-key exclusion set to KEYS and return PLAN."
  (plist-put plan :excluded (delete-dups (delq nil (copy-sequence keys)))))

(defun org-air-view--find-source-key-row (key &optional section)
  "Return a rendered row position for exact source KEY, optionally in SECTION."
  (when key
    (save-excursion
      (goto-char (point-min))
      (let ((current-section nil) found)
        (while (and (not found) (not (eobp)))
          (when-let* ((heading (org-air-view--line-section)))
            (setq current-section heading))
          (when (or (null section) (eq section current-section))
            (when-let* ((item (org-air-view--row-property 'org-air-item))
                        ((equal key (org-air-view--item-source-key item))))
              (setq found (line-beginning-position))))
          (forward-line 1))
        found))))

(defun org-air-view--mutation-landing-consume ()
  "Consume and apply the pending R90 local landing plan, if any.
Moved source keys are never accepted as a fallback.  The same-section
successor at the old local row index wins, then the nearest previous row,
then the nearest non-excluded outward item, then section chrome."
  (when org-air-view--pending-mutation-landing
    (let* ((plan org-air-view--pending-mutation-landing)
           (_ (setq org-air-view--pending-mutation-landing nil))
           (key (plist-get plan :key))
           (section (plist-get plan :section))
           (excluded (plist-get plan :excluded))
           (excluded-p (lambda (candidate) (member candidate excluded)))
           (position nil))
      ;; 1. The exact key wins whenever it still renders in the OLD section,
      ;; even when it was a potential board-move key.  Dated backlog items,
      ;; for example, remain in their day section.  Only a key that actually
      ;; left the old section is subject to exclusion; an excluded key is
      ;; never followed globally into a different board section.
      (unless (null key)
        (setq position (org-air-view--find-source-key-row key section))
        (unless (or position (funcall excluded-p key))
          (setq position (org-air-view--find-source-key-row key))))
      ;; 2/3. Same-section survivor at the old local index, else its tail.
      (unless (or position (null key))
        (let* ((survivors (seq-remove excluded-p
                                      (plist-get plan :section-keys)))
               (index (plist-get plan :section-index))
               (candidate (or (nth index survivors) (car (last survivors)))))
          (setq position (org-air-view--find-source-key-row candidate section))))
      ;; 4. Nearest non-excluded survivor in old full visible order;
      ;; following wins at equal distance.
      (unless (or position (null key))
        (let* ((keys (plist-get plan :full-keys))
               (origin (plist-get plan :full-index))
               (radius 1)
               (limit (length keys)))
          (while (and (not position) (< radius (1+ limit)))
            (dolist (index (list (+ origin radius) (- origin radius)))
              (when (and (not position) (>= index 0) (< index limit))
                (let ((candidate (nth index keys)))
                  (unless (funcall excluded-p candidate)
                    (setq position
                          (org-air-view--find-source-key-row candidate))))))
            (setq radius (1+ radius)))))
      (cond
       (position
        (goto-char position)
        (org-air-view--goto-row-title))
       ;; A bulk command may run while point is on chrome; preserve that
       ;; chrome when possible instead of inventing an item selection.
       ((and (null key)
             section
             (org-air-view--find-property 'org-air-section section))
        (goto-char (org-air-view--find-property 'org-air-section section))
        (org-air-view--beginning-of-visible))
       ;; 6. Old section header, nearest/first section header, then chrome.
       ((and section (org-air-view--find-property 'org-air-section section))
        (goto-char (org-air-view--find-property 'org-air-section section))
        (org-air-view--beginning-of-visible))
       ((text-property-not-all (point-min) (point-max) 'org-air-section nil)
        (goto-char (text-property-not-all (point-min) (point-max)
                                          'org-air-section nil))
        (org-air-view--beginning-of-visible))
       (t
        (goto-char (point-min))
        (org-air-view--beginning-of-visible))))))

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
                    org-air-view--items-key (org-air-view--cache-key)
                    org-air-view--tag-filter tag-filter)
              ;; A replacement generation builds linear eq/file indexes,
              ;; releases stale locators, and hydrates already-live sources.
              ;; Same-generation repaint/filter/mark/collapse skips all source
              ;; tracking work; no file is opened in either path.
              (org-air-view--source-generation-synchronize items)
              ;; R90 generation swap: exact source-key reconciliation only.
              (org-air-view--marked-reconcile items)
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
    ;; for dispatch again.  R26-5/R49-2: the placement seeds too, through
    ;; the ONE shared resolver `org-air-rail--placement' (interactive
    ;; only; batch keeps `unset' -> nil, or the explicit
    ;; `org-air-rail-style' back-compat force).
    (when (eq org-air-view--rail-popped-out 'unset)
      (setq-local org-air-view--rail-popped-out
                  (or (eq org-air-rail-style 'side-window)
                      (and (not noninteractive)
                           (eq (org-air-rail--placement 'board)
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
      ;; R43-2: the two-pane pane-divider column (item-width + the divider's
      ;; leading space), so the finalize tail keeps the divider INTERIOR on
      ;; every board row instead of trimming the blank rail tail.  Nil for
      ;; board-only / stacked / side-window (and plain style, which has no
      ;; faced vrule) — those keep today's trim behaviour exactly.
      (setq-local org-air-view--pane-divider-col
                  (and (eq org-air-view--orientation 'two-pane)
                       (let* ((geom org-air-view--inspector-geom)
                              (iw (plist-get geom :item-width))
                              (div (plist-get geom :divider))
                              (pos (and (stringp div)
                                        (string-match
                                         (regexp-quote
                                          (org-air-view--glyph 'vrule))
                                         div))))
                         (and (integerp iw) pos (+ iw pos)))))
      (org-air-view--insert-lines header)
      (setq-local org-air-view--body-beg (point-marker))
      (org-air-view--insert-lines body)
      (setq-local org-air-view--body-end (point-marker))
      (org-air-view--insert-lines footer))
    (if (integerp org-air-view-width)
        (org-air-view--normalize-buffer-lines org-air-view-width)
      ;; D7/D6 — cap every line at the displaying window and right-trim;
      ;; R43-2 — two-pane divider rows are padded to width (divider interior).
      (org-air-view--finalize-buffer-lines width org-air-view--pane-divider-col))
    ;; T5: drop the trailing newline so the buffer is EXACTLY the filled
    ;; line count — otherwise the final \n renders one phantom blank row
    ;; below the footer, overrunning the body height by one.
    (goto-char (point-max))
    (when (and (bolp) (> (point-max) (point-min)))
      (delete-char -1))
    (setq org-air-view--rendered-width width
          org-air-view--rendered-height height)
    (org-air-view--goto-first-item)
    ;; R58: an armed bookmark locator owns the landing — one text-property
    ;; scan over the rendered rows; stays armed only while the refresh
    ;; machine is still filling (a progressive paint may not hold the row
    ;; yet), so the inspector below syncs to the bookmarked item.
    (org-air-view--bookmark-consume)
    ;; R90 mutation landing overrides only this mutation repaint.  Consume
    ;; it after default/bookmark placement and BEFORE inspector/rail setup.
    (org-air-view--mutation-landing-consume)
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
      ;; R63-1a: the responsive teardown is an OWNER privilege — the
      ;; fourth gated tail.  A narrow NON-owner (or suspended) render
      ;; must never delete another view's live rail; the actual owner's
      ;; R56 board-only collapse passes the gate unchanged.
      ;; R58: an undisplayed (bookmark-restored) board must not delete the
      ;; displayed layout's windows.
      (when (and (org-air-rail--tail-owner-p (current-buffer))
                 (not (org-air-rail--undisplayed-host-p (current-buffer))))
        (org-air-rail--hide (current-buffer)))))
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
or `org-air-view--finalize-buffer-lines' (cap to WIDTH + right-trim, and —
R43-2/R44-2 — pad + `:align-to'-pin a pane-divider row) so a spliced /
inspector-refilled line is byte-identical AND pixel-identical to the
corresponding full-render line.  The pin keeps the divider straight on the
inspector-region rows too (they are re-composed here AFTER finalize)."
  (if (integerp org-air-view-width)
      (org-air-view--pad-to line org-air-view-width)
    (let* ((capped (if (> (string-width line) width)
                       (truncate-string-to-width
                        line width nil nil (org-air-view--glyph 'more))
                     line))
           (col org-air-view--pane-divider-col))
      (if (org-air-view--pane-divider-line-p capped col)
          (org-air-view--pin-pane-divider
           (org-air-view--pad-to capped width) col)
        (string-trim-right capped)))))

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
    ;; R58: with a bookmark locator armed the locator owns the landing —
    ;; the saved point belongs to the pre-restore skeleton, so the
    ;; save/restore pair is skipped and the render tail's
    ;; `org-air-view--bookmark-consume' places point instead.
    (let* ((mutationp org-air-view--pending-mutation-landing)
           (token (and (not mutationp)
                       (not org-air-view--bookmark-locator)
                       (org-air-view--save-position))))
      (org-air-view--render (or org-air-view--items (org-air-query-items))
                            org-air-view--tag-filter)
      (when token
        (org-air-view--restore-position token))))))

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
           ;; R56 P3b: the skeleton's centred line carries the live scan
           ;; numbers too, so even the pre-content chrome answers "is it
           ;; doing anything?" honestly.
           (msg (propertize
                 (if (and (eq org-air-view--refresh-state 'refreshing)
                          (> org-air-view--refresh-total 0))
                     (format "Loading your board… (scanning %d/%d)"
                             (max 0 (- org-air-view--refresh-total
                                       (length org-air-view--refresh-queue)))
                             org-air-view--refresh-total)
                   "Loading your board…")
                 'face 'org-air-face-faded))
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

(defconst org-air-view--cache-version 6
  "Serialisation version of `org-air-cache-file' (R26-8).  Bump = discard.
v2 (R53): `org-air-item' gained the scan-time slots
\(kind/donep/activity/body-deadline) that make the cache LOAD-BEARING —
a cache-hit board renders data-pure, never opening a file.
v3 (R53fix): the struct gained `subtree-ts' (day view's Logged/created
key); a v2 cache would hydrate records of the wrong shape.
v4 (R54): the struct gained `active-ts' (the R54-1 stale-eligibility
signal) and `ntype' (the R54-2 note type), and the cache carries the
per-file `:file-meta' table.  R54-3 (still v4, the declared one-bump
shape): the file-meta plists gained the link-graph keys
\(:ids/:links-raw/:links-out/:links-in) and the cache the `:visits'
ledger — both were declared part of the v4 shape when R54 part 1
landed, so a part-1 v4 cache still hydrates cleanly: empty ledger, and
link-less metas are SKIPPED by `org-air-query-file-meta-hydrate' (they
would read as false all-orphans and get re-persisted by a warm cache
write), so the file-meta table starts empty and re-fills via the paced
scan.  A v3 cache is simply a cold miss — skeleton + paced rescan,
never a hang.
v5 (R59): the struct gained the two container signal slots (`childp' +
`own-active-ts') backing `org-air-query-container-item-p'; a v4 cache
would hydrate records of the wrong shape, so it is a clean one-time
cold miss — skeleton + the R56 paced rescan, never a hang (the
documented version-mismatch path).
v6 (R61): the struct gained the four review harvest slots (`clocks' /
`logs' / `created' / `rtrunc') — v5 records have the wrong record
length, so a v5 cache is the same clean one-time cold miss.  New
TRAILING slots ride the existing print/`read' machinery with zero new
serialisation code — the version bump exists precisely for the record
length.  R90 retains v6 because its final source projection is again Org's
native title/tag semantics; the experimental broad projection was never a
released cache contract.")

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
                  :key (org-air-view--cache-key)
                  :mtimes mtimes
                  ;; R54-2: persist the per-file fact table, pruned to the
                  ;; snapshot's files so vanished files never linger.
                  :file-meta (org-air-query-file-meta-alist
                              (mapcar #'car mtimes))
                  ;; R54-3: the bounded visit ledger — the prune to the
                  ;; snapshot's files IS the bound (size <= file count).
                  :visits (org-air-query-visits-alist
                           (mapcar #'car mtimes))
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
               (equal (plist-get data :key) (org-air-view--cache-key))
               (listp (plist-get data :items))
               data))
      (error nil))))

(defun org-air-view--mtimes-snapshot (files)
  "Return an alist FILE -> current mtime for FILES (R42-1).
The baseline recorded in `org-air-view--items-mtimes' after a completed
full scan, so a later `org-air-view--refresh-start' can prove — via
`org-air-view--changed-files' — exactly which files changed without
reparsing anything."
  (mapcar (lambda (f)
            (cons f (file-attribute-modification-time
                     (file-attributes f))))
          files))

(defvar org-air-view--changed-files-mtimes nil
  "FILE -> mtime memo of the last `org-air-view--changed-files' run (R56 P1c).
An alist.  The enumeration diff already stats every current file; this
stash lets `org-air-view--refresh-queue-order' sort the paced queue by recency
WITHOUT a second stat pass (reuse, never re-stat).  A memo of
already-paid data only — never read as a staleness source (the scan-time
mtimes in `org-air-view--refresh-mtimes' stay the one baseline
authority).")

(defun org-air-view--changed-files (files snapshot)
  "Return the FILES whose mtime diverged from SNAPSHOT (R42-1).
SNAPSHOT is an alist FILE -> mtime from the last completed full scan.  A
file is \"changed\" when it is new (absent from SNAPSHOT), its mtime
differs, or it VANISHED (present in SNAPSHOT but no longer among FILES —
its stale rows must be dropped).  Pure: the shared staleness oracle for
both the cache-first load (`org-air-view--cache-load') and the
mtime-incremental refresh (`org-air-view--refresh-start').  nil when every
mtime matches (FRESH: no scan at all)."
  ;; R53 P1d: hash the snapshot and diff in ONE pass — the old per-file
  ;; `assoc' + `member' was O(n²) (measured 0.275s -> 0.051s at 5006
  ;; files).  The snapshot stays an alist on disk; hashed in memory only.
  (let ((table (make-hash-table :test #'equal :size (length snapshot)))
        (changed nil)
        (stats nil))
    (dolist (entry snapshot)
      (puthash (car entry) (cdr entry) table))
    (dolist (f files)
      (let ((mtime (file-attribute-modification-time
                    (file-attributes f))))
        ;; R56 P1c: stash the stat this diff already paid so the paced
        ;; queue can order by recency with zero extra stats.
        (push (cons f mtime) stats)
        (unless (equal (gethash f table 'org-air--missing) mtime)
          (push f changed)))
      (remhash f table))
    (setq changed (nreverse changed)
          org-air-view--changed-files-mtimes (nreverse stats))
    ;; a snapshot file that vanished also invalidates (its rows linger).
    (maphash (lambda (k _v) (push k changed)) table)
    changed))

(defun org-air-view--files-intersect (changed files)
  "Return the members of CHANGED that are in FILES, order preserved (R60-3).
The defence-in-depth half of the R60 refresh key guard, independently
correct: `org-air-query-items-in-files' must never be handed a path
outside the CURRENT enumerated set, whatever the reason it left
\(excluded, un-listed, or vanished) — `file-exists-p' was always the
wrong predicate for the vanished-from-CONFIG case, since a file removed
from the configured set still exists on disk and would re-scan and
resurrect its rows through the merge.  Hash-backed single pass (R53)."
  (let ((table (make-hash-table :test #'equal :size (length files))))
    (dolist (f files) (puthash f t table))
    (seq-filter (lambda (f) (gethash f table)) changed)))

(defun org-air-view--cache-load ()
  "Return (ITEMS . STALE-FILES) from the persisted cache, or nil.
ITEMS carry cons (FILE . POS) marker slots (hydrated on demand by
`org-air-view--item-pos').  STALE-FILES is the list of configured files
whose mtime diverged from the snapshot — new files and snapshot files now
missing included — nil when every mtime matches (FRESH: no scan at all).
Hydrates `org-air-view--items-mtimes' from the persisted `:mtimes' so the
FIRST post-cache `g r' is already mtime-incremental (R42-1)."
  (when-let* ((data (org-air-view--cache-read)))
    (let* ((files (org-air-query-files))
           (mtimes (plist-get data :mtimes)))
      (setq org-air-view--items-mtimes mtimes)
      ;; R54-2: rehydrate the per-file fact table so a warm board answers
      ;; file-level questions (F1 `title-from-org', the denote: shim's ID
      ;; index) with zero file opens.
      (org-air-query-file-meta-hydrate (plist-get data :file-meta))
      ;; R54-3: rehydrate the opt-in visit ledger alongside it.
      (org-air-query-visits-hydrate (plist-get data :visits))
      (cons (plist-get data :items)
            (org-air-view--changed-files files mtimes)))))

(defun org-air-view--refresh-queue-order (files &optional mtimes)
  "Return FILES ordered for the paced fill: inbox FIRST, then mtime DESC (R56 P1c).
Pure given its inputs.  `org-air-inbox-file' (when it is in FILES) always
takes position 1, so the very first budgeted slice scans the user's
captures and the first progressive paint is real, triageable content.
The remaining files sort by MTIMES — an alist FILE -> mtime, defaulting
to the `org-air-view--changed-files-mtimes' memo the enumeration diff
already paid — newest first: recently-edited files are where today's
tasks live, so Needs-attention/Upcoming fill early.  Ties and missing
mtimes keep enumeration order (stable sort, epoch-0 fallback) —
deterministic.  Never stats a file.  Out of scope (spec P1c): an inbox
file OUTSIDE the configured roots is not enumerated and therefore not in
FILES; enumeration semantics are unchanged this round."
  (let* ((mtimes (or mtimes org-air-view--changed-files-mtimes))
         (inbox (and org-air-inbox-file (expand-file-name org-air-inbox-file)))
         (table (make-hash-table :test #'equal :size (length mtimes)))
         (head nil)
         (rest nil))
    (dolist (e mtimes)
      (puthash (car e) (if (cdr e) (float-time (cdr e)) 0.0) table))
    (dolist (f files)
      (if (and inbox (null head) (equal f inbox))
          (setq head f)
        (push f rest)))
    (setq rest (sort (nreverse rest)
                     (lambda (a b)
                       (> (gethash a table 0.0) (gethash b table 0.0)))))
    (if head (cons head rest) rest)))

(defun org-air-view--refresh-repaint ()
  "Repaint the board from `org-air-view--items' as-is, preserving point.
Unlike `org-air-view--render-current' this never falls back to a
synchronous query when the items are nil (the cold/failed machine states
must not re-block the frame).
R58: with a bookmark locator armed the locator OWNS the landing (the
saved point belongs to the pre-restore skeleton), so the save/restore
pair is skipped — the render tail's `org-air-view--bookmark-consume'
places point instead."
  (let ((token (and (not org-air-view--bookmark-locator)
                    (org-air-view--save-position))))
    (org-air-view--render org-air-view--items org-air-view--tag-filter)
    (when token
      (org-air-view--restore-position token))
    ;; R73-1: the swap-tail resync — point is FINAL here (restored above,
    ;; or bookmark-consumed inside the render), so the pane/inspector are
    ;; nudged onto the item NOW at point.  Identity-limited: a repaint
    ;; that changed nothing (failure/cancel/mid-machine marker paint)
    ;; no-ops via the `eq' guards.
    (org-air-view--panes-resync-now)))

(defconst org-air-view--refresh-pace 0.05
  "Bounded idle interval (seconds) pacing the chunked refresh slices (R34-3).
An internal constant, never a defcustom.")

(defconst org-air-view--refresh-sync-budget 12
  "Changed-file count at/below which refresh scans SYNCHRONOUSLY (R42-2).
Warm per-file reparse is ~0.13ms, so up to this many changed files finish
well under one frame with no idle pacer and no marker churn; a larger
change set (a bulk edit, or the cold first load which routes through
`org-air-view' not the refresh) keeps the R34-3 repeating idle pacer over
the changed subset.  An internal constant, never a defcustom.")

(defconst org-air-view--refresh-watchdog-timeout 8.0
  "Seconds before a stranded paced scan is force-completed (R42-2).
A wall-clock backstop only — the R34-3 idle pacer resumes across idle
periods and normally finishes long before this — that guarantees
`org-air-view--refresh-state' can never persist at `refreshing'
indefinitely (the user's \"refreshing FOREVER\" failure).")

(defun org-air-view--refresh-next-delay (idle-elapsed interrupted-p)
  "Return the bounded delay before the next refresh slice (R34-3).
OBSOLETE (R56 P2a): the pacer is now the adaptive self-chaining
wall-clock chain of `org-air-view--refresh-next-gap' — kept as a
documented alias of the bounded constant `org-air-view--refresh-pace' so
R34-3's boundedness ERTs re-bless explicitly.  The R34-3 law it locked
\(the delay must NOT grow with the slice index; the old chain pushed an
ABSOLUTE idle target forward ~0.05s per slice, so any interaction
stranded it at \"loading N/M\" forever) carries over: the R56 gap is
bounded 0.01..0.6s both ways.  IDLE-ELAPSED and INTERRUPTED-P are
accepted for the model but never push the delay up."
  (ignore idle-elapsed interrupted-p)
  org-air-view--refresh-pace)

(defconst org-air-view--refresh-gap-fast 0.01
  "Chain gap after an UNINTERRUPTED slice (R56 P2a).
Near-continuous drain: an uninterrupted slice means Emacs is idle anyway,
so the fill runs at ~2/3 duty cycle (measured ~23ms slice + this gap) and
a cold 1801-file scan completes in ~6.5-8s — the old once-per-idle-period
idle pacer + 0.2s wall-clock fallback (9-12%% duty) spread the same 4.6s
of scan CPU over 48s-to-minutes.  An internal constant, never a
defcustom.")

(defconst org-air-view--refresh-gap-backoff 0.15
  "First chain gap after an input-aborted slice (R56 P2a).
Doubles per consecutive abort up to `org-air-view--refresh-gap-cap' so
sustained typing costs at most one aborted slice attempt per cap period
\(input latency stays bounded by one 18ms slice either way); the first
uninterrupted slice resets to `org-air-view--refresh-gap-fast'.")

(defconst org-air-view--refresh-gap-cap 0.6
  "Ceiling for the chain's abort-backoff gap (R56 P2a).
Bounded both ways: the chain can never slow past this, so convergence
under sustained typing is never worse than the old 0.2s-period fallback's
throughput floor — and never faster than input comfort allows.")

(defun org-air-view--refresh-next-gap (outcome &optional last-gap)
  "Return the adaptive wall-clock gap before the next chain slice (R56 P2a).
Pure.  OUTCOME is `aborted' when the last slice was cut by input; any
other value counts as uninterrupted.  Uninterrupted -> the fast
near-continuous `org-air-view--refresh-gap-fast'; aborted -> back off
from LAST-GAP (the previously armed gap, nil on the first slice):
`org-air-view--refresh-gap-backoff' doubling to the
`org-air-view--refresh-gap-cap' ceiling while aborts continue; the first
uninterrupted slice resets to fast.  Monotone under consecutive aborts
and bounded both ways, so the pacing can never strand.  One-shot
chaining is safe here where R34-3's chain was not: that bug was an IDLE
timer whose absolute idle target grew per slice; a wall-clock one-shot
has no idle target (and the \"repeating\" idle timer never delivered
idle pacing anyway — one fire per continuous idle period)."
  (if (eq outcome 'aborted)
      (min org-air-view--refresh-gap-cap
           (max org-air-view--refresh-gap-backoff
                (* 2 (or last-gap 0))))
    org-air-view--refresh-gap-fast))

(defun org-air-view--refresh-chain-live-p ()
  "Non-nil when the adaptive chain's one-shot is armed and PENDING (R56 P2a).
`timerp' alone is not liveness — a fired one-shot is still a timer
struct; membership in `timer-list' is what proves a next slice is
scheduled."
  (and (timerp org-air-view--refresh-timer)
       (memq org-air-view--refresh-timer timer-list)))

(defun org-air-view--refresh-chain-arm (buffer token outcome)
  "(Re-)arm the adaptive one-shot chain for BUFFER under TOKEN (R56 P2a).
Cancels any pending chain one-shot first so exactly ONE live pacing
timer exists, then schedules `org-air-view--refresh-run-slice' after the
`org-air-view--refresh-next-gap' gap for OUTCOME (recording the gap as
the next backoff base).  Never arms under `noninteractive' — the
deterministic ERTs drive the slice runner directly.  Must be called with
BUFFER current (all state is buffer-local)."
  (unless noninteractive
    (let ((gap (org-air-view--refresh-next-gap
                outcome org-air-view--refresh-gap)))
      (setq org-air-view--refresh-gap gap)
      (when (timerp org-air-view--refresh-timer)
        (cancel-timer org-air-view--refresh-timer))
      (setq org-air-view--refresh-timer
            (run-with-timer gap nil
                            #'org-air-view--refresh-run-slice
                            buffer token)))))

(defun org-air-view--refresh-watchdog-disarm ()
  "Cancel the one-shot refresh safety timer, if any (R42-2)."
  (when (timerp org-air-view--refresh-watchdog)
    (cancel-timer org-air-view--refresh-watchdog))
  (setq org-air-view--refresh-watchdog nil))

(defconst org-air-view--refresh-wallclock-pace 0.2
  "Repeating wall-clock pacer period for the OLD watchdog fallback (R53 P1c).
OBSOLETE (R56 P2a): the 18ms-budget-per-0.2s fallback was a 9-12% duty
cycle — 198 slices ≈ 48-60s at a measured 1801-file corpus, minutes with
interaction — superseded by the adaptive chain
\(`org-air-view--refresh-next-gap'), which the watchdog now re-arms
instead.  Kept only as documentation of the superseded cadence; unused.")

(defun org-air-view--refresh-watchdog-fire (buffer token)
  "Rescue a stranded paced refresh WITHOUT ever hanging (R42-2/R56 P2b).
A PURE BACKSTOP: with the R56 P2a pacer already wall-clock (progress
never depends on idleness), the watchdog's only remaining job is
machine-level convergence insurance — a lost timer, a wedged token.  If
BUFFER is still `refreshing' under TOKEN when it fires: a PROVABLY SMALL
remainder (<= `org-air-view--refresh-sync-budget' files) is drained
synchronously — well under a frame at the measured p95.  A LARGER queue
is NEVER drained synchronously (the old force-complete WAS the user's
minutes-long mid-session freeze at 5000 files — the R53 law stands): the
adaptive P2a chain is re-armed IF none is live, and the watchdog re-arms
behind it.  Token-guarded, so a superseded refresh's watchdog is a
silent no-op.  The state still can never strand at `refreshing' — it
converges by pacing, not by freezing."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (eq token org-air-view--refresh-token)
                 (eq org-air-view--refresh-state 'refreshing))
        (if (> (length org-air-view--refresh-queue)
               org-air-view--refresh-sync-budget)
            ;; Mirror `org-air-view--refresh-arm': never arm real timers
            ;; under `noninteractive' — the deterministic ERTs drive the
            ;; slice runner directly; the state stays `refreshing' and
            ;; converges by pacing either way.
            (unless noninteractive
              (unless (org-air-view--refresh-chain-live-p)
                (org-air-view--refresh-chain-arm buffer token 'completed))
              (org-air-view--refresh-watchdog-disarm)
              (setq org-air-view--refresh-watchdog
                    (run-with-timer org-air-view--refresh-watchdog-timeout
                                    nil
                                    #'org-air-view--refresh-watchdog-fire
                                    buffer token)))
          (org-air-view--refresh-disarm)
          (condition-case err
              (progn
                (let ((remaining org-air-view--refresh-queue))
                  (setq org-air-view--refresh-queue nil)
                  (when remaining
                    (dolist (f remaining)
                      (push (cons f (file-attribute-modification-time
                                     (file-attributes f)))
                            org-air-view--refresh-mtimes))
                    (setq org-air-view--refresh-acc
                          (nconc org-air-view--refresh-acc
                                 (copy-sequence
                                  (org-air-query-items-in-files
                                   remaining))))))
                (org-air-view--refresh-finish))
            (error
             (setq org-air-view--refresh-state 'failed
                   org-air-view--refresh-queue nil
                   org-air-view--refresh-acc nil
                   org-air-view--loading nil)
             (org-air-view--refresh-repaint)
             (message "org-air: refresh failed: %s (g r retries)"
                      (org-air-view--short-error err)))))))))

(defun org-air-view--refresh-disarm ()
  "Cancel the pacing + watchdog timers, if any (R34-3/R42-2).
Single teardown point for every timer the paced refresh arms."
  (when (timerp org-air-view--refresh-timer)
    (cancel-timer org-air-view--refresh-timer))
  (setq org-air-view--refresh-timer nil)
  (org-air-view--refresh-watchdog-disarm))

(defun org-air-view--refresh-arm (buffer token)
  "Arm the adaptive self-chaining wall-clock pacer for BUFFER, TOKEN (R56 P2a).
ONE one-shot `run-with-timer' whose callback — the budgeted slice runner
itself — runs one `org-air-refresh-slice-budget' slice (unchanged;
still `while-no-input', still C-g-abortable) and re-arms itself with the
adaptive `org-air-view--refresh-next-gap' gap: near-continuous (0.01s)
while uninterrupted, backing off 0.15..0.6s under input.  Wall-clock,
not idle: the old \"repeating\" idle pacer fired ONCE per continuous
idle period ((elisp) Idle Timers; R56 pty-probed 1 fire in 3s of
idleness), so a user who opened the board and WAITED got one slice per
watchdog period — the measured minutes-long fill.  Re-arm discipline:
the callback re-arms ONLY under its own live token+state and only while
a chain handle exists; `org-air-view--refresh-disarm' (from finish /
failure / cancel) is the single teardown, so exactly one live pacing
timer exists at a time.  The R42-2 watchdog arms behind it as a pure
backstop (P2b).  Never arms under `noninteractive' — the deterministic
ERTs call `org-air-view--refresh-run-slice' directly instead."
  (unless noninteractive
    (with-current-buffer buffer
      (unless (org-air-view--refresh-chain-live-p)
        (setq org-air-view--refresh-gap nil)
        (org-air-view--refresh-chain-arm buffer token 'completed)
        ;; R42-2: a wall-clock backstop behind the chain, so `refreshing'
        ;; can never persist indefinitely even if the chain is lost.
        (org-air-view--refresh-watchdog-disarm)
        (setq org-air-view--refresh-watchdog
              (run-with-timer
               org-air-view--refresh-watchdog-timeout nil
               #'org-air-view--refresh-watchdog-fire buffer token))))))

(defun org-air-view--refresh-cancel ()
  "Invalidate any in-flight refresh: bump the token, cancel the timer.
Every pending slice callback carries the old token and self-cancels."
  (cl-incf org-air-view--refresh-token)
  (org-air-view--refresh-disarm)
  (setq org-air-view--refresh-queue nil
        org-air-view--refresh-acc nil
        org-air-view--refresh-abort-file nil
        org-air-view--refresh-progressive nil
        org-air-view--refresh-gap nil
        org-air-view--refresh-scan-started nil
        org-air-view--refresh-paint-items 0
        org-air-view--refresh-render-secs nil))

(defun org-air-view--refresh-teardown ()
  "Cancel the in-flight refresh outright (the board buffer is dying)."
  (org-air-view--refresh-cancel)
  (org-air-view--deferred-disarm)
  (setq org-air-view--refresh-state nil)
  ;; R53 P1: the session's scan work buffer goes with the board (it is
  ;; recreated on demand, so an early teardown only costs one re-init).
  (org-air-query-teardown))

(defun org-air-view--deferred-disarm ()
  "Cancel the pending one-shot cache-first first-paint timer, if any (R45-2).
Called on every interactive (re)entry and on teardown so a stale one-shot
never double-renders and a killed buffer's timer never strands."
  (when (timerp org-air-view--deferred-timer)
    (cancel-timer org-air-view--deferred-timer))
  (setq org-air-view--deferred-timer nil))

(defun org-air-view--deferred-arm (buffer token &optional stale)
  "Arm the one-shot idle first-paint for BUFFER under TOKEN (R45-2/R56 P1a).
Scheduled by BOTH cache-hit branches AFTER the pill-free skeleton is
painted, so the cached full board (and its cold SVG pill rasterization)
lands OFF the launch critical path.  With STALE non-nil the one-shot is
`org-air-view--deferred-stale-paint' — it paints the FULL cached board
and then OWNS the paced changed-subset kickoff (R56 P1a: a command-body
`org-air-view--refresh-start' would bump the token via
`--refresh-cancel' and orphan this very one-shot).  Never arms under
`noninteractive' \(the byte gate stays synchronous with no timer); the
deterministic ERTs call the one-shot functions directly instead."
  (unless noninteractive
    (with-current-buffer buffer
      (org-air-view--deferred-disarm)
      (setq org-air-view--deferred-timer
            (run-with-idle-timer
             0 nil
             (if stale #'org-air-view--deferred-stale-paint
               #'org-air-view--deferred-first-paint)
             buffer token)))))

(defun org-air-view--deferred-first-paint (buffer token)
  "Render the cached full board off the launch critical path (R45-2).
The one-shot the cache-HIT FRESH branch schedules after its skeleton
paint.  Renders the cached items AS-IS — org-ql is NOT called, so the
cache's no-scan benefit is fully preserved.  Robustness mirrors the slice
runner: a stale TOKEN (a re-open / refresh bumped it) or a dead BUFFER is
a silent no-op, so the buffer can never wedge; any render error falls back
to the single-message + empty-board discipline."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq org-air-view--deferred-timer nil)
      (when (eq token org-air-view--refresh-token)
        (condition-case err
            (org-air-view--render org-air-view--items
                                  org-air-view--tag-filter)
          (error
           (setq org-air-view--items nil
                 org-air-view--classify-cache nil
                 org-air-view--loading nil)
           (org-air-view--render nil org-air-view--tag-filter)
           (message "org-air: load failed: %s"
                    (org-air-view--short-error err))))))))

(defun org-air-view--deferred-stale-paint (buffer token)
  "Paint the FULL cached board, then kick the paced rescan (R56 P1a).
The paint LAW made concrete for the cache-STALE open: the board never
shows chrome in place of content it already holds — the full last-known
board sat IN MEMORY while the old skeleton-until-finish arm made the
user stare at \"Loading your board…\" for the whole paced fill (R56
measured: ZERO progressive paints; first content = the finish paint,
48s-to-minutes at 1801 files).  This one-shot (the STALE arm's R45-2
deferred tick) renders the cached items AS-IS — no org-ql call — then
OWNS the `org-air-view--refresh-start' kickoff: paint, then start (a
command-body start would bump the token and orphan this callback; the
started refresh's own token supersedes cleanly).  The banner then ticks
to the live `scanning N/M' segment immediately.  Everything painted
stays interactive; triage on a stale file stays guarded by
`org-air-view--refresh-stale-item-guard'; the finish swap replaces
content in one motion as today.  Robustness mirrors
`org-air-view--deferred-first-paint': a stale TOKEN or dead BUFFER is a
silent no-op; a render error falls back to the single-message +
empty-board discipline."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq org-air-view--deferred-timer nil)
      (when (eq token org-air-view--refresh-token)
        (condition-case err
            (progn
              (org-air-view--render org-air-view--items
                                    org-air-view--tag-filter)
              (org-air-view--refresh-start t)
              ;; surface the `⟳ scanning N/M…' segment without waiting
              ;; for the first slice (tick time is nil = immediate).
              (org-air-view--refresh-banner-tick))
          (error
           (setq org-air-view--items nil
                 org-air-view--classify-cache nil
                 org-air-view--loading nil)
           (org-air-view--render nil org-air-view--tag-filter)
           (message "org-air: load failed: %s"
                    (org-air-view--short-error err))))))))

(defun org-air-view--refresh-start (&optional cold)
  "Enter (or short-circuit) a refresh for the current board; return the token.
With COLD non-nil (the initial skeleton load, where the board is NOT yet
painted) the sync fast path is bypassed so the async skeleton machine
paints instantly and keeps input live; only a WARM refresh (already
painted) takes the sync path.
Mtime-incremental (R42-2): cancels any in-flight refresh (its slices go
stale via the token), stats the CURRENT file list and diffs it against the
`org-air-view--items-mtimes' baseline via `org-air-view--changed-files'.

- No file changed => single-swap the same items, clear the marker, refresh
  the baseline, DONE.  No org-ql call, no slice, no pacer (the board is
  already painted; this is the initial-load FRESH behaviour, now on the
  refresh path too).

- A small changed set (<= `org-air-view--refresh-sync-budget') is scanned
  SYNCHRONOUSLY in one `org-air-query-items-in-files' call, merged with
  the retained items and single-swapped in — no timer, no marker churn,
  well under one frame for the common save-a-file case.  This path never
  enters `refreshing', so it can never strand.

- A larger changed set (bulk edit) SEEDS the accumulator with the retained
  unchanged items (reused verbatim — their markers are valid by mtime
  match) and queues ONLY the changed files — inbox first, then mtime
  descending (R56 P1c, `org-air-view--refresh-queue-order') — paced by
  the R56 P2a adaptive chain (with the R42-2 watchdog backstop); on
  finish the accumulator = retained ∪ rescanned = the full set (ordering
  is irrelevant, the R20-6 partition regroups/sorts at render).

R56 P1 — the paint LAW: at no point may the board display an empty
skeleton while item content is available in-buffer — cached items from a
previous session (the cache-stale open paints them via
`org-air-view--deferred-stale-paint' before this machine runs), or
accumulated items from the running scan (a fill with NO previous content
enters progressive STREAM mode here and paints repeatedly as slices
land).

R60-3 key guard (the R57 discipline — the key IS the detector): when
the live `org-air-view--cache-key' no longer matches the key the
retained items were derived under (the exclude set / file set narrowed
mid-session, a vocabulary or routed-knob change), the session is COLD
for data purposes — the retained merge inputs are dropped (nil mtime
baseline, so every current file is \"changed\"; nothing is retained
into the merge) and the full machine runs: paced when large,
sync-budget when small.  Without the guard the old baseline names the
removed files \"vanished\", the sync path finds them still ON DISK and
resurrects their rows through the incremental merge.  The painted
board stays up until the swap (R56); the swap stamps the fresh key
exactly where the machine already does.

The caller paints (board or skeleton) after this so the header
marker/progress is visible from the first paint."
  (org-air-view--refresh-cancel)
  (let* ((stale-key (not (equal org-air-view--items-key
                                (org-air-view--cache-key))))
         (files (progn
                  ;; R60-3: a stale key voids the merge baseline — the
                  ;; retained mtimes were recorded under another config.
                  (when stale-key
                    (setq org-air-view--items-mtimes nil))
                  (org-air-query-files)))
         (changed (org-air-view--changed-files
                   files org-air-view--items-mtimes)))
    (cond
     ;; No-change short-circuit: prove "0 files to reparse" and stop.  The
     ;; board already shows exactly these items, so no org-ql call, no
     ;; slice, no pacer.  F2: REPAINT UNCONDITIONALLY — the day-keyed
     ;; classify cache (`--classify-cache-day') only invalidates ON RENDER
     ;; and there is no midnight timer, so skipping the repaint would strand
     ;; yesterday's overdue/today/upcoming bucketing on screen after a day
     ;; rolls (and would break the R18 D-P1c "next refresh picks it up"
     ;; promise for a retuned classify defcustom).  Render is ~28ms.  B1:
     ;; keep the existing mtime baseline VERBATIM (do NOT re-stat) — it just
     ;; proved itself (every current file matched, none vanished); a re-stat
     ;; would only risk masking a file changed post-diff/pre-restat.
     ((and (null changed) (not stale-key))
      (setq org-air-view--refresh-acc nil
            org-air-view--refresh-queue nil
            org-air-view--refresh-total 0
            org-air-view--refresh-mtimes nil
            org-air-view--refresh-state nil
            org-air-view--cache-stale-files nil
            org-air-view--loading nil)
      (org-air-view--refresh-repaint))
     ;; Sync fast path: few changed files -> one query, merge, single-swap,
     ;; DONE.  Never enters `refreshing' (so it cannot strand); no pacer.
     ;; Skipped on the COLD load (board not yet painted -> keep the paced
     ;; skeleton machine).
     ((and (not cold)
           (<= (length changed) org-air-view--refresh-sync-budget))
      ;; F3: the scan is the one sync site that used to run UNPROTECTED — a
      ;; signal here (with the board `refreshing'+`loading' on a small cold
      ;; load whose all-changed set fits the budget) would leave state
      ;; `refreshing' with no timer and no watchdog: the exact strand this
      ;; round exists to kill.  Mirror the slice handler: error -> `failed'
      ;; + repaint.
      (condition-case err
          (let* ((_ (org-air-query-skip-log-reset))
                 ;; R60-3 defence-in-depth: intersect with the CURRENT
                 ;; enumerated set, never `file-exists-p' — an excluded/
                 ;; un-listed file still exists on disk.
                 (existing (org-air-view--files-intersect changed files))
                 ;; R60-3: under a stale key nothing is retained — the
                 ;; old items were derived under another config.
                 (retained (and (not stale-key)
                                (seq-remove
                                 (lambda (it)
                                   (member (org-air-item-file it) changed))
                                 org-air-view--items)))
                 ;; B1: stat the FULL set ONCE *before* the scan reads any
                 ;; file, so no file is stat'd AFTER it is read; a file
                 ;; changed post-stat/pre-read then diverges next refresh
                 ;; and re-scans (errors converge) instead of masking.
                 (snapshot (org-air-view--mtimes-snapshot files))
                 (fresh (copy-sequence
                         (org-air-query-items-in-files existing)))
                 (merged (nconc retained fresh)))
            (setq org-air-view--items merged
                  org-air-view--items-key (org-air-view--cache-key)
                  org-air-view--items-mtimes snapshot
                  org-air-view--classify-cache nil
                  org-air-view--refresh-acc nil
                  org-air-view--refresh-queue nil
                  org-air-view--refresh-total 0
                  org-air-view--refresh-mtimes nil
                  org-air-view--refresh-state nil
                  org-air-view--cache-stale-files nil
                  org-air-view--loading nil)
            (org-air-view--refresh-repaint)
            (org-air-view--cache-write merged snapshot))
        (error
         (setq org-air-view--refresh-state 'failed
               org-air-view--refresh-queue nil
               org-air-view--refresh-acc nil
               org-air-view--loading nil)
         (org-air-view--refresh-repaint)
         (message "org-air: refresh failed: %s (g r retries)"
                  (org-air-view--short-error err)))))
     ;; Bulk/cold: retain unchanged items, PACE only the changed subset.
     (t
      (let ((existing (org-air-view--files-intersect changed files))
            ;; R60-3: stale key => retain nothing (see the sync path).
            (retained (and (not stale-key)
                           (seq-remove
                            (lambda (it)
                              (member (org-air-item-file it) changed))
                            org-air-view--items))))
        (org-air-query-skip-log-reset)
        ;; R56 P1c: inbox first, then recency — the first budgeted slice
        ;; scans the inbox, so the first progressive paint is real,
        ;; triageable content (cold time-to-inbox was unbounded before:
        ;; plain directory-walk order).
        (setq org-air-view--refresh-queue
              (org-air-view--refresh-queue-order existing)
              org-air-view--refresh-total (length existing)
              org-air-view--refresh-acc (copy-sequence retained)
              org-air-view--refresh-mtimes nil
              ;; R56 P1b: STREAM mode only when there is NO previous full
              ;; content to show (true cold open) — a painted or
              ;; cache-seeded board keeps the R26-8 single-swap rule.  A
              ;; nil last-paint lets the first item-bearing slice paint
              ;; IMMEDIATELY (P1c), then the interval cadence takes over.
              org-air-view--refresh-progressive (null org-air-view--items)
              org-air-view--refresh-last-paint
              (and org-air-view--items (float-time))
              org-air-view--refresh-paint-items (length retained)
              org-air-view--refresh-banner-tick-time nil
              org-air-view--cache-stale-files changed
              org-air-view--refresh-state 'refreshing)
        ;; Every changed file may have vanished (existing empty) — finish
        ;; immediately so the retained set (minus the dropped rows) swaps in
        ;; and the state can never stick at `refreshing'.
        (if (null org-air-view--refresh-queue)
            (org-air-view--refresh-finish)
          (org-air-view--refresh-arm (current-buffer)
                                     org-air-view--refresh-token))))))
  org-air-view--refresh-token)

(defun org-air-view--refresh-finish ()
  "All slices done: swap ONCE, re-render, clear the marker, write the cache.
The single-swap rule (no partial paints): the accumulated items replace
`org-air-view--items' in one motion, the board repaints once with point
preserved, and the machine returns to FRESH.  Disarms the repeating pacer
first so it cannot fire again after the chain is DONE (R34-3)."
  (org-air-view--refresh-disarm)
  ;; R42-1 (B1): the merged accumulator (retained unchanged items ∪
  ;; rescanned changed items) is the full set; its mtime baseline is built
  ;; ENTIRELY from SCAN-TIME data — never re-stat'd at finish.  For each
  ;; current file take the scan-time mtime captured by the slices
  ;; (`org-air-view--refresh-mtimes', one entry per file actually read),
  ;; else — for a retained (unchanged, un-scanned) file — the existing
  ;; baseline entry.  Re-stat'ing here would stamp a FRESH mtime over items
  ;; read from an OLDER revision (a file written during the paced-scan
  ;; window — an external git-pull/sync write to a retained file, or a
  ;; changed file re-touched after its slice) so the next refresh's
  ;; no-change short-circuit would call it FRESH and mask the staleness
  ;; FOREVER.  Building from scan-time data instead leaves such a file with
  ;; its pre-write mtime, so the NEXT `--changed-files' names it and it
  ;; re-scans — errors converge instead of sticking.  (This is the seam the
  ;; B1 revert-fails ERT drives: external write to a retained file
  ;; mid-paced-scan => next `--changed-files' must name it.)
  (let* ((items org-air-view--refresh-acc)
         (changed org-air-view--cache-stale-files)
         (scan-time org-air-view--refresh-mtimes)
         (scanned (length scan-time))
         (skipped (length org-air-query--skip-log))
         (mtimes (delq nil
                       (mapcar
                        (lambda (f)
                          (or (assoc f scan-time)
                              (and (not (member f changed))
                                   (assoc f org-air-view--items-mtimes))))
                        (org-air-query-files)))))
    (setq org-air-view--items items
          org-air-view--items-key (org-air-view--cache-key)
          org-air-view--items-mtimes mtimes
          org-air-view--classify-cache nil
          org-air-view--refresh-state nil
          org-air-view--refresh-acc nil
          org-air-view--refresh-queue nil
          org-air-view--refresh-total 0
          org-air-view--refresh-mtimes nil
          org-air-view--refresh-progressive nil
          org-air-view--refresh-paint-items 0
          org-air-view--refresh-render-secs nil
          org-air-view--cache-stale-files nil
          org-air-view--loading nil)
    ;; R56 P3c: this repaint runs with the machine already idle, so the
    ;; `⟳ scanning N/M…' segment vanishes in the same motion the final
    ;; content lands — a crisp clear, no lingering "done!" chrome.
    (org-air-view--refresh-repaint)
    ;; R53 P1b: ONE summary line per completed scan — never per-file echo
    ;; spam; the details live behind M-x org-air-scan-report.
    (unless (or noninteractive (zerop scanned))
      (message "org-air: scanned %d file%s (%d item%s)%s"
               scanned (if (= scanned 1) "" "s")
               (length items) (if (= (length items) 1) "" "s")
               (if (> skipped 0)
                   (format ", skipped %d — M-x org-air-scan-report" skipped)
                 "")))
    (org-air-view--cache-write items mtimes)))

(defun org-air-view--refresh-note-abort ()
  "Track input-aborts of the queue-head file; skip it after N (R53 P1c).
`while-no-input' aborted before the head file's items were committed; a
file that aborts `org-air-scan-abort-retries' consecutive slices is
skip-logged `slow' and dropped WITHOUT entering the scan-time mtimes, so
the next refresh's diff names it again (errors converge) while the board
stays usable now — no livelock.
R56 P2c: counts ONLY aborts that landed while the head file's scan had
actually STARTED (`org-air-view--refresh-scan-started', raised right
before the query call, cleared on commit).  An abort BEFORE the head
started — input landed between files, or before the first — increments
nothing: key-repeat can no longer silently skip-drop innocent queue
heads as `slow' (measured: one dropped file per 0.6s of held-down arrow
key at the old accounting).  A file that genuinely aborts mid-scan N
times still skips — the anti-livelock stays."
  (let ((head (car org-air-view--refresh-queue)))
    (when (and head org-air-view--refresh-scan-started)
      (if (equal head (car-safe org-air-view--refresh-abort-file))
          (setcdr org-air-view--refresh-abort-file
                  (1+ (cdr org-air-view--refresh-abort-file)))
        (setq org-air-view--refresh-abort-file (cons head 1)))
      (when (>= (cdr org-air-view--refresh-abort-file)
                (max 1 org-air-scan-abort-retries))
        (org-air-query--skip head 'slow)
        (setq org-air-view--refresh-queue (cdr org-air-view--refresh-queue)
              org-air-view--refresh-abort-file nil)))))

(defun org-air-view--refresh-paint-interval ()
  "Effective seconds between progressive stream paints (R56 P1b).
`org-air-cold-paint-interval' floored by 3x the last progressive render
cost (`org-air-view--refresh-render-secs'), so paint overhead can never
exceed ~1/3 of the fill even when the accumulated board grows expensive
to re-render."
  (max org-air-cold-paint-interval
       (* 3 (or org-air-view--refresh-render-secs 0))))

(defun org-air-view--refresh-progressive-paint ()
  "Repaint the still-filling COLD board from the accumulator (R53 P1c/R56 P1b).
The STREAM paint: swaps a COPY of the accumulator in, invalidates the
classify cache and repaints with point preserved.  The FIRST progressive
paint drops the `org-air-view--loading' guard — the verbs go live over
real rows (items in still-queued files stay guarded by
`org-air-view--refresh-stale-item-guard') — and the stream then
CONTINUES on the `org-air-view--refresh-paint-interval' cadence until
`org-air-view--refresh-finish''s single final swap ends stream mode.
R56 P1b decoupled the stream gate (`org-air-view--refresh-progressive',
set at refresh start) from the loading flag this paint clears — the R53
reuse made the throttle a one-shot: measured exactly ONE progressive
paint per cold fill, frozen at ~1% content until the finish.  Warm
incremental refreshes never take this path, so the R26-8 single-swap
rule holds there.  Records its own render cost so paint overhead stays
bounded."
  (setq org-air-view--refresh-last-paint (float-time)
        org-air-view--refresh-paint-items
        (length org-air-view--refresh-acc)
        org-air-view--items (copy-sequence org-air-view--refresh-acc)
        org-air-view--classify-cache nil
        org-air-view--loading nil)
  (let ((start (float-time)))
    (org-air-view--refresh-repaint)
    (setq org-air-view--refresh-render-secs (- (float-time) start))))

(defconst org-air-view--refresh-banner-tick-interval 0.5
  "Seconds between in-place banner progress rewrites while refreshing (R56 P3b).
Progressive paints are ~1s apart and single-swap refreshes paint only at
the end, so the slice runner ticks the BANNER LINE alone at this bound —
the `⟳ scanning N/M…' numbers stay live between full paints.  An
internal constant, never a defcustom.")

(defun org-air-view--refresh-banner-tick ()
  "Rewrite ONLY the banner line in place with fresh progress numbers (R56 P3b).
Bounded: at most once per `org-air-view--refresh-banner-tick-interval',
one line-1 rewrite (microseconds), no body touch, point preserved by
`save-excursion'; only while the machine is `refreshing'; never in
batch.  Works over the skeleton and the painted board alike — both
compose the banner as buffer line 1."
  (when (and (not noninteractive)
             (eq org-air-view--refresh-state 'refreshing)
             (>= (- (float-time)
                    (or org-air-view--refresh-banner-tick-time 0))
                 org-air-view--refresh-banner-tick-interval)
             (> (buffer-size) 0))
    (setq org-air-view--refresh-banner-tick-time (float-time))
    (let* ((width (or org-air-view--rendered-width
                      (org-air-view--render-width)))
           (line (car (org-air-view--render-lines
                       width
                       (lambda ()
                         (org-air-view--insert-banner
                          org-air-view--items)))))
           (inhibit-read-only t))
      (when line
        ;; Mirror the top-level line normalization: fixed-width configs
        ;; keep the pad (already applied by `--render-lines'); live mode
        ;; right-trims (`--finalize-buffer-lines' contract).
        (unless (integerp org-air-view-width)
          (setq line (string-trim-right line)))
        (save-excursion
          (goto-char (point-min))
          (delete-region (point) (line-end-position))
          (insert line))))))

(defun org-air-view--refresh-run-slice (buffer token)
  "Scan ONE budgeted slice of BUFFER's refresh queue under TOKEN (R53 P1c).
The named slice runner the pacing timers schedule — ERTs call it directly
in a loop, so the whole machine is testable synchronously with zero
timers.  The slice consumes queued files until
`org-air-refresh-slice-budget' is exceeded (minimum 1 file — the R26-8
fixed `org-air-refresh-files-per-slice' count is superseded), each file
through the never-signalling data layer.  The loop runs under
`while-no-input': pending input aborts BETWEEN files and the unconsumed
remainder (including the in-progress file — per-file work-buffer state
makes retry free) stays queued for the next tick;
`org-air-view--refresh-note-abort' skip-logs a file that keeps aborting
mid-scan (R56 P2c: aborts landing BETWEEN files count against nobody).
A `quit' propagates untouched: queue intact, machine still
`refreshing', pacer re-fires.  A COLD load streams REPEATED progressive
paints (interactive only; R56 P1b) — the first item-bearing slice paints
immediately (P1c: the inbox, by queue order), then on the bounded
`org-air-view--refresh-paint-interval' cadence whenever new items
accumulated — while warm refreshes accumulate privately and swap ONCE.
A machine-level error keeps the painted board, flips to FAILED (header:
`refresh failed (g r retries)') and always clears
`org-air-view--loading' so the buffer can never wedge."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (eq token org-air-view--refresh-token)
                 (eq org-air-view--refresh-state 'refreshing))
        ;; R56 P2a: the slice re-arms its own adaptive one-shot chain at
        ;; the bottom (only under a live token+state, and only while a
        ;; chain handle exists — direct ERT drives never spawn timers).
        ;; `refresh-finish'/failure/cancel disarm.
        (condition-case err
            (let* ((budget org-air-refresh-slice-budget)
                   (start (float-time))
                   (scanned 0)
                   (interrupted
                    (while-no-input
                      (while (and org-air-view--refresh-queue
                                  (or (zerop scanned)
                                      (< (- (float-time) start) budget)))
                        (let* ((f (car org-air-view--refresh-queue))
                               ;; mtime captured per file AT SCAN TIME
                               ;; (the cache snapshot), BEFORE the read.
                               (mtime (file-attribute-modification-time
                                       (file-attributes f)))
                               ;; copy: org-ql may hand back a CACHED list
                               ;; object — nconc'ing it would mutate the
                               ;; cache.  One-file calls keep the R26-8
                               ;; query seam (and its ERT stubs) intact.
                               ;; R56 P2c: raise the started flag ONLY
                               ;; once the head file's scan is actually
                               ;; entered; cleared on the commit below.
                               (items (progn
                                        (setq org-air-view--refresh-scan-started t)
                                        (copy-sequence
                                         (org-air-query-items-in-files
                                          (list f))))))
                          ;; commit ATOMICALLY per file — an abort mid-scan
                          ;; leaves the file at the queue head, retry free.
                          (push (cons f mtime) org-air-view--refresh-mtimes)
                          (setq org-air-view--refresh-acc
                                (nconc org-air-view--refresh-acc items)
                                org-air-view--refresh-queue
                                (cdr org-air-view--refresh-queue)
                                scanned (1+ scanned)
                                org-air-view--refresh-scan-started nil)))
                      nil)))
              (when (eq interrupted t)
                (org-air-view--refresh-note-abort))
              (setq org-air-view--refresh-scan-started nil)
              ;; Done -> finish (which disarms the chain and swaps once).
              (if (null org-air-view--refresh-queue)
                  (org-air-view--refresh-finish)
                ;; R56 P1b/P1c: the progressive STREAM — gated on stream
                ;; mode (NOT the self-clearing `--loading' flag), at least
                ;; one NEW item since the last paint, and either no paint
                ;; yet (immediate first content) or the bounded cadence.
                (when (and (not noninteractive)
                           org-air-view--refresh-progressive
                           (> (length org-air-view--refresh-acc)
                              org-air-view--refresh-paint-items)
                           (or (null org-air-view--refresh-last-paint)
                               (>= (- (float-time)
                                      org-air-view--refresh-last-paint)
                                   (org-air-view--refresh-paint-interval))))
                  (org-air-view--refresh-progressive-paint))
                ;; R56 P3b: live numbers between paints (banner line only).
                (org-air-view--refresh-banner-tick)
                ;; R56 P2a: re-arm the adaptive chain — fast after a clean
                ;; slice, backing off while input keeps aborting.
                (when (and (not noninteractive)
                           (timerp org-air-view--refresh-timer))
                  (org-air-view--refresh-chain-arm
                   buffer token
                   (if (eq interrupted t) 'aborted 'completed)))))
          (error
           (org-air-view--refresh-disarm)   ; stop the pacer on failure
           (setq org-air-view--refresh-state 'failed
                 org-air-view--refresh-queue nil
                 org-air-view--refresh-acc nil
                 org-air-view--loading nil)
           (org-air-view--refresh-repaint)   ; same board + honest header
           (message "org-air: refresh failed: %s (g r retries)"
                    (org-air-view--short-error err))))))))

(defun org-air-view--refresh-stale-item-guard (item)
  "Soft-error on a triage verb for ITEM while its file is mid-refresh (R26-8).
Only an item whose source file's mtime diverged from the cache snapshot is
blocked (its cached position may be wrong); positions in unchanged files
are valid by construction (mtime match), so triage there stays live.
R53 P1c: after a progressive cold paint the painted rows come from the
scan accumulator — a file ALREADY scanned this refresh carries fresh
positions, so only still-queued files stay guarded."
  (when (and (eq org-air-view--refresh-state 'refreshing)
             (member (org-air-item-file item) org-air-view--cache-stale-files)
             (not (and org-air-view--refresh-progressive
                       (assoc (org-air-item-file item)
                              org-air-view--refresh-mtimes))))
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
buffer can never wedge in a loading state.
R58: the three-branch data body lives in `org-air-view--open-core' (the
bookmark handlers re-enter it with display suppressed); this command is
prep + display + the core — byte-identical behaviour, same order
\(display still precedes the cond so width derivation sees the window)."
  (interactive)
  (let ((buffer (get-buffer-create org-air-view-buffer-name)))
    (with-current-buffer buffer
      ;; R26-5: IDEMPOTENT entry — re-running the mode on a live buffer
      ;; runs `kill-all-local-variables' and wipes the whole session (rail
      ;; placement, sort, filter...).  Initialise only when not already in
      ;; the mode (the same guard as the project entry; one discipline).
      (unless (derived-mode-p 'org-air-view-mode)
        (org-air-view-mode))
      ;; Every command entry revalidates ownership.  This recovers from a
      ;; renamed/former owner even when BUFFER was already mode-initialized.
      (org-air-view--source-tracking-claim))
    ;; Display the buffer first so width derivation measures the window
    ;; that actually shows the dashboard (U1), in a full-width window so
    ;; the rail/calendar are never pushed off-screen (D4).
    (pop-to-buffer buffer
                   (or org-air-display-action
                       '((display-buffer-reuse-window
                          display-buffer-same-window
                          display-buffer-full-frame))))
    (org-air-view--open-core buffer t)))

(defun org-air-view--open-core (buffer display)
  "Run the board's three-branch data entry for BUFFER (R58 factoring).
The EXACT cond `org-air-view' always ran — WARM/batch synchronous render
→ persisted-cache skeleton + deferred one-shot (+ paced stale kickoff) →
COLD skeleton + the R56 paced machine.  DISPLAY non-nil is the command
path (BUFFER was just displayed; the skeleton `redisplay' flushes the
visible frame); nil is the bookmark-handler path, which skips ONLY those
skeleton-flush `redisplay' calls (pointless and mildly wasteful
undisplayed) — branch choice, token discipline and every render are
identical.  Ensures the mode idempotently (R26-5) and never displays
BUFFER itself."
  (with-current-buffer buffer
    (unless (derived-mode-p 'org-air-view-mode)
      (org-air-view-mode))
    ;; Bookmark and other core-only entries obey the same owner lifecycle as
    ;; the interactive command, including already-in-mode recovery.
    (org-air-view--source-tracking-claim)
    (let ((cached (and org-air-view--items
                       (equal org-air-view--items-key
                              (org-air-view--cache-key)))))
      ;; R45-2: a (re)entry supersedes any pending one-shot first paint —
      ;; the buffer is about to be (re)painted synchronously
      ;; (WARM/cache-hit) or via a fresh skeleton (cold/stale), so a stale
      ;; one-shot must never fire and double-render.  Interactive only: the
      ;; batch gate never arms the timer, so there is nothing to disarm.
      (unless noninteractive
        (org-air-view--deferred-disarm))
      (cond
       ;; Cache hit, or batch/noninteractive (the byte goldens never see the
       ;; fast-paint path): synchronous — unchanged behaviour, byte-stable.
       ((or cached noninteractive)
        (unless cached
          ;; R18 D-P1c: fresh structs from a re-query invalidate the
          ;; classify cache (old `eq' entries can never be wrongly hit, but
          ;; drop them).  R42-1: snapshot the queried files' mtimes as the
          ;; incremental baseline (a display-invisible local; no golden
          ;; reads it, so the byte fixtures are untouched).
          (setq org-air-view--items (org-air-query-items)
                org-air-view--classify-cache nil
                org-air-view--items-mtimes
                (org-air-view--mtimes-snapshot (org-air-query-files))))
        (org-air-view--render org-air-view--items org-air-view--tag-filter))
       ;; R26-8 CACHED: a valid persisted cache paints the FULL last-known
       ;; board instantly.  All mtimes match -> FRESH, no scan at all; any
       ;; divergence -> REFRESHING (chunked slices; `stale · refreshing…'
       ;; marker in the count slot from this very first paint).  Nothing
       ;; modal remains on this path (`--loading' stays nil).
       ((when-let* ((cache (org-air-view--cache-load)))
          (setq org-air-view--items (car cache)
                org-air-view--items-key (org-air-view--cache-key)
                org-air-view--classify-cache nil
                org-air-view--cache-stale-files (cdr cache))
          ;; R45-2: paint the pill-free chrome skeleton FIRST (instant, like
          ;; the cold path), then let the full pill-bearing board land OFF
          ;; the launch critical path — never a blocking full paint in the
          ;; command body (a fresh Emacs has a cold SVG image cache, so that
          ;; first full paint rasterizes the viewport's pills synchronously =
          ;; the 2-10s blank-frozen open).  The board still appears in ONE
          ;; motion, merely deferred one idle tick behind the skeleton.
          (org-air-view--render-loading)
          ;; R58: the `redisplay' exists to flush the VISIBLE skeleton —
          ;; skipped on the undisplayed bookmark-handler path.
          (when display (redisplay t))
          ;; Bump the token first (via cancel) so any earlier in-flight
          ;; callback self-cancels before the one-shot below is armed.
          (org-air-view--refresh-cancel)
          (if (cdr cache)
              ;; STALE: R56 P1a (the paint LAW — the board never shows
              ;; chrome in place of content it already holds).  The
              ;; deferred one-shot paints the FULL cached board one idle
              ;; tick behind the skeleton (R26-8's warm-open contract
              ;; restored; pill rasterization stays off the command body,
              ;; the R45-2 rationale is about WHICH tick paints, never
              ;; WHETHER) and then OWNS the paced changed-subset kickoff:
              ;; calling `--refresh-start' here would bump the token again
              ;; and orphan the very one-shot it precedes.  See
              ;; `org-air-view--deferred-stale-paint'.
              (org-air-view--deferred-arm (current-buffer)
                                          org-air-view--refresh-token 'stale)
            ;; FRESH (no stale files): no scan at all.  Defer the cached
            ;; full-board render to a token-guarded one-shot idle callback
            ;; SEEDED with the cached items — NO org-ql call, the cache's
            ;; no-scan benefit is fully preserved.
            (org-air-view--deferred-arm (current-buffer)
                                        org-air-view--refresh-token))
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
              (org-air-view--refresh-start t) ; COLD: paced skeleton machine
              (org-air-view--render-loading)
              ;; R58: skeleton flush — visible (command) path only.
              (when display (redisplay t)))
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
      ;; defcustom on the next refresh).  R42-1: refresh the incremental
      ;; mtime baseline too (display-invisible local; no golden reads it).
      (setq org-air-view--items (org-air-query-items)
            org-air-view--classify-cache nil
            org-air-view--items-mtimes
            (org-air-view--mtimes-snapshot (org-air-query-files)))
      (org-air-view--render org-air-view--items filter)
      (org-air-view--restore-position token)
      ;; R73-1: the second swap tail — the fully-synchronous (batch /
      ;; byte-gate) re-query rebuilt every struct, so the resync redraws
      ;; the panes for the restored point in the same motion.
      (org-air-view--panes-resync-now))))

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
  (when (and (not org-air-view--bulk-source-write)
             buffer-file-name (org-air--relevant-file-p buffer-file-name))
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

(defun org-air-view--read-filter (candidate-tags &optional vocab)
  "Prompt for a tag filter PRE-FILLED with the active one (R18 D-P2/D-P3).
CANDIDATE-TAGS is the completion vocabulary (board item tags, or project
doc tags).  VOCAB, when non-nil, is the R72 date/status token offer list
\(`org-air-view--filter-vocabulary'), appended after the tag candidates;
the prompt then names the shapes.  REQUIRE-MATCH stays nil, so `due:12d'
types freely.  View-agnostic: shared by `org-air-filter',
`org-air-review-filter' and `org-air-project-filter' so the pre-fill +
AND default + `M-/' toggle are coded once (project/revisit pass no VOCAB
— their records carry no planning slots, R72 Decision 8)."
  (completing-read-multiple
   (if vocab "Filter (#tag, text, is:/due:/todo:): " "Filter (#tag or text): ")
   (append candidate-tags vocab) nil nil
   (when (org-air-view--filter-tags)
     (mapconcat #'identity (org-air-view--filter-tags) ","))))

(defun org-air-view--rerender-current-view ()
  "Re-render whichever org-air view is current: board or project (R18 D-P3).
The shared filter commands (`org-air-filter-clear',
`org-air-filter-toggle-match') re-render through this so they work in BOTH
the board and the project view without a hard dependency on
org-air-project (resolved by `fboundp')."
  (cond
   ((and (derived-mode-p 'org-air-project-mode)
         (fboundp 'org-air-project-refresh))
    (org-air-project-refresh))
   ;; R54-3: the revisit view re-renders in place (data untouched).
   ((and (derived-mode-p 'org-air-revisit-mode)
         (fboundp 'org-air-revisit--render-current))
    (org-air-revisit--render-current))
   ;; R61-4: the review view re-renders in place (data untouched).
   ((and (derived-mode-p 'org-air-review-mode)
         (fboundp 'org-air-review--render-current))
    (org-air-review--render-current))
   (t (org-air-view--render-current))))

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
                               #'string<))
            (org-air-view--filter-vocabulary)))))
  (setq org-air-view--tag-filter (unless (null tags) tags))
  (org-air-view--ensure-explicit-backlog-lens)
  (org-air-view--render-current))

(defun org-air-filter-by-tag (tag)
  "Compatibility wrapper: filter dashboard to TAG.
R18 D-P2: pre-fills with the first active filter tag (empty clears)."
  (interactive (list (read-string "Tag filter (empty clears): "
                                  (car (org-air-view--filter-tags)))))
  (setq org-air-view--tag-filter (unless (string-empty-p tag) (list tag)))
  (org-air-view--ensure-explicit-backlog-lens)
  (org-air-view--render-current))

(defun org-air-filter-toggle (tag)
  "Toggle TAG in the active filter list."
  (interactive "sTag: ")
  (let ((filters (org-air-view--filter-tags)))
    (setq org-air-view--tag-filter
          (if (member tag filters)
              (delete tag filters)
            (cons tag filters))))
  (org-air-view--ensure-explicit-backlog-lens)
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
THE HEADER so it can be re-collapsed immediately.  R51-3: on the
`…and N more' fold row itself, EXPAND that section — the row literally
teaches TAB, so TAB there must act, not drift; point lands on the first
newly-revealed row (the rows replace the fold row, so point stays put
visually).  On any other line TAB is safe — it moves to the next section
header and never toggles or hangs."
  (interactive)
  (org-air-view--loading-guard)
  (let* ((bucket (org-air-view--line-section))
         (more (and (not bucket)
                    (org-air-view--row-property 'org-air-more-row))))
    (cond
     (bucket
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
          (org-air-view--goto-row-title))))
     (more
      ;; R51-3: EXPAND the fold row's bucket (the row exists only while
      ;; collapsed, so this branch never collapses).  Remember the first
      ;; HIDDEN item — index `org-air-view--section-limit' of the bucket's
      ;; displayed-order list — BEFORE the render so point can land on its
      ;; revealed row after (never worse than the section header).
      (let* ((sorted (org-air-view--sort-items
                      (org-air-view--items-for-bucket
                       more (or org-air-view--items '()))
                      more))
             (first-hidden (nth (org-air-view--section-limit more) sorted)))
        (cl-pushnew more org-air-view--expanded-sections)
        ;; The SAME render seam the header branch takes (R18 D-P1b).
        (if (memq org-air-view--orientation '(board-only side-window))
            (org-air-view--render-section more)
          (org-air-view--render (or org-air-view--items (org-air-query-items))
                                org-air-view--tag-filter))
        (let ((pos (or (and first-hidden
                            (org-air-view--find-property 'org-air-item
                                                         first-hidden))
                       (org-air-view--find-property 'org-air-section more))))
          (when pos
            (goto-char pos)
            (org-air-view--goto-row-title)))))
     (t (org-air-next-section)))))

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
  "Toggle the item at point in the exact source-key bulk selection (R90).
Every mirror occurrence renders together; the selection survives cached
repaints, filters, sorts, folds and view changes in this live board.
The mark records the bounded projection witness of the heading actually
selected, so a later generation can never re-bind the key to another
heading (`org-air-view--marked-reconcile')."
  (interactive)
  (let* ((item (org-air-view--item-at-point))
         (key (or (org-air-view--item-source-key item)
                  (user-error "Item has no source identity")))
         (marked (org-air-view--marked-key-p key)))
    (if marked
        (org-air-view--marked-remove-keys (list key))
      (setq org-air-view--marked-keys
            (append org-air-view--marked-keys (list key)))
      (unless (hash-table-p org-air-view--marked-key-table)
        (org-air-view--marked-table-rebuild))
      (puthash key t org-air-view--marked-key-table)
      (puthash key (org-air-view--item-mark-witness item)
               (org-air-view--marked-witness-table)))
    (org-air-view--refresh-current)
    (message "%s %d item%s — b backlog, t add tag, M clears"
             (if marked "Unmarked" "Marked")
             (length org-air-view--marked-keys)
             (if (= (length org-air-view--marked-keys) 1) "" "s"))))

(defun org-air-clear-marks ()
  "Clear every source-key mark in the live board, repainting once (R90)."
  (interactive)
  (if (null org-air-view--marked-keys)
      (message "No marked items")
    (setq org-air-view--marked-keys nil)
    (org-air-view--marked-table-rebuild)
    (org-air-view--refresh-current)
    (message "Cleared all marks")))

(defun org-air-view--single-mutation-guard (label)
  "Soft-error for single-item mutation LABEL while a board selection is active."
  (when (org-air-view--marks-active-p)
    (user-error "%s is single-item while %d marks are active; M clears marks"
                label (length org-air-view--marked-keys))))

;;;; Inbox triage — inline dispositions + process-inbox (org-air-triage.org)

(defvar org-air-view--triage-source-buffer nil
  "Source buffer of the most recent triage disposition.
R73: a compatibility shadow — still SET by every recording path, but no
longer read by `u' (`org-air-edit-undo' dispatches on the bounded
`org-air-view--edit-ring' instead).")

(defconst org-air-view--edit-ring-max 20
  "Depth bound of the recent-edits undo ring (R73 Decision 7).
An internal bound like `org-air-view--refresh-pace' — never a
defcustom; 20 covers a whole process-inbox sitting, and the payload per
record is a short string, a buffer ref, ints, and bounded opaque tokens.")

(defvar org-air-view--edit-ring nil
  "The bounded recent-edits ring: a plain NEWEST-FIRST list (R73).
A single-file record is a plist (:desc :buffer :file :kind :tick :time).
R90 adds one compound shape, (:desc :kind bulk :parts :time), whose
ordered PARTS carry the same live-buffer/file/tick facts plus a bounded
opaque token for the expected undo head; it holds no source markers, raw undo
objects, or retained content.  A killed buffer degrades honestly in either
shape.  GLOBAL, not buffer-local:
edits are global facts about the user's org files, and history
survives a board kill/recreate (`q' + \\[org-air]).  `u' consumes from
the head; `org-air-view--edit-ring-push' truncates the tail at
`org-air-view--edit-ring-max' — no `make-ring' wraparound semantics
wanted.")

(defvar org-air-view--edit-redo-ring nil
  "The REDO side of the recent-edits ring (R75): a NEWEST-FIRST list.
Same single-file and R90 compound shapes as `org-air-view--edit-ring',
with small metadata and no source markers, raw undo objects, or retained
content, and the same GLOBAL single-timeline discipline (Decision 1).  Fed ONLY
by `org-air-edit-undo''s SUCCESS branch — the popped record, tick
re-stamped to the buffer's post-undo chars tick; the
consumed-without-revert branches (structural, dead, tick-tripped) feed
NOTHING, so a refile/archive record can never sit here and `U' can
never re-run a cross-buffer delete+insert (Decision 4 — not-redoable
BY CONSTRUCTION, the R64 duplicate class closed on both sides).
CLEARED by every FRESH edit inside `org-air-view--edit-ring-push' (the
one choke point every verb routes through — a fresh edit forks history
and discards the redone branch); the ring ops themselves push/re-push
DIRECTLY (`org-air-view--edit-ring-requeue') and never clear, so
undo/redo walks never eat their own future.  Bounded by CONSERVATION
under the one `org-air-view--edit-ring-max' defconst (Decision 6:
records only ever shuffle between the two sides until a fresh edit
truncates history), with the same defensive truncate on every push.")

(cl-defstruct
    (org-air-view--history-token
     (:constructor org-air-view--history-token-create (projection)))
  "Bounded opaque reference to an Emacs-owned undo identity."
  projection)

(defvar org-air-view--history-identity-registry
  (make-hash-table :test #'eq :weakness 'key-and-value)
  "Weak map from bounded history tokens to Emacs-owned undo-list tails.
`key-and-value' is intentional: a mapping survives only while both the token
is independently owned by a history record and the raw tail is independently
owned by Emacs' undo machinery.  The table therefore retains neither side
when a ring drops the token or a source buffer/undo lineage drops the tail.")

(defun org-air-view--history-identity-register (identity &optional projection)
  "Return a bounded token weakly resolving to raw undo IDENTITY.
PROJECTION is nil for a tail identity and `head' when callers compare the
first non-boundary undo object represented by that tail.  A nil IDENTITY
still returns an unregistered token: unavailable undo identity must block
conservatively rather than accidentally compare equal to nil."
  (let ((token (org-air-view--history-token-create projection)))
    (when identity
      (puthash token identity org-air-view--history-identity-registry))
    token))

(defun org-air-view--history-identity-resolve (identity)
  "Resolve bounded undo IDENTITY, leaving legacy raw values unchanged.
Return nil when a bounded token's weak registry entry has disappeared."
  (if (org-air-view--history-token-p identity)
      (when-let* ((raw (gethash identity
                                org-air-view--history-identity-registry)))
        (if (eq (org-air-view--history-token-projection identity) 'head)
            (car-safe raw)
          raw))
    identity))

(defun org-air-view--history-identity-match-p (identity current)
  "Return non-nil when stored IDENTITY resolves exactly to CURRENT.
A collected bounded token never matches, including when CURRENT is nil.
Legacy/synthetic raw values retain their established `eq' comparison."
  (if (org-air-view--history-token-p identity)
      (let ((missing (list 'missing)))
        (let ((raw (gethash identity org-air-view--history-identity-registry
                            missing)))
          (and (not (eq raw missing))
               (eq (if (eq (org-air-view--history-token-projection identity)
                           'head)
                       (car-safe raw)
                     raw)
                   current))))
    (eq identity current)))

(defun org-air-view--history-identity-forget (identity)
  "Remove bounded undo IDENTITY from the weak registry immediately."
  (when (org-air-view--history-token-p identity)
    (remhash identity org-air-view--history-identity-registry)))

(defun org-air-view--history-identity-put
    (plist property identity &optional projection)
  "Store raw IDENTITY in PLIST PROPERTY as a bounded opaque token.
PROJECTION has the meaning documented by
`org-air-view--history-identity-register'.  Return the updated plist."
  (when (plist-member plist property)
    (org-air-view--history-identity-forget (plist-get plist property)))
  (plist-put plist property
             (org-air-view--history-identity-register identity projection)))

(defun org-air-view--history-identity-remove (plist property)
  "Remove PLIST PROPERTY and forget any bounded identity it carried."
  (when (plist-member plist property)
    (org-air-view--history-identity-forget (plist-get plist property))
    (cl-remf plist property))
  plist)

(defun org-air-view--history-part-forget-identities (part)
  "Forget every bounded undo identity and side fact stored for PART."
  (dolist (property '(:expected-undo :undo-head))
    (when (plist-member part property)
      (org-air-view--history-identity-forget (plist-get part property))))
  (remhash part org-air-view--cache-sync-history))

(defvar org-air-view--cache-sync-history
  (make-hash-table :test #'eq :weakness 'key)
  "Weak record/part table for metadata history cache synchronization.
A value of `intervening-commit' forces disk-truth invalidation/rebuild after
its exact history step; t uses ordinary exact slot finalization.  Values
contain no source bytes, markers, undo objects, or snapshots.")

(defun org-air-view--history-buffer-killed ()
  "Forget weak identities owned by the source buffer being killed."
  (let ((buffer (current-buffer)))
    (dolist (record (append org-air-view--edit-ring
                            org-air-view--edit-redo-ring))
      (if (eq (plist-get record :kind) 'bulk)
          (dolist (part (plist-get record :parts))
            (when (eq (plist-get part :buffer) buffer)
              (org-air-view--history-part-forget-identities part)))
        (when (eq (plist-get record :buffer) buffer)
          (org-air-view--history-part-forget-identities record)
          (remhash record org-air-view--cache-sync-history))))))

(defun org-air-view--history-track-buffer (buffer)
  "Install bounded-history identity cleanup in live source BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (add-hook 'kill-buffer-hook
                #'org-air-view--history-buffer-killed nil t))))

(defun org-air-view--history-record-discard (record)
  "Release side metadata for a permanently discarded history RECORD."
  (if (eq (plist-get record :kind) 'bulk)
      (dolist (part (plist-get record :parts))
        (org-air-view--history-part-forget-identities part))
    (org-air-view--history-part-forget-identities record))
  (remhash record org-air-view--cache-sync-history))

(defun org-air-view--history-records-discard (records)
  "Release side metadata for permanently discarded history RECORDS."
  (dolist (record records)
    (org-air-view--history-record-discard record)))

(defun org-air-view--history-ring-clear (ring)
  "Discard every record on history RING and set that ring to nil."
  (org-air-view--history-records-discard (symbol-value ring))
  (set ring nil))

(defun org-air-view--history-ring-truncate (ring)
  "Truncate history RING to `org-air-view--edit-ring-max' with cleanup."
  (let ((records (symbol-value ring)))
    (when (> (length records) org-air-view--edit-ring-max)
      (let* ((last (nthcdr (1- org-air-view--edit-ring-max) records))
             (discarded (cdr last)))
        (setcdr last nil)
        (org-air-view--history-records-discard discarded)))))

(defun org-air-view--edit-ring-push (desc buffer &optional kind save-result cache-sync)
  "Push an undo record for the DESC edit made in BUFFER (R73 Decision 3).
SAVE-RESULT supplies the exact pre-user-hook undo identity/tick.  CACHE-SYNC
marks metadata edits whose undo/redo can finalize from cached source state.
KIND defaults to `in-place' (single-buffer, honestly undoable via the
buffer's own undo list); `refile' / `archive' mark the record
STRUCTURAL — named by `u' but never auto-reverted (Decision 6: a
source-side undo beside the moved copy would make a silent duplicate).
The tick is `buffer-chars-modified-tick' (never `buffer-modified-tick')
so fontification/text-property churn can never trip the `u' guard.
Older same-buffer records are NOT re-stamped here — only a successful
ring UNDO re-stamps (`org-air-view--edit-ring-restamp'): an in-place
chain never needs it (undoing the newer record first restores the
older record's expected state and re-stamps then), while re-stamping
under a STRUCTURAL push would arm the exact duplicate-maker Decision 6
forbids — the consumed-without-undo refile/archive record would leave
its source-side cut as the buffer's newest undo step, and a
guard-passing `u' on the older in-place record would `undo-only' THAT
cut, resurrecting the item beside its moved copy.  Letting the tick
guard trip there (\"changed since\") is the honest degrade.  Bounded
push-and-truncate to `org-air-view--edit-ring-max'.  Never signals.

R75 Decision 1: the SAME push CLEARS `org-air-view--edit-redo-ring' —
a fresh edit forks history and discards the redone branch (standard
undo/redo semantics), enforced BY CONSTRUCTION at this one choke point
every current and future verb already routes through.  The clear is
GLOBAL, not per-buffer: the ring is ONE timeline, and a `U' that
re-applied an edit from before the newest recorded mutation would lie
about it (the buffer-level `undo-redo' refusal backstops independently
beneath this — Decision 3).  The ring ops themselves never come
through here (`org-air-view--edit-ring-requeue' pushes directly)."
  (condition-case nil
      (when (buffer-live-p buffer)
        (org-air-view--history-ring-clear
         'org-air-view--edit-redo-ring)
        (let ((record
               (list :desc desc
                     :buffer buffer
                     :file (buffer-file-name buffer)
                     :kind (or kind 'in-place)
                     ;; The sentinel tick names the committed org-air state,
                     ;; not a later ordinary after-save-hook mutation.
                     :tick (or (plist-get save-result :expected-tick)
                               (buffer-chars-modified-tick buffer))
                     :time (current-time))))
          ;; Persist only a bounded token when a later hook left another step
          ;; ahead.  Its exact raw tail remains weakly owned by Emacs' undo
          ;; machinery, never by this record.
          (when (org-air-view--save-result-ahead-p
                 save-result buffer 'undo)
            (setq record
                  (org-air-view--history-identity-put
                   record :expected-undo
                   (plist-get save-result :expected-undo))))
          (when cache-sync
            (puthash record cache-sync org-air-view--cache-sync-history))
          (org-air-view--history-track-buffer buffer)
          (push record org-air-view--edit-ring))
        (org-air-view--history-ring-truncate
         'org-air-view--edit-ring))
    (error nil)))

(defun org-air-view--history-restamp-pair (plist buffer tick head)
  "Refresh PLIST's paired BUFFER history guard to TICK and HEAD, or not at all.
R90 gave a compound part a SECOND guard component beside `:tick' — the
`:undo-head' identity `org-air-view--bulk-history-blockers' checks straight
after the tick — so the two are ONE fact about one buffer state and may only
ever move together.  Refreshing half of the pair leaves the other half saying
\"a change intervened\" about a change org-air itself made through the ring; a
compound record can then never pass its own preflight again, and because a
blocked compound is requeued at the head of the same ring it shadows every
older record too.

HEAD is the authoritative post-commit undo head identity for the SAME buffer
state TICK names (`org-air-view--save-result-undo-head', captured inside the
save attempt).  When PLIST carries no `:undo-head' there is no pair and the
tick alone is the whole guard.  When it does and HEAD is nil — no proof of
what org-air last left there — or when HEAD is no longer the buffer's head,
stamp NOTHING and leave the part honestly blocked and retryable rather than
blessing a state org-air did not produce.  `:undo-head' is already a member
wherever it is refreshed, so both writes mutate PLIST in place.  Return
non-nil when the pair was refreshed."
  (if (plist-member plist :undo-head)
      (when (and head
                 (eq (car-safe head)
                     (car-safe
                      (org-air-view--history-undo-head-step buffer))))
        (plist-put plist :tick tick)
        (org-air-view--history-identity-put plist :undo-head head 'head)
        t)
    (plist-put plist :tick tick)
    t))

(defun org-air-view--edit-ring-restamp (buffer &optional tick head)
  "Re-stamp BUFFER's ring records with its current chars tick (R73/R75).
Run after every successful ring op in BUFFER (`u' undo, `U' redo): the
op restored exactly the content state the neighbouring records were
stamped against, so the tick guard keeps meaning \"no NON-ring change
intervened\" — while a real user edit still trips it (the tick bumps
with no re-stamp).  R75 Decision 5: TWO-SIDED — iterates BOTH
`org-air-view--edit-ring' AND `org-air-view--edit-redo-ring', because
in a same-buffer deep walk (edits A,B → u,u → U,U) each ring op
changes the buffer's tick, so the OTHER side's remaining records for
that buffer would trip the guard on pure ring-internal history.
Deliberately NOT run on push — see `org-air-view--edit-ring-push' (the
structural same-buffer duplicate hazard; the push clears the redo side
outright, so the hazard has no redo-side twin at all).

TICK, when given, is the AUTHORITATIVE post-commit tick org-air itself
left in BUFFER (`:expected-tick', captured inside the save attempt),
and is written verbatim instead of a fresh sample: a committed-buffer
restamp must record the state org-air PRODUCED, never whatever the
buffer happens to hold when a command-final sweep gets around to it
\(see `org-air-view--history-restamp-committed').

HEAD is that same authoritative state's undo head identity, and the
restamp is TWO-SIDED IN THE OTHER SENSE TOO: a compound part's
`:tick' and `:undo-head' are refreshed as one atomic pair
\(`org-air-view--history-restamp-pair'), so they can never disagree
and starve the record.  TICK and HEAD always travel together: with
TICK given, HEAD must come from the same save result; with TICK nil
\(the caller has just restored BUFFER itself) BOTH are sampled from
BUFFER here, in one instant, never one now and one later."
  (when (buffer-live-p buffer)
    (let* ((sampled (null tick))
           (tick (or tick (buffer-chars-modified-tick buffer)))
           (head (if sampled
                     (org-air-view--history-undo-head-step buffer)
                   head)))
      (dolist (rec (append org-air-view--edit-ring
                           org-air-view--edit-redo-ring))
        (if (eq (plist-get rec :kind) 'bulk)
            (dolist (part (plist-get rec :parts))
              (when (and (eq (plist-get part :buffer) buffer)
                         (not (plist-member part :expected-undo)))
                (org-air-view--history-restamp-pair
                 part buffer tick head)))
          (when (and (eq (plist-get rec :buffer) buffer)
                     (not (plist-member rec :expected-undo)))
            (org-air-view--history-restamp-pair rec buffer tick head)))))))

(defun org-air-view--edit-ring-requeue (rec buffer ring)
  "Re-stamp REC to BUFFER's current chars tick and push it onto RING.
RING is the SYMBOL of one ring side (`org-air-view--edit-ring' /
`org-air-view--edit-redo-ring').  The DIRECT ring-op push (R75
Decision 1): `org-air-edit-undo''s success branch moves the reverted
record onto the redo side, `org-air-edit-redo''s success branch moves
it back onto the undo side — deliberately NOT via
`org-air-view--edit-ring-push', which would clear the redo remainder
\(undo/redo walks must never eat their own future).  The re-stamp
makes the record's tick guard mean \"no non-ring change since the
ring op\" on its new side; the same defensive push-and-truncate to
`org-air-view--edit-ring-max' applies (Decision 6: the bound is
enforced on BOTH sides, not merely argued by conservation).

The re-stamp goes through the same atomic pair as every other one
\(`org-air-view--history-restamp-pair'): REC is an ordinary
single-buffer record today and carries no `:undo-head', but a
one-sided tick write is exactly the defect that starved compound
records, so no path writes half a guard."
  (unless (plist-member rec :expected-undo)
    (org-air-view--history-restamp-pair
     rec buffer (buffer-chars-modified-tick buffer)
     (org-air-view--history-undo-head-step buffer)))
  (org-air-view--history-track-buffer buffer)
  (set ring (cons rec (symbol-value ring)))
  (org-air-view--history-ring-truncate ring))

(defun org-air-view--item-at-point ()
  "Return the org-air item at point, or signal a `user-error'.
R22-2: line-based so a row action resolves from ANY column on the row (the
leading margin / rail / trailing pad carry no item property; a point-only
lookup there fails)."
  (or (org-air-view--row-property 'org-air-item)
      (user-error "No org-air item at point")))

(defvar org-air-view--save-attempt-token nil
  "Dynamic identity of the org-air save attempt currently running hooks.")

(defun org-air-view--visited-buffer-full-text ()
  "Return the current buffer's COMPLETE live text, ignoring any narrowing.
Save-boundary, durability and undo-guard logic compare a visited buffer
against a WHOLE file read, so a user's restriction is never truth about the
file: a narrowed source must still be compared as a full buffer.  The user's
exact restriction, point and mark are restored before returning."
  (save-mark-and-excursion
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun org-air-view--buffer-matches-visited-file-p ()
  "Return non-nil when current live text conservatively matches its file.
The comparison is always whole-buffer against whole-file; narrowing neither
hides a real divergence nor invents one."
  (and buffer-file-name
       (file-readable-p buffer-file-name)
       (condition-case nil
           (let ((file buffer-file-name)
                 (live (org-air-view--visited-buffer-full-text)))
             (with-temp-buffer
               (insert-file-contents file)
               (equal live (buffer-substring-no-properties
                            (point-min) (point-max)))))
         ((error quit) nil))))

(defun org-air-view--undo-disk-truth-guard ()
  "Keep modified state truthful after undoing a recursively saved hook group.
This function is installed as a source-free custom undo entry.  It runs after
that group's text and save-state entries — which have ALREADY reset modified
state — so success-by-omission is unsafe: an unreadable file, a failed read,
an error or a quit anywhere in the comparison would leave divergent live/disk
bytes falsely clean, making the user's ordinary `save-buffer' a no-op and the
documented manual save + retry path unreachable.  Only a POSITIVELY PROVEN
full-buffer equality with the visited file may leave the buffer clean; every
other outcome conservatively restores modified state.

The SUBJECT is resolved to the CANONICAL visited buffer first.  An indirect
clone (notably an `org-tree-to-indirect-buffer' clone) shares its base's text
and `buffer-undo-list' but has no visited file of its own, so an ordinary
`C-/' inside such a clone runs this guard with the clone current; the
comparison subject must be the base, whose widened full text is compared
against the base's own visited file.  If, AFTER that resolution, there is
genuinely no visited file, the guard is a strict no-op: with no disk truth
there is nothing to be dishonest about, and inventing modified state would
only raise a spurious dirty-buffer prompt.  The whole predicate/read/compare
path is protected and this never signals out of the undo machinery."
  (condition-case nil
      (let* ((subject (or (org-air-view--source-canonical-buffer
                           (current-buffer))
                          (current-buffer)))
             (file (and (buffer-live-p subject)
                        (buffer-local-value 'buffer-file-name subject))))
        ;; No visited file after canonicalization: no disk truth, no claim.
        (when (stringp file)
          (let ((proven nil))
            (condition-case nil
                (when (file-readable-p file)
                  (let ((live (with-current-buffer subject
                                (org-air-view--visited-buffer-full-text))))
                    (setq proven
                          (with-temp-buffer
                            (let ((read-result (insert-file-contents file)))
                              (and (consp read-result)
                                   (equal live
                                          (buffer-substring-no-properties
                                           (point-min) (point-max)))))))))
              ((error quit) (setq proven nil)))
            (unless proven
              (condition-case nil
                  (with-current-buffer subject
                    (restore-buffer-modified-p t))
                ((error quit) nil))))))
    ((error quit) nil)))

(defun org-air-view--undo-disk-truth-guard-install (expected-tail)
  "Install one disk-truth guard before EXPECTED-TAIL's boundary.
The current undo head must be one isolated recursively committed group ending
at the exact boundary whose cdr is EXPECTED-TAIL.  A save-state entry must be
present and no other custom undo handler may precede that boundary.  The whole
cdr spine through EXPECTED-TAIL must be proper, eq-unique and disjoint from the
newest group.  Cyclic, shared, dotted and otherwise malformed shapes return nil
without mutation.  Return non-nil only when the bounded function-only guard is
exactly at the old edge."
  (when (and (consp expected-tail) (consp buffer-undo-list))
    (let ((tail buffer-undo-list)
          (seen (make-hash-table :test #'eq))
          previous boundary guard-node
          save-state custom-handler ambiguous)
      ;; Inspect only the newest group.  Eq membership makes this traversal
      ;; total for arbitrarily deep valid groups without imposing a size cap.
      (while (and (consp tail) (car tail) (not ambiguous))
        (if (gethash tail seen)
            (setq ambiguous t)
          (puthash tail t seen)
          (let ((entry (car tail)))
            (cond
             ((and (consp entry) (eq (car entry) t))
              (setq save-state t))
             ((and (consp entry) (eq (car entry) 'apply))
              (let ((arguments (cdr entry)))
                (if (and (consp arguments)
                         (eq (car arguments)
                             #'org-air-view--undo-disk-truth-guard)
                         (null (cdr arguments)))
                    (if guard-node
                        (setq ambiguous t)
                      (setq guard-node tail))
                  (setq custom-handler t))))))
          (setq previous tail
                tail (cdr tail))))
      (when (and (not ambiguous)
                 (consp tail) (null (car tail))
                 (not (eq tail expected-tail))
                 (not (gethash expected-tail seen))
                 (eq (cdr tail) expected-tail))
        (setq boundary tail))
      ;; The old tail is untrusted too.  Validate its complete cdr spine before
      ;; installing anything: it must be proper, acyclic, and disjoint from
      ;; every node in the newest group (including its boundary).  Undo entries
      ;; may themselves be cons data, so deliberately inspect no car payload.
      (when boundary
        (puthash boundary t seen)
        (let ((older expected-tail))
          (while (and (consp older) (not ambiguous))
            (if (gethash older seen)
                (setq ambiguous t)
              (puthash older t seen)
              (setq older (cdr older))))
          (unless (null older)
            (setq ambiguous t))))
      (cond
       ((or ambiguous (null boundary) (null previous) (null save-state)
            custom-handler)
        nil)
       (guard-node
        ;; Repeated observation of the SAME isolated group confirms only the
        ;; one guard already occupying its exact old edge.
        (and (eq guard-node previous) (eq (cdr guard-node) boundary)))
       (t
        (condition-case nil
            (let ((node
                   (cons (list 'apply
                               #'org-air-view--undo-disk-truth-guard)
                         boundary)))
              (setcdr previous node)
              (and (eq (cdr previous) node)
                   (eq (cdr node) boundary)))
          ((error quit) nil)))))))

(defun org-air-view--save-attempt (&optional prepare)
  "Save current buffer and return an explicit irreversible-boundary result.
PREPARE runs once at the outer pre-write boundary.  The earliest outer
`after-save-hook' captures the org-air undo/redo/tick identity exactly once.
Recursive saves may mark `:recursive-commit' but can never overwrite either
outer fact.  Post-write errors are committed warnings; pre-write errors are
failures.  Sentinels are buffer-local and removed unconditionally."
  (let* ((token (list 'org-air-save-attempt))
         (save-function (symbol-function 'save-buffer))
         (save-depth 0)
         (prepared nil)
         (state nil)
         (after-save-began nil)
         (recursive-commit nil)
         (recursive-before-boundary nil)
         (recursive-guard-state nil)
         (recursive-guard-head nil)
         (recursive-guard-tick nil)
         (expected-undo nil)
         (expected-redo nil)
         (expected-tick nil)
         (failure nil)
         (returned nil)
         (before-sentinel
          (lambda ()
            (when (eq org-air-view--save-attempt-token token)
              (cond
               ((> save-depth 1)
                (setq recursive-commit t
                      recursive-before-boundary
                      (or recursive-before-boundary
                          (not after-save-began))))
               ((and prepare prepared)
                ;; A handler that recursively calls `basic-save-buffer' skips
                ;; the wrapped `save-buffer' depth counter.  Repeated prepare
                ;; still records recursion; only a repeat before the outer
                ;; identity is known makes that identity nonstandard.
                (setq recursive-commit t
                      recursive-before-boundary
                      (or recursive-before-boundary
                          (not after-save-began))))
               (prepare
                ;; One-shot: a recursive callback cannot replace STATE.
                (setq state (funcall prepare)
                      prepared t))))))
         (after-sentinel
          (lambda ()
            (when (eq org-air-view--save-attempt-token token)
              (if (or (> save-depth 1) after-save-began)
                  ;; A user hook committed another save step.  Preserve the
                  ;; first outer identity and make callers rebuild disk truth.
                  ;; Its save-state marker would otherwise make a later
                  ;; independent undo report the buffer unmodified after its
                  ;; text entries ran.  Put one function-only guard after that
                  ;; marker, at the old edge of this isolated hook group.
                  (progn
                    (setq recursive-commit t
                          recursive-before-boundary
                          (or recursive-before-boundary
                              (not after-save-began)))
                    ;; Revalidate every recursive save observation.  A repeat
                    ;; of the same isolated group confirms its one old-edge
                    ;; guard; any distinct/ambiguous later group makes identity
                    ;; permanently unavailable for this attempt.
                    (unless (eq recursive-guard-state 'unavailable)
                      (if (and
                           ;; A repeated sentinel may confirm the exact same
                           ;; group head.  New text/save-state entries mean a
                           ;; distinct recursive save even when they happen to
                           ;; share the old terminating boundary.
                           (or (null recursive-guard-state)
                               (and (eq recursive-guard-head buffer-undo-list)
                                    (eql recursive-guard-tick
                                         (buffer-chars-modified-tick))))
                           (condition-case nil
                               (org-air-view--undo-disk-truth-guard-install
                                expected-undo)
                             ((error quit) nil)))
                          (setq recursive-guard-state 'installed
                                recursive-guard-head buffer-undo-list
                                recursive-guard-tick
                                (buffer-chars-modified-tick))
                        (setq recursive-guard-state 'unavailable
                              recursive-guard-head nil
                              recursive-guard-tick nil
                              expected-undo nil
                              expected-redo nil))))
                ;; Close the org-air edit group before ordinary after-save
                ;; hooks mutate bytes, then capture its exact outer identity.
                (undo-boundary)
                (setq after-save-began t
                      expected-undo
                      (org-air-view--expected-undo-step (current-buffer))
                      expected-redo
                      (org-air-view--expected-redo-step (current-buffer))
                      expected-tick (buffer-chars-modified-tick)))))))
    (when prepare
      ;; Emacs 29's numeric hook depth keeps this after ordinary depth-zero
      ;; before-save hooks, so STATE describes the outer bytes to be written.
      (add-hook 'before-save-hook before-sentinel 100 t))
    ;; Earliest ordinary depth: once this runs, a later hook error cannot make
    ;; the completed outer write reversible again.
    (add-hook 'after-save-hook after-sentinel -100 t)
    (unwind-protect
        (let ((org-air-view--save-attempt-token token))
          (condition-case err
              (cl-letf (((symbol-function 'save-buffer)
                         (lambda (&rest args)
                           (cl-incf save-depth)
                           (unwind-protect
                               (apply save-function args)
                             (cl-decf save-depth)))))
                (save-buffer)
                (setq returned t))
            ((error quit) (setq failure err))))
      (when prepare
        (remove-hook 'before-save-hook before-sentinel t))
      (remove-hook 'after-save-hook after-sentinel t))
    (let* ((committed
            (or returned
                after-save-began
                ;; Belt-and-braces for unusual file handlers that write and
                ;; signal without running the standard after-save lifecycle.
                (and failure
                     (not (buffer-modified-p))
                     (verify-visited-file-modtime (current-buffer))
                     (org-air-view--buffer-matches-visited-file-p))))
           (standard-boundary after-save-began))
      (when (and committed prepare (not prepared))
        ;; A no-op/file-handler save may return without normal hooks.  Capture
        ;; final live truth, but mark its history identity nonstandard below.
        (condition-case prepare-error
            (setq state (funcall prepare)
                  prepared t)
          ((error quit)
           (unless failure (setq failure prepare-error)))))
      (when (and committed (null expected-tick))
        (undo-boundary)
        (setq expected-undo
              (org-air-view--expected-undo-step (current-buffer))
              expected-redo
              (org-air-view--expected-redo-step (current-buffer))
              expected-tick (buffer-chars-modified-tick)))
      (let ((guarded-identity
             (or (not recursive-commit)
                 (and (eq recursive-guard-state 'installed)
                      (not recursive-before-boundary)))))
        ;; Without the exact old-edge guard, persist no resolvable identity:
        ;; the intervening record must stay conservatively zero-byte blocked.
        (unless guarded-identity
          (setq expected-undo nil
                expected-redo nil))
        (list :committed committed :state state :error failure
              :expected-undo expected-undo :expected-redo expected-redo
              :expected-tick expected-tick
              :identity-known (and standard-boundary guarded-identity)
              :recursive-commit recursive-commit
              :intervening-commit recursive-commit
              :undo-disk-guard recursive-guard-state)))))

(defun org-air-view--persistent-warning (text)
  "Persistently surface bounded org-air warning TEXT without signaling."
  (condition-case nil
      (display-warning 'org-air
                       (truncate-string-to-width text 160 nil nil "…")
                       :warning)
    (error (message "org-air warning: %s" text))))

(defun org-air-view--report-save-warning (result)
  "Surface RESULT's bounded post-commit hook warning, if any."
  (when (and (plist-get result :committed)
             (not (plist-get result :intervening-warning-reported))
             (or (plist-get result :error)
                 (plist-get result :recursive-commit)))
    (let ((text
           (if (plist-get result :recursive-commit)
               (concat
                "intervening committed after-save hook detected; resolve and "
                "save that hook step before org-air history proceeds"
                (when (plist-get result :error)
                  (format ": %s"
                          (truncate-string-to-width
                           (error-message-string (plist-get result :error))
                           80 nil nil "…"))))
             (format "saved changes kept after hook error: %s"
                     (truncate-string-to-width
                      (error-message-string (plist-get result :error))
                      120 nil nil "…")))))
      (org-air-view--persistent-warning text)
      (message "org-air warning: %s" text))))

(defun org-air-view--signal-save-failure (result)
  "Re-signal the true pre-write failure represented by RESULT."
  (if-let* ((failure (plist-get result :error)))
      (signal (car failure) (cdr failure))
    (error "Source save failed before writing")))

(defun org-air-view--run-source-transaction
    (item desc-function kind body-function flush-function cache-sync)
  "Run ITEM's BODY-FUNCTION under the shared source-save boundary.
DESC-FUNCTION is evaluated in the source buffer after a committed save.
KIND is the history kind.  FLUSH-FUNCTION synchronously stores Org's pending
log record.  CACHE-SYNC requests total touched-file marker/tag capture, used
by no-scan metadata commands such as single-item backlog."
  (org-air-view--refresh-stale-item-guard item)
  (let* ((board (current-buffer))
         (buffer (find-file-noselect (org-air-item-file item)))
         (file (expand-file-name (org-air-item-file item)))
         (relocations
          (when cache-sync
            (with-current-buffer board
              (org-air-view--relocation-markers file buffer))))
         (snapshot (org-air-view--buffer-attempt-snapshot buffer))
         (finalizer-status 'ok)
         result)
    (unwind-protect
        (progn
          (condition-case err
              (with-current-buffer buffer
                (org-with-wide-buffer
                 (undo-boundary)
                 ;; The edit group is accepted BEFORE save.  No unwind may
                 ;; roll live bytes behind a file already written to disk.
                 (atomic-change-group
                   (save-excursion
                     (goto-char (or (and relocations
                                         (marker-position
                                          (cdr (assq item relocations))))
                                    (org-air-view--item-pos item)))
                     (org-back-to-heading t)
                     (let ((org-inhibit-logging
                            (or org-inhibit-logging 'note))
                           (org-log-reschedule
                            (if (eq org-log-reschedule 'note) 'time
                              org-log-reschedule))
                           (org-log-redeadline
                            (if (eq org-log-redeadline 'note) 'time
                              org-log-redeadline)))
                       (funcall body-function)))
                   (funcall flush-function))))
            ((error quit)
             (org-air-view--buffer-attempt-restore buffer snapshot)
             (signal (car err) (cdr err))))
          (org-air-view--relocation-arm-save-hooks relocations)
          (with-current-buffer buffer
            (org-with-wide-buffer
             (setq result
                   (org-air-view--save-attempt
                    (when cache-sync
                      (lambda ()
                        (org-air-view--cache-sync-capture relocations)))))))
          (if (not (plist-get result :committed))
              (progn
                (org-air-view--buffer-attempt-restore buffer snapshot)
                (org-air-view--signal-save-failure result))
            ;; The write is irreversible.  Cache/history finalization below is
            ;; total and must happen even when a later after-save hook signaled.
            (when cache-sync
              (with-current-buffer board
                (setq finalizer-status
                      (org-air-view--cache-sync-finalize
                       file relocations (plist-get result :state)
                       (when (plist-get result :recursive-commit)
                         (format
                          "intervening committed save hook%s"
                          (if-let* ((failure (plist-get result :error)))
                              (format ": %s" (error-message-string failure))
                            ""))))))
              (when (and (eq finalizer-status 'invalidated)
                         (plist-get result :recursive-commit))
                (plist-put result :intervening-warning-reported t)))
            (with-current-buffer buffer
              (org-air-view--edit-ring-push
               (funcall desc-function) buffer kind result
               (if (and cache-sync
                        (plist-get result :recursive-commit))
                   'intervening-commit
                 cache-sync)))
            (setq org-air-view--triage-source-buffer buffer)
            (org-air-view--report-save-warning result)
            finalizer-status))
      (when relocations
        (org-air-view--relocation-release relocations))
      (setq snapshot nil))))

(defmacro org-air-view--at-item-source (item &rest body)
  "At ITEM's source heading run BODY, save, and record one honest edit.
Optional leading forms are a DESC expression, `:structural', and
`:cache-sync' (in either order).  DESC is evaluated in the source buffer
after commit.  BODY and the synchronous Org log flush form one accepted
change group; save runs only after that group.  The shared save-attempt
sentinels distinguish true pre-write failure from a later `after-save-hook'
signal.  A true failure restores ephemeral live/undo state and records
nothing; a committed hook error finalizes cache/history and emits a bounded
warning.  `:cache-sync' additionally relocates every cached heading in the
touched file and mirrors committed effective tags without a query scan."
  (declare (indent 1) (debug t))
  (let ((desc nil) (structural nil) (cache-sync nil) (parsing t))
    (while (and parsing body)
      (let ((head (car body)))
        (cond
         ((eq head :structural) (setq structural t) (pop body))
         ((eq head :cache-sync) (setq cache-sync t) (pop body))
         ((and (null desc)
               (or (stringp head)
                   (and (consp head) (memq (car head) '(format concat)))))
          (setq desc (pop body)))
         (t (setq parsing nil)))))
    (let ((it (make-symbol "it")))
      `(let ((,it ,item))
         (org-air-view--run-source-transaction
          ,it
          (lambda ()
            ,(or desc `(format "edit \"%s\"" (org-air-item-title ,it))))
          ,(and structural ''archive)
          (lambda () ,@body)
          #'org-air-inbox--flush-pending-log-note
          ,cache-sync)))))

;;;; R90 source-key bulk tag coordinator

(defvar org-air-view--bulk-source-write nil
  "Non-nil while R90 owns source saves and the one cached repaint.")

(defun org-air-view--items-by-source-key (&optional items)
  "Return an equal hash from exact source key to all matching cached ITEMS."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (item (or items org-air-view--items))
      (when-let* ((key (org-air-view--item-source-key item)))
        (puthash key (append (gethash key table) (list item)) table)))
    table))

(defun org-air-view--bulk-eligible-p (item action)
  "Return non-nil when ITEM is eligible for marked tag ACTION."
  (and (eq (org-air-item-kind item) 'heading)
       (pcase action
         ('backlog (and (org-air-classify--board-active-p item)
                        (org-air-classify--task-routed-p item)))
         ('tag t))))

(defun org-air-view--source-heading-title ()
  "Return Org's synchronous native plain title at point."
  (plist-get (org-air-query--heading-projection) :title))

(defun org-air-view--source-local-tags ()
  "Return Org's synchronous native local tags at point."
  (plist-get (org-air-query--heading-projection) :local-tags))

(defun org-air-view--source-effective-tags ()
  "Return Org's synchronous native local-plus-inherited tags at point."
  (plist-get (org-air-query--heading-projection) :effective-tags))

(defun org-air-view--source-toggle-local-tag (tag state)
  "Set native local TAG on/off at point according to STATE.
Validation is prewrite and point-inert.  `org-toggle-tag' remains the only
writer, so inherited tags are never copied onto the child heading."
  (org-air-query--validate-single-tag-value tag)
  (org-back-to-heading t)
  (let ((org-element-use-cache nil))
    (org-toggle-tag tag state))
  ;; Ordinary after-save hooks and downstream Org match APIs must observe the
  ;; just-written native suffix synchronously, not a deferred cache snapshot.
  (condition-case nil
      (org-element-cache-reset nil t)
    (error nil)))

(defun org-air-view--source-heading-exact-p (item position)
  "Verify ITEM is the exact heading at POSITION in the current buffer."
  (and (integerp position)
       (<= (point-min) position) (<= position (point-max))
       (save-excursion
         (goto-char position)
         (when (org-at-heading-p)
           (let* ((indexed (and org-air-view--source-projection-index
                                (gethash item
                                         org-air-view--source-projection-index)))
                  (projection
                   (if (and indexed (eql position (car indexed)))
                       (cdr indexed)
                     (org-air-query--heading-projection))))
             (and
              (equal (plist-get projection :title)
                     (substring-no-properties (or (org-air-item-title item) "")))
              ;; Cached query tags are effective (local plus inherited), so
              ;; exact preflight compares the same one native projection.  The
              ;; later writer still changes local tags only.
              (equal (sort (plist-get projection :effective-tags) #'string<)
                     (sort (mapcar #'substring-no-properties
                                   (org-air-item-tags item))
                           #'string<))))))))

(defun org-air-view--bulk-preflight (action)
  "Preflight every ordered mark for ACTION without changing source bytes.
Return a plist carrying the old index, stale/ineligible/failed keys and
verified candidate records.  Distinct files are opened at most once."
  (let* ((keys (copy-sequence org-air-view--marked-keys))
         (index (org-air-view--items-by-source-key))
         (buffers (make-hash-table :test #'equal))
         stale ineligible failed candidates)
    (dolist (key keys)
      (let ((item (car (gethash key index))))
        (cond
         ((null item)
          (push key stale))
         ((not (org-air-view--bulk-eligible-p item action))
          (push key ineligible))
         (t
          (condition-case nil
              (progn
                (org-air-view--refresh-stale-item-guard item)
                (let* ((file (expand-file-name (org-air-item-file item)))
                       (pos (cdr key)))
                  ;; Reject a known read-only/vanished path before opening it:
                  ;; `find-file-noselect' may otherwise ask whether to make an
                  ;; already-known writable buffer read-only, violating the
                  ;; one-shared-prompt bulk contract.
                  (if (not (file-writable-p file))
                      (push key failed)
                    (let ((buf (or (gethash file buffers)
                                   (let ((opened (find-file-noselect file)))
                                     (puthash file opened buffers)
                                     opened))))
                      (with-current-buffer buf
                        (org-with-wide-buffer
                         (let* ((tracked
                                 (and (hash-table-p
                                       org-air-view--source-locator-index)
                                      (gethash
                                       item
                                       org-air-view--source-locator-index)))
                                (live-pos (or (and (markerp tracked)
                                                   (marker-position tracked))
                                              pos)))
                           (if buffer-read-only
                               (push key failed)
                             (if (org-air-view--source-heading-exact-p
                                  item live-pos)
                                 (push (list :key key :item item :file file
                                             :buffer buf :position live-pos
                                             :marker (copy-marker live-pos t))
                                       candidates)
                               (push key failed))))))))))
            (error
             (push key failed)))))))
    (setq candidates (nreverse candidates))
    ;; Validate and retain a TOTAL relocation set for every candidate file
    ;; before the first source mutation.  A file with one incomplete cached
    ;; locator is rejected wholesale; no earlier file can have committed yet.
    (let ((relocations (make-hash-table :test #'equal))
          (bad-files (make-hash-table :test #'equal))
          files)
      (dolist (record candidates)
        (cl-pushnew (plist-get record :file) files :test #'equal))
      (dolist (file files)
        (let ((buffer (gethash file buffers)))
          (condition-case nil
              (puthash file (org-air-view--relocation-markers file buffer)
                       relocations)
            (error
             (puthash file t bad-files)
             (dolist (record candidates)
               (when (equal file (plist-get record :file))
                 (push (plist-get record :key) failed)))))))
      (setq candidates
            (seq-remove (lambda (record)
                          (gethash (plist-get record :file) bad-files))
                        candidates))
      (list :index index
            :stale (nreverse stale)
            :ineligible (nreverse ineligible)
            :failed (nreverse failed)
            :candidates candidates
            :relocations relocations))))

(defun org-air-view--undo-head (buffer)
  "Return BUFFER's first non-boundary raw undo-list object, or nil."
  (with-current-buffer buffer
    (seq-find #'identity buffer-undo-list)))

(defun org-air-view--history-undo-head-step (buffer)
  "Return BUFFER's current first non-boundary undo list tail, or nil.
A stored `head' identity resolves to its raw tail's car, which is exactly
`org-air-view--undo-head', so this is the tail shape a `:undo-head' stamp
and its later check both speak.  Buffers with undo disabled, empty or
all-boundary lists honestly return nil rather than signalling."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((tail (org-air-view--undo-list-without-boundaries
                   buffer-undo-list)))
        (and (consp tail) (car tail) tail)))))

(defun org-air-view--undo-list-without-boundaries (list)
  "Return LIST advanced past leading undo boundaries."
  (while (and (consp list) (null (car list)))
    (setq list (cdr list)))
  list)

(defun org-air-view--expected-undo-step (buffer)
  "Return BUFFER's exact list-tail identity the next `undo-only' will apply.
Redo groups created by independently undoing an ahead edit are followed
through `undo-equiv-table', exactly as `undo-only' skips them."
  (with-current-buffer buffer
    (let ((tail (org-air-view--undo-list-without-boundaries
                 buffer-undo-list))
          next)
      (while (and (consp tail)
                  (consp (setq next (gethash tail undo-equiv-table))))
        (setq tail (org-air-view--undo-list-without-boundaries next)))
      (and (consp tail) (car tail) tail))))

(defun org-air-view--expected-redo-step (buffer)
  "Return BUFFER's exact list-tail identity the next `undo-redo' will apply."
  (with-current-buffer buffer
    (let ((tail (org-air-view--undo-list-without-boundaries
                 buffer-undo-list)))
      (and (consp tail) (gethash tail undo-equiv-table) tail))))

(defun org-air-view--save-result-ahead-p (result buffer operation)
  "Return non-nil when RESULT cannot expose OPERATION safely in BUFFER.
An intervening mutation/recursive commit, disabled undo, or nonstandard file
handler all require a bounded exact blocker.  No text/title identity is ever
guessed."
  (when result
    (let* ((expected
            (plist-get result
                       (if (eq operation 'undo)
                           :expected-undo :expected-redo)))
           (current
            (if (eq operation 'undo)
                (org-air-view--expected-undo-step buffer)
              (org-air-view--expected-redo-step buffer))))
      (or (not (plist-get result :identity-known))
          (null expected)
          (not (eq expected current))
          (not (eql (plist-get result :expected-tick)
                    (buffer-chars-modified-tick buffer)))))))

(defun org-air-view--save-result-undo-head (result)
  "Return the authoritative post-commit undo head tail RESULT captured.
Both `:expected-undo' and `:expected-redo' are sampled in ONE instant inside
the save attempt, right after the org-air edit group's own `undo-boundary'
and before any later hook can move the buffer, so either one is a fact about
the state org-air PRODUCED — never a late re-sample of whatever the buffer
holds when a command-final sweep gets around to it.

Exactly one of them is the buffer's own head tail, and which one is
decidable without knowing the direction: `org-air-view--expected-redo-step'
returns that head tail itself whenever the head carries a redo equivalence,
and when it does not — so this returns nil — `org-air-view--expected-undo-step'
has no cons equivalence to follow and returns that same head tail.  Return
nil exactly when RESULT persisted no resolvable identity at all (undo
disabled, a nonstandard handler, an unguarded recursive commit): the
no-proof case, where nothing may be stamped."
  (when result
    (or (plist-get result :expected-redo)
        (plist-get result :expected-undo))))

(defun org-air-view--relocation-markers (file buffer)
  "Create a total exact relocation set in BUFFER for cached headings in FILE.
Signal before mutation when any locator/identity is incomplete; never search
by title.  Markers stay before Org's own heading-line replacements during the
source mutation, then are armed with insertion-type t immediately before save
hooks run."
  (let (out)
    (condition-case err
        (progn
          (dolist (item
                   (if (hash-table-p org-air-view--source-items-by-file)
                       (gethash file org-air-view--source-items-by-file)
                     org-air-view--items))
            (when (and (eq (org-air-item-kind item) 'heading)
                       (equal file (expand-file-name (org-air-item-file item))))
              (let* ((tracked
                      (with-current-buffer buffer
                        (and (hash-table-p
                              org-air-view--source-locator-index)
                             (gethash
                              item org-air-view--source-locator-index))))
                     (position (or (and (markerp tracked)
                                        (marker-position tracked))
                                   (cdr (org-air-view--item-source-key item)))))
                (unless (with-current-buffer buffer
                          (org-with-wide-buffer
                           (and (integerp position)
                                (<= (point-min) position (point-max))
                                (org-air-view--source-heading-exact-p
                                 item position))))
                  (user-error "Incomplete source locator in %s; run g r"
                              (file-name-nondirectory file)))
                (push (cons item (with-current-buffer buffer
                                   (copy-marker position nil)))
                      out))))
          (nreverse out))
      ((error quit)
       (org-air-view--relocation-release out)
       (signal (car err) (cdr err))))))

(defun org-air-view--relocation-arm-save-hooks (relocations)
  "Make RELOCATIONS follow insertions exactly before headings during save."
  (dolist (entry relocations)
    (set-marker-insertion-type (cdr entry) t)))

(defun org-air-view--relocation-release (relocations)
  "Release every temporary marker in RELOCATIONS."
  (dolist (entry relocations)
    (set-marker (cdr entry) nil)))

(defun org-air-view--relocation-commit (file relocations)
  "Write RELOCATIONS back to cached `(FILE . POS)' marker slots."
  (dolist (entry relocations)
    (when (marker-position (cdr entry))
      (setf (org-air-item-marker (car entry))
            (cons file (marker-position (cdr entry)))))))

(defun org-air-view--cache-sync-capture (relocations)
  "Capture total committed cache state through live RELOCATIONS.
Each entry is `(ITEM POSITION LOCAL-TAGS EFFECTIVE-TAGS)'.  This runs as
the latest save preparation, after ordinary `before-save-hook' edits and
before the write.  Any missing exact heading aborts before disk commit."
  (let (state)
    (dolist (entry relocations)
      (let* ((item (car entry))
             (marker (cdr entry))
             (position (marker-position marker))
             (buffer (marker-buffer marker)))
        (unless (and position (buffer-live-p buffer))
          (error "Incomplete source relocation; run g r"))
        (with-current-buffer buffer
          (org-with-wide-buffer
           (goto-char position)
           (unless (and (org-at-heading-p)
                        (equal (org-air-view--source-heading-title)
                               (substring-no-properties
                                (or (org-air-item-title item) ""))))
             (error "Source heading %S changed at %s before save; run g r"
                    (org-air-item-title item) position))
           (let ((projection (org-air-query--heading-projection)))
             (push (list item position
                         (plist-get projection :local-tags)
                         (plist-get projection :effective-tags))
                   state))))))
    (nreverse state)))

(defun org-air-view--cache-sync-invalidate-generation (file reason)
  "Invalidate the current source generation after mandatory FILE failure.
REASON is surfaced persistently.  The next cached repaint must re-query
rather than retain a wrong-target item generation."
  (setq org-air-view--items nil
        org-air-view--items-key nil
        org-air-view--marked-keys nil
        org-air-view--marked-key-table (make-hash-table :test #'equal)
        org-air-view--marked-witnesses nil
        org-air-view--marked-generation nil
        org-air-view--classify-cache nil
        org-air-view--classify-cache-day nil)
  (org-air-view--source-prune-generation nil)
  (org-air-view--persistent-warning
   (format "Cache finalization invalidated %s: %s; run g r"
           (file-name-nondirectory file) reason)))

(defun org-air-view--cache-sync-write-slots (item file position effective-tags)
  "Write mandatory ITEM source slots for FILE, POSITION and EFFECTIVE-TAGS."
  (unless (integerp position)
    (error "Missing numeric source position"))
  (setf (org-air-item-marker item) (cons file position)
        (org-air-item-tags item) (copy-sequence effective-tags)))

(defun org-air-view--cache-sync-invalidate-classify (item file)
  "Invalidate ITEM's classify memo for FILE, poisoning cache on failure."
  (when org-air-view--classify-cache
    (condition-case err
        (remhash item org-air-view--classify-cache)
      (error
       ;; Never retain a table after its exact invalidation operation failed.
       ;; Assigning nil is deterministic even when `remhash' itself is hostile.
       (setq org-air-view--classify-cache nil
             org-air-view--classify-cache-day nil)
       (org-air-view--persistent-warning
        (format "Classification cache invalidated for %s after %s"
                (file-name-nondirectory file)
                (error-message-string err)))))))

(defun org-air-view--cache-sync-live-entry (item marker)
  "Return exact native fallback state for ITEM at live MARKER, or nil."
  (condition-case nil
      (when (and (marker-position marker)
                 (buffer-live-p (marker-buffer marker)))
        (with-current-buffer (marker-buffer marker)
          (org-with-wide-buffer
           (goto-char marker)
           (when (org-at-heading-p)
             (let ((projection (org-air-query--heading-projection)))
               (when (equal
                      (plist-get projection :title)
                      (substring-no-properties
                       (or (org-air-item-title item) "")))
                 (list item (marker-position marker)
                       (plist-get projection :local-tags)
                       (plist-get projection :effective-tags))))))))
    (error nil)))

(defun org-air-view--cache-sync-finalize
    (file relocations state &optional intervening-reason)
  "Finalize FILE's RELOCATIONS from STATE; return `ok' or `invalidated'.
INTERVENING-REASON forces one safe generation invalidation when a recursive
save committed bytes beyond STATE.  A mandatory slot failure is likewise a
one-way transition: invalidate once and stop all detached-item mandatory and
cosmetic work.  Parser and `remhash' failures retain their total semantics."
  ;; Native Org consumers must see synchronous post-write truth too.  Reset
  ;; only this already-live source's deferred element cache; no timer/file IO.
  (condition-case nil
      (when-let* ((marker (cdar relocations))
                  (buffer (marker-buffer marker)))
        (with-current-buffer buffer
          (org-element-cache-reset nil t)))
    (error nil))
  (if intervening-reason
      (progn
        (org-air-view--cache-sync-invalidate-generation
         file intervening-reason)
        'invalidated)
    ;; Preserve the injectable compatibility seam, but never rely on it: the
    ;; mandatory loop directly writes each relocation from total STATE.
    (condition-case nil
        (org-air-view--relocation-commit file relocations)
      (error nil))
    (catch 'invalidated
      (dolist (relocation relocations)
        (let* ((item (car relocation))
               (marker (cdr relocation))
               ;; Unusual handlers may skip preparation.  Exact live native
               ;; truth is the only fallback; no title search.
               (entry (or (assq item state)
                          (org-air-view--cache-sync-live-entry item marker)))
               (position (and entry (nth 1 entry)))
               (local-tags (and entry (nth 2 entry)))
               (effective-tags (and entry (nth 3 entry))))
          (condition-case err
              (if entry
                  (org-air-view--cache-sync-write-slots
                   item file position effective-tags)
                (error "Committed heading state unavailable"))
            (error
             (org-air-view--cache-sync-invalidate-generation
              file (error-message-string err))
             ;; Generation invalidation already poisons classification.  No
             ;; later relocation may mutate a detached item struct.
             (throw 'invalidated 'invalidated)))
          (org-air-view--cache-sync-invalidate-classify item file)
          ;; Cosmetic deferred Org-element mirroring is isolated and runs only
          ;; while the generation remains valid.
          (condition-case nil
              (when (and entry (buffer-live-p (marker-buffer marker))
                         (marker-position marker))
                (with-current-buffer (marker-buffer marker)
                  (save-excursion
                    (goto-char marker)
                    (when-let* ((element (org-element-at-point)))
                      (org-element-put-property
                       element :tags (copy-sequence local-tags))))))
            (error nil))))
      'ok)))

(defun org-air-view--bulk-key-after-relocation (key old-index)
  "Translate old KEY through OLD-INDEX after cached marker relocation."
  (if-let* ((item (car (gethash key old-index)))
            (new (org-air-view--item-source-key item)))
      new
    key))

(defun org-air-view--bulk-rekey-marks (old-index)
  "Rekey the surviving selection through OLD-INDEX after source relocation.
Each mark's bounded witness travels with its own key: org-air's OWN write
moved that heading, so the mark still names the heading the user selected and
must follow it, exactly as the key does."
  (let ((seen (make-hash-table :test #'equal))
        (witnesses (org-air-view--marked-witness-table))
        (moved (make-hash-table :test #'equal))
        out)
    (dolist (key org-air-view--marked-keys)
      (let ((new (org-air-view--bulk-key-after-relocation key old-index)))
        (unless (gethash new seen)
          (puthash new t seen)
          (push new out))
        (when-let* ((witness (gethash key witnesses)))
          (unless (gethash new moved) (puthash new witness moved)))))
    (setq org-air-view--marked-keys (nreverse out))
    (clrhash witnesses)
    (maphash (lambda (key witness) (puthash key witness witnesses)) moved)
    (org-air-view--marked-table-rebuild)))

(defun org-air-view--bulk-rekey-landing (plan old-index)
  "Translate PLAN's rendered source keys through OLD-INDEX after relocation."
  (when plan
    (dolist (slot '(:key))
      (when (plist-get plan slot)
        (plist-put plan slot
                   (org-air-view--bulk-key-after-relocation
                    (plist-get plan slot) old-index))))
    (dolist (slot '(:section-keys :full-keys))
      (plist-put plan slot
                 (mapcar (lambda (key)
                           (org-air-view--bulk-key-after-relocation
                            key old-index))
                         (plist-get plan slot))))
    plan))

(defun org-air-view--edit-ring-push-bulk (desc parts)
  "Push one compound bulk history record DESC with committed file PARTS."
  (when parts
    (org-air-view--history-ring-clear
     'org-air-view--edit-redo-ring)
    (dolist (part parts)
      (org-air-view--history-track-buffer (plist-get part :buffer)))
    (push (list :desc desc :kind 'bulk :parts parts :time (current-time))
          org-air-view--edit-ring)
    (org-air-view--history-ring-truncate
     'org-air-view--edit-ring)))

(defun org-air-view--bulk-message (action desired tag successful noops
                                          stale ineligible failed unattempted
                                          failed-file)
  "Echo marked ACTION/TAG completion for DESIRED and all target counts."
  (let ((base (pcase action
                ('backlog
                 (format "%s %d marked item%s"
                         (if desired "Backlogged" "Un-backlogged")
                         successful (if (= successful 1) "" "s")))
                ('tag
                 (format "Added #%s to %d marked item%s"
                         tag successful (if (= successful 1) "" "s")))))
        extras)
    (when (> noops 0)
      (push (format "%d already %s" noops
                    (if (eq action 'backlog)
                        (if desired "backlog" "not backlog")
                      "tagged"))
            extras))
    (when (> stale 0) (push (format "%d stale pruned" stale) extras))
    (when (> ineligible 0)
      (push (format "%d ineligible remains marked" ineligible) extras))
    (when (> failed 0)
      (push (format "%d failed%s — run g r"
                    failed (if failed-file
                               (format " in %s" (file-name-nondirectory failed-file))
                             ""))
            extras))
    (when (> unattempted 0)
      (push (format "%d unattempted remains marked" unattempted) extras))
    (message "%s%s" base
             (if extras (concat "; " (mapconcat #'identity (nreverse extras) "; "))
               ""))))

(defun org-air-view--marked-tag-action (action &optional tag)
  "Apply marked tag ACTION (`backlog' or `tag') with one cached repaint.
TAG is required for `tag'.  Source files commit deterministically and
atomically one file at a time; the first runtime file failure stops later
files.  Earlier files remain committed and only successes/no-ops clear."
  ;; Defend direct callers before landing capture or source preflight.  The
  ;; interactive `t' path has already performed this same immediate check.
  (setq tag (org-air-query--validate-single-tag-value
             (if (eq action 'backlog) org-air-backlog-tag tag)))
  (let* ((landing (org-air-view--mutation-landing-capture))
         (pre (org-air-view--bulk-preflight action))
         (old-index (plist-get pre :index))
         (stale (plist-get pre :stale))
         (ineligible (plist-get pre :ineligible))
         (pre-failed (plist-get pre :failed))
         (candidates (plist-get pre :candidates))
         (relocations-by-file (plist-get pre :relocations))
         (desired (if (eq action 'backlog)
                      (not (and candidates
                                (seq-every-p
                                 (lambda (record)
                                   (member org-air-backlog-tag
                                           (org-air-item-tags
                                            (plist-get record :item))))
                                 candidates)))
                    t))
         changed noops)
    ;; The stale exact-key misses are UI-state facts, pruned before writes.
    (org-air-view--marked-remove-keys stale)
    (dolist (record candidates)
      (let* ((item (plist-get record :item))
             (had (and (member tag (org-air-item-tags item)) t))
             (change (if desired (not had) had)))
        (plist-put record :had had)
        (plist-put record :desired desired)
        (if change (push record changed) (push record noops))))
    (setq changed (nreverse changed)
          noops (nreverse noops))
    (let ((by-file (make-hash-table :test #'equal))
          (committed nil) (parts nil) (runtime-failed nil) warnings
          (failed-file nil) (stop nil) (unattempted 0)
          (generation-invalidated nil))
      (dolist (record changed)
        (puthash (plist-get record :file)
                 (append (gethash (plist-get record :file) by-file)
                         (list record))
                 by-file))
      (let (files)
        (maphash (lambda (file _records) (push file files)) by-file)
        (setq files (sort files #'string<))
        (dolist (file files)
          (let ((records (gethash file by-file)))
            (if stop
                (setq unattempted (+ unattempted (length records)))
              (let* ((buffer (plist-get (car records) :buffer))
                     ;; Created for every candidate file during whole-command
                     ;; preflight, before any earlier file could commit.
                     (relocations (gethash file relocations-by-file))
                     (snapshot (org-air-view--buffer-attempt-snapshot buffer))
                     (mutation-error nil)
                     (save-result nil))
                (unwind-protect
                    (progn
                      (condition-case err
                          (with-current-buffer buffer
                            (org-with-wide-buffer
                             (undo-boundary)
                             ;; Accept the file-local edit group before save.
                             ;; Its unwind can never put live bytes behind a
                             ;; file whose write already completed.
                             (atomic-change-group
                               (dolist (record
                                        (sort (copy-sequence records)
                                              (lambda (a b)
                                                (> (marker-position
                                                    (plist-get a :marker))
                                                   (marker-position
                                                    (plist-get b :marker))))))
                                 (goto-char (plist-get record :marker))
                                 (unless (org-air-view--source-heading-exact-p
                                          (plist-get record :item) (point))
                                   (error "Source changed; run g r"))
                                 (let ((org-inhibit-logging
                                        (or org-inhibit-logging 'note))
                                       (org-log-reschedule
                                        (if (eq org-log-reschedule 'note) 'time
                                          org-log-reschedule))
                                       (org-log-redeadline
                                        (if (eq org-log-redeadline 'note) 'time
                                          org-log-redeadline)))
                                   (org-air-view--source-toggle-local-tag
                                    tag (if desired 'on 'off)))
                                 ;; Org's pending globals are shared: flush one
                                 ;; heading before another can overwrite them.
                                 (org-air-inbox--flush-pending-log-note)))))
                        ((error quit) (setq mutation-error err)))
                      (if mutation-error
                          (progn
                            (org-air-view--buffer-attempt-restore
                             buffer snapshot)
                            (setq runtime-failed records
                                  failed-file file
                                  stop t))
                        (org-air-view--relocation-arm-save-hooks relocations)
                        (with-current-buffer buffer
                          (org-with-wide-buffer
                           (let ((org-air-view--bulk-source-write t))
                             (setq save-result
                                   (org-air-view--save-attempt
                                    (lambda ()
                                      (org-air-view--cache-sync-capture
                                       relocations)))))))
                        (if (not (plist-get save-result :committed))
                            (progn
                              ;; True pre-write failure: recover the exact
                              ;; pre-attempt bytes, modified/visited state and
                              ;; undo machinery, then stop before later files.
                              (org-air-view--buffer-attempt-restore
                               buffer snapshot)
                              (setq runtime-failed records
                                    failed-file file
                                    stop t))
                          ;; A later hook signal is committed success.  A
                          ;; recursive commit or mandatory slot failure makes
                          ;; finalization one-way and stops later old-generation
                          ;; file work, while preserving this committed part.
                          (let ((status
                                 (org-air-view--cache-sync-finalize
                                  file relocations
                                  (plist-get save-result :state)
                                  (when (plist-get save-result
                                                   :recursive-commit)
                                    (format
                                     "intervening committed save hook%s"
                                     (if-let* ((failure
                                                (plist-get save-result :error)))
                                         (format ": %s"
                                                 (error-message-string failure))
                                       ""))))))
                            (when (eq status 'invalidated)
                              (setq generation-invalidated t
                                    stop t)
                              (when (plist-get save-result :recursive-commit)
                                (plist-put save-result
                                           :intervening-warning-reported t))))
                          (setq committed (append committed records))
                          (let* ((expected
                                  (plist-get save-result :expected-undo))
                                 (part
                                  (list :buffer buffer :file file
                                        :tick
                                        (plist-get save-result :expected-tick)
                                        :undo-head nil)))
                            ;; New compound metadata stores bounded tokens for
                            ;; both normal head and exceptional tail identity.
                            ;; The raw tail remains ephemeral in SAVE-RESULT.
                            (setq part
                                  (org-air-view--history-identity-put
                                   part :undo-head expected 'head))
                            (when (org-air-view--save-result-ahead-p
                                   save-result buffer 'undo)
                              (setq part
                                    (org-air-view--history-identity-put
                                     part :expected-undo expected)))
                            (when (plist-get save-result :recursive-commit)
                              (puthash part 'intervening-commit
                                       org-air-view--cache-sync-history))
                            (push part parts))
                          (when (or (plist-get save-result :error)
                                    (plist-get save-result :recursive-commit))
                            (push save-result warnings)))))
                  (org-air-view--relocation-release relocations)
                  (remhash file relocations-by-file)
                  (setq snapshot nil)
                  (dolist (record records)
                    (when (markerp (plist-get record :marker))
                      (set-marker (plist-get record :marker) nil)))))))))
      ;; No-op-only and stop-before-later-file relocation sets were never
      ;; consumed, but they were part of total preflight and must be released.
      (maphash (lambda (_file relocations)
                 (org-air-view--relocation-release relocations))
               relocations-by-file)
      (clrhash relocations-by-file)
      ;; Every exact-target marker was temporary (no marker enters marks,
      ;; cache persistence or history), including desired-state no-ops.
      (dolist (record candidates)
        (when (markerp (plist-get record :marker))
          (set-marker (plist-get record :marker) nil)))
      (setq parts (nreverse parts))
      (when parts
        (org-air-view--edit-ring-push-bulk
         (if (eq action 'backlog)
             (format "%s %d marked item%s"
                     (if desired "backlog" "un-backlog")
                     (length committed) (if (= (length committed) 1) "" "s"))
           (format "tag %d marked item%s +%s"
                   (length committed) (if (= (length committed) 1) "" "s") tag))
         parts))
      (if generation-invalidated
          (progn
            ;; Invalidation already cleared marks, classification and source
            ;; locators.  Never rekey, clear, classify or land through detached
            ;; old structs; rebuild final disk truth exactly once.
            (setq landing nil
                  org-air-view--pending-mutation-landing nil)
            (org-air-view--refresh-current)
            (org-air-view--panes-resync-now)
            (dolist (warning (nreverse warnings))
              (org-air-view--report-save-warning warning))
            (org-air-view--bulk-message
             action desired tag (length committed) 0 (length stale)
             (length ineligible)
             (+ (length pre-failed) (length runtime-failed))
             unattempted failed-file))
        ;; Healthy finalization may update old-generation UI identities.
        (dolist (record noops)
          (dolist (item (gethash (plist-get record :key) old-index))
            (when org-air-view--classify-cache
              (remhash item org-air-view--classify-cache))))
        (org-air-view--bulk-rekey-marks old-index)
        (setq landing (org-air-view--bulk-rekey-landing landing old-index))
        (let* ((clear-old
                (append (mapcar (lambda (r) (plist-get r :key)) committed)
                        (mapcar (lambda (r) (plist-get r :key)) noops)))
               (clear (mapcar (lambda (key)
                                (org-air-view--bulk-key-after-relocation
                                 key old-index))
                              clear-old))
               (excluded (mapcar (lambda (record)
                                   (org-air-view--bulk-key-after-relocation
                                    (plist-get record :key) old-index))
                                 (if (eq action 'backlog) committed nil)))
               (successful (+ (length committed) (length noops)))
               (changed-state (or stale clear committed)))
          (org-air-view--marked-remove-keys clear)
          (when changed-state
            (setq org-air-view--pending-mutation-landing
                  (org-air-view--mutation-landing-exclude landing excluded))
            (org-air-view--refresh-current)
            ;; Landing was consumed before setup; now directly converge panes.
            (org-air-view--panes-resync-now))
          (dolist (warning (nreverse warnings))
            (org-air-view--report-save-warning warning))
          (org-air-view--bulk-message
           action desired tag successful (length noops) (length stale)
           (length ineligible)
           (+ (length pre-failed) (length runtime-failed))
           unattempted failed-file))))))

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
DATE is a date string or `clear'.  Refinement only — stays in Inbox.
R68-3 audit: the quick-date `read-char-exclusive' runs BEFORE the
macro, against the displayed board — safe; a `lognotereschedule' /
`lognoteredeadline' note is downgraded + flushed by the macro's
logging discipline."
  (org-air-view--single-mutation-guard
   (if (eq kind 'deadline) "Setting a deadline" "Scheduling"))
  (let* ((item (org-air-view--item-at-point))
         (clearp (eq date 'clear))
         (setter (if (eq kind 'deadline) #'org-deadline #'org-schedule)))
    (org-air-view--at-item-source item
      (format "%s \"%s\"%s"
              (if (eq kind 'deadline) "deadline" "schedule")
              (org-air-item-title item)
              (if clearp " cleared" (format " → %s" date)))
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
  (org-air-view--single-mutation-guard "Scheduling")
  (org-air-view--apply-date 'scheduled
                            (or date (org-air-view--read-quick-date "Schedule"))))

;;;###autoload
(defun org-air-item-deadline (&optional date)
  "Set DEADLINE on the item at point via the quick-date sub-prompt.
DATE may be supplied non-interactively.  A refinement: stays in Inbox."
  (interactive)
  (org-air-view--single-mutation-guard "Setting a deadline")
  (org-air-view--apply-date 'deadline
                            (or date (org-air-view--read-quick-date "Deadline"))))

(defun org-air-set-schedule (date)
  "Set SCHEDULED DATE on the item at point (legacy — no key binding).
R68-3: converted to `org-air-view--at-item-source' (its old body was
a hand-copy of the macro), so a `lognotereschedule' configuration now
records a downgraded timestamped entry in the same save instead of
trapping an `*Org Note*' prompt against the undisplayed source buffer
— and the edit gains the triage-undo (`u') recording.
R68fix: this defun must sit BELOW the `org-air-view--at-item-source'
defmacro — a use above the definition byte-compiles against whatever
macro a stale `.elc' happens to carry on an incremental build,
silently losing the logging discipline (the round-68 Fable blocker)."
  (interactive "sSchedule (empty clears): ")
  (org-air-view--single-mutation-guard "Scheduling")
  (let ((item (org-air-view--item-at-point)))
    (org-air-view--at-item-source item
      (format "schedule \"%s\"%s" (org-air-item-title item)
              (if (string-empty-p date) " cleared" (format " → %s" date)))
      (org-schedule nil (unless (string-empty-p date) date))))
  (org-air-refresh))

(defun org-air-set-tag ()
  "Add TAG to the item at point.
The R68-3 audit left this raw (the `read-string' prompt runs BEFORE
the buffer switch — against the displayed board — and `org-toggle-tag'
is prompt-free; uniformity alone had no bug behind it).  R73-2
converts it onto `org-air-view--at-item-source' after all: an
unrecorded tag edit would be invisible to the recent-edits
ring (`u'), and the conversion brings the R68 logging discipline for free.
The prompt STAYS before the macro — the R68-3 safety shape.
Relocated BELOW the defmacro (the R68fix source-order law: a call
above the definition byte-compiles against whatever macro a stale
`.elc' carries on an incremental build).

R90: with marks active, prompts once and adds that tag to every eligible
exact source heading as one compound, file-atomic edit."
  (interactive)
  (if (org-air-view--marks-active-p)
      ;; Marked targets are durable and may be hidden, so chrome point is
      ;; valid.  Prompt once, then validate before landing/preflight/open.
      (let ((tag (org-air-query--validate-single-tag-value
                  (read-string "Tag all marked items: "))))
        (org-air-view--marked-tag-action 'tag tag))
    ;; Unmarked dispatch resolves point before prompting.  A section header
    ;; or collapsed chrome row therefore refuses without asking for a value.
    (let* ((item (org-air-view--item-at-point))
           (tag (org-air-query--validate-single-tag-value
                 (read-string "Tag: "))))
      (org-air-view--at-item-source item
        (format "tag \"%s\" +%s" (org-air-item-title item) tag)
        (org-air-view--source-toggle-local-tag tag 'on))
      (org-air-refresh))))

;;;###autoload
(defun org-air-item-backlog ()
  "Toggle `org-air-backlog-tag' on the item at point — defer/un-defer (R83).
Adds the tag when absent, REMOVES it when present (a reversible
un-backlog): a board-active tagged item routes OFF the four task buckets
\(Upcoming / Needs attention / High priority / Stale) and the Inbox into
the single `backlog' bucket — off the attention surfaces, still trackable
\(the Backlog section + a rail Summary count) and reachable everywhere
non-attention (Notes, all-items, the day view, the calendar, `#backlog',
`is:backlog').

PROMPT-FREE: unlike `org-air-set-tag' (a `read-string') and
`org-air-item-kill' (a `yes-or-no-p'), the `b' key reads NOTHING — the
tag is the `org-air-backlog-tag' defcustom, so the whole action is a
single non-interactive keystroke (fully batch-testable, no GUI-confirm
bits).

Writes the SOURCE heading via `org-air-view--at-item-source' (the SAME
path `org-air-set-tag' uses): the R68 board-context logging discipline
\(inhibit the note-downgrade + pre-save flush), ONE `atomic-change-group'
\(one edit = one undo group), an `org-toggle-tag' that preserves every
OTHER tag, and — for free — an R73/R75 recent-edits ring record (`u'
undoes, `U' redoes; an in-place, single-buffer record, never structural).

Then — R53, no rescan — mutates the CACHED item's `tags' slot in place,
drops that one item's `eq' classify-memo entry, and repaints from cache
via `org-air-view--refresh-current': the row moves from Needs-attention
to Backlog (Summary follows) with NO org-ql re-query.  Never-error: the
soft `user-error' (no item at point / a mid-refresh stale file) is
message-only, a mid-body signal rolls back the `atomic-change-group'
\(no save, no ring push), and any RESIDUAL hard error downgrades to a
message (the R53 never-error law).

Relocated BELOW the `org-air-view--at-item-source' defmacro (the R68fix
source-order law), beside `org-air-set-tag'.

R90: the ordinary Backlog is header-only until TAB expands it.  A single
unmarked toggle lands on a local survivor instead of following the moved
item.  With marks active, set-all semantics apply to every eligible exact
source heading and one compound `u'/`U' record covers all changed files."
  (interactive)
  (if (org-air-view--marks-active-p)
      (org-air-view--marked-tag-action 'backlog)
    (let* ((item (org-air-view--item-at-point))
           (tag  org-air-backlog-tag)
           (had  (and (member tag (org-air-item-tags item)) t))
           (want (if had 'off 'on))
           (old-index (org-air-view--items-by-source-key))
           (landing (and (derived-mode-p 'org-air-view-mode)
                         (org-air-view--mutation-landing-capture)))
           (finalizer-status 'ok))
      (condition-case err
          (progn
            (setq finalizer-status
                  (org-air-view--at-item-source item
                    (format "backlog \"%s\" %s%s" (org-air-item-title item)
                            (if had "-" "+") tag)
                    :cache-sync
                    (org-air-view--source-toggle-local-tag
                     tag want))) ; local and idempotent
            (unless (eq finalizer-status 'invalidated)
              ;; Save-hook edits may shift every source key in the file.  The
              ;; macro finalized those exact committed marker positions.
              (setq landing
                    (org-air-view--bulk-rekey-landing landing old-index))
              (when org-air-view--classify-cache
                (remhash item org-air-view--classify-cache))
              ;; R90: this one mutation repaint excludes the moved identity;
              ;; generic repaint/sort/resize restoration stays untouched.
              (when landing
                (setq org-air-view--pending-mutation-landing
                      (org-air-view--mutation-landing-exclude
                       landing (list (org-air-view--item-source-key item))))))
            ;; Invalidated generations query exactly once here; healthy ones
            ;; repaint only from their current cached list.
            (org-air-view--refresh-current)
            (when (or landing (eq finalizer-status 'invalidated))
              (org-air-view--panes-resync-now))
            (message "%s \"%s\"" (if had "Un-backlogged" "Backlogged")
                     (org-air-item-title item)))
        ;; R53 never-error: the soft `user-error' (no item / the mid-refresh
        ;; stale guard) propagates message-only; any RESIDUAL hard error
        ;; downgrades to a message (belt-and-suspenders) — never a backtrace.
        (user-error (signal (car err) (cdr err)))
        (error (message "Backlog: %s" (error-message-string err)))))))

;;;###autoload
(defun org-air-item-file-group ()
  "Fast-refile the item at point under a category/group (graduates it)."
  (interactive)
  (org-air-view--single-mutation-guard "Filing")
  (call-interactively #'org-air-refile-item))

;;;###autoload
(defun org-air-item-cycle-todo ()
  "Set the TODO state of the item at point — select from its file's keywords.
R68-1: completes over the item's SOURCE file's own merged todo
vocabulary (`org-air-inbox--read-todo-keyword' — the user's globals +
the file's `#+TODO:' line win, dir/file-local declarations honoured;
R57) and applies the choice via an EXPLICIT-string `org-todo' through
`org-air-view--at-item-source'.  Never a nil-arg `org-todo': under
fast-selection todo keywords (`TODO(t)' …) that routes into
`org-fast-todo-selection' even from pure Lisp — a synchronous key
read against the board's UNDISPLAYED source buffer, the reported
silent no-op.  An empty choice, or re-picking the current keyword, is
a gentle \"Todo unchanged\" no-op (no write, no refresh churn).
Named -cycle-todo (name/binding kept — muscle memory + test pins) to
avoid colliding with the `org-air-item-todo' struct accessor; the
triage spec's `T' key maps here."
  (interactive)
  (org-air-view--single-mutation-guard "Setting TODO state")
  (let* ((item (org-air-view--item-at-point))
         (old (org-air-item-todo item))
         (choice (org-air-inbox--read-todo-keyword
                  (org-air-item-file item) old)))
    (if (or (null choice) (equal choice old))
        (message "Todo unchanged")
      (org-air-view--at-item-source item
        (format "todo \"%s\" → %s" (org-air-item-title item) choice)
        (org-todo choice))
      (org-air-refresh)
      (message "Todo \"%s\": %s → %s"
               (org-air-item-title item) old choice))))

;;;###autoload
(defun org-air-item-archive ()
  "Archive the item at point's subtree (graduates it out of Inbox).
R68-3 audit: SAFE — `org-archive-subtree' performs no interactive
reads and its archive-buffer writes are non-interactive.
R73 Decision 6: the record is `:structural' — `org-archive-subtree'
writes the archive location too, so a source-side undo would leave the
archived COPY (the duplicate shape); `u' consumes the record with a
message naming the archive file instead."
  (interactive)
  (org-air-view--single-mutation-guard "Archiving")
  (let ((item (org-air-view--item-at-point)))
    (org-air-view--at-item-source item
      (format "archive \"%s\" → %s" (org-air-item-title item)
              (or (ignore-errors
                    (file-name-nondirectory
                     (car (org-archive--compute-location
                           org-archive-location))))
                  "the archive file"))
      :structural
      (org-archive-subtree))
    (org-air-refresh)
    (message "Archived \"%s\"" (org-air-item-title item))))

;;;###autoload
(defun org-air-item-done ()
  "Mark the item at point DONE (graduates it out of Inbox).
The explicit-symbol `(org-todo \='done)' never enters fast-selection;
R68-3: a `COMP(c!)'-style time record — or a `DONE(d@)' note,
downgraded to its timestamp — is flushed into the same save by the
`org-air-view--at-item-source' logging discipline instead of pending
against the undisplayed source buffer."
  (interactive)
  (org-air-view--single-mutation-guard "Marking DONE")
  (let ((item (org-air-view--item-at-point)))
    (org-air-view--at-item-source item
      (format "done \"%s\"" (org-air-item-title item))
      (org-todo 'done))
    (org-air-refresh)
    (message "Marked DONE \"%s\"" (org-air-item-title item))))

;;;###autoload
(defun org-air-item-kill ()
  "Delete the item at point's subtree, with confirmation (graduates it).
R68-3 audit: SAFE — the `yes-or-no-p' confirm runs BEFORE the macro,
against the displayed board; `org-cut-subtree' is prompt-free."
  (interactive)
  (org-air-view--single-mutation-guard "Killing")
  (let ((item (org-air-view--item-at-point)))
    (when (yes-or-no-p (format "Delete \"%s\"? " (org-air-item-title item)))
      (org-air-view--at-item-source item
        (format "kill \"%s\"" (org-air-item-title item))
        (org-cut-subtree))
      (org-air-refresh)
      (message "Deleted \"%s\"" (org-air-item-title item)))))

(defun org-air-view--history-expected-durable-p (record)
  "Return non-nil when expected-history RECORD is safe to write now.
This command-time predicate is intentionally separate from identity/status
polling: it reads the visited file only for an inverse operation governed by
`:expected-undo'.  Live source bytes must already be an unmodified, current-
modtime, readable and byte-exact image of the recorded visited file.  The live
image is always the COMPLETE buffer: a user narrowing is not a durability
failure.  Any nil, error or quit is conservative failure."
  (and (plist-member record :expected-undo)
       (let ((buffer (plist-get record :buffer))
             (recorded-file (plist-get record :file)))
         (and (buffer-live-p buffer)
              (with-current-buffer buffer
                (condition-case nil
                    (let ((file buffer-file-name))
                      (and (stringp file) (> (length file) 0)
                           (stringp recorded-file)
                           (equal (expand-file-name file)
                                  (expand-file-name recorded-file))
                           (file-exists-p file)
                           (file-readable-p file)
                           (not (buffer-modified-p))
                           (verify-visited-file-modtime (current-buffer))
                           (let ((live
                                  (org-air-view--visited-buffer-full-text)))
                             (with-temp-buffer
                               (let ((read-result
                                      (insert-file-contents file)))
                                 (and (consp read-result)
                                      (equal live
                                             (buffer-substring-no-properties
                                              (point-min) (point-max)))))))))
                  ((error quit) nil)))))))

(defun org-air-view--history-durability-blocker (record)
  "Return a source-free command blocker when expected RECORD is not durable."
  (when (and (plist-member record :expected-undo)
             (not (org-air-view--history-expected-durable-p record)))
    (let* ((buffer (plist-get record :buffer))
           (name (file-name-nondirectory
                  (or (plist-get record :file) "source"))))
      (cond
       ((not (buffer-live-p buffer)) (format "%s gone" name))
       ((buffer-modified-p buffer)
        (format "unsaved edit in %s is not durably saved" name))
       (t
        (format "%s visited file is unavailable, stale, or differs from live source"
                name))))))

(defun org-air-view--history-expected-safe-p (record &optional operation)
  "Return non-nil when RECORD's exact OPERATION identity is safe.
New pass-2 records use bounded opaque tokens.  For undo, exact comparison
follows only Emacs' redo-equivalence chain; for redo it requires the exact
redo tail.  Missing weak identities safely fail.  With OPERATION nil, either
direction is accepted for read-only status callers.  Legacy/synthetic raw
`:expected-undo' and `:undo-head' values retain their established semantics."
  (let ((buffer (plist-get record :buffer)))
    (and (buffer-live-p buffer)
         (if (plist-member record :expected-undo)
             (let ((expected (plist-get record :expected-undo)))
               (pcase operation
                 ('undo
                  (org-air-view--history-identity-match-p
                   expected (org-air-view--expected-undo-step buffer)))
                 ('redo
                  (or (org-air-view--history-identity-match-p
                       expected (org-air-view--expected-redo-step buffer))
                      ;; Independently undoing a recursively committed hook
                      ;; creates a redo group ahead of the org-air redo.  Its
                      ;; exact equivalence chain exposes the expected org-air
                      ;; tail to `undo-only', which safely skips that hook redo.
                      (and (eq (gethash record
                                        org-air-view--cache-sync-history)
                               'intervening-commit)
                           (org-air-view--history-identity-match-p
                            expected
                            (org-air-view--expected-undo-step buffer)))))
                 (_
                  (or (org-air-view--history-identity-match-p
                       expected (org-air-view--expected-undo-step buffer))
                      (org-air-view--history-identity-match-p
                       expected (org-air-view--expected-redo-step buffer))))))
           (and (eql (plist-get record :tick)
                     (buffer-chars-modified-tick buffer))
                (or (not (plist-member record :undo-head))
                    (org-air-view--history-identity-match-p
                     (plist-get record :undo-head)
                     (org-air-view--undo-head buffer))))))))

(defun org-air-view--history-ahead-edit-name (record)
  "Name the intervening edit ahead of RECORD's expected org-air step."
  (let* ((buffer (plist-get record :buffer))
         (name (file-name-nondirectory
                (or (plist-get record :file) "source"))))
    (format "%s in %s is ahead of the expected org-air step"
            (if (and (buffer-live-p buffer)
                     (buffer-modified-p buffer))
                "unsaved edit"
              "intervening committed hook edit")
            name)))

(defun org-air-view--bulk-part-live-p (part)
  "Return non-nil when PART's exact expected history identity is healthy."
  (org-air-view--history-expected-safe-p part))

(defun org-air-view--bulk-history-blockers (parts &optional operation)
  "Return human-readable preflight blockers across compound PARTS.
OPERATION, when non-nil, selects the exact undo or redo identity."
  (let (out)
    (dolist (part parts)
      (let* ((buffer (plist-get part :buffer))
             (name (file-name-nondirectory (or (plist-get part :file) "source"))))
        (cond
         ((not (buffer-live-p buffer))
          (push (format "%s gone" name) out))
         ;; Exact pass-2 parts are governed solely by opaque-token resolution.
         ;; Independently undoing an ahead hook edit necessarily changes the
         ;; chars tick and raw head while restoring the safe org-air tail.
         ((plist-member part :expected-undo)
          (unless (org-air-view--history-expected-safe-p part operation)
            (push (org-air-view--history-ahead-edit-name part) out)))
         ((not (eql (plist-get part :tick)
                    (buffer-chars-modified-tick buffer)))
          (push (format "%s changed since" name) out))
         ((not (org-air-view--history-identity-match-p
                (plist-get part :undo-head)
                (org-air-view--undo-head buffer)))
          (push (format "%s history step missing" name) out)))))
    (nreverse out)))

(defun org-air-view--bulk-history-command-blockers (parts operation)
  "Return all command-time blockers for PARTS and OPERATION.
Identity/tick checks run for every part first.  Only when those pass does the
expected-history durability predicate read each governed source file, still
before any undo primitive, relocation marker, save, or ring mutation."
  (or (org-air-view--bulk-history-blockers parts operation)
      (delq nil (mapcar #'org-air-view--history-durability-blocker parts))))

(defun org-air-view--history-command-blockers (record operation)
  "Return command-time blockers for RECORD's inverse OPERATION.
Ordinary records without `:expected-undo' deliberately retain the R73/R75
stale-edit law and perform no durability reads here."
  (if (eq (plist-get record :kind) 'bulk)
      (org-air-view--bulk-history-command-blockers
       (plist-get record :parts) operation)
    (when (plist-member record :expected-undo)
      (cond
       ((not (org-air-view--history-expected-safe-p record operation))
        (list (org-air-view--history-ahead-edit-name record)))
       (t
        (when-let* ((blocker
                     (org-air-view--history-durability-blocker record)))
          (list blocker)))))))

(defun org-air-view--history-operation-function (record operation)
  "Return the exact Emacs undo function for RECORD's OPERATION.
Ordinary redo uses `undo-redo'.  After an independently undone recursive hook,
the bounded expected org-air redo tail is instead exposed through Emacs' undo
equivalence chain; `undo-only' skips the hook redo and applies that exact tail."
  (if (eq operation 'undo)
      #'undo-only
    (let ((expected (plist-get record :expected-undo))
          (buffer (plist-get record :buffer)))
      (if (and expected
               (eq (gethash record org-air-view--cache-sync-history)
                   'intervening-commit)
               (not (org-air-view--history-identity-match-p
                     expected (org-air-view--expected-redo-step buffer)))
               (org-air-view--history-identity-match-p
                expected (org-air-view--expected-undo-step buffer)))
          #'undo-only
        #'undo-redo))))

(defun org-air-view--history-apply-operation (record operation)
  "Apply RECORD OPERATION through its exact undo primitive.
A resolved recursive-hook redo starts a fresh `undo-only' sequence.  Its
expected raw tail is temporarily made the end of the redo-equivalence skip:
this skips only the independently undone hook group, applies the exact org-air
tail, and then restores Emacs' older mapping unconditionally."
  (let ((function (org-air-view--history-operation-function record operation)))
    (if (and (eq operation 'redo) (eq function #'undo-only))
        (let* ((expected
                (org-air-view--history-identity-resolve
                 (plist-get record :expected-undo)))
               (missing (list 'missing))
               (mapping (and (consp expected)
                             (gethash expected undo-equiv-table missing))))
          (unless (consp expected)
            (user-error "Expected recursive-hook history step is unavailable"))
          (unwind-protect
              (progn
                ;; `undo-only' normally follows the desired org-air redo tail's
                ;; own equivalence too and skips it.  Stop exactly at the
                ;; bounded token's raw tail for this one operation.
                (remhash expected undo-equiv-table)
                (let ((last-command nil))
                  (funcall function)))
            (if (eq mapping missing)
                (remhash expected undo-equiv-table)
              (puthash expected mapping undo-equiv-table))))
      (funcall function))))

(defun org-air-view--buffer-attempt-snapshot (buffer)
  "Capture BUFFER state needed to recover one ephemeral history attempt.
The source text lives only in this call's dynamic operation scope and is
never attached to an edit-ring record."
  (with-current-buffer buffer
    (let ((minimum (point-min))
          (maximum (point-max))
          (position (point))
          (mark-position (mark t)))
      (save-restriction
        (widen)
        (list :text (buffer-substring (point-min) (point-max))
              :multibyte enable-multibyte-characters
              :minimum minimum :maximum maximum
              :point position :mark mark-position :mark-active mark-active
              :modified (buffer-modified-p)
              :saved-size buffer-saved-size
              :visited-modtime (visited-file-modtime)
              :undo-list buffer-undo-list
              :pending-undo pending-undo-list
              :undo-equiv (and (hash-table-p undo-equiv-table)
                               (copy-hash-table undo-equiv-table))
              :this-command this-command
              :last-command last-command)))))

(defun org-air-view--buffer-attempt-restore (buffer snapshot)
  "Restore BUFFER from ephemeral SNAPSHOT after a failed history attempt.
Bytes, restriction, modified flag and undo machinery return to their
pre-attempt state.  Character ticks necessarily advance while restoring;
the caller restamps every affected metadata record to that honest tick."
  (let ((source (generate-new-buffer " *org-air-history-restore*" t)))
    (unwind-protect
        (progn
          (with-current-buffer source
            (set-buffer-multibyte (plist-get snapshot :multibyte))
            (insert (plist-get snapshot :text)))
          (with-current-buffer buffer
            (save-restriction
              (widen)
              (let ((buffer-undo-list t)
                    (inhibit-read-only t))
                (replace-buffer-contents source))
              (narrow-to-region (plist-get snapshot :minimum)
                                (plist-get snapshot :maximum))
              (goto-char (plist-get snapshot :point)))
            (if-let* ((position (plist-get snapshot :mark)))
                (set-marker (mark-marker) position buffer)
              (set-marker (mark-marker) nil))
            (setq mark-active (plist-get snapshot :mark-active)
                  buffer-undo-list (plist-get snapshot :undo-list)
                  buffer-saved-size (plist-get snapshot :saved-size))
            (restore-buffer-modified-p (plist-get snapshot :modified))
            (set-visited-file-modtime
             (plist-get snapshot :visited-modtime)))
          ;; These undo walkers are global in Emacs even though the undo list
          ;; itself is buffer-local; restore them only after text recovery.
          (setq pending-undo-list (plist-get snapshot :pending-undo)
                undo-equiv-table (plist-get snapshot :undo-equiv)
                this-command (plist-get snapshot :this-command)
                last-command (plist-get snapshot :last-command)))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(defun org-air-view--bulk-history-sync-file
    (part relocations state &optional intervening-reason)
  "Finalize PART through RELOCATIONS/STATE with optional INTERVENING-REASON."
  (org-air-view--cache-sync-finalize
   (plist-get part :file) relocations state intervening-reason))

(defun org-air-view--bulk-history-requeue (record ring)
  "Push compound RECORD directly onto RING without clearing its peer."
  (dolist (part (plist-get record :parts))
    (org-air-view--history-track-buffer (plist-get part :buffer)))
  (set ring (cons record (symbol-value ring)))
  (org-air-view--history-ring-truncate ring))

(defun org-air-view--bulk-history-restamp-part
    (part operation &optional save-result)
  "Restamp PART for the inverse of OPERATION using SAVE-RESULT identity.
TWO DIFFERENT FACTS meet here and they must never be confused.  EXPECTED is
the DIRECTION-DEPENDENT next step the inverse ring action will consume — the
exact identity `:expected-undo' arms when a later hook has put a user step
ahead of it.  HEAD is the AUTHORITATIVE POST-COMMIT undo head of the state
org-air just left in this buffer: the one save result's own head
\(`org-air-view--save-result-undo-head', sampled INSIDE the save attempt right
after org-air's own `undo-boundary'), or — when this call is recovering an
attempt org-air itself just rolled back — that restored buffer's head, taken
in the same instant as its tick.  ONLY HEAD MAY EVER REACH `:undo-head'.

After a REDO the two diverge, and that divergence was the defect: the head
tail carries an `undo-equiv-table' entry whenever the previous change in that
buffer was itself a ring undo, so `org-air-view--expected-undo-step' follows
the equivalence chain PAST the head — exactly as `undo-only' does — and names
the step beyond it.  Stamping that into `:undo-head' left the guard's two
halves describing two different buffer states, so a compound that had been
undone and redone once could never be undone again: it refused forever with
\"history step missing\" and, because a blocked compound is requeued at the
head of the same ring, it shadowed every older record too — including records
in files the marked command never touched (README:292 promises one `u'/`U'
round-trips the whole marked command; it round-tripped exactly once).

The `:tick'/`:undo-head' pair therefore travels through THE ONE WRITER every
other restamp path already uses (`org-air-view--history-restamp-pair'), which
refuses to write half a guard: with no proof (HEAD nil) or a head that is no
longer the buffer's own, it stamps NOTHING and leaves the part honestly
blocked and retryable rather than blessing a state org-air did not produce.
`:expected-undo' is retired only when that pair really was refreshed, so a
part can never be left governed by a stale pair alone."
  (let* ((buffer (plist-get part :buffer))
         (inverse (if (eq operation 'undo) 'redo 'undo))
         (tick (or (plist-get save-result :expected-tick)
                   (buffer-chars-modified-tick buffer)))
         ;; Committed: the save result's own head, or nil for the no-proof
         ;; case.  Restored: org-air itself just put the buffer back, so its
         ;; head IS the authority and is read in the same instant as TICK.
         (head (if save-result
                   (org-air-view--save-result-undo-head save-result)
                 (org-air-view--history-undo-head-step buffer)))
         (expected
          (or (and save-result
                   (plist-get save-result
                              (if (eq operation 'undo)
                                  :expected-redo :expected-undo)))
              (if (eq operation 'undo)
                  (org-air-view--expected-undo-step buffer)
                (org-air-view--expected-redo-step buffer))))
         (paired
          (org-air-view--history-restamp-pair part buffer tick head)))
    (cond
     ;; A restored failed attempt keeps its existing exact retry contract.
     ((and (null save-result) (plist-member part :expected-undo))
      (setq part
            (org-air-view--history-identity-put
             part :expected-undo expected)))
     ;; A committed operation needs exact metadata only if its own later hook
     ;; has now put a new user step ahead of the inverse ring action.
     ((org-air-view--save-result-ahead-p save-result buffer inverse)
      (setq part
            (org-air-view--history-identity-put
             part :expected-undo expected)))
     ;; Retire the exact identity ONLY when the pair really was refreshed:
     ;; dropping it beside an unrefreshed pair would hand the part back to a
     ;; guard describing a buffer state that no longer exists.
     ((and paired (plist-member part :expected-undo))
      (setq part
            (org-air-view--history-identity-remove
             part :expected-undo))))))

(defun org-air-view--history-arm-next
    (ring buffer expected tick)
  "Arm RING's next BUFFER record with exact EXPECTED identity and TICK.
Only the first local in-place record (or compound part) can represent the next
buffer undo/redo step.  A same-buffer structural record stops the search: its
cut must never be made automatically undoable through an older record."
  (when expected
    (let ((records (symbol-value ring)) found)
      (while (and records (not found))
        (let ((record (pop records)))
          (if (eq (plist-get record :kind) 'bulk)
              (when-let* ((part
                           (seq-find
                            (lambda (candidate)
                              (eq (plist-get candidate :buffer) buffer))
                            (plist-get record :parts))))
                ;; Arm the ONE exact identity and nothing else.  EXPECTED is
                ;; the direction-dependent next step, NOT a head, and this
                ;; path exists precisely because org-air can no longer prove
                ;; what it left in BUFFER — so the part's `:tick'/`:undo-head'
                ;; pair, one fact about one older buffer state, is left
                ;; exactly as it stands rather than half-refreshed from a
                ;; value that is not a head at all.  `:expected-undo' governs
                ;; the part from here (`org-air-view--bulk-history-blockers'
                ;; consults it first) and only an authoritative pair refresh
                ;; (`org-air-view--bulk-history-restamp-part') retires it.
                (setq part
                      (org-air-view--history-identity-put
                       part :expected-undo expected))
                (setq found t))
            (when (eq (plist-get record :buffer) buffer)
              (setq found t)
              (unless (memq (plist-get record :kind) '(refile archive))
                (plist-put record :tick tick)
                (setq record
                      (org-air-view--history-identity-put
                       record :expected-undo expected))))))))))

(defun org-air-view--history-restamp-committed
    (buffer tick expected head ring)
  "Restamp BUFFER's ring records only against its OWN authoritative TICK.
TICK is the post-commit `buffer-chars-modified-tick' org-air itself left in
BUFFER — `:expected-tick', captured INSIDE the save attempt at commit time,
never sampled again afterwards.  R75 Decision 5 gives a restamp its whole
meaning: the ring op restored exactly the state the neighbouring records were
stamped against, so the guard keeps saying \"no NON-ring change intervened\".
A restamp may therefore only ever record a change org-air MADE.  A compound
`u'/`U' commits its files one at a time but sweeps the committed buffers ONCE,
at command end, so a LATER part's own committed `after-save-hook' can move an
EARLIER, already-written part's buffer in between; re-sampling the tick there
would absorb that third-party change into the new stamp and BLESS the user's
unsaved text — the next `u' would pass a guard that must refuse, `undo-only'
the USER's newest group, save it away, and still claim an org-air step.

So verify before stamping.  When BUFFER is still exactly where org-air left
it, restamp with that authoritative TICK.  On any mismatch stamp NOTHING and
arm the one exact EXPECTED next identity on RING instead — precisely what the
same-buffer hook path does — leaving every other record for BUFFER honestly
blocked and retryable rather than blessed.

HEAD is the SAME commit instant's undo head identity
\(`org-air-view--save-result-undo-head'), and it is what a compound part's
`:undo-head' half of the guard is refreshed to, atomically with its `:tick'.
The two authoritative facts are never split and never re-derived here: a head
sampled at sweep time would be trivially equal to the buffer's head and would
bless a foreign group that moved it, exactly as a re-sampled tick would bless
foreign text.  EXPECTED remains the next SAME-DIRECTION step armed on RING,
which after an undo is a different tail from HEAD."
  (when (buffer-live-p buffer)
    (if (eql tick (buffer-chars-modified-tick buffer))
        (org-air-view--edit-ring-restamp buffer tick head)
      (org-air-view--history-arm-next ring buffer expected tick))))

(defun org-air-view--bulk-history-restamp-committed (parts authority ring)
  "Sweep committed compound PARTS, restamping only truly untouched buffers.
AUTHORITY is the command's `(BUFFER TICK EXPECTED HEAD)' alist, one entry per
committed buffer, recorded from that buffer's OWN save result at ITS commit
instant.  TICK and HEAD are the two halves of one guard and travel as one
fact.  A part whose buffer has no authority entry is never stamped: with no
proof of what org-air last left there, blessing is not an option.  RING is the
side a same-buffer next step would be popped from, so a skipped buffer's exact
next identity is armed there (`org-air-view--history-restamp-committed')."
  (dolist (part parts)
    (let* ((buffer (plist-get part :buffer))
           (entry (assq buffer authority)))
      (org-air-view--history-restamp-committed
       buffer (nth 1 entry) (nth 2 entry) (nth 3 entry) ring))))

(defun org-air-view--single-history-restamp (record save-result operation)
  "Restamp RECORD from SAVE-RESULT for inverse of committed OPERATION."
  (let* ((buffer (plist-get record :buffer))
         (inverse (if (eq operation 'undo) 'redo 'undo))
         (expected
          (plist-get save-result
                     (if (eq operation 'undo)
                         :expected-redo :expected-undo))))
    (plist-put record :tick (plist-get save-result :expected-tick))
    (if (org-air-view--save-result-ahead-p save-result buffer inverse)
        (setq record
              (org-air-view--history-identity-put
               record :expected-undo expected))
      (when (plist-member record :expected-undo)
        (setq record
              (org-air-view--history-identity-remove
               record :expected-undo))))))

(defun org-air-view--single-history-operation (record operation)
  "Apply single RECORD OPERATION without ever undoing an intervening edit."
  (let* ((buffer (plist-get record :buffer))
         (file (expand-file-name (plist-get record :file)))
         (source-ring (if (eq operation 'undo)
                          'org-air-view--edit-ring
                        'org-air-view--edit-redo-ring))
         (target-ring (if (eq operation 'undo)
                          'org-air-view--edit-redo-ring
                        'org-air-view--edit-ring))
         (cache-sync-mode (gethash record org-air-view--cache-sync-history))
         (cache-sync (and cache-sync-mode t))
         (intervening (eq cache-sync-mode 'intervening-commit))
         (finalizer-status 'ok)
         relocations snapshot operation-error save-result)
    (condition-case err
        (when (and cache-sync (not intervening))
          (setq relocations (org-air-view--relocation-markers file buffer)))
      (error (setq operation-error err)))
    (if operation-error
        (progn
          (org-air-view--edit-ring-requeue record buffer source-ring)
          (message "Cannot %s: %s — %s"
                   operation (plist-get record :desc)
                   (error-message-string operation-error)))
      (setq snapshot (org-air-view--buffer-attempt-snapshot buffer))
      (unwind-protect
          (progn
            (condition-case err
                (with-current-buffer buffer
                  (org-with-wide-buffer
                   (undo-boundary)
                   (org-air-view--history-apply-operation record operation)))
              ((error quit) (setq operation-error err)))
            (unless operation-error
              (org-air-view--relocation-arm-save-hooks relocations)
              (with-current-buffer buffer
                (org-with-wide-buffer
                 (let ((org-air-view--bulk-source-write t))
                   (setq save-result
                         (org-air-view--save-attempt
                          (when (and cache-sync (not intervening))
                            (lambda ()
                              (org-air-view--cache-sync-capture
                               relocations)))))))))
            (if (or operation-error
                    (not (plist-get save-result :committed)))
                (progn
                  (org-air-view--buffer-attempt-restore buffer snapshot)
                  ;; Restoration preserves an exact expected object but bumps
                  ;; the chars tick; keep an exceptional blocked record
                  ;; retryable without changing legacy/synthetic metadata.
                  (plist-put record :tick
                             (buffer-chars-modified-tick buffer))
                  (when (plist-member record :expected-undo)
                    (setq record
                          (org-air-view--history-identity-put
                           record :expected-undo
                           (if (eq operation 'undo)
                               (org-air-view--expected-undo-step buffer)
                             (org-air-view--expected-redo-step buffer)))))
                  (if (plist-member record :expected-undo)
                      (org-air-view--edit-ring-requeue
                       record buffer source-ring)
                    (org-air-view--history-record-discard record))
                  (message "Cannot %s: %s — %s"
                           operation (plist-get record :desc)
                           (cond
                            ((not operation-error)
                             "source save failed before writing")
                            ((and (eq operation 'redo)
                                  (not (plist-member record :expected-undo)))
                             (format "no redo step left in %s"
                                     (buffer-name buffer)))
                            (t (error-message-string operation-error)))))
              ;; The disk step committed.  Cache-sync records finalize from
              ;; the pre-hook state while a later legitimate hook edit remains
              ;; live and modified behind its separate undo boundary.
              (when cache-sync
                (setq finalizer-status
                      (org-air-view--cache-sync-finalize
                       file relocations (plist-get save-result :state)
                       (when (or intervening
                                 (plist-get save-result :recursive-commit))
                         (format
                          "intervening committed save hook%s"
                          (if-let* ((failure (plist-get save-result :error)))
                              (format ": %s" (error-message-string failure))
                            "")))))
                (when (and (eq finalizer-status 'invalidated)
                           (plist-get save-result :recursive-commit))
                  (plist-put save-result :intervening-warning-reported t)
                  (puthash record 'intervening-commit
                           org-air-view--cache-sync-history)))
              (org-air-view--single-history-restamp
               record save-result operation)
              (org-air-view--edit-ring-requeue record buffer target-ring)
              ;; ONE COMMITTED-BUFFER RESTAMP LAW for both paths.  The
              ;; authority is the tick org-air itself left behind at commit
              ;; time, never a later sample: an untouched buffer keeps the
              ;; classic two-sided stamps, while a later hook edit ahead of the
              ;; next same-buffer record arms that one exact step instead of
              ;; restamping it to the user's unsafe head.
              (org-air-view--history-restamp-committed
               buffer
               (plist-get save-result :expected-tick)
               (plist-get save-result
                          (if (eq operation 'undo)
                              :expected-undo :expected-redo))
               (org-air-view--save-result-undo-head save-result)
               source-ring)
              (if cache-sync
                  (progn
                    (org-air-view--refresh-current)
                    (org-air-view--panes-resync-now))
                ;; Preserve the historic full re-query only when no ordinary
                ;; hook inserted a live step after the captured identity.
                (if (and (eq (plist-get save-result
                                         (if (eq operation 'undo)
                                             :expected-redo :expected-undo))
                             (if (eq operation 'undo)
                                 (org-air-view--expected-redo-step buffer)
                               (org-air-view--expected-undo-step buffer)))
                         (eql (plist-get save-result :expected-tick)
                              (buffer-chars-modified-tick buffer)))
                    (org-air-refresh)
                  (org-air-view--persistent-warning
                   (format "board refresh deferred: unsaved edit in %s is ahead; resolve it, then run g r"
                           (file-name-nondirectory file)))))
              (org-air-view--report-save-warning save-result)
              (message (if (eq operation 'undo)
                           "Undid: %s (%d more)"
                         "Redid: %s (%d more redoable)")
                       (plist-get record :desc)
                       (length (symbol-value source-ring)))))
        (org-air-view--relocation-release relocations)
        (setq snapshot nil)))))

(defun org-air-view--bulk-history-operation (record operation)
  "Apply compound RECORD OPERATION (`undo' or `redo') honestly.
All history and total relocation sets preflight before any bytes move.
Runtime processing is reverse commit order for undo and commit order for
redo, stopping on the first true pre-write/operation failure.  A signal
after disk commit is finalized as success and only emits a bounded warning."
  (let* ((parts (plist-get record :parts))
         (ordered (if (eq operation 'undo) (reverse parts) parts))
         (blockers (org-air-view--bulk-history-blockers parts operation))
         (desc (plist-get record :desc))
         (relocations-by-file (make-hash-table :test #'equal)))
    ;; Cache locators are part of compound preflight too.  Keep every marker
    ;; live across the operation so no later file is discovered incomplete
    ;; only after an earlier file has committed.
    (unless blockers
      (dolist (part parts)
        (let ((file (plist-get part :file))
              (buffer (plist-get part :buffer)))
          (condition-case nil
              (puthash file
                       (unless (eq (gethash part
                                            org-air-view--cache-sync-history)
                                   'intervening-commit)
                         (org-air-view--relocation-markers file buffer))
                       relocations-by-file)
            (error
             (push (format "%s source locators changed; run g r"
                           (file-name-nondirectory file))
                   blockers))))))
    (setq blockers (nreverse blockers))
    (if blockers
        (progn
          (maphash (lambda (_file relocations)
                     (org-air-view--relocation-release relocations))
                   relocations-by-file)
          ;; The caller popped RECORD before dispatch.  A preflight blocker
          ;; changes zero bytes and remains retryable on the same ring side.
          (org-air-view--bulk-history-requeue
           record (if (eq operation 'undo)
                      'org-air-view--edit-ring
                    'org-air-view--edit-redo-ring))
          (message "Cannot %s: %s — %s"
                   operation desc (mapconcat #'identity blockers "; ")))
      (let* ((remaining ordered) (successes nil) (committed nil) (failed nil)
             warnings (generation-invalidated nil)
             ;; The side a same-buffer NEXT step is popped from: where an
             ;; exact identity is armed whenever a buffer may not be stamped.
             (source-ring (if (eq operation 'undo)
                              'org-air-view--edit-ring
                            'org-air-view--edit-redo-ring))
             ;; AUTHORITY: one `(BUFFER TICK EXPECTED)' entry per committed
             ;; buffer, recorded from that part's own save result AT ITS
             ;; COMMIT INSTANT.  The command-final sweeps run long after the
             ;; last part's `after-save-hook', so they may never sample a
             ;; buffer's tick themselves — they verify against this.
             (authority nil))
        (while (and remaining (not failed) (not generation-invalidated))
          (let* ((part (pop remaining))
                 (buffer (plist-get part :buffer))
                 (file (plist-get part :file))
                 ;; CROSS-FILE COMPOUND TOCTOU.  One command-wide preflight is
                 ;; not permission for a LATER file: an earlier part's own
                 ;; committed `after-save-hook' can mutate this part's source
                 ;; between that preflight and this iteration.  Applying the
                 ;; stale authorization would consume the user's newly-ahead
                 ;; undo group and still cross the whole record as complete.
                 ;; Revalidate this exact part's identity (`:expected-undo'
                 ;; token / tick / head as applicable) and, for expected-
                 ;; history parts, the command durability predicate, strictly
                 ;; before its own undo primitive, attempt snapshot,
                 ;; relocation arming and save.  No state is popped or
                 ;; touched by the check itself.
                 (stale (org-air-view--bulk-history-command-blockers
                         (list part) operation))
                 ;; Undo walkers do not compose safely with change groups.
                 ;; Keep only ephemeral attempt state, never history payload.
                 (snapshot (unless stale
                             (org-air-view--buffer-attempt-snapshot buffer)))
                 (relocations (gethash file relocations-by-file))
                 (intervening
                  (eq (gethash part org-air-view--cache-sync-history)
                      'intervening-commit))
                 (operation-error nil)
                 (save-result nil)
                 (ok nil))
            (if stale
                ;; A BLOCKED STOP, never a pre-write failure: zero bytes move
                ;; in this part, no wrong undo group is consumed, and its
                ;; exact retry identity/tick is deliberately left untouched
                ;; (restamping here would bless the user's ahead step).  The
                ;; already-committed prefix stays committed and the record
                ;; can never cross as complete; this part's relocation
                ;; markers are still owned by RELOCATIONS-BY-FILE and are
                ;; released exactly once by the command-wide sweep below.
                (setq failed part)
              (unwind-protect
                  (progn
                    (condition-case err
                        (with-current-buffer buffer
                          (org-with-wide-buffer
                           (undo-boundary)
                           (org-air-view--history-apply-operation
                            part operation)))
                      ((error quit) (setq operation-error err)))
                    (unless operation-error
                      (org-air-view--relocation-arm-save-hooks relocations)
                      (with-current-buffer buffer
                        (org-with-wide-buffer
                         (let ((org-air-view--bulk-source-write t))
                           (setq save-result
                                 (org-air-view--save-attempt
                                  (unless intervening
                                    (lambda ()
                                      (org-air-view--cache-sync-capture
                                       relocations)))))))))
                    (if (or operation-error
                            (not (plist-get save-result :committed)))
                        (progn
                          ;; Only a true pre-write/undo failure restores the
                          ;; attempt.  Disk and live bytes stay equal.
                          (org-air-view--buffer-attempt-restore
                           buffer snapshot)
                          (org-air-view--bulk-history-restamp-part
                           part operation)
                          (org-air-view--edit-ring-restamp buffer)
                          (setq failed part))
                      (let ((status
                             (org-air-view--bulk-history-sync-file
                              part relocations (plist-get save-result :state)
                              (when (or intervening
                                        (plist-get save-result
                                                   :recursive-commit))
                                (format
                                 "intervening committed save hook%s"
                                 (if-let* ((failure
                                            (plist-get save-result :error)))
                                     (format ": %s"
                                             (error-message-string failure))
                                   ""))))))
                        (when (eq status 'invalidated)
                          (setq generation-invalidated t)
                          (when (plist-get save-result :recursive-commit)
                            (plist-put save-result
                                       :intervening-warning-reported t)
                            (puthash part 'intervening-commit
                                     org-air-view--cache-sync-history))))
                      (org-air-view--bulk-history-restamp-part
                       part operation save-result)
                      (let ((tick (plist-get save-result :expected-tick))
                            (expected
                             (plist-get save-result
                                        (if (eq operation 'undo)
                                            :expected-undo :expected-redo)))
                            (head (org-air-view--save-result-undo-head
                                   save-result)))
                        ;; Record what org-air really left in this buffer
                        ;; before any later part can move it again — both
                        ;; halves of the guard, from the one save result.
                        (setq authority
                              (cons (list buffer tick expected head)
                                    (assq-delete-all buffer authority)))
                        (when (not (eql tick
                                        (buffer-chars-modified-tick buffer)))
                          (org-air-view--history-arm-next
                           source-ring buffer expected tick)))
                      (push part successes)
                      (when (or (plist-get save-result :error)
                                (plist-get save-result :recursive-commit))
                        (push save-result warnings))
                      (setq ok t)))
                (org-air-view--relocation-release relocations)
                (remhash file relocations-by-file)
                (setq snapshot nil))
              (unless ok (setq failed part)))))
        ;; THE COMMITTED SET IS READ MANY TIMES BELOW, SO IT IS BUILT ONCE,
        ;; NON-DESTRUCTIVELY.  SUCCESSES is accumulated newest-first by
        ;; `push'; every incomplete branch needs it in commit order for the
        ;; redo residual, needs to iterate it for the forgotten identities
        ;; and the committed-buffer restamp sweep, and needs its LENGTH for
        ;; the honest K/N echo.  A destructive `nreverse' would leave the
        ;; accumulator naming the original head cons — a one-element tail —
        ;; so the echo would claim `1/N' however many files really moved and
        ;; the sweep would restamp only one of several committed buffers.
        ;; `reverse' keeps this structurally identical to the undo residual's
        ;; own non-destructive `reverse' forms.
        (setq committed (reverse successes))
        (maphash (lambda (_file relocations)
                   (org-air-view--relocation-release relocations))
                 relocations-by-file)
        (clrhash relocations-by-file)
        ;; Persist diagnostics first; the branch-specific concise status below
        ;; remains the final success/incomplete minibuffer echo.
        (dolist (warning (nreverse warnings))
          (org-air-view--report-save-warning warning))
        (cond
         (failed
          ;; A runtime split never creates a speculative redo branch.  BLOCKED
          ;; parts share this residual law exactly: whether the part was
          ;; restored after a true pre-write failure or was never touched at
          ;; all, zero of its bytes moved.
          (org-air-view--history-ring-clear
           'org-air-view--edit-redo-ring)
          (let ((residual
                 (if (eq operation 'undo)
                     ;; FAILED was restored to its committed pre-attempt state
                     ;; (or, when BLOCKED, never left it), so it and every
                     ;; untouched suffix remain undoable.
                     (reverse (cons failed remaining))
                   ;; Successfully reapplied parts alone are undoable.
                   committed)))
            (when residual
              (org-air-view--bulk-history-requeue
               (list :desc (format "%s (residual %d file%s)"
                                   desc (length residual)
                                   (if (= (length residual) 1) "" "s"))
                     :kind 'bulk :parts residual :time (current-time))
               'org-air-view--edit-ring)))
          (dolist (part (if (eq operation 'undo)
                            committed
                          (cons failed remaining)))
            (org-air-view--history-part-forget-identities part))
          (when committed
            (org-air-view--bulk-history-restamp-committed
             committed authority source-ring)
            (org-air-view--refresh-current)
            (org-air-view--panes-resync-now))
          (message "%s incomplete: %d/%d files %s; failed %s"
                   (if (eq operation 'undo) "Undo" "Redo")
                   (length committed) (length parts)
                   (if (eq operation 'undo) "reverted" "reapplied")
                   (file-name-nondirectory (plist-get failed :file))))
         ((and generation-invalidated remaining)
          ;; The current part committed, but final disk truth replaced the
          ;; generation.  Stop detached-file work and expose only honest
          ;; residual undo; no speculative redo survives.
          (org-air-view--history-ring-clear
           'org-air-view--edit-redo-ring)
          (let ((residual (if (eq operation 'undo)
                              (reverse remaining)
                            committed)))
            (when residual
              (org-air-view--bulk-history-requeue
               (list :desc (format "%s (residual %d file%s)"
                                   desc (length residual)
                                   (if (= (length residual) 1) "" "s"))
                     :kind 'bulk :parts residual :time (current-time))
               'org-air-view--edit-ring)))
          (dolist (part (if (eq operation 'undo) committed remaining))
            (org-air-view--history-part-forget-identities part))
          (org-air-view--bulk-history-restamp-committed
           committed authority source-ring)
          ;; `org-air-view--items' is nil: this is the one safe rebuild.
          (org-air-view--refresh-current)
          (org-air-view--panes-resync-now)
          (message "%s incomplete: %d/%d files %s; cache generation rebuilt"
                   (if (eq operation 'undo) "Undo" "Redo")
                   (length committed) (length parts)
                   (if (eq operation 'undo) "reverted" "reapplied")))
         (t
          ;; Complete success: one compound record crosses rings as a unit.
          ;; THE RECORD GOES BACK ON ITS RING FIRST, BEFORE THE SWEEP — the
          ;; same order the incomplete branches above already use.  The sweep
          ;; (`org-air-view--edit-ring-restamp') iterates the two rings, so a
          ;; record requeued AFTER it is structurally invisible to the one
          ;; authoritative pair writer and its parts would be left to whatever
          ;; their own post-commit restamp wrote.  Requeueing first gives
          ;; every compound record — its own parts included — exactly ONE
          ;; restamp path, and both writes carry the identical authoritative
          ;; `(TICK . HEAD)' fact, so the second is an idempotent confirmation
          ;; rather than a second opinion.  The sweep's other branch arms the
          ;; next step on SOURCE-RING, which is the side the record just left,
          ;; so nothing here can arm the record against itself.
          (if (eq operation 'undo)
              (org-air-view--bulk-history-requeue
               record 'org-air-view--edit-redo-ring)
            (org-air-view--bulk-history-requeue
             record 'org-air-view--edit-ring))
          (org-air-view--bulk-history-restamp-committed
           committed authority source-ring)
          (org-air-view--refresh-current)
          (org-air-view--panes-resync-now)
          (message "%s: %s (%d file%s)"
                   (if (eq operation 'undo) "Undid" "Redid")
                   desc (length parts) (if (= (length parts) 1) "" "s"))))))))

(defun org-air-edit-undo ()
  "Undo the MOST RECENT recorded board edit (R73 Decisions 5 + 6).
Pops the newest record off the bounded recent-edits ring
`org-air-view--edit-ring' and dispatches on its kind — one press, one
record, never a cascade:

- an IN-PLACE record (done / todo / tag / priority / schedule /
  deadline / category / note / kill — all single-buffer) is undone via
  `undo-only' in ITS OWN buffer — NEVER plain `undo', whose second
  non-consecutive press REDOES the first undo; `undo-only' skips redo
  entries by construction, so successive presses WALK BACK — guarded
  by a `buffer-chars-modified-tick' check (a manual/external change
  since the edit consumes the record with a message instead of eating
  the WRONG change; after a successful undo the same-buffer records
  are re-stamped — two-sided since R75 — so the ring's own steps never
  trip it), then saved + refreshed — the R73-1 resync rides the
  refresh tail, so the row AND the pane/inspector revert in the same
  motion — and the reverted record moves onto the REDO ring
  \(`org-air-view--edit-redo-ring', tick re-stamped post-undo), so `U'
  \(`org-air-edit-redo') can re-apply it (R75 Decision 1);
- a STRUCTURAL record (refile / archive — a cross-buffer
  delete+insert) is honestly NOT undone: consumed with a message
  naming the manual path (a source-side undo would resurrect the item
  beside its moved copy — a silent duplicate, worse than no undo);
- a DEAD record (source buffer killed) is consumed with a message.

Only the SUCCESS branch feeds the redo ring — the consumed-without-
revert branches (structural, dead, tick-tripped) reverted nothing, so
pushing would advertise a redo that does not exist (R75 Decision 1;
structural records are therefore not-redoable BY CONSTRUCTION).

`?' shows the ring (the Recent edits block); `u' undoes the top.
Named `org-air-edit-undo' — `u' now covers every board edit, not just
triage dispositions; `org-air-triage-undo' stays as a defalias."
  (interactive)
  (unless org-air-view--edit-ring
    (user-error "Nothing to undo"))
  (let* ((rec (car org-air-view--edit-ring))
         (desc (plist-get rec :desc))
         (blockers (org-air-view--history-command-blockers rec 'undo)))
    (if blockers
        ;; Leave the exact ring cons, record, tokens and every source/UI fact
        ;; untouched when command-time identity or durability preflight fails.
        (message "Cannot undo: %s — %s"
                 desc (mapconcat #'identity blockers "; "))
      (setq rec (pop org-air-view--edit-ring))
      (let ((buf (plist-get rec :buffer))
            (kind (plist-get rec :kind))
            (next (car org-air-view--edit-ring)))
        (cond
         ;; R90 compound metadata history: preflight every file before one
         ;; reverse-commit-order undo and one final cached repaint.
         ((eq kind 'bulk)
          (org-air-view--bulk-history-operation rec 'undo))
         ;; Decision 6: structural honesty — named, never a duplicate-making
         ;; partial undo.
         ((memq kind '(refile archive))
          (org-air-view--history-record-discard rec)
          (message "Not simply undoable: %s — it moved; %s%s"
                   desc
                   (if (eq kind 'refile)
                       "refile it back (e) from there."
                     "restore it from the archive file.")
                   (if next
                       (format "  Next u: %s" (plist-get next :desc))
                     "")))
         ;; Decision 5 step 1: ordinary dead records are consumed.
         ((not (buffer-live-p buf))
          (org-air-view--history-record-discard rec)
          (message "Cannot undo: %s — source buffer gone" desc))
         ;; Preserve R73's established stale-user-edit degrade for ordinary
         ;; records: consume it, move zero bytes, and name the changed buffer.
         ((and (not (plist-member rec :expected-undo))
               (not (eql (plist-get rec :tick)
                         (buffer-chars-modified-tick buf))))
          (org-air-view--history-record-discard rec)
          (message "Cannot undo: %s — %s changed since"
                   desc (buffer-name buf)))
         (t
          (org-air-view--single-history-operation rec 'undo)))))))

(defalias 'org-air-triage-undo #'org-air-edit-undo
  "Renamed to `org-air-edit-undo' (R73) — `u' now undoes every board
edit through the bounded recent-edits ring, not just the last triage
disposition.  Kept as an alias for muscle memory, the process-inbox
`?u' route, and test pins.")

(defun org-air-edit-redo ()
  "Re-apply the edit `u' just reverted (R75 Decisions 1–5).
Pops the newest record off the redo ring
`org-air-view--edit-redo-ring' — fed exclusively by
`org-air-edit-undo''s success branch, so a refile/archive record can
NEVER be here (consumed without reversal, structural records are
not-redoable BY CONSTRUCTION — Decision 4, the R64 duplicate class
closed on both sides) — and re-applies it via ONE buffer-level
`undo-redo' step in the record's OWN buffer.  The R73/R73fix
one-edit-one-group + one-undo-one-step discipline makes `undo-redo'
\(the purpose-built partner of `undo-only'; Emacs 28.1+, our floor is
29.1) re-apply EXACTLY the reverted edit in one step — including its
flushed log line, since the group carried both — byte-identical to
the post-edit state, and LINEARISE the undo list so a later `u'
walks back through the same edit again (Decision 3, batch-verified).

Guards mirror `u' (Decision 5): a dead source buffer or a
manual/external change since the undo (the chars-tick guard — ring ops
re-stamp two-sided, a real edit does not) consumes the record with a
message, never an error; the `undo-redo' call itself is
condition-case-caught (the theoretical `undo-limit' GC chain break
with no char change — R53's never-error law), degrading to a named
message with zero bytes moved.  On success: save, two-sided restamp,
the record moves BACK onto the undo ring head DIRECTLY
\(`org-air-view--edit-ring-requeue' — never the push choke point,
which would clear the redo remainder), and the refresh tail carries
the R73-1 pane/inspector resync — so repeated `u'/`U' walk the ring
both ways, byte-exact.  A FRESH edit clears the redo ring at the push
choke point (standard undo/redo semantics — Decision 1); the explicit
`undo-boundary' before `undo-redo' is harmless (the
last-change-was-undo lookup skips leading boundaries) and kept for
symmetry with the undo side's Lisp-landed-buffer ruling.

Bound to `U' — the board's own shift-pair inverse idiom (v/V, s/S,
o/O); `C-r' was rejected (it shadows `isearch-backward' in a
read-only board where isearch is a real navigation path)."
  (interactive)
  (unless org-air-view--edit-redo-ring
    (user-error "Nothing to redo"))
  (let* ((rec (car org-air-view--edit-redo-ring))
         (desc (plist-get rec :desc))
         (blockers (org-air-view--history-command-blockers rec 'redo)))
    (if blockers
        (message "Cannot redo: %s — %s"
                 desc (mapconcat #'identity blockers "; "))
      (setq rec (pop org-air-view--edit-redo-ring))
      (let ((kind (plist-get rec :kind))
            (buf (plist-get rec :buffer)))
        (cond
         ;; R90 compound metadata history: forward commit order, all-part
         ;; preflight, one final cached repaint.
         ((eq kind 'bulk)
          (org-air-view--bulk-history-operation rec 'redo))
         ;; Decision 5 step 2: ordinary dead records are consumed.
         ((not (buffer-live-p buf))
          (org-air-view--history-record-discard rec)
          (message "Cannot redo: %s — source buffer gone" desc))
         ;; Ordinary later user edits retain R75's consumed stale-record law.
         ((and (not (plist-member rec :expected-undo))
               (not (eql (plist-get rec :tick)
                         (buffer-chars-modified-tick buf))))
          (org-air-view--history-record-discard rec)
          (message "Cannot redo: %s — %s changed since"
                   desc (buffer-name buf)))
         (t
          (org-air-view--single-history-operation rec 'redo)))))))

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
  (org-air-view--single-mutation-guard "Inbox processing")
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
                                      "[f]ile [t]ag [T]odo [b]acklog [a]rchive "
                                      "[D]one [k]ill "
                                      "┆ [SPC]skip [p]rev [u]ndo [U]redo "
                                      "[g]refresh [q]uit ")
                              n))))
            (pcase key
              (?s (call-interactively #'org-air-item-schedule))
              (?d (call-interactively #'org-air-item-deadline))
              (?r (call-interactively #'org-air-refile-item))
              (?f (call-interactively #'org-air-item-file-group))
              (?t (call-interactively #'org-air-set-tag))
              (?T (org-air-item-cycle-todo))
              ;; R83: `b' is a DEFER disposition — the single-home backlog
              ;; gate drops the item out of the `inbox' bucket, so the
              ;; guided walk's countdown advances like any filing verb.
              (?b (org-air-item-backlog))
              (?a (org-air-item-archive))
              (?D (org-air-item-done))
              (?k (org-air-item-kill))
              (?u (org-air-triage-undo))
              (?U (org-air-edit-redo))
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

(defun org-air-view--day-owner ()
  "Resolve the OWNER board buffer for a day-open (R55-1).
Resolution order, first live hit wins: the current buffer when it is an
`org-air-view-mode' board (the IDENTITY tier — inline calendar, day-nav,
batch); else the rail's R25-6 back-pointer `org-air-rail--board-buffer'
when live AND a board (a PROJECT/REVISIT owner falls through — their
rails carry no day cells); else the `*org-air*' buffer when live.
Signals `user-error' otherwise — a day-open NEVER renders into whatever
buffer happens to be current."
  (or (and (derived-mode-p 'org-air-view-mode) (current-buffer))
      (let ((owner org-air-rail--board-buffer))
        (and (buffer-live-p owner)
             (with-current-buffer owner
               (derived-mode-p 'org-air-view-mode))
             owner))
      (let ((board (get-buffer org-air-view-buffer-name)))
        (and (buffer-live-p board) board))
      (user-error "No org-air board to focus")))

(defun org-air-view--day-owner-window (owner)
  "Return a live MAIN window showing OWNER on the selected frame, or nil.
MAIN means nil `window-side' parameter and nil dedication — by
construction never the dedicated `*org-air-rail*' side window (R55-1)."
  (catch 'hit
    (dolist (w (get-buffer-window-list owner nil (selected-frame)))
      (when (and (window-live-p w)
                 (not (window-parameter w 'window-side))
                 (not (window-dedicated-p w)))
        (throw 'hit w)))
    nil))

(defun org-air-view--day-focus-owner (owner)
  "Leave focus in a MAIN window showing OWNER after a day-open (R55-1).
When the selected window already shows OWNER (the inline/identity tier)
this is a no-op — focus behaviour byte-identical to trunk.  Otherwise hop
to OWNER's main window (the `org-air-rail-return' pattern); when OWNER
has no live main window (a between-reconciles foreign-rail state, or
`M-x' from an unrelated window), `display-buffer' it — never into the
invoking (possibly rail) window, never into a dedicated side window, and
never via `switch-to-buffer' (which errors in a dedicated window).
Batch never routes (the R26-5 `noninteractive' gate: no live windows to
speak of, and the goldens must not move)."
  (unless (or noninteractive
              (eq (window-buffer (selected-window)) owner))
    (let ((win (org-air-view--day-owner-window owner)))
      (if (window-live-p win)
          (select-window win)
        ;; `display-buffer-use-some-window' skips dedicated windows (it can
        ;; never squat the rail); `inhibit-same-window' keeps it out of the
        ;; invoking window.
        (let ((shown (display-buffer
                      owner
                      '((display-buffer-reuse-window
                         display-buffer-use-some-window)
                        (inhibit-same-window . t)))))
          (when (window-live-p shown)
            (select-window shown)))))))

;;;###autoload
(defun org-air-view-day (&optional date)
  "Focus the single-day view (R6) on DATE, the calendar day at point, or today.
The item pane becomes that day's items grouped Deadline / Scheduled /
Logged.  `q' or `g' returns to the full board; `<'/`>' move to the
adjacent day; the rail calendar re-centres on the focused month.

R55-1 (owner-routed): day state is applied to, and the re-render runs
in, the OWNER board buffer — resolved by `org-air-view--day-owner', NOT
`(current-buffer)' — and focus lands in a MAIN window showing it,
regardless of where the command was invoked (rail side window, inline
cell, board, `M-x').  Under the R49-3 default `side-window' placement
the calendar's day cells live in the dedicated `*org-air-rail*' side
window; the naive body rendered the day pane INTO that window and
trapped focus there.  The rail buffer/window are NEVER rendered into,
reused, resized or deleted by a day-open — the only writer of the rail
buffer stays the rail render path (the single-writer law).  Rendering
in the owner also uses the owner's cached `org-air-view--items', so a
day-open queries NOTHING (trunk's rail-buffer render fell back to a
synchronous `org-air-query-items' re-scan on a keypress)."
  (interactive)
  ;; Step 1: read the cell date AT THE INVOCATION POINT — the rail's (or
  ;; inline) calendar cell — before any buffer switch.  The DATE argument
  ;; (day-nav) wins outright; the `org-air-view--day'/today fallbacks are
  ;; owner state and are read in the OWNER buffer below.
  (let ((cell (get-text-property (point) 'org-air-day))
        (owner (org-air-view--day-owner)))
    ;; Step 2: day state + re-render in the OWNER.  The owner's render
    ;; tail re-shows the rail via `org-air-rail--show' (the calendar
    ;; re-centres on the focused month); the rail stays the rail.
    (with-current-buffer owner
      (let ((day (or date cell org-air-view--day (current-time))))
        (setq org-air-view--day day
              org-air-view--cal-month day)
        ;; R79 D4: swap the sort key vocabulary to the day view's and
        ;; coerce the active key (shared priority/title carry across).
        (org-air-view--enter-day-sort)
        (org-air-view--render-current)))
    ;; Steps 3/4: window routing — hop to the owner's MAIN window.
    (org-air-view--day-focus-owner owner)))

(defun org-air-goto-date--read-date ()
  "Read a target date for `org-air-goto-date' via `org-read-date'.
Full Org date entry (absolute, relative like \"+3d\"/\"fri\", the
calendar popup); returns an internal time at midnight.  DEFAULT-TIME is
the owner board's focused day when a day view is up (so a re-jump
nudges from the SHOWN day), else today.  Owner resolution is LENIENT
here (no board yet -> today): `org-air-goto-date' opens the board
itself (R78 Decision 4), so the reader must not pre-empt it with the
R55-1 `user-error'."
  (let* ((owner (ignore-errors (org-air-view--day-owner)))
         (default (and owner
                       (buffer-local-value 'org-air-view--day owner))))
    (org-read-date nil t nil "Jump to date: " default)))

;;;###autoload
(defun org-air-goto-date (date)
  "Jump the org-air board to DATE's single-day view (R78).
Interactively, prompt with the full `org-read-date' vocabulary.  DATE
is an internal time.  Delegates to `org-air-view-day', so the R55-1
owner routing applies unchanged: the day renders in the OWNER board
from its CACHED items (no rescan — R53) and focus lands in a MAIN
window.  With no live board anywhere, opens `org-air-view' first (the
`org-air-process-inbox' precedent), then jumps.  `<'/`>' step days
from the landed date; `q' returns to the full board (R28-2)."
  (interactive (list (org-air-goto-date--read-date)))
  (unless (ignore-errors (org-air-view--day-owner))
    (org-air-view))
  (org-air-view-day date))

(defun org-air-view-board ()
  "Leave the single-day view and return to the full board (R6)."
  (interactive)
  (when org-air-view--day
    (setq org-air-view--day nil)
    ;; R79 D4: restore the board sort key vocabulary and coerce back.
    (org-air-view--leave-day-sort)
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

(defconst org-air-help-buffer-name "*org-air-help*"
  "Name of the org-air help buffer (R50-2).")

(define-derived-mode org-air-help-mode special-mode "Org-Air-Help"
  "Major mode for the `*org-air-help*' buffer (R50-2).
A normal read-only, scrollable buffer (SPC/DEL page via the
`special-mode' parent; evil motions apply under evil).  `q' quits back to
the origin window via the PARENT `special-mode' binding (`quit-window'),
so it works even with `org-air-use-default-keybindings' nil — the knob
only clears installer-owned keys, and org-air installs none here."
  (setq-local truncate-lines nil)
  (setq-local line-spacing org-air-line-spacing)
  ;; R58: a trivial record (context symbol only) so a `*org-air-help*'
  ;; buffer in a saved layout can never raise the activities.el error.
  (setq-local bookmark-make-record-function
              #'org-air-help--bookmark-make-record))

(defvar-local org-air-help--context-sym nil
  "Help context symbol this `*org-air-help*' buffer was rendered for (R58).
Set by `org-air-help--render' (after the mode call, which wipes locals);
read by the bookmark record producer.")

(defconst org-air-help--board-groups
  '(("Navigation"
     (org-air-next-item . "next item")
     (org-air-prev-item . "previous item")
     (org-air-toggle-section . "toggle section fold")
     (org-air-next-section . "next section")
     (org-air-prev-section . "previous section")
     (org-air-goto-top . "top of board")
     (org-air-goto-bottom . "bottom of board")
     (org-air-peek-item . "peek at item (keep focus)")
     (org-air-visit-item . "visit item")
     (org-air-visit-item-stay . "visit item, stay on board")
     (org-air-calendar-prev . "calendar: previous month")
     (org-air-calendar-next . "calendar: next month")
     (org-air-calendar-today . "calendar: today")
     (org-air-goto-date . "jump to a date's items (day view)"))
    ("Triage"
     (org-air-capture . "capture")
     (org-air-refile-item . "edit item (a destination refiles)")
     (org-air-item-deadline . "set deadline")
     (org-air-set-tag . "add tag")
     (org-air-item-cycle-todo . "set todo state")
     (org-air-item-backlog . "backlog / un-backlog item (defer off attention)")
     (org-air-item-archive . "archive item")
     (org-air-item-done . "mark done")
     (org-air-item-kill . "kill item")
     (org-air-edit-undo . "undo last edit (ring; ? shows recent)")
     (org-air-edit-redo . "redo last undo (a new edit clears redo)")
     (org-air-toggle-mark . "mark for bulk (b backlog, t add tag)")
     (org-air-clear-marks . "clear all marks")
     (org-air-process-inbox . "process inbox (guided walk)"))
    ("Filter & sort"
     (org-air-filter . "filter by tag / text / date (is:overdue, due:7d…)")
     (org-air-filter-clear . "clear filter")
     (org-air-filter-toggle-match . "toggle AND/OR combinator")
     (org-air-view-sort-cycle . "cycle sort key")
     (org-air-view-sort-reverse . "reverse sort"))
    ("Source & scope"
     (org-air-scope . "source lens (file/group/all)")
     (org-air-scope-clear . "clear source lens"))
    ("Columns"
     (org-air-toggle-origin . "toggle origin (filename) column")
     (org-air-toggle-dates . "toggle dates column")
     (org-air-toggle-tags . "toggle tags column"))
    ("Rail"
     (org-air-rail-toggle . "pop rail out/in")
     (org-air-rail-return . "focus the rail"))
    ("Refresh"
     (org-air-refresh . "refresh")
     (org-air-refresh-all . "refresh all (drop caches)"))
    ("Session"
     (org-air-project . "project tree")
     (org-air-revisit . "revisit dusty notes")
     (org-air-quit . "quit")
     (org-air-help . "this help")))
  "BOARD help groups: (TITLE . ((COMMAND . DESCRIPTION) …)) (R50-2).
Key text is NEVER stored here — it is derived at render time from the
ACTUAL live keymaps of the origin buffer, so the help can never lie.")

(defconst org-air-help--project-groups
  '(("Navigation"
     (org-air-project-next . "next doc")
     (org-air-project-prev . "previous doc")
     (org-air-project-open . "open doc (same window)")
     (org-air-project-visit . "visit doc (other window)")
     (org-air-project-toggle-dropped . "reveal/fold dropped docs"))
    ("Display"
     (org-air-project-toggle-filenames . "flip filename/title")
     (org-air-project-group-by-state . "group by state")
     (org-air-project-group-by-directory . "group by directory")
     (org-air-project-group-by-tag . "group by tag")
     (org-air-view-sort-cycle . "cycle sort key")
     (org-air-view-sort-reverse . "reverse sort"))
    ("Filter"
     (org-air-project-filter . "filter by doc tags (live)")
     (org-air-filter-clear . "clear filter")
     (org-air-filter-toggle-match . "toggle AND/OR combinator"))
    ("Rail"
     (org-air-rail-toggle . "pop rail out/in"))
    ("Refresh"
     (org-air-project-refresh . "refresh"))
    ("Session"
     (org-air-revisit . "revisit dusty notes")
     (org-air-project-quit . "quit")
     (org-air-help . "this help")))
  "PROJECT help groups: (TITLE . ((COMMAND . DESCRIPTION) …)) (R50-2).")

(defconst org-air-help--doc-groups
  '(("Session"
     (org-air-project-back . "back to the project tree"))
    ("Navigation"
     (org-air--repeat-next . "next heading (repeatable)")
     (org-air--repeat-prev . "previous heading (repeatable)"))
    ("Rail"
     (org-air-rail-toggle . "pop rail out/in"))
    ("Help"
     (org-air-help . "this help")))
  "DOC-SESSION help groups: (TITLE . ((COMMAND . DESCRIPTION) …)) (R50-2).
The doc file buffer is EDITABLE, so these derive to the
`org-air-leader-key' leader forms plus the direct back-verb binding.")

(defun org-air-help--context (buffer)
  "Return the help context symbol for BUFFER (R50-2).
`board' / `project' / `revisit' / `review' / `doc-session'; anything
else falls back to `board' (matching the old echo-area fallback)."
  (with-current-buffer buffer
    (cond
     ((derived-mode-p 'org-air-project-mode) 'project)
     ((derived-mode-p 'org-air-revisit-mode) 'revisit)
     ((derived-mode-p 'org-air-review-mode) 'review)
     ((derived-mode-p 'org-air-view-mode) 'board)
     ((and (boundp 'org-air-doc-session-mode) org-air-doc-session-mode)
      'doc-session)
     (t 'board))))

(defun org-air-help--groups (context)
  "Return the group table for help CONTEXT (R50-2)."
  (pcase context
    ('project org-air-help--project-groups)
    ('doc-session org-air-help--doc-groups)
    ;; R54-3: the revisit groups live in org-air-revisit.el (module split).
    ('revisit (if (boundp 'org-air-revisit--help-groups)
                  (symbol-value 'org-air-revisit--help-groups)
                org-air-help--board-groups))
    ;; R61-4: the review groups live in org-air-review.el (module split).
    ('review (if (boundp 'org-air-review--help-groups)
                 (symbol-value 'org-air-review--help-groups)
               org-air-help--board-groups))
    (_ org-air-help--board-groups)))

(defun org-air-help--context-title (context)
  "Return the human name of help CONTEXT for the title line (R50-2)."
  (pcase context
    ('project "project")
    ('revisit "revisit")
    ('review "review")
    ('doc-session "doc session")
    (_ "board")))

(defun org-air-help--insert-header (label)
  "Insert a help section header for LABEL, rail-header idiom (R50-2).
The rail's prefix-marker + `org-air-face-rail-header' face family — no
new faces, no theme surface growth."
  (insert (propertize (org-air-layout-glyph 'rail-marker)
                      'face 'org-air-face-rail-marker)
          " "
          (propertize label 'face 'org-air-face-rail-header)
          "\n"))

(defun org-air-help--rows (cells origin)
  "Resolve CELLS ((COMMAND . DESC) …) against ORIGIN's live maps (R50-2).
Returns ((KEY-TEXT LIVE-P . DESC) …): KEY-TEXT via
`org-air-view--legend-key' (`where-is' in ORIGIN, so prefix and leader
sequences render naturally, and an evil rebinding, a custom
`org-air-leader-key' or a user `define-key' all show what is REALLY
bound).  A command with NO live key (knob off, user unbind)
yields a faded `M-x command-name' cell instead of a lie."
  (mapcar (lambda (cell)
            (let ((key (org-air-view--legend-key (car cell) origin)))
              (if key
                  (cons key (cons t (cdr cell)))
                (cons (concat "M-x " (symbol-name (car cell)))
                      (cons nil (cdr cell))))))
          cells))

(defun org-air-help ()
  "Show org-air key bindings in the `*org-air-help*' buffer (R50-2).
A dedicated, formatted, scrollable help view (mu4e/magit style) that
replaces the old one-line echo-area message: grouped sections, one
binding per row, KEY derived from the ACTUAL active keymaps of the
origin buffer (`where-is'), so it is correct under evil, a custom
`org-air-leader-key', and `org-air-use-default-keybindings' both ways.
Context-aware: board / project / doc-session pick their own group set.
Displayed via `pop-to-buffer'; `q' (`quit-window', the `special-mode'
parent binding) restores the origin window.
R58: the render body lives in `org-air-help--render' (the bookmark
handler re-renders undisplayed); this command is the render + display."
  (interactive)
  (let* ((origin (current-buffer))
         (context (org-air-help--context origin))
         (buffer (get-buffer-create org-air-help-buffer-name)))
    (org-air-help--render buffer context origin)
    (pop-to-buffer buffer)
    buffer))

(defun org-air-help--render (buffer context origin)
  "Render the CONTEXT help view into BUFFER, keys derived from ORIGIN (R58).
The extracted render core of `org-air-help' (byte-identical output): the
command wraps it with `pop-to-buffer'; the bookmark handler calls it
directly and never displays (the restorer owns the windows).  ORIGIN is
the buffer whose live keymaps resolve the KEY column; a dead or
unrelated ORIGIN degrades to `M-x' cells, never an error.  Returns
BUFFER."
  (let ((groups (org-air-help--groups context))
        (title (format "org-air help — %s"
                       (org-air-help--context-title context)))
        (mark-counts
         (and (eq context 'board)
              (buffer-live-p origin)
              (with-current-buffer origin
                (and org-air-view--marked-keys
                     (cons (length org-air-view--marked-keys)
                           (org-air-view--marked-shown-count
                            org-air-view--items)))))))
    (with-current-buffer buffer
      (org-air-help-mode)
      ;; R58: remember the context for the bookmark record producer (the
      ;; mode call above ran `kill-all-local-variables').
      (setq-local org-air-help--context-sym context)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq-local header-line-format
                    (list (propertize (concat " " title)
                                      'face 'org-air-face-rail-header)))
        (org-air-help--insert-header title)
        (dolist (group groups)
          (insert "\n")
          (org-air-help--insert-header (car group))
          (let* ((rows (org-air-help--rows (cdr group) origin))
                 ;; KEY right-padded to the widest key in the SECTION.
                 (keyw (apply #'max (mapcar (lambda (r)
                                              (string-width (car r)))
                                            rows))))
            (pcase-dolist (`(,key ,live . ,desc) rows)
              (insert "  "
                      (propertize key 'face (if live
                                                'org-air-face-rail-key
                                              'org-air-face-faded))
                      (make-string (- keyw (string-width key)) ?\s)
                      "  "
                      (propertize desc 'face 'org-air-face-faded)
                      "\n"))))
        ;; R90: a conditional selection explainer.  Empty marks emit no
        ;; bytes, preserving every pre-R90 help fixture.
        (when mark-counts
          (insert "\n")
          (org-air-help--insert-header "Marked items")
          (insert "  "
                  (propertize
                   (format "%d marked · %d shown; hidden marks are included"
                           (car mark-counts) (cdr mark-counts))
                   'face 'org-air-face-faded)
                  "\n"
                  "  "
                  (propertize "b backlogs all · t adds one tag to all · M clears marks"
                              'face 'org-air-face-faded)
                  "\n"
                  "  "
                  (propertize "Other mutations are single-item; press M first"
                              'face 'org-air-face-faded)
                  "\n"))
        ;; R73 Decision 8 + R75 Decision 7: the Recent-edits block —
        ;; newest first, at most 5 rows per side, `u' undoes the top /
        ;; `U' redoes the top.  The gate is EITHER-ring-non-empty
        ;; (undoing the only edit leaves undo empty + redo populated —
        ;; the block must still render); each sub-list is independently
        ;; empty-suppressed, so every help golden/mockup (rendered with
        ;; fresh rings) stays byte-clean.  No transient, no new key: `?'
        ;; is already the \"what just happened / what can I do\" surface.
        (when (or (and (boundp 'org-air-view--edit-ring)
                       org-air-view--edit-ring)
                  (and (boundp 'org-air-view--edit-redo-ring)
                       org-air-view--edit-redo-ring))
          (insert "\n")
          (org-air-help--insert-header "Recent edits")
          (when (and (boundp 'org-air-view--edit-ring)
                     org-air-view--edit-ring)
            (insert "  "
                    (propertize "u undoes the top" 'face 'org-air-face-faded)
                    "\n")
            (let ((n 0))
              (dolist (rec (seq-take org-air-view--edit-ring 5))
                (setq n (1+ n))
                (let* ((kind (plist-get rec :kind))
                       (rbuf (plist-get rec :buffer))
                       (suffix (cond ((memq kind '(refile archive))
                                      " [not simply undoable]")
                                     ((eq kind 'bulk)
                                      (if (org-air-view--bulk-history-blockers
                                           (plist-get rec :parts))
                                          " [part blocked]"
                                        ""))
                                     ((not (buffer-live-p rbuf)) " [gone]")
                                     (t ""))))
                  (insert "  "
                          (propertize (format "%d." n)
                                      'face 'org-air-face-rail-key)
                          " "
                          (propertize (concat (plist-get rec :desc) suffix)
                                      'face 'org-air-face-faded)
                          "\n")))))
          ;; R75: the redo sub-list — `[not simply undoable]' CANNOT
          ;; occur here (structural records never enter the redo ring,
          ;; Decision 4); only the `[gone]' suffix applies.
          (when (and (boundp 'org-air-view--edit-redo-ring)
                     org-air-view--edit-redo-ring)
            (insert "  "
                    (propertize "U redoes the top" 'face 'org-air-face-faded)
                    "\n")
            (let ((n 0))
              (dolist (rec (seq-take org-air-view--edit-redo-ring 5))
                (setq n (1+ n))
                (let* ((rbuf (plist-get rec :buffer))
                       (suffix
                        (if (eq (plist-get rec :kind) 'bulk)
                            (if (org-air-view--bulk-history-blockers
                                 (plist-get rec :parts))
                                " [part blocked]"
                              "")
                          (if (buffer-live-p rbuf) "" " [gone]"))))
                  (insert "  "
                          (propertize (format "%d." n)
                                      'face 'org-air-face-rail-key)
                          " "
                          (propertize (concat (plist-get rec :desc) suffix)
                                      'face 'org-air-face-faded)
                          "\n"))))))
        (goto-char (point-min))))
    buffer))

;;;###autoload
(defun org-air-visit-item (&optional item display)
  "Visit ITEM's original Org heading.
When ITEM is nil, use the item at point in an org-air dashboard.  DISPLAY
controls window choice and defaults to `org-air-visit-display'."
  (interactive)
  (let ((item (or item (get-text-property (point) 'org-air-item))))
    (unless item
      (user-error "No org-air item at point"))
    ;; R54-3: an org-air-initiated open — feed the opt-in visit ledger.
    (org-air--note-visited (org-air-item-file item))
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
  "Enable `org-air-return-mode', recording window CONFIG and ORIGIN (T4).
R35.1: the `org-air-return-key' binding is gated on
`org-air-use-default-keybindings' — with the knob nil org-air installs NO
key in the user's OWN visited file buffer (and still honours the existing
empty `org-air-return-key' opt-out).  When gated off, any stale binding of
the current key is actively REMOVED from the shared return map so a prior
knob-on visit does not leave a key behind."
  (setq org-air-view--visit-config config
        org-air-view--visit-origin origin)
  (when (and (stringp org-air-return-key)
             (not (string-empty-p org-air-return-key)))
    (if org-air-use-default-keybindings
        (define-key org-air-return-mode-map (kbd org-air-return-key)
                    #'org-air-return)
      (define-key org-air-return-mode-map (kbd org-air-return-key) nil t)))
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

;;;; ---------------------------------------------------------------------
;;;; R58 — Emacs bookmark support (activities.el / burly / `C-x r m').
;;;; ---------------------------------------------------------------------
;;
;; Every org-air view buffer provides a buffer-local
;; `bookmark-make-record-function' and an autoloadable handler, so the
;; bookmark-driven session restorers (activities.el, burly.el, plain
;; `bookmark-jump') can save and rebuild the views.  Records carry the
;; VIEW-defining state plus a durable (FILE . POS) locator for point (the
;; R53 marker-slot model — never a raw buffer position) and ONLY
;; printable values (`bookmark-save' writes the alist with `prin1').
;; Handlers re-enter the EXISTING cache-first entry cores with display
;; suppressed and NEVER touch windows: `bookmark--jump-via' hands
;; `current-buffer' to the caller's DISPLAY-FUNCTION — activities.el
;; places it into ITS restored layout, and a handler that popped windows
;; would fight that layout.  org-air's own record keys are
;; `org-air-'-prefixed so no future bookmark.el reserved key can collide.

(declare-function org-air-project-bookmark-jump "org-air-project")
(declare-function org-air-revisit-bookmark-jump "org-air-revisit")
(declare-function org-air-review-bookmark-jump "org-air-review")

(defconst org-air-view--bookmark-version 1
  "Schema version stamped into every org-air bookmark record (R58).
Readers treat every field as optional (unknown fields ignored, missing
fields defaulted), so a version mismatch reads best-effort — never a
signal into bookmark.el / activities.el.")

(defconst org-air-view--bookmark-header-keys
  '(org-air-version org-air-view org-air-context)
  "The per-record header trio every org-air bookmark record carries (R58).
Stripped by `org-air-view--bookmark-host-fields' when a dependent
buffer's record embeds its host's state fields (the rail delegation), so
the embedding record keeps its OWN header.")

(defun org-air-view--bookmark-epoch (time)
  "Return TIME as a printable integer epoch, or nil (R58 rule 5).
Times are stored as integer epochs (`time-convert') — an Emacs time
object must never enter a bookmark record."
  (and time (ignore-errors (time-convert time 'integer))))

(defun org-air-view--bookmark-header (view handler location names)
  "Return the shared org-air bookmark record header alist (R58).
VIEW is the record's view symbol, HANDLER the jump function, LOCATION
the string `bookmark-location' displays in the bmenu list (views are not
file-visiting, so records carry `location' instead of `filename'), and
NAMES the `defaults' name candidates, most specific first.
`org-air-context' carries the live `org-air-view--cache-key' —
DIAGNOSTIC ONLY: a jump always opens under the user's LIVE configuration
\(a bookmark restores a VIEW, never an old configuration)."
  (list (cons 'handler handler)
        (cons 'location location)
        (cons 'defaults names)
        (cons 'org-air-version org-air-view--bookmark-version)
        (cons 'org-air-view view)
        (cons 'org-air-context (org-air-view--cache-key))))

(defun org-air-view--bookmark-host-fields (record)
  "Return RECORD's embeddable `org-air-…' state fields (R58 delegation).
Drops bookmark.el's reserved keys (`handler', `defaults', `location'… —
anything not `org-air-'-prefixed) and the header trio
`org-air-view--bookmark-header-keys', leaving exactly the view-defining
state a dependent buffer's record embeds from its host."
  (seq-filter (lambda (cell)
                (and (consp cell) (symbolp (car cell))
                     (string-prefix-p "org-air-" (symbol-name (car cell)))
                     (not (memq (car cell)
                                org-air-view--bookmark-header-keys))))
              (and (listp record) record)))

(defun org-air-view--bookmark-scan (prop pred)
  "Return the first position whose text PROP value satisfies PRED, or nil.
A pure text-property scan over the rendered buffer (R58): never opens a
file, bounded by the buffer size.  PRED is called only on non-nil
values."
  (let ((pos (point-min)) (found nil))
    (while (and (not found) pos (< pos (point-max)))
      (let ((val (get-text-property pos prop)))
        (if (and val (funcall pred val))
            (setq found pos)
          (setq pos (next-single-property-change pos prop nil (point-max))))))
    found))

(defun org-air-view--bookmark-locator-of (record)
  "Return the point-locator slot RECORD arms, or nil (R58).
A plist (:item (FILE . POS) :title TITLE); either half may be absent —
the consume chain falls through accordingly.  Malformed fields are
dropped, never signalled on."
  (let ((item (cdr (assq 'org-air-item record)))
        (title (cdr (assq 'org-air-item-title record))))
    (setq item (and (consp item) (stringp (car item))
                    (cons (car item)
                          (if (integerp (cdr item)) (cdr item) 1)))
          title (and (stringp title) title))
    (when (or item title)
      (list :item item :title title))))

;;; --- Board (`*org-air*') -----------------------------------------------

(defun org-air-view--bookmark-name ()
  "Return the board record's `defaults' candidates, most specific first (R58).
Day view first (\"org-air: day 2026-07-18\"), then the scoped board
\(\"org-air: board · file inbox.org\", · #tag, · group G), then the
generic \"org-air: board\"."
  (delete-dups
   (delq nil
         (list (and org-air-view--day
                    (format "org-air: day %s"
                            (format-time-string "%Y-%m-%d"
                                                org-air-view--day)))
               (pcase org-air-view--scope
                 (`(:file ,file)
                  (and (stringp file)
                       (format "org-air: board · file %s"
                               (file-name-nondirectory file))))
                 (`(:tag ,tag) (format "org-air: board · #%s" tag))
                 (`(:group ,group) (format "org-air: board · group %s" group))
                 (_ nil))
               "org-air: board"))))

(defun org-air-view--bookmark-item-fields ()
  "Return the (FILE . POS) locator fields for the row at point, or nil (R58).
The R53 marker-slot model: a scanned item's marker slot IS a durable
\(FILE . POS) cons; a live-capture item whose marker is a real marker
degrades to (FILE . POS) via `marker-position' at record time — a marker
must never enter the alist (rule 5).  Point on chrome (banner, section
heading, calendar) records NO item fields."
  (let ((item (org-air-view--row-property 'org-air-item)))
    (when item
      (let* ((file (org-air-item-file item))
             (m (org-air-item-marker item))
             (loc (cond ((and (consp m) (stringp (car m)))
                         (cons (car m)
                               (if (integerp (cdr m)) (cdr m) 1)))
                        ((and (markerp m) (stringp file))
                         (cons file (or (marker-position m) 1)))
                        ((stringp file) (cons file 1))))
             (title (org-air-item-title item)))
        (append (and loc (list (cons 'org-air-item loc)))
                (and (stringp title)
                     (list (cons 'org-air-item-title
                                 (substring-no-properties title)))))))))

(defun org-air-view--bookmark-make-record ()
  "Return the Emacs bookmark record for the current board buffer (R58).
Pure buffer-local reads plus one row text-property lookup — cheap enough
for activities.el's repeated timer saves, valid mid-refresh (no
in-flight machine state leaks into the alist) and NEVER signals: any
failure degrades to the bare header record, which restores a plain
board.  Deliberately NOT recorded: rail placement/pop state (window
arrangement is the restorer's domain), the column toggles (presentation
follows the user's live customisation) and the items themselves (data
comes from the cache/scan — records stay tiny)."
  (condition-case nil
      (append
       (org-air-view--bookmark-header (if org-air-view--day 'day 'board)
                                      'org-air-view-bookmark-jump
                                      "org-air: board"
                                      (org-air-view--bookmark-name))
       (and (consp org-air-view--scope)
            (list (cons 'org-air-scope org-air-view--scope)))
       (and org-air-view--tag-filter
            (list (cons 'org-air-filter org-air-view--tag-filter)))
       (list (cons 'org-air-sort
                   (cons (or org-air-view--sort-key org-air-sort-key)
                         (or org-air-view--sort-direction
                             org-air-sort-direction))))
       (and org-air-view--expanded-sections
            (list (cons 'org-air-expanded org-air-view--expanded-sections)))
       (let ((month (org-air-view--bookmark-epoch org-air-view--cal-month)))
         (and month (list (cons 'org-air-cal-month month))))
       (let ((day (org-air-view--bookmark-epoch org-air-view--day)))
         (and day (list (cons 'org-air-day day))))
       (org-air-view--bookmark-item-fields))
    (error (org-air-view--bookmark-header 'board
                                          'org-air-view-bookmark-jump
                                          "org-air: board"
                                          (list "org-air: board")))))

(defun org-air-view--bookmark-apply (record)
  "Apply RECORD's org-air view fields to the current board buffer (R58).
A pure `setq-local' translator: record fields → the view-defining
buffer-locals.  Every field is optional, unknown fields are IGNORED
\(forward compatibility) and a missing/malformed field lands on the mode
default — so a bare header record (or an `org-air-version' ≠ 1 record,
read best-effort through this same path) restores a plain board."
  (let ((scope (cdr (assq 'org-air-scope record)))
        (filter (cdr (assq 'org-air-filter record)))
        (sort (cdr (assq 'org-air-sort record)))
        (day (cdr (assq 'org-air-day record)))
        (month (cdr (assq 'org-air-cal-month record)))
        (expanded (cdr (assq 'org-air-expanded record))))
    (setq-local org-air-view--scope (and (consp scope) scope)
                org-air-view--tag-filter
                (and (or (stringp filter) (consp filter)) filter)
                org-air-view--expanded-sections
                (and (listp expanded) expanded)
                org-air-view--day
                (and (integerp day) (seconds-to-time day))
                org-air-view--cal-month
                (and (integerp month) (seconds-to-time month)))
    (when (and (consp sort)
               (car sort) (symbolp (car sort))
               (cdr sort) (symbolp (cdr sort)))
      (setq-local org-air-view--sort-key (car sort)
                  org-air-view--sort-direction (cdr sort)))
    ;; R90: restoring an explicit lens is the same explicit reveal request
    ;; as applying it interactively; raw `#backlog' remains collapse-neutral.
    (org-air-view--ensure-explicit-backlog-lens)))

(defun org-air-view--bookmark-consume ()
  "Land point per the armed bookmark locator; one-shot (R58).
Called at the tail of every successful full paint (the one choke point:
the sync path, the deferred one-shot, the stale paint, every R56
progressive paint and the machine's final swap).  Drift chain: exact
\(FILE . POS) marker row → same FILE + title (POS drifted) → same title
\(refiled) → stay ARMED while a refresh is still in flight, else clear
and leave the render's `org-air-view--goto-first-item' landing.  A pure
text-property scan; never opens a file, never signals."
  (when org-air-view--bookmark-locator
    (condition-case nil
        (let* ((slot org-air-view--bookmark-locator)
               (loc (plist-get slot :item))
               (title (plist-get slot :title))
               (pos (or (and loc (org-air-view--find-property
                                  'org-air-marker loc))
                        (and loc title
                             (org-air-view--bookmark-scan
                              'org-air-item
                              (lambda (it)
                                (and (org-air-item-p it)
                                     (equal (org-air-item-file it) (car loc))
                                     (equal (org-air-item-title it) title)))))
                        (and title
                             (org-air-view--bookmark-scan
                              'org-air-item
                              (lambda (it)
                                (and (org-air-item-p it)
                                     (equal (org-air-item-title it)
                                            title))))))))
          (cond
           (pos
            (setq org-air-view--bookmark-locator nil)
            (goto-char pos)
            (org-air-view--goto-row-title))
           ;; No row yet, machine still filling: stay armed for the next
           ;; paint; the finish swap runs with the state already idle, so
           ;; the slot can never survive past refresh-idle.
           ((eq org-air-view--refresh-state 'refreshing))
           (t
            (setq org-air-view--bookmark-locator nil)
            (org-air-view--goto-first-item))))
      (error (setq org-air-view--bookmark-locator nil)))))

;;;###autoload
(defun org-air-view-bookmark-jump (record)
  "Handler for org-air board bookmarks (R58).
Rebuilds `*org-air*' from RECORD without displaying it (the bookmark
caller owns display — the activities.el contract) and without a blocking
rescan (the R26-8/R56 cache-first machine).  Never signals: a malformed
or future-versioned RECORD degrades to a plain board open."
  (require 'org-air)
  (let ((buffer (get-buffer-create org-air-view-buffer-name)))
    (condition-case err
        (with-current-buffer buffer
          ;; R26-5 idempotent entry guard — identical to the command's.
          (unless (derived-mode-p 'org-air-view-mode)
            (org-air-view-mode))
          ;; 1. Saved view state FIRST, so the very first paint (skeleton
          ;;    aside) already composes the bookmarked view.
          (org-air-view--bookmark-apply record)
          ;; 2. Stash the point locator for the paint tail to consume.
          (setq org-air-view--bookmark-locator
                (org-air-view--bookmark-locator-of record))
          ;; 3. The entry core, display suppressed (R58 factoring).
          (org-air-view--open-core buffer nil))
      (error
       (message "org-air: bookmark restore degraded: %s"
                (org-air-view--short-error err))
       (with-current-buffer buffer
         (unless (derived-mode-p 'org-air-view-mode) (org-air-view-mode))
         (ignore-errors (org-air-view--open-core buffer nil)))))
    ;; The handler contract: make the target buffer CURRENT, never shown.
    (set-buffer buffer)))
;;;###autoload
(put 'org-air-view-bookmark-jump 'bookmark-handler-type "org-air")

;;; --- Rail (`*org-air-rail*', delegating) -------------------------------

(defun org-air-rail--bookmark-make-record ()
  "Return the DELEGATING bookmark record for the rail buffer (R58).
Shared header with view `rail' plus `org-air-host' (which view owns the
rail, from the back-pointer's major mode) and the HOST's full state
field set — obtained by calling the host buffer's own
`bookmark-make-record-function' through `org-air-rail--board-buffer' and
embedding its `org-air-…' fields.  A dead/absent host degrades to a bare
board-hosted rail record; never signals."
  (condition-case nil
      (let* ((host (and (buffer-live-p org-air-rail--board-buffer)
                        org-air-rail--board-buffer))
             (host-kind
              (and host
                   (with-current-buffer host
                     (cond ((derived-mode-p 'org-air-project-mode) 'project)
                           ((derived-mode-p 'org-air-revisit-mode) 'revisit)
                           ((derived-mode-p 'org-air-review-mode) 'review)
                           ((derived-mode-p 'org-air-view-mode) 'board)))))
             (host-record
              (and host-kind
                   (with-current-buffer host
                     (and (local-variable-p 'bookmark-make-record-function)
                          (functionp bookmark-make-record-function)
                          (funcall bookmark-make-record-function)))))
             (host-name (or (car-safe (cdr (assq 'defaults host-record)))
                            "org-air: board")))
        (append
         (org-air-view--bookmark-header 'rail 'org-air-rail-bookmark-jump
                                        "org-air: rail"
                                        (list (concat host-name " · rail")))
         (list (cons 'org-air-host (or host-kind 'board)))
         (org-air-view--bookmark-host-fields host-record)))
    (error (append (org-air-view--bookmark-header
                    'rail 'org-air-rail-bookmark-jump "org-air: rail"
                    (list "org-air: board · rail"))
                   (list (cons 'org-air-host 'board))))))

;;;###autoload
(defun org-air-rail-bookmark-jump (record)
  "Handler for org-air rail bookmarks (R58, delegating).
Restores the HOST view from the embedded fields in RECORD via the host's
own handler flow (no display), then recreates `*org-air-rail*' and
renders it from the host (`org-air-rail--render', batch-safe per R15
D-P2), leaving the RAIL current.  Restore-order independent: if the
host's own record restores later, its idempotent entry finds the buffer
already initialised — no double init (the R26-5 guard).  Never touches
windows; never signals."
  (require 'org-air)
  (condition-case err
      (let* ((host-kind (let ((h (cdr (assq 'org-air-host record))))
                          (if (memq h '(board project revisit review))
                              h 'board)))
             (handler (pcase host-kind
                        ('project #'org-air-project-bookmark-jump)
                        ('revisit #'org-air-revisit-bookmark-jump)
                        ('review #'org-air-review-bookmark-jump)
                        (_ #'org-air-view-bookmark-jump))))
        ;; The host's handler applies the embedded fields, arms the point
        ;; locator and re-enters the cache-first core — and leaves the
        ;; host current.
        (funcall handler record)
        (let ((host (current-buffer))
              (rail (org-air-rail--get-buffer)))
          (org-air-rail--render host
                                (org-air-rail--window-cols org-air-view-width))
          (set-buffer rail)))
    (error
     (message "org-air: bookmark restore degraded: %s"
              (org-air-view--short-error err))
     (set-buffer (org-air-rail--get-buffer)))))
;;;###autoload
(put 'org-air-rail-bookmark-jump 'bookmark-handler-type "org-air")

;;; --- Entry pane (`*org-air-view*', delegating) -------------------------

(defun org-air-view-pane--bookmark-make-record ()
  "Return the DELEGATING bookmark record for the snapshot pane (R58).
Shared header with view `entry', `org-air-host' and `org-air-entry-ctx'
— the (FILE . POS) of the snapshot's source, from the printable stash
the snapshot writer left (`org-air-view-pane--bookmark-ctx').  Never
signals; a stash-less pane records a bare degrading header."
  (condition-case nil
      (let* ((ctx org-air-view-pane--bookmark-ctx)
             (file (nth 0 ctx))
             (pos (nth 1 ctx))
             (title (nth 2 ctx))
             (host (nth 3 ctx))
             (host-name (pcase host
                          ('project "org-air: project")
                          ('revisit "org-air: revisit")
                          ('review "org-air: review")
                          (_ "org-air: board"))))
        (append
         (org-air-view--bookmark-header 'entry
                                        'org-air-entry-view-bookmark-jump
                                        "org-air: entry"
                                        (list (concat host-name " · entry")))
         (list (cons 'org-air-host
                     (if (memq host '(board project revisit review))
                         host 'board)))
         (and (stringp file)
              (list (cons 'org-air-entry-ctx
                          (cons file (if (integerp pos) pos 1)))))
         (and (stringp title)
              (list (cons 'org-air-item-title title)))))
    (error (append (org-air-view--bookmark-header
                    'entry 'org-air-entry-view-bookmark-jump
                    "org-air: entry" (list "org-air: board · entry"))
                   (list (cons 'org-air-host 'board))))))

;;;###autoload
(defun org-air-entry-view-bookmark-jump (record)
  "Handler for org-air entry-pane bookmarks (R58, delegating).
Ensures the host board exists (undisplayed, through the same idempotent
entry guard the commands use — a LIVE board is left untouched, so
restore order never matters), recreates the pane buffer and renders the
read-only snapshot for RECORD's (FILE . POS) source — one bounded file
read, the pane's normal render path.  A vanished source shows the pane's
existing missing-source rendering, never a signal.  Leaves the PANE
current; never touches windows."
  (require 'org-air)
  (condition-case err
      (let ((board (get-buffer org-air-view-buffer-name)))
        (unless (and board
                     (with-current-buffer board
                       (derived-mode-p 'org-air-view-mode)))
          (save-current-buffer
            (org-air-view--open-core
             (get-buffer-create org-air-view-buffer-name) nil)))
        (let* ((src-loc (cdr (assq 'org-air-entry-ctx record)))
               (file (and (consp src-loc) (stringp (car src-loc))
                          (car src-loc)))
               (pos (and (consp src-loc) (integerp (cdr src-loc))
                         (cdr src-loc)))
               (title (let ((tt (cdr (assq 'org-air-item-title record))))
                        (and (stringp tt) tt)))
               (ctx (append (and file (list :file file
                                            :marker (cons file (or pos 1))))
                            (and title (list :title title))))
               (src (and file (org-air-view-pane--source-buffer-pos
                               (plist-get ctx :marker))))
               (pane (with-current-buffer
                         (get-buffer org-air-view-buffer-name)
                       (org-air-view-pane--render-snapshot ctx src))))
          (set-buffer pane)
          (goto-char (point-min))))
    (error
     (message "org-air: bookmark restore degraded: %s"
              (org-air-view--short-error err))
     (set-buffer (org-air-view-pane--buffer)))))
;;;###autoload
(put 'org-air-entry-view-bookmark-jump 'bookmark-handler-type "org-air")

;;; --- Help (`*org-air-help*', trivial) ----------------------------------

(defun org-air-help--bookmark-make-record ()
  "Return the trivial bookmark record for the help buffer (R58).
Header plus `org-air-help-context' (the `org-air-help--context' symbol
this buffer was rendered for) — included so no `*org-air-help*' in a
saved layout can ever raise the activities.el error."
  (append
   (org-air-view--bookmark-header 'help 'org-air-help-bookmark-jump
                                  "org-air: help" (list "org-air: help"))
   (list (cons 'org-air-help-context (or org-air-help--context-sym 'board)))))

;;;###autoload
(defun org-air-help-bookmark-jump (record)
  "Handler for org-air help bookmarks (R58).
Re-renders `*org-air-help*' for RECORD's stored context.  The keymap
origin is the matching live view buffer when one exists (so the KEY
column stays honest), else the help buffer itself (degrading to `M-x'
cells).  Leaves the help buffer current; never touches windows; never
signals."
  (require 'org-air)
  (let* ((context (let ((c (cdr (assq 'org-air-help-context record))))
                    (if (memq c '(board project revisit review doc-session))
                        c 'board)))
         (buffer (get-buffer-create org-air-help-buffer-name))
         (origin (or (pcase context
                       ('project (get-buffer "*org-air-project*"))
                       ('revisit (and (boundp 'org-air-revisit-buffer-name)
                                      (get-buffer
                                       org-air-revisit-buffer-name)))
                       ('review (and (boundp 'org-air-review-buffer-name)
                                     (get-buffer
                                      org-air-review-buffer-name)))
                       (_ (get-buffer org-air-view-buffer-name)))
                     buffer)))
    (condition-case err
        (org-air-help--render buffer context origin)
      (error
       (message "org-air: bookmark restore degraded: %s"
                (org-air-view--short-error err))))
    (set-buffer buffer)))
;;;###autoload
(put 'org-air-help-bookmark-jump 'bookmark-handler-type "org-air")

(provide 'org-air-view)

;; Local Variables:
;; package-lint-main-file: "org-air.el"
;; End:
;;; org-air-view.el ends here
