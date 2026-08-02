# Interface Design — org-air

The visual design system as shipped. Inspiration: rougier's nano-emacs /
svg-tag-mode / svg-lib; calm, typographic, modern.

## Buffer-naming convention (stable, public)
**Invariant: every buffer org-air creates and shows in a window is named
with the `*org-air` prefix** — no hidden-buffer leading space — so
users (and packages like dimmer, popper, shackle, `display-buffer-alist`)
can match them all with one regexp — `\\*org-air`:

| Buffer            | What                                              | Window                         |
|-------------------|---------------------------------------------------|--------------------------------|
| `*org-air*`       | the board (`M-x org-air`)                          | main window                    |
| `*org-air-project*` | the project / Air-docs view (`M-x org-air-project`) | main window                  |
| `*org-air revisit*` | the Revisit view (`M-x org-air-revisit`)        | main window                    |
| `*org-air review*`  | the Review view (`M-x org-air-review`)          | main window                    |
| `*org-air-rail*`  | the popped-out context rail                        | side window (on demand)        |
| `*org-air-view*`  | the read-only source/entry view pane               | bottom side window (on demand) |
| `*org-air-pane:TITLE*` | the EDITABLE indirect pane                    | bottom pane window (on demand) |

This naming is a **public contract**: it will not change without a major
note. The doc-session and return-mode host buffers are the **user's** file
buffers — never renamed, never excluded (dimming those is the user's own
policy).

**Shipped dimmer integration (zero config):** when dimmer.el is
loaded, org-air registers `org-air-dimmer-buffer-p` on
`dimmer-buffer-exclusion-predicates` (a soft dep — dimmer is never
required; without dimmer the registration is dormant and creates no dimmer
variable), so no org-air-owned buffer is ever dimmed. The manual regexp
stays a valid alternative:
```elisp
(with-eval-after-load 'dimmer
  (add-to-list 'dimmer-buffer-exclusion-regexps "\\*org-air"))
```

## Four views, one renderer
- **Board** (`M-x org-air`): the GTD dashboard.
- **Project** (`M-x org-air-project`): the Air-docs viewer.
- **Revisit** (`N`): evergreen knowledge notes, dustiest first.
- **Review** (`W`): the period retrospective.
All four render through the SAME core (`org-air-view--insert-row`, the
inspector core, the rail descriptor, the sort/filter cores, the bookmark
quartet, the svg layer, faces) so they cannot drift. The per-view code is
thin: a row mapping, a rail descriptor and a section table.

## Layout — content pane plus rail
`[ board/item pane ]  │  [ rail ]`
- `org-air-rail-placement` decides where the rail lives: `side-window`
  (the default — a dedicated `*org-air-rail*` window) or `inline` (one
  buffer holds both panes and the divider, which is what keeps the layout
  byte-testable). Per-view overrides win when non-nil.
- **Rail order (top→bottom):** Calendar, Filter, Summary, **Inspector**,
  Actions. Filter sits above Summary so the active narrowing is read
  BEFORE the counts it explains; the inspector takes a fixed reserved
  middle and Actions is pinned to the foot.
- **Responsive:** below `org-air-rail-min-width` (default 90) the rail is
  dropped and the board fills the full width (board-only) — so opening a
  file in a narrow split never crowds. Re-renders on resize.

## The alignment contract (load-bearing — never break)
Every row: title LEFT (flex, truncate-last), then a fixed-width right
cluster of meta cells (date · tags · origin). Columns are computed from
`string-width` (text layer) and stay pixel-locked. **svg pills must occupy
EXACTLY their text cell's pixel box** (`Ncols × char-width`) so turning
svg on/off shifts zero columns. Titles share one left edge whether or not
the item has a TODO keyword (the keyword column is reserved).

## svg pills (date / tags / priority)
- A pill is an svg overlay sized to its reserved text cell; the label is
  centred with reserved **pad columns** so it never clips (svg-tag style).
- **Critical:** the pill image is clamped to the EXACT font line height with
  an integer baseline `:ascent` (`org-air-view--svg-line-image`) so it never
  grows the row — otherwise the row grows taller than the `│` glyph and the
  divider gaps.
- Calm look: soft radius (~`ch/6`), monochrome capsule (near-zero fill,
  hairline border), colour lives in the LABEL only (tag accent / date
  semantic hue). Date pills are padded to a UNIFORM width (the date column).
- **Priority** = a tiny solid colour SQUARE (no letter/outline) in a fixed
  2-col slot, warm to cool: A=red, B=orange, C=yellow-green, D=teal,
  E=indigo (`org-air-priority-colors`).
- Knobs: `org-air-pill-pad-cols`, `-radius`, `-fill-alpha`,
  `-border(-opacity)`, `-vinset`, `org-air-date-pill-align`,
  `org-air-priority-style`, `org-air-tag-style`/`-date-style` (pill|text).

## Rail headers — prefix-svg markers
Section headers (`▌ Summary`, `▌ June 2026`, `▌ Inspector`, …) use a slim
prefix-svg marker + clean label (`org-air-rail-header-style` marker|rule).
TTY fallback = a plain prefix char.

## The divider
Inline: a text `│` at `line-spacing 0` with line-clamped pills, pinned to
one pixel-X per row with `display (space :align-to)`. Under
`side-window` the divider is a real window border themed through
`org-air-face-window-divider`. Known fragility of the inline form:
box-drawing glyph coverage varies by font, and a non-zero
`org-air-line-spacing` opens a gap the per-cell glyph cannot paint into.

## The inspector
A mid-rail block showing metadata for the highlighted line, live-updating as
point moves (buffer-local `post-command-hook` → **debounced**, redraw only
when the inspected thing changes, marker-region delete+reinsert — NEVER a
full re-render on motion). **Must be inert when `noninteractive`**, else
batch and regen deadlock. Fields: title, state+priority, tags, then
origin/file, Scheduled, Deadline (+◆ overdue), Created, Updated (the
measured stamp, or the `~file` lower bound), Repeat
(`every 1w → next Mon 22 Jun`) and the derived Bucket; nil lines omitted.
The generalised core
(`inspector-active/-property/-fields-function/-initial-fn`) lets every
non-board view host its own fields with the same machinery.

## Calendar
Month grid with due (◆) / scheduled (●) / created (·) marks, today
highlighted, muted palette, centred in the rail, full-width header.

## Faces & theming
All colour in `org-air-faces.el`; light/dark aware; svg colours derive from
faces and re-render on theme change. Inspector field labels are mid-tier
readable (WCAG-AA), values keep their semantic faces.

## Repeats (read-only)
Org repeaters (`.+1d`/`++1w`) on scheduled/deadline are detected (via Org's
own `org-get-repeat` — not reimplemented) and shown as a `↻` marker in the
date cluster + an inspector `Repeat` line. The custom working-day-aware
advance (`:AIR_REPEAT: workday`, skip weekends) is DEFERRED — lean on
Org's defaults, fill the gap only when needed.
