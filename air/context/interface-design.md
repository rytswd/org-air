# Interface Design — org-air

The visual design system as shipped through round-14. Inspiration:
rougier's nano-emacs / svg-tag-mode / svg-lib; calm, typographic, modern.

## Buffer-naming convention (stable; round-16 D-P2)
Every org-air buffer shares the `*org-air` prefix so users (and packages
like dimmer, popper, shackle, `display-buffer-alist`) can match them all
with one regexp — `\\*org-air`:

| Buffer            | What                                              | Window                         |
|-------------------|---------------------------------------------------|--------------------------------|
| `*org-air*`       | the board (`M-x org-air`)                          | main window                    |
| `*org-air-project*` | the project / Air-docs view (`M-x org-air-project`) | main window                  |
| `*org-air-rail*`  | the popped-out context rail (round-16 D-P1)        | right side window (on demand)  |
| `*org-air-view*`  | the bottom source/entry view pane (round-16 D-P3)  | bottom side window (on demand) |

This naming is a **public contract**: it will not change without a major
note. org-air's job is *only* the stable names — dimming/excluding the side
windows is the **user's** config (e.g. exclude org-air from dimmer):
```elisp
(with-eval-after-load 'dimmer
  (add-to-list 'dimmer-buffer-exclusion-regexps "\\*org-air"))
```
org-air ships no dimmer integration of its own (not its territory).

## Two views, one renderer
- **Board** (`M-x org-air`): the GTD dashboard.
- **Project** (`M-x org-air-project`): the Air-docs viewer.
Both render through the SAME core (`org-air-view--insert-row`, the inspector
core, the svg layer, faces) so they cannot drift. Project-specific code is
thin (doc→item mapping, bucket grouping, the two-line arrangement).

## Layout — two panes in one buffer
`[ board/item pane ]  │  [ rail ]`
- One buffer holds both panes + the divider inline (keeps it byte-testable).
- **Rail order (top→bottom):** Calendar, Summary, **Inspector**, Filters,
  Actions (Filters+Actions pinned to the foot; the inspector takes the
  expanded middle).
- **Responsive:** below `org-air-rail-min-width` (default 90) the rail is
  dropped and the board fills the full width (board-only) — so opening a
  file in a narrow split never crowds. Re-renders on resize (round-9 C1).

## The V6 alignment contract (load-bearing — never break)
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
  divider gaps. (This was the round-13 fix; do not regress.)
- Calm look: soft radius (~`ch/6`), monochrome capsule (near-zero fill,
  hairline border), colour lives in the LABEL only (tag accent / date
  semantic hue). Date pills are padded to a UNIFORM width (the date column).
- **Priority** = a tiny solid colour SQUARE (no letter/outline) in a fixed
  ~2-col slot: A=red, B=orange, C=yellow-green (`org-air-priority-colors`).
- Knobs: `org-air-pill-pad-cols`, `-radius`, `-fill-alpha`,
  `-border(-opacity)`, `-vinset`, `org-air-date-pill-align`,
  `org-air-priority-style`, `org-air-tag-style`/`-date-style` (pill|text).

## Rail headers — prefix-svg markers (round-11/D-P6)
Section headers (`▌ Summary`, `▌ June 2026`, `▌ Inspector`, …) use a slim
prefix-svg marker + clean label (`org-air-rail-header-style` marker|rule).
The old hl-block card / `────` rule chrome is retired. TTY fallback = a
plain prefix char.

## The divider (still being refined — see round-15)
A single-buffer text `│` at `line-spacing 0` with line-clamped pills. Known
fragility: box-drawing glyph coverage varies by font. Round-15 proposes the
architecture decision (real side-window + `window-divider` vs a robust
in-buffer rule) for a truly gap-free, sophisticated divider.

## The inspector (round-11..14)
A mid-rail block showing metadata for the highlighted line, live-updating as
point moves (buffer-local `post-command-hook` → **debounced**, redraw only
when the inspected item changes, marker-region delete+reinsert — NEVER a
full re-render on motion). **Must be inert when `noninteractive`** (else
batch/regen deadlocks — round-13 hardening). Fields: title, state+priority,
origin/file, tags, Scheduled, Deadline (+◆ overdue), Created, Closed,
Repeat (`every 1w → next Mon 22 Jun`), Bucket/stale-days; nil lines omitted.
Generalised core (`inspector-active/-property/-fields-function/-initial-fn`)
so the project view hosts a doc-field inspector with the same machinery.

## Calendar
Month grid with due (◆) / scheduled (●) / created (·) marks, today
highlighted, muted palette, centred in the rail, full-width header.

## Faces & theming
All colour in `org-air-faces.el`; light/dark aware; svg colours derive from
faces and re-render on theme change. Inspector field labels are mid-tier
readable (WCAG-AA), values keep their semantic faces.

## Repeats (round-14, read-only)
Org repeaters (`.+1d`/`++1w`) on scheduled/deadline are detected (via Org's
own `org-get-repeat` — not reimplemented) and shown as a `↻` marker in the
date cluster + an inspector `Repeat` line. The custom working-day-aware
advance (`:AIR_REPEAT: workday`, skip weekends) is DEFERRED — lean on Org's
defaults, fill the gap only when needed.
