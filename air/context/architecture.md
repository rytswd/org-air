# Architecture — org-air

## Pipeline
```
org files ──org-ql over org-air's OWN work buffer──▶ org-air-query
                                   │        (org-air-item structs + the
                                   │         per-file meta table)
                          org-air-classify  (buckets; the measured recency
                                   │         clock; date semantics)
                          org-air-view      (compose TEXT + svg overlay;
                                   │         sections; shared insert-row;
                                   │         inspector core; filter core;
                                   │         refresh machine; disk cache;
                                   │         scroll/landing seam)
                          org-air-layout    (usable width, two-pane
                                   │         composition, glyph tiers)
                                   ▼
                          buffer text (byte-testable) + GUI svg overlay
```
`org-air-faces` supplies faces + the svg colour/knob defcustoms used
throughout. `org-air-calendar` renders the month grid. `org-air-project`,
`org-air-revisit` and `org-air-review` reuse the view core.
`org-air-inbox` handles capture and the refile engine.

## Key invariants (do not regress)
1. **Pixel-lock**: metadata columns are computed from `string-width`; an
   svg pill occupies exactly its text cell box → turning svg on or off
   shifts zero columns. Titles share one left edge (the TODO-keyword
   column is reserved even when empty).
2. **svg never grows the line**: `org-air-view--svg-line-image` builds
   every org-air svg at the exact font line height with an INTEGER
   baseline `:ascent` (not `:ascent 'center`). A taller image makes the row
   grow → the `│` divider glyph cannot fill it → gaps.
3. **No `mouse-face` on an image position**: a buffer position may carry
   `mouse-face` OR an image `display`, never both. Emacs 30's
   `DRAW_MOUSE_FACE` re-looks-up the SVG, so a hover crossing over a pill
   re-rasterises it synchronously. Row hover is a text-only band.
4. **Inspector and every live hook inert when `noninteractive`**: the
   post-command hooks, idle timers and any `sit-for`/input wait MUST be
   guarded `(unless noninteractive …)`; the compose path is pure and
   synchronous so batch and `make regen-mockups` never deadlock.
5. **One renderer, four views**: project / revisit / review are
   parameterised variants of the shared core (insert-row, inspector core,
   rail descriptor, sort core, filter core, bookmark quartet) — NOT
   parallel implementations. Generalise the shared primitive rather than
   fork.
6. **Text is the contract**: every svg/GUI element has a TTY/byte
   fallback; fixtures assert the text layer only.
7. **Data-pure render**: a repaint opens NO file. Everything classify and
   render read is a scan-time struct slot or a hash lookup in
   `org-air-query--file-meta`. The one exception is the inspector's
   on-demand hydration of a single inspected item.
8. **Filter ⇔ bucket agreement by construction**: a date/status filter
   token evaluates the SAME hoisted classify predicate its section does.
   Never re-derive date arithmetic in the filter.
9. **Marks are validated, never guessed**: a mark records an exact source
   heading; if that heading moved, changed title/effective tags, or
   vanished, the mark is PRUNED, never re-pointed.
10. **The repaint keeps the row on its screen line**: every repaint runs
    inside `org-air-view--with-scroll-stable`, which takes per-window
    anchors BEFORE the erase and re-applies them AFTER the landing. A
    deliberate NEW landing (an entry, a bookmark jump) stands the seam
    down for the acting window only.
11. **Every command states its surface first**: `org-air-require-surface`
    signals a `user-error` before any prompt, read or mutation, so an
    out-of-context `M-x` cannot touch a foreign buffer.

## The cache
`org-air-view--cache-version` is the serialisation generation of
`org-air-cache-file` (currently **8**); a mismatch is discarded, never
migrated. Bump it whenever the persisted record SHAPE changes — and also
when the MEANING of an existing slot narrows, because a stale value that
looks right is worse than a cold miss.

`org-air-view--cache-key` is the separate detector for CONFIGURATION
change (files, inbox, scan vocabulary, container knob, exclude regexps,
log cap, task-requires-todo). A key mismatch takes the cold re-derive.
Adding a key element needs no version bump while the shape is unchanged:
a shorter key from an older org-air already misses on length.

## The inspector core
Buffer-local: `org-air-view--inspector-active`, `-property`,
`-fields-function`, `-initial-fn`, `-beg`/`-end` markers. The board sets
the default (item fields); the other views supply their own. Update =
delete-region between markers + reinsert the recomposed FULL-WIDTH
inspector lines under `inhibit-read-only` + `save-excursion`; redraw only
when the inspected thing changes.

## Testing model
- ERT suites in `tests/`; golden text fixtures in `tests/fixtures/`
  (frozen clock = `org-air-test-now`, inside `org-air-test-with-fixtures`,
  helpers in `tests/org-air-test-helpers.el`).
- Anti-tautology guards on regen (the render must produce the bytes, not
  the test). `make regen-mockups` regenerates; design blesses;
  orchestrator integrates.
- **Self-policing manifests.** `tests/org-air-known-failures.el`: a listed
  test that PASSES is itself a failure (forces closeout); an unlisted
  failing test fails the gate. `tests/org-air-lint-baseline.el` works the
  same way for lint findings. So `make check` exit code is the single
  gate, and both manifests can only shrink honestly.
- **The lint gate carries a rule the byte-compiler cannot**: a package
  symbol may be DEFINED AT MOST ONCE per namespace. `cl-defstruct`
  installs a compiler macro for every accessor, so a static accessor call
  keeps working even when a `defun` elsewhere has taken over that symbol's
  function cell — while `funcall`, `apply`, a sharp-quoted `mapcar`, a
  hook or a late `eval` silently get the OTHER definition. This is why the
  `d` command is `org-air-item-set-deadline` and no alias is provided.
- Runtime struct integrity is asserted too: the accessors must read the
  slots, not a shadowing function.

## What batch CANNOT see
`make check` is batch-only, and `emacs --batch` is a different machine
from an interactive frame:
- it keeps `gc-cons-percentage` at its startup **1.0** ("collect when the
  heap doubles"), so the collector effectively never runs during a test —
  and in a loaded session ONE collection costs ~55 ms;
- `string-width` allocates nothing in batch and ~4 conses per column on a
  frame for any non-ASCII string — and every org-air row carries the
  approved glyph vocabulary.

So a green batch run says NOTHING about rendering speed. Two targets
close that:
- `make check-gui` — the same ERTs under a real display, so the
  `(skip-unless (display-graphic-p))` tests actually run. It arms a
  `run-with-timer` one-shot and returns: idle timers cannot fire while a
  file is still being `-l`-loaded, so running ERT during the load is a
  false "the board never loads".
- `make bench-gui` — the expand fence: one realistic section, the SHIPPED
  default styles, a real frame, `redisplay` inside the measured window,
  and a wall-clock ceiling. It exits **2** (not 0) with no display,
  because a fence that silently "passes" when it could not run is worse
  than no fence.

Neither is part of `make check`, because a gate that cannot run everywhere
is a gate people learn to ignore; `make check` prints a loud notice that
the fence did not run.

## Build / deps
`make deps` → repo-local `.deps/` (org-ql etc.). `make check` =
byte-compile (warnings non-fatal, but checkdoc catches docstring style
including the ≤80-column rule) + lint (baselined) + ERT. `make clean`
before EVERY verify (a stale `.elc` shadows results).
