# Implementation Guide — org-air

## Files & where things live
| file | responsibility |
|------|----------------|
| `org-air.el` | entry point, `org-air-files`, `org-air-exclude-regexps`, `org-air-inbox-file` |
| `org-air-query.el` | the never-error work-buffer scan → `org-air-item` structs, the per-file meta table, note types, the review harvest, `org-air-require-surface` |
| `org-air-classify.el` | buckets (Overdue / Upcoming / High priority / Needs attention / Untracked / Inbox / Backlog + the note buckets), the measured recency clock, date semantics |
| `org-air-inbox.el` | capture, the refile engine, the `e` edit transient |
| `org-air-view.el` | renderer: `--insert-row`, `--svg-pillify`, `--svg-line-image`, priority square, inspector core, sections, filter language, refresh machine, disk cache, bookmarks, scroll/landing seam |
| `org-air-layout.el` | usable width, two-pane composition, glyph tiers, rail headers |
| `org-air-calendar.el` | month grid + marks |
| `org-air-project.el` | Air-docs view (reuses the view core) |
| `org-air-revisit.el` | evergreen-notes view (reuses the view core) |
| `org-air-review.el` | period retrospective (reuses the view core) |
| `org-air-faces.el` | faces + svg colour/knob defcustoms |
| `org-air-bench.el` | the frame-real expand fence (`make bench-gui`) |
| `tests/` | ERT suites, `fixtures/`, `org-air-known-failures.el`, `org-air-lint-baseline.el` |

## Conventions
- Follow `air/context/air-conventions.md` for Air docs.
- Emacs Lisp: lexical-binding; `org-air-` public / `org-air-view--`
  internal prefixes; checkdoc-clean docstrings (imperative first line,
  arguments in CAPS, ≤80 columns, escape column-0 parens, quote symbols);
  defcustoms grouped and themable.
- **A package symbol may be defined at most ONCE per namespace.** The lint
  gate enforces it, because the byte-compiler cannot see this class: a
  `cl-defstruct` accessor call is inlined by its compiler macro and keeps
  working even when a `defun` has stolen the function cell, while every
  indirect call gets the other definition.
- **Every new GUI/svg feature needs a TTY/byte fallback** and (if it
  changes rendered bytes) a fixture regen recorded in the known-failures
  manifest.
- Keep the invariants in `architecture.md`: pixel-lock, the svg
  line-height clamp, no `mouse-face` on an image position, the
  `noninteractive` guard on every live hook, data-pure render, filter ⇔
  bucket agreement, validated marks, the scroll seam, and the surface
  precondition on every command.

## Adding a feature (the round loop)
1. Spec it in `air/v0.1/org-air-roundN.org` (state `ready`); design writes
   a `-design.org` companion with exact contracts + knobs.
2. impl implements per the design doc; commits per item; records
   byte-changing fixtures in the manifest (does NOT edit fixtures).
3. test regenerates + re-blesses fixtures via the frozen-clock renderer,
   empties the manifest, gates green; holds RED + reports any real impl
   bug.
4. Orchestrator verifies `make check` itself (timeout-guarded), integrates
   to trunk, marks the round `complete`.

## The gate (binary)
`make clean && make check` → green = "Ran N tests, N as expected, 0
unexpected" + lint(0) + manifest EMPTY. Anything else = not shippable.
The suite runs in about a minute; a HANG means a live hook lost its
`noninteractive` guard.

`make check` is BATCH-ONLY and proves nothing about rendering speed —
`emacs --batch` keeps `gc-cons-percentage` at 1.0 so the collector never
runs, and `string-width` is free there and costly on a frame. On a
display run `make check-gui` (the ERTs, including the ones batch skips)
and `make bench-gui` (the expand fence, which exits 2 rather than 0 when
it cannot run).

## jj / workflow / orchestration
See `air/context/ace-orchestration.md` — the mandatory jj incantation
(`GIT_CONFIG_GLOBAL=/dev/null jj --config signing.behavior=drop ...`), the
sandboxed worker-seat spawn recipe, the monitor timer/hooks, and the
hard-won process lessons (one authoritative decision message; workers
never integrate; byte-verify what shipped; park `@` before abandoning
strays).
