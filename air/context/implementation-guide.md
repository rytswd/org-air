# Implementation Guide — org-air

## Files & where things live
| file | responsibility |
|------|----------------|
| `org-air.el` | entry points, mode, keymap (`M-x org-air`, `org-air-project`) |
| `org-air-query.el` | org-ql queries → `org-air-item` structs (title, todo, priority, tags, scheduled/deadline/created/closed, group, file) |
| `org-air-classify.el` | buckets (Inbox/Needs-attention/Upcoming/High-priority/Stale), staleness, date relative terms |
| `org-air-view.el` | renderer: `--insert-row`, `--svg-pillify`, `--svg-line-image`, priority square, inspector core, sections, point-tracking |
| `org-air-layout.el` | two-pane geometry, rail blocks, prefix-svg headers, divider, responsive board-only |
| `org-air-calendar.el` | month grid + marks |
| `org-air-project.el` | Air-docs view (thin; reuses view core) |
| `org-air-faces.el` | faces + svg colour/knob defcustoms |
| `tests/` | ERT suites, `fixtures/`, `org-air-known-failures.el` |

## Conventions
- Follow `air/context/air-conventions.md` for Air docs.
- Emacs Lisp: lexical-binding; `org-air-` / `org-air-view--` (internal `--`)
  prefixes; checkdoc-clean docstrings (escape col-0 parens, quote symbols);
  custom defcustoms grouped, themable.
- **Every new GUI/svg feature needs a TTY/byte fallback** and (if it changes
  rendered bytes) a fixture regen recorded in the known-failures manifest.
- Keep the V6 pixel-lock + the svg-line-height clamp + inspector
  noninteractive-guard invariants (see `architecture.md`).

## Adding a feature (the round loop)
1. Spec it in `air/v0.4/org-air-roundN.org` (state `ready`); design writes a
   `-design.org` companion with exact contracts + knobs.
2. impl implements per the design doc; commits PER-ITEM (`RNN` prefix);
   records byte-changing fixtures in the manifest (does NOT edit fixtures).
3. test regenerates + re-blesses fixtures via the frozen-clock renderer,
   empties the manifest, gates green; holds RED + reports any real impl bug.
4. Orchestrator verifies `make check` itself (timeout-guarded), integrates
   to trunk, marks the round `complete`.

## The gate (binary)
`make clean && make check` → green = "Ran N tests, N as expected, 0
unexpected" + lint(0) + manifest EMPTY. Anything else = not shippable.
`make check` must COMPLETE quickly (~16s); a hang means the inspector
noninteractive-guard is incomplete.

## jj / workflow / orchestration
See `air/context/ace-orchestration.md` — the mandatory jj incantation
(`GIT_CONFIG_GLOBAL=/dev/null jj --config signing.behavior=drop ...`), the
sandboxed worker-seat spawn recipe, the monitor timer/hooks, and the
hard-won process lessons (one authoritative decision message; workers never
integrate; byte-verify what shipped; park `@` before abandoning strays).

## Known follow-ups (deferred, not lost)
- Round-15: no-TODO title alignment; the divider architecture decision
  (side-window + `window-divider` vs robust in-buffer rule).
- Repeats B: custom `:AIR_REPEAT: workday` weekend/holiday-skipping advance
  (deferred — lean on Org defaults first).
- Journals (Denote `_journal`) affordance — later.
