# Architecture — org-air

## Pipeline
```
org files ──org-ql──▶ org-air-query ──▶ org-air-item structs
                                   │
                          org-air-classify  (buckets, staleness, date semantics)
                                   │
                          org-air-view      (compose TEXT + svg overlay; sections;
                                   │         shared insert-row; inspector core;
                                   │         point-tracking)
                          org-air-layout    (two-pane geometry: board │ rail;
                                   │         calendar/summary/inspector/filters/
                                   │         actions; prefix-svg headers; divider;
                                   │         responsive board-only)
                                   ▼
                          single buffer (byte-testable text + GUI svg overlay)
```
`org-air-faces` supplies faces + the svg colour/knob defcustoms used
throughout. `org-air-calendar` renders the month grid. `org-air-project`
reuses the view core for the Air-docs viewer. `org-air-inbox` handles
capture.

## Key invariants (do not regress)
1. **V6 pixel-lock**: meta columns computed from `string-width`; svg pills
   occupy exactly their text cell box → svg on/off shifts zero columns.
   Titles share one left edge (TODO-keyword column reserved even when empty).
2. **svg never grows the line**: `org-air-view--svg-line-image` builds every
   org-air svg at the exact font line height with an INTEGER baseline
   `:ascent` (not `:ascent 'center`). A taller image makes the row grow →
   the `│` divider glyph can't fill it → gaps. (Round-13 fix.)
3. **Inspector inert when `noninteractive`**: the live-update
   post-command-hook + idle timer + any sit-for/input-wait MUST be guarded
   `(unless noninteractive ...)`; the compose path is pure/synchronous so
   batch + `make regen-mockups` never deadlock. (Round-13 hardening — a 2h
   hang taught this.)
4. **One renderer, two views**: project view is a parameterised variant of
   the shared core (insert-row, inspector core, sections, pills, faces) —
   NOT a parallel implementation. Generalise the shared primitive rather
   than fork, so board and project can't drift.
5. **Text is the contract**: every svg/GUI element has a TTY/byte fallback;
   fixtures assert the text layer only.

## The inspector core (generalised, round-14)
Buffer-local: `org-air-view--inspector-active`, `-property`,
`-fields-function`, `-initial-fn`, `-beg`/`-end` markers. The board sets the
default (item fields); the project view supplies doc-fields. Update =
delete-region between markers + reinsert the recomposed FULL-WIDTH inspector
lines under `inhibit-read-only`+`save-excursion`; redraw only when the
inspected item changes.

## Testing model
- ERT suites in `tests/`; golden text fixtures in `tests/fixtures/`
  (frozen clock = `org-air-test-now`, inside `org-air-test-with-fixtures`,
  helpers in `tests/org-air-test-helpers.el`).
- Anti-tautology guards on regen (the render must produce the bytes, not the
  test). `make regen-mockups` regenerates; design blesses; orchestrator
  integrates.
- **Self-policing manifest** `tests/org-air-known-failures.el`: a listed
  test that PASSES is itself a failure (forces closeout); an unlisted
  failing test fails the gate. So `make check` exit code is the single gate
  and the manifest must be EMPTY at integration.

## Build / deps
`make deps` → repo-local `.deps/` (org-ql etc.). `make check` =
byte-compile (warnings non-fatal but lint catches) + lint (checkdoc/custom,
baselined) + ERT. `make clean` before EVERY verify (stale `.elc` shadow).
