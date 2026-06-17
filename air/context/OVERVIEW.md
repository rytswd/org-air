# Project Overview — org-air

## Description
**org-air** is a modern Emacs package: an aesthetically refined replacement
for org-agenda that doubles as (a) a **GTD dashboard** over org-mode files
and (b) an **Air-docs project viewer**. It renders dated org TODOs into
clean, classified sections with an svg-polished, rougier/nano-inspired look
— without using any org-agenda machinery.

## Core Principles
- **org-mode files are the source of truth** — org-air only reads/queries.
- **org-ql for querying** (NOT org-agenda).
- **Aesthetics matter** — svg-pill chips, prefix-svg headers, a live
  inspector, calm calendar, pixel-locked column alignment (the "V6"
  contract).
- **GUI svg is a cosmetic overlay only** — every svg element has a TTY/byte
  fallback; the *text* is always the contract, so the whole UI is
  byte-testable.
- **Planning-first** (Air): each screenshot-driven round is specced in
  `air/v0.4/org-air-roundN.org` (+ a `-design.org` companion) before impl.

## Technology Stack
- **Language**: Emacs Lisp (Emacs **29.1+** — needs `string-pixel-width`,
  svg, modern faces).
- **Dependency**: **org-ql** (queries), org-mode, optional Denote (origin
  filenames / journal), svg.el. Soft: none required for the byte layer.
- **Testing**: ERT byte/fixture tests; `make check` = byte-compile + lint
  (checkdoc/custom) + tests. Fixtures are golden text renders (frozen
  clock); svg is GUI-only so tests assert the text layer.
- **Build**: `make deps` (installs org-ql etc. into repo-local `.deps/`),
  `make check`, `make regen-mockups` (regenerate fixtures via frozen-clock
  render with anti-tautology guards).

## Source Structure
- `org-air.el` — entry points (`M-x org-air`, `M-x org-air-project`), mode.
- `org-air-query.el` — org-ql queries → `org-air-item` structs.
- `org-air-classify.el` — bucket logic (Inbox / Needs-attention / Upcoming /
  High-priority / Stale; staleness; date semantics).
- `org-air-inbox.el` — capture / inbox handling.
- `org-air-view.el` — the renderer: the shared `org-air-view--insert-row`
  primitive, the svg pill (`--svg-pillify`) + line-clamped svg
  (`--svg-line-image`), the priority square, the generalised **inspector
  core**, sections, point-tracking.
- `org-air-layout.el` — geometry: two-pane (board | rail) composition,
  the rail (calendar/summary/inspector/filters/actions), prefix-svg
  headers, the divider, responsive board-only mode.
- `org-air-calendar.el` — the month calendar grid (due/sched/created marks).
- `org-air-project.el` — the Air-docs project view (state buckets, two-line
  doc blocks, project inspector) built on the SHARED view core (no drift).
- `org-air-faces.el` — all faces + the svg colour/knob defcustoms.
- `tests/` — ERT suites + `fixtures/` golden renders + the self-policing
  `org-air-known-failures.el` manifest.

## Core Components / Pipeline
`query` → `classify` → `view`(compose text + svg overlay) → `layout`(two-
pane geometry) → `faces`. A single buffer holds the board, the divider, and
the rail inline (byte-testable). The **inspector core** is generalised so
both the board and the project view host the same mid-rail inspector.

## Build / Test workflow
See `air/context/ace-orchestration.md` for the jj/gate/worker mechanics
(the project is built by sandboxed worker agents under an ACE supervisor).
Binary gate: `make check` green = "Ran N tests, N as expected, 0
unexpected" + lint(0) + the known-failures manifest EMPTY.

## Document States (Air Workflow)
- `draft` → `ready` → `work-in-progress` → `complete`.
- Shipped rounds are marked `complete`; the active round stays `ready`/WIP.
- `airctl status` for live state; `airctl update --state ...` to transition.
