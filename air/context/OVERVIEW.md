# Project Overview — org-air

## Description
**org-air** is a modern Emacs package: an aesthetically refined replacement
for org-agenda that doubles as (a) a **GTD dashboard** over org-mode files
and (b) an **Air-docs project viewer**, with two further surfaces —
**Revisit** (evergreen notes) and **Review** (retrospective). It renders
org headings into classified sections with an svg-polished,
rougier/nano-inspired look — without using any org-agenda machinery.

## Core Principles
- **org-mode files are the source of truth** — org-air only reads/queries.
- **org-ql for querying** (NOT org-agenda), over buffers org-air manages.
- **Aesthetics matter** — svg-pill chips, prefix-svg headers, a live
  inspector, calm calendar, pixel-locked column alignment.
- **GUI svg is a cosmetic overlay only** — every svg element has a TTY/byte
  fallback; the *text* is always the contract, so the whole UI is
  byte-testable.
- **Data-pure render** — everything classify and render need lives in the
  scanned `org-air-item` struct and the per-file meta table. A repaint
  opens NO file, ever.
- **Planning-first** (Air): each round is specced in
  `air/v0.1/org-air-roundN.org` (+ a `-design.org` companion) before impl.

## Technology Stack
- **Language**: Emacs Lisp (Emacs **29.1+** — needs `string-pixel-width`,
  svg, modern faces).
- **Dependency**: **org-ql** (queries), org-mode, optional Denote (origin
  filenames / journal), svg.el. Soft: evil, dimmer — neither required.
- **Testing**: ERT byte/fixture tests; `make check` = byte-compile + lint
  (checkdoc / package-lint / the duplicate-definition rule) + tests.
  Fixtures are golden text renders (frozen clock); svg is GUI-only so
  tests assert the text layer.
- **Build**: `make deps` (installs org-ql etc. into repo-local `.deps/`),
  `make check`, `make check-gui` / `make bench-gui` (need a display),
  `make regen-mockups` (regenerate fixtures via frozen-clock render with
  anti-tautology guards).

## Source Structure
- `org-air.el` — entry points (`M-x org-air`), `org-air-files`,
  `org-air-exclude-regexps`, `org-air-inbox-file`.
- `org-air-query.el` — the never-error work-buffer scan → `org-air-item`
  structs, the per-file meta table, the note-type model, the review
  harvest, the surface precondition (`org-air-require-surface`).
- `org-air-classify.el` — bucket logic: Overdue / Upcoming / High priority
  / Needs attention (the measured recency clock) / Untracked / Inbox /
  Backlog, plus the note buckets.
- `org-air-inbox.el` — capture, the refile engine, the `e` edit transient.
- `org-air-view.el` — the renderer: the shared `org-air-view--insert-row`
  primitive, the svg pill (`--svg-pillify`) and line-clamped svg
  (`--svg-line-image`), the priority square, the generalised **inspector
  core**, sections, the filter language, the refresh machine, the disk
  cache, bookmarks, the scroll/landing seam, point-tracking.
- `org-air-layout.el` — geometry: two-pane composition, usable-width
  derivation, the glyph tier table, rail headers.
- `org-air-calendar.el` — the month calendar grid (due/sched/created/period
  marks).
- `org-air-project.el` — the Air-docs project view built on the SHARED
  view core (no drift).
- `org-air-revisit.el` / `org-air-review.el` — the notes and retrospective
  surfaces, also on the shared core.
- `org-air-faces.el` — all faces + the svg colour/knob defcustoms.
- `org-air-bench.el` — the frame-real expand fence (`make bench-gui`).
- `tests/` — ERT suites + `fixtures/` golden renders + the self-policing
  `org-air-known-failures.el` manifest and `org-air-lint-baseline.el`.

## Core Components / Pipeline
`query` → `classify` → `view` (compose text + svg overlay) → `layout`
(geometry) → `faces`. The board, the divider and the rail are one buffer's
text when the rail is inline; `side-window` (the default) puts the rail in
a dedicated window rendered from the same descriptor. The **inspector
core** is generalised so every view hosts the same mid-rail inspector.

## Build / Test workflow
See `air/context/ace-orchestration.md` for the jj/gate/worker mechanics
(the project is built by sandboxed worker agents under an ACE supervisor).
Binary gate: `make check` green = "Ran N tests, N as expected, 0
unexpected" + lint(0) + the known-failures manifest EMPTY.  `make check`
is BATCH-ONLY and says nothing about rendering speed — see
`architecture.md` on `make check-gui` / `make bench-gui`.

## Document States (Air Workflow)
- `draft` → `ready` → `work-in-progress` → `complete`.
- Shipped rounds are marked `complete`; the active round stays `ready`/WIP.
- `airctl status` for live state; `airctl update --state ...` to transition.
