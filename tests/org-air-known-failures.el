;;; org-air-known-failures.el --- expected-failure manifest for org-air -*- lexical-binding: t; -*-

;;; Commentary:
;; Single source of truth for tests that are EXPECTED to fail right now
;; (grind signals for the impl track).  `make test' loads this file after
;; all test suites and calls `org-air-test-apply-known-failures', which
;; marks every listed test with an expected result of :failed.
;;
;; Effect on orchestration: the suite is binary.
;;   - a listed test failing      -> expected   (exit 0)
;;   - a listed test PASSING      -> unexpected (exit 1) — remove it here!
;;   - an unlisted test failing   -> unexpected (exit 1)
;; So `make test' exit code is the single integration gate, and a stale
;; manifest entry is itself a failure, keeping this list honest.
;;
;; Workflow: when writing a spec-true test the impl does not satisfy yet,
;; add its symbol here with a short reason.  When impl fixes it, the run
;; goes red until the entry is deleted — that deletion is the closeout.

;;; Code:

(require 'ert)

(defvar org-air-test-known-failures
  '(
    ;; (test-symbol . "reason / spec reference")  — none right now.
    ;; 2026-06-12: 5 date-label sign-inversion entries closed out — fix
    ;; landed on trunk3 (pwuqtvlt) and all 5 regression tests pass.
    ;; 2026-06-12: the entire 17-entry v0.2 grind manifest closed out on
    ;; the integrated main line (wzmskzwn) — 16 entries verified
    ;;
    ;; ===================================================================
    ;; v0.4 ROUND-10 grind (impl track) — fixtures/tests that change BYTES
    ;; under the design-blessed D-P1.PAD / D-P4 / D-P5 contracts.
    ;;
    ;; 2026-06-17: round-10 TEST-track re-bless (change <this commit>).
    ;; 15 of the 19 entries CLOSED — fixtures regenerated from impl's tip
    ;; umrpsoxp via the FROZEN-CLOCK renderer (make regen-mockups, guards
    ;; active) and the byte/assertion tests re-blessed to the new D-P1.PAD
    ;; / D-P4 / D-P5 contracts:
    ;;   D-P1.PAD (pill pad-cols, text-layer geometry): mockup-80/120/160
    ;;     regen'd; r10-item-row, v1b, b1, b2, s5a, ux-u3 (x2) re-rendered
    ;;     wide (160) where the padded cluster no longer truncates the
    ;;     searched title; r9-c2 re-blessed to the NEW pure-overlay
    ;;     contract (pad-cols=0 -> pill == text; pad-cols=1 -> pill wider).
    ;;   D-P4 (centred calendar): r9-d5b re-blessed — text blocks share the
    ;;     content spine, the calendar grid is centred OFF it.
    ;;   D-P5 (shared-row project view): project-view-{state,dir,tag}
    ;;     fixtures regen'd; f5-tree-structure + f5-grouping-toggle
    ;;     rewritten to the shared-row sections (no box glyphs/roll-up).
    ;; -------------------------------------------------------------------
    ;; 2026-06-17: round-10 CLOSEOUT — the last 4 entries CLOSED on impl's
    ;; narrow-tier fix nzruxkrm (clamps the assembled LEFT to width -
    ;; cluster-w - gap, so a wide [#A] prefix + reserved pad cols can no
    ;; longer shove the V6 cluster right).  Byte-verified the fix is a
    ;; NO-OP at the wide tiers (W80/120/160/90/104/110 + project + empty
    ;; fixtures byte-identical to the earlier regen ovrwrtzt); regenerated
    ;; ONLY the W96/W100 mockups, which now show the V6 date column ALIGNED
    ;; (W96 and W100 each render a single date column).  All four deleted:
    ;;   org-air-layout-mockup-thresholds, org-air-layout-mockup-heights,
    ;;   org-air-r9-c1-narrow-resize-refits,
    ;;   org-air-r9-c3-text-scale-refit-consistent.
    ;; Round-10 manifest is EMPTY; the tests stay as permanent regression
    ;; guards (V6 alignment at every tier).
    ;; ===================================================================
    ;; passed-unexpectedly through the anti-tautology render guards
    ;; (real renderer produced the bytes: width seam, 80/120/160
    ;; composition, byte-precise §3 mockups, divider geometry,
    ;; persistent calendar, empty-board shape, summary integrity, rail
    ;; faces, TTY fallback); the 17th, org-air-viewport-no-trailing-
    ;; whitespace, was RETIRED with its test as §9.1-obsolete: spec rev
    ;; orwonzvz makes plain-space padding to full width the contract
    ;; (comparison is right-trimmed), so trailing whitespace is mandated
    ;; — drift protection now lives in the exact-width assertions and
    ;; the byte-precise mockup comparison.
    ;; 2026-06-12: all 3 F1/F2a/F3 data-variation entries closed out —
    ;; impl2's de-hardcoding (svknxvow) verified honest on both boards
    ;; (calendar = true data union, no TTY glyph leaks).  Note: the
    ;; fidelity review's hand-derived original-fixture union missed the
    ;; real Jun 19 deadline; the test now parses ground truth from the
    ;; fixture files directly.
    ;; 2026-06-13: mockup-80/120/160 closed out — fixtures regenerated
    ;; ONCE from the honest D1-D7 renderer (ymvlyvuk) via make
    ;; regen-mockups (guards active), plus the new threshold set
    ;; 90/96/100/104/110; design FINAL-blessed at rpuxmmlz.
    ;;
    ;; Screenshot round (GUI bugs the byte gate missed; impl2 punch
    ;; list — delete as fixes land):
    ;; 2026-06-13: S1 entry closed — impl2 nnoosnpn removed the
    ;; header-line path; verified header-line-format nil on both glyph
    ;; paths, and the v0.1 filter-chip test rewritten to read the
    ;; in-buffer band.
    ;; 2026-06-13: 4 stale-mockup entries closed — S-round one-shot
    ;; regen executed against impl2 nnoosnpn@addb0c59 (widths ×
    ;; {natural,24,50} + empty-board@120x50); routed to design for
    ;; re-blessing.
    ;; 2026-06-13: S5/S6 entries closed — impl2 nnoosnpn@addb0c59
    ;; verified honest: 3-tier glyph table complete incl. tees (ASCII
    ;; final fallback), org-air-view-height defcustom adopted per the
    ;; proposed contract, 120x50 fill + empty-board sparse fill exact.
    ;;
    ;; 2026-06-13: triage-round closeout (impl2 wrqpvwzt + test-track
    ;; reconciliation).  All 9 grind entries closed:
    ;;  - 6 verified passed-unexpectedly on impl2 wrqpvwzt: S7 right-
    ;;    margin (status ends at W-1), S8 buffer-local line-spacing,
    ;;    triage scope remap (S/M-s), process-inbox entry point, the
    ;;    dated-inbox 'file with r' hint, and S5a point-on-visible across
    ;;    all point-moving paths.
    ;;  - 3 fixed on the TEST track (orchestrator ruling; impl frozen):
    ;;    keymap-dispositions T-row now expects org-air-item-cycle-todo
    ;;    (org-air-item-todo stays the cl-defstruct accessor); and the
    ;;    data-variation board's dated items were re-dated inside the
    ;;    upcoming window (dated-inbox 23->17 Jun, midsummer 25->22 Jun)
    ;;    so every calendar mark is backed by a visible date-bucket row
    ;;    (ruling xsqrnoyn consistency invariant).  The section-rows test
    ;;    helper now scans each line for the org-air-section/org-air-item
    ;;    properties (they sit past the left margin), not just column 0.
    ;;
    ;; v0.3 ROUND-7 closeout (impl2 rlqqoumn): all 9 grind entries
    ;; verified passed-unexpectedly and deleted — R3 k≠kill+j/k scroll,
    ;; R7 today-cell-no-glyph + legend-drops-today, R8 Sunday-first, R10
    ;; tags-max-2 + right-cluster, R6 day-view-command + clickable-cells,
    ;; R4 footer-removed (the rail-hint expansion landed in impl2's amend,
    ;; so the verbs are no longer lost).
    ;;
    ;; v0.4 ROUND-8 closeout (impl2 ysmvqzto): all 9 F5/V6/V3 grind
    ;; entries verified passed-unexpectedly and deleted — F5 sources/
    ;; detect/command/mode/P/tree-structure/grouping, V6 date-column +
    ;; dates-align, V3 tag-style.  The V6 metadata table truncated long
    ;; titles at narrow widths, so the title-search helpers were rendered
    ;; wide (160) or switched to org-air-item-property lookups, and the
    ;; v1b inline tests were reworked to the V6 column order.
    ;;
    ;; 2026-06-14: f5-grouping-toggle distinctness grind CLOSED — impl2
    ;; yxvztrsy landed genuinely distinct dir/tag groupings (commands
    ;; renamed to org-air-project-group-by-{state,directory,tag}); the
    ;; three renders are now pairwise byte-different.  project-view-dir/
    ;; -tag regenerated to the distinct trees; state-view + V6 dashboard
    ;; stay blessed.
    ;;
    ;; v0.4 ROUND-9 C1-Q1 closeout (impl sxyxrpzk, verified on impl's
    ;; tip): all C1/C2/C3 guards pass and F1 (×3) + Q1 scope-reset closed
    ;; — impl landed `org-air-origin-style' (auto Denote-aware), the
    ;; Denote-aware `org-air-view--origin' (strip id/__tag/.org), and the
    ;; rail `(S clears)' scope-reset hint; the D5 `org-air-face-rail-key'
    ;; face also landed via design utwrpzmx.  Five manifest entries
    ;; deleted here as passed-unexpectedly.
    ;;
    ;; v0.4 ROUND-9 D5 closeout (impl settled tip ysvynsyl, verified):
    ;; ALL 9 D5 rail-structure grinds flipped GREEN — hrule-cap glyph,
    ;; the labelled-rule family (Summary/Filters/calendar-as-rule + named
    ;; Actions), content-spine inset (org-air-rail-content-inset=3),
    ;; spaced legend, short ledger-sum rule, column-aligned no-dot verbs,
    ;; and rail-key keycaps applied.  Manifest entries deleted; the tests
    ;; stay as permanent regression guards.  Round-9 manifest is EMPTY.
    ;;
    ;; ===================================================================
    ;; v0.4 ROUND-11 closeout (impl tip slvutooqm + test re-bless
    ;; <this commit>).  ALL 11 grind entries CLOSED — fixtures regenerated
    ;; from impl's render via the FROZEN-CLOCK renderer (make regen-mockups,
    ;; guards active) and the assertion tests re-blessed to the new
    ;; design-blessed D-P1/D-P3/D-P5/D-P6/D-P7 contracts:
    ;;   D-P6 (prefix-marker rail headers, no hl-block rule/overline):
    ;;     r9-d5a-rail-rule-family-has-cap, -actions-block-named, and
    ;;     layout-rail-faces-applied re-blessed to `▌ Label' headers +
    ;;     org-air-face-rail-header/-marker (rail-title/card-header retired).
    ;;   D-P5 (calendar header full-width): r9-d5a-calendar-is-labelled-rule
    ;;     re-blessed to the full-width `▌ June 2026 … ‹ ›' marker header
    ;;     (grid stays centred).
    ;;   D-P2 #4 (display): s8-line-spacing re-blessed — line-spacing is now
    ;;     org-air-line-spacing (0.15 default), reversing the S8 zero.
    ;;   D-P7 (lower-rail inspector): r9-c1-narrow-resize-refits re-blessed —
    ;;     (c) now skips the inspector block's own rail `⌂' origin (tagged
    ;;     org-air-inspector) and guards only board item-row origins; the
    ;;     inspector's first-item field text is byte-tested via the mockups.
    ;;   D-P1 (uniform date pills) + D-P3 (no-trim overflow tag clusters):
    ;;     the byte mockups (80/120/160 + heights + thresholds) regenerated;
    ;;     verified the date column stays single-valued at every tier and
    ;;     the ⌂ origin column does not drift on overflow vs non-overflow
    ;;     rows (per-filename single column at W96/120/160).
    ;; Round-11 manifest is EMPTY; the tests stay as permanent regression
    ;; guards.  No real impl bug surfaced this round (inspector renders as
    ;; the LAST foot block, does not disturb board rows or move point; V6
    ;; alignment and origin alignment hold).  No .el source touched.
    ;; ===================================================================
    ;; v0.4 ROUND-12 closeout (impl tip lvosluym + test re-bless
    ;; <this commit>).  ALL 10 grind entries CLOSED — fixtures regenerated
    ;; from impl's render via the FROZEN-CLOCK renderer (make regen-mockups,
    ;; guards active; it NO LONGER HANGS — impl made the inspector inert
    ;; when noninteractive, so the round-11 regen deadlock is fixed) and the
    ;; assertion tests re-blessed to the design-blessed D-P1/D-P2/D-P3
    ;; contracts (air/v0.4/org-air-round12-design.org):
    ;;   D-P1 rail reorder Calendar/Summary/Inspector/Filters/Actions with
    ;;     the inspector moved into a fixed reserved mid-rail region
    ;;     (Filters+Actions pinned to the foot): layout-mockup-80/120/160/
    ;;     heights/thresholds regenerated wholesale.
    ;;   D-P2 origin glyph ⌂→▤ (GUI/preferred), ASCII/TTY tier `.':
    ;;     item-row + inspector origin cells; f5-project-view-byte-mockups
    ;;     regenerated (shared row TTY origin H→.); r10-item-row-right-
    ;;     cluster, v1b-inline-tag-placement, v1b-origin-protected re-blessed
    ;;     to assert ▤ (was ⌂).
    ;;   D-P3 line-spacing default 0.15→0 (solid `│' divider; capsule
    ;;     breathing moved into org-air-pill-vinset): s8-line-spacing
    ;;     re-blessed to 0.
    ;; D-P4 priority badge is a [gui] svg overlay over the unchanged `[#A]'
    ;; text → no fixture moved.  Verified: regen did NOT hang; the `│'
    ;; divider is a single contiguous column down all 30 body rows at
    ;; W96/120/160 (D-P3 fix holds); no extra test failed beyond the 10
    ;; intended deltas; date/origin columns stay aligned.  The inspector is
    ;; inert (blank reserved region) in --batch by design — its live
    ;; per-item content + column-only motion update are interactive and not
    ;; byte-tested here.  No real impl bug surfaced.  No .el source touched.
    ;; Round-12 manifest is EMPTY; the tests stay as permanent guards.
    ;; ===================================================================
    ;; v0.4 ROUND-13 closeout (impl tip ssvowptt + test re-bless
    ;; <this commit>).  ALL 5 grind entries CLOSED — the GTD-board mockups
    ;; regenerated from impl's render via the FROZEN-CLOCK renderer (make
    ;; regen-mockups, guards active; verified NO HANG, exit 0) for the
    ;; design-blessed D-P2 fixed 2-col priority slot (air/v0.4/org-air-
    ;; round13-design.org):
    ;;   D-P2 — every item-row prefix now carries a fixed 2-col slot (`■ '
    ;;     for a shown priority, `␣␣' otherwise) where only priority rows
    ;;     carried `[#A] ' before, so titles left-align across ALL rows;
    ;;     layout-mockup-80/120/160/heights/thresholds regenerated.  The
    ;;     divider + date/tags/origin cluster columns are UNCHANGED.
    ;; New round-13 guards added (passing, not grind):
    ;;   D-P1 — org-air-r13-divider-contiguous-full-height (solid `│' down
    ;;     the whole body now that the pill svg is line-height clamped via
    ;;     org-air-view--svg-line-image) + -svg-line-height-clamp-mechanism.
    ;;   D-P2 — -priority-square-glyph / -priority-slot-fixed-two-col.
    ;;   D-P3 — -board-only-below-rail-min-width + -board-only-byte-mockup
    ;;     (new layout-mockup-70.txt board-only fixture, < rail-min-width).
    ;; D-P1 (pill svg clamp + retuned pill knobs) and D-P4 (brighter
    ;; inspector-label face) are [gui]/[face] → pill/inspector TEXT
    ;; unchanged, fixtures HELD (no regen).  Verified: regen did NOT hang;
    ;; only the 5 D-P2 mockups moved (title left-shift); no extra test
    ;; failed.  No real impl bug surfaced.  No .el source touched.
    ;; Round-13 manifest is EMPTY; the tests stay as permanent guards.
    ;; ===================================================================
    ;; v0.4 ROUND-14 closeout (impl tip supwwrkv + test re-bless
    ;; <this commit>).  ALL 3 grind entries CLOSED — the project-view
    ;; fixtures regenerated from impl's render via the FROZEN-CLOCK renderer
    ;; (make regen-mockups, guards active; verified NO HANG, exit 0) for the
    ;; design-blessed D-P1 project two-line rework + project inspector
    ;; (air/v0.4/org-air-round14-design.org):
    ;;   D-P1.A — each doc is now a TWO-LINE block: line 1 title + inline
    ;;     left-flowing tag pills; line 2 indented `▤ relpath  created…
    ;;     updated…' (NOT right-pinned).  Section headings gain the round-11
    ;;     `▌'/`|' prefix marker; the `↻ date' updated token is gone.
    ;;     f5-tree-structure re-blessed to the two-line block + `created'/
    ;;     `updated' labels + `| [badge] State N' markers.
    ;;   D-P1.B — the view is now TWO-PANE (docs LEFT, Summary+Inspector
    ;;     rail RIGHT); section-header lines no longer END the line (the
    ;;     rail follows the `|' divider).  f5-grouping-toggle re-blessed to
    ;;     the marker-led `| <key> N' headers.  project-view-{state,dir,tag}
    ;;     regenerated wholesale (two-line + rail + first-doc inspector).
    ;;     The project inspector carries relative date terms, so the test
    ;;     render helper now freezes current-time to org-air-test-now
    ;;     (matching the regen tool) for byte stability.
    ;; D-P1.B inspector-core generalisation reproduced the BOARD inspector
    ;; byte-identically — every board fixture HELD (no regen).  D-P2 repeats
    ;; ([byte] ↻ marker + inspector Repeat line) is GREEN: no fixture .org
    ;; carries an Org repeater, so no date cell changed (the detection is
    ;; live; adding a repeating fixture item is deferred with the design's
    ;; D-P2 fixture-bless).  Verified: regen did NOT hang; ONLY the 3
    ;; project tests moved; no real impl bug surfaced.  No .el source
    ;; touched.  Round-14 manifest is EMPTY; the tests stay as guards.
    ;; ===================================================================
    ;; v0.4 ROUND-15 D-P1 grind (impl track) — fixtures/tests that change
    ;; BYTES under the design-blessed reserved TODO-keyword cell
    ;; (air/v0.4/org-air-round15-design-d1.org).
    ;;
    ;; 2026-06-18: D-P1 landed in org-air-view.el — a FIXED reserved
    ;; keyword cell (`org-air-view--meta-todo-w' = widest keyword
    ;; board-wide; `org-air-view--todo-cell' pads the keyword (or blanks
    ;; when absent) to that width + one separator space) replaces the
    ;; conditional `(when todo …)' prefix in `org-air-view--insert-item'.
    ;; Keyword-LESS rows now gain `meta-todo-w + 1' leading spaces so ALL
    ;; titles share one left edge — the title left edge shifts right on
    ;; those rows, regenerating every board fixture that mixes keyword /
    ;; keyword-less items.  Pure text, V6 right-cluster (date/tags/origin)
    ;; unchanged.  These are [byte] fixture/assertion deltas, NOT impl
    ;; bugs; TEST re-blesses + BLESS follows with this change-id.
    ;;
    ;; 2026-06-18: ROUND-15 D-P1 CLOSEOUT (change <this commit>).  All 7
    ;; entries CLOSED — board fixtures regenerated from impl's tip
    ;; xqnzpzlykkxm via the FROZEN-CLOCK renderer (make regen-mockups,
    ;; guards active) and the byte/assertion tests re-blessed to the
    ;; reserved-keyword-cell contract:
    ;;   25 layout-mockup-*.txt fixtures regen'd (W70..W160 + height
    ;;     variants); keyword-less rows (`Quick note', `Call plumber', …)
    ;;     now share the common title left edge with the `TODO ' rows, and
    ;;     short-keyword rows gain right-padding to `meta-todo-w'.
    ;;   org-air-data-variation-titles-render re-blessed to the aligned
    ;;     render; the V6 right-cluster (date/tags/origin) is byte-
    ;;     identical, only the title left edge moved.  Closed entries:
    ;;     org-air-data-variation-titles-render, org-air-layout-mockup-80,
    ;;     -120, -160, -heights, -thresholds, org-air-r13-board-only-byte-
    ;;     mockup.  Round-15 D-P1 manifest is EMPTY.
    ;; ===================================================================
    ;; v0.5 ROUND-16 D-P4/D-P5 CLOSEOUT (impl tip lqpnpklprpwk + test
    ;; re-bless <this commit>).  The single grind entry CLOSED — the three
    ;; project fixtures regenerated from impl's render via the FROZEN-CLOCK
    ;; renderer (make regen-mockups, guards active; verified NO HANG) for
    ;; the design-blessed sortable project view
    ;; (air/v0.5/org-air-round16-design.org):
    ;;   D-P4 — the project header gained the right-clustered sort badge
    ;;     `↕ <key> <dir>' (TTY `| name ^', the ascending name default);
    ;;     doc rows are now ordered by ONE deterministic comparator
    ;;     (name/created/updated + direction).
    ;;   D-P5 — within a directory/tag group, rows run state-rank primary
    ;;     (Draft→Ready→WIP→Review→Complete) then the sort key secondary,
    ;;     from the SAME comparator.
    ;; project-view-{state,dir,tag}.txt re-ordered + gained the badge line:
    ;;   - state grouping: each group is one state -> the name key drives
    ;;     order (Draft group now Epsilon < Gamma).
    ;;   - dir grouping (v0.1/): Draft Gamma -> Ready Alpha -> Complete Beta
    ;;     (state primary, name secondary).
    ;;   - tag grouping (#context): the two Drafts sort Epsilon < Gamma.
    ;; ONLY the three project fixtures moved (board mockups byte-identical,
    ;; confirming the change is project-view-local).  No real impl bug
    ;; surfaced; no .el source touched.  Round-16 manifest is EMPTY; the
    ;; tests stay as permanent regression guards.
    ;; ===================================================================
    ;; v0.5 ROUND-17 D-P1/D-P2 CLOSEOUT (impl tip wvxmvqlu + test re-bless
    ;; <this commit>).  ALL 8 grind entries CLOSED — fixtures regenerated
    ;; from impl's render via the FROZEN-CLOCK renderer (make regen-mockups,
    ;; guards active; verified NO HANG, exit 0) and the byte/assertion tests
    ;; re-blessed to the design-blessed origin-cap + title-protected budget
    ;; (air/v0.5/org-air-round17-design.org):
    ;;   D-P1.C/D (origin cap + title-min fit): the GTD board mockups moved
    ;;     wherever the board pane would starve the title below 24 — the
    ;;     title gains columns while the origin (then tags) yield (origin
    ;;     floored at `org-air-origin-min' 12).  Regen'd: layout-mockup-80
    ;;     + -120 (board-pane ~83) + the 90/96/100/104/110 thresholds + the
    ;;     x24/x50 height variants + the W70 board-only fixture.  W160
    ;;     natural/x24 are byte-identical (board pane ~113 never starves the
    ;;     title); 160x50 + the x50 tier moved ONLY on the D-P2 inspector
    ;;     de-slug line (▤ inbox/inbox.org → ▤ inbox.org, the redundant
    ;;     group dropped).  Closed: org-air-layout-mockup-80, -120,
    ;;     -heights, -thresholds, org-air-r13-board-only-byte-mockup.
    ;;   D-P1 #2 (isolated long-Denote byte golden): denote-origin-{80,120}
    ;;     .txt regen'd + blessed via org-air-regen--write-denote (NOT in the
    ;;     GTD board *.org set, so the 25 layout mockups stay local to the
    ;;     above deltas).  Closed: org-air-r17-denote-origin-byte-mockup.
    ;;   The two pre-R17 contract tests reworked on the TEST track to the
    ;;     title-protected contract (the D2 origin-protected priority is
    ;;     inverted): org-air-r9-f1-denote-origin-rendered-and-truncates now
    ;;     asserts the de-slug PREFIX + the `<= org-air-origin-max-width' cap
    ;;     (the full long slug no longer surfaces); org-air-v1b-origin-
    ;;     protected-on-overflow now asserts the ORIGIN yields (capped, then
    ;;     shrunk to its floor → truncated with the ellipsis) while the
    ;;     TITLE keeps its guaranteed minimum.  Both deleted here as
    ;;     passed-unexpectedly after the rework.
    ;; New round-17 guards added on the test track (passing, not grind):
    ;;   org-air-r17-long-denote-origin-keeps-title — the title-floor guard
    ;;     the line-width-only F1 test missed; asserts on the MODELED row
    ;;     (`org-air-priority-style' square slot + a BARE date, NOT the
    ;;     Inbox-nudged row) so the floor is the genuine budget guarantee.
    ;;   org-air-r17-origin-capped-{defcustoms,unit} + -compute-meta-widths-
    ;;     title-budget — the cap + budget units.
    ;;   org-air-r17-inspector-origin-deslugs — D-P2 + the design-approved
    ;;     no-redundant-group refinement (ouyqxrnt): de-slugged title shows,
    ;;     the defaulted Denote-base group is DROPPED, a real #+CATEGORY
    ;;     group is shown DE-SLUGGED.
    ;;   org-air-r17-project-line2-deslugs-leaf — D-P2 project line-2 leaf.
    ;; Pre-existing harness artifact fixed (was path-dependent, only green
    ;; on the blessing machine): org-air-f5-project-view-byte-mockups now
    ;; freezes the inspector `Path' via `directory-abbrev-alist'
    ;; (org-air-test-with-frozen-project-path → `~air/…') in BOTH the test
    ;; render and the regen, so the project goldens are path-INDEPENDENT and
    ;; deterministic across checkout roots (~/… vs /tmp/…).
    ;; Verified: regen did NOT hang; the board deltas are exactly the
    ;; title-gain / origin-yield rows + the D-P2 inspector de-slug; no extra
    ;; test failed; V6 date/origin columns stay aligned.  No .el SOURCE
    ;; touched (the impl landed D-P1/D-P2 in ruskyvwn+yuurwyro).  Round-17
    ;; manifest is EMPTY; the tests stay as permanent regression guards.
    ;; ===================================================================
    ;; v0.5 ROUND-19 CLOSEOUT (impl tip kpzqqxyw + test re-bless <this
    ;; commit>).  ALL 7 grind entries CLOSED — the rail/nudge byte goldens
    ;; regenerated from impl's render via the FROZEN-CLOCK renderer (make
    ;; regen-mockups, guards active; verified NO HANG, exit 0) and the two
    ;; rail assertions + the triage nudge assertion re-blessed to the
    ;; design-blessed R19-2(c) / R19-4c/d contracts
    ;; (air/v0.5/org-air-round19-design.org):
    ;;   R19-2(c) de-cryptify the dated-Inbox nudge — the row suffix
    ;;     `· file with r' -> `· r to file' (`org-air-view--item-date-text').
    ;;     Re-blessed denote-origin-{80,120}.txt (the two goldens carrying
    ;;     it) + the triage assertion org-air-triage-dated-inbox-row-
    ;;     carries-file-hint (now asserts `r to file', forbids `file with
    ;;     r').  The 2-col-shorter nudge frees title cells, so denote-80's
    ;;     title de-truncates a hair (`File the …' -> `File the re…').
    ;;   R19-4c/d rail reorder + crisp Scope/Filter split — the Filter
    ;;     block MOVED UP to between Calendar and Summary (new order
    ;;     Calendar -> Filter -> Scope -> Summary -> Inspector -> Actions;
    ;;     only Actions stays foot-pinned), the single `▌ Filters' header
    ;;     split into `▌ Filter' (live tags) + `▌ Scope' (structural lens)
    ;;     with `s changes · S clears' on the Scope line.  Regen'd every
    ;;     side-rail-bearing golden (layout-mockup 90/96/100/104/110/120/
    ;;     160 + x24/x50 variants + empty-120x50 + denote-origin-120);
    ;;     re-blessed org-air-r9-d5a-rail-rule-family-has-cap (`▌ Filters'
    ;;     -> `▌ Filter') and org-air-r9-q1-rail-advertises-scope-reset
    ;;     (inline `Scope: #work  (S clears)' -> labelled `▌ Scope' block +
    ;;     `#work   s changes · S clears').  The rail-only mode-line (a) is
    ;;     byte-invisible (per-buffer mode-line stripped by the goldens);
    ;;     the `\ clears' hint (b) only shows with a filter ACTIVE so it is
    ;;     covered by the new org-air-r19-4-clear-hint-shows-clear-key ERT,
    ;;     not the unfiltered goldens; the help (d) is a `message'.
    ;; The board-only (70) + top-rail (80) + project + entry-view fixtures
    ;; are byte-identical (no side rail / no nudge), confirming the deltas
    ;; are local to the rail and the Inbox nudge.  New R19 substantive ERTs
    ;; (async first load, refile UX, editable view pane, rail order) added
    ;; on the test track in tests/org-air-round19-test.el.  No .el SOURCE
    ;; touched (the impl landed R19-1..R19-4 in ltpkvxvy..kpzqqxyw).
    ;; Round-19 manifest is EMPTY; the tests stay as permanent guards.
    ;; ===================================================================
    ;; v0.5 ROUND-20 grind (impl track) — R20-5 project view rebuild.
    ;;
    ;; 2026-06-26: R20-5(a) NESTED directory tree landed (the project
    ;; `directory' grouping now matches `airctl status -Da': a rolled-up
    ;; top-dir box header with state-NAME totals, a per-dir `BADGE N (+M)'
    ;; count heading with the descendant roll-up, state-first own docs, and
    ;; depth-indented child dirs) — replacing the FLAT first-segment
    ;; grouping.  The default group is now `directory'.  This re-blesses
    ;; the project-view-dir.txt golden (and the state/tag goldens re-bless
    ;; once R20-5(b) swaps in the shared board rail), so the byte-mockup
    ;; test fails until the test seat regenerates the fixtures; the
    ;; grouping-toggle test's dir-header regex (`v0.N/ <count>') no longer
    ;; matches the badge-led tree header and is re-blessed there too.
    (org-air-f5-project-view-byte-mockups
     . "R20-5(a): nested dir tree + shared rail re-bless project-view-*.txt")
    (org-air-f5-grouping-toggle
     . "R20-5(a): dir grouping is now the airctl -Da nested tree (header form changed)")
    ;; 2026-06-26: R20-5(b) truly REUSE the dashboard core landed: the
    ;; project rail is now the SHARED board rail (Calendar/Filter/Scope/
    ;; Summary/Inspector/Actions) driven by a buffer-local view descriptor
    ;; (`org-air-view--rail-descriptor'), and the project keymap is a THIN
    ;; child of `org-air-view-core-map' that no longer SHADOWS the shared
    ;; board keys — s / d / t / o / O are gone (state/tag/sort move to
    ;; M-x).  The two R18 keymap drift-guards asserted the OLD project-
    ;; specific bindings (s=group-by-state, o=sort-cycle, O=sort-reverse),
    ;; so they fail until the test seat re-blesses them to the R20-5 thin
    ;; keymap (the new drift guard: shared keys resolve to the board's).
    (org-air-r18-dp3-project-keymap-inherits-core
     . "R20-5(b): thin keymap drops s/o domain shadows (re-bless drift guard)")
    (org-air-r18-dp4-keymap-ret-and-sret-board-and-project
     . "R20-5(b): thin keymap drops O=sort-reverse / s=group-by-state shadows")
    ;; ===================================================================
    ;; (test-symbol . "reason")  — none right now.
    )
  "Alist of (TEST-SYMBOL . REASON) for tests expected to fail.")

(defun org-air-test-apply-known-failures ()
  "Mark every test in `org-air-test-known-failures' as expected-to-fail.
Signal an error for manifest entries that name no loaded test, so the
manifest cannot rot silently."
  (pcase-dolist (`(,name . ,reason) org-air-test-known-failures)
    (let ((test (and (ert-test-boundp name) (ert-get-test name))))
      (unless test
        (error "Known-failures manifest names unknown test: %s (%s)"
               name reason))
      (setf (ert-test-expected-result-type test) :failed)
      (message "known-failure: %s — %s" name reason))))

(provide 'org-air-known-failures)
;;; org-air-known-failures.el ends here
