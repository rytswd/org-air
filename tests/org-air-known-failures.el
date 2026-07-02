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
    ;; v0.5 ROUND-20 CLOSEOUT (impl tip urnnozpp + test re-bless <this
    ;; commit>).  ALL 6 grind entries CLOSED — the project + view-pane byte
    ;; goldens regenerated from impl's render via the FROZEN-CLOCK renderer
    ;; (make regen-mockups, guards active; verified NO HANG, exit 0) and the
    ;; three assertion tests re-blessed to the design-blessed R20-5 / R20-3
    ;; contracts (air/v0.5/org-air-round20-design.org):
    ;;   R20-5(a) NESTED directory tree — the `directory' grouping now
    ;;     matches `airctl status -Da' (verified against the REAL Air repo
    ;;     ~/Coding/github.com/withre/air): a rolled-up top-dir header with
    ;;     state-NAME totals (`| v0.1/  [R] Ready (1) …'), a per-dir
    ;;     `BADGE N (+M)' count heading with the `(+M)' descendant roll-up,
    ;;     state-first own docs, and a depth-indented child node
    ;;     (`|   air-context/  [D] 1') — replacing the FLAT first-segment
    ;;     grouping.  `directory' is the DEFAULT group now.
    ;;     project-view-dir.txt re-blessed wholesale; org-air-f5-grouping-
    ;;     toggle's dir-header regex re-blessed from `v0.N/ <count>' to the
    ;;     three nested-tree hallmarks (rolled-up header / nested child dir
    ;;     heading / `(+N)' roll-up, with state+tag asserted roll-up-FREE).
    ;;   R20-5(b) shared dashboard core — the project rail is now the SHARED
    ;;     board rail (Calendar/Filter/Scope/Summary/Inspector/Actions) via
    ;;     the buffer-local `org-air-view--rail-descriptor', and the project
    ;;     keymap is a THIN child of `org-air-view-core-map' that no longer
    ;;     SHADOWS s/d/t/o/O (state/tag/sort move to `M-x').  ALL three
    ;;     project goldens (dir/state/tag) re-blessed to carry the full
    ;;     rail; the two R18 keymap drift-guards re-blessed to the thin
    ;;     keymap (s/o/O drop their project-specific overrides; the shared
    ;;     core keys RET/v/V/\/M-/ still resolve identically board↔project).
    ;;   R20-3(a) pane close-key hint — both view-pane goldens (entry-view-
    ;;     pane.txt + entry-view-dead.txt) gain the trailing `· q close'
    ;;     header-line hint; the entry snapshot BODY bytes are unchanged.
    ;; The R20-1/R20-2/R20-3/R20-6 changes are GUI/interactive and INERT
    ;; under `noninteractive', so EVERY board byte golden (layout-mockup-*,
    ;; denote-origin-*) is byte-identical — only the 5 project+pane fixtures
    ;; moved, confirming the change is project-view / pane-local.  New R20
    ;; substantive ERTs (sync first load + wedge guard, pane q/C-c C-q close
    ;; + cheap same-file follow, refile action-first menu + CRM category +
    ;; move-to-file, perf partition + displayed-only meta-widths + bench;
    ;; PLUS the test-seat additions: airctl-tree directory model, shared
    ;; rail/keymap parity, status mode-line) cover the live behaviour.  No
    ;; .el SOURCE touched (the impl landed R20-1..R20-6 in votptnto..
    ;; urnnozpp).  Round-20 manifest is EMPTY; the tests stay as permanent
    ;; regression guards.
    ;; ===================================================================
    ;; v0.5 ROUND-21 (impl track) — RESOLVED by the test seat.
    ;;
    ;; 2026-06-26: R21-5 landed in org-air-project.el — each Air doc now
    ;; renders as ONE board-style row via the SHARED
    ;; `org-air-view--insert-row' (fixed-width state cell + title + the
    ;; V6 date/tags/origin meta cluster), DROPPING the old two-line emoji
    ;; block.  The nested dir tree, per-dir `BADGE N (+M)' counts and the
    ;; shared rail/keymap are UNCHANGED.  The test seat regenerated the
    ;; three project goldens (project-view-{dir,state,tag}.txt via
    ;; `make regen-mockups', frozen clock) and re-blessed the
    ;; `org-air-f5-tree-structure' assertion to the one-line contract, so
    ;; `org-air-f5-project-view-byte-mockups' + `org-air-f5-tree-structure'
    ;; PASS again — removed from this manifest.  Board / entry-view /
    ;; denote goldens are byte-identical (R21-1/2/4/6 are byte-invisible:
    ;; point column / title mark / svg overlay / chrome faces), confirmed
    ;; by the new substantive ERTs in org-air-round21-test.el.
    ;; ===================================================================
    ;; v0.5 ROUND-22 CLOSEOUT (impl tips pnkrznqp..vmuunmts + test re-bless
    ;; <this commit>).  ALL 21 grind entries CLOSED — the rail/mode-line +
    ;; project byte goldens regenerated from impl's render via the
    ;; FROZEN-CLOCK renderer (make regen-mockups, guards active; verified NO
    ;; HANG, exit 0) and the assertion ERTs re-blessed to the design-blessed
    ;; R22 contracts (air/v0.5/org-air-round22-design.org):
    ;;   R22-4 (13) scope->SOURCE / FILTER wording: every rail-bearing byte
    ;;     golden (layout-mockup 90/96/100/104/110/120/160 + x24/x50 + empty-
    ;;     120x50 + denote-origin-120) re-blessed to `▌ Filter'/`none' +
    ;;     `▌ Source'/`all items · M loaded' + `s source'; the wording ERTs
    ;;     re-blessed — r19-4 rail order (Scope->Source header), r9-q1 scope-
    ;;     reset (Source header + `· N loaded' before `s changes · S clears'),
    ;;     r9-d5b content-spine (`No filters'->`none' pick), r4-footer
    ;;     (`s scope'->`s source'), layout empty-board (`none' + `M loaded'),
    ;;     r20-2 board-status (`no filter'->`filter none', `scope'->`source'),
    ;;     r20-5 project-reuses-rail (`| Scope'->`| Source').
    ;;   R22-6 (3) project by-directory: project-view-dir.txt regen'd to the
    ;;     ONE-header/dir + right-aligned letter-count summary (`R1 C1 D(+1)'/
    ;;     `W1 X1 D1') + deeper doc indent; f5-grouping-toggle + r20-5-fix-
    ;;     directory-render re-blessed off the `[X] N' badge wall to the
    ;;     letter-count regexps.  airctl `status -Da' parity RE-VERIFIED on
    ;;     ~/Coding/github.com/withre/air (the r20-5-fix guard): v0.1/ own
    ;;     R4 C14 X1 D2 + desc +1 +14 +9 +8 -> `R4(+1) C14(+14) X1(+9) D2(+8)'
    ;;     byte-for-byte (pinned by org-air-r22-6-dir-count-summary-matches-
    ;;     airctl); the new format keeps the same per-dir counts/totals.
    ;;   R22-3 (4) dashboard sort: o/O are the SHARED view-core sort now
    ;;     (org-air-view-sort-cycle/-reverse); r18-dp4-keymap + r18-dp3-
    ;;     project-inherits-core + r20-5-project-keymap-shares + view-ret-
    ;;     bound-to-visit re-blessed to the NEW reality (board `O' = sort-
    ;;     reverse, project o/O inherit the shared sort, TTY visit moved to
    ;;     g RET / g o, S-RET visits, RET = pane).
    ;;   R22-7 (1) pane chrome: r18-dp5-pane-header-chrome-faces re-blessed —
    ;;     the pane filename rides the readable `org-air-face-inspector-label'
    ;;     (>= WCAG AA) now, not the sub-AA `org-air-face-faded'.
    ;; SUBSTANTIVE new ERTs added on the test track (the under-covered fixes):
    ;; org-air-round22-test.el covers R22-1 priorities A..E (the KEY gap: a
    ;; #A..#E fixture/board exercises every level's square + face, the
    ;; explicit-[#B] parser fix, the no-cookie blank), R22-2 cursor (line-
    ;; based row resolution from col0/margin/rail + --normalize-point snap),
    ;; R22-3 sort (--sort-items within-bucket order, cycle/reverse, default
    ;; byte-identical, indicator gating, shared board+project pair), R22-4
    ;; (Source/Filter wording + `N of M shown' only when narrowed), R22-5
    ;; (project `|' rail-toggle no-crash + pane shows the doc FILE), R22-6
    ;; (airctl parity / one-header / right-aligned counts / nesting indent),
    ;; R22-7 (pane filename/state + origin WCAG >= 4.5, title strongest).
    ;; Board / entry-view byte goldens not bearing the rail (mockup-70/80,
    ;; entry-view-pane/dead) are byte-identical (R22-1/2/3/5/7 are byte-
    ;; invisible: svg overlay / point+property / keymap / faces / interactive).
    ;; No .el SOURCE touched (the impl landed R22-1..R22-7 in pnkrznqp..
    ;; vmuunmts).  Round-22 manifest is EMPTY; the tests stay as permanent
    ;; regression guards.
    ;; ===================================================================
    ;; v0.5 ROUND-23 CLOSEOUT (impl tips qupwuplw..xsrptorp + test re-bless
    ;; <this commit>).  ALL 4 grind entries CLOSED — the ONE moved golden
    ;; regenerated from impl's render via the FROZEN-CLOCK renderer (make
    ;; regen-mockups, guards active; verified NO HANG, exit 0) and the three
    ;; assertion ERTs re-blessed to the design-blessed R23 contracts
    ;; (air/v0.5/org-air-round23-design.org):
    ;;   R23-2 (mode-line off by default): `org-air-modeline-style' default
    ;;     flipped `calm' -> `default' (org-air leaves the user's own mode-
    ;;     line alone; calm is opt-in), and `--install-modeline' is symmetric
    ;;     (kill-local-variable on the non-calm path).  Retargeted +renamed
    ;;     org-air-r18-dp5-modeline-style-default-is-calm ->
    ;;     -is-default (asserts the `default' ship).  byte-INVISIBLE (the
    ;;     mode-line is not buffer text; every golden is byte-identical).
    ;;   R23-3 (project tree connectors): child dir headers now lead with
    ;;     faded `box' tree connectors (batch `+- ' / `|  ', GUI `├─ └─ │')
    ;;     in `org-air-face-air-tree', threaded down `--insert-dir-node' via
    ;;     new rails/lastp params; top dirs keep the `▌'/`|' accent marker;
    ;;     doc rows + counts + airctl `-Da' parity UNCHANGED.  Regenerated
    ;;     ONLY project-view-dir.txt (child-dir HEADER lines `    | air-
    ;;     context/' -> `  +- air-context/'); the state/tag goldens + every
    ;;     board golden are byte-identical (verified jj diff --stat = 1 file).
    ;;     Retargeted org-air-r22-6-nesting-indents-deepen (metric ->
    ;;     connector NAME column, since the connector sits at the marker's
    ;;     leading column) and org-air-f5-grouping-toggle (child-dir regex
    ;;     `| air-context/' -> `+- air-context/').  airctl `status -Da'
    ;;     parity RE-VERIFIED on ~/Coding/github.com/withre/air: v0.1/ own
    ;;     R4 C14 X1 D2 + desc +1 +14 +9 +8 unchanged by the connectors
    ;;     (org-air-r22-6-dir-count-summary-matches-airctl stays green).
    ;;     org-air-f5-project-view-byte-mockups passes on the regen'd golden.
    ;; SUBSTANTIVE new ERTs added on the test track (tests/org-air-round23-
    ;; test.el): R23-1 refile face-leak (source strip from a fontified
    ;; buffer + the defensive `--insert-row' strip GUARD + a real
    ;; refile->refresh end-to-end proving the moved row carries org-air
    ;; faces ONLY + V6 width-lock), R23-2 mode-line (default leaves the user
    ;; line across board/rail/pane/project; calm installs; runtime toggle-
    ;; back restores), R23-3 connectors (child connector faced air-tree +
    ;; top marker; depth-2 ancestor rail + last-vs-tee under the GUI stub),
    ;; R23-4 emoji badges (default emoji; batch token byte-stable; emoji on
    ;; GUI + text/badge styles; cell pixel-lock; VS16 + airctl glyph parity).
    ;; R23-1/R23-2/R23-4 are byte-INVISIBLE (text-property strip / mode-line
    ;; not buffer text / GUI-gated emoji with the batch token fallback), so
    ;; no board/state/tag/entry/denote golden moved.  No .el SOURCE touched
    ;; (the impl landed R23-1..R23-4 in qupwuplw..xsrptorp).  Round-23
    ;; manifest is EMPTY; the tests stay as permanent regression guards.
    ;; ===================================================================
    ;; v0.5 ROUND-24 (impl track) — the ONE design-blessed byte golden that
    ;; changes under R24-2 (tree rails extend DOWN to the leaf doc rows).
    ;;
    ;; 2026-06-27: R24-2 landed in org-air-project.el — `--insert-dir-node'
    ;; now threads the dir's rails/last-child flag into its OWN docs (emitted
    ;; before the child dirs) and `--insert-doc-row' PAINTS the doc's leading
    ;; gutter with the faded `org-air-face-air-tree' ancestor rails + the
    ;; doc's own `├─'/`└─' connector (ascii `+-'/`|' in batch), sized to
    ;; EXACTLY the old plain-indent width so the state cell / title / right
    ;; date-tag-origin cluster stay V6-locked.  This is the design-blessed
    ;; re-bless of the DIRECTORY golden's DOC-row gutters only: in
    ;; tests/fixtures/project-view-dir.txt the doc rows' leading SPACES
    ;; (`        [R]') become tree glyphs (`  +-    [R]'; nested docs gain a
    ;; `|' ancestor rail).  The dir-header lines (already railed by R23-3),
    ;; the right-pinned cluster, and the STATE-/TAG-grouping goldens are
    ;; byte-IDENTICAL (verified: state/tag renders equal the fixtures; only
    ;; the dir grouping differs).  Per the impl brief the fixture is NOT
    ;; edited here — the golden comparison is manifested expected-to-fail
    ;; until the test-track re-blesses project-view-dir.txt via
    ;; `make regen-mockups'.  Counts + airctl `-Da' parity unchanged
    ;; (org-air-r22-6-* stay green); the R24-2 rail/last-corner/V6-lock
    ;; behaviour is covered by org-air-round24-test.el (org-air-r24-2-*).
    ;;
    ;; 2026-06-27: R24-2 CLOSEOUT — test-track re-bless landed.
    ;; `make regen-mockups' (FROZEN-CLOCK renderer, anti-tautology guards
    ;; active) regenerated ONLY tests/fixtures/project-view-dir.txt: the doc
    ;; rows' leading SPACES (`        [R]') became faded air-tree gutters
    ;; (`  +-    [R]'; the nested air-context/ doc gains a `|' ancestor rail
    ;; at `     +-   [D]'), sized to EXACTLY the old indent so the
    ;; [R]/title/`~ date'/`#tag'/`. origin' V6 cluster + the right `|' pane
    ;; rail do NOT move.  jj diff --stat = 1 file — every layout board golden
    ;; + the STATE/TAG project goldens are byte-IDENTICAL (R24-3's batch
    ;; state cell still emits the `[R]' token, so no board/project churn).
    ;; `airctl status -Da' parity RE-VERIFIED on ~/Coding/github.com/withre/
    ;; air: the `│'/`├─'/`└─' rails reach the leaf doc rows there too; the V6
    ;; columns + per-dir counts are unchanged (org-air-r22-6-* + org-air-f5-*
    ;; stay green).  org-air-f5-project-view-byte-mockups now PASSES on the
    ;; regen'd golden, so the entry is deleted.  Round-24 manifest is EMPTY.
    ;; ===================================================================
    ;; v0.5 ROUND-25 CLOSEOUT (impl tips qmswyxso..soxomzwk + test re-bless
    ;; <this commit>).  ALL 7 grind entries CLOSED — the three project byte
    ;; goldens regenerated from impl's render via the FROZEN-CLOCK renderer
    ;; (make regen-mockups, anti-tautology guards active; verified NO HANG,
    ;; exit 0; jj diff --stat = ONLY the 3 project goldens — every board /
    ;; entry-view / denote golden is byte-identical, confirming R25-2/R25-4
    ;; are byte-invisible) and the assertion ERTs re-blessed to the
    ;; design-blessed R25 contracts (air/v0.5/org-air-round25-design.org):
    ;;   R25-6 (1) CLEAN rail dual-mode: `org-air-rail--reconcile' now DEFERS
    ;;     the window-mutating reconcile to a 0s timer (mutation never runs
    ;;     inside `window-configuration-change-hook').  org-air-r24-5-native-
    ;;     close-reconciles-to-inline drove the OLD synchronous flag-flip; it
    ;;     re-blesses to drive the DEFERRED body directly via `--reconcile-
    ;;     frame' (same close-to-inline outcome).  The 7 NEW R25-6 rail ERTs
    ;;     (no-double-rail-after-view-switch / side-rail-shows-current-view /
    ;;     toggle-idempotent-reversible / board-project-independent / close-
    ;;     reconciles-to-inline / no-orphan-when-navigating-away / refresh-
    ;;     never-strands) prove the single-owner invariant.
    ;;   R25-3 (4) DROP the phantom `review' state (Air has only draft|ready|
    ;;     work-in-progress|complete|dropped — RE-VERIFIED against airctl
    ;;     status -Da on withre/air, NO review state printed): the three
    ;;     project goldens' `▌ Summary' block drops the always-listed
    ;;     `0  Review' line (folded into f5-project-view-byte-mockups);
    ;;     org-air-r20-5-state-display-order-matches-airctl drops `review'
    ;;     from the order; org-air-r23-4-batch-state-cell-is-token-byte-stable
    ;;     drops the `[V]' review token.  NEW R25-3 ERTs (review-absent /
    ;;     five-real-states-ordered / display-order-minus-review / summary-
    ;;     has-no-review-row / orphan-review-face-gone) PASS.
    ;;   R25-1 (2) TREE ARM LENGTH: a directory-tree DOC row fills the gutter
    ;;     run AFTER the corner with `box-horizontal' (`-' batch / `─' GUI)
    ;;     instead of spaces, so the arm REACHES the badge (`  +-----[R]');
    ;;     gutter TOTAL width unchanged (V6-frozen).  The dir golden's
    ;;     doc-row gutters (`  +-    [R]' -> `  +-----[R]', nested
    ;;     `     +----[D]') re-blessed via regen (folded into f5-project-
    ;;     view-byte-mockups); org-air-r24-2-doc-row-carries-tree-rail
    ;;     re-blessed off the ` +'-spaces assertion onto the box-horizontal
    ;;     flush run.  NEW R25-1 ERTs (arm-reaches-the-badge / v6-columns-
    ;;     frozen depth 0+1 / corner-then-dash-run / nested-ancestor-rail-
    ;;     then-arm) PASS.
    ;;   R25-5 (3) DROP the meaningless origin/path column in the PROJECT
    ;;     view: `--fit-meta-widths' returns ocol 0 and `--insert-doc-row'
    ;;     passes no `:origin-text', so `--insert-row' omits the cell and the
    ;;     freed width flows to the title.  The three project goldens lose the
    ;;     `. v0.N/...' cell (regen, folded into f5-project-view-byte-
    ;;     mockups); org-air-f5-tree-structure + org-air-r21-5-doc-row-
    ;;     carries-doc-and-marker-across-the-row re-blessed off the relpath
    ;;     origin cell (org-air-doc/marker still span the row).  The BOARD is
    ;;     UNTOUCHED (its goldens byte-identical) and the relpath STAYS in the
    ;;     R24-6 filter search key.  NEW R25-5 ERTs (project-row-has-no-
    ;;     origin-cell / board-still-has-origin / relpath-still-filterable /
    ;;     title-reclaims-width) PASS.
    ;;   R25-2/R25-4 (byte-invisible): the bigger/bold svg badge LETTER and
    ;;     the DISTINCT Draft=D vs Dropped=X letter map are GUI svg :data /
    ;;     letter-map changes with stable batch `[R]'/`[D]'/`[X]' tokens, so
    ;;     no golden moved; covered by the NEW R25-2 (badge-draws-bold-letter
    ;;     / badge-width-pixel-locked / gui-chip-letters-distinct / batch-
    ;;     token-stable / board-pills-unaffected) and R25-4 (draft-not-
    ;;     dropped-both-layers / letters-airctl-aligned-distinct / fallback-
    ;;     never-collides-d-d / rollup-draft-dropped-distinct) ERTs.
    ;; No .el SOURCE touched (impl landed R25-1..R25-6 in qmswyxso..
    ;; soxomzwk).  Round-25 manifest is EMPTY; the tests stay as permanent
    ;; regression guards.
    ;; ===================================================================
    ;; v0.5 ROUND-26 impl grind (air/v0.5/org-air-round26-design.org).
    ;; R26-3 PROJECT LEGEND + RET: the project map rebinds RET/<mouse-1> to
    ;; the SAME-WINDOW `org-air-project-open' (the R26-5 session model; the
    ;; shared pane-return stays the BOARD's RET), adds s/d/t grouping +
    ;; `(' flip + `?' help keys, and rewrites the rail Actions legend to
    ;; the real project verbs (3 rows, table-driven).  The legend rows are
    ;; buffer text in the two-pane project goldens → those re-bless (test
    ;; seat; fixtures NOT edited on the impl track), and the legacy keymap
    ;; ERTs that assert the OLD RET-=-pane-return / s-d-t-unbound contract
    ;; retune on the test track:
    (org-air-f5-project-view-byte-mockups
     . "R26-3: inline rail Actions legend re-written (RET open/( flip//
filter · o sort/s\/d\/t group/| rail · g refresh/? help/q quit) — the three
project goldens' Actions rows await test-seat re-bless via regen-mockups.
R26-1: the dir golden's doc-row gutters additionally re-bless to the
one-space arm join (`+---- [R]', was `+-----[R]'; nested `+--- [D]').
R26-2: ALL THREE project goldens re-bless again — the state cell is the
uniform 5-col WORD token (READY/COMP/DRAFT/WIP/DROP/UNKNO, was
[R]/[C]/[D]/[W]/[X]/[U]) and every doc-row title/date/tag column shifts
right by exactly 2 (V6 relock at the wider cell)")
    (org-air-r24-2-doc-row-carries-tree-rail
     . "R26-1: the arm run stops one column short and a single SPACE joins
it to the badge — the R25 flush-run assertion retunes to corner + dashes
+ one space")
    (org-air-r25-1-arm-reaches-the-badge
     . "R26-1: deliberate inversion — the R25-1 'NO space sits between the
connector fill and [' contract becomes 'exactly ONE space' (user ask:
breathing room); retune to the R26-1 one-space contract")
    (org-air-r25-1-arm-reaches-the-badge-at-depth-2
     . "R26-1: same inversion at depth 2 — the nested arm is one shorter
and a single space joins it to the badge")
    (org-air-r25-1-nested-ancestor-rail-then-arm
     . "R26-1: the depth-1 arm ends in a space before [ — the flush-to-
badge tail assertion retunes (ancestor rail + corner rules unchanged)")
    (org-air-r18-dp3-project-keymap-inherits-core
     . "R26-3: project RET is `org-air-project-open' (same-window doc
open), no longer the inherited shared pane-return — assertion retunes")
    (org-air-r18-dp4-keymap-ret-and-sret-board-and-project
     . "R26-3: project RET -> org-air-project-open and `s' is the state
grouping key now (airctl -a parity) — assertions retune")
    (org-air-r20-5-project-keymap-shares-board-keys-no-shadow
     . "R26-3: RET forks deliberately (board pane-return vs project
same-window open) and s/d/t are project grouping keys again (on-key airctl
parity; the old 'moved to M-x' contract is superseded)")
    (org-air-r22-5-rail-toggle-bound-in-board-and-project
     . "R26-3: the both-maps RET-=-pane-return assertion retunes (project
RET is org-air-project-open; v/V/| stay shared)")
    (org-air-r24-4-click-shares-ret-resolver-on-dir-header
     . "R26-3: <mouse-1> follows RET onto org-air-project-open in the
project (click == RET still holds; the shared resolver claim retunes)")
    (org-air-r24-5-rail-blocks-shared-with-board
     . "R26-3 fit rule: a height-CLAMPED popped rail drops the Inspector
region when the side window is too short (inspector shrinks first so
Actions stays on-screen) — the all-blocks assertion needs a tall window")
    ;; R26-2 WORD PILLS (V6 RELOCK): the state cell widens 3->5 and the
    ;; byte token becomes the uniform padded WORD from the canonical
    ;; `org-air-project--state-words' map (DRAFT/READY/WIP/COMP/DROP;
    ;; unknown -> UNKNO).  Every legacy ERT that asserts the `[R]'-style
    ;; bracket token, the 3-col cell width, the 3*char-px badge image box,
    ;; the single-letter chip label, or a column position downstream of the
    ;; old cell retunes on the test track:
    (org-air-f5-tree-structure
     . "R26-2: row-shape assertions grep the `[R] Alpha feature' bracket
token and the `| [D] State N' section headings — both are the 5-col word
cells now (READY Alpha feature / | DRAFT Draft 2)")
    (org-air-r20-5-fix-directory-render-guards-divergence
     . "R26-2: the dir-render divergence guard compares against rows
carrying the old bracket tokens/columns; retunes to the word cells")
    (org-air-r21-2-project-motion-lands-past-state-cell
     . "R26-2: the motion landing column sits after the state cell, which
grew 3->5 — the expected-column assertion retunes (+2)")
    (org-air-r21-4-keyword-and-state-cells-keep-text-contract
     . "R26-2: the state-cell TRUE-text contract is the padded WORD token
now, not `[R]' — the text-contract assertion retunes")
    (org-air-r22-6-nesting-indents-deepen
     . "R26-2: picks the Gamma doc row by `\\[D\\]' — that token is DRAFT
now; the name-column metric itself is unchanged")
    (org-air-r23-4-batch-state-cell-is-token-byte-stable
     . "R26-2: asserts the per-state cell alist ((ready . '[R] ') ...) —
retunes to the padded word cells ((ready . 'READY ') ...)")
    (org-air-r23-4-emoji-rendered-on-gui-styles-honoured
     . "R26-2: the emoji style's non-GUI fallback assertions expect the
`[R]' token; the fallback is the 5-col word token now")
    (org-air-r24-2-depth-2-leaf-carries-two-ancestor-rails
     . "R26-2: locates the depth-2 leaf's badge by `[' — word cells have
no bracket; the rails/corner geometry itself is unchanged")
    (org-air-r24-2-v6-state-cell-column-locked
     . "R26-2: deliberate V6 RELOCK — the locked state-cell/title columns
move right by exactly 2 for the 5-col word cell; re-pin at the new lock")
    (org-air-r24-3-batch-state-cell-is-token-byte-guard
     . "R26-2: the batch byte-guard alist pins `[R] '-style cells; retunes
to the padded word cells")
    (org-air-r24-3-nerd-and-text-styles
     . "R26-2: the text/nerd style branches' token text is the word now,
not `[R]' — the true-text assertions retune")
    (org-air-r24-3-rails-stay-aligned-under-svg-default
     . "R26-2: rail alignment is asserted against columns downstream of
the 3-col cell; the +2 uniform shift retunes the expected columns")
    (org-air-r24-3-svg-badge-on-gui-is-cell-locked-image
     . "R26-2: the badge image box is 5*char-px now (was 3*char-px) and
the cell TRUE text is the word token — both assertions retune")
    (org-air-r25-1-v6-columns-frozen
     . "R26-2: deliberate relock — the R25-1 frozen column positions move
right by exactly 2 (wider state cell); re-freeze at the new positions")
    (org-air-r25-2-badge-draws-bold-letter
     . "R26-2: the chip label is the BARE WORD (>DRAFT<) now, not the
single letter >D< — the letter-glyph + bigger-than-token-scale assertions
retune (bold stays)")
    (org-air-r25-2-badge-width-pixel-locked
     . "R26-2: the badge image width is 5*char-px (uniform word capsule),
not 3*char-px — the pixel-lock assertion re-pins")
    (org-air-r25-2-batch-token-stable
     . "R26-2: the batch cell is `READY ' now, not `[R] ' — the byte-guard
retunes (still no display image off-GUI)")
    (org-air-r25-2-gui-chip-letters-distinct
     . "R26-2: draft/dropped chips draw the WORDS DRAFT/DROP (still never
alike); the >D</>X< letter assertions retune")
    (org-air-r25-2-svg-badge-keeps-r25-1-columns
     . "R26-2: the badge-on/off column-parity harness pins the R25-1
columns, which shift +2 under the 5-col cell — re-pin")
    (org-air-r25-4-draft-not-dropped-both-layers
     . "R26-2: the LETTER-layer assertions still hold (D vs X); the TOKEN
assertions retune from [D]/[X] to the DRAFT/DROP word cells — still never
equal")
    ;; R26-6 KILL THE "· r to file" ROW HINT: the dated-Inbox date-cell
    ;; nudge is deleted from `org-air-view--item-date-text' — rows reclaim
    ;; the 12 columns and the hinted row's tag/origin cells snap back into
    ;; V6 alignment; discovery lives in `?' help + Actions (r stays bound):
    (org-air-r17-denote-origin-byte-mockup
     . "R26-6: the two denote-origin goldens are the only ones carrying
the `· r to file' nudge — the dated inbox row loses it, its title
de-truncates and its tags/origin snap LEFT; goldens await test-seat
re-bless via regen-mockups")
    (org-air-triage-dated-inbox-row-carries-file-hint
     . "R26-6: deliberate inversion — the R19-2(c) carries-the-nudge
contract is retired (user: wasteful + cryptic); the assertion inverts to
NO row hint, with `?' help (`r refile') the single teaching surface")
    ;; (test-symbol . "reason")  — end of round-26 entries.
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
