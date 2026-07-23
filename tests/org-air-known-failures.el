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
    ;; ===================================================================
    ;; v0.5 ROUND-39 CLOSEOUT (impl tips pvvsknxmxqtl..snzpmtnrlzlx +
    ;; test re-bless <this commit>).  ALL R39 grind entries CLOSED.
    ;;   R39-1 SYMMETRIC BANNER GUTTER: the header now reserves a right
    ;;     gutter equal to the left indent (`org-air-view--banner-indent'
    ;;     = 2) so the right status ends banner-indent columns before the
    ;;     last usable column (lhs-margin == rhs-margin).  The 28 chrome
    ;;     byte goldens (layout-mockup-* tiers + board-only + empty +
    ;;     denote-origin-{80,120}) regenerated via `make regen-mockups'
    ;;     (FROZEN-CLOCK renderer, guards active; verified NO HANG, exit 0;
    ;;     jj diff = ONLY the banner line of each golden, right status
    ;;     -2 cols; every rule/rail/row/day-header line byte-identical).
    ;;     The end-column assertions that hardcoded the pre-R39 flush-to-
    ;;     W-1 contract (r29-1, r31-1 inline/sweep/side-window/glyphs,
    ;;     r33-1 empty/populated, r36-1 no-reserved-margin +
    ;;     reverting-the-fix, r37-1 fits-body-1 + reverting-to-r34, r38-1
    ;;     reverting-to-string-width, s7 status-ends) re-blessed on the
    ;;     test seat to the W-1-banner-indent (symmetric gutter) invariant.
    ;;     R38's pixel-true `left-cols' accounting stays intact (R39-1
    ;;     threads the indent constant through it); the R38 pixel/string
    ;;     fences stay green with the shifted content.
    ;;   R39-3 DROP `C-c C-a o' IN THE DOC BUFFER: the leader `o'
    ;;     (outline-goto-current-heading) duplicated RET, so it is dropped
    ;;     from `org-air-doc-leader-map' ONLY; the doc Actions legend
    ;;     `jump' cell drops with it (no dead/lying cell).  The legacy ERTs
    ;;     (r30-2-leader-reaches-actions-from-doc, r30-2-legend-shows-
    ;;     context-key, r28-3-legend-fits-narrow-rail) re-blessed to the
    ;;     R39-3 contract; the new `org-air-r39-3-doc-leader-no-open' ERT
    ;;     locks the drop (RET still opens; board/project `o' unchanged).
    ;;   R39-2 (byte-invisible) single fence column
    ;;     (`org-air-view--fence-column') and R39-4 (byte-invisible)
    ;;     repeatable leader p/n via `set-transient-map' are covered by the
    ;;     new executing ERTs in tests/org-air-round39-test.el; no golden
    ;;     moved for either.  No .el SOURCE touched (impl landed R39-1..
    ;;     R39-4 in pvvsknxmxqtl..snzpmtnrlzlx).  Round-39 manifest is
    ;;     EMPTY; the tests stay as permanent regression guards.
    ;; ===================================================================
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
    ;; (test-symbol . "reason")  — none right now.
    ;;
    ;; =================================================================
    ;; v0.5 ROUND-26 CLOSEOUT (impl tips wltlopmknwpt..ynmwluzvuomx +
    ;; test re-bless <this commit>).  ALL 34 grind entries CLOSED — the 5
    ;; moved byte goldens regenerated from impl's render via the
    ;; FROZEN-CLOCK renderer (make regen-mockups, guards active; verified
    ;; NO HANG, exit 0; jj diff --stat = ONLY the 3 project goldens + the
    ;; 2 denote-origin goldens — every board / entry-view golden is
    ;; byte-identical) and the legacy ERTs migrated to the design-blessed
    ;; R26 contracts (air/v0.5/org-air-round26-design.org):
    ;;   R26-2 (20 + 1 golden) WORD PILLS / V6 RELOCK: the state cell is
    ;;     the uniform padded 5-col WORD token (DRAFT/READY/WIP/COMP/DROP;
    ;;     unknown -> UNKNO) — token/byte guards, GUI capsule (5*char-px,
    ;;     bare-word bold label), column locks (badge column FROZEN at
    ;;     margin + 2*(1+depth); title/downstream +2) and render greps all
    ;;     re-pinned; rollup letters (D/X) unchanged.
    ;;   R26-1 (4) ONE-SPACE ARM: deliberate inversion of R25-1's flush
    ;;     run — corner + box-horizontal dashes + exactly ONE joining
    ;;     space before the badge; gutter width/corner/ancestor-rail
    ;;     rules unchanged.
    ;;   R26-3 (6 + the golden's Actions rows): project RET forks to the
    ;;     same-window `org-air-project-open' (R26-5 session; board RET
    ;;     stays pane-return; click == RET holds), s/d/t are on-key
    ;;     grouping verbs again (airctl parity), and the popped rail is
    ;;     height-clamped (Inspector shrinks first — the all-blocks
    ;;     assertion runs in a tall frame).
    ;;   R26-6 (2): the `· r to file' nudge is DEAD — the triage ERT
    ;;     inverts to NO row hint anywhere (discovery = `r' binding + `?'
    ;;     help), and the 2 denote-origin goldens re-blessed (title
    ;;     de-truncates, tags/origin snap left into V6).
    ;;   R26-8 (1): the r20-1 wedge ERT re-blessed to the CACHED/COLD
    ;;     dispatch — cold interactive returns with skeleton + loading t
    ;;     + queued machine and NO synchronous query in the call; the
    ;;     machine-START error keeps the R20-1 bounded-failure discipline;
    ;;     batch keeps the EXACT sync path (zero golden churn from R26-8).
    ;; airctl `status -Da' parity RE-VERIFIED on ~/Coding/github.com/
    ;; withre/air after the word-pill relock (counts + tree shape; the
    ;; wider state cell does not break column parity).  Round-26 manifest
    ;; is EMPTY; the tests stay as permanent regression guards.
    ;; =================================================================
    ;; v0.5 ROUND-30 CLOSEOUT (impl tips onmsukrsoztu..ynrluxnknmvt +
    ;; test re-bless <this commit>).  ALL 17 grind entries CLOSED.
    ;; Two design-blessed byte moves + the legacy origin-era ERTs:
    ;;   R30-1 (1) RAIL INSPECTOR IDENTITY BLOCK: the mid-rail inspector
    ;;     FULL-WRAPS the title (`org-air-inspector-max-title-lines' default
    ;;     nil = no cap, no more-glyph) and REORDERS the leading fields into
    ;;     a compact identity block (title / state / TAGS atop, THEN the
    ;;     breathing blank, THEN the metadata KV rows).  The height-50 two-
    ;;     pane goldens carry the inspector region, so `#inbox' (tags) and
    ;;     `▤ inbox.org' (origin KV) swap places — regenerated via make
    ;;     regen-mockups (frozen-clock guards).  org-air-layout-mockup-
    ;;     heights re-blesses on the regen'd fixtures.
    ;;   R30-3 (16) DASHBOARD COLUMN TOGGLES: `org-air-show-origin' now
    ;;     defaults nil — the DEFAULT board drops the filename column and
    ;;     the freed width flows to the flex title + tags (both de-truncate;
    ;;     dates stay).  BIG re-bless: 27 board byte goldens (layout-mockup
    ;;     70..160 + x24/x50 tiers + board-only) regenerated origin-less via
    ;;     make regen-mockups; jj-verified the origin cell is gone, the
    ;;     title/tags reflowed, and the V6 divider alignment holds.  The
    ;;     denote-origin-{80,120} goldens are the ORIGIN-ON goldens (their
    ;;     whole purpose is the long-Denote origin de-slug/cap), so the
    ;;     shared render helper `org-air-viewport-test-denote-board-lines'
    ;;     now binds `org-air-show-origin' t — both the golden and the byte
    ;;     test render origin-ON, byte-IDENTICAL to pre-R30 (no coverage
    ;;     lost).  The 10 legacy origin-era ASSERTION ERTs are HONESTLY
    ;;     re-scoped to bind `org-air-show-origin' t (the toggle-ON board,
    ;;     which reproduces the pre-R30 bytes their assertions were written
    ;;     for — NOT gutted): org-air-data-variation-titles-render (real
    ;;     generated-file origins), org-air-r10-item-row-right-cluster +
    ;;     org-air-v1b-inline-tag-placement (the [date][tags][▤origin]
    ;;     cluster order), org-air-r17-compute-meta-widths-title-budget +
    ;;     org-air-r17-long-denote-origin-keeps-title + org-air-r9-f1-
    ;;     denote-origin-rendered-and-truncates + org-air-v1b-origin-
    ;;     protected-on-overflow (origin de-slug/cap + title-protect fit),
    ;;     org-air-r20-6-meta-widths-cover-displayed-cells (origin-column
    ;;     coverage invariant), org-air-r25-5-board-still-has-origin (the
    ;;     toggle-ON board keeps its origin), org-air-v6-dates-align-in-
    ;;     column (date alignment on its original origin-ON layout — the
    ;;     default-board date alignment is now pinned by the regen'd byte
    ;;     goldens).  NEW R30 ERTs (R30-1 identity block; R30-2 leader;
    ;;     R30-3 toggles incl. default-hides-origin; R30-4 org-air-outline-
    ;;     mode; R30-5 doc-rail revert-guard) PASS on the .el source.
    ;; airctl `status -Da' parity RE-VERIFIED on ~/Coding/github.com/withre/
    ;; air (the board origin toggle is board-only; the project view already
    ;; dropped origin in R25-5, so tree shape + counts are UNCHANGED).
    ;; Round-30 manifest is EMPTY; the tests stay as permanent guards.
    ;; =================================================================
    ;; v0.5 ROUND-33 CLOSEOUT (impl tips tpylzztr..qtmsmtxooyls +
    ;; test re-bless <this commit>).  ALL 16 R33-1 grind entries CLOSED.
    ;;   R33-1 (16) AMBIGUOUS-WIDTH CHROME SEPARATOR: the chrome middle-dot
    ;;     separator swapped `·' (U+00B7 MIDDLE DOT, East-Asian AMBIGUOUS ->
    ;;     a GUI font may PAINT it two columns while `string-width' measures
    ;;     one, overflowing the right-filled header even at 0 items = Seam B)
    ;;     -> `∙' (U+2219 BULLET OPERATOR, Neutral, always painted one
    ;;     column) via the shared `org-air-chrome-separator' / `sep-dot'
    ;;     glyph.  `string-width' is IDENTICAL (both 1) so every column and
    ;;     the whole V6/R31 width math are byte-identical in COLUMNS; only
    ;;     the glyph BYTE changes.  The 31 chrome byte goldens regenerated
    ;;     via make regen-mockups (FROZEN-CLOCK renderer, guards active):
    ;;     jj-verified the ONLY fixture delta is 113 `·'->`∙' swaps (removed
    ;;     and added line sets are identical multisets after normalising the
    ;;     glyph — nothing else moved, columns unchanged).  The assertion
    ;;     ERTs that hardcoded the old U+00B7 (r22-4 Source line, r9-q1
    ;;     Source/Scope, r9-d5c + t3b + calendar-survives calendar `∙ created'
    ;;     legend key, r26-8 `stale ∙ refreshing' marker, s1 header, empty-
    ;;     board header) retargeted to U+2219 on the test seat.  No product
    ;;     code touched on the test track (impl landed R33-1 in nvyxvtvp).
    ;;   NEW R33 ERTs (round33-test.el): R33-1 empty-header fits the
    ;;     ambiguous-width-2 model + no East-Asian-Ambiguous glyph in swept
    ;;     chrome (the r31-1 single-width guard); R33-2 hover invariants
    ;;     (no help-echo, no track-mouse/<mouse-movement> hook, static
    ;;     background-only mouse-face, follow on point-move post-command,
    ;;     click/RET single-doc open) PASS on the .el source.
    ;; airctl `status -Da' parity RE-VERIFIED on ~/Coding/github.com/withre/
    ;; air (airctl output itself is unchanged — the chrome glyph swap is
    ;; org-air-side and preserves string-width, so no column shifted).
    ;; Round-33 manifest is EMPTY; the tests stay as permanent guards.
    ;; =================================================================
    ;; v0.5 ROUND-36 CLOSEOUT (impl tip pxmorlro + test re-bless <this
    ;; commit>).  ALL 11 grind entries CLOSED.
    ;;   R36-1 HEADER TRAILING-WHITESPACE OVERFLOW: the board header AND
    ;;     the loading-skeleton header shared `org-air-view--insert-banner',
    ;;     which right-aligned the status to column W-2 and then APPENDED a
    ;;     reserved one-column right margin (the S7 mirror of the visible
    ;;     left margin) via `(org-air-view--justify left (concat right " ")
    ;;     w)' with `budget = (- w (string-width left) 2 1)', so the
    ;;     composed line filled `window-body-width' EXACTLY and its LAST
    ;;     column was a blank — the column drawn OFF the right edge on the
    ;;     user's fringed frame ("trailing empty space that's OFF the
    ;;     window", the `↦' arrow).  The impl dropped the appended " " and
    ;;     the budget's reserved `1' (compose `(org-air-view--justify left
    ;;     right w)', `budget = (- w (string-width left) 2)') so the
    ;;     status' last glyph sits on column W-1 (the true last usable
    ;;     column) with NO trailing pad; the S7 spare column is now
    ;;     supplied UPSTREAM by R34's fringe-aware usable-columns (guard
    ;;     preserved verbatim).  Byte-VISIBLE header re-baseline: the 28
    ;;     chrome byte goldens (layout-mockup 70..160 + x24/x50 tiers +
    ;;     board-only + empty-120x50, denote-origin-{80,120}) regenerated
    ;;     via `make regen-mockups' (FROZEN-CLOCK renderer, guards active;
    ;;     jj diff --stat = ONLY the header line of each golden — every
    ;;     row/rule/rail/day-header/pane line is byte-identical; the
    ;;     project goldens are byte-identical too).  The status shifts one
    ;;     column right (its last glyph lands on the final column instead
    ;;     of the penultimate one).  The legacy S7/R29/R31 assertions that
    ;;     hardcoded the reserved-blank contract (right-trimmed width =
    ;;     W-1, blank final column) re-blessed on the test seat to the
    ;;     no-trailing-pad invariant (trimmed width == compose width, raw
    ;;     == compose): org-air-s7-header-status-ends-at-w-minus-1,
    ;;     org-air-r29-1-header-ends-at-contract-column, org-air-r31-1-
    ;;     inline-header-ends-at-contract-column, org-air-r31-1-side-
    ;;     window-and-board-only-still-fit.  R34's usable-columns guard
    ;;     (org-air-r34-*) stays GREEN unchanged.
    ;;   NEW R36 DECISIVE ERT (round36-test.el): the headless composed-line
    ;;     guard for BOTH the BOARD header AND the LOADING-SKELETON header,
    ;;     at several window-body-widths (incl. the user's 191): asserts
    ;;     (length <= W), (string-width <= W), NO trailing whitespace past
    ;;     the right content, the left margin STILL present inside W, and
    ;;     the right content right-aligned FLUSH to the last usable column.
    ;;     Reverting the fix (re-adding the reserved trailing margin col)
    ;;     -> the guard FAILS (asserted in-process by re-composing the
    ;;     old-contract line).  No .el SOURCE touched (impl landed R36-1 in
    ;;     pxmorlro).  Round-36 manifest is EMPTY; the tests stay as
    ;;     permanent regression guards.
    ;; =================================================================
    ;; v0.5 ROUND-40 CLOSEOUT (impl tips 60dd39c4..1a1a4484 + test re-bless
    ;; <this commit>).  ALL grind entries CLOSED — the moved rule goldens
    ;; regenerated from impl's render via the FROZEN-CLOCK renderer (make
    ;; regen-mockups, guards active; verified NO HANG, exit 0) and the
    ;; legacy r36-1 rule assertion retuned to the design-blessed R40
    ;; contracts (air/v0.5/org-air-round40-design.org):
    ;;   R40-1 (28 goldens) HEADER SEPARATOR RULE SYMMETRY: the header rule
    ;;     (`org-air-view--insert-rule') now reserves a RIGHT gutter equal
    ;;     to its left margin (`org-air-view--banner-indent' = `org-air-
    ;;     margin' = 2) instead of filling flush to W, so the `─' rule spans
    ;;     EXACTLY the R39-1 symmetric banner content columns (indent ..
    ;;     usable-indent).  make regen-mockups re-blessed the 28 chrome byte
    ;;     goldens (layout-mockup 70..160 + x24/x50 tiers + board-only +
    ;;     empty-120x50, denote-origin-{80,120}); jj-verified the ONLY
    ;;     fixture delta is the `─' rule shrinking by EXACTLY `banner-indent'
    ;;     (2) glyphs on the RIGHT at every width (31 rule lines total incl.
    ;;     the W90 calendar-band separator) — every row/header/rail/day-
    ;;     header/pane line is byte-identical (each changed line is a pure
    ;;     spaces+`─' rule; lead stays 2, run = width - 2*margin).  The
    ;;     legacy org-air-r36-1-right-padded-chrome-lines-do-not-overshoot
    ;;     (which hardcoded the pre-R40 flush-to-W contract `(= (string-width
    ;;     rule) width)') retuned on the test seat to the symmetric contract
    ;;     (right gutter == left margin == banner-indent; run == width -
    ;;     2*margin; no overshoot) — now PASSES, deleted from the manifest.
    ;;   R40-2 (byte-invisible) NO-RAIL FENCE CONTINUITY: `org-air-view--
    ;;     insert-row' now right-anchors the standard board row to the ONE
    ;;     shared board-wide `org-air-view--fence-column' (no-arg form,
    ;;     derived from `meta-cluster-width') rather than the per-row
    ;;     CLUSTER-W, so the fence is CONTINUOUS BY CONSTRUCTION; every board
    ;;     row already anchored to the shared column on the fixtures, so no
    ;;     golden moved.
    ;; NEW R40 EXECUTING ERTs (tests/org-air-round40-test.el, revert-fails):
    ;;   org-air-r40-1-rule-margins-symmetric — the rule's first/last non-
    ;;     space cols == the banner content's (lhs-margin == rhs-margin ==
    ;;     banner-indent); reverting to a flush-right rule FAILS.
    ;;   org-air-r40-2-fence-continuous-under-divergent-cluster — the fence
    ;;     column is IDENTICAL on every board row (incl. blank/fill/separator
    ;;     rows) even with DIVERGENT per-row cluster widths, and equals the
    ;;     shared no-arg `org-air-view--fence-column'; reverting to per-row
    ;;     CLUSTER-W anchoring FAILS (fence varies row-to-row).
    ;; airctl `status -Da' parity RE-VERIFIED (the rule symmetry is org-air-
    ;; side chrome and preserves every column; the fence hardening is byte-
    ;; identical on the current data).  No .el SOURCE touched (impl landed
    ;; R40-1/R40-2 in 60dd39c4..1a1a4484).  Round-40 manifest is EMPTY; the
    ;; tests stay as permanent regression guards.
    ;; =================================================================
    ;; v0.5 ROUND-41 CLOSEOUT (impl tip rxlsswny + test re-bless <this
    ;;   commit>).  ALL 9 grind entries CLOSED — the moved rule goldens
    ;;   regenerated from impl's render via the FROZEN-CLOCK renderer (make
    ;;   regen-mockups, guards active; verified NO HANG, exit 0) and the
    ;;   legacy R40/R36 rule assertions retuned to the design-blessed R41
    ;;   contract (air/v0.5/org-air-round41-design.org):
    ;;   R41-1 (28 goldens) FULL-WIDTH HEADER SEPARATOR RULE: the header rule
    ;;     (`org-air-view--insert-rule') now spans the FULL usable width
    ;;     (`0' .. `usable', flush to BOTH text-area edges) instead of the
    ;;     R40-1 inset span (`banner-indent' .. `usable - banner-indent'), so
    ;;     the `─' rule is `banner-indent' (2) glyphs WIDER on the LEFT (no
    ;;     leading margin) and 2 wider on the RIGHT than R40 — a full-bleed
    ;;     rule under the still-inset R39-1 banner content (the nano-emacs
    ;;     look).  make regen-mockups re-blessed the 28 chrome byte goldens
    ;;     (layout-mockup 70..160 + x24/x50 tiers + board-only + empty-120x50,
    ;;     denote-origin-{80,120}); jj-verified the ONLY fixture delta is the
    ;;     `─' rule GAINING exactly `banner-indent' (2) glyphs on the LEFT
    ;;     (lead 2 → 0) and 2 on the RIGHT at every width (31 rule lines total
    ;;     incl. the W90 calendar-band separator) — every row/header/rail/day-
    ;;     header/pane line is byte-identical (each changed line is a pure
    ;;     `─' rule now flush both edges: lead 0, run == usable, no overshoot).
    ;;     The legacy org-air-r40-1-rule-margins-symmetric retuned on the test
    ;;     seat to the full-width contract (rule first-non-space col == 0 AND
    ;;     last-non-space col == usable-1 AND rule-width - banner-content-width
    ;;     == 2*banner-indent; reverting to the R40 inset FAILS), and org-air-
    ;;     r36-1-right-padded-chrome-lines-do-not-overshoot retuned from the
    ;;     R40 symmetric-inset (right gutter == margin) to the R41 flush-both-
    ;;     edges (rulew == width, no leading margin, no right gutter, no
    ;;     overshoot) — both now PASS, deleted from the manifest.  The banner
    ;;     CONTENT (R39-1) is byte-identical; the rule NEVER exceeds usable
    ;;     (ends at the last usable column, R37 body-1 safety margin honoured).
    ;; airctl `status -Da' parity RE-VERIFIED (the rule full-width is org-air-
    ;; side chrome and preserves every column; no data column shifted).  No
    ;; .el SOURCE touched (impl landed R41-1 in rxlsswny).  Round-41 manifest
    ;; is EMPTY; the tests stay as permanent regression guards.
    ;; =================================================================
    ;; v0.5 ROUND-46 CLOSEOUT (impl tip yswxwpwl + test re-bless <this
    ;; commit>).  The single grind entry CLOSED — nothing left open.
    ;;   R46-2 UNIVERSAL PER-ROW TITLE-BAND CLAMP (byte-invisible; make
    ;;     regen-mockups verified ZERO churn — the clamp is point-only):
    ;;     the R29-2 item/doc-only, LEFT-only dead-zone snap
    ;;     (`org-air-view--dead-zone-p') is replaced by
    ;;     `org-air-view--row-band' + the two-edged clamp in
    ;;     `--normalize-point-now' behind the unchanged R29-2 line-motion
    ;;     gate, so EVERY visible board row — section headers, the
    ;;     `…and N more' / `Nothing scheduled …' notes, the banner, the
    ;;     item-row RIGHT edges — clamps a stray col-0 / EOL landing into
    ;;     its title band while an in-band goal column is KEPT.
    ;;   The legacy org-air-r22-2-normalize-point-snaps-off-dead-column
    ;;     clause (3) asserted the PRE-R46 contract ("the banner top is
    ;;     never touched", point held at point-min col 0) — the
    ;;     design-blessed inversion this round.  Re-blessed on the test
    ;;     seat: the banner top now clamps onto the banner's first visible
    ;;     glyph (`org-air-view--beginning-of-visible', col 2) and the
    ;;     clamp is idempotent there; clauses (1)/(2) unchanged.  Now
    ;;     PASSES — entry deleted.
    ;; NEW R46 EXECUTING ERTs (tests/org-air-round46-test.el, each
    ;; REVERT-FAILS — verified against the pre-impl trunk, where both die
    ;; on the diagnosed shape `line 9 lands col 0 OUTSIDE visible band
    ;; [2..21]: "  ! Needs attention 17"'):
    ;;   org-air-r46-2-native-vertical-nav-stays-in-band — NATIVE
    ;;     next-line/previous-line down AND up the live fixture board
    ;;     (board-only + side-window popped + the EMPTY board's `Nothing
    ;;     scheduled …' notes) through the real command loop with hostile
    ;;     goal columns (col 0 — the batch-deterministic visual pixel goal
    ;;     — and EOL via the logical goal path); every non-blank landing
    ;;     inside its row's visible band, in-band goal respected EXACTLY.
    ;;   org-air-r46-2-evil-vertical-nav-stays-in-band — REAL evil from
    ;;     .deps (motion state): evil-next-line/evil-previous-line (vanilla
    ;;     evil's j/k) over the same walks.
    ;; airctl `status -Da' parity unaffected (point-only; zero fixture
    ;; churn, project goldens byte-identical).  Round-46 manifest is EMPTY;
    ;; the tests stay as permanent regression guards.
    ;; =================================================================
    ;; v0.5 ROUND-47 CLOSEOUT (impl tip wwxwvynl + test re-bless <this
    ;; commit>).  ALL 3 grind entries CLOSED — byte goldens byte-IDENTICAL
    ;; (make regen-mockups verified ZERO churn: `mouse-face' is a text
    ;; property invisible to `buffer-substring-no-properties' captures),
    ;; so the re-bless is assertion-only:
    ;;   R47-2 HOVER = TEXT-ONLY TITLE BAND: `org-air-view--insert-row'
    ;;     pops `mouse-face' out of the whole-extent PROPS and applies it
    ;;     ONLY over [title-start, cluster-start) — so NO buffer position
    ;;     carries both `mouse-face' and an image `display', and Emacs
    ;;     30's DRAW_MOUSE_FACE SVG re-lookup (xdisp.c e69fafdb/bug#67794;
    ;;     C image cache keyed on face fg/bg/font → hover-face MISS →
    ;;     synchronous librsvg re-raster per crossing) can never fire.
    ;;     org-air-r32-1-doc-row-mouse-face-own-span +
    ;;     org-air-r32-1-adjacent-rows-independent re-blessed from the
    ;;     BOL..newline run edges to title-start..cluster-start (run
    ;;     starts AT the `org-air-row-title' mark, BOL/tree-gutter/state
    ;;     badge carry NO mouse-face; the no-fusion / no-newline-span /
    ;;     distinct-run assertions carry over verbatim).  Verified both
    ;;     REVERT-FAIL on the pre-impl trunk (whole-row spans from BOL).
    ;;   R47-2 second seam — the svg TODAY cell:
    ;;     org-air-r6-calendar-cells-clickable re-blessed to the R47
    ;;     contract — the svg-backed TODAY cell is clickable via keymap +
    ;;     `org-air-day' with NO `mouse-face' over its image `display';
    ;;     plain-text day cells keep hover highlight + keymap.  Verified
    ;;     REVERT-FAILS on the pre-impl trunk (today cell carried both).
    ;; NEW R47 EXECUTING ERTs (tests/org-air-round47-test.el; the impl
    ;; head-start draft was validated and REWORKED — its row scanner
    ;; looped forever and its commentary pinned the WRONG mechanism):
    ;;   org-air-r47-1-no-image-under-mouse-face-{board,project} — ZERO
    ;;     positions carry both `mouse-face' + image `display' (incl. the
    ;;     calendar band / svg TODAY cell); trunk FAILS (measured on the
    ;;     pre-impl trunk: board 139 overlapping svg-glyph runs on the
    ;;     fully-EXPANDED 120x80 fixture render incl. the today cell — the
    ;;     design's 95 was the unexpanded board — project 17, exactly the
    ;;     design's count).
    ;;   org-air-r47-2-hover-run-is-single-title-band — every item/doc
    ;;     row: EXACTLY ONE maximal run, starting at the title mark, no
    ;;     newline, text-only, ending before the first cluster pill;
    ;;     anti-tautology >20 board hover rows; trunk FAILS.
    ;;   org-air-r47-3-zero-pill-builds-per-hover-sweep — counters on
    ;;     `org-air-view--svg-image-cached' (build) + `svg-image': a
    ;;     simulated sweep across every run = 0 builds, 0 svg-image calls
    ;;     (elisp layer, R33-2) AND 0 svg glyphs inside any resolved run
    ;;     (the Emacs 30 C-path guard); trunk FAILS on the third conjunct.
    ;;   org-air-r47-4-no-inspector-or-outline-recompute-per-crossing —
    ;;     inspector/outline counters stay 0 across the sweep, no debounce
    ;;     timer armed, followers wired ONLY on pre/post-command hooks
    ;;     (extends the R33-2 lock; already-true by design).
    ;; make regen-mockups: NO HANG, exit 0, jj diff — ZERO fixture churn
    ;; (every board/project/entry/denote golden byte-identical).  airctl
    ;; `status -Da' parity unaffected (hover is a text-property span;
    ;; project tree shape / counts / columns unchanged — org-air-r22-6-*
    ;; + org-air-f5-* stay green).  No .el SOURCE touched (impl landed
    ;; R47-2 in wwxwvynl).  Round-47 manifest is EMPTY; the tests stay as
    ;; permanent regression guards.
    ;; =================================================================
    ;; v0.5 ROUND-48 CLOSEOUT (impl tip xprqntst/785bfa4d + test re-bless
    ;; <this commit>).  ALL 3 grind entries CLOSED — the three project
    ;; byte goldens regenerated from impl's render via the FROZEN-CLOCK
    ;; renderer (make regen-mockups AFTER make clean — stale pre-impl
    ;; .elc first shadowed the R48 .el and regenerated a byte-identical
    ;; no-op; verified NO HANG, exit 0; jj diff --stat = ONLY the 3
    ;; project goldens, 5 changed lines total — every board / layout /
    ;; entry-view / denote golden is byte-identical, confirming R48-2 is
    ;; a face, byte-invisible) and the two flagged assertion ERTs
    ;; re-blessed HONESTLY to the design-blessed R48 contracts
    ;; (air/v0.5/org-air-round48-design.org):
    ;;   R48-3 (1 golden set) DROPPED-DOC FOLD: project-view-dir.txt —
    ;;     the `+---- DROP Delta UI exploration' row is replaced by the
    ;;     `+---- … 1 dropped — TAB to show' fold row as the LAST
    ;;     own-doc slot of v0.2/ (after UNKNO Eta; connector math holds;
    ;;     the header's X1 rollup is UNCHANGED); project-view-state.txt —
    ;;     the `Dropped 1' heading stays, its body becomes the fold row;
    ;;     project-view-tag.txt — #ui loses the Delta row, gains the
    ;;     fold row after its visible rows (`| #ui 4' count unchanged).
    ;;   org-air-f5-tree-structure re-blessed STRONG (not gutted): the
    ;;     `every fixture doc renders by TITLE' conjunct splits — every
    ;;     NON-dropped doc renders by title, the dropped Delta title is
    ;;     ABSENT behind the `… 1 dropped' fold row, AND a knob-nil
    ;;     re-render brings EVERY doc (dropped included) back by title,
    ;;     so a render that simply lost the doc can never pass.
    ;;   org-air-r18-dp3-project-filter-narrows re-blessed: the NO-FILTER
    ;;     baseline + clear-restores conjuncts assert every NON-dropped
    ;;     title + the fold row (Delta ABSENT); the filter-LIVE conjuncts
    ;;     (Delta VISIBLE under #ui — the R48-3 fold bypass) carry over
    ;;     VERBATIM and now double as the bypass guard.
    ;; NEW R48 EXECUTING ERTs (tests/org-air-round48-test.el; revert of
    ;; the collapse/grey FAILS r48-1/-2/-3/-4/-6):
    ;;   r48-1 folded-by-default (dir grouping: no visible dropped doc,
    ;;     ONE fold row keyed (directory . "v0.2"), X1 rollup unmoved,
    ;;     fold row carries NO org-air-doc + text-only mouse-face);
    ;;   r48-2 fold rows per group in ALL groupings (state: heading +
    ;;     count stay, body = the fold row, key (state . "dropped");
    ;;     tag: exactly one fold row keyed (tag . "#ui"));
    ;;   r48-3 TAB toggle round-trip (expand from the fold row: Delta
    ;;     revealed GREYED `org-air-face-project-dropped' in its
    ;;     state-first slot, visible rows +EXACTLY the hidden count —
    ;;     anti-tautology — point on the revealed title, no residual
    ;;     fold row; rule-2 re-collapse from the dropped row: point back
    ;;     on the fold row, `--expanded-dropped' round-trips empty; the
    ;;     face DEFINITION asserted: inherits faded + :strike-through t,
    ;;     DISTINCT from the badge-only air-state-dropped);
    ;;   r48-4 knob nil = inline greyed in TODAY's byte position, no
    ;;     fold row in ANY grouping (R48-2 independent of the fold);
    ;;   r48-5 LOCK-style numeric fence (dir X1 rollup / `Dropped 1'
    ;;     heading / `#ui 4' / rail Summary `1 Dropped' all count the
    ;;     folded doc, pinned against `--collect-docs' ground truth —
    ;;     the airctl -Da parity surface);
    ;;   r48-6 live-filter bypass (`delta' token: the match VISIBLE +
    ;;     greyed, NO fold row; clearing restores the fold);
    ;;   r48-7 LOCK-style scope fence (board render carries no
    ;;     `org-air-dropped-fold', board TAB stays org-air-toggle-section
    ;;     while project TAB is the R48 toggle, and the expansion
    ;;     survives refresh — the R26-5 session locals).
    ;; No .el SOURCE touched on the test track (impl landed R48-2/R48-3
    ;; in xprqntst).  Round-48 manifest is EMPTY; the tests stay as
    ;; permanent regression guards.
    ;; =================================================================
    ;; v0.5 ROUND-49 CLOSEOUT (impl tip ooxvoxlv/bc4fd92e + test additions
    ;; <this commit>).  ZERO grind entries were ever opened — `make clean
    ;; && make regen-mockups' verified ZERO fixture churn (NO HANG, exit
    ;; 0, jj: working copy unchanged): the R49-2/R49-3 placement seeds are
    ;; interactive-only (batch normalises the `unset' sentinel to nil
    ;; exactly as before, so batch renders never consult placement) and
    ;; the R49-4 max(doc-h,window)->window rail-foot change is inert at
    ;; fixture sizes — every board/layout/project/entry/denote golden is
    ;; byte-identical.  ONE legacy assertion re-blessed honestly:
    ;;   org-air-r26-5-placement-default-pops-project-rail — its "fresh
    ;;     board stays inline" half pinned the OLD R26-5 asymmetric
    ;;     default, the design-blessed R49-3 inversion (and had gone
    ;;     VACUOUS anyway: the R26-8 cold interactive entry paints the
    ;;     skeleton and defers the full render, so the placement seed —
    ;;     which lives in `org-air-view--render' — never ran in that
    ;;     conjunct).  Re-scoped to pin the LEGACY back-compat surface
    ;;     explicitly instead: under the old R26-5 alist shape
    ;;     '((board . inline) (project . side-window)) the board still
    ;;     seeds inline (zero-migration `consp' branch); the project-pops
    ;;     + owner + no-inline-text conjuncts survive verbatim.  The NEW
    ;;     consistent default is pinned by org-air-r49-2 (below).
    ;; NEW R49 EXECUTING ERTs (tests/org-air-round49-test.el;
    ;; r49-1/-2/-3/-4 verified FAIL on the pre-impl trunk, r49-5/-6 are
    ;; both-sides locks):
    ;;   r49-1 resolver table — shipped defaults CONSISTENT (the ONE
    ;;     shared `org-air-rail-placement' = side-window; every per-view
    ;;     override nil-inherit), override wins for THAT view only, nil
    ;;     inherits a flipped shared value, the LEGACY alist bound to the
    ;;     shared knob resolves per view (unnamed views fall back to
    ;;     side-window), and the override beats even the alist.  Trunk
    ;;     FAILS (no resolver, no per-view defcustoms).
    ;;   r49-2 consistent default SEEDS both — a fresh interactive BOARD
    ;;     (rendered with the `unset' sentinel intact so the seed — not a
    ;;     pre-cooked flag — is what runs) and a fresh interactive PROJECT
    ;;     both pop the side rail: flag t, live owned `*org-air-rail*'
    ;;     side window, NO inline rail text.  Trunk FAILS on the board
    ;;     half (the alist seeded the board inline).
    ;;   r49-3 per-view override SPLITS — board-inline override + shared
    ;;     side-window: board renders the inline two-pane rail while a
    ;;     fresh project still pops; the MIRRORED project-inline split
    ;;     holds; shared flipped to inline with no overrides seeds BOTH
    ;;     views inline.  Trunk FAILS (overrides unknown; a symbol-valued
    ;;     shared knob breaks the alist-get seed).
    ;;   r49-4 inline one-windowful — a synthetic 40-doc project (doc
    ;;     pane TALLER than the pinned height-30 window; placement pinned
    ;;     inline BOTH ways — R49-2 override AND legacy alist — so the
    ;;     trunk comparison reaches the legend conjunct): the rail's
    ;;     Actions header lands at line <= 30 (the FIRST windowful,
    ;;     visible on open) with the calendar above it, and the divider
    ;;     column is intact at ONE column on all 40 doc rows (the padded
    ;;     blank rail cells keep the fence past the rail foot).  Trunk
    ;;     FAILS at exactly `(<= 41 30)' — Actions pinned to the doc-h
    ;;     foot, the user's report verbatim.
    ;;   r49-5 LOCK batch placement-blind — a `noninteractive' render
    ;;     with the shared default side-window is byte-identical to the
    ;;     120-col golden and creates no side window (the byte-golden
    ;;     freeze by construction; passes on both sides).
    ;;   r49-6 the `|' toggle + R25-6 reconciler — from the popped R49-3
    ;;     default: `|' to inline, `|' back out, then a NATIVE close +
    ;;     `org-air-rail--reconcile-frame' falls back inline with the
    ;;     flag cleared (placement consulted ONCE; the toggle/reconciler
    ;;     ownership model untouched).
    ;; airctl `status -Da' parity RE-VERIFIED on ~/Coding/github.com/
    ;; withre/air (placement is a seed/window concern — tree shape,
    ;; per-dir counts and the v0.1/ own R4 C14 X1 D2 + desc +1 +14 +9 +8
    ;; roll-ups unchanged; org-air-r22-6-* + org-air-f5-* stay green).
    ;; No .el SOURCE touched on the test track (impl landed R49-2/3/4 in
    ;; ooxvoxlv/bc4fd92e).  Round-49 manifest is EMPTY; the tests stay as
    ;; permanent regression guards.
    ;; =================================================================
    ;; v0.5 ROUND-50 impl grind (air/v0.5/org-air-round50-design.org).
    ;; R50-1 LEGEND KEY TRUTH: the board Actions legend stops hardcoding
    ;; key strings — every cell derives from the LIVE binding in the BOARD
    ;; buffer via `org-air-view--legend-key' (where-is), so the refresh
    ;; cell shows the TRUE sequence `g r' (`g' alone is the B4 prefix
    ;; map), and column widths follow the derived strings (`g r' widens
    ;; column 1 by 2).  The failed-marker/echo strings become `(g r
    ;; retries)'; the off-by-default footer band becomes `[g r] refresh'.
    ;; R50-2: `?' help is a proper grouped/scrollable `*org-air-help*'
    ;; buffer (org-air-help-mode, special-mode child; keys derived from
    ;; the origin buffer's ACTUAL keymaps; q quits back) — the one-line
    ;; echo-area `message' is DELETED.  The keymaps themselves are
    ;; UNTOUCHED (`g' stays the prefix, `g r' stays refresh).
    ;; Byte goldens: the 20 board-legend fixtures (layout-mockup-
    ;; {96,100,104,110,120,160} × {-,x24,x50} + empty-120x50 +
    ;; denote-origin-120) move by construction — the Actions row widens
    ;; `g' -> `g r'.  Intended re-bless via `make regen-mockups' on the
    ;; TEST seat (fixtures NOT edited on the impl track); the narrow
    ;; mockups (70/80/90) + denote-origin-80 carry no rail legend and are
    ;; byte-identical, and the three project-view-*.txt goldens are
    ;; byte-identical (the project's bare `g' IS a direct refresh — its
    ;; legend was already true; any project fixture movement is a
    ;; regression, not a re-bless).
    ;;
    ;; =================================================================
    ;; v0.5 ROUND-50 CLOSEOUT (impl tip ptrqsmxv/2c8d6741 + test re-bless
    ;; <this commit>).  ALL 10 grind entries CLOSED — the 20 moved
    ;; board-legend byte goldens regenerated from impl's render via the
    ;; FROZEN-CLOCK renderer (make clean && make regen-mockups, anti-
    ;; tautology guards active; verified NO HANG, exit 0; jj diff --stat
    ;; = EXACTLY the 20 predicted fixtures: layout-mockup-
    ;; {96,100,104,110,120,160} × {-,x24,x50} + empty-120x50 +
    ;; denote-origin-120, the 2 Actions legend lines each — `g refresh'
    ;; -> `g r refresh' with the derived column-1 field widening; the
    ;; narrow 70/80/90 mockups + denote-origin-80 carry no rail legend
    ;; and are byte-identical, and the three project-view-*.txt goldens
    ;; are byte-identical, the project's bare `g' being a DIRECT refresh
    ;; — its legend was already true).  The flagged assertion ERTs
    ;; re-blessed HONESTLY to the design-blessed R50 contracts
    ;; (air/v0.5/org-air-round50-design.org) — each asserts the NEW true
    ;; behaviour, none weakened:
    ;;   org-air-b4-rail-hint-shows-g-prefix — the byte-pin RE-REVERSED
    ;;     to `g r refresh' with the R8 `gr' -> D5f `g' -> R50 `g r'
    ;;     history documented in the docstring (never silent); asserts
    ;;     BOTH older cell generations absent.
    ;;   org-air-r26-8-failure-honest-and-g-retries — the failed header
    ;;     marker pins `refresh failed (g r retries)' AND the old
    ;;     `(g retries)' ABSENT; the byte-intact-body / retry /
    ;;     completion machine conjuncts carry over verbatim.
    ;;   org-air-r26-6-refile-still-works + org-air-triage-dated-inbox-
    ;;     row-carries-file-hint — the echo `message' captures retuned
    ;;     to read the rendered *org-air-help* BUFFER (the one-liner is
    ;;     deleted); the refile discovery guarantee survives, relocated
    ;;     (the `^  r +refile' row, key derived from the live board map).
    ;;   org-air-r26-3-legend-truth-table-driven — gains the R50-1
    ;;     not-a-prefix conjunct (every legend key's `key-binding' must
    ;;     be a command and NOT `keymapp'); the table itself unchanged.
    ;; NEW R50 EXECUTING ERTs (tests/org-air-round50-test.el; reverting
    ;; the derived legend / the help buffer FAILS each):
    ;;   r50-1-board-legend-shows-g-r — live render shows `g r refresh',
    ;;     neither `\_<g refresh' nor `gr refresh' anywhere;
    ;;   r50-1-legend-keys-are-commands-not-prefixes — all 6 parsed
    ;;     board legend cells resolve via `key-binding' to commands,
    ;;     none `keymapp', none the bare `g' — while bare `g' IS a
    ;;     prefix map in the same buffer (the mislabel class is real
    ;;     and would be caught);
    ;;   r50-1-legend-follows-rebinding — `g r' removed + F5 added ->
    ;;     a re-render shows `<f5> refresh' (a hardcoded string cannot
    ;;     follow);
    ;;   r50-2-help-opens-buffer-from-board — `?' dispatch pops a LIVE
    ;;     displayed *org-air-help* buffer (org-air-help-mode <
    ;;     special-mode, read-only) with `g r refresh' + `r refile'
    ;;     rows — NOT an echo-area line;
    ;;   r50-2-help-context-aware — project help lists open/flip/group
    ;;     + refresh at the DIRECT bare `g' (never `g r'); doc-session
    ;;     help lists the `C-c C-q' back verb + the `C-c C-a' leader
    ;;     forms, and a customized leader (`C-c C-z') shows the NEW
    ;;     prefix (rows derive from the live maps);
    ;;   r50-2-help-q-quits-back — `q' = `quit-window' (the special-mode
    ;;     PARENT binding), origin window restored;
    ;;   r50-2-help-knob-gated — knob nil: `?' gone from the board /
    ;;     project / doc-leader maps, yet M-x org-air-help still renders
    ;;     the buffer with honest `M-x command-name' cells and a working
    ;;     parent `q'.
    ;; No .el SOURCE touched on the test track (impl landed R50-1/R50-2
    ;; in ptrqsmxv).  Round-50 manifest is EMPTY; the tests stay as
    ;; permanent regression guards.
    ;; =================================================================
    ;; v0.5 ROUND-51 impl grind (air/v0.5/org-air-round51-design.org).
    ;; R51-1 DE-STRIKE dropped everywhere: BOTH dropped faces
    ;; (`org-air-face-project-dropped' row face + the badge-only
    ;; `org-air-face-air-state-dropped') lose `:strike-through' — applied
    ;; over the whole row extent the strike drew a full-width RULE
    ;; through the inter-column fill; grey (`org-air-face-faded' inherit)
    ;; is now the SOLE dropped affordance.  R51-2 dropped sorts to the
    ;; group BOTTOM: the new `org-air-project--state-sort-rank' (ready →
    ;; work-in-progress → complete → draft → unknown → DROPPED last) is
    ;; the ONE rank source in BOTH comparators (`--state-first-lessp' +
    ;; `--doc-compare'); the single-caller rank fns
    ;; `--state-display-rank' / `--state-rank' are DELETED
    ;; (`--state-display-order' stays untouched, counts-only — the airctl
    ;; `-Da' letter-parity contract).  R51-3 (byte-invisible: text
    ;; PROPERTIES only) makes the board `…and N more' row an actionable
    ;; TAB/RET target — no golden moves for it.  Byte-golden movement is
    ;; exactly ONE fixture: project-view-tag.txt (#ui reorders Epsilon→
    ;; Alpha→Zeta→fold to Alpha→Zeta→Epsilon→fold; the draft sinks below
    ;; the live states).  Verified on the impl tip: dir/state project
    ;; goldens + EVERY board/layout/entry-view/denote golden are
    ;; byte-identical (the f5 byte test fails at the TAG grouping only).
    ;; Intended re-bless via `make clean && make regen-mockups' on the
    ;; TEST seat (fixtures NOT edited on the impl track); the legacy
    ;; assertion ERTs below re-bless there too (spec §R51-1/§R51-2 name
    ;; the r48 inversions; the rank-fn callers retarget to
    ;; `--state-sort-rank' or pin the new ordering):
    ;;
    ;; =================================================================
    ;; v0.5 ROUND-51 CLOSEOUT (impl tip vpwwokuw/7459d4d2 + test re-bless
    ;; <this commit>).  ALL 8 grind entries CLOSED — the ONE moved byte
    ;; golden regenerated from impl's render via the FROZEN-CLOCK
    ;; renderer (make clean && make regen-mockups — the R48 lesson: clean
    ;; first, or a stale .elc regen is a silent no-op; anti-tautology
    ;; guards active; NO HANG, exit 0; jj diff --stat = ONLY
    ;; tests/fixtures/project-view-tag.txt, 3 changed lines: #ui reorders
    ;; Epsilon→Alpha→Zeta→fold to Alpha→Zeta→Epsilon→fold — the draft
    ;; sinks below the live states, the fold row stays the group bottom;
    ;; the dir/state project goldens + EVERY board/layout/entry-view/
    ;; denote golden are byte-identical, confirming R51-1 is face-only
    ;; and R51-3 is text-PROPERTIES-only) and the flagged assertion ERTs
    ;; re-blessed HONESTLY to the design-blessed R51 contracts
    ;; (air/v0.5/org-air-round51-design.org) — each asserts the NEW true
    ;; order, none weakened:
    ;;   org-air-f5-project-view-byte-mockups — PASSES on the regen'd tag
    ;;     golden (no assertion edit); entry deleted.
    ;;   org-air-r48-3-toggle-reveals-greyed-and-hides — the face-
    ;;     DEFINITION conjunct INVERTED in place (R51-1 supersedes the
    ;;     R48-2 strike detail: `:strike-through' asserted nil/
    ;;     unspecified, NEVER t; the inherit-faded conjunct stays — grey
    ;;     is retained, not dropped); the revealed-position conjunct
    ;;     re-pinned to the R51-2 group BOTTOM (Delta below the draft
    ;;     Epsilon AND the unknown Eta — was the mid-list
    ;;     Zeta<Delta<Epsilon slot, which R51-2 retires: the revealed row
    ;;     now renders exactly where the fold row sat).
    ;;   org-air-r48-4-collapse-dropped-nil-renders-inline-greyed — the
    ;;     knob-nil inline position re-pinned the same way (Delta LAST in
    ;;     v0.2/: below Zeta, Epsilon AND Eta); the grey-face conjunct is
    ;;     unchanged and still true.
    ;;   org-air-r16-d5-state-rank-is-within-group-primary — the expected
    ;;     order re-blessed to the R51-2 rank (ready → wip → complete →
    ;;     draft → unknown → dropped LAST): Abe-ready, Zed-complete,
    ;;     Ben-draft, Yan-draft (was the draft-first lifecycle order).
    ;;   org-air-r16-d5-state-rank-unknown-last — retargeted from the
    ;;     DELETED `org-air-project--state-rank' to `--state-sort-rank';
    ;;     now also pins dropped PAST unknown (dead sorts after broken).
    ;;   org-air-r20-5-state-display-order-matches-airctl — the
    ;;     `--state-display-order' equality + no-review conjuncts stay
    ;;     (the constant is counts-only now — the airctl `-Da' LETTER
    ;;     parity contract, untouched); the rank conjuncts retargeted
    ;;     from the DELETED `--state-display-rank' to `--state-sort-rank'
    ;;     with dropped>draft — the old dropped<draft slot survives ONLY
    ;;     in the letter order, per the R51-2 rank-source split.
    ;;   org-air-r20-5-fix-collect-excludes-overview-stateless-unknown —
    ;;     the unknown>draft rank conjunct retargeted to
    ;;     `--state-sort-rank' + the new dropped>unknown pin; every
    ;;     collect/count conjunct unchanged.
    ;;   org-air-r25-3-display-order-minus-review — the ready<draft rank
    ;;     conjunct retargeted to `--state-sort-rank'; the equality
    ;;     conjunct unchanged.
    ;; NEW R51 EXECUTING ERTs (tests/org-air-round51-test.el):
    ;;   org-air-r51-1-dropped-faces-carry-no-strike — BOTH dropped faces
    ;;     (`org-air-face-project-dropped' + the badge-only
    ;;     `org-air-face-air-state-dropped'): `:strike-through' is nil/
    ;;     unspecified AND each still inherits `org-air-face-faded';
    ;;     executing seams — the knob-nil dir render still faces the
    ;;     Delta title with the row face (R48-2 seam intact) and the
    ;;     inspector State line faces its `Dropped' label with the badge
    ;;     face.  Reverting R51-1 FAILS.
    ;;   org-air-r51-2-dropped-sorts-after-last-draft — (a) #ui rows in
    ;;     exact consecutive buffer order Alpha/Zeta/Epsilon/fold with
    ;;     nothing after the fold inside the section; (b) dir grouping
    ;;     EXPANDED via the toggle — Delta's position GREATER than the
    ;;     draft Epsilon's and the unknown Eta's; (c) LOCK — collapsed
    ;;     default: the fold row is its group's LAST row before the next
    ;;     `org-air-section' heading in ALL THREE groupings.  Reverting
    ;;     R51-2 FAILS (a)+(b).
    ;;   org-air-r51-3-tab-ret-on-fold-rows-expand — REAL key dispatch
    ;;     (`key-binding' → `call-interactively', so the knob-installed
    ;;     map is what's asserted): BOARD TAB on the `…and N more' row
    ;;     expands its bucket (joins `org-air-view--expanded-sections',
    ;;     THAT more row gone while the OTHER bucket's survives, section
    ;;     item rows +EXACTLY N, point on the first newly-revealed row's
    ;;     title); fresh-render RET performs the SAME expansion and ONLY
    ;;     that (no pane window, no *org-air-view* buffer); PROJECT lock
    ;;     — TAB and RET dispatched on the v0.2 `… 1 dropped' fold row
    ;;     still reveal the hidden doc (+1 visible, fold gone).
    ;;     Reverting R51-3 FAILS the board half (TAB drifted to the next
    ;;     header, RET ran the pane).
    ;; airctl `status -Da' parity: preserved by construction — the R51-2
    ;; rank-source split leaves `--state-display-order' (the letter-
    ;; parity contract) untouched and every count surface reads the FULL
    ;; doc list (org-air-r22-6-* / org-air-f5-* / org-air-r48-5 stay
    ;; green; the R48 closeout letter pins `R4(+1) C14(+14) X1(+9)
    ;; D2(+8)' hold).  No .el SOURCE touched on the test track (impl
    ;; landed R51-1/R51-2/R51-3 in vpwwokuw).  Round-51 manifest is
    ;; EMPTY; the tests stay as permanent regression guards.
    ;; =================================================================
    ;; v0.5 ROUND-53 impl grind (air/v0.5/org-air-round53-design.org).
    ;; NEVER-HANG AT 5000+ FILES.  P1 work-buffer scan: the data layer
    ;; scans every file in ONE reused org-mode work buffer (org-ql stays
    ;; the only query engine; org-air changed the BUFFERS it hands over),
    ;; so every `org-air-item' marker slot is now the durable (FILE . POS)
    ;; cons — first-class everywhere since R26-8 — and NO source buffer is
    ;; ever retained by scanning.  P1b never-error law: a signalling file
    ;; (gpg/unreadable/binary/vanished/too-large) contributes 0 items + one
    ;; skip-log entry, never an abort.  P1c: slices are TIME-budgeted
    ;; (`org-air-refresh-slice-budget'; `org-air-refresh-files-per-slice'
    ;; obsoleted), run under `while-no-input', and the R42-2 watchdog NEVER
    ;; drains a queue bigger than the sync budget synchronously (that
    ;; force-complete WAS the measured 4.5-minute hang) — it re-arms the
    ;; same budgeted driver on a repeating wall-clock timer instead.
    ;; P1d/P2: cache v2 (version bump), struct gains scan-time slots
    ;; (kind/donep/activity/body-deadline) so classify/render is data-pure.
    ;; Byte goldens: ZERO churn verified (all viewport/layout byte tests
    ;; green on the impl tip).  The legacy assertion ERTs pinning the
    ;; superseded contracts re-bless on the TEST seat:
    ;;
    ;; =================================================================
    ;; v0.5 ROUND-53 CLOSEOUT (impl tip mmxnzrpm/ca547182 + test re-bless
    ;; <this commit>).  ALL 6 grind entries CLOSED — byte goldens
    ;; byte-IDENTICAL (make clean && make regen-mockups verified ZERO
    ;; churn: jj diff = 0 files changed — the batch path stays the exact
    ;; synchronous scan and the small heading-bearing fixtures grow no
    ;; Notes section), so the re-bless is assertion-only.  Each flagged
    ;; ERT re-blessed HONESTLY to the design-blessed R53 contracts
    ;; (air/v0.5/org-air-round53-design.org) — each asserts the NEW true
    ;; behaviour, none weakened:
    ;;   org-air-query-item-accessors — the `markerp' assertion re-blessed
    ;;     to the durable (FILE . POS) cons (spec §P1 rule 3): car = the
    ;;     item's source file (suffix-pinned), cdr = a position >= 1.  The
    ;;     work-buffer scan retains NO source buffer, so a scanned item
    ;;     can never carry a live marker into a scan-opened buffer.
    ;;   org-air-r26-8-batch-purity-never-reads-cache — the batch-purity
    ;;     conjuncts (zero cache reads, sync scan in the call) carry over
    ;;     VERBATIM (the zero-reads counter IS the purity fence); only the
    ;;     trailing `live markers, not cache cons' assertion re-blessed to
    ;;     the (FILE . POS) cons the sync scan now yields too.
    ;;   org-air-r26-8-interleaving-single-swap +
    ;;   org-air-r26-8-token-cancels-stale-slice — slices are
    ;;     TIME-budgeted (`org-air-refresh-slice-budget';
    ;;     `org-air-refresh-files-per-slice' obsoleted — binding it to 1
    ;;     no longer forces >1 slice).  Re-blessed onto the design's own
    ;;     budget seam: budget 0 = the slice loop's ONE-file minimum per
    ;;     slice, which reopens the mid-refresh window both tests need;
    ;;     every mid-refresh conjunct (no repaint between slices, board
    ;;     usable, single swap on completion; stale-token slice a no-op on
    ;;     queue AND accumulator) carries over verbatim.
    ;;   org-air-r42-watchdog-force-completes-strand — re-blessed to the
    ;;     R53 not-drained + pacer-armed contract (spec §P1c, ERT seam 4):
    ;;     above the sync budget the watchdog fire runs ZERO scans (org-ql
    ;;     entry-point counter), leaves the queue intact and the state
    ;;     `refreshing'; interactively (noninteractive bound nil) it
    ;;     re-arms the SAME slice driver on the repeating WALL-CLOCK pacer
    ;;     (`timer--repeat-delay' == `org-air-view--refresh-wallclock-
    ;;     pace') + a fresh watchdog; the queue then CONVERGES BY PACING
    ;;     (slices drain to the terminal single-swap).  The old
    ;;     unconditional sync force-complete WAS the measured 4.5-minute
    ;;     hang at 5000 files.
    ;;   org-air-r42-watchdog-fails-honestly — re-blessed onto the
    ;;     small-remainder sync branch: above the budget the stubbed
    ;;     erroring scan is NEVER reached (state stays `refreshing', queue
    ;;     intact); with the budget raised to the queue length the sync
    ;;     branch runs, the scan signals, and the state lands HONESTLY at
    ;;     `failed' — never stuck at `refreshing'.
    ;; NEW R53 EXECUTING ERTs (tests/org-air-round53-test.el, all
    ;; batch/headless per the spec's ERT seams; revert of each FAILS):
    ;; never-error law (seams 1/2), work-buffer no-retention (seam 3),
    ;; data-pure render (seam 6), bounded file-items + Notes section
    ;; (seams 1/9), refile-to-file-top + cache-enumerated targets/vocab
    ;; (seam 7), budgeted pacing + non-syncing watchdog + abort (seams
    ;; 4/5).  The perf probes stay OUT of the gate (tiny fixture corpora
    ;; only; no 5000-file scan in `make check').  No .el SOURCE touched
    ;; on the test track (impl landed R53 P1–P5 in mmxnzrpm).  Round-53
    ;; manifest is EMPTY; the tests stay as permanent regression guards.
    ;; =================================================================
    ;; 2026-07-15 — ROUND-53 FIX (Fable review B1 + M2), impl track.
    ;; STRUCT/CACHE CHURN FLAG for the test seat:
    ;;   • `org-air-item' gained the `subtree-ts' slot (epoch float of the
    ;;     first body timestamp, populated at scan time).  Any golden that
    ;;     prints/reads raw item records reshapes.
    ;;   • `org-air-view--cache-version' bumped 2 -> 3 (v2 caches discard;
    ;;     old-shape records must never hydrate).
    ;;   • B1: `org-air-view--day-groups' Logged/created now reads the
    ;;     live-marker probe FIRST, falling back to `subtree-ts' — never
    ;;     bare `activity' (its mtime fallback would wrongly fill the
    ;;     group).  ERT seam: a cons-marker item with a body ts of today
    ;;     lands in a NON-empty Logged/created group; test seat adds the
    ;;     revert-fails ERT.
    ;;   • M2: `org-air-query--scan-live-buffer' binds `inhibit-message'
    ;;     (echo hygiene parity with the work-buffer path).
    ;; No golden fixture churn observed (`make check' green untouched).
    ;; Round-53fix manifest is EMPTY.
    ;; =================================================================
    ;; v0.5 ROUND-53 FIX CLOSEOUT (impl tip lsyqxnwrsptn + test seat
    ;; <this commit>).  The impl's STRUCT/CACHE CHURN FLAG above is
    ;; CLOSED — the churn is byte-invisible and self-blessing:
    ;;   • `subtree-ts' slot / cache v3: NO golden prints or reads raw
    ;;     `org-air-item' records (swept fixtures + tests: no `#s(', no
    ;;     positional record construction), and the r26-8 cache ERTs
    ;;     read `org-air-view--cache-version' as a VARIABLE, so the
    ;;     2 -> 3 bump follows automatically (the version-mismatch
    ;;     cold-path conjunct pins -99, not 2).  `make clean && make
    ;;     regen-mockups' (FROZEN-CLOCK renderer, guards active; NO
    ;;     HANG, exit 0) verified ZERO fixture churn — jj diff = the
    ;;     new ERT only; every board/layout/project/entry-view/denote
    ;;     golden is byte-identical.  No assertion re-blessed — none
    ;;     pinned the v2 shape.
    ;; NEW R53fix EXECUTING ERT (tests/org-air-round53-test.el):
    ;;   org-air-r53fix-b1-day-groups-read-subtree-ts — a (FILE . POS)
    ;;     cons-marker scanned item whose subtree BODY carries a <today>
    ;;     active timestamp lands in a NON-empty `Logged / created'
    ;;     day-view group keyed by the scan-time `subtree-ts' slot
    ;;     (asserted populated + day-keyed to today at scan time); the
    ;;     in-process revert proof pins `--marker-timestamp-time' = NIL
    ;;     on the cons item, and the undated sibling fence pins that
    ;;     `activity''s mtime fallback (= today on the just-written
    ;;     file) NEVER fills the group.  Verified REVERT-FAILS by
    ;;     re-running against the pre-fix `--day-groups' body (probe-only
    ;;     key): the group empties and the ERT fails.  M2 (live-buffer
    ;;     `inhibit-message' echo hygiene) is already fenced by the
    ;;     r53-1 no-echo-spam conjunct on the scan path.  The R48/R51
    ;;     lesson re-learned: `make clean' FIRST — a stale pre-fix .elc
    ;;     shadowed the new accessor (`void-function
    ;;     org-air-item-subtree-ts') until cleaned.
    ;; Round-53fix manifest is EMPTY; the test stays as a permanent
    ;; regression guard.
    ;; =================================================================
    ;; 2026-07-15 — ROUND-54 part 1 (R54-1 + R54-2; R54-3 Revisit view
    ;; HELD for a follow-on), impl track
    ;; (air/v0.5/org-air-round54-design.org).
    ;; STRUCT/CACHE CHURN FLAG for the test seat:
    ;;   • `org-air-item' gained `active-ts' (epoch float of the first
    ;;     ACTIVE <ts> in the subtree via `org-ts-regexp'; planning lines
    ;;     in, inactive [..] out — the R54-1 stale-eligibility signal)
    ;;     and `ntype' ('task | 'journal | 'knowledge — the R54-2
    ;;     content-derived note type; nil = built outside the scan =
    ;;     task treatment).
    ;;   • `org-air-view--cache-version' bumped 3 -> 4 (one bump for both
    ;;     slots + the new `:file-meta' cache key); a v3 cache is a clean
    ;;     cold miss.  The per-file fact table
    ;;     (`org-air-query--file-meta': :title/:org-title/:tags/:ntype/
    ;;     :mtime/:created) is persisted as `:file-meta' and hydrated on
    ;;     cache load; the R54-3 link-graph keys (:ids/:links-out/
    ;;     :links-in) and the `:visits' ledger land WITH the Revisit view.
    ;;   • R54-1 stale gate: `org-air-classify--stale-eligible-p'
    ;;     (scheduled ‖ deadline ‖ active-ts ‖ the live-marker
    ;;     `--marker-active-ts' fallback) is the FIRST conjunct of the
    ;;     stale clause; the stale CLOCK (`--last-activity') unchanged.
    ;;   • R54-2 routing in `org-air-classify-item': kind 'file -> notes;
    ;;     inbox-dweller -> task buckets (bypass, xsqrnoyn semantics
    ;;     unchanged); ntype 'journal/'knowledge -> own bucket with NO
    ;;     board section; 'task/nil -> task buckets + R54-1 gate.
    ;;   • Denote READ compat: `org-air-query--denote-id-regexp' (query
    ;;     layer, `--' not required), filename `__tags' fallback, denote
    ;;     slug title fallback, and the read-only `denote:' follower shim
    ;;     (registered iff no other owner; resolves via the scan's ID
    ;;     index).  No `denote-*' call anywhere.
    ;;   • F1 'title-from-org now answers from file-meta when an entry
    ;;     exists (the per-row 4KB read survives only for unscanned
    ;;     files).
    ;; GOLDEN/FIXTURE CHURN (spec §Byte-golden; impl does NOT regen —
    ;; the test seat re-blesses via `make regen-mockups' + audit):
    ;;   the fixture corpus's two inactive-[ts]-only items ("Dust off
    ;;   old archive project", "Learn lute") leave Stale (2 -> 0) per the
    ;;   R54-1 semantics table, and the plain dateless "Reference notes
    ;;   without a todo state" (:note: tag) leaves Needs attention
    ;;   (8 -> 7) per R54-2 — the fold/date-column geometry reflows with
    ;;   the lost rows.  Verified against the live renderer: the diff is
    ;;   EXACTLY those sections (banner/inbox/upcoming/high-priority
    ;;   contents otherwise identical modulo column reflow).
    ;;
    ;; 2026-07-15: ROUND-54 part 1 CLOSEOUT (test re-bless <this
    ;; commit>).  ALL 10 grind entries CLOSED — the 25 board goldens
    ;; regenerated from impl's render via the FROZEN-CLOCK renderer
    ;; (make clean FIRST — the R48/R51/R53 stale-.elc lesson bit again:
    ;; the first regen ran pre-R54 bytecode and moved NOTHING — then
    ;; make regen-mockups, anti-tautology guards active; verified NO
    ;; HANG, exit 0).  AUDIT: jj diff = ONLY the 25 layout-mockup-*
    ;; goldens; the delta is EXACTLY the flagged R54 semantics — Stale
    ;; 2 -> 0 ("Dust off old archive project" + "Learn lute", inactive-
    ;; [ts]-only, now render as dateless-TODO `no date' Attention rows;
    ;; the Stale section shows its empty message), Needs attention
    ;; 8 -> 7 ("Reference notes without a todo state" types knowledge
    ;; via the :note: tag and is ABSENT from every golden), the date
    ;; column narrows 2 cols (the widest `∙ Nd quiet' cells left) and
    ;; the folds/summary counts follow (23 loaded/total unchanged —
    ;; knowledge items stay counted, just not board-sectioned).  The
    ;; project / entry-view / denote-origin / empty goldens are
    ;; byte-identical.  org-air-r49-5-batch-placement-blind +
    ;; org-air-r13-board-only-byte-mockup + the 5 mockup suites went
    ;; green on the regen alone.  The 3 assertion ERTs re-blessed to the
    ;; R54 contracts on the test track:
    ;;   org-air-classify-stale — retuned to DATED items (a SCHEDULED
    ;;     two months past + a bare active <ts> three months past,
    ;;     appended to the SCRATCH fixture copy — the canonical corpus
    ;;     deliberately renders Stale 0 now) + the R54-1 inversion
    ;;     (inactive-[ts]-only fixtures never stale).
    ;;   org-air-ux-bare-inactive-timestamp-is-activity — retargeted +
    ;;     RENAMED org-air-ux-bare-timestamp-stale-signal-is-active-only:
    ;;     inactive stamps are archival metadata ("Learn lute" NOT
    ;;     stale); an equally old ACTIVE <ts> on a scratch task IS.
    ;;   org-air-data-variation-titles-render — the alt-board knowledge
    ;;     row (:note: tag) marked :sectionless (off the board per
    ;;     R54-2; still in the summary-total ground truth, which stays
    ;;     green at 8).
    ;; NEW R54 executing ERTs (tests/org-air-round54-test.el, all
    ;; batch, revert-of-each-fails): r54-1a dateless prose never Stale
    ;; (legacy 'task knob isolates the GATE), r54-1b dateless TODO =>
    ;; Attention only, r54-1c scheduled/deadline/active-<ts> quiet =>
    ;; Stale + fresh-dated guard, r54-1d inactive CREATED drawer =>
    ;; never Stale (active-ts nil, subtree-ts filled — the probes stay
    ;; distinct), r54-1e eligibility is the FIRST conjunct (ineligible
    ;; items never consult `--last-activity'; the clock still answers
    ;; mtime — unchanged), r54-1f `--marker-active-ts' live fallback
    ;; (active answers, inactive nil, cons markers nil — data-pure),
    ;; r54-2g type derivation table, r54-2h board shows TASKS only +
    ;; inbox bypass, r54-2i property/keyword/tag overrides both
    ;; directions + invalid-value fall-through, r54-2j denote READ
    ;; fallbacks (slug/__tags/ID->:created) with (featurep 'denote)
    ;; asserted nil.  No .el SOURCE touched (impl landed R54-1/R54-2 in
    ;; uyrtyuqnlvlo).  Round-54 part 1 manifest is EMPTY; the tests
    ;; stay as permanent regression guards.
    ;; =================================================================
    ;; 2026-07-15 — ROUND-54 part 2 (R54-3 REVISIT view), impl track
    ;; (air/v0.5/org-air-round54-design.org §R54-3).
    ;; NEW MODULE org-air-revisit.el: `org-air-revisit' (buffer
    ;; "*org-air revisit*", `org-air-revisit-mode', keymap parent
    ;; `org-air-view-core-map').  ONE view over the R54-2 file-meta
    ;; scope (`org-air-revisit-types', default '(knowledge) — fork F3),
    ;; ONE row per FILE, DEFAULT sort age-ascending = dustiest first
    ;; (D2); `o'/`O' cycle age/created/title via the shared R22-3 core;
    ;; `m' cycles ALL -> ORPHANS -> SPACED; `/' filter, `z c' created
    ;; column, standard rail (age-band Summary >1y/>90d/>21d/fresh +
    ;; Actions), bounded paging (`org-air-revisit-page-limit' 200 + the
    ;; `…and N more — TAB for another page' fold row extending ONE
    ;; page).  DATA-PURE render law holds: every cell reads file-meta /
    ;; ledger slots; (buffer-list) is unchanged by a full render.
    ;; STRUCT/CACHE CHURN FLAG for the test seat:
    ;;   • `org-air-view--cache-version' stays 4 — the part-1 manifest
    ;;     declared the link-graph keys + `:visits' part of the v4
    ;;     shape, landing with this change.  file-meta plists gained
    ;;     :ids / :links-raw / :links-out / :links-in (scan-time
    ;;     extraction in `org-air-query--file-signals'; finish-time PURE
    ;;     resolution + inversion in `--link-graph-finish', run at most
    ;;     once per dirty table via `org-air-query-link-graph-ensure' —
    ;;     called from the cache serialisation and the ORPHANS render,
    ;;     never per row).  The cache gained `:visits' (the bounded
    ;;     ledger, pruned to the snapshot files at write).  A cache
    ;;     written by part-1 code (also "v4") still hydrates cleanly:
    ;;     empty ledger; its metas lack link keys, so ORPHANS can
    ;;     over-report on that one warm open until the next scan
    ;;     re-fills them (self-healing, documented on the version
    ;;     docstring).  The r54-x cache seam's "v4 roundtrips
    ;;     active-ts/ntype/:file-meta/:visits" holds as written.
    ;;   • VISIT LEDGER (D2: opt-in, `org-air-revisit-visit-ledger'
    ;;     default nil): `org-air--note-visited' records org-air's OWN
    ;;     open paths ONLY — `org-air-visit-item' (S-RET/g RET), the
    ;;     board pane RET (`org-air-view-pane-return'), revisit
    ;;     RET/S-RET; a no-op at the default; NEVER a global find-file
    ;;     hook.  With the knob on, age = max(mtime, visit) and SPACED
    ;;     shows the visited-today tick (new `visited' glyph "✓"/"v"
    ;;     in the org-air-glyphs table — no golden renders it).
    ;;   • SPACED: deterministic K-window `(mod (* day K) N)' over the
    ;;     file-name-ordered scope, K = `org-air-revisit-daily-count'
    ;;     (5, fork F9); exactly K rows, zero disk state.  ORPHANS:
    ;;     `org-air-revisit-orphan-rule' default 'disconnected (F8).
    ;;   • ENTRY POINTS: `N' registered on BOTH the board and project
    ;;     maps (-> `org-air-revisit'; `P'/`N' the symmetric switch
    ;;     pair), AND the board's Notes section HEADING answers RET
    ;;     with `org-air-revisit' (F4 doorway — `org-air-view-pane-
    ;;     return' notes-heading branch; item rows keep the pane; TAB
    ;;     still expands the preview in place).  The F4 count question
    ;;     (bucket count vs revisit-scope count) stays at the bucket
    ;;     count — flagged OPEN, one predicate if ruled the other way.
    ;;     All keys installer-owned (R35-1 knob covers install/clear;
    ;;     org-air-revisit.el re-runs the installer once at load since
    ;;     it loads after the project.el seed); evil via the shared
    ;;     `org-air-view--setup-evil'.
    ;;   • COLD PATH: warm = table already filled; cache = hydrate
    ;;     :file-meta + :visits (no scan); cold-interactive = a paced
    ;;     wall-clock fill (`org-air-refresh-slice-budget' slices at
    ;;     `org-air-view--refresh-wallclock-pace', token-guarded,
    ;;     progressive repaint per `org-air-cold-paint-interval',
    ;;     killed with the buffer) — NEVER a synchronous scan
    ;;     interactively; batch (`noninteractive') scans inline for
    ;;     deterministic ERT/regen.  `g' rides the same pacer.
    ;; GOLDEN/FIXTURE CHURN: NONE expected — no committed golden
    ;; renders a Notes section (verified: no fixture .txt contains
    ;; "Notes"), the board/project goldens are byte-identical under
    ;; make check (0 unexpected), and the R54-3 goldens are NEW
    ;; (revisit 80/120-col mockups, folded + paged states) for the test
    ;; seat to bless via the regen tool.  NO fixture edited here; no
    ;; expected-fail entries — the R54-3 ERT seams (r54-3a..3f, r54-2e
    ;; link graph, r54-3d ledger, r54-x cache) are the test seat's to
    ;; write against this impl.
    ;;
    ;; =================================================================
    ;; 2026-07-15: ROUND-54 part 2 CLOSEOUT (impl tip rszwspyuorst/
    ;; 61bca936 + test seat <this commit>).  The impl's churn flag above
    ;; is CLOSED — the R54-3 surface is byte-invisible on every
    ;; committed golden and self-blessing:
    ;;   • `make clean && make regen-mockups' (FROZEN-CLOCK renderer,
    ;;     anti-tautology guards active; NO HANG, exit 0) verified ZERO
    ;;     fixture churn — jj diff = 0 files changed.  The board Notes
    ;;     count-row's RET doorway and the `N' key are text-property /
    ;;     keymap concerns (`buffer-substring-no-properties' captures
    ;;     never see them), no committed golden renders a Notes section
    ;;     (re-verified: no fixture .txt contains "Notes"), and the
    ;;     board rail Actions legend table is UNCHANGED (`N' surfaces
    ;;     in the R50-2 help groups, not in a byte golden).  No legacy
    ;;     assertion pinned the pre-R54-3 shape — nothing re-blessed.
    ;; NEW R54-3 EXECUTING ERTs (tests/org-air-round54c-test.el, all
    ;; batch/headless through the REAL entry point + real key dispatch;
    ;; ALL 7 verified REVERT-FAIL against the pre-impl trunk
    ;; zyytqtyzoqlp in a scratch workspace):
    ;;   r54c-3a knowledge-only scope — headed AND headingless
    ;;     knowledge files render one row per FILE while task/journal
    ;;     files stay OFF the surface despite sitting in the same
    ;;     file-meta table (anti-tautology); RET opens the note at its
    ;;     top (the (FILE . POS 1) cons path).
    ;;   r54c-3b default sort dustiest-first — the shared R22-3 spec
    ;;     seeds (age . ascending) and rows render strict oldest-mtime
    ;;     first; corpus mtimes deliberately NOT in name order.
    ;;   r54c-3c visit ledger — DEFAULT nil: the revisit RET open
    ;;     records NOTHING, age stays pure mtime (D2); knob t: the same
    ;;     RET (and the board `org-air-visit-item' path) records + age
    ;;     shifts to max(mtime, visit) with the visited note re-sorting
    ;;     to the bottom while the mtime stays old; the ledger
    ;;     roundtrips through the cache `:visits' with ZERO rescans
    ;;     (spy on `org-air-query-items') and is BOUNDED — the
    ;;     write-time prune drops a vanished file's entry and the
    ;;     hydrated ledger never exceeds the enumerated file count.
    ;;   r54c-3d orphans over the link graph — denote:/id:/file: links
    ;;     each resolve to FILES (https: noise never enters), the
    ;;     `:links-in' inversion counts exactly, `m'-cycled ORPHANS
    ;;     shows the disconnected notes ONLY under the default rule,
    ;;     and 'no-outbound/'either follow the knob.
    ;;   r54c-3e spaced rotation — exactly K=5 rows, NO fold row,
    ;;     stable across re-renders on a pinned day, rotating on day+1,
    ;;     covering the 15-note scope exactly once over the 3-day
    ;;     partition (frozen `current-time').
    ;;   r54c-3f entry points, knob-gated — real dispatch on a live
    ;;     rendered board: RET on the Notes section HEADING runs
    ;;     `org-air-revisit' and never the pane, RET on an ITEM row
    ;;     keeps the pane and never Revisit (counter stubs pin BOTH
    ;;     directions of the F4 doorway); `N' resolves + dispatches
    ;;     from the board AND the project maps; with the R35-1 knob
    ;;     nil every one of those keys is gone.
    ;;   r54c-3g data-pure + bounded — a 300-entry SYNTHETIC file-meta
    ;;     table (paths that do NOT exist) renders exactly the 200-row
    ;;     page + the `…and 100 more' fold row, TAB extends ONE page,
    ;;     and the whole build + paging runs `find-file-noselect' /
    ;;     `find-file' ZERO times with the visible buffer list grown by
    ;;     nothing but the revisit buffer.
    ;; OBSERVATION flagged (no gate impact, mirrors the open F4 count
    ;; question): an inbox file with NO task headings types 'knowledge
    ;; at the FILE level (`--file-ntype' has no inbox special case), so
    ;; an empty inbox would surface as a Revisit row; the corpora here
    ;; give the inbox a TODO capture (the realistic shape).  One
    ;; predicate in `org-air-revisit--scope-entries' if design rules it
    ;; out.  No .el SOURCE touched on the test track (impl landed R54-3
    ;; in rszwspyuorst).  Round-54 part 2 manifest is EMPTY; the tests
    ;; stay as permanent regression guards.
    ;; =================================================================
    ;; 2026-07-15: ROUND-54c FIX CLOSEOUT (review BLOCK -> impl fix
    ;; psnwkpxmlunq + test seat <this commit>).  The review's three
    ;; findings landed on the impl track; the test seat closed the
    ;; hole its own fixtures had hidden:
    ;;   • BLOCKER 1 (inbox in Revisit) CLOSED: the part-2 OBSERVATION
    ;;     above is RESOLVED the excluding way — the inbox is a triage
    ;;     queue, not an evergreen; `org-air-revisit--scope-entries'
    ;;     now drops `org-air-inbox-file' (view policy at the scope
    ;;     seat, memoised-truename compare; `:ntype' stays
    ;;     content-derived).  Test seat: the 3a fixture inbox retuned
    ;;     from pure-TODO (which excluded it for the WRONG reason —
    ;;     `:ntype' task — hiding the hole) to PROSE captures + an
    ;;     anti-tautology assert that it types 'knowledge yet stays
    ;;     absent; 3f now asserts the EMPTY inbox is not a rendered
    ;;     row (it silently WAS one pre-fix); NEW ERT r54c-3h drives
    ;;     both shapes — a PROSE (#+title + taskless captures) inbox
    ;;     AND an empty `#+title: inbox' file each type 'knowledge in
    ;;     file-meta yet are ABSENT from `--scope-entries' and the
    ;;     rendered rows.  Reverting the predicate fails 3a + 3f + 3h
    ;;     (verified on this tree).
    ;;   • MAJOR 1 (part-1 v4 cache => false all-orphans) CLOSED: the
    ;;     part-2 impl-track note above ("ORPHANS can over-report on
    ;;     that one warm open until the next scan re-fills them") is
    ;;     STALE as written — `org-air-query-file-meta-hydrate' now
    ;;     SKIPS metas lacking `:links-out', so a part-1 cache hydrates
    ;;     an EMPTY table (never link-less metas that read all-orphan
    ;;     and got re-persisted by the next warm write); Revisit
    ;;     re-fills via the pacer (interactive) / inline scan (batch).
    ;;     NEW ERT r54c-3i forges a REAL part-1-shaped v4 cache with
    ;;     the real writer (link keys stripped from the live table
    ;;     first, dirty flag cleared), proves hydrate fills ZERO
    ;;     entries, and the reopened Revisit's ORPHANS shows only the
    ;;     true island — never the linked hub/spoke pair.  Reverting
    ;;     the `plist-member' guard fails it (verified).
    ;;   • NIT (visits-hydrate clobber) CLOSED: hydrate takes
    ;;     max(existing, cached); r54c-3i also asserts an in-session
    ;;     visit survives an OLDER cached epoch while fresh entries
    ;;     and NEWER cached epochs still land.  Reverting the max
    ;;     fails it (verified).
    ;; GOLDEN/FIXTURE CHURN: NONE — `make clean && make regen-mockups'
    ;; re-verified zero fixture diff (the predicate/hydrate seams are
    ;; invisible to every committed golden).  No .el SOURCE touched on
    ;; the test track (impl landed the fix in psnwkpxmlunq).  Round-54c
    ;; manifest is EMPTY; the tests stay as permanent regression
    ;; guards.
    ;; =================================================================
    ;; v0.5 ROUND-52 impl grind (air/v0.5/org-air-round52-design.org).
    ;; R52-1 PROJECT dir-header rollup moves from RIGHT-JUSTIFIED (the
    ;; R22-6 `org-air-view--justify' to the pane width — detached out by
    ;; the date column on wide frames) to LEFT-ANCHORED: the `dir/' name,
    ;; a two-space gap, then the same summary (`v0.1/  R1 C1 D(+1)'),
    ;; clamped to WIDTH with the shared `more' ellipsis (the R48-3
    ;; fold-row clamp idiom).  Tokens/faces/(+N)/letter ORDER untouched
    ;; (`--dir-count-summary' + `--state-display-order' — the airctl
    ;; `status -Da' parity contract); nested child headers keep the
    ;; rollup adjacent to their OWN name at their indent for free (LEFT
    ;; already carries rails + connector).  ONE expression in
    ;; `org-air-project--insert-dir-node' + docstring/comment sweep.
    ;; Byte-golden movement is exactly ONE fixture,
    ;; tests/fixtures/project-view-dir.txt, exactly its THREE
    ;; group-header lines (spec §Golden impact pins OLD/NEW bytes).
    ;; Intended re-bless via `make clean && make regen-mockups' on the
    ;; TEST seat (fixtures NOT edited on the impl track; clean FIRST —
    ;; the R48/R51/R53/R54 stale-.elc lesson).  The test seat also owns
    ;; the r22-6 inversion + the new R52 ERTs (spec §Test churn / §ERTs).
    ;;
    ;; =================================================================
    ;; v0.5 ROUND-52 CLOSEOUT (impl tip xxyrllwq/52da2c01 + test re-bless
    ;; <this commit>).  BOTH grind entries CLOSED — the ONE moved byte
    ;; golden regenerated from impl's render via the FROZEN-CLOCK
    ;; renderer (make clean FIRST — the R48/R51/R53/R54 stale-.elc
    ;; lesson — then make regen-mockups, anti-tautology guards active;
    ;; NO HANG, exit 0).  AUDIT: jj diff = ONLY tests/fixtures/
    ;; project-view-dir.txt, EXACTLY its three group-header lines,
    ;; byte-matching spec §Golden impact's pinned NEW forms —
    ;; `| v0.1/  R1 C1 D(+1)', `+- air-context/  D1' (the nested child
    ;; adjacent to its OWN name at its OWN indent), `| v0.2/  W1 X1 D1';
    ;; the divider column + rail cells are byte-identical (the doc-pane
    ;; pad is re-emitted by `--compose-columns'), and the state/tag
    ;; project goldens + every board/layout/entry-view/denote golden are
    ;; byte-identical.  The flagged assertion ERTs re-blessed HONESTLY
    ;; to the design-blessed R52 contract
    ;; (air/v0.5/org-air-round52-design.org) — asserts the NEW true
    ;; behaviour, none weakened:
    ;;   org-air-f5-project-view-byte-mockups — PASSES on the regen'd
    ;;     dir golden (no assertion edit); entry deleted.
    ;;   org-air-r22-6-count-summaries-right-aligned — INVERTED in place
    ;;     + RENAMED org-air-r22-6-count-summaries-left-anchored (never
    ;;     silent; R52-1 supersedes the R22-6 shared-vertical-column
    ;;     rationale — adjacency wins): headers are NO LONGER justified
    ;;     to W (right-trimmed width < W), each header ENDS with its
    ;;     summary, the summary starts exactly TWO columns after the
    ;;     name's `/', and the same-length names share a START column
    ;;     while the right edges DIFFER — the exact inversion of the
    ;;     old flush-right proof.  Coverage retargeted, not deleted.
    ;; NEW R52 EXECUTING ERTs (tests/org-air-round52-test.el; the
    ;; adjacency + width-invariance pair verified REVERT-FAILS — both
    ;; red against the pre-impl right-justify by construction):
    ;;   org-air-r52-1-group-header-summary-adjacent — dir grouping at
    ;;     width 100 carries the EXACT adjacent forms (all three
    ;;     headers, incl. the NESTED `+- air-context/  D1' at its own
    ;;     indent); anti-revert conjunct: the name→summary gap is
    ;;     exactly 2 on every header (trunk: ~40-50 pad spaces).
    ;;   org-air-r52-1-summary-column-is-width-invariant — direct tree
    ;;     renders at w 80 AND w 120: right-trimmed header lines
    ;;     byte-IDENTICAL across widths (the summary no longer tracks
    ;;     the right edge — the user's wide-frame detachment is
    ;;     structurally impossible), each ending with its summary at
    ;;     width < w.  Trunk FAILS (justify pads to exactly w).
    ;;   org-air-r52-1-long-name-header-clamps — LOCK: a synthetic
    ;;     overflowing node emits a header of string-width exactly W
    ;;     ending with the shared `more' glyph (the clamp can never be
    ;;     dropped now that the justify call — whose own truncation
    ;;     provided it — is gone).
    ;;   org-air-r52-1-rollup-tokens-unchanged — LOCK, anti-scope-creep:
    ;;     `--dir-count-summary' over the R22-6 airctl vector still
    ;;     yields exactly `R4(+1) C14(+14) X1(+9) D2(+8)' — tokens,
    ;;     letter ORDER (`--state-display-order', the airctl `status
    ;;     -Da' parity contract) and `(+N)' semantics did NOT move with
    ;;     the position.
    ;; No .el SOURCE touched on the test track (impl landed R52-1 in
    ;; xxyrllwq).  Round-52 manifest is EMPTY; the tests stay as
    ;; permanent regression guards.
    ;; =================================================================
    ;; v0.5 ROUND-56 impl grind (air/v0.5/org-air-round56-design.org).
    ;; R56 lands the measured stuck-refresh fixes: P1 inbox-first
    ;; progressive STREAM paints + the cache-stale open painting its
    ;; full cached board (deferred one-shot owns the paced kickoff); P2
    ;; the adaptive self-chaining wall-clock pacer (one-shot
    ;; `run-with-timer' chain, 0.01s fast gap / 0.15-0.6s abort backoff
    ;; via `org-air-view--refresh-next-gap') replacing BOTH the
    ;; once-per-idle-period "repeating" idle pacer and the 0.2s
    ;; repeating wall-clock watchdog fallback, plus started-scans-only
    ;; abort accounting (`org-air-view--refresh-scan-started'); P3 the
    ;; salient `⟳ scanning N/M…' banner segment replacing both
    ;; `loading N/M files' and `stale · refreshing…'.  Six legacy ERTs
    ;; hardcode the superseded contracts and need TEST-track re-bless
    ;; (no fixture/golden moved — every change is gated off batch; `make
    ;; regen-mockups' churn is zero by construction).
    ;;
    ;; =================================================================
    ;; v0.5 ROUND-56 CLOSEOUT (impl tip tvlvvlmm/8afc5347 + test re-bless
    ;; <this commit>).  ALL 6 grind entries CLOSED — byte goldens
    ;; byte-IDENTICAL as flagged (make clean && make regen-mockups,
    ;; FROZEN-CLOCK renderer, anti-tautology guards active; NO HANG,
    ;; exit 0; jj diff = ONLY test .el files, ZERO fixture churn — every
    ;; R56 change is gated off the batch/idle machine), so the re-bless
    ;; is assertion-only.  Each flagged ERT re-blessed HONESTLY to the
    ;; design-blessed R56 contracts
    ;; (air/v0.5/org-air-round56-design.org) — each asserts the NEW true
    ;; behaviour, none weakened; ALL 6 verified REVERT-FAIL against the
    ;; pre-impl trunk mmttlvtu in a scratch workspace:
    ;;   org-air-r26-8-stale-paint-marker-then-swap — the mid-refresh
    ;;     banner re-pinned to the salient `⟳ scanning N/M…' segment
    ;;     (P3a: ONE string everywhere) with the retired faded `stale ∙
    ;;     refreshing' AND the retired `loading N/M' asserted ABSENT;
    ;;     the paint-cached-first / single-swap / crisp-clear (P3c)
    ;;     conjuncts carry over verbatim.
    ;;   org-air-r34-3-arm-disarm-lifecycle — the lifecycle law (exactly
    ;;     one live pacing timer; disarm/cancel tear down) re-asserted
    ;;     against `timer-list': the chain link is a WALL-CLOCK one-shot
    ;;     (never `timer-idle-list', `timer--repeat-delay' nil — neither
    ;;     the retired repeating idle pacer nor the obsolete 0.2s
    ;;     repeating fallback), re-arm is single (same timer object,
    ;;     count 1), and disarm now also proves the watchdog backstop
    ;;     down.
    ;;   org-air-r34-3-cold-end-to-end-reaches-done — the skeleton greps
    ;;     `scanning 0/N…' in the banner AND the centred body line's
    ;;     `(scanning 0/N)' copy (P3a/P3b, independent of `--loading'),
    ;;     with the retired `loading 0/N' pinned ABSENT; at DONE the
    ;;     clear-check covers the scanning segment too.  Every
    ;;     end-to-end convergence conjunct carries over verbatim.
    ;;   org-air-r34-3-warm-run-leaves-no-live-pacer — same banner
    ;;     re-bless (salient segment mid-refresh, retired marker ABSENT
    ;;     both mid-refresh and at DONE); the no-live-pacer-after-done
    ;;     law carries over verbatim.
    ;;   org-air-r42-watchdog-force-completes-strand — part (2)
    ;;     re-blessed to the P2b fallback: the fire re-arms the adaptive
    ;;     ONE-SHOT chain (`--refresh-chain-live-p', live `timer-list'
    ;;     entry, repeat-delay nil, never idle-gated) + a fresh watchdog
    ;;     — the `timer--repeat-delay' == `--refresh-wallclock-pace'
    ;;     assertion retired with the parallel repeating pacer.  The
    ;;     never-sync-a-big-queue law (zero scans in the fire, queue
    ;;     intact, convergence by pacing) carries over verbatim.
    ;;   org-air-r53-7-over-budget-paces-never-force-scans — part (4)
    ;;     re-blessed to P2c started-scans-only accounting (spec ERT
    ;;     seam 6, BOTH halves): (4a) a FULL retry-budget of PRE-START
    ;;     aborts (pending input before the head's scan begins —
    ;;     `while-no-input' aborts on its opening `input-pending-p')
    ;;     skip-logs NOTHING and the head stays queued; (4b) the same
    ;;     budget of MID-SCAN aborts (the started flag raised as the
    ;;     slice loop does right before its query call) still
    ;;     skip-logs `slow' and drops — the anti-livelock stays.  Parts
    ;;     (1)-(3)/(5) (zero sync scans, watchdog paces, C-g abort,
    ;;     convergence) carry over verbatim.
    ;; NEW R56 EXECUTING ERTs (tests/org-air-round56-test.el, all
    ;; batch/headless through the spec's named seams; paint gates opened
    ;; by let-binding `noninteractive' nil around the direct slice
    ;; drives only — no real timer ever fires; R56-1..6 verified
    ;; REVERT-FAIL against the pre-impl trunk mmttlvtu):
    ;;   r56-1-inbox-paints-before-full-scan — P1c both halves: the pure
    ;;     `--refresh-queue-order' table (inbox position 1, rest mtime
    ;;     DESC, stable ties/missing-mtimes) + the executing seam: inbox
    ;;     sorts LAST in enumeration AND carries the OLDEST mtime
    ;;     (anti-tautology — only the inbox rule can head it), yet ONE
    ;;     budgeted slice + the immediate un-throttled first paint put
    ;;     the inbox capture in `--items' AND on the rendered board
    ;;     while the queue is >90% full and the machine live.
    ;;   r56-2-cold-stream-paints-repeatedly — P1b (seams 2/4): stubbed
    ;;     clock stepping 0.6s/slice -> >=3 progressive paints STRICTLY
    ;;     increasing in item count (empty-file slices paint nothing —
    ;;     the new-items condition), `--loading' nil after the first,
    ;;     stream mode set AT refresh-start and ended by the finish
    ;;     swap.  The retired self-clearing `--loading' gate yields
    ;;     exactly 1 paint — fails.
    ;;   r56-3-cache-stale-paints-cached-board-first — P1a (seam 3): the
    ;;     STALE deferred one-shot (driven directly, the sanctioned
    ;;     R45-2 seam) renders the FULL cached board with ZERO scans in
    ;;     the call, then OWNS the paced kickoff — cached content on
    ;;     screen while `refreshing' with a non-empty queue, stream mode
    ;;     OFF (single-swap behind the painted board); the P3b banner
    ;;     tick rewrites line 1 in place with `scanning N/M'.
    ;;   r56-4-adaptive-pacer-gap-and-convergence — P2a (seam 5): pure
    ;;     gap table (0.01 uninterrupted; 0.15/0.3/0.6/0.6 doubling
    ;;     abort backoff; 0.01 recovery; bounded both ways over a
    ;;     500-step mixed chain — the R34-3 anti-strand law); the chain
    ;;     armer records the gap and keeps EXACTLY ONE pending one-shot
    ;;     across transitions; the 198-slice wall-clock model beats the
    ;;     retired 0.2s duty-cycle by >5x; a real 30-file queue
    ;;     converges in <= one slice per file (never indefinite).
    ;;   r56-5-progress-segment-truth — P3a/P3b/P3c (seam 7): skeleton
    ;;     carries `scanning 0/6…' (banner + body-line copy), N grows
    ;;     1/6 -> 2/6 from the machine's own numbers, the segment shows
    ;;     on a PAINTED board with `--loading' NIL wearing the salient
    ;;     `org-air-face-progress', clears crisply at the finish swap
    ;;     (plain `N items' returns); retired strings never render.
    ;;   r56-6-never-hang-abort-and-watchdog — P2b (seam 8): pending
    ;;     input aborts a slice with queue/acc/state untouched; the
    ;;     watchdog fire on a >budget queue runs ZERO scans (never a
    ;;     main-thread force-scan) and interactively re-arms the
    ;;     ONE-SHOT chain + fresh watchdog; convergence by pacing.
    ;;   r56-7-warm-refresh-stays-single-swap — seam 9 LOCK (not a
    ;;     revert-fence: pre-R56 warm refreshes never streamed either):
    ;;     a painted board + all-files-touched paced refresh with the
    ;;     paint gates OPEN performs ZERO progressive paints and exactly
    ;;     ONE content repaint (the finish swap); `--refresh-progressive'
    ;;     stays nil — P1b's stream mode can never leak into warm
    ;;     refreshes.
    ;; No .el SOURCE touched on the test track (impl landed R56 P1-P3 in
    ;; tvlvvlmm).  Round-56 manifest is EMPTY; the tests stay as
    ;; permanent regression guards.
    ;; ===================================================================
    ;; v0.5 ROUND-60 impl grind (air/v0.5/org-air-round60-design.org).
    ;; R60-3 CACHE-KEY EXTENSION: `org-air-view--cache-key' gains
    ;; `org-air-exclude-regexps' as the design-blessed FIFTH element (the
    ;; key IS the detector — a cache written under exclude set A must
    ;; never hydrate under set B; pre-R60 4-element keys miss on length,
    ;; no `org-air-view--cache-version' bump).  The R59 key-shape
    ;; assertion pins the OLD 4-element contract (`(= (length key) 4)'),
    ;; so it fails by design until the test seat re-blesses it to the
    ;; 5-element shape (fifth = the live `org-air-exclude-regexps',
    ;; default nil).  NOT hand-blessed on the impl track per the brief.
    ;; No byte golden moved (default-nil excludes are the pre-R60 path
    ;; byte-for-byte; verified: all layout/project/denote goldens green).
    ;;
    ;; =================================================================
    ;; v0.5 ROUND-60 CLOSEOUT (impl tip yukzyouz/5a138df1 + test seat
    ;; <this commit>).  The ONE grind entry CLOSED — byte goldens
    ;; byte-IDENTICAL as flagged (default-nil excludes are the pre-R60
    ;; discovery path byte-for-byte; `make clean && make check' green
    ;; with zero fixture churn), so the re-bless is assertion-only.  The
    ;; flagged ERT re-blessed HONESTLY to the design-blessed R60-3
    ;; contract (air/v0.5/org-air-round60-design.org) — no conjunct
    ;; weakened:
    ;;   org-air-r59-13-cache-v5-and-key — the key-shape conjunct moves
    ;;     4 -> 5 (`org-air-exclude-regexps' is the FIFTH element), and
    ;;     the re-blessed shape still MEANS something: the fifth element
    ;;     is asserted to TRACK the live exclude set both ways (the nil
    ;;     default and a let-bound set) and to DETECT a flip (different
    ;;     exclude sets compare un-`equal'), and a NEW knob-parallel
    ;;     hydration conjunct pins that a cache written under nil
    ;;     excludes never hydrates under a non-nil set (the exact mirror
    ;;     of the R59 skip-container-headings conjunct — reverting the
    ;;     R60 key extension hydrates it and FAILS).  Every other
    ;;     conjunct (v5 roundtrip, crafted-v4 cold miss, knob-t/nil
    ;;     hydration fence) carries over verbatim.
    ;; NEW R60 EXECUTING ERTs (tests/org-air-round60-test.el, all
    ;; batch/headless over temp trees through the REAL discovery layer
    ;; (`org-air-query-files' / `org-air-query-items' / the real board
    ;; render / `org-air-view--refresh-start'); revert of each FAILS —
    ;; the spec's T1-T11 seams mapped onto the eight ERTs):
    ;;   r60-1 excluded FILE never appears (query-files, scan items, the
    ;;     rendered board) while the non-matching sibling survives; the
    ;;     nil-knob anti-tautology leg proves the file is otherwise
    ;;     enumerated.  Reverting the file post-filter FAILS.
    ;;   r60-2 excluded DIRECTORY is PRUNED, never descended — both a
    ;;     dot-dir ("\\.git/") and a named dir ("/archive/"): the
    ;;     `file-name-all-completions' listing spy proves archive/,
    ;;     archive/deep/, .git/ and .git/objects/ were never LISTED
    ;;     (the planted deep files under both would be counted if
    ;;     walked) — a post-filter-only impl passes the membership
    ;;     asserts and FAILS the spy.
    ;;   r60-3 inbox NEVER excluded: file-level guard (a regexp matching
    ;;     the inbox name drops the non-inbox twin, keeps the inbox),
    ;;     ancestor guard (inbox inside the excluded tree: the spine is
    ;;     walked, archive/deep/ still pruned, every OTHER file in the
    ;;     tree still dropped), and the excluded SOURCE ROOT (nil when
    ;;     the inbox is outside it; exactly the inbox when inside).
    ;;   r60-4 exclude WINS over an explicit `org-air-files' listing
    ;;     (with the nil-knob anti-tautology leg).
    ;;   r60-5 nil / all-invalid excludes = pre-R60 discovery EXACTLY:
    ;;     result `equal' to the direct nil-PREDICATE enumeration, the
    ;;     PREDICATE argument spied as LITERAL nil at every call, the
    ;;     dot-dir file that leaks today still leaks; never-error —
    ;;     ("[" "/archive/") signals nowhere, "/archive/" still prunes,
    ;;     exactly ONE warning per session for "[".
    ;;   r60-6 the exclude set is the FIFTH cache-key element: different
    ;;     sets = different keys; a cache written under set A hydrates
    ;;     under A (anti-vacuous) and never under B or nil; a crafted
    ;;     pre-R60 4-element `:key' misses on length.
    ;;   r60-7 the refresh-start key guard: flipping the exclude on a
    ;;     live board drops the excluded file's rows on `g' — the spy
    ;;     proves `org-air-query-items-in-files' is never handed the
    ;;     excluded path — and the NEXT refresh does not resurrect them;
    ;;     same for narrowing `org-air-files' mid-session (the bonus
    ;;     bug: the pre-R60 vanished/`file-exists-p' branch resurrected
    ;;     removed rows on every refresh).
    ;;   r60-8 symlink-truename dedupe holds with exclusion active:
    ;;     exclusion is BY NAME (a non-matching symlink into the pruned
    ;;     tree survives — its target truename enumerates exactly once;
    ;;     a symlink whose OWN path matches is dropped and its otherwise
    ;;     unreachable target never surfaces) and the R53 dedupe still
    ;;     collapses a symlink twin to ONE entry.
    ;; No .el SOURCE touched on the test track (impl landed R60-1..3 in
    ;; yukzyouz).  Round-60 manifest is EMPTY; the tests stay as
    ;; permanent regression guards.
    ;; =================================================================
    ;; =================================================================
    ;; v0.5 ROUND-61 grind (impl track) — assertions that legitimately
    ;; shift under the design-blessed R61-2 cache-v6 contract
    ;; (air/v0.5/org-air-round61-design.org): `org-air-view--cache-version'
    ;; bumped 5 -> 6 (the org-air-item struct gained the four review
    ;; harvest slots clocks/logs/created/rtrunc — v5 records have the
    ;; wrong record length) and `org-air-log-cap' joined
    ;; `org-air-view--cache-key' as the SIXTH element (the cap shapes
    ;; scanned-and-persisted data; the R57 "the key IS the detector"
    ;; discipline).  The two pre-R61 tests below hardcode the old
    ;; version/key-length contract; every other conjunct in them still
    ;; holds.  NOT hand-blessed here — the test seat re-blesses them to
    ;; the v6 / 6-element contract and deletes these entries as closeout.
    ;;
    ;; 2026-07-20: ROUND-61 CLOSEOUT (test re-bless <this commit>).
    ;; BOTH entries CLOSED — the two key-contract ERTs re-blessed
    ;; HONESTLY to the v6 / 6-element-key contract, no conjunct
    ;; weakened:
    ;;   org-air-r59-13-cache-v5-and-key — version assert 5 -> 6, key
    ;;     length 5 -> 6; GAINED the sixth-element conjuncts (the live
    ;;     `org-air-log-cap' is (nth 5), tracks a let-bound value,
    ;;     detects a flip as a different key) and the cap hydrate fence
    ;;     (a cache written under the default cap never hydrates under
    ;;     a changed cap, while the original cap still does).  The
    ;;     exclude set is STILL detected at its fifth seat — every
    ;;     pre-existing R59-knob/R60-exclude detection + hydrate/miss +
    ;;     v4-cold-miss conjunct kept verbatim.
    ;;   org-air-r60-6-exclude-set-is-fifth-cache-key-element — key
    ;;     length 5 -> 6 with the exclude set asserted UNMOVED at
    ;;     (nth 4) and every pairwise-distinct / hydrates-under-A /
    ;;     never-under-B-or-nil conjunct kept; GAINED the (nth 5)
    ;;     log-cap tracking + flip detection, and the crafted short-key
    ;;     miss now covers BOTH legacy shapes (pre-R60 4-element AND
    ;;     pre-R61 5-element), each missing on length alone.
    ;; The new R61 acceptance ERTs (tests/org-air-round61-test.el,
    ;; r61-1..r61-13: same-pass own-body harvest, never-error net, the
    ;; log-cap truncation, cal-iso period oracle, clip exactness, R57
    ;; done-set, exact rollups, cache v6 + zero-rescan navigation +
    ;; warm byte-parity, started/carried predicates, suspect clocks,
    ;; R59/R60 inheritance, the ×N chip, and R58 bookmark parity) all
    ;; PASS against impl tip tklmvwom.  No .el SOURCE touched on the
    ;; test track.  Round-61 manifest is EMPTY; the tests stay as
    ;; permanent regression guards.
    ;; =================================================================
    ;; 2026-07-20: ROUND-62 grind (impl <this commit>, spec
    ;; air/v0.5/org-air-round62-design.org).  DELIBERATE R62-3 behaviour
    ;; change, design-blessed: `m' (`org-air-review-toggle-kind', kept
    ;; as an obsolete alias of the new `org-air-review-cycle-range')
    ;; cycles the FULL R62-2 range ladder — week → fortnight → month →
    ;; quarter → year → week — instead of toggling week↔month.  Two
    ;; overlapping span mechanisms (a 2-state toggle beside a 5-rung
    ;; ladder) would leave `m' undefined on the three new rungs; one
    ;; ladder, three verbs (`+'/`-' directional, `m' rotary).  The two
    ;; R61 ERTs below assert the OLD toggle at the command level (their
    ;; pure-engine oracle asserts stand untouched); the design's Item 2
    ;; says exactly these command-level `m' asserts are UPDATED to the
    ;; ladder.  NOT hand-blessed here — the test seat re-blesses them
    ;; and deletes these entries as closeout.
    ;;
    ;; 2026-07-20: ROUND-62 CLOSEOUT (impl tip muqylqul/ae928f60 + test
    ;; seat <this commit>).  BOTH grind entries CLOSED — no fixture/
    ;; golden moved (the R62 changes are window machinery + render
    ;; state; `make clean && make check' green with zero fixture churn —
    ;; `make clean' FIRST, the R48/R51/R53/R54 stale-.elc lesson bit
    ;; AGAIN: the first probe ran pre-R62 bytecode and reproduced the
    ;; \"unfixed\" behaviour on the fixed tree).  The two R61 ERTs
    ;; re-blessed HONESTLY to the design-blessed R62-3 ladder contract
    ;; (air/v0.5/org-air-round62-design.org, Item 2) — the pure-engine
    ;; oracle asserts stand untouched, no conjunct weakened:
    ;;   org-air-r61-4-period-engine-oracle — the command-level walk now
    ;;     drives the FULL ladder: `m' from the current week lands
    ;;     FORTNIGHT ([Jun 8, Jun 22) — the deliberate R62-3 behaviour
    ;;     change), then month, `>' `>' to August, `m' on through
    ;;     quarter (Q3) and year (2026), the year→week WRAP landing on
    ;;     the week containing the anchor day (Aug 1 ⇒ Jul 27 … Aug 3 —
    ;;     the R61 anchor rule, now uniform over five rungs); the
    ;;     obsolete alias `org-air-review-toggle-kind' is asserted to
    ;;     still cycle, `+' to widen back to August, and `.' still
    ;;     returns to the current period.
    ;;   org-air-r61-8-cache-v6-and-no-rescan-nav — the nav burst's two
    ;;     toggle-kind presses became the five-rung ladder walk (`m'
    ;;     week→fortnight [Mar 30, Apr 13)→month, `+' ×2 through quarter
    ;;     to year, `m' wrapping back to week) under the SAME
    ;;     `org-air-query--scan-file' + `insert-file-contents' spies at
    ;;     ZERO calls; every cache-v6 / sixth-key-element / v5-cold-miss
    ;;     / warm-byte-parity conjunct carries over verbatim.
    ;; NEW R62 EXECUTING ERTs (tests/org-air-round62-test.el, all
    ;; batch/headless; the spec's T1-T10 seams; revert of each FAILS):
    ;; r62-1 host roster (all four modes + doc-session), r62-2 the R25
    ;; noninteractive-nil synchronous reconcile KEEPS review's rail
    ;; (live side window, owner review, never suspended — RED on the
    ;; unfixed predicate), r62-3 a popped board never steals the
    ;; selected review's rail while ownership still follows the ACTIVE
    ;; view both ways (no reconciler regression), r62-4 the `|' toggle
    ;; guard admits review (and still rejects foreign buffers), r62-5
    ;; placement parity (per-view review/revisit knobs win, legacy
    ;; alist + fallback + board/project/outline unchanged), r62-6 the
    ;; range-ladder oracle (fixed-phase Monday fortnights incl. the
    ;; W53/2026 edge, quarter/year bounds incl. year rollover, the
    ;; adjacency law 5 kinds × 9 edge dates × 2 TZs, clip
    ;; complementarity at the three NEW edges, unknown-kind totality),
    ;; r62-7 `<'/`>' step by ONE unit of the ACTIVE range per kind,
    ;; r62-8 `+'/`-'/`m' ladder keys (clamped ends, wrap cycle, ladder
    ;; knob validation, off-ladder narrow-out, anchor preserved) under
    ;; scan+read spies at ZERO calls, r62-9 the bookmark record carries
    ;; the range (quarter round-trip cache-first with zero display
    ;; calls, R61-shape compat, 'decade degrade, 'current re-tracking
    ;; at quarter scale), r62-10 the five label shapes + cross-ISO-year
    ;; fortnight + the four-row legend truth (`.' listed, `=' the
    ;; legend-less alias).  No .el SOURCE touched on the test track
    ;; (impl landed R62-1..3 in muqylqul).  Round-62 manifest is EMPTY;
    ;; the tests stay as permanent regression guards.
    ;; =================================================================
    ;; 2026-07-20: ROUND-63 grind (impl <this commit>, spec
    ;; air/v0.5/org-air-round63-design.org).  DELIBERATE R63-2a
    ;; behaviour change, design-blessed: the review's per-item sections
    ;; are FLAT under EVERY rollup basis — one row per item, the
    ;; per-item `(group …)' lines removed (`org-air-review--group-rows'
    ;; DELETED; the `day' group lines duplicated the date cell, the
    ;; `origin' ones were the screenshot's fake filename headers) — and
    ;; `f' is the Time-invested lens only, re-ruling R61-3's "Completed
    ;; groups its rows by it".  The ONE R61 ERT below pins the OLD
    ;; day-group line at the buffer level (its `(should group)' searches
    ;; the rendered pane for the "Sun Jun 21" group label above the
    ;; habit row); every OTHER conjunct in it — the one-fold-row ×7
    ;; chip, the latest-stamp epoch, the chip-less sibling — still
    ;; holds and stays green under R63.  NOT hand-blessed here — the
    ;; test seat re-blesses it to the flat-section contract (and adds
    ;; the R63 T7-T12 acceptance seams + the review-mockup-170 golden
    ;; via the regen-mockups discipline) and deletes this entry as
    ;; closeout.
    ;; 2026-07-20: ROUND-63 CLOSEOUT (impl tip pusntykn/ee638f3f + test
    ;; seat <this commit>).  The ONE grind entry CLOSED — the flagged
    ;; ERT re-blessed HONESTLY to the design-blessed R63-2a flat-section
    ;; contract (air/v0.5/org-air-round63-design.org, Item 2a), no
    ;; conjunct weakened (`make clean' FIRST — the R48/R51/R53/R54/R62
    ;; stale-.elc lesson bit AGAIN: the first fixture probe ran pre-R63
    ;; bytecode and rendered the grouped layout on the fixed tree):
    ;;   org-air-r61-12-completed-count-chip — the day-group ordering
    ;;     assert ("Sun Jun 21" group label precedes the habit row) is
    ;;     INVERTED to the flat-section contract: the weekday group
    ;;     label is asserted ABSENT from the rendered pane and no pane
    ;;     line is a bare weekday-date group label (the date cell alone
    ;;     carries the chronology); the one-fold-row ×7 chip, the
    ;;     latest-stamp epoch and the chip-less sibling conjuncts carry
    ;;     over verbatim.
    ;; GOLDEN/FIXTURE CHURN: exactly ONE NEW fixture —
    ;; tests/fixtures/review-mockup-170.txt, generated from the REAL
    ;; renderer over the spec's T7 mirror corpus (width 170, height 40,
    ;; frozen clock Sun 2026-07-19 W29, DEFAULT knobs incl. the default
    ;; `org-air-log-cap' — the rtrunc row carries a genuine over-cap
    ;; LOGBOOK) via `make clean && make regen-mockups' (anti-tautology
    ;; guards active; NO HANG, exit 0); every board/layout/project/
    ;; entry-view/denote golden is byte-identical (jj diff = the new
    ;; fixture + test .el files only).  The corpus + render harness live
    ;; in org-air-viewport-helpers.el (`org-air-viewport-test-review-
    ;; mockup-lines'), shared by the regen tool and the byte test — the
    ;; denote-golden discipline, no fork.
    ;; NEW R63 EXECUTING ERTs (tests/org-air-round63-test.el, all
    ;; batch/headless; the spec's T1-T12 seams; the rail ERTs drive the
    ;; window machinery under the R25 noninteractive-nil idiom with the
    ;; reconcile SYNCHRONOUS; revert of each FAILS — ALL TWELVE
    ;; verified RED against the pre-impl trunk muqylqul in a scratch
    ;; workspace: r63-1 red with SIX owner flips (one per slice — the
    ;; reported flicker, reproduced by the seam itself), r63-6 red on
    ;; the belt-1 handoff conjunct, r63-3/8 red on the R63
    ;; predicate/identity APIs (their neutrality + harvest-extents
    ;; halves are the permanent locks), the rest red on the pre-R63
    ;; behaviour by construction; the re-blessed r61-12 also verified
    ;; RED there — the inversion is not vacuous):
    ;;   r63-1 (T1) owner stability across a SIMULATED PACED FILL: board
    ;;     + review in two main windows, review selected + owning the
    ;;     rail, the board's cold refresh driven synchronously
    ;;     (`--refresh-start' + `--refresh-run-slice' one file per
    ;;     slice, progressive paints forced every slice) — after EVERY
    ;;     slice, paint and synchronous reconcile the owner is STILL
    ;;     review, the rail window live, ZERO owner flips, the rail
    ;;     bytes untouched, and `org-air-rail--render' is NEVER called
    ;;     with the board host (pins R63-1c: the R58 carve is
    ;;     subordinate — no content flip even windowless); the board
    ;;     ends self-suspended (belt 1's raising edge).
    ;;   r63-2 (T2) reconcile-timer count: the same drive with N rapid
    ;;     deferred `org-air-rail--reconcile' fires between slices —
    ;;     pending `org-air-rail--reconcile-run' timers in `timer-list'
    ;;     never exceed ONE (R27-1 S3 under load; R63 adds belts, not
    ;;     timers).
    ;;   r63-3 (T3) the gate truth table: active==self t; active==other
    ;;     nil; active==nil t; suspended self nil REGARDLESS (even
    ;;     active==self); a dead buffer never holds a claim —
    ;;     data-light, mode-initialised buffers + the real window tree.
    ;;   r63-4 (T4) a non-owner tail is INERT: with review owning the
    ;;     rail, a direct `org-air-rail--show' on the board returns nil,
    ;;     leaves window/owner/rail-bytes byte-identical and
    ;;     self-suspends the board; `--host-width' skips the window
    ;;     ensure (fingerprint unchanged, the board's rail-buffer local
    ;;     never set, width still measured); `--evict-foreign-rail'
    ;;     never sweeps (review's rail survives, review not suspended).
    ;;   r63-5 (T5) transfer + round-trip: `--show(review)' while the
    ;;     board owns ⇒ the board's suspended flag raised SYNCHRONOUSLY
    ;;     in the same call (belt 1) + owner review; selecting the board
    ;;     back + synchronous reconcile re-owns the board with its flag
    ;;     CLEARED (the R63-1d falling edge belongs to the reconciler)
    ;;     and review suspended by transfer.
    ;;   r63-6 (T6) neutrality: a LONE displayed board (active==self)
    ;;     renders through the tail exactly as today — rail live, owner
    ;;     board, never suspended, re-render + reconcile steady; same
    ;;     for a lone review (the R62 handoff unregressed).
    ;;   r63-7 (T7) one row per item, no echo: collapse OFF, all FOUR
    ;;     bases — the composed per-item sections contain ZERO `group'
    ;;     specs (item/agg/note only), each fixture heading yields
    ;;     exactly one row, no rendered line is a bare filename or a
    ;;     bare weekday-date, the denote row's origin cell reads the
    ;;     de-machined TITLE and the raw `20260…' ID never renders.
    ;;   r63-8 (T8) the no-double-count law + the harvest verdict: the
    ;;     parent/child same-title fixture scans to TWO items at
    ;;     DISTINCT (FILE . POS) with the child's log NOT on the parent
    ;;     (the extents law); no rendered per-item section holds two
    ;;     rows sharing an identity (collapse ON and OFF — the
    ;;     mechanical guard that turns any future harvest duplication
    ;;     RED); the one-heading done-log + CLOSED shape still folds to
    ;;     ONE stamp (the R61 stamps-win rule re-pinned).
    ;;   r63-9 (T9) mirror collapse: the T7 arithmetic — Completed
    ;;     exactly 2 rows, `▤ 3 files' / `▤ 2 files', canonical item =
    ;;     the TAGGED denote note's (FILE . POS) on the row's item/
    ;;     marker props with the member list on `org-air-review-mirrors'
    ;;     (no duplicate identities), header "2 done", rail Summary 2,
    ;;     NO ×chip on the mirrored single completion; knob nil ⇒ 5
    ;;     rows / "5 done" (old behaviour restored); a synthetic ×7
    ;;     habit keeps ×7 through collapse (stamp UNION deduped by
    ;;     epoch) with `▤ 2 files'; Started/Carried collapse under the
    ;;     same title×day key; Time invested totals byte-unchanged
    ;;     either way (time is attributed where clocked).
    ;;   r63-10 (T10) section headers: each of the four heading lines
    ;;     carries the icon glyph (✓ ◔ ▷ ↻ GUI / + %% > ~ TTY tier),
    ;;     the `org-air-section' property, the count chip
    ;;     (`org-air-count-badge'); `org-air-section-rule' t appends the
    ;;     rule line after every heading (board parity).
    ;;   r63-11 (T11) split cluster fits: spied through the shared V6
    ;;     `org-air-view--insert-row' — the item rows' date column
    ;;     equals the widest ITEM date (8, the `⚠' row) and NEVER the
    ;;     wider agg text (13); the agg rows compose their own
    ;;     (aw 0 0) fit; the rtrunc row's date cell is exactly
    ;;     "MMM D ⚠"-shaped with the loud phrase ONLY on the
    ;;     Time-invested note line.
    ;;   r63-12 (T12) the golden: the rendered review surface over the
    ;;     T7 corpus is byte-identical to review-mockup-170.txt
    ;;     (right-trimmed, trailing blanks dropped — the regen-mockups
    ;;     discipline, guards active).
    ;; No .el SOURCE touched on the test track (impl landed R63-1/2 in
    ;; pusntykn).  Round-63 manifest is EMPTY; the tests stay as
    ;; permanent regression guards.
    ;; =================================================================
    ;; v0.5 ROUND-64 impl grind (air/v0.5/org-air-round64-design.org).
    ;; R64-3 ONE-SHOT REFILE FORM: the R20-4 action-first refile menu and
    ;; its dispatcher (`org-air-inbox--refile-candidates',
    ;; `org-air-inbox--decode-target') are RETIRED — `r' (the command
    ;; NAME and binding preserved) now opens the transient
    ;; destination+metadata form `org-air-refile-transient', whose one
    ;; confirm executes ONE superset `org-air-refile-item' call (olp +
    ;; tags + schedule + category + todo + priority).  The retired menu's
    ;; actual GUARANTEES (short truncated title, real `⌂' file
    ;; candidates, decode-to-real-file, CRM category first-is-category-
    ;; rest-are-tags) survive in the batch-drivable `f'/`p'/`c' infix
    ;; readers (`org-air-inbox--read-target-file' /
    ;; `--read-target-path' / `--edit-categories') per spec seam T8.
    ;; The four legacy ERTs flagged in this grind drove the retired menu
    ;; SHAPE and died with it — the R64 spec names the R19-2 stub-chain
    ;; test as the deliberate retirement (R62-T14 style); the test seat
    ;; re-blesses/retires them (NOT hand-blessed on the impl track).
    ;;
    ;; 2026-07-21: ROUND-64 CLOSEOUT (impl tip uoywyzpm/f0c2cd0f + test
    ;; seat <this commit>).  ALL FOUR grind entries CLOSED — no fixture/
    ;; golden moved (the R64 changes live in the refile engine + form;
    ;; `make clean && make check' green with zero fixture churn — `make
    ;; clean' FIRST, the standing R48/R51/R53/R54/R62/R63 stale-.elc
    ;; lesson: the first probe of this round again ran pre-R64 bytecode).
    ;; Each flagged ERT re-blessed or retired HONESTLY against the
    ;; design-blessed R64 contract (air/v0.5/org-air-round64-design.org),
    ;; each retained guarantee asserted at its new home, none weakened;
    ;; the three re-blessed ones verified REVERT-FAIL against the
    ;; pre-impl trunk kklxowyq in a scratch workspace:
    ;;   org-air-r19-2-refile-prompt-shows-tags-and-move-relocates —
    ;;     RE-BLESSED in place to the one-shot API (the spec's NAMED
    ;;     deliberate retirement): the stub-chain no longer drives the
    ;;     retired action menu's prompt strings; the menu's actual
    ;;     guarantees are re-asserted through the form's batch seams —
    ;;     the SHORT truncated no-tag-block title on the form HEADER
    ;;     (`org-air-inbox--form-heading', tags still visible on their
    ;;     own pre-filled field), the real `⌂' candidates + decode-to-
    ;;     real-file in the `f' infix reader (driven with the DIRECTORY
    ;;     `org-air-files' bug-trigger config, unchanged), and the
    ;;     on-disk RELOCATION now through the form's execute suffix
    ;;     (ONE `org-air-refile-item' call, tags riding along, the
    ;;     destination recorded for the `l' recall).
    ;;   org-air-r20-4-refile-menu-is-action-first-move-leads — REMOVED
    ;;     (the one true retirement): the action-first menu SHAPE it
    ;;     pinned (`--refile-candidates' ordering, `Schedule: …' quick
    ;;     rows, no-#tag-soup) has NO successor — the transient form
    ;;     shows every field at once, so there is no action list left
    ;;     to lead.  Each surviving guarantee is asserted at its new
    ;;     home (justification comment left in its place in
    ;;     tests/org-air-round20-test.el): `⌂' candidates/decode in the
    ;;     re-blessed r19-2 + the renamed move-target test, the CRM
    ;;     category semantics in the re-blessed `c'-reader test, the
    ;;     schedule quicks in `org-air-inbox--schedule-options', the
    ;;     command/binding contract in r64-7 (seam T8).
    ;;   org-air-r20-4-decode-category-first-is-category-rest-are-tags —
    ;;     RE-BLESSED in place: `--decode-target' is gone; the SAME
    ;;     first-is-category-rest-are-tags + pre-filled-CRM conjuncts
    ;;     now drive the form's `c' suffix (`org-air-refile-form-
    ;;     category' over the unchanged `--edit-categories'): FIRST
    ;;     pick → the form's `:category', extras merged onto `:tags',
    ;;     the item's current tags kept — nothing lost.
    ;;   org-air-r20-4-decode-move-routes-to-read-move-target —
    ;;     RE-BLESSED + RENAMED `org-air-r20-4-move-target-rehomes-in-
    ;;     file-and-path-readers' (never silent: the old name named the
    ;;     retired router): the `f' reader resolves the `⌂' candidate
    ;;     to the REAL file (never the item's own — the original move
    ;;     bug), the `p' reader completes over that ONE file's real
    ;;     outline table (existing → `:new' 0, typed-beyond → counted
    ;;     creations) and the prompt mutates NOTHING on disk (the
    ;;     R64-2 defer-to-execute ruling).
    ;; NEW R64 EXECUTING ERTs (tests/org-air-round64-test.el, all
    ;; batch/headless driving the NON-INTERACTIVE `org-air-refile-item'
    ;; + the named reader seams — the transient UI polish is the
    ;; round's one user-confirm residue; ALL EIGHT verified REVERT-FAIL
    ;; against the pre-impl trunk kklxowyq in a scratch workspace):
    ;;   r64-1 (T1/T2) nested parents created when absent (‘* Infra’ ›
    ;;     ‘** Cloud’, exactly one of each, keyword-less R59
    ;;     containers, no timestamp), item under the deepest; the same
    ;;     path AGAIN creates nothing (last-child rule); a third call
    ;;     files a CHILD under a heading a prior call moved into place
    ;;     (n8n › daily automation); the on-disk tree asserted exactly,
    ;;     the emptied `* New' container left in the inbox.
    ;;   r64-2 (T5) the acceptance mirror: the worked example's THREE
    ;;     engine calls verbatim (syncthing → Infra/Cloud created +
    ;;     tag + "+1w", n8n → same place, daily automation → CHILD of
    ;;     n8n) ⇒ the exact final tree modulo the timestamp, every item
    ;;     re-leveled parent+1 (2→3, 2→4), exactly ONE SCHEDULED stamp
    ;;     under syncthing, the `inbox' tag gone from all three.
    ;;   r64-3 (T4) ONE call sets olp + tags + category + schedule +
    ;;     todo + priority together — all six asserted on-disk via a
    ;;     fresh org parse (DONE, [#B], both tags, :CATEGORY:,
    ;;     SCHEDULED 2026-08-01, ("Ops" "Weekly") ancestry, level 3).
    ;;   r64-4 (T3) the MEASURED sibling-level bug is dead: the level-2
    ;;     `* New'-container item refiled "under" level-2 `Cloud' lands
    ;;     level 3 as Cloud's LAST child (never its sibling), and an
    ;;     item with its OWN child re-levels both (2/3 → 4/5) under a
    ;;     level-3 parent — string AND path targets.
    ;;   r64-5 (T6, R53 re-pin) pickers never scan: seeded board-items
    ;;     cache ⇒ the `f' candidates, the tag/category vocabs (through
    ;;     the REAL CRM editors) and the R64-2 path table build under
    ;;     spies on `org-air-query--scan-file' / `org-air-query-items'
    ;;     / `-in-files' / `org-air-query-files' at ZERO calls; the
    ;;     path stage opens exactly ONE file — the destination
    ;;     (`find-file-noselect' spy, distinct files); the vocab/table
    ;;     content is real cached data (anti-tautology); the cache
    ;;     fallback leg still scans nothing.
    ;;   r64-6 (T7) signature compatibility: `(item file)' ⇒ file end
    ;;     under the `#+title' prose (the R53-6 call shape re-run; nil
    ;;     paste level 1 = the called-out promotion); `"Existing"' ⇒
    ;;     filed AT child level (string ≡ one-segment path);
    ;;     `"Missing"' ⇒ CREATED at top level — the silent file-end
    ;;     fallback is retired.
    ;;   r64-7 (T8) the shell's batch-safe parts: `org-air-refile-item'
    ;;     stays a command + the board's `r' binding;
    ;;     `org-air-refile-transient' is a command; the `p' resolver is
    ;;     pure over fixture tables ("A/B" → (:olp ("A" "B") :new 2),
    ;;     trailing `/' tolerated, partial chains count the missing
    ;;     tail, empty → :olp nil, a `/'-NAMED heading matches as a
    ;;     table entry FIRST); the `f' reader resolves a DISAMBIGUATED
    ;;     `⌂' twin to the real file (the R19-2 guarantee re-homed).
    ;;   r64-8 (T9) the todo vocabulary is the DESTINATION file's OWN
    ;;     merged keywords (`#+TODO: TODO HOLD | CLOSED' ⇒ exactly
    ;;     those three), "CLOSED" round-trips on-disk and `donep'
    ;;     follows the file's own done set (moved item done, the HOLD
    ;;     sibling not) — the R57 law.
    ;; No .el SOURCE touched on the test track (impl landed R64-1..3 in
    ;; uoywyzpm).  Round-64 manifest is EMPTY; the tests stay as
    ;; permanent regression guards.
    ;; =================================================================
    ;; v0.1 ROUND-69 impl grind (air/v0.1/org-air-round69-design.org) —
    ;; goldens/assertions that change BYTES under the design-blessed
    ;; R69-1 Filter→Source spacer, R69-2 dropped rail ✕, R69-4 Actions
    ;; reflow and R69-5 token-label switch.  Per the impl brief these
    ;; are FLAGGED, not hand-blessed — the test seat regenerates the
    ;; fixtures (`make regen-mockups', frozen clock, guards active) and
    ;; re-blesses the assertion ERTs.  A scratch regen was diffed to
    ;; characterise the classes (fixtures then RESTORED byte-identical):
    ;;   (a) R69-1 spacer: every side-rail golden gains ONE blank line
    ;;       between the Filter block and the `▌ Source' header.
    ;;   (b) R69-4 reflow: rails whose Actions block TRUNCATED today
    ;;       (the mid-tier ~32-col board rail's `s sou…', the 28-col
    ;;       project rail's `/ ...') reflow 3→2 columns — more rows,
    ;;       no ellipsis; wide-tier Actions are byte-identical (pinned
    ;;       by org-air-r69-4-*-parity-byte-identical).  Natural-height
    ;;       goldens grow by the extra row(s) + the spacer line.
    ;;   (c) R69-5 token-label: the banner/project header filter
    ;;       segments now route through `--filter-token-label', so a
    ;;       BARE token reads quoted (`"work"'), no longer falsely
    ;;       tag-dressed (`#work'); `#tag' tokens are unchanged.
    ;;
    ;; 2026-07-22: ROUND-69 CLOSEOUT (impl tip zxrxlzyo/25d4a0ca + test
    ;; seat <this commit>).  ALL 12 grind entries CLOSED — the goldens
    ;; regenerated from impl's render via the FROZEN-CLOCK renderer
    ;; (`make clean' FIRST — the standing R48/R51/R53/R54/R62/R63/R64
    ;; stale-.elc lesson — then `make regen-mockups', anti-tautology
    ;; guards active; NO HANG, exit 0).  AUDIT: jj diff = EXACTLY 27
    ;; fixtures, every hunk inside the blessed classes and NOTHING else:
    ;;   (a) R69-1 spacer — ONE blank line between the Filter block and
    ;;       the `▌ Source' header in every side-rail golden (incl. the
    ;;       90-tier stacked top-band's Filter column and the review/
    ;;       denote rails); fixed-height x50 goldens absorb it in the
    ;;       blank tail (total height constant), natural-height goldens
    ;;       grow by exactly the extra line(s).
    ;;   (b) R69-4 reflow — Actions reflow 3→2 columns EXACTLY where the
    ;;       old goldens truncated: the mid-tier board rails' `s s…'/
    ;;       `s sou…'/`? h…' become 3×2 rows with all six verbs whole,
    ;;       and the 28-col project rails' `/ ...'/`| ...'/`q ...' become
    ;;       5×2 rows with all NINE verbs whole; the wide 160-tier
    ;;       Actions block is byte-identical (the T5 parity law).
    ;;   (c) NO fixture renders an active filter, so classes R69-2/R69-5
    ;;       move zero golden bytes — confirmed zero ✕/token drift.
    ;; The 8 byte-golden tests (layout-mockup-120/-160/-heights/
    ;; -thresholds, f5-project-view-byte-mockups, r17-denote-origin,
    ;; r63-12-review-mockup-golden, r49-5-batch-placement-blind) pass on
    ;; the regen'd fixtures with NO assertion edit.  The 4 assertion ERTs
    ;; re-blessed HONESTLY to the design-blessed R69 contracts
    ;; (air/v0.1/org-air-round69-design.org), none weakened:
    ;;   org-air-r18-dp2-banner-shows-combinator — bare tokens now read
    ;;     QUOTED in the banner (`"work" AND "home"' / `"work" OR
    ;;     "home"' / single `"work"') per the R24-6 token contract;
    ;;     GAINED the anti-dressing pin (`#work' ABSENT from the
    ;;     banner); the combinator-word + single-token-no-combinator
    ;;     conjuncts carry over verbatim.
    ;;   org-air-r18-dp3-project-filter-narrows — the project header's
    ;;     bare `ui'/`core' tokens read quoted (`"ui" AND "core"',
    ;;     `"ui" OR "core"'); GAINED the `#ui AND #core' ABSENT pin;
    ;;     every narrowing/membership/fold-bypass conjunct verbatim.
    ;;   org-air-r19-4-clear-hint-shows-clear-key-when-filter-active —
    ;;     the `#work AND #client' conjunct (the banner's hand-prepended
    ;;     `#' on BARE tokens; the rail already read them quoted)
    ;;     re-blessed to `"work" AND "client"' + the dressed join pinned
    ;;     ABSENT; both hint-verb conjuncts verbatim.
    ;;   org-air-r26-3-legend-on-screen-popped — the impl FINDING
    ;;     honoured: at the 24-line CLAMPED side rail the inspector had
    ;;     ALREADY reached zero, so the spacer pushes the first verb row
    ;;     one line below the fold (verb rows 2+ were below the fold
    ;;     pre-R69 too).  The guarantee SPLITS, never weakens: body 24
    ;;     still keeps the Actions HEADER within the fold, and body 25
    ;;     (frame +1) carries the ORIGINAL header+`RET open' law at the
    ;;     minimum height where it is now structurally achievable.
    ;; The 16 new R69 ERTs (tests/org-air-round69-test.el, T1–T7)
    ;; independently audited on the test seat: all 16 green, and the
    ;; T1–T4/T6–T7 seams verified REVERT-RED against the pre-impl trunk
    ;; nwptkmykkoxz in a scratch workspace (13 RED; the 3 green are
    ;; exactly the by-design pins — T5's two parity byte-pins + the
    ;; T7 fold regression pin).  The audit found FOUR uncovered seams;
    ;; the test seat added gap ERTs for each (all verified revert-RED,
    ;; the revisit parity leg green there by design):
    ;;   r69-4-actions-single-column-floor — the reflow ladder's n=1
    ;;     rung (only 3→2 was driven): board Actions at 20w = six
    ;;     single-cell rows, nothing truncated.
    ;;   r69-4-revisit-actions-parity-and-reflow — the REVISIT emitter
    ;;     (the one collapsed call site with neither an ERT nor a
    ;;     golden): 32w byte-identical to the pre-consolidation 3×3
    ;;     (pinned from the pre-R69 tree's actual bytes), 24w reflows
    ;;     to 2 cols with all nine verbs whole.
    ;;   r69-5-banner-scope-segment-dedup — the impl-found BANNER
    ;;     scope straggler (the exact screenshot scenario `board
    ;;     scoped to #nix'): `#nix' once, plain `nix' zero-shift.
    ;;   r69-5-inspector-and-project-surface-dedup — the board
    ;;     inspector tags line, project inspector tags line and
    ;;     project by-tag section titles: `#nix' once, `#plain'
    ;;     still prefixed.
    ;; No .el SOURCE touched on the test track (impl landed
    ;; R69-1..5 in zxrxlzyo).  Round-69 manifest is EMPTY; the tests
    ;; stay as permanent regression guards.
    ;; =================================================================
    ;; v0.1 ROUND-70 impl grind (air/v0.1/org-air-round70-design.org).
    ;; R70-1 REBIND, design-blessed: the board's editor entry moved
    ;; `r' → `e' (`org-air-refile-item' — the command NAME stays; `r'
    ;; DROPPED, no alias, per Decision 1) and the `?' help Triage row
    ;; reframed to the edit wording (key derived via `where-is', so the
    ;; row now reads `e  edit item (a destination refiles)').  The
    ;; spec's five flip-pins (triage-test key table + lookup/help row,
    ;; r26-6, r35 knob legs, r67 heading string) were design-listed for
    ;; the impl seat and are updated in this commit.  The impl audit
    ;; found THREE MORE exact pins of the OLD framing, not in the
    ;; spec's table — FLAGGED here per the brief, NOT hand-blessed; the
    ;; test seat re-blesses them to the R70-1 contract and deletes
    ;; these entries as closeout.  ZERO byte goldens moved (`make
    ;; regen-mockups' byte-clean — no fixture renders the transient,
    ;; the help buffer, or an Actions refile cell, exactly as the spec
    ;; predicts).
    ;;
    ;; 2026-07-22: ROUND-70 CLOSEOUT (impl tip lotzoxnt/d804378b + test
    ;; seat <this commit>).  ALL THREE grind entries CLOSED — no
    ;; fixture/golden moved (the R70 changes are one keymap row, help/
    ;; heading strings and the note machinery; `make clean && make
    ;; check' green with zero fixture churn — `make clean' FIRST, the
    ;; standing R48/R51/R53/R54/R62/R63/R64 stale-.elc lesson).  Each
    ;; flagged ERT re-blessed HONESTLY to the design-blessed R70-1
    ;; contract (air/v0.1/org-air-round70-design.org), none weakened —
    ;; each still MEANS something and each gained the anti-revert `r'
    ;; conjunct (a restored `r' binding turns it RED again):
    ;;   org-air-ux-keys-refile — the §9 verb-key pin flips to `e'
    ;;     (`(key-binding (kbd "e"))' is `org-air-refile-item' on the
    ;;     live dashboard) + the anti-revert conjunct (`r' no longer
    ;;     reaches the command — dropped, no alias, Decision 1).
    ;;   org-air-r64-7-shell-binding-and-pure-readers — ONLY the one
    ;;     binding conjunct moved (`r' → `e' on the live board keymap,
    ;;     + the `r'-gone anti-revert pin); the command/transient/pure
    ;;     `p'-resolver/`f'-reader conjuncts carry over verbatim — the
    ;;     T8 shell contract itself is untouched by the rebind.
    ;;   org-air-r50-2-help-opens-buffer-from-board — the one help-row
    ;;     regexp re-blessed to the derived `^  e +edit' Triage row
    ;;     (edit wording, `where-is' key) + `^  r +refile' pinned
    ;;     ABSENT; the buffer/window/mode/`g r' conjuncts verbatim.
    ;; NEW R70 EXECUTING ERTs (tests/org-air-round70-test.el, the
    ;; spec's ten seams r70-1..r70-10 + two audit-gap ERTs, all
    ;; batch/headless through the r19/r64/r67 form idiom — no transient
    ;; event loop; the note core driven UNMOCKED through org's own
    ;; `org-add-log-setup' + synchronous `org-store-log-note'):
    ;;   r70-1 rebind (e → `org-air-refile-item', r → nil, batch
    ;;     dispatch reaches the transient's own interactive-only guard),
    ;;   r70-2 `where-is' determinism (exactly ONE binding, [?e]),
    ;;   r70-3 help shows `e  edit…' / never `r  refile',
    ;;   r70-4 transient layout (S-<return> → `org-air-refile-form-note',
    ;;     :transient t; RET/q unchanged),
    ;;   r70-5 the note core's exact dated shape (no *Org Note*, hook
    ;;     clean, `org-log-setup' nil),
    ;;   r70-6 drawer honoured (global knob + the file's own #+STARTUP:
    ;;     logdrawer in the SAVED bytes),
    ;;   r70-7 the wrapper: in place, saved, heading byte-identical, no
    ;;     move, triage-undo source recorded, cache-cold (FILE . POS),
    ;;   r70-8 NOT suppressed by the R68 discipline (org-inhibit-logging
    ;;     'note + inside `org-air-view--at-item-source', exactly ONE
    ;;     note line, the post-body flush no-ops),
    ;;   r70-9 atomicity (a signalling store rolls back byte-exactly,
    ;;     no save),
    ;;   r70-10 the suffix (writes via minibuffer read, form state
    ;;     SURVIVES with fields untouched, empty read is a no-op
    ;;     message + zero bytes moved, then note + refile COMPOSE: the
    ;;     note text travels with the subtree to the target).
    ;;   AUDIT GAPS (the test seat's own): r70-11 note + IN-PLACE
    ;;     metadata edit in one session (the compose leg the spec's
    ;;     r70-10 only drove through the refile leg) — both land, no
    ;;     move, one note line; r70-12 two S-RETs = two dated notes
    ;;     (the Decision-5 journaling law, batch-pinned).
    ;; All 12 verified green against impl tip lotzoxnt, and ALL 15
    ;; touched ERTs (the 12 new + the 3 re-blessed) verified
    ;; REVERT-RED against the pre-impl trunk zlymnnmo in a scratch
    ;; workspace — r70-9 initially passed there VACUOUSLY (its
    ;; `should-error' swallowed the void-function error) and was
    ;; tightened to pin the stubbed store's OWN signal, closing the
    ;; vacuity.  No .el SOURCE touched on the test track (impl landed
    ;; R70-1/2 in lotzoxnt).
    ;; Round-70 manifest is EMPTY; the tests stay as permanent
    ;; regression guards.
    ;; =================================================================
    ;; v0.1 ROUND-72 impl grind (air/v0.1/org-air-round72-design.org).
    ;; R72 Decision 4 MEMO-KEY EXTENSION: the classify memo key
    ;; `org-air-view--classify-cache-day' grows from the bare
    ;; `time-to-days' integer to the pair (DAY . EFFECTIVE-HORIZON) — an
    ;; active `due:Nd/Nw' window filter token WIDENS the Upcoming
    ;; horizon, so the key must carry the horizon dimension (toggling a
    ;; window filter rebuilds the memo instead of serving stale buckets;
    ;; a NO-OP when the horizon is unchanged — `due:7d' at defaults keys
    ;; identically to no filter, pinned by r72-9).  The ONE pre-R72 ERT
    ;; below hardcodes the old integer shape at both ends of its
    ;; midnight-rollover walk: it stamps the cache day with the bare
    ;; integer `yesterday' and asserts `(eql --classify-cache-day
    ;; today)' after the repaint.  The rollover CONTRACT itself survives
    ;; byte-exact (an `equal' mismatch on the old integer still rebuilds
    ;; — day change detection carries; the repaint tick conjunct is
    ;; untouched); only the key-SHAPE assert moved.  NOT hand-blessed on
    ;; the impl track per the brief — the test seat re-blesses the final
    ;; assert to the (DAY . HORIZON) pair (cdr = the default horizon 7
    ;; on an unfiltered board) and deletes this entry as closeout.  ZERO
    ;; byte goldens moved (`make regen-mockups' byte-clean — no fixture
    ;; renders an active filter, exactly as the spec predicts).
    ;;
    ;; 2026-07-22: ROUND-72 CLOSEOUT (impl tip skvpxswu/e1c4c4a6 + test
    ;; seat <this commit>).  The ONE grind entry CLOSED — no fixture/
    ;; golden moved (`make clean && make check' green with zero fixture
    ;; churn — `make clean' FIRST, the standing R48/R51/R53/R54/R62/R63/
    ;; R64 stale-.elc lesson).  The flagged ERT re-blessed HONESTLY to
    ;; the design-blessed R72 Decision 4 memo-key contract
    ;; (air/v0.1/org-air-round72-design.org), no conjunct weakened:
    ;;   org-air-r42-f2-no-change-repaints — the stale midnight stamp now
    ;;     uses the NEW (DAY . EFFECTIVE-HORIZON) pair shape with
    ;;     yesterday's day (so the rebuild is driven by the DAY rollover
    ;;     alone, never a shape mismatch — the rollover conjunct stays
    ;;     non-vacuous), and the post-render assert moves from the
    ;;     bare-integer `(eql … today)' to
    ;;     `(equal … (cons today org-air-upcoming-days))' — the
    ;;     unfiltered board's effective horizon IS the knob, so the cdr
    ;;     is pinned too.  The repaint tick conjunct and every other
    ;;     no-change/single-swap conjunct carry over verbatim.
    ;; The 12 new R72 ERTs (tests/org-air-round72-test.el, the spec's
    ;; seams r72-1..r72-12) independently audited on the test seat: all
    ;; 12 green against impl tip skvpxswu, and ALL touched ERTs (the 12
    ;; + the 4 gap ERTs below + the re-blessed r42-f2) verified against
    ;; the pre-impl trunk mvutqotu in a scratch workspace: 16 of 17 RED;
    ;; the one green is r72-8-no-rescan, which is a LOCK, not a fence —
    ;; the pre-R72 filter fold never scanned either (the R53 law the
    ;; test pins is a carried-over invariant; the spec's "each
    ;; revert-RED" was optimistic for exactly this seam — same class as
    ;; the R56-7/R69-T5 by-design pins).  The audit found FOUR
    ;; uncovered seams; the test seat added gap ERTs for each (in the
    ;; same file):
    ;;   r72-13-overdue-agrees-with-attention-members — the crux driven
    ;;     at the ATTENTION bucket: `is:overdue' ⇔ (attention-member ∧
    ;;     `--overdue-p') over EVERY fixture item — every filter hit has
    ;;     a home row in Needs-attention, and the dateless/hipri/stale
    ;;     attention members are exactly the non-overdue remainder.
    ;;   r72-14-due-2w-widens-and-shows — the user's literal ask with
    ;;     the literal `due:2w' TOKEN (r72-9 drove `due:14d'):
    ;;     `due:2w' ≡ `due:14d' set-equal, past stays OUT of the wide
    ;;     window, and the +10d item is both MATCHED (visible) and SHOWN
    ;;     (an Upcoming bucket member of the partition table) under a
    ;;     widened 14-day horizon.
    ;;   r72-15-rail-filter-line-renders-tokens — the rail Filter block
    ;;     with `is:overdue,#work' live: the lens line reads
    ;;     `is:overdue AND #work' (parsed token verbatim-unquoted), the
    ;;     `Match: AND' line renders, the OR flip renders `is:overdue OR
    ;;     #work' + `Match: OR', and an unparsed `is:urgent' renders
    ;;     QUOTED in the same rendered line (the tell, on the surface).
    ;;   r72-16-interactive-filter-apply-no-rescan — the R53 law at the
    ;;     COMMAND seam (r72-8 drove the bare fold): `org-air-filter'
    ;;     apply with date tokens on a warm cons-marker board re-renders
    ;;     through the cached items with the scan layer +
    ;;     `find-file-noselect' spied at ZERO calls — and the render is
    ;;     REAL (the repaint tick advances, the widened-horizon memo key
    ;;     lands, clearing restores the default key).  AUDIT FINDING
    ;;     while driving it: the rail INSPECTOR's CREATED hydration
    ;;     (`org-air-view--item-created', the design-sanctioned bounded
    ;;     one-file probe for the single inspected item) fires on any
    ;;     render with an item at point — pre-R72 behaviour, not the
    ;;     filter's doing; the ERT binds `org-air-show-inspector' nil so
    ;;     the ZERO-opens assert pins the R72 contract exactly.
    ;; All 16 + the re-blessed r42-f2 green; `make clean && make check'
    ;; green (0 unexpected), goldens byte-consistent (zero fixture
    ;; churn).  No .el SOURCE touched on the test track (impl landed
    ;; R72-1..3 in skvpxswu).  Round-72 manifest is EMPTY; the tests
    ;; stay as permanent regression guards.
    ;; =================================================================
    ;; v0.1 ROUND-75 impl note (air/v0.1/org-air-round75-design.org) —
    ;; REDO for the recent-edits ring: `U' (`org-air-edit-redo', the
    ;; shift-pair inverse of `u') + the global
    ;; `org-air-view--edit-redo-ring' fed by `u''s success branch,
    ;; cleared by every fresh edit inside `--edit-ring-push'.  The
    ;; spec's ONE predicted golden/mockup shift — a board-help mockup
    ;; pinning the Triage table gaining the always-rendered
    ;; `U  redo last undo (a new edit clears redo)' row (+ the
    ;; process-inbox `[U]redo' prompt token) — did NOT materialise:
    ;; NO golden and NO pre-R75 ERT renders the board help buffer's
    ;; full Triage table or the process-inbox prompt (`make
    ;; regen-mockups' byte-clean, zero fixture churn — the R74
    ;; header-✕ precedent verbatim).  Flagging a passing test would
    ;; itself fail the binary gate (a listed test PASSING is
    ;; unexpected), so the manifest carries this NOTE and no entry;
    ;; the round's coverage is the eleven executing r75 seams
    ;; (tests/org-air-round75-test.el) + the 16 preserved r73 seams
    ;; (the r73 fixture gained ONLY an `org-air-view--edit-redo-ring'
    ;; nil isolation binding — no assertion touched).  GUI residue
    ;; (not ERT-able, flag for user confirm): the visible board/pane
    ;; repaint right after `U' (the R73-1 resync class) and the
    ;; process-inbox prompt readability with the new token.
    ;; Round-75 manifest is EMPTY.
    ;; =================================================================
    ;; v0.1 ROUND-77 impl grind (air/v0.1/org-air-round77-design.org):
    ;; `org-air-task-requires-todo' (default nil — R54 D1 preserved,
    ;; knob-off byte-identical) joins `org-air-view--cache-key' as the
    ;; SEVENTH element (the R59/R60/R61 precedent — the knob shapes
    ;; scan-time `ntype'/file-meta `:ntype', so a flip must invalidate
    ;; like a vocabulary change).  The THREE prior-round cache-key
    ;; goldens below pin the pre-R77 key LENGTH literally (`(= (length
    ;; key) 6)`) and fail on exactly that assertion — the spec's
    ;; predicted shifted-golden class (every OTHER assertion in each
    ;; still passes; the r60-6 five-element/`nth 4' and r61-8 sixth-
    ;; element/`nth 5' positional claims are UNCHANGED since R77
    ;; appends).  TEST SEAT: re-bless the length assertion in each to 7
    ;; (and r59-13's element-count prose), then delete these entries.
    ;;
    ;; 2026-07-23: ROUND-77 CLOSEOUT (impl tip qrlkzkro/529f45c1 + test
    ;; seat <this commit>).  ALL THREE grind entries CLOSED — no
    ;; fixture/golden moved (`make clean && make check' green with zero
    ;; fixture churn — `make clean' FIRST: the standing R48/R51/R53/
    ;; R54/R62/R63/R64 stale-.elc lesson bit AGAIN; the first probe of
    ;; this round ran pre-R77 bytecode and all three re-blessed ERTs
    ;; failed with (= 6 7) on the FIXED tree — which doubled as a
    ;; genuine revert-RED check: each re-blessed test fails against the
    ;; pre-R77 key).  Each flagged ERT re-blessed HONESTLY to the
    ;; design-blessed R77 7-element-key contract
    ;; (air/v0.1/org-air-round77-design.org, Cache coherence), no
    ;; conjunct weakened — each still DETECTS its own element AT ITS
    ;; OLD SEAT and GAINED the seventh-element conjuncts:
    ;;   org-air-r59-13-cache-v5-and-key — key length 6 -> 7; the
    ;;     R59-knob (nth 3), exclude-set (nth 4) and log-cap (nth 5)
    ;;     detection + hydrate/miss + v4-cold-miss conjuncts kept
    ;;     verbatim; GAINED the seventh-element conjuncts (the live
    ;;     `org-air-task-requires-todo' is (nth 6), nil at the default,
    ;;     tracks a let-bound t, detects a flip as a different key) and
    ;;     the knob hydrate fence (a cache written under the nil
    ;;     default never hydrates under the knob — the baked
    ;;     ntype/file-meta :ntype split — while nil still does).
    ;;   org-air-r60-6-exclude-set-is-fifth-cache-key-element — key
    ;;     length 6 -> 7 with the exclude set asserted UNMOVED at
    ;;     (nth 4) and the cap at (nth 5); every pairwise-distinct /
    ;;     hydrates-under-A / never-under-B-or-nil conjunct kept;
    ;;     GAINED the (nth 6) knob tracking + flip detection, and the
    ;;     crafted short-key misses now cover ALL THREE legacy shapes
    ;;     (pre-R60 4-element, pre-R61 5-element, pre-R77 6-element),
    ;;     each missing on length alone.
    ;;   org-air-r61-8-cache-v6-and-no-rescan-nav — key length 6 -> 7
    ;;     with the cap asserted UNMOVED at (nth 5); every cache-v6 /
    ;;     v5-cold-miss / zero-rescan-nav / warm-byte-parity conjunct
    ;;     kept verbatim; GAINED the (nth 6) knob tracking + flip
    ;;     detection + the knob hydrate fence, and the crafted
    ;;     short-key misses now cover BOTH legacy shapes (pre-R61
    ;;     5-element AND pre-R77 6-element).
    ;; The 12 R77 acceptance ERTs (tests/org-air-round77-test.el,
    ;; r77-1..r77-12) independently audited on the test seat: all 12
    ;; green against impl tip qrlkzkro.  The audit found THREE
    ;; uncovered seams; the test seat added gap ERTs for each (same
    ;; file, all green on the fixed tree, the knob-sensitive ones RED
    ;; by construction on pre-R77 bytecode):
    ;;   r77-13-deadline-routine-and-journal-flavour — the step-4
    ;;     gate's DEADLINE disjunct (r77-1 drove only SCHEDULED —
    ;;     reverting the deadline half alone was uncaught) + the D2
    ;;     step-5 journal flavour (a keyword-less routine in a
    ;;     journal-typed file demotes to `journal', not knowledge —
    ;;     off Revisit under the default `org-air-revisit-types') +
    ;;     the routed filter gate's JOURNAL leg (r77-4/5 drove only
    ;;     knowledge).
    ;;   r77-14-archived-unaffected — the round ask's "donep/archived
    ;;     unaffected" ARCHIVED half (r77-6 drove only donep): an
    ;;     `org-archive-tag'-tagged keyworded heading types task and
    ;;     classifies into ZERO buckets under the knob ON and OFF
    ;;     alike, and never matches is:overdue either way.
    ;;   r77-15-revisit-surface-renders-demoted-file — the r77-8
    ;;     reachability claim driven at the actual SURFACE (r77-8
    ;;     stopped at `org-air-revisit--scope-entries'): under the knob
    ;;     the REAL `org-air-revisit' view renders the pure-routines
    ;;     file as a row (the rail notes count includes it) — the
    ;;     routine is demoted, NOT lost; also pins the D7 MIXED-file
    ;;     wrinkle as specced (one demoted routine flips a mixed file's
    ;;     F7 vote to knowledge — the standing R54 F7 fork, flagged
    ;;     not re-ruled).
    ;; AUDIT NOTE (spec prose, no code/test impact): D7's parenthetical
    ;; claims a `#+type: task' FILE keyword re-types only the FILE
    ;; ("its headings STILL demote per-heading") — but `#+type:' is
    ;; step 2 of the R54-2 heading chain, so it forces every heading
    ;; in the file back to task (exactly what the spec's OWN D2 lists
    ;; among the per-item force-backs, and what r77-5 exercises in the
    ;; note direction).  The code follows D2/R54-2; the D7 asymmetry
    ;; sentence is unimplementable as written — flagged for crosstalk,
    ;; not re-ruled here.  No .el SOURCE touched on the test track
    ;; (impl landed R77 in qrlkzkro).  Round-77 manifest is EMPTY; the
    ;; tests stay as permanent regression guards.
    ;;
    ;; ===================================================================
    ;; v0.5 ROUND-79 IMPL GRIND (day-view repair; impl landed on a fresh
    ;; child of mnnunppoqlmw — D1 badge face split, D2 fixed badge column
    ;; alignment, D3 keyword filter axis, D4 o/O day sorting).  ONE
    ;; existing golden shifts under D1 and is flagged for the test seat to
    ;; RE-BLESS (do NOT hand-bless here):
    ;;
    ;;   R79-D1 KEYWORD-BADGE FACE SPLIT: `org-air-view--todo-face' now
    ;;     routes each keyword through its OWN face.  The DONE family
    ;;     splits — completions (DONE/COMP/COMPLETED) keep
    ;;     `org-air-face-done' (faded blue) while the cancelled/abandoned
    ;;     set (DROPPED/DROP/CANCELLED/CANCELED/KILL/KILLED) reads the new
    ;;     `org-air-face-dropped' (muted terracotta), and unknown keywords
    ;;     resolve through the R57 merged scan vocabulary instead of the
    ;;     blanket donep fallback — so COMP/DROPPED/READY/WIP each read
    ;;     distinctly (defect D1).  The default `own' source keeps R57-1
    ;;     and the BOARD goldens byte-identical (faces don't move the byte
    ;;     layer); the ERT below hard-wired the pre-R79 collapse
    ;;     (DROPPED/DROP => `org-air-face-done') and must re-bless to the
    ;;     R79 split (DROPPED/DROP => `org-air-face-dropped'; COMP/DONE
    ;;     stay `org-air-face-done'):
    (org-air-r57-9-donep-aware-todo-face
     . "R79-D1: DROPPED/DROP now => org-air-face-dropped (done family split); \
re-bless the pre-R79 face-collapse assertions on the test seat.")
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
