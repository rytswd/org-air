;;; org-air-round69-test.el --- executing ERTs for round-69 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for round-69 (air/v0.1/org-air-round69-design.org):
;; five screenshot-confirmed rail polish items — R69-1 the missing blank
;; line between the rail Filter and Source sections (the one-blank
;; inter-section spacer inside `org-air-view--insert-rail-filters');
;; R69-2 the dead ✕ clear glyph dropped from the rail chips line (it
;; carried no action; the `M-/ toggles ∙ \ clears' hint line teaches
;; both verbs); R69-3 `org-air-filter-match' added to the rail input
;; STAMP (`org-air-rail--input-stamp') so M-/ busts the S4 skip guard
;; exactly like `/' and `\' do; R69-4 the fit-driven Actions verb-row
;; reflow (ONE shared emitter `org-air-view--insert-verb-rows',
;; 3→2→1 columns — the four copy-pasted per-view emitters collapse
;; into it, byte-identical where 3 columns fit, never a truncated
;; verb where they do not); R69-5 the `org-air-view--tag-chip-label'
;; prefix-dedup primitive applied at every tag-NAME chip surface
;; (a literal `:#Nix:' org tag renders `#Nix', never `##Nix'), the two
;; token-label stragglers (banner + project header) routed through the
;; R24-6 `org-air-view--filter-token-label', and the matcher's tag
;; branch widened to hit a VERBATIM `#'-named tag (so the deduped
;; chips stay clickable/filterable).
;;
;; All BATCH/headless: rendering seams run the block functions into a
;; temp buffer with the relevant buffer-locals let-bound (the P1/P2
;; probe idiom) — no window, no fixture scan.  Batch glyphs are the
;; ASCII tier (✕ → `x', … → `...').  The spec's seams T1–T7 map onto
;; the ERTs below (T1–T4, T6–T7 revert-RED against the pre-R69 tree;
;; T5 is the consolidation parity guard, green today BY DESIGN).
;;
;; GUI-confirm residue (per spec, NOT ERT-able): the live side-window
;; repaint on the actual M-/ keypress, the spacer's visual rhythm, svg
;; pill rendering of deduped chips, the reflowed Actions block at
;; ryota's real rail width, the ✕'s absence in the live rail.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)

(require 'org-air)
(require 'org-air-view)
(require 'org-air-project)
(require 'org-air-review)
(require 'org-air-revisit)

;;;; ---------------------------------------------------------------------
;;;; Helpers
;;;; ---------------------------------------------------------------------

(defun org-air-r69--rail-filters-lines (filter &optional scope width)
  "Render the Filter+Source rail block and return its plain-text lines.
FILTER is the token list; SCOPE the scope form; WIDTH defaults to 32."
  (with-temp-buffer
    (let ((org-air-show-rail-filters t)
          (org-air-view--tag-filter filter)
          (org-air-view--scope scope)
          (org-air-filter-match 'all))
      (org-air-view--insert-rail-filters (or width 32)))
    (split-string (buffer-substring-no-properties (point-min) (point-max))
                  "\n")))

(defun org-air-r69--actions-lines (width &optional scope)
  "Render the BOARD Actions block at WIDTH (board buffer DEAD ⇒ fallback keys).
SCOPE, when non-nil, is let-bound as the active scope (`reset' replaces
`expand').  Returns the block's plain-text lines (no trailing empty)."
  (with-temp-buffer
    ;; A board buffer left behind by another suite must not leak derived
    ;; keys into this seam: point the legend at a name that cannot exist.
    (let ((org-air-view-buffer-name "*org-air-r69-no-board*")
          (org-air-view--scope scope))
      (org-air-view--insert-actions-default width))
    (butlast (split-string (buffer-substring-no-properties
                            (point-min) (point-max))
                           "\n"))))

(defun org-air-r69--count-matches (needle haystack)
  "Return the number of non-overlapping NEEDLE occurrences in HAYSTACK."
  (let ((n 0) (start 0))
    (while (setq start (string-search needle haystack start))
      (setq n (1+ n) start (+ start (length needle))))
    n))

;;;; ---------------------------------------------------------------------
;;;; T1 — R69-1: exactly ONE blank line between Filter and Source.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r69-1-spacer-between-filter-and-source ()
  "R69-1: the rail Filter block and the Source header breathe ONE blank line.
At 32w with two tokens the line order is: Filter header, chips, `Match:'
hint, ONE blank, Source header — matching the inter-section spacer
`--insert-rail-1' emits between every other rail section pair.  Exactly
one blank (never two: the blank sits between the Match hint and the
Source header, both non-empty)."
  (let* ((lines (org-air-r69--rail-filters-lines '("#alpha" "#beta")))
         (src (seq-position lines nil
                            (lambda (l _) (string-match-p "\\bSource\\b" l)))))
    (should src)
    ;; the line immediately before the Source header is the ONE blank...
    (should (equal "" (nth (1- src) lines)))
    ;; ...and it is exactly one: the line above it is the Match hint.
    (should (string-match-p "Match: AND" (nth (- src 2) lines)))))

(ert-deftest org-air-r69-1-spacer-on-empty-filter-too ()
  "R69-1: the empty-filter branch (`none') gains the SAME one-blank spacer."
  (let* ((lines (org-air-r69--rail-filters-lines nil))
         (src (seq-position lines nil
                            (lambda (l _) (string-match-p "\\bSource\\b" l)))))
    (should src)
    (should (equal "" (nth (1- src) lines)))
    ;; exactly one blank: the line above it is the `none' filter line.
    (should (string-match-p "\\bnone\\b" (nth (- src 2) lines)))))

;;;; ---------------------------------------------------------------------
;;;; T2 — R69-2: the dead ✕ clear glyph is GONE from the chips line.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r69-2-no-clear-glyph-after-rail-chips ()
  "R69-2: the rail chips line is EXACTLY inset + the joined chips.
Exact-line pin (immune to the batch `x' glyph appearing inside a tag
name): at 32w (inset 3) the chips line is `   #alpha AND #beta' — no
trailing clear glyph, no trailing spacer.  The `M-/ toggles ∙ \\ clears'
hint line below stays (it is the teaching surface, naming BOTH verbs)."
  (let ((lines (org-air-r69--rail-filters-lines '("#alpha" "#beta"))))
    ;; line 0 is the Filter header; line 1 the chips line.
    (should (equal "   #alpha AND #beta" (nth 1 lines)))
    (should (string-match-p "M-/ toggles" (nth 2 lines)))))

;;;; ---------------------------------------------------------------------
;;;; T3 — R69-3: the rail input stamp learns the AND/OR combinator.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r69-3-input-stamp-includes-filter-match ()
  "R69-3: `org-air-rail--input-stamp' differs across an AND↔OR flip.
This IS the M-/ repaint guarantee: `org-air-rail--show-1' repaints
exactly when the stamp changed (R27-1 S4), and the paint reads
`org-air-filter-match' three times (chip join word, `Match: %s' line,
`N of M shown' count) — so the flip must bust the stamp exactly like
`/' and `\\' (which mutate the already-stamped `--tag-filter') do.
A no-op re-read stays `equal' (the steady state keeps zero repaints)."
  (with-temp-buffer
    (org-air-view-mode)
    (let ((buf (current-buffer)))
      (let* ((org-air-filter-match 'all)
             (s1 (org-air-rail--input-stamp buf 40 30))
             (s2 (org-air-rail--input-stamp buf 40 30)))
        ;; no-op re-read ⇒ equal (the S4 skip-proof keeps its teeth).
        (should (equal s1 s2))
        ;; AND ↔ OR flip ⇒ the stamp differs ⇒ the repaint runs.
        (let ((org-air-filter-match 'any))
          (should-not (equal s1 (org-air-rail--input-stamp buf 40 30))))))))

;;;; ---------------------------------------------------------------------
;;;; T4 — R69-4: the Actions block REFLOWS at narrow widths (no truncation).
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r69-4-actions-reflow-narrow-28 ()
  "R69-4: at 28w (fallback keys) the board Actions block reflows — nothing
truncates: no `more' glyph anywhere, all six verb words present, every
line fits the width."
  (let ((more (org-air-view--glyph 'more))
        (lines (org-air-r69--actions-lines 28)))
    (dolist (line lines)
      (should-not (string-search more line))
      (should (<= (string-width line) 28)))
    (let ((text (string-join lines "\n")))
      (dolist (verb '("capture" "filter" "source" "refresh" "expand" "help"))
        (should (string-search verb text))))))

(ert-deftest org-air-r69-4-actions-reflow-31-scoped ()
  "R69-4: at 31w with a scope active (`reset' replaces `expand' — the
screenshot's exact `s sou…' case) the block reflows: no `more' glyph,
all six verbs present, every line fits."
  (let ((more (org-air-view--glyph 'more))
        (lines (org-air-r69--actions-lines 31 '(:tag "nix"))))
    (dolist (line lines)
      (should-not (string-search more line))
      (should (<= (string-width line) 31)))
    (let ((text (string-join lines "\n")))
      (dolist (verb '("capture" "filter" "source" "refresh" "reset" "help"))
        (should (string-search verb text)))
      (should-not (string-search "expand" text)))))

;;;; ---------------------------------------------------------------------
;;;; T5 — R69-4 parity: byte-identical where 3 columns fit (consolidation
;;;; carries ZERO golden shift at standard widths).
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r69-4-actions-wide-parity-byte-identical ()
  "R69-4 parity: the board Actions block at 41w is byte-identical to the
historical 2×3 layout (exact-string pin — same column maxima, same
4-space wide-tier gap, same unpadded-last-column rule)."
  (should (equal '("| Actions                                "
                   "   c capture      / filter      s source "
                   "   g r refresh    TAB expand    ? help   ")
                 (org-air-r69--actions-lines 41))))

(ert-deftest org-air-r69-4-static-table-parity-byte-identical ()
  "R69-4 parity for a STATIC-table site: the review Actions block at 32w
is byte-identical to the pre-consolidation 4×3 emitter (exact-string
pin — guards the four-site collapse into the shared emitter)."
  (should (equal '("| Actions                       "
                   "   RET open < prev   > next     "
                   "   m span   + widen  - narrow   "
                   "   f rollup / filter g refresh  "
                   "   . today  ? help   q quit     ")
                 (with-temp-buffer
                   (org-air-review--insert-actions 32)
                   (butlast (split-string (buffer-substring-no-properties
                                           (point-min) (point-max))
                                          "\n"))))))

;;;; ---------------------------------------------------------------------
;;;; T6 — R69-5: the chip-label dedup primitive, at every surface kind.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r69-5-tag-chip-label-unit ()
  "R69-5 unit pins: `--tag-chip-label' prefixes a plain name, returns a
`#'-led name VERBATIM, and collapses ONLY the org-air-prepended prefix
(a tag literally named `##x' is never rewritten)."
  (should (equal "#Nix" (org-air-view--tag-chip-label "Nix")))
  (should (equal "#Nix" (org-air-view--tag-chip-label "#Nix")))
  (should (equal "##x" (org-air-view--tag-chip-label "##x"))))

(ert-deftest org-air-r69-5-board-pill-and-chip-dedup ()
  "R69-5 surfaces: the board tag pills (`--item-tagstr', plain style) and
the inline chip button (`--insert-tag-chip') render a `#nix' tag with
ONE `#' — never `##nix'."
  (let ((org-air-tag-style 'plain))
    (should (equal "#nix" (substring-no-properties
                           (org-air-view--item-tagstr '("#nix") 1 1)))))
  (with-temp-buffer
    (org-air-view--insert-tag-chip "#nix")
    (should (equal "#nix" (buffer-substring-no-properties
                           (point-min) (point-max))))))

(ert-deftest org-air-r69-5-scope-label-dedup ()
  "R69-5 surface: the rail Source chip (`--scope-label' `:tag' branch)
renders a `#nix' tag scope as `#nix', never `##nix'."
  (let ((org-air-view--scope '(:tag "#nix")))
    (should (equal "#nix" (substring-no-properties
                           (org-air-view--scope-label))))))

(ert-deftest org-air-r69-5-review-rollup-label-dedup ()
  "R69-5 surface: the review tag rollup labels a `#nix'-tagged item
`#nix' (label-only — the fold/totals are untouched)."
  (let ((item (org-air-item-create :title "x" :tags '("#nix"))))
    (should (equal '("#nix") (org-air-review--rollup-labels item 'tag)))))

(ert-deftest org-air-r69-5-banner-filter-segment-token-label ()
  "R69-5 token-label switch: the BANNER filter segment routes through the
R24-6 `--filter-token-label' — a `#nix' token appears exactly ONCE
(never `##nix') and a bare `git' token reads quoted (`\"git\"'), not
falsely tag-dressed as `#git'."
  (with-temp-buffer
    (org-air-view-mode)
    (setq buffer-read-only nil)
    (let ((org-air-view--tag-filter '("#nix")))
      (org-air-view--insert-banner nil))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (= 1 (org-air-r69--count-matches "#nix" text)))
      (should-not (string-search "##" text))))
  (with-temp-buffer
    (org-air-view-mode)
    (setq buffer-read-only nil)
    (let ((org-air-view--tag-filter '("git")))
      (org-air-view--insert-banner nil))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-search "\"git\"" text))
      (should-not (string-search "#git" text)))))

(ert-deftest org-air-r69-5-project-filter-segment-token-label ()
  "R69-5 token-label switch: the PROJECT header filter segment routes
through `--filter-token-label' — `#nix' once, bare `git' quoted."
  (let ((org-air-view--tag-filter '("#nix")))
    (let ((text (substring-no-properties (org-air-project--filter-segment))))
      (should (= 1 (org-air-r69--count-matches "#nix" text)))
      (should-not (string-search "##" text))))
  (let ((org-air-view--tag-filter '("git")))
    (let ((text (substring-no-properties (org-air-project--filter-segment))))
      (should (string-search "\"git\"" text))
      (should-not (string-search "#git" text)))))

;;;; ---------------------------------------------------------------------
;;;; T7 — R69-5 companion: the matcher's tag branch hits a literal #-tag.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r69-5-matcher-verbatim-hash-tag ()
  "R69-5 companion: `--filter-token-match-p' resolves a `#Nix' token
against BOTH tag worlds — an item tagged `#nix' (VERBATIM, the literal-#
world; unmatchable before this round) and one tagged `nix' (STRIPPED,
the regression pin) — case-insensitively; a non-member still misses."
  (should (org-air-view--filter-token-match-p "#Nix" "" '("#nix")))
  (should (org-air-view--filter-token-match-p "#Nix" "" '("nix")))
  (should-not (org-air-view--filter-token-match-p "#Nix" "" '("other"))))

(ert-deftest org-air-r69-5-combinator-fold-untouched ()
  "R69-5 regression pin: `--tokens-pass-filter-p' under `all' with tokens
`(\"#a\" \"#b\")' is the unchanged AND fold — both tags pass, one alone
fails; under `any' one suffices (token storage + fold untouched)."
  (let ((org-air-view--tag-filter '("#a" "#b"))
        (org-air-filter-match 'all))
    (should (org-air-view--tokens-pass-filter-p "" '("a" "b")))
    (should-not (org-air-view--tokens-pass-filter-p "" '("a"))))
  (let ((org-air-view--tag-filter '("#a" "#b"))
        (org-air-filter-match 'any))
    (should (org-air-view--tokens-pass-filter-p "" '("a")))))

(provide 'org-air-round69-test)
;;; org-air-round69-test.el ends here
