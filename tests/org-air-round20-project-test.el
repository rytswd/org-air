;;; org-air-round20-project-test.el --- R20-5 + R20-2 test-seat suite -*- lexical-binding: t; -*-

;;; Commentary:
;; Test-seat substantive ERTs for v0.5 round-20
;; (air/v0.5/org-air-round20-design.org), covering the two items the
;; impl-track round-20 suite (org-air-round20-test.el) does NOT:
;;
;;   R20-5  project view rebuild — match `airctl status -Da' (nested
;;          directory tree, per-dir state counts, state-first docs) and
;;          truly REUSE the dashboard core (shared rail + thin keymap +
;;          same filter keys).  The directory model is asserted both over
;;          a synthetic doc set AND over the real ./air fixture; the rail
;;          reuse + the keymap drift guard pin the "no fork" contract.
;;
;;   R20-2  mode-line — a USEFUL, VISIBLE calm status line: counts ·
;;          filter · scope on the board, the doc count on the project, and
;;          a face that draws the board->pane boundary rule.
;;
;; These complement (do not duplicate) the impl suite's R20-1/3/4/6
;; behaviour tests; together with the re-blessed byte goldens they cover
;; every round-20 item.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org-air-test-helpers)
(require 'org-air-project-test)            ; fixture root + render helpers
(require 'org-air)

;;;; ---------------------------------------------------------------------
;;;; R20-5(a) — the `airctl status -Da' nested directory-tree MODEL.
;;;; ---------------------------------------------------------------------

(defun org-air-r20-5--doc (relpath state &optional name)
  "Build a synthetic `org-air-doc' at RELPATH in STATE (NAME optional)."
  (org-air-doc-create
   :name (or name (file-name-base relpath))
   :file (concat "/tmp/air/" relpath)
   :relpath relpath
   :state state
   :tags nil
   :updated nil :created nil))

(ert-deftest org-air-r20-5-directory-tree-nests-counts-and-state-first ()
  "`org-air-project--directory-tree' is the `airctl status -Da' model:
 (a) it NESTS to any depth (v0.1 -> air-template -> references), not the
     old flat first-segment grouping;
 (b) every directory node carries per-state DIRECT counts plus the
     DESCENDANT roll-up (the `(+M)') and the total;
 (c) a node's own docs are STATE-FIRST in `org-air-project--state-display-
     order', ties broken by name."
  (skip-unless (locate-library "org-air"))
  (let* ((docs (list
                ;; v0.1/ own docs (mixed states, to pin state-first order)
                (org-air-r20-5--doc "v0.1/zeta-ready.org"    "ready"    "Zeta")
                (org-air-r20-5--doc "v0.1/alpha-complete.org" "complete" "Alpha")
                ;; v0.1/air-template/ own docs (two complete)
                (org-air-r20-5--doc "v0.1/air-template/adr.org"  "complete" "Adr")
                (org-air-r20-5--doc "v0.1/air-template/file.org" "complete" "File")
                ;; v0.1/air-template/references/ own docs (two draft)
                (org-air-r20-5--doc "v0.1/air-template/references/pep.org" "draft" "Pep")
                (org-air-r20-5--doc "v0.1/air-template/references/kep.org" "draft" "Kep")))
         (tree (org-air-project--directory-tree docs)))
    ;; ---- (a) nesting: one top node v0.1 -> air-template -> references ----
    (should (= (length tree) 1))
    (let* ((v01 (car tree)))
      (should (equal (plist-get v01 :dir) "v0.1"))
      (should (equal (plist-get v01 :path) "v0.1"))
      (should (= (plist-get v01 :depth) 0))
      (let ((kids (plist-get v01 :children)))
        (should (= (length kids) 1))
        (let* ((tmpl (car kids)))
          (should (equal (plist-get tmpl :dir) "air-template"))
          (should (= (plist-get tmpl :depth) 1))
          (let ((refs (car (plist-get tmpl :children))))
            (should (equal (plist-get refs :dir) "references"))
            (should (= (plist-get refs :depth) 2))
            (should (null (plist-get refs :children)))
            ;; ---- (c) references own docs state-first then by name ----
            (let ((names (mapcar #'org-air-doc-name (plist-get refs :own-docs))))
              ;; both DRAFT -> name tiebreak: Kep < Pep
              (should (equal names '("Kep" "Pep"))))
            ;; references direct counts: draft 2, no descendants
            (should (= (cdr (assoc "draft" (plist-get refs :direct-counts))) 2))
            (should (null (plist-get refs :desc-counts))))
          ;; ---- (b) air-template: 2 complete DIRECT, 2 draft DESCENDANT ----
          (should (= (cdr (assoc "complete" (plist-get tmpl :direct-counts))) 2))
          (should (= (cdr (assoc "draft" (plist-get tmpl :desc-counts))) 2))
          (should (null (assoc "complete" (plist-get tmpl :desc-counts))))))
      ;; ---- v0.1 own docs: state-first (ready before complete) ----
      (let ((names (mapcar #'org-air-doc-name (plist-get v01 :own-docs))))
        (should (equal names '("Zeta" "Alpha"))))   ; ready(0) before complete(3)
      ;; ---- v0.1 counts: direct ready1/complete1; roll-up complete3/draft2 ----
      (should (= (cdr (assoc "ready"    (plist-get v01 :direct-counts))) 1))
      (should (= (cdr (assoc "complete" (plist-get v01 :direct-counts))) 1))
      (should (= (cdr (assoc "complete" (plist-get v01 :desc-counts))) 2))
      (should (= (cdr (assoc "draft"    (plist-get v01 :desc-counts))) 2))
      (should (= (cdr (assoc "complete" (plist-get v01 :total-counts))) 3))
      (should (= (cdr (assoc "ready"    (plist-get v01 :total-counts))) 1)))))

(ert-deftest org-air-r20-5-directory-tree-over-real-fixture ()
  "The tree model over the REAL ./air fixture docs nests `v0.1/' with its
own docs PLUS an `air-context/' child node (the fixture's nested
`v0.1/air-context/gamma-context.org'), and a separate `v0.2/' top node —
proving the nesting is driven by the actual files, not hard-coded."
  (skip-unless (locate-library "org-air"))
  (let* ((docs (org-air-project--collect-docs org-air-project-test-root))
         (tree (org-air-project--directory-tree docs))
         (dirs (mapcar (lambda (n) (plist-get n :dir)) tree)))
    ;; top dirs are v0.1 and v0.2 (name-sorted)
    (should (member "v0.1" dirs))
    (should (member "v0.2" dirs))
    (let* ((v01 (seq-find (lambda (n) (equal (plist-get n :dir) "v0.1")) tree))
           (kids (mapcar (lambda (n) (plist-get n :dir))
                         (plist-get v01 :children))))
      ;; v0.1 has its OWN docs (alpha/beta) at depth 0 AND a nested child
      (should (plist-get v01 :own-docs))
      (should (member "air-context" kids))
      ;; the gamma Draft lives in the child dir, so v0.1's DRAFT count is a
      ;; pure descendant roll-up (direct 0, descendant 1).
      (should (null (assoc "draft" (plist-get v01 :direct-counts))))
      (should (= (cdr (assoc "draft" (plist-get v01 :desc-counts))) 1)))))

(ert-deftest org-air-r20-5-state-display-order-matches-airctl ()
  "`org-air-project--state-display-order' is the airctl `-Da' state order
for the COUNT surfaces — the per-dir LETTER summaries: Ready ·
Work-In-Progress · Complete · Out · Off · Dropped · Draft.  R25-3
re-bless: the phantom `review' state is gone.  R51-2 re-bless: the
constant is counts-only now — `--state-display-rank' (its only caller
retargeted) is DELETED and doc ROWS rank via
`org-air-project--state-sort-rank', under which dropped sorts AFTER
draft (the group bottom), not before; the letter order above stays the
airctl byte-parity contract, untouched.  R80 re-bless: the parked pair
`out'/`off' joins the rollup after `complete', before `dropped' — a
DELIBERATE forward divergence from airctl's Rust enum (which does not yet
know out/off), flagged PRODUCT-CONFIRM in the R80 spec; org-air renders
what the user writes and airctl is expected to follow."
  (skip-unless (locate-library "org-air"))
  (should (equal org-air-project--state-display-order
                 '("ready" "work-in-progress"
                   "complete" "out" "off" "dropped" "draft")))
  ;; R25-3: `review' is absent from the canonical order.
  (should-not (member "review" org-air-project--state-display-order))
  ;; the R51-2 row-rank fn orders the live states ahead of draft…
  (should (< (org-air-project--state-sort-rank "ready")
             (org-air-project--state-sort-rank "draft")))
  (should (< (org-air-project--state-sort-rank "complete")
             (org-air-project--state-sort-rank "draft")))
  ;; …and dropped AFTER draft (R51-2 inverted the old display-rank slot:
  ;; rows sink dropped to the group bottom; only the LETTER summaries
  ;; keep airctl's dropped-before-draft listing).
  (should (> (org-air-project--state-sort-rank "dropped")
             (org-air-project--state-sort-rank "draft"))))

(ert-deftest org-air-r20-5-default-group-is-directory ()
  "R20-5: the SHIPPED default project grouping is `directory' — the nested
tree that matches `airctl status -Da' (the most useful view).  Asserted
against the defcustom's STANDARD value so a sibling test that toggled the
global grouping (the group-by commands `setq' it) cannot perturb us."
  (skip-unless (locate-library "org-air"))
  (should (eq (eval (car (get 'org-air-project-group 'standard-value)) t)
              'directory)))

;;;; ---------------------------------------------------------------------
;;;; R20-5 FIX-GUARD — exclude OVERVIEW/README/SKILL + stateless->unknown.
;;;;
;;;; Closes the fixture GAP review flagged: the ./air fixture now carries
;;;; `v0.2/OVERVIEW.org' (a directory-summary doc, even `#+state: ready')
;;;; AND `v0.2/eta-notes.org' (NO `#+state:'), so the divergence the fix
;;;; corrects is EXERCISED end-to-end (data model + the directory-tree
;;;; goldens).  Pins airctl `-Da' parity: a summary doc is never a tracked
;;;; row and a stateless doc is UNKNOWN, never silently Draft.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r20-5-fix-overview-file-p-matches-airctl ()
  "`org-air-project--overview-file-p' mirrors airctl's `is_overview_file':
the file STEM (sans extension), case-folded, is README / OVERVIEW / SKILL
-> non-trackable; any other stem (and nil) -> trackable."
  (skip-unless (locate-library "org-air"))
  ;; the reserved stems, in every case / extension airctl folds.
  (dolist (f '("a/OVERVIEW.org" "a/overview.org" "a/Overview.org"
               "a/README.org" "a/readme.md" "a/SKILL.org" "deep/dir/skill.org"))
    (should (org-air-project--overview-file-p f)))
  ;; ordinary work docs (incl. the fixture's) are trackable.
  (dolist (f '("v0.1/alpha-feature.org" "v0.2/eta-notes.org"
               "a/overview-of-pricing.org" "a/my-readme-notes.org"))
    (should-not (org-air-project--overview-file-p f)))
  ;; nil is safe (defensive).
  (should-not (org-air-project--overview-file-p nil)))

(ert-deftest org-air-r20-5-fix-collect-excludes-overview-stateless-unknown ()
  "Over the REAL ./air fixture, `org-air-project--collect-docs' enacts the
R20-5 fix divergence:
 (a) `v0.2/OVERVIEW.org' is EXCLUDED -- no doc row, and its `#+state:
     ready' never leaks into the Ready count (Ready stays 1 = Alpha);
 (b) `v0.2/eta-notes.org' (NO `#+state:') is present and classifies as
     `unknown' -- NEVER `draft', so Draft stays 2 (Epsilon + Gamma)
     instead of inflating to 3/4 the way the pre-fix `unwrap_or draft'
     silently did."
  (skip-unless (locate-library "org-air"))
  (let* ((docs (org-air-project--collect-docs org-air-project-test-root))
         (relpaths (mapcar #'org-air-doc-relpath docs))
         (eta (seq-find (lambda (d)
                          (equal (org-air-doc-relpath d) "v0.2/eta-notes.org"))
                        docs))
         (by-state (seq-group-by #'org-air-doc-state docs)))
    ;; (a) OVERVIEW excluded -- not by relpath, not by any reserved stem.
    (should-not (member "v0.2/OVERVIEW.org" relpaths))
    (should-not (seq-find #'org-air-project--overview-file-p
                          (mapcar #'org-air-doc-file docs)))
    ;; its `ready' state did not leak: Ready is still ONLY Alpha.
    (should (= (length (cdr (assoc "ready" by-state))) 1))
    ;; (b) the stateless doc IS tracked, and it is UNKNOWN not DRAFT.
    (should eta)
    (should (equal (org-air-doc-state eta) "unknown"))
    (should-not (equal (org-air-doc-state eta) "draft"))
    ;; unknown ranks after every LIVE state incl. draft (R51-2 re-bless:
    ;; `--state-display-rank' is deleted; rows rank via
    ;; `--state-sort-rank') — and only dropped sorts past the unknowns.
    (should (> (org-air-project--state-sort-rank "unknown")
               (org-air-project--state-sort-rank "draft")))
    (should (> (org-air-project--state-sort-rank "dropped")
               (org-air-project--state-sort-rank "unknown")))
    ;; the stateless body did NOT inflate Draft: still the two real drafts.
    (should (= (length (cdr (assoc "draft" by-state))) 2))
    (should (= (length (cdr (assoc "unknown" by-state))) 1))))

(ert-deftest org-air-r20-5-fix-directory-render-guards-divergence ()
  "The DIRECTORY-tree render (the shipped default, the goldens) shows the
fix where it matters: the stateless `Eta notes' renders with the faded
`UNKNO' word cell (UNKNOWN, never `DRAFT'/`READY'; R26-2 re-bless of the
old `[U]' bracket token), the v0.2 per-dir Draft count is NOT inflated
(Epsilon only), and `OVERVIEW.org' contributes NOTHING -- no title row, no
`#summary' tag, no Ready badge in v0.2.  R22-6 re-bless: the per-dir
counts are the quiet right-aligned LETTER-count summary (`W1 X1 D1', same
numbers / `airctl status -Da' parity) now, not the old `[W] 1  [X] 1  [D] 1'
badge wall."
  (skip-unless (locate-library "org-air"))
  (let ((text (string-join
               (org-air-project-test--render-lines
                'org-air-project-group-by-directory 100)
               "\n")))
    ;; stateless doc -> UNKNOWN word cell, never Draft/Ready.
    (should (string-match-p "UNKNO Eta notes" text))
    (should-not (string-match-p "\\(DRAFT\\|READY\\) *Eta notes" text))
    ;; R22-6: the v0.2 per-dir count summary is the letter-count `W1 X1 D1'
    ;; (Work-In-Progress 1, Dropped 1, Draft 1 = Epsilon only) — NOT inflated
    ;; by the unknown/excluded docs, same numbers as the old badge wall.
    (should (string-match-p "v0\\.2/ +W1 X1 D1" text))
    (should-not (string-match-p "\\[W\\] 1  \\[X\\] 1  \\[D\\] 1" text))
    ;; OVERVIEW.org contributes NOTHING to the render.
    (should-not (string-match-p "Overview" text))
    (should-not (string-match-p "#summary" text))))

;;;; ---------------------------------------------------------------------
;;;; R20-5(b) — truly REUSE the dashboard core (shared rail + thin keymap).
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r20-5-project-reuses-shared-board-rail ()
  "The project view drives the SHARED board rail via a buffer-local view
descriptor (no bespoke parallel rail): the rendered rail carries
Calendar · Filter · Source · Summary · Inspector · Actions — the bespoke
pre-R20 project rail had ONLY Summary + Inspector, so the new blocks
(Calendar grid, Filter, Source, Actions) prove the board rail is reused.
R22-4 re-bless: the structural-lens block header is `Source' now (was
`Scope'); the shared block carries the `none' empty filter + the `N
loaded' dataset count."
  (skip-unless (locate-library "org-air"))
  (let ((org-air-project-view-width 100))
    (org-air-project-test--render
     ;; the view descriptor is installed -> the shared rail consults it
     (should org-air-view--rail-descriptor)
     (should (plist-get org-air-view--rail-descriptor :summary-fn))
     (should (plist-get org-air-view--rail-descriptor :actions-fn))
     (should (plist-get org-air-view--rail-descriptor :calendar-fn))
     (let ((text (buffer-string)))
       ;; shared-rail blocks the OLD bespoke rail never had (R22-4:
       ;; `Source' replaces the `Scope' header):
       (should (string-match-p "| Filter" text))
       (should (string-match-p "| Source" text))
       (should-not (string-match-p "| Scope\\b" text))
       (should (string-match-p "| Actions" text))
       ;; the calendar grid (weekday header) — the board's calendar, reused
       (should (string-match-p "Su Mo Tu We Th Fr Sa" text))
       ;; and the blocks the bespoke rail DID carry are still present
       (should (string-match-p "| Summary" text))
       (should (string-match-p "| Inspector" text))
       ;; the Actions block advertises the SAME board keys (RET/filter/quit)
       (should (string-match-p "RET" text))
       (should (string-match-p "filter" text))))))

(ert-deftest org-air-r20-5-project-keymap-shares-board-keys-no-shadow ()
  "Drift guard for `use the same keys': the project map is a THIN child of
`org-air-view-core-map' that KEEPS the board's meaning for every SHARED
key.
 - parent is the shared core; the core keys v/V/\\\\/M-/ resolve to the
   SAME command in the board AND the project map;
 - R26-3: RET forks DELIBERATELY — board RET stays the pane-return, the
   project's RET is the same-window `org-air-project-open' (R26-5 session
   model);
 - R22-3: `o'/`O' are the SHARED within-view sort, inherited from the
   core, so they resolve IDENTICALLY board <-> project (not project-local);
 - the per-mode `/' filter stays per-view (different candidate source);
 - R26-3: s/d/t are project GROUPING keys again (on-key airctl -a/-Da/-Ta
   parity; the R22-3 'moved to M-x' contract is superseded — the board's
   triage meanings for those keys never applied in the project)."
  (skip-unless (locate-library "org-air"))
  ;; thin child of the shared core
  (should (eq (keymap-parent org-air-project-mode-map) org-air-view-core-map))
  ;; SHARED core keys resolve identically board <-> project.  R22-3 adds the
  ;; within-view sort pair `o'/`O' to the shared core, so they too resolve
  ;; identically across the two views (the project's old bespoke sort retired).
  (dolist (key '("v" "V" "\\" "M-/" "o" "O"))
    (should (eq (lookup-key org-air-project-mode-map (kbd key))
                (lookup-key org-air-view-mode-map (kbd key))))
    ;; and they are genuinely BOUND (not both nil)
    (should (lookup-key org-air-view-core-map (kbd key))))
  ;; R26-3: RET forks — same-window doc open in the project, pane-return
  ;; on the board.
  (should (eq (lookup-key org-air-project-mode-map (kbd "RET"))
              'org-air-project-open))
  (should (eq (lookup-key org-air-view-mode-map (kbd "RET"))
              'org-air-view-pane-return))
  ;; the shared sort pair is exactly the cross-view cycle/reverse commands.
  (should (eq (lookup-key org-air-project-mode-map (kbd "o"))
              'org-air-view-sort-cycle))
  (should (eq (lookup-key org-air-project-mode-map (kbd "O"))
              'org-air-view-sort-reverse))
  ;; `\\' clear + `M-/' toggle are the shared filter keys
  (should (eq (lookup-key org-air-project-mode-map (kbd "\\"))
              'org-air-filter-clear))
  (should (eq (lookup-key org-air-project-mode-map (kbd "M-/"))
              'org-air-filter-toggle-match))
  ;; `/' is the per-mode filter (project doc tags vs board item tags)
  (should (eq (lookup-key org-air-project-mode-map (kbd "/"))
              'org-air-project-filter))
  (should (eq (lookup-key org-air-view-mode-map (kbd "/"))
              'org-air-filter))
  ;; R26-3: s/d/t are the project's own grouping verbs (airctl parity on
  ;; keys); o/O are SHARED, asserted above.
  (should (eq (lookup-key org-air-project-mode-map (kbd "s"))
              'org-air-project-group-by-state))
  (should (eq (lookup-key org-air-project-mode-map (kbd "d"))
              'org-air-project-group-by-directory))
  (should (eq (lookup-key org-air-project-mode-map (kbd "t"))
              'org-air-project-group-by-tag))
  ;; the project keeps only its OWN non-shared verbs
  (should (eq (lookup-key org-air-project-mode-map (kbd "g"))
              'org-air-project-refresh))
  (should (eq (lookup-key org-air-project-mode-map (kbd "q"))
              'org-air-project-quit))
  (should (eq (lookup-key org-air-project-mode-map (kbd "<S-return>"))
              'org-air-project-visit)))

;;;; ---------------------------------------------------------------------
;;;; R20-2 — a USEFUL, VISIBLE calm status mode-line.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r20-2-board-status-has-counts-filter-scope ()
  "The board status mode-line earns its row: it reports the visible ITEM
count, the active FILTER tags joined by the combinator WORD, and the
SCOPE label — all from buffer-locals already on hand."
  (skip-unless (locate-library "org-air"))
  (with-temp-buffer
    (org-air-view-mode)
    (setq org-air-view--mode-line-count 2076)
    ;; R24-6: tokens stored VERBATIM; `#tag' tokens show verbatim in the line.
    (setq org-air-view--tag-filter '("#airctl" "#ui"))
    (setq org-air-view--scope '(:tag "work"))
    (let* ((org-air-filter-match 'all)
           (s (org-air-view--mode-line-content)))
      (should (string-match-p "2076 items" s))
      (should (string-match-p "filter #airctl AND #ui" s))
      ;; R22-4: the source segment reads `source <...>' now (was `scope').
      (should (string-match-p "source #work" s))
      (should-not (string-match-p "scope #work" s)))
    ;; OR combinator joins with the literal word OR
    (let* ((org-air-filter-match 'any)
           (s (org-air-view--mode-line-content)))
      (should (string-match-p "#airctl OR #ui" s)))))

(ert-deftest org-air-r20-2-board-status-calm-no-filter-form ()
  "With no filter and no scope the board status reads the calm
`filter none · source all items' form (R22-4 wording), and singular counts
drop the `s'."
  (skip-unless (locate-library "org-air"))
  (with-temp-buffer
    (org-air-view-mode)
    (setq org-air-view--mode-line-count 1
          org-air-view--tag-filter nil
          org-air-view--scope nil)
    (let ((s (org-air-view--mode-line-content)))
      (should (string-match-p "1 item\\b" s))     ; singular, no trailing s
      (should-not (string-match-p "1 items" s))
      ;; R22-4: empty filter reads `filter none' (was `no filter'); the
      ;; source segment reads `source all items' (was `scope all items')
      ;; so the two roles no longer both say "all items".
      (should (string-match-p "filter none" s))
      (should-not (string-match-p "no filter" s))
      (should (string-match-p "source all items" s))
      (should-not (string-match-p "scope all items" s)))))

(ert-deftest org-air-r20-2-project-status-reports-doc-count ()
  "The PROJECT status mode-line reports the doc count (shared construct,
branched on major-mode — invariant #4, not a bespoke `Org-Air-Project'
lighter)."
  (skip-unless (locate-library "org-air"))
  (with-temp-buffer
    (org-air-project-mode)
    (setq org-air-project--doc-count 70)
    (let ((s (org-air-view--mode-line-content)))
      (should (string-match-p "project" s))
      (should (string-match-p "70 docs" s))
      ;; NOT the meaningless mode-name lighter
      (should-not (string-match-p "Org-Air-Project" s)))))

(ert-deftest org-air-r20-2-modeline-and-pane-faces-draw-boundary ()
  "R20-2 #2: the status mode-line face AND the view-pane header-line face
declare a 1px `:overline' boundary rule (in the divider hue) so the
board->pane seam is visible — the calm line becomes the boundary."
  (skip-unless (locate-library "org-air"))
  (dolist (face '(org-air-face-modeline org-air-face-pane-header))
    (should (facep face))
    ;; the colour-display clauses declare an :overline (boundary rule).
    (should (string-match-p
             ":overline"
             (prin1-to-string (get face 'face-defface-spec))))))

(provide 'org-air-round20-project-test)
;;; org-air-round20-project-test.el ends here
