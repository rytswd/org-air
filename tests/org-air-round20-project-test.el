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
  "`org-air-project--state-display-order' is the single airctl `-Da' state
order driving BOTH the per-dir badges and the within-dir doc ordering, so
the two can never drift: Ready · Work-In-Progress · Review · Complete ·
Dropped · Draft."
  (skip-unless (locate-library "org-air"))
  (should (equal org-air-project--state-display-order
                 '("ready" "work-in-progress" "review"
                   "complete" "dropped" "draft")))
  ;; the rank function orders accordingly (ready precedes draft)
  (should (< (org-air-project--state-display-rank "ready")
             (org-air-project--state-display-rank "draft")))
  (should (< (org-air-project--state-display-rank "complete")
             (org-air-project--state-display-rank "draft")))
  ;; dropped precedes draft (we normalise airctl's listing swap)
  (should (< (org-air-project--state-display-rank "dropped")
             (org-air-project--state-display-rank "draft"))))

(ert-deftest org-air-r20-5-default-group-is-directory ()
  "R20-5: the SHIPPED default project grouping is `directory' — the nested
tree that matches `airctl status -Da' (the most useful view).  Asserted
against the defcustom's STANDARD value so a sibling test that toggled the
global grouping (the group-by commands `setq' it) cannot perturb us."
  (skip-unless (locate-library "org-air"))
  (should (eq (eval (car (get 'org-air-project-group 'standard-value)) t)
              'directory)))

;;;; ---------------------------------------------------------------------
;;;; R20-5(b) — truly REUSE the dashboard core (shared rail + thin keymap).
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r20-5-project-reuses-shared-board-rail ()
  "The project view drives the SHARED board rail via a buffer-local view
descriptor (no bespoke parallel rail): the rendered rail carries
Calendar · Filter · Scope · Summary · Inspector · Actions — the bespoke
pre-R20 project rail had ONLY Summary + Inspector, so the new blocks
(Calendar grid, Filter, Scope, Actions) prove the board rail is reused."
  (skip-unless (locate-library "org-air"))
  (let ((org-air-project-view-width 100))
    (org-air-project-test--render
     ;; the view descriptor is installed -> the shared rail consults it
     (should org-air-view--rail-descriptor)
     (should (plist-get org-air-view--rail-descriptor :summary-fn))
     (should (plist-get org-air-view--rail-descriptor :actions-fn))
     (should (plist-get org-air-view--rail-descriptor :calendar-fn))
     (let ((text (buffer-string)))
       ;; shared-rail blocks the OLD bespoke rail never had:
       (should (string-match-p "| Filter" text))
       (should (string-match-p "| Scope" text))
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
key and never shadows the board verbs.
 - parent is the shared core; the core keys RET/v/V/\\\\/M-/ resolve to the
   SAME command in the board AND the project map;
 - the per-mode `/' filter stays per-view (different candidate source);
 - the old project domain verbs s/d/t/o/O are GONE (state/tag/sort moved
   to `M-x'), so they no longer shadow the board."
  (skip-unless (locate-library "org-air"))
  ;; thin child of the shared core
  (should (eq (keymap-parent org-air-project-mode-map) org-air-view-core-map))
  ;; SHARED core keys resolve identically board <-> project
  (dolist (key '("RET" "v" "V" "\\" "M-/"))
    (should (eq (lookup-key org-air-project-mode-map (kbd key))
                (lookup-key org-air-view-mode-map (kbd key))))
    ;; and they are genuinely BOUND (not both nil)
    (should (lookup-key org-air-view-core-map (kbd key))))
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
  ;; the board domain verbs are NOT shadowed by a project override
  (dolist (key '("s" "d" "t" "o" "O"))
    (should (null (lookup-key org-air-project-mode-map (kbd key)))))
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
    (setq org-air-view--tag-filter '("airctl" "ui"))
    (setq org-air-view--scope '(:tag "work"))
    (let* ((org-air-filter-match 'all)
           (s (org-air-view--mode-line-content)))
      (should (string-match-p "2076 items" s))
      (should (string-match-p "filter #airctl AND #ui" s))
      (should (string-match-p "scope #work" s)))
    ;; OR combinator joins with the literal word OR
    (let* ((org-air-filter-match 'any)
           (s (org-air-view--mode-line-content)))
      (should (string-match-p "#airctl OR #ui" s)))))

(ert-deftest org-air-r20-2-board-status-calm-no-filter-form ()
  "With no filter and no scope the board status reads the calm
`no filter · scope all items' form, and singular counts drop the `s'."
  (skip-unless (locate-library "org-air"))
  (with-temp-buffer
    (org-air-view-mode)
    (setq org-air-view--mode-line-count 1
          org-air-view--tag-filter nil
          org-air-view--scope nil)
    (let ((s (org-air-view--mode-line-content)))
      (should (string-match-p "1 item\\b" s))     ; singular, no trailing s
      (should-not (string-match-p "1 items" s))
      (should (string-match-p "no filter" s))
      (should (string-match-p "scope all items" s)))))

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
