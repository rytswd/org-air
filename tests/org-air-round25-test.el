;;; org-air-round25-test.el --- substantive ERTs for v0.5 round-25 -*- lexical-binding: t; -*-

;;; Commentary:
;; Test-seat SUBSTANTIVE ERTs for v0.5 round-25
;; (air/v0.5/org-air-round25-design.org).  These cover the six R25 fixes:
;;
;;   R25-1  TREE ARM LENGTH [POLISH] — a doc row's leading gutter fills the
;;          run AFTER the corner with `box-horizontal' so the arm REACHES the
;;          state badge (`+-----[R]'); V6 columns frozen.
;;   R25-2  LEGIBLE SVG BADGE [POLISH] — `--svg-pillify' gains `:label' +
;;          `:font-weight'; the project state chip draws a big BOLD single
;;          letter while the `[D]' cell box stays fixed width.
;;   R25-3  DROP "review" [BUG/CLEANUP] — the phantom `review' state is gone
;;          from every list/map/badge/glyph/face; the canonical 5 remain.
;;   R25-4  DRAFT != DROPPED [BUG] — one canonical letter map (D/R/W/C/X),
;;          airctl-aligned + DISTINCT, drives BOTH the per-doc badge and the
;;          per-dir rollup; the name-first-char collision is killed.
;;   R25-5  DROP PROJECT ORIGIN COL [POLISH] — project rows carry no origin
;;          cell; the BOARD keeps its origin; the relpath stays filterable.
;;   R25-6  CLEAN RAIL DUAL-MODE [BUG — priority] — single-owner invariant:
;;          exactly ONE `*org-air-rail*' side window, for the ACTIVE view,
;;          never both, never cross-view.  DRIVEN through live windows.
;;
;; The rail item (R25-6) is DRIVEN, not inspected: a live frame, board +
;; project in real windows, `org-air-rail-toggle' / `--render-current' /
;; native window ops EXECUTED with `noninteractive' bound nil so the
;; `display-buffer-in-side-window' path creates a REAL side window, and the
;; double-rail / cross-view leak is asserted from the live window tree.
;; Reconcile is deferred to a 0s timer, so the ERTs call
;; `org-air-rail--reconcile-frame' directly to assert synchronously.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)
(require 'org-air-project-test)            ; project fixture root + render

(when (locate-library "org-air")
  (require 'org-air))

;;;; =====================================================================
;;;; R25-6 — CLEAN rail dual-mode (DRIVEN through live windows).
;;;; =====================================================================

(defun org-air-r25--kill-aux-buffers ()
  "Kill the shared pane/rail buffers so a test never inherits stale windows."
  (let ((kill-buffer-query-functions nil))
    (dolist (name (list org-air-view-pane-buffer-name org-air-rail-buffer-name
                        org-air-view-buffer-name "*org-air-project*"))
      (when (get-buffer name) (kill-buffer name)))
    (dolist (b (buffer-list))
      (when (and (buffer-live-p b)
                 (string-match-p "\\` ?\\*org-air-pane:" (buffer-name b)))
        (with-current-buffer b (set-buffer-modified-p nil))
        (kill-buffer b)))))

(defmacro org-air-r25--with-board+project (&rest body)
  "Open a board AND a project in a live frame (noninteractive nil); run BODY.
The board (`*org-air*') is rendered inline; the project (`*org-air-project*')
is shown in the single MAIN window.  BODY drives toggles + switches the main
window between the two via `org-air-r25--show-in-main', asserting the rail
from the live window tree.  Both render widths are pinned wide (120) so the
view is never responsive-narrow board-only."
  (declare (indent 0) (debug t))
  `(org-air-test-with-fixtures
    (let ((org-air-sources (list (list :air org-air-project-test-root)))
          (org-air-project-group 'directory)
          (org-air-project-view-width 120)
          (org-air-view-width 120)
          (org-air-rail-style 'inline)
          ;; R26-5 pin: the project now DEFAULTS to a popped side rail
          ;; (`org-air-rail-placement'); this harness asserts the R25-6
          ;; single-owner invariant from a known INLINE start, so pin it.
          (org-air-rail-placement '((board . inline) (project . inline)))
          (org-air-rail-focus-on-popout nil))
      (save-window-excursion
        (org-air-r25--kill-aux-buffers)
        (let ((noninteractive nil))
          ;; Board: minimal synchronous inline render (mode init installs the
          ;; reconcile hook because noninteractive is nil here).
          (let ((bbuf (get-buffer-create org-air-view-buffer-name)))
            (with-current-buffer bbuf
              (org-air-view-mode)
              (setq org-air-view--items (org-air-query-items))
              (setq-local org-air-view--rail-popped-out nil)
              (org-air-view--render org-air-view--items nil)))
          ;; Project: full entry — pops to the selected (main) window.
          (org-air-project))
        (let* ((pbuf (get-buffer "*org-air-project*"))
               (bbuf (get-buffer org-air-view-buffer-name))
               (w (get-buffer-window pbuf)))
          (should pbuf) (should bbuf) (should (window-live-p w))
          (select-window w)
          (delete-other-windows w)       ; single MAIN window; board offscreen
          (unwind-protect
              (let ((noninteractive nil)) ,@body)
            (org-air-r25--kill-aux-buffers)))))))

(defun org-air-r25--main-window ()
  "Return the first non-side window on the selected frame (the MAIN view)."
  (catch 'hit
    (dolist (w (window-list (selected-frame) 'no-mini))
      (unless (window-parameter w 'window-side)
        (throw 'hit w)))
    nil))

(defun org-air-r25--show-in-main (buf)
  "Switch the MAIN window to BUF and re-render it via its mode (R25-6)."
  (let ((w (org-air-r25--main-window)))
    (select-window w)
    (switch-to-buffer buf)
    (with-current-buffer buf
      (cond ((derived-mode-p 'org-air-view-mode) (org-air-view--render-current))
            ((derived-mode-p 'org-air-project-mode)
             (org-air-project--render-current))))))

(defun org-air-r25--pop (buf)
  "Pop BUF's rail OUT via the real toggle, with BUF selected in the main window."
  (let ((w (get-buffer-window buf)))
    (when (window-live-p w) (select-window w)))
  (with-current-buffer buf
    (org-air-rail-toggle)))

(defun org-air-r25--reconcile ()
  "Run the single-owner reconcile synchronously on the selected frame."
  (org-air-rail--reconcile-frame (selected-frame)))

(defun org-air-r25--side-windows ()
  "Return the list of live windows showing the rail buffer on this frame."
  (let ((rb (get-buffer org-air-rail-buffer-name)))
    (and rb (get-buffer-window-list rb 'no-mini (selected-frame)))))

(ert-deftest org-air-r25-6-no-double-rail-after-view-switch ()
  "Project popped, then the MAIN window switches to the board: exactly ONE
rail, never the project's side rail beside an inline board.  On trunk the
`*org-air-rail*' side window survives with back-ptr=*org-air-project* beside
the board's inline rail (two rails) — this fails there."
  (skip-unless (locate-library "org-air"))
  (org-air-r25--with-board+project
   (let ((pbuf (get-buffer "*org-air-project*"))
         (bbuf (get-buffer org-air-view-buffer-name)))
     ;; Project pops its rail OUT.
     (org-air-r25--pop pbuf)
     (should (window-live-p (org-air-rail--side-window)))
     (should (eq (org-air-rail--side-owner) pbuf))
     ;; Switch the MAIN window to the board + reconcile.
     (org-air-r25--show-in-main bbuf)
     (org-air-r25--reconcile)
     ;; The board is INLINE (never popped) -> the side rail is GONE; there is
     ;; certainly no side window still showing the PROJECT.
     (let ((side (org-air-rail--side-window)))
       (when (window-live-p side)
         (should-not (eq (org-air-rail--side-owner) pbuf))
         (should (eq (org-air-rail--side-owner) bbuf)))
       (should-not (eq (org-air-rail--side-owner) pbuf)))
     ;; Exactly one rail at most.
     (should (<= (length (org-air-r25--side-windows)) 1)))))

(ert-deftest org-air-r25-6-side-rail-shows-current-view ()
  "Whenever a side rail exists its OWNER is the ACTIVE main view and its
carried inspector property matches that view (`org-air-doc' for the project,
`org-air-item' for the board) — never the OTHER view's content."
  (skip-unless (locate-library "org-air"))
  (org-air-r25--with-board+project
   (let ((pbuf (get-buffer "*org-air-project*"))
         (bbuf (get-buffer org-air-view-buffer-name)))
     ;; Project popped -> side rail owned by the project, inspecting DOCS.
     (org-air-r25--pop pbuf)
     (org-air-r25--reconcile)
     (should (eq (org-air-rail--side-owner) pbuf))
     (with-current-buffer org-air-rail-buffer-name
       (should (eq org-air-rail--board-buffer pbuf))
       (should (eq org-air-view--inspector-property 'org-air-doc)))
     ;; Switch to the board + pop it -> the side rail re-owns to the BOARD,
     ;; inspecting ITEMS (never the project's doc inspector).
     (org-air-r25--show-in-main bbuf)
     (org-air-r25--pop bbuf)
     (org-air-r25--reconcile)
     (should (eq (org-air-rail--side-owner) bbuf))
     (with-current-buffer org-air-rail-buffer-name
       (should (eq org-air-rail--board-buffer bbuf))
       (should (eq org-air-view--inspector-property 'org-air-item))))))

(ert-deftest org-air-r25-6-toggle-idempotent-reversible ()
  "Drive `|' twice in the project: after the first there is ONE side rail
\(popped); after the second NONE (inline) with `org-air-view--rail-popped-out'
nil and no orphan `*org-air-rail*' window."
  (skip-unless (locate-library "org-air"))
  (org-air-r25--with-board+project
   (let ((pbuf (get-buffer "*org-air-project*")))
     (org-air-r25--pop pbuf)
     (org-air-r25--reconcile)
     (should (window-live-p (org-air-rail--side-window)))
     (with-current-buffer pbuf (should (eq org-air-view--rail-popped-out t)))
     (should (= (length (org-air-r25--side-windows)) 1))
     ;; Toggle back inline.
     (org-air-r25--pop pbuf)
     (org-air-r25--reconcile)
     (with-current-buffer pbuf
       (should (null org-air-view--rail-popped-out))
       (should (null org-air-view--rail-suspended)))
     (should-not (window-live-p (org-air-rail--side-window)))
     (should (null (org-air-r25--side-windows))))))

(ert-deftest org-air-r25-6-board-project-independent ()
  "Board popped, switch to project + back: the board RE-OWNS the side rail
\(back-ptr=board) and the project, left inline, never spawned a second rail.
Then project popped: switch project->board->project preserves the project's
popped state (suspended round-trip) and only ever ONE rail shows."
  (skip-unless (locate-library "org-air"))
  (org-air-r25--with-board+project
   (let ((pbuf (get-buffer "*org-air-project*"))
         (bbuf (get-buffer org-air-view-buffer-name)))
     ;; --- board popped, glance at the project, come back ---
     (org-air-r25--show-in-main bbuf)
     (org-air-r25--pop bbuf)
     (org-air-r25--reconcile)
     (should (eq (org-air-rail--side-owner) bbuf))
     (org-air-r25--show-in-main pbuf)       ; project is INLINE (never popped)
     (org-air-r25--reconcile)
     (with-current-buffer pbuf (should (null org-air-view--rail-popped-out)))
     (should (<= (length (org-air-r25--side-windows)) 1))
     (org-air-r25--show-in-main bbuf)       ; back to the board
     (org-air-r25--reconcile)
     (should (eq (org-air-rail--side-owner) bbuf)) ; board re-owns
     (with-current-buffer bbuf (org-air-rail-toggle)) ; tidy: pop board back in
     (org-air-r25--reconcile)
     ;; --- project popped, board inline, round-trip preserves popped ---
     (org-air-r25--show-in-main pbuf)
     (org-air-r25--pop pbuf)
     (org-air-r25--reconcile)
     (should (eq (org-air-rail--side-owner) pbuf))
     (org-air-r25--show-in-main bbuf)       ; board inline -> project suspended
     (org-air-r25--reconcile)
     (with-current-buffer pbuf
       (should (eq org-air-view--rail-popped-out t))   ; preference KEPT
       (should (eq org-air-view--rail-suspended t)))
     (should (<= (length (org-air-r25--side-windows)) 1))
     (org-air-r25--show-in-main pbuf)       ; return -> re-pops
     (org-air-r25--reconcile)
     (should (eq (org-air-rail--side-owner) pbuf))
     (with-current-buffer pbuf
       (should (eq org-air-view--rail-popped-out t))
       (should (null org-air-view--rail-suspended)))
     (should (= (length (org-air-r25--side-windows)) 1)))))

(ert-deftest org-air-r25-6-close-reconciles-to-inline ()
  "Project popped; delete the rail side window NATIVELY then reconcile:
`org-air-view--rail-popped-out' nil, `--rail-suspended' nil, no
`*org-air-rail*' window, and the project fell back to its INLINE rail."
  (skip-unless (locate-library "org-air"))
  (org-air-r25--with-board+project
   (let ((pbuf (get-buffer "*org-air-project*")))
     (org-air-r25--pop pbuf)
     (let ((side (org-air-rail--side-window)))
       (should (window-live-p side))
       (delete-window side))
     (should-not (window-live-p (org-air-rail--side-window)))
     ;; Keep the project the active main view, wide -> a genuine user-close.
     (select-window (get-buffer-window pbuf))
     (org-air-r25--reconcile)
     (with-current-buffer pbuf
       (should (null org-air-view--rail-popped-out))
       (should (null org-air-view--rail-suspended))
       (should (eq org-air-view--orientation 'two-pane)))
     (should-not (window-live-p (org-air-rail--side-window))))))

(ert-deftest org-air-r25-6-no-orphan-when-navigating-away ()
  "Popped project, then a NON-org-air buffer becomes the main view: the
orphan side rail is hidden and its owner suspended; returning to the project
re-pops it (no stranded cross-view rail)."
  (skip-unless (locate-library "org-air"))
  (org-air-r25--with-board+project
   (let ((pbuf (get-buffer "*org-air-project*"))
         (scratch (get-buffer-create "*org-air-r25-scratch*")))
     (org-air-r25--pop pbuf)
     (should (window-live-p (org-air-rail--side-window)))
     ;; Navigate the MAIN window away to a plain buffer.
     (select-window (org-air-r25--main-window))
     (switch-to-buffer scratch)
     (org-air-r25--reconcile)
     (should-not (window-live-p (org-air-rail--side-window)))
     (with-current-buffer pbuf
       (should (eq org-air-view--rail-popped-out t))
       (should (eq org-air-view--rail-suspended t)))
     ;; Return to the project -> re-pop.
     (org-air-r25--show-in-main pbuf)
     (org-air-r25--reconcile)
     (should (eq (org-air-rail--side-owner) pbuf))
     (with-current-buffer pbuf
       (should (null org-air-view--rail-suspended)))
     (when (buffer-live-p scratch)
       (let ((kill-buffer-query-functions nil)) (kill-buffer scratch))))))

(ert-deftest org-air-r25-6-refresh-never-strands ()
  "Popped project; a plain refresh (`--render-current') re-owns the side rail,
never duplicates it: STILL exactly one side window, owned by the project,
back-ptr intact."
  (skip-unless (locate-library "org-air"))
  (org-air-r25--with-board+project
   (let ((pbuf (get-buffer "*org-air-project*")))
     (org-air-r25--pop pbuf)
     (org-air-r25--reconcile)
     (should (= (length (org-air-r25--side-windows)) 1))
     ;; Plain refresh of the popped project.
     (select-window (get-buffer-window pbuf))
     (with-current-buffer pbuf (org-air-project--render-current))
     (org-air-r25--reconcile)
     (should (= (length (org-air-r25--side-windows)) 1))
     (should (eq (org-air-rail--side-owner) pbuf))
     (with-current-buffer org-air-rail-buffer-name
       (should (eq org-air-rail--board-buffer pbuf))))))

;;;; =====================================================================
;;;; R25-3 — remove the phantom "review" state everywhere.
;;;; =====================================================================

(ert-deftest org-air-r25-3-review-absent-everywhere ()
  "`review' is not a member of any state list / map, and an unexpected
`review' faces the unknown fallback (no dedicated face)."
  (skip-unless (locate-library "org-air"))
  (should-not (member "review" org-air-project-states))
  (should-not (member "review" org-air-project-sections))
  (should-not (member "review" org-air-project--state-display-order))
  (should-not (assoc "review" org-air-project-state-badges))
  (should-not (assoc "review" org-air-project-state-nerd-glyphs))
  (should (eq (org-air-project--state-face "review") 'org-air-face-faded)))

(ert-deftest org-air-r25-3-five-real-states-ordered ()
  "The canonical 5 states remain, in the brief's lifecycle order; both
`work-in-progress' (real, just 0 here) survives in BOTH lists."
  (skip-unless (locate-library "org-air"))
  (should (equal org-air-project-states
                 '("draft" "ready" "work-in-progress" "complete" "dropped")))
  (should (equal org-air-project-sections
                 '("draft" "ready" "work-in-progress" "complete" "dropped")))
  (should (member "work-in-progress" org-air-project-states))
  (should (member "work-in-progress" org-air-project-sections)))

(ert-deftest org-air-r25-3-display-order-minus-review ()
  "The directory/rollup display order drops `review' and the display-rank
still orders ready BEFORE draft (R20-5 order preserved, minus review)."
  (skip-unless (locate-library "org-air"))
  (should (equal org-air-project--state-display-order
                 '("ready" "work-in-progress" "complete" "dropped" "draft")))
  (should (< (org-air-project--state-display-rank "ready")
             (org-air-project--state-display-rank "draft"))))

(ert-deftest org-air-r25-3-summary-has-no-review-row ()
  "The rail Summary lists exactly the 5 real state titles and NO `Review'
row (the phantom `0  Review' line is gone)."
  (skip-unless (locate-library "org-air"))
  (let ((text (with-temp-buffer
                (org-air-project--insert-summary nil 40)
                (substring-no-properties (buffer-string)))))
    (should (string-match-p "Draft" text))
    (should (string-match-p "Ready" text))
    (should (string-match-p "Work In Progress" text))
    (should (string-match-p "Complete" text))
    (should (string-match-p "Dropped" text))
    (should-not (string-match-p "Review" text))))

(ert-deftest org-air-r25-3-orphan-review-face-gone ()
  "The dedicated `org-air-face-air-state-review' declaration is removed and
nothing references it (anti-tautology: the face no longer exists)."
  (skip-unless (locate-library "org-air"))
  (should-not (facep 'org-air-face-air-state-review)))

;;;; =====================================================================
;;;; R25-4 — Draft vs Dropped must never look alike (distinct letters).
;;;; =====================================================================

(ert-deftest org-air-r25-4-draft-not-dropped-both-layers ()
  "Draft and Dropped are distinct in BOTH layers: the rollup LETTER map
(D vs X, unchanged by R26-2) and the per-doc token — R26-2: the padded
5-col WORD cells DRAFT vs DROP (still never equal)."
  (skip-unless (locate-library "org-air"))
  (should (equal (org-air-project--state-letter "draft") "D"))
  (should (equal (org-air-project--state-letter "dropped") "X"))
  (should-not (equal (org-air-project--state-letter "draft")
                     (org-air-project--state-letter "dropped")))
  (should (equal (org-air-project--state-token "draft") "DRAFT"))
  (should (equal (org-air-project--state-token "dropped") "DROP "))
  (should-not (equal (org-air-project--state-token "draft")
                     (org-air-project--state-token "dropped"))))

(ert-deftest org-air-r25-4-letters-airctl-aligned-distinct ()
  "The five canonical letters are exactly draft=D, ready=R,
work-in-progress=W, complete=C, dropped=X — all DISTINCT (airctl rollup
positions)."
  (skip-unless (locate-library "org-air"))
  (should (equal (org-air-project--state-letter "draft") "D"))
  (should (equal (org-air-project--state-letter "ready") "R"))
  (should (equal (org-air-project--state-letter "work-in-progress") "W"))
  (should (equal (org-air-project--state-letter "complete") "C"))
  (should (equal (org-air-project--state-letter "dropped") "X"))
  (let ((letters (mapcar #'org-air-project--state-letter
                         '("draft" "ready" "work-in-progress"
                           "complete" "dropped"))))
    (should (= (length (seq-uniq letters)) 5))))

(ert-deftest org-air-r25-4-fallback-never-collides-d-d ()
  "Anti-tautology: the bare-first-char rule returns `D' for BOTH `draft' and
`dropped' (the trunk hazard); the canonical map is the AUTHORITY, pinning
D=draft and X=dropped so the badge layer never takes that colliding path for
a canonical state."
  (skip-unless (locate-library "org-air"))
  ;; the colliding rule the map replaces.
  (should (equal (upcase (substring "draft" 0 1)) "D"))
  (should (equal (upcase (substring "dropped" 0 1)) "D"))
  ;; the map pins them distinctly, and `--state-letter' reads the map first.
  (should (equal (cdr (assoc "draft" org-air-project--state-letters)) "D"))
  (should (equal (cdr (assoc "dropped" org-air-project--state-letters)) "X"))
  (should (equal (org-air-project--state-letter "dropped") "X"))
  ;; a genuinely non-canonical state still resolves (no error, distinct from
  ;; the canonical letters where its name allows).
  (should (equal (org-air-project--state-letter "unknown") "U")))

(ert-deftest org-air-r25-4-rollup-draft-dropped-distinct ()
  "The per-dir count summary over a dir with BOTH a draft and a dropped doc
contains BOTH a `D' and an `X' letter cell (never two `D')."
  (skip-unless (locate-library "org-air"))
  (let ((summary (substring-no-properties
                  (org-air-project--dir-count-summary
                   '(("draft" . 1) ("dropped" . 1)) nil))))
    (should (string-match-p "D" summary))
    (should (string-match-p "X" summary))
    ;; exactly one D (draft) and one X (dropped) — no D/D collision.
    (should (= (cl-count ?D summary) 1))
    (should (= (cl-count ?X summary) 1))))

;;;; =====================================================================
;;;; R25-2 — legible SVG state badge (bigger + BOLD single letter).
;;;; =====================================================================

(defun org-air-r25--display-image (s)
  "Return the `display' IMAGE on string S, or nil."
  (let ((disp (get-text-property 0 'display s)))
    (and (imagep disp) disp)))

(defun org-air-r25--svg-data (s)
  "Return the raw SVG string from string S's display image, or nil."
  (let ((img (org-air-r25--display-image s)))
    (and img (image-property img :data))))

(defun org-air-r25--svg-font-size (svg)
  "Return the first numeric font-size in SVG string SVG, or nil."
  (and svg (string-match "font-size=\"\\([0-9.]+\\)\"" svg)
       (string-to-number (match-string 1 svg))))

(defmacro org-air-r25--with-gui-metrics (&rest body)
  "Run BODY with a stubbed graphical frame + fixed pill char metrics."
  (declare (indent 0) (debug t))
  `(let ((org-air-view--pill-char-w 8)
         (org-air-view--pill-char-h 16))
     (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) t)))
       (should (org-air-view--svg-available-p))
       ,@body)))

(ert-deftest org-air-r25-2-badge-draws-bold-letter ()
  "R26-2 re-bless: the project state chip draws the BARE WORD label
(>DRAFT<, not the padded token and not the single letter >D<), BOLD, in
the state colour — the capsule is the uniform padded 5-col token box."
  (skip-unless (locate-library "org-air"))
  (org-air-r25--with-gui-metrics
    (let* ((badge (org-air-project--state-svg-badge "draft"))
           (svg (org-air-r25--svg-data badge)))
      (should svg)
      ;; (a) the drawn glyph is the BARE word DRAFT — not the single letter
      ;; >D<, not a padded "DRAFT " label.
      (should (string-match-p ">DRAFT<" svg))
      (should-not (string-match-p ">D<" svg))
      ;; (b) bold stays.
      (should (string-match-p "font-weight=\"bold\"" svg))
      ;; (c) the label is drawn at a real fitted size (>= the D-P1.FIT
      ;; 7px floor) — a 5-char word wants a smaller scale than the R25-2
      ;; giant letter, but never an unreadable/clipped one.
      (let ((fs (org-air-r25--svg-font-size svg)))
        (should fs)
        (should (>= fs 7))))))

(ert-deftest org-air-r25-2-badge-width-pixel-locked ()
  "The word pill changes ZERO columns: the badge image WIDTH == 5 * char-px
— R26-2 re-pin: the uniform 5-col word capsule (was 3) — IDENTICAL for
every state (equal capsules, not just equal cells), and the text-layer
cell stays within the fixed `org-air-project--state-cell-w'."
  (skip-unless (locate-library "org-air"))
  (org-air-r25--with-gui-metrics
    (dolist (state '("draft" "ready" "work-in-progress" "complete" "dropped"))
      (ert-info ((format "state %s" state))
        (let* ((badge (org-air-project--state-svg-badge state))
               (img (org-air-r25--display-image badge)))
          (should img)
          (should (= (image-property img :width)
                     (* org-air-project--state-cell-w 8))))
        (should (<= (string-width (substring-no-properties
                                   (org-air-project--state-badge-cell state)))
                    org-air-project--state-cell-w))))))

(ert-deftest org-air-r25-2-gui-chip-letters-distinct ()
  "DRIVEN (R25-4 via R25-2 overlay): the `draft' chip draws the word DRAFT
and the `dropped' chip draws DROP (R26-2 words) — still never alike."
  (skip-unless (locate-library "org-air"))
  (org-air-r25--with-gui-metrics
    (let ((draft (org-air-r25--svg-data (org-air-project--state-svg-badge "draft")))
          (drop  (org-air-r25--svg-data (org-air-project--state-svg-badge "dropped"))))
      (should draft) (should drop)
      (should (string-match-p ">DRAFT<" draft))
      (should (string-match-p ">DROP<" drop))
      (should-not (string-match-p ">DRAFT<" drop)))))

(ert-deftest org-air-r25-2-batch-token-stable ()
  "Byte guard: under --batch (no graphical frame) `--state-cell \"ready\"' has
true text `READY ' (the R26-2 padded 5-col word + separator), no display
image, no letter-only glyph."
  (skip-unless (locate-library "org-air"))
  (should-not (display-graphic-p))
  (let ((cell (org-air-project--state-cell "ready")))
    (should (equal (substring-no-properties cell) "READY "))
    (should-not (get-text-property 0 'display cell))))

(ert-deftest org-air-r25-2-board-pills-unaffected ()
  "A board pill passes NO :label/:font-weight, so it draws its OWN label
\(`#ui') and carries NO bold weight — the defaults reproduce the existing
pill exactly."
  (skip-unless (locate-library "org-air"))
  (org-air-r25--with-gui-metrics
    (let ((svg (org-air-r25--svg-data
                (org-air-view--svg-pillify "#ui" 'org-air-face-tag))))
      (should svg)
      (should (string-match-p ">#ui<" svg))
      (should-not (string-match-p "font-weight=\"bold\"" svg)))))

;;;; =====================================================================
;;;; R25-1 — lengthen the tree-connector arm to REACH the state badge.
;;;; =====================================================================

(defun org-air-r25-1--fixture-docs ()
  "Return the fixture project's docs."
  (org-air-project--collect-docs org-air-project-test-root))

(defun org-air-r25-1--insert-tree (tree width)
  "Insert TREE at WIDTH with the project render dynamics bound."
  (let* ((dims (org-air-view--char-dimensions))
         (org-air-view--pill-char-w (car dims))
         (org-air-view--pill-char-h (cdr dims))
         (org-air-view-width width)
         (docs (org-air-r25-1--fixture-docs))
         (mw (org-air-project--fit-meta-widths docs width))
         (org-air-project--meta-date-w (nth 0 mw))
         (org-air-project--meta-tags-w (nth 1 mw))
         (org-air-project--meta-origin-w (nth 2 mw)))
    (org-air-project--insert-directory-tree tree width)))

(defun org-air-r25-1--connector-pos (gutter tee corner)
  "Return the index of the LAST tee/corner connector glyph in GUTTER, or nil."
  (cl-loop for i from (1- (length gutter)) downto 0
           when (member (char-to-string (aref gutter i)) (list tee corner))
           return i))

(ert-deftest org-air-r25-1-arm-reaches-the-badge ()
  "On a leaf DOC row the run of cells BETWEEN the corner glyph and the state
cell is `box-horizontal' (`-' in batch), faced `org-air-face-air-tree', up
to exactly ONE breathing-room SPACE joining the arm to the badge (R26-1:
the deliberate inversion of R25-1's flush `no space' contract — user ask)."
  (skip-unless (locate-library "org-air"))
  (let* ((docs (org-air-r25-1--fixture-docs))
         (tree (org-air-project--directory-tree docs))
         (hbar (org-air-layout-glyph 'box-horizontal))
         (tee  (org-air-layout-glyph 'box-tee-left))
         (corner (org-air-layout-glyph 'box-bottom-left)))
    (org-air-test-with-frozen-project-path org-air-project-test-root
      (org-air-project-test--with-frozen-mtime
        (with-temp-buffer
          (org-air-r25-1--insert-tree tree 100)
          (goto-char (point-min))
          (should (re-search-forward "READY Alpha feature" nil t))
          (let* ((badge (match-beginning 0))
                 (bol (line-beginning-position))
                 (gutter (buffer-substring-no-properties bol badge))
                 (conn (org-air-r25-1--connector-pos gutter tee corner)))
            (should conn)
            (let ((run (substring gutter (1+ conn))))
              (should (> (length run) 1))
              ;; the arm run is box-horizontal…
              (should (cl-every (lambda (c) (equal (char-to-string c) hbar))
                                (substring run 0 -1)))
              ;; …and exactly ONE space joins it to the badge (no space
              ;; anywhere else in the run).
              (should (equal (substring run -1) " ")))
            ;; the first run cell is faced air-tree.
            (should (eq (get-text-property (+ bol conn 1) 'face)
                        'org-air-face-air-tree))))))))

(ert-deftest org-air-r25-1-v6-columns-frozen ()
  "The arm only repaints gutter glyphs: a leaf doc's state cell (the R26-2
word token) lands at EXACTLY margin + (* 2 (1+ depth)) for depth 0 AND
depth 1 (gutter total width unchanged), and its TITLE follows at cell-w +
1 — the R26-2 relock: the badge column is frozen, the downstream columns
sit exactly 2 right of the 3-col era."
  (skip-unless (locate-library "org-air"))
  (let* ((docs (org-air-r25-1--fixture-docs))
         (tree (org-air-project--directory-tree docs))
         (margin-w (string-width (org-air-view--item-margin))))
    (org-air-test-with-frozen-project-path org-air-project-test-root
      (org-air-project-test--with-frozen-mtime
        (with-temp-buffer
          (org-air-r25-1--insert-tree tree 100)
          (goto-char (point-min))
          (should (re-search-forward "READY Alpha feature" nil t))
          (should (= (save-excursion (goto-char (match-beginning 0))
                                     (current-column))
                     (+ margin-w (* 2 (1+ 0)))))
          ;; the title sits at badge + cell-w + 1 (the +2 relock).
          (should (= (save-excursion
                       (goto-char (+ (match-beginning 0)
                                     org-air-project--state-cell-w 1))
                       (current-column))
                     (+ margin-w (* 2 (1+ 0))
                        org-air-project--state-cell-w 1)))
          (goto-char (point-min))
          (should (re-search-forward "DRAFT Gamma context" nil t))
          (should (= (save-excursion (goto-char (match-beginning 0))
                                     (current-column))
                     (+ margin-w (* 2 (1+ 1))))))))))

(ert-deftest org-air-r25-1-corner-then-dash-run ()
  "A dir whose only children are docs (GUI stub): its LAST doc uses
`box-bottom-left' then the dash run; earlier docs `box-tee-left' then the
dash run (R24-2's corner logic unchanged, only the fill after it)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (let* ((docs (org-air-r25-1--fixture-docs))
           (d1 (nth 0 docs)) (d2 (nth 1 docs))
           (tree (list (list :dir "v9" :depth 0 :path "v9"
                             :own-docs (list d1 d2) :children nil
                             :direct-counts nil :desc-counts nil)))
           (tee    (org-air-layout-glyph 'box-tee-left))
           (corner (org-air-layout-glyph 'box-bottom-left))
           (hbar   (org-air-layout-glyph 'box-horizontal)))
      (org-air-test-with-frozen-project-path org-air-project-test-root
        (org-air-project-test--with-frozen-mtime
          (with-temp-buffer
            (org-air-r25-1--insert-tree tree 100)
            (let (doc-lines)
              (goto-char (point-min))
              (while (not (eobp))
                (when (text-property-not-all (line-beginning-position)
                                             (line-end-position) 'org-air-doc nil)
                  (push (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))
                        doc-lines))
                (forward-line 1))
              (setq doc-lines (nreverse doc-lines))
              (should (= (length doc-lines) 2))
              ;; first own doc -> tee + dash run; last -> corner + dash run.
              (should (string-match-p (concat "^ *" (regexp-quote tee)
                                              (regexp-quote hbar) "+")
                                      (nth 0 doc-lines)))
              (should (string-match-p (concat "^ *" (regexp-quote corner)
                                              (regexp-quote hbar) "+")
                                      (nth 1 doc-lines))))))))))

(ert-deftest org-air-r25-1-nested-ancestor-rail-then-arm ()
  "A depth-1 doc still carries the ancestor `box-vertical' (`|') to the LEFT
of its corner, THEN the dash run + the R26-1 single joining space reach the
badge (the ancestor rail + corner rules are unchanged; only the run's tail
cell is the breathing-room space)."
  (skip-unless (locate-library "org-air"))
  (let* ((docs (org-air-r25-1--fixture-docs))
         (doc (car docs))
         (tree (list
                (list :dir "v0.1" :depth 0 :path "v0.1" :own-docs nil
                      :direct-counts nil :desc-counts nil
                      :children
                      (list
                       (list :dir "air-template" :depth 1
                             :path "v0.1/air-template" :own-docs (list doc)
                             :direct-counts nil :desc-counts nil :children nil)
                       (list :dir "config" :depth 1 :path "v0.1/config"
                             :own-docs nil :direct-counts nil
                             :desc-counts nil :children nil)))))
         (vrail  (org-air-layout-glyph 'box-vertical))
         (hbar   (org-air-layout-glyph 'box-horizontal))
         (tee    (org-air-layout-glyph 'box-tee-left))
         (corner (org-air-layout-glyph 'box-bottom-left)))
    (org-air-test-with-frozen-project-path org-air-project-test-root
      (org-air-project-test--with-frozen-mtime
        (with-temp-buffer
          (org-air-r25-1--insert-tree tree 100)
          (let ((pos (text-property-not-all (point-min) (point-max)
                                            'org-air-doc nil)))
            (should pos)
            (goto-char pos)
            (let* ((bol (line-beginning-position))
                   (line (buffer-substring-no-properties
                          bol (line-end-position)))
                   ;; R26-2: word cells have no `[' — the badge is the first
                   ;; UPPERCASE cell (the gutter carries none).
                   (badge (string-match "[A-Z]" line))
                   (gutter (substring line 0 badge))
                   (conn (org-air-r25-1--connector-pos gutter tee corner))
                   (vpos (string-match (regexp-quote vrail) gutter)))
              (should vpos)
              (should conn)
              ;; the ancestor rail sits to the LEFT of the connector.
              (should (< vpos conn))
              ;; the run from the connector is box-horizontal, ending in the
              ;; R26-1 single joining space before the badge.
              (let ((run (substring gutter (1+ conn))))
                (should (> (length run) 1))
                (should (equal (substring run -1) " "))
                (should (cl-every (lambda (c) (equal (char-to-string c) hbar))
                                  (substring run 0 -1)))))))))))

;;;; =====================================================================
;;;; R25-5 — drop the meaningless origin/path column in the PROJECT view.
;;;; =====================================================================

(ert-deftest org-air-r25-5-project-row-has-no-origin-cell ()
  "Project rows carry NO origin/path cell: the fit allocates 0 origin columns
and no doc row shows the relpath origin (`. v0.N/...').  On trunk the row
carried the `. v0.1/...' cell."
  (skip-unless (locate-library "org-air"))
  (let ((docs (org-air-project--collect-docs org-air-project-test-root)))
    (should (= 0 (nth 2 (org-air-project--fit-meta-widths docs 100)))))
  (let ((text (mapconcat #'identity
                         (org-air-project-test--render-lines
                          'org-air-project-group-by-state 100)
                         "\n")))
    (should (string-match-p "Alpha feature" text))          ; title still shows
    (should-not (string-match-p "alpha-feature\\.org" text)) ; no relpath cell
    (should-not (string-match-p "\\. v0\\.[0-9]" text))))    ; no `. v0.N/' origin

(ert-deftest org-air-r25-5-board-still-has-origin ()
  "Independence: the BOARD keeps its origin cell (the removal is project-only)
— a board item row still shows its file origin."
  (skip-unless (locate-library "org-air"))
  (let ((org-air-show-origin t)) ; R30-3: board origin toggled ON (removal was project-only)
   (org-air-viewport-test-with-dashboard 140
    (let ((text (buffer-string)))
      (should (string-match-p "[.▤] \\(inbox\\|projects\\|personal\\)" text))))))

(ert-deftest org-air-r25-5-relpath-still-filterable ()
  "The relpath stays in the FILTER search key: a bare PATH token narrows the
docs to those under that path, even though the DISPLAY origin cell is gone."
  (skip-unless (locate-library "org-air"))
  (let* ((docs (org-air-project--collect-docs org-air-project-test-root))
         (org-air-view--tag-filter (list "air-context"))
         (org-air-filter-match 'all)
         (matched (seq-filter
                   (lambda (d)
                     (org-air-view--tokens-pass-filter-p
                      (concat (org-air-doc-name d) " "
                              (org-air-doc-relpath d))
                      (org-air-doc-tags d)))
                   docs)))
    (should matched)
    ;; only the doc UNDER v0.1/air-context/ matches the path token.
    (should (cl-every (lambda (d) (string-match-p "air-context"
                                                  (org-air-doc-relpath d)))
                      matched))
    (should (member "Gamma context" (mapcar #'org-air-doc-name matched)))
    (should (= 1 (length matched)))))

(ert-deftest org-air-r25-5-title-reclaims-width ()
  "With the origin column gone the fit allocates ocol 0 at every tier and no
rendered row overflows the width (the freed columns flow to the flex title)."
  (skip-unless (locate-library "org-air"))
  (let ((docs (org-air-project--collect-docs org-air-project-test-root)))
    (dolist (w '(80 100 120))
      (should (= 0 (nth 2 (org-air-project--fit-meta-widths docs w))))))
  (dolist (group-fn '(org-air-project-group-by-state
                      org-air-project-group-by-directory
                      org-air-project-group-by-tag))
    (let ((lines (org-air-project-test--render-lines group-fn 100)))
      (should lines)
      (dolist (l lines)
        (should (<= (string-width l) 100))))))

;;;; =====================================================================
;;;; Test-seat GAP-FILL (R25) — the impl ERTs cover R25-1 at depth 0/1 and
;;;; the R25-2 badge cell-lock in isolation; these tie the DEEPER tier and
;;;; the R25-2 svg overlay back to the R25-1 gutter (the prompt's explicit
;;;; "arm length depth>=2" + "R25-1 rails stay aligned" checks).
;;;; =====================================================================

(defun org-air-r25--doc-badge-columns (tree width)
  "Render TREE at WIDTH in the CURRENT display mode; return the list of
0-based columns where each doc row's state badge begins (R26-2: the first
UPPERCASE cell — the word cells carry no `[' and the gutter carries no
uppercase)."
  (org-air-test-with-frozen-project-path org-air-project-test-root
    (org-air-project-test--with-frozen-mtime
      (with-temp-buffer
        (org-air-r25-1--insert-tree tree width)
        (let (cols)
          (goto-char (point-min))
          (while (not (eobp))
            (when (text-property-not-all (line-beginning-position)
                                         (line-end-position) 'org-air-doc nil)
              (let* ((line (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position)))
                     (b (string-match "[A-Z]" line)))
                (when b (push b cols))))
            (forward-line 1))
          (nreverse cols))))))

(ert-deftest org-air-r25-1-arm-reaches-the-badge-at-depth-2 ()
  "R25-1 depth>=2 (test-seat gap-fill): the fixture only nests ONE dir level,
so drive a synthetic depth-2 dir (v0.1/air-template/references/, as airctl
renders it).  Its doc carries TWO `box-vertical' ancestor rails, then its
corner, then a `box-horizontal' arm ONE glyph shorter than R25-1's, joined
to the badge by the single R26-1 breathing-room space; the badge (the word
state cell) lands at the V6 column margin + (* 2 (1+ 2))."
  (skip-unless (locate-library "org-air"))
  (let* ((docs (org-air-r25-1--fixture-docs))
         (doc (car docs))
         (tree
          (list
           (list :dir "v0.1" :depth 0 :path "v0.1" :own-docs nil
                 :direct-counts nil :desc-counts nil
                 :children
                 (list
                  (list :dir "air-template" :depth 1 :path "v0.1/air-template"
                        :own-docs nil :direct-counts nil :desc-counts nil
                        :children
                        (list
                         (list :dir "references" :depth 2
                               :path "v0.1/air-template/references"
                               :own-docs (list doc) :direct-counts nil
                               :desc-counts nil :children nil)
                         (list :dir "helpers" :depth 2
                               :path "v0.1/air-template/helpers"
                               :own-docs nil :direct-counts nil
                               :desc-counts nil :children nil)))
                  (list :dir "config" :depth 1 :path "v0.1/config"
                        :own-docs nil :direct-counts nil
                        :desc-counts nil :children nil)))))
         (vrail  (org-air-layout-glyph 'box-vertical))
         (hbar   (org-air-layout-glyph 'box-horizontal))
         (tee    (org-air-layout-glyph 'box-tee-left))
         (corner (org-air-layout-glyph 'box-bottom-left))
         (margin-w (string-width (org-air-view--item-margin))))
    (org-air-test-with-frozen-project-path org-air-project-test-root
      (org-air-project-test--with-frozen-mtime
        (with-temp-buffer
          (org-air-r25-1--insert-tree tree 100)
          (let ((pos (text-property-not-all (point-min) (point-max)
                                            'org-air-doc nil)))
            (should pos)
            (goto-char pos)
            (let* ((line (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position)))
                   ;; R26-2: no `[' in a word cell — the badge is the first
                   ;; UPPERCASE cell after the (uppercase-free) gutter.
                   (badge (string-match "[A-Z]" line))
                   (gutter (substring line 0 badge))
                   (conn (org-air-r25-1--connector-pos gutter tee corner)))
              (should conn)
              ;; (a) TWO box-vertical ancestor rails LEFT of the corner.
              (should (= 2 (cl-loop for i below conn
                                    count (equal (char-to-string (aref gutter i))
                                                 vrail))))
              ;; (b) the connector is the corner (the doc is the last child
              ;; overall under `references').
              (should (equal (char-to-string (aref gutter conn)) corner))
              ;; (c) the corner->badge run is box-horizontal, one glyph
              ;; shorter, ending in the SINGLE R26-1 joining space (the only
              ;; space in the run).
              (let ((run (substring gutter (1+ conn))))
                (should (> (length run) 1))
                (should (equal (substring run -1) " "))
                (should (cl-every (lambda (c) (equal (char-to-string c) hbar))
                                  (substring run 0 -1))))
              ;; (d) V6 column at the deeper tier: margin + 2*(1+2).
              (should (= badge (+ margin-w (* 2 (1+ 2))))))))))))

(ert-deftest org-air-r25-2-svg-badge-keeps-r25-1-columns ()
  "R25-2 x R25-1 (test-seat gap-fill): turning the bold word-pill SVG badge
on (the GUI default) keeps the R25-1 gutter aligned.  The badge is a
`display' overlay sized to EXACTLY the 5 char-px text cell (R26-2 re-pin),
so rendering the directory tree on a graphical frame lands every doc's
badge at the SAME column as the batch render (the Unicode rails + the word
capsule never shift the V6 columns); the badge image is exactly the
text-cell width, so it can never bleed into the R26-1 arm."
  (skip-unless (locate-library "org-air"))
  (let* ((docs (org-air-r25-1--fixture-docs))
         (tree (org-air-project--directory-tree docs))
         (batch-cols (org-air-r25--doc-badge-columns tree 100)))
    (should (> (length batch-cols) 2))
    (org-air-r25--with-gui-metrics
      (let ((gui-cols (org-air-r25--doc-badge-columns tree 100)))
        ;; every doc badge lands at the IDENTICAL column GUI vs batch.
        (should (equal gui-cols batch-cols))
        ;; the SVG badge occupies exactly the text cell (3 char-px), so it
        ;; can never bleed into the R25-1 arm.
        (let ((img (org-air-r25--display-image
                    (org-air-project--state-svg-badge "ready"))))
          (should img)
          (should (= (image-property img :width)
                     (* org-air-project--state-cell-w
                        org-air-view--pill-char-w))))))))

(provide 'org-air-round25-test)
;;; org-air-round25-test.el ends here
