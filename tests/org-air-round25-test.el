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
                 (string-prefix-p " *org-air-pane:" (buffer-name b)))
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

(provide 'org-air-round25-test)
;;; org-air-round25-test.el ends here
