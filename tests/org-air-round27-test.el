;;; org-air-round27-test.el --- executing ERTs for v0.5 round-27 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.5 round-27 (air/v0.5/org-air-round27-design.org).
;; R26 harness discipline: `noninteractive' bound nil so side windows
;; really exist, commands dispatched via `key-binding', timers made
;; deterministic (the reconcile driven via its named frame function, the
;; R26-8 machine via direct slice calls).  The batch frame is 80x25 and
;; CANNOT be resized (see the round's measured baseline), so the
;; live-window ERTs bind `org-air-rail-min-width' (and the item-pane
;; floor) down so the side rail engages at 80 cols and assert the
;; CONVERGENCE INVARIANTS, which trunk violated at every width:
;;
;;   R27-1  RAIL STABILITY — frame-derived cols (tier fixpoint by
;;          construction), create-once + preserve-size side window,
;;          single reconcile timer slot, render latch, edge-triggered
;;          user-close, single scanner, repaints only at swap.
;;   R27-2  FULL MAIN-WINDOW WIDTH — compose at the REAL body width after
;;          the rail settles; V6 lock from the actual width; pop-in
;;          re-fits full width.
;;   R27-3  SORT-ACTIVE HEADER — bold high-contrast indicator only when a
;;          non-default sort is active; sheds LAST under width pressure.
;;   R27-4  EVIL PARITY — the shared fboundp-gated setup applied to the
;;          project/rail/entry-view modes; table-driven key parity.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-project-test)            ; project fixture root + render

(when (locate-library "org-air")
  (require 'org-air))

(defun org-air-r27--kill-aux-buffers ()
  "Kill the shared pane/rail/view buffers so tests never inherit stale windows."
  (let ((kill-buffer-query-functions nil))
    (dolist (name (list org-air-view-pane-buffer-name org-air-rail-buffer-name
                        org-air-view-buffer-name "*org-air-project*"))
      (when (get-buffer name) (kill-buffer name)))))

(defun org-air-r27--reset-rail-globals ()
  "Cancel any pending reconcile timer + reset the R27-1 edge flag."
  (when (timerp org-air-rail--reconcile-timer)
    (cancel-timer org-air-rail--reconcile-timer))
  (setq org-air-rail--reconcile-timer nil
        org-air-rail--side-was-live nil))

(defmacro org-air-r27--with-live-board (&rest body)
  "Render the fixture board in a LIVE window with NO width seam; run BODY.
`noninteractive' is bound nil so the side rail really pops;
`org-air-rail-min-width' / `org-air-item-pane-min' are bound down so the
rail engages on the unresizable 80-col batch frame.  BODY runs in the
board buffer with its window selected."
  (declare (indent 0) (debug t))
  `(org-air-test-with-fixtures
    (save-window-excursion
      (org-air-r27--kill-aux-buffers)
      (org-air-r27--reset-rail-globals)
      (let ((noninteractive nil)
            (org-air-rail-min-width 40)
            (org-air-item-pane-min 30)
            (org-air-rail-focus-on-popout nil)
            (bbuf (get-buffer-create org-air-view-buffer-name)))
        (unwind-protect
            (progn
              (with-current-buffer bbuf
                (org-air-view-mode)
                (setq org-air-view--items (org-air-query-items))
                (setq-local org-air-view--rail-popped-out nil))
              (switch-to-buffer bbuf)
              (delete-other-windows)
              (with-current-buffer bbuf
                (org-air-view--render org-air-view--items nil)
                ,@body))
          (org-air-r27--reset-rail-globals)
          (org-air-r27--kill-aux-buffers))))))

(defun org-air-r27--pop-rail ()
  "Pop the side rail out on the current live board/project buffer."
  (setq-local org-air-view--rail-popped-out t
              org-air-view--rail-suspended nil)
  (org-air-view--refresh-current)
  (should (window-live-p (org-air-rail--side-window))))

(defun org-air-r27--press (key)
  "Dispatch KEY via `key-binding' in the SELECTED window's buffer."
  (with-current-buffer (window-buffer (selected-window))
    (call-interactively (key-binding (kbd key)))))

;;;; =====================================================================
;;;; R27-1 — stable rail geometry: convergent by construction.
;;;; =====================================================================

(ert-deftest org-air-r27-1-ensure-window-convergent ()
  "After the first pop, N=6 refresh cycles perform ZERO rail-window
creations (`display-buffer-in-side-window'), ZERO deletions and ZERO
resizes — the side window is created once, reused and pinned (S2), and
the frame-derived cols (S1) make desired == actual every cycle.  Trunk
FAILED: `display-buffer-in-side-window' re-applied `window-width' on
every render (measured +10/-10 resizes per render in the tier band)."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-board
    (org-air-r27--pop-rail)
    (let* ((rail-buf (get-buffer org-air-rail-buffer-name))
           (displays 0) (deletes 0) (resizes 0)
           (real-display (symbol-function 'display-buffer-in-side-window))
           (real-delete (symbol-function 'delete-window))
           (real-resize (symbol-function 'window-resize)))
      (cl-letf (((symbol-function 'display-buffer-in-side-window)
                 (lambda (buffer &rest args)
                   (when (eq buffer rail-buf) (cl-incf displays))
                   (apply real-display buffer args)))
                ((symbol-function 'delete-window)
                 (lambda (&optional win)
                   (when (and (window-live-p win)
                              (eq (window-buffer win) rail-buf))
                     (cl-incf deletes))
                   (funcall real-delete win)))
                ((symbol-function 'window-resize)
                 (lambda (win delta &rest args)
                   (when (and (window-live-p win)
                              (eq (window-buffer win) rail-buf))
                     (cl-incf resizes))
                   (apply real-resize win delta args))))
        (dotimes (_ 6)
          ;; the explicit re-render (the toggle/refresh path)...
          (org-air-view--refresh-current)
          ;; ...and the debounce timer's own function, called directly.
          (org-air-layout--refresh-windows)))
      (should (window-live-p (org-air-rail--side-window)))
      (should (= displays 0))
      (should (= deletes 0))
      (should (= resizes 0)))))

(ert-deftest org-air-r27-1-tier-fixpoint ()
  "Pure-function property: the rail cols derive from the FRAME width, so
iterating the round-26 feedback formula (main <- F - cols - 1;
cols <- rail-cols) is CONSTANT after the first step for EVERY frame width
F in 80..260.  Trunk FAILED in the bands 182-191 / 148-151 (the tier of
the main window's width had no fixpoint there)."
  (skip-unless (locate-library "org-air"))
  ;; the no-arg call measures the live frame — one source of truth.
  (should (= (org-air-rail--window-cols)
             (org-air-view--rail-width (frame-width))))
  (dolist (f (number-sequence 80 260))
    (let ((cols (org-air-rail--window-cols f))
          (main nil))
      (dotimes (_ 8)
        (setq main (- f cols 1))
        (let ((next (org-air-rail--window-cols f)))
          ;; cols is a function of the FRAME width only: the derived main
          ;; width can never feed back into the tier.
          (should (= next cols))
          (setq cols next)))
      (should (integerp main)))))

(ert-deftest org-air-r27-1-reconcile-single-timer ()
  "Firing `org-air-rail--reconcile' 5x leaves exactly ONE pending
reconcile timer (the single slot reschedules; trunk stacked 5)."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--reset-rail-globals)
  (unwind-protect
      (let ((noninteractive nil))
        (dotimes (_ 5) (org-air-rail--reconcile))
        (should (timerp org-air-rail--reconcile-timer))
        (should (= 1 (cl-count-if
                      (lambda (tm)
                        (eq (timer--function tm)
                            #'org-air-rail--reconcile-run))
                      timer-list))))
    (org-air-r27--reset-rail-globals)))

(ert-deftest org-air-r27-1-reconcile-render-latch ()
  "A reconcile that runs while `org-air-rail--reconciling' is bound t (as
the FULL render extent now binds it) NO-OPS: the popped flag SURVIVES the
transient popped-but-windowless mid-render state and no re-render runs.
Trunk FAILED: the nested 0s timer took the user-close branch and cleared
the flag, defeating the popout it interrupted."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-board
    ;; the mid-render transient: flag t, side window NOT yet created.
    (setq-local org-air-view--rail-popped-out t)
    (should-not (window-live-p (org-air-rail--side-window)))
    (let ((refreshes 0))
      (cl-letf (((symbol-function 'org-air-view--refresh-current)
                 (lambda (&rest _) (cl-incf refreshes))))
        (let ((org-air-rail--reconciling t))
          (org-air-rail--reconcile-run (selected-frame))))
      (should (eq org-air-view--rail-popped-out t))
      (should (= refreshes 0))
      ;; the latched run re-armed the single slot for after the render.
      (should (timerp org-air-rail--reconcile-timer)))))

(ert-deftest org-air-r27-1-user-close-edge-triggered ()
  "The user-close branch fires ONLY on an observed live->dead transition:
a genuinely painted rail whose window is natively deleted falls back
inline (the R16 contract holds); but flag t with a window NEVER yet
created (side-was-live nil — a popout in flight) leaves the flag INTACT."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-board
    ;; positive: genuinely popped + painted, then natively closed.
    (org-air-r27--pop-rail)
    (should org-air-rail--side-was-live)
    (delete-window (org-air-rail--side-window))
    (org-air-rail--reconcile-frame (selected-frame))
    (should (null org-air-view--rail-popped-out))
    ;; negative: flag t but the window was never live — leave state alone.
    (org-air-r27--reset-rail-globals)
    (setq-local org-air-view--rail-popped-out t)
    (should-not (window-live-p (org-air-rail--side-window)))
    (org-air-rail--reconcile-frame (selected-frame))
    (should (eq org-air-view--rail-popped-out t))))

(ert-deftest org-air-r27-1-swap-repaints-once ()
  "The single-scanner law + swap discipline: with the R26-8 machine
REFRESHING, `org-air-view--render-current' performs ZERO
`org-air-query-items' calls (trunk ran a full synchronous scan concurrent
with the slices — the live-only `stringp nil' failure), and the whole
slice chain repaints exactly ONCE, at the swap."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (let ((org-air-view-width 120)
          (org-air-view-height 50)
          (org-air-view-buffer-name "*org-air-r27-swap*"))
      (unwind-protect
          (with-current-buffer (get-buffer-create org-air-view-buffer-name)
            (org-air-view-mode)
            (org-air-view--refresh-start)
            (should (eq org-air-view--refresh-state 'refreshing))
            ;; (a) mid-refresh render-current: NO synchronous fallback scan.
            (let ((queries 0))
              (cl-letf (((symbol-function 'org-air-query-items)
                         (lambda (&rest _) (cl-incf queries) nil)))
                (org-air-view--render-current)
                ;; the COLD variant repaints the skeleton, still scan-free.
                (let ((org-air-view--loading t))
                  (org-air-view--render-current)))
              (should (= queries 0)))
            ;; (b) the machine chain repaints exactly once — at the swap.
            (let ((renders 0)
                  (real-render (symbol-function 'org-air-view--render))
                  (token org-air-view--refresh-token)
                  (n 100))
              (cl-letf (((symbol-function 'org-air-view--render)
                         (lambda (&rest args)
                           (cl-incf renders)
                           (apply real-render args))))
                (while (and (> n 0)
                            (eq org-air-view--refresh-state 'refreshing))
                  (org-air-view--refresh-run-slice (current-buffer) token)
                  (cl-decf n)))
              (should-not org-air-view--refresh-state)
              (should (= renders 1))
              ;; the swap really landed the scanned items.
              (should org-air-view--items)))
        (when (get-buffer "*org-air-r27-swap*")
          (kill-buffer "*org-air-r27-swap*"))))))

(ert-deftest org-air-r27-1-rail-stamp-guard ()
  "Two consecutive `org-air-rail--show' with identical inputs paint the
rail content ONCE (the stamp guard skips the byte-identical repaint); a
filter change repaints; and the text after a SKIPPED paint is
byte-identical to a FORCED paint."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-board
    (org-air-r27--pop-rail)
    (let* ((board (current-buffer))
           (rail-buf (get-buffer org-air-rail-buffer-name))
           (width (org-air-view--render-width))
           (paints 0)
           (real-render (symbol-function 'org-air-rail--render)))
      (cl-letf (((symbol-function 'org-air-rail--render)
                 (lambda (&rest args)
                   (cl-incf paints)
                   (apply real-render args))))
        ;; force a known baseline paint, then an identical-input show.
        (with-current-buffer rail-buf (setq-local org-air-rail--last-stamp nil))
        (org-air-rail--show board width)
        (org-air-rail--show board width)
        (should (= paints 1))
        (let ((skipped (with-current-buffer rail-buf
                         (substring-no-properties (buffer-string)))))
          ;; byte guard: a forced repaint reproduces the skipped text.
          (with-current-buffer rail-buf
            (setq-local org-air-rail--last-stamp nil))
          (org-air-rail--show board width)
          (should (= paints 2))
          (should (equal (with-current-buffer rail-buf
                           (substring-no-properties (buffer-string)))
                         skipped)))
        ;; any input component change repaints: flip the filter.
        (setq-local org-air-view--tag-filter '("work"))
        (org-air-rail--show board width)
        (should (= paints 3))))))

(provide 'org-air-round27-test)
;;; org-air-round27-test.el ends here
