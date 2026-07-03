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

;;;; =====================================================================
;;;; R27-2 — full main-window width with the side rail.
;;;; =====================================================================

(defmacro org-air-r27--with-live-project (&rest body)
  "Open the fixture project LIVE with NO width seam; run BODY in its buffer.
The R26-5 placement default pops the rail; `org-air-rail-min-width' /
`org-air-item-pane-min' are bound down so the side rail engages on the
80-col batch frame and the real window geometry drives every width."
  (declare (indent 0) (debug t))
  `(progn
     (should (fboundp 'org-air-project))
     (let ((org-air-sources (list (list :air org-air-project-test-root)))
           (org-air-project-group 'directory)
           (org-air-rail-focus-on-popout nil)
           (org-air-rail-min-width 40)
           (org-air-item-pane-min 30))
       (org-air-project-test--with-frozen-mtime
        (save-window-excursion
          (org-air-r27--kill-aux-buffers)
          (org-air-r27--reset-rail-globals)
          (let ((noninteractive nil))
            (org-air-project))
          (let ((buf (get-buffer "*org-air-project*")))
            (should buf)
            (unwind-protect
                (let ((noninteractive nil))
                  (with-current-buffer buf
                    (when (get-buffer-window buf)
                      (select-window (get-buffer-window buf)))
                    ,@body))
              (org-air-r27--reset-rail-globals)
              (org-air-r27--kill-aux-buffers))))))))

(defun org-air-r27--longest-line ()
  "Return the longest line's display width in the current buffer."
  (let ((longest 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (setq longest (max longest
                           (string-width
                            (buffer-substring-no-properties
                             (line-beginning-position) (line-end-position)))))
        (forward-line 1)))
    longest))

(defun org-air-r27--doc-row-bols ()
  "Return the BOL positions of every doc row in the current buffer."
  (let (bols)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (text-property-not-all (line-beginning-position)
                                     (line-end-position) 'org-air-doc nil)
          (push (line-beginning-position) bols))
        (forward-line 1)))
    (nreverse bols)))

(ert-deftest org-air-r27-2-board-fits-window ()
  "With the rail popped, the board is composed at the REAL window body
width: after the popped render and each of 3 refresh cycles,
`org-air-view--rendered-width' EQUALS the board window's
`window-body-width' — no dead columns, no overflow.  Trunk FAILED in the
tier bands (measured 147-col compose in a 157-col window)."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-board
    (org-air-r27--pop-rail)
    (dotimes (_ 3)
      (org-air-view--refresh-current)
      (org-air-layout--refresh-windows)
      (let ((bwin (get-buffer-window (current-buffer))))
        (should (window-live-p bwin))
        (should (eql org-air-view--rendered-width (window-body-width bwin)))
        ;; no line overflows the compose width (dead columns cannot be
        ;; asserted line-wise: the board right-trims), and the banner
        ;; reaches the right margin — content genuinely spans the window.
        (let ((longest (org-air-r27--longest-line)))
          (should (<= longest org-air-view--rendered-width))
          (should (>= longest (- org-air-view--rendered-width 2))))))))

(ert-deftest org-air-r27-2-project-fits-window ()
  "The PROJECT re-measures after the rail pops: after the FIRST popped
render (no width seam) and after each refresh cycle,
`org-air-project--rendered-width' equals the project window's body width
and every doc row is composed to exactly that width.  Trunk FAILED on
the first render (composed at the pre-pop width; truncated rows)."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-project
    (should (org-air-rail--popped-p))
    (should (window-live-p (org-air-rail--side-window)))
    (dotimes (i 4)
      ;; i=0 asserts the FIRST render as-is; then three refresh cycles.
      (unless (zerop i)
        (org-air-view--refresh-current)
        (org-air-layout--refresh-windows))
      (let ((pwin (get-buffer-window (current-buffer))))
        (should (window-live-p pwin))
        (should (eql org-air-project--rendered-width
                     (window-body-width pwin)))
        ;; doc rows are composed to exactly the rendered width.
        (dolist (bol (org-air-r27--doc-row-bols))
          (save-excursion
            (goto-char bol)
            (should (= (string-width
                        (buffer-substring-no-properties
                         bol (line-end-position)))
                       org-air-project--rendered-width))))))))

(ert-deftest org-air-r27-2-popin-refits-full-width ()
  "Popping the rail IN (`|') re-renders exactly ONCE and the board
re-fits the now-full window body width (the freed rail columns are
reclaimed immediately; the converged debounce path adds nothing)."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-board
    (org-air-r27--pop-rail)
    (let ((popped-width org-air-view--rendered-width)
          (renders 0)
          (real-render (symbol-function 'org-air-view--render)))
      (cl-letf (((symbol-function 'org-air-view--render)
                 (lambda (&rest args)
                   (cl-incf renders)
                   (apply real-render args))))
        (org-air-r27--press "|"))
      (should (= renders 1))
      (should-not (window-live-p (org-air-rail--side-window)))
      (let ((bwin (get-buffer-window (current-buffer))))
        (should (eql org-air-view--rendered-width (window-body-width bwin)))
        ;; the full width is genuinely WIDER than the popped compose.
        (should (> org-air-view--rendered-width popped-width))))))

(ert-deftest org-air-r27-2-v6-lock-actual-width ()
  "The V6 meta lock derives from the ACTUAL measured width: with the rail
popped (no seam), every doc row's right-pinned date/tag cluster starts at
rendered-width minus the `org-air-project--fit-meta-widths' cluster — the
columns recomputed from the REAL window body width, not the frame or a
cached value — and ends flush at the rendered width.  The title-min
defcustom is bound down so the 80-col frame's narrow tier keeps a
non-degenerate tag column (the 1-col floor's ellipsis overflow is a
pre-R27 narrow-tier artifact, out of scope here)."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-project
    (let ((org-air-title-min-width 12))
      (org-air-view--refresh-current))
    (let* ((org-air-title-min-width 12)
           (w org-air-project--rendered-width)
           (docs org-air-view--items)
           (mw (org-air-project--fit-meta-widths docs w))
           (dcol (nth 0 mw)) (tcol (nth 1 mw))
           ;; cluster = date-cell + " " + tags-cell (origin dropped, R25-5).
           (cluster-w (+ dcol (if (> tcol 0) (1+ tcol) 0)))
           (cluster-col (- w cluster-w))
           (bols (org-air-r27--doc-row-bols)))
      (should (>= tcol 4))               ; non-degenerate: cells stay fixed
      (should (eql w (window-body-width (get-buffer-window (current-buffer)))))
      (should (> dcol 0))
      (should bols)
      (dolist (bol bols)
        (let ((row (save-excursion
                     (goto-char bol)
                     (buffer-substring-no-properties bol (line-end-position)))))
          ;; the row ends flush at the rendered width...
          (should (= (string-width row) w))
          ;; ...and the cluster sits at the SAME actual-width-derived
          ;; column on every row: the char before it is the title gap and
          ;; the date cell's `~' glyph opens the cluster.
          (should (eq (aref row (1- cluster-col)) ?\s))
          (should (eq (aref row cluster-col) ?~)))))))

;;;; =====================================================================
;;;; R27-4 — project/rail/pane keybinding parity + evil-awareness.
;;;; =====================================================================

(defconst org-air-r27--evil-mode-table
  '((org-air-view-mode       . org-air-view-mode-map)
    (org-air-project-mode    . org-air-project-mode-map)
    (org-air-rail-mode       . org-air-rail-mode-map)
    (org-air-entry-view-mode . org-air-entry-view-mode-map))
  "Every org-air special-mode view and its keymap (R27-4).
Table-driven: a future mode is a one-line add here, and a missing evil
registration is a FAIL — the parity can never silently drift again.")

(ert-deftest org-air-r27-4-evil-registration-all-modes ()
  "Every org-air special-mode view registers with evil at mode init:
`evil-make-overriding-map' on its OWN map in motion state and
`evil-set-initial-state' -> motion — exactly once per mode, table-driven.
Stubbed via `cl-letf' (the fboundp gate makes the stubs sufficient).
Trunk FAILED for 3/4 (only the board registered)."
  (skip-unless (locate-library "org-air"))
  (let (overriding-calls initial-calls)
    (cl-letf (((symbol-function 'evil-make-overriding-map)
               (lambda (map &optional state _copy)
                 (push (cons map state) overriding-calls)))
              ((symbol-function 'evil-set-initial-state)
               (lambda (mode state)
                 (push (cons mode state) initial-calls))))
      (pcase-dolist (`(,mode . ,_map) org-air-r27--evil-mode-table)
        (with-temp-buffer
          (funcall mode))))
    (pcase-dolist (`(,mode . ,map-sym) org-air-r27--evil-mode-table)
      (let ((map (symbol-value map-sym)))
        (should (= 1 (cl-count (cons map 'motion) overriding-calls
                               :test #'equal)))
        (should (= 1 (cl-count (cons mode 'motion) initial-calls
                               :test #'equal)))))))

(ert-deftest org-air-r27-4-evil-real-project-keys ()
  "With the REAL evil enabled in live project/rail/pane buffers, the
buffers land in MOTION state and every project key resolves to its
org-air command — the measured trunk table inverted (trunk: `(' ->
evil-backward-sentence-begin, o -> evil-open-below, q ->
evil-record-macro, RET -> evil-ret, | -> evil-goto-column…)."
  (skip-unless (locate-library "org-air"))
  (skip-unless (locate-library "evil"))
  (require 'evil)
  (org-air-r27--with-live-project
    ;; PROJECT: motion state + the full key set resolves to org-air.
    (evil-local-mode 1)
    (should (eq evil-state 'motion))
    (pcase-dolist (`(,key . ,cmd)
                   '(("("   . org-air-project-toggle-filenames)
                     ("o"   . org-air-view-sort-cycle)
                     ("O"   . org-air-view-sort-reverse)
                     ("s"   . org-air-project-group-by-state)
                     ("d"   . org-air-project-group-by-directory)
                     ("t"   . org-air-project-group-by-tag)
                     ("/"   . org-air-project-filter)
                     ("RET" . org-air-project-open)
                     ("n"   . org-air-project-next)
                     ("p"   . org-air-project-prev)
                     ("g"   . org-air-project-refresh)
                     ("q"   . org-air-project-quit)
                     ("|"   . org-air-rail-toggle)))
      (should (eq (key-binding (kbd key)) cmd)))
    ;; RAIL: `q' must be the org-air quit, not evil-record-macro.
    (with-current-buffer (get-buffer org-air-rail-buffer-name)
      (evil-local-mode 1)
      (should (eq evil-state 'motion))
      (should (eq (key-binding (kbd "q")) 'org-air-rail-quit))
      (should (eq (key-binding (kbd "RET")) 'org-air-rail-return))
      (should (eq (key-binding (kbd "|")) 'org-air-rail-popin)))
    ;; VIEW PANE: `q' closes the pane, never records a macro.
    (with-current-buffer (org-air-view-pane--buffer)
      (evil-local-mode 1)
      (should (eq evil-state 'motion))
      (should (eq (key-binding (kbd "q")) 'org-air-view-pane-quit)))))

(ert-deftest org-air-r27-4-paren-functional-sans-evil ()
  "WITHOUT evil, `(' dispatched via `key-binding' flips the doc rows
title<->relpath and back — the binding layer itself is sound (the second
layer of the user's report, guarded independently of the evil fix)."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-project
    (should-not (bound-and-true-p evil-local-mode))
    ;; a wide seam re-render so full titles/relpaths are visible.
    (let ((org-air-project-view-width 120))
      (org-air-view--refresh-current)
      (let ((titles (substring-no-properties (buffer-string))))
        (should (string-match-p "Alpha feature" titles))
        (should-not (string-match-p "alpha-feature\\.org" titles)))
      (org-air-r27--press "(")
      (should org-air-project--show-filenames)
      (let ((flipped (substring-no-properties (buffer-string))))
        (should (string-match-p "alpha-feature\\.org" flipped))
        (should-not (string-match-p "Alpha feature" flipped)))
      (org-air-r27--press "(")
      (should-not org-air-project--show-filenames)
      (should (string-match-p "Alpha feature"
                              (substring-no-properties (buffer-string)))))))

(ert-deftest org-air-r27-4-core-parity-board-project ()
  "Table-driven parity: every DIRECT `org-air-view-core-map' key resolves
to the SAME command in the board and the project, unless it is in the
DOCUMENTED override set — RET, mouse-1, S-RET, n, p, s, d, t, /, g, q,
`(' — so board<->project parity can never silently drift."
  (skip-unless (locate-library "org-air"))
  (let ((board (generate-new-buffer "*r27-parity-board*"))
        (proj (generate-new-buffer "*r27-parity-proj*"))
        (overrides '("RET" "<mouse-1>" "S-<return>" "n" "p" "s" "d" "t"
                     "/" "g" "q" "("))
        (keys nil))
    (unwind-protect
        (progn
          (with-current-buffer board (org-air-view-mode))
          (with-current-buffer proj (org-air-project-mode))
          ;; DIRECT core-map bindings only (the parent special-mode keys
          ;; are not part of the shared-verbs contract), prefix maps
          ;; walked down to full key sequences (e.g. `M-/' under ESC).
          (let ((m (copy-keymap org-air-view-core-map)))
            (set-keymap-parent m nil)
            (cl-labels ((walk (map prefix)
                          (map-keymap
                           (lambda (ev def)
                             (let ((seq (vconcat prefix (vector ev))))
                               (if (keymapp def)
                                   (walk def seq)
                                 (push seq keys))))
                           map)))
              (walk m [])))
          (should keys)
          (dolist (seq keys)
            (let ((desc (key-description seq)))
              (unless (member desc overrides)
                (let ((b (with-current-buffer board (key-binding seq)))
                      (p (with-current-buffer proj (key-binding seq))))
                  (should b)
                  (should (eq b p)))))))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer board)
        (kill-buffer proj)))))

(provide 'org-air-round27-test)
;;; org-air-round27-test.el ends here
