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
(require 'org-air-viewport-helpers)        ; R27-3: dashboard render + as-gui

(when (locate-library "org-air")
  (require 'org-air))

(defun org-air-r27--kill-aux-buffers ()
  "Kill the shared pane/rail/view buffers so tests never inherit stale windows."
  (let ((kill-buffer-query-functions nil))
    (dolist (name (list org-air-view-pane-buffer-name org-air-rail-buffer-name
                        org-air-view-buffer-name "*org-air-project*"))
      (when (get-buffer name) (kill-buffer name)))))

(defun org-air-r27--reset-rail-globals ()
  "Cancel any pending reconcile timer + reset the R27-1 edge flag.
Also sweeps ORPHANED `org-air-rail--reconcile-run' timers (a test that
fired the slot's body directly leaves the scheduled timer object in
`timer-list'), so the single-slot assertions can never see pollution
from an earlier test."
  (when (timerp org-air-rail--reconcile-timer)
    (cancel-timer org-air-rail--reconcile-timer))
  (dolist (tm (copy-sequence timer-list))
    (when (eq (timer--function tm) #'org-air-rail--reconcile-run)
      (cancel-timer tm)))
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
;;;; R27-3 — sort-active header indicator: white bold, only when it matters.
;;;; =====================================================================

(defun org-air-r27-3--banner-key-face-run (key-name)
  "Assert the first-line sort badge for KEY-NAME is fully `sort-active'.
Finds KEY-NAME on the banner (line 1) of the current buffer and checks
the marker glyph two columns before it, every KEY-NAME char and the
direction arrow one column after it all carry `org-air-face-sort-active'."
  (save-excursion
    (goto-char (point-min))
    (let ((eol (line-end-position)))
      (should (search-forward key-name eol t))
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        ;; marker glyph sits at <mk> SPC <key>.
        (should (eq (get-text-property (- beg 2) 'face)
                    'org-air-face-sort-active))
        (cl-loop for pos from beg below end do
                 (should (eq (get-text-property pos 'face)
                             'org-air-face-sort-active)))
        ;; direction arrow sits at <key> SPC <arrow>.
        (should (eq (get-text-property (1+ end) 'face)
                    'org-air-face-sort-active))))))

(ert-deftest org-air-r27-3-board-active-sort-bold ()
  "A cycled (non-default) board sort renders the banner badge in
`org-air-face-sort-active' — marker glyph, key name and arrow — through
the REAL render path (`o' dispatched via `key-binding'); cycling back to
the default removes the segment entirely (quiet, byte-identical banner).
Trunk styled the badge `faded'/`summary-label' — WCAG-failing, invisible."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 160
      (should (org-air-view--sort-default-p))
      ;; `o' -> priority: the badge appears, every glyph bold-active.
      (call-interactively (key-binding (kbd "o")))
      (should-not (org-air-view--sort-default-p))
      (should (eq org-air-view--sort-key 'priority))
      (org-air-r27-3--banner-key-face-run "priority")
      ;; cycle back to the default (title -> recency -> date): ABSENT.
      (dotimes (_ 3) (call-interactively (key-binding (kbd "o"))))
      (should (org-air-view--sort-default-p))
      (save-excursion
        (goto-char (point-min))
        (should-not (search-forward (org-air-layout-glyph 'sort-key)
                                    (line-end-position) t))))))

(ert-deftest org-air-r27-3-project-active-sort-bold ()
  "The PROJECT header badge is quiet at the default sort (`name'
ascending, key faced `org-air-face-summary-label' — unchanged bytes and
faces) and goes `org-air-face-sort-active' bold after `o' cycles to a
non-default key.  Trunk styled default and active IDENTICALLY."
  (skip-unless (locate-library "org-air"))
  (org-air-project-test--render
    ;; default: `<mk> name <arrow>' present, key quietly summary-label.
    (goto-char (point-min))
    (let ((eol (line-end-position)))
      (should (search-forward "name" eol t))
      (should (eq (get-text-property (match-beginning 0) 'face)
                  'org-air-face-summary-label))
      (should (eq (get-text-property (- (match-beginning 0) 2) 'face)
                  'org-air-face-faded)))
    ;; `o' -> created (non-default): the whole badge goes bold-active.
    (call-interactively (key-binding (kbd "o")))
    (should (eq org-air-view--sort-key 'created))
    (org-air-r27-3--banner-key-face-run "created")))

(ert-deftest org-air-r27-3-shed-keeps-active-sort ()
  "Under width pressure with a long active filter + scope + a NON-default
sort, the banner sheds filter and scope FIRST and the sort badge
SURVIVES.  Trunk shed the sort segment first — precisely the state the
user needs was the first casualty of a narrow window."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 160
      (setq-local org-air-view--tag-filter
                  '("averyveryverylongfiltertagname0123456789"))
      (setq-local org-air-view--scope
                  '(:tag "anotherveryverylongscopetagname098765432"))
      (setq-local org-air-view--sort-key 'priority)
      ;; squeeze: date + count + sort fit; filter/scope cannot.
      (let ((org-air-view-width 70))
        (org-air-view--render-current))
      (save-excursion
        (goto-char (point-min))
        (let ((banner (buffer-substring-no-properties
                       (point) (line-end-position))))
          ;; the ACTIVE sort badge survived the squeeze...
          (should (string-match-p "priority" banner))
          ;; ...while the longer, earlier-shed segments gave way.
          (should-not (string-match-p "longfiltertagname" banner))
          (should-not (string-match-p "longscopetagname" banner))))
      ;; and it survived FACED: the badge is still bold-active.
      (org-air-r27-3--banner-key-face-run "priority"))))

(ert-deftest org-air-r27-3-face-contrast ()
  "`org-air-face-sort-active' declares `:weight bold' AND an explicit
foreground for BOTH background classes (light + dark), with a plain-bold
terminal fallback — guarding a theme regressing it to the faded idiom
the faces file documents as failing WCAG AA."
  (skip-unless (locate-library "org-air"))
  (should (facep 'org-air-face-sort-active))
  (let ((spec (get 'org-air-face-sort-active 'face-defface-spec)))
    (should spec)
    (dolist (bg '(light dark))
      (let ((atts (cl-loop for (display atts) in spec
                           when (and (listp display)
                                     (member (list 'background bg) display))
                           return atts)))
        (should atts)
        (should (stringp (plist-get atts :foreground)))
        (should (eq (plist-get atts :weight) 'bold))))
    ;; low-colour fallback keeps the weight.
    (should (eq (plist-get (cadr (assq t spec)) :inherit) 'bold))))

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

;;;; =====================================================================
;;;; R27 test-seat additions — ADVERSARIAL rail sequences (the verify
;;;; gaps): PROJECT-side convergence + stable timer census, rapid `|'
;;;; toggle storms, refresh mid doc-session, grouping change w/ the rail
;;;; popped.  The impl round proved the invariants on the BOARD; these
;;;; drive the same S1-S4 contract through the project's own paths.
;;;; =====================================================================

(defun org-air-r27--org-air-timer-count ()
  "Count pending timers (normal + idle) owned by an org-air function."
  (cl-count-if
   (lambda (tm)
     (let ((fn (timer--function tm)))
       (and (symbolp fn) (string-prefix-p "org-air" (symbol-name fn)))))
   (append timer-list timer-idle-list)))

(defun org-air-r27--reconcile-timers ()
  "Count pending `org-air-rail--reconcile-run' timers (the single slot)."
  (cl-count-if (lambda (tm)
                 (eq (timer--function tm) #'org-air-rail--reconcile-run))
               timer-list))

(defun org-air-r27--fire-pending-reconcile ()
  "Fire the deferred reconcile exactly as the timer would.
Cancels the pending slot timer FIRST (a direct body call leaves the
scheduled timer object orphaned in `timer-list' otherwise), then runs
`org-air-rail--reconcile-run' on the selected frame."
  (when (timerp org-air-rail--reconcile-timer)
    (cancel-timer org-air-rail--reconcile-timer))
  (org-air-rail--reconcile-run (selected-frame)))

(ert-deftest org-air-r27-1-project-rail-convergent-cycles ()
  "PROJECT w/ side rail: 6 reconcile/toggle/refresh cycles perform ZERO
rail-window creations/deletions/resizes after the first pop, the SAME
window object survives all cycles at the converged frame-derived cols,
and the org-air timer census is STABLE across cycles (no growth) — the
prompt's project-side convergence invariant the impl round only proved
on the board.  Each cycle runs the explicit refresh, the debounce body,
a 3x hook-fire reconcile burst (single slot holds) and the deferred
reconcile body itself."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-project
    (should (org-air-rail--popped-p))
    (let* ((rail-buf (get-buffer org-air-rail-buffer-name))
           (rail-win (org-air-rail--side-window))
           (displays 0) (deletes 0) (resizes 0)
           (census nil)
           (real-display (symbol-function 'display-buffer-in-side-window))
           (real-delete (symbol-function 'delete-window))
           (real-resize (symbol-function 'window-resize)))
      (should (window-live-p rail-win))
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
          ;; the explicit refresh (the toggle/refresh path)...
          (org-air-view--refresh-current)
          ;; ...the debounce timer's own body...
          (org-air-layout--refresh-windows)
          ;; ...a hook-fire burst: the single reconcile slot never stacks...
          (dotimes (_ 3) (org-air-rail--reconcile))
          (should (<= (org-air-r27--reconcile-timers) 1))
          ;; ...and the deferred reconcile body, as the timer runs it.
          (org-air-r27--fire-pending-reconcile)
          ;; stable timer census: cycle N owns exactly what cycle 1 owned.
          (if (null census)
              (setq census (org-air-r27--org-air-timer-count))
            (should (= census (org-air-r27--org-air-timer-count))))))
      (should (= displays 0))
      (should (= deletes 0))
      (should (= resizes 0))
      ;; the very same pinned window survived all six cycles, converged.
      (should (eq (org-air-rail--side-window) rail-win))
      (should (= (window-total-width rail-win) (org-air-rail--window-cols)))
      (should (org-air-rail--popped-p)))))

(ert-deftest org-air-r27-1-rapid-toggle-storm-consistent ()
  "A rapid `|' toggle storm (6 dispatches, no timer settles in between)
never leaves flag and window disagreeing, never stacks reconcile timers,
and re-renders exactly once per toggle (no amplification).  After the
storm settles (debounce body + deferred reconcile body) the rail is back
POPPED (even count), owned by the project, at the converged cols."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-project
    (should (org-air-rail--popped-p))
    (let ((renders 0)
          (real-render (symbol-function 'org-air-project--render-current)))
      (cl-letf (((symbol-function 'org-air-project--render-current)
                 (lambda (&rest args)
                   (cl-incf renders)
                   (apply real-render args))))
        (dotimes (_ 6)
          (org-air-r27--press "|")
          ;; the flag and the window agree after EVERY toggle — the
          ;; toggle is synchronous, never a transient lie.
          (should (eq (org-air-rail--popped-p)
                      (and (window-live-p (org-air-rail--side-window)) t)))
          ;; the single reconcile slot holds under the storm.
          (should (<= (org-air-r27--reconcile-timers) 1))))
      ;; one re-render per toggle, exactly.
      (should (= renders 6)))
    ;; even toggle count from popped -> POPPED again; settle everything.
    (should (org-air-rail--popped-p))
    (org-air-layout--refresh-windows)
    (org-air-r27--fire-pending-reconcile)
    (let ((rail-win (org-air-rail--side-window)))
      (should (window-live-p rail-win))
      (should (eq (org-air-rail--side-owner) (current-buffer)))
      (should (= (window-total-width rail-win) (org-air-rail--window-cols)))
      ;; the settled state is a fixpoint: one more settle cycle keeps the
      ;; SAME window object and geometry.
      (org-air-layout--refresh-windows)
      (org-air-r27--fire-pending-reconcile)
      (should (eq (org-air-rail--side-window) rail-win))
      (should (= (window-total-width rail-win) (org-air-rail--window-cols))))))

(ert-deftest org-air-r27-1-session-refresh-keeps-rail ()
  "Refresh DURING a TREE->DOC session (R26-5): with the rail popped, RET
opens the doc in the SAME window and hands the rail to the DOC half;
then the debounce body + hook-fired reconcile + deferred reconcile body
all fire MID-SESSION 3x — the rail window survives untouched (zero
deletes, same object, owner stays the doc), the popped flag holds, and
the user's FILE TEXT is never re-rendered.  C-c C-q restores the tree
into the same window with the rail re-owned to the tree, still popped."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-project
    (should (org-air-rail--popped-p))
    (goto-char (car (org-air-r27--doc-row-bols)))
    (let ((tree (current-buffer))
          (rail-buf (get-buffer org-air-rail-buffer-name))
          (rail-win (org-air-rail--side-window))
          (deletes 0)
          (real-delete (symbol-function 'delete-window))
          (docbuf nil))
      (unwind-protect
          (progn
            (org-air-r27--press "RET")
            (setq docbuf (window-buffer (selected-window)))
            (should-not (eq docbuf tree))
            (should (buffer-local-value 'org-air-project--session-tree docbuf))
            ;; the rail survived the swap, now owned by the DOC half.
            (should (eq (org-air-rail--side-window) rail-win))
            (should (eq (org-air-rail--side-owner) docbuf))
            (let ((text (with-current-buffer docbuf
                          (substring-no-properties (buffer-string)))))
              (cl-letf (((symbol-function 'delete-window)
                         (lambda (&optional win)
                           (when (and (window-live-p win)
                                      (eq (window-buffer win) rail-buf))
                             (cl-incf deletes))
                           (funcall real-delete win))))
                (dotimes (_ 3)
                  (org-air-layout--refresh-windows)
                  (org-air-rail--reconcile)
                  (org-air-r27--fire-pending-reconcile)))
              (should (= deletes 0))
              (should (eq (org-air-rail--side-window) rail-win))
              (should (eq (org-air-rail--side-owner) docbuf))
              (should (org-air-rail--popped-p docbuf))
              (should (equal (with-current-buffer docbuf
                               (substring-no-properties (buffer-string)))
                             text)))
            ;; back: the tree returns to the SAME window; rail re-owned.
            (with-current-buffer docbuf
              (call-interactively (key-binding (kbd "C-c C-q"))))
            (should (eq (window-buffer (selected-window)) tree))
            (should (eq (org-air-rail--side-owner) tree))
            (should (org-air-rail--popped-p tree)))
        (when (buffer-live-p docbuf)
          (let ((kill-buffer-query-functions nil))
            (with-current-buffer docbuf (set-buffer-modified-p nil))
            (kill-buffer docbuf)))))))

(ert-deftest org-air-r27-1-grouping-change-rail-stable ()
  "Grouping changes (s/t/d) with the rail POPPED: each regroup + settle
cycle keeps the SAME pinned rail window with ZERO creations/deletions/
resizes, really regroups the tree, and composes at the REAL settled
window body width (the R27-2 lock under the adversarial regroup)."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-project
    (should (org-air-rail--popped-p))
    (let* ((rail-buf (get-buffer org-air-rail-buffer-name))
           (rail-win (org-air-rail--side-window))
           (displays 0) (deletes 0) (resizes 0)
           (real-display (symbol-function 'display-buffer-in-side-window))
           (real-delete (symbol-function 'delete-window))
           (real-resize (symbol-function 'window-resize)))
      (should (window-live-p rail-win))
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
        (pcase-dolist (`(,key ,hallmark) '(("s" "| DRAFT Draft")
                                           ("t" "| #context")
                                           ("d" "| v0.1/")))
          (org-air-r27--press key)
          ;; settle the machinery after each regroup.
          (org-air-layout--refresh-windows)
          (org-air-rail--reconcile)
          (org-air-r27--fire-pending-reconcile)
          ;; the grouping really changed...
          (should (string-match-p (regexp-quote hallmark)
                                  (substring-no-properties (buffer-string))))
          ;; ...the SAME pinned rail window survived, still popped...
          (should (eq (org-air-rail--side-window) rail-win))
          (should (org-air-rail--popped-p))
          ;; ...composed at the REAL settled body width.
          (should (eql org-air-project--rendered-width
                       (window-body-width
                        (get-buffer-window (current-buffer)))))))
      (should (= displays 0))
      (should (= deletes 0))
      (should (= resizes 0)))))

(provide 'org-air-round27-test)
;;; org-air-round27-test.el ends here
