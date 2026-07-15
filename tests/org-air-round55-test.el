;;; org-air-round55-test.el --- executing ERTs for v0.5 round-55 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.5 round-55 (air/v0.5/org-air-round55-design.org):
;; R55-1 owner-routed calendar day-open — RET on a rail day cell renders
;; the single-day view (R6) in the MAIN board window, never the dedicated
;; side-window rail.
;;
;; Harness: the R27 live-window discipline — `noninteractive' bound nil so
;; side windows REALLY exist on the 80x25 batch frame;
;; `org-air-rail-min-width' / `org-air-item-pane-min' bound down so the
;; rail engages at 80 cols.  Unlike the R27 harness the per-buffer
;; popped flag is left at the `unset' sentinel so the FIRST render runs
;; the R49-2 placement seed (the R49 pattern) — each test names its
;; placement.  Keys are dispatched via `key-binding' AT POINT so the
;; day-cell PROPERTY keymap resolves exactly as a real keypress.
;;
;;   E1  rail RET routes to the MAIN board window (revert-FAILS): day
;;       state lands in the BOARD's buffer-locals (the rail's stay nil),
;;       focus lands in a non-side, non-dedicated window showing the
;;       board, the R6 day header is composed in the BOARD text and NOT
;;       in the rail, and ZERO `org-air-query-items' calls ran (trunk's
;;       rail-buffer render fell back to a synchronous re-scan).
;;   E2  the rail survives the day-open (single-writer law): same side
;;       window object, still live/dedicated/showing `*org-air-rail*',
;;       width unchanged, back-pointer still names the BOARD (trunk's
;;       self-owned rail makes this clause revert-FAIL too).
;;   E3  inline identity (LOCK): placement `inline' — RET on an inline
;;       day cell sets the board's day state, the selected window is
;;       UNCHANGED, no side rail appears, zero queries.
;;   E3b rail-off identity (LOCK): responsive board-only —
;;       `org-air-view-day' still opens the day view in the board with
;;       the selected window unchanged, no rail, zero queries.
;;   E4  the return path: after E1's dispatch, `q' in the (now selected)
;;       board window exits the day view (R28-2 layer 2), the full board
;;       re-renders, and the rail side window is STILL live and popped.
;;   E5  no-board-window edge (defensive): the board's main window shows
;;       another buffer (the sole ordinary window of the single batch
;;       frame cannot be DELETED, so reuse is the same owner-has-no-
;;       window state); a rail day-open lands the day view in a window
;;       with nil `window-side' and nil dedication showing the board —
;;       the `display-buffer' fallback can never squat the rail.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-air-test-helpers)

(when (locate-library "org-air")
  (require 'org-air))

;;;; ---------------------------------------------------------------------
;;;; Harness — a live fixture board whose FIRST render seeds the placement.
;;;; ---------------------------------------------------------------------

(defun org-air-r55--kill-aux-buffers ()
  "Kill the shared pane/rail/view buffers so tests never inherit stale windows."
  (let ((kill-buffer-query-functions nil))
    (dolist (name (list org-air-view-pane-buffer-name org-air-rail-buffer-name
                        org-air-view-buffer-name "*org-air-project*"))
      (when (get-buffer name) (kill-buffer name)))))

(defun org-air-r55--reset-rail-globals ()
  "Cancel pending reconcile timers + reset the R27-1 edge flag (R27 pattern)."
  (when (timerp org-air-rail--reconcile-timer)
    (cancel-timer org-air-rail--reconcile-timer))
  (dolist (tm (copy-sequence timer-list))
    (when (eq (timer--function tm) #'org-air-rail--reconcile-run)
      (cancel-timer tm)))
  (setq org-air-rail--reconcile-timer nil
        org-air-rail--side-was-live nil))

(defmacro org-air-r55--with-live-board (placement &rest body)
  "Render a fresh fixture board LIVE under PLACEMENT; run BODY in its buffer.
`noninteractive' is bound nil so the R49-2 seed really consults
PLACEMENT (the popped flag is left at the `unset' sentinel) and side
windows really exist; `org-air-rail-min-width' / `org-air-item-pane-min'
are bound down so the rail engages on the unresizable 80-col batch
frame.  BODY runs in the board buffer with its MAIN window selected."
  (declare (indent 1) (debug t))
  `(org-air-test-with-fixtures
    (save-window-excursion
      (org-air-r55--kill-aux-buffers)
      (org-air-r55--reset-rail-globals)
      (let ((noninteractive nil)
            (org-air-rail-min-width 40)
            (org-air-item-pane-min 30)
            (org-air-rail-focus-on-popout nil)
            (org-air-rail-placement ,placement)
            (org-air-board-rail-placement nil)
            (bbuf (get-buffer-create org-air-view-buffer-name)))
        (unwind-protect
            (progn
              (with-current-buffer bbuf
                (org-air-view-mode)
                (setq org-air-view--items (org-air-query-items)))
              (switch-to-buffer bbuf)
              (delete-other-windows)
              (with-current-buffer bbuf
                ;; anti-tautology: the flag really is the sentinel, so the
                ;; render below runs the R49-2 SEED, not a pre-cooked flag.
                (should (eq org-air-view--rail-popped-out 'unset))
                (org-air-view--render org-air-view--items nil)
                ,@body))
          (org-air-r55--reset-rail-globals)
          (org-air-r55--kill-aux-buffers))))))

(defun org-air-r55--first-day-cell (buffer)
  "Return the first `org-air-day' calendar-cell position in BUFFER."
  (with-current-buffer buffer
    (let ((pos (text-property-not-all (point-min) (point-max)
                                      'org-air-day nil)))
      (should pos)
      pos)))

(defun org-air-r55--press-at (window pos key)
  "Select WINDOW, move point to POS, dispatch KEY via `key-binding' at point.
Resolving the binding AT POINT is what makes the seam faithful headless:
the day-cell PROPERTY keymap outranks the major-mode map exactly as it
does for a real keypress."
  (select-window window)
  (with-current-buffer (window-buffer window)
    (goto-char pos)
    (let ((cmd (key-binding (kbd key))))
      (should cmd)
      (call-interactively cmd))))

(defmacro org-air-r55--counting-queries (counter &rest body)
  "Run BODY counting `org-air-query-items' calls into the variable COUNTER.
The real function still runs (nothing is stubbed away) — the count is
the R55 no-query guard: a day-open renders from the owner's CACHED
items, so the count must stay 0 (trunk's rail-buffer render scored >= 1)."
  (declare (indent 1) (debug t))
  `(let ((,counter 0)
         (org-air-r55--real-query (symbol-function 'org-air-query-items)))
     (cl-letf (((symbol-function 'org-air-query-items)
                (lambda (&rest args)
                  (cl-incf ,counter)
                  (apply org-air-r55--real-query args))))
       ,@body)))

(defun org-air-r55--buffer-text (buffer)
  "Return BUFFER's text without properties."
  (with-current-buffer buffer
    (substring-no-properties (buffer-string))))

(defun org-air-r55--day-header (date)
  "Return the R6 day-view header rendering of DATE."
  (format-time-string "%A %-d %B %Y" date))

;;;; ---------------------------------------------------------------------
;;;; E1 — rail RET routes to the MAIN board window (revert-FAILS).
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r55-1-rail-ret-routes-to-main-board-window ()
  "RET on a rail day cell opens the day view in the MAIN board window.
Under the R49-3 default `side-window' placement the calendar's day cells
live in the dedicated `*org-air-rail*' side window; the day-open must
resolve its TARGET to the OWNER board buffer/window (R55-1), NOT
`(current-buffer)'/`(selected-window)'.  Asserts ALL of: day state in
the BOARD's locals (the rail's stay nil); focus in a non-side,
non-dedicated window showing the board (not the rail side window); the
R6 day header composed in the BOARD text and NOT in the rail; ZERO
`org-air-query-items' calls (the owner's cached items drive the render).
Trunk fails every clause — reverting to current-buffer routing FAILS."
  (skip-unless (locate-library "org-air"))
  (org-air-r55--with-live-board 'side-window
    (let* ((board (current-buffer))
           (board-win (selected-window))
           (rail-win (org-air-rail--side-window))
           (rail-buf (get-buffer org-air-rail-buffer-name)))
      (should (window-live-p rail-win))
      (should (buffer-live-p rail-buf))
      (let* ((cell (org-air-r55--first-day-cell rail-buf))
             (cell-date (with-current-buffer rail-buf
                          (get-text-property cell 'org-air-day))))
        (should cell-date)
        (org-air-r55--counting-queries queries
          (org-air-r55--press-at rail-win cell "RET")
          ;; no-query guard (folded per the spec): the day-open rendered
          ;; from the owner's cached items — trunk's rail render re-scanned.
          (should (= queries 0)))
        ;; day state landed in the BOARD's buffer-locals, not the rail's.
        (should (equal (org-air-view--day-key
                        (buffer-local-value 'org-air-view--day board))
                       (org-air-view--day-key cell-date)))
        (should-not (buffer-local-value 'org-air-view--day rail-buf))
        ;; focus: the MAIN board window — nil `window-side', nil
        ;; dedication, not the rail side window.
        (should (eq (window-buffer (selected-window)) board))
        (should (eq (selected-window) board-win))
        (should-not (window-parameter (selected-window) 'window-side))
        (should-not (window-dedicated-p (selected-window)))
        (should-not (eq (selected-window) (org-air-rail--side-window)))
        ;; the day pane composed in the BOARD text; the rail carries NO
        ;; day header (the reported symptom: the pane painted the rail).
        (let ((header (regexp-quote (org-air-r55--day-header cell-date))))
          (should (string-match-p header (org-air-r55--buffer-text board)))
          (should-not (string-match-p header
                                      (org-air-r55--buffer-text rail-buf))))))))

;;;; ---------------------------------------------------------------------
;;;; E2 — the rail survives the day-open (single-writer law LOCK).
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r55-2-rail-survives-day-open ()
  "The rail buffer/window are never reused, resized or deleted by a day-open.
After E1's dispatch the side window is the SAME live window object,
still dedicated, still showing `*org-air-rail*', width unchanged; and
the rail's R25-6 back-pointer still names the BOARD — trunk's defect 3
seeded a SELF-OWNED rail (back-pointer eq the rail buffer itself), so
the back-pointer clause is revert-FAIL; the rest LOCK the contract."
  (skip-unless (locate-library "org-air"))
  (org-air-r55--with-live-board 'side-window
    (let* ((board (current-buffer))
           (rail-win (org-air-rail--side-window))
           (rail-buf (get-buffer org-air-rail-buffer-name))
           (rail-width (window-total-width rail-win))
           (cell (org-air-r55--first-day-cell rail-buf)))
      (org-air-r55--press-at rail-win cell "RET")
      ;; the very same side window survived: live, dedicated, rail-owned.
      (should (window-live-p rail-win))
      (should (eq (org-air-rail--side-window) rail-win))
      (should (window-dedicated-p rail-win))
      (should (eq (window-buffer rail-win) rail-buf))
      (should (= (window-total-width rail-win) rail-width))
      ;; single-writer law: the rail still mirrors the BOARD (never
      ;; itself), and its calendar cells are intact for the next press.
      (should (eq (buffer-local-value 'org-air-rail--board-buffer rail-buf)
                  board))
      (should (org-air-r55--first-day-cell rail-buf)))))

;;;; ---------------------------------------------------------------------
;;;; E3 — inline + rail-off identity (LOCK, passes both sides).
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r55-3-inline-identity ()
  "Placement `inline': RET on an inline day cell is byte-preserved trunk.
The cell lives in the BOARD buffer (owner tier 1 — identity): day state
lands in the board, the selected window is UNCHANGED (no hop), no
`*org-air-rail*' side window appears, and ZERO `org-air-query-items'
calls ran."
  (skip-unless (locate-library "org-air"))
  (org-air-r55--with-live-board 'inline
    (let ((board (current-buffer))
          (board-win (selected-window)))
      ;; inline: the calendar is composed INTO the board; no side rail.
      (should-not (org-air-rail--side-window))
      (let* ((cell (org-air-r55--first-day-cell board))
             (cell-date (get-text-property cell 'org-air-day)))
        (should cell-date)
        (org-air-r55--counting-queries queries
          (org-air-r55--press-at board-win cell "RET")
          (should (= queries 0)))
        (should (equal (org-air-view--day-key
                        (buffer-local-value 'org-air-view--day board))
                       (org-air-view--day-key cell-date)))
        ;; identity tier: no hop, no side window materialised.
        (should (eq (selected-window) board-win))
        (should (eq (window-buffer (selected-window)) board))
        (should-not (org-air-rail--side-window))
        (should (string-match-p
                 (regexp-quote (org-air-r55--day-header cell-date))
                 (org-air-r55--buffer-text board)))))))

(ert-deftest org-air-r55-3b-rail-off-identity ()
  "Responsive board-only (rail off): the day view still opens in the board.
With the width below `org-air-rail-min-width' there is no calendar at
all, so the day-open runs the `M-x' path (owner tier 1 — the current
buffer IS the board): the day defaults to TODAY, renders in the board,
the selected window is unchanged, no rail appears, zero queries."
  (skip-unless (locate-library "org-air"))
  (org-air-r55--with-live-board 'inline
    (let ((board (current-buffer))
          (board-win (selected-window))
          (org-air-rail-min-width 200))
      ;; re-render responsive board-only: no rail, no day cells anywhere.
      (org-air-view--render-current)
      (should (eq org-air-view--orientation 'board-only))
      (should-not (org-air-rail--side-window))
      (should-not (text-property-not-all (point-min) (point-max)
                                         'org-air-day nil))
      (let ((before-key (org-air-view--day-key (current-time))))
        (org-air-r55--counting-queries queries
          (call-interactively #'org-air-view-day)
          (should (= queries 0)))
        (let ((after-key (org-air-view--day-key (current-time)))
              (day (buffer-local-value 'org-air-view--day board)))
          (should day)
          ;; today (tolerating a midnight rollover between the two reads).
          (should (member (org-air-view--day-key day)
                          (list before-key after-key)))
          (should (eq (selected-window) board-win))
          (should (eq (window-buffer (selected-window)) board))
          (should-not (org-air-rail--side-window))
          (should (string-match-p
                   (regexp-quote (org-air-r55--day-header day))
                   (org-air-r55--buffer-text board))))))))

;;;; ---------------------------------------------------------------------
;;;; E4 — the return path: `q' exits the day view; the rail survives.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r55-4-return-path-q-exits-day-view ()
  "After a rail day-open, `q' in the (now selected) board window returns.
With day state in the BOARD and focus in the MAIN window, the R28-2
layer-2 `q' means day-exit (`org-air-view-board'): `org-air-view--day'
goes nil, the full board re-renders (day header gone), and the rail side
window is STILL live and popped — the round-trip never touches it.
Trunk had no in-band return at all (focus trapped in the rail, where `q'
means pop-in)."
  (skip-unless (locate-library "org-air"))
  (org-air-r55--with-live-board 'side-window
    (let* ((board (current-buffer))
           (rail-win (org-air-rail--side-window))
           (rail-buf (get-buffer org-air-rail-buffer-name))
           (cell (org-air-r55--first-day-cell rail-buf))
           (cell-date (with-current-buffer rail-buf
                        (get-text-property cell 'org-air-day))))
      (org-air-r55--press-at rail-win cell "RET")
      ;; E1 landed us in the main board window, day view up.
      (should (eq (window-buffer (selected-window)) board))
      (should (buffer-local-value 'org-air-view--day board))
      ;; `q' in the board window: R28-2 layer 2 — the day-exit.
      (with-current-buffer (window-buffer (selected-window))
        (call-interactively (key-binding (kbd "q"))))
      (should-not (buffer-local-value 'org-air-view--day board))
      (should-not (string-match-p
                   (regexp-quote (org-air-r55--day-header cell-date))
                   (org-air-r55--buffer-text board)))
      ;; the rail survived the whole round-trip, still popped.
      (should (window-live-p rail-win))
      (should (eq (org-air-rail--side-window) rail-win))
      (should (org-air-rail--popped-p board)))))

;;;; ---------------------------------------------------------------------
;;;; E5 — no-board-window edge: display-buffer fallback, never the rail.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r55-5-no-board-window-fallback ()
  "A rail day-open with NO live board window still lands in a MAIN window.
The board's main window is given to an unrelated buffer (the sole
ordinary window of the single-frame batch harness cannot be DELETED, so
reuse is the same owner-has-no-window state the spec's edge names); the
rail day-open must `display-buffer' the owner into a window with nil
`window-side' and nil dedication — never `switch-to-buffer' into the
invoking (dedicated) rail window, never a side-window squat.
`org-air-rail-min-width' is bound down so the responsive board-only
teardown stays out of this edge's way (it is not what E5 tests)."
  (skip-unless (locate-library "org-air"))
  (org-air-r55--with-live-board 'side-window
    (let* ((board (current-buffer))
           (board-win (selected-window))
           (rail-win (org-air-rail--side-window))
           (rail-buf (get-buffer org-air-rail-buffer-name))
           (other (get-buffer-create "*org-air-r55-elsewhere*")))
      (unwind-protect
          (progn
            (set-window-buffer board-win other)
            (should-not (get-buffer-window board))
            (let ((org-air-rail-min-width 20)
                  (cell (org-air-r55--first-day-cell rail-buf)))
              (org-air-r55--press-at rail-win cell "RET")
              ;; day state reached the owner...
              (should (buffer-local-value 'org-air-view--day board))
              ;; ...and focus landed in a MAIN window showing the board:
              ;; nil `window-side', nil dedication, not the rail.
              (should (eq (window-buffer (selected-window)) board))
              (should-not (window-parameter (selected-window) 'window-side))
              (should-not (window-dedicated-p (selected-window)))
              (should-not (eq (selected-window) rail-win))
              ;; the dedicated rail was never squatted.
              (should (window-live-p rail-win))
              (should (eq (window-buffer rail-win) rail-buf))))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer other))))))

(provide 'org-air-round55-test)
;;; org-air-round55-test.el ends here
