;;; org-air-round34-test.el --- R34 acceptance ERTs -*- lexical-binding: t; -*-

;;; Commentary:
;; Round-34 acceptance tests (air/v0.5/org-air-round34-design.org), EXECUTING
;; and deterministic in batch (no real timers, no font pixels).
;;
;; R34-1  HEADER +1 OVER ON THE REAL (fringed + right-scroll-bar) GEOMETRY.
;;        R29-1 composed to `window-max-chars-per-line', which divides the
;;        body pixels by the DEFAULT-FACE font's average advance and so can
;;        exceed `window-body-width' when that font is narrower than the
;;        frame canonical cell (Adwaita Mono 15px set after frame creation) —
;;        192 for a 191-col body -> the header/rows paint one column past the
;;        text area (the `↦' truncation arrow).  Fixed: derive "usable" from
;;        `window-body-width' (fringes/scroll-bar already excluded), reserving
;;        the continuation column ONLY when the right fringe is absent.  The
;;        arithmetic lives behind the pure helper
;;        `org-air-layout--usable-columns-for'; these ERTs pin BOTH geometries
;;        (a HEADLESS width model) + a real-frame guarded check.
;;
;; R34-2  NO-RAIL ROWS MISALIGNED.  Every board row's metadata cluster is
;;        right-anchored to ONE compose width via the shared
;;        `org-air-view--insert-row' (cluster at `width - cluster-w' with a
;;        FIXED cluster-w from the meta-width pass).  R34-2 locks that the
;;        cluster starts at a CONSTANT screen column across rows in the
;;        board-only AND side-window (rail popped out) orientations, and that
;;        the same holds inline (two-pane) — the paths do not fork.
;;
;; R34-3  CACHE-FIRST LAZY-LOAD STRAND.  The old chain re-armed a one-shot
;;        idle timer from inside its own idle callback, pushing the ABSOLUTE
;;        idle target forward ~0.05s per slice (target ~= k*0.05) so a large
;;        tree — or any interaction that reset the idle clock — never reached
;;        the ever-larger far-future target ("loading N/M" / "stale ∙
;;        refreshing" stuck).  Fixed: one BOUNDED repeating idle pacer
;;        (`org-air-view--refresh-arm'/`-disarm', constant
;;        `org-air-view--refresh-next-delay').  The R26-8 state machine bytes
;;        are untouched; these ERTs prove the anti-stall pacing (pure), the
;;        arm/disarm lifecycle, and a cold end-to-end run reaching DONE.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-air-viewport-helpers)
(require 'org-air-round26-test)

;;;; =====================================================================
;;;; R34-1 — usable columns derive from window-body-width, never overshoot.
;;;; =====================================================================

(ert-deftest org-air-r34-1-usable-columns-for-both-geometries ()
  "The pure width model `org-air-layout--usable-columns-for' reserves ONE
right column on EVERY graphical frame (R37 universal safety margin):
- fringed + right scroll-bar (this user): body-1 (the last column sits
  under the scroll bar / is a partial cell; a glyph there clips);
- fringe-less GUI: body-1 (the continuation column, R29-1 preserved);
- TTY / mock (non-graphic): always the plain body width (goldens intact).
The fringe/scroll-bar arg is now advisory only; the reserve is
unconditional on graphical frames."
  (skip-unless (locate-library "org-air"))
  (should (fboundp 'org-air-layout--usable-columns-for))
  ;; fringed + right scroll-bar (R37 re-bless 191->190): the last body
  ;; column clips under the scroll bar / is a partial cell -> reserve one.
  (should (= (org-air-layout--usable-columns-for t 191 8) 190))
  ;; fringe-less GUI: reserve the continuation column (R29-1 preserved).
  (should (= (org-air-layout--usable-columns-for t 191 0) 190))
  (should (= (org-air-layout--usable-columns-for t 80 nil) 79))
  ;; TTY / mock: the plain body width regardless of the fringe value.
  (should (= (org-air-layout--usable-columns-for nil 191 0) 191))
  (should (= (org-air-layout--usable-columns-for nil 80 8) 80)))

(ert-deftest org-air-r34-1-usable-never-exceeds-body ()
  "The invariant that killed the +1: for EVERY (graphic, body, right-fringe)
the usable columns NEVER exceed BODY — the exact property
`window-max-chars-per-line' violated on the user's frame (it returned
body+1).  Reverting to a body+1 source fails this guard."
  (skip-unless (locate-library "org-air"))
  (dolist (graphic '(t nil))
    (dolist (body '(1 2 40 80 120 191 192 195 400))
      (dolist (rf '(nil 0 1 8 16))
        (let ((u (org-air-layout--usable-columns-for graphic body rf)))
          (ert-info ((format "graphic=%s body=%d right-fringe=%s -> %d"
                             graphic body rf u))
            ;; never overshoots the text area…
            (should (<= u body))
            ;; …and never collapses below one column.
            (should (>= u 1))
            ;; graphical (R37): reserve exactly one column regardless of
            ;; the fringe/scroll-bar geometry (universal safety margin).
            (when graphic
              (should (= u (max 1 (1- body)))))
            ;; non-graphic is always the plain body width.
            (unless graphic
              (should (= u body)))))))))

(ert-deftest org-air-r34-1-header-and-rows-never-overshoot-width ()
  "Integration (batch width seam): across a width sweep, the composed header
line AND every item/divider/rail row has `string-width' <= the render width
W (never W+1) — the composition never overshoots its target at any width."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (dolist (w '(40 80 120 191 192 195))
      (org-air-viewport-test-with-dashboard w
        (let ((max-w 0) (over '()))
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (let ((lw (string-width
                         (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position)))))
                (setq max-w (max max-w lw))
                (when (> lw w) (push (cons (line-number-at-pos) lw) over)))
              (forward-line 1)))
          (ert-info ((format "W=%d max-line-width=%d overs=%S" w max-w over))
            ;; no composed line exceeds the render width.
            (should (null over))
            (should (<= max-w w))))))))

(ert-deftest org-air-r34-1-real-frame-body-width-bound ()
  "Real-frame guarded check (skipped in batch): on a graphical frame with
default fringes + a RIGHT scroll-bar, sized so `window-body-width' = 191
\(and a few other widths), the composed header + every row have
`string-width' <= `window-body-width' AND
`org-air-layout--usable-columns' <= `window-body-width' — the assertion
that FAILS today on a font whose average advance < the frame canonical
cell (`window-max-chars-per-line' returned body+1)."
  (skip-unless (and (locate-library "org-air") (display-graphic-p)))
  (let ((frame (make-frame '((width . 200) (height . 50)
                             (vertical-scroll-bars . right)
                             (left-fringe . 8) (right-fringe . 8)
                             (minibuffer . nil)))))
    (unwind-protect
        (with-selected-frame frame
          (dolist (target '(191 160 120 80))
            (let ((win (frame-selected-window frame)))
              ;; nudge the frame width until the body is the target columns.
              (set-frame-width frame (+ target
                                        (- (window-total-width win)
                                           (window-body-width win))))
              (let* ((buf (save-window-excursion (org-air) (get-buffer "*org-air*"))))
                (when buf
                  (set-window-buffer win buf)
                  (with-current-buffer buf
                    (let ((body (window-body-width win)))
                      (should (<= (org-air-layout--usable-columns win) body))
                      (save-excursion
                        (goto-char (point-min))
                        (while (not (eobp))
                          (should (<= (string-width
                                       (buffer-substring-no-properties
                                        (line-beginning-position)
                                        (line-end-position)))
                                      body))
                          (forward-line 1))))))))))
      (delete-frame frame))))

;;;; =====================================================================
;;;; R34-2 — one cluster column across every no-rail row (and inline).
;;;; =====================================================================

(defun org-air-r34--item-row-tag-columns ()
  "Return the list of display columns where the FIRST tag `#' begins on
each ITEM row of the current board buffer (rows carrying `org-air-item').
Measured as display width (not char index) so wide glyphs are honoured;
the tag cell is the fixed metadata cluster, so a constant column proves
the cluster lands on one screen column down the list (V6 / R34-2)."
  (let (cols)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((bol (line-beginning-position))
               (eol (line-end-position))
               (raw (buffer-substring-no-properties bol eol))
               ;; In the two-pane layout the SAME line carries the item pane
               ;; then the divider then the rail (which also holds `#'-tags in
               ;; its Source/Filter blocks).  Cut at the pane divider so the
               ;; probe only ever measures the ITEM pane's own tag column;
               ;; board-only / side-window have no divider (whole line).
               (line (car (split-string raw "\u2502"))))
          ;; an item row carries `org-air-item' SOMEWHERE on the line (in the
          ;; two-pane layout the item pane is indented, so bol is a space).
          (when (and (text-property-not-all bol eol 'org-air-item nil)
                     (string-match "#" line))
            (push (string-width (substring line 0 (string-match "#" line)))
                  cols)))
        (forward-line 1)))
    (nreverse cols)))

(defun org-air-r34--render-board (width popped-out)
  "Render the board over the fixtures at WIDTH in the current temp buffer.
POPPED-OUT non-nil forces the side-window orientation (rail popped out);
nil lets WIDTH pick board-only (< rail-min) or two-pane.  Returns the
chosen orientation."
  (org-air-view-mode)
  (setq org-air-view--items (org-air-query-items))
  (setq-local org-air-view--rail-popped-out (and popped-out t))
  (let ((org-air-view-width width))
    (org-air-view--render org-air-view--items nil))
  org-air-view--orientation)

(ert-deftest org-air-r34-2-board-only-cluster-column-constant ()
  "Board-only orientation (width < rail-min): the metadata cluster starts
at ONE constant screen column across every item row (the tag cell anchors
it), and the meta-width pass ran (widths non-nil) so cluster-w is fixed —
not the per-row fallback.  Reverting the single-width fix (rows composed
to divergent widths) makes these columns disagree."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-test-with-fixtures
      (org-air-viewport-test--with-frozen-now
        (with-temp-buffer
          (let ((org-air-view-height 45))
            (should (eq (org-air-r34--render-board 80 nil) 'board-only))
            ;; the meta-width pass set the fixed cluster columns.
            (should (integerp org-air-view--meta-date-w))
            (should (integerp org-air-view--meta-tags-w))
            (let ((cols (org-air-r34--item-row-tag-columns)))
              (ert-info ((format "tag columns: %S" cols))
                (should (> (length cols) 2))
                (should (= 1 (length (delete-dups (copy-sequence cols)))))))))))))

(ert-deftest org-air-r34-2-side-window-cluster-column-constant ()
  "Side-window orientation (rail popped OUT): the popped-out board rows
share ONE cluster column too, at several widths (odd + even).  The
board-only and side-window paths both funnel the item pane through the
shared `insert-item-pane', so the columns must agree row-to-row."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (dolist (w '(110 120 121 160))
      (org-air-test-with-fixtures
        (org-air-viewport-test--with-frozen-now
          (with-temp-buffer
            (let ((org-air-view-height 45))
              (should (eq (org-air-r34--render-board w t) 'side-window))
              (should (integerp org-air-view--meta-tags-w))
              (let ((cols (org-air-r34--item-row-tag-columns)))
                (ert-info ((format "W=%d tag columns: %S" w cols))
                  (should (> (length cols) 2))
                  (should (= 1 (length (delete-dups
                                        (copy-sequence cols))))))))))))))

(ert-deftest org-air-r34-2-inline-cluster-column-constant ()
  "Parity of the primitive: the INLINE (two-pane) item pane also lands its
cluster on one constant column across rows (a different absolute column
than the no-rail case because the rail consumes width, but constant among
its own rows) — proving board-only / side-window / inline share the one
`org-air-view--insert-row' math (no fork)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 120
      (should (eq org-air-view--orientation 'two-pane))
      (let ((cols (org-air-r34--item-row-tag-columns)))
        (ert-info ((format "inline tag columns: %S" cols))
          (should (> (length cols) 2))
          (should (= 1 (length (delete-dups (copy-sequence cols))))))))))

(ert-deftest org-air-r34-2-one-width-every-row-full ()
  "One-width contract (ties R34-2 to R34-1): in the board-only and
side-window orientations every item row composes to EXACTLY the render
width (`string-width' == W), so the right cluster is never clipped
unevenly across rows."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (dolist (spec '((80 . nil) (120 . t) (160 . t)))
      (org-air-test-with-fixtures
        (org-air-viewport-test--with-frozen-now
          (with-temp-buffer
            (let ((org-air-view-height 45)
                  (w (car spec)))
              (org-air-r34--render-board w (cdr spec))
              (save-excursion
                (goto-char (point-min))
                (while (not (eobp))
                  (when (get-text-property (line-beginning-position) 'org-air-item)
                    (should (= (string-width
                                (buffer-substring-no-properties
                                 (line-beginning-position) (line-end-position)))
                               w)))
                  (forward-line 1))))))))))

;;;; =====================================================================
;;;; R34-3 — bounded, resumable idle pacer (no strand).
;;;; =====================================================================

(ert-deftest org-air-r34-3-next-delay-bounded ()
  "Anti-stall pacing (pure): `org-air-view--refresh-next-delay' returns a
BOUNDED delay (<= the constant `org-air-view--refresh-pace') at every slice
of a long chain, WITH or WITHOUT a mid-load interruption — it must NOT grow
with the slice index.  The old chain's effective target ~= k*0.05 was
unbounded; reverting to it fails this guard."
  (skip-unless (locate-library "org-air"))
  (should (fboundp 'org-air-view--refresh-next-delay))
  (should (numberp org-air-view--refresh-pace))
  (let ((pace org-air-view--refresh-pace))
    ;; simulate a 500-slice chain: idle-elapsed grows, an interruption is
    ;; injected mid-way; the returned delay stays bounded throughout.
    (dotimes (k 500)
      (let* ((idle-elapsed (* k pace))       ; the accumulating idle target
             (interrupted (zerop (mod k 37))) ; periodic mid-load interruption
             (d (org-air-view--refresh-next-delay idle-elapsed interrupted)))
        (ert-info ((format "slice %d idle=%.2f interrupted=%s -> %s"
                           k idle-elapsed interrupted d))
          (should (numberp d))
          (should (> d 0))
          ;; the pacing is CONSTANT — it never creeps up with the slice index.
          (should (<= d pace)))))))

(ert-deftest org-air-r34-3-arm-disarm-lifecycle ()
  "Repeating-timer lifecycle: `refresh-arm' arms EXACTLY ONE live pacing
timer (arming again does not add a second); `refresh-disarm' /
`refresh-cancel' leave NO live pacing timer.  Driven with `noninteractive'
bound nil (the arm is P0-guarded off under batch); every timer armed is
cancelled so no idle timer leaks into other tests."
  (skip-unless (locate-library "org-air"))
  (with-temp-buffer
    (org-air-view-mode)
    (let ((noninteractive nil))
      (unwind-protect
          (progn
            (setq org-air-view--refresh-timer nil)
            ;; arm exactly one live repeating pacing timer.
            (org-air-view--refresh-arm (current-buffer) org-air-view--refresh-token)
            (should (timerp org-air-view--refresh-timer))
            (should (memq org-air-view--refresh-timer timer-idle-list))
            (let ((first org-air-view--refresh-timer))
              ;; arming again must NOT create a second live timer.
              (org-air-view--refresh-arm (current-buffer) org-air-view--refresh-token)
              (should (eq org-air-view--refresh-timer first))
              (should (= 1 (cl-count first timer-idle-list))))
            ;; disarm tears it down cleanly.
            (org-air-view--refresh-disarm)
            (should-not (timerp org-air-view--refresh-timer))
            ;; a token bump (`g' mid-refresh) + re-arm never leaves two.
            (org-air-view--refresh-arm (current-buffer) org-air-view--refresh-token)
            (org-air-view--refresh-cancel)   ; bumps token + disarms
            (should-not (timerp org-air-view--refresh-timer)))
        ;; belt-and-braces cleanup.
        (when (timerp org-air-view--refresh-timer)
          (cancel-timer org-air-view--refresh-timer))
        (setq org-air-view--refresh-timer nil)))))

(ert-deftest org-air-r34-3-cold-end-to-end-reaches-done ()
  "Cold dispatch (no cache), driven to completion with NO real timers: the
skeleton paints `loading 0/N', then driving `org-air-view--refresh-run-slice'
to completion performs the SINGLE swap — the board is populated,
`org-air-view--loading' is cleared, `refresh-state' is nil, and no
`loading' marker remains.  Regression fence over the reported \"stuck
forever\" once the pacer lets the slices run."
  (skip-unless (locate-library "org-air"))
  (org-air-r26--with-cache-env
    ;; a genuinely COLD start: no cache file on disk.
    (when (file-exists-p org-air-cache-file) (delete-file org-air-cache-file))
    (with-current-buffer (org-air-r26--cache-board)
      ;; model the COLD branch of the dispatcher (batch takes the sync path,
      ;; so drive the machine explicitly): loading + start + skeleton paint.
      (setq org-air-view--loading t)
      (org-air-view--refresh-start)
      (should (eq org-air-view--refresh-state 'refreshing))
      (should (> org-air-view--refresh-total 0))
      (org-air-view--render-loading)
      (let ((skeleton (substring-no-properties (buffer-string))))
        (should (string-match-p "loading 0/[0-9]+" skeleton)))
      ;; no live pacing timer under batch (P0), but the chain still runs
      ;; when the slices are driven directly.
      (should-not (timerp org-air-view--refresh-timer))
      ;; drive the slices to completion (the pacer would do this on idle).
      (org-air-r26--run-slices)
      ;; DONE: single swap populated the board, all markers cleared.
      (should-not org-air-view--refresh-state)
      (should-not org-air-view--loading)
      (should-not (timerp org-air-view--refresh-timer))
      (should org-air-view--items)
      (let ((text (substring-no-properties (buffer-string))))
        (should-not (string-match-p "loading [0-9]+/[0-9]+" text))
        (should-not (string-match-p "stale \u2219 refreshing" text))
        ;; the real board is present (a known fixture title rendered).
        (should (string-match-p "items" text))))))

(ert-deftest org-air-r34-3-warm-run-leaves-no-live-pacer ()
  "Warm cache-first path: after the driven slices reach DONE the `stale ∙
refreshing' marker is cleared (R26-8) AND no live pacing timer survives —
the finish/disarm teardown cannot leave the pacer running past the single
swap (which would re-fire on the next idle)."
  (skip-unless (locate-library "org-air"))
  (org-air-r26--with-cache-env
    (with-current-buffer (org-air-r26--cache-board)
      (org-air-r26--scan-and-cache))
    (kill-buffer org-air-view-buffer-name)
    (org-air-r26--kill-file-buffers org-air-test--dir)
    ;; touch a file so the cache is stale -> WARM refresh on next open.
    (write-region "* TODO r34-3 warm probe\n" nil org-air-inbox-file 'append)
    (set-file-times org-air-inbox-file (time-add (current-time) 5))
    (let ((cache (org-air-view--cache-load)))
      (should cache)
      (should (cdr cache))                    ; stale files present -> WARM
      (with-current-buffer (org-air-r26--cache-board)
        (setq org-air-view--items (car cache)
              org-air-view--cache-stale-files (cdr cache))
        (org-air-view--refresh-start)
        (org-air-view--render org-air-view--items nil)
        (should (string-match-p "stale \u2219 refreshing"
                                (substring-no-properties (buffer-string))))
        (should-not (timerp org-air-view--refresh-timer))  ; P0: none in batch
        (org-air-r26--run-slices)
        (should-not org-air-view--refresh-state)
        (should-not (timerp org-air-view--refresh-timer))
        (let ((text (substring-no-properties (buffer-string))))
          (should-not (string-match-p "stale \u2219 refreshing" text))
          (should (string-match-p "r34-3 warm probe" text)))))))

(provide 'org-air-round34-test)
;;; org-air-round34-test.el ends here
