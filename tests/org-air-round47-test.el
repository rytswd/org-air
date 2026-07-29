;;; org-air-round47-test.el --- executing ERTs for v0.5 round-47 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.5 round-47 (air/v0.5/org-air-round47-design.org):
;; MOUSE HOVER is very noticeably slow on the board (Emacs 30, GUI).  R32-1
;; scoped the hover highlight to one row and R33-2 made the hover hot path run
;; ZERO org-air Lisp (pure redisplay, background-only face) — yet hovering
;; stayed slow.
;;
;; ROOT (R47-1, confirmed): the defeated cache is Emacs's C-LEVEL image
;; (raster) cache, NOT the org-air elisp pill memo.  Since Emacs 30 (commit
;; e69fafdb, bug#67794, "Respect mouse-face on SVG image glyphs")
;; `draw_glyphs' (xdisp.c) re-looks-up EVERY SVG image glyph drawn under
;; DRAW_MOUSE_FACE with the hover face: `lookup_image (f, s->img->spec,
;; hlinfo->mouse_face_face_id)'.  `lookup_image' keys the C cache on the
;; face's fg/bg/font (image.c `search_image_cache'), and the hover
;; :background DIFFERS from the row face BY CONSTRUCTION, so the lookup
;; MISSES and `svg_load' (librsvg parse + full raster) runs SYNCHRONOUSLY
;; inside redisplay for every SVG pill under the hovered run — per crossing.
;; Our rows put `mouse-face' over the WHOLE row body including every pill
;; image glyph, so a crossing paid ~pills-per-row librsvg rasters (the R45
;; cold-pill cost relocated onto the hover hot path).  The elisp memo
;; (`org-air-view--svg-image-cache') is healthy and hover-irrelevant (0
;; builds per sweep — R33-2 still true), and the inspector/R28 outline
;; followers are command-driven + debounced (a hover dispatches no command).
;;
;; FIX (R47-2, landed): `org-air-view--insert-row' pops `mouse-face' out of
;; the whole-extent PROPS and applies it ONLY over the text-only TITLE BAND
;; [title-start, cluster-start) — the R21-2/R46 title mark through the flex
;; pad, ending at the R40-2 fence.  Invariant: NO buffer position may carry
;; both `mouse-face' and an image `display'.  Second seam: the calendar
;; TODAY cell (the one day cell that is NOT plain text — svg background)
;; drops its `mouse-face' too (keymap + `org-air-day' stay clickable).
;;
;; BATCH CONTRACT (the R45 lesson): SVG rasterization timing is GUI-only and
;; structurally invisible to `noninteractive' ERTs, so these guards are
;; STRUCTURAL — they pin the exact preconditions under which the Emacs 30
;; DRAW_MOUSE_FACE block can rasterize at all, with the SVG spec layer forced
;; on via the R24-3 `display-graphic-p' stub ("0 pill image rebuilds per
;; crossing" == "0 svg glyphs under any hover run").  Reverting the fix
;; (mouse-face over the whole row incl. pills) FAILS r47-1/-2/-3.
;;
;; NOTE on the head-start draft of this file: it was validated and REWORKED —
;; its row scanner looped forever (`line-end-position' evaluated at an
;; unmoving `point'), and its commentary pinned the WRONG mechanism ("not
;; re-rasterization"); the design-confirmed diagnosis is the Emacs 30
;; C-image-cache miss above.  Its useful probes (image-cell detection, the
;; memo-build counter) carry over here under the design-blessed test names.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)     ; dashboard harness + as-gui stub
(require 'org-air-project-test)         ; project fixture root + --render
(require 'org-air-round32-test)         ; mouse-face run probes + doc rows
(require 'org-air-round27-test)         ; live-board harness (hook wiring)

(when (locate-library "org-air")
  (require 'org-air))

;;;; =====================================================================
;;;; Harness — the fixture board with EVERY section expanded, rendered
;;;; with real svg pill `display' images (R24-3 GUI stub; pill char
;;;; metrics resolve through the render's own `--char-dimensions').
;;;; =====================================================================

(defmacro org-air-r47--with-gui-board (&rest body)
  "Render the fully-EXPANDED fixture dashboard with svg pills; run BODY.
`display-graphic-p' is stubbed t (R24-3) so `org-air-view--svg-pillify'
emits real `display' images headless; every section is expanded so the
board carries the full fixture row population — the anti-tautology
floor, which is DERIVED from the painted board rather than hardcoded
\(see `org-air-r47-2-hover-run-is-single-title-band').

R93 RE-BLESS: the expanded list carried the PRE-R93 section vocabulary
— it named the retired `stale' bucket and, once R93 split Overdue out
of Needs attention, it no longer named `overdue' at all.  Both are
corrected here so the list means what it says: EVERY board section
expanded."
  (declare (indent 0) (debug t))
  `(org-air-viewport-test-as-gui
     (let ((org-air-view--expanded-sections
            '(inbox overdue upcoming high-priority attention notes backlog)))
       (org-air-viewport-test-with-dashboard '(120 . 80)
         ,@body))))

(defun org-air-r47--item-row-count ()
  "Return the number of ITEM rows the board in this buffer actually painted.
The anti-tautology floor for the hover-run invariant, measured off the
render instead of hardcoded: a board that painted nothing would make
every \"no violation\" assertion vacuous, and a hardcoded floor goes
stale whenever a legitimate product change moves the row population
\(R93 FIX-3 removed three duplicated `#A' rows from Needs attention and
broke the old literal `> 20')."
  (let ((pos (point-min)) (n 0))
    (while (setq pos (text-property-not-all pos (point-max)
                                            'org-air-item nil))
      (setq n (1+ n)
            pos (or (next-single-property-change pos 'org-air-item)
                    (point-max))))
    n))

(defun org-air-r47--image-at-p (pos)
  "Non-nil when POS carries an image `display' spec."
  (let ((d (get-text-property pos 'display)))
    (and d (eq (car-safe d) 'image))))

(defun org-air-r47--overlap-positions ()
  "Return buffer positions carrying BOTH `mouse-face' and an image `display'.
Each returned position is one SVG glyph Emacs 30's DRAW_MOUSE_FACE block
would re-look-up (and, on the C cache miss, re-rasterize) per crossing."
  (let ((bad nil) (pos (point-min)))
    (while (< pos (point-max))
      (when (and (org-air-r47--image-at-p pos)
                 (get-text-property pos 'mouse-face))
        (push pos bad))
      (setq pos (or (next-property-change pos) (point-max))))
    (nreverse bad)))

(defun org-air-r47--image-cell-count ()
  "Return how many image `display' cells the buffer carries (anti-tautology)."
  (let ((n 0) (pos (point-min)))
    (while (< pos (point-max))
      (when (org-air-r47--image-at-p pos) (setq n (1+ n)))
      (setq pos (or (next-property-change pos) (point-max))))
    n))

(defun org-air-r47--hover-rows (prop)
  "Return (BOL . EOL) for every row carrying PROP anywhere on the line.
PROP is `org-air-item' (board) or `org-air-doc' (project); the board's
row properties start after the leading margin, so BOL alone is not
enough (R22-2)."
  (let (rows)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((bol (line-beginning-position)) (eol (line-end-position)))
          (when (text-property-not-all bol eol prop nil)
            (push (cons bol eol) rows)))
        (forward-line 1)))
    (nreverse rows)))

(defun org-air-r47--simulate-hover-sweep ()
  "Simulate the pointer crossing EVERY hover run; return the resolved runs.
The Lisp-visible face of `note_mouse_highlight' per crossing: resolve the
`mouse-face' run's extent (the property-change scan redisplay performs)
plus the `help-echo' / `keymap' lookups at the hovered position.  No
org-air code is called — a hover dispatches no command (R33-2)."
  (let ((runs (org-air-r32--mouse-face-runs)))
    (dolist (run runs)
      (let ((pos (car run)))
        (get-char-property pos 'mouse-face)
        (previous-single-property-change (min (1+ pos) (point-max))
                                         'mouse-face)
        (next-single-property-change pos 'mouse-face)
        (get-char-property pos 'help-echo)
        (get-char-property pos 'keymap)))
    runs))

;;;; =====================================================================
;;;; R47-1 — the invariant, board: NO position carries both `mouse-face'
;;;; and an image `display'.  This count is EXACTLY the number of
;;;; per-crossing SVG re-lookups Emacs 30 performs, so "0 pill image
;;;; rebuilds per crossing" is asserted as "0 svg glyphs under any hover
;;;; run".  REVERT-FAILS: trunk measured 95(+2 calendar) overlaps.
;;;; =====================================================================

(ert-deftest org-air-r47-1-no-image-under-mouse-face-board ()
  "Full fixture board render (GUI stub, all sections expanded): ZERO
buffer positions carry both `mouse-face' and an image `display' spec —
including the calendar band, whose svg-backed TODAY cell was the second
seam the invariant surfaced.  Anti-tautology: the board really renders
svg pill images, and the svg TODAY cell really exists."
  (skip-unless (locate-library "org-air"))
  (org-air-r47--with-gui-board
    ;; anti-tautology: the svg pill path is ON — image cells exist.
    (should (> (org-air-r47--image-cell-count) 0))
    ;; the calendar's svg TODAY cell is present (the second seam is
    ;; genuinely exercised, not skipped by a TTY fallback)...
    (let ((today (let ((pos (point-min)) (found nil))
                   (while (and (not found) (< pos (point-max)))
                     (when (and (get-text-property pos 'org-air-day)
                                (org-air-r47--image-at-p pos))
                       (setq found pos))
                     (setq pos (1+ pos)))
                   found)))
      (should today)
      ;; ...and it carries NO mouse-face over its image display.
      (should-not (get-text-property today 'mouse-face)))
    ;; the invariant: no svg glyph sits under any hover run, anywhere.
    (should (equal (org-air-r47--overlap-positions) '()))))

(ert-deftest org-air-r47-1-no-image-under-mouse-face-project ()
  "Project view render (GUI stub): ZERO positions carry both `mouse-face'
and an image `display' — the doc rows' state badges / pills never sit
under the hover highlight.  REVERT-FAILS: trunk measured 17 overlaps."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-project-test--render
      ;; anti-tautology: the project really renders image cells (badges).
      (should (> (org-air-r47--image-cell-count) 0))
      (should (org-air-r47--hover-rows 'org-air-doc))
      (should (equal (org-air-r47--overlap-positions) '())))))

;;;; =====================================================================
;;;; R47-2 — the hover run is ONE text-only title band per row.
;;;; =====================================================================

(ert-deftest org-air-r47-2-hover-run-is-single-title-band ()
  "Every board item row and project doc row has EXACTLY ONE maximal
`mouse-face' run; it BEGINS at the `org-air-row-title' mark (not BOL —
the prefix badges are outside), EMBEDS NO newline (the R32-1 no-fusion
invariant carried forward), never reaches EOL past the cluster fence, and
contains no image `display'.  Anti-tautology: the expanded board yields
one hover row per painted ITEM row, and the board really is a populated
one.  REVERT-FAILS: the pre-R47 whole-row span starts at BOL and covers
the pill images.

R93 RE-BLESS (collateral, and a floor made honest).  The floor was the
literal `(> (length rows) 20)', evaluated BEFORE the invariant it
guards.  R93 FIX-3 removed three rows from the board — the `#A' items
that Needs attention used to restate from High priority — leaving 19,
so the floor failed while the invariant it protects held perfectly (19
hover rows, 0 violations, measured).  A literal row count was never the
thing being asserted: it is a witness that the board painted.  It is
now DERIVED from the render — every painted item row contributes
exactly one hover row — with a loose populated-board floor underneath,
so a legitimate product change to the row population can never again
read as a hover-invariant failure, while an EMPTY board (the only
tautology this guard exists to catch) still fails it."
  (skip-unless (locate-library "org-air"))
  (cl-flet
      ((check-rows (rows label)
         (let ((cluster-rows 0))
           (dolist (row rows)
             (let* ((bol (car row)) (eol (cdr row))
                    ;; the two-pane board shares physical lines between
                    ;; item rows and the rail's calendar day cells (their
                    ;; own legit `mouse-face' carriers) — the ROW's hover
                    ;; runs are the non-day-cell runs inside the line.
                    (runs (cl-remove-if-not
                           (lambda (r)
                             (and (>= (car r) bol) (<= (cdr r) eol)
                                  (not (get-text-property
                                        (car r) 'org-air-day))))
                           (org-air-r32--mouse-face-runs)))
                    (title (text-property-not-all
                            bol eol 'org-air-row-title nil)))
               (ert-info ((format "%s row @%d" label bol))
                 ;; exactly ONE maximal hover run, fully inside the row.
                 (should (= (length runs) 1))
                 (let ((run (car runs)))
                   ;; it BEGINS at the title mark — after the prefix
                   ;; badges, never at BOL.
                   (should title)
                   (should (= (car run) title))
                   (should (> (car run) bol))
                   (should-not (get-text-property bol 'mouse-face))
                   ;; no newline inside the run (rows never fuse).
                   (should-not
                    (string-match-p
                     "\n" (buffer-substring-no-properties
                           (car run) (cdr run))))
                   ;; the band is TEXT-ONLY: no image display inside it.
                   (let ((p (car run)) (imgs 0))
                     (while (< p (cdr run))
                       (when (org-air-r47--image-at-p p)
                         (setq imgs (1+ imgs)))
                       (setq p (1+ p)))
                     (should (= imgs 0)))
                   ;; it ENDS before the meta cluster: when the row carries
                   ;; cluster images (date/tag pills), they all sit AT or
                   ;; PAST the run's end.
                   (let ((first-img
                          (let ((p (car run)) (found nil))
                            (while (and (not found) (< p eol))
                              (when (org-air-r47--image-at-p p)
                                (setq found p))
                              (setq p (1+ p)))
                            found)))
                     (when first-img
                       (setq cluster-rows (1+ cluster-rows))
                       (should (>= first-img (cdr run)))))))))
           cluster-rows)))
    ;; the board (expanded — the anti-tautology floor, DERIVED).
    (org-air-r47--with-gui-board
      (let ((rows (org-air-r47--hover-rows 'org-air-item))
            (painted (org-air-r47--item-row-count)))
        ;; every painted item row contributes exactly one hover row…
        (should (= (length rows) painted))
        ;; …and the board really is populated (all five task sections
        ;; expanded and non-empty on this fixture).
        (should (> painted 15))
        ;; at least SOME rows carry a pill cluster after the band (the
        ;; "ends before the cluster" clause is genuinely exercised).
        (should (> (check-rows rows "board") 0))))
    ;; the project doc rows (state badges before the title).
    (org-air-viewport-test-as-gui
      (org-air-project-test--render
        (let ((rows (org-air-r47--hover-rows 'org-air-doc)))
          (should rows)
          (check-rows rows "project"))))))

;;;; =====================================================================
;;;; R47-3 — a hover sweep across every row performs ZERO pill work.
;;;; =====================================================================

(ert-deftest org-air-r47-3-zero-pill-builds-per-hover-sweep ()
  "Advice counters on `org-air-view--svg-image-cached' (memo build) and
`svg-image' (raster-spec construction): a simulated hover sweep across
EVERY hover run performs 0 builds and 0 `svg-image' calls (the elisp
layer — already true per R33-2), AND no resolved run contains an svg
glyph (the Emacs 30 C-path guard — REVERT-FAILS: the whole-row span puts
~2 pills under every board run)."
  (skip-unless (locate-library "org-air"))
  (org-air-r47--with-gui-board
    (let* ((builds 0) (svg-calls 0)
           (real-memo (symbol-function 'org-air-view--svg-image-cached))
           (real-svg (symbol-function 'svg-image)))
      (cl-letf (((symbol-function 'org-air-view--svg-image-cached)
                 (lambda (key thunk)
                   (funcall real-memo key
                            (lambda ()
                              (cl-incf builds)
                              (funcall thunk)))))
                ((symbol-function 'svg-image)
                 (lambda (&rest args)
                   (cl-incf svg-calls)
                   (apply real-svg args))))
        ;; positive control: the counter machinery is live (a fresh memo
        ;; key really increments `builds').
        (org-air-view--svg-image-cached (list 'org-air-r47-probe (random))
                                        (lambda () 'probe))
        (should (= builds 1))
        (setq builds 0)
        ;; the sweep: cross every hover run the way redisplay resolves it.
        (let ((runs (org-air-r47--simulate-hover-sweep)))
          ;; anti-tautology: a real population was swept.
          (should (> (length runs) 20))
          ;; elisp layer: ZERO pill builds, ZERO raster-spec constructions.
          (should (= builds 0))
          (should (= svg-calls 0))
          ;; C layer precondition: ZERO svg glyphs inside any resolved run,
          ;; so the DRAW_MOUSE_FACE re-lookup block can never match.
          (dolist (run runs)
            (let ((p (car run)))
              (while (< p (cdr run))
                (should-not (org-air-r47--image-at-p p))
                (setq p (1+ p))))))))))

;;;; =====================================================================
;;;; R47-4 — a hover crossing runs NO inspector / outline recompute.
;;;; =====================================================================

(ert-deftest org-air-r47-4-no-inspector-or-outline-recompute-per-crossing ()
  "Counters on `org-air-view--maybe-update-inspector',
`org-air-view--inspector-update-now' and
`org-air-outline--highlight-update' stay 0 across a simulated hover sweep
of every run; no inspector debounce timer is armed; and the follow hooks
live ONLY on pre/post-command hooks (a hover dispatches no command) — the
R33-2 lock extended so a future hover-follow can never reintroduce
per-motion work."
  (skip-unless (locate-library "org-air"))
  (org-air-r47--with-gui-board
    (let ((maybe 0) (now 0) (outline 0)
          ;; scope the GLOBAL debounce-timer var to this test so a stale
          ;; timer leaked by an earlier suite member (armed for a buffer
          ;; killed since) can never masquerade as sweep-armed work — only
          ;; a timer the SWEEP arms is visible here.
          (org-air-view--inspector-timer nil)
          (real-maybe (symbol-function 'org-air-view--maybe-update-inspector))
          (real-now (symbol-function 'org-air-view--inspector-update-now))
          (real-outline (symbol-function 'org-air-outline--highlight-update)))
      (cl-letf (((symbol-function 'org-air-view--maybe-update-inspector)
                 (lambda (&rest args)
                   (cl-incf maybe) (apply real-maybe args)))
                ((symbol-function 'org-air-view--inspector-update-now)
                 (lambda (&rest args)
                   (cl-incf now) (apply real-now args)))
                ((symbol-function 'org-air-outline--highlight-update)
                 (lambda (&rest args)
                   (cl-incf outline) (apply real-outline args))))
        (let ((runs (org-air-r47--simulate-hover-sweep)))
          (should (> (length runs) 20))
          (should (= maybe 0))
          (should (= now 0))
          (should (= outline 0))))
      ;; no debounce timer was armed by the sweep.
      (should-not (timerp org-air-view--inspector-timer))
      ;; no mouse-motion machinery exists to run anything per crossing
      ;; (the R33-2 locks, re-asserted on the hovered board).
      (should-not (bound-and-true-p mouse-movement-hook))
      (should-not track-mouse)
      (should-not (and (current-local-map)
                       (lookup-key (current-local-map) [mouse-movement])))))
  ;; the followers live ONLY on the COMMAND hooks: a live (interactive)
  ;; board wires them into the buffer-local `post-command-hook' — which a
  ;; hover never fires — and into NO mouse hook.  (Batch renders skip the
  ;; install entirely: the P0 `noninteractive' guard.)
  (org-air-r27--with-live-board
    (should (memq 'org-air-view--inspector-post-command post-command-hook))
    (should (memq 'org-air-view--view-pane-post-command post-command-hook))
    (should-not (bound-and-true-p mouse-movement-hook))))

(provide 'org-air-round47-test)
;;; org-air-round47-test.el ends here
