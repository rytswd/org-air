;;; org-air-round49-test.el --- executing ERTs for v0.5 round-49 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.5 round-49 (air/v0.5/org-air-round49-design.org):
;; consistent rail placement — ONE shared default + per-view knobs — and
;; the inline rail's bottom-pinned Actions legend fix.
;;
;;   R49-2  CONFIG SCHEME — `org-air-rail-placement' is the ONE shared
;;          default (a SYMBOL now; the R26-5 alist shape still honoured as
;;          legacy) with nil-inherit per-view overrides
;;          `org-air-board-rail-placement' /
;;          `org-air-project-rail-placement' /
;;          `org-air-outline-rail-placement', all resolving through the
;;          ONE `org-air-rail--placement' resolver.
;;   R49-3  CONSISTENT DEFAULT — `side-window' for BOTH the board and the
;;          project (the board's old inline default flips; the project
;;          keeps R26-5's).
;;   R49-4  INLINE RAIL UX — `org-air-project--two-pane-body' sizes the
;;          rail to ONE windowful (the board rail's exact rule) instead of
;;          MAX(doc pane, window), so the Actions legend lands inside the
;;          FIRST windowful for any number of docs while the divider still
;;          spans the full doc list (`--compose-columns' pads the shorter
;;          rail pane).
;;
;; Harness discipline: the seed tests drive REAL first renders under
;; `noninteractive' nil (side windows really exist) with the `unset'
;; sentinel intact, so the placement seed — not a pre-cooked flag — is
;; what is asserted.  Revert-FAILS (verified against the pre-impl trunk):
;;   r49-1 — no resolver / no per-view defcustoms on trunk;
;;   r49-2 — trunk's board half seeds INLINE (the old alist asymmetry);
;;   r49-3 — trunk ignores the per-view overrides (the mirrored split
;;           dies; the symbol-valued shared knob errors the alist-get);
;;   r49-4 — trunk pins the Actions legend to the doc-h foot (line >
;;           the render height with a tall doc list).
;; r49-5 is LOCK-style (passes on both sides): batch renders stay
;; placement-blind, so the placement-default flip moves ZERO byte goldens
;; by construction.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)        ; frozen mtime + mockup harness
(require 'org-air-project-test)            ; project fixture root
(require 'org-air-round26-test)            ; live-project harness (R26-5)

(when (locate-library "org-air")
  (require 'org-air))

;;;; =====================================================================
;;;; Harness — a fresh interactive BOARD whose first render SEEDS placement.
;;;; =====================================================================

(defun org-air-r49--reset-rail-globals ()
  "Cancel pending reconcile timers + reset the R27-1 edge flag (R27 pattern)."
  (when (timerp org-air-rail--reconcile-timer)
    (cancel-timer org-air-rail--reconcile-timer))
  (dolist (tm (copy-sequence timer-list))
    (when (eq (timer--function tm) #'org-air-rail--reconcile-run)
      (cancel-timer tm)))
  (setq org-air-rail--reconcile-timer nil
        org-air-rail--side-was-live nil))

(defmacro org-air-r49--with-live-board (&rest body)
  "Render a FRESH fixture board in a live window; run BODY in its buffer.
`noninteractive' is bound nil so the seed site really consults placement
and side windows really exist.  Unlike the R27 harness, the per-buffer
`org-air-view--rail-popped-out' flag is left at the `unset' sentinel so
the FIRST render runs the R49-2 placement seed — the thing under test.
The render width is pinned wide (120) so the view is never
responsive-narrow board-only on the 80-col batch frame."
  (declare (indent 0) (debug t))
  `(org-air-test-with-fixtures
    (save-window-excursion
      (org-air-r26--kill-aux-buffers)
      (org-air-r49--reset-rail-globals)
      (let ((noninteractive nil)
            (org-air-view-width 120)
            (org-air-rail-focus-on-popout nil)
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
                ;; render below runs the SEED, not a pre-cooked flag.
                (should (eq org-air-view--rail-popped-out 'unset))
                (org-air-view--render org-air-view--items nil)
                ,@body))
          (org-air-r49--reset-rail-globals)
          (org-air-r26--kill-aux-buffers))))))

;;;; =====================================================================
;;;; R49-2 — the ONE resolver (pure table).
;;;; =====================================================================

(ert-deftest org-air-r49-1-resolver-table ()
  "R49-2: `org-air-rail--placement' — shipped defaults CONSISTENT
\(side-window for board AND project), per-view override wins for THAT
view only, nil inherits the shared knob, and the LEGACY R26-5 alist
shape of the shared knob is still honoured per view (the R24/R25
harness let-binds keep working).  Trunk FAILS: no resolver, no
per-view defcustoms, alist-typed shared knob."
  (skip-unless (locate-library "org-air"))
  (should (fboundp 'org-air-rail--placement))
  ;; The shipped defaults: ONE shared SYMBOL knob = `side-window' (R49-3),
  ;; every per-view override nil-inherit.
  (should (eq (default-value 'org-air-rail-placement) 'side-window))
  (should-not (default-value 'org-air-board-rail-placement))
  (should-not (default-value 'org-air-project-rail-placement))
  (should-not (default-value 'org-air-outline-rail-placement))
  ;; CONSISTENT default: both main views resolve the SAME placement.
  (let ((org-air-rail-placement 'side-window)
        (org-air-board-rail-placement nil)
        (org-air-project-rail-placement nil)
        (org-air-outline-rail-placement nil))
    (should (eq (org-air-rail--placement 'board) 'side-window))
    (should (eq (org-air-rail--placement 'project) 'side-window))
    (should (eq (org-air-rail--placement 'board)
                (org-air-rail--placement 'project)))
    ;; the R30-4 outline rail resolves through the SAME resolver.
    (should (eq (org-air-rail--placement 'outline) 'side-window)))
  ;; Per-view override WINS — and only for THAT view.
  (let ((org-air-rail-placement 'side-window)
        (org-air-board-rail-placement 'inline)
        (org-air-project-rail-placement nil))
    (should (eq (org-air-rail--placement 'board) 'inline))
    (should (eq (org-air-rail--placement 'project) 'side-window)))
  (let ((org-air-rail-placement 'side-window)
        (org-air-board-rail-placement nil)
        (org-air-project-rail-placement 'inline))
    (should (eq (org-air-rail--placement 'project) 'inline))
    (should (eq (org-air-rail--placement 'board) 'side-window)))
  ;; nil INHERITS a non-default shared value (both flip together).
  (let ((org-air-rail-placement 'inline)
        (org-air-board-rail-placement nil)
        (org-air-project-rail-placement nil)
        (org-air-outline-rail-placement nil))
    (should (eq (org-air-rail--placement 'board) 'inline))
    (should (eq (org-air-rail--placement 'project) 'inline))
    (should (eq (org-air-rail--placement 'outline) 'inline)))
  ;; LEGACY: the R26-5 alist shape bound to the SHARED knob wins per view
  ;; (zero-migration back-compat for custom-set-variables + harnesses).
  (let ((org-air-rail-placement '((board . inline) (project . side-window)))
        (org-air-board-rail-placement nil)
        (org-air-project-rail-placement nil)
        (org-air-outline-rail-placement nil))
    (should (eq (org-air-rail--placement 'board) 'inline))
    (should (eq (org-air-rail--placement 'project) 'side-window))
    ;; a view the alist does not name falls back to `side-window'.
    (should (eq (org-air-rail--placement 'outline) 'side-window)))
  ;; The per-view override beats even the legacy alist.
  (let ((org-air-rail-placement '((board . inline)))
        (org-air-board-rail-placement 'side-window))
    (should (eq (org-air-rail--placement 'board) 'side-window))))

;;;; =====================================================================
;;;; R49-3 — the CONSISTENT side-window default seeds BOTH views.
;;;; =====================================================================

(ert-deftest org-air-r49-2-consistent-default-seeds-both ()
  "R49-3: a fresh interactive BOARD and a fresh interactive PROJECT both
seed the popped side-window rail from the ONE shared default — no `|'
pressed, no placement variable bound.  Trunk FAILS on the board half
\(the R26-5 alist seeded the board INLINE)."
  (skip-unless (locate-library "org-air"))
  ;; Fresh BOARD (the flipped half): popped, live side window, owned by
  ;; the board, NO inline rail text.
  (org-air-r49--with-live-board
    (should (eq org-air-view--rail-popped-out t))
    (should (org-air-rail--popped-p))
    (should (window-live-p (org-air-rail--side-window)))
    (should (eq (org-air-rail--side-owner) (current-buffer)))
    (should-not (org-air-r26--inline-rail-text-p (current-buffer))))
  ;; Fresh PROJECT (R26-5's default, preserved through the resolver).
  (org-air-r26--with-live-project
    (should (eq org-air-view--rail-popped-out t))
    (should (org-air-rail--popped-p))
    (should (window-live-p (org-air-rail--side-window)))
    (should (eq (org-air-rail--side-owner) (current-buffer)))
    (should-not (org-air-r26--inline-rail-text-p (current-buffer)))))

;;;; =====================================================================
;;;; R49-2 — per-view override splits the two views (live seeds).
;;;; =====================================================================

(ert-deftest org-air-r49-3-per-view-override-splits ()
  "R49-2: a per-view override pins THAT view only; the other view keeps
the shared default; nil inherits.  Trunk FAILS: the overrides do not
exist (the mirrored project split dies) and a symbol-valued shared knob
breaks the alist-get seed."
  (skip-unless (locate-library "org-air"))
  ;; Board pinned INLINE, shared side-window: the board renders the
  ;; inline two-pane rail; a fresh project still pops.
  (let ((org-air-rail-placement 'side-window)
        (org-air-board-rail-placement 'inline))
    (org-air-r49--with-live-board
      (should-not (org-air-rail--popped-p))
      (should-not (window-live-p (org-air-rail--side-window)))
      (should (org-air-r26--inline-rail-text-p (current-buffer))))
    (org-air-r26--with-live-project
      (should (org-air-rail--popped-p))
      (should (window-live-p (org-air-rail--side-window)))
      (should-not (org-air-r26--inline-rail-text-p (current-buffer)))))
  ;; The MIRRORED split: project pinned INLINE, shared side-window.
  (let ((org-air-rail-placement 'side-window)
        (org-air-project-rail-placement 'inline))
    (org-air-r26--with-live-project
      (should-not (org-air-rail--popped-p))
      (should-not (window-live-p (org-air-rail--side-window)))
      (should (org-air-r26--inline-rail-text-p (current-buffer))))
    (org-air-r49--with-live-board
      (should (org-air-rail--popped-p))
      (should (window-live-p (org-air-rail--side-window)))))
  ;; nil INHERITS: shared flipped to `inline', no overrides -> BOTH views
  ;; seed inline (consistent the other way too).
  (let ((org-air-rail-placement 'inline))
    (org-air-r49--with-live-board
      (should-not (org-air-rail--popped-p))
      (should (org-air-r26--inline-rail-text-p (current-buffer))))
    (org-air-r26--with-live-project
      (should-not (org-air-rail--popped-p))
      (should (org-air-r26--inline-rail-text-p (current-buffer))))))

;;;; =====================================================================
;;;; R49-4 — inline rail: calendar + Actions inside the FIRST windowful.
;;;; =====================================================================

(defconst org-air-r49--tall-height 30
  "Pinned render height for the tall-doc-list inline project render.")

(defconst org-air-r49--tall-ndocs 40
  "Doc count for the tall project fixture — doc pane HEIGHT > the window.")

(defmacro org-air-r49--with-tall-inline-project (&rest body)
  "Open an INLINE project over a synthetic 40-doc tree; run BODY in it.
The doc pane (40 one-line doc rows + headers) is TALLER than the pinned
render height (`org-air-r49--tall-height'), the design's exact (B)
shape.  Placement is pinned inline BOTH ways — the R49-2 per-view
override AND the legacy R26-5 alist (which the pre-impl trunk also
honours) — so the two-pane in-buffer rail composes on either side and
the trunk comparison fails on the R49-4 LEGEND conjunct (Actions pinned
to the doc-h foot), not on the placement seed."
  (declare (indent 0) (debug t))
  `(let ((root (make-temp-file "org-air-r49-tall" t)))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "v0.1" root))
           (write-region "" nil (expand-file-name "air-config.toml" root))
           (dotimes (i org-air-r49--tall-ndocs)
             (write-region (format "#+title: Doc %02d\n#+state: ready\n"
                                   (1+ i))
                           nil
                           (expand-file-name (format "v0.1/doc-%02d.org"
                                                     (1+ i))
                                             root)))
           (let ((org-air-sources (list (list :air root)))
                 (org-air-project-group 'directory)
                 (org-air-project-view-width 120)
                 (org-air-view-height org-air-r49--tall-height)
                 ;; inline BOTH ways: the R49-2 override (impl) and the
                 ;; legacy alist (trunk + impl's consp branch), so the
                 ;; trunk revert-check reaches the LEGEND assertion.
                 (org-air-project-rail-placement 'inline)
                 (org-air-rail-placement '((board . inline)
                                           (project . inline)))
                 (org-air-rail-focus-on-popout nil))
             (org-air-project-test--with-frozen-mtime
              (save-window-excursion
                (org-air-r26--kill-aux-buffers)
                (let ((noninteractive nil))
                  (org-air-project))
                (let ((buf (get-buffer "*org-air-project*")))
                  (should buf)
                  (unwind-protect
                      (with-current-buffer buf ,@body)
                    (org-air-r26--kill-aux-buffers)))))))
       (delete-directory root t))))

(ert-deftest org-air-r49-4-project-inline-legend-first-windowful ()
  "R49-4: with placement INLINE and a doc list TALLER than the window,
the rail's Actions legend AND the calendar land inside the FIRST
windowful (rail height = ONE windowful, the board rail's rule), while
the divider column still spans EVERY doc row (`--compose-columns' pads
the shorter rail pane).  Trunk FAILS: the old MAX(doc-h, window)
target pinned Actions to the bottom of the ENTIRE doc list."
  (skip-unless (locate-library "org-air"))
  (org-air-r49--with-tall-inline-project
    ;; the seed really went INLINE (the override path).
    (should-not (org-air-rail--popped-p))
    (should-not (window-live-p (org-air-rail--side-window)))
    (let* ((lines (split-string (buffer-substring-no-properties
                                 (point-min) (point-max))
                                "\n"))
           (height org-air-r49--tall-height)
           (nth-match (lambda (rx)
                        (let ((i (cl-position-if
                                  (lambda (l) (string-match-p rx l))
                                  lines)))
                          (and i (1+ i)))))    ; 1-based line number
           (actions-line (funcall nth-match "| Actions\\b"))
           (calendar-line (funcall nth-match "Su Mo Tu We Th Fr Sa"))
           (doc-idxs (let (acc)
                       (dotimes (i (length lines))
                         (when (string-match-p "READY +Doc [0-9]+"
                                               (nth i lines))
                           (push i acc)))
                       (nreverse acc))))
      ;; Preconditions (anti-tautology): every doc rendered, and the doc
      ;; pane really is TALLER than the render window.
      (should (= (length doc-idxs) org-air-r49--tall-ndocs))
      (should (> (1+ (car (last doc-idxs))) height))
      ;; The Actions legend sits inside the FIRST windowful — visible on
      ;; open with NO scrolling (trunk: pinned past the last doc row).
      (should actions-line)
      (should (<= actions-line height))
      ;; ...and the calendar is above it, inside the first windowful too.
      (should calendar-line)
      (should (< calendar-line actions-line))
      (should (<= calendar-line height))
      ;; The divider column is intact on EVERY doc row — including the
      ;; rows PAST the one-windowful rail (padded blank rail cells), so
      ;; the shorter rail never truncates the fence.
      (let ((cols (mapcar (lambda (i) (cl-position ?| (nth i lines)))
                          doc-idxs)))
        (should (cl-every #'integerp cols))
        (should (= 1 (length (delete-dups cols))))
        ;; the divider sits at the two-pane seam, not somewhere degenerate.
        (should (> (car cols) 60))))))

;;;; =====================================================================
;;;; R49 lock — batch renders stay placement-blind (byte goldens frozen).
;;;; =====================================================================

(ert-deftest org-air-r49-5-batch-placement-blind ()
  "LOCK (passes on both sides): a `noninteractive' render with the shared
default `side-window' produces the byte-identical 120-col board golden
and never creates a rail side window — the placement-default flip is
invisible to batch by construction (the seed is interactive-only)."
  (skip-unless (locate-library "org-air"))
  (let ((org-air-rail-placement 'side-window))
    (org-air-viewport-test-as-gui
      (org-air-viewport-test-with-dashboard 120
        (org-air-viewport-test-assert-matches-mockup 120)
        ;; batch normalised the sentinel to nil — never popped, no window.
        (should-not (org-air-rail--popped-p))
        (should-not (get-buffer-window org-air-rail-buffer-name t))))))

;;;; =====================================================================
;;;; R49 — the `|' toggle + R25-6 reconciler still own the flag.
;;;; =====================================================================

(ert-deftest org-air-r49-6-toggle-and-reconciler-still-work ()
  "The placement seed is consulted ONCE: from the popped R49-3 default,
`|' pops the rail back INLINE, `|' again re-pops it, and a NATIVE close
of the side window reconciles to inline via the R25-6 single-owner
reconciler (user close respected — never re-created behind the user).
`org-air-view-width' is pinned wide (120, the R25 harness discipline) so
the reconciler's width probe never reads the 80-col batch frame as a
responsive-narrow teardown (which correctly KEEPS the flag)."
  (skip-unless (locate-library "org-air"))
  (let ((org-air-view-width 120))
    (org-air-r26--with-live-project
      ;; R49-3 default: popped on open.
      (should (org-air-rail--popped-p))
      (should (window-live-p (org-air-rail--side-window)))
      ;; `|' -> INLINE (the seed does not fight the toggle).
      (org-air-r26--press "|")
      (should-not (org-air-rail--popped-p))
      (should-not (window-live-p (org-air-rail--side-window)))
      (should (org-air-r26--inline-rail-text-p (current-buffer)))
      ;; `|' -> popped OUT again, inline text gone.
      (org-air-r26--press "|")
      (should (org-air-rail--popped-p))
      (should (window-live-p (org-air-rail--side-window)))
      (should-not (org-air-r26--inline-rail-text-p (current-buffer)))
      ;; NATIVE close + R25-6 reconcile -> inline fallback, flag cleared.
      (delete-window (org-air-rail--side-window))
      (select-window (get-buffer-window (current-buffer)))
      (org-air-rail--reconcile-frame (selected-frame))
      (should (null org-air-view--rail-popped-out))
      (should-not (window-live-p (org-air-rail--side-window)))
      (should (org-air-r26--inline-rail-text-p (current-buffer))))))

(provide 'org-air-round49-test)
;;; org-air-round49-test.el ends here
