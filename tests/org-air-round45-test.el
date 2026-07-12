;;; org-air-round45-test.el --- executing ERT for v0.5 round-45 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERT for v0.5 round-45 (air/v0.5/org-air-round45-design.org):
;; the interactive launch HANGS 2-10s on every fresh Emacs open because the
;; cache-HIT branch did a BLOCKING synchronous full-board paint in the command
;; body.  On a fresh Emacs the SVG pill image cache is cold, so that first GUI
;; redisplay rasterizes the viewport's pills synchronously = the freeze.
;;
;; ROOT (R45-1, confirmed): `org-air-view' had two interactive branches with
;; OPPOSITE first-paint discipline.  The CACHED branch called
;; `org-air-view--render' on the FULL board directly, with no prior skeleton
;; and no yield; the COLD branch painted `org-air-view--render-loading' +
;; `(redisplay t)' and filled the board off the critical path.  Deleting the
;; disk cache routed the open through the skeleton-first cold path -> the hang
;; vanished, proving the freeze lives on the cache-HIT branch.  The cost is
;; GUI SVG rasterization (needs a live frame), so it is INVISIBLE to batch —
;; the guard MUST be STRUCTURAL, not a timing measurement.
;;
;; R45-2 fix (Option A): the cache-HIT branch now paints the pill-free chrome
;; skeleton FIRST (instant, like cold), then defers the cached full-board
;; render to a token-guarded ONE-SHOT idle callback SEEDED with the cached
;; items (NO org-ql re-scan — the cache benefit is preserved).  The FRESH
;; sub-case arms `org-air-view--deferred-first-paint'; the STALE sub-case
;; passes COLD to `org-air-view--refresh-start' so its finish-repaint is the
;; deferred full render behind the skeleton.  `noninteractive'/batch and WARM
;; re-open keep the EXACT synchronous `--render' (no skeleton, no timer) so the
;; byte goldens are untouched.
;;
;; These ERTs drive the entry DETERMINISTICALLY: `noninteractive' is bound nil
;; to reach the interactive branches, but `run-with-idle-timer' is stubbed to
;; CAPTURE the one-shot (no real timer fires) and `--render-loading'/`--render'
;; are spied so the call ORDER is observable.  The deferred callback is then
;; invoked BY HAND.  The decisive fences (each REVERT-FAILS):
;;
;;   1  CACHED BRANCH PAINTS A SKELETON BEFORE ANY FULL RENDER.  With a valid
;;      FRESH cache and the entry driven interactively, `--render-loading'
;;      fires and `--render' on the full board is DEFERRED (scheduled via the
;;      one-shot), NOT called in the command body.  Reverted (the old direct
;;      `--render' with no skeleton) the recorded order is `(render)' with no
;;      skeleton and no one-shot => FAILS.
;;   2  FRESH CACHE DOES NOT RE-SCAN.  On an all-mtimes-match cache the cached
;;      open (and its deferred paint) calls NEITHER `org-air-query-items' NOR
;;      `org-air-query-items-in-files' — scan count == 0 — and the deferred
;;      paint renders exactly the cached items.  Reverting to a re-query path
;;      drives the scan counter > 0 => FAILS.
;;   3  `noninteractive' PATH EXACT.  Under real batch the entry runs the
;;      synchronous `--render' with NO skeleton and NO idle timer armed
;;      (`--refresh-timer'/`--deferred-timer' nil) => the byte goldens are
;;      produced exactly as before (regen zero churn).
;;   4  COLD PATH ALREADY SKELETON-FIRST (regression guard).  The cold branch
;;      still paints `--render-loading' before its paced scan, its full render
;;      is reached only via the idle machine (state `refreshing on the launch
;;      frame), never synchronously in the command body.
;;   5  NO WEDGE.  A superseded (stale-token) or dead-buffer one-shot is a
;;      silent no-op — no double render, no error, `--loading' never strands.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-air-test-helpers)

(when (locate-library "org-air")
  (require 'org-air))

;;;; -------------------------------------------------------------------
;;;; Scaffolding
;;;; -------------------------------------------------------------------

(defvar org-air-r45--calls nil
  "Ordered spy log of full-board render events (`loading' / `render').
Newest first (pushed): `loading' = `org-air-view--render-loading' (the
pill-free skeleton), `render' = `org-air-view--render' (the full board).")

(defvar org-air-r45--idle nil
  "Captured one-shot as (FN . ARGS) from the stubbed `run-with-idle-timer'.
The cache-HIT FRESH branch schedules `org-air-view--deferred-first-paint'
here instead of on a real idle timer, so the deferred paint can be fired
deterministically by hand.")

(defmacro org-air-r45--with-fresh-cache (&rest body)
  "Fixtures + a VALID FRESH on-disk cache (all mtimes match); no warm buffer.
Writes the cache from one full scan whose mtime snapshot matches the files
on disk, so `org-air-view--cache-load' returns FRESH (no stale files).  The
board buffer name is scratch so the entry sees NO in-buffer warm items and
takes the cache-HIT branch.  BODY runs with the cache primed."
  (declare (indent 0) (debug t))
  `(org-air-test-with-fixtures
    (let* ((org-air-view-width 120)
           (org-air-view-height 50)
           (org-air-view-buffer-name "*org-air-r45*")
           (org-air-cache-file
            (expand-file-name "cache/board-r45.eld" org-air-test--dir)))
      (unwind-protect
          (progn
            ;; FRESH cache: items + a snapshot of the SAME files, unchanged
            ;; on disk afterwards => cache-load reports no stale files.
            (let ((files (org-air-query-files)))
              (org-air-view--cache-write
               (org-air-query-items)
               (org-air-view--mtimes-snapshot files)))
            ,@body)
        (when (get-buffer org-air-view-buffer-name)
          (let ((kill-buffer-query-functions nil))
            (kill-buffer org-air-view-buffer-name)))))))

(defmacro org-air-r45--with-no-cache (&rest body)
  "Fixtures with NO on-disk cache (a non-existent cache file) + scratch buffer.
`org-air-view--cache-load' returns nil, so the entry takes the COLD branch."
  (declare (indent 0) (debug t))
  `(org-air-test-with-fixtures
    (let* ((org-air-view-width 120)
           (org-air-view-height 50)
           (org-air-view-buffer-name "*org-air-r45*")
           (org-air-cache-file
            (expand-file-name "cache/absent-r45.eld" org-air-test--dir)))
      (unwind-protect
          (progn ,@body)
        (when (get-buffer org-air-view-buffer-name)
          (let ((kill-buffer-query-functions nil))
            (kill-buffer org-air-view-buffer-name)))))))

(defmacro org-air-r45--driving-interactive (&rest body)
  "Run BODY with `org-air-view' driven INTERACTIVELY yet deterministically.
`noninteractive' is bound nil (so the entry leaves the batch sync arm and
takes the cache-HIT / cold branch), `display-graphic-p' is stubbed t, and
all frame side effects are neutralised: `pop-to-buffer'/`redisplay' are
no-ops, `run-with-idle-timer' CAPTURES the one-shot into `org-air-r45--idle'
(no real timer ever fires), and `--render-loading'/`--render' are spied
into `org-air-r45--calls' (call ORDER only — no actual paint).  Reset both
logs first."
  (declare (indent 0) (debug t))
  `(let ((org-air-r45--calls nil)
         (org-air-r45--idle nil)
         (noninteractive nil))
     (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
               ((symbol-function 'redisplay) (lambda (&rest _) t))
               ((symbol-function 'pop-to-buffer)
                (lambda (buf &rest _) (set-buffer (get-buffer buf))))
               ((symbol-function 'org-air-view--render-loading)
                (lambda (&rest _) (push 'loading org-air-r45--calls)))
               ((symbol-function 'org-air-view--render)
                (lambda (&rest _) (push 'render org-air-r45--calls)))
               ((symbol-function 'run-with-idle-timer)
                (lambda (_secs _repeat fn &rest args)
                  (setq org-air-r45--idle (cons fn args))
                  'org-air-r45--fake-timer)))
       ,@body)))

(defun org-air-r45--fire-one-shot ()
  "Invoke the captured deferred one-shot by hand (the idle tick)."
  (should org-air-r45--idle)
  (apply (car org-air-r45--idle) (cdr org-air-r45--idle)))

;;;; -------------------------------------------------------------------
;;;; 1. Cached branch paints a skeleton BEFORE any full render
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r45-cached-branch-skeleton-before-full-render ()
  "R45-2 fence 1: with a valid FRESH cache the interactive open paints the
pill-free skeleton FIRST (`--render-loading') and DEFERS the full-board
`--render' to a token-guarded one-shot idle callback — it is NEVER called
synchronously in the command body.  Firing the one-shot then produces the
full render.

REVERT-FAILS: the pre-R45 cache-HIT arm called `org-air-view--render'
directly with no prior skeleton, so the recorded order would be `(render)'
with NO `loading' and NO captured one-shot — every assertion below fails."
  (skip-unless (locate-library "org-air"))
  (should (fboundp 'org-air-view--deferred-first-paint))
  (should (fboundp 'org-air-view--deferred-arm))
  (org-air-r45--with-fresh-cache
    (org-air-r45--driving-interactive
      (org-air-view)
      (ert-info ((format "calls=%S idle=%S" org-air-r45--calls org-air-r45--idle))
        ;; the skeleton is the ONLY thing painted on the launch critical
        ;; path — the full board render is deferred, not done in the body.
        (should (equal org-air-r45--calls '(loading)))
        (should-not (memq 'render org-air-r45--calls))
        ;; the deferred full render is scheduled as the documented one-shot.
        (should org-air-r45--idle)
        (should (eq (car org-air-r45--idle)
                    'org-air-view--deferred-first-paint)))
      ;; the one-shot, fired off the launch path, paints the full board.
      (org-air-r45--fire-one-shot)
      (should (memq 'render org-air-r45--calls))
      ;; order: skeleton strictly before the full board.
      (should (equal (reverse org-air-r45--calls) '(loading render))))))

;;;; -------------------------------------------------------------------
;;;; 2. FRESH cache does NOT re-scan (org-ql query count == 0)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r45-fresh-cache-no-rescan ()
  "R45-2 fence 2: on an all-mtimes-match cache the cached open does ZERO
org-ql scans — NEITHER `org-air-query-items' NOR `org-air-query-items-in-
files' is called on the launch path OR in the deferred paint — and the
cached items are seeded and rendered as-is (the cache's whole benefit).

REVERT-FAILS: routing the cached open through a (re)query drives the scan
counter above 0."
  (skip-unless (locate-library "org-air"))
  (org-air-r45--with-fresh-cache
    (let ((scans 0))
      (cl-letf* ((items-orig (symbol-function 'org-air-query-items))
                 ((symbol-function 'org-air-query-items)
                  (lambda (&rest a) (cl-incf scans) (apply items-orig a)))
                 (inf-orig (symbol-function 'org-air-query-items-in-files))
                 ((symbol-function 'org-air-query-items-in-files)
                  (lambda (&rest a) (cl-incf scans) (apply inf-orig a))))
        (org-air-r45--driving-interactive
          (org-air-view)
          ;; skeleton-first, and NOT ONE org-ql scan on the cache-fresh path.
          (should (equal org-air-r45--calls '(loading)))
          (should (= scans 0))
          ;; the cached items are seeded (the deferred paint renders THESE).
          (with-current-buffer "*org-air-r45*"
            (should org-air-view--items))
          ;; firing the deferred paint renders the cached board — still no
          ;; scan (it renders the seeded items, never re-queries).
          (org-air-r45--fire-one-shot)
          (should (memq 'render org-air-r45--calls))
          (should (= scans 0)))))))

;;;; -------------------------------------------------------------------
;;;; 3. `noninteractive' path EXACT — synchronous render, no skeleton/timer
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r45-noninteractive-sync-no-skeleton ()
  "R45-2 fence 3: under real `noninteractive' (the byte gate) the entry runs
the EXACT synchronous `org-air-view--render' with NO skeleton and NO idle
timer armed (`--refresh-timer'/`--deferred-timer' nil) — so every byte
fixture is produced exactly as before (`make regen-mockups' zero churn).

The skeleton-first async path is GUI/interactive ONLY; it must never touch
the batch render."
  (skip-unless (locate-library "org-air"))
  (org-air-r45--with-fresh-cache
    (let ((calls nil))
      (cl-letf* (((symbol-function 'pop-to-buffer)
                  (lambda (buf &rest _) (set-buffer (get-buffer buf))))
                 ((symbol-function 'org-air-view--render-loading)
                  (lambda (&rest _) (push 'loading calls)))
                 (render-orig (symbol-function 'org-air-view--render))
                 ((symbol-function 'org-air-view--render)
                  (lambda (&rest a) (push 'render calls) (apply render-orig a))))
        (should noninteractive)                 ; the real batch gate
        (org-air-view)
        ;; synchronous full render, NO skeleton on the batch path.
        (should (memq 'render calls))
        (should-not (memq 'loading calls))
        (with-current-buffer "*org-air-r45*"
          ;; no async machinery armed under batch.
          (should-not org-air-view--refresh-timer)
          (should-not org-air-view--deferred-timer)
          (should-not org-air-view--refresh-state))))))

;;;; -------------------------------------------------------------------
;;;; 4. Cold path already skeleton-first (regression guard)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r45-cold-path-skeleton-first ()
  "R45-2 fence 4: with NO cache the cold branch still paints the skeleton
FIRST and defers the full board off the launch critical path — the paced
machine is `refreshing on the launch frame and the full `--render' is
reached ONLY via the idle machine's finish-repaint, never synchronously in
the command body.  Documents that the fix did not regress the cold feel and
that the shared skeleton-first invariant holds for BOTH branches."
  (skip-unless (locate-library "org-air"))
  (org-air-r45--with-no-cache
    (org-air-r45--driving-interactive
      (org-air-view)
      (ert-info ((format "calls=%S" org-air-r45--calls))
        ;; skeleton painted; NO full board render on the launch path.
        (should (memq 'loading org-air-r45--calls))
        (should-not (memq 'render org-air-r45--calls)))
      (with-current-buffer "*org-air-r45*"
        ;; the paced machine owns the deferred full render (off critical path).
        (should (eq org-air-view--refresh-state 'refreshing))
        ;; drive the slices: the full render is reached ONLY here, via the
        ;; machine's finish-repaint — not on the launch frame.
        (let ((token org-air-view--refresh-token) (n 40))
          (while (and (> n 0) (eq org-air-view--refresh-state 'refreshing))
            (org-air-view--refresh-run-slice (current-buffer) token)
            (cl-decf n)))
        (should-not (eq org-air-view--refresh-state 'refreshing)))
      (should (memq 'render org-air-r45--calls)))))

;;;; -------------------------------------------------------------------
;;;; 5. No wedge — superseded / dead-buffer one-shot is a silent no-op
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r45-deferred-one-shot-no-wedge ()
  "R45-2 fence 5: the deferred one-shot is token-guarded and buffer-guarded.
A re-open / refresh before it fires BUMPS the token, so the stale one-shot
is a silent no-op (no double render); a DEAD buffer is likewise a no-op (no
error).  `--loading' never strands.

REVERT-FAILS a naive unconditional one-shot: it would double-render the
board over a fresh (re)paint."
  (skip-unless (locate-library "org-air"))
  (org-air-r45--with-fresh-cache
    (org-air-r45--driving-interactive
      (org-air-view)
      (should org-air-r45--idle)
      (let* ((buf (nth 0 (cdr org-air-r45--idle)))
             (stale-token (nth 1 (cdr org-air-r45--idle))))
        (should (buffer-live-p buf))
        ;; supersede: a re-open / g-refresh bumped the token in the buffer.
        (with-current-buffer buf
          (cl-incf org-air-view--refresh-token)
          (should-not (eq stale-token org-air-view--refresh-token)))
        ;; the stale one-shot fires -> silent no-op, no render, no error.
        (setq org-air-r45--calls nil)
        (org-air-view--deferred-first-paint buf stale-token)
        (should-not (memq 'render org-air-r45--calls))
        (with-current-buffer buf
          (should-not org-air-view--loading)))
      ;; a DEAD buffer one-shot is also a silent no-op (buffer-live-p guard).
      (let ((dead (generate-new-buffer " *org-air-r45-dead*")))
        (kill-buffer dead)
        (setq org-air-r45--calls nil)
        (org-air-view--deferred-first-paint dead 0)
        (should-not (memq 'render org-air-r45--calls))))))

(provide 'org-air-round45-test)
;;; org-air-round45-test.el ends here
