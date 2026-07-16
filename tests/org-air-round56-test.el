;;; org-air-round56-test.el --- executing ERTs for v0.5 round-56 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.5 round-56 (air/v0.5/org-air-round56-design.org):
;; inbox-first progressive paint, adaptive cold pacing, and the visible
;; `⟳ scanning N/M…' progress indicator — the measured stuck-refresh fixes
;; (the board sat on `refreshing…' with NO content for minutes at ~1700
;; files; NOT a hang, NOT scan cost: self-destructing paint gates + a
;; 9-12% duty-cycle pacer).  All BATCH/headless, driven through the spec's
;; named seams (the slice runner, the deferred stale one-shot, the chain
;; armer, the watchdog fire); paint gates are opened by let-binding
;; `noninteractive' nil ONLY around the direct slice drives — no real
;; timer ever fires (the chain re-arms only while a chain handle exists,
;; and none is armed under batch).  Reverting the R56 impl fails each:
;;
;;   R56-1 INBOX-FIRST (P1c, ERT seam 1) — the paced queue puts
;;         `org-air-inbox-file' at position 1 even when it sorts LAST in
;;         enumeration AND carries the OLDEST mtime (anti-tautology: only
;;         the explicit inbox rule can head it), the rest mtime-DESC with
;;         stable ties (pure `--refresh-queue-order' table); ONE budgeted
;;         slice paints REAL inbox content while the queue is still >90%
;;         full.  Reverting P1c (ordering or the immediate first paint)
;;         fails.
;;   R56-2 THE STREAM REPEATS (P1b, seams 2/4) — a cold fill under a
;;         stubbed clock stepping 0.6s per slice produces >=3 progressive
;;         paints strictly increasing in item count (`--loading' nil after
;;         the first; the stream runs to the finish swap), and slices of
;;         EMPTY files paint nothing (the new-items condition).  Reverting
;;         P1b (the self-clearing `--loading' gate) yields exactly 1 paint
;;         — fails.
;;   R56-3 CACHE-STALE PAINTS THE CACHED BOARD (P1a, seam 3) — the STALE
;;         arm's deferred one-shot renders the FULL cached board (zero
;;         scans in the call) and then OWNS the paced kickoff: content on
;;         screen while the machine is `refreshing' with a non-empty
;;         queue; the P3b banner tick surfaces `scanning N/M' on that
;;         painted board.  Reverting P1a (skeleton-until-finish) fails.
;;   R56-3b DISPATCHER STALE-ARM FENCE (P1a wiring; R56fix Blocker 1) —
;;         R56-3 drives the one-shot DIRECTLY, so it cannot fence the
;;         dispatcher WIRING.  This fence drives the REAL `org-air-view'
;;         command through the R45-2 interactive scaffolding (stubbed
;;         `run-with-idle-timer' capture) over a STALE cache: the
;;         captured one-shot IS `org-air-view--deferred-stale-paint',
;;         the command body NEVER enters `refreshing' (no pre-R56
;;         command-body `--refresh-start' kickoff, no chain timer), and
;;         the hand-fired one-shot renders the cached board BEFORE the
;;         machine goes live.  Reverting ONLY the dispatcher's STALE arm
;;         to the command-body kickoff (skeleton-until-finish — the
;;         user's minutes-long defect) fails.
;;   R56-4 ADAPTIVE PACER (P2a, seam 5) — `--refresh-next-gap' is pure
;;         and monotone (0.01 uninterrupted; 0.15/0.3/0.6/0.6 across
;;         consecutive aborts; 0.01 on recovery; bounded both ways); the
;;         chain records the gap and keeps EXACTLY ONE pending one-shot;
;;         an uninterrupted drain spends FAR less wall clock in gaps than
;;         the retired 0.2s duty-cycle pacer for the same slices; a real
;;         queue CONVERGES in a bounded number of slices (never
;;         indefinite).  Reverting P2a (constant 0.2s / idle pacing)
;;         fails.
;;   R56-4b SLICE-RUNNER RE-ARM FENCE (P2a wiring; R56fix Blocker 2) —
;;         R56-4 part (3) proves the chain armer in ISOLATION; this
;;         fence proves the slice runner CALLS it.  Under a seeded DUMMY
;;         chain handle with the interactive gate open and
;;         `--refresh-chain-arm' spied (spy only — no real arm): a clean
;;         slice with a non-empty queue re-arms with `completed', an
;;         input-aborted slice with `aborted', and the FINAL
;;         queue-emptying slice does NOT re-arm (the finish disarms the
;;         handle).  DELETING the run-slice re-arm hunk (interactive
;;         fills degrade to ~1 slice per 8s watchdog period ≈ tens of
;;         minutes at ~1800 files) fails.
;;   R56-5 PROGRESS SEGMENT TRUTH (P3a/P3b/P3c, seam 7) — the skeleton
;;         carries `scanning 0/N…' (banner AND centred body line), the
;;         numbers GROW as slices land, the segment shows on a PAINTED
;;         board with `--loading' nil (independence from the flag that
;;         killed `loading N/M') wearing the salient
;;         `org-air-face-progress', and it clears crisply at the finish
;;         swap.  Reverting P3a/P3c fails.
;;   R56-6 NEVER-HANG PRESERVED (P2b, seam 8) — pending input aborts a
;;         slice with queue/accumulator/state untouched (C-g abortable);
;;         the watchdog runs ZERO scans on a >budget queue (never a
;;         main-thread force-scan) and interactively re-arms the ONE-SHOT
;;         chain; the queue then converges by pacing.
;;   R56-7 WARM SINGLE-SWAP LAW (P1b mode condition, seam 9) — a painted
;;         board + a large changed set through the paced path performs
;;         ZERO progressive paints and exactly ONE content repaint (the
;;         finish swap); stream mode never engages.  A LOCK, not a
;;         revert-fence: pre-R56 warm refreshes never streamed either —
;;         this fences P1b's NEW stream mode from ever leaking into warm
;;         refreshes (breaking the mode condition to always-stream fails
;;         it).
;;
;; REVERT-FAIL verified against the pre-impl trunk (mmttlvtu) in a
;; scratch workspace: R56-1..6 all fail there (R56-7 passes by design —
;; the lock above), as do the six re-blessed legacy ERTs.  The R56fix
;; fences verified against PARTIAL reverts of the impl tip in a scratch
;; sandbox (the two holes the R56 review blocked on): reverting only the
;; dispatcher's STALE arm to the command-body kickoff fails R56-3b;
;; deleting only the run-slice re-arm hunk fails R56-4b.
;;
;; Perf probes stay OUT of the gate (tiny corpora only; no large scan in
;; `make check') — the wall-clock claims are locked as pure arithmetic
;; over the pacing constants, per the round instructions.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)      ; frozen-clock render env
(require 'org-air-round26-test)          ; cache env helpers
(require 'org-air-round45-test)          ; interactive-drive scaffolding
(require 'org-air-round53-test)          ; corpus/board/counting helpers

(when (locate-library "org-air")
  (require 'org-air))

;;;; -------------------------------------------------------------------
;;;; R56-1 — inbox-first queue order + immediate first paint (P1c)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r56-1-inbox-paints-before-full-scan ()
  "The inbox bucket paints REAL content BEFORE the full scan completes.
Corpus where the inbox sorts LAST in enumeration (a00..a17 < inbox) AND
carries the OLDEST mtime (so recency ordering alone could never head it
— anti-tautology): `--refresh-start t' queues the inbox at position 1,
and ONE budgeted slice + the immediate first paint put the inbox capture
in `org-air-view--items' AND on the rendered board while the scan queue
is still >90% full.  The ordering seam is also pinned PURE: inbox first,
rest mtime DESC, ties/missing-mtimes stable.  Reverting P1c fails."
  (skip-unless (locate-library "org-air"))
  ;; the PURE ordering table (never stats a file).
  (let ((org-air-inbox-file "/air/inbox.org"))
    (should (equal (org-air-view--refresh-queue-order
                    '("/air/a.org" "/air/b.org" "/air/inbox.org"
                      "/air/c.org" "/air/d.org" "/air/e.org")
                    '(("/air/a.org" . 10) ("/air/b.org" . 30)
                      ("/air/c.org" . 20) ("/air/d.org" . 10)))
                   '("/air/inbox.org" "/air/b.org" "/air/c.org"
                     "/air/a.org" "/air/d.org" "/air/e.org")))
    ;; no inbox in the enumerated set: pure recency, stable ties.
    (should (equal (org-air-view--refresh-queue-order
                    '("/air/a.org" "/air/b.org")
                    '(("/air/a.org" . 10) ("/air/b.org" . 30)))
                   '("/air/b.org" "/air/a.org"))))
  ;; the EXECUTING seam: one slice = triageable inbox content on screen.
  (let ((specs (list '("inbox.org" . "* TODO Inbox capture first :inbox:\n"))))
    (dotimes (i 18)
      (push (cons (format "a%02d.org" i) (format "* TODO Deep item %02d\n" i))
            specs))
    (org-air-r53--with-corpus specs
      ;; inbox = OLDEST mtime, so only the P1c inbox rule can head it.
      (set-file-times org-air-inbox-file
                      (time-subtract (current-time) 3600))
      (org-air-r53--with-board
        (setq org-air-view--items nil
              org-air-view--items-mtimes nil)
        (org-air-view--refresh-start t)
        (should (eq org-air-view--refresh-state 'refreshing))
        ;; STREAM mode is set AT refresh-start for a no-content fill —
        ;; pre-R56 the flag was first raised by the first paint, so this
        ;; read right after start is itself a revert fence.
        (should org-air-view--refresh-progressive)
        ;; queue head IS the inbox despite last-in-enumeration + oldest.
        (should (equal (car org-air-view--refresh-queue)
                       org-air-inbox-file))
        (let* ((total org-air-view--refresh-total)
               (paints 0)
               (paint-orig
                (symbol-function 'org-air-view--refresh-progressive-paint)))
          (should (= total 19))
          (cl-letf (((symbol-function 'org-air-view--refresh-progressive-paint)
                     (lambda ()
                       (cl-incf paints)
                       (funcall paint-orig))))
            (let ((org-air-refresh-slice-budget 0)   ; one file: the inbox
                  (noninteractive nil))              ; open the paint gate
              (org-air-view--refresh-run-slice (current-buffer)
                                               org-air-view--refresh-token)))
          ;; the FIRST slice painted immediately (no throttle on paint 1)…
          (should (= paints 1))
          (should (org-air-test-find-item "Inbox capture first"
                                          org-air-view--items))
          (should (string-match-p "Inbox capture first" (buffer-string)))
          ;; …while the scan queue is still >90% full and the machine live.
          (should (eq org-air-view--refresh-state 'refreshing))
          (should (> (/ (float (length org-air-view--refresh-queue)) total)
                     0.9)))))))

;;;; -------------------------------------------------------------------
;;;; R56-2 — the progressive stream repeats to the finish (P1b)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r56-2-cold-stream-paints-repeatedly ()
  "A cold fill streams REPEATED progressive paints, never one-and-frozen.
Stubbed clock stepping 0.6s per slice (`org-air-cold-paint-interval'
0.5): >=3 progressive paints, STRICTLY increasing in item count (which
also proves slices of EMPTY files never paint — the new-items
condition), `--loading' nil after the first, stream mode live until the
finish swap ends it.  Reverting P1b (the self-clearing `--loading' gate)
yields exactly ONE paint — fails."
  (skip-unless (locate-library "org-air"))
  (let ((specs (list '("inbox.org" . "* TODO Stream capture :inbox:\n")
                     '("e00.org" . "")          ; empty: 0 items, no paint
                     '("e01.org" . "\n\n"))))   ; blank: 0 items, no paint
    (dotimes (i 6)
      (push (cons (format "f%02d.org" i) (format "* TODO Stream %02d\n" i))
            specs))
    (org-air-r53--with-corpus specs
      (org-air-r53--with-board
        (setq org-air-view--items nil
              org-air-view--items-mtimes nil
              org-air-view--loading t)      ; the real cold open raises it
        (org-air-view--refresh-start t)
        (should (eq org-air-view--refresh-state 'refreshing))
        (should org-air-view--refresh-progressive)   ; STREAM mode from start
        (let* ((fake-now (float-time))
               (paints nil)
               (paint-orig
                (symbol-function 'org-air-view--refresh-progressive-paint))
               (ft-orig (symbol-function 'float-time)))
          (cl-letf (((symbol-function 'float-time)
                     (lambda (&optional time)
                       (if time (funcall ft-orig time) fake-now)))
                    ((symbol-function 'org-air-view--refresh-progressive-paint)
                     (lambda ()
                       (funcall paint-orig)
                       (push (length org-air-view--items) paints))))
            (let ((org-air-cold-paint-interval 0.5)
                  (org-air-refresh-slice-budget 0)  ; one file per slice
                  (noninteractive nil)              ; open the paint gates
                  (token org-air-view--refresh-token)
                  (n 40))
              (while (and (> n 0) (eq org-air-view--refresh-state 'refreshing))
                (org-air-view--refresh-run-slice (current-buffer) token)
                (setq fake-now (+ fake-now 0.6))    ; the stubbed clock
                (cl-decf n))))
          (setq paints (nreverse paints))
          ;; the stream REPEATED (>=3) and grew strictly — an empty-file
          ;; slice repainting would repeat a count and break the <-chain;
          ;; the retired one-shot `--loading' gate would stop at 1.
          (should (>= (length paints) 3))
          (should (apply #'< paints))
          (should (<= (length paints) 7))   ; only item-bearing slices paint
          ;; verbs went live at the first paint; the finish ended the stream.
          (should-not org-air-view--loading)
          (should-not org-air-view--refresh-state)
          (should-not org-air-view--refresh-progressive)
          (should (= (length org-air-view--items) 7)))))))

;;;; -------------------------------------------------------------------
;;;; R56-3 — cache-stale open paints the cached board first (P1a)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r56-3-cache-stale-paints-cached-board-first ()
  "The cache-STALE open shows the FULL cached board before the scan ends.
Valid cache + one touched file; the sanctioned R45-2 seam runs the STALE
deferred one-shot (`org-air-view--deferred-stale-paint') directly: the
first content render carries the CACHED items — ZERO scans inside the
call — and the one-shot then OWNS the `--refresh-start t' kickoff, so
the board sits painted while `--refresh-state' is `refreshing' with a
non-empty queue (the paint LAW: never chrome in place of held content).
The P3b banner tick then surfaces `scanning N/M' in-place on line 1 of
that painted board.  Reverting P1a (skeleton-until-finish, command-body
kickoff) fails."
  (skip-unless (locate-library "org-air"))
  (org-air-r26--with-cache-env
    (with-current-buffer (org-air-r26--cache-board)
      (org-air-r26--scan-and-cache))
    (kill-buffer org-air-view-buffer-name)
    (org-air-r26--kill-file-buffers org-air-test--dir)
    ;; touch the inbox: new capture + a decisive mtime bump -> STALE.
    (write-region "* TODO Deferred stale probe\n" nil
                  org-air-inbox-file 'append)
    (set-file-times org-air-inbox-file (time-add (current-time) 5))
    (with-current-buffer (org-air-r26--cache-board)
      ;; mirror the dispatcher's STALE arm: cache into the buffer-locals,
      ;; token bumped via cancel, then the deferred one-shot (driven
      ;; directly — batch never arms the idle timer).
      (let ((cache (org-air-view--cache-load)))
        (should cache)
        (should (cdr cache))                ; stale files present
        (setq org-air-view--items (car cache)
              org-air-view--items-key (list org-air-files
                                            org-air-inbox-file)
              org-air-view--cache-stale-files (cdr cache))
        (org-air-view--refresh-cancel)
        (let ((scans 0))
          (cl-letf* ((inf-orig (symbol-function 'org-air-query-items-in-files))
                     ((symbol-function 'org-air-query-items-in-files)
                      (lambda (&rest args)
                        (cl-incf scans)
                        (apply inf-orig args)))
                     (items-orig (symbol-function 'org-air-query-items))
                     ((symbol-function 'org-air-query-items)
                      (lambda (&rest args)
                        (cl-incf scans)
                        (apply items-orig args))))
            (org-air-view--deferred-stale-paint (current-buffer)
                                                org-air-view--refresh-token))
          ;; ZERO scans: the cached paint + paced kickoff ran scan-free.
          (should (= scans 0)))
        ;; content on screen, machine live behind it, queue non-empty.
        (let ((text (substring-no-properties (buffer-string))))
          (should-not (string-match-p "Loading your board" text))
          (should (string-match-p "Email finance about budget" text))
          (should-not (string-match-p "Deferred stale probe" text)))
        (should (eq org-air-view--refresh-state 'refreshing))
        (should org-air-view--refresh-queue)
        ;; a painted board never streams: the R26-8 single-swap rule.
        (should-not org-air-view--refresh-progressive)
        ;; P3b: the banner-line tick rewrites line 1 in place with the
        ;; live numbers (interactive-only; tick time nil = immediate).
        (let ((noninteractive nil))
          (org-air-view--refresh-banner-tick))
        (should (string-match-p
                 "scanning [0-9]+/[0-9]+"
                 (buffer-substring-no-properties
                  (point-min)
                  (save-excursion (goto-char (point-min))
                                  (line-end-position)))))
        ;; slices to completion: the probe lands, the segment clears.
        (org-air-r26--run-slices)
        (should-not org-air-view--refresh-state)
        (let ((text (substring-no-properties (buffer-string))))
          (should (string-match-p "Deferred stale probe" text))
          (should-not (string-match-p "scanning [0-9]+/[0-9]+" text)))))))

;;;; -------------------------------------------------------------------
;;;; R56-3b — the dispatcher's STALE arm owns the deferred paint (P1a)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r56-3b-dispatcher-stale-arm-defers-paint ()
  "The REAL `org-air-view' STALE arm defers to the stale one-shot (P1a).
R56-3 drives `org-air-view--deferred-stale-paint' DIRECTLY, so it alone
cannot fence the dispatcher WIRING: reverting only the STALE arm back to
the pre-R56 command-body `(org-air-view--refresh-start t)' kickoff
\(skeleton-until-finish — the user's measured minutes-long \"Loading
your board…\") would pass it and ship the defect green.  This fence
drives the command itself through the R45-2 interactive scaffolding
\(stubbed `run-with-idle-timer' capture; no real timer fires) over a
valid STALE cache and pins the wiring: (a) the captured one-shot IS
`org-air-view--deferred-stale-paint'; (b) the command body never enters
`refreshing' — no command-body kickoff, no chain timer; (c) fired by
hand, the one-shot renders the cached board with the machine still idle
and only THEN starts the paced rescan (render-BEFORE-refreshing order),
leaving a live queue + armed chain behind the painted board.  Reverting
the dispatcher's STALE arm fails (a) and (b)."
  (skip-unless (locate-library "org-air"))
  (org-air-r26--with-cache-env
    ;; prime a valid cache, then make it STALE: fresh session + a touched
    ;; inbox (new capture + a decisive mtime bump) — the R56-3 recipe.
    (with-current-buffer (org-air-r26--cache-board)
      (org-air-r26--scan-and-cache))
    (kill-buffer org-air-view-buffer-name)
    (org-air-r26--kill-file-buffers org-air-test--dir)
    (write-region "* TODO Dispatcher stale probe\n" nil
                  org-air-inbox-file 'append)
    (set-file-times org-air-inbox-file (time-add (current-time) 5))
    (org-air-r45--driving-interactive
      (org-air-view)
      ;; the STALE branch really ran: cached items seeded, stale files on
      ;; record, the skeleton the ONLY paint on the launch path.
      (with-current-buffer org-air-view-buffer-name
        (should org-air-view--items)
        (should org-air-view--cache-stale-files)
        ;; (b) the command body did NOT enter `refreshing' — the paced
        ;; kickoff belongs to the one-shot, never the command body (the
        ;; pre-R56 arm entered it right here, skeleton-until-finish).
        (should-not org-air-view--refresh-state)
        (should-not org-air-view--refresh-queue)
        (should-not (timerp org-air-view--refresh-timer)))
      (should (equal org-air-r45--calls '(loading)))
      ;; (a) the captured one-shot IS the stale paint fn.
      (ert-info ((format "idle=%S" org-air-r45--idle))
        (should org-air-r45--idle)
        (should (eq (car org-air-r45--idle)
                    'org-air-view--deferred-stale-paint)))
      ;; (c) fire it by hand, recording the machine state AT the full
      ;; render: the cached board paints BEFORE the machine goes live.
      (let ((render-state 'never-rendered))
        (cl-letf (((symbol-function 'org-air-view--render)
                   (lambda (&rest _)
                     (setq render-state org-air-view--refresh-state)
                     (push 'render org-air-r45--calls))))
          (org-air-r45--fire-one-shot))
        ;; rendered (never-rendered is non-nil), machine idle at render.
        (should-not render-state))
      (should (equal (reverse org-air-r45--calls) '(loading render)))
      ;; …and only THEN the paced kickoff: live machine, non-empty queue,
      ;; adaptive chain + watchdog armed behind the painted board.
      (with-current-buffer org-air-view-buffer-name
        (should (eq org-air-view--refresh-state 'refreshing))
        (should org-air-view--refresh-queue)
        (should-not org-air-view--refresh-progressive)
        (should (org-air-view--refresh-chain-live-p))
        (should (timerp org-air-view--refresh-watchdog))
        ;; drive to the finish: converges, every timer down.
        (org-air-r26--run-slices)
        (should-not org-air-view--refresh-state)
        (should-not (timerp org-air-view--refresh-timer))
        (should-not (timerp org-air-view--refresh-watchdog))))))

;;;; -------------------------------------------------------------------
;;;; R56-4 — the adaptive self-chaining pacer (P2a)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r56-4-adaptive-pacer-gap-and-convergence ()
  "The adaptive chain gap is pure/monotone/bounded; the chain keeps ONE
one-shot; an uninterrupted drain spends far less wall clock than the
retired 0.2s duty-cycle pacer; a real queue converges in bounded slices.
Reverting P2a (constant 0.2s cadence or idle pacing) fails."
  (skip-unless (locate-library "org-air"))
  ;; (1) PURE gap table (spec seam 5): fast when uninterrupted…
  (should (= (org-air-view--refresh-next-gap 'completed nil)
             org-air-view--refresh-gap-fast))
  (should (= (org-air-view--refresh-next-gap 'completed 0.6)
             org-air-view--refresh-gap-fast))
  ;; …doubling backoff to the cap across consecutive aborts, reset after.
  (let ((gap nil) (seen nil))
    (dotimes (_ 4)
      (setq gap (org-air-view--refresh-next-gap 'aborted gap))
      (push gap seen))
    (should (equal (nreverse seen) '(0.15 0.3 0.6 0.6)))
    (should (= (org-air-view--refresh-next-gap 'completed gap)
               org-air-view--refresh-gap-fast)))
  ;; bounded BOTH ways at every point of a long mixed chain (the R34-3
  ;; anti-strand law carries over: the gap never grows with the index).
  (let ((gap nil))
    (dotimes (k 500)
      (setq gap (org-air-view--refresh-next-gap
                 (if (zerop (mod k 7)) 'aborted 'completed) gap))
      (should (>= gap org-air-view--refresh-gap-fast))
      (should (<= gap org-air-view--refresh-gap-cap))))
  ;; (2) the wall-clock model: the same 198-slice fill (the measured 1801
  ;; -file corpus) spends FAR less clock in gaps than the retired 0.2s
  ;; duty-cycle pacer — seconds, not minutes.
  (let* ((slices 198)
         (adaptive (* slices org-air-view--refresh-gap-fast))
         (retired (* slices org-air-view--refresh-wallclock-pace)))
    (should (< adaptive (/ retired 5.0))))
  ;; (3) EXECUTING: the chain records its gap and keeps EXACTLY ONE
  ;; pending one-shot through completed/aborted/recovery transitions.
  (with-temp-buffer
    (org-air-view-mode)
    (let ((noninteractive nil))
      (unwind-protect
          (let ((expect (list org-air-view--refresh-gap-fast
                              0.15 0.3 0.6 0.6
                              org-air-view--refresh-gap-fast))
                (outcomes '(completed aborted aborted aborted aborted
                            completed))
                (prev nil))
            (setq org-air-view--refresh-gap nil)
            (cl-mapc
             (lambda (outcome want)
               (org-air-view--refresh-chain-arm (current-buffer)
                                                org-air-view--refresh-token
                                                outcome)
               (should (= org-air-view--refresh-gap want))
               (should (org-air-view--refresh-chain-live-p))
               (should-not (timer--repeat-delay org-air-view--refresh-timer))
               (should (= 1 (cl-count org-air-view--refresh-timer timer-list)))
               (when prev (should-not (memq prev timer-list)))
               (setq prev org-air-view--refresh-timer))
             outcomes expect))
        (org-air-view--refresh-disarm))))
  ;; (4) CONVERGENCE is never indefinite: a real 30-file queue drains to
  ;; the terminal single-swap in a bounded number of budgeted slices.
  (let ((specs (list '("inbox.org" . "* TODO Inbox capture :inbox:\n"))))
    (dotimes (i 29)
      (push (cons (format "f%02d.org" i) (format "* TODO Item %02d\n" i))
            specs))
    (org-air-r53--with-corpus specs
      (org-air-r53--with-board
        (setq org-air-view--items nil
              org-air-view--items-mtimes nil)
        (org-air-view--refresh-start t)
        (should (eq org-air-view--refresh-state 'refreshing))
        (let ((token org-air-view--refresh-token)
              (slices 0))
          (while (and (< slices 40) (eq org-air-view--refresh-state
                                        'refreshing))
            (org-air-view--refresh-run-slice (current-buffer) token)
            (cl-incf slices))
          ;; converged — and in at most one slice per queued file.
          (should-not org-air-view--refresh-state)
          (should (<= slices 30)))
        (should (= (length org-air-view--items) 30))))))

;;;; -------------------------------------------------------------------
;;;; R56-4b — the slice runner ITSELF re-arms the chain (P2a wiring)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r56-4b-slice-runner-rearms-the-chain ()
  "The slice runner re-arms the adaptive chain with the slice's outcome.
R56-4 part (3) proves `org-air-view--refresh-chain-arm' in ISOLATION;
nothing there proves the run-slice bottom CALLS it — deleting the re-arm
hunk leaves every direct-drive ERT green (the gate reads the chain
HANDLE, nil in every handle-less direct drive) while an interactive fill
degrades to ~1 slice per 8s watchdog period ≈ tens of minutes at the
measured 1801-file corpus: the exact defect this round kills.  Fence:
under a seeded DUMMY chain handle (interactively a handle always exists
while `refreshing') with the interactive gate open and the armer spied
\(spy ONLY — no real arm, no timer spawned): a clean slice with a
non-empty queue re-arms with `completed'; an input-aborted slice re-arms
with `aborted' (the backoff outcome); the FINAL queue-emptying slice
does NOT re-arm — the finish disarms the handle instead.  DELETING the
run-slice re-arm hunk fails."
  (skip-unless (locate-library "org-air"))
  (let ((specs (list '("inbox.org" . "* TODO Inbox capture :inbox:\n"))))
    (dotimes (i 3)
      (push (cons (format "f%02d.org" i) (format "* TODO Item %02d\n" i))
            specs))
    (org-air-r53--with-corpus specs
      (org-air-r53--with-board
        (setq org-air-view--items nil
              org-air-view--items-mtimes nil)
        (org-air-view--refresh-start t)     ; real batch start: no timer
        (should (eq org-air-view--refresh-state 'refreshing))
        (should (= org-air-view--refresh-total 4))
        (should-not org-air-view--refresh-timer)   ; why the seed below
        (let ((arms nil)
              (token org-air-view--refresh-token))
          (unwind-protect
              (cl-letf (((symbol-function 'org-air-view--refresh-chain-arm)
                         (lambda (_buffer _token outcome)
                           (push outcome arms))))   ; spy — never arms
                (let ((noninteractive nil)          ; the re-arm gate…
                      (org-air-refresh-slice-budget 0)) ; one file/slice
                  ;; …needs a live chain HANDLE: seed a dummy one-shot.
                  (setq org-air-view--refresh-timer
                        (run-with-timer 1000 nil #'ignore))
                  ;; (1) a clean slice, queue still non-empty -> 'completed.
                  (org-air-view--refresh-run-slice (current-buffer) token)
                  (should (eq org-air-view--refresh-state 'refreshing))
                  (should (equal arms '(completed)))
                  ;; (2) an input-aborted slice -> 'aborted (backoff).
                  (let ((unread-command-events (list ?g)))
                    (org-air-view--refresh-run-slice (current-buffer)
                                                     token))
                  (should (eq org-air-view--refresh-state 'refreshing))
                  (should (equal arms '(aborted completed)))
                  ;; recovery slices keep the chain 'completed…
                  (org-air-view--refresh-run-slice (current-buffer) token)
                  (org-air-view--refresh-run-slice (current-buffer) token)
                  (should (equal arms '(completed completed
                                        aborted completed)))
                  (should (= (length org-air-view--refresh-queue) 1))
                  ;; (3) …and the FINAL queue-emptying slice does NOT
                  ;; re-arm: the finish disarms the dummy handle instead.
                  (org-air-view--refresh-run-slice (current-buffer) token)
                  (should-not org-air-view--refresh-state)
                  (should (equal arms '(completed completed
                                        aborted completed)))
                  (should-not org-air-view--refresh-timer)
                  (should (= (length org-air-view--items) 4))))
            (when (timerp org-air-view--refresh-timer)
              (cancel-timer org-air-view--refresh-timer)
              (setq org-air-view--refresh-timer nil))))))))

;;;; -------------------------------------------------------------------
;;;; R56-5 — the visible progress segment (P3a/P3b/P3c)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r56-5-progress-segment-truth ()
  "The banner shows `scanning N/M…' whenever refreshing with a queue — on
the skeleton AND on a painted board (independent of `--loading'), with N
GROWING as slices land, wearing the salient `org-air-face-progress' —
and it clears crisply the moment the machine is idle.  The retired
`loading N/M files' / `stale ∙ refreshing' strings never render.
Reverting P3a/P3c fails."
  (skip-unless (locate-library "org-air"))
  (let ((specs (list '("inbox.org" . "* TODO Progress capture :inbox:\n"))))
    (dotimes (i 5)
      (push (cons (format "f%02d.org" i) (format "* TODO Item %02d\n" i))
            specs))
    (org-air-r53--with-corpus specs
      (org-air-r53--with-board
        (setq org-air-view--items nil
              org-air-view--items-mtimes nil
              org-air-view--loading t)     ; the cold open's guard, up
        (org-air-view--refresh-start t)
        (should (eq org-air-view--refresh-state 'refreshing))
        (should (= org-air-view--refresh-total 6))
        ;; (1) the SKELETON carries the numbers: banner segment + the
        ;; centred body line; the retired strings are absent.
        (org-air-view--render-loading)
        (let ((text (substring-no-properties (buffer-string))))
          (should (string-match-p "scanning 0/6…" text))
          (should (string-match-p "(scanning 0/6)" text))
          (should-not (string-match-p "loading [0-9]+/[0-9]+" text))
          (should-not (string-match-p "stale ∙ refreshing" text)))
        ;; (2) N GROWS from the machine's own queue/total as slices land.
        (let ((org-air-refresh-slice-budget 0)
              (token org-air-view--refresh-token))
          (org-air-view--refresh-run-slice (current-buffer) token)
          (should (string-match-p "scanning 1/6…"
                                  (org-air-view--refresh-progress-string)))
          (org-air-view--refresh-run-slice (current-buffer) token)
          (should (string-match-p "scanning 2/6…"
                                  (org-air-view--refresh-progress-string))))
        ;; (3) independence from `--loading': a PAINTED board mid-refresh
        ;; (loading long cleared) still shows the segment — salient-faced,
        ;; never the faded chrome face.
        (setq org-air-view--items (copy-sequence org-air-view--refresh-acc)
              org-air-view--classify-cache nil
              org-air-view--loading nil)
        (org-air-view--render org-air-view--items nil)
        (let ((text (buffer-string)))
          (should (string-match "scanning 2/6…" text))
          (should (eq (get-text-property (match-beginning 0) 'face text)
                      'org-air-face-progress))
          (should-not (string-match-p "loading [0-9]+/[0-9]+" text))
          (should-not (string-match-p "stale ∙ refreshing" text)))
        ;; (4) crisp clear: the finish swap repaints with the machine
        ;; idle — the segment vanishes in the same motion, the plain item
        ;; count returns.
        (let ((token org-air-view--refresh-token) (n 20))
          (while (and (> n 0) (eq org-air-view--refresh-state 'refreshing))
            (org-air-view--refresh-run-slice (current-buffer) token)
            (cl-decf n)))
        (should-not org-air-view--refresh-state)
        (let ((text (substring-no-properties (buffer-string))))
          (should-not (string-match-p "scanning" text))
          (should (string-match-p "[0-9]+ items" text)))))))

;;;; -------------------------------------------------------------------
;;;; R56-6 — never-hang preserved (P2b, the R53 laws)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r56-6-never-hang-abort-and-watchdog ()
  "C-g abortability + the watchdog-never-syncs-a-big-queue law survive the
R56 pacer: pending input aborts a slice with queue/accumulator/state
untouched; the watchdog fire on a >budget queue runs ZERO scans on the
main thread and leaves the queue intact — interactively it re-arms the
adaptive ONE-SHOT chain (never the retired repeating pacer, never a sync
drain); the queue then converges by pacing."
  (skip-unless (locate-library "org-air"))
  (let ((specs (list '("inbox.org" . "* TODO Inbox capture :inbox:\n"))))
    (dotimes (i 20)
      (push (cons (format "f%02d.org" i) (format "* TODO Item %02d\n" i))
            specs))
    (org-air-r53--with-corpus specs
      (org-air-r53--with-board
        (setq org-air-view--items nil
              org-air-view--items-mtimes nil)
        (org-air-view--refresh-start t)
        (should (eq org-air-view--refresh-state 'refreshing))
        (should (> (length org-air-view--refresh-queue)
                   org-air-view--refresh-sync-budget))
        ;; (1) C-g abortable: queue/accumulator/state all untouched.
        (let ((acc-before org-air-view--refresh-acc)
              (queue-before org-air-view--refresh-queue))
          (let ((unread-command-events (list ?g)))
            (org-air-view--refresh-run-slice (current-buffer)
                                             org-air-view--refresh-token))
          (should (eq org-air-view--refresh-state 'refreshing))
          (should (equal org-air-view--refresh-queue queue-before))
          (should (eq org-air-view--refresh-acc acc-before))
          ;; (2) the watchdog NEVER force-scans the >budget queue on the
          ;; main thread: zero scans in the fire, queue intact.
          (org-air-r53--counting
            (org-air-view--refresh-watchdog-fire (current-buffer)
                                                 org-air-view--refresh-token)
            (should (= org-air-r53--scan-calls 0)))
          (should (eq org-air-view--refresh-state 'refreshing))
          (should (equal org-air-view--refresh-queue queue-before)))
        ;; (3) interactively the fire re-arms the ONE-SHOT chain + a fresh
        ;; watchdog (assert the arming; timers never fire in batch).
        (let ((noninteractive nil))
          (org-air-view--refresh-watchdog-fire (current-buffer)
                                               org-air-view--refresh-token))
        (should (org-air-view--refresh-chain-live-p))
        (should-not (timer--repeat-delay org-air-view--refresh-timer))
        (should (timerp org-air-view--refresh-watchdog))
        (org-air-view--refresh-disarm)
        (should (eq org-air-view--refresh-state 'refreshing))
        ;; (4) convergence by pacing to the terminal single-swap.
        (let ((token org-air-view--refresh-token) (n 40))
          (while (and (> n 0) (eq org-air-view--refresh-state 'refreshing))
            (org-air-view--refresh-run-slice (current-buffer) token)
            (cl-decf n)))
        (should-not org-air-view--refresh-state)
        (should (= (length org-air-view--items) 21))))))

;;;; -------------------------------------------------------------------
;;;; R56-7 — warm refreshes keep the single-swap law (P1b mode, seam 9)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r56-7-warm-refresh-stays-single-swap ()
  "A PAINTED board + a large changed set through the paced path: stream
mode never engages (`--refresh-progressive' nil), ZERO progressive
paints, exactly ONE content repaint — the finish swap (R26-8).
A LOCK (spec seam 9), not a revert-fence: pre-R56 warm refreshes never
streamed either; this fences the P1b stream mode from ever leaking into
warm refreshes — with the paint gates OPEN (interactive semantics), so
only the mode condition holds the law.  Breaking that condition
\(always-stream) fails."
  (skip-unless (locate-library "org-air"))
  (let ((specs (list '("inbox.org" . "* TODO Inbox capture :inbox:\n"))))
    (dotimes (i 15)
      (push (cons (format "f%02d.org" i) (format "* TODO Item %02d\n" i))
            specs))
    (org-air-r53--with-corpus specs
      (org-air-r53--with-board
        ;; a WARM painted board: full fill + paint, machine idle.
        (setq org-air-view--items nil
              org-air-view--items-mtimes nil)
        (org-air-view--refresh-start t)
        (let ((token org-air-view--refresh-token) (n 40))
          (while (and (> n 0) (eq org-air-view--refresh-state 'refreshing))
            (org-air-view--refresh-run-slice (current-buffer) token)
            (cl-decf n)))
        (should-not org-air-view--refresh-state)
        (should (= (length org-air-view--items) 16))
        (org-air-view--render org-air-view--items nil)
        ;; bulk change: every file touched -> a >budget paced refresh.
        (dolist (f (org-air-query-files))
          (set-file-times f (time-add (current-time) 5)))
        (org-air-view--refresh-start)
        (should (eq org-air-view--refresh-state 'refreshing))
        (should (> (length org-air-view--refresh-queue)
                   org-air-view--refresh-sync-budget))
        ;; the painted board did NOT enter stream mode.
        (should-not org-air-view--refresh-progressive)
        (let ((paints 0)
              (repaints 0)
              (paint-orig
               (symbol-function 'org-air-view--refresh-progressive-paint))
              (repaint-orig
               (symbol-function 'org-air-view--refresh-repaint)))
          (cl-letf (((symbol-function 'org-air-view--refresh-progressive-paint)
                     (lambda ()
                       (cl-incf paints)
                       (funcall paint-orig)))
                    ((symbol-function 'org-air-view--refresh-repaint)
                     (lambda ()
                       (cl-incf repaints)
                       (funcall repaint-orig))))
            ;; paint gates OPEN (interactive semantics) — the mode
            ;; condition, not batch gating, is what must hold the law.
            (let ((org-air-refresh-slice-budget 0)
                  (noninteractive nil)
                  (token org-air-view--refresh-token)
                  (n 40))
              (while (and (> n 0) (eq org-air-view--refresh-state
                                      'refreshing))
                (org-air-view--refresh-run-slice (current-buffer) token)
                (cl-decf n))))
          (should-not org-air-view--refresh-state)
          (should (= paints 0))       ; stream never engaged
          (should (= repaints 1)))    ; exactly the finish swap
        (should (= (length org-air-view--items) 16))))))

(provide 'org-air-round56-test)
;;; org-air-round56-test.el ends here
