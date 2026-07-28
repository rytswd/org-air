;;; org-air-round42-test.el --- executing ERTs for v0.5 round-42 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.5 round-42 (air/v0.5/org-air-round42-design.org):
;; the mtime-incremental refresh that collapses the ~850ms idle-pacer floor.
;;
;; These are the DECISIVE perf/correctness fences for Fix A/B/C.  All run
;; deterministically under `noninteractive' — they call the refresh helpers
;; directly (no timers, no idle waits): the pacer never arms under batch, so
;; the paced path is driven either by the named slice runner in a loop or by
;; firing the watchdog by hand.  org-ql scans are counted at the SCAN ENTRY
;; POINTS — `org-air-query-items-in-files' (the incremental/sync/paced
;; reparse) and `org-air-query-items' (a full re-scan) — because `org-ql-
;; select' is a MACRO expanded into the byte-compiled callers, so a live
;; call count only exists at these named functions; the changed-file subset
;; is captured from the FILES argument of the former.
;;
;;   R42-2 NO-CHANGE short-circuit — a warm board + matching mtime snapshot
;;         refreshes with ZERO org-ql scans, single-swaps the same items
;;         (`eq'), and returns state to nil (never `refreshing).  Reverting
;;         Fix B (re-scan all files) drives the scan counter > 0 => FAILS.
;;   R42-2 INCREMENTAL — one changed file => ONLY that file is (re)queried,
;;         the other files' items are reused verbatim (`eq'), and the merged
;;         set equals the full set.  Reverting (re-scan all) loses the `eq'
;;         retention => FAILS.
;;   R42-2 SYNC FAST PATH — a changed set <= the budget (12) scans
;;         synchronously (state nil at return, no queue, no marker); a set >
;;         budget routes to the paced machine (state `refreshing, queue
;;         non-empty).  Reverting the sync path (always-pace) leaves a
;;         <=budget change `refreshing => FAILS.
;;   R42-2 WATCHDOG / no-strand — the paced path's `refreshing state ALWAYS
;;         resolves to nil (finish) or `failed; the wall-clock watchdog
;;         force-completes a stranded scan.  Reverting (a no-op watchdog)
;;         leaves the state stuck at `refreshing => FAILS.
;;   R42-1 `org-air-view--changed-files' pure table — new / changed /
;;         vanished / all-match(nil).
;;   R42   Timing sanity — a warm no-change refresh finishes well under a
;;         generous wall-clock bound.
;;
;; R42.1 REVIEW FENCES (the three Fable asked for; each REVERT-FAILS):
;;   B1  RETAINED-file coherence — force the PACED path, then EXTERNALLY
;;       write-region a RETAINED (unchanged-at-start) file BETWEEN
;;       `--refresh-start' and draining the slice queue (the git-pull /
;;       sync-daemon case: no after-save hook).  After finish, the NEXT
;;       refresh's `org-air-view--changed-files' MUST name that file — its
;;       baseline mtime is the PRE-write (scan-time) value, so the divergence
;;       is visible.  Reverting B1 (a finish-time full re-stat) stamps the
;;       POST-write mtime over the OLD items => masked FRESH forever => FAILS.
;;   F2  no-change REPAINT — a no-change `g r' with NO marker up still
;;       repaints (`buffer-chars-modified-tick' advances AND the day-keyed
;;       classify cache rebuilds to today's day).  Reverting to the
;;       `had-marker' skip strands yesterday's bucketing => FAILS.
;;   F3  sync fast-path is FAIL-SAFE — a scan error on the SYNC path (with
;;       the board already `refreshing' + `loading', the mid-load strand
;;       route) resolves to `failed' with the board intact, NEVER stuck at
;;       `refreshing'.  Reverting (drop the condition-case) lets the signal
;;       propagate and the state stays `refreshing' => FAILS.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-air-test-helpers)

(when (locate-library "org-air")
  (require 'org-air))

;;;; -------------------------------------------------------------------
;;;; Fixtures / warm-board scaffolding
;;;; -------------------------------------------------------------------

(defvar org-air-r42--ql-calls 0
  "Count of org-ql SCAN calls while a counter is installed.
Every call to a scan entry point (`org-air-query-items-in-files' or the
full `org-air-query-items') = one real org-ql pass (`org-ql-select' is a
macro, so this is the only place a live count exists).")

(defvar org-air-r42--in-files-args nil
  "List of FILES arguments passed to `org-air-query-items-in-files'.")

(defmacro org-air-r42--counting (&rest body)
  "Run BODY with the org-ql scan entry points counted.
Resets and exposes `org-air-r42--ql-calls' (number of real org-ql scans:
calls to `org-air-query-items-in-files' + `org-air-query-items') and
`org-air-r42--in-files-args' (the FILES lists the incremental/sync paths
asked to reparse)."
  (declare (indent 0) (debug t))
  `(let ((org-air-r42--ql-calls 0)
         (org-air-r42--in-files-args nil))
     (cl-letf* ((inf-orig (symbol-function 'org-air-query-items-in-files))
                ((symbol-function 'org-air-query-items-in-files)
                 (lambda (files &rest rest)
                   (cl-incf org-air-r42--ql-calls)
                   (push (copy-sequence files) org-air-r42--in-files-args)
                   (apply inf-orig files rest)))
                (items-orig (symbol-function 'org-air-query-items))
                ((symbol-function 'org-air-query-items)
                 (lambda (&rest rest)
                   (cl-incf org-air-r42--ql-calls)
                   (apply items-orig rest))))
       ,@body)))

(defmacro org-air-r42--with-warm-board (&rest body)
  "Fixtures + a WARM, fully-scanned board buffer as the current buffer.
Binds a scratch fixture set, opens a board buffer in `org-air-view-mode',
runs ONE full synchronous scan and records the completed-scan mtime
baseline (`org-air-view--items-mtimes') exactly as the live sync path
does, then paints.  BODY runs warm — a subsequent `org-air-view--refresh-
start' is the real `g r'."
  (declare (indent 0) (debug t))
  `(org-air-test-with-fixtures
    (let ((org-air-view-width 120)
          (org-air-view-height 50)
          (org-air-view-buffer-name "*org-air-r42*")
          (org-air-cache-file
           (expand-file-name "cache/board-r42.eld" org-air-test--dir)))
      (unwind-protect
          (with-current-buffer (get-buffer-create org-air-view-buffer-name)
            (unless (derived-mode-p 'org-air-view-mode) (org-air-view-mode))
            (let ((files (org-air-query-files)))
              (setq org-air-view--items (org-air-query-items)
                    org-air-view--items-key (list org-air-files
                                                  org-air-inbox-file)
                    org-air-view--classify-cache nil
                    org-air-view--items-mtimes
                    (org-air-view--mtimes-snapshot files))
              (org-air-view--render org-air-view--items nil))
            ,@body)
        (when (get-buffer org-air-view-buffer-name)
          (let ((kill-buffer-query-functions nil))
            (kill-buffer org-air-view-buffer-name)))))))

(defun org-air-r42--titles (items)
  "Return the sorted list of ITEM titles (a set-comparable signature)."
  (sort (mapcar (lambda (it) (or (org-air-item-title it) "")) items)
        #'string<))

(defun org-air-r42--touch (file)
  "Bump FILE's mtime decisively forward so the snapshot diverges."
  (set-file-times file (time-add (current-time) 10)))

;;;; -------------------------------------------------------------------
;;;; 1. NO-CHANGE short-circuit — ZERO org-ql scans, state nil, items eq
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r42-no-change-zero-scans ()
  "Warm board + matching mtime snapshot: `g r' with nothing changed does
ZERO org-ql scans, single-swaps the SAME items (`eq'), leaves the queue
empty and returns state to nil (NOT `refreshing).  Revert-fails: the R41
all-files re-queue would call org-ql (scan count > 0)."
  (skip-unless (locate-library "org-air"))
  (org-air-r42--with-warm-board
    (let ((before org-air-view--items))
      (org-air-r42--counting
        (org-air-view--refresh-start)
        ;; the decisive fence (counters are dynamic in the block): NOT ONE
        ;; org-ql scan, and nothing was queued.
        (should (= org-air-r42--ql-calls 0))
        (should (null org-air-r42--in-files-args)))
      ;; synchronous, no marker: state clears, no paced machine.
      (should-not org-air-view--refresh-state)
      (should (null org-air-view--refresh-queue))
      (should-not org-air-view--refresh-timer)
      ;; same items, reused verbatim (single-swap, no new structs).
      (should (eq org-air-view--items before))
      ;; the baseline is refreshed so the NEXT `g r' is still incremental.
      (should org-air-view--items-mtimes))))

;;;; -------------------------------------------------------------------
;;;; 2. INCREMENTAL — one changed file, only that file reparsed, rest eq
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r42-incremental-one-file ()
  "One file's mtime diverges: `org-air-view--changed-files' names exactly
it; the refresh reparses ONLY that file (a SINGLE org-ql scan over the
one-file subset, never the other four); the unchanged files' items are
reused `eq' (never re-queried); and the merged set equals the full set.
Revert-fails: an all-files re-scan makes the scan count/argument the whole
file set, not the one changed file."
  (skip-unless (locate-library "org-air"))
  (org-air-r42--with-warm-board
    (let* ((files (org-air-query-files))
           (changed-file (car files))
           (before org-air-view--items)
           (before-titles (org-air-r42--titles before)))
      ;; exactly one file changed.
      (org-air-r42--touch changed-file)
      (should (equal (org-air-view--changed-files
                      files org-air-view--items-mtimes)
                     (list changed-file)))
      (org-air-r42--counting
        (org-air-view--refresh-start)
        ;; ONLY the one changed file was reparsed: a single scan, over it
        ;; alone (counters are dynamic — read them inside the block).
        (should (= org-air-r42--ql-calls 1))
        (should (equal org-air-r42--in-files-args
                       (list (list changed-file)))))
      ;; synchronous single-swap (1 <= budget): state clears.
      (should-not org-air-view--refresh-state)
      ;; every UNCHANGED file's items are reused verbatim (`eq') — they were
      ;; never re-queried, only the changed file was.  (The changed file's
      ;; items may legitimately be `eq' too when org-ql serves them from its
      ;; parse cache, so we do NOT assert their identity — the decisive
      ;; incremental fence is the scan count/argument above.)
      (dolist (it before)
        (unless (equal (org-air-item-file it) changed-file)
          (should (memq it org-air-view--items))))
      ;; merged set is complete: identical to the full set (no content
      ;; edit here, so the titles are preserved end-to-end).
      (should (equal (org-air-r42--titles org-air-view--items)
                     before-titles)))))

(ert-deftest org-air-r42-incremental-content-merge-complete ()
  "A REAL edit to one file (a new heading) is picked up incrementally and
the merged set equals a full re-scan — retained-plus-rescanned == full."
  (skip-unless (locate-library "org-air"))
  (org-air-r42--with-warm-board
    (let* ((files (org-air-query-files))
           (target (file-truename org-air-inbox-file)))
      ;; a REAL edit saved through the live Org buffer (the after-save flow):
      ;; org-ql keeps the file's buffer resident, so the change must land in
      ;; that buffer (a bare disk write would leave the resident parse stale).
      (with-current-buffer (find-file-noselect org-air-inbox-file)
        (goto-char (point-max))
        (insert "\n* TODO R42 incremental probe\n")
        (let ((save-silently t)) (save-buffer)))
      (org-air-r42--touch org-air-inbox-file)   ; decisive mtime divergence
      (should (member target
                      (org-air-view--changed-files
                       files org-air-view--items-mtimes)))
      (org-air-r42--counting
        (org-air-view--refresh-start)
        (should (= org-air-r42--ql-calls 1)))
      (should-not org-air-view--refresh-state)
      ;; the new heading is present…
      (should (org-air-test-find-item "R42 incremental probe"
                                      org-air-view--items))
      ;; …and the incremental merge == a fresh full scan (set-equal).
      (should (equal (org-air-r42--titles org-air-view--items)
                     (org-air-r42--titles (org-air-query-items)))))))

;;;; -------------------------------------------------------------------
;;;; 3. SYNC FAST PATH vs PACED — budget gate
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r42-sync-fast-path-under-budget ()
  "A changed set at/below the budget scans SYNCHRONOUSLY: no queue is left
paced, no timer armed, state nil at return.  Revert-fails: an always-pace
refresh would leave a <=budget change `refreshing with a non-empty queue."
  (skip-unless (locate-library "org-air"))
  (org-air-r42--with-warm-board
    (let ((changed-file (car (org-air-query-files))))
      (should (<= 1 org-air-view--refresh-sync-budget))
      (org-air-r42--touch changed-file)
      (org-air-view--refresh-start)
      ;; SYNC: finished before returning — no paced machine state at all.
      (should-not org-air-view--refresh-state)
      (should (null org-air-view--refresh-queue))
      (should-not org-air-view--refresh-timer)
      (should-not org-air-view--refresh-watchdog))))

(ert-deftest org-air-r42-over-budget-routes-to-paced ()
  "A changed set ABOVE the budget routes to the R34-3 paced machine: state
`refreshing, the changed subset queued (non-empty).  Forcing the budget to
0 makes a single changed file exceed it (the sync fast path is skipped)."
  (skip-unless (locate-library "org-air"))
  (org-air-r42--with-warm-board
    (let ((org-air-view--refresh-sync-budget 0)
          (changed-file (car (org-air-query-files))))
      (org-air-r42--touch changed-file)
      (org-air-view--refresh-start)
      ;; PACED: the machine is live, the changed file queued for slicing.
      (should (eq org-air-view--refresh-state 'refreshing))
      (should (member changed-file org-air-view--refresh-queue))
      (should (= (length org-air-view--refresh-queue) 1))
      ;; driving the slices to completion resolves it (single swap, nil).
      (let ((token org-air-view--refresh-token)
            (n 20))
        (while (and (> n 0) (eq org-air-view--refresh-state 'refreshing))
          (org-air-view--refresh-run-slice (current-buffer) token)
          (cl-decf n)))
      (should-not org-air-view--refresh-state))))

;;;; -------------------------------------------------------------------
;;;; 4. WATCHDOG / no-strand — `refreshing NEVER persists
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r42-watchdog-force-completes-strand ()
  "R53 P1c / R56 P2b (re-bless, spec §P2b / ERT seam 8): the watchdog NEVER
drains a queue ABOVE the sync budget synchronously — that force-complete
WAS the measured 4.5-minute mid-session freeze at 5000 files.  Above
budget it re-arms the R56 P2a adaptive ONE-SHOT wall-clock chain (a live
`timer-list' entry, `timer--repeat-delay' nil — the parallel 0.2s
REPEATING wall-clock fallback pacer is retired) and re-arms itself
behind it; the state legitimately STAYS `refreshing' and converges by
pacing, never by freezing.  A provably SMALL remainder (<= the budget)
still force-completes synchronously, so the R42-2 no-strand guarantee
survives.  Revert-fails: the old unconditional sync drain scans the
over-budget queue inside the fire (the counter > 0); the retired
fallback armed a REPEATING timer (repeat-delay non-nil)."
  (skip-unless (locate-library "org-air"))
  (org-air-r42--with-warm-board
    (let ((org-air-view--refresh-sync-budget 0)   ; queue(1) > budget(0)
          (changed-file (car (org-air-query-files))))
      (org-air-r42--touch changed-file)
      (org-air-view--refresh-start)
      (should (eq org-air-view--refresh-state 'refreshing))
      (let ((queue-before org-air-view--refresh-queue))
        (should queue-before)
        ;; (1) over-budget fire: NOT drained synchronously — zero scans
        ;; run inside the fire, the queue is untouched, the state stays
        ;; `refreshing' (it converges by pacing, below).
        (org-air-r42--counting
          (org-air-view--refresh-watchdog-fire (current-buffer)
                                               org-air-view--refresh-token)
          (should (= org-air-r42--ql-calls 0)))
        (should (eq org-air-view--refresh-state 'refreshing))
        (should (equal org-air-view--refresh-queue queue-before))
        ;; (2) the fallback driver: outside batch the fire re-arms the R56
        ;; P2a adaptive ONE-SHOT chain (a live pending `timer-list' entry
        ;; scheduling the SAME slice runner; wall-clock, not idle-gated;
        ;; NOT the retired 0.2s repeating pacer) + a fresh watchdog behind
        ;; it.  Timers never fire in batch — assert the arming, then
        ;; disarm for determinism.
        (let ((noninteractive nil))
          (org-air-view--refresh-watchdog-fire (current-buffer)
                                               org-air-view--refresh-token))
        (should (timerp org-air-view--refresh-timer))
        (should (org-air-view--refresh-chain-live-p))
        (should (memq org-air-view--refresh-timer timer-list))
        (should-not (memq org-air-view--refresh-timer timer-idle-list))
        (should-not (timer--repeat-delay org-air-view--refresh-timer))
        (should (timerp org-air-view--refresh-watchdog))
        (org-air-view--refresh-disarm)
        (should (eq org-air-view--refresh-state 'refreshing))
        ;; (3) convergence by PACING: the budgeted slices drain the queue
        ;; to the terminal single-swap — `refreshing' still never strands.
        (let ((token org-air-view--refresh-token) (n 30))
          (while (and (> n 0) (eq org-air-view--refresh-state 'refreshing))
            (org-air-view--refresh-run-slice (current-buffer) token)
            (cl-decf n)))
        (should-not org-air-view--refresh-state)
        (should org-air-view--items)))))

(ert-deftest org-air-r42-watchdog-fails-honestly ()
  "R53 P1c (re-bless, spec §P1c / ERT seam 4): the watchdog's synchronous
force-completion only runs for a PROVABLY SMALL remainder (<= the sync
budget).  Above the budget an erroring scan is never even reached — the
watchdog paces instead of scanning.  On the small-remainder sync branch a
scan error lands at `failed' (an honest terminal state) — NEVER stuck at
`refreshing'."
  (skip-unless (locate-library "org-air"))
  (org-air-r42--with-warm-board
    (let ((org-air-view--refresh-sync-budget 0)   ; route to the paced path
          (changed-file (car (org-air-query-files))))
      (org-air-r42--touch changed-file)
      (org-air-view--refresh-start)
      (should (eq org-air-view--refresh-state 'refreshing))
      (cl-letf (((symbol-function 'org-air-query-items-in-files)
                 (lambda (&rest _) (error "disk on fire"))))
        ;; over budget: the erroring scan is NEVER reached — the watchdog
        ;; paces, so the state stays `refreshing' with the queue intact.
        (org-air-view--refresh-watchdog-fire (current-buffer)
                                             org-air-view--refresh-token)
        (should (eq org-air-view--refresh-state 'refreshing))
        (should org-air-view--refresh-queue)
        ;; small remainder (queue(1) <= budget): the sync branch runs, the
        ;; scan signals, the state lands HONESTLY at `failed'.
        (let ((org-air-view--refresh-sync-budget
               (length org-air-view--refresh-queue)))
          (org-air-view--refresh-watchdog-fire (current-buffer)
                                               org-air-view--refresh-token)))
      (should (eq org-air-view--refresh-state 'failed))
      (should-not (eq org-air-view--refresh-state 'refreshing)))))

(ert-deftest org-air-r42-watchdog-superseded-token-noop ()
  "A watchdog carrying a stale TOKEN (a superseded refresh) never touches
the live state — it is a silent no-op, so it can never corrupt a fresh
refresh."
  (skip-unless (locate-library "org-air"))
  (org-air-r42--with-warm-board
    (let ((org-air-view--refresh-sync-budget 0)
          (changed-file (car (org-air-query-files))))
      (org-air-r42--touch changed-file)
      (org-air-view--refresh-start)
      (should (eq org-air-view--refresh-state 'refreshing))
      (let ((stale (1- org-air-view--refresh-token)))
        (org-air-view--refresh-watchdog-fire (current-buffer) stale)
        ;; the live refresh is untouched by the stale watchdog.
        (should (eq org-air-view--refresh-state 'refreshing))))))

;;;; -------------------------------------------------------------------
;;;; 5. `org-air-view--changed-files' — pure table
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r42-changed-files-table ()
  "The shared staleness oracle: all-match => nil; a changed mtime, a new
file (absent from snapshot), and a vanished snapshot file each surface."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (let* ((files (org-air-query-files))
           (snapshot (org-air-view--mtimes-snapshot files)))
      ;; all mtimes match => FRESH (no scan at all).
      (should (null (org-air-view--changed-files files snapshot)))
      ;; a changed mtime surfaces exactly its file.
      (let ((f (car files)))
        (org-air-r42--touch f)
        (should (equal (org-air-view--changed-files files snapshot)
                       (list f)))
        ;; refresh the snapshot for f so the next probes isolate cleanly.
        (setq snapshot (org-air-view--mtimes-snapshot files)))
      ;; a NEW file (present on disk, absent from the snapshot) surfaces.
      (let* ((partial (cdr snapshot))          ; drop the first file's entry
             (new-file (car (mapcar #'car snapshot)))
             (changed (org-air-view--changed-files files partial)))
        (should (member new-file changed))
        (should (= (length changed) 1)))
      ;; a VANISHED snapshot file (in the baseline, no longer configured)
      ;; surfaces so its stale rows are dropped.
      (let* ((ghost (expand-file-name "ghost.org" org-air-test--dir))
             (haunted (cons (cons ghost (current-time)) snapshot))
             (changed (org-air-view--changed-files files haunted)))
        (should (member ghost changed))))))

;;;; -------------------------------------------------------------------
;;;; 6. Timing sanity — warm no-change refresh is sub-frame-ish (generous)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r42-no-change-timing-bound ()
  "A warm no-change refresh completes well under a generous wall-clock
bound (no org-ql scan, no ~850ms pacer floor).  Deliberately generous so
it is not a CI/GUI-fragile micro-benchmark — it only guards against a
regression back to the paced all-files re-scan."
  (skip-unless (locate-library "org-air"))
  (org-air-r42--with-warm-board
    (let ((t0 (float-time)))
      (org-air-view--refresh-start)
      (should-not org-air-view--refresh-state)
      (should (< (- (float-time) t0) 0.5)))))

;;;; -------------------------------------------------------------------
;;;; 7. R42.1 B1 — RETAINED file written externally mid-paced-scan is NOT
;;;;    masked FRESH by a finish-time re-stat (THE decisive coherence fence)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r42-b1-retained-external-write-not-masked ()
  "Force the PACED path (budget 0, one changed file queued).  BETWEEN
`--refresh-start' and draining the slice queue, EXTERNALLY write a RETAINED
(unchanged-at-start, un-queued) file so its mtime diverges — the git-pull /
sync-daemon case (no after-save hook fires).  After finish the retained
file's baseline mtime must be its PRE-write (scan-time) value, so the NEXT
refresh's `org-air-view--changed-files' NAMES it and it re-scans.

Revert-fails: the pre-R42.1 finish RE-STAT'd every file at finish time,
stamping the retained file's POST-write mtime over its OLD (pre-write)
items — the next no-change short-circuit then calls it FRESH and the
staleness is masked FOREVER, so this `should' FAILS."
  (skip-unless (locate-library "org-air"))
  (org-air-r42--with-warm-board
    (let* ((files (org-air-query-files))
           (org-air-view--refresh-sync-budget 0)   ; force the PACED path
           (changed-file (car files))
           (retained-file (cadr files)))           ; unchanged AT START
      ;; exactly one file changed at start => the paced machine queues it and
      ;; retains everything else (retained-file among them).
      (org-air-r42--touch changed-file)
      (should-not (member retained-file
                          (org-air-view--changed-files
                           files org-air-view--items-mtimes)))
      (org-air-view--refresh-start)
      (should (eq org-air-view--refresh-state 'refreshing))
      (should (member changed-file org-air-view--refresh-queue))
      (should-not (member retained-file org-air-view--refresh-queue))
      ;; --- the paced-scan WINDOW: an EXTERNAL writer (git pull / sync
      ;; daemon) rewrites a RETAINED file on disk; no after-save hook, no
      ;; entry in the changed set, no slice will read it.
      (with-temp-buffer
        (insert "* TODO sneaky external write mid-paced-scan\n")
        (write-region (point-min) (point-max) retained-file nil 'silent))
      (org-air-r42--touch retained-file)          ; decisive mtime divergence
      ;; drain the queued slices to the terminal single-swap.
      (let ((token org-air-view--refresh-token) (n 30))
        (while (and (> n 0) (eq org-air-view--refresh-state 'refreshing))
          (org-air-view--refresh-run-slice (current-buffer) token)
          (cl-decf n)))
      (should-not org-air-view--refresh-state)     ; finished cleanly
      ;; THE FENCE: the retained file's external write survived the finish —
      ;; its baseline is the PRE-write mtime, so the next diff names it.
      (should (member retained-file
                      (org-air-view--changed-files
                       files org-air-view--items-mtimes)))
      ;; guard against tautology: the QUEUED changed file was read at scan
      ;; time and is legitimately FRESH now (not a blanket "everything
      ;; changed" that would satisfy the fence trivially).
      (should-not (member changed-file
                          (org-air-view--changed-files
                           files org-air-view--items-mtimes))))))

;;;; -------------------------------------------------------------------
;;;; 8. R42.1 F2 — no-change `g r' REPAINTS even with NO marker up
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r42-f2-no-change-repaints ()
  "A no-change `g r' with NO marker up (the common warm case) still REPAINTS:
`buffer-chars-modified-tick' advances AND the day-keyed classify cache,
left stamped with YESTERDAY's day, rebuilds to today's day on the render.
This is the documented \"single-swap the same items, clear the marker\"
(repaint) contract — without it, a day rolling over after midnight leaves
yesterday's overdue/today/upcoming bucketing on screen indefinitely (no
midnight timer; the classify cache invalidates only ON RENDER).

R72 Decision 4 re-bless: the memo key is now the pair
(DAY . EFFECTIVE-HORIZON) — the stale stamp uses the NEW shape with
yesterday's day, so the rebuild below is driven by the DAY rollover
alone (not a shape mismatch), and the post-render key is asserted to be
(TODAY . `org-air-upcoming-days') — the unfiltered board's effective
horizon is the knob.  The rollover/repaint contract itself is unchanged.

R93 re-bless (honest — no conjunct weakened): the key gained the two
aging-threshold knobs (`org-air-attention-days' and
`org-air-attention-default-days'), which are classify inputs that read
no file.  The stale stamp below uses the FULL current shape with
yesterday's day, so the rebuild is still driven by the DAY rollover
alone and the rollover contract is asserted exactly as before.

Revert-fails: the pre-R42.1 `had-marker' guard SKIPPED the repaint when no
`refreshing'/`failed'/`loading' marker was up — the tick would not advance
and the classify day would stay YESTERDAY, so both `should's FAIL."
  (skip-unless (locate-library "org-air"))
  (org-air-r42--with-warm-board
    ;; no marker: a plain warm board (the fixture leaves state/loading nil).
    (should-not org-air-view--refresh-state)
    (should-not org-air-view--loading)
    (let ((tick0 (buffer-chars-modified-tick))
          (yesterday (1- (time-to-days (current-time))))
          (today (time-to-days (current-time))))
      ;; stamp the classify cache with a STALE (yesterday) key, as a board
      ;; left open across midnight would be.  R72/R83: the key is the list
      ;; (DAY EFFECTIVE-HORIZON BACKLOG-TAG); stamp the new shape with
      ;; yesterday's day so ONLY the day is stale.
      (setq org-air-view--classify-cache-day
            (list yesterday (org-air-view--filter-effective-horizon)
                  org-air-backlog-tag org-air-attention-days
                  org-air-attention-default-days))
      (should org-air-view--classify-cache)      ; a table is present to rebuild
      (org-air-view--refresh-start)
      ;; no-change: synchronous, no paced machine, state stays nil.
      (should-not org-air-view--refresh-state)
      ;; …but it REPAINTED: the buffer's char tick advanced…
      (should (> (buffer-chars-modified-tick) tick0))
      ;; …and the render rebuilt the classify cache for TODAY (drops the
      ;; stale midnight bucketing).  R72/R83: the rebuilt key is the list
      ;; (TODAY HORIZON BACKLOG-TAG) with HORIZON = the knob horizon (the
      ;; board is unfiltered, so no window token widens it).
      (should (equal org-air-view--classify-cache-day
                     (list today org-air-upcoming-days
                           org-air-backlog-tag org-air-attention-days
                           org-air-attention-default-days))))))

;;;; -------------------------------------------------------------------
;;;; 9. R42.1 F3 — sync fast-path scan error => `failed', never stranded
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r42-f3-sync-scan-error-fails-not-strands ()
  "The SYNC fast path is fail-safe.  Model the residual strand route: the
board is already `refreshing' + `loading' (a small cold load mid-flight),
the user presses `g', the all-changed set fits the budget so the SYNC
branch runs — and the scan SIGNALS.  It must resolve to `failed' with the
painted board intact (`--items' untouched), NEVER left stuck at
`refreshing' with no timer and no watchdog.

Revert-fails: without the condition-case the signal propagates out of
`--refresh-start' with state untouched, so it stays `refreshing' — the
exact \"refreshing FOREVER\" strand this round exists to kill — and the
`failed' assertion FAILS."
  (skip-unless (locate-library "org-air"))
  (org-air-r42--with-warm-board
    (let* ((changed-file (car (org-air-query-files)))
           (before org-air-view--items))
      ;; one changed file: the change set (1) fits the sync budget (>=1).
      (should (<= 1 org-air-view--refresh-sync-budget))
      (org-air-r42--touch changed-file)
      ;; simulate the mid-load state a `g' would land on: `--refresh-cancel'
      ;; (inside `--refresh-start') disarms timers but does NOT clear state.
      (setq org-air-view--refresh-state 'refreshing
            org-air-view--loading t)
      (cl-letf (((symbol-function 'org-air-query-items-in-files)
                 (lambda (&rest _) (error "disk on fire"))))
        ;; catch here so the FIX passes cleanly (it handles internally) while
        ;; the REVERT — which lets the signal escape — leaves state
        ;; `refreshing' and the assertion below fails.
        (condition-case _ (org-air-view--refresh-start) (error nil)))
      ;; honest terminal state, never stranded.
      (should (eq org-air-view--refresh-state 'failed))
      (should-not (eq org-air-view--refresh-state 'refreshing))
      ;; the painted board is intact — the failed scan swapped nothing in.
      (should (eq org-air-view--items before))
      (should-not org-air-view--loading))))

(provide 'org-air-round42-test)
;;; org-air-round42-test.el ends here
