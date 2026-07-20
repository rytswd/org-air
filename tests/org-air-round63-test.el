;;; org-air-round63-test.el --- executing ERTs for v0.5 round-63 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.5 round-63 (air/v0.5/org-air-round63-design.org):
;; Item 1 the rail load-flicker fix — the deterministic TWO-BELT
;; single-owner rule (`org-air-rail--tail-owner-p' gating the three rail
;; choke points `org-air-rail--show' / `--host-width''s window ensure /
;; `--evict-foreign-rail', plus the synchronous ownership-transfer
;; suspension inside `--show'; the R58 undisplayed-host carve
;; subordinated, R63-1c); Item 2 the review main-pane redesign — flat
;; one-row-per-item sections (R63-2a), the shared F1 origin through
;; `org-air-view--item-origin-raw' (R63-2b), the mirror-collapse rule +
;; `org-air-review-collapse-mirrors' + the no-double-count law (R63-2c),
;; the board's section-header treatment (R63-2d), split cluster fits +
;; the compact `⚠' (R63-2e) and the review-mockup-170 golden (R63-2f).
;;
;; All BATCH/headless.  The rail ERTs use the R25 harness idiom the spec
;; names (r62-proven): batch Emacs has a real dummy-terminal frame whose
;; window tree and side-window machinery function, so `noninteractive'
;; is let-bound nil around the window choreography ONLY and
;; `org-air-rail--reconcile-frame' is driven SYNCHRONOUSLY; the board's
;; cold refresh is driven through `org-air-view--refresh-start' +
;; `--refresh-run-slice' (the machine's documented ERT drive — the start
;; runs under plain batch so no pacing timer is ever armed, the slices
;; under `noninteractive' nil so the progressive paint gates open).  The
;; spec's ERT seams T1-T12 map onto twelve ERTs; revert of each FAILS:
;;
;;   r63-1  (T1) OWNER STABILITY ACROSS A SIMULATED PACED FILL: board +
;;          review in two main windows of the batch frame, review
;;          SELECTED and owning the rail; the board's cold refresh
;;          driven synchronously one file per slice with a progressive
;;          paint FORCED after every slice (the paced fill's repeated
;;          repaints, compressed) and a synchronous reconcile after
;;          each — the owner is STILL the review buffer at every step,
;;          the rail window stays live, owner flips = 0, the rail
;;          buffer's bytes never move, `org-air-rail--render' is NEVER
;;          called with the board host (R63-1c: the R58 content carve
;;          is subordinate to the gate — no content flip even
;;          windowless), and the board ends SELF-SUSPENDED (belt 1's
;;          raising edge).  RED today: the first progressive paint's
;;          render tail re-owns the rail for the board.
;;   r63-2  (T2) RECONCILE-TIMER COUNT: the same drive with N rapid
;;          deferred `org-air-rail--reconcile' fires between slices —
;;          the pending `org-air-rail--reconcile-run' timers counted
;;          from `timer-list' never exceed ONE (pins R27-1 S3 under the
;;          load scenario, and pins that R63 adds belts, not timers);
;;          running the slot clears it to zero.
;;   r63-3  (T3) THE GATE TRUTH TABLE: `org-air-rail--tail-owner-p' —
;;          active==self ⇒ t; active==other ⇒ nil; active==nil ⇒ t;
;;          suspended self ⇒ nil REGARDLESS (even while active==self);
;;          a dead buffer never holds a claim.  Data-light:
;;          mode-initialised buffers + the real batch window tree.
;;   r63-4  (T4) A NON-OWNER TAIL IS INERT: with review owning the
;;          rail, a direct `org-air-rail--show' on the BOARD returns
;;          nil and leaves the rail window, the owner AND the rail
;;          buffer's text byte-identical while SELF-SUSPENDING the
;;          board; `--host-width' skips the window ensure (window
;;          fingerprint unchanged, the board's `org-air-view--rail-
;;          buffer' local never set, the width still measured); and
;;          `--evict-foreign-rail' never sweeps the active view's rail
;;          (review's window survives, review not suspended).  RED
;;          today on all three counts (content rewrite, re-own, sweep).
;;   r63-5  (T5) TRANSFER + ROUND-TRIP: `--show(review)' while the
;;          BOARD owns the rail suspends the board SYNCHRONOUSLY in the
;;          same call (belt 1) and re-owns for review; selecting the
;;          board window back + a synchronous reconcile re-owns the
;;          board with its suspended flag CLEARED (the R63-1d falling
;;          edge belongs to the reconciler) and review suspended by
;;          transfer; selecting review back re-owns review (ownership
;;          follows the ACTIVE view — R25-6 unbroken).  RED today: no
;;          belt 1, the board's flag stays nil after the takeover.
;;   r63-6  (T6) NEUTRALITY: a LONE displayed board (active == self)
;;          renders through the tail exactly as today — rail live,
;;          owner board, never suspended, repeat render + reconcile
;;          steady; handing the same window to review re-owns review
;;          through the same tail (the R62 handoff unregressed).
;;   r63-7  (T7) ONE ROW PER ITEM, NO ECHO: collapse OFF (isolating
;;          R63-2a), under EVERY basis (day/tag/directory/origin) the
;;          composed per-item sections contain ZERO `group' specs
;;          (item/agg/note only) and exactly one `item' spec per fold
;;          row; the rendered pane (all four bases, driven by real `f'
;;          presses) never contains a line that is a bare filename or a
;;          bare weekday-date group label, renders exactly one row per
;;          fixture heading, and the denote-file row's origin cell
;;          reads the de-machined denote TITLE while the raw `20260…'
;;          ID never renders anywhere.  RED today on the group lines
;;          and the raw-filename origin cell.
;;   r63-8  (T8) NO-DOUBLE-COUNT LAW + THE HARVEST VERDICT: the
;;          parent/child same-title fixture scans to TWO items at
;;          DISTINCT (FILE . POS) with the child's done log NOT on the
;;          parent (the own-body extents law — a subtree-wide harvest
;;          double-credits and FAILS); no rendered per-item section
;;          holds two rows sharing an identity — collapse ON and OFF,
;;          over this fixture AND the full mirror corpus (the
;;          mechanical guard that turns any future harvest duplication
;;          RED); the same-file parent/child pair collapses to ONE
;;          clean row with the normal origin (never `N files'); the
;;          one-heading done-log + CLOSED shape still folds to ONE
;;          stamp (the R61 stamps-win rule re-pinned).
;;   r63-9  (T9) MIRROR COLLAPSE: over the spec's T7 mirror corpus the
;;          default knob yields the fixture arithmetic — Completed
;;          exactly 2 rows, `▤ 3 files' / `▤ 2 files', the canonical
;;          item = the TAGGED denote note (its (FILE . POS) on the
;;          row's item/marker props, the full member list on
;;          `org-air-review-mirrors'), header "2 done", rail Summary 2,
;;          NO ×chip on the mirrored single completion; knob nil ⇒ 5
;;          rows / "5 done" (old behaviour), flipping back repaints to
;;          2 (render state only); a synthetic ×7 habit mirrored across
;;          two files keeps ×7 through collapse (the stamp UNION
;;          deduped by epoch) with `▤ 2 files' and the TAGGED mirror
;;          canonical; Started and Carried collapse under the same
;;          title×day key; Time invested totals and items are
;;          byte-unchanged either way (time attributed where clocked).
;;          RED today (5 rows, "5 done", no affordance).
;;   r63-10 (T10) SECTION HEADERS: each of the four heading lines
;;          carries the icon glyph (✓/◔/▷/↻ GUI, +/%/>/~ TTY — the S5b
;;          tier), the `org-air-section' property, the count chip and
;;          its `org-air-count-badge'; with `org-air-section-rule' t a
;;          rule line follows every heading (board parity).  RED today:
;;          no glyph, no badge property, no rule.
;;   r63-11 (T11) SPLIT CLUSTER FITS: spied through the shared V6
;;          `org-air-view--insert-row' — every ITEM row's date column
;;          equals the widest ITEM date (the `⚠' row) and NEVER the
;;          wider agg text; the agg rows compose their own (AW 0 0)
;;          fit with AW = the agg text width > the item fit; the
;;          rtrunc row's date cell is exactly "MMM D ⚠"-shaped and the
;;          loud "history truncated" phrase never rides a row (RED
;;          today: one folded fit, date column 13; the row carries the
;;          full phrase).
;;   r63-12 (T12) THE GOLDEN: the review surface rendered over the T7
;;          mirror corpus (width 170, height 40, frozen W29 2026 clock,
;;          default knobs, GUI glyphs, anti-tautology guards active) is
;;          byte-identical to tests/fixtures/review-mockup-170.txt —
;;          the regen-mockups discipline: fixture and assert share ONE
;;          render path (`org-air-viewport-test-review-mockup-lines').
;;   r63-13 (R63fix) THE FOURTH TAIL — the board-only RESPONSIVE
;;          TEARDOWN is owner-gated: a NARROW displayed NON-owner (or
;;          suspended) render of the board / revisit / project / review
;;          view reaches its board-only teardown branch and must NOT
;;          hide/delete another view's LIVE rail; the ACTUAL owner's
;;          narrow render still collapses (R56 unregressed).  Reverting
;;          the `org-air-rail--tail-owner-p' conjunct at ANY of the four
;;          teardown sites FAILS the matching leg.
;;
;; REVERT-FAIL: ALL TWELVE verified RED against the pre-R63 trunk
;; (muqylqul) in a scratch workspace — r63-1 fails with SIX owner
;; flips (one per slice: the reported flicker, reproduced by the seam
;; itself), r63-6 on the belt-1 handoff conjunct, r63-3/8 on the R63
;; predicate/identity APIs (their neutrality and harvest-extents
;; halves are the permanent green locks), and r63-2/4/5/7/9/10/11/12
;; on the pre-R63 behaviour by construction — the gate, the transfer
;; belt, the flat sections, the collapse knob, the shared origin, the
;; section glyphs and the golden's bytes do not exist there.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'timer)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)

(when (locate-library "org-air")
  (require 'org-air))

(defvar org-air-exclude-regexps)
(defvar org-air-cache-file)
(defvar org-air-rail-focus-on-popout)
(defvar org-air-review-collapse-mirrors)
(defvar org-air-section-rule)
(defvar org-air-refresh-slice-budget)
(defvar org-air-cold-paint-interval)

;;;; -------------------------------------------------------------------
;;;; Corpus scaffolding (the r61/r62 idioms, r63-sized)
;;;; -------------------------------------------------------------------

(defvar org-air-r63--dir nil
  "The temp corpus directory of the current `org-air-r63--with-corpus'.")

(defun org-air-r63--reset-tables ()
  "Clear the GLOBAL query-layer tables so no temp path leaks across tests."
  (clrhash org-air-query--file-meta)
  (clrhash org-air-query--visits)
  (clrhash org-air-query--denote-id-index)
  (setq org-air-query--link-graph-dirty nil))

(defmacro org-air-r63--with-corpus (specs &rest body)
  "Create a temp Org corpus from SPECS and run BODY against it.
SPECS is a list of (NAME . CONTENT) files written into a fresh TRUENAMED
temp directory.  Binds `org-air-files'/`org-air-inbox-file'/a temp
`org-air-cache-file', nil excludes and the 170x40 batch viewport (the
golden's geometry, so the origin cap is never squeezed by the fit
pass); wraps BODY in `save-window-excursion'; starts from EMPTY query
tables and cleans up tables, the reconcile timer slot, org-air view
buffers, corpus-visiting buffers and the directory."
  (declare (indent 1) (debug t))
  `(let* ((org-air-r63--dir (file-truename (make-temp-file "org-air-r63-" t))))
     (unwind-protect
         (progn
           (org-air-r63--reset-tables)
           (pcase-dolist (`(,name . ,content) ,specs)
             (let ((path (expand-file-name name org-air-r63--dir))
                   (coding-system-for-write 'utf-8-unix))
               (make-directory (file-name-directory path) t)
               (write-region (or content "") nil path nil 'silent)))
           (let ((org-air-files (list org-air-r63--dir))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-r63--dir))
                 (org-air-exclude-regexps nil)
                 (org-air-cache-file
                  (expand-file-name ".cache/board.eld" org-air-r63--dir))
                 (org-air-view-width 170)
                 (org-air-view-height 40))
             (save-window-excursion
               ,@body)))
       (org-air-query-teardown)
       (org-air-r63--reset-tables)
       (when (timerp org-air-rail--reconcile-timer)
         (cancel-timer org-air-rail--reconcile-timer))
       (setq org-air-rail--reconcile-timer nil)
       (let ((kill-buffer-query-functions nil))
         (dolist (name (list org-air-review-buffer-name
                             org-air-view-buffer-name
                             org-air-rail-buffer-name))
           (when (get-buffer name)
             (kill-buffer name)))
         (dolist (buf (buffer-list))
           (let ((fn (buffer-file-name buf)))
             (when (and fn (string-prefix-p org-air-r63--dir fn))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r63--dir t))))

(defun org-air-r63--file (name)
  "Return the absolute path of corpus file NAME."
  (expand-file-name name org-air-r63--dir))

(defconst org-air-r63--now org-air-viewport-test-review-now
  "The frozen \"now\": Sun 2026-07-19 18:00 local — inside ISO W29 2026.
The current week under it is [Jul 13, Jul 20) — the golden's clock.")

(defmacro org-air-r63--frozen-at (epoch &rest body)
  "Run BODY with the Lisp-visible clock frozen to integer EPOCH.
Overrides `current-time' AND no-arg `float-time'; `float-time' WITH an
argument passes through, so timestamp parsing and period math stay real."
  (declare (indent 1) (debug t))
  `(let ((org-air-r63--frozen ,epoch))
     (cl-letf* ((org-air-r63--real-ft (symbol-function 'float-time))
                ((symbol-function 'float-time)
                 (lambda (&optional time)
                   (if time (funcall org-air-r63--real-ft time)
                     (float org-air-r63--frozen))))
                ((symbol-function 'current-time)
                 (lambda () (seconds-to-time org-air-r63--frozen))))
       ,@body)))

(defun org-air-r63--epoch (y m d &optional hh mm)
  "Return the LOCAL integer epoch of Y-M-D HH:MM (defaults midnight)."
  (floor (float-time (encode-time (list 0 (or mm 0) (or hh 0)
                                        d m y nil -1 nil)))))

(defun org-air-r63--p0 () (org-air-r63--epoch 2026 7 13))
(defun org-air-r63--p1 () (org-air-r63--epoch 2026 7 20))

(defun org-air-r63--buffer-text ()
  "Return the current buffer's text without properties."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun org-air-r63--review-row-titles ()
  "Return the rendered review rows' item titles, in buffer order."
  (let ((pos (point-min)) titles)
    (while (setq pos (text-property-not-all pos (point-max)
                                            'org-air-item nil))
      (push (org-air-item-title (get-text-property pos 'org-air-item))
            titles)
      (setq pos (next-single-property-change pos 'org-air-item
                                             nil (point-max))))
    (nreverse titles)))

(defun org-air-r63--goto-review-row (title &optional file)
  "Move point onto the review row for TITLE (and item FILE, when given)."
  (let ((pos (point-min)) target)
    (while (and (not target)
                (setq pos (text-property-not-all pos (point-max)
                                                 'org-air-item nil)))
      (let ((item (get-text-property pos 'org-air-item)))
        (if (and (equal (org-air-item-title item) title)
                 (or (null file) (equal (org-air-item-file item) file)))
            (setq target pos)
          (setq pos (next-single-property-change pos 'org-air-item
                                                 nil (point-max))))))
    (should target)
    (goto-char target)))

(defun org-air-r63--row-line ()
  "Return the current line's text without properties."
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

(defun org-air-r63--rendered-rows ()
  "Return ((SECTION ID MIRRORS) …) for every rendered review item row.
SECTION is the enclosing heading's `org-air-section' symbol, ID the
row item's durable `org-air-review--item-id' and MIRRORS the row's
`org-air-review-mirrors' property (nil on an unmerged row)."
  (let ((pos (point-min)) out)
    (while (setq pos (text-property-not-all pos (point-max)
                                            'org-air-item nil))
      (let* ((item (get-text-property pos 'org-air-item))
             (mirrors (get-text-property pos 'org-air-review-mirrors))
             (section
              (save-excursion
                (goto-char pos)
                (catch 'sec
                  (while (> (point) (point-min))
                    (forward-line -1)
                    (let ((s (get-text-property (point) 'org-air-section)))
                      (when s (throw 'sec s))))
                  nil))))
        (push (list section (org-air-review--item-id item) mirrors) out))
      (setq pos (next-single-property-change pos 'org-air-item
                                             nil (point-max))))
    (nreverse out)))

(defun org-air-r63--assert-no-double-count ()
  "Assert the no-double-count law over the rendered review pane (T8).
Within each per-item section no two rows share a (FILE . POS) identity,
and no row's mirror list carries a duplicate identity."
  (let ((table (make-hash-table :test #'equal)))
    (pcase-dolist (`(,section ,id ,mirrors) (org-air-r63--rendered-rows))
      (let ((key (cons section id)))
        (should-not (gethash key table))
        (puthash key t table))
      (when mirrors
        (should (equal (length mirrors)
                       (length (delete-dups (copy-sequence mirrors)))))))))

;;;; -------------------------------------------------------------------
;;;; The rail seam (the R25 noninteractive-nil synchronous idiom)
;;;; -------------------------------------------------------------------

(defconst org-air-r63--rail-specs
  (cons '("inbox.org" . "* TODO Inbox capture\n")
        (mapcar
         (lambda (n)
           (cons (format "work-%d.org" n)
                 (format "* TODO Paced task %d :work:\n:LOGBOOK:\n- State \"DONE\"       from \"TODO\"       [2026-07-%02d Tue 10:00]\n:END:\n"
                         n (+ 13 n))))
         '(1 2 3 4 5)))
  "Six-file corpus for the simulated paced-fill rail ERTs.
Every file yields at least one item so every one-file slice triggers a
progressive paint (the paced fill's repeated repaints, compressed).")

(defmacro org-air-r63--with-review-rail (&rest body)
  "Open a data-filled review owning a REAL side rail; run BODY.
Data fills in plain batch (the inline scan); the window choreography —
select + `delete-other-windows', popped flag t, render (whose tail runs
`org-air-rail--show' and creates a REAL side window) — runs under
`noninteractive' nil (the R25 idiom).  BODY runs with `noninteractive'
nil, the review buffer current and `rbuf' bound to it."
  (declare (indent 0) (debug t))
  `(org-air-r63--with-corpus org-air-r63--rail-specs
     (org-air-r63--frozen-at org-air-r63--now
       (org-air-review)                 ; batch: inline scan, no windows
       (let ((rbuf (get-buffer org-air-review-buffer-name))
             (org-air-rail-focus-on-popout nil))
         (should (buffer-live-p rbuf))
         (should (buffer-local-value 'org-air-review--items rbuf))
         (let ((noninteractive nil))
           (select-window (frame-selected-window))
           (switch-to-buffer rbuf)
           (delete-other-windows (selected-window))
           (with-current-buffer rbuf
             (setq-local org-air-view--rail-popped-out t)
             (setq-local org-air-view--rail-suspended nil)
             (org-air-review--render-current)
             ,@body))))))

(defun org-air-r63--rail-bytes ()
  "Return the shared rail buffer's text without properties."
  (with-current-buffer org-air-rail-buffer-name
    (buffer-substring-no-properties (point-min) (point-max))))

(defun org-air-r63--window-fingerprint ()
  "Return the frame's window tree as plain comparable data."
  (list (selected-frame)
        (selected-window)
        (mapcar (lambda (w)
                  (list w (window-buffer w) (window-edges w)))
                (window-list nil t))))

(defun org-air-r63--pending-reconciles ()
  "Count pending `org-air-rail--reconcile-run' timers in `timer-list'."
  (seq-count (lambda (tm)
               (eq (timer--function tm) #'org-air-rail--reconcile-run))
             timer-list))

(defun org-air-r63--make-cold-board ()
  "Return the board buffer, mode-initialised, popped and NOT suspended.
The aggressive pre-fix shape: nothing has told this board it may not
drive the rail — only the R63 gate stands between its render tails and
the review's side window."
  (let ((bbuf (get-buffer-create org-air-view-buffer-name)))
    (with-current-buffer bbuf
      (unless (derived-mode-p 'org-air-view-mode)
        (org-air-view-mode))
      (setq-local org-air-view--rail-popped-out t)
      (setq-local org-air-view--rail-suspended nil))
    bbuf))

(defun org-air-r63--drive-cold-fill (bbuf rbuf &optional between-fn)
  "Drive BBUF's cold refresh one file per slice; assert RBUF keeps the rail.
The T1 seam: `org-air-view--refresh-start' runs under plain batch (no
pacing timer is ever armed), the slices under the ambient
`noninteractive' nil with a progressive paint FORCED after every slice
and a synchronous reconcile after each.  BETWEEN-FN (when given) runs
after every slice, before the asserts.  Returns the number of owner
flips observed (the T1 contract: 0)."
  (let ((flips 0) (steps 0) token)
    (with-current-buffer bbuf
      (setq token (let ((noninteractive t))
                    (org-air-view--refresh-start t)))
      (should (eq org-air-view--refresh-state 'refreshing))
      ;; True cold fill: stream mode on, ready to paint immediately.
      (should org-air-view--refresh-progressive))
    (let ((org-air-refresh-slice-budget 0)   ; exactly one file per slice
          (org-air-cold-paint-interval 0))
      (while (and (buffer-local-value 'org-air-view--refresh-queue bbuf)
                  (< steps 50))
        (cl-incf steps)
        (with-current-buffer bbuf
          ;; Force the progressive paint on EVERY slice — the paced
          ;; fill's repeated repaints, compressed into the drive.
          (setq org-air-view--refresh-last-paint nil)
          (org-air-view--refresh-run-slice bbuf token))
        (when between-fn (funcall between-fn))
        (unless (eq (org-air-rail--side-owner) rbuf) (cl-incf flips))
        ;; The deferred reconcile, driven synchronously (the R25 idiom).
        (org-air-rail--reconcile-frame (selected-frame))
        (should (window-live-p (org-air-rail--side-window)))
        (unless (eq (org-air-rail--side-owner) rbuf) (cl-incf flips))))
    ;; The machine converged (finish ran inside the last slice).
    (should (> steps 2))
    (should-not (buffer-local-value 'org-air-view--refresh-queue bbuf))
    (should-not (buffer-local-value 'org-air-view--refresh-state bbuf))
    flips))

(ert-deftest org-air-r63-1-owner-stable-across-paced-fill ()
  "T1: the board's simulated paced cold fill never steals review's rail.
Review selected + owning the rail, the board popped-and-UNsuspended in
a second main window (the aggressive shape): the whole synchronous
slice/paint/reconcile drive produces ZERO owner flips — the flicker
cause — with the rail window live and its BYTES untouched throughout,
`org-air-rail--render' never called with the board host (the R58 carve
subordinated, R63-1c), and the board SELF-SUSPENDED by its first gated
tail (belt 1's raising edge; every later background render is skipped
by the flag alone).  RED today: the first progressive paint's render
tail re-owns and rewrites the shared rail for the board."
  (skip-unless (locate-library "org-air"))
  (org-air-r63--with-review-rail
    (should (eq (org-air-rail--side-owner) rbuf))
    (let ((bbuf (org-air-r63--make-cold-board))
          (board-rail-renders 0))
      (let ((w2 (split-window (selected-window) nil 'below)))
        (set-window-buffer w2 bbuf)
        ;; The user sits in REVIEW while the board fills.
        (should (eq (window-buffer (selected-window)) rbuf))
        (let ((rail-before (org-air-r63--rail-bytes)))
          (cl-letf* ((org-air-r63--real-rr
                      (symbol-function 'org-air-rail--render))
                     ((symbol-function 'org-air-rail--render)
                      (lambda (host &rest args)
                        (when (eq host bbuf)
                          (cl-incf board-rail-renders))
                        (apply org-air-r63--real-rr host args))))
            (should (= 0 (org-air-r63--drive-cold-fill bbuf rbuf))))
          ;; Zero board-content renders: the R58 carve never rewrote the
          ;; shared rail buffer for the windowed OR windowless board.
          (should (= 0 board-rail-renders))
          ;; Review still owns; the rail bytes never moved.
          (should (eq (org-air-rail--side-owner) rbuf))
          (should (equal rail-before (org-air-r63--rail-bytes)))
          (should-not (buffer-local-value 'org-air-view--rail-suspended
                                          rbuf))
          ;; Belt 1's raising edge: the board suspended ITSELF at its
          ;; first gated tail and stayed down for the rest of the fill.
          (should (buffer-local-value 'org-air-view--rail-suspended
                                      bbuf)))))))

(ert-deftest org-air-r63-2-reconcile-timer-count-under-load ()
  "T2: one pending reconcile slot throughout the load; R63 adds no timer.
The same simulated paced fill with FIVE rapid deferred
`org-air-rail--reconcile' fires after every slice: the pending
`org-air-rail--reconcile-run' timers in `timer-list' never exceed ONE
(the R27-1 S3 single slot RESCHEDULES, never stacks), running the slot
clears it to zero, and the owner still never flips.  Reverting the
single-slot reschedule (one timer per hook fire) FAILS."
  (skip-unless (locate-library "org-air"))
  (org-air-r63--with-review-rail
    (let ((bbuf (org-air-r63--make-cold-board)))
      (let ((w2 (split-window (selected-window) nil 'below)))
        (set-window-buffer w2 bbuf)
        (unwind-protect
            (progn
              (should
               (= 0 (org-air-r63--drive-cold-fill
                     bbuf rbuf
                     (lambda ()
                       ;; N rapid hook-side fires between slices: the
                       ;; single slot reschedules, never stacks.
                       (dotimes (_ 5) (org-air-rail--reconcile))
                       (should (<= (org-air-r63--pending-reconciles) 1))))))
              (should (<= (org-air-r63--pending-reconciles) 1))
              ;; Running the slot clears it; the owner survives the run.
              (when (timerp org-air-rail--reconcile-timer)
                (cancel-timer org-air-rail--reconcile-timer)
                (org-air-rail--reconcile-run (selected-frame)))
              (should (= 0 (org-air-r63--pending-reconciles)))
              (should (eq (org-air-rail--side-owner) rbuf)))
          (when (timerp org-air-rail--reconcile-timer)
            (cancel-timer org-air-rail--reconcile-timer))
          (setq org-air-rail--reconcile-timer nil))))))

(ert-deftest org-air-r63-3-tail-owner-gate-truth-table ()
  "T3: the `org-air-rail--tail-owner-p' truth table, both conjuncts.
active==self ⇒ t; active==other ⇒ nil; active==nil ⇒ t; a SUSPENDED
self ⇒ nil regardless — even while its own window is selected (belt 1
must hold inside the C1 hook-selection window where an instantaneous
active check misreads); a dead buffer never holds a claim.  Data-light:
mode-initialised buffers + the real batch window tree."
  (skip-unless (locate-library "org-air"))
  (let ((host1 (generate-new-buffer " *org-air-r63-host1*"))
        (host2 (generate-new-buffer " *org-air-r63-host2*"))
        (plain (generate-new-buffer " *org-air-r63-plain*")))
    (unwind-protect
        (progn
          (with-current-buffer host1 (org-air-view-mode))
          (with-current-buffer host2 (org-air-review-mode))
          (save-window-excursion
            (let ((noninteractive nil))
              (select-window (frame-selected-window))
              (delete-other-windows (selected-window))
              (set-window-buffer (selected-window) host1)
              ;; active == self ⇒ t; active == other ⇒ nil.
              (should (org-air-rail--tail-owner-p host1))
              (should-not (org-air-rail--tail-owner-p host2))
              ;; Suspended self ⇒ nil REGARDLESS (active is still self —
              ;; the belt-1 conjunct blocks where the active check lies).
              (with-current-buffer host1
                (setq-local org-air-view--rail-suspended t))
              (should-not (org-air-rail--tail-owner-p host1))
              (with-current-buffer host1
                (setq-local org-air-view--rail-suspended nil))
              (should (org-air-rail--tail-owner-p host1))
              ;; active == nil ⇒ t (the R58 bookmark-restore flow).
              (set-window-buffer (selected-window) plain)
              (should (org-air-rail--tail-owner-p host1))
              (should (org-air-rail--tail-owner-p host2))))
          ;; A dead buffer never holds a claim.
          (kill-buffer host2)
          (should-not (org-air-rail--tail-owner-p host2)))
      (dolist (buf (list host1 host2 plain))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest org-air-r63-4-non-owner-tail-is-inert ()
  "T4: every gated choke point is a FULL no-op for a non-owner tail.
With review owning the rail: a direct `org-air-rail--show' on the board
returns nil, leaves the window tree, the owner and the rail buffer's
BYTES identical, and self-suspends the board; `--host-width' skips the
window ensure (fingerprint unchanged, the board's rail-buffer local
never set, an integer width still measured); `--evict-foreign-rail'
never sweeps the active view's rail.  RED today on all three counts
(content rewrite + re-own, the ensure, the sweep — the sweep DELETES
review's rail: the same bug with the opposite sign)."
  (skip-unless (locate-library "org-air"))
  (org-air-r63--with-review-rail
    (let ((bbuf (org-air-r63--make-cold-board)))
      (with-current-buffer bbuf
        (setq org-air-view--items (org-air-query-items)))
      (let ((w2 (split-window (selected-window) nil 'below)))
        (set-window-buffer w2 bbuf)
        (should (eq (org-air-rail--side-owner) rbuf))
        (let ((fp (org-air-r63--window-fingerprint))
              (bytes (org-air-r63--rail-bytes))
              (side (org-air-rail--side-window)))
          ;; (a) `--show' on the non-owner: full no-op + self-suspend.
          (should-not (org-air-rail--show bbuf 170))
          (should (eq (org-air-rail--side-window) side))
          (should (eq (org-air-rail--side-owner) rbuf))
          (should (equal bytes (org-air-r63--rail-bytes)))
          (should (equal fp (org-air-r63--window-fingerprint)))
          (should (buffer-local-value 'org-air-view--rail-suspended bbuf))
          ;; (b) `--host-width': the ensure is skipped, the measure not.
          (with-current-buffer bbuf
            (setq-local org-air-view--rail-suspended nil))
          (should (integerp (org-air-rail--host-width bbuf 170)))
          (should (equal fp (org-air-r63--window-fingerprint)))
          (should (eq (org-air-rail--side-owner) rbuf))
          ;; The skipped ensure never wired the board to the rail.
          (should-not (buffer-local-value 'org-air-view--rail-buffer bbuf))
          ;; (c) `--evict-foreign-rail': no sweep privilege without the
          ;; claim — review's rail survives, review is never suspended.
          (org-air-rail--evict-foreign-rail bbuf)
          (should (window-live-p (org-air-rail--side-window)))
          (should (eq (org-air-rail--side-owner) rbuf))
          (should (equal bytes (org-air-r63--rail-bytes)))
          (should-not (buffer-local-value 'org-air-view--rail-suspended
                                          rbuf)))))))

(ert-deftest org-air-r63-5-transfer-suspension-and-round-trip ()
  "T5: ownership transfer is synchronous; the reconciler owns the fall.
The board owns the rail; showing REVIEW through the same tail (the `W'
press's essential call) suspends the board IN THE SAME CALL (belt 1 —
before any C1 resize render can fire) and re-owns for review; selecting
the board window back + a synchronous reconcile re-owns the board with
its suspended flag CLEARED (the R63-1d falling edge: only the
reconciler's suspended branch or a user toggle may clear it) and review
suspended by transfer; selecting review back re-owns review — ownership
follows the ACTIVE view (R25-6 unbroken).  RED today: no belt 1 — the
board's flag stays nil after the takeover."
  (skip-unless (locate-library "org-air"))
  (org-air-r63--with-corpus org-air-r63--rail-specs
    (org-air-r63--frozen-at org-air-r63--now
      (org-air-review)                  ; batch: data only, no windows
      (let ((rbuf (get-buffer org-air-review-buffer-name))
            (bbuf (org-air-r63--make-cold-board))
            (org-air-rail-focus-on-popout nil))
        (with-current-buffer bbuf
          (setq org-air-view--items (org-air-query-items)))
        (with-current-buffer rbuf
          (setq-local org-air-view--rail-popped-out t)
          (setq-local org-air-view--rail-suspended nil))
        (let ((noninteractive nil))
          (select-window (frame-selected-window))
          (switch-to-buffer bbuf)
          (delete-other-windows (selected-window))
          (let ((w1 (selected-window)))
            ;; The board renders through the tail and owns the rail.
            (with-current-buffer bbuf
              (org-air-view--render org-air-view--items nil))
            (should (eq (org-air-rail--side-owner) bbuf))
            (should-not (buffer-local-value 'org-air-view--rail-suspended
                                            bbuf))
            (let ((w2 (split-window w1 nil 'below)))
              (set-window-buffer w2 rbuf)
              (select-window w2)        ; review is the active view now
              ;; The transfer: ONE call takes the rail AND suspends the
              ;; previous owner synchronously (belt 1).
              (should (org-air-rail--show rbuf 170))
              (should (eq (org-air-rail--side-owner) rbuf))
              (should (buffer-local-value 'org-air-view--rail-suspended
                                          bbuf))
              (should-not (buffer-local-value 'org-air-view--rail-suspended
                                              rbuf))
              ;; Round-trip: the reconciler (settled active view) owns
              ;; the falling edge — the board re-pops, flag cleared.
              (select-window w1)
              (org-air-rail--reconcile-frame (selected-frame))
              (should (eq (org-air-rail--side-owner) bbuf))
              (should-not (buffer-local-value 'org-air-view--rail-suspended
                                              bbuf))
              (should (buffer-local-value 'org-air-view--rail-suspended
                                          rbuf))
              ;; …and back: ownership follows the active view.
              (select-window w2)
              (org-air-rail--reconcile-frame (selected-frame))
              (should (eq (org-air-rail--side-owner) rbuf))
              (should-not (buffer-local-value 'org-air-view--rail-suspended
                                              rbuf))
              (should (buffer-local-value 'org-air-view--rail-suspended
                                          bbuf)))))))))

(ert-deftest org-air-r63-6-lone-view-tail-neutrality ()
  "T6: a lone displayed view (active == self) renders exactly as today.
The gate is invisible to the legitimate owner: a lone board's render
tail pops, owns and keeps the rail (never suspended), a repeat render
and a double reconcile are steady no-ops; handing the SAME window to
review re-owns the rail for review through the same tail (the R62
board→review handoff unregressed).  A green-stays-green lock."
  (skip-unless (locate-library "org-air"))
  (org-air-r63--with-corpus org-air-r63--rail-specs
    (org-air-r63--frozen-at org-air-r63--now
      (org-air-review)                  ; data for the handoff leg
      (let ((rbuf (get-buffer org-air-review-buffer-name))
            (bbuf (org-air-r63--make-cold-board))
            (org-air-rail-focus-on-popout nil))
        (with-current-buffer bbuf
          (setq org-air-view--items (org-air-query-items)))
        (with-current-buffer rbuf
          (setq-local org-air-view--rail-popped-out t)
          (setq-local org-air-view--rail-suspended nil))
        (let ((noninteractive nil))
          (select-window (frame-selected-window))
          (switch-to-buffer bbuf)
          (delete-other-windows (selected-window))
          ;; The lone board: tail pops + owns, never suspended.
          (with-current-buffer bbuf
            (org-air-view--render org-air-view--items nil))
          (should (window-live-p (org-air-rail--side-window)))
          (should (eq (org-air-rail--side-owner) bbuf))
          (should-not (buffer-local-value 'org-air-view--rail-suspended
                                          bbuf))
          ;; Repeat render: steady state, still the owner.
          (with-current-buffer bbuf
            (org-air-view--render org-air-view--items nil))
          (should (eq (org-air-rail--side-owner) bbuf))
          (should-not (buffer-local-value 'org-air-view--rail-suspended
                                          bbuf))
          ;; Double reconcile: a no-op for the settled owner.
          (org-air-rail--reconcile-frame (selected-frame))
          (org-air-rail--reconcile-frame (selected-frame))
          (should (eq (org-air-rail--side-owner) bbuf))
          (should-not (buffer-local-value 'org-air-view--rail-suspended
                                          bbuf))
          ;; The handoff: review takes the same window; its render tail
          ;; re-owns the rail (belt 1 suspends the board).
          (switch-to-buffer rbuf)
          (with-current-buffer rbuf
            (org-air-review--render-current))
          (should (window-live-p (org-air-rail--side-window)))
          (should (eq (org-air-rail--side-owner) rbuf))
          (should (buffer-local-value 'org-air-view--rail-suspended bbuf))
          (should-not (buffer-local-value 'org-air-view--rail-suspended
                                          rbuf)))))))

;;;; -------------------------------------------------------------------
;;;; Item 2 — the review main-pane redesign (the T7 mirror corpus)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r63-7-flat-sections-no-echo ()
  "T7: one row per item under EVERY basis; the echo lines are gone.
Collapse OFF (isolating R63-2a): the composed per-item sections carry
ZERO `group' specs and exactly one `item' spec per fold row under all
four bases; the rendered pane — walked through real `f' presses —
renders exactly one row per fixture heading (3 Manage + 2 Ensure
completed rows over the mirror corpus), never a line that is a bare
filename (the `origin'-basis fake headers) or a bare weekday-date (the
`day'-basis date echo), and the denote row's origin cell reads the
de-machined denote TITLE while the raw `20260…' ID never renders.
RED today on the group lines and the raw-filename origin cell."
  (skip-unless (locate-library "org-air"))
  (org-air-r63--with-corpus (org-air-viewport-test-review-fixture-specs)
    (let ((org-air-review-collapse-mirrors nil))
      ;; Structural: the composed sections are FLAT under every basis.
      (let* ((items (org-air-query-items))
             (data (org-air-review--section-data
                    items (org-air-r63--p0) (org-air-r63--p1) nil)))
        (dolist (basis '(day tag directory origin))
          (let ((sections (org-air-review--compose-sections
                           data basis (org-air-r63--p0) (org-air-r63--p1))))
            (dolist (section sections)
              (dolist (line (nth 3 section))
                ;; item/agg/note ONLY — `group' is design-removed.
                (should (memq (car line) '(item agg note)))))
            ;; One `item' spec per fold row, per section.
            (should (= (seq-count (lambda (l) (eq (car l) 'item))
                                  (nth 3 (assq 'completed sections)))
                       (length (plist-get data :completed))))
            (should (= (length (plist-get data :completed)) 5)))))
      ;; Rendered: all four bases via real `f' presses.
      (org-air-r63--frozen-at org-air-r63--now
        (org-air-review)
        (with-current-buffer org-air-review-buffer-name
          (dolist (basis '(day tag directory origin))
            (should (eq org-air-review--rollup basis))
            (let ((text (org-air-r63--buffer-text)))
              ;; The raw denote IDs never render — not in an origin
              ;; cell, not as a group line.  (The `directory' basis'
              ;; Time-agg LABEL is `org-air-item-group' — CATEGORY /
              ;; `file-name-base', the pre-R63 R61 lens outside the
              ;; R63-2b origin fork — so the ID may legitimately name
              ;; that one agg row; every ITEM row stays ID-free, which
              ;; the denote-row origin assert below pins per basis.)
              (unless (eq basis 'directory)
                (should-not (string-match-p "20260131T020339" text))
                (should-not (string-match-p "20260129T113501" text)))
              (dolist (line (split-string text "\n"))
                (let ((trimmed (string-trim line)))
                  ;; No line is a bare filename (the fake headers)…
                  (dolist (leaf '("Active-Work.org" "Active-Tasks.org"
                                  "Journal.org" "inbox.org"))
                    (should-not (equal trimmed leaf)))
                  ;; …or a bare weekday-date (the day-basis echo).
                  (should-not
                   (string-match-p
                    "\\`\\(?:Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\|Sun\\) [A-Z][a-z]+ [0-9]+\\'"
                    trimmed))))
              ;; Exactly one row per fixture heading.
              (let ((titles (org-air-r63--review-row-titles)))
                (should (= 3 (seq-count
                              (lambda (x)
                                (equal x "Manage data-handler logic"))
                              titles)))
                (should (= 2 (seq-count
                              (lambda (x)
                                (equal x "Ensure multi-admin-web is published to ghcr"))
                              titles)))
                (should (= 1 (seq-count
                              (lambda (x)
                                (equal x "Prototype the range ladder"))
                              titles)))
                (should (= 1 (seq-count
                              (lambda (x) (equal x "Fix flaky rail test"))
                              titles))))
              ;; The denote row's origin cell: the de-machined TITLE,
              ;; never the machine ID — under every basis.
              (org-air-r63--goto-review-row
               "Manage data-handler logic"
               (org-air-r63--file
                "20260131T020339--manage-data-handler-logic.org"))
              (should (string-match-p "manage-data-handler"
                                      (org-air-r63--row-line)))
              (should-not (string-match-p "20260131T020339"
                                          (org-air-r63--row-line))))
            (org-air-review-cycle-rollup))
          ;; The lens cycled full circle.
          (should (eq org-air-review--rollup 'day)))))))

(defconst org-air-r63--extents-specs
  '(("inbox.org" . "* TODO Inbox capture\n")
    ("parent-child.org" . "\
* DONE Review the sprint
:PROPERTIES:
:CREATED: [2026-06-01 Mon 09:00]
:END:
:LOGBOOK:
- State \"DONE\"       from \"TODO\"       [2026-07-14 Tue 14:00]
:END:
** DONE Review the sprint
:PROPERTIES:
:CREATED: [2026-06-01 Mon 09:00]
:END:
:LOGBOOK:
- State \"DONE\"       from \"TODO\"       [2026-07-14 Tue 14:00]
:END:
")
    ("closed-shape.org" . "\
* DONE Ship the closed shape
CLOSED: [2026-07-15 Wed 12:00]
:PROPERTIES:
:CREATED: [2026-06-01 Mon 09:00]
:END:
:LOGBOOK:
- State \"DONE\"       from \"TODO\"       [2026-07-15 Wed 12:00]
:END:
"))
  "The T8 decision fixture: a KEYWORDED same-title parent/child pair
(both render — unlike the mirror corpus's keyword-less container) and
the one-heading done-log + CLOSED shape.")

(ert-deftest org-air-r63-8-no-double-count-law ()
  "T8: the harvest verdict re-run + the permanent mechanical guard.
The same-title parent/child pair — DONE-logged at the SAME instant,
the Air convention's mirror shape — scans to TWO items at DISTINCT
\(FILE . POS) and each heading's done log stays its OWN (exactly one
stamp each — a subtree-wide harvest credits the child's log line to
the parent too, giving it TWO, and FAILS); the rendered sections never
hold two
rows sharing an identity — collapse ON and OFF, over this fixture AND
the full mirror corpus (any future harvest/merge duplication turns
RED here); the same-file pair collapses to ONE clean CHIP-LESS row
\(the stamp union dedupes the shared epoch — a mirrored single
completion is one completion) wearing the normal origin (never
`N files') with both member identities on the mirrors property; the
done-log + CLOSED heading folds to ONE stamp (the R61 stamps-win rule
re-pinned — no double count, no ×chip)."
  (skip-unless (locate-library "org-air"))
  (org-air-r63--with-corpus org-air-r63--extents-specs
    (let* ((items (org-air-query-items))
           (pair (seq-filter (lambda (it)
                               (equal (org-air-item-title it)
                                      "Review the sprint"))
                             items))
           (p0 (org-air-r63--p0))
           (p1 (org-air-r63--p1)))
      ;; TWO real items at DISTINCT positions in ONE file.
      (should (= (length pair) 2))
      (let ((ids (mapcar #'org-air-review--item-id pair)))
        (should-not (equal (nth 0 ids) (nth 1 ids)))
        (should (equal (car (nth 0 ids)) (car (nth 1 ids)))))
      ;; The extents law: each heading's log is its OWN — exactly ONE
      ;; stamp each (a subtree-wide harvest hands the parent BOTH log
      ;; lines and fails the length here).
      (dolist (it pair)
        (should (= (length (org-air-item-logs it)) 1))
        (should (equal (car (car (org-air-item-logs it)))
                       (org-air-r63--epoch 2026 7 14 14))))
      ;; Stamps win: the done-log + CLOSED shape folds to ONE stamp.
      (let ((closed (seq-find (lambda (it)
                                (equal (org-air-item-title it)
                                       "Ship the closed shape"))
                              items)))
        (should closed)
        (should (equal (org-air-review--completed-stamps closed p0 p1)
                       (list (org-air-r63--epoch 2026 7 15 12)))))
      ;; Rendered: the law, both knob states.
      (org-air-r63--frozen-at org-air-r63--now
        (org-air-review)
        (with-current-buffer org-air-review-buffer-name
          (org-air-r63--assert-no-double-count)
          ;; Collapse ON: the same-file pair is ONE clean row — normal
          ;; origin, never a mirror-count affordance, both identities
          ;; on the mirrors property.
          (let ((titles (org-air-r63--review-row-titles)))
            (should (= 1 (seq-count (lambda (x)
                                      (equal x "Review the sprint"))
                                    titles))))
          (org-air-r63--goto-review-row "Review the sprint")
          (should-not (string-match-p "[0-9] files"
                                      (org-air-r63--row-line)))
          (should-not (string-match-p "×" (org-air-r63--row-line)))
          (let ((mirrors (get-text-property (point)
                                            'org-air-review-mirrors)))
            (should (= (length mirrors) 2))
            (should (= (length (delete-dups (mapcar #'car mirrors))) 1)))
          ;; The CLOSED-shape row is chip-less (one stamp, never two).
          (org-air-r63--goto-review-row "Ship the closed shape")
          (should-not (string-match-p "×" (org-air-r63--row-line)))
          ;; Collapse OFF: two distinct rows, the law still holds.
          (let ((org-air-review-collapse-mirrors nil))
            (org-air-review--render-current)
            (let ((titles (org-air-r63--review-row-titles)))
              (should (= 2 (seq-count (lambda (x)
                                        (equal x "Review the sprint"))
                                      titles))))
            (org-air-r63--assert-no-double-count))))))
  ;; The guard over the FULL mirror corpus, both knob states.
  (org-air-r63--with-corpus (org-air-viewport-test-review-fixture-specs)
    (org-air-r63--frozen-at org-air-r63--now
      (org-air-review)
      (with-current-buffer org-air-review-buffer-name
        (org-air-r63--assert-no-double-count)
        (let ((org-air-review-collapse-mirrors nil))
          (org-air-review--render-current)
          (org-air-r63--assert-no-double-count))))))

(ert-deftest org-air-r63-9-mirror-collapse ()
  "T9: the mirror-collapse arithmetic, the knob and the honest counts.
Default knob over the T7 mirror corpus: Completed is exactly 2 rows —
Manage once with `▤ 3 files' (canonical = the TAGGED denote note: its
\(FILE . POS) on the row's item/marker props, all three member
identities on `org-air-review-mirrors'), Ensure once with `▤ 2 files';
the header reads \"2 done\" and the rail Summary 2 (the R61-4 law:
totals honestly describe what is shown); the mirrored SINGLE completion
wears no ×chip (the stamp union dedupes by epoch).  Knob nil ⇒ 5 rows /
\"5 done\" (the old behaviour, restorable), and flipping back repaints
to 2 — render state only, zero rescans by the R61 law.  A synthetic ×7
habit mirrored across two files keeps its ×7 through collapse with
`▤ 2 files' and the TAGGED mirror canonical; Started and Carried
collapse under the same title×day key; Time invested totals and item
counts are IDENTICAL under both knob states.  RED today."
  (skip-unless (locate-library "org-air"))
  ;; The T7 corpus arithmetic.
  (org-air-r63--with-corpus (org-air-viewport-test-review-fixture-specs)
    (org-air-r63--frozen-at org-air-r63--now
      (org-air-review)
      (with-current-buffer org-air-review-buffer-name
        ;; Exactly 2 Completed rows; the honest header + rail Summary.
        (let ((titles (org-air-r63--review-row-titles)))
          (should (= 1 (seq-count (lambda (x)
                                    (equal x "Manage data-handler logic"))
                                  titles)))
          (should (= 1 (seq-count
                        (lambda (x)
                          (equal x "Ensure multi-admin-web is published to ghcr"))
                        titles))))
        (let ((text (org-air-r63--buffer-text)))
          (should (string-match-p "2 done" text))
          (should-not (string-match-p "5 done" text))
          (should (string-match-p "  2   completed" text)))
        ;; The Manage row: `▤ 3 files', canonical = the tagged denote
        ;; note, member list riding the mirrors property, no ×chip.
        (org-air-r63--goto-review-row "Manage data-handler logic")
        (let ((line (org-air-r63--row-line))
              (item (get-text-property (point) 'org-air-item))
              (marker (get-text-property (point) 'org-air-marker))
              (mirrors (get-text-property (point) 'org-air-review-mirrors)))
          (should (string-match-p "3 files" line))
          (should-not (string-match-p "×" line))
          (should (equal (org-air-item-file item)
                         (org-air-r63--file
                          "20260131T020339--manage-data-handler-logic.org")))
          (should (equal (org-air-item-tags item) '("backend" "code")))
          (should (equal marker (org-air-item-marker item)))
          (should (= (length mirrors) 3))
          (should (= (length (delete-dups (mapcar #'car mirrors))) 3))
          (should (member (org-air-review--item-id item) mirrors)))
        (org-air-r63--goto-review-row
         "Ensure multi-admin-web is published to ghcr")
        (should (string-match-p "2 files" (org-air-r63--row-line)))
        ;; Knob nil: the old one-row-per-heading behaviour, honestly
        ;; counted; flipping back repaints to the collapsed shape.
        (let ((org-air-review-collapse-mirrors nil))
          (org-air-review--render-current)
          (let ((titles (org-air-r63--review-row-titles))
                (text (org-air-r63--buffer-text)))
            (should (= 3 (seq-count (lambda (x)
                                      (equal x "Manage data-handler logic"))
                                    titles)))
            (should (string-match-p "5 done" text))
            (should (string-match-p "  5   completed" text))
            (should-not (string-match-p "[0-9] files" text))))
        (org-air-review--render-current)
        (should (= 1 (seq-count (lambda (x)
                                  (equal x "Manage data-handler logic"))
                                (org-air-r63--review-row-titles)))))))
  ;; The ×7 habit + Started/Carried collapse + time neutrality.
  (org-air-r63--with-corpus
      (let ((stamps (mapconcat
                     (lambda (d)
                       (format "- State \"DONE\"       from \"TODO\"       [2026-07-%02d 07:00]\n" d))
                     '(13 14 15 16 17 18 19) "")))
        (list
         '("inbox.org" . "* TODO Inbox capture\n")
         (cons "habit-a.org"
               (concat "* DONE Water the plants\n"
                       ":PROPERTIES:\n:CREATED: [2026-06-01 Mon 08:00]\n:END:\n"
                       ":LOGBOOK:\n" stamps
                       "CLOCK: [2026-07-14 Tue 06:00]--[2026-07-14 Tue 07:00] =>  1:00\n"
                       ":END:\n"))
         (cons "habit-b.org"
               (concat "* DONE Water the plants :garden:\n"
                       ":PROPERTIES:\n:CREATED: [2026-06-01 Mon 08:00]\n:END:\n"
                       ":LOGBOOK:\n" stamps
                       "CLOCK: [2026-07-15 Wed 06:00]--[2026-07-15 Wed 07:00] =>  1:00\n"
                       ":END:\n"))
         '("started-a.org" . "* TODO Draft the launch note\n:PROPERTIES:\n:CREATED: [2026-07-16 Thu 09:00]\n:END:\n")
         '("started-b.org" . "* TODO Draft the launch note\n:PROPERTIES:\n:CREATED: [2026-07-16 Thu 11:00]\n:END:\n")
         '("carried-a.org" . "* TODO Chase the flaky bug\n:PROPERTIES:\n:CREATED: [2026-06-01 Mon 08:00]\n:END:\n:LOGBOOK:\n- Note taken on [2026-07-17 Fri 09:30] \\\\\n  still flaky\n:END:\n")
         '("carried-b.org" . "* TODO Chase the flaky bug\n:PROPERTIES:\n:CREATED: [2026-06-01 Mon 08:00]\n:END:\n:LOGBOOK:\n- Note taken on [2026-07-17 Fri 15:00] \\\\\n  still flaky\n:END:\n")))
    (let* ((items (org-air-query-items))
           (p0 (org-air-r63--p0))
           (p1 (org-air-r63--p1))
           (raw (org-air-review--section-data items p0 p1 t))
           (collapsed (org-air-review--collapse-data
                       (org-air-review--section-data items p0 p1 t))))
      ;; Time invested is NOT collapsed: totals + items identical.
      (should (= (plist-get raw :time-total) 7200))
      (should (= (plist-get collapsed :time-total) 7200))
      (should (= (length (plist-get raw :time-items)) 2))
      (should (= (length (plist-get collapsed :time-items)) 2))
      ;; The stamp UNION dedupes by epoch: 7 real completions stay 7.
      (should (= (length (plist-get raw :completed)) 2))
      (should (= (length (plist-get collapsed :completed)) 1))
      (let ((row (car (plist-get collapsed :completed))))
        (should (= (nth 2 row) 7))
        (should (= (length (nth 3 row)) 7))
        (should (= (length (nth 4 row)) 2))))
    (org-air-r63--frozen-at org-air-r63--now
      (org-air-review)
      (with-current-buffer org-air-review-buffer-name
        ;; The habit: ONE row, ×7 through collapse, 2 files, the TAGGED
        ;; mirror canonical (precedence rule 1 over snapshot order).
        (let ((titles (org-air-r63--review-row-titles)))
          (should (= 1 (seq-count (lambda (x) (equal x "Water the plants"))
                                  titles)))
          (should (= 1 (seq-count (lambda (x)
                                    (equal x "Draft the launch note"))
                                  titles)))
          (should (= 1 (seq-count (lambda (x)
                                    (equal x "Chase the flaky bug"))
                                  titles))))
        (org-air-r63--goto-review-row "Water the plants")
        (let ((line (org-air-r63--row-line))
              (item (get-text-property (point) 'org-air-item)))
          (should (string-match-p "×7" line))
          (should (string-match-p "2 files" line))
          (should (equal (org-air-item-file item)
                         (org-air-r63--file "habit-b.org"))))
        ;; Time invested renders identically under both knob states.
        (should (string-match-p "2:00" (org-air-r63--buffer-text)))
        (let ((org-air-review-collapse-mirrors nil))
          (org-air-review--render-current)
          (let ((titles (org-air-r63--review-row-titles)))
            (should (= 2 (seq-count (lambda (x)
                                      (equal x "Water the plants"))
                                    titles)))
            (should (= 2 (seq-count (lambda (x)
                                      (equal x "Draft the launch note"))
                                    titles)))
            (should (= 2 (seq-count (lambda (x)
                                      (equal x "Chase the flaky bug"))
                                    titles))))
          (should (string-match-p "2:00" (org-air-r63--buffer-text))))))))

(ert-deftest org-air-r63-10-board-section-headers ()
  "T10: the four review headings wear the board's section treatment.
Under the GUI tier each heading line carries its icon glyph (✓ ◔ ▷ ↻),
the section title, the count chip, the `org-air-section' property and
the `org-air-count-badge'; the pure-ASCII TTY tier degrades the glyphs
\(+ % > ~) through the same S5b table; with `org-air-section-rule' t a
rule line follows every heading (board parity).  RED today: no glyph,
no badge property, no rule."
  (skip-unless (locate-library "org-air"))
  (org-air-r63--with-corpus (org-air-viewport-test-review-fixture-specs)
    (org-air-r63--frozen-at org-air-r63--now
      ;; GUI tier: the four icon glyphs + properties.
      (org-air-viewport-test-as-gui
        (org-air-review)
        (with-current-buffer org-air-review-buffer-name
          (pcase-dolist (`(,sym ,heading ,count)
                         '((completed "✓ Completed" 2)
                           (time "◔ Time invested" 1)
                           (started "▷ Started" 1)
                           (carried "↻ Carried over" 1)))
            (goto-char (point-min))
            (should (search-forward (format "%s %d" heading count) nil t))
            (let ((bol (line-beginning-position)))
              (should (eq (get-text-property bol 'org-air-section) sym))
              (should (= (get-text-property bol 'org-air-count-badge)
                         count))))))
      ;; TTY tier: the ASCII fallbacks through the same table.
      (org-air-review)
      (with-current-buffer org-air-review-buffer-name
        (dolist (heading '("+ Completed 2" "% Time invested 1"
                           "> Started 1" "~ Carried over 1"))
          (goto-char (point-min))
          (should (search-forward heading nil t))))
      ;; The section rule follows every heading when the knob is on.
      (org-air-viewport-test-as-gui
        (let ((org-air-section-rule t))
          (org-air-review)
          (with-current-buffer org-air-review-buffer-name
            (dolist (heading '("✓ Completed" "◔ Time invested"
                               "▷ Started" "↻ Carried over"))
              (goto-char (point-min))
              (should (search-forward heading nil t))
              (forward-line 1)
              (should (string-match-p
                       "\\`─" (org-air-r63--row-line))))))))))

(ert-deftest org-air-r63-11-split-cluster-fits ()
  "T11: the item date fit never folds in the agg text; the compact ⚠.
Spied through the shared V6 `org-air-view--insert-row': every ITEM
row's date column (car :widths) equals the widest ITEM date — the
rtrunc row's \"MMM D ⚠\" — and NEVER the wider agg text; the agg rows
compose their own (AW 0 0) fit with AW = the widest agg text, strictly
wider than the item fit in this corpus (13 vs the measured 6-per-date +
the ⚠ cell); the rtrunc row's date cell is exactly \"MMM D ⚠\"-shaped
and the loud \"history truncated\" phrase never rides a row (the note
line owns the words — r61-3 pins them where a capped heading clocks).
RED today: ONE folded fit (date column = the 13-col agg text) and the
full phrase on the row."
  (skip-unless (locate-library "org-air"))
  (org-air-r63--with-corpus (org-air-viewport-test-review-fixture-specs)
    (org-air-r63--frozen-at org-air-r63--now
      (let (calls)
        (cl-letf* ((org-air-r63--real-ir
                    (symbol-function 'org-air-view--insert-row))
                   ((symbol-function 'org-air-view--insert-row)
                    (lambda (&rest args)
                      (push args calls)
                      (apply org-air-r63--real-ir args))))
          (org-air-review))
        (with-current-buffer org-air-review-buffer-name
          (let* ((item-calls
                  (seq-filter (lambda (args)
                                (plist-member (plist-get args :props)
                                              'org-air-item))
                              calls))
                 (agg-calls
                  (seq-filter (lambda (args)
                                (and (null (plist-get args :props))
                                     (equal (nth 1 (plist-get args :widths))
                                            0)))
                              calls))
                 (item-dates
                  (mapcar (lambda (args)
                            (substring-no-properties
                             (plist-get args :date-text)))
                          item-calls))
                 (item-dw (apply #'max (mapcar #'string-width item-dates)))
                 (agg-texts
                  (mapcar (lambda (args)
                            (substring-no-properties
                             (plist-get args :date-text)))
                          agg-calls))
                 (agg-aw (apply #'max (mapcar #'string-width agg-texts))))
            (should (= (length item-calls) 4))   ; 2 done + started + carried
            (should (>= (length agg-calls) 1))   ; the day-rollup agg row
            ;; The split: item rows fit over ITEM dates only…
            (dolist (args item-calls)
              (should (= (nth 0 (plist-get args :widths)) item-dw)))
            ;; …the agg rows over agg text only, each its own column.
            (dolist (args agg-calls)
              (should (equal (plist-get args :widths) (list agg-aw 0 0))))
            ;; The anti-fold conjunct: the agg text is strictly wider —
            ;; a folded fit would inflate every item date cell to it.
            (should (string-match-p "· 1 item" (car agg-texts)))
            (should (> agg-aw item-dw))
            ;; The compact marker: exactly "MMM D ⚠", nothing louder.
            (let ((trunc (seq-find (lambda (d) (string-match-p "⚠" d))
                                   item-dates)))
              (should trunc)
              (should (string-match-p
                       "\\`[A-Z][a-z][a-z] [0-9]+ ⚠\\'" trunc))
              (should (= (string-width trunc) item-dw)))
            (dolist (d item-dates)
              (should-not (string-match-p "history truncated" d))))
          ;; The rendered row carries the compact cell, not the phrase.
          (org-air-r63--goto-review-row "Prototype the range ladder")
          (should (string-match-p "Jul 16 ⚠" (org-air-r63--row-line)))
          (should-not (string-match-p "history truncated"
                                      (org-air-r63--row-line))))))))

(ert-deftest org-air-r63-12-review-mockup-golden ()
  "T12: the review surface is byte-identical to the blessed golden.
`org-air-viewport-test-review-mockup-lines' — the T7 mirror corpus at
width 170, height 40, frozen W29 2026 clock, DEFAULT knobs, GUI glyphs,
the anti-tautology render guards active — against
tests/fixtures/review-mockup-170.txt (right-trimmed, trailing blanks
dropped: the regen-mockups contract).  The fixture and this assert
share ONE render path, so the golden pins the real renderer's bytes;
any layout drift (a returning group line, a raw denote ID, a lost
glyph, a moved column) diverges here first."
  (skip-unless (locate-library "org-air"))
  (let ((file (expand-file-name "review-mockup-170.txt"
                                org-air-test-fixture-dir)))
    (should (file-readable-p file))
    (let* ((expected (org-air-viewport-test--drop-trailing-blanks
                      (mapcar #'string-trim-right
                              (split-string
                               (with-temp-buffer
                                 (insert-file-contents file)
                                 (buffer-string))
                               "\n"))))
           (actual (org-air-viewport-test-review-mockup-lines)))
      (unless (equal actual expected)
        (let ((i 0))
          (while (and (< i (length expected)) (< i (length actual))
                      (equal (nth i expected) (nth i actual)))
            (setq i (1+ i)))
          (ert-fail
           (format "review render diverges from the golden at line %d (%d expected / %d actual lines)\nexpected: %S\nactual:   %S"
                   (1+ i) (length expected) (length actual)
                   (nth i expected) (nth i actual))))))))

(ert-deftest org-air-r63-13-board-only-teardown-gated ()
  "R63fix: the board-only responsive teardown is an OWNER privilege.
The FOURTH tail: each view's render tail deletes the side rail when it
lands board-only — and before the fix it did so UNGATED, so a narrow
DISPLAYED non-owner render (the R58 undisplayed carve does not mask a
windowed host) deleted another view's LIVE rail.  Four legs, one per
teardown site: with REVIEW owning the rail, a narrow board / revisit /
project render (each landing `board-only', each displayed in a second
main window while review stays selected) leaves review's rail window
live, the owner and the rail bytes untouched; with the BOARD owning the
rail, a narrow displayed review render — unsuspended AND suspended —
leaves the board's rail alike.  The non-regression conjunct: the ACTUAL
owner's own narrow render still tears the side window down (the R56
responsive collapse).  Reverting the `org-air-rail--tail-owner-p'
conjunct at ANY of the four sites FAILS the matching leg here."
  (skip-unless (locate-library "org-air"))
  ;; Legs 1-3: review owns the rail; board / revisit / project teardowns.
  (org-air-r63--with-review-rail
    (should (eq (org-air-rail--side-owner) rbuf))
    (let ((bytes (org-air-r63--rail-bytes))
          (bbuf (org-air-r63--make-cold-board))
          (vbuf (get-buffer-create org-air-revisit-buffer-name))
          (pbuf (generate-new-buffer "*org-air-r63-project*")))
      (unwind-protect
          (let ((w2 (split-window (selected-window) nil 'below)))
            ;; -- the board site (org-air-view.el) --
            (with-current-buffer bbuf
              (setq org-air-view--items (org-air-query-items)))
            (set-window-buffer w2 bbuf)
            (should (eq (window-buffer (selected-window)) rbuf))
            (with-current-buffer bbuf
              (let ((org-air-view-width 60))
                (org-air-view--render org-air-view--items nil))
              ;; The teardown branch WAS taken — no vacuous pass…
              (should (eq org-air-view--orientation 'board-only)))
            ;; …and the gate held: review's live rail survives intact.
            (should (window-live-p (org-air-rail--side-window)))
            (should (eq (org-air-rail--side-owner) rbuf))
            (should (equal bytes (org-air-r63--rail-bytes)))
            ;; -- the revisit site (org-air-revisit.el) --
            (with-current-buffer vbuf
              (unless (derived-mode-p 'org-air-revisit-mode)
                (org-air-revisit-mode))
              (setq-local org-air-view--rail-popped-out nil)
              (setq-local org-air-view--rail-suspended nil))
            (set-window-buffer w2 vbuf)
            (with-current-buffer vbuf
              (let ((org-air-view-width 60))
                (org-air-revisit--render))
              (should (eq org-air-view--orientation 'board-only)))
            (should (window-live-p (org-air-rail--side-window)))
            (should (eq (org-air-rail--side-owner) rbuf))
            (should (equal bytes (org-air-r63--rail-bytes)))
            ;; -- the project site (org-air-project.el) --
            (with-current-buffer pbuf
              (unless (derived-mode-p 'org-air-project-mode)
                (org-air-project-mode))
              (setq-local org-air-project--root org-air-r63--dir)
              (setq-local org-air-view--rail-popped-out nil)
              (setq-local org-air-view--rail-suspended nil))
            (set-window-buffer w2 pbuf)
            (with-current-buffer pbuf
              (let ((org-air-view-width 60))
                (org-air-project--render-current))
              (should (eq org-air-view--orientation 'board-only)))
            (should (window-live-p (org-air-rail--side-window)))
            (should (eq (org-air-rail--side-owner) rbuf))
            (should (equal bytes (org-air-r63--rail-bytes))))
        ;; The revisit/project buffers are not on the corpus macro's kill
        ;; list; their kill hooks (rail teardown) run AFTER the asserts.
        (let ((kill-buffer-query-functions nil))
          (dolist (buf (list vbuf pbuf))
            (when (buffer-live-p buf) (kill-buffer buf)))))))
  ;; Leg 4: the board owns the rail; the REVIEW teardown site — plus the
  ;; R56 owner-collapse non-regression conjunct.
  (org-air-r63--with-corpus org-air-r63--rail-specs
    (org-air-r63--frozen-at org-air-r63--now
      (org-air-review)                  ; batch: data only, no windows
      (let ((rbuf (get-buffer org-air-review-buffer-name))
            (bbuf (org-air-r63--make-cold-board))
            (org-air-rail-focus-on-popout nil))
        (with-current-buffer bbuf
          (setq org-air-view--items (org-air-query-items)))
        (let ((noninteractive nil))
          (select-window (frame-selected-window))
          (switch-to-buffer bbuf)
          (delete-other-windows (selected-window))
          ;; The board renders through the tail and owns the rail.
          (with-current-buffer bbuf
            (org-air-view--render org-air-view--items nil))
          (should (eq (org-air-rail--side-owner) bbuf))
          (let ((w2 (split-window (selected-window) nil 'below))
                (bytes (org-air-r63--rail-bytes)))
            (set-window-buffer w2 rbuf)   ; displayed; the board stays
            ;; selected — review is a windowed NON-owner.
            ;; -- the review site (org-air-review.el), unsuspended --
            (with-current-buffer rbuf
              (setq-local org-air-view--rail-popped-out nil)
              (setq-local org-air-view--rail-suspended nil)
              (let ((org-air-view-width 60))
                (org-air-review--render-current))
              (should (eq org-air-view--orientation 'board-only)))
            (should (window-live-p (org-air-rail--side-window)))
            (should (eq (org-air-rail--side-owner) bbuf))
            (should (equal bytes (org-air-r63--rail-bytes)))
            ;; -- the review site, SUSPENDED (belt 1 blocks alone) --
            (with-current-buffer rbuf
              (setq-local org-air-view--rail-suspended t)
              (let ((org-air-view-width 60))
                (org-air-review--render-current))
              (should (eq org-air-view--orientation 'board-only)))
            (should (window-live-p (org-air-rail--side-window)))
            (should (eq (org-air-rail--side-owner) bbuf))
            (should (equal bytes (org-air-r63--rail-bytes)))
            ;; R56 unregressed: the ACTUAL owner's narrow render still
            ;; collapses — the gate passes for the owner and the
            ;; responsive teardown deletes the side window.
            (with-current-buffer bbuf
              (let ((org-air-view-width 60))
                (org-air-view--render org-air-view--items nil))
              (should (eq org-air-view--orientation 'board-only)))
            (should-not (org-air-rail--side-window))))))))

(provide 'org-air-round63-test)
;;; org-air-round63-test.el ends here
