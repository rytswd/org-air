;;; org-air-round62-test.el --- executing ERTs for v0.5 round-62 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.5 round-62 (air/v0.5/org-air-round62-design.org):
;; Item 1 the review side-window rail fix — `org-air-review-mode' joins
;; the `org-air-rail--host-buffer-p' roster (R62-1a, the root cause), the
;; `org-air-rail-toggle' guard (R62-1b), the pane-stash host cond
;; (R62-1c) and the `org-air-rail--placement' resolver with the two new
;; per-view knobs (R62-1d); Item 2 the range ladder
;; week → fortnight → month → quarter → year (R62-2: fixed-phase Monday
;; fortnights, exact clip + adjacency, render-time, ZERO rescan) with the
;; `+'/`-'/`m'/`.'/`<'/`>' bindings, the four-row legend and the
;; bookmark range carry (R62-3).
;;
;; All BATCH/headless.  The rail ERTs use the R25 harness idiom the spec
;; names: batch Emacs has a real (dummy-terminal) frame whose window
;; tree, side-window parameters and `display-buffer-in-side-window' all
;; function, so `noninteractive' is let-bound nil around the window
;; choreography ONLY and `org-air-rail--reconcile-frame' is driven
;; SYNCHRONOUSLY (the deferred 0s-timer choreography is the spec's named
;; GUI residue, user-confirm only).  The spec's ERT seams T1-T10 map
;; onto ten ERTs; revert of each FAILS:
;;
;;   r62-1  (T1) HOST ROSTER: `org-air-rail--host-buffer-p' is non-nil
;;          for a buffer in EACH of the four host modes — board,
;;          project, revisit AND review (the unfixed tree fails the
;;          review clause; the next sibling surface fails this test the
;;          moment it derives a new mode and forgets the roster) — plus
;;          the R26-5 doc-session clause; nil for a foreign buffer and
;;          a dead one.
;;   r62-2  (T2) RECONCILE KEEPS REVIEW'S RAIL: review in the batch
;;          frame's single MAIN window, popped flag t, rendered (the
;;          tail runs `org-air-rail--show'), then a synchronous
;;          `org-air-rail--reconcile-frame' under `noninteractive' nil
;;          ⇒ the side window is LIVE with `window-side' =
;;          `org-air-rail-side', `org-air-rail--side-owner' IS the
;;          review buffer, orientation stays `side-window', suspended
;;          stays nil, exactly ONE rail window — and a second reconcile
;;          is a no-op (steady state).  Unfixed: window dead, owner
;;          nil, suspended t (verified RED via the pre-fix predicate).
;;   r62-3  (T3) BOARD STEALS NOTHING: a popped-suspended BOARD in a
;;          second main window while review is SELECTED ⇒ after the
;;          synchronous reconcile the owner is STILL review (pins the
;;          GUI Scenario 1 content theft T2's single-window shape
;;          cannot see) — and ownership still FOLLOWS THE ACTIVE VIEW
;;          both ways (select the board ⇒ re-owned board, review
;;          suspended; select review back ⇒ re-owned review), so the
;;          single-owner machinery itself is pinned unbroken.
;;   r62-4  (T4) TOGGLE GUARD: `org-air-rail-toggle' in a review buffer
;;          never signals — it pops the rail IN (flag nil, side window
;;          gone) and back OUT (flag t, owner review); a foreign
;;          fundamental-mode buffer still gets the `user-error' (the
;;          guard was widened, not dropped).
;;   r62-5  (T5) PLACEMENT PARITY: `(org-air-rail--placement 'review)'
;;          and `'revisit' honour their new per-view knobs over the
;;          shared default, the legacy alist shape still resolves, the
;;          nil fallback is `side-window', and board/project/outline
;;          resolution is unchanged (no regression); both knobs are
;;          real nil-default defcustoms.
;;   r62-6  (T6) RANGE ORACLE: fortnight/quarter/year bounds against
;;          independently built local-midnight epochs — the fixed-phase
;;          `defconst' is Monday 1970-01-05, every fortnight starts on
;;          a Monday and is EXACTLY two consecutive ISO weeks tiling it
;;          (incl. across the W53/2026 edge: [Dec 21, Jan 4) then
;;          [Jan 4, Jan 18)); quarter = calendar quarters with the Q4
;;          December-style year rollover and a year-boundary pair;
;;          year = the calendar year; the ADJACENCY LAW (bounds(kind,
;;          END) starts at END, bounds(kind, START−1) ends at START,
;;          the anchor inside its own bounds) over 5 kinds × 9 edge
;;          dates × 2 TZs (UTC + America/New_York, `setenv' restored);
;;          clip complementarity at the three NEW edges (60 + 30 =
;;          5400 exact, adjacent period 0); an unknown KIND totals to
;;          the week branch, never signals.
;;   r62-7  (T7) NAV STEPS BY THE ACTIVE UNIT: at the command level,
;;          for EACH of the five ranges — `.' shows the current
;;          period, `>' lands on the bounds STARTING at the previous
;;          half-open END (one week / 14 days / 1 month / 3 months /
;;          1 year, pinned by explicit epochs), `<' returns
;;          byte-identical bounds, `.' clears the anchor; the rungs are
;;          adopted by real `+' presses.
;;   r62-8  (T8) LADDER KEYS + NO RESCAN: `+'/`='/`-'/`m'/`.'/`<'/`>'
;;          resolve in `org-air-review-mode-map'; from the current week
;;          `+' × 4 walks fortnight → month → quarter → year with a nil
;;          anchor staying nil (every rung tracks the current period),
;;          a final clamped `+' messages \"widest range\" without
;;          moving, `-' walks back down and clamps at week, `m' × 5
;;          wraps back to week; an ABSOLUTE anchor (a `<' press) is
;;          preserved across the whole widen walk with every rung's
;;          period CONTAINING the anchor day; a trimmed
;;          `org-air-review-ranges' cycles only its members (the
;;          (week month) trim IS the old toggle), an off-ladder current
;;          kind still narrows out, unknown symbols drop and an empty
;;          knob degrades to (week month) — and the WHOLE burst runs
;;          under `org-air-query--scan-file' AND corpus
;;          `insert-file-contents' spies at ZERO calls (range change is
;;          filter+fold only — the R61 law, re-pinned).
;;   r62-9  (T9) BOOKMARK CARRIES THE RANGE: a navigated QUARTER view
;;          records `(quarter . <Q-start epoch>)' in the unchanged
;;          `(KIND . ANCHOR)' shape, round-trips prin1→read `equal',
;;          and the autoloaded handler alone rebuilds it CACHE-FIRST
;;          (zero scans spied, zero display calls, window fingerprint
;;          unchanged) restoring kind + anchor + bounds with point on
;;          the bookmarked (non-first) row; an R61-shape week record
;;          still applies; a KIND `decade' record degrades to the
;;          default current week on a fresh buffer without signalling;
;;          a `current'-anchor quarter record restored under an
;;          advanced frozen clock tracks the NEW current quarter.
;;   r62-10 (T10) LABELS + LEGEND TRUTH: the five short/full label
;;          shapes at frozen period starts, the cross-ISO-year
;;          fortnight label (\"W52 2025–W1 2026\") and the same-year
;;          W52–53 pairing at the W53/2026 edge (second week numbered
;;          by calendar arithmetic, never first-week + 1); the Actions
;;          legend is FOUR rows of three, every KEY resolves via
;;          `key-binding' in the live review buffer to a command (and
;;          never a bare prefix map — the R26/R50 discipline), `.'
;;          joined the legend, `+'/`-' are listed, `=' is bound to
;;          widen but stays a legend-less alias; the R62-2 calendar
;;          centring refinement (today's month iff today ∈ [P0, P1),
;;          else P0's month).
;;
;; REVERT-FAIL: r62-1..4 were verified RED against the pre-fix
;; predicate/guard (swapped back in-memory) during development; r62-5..10
;; are red on the pre-R62 trunk by construction — the placement knobs,
;; the fortnight/quarter/year branches, the widen/narrow/cycle commands,
;; the five-kind apply `memq' and the four-row legend do not exist there.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'bookmark)
(require 'calendar)
(require 'cal-iso)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)

(when (locate-library "org-air")
  (require 'org-air))

(defvar org-air-exclude-regexps)
(defvar org-air-cache-file)
(defvar org-air-rail-placement)
(defvar org-air-rail-side)
(defvar org-air-rail-focus-on-popout)
(defvar org-air-board-rail-placement)
(defvar org-air-project-rail-placement)
(defvar org-air-outline-rail-placement)
(defvar org-air-revisit-rail-placement)
(defvar org-air-review-rail-placement)
(defvar org-air-review-ranges)

;;;; -------------------------------------------------------------------
;;;; Corpus scaffolding (the r61 idioms, r62-sized)
;;;; -------------------------------------------------------------------

(defvar org-air-r62--dir nil
  "The temp corpus directory of the current `org-air-r62--with-corpus'.")

(defun org-air-r62--reset-tables ()
  "Clear the GLOBAL query-layer tables so no temp path leaks across tests."
  (clrhash org-air-query--file-meta)
  (clrhash org-air-query--visits)
  (clrhash org-air-query--denote-id-index)
  (setq org-air-query--link-graph-dirty nil))

(defmacro org-air-r62--with-corpus (specs &rest body)
  "Create a temp Org corpus from SPECS and run BODY against it.
SPECS is a list of (NAME . CONTENT) files written into a fresh TRUENAMED
temp directory.  Binds `org-air-files'/`org-air-inbox-file'/a temp
`org-air-cache-file', nil excludes, a nil `bookmark-alist' and the
120x40 batch viewport; wraps BODY in `save-window-excursion'; starts
from EMPTY query tables and cleans up tables, org-air view buffers,
corpus-visiting buffers and the directory."
  (declare (indent 1) (debug t))
  `(let* ((org-air-r62--dir (file-truename (make-temp-file "org-air-r62-" t))))
     (unwind-protect
         (progn
           (org-air-r62--reset-tables)
           (pcase-dolist (`(,name . ,content) ,specs)
             (let ((path (expand-file-name name org-air-r62--dir))
                   (coding-system-for-write 'utf-8-unix))
               (make-directory (file-name-directory path) t)
               (write-region (or content "") nil path nil 'silent)))
           (let ((org-air-files (list org-air-r62--dir))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-r62--dir))
                 (org-air-exclude-regexps nil)
                 (org-air-cache-file
                  (expand-file-name ".cache/board.eld" org-air-r62--dir))
                 (org-air-view-width 120)
                 (org-air-view-height 40)
                 (bookmark-alist nil))
             (save-window-excursion
               ,@body)))
       (org-air-query-teardown)
       (org-air-r62--reset-tables)
       (let ((kill-buffer-query-functions nil))
         (dolist (name (list org-air-review-buffer-name
                             org-air-view-buffer-name
                             org-air-rail-buffer-name))
           (when (get-buffer name)
             (kill-buffer name)))
         (dolist (buf (buffer-list))
           (let ((fn (buffer-file-name buf)))
             (when (and fn (string-prefix-p org-air-r62--dir fn))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r62--dir t))))

(defun org-air-r62--file (name)
  "Return the absolute path of corpus file NAME."
  (expand-file-name name org-air-r62--dir))

(defun org-air-r62--epoch (y m d &optional hh mm)
  "Return the LOCAL integer epoch of Y-M-D HH:MM (defaults midnight).
The tests' independent oracle: built with `encode-time' on local
calendar dates exactly like the period engine's boundaries, so the
comparisons are TZ-independent."
  (floor (float-time (encode-time (list 0 (or mm 0) (or hh 0)
                                        d m y nil -1 nil)))))

(defconst org-air-r62--now (org-air-r62--epoch 2026 6 15 10)
  "The frozen \"now\": Mon 2026-06-15 10:00 local — inside ISO W25 2026.
Current periods under it: week [Jun 15, Jun 22), fortnight
[Jun 8, Jun 22), month June, quarter Q2 [Apr 1, Jul 1), year 2026.")

(defmacro org-air-r62--frozen-at (epoch &rest body)
  "Run BODY with the Lisp-visible clock frozen to integer EPOCH.
Overrides `current-time' AND no-arg `float-time'; `float-time' WITH an
argument passes through, so timestamp parsing and period math stay real."
  (declare (indent 1) (debug t))
  `(let ((org-air-r62--frozen ,epoch))
     (cl-letf* ((org-air-r62--real-ft (symbol-function 'float-time))
                ((symbol-function 'float-time)
                 (lambda (&optional time)
                   (if time (funcall org-air-r62--real-ft time)
                     (float org-air-r62--frozen))))
                ((symbol-function 'current-time)
                 (lambda () (seconds-to-time org-air-r62--frozen))))
       ,@body)))

(defmacro org-air-r62--spying-scans (counter &rest body)
  "Run BODY counting `org-air-query--scan-file' calls into place COUNTER."
  (declare (indent 1) (debug t))
  `(cl-letf* ((org-air-r62--real-scan
               (symbol-function 'org-air-query--scan-file))
              ((symbol-function 'org-air-query--scan-file)
               (lambda (&rest args)
                 (cl-incf ,counter)
                 (apply org-air-r62--real-scan args))))
     ,@body))

(defmacro org-air-r62--spying-reads (counter &rest body)
  "Run BODY counting corpus-file `insert-file-contents' calls in COUNTER."
  (declare (indent 1) (debug t))
  `(cl-letf* ((org-air-r62--real-ifc
               (symbol-function 'insert-file-contents))
              ((symbol-function 'insert-file-contents)
               (lambda (filename &rest args)
                 (when (and (stringp filename)
                            (string-prefix-p org-air-r62--dir
                                             (expand-file-name filename)))
                   (cl-incf ,counter))
                 (apply org-air-r62--real-ifc filename args))))
     ,@body))

(defvar org-air-r62--messages nil
  "Messages captured by `org-air-r62--capturing-messages', newest first.")

(defmacro org-air-r62--capturing-messages (&rest body)
  "Run BODY capturing every `message' into `org-air-r62--messages'."
  (declare (indent 0) (debug t))
  `(let ((org-air-r62--messages nil))
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args)
                  (when fmt
                    (push (apply #'format fmt args) org-air-r62--messages))
                  nil)))
       ,@body)))

(defun org-air-r62--review-row-titles ()
  "Return the rendered review rows' item titles, in buffer order."
  (let ((pos (point-min)) titles)
    (while (setq pos (text-property-not-all pos (point-max)
                                            'org-air-item nil))
      (push (org-air-item-title (get-text-property pos 'org-air-item))
            titles)
      (setq pos (next-single-property-change pos 'org-air-item
                                             nil (point-max))))
    (nreverse titles)))

(defun org-air-r62--goto-review-row (title)
  "Move point onto the review row whose item TITLE matches exactly; assert."
  (let ((pos (point-min)) target)
    (while (and (not target)
                (setq pos (text-property-not-all pos (point-max)
                                                 'org-air-item nil)))
      (if (equal (org-air-item-title (get-text-property pos 'org-air-item))
                 title)
          (setq target pos)
        (setq pos (next-single-property-change pos 'org-air-item
                                               nil (point-max)))))
    (should target)
    (goto-char target)
    (org-air-view--goto-row-title)))

;;;; Bookmark helpers (the r58/r61 idioms).

(defun org-air-r62--alist (record)
  "Return RECORD's alist half (`bookmark-make-record' may prepend NAME)."
  (if (stringp (car-safe record)) (cdr record) record))

(defun org-air-r62--field (record key)
  "Return KEY's value in RECORD's alist, or nil."
  (cdr (assq key (org-air-r62--alist record))))

(defun org-air-r62--roundtrip (record)
  "Assert RECORD prin1→read round-trips `equal'; return the re-read copy."
  (let* ((printed (let ((print-length nil) (print-level nil))
                    (prin1-to-string record)))
         (reread (car (read-from-string printed))))
    (should (equal reread record))
    reread))

(defun org-air-r62--window-fingerprint ()
  "Return the frame's window tree as plain comparable data (R58 T7)."
  (list (selected-frame)
        (selected-window)
        (mapcar (lambda (w)
                  (list w (window-buffer w) (window-edges w)))
                (window-list nil t))))

(defvar org-air-r62--display-calls 0
  "Calls to the window-display entry points inside the no-display spy.")

(defmacro org-air-r62--asserting-no-display (&rest body)
  "Run BODY spying every window-display entry point (the R58 rule 3)."
  (declare (indent 0) (debug t))
  `(let ((org-air-r62--display-calls 0)
         (org-air-r62--wc-before (org-air-r62--window-fingerprint)))
     (cl-letf* ((org-air-r62--real-ptb (symbol-function 'pop-to-buffer))
                ((symbol-function 'pop-to-buffer)
                 (lambda (&rest args)
                   (cl-incf org-air-r62--display-calls)
                   (apply org-air-r62--real-ptb args)))
                (org-air-r62--real-stb (symbol-function 'switch-to-buffer))
                ((symbol-function 'switch-to-buffer)
                 (lambda (&rest args)
                   (cl-incf org-air-r62--display-calls)
                   (apply org-air-r62--real-stb args)))
                (org-air-r62--real-db (symbol-function 'display-buffer))
                ((symbol-function 'display-buffer)
                 (lambda (&rest args)
                   (cl-incf org-air-r62--display-calls)
                   (apply org-air-r62--real-db args))))
       ,@body)
     (should (= 0 org-air-r62--display-calls))
     (should (equal org-air-r62--wc-before
                    (org-air-r62--window-fingerprint)))))

;;;; -------------------------------------------------------------------
;;;; The rail seam (the R25 noninteractive-nil synchronous idiom)
;;;; -------------------------------------------------------------------

(defconst org-air-r62--rail-specs
  '(("inbox.org" . "* TODO Inbox capture\n")
    ("work.org" . "\
* TODO Clocked task :work:
:LOGBOOK:
CLOCK: [2026-06-16 Tue 09:00]--[2026-06-16 Tue 10:00] =>  1:00
:END:
"))
  "Minimal live corpus for the rail choreography ERTs.")

(defmacro org-air-r62--with-review-rail (&rest body)
  "Open a data-filled review in the batch frame's single MAIN window; run BODY.
Data fills in plain batch (the inline scan); the window choreography —
select + `delete-other-windows', popped flag t, render (whose tail runs
`org-air-rail--show' and creates a REAL side window) — runs under
`noninteractive' bound nil, exactly the R25 harness idiom the spec
names.  BODY runs with `noninteractive' nil and the review buffer
current, `rbuf' bound to it."
  (declare (indent 0) (debug t))
  `(org-air-r62--with-corpus org-air-r62--rail-specs
     (org-air-r62--frozen-at org-air-r62--now
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

(ert-deftest org-air-r62-1-rail-host-roster ()
  "T1: every one of the FOUR host modes passes the rail host predicate.
`org-air-rail--host-buffer-p' answers non-nil for a buffer in
`org-air-view-mode', `org-air-project-mode', `org-air-revisit-mode' AND
`org-air-review-mode' (the R62-1a root cause: the unfixed roster names
only the first three, so the reconciler treated review as a FOREIGN
window and evicted its rail) plus the R26-5 doc-session clause; a
fundamental-mode buffer and a dead buffer answer nil.  Data-free: mode
init only.  Reverting the roster FAILS the review clause; a future
sibling surface that derives a new host mode and forgets the roster
fails here first."
  (skip-unless (locate-library "org-air"))
  ;; The roster — the review clause is the one the unfixed tree fails.
  (dolist (mode '(org-air-view-mode
                  org-air-project-mode
                  org-air-revisit-mode
                  org-air-review-mode))
    (with-temp-buffer
      (funcall mode)
      (should (org-air-rail--host-buffer-p (current-buffer)))))
  ;; The R26-5 doc-session clause is untouched.
  (with-temp-buffer
    (setq-local org-air-project--session-tree '(doc-session))
    (should (org-air-rail--host-buffer-p (current-buffer))))
  ;; Foreign and dead buffers are never hosts.
  (with-temp-buffer
    (should-not (org-air-rail--host-buffer-p (current-buffer))))
  (let ((dead (generate-new-buffer " *org-air-r62-dead*")))
    (kill-buffer dead)
    (should-not (org-air-rail--host-buffer-p dead))))

(ert-deftest org-air-r62-2-reconcile-keeps-review-rail ()
  "T2: the synchronous single-owner reconcile KEEPS review's side rail.
Review as the ONLY main window (the user's reported shape), popped flag
t, rendered — then `org-air-rail--reconcile-frame' driven SYNCHRONOUSLY
under `noninteractive' nil (the R25 idiom): the `*org-air-rail*' side
window stays LIVE with its `window-side' parameter on
`org-air-rail-side', `org-air-rail--side-owner' IS the review buffer,
review's orientation stays `side-window', its suspended flag stays nil
and exactly ONE rail window exists; a SECOND reconcile changes nothing
\(steady state).  On the unfixed roster the reconcile resolves the
active view to nil and takes the orphan branch — window DELETED, owner
nil, review suspended (the reported symptom) — so every assert here
FAILS."
  (skip-unless (locate-library "org-air"))
  (org-air-r62--with-review-rail
    ;; The render tail popped the rail: live side window, owned by us.
    (let ((side (org-air-rail--side-window)))
      (should (window-live-p side))
      (should (eq (org-air-rail--side-owner) rbuf)))
    ;; The deferred reconcile, driven synchronously — the fix's seam.
    (org-air-rail--reconcile-frame (selected-frame))
    (let ((side (org-air-rail--side-window)))
      (should (window-live-p side))
      (should (eq (window-parameter side 'window-side) org-air-rail-side))
      (should (eq (org-air-rail--side-owner) rbuf))
      (should (eq (buffer-local-value 'org-air-view--orientation rbuf)
                  'side-window))
      (should-not (buffer-local-value 'org-air-view--rail-suspended rbuf))
      (with-current-buffer org-air-rail-buffer-name
        (should (eq org-air-rail--board-buffer rbuf)))
      (should (= 1 (length (get-buffer-window-list
                            (get-buffer org-air-rail-buffer-name)
                            'no-mini (selected-frame))))))
    ;; Steady state: a second reconcile is a no-op.
    (org-air-rail--reconcile-frame (selected-frame))
    (should (window-live-p (org-air-rail--side-window)))
    (should (eq (org-air-rail--side-owner) rbuf))
    (should-not (buffer-local-value 'org-air-view--rail-suspended rbuf))))

(ert-deftest org-air-r62-3-board-never-steals-review-rail ()
  "T3: a popped board in a second main window never steals review's rail.
Review SELECTED and popped with the rail live; the board rendered in a
second main window carrying a popped-but-SUSPENDED flag (exactly the
state the reconciler leaves the backgrounded view in) ⇒ after the
synchronous reconcile the owner is STILL the review buffer, review is
not suspended and the board still is — the unfixed roster resolves the
active view to the BOARD and re-owns the rail to it (the GUI Scenario 1
ping-pong), failing here.  Then the machinery itself is pinned
unbroken: selecting the BOARD window and reconciling re-owns the rail
to the board with review suspended, and selecting review back re-owns
it to review — ownership follows the ACTIVE view, exactly R25-6."
  (skip-unless (locate-library "org-air"))
  (org-air-r62--with-corpus org-air-r62--rail-specs
    (org-air-r62--frozen-at org-air-r62--now
      (org-air-review)                  ; batch: inline scan, no windows
      (let ((rbuf (get-buffer org-air-review-buffer-name))
            (bbuf (get-buffer-create org-air-view-buffer-name))
            (org-air-rail-focus-on-popout nil))
        (should (buffer-live-p rbuf))
        ;; A minimal REAL board, rendered in plain batch BEFORE any side
        ;; window exists (its render tail's foreign-rail sweep must not
        ;; run against review's rail during setup), then flagged
        ;; popped-but-SUSPENDED — the backgrounded-view state.
        (with-current-buffer bbuf
          (org-air-view-mode)
          (setq org-air-view--items (org-air-query-items))
          (setq-local org-air-view--rail-popped-out nil)
          (org-air-view--render org-air-view--items nil)
          (setq-local org-air-view--rail-popped-out t)
          (setq-local org-air-view--rail-suspended t))
        (let ((noninteractive nil))
          (select-window (frame-selected-window))
          (switch-to-buffer rbuf)
          (delete-other-windows (selected-window))
          (with-current-buffer rbuf
            (setq-local org-air-view--rail-popped-out t)
            (setq-local org-air-view--rail-suspended nil)
            (org-air-review--render-current))
          (let ((w2 (split-window (selected-window) nil 'below)))
            (set-window-buffer w2 bbuf)
            (should (eq (window-buffer (selected-window)) rbuf))
            (should (eq (org-air-rail--side-owner) rbuf))
            ;; Review selected ⇒ review keeps the rail.
            (org-air-rail--reconcile-frame (selected-frame))
            (should (eq (org-air-rail--side-owner) rbuf))
            (should-not (buffer-local-value 'org-air-view--rail-suspended
                                            rbuf))
            (should (buffer-local-value 'org-air-view--rail-suspended bbuf))
            ;; No regression: ownership still follows the ACTIVE view.
            (select-window w2)          ; board active now
            (org-air-rail--reconcile-frame (selected-frame))
            (should (eq (org-air-rail--side-owner) bbuf))
            (should (buffer-local-value 'org-air-view--rail-suspended rbuf))
            (should-not (buffer-local-value 'org-air-view--rail-suspended
                                            bbuf))
            ;; …and back: review re-owns on return.
            (select-window (get-buffer-window rbuf (selected-frame)))
            (org-air-rail--reconcile-frame (selected-frame))
            (should (eq (org-air-rail--side-owner) rbuf))
            (should-not (buffer-local-value 'org-air-view--rail-suspended
                                            rbuf))))))))

(ert-deftest org-air-r62-4-rail-toggle-works-in-review ()
  "T4: `|' (`org-air-rail-toggle') works in review; foreign still guarded.
In a popped review the toggle never signals: the first press pops the
rail IN (flag nil, no side window), the second pops it back OUT (flag
t, side window live, owner review) — on the unfixed guard BOTH presses
die with \"Not in an org-air board or project buffer\" (the GUI
Scenario 3).  The guard was WIDENED, not dropped: a fundamental-mode
buffer still gets the `user-error'."
  (skip-unless (locate-library "org-air"))
  (org-air-r62--with-review-rail
    (should (org-air-rail--popped-p))
    ;; Pop IN — a signal here fails the test on the unfixed guard.
    (org-air-rail-toggle)
    (should-not (org-air-rail--popped-p))
    (should-not (org-air-rail--side-window))
    ;; Pop back OUT.
    (org-air-rail-toggle)
    (should (org-air-rail--popped-p))
    (should (window-live-p (org-air-rail--side-window)))
    (should (eq (org-air-rail--side-owner) rbuf)))
  ;; The guard still rejects a foreign buffer.
  (with-temp-buffer
    (should-error (org-air-rail-toggle) :type 'user-error)))

(ert-deftest org-air-r62-5-rail-placement-parity ()
  "T5: review/revisit placement parity — knobs, alist, fallback, no drift.
`org-air-review-rail-placement' and `org-air-revisit-rail-placement'
are real nil-default defcustoms; `org-air-rail--placement' resolves the
per-view knob FIRST (it beats the shared default), the shared symbol
next, the legacy R26-5 alist shape per view, and `side-window' when
everything is nil — for `review' and `revisit' exactly as for
board/project/outline, whose resolution is asserted UNCHANGED.
Reverting R62-1d (the pcase entries or the knobs) FAILS."
  (skip-unless (locate-library "org-air"))
  ;; The two new knobs exist, defcustom'd, nil-default (inherit).
  (dolist (knob '(org-air-review-rail-placement
                  org-air-revisit-rail-placement))
    (should (custom-variable-p knob))
    (should (null (default-value knob))))
  ;; Default resolution: the shared knob (side-window out of the box).
  (should (eq (org-air-rail--placement 'review) 'side-window))
  (should (eq (org-air-rail--placement 'revisit) 'side-window))
  ;; The per-view knob wins — both values, both views.
  (let ((org-air-review-rail-placement 'inline))
    (should (eq (org-air-rail--placement 'review) 'inline)))
  (let ((org-air-revisit-rail-placement 'inline))
    (should (eq (org-air-rail--placement 'revisit) 'inline)))
  (let ((org-air-rail-placement 'inline)
        (org-air-review-rail-placement 'side-window)
        (org-air-revisit-rail-placement 'side-window))
    (should (eq (org-air-rail--placement 'review) 'side-window))
    (should (eq (org-air-rail--placement 'revisit) 'side-window)))
  ;; The shared symbol still reaches both views when the knob is nil.
  (let ((org-air-rail-placement 'inline))
    (should (eq (org-air-rail--placement 'review) 'inline))
    (should (eq (org-air-rail--placement 'revisit) 'inline)))
  ;; The legacy alist shape resolves per view (windows never touched).
  (let ((org-air-rail-placement '((review . inline)
                                  (revisit . side-window)
                                  (board . inline))))
    (should (eq (org-air-rail--placement 'review) 'inline))
    (should (eq (org-air-rail--placement 'revisit) 'side-window))
    (should (eq (org-air-rail--placement 'board) 'inline)))
  ;; Nil everything falls back to side-window.
  (let ((org-air-rail-placement nil))
    (should (eq (org-air-rail--placement 'review) 'side-window))
    (should (eq (org-air-rail--placement 'revisit) 'side-window)))
  ;; No regression: board/project/outline resolve exactly as before.
  (let ((org-air-board-rail-placement 'inline))
    (should (eq (org-air-rail--placement 'board) 'inline)))
  (let ((org-air-project-rail-placement 'inline))
    (should (eq (org-air-rail--placement 'project) 'inline)))
  (let ((org-air-outline-rail-placement 'inline))
    (should (eq (org-air-rail--placement 'outline) 'inline)))
  (let ((org-air-rail-placement 'inline))
    (should (eq (org-air-rail--placement 'board) 'inline))
    (should (eq (org-air-rail--placement 'project) 'inline))))

;;;; -------------------------------------------------------------------
;;;; Item 2 — the range ladder
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r62-6-range-ladder-oracle ()
  "T6: fortnight/quarter/year bounds match the oracle; adjacency; clip.
The fortnight phase is the absolute day of Monday 1970-01-05 (verified
a Monday against `calendar-day-of-week'); every fortnight starts on a
Monday, spans exactly 14 absolute days and is tiled by exactly TWO
consecutive ISO weeks — including across the W53/2026 edge, where the
fixed phase yields [Dec 21, Jan 4) then [Jan 4, Jan 18) with no year
seam (the ISO-year-local pairing the design ruled out would break
here).  Quarter bounds are the calendar quarters with the Q4
December-style rollover and the year-boundary pair; year bounds the
calendar year.  The ADJACENCY LAW — bounds(kind, END) starts at END,
bounds(kind, START−1) ends at START, the anchor lies inside its own
bounds — holds over 5 kinds × 9 edge dates (year/quarter edges, the
W53 Monday, leap Feb 29 2028, the US DST transition Sundays) × 2
timezones.  A 90-minute interval straddling a fortnight/quarter/year
edge clips 3600 + 1800 = 5400 exactly with the adjacent period at 0.
An unknown KIND totals to the week branch, never signalling.
Reverting any new `org-air-review--period-bounds' branch FAILS."
  (skip-unless (locate-library "org-air"))
  ;; The fixed phase: Monday 1970-01-05, as a defconst.
  (should (= (calendar-day-of-week '(1 5 1970)) 1))
  (should (= org-air-review--fortnight-phase
             (calendar-absolute-from-gregorian '(1 5 1970))))
  ;; Fortnight bounds against explicit local-midnight epochs.
  (should (equal (org-air-review--period-bounds
                  'fortnight (org-air-r62--epoch 2026 6 17 12))
                 (cons (org-air-r62--epoch 2026 6 8)
                       (org-air-r62--epoch 2026 6 22))))
  ;; The W53/2026 edge: no year seam, both blocks exactly 14 days.
  (should (equal (org-air-review--period-bounds
                  'fortnight (org-air-r62--epoch 2026 12 28))
                 (cons (org-air-r62--epoch 2026 12 21)
                       (org-air-r62--epoch 2027 1 4))))
  (should (equal (org-air-review--period-bounds
                  'fortnight (org-air-r62--epoch 2027 1 4))
                 (cons (org-air-r62--epoch 2027 1 4)
                       (org-air-r62--epoch 2027 1 18))))
  ;; The fortnight containing New Year 2026 straddles the ISO years.
  (should (equal (org-air-review--period-bounds
                  'fortnight (org-air-r62--epoch 2026 1 1))
                 (cons (org-air-r62--epoch 2025 12 22)
                       (org-air-r62--epoch 2026 1 5))))
  ;; Quarter bounds: Q2, the Q4 December-style rollover, the
  ;; year-boundary pair.
  (should (equal (org-air-review--period-bounds
                  'quarter (org-air-r62--epoch 2026 6 17 12))
                 (cons (org-air-r62--epoch 2026 4 1)
                       (org-air-r62--epoch 2026 7 1))))
  (should (equal (org-air-review--period-bounds
                  'quarter (org-air-r62--epoch 2026 11 15))
                 (cons (org-air-r62--epoch 2026 10 1)
                       (org-air-r62--epoch 2027 1 1))))
  (should (equal (org-air-review--period-bounds
                  'quarter (org-air-r62--epoch 2026 12 31 23))
                 (cons (org-air-r62--epoch 2026 10 1)
                       (org-air-r62--epoch 2027 1 1))))
  (should (equal (org-air-review--period-bounds
                  'quarter (org-air-r62--epoch 2027 1 1))
                 (cons (org-air-r62--epoch 2027 1 1)
                       (org-air-r62--epoch 2027 4 1))))
  ;; Year bounds: the calendar year, both edges inside.
  (should (equal (org-air-review--period-bounds
                  'year (org-air-r62--epoch 2026 6 17 12))
                 (cons (org-air-r62--epoch 2026 1 1)
                       (org-air-r62--epoch 2027 1 1))))
  (should (equal (org-air-review--period-bounds
                  'year (org-air-r62--epoch 2026 12 31 23))
                 (cons (org-air-r62--epoch 2026 1 1)
                       (org-air-r62--epoch 2027 1 1))))
  (should (equal (org-air-review--period-bounds
                  'year (org-air-r62--epoch 2027 1 1))
                 (cons (org-air-r62--epoch 2027 1 1)
                       (org-air-r62--epoch 2028 1 1))))
  ;; The adjacency law over 5 kinds x 9 edge dates x 2 timezones, with
  ;; the Monday/two-ISO-week fortnight structure asserted per date.
  (let ((old-tz (getenv "TZ")))
    (unwind-protect
        (dolist (tz '("UTC" "America/New_York"))
          (setenv "TZ" tz)
          (dolist (date '((2026 1 1) (2027 1 1) (2026 3 31) (2026 4 1)
                          (2026 12 31) (2026 12 28) (2028 2 29)
                          (2026 3 8) (2026 11 1)))
            (let ((anchor (apply #'org-air-r62--epoch date)))
              (dolist (kind '(week fortnight month quarter year))
                (let* ((b (org-air-review--period-bounds kind anchor))
                       (nxt (org-air-review--period-bounds kind (cdr b)))
                       (prv (org-air-review--period-bounds
                             kind (1- (car b)))))
                  ;; The anchor lies inside its own bounds.
                  (should (<= (car b) anchor))
                  (should (< anchor (cdr b)))
                  ;; Period end == next start; START-1's period ends at
                  ;; START — no gap, no overlap.
                  (should (= (car nxt) (cdr b)))
                  (should (= (cdr prv) (car b)))))
              ;; Fortnight structure: a Monday start, 14 absolute days,
              ;; tiled by exactly two consecutive ISO weeks.
              (let* ((b (org-air-review--period-bounds 'fortnight anchor))
                     (d (decode-time (car b)))
                     (greg (list (decoded-time-month d)
                                 (decoded-time-day d)
                                 (decoded-time-year d)))
                     (abs (calendar-absolute-from-gregorian greg))
                     (mid (org-air-review--date-epoch
                           (calendar-gregorian-from-absolute (+ abs 7))))
                     (w1 (org-air-review--period-bounds 'week (car b)))
                     (w2 (org-air-review--period-bounds 'week mid)))
                (should (= (calendar-day-of-week greg) 1))
                (should (= (car w1) (car b)))
                (should (= (cdr w1) mid))
                (should (= (car w2) mid))
                (should (= (cdr w2) (cdr b)))))))
      (setenv "TZ" old-tz)))
  ;; Clip complementarity at the three NEW edges: 60 + 30 = 90 exact,
  ;; the adjacent period 0.
  (pcase-dolist (`(,edge ,p-1 ,p+1 ,adj0 ,adj1)
                 (list (list (org-air-r62--epoch 2026 6 22)  ; fortnight
                             (org-air-r62--epoch 2026 6 8)
                             (org-air-r62--epoch 2026 7 6)
                             (org-air-r62--epoch 2026 5 25)
                             (org-air-r62--epoch 2026 6 8))
                       (list (org-air-r62--epoch 2026 7 1)   ; quarter
                             (org-air-r62--epoch 2026 4 1)
                             (org-air-r62--epoch 2026 10 1)
                             (org-air-r62--epoch 2026 1 1)
                             (org-air-r62--epoch 2026 4 1))
                       (list (org-air-r62--epoch 2027 1 1)   ; year
                             (org-air-r62--epoch 2026 1 1)
                             (org-air-r62--epoch 2028 1 1)
                             (org-air-r62--epoch 2025 1 1)
                             (org-air-r62--epoch 2026 1 1))))
    (let ((s (- edge 3600))
          (e (+ edge 1800)))
      (should (= (org-air-review--clip s e p-1 edge) 3600))
      (should (= (org-air-review--clip s e edge p+1) 1800))
      (should (= (+ (org-air-review--clip s e p-1 edge)
                    (org-air-review--clip s e edge p+1))
                 5400))
      (should (= (org-air-review--clip s e adj0 adj1) 0))))
  ;; An unknown kind totals to the week branch — never signals.
  (let ((e (org-air-r62--epoch 2026 6 17 12)))
    (should (equal (org-air-review--period-bounds 'decade e)
                   (org-air-review--period-bounds 'week e)))))

(defconst org-air-r62--nav-specs
  '(("inbox.org" . "* TODO Inbox capture\n")
    ("work.org" . "\
* TODO Deep work :work:
:LOGBOOK:
CLOCK: [2026-06-16 Tue 09:00]--[2026-06-16 Tue 11:00] =>  2:00
:END:
"))
  "Minimal live corpus for the range-navigation ERTs.")

(ert-deftest org-air-r62-7-nav-steps-by-active-unit ()
  "T7: `<'/`>' step by ONE unit of the ACTIVE range, per rung.
At the command level, for EACH of the five ranges (adopted by real `+'
presses): `.' shows the CURRENT period whose explicit epoch bounds pin
the rung's unit — one ISO week, one 14-day fortnight, one calendar
month, one 3-MONTH quarter, one calendar year; `>' lands on the bounds
STARTING exactly at the previous half-open END (a quarter step moves
three months, a year step one year — pinned by explicit next-period
epochs); `<' returns byte-identical bounds; `.' clears the anchor.
ZERO changes to the nav commands were needed — this is the partition
law observable at the keys; reverting any bounds branch FAILS its
rung."
  (skip-unless (locate-library "org-air"))
  (org-air-r62--with-corpus org-air-r62--nav-specs
    (org-air-r62--frozen-at org-air-r62--now
      (org-air-review)
      (with-current-buffer org-air-review-buffer-name
        (should (eq org-air-review--period-kind 'week))
        (pcase-dolist (`(,kind ,c0 ,c1 ,n1)
                       (list (list 'week
                                   (org-air-r62--epoch 2026 6 15)
                                   (org-air-r62--epoch 2026 6 22)
                                   (org-air-r62--epoch 2026 6 29))
                             (list 'fortnight
                                   (org-air-r62--epoch 2026 6 8)
                                   (org-air-r62--epoch 2026 6 22)
                                   (org-air-r62--epoch 2026 7 6))
                             (list 'month
                                   (org-air-r62--epoch 2026 6 1)
                                   (org-air-r62--epoch 2026 7 1)
                                   (org-air-r62--epoch 2026 8 1))
                             (list 'quarter
                                   (org-air-r62--epoch 2026 4 1)
                                   (org-air-r62--epoch 2026 7 1)
                                   (org-air-r62--epoch 2026 10 1))
                             (list 'year
                                   (org-air-r62--epoch 2026 1 1)
                                   (org-air-r62--epoch 2027 1 1)
                                   (org-air-r62--epoch 2028 1 1))))
          (should (eq org-air-review--period-kind kind))
          ;; `.': the current period of the ACTIVE range.
          (org-air-review-period-today)
          (should (equal (org-air-review--bounds) (cons c0 c1)))
          ;; `>': one unit forward — starts at the half-open END.
          (org-air-review-period-next)
          (should (equal (org-air-review--bounds) (cons c1 n1)))
          ;; `<': byte-identical bounds back.
          (org-air-review-period-prev)
          (should (equal (org-air-review--bounds) (cons c0 c1)))
          ;; `.': the anchor clears.
          (org-air-review-period-today)
          (should-not org-air-review--period-anchor)
          (should (equal (org-air-review--bounds) (cons c0 c1)))
          ;; Adopt the next rung with a real `+' press.
          (unless (eq kind 'year)
            (org-air-review-range-widen)))))))

(ert-deftest org-air-r62-8-ladder-keys-and-no-rescan ()
  "T8: `+'/`-'/`m' walk the ladder; the whole burst never rescans.
The keys resolve in `org-air-review-mode-map' (`+' and its unshifted
alias `=' → widen, `-' → narrow, `m' → cycle, plus `.'/`<'/`>'); from
the current week `+' × 4 walks fortnight → month → quarter → year with
the nil anchor STAYING nil (every rung tracks the current period) and
each rung showing its exact current bounds; a fifth `+' CLAMPS —
\"widest range\" messaged, kind and bounds unmoved; `-' walks back down
to week and clamps with \"narrowest range\"; `m' × 5 wraps the full
cycle back to week.  An ABSOLUTE anchor (one `<' press → Jun 8) is
preserved across the whole widen walk with every rung's period
CONTAINING it.  The knob: unknown symbols drop, an empty result
degrades to (week month), a (week month) trim makes `m' the old R61
toggle, and an off-ladder current kind still narrows out at its
natural rank.  EVERYTHING above runs under `org-air-query--scan-file'
AND corpus `insert-file-contents' spies at ZERO calls — range change
is filter+fold only (the R61 law as the R62 design invariant).
Reverting the commands, the ladder validation or sneaking a scan into
the repaint FAILS."
  (skip-unless (locate-library "org-air"))
  ;; The bindings — legend-truth at the map level.
  (should (eq (lookup-key org-air-review-mode-map (kbd "+"))
              #'org-air-review-range-widen))
  (should (eq (lookup-key org-air-review-mode-map (kbd "="))
              #'org-air-review-range-widen))
  (should (eq (lookup-key org-air-review-mode-map (kbd "-"))
              #'org-air-review-range-narrow))
  (should (eq (lookup-key org-air-review-mode-map (kbd "m"))
              #'org-air-review-cycle-range))
  (should (eq (lookup-key org-air-review-mode-map (kbd "."))
              #'org-air-review-period-today))
  (should (eq (lookup-key org-air-review-mode-map (kbd "<"))
              #'org-air-review-period-prev))
  (should (eq (lookup-key org-air-review-mode-map (kbd ">"))
              #'org-air-review-period-next))
  ;; The ladder knob's blessed default is the full five, ladder order.
  (should (equal org-air-review--range-ladder
                 '(week fortnight month quarter year)))
  (should (equal (eval (car (get 'org-air-review-ranges 'standard-value)))
                 '(week fortnight month quarter year)))
  (org-air-r62--with-corpus org-air-r62--nav-specs
    (org-air-r62--frozen-at org-air-r62--now
      (org-air-review)
      (with-current-buffer org-air-review-buffer-name
        (let ((scans 0) (reads 0))
          (org-air-r62--spying-scans scans
            (org-air-r62--spying-reads reads
              ;; Nil anchor: the widen walk tracks the CURRENT period.
              (should (eq org-air-review--period-kind 'week))
              (should-not org-air-review--period-anchor)
              (pcase-dolist (`(,kind ,c0 ,c1)
                             (list (list 'fortnight
                                         (org-air-r62--epoch 2026 6 8)
                                         (org-air-r62--epoch 2026 6 22))
                                   (list 'month
                                         (org-air-r62--epoch 2026 6 1)
                                         (org-air-r62--epoch 2026 7 1))
                                   (list 'quarter
                                         (org-air-r62--epoch 2026 4 1)
                                         (org-air-r62--epoch 2026 7 1))
                                   (list 'year
                                         (org-air-r62--epoch 2026 1 1)
                                         (org-air-r62--epoch 2027 1 1))))
                (org-air-review-range-widen)
                (should (eq org-air-review--period-kind kind))
                (should-not org-air-review--period-anchor)
                (should (equal (org-air-review--bounds) (cons c0 c1))))
              ;; Clamped at the widest rung: message, no move.
              (let ((bounds (org-air-review--bounds)))
                (org-air-r62--capturing-messages
                  (org-air-review-range-widen)
                  (should (string-match-p "widest range"
                                          (car org-air-r62--messages))))
                (should (eq org-air-review--period-kind 'year))
                (should (equal (org-air-review--bounds) bounds)))
              ;; `-' walks back down and clamps at the narrowest.
              (dolist (kind '(quarter month fortnight week))
                (org-air-review-range-narrow)
                (should (eq org-air-review--period-kind kind)))
              (org-air-r62--capturing-messages
                (org-air-review-range-narrow)
                (should (string-match-p "narrowest range"
                                        (car org-air-r62--messages))))
              (should (eq org-air-review--period-kind 'week))
              ;; `m' x 5 wraps the full cycle back to week.
              (dolist (kind '(fortnight month quarter year week))
                (org-air-review-cycle-range)
                (should (eq org-air-review--period-kind kind)))
              ;; An ABSOLUTE anchor survives the whole widen walk, the
              ;; shown period always CONTAINING the anchor day (Jun 8).
              (org-air-review-period-prev)   ; anchor := Jun 8 (W24)
              (let ((anchor (org-air-r62--epoch 2026 6 8)))
                (should (equal org-air-review--period-anchor anchor))
                (dolist (kind '(fortnight month quarter year))
                  (org-air-review-range-widen)
                  (should (eq org-air-review--period-kind kind))
                  (should (equal org-air-review--period-anchor anchor))
                  (let ((b (org-air-review--bounds)))
                    (should (<= (car b) anchor))
                    (should (< anchor (cdr b))))))
              (org-air-review-period-today)  ; anchor clears
              ;; The trimmed ladder: only its members cycle — the
              ;; (week month) trim IS the old R61 toggle.
              (let ((org-air-review-ranges '(month bogus week)))
                (should (equal (org-air-review--effective-ranges)
                               '(week month)))
                (dotimes (_ 4) (org-air-review-range-narrow))
                (should (eq org-air-review--period-kind 'week))
                (org-air-review-cycle-range)
                (should (eq org-air-review--period-kind 'month))
                (org-air-review-cycle-range)
                (should (eq org-air-review--period-kind 'week))
                ;; An off-ladder current kind still narrows out at its
                ;; natural rank (quarter sits above month).
                (setq-local org-air-review--period-kind 'quarter)
                (org-air-review--render-current)
                (should (equal (org-air-review--effective-ranges 'quarter)
                               '(week month quarter)))
                (org-air-review-range-narrow)
                (should (eq org-air-review--period-kind 'month)))
              ;; Validation shapes: unknown drops, empty degrades.
              (let ((org-air-review-ranges '(bogus)))
                (should (equal (org-air-review--effective-ranges)
                               '(week month))))
              (let ((org-air-review-ranges nil))
                (should (equal (org-air-review--effective-ranges)
                               '(week month))))
              (let ((org-air-review-ranges '(year week)))
                (should (equal (org-air-review--effective-ranges)
                               '(week year))))))
          ;; The whole burst: ZERO scans, ZERO corpus file opens.
          (should (= scans 0))
          (should (= reads 0)))))))

(defconst org-air-r62--bookmark-specs
  '(("inbox.org" . "* TODO Inbox capture\n")
    ("work.org" . "\
* TODO Alpha done :work:
:PROPERTIES:
:CREATED: [2025-05-01 Thu 08:00]
:END:
:LOGBOOK:
- State \"DONE\"       from \"TODO\"       [2026-02-10 Tue 10:00]
:END:
* TODO Beta done :work:
:PROPERTIES:
:CREATED: [2025-05-01 Thu 08:00]
:END:
:LOGBOOK:
- State \"DONE\"       from \"TODO\"       [2026-02-10 Tue 11:00]
:END:
"))
  "T9 corpus: two SAME-DAY Q1-2026 completions (one day group, so the
title sort alone orders the rows); `:CREATED:' outside the quarter.")

(ert-deftest org-air-r62-9-bookmark-carries-range ()
  "T9: the bookmark record carries the range; the round-trip restores it.
A QUARTER view navigated to Q1 2026 with point on a NON-first row
records `(quarter . <Jan 1 epoch>)' in the unchanged (KIND . ANCHOR)
shape with the period-qualified default name \"… · Q1 2026\"; the
record survives prin1→read `equal'; after killing the buffer the
autoloaded handler ALONE rebuilds the surface from the CACHE (zero
scans spied), undisplayed (zero display calls, window fingerprint
unchanged), restoring kind + anchor + bounds and landing point on the
bookmarked row.  An R61-shape week record applies unchanged; a KIND
`decade' record on a FRESH buffer degrades to the default current week
without signalling; a `current'-anchor quarter record restored under a
clock advanced to Q4 tracks the NEW current quarter.  Reverting the
apply-side `memq' widening FAILS the quarter restore (the R61 memq
degrades it to the current week)."
  (skip-unless (locate-library "org-air"))
  (org-air-r62--with-corpus org-air-r62--bookmark-specs
    (org-air-r62--frozen-at org-air-r62--now
      (let ((q1-0 (org-air-r62--epoch 2026 1 1))
            (q1-1 (org-air-r62--epoch 2026 4 1))
            record reread items)
        ;; Compose the navigated quarter view; bookmark the SECOND row.
        (org-air-review)
        (with-current-buffer org-air-review-buffer-name
          (setq items org-air-review--items)
          (dotimes (_ 3) (org-air-review-cycle-range))  ; week → quarter
          (should (eq org-air-review--period-kind 'quarter))
          (org-air-review-period-prev)                  ; Q2 → Q1 2026
          (should (equal (org-air-review--bounds) (cons q1-0 q1-1)))
          (setq-local org-air-view--sort-key 'title)
          (setq-local org-air-view--sort-direction 'descending)
          (org-air-review--render-current)
          ;; Title-descending ⇒ Beta first: the landing assert below
          ;; can never pass off the render's first-row default.
          (should (equal (org-air-r62--review-row-titles)
                         '("Beta done" "Alpha done")))
          (org-air-r62--goto-review-row "Alpha done")
          (setq record (bookmark-make-record))
          (should (equal (org-air-r62--field record 'org-air-period)
                         (cons 'quarter q1-0)))
          (should (equal (car (org-air-r62--field record 'defaults))
                         "org-air: review · Q1 2026"))
          (setq reread (org-air-r62--roundtrip record)))
        ;; Persist the cache so the jump can rebuild CACHE-FIRST.
        (org-air-view--cache-write
         items (org-air-view--mtimes-snapshot (org-air-query-files)))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer org-air-review-buffer-name))
        ;; The handler alone: zero scans, zero display, range restored.
        (let ((scans 0))
          (org-air-r62--asserting-no-display
            (org-air-r62--spying-scans scans
              (org-air-review-bookmark-jump reread)))
          (should (= scans 0)))
        (should (eq (current-buffer)
                    (get-buffer org-air-review-buffer-name)))
        (with-current-buffer org-air-review-buffer-name
          (should (eq org-air-review--period-kind 'quarter))
          (should (equal org-air-review--period-anchor q1-0))
          (should (equal (org-air-review--bounds) (cons q1-0 q1-1)))
          ;; Point on the bookmarked row — NOT the first-row default.
          (should (equal (org-air-r62--review-row-titles)
                         '("Beta done" "Alpha done")))
          (let ((item (org-air-view--row-property 'org-air-item)))
            (should item)
            (should (equal (org-air-item-title item) "Alpha done"))))
        ;; An R61-shape week record still applies (compat both ways).
        (org-air-review-bookmark-jump
         `((handler . org-air-review-bookmark-jump)
           (location . "org-air: review")
           (org-air-view . review)
           (org-air-period . (week . ,(org-air-r62--epoch 2026 6 8)))))
        (with-current-buffer org-air-review-buffer-name
          (should (eq org-air-review--period-kind 'week))
          (should (equal (org-air-review--bounds)
                         (cons (org-air-r62--epoch 2026 6 8)
                               (org-air-r62--epoch 2026 6 15)))))
        ;; KIND `decade' on a FRESH buffer degrades to the default
        ;; current week — no signal.
        (let ((kill-buffer-query-functions nil))
          (kill-buffer org-air-review-buffer-name))
        (org-air-review-bookmark-jump
         `((handler . org-air-review-bookmark-jump)
           (location . "org-air: review")
           (org-air-view . review)
           (org-air-period . (decade . ,q1-0))))
        (with-current-buffer org-air-review-buffer-name
          (should (eq org-air-review--period-kind 'week))
          (should-not org-air-review--period-anchor)
          (should (equal (org-air-review--bounds)
                         (cons (org-air-r62--epoch 2026 6 15)
                               (org-air-r62--epoch 2026 6 22))))
          (should org-air-review--items))))
    ;; The `current' anchor at QUARTER scale: recorded on the current
    ;; quarter, restored a season later, tracking the NEW quarter.
    (let (record)
      (org-air-r62--frozen-at org-air-r62--now
        (let ((kill-buffer-query-functions nil))
          (when (get-buffer org-air-review-buffer-name)
            (kill-buffer org-air-review-buffer-name)))
        (org-air-review)
        (with-current-buffer org-air-review-buffer-name
          (dotimes (_ 3) (org-air-review-cycle-range))  ; week → quarter
          (should-not org-air-review--period-anchor)
          (setq record (bookmark-make-record))
          (should (equal (org-air-r62--field record 'org-air-period)
                         '(quarter . current))))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer org-air-review-buffer-name)))
      (org-air-r62--frozen-at (org-air-r62--epoch 2026 10 15 10)
        (org-air-review-bookmark-jump (org-air-r62--roundtrip record))
        (with-current-buffer org-air-review-buffer-name
          (should (eq org-air-review--period-kind 'quarter))
          (should-not org-air-review--period-anchor)
          (should (equal (org-air-review--bounds)
                         (cons (org-air-r62--epoch 2026 10 1)
                               (org-air-r62--epoch 2027 1 1)))))))))

(ert-deftest org-air-r62-10-labels-and-legend-truth ()
  "T10: the five label shapes; the four-row legend never drifts.
Under the C locale: week \"W25 2026\" (+ day range), fortnight
\"W24–25 2026\" (+ day range), month \"June 2026\", quarter \"Q2 2026\"
\(+ \"Apr – Jun\"), year \"2026\"; a fortnight straddling the ISO years
qualifies BOTH (\"W52 2025–W1 2026\") while the W53/2026 pair stays
single-year \"W52–53 2026\" — the second week's number comes from
calendar arithmetic on P0+7d, never first-week + 1, so W52/W53
pairings label correctly.  The Actions legend is FOUR rows of three;
every KEY resolves via `key-binding' in the live review buffer to a
command that is never a bare prefix map (the R26/R50-1 discipline);
`.' joined the legend, `+' widen and `-' narrow are listed, `=' is
bound to widen yet appears in NO cell (the legend-less alias); the
rendered Actions block carries the new cells.  Plus the R62-2 calendar
centring refinement: today's month iff today ∈ [P0, P1), else P0's
month.  Reverting the label branches, the legend rows or the centring
FAILS."
  (skip-unless (locate-library "org-air"))
  (let ((system-time-locale "C"))
    ;; Short labels at frozen period starts.
    (should (equal (org-air-review--period-short-label
                    'week (org-air-r62--epoch 2026 6 15))
                   "W25 2026"))
    (should (equal (org-air-review--period-short-label
                    'fortnight (org-air-r62--epoch 2026 6 8))
                   "W24–25 2026"))
    (should (equal (org-air-review--period-short-label
                    'month (org-air-r62--epoch 2026 6 1))
                   "June 2026"))
    (should (equal (org-air-review--period-short-label
                    'quarter (org-air-r62--epoch 2026 4 1))
                   "Q2 2026"))
    (should (equal (org-air-review--period-short-label
                    'year (org-air-r62--epoch 2026 1 1))
                   "2026"))
    ;; Full header labels.
    (should (equal (org-air-review--period-label
                    'week (org-air-r62--epoch 2026 6 15)
                    (org-air-r62--epoch 2026 6 22))
                   (format "W25 2026%sJun 15 – Jun 21"
                           (org-air-view--sep))))
    (should (equal (org-air-review--period-label
                    'fortnight (org-air-r62--epoch 2026 6 8)
                    (org-air-r62--epoch 2026 6 22))
                   (format "W24–25 2026%sJun 8 – Jun 21"
                           (org-air-view--sep))))
    (should (equal (org-air-review--period-label
                    'month (org-air-r62--epoch 2026 6 1)
                    (org-air-r62--epoch 2026 7 1))
                   "June 2026"))
    (should (equal (org-air-review--period-label
                    'quarter (org-air-r62--epoch 2026 4 1)
                    (org-air-r62--epoch 2026 7 1))
                   (format "Q2 2026%sApr – Jun" (org-air-view--sep))))
    (should (equal (org-air-review--period-label
                    'year (org-air-r62--epoch 2026 1 1)
                    (org-air-r62--epoch 2027 1 1))
                   "2026"))
    ;; The cross-ISO-year fortnight qualifies BOTH years; the W53/2026
    ;; pair stays single-year (weeks 52 and 53 of ISO 2026).
    (should (equal (org-air-review--period-short-label
                    'fortnight (org-air-r62--epoch 2025 12 22))
                   "W52 2025–W1 2026"))
    (should (equal (org-air-review--period-short-label
                    'fortnight (org-air-r62--epoch 2026 12 21))
                   "W52–53 2026")))
  ;; The calendar centring refinement (pure, frozen clock).
  (org-air-r62--frozen-at org-air-r62--now
    ;; Today inside the period (the current year): centre on TODAY.
    (should (equal (org-air-review--calendar-month
                    (org-air-r62--epoch 2026 1 1)
                    (org-air-r62--epoch 2027 1 1))
                   (seconds-to-time org-air-r62--now)))
    ;; Today outside (a past year): centre on P0 as before.
    (should (equal (org-air-review--calendar-month
                    (org-air-r62--epoch 2020 1 1)
                    (org-air-r62--epoch 2021 1 1))
                   (seconds-to-time (org-air-r62--epoch 2020 1 1)))))
  ;; The legend table: FOUR rows of three, the new cells present, and
  ;; `=' nowhere in it.
  (should (= (length org-air-review--actions-table) 4))
  (dolist (row org-air-review--actions-table)
    (should (= (length row) 3)))
  (let ((cells (apply #'append org-air-review--actions-table)))
    (dolist (cell '(("m" . "span") ("+" . "widen") ("-" . "narrow")
                    ("." . "today")))
      (should (member cell cells)))
    (should-not (assoc "=" cells)))
  ;; Legend truth on the LIVE surface: every key resolves to a command.
  (org-air-r62--with-corpus org-air-r62--nav-specs
    (org-air-r62--frozen-at org-air-r62--now
      (org-air-review)
      (with-current-buffer org-air-review-buffer-name
        (dolist (row org-air-review--actions-table)
          (dolist (cell row)
            (let ((cmd (key-binding (kbd (car cell)))))
              (should cmd)
              (should (commandp cmd))
              ;; R50-1: a legend key must never be a bare prefix map.
              (should-not (keymapp cmd)))))
        ;; `=' is the bound-but-unlisted alias.
        (should (eq (key-binding (kbd "=")) #'org-air-review-range-widen))
        ;; The rendered Actions block carries the four-row shape.
        (let ((text (with-temp-buffer
                      (org-air-review--insert-actions 40)
                      (buffer-substring-no-properties (point-min)
                                                      (point-max)))))
          (should (string-match-p "m +span" text))
          (should (string-match-p "\\+ +widen" text))
          (should (string-match-p "- +narrow" text))
          (should (string-match-p "\\. +today" text))
          (should-not (string-match-p "=" text)))))))

(provide 'org-air-round62-test)
;;; org-air-round62-test.el ends here
