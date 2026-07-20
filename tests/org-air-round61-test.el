;;; org-air-round61-test.el --- executing ERTs for v0.5 round-61 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.5 round-61 (air/v0.5/org-air-round61-design.org):
;; org-air-review — the retrospective surface ("what happened this week /
;; month").  R61-1 the same-pass CLOCK/LOGBOOK/`:CREATED:' harvest
;; (own-body, never-error, capped by `org-air-log-cap'); R61-2 cache v6
;; with `org-air-log-cap' as the SIXTH `org-air-view--cache-key' element;
;; R61-3 the pure ISO-week/month period engine (cal-iso bounds, EXACT
;; boundary clipping, the four section predicates, the suspect-clock
;; rule); R61-4 the new module org-air-review.el (period nav `<'/`>'/`.',
;; week↔month `m', rollup `f', filter/scope reuse); R61-6 R58 bookmark
;; parity.
;;
;; All BATCH/headless over temp-dir corpora through the REAL scan
;; (`org-air-query-items' / `org-air-query--scan-file'), the real fold
;; (`org-air-review--section-data' — exactly what the render consumes)
;; and the real surface (`(org-air-review)' + its commands).  The spec's
;; ERT seams T1-T14 map onto thirteen ERTs; revert of each FAILS:
;;
;;   r61-1  (T1) SAME PASS + OWN BODY: a heading's closed CLOCKs, its
;;          LOGBOOK state/note stamps and `:CREATED:' land in the
;;          clocks/logs/created slots as INTEGER epochs + interned
;;          symbols, newest-first, in ONE scan pass — an
;;          `insert-file-contents' spy proves exactly ONE read per
;;          corpus file (the harvest opens no second file, ever); the
;;          harvest is bounded to the heading's OWN body: a parent
;;          NEVER absorbs its child's clock (a subtree-wide harvest
;;          double-counts and FAILS); an R53 P3 'file blob item carries
;;          nil review slots.  Reverting the harvest (nil slots) FAILS.
;;   r61-2  (T2) NEVER-ERROR: a salted buffer — a RUNNING (unclosed)
;;          clock, a `CLOCK: [garbage' fragment, a `[not-a-date]' state
;;          stamp — scans without aborting: the good lines of the SAME
;;          heading are harvested, the bad lines are dropped, the item
;;          is present; a heading whose harvest is FORCED to signal (a
;;          poisoned stamp) degrades to nil review slots while the item
;;          is still built, the file still scans, the skip log stays
;;          EMPTY and nothing is echoed.  Reverting the per-heading
;;          inner net FAILS (the file dies wholesale).
;;   r61-3  (T11) THE CAP: cap+3 clock lines under `org-air-log-cap' 5
;;          retain EXACTLY 5, NEWEST kept (the oldest stamps absent),
;;          `rtrunc' t; the rendered Completed row carries the
;;          "⚠ history truncated" marker; a truncated heading NEVER
;;          claims Started through the earliest-stamp fallback while an
;;          untruncated twin does (anti-vacuous).
;;   r61-4  (T14) PERIOD ENGINE vs the cal-iso oracle: ISO week bounds
;;          (Monday 00:00 → next Monday 00:00, half-open) and month
;;          bounds (1st → next 1st, December → January rollover) match
;;          independently-constructed local-midnight epochs; the
;;          year-boundary ISO weeks (2026-01-01 → W1/2026, 2027-01-01 →
;;          W53/2026, its week = Dec 28 2026 … Jan 4 2027); the
;;          week↔month toggle keeps the anchor day BOTH directions and
;;          `.' returns to the current period after navigation.
;;   r61-5  (T3) CLIP EXACTNESS: a 90-minute CLOCK across a week
;;          boundary contributes 60 min to the earlier and 30 min to
;;          the later week (sum EXACTLY 90 — never dropped, never
;;          double-counted; the adjacent week gets 0); same at a month
;;          edge; the day-bounds engine covers any week with days that
;;          sum exactly to the period span (the DST-exact integer law).
;;   r61-6  (T5) DONE-SET RESPECTED (R57): under a user global that
;;          declares CLOSED after the bar (file has no `#+TODO:'), a
;;          `- State "CLOSED"' stamp harvests KIND `done' and the item
;;          lists under Completed — while a `- State "DONE"' stamp
;;          under that SAME vocabulary harvests `todo' (DONE is not in
;;          the user's done set).  A CLOSED: planning stamp is the
;;          fallback when NO done stamps exist; when both exist the
;;          stamps WIN (one row, no double count).  Reverting to a
;;          hard-wired DONE set FAILS both directions.
;;   r61-7  (T4) ROLLUPS: two dirs × two files × tags — by-tag /
;;          by-directory / by-origin sums are exact integers; a two-tag
;;          heading contributes its FULL time to EACH of its tag rows;
;;          the section headline total equals the item fold and
;;          DIFFERS from the Σ of tag rows in this fixture (the lens
;;          rule).
;;   r61-8  (T8 + T13) CACHE v6 + NO RESCAN: a crafted v5 cache is a
;;          clean cold miss; a v6 write→read round-trip preserves all
;;          four review slots `equal'; `org-air-log-cap' participates
;;          in `org-air-view--cache-key' (sixth element, tracks the
;;          live value, a flip refuses the stale cache); twelve `<'
;;          presses + `m'/`>'/`.'/`f' under `org-air-query--scan-file'
;;          AND `insert-file-contents' spies ⇒ ZERO calls (period
;;          navigation is a filter+fold — data-pure); the render from a
;;          live scan and from a cache hydrate are byte-identical at a
;;          fixed width (warm parity), the hydrate spied at zero scans.
;;   r61-9  (T6) STARTED + CARRIED: Started = `created' ∈ P (the
;;          out-of-period twin is out), fallback = earliest retained
;;          stamp (rtrunc-gated, see r61-3); Carried = activity in P ∧
;;          not-done at P's END — done AFTER P-end ⇒ carried in P; done
;;          IN P ⇒ not carried; an unlogged item with clock activity in
;;          the CURRENT period + live `donep' nil ⇒ carried, `donep' t
;;          ⇒ not.  Each clause reverts independently.
;;   r61-10 (T7) SUSPECT CLOCK: a 17 h interval is EXCLUDED from every
;;          total and surfaced on the "⚠ N suspect clock(s), H:MM
;;          excluded" line with the owning heading named; a 15 h
;;          interval is summed; flipping the threshold (20 h / nil)
;;          repaints with the interval included — under spies proving
;;          ZERO rescans (render-time only).
;;   r61-11 (T10) INHERITANCE: R60-excluding a clocked file removes its
;;          time and rows everywhere (anti-vacuous baseline included);
;;          an R59 container with a clocked child never appears in
;;          Completed/Started/Carried (its own done stamp and CREATED
;;          notwithstanding) while the child's time attributes to the
;;          child; the container's OWN-body clocks still count in Time
;;          invested at EXACTLY their own value — a subtree-wide
;;          harvest double-counts the child and FAILS; the knob-nil
;;          twin re-admits the container (anti-tautology).
;;   r61-12 (T12) ×N CHIP: a habit with 7 done stamps in the week
;;          renders ONE Completed row with "×7", day-grouped under the
;;          LATEST stamp's day.
;;   r61-13 (T9) BOOKMARK (R58 parity): the record is printable
;;          (prin1→read `equal') with period kind/anchor + rollup +
;;          filter + scope + sort + the (FILE . POS) locator; the
;;          autoloaded `org-air-review-bookmark-jump' rebuilds the
;;          surface CACHE-FIRST (zero scans spied), CURRENT buffer,
;;          point on the bookmarked row (NOT the first), windows
;;          untouched (fingerprint + display spies at zero); a record
;;          made on the CURRENT period stores the symbol `current' and,
;;          restored a week later (frozen clock advanced), shows the
;;          NEW current period; a malformed record degrades to a plain
;;          review without signalling; handler metadata (fboundp +
;;          `bookmark-handler-type' + the literal `;;;###autoload'
;;          cookie) present.
;;
;; REVERT-FAIL: every ERT above is red against the pre-R61 trunk by
;; construction — the clocks/logs/created/rtrunc accessors,
;; `org-air-log-cap', the org-air-review module and its handler do not
;; exist there; the key is 5 elements and the cache version 5.

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

(defvar org-air-log-cap)
(defvar org-air-cache-file)
(defvar org-air-exclude-regexps)
(defvar org-air-skip-container-headings)
(defvar org-air-review-suspect-clock-hours)

;;;; -------------------------------------------------------------------
;;;; Corpus scaffolding
;;;; -------------------------------------------------------------------

(defvar org-air-r61--dir nil
  "The temp corpus directory of the current `org-air-r61--with-corpus'.")

(defun org-air-r61--reset-tables ()
  "Clear the GLOBAL query-layer tables (file-meta / visits / denote ids).
Session globals are never cleared by a scan; every test starts and ends
empty so absolute temp paths from another test can never leak."
  (clrhash org-air-query--file-meta)
  (clrhash org-air-query--visits)
  (clrhash org-air-query--denote-id-index)
  (setq org-air-query--link-graph-dirty nil))

(defmacro org-air-r61--with-corpus (specs &rest body)
  "Create a temp Org corpus from SPECS and run BODY against it.
SPECS is a list of (NAME . CONTENT) files written into a fresh temp
directory (subdirectories created; the root is TRUENAMED so path
spellings are stable).  Binds `org-air-files' to the root,
`org-air-inbox-file' to its inbox.org, nil `org-air-exclude-regexps',
a temp `org-air-cache-file', a nil `bookmark-alist' and the 120x40
batch viewport; wraps BODY in `save-window-excursion'.  Starts from
EMPTY query tables and cleans up the tables, the scan work buffer,
every org-air view buffer, every corpus-visiting buffer and the
directory."
  (declare (indent 1) (debug t))
  `(let* ((org-air-r61--dir (file-truename (make-temp-file "org-air-r61-" t))))
     (unwind-protect
         (progn
           (org-air-r61--reset-tables)
           (pcase-dolist (`(,name . ,content) ,specs)
             (let ((path (expand-file-name name org-air-r61--dir))
                   (coding-system-for-write 'utf-8-unix))
               (make-directory (file-name-directory path) t)
               (write-region (or content "") nil path nil 'silent)))
           (let ((org-air-files (list org-air-r61--dir))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-r61--dir))
                 (org-air-exclude-regexps nil)
                 (org-air-cache-file
                  (expand-file-name ".cache/board.eld" org-air-r61--dir))
                 (org-air-view-width 120)
                 (org-air-view-height 40)
                 (bookmark-alist nil))
             (save-window-excursion
               ,@body)))
       (org-air-query-teardown)
       (org-air-r61--reset-tables)
       (let ((kill-buffer-query-functions nil))
         (dolist (name (list org-air-review-buffer-name
                             org-air-view-buffer-name
                             org-air-rail-buffer-name))
           (when (get-buffer name)
             (kill-buffer name)))
         (dolist (buf (buffer-list))
           (let ((fn (buffer-file-name buf)))
             (when (and fn (string-prefix-p org-air-r61--dir fn))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r61--dir t))))

(defun org-air-r61--file (name)
  "Return the absolute path of corpus file NAME."
  (expand-file-name name org-air-r61--dir))

(defun org-air-r61--item (title items)
  "Return the item in ITEMS whose title contains TITLE; assert it exists."
  (let ((item (org-air-test-find-item title items)))
    (should item)
    item))

(defun org-air-r61--epoch (y m d &optional hh mm)
  "Return the LOCAL integer epoch of Y-M-D HH:MM (defaults midnight).
The tests' independent oracle: built with `encode-time' on local
calendar dates exactly like the period engine's boundaries, so the
comparisons are TZ-independent."
  (floor (float-time (encode-time (list 0 (or mm 0) (or hh 0)
                                        d m y nil -1 nil)))))

(defconst org-air-r61--now (org-air-r61--epoch 2026 6 15 10)
  "The frozen \"now\": Mon 2026-06-15 10:00 local — inside ISO W25 2026.
The current week under it is [Jun 15, Jun 22), the current month June.")

(defmacro org-air-r61--frozen-at (epoch &rest body)
  "Run BODY with the Lisp-visible clock frozen to integer EPOCH.
Overrides both `current-time' AND no-arg `float-time' (the review
engine's \"now\" reads); `float-time' WITH an argument passes through
untouched, so timestamp parsing and period math stay real."
  (declare (indent 1) (debug t))
  `(let ((org-air-r61--frozen ,epoch))
     (cl-letf* ((org-air-r61--real-ft (symbol-function 'float-time))
                ((symbol-function 'float-time)
                 (lambda (&optional time)
                   (if time (funcall org-air-r61--real-ft time)
                     (float org-air-r61--frozen))))
                ((symbol-function 'current-time)
                 (lambda () (seconds-to-time org-air-r61--frozen))))
       ,@body)))

(defun org-air-r61--section-data (items p0 p1 &optional currentp)
  "Fold ITEMS for [P0, P1) — the exact fold the review render consumes."
  (org-air-review--section-data items p0 p1 currentp))

(defun org-air-r61--titles (data key)
  "Return the item titles of DATA's per-item section KEY."
  (mapcar (lambda (row) (org-air-item-title (nth 0 row)))
          (plist-get data key)))

(defun org-air-r61--time-alist (data)
  "Return DATA's `:time-items' as ((TITLE . SECS) …)."
  (mapcar (lambda (cell) (cons (org-air-item-title (car cell)) (cdr cell)))
          (plist-get data :time-items)))

(defun org-air-r61--review-row-titles ()
  "Return the rendered review rows' item titles, in buffer order."
  (let ((pos (point-min)) titles)
    (while (setq pos (text-property-not-all pos (point-max)
                                            'org-air-item nil))
      (push (org-air-item-title (get-text-property pos 'org-air-item))
            titles)
      (setq pos (next-single-property-change pos 'org-air-item
                                             nil (point-max))))
    (nreverse titles)))

(defun org-air-r61--goto-review-row (title)
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

(defun org-air-r61--buffer-text ()
  "Return the current buffer's text without properties."
  (buffer-substring-no-properties (point-min) (point-max)))

(defmacro org-air-r61--spying-scans (counter &rest body)
  "Run BODY counting `org-air-query--scan-file' calls into place COUNTER."
  (declare (indent 1) (debug t))
  `(cl-letf* ((org-air-r61--real-scan
               (symbol-function 'org-air-query--scan-file))
              ((symbol-function 'org-air-query--scan-file)
               (lambda (&rest args)
                 (cl-incf ,counter)
                 (apply org-air-r61--real-scan args))))
     ,@body))

(defmacro org-air-r61--spying-reads (counter &rest body)
  "Run BODY counting corpus-file `insert-file-contents' calls in COUNTER.
Only files under the live corpus root are counted, so unrelated
library reads can never leak into the assertion."
  (declare (indent 1) (debug t))
  `(cl-letf* ((org-air-r61--real-ifc
               (symbol-function 'insert-file-contents))
              ((symbol-function 'insert-file-contents)
               (lambda (filename &rest args)
                 (when (and (stringp filename)
                            (string-prefix-p org-air-r61--dir
                                             (expand-file-name filename)))
                   (cl-incf ,counter))
                 (apply org-air-r61--real-ifc filename args))))
     ,@body))

;;;; Bookmark helpers (the r58 idioms, review-sized).

(defun org-air-r61--alist (record)
  "Return RECORD's alist half (`bookmark-make-record' may prepend NAME)."
  (if (stringp (car-safe record)) (cdr record) record))

(defun org-air-r61--field (record key)
  "Return KEY's value in RECORD's alist, or nil."
  (cdr (assq key (org-air-r61--alist record))))

(defun org-air-r61--roundtrip (record)
  "Assert RECORD prin1→read round-trips `equal'; return the re-read copy."
  (let* ((printed (let ((print-length nil) (print-level nil))
                    (prin1-to-string record)))
         (reread (car (read-from-string printed))))
    (should (equal reread record))
    reread))

(defconst org-air-r61--reserved-keys
  '(handler location defaults position filename annotation
    front-context-string rear-context-string)
  "bookmark.el's own record keys (the R58 rule 2).")

(defun org-air-r61--assert-clean-keys (record)
  "Every RECORD key is bookmark.el-reserved or `org-air-'-prefixed."
  (dolist (cell (org-air-r61--alist record))
    (should (consp cell))
    (let ((key (car cell)))
      (should (symbolp key))
      (should (or (memq key org-air-r61--reserved-keys)
                  (string-prefix-p "org-air-" (symbol-name key)))))))

(defun org-air-r61--window-fingerprint ()
  "Return the frame's window tree as plain comparable data (R58 T7)."
  (list (selected-frame)
        (selected-window)
        (mapcar (lambda (w)
                  (list w (window-buffer w) (window-edges w)))
                (window-list nil t))))

(defvar org-air-r61--display-calls 0
  "Calls to the window-display entry points inside the no-display spy.")

(defmacro org-air-r61--asserting-no-display (&rest body)
  "Run BODY spying every window-display entry point (the R58 rule 3).
Asserts ZERO calls to `pop-to-buffer' / `switch-to-buffer' /
`display-buffer' and an unchanged window fingerprint after BODY."
  (declare (indent 0) (debug t))
  `(let ((org-air-r61--display-calls 0)
         (org-air-r61--wc-before (org-air-r61--window-fingerprint)))
     (cl-letf* ((org-air-r61--real-ptb (symbol-function 'pop-to-buffer))
                ((symbol-function 'pop-to-buffer)
                 (lambda (&rest args)
                   (cl-incf org-air-r61--display-calls)
                   (apply org-air-r61--real-ptb args)))
                (org-air-r61--real-stb (symbol-function 'switch-to-buffer))
                ((symbol-function 'switch-to-buffer)
                 (lambda (&rest args)
                   (cl-incf org-air-r61--display-calls)
                   (apply org-air-r61--real-stb args)))
                (org-air-r61--real-db (symbol-function 'display-buffer))
                ((symbol-function 'display-buffer)
                 (lambda (&rest args)
                   (cl-incf org-air-r61--display-calls)
                   (apply org-air-r61--real-db args))))
       ,@body)
     (should (= 0 org-air-r61--display-calls))
     (should (equal org-air-r61--wc-before
                    (org-air-r61--window-fingerprint)))))

;;;; -------------------------------------------------------------------
;;;; r61-1 — T1: same pass, own body, integer shapes
;;;; -------------------------------------------------------------------

(defconst org-air-r61--harvest-specs
  '(("inbox.org" . "* TODO Inbox capture\n")
    ("notes.org" . "#+title: Prose blob\n\nJust prose, no headings.\n")
    ("parent.org" . "\
* TODO Parent heading :work:
:PROPERTIES:
:CREATED: [2026-06-10 Wed 09:00]
:END:
:LOGBOOK:
CLOCK: [2026-06-15 Mon 09:00]--[2026-06-15 Mon 10:00] =>  1:00
CLOCK: [2026-06-16 Tue 09:00]--[2026-06-16 Tue 09:30] =>  0:30
- State \"DONE\"       from \"TODO\"       [2026-06-18 Thu 11:00]
- State \"TODO\"       from \"DONE\"       [2026-06-14 Sun 08:00]
- Note taken on [2026-06-19 Fri 12:00] \\\\
  a plain note
:END:
Body prose.
** TODO Child heading
:LOGBOOK:
CLOCK: [2026-06-20 Sat 14:00]--[2026-06-20 Sat 16:00] =>  2:00
:END:
"))
  "T1 corpus: a parent with own-body facts, a clocked child, a P3 blob.")

(ert-deftest org-air-r61-1-harvest-same-pass-own-body ()
  "T1: the harvest lands in the slots in ONE pass, bounded to the own body.
The parent's two closed CLOCKs, three LOGBOOK stamps (done/todo/note —
the note stamp keeps KIND nil: activity signal, never state inference)
and `:CREATED:' come back as newest-first INTEGER epoch shapes matching
the independently-computed local epochs EXACTLY; the child's clock
lands on the CHILD and never on the parent (the own-body bound —
reverting to a subtree-wide harvest FAILS the parent's exact-`equal'
clock list); an `insert-file-contents' spy proves each corpus file was
read exactly ONCE for the whole scan (the harvest runs inside the same
work-buffer pass — a second file open FAILS); the R53 P3 'file blob
item carries nil review slots.  Reverting the harvest FAILS on every
slot assert."
  (skip-unless (locate-library "org-air"))
  (org-air-r61--with-corpus org-air-r61--harvest-specs
    (let ((reads 0)
          (files (org-air-query-files))
          items)
      (org-air-r61--spying-reads reads
        (setq items (org-air-query-items)))
      ;; ONE read per corpus file — the same-pass law.
      (should (= reads (length files)))
      (let ((parent (org-air-r61--item "Parent heading" items))
            (child (org-air-r61--item "Child heading" items))
            (blob (org-air-r61--item "Prose blob" items)))
        ;; Exact integer shapes, newest-first.
        (should (equal (org-air-item-clocks parent)
                       (list (cons (org-air-r61--epoch 2026 6 16 9)
                                   (org-air-r61--epoch 2026 6 16 9 30))
                             (cons (org-air-r61--epoch 2026 6 15 9)
                                   (org-air-r61--epoch 2026 6 15 10)))))
        (should (equal (org-air-item-logs parent)
                       (list (cons (org-air-r61--epoch 2026 6 19 12) nil)
                             (cons (org-air-r61--epoch 2026 6 18 11) 'done)
                             (cons (org-air-r61--epoch 2026 6 14 8) 'todo))))
        (should (equal (org-air-item-created parent)
                       (org-air-r61--epoch 2026 6 10 9)))
        (should-not (org-air-item-rtrunc parent))
        (dolist (pair (org-air-item-clocks parent))
          (should (integerp (car pair)))
          (should (integerp (cdr pair))))
        ;; The child's clock is the CHILD's — and only the child's.
        (should (equal (org-air-item-clocks child)
                       (list (cons (org-air-r61--epoch 2026 6 20 14)
                                   (org-air-r61--epoch 2026 6 20 16)))))
        (should-not (member (cons (org-air-r61--epoch 2026 6 20 14)
                                  (org-air-r61--epoch 2026 6 20 16))
                            (org-air-item-clocks parent)))
        ;; The P3 file blob: nil review slots, ignored by design.
        (should (eq (org-air-item-kind blob) 'file))
        (should-not (org-air-item-clocks blob))
        (should-not (org-air-item-logs blob))
        (should-not (org-air-item-created blob))
        (should-not (org-air-item-rtrunc blob))))))

;;;; -------------------------------------------------------------------
;;;; r61-2 — T2: never-error (bad lines skipped, poisoned heading nets)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r61-2-harvest-never-error ()
  "T2: malformed lines drop, a poisoned heading degrades, the scan lives.
The salted heading's RUNNING clock (no `--[TS]'), its `CLOCK: [garbage'
fragment and its `[not-a-date]' state stamp are skipped LINE-wise while
its good clock pair and good stamp harvest exactly; the heading whose
harvest is FORCED to signal (`org-air-query--stamp-epoch' poisoned on a
magic stamp) degrades to nil review slots with the item still built
\(title/todo intact) and its good sibling untouched; the skip log stays
EMPTY (the R53 P1b outer net was NOT consumed) and nothing about the
failure is echoed.  Reverting the per-heading inner net FAILS: the
poison signal escapes to the outer net and the whole file — good
headings included — dies into the skip log."
  (skip-unless (locate-library "org-air"))
  (org-air-r61--with-corpus
      '(("inbox.org" . "* TODO Inbox capture\n")
        ("salted.org" . "\
* TODO Good heading :work:
:LOGBOOK:
CLOCK: [2026-06-16 Tue 09:00]--[2026-06-16 Tue 10:00] =>  1:00
- State \"DONE\"       from \"TODO\"       [2026-06-16 Tue 12:00]
:END:
* TODO Salted heading
:LOGBOOK:
CLOCK: [2026-06-17 Wed 09:00]
CLOCK: [garbage
CLOCK: [2026-06-17 Wed 10:00]--[2026-06-17 Wed 11:00] =>  1:00
CLOCK: [2026-06-17 Wed 12:00]--[2026-06-17 Wed 12:30] =>  0:30
- State \"WAIT\" from \"TODO\" [not-a-date]
- State \"DONE\"       from \"TODO\"       [2026-06-17 Wed 13:00]
:END:
* TODO Poisoned heading
:LOGBOOK:
CLOCK: [2026-06-18 Thu 09:00]--[2026-06-18 Thu 10:00] =>  1:00
CLOCK: [POISON]--[2026-06-18 Thu 11:00] =>  1:00
:END:
"))
    (let ((msgs nil) items)
      (cl-letf* ((real-stamp (symbol-function 'org-air-query--stamp-epoch))
                 ((symbol-function 'org-air-query--stamp-epoch)
                  (lambda (ts)
                    (if (and (stringp ts) (string-match-p "POISON" ts))
                        (error "poisoned stamp")
                      (funcall real-stamp ts))))
                 (real-msg (symbol-function 'message))
                 ((symbol-function 'message)
                  (lambda (fmt &rest args)
                    (when fmt (push (apply #'format fmt args) msgs))
                    nil)))
        (ignore real-msg)
        (setq items (org-air-query-items)))
      ;; The scan completed: every heading is an item; the log is EMPTY.
      (should-not org-air-query--skip-log)
      (let ((good (org-air-r61--item "Good heading" items))
            (salted (org-air-r61--item "Salted heading" items))
            (poisoned (org-air-r61--item "Poisoned heading" items)))
        ;; Good heading: untouched by the neighbours' rot.
        (should (equal (org-air-item-clocks good)
                       (list (cons (org-air-r61--epoch 2026 6 16 9)
                                   (org-air-r61--epoch 2026 6 16 10)))))
        (should (equal (org-air-item-logs good)
                       (list (cons (org-air-r61--epoch 2026 6 16 12)
                                   'done))))
        ;; Salted heading: the two good clock lines + the one good stamp
        ;; harvested; running clock / garbage / not-a-date dropped.
        (should (equal (org-air-item-clocks salted)
                       (list (cons (org-air-r61--epoch 2026 6 17 12)
                                   (org-air-r61--epoch 2026 6 17 12 30))
                             (cons (org-air-r61--epoch 2026 6 17 10)
                                   (org-air-r61--epoch 2026 6 17 11)))))
        (should (equal (org-air-item-logs salted)
                       (list (cons (org-air-r61--epoch 2026 6 17 13)
                                   'done))))
        (should-not (org-air-item-rtrunc salted))
        ;; Poisoned heading: nil review slots, item still built.
        (should (equal (org-air-item-todo poisoned) "TODO"))
        (should-not (org-air-item-clocks poisoned))
        (should-not (org-air-item-logs poisoned))
        (should-not (org-air-item-created poisoned))
        (should-not (org-air-item-rtrunc poisoned)))
      ;; Nothing about the failure was echoed.
      (should-not (seq-filter
                   (lambda (m)
                     (string-match-p "poison\\|rror\\|args-out-of-range" m))
                   msgs)))))

;;;; -------------------------------------------------------------------
;;;; r61-3 — T11: the cap — newest kept, rtrunc loud, Started guarded
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r61-3-log-cap-truncation ()
  "T11: `org-air-log-cap' 5 over 8 clock lines keeps EXACTLY the 5 newest.
The oldest three intervals are absent, `rtrunc' is t, the rendered
Completed row carries the inline \"⚠ history truncated\" marker, and
the truncated heading NEVER claims Started through the earliest-stamp
fallback — while the untruncated control twin (same stamp shape,
rtrunc nil) DOES claim it (anti-vacuous).  Reverting the cap (unbounded
lists), the newest-kept order, the `rtrunc' flag or the Started guard
each FAILS its assert."
  (skip-unless (locate-library "org-air"))
  (org-air-r61--with-corpus
      (list '("inbox.org" . "* TODO Inbox capture\n")
            (cons "cap.org"
                  (concat "* TODO Capped heading :work:\n:LOGBOOK:\n"
                          (mapconcat
                           (lambda (d)
                             (format "CLOCK: [2026-06-%02d 09:00]--[2026-06-%02d 10:00] =>  1:00\n"
                                     d d))
                           '(8 9 10 11 15 16 17 18) "")
                          "- State \"DONE\"       from \"TODO\"       [2026-06-16 Tue 12:00]\n"
                          ":END:\n"
                          "* TODO Control heading\n:LOGBOOK:\n"
                          "- State \"TODO\"       from              [2026-06-16 Tue 08:00]\n"
                          ":END:\n")))
    (let ((org-air-log-cap 5))
      (let* ((items (org-air-query-items))
             (capped (org-air-r61--item "Capped heading" items))
             (control (org-air-r61--item "Control heading" items))
             (p0 (org-air-r61--epoch 2026 6 15))
             (p1 (org-air-r61--epoch 2026 6 22)))
        ;; Exactly cap retained, NEWEST kept: Jun 11/15/16/17/18 stay,
        ;; Jun 8/9/10 are gone.
        (should (= (length (org-air-item-clocks capped)) 5))
        (should (equal (caar (org-air-item-clocks capped))
                       (org-air-r61--epoch 2026 6 18 9)))
        (should (equal (car (car (last (org-air-item-clocks capped))))
                       (org-air-r61--epoch 2026 6 11 9)))
        (dolist (d '(8 9 10))
          (should-not (assq (org-air-r61--epoch 2026 6 d 9)
                            (org-air-item-clocks capped))))
        (should (eq (org-air-item-rtrunc capped) t))
        (should-not (org-air-item-rtrunc control))
        ;; Started fallback: the truncated heading never claims it; the
        ;; untruncated twin does (same no-`created', stamp-in-period
        ;; shape — the guard, not the fallback, is what differs).
        (should-not (org-air-review--started-epoch capped p0 p1))
        (should (equal (org-air-review--started-epoch control p0 p1)
                       (org-air-r61--epoch 2026 6 16 8)))
        (let ((data (org-air-r61--section-data items p0 p1)))
          (should-not (member "Capped heading"
                              (org-air-r61--titles data :started)))
          (should (member "Control heading"
                          (org-air-r61--titles data :started)))))
      ;; The render is loud about it: the Completed row (the heading has
      ;; a done stamp in the shown week) carries the marker.
      (org-air-r61--frozen-at org-air-r61--now
        (org-air-review)
        (with-current-buffer org-air-review-buffer-name
          (should (member "Capped heading" (org-air-r61--review-row-titles)))
          (should (string-match-p "⚠ history truncated"
                                  (org-air-r61--buffer-text))))))))

;;;; -------------------------------------------------------------------
;;;; r61-4 — T14: the period engine against the cal-iso oracle
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r61-4-period-engine-oracle ()
  "T14: ISO-week/month bounds match the oracle; the toggle keeps the day.
Week bounds are Monday-00:00 → next-Monday-00:00 local half-open pairs
\(the cal-iso oracle pins the Monday), month bounds 1st → next 1st with
the December → January rollover; the year-edge ISO weeks read W1/2026
for 2026-01-01 and W53/2026 for 2027-01-01 (whose week runs Dec 28 …
Jan 4); adjacent periods share the boundary epoch EXACTLY (half-open);
the `m' toggle shows the other kind's period CONTAINING the anchor day
BOTH directions and `.' returns to the current period.  Reverting the
cal-iso math, the half-open construction or the anchor-preserving
toggle FAILS."
  (skip-unless (locate-library "org-air"))
  ;; The pure engine against independently-built local-midnight epochs.
  (should (equal (org-air-review--period-bounds
                  'week (org-air-r61--epoch 2026 6 17 12))
                 (cons (org-air-r61--epoch 2026 6 15)
                       (org-air-r61--epoch 2026 6 22))))
  (should (equal (org-air-review--period-bounds
                  'month (org-air-r61--epoch 2026 6 17 12))
                 (cons (org-air-r61--epoch 2026 6 1)
                       (org-air-r61--epoch 2026 7 1))))
  (should (equal (org-air-review--period-bounds
                  'month (org-air-r61--epoch 2026 12 31))
                 (cons (org-air-r61--epoch 2026 12 1)
                       (org-air-r61--epoch 2027 1 1))))
  ;; The year-boundary ISO weeks (the cal-iso edge cases).
  (should (equal (org-air-review--iso-week (org-air-r61--epoch 2026 1 1))
                 '(1 . 2026)))
  (should (equal (org-air-review--iso-week (org-air-r61--epoch 2027 1 1))
                 '(53 . 2026)))
  (should (equal (org-air-review--period-bounds
                  'week (org-air-r61--epoch 2027 1 1))
                 (cons (org-air-r61--epoch 2026 12 28)
                       (org-air-r61--epoch 2027 1 4))))
  ;; The oracle agrees with cal-iso called directly.
  (should (equal (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian '(6 15 2026)))
                 '(25 1 2026)))
  ;; Half-open exactness: END is literally the next period's START.
  (let* ((w (org-air-review--period-bounds
             'week (org-air-r61--epoch 2026 6 17)))
         (next (org-air-review--period-bounds 'week (cdr w))))
    (should (equal (cdr w) (car next))))
  ;; Command level: navigation + the anchor-day-preserving toggle.
  (org-air-r61--with-corpus '(("inbox.org" . "* TODO Inbox capture\n"))
    (org-air-r61--frozen-at org-air-r61--now
      (org-air-review)
      (with-current-buffer org-air-review-buffer-name
        ;; Default: the CURRENT ISO week (nil anchor).
        (should (equal (org-air-review--bounds)
                       (cons (org-air-r61--epoch 2026 6 15)
                             (org-air-r61--epoch 2026 6 22))))
        ;; m keeps the (current) anchor: June.  > > walks to August.
        (org-air-review-toggle-kind)
        (should (equal (org-air-review--bounds)
                       (cons (org-air-r61--epoch 2026 6 1)
                             (org-air-r61--epoch 2026 7 1))))
        (org-air-review-period-next)
        (org-air-review-period-next)
        (should (equal (org-air-review--bounds)
                       (cons (org-air-r61--epoch 2026 8 1)
                             (org-air-r61--epoch 2026 9 1))))
        ;; month → week keeps the anchor DAY: the week containing
        ;; Aug 1 2026 (a Saturday) is Jul 27 … Aug 3.
        (org-air-review-toggle-kind)
        (should (eq org-air-review--period-kind 'week))
        (should (equal (org-air-review--bounds)
                       (cons (org-air-r61--epoch 2026 7 27)
                             (org-air-r61--epoch 2026 8 3))))
        ;; week → month keeps it too: back to August, not July.
        (org-air-review-toggle-kind)
        (should (equal (org-air-review--bounds)
                       (cons (org-air-r61--epoch 2026 8 1)
                             (org-air-r61--epoch 2026 9 1))))
        ;; `.' returns to the CURRENT period (anchor nil again).
        (org-air-review-period-today)
        (should-not org-air-review--period-anchor)
        (should (equal (org-air-review--bounds)
                       (cons (org-air-r61--epoch 2026 6 1)
                             (org-air-r61--epoch 2026 7 1))))))))

;;;; -------------------------------------------------------------------
;;;; r61-5 — T3: boundary clipping is exact
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r61-5-clip-exactness ()
  "T3: a boundary-crossing CLOCK splits exactly — 60+30, summed 90.
The 90-minute interval across the W25→W26 Monday-midnight edge
contributes 3600 s to W25 and 1800 s to W26 (sum = the interval; the
adjacent W24 gets 0 — never dropped, never double-counted), through
both the pure clip law AND a REAL scanned item folded with the
suspect-filtered item-time; the same at a June→July month edge; the
day-bounds engine tiles any week into days that sum EXACTLY to the
period span (the DST-exact integer-subtraction law).  Reverting the
clip law (dropping or double-counting boundary clocks) FAILS."
  (skip-unless (locate-library "org-air"))
  (let* ((edge (org-air-r61--epoch 2026 6 22))
         (s (- edge 3600))
         (e (+ edge 1800)))
    ;; The pure law.
    (should (= (org-air-review--clip s e (org-air-r61--epoch 2026 6 15) edge)
               3600))
    (should (= (org-air-review--clip s e edge (org-air-r61--epoch 2026 6 29))
               1800))
    (should (= (org-air-review--clip s e (org-air-r61--epoch 2026 6 8)
                                     (org-air-r61--epoch 2026 6 15))
               0)))
  ;; The real item, scanned then folded.
  (org-air-r61--with-corpus
      '(("inbox.org" . "* TODO Inbox capture\n")
        ("edge.org" . "\
* TODO Week straddler
:LOGBOOK:
CLOCK: [2026-06-21 Sun 23:00]--[2026-06-22 Mon 00:30] =>  1:30
:END:
* TODO Month straddler
:LOGBOOK:
CLOCK: [2026-06-30 Tue 23:00]--[2026-07-01 Wed 00:30] =>  1:30
:END:
"))
    (let* ((items (org-air-query-items))
           (week-item (org-air-r61--item "Week straddler" items))
           (month-item (org-air-r61--item "Month straddler" items)))
      ;; Week edge: 60 min | 30 min, sum 90, W24 zero.
      (should (= (org-air-review--item-time
                  week-item (org-air-r61--epoch 2026 6 15)
                  (org-air-r61--epoch 2026 6 22))
                 3600))
      (should (= (org-air-review--item-time
                  week-item (org-air-r61--epoch 2026 6 22)
                  (org-air-r61--epoch 2026 6 29))
                 1800))
      (should (= (org-air-review--item-time
                  week-item (org-air-r61--epoch 2026 6 8)
                  (org-air-r61--epoch 2026 6 15))
                 0))
      (should (= (+ (org-air-review--item-time
                     week-item (org-air-r61--epoch 2026 6 15)
                     (org-air-r61--epoch 2026 6 22))
                    (org-air-review--item-time
                     week-item (org-air-r61--epoch 2026 6 22)
                     (org-air-r61--epoch 2026 6 29)))
                 5400))
      ;; Month edge: June takes 60, July takes 30.
      (should (= (org-air-review--item-time
                  month-item (org-air-r61--epoch 2026 6 1)
                  (org-air-r61--epoch 2026 7 1))
                 3600))
      (should (= (org-air-review--item-time
                  month-item (org-air-r61--epoch 2026 7 1)
                  (org-air-r61--epoch 2026 8 1))
                 1800))))
  ;; The day engine tiles EVERY week exactly — including a March week
  ;; that crosses a DST switch in zones that have one (23/25-hour days
  ;; still sum to the span; in fixed-offset zones each day is 86400).
  (dolist (anchor (list (org-air-r61--epoch 2026 6 17)
                        (org-air-r61--epoch 2026 3 29)
                        (org-air-r61--epoch 2026 10 25)))
    (let* ((bounds (org-air-review--period-bounds 'week anchor))
           (days (org-air-review--day-bounds (car bounds) (cdr bounds))))
      (should (= (length days) 7))
      (should (= (apply #'+ (mapcar (lambda (d) (- (cdr d) (car d))) days))
                 (- (cdr bounds) (car bounds)))))))

;;;; -------------------------------------------------------------------
;;;; r61-6 — T5: Completed respects the user's done-set (R57)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r61-6-completed-respects-done-set ()
  "T5: the done-set is the R57 merged vocabulary, never a hard-wired list.
Under a user global declaring CLOSED after the bar (the file has no
`#+TODO:'), a `- State \"CLOSED\"' stamp harvests KIND `done' and the
item lists under Completed — a hard-wired (\"DONE\") set would read it
`todo' and FAIL; a `- State \"WAIT\"' stamp (a NOT-done keyword of the
merged vocabulary) harvests `todo' and does NOT list — an
every-quoted-keyword-is-done impl FAILS here; a `CLOSED:' planning
stamp completes an item with NO done stamps (the fallback); an item
with BOTH keeps ONE row counting only its stamps (stamps win — no
double count)."
  (skip-unless (locate-library "org-air"))
  (org-air-r61--with-corpus
      '(("inbox.org" . "* TODO Inbox capture\n")
        ("vocab.org" . "\
* Migrated database
:LOGBOOK:
- State \"CLOSED\"     from \"TODO\"       [2026-06-16 Tue 09:00]
:END:
* Wrong vocabulary
:LOGBOOK:
- State \"WAIT\"       from \"TODO\"       [2026-06-16 Tue 10:00]
:END:
* Old style task
CLOSED: [2026-06-17 Wed 15:00]
* Doubly recorded
CLOSED: [2026-06-18 Thu 09:00]
:LOGBOOK:
- State \"CLOSED\"     from \"TODO\"       [2026-06-18 Thu 09:00]
:END:
"))
    (let ((org-todo-keywords '((sequence "TODO" "|" "CLOSED"))))
      (let* ((items (org-air-query--scan-file (org-air-r61--file "vocab.org")))
             (p0 (org-air-r61--epoch 2026 6 15))
             (p1 (org-air-r61--epoch 2026 6 22))
             (migrated (org-air-r61--item "Migrated database" items))
             (wrong (org-air-r61--item "Wrong vocabulary" items))
             (doubly (org-air-r61--item "Doubly recorded" items))
             (data (org-air-r61--section-data items p0 p1)))
        ;; The stamps were classified against the MERGED vocabulary at
        ;; scan time: the user's CLOSED is done (hard-wired ("DONE")
        ;; would read `todo'), WAIT is a recognised not-done state.
        (should (equal (org-air-item-logs migrated)
                       (list (cons (org-air-r61--epoch 2026 6 16 9)
                                   'done))))
        (should (equal (org-air-item-logs wrong)
                       (list (cons (org-air-r61--epoch 2026 6 16 10)
                                   'todo))))
        ;; The buffer's done set really was the merged one.
        (should (member "CLOSED" (org-air-query-merged-done-keywords)))
        ;; Completed: the CLOSED-stamped item and the planning-line
        ;; fallback are in; the DONE-stamped item is NOT.
        (let ((completed (org-air-r61--titles data :completed)))
          (should (member "Migrated database" completed))
          (should (member "Old style task" completed))
          (should (member "Doubly recorded" completed))
          (should-not (member "Wrong vocabulary" completed)))
        ;; The fallback rides the CLOSED planning stamp's exact epoch.
        (let ((row (assoc "Old style task"
                          (mapcar (lambda (r)
                                    (cons (org-air-item-title (nth 0 r)) r))
                                  (plist-get data :completed)))))
          (should row)
          (should (equal (nth 1 (cdr row))
                         (org-air-r61--epoch 2026 6 17 15))))
        ;; Stamps WIN over CLOSED when both exist: ONE row, ONE count.
        (let ((row (seq-find (lambda (r)
                               (equal (org-air-item-title (nth 0 r))
                                      "Doubly recorded"))
                             (plist-get data :completed))))
          (should row)
          (should (= (nth 2 row) 1)))))))

;;;; -------------------------------------------------------------------
;;;; r61-7 — T4: the time rollup sums exactly under every basis
;;;; -------------------------------------------------------------------

(defconst org-air-r61--rollup-specs
  '(("inbox.org" . "* TODO Inbox capture\n")
    ("dir1/a.org" . "#+category: dir1\n\n* TODO Alpha task :work:\n:LOGBOOK:\nCLOCK: [2026-06-16 Tue 09:00]--[2026-06-16 Tue 10:00] =>  1:00\n:END:\n")
    ("dir1/b.org" . "#+category: dir1\n\n* TODO Beta task :deep:\n:LOGBOOK:\nCLOCK: [2026-06-16 Tue 11:00]--[2026-06-16 Tue 13:00] =>  2:00\n:END:\n")
    ("dir2/c.org" . "#+category: dir2\n\n* TODO Gamma task :work:deep:\n:LOGBOOK:\nCLOCK: [2026-06-17 Wed 08:00]--[2026-06-17 Wed 12:00] =>  4:00\n:END:\n")
    ("dir2/d.org" . "#+category: dir2\n\n* TODO Delta task\n:LOGBOOK:\nCLOCK: [2026-06-18 Thu 09:00]--[2026-06-18 Thu 09:30] =>  0:30\n:END:\n"))
  "T4 corpus: two dirs × two files, tags work/deep, one two-tag heading.")

(ert-deftest org-air-r61-7-time-rollup-exact ()
  "T4: by-tag / by-directory / by-origin aggregate to exact integers.
1h + 2h + 4h + 30min across dir1/dir2: the item fold's headline total
is 27000 s; the tag lens reads #deep 21600 (2 items), #work 18000 (2),
\(untagged) 1800 (1) — the two-tag Gamma contributes its FULL 4h to
BOTH tag rows, so Σ tag rows (41400) deliberately DIFFERS from the
headline total (the lens rule: the headline comes from the item fold,
never from summing rollup rows); the directory lens reads dir1 10800 /
dir2 16200 (which DO sum to the total — each item has one directory);
the origin lens reads the four per-file sums.  Reverting the rollup
fold, the multi-tag attribution or the headline-total sourcing FAILS."
  (skip-unless (locate-library "org-air"))
  (org-air-r61--with-corpus org-air-r61--rollup-specs
    (let* ((items (org-air-query-items))
           (p0 (org-air-r61--epoch 2026 6 15))
           (p1 (org-air-r61--epoch 2026 6 22))
           (data (org-air-r61--section-data items p0 p1))
           (time-items (plist-get data :time-items)))
      ;; The headline total IS the item fold.
      (should (= (plist-get data :time-total) 27000))
      (should (equal (sort (org-air-r61--time-alist data)
                           (lambda (a b) (string< (car a) (car b))))
                     '(("Alpha task" . 3600)
                       ("Beta task" . 7200)
                       ("Delta task" . 1800)
                       ("Gamma task" . 14400))))
      ;; By TAG: full-time-per-tag-row, secs-descending, the lens.
      (let ((rows (org-air-review--time-rollup time-items 'tag p0 p1)))
        (should (equal rows
                       '(("#deep" 21600 2)
                         ("#work" 18000 2)
                         ("(untagged)" 1800 1))))
        ;; Σ tag rows ≠ the headline total in THIS fixture.
        (should (= (apply #'+ (mapcar (lambda (r) (nth 1 r)) rows)) 41400))
        (should-not (= (apply #'+ (mapcar (lambda (r) (nth 1 r)) rows))
                       (plist-get data :time-total))))
      ;; By DIRECTORY (the item's group slot).
      (should (equal (org-air-review--time-rollup time-items 'directory p0 p1)
                     '(("dir2" 16200 2) ("dir1" 10800 2))))
      ;; By ORIGIN (the file leaf).
      (should (equal (org-air-review--time-rollup time-items 'origin p0 p1)
                     '(("c.org" 14400 1) ("b.org" 7200 1)
                       ("a.org" 3600 1) ("d.org" 1800 1)))))))

;;;; -------------------------------------------------------------------
;;;; r61-8 — T8 + T13: cache v6, the sixth key element, zero rescans
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r61-8-cache-v6-and-no-rescan-nav ()
  "T8+T13: v5 misses clean, v6 round-trips, the cap keys, nav never scans.
A crafted v5 cache under the CURRENT key returns nil from both
`org-air-view--cache-read' and `--cache-load' (the documented one-time
cold miss — no error, no hang); a real v6 write→read round-trip
preserves clocks/logs/created/rtrunc `equal'; `org-air-log-cap' is the
SIXTH `org-air-view--cache-key' element (tracks the live value, a
let-bound flip makes the written cache MISS — the R57 key-IS-detector
discipline) and a crafted pre-R61 5-element `:key' misses on length;
twelve `<' presses plus `m'/`>'/`.'/`f' on the live surface under
`org-air-query--scan-file' AND corpus `insert-file-contents' spies make
ZERO calls (period navigation is a filter+fold over cached integers);
and the render from the live scan is BYTE-IDENTICAL to the render from
the cache hydrate at the same fixed width (T13 warm parity — the
hydrate itself spied at zero scans).  Reverting the version bump, the
key element or sneaking a rescan/file-open into navigation FAILS."
  (skip-unless (locate-library "org-air"))
  (org-air-r61--with-corpus
      '(("inbox.org" . "* TODO Inbox capture\n")
        ("clocked.org" . "\
* DONE Weekly habit :habit:
CLOSED: [2026-06-17 Wed 09:10]
:PROPERTIES:
:CREATED: [2026-06-10 Wed 08:00]
:END:
:LOGBOOK:
- State \"DONE\"       from \"TODO\"       [2026-06-17 Wed 09:00]
CLOCK: [2026-06-16 Tue 09:00]--[2026-06-16 Tue 10:30] =>  1:30
:END:
* TODO Deep work :work:
:LOGBOOK:
CLOCK: [2026-06-17 Wed 09:00]--[2026-06-17 Wed 11:00] =>  2:00
:END:
"))
    (org-air-r61--frozen-at org-air-r61--now
      ;; Cold open — the live scan render.
      (org-air-review)
      (let (scan-render items)
        (with-current-buffer org-air-review-buffer-name
          (setq scan-render (org-air-r61--buffer-text)
                items org-air-review--items)
          (should (member "Weekly habit" (org-air-r61--review-row-titles)))
          ;; The nav burst: pure repaints, zero scans, zero file opens.
          (let ((scans 0) (reads 0))
            (org-air-r61--spying-scans scans
              (org-air-r61--spying-reads reads
                (dotimes (_ 12) (org-air-review-period-prev))
                (should (equal (org-air-review--bounds)
                               (cons (org-air-r61--epoch 2026 3 23)
                                     (org-air-r61--epoch 2026 3 30))))
                (org-air-review-period-next)
                (org-air-review-toggle-kind)
                (should (equal (org-air-review--bounds)
                               (cons (org-air-r61--epoch 2026 3 1)
                                     (org-air-r61--epoch 2026 4 1))))
                (org-air-review-toggle-kind)
                (org-air-review-period-today)
                (org-air-review-cycle-rollup)
                (should (equal (org-air-review--bounds)
                               (cons (org-air-r61--epoch 2026 6 15)
                                     (org-air-r61--epoch 2026 6 22))))))
            (should (= scans 0))
            (should (= reads 0))
            ;; Restore the default lens for the parity comparison below.
            (setq-local org-air-review--rollup 'day)
            (org-air-review--render-current)))
        ;; v6 round-trip: all four review slots survive `equal'.
        (org-air-view--cache-write
         items (org-air-view--mtimes-snapshot (org-air-query-files)))
        (let* ((data (org-air-view--cache-read))
               (hydrated (plist-get data :items)))
          (should data)
          (dolist (title '("Weekly habit" "Deep work"))
            (let ((live (org-air-r61--item title items))
                  (twin (org-air-r61--item title hydrated)))
              (should (equal (org-air-item-clocks twin)
                             (org-air-item-clocks live)))
              (should (equal (org-air-item-logs twin)
                             (org-air-item-logs live)))
              (should (equal (org-air-item-created twin)
                             (org-air-item-created live)))
              (should (equal (org-air-item-rtrunc twin)
                             (org-air-item-rtrunc live)))))
          ;; Anti-vacuous: the hydrated habit really carries facts.
          (should (org-air-item-clocks
                   (org-air-r61--item "Weekly habit" hydrated))))
        ;; The cap is the SIXTH key element and the key detects a flip.
        (let ((key (org-air-view--cache-key)))
          (should (= (length key) 6))
          (should (eq (nth 5 key) org-air-log-cap))
          (let ((org-air-log-cap 123))
            (should (equal (nth 5 (org-air-view--cache-key)) 123))
            (should-not (equal (org-air-view--cache-key) key))
            ;; The written cache never hydrates under the flipped cap…
            (should-not (org-air-view--cache-read))
            (should-not (org-air-view--cache-load))))
        ;; …while the original cap still hydrates (the miss is the KEY).
        (should (org-air-view--cache-read))
        ;; T13 warm parity: a fresh surface hydrates cache-first (zero
        ;; scans) and paints the SAME bytes.
        (let ((kill-buffer-query-functions nil))
          (kill-buffer org-air-review-buffer-name))
        (let ((scans 0))
          (org-air-r61--spying-scans scans
            (org-air-review))
          (should (= scans 0)))
        (with-current-buffer org-air-review-buffer-name
          (should org-air-review--items)
          (should (equal (org-air-r61--buffer-text) scan-render)))
        ;; A crafted v5 cache — and a pre-R61 5-element key — are clean
        ;; cold misses (nil, no signal, no hang).
        (let ((print-length nil) (print-level nil))
          (write-region
           (prin1-to-string
            (list :version 5 :key (org-air-view--cache-key)
                  :mtimes nil :file-meta nil :visits nil :items nil))
           nil (expand-file-name org-air-cache-file) nil 'silent))
        (should-not (org-air-view--cache-read))
        (should-not (org-air-view--cache-load))
        (let ((print-length nil) (print-level nil))
          (write-region
           (prin1-to-string
            (list :version org-air-view--cache-version
                  :key (butlast (org-air-view--cache-key))
                  :mtimes nil :file-meta nil :visits nil :items nil))
           nil (expand-file-name org-air-cache-file) nil 'silent))
        (should-not (org-air-view--cache-read))
        (should-not (org-air-view--cache-load))))))

;;;; -------------------------------------------------------------------
;;;; r61-9 — T6: the Started and Carried-over predicates
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r61-9-started-and-carried-predicates ()
  "T6: Started = created-in-period; Carried = activity ∧ not-done at end.
Started: `created' inside the period claims it, outside does not, and
the no-`created' fallback rides the earliest retained stamp (r61-3
pins the rtrunc guard).  Carried over, clause by clause: (a) activity
in W24 + the done stamp AFTER W24's end ⇒ carried in W24 (done-at-end
reads the newest state stamp BEFORE the boundary); (b) done IN W24 ⇒
not carried there despite the activity; (c) an unlogged TODO with
clock activity in the CURRENT week and live `donep' nil ⇒ carried (the
R59 house default: when in doubt, render); (d) the `donep' t twin ⇒
NOT carried (the live tier applies only to the current period).  Each
clause reverts independently to RED."
  (skip-unless (locate-library "org-air"))
  (org-air-r61--with-corpus
      '(("inbox.org" . "* TODO Inbox capture\n")
        ("flow.org" . "\
* TODO Late finisher :work:
:LOGBOOK:
CLOCK: [2026-06-10 Wed 09:00]--[2026-06-10 Wed 10:00] =>  1:00
- State \"TODO\"       from              [2026-06-10 Wed 08:00]
- State \"DONE\"       from \"TODO\"       [2026-06-16 Tue 09:00]
:END:
* DONE Done within
CLOSED: [2026-06-10 Wed 12:00]
:LOGBOOK:
- State \"DONE\"       from \"TODO\"       [2026-06-10 Wed 12:00]
:END:
* TODO Unlogged active
:LOGBOOK:
CLOCK: [2026-06-15 Mon 09:00]--[2026-06-15 Mon 09:30] =>  0:30
:END:
* DONE Unlogged finished
:LOGBOOK:
CLOCK: [2026-06-15 Mon 10:00]--[2026-06-15 Mon 10:30] =>  0:30
:END:
* TODO Fresh starter
:PROPERTIES:
:CREATED: [2026-06-16 Tue 08:00]
:END:
* TODO Old timer
:PROPERTIES:
:CREATED: [2026-02-01 Sun 08:00]
:END:
:LOGBOOK:
CLOCK: [2026-06-16 Tue 09:00]--[2026-06-16 Tue 09:15] =>  0:15
:END:
"))
    (let* ((items (org-air-query-items))
           (w24-0 (org-air-r61--epoch 2026 6 8))
           (w24-1 (org-air-r61--epoch 2026 6 15))
           (w25-0 (org-air-r61--epoch 2026 6 15))
           (w25-1 (org-air-r61--epoch 2026 6 22))
           ;; W24 is a PAST week (currentp nil); W25 is the current one.
           (past (org-air-r61--section-data items w24-0 w24-1 nil))
           (cur (org-air-r61--section-data items w25-0 w25-1 t)))
      ;; (a) done AFTER the period's end ⇒ carried in the period.
      (should (member "Late finisher" (org-air-r61--titles past :carried)))
      ;; …and no longer carried in the week it was finished.
      (should-not (member "Late finisher" (org-air-r61--titles cur :carried)))
      ;; (b) done IN the period ⇒ not carried (activity notwithstanding).
      (should-not (member "Done within" (org-air-r61--titles past :carried)))
      (should (member "Done within" (org-air-r61--titles past :completed)))
      ;; (c) unlogged + clock in the CURRENT period + donep nil ⇒ carried.
      (should (member "Unlogged active" (org-air-r61--titles cur :carried)))
      ;; (d) the donep-t twin ⇒ not carried.
      (should-not (member "Unlogged finished"
                          (org-air-r61--titles cur :carried)))
      ;; Started: created-in-period in, out-of-period out.
      (should (member "Fresh starter" (org-air-r61--titles cur :started)))
      (should-not (member "Old timer" (org-air-r61--titles cur :started)))
      (should-not (member "Fresh starter" (org-air-r61--titles past :started)))
      (let ((row (seq-find (lambda (r)
                             (equal (org-air-item-title (nth 0 r))
                                    "Fresh starter"))
                           (plist-get cur :started))))
        (should (equal (nth 1 row) (org-air-r61--epoch 2026 6 16 8)))))))

;;;; -------------------------------------------------------------------
;;;; r61-10 — T7: suspect clocks are loud, separate, render-time only
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r61-10-suspect-clock-flagged ()
  "T7: a 17 h clock is excluded loudly; the threshold repaints, no rescan.
Under the default 16 h rule the 17 h interval is EXCLUDED from every
total (item fold + rollup + header) and surfaced on the \"⚠ 1 suspect
clock, 17:00 excluded — Marathon session\" line naming the owning
heading, while the 15 h interval IS summed; raising the threshold to
20 h (and disabling with nil) repaints with the interval included —
both flips under `org-air-query--scan-file' + `insert-file-contents'
spies at ZERO calls (render-time only, never a cache/key concern).
Reverting the exclusion (silently summing) or the loud line FAILS."
  (skip-unless (locate-library "org-air"))
  (org-air-r61--with-corpus
      '(("inbox.org" . "* TODO Inbox capture\n")
        ("clocks.org" . "\
* TODO Marathon session :work:
:LOGBOOK:
CLOCK: [2026-06-16 Tue 06:00]--[2026-06-16 Tue 23:00] => 17:00
:END:
* TODO Long haul :work:
:LOGBOOK:
CLOCK: [2026-06-17 Wed 06:00]--[2026-06-17 Wed 21:00] => 15:00
:END:
* TODO Normal session
:LOGBOOK:
CLOCK: [2026-06-18 Thu 09:00]--[2026-06-18 Thu 10:00] =>  1:00
:END:
"))
    (let* ((items (org-air-query-items))
           (p0 (org-air-r61--epoch 2026 6 15))
           (p1 (org-air-r61--epoch 2026 6 22))
           (data (org-air-r61--section-data items p0 p1)))
      ;; Excluded from the fold: 15h + 1h only; counted separately.
      (should (= (plist-get data :time-total) (* 16 3600)))
      (should-not (assoc "Marathon session" (org-air-r61--time-alist data)))
      (should (equal (assoc "Long haul" (org-air-r61--time-alist data))
                     (cons "Long haul" (* 15 3600))))
      (should (= (plist-get data :suspect-count) 1))
      (should (= (plist-get data :suspect-secs) (* 17 3600)))
      (should (equal (mapcar #'org-air-item-title
                             (plist-get data :suspect-items))
                     '("Marathon session")))
      ;; 15 h is NOT suspect under the default.
      (should (member "Long haul"
                      (mapcar #'car (org-air-r61--time-alist data))))
      ;; Threshold changes are pure repaints.
      (let ((org-air-review-suspect-clock-hours 20))
        (should (= (plist-get (org-air-r61--section-data items p0 p1)
                              :time-total)
                   (* 33 3600))))
      (let ((org-air-review-suspect-clock-hours nil))
        (should (= (plist-get (org-air-r61--section-data items p0 p1)
                              :time-total)
                   (* 33 3600)))))
    ;; The surface: the loud line, then the flip repaint under spies.
    (org-air-r61--frozen-at org-air-r61--now
      (org-air-review)
      (with-current-buffer org-air-review-buffer-name
        (let ((text (org-air-r61--buffer-text)))
          (should (string-match-p
                   "⚠ 1 suspect clock, 17:00 excluded — Marathon session"
                   text))
          (should (string-match-p "16:00" text)))
        (let ((scans 0) (reads 0))
          (org-air-r61--spying-scans scans
            (org-air-r61--spying-reads reads
              (let ((org-air-review-suspect-clock-hours 20))
                (org-air-review--render-current)
                (let ((text (org-air-r61--buffer-text)))
                  (should-not (string-match-p "suspect clock" text))
                  (should (string-match-p "33:00" text))))))
          (should (= scans 0))
          (should (= reads 0)))))))

;;;; -------------------------------------------------------------------
;;;; r61-11 — T10: R59 container skip + R60 exclusion inheritance
;;;; -------------------------------------------------------------------

(defconst org-air-r61--inherit-specs
  '(("inbox.org" . "* TODO Inbox capture\n")
    ("keep/tracked.org" . "\
* TODO Tracked task :work:
:LOGBOOK:
CLOCK: [2026-06-16 Tue 13:00]--[2026-06-16 Tue 15:00] =>  2:00
:END:
")
    ("archive/buried.org" . "\
* TODO Buried task :work:
:PROPERTIES:
:CREATED: [2026-06-16 Tue 08:00]
:END:
:LOGBOOK:
CLOCK: [2026-06-16 Tue 13:00]--[2026-06-16 Tue 17:00] =>  4:00
- State \"DONE\"       from \"TODO\"       [2026-06-16 Tue 17:00]
:END:
")
    ("container.org" . "\
* Container parent
:PROPERTIES:
:CREATED: [2026-06-16 Tue 08:00]
:END:
:LOGBOOK:
CLOCK: [2026-06-16 Tue 09:00]--[2026-06-16 Tue 10:00] =>  1:00
- State \"DONE\"       from \"TODO\"       [2026-06-16 Tue 12:00]
:END:
** TODO Container child
:LOGBOOK:
CLOCK: [2026-06-17 Wed 09:00]--[2026-06-17 Wed 11:00] =>  2:00
:END:
"))
  "T10 corpus: a clocked excludable file, a container with a clocked child.")

(ert-deftest org-air-r61-11-container-and-exclusion-inheritance ()
  "T10: exclusion removes a file's time everywhere; containers skip rows.
Baseline (nil excludes, anti-vacuous): Buried task's 4 h is in the fold
and its rows render.  Under an R60 `/archive/' exclude the file's items
never reach the surface — no row, no time, no Completed/Started entry
anywhere (the review's data flows from `org-air-query-files').  The R59
container never appears in Completed/Started/Carried (its own done
stamp and CREATED notwithstanding) while its own-body clock counts in
Time invested at EXACTLY 3600 s and the child's 7200 s attributes to
the CHILD — a subtree-wide harvest reads 10800 on the parent and FAILS;
the knob-nil twin re-admits the container to the per-item sections
\(anti-tautology).  Reverting the exclusion plumbing, the container
skip or the own-body bound FAILS."
  (skip-unless (locate-library "org-air"))
  (org-air-r61--with-corpus org-air-r61--inherit-specs
    (let* ((p0 (org-air-r61--epoch 2026 6 15))
           (p1 (org-air-r61--epoch 2026 6 22)))
      ;; Baseline: the buried facts are real (anti-vacuous).
      (let* ((items (org-air-query-items))
             (data (org-air-r61--section-data items p0 p1)))
        (should (equal (assoc "Buried task" (org-air-r61--time-alist data))
                       (cons "Buried task" 14400)))
        (should (member "Buried task" (org-air-r61--titles data :completed)))
        (should (member "Buried task" (org-air-r61--titles data :started)))
        ;; Container: no per-item row, own-body time only.
        (should-not (member "Container parent"
                            (org-air-r61--titles data :completed)))
        (should-not (member "Container parent"
                            (org-air-r61--titles data :started)))
        (should-not (member "Container parent"
                            (org-air-r61--titles data :carried)))
        (should (equal (assoc "Container parent"
                              (org-air-r61--time-alist data))
                       (cons "Container parent" 3600)))
        (should (equal (assoc "Container child"
                              (org-air-r61--time-alist data))
                       (cons "Container child" 7200)))
        ;; Knob-nil twin: the skip is knob-gated, not hard-wired.
        (let ((org-air-skip-container-headings nil))
          (let ((data2 (org-air-r61--section-data items p0 p1)))
            (should (member "Container parent"
                            (org-air-r61--titles data2 :completed))))))
      ;; The exclusion, end to end through the real surface.
      (let ((org-air-exclude-regexps '("/archive/")))
        (org-air-r61--frozen-at org-air-r61--now
          (org-air-review)
          (with-current-buffer org-air-review-buffer-name
            (let* ((items org-air-review--items)
                   (data (org-air-r61--section-data items p0 p1)))
              (should-not (org-air-test-find-item "Buried task" items))
              (should-not (assoc "Buried task"
                                 (org-air-r61--time-alist data)))
              (should-not (member "Buried task"
                                  (org-air-r61--titles data :completed)))
              ;; 2h tracked + 1h container + 2h child — the 4h is GONE.
              (should (= (plist-get data :time-total) 18000))
              (should-not (member "Buried task"
                                  (org-air-r61--review-row-titles)))
              (should-not (string-match-p "Buried task"
                                          (org-air-r61--buffer-text)))
              ;; The container renders no row even here.
              (should-not (member "Container parent"
                                  (org-air-r61--review-row-titles))))))))))

;;;; -------------------------------------------------------------------
;;;; r61-12 — T12: the ×N count chip
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r61-12-completed-count-chip ()
  "T12: 7 done stamps in the week fold to ONE row carrying \"×7\".
The habit's Completed entry is a single row (never seven), its count
chip reads ×7, and day-grouping keys on the LATEST stamp in the period
\(the \"Sun Jun 21\" group precedes the row; the fold row's epoch IS
the latest stamp).  A one-stamp sibling renders chip-less.  Reverting
the one-row-per-item fold or the latest-stamp grouping FAILS."
  (skip-unless (locate-library "org-air"))
  (org-air-r61--with-corpus
      (list '("inbox.org" . "* TODO Inbox capture\n")
            ;; `:CREATED:' sits OUTSIDE the shown week so neither habit
            ;; claims a Started row — the chip assert counts RENDERED
            ;; rows and must see exactly the one Completed row.
            (cons "habit.org"
                  (concat "* DONE Water the plants :habit:\n"
                          ":PROPERTIES:\n:CREATED: [2026-06-01 Mon 08:00]\n:END:\n"
                          ":LOGBOOK:\n"
                          (mapconcat
                           (lambda (d)
                             (format "- State \"DONE\"       from \"TODO\"       [2026-06-%02d 07:00]\n" d))
                           '(15 16 17 18 19 20 21) "")
                          ":END:\n"
                          "* DONE One shot\n"
                          ":PROPERTIES:\n:CREATED: [2026-06-01 Mon 08:00]\n:END:\n"
                          ":LOGBOOK:\n"
                          "- State \"DONE\"       from \"TODO\"       [2026-06-16 Tue 09:00]\n"
                          ":END:\n")))
    (let* ((items (org-air-query-items))
           (p0 (org-air-r61--epoch 2026 6 15))
           (p1 (org-air-r61--epoch 2026 6 22))
           (data (org-air-r61--section-data items p0 p1))
           (row (seq-find (lambda (r)
                            (equal (org-air-item-title (nth 0 r))
                                   "Water the plants"))
                          (plist-get data :completed))))
      ;; ONE fold row, count 7, keyed on the LATEST stamp.
      (should row)
      (should (= (nth 2 row) 7))
      (should (equal (nth 1 row) (org-air-r61--epoch 2026 6 21 7)))
      (should (= (seq-count (lambda (r)
                              (equal (org-air-item-title (nth 0 r))
                                     "Water the plants"))
                            (plist-get data :completed))
                 1)))
    (org-air-r61--frozen-at org-air-r61--now
      (org-air-review)
      (with-current-buffer org-air-review-buffer-name
        ;; ONE rendered row for the habit, carrying the ×7 chip.
        (should (= (seq-count (lambda (title)
                                (equal title "Water the plants"))
                              (org-air-r61--review-row-titles))
                   1))
        (org-air-r61--goto-review-row "Water the plants")
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (should (string-match-p "×7" line))
          (should (string-match-p "Jun 21" line)))
        ;; Day-grouped under the LATEST stamp's day, group label first.
        (let ((group (string-match
                      (regexp-quote
                       (format-time-string
                        "%a %b %-d" (org-air-r61--epoch 2026 6 21)))
                      (org-air-r61--buffer-text)))
              (row (string-match "Water the plants"
                                 (org-air-r61--buffer-text))))
          (should group)
          (should row)
          (should (< group row)))
        ;; The one-stamp sibling renders chip-less.
        (org-air-r61--goto-review-row "One shot")
        (should-not (string-match-p
                     "×" (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position))))))))

;;;; -------------------------------------------------------------------
;;;; r61-13 — T9: the R58 bookmark parity
;;;; -------------------------------------------------------------------

(defconst org-air-r61--bookmark-specs
  '(("inbox.org" . "* TODO Inbox capture\n")
    ("work.org" . "\
* TODO Alpha done :work:
:PROPERTIES:
:CREATED: [2026-05-01 Fri 08:00]
:END:
:LOGBOOK:
- State \"DONE\"       from \"TODO\"       [2026-06-09 Tue 10:00]
:END:
* TODO Beta done :work:
:PROPERTIES:
:CREATED: [2026-05-01 Fri 08:00]
:END:
:LOGBOOK:
- State \"DONE\"       from \"TODO\"       [2026-06-10 Wed 10:00]
:END:
")
    ("play.org" . "\
* TODO Gamma done :play:
:PROPERTIES:
:CREATED: [2026-05-01 Fri 08:00]
:END:
:LOGBOOK:
- State \"DONE\"       from \"TODO\"       [2026-06-11 Thu 10:00]
:END:
"))
  "T9 corpus: three W24 completions across two files and two tags.
Every heading's `:CREATED:' sits OUTSIDE W24 so no Started row can
shadow the two-row Completed shape the landing asserts depend on.")

(ert-deftest org-air-r61-13-bookmark-roundtrip ()
  "T9: the review record round-trips; the jump rebuilds it cache-first.
A navigated (W24) + tag-rollup + filtered + scoped + title-descending
review with point on a NON-first row records every view-defining field
printable (prin1→read `equal', every key reserved-or-org-air-prefixed,
the period anchor an INTEGER epoch); after killing the buffer the
autoloaded handler alone rebuilds the surface from the CACHE (zero
scans spied), leaves the review buffer CURRENT but undisplayed (zero
display calls, window fingerprint unchanged), restores period kind +
anchor + rollup + filter + scope + sort and lands point on the
bookmarked row — which is NOT the render's first-row default.  A
record made on the CURRENT period stores the symbol `current' and,
restored a week later (frozen clock advanced), tracks the NEW current
week.  A malformed record degrades to a plain review without a signal.
Handler metadata: `fboundp', `bookmark-handler-type' \"org-air\", the
literal `;;;###autoload' cookie, and the mode wires a buffer-local
`bookmark-make-record-function'.  Reverting the record fns, the
handler, the apply or the locator chain FAILS."
  (skip-unless (locate-library "org-air"))
  ;; Handler metadata first (corpus-free).
  (should (fboundp 'org-air-review-bookmark-jump))
  (should (equal (get 'org-air-review-bookmark-jump 'bookmark-handler-type)
                 "org-air"))
  (let* ((loaded (locate-library "org-air-review"))
         (el (and loaded (concat (file-name-sans-extension loaded) ".el"))))
    (should (and el (file-readable-p el)))
    (with-temp-buffer
      (insert-file-contents el)
      (goto-char (point-min))
      (should (re-search-forward
               "^;;;###autoload[ \t]*\n(defun org-air-review-bookmark-jump "
               nil t))))
  (org-air-r61--with-corpus org-air-r61--bookmark-specs
    (org-air-r61--frozen-at org-air-r61--now
      (let ((work (org-air-r61--file "work.org"))
            (w24-0 (org-air-r61--epoch 2026 6 8))
            record reread items)
        ;; Compose the navigated, lensed view; bookmark the SECOND row.
        (org-air-review)
        (with-current-buffer org-air-review-buffer-name
          (should (derived-mode-p 'org-air-review-mode))
          (should (local-variable-p 'bookmark-make-record-function))
          (should (eq bookmark-make-record-function
                      #'org-air-review--bookmark-make-record))
          (setq items org-air-review--items)
          (org-air-review-period-prev)     ; → W24, an ABSOLUTE anchor
          (setq-local org-air-review--rollup 'tag)
          (setq org-air-view--tag-filter '("#work"))
          (setq-local org-air-view--scope (list :file work))
          (setq-local org-air-view--sort-key 'title)
          (setq-local org-air-view--sort-direction 'descending)
          (org-air-review--render-current)
          ;; Under title-descending #work rows read Beta, Alpha — the
          ;; landing assert below can never pass off the first row.
          (should (equal (org-air-r61--review-row-titles)
                         '("Beta done" "Alpha done")))
          (org-air-r61--goto-review-row "Alpha done")
          (setq record (bookmark-make-record))
          (should (eq (org-air-r61--field record 'handler)
                      'org-air-review-bookmark-jump))
          (should (eq (org-air-r61--field record 'org-air-view) 'review))
          (should (equal (org-air-r61--field record 'org-air-period)
                         (cons 'week w24-0)))
          (should (integerp (cdr (org-air-r61--field record
                                                     'org-air-period))))
          (should (eq (org-air-r61--field record 'org-air-rollup) 'tag))
          (should (equal (org-air-r61--field record 'org-air-filter)
                         '("#work")))
          (should (equal (org-air-r61--field record 'org-air-scope)
                         (list :file work)))
          (should (equal (org-air-r61--field record 'org-air-sort)
                         '(title . descending)))
          (should (equal (car (org-air-r61--field record 'org-air-item))
                         work))
          (should (integerp (cdr (org-air-r61--field record 'org-air-item))))
          (should (equal (org-air-r61--field record 'org-air-item-title)
                         "Alpha done"))
          (should (equal (car (org-air-r61--field record 'defaults))
                         "org-air: review · W24 2026"))
          (should (stringp (org-air-r61--field record 'location)))
          (org-air-r61--assert-clean-keys record)
          (setq reread (org-air-r61--roundtrip record)))
        ;; Persist the cache so the jump can rebuild CACHE-FIRST.
        (org-air-view--cache-write
         items (org-air-view--mtimes-snapshot (org-air-query-files)))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer org-air-review-buffer-name))
        ;; The handler alone: zero scans, zero display, state restored.
        (let ((scans 0))
          (org-air-r61--asserting-no-display
            (org-air-r61--spying-scans scans
              (org-air-review-bookmark-jump reread)))
          (should (= scans 0)))
        (should (eq (current-buffer)
                    (get-buffer org-air-review-buffer-name)))
        (with-current-buffer org-air-review-buffer-name
          (should (derived-mode-p 'org-air-review-mode))
          (should (eq org-air-review--period-kind 'week))
          (should (equal org-air-review--period-anchor w24-0))
          (should (equal (org-air-review--bounds)
                         (cons w24-0 (org-air-r61--epoch 2026 6 15))))
          (should (eq org-air-review--rollup 'tag))
          (should (equal org-air-view--tag-filter '("#work")))
          (should (equal org-air-view--scope (list :file work)))
          (should (eq org-air-view--sort-key 'title))
          (should (eq org-air-view--sort-direction 'descending))
          ;; Point on the bookmarked row — NOT the first-row default.
          (should (equal (org-air-r61--review-row-titles)
                         '("Beta done" "Alpha done")))
          (let ((item (org-air-view--row-property 'org-air-item)))
            (should item)
            (should (equal (org-air-item-title item) "Alpha done"))))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer org-air-review-buffer-name))))
    ;; The `current' anchor: recorded on the default view, restored a
    ;; week later, the surface tracks the NEW current period.
    (let (record)
      (org-air-r61--frozen-at org-air-r61--now
        (org-air-review)
        (with-current-buffer org-air-review-buffer-name
          (setq record (bookmark-make-record))
          (should (equal (org-air-r61--field record 'org-air-period)
                         '(week . current))))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer org-air-review-buffer-name)))
      (org-air-r61--frozen-at (org-air-r61--epoch 2026 6 22 10)
        (org-air-review-bookmark-jump (org-air-r61--roundtrip record))
        (with-current-buffer org-air-review-buffer-name
          (should-not org-air-review--period-anchor)
          (should (equal (org-air-review--bounds)
                         (cons (org-air-r61--epoch 2026 6 22)
                               (org-air-r61--epoch 2026 6 29)))))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer org-air-review-buffer-name))))
    ;; Malformed records degrade to a plain review, never a signal.
    (org-air-r61--frozen-at org-air-r61--now
      (org-air-review-bookmark-jump
       '((handler . org-air-review-bookmark-jump)
         (location . "org-air: review")
         (org-air-version . 999)
         (org-air-view . review)
         (org-air-period . "junk")
         (org-air-rollup . 42)
         (org-air-scope . 42)
         (org-air-sort . "sideways")
         (org-air-flux-capacitor . "later")))
      (should (eq (current-buffer) (get-buffer org-air-review-buffer-name)))
      (with-current-buffer org-air-review-buffer-name
        (should (derived-mode-p 'org-air-review-mode))
        (should (eq org-air-review--period-kind 'week))
        (should-not org-air-review--period-anchor)
        (should (eq org-air-review--rollup 'day))
        (should-not org-air-view--scope)
        ;; A REAL review, not a husk.
        (should org-air-review--items)))))

(provide 'org-air-round61-test)
;;; org-air-round61-test.el ends here
