;;; org-air-round84-test.el --- executing ERTs for v0.1 round-84 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.1 round-84 — the review-pane repair
;; (air/v0.1/org-air-round84-design.org).  Two defects:
;;
;;   D1 — the review's per-item rows (Completed / Started / Carried over
;;        / Dropped) carried NO priority pill, unlike the board/day panes.
;;        R84 EXTRACTS the board's inline priority branch into a shared
;;        `org-air-view--priority-cell' and prepends it to the review row
;;        prefix too (its width reserved in the cluster fit).  The board
;;        render is byte-IDENTICAL (a pure extraction).
;;
;;   D2 — a started-then-DROPPED item mis-filed as "Started" (and, unseen,
;;        "Completed", since DROPPED shares the merged done-vocabulary).
;;        R84 gives abandonment a first-class, period-honest treatment: a
;;        new `org-air-review--abandoned-p' (final keyword is a
;;        cancelled/abandoned spelling via the shared
;;        `org-air-view--dropped-keyword-p' AND an in-period close) routes
;;        the item OUT of Started/Completed/Carried and INTO its OWN
;;        conditional "Dropped" section (⊘ glyph, mirror-collapsed, a
;;        conditional Summary count).  Time invested still credits the
;;        clocked hours; the header's "N done" becomes honest for free.
;;
;; All BATCH/headless.  The pill/section COLOURS are GUI-confirm-only; the
;; seams assert pill PRESENCE + FACE at the byte/text layer and the exact
;; review BUCKET a fixture item lands in.  These 14 seams are the
;; permanent regression guards named by the design's "ERT seams" ledger;
;; each names what reverting the fix breaks.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)

(eval-and-compile
  (require 'org-air))

(defvar org-air-files)
(defvar org-air-inbox-file)
(defvar org-air-exclude-regexps)
(defvar org-air-cache-file)
(defvar org-air-view-width)
(defvar org-air-view-height)
(defvar org-air-priority-style)
(defvar org-air-priority-show)
(defvar org-air-review-collapse-mirrors)
(defvar org-air-review-buffer-name)

;;;; -------------------------------------------------------------------
;;;; Corpus scaffolding (the r61/r63 idioms, r84-sized)
;;;; -------------------------------------------------------------------

(defvar org-air-r84--dir nil
  "The temp corpus directory of the current `org-air-r84--with-corpus'.")

(defun org-air-r84--reset-tables ()
  "Clear the GLOBAL query-layer tables so no temp path leaks across tests."
  (clrhash org-air-query--file-meta)
  (clrhash org-air-query--visits)
  (clrhash org-air-query--denote-id-index)
  (setq org-air-query--link-graph-dirty nil))

(defmacro org-air-r84--with-corpus (specs &rest body)
  "Create a temp Org corpus from SPECS and run BODY against it.
SPECS is a list of (NAME . CONTENT) files written into a fresh TRUENAMED
temp directory.  Binds `org-air-files'/`org-air-inbox-file'/a temp
`org-air-cache-file', nil excludes and the 170x40 batch viewport (the
review golden's geometry, so the origin cap is never squeezed by the fit
pass); wraps BODY in `save-window-excursion'; starts from EMPTY query
tables and cleans up tables, org-air view buffers, corpus-visiting
buffers and the directory."
  (declare (indent 1) (debug t))
  `(let* ((org-air-r84--dir (file-truename (make-temp-file "org-air-r84-" t))))
     (unwind-protect
         (progn
           (org-air-r84--reset-tables)
           (pcase-dolist (`(,name . ,content) ,specs)
             (let ((path (expand-file-name name org-air-r84--dir))
                   (coding-system-for-write 'utf-8-unix))
               (make-directory (file-name-directory path) t)
               (write-region (or content "") nil path nil 'silent)))
           (let ((org-air-files (list org-air-r84--dir))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-r84--dir))
                 (org-air-exclude-regexps nil)
                 (org-air-cache-file
                  (expand-file-name ".cache/board.eld" org-air-r84--dir))
                 (org-air-view-width 170)
                 (org-air-view-height 40))
             (save-window-excursion
               ,@body)))
       (org-air-query-teardown)
       (org-air-r84--reset-tables)
       (let ((kill-buffer-query-functions nil))
         (dolist (name (list org-air-review-buffer-name
                             org-air-view-buffer-name
                             org-air-rail-buffer-name))
           (when (get-buffer name)
             (kill-buffer name)))
         (dolist (buf (buffer-list))
           (let ((fn (buffer-file-name buf)))
             (when (and fn (string-prefix-p org-air-r84--dir fn))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r84--dir t))))

(defun org-air-r84--epoch (y m d &optional hh mm)
  "Return the LOCAL integer epoch of Y-M-D HH:MM (defaults midnight).
The independent oracle, built with `encode-time' on local calendar
dates exactly like the period engine's boundaries (TZ-independent)."
  (floor (float-time (encode-time (list 0 (or mm 0) (or hh 0)
                                        d m y nil -1 nil)))))

(defconst org-air-r84--now
  (org-air-r84--epoch 2026 7 19 18)
  "The frozen \"now\": Sun 2026-07-19 18:00 local — inside ISO W29 2026.
The current week under it is [Jul 13, Jul 20).")

(defun org-air-r84--w29-0 () (org-air-r84--epoch 2026 7 13))
(defun org-air-r84--w29-1 () (org-air-r84--epoch 2026 7 20))
(defun org-air-r84--w30-0 () (org-air-r84--epoch 2026 7 20))
(defun org-air-r84--w30-1 () (org-air-r84--epoch 2026 7 27))

(defmacro org-air-r84--frozen (&rest body)
  "Run BODY with the Lisp-visible clock frozen to `org-air-r84--now'.
Overrides `current-time' AND no-arg `float-time'; `float-time' WITH an
argument passes through, so timestamp parsing and period math stay real."
  (declare (indent 0) (debug t))
  `(cl-letf* ((org-air-r84--real-ft (symbol-function 'float-time))
              ((symbol-function 'float-time)
               (lambda (&optional time)
                 (if time (funcall org-air-r84--real-ft time)
                   (float org-air-r84--now))))
              ((symbol-function 'current-time)
               (lambda () (seconds-to-time org-air-r84--now))))
     ,@body))

;; The master review corpus (W29 clock).  A DONE [#A] (Completed, priority
;; pill + a `:backlog:' tag), a no-priority DONE (Completed, blank slot), a
;; started-then-DROPPED [#B] with an in-period clock (the D2 core — Dropped,
;; priority pill, time credited), a genuine STARTED (control), a plain
;; `:backlog:'-tagged DROPPED (Dropped), and a period-honesty item started
;; in W29 but dropped in W30 (`Late abandon').  The `#+TODO:' names DROPPED
;; as a done-family keyword (so its transition scans done-kind AND its
;; `todo' slot reads "DROPPED").
(defconst org-air-r84--fixture
  "#+title: r84 review
#+TODO: TODO STARTED | DONE DROPPED

* DONE [#A] Ship the release :ship:backlog:
:PROPERTIES:
:CREATED: [2026-05-01 Fri 09:00]
:END:
:LOGBOOK:
- State \"DONE\"       from \"TODO\"       [2026-07-16 Thu 10:00]
:END:
* DONE Plain finished
:PROPERTIES:
:CREATED: [2026-05-01 Fri 09:00]
:END:
:LOGBOOK:
- State \"DONE\"       from \"TODO\"       [2026-07-15 Wed 09:00]
:END:
* DROPPED [#B] Abandoned effort :work:
:PROPERTIES:
:CREATED: [2026-05-01 Fri 09:00]
:END:
:LOGBOOK:
- State \"DROPPED\"    from \"STARTED\"    [2026-07-15 Wed 16:00]
- State \"STARTED\"    from \"TODO\"       [2026-07-14 Tue 09:00]
CLOCK: [2026-07-14 Tue 13:00]--[2026-07-14 Tue 17:00] =>  4:00
:END:
* STARTED Genuine active work
:PROPERTIES:
:CREATED: [2026-07-14 Tue 10:00]
:END:
* DROPPED Second abandon :backlog:
:PROPERTIES:
:CREATED: [2026-05-01 Fri 09:00]
:END:
:LOGBOOK:
- State \"DROPPED\"    from \"TODO\"       [2026-07-17 Fri 11:00]
:END:
* DROPPED Late abandon
:PROPERTIES:
:CREATED: [2026-07-15 Wed 10:00]
:END:
:LOGBOOK:
- State \"DROPPED\"    from \"STARTED\"    [2026-07-22 Wed 11:00]
- State \"STARTED\"    from \"TODO\"       [2026-07-16 Thu 09:00]
:END:
"
  "The master review corpus, six headings, built around the W29 clock.")

(defun org-air-r84--master-specs ()
  "The master corpus SPECS (the fixture + an empty inbox)."
  `(("inbox.org" . "#+title: inbox\n")
    ("review.org" . ,org-air-r84--fixture)))

(defmacro org-air-r84--rendered (specs &rest body)
  "Scan SPECS, render `org-air-review' (GUI glyphs, frozen W29), run BODY.
BODY runs inside the live `*org-air-review*' buffer, so the render is the
REAL renderer's bytes (the golden discipline).  `org-air-review--items'
and `org-air-review--section-data' are available for bucket asserts."
  (declare (indent 1) (debug t))
  `(org-air-r84--with-corpus ,specs
     (org-air-viewport-test-as-gui
       (org-air-r84--frozen
         (org-air-review)
         (with-current-buffer org-air-review-buffer-name
           ,@body)))))

(defun org-air-r84--items ()
  "Return the scanned review items in the current review buffer."
  org-air-review--items)

(defun org-air-r84--data (p0 p1 &optional currentp)
  "Fold the review buffer's items for [P0, P1) — the render's own fold."
  (org-air-review--section-data (org-air-r84--items) p0 p1 currentp))

(defun org-air-r84--titles (data key)
  "Return the item titles of DATA's per-item section KEY."
  (mapcar (lambda (row) (org-air-item-title (nth 0 row)))
          (plist-get data key)))

(defun org-air-r84--in (title data key)
  "Non-nil when a row titled TITLE is in DATA's section KEY."
  (and (member title (org-air-r84--titles data key)) t))

(defun org-air-r84--row-line (title)
  "Return the rendered review row LINE (properties kept) for TITLE, or nil.
Resolves via the row's `org-air-item' property so the match is a genuine
item row, not a stray title occurrence."
  (let ((pos (point-min)) line)
    (while (and (not line)
                (setq pos (text-property-not-all pos (point-max)
                                                 'org-air-item nil)))
      (let ((item (get-text-property pos 'org-air-item)))
        (if (equal (org-air-item-title item) title)
            (save-excursion
              (goto-char pos)
              (setq line (buffer-substring (line-beginning-position)
                                           (line-end-position))))
          (setq pos (next-single-property-change pos 'org-air-item
                                                 nil (point-max))))))
    line))

(defun org-air-r84--row-section (title)
  "Return the enclosing `org-air-section' symbol of the review row TITLE."
  (let ((pos (point-min)) sec)
    (while (and (not sec)
                (setq pos (text-property-not-all pos (point-max)
                                                 'org-air-item nil)))
      (let ((item (get-text-property pos 'org-air-item)))
        (if (equal (org-air-item-title item) title)
            (save-excursion
              (goto-char pos)
              (catch 'sec
                (while (> (point) (point-min))
                  (forward-line -1)
                  (let ((s (get-text-property (point) 'org-air-section)))
                    (when s (setq sec s) (throw 'sec s))))))
          (setq pos (next-single-property-change pos 'org-air-item
                                                 nil (point-max))))))
    sec))

(defun org-air-r84--title-col (line title)
  "Return the 0-based column where TITLE begins in LINE (properties ok)."
  (string-match (regexp-quote title) (substring-no-properties line)))

(defun org-air-r84--square-glyph ()
  "Return the GUI priority-square glyph (`■'), as the renders emit it.
`org-air-layout-glyph' is display-tier-sensitive; the review/board renders
run under the GUI stub (`org-air-viewport-test-as-gui'), so the glyph must
be resolved in that same tier — resolving it in plain batch yields the
ASCII `#' fallback, which collides with the `[#A]' token's `#'."
  (org-air-viewport-test-as-gui (org-air-layout-glyph 'priority-square)))

;; The pre-R84 inline priority branch (org-air-view.el:3835), frozen here
;; as the revert reference: the extracted `org-air-view--priority-cell'
;; must equal THIS for every style x priority, else the board — which now
;; reads the shared cell — would move.
(defun org-air-r84--old-priority-branch (item)
  "The verbatim pre-R84 `--insert-item' inline priority branch."
  (let ((priority (org-air-view--priority-char item)))
    (if (eq org-air-priority-style 'square)
        (org-air-view--priority-slot priority)
      (when (and priority (member priority org-air-priority-show))
        (concat (org-air-view--priority-token priority) " ")))))

;; An isolated board carrying one [#A] deadline row + a no-cookie row, so
;; the SAME priority pill can be compared board<->review.  Not part of the
;; canonical fixture set (the 25 layout goldens stay byte-identical).
(defconst org-air-r84--board-org
  "* TODO [#A] Board priority row  :prio:
DEADLINE: <2026-07-10 Fri>
* TODO Board plain row  :prio:
DEADLINE: <2026-07-10 Fri>
"
  "A tiny overdue board: one [#A] row and one no-cookie row.")

(defmacro org-air-r84--with-board (&rest body)
  "Render the [#A] board (isolated dir, GUI glyphs, frozen W29); run BODY
in the `*org-air*' buffer."
  (declare (indent 0) (debug t))
  `(let ((dir (make-temp-file "org-air-r84-board-" t)))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name "inbox.org" dir)
             (insert org-air-r84--board-org))
           (let ((org-air-files (list dir))
                 (org-air-inbox-file (expand-file-name "no-inbox.org" dir))
                 (org-air-view-width 160))
             (org-air-viewport-test-as-gui
               (org-air-r84--frozen
                 (org-air)
                 (unwind-protect
                     (with-current-buffer "*org-air*" ,@body)
                   (when (get-buffer "*org-air*")
                     (kill-buffer "*org-air*")))))))
       (delete-directory dir t))))

(defun org-air-r84--board-row-line (title)
  "Return the first `*org-air*' buffer line containing TITLE (props kept)."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward title nil t)
      (buffer-substring (line-beginning-position) (line-end-position)))))

;;;; ===================================================================
;;;; D1 — the shared priority pill on review rows
;;;; ===================================================================

(ert-deftest org-air-r84-1-review-row-carries-priority-pill ()
  "r84-1: a review row carries the SAME priority pill as a board row.
The [#A] Completed row's prefix begins with the `square slot — the
priority-square glyph carrying `org-air-face-priority-a', the exact pill
the board draws for a #A item (asserted against a real board render); a
no-priority Completed row shows the two-blank slot (no square) and its
title still LEFT-ALIGNS with the priority row's (the D1c fit reserve
holds the column).  Reverting the D1b prefix injection FAILS (no pill on
the review row)."
  (skip-unless (locate-library "org-air"))
  (let ((sq (org-air-r84--square-glyph)))
    ;; The board's #A pill (the reference the review must match).
    (let (board-line board-pos)
      (org-air-r84--with-board
        (setq board-line (org-air-r84--board-row-line "Board priority row"))
        (should board-line)
        (setq board-pos (string-match (regexp-quote sq)
                                      (substring-no-properties board-line)))
        (should board-pos)
        (should (eq (get-text-property board-pos 'face board-line)
                    (org-air-view--priority-face ?A)))
        ;; the no-cookie board row has the blank slot (no square).
        (should-not (string-match-p
                     (regexp-quote sq)
                     (substring-no-properties
                      (org-air-r84--board-row-line "Board plain row")))))
      ;; The review's #A Completed row carries the IDENTICAL pill.
      (org-air-r84--rendered (org-air-r84--master-specs)
        (let* ((pri-line (org-air-r84--row-line "Ship the release"))
               (pri-txt (substring-no-properties pri-line))
               (pri-pos (string-match (regexp-quote sq) pri-txt))
               (blank-line (org-air-r84--row-line "Plain finished"))
               (blank-txt (substring-no-properties blank-line)))
          (should pri-line)
          ;; the square is present, BEFORE the title, faced priority-a —
          ;; the same glyph + face the board drew.
          (should pri-pos)
          (should (< pri-pos (org-air-r84--title-col pri-line
                                                     "Ship the release")))
          (should (eq (get-text-property pri-pos 'face pri-line)
                      (org-air-view--priority-face ?A)))
          ;; the no-priority row shows NO square (the blank 2-col slot)…
          (should blank-line)
          (should-not (string-match-p (regexp-quote sq) blank-txt))
          ;; …yet its title left-aligns with the priority row's title
          ;; (the slot reserves its column — alignment holds).
          (should (= (org-air-r84--title-col pri-line "Ship the release")
                     (org-air-r84--title-col blank-line "Plain finished"))))))))

(ert-deftest org-air-r84-2-priority-cell-inert-on-board ()
  "r84-2: the shared `--priority-cell' is a pure extraction (board inert).
Leg A (revert-check): for EVERY `org-air-priority-style' x priority
level, `org-air-view--priority-cell' returns byte-for-byte the frozen
pre-R84 inline branch — so `--insert-item', now reading the shared cell,
renders the board's priority prefix identically.  Leg B (wire): a real
board's [#A] row actually carries `--priority-cell''s output, proving
`--insert-item' prepends the shared cell (not a private copy).  Forking
the cell (dropping the pad, hard-wiring a style, changing the slot) FAILS
leg A; reverting `--insert-item' to a divergent branch FAILS leg B.  The
25 layout byte-goldens (`org-air-layout-mockup-*') independently pin that
the board bytes did not move."
  (skip-unless (locate-library "org-air"))
  ;; Leg A: pure equality against the frozen reference, all styles/levels.
  (let ((dir (make-temp-file "org-air-r84-pc-" t)))
    (unwind-protect
        (let ((file (expand-file-name "bare.org" dir)))
          (with-temp-file file
            (insert "* [#A] A\n* [#B] B\n* [#C] C\n* [#D] D\n* [#E] E\n* Plain\n"))
          (let ((items (org-air-query-items-in-files (list file))))
            (should (= (length items) 6))
            (dolist (style '(square badge text))
              (let ((org-air-priority-style style))
                (dolist (item items)
                  (let ((cell (org-air-view--priority-cell item))
                        (ref  (org-air-r84--old-priority-branch item)))
                    ;; text bytes (what the golden pins) are identical…
                    (should (equal (and cell (substring-no-properties cell))
                                   (and ref (substring-no-properties ref))))
                    ;; …and the face at each position agrees (the pill).
                    (when (and cell (> (length cell) 0))
                      (should (equal (get-text-property 0 'face cell)
                                     (get-text-property 0 'face ref))))))))))
      (delete-directory dir t)))
  ;; Leg B: the board wire — the [#A] row carries the shared cell's pill.
  (let ((sq (org-air-r84--square-glyph)))
    (org-air-r84--with-board
      (let ((line (org-air-r84--board-row-line "Board priority row")))
        (should line)
        (should (string-match-p (regexp-quote sq)
                                (substring-no-properties line)))
        (should (eq (get-text-property
                     (string-match (regexp-quote sq)
                                   (substring-no-properties line))
                     'face line)
                    (org-air-view--priority-face ?A)))))))

(ert-deftest org-air-r84-3-priority-style-respected-in-review ()
  "r84-3: the review row honours `org-air-priority-style'.
`square (default): the [#A] row prefix carries the priority-square glyph
\(the 2-col slot), NOT the `[#A]' token.  `badge/`text: the `[#A]' token
appears (the pillified/plain cookie) and the square glyph does NOT.
Hard-wiring one style FAILS (each style asserts its own cell shape)."
  (skip-unless (locate-library "org-air"))
  (let ((sq (org-air-r84--square-glyph)))
    ;; `square — the slot glyph, no token.
    (let ((org-air-priority-style 'square))
      (org-air-r84--rendered (org-air-r84--master-specs)
        (let ((txt (substring-no-properties
                    (org-air-r84--row-line "Ship the release"))))
          (should (string-match-p (regexp-quote sq) txt))
          (should-not (string-match-p (regexp-quote "[#A]") txt)))))
    ;; `badge and `text — the `[#A]' token, no square slot.
    (dolist (style '(badge text))
      (let ((org-air-priority-style style))
        (org-air-r84--rendered (org-air-r84--master-specs)
          (let ((txt (substring-no-properties
                      (org-air-r84--row-line "Ship the release"))))
            (should (string-match-p (regexp-quote "[#A]") txt))
            (should-not (string-match-p (regexp-quote sq) txt))))))))

;;;; ===================================================================
;;;; D2 — abandonment as a first-class, period-honest slot
;;;; ===================================================================

(ert-deftest org-air-r84-4-started-then-dropped-is-dropped-only ()
  "r84-4 (D2 core): a started-then-DROPPED item lands in :dropped ONLY.
The heading (STARTED mid-period, then DROPPED + closed in-period, todo
slot \"DROPPED\") is in `:dropped' and in NONE of :completed / :started /
:carried — the exact bucket, all four.  Reverting the D2c gate FAILS
\(the item springs back into Started AND, since DROPPED shares the merged
done-vocab, Completed too)."
  (skip-unless (locate-library "org-air"))
  (org-air-r84--rendered (org-air-r84--master-specs)
    (let ((data (org-air-r84--data (org-air-r84--w29-0) (org-air-r84--w29-1))))
      (should (org-air-r84--in "Abandoned effort" data :dropped))
      (should-not (org-air-r84--in "Abandoned effort" data :completed))
      (should-not (org-air-r84--in "Abandoned effort" data :started))
      (should-not (org-air-r84--in "Abandoned effort" data :carried)))))

(ert-deftest org-air-r84-5-genuine-started-stays-started ()
  "r84-5: a genuine started+active item IS in Started (not over-evicted).
A `* STARTED …' with a mid-period `:CREATED:' and no drop is in
`:started' and absent from `:dropped'.  Reverting the abandoned gate to
fire on mere done-ness (rather than a cancelled keyword) FAILS."
  (skip-unless (locate-library "org-air"))
  (org-air-r84--rendered (org-air-r84--master-specs)
    (let ((data (org-air-r84--data (org-air-r84--w29-0) (org-air-r84--w29-1))))
      (should (org-air-r84--in "Genuine active work" data :started))
      (should-not (org-air-r84--in "Genuine active work" data :dropped)))))

(ert-deftest org-air-r84-6-done-in-period-stays-completed ()
  "r84-6: a DONE-in-period item stays in Completed (DONE is not dropped).
The `* DONE …' closed in-period (todo \"DONE\") is in `:completed' and
absent from `:dropped'; `org-air-view--dropped-keyword-p' is nil for
\"DONE\".  Making the gate catch every done keyword FAILS."
  (skip-unless (locate-library "org-air"))
  (should-not (org-air-view--dropped-keyword-p "DONE"))
  (org-air-r84--rendered (org-air-r84--master-specs)
    (let ((data (org-air-r84--data (org-air-r84--w29-0) (org-air-r84--w29-1))))
      (should (org-air-r84--in "Plain finished" data :completed))
      (should-not (org-air-r84--in "Plain finished" data :dropped)))))

(ert-deftest org-air-r84-7-dropped-item-still-counts-time ()
  "r84-7: a dropped item's clocked hours STILL credit Time invested.
The r84-4 heading has a 4h in-period clock; its seconds are in
`:time-items' / `:time-total' despite being Dropped (time attributed
where it was clocked — \"4h before I gave up\").  Gating the time fold on
abandonment FAILS."
  (skip-unless (locate-library "org-air"))
  (org-air-r84--rendered (org-air-r84--master-specs)
    (let* ((data (org-air-r84--data (org-air-r84--w29-0) (org-air-r84--w29-1)))
           (cell (assoc (org-air-test-find-item "Abandoned effort"
                                                (org-air-r84--items))
                        (plist-get data :time-items))))
      ;; it is Dropped…
      (should (org-air-r84--in "Abandoned effort" data :dropped))
      ;; …yet its 4h (14400s) are in the time fold and the total.
      (should cell)
      (should (= (cdr cell) 14400))
      (should (>= (plist-get data :time-total) 14400)))))

(ert-deftest org-air-r84-8-abandoned-p-is-period-honest ()
  "r84-8: `--abandoned-p' requires an IN-PERIOD close (period-honesty).
`Late abandon' starts in W29 but its DROP falls in W30.  In W29 it is
NOT abandoned (no in-period close) — `--abandoned-p' is nil and it reads
`:started'; in W30 `--abandoned-p' returns the drop epoch and it reads
`:dropped' (and NOT :started/:completed).  Dropping the in-period-stamp
clause (routing on the keyword alone) FAILS W29 (it would mis-drop there
too)."
  (skip-unless (locate-library "org-air"))
  (org-air-r84--rendered (org-air-r84--master-specs)
    (let* ((item (org-air-test-find-item "Late abandon" (org-air-r84--items)))
           (w29 (org-air-r84--data (org-air-r84--w29-0) (org-air-r84--w29-1) t))
           (w30 (org-air-r84--data (org-air-r84--w30-0) (org-air-r84--w30-1) nil)))
      ;; the final keyword IS a dropped spelling in BOTH periods…
      (should (org-air-view--dropped-keyword-p (org-air-item-todo item)))
      ;; …but only the in-period close makes it abandoned-here.
      (should-not (org-air-review--abandoned-p item
                                               (org-air-r84--w29-0)
                                               (org-air-r84--w29-1)))
      (should (org-air-review--abandoned-p item
                                           (org-air-r84--w30-0)
                                           (org-air-r84--w30-1)))
      ;; W29: started, not dropped.  W30: dropped, not started/completed.
      (should (org-air-r84--in "Late abandon" w29 :started))
      (should-not (org-air-r84--in "Late abandon" w29 :dropped))
      (should (org-air-r84--in "Late abandon" w30 :dropped))
      (should-not (org-air-r84--in "Late abandon" w30 :started))
      (should-not (org-air-r84--in "Late abandon" w30 :completed)))))

(ert-deftest org-air-r84-9-conditional-dropped-section-and-summary ()
  "r84-9: the Dropped section + Summary count appear ONLY when non-empty.
The W29 master corpus has two drops: the render shows a `⊘ Dropped 2'
section header whose body holds EXACTLY those two rows (each enclosed by
the `dropped' section), and the rail Summary carries a `2   dropped' row.
A drop-free corpus renders NO Dropped section header and NO Summary
`dropped' row.  Reverting the conditional D2d/D2e append FAILS one half
or the other."
  (skip-unless (locate-library "org-air"))
  ;; Positive: two drops -> the section + Summary row appear.
  (org-air-r84--rendered (org-air-r84--master-specs)
    (let ((txt (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p
               (concat (regexp-quote (org-air-layout-glyph 'dropped))
                       " Dropped 2")
               txt))
      ;; the two dropped rows sit under the `dropped' section…
      (should (eq (org-air-r84--row-section "Abandoned effort") 'dropped))
      (should (eq (org-air-r84--row-section "Second abandon") 'dropped))
      ;; …and no OTHER row is filed there.
      (let ((n 0) (pos (point-min)))
        (while (setq pos (text-property-not-all pos (point-max)
                                                'org-air-item nil))
          (let ((item (get-text-property pos 'org-air-item)))
            (when (eq (org-air-r84--row-section (org-air-item-title item))
                      'dropped)
              (setq n (1+ n))))
          (setq pos (next-single-property-change pos 'org-air-item
                                                 nil (point-max))))
        (should (= n 2)))
      ;; the rail Summary carries the conditional `dropped' count row.
      (should (string-match-p "2[ ]+dropped" txt))))
  ;; Negative: a drop-free corpus -> no section, no Summary row.
  (org-air-r84--rendered
      '(("inbox.org" . "#+title: inbox\n")
        ("d.org" . "#+title: d\n#+TODO: TODO | DONE\n\n\
* DONE Just finished\n:PROPERTIES:\n:CREATED: [2026-05-01 Fri 09:00]\n:END:\n\
:LOGBOOK:\n- State \"DONE\"       from \"TODO\"       [2026-07-15 Wed 09:00]\n:END:\n"))
    (let ((txt (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "Completed 1" txt))
      (should-not (string-match-p "Dropped" txt))
      (should-not (string-match-p "dropped" txt)))))

(ert-deftest org-air-r84-10-mirror-collapse-extends-to-dropped ()
  "r84-10: mirror collapse extends to the Dropped section.
Two same-title / same-day DROPPED mirrors (two files) collapse to ONE
Dropped row under `org-air-review-collapse-mirrors' (the default): the
collapsed fold's `:dropped' has one row, and the render shows one row.
Omitting `:dropped' from `--collapse-data' FAILS (two rows leak)."
  (skip-unless (locate-library "org-air"))
  (let ((drop
         (concat "#+TODO: TODO | DONE DROPPED\n\n"
                 "* DROPPED Duplicate abandon\n"
                 ":PROPERTIES:\n:CREATED: [2026-05-01 Fri 09:00]\n:END:\n"
                 ":LOGBOOK:\n- State \"DROPPED\"    from \"TODO\"    \
[2026-07-16 Thu 12:00]\n:END:\n")))
    (org-air-r84--rendered
        `(("inbox.org" . "#+title: inbox\n")
          ("a.org" . ,(concat "#+title: a\n" drop))
          ("b.org" . ,(concat "#+title: b\n" drop)))
      ;; the raw fold has TWO dropped rows; collapse-data merges to ONE.
      (let* ((raw (org-air-r84--data (org-air-r84--w29-0)
                                     (org-air-r84--w29-1)))
             (org-air-review-collapse-mirrors t)
             (collapsed (org-air-review--collapse-data
                         (copy-sequence raw))))
        (should (= (length (plist-get raw :dropped)) 2))
        (should (= (length (plist-get collapsed :dropped)) 1)))
      ;; the render (collapse on by default) shows exactly ONE row.
      (let ((n 0) (pos (point-min)))
        (while (setq pos (text-property-not-all pos (point-max)
                                                'org-air-item nil))
          (when (eq (org-air-r84--row-section
                     (org-air-item-title
                      (get-text-property pos 'org-air-item)))
                    'dropped)
            (setq n (1+ n)))
          (setq pos (next-single-property-change pos 'org-air-item
                                                 nil (point-max))))
        (should (= n 1))))))

(ert-deftest org-air-r84-11-backlog-orthogonal-to-abandonment ()
  "r84-11: the abandoned gate ignores the `:backlog:' tag (R83 orthogonal).
A `:backlog:'-tagged DONE-in-period heading is `:completed'; the same
tag on a DROPPED-in-period heading is `:dropped'.  The routing is by the
final keyword, never the tag — both items carry `backlog' yet land by
their outcome.  Making the gate tag-sensitive FAILS."
  (skip-unless (locate-library "org-air"))
  (org-air-r84--rendered (org-air-r84--master-specs)
    (let* ((items (org-air-r84--items))
           (data (org-air-r84--data (org-air-r84--w29-0) (org-air-r84--w29-1)))
           (ship (org-air-test-find-item "Ship the release" items))
           (second (org-air-test-find-item "Second abandon" items)))
      ;; both really carry the backlog tag…
      (should (member "backlog" (org-air-item-tags ship)))
      (should (member "backlog" (org-air-item-tags second)))
      ;; …yet the DONE one is Completed and the DROPPED one is Dropped.
      (should (org-air-r84--in "Ship the release" data :completed))
      (should-not (org-air-r84--in "Ship the release" data :dropped))
      (should (org-air-r84--in "Second abandon" data :dropped))
      (should-not (org-air-r84--in "Second abandon" data :completed)))))

(ert-deftest org-air-r84-12-header-n-done-excludes-drops ()
  "r84-12: the header's \"N done\" counts completions only (honest).
The W29 corpus has 2 DONE + 2 DROPPED: `done-count' (the collapsed
`:completed' length) is 2, and the header reads \"2 done\", not \"4\".
Reverting D2c (a drop back into Completed) makes it \"4 done\"."
  (skip-unless (locate-library "org-air"))
  (org-air-r84--rendered (org-air-r84--master-specs)
    (let* ((data (org-air-review--collapse-data
                  (org-air-r84--data (org-air-r84--w29-0)
                                     (org-air-r84--w29-1) t)))
           (header (save-excursion
                     (goto-char (point-min))
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))))
      (should (= (length (plist-get data :completed)) 2))
      (should (string-match-p "2 done" header))
      (should-not (string-match-p "4 done" header)))))

;;;; ===================================================================
;;;; Invariants — period math (R62), R79 face split, no rescan
;;;; ===================================================================

(ert-deftest org-air-r84-13-period-math-unaffected ()
  "r84-13: R84 is a per-item routing change — period math is unchanged.
`org-air-review--period-bounds' matches the independent local-midnight
oracle for every rung of the ladder (week/fortnight/month/quarter/year),
and the ladder is exactly (week fortnight month quarter year).  Any range
regression (a shifted boundary, a reordered/renamed rung) FAILS."
  (skip-unless (locate-library "org-air"))
  (let ((anchor (org-air-r84--epoch 2026 7 15 12)))
    (should (equal (org-air-review--period-bounds 'week anchor)
                   (cons (org-air-r84--epoch 2026 7 13)
                         (org-air-r84--epoch 2026 7 20))))
    (should (equal (org-air-review--period-bounds 'fortnight anchor)
                   (cons (org-air-r84--epoch 2026 7 6)
                         (org-air-r84--epoch 2026 7 20))))
    (should (equal (org-air-review--period-bounds 'month anchor)
                   (cons (org-air-r84--epoch 2026 7 1)
                         (org-air-r84--epoch 2026 8 1))))
    (should (equal (org-air-review--period-bounds 'quarter anchor)
                   (cons (org-air-r84--epoch 2026 7 1)
                         (org-air-r84--epoch 2026 10 1))))
    (should (equal (org-air-review--period-bounds 'year anchor)
                   (cons (org-air-r84--epoch 2026 1 1)
                         (org-air-r84--epoch 2027 1 1))))
    ;; the month rollover and the ladder order (R62-3) are intact.
    (should (equal (org-air-review--period-bounds
                    'month (org-air-r84--epoch 2026 12 31))
                   (cons (org-air-r84--epoch 2026 12 1)
                         (org-air-r84--epoch 2027 1 1))))
    (should (equal org-air-review--range-ladder
                   '(week fortnight month quarter year)))))

(ert-deftest org-air-r84-14-shared-dropped-predicate-inert-for-r79 ()
  "r84-14: `--dropped-keyword-p' is the ONE vocabulary test (R79 inert).
The extracted predicate is t for every cancelled/abandoned spelling in
`org-air-view--dropped-keyword-names' (case-insensitively) and nil for a
completion / not-done keyword; `--merged-vocab-face' still resolves a
DROPPED keyword to `org-air-face-dropped' and a DONE keyword to
`org-air-face-done' (the R79 split, unchanged).  Forking the predicate or
mutating `dropped-keyword-names' FAILS."
  (skip-unless (locate-library "org-air"))
  ;; every cancelled spelling reads dropped (case-insensitive)…
  (dolist (kw '("DROPPED" "DROP" "CANCELLED" "CANCELED"
                "KILL" "KILLED" "ABANDONED" "dropped" "Cancelled"))
    (should (org-air-view--dropped-keyword-p kw)))
  ;; …and a completion / not-done keyword does NOT.
  (dolist (kw '("DONE" "COMP" "TODO" "STARTED" "WIP"))
    (should-not (org-air-view--dropped-keyword-p kw)))
  (should-not (org-air-view--dropped-keyword-p nil))
  ;; the R79 face split still agrees by construction.
  (should (eq (org-air-view--merged-vocab-face "DROPPED" t)
              'org-air-face-dropped))
  (should (eq (org-air-view--merged-vocab-face "DONE" t)
              'org-air-face-done))
  (should-not (eq (org-air-view--merged-vocab-face "DROPPED" t)
                  (org-air-view--merged-vocab-face "DONE" t))))

(provide 'org-air-round84-test)
;;; org-air-round84-test.el ends here
