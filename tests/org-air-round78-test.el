;;; org-air-round78-test.el --- executing ERTs for round-78 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for round-78 (air/v0.1/org-air-round78-design.org):
;; `org-air-goto-date' — jump to a given date's items via an
;; `org-read-date' prompt (nil t; PROMPT "Jump to date: "; DEFAULT-TIME
;; the focused day when a day view is up, else today) delegating
;; UNCHANGED to `org-air-view-day' (R55-1 owner routing, R53 cached
;; items — the jump queries NOTHING).  Bound at `g d' on the board's
;; g-prefix via the R35-1 registry; `;;;###autoload' cookie; with no
;; live board the command opens `org-air-view' first (the
;; `org-air-process-inbox' precedent).  The spec's eight seams E1..E8
;; map onto the ERTs below:
;;
;;   r78-1  a non-interactive `(org-air-goto-date TARGET)' lands the
;;          board's day state on TARGET (no prompt fires — the
;;          Decision-1 arity seam) and the day pane groups THAT date's
;;          fixture items Deadline / Scheduled / Logged-created.
;;   r78-2  a far no-item date is a rendered STATE, not an error:
;;          "0 items" header, "Nothing on this day.", day state moved.
;;   r78-3  `M-x' from an UNRELATED buffer routes to the OWNER board
;;          (R55-1): day state lands in the board's buffer-locals,
;;          never the invoking buffer's.
;;   r78-4  the no-rescan spy: the jump runs ZERO real
;;          `org-air-query-items' calls (R53/R55-1 — cached items).
;;   r78-5  `g d' resolves on the g-prefix + board map; the command is
;;          a command; the literal `;;;###autoload' cookie sits above
;;          the defun; the R35-1 knob strips the binding on a nil sync.
;;   r78-6  the reader's argument contract: WITH-TIME nil, TO-TIME t,
;;          PROMPT "Jump to date: ", DEFAULT-TIME nil from the plain
;;          board and the FOCUSED day when a day view is up (the
;;          Decision-2 nudge default); the command lands on the mocked
;;          return either way.
;;   r78-7  the R77 carve-out is REACHABLE by date: under
;;          `org-air-task-requires-todo' t the keyword-less routine is
;;          absent from the rendered board (the demotion precondition),
;;          yet the jump to its day shows it under "Scheduled" — the
;;          day view is planning-slot keyed, not task-gated.
;;   r78-8  composition: `>' from the landed day steps +1d
;;          (`org-air-calendar-next' reads the jump-set day); the `q'
;;          layer (R28-2 layer 2) returns to the full board — day state
;;          nil, day header gone, the board rows back.
;;   r78-9  (audit round) the Decision-4 cold open: with NO live board
;;          anywhere the command opens `org-air-view' FIRST and then
;;          jumps — from Lisp and interactively (the reader's LENIENT
;;          owner resolve must not pre-empt the open with the R55-1
;;          `user-error'); a quit AT the prompt opens nothing.
;;   r78-10 (audit round) DATE beats a calendar CELL at point AND the
;;          sticky day: invoking over a propertized `org-air-day' cell
;;          still lands on the PASSED date; `<' steps -1d from it.
;;   r78-11 (audit round) the R50-2 help surface: the Navigation group
;;          carries the `org-air-goto-date' row and the LIVE-derived
;;          key text `g d' renders in `*org-air-help*'; the collision
;;          audit holds — `j' is still the R29-2 vim-ish next-line.
;;
;; Harness: the standard batch board (pure batch — `noninteractive'
;; stays t, so the R55-1 focus hop is gated off and no windows are
;; involved; the R55 window-landing matrix is owned by round55-test
;; E1-E5) over a round-78 temp corpus with items on the fixed target
;; day <2026-07-25 Sat>; clock frozen at `org-air-test-now'
;; (Mon 2026-06-15); anti-tautology render guards active;
;; `org-air-rail-min-width' bound above the width so the board renders
;; board-only (clean single-column day-pane lines for the text
;; asserts).  The no-query spy reuses the `org-air-r55--counting-
;; queries' shape (the REAL function still runs; the count must stay 0).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)

(when (locate-library "org-air")
  (require 'org-air))

;;;; -------------------------------------------------------------------
;;;; Corpus scaffolding (the r77 temp-corpus pattern).
;;;; -------------------------------------------------------------------

(defvar org-air-r78--dir nil
  "The temp corpus directory of the current `org-air-r78--with-corpus'.")

(defun org-air-r78--reset-tables ()
  "Clear the GLOBAL query-layer tables so corpus entries never leak.
File-meta, the visit ledger and the denote-ID index are session globals
\(never cleared by the scan), so every test starts and ends empty."
  (clrhash org-air-query--file-meta)
  (clrhash org-air-query--visits)
  (clrhash org-air-query--denote-id-index)
  (setq org-air-query--link-graph-dirty nil))

(defmacro org-air-r78--with-corpus (specs &rest body)
  "Create a temp Org corpus from SPECS and run BODY against it.
SPECS is a list of (NAME . CONTENT) files written into a fresh temp
directory.  Binds `org-air-files' to the directory, `org-air-inbox-file'
to its inbox.org and a temp `org-air-cache-file'.  Starts from EMPTY
query tables and cleans up the tables, every corpus-visiting buffer and
the directory."
  (declare (indent 1) (debug t))
  `(let* ((org-air-r78--dir (make-temp-file "org-air-r78-" t)))
     (unwind-protect
         (progn
           (org-air-r78--reset-tables)
           (pcase-dolist (`(,name . ,content) ,specs)
             (let ((path (expand-file-name name org-air-r78--dir))
                   (coding-system-for-write 'utf-8-unix))
               (make-directory (file-name-directory path) t)
               (write-region (or content "") nil path nil 'silent)))
           (let ((org-air-files (list org-air-r78--dir))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-r78--dir))
                 (org-air-cache-file
                  (expand-file-name ".cache/board.eld" org-air-r78--dir)))
             ,@body))
       (org-air-query-teardown)
       (org-air-r78--reset-tables)
       (let ((kill-buffer-query-functions nil))
         (dolist (buf (buffer-list))
           (let ((fn (buffer-file-name buf)))
             (when (and fn (string-prefix-p org-air-r78--dir fn))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r78--dir t))))

(defconst org-air-r78--target (encode-time '(0 0 0 25 7 2026 nil -1 nil))
  "The round's fixture day: Sat 2026-07-25 — 40 days past the frozen now.")

(defconst org-air-r78--routine-day (encode-time '(0 0 0 20 7 2026 nil -1 nil))
  "The R77 routine's day: Mon 2026-07-20.")

(defconst org-air-r78--far-day (encode-time '(0 0 0 1 1 2031 nil -1 nil))
  "A far date with ZERO corpus items: 2031-01-01 (the E2 empty state).")

(defconst org-air-r78--corpus
  `(("dated.org"
     . ,(concat "#+title: dated\n\n"
                "* TODO Ship the launch email\n"
                "DEADLINE: <2026-07-25 Sat>\n\n"
                "* TODO Rehearse the demo\n"
                "SCHEDULED: <2026-07-25 Sat>\n\n"
                "* Capture from the launch call\n"
                ":PROPERTIES:\n"
                ":CREATED: [2026-07-25 Sat 09:00]\n"
                ":END:\n"))
    ("routines.org"
     . "* Water plants\nSCHEDULED: <2026-07-20 Mon ++2w>\nRoutine.\n")
    ("inbox.org" . "#+title: inbox\n* Sort receipts\n"))
  "Round-78 corpus: one deadline + one scheduled + one :CREATED: item on
the target day, the R77 keyword-less routine on its own day, and one
inbox row so the FULL board renders a section (the E8 return assert).")

;;;; -------------------------------------------------------------------
;;;; Board harness + helpers.
;;;; -------------------------------------------------------------------

(defmacro org-air-r78--with-board (&rest body)
  "Render the live batch board over the round-78 corpus; BODY in its buffer.
Pure batch (`noninteractive' stays t — the R55-1 focus hop is gated
off, so no windows are involved and day state is asserted on the board
buffer directly).  Clock frozen at `org-air-test-now'; anti-tautology
render guards active; width pinned at 120 with the rail threshold above
it, so the item pane renders board-only (single-column day-pane lines)."
  (declare (indent 0) (debug t))
  `(org-air-r78--with-corpus org-air-r78--corpus
     (org-air-viewport-test--with-frozen-now
       (unwind-protect
           (org-air-viewport-test--with-render-guards
             (let ((org-air-view-width 120)
                   (org-air-rail-min-width 200))
               (org-air)
               (let ((buf (get-buffer org-air-view-buffer-name)))
                 (should buf)
                 (with-current-buffer buf
                   ,@body))))
         (when (get-buffer org-air-view-buffer-name)
           (let ((kill-buffer-query-functions nil))
             (kill-buffer org-air-view-buffer-name)))))))

(defmacro org-air-r78--counting-queries (counter &rest body)
  "Run BODY counting `org-air-query-items' calls into the variable COUNTER.
The real function still runs (nothing is stubbed away) — the count is
the R53/R55-1 no-query guard: the jump renders from the owner's CACHED
items, so the count must stay 0."
  (declare (indent 1) (debug t))
  `(let ((,counter 0)
         (org-air-r78--real-query (symbol-function 'org-air-query-items)))
     (cl-letf (((symbol-function 'org-air-query-items)
                (lambda (&rest args)
                  (cl-incf ,counter)
                  (apply org-air-r78--real-query args))))
       ,@body)))

(defun org-air-r78--text (&optional buffer)
  "Return BUFFER's (default: current) text without properties."
  (with-current-buffer (or buffer (current-buffer))
    (substring-no-properties (buffer-string))))

(defun org-air-r78--day-header (date)
  "Return the R6 day-view header rendering of DATE."
  (format-time-string "%A %-d %B %Y" date))

(defun org-air-r78--board-day-key (board)
  "Return the day-key of BOARD's buffer-local focused day, or nil."
  (let ((day (buffer-local-value 'org-air-view--day board)))
    (and day (org-air-view--day-key day))))

(defun org-air-r78--pos (needle text &optional start)
  "Return the position of NEEDLE in TEXT from START, case-sensitively.
Asserts the match exists — the day-pane ordering helper."
  (let ((case-fold-search nil))
    (let ((pos (string-match (regexp-quote needle) text start)))
      (should pos)
      pos)))

;;;; -------------------------------------------------------------------
;;;; r78-1 (E1) — the non-interactive jump lands on THAT date.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r78-1-jump-lands-on-that-date ()
  "A Lisp `(org-air-goto-date TARGET)' NEVER prompts and lands on TARGET.
Decision 1's arity seam: DATE is a required argument, so the core path
needs no reader mock.  Asserts: the board's buffer-local
`org-air-view--day' day-keys to 2026-07-25; the buffer carries the day
header; and the day pane groups THAT date's items — the deadline title
under \"Deadline\", the scheduled one under \"Scheduled\", the
:CREATED: capture under \"Logged / created\" (ordering asserted via
positions in the rendered text)."
  (skip-unless (locate-library "org-air"))
  (org-air-r78--with-board
    (let ((board (current-buffer)))
      (org-air-goto-date org-air-r78--target)
      (should (equal (org-air-r78--board-day-key board) "2026-07-25"))
      (let ((text (org-air-r78--text board)))
        (should (string-match-p
                 (regexp-quote (org-air-r78--day-header org-air-r78--target))
                 text))
        ;; the grouped day pane for THAT date: each group header precedes
        ;; its member title, in the fixed Deadline > Scheduled > Logged
        ;; order the renderer emits.
        (let* ((d (org-air-r78--pos "Deadline" text))
               (d-item (org-air-r78--pos "Ship the launch email" text d))
               (s (org-air-r78--pos "Scheduled" text d-item))
               (s-item (org-air-r78--pos "Rehearse the demo" text s))
               (c (org-air-r78--pos "Logged / created" text s-item))
               (c-item (org-air-r78--pos "Capture from the launch call"
                                         text c)))
          (should (< d d-item s s-item c c-item)))
        ;; not today's view: the frozen-now day header is absent.
        (should-not (string-match-p
                     (regexp-quote (org-air-r78--day-header org-air-test-now))
                     text))))))

;;;; -------------------------------------------------------------------
;;;; r78-2 (E2) — an empty day is a rendered state, not an error.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r78-2-empty-day-is-a-state-not-an-error ()
  "A jump to a far no-item date renders the R6 empty state.
No error signalled; the header shows \"0 items\"; \"Nothing on this
day.\" is present; `org-air-view--day' really moved to that date."
  (skip-unless (locate-library "org-air"))
  (org-air-r78--with-board
    (let ((board (current-buffer)))
      (org-air-goto-date org-air-r78--far-day)
      (should (equal (org-air-r78--board-day-key board) "2031-01-01"))
      (let ((text (org-air-r78--text board)))
        (should (string-match-p
                 (regexp-quote (org-air-r78--day-header org-air-r78--far-day))
                 text))
        (should (string-match-p "0 items" text))
        (should (string-match-p "Nothing on this day\\." text))))))

;;;; -------------------------------------------------------------------
;;;; r78-3 (E3) — M-x from an unrelated buffer routes to the OWNER.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r78-3-owner-routing-from-mx ()
  "`call-interactively' from an UNRELATED buffer lands in the BOARD.
With the board live and a temp buffer current, the mocked reader
returns TARGET and the day state lands in the board's buffer-locals
\(R55-1 owner routing), NOT the invoking buffer's; the board's text
carries the day header.  (Window-focus assertions are owned by the R55
harness; the `noninteractive' gate keeps this pure-batch run
window-free.)"
  (skip-unless (locate-library "org-air"))
  (org-air-r78--with-board
    (let ((board (current-buffer)))
      (with-temp-buffer
        (cl-letf (((symbol-function 'org-read-date)
                   (lambda (&rest _) org-air-r78--target)))
          (call-interactively #'org-air-goto-date))
        ;; the INVOKING buffer never received day state.
        (should-not org-air-view--day))
      (should (equal (org-air-r78--board-day-key board) "2026-07-25"))
      (should (string-match-p
               (regexp-quote (org-air-r78--day-header org-air-r78--target))
               (org-air-r78--text board))))))

;;;; -------------------------------------------------------------------
;;;; r78-4 (E4) — the jump queries NOTHING (R53/R55-1).
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r78-4-no-rescan-spy ()
  "The jump renders from the owner's CACHED items — zero query calls.
E1's jump wrapped in the query counter: the count of REAL
`org-air-query-items' calls during the jump must be 0 (the R53 law,
scoped to a live board; the cold `org-air-view' open's scan belongs to
the board open, which precedes the spy here)."
  (skip-unless (locate-library "org-air"))
  (org-air-r78--with-board
    (let ((board (current-buffer)))
      (org-air-r78--counting-queries queries
        (org-air-goto-date org-air-r78--target)
        (should (= queries 0)))
      (should (equal (org-air-r78--board-day-key board) "2026-07-25")))))

;;;; -------------------------------------------------------------------
;;;; r78-5 (E5) — key bound via the R35-1 registry; autoload cookie.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r78-5-key-bound-and-autoloaded ()
  "`g d' resolves to `org-air-goto-date'; cookie present; knob strips it.
Both `(lookup-key org-air-g-prefix-map \"d\")' and the full board
`(kbd \"g d\")' chain resolve; the command is a command; the literal
`;;;###autoload' cookie sits immediately above the defun in
org-air-view.el (the prior rounds' source-scrape assert).  Knob leg:
with `org-air-use-default-keybindings' nil (synced), `g d' no longer
resolves; the knob and maps are restored + re-synced after."
  (skip-unless (locate-library "org-air"))
  ;; bindings at the shipped default (knob t).
  (should (eq (lookup-key org-air-g-prefix-map "d") 'org-air-goto-date))
  (should (eq (lookup-key org-air-view-mode-map (kbd "g d"))
              'org-air-goto-date))
  (should (commandp 'org-air-goto-date))
  ;; the literal autoload cookie above the defun (source scrape).
  (let* ((loaded (locate-library "org-air-view"))
         (el (and loaded (concat (file-name-sans-extension loaded) ".el"))))
    (should (and el (file-readable-p el)))
    (with-temp-buffer
      (insert-file-contents el)
      (goto-char (point-min))
      (let ((case-fold-search nil))
        (should (re-search-forward
                 "^;;;###autoload[ \t]*\n(defun org-air-goto-date "
                 nil t)))))
  ;; knob leg: nil sync strips the installer-owned pair like every other
  ;; default; always restore + re-sync (the maps are GLOBAL — r35 pattern).
  (let ((org-air-r78--saved org-air-use-default-keybindings))
    (unwind-protect
        (progn
          (setq org-air-use-default-keybindings nil)
          (org-air--sync-default-keybindings)
          (should-not (lookup-key org-air-g-prefix-map "d"))
          (should-not (eq (lookup-key org-air-view-mode-map (kbd "g d"))
                          'org-air-goto-date)))
      (setq org-air-use-default-keybindings org-air-r78--saved)
      (setq org-air--default-keybindings-state 'unset)
      (org-air--sync-default-keybindings))
    ;; restored: the default binding is back.
    (should (eq (lookup-key org-air-g-prefix-map "d") 'org-air-goto-date))))

;;;; -------------------------------------------------------------------
;;;; r78-6 (E6) — the reader's argument contract + the day-view default.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r78-6-reader-args-and-day-default ()
  "`org-read-date' is called nil t nil \"Jump to date: \" DEFAULT-TIME.
From the PLAIN board DEFAULT-TIME is nil (org-read-date's own today
default); with a day view up it is the FOCUSED day (the Decision-2
nudge default — a re-jump's relative entry composes with the shown
day).  The command lands on the mocked return either way."
  (skip-unless (locate-library "org-air"))
  (org-air-r78--with-board
    (let ((board (current-buffer))
          (captured nil))
      ;; leg 1: plain board (no day view) -> DEFAULT-TIME nil.
      (cl-letf (((symbol-function 'org-read-date)
                 (lambda (&rest args)
                   (setq captured args)
                   org-air-r78--target)))
        (call-interactively #'org-air-goto-date))
      (should (null (nth 0 captured)))          ; WITH-TIME nil
      (should (eq (nth 1 captured) t))          ; TO-TIME t
      (should (null (nth 2 captured)))          ; FROM-STRING nil
      (should (equal (nth 3 captured) "Jump to date: "))
      (should (null (nth 4 captured)))          ; DEFAULT-TIME nil
      (should (equal (org-air-r78--board-day-key board) "2026-07-25"))
      ;; leg 2: day view up on TARGET -> DEFAULT-TIME day-keys to it.
      (setq captured nil)
      (cl-letf (((symbol-function 'org-read-date)
                 (lambda (&rest args)
                   (setq captured args)
                   org-air-r78--routine-day)))
        (call-interactively #'org-air-goto-date))
      (should (nth 4 captured))
      (should (equal (org-air-view--day-key (nth 4 captured)) "2026-07-25"))
      (should (equal (org-air-r78--board-day-key board) "2026-07-20")))))

;;;; -------------------------------------------------------------------
;;;; r78-7 (E7) — the R77-demoted routine is REACHABLE by date.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r78-7-r77-routine-reachable-by-date ()
  "Under `org-air-task-requires-todo' t the routine shows on its day.
Precondition (the R77 demotion re-asserted): the keyword-less
SCHEDULED routine types knowledge and its row is ABSENT from the
rendered full board.  Then `(org-air-goto-date <its day>)' shows it
under \"Scheduled\" in the day pane — the day view groups by PLANNING
SLOTS, not note types (R77's explicit carve-out, now reachable by
date)."
  (skip-unless (locate-library "org-air"))
  (let ((org-air-task-requires-todo t))
    (org-air-r78--with-board
      (let ((board (current-buffer)))
        ;; precondition: demoted — typed knowledge, no board row.
        (let ((routine (org-air-test-find-item "Water plants"
                                               org-air-view--items)))
          (should routine)
          (should (eq (org-air-item-ntype routine) 'knowledge)))
        (should-not (string-match-p "Water plants" (org-air-r78--text board)))
        ;; the jump makes the carve-out reachable: Scheduled on its day.
        (org-air-goto-date org-air-r78--routine-day)
        (should (equal (org-air-r78--board-day-key board) "2026-07-20"))
        (let* ((text (org-air-r78--text board))
               (s (org-air-r78--pos "Scheduled" text))
               (row (org-air-r78--pos "Water plants" text s)))
          (should (< s row)))))))

;;;; -------------------------------------------------------------------
;;;; r78-8 (E8) — day-nav and the return path compose.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r78-8-day-nav-and-return-compose ()
  "`>' steps +1d from the landed date; the `q' layer returns to the board.
After the jump, `org-air-calendar-next' reads the jump-set
`org-air-view--day' and lands TARGET+1d; then `q' (dispatched through
the live keymap — the R28-2 layer-2 day-exit) clears the day state, the
day header is gone and the full board's rows are back."
  (skip-unless (locate-library "org-air"))
  (org-air-r78--with-board
    (let ((board (current-buffer)))
      (org-air-goto-date org-air-r78--target)
      ;; day-nav composes: > steps to Sun 2026-07-26.
      (org-air-calendar-next)
      (should (equal (org-air-r78--board-day-key board) "2026-07-26"))
      (let ((next (time-add org-air-r78--target (days-to-time 1))))
        (should (string-match-p
                 (regexp-quote (org-air-r78--day-header next))
                 (org-air-r78--text board)))
        ;; the return path: `q' = R28-2 layer 2 (no pane is live).
        (call-interactively (key-binding (kbd "q")))
        (should-not (buffer-local-value 'org-air-view--day board))
        (let ((text (org-air-r78--text board)))
          (should-not (string-match-p
                       (regexp-quote (org-air-r78--day-header next))
                       text))
          ;; the FULL board is back: the inbox row renders again.
          (should (string-match-p "Sort receipts" text)))))))

;;;; -------------------------------------------------------------------
;;;; r78-9 (audit) — the Decision-4 cold open: no live board anywhere.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r78-9-cold-open-then-jump ()
  "With NO live board the command opens `org-air-view' FIRST, then jumps.
Leg 1 (Lisp arity): `(org-air-goto-date TARGET)' from cold creates the
`*org-air*' board (org-air-view-mode) and lands its day state on
TARGET.  Leg 2 (interactive): the reader's owner resolve is LENIENT
\(`ignore-errors' — without it the R55-1 `user-error' fires before the
body can open anything), DEFAULT-TIME is nil from cold, and the
command still opens + lands.  Leg 3: a quit AT the prompt opens
NOTHING — the reader runs first, so `C-g' leaves no board behind.
Revert of the `(unless … (org-air-view))' branch fails legs 1 and 2."
  (skip-unless (locate-library "org-air"))
  (org-air-r78--with-corpus org-air-r78--corpus
    (org-air-viewport-test--with-frozen-now
      (unwind-protect
          (org-air-viewport-test--with-render-guards
            (let ((org-air-view-width 120)
                  (org-air-rail-min-width 200)
                  (kill-buffer-query-functions nil))
              ;; leg 1: Lisp call from cold — the board opens, then jumps.
              (should-not (get-buffer org-air-view-buffer-name))
              (org-air-goto-date org-air-r78--target)
              (let ((board (get-buffer org-air-view-buffer-name)))
                (should board)
                (with-current-buffer board
                  (should (derived-mode-p 'org-air-view-mode)))
                (should (equal (org-air-r78--board-day-key board)
                               "2026-07-25"))
                (should (string-match-p
                         (regexp-quote
                          (org-air-r78--day-header org-air-r78--target))
                         (org-air-r78--text board)))
                (kill-buffer board))
              ;; leg 2: interactive from cold — the lenient reader must not
              ;; pre-empt the open; DEFAULT-TIME is nil (no owner to read).
              (should-not (get-buffer org-air-view-buffer-name))
              (let ((captured 'unset))
                (with-temp-buffer
                  (cl-letf (((symbol-function 'org-read-date)
                             (lambda (&rest args)
                               (setq captured args)
                               org-air-r78--target)))
                    (call-interactively #'org-air-goto-date)))
                (should (consp captured))
                (should (null (nth 4 captured))))   ; cold DEFAULT-TIME nil
              (let ((board (get-buffer org-air-view-buffer-name)))
                (should board)
                (should (equal (org-air-r78--board-day-key board)
                               "2026-07-25"))
                (kill-buffer board))
              ;; leg 3: quit AT the prompt — nothing opens.
              (should-not (get-buffer org-air-view-buffer-name))
              (with-temp-buffer
                (cl-letf (((symbol-function 'org-read-date)
                           (lambda (&rest _) (signal 'quit nil))))
                  ;; ERT's `should-error' handler does not trap `quit';
                  ;; catch it explicitly — the quit MUST fire (anti-vacuity)
                  ;; and the body must never have run.
                  (let ((quit-fired nil))
                    (condition-case nil
                        (call-interactively #'org-air-goto-date)
                      (quit (setq quit-fired t)))
                    (should quit-fired))))
              (should-not (get-buffer org-air-view-buffer-name))))
        (when (get-buffer org-air-view-buffer-name)
          (let ((kill-buffer-query-functions nil))
            (kill-buffer org-air-view-buffer-name)))))))

;;;; -------------------------------------------------------------------
;;;; r78-10 (audit) — DATE beats the cell at point + the sticky day.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r78-10-date-beats-cell-at-point ()
  "The PASSED date wins over an `org-air-day' cell at point AND the
sticky `org-air-view--day'.  Precondition (anti-vacuity): from a
foreign buffer with point ON a propertized cell, the bare
`org-air-view-day' DOES land on the cell's day — the cell genuinely
binds through this code path (the rail-cell invoke shape).  Then
`org-air-goto-date' from the SAME point lands on the passed TARGET —
DATE beat both the cell (2026-07-20) and the now-sticky day.  `<'
composes: `org-air-calendar-prev' steps to TARGET-1d."
  (skip-unless (locate-library "org-air"))
  (org-air-r78--with-board
    (let ((board (current-buffer)))
      (with-temp-buffer
        (insert (propertize "x" 'org-air-day org-air-r78--routine-day))
        (goto-char (point-min))
        ;; precondition: the cell at point binds for the bare cell verb.
        (org-air-view-day)
        (should (equal (org-air-r78--board-day-key board) "2026-07-20"))
        ;; DATE beats the cell AND the sticky day.
        (org-air-goto-date org-air-r78--target)
        (should (equal (org-air-r78--board-day-key board) "2026-07-25")))
      (should (string-match-p
               (regexp-quote (org-air-r78--day-header org-air-r78--target))
               (org-air-r78--text board)))
      ;; `<' composes from the landed date: Fri 2026-07-24.
      (with-current-buffer board
        (org-air-calendar-prev))
      (should (equal (org-air-r78--board-day-key board) "2026-07-24")))))

;;;; -------------------------------------------------------------------
;;;; r78-11 (audit) — the help Navigation row + the collision audit.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r78-11-help-row-and-key-audit ()
  "The R50-2 help carries the goto-date Navigation row; `j' is untouched.
Data leg: `org-air-help--board-groups' \"Navigation\" holds the
`org-air-goto-date' pair.  Render leg: `org-air-help' from the live
board derives the LIVE key text — the `*org-air-help*' buffer shows
`g d' beside the row wording (the help cannot lie).  Collision audit:
`j' still resolves to the R29-2 `org-air-next-line' and `g d' to the
new command on the live board."
  (skip-unless (locate-library "org-air"))
  ;; data leg: the pair sits in the Navigation group.
  (let ((nav (cdr (assoc "Navigation" org-air-help--board-groups))))
    (should nav)
    (should (assq 'org-air-goto-date nav)))
  (org-air-r78--with-board
    ;; collision audit on the LIVE board maps.
    (should (eq (key-binding "j") 'org-air-next-line))
    (should (eq (key-binding (kbd "g d")) 'org-air-goto-date))
    (should (eq (key-binding (kbd "?")) 'org-air-help))
    ;; render leg: the row derives to `g d' live.
    (unwind-protect
        (progn
          (org-air-help)
          (let ((help (get-buffer org-air-help-buffer-name)))
            (should help)
            (let ((text (org-air-r78--text help)))
              (should (string-match-p
                       "^  g d +jump to a date's items (day view)" text)))))
      (when (get-buffer org-air-help-buffer-name)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer org-air-help-buffer-name))))))

(provide 'org-air-round78-test)
;;; org-air-round78-test.el ends here
