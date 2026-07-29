;;; org-air-round95-test.el --- R95: coverage closure + a fence that sees -*- lexical-binding: t; -*-

;;; Commentary:
;; Permanent coverage for R95 — the round the R94 review scoped as
;; "coverage closure, not model change".  No bucket was added, no section
;; list changed, no cache version bumped.  Six one-idea fixes, each of
;; which makes an EXISTING promise true, and one instrument repair that
;; matters more than any of them.
;;
;; THE SIX FIXES, one sentence each.
;;
;;   FU1  the catch-all must negate what the date sections READ.
;;        `org-air-classify--untracked-p' asked `--dated-p' (scheduled ∨
;;        deadline ∨ an active `<ts>' ANYWHERE in the subtree) while
;;        Overdue and Upcoming read only the two PLAN slots.  Every
;;        heading in the gap — a `TODO' whose only date is a bare
;;        `<2026-06-14 Sun>' in its body — was excused from the catch-all
;;        and never reached a date section: NO ROW ANYWHERE, at any file
;;        age, with the stamp yesterday or tomorrow, answering no `is:'
;;        token at all.  `org-air-classify--planned-p' closes it.
;;   FU2  the Untracked ordering reproduced the pathology it was created
;;        to solve: its rank key is a FILE fact shared by every heading in
;;        a file, so one cold `someday.org' owned the capped section.  A
;;        per-file quota of 2 while collapsed fixes it WITHOUT touching
;;        the order law — the window is returned as a SUBSEQUENCE.
;;   FU3  `org-air-query--planning-keyword-regexp' is anchored, so
;;        `:LAST_DEADLINE:' / `:ORIG_SCHEDULED:' keep their own stamps.
;;   FU4  the `recency' sort key is the MEASURED clock; a heading org-air
;;        never measured ranks LAST rather than by its file's mtime.
;;   FU6  an unprocessed inbox capture is not Untracked — the clause lives
;;        INSIDE `--untracked-p', so the R72 filter⇔bucket law survives.
;;   FU7  absurd day counts print `10y+'.  Presentation only: the fact and
;;        every sort key keep the true number.
;;
;; AND THE ONE THAT IS NOT A FEATURE — THE COMPILED-ONLY FENCE GAP.
;;
;; R95's first implementation of the quota used `(memq item chosen)'.  It
;; is a genuine linearity defect and `r90-50' exists to catch exactly
;; that: it counts `memq'/`assq' calls whose object is an `org-air-item'
;; through a `cl-letf' shim.  The COMPILED gate passed anyway, because
;; `memq' is a BYTE OPCODE and a compiled caller never consults the
;; function cell the shim replaced.  Only a run of the same suite with no
;; `.elc' present reddened, at `forbidden=5020'.
;;
;; That is the 1204-green-tests failure again in miniature: the strongest
;; performance fence in the project was blind IN THE MODE WE GATE ON, and
;; nothing said so.  `r95-17' .. `r95-20' close it: the gap is asserted as
;; a FACT (so it can never be forgotten), the repair is proven to work,
;; and the repaint path is then fenced in BOTH modes by forcing the
;; production functions to be INTERPRETED for the duration of the probe —
;; exactly as `load' would evaluate them, compiler macros and all.
;;
;; The ERTs below, one invariant each:
;;
;;   r95-1   FU1 unit: `--planned-p' is the SCHEDULED/DEADLINE pair and
;;           nothing else, and `--untracked-p' negates IT.
;;   r95-2   FU1 through the REAL renderer: the body-timestamp heading has
;;           a row, at three plan distances and both file ages.
;;   r95-3   FU1: `is:nodate' and `is:untracked' are INDEPENDENT — each
;;           has a member the other lacks — and the R72 law holds for both.
;;   r95-4   FU2: the per-file quota — at most two rows from any one file,
;;           and the visible sample spans the corpus.
;;   r95-5   FU2: the window is a SUBSEQUENCE of the sorted order, so the
;;           PAINTED cells stay monotone worst-first (the FIX-2 law).
;;   r95-6   FU2: the slack fill — a single-file board still fills the cap.
;;   r95-7   FU2 counts MEMBERS: badge, Summary, `…and N more' and
;;           `is:untracked' are untouched by a display rule.
;;   r95-8   FU2: TAB on the fold row lands on the first row the QUOTA
;;           hid, never on one that was visible all along.
;;   r95-9   FU3: the anchored planning keyword.
;;   r95-10  FU4: `--item-activity' IS the measured clock; a FUTURE plan
;;           cannot move it.
;;   r95-11  FU4: `--sort-by-recency' — least-recent first, unmeasured
;;           LAST in title-then-query order, `O' reverses the whole thing.
;;   r95-12  FU4 end to end: `recency' on the real board agrees with the
;;           numbers the rows print.
;;   r95-13  FU6: the inbox clause, and coverage preserved BY CONSTRUCTION.
;;   r95-14  FU7: `--days-text' clamps at `10y+'.
;;   r95-15  FU7: the FACT is not clamped — sorts, thresholds and
;;           predicates keep the true number.
;;   r95-16  FU7: every day count the board prints goes through the shared
;;           formatter (the call sites, on a real render).
;;   r95-17  THE GAP, as a fact: a byte-compiled `memq' bypasses the shim.
;;   r95-18  THE REPAIR: forced interpretation restores the shim's sight.
;;   r95-19  THE FENCE: the repaint path does ZERO linear scans over items
;;           — enforced in BOTH compiled and interpreted gate modes.
;;   r95-20  THE FENCE'S COVERAGE: which modules it interprets, that they
;;           really are interpreted inside it, and that every function
;;           R95 put on the repaint path is inside the net.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)

(when (locate-library "org-air")
  (require 'org-air))

;;;; -------------------------------------------------------------------
;;;; Scaffolding
;;;; -------------------------------------------------------------------

(defvar org-air-r95--dir nil
  "Temp corpus directory of the running round-95 test.")

(defconst org-air-r95--outside-file "/tmp/org-air-r95-outside.org"
  "A FICTION: a path with no entry in the scan's file-meta table.
An item carrying it has neither an `updated' slot nor an mtime floor —
the R94 shape of \"a number org-air simply does not have\".")

(defun org-air-r95--path (name)
  "Return corpus file NAME's absolute path."
  (expand-file-name name org-air-r95--dir))

(defun org-air-r95--epoch (days)
  "Return the epoch integer DAYS from the frozen now (negative = past)."
  (floor (float-time (time-add org-air-test-now (days-to-time days)))))

(defun org-air-r95--date (days)
  "Return an ACTIVE Org date DAYS from the frozen now (a PLAN)."
  (format-time-string "<%Y-%m-%d %a>"
                      (time-add org-air-test-now (days-to-time days))))

(defun org-air-r95--stamp (days)
  "Return an INACTIVE Org stamp DAYS from the frozen now (a RECORD)."
  (format-time-string "[%Y-%m-%d %a 09:00]"
                      (time-add org-air-test-now (days-to-time days))))

(defun org-air-r95--timestamp (days)
  "Return an Org timestamp OBJECT DAYS from the frozen now."
  (org-timestamp-from-string (org-air-r95--date days)))

(cl-defun org-air-r95--item (&key (title "R95 probe") priority updated
                                  scheduled deadline file tags (todo "TODO")
                                  active-ts)
  "Build a cache-hydrated-shape `org-air-item' for the R95 rules.
PRIORITY is a priority LETTER; UPDATED / SCHEDULED / DEADLINE /
ACTIVE-TS are day offsets from the frozen now.  FILE defaults to
`org-air-r95--outside-file'."
  (org-air-item-create
   :title title
   :tags tags
   :file (or file org-air-r95--outside-file)
   :marker (cons (or file org-air-r95--outside-file) 1)
   :kind 'heading
   :ntype 'task
   :todo todo
   :priority (and priority (* 1000 (- org-priority-lowest priority)))
   :updated (and updated (org-air-r95--epoch updated))
   :active-ts (and active-ts
                   (float-time (time-add org-air-test-now
                                         (days-to-time active-ts))))
   :own-active-ts (and active-ts
                       (float-time (time-add org-air-test-now
                                             (days-to-time active-ts))))
   :scheduled (and scheduled (org-air-r95--timestamp scheduled))
   :deadline (and deadline (org-air-r95--timestamp deadline))))

(defmacro org-air-r95--with-corpus (specs &rest body)
  "Write SPECS into a fresh temp corpus and run BODY against it."
  (declare (indent 1) (debug t))
  `(let ((org-air-r95--dir (make-temp-file "org-air-r95-" t)))
     (unwind-protect
         (progn
           (when (fboundp 'org-air-query-teardown)
             (org-air-query-teardown)
             (clrhash org-air-query--file-meta)
             (clrhash org-air-query--visits)
             (clrhash org-air-query--denote-id-index))
           (pcase-dolist (`(,name . ,content) ,specs)
             (let ((path (expand-file-name name org-air-r95--dir))
                   (coding-system-for-write 'utf-8-unix)
                   (file-name-handler-alist nil))
               (make-directory (file-name-directory path) t)
               (write-region (or content "") nil path nil 'silent)))
           (let ((org-air-files (list org-air-r95--dir))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-r95--dir))
                 (org-air-cache-file
                  (expand-file-name ".cache/board.eld" org-air-r95--dir))
                 (org-air-view-buffer-name "*org-air-r95*")
                 (org-air-plain-heading-type 'task)
                 (create-lockfiles nil)
                 (inhibit-message t))
             ,@body))
       (when (fboundp 'org-air-query-teardown) (org-air-query-teardown))
       (let ((kill-buffer-query-functions nil))
         (when (get-buffer "*org-air-r95*") (kill-buffer "*org-air-r95*"))
         (dolist (buf (buffer-list))
           (let ((fn (buffer-file-name buf)))
             (when (and fn (string-prefix-p org-air-r95--dir fn))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r95--dir t))))

(defmacro org-air-r95--render-board (size &rest body)
  "Render the REAL board over the bound corpus at SIZE; run BODY in its buffer.
SIZE is (WIDTH . HEIGHT).  Frozen clock, anti-tautology render guards
active, board buffer killed afterwards."
  (declare (indent 1) (debug t))
  `(org-air-viewport-test--with-frozen-now
     (unwind-protect
         (org-air-viewport-test--with-render-guards
           (let ((org-air-view-width (car ,size))
                 (org-air-view-height (cdr ,size)))
             (org-air)
             (with-current-buffer org-air-view-buffer-name
               ,@body)))
       (when (get-buffer org-air-view-buffer-name)
         (let ((kill-buffer-query-functions nil))
           (kill-buffer org-air-view-buffer-name))))))

(defun org-air-r95--scanned (title &optional items)
  "Return the scanned item whose title contains TITLE."
  (let ((item (org-air-test-find-item title (or items (org-air-query-items)))))
    (should item)
    item))

(defun org-air-r95--buckets (title &optional items)
  "Return TITLE's bucket list at the frozen now."
  (org-air-classify-item (org-air-r95--scanned title items) org-air-test-now))

(defun org-air-r95--members (bucket &optional items)
  "Return the sorted TITLES of BUCKET's members among ITEMS."
  (sort (delq nil
              (mapcar (lambda (item)
                        (and (memq bucket (org-air-classify-item
                                           item org-air-test-now))
                             (org-air-item-title item)))
                      (or items (org-air-query-items))))
        #'string<))

(defun org-air-r95--token-members (token &optional items)
  "Return the sorted TITLES that answer the `is:' TOKEN string."
  (let ((org-air-view--tag-filter (list token))
        (org-air-filter-match 'all)
        (org-air-view--filter-now org-air-test-now)
        (org-air-view--scope nil)
        (org-air-view--render-partition nil))
    (sort (delq nil
                (mapcar (lambda (item)
                          (and (org-air-view--passes-filter-p item)
                               (org-air-item-title item)))
                        (or items (org-air-query-items))))
          #'string<)))

(defun org-air-r95--rendered-sections ()
  "Return an alist (BUCKET . ROWS) for the board in the current buffer.
ROWS are in RENDER order; each is (TITLE CELL ITEM), where CELL is the
row's own painted span.  Property-driven, so chrome can never be
mistaken for a row."
  (let ((pos (point-min)) (bucket nil) (out ()))
    (while (< pos (point-max))
      (let ((sec (get-text-property pos 'org-air-section))
            (item (get-text-property pos 'org-air-item)))
        (cond
         (sec
          (setq bucket sec)
          (unless (assq bucket out) (push (cons bucket nil) out))
          (setq pos (or (next-single-property-change pos 'org-air-section)
                        (point-max))))
         (item
          (let* ((end (or (next-single-property-change pos 'org-air-item)
                          (point-max)))
                 (span (buffer-substring-no-properties pos end)))
            (push (list (org-air-item-title item) span item)
                  (cdr (assq bucket out)))
            (setq pos end)))
         (t (setq pos (1+ pos))))))
    (mapcar (lambda (entry) (cons (car entry) (nreverse (cdr entry))))
            (nreverse out))))

(defun org-air-r95--rows (bucket)
  "Return the rendered rows of BUCKET in the current board buffer."
  (cdr (assq bucket (org-air-r95--rendered-sections))))

(defun org-air-r95--row-titles (bucket)
  "Return the rendered row TITLES of BUCKET in the current board buffer."
  (mapcar #'car (org-air-r95--rows bucket)))

(defun org-air-r95--cell-number (span)
  "Return the INTEGER a rendered row SPAN prints, or nil.
`OVERDUE 40d' => 40; `~210d quiet' => 210; `273d quiet' => 273."
  (cond ((string-match "OVERDUE \\([0-9]+\\)d" span)
         (string-to-number (match-string 1 span)))
        ((string-match "~?\\([0-9]+\\)d quiet" span)
         (string-to-number (match-string 1 span)))))

(defun org-air-r95--badge (bucket)
  "Return BUCKET's rendered section badge count, or nil."
  (let ((pos (point-min)) (found nil))
    (while (and (not found)
                (setq pos (text-property-not-all pos (point-max)
                                                 'org-air-count-badge nil)))
      (if (eq (get-text-property pos 'org-air-section) bucket)
          (setq found (get-text-property pos 'org-air-count-badge))
        (setq pos (or (next-single-property-change pos 'org-air-count-badge)
                      (point-max)))))
    found))

(defun org-air-r95--fold-count (bucket)
  "Return the N of BUCKET's rendered \"…and N more\" row, or nil."
  (let ((pos (point-min)) (found nil))
    (while (and (not found)
                (setq pos (text-property-not-all pos (point-max)
                                                 'org-air-more-row nil)))
      (let ((end (or (next-single-property-change pos 'org-air-more-row)
                     (point-max))))
        (when (eq (get-text-property pos 'org-air-more-row) bucket)
          (let ((span (buffer-substring-no-properties pos end)))
            (when (string-match "\\([0-9]+\\) more" span)
              (setq found (string-to-number (match-string 1 span))))))
        (setq pos end)))
    found))

(defun org-air-r95--more-row-position (bucket)
  "Return the buffer position of BUCKET's \"…and N more\" fold row, or nil."
  (let ((pos (point-min)) (found nil))
    (while (and (not found)
                (setq pos (text-property-not-all pos (point-max)
                                                 'org-air-more-row nil)))
      (if (eq (get-text-property pos 'org-air-more-row) bucket)
          (setq found pos)
        (setq pos (or (next-single-property-change pos 'org-air-more-row)
                      (point-max)))))
    found))

;;;; -------------------------------------------------------------------
;;;; r95-1 — FU1, the predicate
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r95-1-planned-p-is-what-the-date-sections-read ()
  "The Untracked plan clause is `--planned-p', never the broader `--dated-p'.

THE DEFECT R95 CLOSED.  `org-air-classify--untracked-p' negated
`org-air-classify--dated-p' — scheduled ∨ deadline ∨ an active `<ts>'
ANYWHERE in the subtree — while `--overdue-p' and `--due-within-p' read
only the SCHEDULED and DEADLINE slots.  A heading in the gap was excused
from the catch-all for being \"planned\" by a notion no date section
consumed, so it had NO ROW ANYWHERE.

The catch-all for a family of sections must be the exact negation of what
those sections read.  Four properties, each of which fails if the clause
is reverted to `--dated-p':

  1. `--planned-p' is exactly the two PLAN SLOTS: it answers for a
     SCHEDULED, for a DEADLINE, for both — and NOT for a body `<ts>'.
  2. `--planned-p' is exactly what the date sections read: over every
     shape, `(or overdue upcoming)' implies it, and a heading it refuses
     can never be in either section.
  3. the body-`<ts>' heading is DATED and NOT PLANNED and IS untracked —
     the discriminating triple, the one cell R94 got wrong.
  4. `--dated-p' itself is UNTOUCHED (R54-1 still owns `is:nodate')."
  (skip-unless (locate-library "org-air"))
  (let* ((org-air-upcoming-days 7)
         (bare      (org-air-r95--item :title "Bare"))
         (sched     (org-air-r95--item :title "Sched" :scheduled 3))
         (dl        (org-air-r95--item :title "Deadline" :deadline 3))
         (both      (org-air-r95--item :title "Both" :scheduled -4 :deadline 30))
         (far       (org-air-r95--item :title "Far" :scheduled 90))
         (body-past (org-air-r95--item :title "Body past" :active-ts -5))
         (body-soon (org-air-r95--item :title "Body soon" :active-ts 1))
         (body-far  (org-air-r95--item :title "Body far" :active-ts 92))
         (logged    (org-air-r95--item :title "Logged" :updated -60)))
    ;; 1. `--planned-p' IS the two plan slots.
    (dolist (planned (list sched dl both far))
      (ert-info ((format "planned: %s" (org-air-item-title planned)))
        (should (org-air-classify--planned-p planned))))
    (dolist (unplanned (list bare body-past body-soon body-far logged))
      (ert-info ((format "unplanned: %s" (org-air-item-title unplanned)))
        (should-not (org-air-classify--planned-p unplanned))))
    ;; 2. it is exactly what the DATE SECTIONS read: no heading can be
    ;;    overdue or upcoming without it, and none it admits is excluded
    ;;    from both by anything other than distance.
    (dolist (item (list bare sched dl both far body-past body-soon body-far
                        logged))
      (let ((datedp (or (org-air-classify--overdue-p item org-air-test-now)
                        (org-air-classify--due-within-p
                         item org-air-test-now org-air-upcoming-days))))
        (ert-info ((format "date sections: %s" (org-air-item-title item)))
          (when datedp (should (org-air-classify--planned-p item)))
          (unless (org-air-classify--planned-p item) (should-not datedp)))))
    ;; 3. THE DISCRIMINATING TRIPLE — dated, not planned, untracked.
    (dolist (body (list body-past body-soon body-far))
      (ert-info ((format "body ts: %s" (org-air-item-title body)))
        (should (org-air-classify--dated-p body))
        (should-not (org-air-classify--planned-p body))
        (should (org-air-classify--untracked-p body))
        (should (memq 'untracked
                      (org-air-classify-item body org-air-test-now)))))
    ;; ...and the conjunction is still a conjunction: a plan or a record
    ;; still takes a heading out.
    (dolist (out (list sched dl both far logged))
      (should-not (org-air-classify--untracked-p out)))
    (should (org-air-classify--untracked-p bare))
    ;; 4. `--dated-p' is UNTOUCHED: it still counts the body stamp and
    ;;    still refuses the bare heading (R54-1 owns `is:nodate').
    (should (org-air-classify--dated-p sched))
    (should (org-air-classify--dated-p dl))
    (should (org-air-classify--dated-p body-past))
    (should-not (org-air-classify--dated-p bare))
    (should-not (org-air-classify--dated-p logged))))

;;;; -------------------------------------------------------------------
;;;; r95-2 — FU1 through the real renderer
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r95-2-body-timestamp-heading-has-a-row ()
  "A `TODO' whose only date is a body `<ts>' now has a row — really rendered.

This is the R94 review's §3 finding, reproduced as a permanent fence and
asserted where the user would see it: on the PAINTED board, not only in
the predicate.  Three plan distances (yesterday / tomorrow / three months
out — the review measured all three invisible) at TWO file ages, because
R93's cold-file floor used to cover this shape accidentally and R94's
removal of the floor is what exposed it.

Legs:

  * every one of the three headings is a row in the Untracked section, at
    a HOT file and at a 210-day-cold one, with identical membership;
  * the controls still behave: a real `SCHEDULED' inside the horizon is
    Upcoming and NOT untracked; one 240-day-old body `<ts>' PLUS a record
    is neither (it was worked on, and it is not yet at its threshold);
  * the board prints all three titles, so this is a row a user can see
    rather than a predicate that returns t.

Reverting the `--planned-p' clause reddens every leg."
  (skip-unless (locate-library "org-air"))
  (dolist (age '(0 210))
    (org-air-r95--with-corpus
        (list
         (cons "tasks.org"
               (concat
                "* TODO Offsite follow-ups\n"
                "We agreed on this at the offsite " (org-air-r95--date -166)
                " and never wrote it up.\n"
                "* TODO Body ts tomorrow\nDrinks " (org-air-r95--date 1) ".\n"
                "* TODO Body ts three months out\nConference "
                (org-air-r95--date 92) ".\n"
                "* TODO Real plan inside the horizon\nSCHEDULED: "
                (org-air-r95--date 3) "\n"
                "* TODO Body ts and a record\nSeen " (org-air-r95--date -240)
                "\n" (org-air-r95--stamp -2) "\n"))
         (cons "inbox.org" "#+title: inbox\n"))
      (org-air-test-age-file (org-air-r95--path "tasks.org") age)
      (org-air-test-age-file (org-air-r95--path "inbox.org") age)
      (let ((items (org-air-query-items)))
        (ert-info ((format "file age %dd" age))
          (dolist (title '("Offsite follow-ups" "Body ts tomorrow"
                           "Body ts three months out"))
            (let ((item (org-air-r95--scanned title items)))
              (ert-info ((format "shape: %s" title))
                (should (org-air-classify--dated-p item))
                (should-not (org-air-classify--planned-p item))
                (should-not (org-air-classify-updated item))
                (should (memq 'untracked
                              (org-air-classify-item item org-air-test-now))))))
          ;; controls
          (should (equal '(upcoming)
                         (org-air-r95--buckets "Real plan inside the horizon"
                                               items)))
          (let ((worked (org-air-r95--scanned "Body ts and a record" items)))
            (should (org-air-classify-updated worked))
            (should-not (org-air-classify--untracked-p worked))
            (should-not (org-air-classify-item worked org-air-test-now)))
          ;; the membership is identical at both file ages
          (should (equal '("Body ts three months out" "Body ts tomorrow"
                           "Offsite follow-ups")
                         (org-air-r95--members 'untracked items)))))
      ;; ...and it is a ROW, on the real board.
      (org-air-r95--render-board '(120 . 46)
        (let ((titles (org-air-r95--row-titles 'untracked)))
          (ert-info ((format "untracked rows at age %dd: %S" age titles))
            (dolist (title '("Offsite follow-ups" "Body ts tomorrow"
                             "Body ts three months out"))
              (should (member title titles)))))))))

;;;; -------------------------------------------------------------------
;;;; r95-3 — FU1: `is:nodate' and `is:untracked' are INDEPENDENT
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r95-3-nodate-and-untracked-are-independent ()
  "`is:nodate' and `is:untracked' now answer different questions, both ways.

R94's `is:untracked' was a STRICT SUBSET of `is:nodate'.  R95 makes the
two genuinely INDEPENDENT, and that is the one user-visible consequence
of FU1 worth pinning:

  | shape                                | is:nodate | is:untracked |
  |--------------------------------------+-----------+--------------|
  | bare TODO                            | yes       | yes          |
  | undated + a LOGBOOK trail            | yes       | NO           |
  | only date is a body `<ts>', no trail | NO        | YES          |
  | SCHEDULED far future, no trail       | no        | no           |

Three legs:

  1. the four cells above, measured on a real scanned corpus, so BOTH
     separating rows exist and neither set contains the other;
  2. the R72 law survives the change for BOTH tokens — per item, the
     token's answer equals the classifier's, which is the only form of
     that law worth having;
  3. `is:untracked' still parses and is still taught.

Reverting `--untracked-p' to `--dated-p' collapses the third row and
reddens legs 1 and 2."
  (skip-unless (locate-library "org-air"))
  (org-air-r95--with-corpus
      (list
       (cons "tasks.org"
             (concat
              "* TODO Bare and homeless\nNo plan, no record.\n"
              "* TODO Undated but logged\n" (org-air-r95--stamp -1) "\n"
              "* TODO Only a body stamp\nSeen " (org-air-r95--date -3) ".\n"
              "* TODO Planned far out\nSCHEDULED: " (org-air-r95--date 90)
              "\n"))
       (cons "inbox.org" "#+title: inbox\n"))
    (let* ((items (org-air-query-items))
           (nodate (org-air-r95--token-members "is:nodate" items))
           (untracked (org-air-r95--token-members "is:untracked" items)))
      ;; 1. the table, both separating rows present.
      (should (equal '("Bare and homeless" "Undated but logged") nodate))
      (should (equal '("Bare and homeless" "Only a body stamp") untracked))
      (should (member "Undated but logged" nodate))
      (should-not (member "Undated but logged" untracked))
      (should (member "Only a body stamp" untracked))
      (should-not (member "Only a body stamp" nodate))
      ;; NEITHER contains the other — independence, stated as such.
      (should-not (cl-subsetp untracked nodate :test #'equal))
      (should-not (cl-subsetp nodate untracked :test #'equal))
      ;; ...and they do overlap, so "independent" is not "disjoint".
      (should (member "Bare and homeless" nodate))
      (should (member "Bare and homeless" untracked))
      ;; 2. the R72 law, per item, for BOTH tokens.
      (dolist (spec (list (cons "is:untracked" #'org-air-classify--untracked-p)
                          (cons "is:nodate"
                                (lambda (item)
                                  (not (org-air-classify--dated-p item))))))
        (let ((org-air-view--tag-filter (list (car spec)))
              (org-air-filter-match 'all)
              (org-air-view--filter-now org-air-test-now)
              (org-air-view--scope nil)
              (org-air-view--render-partition nil))
          (dolist (item items)
            (ert-info ((format "%s vs %s" (car spec)
                               (org-air-item-title item)))
              (should (eq (and (org-air-view--passes-filter-p item) t)
                          (and (funcall (cdr spec) item) t)))))))
      ;; ...and the section body is the same set the token selects.
      (should (equal untracked (org-air-r95--members 'untracked items))))
    ;; 3. still taught.
    (should (equal '(is . untracked)
                   (org-air-view--filter-token-parse "is:untracked")))
    (should (member "untracked" org-air-view--filter-is-values))
    (should (member "is:nodate" (org-air-view--filter-vocabulary)))))

;;;; -------------------------------------------------------------------
;;;; FU2 scaffolding — the diversity corpus the R94 review measured
;;;; -------------------------------------------------------------------

(defconst org-air-r95--diversity-corpus
  '(("someday.org" . 6) ("personal.org" . 1) ("work.org" . 5))
  "(FILE . BARE-TODO-COUNT) for the FU2 corpus, coldest file first.
The shape the R94 review measured: one cold someday list with enough
rows to fill the cap on its own, one middling file, and the file the user
edits every day holding the bare admin `TODO's the section exists for.")

(defconst org-air-r95--diversity-ages
  '(("someday.org" . 210) ("personal.org" . 40) ("work.org" . 0))
  "Per-file mtime age in days for `org-air-r95--diversity-corpus'.")

(defun org-air-r95--diversity-specs ()
  "Return the `org-air-r95--with-corpus' SPECS for the FU2 corpus."
  (append
   (mapcar (lambda (cell)
             (let ((file (car cell)) (n (cdr cell)) (body ""))
               (dotimes (i n)
                 (setq body (concat body
                                    (format "* TODO %s %d\nNo plan, no record.\n"
                                            (capitalize
                                             (file-name-base file))
                                            (1+ i)))))
               (cons file body)))
           org-air-r95--diversity-corpus)
   (list (cons "inbox.org" "#+title: inbox\n"))))

(defmacro org-air-r95--with-diversity-corpus (&rest body)
  "Run BODY over the FU2 diversity corpus with per-file mtimes applied."
  (declare (indent 0) (debug t))
  `(org-air-r95--with-corpus (org-air-r95--diversity-specs)
     (pcase-dolist (`(,file . ,age) org-air-r95--diversity-ages)
       (org-air-test-age-file (org-air-r95--path file) age))
     (org-air-test-age-file (org-air-r95--path "inbox.org") 0)
     ,@body))

(defun org-air-r95--file-of (item)
  "Return ITEM's file NAME (no directory), for the per-file quota checks."
  (file-name-nondirectory (or (org-air-item-file item) "")))

(defun org-air-r95--subsequence-p (window sorted)
  "Non-nil when WINDOW is a SUBSEQUENCE of SORTED (`eq' identity)."
  (let ((rest sorted) (ok t))
    (dolist (item window ok)
      (let ((tail (memq item rest)))
        (if tail (setq rest (cdr tail)) (setq ok nil))))))

;;;; -------------------------------------------------------------------
;;;; r95-4 — FU2: the per-file diversity quota
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r95-4-untracked-collapsed-window-caps-one-file-at-two ()
  "No single file may claim more than two rows of the collapsed Untracked window.

THE FINDING THIS FENCES.  `--sort-by-floor' ranks Untracked by
`org-air-classify-quiet-floor-days' — a fact about the heading's FILE,
IDENTICAL for every heading in it — so the capped section was won by
whichever file was coldest.  The R94 review measured 4 of 4 visible rows
from one `someday.org' while the bare admin `TODO's in the file the user
edits daily sat at ranks 16-20 of 21, behind the fold.  That is the R93
Needs-attention pathology rebuilt one section lower, inside a section
whose whole claim is that org-air CANNOT rank this work.

Four legs:

  1. UNIT.  Over a synthetic worst-first list where one file could fill
     the cap alone, `org-air-view--collapsed-window' takes at most
     `org-air-view--untracked-per-file-max' from any one file;
  2. and the quota is UNTRACKED-ONLY: every other bucket still gets a
     plain `seq-take' of its own cap, one-file corpus or not;
  3. END TO END.  On the REAL board over the review's corpus shape, the
     four painted Untracked rows come from THREE files, and the coldest
     file contributes exactly two;
  4. ANTI-VACUITY.  The corpus really could have been monopolised: the
     coldest file holds more members than the whole cap.

Raising the quota to the cap (or to any value >= 4) reddens legs 1 and 3."
  (skip-unless (locate-library "org-air"))
  (let* ((limit (org-air-view--section-limit 'untracked))
         (cold (mapcar (lambda (i)
                         (org-air-r95--item :title (format "Cold %d" i)
                                            :file "/tmp/r95/someday.org"))
                       (number-sequence 1 6)))
         (mid (list (org-air-r95--item :title "Shed door"
                                       :file "/tmp/r95/personal.org")))
         (hot (mapcar (lambda (i)
                        (org-air-r95--item :title (format "Admin %d" i)
                                           :file "/tmp/r95/work.org"))
                      (number-sequence 1 5)))
         (sorted (append cold mid hot))
         (window (org-air-view--collapsed-window 'untracked sorted)))
    ;; 4. anti-vacuity: one file COULD have taken the whole cap.
    (should (> (length cold) limit))
    ;; 1. the quota
    (should (= limit (length window)))
    (let ((per-file (make-hash-table :test #'equal)))
      (dolist (item window)
        (puthash (org-air-r95--file-of item)
                 (1+ (gethash (org-air-r95--file-of item) per-file 0))
                 per-file))
      (maphash (lambda (file n)
                 (ert-info ((format "%s -> %d rows" file n))
                   (should (<= n org-air-view--untracked-per-file-max))))
               per-file)
      (should (>= (hash-table-count per-file) 3))
      (should (= org-air-view--untracked-per-file-max
                 (gethash "someday.org" per-file))))
    ;; 2. UNTRACKED ONLY — every other bucket keeps the plain cap.
    (dolist (bucket '(overdue upcoming attention high-priority inbox))
      (let ((n (org-air-view--section-limit bucket)))
        (ert-info ((format "bucket %S" bucket))
          (should (equal (seq-take sorted n)
                         (org-air-view--collapsed-window bucket sorted)))))))
  ;; 3. END TO END, on the real renderer.
  (org-air-r95--with-diversity-corpus
    (org-air-r95--render-board '(120 . 46)
      (let* ((rows (org-air-r95--rows 'untracked))
             (files (mapcar (lambda (row) (org-air-r95--file-of (nth 2 row)))
                            rows)))
        (ert-info ((format "visible untracked rows: %S" files))
          (should (= 4 (length rows)))
          (should (>= (length (delete-dups (copy-sequence files))) 3))
          (should (= 2 (seq-count (lambda (f) (equal f "someday.org")) files)))
          (should (member "personal.org" files))
          (should (member "work.org" files)))))))

;;;; -------------------------------------------------------------------
;;;; r95-5 — FU2: the window is a SUBSEQUENCE (the FIX-2 order law)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r95-5-collapsed-window-is-a-subsequence-of-the-order ()
  "The quota decides WHICH rows, never in WHAT ORDER — FIX-2 survives it.

FIX-2's law is that a capped section shows its rows worst-first BY THE
VERY NUMBER THOSE ROWS PRINT.  The quota is a SELECTION rule, and a
selection rule that returned its picks in pick-order would silently break
that law — the user would read `~40d' above `~210d' and the section would
stop meaning what it says.

So the window is returned as a SUBSEQUENCE of the sorted list, and this
test states exactly that, three ways:

  1. STRUCTURALLY: every window is a subsequence of the order it was
     computed from — checked over several corpus shapes including ones
     where the slack fill must reach backwards;
  2. NUMERICALLY: the numbers the PAINTED cells print are monotone
     non-increasing on the real board, `no history' last;
  3. and the expanded (TAB) list is the FULL worst-first order, so the
     collapsed window is a window ON it rather than a different list.

Returning the quota's picks unsorted reddens legs 1 and 2."
  (skip-unless (locate-library "org-air"))
  ;; 1. STRUCTURAL, over several shapes.
  (dolist (shape '((("a.org" . 6))                    ; one file only
                   (("a.org" . 6) ("b.org" . 1))      ; slack reaches back
                   (("a.org" . 2) ("b.org" . 2) ("c.org" . 2))
                   (("a.org" . 1) ("b.org" . 1))))    ; fewer than the cap
    (let* ((i 0)
           (sorted (apply #'append
                          (mapcar (lambda (cell)
                                    (mapcar (lambda (_)
                                              (setq i (1+ i))
                                              (org-air-r95--item
                                               :title (format "Row %d" i)
                                               :file (concat "/tmp/r95/"
                                                             (car cell))))
                                            (number-sequence 1 (cdr cell))))
                                  shape)))
           (window (org-air-view--collapsed-window 'untracked sorted)))
      (ert-info ((format "shape %S -> %S" shape
                         (mapcar #'org-air-item-title window)))
        (should (org-air-r95--subsequence-p window sorted))
        (should (<= (length window)
                    (min (length sorted)
                         (org-air-view--section-limit 'untracked)))))))
  ;; 2. NUMERICAL, on the painted board.
  (org-air-r95--with-diversity-corpus
    (org-air-r95--render-board '(120 . 46)
      (let* ((rows (org-air-r95--rows 'untracked))
             (numbers (mapcar (lambda (row)
                                (org-air-r95--cell-number (nth 1 row)))
                              rows)))
        (ert-info ((format "painted untracked cells: %S" numbers))
          ;; every KNOWN number is >= the next known one, and every
          ;; `no history' row comes after every numbered one.
          (let ((seen-unknown nil) (previous nil))
            (dolist (n numbers)
              (if (null n)
                  (setq seen-unknown t)
                (should-not seen-unknown)
                (when previous (should (<= n previous)))
                (setq previous n))))
          (should (equal '(210 210 40) (delq nil numbers))))))
    ;; 3. TAB shows the FULL worst-first order, and the window is a
    ;;    subsequence OF IT.
    (org-air-r95--render-board '(120 . 46)
      (let* ((collapsed (mapcar (lambda (r) (nth 2 r))
                                (org-air-r95--rows 'untracked)))
             (pos (org-air-r95--more-row-position 'untracked)))
        (should pos)
        (goto-char pos)
        (org-air-toggle-section)
        (let ((expanded (mapcar (lambda (r) (nth 2 r))
                                (org-air-r95--rows 'untracked))))
          (should (= 12 (length expanded)))
          (should (org-air-r95--subsequence-p collapsed expanded)))))))

;;;; -------------------------------------------------------------------
;;;; r95-6 — FU2: the slack fill
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r95-6-collapsed-window-fills-slack-from-skipped-rows ()
  "A quota may never make the section SHORTER than it was without one.

The diversity rule has an obvious failure mode: with nothing to diversify
WITH — one file, which is a single-file user's NORMAL state — a naive
quota shows two rows where the cap allows four, and the section silently
shrinks for the users who need it most.  R95 fills the slack from the
rows the quota just skipped, still worst-first.

Four legs:

  1. a SINGLE-FILE list still fills the cap exactly;
  2. the filled window is STILL a subsequence (the slack fill must not
     append the skipped rows at the end);
  3. the window is NEVER shorter than the plain `seq-take' it replaced —
     swept over every corpus size from 0 to 3x the cap, one file and
     several;
  4. END TO END: a one-file board really paints four Untracked rows.

Dropping the slack fill reddens legs 1, 3 and 4."
  (skip-unless (locate-library "org-air"))
  (let ((limit (org-air-view--section-limit 'untracked)))
    ;; 1 + 2: one file, cap filled, order preserved.
    (let* ((sorted (mapcar (lambda (i)
                             (org-air-r95--item :title (format "Only %d" i)
                                                :file "/tmp/r95/only.org"))
                           (number-sequence 1 (* 2 limit))))
           (window (org-air-view--collapsed-window 'untracked sorted)))
      (should (= limit (length window)))
      (should (equal (seq-take sorted limit) window))
      (should (org-air-r95--subsequence-p window sorted)))
    ;; 3: never shorter than `seq-take', at every size and shape.
    (dolist (files '(("a.org") ("a.org" "b.org") ("a.org" "b.org" "c.org")))
      (dotimes (n (1+ (* 3 limit)))
        (let* ((sorted (mapcar (lambda (i)
                                 (org-air-r95--item
                                  :title (format "Row %d" i)
                                  :file (concat "/tmp/r95/"
                                                (nth (mod i (length files))
                                                     files))))
                               (number-sequence 1 n)))
               (window (org-air-view--collapsed-window 'untracked sorted)))
          (ert-info ((format "files=%S n=%d" files n))
            (should (= (length (seq-take sorted limit)) (length window)))
            (should (org-air-r95--subsequence-p window sorted)))))))
  ;; 4: END TO END on a genuinely single-file board.
  (org-air-r95--with-corpus
      (list (cons "only.org"
                  (mapconcat (lambda (i)
                               (format "* TODO Only %d\nNo plan, no record.\n" i))
                             (number-sequence 1 9) ""))
            (cons "inbox.org" "#+title: inbox\n"))
    (org-air-test-age-file (org-air-r95--path "only.org") 210)
    (org-air-r95--render-board '(120 . 46)
      (let ((rows (org-air-r95--rows 'untracked)))
        (should (= 4 (length rows)))
        (should (= 5 (org-air-r95--fold-count 'untracked)))
        (should (= 9 (org-air-r95--badge 'untracked)))))))

;;;; -------------------------------------------------------------------
;;;; r95-7 — FU2: everything that COUNTS counts MEMBERS
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r95-7-untracked-counts-are-members-not-visible-rows ()
  "The quota is a DISPLAY rule: nothing that counts may notice it.

A selection rule that leaked into the arithmetic would be a lie of a
worse kind than the one it fixed — the badge would under-report the work
and `…and N more' would not add up.  So on one board, over the review's
corpus shape:

  * the section BADGE is the member count (12), not the visible 4;
  * the fold row's N is members minus VISIBLE rows, so
    visible + N = badge exactly;
  * `is:untracked' selects all 12 — the token is bucket membership and
    knows nothing about windows;
  * and the classifier agrees with the token, per item (the R72 law)."
  (skip-unless (locate-library "org-air"))
  (org-air-r95--with-diversity-corpus
    (let ((members (org-air-r95--members 'untracked)))
      (should (= 12 (length members)))
      (should (equal members (org-air-r95--token-members "is:untracked")))
      (org-air-r95--render-board '(120 . 46)
        (let ((rows (org-air-r95--rows 'untracked)))
          (should (= 4 (length rows)))
          (should (= 12 (org-air-r95--badge 'untracked)))
          (should (= 8 (org-air-r95--fold-count 'untracked)))
          (should (= (org-air-r95--badge 'untracked)
                     (+ (length rows) (org-air-r95--fold-count 'untracked)))))))))

;;;; -------------------------------------------------------------------
;;;; r95-8 — FU2: TAB lands on a row that was actually hidden
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r95-8-tab-lands-on-the-first-row-the-quota-hid ()
  "TAB on the fold row reveals, and lands on, the first row the QUOTA hid.

The fold row teaches TAB, so TAB there must act rather than drift (R51-3),
and it lands on the first HIDDEN member so the reveal is visible.  Before
R95 `first hidden' was `(nth CAP SORTED)' — an INDEX into the order.  With
a per-file quota the visible set is no longer a prefix of that order, so
the index names a row that was on screen all along and the landing lies.

The corpus is chosen so the two answers genuinely DIFFER and the old one
is the embarrassing kind of wrong: three rows in the coldest file, then
one row each in two warmer ones.  The quota skips the third cold row, so
the visible window is ranks 0, 1, 3, 4 — and rank 4, the row the CAP
INDEX names, IS ON SCREEN.

Four legs:

  1. the pre-R95 answer is demonstrably wrong here: the cap-index row is
     one of the VISIBLE rows;
  2. point lands on an item that was NOT in the collapsed window;
  3. it lands on the FIRST such item in the sorted order (rank 2);
  4. the section really did expand (all 8 rows painted).

Restoring the cap-index landing reddens legs 2 and 3."
  (skip-unless (locate-library "org-air"))
  (org-air-r95--with-corpus
      (list
       (cons "alpha.org"
             (mapconcat (lambda (i) (format "* TODO Alpha %d\nNothing.\n" i))
                        (number-sequence 1 3) ""))
       (cons "bravo.org" "* TODO Bravo 1\nNothing.\n")
       (cons "charlie.org" "* TODO Charlie 1\nNothing.\n")
       (cons "delta.org"
             (mapconcat (lambda (i) (format "* TODO Delta %d\nNothing.\n" i))
                        (number-sequence 1 3) ""))
       (cons "inbox.org" "#+title: inbox\n"))
    (pcase-dolist (`(,file . ,age) '(("alpha.org" . 210) ("bravo.org" . 150)
                                     ("charlie.org" . 90) ("delta.org" . 30)
                                     ("inbox.org" . 0)))
      (org-air-test-age-file (org-air-r95--path file) age))
    (org-air-r95--render-board '(120 . 46)
      (let* ((collapsed (mapcar (lambda (r) (nth 2 r))
                                (org-air-r95--rows 'untracked)))
             (sorted (org-air-view--sort-items
                      (org-air-view--items-for-bucket
                       'untracked org-air-view--items)
                      'untracked))
             (expected (seq-find (lambda (it) (not (memq it collapsed)))
                                 sorted))
             (cap-index-row (nth (org-air-view--section-limit 'untracked)
                                 sorted))
             (pos (org-air-r95--more-row-position 'untracked)))
        (should (= 8 (length sorted)))
        (should pos)
        ;; 1. the pre-R95 landing really would have been WRONG here: it
        ;;    names a row that never left the screen.
        (should (memq cap-index-row collapsed))
        (should-not (eq expected cap-index-row))
        ;; 3. the first genuinely hidden row is rank 2.
        (should (eq expected (nth 2 sorted)))
        (should (equal "Alpha 3" (org-air-item-title expected)))
        (goto-char pos)
        (org-air-toggle-section)
        (let ((landed (get-text-property (point) 'org-air-item)))
          (ert-info ((format "landed on %S, expected %S"
                             (and landed (org-air-item-title landed))
                             (org-air-item-title expected)))
            (should landed)
            ;; 2. + 3.
            (should (eq landed expected))
            (should-not (memq landed collapsed))))
        ;; 4.
        (should (= 8 (length (org-air-r95--rows 'untracked))))))))

;;;; -------------------------------------------------------------------
;;;; r95-9 — FU3: the anchored planning keyword
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r95-9-planning-keyword-is-anchored ()
  "A property whose NAME ends in a planning keyword keeps its own stamp.

R94 excluded `SCHEDULED:'/`DEADLINE:' VALUES from the measured clock —
correctly: a plan is not an update.  The keyword regexp was UNANCHORED,
so it also matched the TAIL of a longer word and any property whose name
merely ENDS in a planning keyword swallowed its own stamp:

  :LAST_DEADLINE:  […]   written by hand-rolled repeaters and exporters
  :ORIG_SCHEDULED: […]   written by `org-depend' and friends

Both are RECORDS of something that happened.  Read as plans, they wiped
the heading's clock and pushed a heading with a perfectly good history
into Untracked (found by the R94 review).

Four legs:

  1. UNIT, on the consumer: `org-air-query--plan-stamp-p' says NO for the
     two property spellings and YES for the two real planning lines, in a
     real buffer, at the real stamp position;
  2. the mixed planning line still resolves KEYWORD BY KEYWORD — the
     space before `DEADLINE:' satisfies the anchor, so `CLOSED:' keeps
     its stamp and `DEADLINE:' still loses its own;
  3. END TO END on a scanned corpus: the two property headings have a
     MEASURED clock of 14 days and are NOT untracked, while the two real
     plan lines still have none and still reach their date section;
  4. the shy group keeps the keyword in group 1, which is what leg 1
     depends on.

Unanchoring the regexp reddens legs 1 and 3."
  (skip-unless (locate-library "org-air"))
  ;; 1 + 2 + 4: the regexp and its consumer, in a buffer.
  (let ((lines '((":LAST_DEADLINE:  [2026-06-01 Mon 08:00]" . nil)
                 (":ORIG_SCHEDULED: [2026-06-01 Mon 08:00]" . nil)
                 (":LAST_REPEAT:    [2026-06-01 Mon 08:00]" . nil)
                 ("DEADLINE: [2026-06-01 Mon]" . t)
                 ("SCHEDULED: [2026-06-01 Mon]" . t)
                 ("  DEADLINE: [2026-06-01 Mon]" . t)
                 ("CLOSED: [2026-06-01 Mon 08:00]" . nil)
                 ("- Note taken on [2026-06-01 Mon 08:00]" . nil))))
    (pcase-dolist (`(,line . ,planp) lines)
      (with-temp-buffer
        (insert line "\n")
        (goto-char (point-min))
        (should (re-search-forward org-ts-regexp-inactive nil t))
        (ert-info ((format "%S -> plan-stamp-p %S" line planp))
          (should (eq (and (org-air-query--plan-stamp-p (match-beginning 0)) t)
                      (and planp t)))))))
  ;; 2 (continued): one MIXED planning line, both stamps, keyword by keyword.
  (with-temp-buffer
    (insert "CLOSED: [2026-06-01 Mon 08:00] DEADLINE: [2026-06-14 Sun]\n")
    (goto-char (point-min))
    (should (re-search-forward org-ts-regexp-inactive nil t))
    (should-not (org-air-query--plan-stamp-p (match-beginning 0)))
    (should (re-search-forward org-ts-regexp-inactive nil t))
    (should (org-air-query--plan-stamp-p (match-beginning 0))))
  ;; 4: the keyword really is group 1 under the shy outer group.
  (should (string-match org-air-query--planning-keyword-regexp
                        "  DEADLINE: [2026-06-01 Mon]"))
  (should (equal "DEADLINE" (match-string 1 "  DEADLINE: [2026-06-01 Mon]")))
  (should-not (string-match org-air-query--planning-keyword-regexp
                            ":LAST_DEADLINE:  [2026-06-01 Mon 08:00]"))
  (should-not (string-match org-air-query--planning-keyword-regexp
                            ":ORIG_SCHEDULED: [2026-06-01 Mon 08:00]"))
  ;; 3: END TO END, on a real scan.
  (org-air-r95--with-corpus
      (list
       (cons "tasks.org"
             (concat
              "* TODO Repeater with a last deadline\n:PROPERTIES:\n"
              ":LAST_DEADLINE: " (org-air-r95--stamp -14) "\n:END:\n"
              "* TODO Dependency with an orig schedule\n:PROPERTIES:\n"
              ":ORIG_SCHEDULED: " (org-air-r95--stamp -14) "\n:END:\n"
              "* TODO Really has a deadline\nDEADLINE: "
              (org-air-r95--date -14) "\n"
              ;; the INACTIVE plan spelling too (R94 FIX-2): still a
              ;; plan, still not an update, and the anchor must not
              ;; change that.
              "* TODO Really is scheduled\nSCHEDULED: "
              (format-time-string
               "[%Y-%m-%d %a]"
               (time-add org-air-test-now (days-to-time -14)))
              "\n"))
       (cons "inbox.org" "#+title: inbox\n"))
    (let ((items (org-air-query-items)))
      ;; the two PROPERTY headings keep their measured clock
      (dolist (title '("Repeater with a last deadline"
                       "Dependency with an orig schedule"))
        (let ((item (org-air-r95--scanned title items)))
          (ert-info ((format "property: %s" title))
            (should (org-air-classify-updated item))
            (should (= 14 (org-air-classify-quiet-days item org-air-test-now)))
            (should-not (org-air-classify--untracked-p item))
            (should-not (memq 'untracked (org-air-classify-item
                                          item org-air-test-now))))))
      ;; the two REAL plan lines still lose theirs, and still reach Overdue
      (dolist (title '("Really has a deadline" "Really is scheduled"))
        (let ((item (org-air-r95--scanned title items)))
          (ert-info ((format "plan: %s" title))
            (should-not (org-air-classify-updated item))
            (should (memq 'overdue (org-air-classify-item
                                    item org-air-test-now)))
            (should-not (memq 'untracked (org-air-classify-item
                                          item org-air-test-now)))))))))

;;;; -------------------------------------------------------------------
;;;; r95-10 — FU4: the recency key IS the measured clock
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r95-10-item-activity-is-the-measured-clock ()
  "`--item-activity' is `org-air-classify-updated'; a FUTURE plan cannot move it.

The `recency' sort key used to read `org-air-classify--last-activity' —
closed ‖ scheduled ‖ deadline ‖ first subtree stamp ‖ FILE MTIME.  The
R94 review measured the consequence through the real renderer: a key
labelled RECENCY ranked a Needs-attention row by its SCHEDULED date in
JULY.  R94 abolished the plan/record conflation everywhere else; this was
the last place it decided an order a user reads.

Four legs:

  1. IDENTITY: for every shape, `--item-activity' is exactly
     `org-air-classify-updated' — the same function the Needs-attention
     numbers come from, so key and cell can never disagree;
  2. A FUTURE PLAN IS INVISIBLE to it: the same measured epoch with and
     without a `SCHEDULED' next year;
  3. NIL IS AN ANSWER, not a zero: an unmeasured heading returns nil,
     where the legacy chain returned SOMETHING (a plan, or the file's
     mtime) for the very same item;
  4. ANTI-VACUITY: the legacy chain really does answer differently on
     this corpus, so leg 1 is not trivially true.

Pointing it back at `--last-activity' reddens legs 1, 2 and 3."
  (skip-unless (locate-library "org-air"))
  (let* ((measured (org-air-r95--item :title "Measured" :updated -87))
         (planned  (org-air-r95--item :title "Planned" :updated -87
                                      :scheduled 200))
         (plan-only (org-air-r95--item :title "Plan only" :scheduled 200))
         (bare (org-air-r95--item :title "Bare"))
         (all (list measured planned plan-only bare)))
    ;; 1. identity
    (dolist (item all)
      (ert-info ((format "identity: %s" (org-air-item-title item)))
        (should (equal (org-air-view--item-activity item)
                       (org-air-classify-updated item)))))
    ;; 2. a future plan moves nothing
    (should (equal (org-air-view--item-activity measured)
                   (org-air-view--item-activity planned)))
    ;; 3. nil is an answer
    (should-not (org-air-view--item-activity plan-only))
    (should-not (org-air-view--item-activity bare))
    ;; 4. anti-vacuity: the LEGACY chain answers, and differently.
    (should (org-air-classify--last-activity plan-only))
    (should-not (equal (org-air-classify--last-activity planned)
                       (org-air-view--item-activity planned)))))

;;;; -------------------------------------------------------------------
;;;; r95-11 — FU4: the order, and the unmeasured-last rule
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r95-11-sort-by-recency-ranks-unmeasured-last ()
  "`--sort-by-recency': least-recent first, unmeasured LAST, `O' reverses all.

The order convention every board key follows is worst-first, and for a
recency key `worst' is `longest since anything happened'.  A heading
org-air has NEVER measured is not the oldest — it is unranked, and
org-air will not rank what it never measured (the same rule
`--sort-by-quiet' and `--sort-by-floor' already follow).  The rule is
VISIBLE rather than silent: those rows print `quiet' or `no history' in
their own date cell.

Five legs:

  1. ascending: oldest measured first, newest measured last;
  2. the unmeasured rows come AFTER every measured one;
  3. among themselves they are in TITLE-then-query order, not scan order;
  4. equal measured clocks tiebreak by title then by incoming order;
  5. DESC reverses the WHOLE result, unmeasured included — `O' has always
     meant that and still does.

Re-pointing the key at `--last-activity' reddens legs 1, 2 and 3 (a bare
heading would rank by its FILE, and a planned one by its plan)."
  (skip-unless (locate-library "org-air"))
  (let* ((lute (org-air-r95--item :title "Learn the lute" :updated -274))
         (cdn (org-air-r95--item :title "Renegotiate the CDN contract"
                                 :updated -87 :scheduled 16))
         (passport (org-air-r95--item :title "Renew the passport" :updated -40))
         (tie-b (org-air-r95--item :title "Bravo tie" :updated -12))
         (tie-a (org-air-r95--item :title "Alpha tie" :updated -12))
         (zeta (org-air-r95--item :title "Zeta never measured"))
         (alpha (org-air-r95--item :title "Alpha never measured"
                                   :scheduled 200))
         (items (list zeta cdn tie-b lute alpha passport tie-a))
         (asc (org-air-view--sort-by-recency items nil))
         (desc (org-air-view--sort-by-recency items t))
         (titles (mapcar #'org-air-item-title asc)))
    (ert-info ((format "ascending: %S" titles))
      ;; 1 + 2 + 3 + 4, all at once and in one literal.
      (should (equal '("Learn the lute"
                       "Renegotiate the CDN contract"
                       "Renew the passport"
                       "Alpha tie"
                       "Bravo tie"
                       "Alpha never measured"
                       "Zeta never measured")
                     titles)))
    ;; 2, stated on its own so a reordering inside either half still fails
    ;; the right leg.
    (let ((measured-tail (seq-drop asc 5)))
      (dolist (item measured-tail)
        (should-not (org-air-view--item-activity item)))
      (dolist (item (seq-take asc 5))
        (should (org-air-view--item-activity item))))
    ;; 5. DESC reverses the whole thing.
    (should (equal (reverse titles) (mapcar #'org-air-item-title desc)))
    ;; ...and the key really is dispatched by `--sort-items'.
    (let ((org-air-sort-key 'recency)
          (org-air-view--sort-key nil)
          (org-air-view--sort-direction nil))
      (should (equal titles
                     (mapcar #'org-air-item-title
                             (org-air-view--sort-items items 'attention)))))))

;;;; -------------------------------------------------------------------
;;;; r95-12 — FU4 end to end: `recency' agrees with the printed numbers
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r95-12-recency-sort-agrees-with-the-printed-numbers ()
  "On the real board, `recency' orders Needs attention by what its rows print.

The R94 review's §7.3: the moment the user touched the sort key, the
section stopped matching the `Nd quiet' numbers its own rows print,
because the key ranked a row by a date in the FUTURE.  This is that
finding as a rendered fence.

Four legs:

  1. the PAINTED Needs-attention cells are monotone worst-first under
     `recency' — the same property the default key has;
  2. the row whose only non-measured fact is a FUTURE `SCHEDULED' sits
     where its measured age puts it, not first and not last;
  3. ANTI-VACUITY: the legacy `--last-activity' key really would have
     ordered this corpus differently, so leg 1 is a fact about the fix;
  4. an all-unmeasured section (Untracked) degrades to title-then-query
     order rather than to the file mtime."
  (skip-unless (locate-library "org-air"))
  (org-air-r95--with-corpus
      (list
       (cons "work.org"
             (concat
              "* TODO Learn the lute\n" (org-air-r95--stamp -274) "\n"
              "* TODO Renegotiate the CDN contract\nSCHEDULED: "
              (org-air-r95--date 16) "\n" (org-air-r95--stamp -87) "\n"
              "* TODO Renew the passport\n" (org-air-r95--stamp -40) "\n"
              "* TODO Write the incident postmortem\n"
              (org-air-r95--stamp -31) "\n"
              "* TODO Bare one\nNothing.\n"
              "* TODO Another bare\nNothing.\n"))
       (cons "inbox.org" "#+title: inbox\n"))
    (org-air-test-age-file (org-air-r95--path "work.org") 210)
    (let ((org-air-sort-key 'recency))
      (org-air-r95--render-board '(120 . 46)
        (let* ((rows (org-air-r95--rows 'attention))
               (titles (mapcar #'car rows))
               (numbers (mapcar (lambda (r) (org-air-r95--cell-number (nth 1 r)))
                                rows))
               (items (mapcar (lambda (r) (nth 2 r)) rows)))
          (ert-info ((format "attention under recency: %S %S" titles numbers))
            ;; 1. worst-first by the very number the rows print
            (should (equal '(274 87 40 31) numbers))
            (should (equal '("Learn the lute" "Renegotiate the CDN contract"
                             "Renew the passport"
                             "Write the incident postmortem")
                           titles))
            ;; 2. the future-plan row is where its MEASURED age puts it
            (should (equal "Renegotiate the CDN contract" (nth 1 titles)))
            ;; 3. ANTI-VACUITY: the legacy key disagrees on this corpus
            (let ((legacy (mapcar
                           #'org-air-item-title
                           (org-air-view--sort-by
                            items #'time-less-p
                            (lambda (it)
                              (or (org-air-classify--last-activity it) '(0 0)))
                            nil))))
              (ert-info ((format "legacy order: %S" legacy))
                (should-not (equal titles legacy))))))
        ;; 4. an all-unmeasured section degrades to title-then-query order.
        (should (equal '("Another bare" "Bare one")
                       (org-air-r95--row-titles 'untracked)))))))

;;;; -------------------------------------------------------------------
;;;; r95-13 — FU6: an unprocessed capture is not Untracked
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r95-13-inbox-dwellers-leave-untracked ()
  "An inbox dweller is not Untracked — and coverage survives BY CONSTRUCTION.

An unprocessed capture has no plan and no record BY DEFINITION, so the
Untracked row said nothing the Inbox row two sections above had not
already said, while consuming a scarce cap slot (1 of the 3 rows on the
shipped demo board).  This is NOT the R93 inbox carve-out returning: that
one was about AGING and it HID work.  This one cannot hide anything, and
the test says WHY rather than taking it on trust.

Six legs:

  1. the capture is `(inbox)' and nothing else;
  2. the SAME heading text outside the inbox is `(untracked)' — so the
     exclusion is about the QUEUE, not about the heading;
  3. an `:inbox:'-TAGGED heading in an ordinary file is excluded too
     (`--inbox-dweller-p' is the one definition of a dweller);
  4. COVERAGE IS PRESERVED BY CONSTRUCTION: every heading the clause
     removes from `untracked' is pushed into `inbox' unconditionally, so
     no heading can lose its last row — asserted over every capture shape;
  5. the overlap rule still stands where the two rows differ: an
     untracked `#A' is still in BOTH High priority and Untracked;
  6. the clause lives INSIDE `--untracked-p', so `is:untracked' drops
     exactly the rows the section drops (the R72 law).

Dropping the clause reddens legs 1, 2, 3 and 6."
  (skip-unless (locate-library "org-air"))
  (org-air-r95--with-corpus
      (list
       (cons "inbox.org"
             (concat "#+title: inbox\n\n"
                     "* Call plumber\nNo plan, no record.\n"
                     "* TODO Chase the framing quote\nNo plan, no record.\n"
                     "* TODO [#A] Urgent capture\nNo plan, no record.\n"))
       (cons "tasks.org"
             (concat "* Call plumber\nNo plan, no record.\n"
                     "* TODO [#A] Top and untracked\nNo plan, no record.\n"
                     "* TODO Tagged for the queue :inbox:\nNo plan, no record.\n"))
       (cons "empty-inbox.org" ""))
    (let* ((items (org-air-query-items))
           (captures (seq-filter
                      (lambda (it)
                        (and (org-air-classify--inbox-dweller-p it)
                             (org-air-classify--board-active-p it)))
                      items)))
      ;; 1. the capture is `(inbox)' alone.
      (dolist (title '("Call plumber" "Chase the framing quote"))
        (let ((item (car (seq-filter
                          (lambda (it)
                            (and (equal title (org-air-item-title it))
                                 (org-air-classify--inbox-dweller-p it)))
                          items))))
          (ert-info ((format "capture: %s" title))
            (should item)
            (should-not (org-air-classify--planned-p item))
            (should-not (org-air-classify-updated item))
            (should-not (org-air-classify--untracked-p item))
            (should (equal '(inbox)
                           (org-air-classify-item item org-air-test-now))))))
      ;; 2. the SAME heading outside the inbox IS untracked.
      (let ((twin (car (seq-filter
                        (lambda (it)
                          (and (equal "Call plumber" (org-air-item-title it))
                               (not (org-air-classify--inbox-dweller-p it))))
                        items))))
        (should twin)
        (should (org-air-classify--untracked-p twin))
        (should (memq 'untracked
                      (org-air-classify-item twin org-air-test-now))))
      ;; 3. the `:inbox:' TAG is a dweller too.
      (let ((tagged (org-air-r95--scanned "Tagged for the queue" items)))
        (should (org-air-classify--inbox-dweller-p tagged))
        (should-not (org-air-classify--untracked-p tagged))
        (should (memq 'inbox (org-air-classify-item tagged org-air-test-now))))
      ;; 4. COVERAGE BY CONSTRUCTION — nothing the clause removed lost its
      ;;    last row.
      (should (> (length captures) 0))
      (dolist (item captures)
        (ert-info ((format "coverage: %s" (org-air-item-title item)))
          (let ((buckets (org-air-classify-item item org-air-test-now)))
            (should buckets)
            (should (memq 'inbox buckets))
            (should-not (memq 'untracked buckets)))))
      ;; 5. the OVERLAP RULE survives where the two rows differ.
      (let ((hipri (org-air-r95--scanned "Top and untracked" items)))
        (should (equal '(high-priority untracked)
                       (org-air-classify-item hipri org-air-test-now))))
      ;; ...and an `#A' CAPTURE is High priority + Inbox, never Untracked.
      (let ((urgent (org-air-r95--scanned "Urgent capture" items)))
        (should (equal '(high-priority inbox)
                       (org-air-classify-item urgent org-air-test-now))))
      ;; 6. the R72 law, per item.
      (let ((org-air-view--tag-filter '("is:untracked"))
            (org-air-filter-match 'all)
            (org-air-view--filter-now org-air-test-now)
            (org-air-view--scope nil)
            (org-air-view--render-partition nil))
        (dolist (item items)
          (ert-info ((format "is:untracked vs %s" (org-air-item-title item)))
            (should (eq (and (org-air-view--passes-filter-p item) t)
                        (and (memq 'untracked
                                   (org-air-classify-item item org-air-test-now))
                             t)))))))))

;;;; -------------------------------------------------------------------
;;;; r95-14 — FU7: the presentation clamp
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r95-14-days-text-clamps-at-ten-years ()
  "`--days-text' prints `%dd' up to ten years and `10y+' beyond it.

`[0001-01-01]' — a typo, a zeroed mtime, a bad restore — rendered
`~739781d quiet': an eight-character number in a narrow cell.

Four legs:

  1. the boundary is EXACT and stated in terms of the constant, so the
     two can never drift apart;
  2. `10y+' is TRUE of every value it replaces (it reads: more than ten
     years), which is the test a clamp must pass to be honest — swept
     over the whole clamped range;
  3. ordinary values are byte-identical to the old `%dd' spelling, so
     nothing a normal board prints moved;
  4. the constant is the ten years it claims to be.

Raising `org-air-view--days-text-max' reddens legs 1 and 4."
  (skip-unless (locate-library "org-air"))
  (let ((max org-air-view--days-text-max))
    ;; 4 + 1
    (should (= 3650 max))
    (should (equal (format "%dd" max) (org-air-view--days-text max)))
    (should (equal "10y+" (org-air-view--days-text (1+ max))))
    ;; 3: every ordinary value is unchanged
    (dolist (d '(0 1 2 7 30 90 210 365 1000 3649))
      (ert-info ((format "days %d" d))
        (should (equal (format "%dd" d) (org-air-view--days-text d)))))
    ;; 2: the clamp is honest over the whole range above the max
    (dolist (d (list (1+ max) 4000 10000 46186 739781 (* 100 max)))
      (ert-info ((format "absurd %d" d))
        (should (equal "10y+" (org-air-view--days-text d)))
        (should (> d (* 10 365)))))))

;;;; -------------------------------------------------------------------
;;;; r95-15 — FU7: the FACT is not clamped
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r95-15-the-clamp-is-presentation-only ()
  "Only the LABEL is clamped: sorts, thresholds and predicates keep the truth.

A clamp that reached the model would be a lie: a corrupt date would stop
ranking where it belongs and could even stop surfacing.  So the number
keeps flowing and only its spelling stops.

Five legs:

  1. `org-air-classify-quiet-floor-days' returns the TRUE (absurd) number;
  2. `org-air-classify-quiet-days' does too, and the attention threshold
     still fires on it;
  3. `--sort-by-floor' ranks the absurd file ABOVE a merely-old one — the
     clamp did not flatten the order;
  4. `--sort-by-quiet' likewise for a measured absurd age;
  5. and the LABEL for the very same item is the clamped one, so legs 1-4
     are about the same rows the user sees."
  (skip-unless (locate-library "org-air"))
  (org-air-r95--with-corpus
      (list (cons "ancient.org" "* TODO From the archives\nNothing.\n")
            (cons "old.org" "* TODO Merely old\nNothing.\n")
            (cons "inbox.org" "#+title: inbox\n"))
    (org-air-test-age-file (org-air-r95--path "ancient.org") 20000)
    (org-air-test-age-file (org-air-r95--path "old.org") 210)
    (let* ((items (org-air-query-items))
           (ancient (org-air-r95--scanned "From the archives" items))
           (old (org-air-r95--scanned "Merely old" items))
           (floor-days (org-air-classify-quiet-floor-days
                        ancient org-air-test-now)))
      ;; 1. the fact
      (should (= 20000 floor-days))
      (should (> floor-days org-air-view--days-text-max))
      ;; 3. the order
      (should (equal (list ancient old)
                     (org-air-viewport-test--with-frozen-now
                       (org-air-view--sort-by-floor (list old ancient)))))
      ;; 5. the label, for the same row
      (should (equal "~10y+ quiet"
                     (car (org-air-view--untracked-reason
                           ancient org-air-test-now))))
      (should (equal "~210d quiet"
                     (car (org-air-view--untracked-reason
                           old org-air-test-now)))))
    ;; 2 + 4: a MEASURED absurd age still ages, still ranks, still clamps.
    (let* ((absurd (org-air-r95--item :title "Absurd measured"
                                      :updated -20000))
           (merely (org-air-r95--item :title "Merely quiet" :updated -210)))
      (should (= 20000 (org-air-classify-quiet-days absurd org-air-test-now)))
      (should (memq 'attention (org-air-classify-item absurd org-air-test-now)))
      (should (equal (list absurd merely)
                     (org-air-viewport-test--with-frozen-now
                       (org-air-view--sort-by-quiet (list merely absurd)))))
      (should (equal "10y+ quiet"
                     (car (org-air-view--attention-reason
                           absurd org-air-test-now))))
      (should (equal "210d quiet"
                     (car (org-air-view--attention-reason
                           merely org-air-test-now)))))))

;;;; -------------------------------------------------------------------
;;;; r95-16 — FU7: every day count the board prints goes through it
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r95-16-every-printed-day-count-is-clamped ()
  "One shared formatter: the attention reason, the untracked reason, the
OVERDUE label and the rail's relative terms all degrade together.

A clamp applied at one call site is worse than no clamp: the board would
show `10y+' in one cell and `739781d' in another, and a reader would not
know which to believe.

Three legs:

  1. UNIT, every documented call site, with an absurd input and an
     ordinary one;
  2. END TO END: a real render of a corpus holding one absurd deadline
     and one absurd file prints `10y+' and NOWHERE prints a raw day
     count of five digits or more;
  3. the row cell and the rail agree on the same heading."
  (skip-unless (locate-library "org-air"))
  ;; 1. the call sites, as units.
  (let* ((absurd-deadline (org-air-r95--item :title "Absurd deadline"
                                             :deadline -20000))
         (ordinary (org-air-r95--item :title "Ordinary" :deadline -5))
         (far (time-add org-air-test-now (days-to-time 20000)))
         (long-ago (time-subtract org-air-test-now (days-to-time 20000))))
    (should (equal "OVERDUE 10y+"
                   (car (org-air-viewport-test--with-frozen-now
                          (org-air-view--date-label absurd-deadline 'overdue)))))
    (should (equal "OVERDUE 5d"
                   (car (org-air-viewport-test--with-frozen-now
                          (org-air-view--date-label ordinary 'overdue)))))
    (should (equal "in 10y+" (org-air-view--inspector-relative
                              far org-air-test-now)))
    (should (equal "10y+ ago" (org-air-view--inspector-relative
                               long-ago org-air-test-now)))
    (should (equal "in 5d" (org-air-view--inspector-relative
                            (time-add org-air-test-now (days-to-time 5))
                            org-air-test-now)))
    (should (equal "today" (org-air-view--inspector-relative
                            org-air-test-now org-air-test-now))))
  ;; 2 + 3: on the real board.
  (org-air-r95--with-corpus
      (list
       (cons "tasks.org"
             (concat "* TODO Ancient deadline\nDEADLINE: "
                     (org-air-r95--date -20000) "\n"
                     "* TODO Ancient and untracked\nNothing.\n"
                     "* TODO Ancient and measured\n"
                     (org-air-r95--stamp -20000) "\n"))
       (cons "inbox.org" "#+title: inbox\n"))
    (org-air-test-age-file (org-air-r95--path "tasks.org") 20000)
    (org-air-r95--render-board '(120 . 46)
      (let ((text (buffer-string)))
        (should (string-match-p "10y\\+" text))
        ;; no five-or-more digit day count anywhere on the board
        (ert-info ((format "board:\n%s" text))
          (should-not (string-match-p "[0-9]\\{5,\\}d" text))
          (should-not (string-match-p "20000" text))))
      ;; 3. the cell and the rail agree on the same heading.
      (let ((untracked (car (org-air-r95--rows 'untracked))))
        (should untracked)
        (should (string-match-p "~10y\\+ quiet" (nth 1 untracked)))))))

;;;; -------------------------------------------------------------------
;;;; THE COMPILED-ONLY FENCE GAP (r95-17 .. r95-20)
;;;;
;;;; `r90-50' asserts a LINEARITY law: a current repaint may not do list
;;;; membership work over `org-air-item' objects, because at 5k rows one
;;;; `memq' per item is the shape that turns a repaint quadratic.  It
;;;; measures the law by replacing `memq' and `assq' with counting shims
;;;; through `cl-letf'.
;;;;
;;;; That instrument only works on INTERPRETED code.  `memq', `assq' and
;;;; `member' are BYTE OPCODES: the compiler emits `Bmemq' inline and the
;;;; running code never consults the function cell the shim replaced.  So
;;;; in the mode we actually gate on (`make check' compiles first), the
;;;; strongest performance fence in this project is BLIND.
;;;;
;;;; R95 proved it the expensive way.  The first implementation of the
;;;; Untracked quota used `(memq item chosen)' and a `seq-filter' over the
;;;; same list.  The compiled gate went green.  The identical suite run
;;;; with no `.elc' present reddened `r90-50' immediately, at
;;;; `forbidden=5020' on the 5k corpus.  A real defect shipped past a
;;;; fence that exists to catch exactly it.
;;;;
;;;; The repair below does not try to make a compiled `memq' visible — it
;;;; cannot be.  It makes the PRODUCTION FUNCTIONS interpreted for the
;;;; duration of the probe, exactly as `load' would evaluate them
;;;; (`macroexpand-all' first, so compiler macros — struct accessors above
;;;; all — expand precisely as they do in the interpreted gate).  The law
;;;; is then enforced in BOTH modes by one test, and the three tests
;;;; around it make sure the instrument can never go quietly blind again.
;;;; -------------------------------------------------------------------

(defconst org-air-r95--linearity-modules
  '("org-air-view" "org-air-classify" "org-air-query" "org-air-layout"
    "org-air-calendar" "org-air-faces")
  "Production modules the linearity fence forces INTERPRETED.
Every module a REPAINT can reach: the renderer itself, the classifier and
the sort/section layer it calls per item, the query module that owns the
`org-air-item' struct and the row source keys, and the layout/face
modules the row builders call.  The point of taking whole modules rather
than a hand-picked list is that a violation cannot hide in a helper
nobody remembered to name.")

(defconst org-air-r95--linear-scan-primitives
  '(memq assq member assoc rassq delq remq)
  "List primitives whose cost is LINEAR in the list they are given.
Calling any of them with an `org-air-item' during a current repaint is
the R90 violation `r90-50' names.  The first three are byte OPCODES — see
`org-air-r95-17-compiled-memq-is-invisible-to-the-shim'.")

(defconst org-air-r95--fenced-repaint-functions
  '(org-air-view--collapsed-window
    org-air-view--displayed-for-bucket-1
    org-air-view--displayed-items-for-bucket
    org-air-view--sort-items
    org-air-view--sort-by-floor
    org-air-view--sort-by-recency
    org-air-view--item-activity
    org-air-view--days-text
    org-air-view--untracked-reason
    org-air-view--attention-reason
    org-air-toggle-section
    org-air-classify--planned-p
    org-air-classify--untracked-p
    org-air-classify-item)
  "Every function R95 added to or changed on the REPAINT path.
The linearity fence must cover all of them; `r95-20' asserts it does.")

(defun org-air-r95--module-source (module)
  "Return MODULE's SOURCE (.el) path, whatever `load' resolved to."
  (let ((found (locate-library module)))
    (and found (concat (file-name-sans-extension found) ".el"))))

(defun org-air-r95--interpret-file (file)
  "Install INTERPRETED definitions for every top-level defun in FILE.
Return an alist (SYMBOL . PREVIOUS-DEFINITION) for restoration.

Each form is `macroexpand-all'-ed before evaluation, which is what `load'
does to a source file: macros AND compiler macros expand, so a struct
accessor inlines to its `aref' exactly as it does under the interpreted
gate, while `memq' and friends stay ORDINARY FUNCTION CALLS that a
`cl-letf' shim can see.  Nothing but `defun'/`cl-defun' forms is touched,
so no `defvar', `defcustom', `defconst', keymap or mode definition is
re-run and no state moves."
  (let ((saved nil))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (and (consp form)
                         (memq (car form) '(defun cl-defun))
                         (symbolp (nth 1 form))
                         (fboundp (nth 1 form)))
                (push (cons (nth 1 form) (symbol-function (nth 1 form))) saved)
                (eval (macroexpand-all form) t))))
        (end-of-file nil)))
    saved))

(defun org-air-r95--call-interpreted (thunk)
  "Call THUNK with every `org-air-r95--linearity-modules' defun INTERPRETED.
Restores every replaced definition afterwards, whatever THUNK does."
  (let ((saved nil))
    (unwind-protect
        (progn
          (dolist (module org-air-r95--linearity-modules)
            (let ((file (org-air-r95--module-source module)))
              (should file)
              (should (file-exists-p file))
              (setq saved (append (org-air-r95--interpret-file file) saved))))
          (funcall thunk saved))
      (dolist (cell saved) (fset (car cell) (cdr cell))))))

(defmacro org-air-r95--with-interpreted-source (saved &rest body)
  "Run BODY with the production repaint path forced INTERPRETED.
SAVED is bound to the alist of replaced (SYMBOL . DEFINITION) pairs."
  (declare (indent 1) (debug t))
  `(org-air-r95--call-interpreted (lambda (,saved) (ignore ,saved) ,@body)))

(defvar org-air-r95--forbidden 0
  "Count of LINEAR SCANS over `org-air-item' objects seen by the shim.")

(defun org-air-r95--install-scan-counter ()
  "Replace every `org-air-r95--linear-scan-primitives' with a counting shim.
Return an alist (SYMBOL . PREVIOUS-DEFINITION) for restoration.  The shim
tests `(eq (type-of OBJECT) \\='org-air-item)' — the same test `r90-50'
uses, and deliberately NOT the struct predicate, whose own expansion
calls `memq' and would recurse through the shim forever."
  (let ((saved nil))
    (dolist (prim org-air-r95--linear-scan-primitives saved)
      (let ((original (symbol-function prim)))
        (push (cons prim original) saved)
        (fset prim
              (lambda (&rest args)
                (when (eq (type-of (car args)) 'org-air-item)
                  (setq org-air-r95--forbidden
                        (1+ org-air-r95--forbidden)))
                (apply original args)))))))

(defmacro org-air-r95--counting-linear-scans (&rest body)
  "Run BODY with `org-air-r95--forbidden' counting item-wise linear scans."
  (declare (indent 0) (debug t))
  `(let ((org-air-r95--saved-primitives nil))
     (setq org-air-r95--forbidden 0)
     (unwind-protect
         (progn
           (setq org-air-r95--saved-primitives
                 (org-air-r95--install-scan-counter))
           ,@body)
       (dolist (cell org-air-r95--saved-primitives)
         (fset (car cell) (cdr cell))))))

(defun org-air-r95--scale-items (count files)
  "Return COUNT durable board items spread over FILES, all UNTRACKED.
No plan and no record, which is the R95 shape: they land in the one
section whose collapsed window runs the per-file quota."
  (let (items)
    (dotimes (index count)
      (let ((file (nth (mod index (length files)) files)))
        (push (org-air-item-create
               :title (format "Task %05d" index)
               :tags '("file_native" "scale_tag")
               :file file :marker (cons file (1+ index))
               :todo "TODO" :priority 0 :kind 'heading :donep nil
               :ntype 'task)
              items)))
    (nreverse items)))

(defun org-air-r95--repaint-probe (items board)
  "Do a full CURRENT repaint of ITEMS in BOARD: render, filter, mark, fold.
The same action set `r90-50' drives, plus the R95 fold-row TAB, which is
the second place the collapsed window is computed."
  (with-current-buffer board
    (let ((org-air-view-width 120)
          (org-air-view-height 40)
          (org-air-show-inspector nil)
          (org-air-view--inspector-active nil))
      (org-air-view--render items nil)
      (org-air-filter '("#file_native"))
      (setq org-air-view--tag-filter nil)
      (org-air-view--render items nil)
      (goto-char (point-min))
      (org-air-view--goto-first-item)
      (org-air-toggle-mark)
      (let ((more (org-air-r95--more-row-position 'untracked)))
        (when more (goto-char more) (org-air-toggle-section))))))

;;;; -------------------------------------------------------------------
;;;; r95-17 — THE GAP, asserted as a fact
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r95-17-compiled-memq-is-invisible-to-the-shim ()
  "A byte-compiled `memq' bypasses the `cl-letf' shim `r90-50' counts with.

THIS TEST EXISTS TO MAKE A BLIND SPOT UNFORGETTABLE.  It asserts a
limitation of our own instrument rather than a property of the product,
because the limitation is what let a real linearity defect through a
green gate in this very round: `(memq item chosen)' in the first cut of
`org-air-view--collapsed-window' passed `make check' and reddened
`r90-50' at `forbidden=5020' the moment the same suite ran with no
`.elc' present.

The cause is mechanical.  `memq', `assq' and `member' are BYTE OPCODES:
the compiler emits them inline and the running code never looks at the
symbol's function cell, so replacing that cell can change nothing.
Other linear primitives are ordinary calls and stay visible.

Three legs:

  1. for each OPCODE primitive: the identical body, interpreted, is
     COUNTED; byte-compiled, it is NOT — same shim, same call, same
     arguments;
  2. for each NON-opcode primitive: both forms are counted, IDENTICALLY,
     so the difference really is opcodes and not something about the
     harness (`remq' counts twice in both forms: it is `delq' on a copy,
     which is itself on the watch list);
  3. the two probes really are what they claim (`byte-code-function-p').

If a future Emacs stops emitting one of these as an opcode this test
will fail, and that is correct: the honest response is to shrink the
blind list, not to widen the assertion."
  (skip-unless (locate-library "org-air"))
  (let* ((item (org-air-r95--item :title "Probe"))
         (blind '(memq assq member))
         (visible '(assoc rassq delq remq)))
    (dolist (prim (append blind visible))
      (let* ((form `(lambda (object list) (,prim object list)))
             (interpreted (eval form t))
             (compiled (byte-compile (eval form t)))
             (pair (list item (cons item t)))
             interpreted-count compiled-count)
        ;; 3. the probes are what they claim
        (should-not (byte-code-function-p interpreted))
        (should (byte-code-function-p compiled))
        (org-air-r95--counting-linear-scans
          (funcall interpreted item pair)
          (setq interpreted-count org-air-r95--forbidden))
        (org-air-r95--counting-linear-scans
          (funcall compiled item pair)
          (setq compiled-count org-air-r95--forbidden))
        (ert-info ((format "%S: interpreted=%S compiled=%S"
                           prim interpreted-count compiled-count))
          ;; the shim can always see the interpreted form
          (should (>= interpreted-count 1))
          ;; 1 + 2: and the opcodes, and only the opcodes, hide
          (if (memq prim blind)
              (should (= 0 compiled-count))
            (should (= interpreted-count compiled-count))))))))

;;;; -------------------------------------------------------------------
;;;; r95-18 — THE REPAIR: forced interpretation, proven
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r95-18-forced-interpretation-restores-the-shim-s-sight ()
  "The fence's instrument: production functions really become interpreted.

Since a compiled `memq' cannot be made visible, the fence makes the code
INTERPRETED instead — for the duration of one probe, from the SAME source
`load' reads, `macroexpand-all'-ed exactly as `load' would.  A harness
that quietly failed to do that would restore the blindness it exists to
remove, so it is asserted rather than assumed.

Six legs:

  1. OUTSIDE the harness the definitions are whatever this gate run
     compiled them to — recorded, so the test states which mode it is in;
  2. INSIDE, every sampled production function is NOT byte-code;
  3. FUNCTIONALLY IDENTICAL: `--collapsed-window' and `--days-text'
     return `equal' answers inside and outside, so the fence measures the
     shipped behaviour and not a paraphrase of it;
  4. the shim SEES an interpreted production-shaped violation (the exact
     `memq' shape R95 shipped by accident) — which is the whole point;
  5. and does NOT see the byte-compiled twin of the same function, which
     is the gap, restated at the real call site;
  6. RESTORATION IS EXACT: every replaced cell is `eq' to what it was."
  (skip-unless (locate-library "org-air"))
  (let* ((sorted (mapcar (lambda (i)
                           (org-air-r95--item
                            :title (format "Row %d" i)
                            :file (format "/tmp/r95/f%d.org" (mod i 3))))
                         (number-sequence 1 9)))
         (before-compiled
          (byte-code-function-p (symbol-function 'org-air-view--collapsed-window)))
         (before-cells
          (mapcar (lambda (sym) (cons sym (symbol-function sym)))
                  org-air-r95--fenced-repaint-functions))
         (outside-window (org-air-view--collapsed-window 'untracked sorted))
         (outside-text (org-air-view--days-text 739781))
         (mutant-form
          ;; the EXACT shape R95 shipped by accident and then fixed
          '(lambda (bucket sorted)
             (let ((limit (org-air-view--section-limit bucket)))
               (if (not (eq bucket 'untracked))
                   (seq-take sorted limit)
                 (let ((chosen nil) (n 0))
                   (dolist (item sorted)
                     (when (and (< n limit) (not (memq item chosen)))
                       (push item chosen)
                       (setq n (1+ n))))
                   (seq-filter (lambda (it) (memq it chosen)) sorted))))))
         seen-clean seen-mutant seen-compiled-mutant inside-compiled
         inside-window inside-text)
    ;; 1. which mode is this gate run in?
    (ert-info ((format "ambient mode: %s"
                       (if before-compiled "COMPILED" "interpreted")))
      (org-air-r95--with-interpreted-source saved
        (should (> (length saved) 100))
        ;; 2. genuinely interpreted now
        (setq inside-compiled
              (seq-filter (lambda (sym)
                            (byte-code-function-p (symbol-function sym)))
                          org-air-r95--fenced-repaint-functions))
        ;; 3. and functionally identical
        (setq inside-window (org-air-view--collapsed-window 'untracked sorted)
              inside-text (org-air-view--days-text 739781))
        ;; 4. the shim now SEES a production-shaped violation
        (org-air-r95--counting-linear-scans
          (cl-letf (((symbol-function 'org-air-view--collapsed-window)
                     (eval (macroexpand-all mutant-form) t)))
            (org-air-view--collapsed-window 'untracked sorted))
          (setq seen-mutant org-air-r95--forbidden))
        ;; ...and the FIXED function is clean under the same shim
        (org-air-r95--counting-linear-scans
          (org-air-view--collapsed-window 'untracked sorted)
          (setq seen-clean org-air-r95--forbidden))
        ;; 5. the same mutant, BYTE-COMPILED, is invisible — the gap again
        (org-air-r95--counting-linear-scans
          (cl-letf (((symbol-function 'org-air-view--collapsed-window)
                     (byte-compile (eval (macroexpand-all mutant-form) t))))
            (org-air-view--collapsed-window 'untracked sorted))
          (setq seen-compiled-mutant org-air-r95--forbidden))))
    (should (equal '() inside-compiled))
    (should (equal outside-window inside-window))
    (should (equal outside-text inside-text))
    (should (equal "10y+" outside-text))
    (should (> seen-mutant 0))
    (should (= 0 seen-clean))
    (should (= 0 seen-compiled-mutant))
    ;; 6. exact restoration
    (pcase-dolist (`(,sym . ,fn) before-cells)
      (ert-info ((format "restored %S" sym))
        (should (eq fn (symbol-function sym)))))
    (should (eq before-compiled
                (byte-code-function-p
                 (symbol-function 'org-air-view--collapsed-window))))))

;;;; -------------------------------------------------------------------
;;;; r95-19 — THE FENCE: linearity, enforced in BOTH modes
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r95-19-repaint-is-linear-in-both-gate-modes ()
  "A current repaint does ZERO list-membership work over items — either mode.

This is `r90-50''s law, measured through an instrument that is not blind
when the gate compiles.  The whole repaint path is forced INTERPRETED and
the same counting shim is installed; a single `memq' over an item
anywhere under `--render' / filter / mark / TAB is then counted, whether
or not `make check' compiled first.

The corpus is deliberately UNTRACKED-heavy: that is the section whose
collapsed window runs the per-file quota, and the quota is exactly where
R95's own linearity defect lived.

Five legs:

  1. a full repaint over 1200 items across 4 files does ZERO item-wise
     linear scans;
  2. ANTI-VACUITY — the probe really did the work: rows were painted, the
     quota really ran (at most two rows per file), and the fold row
     really expanded;
  3. ANTI-VACUITY — the shim really was live: the SAME probe with the
     pre-R95 `memq' quota spliced in counts thousands of violations;
  4. the count in leg 3 scales with the corpus, which is what makes this
     a LINEARITY fence and not a style check;
  5. and it is all restored: the board is killed and the primitives are
     the ones Emacs shipped."
  (skip-unless (locate-library "org-air"))
  (let* ((files (mapcar (lambda (i) (format "/tmp/org-air-r95-scale/f%d.org" i))
                        (number-sequence 1 4)))
         (board (get-buffer-create "*org-air-r95-scale*"))
         (mutant-form
          '(lambda (bucket sorted)
             (let ((limit (org-air-view--section-limit bucket)))
               (if (not (eq bucket 'untracked))
                   (seq-take sorted limit)
                 (let ((chosen nil) (n 0))
                   (dolist (item sorted)
                     (when (and (< n limit) (not (memq item chosen)))
                       (push item chosen)
                       (setq n (1+ n))))
                   (seq-filter (lambda (it) (memq it chosen)) sorted))))))
         clean rows fold-expanded mutant-small mutant-large)
    (unwind-protect
        (org-air-viewport-test--with-frozen-now
          (with-current-buffer board (org-air-view-mode))
          (org-air-r95--with-interpreted-source _saved
            ;; 1. the fence
            (let ((items (org-air-r95--scale-items 1200 files)))
              (with-current-buffer board
                (setq org-air-view--items items
                      org-air-view--items-key (org-air-view--cache-key)))
              (org-air-r95--counting-linear-scans
                (org-air-r95--repaint-probe items board)
                (setq clean org-air-r95--forbidden))
              ;; 2. the probe really did the work
              (with-current-buffer board
                (setq rows (org-air-r95--rows 'untracked)
                      fold-expanded (null (org-air-r95--more-row-position
                                           'untracked))))
              ;; 3 + 4. the shim really was live, and it SCALES
              (dolist (count '(300 1200))
                (let ((scaled (org-air-r95--scale-items count files)))
                  (with-current-buffer board
                    (setq org-air-view--expanded-sections nil
                          org-air-view--items scaled
                          org-air-view--items-key (org-air-view--cache-key)))
                  (org-air-r95--counting-linear-scans
                    (cl-letf (((symbol-function 'org-air-view--collapsed-window)
                               (eval (macroexpand-all mutant-form) t)))
                      (org-air-r95--repaint-probe scaled board))
                    (if (= count 300)
                        (setq mutant-small org-air-r95--forbidden)
                      (setq mutant-large org-air-r95--forbidden))))))))
      (when (buffer-live-p board)
        (let ((kill-buffer-query-functions nil)) (kill-buffer board))))
    (ert-info ((format "clean=%S mutant 300=%S 1200=%S rows=%S"
                       clean mutant-small mutant-large (length rows)))
      ;; 1
      (should (= 0 clean))
      ;; 2
      (should (> (length rows) 0))
      (should fold-expanded)
      ;; 3
      (should (> mutant-small 100))
      ;; 4 — four times the corpus, several times the violations
      (should (> mutant-large (* 2 mutant-small))))
    ;; 5
    (should (subrp (symbol-function 'memq)))
    (should (subrp (symbol-function 'assq)))
    (should-not (get-buffer "*org-air-r95-scale*"))))

;;;; -------------------------------------------------------------------
;;;; r95-20 — THE FENCE'S COVERAGE
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r95-20-linearity-fence-covers-the-whole-repaint-path ()
  "The fence's NET, stated: which modules, which functions, and nothing lost.

A fence that covers three hand-picked functions is a fence a future round
walks around without noticing.  This states the net as data and checks
it, so widening the repaint path forces a decision instead of a silent
hole.

Five legs:

  1. every module in `org-air-r95--linearity-modules' resolves to a
     SOURCE file that exists and yields defuns;
  2. together they cover EVERY function in
     `org-air-r95--fenced-repaint-functions' — the R95-touched repaint
     path — and the interpreted set is far larger than that list;
  3. the modules are the production modules and only those: each is a
     real `org-air-*' library, none is a test file;
  4. the primitives the shim watches include every OPCODE one, so the
     blind spot `r95-17' documents is the one the fence closes;
  5. `r90-50' itself is still present and still the compiled-mode fence
     it always was — this round ADDS a mode, it does not replace a test."
  (skip-unless (locate-library "org-air"))
  (let ((interpreted nil))
    ;; 1 + 2
    (org-air-r95--with-interpreted-source saved
      (setq interpreted (mapcar #'car saved)))
    (should (> (length interpreted) 400))
    (dolist (sym org-air-r95--fenced-repaint-functions)
      (ert-info ((format "fenced: %S" sym))
        (should (fboundp sym))
        (should (memq sym interpreted))))
    ;; 3
    (dolist (module org-air-r95--linearity-modules)
      (let ((file (org-air-r95--module-source module)))
        (ert-info ((format "module %s -> %s" module file))
          (should file)
          (should (file-exists-p file))
          (should (string-prefix-p "org-air-" (file-name-nondirectory file)))
          (should-not (string-match-p "/tests/" file))
          (should (> (length (org-air-r95--interpret-file file)) 0)))))
    ;; ...and the restoration from that last call is complete too
    (should (fboundp 'org-air-view--collapsed-window))
    ;; 4
    (dolist (prim '(memq assq member))
      (should (memq prim org-air-r95--linear-scan-primitives)))
    ;; 5 — R95 ADDS a mode to the law; it does not replace `r90-50'.
    (require 'org-air-round90-test nil t)
    (should (ert-test-boundp
             'org-air-r90-50-source-tracking-is-linear-at-1k-and-5k))))

(provide 'org-air-round95-test)
;;; org-air-round95-test.el ends here
