;;; org-air-review.el --- Review (retrospective) view for org-air -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: org-air contributors
;; Keywords: outlines, calendar
;; Version: 0.1.0
;; URL: https://github.com/rytswd/org-air
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; R61: ONE view answering "what happened over this week / month" — the
;; retrospective surface.  Four sections over the scan's per-heading
;; review facts (the R61-1 harvest: CLOCK intervals, LOGBOOK stamps,
;; `:CREATED:'): Completed / Time invested / Started / Carried over.
;; The third leg of the family: board = now, revisit = evergreen,
;; review = retrospect.
;;
;; Period navigation (`<' / `>' / `.'; the R62-2 range ladder
;; week ↔ fortnight ↔ month ↔ quarter ↔ year on `+'/`-'/`m') is a
;; RENDER state: a pure filter+fold over cached integer lists — zero
;; file I/O, NEVER a rescan (the period parameters are deliberately not
;; cache-key elements, R61-2).  Data arrives through the same never-blocking tiers
;; as the Revisit view: warm borrow from a live board, cache hydrate,
;; else the R56-paced cold fill (batch scans synchronously so ERT stays
;; deterministic).
;;
;; Kept out of the view file per the module split convention; the
;; view/render machinery (shared V6 row primitive, rail descriptor,
;; sort core, filter core, R58 bookmark quartet) is REUSED, never
;; forked.  All R53 laws hold: never-hang, never-error, data-pure
;; render, bounded memory.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'calendar)
(require 'cal-iso)
(require 'org-air-query)
(require 'org-air-view)
(require 'org-air-calendar)

;; R58: `bookmark-make-record-function' is bookmark.el's (not preloaded);
;; the mode sets it buffer-locally without requiring bookmark at load.
(defvar bookmark-make-record-function)

;;;; ---------------------------------------------------------------------
;;;; Knobs
;;;; ---------------------------------------------------------------------

(defcustom org-air-review-suspect-clock-hours 16
  "Hours above which a single CLOCK interval is SUSPECT (R61-3).
An interval longer than this is EXCLUDED from every total and surfaced
on its own line under Time invested (\"⚠ N suspect clock(s), H:MM
excluded\") with the owning headings listed — a forgotten 3-day clock
swamping a week's report is the common failure mode this surface exists
to repair; including it would make totals useless, hiding it would be a
lie.  Render-time only: a threshold change repaints, never rescans.
nil disables the rule entirely."
  :type '(choice (const :tag "Disabled" nil) number)
  :group 'org-air)

(defcustom org-air-review-rail-placement nil
  "REVIEW override for `org-air-rail-placement' (R62-1d).
nil (the default) inherits the shared `org-air-rail-placement'; `inline'
or `side-window' pins the review view regardless of the shared default.
Resolved through `org-air-rail--placement'."
  :type '(choice (const :tag "Inherit `org-air-rail-placement'" nil)
                 (const inline) (const side-window))
  :group 'org-air)

(defconst org-air-review--range-ladder '(week fortnight month quarter year)
  "The FULL range ladder, narrowest → widest (R62-2).
The domain of `org-air-review--period-kind' and the natural rank order
every ladder walk uses; `org-air-review-ranges' trims which rungs the
keys visit, never what a bookmark may restore.")

(defcustom org-air-review-ranges '(week fortnight month quarter year)
  "The range rungs `+' / `-' / `m' walk, in ladder order (R62-2).
A list drawn from `week' / `fortnight' / `month' / `quarter' / `year'.
Validated at use, never trusted raw: unknown symbols are dropped, an
empty result degrades to (week month), and a current kind missing from
the trimmed ladder still participates at its natural rank so the user
is never trapped on an unreachable rung.  Render state only — like the
kind and the anchor it is deliberately NOT a cache-key element (only
scan-shaping knobs join the key, R61-2/R57)."
  :type '(repeat (choice (const week) (const fortnight) (const month)
                         (const quarter) (const year)))
  :group 'org-air)

(defconst org-air-review-buffer-name "*org-air review*"
  "Name of the Review view buffer (R61-4).")

;;;; ---------------------------------------------------------------------
;;;; Buffer state
;;;; ---------------------------------------------------------------------

(defvar-local org-air-review--period-kind 'week
  "Shown period kind, one of `org-air-review--range-ladder' (R62-2).
`week' (ISO, the default) / `fortnight' / `month' / `quarter' / `year';
walked by `+' / `-' (one rung, clamped) and `m' (full cycle).")

(defvar-local org-air-review--period-anchor nil
  "Integer epoch anchoring the shown period, or nil (R61-3).
nil means \"the CURRENT period\", recomputed each render — the default
surface tracks today across midnight for free.  Navigation normalises
the anchor to the shown period's START epoch.")

(defvar-local org-air-review--rollup 'day
  "Active rollup basis: `day' (default) / `tag' / `directory' / `origin'.
Cycled by `f' (R61-3) and applied coherently: Completed groups its rows
by it; Time invested aggregates by it.")

(defvar-local org-air-review--items nil
  "The item snapshot this review buffer folds over (R61-4).
Filled by `org-air-review--ensure-data' (warm borrow / cache hydrate /
paced cold fill); items are immutable to review (read-only share).")

(defvar-local org-air-review--collapsed nil
  "Review sections currently folded shut (TAB), a list of symbols.")

(defvar-local org-air-review--count nil
  "Completed count of the last render (header + mode-line).")

(defvar-local org-air-review--rendered-width nil
  "Width of the most recent Review render (the resize-refresh guard).")

(defvar-local org-air-review--fill-token 0
  "Monotonic token guarding the cold-fill pacer's slices (R61-4).")

(defvar-local org-air-review--fill-queue nil
  "Files the in-flight cold fill has not scanned yet (R61-4).")

(defvar-local org-air-review--fill-total 0
  "Total file count of the in-flight cold fill (R61-4).")

(defvar-local org-air-review--fill-timer nil
  "The single repeating wall-clock pacer of the cold fill, or nil.")

(defvar-local org-air-review--fill-last-paint nil
  "Float time of the last progressive cold-fill repaint, or nil.")

(defvar-local org-air-review--fill-items nil
  "Items the in-flight cold fill has collected so far (R61-4).")

(defvar-local org-air-review--bookmark-locator nil
  "Armed point locator of an in-flight bookmark restore, or nil (R61-6).
The review twin of `org-air-view--bookmark-locator': a plist
\(:item (FILE . POS) :title TITLE) consumed at the render tail; stays
armed across the paced cold-fill's progressive paints until the row
appears or the fill goes idle (one-shot either way).")

;;;; ---------------------------------------------------------------------
;;;; The period engine — pure functions, unit-testable without a buffer
;;;; (R61-3).  ISO math rides the stock `cal-iso' oracle; boundaries are
;;;; built with `encode-time' on LOCAL calendar dates, so a DST-crossing
;;;; period has 23/25-hour days and stays exact by construction.
;;;; ---------------------------------------------------------------------

(defun org-air-review--date-epoch (gregorian)
  "Return the local-midnight INTEGER epoch of GREGORIAN (MONTH DAY YEAR)."
  (floor (float-time (encode-time (list 0 0 0 (nth 1 gregorian)
                                        (nth 0 gregorian) (nth 2 gregorian)
                                        nil -1 nil)))))

(defconst org-air-review--fortnight-phase
  (calendar-absolute-from-gregorian '(1 5 1970))
  "Absolute (fixed) day number of Monday 1970-01-05 — the fortnight phase.
R62-2: fortnights are FIXED-PHASE Monday-anchored 14-day blocks over
`calendar-absolute-from-gregorian' day numbers, so every fortnight is
exactly two consecutive ISO weeks and there is NO year seam.  The
\"obvious\" ISO-year-local odd/even week pairing was RULED OUT: a
53-week ISO year (2026 is one) leaves W53 unpaired, breaking the
partition law (every instant in exactly ONE period) that `<'/`>'
adjacency and clip complementarity are built on.  Which week of a pair
leads is phase-determined, not year-determined.")

(defun org-air-review--period-bounds (kind anchor)
  "Return KIND's half-open (START . END) integer bounds containing ANCHOR.
KIND `week' is the ISO week (Monday 00:00 local → next Monday 00:00,
via the `cal-iso' oracle); `fortnight' the fixed-phase Monday-anchored
14-day block (`org-air-review--fortnight-phase'); `month' the calendar
month (1st 00:00 → next month's 1st); `quarter' the calendar quarter
\(Jan–Mar / Apr–Jun / Jul–Sep / Oct–Dec); `year' the calendar year.
ANCHOR is any integer epoch inside the period.  Durations are pure
integer subtraction between two local-midnight epochs — DST-exact by
construction; an unknown KIND totals to the `week' branch (never
signals)."
  (let* ((d (decode-time anchor))
         (month (decoded-time-month d))
         (day (decoded-time-day d))
         (year (decoded-time-year d)))
    (pcase kind
      ('month
       (cons (org-air-review--date-epoch (list month 1 year))
             (org-air-review--date-epoch
              (if (= month 12)
                  (list 1 1 (1+ year))
                (list (1+ month) 1 year)))))
      ('fortnight
       (let* ((abs (calendar-absolute-from-gregorian (list month day year)))
              (start (- abs (mod (- abs org-air-review--fortnight-phase)
                                 14))))
         (cons (org-air-review--date-epoch
                (calendar-gregorian-from-absolute start))
               (org-air-review--date-epoch
                (calendar-gregorian-from-absolute (+ start 14))))))
      ('quarter
       (let ((qm (1+ (* 3 (/ (1- month) 3)))))
         (cons (org-air-review--date-epoch (list qm 1 year))
               (org-air-review--date-epoch
                (if (= qm 10)
                    (list 1 1 (1+ year))
                  (list (+ qm 3) 1 year))))))
      ('year
       (cons (org-air-review--date-epoch (list 1 1 year))
             (org-air-review--date-epoch (list 1 1 (1+ year)))))
      (_
       (let* ((iso (calendar-iso-from-absolute
                    (calendar-absolute-from-gregorian
                     (list month day year))))
              (monday (calendar-iso-to-absolute
                       (list (nth 0 iso) 1 (nth 2 iso)))))
         (cons (org-air-review--date-epoch
                (calendar-gregorian-from-absolute monday))
               (org-air-review--date-epoch
                (calendar-gregorian-from-absolute (+ monday 7)))))))))

(defun org-air-review--effective-ranges (&optional kind)
  "Return the validated range ladder the keys walk, in natural rank order.
R62-2: `org-air-review-ranges' filtered against the full
`org-air-review--range-ladder' (unknown symbols dropped, an empty
result degrading to (week month)); a non-nil KIND absent from the
trimmed ladder is spliced in at its natural rank, so an off-ladder
current kind still widens/narrows/cycles out instead of trapping."
  (let* ((knob (and (listp org-air-review-ranges) org-air-review-ranges))
         (kept (or (seq-filter (lambda (r) (memq r knob))
                               org-air-review--range-ladder)
                   '(week month))))
    (if (or (null kind) (memq kind kept))
        kept
      (seq-filter (lambda (r) (or (memq r kept) (eq r kind)))
                  org-air-review--range-ladder))))

(defun org-air-review--iso-week (epoch)
  "Return EPOCH's ISO commercial week as (WEEK . YEAR) via `cal-iso'."
  (let* ((d (decode-time epoch))
         (iso (calendar-iso-from-absolute
               (calendar-absolute-from-gregorian
                (list (decoded-time-month d) (decoded-time-day d)
                      (decoded-time-year d))))))
    (cons (nth 0 iso) (nth 2 iso))))

(defun org-air-review--bounds ()
  "Return the SHOWN period's half-open (START . END) integer bounds.
A nil `org-air-review--period-anchor' means the CURRENT period."
  (org-air-review--period-bounds org-air-review--period-kind
                                 (or org-air-review--period-anchor
                                     (floor (float-time)))))

(defun org-air-review--clip (start end p0 p1)
  "Return interval [START, END)'s overlap with period [P0, P1), in seconds.
The exactness law: max(0, min(END, P1) − max(START, P0)) — a
boundary-crossing interval contributes complementary parts to adjacent
periods, nothing dropped, nothing double-counted (integer seconds)."
  (max 0 (- (min end p1) (max start p0))))

(defun org-air-review--day-bounds (p0 p1)
  "Return the ((DAY-START . DAY-END) …) local calendar days covering [P0, P1).
Day edges come from `encode-time' on successive calendar dates, so a
DST-crossing period yields 23/25-hour days that still sum exactly."
  (let* ((d (decode-time p0))
         (abs (calendar-absolute-from-gregorian
               (list (decoded-time-month d) (decoded-time-day d)
                     (decoded-time-year d))))
         (start p0)
         (out nil))
    (while (< start p1)
      (setq abs (1+ abs))
      (let ((next (min p1 (org-air-review--date-epoch
                           (calendar-gregorian-from-absolute abs)))))
        (push (cons start next) out)
        (setq start (if (> next start) next p1))))
    (nreverse out)))

(defun org-air-review--hhmm (secs)
  "Format SECS (integer seconds) as the H:MM print shape (R61-3).
Section sums stay exact integer arithmetic; H:MM appears only here, at
print time."
  (format "%d:%02d" (/ secs 3600) (/ (% secs 3600) 60)))

;;;; ---------------------------------------------------------------------
;;;; The fold — every predicate below reads item slots ONLY (the
;;;; data-pure render law: no file opens anywhere in this section).
;;;; ---------------------------------------------------------------------

(defun org-air-review--fold-item-p (item)
  "Non-nil when ITEM participates in the PER-ITEM sections (R61-3).
`file' items (a blob has no per-heading LOGBOOK) and R59 pure containers
are skipped in Completed / Started / Carried over; Time invested
deliberately folds EVERY heading's own-body clocks instead — time is
attributed where it was clocked."
  (and (eq (org-air-item-kind item) 'heading)
       (not (org-air-query-container-item-p item))))

(defun org-air-review--suspect-limit ()
  "Return the suspect-clock threshold in seconds, or nil when disabled."
  (and org-air-review-suspect-clock-hours
       (floor (* 3600 org-air-review-suspect-clock-hours))))

(defun org-air-review--item-time (item p0 p1)
  "Return ITEM's clipped, suspect-filtered clock seconds inside [P0, P1)."
  (let ((limit (org-air-review--suspect-limit))
        (sum 0))
    (pcase-dolist (`(,s . ,e) (org-air-item-clocks item))
      (unless (and limit (> (- e s) limit))
        (setq sum (+ sum (org-air-review--clip s e p0 p1)))))
    sum))

(defun org-air-review--item-suspects (item p0 p1)
  "Return (COUNT . SECS) of ITEM's suspect clocks overlapping [P0, P1).
SECS is the clipped amount the exclusion kept out of the period's
totals — surfaced loud and separate, never summed silently."
  (let ((limit (org-air-review--suspect-limit))
        (n 0)
        (secs 0))
    (when limit
      (pcase-dolist (`(,s . ,e) (org-air-item-clocks item))
        (when (> (- e s) limit)
          (let ((part (org-air-review--clip s e p0 p1)))
            (when (> part 0)
              (setq n (1+ n)
                    secs (+ secs part)))))))
    (cons n secs)))

(defun org-air-review--completed-stamps (item p0 p1)
  "Return ITEM's completion epochs inside [P0, P1), newest-first, or nil.
The `done' stamps in the item's logs; when there are NONE and the
CLOSED stamp falls in the period, the CLOSED stamp counts (fallback,
not addition — the final done transition and CLOSED normally coincide,
so stamps win to avoid double counting)."
  (or (cl-loop for (epoch . kind) in (org-air-item-logs item)
               when (and (eq kind 'done) (>= epoch p0) (< epoch p1))
               collect epoch)
      (when-let* ((closed (org-air-query--time-float
                           (org-air-item-closed item))))
        (let ((closed (floor closed)))
          (and (>= closed p0) (< closed p1) (list closed))))))

(defun org-air-review--done-at-p (item time currentp)
  "Non-nil when ITEM counts as DONE strictly before epoch TIME (R61-3).
In order: the newest `done'/`todo' stamp < TIME decides; else a CLOSED
stamp < TIME means done; else, when CURRENTP (the shown period is the
current one), the live `donep' slot; else NOT done — the R59 house
default (when in doubt, render): an unlogged, uncloseable item with
activity shows as carried rather than silently vanishing."
  (let ((verdict 'unknown))
    (catch 'decided
      (pcase-dolist (`(,epoch . ,kind) (org-air-item-logs item))
        (when (and kind (< epoch time))
          (setq verdict (eq kind 'done))
          (throw 'decided t))))
    (if (not (eq verdict 'unknown))
        verdict
      (let ((closed (org-air-query--time-float (org-air-item-closed item))))
        (cond ((and closed (< closed time)) t)
              (currentp (and (org-air-item-donep item) t))
              (t nil))))))

(defun org-air-review--activity-p (item p0 p1)
  "Non-nil when ITEM had any recorded activity inside [P0, P1).
Any log stamp (notes included) or any clock overlap — suspect clocks
count as activity too (they happened; only their TIME is excluded)."
  (or (cl-loop for (epoch . _kind) in (org-air-item-logs item)
               thereis (and (>= epoch p0) (< epoch p1)))
      (cl-loop for (s . e) in (org-air-item-clocks item)
               thereis (> (org-air-review--clip s e p0 p1) 0))))

(defun org-air-review--activity-epoch (item p0 p1)
  "Return ITEM's latest activity epoch inside [P0, P1), or nil."
  (let ((best nil))
    (pcase-dolist (`(,epoch . ,_kind) (org-air-item-logs item))
      (when (and (>= epoch p0) (< epoch p1)
                 (or (null best) (> epoch best)))
        (setq best epoch)))
    (pcase-dolist (`(,s . ,e) (org-air-item-clocks item))
      (when (> (org-air-review--clip s e p0 p1) 0)
        (let ((tip (min (1- p1) e)))
          (when (or (null best) (> tip best))
            (setq best tip)))))
    best))

(defun org-air-review--started-epoch (item p0 p1)
  "Return ITEM's Started epoch when it falls inside [P0, P1), else nil.
`created' decides when present; the fallback (draft-preserved) is the
EARLIEST retained log stamp, taken only when `rtrunc' is nil — a
truncated heading's oldest retained stamp is not its birth, so
truncated headings never claim Started through the fallback."
  (let ((created (org-air-item-created item)))
    (if created
        (and (>= created p0) (< created p1) created)
      (unless (org-air-item-rtrunc item)
        (when-let* ((logs (org-air-item-logs item)))
          (let ((earliest (car (last logs))))
            (and (>= (car earliest) p0) (< (car earliest) p1)
                 (car earliest))))))))

(defun org-air-review--carried-p (item p0 p1 currentp)
  "Non-nil when ITEM is CARRIED OVER in [P0, P1) (R61-3).
Activity in the period AND still not-done at the period's end;
CURRENTP gates the live-`donep' tier of the done-at inference.  An item
completed after the period's end correctly reads as carried here."
  (and (org-air-review--activity-p item p0 p1)
       (not (org-air-review--done-at-p item p1 currentp))))

(defun org-air-review--section-data (items p0 p1 currentp)
  "Fold ITEMS into the four review sections for [P0, P1) (R61-3).
CURRENTP marks the shown period as the current one (the live-`donep'
tier).  One pass, slots only.  Returns a plist:
`:completed' ((ITEM LATEST COUNT STAMPS) …), `:time-items'
\((ITEM . SECS) …) with SECS > 0, `:time-total', `:suspect-count' /
`:suspect-secs' / `:suspect-items', `:trunc' (rtrunc contributors),
`:started' and `:carried' ((ITEM EPOCH) …)."
  (let ((completed nil) (started nil) (carried nil)
        (time-items nil) (time-total 0)
        (suspect-count 0) (suspect-secs 0) (suspect-items nil)
        (trunc 0))
    (dolist (item items)
      ;; Time invested folds EVERY heading's own-body clocks — containers
      ;; included (aggregates only; no container ROW can appear).
      (let ((secs (org-air-review--item-time item p0 p1)))
        (when (> secs 0)
          (push (cons item secs) time-items)
          (setq time-total (+ time-total secs))
          (when (org-air-item-rtrunc item)
            (setq trunc (1+ trunc)))))
      (pcase-let ((`(,n . ,secs) (org-air-review--item-suspects item p0 p1)))
        (when (> n 0)
          (setq suspect-count (+ suspect-count n)
                suspect-secs (+ suspect-secs secs))
          (push item suspect-items)))
      (when (org-air-review--fold-item-p item)
        (when-let* ((stamps (org-air-review--completed-stamps item p0 p1)))
          (push (list item (car stamps) (length stamps) stamps) completed))
        (when-let* ((epoch (org-air-review--started-epoch item p0 p1)))
          (push (list item epoch) started))
        (when (org-air-review--carried-p item p0 p1 currentp)
          (push (list item (or (org-air-review--activity-epoch item p0 p1)
                               p0))
                carried))))
    (list :completed (nreverse completed)
          :time-items (nreverse time-items)
          :time-total time-total
          :suspect-count suspect-count
          :suspect-secs suspect-secs
          :suspect-items (nreverse suspect-items)
          :trunc trunc
          :started (nreverse started)
          :carried (nreverse carried))))

;;;; ---------------------------------------------------------------------
;;;; Rollup (the lens) — one basis, applied coherently (R61-3).
;;;; ---------------------------------------------------------------------

(defun org-air-review--rollup-labels (item basis)
  "Return ITEM's rollup label list under BASIS (`tag'/`directory'/`origin').
A multi-tag heading contributes to EACH of its tag rows — the tag
rollup is a lens; the section's headline total comes from the item
fold, never from summing rollup rows."
  (pcase basis
    ('tag (or (mapcar (lambda (tag) (concat "#" tag))
                      (org-air-item-tags item))
              (list "(untagged)")))
    ('directory (list (or (org-air-item-group item) "(no group)")))
    (_ (list (file-name-nondirectory (or (org-air-item-file item) ""))))))

(defun org-air-review--time-rollup (time-items basis p0 p1)
  "Aggregate TIME-ITEMS ((ITEM . SECS) …) by BASIS over [P0, P1).
Returns ((LABEL SECS COUNT) …): `day' yields the period's days in
chronological order (per-day re-clip of each item's filtered clocks);
the other bases sort by SECS descending."
  (if (eq basis 'day)
      (let ((out nil))
        (pcase-dolist (`(,d0 . ,d1) (org-air-review--day-bounds p0 p1))
          (let ((secs 0) (count 0))
            (pcase-dolist (`(,item . ,_secs) time-items)
              (let ((part (org-air-review--item-time item d0 d1)))
                (when (> part 0)
                  (setq secs (+ secs part)
                        count (1+ count)))))
            (when (> secs 0)
              (push (list (format-time-string "%a %b %-d" d0) secs count)
                    out))))
        (nreverse out))
    (let ((table (make-hash-table :test #'equal)))
      (pcase-dolist (`(,item . ,secs) time-items)
        (dolist (label (org-air-review--rollup-labels item basis))
          (let ((cell (gethash label table)))
            (if cell
                (setcar cell (cons (+ (caar cell) secs)
                                   (1+ (cdar cell))))
              (puthash label (list (cons secs 1)) table)))))
      (let (out)
        (maphash (lambda (label cell)
                   (push (list label (caar cell) (cdar cell)) out))
                 table)
        (sort out (lambda (a b) (> (nth 1 a) (nth 1 b))))))))

(defun org-air-review--sort-rows (rows)
  "Return the per-item ROWS ((ITEM EPOCH …) …) under the shared sort.
Key `date' (default) orders by the row's period epoch, `title'
alphabetically; `O' reverses (the R22-3 shared core)."
  (let* ((key (or org-air-view--sort-key 'date))
         (desc (eq org-air-view--sort-direction 'descending))
         (sorted
          (if (eq key 'title)
              (sort (copy-sequence rows)
                    (lambda (a b)
                      (string-lessp
                       (downcase (org-air-item-title (nth 0 a)))
                       (downcase (org-air-item-title (nth 0 b))))))
            (sort (copy-sequence rows)
                  (lambda (a b) (< (nth 1 a) (nth 1 b)))))))
    (if desc (nreverse sorted) sorted)))

(defun org-air-review--group-rows (rows basis)
  "Group the per-item ROWS by BASIS into ((KEY LABEL . ROWS) …).
`day' keys on each row's epoch date (chronological order); the other
bases key on `org-air-review--rollup-labels' (label order), so a
multi-tag item appears under each of its tag groups.  Row order inside
a group preserves ROWS order (sort upstream)."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (row rows)
      (let ((keys (if (eq basis 'day)
                      (list (cons (format-time-string "%Y-%m-%d" (nth 1 row))
                                  (format-time-string "%a %b %-d"
                                                      (nth 1 row))))
                    (mapcar (lambda (label) (cons label label))
                            (org-air-review--rollup-labels (nth 0 row)
                                                           basis)))))
        (pcase-dolist (`(,key . ,label) keys)
          (let ((cell (gethash key table)))
            (if cell
                (setcdr (cdr cell) (cons row (cddr cell)))
              (puthash key (list key label row) table))))))
    (let (out)
      (maphash (lambda (_key cell)
                 (push (cons (nth 0 cell)
                             (cons (nth 1 cell)
                                   (nreverse (cddr cell))))
                       out))
               table)
      (sort out (lambda (a b) (string-lessp (car a) (car b)))))))

;;;; ---------------------------------------------------------------------
;;;; Data path (never-blocking, R53 laws inherited)
;;;; ---------------------------------------------------------------------

(defun org-air-review--fill-disarm ()
  "Cancel the cold-fill pacer, if armed."
  (when (timerp org-air-review--fill-timer)
    (cancel-timer org-air-review--fill-timer))
  (setq org-air-review--fill-timer nil))

(defun org-air-review--fill-start ()
  "Start the paced cold fill of the review item snapshot (R61-4).
Revisit's pacer verbatim: token-guarded budgeted slices
\(`org-air-refresh-slice-budget') on the repeating wall-clock pace
\(`org-air-view--refresh-wallclock-pace'), collecting the items each
`org-air-query--scan-file' returns; progressive repaint at most once
per `org-air-cold-paint-interval'.  NEVER a synchronous scan on the
interactive path; under `noninteractive' the callers scan synchronously
instead (deterministic ERT), so no timer is ever armed in batch."
  (cl-incf org-air-review--fill-token)
  (org-air-review--fill-disarm)
  (org-air-query-skip-log-reset)
  (setq org-air-review--fill-queue (org-air-query-files)
        org-air-review--fill-total (length org-air-review--fill-queue)
        org-air-review--fill-items nil
        org-air-review--fill-last-paint (float-time))
  (if (null org-air-review--fill-queue)
      (org-air-review--fill-finish)
    (unless noninteractive
      (setq org-air-review--fill-timer
            (run-with-timer org-air-view--refresh-wallclock-pace
                            org-air-view--refresh-wallclock-pace
                            #'org-air-review--fill-slice
                            (current-buffer)
                            org-air-review--fill-token)))))

(defun org-air-review--fill-slice (buffer token)
  "Drain one budgeted cold-fill slice for BUFFER under TOKEN (R61-4).
Consumes queued files until `org-air-refresh-slice-budget' is exceeded
\(minimum 1 — the R53 P1c shape); repaints progressively at most once
per `org-air-cold-paint-interval'; a stale TOKEN or dead BUFFER is a
silent no-op, so a superseded fill can never touch the view."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (eq token org-air-review--fill-token)
        (let ((deadline (+ (float-time) org-air-refresh-slice-budget))
              (first t))
          (while (and org-air-review--fill-queue
                      (or first (< (float-time) deadline)))
            (setq first nil)
            (setq org-air-review--fill-items
                  (nconc org-air-review--fill-items
                         (org-air-query--scan-file
                          (pop org-air-review--fill-queue))))))
        (if (null org-air-review--fill-queue)
            (org-air-review--fill-finish)
          (when (>= (- (float-time)
                       (or org-air-review--fill-last-paint 0))
                    org-air-cold-paint-interval)
            (setq org-air-review--fill-last-paint (float-time))
            (setq org-air-review--items org-air-review--fill-items)
            (org-air-review--render-current)))))))

(defun org-air-review--fill-finish ()
  "Complete the cold fill: disarm, adopt the items, render once."
  (org-air-review--fill-disarm)
  (setq org-air-review--items org-air-review--fill-items
        org-air-review--fill-queue nil)
  (org-air-review--render-current))

(defun org-air-review--fill-teardown ()
  "Kill-buffer hook: a dying Review buffer takes its pacer with it."
  (cl-incf org-air-review--fill-token)
  (org-air-review--fill-disarm))

(defun org-air-review--ensure-data ()
  "Make sure the item snapshot has data, never blocking the frame (R61-4).
In order: WARM borrow — the `*org-air*' board is live, its items were
built under the live `org-air-view--cache-key' and its refresh machine
is idle ⇒ snapshot its items (read-only share, zero I/O).  CACHE
hydrate — `org-air-view--cache-read' under the SAME key ⇒ adopt
`:items' (cons-marker slots, the R26-8 shape; one bounded disk read,
zero file opens).  COLD — pace the fill (interactive) or scan
synchronously (batch only — deterministic ERT)."
  (when (null org-air-review--items)
    (let ((board (get-buffer org-air-view-buffer-name)))
      (when (and (buffer-live-p board)
                 (with-current-buffer board
                   (and (derived-mode-p 'org-air-view-mode)
                        org-air-view--items
                        (equal org-air-view--items-key
                               (org-air-view--cache-key))
                        (not (eq org-air-view--refresh-state 'refreshing)))))
        (setq org-air-review--items
              (copy-sequence
               (buffer-local-value 'org-air-view--items board)))))
    (when (null org-air-review--items)
      (when-let* ((data (org-air-view--cache-read)))
        (setq org-air-review--items (plist-get data :items))
        (org-air-query-file-meta-hydrate (plist-get data :file-meta))
        (org-air-query-visits-hydrate (plist-get data :visits))))
    (when (null org-air-review--items)
      (if noninteractive
          (setq org-air-review--items (org-air-query-items))
        (org-air-review--fill-start)))))

;;;; ---------------------------------------------------------------------
;;;; Visible items — filter + scope BEFORE the fold, so totals honestly
;;;; describe what is shown (R61-4).
;;;; ---------------------------------------------------------------------

(defun org-air-review--visible-items ()
  "Return the item snapshot after the live filter and scope.
The `/' filter matches title + file leaf + group + tags through the
shared `org-air-view--tokens-pass-filter-p' (the R24-6 mini-language);
`s' is the board's structural lens (`org-air-view--passes-scope-p').
Pure slot/string work — zero file opens."
  (seq-filter
   (lambda (item)
     (and (org-air-view--passes-scope-p item)
          (org-air-view--tokens-pass-filter-p
           (concat (org-air-item-title item) " "
                   (file-name-nondirectory (or (org-air-item-file item) ""))
                   " " (or (org-air-item-group item) ""))
           (org-air-item-tags item))))
   org-air-review--items))

;;;; ---------------------------------------------------------------------
;;;; Rows + sections
;;;; ---------------------------------------------------------------------

(defconst org-air-review--trunc-marker "⚠ history truncated"
  "The inline marker a `rtrunc' item's rows carry (R61-1/T11).
Truncation is never silent: period totals older than the retained
window under-report only on marked headings.")

(defun org-air-review--row-date-text (row)
  "Return the UNFACED date-cell text for the per-item ROW (ITEM EPOCH [N]).
The stamp's day, the ×N count chip when the row folds N > 1 completions
\(a daily habit reads \"×7\", not seven rows), and the `rtrunc' marker."
  (let ((item (nth 0 row))
        (epoch (nth 1 row))
        (n (nth 2 row)))
    (concat (format-time-string "%b %-d" epoch)
            (when (and (integerp n) (> n 1)) (format " ×%d" n))
            (when (org-air-item-rtrunc item)
              (concat " " org-air-review--trunc-marker)))))

(defun org-air-review--item-tagstr (item)
  "Return ITEM's tag pills via the shared board renderer."
  (let* ((tags (org-air-item-tags item))
         (n (length tags)))
    (if (zerop n) ""
      (org-air-view--item-tagstr tags (min org-air-tags-inline-max n) n))))

(defun org-air-review--origin-cell (item)
  "Return ITEM's `▤ file' origin cell text (the F1 column idiom)."
  (let ((text (file-name-nondirectory (or (org-air-item-file item) "")))
        (budget (max 1 (- org-air-origin-max-width 2))))
    (concat (org-air-view--svg-file-icon (org-air-view--glyph 'origin))
            " "
            (if (<= (string-width text) budget)
                text
              (truncate-string-to-width text budget nil nil
                                        (org-air-view--glyph 'more))))))

(defun org-air-review--item-lines (rows)
  "Return `item' display specs for the per-item ROWS ((ITEM EPOCH …) …)."
  (mapcar (lambda (row)
            (list 'item (nth 0 row)
                  (org-air-review--row-date-text row)
                  (org-air-review--item-tagstr (nth 0 row))
                  (org-air-review--origin-cell (nth 0 row))))
          rows))

(defun org-air-review--compose-sections (data basis p0 p1)
  "Return the render-ready section table from DATA under BASIS.
P0/P1 bound the period for the Time rollup.  Each entry is
\(SECTION TITLE COUNT LINES); LINES are display specs — (group LABEL),
\(item ITEM DATE TAGS ORIGIN), (agg LABEL TEXT) or (note TEXT) — every
cell a pure slot/string derivation (data-pure)."
  (let* ((completed (org-air-review--sort-rows (plist-get data :completed)))
         (cgroups (org-air-review--group-rows completed basis))
         (clines (let (lines)
                   (pcase-dolist (`(,_key ,label . ,rows) cgroups)
                     (push (list 'group label) lines)
                     (dolist (line (org-air-review--item-lines rows))
                       (push line lines)))
                   (nreverse lines)))
         (rollup (org-air-review--time-rollup (plist-get data :time-items)
                                              basis p0 p1))
         (tlines
          (append
           (mapcar (lambda (agg)
                     (list 'agg (nth 0 agg)
                           (format "%s · %d item%s"
                                   (org-air-review--hhmm (nth 1 agg))
                                   (nth 2 agg)
                                   (if (= (nth 2 agg) 1) "" "s"))))
                   rollup)
           (let ((n (plist-get data :suspect-count)))
             (when (> n 0)
               (list (list 'note
                           (format "⚠ %d suspect clock%s, %s excluded — %s"
                                   n (if (= n 1) "" "s")
                                   (org-air-review--hhmm
                                    (plist-get data :suspect-secs))
                                   (mapconcat #'org-air-item-title
                                              (plist-get data :suspect-items)
                                              ", "))))))
           (let ((n (plist-get data :trunc)))
             (when (> n 0)
               (list (list 'note
                           (format "%s on %d heading%s"
                                   org-air-review--trunc-marker
                                   n (if (= n 1) "" "s"))))))))
         (started (org-air-review--sort-rows (plist-get data :started)))
         (carried (org-air-review--sort-rows (plist-get data :carried))))
    (list (list 'completed "Completed" (length completed) clines)
          (list 'time "Time invested" (length (plist-get data :time-items))
                tlines)
          (list 'started "Started" (length started)
                (org-air-review--item-lines started))
          (list 'carried "Carried over" (length carried)
                (org-air-review--item-lines carried)))))

(defun org-air-review--insert-section-heading (section title count)
  "Insert SECTION's heading row: TITLE + COUNT chip (the board idiom).
The whole line carries `org-air-section' SECTION so the TAB fold and
the shared section-motion commands work through the property
machinery."
  (let ((start (point)))
    (insert (org-air-view--item-margin)
            (propertize title 'face 'org-air-face-section)
            " "
            (propertize (format "%d" count) 'face 'org-air-face-count)
            "\n")
    (add-text-properties start (point) (list 'org-air-section section))))

(defun org-air-review--insert-body (sections collapsed scanning width)
  "Insert the four review SECTIONS at WIDTH — the bounded left pane.
COLLAPSED lists folded section symbols (header only); SCANNING prefixes
the cold fill's progress note.  Rows go through the shared V6
`org-air-view--insert-row' primitive carrying the SHARED `org-air-item'
/ `org-air-marker' text properties, so RET/S-RET, the pane,
`org-air-view--find-property' and the bookmark consume chain work
verbatim.  Fixed cluster widths are fitted over the displayed rows only
\(the R17 title-protected fit)."
  (when scanning
    (insert (org-air-view--item-margin)
            (propertize "Scanning your files…" 'face 'org-air-face-empty)
            "\n\n"))
  (let ((dw 0) (tw 0) (ow 0))
    (dolist (section sections)
      (unless (memq (nth 0 section) collapsed)
        (dolist (line (nth 3 section))
          (pcase line
            (`(item ,_item ,date ,tags ,origin)
             (setq dw (max dw (string-width date))
                   tw (max tw (string-width tags))
                   ow (max ow (string-width origin))))
            (`(agg ,_label ,text)
             (setq dw (max dw (string-width text))))
            (_ nil)))))
    (setq ow (min ow org-air-origin-max-width))
    (let* ((gap 2)
           (left-reserve (string-width (org-air-view--item-margin)))
           (cluster (lambda (o)
                      (let ((cells (delq nil (list (and (> dw 0) dw)
                                                   (and (> tw 0) tw)
                                                   (and (> o 0) o)))))
                        (+ (apply #'+ (or cells '(0)))
                           (max 0 (1- (length cells)))))))
           (budget (lambda (o)
                     (- width left-reserve gap (funcall cluster o)))))
      (while (and (> ow org-air-origin-min)
                  (< (funcall budget ow) org-air-title-min-width))
        (setq ow (1- ow)))
      (let ((tw-floor (if (> tw 0) 1 0)))
        (while (and (> tw tw-floor)
                    (< (funcall budget ow) org-air-title-min-width))
          (setq tw (1- tw)))))
    (let ((first t))
      (dolist (section sections)
        (pcase-let ((`(,sym ,title ,count ,lines) section))
          (if first (setq first nil) (insert "\n"))
          (org-air-review--insert-section-heading sym title count)
          (unless (memq sym collapsed)
            (if (null lines)
                (insert (org-air-view--item-margin)
                        (propertize "none" 'face 'org-air-face-empty)
                        "\n")
              (dolist (line lines)
                (pcase line
                  (`(group ,label)
                   (insert (org-air-view--item-margin)
                           (propertize label 'face 'org-air-face-faded)
                           "\n"))
                  (`(item ,item ,date ,tags ,origin)
                   (org-air-view--insert-row
                    :prefix (org-air-view--item-margin)
                    :title (org-air-item-title item)
                    :date-text (propertize date 'face 'org-air-face-date)
                    :tags tags
                    :origin-text origin
                    :origin-face 'org-air-face-origin
                    :widths (list dw tw ow)
                    ;; Review composes its OWN cluster field (own fit) —
                    ;; the documented project-style exception (R40-2).
                    :own-fence t
                    :props (list 'org-air-item item
                                 'org-air-marker (org-air-item-marker item)
                                 'mouse-face 'org-air-face-cursor)))
                  (`(agg ,label ,text)
                   (org-air-view--insert-row
                    :prefix (org-air-view--item-margin)
                    :title label
                    :date-text (propertize text 'face 'org-air-face-date)
                    :tags ""
                    :origin-text ""
                    :widths (list dw tw ow)
                    :own-fence t))
                  (`(note ,text)
                   (insert (truncate-string-to-width
                            (concat (org-air-view--item-margin)
                                    (propertize text
                                                'face 'org-air-face-faded))
                            (max 1 (1- width)) nil nil
                            (org-air-view--glyph 'more))
                           "\n")))))))))))

;;;; ---------------------------------------------------------------------
;;;; Rail (the standard descriptor seam — parameterise, never fork)
;;;; ---------------------------------------------------------------------

(defun org-air-review--calendar-marks (data p0 p1)
  "Return the date-key → mark table for the period [P0, P1) (R61-5).
Every day of the period marks `period' (space glyph, the period face on
the day number); a period day carrying ≥1 completion from DATA marks
`period-done' (the quiet dot in the same face).  Table-driven: two
entries in `org-air-calendar--mark', no renderer surgery."
  (let ((table (make-hash-table :test #'equal)))
    (pcase-dolist (`(,d0 . ,_d1) (org-air-review--day-bounds p0 p1))
      (puthash (org-air-calendar--time-key d0) 'period table))
    (dolist (row (plist-get data :completed))
      (dolist (stamp (nth 3 row))
        (puthash (org-air-calendar--time-key stamp) 'period-done table)))
    table))

(defun org-air-review--insert-summary (data width)
  "Insert the Review rail Summary from DATA fitted to WIDTH (R61-5).
The four section counts, the clocked total and the top three tags by
clipped time — the \"which part needs more\" glance — in the revisit
Summary's row idiom."
  (org-air-view--rail-header "Summary" width)
  (let ((inset (org-air-view--rail-inset-str width)))
    (pcase-dolist (`(,label . ,count)
                   (list (cons "completed"
                               (length (plist-get data :completed)))
                         (cons "clocked"
                               (length (plist-get data :time-items)))
                         (cons "started" (length (plist-get data :started)))
                         (cons "carried"
                               (length (plist-get data :carried)))))
      (insert inset
              (propertize (format "%3d" count)
                          'face (if (zerop count) 'org-air-face-faded
                                  'org-air-face-summary-number))
              "   "
              (propertize label 'face 'org-air-face-summary-label)
              "\n"))
    (insert inset
            (propertize (make-string
                         4 (string-to-char (org-air-view--glyph 'hrule)))
                        'face 'org-air-face-pane-border)
            "\n")
    (insert inset
            (propertize (format "%6s"
                                (org-air-review--hhmm
                                 (plist-get data :time-total)))
                        'face 'org-air-face-summary-number)
            " "
            (propertize "clocked" 'face 'org-air-face-summary-label)
            "\n")
    (when-let* ((top (seq-take (plist-get data :top-tags) 3)))
      (insert "\n")
      (pcase-dolist (`(,label ,secs ,_count) top)
        (insert inset
                (propertize (format "%6s" (org-air-review--hhmm secs))
                            'face 'org-air-face-summary-number)
                " "
                (propertize label 'face 'org-air-face-summary-label)
                "\n")))))

(defconst org-air-review--actions-table
  '((("RET" . "open")    ("<" . "prev")    (">" . "next"))
    (("m" . "span")      ("+" . "widen")   ("-" . "narrow"))
    (("f" . "rollup")    ("/" . "filter")  ("g" . "refresh"))
    (("." . "today")     ("?" . "help")    ("q" . "quit")))
  "Review rail Actions legend: four rows of (KEY . VERB) cells (R62-3).
Every KEY must resolve to a real command in `org-air-review-mode-map'
\(the round-26 legend-truth discipline — the compound \"+/-\" cell
shape was rejected for exactly this reason; `=' stays a legend-less
alias).  The renderer is row-count-generic.")

(defun org-air-review--insert-actions (width)
  "Insert the Review rail Actions block fitted to content WIDTH.
Same shape/keycap idiom as the board and revisit Actions blocks."
  (org-air-view--rail-header "Actions" width)
  (let* ((inset (org-air-view--rail-inset-str width))
         (rows org-air-review--actions-table)
         (cellw (lambda (cell) (+ (length (car cell)) 1 (length (cdr cell)))))
         (c1 (apply #'max (mapcar (lambda (r) (funcall cellw (nth 0 r))) rows)))
         (c2 (apply #'max (mapcar (lambda (r) (funcall cellw (nth 1 r))) rows)))
         (gap (if (>= width 38) "    " " ")))
    (dolist (row rows)
      (insert (org-air-view--pad-to
               (concat inset
                       (org-air-view--verb-cell
                        (car (nth 0 row)) (cdr (nth 0 row)) c1)
                       gap
                       (org-air-view--verb-cell
                        (car (nth 1 row)) (cdr (nth 1 row)) c2)
                       gap
                       (org-air-view--verb-cell
                        (car (nth 2 row)) (cdr (nth 2 row)) 0))
               width)
              "\n"))))

(defun org-air-review--calendar-month (p0 p1)
  "Return the TIME the rail calendar centres on for the period [P0, P1).
R62-2 refinement: TODAY's month when today falls inside the period — a
current YEAR must not stare at January in July (the nil-anchor default
surface tracks today, so its calendar should too); else P0's month, so
`<'/`>' on a past period page by its start month as before."
  (let ((now (floor (float-time))))
    (seconds-to-time (if (and (>= now p0) (< now p1)) now p0))))

(defun org-air-review--rail-descriptor (data p0 p1)
  "Return the Review rail descriptor for DATA over [P0, P1) (R20-5 seam).
The calendar centres on the period's month — today's month when the
period contains today (R62-2) — with the period highlighted (the
precomputed MARKS table); the Summary reads the same DATA fold."
  (let ((marks (org-air-review--calendar-marks data p0 p1))
        (month (org-air-review--calendar-month p0 p1)))
    (list :visible-fn #'identity
          :calendar-fn
          (lambda (entries w inset)
            (org-air-calendar-insert-month month entries w inset marks))
          :summary-fn
          (lambda (_entries w)
            (org-air-review--insert-summary data w))
          :first-thing-fn (lambda (_entries) nil)
          :actions-fn #'org-air-review--insert-actions)))

;;;; ---------------------------------------------------------------------
;;;; Render
;;;; ---------------------------------------------------------------------

(defun org-air-review--render-width ()
  "Return the width to render the Review view at.
An integer `org-air-view-width' is the batch/golden seam, exactly as
the board reads it; else the live window body; else 80."
  (or (and (integerp org-air-view-width) org-air-view-width)
      (and (get-buffer-window (current-buffer) t)
           (org-air-layout-current-width (current-buffer)))
      80))

(defun org-air-review--host-width ()
  "Return the compose width, rail-geometry aware (the R27-2 discipline)."
  (if (and (not noninteractive)
           (not (integerp org-air-view-width))
           (org-air-rail--popped-p)
           (not org-air-view--rail-suspended))
      (org-air-rail--host-width (current-buffer)
                                (org-air-review--render-width))
    (org-air-review--render-width)))

(defun org-air-review--sort-indicator ()
  "Return the shared `↕ key dir' header badge (R22-3 core)."
  (let ((key (or org-air-view--sort-key 'date))
        (dir (or org-air-view--sort-direction 'ascending)))
    (org-air-view--sort-indicator-text
     key dir (not (and (eq key 'date) (eq dir 'ascending))))))

(defun org-air-review--period-short-label (kind p0)
  "Return the short period name for KIND starting at P0 (\"W30 2026\").
R62-2 shapes: week \"W30 2026\", fortnight \"W30–31 2026\" (a pair
straddling an ISO year qualifies both: \"W52 2025–W1 2026\"), month
\"July 2026\", quarter \"Q3 2026\", year \"2026\".  The fortnight's
week numbers come from `org-air-review--iso-week' on P0 and on the
second week's Monday (calendar arithmetic, never P0-week + 1), so
W52/W53 pairings label correctly by construction."
  (pcase kind
    ('month (format-time-string "%B %Y" p0))
    ('year (format-time-string "%Y" p0))
    ('quarter
     (let ((d (decode-time p0)))
       (format "Q%d %d" (1+ (/ (1- (decoded-time-month d)) 3))
               (decoded-time-year d))))
    ('fortnight
     (let* ((d (decode-time p0))
            (abs (calendar-absolute-from-gregorian
                  (list (decoded-time-month d) (decoded-time-day d)
                        (decoded-time-year d))))
            (w1 (org-air-review--iso-week p0))
            (w2 (org-air-review--iso-week
                 (org-air-review--date-epoch
                  (calendar-gregorian-from-absolute (+ abs 7))))))
       (if (= (cdr w1) (cdr w2))
           (format "W%d–%d %d" (car w1) (car w2) (cdr w1))
         (format "W%d %d–W%d %d" (car w1) (cdr w1) (car w2) (cdr w2)))))
    (_ (let ((iso (org-air-review--iso-week p0)))
         (format "W%d %d" (car iso) (cdr iso))))))

(defun org-air-review--period-label (kind p0 p1)
  "Return the full header period label for KIND over [P0, P1).
R62-2: month and year read as their short label; quarter adds the month
range (\"Q3 2026 · Jul – Sep\"); week and fortnight add the day range
\(\"W30–31 2026 · Jul 20 – Aug 2\")."
  (pcase kind
    ((or 'month 'year) (org-air-review--period-short-label kind p0))
    ('quarter
     (format "%s%s%s – %s"
             (org-air-review--period-short-label kind p0)
             (org-air-view--sep)
             (format-time-string "%b" p0)
             (format-time-string "%b" (1- p1))))
    (_ (format "%s%s%s – %s"
               (org-air-review--period-short-label kind p0)
               (org-air-view--sep)
               (format-time-string "%b %-d" p0)
               (format-time-string "%b %-d" (1- p1))))))

(defun org-air-review--header-line (width label done total)
  "Return the Review header for WIDTH: LABEL · DONE done · TOTAL clocked."
  (let* ((title (concat
                 (propertize (concat "  org-air" (org-air-view--sep)
                                     "review" (org-air-view--sep)
                                     label)
                             'face 'org-air-face-title)
                 (propertize (format "%s%d done%s%s" (org-air-view--sep)
                                     done (org-air-view--sep)
                                     (org-air-review--hhmm total))
                             'face 'org-air-face-faded)))
         (badge (org-air-review--sort-indicator))
         (pad (max 1 (- width (string-width title) (string-width badge) 2))))
    (concat title (make-string pad ?\s) badge)))

(defun org-air-review--two-pane-body (entries left-fn width)
  "Return the composed rows-pane | rail body lines for ENTRIES (R61-4).
LEFT-FN inserts the section pane at the width it is given; the rail
\(fed ENTRIES through the descriptor) is sized to one windowful of the
total WIDTH (the R49-4 rule), inspector-free."
  (let* ((rail-width (org-air-view--rail-width width))
         (divider (org-air-view--divider))
         (item-width (max 20 (- width rail-width (string-width divider))))
         (row-lines (org-air-view--render-lines
                     item-width
                     (lambda () (funcall left-fn item-width))))
         (target-h (max 1 (- (org-air-view--render-height) 3)))
         (rail-lines
          (let ((org-air-view--rail-descriptor
                 (plist-put (copy-sequence org-air-view--rail-descriptor)
                            :rail-target-height target-h))
                (org-air-show-inspector nil))
            (mapcar
             (lambda (l) (org-air-view--pad-to l rail-width))
             (org-air-view--render-lines
              rail-width
              (lambda ()
                (org-air-view--insert-rail entries rail-width)))))))
    (org-air-view--compose-columns
     (list (cons row-lines item-width) (cons rail-lines rail-width))
     divider)))

(defun org-air-review--goto-first-row ()
  "Place point on the first item row's title, if any."
  (goto-char (or (text-property-not-all (point-min) (point-max)
                                        'org-air-item nil)
                 (point-min)))
  (org-air-view--goto-row-title))

(defun org-air-review--render ()
  "Render the Review view into the current buffer (R61-4).
DATA-PURE: every section row and every rollup number is a fold over
cached item slots — the render path opens no file, ever; period
navigation and the rollup/threshold knobs repaint without touching
`org-air-query--scan-file' at all.  Rail placement, popped side-window
lifecycle and the foreign-rail sweep mirror the revisit view (one
machinery, parameterised)."
  (when (and (not noninteractive)
             (eq org-air-view--rail-popped-out 'unset))
    (setq-local org-air-view--rail-popped-out
                (eq (org-air-rail--placement 'review) 'side-window)))
  (let* ((inhibit-read-only t)
         (org-air-rail--reconciling t)
         (width (org-air-review--host-width))
         (org-air-view-width width)
         (dims (org-air-view--char-dimensions))
         (org-air-view--pill-char-w (car dims))
         (org-air-view--pill-char-h (cdr dims))
         (kind org-air-review--period-kind)
         (bounds (org-air-review--bounds))
         (p0 (car bounds))
         (p1 (cdr bounds))
         (now (floor (float-time)))
         (currentp (and (>= now p0) (< now p1)))
         (visible (org-air-review--visible-items))
         (data (org-air-review--section-data visible p0 p1 currentp))
         (data (plist-put data :top-tags
                          (org-air-review--time-rollup
                           (plist-get data :time-items) 'tag p0 p1)))
         (sections (org-air-review--compose-sections
                    data org-air-review--rollup p0 p1))
         (collapsed org-air-review--collapsed)
         (scanning (and (null org-air-review--items)
                        org-air-review--fill-queue t))
         (done-count (length (plist-get data :completed)))
         (label (org-air-review--period-label kind p0 p1))
         (left-fn (lambda (w)
                    (org-air-review--insert-body sections collapsed
                                                 scanning w))))
    (setq-local org-air-review--count done-count)
    ;; The rail back-pointer: a popped-out side rail renders THESE items
    ;; through the descriptor (the R22-5 shared primitive); the calendar
    ;; follows the period's month.
    (setq-local org-air-view--items org-air-review--items)
    (setq-local org-air-view--cal-month
                (org-air-review--calendar-month p0 p1))
    (setq-local org-air-view--rail-descriptor
                (org-air-review--rail-descriptor data p0 p1))
    (setq-local org-air-view--inspector-region-height nil)
    (erase-buffer)
    (setq org-air-view--orientation
          (cond
           ((org-air-rail--popped-p) 'side-window)
           ((not (org-air-view--board-only-p width)) 'two-pane)
           (t 'board-only)))
    (insert (org-air-review--header-line width label done-count
                                         (plist-get data :time-total))
            "\n\n")
    (if (eq org-air-view--orientation 'two-pane)
        (org-air-view--insert-lines
         (org-air-review--two-pane-body visible left-fn width))
      (funcall left-fn width))
    (goto-char (point-max))
    (when (and (bolp) (> (point-max) (point-min))) (delete-char -1))
    (goto-char (point-min))
    (org-air-review--goto-first-row)
    ;; R61-6: an armed bookmark locator owns the landing; it stays armed
    ;; while the paced cold fill is still running and clears on match or
    ;; fill-idle.
    (org-air-review--bookmark-consume)
    (setq org-air-review--rendered-width width)
    (cond
     ((eq org-air-view--orientation 'side-window)
      (org-air-rail--show (current-buffer) width))
     ((eq org-air-view--orientation 'board-only)
      ;; R58: an undisplayed (bookmark-restored) review view must not
      ;; delete the displayed layout's windows.
      (unless (org-air-rail--undisplayed-host-p (current-buffer))
        (org-air-rail--hide (current-buffer)))))
    (org-air-rail--evict-foreign-rail (current-buffer))))

(defun org-air-review--render-current ()
  "Re-render the current Review buffer (the shared dispatch target)."
  (when (derived-mode-p 'org-air-review-mode)
    (org-air-review--render)))

(defun org-air-review--resize-refresh ()
  "Re-render when the displaying window's width changed (the C1 path)."
  (let ((width (org-air-review--host-width)))
    (unless (eql width org-air-review--rendered-width)
      (org-air-review--render-current))))

;;;; ---------------------------------------------------------------------
;;;; Commands
;;;; ---------------------------------------------------------------------

(defun org-air-review-period-prev ()
  "Show the previous period (key `<').
One period back: the anchor normalises to the previous period's START
epoch.  A pure repaint over cached data — NEVER a rescan (R61-3)."
  (interactive)
  (setq-local org-air-review--period-anchor
              (car (org-air-review--period-bounds
                    org-air-review--period-kind
                    (1- (car (org-air-review--bounds))))))
  (org-air-review--render-current))

(defun org-air-review-period-next ()
  "Show the next period (key `>').
The half-open END epoch IS the next period's start — exact by
construction.  A pure repaint over cached data — NEVER a rescan."
  (interactive)
  (setq-local org-air-review--period-anchor
              (cdr (org-air-review--bounds)))
  (org-air-review--render-current))

(defun org-air-review-period-today ()
  "Return to the CURRENT period (key `.').
Resets the anchor to nil — the default surface tracks today across
midnight for free."
  (interactive)
  (setq-local org-air-review--period-anchor nil)
  (org-air-review--render-current))

(defun org-air-review--set-range (kind)
  "Adopt range KIND and repaint, preserving the anchor (R62-3).
The R61 anchor-day rule, uniform across all five rungs: the shown
period becomes KIND's period CONTAINING the anchor; a nil anchor stays
nil (every rung tracks the current period by default).  A pure repaint
over cached data — NEVER a rescan (the R61 law)."
  (setq-local org-air-review--period-kind kind)
  (org-air-review--render-current)
  (message "org-air review: by %s" kind))

(defun org-air-review-range-widen ()
  "Widen the range one rung along the ladder (key `+', alias `=').
week → fortnight → month → quarter → year over the effective
`org-air-review-ranges' ladder (R62-3); CLAMPED at the widest rung with
a bounded message — no wrap, so repeated presses park safely at year."
  (interactive)
  (let* ((kind org-air-review--period-kind)
         (tail (cdr (memq kind (org-air-review--effective-ranges kind)))))
    (if tail
        (org-air-review--set-range (car tail))
      (message "org-air review: widest range (%s)" kind))))

(defun org-air-review-range-narrow ()
  "Narrow the range one rung along the ladder (key `-').
The inverse of `org-air-review-range-widen' (R62-3); CLAMPED at the
narrowest rung with a bounded message — no wrap."
  (interactive)
  (let* ((kind org-air-review--period-kind)
         (ladder (org-air-review--effective-ranges kind))
         (pos (seq-position ladder kind)))
    (if (and pos (> pos 0))
        (org-air-review--set-range (nth (1- pos) ladder))
      (message "org-air review: narrowest range (%s)" kind))))

(defun org-air-review-cycle-range ()
  "Cycle the range ladder with wrap-around (key `m') (R62-3).
week → fortnight → month → quarter → year → week over the effective
`org-air-review-ranges' ladder.  Generalises the R61 week↔month toggle
\(one ladder, three verbs: `+'/`-' directional, `m' rotary) — with the
ladder knob trimmed to (week month) the cycle IS the old toggle."
  (interactive)
  (let* ((kind org-air-review--period-kind)
         (ladder (org-air-review--effective-ranges kind)))
    (org-air-review--set-range (or (cadr (memq kind ladder))
                                   (car ladder)))))

(define-obsolete-function-alias 'org-air-review-toggle-kind
  #'org-air-review-cycle-range "0.1.0"
  "R62-3: the 2-state week↔month toggle generalised to the range ladder.")

(defun org-air-review-cycle-rollup ()
  "Cycle the rollup basis: day → tag → directory → origin (key `f').
One buffer-local lens applied coherently: Completed regroups, Time
invested re-aggregates — a pure repaint, never a rescan (R61-3)."
  (interactive)
  (setq-local org-air-review--rollup
              (pcase org-air-review--rollup
                ('day 'tag)
                ('tag 'directory)
                ('directory 'origin)
                (_ 'day)))
  (org-air-review--render-current)
  (message "org-air review: rollup by %s" org-air-review--rollup))

(defun org-air-review-toggle-section ()
  "TAB: toggle the fold of the review section at point (board idiom).
On any other line, move to the next section heading instead — TAB is
always safe, it never toggles blindly or hangs."
  (interactive)
  (let ((section (org-air-view--line-section)))
    (if (null section)
        (org-air-next-section)
      (setq-local org-air-review--collapsed
                  (if (memq section org-air-review--collapsed)
                      (delq section org-air-review--collapsed)
                    (cons section org-air-review--collapsed)))
      (org-air-review--render-current)
      (let ((pos (org-air-view--find-property 'org-air-section section)))
        (when pos
          (goto-char pos)
          (org-air-view--goto-row-title))))))

(defun org-air-review-filter (tags)
  "Filter the Review view to TAGS (the shared filter core, key `/').
Matches title/tags/origin in memory through the R24-6 `#tag'/free-text
mini-language; filter applies BEFORE the fold, so totals honestly
describe what is shown."
  (interactive
   (list (org-air-view--read-filter
          (delete-dups
           (sort (seq-mapcat (lambda (item)
                               (copy-sequence (org-air-item-tags item)))
                             org-air-review--items)
                 #'string<)))))
  (setq org-air-view--tag-filter (unless (null tags) tags))
  (org-air-review--render-current))

(defun org-air-review-scope (scope)
  "Scope the Review view to SCOPE (key `s').
The board's structural lens — all / @group / ⌂ file — over the review
item snapshot (`org-air-view--passes-scope-p' does the matching); scope
applies BEFORE the fold."
  (interactive
   (let* ((items org-air-review--items)
          (groups (delete-dups (delq nil (mapcar #'org-air-item-group
                                                 items))))
          (files (delete-dups (delq nil (mapcar #'org-air-item-file items))))
          (candidates
           (append '("all")
                   (mapcar (lambda (g) (concat "@" g)) groups)
                   (mapcar (lambda (file)
                             (concat "⌂ " (file-name-nondirectory file)))
                           files))))
     (list (completing-read "Scope: " candidates nil t))))
  (setq org-air-view--scope
        (cond
         ((or (null scope) (equal scope "all")) nil)
         ((string-prefix-p "@" scope) (list :group (substring scope 1)))
         ((string-prefix-p "⌂ " scope)
          (let ((name (substring scope 2)))
            (list :file (seq-find
                         (lambda (file)
                           (equal name (file-name-nondirectory file)))
                         (delete-dups
                          (delq nil (mapcar #'org-air-item-file
                                            org-air-review--items)))))))
         (t nil)))
  (org-air-review--render-current))

(defun org-air-review-scope-clear ()
  "Clear the active Review scope (key `S')."
  (interactive)
  (setq org-air-view--scope nil)
  (org-air-review--render-current))

(defun org-air-review-refresh ()
  "Refresh the Review data and re-render (key `g').
Drops the local snapshot and takes the freshest tier again: NEVER a
synchronous scan interactively (the paced cold-fill machinery rescans
on budgeted wall-clock slices, repainting progressively); in batch
\(deterministic ERT) the scan runs inline."
  (interactive)
  (if noninteractive
      (setq org-air-review--items (org-air-query-items))
    (setq org-air-review--items nil)
    (org-air-review--ensure-data))
  (org-air-review--render-current))

(defun org-air-review-next ()
  "Move point to the next item row, landing on its title."
  (interactive)
  (let ((pos (next-single-property-change (point) 'org-air-item
                                          nil (point-max))))
    (while (and pos (not (get-text-property pos 'org-air-item))
                (< pos (point-max)))
      (setq pos (next-single-property-change pos 'org-air-item
                                             nil (point-max))))
    (when (and pos (get-text-property pos 'org-air-item))
      (goto-char pos)
      (org-air-view--goto-row-title))))

(defun org-air-review-prev ()
  "Move point to the previous item row, landing on its title."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'org-air-item
                                              nil (point-min))))
    (while (and pos
                (not (get-text-property (max (point-min) (1- pos))
                                        'org-air-item))
                (> pos (point-min)))
      (setq pos (previous-single-property-change pos 'org-air-item
                                                 nil (point-min))))
    (when pos
      (goto-char (max (point-min) (1- pos)))
      (org-air-view--goto-row-title))))

(defun org-air-review-quit ()
  "Quit the Review view progressively — one surface per press (R28-2).
A live bottom pane closes first; the next press tears down a popped-out
rail and quits back to the previous view (the shared quit convention)."
  (interactive)
  (unless (org-air-view--quit-close-pane)
    (when (org-air-rail--popped-p)
      (org-air-rail--teardown))
    (quit-window)))

;;;; ---------------------------------------------------------------------
;;;; Help groups (consumed by `org-air-help' via its review context)
;;;; ---------------------------------------------------------------------

(defconst org-air-review--help-groups
  '(("Navigation"
     (org-air-review-next . "next item")
     (org-air-review-prev . "previous item")
     (org-air-review-toggle-section . "toggle section fold")
     (org-air-visit-item . "visit item, other window"))
    ("Period"
     (org-air-review-period-prev . "previous period")
     (org-air-review-period-next . "next period")
     (org-air-review-period-today . "current period")
     (org-air-review-cycle-range . "cycle range (week/2w/month/quarter/year)")
     (org-air-review-range-widen . "widen range")
     (org-air-review-range-narrow . "narrow range"))
    ("Display"
     (org-air-review-cycle-rollup . "cycle rollup (day/tag/dir/origin)")
     (org-air-view-sort-cycle . "cycle sort key (date/title)")
     (org-air-view-sort-reverse . "reverse sort"))
    ("Filter"
     (org-air-review-filter . "filter by tags/text (live)")
     (org-air-filter-clear . "clear filter")
     (org-air-filter-toggle-match . "toggle AND/OR combinator")
     (org-air-review-scope . "source lens (file/group/all)")
     (org-air-review-scope-clear . "clear source lens"))
    ("Rail"
     (org-air-rail-toggle . "pop rail out/in"))
    ("Refresh"
     (org-air-review-refresh . "refresh"))
    ("Session"
     (org-air-project . "project tree")
     (org-air-revisit . "revisit dusty notes")
     (org-air-review-quit . "quit")
     (org-air-help . "this help")))
  "REVIEW help groups: (TITLE . ((COMMAND . DESCRIPTION) …)) (R50-2).
Key text is NEVER stored here — it derives at render time from the
live keymaps (the legend-truth discipline).")

;;;; ---------------------------------------------------------------------
;;;; Keymaps + mode
;;;; ---------------------------------------------------------------------

(defvar org-air-review-mode-map
  (let ((map (make-sparse-keymap)))
    ;; A THIN child of the shared view-core map (R18 D-P3): RET pane,
    ;; o/O sort, `|' rail, `\' clear, M-/ combinator, j/k line motion all
    ;; inherit.  PARENT stays at defvar time — always, even with the knob
    ;; nil (R35-1).
    (set-keymap-parent map org-air-view-core-map)
    map)
  "Keymap for `org-air-review-mode'.
Keys installed by `org-air--install-default-keybindings' (R35-1).")

;; R35-1: the REVIEW default keys (installer-owned).  `<'/`>'/`.' are the
;; board calendar idiom transposed to PERIOD navigation (one unit of the
;; active range); `+'/`=' widen and `-' narrow the R62-2 range ladder,
;; `m' cycles it; `f' cycles the rollup basis; `/' + `s'/`S' reuse the
;; shared filter/scope machinery; S-RET the other-window visit; `P'/`N'
;; the symmetric view switches.
(org-air--register-default-keys 'org-air-review-mode-map
  "n" #'org-air-review-next
  "p" #'org-air-review-prev
  "TAB" #'org-air-review-toggle-section
  "M-n" #'org-air-next-section
  "M-p" #'org-air-prev-section
  "<S-return>" #'org-air-visit-item
  "S-RET" #'org-air-visit-item
  "<" #'org-air-review-period-prev
  ">" #'org-air-review-period-next
  "." #'org-air-review-period-today
  "m" #'org-air-review-cycle-range
  "+" #'org-air-review-range-widen
  "=" #'org-air-review-range-widen
  "-" #'org-air-review-range-narrow
  "f" #'org-air-review-cycle-rollup
  "/" #'org-air-review-filter
  "s" #'org-air-review-scope
  "S" #'org-air-review-scope-clear
  "g" #'org-air-review-refresh
  "P" #'org-air-project
  "N" #'org-air-revisit
  "?" #'org-air-help
  "q" #'org-air-review-quit)

(defvar org-air-review-leader-map
  (make-sparse-keymap)
  "Leader prefix map for the Review content buffer (R30-2).
Installed at `org-air-leader-key' on `org-air-review-mode-map'.
Keys installed by `org-air--install-default-keybindings' (R35-1).")

(org-air--register-default-keys 'org-air-review-leader-map
  "|" #'org-air-rail-toggle
  "o" #'org-air-rail-return
  "s" #'org-air-view-sort-cycle
  "/" #'org-air-review-filter)

(org-air--register-default-leader 'org-air-review-mode-map
                                  'org-air-review-leader-map)

(define-derived-mode org-air-review-mode special-mode "Org-Air-Review"
  "Major mode for the Review (retrospective) view (R61-4)."
  ;; R35-1: reconcile the shared maps on the first review buffer.
  (org-air--sync-default-keybindings)
  (setq-local truncate-lines t)
  (setq-local cursor-type 'box)
  (setq-local line-spacing org-air-line-spacing)
  (org-air-view--install-modeline)
  ;; R61-6: the Review view is bookmarkable — a FULL record: period kind +
  ;; anchor, rollup, filter, scope, sort plus the item-at-point locator.
  ;; Restored by `org-air-review-bookmark-jump'.
  (setq-local bookmark-make-record-function
              #'org-air-review--bookmark-make-record)
  ;; Responsive re-render on resize (the round-9 C1 path).
  (setq-local org-air-layout-refresh-function
              #'org-air-review--resize-refresh)
  ;; R22-3: seed the SHARED sort spec so the inherited o/O drive the
  ;; date/title cycle (default: date ascending — chronological within the
  ;; period).
  (setq-local org-air-view--sort-keys '(date title))
  (setq-local org-air-view--sort-refresh #'org-air-review--render-current)
  (unless org-air-view--sort-key
    (setq-local org-air-view--sort-key 'date))
  (unless org-air-view--sort-direction
    (setq-local org-air-view--sort-direction 'ascending))
  ;; R22-2b/R29-2: point normalization onto row titles; inert in batch.
  (unless noninteractive
    (add-hook 'pre-command-hook #'org-air-view--pre-command-snapshot nil t)
    (add-hook 'post-command-hook #'org-air-view--normalize-point nil t))
  ;; A dying review buffer takes its pacer AND its popped rail with it.
  (add-hook 'kill-buffer-hook #'org-air-review--fill-teardown nil t)
  (add-hook 'kill-buffer-hook #'org-air-rail--teardown nil t)
  ;; R24-5: the shared cooperative rail reconciler; inert under batch.
  (unless noninteractive
    (add-hook 'window-configuration-change-hook
              #'org-air-rail--reconcile nil t))
  ;; R27-4: the shared evil integration (motion state + overriding map);
  ;; fboundp-gated soft dep, skipped with the R35-1 knob off.
  (when org-air-use-default-keybindings
    (org-air-view--setup-evil 'org-air-review-mode
                              org-air-review-mode-map))
  (org-air-layout-install-window-size-hook)
  (buffer-disable-undo))

;;;###autoload
(defun org-air-review ()
  "Open the Review (retrospective) view (R61).
Answers \"what happened over this week / month\": items completed, time
clocked (rolled up by tag / directory / origin), items started and work
touched but not finished.  `<'/`>'/`.' navigate periods by ONE unit of
the active range, `+'/`-' widen/narrow the range ladder
\(week/fortnight/month/quarter/year, R62-2), `m' cycles it, `f' cycles
the rollup.  Reached from the board, the project and the revisit views
via `W'; `q' returns to the previous view."
  (interactive)
  (let ((buffer (get-buffer-create org-air-review-buffer-name)))
    (with-current-buffer buffer
      ;; R26-5 idempotent entry: initialise the mode only once — a
      ;; re-entry re-renders in place (session state survives).
      (unless (derived-mode-p 'org-air-review-mode)
        (org-air-review-mode)))
    (pop-to-buffer buffer)
    (org-air-review--open-core buffer t)))

(defun org-air-review--open-core (buffer _display)
  "Run the Review entry's data+render body in BUFFER (R58 factoring).
Prep + `org-air-review--ensure-data' (never-blocking: warm / cache
hydrate / paced cold fill) + render — exactly the command's body; the
command is prep + `pop-to-buffer' + this core.  The bookmark handler
calls it with DISPLAY nil (undisplayed — the restorer owns the
windows).  Ensures the mode idempotently (R26-5); never displays
BUFFER."
  (with-current-buffer buffer
    (unless (derived-mode-p 'org-air-review-mode)
      (org-air-review-mode))
    (org-air-review--ensure-data)
    (org-air-review--render)))

;;;; ---------------------------------------------------------------------
;;;; R61-6 — Emacs bookmark support (see org-air-view.el's shared core).
;;;; ---------------------------------------------------------------------

(defun org-air-review--bookmark-name ()
  "Return the review record's `defaults' candidates (R61-6).
Period-qualified first (\"org-air: review · W30 2026\"), then the
generic \"org-air: review\"."
  (let ((bounds (org-air-review--bounds)))
    (delete-dups
     (list (format "org-air: review · %s"
                   (org-air-review--period-short-label
                    org-air-review--period-kind (car bounds)))
           "org-air: review"))))

(defun org-air-review--bookmark-make-record ()
  "Return the Emacs bookmark record for the Review buffer (R61-6).
A FULL printable record: `org-air-period' — (KIND . ANCHOR) where
ANCHOR is the period-start integer epoch, or the symbol `current' when
the recorded period IS current at record time (RULED: an activities
layout saved on the default view must restore tracking the NEW current
period, not fossilise last Tuesday's; an explicitly navigated period
records its absolute anchor) — plus rollup, filter, scope, sort and the
row-at-point (FILE . POS) locator.  Pure buffer-local reads; never
signals (degrades to the bare header record)."
  (condition-case nil
      (append
       (org-air-view--bookmark-header 'review
                                      'org-air-review-bookmark-jump
                                      "org-air: review"
                                      (org-air-review--bookmark-name))
       (list (cons 'org-air-period
                   (cons org-air-review--period-kind
                         (if (null org-air-review--period-anchor)
                             'current
                           (car (org-air-review--bounds)))))
             (cons 'org-air-rollup org-air-review--rollup)
             (cons 'org-air-sort
                   (cons (or org-air-view--sort-key 'date)
                         (or org-air-view--sort-direction 'ascending))))
       (and org-air-view--tag-filter
            (list (cons 'org-air-filter org-air-view--tag-filter)))
       (and (consp org-air-view--scope)
            (list (cons 'org-air-scope org-air-view--scope)))
       (org-air-view--bookmark-item-fields))
    (error (org-air-view--bookmark-header 'review
                                          'org-air-review-bookmark-jump
                                          "org-air: review"
                                          (list "org-air: review")))))

(defun org-air-review--bookmark-apply (record)
  "Apply RECORD's org-air fields to the current Review buffer (R61-6).
The review twin of `org-air-view--bookmark-apply': every field
optional, unknown fields ignored, malformed values dropped
\(forward-compatible best-effort).  A `current' (or absent) period
anchor restores tracking the LIVE current period."
  (let ((period (cdr (assq 'org-air-period record)))
        (rollup (cdr (assq 'org-air-rollup record)))
        (filter (cdr (assq 'org-air-filter record)))
        (scope (cdr (assq 'org-air-scope record)))
        (sort (cdr (assq 'org-air-sort record))))
    ;; R62-3: the KIND domain widened apply-side to the five ranges —
    ;; deliberately NOT gated on `org-air-review-ranges' (a record must
    ;; restore on a machine whose ladder was trimmed; the knob governs
    ;; keys, not state validity).  An unknown kind (\='decade) still
    ;; degrades to the default current week, no signal.
    (when (and (consp period)
               (memq (car period) org-air-review--range-ladder))
      (setq-local org-air-review--period-kind (car period))
      (setq-local org-air-review--period-anchor
                  (and (integerp (cdr period)) (cdr period))))
    (when (memq rollup '(day tag directory origin))
      (setq-local org-air-review--rollup rollup))
    (when (or (stringp filter) (consp filter))
      (setq org-air-view--tag-filter filter))
    (when (consp scope)
      (setq-local org-air-view--scope scope))
    (when (and (consp sort)
               (car sort) (symbolp (car sort))
               (cdr sort) (symbolp (cdr sort)))
      (setq-local org-air-view--sort-key (car sort)
                  org-air-view--sort-direction (cdr sort)))))

(defun org-air-review--bookmark-consume ()
  "Land point on the bookmarked item row; never signals (R61-6).
The drift chain on the shared row properties: exact (FILE . POS) via
`org-air-marker', then the item title.  With the paced cold fill still
in flight a miss stays ARMED for the next progressive paint; otherwise
the slot clears and the render's first-row landing stands."
  (when org-air-review--bookmark-locator
    (condition-case nil
        (let* ((slot org-air-review--bookmark-locator)
               (loc (plist-get slot :item))
               (title (plist-get slot :title))
               (pos (or (and loc (org-air-view--find-property
                                  'org-air-marker loc))
                        (and title
                             (org-air-view--bookmark-scan
                              'org-air-item
                              (lambda (it)
                                (and (org-air-item-p it)
                                     (equal (org-air-item-title it)
                                            title))))))))
          (cond
           (pos
            (setq org-air-review--bookmark-locator nil)
            (goto-char pos)
            (org-air-view--goto-row-title))
           ;; Cold fill still running: the row may simply not be painted
           ;; yet — stay armed for the next progressive paint.
           ((or org-air-review--fill-queue
                (timerp org-air-review--fill-timer)))
           (t (setq org-air-review--bookmark-locator nil))))
      (error (setq org-air-review--bookmark-locator nil)))))

;;;###autoload
(defun org-air-review-bookmark-jump (record)
  "Handler for org-air Review bookmarks (R61-6).
Rebuilds `*org-air review*' from RECORD without displaying it (the
bookmark caller owns display — the activities.el contract) through the
existing never-blocking data path (warm / cache hydrate / paced cold
fill — cache-first, no window touching).  Never signals: a malformed
RECORD degrades to a plain Review open."
  (require 'org-air)
  (let ((buffer (get-buffer-create org-air-review-buffer-name)))
    (condition-case err
        (with-current-buffer buffer
          ;; R26-5 idempotent entry guard — identical to the command's.
          (unless (derived-mode-p 'org-air-review-mode)
            (org-air-review-mode))
          (org-air-review--bookmark-apply record)
          (setq org-air-review--bookmark-locator
                (org-air-view--bookmark-locator-of record))
          (org-air-review--open-core buffer nil))
      (error
       (message "org-air: bookmark restore degraded: %s"
                (org-air-view--short-error err))
       (with-current-buffer buffer
         (unless (derived-mode-p 'org-air-review-mode)
           (org-air-review-mode))
         (ignore-errors (org-air-review--open-core buffer nil)))))
    ;; The handler contract: make the target buffer CURRENT, never shown.
    (set-buffer buffer)))
;;;###autoload
(put 'org-air-review-bookmark-jump 'bookmark-handler-type "org-air")

;; R35-1: this file loads AFTER the load-time seed at the bottom of
;; org-air-project.el, so the review key registrations above missed that
;; sync.  Re-install once (idempotent) iff the defaults are currently ON,
;; so the review maps are populated under the default while a knob-off
;; setup stays bare.
(when (eq org-air--default-keybindings-state t)
  (org-air--install-default-keybindings))

(provide 'org-air-review)

;; Local Variables:
;; package-lint-main-file: "org-air.el"
;; End:
;;; org-air-review.el ends here
