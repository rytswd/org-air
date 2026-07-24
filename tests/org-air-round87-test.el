;;; org-air-round87-test.el --- executing ERTs for v0.1 round-87 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.1 round-87 — the TODAY / TOMORROW standout now
;; REACHES the due-date column (air/v0.1/org-air-round87-design.org).
;;
;; R85 shipped two standing-out faces — `org-air-face-day-today' (bold
;; teal) and `org-air-face-day-tomorrow' (bold rose) — but applied them
;; under *rule (B) NEUTRAL-ONLY*: `org-air-view--day-relative-face' was
;; wired into ONLY the neutral `notes' arm of `org-air-view--date-label',
;; so a DEADLINE / SCHEDULED that falls today/tomorrow KEPT its slot face
;; (blue/orange) and never got the standout — the user's "still in blue".
;;
;; R87 revises the ruling to *rule (A): TODAY / TOMORROW WINS*.  The impl
;; routes the SAME helper into the DEADLINE (org-air-view.el:2076) and
;; SCHEDULED (2077) arms too:
;;
;;   (deadline  (cons (--human-date deadline now)
;;                    (or (--day-relative-face deadline now)
;;                        'org-air-face-deadline)))
;;   (scheduled (cons (--human-date scheduled now)
;;                    (or (--day-relative-face scheduled now)
;;                        'org-air-face-scheduled)))
;;
;; so a delta-0 (today) / delta-1 (tomorrow) date takes the day face on
;; ALL THREE date-emitting arms (deadline + scheduled + neutral) and
;; stands out on every slot.  An OVERDUE past deadline (the two `overdue'
;; arms precede in the cond, and the helper only ever answers delta 0/1)
;; KEEPS `org-air-face-overdue'; a date >=2 days out keeps its slot face.
;;
;; The svg crux (§Investigation): the board date pill has NO separate
;; colour path — `org-air-view--svg-pillify' draws its label with
;; `:fill (face-foreground FACE nil t)', i.e. the pill's colour IS the
;; text face's foreground.  So driving the day face into the deadline /
;; scheduled arms reaches BOTH the plain coloured text AND the svg pill in
;; ONE face-only move.  A face is a `display' property, not buffer text,
;; so the label BYTES ("Today"/"Tomorrow") and every text golden are
;; byte-identical — only the GUI pill PIXELS repaint teal/rose.
;;
;; All BATCH/headless.  The exact teal/rose HEXES are GUI-confirm-only;
;; the seams assert the FACE SYMBOL on the (LABEL . FACE) cons (and, where
;; relevant, the rendered TEXT / the declared face foreground that the
;; pill `:fill' reads), never the pixel.  The clock is frozen to
;; `org-air-test-now' (Mon 2026-06-15): today = 2026-06-15, tomorrow =
;; 2026-06-16.  These seams are the permanent regression guards.
;;
;; r87-1..10 are the design's "ERT seams" ledger (each names what
;; reverting breaks); r87-11..15 are the TEST-round STRENGTHENINGS that
;; close audit gaps beyond the ten:
;;   r87-11 proves the day face reaches a RENDERED cell on the SCHEDULED
;;     arm too (the scheduled twin of r87-5's deadline painter path).
;;   r87-12 pins R53 data-purity: the face is chosen from the item's own
;;     deadline/scheduled slot + the delta already computed in
;;     `--date-label' — NO file access, no rescan (a bogus marker + hard-
;;     errored file openers still resolve the day face).
;;   r87-13 pins the rule is UNIFORM across the three arms: for each of
;;     today/tomorrow the deadline, scheduled AND notes arms all resolve
;;     the SAME day face (no per-slot asymmetry — §Decision point 4).
;;   r87-14 pins the svg crux headlessly: the day face's declared
;;     `:foreground' (which becomes the pill `:fill') DIFFERS from the
;;     slot face's in BOTH the light and dark tiers, for deadline AND
;;     scheduled — so the pill actually repaints (inequality-only; exact
;;     hex GUI-confirm-only, incl. the dark scheduled Nord8-vs-Nord9
;;     adjacency).
;;   r87-15 proves the day face survives the PILL-style painter path too
;;     (`org-air-date-style 'pill'), not just the plain-text one — the
;;     colour source the svg pill reads.
;;
;; R88 RE-BLESS: R88 turns the two-level today/tomorrow highlight into a
;; five-level PROXIMITY HEAT-RAMP (adds a this-week AMBER band
;; `org-air-face-day-week' for delta 2..6; see org-air-round88-test.el).
;; Two seams here are re-blessed to the ramp:
;;   r87-7  "delta>=2 keeps slot face" SPLITS at the week boundary — delta
;;     3 now -> `org-air-face-day-week' (amber), delta 7 (BEYOND, "a week
;;     = 7") still -> its slot face.
;;   r87-10 the delta-3 sub-line of the notes-arm guard now ->
;;     `org-air-face-day-week' (its today/tomorrow half is UNCHANGED).
;; r87-1..6/8..9/11..15 are SYMBOL-only (today->day-today,
;; tomorrow->day-tomorrow, overdue->overdue) and stand verbatim under R88
;; (the day faces are RECOLOURED, not renamed).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-air-test-helpers)

(when (locate-library "org-air")
  (require 'org-air))

(defvar org-air-date-style)
(defvar org-air-view--meta-date-w)

;;;; -------------------------------------------------------------------
;;;; Scaffolding (self-contained; mirrors the R85 builders)
;;;; -------------------------------------------------------------------

(defmacro org-air-r87--frozen (&rest body)
  "Run BODY with `current-time' frozen to `org-air-test-now'."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'current-time)
              (lambda () org-air-test-now)))
     ,@body))

(defun org-air-r87--epoch (days)
  "Return the epoch float DAYS calendar days from the frozen now.
The `activity' slot is stored as an epoch float; DAYS = 0 is today,
1 is tomorrow (relative to `org-air-test-now')."
  (float-time (time-add org-air-test-now (days-to-time days))))

(defun org-air-r87--org-ts (days)
  "Return an Org timestamp object DAYS calendar days from the frozen now.
Built from the frozen now's calendar day so `org-air-view--date-label's
deadline/scheduled arms resolve it as today (DAYS=0) / tomorrow (DAYS=1)
/ a weekday (DAYS>=2) / OVERDUE (DAYS<0) — the exact slot dates R87 rule
(A) recolours (or, for OVERDUE / delta>=2, leaves alone)."
  (org-timestamp-from-string
   (format-time-string "<%Y-%m-%d %a>"
                       (time-add org-air-test-now (days-to-time days)))))

(cl-defun org-air-r87--dated (&key deadline scheduled (file "/tmp/org-air-r87-dated.org"))
  "Build an `org-air-item' carrying a DEADLINE and/or SCHEDULED day offset.
DEADLINE / SCHEDULED are day offsets from the frozen now stored as Org
timestamp objects, so `--date-label' takes its SLOT arm (deadline /
scheduled) — the rule-A non-neutral path.  The marker is a (FILE . POS)
cons the render layer never opens (R53); FILE defaults to a path that
does not exist, so a test may assert the face resolves WITHOUT any file
access."
  (org-air-item-create
   :title "A dated task"
   :file file
   :marker (cons file 1)
   :kind 'heading
   :deadline (and deadline (org-air-r87--org-ts deadline))
   :scheduled (and scheduled (org-air-r87--org-ts scheduled))))

(cl-defun org-air-r87--note (&key activity)
  "Build a dateless NOTE-shape `org-air-item' with ACTIVITY (day offset).
No scheduled/deadline, so `org-air-view--date-label' falls through to the
NEUTRAL `notes' arm.  ACTIVITY is a day offset from the frozen now, stored
as the epoch float the scan would have cached (R53 P3)."
  (org-air-item-create
   :title "A quiet note"
   :file "/tmp/org-air-r87-note.org"
   :marker (cons "/tmp/org-air-r87-note.org" 1)
   :kind 'heading
   :activity (and activity (org-air-r87--epoch activity))))

;;;; -------------------------------------------------------------------
;;;; r87-1 (core, deadline) — a DEADLINE due TODAY carries the day face
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r87-1-deadline-today-carries-day-face ()
  "A deadline that falls TODAY → `(cons \"Today\" 'org-air-face-day-today)'
— NOT `org-air-face-deadline'.  This is the rule-A successor of the
retired r85-11 (which pinned the rule-B \"keeps slot face\").  Reverting
the D1 deadline-arm rewire (the day face falls back to the slot face)
FAILS."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (org-air-r87--frozen
    (should (equal (org-air-view--date-label
                    (org-air-r87--dated :deadline 0) 'notes)
                   (cons "Today" 'org-air-face-day-today)))))

;;;; -------------------------------------------------------------------
;;;; r87-2 (core, deadline) — a DEADLINE due TOMORROW carries the day face
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r87-2-deadline-tomorrow-carries-day-face ()
  "A deadline that falls TOMORROW → `(cons \"Tomorrow\"
'org-air-face-day-tomorrow)' — NOT `org-air-face-deadline'.  The rule-A
successor of the retired r85-5 (its fixture-driven twin is the re-blessed
`org-air-date-future-deadline-is-benign').  Reverting D1 FAILS."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (org-air-r87--frozen
    (should (equal (org-air-view--date-label
                    (org-air-r87--dated :deadline 1) 'notes)
                   (cons "Tomorrow" 'org-air-face-day-tomorrow)))))

;;;; -------------------------------------------------------------------
;;;; r87-3 (core, scheduled) — a SCHEDULED TODAY carries the day face
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r87-3-scheduled-today-carries-day-face ()
  "A scheduled date that falls TODAY → `(cons \"Today\"
'org-air-face-day-today)' — NOT `org-air-face-scheduled'.  The rule-A
successor of the retired r85-12.  Reverting the D1 scheduled-arm rewire
FAILS."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (org-air-r87--frozen
    (should (equal (org-air-view--date-label
                    (org-air-r87--dated :scheduled 0) 'notes)
                   (cons "Today" 'org-air-face-day-today)))))

;;;; -------------------------------------------------------------------
;;;; r87-4 (core, scheduled) — a SCHEDULED TOMORROW carries the day face
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r87-4-scheduled-tomorrow-carries-day-face ()
  "A scheduled date that falls TOMORROW → `(cons \"Tomorrow\"
'org-air-face-day-tomorrow)' — NOT `org-air-face-scheduled'.  The rule-A
successor of the retired r85-6 (fixture twin: the re-blessed
`org-air-date-future-scheduled-is-benign').  Reverting D1 FAILS."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (org-air-r87--frozen
    (should (equal (org-air-view--date-label
                    (org-air-r87--dated :scheduled 1) 'notes)
                   (cons "Tomorrow" 'org-air-face-day-tomorrow)))))

;;;; -------------------------------------------------------------------
;;;; r87-5 (rendered cell, deadline) — the day face reaches the painter
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r87-5-deadline-today-face-through-painter ()
  "The deadline-today date cell, rendered through the real painter
`org-air-view--item-date-text' (`org-air-date-style 'text'), carries the
TEXT \"Today\" and the standout `org-air-face-day-today' face property —
while its label BYTES are byte-identical to the pre-R87 render.

Proven by rendering the SAME cell twice: once normally, once with the R87
helper `org-air-view--day-relative-face' stubbed to nil (the pre-R87 /
reverted rule-B choice, which falls back to `org-air-face-deadline').  The
stripped text is IDENTICAL; only the `face' differs.  This is the plain-
text twin of the svg pill: the pill's `:fill' reads the SAME
`face-foreground FACE', so the pill is teal by the same face (its colour
is GUI-confirm-only; the headless proxy is r87-14).  A change that mutes
the deadline-today cell, or alters its label bytes, FAILS."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--item-date-text)))
  (let ((org-air-date-style 'text)      ; plain coloured text, no svg pill
        (item (org-air-r87--dated :deadline 0)))
    (org-air-r87--frozen
      (let* ((r87 (org-air-view--item-date-text item 'notes))
             (pre (cl-letf (((symbol-function 'org-air-view--day-relative-face)
                             (lambda (&rest _) nil)))
                    (org-air-view--item-date-text item 'notes))))
        ;; both render the SAME visible label bytes.
        (should (equal (substring-no-properties r87) "Today"))
        (should (equal (substring-no-properties r87)
                       (substring-no-properties pre)))
        ;; but the FACE differs — R87 paints the standout day face where
        ;; the reverted rule-B painted the slot deadline face.
        (should (eq (get-text-property 0 'face r87) 'org-air-face-day-today))
        (should (eq (get-text-property 0 'face pre) 'org-air-face-deadline))))))

;;;; -------------------------------------------------------------------
;;;; r87-6 (boundary) — an OVERDUE past deadline KEEPS the overdue face
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r87-6-overdue-keeps-overdue-face ()
  "A deadline 5 days in the PAST → `(cons \"OVERDUE 5d\"
'org-air-face-overdue)' — NOT any day face.  The two `overdue' arms
precede the deadline/scheduled arms in the cond and the helper only ever
answers delta 0/1, so a past date never reaches the day-face path.
Leaking a day face onto the overdue arm (delta<0) FAILS."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (org-air-r87--frozen
    (pcase-let ((`(,label . ,face)
                 (org-air-view--date-label
                  (org-air-r87--dated :deadline -5) 'notes)))
      (should (equal label "OVERDUE 5d"))
      (should (eq face 'org-air-face-overdue))
      (should-not (memq face '(org-air-face-day-today
                               org-air-face-day-tomorrow))))))

;;;; -------------------------------------------------------------------
;;;; r87-7 (boundary) — a deadline / scheduled >=2 days out is UNCHANGED
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r87-7-delta-ge2-keeps-slot-face ()
  "R88 RE-BLESS / SPLIT: R88's ramp inserts a this-week band, so the old
\"delta>=2 keeps its slot face\" no longer holds across the board — it
SPLITS at the week boundary.  A deadline/scheduled 3 days out (delta 3,
this week) now → `org-air-face-day-week' (the AMBER bucket); a
deadline/scheduled a WEEK OR MORE out (delta 7, the FIRST beyond day —
\"a week = 7\") STILL falls back to its slot face
(`org-air-face-deadline' / `-scheduled').  The label bytes are
unchanged.  An off-by-one band, collapsing the this-week bucket, OR
over-applying the ramp past the week FAILS."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (org-air-r87--frozen
    ;; delta 3 (THIS WEEK): Mon 2026-06-15 + 3d = Thu -> amber day-week.
    (pcase-let ((`(,dl . ,dface)
                 (org-air-view--date-label
                  (org-air-r87--dated :deadline 3) 'notes)))
      (should (equal dl "Thu"))
      (should (eq dface 'org-air-face-day-week))
      (should-not (memq dface '(org-air-face-day-today
                                org-air-face-day-tomorrow))))
    (pcase-let ((`(,sl . ,sface)
                 (org-air-view--date-label
                  (org-air-r87--dated :scheduled 3) 'notes)))
      (should (equal sl "Thu"))
      (should (eq sface 'org-air-face-day-week))
      (should-not (memq sface '(org-air-face-day-today
                                org-air-face-day-tomorrow))))
    ;; delta 7 (BEYOND a week): the slot face is UNCHANGED (the ramp's
    ;; helper returns nil for delta >= 7, so `(or … 'org-air-face-SLOT)'
    ;; falls back exactly as pre-R88).
    (should (eq (cdr (org-air-view--date-label
                      (org-air-r87--dated :deadline 7) 'notes))
                'org-air-face-deadline))
    (should (eq (cdr (org-air-view--date-label
                      (org-air-r87--dated :scheduled 7) 'notes))
                'org-air-face-scheduled))))

;;;; -------------------------------------------------------------------
;;;; r87-8 (distinct on the slot arms) — today != tomorrow face
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r87-8-today-not-tomorrow-on-slot-arms ()
  "On the SLOT arms the today face and the tomorrow face stay distinct: a
deadline-today face (r87-1) is NOT `eq' to a deadline-tomorrow face
(r87-2), and the scheduled twins likewise.  Collapsing the two standouts
onto one face on the slot arm FAILS."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (org-air-r87--frozen
    (let ((dl-today  (cdr (org-air-view--date-label
                           (org-air-r87--dated :deadline 0) 'notes)))
          (dl-tomor  (cdr (org-air-view--date-label
                           (org-air-r87--dated :deadline 1) 'notes)))
          (sc-today  (cdr (org-air-view--date-label
                           (org-air-r87--dated :scheduled 0) 'notes)))
          (sc-tomor  (cdr (org-air-view--date-label
                           (org-air-r87--dated :scheduled 1) 'notes))))
      (should (eq dl-today 'org-air-face-day-today))
      (should (eq dl-tomor 'org-air-face-day-tomorrow))
      (should-not (eq dl-today dl-tomor))
      (should-not (eq sc-today sc-tomor)))))

;;;; -------------------------------------------------------------------
;;;; r87-9 (byte layer) — the slot label bytes are UNCHANGED
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r87-9-slot-label-bytes-unchanged ()
  "On every recoloured slot arm the LABEL (`car') is EXACTLY the
`org-air-view--human-date' output verbatim — \"Today\" for delta 0,
\"Tomorrow\" for delta 1 — on BOTH deadline and scheduled; only the
`cdr' face changed.  Cross-checked against the pre-R87 render (helper
stubbed to nil): the label bytes are IDENTICAL.  Any label-byte drift
(a decorated/annotated label) FAILS."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (org-air-r87--frozen
    (dolist (spec '((:deadline 0 "Today") (:deadline 1 "Tomorrow")
                    (:scheduled 0 "Today") (:scheduled 1 "Tomorrow")))
      (pcase-let* ((`(,slot ,off ,want) spec)
                   (item (if (eq slot :deadline)
                             (org-air-r87--dated :deadline off)
                           (org-air-r87--dated :scheduled off)))
                   (`(,label . ,_) (org-air-view--date-label item 'notes))
                   (pre (cl-letf (((symbol-function
                                    'org-air-view--day-relative-face)
                                   (lambda (&rest _) nil)))
                          (car (org-air-view--date-label item 'notes)))))
        ;; label == the wanted human date, == --human-date verbatim, and
        ;; == the pre-R87 label bytes (face-only change).
        (should (equal label want))
        (should (equal label
                       (org-air-view--human-date (org-air-r87--epoch off))))
        (should (equal label pre))))))

;;;; -------------------------------------------------------------------
;;;; r87-10 (regression guard) — the NEUTRAL notes arm is UNCHANGED
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r87-10-notes-arm-unchanged ()
  "R88 RE-BLESS (delta-3 line only): the neutral `notes' arm still carries
the day faces R85 wired — a note with `activity' = today →
`(cons \"Today\" 'org-air-face-day-today)', tomorrow →
`(cons \"Tomorrow\" 'org-air-face-day-tomorrow)' (the today/tomorrow half
is UNCHANGED).  The three-days-out sub-assertion now RAMPS: a delta-3
note takes `org-air-face-day-week' (the R88 this-week AMBER band; was
`org-air-face-date' under R85/R87 — the notes arm reads the SAME widened
helper as the slot arms).  Disturbing the notes arm's today/tomorrow
faces, or dropping the this-week band, FAILS."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (org-air-r87--frozen
    (should (equal (org-air-view--date-label
                    (org-air-r87--note :activity 0) 'notes)
                   (cons "Today" 'org-air-face-day-today)))
    (should (equal (org-air-view--date-label
                    (org-air-r87--note :activity 1) 'notes)
                   (cons "Tomorrow" 'org-air-face-day-tomorrow)))
    (should (eq (cdr (org-air-view--date-label
                      (org-air-r87--note :activity 3) 'notes))
                'org-air-face-day-week))))

;;;; -------------------------------------------------------------------
;;;; r87-11 (STRENGTHEN) — the SCHEDULED cell reaches the painter too
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r87-11-scheduled-face-through-painter ()
  "The SCHEDULED twin of r87-5: a scheduled-today / scheduled-tomorrow
cell, rendered through `org-air-view--item-date-text'
(`org-air-date-style 'text'), carries the day face on the rendered text
with byte-identical label bytes vs the pre-R87 (helper-stubbed) render.
Proves the standout reaches a RENDERED scheduled cell, not just the
(LABEL . FACE) cons.  A change that mutes the scheduled slot cell FAILS."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--item-date-text)))
  (let ((org-air-date-style 'text))
    (org-air-r87--frozen
      (dolist (spec '((0 "Today" org-air-face-day-today)
                      (1 "Tomorrow" org-air-face-day-tomorrow)))
        (pcase-let* ((`(,off ,want ,dayface) spec)
                     (item (org-air-r87--dated :scheduled off))
                     (r87 (org-air-view--item-date-text item 'notes))
                     (pre (cl-letf (((symbol-function
                                      'org-air-view--day-relative-face)
                                     (lambda (&rest _) nil)))
                            (org-air-view--item-date-text item 'notes))))
          (should (equal (substring-no-properties r87) want))
          (should (equal (substring-no-properties r87)
                         (substring-no-properties pre)))
          (should (eq (get-text-property 0 'face r87) dayface))
          (should (eq (get-text-property 0 'face pre)
                      'org-air-face-scheduled)))))))

;;;; -------------------------------------------------------------------
;;;; r87-12 (STRENGTHEN, R53) — the face is data-pure: no rescan / no file
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r87-12-day-face-is-data-pure-no-rescan ()
  "R53: the day face is chosen from the item's OWN deadline/scheduled slot
and the `--days-between' delta already computed in `--date-label' — it
never re-opens a file or re-queries.  Proven by resolving a deadline-today
item whose marker points at a NON-EXISTENT file while the file openers
`find-file-noselect' / `insert-file-contents' AND
`org-air-view--marker-timestamp-time' are hard-errored: `--date-label'
still returns `(cons \"Today\" 'org-air-face-day-today)' without touching
any of them.  A face path that reached back to the file (a rescan) FAILS."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (let ((item (org-air-r87--dated :deadline 0
                                  :file "/tmp/org-air-r87-nonexistent.org")))
    (org-air-r87--frozen
      (cl-letf (((symbol-function 'find-file-noselect)
                 (lambda (&rest _) (error "R53 violated: opened a file")))
                ((symbol-function 'insert-file-contents)
                 (lambda (&rest _) (error "R53 violated: read a file")))
                ((symbol-function 'org-air-view--marker-timestamp-time)
                 (lambda (&rest _) (error "R53 violated: rescanned marker"))))
        (should (equal (org-air-view--date-label item 'notes)
                       (cons "Today" 'org-air-face-day-today)))))))

;;;; -------------------------------------------------------------------
;;;; r87-13 (STRENGTHEN) — the rule is UNIFORM across the three arms
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r87-13-uniform-across-slots ()
  "Rule (A) is one uniform rule with no per-slot exceptions (§Decision
point 4): for TODAY the deadline, scheduled AND neutral-notes arms ALL
resolve `org-air-face-day-today'; for TOMORROW they all resolve
`org-air-face-day-tomorrow'.  A partial rule-A that recoloured only ONE
slot (re-introducing the R85 asymmetry the pivot removes) FAILS."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (org-air-r87--frozen
    (dolist (spec '((0 org-air-face-day-today)
                    (1 org-air-face-day-tomorrow)))
      (pcase-let ((`(,off ,dayface) spec))
        (should (eq (cdr (org-air-view--date-label
                          (org-air-r87--dated :deadline off) 'notes))
                    dayface))
        (should (eq (cdr (org-air-view--date-label
                          (org-air-r87--dated :scheduled off) 'notes))
                    dayface))
        (should (eq (cdr (org-air-view--date-label
                          (org-air-r87--note :activity off) 'notes))
                    dayface))))))

;;;; -------------------------------------------------------------------
;;;; r87-14 (STRENGTHEN) — the svg crux: the pill :fill (face foreground)
;;;;           actually repaints (declared foreground inequality)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r87-14-day-face-foreground-repaints-pill ()
  "The board date pill draws its label with `:fill (face-foreground FACE)'
in `org-air-view--svg-pillify' — so the pill colour IS the face
foreground.  For the standout to REACH the pill, the day face's declared
`:foreground' must DIFFER from the slot face's.  This seam pins that,
inequality-only (exact teal/rose GUI-confirm-only), reading the
`face-defface-spec' directly (deterministic in batch): in BOTH the light
and the dark tier, `org-air-face-day-today' / `-day-tomorrow' each declare
a foreground DIFFERENT from `org-air-face-deadline' AND from
`org-air-face-scheduled', and the two day faces differ from each other.
\(The dark scheduled case is the Nord8-vs-Nord9 adjacency — subtle but
still non-equal.)  A day face that resolved to the slot hue — no visible
pill repaint — FAILS."
  (skip-unless (locate-library "org-air"))
  ;; `face-foreground' resolves to nil in a 0-colour batch frame, so read
  ;; the DECLARED foreground from the `face-defface-spec', following one
  ;; level of `:inherit' (the slot faces inherit popout/salient, which
  ;; carry the concrete per-tier hex the pill `:fill' ultimately draws).
  (cl-labels ((entry-atts (entry)
                ;; Normalise both defface element forms (mirrors
                ;; `face-spec-choose'): `(DISPLAY (PLIST))' wrapped (the day
                ;; faces) vs `(DISPLAY . PLIST)' spliced (the slot faces,
                ;; e.g. `(t :inherit org-air-face-popout)').
                (let ((attrs (cdr entry)))
                  (if (and (consp attrs) (null (cdr attrs)))
                      (car attrs)
                    attrs)))
              (tier-atts (face bg)
                (let ((spec (get face 'face-defface-spec)))
                  (or (cl-loop for entry in spec
                               for display = (car entry)
                               when (and (listp display)
                                         (member (list 'background bg) display))
                               return (entry-atts entry))
                      (let ((tentry (assq t spec)))
                        (and tentry (entry-atts tentry))))))
              (eff-fg (face bg)
                (let* ((atts (tier-atts face bg))
                       (fg (plist-get atts :foreground)))
                  (or fg
                      (let ((parent (plist-get atts :inherit)))
                        (and parent (symbolp parent) (eff-fg parent bg)))))))
    (dolist (bg '(light dark))
      (let ((today    (eff-fg 'org-air-face-day-today bg))
            (tomorrow (eff-fg 'org-air-face-day-tomorrow bg))
            (deadline (eff-fg 'org-air-face-deadline bg))
            (scheduled (eff-fg 'org-air-face-scheduled bg)))
        ;; every foreground is a concrete hex the pill can fill with.
        (dolist (c (list today tomorrow deadline scheduled))
          (should (stringp c))
          (should (string-prefix-p "#" c)))
        ;; the day faces repaint away from BOTH slot hues...
        (should-not (equal today deadline))
        (should-not (equal today scheduled))
        (should-not (equal tomorrow deadline))
        (should-not (equal tomorrow scheduled))
        ;; ...and the two standouts stay distinguishable on the pill.
        (should-not (equal today tomorrow))))))

;;;; -------------------------------------------------------------------
;;;; r87-15 (STRENGTHEN) — the day face survives the PILL-style painter
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r87-15-day-face-survives-pill-style ()
  "With `org-air-date-style 'pill' the deadline-today cell rendered
through `org-air-view--item-date-text' STILL carries the day face at the
head of the cell (the colour source the svg pill's `:fill' reads; on a
non-graphical batch frame `--svg-pillify' falls back to the plain padded
coloured text, which keeps the same face).  The trimmed cell text is
\"Today\".  A pill path that dropped the day face — leaving a slot-coloured
pill — FAILS."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--item-date-text)))
  (let ((org-air-date-style 'pill)
        (item (org-air-r87--dated :deadline 0)))
    (org-air-r87--frozen
      (let ((cell (org-air-view--item-date-text item 'notes)))
        (should cell)
        (should (equal (string-trim (substring-no-properties cell)) "Today"))
        (should (eq (get-text-property 0 'face cell)
                    'org-air-face-day-today))))))

(provide 'org-air-round87-test)
;;; org-air-round87-test.el ends here
