;;; org-air-round85-test.el --- executing ERTs for v0.1 round-85 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.1 round-85 — the relative "Today" / "Tomorrow"
;; date labels stop reading as muted grey
;; (air/v0.1/org-air-round85-design.org).
;;
;; `org-air-view--human-date' (org-air-view.el) emits "Today"/"Tomorrow"
;; as bare TEXT; the FACE is chosen downstream in
;; `org-air-view--date-label' by the date SLOT.  In the NEUTRAL `notes'
;; arm that face was `org-air-face-date' (faded grey), so a today/tomorrow
;; note date read as muted grey — the user's complaint.
;;
;; R85 adds two DISTINCT standing-out faces — `org-air-face-day-today'
;; (bold teal) and `org-air-face-day-tomorrow' (bold rose) — and a tiny
;; pure `org-air-view--day-relative-face' keyed on the SAME
;; `org-air-view--days-between' delta as `--human-date'.  The `notes' arm
;; is rewired to `(or (--day-relative-face activity now)
;; 'org-air-face-date)'.
;;
;; Slot-interplay ruling (§Decision, rule B NEUTRAL-ONLY): a deadline or
;; scheduled date that falls today/tomorrow KEEPS its slot face (a
;; deadline-today still reads as a deadline; the R3 date-label tests keep
;; passing).  The day face replaces the muted neutral face ONLY.
;;
;; All BATCH/headless.  The exact teal/rose HEXES are GUI-confirm-only;
;; the seams assert the FACE SYMBOL on the (LABEL . FACE) cons (and, where
;; relevant, the LABEL text), never the pixel.  The clock is frozen to
;; `org-air-test-now' (Mon 2026-06-15): today = 2026-06-15, tomorrow =
;; 2026-06-16.  These seams are the permanent regression guards.  r85-1..10
;; are the design's "ERT seams" ledger (each names what reverting breaks);
;; r85-11..14 are the R85 TEST-round strengthenings that close audit gaps:
;;   r85-11/12 pin the DEADLINE-TODAY / SCHEDULED-TODAY slot faces (rule B
;;     TODAY case) — r85-5/6 pin only the TOMORROW variant, so a PARTIAL
;;     rule-A leaking ONLY the loud `org-air-face-day-today' onto a slot
;;     arm passed all 10 original seams; r85-11/12 redden it.
;;   r85-13 pins the two day faces are DIFFERENT COLOURS (the user's
;;     verbatim ask), strengthening r85-2's symbol-only distinctness to
;;     the hue level (inequality only — exact hex stays GUI-confirm-only).
;;   r85-14 proves the ROSE tomorrow face reaches a RENDERED cell (the
;;     TOMORROW twin of r85-9's today-only painter path).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-air-test-helpers)

(when (locate-library "org-air")
  (require 'org-air))

(defvar org-air-date-style)

;;;; -------------------------------------------------------------------
;;;; Scaffolding
;;;; -------------------------------------------------------------------

(defmacro org-air-r85--frozen (&rest body)
  "Run BODY with `current-time' frozen to `org-air-test-now'."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'current-time)
              (lambda () org-air-test-now)))
     ,@body))

(defun org-air-r85--epoch (days)
  "Return the epoch float DAYS calendar days from the frozen now.
The `activity' slot is stored as an epoch float; DAYS = 0 is today,
1 is tomorrow (relative to `org-air-test-now')."
  (float-time (time-add org-air-test-now (days-to-time days))))

(cl-defun org-air-r85--note (&key activity)
  "Build a dateless NOTE-shape `org-air-item' with ACTIVITY (day offset).
No scheduled/deadline, so `org-air-view--date-label' falls through to the
NEUTRAL `notes' arm.  ACTIVITY is a day offset from the frozen now, stored
as the epoch float the scan would have cached (R53 P3)."
  (org-air-item-create
   :title "A quiet note"
   :file "/tmp/org-air-r85-note.org"
   :marker (cons "/tmp/org-air-r85-note.org" 1)
   :kind 'heading
   :activity (and activity (org-air-r85--epoch activity))))

(defun org-air-r85--org-ts (days)
  "Return an Org timestamp object DAYS calendar days from the frozen now.
Built from the frozen now's calendar day so `org-air-view--date-label's
deadline/scheduled arms resolve it as today (DAYS=0) / tomorrow (DAYS=1) —
exactly the SLOT dates that, under rule B, must KEEP their slot face."
  (org-timestamp-from-string
   (format-time-string "<%Y-%m-%d %a>"
                       (time-add org-air-test-now (days-to-time days)))))

(cl-defun org-air-r85--dated (&key deadline scheduled)
  "Build an `org-air-item' carrying a DEADLINE and/or SCHEDULED day offset.
DEADLINE / SCHEDULED are day offsets from the frozen now (0 = today,
1 = tomorrow) stored as Org timestamp objects, so `--date-label' takes its
SLOT arm (deadline / scheduled) — the rule-B non-neutral path."
  (org-air-item-create
   :title "A dated task"
   :file "/tmp/org-air-r85-dated.org"
   :marker (cons "/tmp/org-air-r85-dated.org" 1)
   :kind 'heading
   :deadline (and deadline (org-air-r85--org-ts deadline))
   :scheduled (and scheduled (org-air-r85--org-ts scheduled))))

(defun org-air-r85--fixture-label (title)
  "Return the (LABEL . FACE) date metadata for fixture item TITLE.
Frozen clock; the bucket is the classified bucket (mirrors the R3 date-
label helper) so the deadline/scheduled arms are exercised exactly as in
production."
  (let* ((items (org-air-query-items))
         (item (org-air-test-find-item title items)))
    (should item)
    (org-air-r85--frozen
      (org-air-view--date-label
       item (car (org-air-classify-item item org-air-test-now))))))

;;;; -------------------------------------------------------------------
;;;; r85-1 — the helper maps the two deltas and only those
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r85-1-helper-maps-two-deltas ()
  "`org-air-view--day-relative-face' returns the today face for a time on
the frozen now's day, the tomorrow face for the next day, and NIL for +2
and for yesterday (-1).  Reverting the helper (or its delta keying)
FAILS."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--day-relative-face)))
  (let ((now org-air-test-now))
    (should (eq (org-air-view--day-relative-face
                 (org-air-r85--epoch 0) now)
                'org-air-face-day-today))
    (should (eq (org-air-view--day-relative-face
                 (org-air-r85--epoch 1) now)
                'org-air-face-day-tomorrow))
    (should-not (org-air-view--day-relative-face
                 (org-air-r85--epoch 2) now))
    (should-not (org-air-view--day-relative-face
                 (org-air-r85--epoch -1) now))
    ;; keyed on the SAME delta as `--human-date' — label & face agree.
    (should (equal (org-air-view--human-date (org-air-r85--epoch 0) now)
                   "Today"))
    (should (equal (org-air-view--human-date (org-air-r85--epoch 1) now)
                   "Tomorrow"))
    ;; NOW defaults to the live clock when omitted.
    (org-air-r85--frozen
      (should (eq (org-air-view--day-relative-face (org-air-r85--epoch 0))
                  'org-air-face-day-today)))))

;;;; -------------------------------------------------------------------
;;;; r85-2 — the two faces are DISTINCT and standing out (anti-collision)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r85-2-faces-distinct-standing-out ()
  "`org-air-face-day-today' and `org-air-face-day-tomorrow' are non-nil,
are NOT `eq' to each other, and are NOT `eq' to any of `org-air-face-date'
/ `-scheduled' / `-deadline' / `-overdue'.  (Exact hex GUI-confirm-only;
this pins the requirement \"distinct from each other AND from
deadline/scheduled/date\".)  Collapsing either onto a date-slot face
FAILS."
  (skip-unless (locate-library "org-air"))
  (should (facep 'org-air-face-day-today))
  (should (facep 'org-air-face-day-tomorrow))
  (should-not (eq 'org-air-face-day-today 'org-air-face-day-tomorrow))
  (dolist (slot '(org-air-face-date org-air-face-scheduled
                  org-air-face-deadline org-air-face-overdue))
    (should-not (eq 'org-air-face-day-today slot))
    (should-not (eq 'org-air-face-day-tomorrow slot))))

;;;; -------------------------------------------------------------------
;;;; r85-3 (core) — a NEUTRAL today/tomorrow carries the day face
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r85-3-neutral-today-tomorrow-day-face ()
  "A `notes'-bucket item (no scheduled, no deadline) with `activity' =
today → `(cons \"Today\" 'org-air-face-day-today)'; with `activity' =
tomorrow → `(cons \"Tomorrow\" 'org-air-face-day-tomorrow)'.  Reverting
the D3 notes-arm rewire (back to `org-air-face-date') FAILS."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (org-air-r85--frozen
    (should (equal (org-air-view--date-label
                    (org-air-r85--note :activity 0) 'notes)
                   (cons "Today" 'org-air-face-day-today)))
    (should (equal (org-air-view--date-label
                    (org-air-r85--note :activity 1) 'notes)
                   (cons "Tomorrow" 'org-air-face-day-tomorrow)))))

;;;; -------------------------------------------------------------------
;;;; r85-4 — the LABEL text is unchanged (byte layer)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r85-4-label-text-unchanged ()
  "In the r85-3 case the `car' of the cons is EXACTLY \"Today\" /
\"Tomorrow\" — only the `cdr' (face) changed.  Any drift in the label text
(e.g. a decorated label) FAILS."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (org-air-r85--frozen
    (should (equal (car (org-air-view--date-label
                         (org-air-r85--note :activity 0) 'notes))
                   "Today"))
    (should (equal (car (org-air-view--date-label
                         (org-air-r85--note :activity 1) 'notes))
                   "Tomorrow"))
    ;; label bytes match `--human-date' verbatim (the producer is untouched).
    (should (equal (car (org-air-view--date-label
                         (org-air-r85--note :activity 0) 'notes))
                   (org-air-view--human-date (org-air-r85--epoch 0))))))

;;;; -------------------------------------------------------------------
;;;; r85-5 — slot-interplay: a deadline today/tomorrow KEEPS the deadline
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r85-5-deadline-tomorrow-keeps-slot-face ()
  "The R3 fixture \"Prep client presentation\" (DEADLINE 2026-06-16) →
`(cons \"Tomorrow\" 'org-air-face-deadline)' — NOT the day face.  This is
exactly `org-air-date-future-deadline-is-benign'; pivoting to rule (A)
(override) FAILS it (rule B keeps the deadline reading as a deadline)."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (org-air-test-with-fixtures
    (pcase-let ((`(,label . ,face)
                 (org-air-r85--fixture-label "Prep client presentation")))
      (should (equal label "Tomorrow"))
      (should (eq face 'org-air-face-deadline))
      (should-not (eq face 'org-air-face-day-tomorrow)))))

;;;; -------------------------------------------------------------------
;;;; r85-6 — slot-interplay: a scheduled today/tomorrow KEEPS scheduled
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r85-6-scheduled-tomorrow-keeps-slot-face ()
  "The R3 fixture \"Prepare standup notes\" (SCHEDULED 2026-06-16) →
`(cons \"Tomorrow\" 'org-air-face-scheduled)' — NOT the day face.  Exactly
`org-air-date-future-scheduled-is-benign'; rule (A) FAILS it."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (org-air-test-with-fixtures
    (pcase-let ((`(,label . ,face)
                 (org-air-r85--fixture-label "Prepare standup notes")))
      (should (equal label "Tomorrow"))
      (should (eq face 'org-air-face-scheduled))
      (should-not (eq face 'org-air-face-day-tomorrow)))))

;;;; -------------------------------------------------------------------
;;;; r85-7 — a non-today/tomorrow NEUTRAL date is UNCHANGED
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r85-7-non-relative-neutral-unchanged ()
  "A `notes' item with `activity' three days out → `(cons \"Thu\"
'org-air-face-date)' — the neutral face, byte-identical to pre-R85.
Over-applying the day face (delta ∉ {0,1}) FAILS."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (org-air-r85--frozen
    (pcase-let ((`(,label . ,face)
                 (org-air-view--date-label
                  (org-air-r85--note :activity 3) 'notes)))
      ;; Mon 2026-06-15 + 3d = Thu 2026-06-18 (weekday label, delta 3).
      (should (equal label "Thu"))
      (should (eq face 'org-air-face-date))
      (should-not (memq face '(org-air-face-day-today
                               org-air-face-day-tomorrow))))))

;;;; -------------------------------------------------------------------
;;;; r85-8 — the non-emitting neutral arms are UNTOUCHED
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r85-8-non-emitting-neutral-arms-untouched ()
  "An `attention' item with no date → `(cons \"no date\"
'org-air-face-date)'; a `stale' item → a \"Nd quiet\" label with
`org-air-face-date' — neither ever carries a day face (they never emit a
relative today/tomorrow).  Accidentally routing them through the day face
FAILS."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (org-air-r85--frozen
    ;; attention: "no date" is a fixed literal, always neutral.
    (should (equal (org-air-view--date-label
                    (org-air-r85--note :activity nil) 'attention)
                   (cons "no date" 'org-air-face-date)))
    ;; stale: even with `activity' = today the label is a "quiet" chip in
    ;; the neutral face (it is never a relative today/tomorrow).
    (pcase-let ((`(,label . ,face)
                 (org-air-view--date-label
                  (org-air-r85--note :activity 0) 'stale)))
      (should (string-match-p "quiet" label))
      (should-not (member label '("Today" "Tomorrow")))
      (should (eq face 'org-air-face-date)))))

;;;; -------------------------------------------------------------------
;;;; r85-9 — the change is FACE-only (label bytes identical)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r85-9-face-only-bytes-identical ()
  "The date cell for a today-dated neutral note carries the TEXT \"Today\"
and, with R85, the day face — while the underlying label BYTES are
byte-identical to the pre-R85 render (a face is a display prop, not text).
Proven by rendering the SAME cell twice through the real date-cell painter
`org-air-view--item-date-text': once normally and once with the R85 helper
stubbed to nil (the pre-R85 face choice).  The stripped text is IDENTICAL;
only the `face' property differs.  Any change that alters the LABEL BYTES
(not just the face) FAILS."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--item-date-text)))
  (let ((org-air-date-style 'text)      ; plain coloured text, no svg pill
        (item (org-air-r85--note :activity 0)))
    (org-air-r85--frozen
      (let* ((r85 (org-air-view--item-date-text item 'notes))
             (pre (cl-letf (((symbol-function 'org-air-view--day-relative-face)
                             (lambda (&rest _) nil)))
                    (org-air-view--item-date-text item 'notes))))
        ;; both render the SAME visible label bytes.
        (should (equal (substring-no-properties r85) "Today"))
        (should (equal (substring-no-properties r85)
                       (substring-no-properties pre)))
        ;; but the FACE differs — R85 paints the standout day face where
        ;; pre-R85 painted the muted neutral face.
        (should (eq (get-text-property 0 'face r85) 'org-air-face-day-today))
        (should (eq (get-text-property 0 'face pre) 'org-air-face-date))))))

;;;; -------------------------------------------------------------------
;;;; r85-10 — the faces obey the R79 face convention
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r85-10-faces-obey-r79-convention ()
  "Both faces carry a 256-colour light spec, a 256-colour dark spec AND a
TTY `(t (:inherit …))' fallback, and each resolves bold.  (Exact hex
GUI-confirm-only.)  A face missing the light/dark/TTY tier FAILS."
  (skip-unless (locate-library "org-air"))
  (dolist (face '(org-air-face-day-today org-air-face-day-tomorrow))
    (should (facep face))
    (let ((spec (get face 'face-defface-spec)))
      (should spec)
      ;; light + dark tiers: explicit foreground hex, bold weight.
      (dolist (bg '(light dark))
        (let ((atts (cl-loop for (display atts) in spec
                             when (and (listp display)
                                       (member (list 'background bg) display))
                             return atts)))
          (should atts)
          (should (stringp (plist-get atts :foreground)))
          (should (string-prefix-p "#" (plist-get atts :foreground)))
          (should (eq (plist-get atts :weight) 'bold))))
      ;; TTY fallback: `(t (:inherit … :weight bold))'.
      (let ((tty (cadr (assq t spec))))
        (should tty)
        (should (memq (plist-get tty :inherit)
                      '(org-air-face-salient org-air-face-popout)))
        (should (eq (plist-get tty :weight) 'bold))))))

;;;; -------------------------------------------------------------------
;;;; r85-11 — slot-interplay: a deadline TODAY KEEPS the deadline face
;;;;           (the TODAY twin of r85-5; closes the partial rule-A gap)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r85-11-deadline-today-keeps-slot-face ()
  "A DEADLINE that falls TODAY → `(cons \"Today\" 'org-air-face-deadline)' —
it KEEPS the deadline slot face, it does NOT take `org-air-face-day-today'
(§Decision rule B: a deadline-today still reads as an urgent deadline).

This is the TODAY companion of r85-5 (which pins only the TOMORROW case
via the R3 fixture).  It is the strictly stronger guard: a PARTIAL rule-A
that leaks ONLY the standout `org-air-face-day-today' onto the deadline
arm (the most tempting mis-fix — the today face is the loud one) passes
r85-5/-6 verbatim yet FAILS here.  The delta-0 slot date is the single
row where repainting teal would most destroy the \"this is a deadline\"
signal, so it is pinned explicitly."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (org-air-r85--frozen
    (pcase-let ((`(,label . ,face)
                 (org-air-view--date-label
                  (org-air-r85--dated :deadline 0) 'notes)))
      (should (equal label "Today"))
      (should (eq face 'org-air-face-deadline))
      (should-not (eq face 'org-air-face-day-today))
      (should-not (memq face '(org-air-face-day-today
                               org-air-face-day-tomorrow))))))

;;;; -------------------------------------------------------------------
;;;; r85-12 — slot-interplay: a scheduled TODAY KEEPS the scheduled face
;;;;           (the TODAY twin of r85-6; closes the partial rule-A gap)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r85-12-scheduled-today-keeps-slot-face ()
  "A SCHEDULED that falls TODAY → `(cons \"Today\" 'org-air-face-scheduled)'
— it KEEPS the scheduled slot face, NOT `org-air-face-day-today' (rule B).
The TODAY companion of r85-6 (which pins only TOMORROW via the R3
fixture): a partial rule-A leaking the today face onto the scheduled arm
passes r85-6 yet FAILS here."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (org-air-r85--frozen
    (pcase-let ((`(,label . ,face)
                 (org-air-view--date-label
                  (org-air-r85--dated :scheduled 0) 'notes)))
      (should (equal label "Today"))
      (should (eq face 'org-air-face-scheduled))
      (should-not (eq face 'org-air-face-day-today))
      (should-not (memq face '(org-air-face-day-today
                               org-air-face-day-tomorrow))))))

;;;; -------------------------------------------------------------------
;;;; r85-13 — the two day faces stand out in DIFFERENT COLOURS
;;;;           (the user's verbatim ask, strengthening r85-2 to the
;;;;            colour level — INEQUALITY only, exact hex GUI-confirm-only)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r85-13-day-faces-different-colours ()
  "The user's verbatim ask was \"TODAY and TOMORROW should be in different
colour to stand out more\".  r85-2 pins the two are different face SYMBOLS
— but two distinct symbols could still resolve to the SAME hue, which
would violate the ask.  This seam pins the stronger, colour-level claim:
the declared `:foreground' of `org-air-face-day-today' and
`org-air-face-day-tomorrow' DIFFER in BOTH the 256-colour light tier AND
the dark tier.  It asserts INEQUALITY only (never a specific hex — the
exact teal/rose values stay GUI-confirm-only), so a palette refresh is
free as long as the two stay distinguishable.  Collapsing today and
tomorrow onto one colour FAILS."
  (skip-unless (locate-library "org-air"))
  (cl-flet ((fg (face bg)
              (let ((spec (get face 'face-defface-spec)))
                (cl-loop for (display atts) in spec
                         when (and (listp display)
                                   (member (list 'background bg) display))
                         return (plist-get atts :foreground)))))
    (dolist (bg '(light dark))
      (let ((today (fg 'org-air-face-day-today bg))
            (tomorrow (fg 'org-air-face-day-tomorrow bg)))
        (should (stringp today))
        (should (stringp tomorrow))
        ;; different COLOUR (not just a different symbol) in each tier.
        (should-not (equal today tomorrow))))))

;;;; -------------------------------------------------------------------
;;;; r85-14 — end-to-end: the ROSE tomorrow face reaches the rendered
;;;;           date cell too (the TOMORROW twin of r85-9's today case)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r85-14-tomorrow-face-only-through-painter ()
  "The date cell for a TOMORROW-dated neutral note carries the TEXT
\"Tomorrow\" and the standout `org-air-face-day-tomorrow' face, while the
label BYTES are byte-identical to the pre-R85 render (a face is a display
prop, not text).  Renders the SAME cell twice through the real painter
`org-air-view--item-date-text' — once normally, once with the R85 helper
stubbed to nil (pre-R85) — and asserts identical stripped text but a
different `face'.  This is the TOMORROW companion of r85-9 (which covers
only TODAY): it proves the ROSE face actually reaches a rendered cell, not
just the (LABEL . FACE) cons.  A change that mutes the tomorrow neutral
cell, or alters its label bytes, FAILS."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--item-date-text)))
  (let ((org-air-date-style 'text)      ; plain coloured text, no svg pill
        (item (org-air-r85--note :activity 1)))
    (org-air-r85--frozen
      (let* ((r85 (org-air-view--item-date-text item 'notes))
             (pre (cl-letf (((symbol-function 'org-air-view--day-relative-face)
                             (lambda (&rest _) nil)))
                    (org-air-view--item-date-text item 'notes))))
        (should (equal (substring-no-properties r85) "Tomorrow"))
        (should (equal (substring-no-properties r85)
                       (substring-no-properties pre)))
        (should (eq (get-text-property 0 'face r85)
                    'org-air-face-day-tomorrow))
        (should (eq (get-text-property 0 'face pre) 'org-air-face-date))))))

(provide 'org-air-round85-test)
;;; org-air-round85-test.el ends here
