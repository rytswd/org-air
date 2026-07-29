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
;; Slot-interplay ruling: R85 shipped rule (B) NEUTRAL-ONLY (a
;; deadline/scheduled date that falls today/tomorrow KEPT its slot face,
;; the day face replaced ONLY the muted neutral face).  R87 SUPERSEDES
;; that with rule (A) TODAY/TOMORROW-WINS: the day face is now routed into
;; the deadline and scheduled arms too, so a due date today/tomorrow
;; stands out on EVERY slot (air/v0.1/org-air-round87-design.org).  The
;; four rule-B seams that pinned "keeps its slot face"
;; (r85-5/6 TOMORROW, r85-11/12 TODAY) were RETIRED here and their rule-A
;; successors live in tests/org-air-round87-test.el as r87-1..4 (the R3
;; date-label `…-is-benign' tests are their fixture-driven twins,
;; re-blessed to the day face).  Everything below (r85-1..4/7..10/13/14)
;; is UNCHANGED by R87 — it pins the helper, the two faces'
;; distinctness/convention, the NEUTRAL notes-arm behaviour and the byte
;; layer, none of which R87 alters.
;;
;; All BATCH/headless.  The exact teal/rose HEXES are GUI-confirm-only;
;; the seams assert the FACE SYMBOL on the (LABEL . FACE) cons (and, where
;; relevant, the LABEL text), never the pixel.  The clock is frozen to
;; `org-air-test-now' (Mon 2026-06-15): today = 2026-06-15, tomorrow =
;; 2026-06-16.  These seams are the permanent regression guards.  The
;; ERT-seams ledger below is what SURVIVES R87: r85-1 (helper), r85-2
;; (two faces distinct), r85-3/4 (NEUTRAL today/tomorrow + byte layer),
;; r85-7 (non-relative neutral unchanged), r85-8 (non-emitting neutral
;; arms), r85-9 (face-only through the painter), r85-10 (R79 convention),
;; r85-13 (the two day faces are DIFFERENT COLOURS — the user's verbatim
;; ask, inequality-only), r85-14 (the ROSE tomorrow face reaches a
;; RENDERED cell).  The retired rule-B seams (r85-5/6/11/12, which pinned
;; "a deadline/scheduled today/tomorrow KEEPS its slot face") are GONE —
;; R87 rule A reverses that ruling; see org-air-round87-test.el r87-1..4.
;;
;; R88 RE-BLESS: R88 turns the two-level today/tomorrow highlight into a
;; five-level PROXIMITY HEAT-RAMP — it adds a this-week AMBER band
;; `org-air-face-day-week' for delta 2..6 and recolours the two day faces
;; (teal->orange today, rose->the today<->week blend tomorrow); see
;; org-air-round88-test.el.  Two seams here are re-blessed to the ramp:
;; r85-1 (the helper now maps 0->today / 1->tomorrow / 2..6->day-week /
;; else nil) and r85-7 (a +3d neutral date now -> `org-air-face-day-week',
;; was `org-air-face-date').  r85-2/3/4/8/9/10/13/14 are SYMBOL- or
;; byte-level and stand verbatim (the day faces are recoloured, not
;; renamed; day-today/day-tomorrow stay distinct).

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

(cl-defun org-air-r85--note (&key activity updated priority)
  "Build a dateless NOTE-shape `org-air-item' for the neutral label arms.
No scheduled/deadline, so `org-air-view--date-label' falls through to the
NEUTRAL `notes' / `attention' arms.  ACTIVITY and UPDATED are day offsets
from the frozen now, stored as the epoch floats the scan would have
cached (R53 P3 / the R93 recency slot); PRIORITY is an Org cookie value."
  (org-air-item-create
   :title "A quiet note"
   :file "/tmp/org-air-r85-note.org"
   :marker (cons "/tmp/org-air-r85-note.org" 1)
   :kind 'heading
   :priority priority
   :updated (and updated (org-air-r85--epoch updated))
   :activity (and activity (org-air-r85--epoch activity))))

;; (The rule-B slot builders `org-air-r85--org-ts' / `--dated' and the
;; fixture helper `--fixture-label' were removed with the retired
;; r85-5/6/11/12 seams; the rule-A slot seams live in
;; org-air-round87-test.el with their own self-contained builders.)

;;;; -------------------------------------------------------------------
;;;; r85-1 — the helper maps the two deltas and only those
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r85-1-helper-maps-two-deltas ()
  "R88 RE-BLESS: `org-air-view--day-relative-face' now maps the FIVE-level
proximity heat-ramp — the today face for delta 0, the tomorrow face for
delta 1, `org-air-face-day-week' for the this-week band 2..6, and NIL
for a PAST date (-1) and for BEYOND a week (delta >= 7).  R85 shipped
only {0->today, 1->tomorrow, else nil}; R88 widened the helper to
{0, 1, 2..6, else nil} (the +2 case that R85 pinned to nil is now the
amber this-week bucket).  Reverting the helper, its delta keying OR the
this-week band FAILS."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--day-relative-face)))
  (let ((now org-air-test-now))
    (should (eq (org-air-view--day-relative-face
                 (org-air-r85--epoch 0) now)
                'org-air-face-day-today))
    (should (eq (org-air-view--day-relative-face
                 (org-air-r85--epoch 1) now)
                'org-air-face-day-tomorrow))
    ;; R88: the this-week band 2..6 now maps to the amber day-week face
    ;; (delta +2 was nil under R85 — this is THE r85-1 re-bless).
    (should (eq (org-air-view--day-relative-face
                 (org-air-r85--epoch 2) now)
                'org-air-face-day-week))
    (should (eq (org-air-view--day-relative-face
                 (org-air-r85--epoch 6) now)
                'org-air-face-day-week))
    ;; boundary "a week = 7": delta 7 is the FIRST beyond day -> nil.
    (should-not (org-air-view--day-relative-face
                 (org-air-r85--epoch 7) now))
    ;; a PAST date stays nil (the OVERDUE arm owns the red, not this helper).
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

;; r85-5 / r85-6 (deadline/scheduled-TOMORROW KEEP their slot face) were
;; RETIRED by R87 rule A — a deadline/scheduled tomorrow now carries
;; `org-air-face-day-tomorrow'.  Their rule-A successors are
;; org-air-round87-test.el r87-2 / r87-4 (with the re-blessed R3
;; `org-air-date-future-{deadline,scheduled}-is-benign' as fixture twins).

;;;; -------------------------------------------------------------------
;;;; r85-7 — a non-today/tomorrow NEUTRAL date is UNCHANGED
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r85-7-non-relative-neutral-unchanged ()
  "R88 RE-BLESS: a `notes' item with `activity' three days out (delta 3)
now → `(cons \"Thu\" 'org-air-face-day-week)' — the this-week AMBER
bucket of the R88 proximity ramp.  Under R85 a +3d neutral date kept the
muted `org-air-face-date' (no this-week band existed); R88 gives delta
2..6 the amber day-week face.  The LABEL bytes are UNCHANGED (\"Thu\") —
this is a face-only ramp, and delta 3 is neither today nor tomorrow.
Reverting the this-week band (delta 3 back to the neutral slot face)
FAILS."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (org-air-r85--frozen
    (pcase-let ((`(,label . ,face)
                 (org-air-view--date-label
                  (org-air-r85--note :activity 3) 'notes)))
      ;; Mon 2026-06-15 + 3d = Thu 2026-06-18 (weekday label, delta 3).
      (should (equal label "Thu"))
      (should (eq face 'org-air-face-day-week))
      (should-not (memq face '(org-air-face-day-today
                               org-air-face-day-tomorrow))))))

;;;; -------------------------------------------------------------------
;;;; r85-8 — the non-emitting neutral arms are UNTOUCHED
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r85-8-non-emitting-neutral-arms-untouched ()
  "The two REASON labels of the Needs-attention arm are always neutral.
R93 re-bless.  The arm this seam guards is the same one, in the same
seat, with the same law -- it must never carry a day face, because it
never emits a relative today/tomorrow -- but its LABEL changed with the
section's meaning.  Pre-R93 a Needs-attention row was there for having
no date, so the cell read the fixed literal \"no date\"; R93 makes it a
quiet-age rule, so the cell carries the REASON the row surfaced:
\"Nd quiet\" (the age that crossed the threshold) or \"always\" (a
threshold-0 row).  Both are still fixed, still neutral
`org-air-face-date', and the retired \"Nd quiet\" chip of the Stale arm
is where the first of them came from.  Accidentally routing either
through the day face FAILS.

R93 FIX-3 re-bless: the \"always\" arm still exists, is still neutral and
is still covered -- it is simply nobody's DEFAULT any more, so it is
asked for by name.  The row that used to reach it (a `#A' with an
UNKNOWN age, back when `#A' sat on threshold 0) now takes the third
arm, the bare word \"quiet\", which was already covered below; since
that arm is now also what a `#A' reads, it is asserted for one
explicitly.  All three arms stay non-emitting and neutral, which is
this seam's whole subject."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (org-air-r85--frozen
    ;; attention, quiet 30 days: the age, in the neutral face.
    (pcase-let ((`(,label . ,face)
                 (org-air-view--date-label
                  (org-air-r85--note :updated -30) 'attention)))
      (should (equal "30d quiet" label))
      (should-not (member label '("Today" "Tomorrow")))
      (should (eq face 'org-air-face-date)))
    ;; attention, threshold 0 (the opt-in): the fixed literal, same face.
    (let ((org-air-attention-days '((?A . 0) (nil . 30))))
      (should (equal (org-air-view--date-label
                      (org-air-r85--note :priority (org-get-priority "[#A]"))
                      'attention)
                     (cons "always" 'org-air-face-date))))
    ;; attention with an UNKNOWN age: the cell is never blank and org-air
    ;; never invents a number -- and at the DEFAULTS this is what a `#A'
    ;; with no history reads too (R93 FIX-3 moved `#A' to three days).
    (should (equal (org-air-view--date-label
                    (org-air-r85--note) 'attention)
                   (cons "quiet" 'org-air-face-date)))
    (should (equal (org-air-view--date-label
                    (org-air-r85--note :priority (org-get-priority "[#A]"))
                    'attention)
                   (cons "quiet" 'org-air-face-date)))))

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

;; r85-11 / r85-12 (deadline/scheduled-TODAY KEEP their slot face) were
;; RETIRED by R87 rule A — a deadline/scheduled today now carries
;; `org-air-face-day-today'.  Their rule-A successors are
;; org-air-round87-test.el r87-1 / r87-3.

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
