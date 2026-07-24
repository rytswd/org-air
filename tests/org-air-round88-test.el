;;; org-air-round88-test.el --- executing ERTs for v0.1 round-88 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.1 round-88 — the date cell stops being a BINARY
;; today/tomorrow highlight and becomes a five-level PROXIMITY HEAT-RAMP
;; (air/v0.1/org-air-round88-design.org).
;;
;; The user read R85/R87's colours as wrong-signed: TOMORROW in red
;; *alarms* (red should mean OVERDUE) and TODAY in teal reads cool/blue
;; when it should read hot.  R88 replaces the two-level highlight with an
;; urgency gradient keyed on the SAME `org-air-view--days-between' delta
;; that `--human-date' already computes:
;;
;;   delta < 0     -> OVERDUE   -> `org-air-face-overdue'      (RED, urgent)
;;   delta = 0     -> TODAY     -> `org-air-face-day-today'    (ORANGE)
;;   delta = 1     -> TOMORROW  -> `org-air-face-day-tomorrow' (today<->week BLEND)
;;   2 <= delta <= 6 -> THIS WEEK -> `org-air-face-day-week'   (AMBER — NEW)
;;   delta >= 7    -> BEYOND    -> the slot's DEFAULT face (deadline/scheduled/date)
;;
;; a RED -> ORANGE -> AMBER -> default ramp that COOLS with distance, so
;; "warmer = nearer/urgent" is legible and RED is reserved for OVERDUE.
;;
;; What R88 KEEPS from R87 verbatim: rule A (the reach across the
;; deadline / scheduled / neutral arms of `org-air-view--date-label') and
;; the svg-pill `:fill (face-foreground FACE)' mechanism — R88 edits ZERO
;; arms, only WIDENS `org-air-view--day-relative-face' (2 levels -> 5) and
;; RECOLOURS the faces it returns (day-today teal->orange, day-tomorrow
;; rose->the per-channel midpoint blend, +day-week amber).  The ONE align:
;; `org-air-face-overdue' read RED in light (#C62828) but its dark tier
;; inherited `org-air-face-critical' = Nord13 #EBCB8B (a pale aurora
;; YELLOW), which is COOLER than a this-week amber and INVERTED the ramp
;; in dark mode; R88 gives overdue an explicit R79 spec dark-aligned to
;; Nord11 #BF616A (the aurora RED formerly on TOMORROW) so overdue reads
;; red in BOTH tiers.
;;
;; A FACE-ONLY change (R53): the label BYTES ("Today"/"Tomorrow"/weekday/
;; "%d %b") are UNTOUCHED — the helper returns only a FACE, a `display'
;; prop — so every text golden is byte-identical; only the GUI pill /
;; coloured-text PIXELS repaint.  No rescan, no cache-version bump.
;;
;; All BATCH/headless.  The exact ramp HEXES are GUI-confirm-only; the
;; seams assert the FACE SYMBOL on the (LABEL . FACE) cons, the rendered
;; TEXT/face, the RELATIONSHIPS between the four ramp hexes (distinctness,
;; blend-betweenness) and the boundary — never a GUI-nudge-able pixel.
;; The ONE exception is the OVERDUE ALIGN: r88-1 pins overdue's dark tier
;; is the aligned RED (#BF616A) and is NOT the pale-yellow critical
;; (#EBCB8B) — that is a STRUCTURAL anti-inversion claim, not a nudge.
;; The clock is frozen to `org-air-test-now' (Mon 2026-06-15): today =
;; 2026-06-15, +3d = Thu 2026-06-18 (this week), +6d = Sun (last this-week
;; day), +7d = 2026-06-22 (first BEYOND day), +8d = 2026-06-23 (beyond),
;; -3d = 2026-06-12 (past neutral), -5d = 2026-06-10 (past deadline).
;;
;; r88-1..13 are the design's §ERT ledger (each names what reverting
;; breaks); r88-14/15 are TEST-round STRENGTHENINGS closing audit gaps:
;;   r88-14 pins the align is SURGICAL — overdue's TTY tier inherits
;;     `error' (NOT `critical'), and `org-air-face-critical' itself is
;;     UNTOUCHED (its dark tier is still the Nord13 #EBCB8B yellow).
;;   r88-15 pins R53 data-purity — the amber this-week face resolves for
;;     a deadline whose marker points at a NON-EXISTENT file with the file
;;     openers hard-errored (no rescan, the r87-12 analogue for the ramp).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-air-test-helpers)

(when (locate-library "org-air")
  (require 'org-air))

(defvar org-air-date-style)

;;;; -------------------------------------------------------------------
;;;; Scaffolding (self-contained; mirrors the R85/R87 builders)
;;;; -------------------------------------------------------------------

(defmacro org-air-r88--frozen (&rest body)
  "Run BODY with `current-time' frozen to `org-air-test-now'."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'current-time)
              (lambda () org-air-test-now)))
     ,@body))

(defun org-air-r88--epoch (days)
  "Return the epoch float DAYS calendar days from the frozen now.
Stored in the `activity' slot (an epoch float); DAYS = 0 is today."
  (float-time (time-add org-air-test-now (days-to-time days))))

(defun org-air-r88--org-ts (days)
  "Return an Org timestamp object DAYS calendar days from the frozen now.
Built from the frozen now's calendar day so `--date-label's deadline /
scheduled arms resolve it as today (0) / tomorrow (1) / a weekday (2..6)
/ a beyond date (>=7) / OVERDUE (<0)."
  (org-timestamp-from-string
   (format-time-string "<%Y-%m-%d %a>"
                       (time-add org-air-test-now (days-to-time days)))))

(cl-defun org-air-r88--dated (&key deadline scheduled
                                   (file "/tmp/org-air-r88-dated.org"))
  "Build an `org-air-item' carrying a DEADLINE and/or SCHEDULED day offset.
DEADLINE / SCHEDULED are day offsets from the frozen now, stored as Org
timestamp objects, so `--date-label' takes its SLOT arm.  The marker is a
\(FILE . POS) cons the render layer never opens (R53); FILE defaults to a
path that does not exist."
  (org-air-item-create
   :title "A dated task"
   :file file
   :marker (cons file 1)
   :kind 'heading
   :deadline (and deadline (org-air-r88--org-ts deadline))
   :scheduled (and scheduled (org-air-r88--org-ts scheduled))))

(cl-defun org-air-r88--note (&key activity)
  "Build a dateless NOTE-shape `org-air-item' with ACTIVITY (day offset).
No scheduled/deadline, so `--date-label' falls through to the NEUTRAL
`notes' arm.  ACTIVITY is a day offset stored as the epoch float the scan
would have cached (R53 P3)."
  (org-air-item-create
   :title "A quiet note"
   :file "/tmp/org-air-r88-note.org"
   :marker (cons "/tmp/org-air-r88-note.org" 1)
   :kind 'heading
   :activity (and activity (org-air-r88--epoch activity))))

(defun org-air-r88--fg (face bg)
  "Return the DECLARED :foreground of FACE for the 256-colour BG tier.
Reads the `face-defface-spec' directly (deterministic in batch); returns
the hex string, or nil when the tier is absent.  Never a `face-foreground'
resolution (nil in a 0-colour batch frame)."
  (let ((spec (get face 'face-defface-spec)))
    (cl-loop for (display atts) in spec
             when (and (listp display)
                       (member (list 'background bg) display))
             return (plist-get atts :foreground))))

(defun org-air-r88--rgb (hex)
  "Return the (R G B) 0..255 channel list of a \"#RRGGBB\" HEX string."
  (list (string-to-number (substring hex 1 3) 16)
        (string-to-number (substring hex 3 5) 16)
        (string-to-number (substring hex 5 7) 16)))

(defconst org-air-r88--ramp-faces
  '(org-air-face-overdue org-air-face-day-today
    org-air-face-day-tomorrow org-air-face-day-week)
  "The four faces of the R88 proximity heat-ramp, hottest -> coolest.")

(defconst org-air-r88--slot-faces
  '(org-air-face-deadline org-air-face-scheduled org-air-face-date)
  "The three BEYOND (>=7d) slot-default faces the ramp must NOT collide with.")

;;;; -------------------------------------------------------------------
;;;; r88-1 — OVERDUE -> red, in BOTH tiers (the align, structural)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r88-1-overdue-red-both-tiers ()
  "A deadline 5 days in the PAST -> `(cons \"OVERDUE 5d\"
'org-air-face-overdue)'; AND `org-air-face-overdue' resolves to a RED
foreground in BOTH tiers.  The align is STRUCTURAL, not a GUI nudge: the
DARK tier is the aligned aurora RED `#BF616A' and is NOT the pale-yellow
`#EBCB8B' that overdue used to inherit from `org-air-face-critical'
\(which was COOLER than the this-week amber and INVERTED the ramp in dark
mode); the LIGHT tier keeps `#C62828' (Red800).  Reverting the align (or
letting overdue re-inherit `critical') FAILS."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (org-air-r88--frozen
    ;; the OVERDUE arm precedes the ramp arms and the helper never answers
    ;; delta<0, so a past deadline reaches the RED overdue face, never a
    ;; day face.
    (pcase-let ((`(,label . ,face)
                 (org-air-view--date-label
                  (org-air-r88--dated :deadline -5) 'notes)))
      (should (equal label "OVERDUE 5d"))
      (should (eq face 'org-air-face-overdue))
      ;; overdue is the ramp's RED bucket, but NOT a warm day face.
      (should-not (memq face '(org-air-face-day-today
                               org-air-face-day-tomorrow
                               org-air-face-day-week))))
    ;; the align: RED in both tiers, dark is the aurora red NOT the yellow.
    (let ((light (org-air-r88--fg 'org-air-face-overdue 'light))
          (dark  (org-air-r88--fg 'org-air-face-overdue 'dark)))
      (should (equal light "#C62828"))                 ; Red800 kept
      (should (equal dark  "#BF616A"))                 ; Nord11 aurora RED
      (should-not (equal dark "#EBCB8B"))              ; NOT the pale critical yellow
      ;; the anti-inversion in one line: overdue's dark is NOT the amber
      ;; this-week face's dark (so overdue reads hotter than this-week).
      (should-not (equal dark (org-air-r88--fg 'org-air-face-day-week 'dark))))))

;;;; -------------------------------------------------------------------
;;;; r88-2 — TODAY -> day-today, on all three arms
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r88-2-today-day-today-all-arms ()
  "A deadline / scheduled / neutral date that falls TODAY ->
`(cons \"Today\" 'org-air-face-day-today)' on EACH arm (R87 rule A).
Reverting the ORANGE today bucket FAILS."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (org-air-r88--frozen
    (should (equal (org-air-view--date-label
                    (org-air-r88--dated :deadline 0) 'notes)
                   (cons "Today" 'org-air-face-day-today)))
    (should (equal (org-air-view--date-label
                    (org-air-r88--dated :scheduled 0) 'notes)
                   (cons "Today" 'org-air-face-day-today)))
    (should (equal (org-air-view--date-label
                    (org-air-r88--note :activity 0) 'notes)
                   (cons "Today" 'org-air-face-day-today)))))

;;;; -------------------------------------------------------------------
;;;; r88-3 — TOMORROW -> day-tomorrow, on all three arms
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r88-3-tomorrow-day-tomorrow-all-arms ()
  "A deadline / scheduled / neutral date that falls TOMORROW ->
`(cons \"Tomorrow\" 'org-air-face-day-tomorrow)' on EACH arm.  Reverting
the BLEND tomorrow bucket FAILS."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (org-air-r88--frozen
    (should (equal (org-air-view--date-label
                    (org-air-r88--dated :deadline 1) 'notes)
                   (cons "Tomorrow" 'org-air-face-day-tomorrow)))
    (should (equal (org-air-view--date-label
                    (org-air-r88--dated :scheduled 1) 'notes)
                   (cons "Tomorrow" 'org-air-face-day-tomorrow)))
    (should (equal (org-air-view--date-label
                    (org-air-r88--note :activity 1) 'notes)
                   (cons "Tomorrow" 'org-air-face-day-tomorrow)))))

;;;; -------------------------------------------------------------------
;;;; r88-4 — THIS-WEEK (delta 3) -> day-week, on all three arms (NEW band)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r88-4-this-week-day-week-all-arms ()
  "A delta-3 deadline / scheduled / neutral date -> `(cons \"Thu\"
'org-air-face-day-week)' on EACH arm — the NEW amber this-week bucket
\(Mon 2026-06-15 + 3d = Thu 2026-06-18, weekday label).  Reverting the D2
this-week band (helper -> nil for delta 3, falling back to the slot face)
FAILS."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (org-air-r88--frozen
    (should (equal (org-air-view--date-label
                    (org-air-r88--dated :deadline 3) 'notes)
                   (cons "Thu" 'org-air-face-day-week)))
    (should (equal (org-air-view--date-label
                    (org-air-r88--dated :scheduled 3) 'notes)
                   (cons "Thu" 'org-air-face-day-week)))
    (should (equal (org-air-view--date-label
                    (org-air-r88--note :activity 3) 'notes)
                   (cons "Thu" 'org-air-face-day-week)))))

;;;; -------------------------------------------------------------------
;;;; r88-5 — BEYOND (delta 8) -> the slot default, unchanged
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r88-5-beyond-slot-default ()
  "A delta-8 date is BEYOND the week and reads its SLOT default face,
byte- and face-identical to pre-R88: a delta-8 deadline ->
`org-air-face-deadline', a delta-8 scheduled -> `org-air-face-scheduled',
a delta-8 neutral note -> `org-air-face-date' (the helper returns nil for
delta >= 7).  The label is `--human-date's \"%d %b\" (\"23 Jun\").
Over-applying the ramp past the week FAILS."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (org-air-r88--frozen
    (pcase-let ((`(,dl . ,dface)
                 (org-air-view--date-label
                  (org-air-r88--dated :deadline 8) 'notes)))
      (should (equal dl "23 Jun"))
      (should (eq dface 'org-air-face-deadline)))
    (should (eq (cdr (org-air-view--date-label
                      (org-air-r88--dated :scheduled 8) 'notes))
                'org-air-face-scheduled))
    (pcase-let ((`(,nl . ,nface)
                 (org-air-view--date-label
                  (org-air-r88--note :activity 8) 'notes)))
      (should (equal nl "23 Jun"))
      (should (eq nface 'org-air-face-date)))))

;;;; -------------------------------------------------------------------
;;;; r88-6 — boundary exactness "a week = 7": 6 -> week, 7 -> slot
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r88-6-boundary-week-equals-seven ()
  "The this-week band ends EXACTLY at delta 6 and BEYOND begins at delta 7
\(\"a week = 7\"):
  - the helper returns `org-air-face-day-week' for delta 6 and NIL for
    delta 7 (queried directly);
  - a RENDERED delta-6 deadline -> `org-air-face-day-week' (the LAST
    this-week day), a delta-7 deadline -> `org-air-face-deadline' (the
    FIRST beyond day).
An off-by-one band ((<= 2 delta 7) or (< 2 delta 6)) FAILS."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--day-relative-face)
                    (fboundp 'org-air-view--date-label)))
  (let ((now org-air-test-now))
    ;; helper, direct:
    (should (eq (org-air-view--day-relative-face (org-air-r88--epoch 6) now)
                'org-air-face-day-week))
    (should-not (org-air-view--day-relative-face (org-air-r88--epoch 7) now)))
  (org-air-r88--frozen
    ;; rendered deadline cell, both sides of the boundary:
    (should (eq (cdr (org-air-view--date-label
                      (org-air-r88--dated :deadline 6) 'notes))
                'org-air-face-day-week))
    (should (eq (cdr (org-air-view--date-label
                      (org-air-r88--dated :deadline 7) 'notes))
                'org-air-face-deadline))))

;;;; -------------------------------------------------------------------
;;;; r88-7 — the five buckets are DISTINCT face SYMBOLS
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r88-7-ramp-faces-distinct-symbols ()
  "`org-air-face-overdue', `-day-today', `-day-tomorrow', `-day-week' are
all `facep', pairwise NOT `eq', and none `eq' to any BEYOND slot face
\(`org-air-face-deadline' / `-scheduled' / `-date').  Collapsing any two
ramp buckets onto one symbol, or onto a slot face, FAILS.  (Supersedes /
extends r85-2 to the four-face ramp.)"
  (skip-unless (locate-library "org-air"))
  (dolist (face org-air-r88--ramp-faces)
    (should (facep face)))
  ;; pairwise distinct ramp symbols.
  (cl-loop for (a . rest) on org-air-r88--ramp-faces do
           (dolist (b rest)
             (should-not (eq a b))))
  ;; and none collides with a BEYOND slot face.
  (dolist (ramp org-air-r88--ramp-faces)
    (dolist (slot org-air-r88--slot-faces)
      (should-not (eq ramp slot)))))

;;;; -------------------------------------------------------------------
;;;; r88-8 — the ramp is red->orange->amber: DISTINCT hexes, both tiers
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r88-8-ramp-hexes-distinct-both-tiers ()
  "The declared `:foreground' of the four ramp faces are pairwise
DIFFERENT strings in BOTH the 256-colour light tier AND the dark tier —
the pixel-level twin of r88-7 (the r85-13 analogue for the 4-colour
ramp).  A same-hue collapse the symbol check is blind to FAILS.
\(Inequality-only; the exact hexes stay GUI-confirm-only, save the r88-1
overdue align.)"
  (skip-unless (locate-library "org-air"))
  (dolist (bg '(light dark))
    (let ((hexes (mapcar (lambda (f) (org-air-r88--fg f bg))
                         org-air-r88--ramp-faces)))
      ;; every tier declares a hex for every ramp face.
      (dolist (h hexes)
        (should (stringp h))
        (should (string-prefix-p "#" h)))
      ;; pairwise distinct in this tier.
      (should (= (length hexes)
                 (length (delete-dups (copy-sequence hexes))))))))

;;;; -------------------------------------------------------------------
;;;; r88-9 — TOMORROW is a BLEND between TODAY and THIS-WEEK (per channel)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r88-9-tomorrow-blend-between-today-and-week ()
  "The user's \"tomorrow is somewhere between today's and this week\",
pinned STRUCTURALLY: for BOTH tiers, each RGB channel of
`org-air-face-day-tomorrow's foreground lies BETWEEN (inclusive) the
corresponding channel of `org-air-face-day-today' and
`org-air-face-day-week'.  Robust to GUI hex nudges that KEEP the blend; a
tomorrow hue OUTSIDE the today<->week interval on any channel FAILS."
  (skip-unless (locate-library "org-air"))
  (dolist (bg '(light dark))
    (let ((today    (org-air-r88--rgb (org-air-r88--fg 'org-air-face-day-today bg)))
          (tomorrow (org-air-r88--rgb (org-air-r88--fg 'org-air-face-day-tomorrow bg)))
          (week     (org-air-r88--rgb (org-air-r88--fg 'org-air-face-day-week bg))))
      (cl-loop for tod in today
               for tom in tomorrow
               for wk in week do
               (should (<= (min tod wk) tom (max tod wk)))))))

;;;; -------------------------------------------------------------------
;;;; r88-10 — the ORANGE today face reaches text AND the svg pill
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r88-10-today-reaches-text-and-pill ()
  "The deadline-today cell rendered through `org-air-view--item-date-text'
\(`org-air-date-style 'text') carries the TEXT \"Today\" and the
`org-air-face-day-today' `face' property — the plain-text twin of the
svg pill, whose `:fill' reads the SAME `face-foreground' (so the pill is
orange by the same face; GUI-confirm-only).  Cross-checked against the
pre-ramp render (helper stubbed to nil): the label bytes are IDENTICAL
and the pre-ramp face is the slot `org-air-face-deadline'.  A change that
mutes the deadline-today cell, or fails to reach the painter, FAILS."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--item-date-text)))
  (let ((org-air-date-style 'text)      ; plain coloured text, no svg pill
        (item (org-air-r88--dated :deadline 0)))
    (org-air-r88--frozen
      (let* ((r88 (org-air-view--item-date-text item 'notes))
             (pre (cl-letf (((symbol-function 'org-air-view--day-relative-face)
                             (lambda (&rest _) nil)))
                    (org-air-view--item-date-text item 'notes))))
        (should (equal (substring-no-properties r88) "Today"))
        (should (equal (substring-no-properties r88)
                       (substring-no-properties pre)))
        (should (eq (get-text-property 0 'face r88) 'org-air-face-day-today))
        (should (eq (get-text-property 0 'face pre) 'org-air-face-deadline))))))

;;;; -------------------------------------------------------------------
;;;; r88-11 — byte layer: the LABELS are unchanged (face-only ramp)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r88-11-labels-unchanged-byte-layer ()
  "The ramp changes only the `cdr' FACE — the `car' LABEL is EXACTLY
`--human-date's output verbatim on every bucket: \"Today\" (delta 0),
\"Tomorrow\" (delta 1), the weekday (\"Thu\", delta 3) and \"%d %b\"
\(\"23 Jun\", delta 8) — and is byte-identical to the pre-ramp render
\(helper stubbed to nil).  Any label-byte drift (a decorated/annotated
label) FAILS."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (org-air-r88--frozen
    (dolist (spec '((0 "Today") (1 "Tomorrow") (3 "Thu") (8 "23 Jun")))
      (pcase-let* ((`(,off ,want) spec)
                   (item (org-air-r88--dated :deadline off))
                   (`(,label . ,_) (org-air-view--date-label item 'notes))
                   (pre (cl-letf (((symbol-function
                                    'org-air-view--day-relative-face)
                                   (lambda (&rest _) nil)))
                          (car (org-air-view--date-label item 'notes)))))
        (should (equal label want))
        (should (equal label pre))))))    ; face-only: label bytes unchanged

;;;; -------------------------------------------------------------------
;;;; r88-12 — a PAST neutral note stays DEFAULT, never red
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r88-12-past-neutral-not-red ()
  "A `notes' item with `activity' 3 days in the PAST (delta -3) ->
`(cons \"12 Jun\" 'org-air-face-date)' — the helper returns nil for
delta<0, so an OLD log note keeps its DEFAULT grey and does NOT take
`org-air-face-overdue'.  Overdue is a DEADLINE concept, not a stale-note
one; leaking the red onto a past neutral date FAILS."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (org-air-r88--frozen
    (pcase-let ((`(,label . ,face)
                 (org-air-view--date-label
                  (org-air-r88--note :activity -3) 'notes)))
      (should (equal label "12 Jun"))
      (should (eq face 'org-air-face-date))
      (should-not (eq face 'org-air-face-overdue))
      (should-not (memq face org-air-r88--ramp-faces)))))

;;;; -------------------------------------------------------------------
;;;; r88-13 — all four ramp faces obey the R79 convention
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r88-13-ramp-faces-obey-r79-convention ()
  "Each of the four ramp faces carries a 256-colour LIGHT spec, a
256-colour DARK spec (each an explicit `#'-prefixed foreground, bold) AND
a TTY `(t (:inherit … :weight bold))' fallback, and resolves bold.  A
face missing a tier FAILS.  (Supersedes / extends r85-10 to the
four-face ramp incl. the now-explicit `org-air-face-overdue'.)"
  (skip-unless (locate-library "org-air"))
  (dolist (face org-air-r88--ramp-faces)
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
      ;; TTY fallback: `(t (:inherit SOME-FACE :weight bold))'.
      (let ((tty (cadr (assq t spec))))
        (should tty)
        (should (plist-get tty :inherit))
        (should (eq (plist-get tty :weight) 'bold))))))

;;;; -------------------------------------------------------------------
;;;; r88-14 (STRENGTHEN) — the align is SURGICAL: critical is UNTOUCHED
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r88-14-align-surgical-critical-untouched ()
  "The overdue align moves ONLY overdue, never the shared error face:
  - `org-air-face-overdue's TTY tier inherits `error' (a red terminal
    face), NOT `org-air-face-critical' — it no longer inherits `critical'
    at all;
  - `org-air-face-critical' itself is UNTOUCHED — its dark tier is STILL
    the pale Nord13 `#EBCB8B' aurora YELLOW (the hue overdue moved AWAY
    from), and its light tier is still `#C62828';
  - so overdue's dark (#BF616A, the aligned red) DIFFERS from critical's
    dark (#EBCB8B) — the inversion is fixed for overdue without retinting
    the shared face.
Re-tinting `critical' to red (rippling the whole UI), or leaving overdue
inheriting it, FAILS."
  (skip-unless (locate-library "org-air"))
  ;; overdue no longer inherits critical; its TTY falls back to `error'.
  (let ((tty (cadr (assq t (get 'org-air-face-overdue 'face-defface-spec)))))
    (should (eq (plist-get tty :inherit) 'error))
    (should-not (eq (plist-get tty :inherit) 'org-air-face-critical)))
  ;; critical is untouched: still the Nord13 yellow in dark, Red800 light.
  (should (equal (org-air-r88--fg 'org-air-face-critical 'dark) "#EBCB8B"))
  (should (equal (org-air-r88--fg 'org-air-face-critical 'light) "#C62828"))
  ;; and overdue's dark red is NOT critical's dark yellow.
  (should-not (equal (org-air-r88--fg 'org-air-face-overdue 'dark)
                     (org-air-r88--fg 'org-air-face-critical 'dark))))

;;;; -------------------------------------------------------------------
;;;; r88-15 (STRENGTHEN, R53) — the amber this-week face is data-pure
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r88-15-day-week-is-data-pure-no-rescan ()
  "R53: the ramp face is chosen from the item's OWN deadline slot and the
`--days-between' delta already computed in `--date-label' — it never
re-opens a file or re-queries.  Proven by resolving a delta-3 (this week)
deadline whose marker points at a NON-EXISTENT file while the file
openers `find-file-noselect' / `insert-file-contents' AND
`org-air-view--marker-timestamp-time' are hard-errored: `--date-label'
still returns `(cons \"Thu\" 'org-air-face-day-week)' without touching any
of them.  A face path that reached back to the file (a rescan) FAILS.
\(The r87-12 analogue, extended to the new amber band.)"
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (let ((item (org-air-r88--dated
               :deadline 3 :file "/tmp/org-air-r88-nonexistent.org")))
    (org-air-r88--frozen
      (cl-letf (((symbol-function 'find-file-noselect)
                 (lambda (&rest _) (error "R53 violated: opened a file")))
                ((symbol-function 'insert-file-contents)
                 (lambda (&rest _) (error "R53 violated: read a file")))
                ((symbol-function 'org-air-view--marker-timestamp-time)
                 (lambda (&rest _) (error "R53 violated: rescanned marker"))))
        (should (equal (org-air-view--date-label item 'notes)
                       (cons "Thu" 'org-air-face-day-week)))))))

(provide 'org-air-round88-test)
;;; org-air-round88-test.el ends here
