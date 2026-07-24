;;; org-air-round82-test.el --- executing ERTs for round-82 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance + audit ERTs for round-82 (air/v0.1/org-air-round82-design.org):
;; the edit transient's `,' priority field stops being R76's full-range
;; one-key read-char PICKER (`read-char-exclusive' + the annoying
;; "Priority %c-%c, SPC clears[, RET keeps %c]:" prompt) and becomes a
;; forward-WRAPPING one-key CYCLE — the user's literal ask ("just cycle
;; through instead"):
;;
;;   : none -> HIGH -> HIGH+1 -> ... -> LOW -> none -> ...   (write target's range)
;;
;;   - `org-air-inbox--priority-cycle-next' is the PURE stub-free seam:
;;     none/nil -> HIGH, a char in [HIGH,LOW) -> 1+, LOW -> `none',
;;     out-of-range/nil -> HIGH restart.  NO prompt, NO read, NO
;;     minibuffer — a cycle cannot be "out of range" (it only ever hands
;;     back an in-range char or the `none' slot), so R76's reject branch
;;     is gone and a stale in-form char SELF-HEALS to HIGH.
;;   - `,' advances ONE slot per press; the ring INCLUDES the cleared
;;     slot (so clearing needs no separate key — R76's SPC retired) and
;;     WRAPS (so R76's up-reachability is delivered by wraparound, not a
;;     prompt — from the lowest one press lands the cleared slot, the
;;     next lands the TOP priority).
;;   - the ring's span + wrap point is the WRITE TARGET's own
;;     `#+PRIORITIES:' range (`org-air-inbox--target-priority-range' over
;;     `org-air-inbox--form-write-target' — the R67-4 law byte-kept), so
;;     a narrower destination wraps SOONER.
;;   - the cleared slot reuses R76's honest tri-state (`:priority' nil
;;     untouched / CHAR set / ?\s CLEAR), armed STATE-AWARE (the `?\s'
;;     sentinel only when the item factually HAS a cookie to remove,
;;     else nil — back to untouched); the sentinel rides BOTH apply legs
;;     byte-unchanged (`(org-priority ?\s)' removes; ZERO apply-leg
;;     edits), and the preview/field-row render the tri-state WYSIWYG
;;     (never "[# ]").
;;
;; All BATCH/headless through the r19/r64/r67/r76 form idiom
;; (`--form-init' + `call-interactively' on the suffix — no transient
;; event loop).  A NO-READ GUARD is the anti-picker anchor: every
;; simulated press runs with `read-char-exclusive' AND `read-char' AND
;; `read-string' `cl-letf'd to a raising stub, so a surviving R76 prompt
;; REDDENS (the cycle must read nothing).  The nine spec seams
;; r82-1..r82-9 SUPERSEDE the retired R76 priority picker ERTs
;; (round76-test.el is deleted — every one of its r76-1..r76-14 seams
;; pinned the now-absent read-char picker; the still-valid coverage —
;; the both-leg apply, the tri-state clear, the WYSIWYG preview — is
;; PRESERVED here, re-driven by cycle presses):
;;
;;   r82-1  THE ASK — form on `[#C]': ONE press (under the no-read
;;          guard) sets `:priority' ?D (C advances one slot), the guard
;;          NEVER fires (no read/minibuffer/message-prompt).  Revert-RED
;;          against R76: the picker fires `read-char-exclusive' (guard
;;          errors) and never lands D from C without a typed key.
;;   r82-2  THE FULL RING, PURE — `--priority-cycle-next' over (?A . ?E):
;;          none/nil -> ?A, ?A..?D -> 1+, ?E -> `none', an OUT-OF-RANGE
;;          ?F and a BELOW-range ?0 both restart at ?A.  No stub.
;;   r82-3  WRAPAROUND + up-reachability — fresh `[#C]': presses
;;          C->D->E, the next lands the CLEARED slot (?\s, preview
;;          cookie-less, row "clear"), the next lands ?A (straight to
;;          the top).  All under the no-read guard.
;;   r82-4  LIVE PREVIEW + FIELD ROW per press (WYSIWYG) — after the D
;;          press `[#D]' / "D"; after the wrap-to-none press NO cookie /
;;          "clear"; after the A press `[#A]' / "A"; "[# ]" NEVER
;;          renders.
;;   r82-5  OUT-OF-RANGE / none / cookie-less starts — (a) cookie-less
;;          item, one press -> ?A (none->top); (b) `:file' narrow (A-C)
;;          + a STALE ?E, one press -> ?A (self-heal, NOT a crash/
;;          reject); (c) pure over (?A . ?C): ?E -> ?A.
;;   r82-6  THE WRITE-TARGET RANGE GOVERNS THE WRAP (R67-4) — with
;;          `:file' narrow (A-C) the effective ?C wraps to `none'
;;          (C->`none', not C->D); `:file' nil -> the item's own A-E
;;          range (C->D).  Same effective slot, different next.
;;   r82-7  ONE-LETTER RANGE toggles none <-> B — one.org (B . B),
;;          `[#B]': press -> ?\s (own B, clear armed), press -> ?B,
;;          press -> ?\s.  Pure: (?B . ?B) gives ?B -> `none',
;;          none -> ?B.
;;   r82-8  APPLY ON RET, BOTH LEGS, RING RECORDED — in place: cycle
;;          `[#C]' to the none slot, execute -> saved bytes lose `[#C]'
;;          (?\s rides the leg), R73 ring recorded; cycle to ?A,
;;          execute -> saved bytes carry `[#A]'; refile leg: `:file'
;;          narrow + cycle to ?A -> the MOVED heading carries `[#A]'.
;;   r82-9  COOKIE-LESS "none" WRITES NOTHING, NO ERROR — cookie-less
;;          item, cycle the full ring back to the none slot -> `:priority'
;;          nil (own nil -> back to untouched); execute -> the gentle
;;          "Nothing to change", bytes identical, NO `user-error' (the
;;          `(org-priority ?\s)' no-cookie error class is unreachable by
;;          construction — Decision 3's state-aware arming).
;;
;; Goldens/mockups: ZERO shifts — the transient renders in no mockup/
;; golden.  Known-failures manifest stays EMPTY.  GUI residue
;; (screenshot-confirm, not ERT-able): the field-row + preview repaint
;; cadence as `,' is held down (each press one slot), and that the echo
;; area is now SILENT (no prompt) during a press.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'transient)
(require 'org-air-test-helpers)

(when (locate-library "org-air")
  (require 'org-air))

;;;; -------------------------------------------------------------------
;;;; Fixture: own (A-E, [#C]) / bare (no cookie) / narrow (A-C target)
;;;;          / one (B-B, [#B])
;;;; -------------------------------------------------------------------

(defvar org-air-r82--dir nil
  "The temp corpus directory of the current `org-air-r82--with-corpus'.")

(defconst org-air-r82--default-specs
  '(("own.org" . "#+PRIORITIES: A E C\n\n* TODO [#C] Widget :inbox:\n  body\n")
    ("bare.org" . "#+title: bare\n\n* TODO Jot :inbox:\n  body\n")
    ("narrow.org" . "#+PRIORITIES: A C B\n\n* Existing\n")
    ("one.org" . "#+PRIORITIES: B B B\n\n* TODO [#B] Solo :inbox:\n  body\n"))
  "The spec fixture: a mid-range `[#C]' item in a file whose own
`#+PRIORITIES: A E C' line widens the range to A-E (room BOTH ways — C
in the middle), a cookie-less item, a refile target with a NARROWER
`#+PRIORITIES: A C B' range (A-C — the r82-5/-6/-8 R67-4 seams), and a
ONE-LETTER `#+PRIORITIES: B B B' file (the r82-7 none<->B toggle).")

(defmacro org-air-r82--with-corpus (specs &rest body)
  "Create a temp Org corpus from SPECS and run BODY against it.
SPECS is a list of (NAME . CONTENT) files (nil = the default spec
corpus).  Binds `org-air-files' to the directory, `org-air-inbox-file'
to its own.org, a temp `org-air-cache-file', a DEAD board buffer name,
fresh form/last/ring state and `org-tags-column' 0 (the r67
byte-stability shape).  Kills every corpus-visiting buffer and deletes
the directory afterwards."
  (declare (indent 1) (debug t))
  `(let* ((org-air-r82--dir (make-temp-file "org-air-r82-" t)))
     (unwind-protect
         (progn
           (pcase-dolist (`(,name . ,content)
                          (or ,specs org-air-r82--default-specs))
             (let ((file-name-handler-alist nil)
                   (coding-system-for-write 'utf-8-unix))
               (write-region (or content "") nil
                             (expand-file-name name org-air-r82--dir)
                             nil 'silent)))
           (let ((org-air-files (list org-air-r82--dir))
                 (org-air-inbox-file
                  (expand-file-name "own.org" org-air-r82--dir))
                 (org-air-cache-file
                  (expand-file-name ".cache/board.eld" org-air-r82--dir))
                 (org-air-view-buffer-name "*org-air-r82-no-board*")
                 (org-air-inbox--refile-form nil)
                 (org-air-inbox--refile-last nil)
                 (org-air-view--edit-ring nil)
                 (org-air-view--edit-redo-ring nil)
                 (org-tags-column 0)
                 (create-lockfiles nil)
                 (inhibit-message t))
             ,@body))
       (when (fboundp 'org-air-query-teardown)
         (org-air-query-teardown))
       (let ((kill-buffer-query-functions nil))
         (dolist (buf (buffer-list))
           (let ((fn (buffer-file-name buf)))
             (when (and fn (string-prefix-p org-air-r82--dir fn))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r82--dir t))))

(defun org-air-r82--file (name)
  "Return the corpus file NAME's absolute path."
  (expand-file-name name org-air-r82--dir))

(defun org-air-r82--item (name text)
  "Build an editor item for the heading containing TEXT in corpus file NAME.
The `:priority' slot carries the heading's raw cookie CHAR (the
`org-air-inbox--item-priority-char' pass-through shape), or nil when
the heading has no cookie."
  (let ((file (org-air-r82--file name)))
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (re-search-forward (regexp-quote text))
       (org-back-to-heading t)
       (let ((line (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position))))
         (org-air-item-create
          :title (substring-no-properties (org-get-heading t t t t))
          :tags (org-get-tags nil t)
          :todo (org-get-todo-state)
          :priority (and (string-match "\\[#\\(.\\)\\]" line)
                         (aref (match-string 1 line) 0))
          :file file
          :marker (point-marker)))))))

(defun org-air-r82--text (name)
  "Return corpus file NAME's on-disk content as a string."
  (with-temp-buffer
    (insert-file-contents (org-air-r82--file name))
    (buffer-string)))

(defvar org-air-r82--read-attempts 0
  "Count of key/minibuffer reads attempted during the last `,' press.
A CYCLE reads nothing — a non-zero count means an R76-style picker
survived (the anti-picker spy).")

(defun org-air-r82--press ()
  "Advance the `,' cycle ONE slot under a NO-READ GUARD.
`read-char-exclusive', `read-char' and `read-string' are `cl-letf'd to
a stub that BUMPS `org-air-r82--read-attempts' and then RAISES — so a
surviving R76 prompt reddens the press (a cycle must read nothing: no
`read-char', no minibuffer, no message-prompt)."
  (setq org-air-r82--read-attempts 0)
  (cl-letf (((symbol-function 'read-char-exclusive)
             (lambda (&rest _)
               (cl-incf org-air-r82--read-attempts)
               (error "cycle must not read a char (read-char-exclusive)")))
            ((symbol-function 'read-char)
             (lambda (&rest _)
               (cl-incf org-air-r82--read-attempts)
               (error "cycle must not read a char (read-char)")))
            ((symbol-function 'read-string)
             (lambda (&rest _)
               (cl-incf org-air-r82--read-attempts)
               (error "cycle must not read the minibuffer (read-string)"))))
    (call-interactively 'org-air-refile-form-priority)))

(defun org-air-r82--field-row ()
  "Render the `,' suffix's field-row description string."
  (let ((proto (get 'org-air-refile-form-priority 'transient--suffix)))
    (funcall (oref proto description))))

(defun org-air-r82--execute (&optional msgs-cell)
  "Drive the execute suffix; push every `message' into MSGS-CELL's car."
  (cl-letf (((symbol-function 'message)
             (lambda (fmt &rest args)
               (when (and msgs-cell fmt)
                 (push (apply #'format fmt args) (car msgs-cell)))
               nil)))
    (call-interactively 'org-air-refile-form-execute)))

;;;; -------------------------------------------------------------------
;;;; r82-1 — THE ASK: C advances to D in ONE press, NO read/minibuffer
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r82-1-one-press-advances-no-read ()
  "The user's ask delivered: on the `[#C]' item ONE press (under the
NO-READ guard) advances `:priority' to ?D — one slot forward, no typed
key.  The guard NEVER fires: no `read-char', no minibuffer, no
message-prompt (`org-air-r82--read-attempts' stays 0 — the
anti-picker spy).  A second press advances D->E (the cycle keeps
moving).  Revert-RED against R76: the picker calls
`read-char-exclusive' (the guard errors) and never lands D from C in
one press without a typed letter."
  (skip-unless (locate-library "org-air"))
  (org-air-r82--with-corpus nil
    (org-air-inbox--form-init (org-air-r82--item "own.org" "Widget"))
    (org-air-r82--press)
    (should (= 0 org-air-r82--read-attempts))   ; the cycle read nothing
    (should (equal (org-air-inbox--form-get :priority) ?D))
    ;; the cycle keeps advancing one slot per press.
    (org-air-r82--press)
    (should (= 0 org-air-r82--read-attempts))
    (should (equal (org-air-inbox--form-get :priority) ?E))))

;;;; -------------------------------------------------------------------
;;;; r82-2 — the full ring, PURE (no stub anywhere)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r82-2-cycle-next-pure-ring ()
  "`org-air-inbox--priority-cycle-next' over (?A . ?E): the `none' slot
and nil both start at ?A (HIGH); each in-range char below the lowest
advances 1+; the lowest ?E wraps to the symbol `none'; an OUT-OF-RANGE
?F and a BELOW-range ?0 each RESTART the ring at ?A (the self-heal —
NOT a clamp, NOT a signal).  The honest stub-free forward-wrapping
ring."
  (skip-unless (locate-library "org-air"))
  (let ((range '(?A . ?E)))
    ;; the cleared slot (and a nil/untouched start) -> the TOP priority.
    (should (equal (org-air-inbox--priority-cycle-next 'none range) ?A))
    (should (equal (org-air-inbox--priority-cycle-next nil range) ?A))
    ;; each in-range char below the lowest advances one slot.
    (should (equal (org-air-inbox--priority-cycle-next ?A range) ?B))
    (should (equal (org-air-inbox--priority-cycle-next ?B range) ?C))
    (should (equal (org-air-inbox--priority-cycle-next ?C range) ?D))
    (should (equal (org-air-inbox--priority-cycle-next ?D range) ?E))
    ;; the lowest wraps into the cleared slot.
    (should (eq (org-air-inbox--priority-cycle-next ?E range) 'none))
    ;; out-of-range / below-range restart at the top (self-heal).
    (should (equal (org-air-inbox--priority-cycle-next ?F range) ?A))
    (should (equal (org-air-inbox--priority-cycle-next ?0 range) ?A))))

;;;; -------------------------------------------------------------------
;;;; r82-3 — wraparound + full up-reachability (no prompt)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r82-3-wraparound-reachability ()
  "From the `[#C]' item, presses walk C->D->E, then the NEXT press
lands the CLEARED slot (`:priority' ?\\s — armed because the item HAS
a cookie; the preview loses its cookie, the row reads \"clear\"), and
the NEXT press lands ?A — straight back to the TOP (R76's
up-reachability, delivered by WRAPAROUND, no prompt).  All presses run
under the NO-READ guard (no read at any step).  Revert-RED: the
pre-R76 one-way cycle's unset step was nil (untouched) and never
reached A without four+ presses; the R76 picker needs a typed key."
  (skip-unless (locate-library "org-air"))
  (org-air-r82--with-corpus nil
    (org-air-inbox--form-init (org-air-r82--item "own.org" "Widget"))
    (org-air-r82--press)                ; C -> D
    (should (equal (org-air-inbox--form-get :priority) ?D))
    (org-air-r82--press)                ; D -> E
    (should (equal (org-air-inbox--form-get :priority) ?E))
    (org-air-r82--press)                ; E (lowest) -> none, armed clear
    (should (equal (org-air-inbox--form-get :priority) ?\s))
    (should (equal (org-air-r82--field-row) "priority clear"))
    (should-not (string-match-p (regexp-quote "[#")
                                (org-air-inbox--form-preview)))
    (org-air-r82--press)                ; none -> A (the wraparound top)
    (should (equal (org-air-inbox--form-get :priority) ?A))
    (should (= 0 org-air-r82--read-attempts))))

;;;; -------------------------------------------------------------------
;;;; r82-4 — the live preview + field row update on each press (WYSIWYG)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r82-4-preview-and-field-per-press ()
  "The tri-state renders on BOTH honest surfaces, refreshed every
press: after the C->D press the preview shows `[#D]' and the row \"D\";
after the wrap-to-none press the preview has NO `[#' cookie and the
row \"clear\"; after the wrap-to-A press `[#A]' / \"A\".  At no point
does the broken \"[# ]\" render."
  (skip-unless (locate-library "org-air"))
  (org-air-r82--with-corpus nil
    (org-air-inbox--form-init (org-air-r82--item "own.org" "Widget"))
    ;; C -> D: the pending char on both surfaces.
    (org-air-r82--press)
    (should (equal (org-air-r82--field-row) "priority D"))
    (should (string-match-p (regexp-quote "[#D]")
                            (org-air-inbox--form-preview)))
    (should-not (string-match-p (regexp-quote "[#C]")
                                (org-air-inbox--form-preview)))
    ;; walk to the lowest, then wrap to the cleared slot.
    (org-air-r82--press)                ; D -> E
    (org-air-r82--press)                ; E -> none (armed clear)
    (should (equal (org-air-r82--field-row) "priority clear"))
    (should-not (string-match-p (regexp-quote "[#")
                                (org-air-inbox--form-preview)))
    (should-not (string-match-p (regexp-quote "[# ]")
                                (org-air-inbox--form-preview)))
    ;; wrap to the top.
    (org-air-r82--press)                ; none -> A
    (should (equal (org-air-r82--field-row) "priority A"))
    (should (string-match-p (regexp-quote "[#A]")
                            (org-air-inbox--form-preview)))))

;;;; -------------------------------------------------------------------
;;;; r82-5 — out-of-range / none / cookie-less starts behave
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r82-5-out-of-range-and-cookieless-starts ()
  "Three degenerate starts self-heal, never crash: (a) a cookie-less
item, one press -> ?A (none -> the top); (b) with `:file' set to
narrow.org (A-C) and a STALE pending ?E (out of A-C from a
since-narrowed destination), one press -> ?A (self-heal RESTART, NOT a
reject or a crash); (c) the pure helper over (?A . ?C) maps the
out-of-range ?E -> ?A."
  (skip-unless (locate-library "org-air"))
  (org-air-r82--with-corpus nil
    ;; (a) cookie-less start: one press lands the top.
    (org-air-inbox--form-init (org-air-r82--item "bare.org" "Jot"))
    (org-air-r82--press)
    (should (equal (org-air-inbox--form-get :priority) ?A))
    ;; (b) a stale ?E over the narrow destination self-heals to the top.
    (org-air-inbox--form-init (org-air-r82--item "own.org" "Widget"))
    (org-air-inbox--form-put :file (org-air-r82--file "narrow.org"))
    (org-air-inbox--form-put :priority ?E)   ; a stale, now-out-of-range pick
    (org-air-r82--press)
    (should (= 0 org-air-r82--read-attempts))
    (should (equal (org-air-inbox--form-get :priority) ?A)))
  ;; (c) pure: ?E is out of A-C -> restart at ?A.
  (should (equal (org-air-inbox--priority-cycle-next ?E '(?A . ?C)) ?A)))

;;;; -------------------------------------------------------------------
;;;; r82-6 — the write-target range governs the ring + wrap point (R67-4)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r82-6-write-target-range-governs-wrap ()
  "The SAME effective slot (?C) advances differently depending on the
WRITE TARGET's own range, read at press time: with `:file' set to
narrow.org (A-C), ?C is the LOWEST slot so one press wraps to the
cleared slot (C -> `none', armed ?\\s because the item has a cookie —
NOT C -> D); with `:file' nil the item's OWN A-E range governs and the
same ?C advances to ?D.  The pure helper agrees: (?A . ?C) wraps ?C to
`none', (?A . ?E) advances ?C to ?D."
  (skip-unless (locate-library "org-air"))
  (org-air-r82--with-corpus nil
    ;; narrow destination (A-C): ?C is the lowest -> wrap to the clear slot.
    (org-air-inbox--form-init (org-air-r82--item "own.org" "Widget"))
    (org-air-inbox--form-put :file (org-air-r82--file "narrow.org"))
    (org-air-r82--press)
    (should (equal (org-air-inbox--form-get :priority) ?\s))
    ;; the item's own A-E range: the same ?C advances to ?D.
    (org-air-inbox--form-init (org-air-r82--item "own.org" "Widget"))
    (org-air-r82--press)
    (should (equal (org-air-inbox--form-get :priority) ?D)))
  ;; pure confirmation of the two wrap points.
  (should (eq (org-air-inbox--priority-cycle-next ?C '(?A . ?C)) 'none))
  (should (equal (org-air-inbox--priority-cycle-next ?C '(?A . ?E)) ?D)))

;;;; -------------------------------------------------------------------
;;;; r82-7 — a one-letter range is a clean none <-> B toggle
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r82-7-one-letter-range-toggle ()
  "`#+PRIORITIES: B B B' collapses the range to (?B . ?B): the ring is
just  none <-> B.  On the `[#B]' item the first press wraps to the
cleared slot (?\\s — armed because the item has a cookie), the next
press lands ?B, the next wraps back to ?\\s — a clean two-state toggle.
Pure: `--priority-cycle-next' over (?B . ?B) maps ?B -> `none' and
none -> ?B."
  (skip-unless (locate-library "org-air"))
  (org-air-r82--with-corpus nil
    (org-air-inbox--form-init (org-air-r82--item "one.org" "Solo"))
    (org-air-r82--press)                ; B (own, also LOW) -> none (armed)
    (should (equal (org-air-inbox--form-get :priority) ?\s))
    (org-air-r82--press)                ; none -> B
    (should (equal (org-air-inbox--form-get :priority) ?B))
    (org-air-r82--press)                ; B -> none (armed)
    (should (equal (org-air-inbox--form-get :priority) ?\s))
    (should (= 0 org-air-r82--read-attempts)))
  ;; pure toggle.
  (should (eq (org-air-inbox--priority-cycle-next ?B '(?B . ?B)) 'none))
  (should (equal (org-air-inbox--priority-cycle-next 'none '(?B . ?B)) ?B)))

;;;; -------------------------------------------------------------------
;;;; r82-8 — apply on RET, BOTH legs, ring recorded (the preserved R76 coverage)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r82-8-apply-on-ret-both-legs ()
  "The cycled slot rides the UNCHANGED apply legs (R76's both-leg apply
+ tri-state clear, re-driven by cycle presses):
  - in place, cleared: cycle `[#C]' to the none slot (?\\s), execute ->
    the SAVED bytes lose `[#C]' with the heading otherwise intact and
    the R73 edit ring recorded the edit;
  - in place, set: cycle to ?A, execute -> the saved bytes carry `[#A]';
  - refile leg: `:file' narrow.org + cycle to ?A -> the MOVED heading
    in the TARGET's saved bytes carries `[#A]' and own.org loses the
    item.
The apply path is byte-unchanged; only the ENTRY is a cycle now."
  (skip-unless (locate-library "org-air"))
  ;; leg 1: in place, cycle to the cleared slot.
  (org-air-r82--with-corpus nil
    (let ((old (org-air-r82--text "own.org")))
      (org-air-inbox--form-init (org-air-r82--item "own.org" "Widget"))
      (org-air-r82--press)              ; C -> D
      (org-air-r82--press)              ; D -> E
      (org-air-r82--press)              ; E -> none (armed clear)
      (should (equal (org-air-inbox--form-get :priority) ?\s))
      (org-air-r82--execute)
      (let ((new (org-air-r82--text "own.org")))
        (should (string-match-p "^\\* TODO Widget :inbox:$" new))
        (should-not (string-match-p (regexp-quote "[#C]") new))
        (should (equal new
                       (replace-regexp-in-string
                        (regexp-quote "* TODO [#C] Widget")
                        "* TODO Widget" old t t))))
      ;; the R73 ring recorded the in-place edit.
      (should (= 1 (length org-air-view--edit-ring)))
      (should (string-match-p "priority"
                              (plist-get (car org-air-view--edit-ring)
                                         :desc)))))
  ;; leg 2: in place, cycle to ?A (a real set).
  (org-air-r82--with-corpus nil
    (org-air-inbox--form-init (org-air-r82--item "own.org" "Widget"))
    (org-air-r82--press)                ; C -> D
    (org-air-r82--press)                ; D -> E
    (org-air-r82--press)                ; E -> none
    (org-air-r82--press)                ; none -> A
    (should (equal (org-air-inbox--form-get :priority) ?A))
    (org-air-r82--execute)
    (should (string-match-p "^\\* TODO \\[#A\\] Widget :inbox:$"
                            (org-air-r82--text "own.org"))))
  ;; leg 3: refile with a cycled ?A — the moved heading carries [#A].
  (org-air-r82--with-corpus nil
    (org-air-inbox--form-init (org-air-r82--item "own.org" "Widget"))
    (org-air-inbox--form-put :file (org-air-r82--file "narrow.org"))
    (org-air-r82--press)                ; C (narrow LOW) -> none (armed)
    (org-air-r82--press)                ; none -> A
    (should (equal (org-air-inbox--form-get :priority) ?A))
    (org-air-r82--execute)
    (let ((target (org-air-r82--text "narrow.org")))
      (should (string-match-p "\\[#A\\] Widget" target))
      (should-not (string-match-p "Widget"
                                  (org-air-r82--text "own.org"))))))

;;;; -------------------------------------------------------------------
;;;; r82-9 — cookie-less "none" writes nothing, no error
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r82-9-cookieless-none-writes-nothing ()
  "On a cookie-less item the ring's `none' slot stores nil (own nil ->
back to untouched), NOT the ?\\s sentinel (state-aware arming: there is
factually no cookie to remove).  Cycle the FULL ring back to the none
slot -> `:priority' nil; execute is the gentle \"Nothing to change\"
no-op, bytes identical, NO `user-error' (the `(org-priority ?\\s)'
no-cookie error class is unreachable by construction)."
  (skip-unless (locate-library "org-air"))
  (org-air-r82--with-corpus nil
    (let ((old (org-air-r82--text "bare.org"))
          (msgs (list nil)))
      (org-air-inbox--form-init (org-air-r82--item "bare.org" "Jot"))
      ;; first press leaves the none slot for a real char (the top)…
      (org-air-r82--press)
      (should (org-air-inbox--form-get :priority))
      ;; …then keep cycling forward until the ring returns to the none
      ;; slot, which on a cookie-less item stores nil (untouched again).
      (let ((guard 0))
        (while (and (org-air-inbox--form-get :priority) (< guard 26))
          (org-air-r82--press)
          (cl-incf guard))
        (should (< guard 26)))
      (should (null (org-air-inbox--form-get :priority)))
      (should (= 0 org-air-r82--read-attempts))
      ;; execute: the gentle no-op, no error, bytes identical.
      (org-air-r82--execute msgs)
      (should (seq-some (lambda (m) (string-match-p "Nothing to change" m))
                        (car msgs)))
      (should (equal (org-air-r82--text "bare.org") old)))))

(provide 'org-air-round82-test)
;;; org-air-round82-test.el ends here
