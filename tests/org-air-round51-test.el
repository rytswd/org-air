;;; org-air-round51-test.el --- executing ERTs for v0.5 round-51 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.5 round-51 (air/v0.5/org-air-round51-design.org),
;; the dropped-UX follow-up to R48:
;;   R51-1 — the strike-through is GONE from BOTH dropped faces (the
;;     whole-row `font-lock-face' drew `:strike-through' across the
;;     inter-column flex fill as a full-width RULE); grey
;;     (`org-air-face-faded' inherit) is the SOLE dropped affordance.
;;   R51-2 — dropped docs sort to the group BOTTOM: ready →
;;     work-in-progress → complete → draft → (unknown) → DROPPED LAST,
;;     via the ONE rank fn `org-air-project--state-sort-rank' in BOTH
;;     comparators — so the ordering holds collapsed (fold row at the
;;     group bottom) AND expanded (revealed rows at the bottom).
;;   R51-3 — the fold/'more' rows themselves answer TAB (and RET): the
;;     board's `…and N more — press TAB on the title to expand' row now
;;     carries `org-air-more-row' BUCKET and expands its section instead
;;     of drifting to the next header; the project's `… N dropped — TAB
;;     to show' row already dispatched (R48-3) and gets a LOCK conjunct.
;;
;; Executing renders over the round-20 air-project fixture (project) and
;; the `org-air-viewport-test-with-dashboard' harness at width 100 (the
;; board render that carries `…and 2 more'/`…and 3 more' rows) — not unit
;; stubs.  r51-1/r51-2 and the BOARD half of r51-3 FAIL if the R51 impl
;; is reverted; the PROJECT half of r51-3 is lock-style (guards the
;; R48-3 affordance on both sides).  org-ql stays the only query path —
;; nothing here touches querying.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)     ; dashboard harness + frozen mtime
(require 'org-air-project-test)         ; project fixture root + --render
(require 'org-air-round48-test)         ; r48 project/fold-row helpers

(when (locate-library "org-air")
  (require 'org-air))

;;;; =====================================================================
;;;; Harness — board `…and N more' row helpers
;;;; =====================================================================

(defconst org-air-r51--more-label-re
  "and \\([0-9]+\\) more — press TAB on the title to expand"
  "The board fold row's byte-frozen label (R51-3 changes properties only).")

(defun org-air-r51--enclosing-bucket (pos)
  "Return the `org-air-section' bucket of the section enclosing POS.
Derived from the nearest section HEADER above — deliberately NOT from
the R51-3 `org-air-more-row' property, so the helper (and the trunk
failure it enables) works on both sides of the impl."
  (let ((change (previous-single-property-change pos 'org-air-section)))
    (and change
         (get-text-property (max (point-min) (1- change))
                            'org-air-section))))

(defun org-air-r51--more-row-pos (&optional bucket)
  "Return the position of a board `…and N more' fold row, or nil.
Found by the LABEL TEXT (present on both sides of the impl; R51-3 froze
the copy).  With BUCKET, only a row inside BUCKET's section."
  (save-excursion
    (goto-char (point-min))
    (let (found)
      (while (and (not found)
                  (re-search-forward org-air-r51--more-label-re nil t))
        (let ((pos (match-beginning 0)))
          (when (or (null bucket)
                    (eq (org-air-r51--enclosing-bucket pos) bucket))
            (setq found pos))))
      found)))

(defun org-air-r51--section-item-lines (bucket)
  "Return the buffer LINE numbers of BUCKET's visible item rows, in order.
Scans from BUCKET's section header to the next `org-air-section' header
\(or `point-max'); a line counts when it carries `org-air-item'."
  (let ((start nil) (pos (point-min)) lines)
    (while (and (not start)
                (setq pos (text-property-not-all pos (point-max)
                                                 'org-air-section nil)))
      (if (eq (get-text-property pos 'org-air-section) bucket)
          (setq start pos)
        (setq pos (or (next-single-property-change pos 'org-air-section)
                      (point-max)))))
    (should start)
    (save-excursion
      (goto-char start)
      (let* ((from (line-end-position))
             (end (or (text-property-not-all from (point-max)
                                              'org-air-section nil)
                      (point-max))))
        (goto-char from)
        (forward-line 1)
        (while (< (point) end)
          (when (text-property-not-all (point) (min (line-end-position) end)
                                       'org-air-item nil)
            (push (line-number-at-pos (point)) lines))
          (forward-line 1))))
    (nreverse lines)))

(defun org-air-r51--dispatch (key)
  "Dispatch KEY (a string for `kbd') through the LIVE `key-binding'.
Asserting through the real binding pins the knob-installed map — what a
user's key press actually runs — not a direct function call."
  (let ((cmd (key-binding (kbd key))))
    (should (commandp cmd))
    (call-interactively cmd)
    cmd))

(defun org-air-r51--assert-board-more-row-expands (key)
  "In the current board buffer, KEY on the first `…and N more' row expands.
Asserts the R51-3 contract: the row's bucket joins
`org-air-view--expanded-sections', THAT bucket's more row is gone (another
bucket's survives — per-bucket isolation — and carries the dispatch
handle), the section's visible item rows grow by EXACTLY the advertised
hidden count (anti-tautology), and point lands on the first newly-revealed
row's title.  The row is located by its LABEL and the bucket derived from
the section header, so on the pre-impl trunk the dispatch runs and the
failure is the diagnosed BEHAVIOUR: point drifts, no expansion."
  (let* ((mpos (org-air-r51--more-row-pos))
         (bucket (and mpos (org-air-r51--enclosing-bucket mpos)))
         (line (save-excursion
                 (goto-char mpos)
                 (buffer-substring-no-properties (line-beginning-position)
                                                 (line-end-position))))
         (hidden (progn (should (string-match org-air-r51--more-label-re
                                              line))
                        (string-to-number (match-string 1 line))))
         (before (org-air-r51--section-item-lines bucket)))
    ;; the fixture board really renders a capped section (R93: Upcoming
    ;; caps at 5 of 8) — the expansion below is observable.
    (should mpos)
    (should (symbolp bucket))
    (should bucket)
    (should (> hidden 0))
    (should-not org-air-view--expanded-sections)
    (goto-char mpos)
    (org-air-r51--dispatch key)
    ;; the bucket joined the expansion set — the TRUNK failure lands HERE
    ;; (TAB drifted to the next section header, RET ran the pane;
    ;; `org-air-view--expanded-sections' stayed nil) — and ITS more row
    ;; is GONE…
    (should (memq bucket org-air-view--expanded-sections))
    (should-not (org-air-r51--more-row-pos bucket))
    (let ((after (org-air-r51--section-item-lines bucket)))
      ;; …the section's visible item rows grew by EXACTLY the hidden
      ;; count (anti-tautology: not merely "some rows appeared")…
      (should (= (+ (length before) hidden) (length after)))
      ;; …and point sits on the FIRST newly-revealed row's title (the
      ;; revealed rows replace the more row — point stays put visually).
      (should (= (line-number-at-pos (point)) (nth (length before) after)))
      (should (org-air-view--row-property 'org-air-item))
      (should (= (point) (org-air-view--row-title-pos))))
    ;; per-bucket isolation: ANOTHER bucket's more row survives (the
    ;; width-100 fixture board caps two sections), and the surviving row
    ;; carries the R51-3 dispatch handle — `org-air-more-row' = its OWN
    ;; bucket over the row (the board twin of `org-air-dropped-fold') —
    ;; plus `mouse-face' over the text-only label.
    (let ((other (org-air-r51--more-row-pos)))
      (should other)
      (let ((handle (get-text-property other 'org-air-more-row)))
        (should handle)
        (should (eq handle (org-air-r51--enclosing-bucket other)))
        (should-not (eq handle bucket)))
      (should (get-text-property other 'mouse-face)))))

;;;; =====================================================================
;;;; r51-1 — the dropped faces carry NO strike-through
;;;; =====================================================================

(ert-deftest org-air-r51-1-dropped-faces-carry-no-strike ()
  "R51-1: BOTH dropped faces lose `:strike-through' entirely.
Applied as the row `font-lock-face' over the WHOLE row extent — title,
inter-column flex fill, trailing pad — a `:strike-through' face draws a
full-width horizontal RULE through the row (the user's screenshot), so
the attribute must be nil/unspecified (NEVER t) on the row face
`org-air-face-project-dropped' AND the badge face
`org-air-face-air-state-dropped'; each still inherits the dim
`org-air-face-faded' — grey survives as the SOLE dropped affordance.
Executing conjuncts pin the two seams the faces reach: the knob-nil dir
render still faces the Delta title with the row face (the R48-2 seam),
and the inspector State line faces its `Dropped' label with the badge
face.  Reverting R51-1 (re-adding `:strike-through t') FAILS."
  (skip-unless (locate-library "org-air"))
  (dolist (face '(org-air-face-project-dropped
                  org-air-face-air-state-dropped))
    (ert-info ((format "face %s" face))
      (should (facep face))
      ;; the strike is GONE: nil/unspecified, never t…
      (should (memq (face-attribute face :strike-through nil)
                    '(nil unspecified)))
      ;; …and the grey inherit STAYS (the sole affordance).
      (let ((inh (face-attribute face :inherit nil)))
        (should (memq 'org-air-face-faded
                      (if (listp inh) inh (list inh)))))))
  ;; Executing seam 1 (row face): knob-nil inline render — the dropped
  ;; Delta title still carries the (de-striked) row face via the R48-2
  ;; `org-air-project--doc-row-face' selector.
  (let ((org-air-project-collapse-dropped nil))
    (org-air-r48--with-project 'directory
      (let ((dropped (org-air-r48--doc-positions "dropped")))
        (should (= 1 (length dropped)))
        (should (eq 'org-air-face-project-dropped
                    (org-air-r48--title-face-at (car dropped)))))))
  ;; Executing seam 2 (badge face): the inspector State line for Delta —
  ;; the `Dropped' label the round calls out — faces via
  ;; `org-air-project--state-face' → the (de-striked) badge face.
  (org-air-r48--with-project 'directory
    (let* ((delta (seq-find (lambda (d)
                              (equal (org-air-doc-state d) "dropped"))
                            (org-air-project--collect-docs
                             org-air-project-test-root)))
           (state-line
            (seq-find (lambda (l)
                        (string-match-p "State"
                                        (substring-no-properties l)))
                      (org-air-project--inspector-doc-fields
                       delta "" 40 org-air-test-now))))
      (should delta)
      (should state-line)
      (let ((idx (string-match "Dropped" state-line)))
        (should idx)
        (should (eq 'org-air-face-air-state-dropped
                    (get-text-property idx 'face state-line)))))))

;;;; =====================================================================
;;;; r51-2 — dropped sorts AFTER the last draft (group bottom, both modes)
;;;; =====================================================================

(ert-deftest org-air-r51-2-dropped-sorts-after-last-draft ()
  "R51-2: dropped docs rank at the group BOTTOM in both comparators.
\(a) tag grouping (the round's one golden mover): the #ui section's rows
in exact buffer order are Alpha (ready), Zeta (wip), Epsilon (draft),
then the fold row — the draft sinks below the live states, dropped past
ALL of them.  Trunk FAILS (the lifecycle-order accident rendered Epsilon
first).  (b) dir grouping EXPANDED: the revealed Delta row's position is
GREATER than the draft Epsilon's AND the unknown Eta's — dropped after
the last draft and after every other state.  Trunk FAILS (Delta landed
directly after Zeta, mid-group).  (c) LOCK conjunct: collapsed default —
the `… 1 dropped' fold row is its group's LAST row before the next group
heading (or buffer end) in ALL THREE groupings."
  (skip-unless (locate-library "org-air"))
  ;; (a) TAG grouping: #ui rows in exact, consecutive buffer order.
  (org-air-r48--with-project 'tag
    (let* ((lines (split-string (buffer-string) "\n"))
           (i (cl-position-if (lambda (l) (string-match-p "| #ui 4" l))
                              lines)))
      (should i)
      (should (string-match-p "READY Alpha feature" (nth (+ i 1) lines)))
      (should (string-match-p "WIP +Zeta work in progress"
                              (nth (+ i 2) lines)))
      (should (string-match-p "DRAFT Epsilon plan" (nth (+ i 3) lines)))
      (should (string-match-p "1 dropped" (nth (+ i 4) lines)))
      ;; …and NOTHING follows the fold row inside the section: the next
      ;; line's left pane is blank (rail-only) or the buffer ends.
      (should (string-match-p "^[[:space:]]*\\(|\\|$\\)"
                              (or (nth (+ i 5) lines) "")))))
  ;; (b) DIR grouping, EXPANDED: the revealed dropped row ranks LAST —
  ;; after the draft AND after the unknown (dead sorts after broken).
  (org-air-r48--with-project 'directory
    (goto-char (car (org-air-r48--fold-positions)))
    (org-air-project-toggle-dropped)
    (should (= 1 (length (org-air-r48--doc-positions "dropped"))))
    (let* ((text (buffer-string))
           (delta (string-match "Delta UI exploration" text)))
      (should delta)
      (dolist (above '("Zeta work in progress" "Epsilon plan" "Eta notes"))
        (ert-info ((format "%s must render above Delta" above))
          (should (< (string-match above text) delta))))))
  ;; (c) LOCK: collapsed default — the fold row is the group's LAST row
  ;; (no doc row between it and the next group heading / buffer end).
  (dolist (group '(directory state tag))
    (ert-info ((format "grouping %s" group))
      (org-air-r48--with-project group
        (let* ((fold (car (org-air-r48--fold-positions)))
               (fold-eol (save-excursion (goto-char fold)
                                         (line-end-position)))
               (next-heading (or (text-property-not-all
                                  fold-eol (point-max)
                                  'org-air-section nil)
                                 (point-max))))
          (should fold)
          (should-not (text-property-not-all fold-eol next-heading
                                             'org-air-doc nil)))))))

;;;; =====================================================================
;;;; r51-3 — TAB (and RET) ON the fold rows expand them
;;;; =====================================================================

(ert-deftest org-air-r51-3-tab-ret-on-fold-rows-expand ()
  "R51-3: the fold rows are themselves actionable TAB/RET targets.
BOARD (trunk FAILS — the row carried no property, so TAB fell through
`org-air-toggle-section' to `org-air-next-section' and DRIFTED to the
next header with `org-air-view--expanded-sections' unchanged, while RET
ran the pane): with point ON the `…and N more — press TAB on the title
to expand' row, dispatching the REAL TAB binding expands that bucket —
it joins `org-air-view--expanded-sections', the more row is gone, the
section's visible item rows grow by EXACTLY N (anti-tautology), point
sits on the first newly-revealed row's title; on a FRESH render the REAL
RET binding on the more row performs the SAME expansion and ONLY that —
nothing opens, no pane.  PROJECT (lock — passes on trunk, guards the
affordance both sides): TAB and RET dispatched on the v0.2 `… 1 dropped'
fold row still reveal the hidden doc (visible docs +1, fold row gone)."
  (skip-unless (locate-library "org-air"))
  ;; R93: the per-bucket ISOLATION leg needs TWO capped sections.  The
  ;; standard fixture used to cap Needs attention (8 of 6, back when a
  ;; dateless item was nagged) and Upcoming; under the aging rule Needs
  ;; attention holds 5 rows under its cap of 6, so only Upcoming caps.
  ;; Tighten the GENERIC cap (`org-air-section-max', which Upcoming's own
  ;; 5 and Overdue/attention's own 6 all override) so High priority caps
  ;; too — a second capped section, from the knob rather than from a
  ;; corpus change that would move every board golden.
  (let ((org-air-section-max 2))
    ;; BOARD — TAB on the more row expands (the row finally does what its
    ;; own label teaches).
    (org-air-viewport-test-with-dashboard 100
      (org-air-r51--assert-board-more-row-expands "TAB"))
    ;; BOARD — RET on a FRESH render's more row: the same expansion, and
    ;; ONLY that — no doc opens, no source pane (the more-row branch runs
    ;; BEFORE the pane logic in `org-air-view-pane-return').
    (org-air-viewport-test-with-dashboard 100
      (org-air-r51--assert-board-more-row-expands "RET")
      (should-not (org-air-view-pane--window-live-p))
      (should-not (get-buffer "*org-air-view*"))))
  ;; PROJECT — LOCK (R48-3, no code this round): the `… 1 dropped' fold
  ;; row keeps answering both keys through the live bindings.
  (dolist (key '("TAB" "RET"))
    (ert-info ((format "project fold row answers %s" key))
      (org-air-r48--with-project 'directory
        (let ((before (length (org-air-r48--doc-positions))))
          (should (org-air-r48--fold-keys))
          (goto-char (car (org-air-r48--fold-positions)))
          (org-air-r51--dispatch key)
          ;; the hidden dropped doc is revealed: +1 visible doc, no fold.
          (should (= (1+ before) (length (org-air-r48--doc-positions))))
          (should (= 1 (length (org-air-r48--doc-positions "dropped"))))
          (should-not (org-air-r48--fold-keys)))))))

(provide 'org-air-round51-test)
;;; org-air-round51-test.el ends here
