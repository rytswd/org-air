;;; org-air-triage-test.el --- triage flow + S7/S8 spec surface -*- lexical-binding: t; -*-

;;; Commentary:
;; Spec-true surface for the inbox triage round
;; (air/v0.2/org-air-triage.org, #+state: ready) and the S7/S8 tuning
;; (air/v0.2/org-air-aesthetics.org).  Written ahead of impl2's landing;
;; failing tests are manifested as the grind punch list.
;;
;; Covered here (batch-testable):
;;  - disposition keymap (s/d/r/f/t/T/a/D/k/I/u) + the s->schedule,
;;    scope->S, scope-clear->M-s remap;
;;  - graduation semantics at the classification level: an inbox-file
;;    item WITH a date is NOT in the Inbox bucket;
;;  - the render-level consistency target (screenshot-3 finding 1, as
;;    resolved by the triage spec): the Inbox section contains only
;;    undated items — dated captures graduate to their date bucket, so
;;    calendar dots always correspond to section-visible items;
;;  - org-air-process-inbox exists as an interactive entry point;
;;  - S7: the header band's right status ends at W-1, intact;
;;  - S8 mechanism: line-spacing is buffer-locally 0 in the dashboard.
;;
;; NOT testable in batch (documented, GUI-only): S8's pixel effect
;; (continuous │ strokes need real glyph rendering); process-inbox's
;; transient bar/dimming visuals; quick-date sub-prompt interaction
;; (behavioural tests follow impl2's command shapes).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)

(when (locate-library "org-air")
  (require 'org-air))

;;;; Keymap — disposition vocabulary + the s/S remap.

(ert-deftest org-air-triage-keymap-dispositions ()
  "Every top-level disposition key is bound to its spec'd command.
Round-7 (R3/R5) reclaimed two prime keys: `s' is now scope (schedule
moved into the process-inbox guided flow) and `k' is now a motion key
(kill relocated to the guarded `x').  The remaining dispositions keep
their keys."
  (skip-unless (locate-library "org-air"))
  (pcase-dolist (`(,key . ,command)
                 '(("d" . org-air-item-set-deadline)
                   ("e" . org-air-refile-item)
                   ("f" . org-air-item-file-group)
                   ("t" . org-air-set-tag)
                   ("T" . org-air-item-cycle-todo)
                   ("a" . org-air-item-archive)
                   ("D" . org-air-item-done)
                   ("x" . org-air-item-kill)
                   ("I" . org-air-process-inbox)))
    (ert-info ((format "key %s -> %s" key command))
      (should (eq (lookup-key org-air-view-mode-map (kbd key)) command))))
  ;; `k' must NOT be the kill key any more (R3 safety: it is a motion).
  (should-not (eq (lookup-key org-air-view-mode-map (kbd "k"))
                  'org-air-item-kill)))

(ert-deftest org-air-triage-keymap-scope-remap ()
  "Round-7 R5: scope is back on the prime key — s = scope, S = scope-clear
(the round-4 s→S remap is superseded; schedule now lives in the
process-inbox flow)."
  (skip-unless (locate-library "org-air"))
  (should (eq (lookup-key org-air-view-mode-map (kbd "s")) 'org-air-scope))
  (should (eq (lookup-key org-air-view-mode-map (kbd "S")) 'org-air-scope-clear)))

;;;; Graduation semantics (classification level).

(ert-deftest org-air-triage-dated-inbox-dual-membership ()
  "A dated inbox capture has DUAL membership: Inbox AND its date bucket.
Design ruling xsqrnoyn (real-signal, option a): dating an inbox item
does NOT graduate it — graduation is filing only (refile/archive/done/
delete); the item shows in Inbox and in its real date bucket."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    ;; Date an inbox capture: tomorrow relative to the frozen clock
    ;; (Mon 15 Jun 2026), squarely inside the upcoming window.
    (with-current-buffer (find-file-noselect org-air-inbox-file)
      (goto-char (point-max))
      (insert "\n* TODO Dated capture under test\nSCHEDULED: <2026-06-16 Tue>\n")
      (save-buffer))
    (let* ((items (org-air-query-items))
           (item (org-air-test-find-item "Dated capture under test" items)))
      (should item)
      (let ((buckets (org-air-classify-item item org-air-test-now)))
        (should (memq 'inbox buckets))
        (should (memq 'upcoming buckets))))))

;;;; Render-level consistency (screenshot-3 finding 1, ruling xsqrnoyn).

(defun org-air-triage-test--section-rows ()
  "Line-walk the render: alist of (BUCKET . ITEM) actually rendered.
The heading carries `org-air-section' and item rows carry `org-air-item',
but both sit past the left margin (and the item hint trails on the
right), so each line is scanned for the properties rather than read at
its first column only."
  (let ((current nil) (rows ()))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((bol (line-beginning-position))
              (eol (line-end-position))
              (section nil) (item nil))
          (let ((p bol))
            (while (< p eol)
              (let ((s (get-text-property p 'org-air-section))
                    (it (get-text-property p 'org-air-item)))
                (when (and s (not section)) (setq section s))
                (when (and it (not item)) (setq item it)))
              (setq p (1+ p))))
          (when section (setq current section))
          (when (and current item)
            (push (cons current item) rows)))
        (forward-line 1)))
    (nreverse rows)))

(ert-deftest org-air-triage-dated-inbox-row-in-both-sections ()
  "The variation board's dated capture renders under Inbox AND Upcoming.
Dual membership made visible (ruling xsqrnoyn); S4 suppression narrows
to the no-date attention default only."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-alt-dashboard 120
    (let* ((rows (org-air-triage-test--section-rows))
           (sections (cl-loop for (bucket . item) in rows
                              when (string-match-p
                                    "Dated inbox capture"
                                    (or (org-air-item-title item) ""))
                              collect bucket)))
      (should (memq 'inbox sections))
      (should (memq 'upcoming sections)))))

(ert-deftest org-air-triage-calendar-marks-have-date-bucket-rows ()
  "Every calendar mark corresponds to a visible row in some DATE bucket.
The ruled calendar↔bucket consistency invariant (xsqrnoyn): a dotted
day must be backed by an item rendered under one of the DATE sections,
never by a row-less phantom.  Data-variation board, GUI glyphs.
R93: the date sections are Overdue and Upcoming.  Needs attention is no
longer one of them -- it is a RECENCY section now, and its rows need
carry no date at all -- while Overdue, which used to be a disjunct
inside it, is where every past date lands.  Naming the old pair here
would have let an overdue mark be backed by a Needs-attention row that
surfaced for being quiet, which is exactly the phantom this invariant
exists to forbid."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-alt-dashboard 120
      (pcase-let ((`(,marked . ,_today)
                   (org-air-viewport-test-calendar-marks)))
        (let* ((rows (org-air-triage-test--section-rows))
               (bucket-days ()))
          ;; June days of items actually rendered in a date bucket.
          (pcase-dolist (`(,bucket . ,item) rows)
            (when (memq bucket '(overdue upcoming))
              (dolist (ts (list (org-air-item-scheduled item)
                                (org-air-item-deadline item)))
                (when ts
                  (let ((time (ignore-errors (org-timestamp-to-time ts))))
                    (when time
                      (let ((d (decode-time time)))
                        (when (and (= (decoded-time-year d) 2026)
                                   (= (decoded-time-month d) 6))
                          (cl-pushnew (decoded-time-day d) bucket-days)))))))))
          (dolist (day marked)
            (ert-info ((format "calendar mark on June %d" day))
              (should (memq day bucket-days)))))))))

(ert-deftest org-air-triage-dated-inbox-row-carries-file-hint ()
  "R26-6 deliberate INVERSION of R19-2(c): dated-unfiled inbox rows carry
NO row hint any more — the `· r to file' nudge is retired (user: wasteful
+ cryptic; it also broke the row's V6 tag/origin alignment).  The date
cell is the date label + optional repeat marker ONLY; `e' (R70-1: the
editor key) stays bound to `org-air-refile-item' and `?' help — the
R50-2 `*org-air-help*' BUFFER now, not an echo-area `message' — is the
single teaching surface (the discovery guarantee relocated into the
buffer text)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-alt-dashboard 160
    (let ((found nil))
      (save-excursion
        (goto-char (point-min))
        (when (search-forward "Dated inbox capture" nil t)
          (setq found (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position)))))
      (should found)
      ;; R26-6: NO row hint — neither wording generation survives, anywhere
      ;; on the board.
      (should-not (string-match-p "r to file" found))
      (should-not (string-match-p "file with r" found))
      (should-not (string-match-p "r to file\\|file with r"
                                  (buffer-substring-no-properties
                                   (point-min) (point-max))))
      ;; discovery lives on the key + help, not the row.
      (should (eq (lookup-key org-air-view-mode-map (kbd "e"))
                  'org-air-refile-item))
      ;; R50-2: `?' renders the *org-air-help* buffer (echo line deleted);
      ;; the editor row is there, key derived from the live board map.
      (save-window-excursion
        (unwind-protect
            (progn
              (org-air-help)             ; origin = this board buffer
              (let ((help (get-buffer org-air-help-buffer-name)))
                (should help)
                (should (string-match-p
                         "^  e +edit"
                         (with-current-buffer help
                           (substring-no-properties (buffer-string)))))))
          (when (get-buffer org-air-help-buffer-name)
            (kill-buffer org-air-help-buffer-name)))))))

;;;; Consistency invariant (ruling xsqrnoyn): calendar <-> buckets <-> total.

(ert-deftest org-air-triage-consistency-summary-total-is-unique-count ()
  "The summary =total= is the UNIQUE item count, even under dual
membership (ruling xsqrnoyn): the dated inbox capture renders in BOTH
Inbox and Upcoming, yet it is counted once.  So the rendered total
equals the number of distinct items, never the sum of section badges."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-alt-dashboard 120
      (let* ((rows (org-air-triage-test--section-rows))
             ;; Distinct items actually rendered, keyed by title.
             (titles (delete-dups
                      (delq nil (mapcar (lambda (r) (org-air-item-title (cdr r)))
                                        rows))))
             ;; A title that shows up under more than one bucket proves
             ;; dual membership is live (not just classified).
             (dual (cl-remove-if-not
                    (lambda (title)
                      (> (length (delete-dups
                                  (cl-loop for (b . it) in rows
                                           when (equal (org-air-item-title it) title)
                                           collect b)))
                         1))
                    titles)))
        ;; Dual membership is visible on this board.
        (should (member "Dated inbox capture" dual))
        ;; The summary total equals the unique item count, NOT the sum
        ;; of the per-section badges (which over-counts the dual item).
        (should (string-match-p
                 (format "%d\\s-+total"
                         (length org-air-viewport-test-alt-items))
                 (buffer-string)))
        ;; And the distinct rendered titles never exceed that total.
        (should (<= (length titles)
                    (length org-air-viewport-test-alt-items)))))))

;;;; Process-inbox entry point.

(ert-deftest org-air-triage-process-inbox-command-exists ()
  "org-air-process-inbox is an interactive command."
  (skip-unless (locate-library "org-air"))
  (should (commandp 'org-air-process-inbox)))

;;;; S7 — right status ends at W-1, intact.

(ert-deftest org-air-s7-header-status-ends-at-w-minus-1 ()
  "R39-1: the header band reserves a symmetric right gutter equal to the
left indent (`org-air-view--banner-indent') — its right-trimmed width
equals the compose width W minus that gutter (the status' last glyph sits
`banner-indent' columns before the final usable column, mirroring the
left `  org-air' indent), and the status text is intact (no clipped
'item‥').  The S7 spare column is supplied UPSTREAM by R34's fringe-aware
`org-air-layout--usable-columns'.  Aesthetics spec S7 (R36-1/R39-1)."
  (skip-unless (locate-library "org-air"))
  (dolist (width '(80 120 160))
    (ert-info ((format "width %d" width))
      (org-air-viewport-test-with-dashboard width
        (let ((header (car (org-air-viewport-test-lines))))
          (should (string-match-p "[0-9]+ items" header))
          ;; R39-1 symmetric gutter: trimmed width == W - banner-indent,
          ;; and no line exceeds W.
          (should (= (string-width (string-trim-right header))
                     (- width org-air-view--banner-indent)))
          (should (<= (string-width header) width)))))))

;;;; S8 — mechanism only (pixel effect is GUI-only, untestable in batch).

(ert-deftest org-air-s8-line-spacing-zero-buffer-local ()
  "org-air-view-mode sets `line-spacing' buffer-locally from the
`org-air-line-spacing' defcustom.  D-P3 [display] re-bless: the default
is back to 0 — a glyph divider at `line-spacing' 0 is continuous
(round-11's 0.15 opened a pixel gap below every row that broke the `│'
divider into a dashed line); the capsule \"breathing\" moved INTO the pill
via `org-air-pill-vinset' instead, so the cell grid + divider stay solid.
The round-11 0.15 default is REVERSED (air/v0.4/org-air-round12-design.org
§D-P3).  Only the MECHANISM is asserted (the pixel effect needs a real
GUI frame)."
  (skip-unless (locate-library "org-air"))
  (should (boundp 'org-air-line-spacing))
  (should (eql org-air-line-spacing 0))
  (org-air-viewport-test-with-dashboard 120
    (should (local-variable-p 'line-spacing))
    (should (eql line-spacing org-air-line-spacing))))

(provide 'org-air-triage-test)
;;; org-air-triage-test.el ends here
