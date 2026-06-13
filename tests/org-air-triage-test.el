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
  "Every triage disposition key is bound to its spec'd command."
  (skip-unless (locate-library "org-air"))
  (pcase-dolist (`(,key . ,command)
                 '(("s" . org-air-item-schedule)
                   ("d" . org-air-item-deadline)
                   ("r" . org-air-refile-item)
                   ("f" . org-air-item-file-group)
                   ("t" . org-air-set-tag)
                   ("T" . org-air-item-cycle-todo)
                   ("a" . org-air-item-archive)
                   ("D" . org-air-item-done)
                   ("k" . org-air-item-kill)
                   ("I" . org-air-process-inbox)))
    (ert-info ((format "key %s -> %s" key command))
      (should (eq (lookup-key org-air-view-mode-map (kbd key)) command)))))

(ert-deftest org-air-triage-keymap-scope-remap ()
  "Scope moves off the prime key: S = scope, M-s = scope-clear, and s
is no longer scope (it is the schedule disposition)."
  (skip-unless (locate-library "org-air"))
  (should (eq (lookup-key org-air-view-mode-map (kbd "S")) 'org-air-scope))
  (should (eq (lookup-key org-air-view-mode-map (kbd "M-s")) 'org-air-scope-clear))
  (should-not (eq (lookup-key org-air-view-mode-map (kbd "s")) 'org-air-scope)))

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
day must be backed by an item rendered under Upcoming/Needs attention,
never by a row-less phantom.  Data-variation board, GUI glyphs."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-alt-dashboard 120
      (pcase-let ((`(,marked . ,_today)
                   (org-air-viewport-test-calendar-marks)))
        (let* ((rows (org-air-triage-test--section-rows))
               (bucket-days ()))
          ;; June days of items actually rendered in a date bucket.
          (pcase-dolist (`(,bucket . ,item) rows)
            (when (memq bucket '(upcoming attention))
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
  "Dated-unfiled inbox rows carry the 'scheduled · file with r' hint
(ruling xsqrnoyn) so the user knows dating did not file the item."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-alt-dashboard 160
    (let ((found nil))
      (save-excursion
        (goto-char (point-min))
        (when (search-forward "Dated inbox capture" nil t)
          (setq found (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position)))))
      (should found)
      (should (string-match-p "file with r" found)))))

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
  "The header band reserves one right-margin column: its right-trimmed
width is exactly W-1, and the status text is intact (no clipped
'item‥').  Aesthetics spec S7."
  (skip-unless (locate-library "org-air"))
  (dolist (width '(80 120 160))
    (ert-info ((format "width %d" width))
      (org-air-viewport-test-with-dashboard width
        (let ((header (car (org-air-viewport-test-lines))))
          (should (string-match-p "[0-9]+ items" header))
          (should (= (string-width (string-trim-right header))
                     (1- width))))))))

;;;; S8 — mechanism only (pixel effect is GUI-only, untestable in batch).

(ert-deftest org-air-s8-line-spacing-zero-buffer-local ()
  "org-air-view-mode sets line-spacing buffer-locally to 0 so divider
glyphs touch into a continuous frame.  Only the MECHANISM is asserted;
the pixel-level stroke continuity needs a real GUI frame."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 120
    (should (local-variable-p 'line-spacing))
    (should (eql line-spacing 0))))

(provide 'org-air-triage-test)
;;; org-air-triage-test.el ends here
