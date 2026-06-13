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
                   ("T" . org-air-item-todo)
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

(ert-deftest org-air-triage-dated-inbox-item-graduates ()
  "An inbox-file item WITH a SCHEDULED date is not in the Inbox bucket.
Triage spec: Inbox == unprocessed == inbox-home AND no SCHEDULED, no
DEADLINE.  The dated item joins its date bucket instead."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    ;; Date an existing inbox capture: tomorrow relative to the frozen
    ;; clock (Mon 15 Jun 2026), squarely inside the upcoming window.
    (with-current-buffer (find-file-noselect org-air-inbox-file)
      (goto-char (point-max))
      (insert "\n* TODO Dated capture under test\nSCHEDULED: <2026-06-16 Tue>\n")
      (save-buffer))
    (let* ((items (org-air-query-items))
           (item (org-air-test-find-item "Dated capture under test" items)))
      (should item)
      (let ((buckets (org-air-classify-item item org-air-test-now)))
        (should-not (memq 'inbox buckets))
        (should (memq 'upcoming buckets))))))

;;;; Render-level consistency target (screenshot-3 finding 1).

(ert-deftest org-air-triage-inbox-section-only-undated ()
  "The rendered Inbox section contains only undated items.
This is the calendar-vs-bucket consistency resolution: dated captures
graduate out of Inbox, so every calendar dot belongs to an item visible
in a date-driven section.  Exercised on the data-variation board, whose
'Dated inbox capture' (SCHEDULED Jun 23) must NOT appear under Inbox."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-alt-dashboard 120
    (let ((in-inbox nil) (saw-inbox nil) (dated-in-inbox ()))
      ;; Walk line by line: section headings and item rows are lines.
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((bol (line-beginning-position))
                 (section (get-text-property bol 'org-air-section))
                 (item (get-text-property bol 'org-air-item)))
            (when section
              (setq in-inbox (eq section 'inbox))
              (when in-inbox (setq saw-inbox t)))
            (when (and in-inbox item
                       (or (org-air-item-scheduled item)
                           (org-air-item-deadline item)))
              (cl-pushnew (org-air-item-title item) dated-in-inbox
                          :test #'equal)))
          (forward-line 1)))
      ;; Guard against vacuity: the Inbox section must have been seen.
      (should saw-inbox)
      (should (null dated-in-inbox)))))

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
