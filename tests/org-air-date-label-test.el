;;; org-air-date-label-test.el --- date-label overdue regression tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression suite for the OVERDUE sign inversion in
;; `org-air-view--date-label' (design-review-r3 HIGH): the original code
;; tested (< (days-between TIME now) 0) but `days-between' returns
;; days(now) - days(TIME), so PAST dates yield a POSITIVE delta.  The
;; inverted test rendered future deadlines as "OVERDUE Nd" and past
;; deadlines as benign human dates.
;;
;; Per the design spec (§4, item line "date" zone):
;;   - future schedule/deadline → humanised date, org-air-face-deadline /
;;     org-air-face-scheduled
;;   - overdue → "OVERDUE Nd" label, org-air-face-overdue
;;
;; The clock is frozen to `org-air-test-now' (Mon 2026-06-15) by
;; rebinding `current-time' around the calls, since the label helper
;; reads the live clock internally.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-air-test-helpers)

(when (locate-library "org-air")
  (require 'org-air))

(defmacro org-air-date-test--frozen (&rest body)
  "Run BODY with `current-time' frozen to `org-air-test-now'."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'current-time)
              (lambda () org-air-test-now)))
     ,@body))

(defun org-air-date-test--label (title)
  "Return the (LABEL . FACE) date metadata for the fixture item TITLE."
  (let* ((items (org-air-query-items))
         (item (org-air-test-find-item title items)))
    (should item)
    (org-air-date-test--frozen
      (org-air-view--date-label
       item (car (org-air-classify-item item org-air-test-now))))))

(ert-deftest org-air-date-future-deadline-is-benign ()
  "A future deadline renders a human date, never OVERDUE.
\"Prep client presentation\" has DEADLINE 2026-06-16, one day after the
frozen now."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (org-air-test-with-fixtures
    (pcase-let ((`(,label . ,face)
                 (org-air-date-test--label "Prep client presentation")))
      (should-not (string-match-p "OVERDUE" label))
      (should (equal label "Tomorrow"))
      (should (eq face 'org-air-face-deadline)))))

(ert-deftest org-air-date-past-deadline-is-overdue ()
  "A past deadline renders \"OVERDUE Nd\" with the overdue face.
\"Fix production outage runbook\" has DEADLINE 2026-06-10, five days
before the frozen now."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (org-air-test-with-fixtures
    (pcase-let ((`(,label . ,face)
                 (org-air-date-test--label "Fix production outage runbook")))
      (should (equal label "OVERDUE 5d"))
      (should (eq face 'org-air-face-overdue)))))

(ert-deftest org-air-date-past-scheduled-is-overdue ()
  "A past SCHEDULED (no deadline) renders \"OVERDUE Nd\" too.
\"Chase missing invoice\" is SCHEDULED 2026-06-08, seven days before the
frozen now."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (org-air-test-with-fixtures
    (pcase-let ((`(,label . ,face)
                 (org-air-date-test--label "Chase missing invoice")))
      (should (equal label "OVERDUE 7d"))
      (should (eq face 'org-air-face-overdue)))))

(ert-deftest org-air-date-future-scheduled-is-benign ()
  "A future SCHEDULED renders a human date with the scheduled face.
\"Prepare standup notes\" is SCHEDULED 2026-06-16."
  (skip-unless (and (locate-library "org-air")
                    (fboundp 'org-air-view--date-label)))
  (org-air-test-with-fixtures
    (pcase-let ((`(,label . ,face)
                 (org-air-date-test--label "Prepare standup notes")))
      (should-not (string-match-p "OVERDUE" label))
      (should (equal label "Tomorrow"))
      (should (eq face 'org-air-face-scheduled)))))

(ert-deftest org-air-date-dashboard-overdue-rendering ()
  "End-to-end: the dashboard shows OVERDUE only on genuinely past items."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (unwind-protect
        ;; Render wide so V6's fixed metadata table leaves the full title
        ;; (and its OVERDUE token) on one untruncated line.
        (let ((org-air-view-width 160))
          (org-air-date-test--frozen (org-air))
          (with-current-buffer "*org-air*"
            (let ((text (buffer-string)))
              ;; Past deadline carries the OVERDUE label on its line...
              (should (string-match-p
                       "Fix production outage runbook.*OVERDUE" text))
              ;; ...and the future deadline does not.
              (let ((case-fold-search nil))
                (should-not (string-match-p
                             "Prep client presentation[^\n]*OVERDUE" text))))))
      (when (get-buffer "*org-air*")
        (kill-buffer "*org-air*")))))

(provide 'org-air-date-label-test)
;;; org-air-date-label-test.el ends here
