;;; org-air-classify-test.el --- contract tests for classification -*- lexical-binding: t; -*-

;;; Commentary:
;; Contract tests for `org-air-classify-item' (frozen round-1 API).
;; Deterministic: every call passes the fixed `org-air-test-now'
;; (Mon 2026-06-15 10:00) — see tests/org-air-test-helpers.el for how the
;; fixture dates relate to it.  Guarded with skip-unless until the
;; implementation lands.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-air-test-helpers)

(when (locate-library "org-air")
  (require 'org-air))

(defun org-air-classify-test--buckets (title)
  "Query the fixtures and classify the item whose title contains TITLE."
  (let* ((items (org-air-query-items))
         (item (org-air-test-find-item title items)))
    (should item)
    (org-air-classify-item item org-air-test-now)))

(ert-deftest org-air-classify-api-present ()
  "The frozen classification API exists."
  (skip-unless (locate-library "org-air"))
  (should (fboundp 'org-air-classify-item)))

(ert-deftest org-air-classify-returns-bucket-symbols ()
  "Classification returns a list drawn from the documented buckets."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (let ((buckets (org-air-classify-test--buckets "Prepare standup notes")))
      (should (listp buckets))
      (dolist (b buckets)
        (should (memq b '(upcoming stale attention high-priority inbox
                          notes knowledge journal)))))))

(ert-deftest org-air-classify-upcoming ()
  "An item scheduled for tomorrow is upcoming."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (should (memq 'upcoming
                  (org-air-classify-test--buckets "Prepare standup notes")))))

(ert-deftest org-air-classify-overdue-is-attention ()
  "An item whose deadline passed needs attention."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (should (memq 'attention
                  (org-air-classify-test--buckets
                   "Fix production outage runbook")))
    (should (memq 'attention
                  (org-air-classify-test--buckets
                   "Book dentist appointment")))))

(ert-deftest org-air-classify-no-schedule-is-attention ()
  "An item with no schedule or deadline needs attention."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (should (memq 'attention
                  (org-air-classify-test--buckets
                   "Untracked idea with no dates")))))

(ert-deftest org-air-classify-high-priority ()
  "[#A] items are high-priority; [#C] items are not."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (should (memq 'high-priority
                  (org-air-classify-test--buckets "Ship quarterly report")))
    (should (memq 'high-priority
                  (org-air-classify-test--buckets
                   "Fix production outage runbook")))
    (should-not (memq 'high-priority
                      (org-air-classify-test--buckets
                       "Low priority cleanup")))))

(ert-deftest org-air-classify-stale ()
  "DATED items whose last activity is months old are stale (R54-1 retune).
Staleness is gated on an actionable date now (scheduled / deadline /
active <ts>), so the two old dateless cases — \"Dust off old archive
project\" and \"Learn lute\", which carry ONLY inactive [ts] stamps —
are no longer stale-ELIGIBLE (spec semantics table: inactive-[ts]-only
=> never Stale).  The positive cases live on DATED scratch items
appended to the fixture copy: a SCHEDULED two months past and a bare
active <ts> three months past, both quiet beyond `org-air-stale-days'."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    ;; Append the dated-but-quiet items to the SCRATCH copy (the
    ;; canonical corpus deliberately renders Stale 0 after R54-1).
    (let ((scratch (expand-file-name
                    "someday.org" (file-name-directory org-air-inbox-file))))
      (with-temp-buffer
        (insert "\n* TODO Revive the archived migration plan       :archive:\n"
                "SCHEDULED: <2026-04-16 Thu>\n"
                "Scheduled two months before the frozen now, untouched since.\n"
                "\n* TODO Lapsed lute practice log                   :hobby:\n"
                "Last session <2026-03-15 Sun> — an ACTIVE stamp, months quiet.\n")
        (append-to-file (point-min) (point-max) scratch)))
    ;; Dated + quiet => stale (the clock is unchanged for dated items).
    (should (memq 'stale
                  (org-air-classify-test--buckets
                   "Revive the archived migration plan")))
    (should (memq 'stale
                  (org-air-classify-test--buckets
                   "Lapsed lute practice log")))
    ;; R54-1 inversion: inactive-[ts]-only items are NEVER stale now.
    (should-not (memq 'stale
                      (org-air-classify-test--buckets
                       "Dust off old archive project")))
    (should-not (memq 'stale (org-air-classify-test--buckets "Learn lute")))))

(ert-deftest org-air-classify-not-stale-when-active ()
  "An item scheduled in the near future is not stale."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (should-not (memq 'stale
                      (org-air-classify-test--buckets
                       "Prepare standup notes")))))

(ert-deftest org-air-classify-inbox ()
  "Items living in `org-air-inbox-file' are classified as inbox."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (should (memq 'inbox (org-air-classify-test--buckets "Call plumber")))))

(ert-deftest org-air-classify-deterministic ()
  "Classifying the same item twice with the same `now' is stable."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (should (equal (org-air-classify-test--buckets "Ship quarterly report")
                   (org-air-classify-test--buckets "Ship quarterly report")))))

(provide 'org-air-classify-test)
;;; org-air-classify-test.el ends here
