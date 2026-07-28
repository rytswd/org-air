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
        ;; R93 vocabulary: `overdue' is a bucket of its own and `stale'
        ;; is retired.
        (should (memq b '(upcoming overdue attention high-priority inbox
                          backlog notes container knowledge journal)))))))

(ert-deftest org-air-classify-upcoming ()
  "An item scheduled for tomorrow is upcoming."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (should (memq 'upcoming
                  (org-air-classify-test--buckets "Prepare standup notes")))))

(ert-deftest org-air-classify-overdue-is-its-own-bucket ()
  "An item whose deadline passed is OVERDUE, and overdue alone (R93).
Pre-R93 the overdue rule was a disjunct of Needs attention; R93 split it
into its own bucket and its own section, because a missed date and a
quiet item are different problems.  A freshly-touched overdue item is
therefore NOT in Needs attention -- being late is not the same as having
gone quiet -- while an overdue `#A' still is, on the priority rule
alone (threshold 0), not on its lateness."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (let ((runbook (org-air-classify-test--buckets
                    "Fix production outage runbook"))
          (dentist (org-air-classify-test--buckets
                    "Book dentist appointment")))
      (should (memq 'overdue runbook))
      (should (memq 'overdue dentist))
      ;; No cookie, freshly-touched: overdue is the WHOLE story.
      (should (equal '(overdue) dentist))
      ;; `#A': attention comes from the threshold-0 rule, not from the
      ;; missed date.
      (should (memq 'high-priority runbook))
      (should (memq 'attention runbook)))))

(ert-deftest org-air-classify-no-schedule-is-not-attention-until-quiet ()
  "A dateless item needs attention when it goes QUIET, never for being dateless.
The R93 problem statement, as a test: \"everything would be sent to
backlog because that would be the easiest to stop seeing them\".  A
dateless item that is fresh is invisible; the SAME item once it has been
quiet past its threshold surfaces.  Nothing about the item's dates
changes between the two halves -- only the clock."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    ;; Fresh and dateless: not nagged.  (Pre-R93 this was `(attention)'.)
    (should-not (memq 'attention
                      (org-air-classify-test--buckets
                       "Untracked idea with no dates")))
    ;; Dateless and quiet since last autumn: nagged.
    (should (memq 'attention
                  (org-air-classify-test--buckets
                   "Dust off old archive project")))
    (should (memq 'attention
                  (org-air-classify-test--buckets "Learn lute")))))

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

(ert-deftest org-air-classify-stale-is-retired ()
  "The `stale' bucket no longer exists; its rule is subsumed (R93).
Stale was \"has a date AND has been quiet >= 21 days\".  Once EVERY
board item ages, dated or not, that is a strictly narrower restatement
of Needs attention wearing a second name and a second knob, so R93
retired it.  The old positive cases -- dated and months quiet -- must
still surface, now as `attention'; nothing may still answer `stale'."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    ;; Append the dated-but-quiet items to the SCRATCH copy.  Each
    ;; carries a body inactive stamp months old: under R93 the recency
    ;; clock, not the R54-1 eligibility gate, is what surfaces them.
    (let ((scratch (expand-file-name
                    "someday.org" (file-name-directory org-air-inbox-file))))
      (with-temp-buffer
        (insert "\n* TODO Revive the archived migration plan       :archive:\n"
                "SCHEDULED: <2026-04-16 Thu>\n"
                "Scheduled two months before the frozen now, untouched since.\n"
                "[2026-04-16 Thu 09:00]\n"
                "\n* TODO Lapsed lute practice log                   :hobby:\n"
                "Last session <2026-03-15 Sun> — an ACTIVE stamp, months quiet.\n"
                "[2026-03-15 Sun 20:00]\n")
        (append-to-file (point-min) (point-max) scratch)))
    (dolist (title '("Revive the archived migration plan"
                     "Lapsed lute practice log"
                     ;; R54-1 called these "never stale" (inactive-[ts]
                     ;; only, so not date-ELIGIBLE).  R93 reads exactly
                     ;; those stamps as the recency clock, so they are
                     ;; the plainest Needs-attention rows on the board.
                     "Dust off old archive project"
                     "Learn lute"))
      (let ((buckets (org-air-classify-test--buckets title)))
        (ert-info ((format "%s => %S" title buckets))
          (should (memq 'attention buckets))
          (should-not (memq 'stale buckets)))))
    ;; And the bucket symbol is gone from the whole board, not just here.
    (dolist (item (org-air-query-items))
      (should-not (memq 'stale (org-air-classify-item item org-air-test-now))))
    (should-not (fboundp 'org-air-classify--stale-p))
    (should-not (boundp 'org-air-stale-days))))

(ert-deftest org-air-classify-fresh-and-scheduled-is-not-attention ()
  "An item scheduled in the near future and freshly touched stays quiet.
The R93 successor of `org-air-classify-not-stale-when-active': a plan is
not an update, so a SCHEDULED date neither starts nor stops the clock --
this item is off Needs attention because it is FRESH, and it would
surface if it went quiet even with that schedule in place."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (should-not (memq 'attention
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
