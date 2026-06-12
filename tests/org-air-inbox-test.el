;;; org-air-inbox-test.el --- contract tests for the inbox workflow -*- lexical-binding: t; -*-

;;; Commentary:
;; Contract tests for the frozen round-1 inbox API:
;;   org-air-inbox-file, org-air-capture, org-air-refile-item.
;; Guarded with skip-unless until the implementation lands.

;;; Code:

(require 'ert)
(require 'org-air-test-helpers)

(when (locate-library "org-air")
  (require 'org-air))

(ert-deftest org-air-inbox-api-present ()
  "The frozen inbox API exists."
  (skip-unless (locate-library "org-air"))
  (should (boundp 'org-air-inbox-file))
  (should (fboundp 'org-air-capture))
  (should (fboundp 'org-air-refile-item)))

(ert-deftest org-air-inbox-capture-is-a-command ()
  "Capture is an interactive entry point."
  (skip-unless (locate-library "org-air"))
  (should (commandp 'org-air-capture)))

(ert-deftest org-air-inbox-file-is-customizable ()
  "`org-air-inbox-file' is a user option (defcustom)."
  (skip-unless (locate-library "org-air"))
  (should (custom-variable-p 'org-air-inbox-file)))

(ert-deftest org-air-inbox-items-are-queried ()
  "Items in the inbox file are part of the query results."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (let ((items (org-air-query-items)))
      (should (org-air-test-find-item "Call plumber" items))
      (should (org-air-test-find-item "Quick note about org-air idea"
                                      items)))))

(ert-deftest org-air-inbox-items-classify-as-inbox ()
  "Every item from the inbox file carries the inbox bucket."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (dolist (title '("Call plumber" "Triage me later"))
      (let* ((items (org-air-query-items))
             (item (org-air-test-find-item title items)))
        (should item)
        (should (memq 'inbox
                      (org-air-classify-item item org-air-test-now)))))))

(ert-deftest org-air-inbox-non-inbox-items-not-inbox ()
  "Items outside the inbox file do not carry the inbox bucket."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (let* ((items (org-air-query-items))
           (item (org-air-test-find-item "Prepare standup notes" items)))
      (should item)
      (should-not (memq 'inbox
                        (org-air-classify-item item org-air-test-now))))))

(provide 'org-air-inbox-test)
;;; org-air-inbox-test.el ends here
