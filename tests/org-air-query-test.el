;;; org-air-query-test.el --- contract tests for the org-air data layer -*- lexical-binding: t; -*-

;;; Commentary:
;; Contract tests for the frozen round-1 API (air/v0.1/org-air-core.org):
;;   org-air-files, org-air-query-items, org-air-item accessors.
;; The implementation lands in a sibling workspace, so every test is
;; guarded with (skip-unless (locate-library "org-air")) and SKIPs
;; cleanly until integration.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-air-test-helpers)

(when (locate-library "org-air")
  (require 'org-air))

(ert-deftest org-air-query-api-present ()
  "The frozen data-layer API exists."
  (skip-unless (locate-library "org-air"))
  (should (boundp 'org-air-files))
  (should (fboundp 'org-air-query-items))
  (dolist (accessor '(org-air-item-title org-air-item-tags
                      org-air-item-file org-air-item-marker
                      org-air-item-todo org-air-item-priority
                      org-air-item-scheduled org-air-item-deadline
                      org-air-item-group))
    (should (fboundp accessor))))

(ert-deftest org-air-query-items-collects-fixtures ()
  "Querying the fixtures returns items from every fixture file."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (let ((items (org-air-query-items)))
      (should (consp items))
      ;; Items from a large multi-item file and from small files.
      (should (org-air-test-find-item "Prepare standup notes" items))
      (should (org-air-test-find-item "Email finance about budget" items))
      (should (org-air-test-find-item "Book dentist appointment" items))
      (should (org-air-test-find-item "Learn lute" items)))))

(ert-deftest org-air-query-item-accessors ()
  "Accessors return sensible values for a known fixture item."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (let* ((items (org-air-query-items))
           (item (org-air-test-find-item "Prepare standup notes" items)))
      (should item)
      (should (string-match-p "Prepare standup notes"
                              (org-air-item-title item)))
      (should (member "work" (org-air-item-tags item)))
      (should (string-suffix-p "projects.org" (org-air-item-file item)))
      (should (equal "TODO" (org-air-item-todo item)))
      (should (org-air-item-scheduled item))
      (should-not (org-air-item-deadline item))
      (should (markerp (org-air-item-marker item)))
      ;; Group must be callable without error (value is impl-defined).
      (org-air-item-group item))))

(ert-deftest org-air-query-item-priority-and-deadline ()
  "Priority and deadline are exposed for a [#A] DEADLINE item."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (let* ((items (org-air-query-items))
           (item (org-air-test-find-item "Prep client presentation" items)))
      (should item)
      (should (org-air-item-priority item))
      (should (org-air-item-deadline item))
      (should-not (org-air-item-scheduled item))
      (should (member "client" (org-air-item-tags item))))))

(ert-deftest org-air-query-optional-query-filters ()
  "The optional QUERY argument restricts the result set."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (let* ((all (org-air-query-items))
           (todos (org-air-query-items '(todo "TODO"))))
      (should (listp todos))
      (should (<= (length todos) (length all)))
      ;; The filtered set is a subset of the full set (by title).
      (let ((all-titles (mapcar #'org-air-item-title all)))
        (dolist (item todos)
          (should (member (org-air-item-title item) all-titles))))
      ;; DONE items must not match a TODO-only query.
      (should-not (org-air-test-find-item "Pay invoices" todos)))))

(provide 'org-air-query-test)
;;; org-air-query-test.el ends here
