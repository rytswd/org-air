;;; org-air-harness-test.el --- sanity tests for the harness itself -*- lexical-binding: t; -*-

;;; Commentary:
;; Self-contained tests proving the harness works: deps are installed,
;; fixtures exist and parse, the helper macro behaves, and the frozen
;; reference time is what the fixtures assume.  These do NOT require
;; org-air and must always run.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-air-test-helpers)

(ert-deftest org-air-harness-ert-runs ()
  "ERT itself executes in this environment."
  (should (= 4 (+ 2 2))))

(ert-deftest org-air-harness-deps-installed ()
  "org-ql is installed into the repo-local .deps/ and loadable."
  (should (string-suffix-p "/.deps" (directory-file-name package-user-dir)))
  (should (locate-library "org-ql"))
  (should (require 'org-ql nil t)))

(ert-deftest org-air-harness-fixtures-present ()
  "All expected fixture files exist and are non-empty."
  (dolist (name '("projects.org" "work.org" "personal.org"
                  "someday.org" "inbox.org"))
    (let ((file (org-air-test-fixture name)))
      (should (file-readable-p file))
      (should (> (file-attribute-size (file-attributes file)) 0)))))

(ert-deftest org-air-harness-fixtures-parse ()
  "Every fixture parses as org and contains at least one heading."
  (dolist (file (org-air-test-fixture-files))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (org-element-parse-buffer) ; must not error
      (goto-char (point-min))
      (should (re-search-forward org-heading-regexp nil t)))))

(ert-deftest org-air-harness-org-ql-queries-fixtures ()
  "org-ql can query the fixtures (proves the data path end to end)."
  (require 'org-ql)
  (let ((results (org-ql-select (org-air-test-fixture "projects.org")
                   '(todo "TODO")
                   :action #'org-get-heading)))
    (should (consp results))
    ;; projects.org has exactly 9 TODO (not DONE) headings.
    (should (= 9 (length results)))))

(ert-deftest org-air-harness-frozen-now ()
  "The canonical fixed `now' is Mon 2026-06-15 10:00."
  (should (equal "2026-06-15 Mon 10:00"
                 (format-time-string "%F %a %H:%M" org-air-test-now))))

(ert-deftest org-air-harness-with-fixtures-macro ()
  "The scratch-copy macro copies fixtures, binds vars, and cleans up."
  (let (scratch-dir)
    (org-air-test-with-fixtures
      (setq scratch-dir (file-name-directory org-air-inbox-file))
      (should (= (length (org-air-test-fixture-files))
                 (length org-air-files)))
      (should (file-exists-p org-air-inbox-file))
      ;; Mutating the copy must not touch the canonical fixture.
      (with-temp-buffer
        (insert "* Scratch heading\n")
        (write-region (point-min) (point-max) org-air-inbox-file t 'silent)))
    (should-not (file-exists-p scratch-dir))
    (with-temp-buffer
      (insert-file-contents (org-air-test-fixture "inbox.org"))
      (should-not (search-forward "Scratch heading" nil t)))))

(provide 'org-air-harness-test)
;;; org-air-harness-test.el ends here
