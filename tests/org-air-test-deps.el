;;; org-air-test-deps.el --- install test dependencies into .deps/ -*- lexical-binding: t; -*-

;;; Commentary:
;; Run via `make deps' (after org-air-test-init.el).  Installs org-ql and
;; its transitive dependencies into the repo-local `.deps/' directory.
;; Idempotent: a no-op when everything is already installed.

;;; Code:

(require 'package)

(defconst org-air-test-deps '(org-ql package-lint evil)
  "Packages required to run the org-air test suites.")

(let ((missing (seq-remove #'package-installed-p org-air-test-deps)))
  (if (null missing)
      (message "deps: all dependencies already installed in %s" package-user-dir)
    (message "deps: installing %s into %s" missing package-user-dir)
    (package-refresh-contents)
    (dolist (pkg missing)
      (package-install pkg))))

;; Fail loudly if anything is still missing.
(dolist (pkg org-air-test-deps)
  (unless (package-installed-p pkg)
    (error "deps: failed to install %s" pkg)))

(message "deps: ok")

(provide 'org-air-test-deps)
;;; org-air-test-deps.el ends here
