;;; org-air-test-init.el --- batch bootstrap for the org-air test harness -*- lexical-binding: t; -*-

;;; Commentary:
;; Loaded first in every batch invocation (see Makefile).  Points
;; package.el at the repo-local `.deps/' directory, initialises installed
;; packages, and puts the repo root + tests/ on `load-path'.  Never
;; touches the user's ~/.emacs.d.

;;; Code:

(defconst org-air-test-root
  (expand-file-name
   (or (and load-file-name
            (locate-dominating-file load-file-name "Makefile"))
       default-directory))
  "Root of the org-air checkout.")

(setq package-user-dir (expand-file-name ".deps" org-air-test-root))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(add-to-list 'load-path org-air-test-root)
(add-to-list 'load-path (expand-file-name "tests" org-air-test-root))

(provide 'org-air-test-init)
;;; org-air-test-init.el ends here
