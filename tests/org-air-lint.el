;;; org-air-lint.el --- batch lint gate: checkdoc + package-lint -*- lexical-binding: t; -*-

;;; Commentary:
;; Run via `make lint'.  Lints every org-air*.el with checkdoc and
;; package-lint and compares the findings against the accepted baseline
;; in tests/org-air-lint-baseline.el.  Same self-policing contract as the
;; known-failures manifest:
;;   - finding matched by a baseline entry -> accepted
;;   - finding NOT in the baseline         -> FAIL (new lint issue)
;;   - baseline entry matching nothing     -> FAIL (stale entry: the
;;     issue was fixed, delete the entry as closeout)
;; So `make lint' is binary and the baseline can only shrink honestly.
;;
;; `package-lint-main-file' is set to org-air.el so the multi-file
;; package is linted as one package (sub-file prefix/header noise is
;; thereby suppressed; genuine issues still surface).

;;; Code:

(require 'checkdoc)
(require 'package-lint)
(require 'org-air-lint-baseline)

(defconst org-air-lint--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root of the org-air checkout (parent of tests/).")

(defun org-air-lint--files ()
  "Return all org-air source files to lint."
  (directory-files org-air-lint--root t "^org-air.*\\.el\\'"))

(defun org-air-lint--collect-checkdoc (file report)
  "Run checkdoc on FILE, calling REPORT with each finding string."
  (let ((checkdoc-create-error-function
         (lambda (text _start _end &optional _unfixable)
           (funcall report (format "checkdoc %s: %s"
                                   (file-name-nondirectory file) text))
           nil)))
    (checkdoc-file file)))

(defun org-air-lint--collect-package-lint (file report)
  "Run package-lint on FILE, calling REPORT with each finding string."
  (let ((package-lint-main-file (expand-file-name "org-air.el"
                                                  org-air-lint--root)))
    (with-temp-buffer
      (insert-file-contents file t)
      (emacs-lisp-mode)
      (dolist (issue (package-lint-buffer))
        (funcall report (format "package-lint %s: %s %s"
                                (file-name-nondirectory file)
                                (nth 2 issue) (nth 3 issue)))))))

(defun org-air-lint-batch ()
  "Lint all org-air sources against the baseline; exit non-zero on failure."
  (let ((findings nil))
    (dolist (file (org-air-lint--files))
      (let ((report (lambda (s) (push s findings))))
        (org-air-lint--collect-checkdoc file report)
        (org-air-lint--collect-package-lint file report)))
    (setq findings (nreverse findings))
    (let* ((unmatched-baseline (copy-sequence org-air-lint-baseline))
           (new nil))
      (dolist (finding findings)
        (let ((hit (seq-find (lambda (entry) (string-match-p entry finding))
                             unmatched-baseline)))
          (if hit
              (setq unmatched-baseline (delete hit unmatched-baseline))
            (push finding new))))
      (setq new (nreverse new))
      (dolist (f new) (message "lint: NEW finding: %s" f))
      (dolist (b unmatched-baseline)
        (message "lint: STALE baseline entry (issue fixed — delete it): %s" b))
      (if (or new unmatched-baseline)
          (progn
            (message "lint: FAIL (%d new, %d stale baseline)"
                     (length new) (length unmatched-baseline))
            (kill-emacs 1))
        (message "lint: ok (%d findings, all baselined)" (length findings))))))

(provide 'org-air-lint)
;;; org-air-lint.el ends here
