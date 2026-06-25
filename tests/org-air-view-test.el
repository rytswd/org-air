;;; org-air-view-test.el --- contract tests for the dashboard view -*- lexical-binding: t; -*-

;;; Commentary:
;; Contract tests for the frozen round-1 view API: `org-air' opens the
;; *org-air* dashboard; `org-air-visit-item' (on RET) jumps to the item's
;; origin.  Asserts section presence, item lines, text properties/faces,
;; and navigation.  Guarded with skip-unless until the implementation
;; lands.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-air-test-helpers)

(when (locate-library "org-air")
  (require 'org-air))

(defmacro org-air-view-test--with-dashboard (&rest body)
  "Open the dashboard over the scratch fixtures and run BODY in it.
Renders wide (160) so V6's fixed metadata table leaves the full title
untruncated for title-based item lookups.  The clock is frozen to
`org-air-test-now' (Mon 2026-06-15) so the render is deterministic — the
fixtures' relative schedule/deadline window is anchored on that date, so
running on any wall-clock day buckets items identically."
  (declare (indent 0) (debug t))
  `(org-air-test-with-fixtures
     (cl-letf (((symbol-function 'current-time)
                (lambda () org-air-test-now)))
     (let ((org-air-view-width 160))
       (unwind-protect
           (progn
             (org-air)
             (let ((buf (get-buffer "*org-air*")))
               (should buf)
               (with-current-buffer buf
                 ,@body)))
         (when (get-buffer "*org-air*")
           (kill-buffer "*org-air*")))))))

(ert-deftest org-air-view-api-present ()
  "The frozen view API exists and `org-air' is interactive."
  (skip-unless (locate-library "org-air"))
  (should (commandp 'org-air))
  (should (fboundp 'org-air-visit-item)))

(ert-deftest org-air-view-dashboard-buffer ()
  "Running `org-air' creates a non-empty *org-air* buffer."
  (skip-unless (locate-library "org-air"))
  (org-air-view-test--with-dashboard
    (should (> (buffer-size) 0))))

(ert-deftest org-air-view-sections-present ()
  "The dashboard shows the documented bucket sections."
  (skip-unless (locate-library "org-air"))
  (org-air-view-test--with-dashboard
    (let ((case-fold-search t)
          (text (buffer-string)))
      (should (string-match-p "upcoming" text))
      (should (string-match-p "stale" text))
      (should (string-match-p "attention" text))
      (should (string-match-p "priorit" text)))))

(ert-deftest org-air-view-item-lines-present ()
  "Known fixture items appear as lines in the dashboard."
  (skip-unless (locate-library "org-air"))
  (org-air-view-test--with-dashboard
    (let ((text (buffer-string)))
      ;; One representative per bucket.
      (should (string-match-p "Prepare standup notes" text))      ; upcoming
      (should (string-match-p "Fix production outage runbook" text)) ; attention
      (should (string-match-p "Dust off old archive project" text)) ; stale
      (should (string-match-p "Ship quarterly report" text)))))   ; high-prio

(ert-deftest org-air-view-item-lines-have-faces ()
  "Item lines carry face text properties (styled rendering)."
  (skip-unless (locate-library "org-air"))
  (org-air-view-test--with-dashboard
    (goto-char (point-min))
    (should (search-forward "Prepare standup notes" nil t))
    (let ((pos (match-beginning 0)))
      (should (or (get-text-property pos 'face)
                  (get-text-property pos 'font-lock-face))))))

(ert-deftest org-air-view-ret-bound-to-visit ()
  "R18 D-P4: RET opens the view pane; S-RET (and `O') visit the item.
RET moved from `org-air-visit-item' to `org-air-view-pane-return' (open the
bottom pane, focus on a 2nd RET); visiting the file in the other window is
now `S-RET' / `O'."
  (skip-unless (locate-library "org-air"))
  (org-air-view-test--with-dashboard
    (should (eq (key-binding (kbd "RET")) 'org-air-view-pane-return))
    (should (eq (key-binding (kbd "<S-return>")) 'org-air-visit-item))
    (should (eq (key-binding (kbd "O")) 'org-air-visit-item))))

(ert-deftest org-air-view-visit-item-jumps-to-origin ()
  "`org-air-visit-item' on an item line jumps to its file and heading."
  (skip-unless (locate-library "org-air"))
  (org-air-view-test--with-dashboard
    (goto-char (point-min))
    (should (search-forward "Fix production outage runbook" nil t))
    (goto-char (match-beginning 0))
    (org-air-visit-item)
    ;; Now in the origin buffer, on the right heading.
    (should (buffer-file-name))
    (should (string-suffix-p "projects.org" (buffer-file-name)))
    (should (string-match-p "Fix production outage runbook"
                            (org-get-heading t t t t)))))

(ert-deftest org-air-view-visit-item-other-file ()
  "Navigation works for items from small fixture files too."
  (skip-unless (locate-library "org-air"))
  (org-air-view-test--with-dashboard
    (goto-char (point-min))
    (should (search-forward "Book dentist appointment" nil t))
    (goto-char (match-beginning 0))
    (org-air-visit-item)
    (should (string-suffix-p "personal.org" (buffer-file-name)))
    (should (string-match-p "Book dentist appointment"
                            (org-get-heading t t t t)))))

(provide 'org-air-view-test)
;;; org-air-view-test.el ends here
