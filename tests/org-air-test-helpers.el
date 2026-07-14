;;; org-air-test-helpers.el --- shared helpers for org-air test suites -*- lexical-binding: t; -*-

;;; Commentary:
;; Common helpers used by all org-air test suites.  Defines the canonical
;; fixed `now' used for deterministic classification tests, fixture path
;; helpers, and a macro that runs a test body against a scratch copy of
;; the fixtures.
;;
;; The fixture set is anchored on a frozen reference time,
;; `org-air-test-now' = Monday 2026-06-15 10:00 (local).  Relative to it:
;;
;;   upcoming      "Prepare standup notes"          SCHEDULED 2026-06-16
;;                 "Email finance about budget"     SCHEDULED 2026-06-16
;;   overdue       "Fix production outage runbook"  DEADLINE  2026-06-10
;;                 "Book dentist appointment"       DEADLINE  2026-06-12
;;   dateless      "Dust off old archive project"   created [2025-11-02]
;;                 "Learn lute"                     created [2025-09-15]
;;                 (R54-1: inactive-[ts]-only, so NEVER stale now — the
;;                 corpus renders Stale 0; stale tests append their own
;;                 dated scratch items)
;;   no schedule   "Untracked idea with no dates"
;;   high priority "Ship quarterly report" / "Fix production outage
;;                 runbook" / "Prep client presentation"  ([#A])
;;   inbox         everything in inbox.org

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Mark the org-air customisation variables special so that `let'-binding
;; them in `org-air-test-with-fixtures' is dynamic even when these test
;; files are loaded before (or without) org-air itself.
(defvar org-air-files)
(defvar org-air-inbox-file)

(defconst org-air-test-now (encode-time '(0 0 10 15 6 2026 nil -1 nil))
  "Frozen reference time for classification tests: Mon 2026-06-15 10:00.")

(defconst org-air-test-fixture-dir
  (expand-file-name "fixtures"
                    (file-name-directory
                     (or load-file-name buffer-file-name default-directory)))
  "Directory containing the canonical org fixture files.")

(defun org-air-test-fixture (name)
  "Return the absolute path of fixture NAME (e.g. \"projects.org\")."
  (expand-file-name name org-air-test-fixture-dir))

(defun org-air-test-fixture-files ()
  "Return all .org fixture files, sorted."
  (directory-files org-air-test-fixture-dir t "\\.org\\'"))

(defmacro org-air-test-with-fixtures (&rest body)
  "Run BODY against a scratch copy of the fixtures.
Copies every fixture into a fresh temporary directory and dynamically
binds `org-air-files' to the copies and `org-air-inbox-file' to the
copied inbox.org, so tests may mutate files freely.  Buffers visiting
the scratch files are killed afterwards and the directory removed."
  (declare (indent 0) (debug t))
  `(let* ((org-air-test--dir (make-temp-file "org-air-fixtures-" t)))
     (unwind-protect
         (progn
           (dolist (f (org-air-test-fixture-files))
             (copy-file f (file-name-as-directory org-air-test--dir)))
           (let ((org-air-files
                  (directory-files org-air-test--dir t "\\.org\\'"))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-test--dir)))
             ,@body))
       (let ((kill-buffer-query-functions nil))
         (dolist (buf (buffer-list))
           (let ((fn (buffer-file-name buf)))
             (when (and fn (string-prefix-p org-air-test--dir fn))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-test--dir t))))

(defun org-air-test-find-item (title items)
  "Return the first item in ITEMS whose title contains TITLE, else nil.
Uses `org-air-item-title'; only call when org-air is available."
  (cl-find-if (lambda (item)
                (string-match-p (regexp-quote title)
                                (or (org-air-item-title item) "")))
              items))

(defconst org-air-test-project-path-token "~air"
  "Deterministic stand-in for the air-project fixture ROOT in the project
inspector `Path' line.  The inspector renders `abbreviate-file-name' of the
doc's ABSOLUTE path, which differs by checkout location (~/… under HOME vs
/tmp/… on CI, where HOME does not abbreviate) — a harness artifact that made
the project-view byte goldens machine-dependent (they only matched on the
blessing machine).  Freezing the root to this token via
`directory-abbrev-alist' makes the Path field — and thus the project-view
goldens — path-INDEPENDENT and deterministic (R17 harness fix).")

(defmacro org-air-test-with-frozen-project-path (root &rest body)
  "Run BODY with the project ROOT abbreviated to a fixed token.
Binds `directory-abbrev-alist' so `abbreviate-file-name' of any doc path
under ROOT renders as `org-air-test-project-path-token'/… regardless of
where the checkout lives, so the project-view inspector `Path' line — and
the byte goldens that pin it — are deterministic (R17 harness fix).  The
entry is anchored at the start of the path and applied BEFORE the HOME→~
step in `abbreviate-file-name', so the result never depends on $HOME."
  (declare (indent 1) (debug t))
  `(let ((directory-abbrev-alist
          (cons (cons (concat "\\`" (regexp-quote (directory-file-name ,root)))
                      org-air-test-project-path-token)
                directory-abbrev-alist)))
     ,@body))

(provide 'org-air-test-helpers)
;;; org-air-test-helpers.el ends here
