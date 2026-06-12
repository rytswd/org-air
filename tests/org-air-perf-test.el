;;; org-air-perf-test.el --- perf smoke tests over a large fixture -*- lexical-binding: t; -*-

;;; Commentary:
;; Performance smoke tests: a deterministic generated corpus of ~1000
;; headings across 5 files.  Budgets are deliberately generous — they
;; exist to catch order-of-magnitude regressions (accidental O(n²),
;; re-parsing per item, etc.), not to benchmark.  Wall-clock on a loaded
;; CI box must still pass comfortably.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-air-test-helpers)

(when (locate-library "org-air")
  (require 'org-air))

(defconst org-air-perf-files 5
  "Number of generated org files.")

(defconst org-air-perf-headings-per-file 200
  "Headings per generated file (total 1000).")

(defconst org-air-perf-query-budget 30.0
  "Seconds allowed for `org-air-query-items' over the corpus.")

(defconst org-air-perf-render-budget 60.0
  "Seconds allowed for a full dashboard render over the corpus.")

(defun org-air-perf--heading (n)
  "Return deterministic org heading N as a string."
  (let* ((todo (pcase (% n 4)
                 (0 "TODO ") (1 "TODO ") (2 "NEXT ") (_ "")))
         (prio (if (= 0 (% n 7)) "[#A] " ""))
         (tags (pcase (% n 5)
                 (0 "  :work:")
                 (1 "  :home:errand:")
                 (2 "  :project:")
                 (_ "")))
         (day (1+ (% n 28)))
         (month (1+ (% n 12)))
         (planning (pcase (% n 3)
                     (0 (format "SCHEDULED: <2026-%02d-%02d>\n" month day))
                     (1 (format "DEADLINE: <2026-%02d-%02d>\n" month day))
                     (_ ""))))
    (concat (format "* %s%sGenerated heading %04d%s\n" todo prio n tags)
            planning
            (format "Body text for heading %d.\n" n))))

(defmacro org-air-perf--with-corpus (&rest body)
  "Generate the corpus into a temp dir, bind `org-air-files', run BODY."
  (declare (indent 0) (debug t))
  `(let ((org-air-perf--dir (make-temp-file "org-air-perf-" t)))
     (unwind-protect
         (progn
           (dotimes (f org-air-perf-files)
             (with-temp-file (expand-file-name (format "gen-%d.org" f)
                                               org-air-perf--dir)
               (insert (format "#+title: Generated %d\n\n" f))
               (dotimes (i org-air-perf-headings-per-file)
                 (insert (org-air-perf--heading
                          (+ i (* f org-air-perf-headings-per-file)))))))
           (let ((org-air-files
                  (directory-files org-air-perf--dir t "\\.org\\'"))
                 (org-air-inbox-file
                  (expand-file-name "gen-0.org" org-air-perf--dir)))
             ,@body))
       (let ((kill-buffer-query-functions nil))
         (dolist (buf (buffer-list))
           (let ((fn (buffer-file-name buf)))
             (when (and fn (string-prefix-p org-air-perf--dir fn))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-perf--dir t))))

(defun org-air-perf--elapsed (thunk)
  "Return seconds spent calling THUNK."
  (let ((start (current-time)))
    (funcall thunk)
    (float-time (time-subtract (current-time) start))))

(ert-deftest org-air-perf-query-1000-headings ()
  "Querying ~1000 headings completes within the smoke budget."
  (skip-unless (locate-library "org-air"))
  (org-air-perf--with-corpus
    (let* ((items nil)
           (elapsed (org-air-perf--elapsed
                     (lambda () (setq items (org-air-query-items))))))
      (should (>= (length items)
                  (* org-air-perf-files org-air-perf-headings-per-file)))
      (message "perf: query %d items in %.2fs (budget %.0fs)"
               (length items) elapsed org-air-perf-query-budget)
      (should (< elapsed org-air-perf-query-budget)))))

(ert-deftest org-air-perf-dashboard-render-1000-headings ()
  "A full dashboard render over ~1000 headings stays within budget."
  (skip-unless (locate-library "org-air"))
  (org-air-perf--with-corpus
    (unwind-protect
        (let ((elapsed (org-air-perf--elapsed #'org-air)))
          (with-current-buffer "*org-air*"
            (should (> (buffer-size) 0)))
          (message "perf: dashboard render in %.2fs (budget %.0fs)"
                   elapsed org-air-perf-render-budget)
          (should (< elapsed org-air-perf-render-budget)))
      (when (get-buffer "*org-air*")
        (kill-buffer "*org-air*")))))

(provide 'org-air-perf-test)
;;; org-air-perf-test.el ends here
