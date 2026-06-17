;;; org-air-ux-test.el --- round-2 UX behaviour tests for org-air -*- lexical-binding: t; -*-

;;; Commentary:
;; Round-2 UX tests against air/v0.1/org-air-design.org:
;;   - keybinding map (§9): RET / g / "/" / q, plus refile + filter-clear
;;   - calendar pane (§6): header, today mark, scheduled/deadline day marks
;;   - tag filtering (§8.3): filter narrows the view, clear restores it
;;   - inbox classification by file (`org-air-inbox-file') and capture (§8.1)
;;   - classification edge cases: same-day deadline/schedule, bare
;;     [inactive] timestamps as activity, DONE-state exclusion
;;
;; Where the implementation currently falls short of the design spec the
;; failing test is written anyway (grind signal for the impl track).
;; Deterministic classification uses `org-air-test-now' (Mon 2026-06-15).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-air-test-helpers)

(when (locate-library "org-air")
  (require 'org-air))

;;; Helpers

(defmacro org-air-ux-test--with-dashboard (&rest body)
  "Open the dashboard over the scratch fixtures and run BODY in it.
Renders wide (160) so V6's fixed metadata table leaves the full title
untruncated for title-based item lookups.  The clock is frozen to
`org-air-test-now' (Mon 2026-06-15) so bucketing is deterministic on any
wall-clock day (the fixtures' schedule/deadline window is anchored there)."
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

(defun org-air-ux-test--filter (tag)
  "Apply TAG as the dashboard tag filter via whatever command exists."
  (cond ((fboundp 'org-air-filter-by-tag) (org-air-filter-by-tag tag))
        ((fboundp 'org-air-filter) (org-air-filter tag))
        (t (ert-fail "No tag filter command is defined"))))

(defun org-air-ux-test--filter-clear ()
  "Clear the dashboard tag filter via whatever command exists."
  (if (fboundp 'org-air-filter-clear)
      (org-air-filter-clear)
    (org-air-ux-test--filter "")))

(defun org-air-ux-test--mode-line-text (construct)
  "Return all literal strings inside mode-line CONSTRUCT, concatenated."
  (cond ((stringp construct) (substring-no-properties construct))
        ((consp construct)
         (mapconcat #'org-air-ux-test--mode-line-text construct ""))
        (t "")))

(defun org-air-ux-test--classify (title)
  "Classify the fixture item whose title contains TITLE at the frozen now."
  (let* ((items (org-air-query-items))
         (item (org-air-test-find-item title items)))
    (should item)
    (org-air-classify-item item org-air-test-now)))

(cl-defun org-air-ux-test--calendar-item (&key scheduled deadline)
  "Build a minimal `org-air-item' with SCHEDULED / DEADLINE timestamp strings."
  (org-air-item-create
   :title "calendar probe" :tags nil :file "" :marker nil :todo "TODO"
   :priority nil
   :scheduled (and scheduled (org-timestamp-from-string scheduled))
   :deadline (and deadline (org-timestamp-from-string deadline))
   :group nil :closed nil))

(defmacro org-air-ux-test--with-month (date items &rest body)
  "Render a calendar month for DATE and ITEMS in a temp buffer, run BODY."
  (declare (indent 2) (debug t))
  `(with-temp-buffer
     (org-air-calendar-insert-month ,date ,items)
     (goto-char (point-min))
     ,@body))

;;; Keybindings (design spec §9; names frozen there)

(ert-deftest org-air-ux-keys-core ()
  "RET visits, g refreshes, q quits — the core §9 bindings.
Round-7 R6: q is `org-air-quit', which returns from the single-day view
to the board, else quits the window."
  (skip-unless (locate-library "org-air"))
  (org-air-ux-test--with-dashboard
    (should (eq (key-binding (kbd "RET")) 'org-air-visit-item))
    ;; Round-8 B4: g is a prefix map now — refresh is `g r'.
    (should (eq (key-binding (kbd "g r")) 'org-air-refresh))
    (should (eq (key-binding (kbd "q")) 'org-air-quit))))

(ert-deftest org-air-ux-keys-filter-spec-name ()
  "\"/\" is bound to `org-air-filter' (§9 frozen command name)."
  (skip-unless (locate-library "org-air"))
  (org-air-ux-test--with-dashboard
    (should (eq (key-binding (kbd "/")) 'org-air-filter))))

(ert-deftest org-air-ux-keys-filter-clear ()
  "\"\\\\\" is bound to `org-air-filter-clear' (§9)."
  (skip-unless (locate-library "org-air"))
  (org-air-ux-test--with-dashboard
    (should (eq (key-binding (kbd "\\")) 'org-air-filter-clear))))

(ert-deftest org-air-ux-keys-refile ()
  "\"r\" is bound to `org-air-refile-item' (§9)."
  (skip-unless (locate-library "org-air"))
  (org-air-ux-test--with-dashboard
    (should (eq (key-binding (kbd "r")) 'org-air-refile-item))))

;;; Calendar pane (design spec §6)

(ert-deftest org-air-ux-calendar-header ()
  "The pane shows \"Month YYYY\" and a weekday-name row."
  (skip-unless (locate-library "org-air"))
  (org-air-ux-test--with-month (encode-time 0 0 0 1 6 2026) nil
    (should (search-forward "June 2026" nil t))
    ;; T3a: weekday labels are "%-4s"-padded (4 cols/day) -> >1 space gap.
    (should (string-match-p
             "\\(Mo +Tu +We +Th +Fr +Sa +Su\\|Su +Mo +Tu +We +Th +Fr +Sa\\)"
             (buffer-string)))))

(ert-deftest org-air-ux-calendar-today-marked ()
  "Today's cell carries the spec \=`org-air-face-calendar-today' face (§6)."
  (skip-unless (locate-library "org-air"))
  (org-air-ux-test--with-month (current-time) nil
    (let* ((today (format "%2d" (decoded-time-day (decode-time (current-time)))))
           (found nil)
           (pos (point-min)))
      (while (and (not found) (setq pos (next-single-property-change pos 'face)))
        (when (and (eq (get-text-property pos 'face) 'org-air-face-calendar-today)
                   (equal (buffer-substring-no-properties
                           pos (min (point-max) (+ pos 2)))
                          today))
          (setq found t)))
      (should found))))

(ert-deftest org-air-ux-calendar-scheduled-day-marked ()
  "A day holding a SCHEDULED item is visibly marked."
  (skip-unless (locate-library "org-air"))
  (org-air-ux-test--with-month (encode-time 0 0 0 1 6 2026)
      (list (org-air-ux-test--calendar-item :scheduled "<2026-06-14 Sun>"))
    ;; Skip past the "June 2026" header before searching for the day cell.
    (search-forward "2026" nil t)
    (should (search-forward "14" nil t))
    ;; The mark glyph (e.g. "•") or a non-default face must flag the day.
    (should (or (looking-at-p "•")
                (not (memq (get-text-property (match-beginning 0) 'face)
                           '(nil org-air-face-default)))))))

(ert-deftest org-air-ux-calendar-deadline-day-marked ()
  "A day holding a DEADLINE item is marked too (§6: any dashboard item)."
  (skip-unless (locate-library "org-air"))
  (org-air-ux-test--with-month (encode-time 0 0 0 1 6 2026)
      (list (org-air-ux-test--calendar-item :deadline "<2026-06-13 Sat>"))
    (search-forward "2026" nil t)
    (should (search-forward "13" nil t))
    (should (or (looking-at-p "•")
                (not (memq (get-text-property (match-beginning 0) 'face)
                           '(nil org-air-face-default)))))))

;;; Tag filtering (design spec §8.3)

(ert-deftest org-air-ux-filter-narrows-view ()
  "Filtering by a tag hides items that do not carry it."
  (skip-unless (locate-library "org-air"))
  (org-air-ux-test--with-dashboard
    (org-air-ux-test--filter "work")
    (let ((text (buffer-string)))
      (should (string-match-p "Prepare standup notes" text))
      (should-not (string-match-p "Book dentist appointment" text))
      ;; S1: the header band is IN-BUFFER text (the only banner — see
      ;; org-air-s1-no-duplicate-banner-in-header-line); the active
      ;; filter chip is echoed in the band, so read the buffer.
      (should (string-match-p "#work" text)))))

(ert-deftest org-air-ux-filter-clear-restores-view ()
  "Clearing the tag filter brings hidden items back."
  (skip-unless (locate-library "org-air"))
  (org-air-ux-test--with-dashboard
    (org-air-ux-test--filter "work")
    (should-not (string-match-p "Book dentist appointment" (buffer-string)))
    (org-air-ux-test--filter-clear)
    (should (string-match-p "Book dentist appointment" (buffer-string)))
    (should (string-match-p "Prepare standup notes" (buffer-string)))))

;;; Inbox by file (core contract: `org-air-inbox-file') and capture (§8.1)

(ert-deftest org-air-ux-inbox-file-classifies-without-tag ()
  "Items in `org-air-inbox-file' are inbox even without an :inbox: tag."
  (skip-unless (locate-library "org-air"))
  (let* ((dir (make-temp-file "org-air-ux-inbox-" t))
         (file (expand-file-name "capture.org" dir)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "#+title: Capture\n\n* Note without any tags\nJust captured.\n"))
          (let* ((org-air-files (list file))
                 (org-air-inbox-file file)
                 (items (org-air-query-items))
                 (item (org-air-test-find-item "Note without any tags" items)))
            (should item)
            (should (memq 'inbox (org-air-classify-item item org-air-test-now)))))
      (when-let* ((buf (find-buffer-visiting file))) (kill-buffer buf))
      (delete-directory dir t))))

(ert-deftest org-air-ux-capture-lands-in-inbox ()
  "`org-air-capture' appends to the inbox file and the item is inbox-bucketed."
  (skip-unless (locate-library "org-air"))
  (let* ((dir (make-temp-file "org-air-ux-capture-" t))
         (file (expand-file-name "inbox.org" dir)))
    (unwind-protect
        (let ((org-air-files (list file))
              (org-air-inbox-file file))
          (org-air-capture "Try the capture flow")
          (should (file-exists-p file))
          (with-temp-buffer
            (insert-file-contents file)
            (should (string-match-p "Try the capture flow" (buffer-string)))
            ;; §8.1: stamped with an inactive CREATED timestamp.
            (should (string-match-p "CREATED.*\\[[0-9]\\{4\\}-" (buffer-string))))
          (let* ((items (org-air-query-items))
                 (item (org-air-test-find-item "Try the capture flow" items)))
            (should item)
            (should (memq 'inbox (org-air-classify-item item org-air-test-now)))))
      (when-let* ((buf (find-buffer-visiting file))) (kill-buffer buf))
      (delete-directory dir t))))

;;; Classification edge cases

(ert-deftest org-air-ux-same-day-schedule-is-upcoming ()
  "An item scheduled for today is upcoming, not attention."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (let ((buckets (org-air-ux-test--classify "Water the garden")))
      (should (memq 'upcoming buckets))
      (should-not (memq 'attention buckets)))))

(ert-deftest org-air-ux-same-day-deadline-is-upcoming ()
  "An item whose deadline is today is upcoming, not attention."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (let ((buckets (org-air-ux-test--classify "Renew library card")))
      (should (memq 'upcoming buckets))
      (should-not (memq 'attention buckets)))))

(ert-deftest org-air-ux-bare-inactive-timestamp-is-activity ()
  "Bare [inactive] timestamps in the body count as recent activity."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    ;; [2026-06-13] — two days before the frozen now — is recent.
    (should-not (memq 'stale (org-air-ux-test--classify "Triage me later")))
    ;; [2025-09-15] — nine months before — is stale.
    (should (memq 'stale (org-air-ux-test--classify "Learn lute")))))

(ert-deftest org-air-ux-done-items-have-no-buckets ()
  "DONE items classify into no bucket at all."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (let* ((items (org-air-query-items))
           (item (org-air-test-find-item "Submit expense report" items)))
      (should item)
      (should (equal "DONE" (org-air-item-todo item)))
      (should-not (org-air-classify-item item org-air-test-now)))))

(ert-deftest org-air-ux-done-items-not-in-dashboard ()
  "DONE items never appear in any dashboard section."
  (skip-unless (locate-library "org-air"))
  (org-air-ux-test--with-dashboard
    (should-not (string-match-p "Submit expense report" (buffer-string)))))

(provide 'org-air-ux-test)
;;; org-air-ux-test.el ends here
