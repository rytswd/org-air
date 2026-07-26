;;; org-air-round90-test.el --- acceptance tests for round 90 -*- lexical-binding: t; -*-

;;; Commentary:
;; Round-90 acceptance and mutation seams: local mutation landing, durable
;; source-key marks, file-coordinated bulk tag writes, compound history, and
;; collapsed-by-default Backlog.  The hostile failure-path tests are
;; intentionally strict: a source defect remains an unexpected failure rather
;; than entering the known-failures manifest.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)

(when (locate-library "org-air")
  (require 'org-air)
  (require 'org-air-review))

(defvar org-air-r90--dir nil
  "Temporary corpus directory of the current round-90 test.")

(defun org-air-r90--reset-query-state ()
  "Reset global query tables between round-90 corpora."
  (when (fboundp 'org-air-query-teardown)
    (org-air-query-teardown)
    (clrhash org-air-query--file-meta)
    (clrhash org-air-query--visits)
    (clrhash org-air-query--denote-id-index)
    (setq org-air-query--link-graph-dirty nil)))

(defmacro org-air-r90--with-corpus (specs &rest body)
  "Create SPECS in a fresh corpus and run BODY with isolated global state."
  (declare (indent 1) (debug t))
  `(let ((org-air-r90--dir (make-temp-file "org-air-r90-" t)))
     (unwind-protect
         (progn
           (org-air-r90--reset-query-state)
           (pcase-dolist (`(,name . ,content) ,specs)
             (let ((path (expand-file-name name org-air-r90--dir))
                   (coding-system-for-write 'utf-8-unix)
                   (file-name-handler-alist nil))
               (make-directory (file-name-directory path) t)
               (write-region (or content "") nil path nil 'silent)))
           (let ((org-air-files (list org-air-r90--dir))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-r90--dir))
                 (org-air-cache-file
                  (expand-file-name ".cache/board.eld" org-air-r90--dir))
                 (org-air-view-buffer-name "*org-air-r90*")
                 (org-air-view--edit-ring nil)
                 (org-air-view--edit-redo-ring nil)
                 (org-air-view--triage-source-buffer nil)
                 (org-air-backlog-tag "backlog")
                 (org-air-plain-heading-type 'task)
                 (org-tags-column 0)
                 (org-use-tag-inheritance t)
                 (create-lockfiles nil)
                 (inhibit-message t))
             ,@body))
       (org-air-r90--reset-query-state)
       (let ((kill-buffer-query-functions nil))
         (dolist (buf (buffer-list))
           (let ((file (buffer-file-name buf)))
             (when (and file (string-prefix-p org-air-r90--dir file))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r90--dir t))))

(defmacro org-air-r90--with-board (specs &rest body)
  "Render a frozen real board over SPECS and run BODY in its owner buffer."
  (declare (indent 1) (debug t))
  `(org-air-r90--with-corpus ,specs
     (org-air-viewport-test--with-frozen-now
       (org-air-viewport-test--with-render-guards
         (let ((org-air-view-width 120)
               (org-air-view-height 70))
           (unwind-protect
               (progn
                 (org-air)
                 (let ((buf (get-buffer org-air-view-buffer-name)))
                   (should buf)
                   (with-current-buffer buf ,@body)))
             (let ((kill-buffer-query-functions nil)
                   (buf (get-buffer org-air-view-buffer-name)))
               (when buf (kill-buffer buf)))))))))

(defun org-air-r90--file (name)
  "Return absolute corpus path NAME."
  (expand-file-name name org-air-r90--dir))

(defun org-air-r90--text (name)
  "Return on-disk text of corpus file NAME."
  (with-temp-buffer
    (insert-file-contents (org-air-r90--file name))
    (buffer-string)))

(defun org-air-r90--item (title &optional items)
  "Return item named TITLE from ITEMS or the current board cache."
  (let ((item (org-air-test-find-item title (or items org-air-view--items))))
    (should item)
    item))

(defun org-air-r90--goto-row (title)
  "Move point to the rendered item row named TITLE and return its item."
  (goto-char (point-min))
  (let (found)
    (while (and (not found) (not (eobp)))
      (let ((item (org-air-view--row-property 'org-air-item)))
        (when (and item
                   (string-match-p (regexp-quote title)
                                   (or (org-air-item-title item) "")))
          (setq found item)))
      (unless found (forward-line 1)))
    (should found)
    (org-air-view--goto-row-title)
    found))

(defun org-air-r90--row-title ()
  "Return the title of the item at point, or nil."
  (when-let* ((item (org-air-view--row-property 'org-air-item)))
    (org-air-item-title item)))

(defun org-air-r90--rendered-rows (title)
  "Return beginning positions of every rendered row named TITLE."
  (save-excursion
    (goto-char (point-min))
    (let (out)
      (while (not (eobp))
        (when-let* ((item (org-air-view--row-property 'org-air-item))
                    ((string-match-p (regexp-quote title)
                                     (or (org-air-item-title item) ""))))
          (push (line-beginning-position) out))
        (forward-line 1))
      (nreverse out))))

(defun org-air-r90--title-columns (title)
  "Return title columns for every rendered occurrence of TITLE."
  (mapcar (lambda (pos)
            (save-excursion
              (goto-char pos)
              (org-air-view--goto-row-title)
              (current-column)))
          (org-air-r90--rendered-rows title)))

(defun org-air-r90--mark-title (title)
  "Mark the exact source heading rendered as TITLE."
  (org-air-r90--goto-row title)
  (org-air-toggle-mark))

(defun org-air-r90--actual-heading-position (name title)
  "Return live heading position for TITLE in corpus file NAME."
  (with-current-buffer (find-file-noselect (org-air-r90--file name))
    (org-with-wide-buffer
     (goto-char (point-min))
     (re-search-forward (regexp-quote title))
     (org-back-to-heading t)
     (point))))

(defun org-air-r90--file-has-tag-p (name title tag)
  "Return non-nil when TITLE's live heading in NAME has local TAG."
  (with-current-buffer (find-file-noselect (org-air-r90--file name))
    (org-with-wide-buffer
     (goto-char (point-min))
     (re-search-forward (regexp-quote title))
     (org-back-to-heading t)
     (member tag (org-get-tags nil t)))))

(defun org-air-r90--disk-has-tag-p (name title tag)
  "Return non-nil when on-disk TITLE in NAME carries exact TAG."
  (let ((text (org-air-r90--text name)))
    (and (string-match-p
          (format "^\\*+ .*%s.*:%s:"
                  (regexp-quote title) (regexp-quote tag))
          text)
         t)))

(defun org-air-r90--hostile-b-snapshot (first later first-key)
  "Inject post-save relocation failure and summarize every truth surface."
  (cl-letf* (((symbol-function 'org-air-view--relocation-commit)
              (lambda (&rest _) (error "post-save relocation failure")))
             ((symbol-function 'read-string)
              (lambda (&rest _) "long-shared-tag")))
    (org-air-set-tag))
  (list :disk (org-air-r90--disk-has-tag-p
               "tasks.org" "First heading" "long-shared-tag")
        :live (and (org-air-r90--file-has-tag-p
                    "tasks.org" "First heading" "long-shared-tag") t)
        :cache-tag (and (member "long-shared-tag"
                                (org-air-item-tags first)) t)
        :position (= (cdr (org-air-view--item-source-key later))
                     (org-air-r90--actual-heading-position
                      "tasks.org" "Later heading"))
        :marked (and (member first-key org-air-view--marked-keys) t)
        :history (and org-air-view--edit-ring
                      (plist-get (car org-air-view--edit-ring) :kind))))

(defmacro org-air-r90--record-messages (var &rest body)
  "Run BODY and collect formatted `message' calls in VAR."
  (declare (indent 1) (debug t))
  `(let ((,var nil))
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args)
                  (when fmt (push (apply #'format fmt args) ,var))
                  nil)))
       ,@body)))

(defmacro org-air-r90--with-keybinding-knob (value &rest body)
  "Set the installer-owned keybinding knob to VALUE while running BODY."
  (declare (indent 1) (debug t))
  `(let ((saved org-air-use-default-keybindings))
     (unwind-protect
         (progn
           (setq org-air-use-default-keybindings ,value)
           (org-air--sync-default-keybindings)
           ,@body)
       (setq org-air-use-default-keybindings saved
             org-air--default-keybindings-state 'unset)
       (org-air--sync-default-keybindings))))

(defconst org-air-r90--three
  '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha task :one:\n* TODO Beta task :two:\n* TODO Gamma task :three:\n")
    ("inbox.org" . "#+title: inbox\n"))
  "Three same-section tasks used by local-focus and mark tests.")

;;; r90-1 — local successor, exclusion, and synchronous surface ordering.

(ert-deftest org-air-r90-1-local-focus-middle-successor-no-scan ()
  "A middle-row `b' lands on its same-section successor, never its new home."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board org-air-r90--three
    ;; Provision the one user-initiated source buffer before timer spying;
    ;; org-mode's own first-mode initialization is outside the landing seam.
    (find-file-noselect (org-air-r90--file "tasks.org"))
    (let* ((moved (org-air-r90--goto-row "Beta task"))
           (moved-key (org-air-view--item-source-key moved))
           (queries 0) (timers 0) (setup nil) (pane-final nil))
      (cl-letf* ((setup-orig (symbol-function 'org-air-view--setup-inspector))
                 ((symbol-function 'org-air-view--setup-inspector)
                  (lambda ()
                    (push (list org-air-view--pending-mutation-landing
                                (org-air-r90--row-title)) setup)
                    (funcall setup-orig)))
                 ((symbol-function 'org-air-view--view-pane-update-now)
                  (lambda (buf)
                    (setq pane-final
                          (with-current-buffer buf (org-air-r90--row-title)))))
                 (query-orig (symbol-function 'org-air-query-items))
                 ((symbol-function 'org-air-query-items)
                  (lambda (&rest args)
                    (cl-incf queries) (apply query-orig args)))
                 (query-files-orig
                  (symbol-function 'org-air-query-items-in-files))
                 ((symbol-function 'org-air-query-items-in-files)
                  (lambda (&rest args)
                    (cl-incf queries) (apply query-files-orig args)))
                 ((symbol-function 'run-with-timer)
                  (lambda (&rest _) (cl-incf timers) nil))
                 ((symbol-function 'run-with-idle-timer)
                  (lambda (&rest _) (cl-incf timers) nil)))
        (org-air-item-backlog))
      (should (equal "Gamma task" (org-air-r90--row-title)))
      (should-not (equal moved-key
                         (org-air-view--item-source-key
                          (org-air-view--row-property 'org-air-item))))
      (should (org-air-r90--file-has-tag-p "tasks.org" "Beta task" "backlog"))
      (should (equal "Gamma task" pane-final))
      (should (equal '(nil "Gamma task") (car setup)))
      (should-not org-air-view--pending-mutation-landing)
      (should (= queries 0))
      (should (= timers 0)))))

;;; r90-2 — last/previous and un-backlog local focus.

(ert-deftest org-air-r90-2-last-and-unbacklog-use-local-neighbours ()
  "Last-row and expanded/lensed un-backlog never follow the moved item."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board org-air-r90--three
    (org-air-r90--goto-row "Gamma task")
    (org-air-item-backlog)
    (should (equal "Beta task" (org-air-r90--row-title))))
  (dolist (lens '(expanded exact-lens))
    (org-air-r90--with-board
        '(("tasks.org" . "#+title: tasks\n\n* TODO Back A :backlog:\n* TODO Back B :backlog:\n* TODO Plain task\n")
          ("inbox.org" . "#+title: inbox\n"))
      (pcase lens
        ('expanded
         (setq org-air-view--expanded-sections '(backlog))
         (org-air-view--render-current))
        ('exact-lens
         (setq org-air-view--tag-filter '("is:backlog"))
         (org-air-view--ensure-explicit-backlog-lens)
         (org-air-view--render-current)))
      (let ((moved-key (org-air-view--item-source-key
                        (org-air-r90--goto-row "Back A"))))
        (org-air-item-backlog)
        (should (equal "Back B" (org-air-r90--row-title)))
        (should-not (equal moved-key
                           (org-air-view--item-source-key
                            (org-air-view--row-property 'org-air-item))))))))

;;; r90-3 — exact ordered marks, mirrors, primitive, and repaint durability.

(ert-deftest org-air-r90-3-durable-ordered-mirror-marks ()
  "Marks are source-key ordered state rendered through every mirror row."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO [#A] Mirror task :proj:\nDEADLINE: <2026-06-10 Wed>\n* TODO Other task :other:\nSCHEDULED: <2026-06-15 Mon>\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let ((before-cols (org-air-r90--title-columns "Mirror task"))
          (primitive-marks 0))
      (should (> (length before-cols) 1))
      (let* ((mirror (org-air-r90--goto-row "Mirror task"))
             (mirror-key (org-air-view--item-source-key mirror)))
        (cl-letf* ((orig (symbol-function 'org-air-view--insert-row))
                   ((symbol-function 'org-air-view--insert-row)
                    (lambda (&rest args)
                      (when (plist-get args :marked)
                        (cl-incf primitive-marks))
                      (apply orig args))))
          (org-air-toggle-mark))
        (should (equal (list mirror-key) org-air-view--marked-keys)))
      (org-air-r90--mark-title "Other task")
      (should (= 2 (length org-air-view--marked-keys)))
      (should (> primitive-marks 1))
      (should (equal before-cols (org-air-r90--title-columns "Mirror task")))
      (dolist (pos (org-air-r90--rendered-rows "Mirror task"))
        (goto-char pos)
        (should (org-air-view--row-property 'org-air-marked))
        (should (string-match-p "•" (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (line-end-position)))))
      ;; Cached repaint, sort, filter/collapse, and day/board switches keep
      ;; the exact ordered source selection even while rows are hidden.
      (let ((keys (copy-sequence org-air-view--marked-keys)))
        (org-air-view--refresh-current)
        (org-air-view-sort-cycle)
        (setq org-air-view--tag-filter '("Other"))
        (org-air-view--render-current)
        (setq org-air-view--expanded-sections nil)
        (org-air-view-day org-air-test-now)
        (org-air-view-board)
        (should (equal keys org-air-view--marked-keys)))
      (org-air-clear-marks)
      (should-not org-air-view--marked-keys)
      (should-not (text-property-not-all (point-min) (point-max)
                                         'org-air-marked nil)))))

;;; r90-4/5 — marked backlog set-all in both directions.

(ert-deftest org-air-r90-4-marked-backlog-mixed-set-all-on ()
  "Mixed backlog state becomes all-ON once; co-tags and one-file grouping survive."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Plain A :red:\n* TODO Plain B :blue:\n* TODO Already C :backlog:green:\n")
        ("inbox.org" . "#+title: inbox\n"))
    (setq org-air-view--expanded-sections '(backlog))
    (org-air-view--render-current)
    (dolist (title '("Plain A" "Plain B" "Already C"))
      (org-air-r90--mark-title title))
    (let ((renders 0) (toggles nil))
      (cl-letf* ((render-orig (symbol-function 'org-air-view--render))
                 ((symbol-function 'org-air-view--render)
                  (lambda (&rest args) (cl-incf renders) (apply render-orig args)))
                 (toggle-orig (symbol-function 'org-toggle-tag))
                 ((symbol-function 'org-toggle-tag)
                  (lambda (tag state)
                    (push (list (org-get-heading t t t t) tag state) toggles)
                    (funcall toggle-orig tag state))))
        (org-air-item-backlog))
      (should (= renders 1))
      (should (= (length toggles) 2)))
    (dolist (title '("Plain A" "Plain B" "Already C"))
      (should (org-air-r90--file-has-tag-p "tasks.org" title "backlog")))
    (dolist (pair '(("Plain A" . "red") ("Plain B" . "blue")
                    ("Already C" . "green")))
      (should (org-air-r90--file-has-tag-p "tasks.org" (car pair) (cdr pair))))
    (should-not org-air-view--marked-keys)
    (should (= 1 (length org-air-view--edit-ring)))
    (let ((record (car org-air-view--edit-ring)))
      (should (eq 'bulk (plist-get record :kind)))
      (should (= 1 (length (plist-get record :parts)))))))

(ert-deftest org-air-r90-5-marked-backlog-all-on-set-all-off ()
  "An all-backlog marked set becomes all-OFF without disturbing co-tags."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Back A :backlog:red:\n* TODO Back B :backlog:blue:\n* TODO Back C :backlog:green:\n")
        ("inbox.org" . "#+title: inbox\n"))
    (setq org-air-view--expanded-sections '(backlog))
    (org-air-view--render-current)
    (dolist (title '("Back A" "Back B" "Back C"))
      (org-air-r90--mark-title title))
    (org-air-item-backlog)
    (dolist (title '("Back A" "Back B" "Back C"))
      (should-not (org-air-r90--file-has-tag-p "tasks.org" title "backlog")))
    (dolist (pair '(("Back A" . "red") ("Back B" . "blue")
                    ("Back C" . "green")))
      (should (org-air-r90--file-has-tag-p "tasks.org" (car pair) (cdr pair))))))

;;; r90-6 — hidden targets and rendered duplicate de-duplication.

(ert-deftest org-air-r90-6-hidden-marks-targeted-once-with-honest-counts ()
  "Filtering hides marks but bulk targets each unique source key exactly once."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO [#A] Alpha mirror\nDEADLINE: <2026-06-10 Wed>\n* TODO Beta hidden\n* TODO Gamma hidden\n")
        ("inbox.org" . "#+title: inbox\n"))
    (dolist (title '("Alpha mirror" "Beta hidden" "Gamma hidden"))
      (org-air-r90--mark-title title))
    (setq org-air-view--tag-filter '("Alpha"))
    (org-air-view--render-current)
    (should (equal "• 3 marked · 1 shown"
                   (org-air-view--marked-count-label org-air-view--items)))
    (let ((calls nil))
      (cl-letf* ((orig (symbol-function 'org-toggle-tag))
                 ((symbol-function 'org-toggle-tag)
                  (lambda (tag state)
                    (push (org-get-heading t t t t) calls)
                    (funcall orig tag state))))
        (org-air-item-backlog))
      (should (= 3 (length calls)))
      (should (= 3 (length (delete-dups calls)))))
    (dolist (title '("Alpha mirror" "Beta hidden" "Gamma hidden"))
      (should (org-air-r90--file-has-tag-p "tasks.org" title "backlog")))))

;;; r90-7 — stale/ineligible policy and exact generation reconciliation.

(ert-deftest org-air-r90-7-stale-pruned-ineligible-retained-and-exact-reconcile ()
  "Stale keys prune; ineligible keys remain; generation swaps use exact keys only."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Eligible task\n* DONE Finished task\n")
        ("note.org" . "#+title: Headingless note\n\nProse.\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((eligible (org-air-r90--item "Eligible task"))
           (done (org-air-r90--item "Finished task"))
           (note (org-air-r90--item "Headingless note"))
           (eligible-key (org-air-view--item-source-key eligible))
           (done-key (org-air-view--item-source-key done))
           (note-key (org-air-view--item-source-key note))
           (stale-key (cons (org-air-r90--file "missing.org") 42)))
      (setq org-air-view--marked-keys
            (list eligible-key done-key note-key stale-key)
            org-air-view--marked-generation org-air-view--items)
      (org-air-view--marked-table-rebuild)
      (org-air-r90--goto-row "Eligible task")
      (org-air-item-backlog)
      (should (org-air-r90--file-has-tag-p "tasks.org" "Eligible task" "backlog"))
      (should (= 2 (length org-air-view--marked-keys)))
      ;; Tag growth before Finished relocates its exact source key; surviving
      ;; ineligible marks follow that relocation rather than going stale.
      (let ((done-key-now (org-air-view--item-source-key done))
            (note-key-now (org-air-view--item-source-key note)))
        (should (member done-key-now org-air-view--marked-keys))
        (should (member note-key-now org-air-view--marked-keys))
        (should-not (member done-key org-air-view--marked-keys))
        (should-not (member stale-key org-air-view--marked-keys))
        ;; A fresh generation keeps an exact survivor but never guesses by title
        ;; and never reads a source file during reconciliation.
        (setq done-key done-key-now
              note-key note-key-now))
      (setq org-air-view--marked-keys (list done-key stale-key)
            org-air-view--marked-generation org-air-view--items)
      (org-air-view--marked-table-rebuild)
      (let ((fresh (mapcar #'copy-sequence org-air-view--items))
            (opens 0))
        (cl-letf (((symbol-function 'find-file-noselect)
                   (lambda (&rest _) (cl-incf opens) (error "file read"))))
          (org-air-view--marked-reconcile fresh))
        (should (= opens 0))
        (should (equal (list done-key) org-air-view--marked-keys))))))

;;; r90-8 — one shared tag prompt and no-op history laws.

(ert-deftest org-air-r90-8-marked-tag-one-prompt-noops-clear ()
  "Marked `t' prompts once, adds one value, clears successes, and groups history."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Tag A\n* TODO Tag B :shared:\n* TODO Tag C\n")
        ("inbox.org" . "#+title: inbox\n"))
    (dolist (title '("Tag A" "Tag B" "Tag C"))
      (org-air-r90--mark-title title))
    (let ((prompts 0))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (cl-incf prompts) "shared")))
        (org-air-set-tag))
      (should (= prompts 1)))
    (dolist (title '("Tag A" "Tag B" "Tag C"))
      (should (org-air-r90--file-has-tag-p "tasks.org" title "shared")))
    (should-not org-air-view--marked-keys)
    (should (eq 'bulk (plist-get (car org-air-view--edit-ring) :kind)))
    (should (= 1 (length (plist-get (car org-air-view--edit-ring) :parts))))))

(ert-deftest org-air-r90-8c-noop-and-stale-only-create-no-history-or-write ()
  "No-op-only and stale-prune-only commands never create source history."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Already tagged :shared:\n")
        ("inbox.org" . "#+title: inbox\n"))
    (org-air-r90--mark-title "Already tagged")
    (let ((before (org-air-r90--text "tasks.org")) (saves 0))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "shared"))
                ((symbol-function 'save-buffer)
                 (lambda (&rest _) (cl-incf saves) (error "unexpected save"))))
        (org-air-set-tag))
      (should (= saves 0))
      (should (equal before (org-air-r90--text "tasks.org")))
      (should-not org-air-view--edit-ring)
      (should-not org-air-view--marked-keys))
    (setq org-air-view--marked-keys
          (list (cons (org-air-r90--file "gone.org") 9))
          org-air-view--marked-generation org-air-view--items)
    (org-air-view--marked-table-rebuild)
    (let ((before (org-air-r90--text "tasks.org")) (saves 0))
      (cl-letf (((symbol-function 'save-buffer)
                 (lambda (&rest _) (cl-incf saves) (error "unexpected save"))))
        (org-air-view--marked-tag-action 'backlog))
      (should (= saves 0))
      (should (equal before (org-air-r90--text "tasks.org")))
      (should-not org-air-view--edit-ring)
      (should-not org-air-view--marked-keys))))

;;; r90-9 — unsupported command guards and non-mutation meanings.

(ert-deftest org-air-r90-9-unsupported-mutations-guard-before-prompts-writes ()
  "T/d/e/f/a/D/x/schedule/I all stop before any prompt or source write."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board org-air-r90--three
    (org-air-r90--mark-title "Alpha task")
    (let ((prompts 0) (writes 0))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (cl-incf prompts) "bad"))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) (cl-incf prompts) "bad"))
                ((symbol-function 'read-char-exclusive)
                 (lambda (&rest _) (cl-incf prompts) ?q))
                ((symbol-function 'yes-or-no-p)
                 (lambda (&rest _) (cl-incf prompts) t))
                ((symbol-function 'save-buffer)
                 (lambda (&rest _) (cl-incf writes) (error "write"))))
        (dolist (command '(org-air-item-cycle-todo org-air-item-deadline
                           org-air-refile-item org-air-item-file-group
                           org-air-item-archive org-air-item-done
                           org-air-item-kill org-air-item-schedule
                           org-air-process-inbox))
          (should-error (call-interactively command) :type 'user-error)))
      (should (= prompts 0))
      (should (= writes 0)))
    ;; Navigation, sort/filter/TAB and u/U retain their own dispatch rather
    ;; than being transformed into marked item actions.
    (let ((keys (copy-sequence org-air-view--marked-keys)))
      (org-air-r90--goto-row "Alpha task")
      (org-air-next-item)
      (should (equal "Beta task" (org-air-r90--row-title)))
      (org-air-view-sort-cycle)
      (setq org-air-view--tag-filter '("Alpha"))
      (org-air-view--render-current)
      (should-error (org-air-edit-undo) :type 'user-error)
      (should-error (org-air-edit-redo) :type 'user-error)
      (should (equal keys org-air-view--marked-keys)))))

;;; r90-10 — one preflight/file group/save, deterministic order, no scans.

(ert-deftest org-air-r90-10-write-discipline-one-repaint-zero-scans ()
  "Bulk write is one preflight, one group/save per file, descending and no-scan."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("a.org" . "#+title: a\n\n* TODO A first\n* TODO A second\n")
        ("b.org" . "#+title: b\n\n* TODO B only\n")
        ("inbox.org" . "#+title: inbox\n"))
    (dolist (title '("A first" "A second" "B only"))
      (org-air-r90--mark-title title))
    ;; Exclude org-mode's one-time source-buffer setup timers from this
    ;; coordinator seam; the command itself must construct none.
    (find-file-noselect (org-air-r90--file "a.org"))
    (find-file-noselect (org-air-r90--file "b.org"))
    (let ((preflights 0) (groups 0) (renders 0) (queries 0)
          (refreshes 0) (timers 0) saves toggles)
      (cl-letf* ((pre-orig (symbol-function 'org-air-view--bulk-preflight))
                 ((symbol-function 'org-air-view--bulk-preflight)
                  (lambda (&rest args)
                    (cl-incf preflights) (apply pre-orig args)))
                 (group-orig (symbol-function 'prepare-change-group))
                 ((symbol-function 'prepare-change-group)
                  (lambda (&rest args)
                    (cl-incf groups) (apply group-orig args)))
                 (save-orig (symbol-function 'save-buffer))
                 ((symbol-function 'save-buffer)
                  (lambda (&rest args)
                    (push (buffer-file-name) saves)
                    (apply save-orig args)))
                 (toggle-orig (symbol-function 'org-toggle-tag))
                 ((symbol-function 'org-toggle-tag)
                  (lambda (tag state)
                    (push (cons (buffer-file-name) (point)) toggles)
                    (funcall toggle-orig tag state)))
                 (render-orig (symbol-function 'org-air-view--render))
                 ((symbol-function 'org-air-view--render)
                  (lambda (&rest args) (cl-incf renders) (apply render-orig args)))
                 ((symbol-function 'org-air-query-items)
                  (lambda (&rest _) (cl-incf queries) (error "query")))
                 ((symbol-function 'org-air-query-items-in-files)
                  (lambda (&rest _) (cl-incf queries) (error "query")))
                 ((symbol-function 'org-air-refresh)
                  (lambda (&rest _) (cl-incf refreshes) (error "refresh")))
                 ((symbol-function 'run-with-timer)
                  (lambda (&rest _) (cl-incf timers) nil))
                 ((symbol-function 'run-with-idle-timer)
                  (lambda (&rest _) (cl-incf timers) nil)))
        (org-air-item-backlog))
      (setq saves (nreverse saves) toggles (nreverse toggles))
      (should (= preflights 1))
      (should (= groups 2))
      (should (= renders 1))
      (should (= queries 0))
      (should (= refreshes 0))
      (should (= timers 0))
      (should (equal saves (sort (copy-sequence saves) #'string<)))
      (should (= 1 (seq-count (lambda (file) (equal file (org-air-r90--file "a.org"))) saves)))
      (should (= 1 (seq-count (lambda (file) (equal file (org-air-r90--file "b.org"))) saves)))
      (let ((a-positions (mapcar #'cdr
                                 (seq-filter
                                  (lambda (entry)
                                    (equal (car entry) (org-air-r90--file "a.org")))
                                  toggles))))
        (should (= 2 (length a-positions)))
        (should (> (car a-positions) (cadr a-positions)))))))

;;; r90-11 — same-file rollback and cross-file stop-on-failure.

(ert-deftest org-air-r90-11-file-atomicity-and-partial-commit ()
  "A same-file signal rolls back all; a later-file failure stops the suffix."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("a.org" . "#+title: a\n\n* TODO A first\n* TODO A second\n")
        ("inbox.org" . "#+title: inbox\n"))
    (dolist (title '("A first" "A second")) (org-air-r90--mark-title title))
    (let ((before (org-air-r90--text "a.org")) (calls 0))
      (cl-letf* ((orig (symbol-function 'org-toggle-tag))
                 ((symbol-function 'org-toggle-tag)
                  (lambda (&rest args)
                    (cl-incf calls)
                    (if (= calls 2) (error "second heading failure")
                      (apply orig args)))))
        (org-air-item-backlog))
      (should (equal before (org-air-r90--text "a.org")))
      (should (equal before
                     (with-current-buffer (find-file-noselect
                                           (org-air-r90--file "a.org"))
                       (buffer-string))))
      (should (= 2 (length org-air-view--marked-keys)))
      (should-not org-air-view--edit-ring)))
  (org-air-r90--with-board
      '(("a.org" . "#+title: a\n\n* TODO A task\n")
        ("b.org" . "#+title: b\n\n* TODO B task\n")
        ("c.org" . "#+title: c\n\n* TODO C task\n")
        ("inbox.org" . "#+title: inbox\n"))
    (dolist (title '("A task" "B task" "C task")) (org-air-r90--mark-title title))
    (let ((save-orig (symbol-function 'save-buffer)))
      (cl-letf (((symbol-function 'save-buffer)
                 (lambda (&rest args)
                   (if (equal (buffer-file-name) (org-air-r90--file "b.org"))
                       (error "b save failure")
                     (apply save-orig args)))))
        (org-air-item-backlog)))
    (should (org-air-r90--file-has-tag-p "a.org" "A task" "backlog"))
    (should-not (org-air-r90--file-has-tag-p "b.org" "B task" "backlog"))
    (should-not (org-air-r90--file-has-tag-p "c.org" "C task" "backlog"))
    (should (= 2 (length org-air-view--marked-keys)))
    (should (= 1 (length org-air-view--edit-ring)))
    (should (= 1 (length (plist-get (car org-air-view--edit-ring) :parts))))))

;;; r90-12 — relocation, second no-scan write, inherited-tag audit.

(ert-deftest org-air-r90-12-relocates-all-later-headings-for-second-action ()
  "Tag growth relocates untouched later headings before a second no-scan action."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO First heading\n* TODO Second heading\n* TODO Later untouched\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((later (org-air-r90--item "Later untouched"))
           (old-pos (cdr (org-air-view--item-source-key later))))
      (org-air-r90--mark-title "First heading")
      (org-air-r90--mark-title "Second heading")
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "a-very-long-shared-tag")))
        (org-air-set-tag))
      (let ((new-pos (cdr (org-air-view--item-source-key later))))
        (should (> new-pos old-pos))
        (should (= new-pos (org-air-r90--actual-heading-position
                            "tasks.org" "Later untouched"))))
      (let ((queries 0))
        (org-air-r90--goto-row "Later untouched")
        (cl-letf (((symbol-function 'org-air-query-items)
                   (lambda (&rest _) (cl-incf queries) (error "query")))
                  ((symbol-function 'org-air-query-items-in-files)
                   (lambda (&rest _) (cl-incf queries) (error "query"))))
          (org-air-item-backlog))
        (should (= queries 0)))
      (should (org-air-r90--file-has-tag-p
               "tasks.org" "Later untouched" "backlog"))
      (should-not (org-air-r90--file-has-tag-p
                   "tasks.org" "Second heading" "backlog")))))

(ert-deftest org-air-r90-12-inherited-effective-tags-do-not-reject-valid-heading ()
  "Exact preflight accepts inherited cached tags absent from the local tag string."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* Project :inherited:\n** TODO Inherited child :local:\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let ((child (org-air-r90--item "Inherited child")))
      (should (member "inherited" (org-air-item-tags child)))
      (should (member "local" (org-air-item-tags child))))
    (org-air-r90--mark-title "Inherited child")
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "shared")))
      (org-air-set-tag))
    ;; A cached effective-tag superset is valid source truth, not a title/
    ;; position mismatch.  Rejecting it here is an R90 source blocker.
    (should (org-air-r90--file-has-tag-p
             "tasks.org" "Inherited child" "shared"))
    (should-not org-air-view--marked-keys)
    (should (eq 'bulk (plist-get (car org-air-view--edit-ring) :kind)))))

(ert-deftest org-air-r90-hostile-b-post-save-sync-cannot-diverge ()
  "A post-save relocation signal cannot split disk/cache/marks/history truth."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO First heading\n* TODO Later heading\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((first (org-air-r90--item "First heading"))
           (later (org-air-r90--item "Later heading"))
           (first-key (org-air-view--item-source-key first)))
      (org-air-r90--mark-title "First heading")
      ;; `save-buffer' succeeds inside the real coordinator.  The injected
      ;; signal is strictly after that save, in cache relocation sync.
      (let ((actual (org-air-r90--hostile-b-snapshot
                     first later first-key)))
        ;; Once disk save succeeds the only truthful outcome is a fully
        ;; synchronized commit.  Current code must not report a failed target
        ;; while retaining stale positions/mark and omitting history.
        (should (equal actual
                       '(:disk t :live t :cache-tag t :position t
                         :marked nil :history bulk)))))))

;;; r90-13 — one top compound record for one/many/cross-file shapes.

(ert-deftest org-air-r90-13-one-top-record-and-one-part-per-file ()
  "One marked command creates one top record and exactly one part per file."
  (skip-unless (locate-library "org-air"))
  (dolist (shape '(one many cross))
    (org-air-r90--with-board
        (pcase shape
          ('one '(("a.org" . "#+title: a\n\n* TODO A one\n")
                  ("inbox.org" . "#+title: inbox\n")))
          ('many '(("a.org" . "#+title: a\n\n* TODO A one\n* TODO A two\n")
                   ("inbox.org" . "#+title: inbox\n")))
          ('cross '(("a.org" . "#+title: a\n\n* TODO A one\n")
                    ("b.org" . "#+title: b\n\n* TODO B one\n")
                    ("inbox.org" . "#+title: inbox\n"))))
      (dolist (title (pcase shape
                       ('one '("A one"))
                       ('many '("A one" "A two"))
                       ('cross '("A one" "B one"))))
        (org-air-r90--mark-title title))
      (org-air-item-backlog)
      (should (= 1 (length org-air-view--edit-ring)))
      (let ((record (car org-air-view--edit-ring)))
        (should (eq 'bulk (plist-get record :kind)))
        (should (= (if (eq shape 'cross) 2 1)
                   (length (plist-get record :parts))))))))

;;; r90-14 — healthy compound round trips and redo clearing.

(ert-deftest org-air-r90-14-compound-u-U-roundtrip-all-shapes ()
  "Healthy one/many/cross-file compound records byte-round-trip through u/U."
  (skip-unless (locate-library "org-air"))
  (dolist (shape '(one many cross))
    (org-air-r90--with-board
        (pcase shape
          ('one '(("a.org" . "#+title: a\n\n* TODO A one\n")
                  ("inbox.org" . "#+title: inbox\n")))
          ('many '(("a.org" . "#+title: a\n\n* TODO A one\n* TODO A two\n")
                   ("inbox.org" . "#+title: inbox\n")))
          ('cross '(("a.org" . "#+title: a\n\n* TODO A one\n")
                    ("b.org" . "#+title: b\n\n* TODO B one\n")
                    ("inbox.org" . "#+title: inbox\n"))))
      (let* ((names (if (eq shape 'cross) '("a.org" "b.org") '("a.org")))
             (before (mapcar (lambda (name) (cons name (org-air-r90--text name)))
                             names)))
        (dolist (title (pcase shape
                         ('one '("A one"))
                         ('many '("A one" "A two"))
                         ('cross '("A one" "B one"))))
          (org-air-r90--mark-title title))
        (org-air-item-backlog)
        (let ((edited (mapcar (lambda (name) (cons name (org-air-r90--text name)))
                              names)))
          (should-not org-air-view--marked-keys)
          (setq last-command 'ignore)
          (org-air-edit-undo)
          (dolist (entry before)
            (should (equal (cdr entry) (org-air-r90--text (car entry)))))
          (should-not org-air-view--marked-keys)
          (setq last-command 'ignore)
          (org-air-edit-redo)
          (dolist (entry edited)
            (should (equal (cdr entry) (org-air-r90--text (car entry)))))
          (should-not org-air-view--marked-keys)))))
  ;; A fresh edit after compound undo clears redo and marks never resurrect.
  (org-air-r90--with-board org-air-r90--three
    (org-air-r90--mark-title "Alpha task")
    (org-air-item-backlog)
    (org-air-edit-undo)
    (should (= 1 (length org-air-view--edit-redo-ring)))
    (org-air-r90--goto-row "Beta task")
    (org-air-item-backlog)
    (should-not org-air-view--edit-redo-ring)
    (should-not org-air-view--marked-keys)))

;;; r90-15 — compound preflight and hostile runtime residual laws.

(ert-deftest org-air-r90-15-compound-preflight-is-zero-byte-and-nonconsuming ()
  "Dead/tick/head blockers are detected up front and unrelated undo remains."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("a.org" . "#+title: a\n\n* TODO A task\n")
        ("b.org" . "#+title: b\n\n* TODO B task\n")
        ("inbox.org" . "#+title: inbox\n"))
    (dolist (title '("A task" "B task")) (org-air-r90--mark-title title))
    (org-air-item-backlog)
    (let* ((record (car org-air-view--edit-ring))
           (parts (plist-get record :parts))
           (live (plist-get (car parts) :buffer))
           (dead (generate-new-buffer "r90-dead-part")))
      (kill-buffer dead)
      (should (= 3 (length
                    (org-air-view--bulk-history-blockers
                     (list (list :buffer dead :file "/tmp/dead.org" :tick 0)
                           (list :buffer live :file "/tmp/tick.org"
                                 :tick (1- (buffer-chars-modified-tick live))
                                 :undo-head (org-air-view--undo-head live))
                           (list :buffer live :file "/tmp/head.org"
                                 :tick (buffer-chars-modified-tick live)
                                 :undo-head (list 'missing)))))))
      ;; Put one unrelated saved edit atop a committed bulk part.  The bulk
      ;; head/tick preflight must move zero bytes and not consume that step.
      (with-current-buffer live
        (goto-char (point-max))
        (insert "unrelated\n")
        (save-buffer))
      (let ((before-a (org-air-r90--text "a.org"))
            (before-b (org-air-r90--text "b.org")))
        (org-air-edit-undo)
        (should (equal before-a (org-air-r90--text "a.org")))
        (should (equal before-b (org-air-r90--text "b.org"))))
      (with-current-buffer live
        (undo-boundary)
        (undo-only)
        (save-buffer))
      (should-not (string-match-p "unrelated" (org-air-r90--text "a.org"))))))

(ert-deftest org-air-r90-15a-runtime-undo-retains-failed-current-part ()
  "Runtime undo failure retains the failed current and every untouched part."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("a.org" . "#+title: a\n\n* TODO A task\n")
        ("b.org" . "#+title: b\n\n* TODO B task\n")
        ("c.org" . "#+title: c\n\n* TODO C task\n")
        ("inbox.org" . "#+title: inbox\n"))
    (dolist (title '("A task" "B task" "C task")) (org-air-r90--mark-title title))
    (org-air-item-backlog)
    (let ((save-orig (symbol-function 'save-buffer)) messages)
      (cl-letf* (((symbol-function 'save-buffer)
                  (lambda (&rest args)
                    (if (equal (buffer-file-name) (org-air-r90--file "b.org"))
                        (error "runtime undo save failure")
                      (apply save-orig args))))
                 ((symbol-function 'message)
                  (lambda (fmt &rest args)
                   (when fmt (push (apply #'format fmt args) messages)) nil)))
        (org-air-edit-undo))
      (should (seq-some (lambda (msg)
                          (string-match-p "Undo incomplete: 1/3 files reverted; failed b.org" msg))
                        messages)))
    (should-not (org-air-r90--file-has-tag-p "c.org" "C task" "backlog"))
    (should (org-air-r90--file-has-tag-p "b.org" "B task" "backlog"))
    (should (org-air-r90--file-has-tag-p "a.org" "A task" "backlog"))
    (should-not org-air-view--edit-redo-ring)
    (let* ((residual (car org-air-view--edit-ring))
           (files (sort (mapcar (lambda (part)
                                 (file-name-nondirectory (plist-get part :file)))
                               (plist-get residual :parts))
                        #'string<)))
      ;; The failed current part was popped from the local `remaining' list,
      ;; but is still committed and MUST join untouched a.org in residual.
      (should (equal '("a.org" "b.org") files)))))

(ert-deftest org-air-r90-15b-runtime-redo-has-truthful-residual ()
  "Runtime redo failure reports exact counts, creates no redo, and keeps truth."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("a.org" . "#+title: a\n\n* TODO A task\n")
        ("b.org" . "#+title: b\n\n* TODO B task\n")
        ("c.org" . "#+title: c\n\n* TODO C task\n")
        ("inbox.org" . "#+title: inbox\n"))
    (dolist (title '("A task" "B task" "C task")) (org-air-r90--mark-title title))
    (org-air-item-backlog)
    (org-air-edit-undo)
    (let ((save-orig (symbol-function 'save-buffer)) messages)
      (cl-letf* (((symbol-function 'save-buffer)
                  (lambda (&rest args)
                    (if (equal (buffer-file-name) (org-air-r90--file "b.org"))
                        (error "runtime redo save failure")
                      (apply save-orig args))))
                 ((symbol-function 'message)
                  (lambda (fmt &rest args)
                   (when fmt (push (apply #'format fmt args) messages)) nil)))
        (org-air-edit-redo))
      (should (seq-some (lambda (msg)
                          (string-match-p "Redo incomplete: 1/3 files reapplied; failed b.org" msg))
                        messages)))
    (let* ((parts (plist-get (car org-air-view--edit-ring) :parts))
           (actual
            (list :a-disk (org-air-r90--disk-has-tag-p
                           "a.org" "A task" "backlog")
                  :a-live (and (org-air-r90--file-has-tag-p
                                "a.org" "A task" "backlog") t)
                  :b-disk (org-air-r90--disk-has-tag-p
                           "b.org" "B task" "backlog")
                  :b-live (and (org-air-r90--file-has-tag-p
                                "b.org" "B task" "backlog") t)
                  :c-disk (org-air-r90--disk-has-tag-p
                           "c.org" "C task" "backlog")
                  :redo (and org-air-view--edit-redo-ring t)
                  :residual (mapcar
                             (lambda (part)
                               (file-name-nondirectory
                                (plist-get part :file)))
                             parts))))
      ;; b.org's failed redo must be rolled back in the live buffer as well
      ;; as remaining absent on disk; only successful a.org is residual undo.
      (should (equal actual
                     '(:a-disk t :a-live t :b-disk nil :b-live nil
                       :c-disk nil :redo nil :residual ("a.org")))))))

;;; r90-16/17/18 — Backlog collapse, persistence, and explicit lens.

(ert-deftest org-air-r90-16-cold-backlog-header-tab-cycle-no-query ()
  "Cold Backlog is header-only; TAB reveals all then collapses without scans."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Cold A :backlog:\n* TODO Cold B :backlog:\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "Backlog" text))
      (should-not (string-match-p "Cold A" text))
      (should-not (string-match-p "Cold B" text))
      (should-not (string-match-p "and [0-9]+ more" text)))
    (should (= 2 (cdr (assq 'backlog
                            (org-air-view--section-counts org-air-view--items)))))
    (let ((queries 0) (key org-air-view--items-key))
      (cl-letf (((symbol-function 'org-air-query-items)
                 (lambda (&rest _) (cl-incf queries) (error "query")))
                ((symbol-function 'org-air-query-items-in-files)
                 (lambda (&rest _) (cl-incf queries) (error "query"))))
        (goto-char (org-air-view--find-property 'org-air-section 'backlog))
        (org-air-toggle-section)
        (should (string-match-p "Cold A" (buffer-substring-no-properties
                                           (point-min) (point-max))))
        (should (string-match-p "Cold B" (buffer-substring-no-properties
                                           (point-min) (point-max))))
        (goto-char (org-air-view--find-property 'org-air-section 'backlog))
        (org-air-toggle-section))
      (should (= queries 0))
      (should (equal key org-air-view--items-key)))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should-not (string-match-p "Cold A" text))
      (should-not (string-match-p "Cold B" text)))))

(ert-deftest org-air-r90-17-expansion-survives-refresh-and-bookmark-shapes ()
  "Explicit Backlog expansion survives repaint/swap/final and bookmark restore."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Persist A :backlog:\n* TODO Persist B :backlog:\n")
        ("inbox.org" . "#+title: inbox\n"))
    (setq org-air-view--expanded-sections '(backlog))
    (org-air-view--render-current)
    (org-air-view--refresh-repaint)
    (should (memq 'backlog org-air-view--expanded-sections))
    ;; Real no-change and sync-changed refresh tails.
    (org-air-refresh)
    (should (memq 'backlog org-air-view--expanded-sections))
    (with-temp-buffer
      (insert-file-contents (org-air-r90--file "tasks.org"))
      (goto-char (point-max))
      (insert "\n")
      (write-region nil nil (org-air-r90--file "tasks.org") nil 'silent))
    (org-air-refresh)
    (should (memq 'backlog org-air-view--expanded-sections))
    ;; The paced final-swap tail, driven without timers.
    (setq org-air-view--refresh-acc org-air-view--items
          org-air-view--refresh-mtimes org-air-view--items-mtimes
          org-air-view--cache-stale-files nil)
    (org-air-view--refresh-finish)
    (should (memq 'backlog org-air-view--expanded-sections))
    (let ((record (org-air-view--bookmark-make-record)))
      (should (equal '(backlog) (cdr (assq 'org-air-expanded record))))
      (setq org-air-view--expanded-sections nil)
      (org-air-view--bookmark-apply record)
      (should (memq 'backlog org-air-view--expanded-sections)))
    ;; Missing/old expansion fields default honestly collapsed.
    (org-air-view--bookmark-apply '((org-air-version . 1)))
    (should-not org-air-view--expanded-sections)))

(ert-deftest org-air-r90-18-explicit-lens-forces-reveal-knob-honest ()
  "is:backlog forces descriptor/reveal; #backlog and knob nil do not."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Lens task :backlog:\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let ((items org-air-view--items) (queries 0))
      (cl-letf (((symbol-function 'org-air-query-items)
                 (lambda (&rest _) (cl-incf queries) (error "query")))
                ((symbol-function 'org-air-query-items-in-files)
                 (lambda (&rest _) (cl-incf queries) (error "query"))))
        ;; Raw membership never auto-expands.
        (setq org-air-view--expanded-sections nil)
        (org-air-filter-by-tag "#backlog")
        (should-not (memq 'backlog org-air-view--expanded-sections))
        (should-not (string-match-p "Lens task" (buffer-substring-no-properties
                                                  (point-min) (point-max))))
        ;; Exact lens reveals under the normal knob.
        (org-air-filter-by-tag "is:backlog")
        (should (memq 'backlog org-air-view--expanded-sections))
        (should (assq 'backlog (org-air-view--section-descriptors items)))
        (should (string-match-p "Lens task" (buffer-substring-no-properties
                                              (point-min) (point-max))))
        ;; Exact lens also overrides knob nil; clearing it removes both
        ;; ordinary section and Summary row.
        (let ((org-air-show-backlog-section nil))
          (setq org-air-view--expanded-sections nil)
          (org-air-filter-by-tag "is:backlog")
          (should (assq 'backlog (org-air-view--section-descriptors items)))
          (should (memq 'backlog org-air-view--expanded-sections))
          (org-air-filter-clear)
          (should-not (assq 'backlog (org-air-view--section-descriptors items)))
          (should-not (assq 'backlog (org-air-view--section-counts items)))))
      (should (= queries 0))
      (should (eq items org-air-view--items)))))

;;; r90-19 — final-item chrome and direct pane/inspector convergence.

(ert-deftest org-air-r90-19-final-item-lands-on-chrome-before-resync ()
  "The final moved row leaves honest chrome, closes pane, and nils inspector."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Only visible task\n")
        ("inbox.org" . "#+title: inbox\n"))
    (org-air-r90--goto-row "Only visible task")
    (find-file-noselect (org-air-r90--file "tasks.org"))
    (let ((hide 0) setup-states (queries 0) (timers 0))
      (cl-letf* ((setup-orig (symbol-function 'org-air-view--setup-inspector))
                 ((symbol-function 'org-air-view--setup-inspector)
                  (lambda ()
                    (push (list org-air-view--pending-mutation-landing
                                (org-air-r90--row-title)) setup-states)
                    (funcall setup-orig)))
                 ((symbol-function 'org-air-view-pane--window-live-p)
                  (lambda () t))
                 ((symbol-function 'org-air-view-pane--hide)
                  (lambda () (cl-incf hide)))
                 (query-orig (symbol-function 'org-air-query-items))
                 ((symbol-function 'org-air-query-items)
                  (lambda (&rest args)
                    (cl-incf queries) (apply query-orig args)))
                 ((symbol-function 'run-with-timer)
                  (lambda (&rest _) (cl-incf timers) nil))
                 ((symbol-function 'run-with-idle-timer)
                  (lambda (&rest _) (cl-incf timers) nil)))
        (org-air-item-backlog))
      (should (> hide 0))
      (should (equal '(nil nil) (car setup-states)))
      (should-not (org-air-view--row-property 'org-air-item))
      (should-not (org-air-view-pane--context-at-point))
      (should-not org-air-view--inspector-item)
      (should-not org-air-view--pending-mutation-landing)
      (should (string-match-p "Backlog"
                              (buffer-substring-no-properties
                               (point-min) (point-max))))
      (should (= queries 0))
      (should (= timers 0)))))

;;; r90-20 — discoverability, key ownership, review collision, and bytes.

(ert-deftest org-air-r90-20-key-help-status-readme-and-empty-bytes ()
  "Board keys are owned/strippable; mark UI teaches bulk; empty bytes restore."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-keybinding-knob t
    (with-temp-buffer
      (org-air-view-mode)
      (should (eq (key-binding (kbd "m")) 'org-air-toggle-mark))
      (should (eq (key-binding (kbd "M")) 'org-air-clear-marks))
      (should (eq (key-binding (kbd "b")) 'org-air-item-backlog))
      (should (eq (key-binding (kbd "t")) 'org-air-set-tag)))
    (with-temp-buffer
      (org-air-review-mode)
      (should (eq (key-binding (kbd "m")) 'org-air-review-cycle-range))))
  (org-air-r90--with-keybinding-knob nil
    (with-temp-buffer
      (org-air-view-mode)
      (pcase-dolist (`(,key . ,command)
                      '(("m" . org-air-toggle-mark)
                        ("M" . org-air-clear-marks)
                        ("b" . org-air-item-backlog)
                        ("t" . org-air-set-tag)))
        ;; `undefined' is Emacs's explicit unbound sentinel and is fine;
        ;; the installer-owned command itself must be absent.
        (should-not (eq (key-binding (kbd key)) command)))))
  (org-air-r90--with-board org-air-r90--three
    (let ((empty-bytes (buffer-substring-no-properties (point-min) (point-max))))
      (org-air-clear-marks)
      (should (equal empty-bytes
                     (buffer-substring-no-properties (point-min) (point-max))))
      (org-air-r90--mark-title "Alpha task")
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "• 1 marked" text))
        (should (string-match-p "backlog all" text))
        (should (string-match-p "tag all" text))
        (should (string-match-p "clear marks" text)))
      (unwind-protect
          (let ((help (org-air-help--render
                       (get-buffer-create "*r90-help*") 'board
                       (current-buffer))))
            (with-current-buffer help
              (let ((text (buffer-substring-no-properties (point-min) (point-max))))
                (should (string-match-p "Marked items" text))
                (should (string-match-p "hidden marks are included" text))
                (should (string-match-p "b backlogs all" text))
                (should (string-match-p "M clears marks" text)))))
        (when (get-buffer "*r90-help*") (kill-buffer "*r90-help*")))
      (org-air-clear-marks)
      (should (equal empty-bytes
                     (buffer-substring-no-properties (point-min) (point-max))))))
  (let ((readme (with-temp-buffer
                  (insert-file-contents
                   (expand-file-name "README.org" default-directory))
                  (buffer-string))))
    (should (string-match-p "header-only" readme))
    (should (string-match-p "hidden marks remain targets" readme))
    (should (string-match-p "set-all semantics" readme))))

(provide 'org-air-round90-test)
;;; org-air-round90-test.el ends here
