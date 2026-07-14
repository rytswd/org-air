;;; org-air-query.el --- Org-QL data layer for org-air -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: org-air contributors
;; Keywords: outlines, calendar
;; Version: 0.1.0
;; URL: https://github.com/rytswd/org-air
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Normalise Org headings from `org-air-files' into `org-air-item' records.
;;
;; R53: the scan is a WORK-BUFFER scan — org-ql stays the only query
;; engine, but org-air now hands it buffers IT manages (one reused work
;; buffer per session, or a user's live buffer) instead of letting it
;; `find-file-noselect' every file.  That kills the measured O(n^2)
;; `buffer-list' cost (271.8s -> 3.41s at 5006 files), retains ZERO source
;; buffers, and lets the per-file body be wrapped in the never-error law:
;; a signalling file (encrypted, unreadable, binary, vanished) contributes
;; 0 items and one skip-log entry — it can NEVER abort a whole scan.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-ql)
(require 'seq)

(defvar org-air-files)
(defvar org-air-inbox-file)

(defcustom org-air-todo-keywords
  '(:not-done ("TODO" "NEXT" "STARTED" "WAIT" "WAITING" "HOLD" "BLOCKED")
    :done     ("DONE" "CANCELLED" "CANCELED" "KILL"))
  "TODO keyword vocabulary org-air recognises when a file declares none.
The active (:not-done) and :done keyword sets org-air falls back to so a
heading like `* NEXT Foo' is parsed as a NEXT task even in a file without
a `#+TODO:' line.  A file's OWN `#+TODO:'/`#+SEQ_TODO:' always wins; this
only fills the gap.  Defaults mirror the keys of
`org-air-todo-keyword-faces' plus the standard done keywords (R21-3)."
  :type '(plist :key-type symbol :value-type (repeat string))
  :group 'org-air)

(defcustom org-air-max-file-size (* 4 1024 1024)
  "Largest file (bytes) the background scan will read; nil = no limit (R53).
A file over the limit is skipped with a `too-large' entry in the scan
report (`org-air-scan-report') instead of stalling a slice — the generic
monster-file valve of the never-hang contract."
  :type '(choice (const :tag "No limit" nil) integer)
  :group 'org-air)

(defun org-air-query--scan-todo-keywords ()
  "Return an `org-todo-keywords' value merging org-air's vocabulary (R21-3).
One sequence: the :not-done keywords, then `|', then the :done keywords.
Let-bound around the org-ql scan so a file WITHOUT its own `#+TODO:'
inherits org-air's NEXT/WAIT/... vocabulary (otherwise the keyword is
swallowed into the title), while a file WITH a `#+TODO:'/`#+SEQ_TODO:'
line still parses with its own (Org's per-file keywords win over the
default)."
  `((sequence
     ,@(plist-get org-air-todo-keywords :not-done)
     "|"
     ,@(plist-get org-air-todo-keywords :done))))

(cl-defstruct (org-air-item
               (:constructor org-air-item-create)
               (:copier nil))
  "A normalised Org heading (or R53 note file) for org-air views.
R53 P2 (cache v2): `kind', `donep', `activity' and `body-deadline' are
SCAN-TIME slots — everything classify/render needs lives in the struct,
so painting a cache-hydrated board never opens a file."
  title tags file marker todo priority scheduled deadline group closed
  ;; R53 scan-time slots (data-pure render):
  kind          ; 'heading | 'file (P3 headingless note file-item)
  donep         ; non-nil when todo ∈ the file's own `org-done-keywords'
  activity      ; epoch float: closed‖scheduled‖deadline‖first subtree ts‖mtime
  subtree-ts    ; epoch float of the first timestamp in the subtree BODY,
                ; or nil (R53fix B1: the day view's Logged/created key —
                ; distinct from `activity', whose mtime fallback must
                ; never fill that group)
  body-deadline) ; epoch float of the first subtree DEADLINE: when the
                 ; heading itself has none (the calendar's origin check)

(defun org-air-query--org-file-p (file)
  "Return non-nil when FILE is an Org file."
  (and (stringp file)
       (file-regular-p file)
       (string-match-p "\\.org\\(?:\\.gpg\\)?\\'" file)))

(defun org-air-query--expand-source (source)
  "Expand SOURCE, which may be a file or directory, to Org files."
  (let ((path (expand-file-name source)))
    (cond
     ((file-directory-p path)
      (directory-files-recursively path "\\.org\\(?:\\.gpg\\)?\\'" nil))
     ((org-air-query--org-file-p path) (list path))
     (t nil))))

(defun org-air-query-files ()
  "Return all existing Org files configured in `org-air-files'.
R53 P1d: order-preserving hash-table dedupe; `file-truename' is paid ONLY
for actual symlinks (`file-symlink-p' pre-check) so a 5000-file tree
enumerates in milliseconds while a symlinked duplicate still dedupes to
its target (measured 0.647s -> 0.044s at 5006 files)."
  (let ((seen (make-hash-table :test #'equal))
        (out nil))
    (dolist (file (seq-mapcat #'org-air-query--expand-source org-air-files))
      (let ((path (if (file-symlink-p file)
                      (or (ignore-errors (file-truename file)) file)
                    file)))
        (unless (gethash path seen)
          (puthash path t seen)
          (push path out))))
    (nreverse out)))

(defun org-air-query--timestamp (property)
  "Return Org timestamp object for PROPERTY at point, or nil."
  (when-let* ((value (org-entry-get (point) property)))
    (ignore-errors (org-timestamp-from-string value))))

(defun org-air-query--time-float (timestamp)
  "Return TIMESTAMP (an Org timestamp object) as an epoch float, or nil."
  (when timestamp
    (ignore-errors (float-time (org-timestamp-to-time timestamp)))))

(defun org-air-query--group (file)
  "Return display group for heading in FILE."
  (or (org-entry-get (point) "CATEGORY")
      (file-name-base file)))

(defvar org-air-query--scan-mtime nil
  "The scanned file's modification time, bound per file by the scan (R53).
Lets `org-air-query--item-at-point' seed the `activity' slot's mtime
fallback from the stat the scan already paid, instead of a per-item
re-stat.")

(defun org-air-query--item-at-point ()
  "Build an `org-air-item' for the heading at point.
R53 P2: also records the scan-time slots (`kind'/`donep'/`activity'/
`body-deadline') so classify/render never open the file again, and the
marker slot is the durable (FILE . POS) cons (source buffers are never
retained by scanning; live positions resolve on demand)."
  (let* ((file (or (buffer-file-name) ""))
         ;; R23-1: `org-get-heading' returns a FONTIFIED title (with `face
         ;; org-level-1') once the source Org buffer is live + fontified
         ;; (e.g. after a refile).  Strip all text-properties at the data
         ;; layer so the struct title is a plain string and no caller leaks
         ;; org heading faces into the calm one-line row (V6 pixel-lock).
         (title (substring-no-properties (org-get-heading t t t t)))
         (todo (org-get-todo-state))
         (scheduled (org-air-query--timestamp "SCHEDULED"))
         (deadline (org-air-query--timestamp "DEADLINE"))
         (closed (org-air-query--timestamp "CLOSED"))
         (subtree-ts nil)
         (body-deadline nil))
    ;; R53 P2: the two bounded subtree probes, run HERE in the already-
    ;; positioned scan buffer (they used to be per-item render-time file
    ;; opens — the 186s warm-paint hang).
    (save-excursion
      (let ((end (save-excursion (ignore-errors (org-end-of-subtree t t))
                                 (point))))
        (save-excursion
          (when (re-search-forward org-ts-regexp-both end t)
            (setq subtree-ts
                  (ignore-errors
                    (float-time
                     (org-timestamp-to-time
                      (org-timestamp-from-string
                       (match-string-no-properties 0))))))))
        (unless deadline
          (save-excursion
            (when (re-search-forward org-deadline-time-regexp end t)
              (setq body-deadline
                    (ignore-errors
                      (float-time
                       (org-timestamp-to-time
                        (org-timestamp-from-string
                         (format "<%s>"
                                 (match-string-no-properties 1))))))))))))
    (org-air-item-create
     :title title
     :tags (org-get-tags nil nil)
     :file file
     ;; R53 P1: (FILE . POS), first-class everywhere since R26-8 — the
     ;; scan retains NO buffer.  A file-less buffer (a test temp buffer)
     ;; keeps the live marker so at-point flows still resolve.
     :marker (if (string-empty-p file)
                 (copy-marker (point-marker))
               (cons file (point)))
     :todo todo
     ;; R22-1: detect an EXPLICIT [#X] cookie via `org-priority-regexp'
     ;; (group 2 = the letter, A..E), so [#B] is recorded even though its
     ;; value equals `org-default-priority' (=?B); a cookie-LESS heading
     ;; stays nil.  The old value-equals-default test dropped explicit [#B].
     :priority (let ((heading (org-get-heading t t nil t)))
                 (when (string-match org-priority-regexp heading)
                   (org-get-priority heading)))
     :scheduled scheduled
     :deadline deadline
     :closed closed
     :group (org-air-query--group file)
     :kind 'heading
     :donep (and todo (member todo org-done-keywords) t)
     :subtree-ts subtree-ts
     :activity (or (org-air-query--time-float closed)
                   (org-air-query--time-float scheduled)
                   (org-air-query--time-float deadline)
                   subtree-ts
                   (when-let* ((mtime
                                (or org-air-query--scan-mtime
                                    (and (not (string-empty-p file))
                                         (file-exists-p file)
                                         (file-attribute-modification-time
                                          (file-attributes file))))))
                     (float-time mtime)))
     :body-deadline body-deadline)))

;;;; ---------------------------------------------------------------------
;;;; R53 P1/P1b — the never-error work-buffer scan.
;;;; ---------------------------------------------------------------------

(defvar org-air-query--skip-log nil
  "Per-scan list of (FILE . REASON) entries the scan skipped (R53 P1b).
Cleared at the start of every full scan (`org-air-query-skip-log-reset');
listed by `org-air-scan-report'.  REASON is a symbol (`encrypted',
`too-large', `slow') or an error string.")

(defun org-air-query-skip-log-reset ()
  "Clear the per-scan skip log (R53 P1b).  Called once per scan start."
  (setq org-air-query--skip-log nil))

(defun org-air-query--skip (file reason)
  "Record FILE as skipped for REASON in the scan's skip log; return nil.
Never messages per file — the scan reports ONE summary line itself and
`org-air-scan-report' lists the details (no echo spam at 5000 files)."
  (push (cons file reason) org-air-query--skip-log)
  nil)

;;;###autoload
(defun org-air-scan-report ()
  "List the files the last org-air scan skipped, and why (R53 P1b)."
  (interactive)
  (if (null org-air-query--skip-log)
      (message "org-air: the last scan skipped no files")
    (let ((entries (reverse org-air-query--skip-log)))
      (with-current-buffer (get-buffer-create "*org-air scan report*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "org-air scan report — %d file(s) skipped\n\n"
                          (length entries)))
          (pcase-dolist (`(,file . ,reason) entries)
            (insert (format "  %-12s %s\n"
                            (if (symbolp reason) (symbol-name reason)
                              (format "%s" reason))
                            file)))
          (goto-char (point-min)))
        (special-mode)
        (pop-to-buffer (current-buffer))))))

(defvar org-air-query--work-buffer nil
  "The single reused scan work buffer, or nil (R53 P1).
A NORMAL-named buffer (org-ql drops space-prefixed ones), the mode
`org-mode' initialised ONCE per session under the
symbol `delay-mode-hooks' (mode hooks never run), element cache ON but
`org-element-cache-persistent' nil buffer-locally (org-persist stays out
of the shared buffer).  Killed by `org-air-query-teardown'.")

(defun org-air-query--work-buffer ()
  "Return the live scan work buffer, creating it on first use (R53 P1)."
  (unless (buffer-live-p org-air-query--work-buffer)
    (setq org-air-query--work-buffer (generate-new-buffer "*org-air scan*"))
    (with-current-buffer org-air-query--work-buffer
      (delay-mode-hooks (org-mode))
      (setq-local org-element-cache-persistent nil)))
  org-air-query--work-buffer)

(defun org-air-query-teardown ()
  "Kill the session's scan work buffer, if any (R53 P1)."
  (when (buffer-live-p org-air-query--work-buffer)
    (kill-buffer org-air-query--work-buffer))
  (setq org-air-query--work-buffer nil))

(defun org-air-query--inbox-file-p (file)
  "Return non-nil when FILE is `org-air-inbox-file' (P3 exclusion)."
  (and (boundp 'org-air-inbox-file)
       org-air-inbox-file
       (equal (or (ignore-errors (file-truename (expand-file-name file)))
                  (expand-file-name file))
              (or (ignore-errors
                    (file-truename (expand-file-name org-air-inbox-file)))
                  (expand-file-name org-air-inbox-file)))))

(defun org-air-query--file-item (file)
  "Return a one-item list for FILE as a headingless note, or nil (R53 P3).
Called with the scanned content in the current buffer AFTER the heading
scan yielded nothing.  A REAL note — no headings, some non-blank content,
no NUL byte in the first 1KB (binary junk never becomes a row), and not
the inbox file (its emptiness is chrome, not content) — synthesises ONE
openable item: `kind' `file', title from `#+title' (file name base
fallback), tags from `#+filetags', group = parent directory name, marker
\(FILE . 1) so RET opens the file at the top."
  (let ((case-fold-search t))
    (org-with-wide-buffer
     (goto-char (point-min))
     (unless (or (string-empty-p file)
                 (re-search-forward org-outline-regexp-bol nil t)
                 (save-excursion
                   (goto-char (point-min))
                   (search-forward "\0" (min (point-max) 1024) t))
                 (not (save-excursion
                        (goto-char (point-min))
                        (re-search-forward "[^ \t\r\n]" nil t)))
                 (org-air-query--inbox-file-p file))
       (goto-char (point-min))
       (let ((title (when (re-search-forward
                           "^#\\+title:[ \t]*\\(.+?\\)[ \t]*$" nil t)
                      (match-string-no-properties 1)))
             (tags (save-excursion
                     (goto-char (point-min))
                     (when (re-search-forward
                            "^#\\+filetags:[ \t]*\\(.+?\\)[ \t]*$" nil t)
                       (split-string (match-string-no-properties 1)
                                     "[: \t]+" t)))))
         (list
          (org-air-item-create
           :title (if (and title (not (string-empty-p title)))
                      title
                    (file-name-base file))
           :tags tags
           :file file
           :marker (cons file 1)
           :todo nil :priority nil
           :scheduled nil :deadline nil :closed nil
           :group (file-name-nondirectory
                   (directory-file-name (file-name-directory file)))
           :kind 'file
           :donep nil
           :activity (when-let* ((mtime
                                  (or org-air-query--scan-mtime
                                      (and (file-exists-p file)
                                           (file-attribute-modification-time
                                            (file-attributes file))))))
                       (float-time mtime))
           :body-deadline nil)))))))

(defun org-air-query--scan-live-buffer (buffer file query)
  "Scan the live user BUFFER visiting FILE with org-ql QUERY (R53 P1 rule 1).
Unsaved edits are respected; every item's marker/file slot is rewritten to
FILE so the (FILE . POS) contract and the mtime bookkeeping stay coherent
even when the buffer's own name differs (a symlinked visit)."
  (let* (;; R53fix M2: same echo hygiene as the work-buffer path — a live
         ;; headingless buffer must not re-spam org-ql's "No headings in
         ;; buffer" message on every refresh.
         (inhibit-message t)
         (message-log-max nil)
         (items (copy-sequence
                 (org-ql-select buffer (or query '(heading))
                   :action #'org-air-query--item-at-point))))
    (dolist (item items)
      (setf (org-air-item-file item) file)
      (let ((m (org-air-item-marker item)))
        (setf (org-air-item-marker item)
              (cons file (cond ((consp m) (or (cdr m) 1))
                               ((markerp m) (or (marker-position m) 1))
                               (t 1))))))
    (or items
        (with-current-buffer buffer
          (org-air-query--file-item file)))))

(defun org-air-query--scan-work-buffer (file query)
  "Scan FILE in the reused work buffer with org-ql QUERY (R53 P1 rule 2).
One `erase-buffer' + `insert-file-contents' per file into the session's
single `org-mode' work buffer; the
variable `buffer-file-name' is set for the file's extent (so Org's
file-relative logic behaves) and always cleared again;
`org-set-regexps-and-options' makes the file's own
`#+TODO:' win, with the R21-3 default vocabulary otherwise.  Known,
accepted difference: file-local variable BLOCKS are not processed here
\(`#+…' keywords ARE); a file whose parsing genuinely depends on local
variables scans like the same Org file without them."
  (with-current-buffer (org-air-query--work-buffer)
    (let ((buffer-undo-list t)
          (create-lockfiles nil)
          ;; Kills org-ql's per-file "No headings in buffer" echo spam ×N;
          ;; the scan reports ONE summary line itself (R53 P1b).
          (inhibit-message t)
          (message-log-max nil)
          (org-todo-keywords (org-air-query--scan-todo-keywords))
          (org-air-query--scan-mtime
           (file-attribute-modification-time (file-attributes file))))
      (unwind-protect
          (progn
            (erase-buffer)
            (insert-file-contents file)
            (setq buffer-file-name file)
            (org-set-regexps-and-options)
            (let ((items (copy-sequence
                          (org-ql-select (current-buffer)
                            (or query '(heading))
                            :action #'org-air-query--item-at-point))))
              (or items (org-air-query--file-item file))))
        (setq buffer-file-name nil)
        (set-buffer-modified-p nil)))))

(defun org-air-query--scan-file (file &optional query)
  "Return `org-air-item' records for FILE; NEVER signals (R53 P1/P1b).
The one per-file scan entry: a live user buffer visiting FILE is scanned
in place (rule 1, cheap `get-file-buffer' — never a `buffer-list' walk);
otherwise the file scans in the single reused work buffer (rule 2).  The
policy table applies BEFORE any read: an `.org.gpg' with no live buffer
is skipped `encrypted' (the background scan NEVER decrypts or prompts; a
live already-decrypted buffer scans normally), an over-
`org-air-max-file-size' file is skipped `too-large', an unreadable /
vanished / dangling-symlink file is skipped with its `file-error'.  ANY
signal inside the body degrades to 0 items + one skip-log entry — a bad
file can never abort the whole scan (the P1b never-error law).  QUERY is
the optional org-ql query (default: all headings).  A `quit' is NOT
swallowed: aborting always works."
  (condition-case err
      (let ((live (get-file-buffer file)))
        (cond
         (live (org-air-query--scan-live-buffer live file query))
         ((string-match-p "\\.gpg\\'" file)
          (org-air-query--skip file 'encrypted))
         ((not (file-readable-p file))
          (org-air-query--skip file 'unreadable))
         ((let ((size (file-attribute-size (file-attributes file))))
            (and org-air-max-file-size size
                 (> size org-air-max-file-size)))
          (org-air-query--skip file 'too-large))
         (t (org-air-query--scan-work-buffer file query))))
    (error (org-air-query--skip file (error-message-string err)))))

;;;###autoload
(defun org-air-query-items (&optional query)
  "Return `org-air-item' records matching org-ql QUERY.

When QUERY is nil, return all headings from `org-air-files' (plus one
bounded file-item per headingless note file, R53 P3).  R53 P1: the scan
loops `org-air-query--scan-file' over the configured files — org-ql stays
the only query engine, but it runs over buffers org-air manages, so no
source buffer is ever retained and one bad file can never abort the scan.
Item order is file order × buffer order, exactly as before."
  (let ((files (org-air-query-files)))
    (org-air-query-skip-log-reset)
    (let (items)
      (dolist (file files)
        (setq items (nconc items (org-air-query--scan-file file query))))
      items)))

(defun org-air-query-items-in-files (files &optional query)
  "Return `org-air-item' records for FILES, a subset of the configured set.

Like `org-air-query-items' but restricted to FILES (already-expanded Org
file paths), so the query can be split into batches and run on an idle
timer without blocking the frame (R19-1).  QUERY defaults to all
headings.  Does NOT reset the skip log — the caller (the refresh machine)
owns the per-scan log across its slices."
  (let (items)
    (dolist (file files)
      (setq items (nconc items (org-air-query--scan-file file query))))
    items))

(provide 'org-air-query)

;; Local Variables:
;; package-lint-main-file: "org-air.el"
;; End:
;;; org-air-query.el ends here
