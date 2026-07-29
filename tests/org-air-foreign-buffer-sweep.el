;;; org-air-foreign-buffer-sweep.el --- the R97 B-1 instrument -*- lexical-binding: t; -*-

;;; Commentary:
;; INSTRUMENT B-1 — the foreign-buffer sweep.
;;
;; Every one of the 1345 ERTs before this file ran INSIDE an org-air
;; surface.  The suite's whole idiom is `…--with-board': set up a board,
;; act, assert.  That idiom cannot express "and now do it somewhere
;; else", and the R96 reviewer, probing from outside it for ninety
;; minutes, found that 28 of 132 commands (21%) misbehave when driven
;; from the buffer the user happens to be standing in — including EIGHT
;; that erase your file's buffer, paint the org-air board over it and
;; leave it modified, so the next `C-x C-s' writes the board to disk.
;;
;;   "Every real defect in this project has come from a place the tests
;;    do not model — the viewport, the compiler, and now the buffer you
;;    happen to be standing in."          — REPORT-r96.md, §Summary
;;
;; This file is the machinery; `tests/org-air-round97-test.el' holds the
;; assertions.  It is deliberately a separate module: the sweep is a
;; reusable INSTRUMENT (a probe can `require' it and print the raw
;; classification table against any tree, which is how the RED-on-pre-R97
;; evidence was produced), not a test-local helper.
;;
;; METHOD (identical to the probe that produced R97's before/after table,
;; so the numbers here and there are comparable):
;;
;;   * enumerate every `commandp' symbol in the `org-air' namespace that
;;     is not itself a mode — 132 today, discovered at runtime so a
;;     command added next round is swept without editing this file;
;;   * for each, a FRESH file buffer on disk holding known text
;;     ("* Chapter 1\nYears of writing.\n"), in `org-mode' and again in
;;     `fundamental-mode';
;;   * `noninteractive' bound to nil — the INTERACTIVE path, the one the
;;     user actually runs and the one 100% of the batch gate never takes;
;;   * every reader (`read-string', `completing-read', `read-char-exclusive',
;;     `y-or-n-p', `read-file-name', `org-read-date', …) stubbed to COUNT
;;     and then SIGNAL, so "did it ask a question before discovering it
;;     could not act?" is a measurement, not a guess;
;;   * `call-interactively' — not funcall: the `interactive' form runs,
;;     which is where R97 hoisted the preconditions of every spec-bearing
;;     command precisely so the refusal PRECEDES the prompt;
;;   * the FOREIGN buffer re-measured afterwards by buffer object (never
;;     "whatever is current now" — a destroyer may have switched away),
;;     plus its bytes ON DISK and the directory listing.
;;
;; CLASSIFICATION, in precedence order:
;;
;;   destroys           text, modified-flag, on-disk bytes or the
;;                      directory changed
;;   prompts-then-fails a reader was reached (the question preceded the
;;                      discovery that the command cannot act)
;;   refuses            a clean `user-error' (or `quit'), no reader
;;   raw error          any other signal — `wrong-type-argument',
;;                      `args-out-of-range', a bare `error'
;;   silent no-op       returned normally, asked nothing, did nothing
;;
;; MEASURED (not assumed): this exact file run against `main' (R96,
;; `qqqvotwp') with only the 11 root `org-air*.el' swapped back, and
;; against R97.  Each mode in its OWN process, one fresh form state per
;; command:
;;
;;   | class              | pre-R97 org | pre-R97 fund | R97 (both) |
;;   |--------------------+-------------+--------------+------------|
;;   | destroys           |           8 |            8 |          0 |
;;   | raw error          |           4 |            7 |          0 |
;;   | prompts-then-fails |          18 |           18 |          2 |
;;   | refuses            |          24 |           24 |        117 |
;;   | silent no-op       |          78 |           75 |         13 |
;;   | total              |         132 |          132 |        132 |
;;
;; destroys = 8 reproduces the reviewer's set EXACTLY, byte for byte:
;; 30 bytes of thesis in, 476 bytes of org-air board out, modified-flag
;; t.  The other two columns reconcile with R97's design table
;; (3 raw / 19 prompts) like this, and the difference is the instrument
;; being STRICTER, not the tree being different:
;;
;;   * `org-air-refile-form-category' is a raw `wrong-type-argument' here
;;     but a prompt there, because this sweep binds a FRESH transient
;;     form per command.  Given leftover form state it gets as far as its
;;     `completing-read'; from a genuinely cold start it does not.  Four
;;     raw-error forms is exactly the set REPORT-r96.md §D5 names.
;;   * `fundamental-mode' is strictly more damning than `org-mode': three
;;     more commands (`org-air-item-file-group', `org-air-refile-item',
;;     `org-air-refile-transient') reach a `wrong-type-argument' in a
;;     buffer that derives from nothing, which is why the sweep runs both
;;     and not just Org.
;;
;; The 15 commands that are still allowed to prompt or act are the
;; declared allow-list in `org-air-round97-test.el' — every one carries a
;; reason, so a command added later must be CLASSIFIED deliberately
;; rather than inherit an exemption by accident (the R92 landing-table
;; discipline).

;;; Code:

(require 'cl-lib)
(require 'seq)

;; Declared special so `org-air-sweep-run''s `let' is a DYNAMIC binding
;; even when this module is loaded before org-air itself.  Without these
;; the bindings below would be lexical and the sweep would silently run
;; against the user's real configuration and real board cache — the
;; instrument would be measuring the wrong Emacs.
(defvar org-air-files)
(defvar org-air-inbox-file)
(defvar org-air-cache-file)
(defvar org-air-report-skipped-files)
(defvar org-air-view-buffer-name)
(defvar org-air-view--edit-ring)
(defvar org-air-view--edit-redo-ring)
(defvar org-air-inbox--refile-form)
(defvar org-air-inbox--refile-last)

(define-error 'org-air-sweep-prompt
  "org-air foreign-buffer sweep: a reader was reached")

(defconst org-air-sweep-foreign-text "* Chapter 1\nYears of writing.\n"
  "The text the foreign buffer holds — the reviewer's `thesis.org'.
30 bytes in; 469-472 bytes of org-air board out, pre-R97.")

(defconst org-air-sweep-readers
  '(read-string read-from-minibuffer completing-read completing-read-multiple
    read-char read-char-exclusive read-event read-key read-number
    read-key-sequence read-key-sequence-vector read-multiple-choice
    y-or-n-p yes-or-no-p read-file-name read-directory-name read-buffer
    read-regexp org-read-date)
  "Every reader the sweep stubs to count-and-signal.
A command that reaches one of these has asked the user a question before
discovering it cannot act (R96 §D4) — the sweep measures that rather
than inferring it.

NOT exhaustive by construction, and it cannot be: a STRING `interactive'
spec (`\"sTag: \"') is read by `callint.c' calling the C `Fread_string'
directly, bypassing the Lisp function cell, so no `fset' can intercept
it.  In batch that read reaches stdin, finds EOF and signals
`end-of-file' — which is therefore ALSO the signature of a prompt, and
`org-air-sweep--call' counts it as one.  (R97 rewrote org-air's two
string specs to explicit `read-string' precisely so a guard could be
placed ahead of the read at all.)")

(defvar org-air-sweep--prompts 0
  "Readers reached during the current `org-air-sweep--call'.")

(defun org-air-sweep-commands ()
  "Return every non-mode `commandp' symbol in the `org-air' namespace, sorted.
Discovered from the loaded image, so a command added in a later round is
swept without an edit here.  Mode commands are excluded: they are how a
surface is ENTERED, and driving `org-air-view-mode' in a foreign buffer
is a user asking for exactly that."
  (let (cmds)
    (mapatoms
     (lambda (s)
       (when (and (string-prefix-p "org-air" (symbol-name s))
                  (commandp s)
                  (not (get s 'derived-mode-parent))
                  (not (memq s minor-mode-list))
                  (not (string-suffix-p "-mode" (symbol-name s))))
         (push s cmds))))
    (sort cmds (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defun org-air-sweep--with-readers-signalling (fn)
  "Call FN with every `org-air-sweep-readers' entry stubbed to signal.
The stub counts first (`org-air-sweep--prompts'), so a command that
catches the signal and carries on is still recorded as having prompted."
  (let ((saved (mapcar (lambda (s)
                         (cons s (and (fboundp s) (symbol-function s))))
                       org-air-sweep-readers)))
    (unwind-protect
        (progn
          (dolist (s org-air-sweep-readers)
            (when (fboundp s)
              (let ((sym s))
                (fset sym (lambda (&rest _)
                            (cl-incf org-air-sweep--prompts)
                            (signal 'org-air-sweep-prompt (list sym)))))))
          (funcall fn))
      (pcase-dolist (`(,s . ,f) saved)
        (when f (fset s f))))))

(defun org-air-sweep--cancel-timers ()
  "Cancel every pending timer whose callback is an org-air function.
`noninteractive' nil unlocks 6 `run-with-idle-timer' sites and the
adaptive wall-clock pacer; nothing may survive one command into the
next."
  (dolist (tm (append timer-list timer-idle-list))
    (let ((fn (timer--function tm)))
      (when (and (symbolp fn) (string-prefix-p "org-air" (symbol-name fn)))
        (ignore-errors (cancel-timer tm))))))

(defun org-air-sweep--kill-new-buffers (known keep)
  "Kill every buffer not in KNOWN, except KEEP."
  (let ((kill-buffer-query-functions nil))
    (dolist (b (buffer-list))
      (unless (or (eq b keep) (memq b known))
        (ignore-errors
          (with-current-buffer b (set-buffer-modified-p nil))
          (kill-buffer b))))))

(defun org-air-sweep--dir-listing (dir)
  "Return a sorted (NAME . SIZE) listing of DIR, recursively."
  (sort (mapcar (lambda (f)
                  (cons (file-relative-name f dir)
                        (file-attribute-size (file-attributes f))))
                (directory-files-recursively dir "" nil))
        (lambda (a b) (string< (car a) (car b)))))

(defun org-air-sweep--call (cmd buf file dir)
  "Drive CMD via `call-interactively' from foreign buffer BUF on FILE in DIR.
Return a plist describing what happened.  BUF is measured by OBJECT
afterwards, never by \"whatever is current\": a destroyer switches away."
  (let* ((known (buffer-list))
         (before-text (with-current-buffer buf (buffer-string)))
         (before-disk (with-temp-buffer
                        (insert-file-contents file) (buffer-string)))
         (before-dir (org-air-sweep--dir-listing dir))
         (before-point (with-current-buffer buf (point)))
         (org-air-sweep--prompts 0)
         ;; a fresh transient refile form per command: no command may
         ;; inherit form state another one left behind.
         (org-air-inbox--refile-form nil)
         (org-air-inbox--refile-last nil)
         ;; `org-air--repeat-next' installs a `repeat-mode' transient map
         ;; via `set-transient-map'.  In a live Emacs that map is cleared
         ;; by the next command; in batch there IS no command loop, so it
         ;; would survive the sweep and shadow `n' for every later test.
         ;; Rebinding here discards whatever the command installed — a
         ;; harness artefact of driving commands without a command loop,
         ;; not something the command does wrong.
         (overriding-terminal-local-map overriding-terminal-local-map)
         (overriding-local-map overriding-local-map)
         (pre-command-hook pre-command-hook)
         (post-command-hook post-command-hook)
         (signal-got nil))
    (unwind-protect
        (save-window-excursion
          (with-current-buffer buf
            (goto-char (point-min))
            (set-buffer-modified-p nil)
            (org-air-sweep--with-readers-signalling
             (lambda ()
               (let ((noninteractive nil)
                     (inhibit-message t)
                     (message-log-max nil))
                 (condition-case err
                     (call-interactively cmd)
                   (quit (setq signal-got '(quit)))
                   (error (setq signal-got err))))))))
      (org-air-sweep--cancel-timers)
      (org-air-sweep--kill-new-buffers known buf))
    (let* ((after-text (and (buffer-live-p buf)
                            (with-current-buffer buf (buffer-string))))
           (modified (and (buffer-live-p buf)
                          (with-current-buffer buf (buffer-modified-p))))
           (after-disk (with-temp-buffer
                         (insert-file-contents file) (buffer-string)))
           (after-dir (org-air-sweep--dir-listing dir))
           (text-changed (not (equal before-text after-text)))
           (disk-changed (not (equal before-disk after-disk)))
           (dir-changed (not (equal before-dir after-dir)))
           (sig (car-safe signal-got))
           ;; `end-of-file' is a C-level `interactive' string spec
           ;; reaching the minibuffer and finding stdin at EOF: a prompt
           ;; no Lisp stub can intercept, counted as one.
           (prompted (or (> org-air-sweep--prompts 0) (eq sig 'end-of-file)))
           (class
            (cond ((or text-changed modified disk-changed dir-changed
                       (not (buffer-live-p buf)))
                   'destroys)
                  (prompted 'prompts-then-fails)
                  ((memq sig '(user-error quit)) 'refuses)
                  (signal-got 'raw-error)
                  (t 'silent-no-op))))
      (list :command cmd
            :class class
            :signal sig
            :signal-data signal-got
            :prompts (if (and (eq sig 'end-of-file)
                              (= 0 org-air-sweep--prompts))
                         1
                       org-air-sweep--prompts)
            :text-changed text-changed
            :modified (and modified t)
            :disk-changed disk-changed
            :dir-changed dir-changed
            :buffer-killed (not (buffer-live-p buf))
            :point-moved (and (buffer-live-p buf)
                              (with-current-buffer buf
                                (/= (point) before-point)))
            :before-bytes (string-bytes before-text)
            :after-bytes (and after-text (string-bytes after-text))))))

(defun org-air-sweep-run (mode &optional commands)
  "Sweep COMMANDS (default all) from a fresh foreign buffer in MODE.
MODE is `org-mode' or `fundamental-mode'.  Returns the list of plists
`org-air-sweep--call' produced, in command order.

Nothing is configured: `org-air-files' is nil and `org-air-inbox-file' is
nil, which is the state a stranger's Emacs is in — and the state under
which R97's capture refusal (D2) must fire before the title prompt."
  (let* ((cmds (or commands (org-air-sweep-commands)))
         (dir (make-temp-file "org-air-sweep-" t))
         ;; The cache lives OUTSIDE the observed directory: the sweep's
         ;; "no file written" leg watches DIR recursively, and org-air's
         ;; own persisted cache is a legitimate write that must not be
         ;; mistaken for one into the user's tree.
         (cachedir (make-temp-file "org-air-sweep-cache-" t))
         (name (if (eq mode 'org-mode) "thesis.org" "notes.txt"))
         (file (expand-file-name name dir))
         (results nil))
    (unwind-protect
        (let ((org-air-files nil)
              (org-air-inbox-file nil)
              (org-air-cache-file (expand-file-name "board.eld" cachedir))
              (org-air-view-buffer-name "*org-air-sweep-board*")
              (org-air-view--edit-ring nil)
              (org-air-view--edit-redo-ring nil)
              (org-air-report-skipped-files nil)
              (create-lockfiles nil)
              (make-backup-files nil)
              (auto-save-default nil)
              (enable-local-variables nil)
              (kill-buffer-query-functions nil))
          (dolist (cmd cmds)
            (let ((file-name-handler-alist nil)
                  (coding-system-for-write 'utf-8-unix))
              (write-region org-air-sweep-foreign-text nil file nil 'silent))
            (let ((buf (find-file-noselect file)))
              (unwind-protect
                  (progn
                    (with-current-buffer buf
                      (unless (eq major-mode mode) (funcall mode))
                      (set-buffer-modified-p nil))
                    (push (org-air-sweep--call cmd buf file dir) results))
                (when (buffer-live-p buf)
                  (with-current-buffer buf (set-buffer-modified-p nil))
                  (kill-buffer buf))))))
      (org-air-sweep--cancel-timers)
      (when (file-directory-p dir) (delete-directory dir t))
      (when (file-directory-p cachedir) (delete-directory cachedir t)))
    (nreverse results)))

(defun org-air-sweep-counts (results)
  "Return an alist of CLASS . COUNT over RESULTS, in precedence order."
  (mapcar (lambda (class)
            (cons class
                  (seq-count (lambda (r) (eq (plist-get r :class) class))
                             results)))
          '(destroys raw-error prompts-then-fails refuses silent-no-op)))

(defun org-air-sweep-describe (r)
  "Return a one-line human description of sweep result R."
  (format "%-40s %-18s prompts=%d sig=%S bytes=%s->%s%s%s%s"
          (plist-get r :command)
          (plist-get r :class)
          (plist-get r :prompts)
          (plist-get r :signal)
          (plist-get r :before-bytes)
          (plist-get r :after-bytes)
          (if (plist-get r :modified) " MODIFIED" "")
          (if (plist-get r :disk-changed) " DISK-CHANGED" "")
          (if (plist-get r :dir-changed) " DIR-CHANGED" "")))

(defun org-air-sweep-report (&optional mode)
  "Print the full classification table for MODE (default `org-mode').
The probe entry point: `emacs --batch -l … -f org-air-sweep-report'
against any tree prints the same table R97's design doc records."
  (let* ((mode (or mode 'org-mode))
         (results (org-air-sweep-run mode)))
    (princ (format "\n=== org-air foreign-buffer sweep — %s ===\n" mode))
    (dolist (r results) (princ (concat (org-air-sweep-describe r) "\n")))
    (princ "\n--- counts ---\n")
    (pcase-dolist (`(,class . ,n) (org-air-sweep-counts results))
      (princ (format "%-20s %3d\n" class n)))
    (princ (format "%-20s %3d\n" 'total (length results)))
    results))

(provide 'org-air-foreign-buffer-sweep)
;;; org-air-foreign-buffer-sweep.el ends here
