;;; org-air-round26-test.el --- executing ERTs for v0.5 round-26 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.5 round-26 (air/v0.5/org-air-round26-design.org).
;; The round's harness discipline: the interaction items are DRIVEN under
;; realistic conditions — `noninteractive' bound nil so side/below windows
;; really exist, the side rail POPPED (the user's default flow), commands
;; dispatched through `(key-binding (kbd ...))' exactly as a keypress
;; would, and the reconciler driven explicitly (timers made deterministic
;; via `org-air-rail--reconcile-frame').
;;
;;   R26-3  PROJECT LEGEND + RET — the popped rail's Actions legend is
;;          ON-SCREEN (height-clamped side rail, inspector shrinks first),
;;          TABLE-DRIVEN true (every legend key resolves to a command), and
;;          RET opens the doc in the SAME window (the R26-5 model) — no
;;          silently-swallowed `display-buffer'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-project-test)            ; project fixture root + render

(when (locate-library "org-air")
  (require 'org-air))

(defun org-air-r26--kill-aux-buffers ()
  "Kill the shared pane/rail/view buffers so tests never inherit stale windows."
  (let ((kill-buffer-query-functions nil))
    (dolist (name (list org-air-view-pane-buffer-name org-air-rail-buffer-name
                        org-air-view-buffer-name "*org-air-project*"))
      (when (get-buffer name) (kill-buffer name)))
    (dolist (b (buffer-list))
      (when (and (buffer-live-p b)
                 (string-prefix-p " *org-air-pane:" (buffer-name b)))
        (with-current-buffer b (set-buffer-modified-p nil))
        (kill-buffer b)))))

(defmacro org-air-r26--with-live-project (&rest body)
  "Open the fixture project in a LIVE window (noninteractive nil); run BODY.
BODY runs in the `*org-air-project*' buffer with it selected in the single
MAIN window, so `display-buffer-in-side-window' really creates the rail
window.  Frozen mtime keeps dates deterministic without touching content."
  (declare (indent 0) (debug t))
  `(progn
     (should (fboundp 'org-air-project))
     (let ((org-air-sources (list (list :air org-air-project-test-root)))
           (org-air-project-group 'directory)
           (org-air-project-view-width 120)
           (org-air-rail-focus-on-popout nil))
       (org-air-project-test--with-frozen-mtime
        (save-window-excursion
          (org-air-r26--kill-aux-buffers)
          (let ((noninteractive nil))
            (org-air-project))
          (let ((buf (get-buffer "*org-air-project*")))
            (should buf)
            (unwind-protect
                (let ((noninteractive nil))
                  (with-current-buffer buf
                    (when (get-buffer-window buf)
                      (select-window (get-buffer-window buf))
                      (delete-other-windows (get-buffer-window buf)))
                    ,@body))
              (org-air-r26--kill-aux-buffers)
              (when (buffer-live-p buf)
                (let ((kill-buffer-query-functions nil))
                  (kill-buffer buf))))))))))

(defun org-air-r26--first-doc-pos ()
  "Return the first buffer position carrying `org-air-doc', or nil."
  (save-excursion
    (goto-char (point-min))
    (if (get-text-property (point) 'org-air-doc)
        (point)
      (next-single-property-change (point) 'org-air-doc))))

(defun org-air-r26--pop-rail ()
  "Pop the rail via the real toggle from the current (project) buffer."
  (org-air-rail-toggle)
  (should (window-live-p (org-air-rail--side-window))))

(defmacro org-air-r26--with-frame-lines (lines &rest body)
  "Resize the batch frame to LINES text lines around BODY; restore after.
Batch windows are real; sizing the frame controls the side window's body
height so the fold-line assertions are deterministic."
  (declare (indent 1) (debug t))
  `(progn
     (set-frame-size (selected-frame) 100 ,lines)
     (unwind-protect
         (progn ,@body)
       (set-frame-size (selected-frame) 80 25))))

;;;; =====================================================================
;;;; R26-3 — project shortcut legend + RET (driven with the rail POPPED).
;;;; =====================================================================

(ert-deftest org-air-r26-3-legend-on-screen-popped ()
  "The popped project rail shows the Actions header + the `RET open' row
WITHIN the side window's body height (not merely present in the buffer).
Trunk renders the rail at the host height with a >=1 inspector floor, so
Actions lands below the fold of a short side window (line 25 of 24)."
  (skip-unless (locate-library "org-air"))
  (org-air-r26--with-frame-lines 26      ; side window body-height = 24
    (org-air-r26--with-live-project
      (org-air-r26--pop-rail)
      (let ((side (org-air-rail--side-window)))
        (should (window-live-p side))
        (with-current-buffer org-air-rail-buffer-name
          (goto-char (point-min))
          (should (search-forward "Actions" nil t))
          (let ((actions-line (line-number-at-pos (match-beginning 0))))
            (should (search-forward "RET open" nil t))
            (let ((ret-line (line-number-at-pos (match-beginning 0)))
                  (start-line (line-number-at-pos (window-start side)))
                  (h (window-body-height side)))
              ;; header + first verb row inside the window's fold.
              (should (<= (1+ (- actions-line start-line)) h))
              (should (<= (1+ (- ret-line start-line)) h)))))))))

(ert-deftest org-air-r26-3-legend-truth-table-driven ()
  "Every key named in the Actions legend table resolves via `key-binding'
to a real command in the project buffer — the legend can never drift from
the map again (table-driven off `org-air-project--actions-table')."
  (skip-unless (locate-library "org-air"))
  (org-air-r26--with-live-project
    (dolist (row org-air-project--actions-table)
      (dolist (cell row)
        (let ((keys (if (string= (car cell) "s/d/t")
                        '("s" "d" "t")
                      (list (car cell)))))
          (dolist (k keys)
            (let ((cmd (key-binding (kbd k))))
              (should cmd)
              (should (commandp cmd)))))))
    ;; And the legend text itself is the table's (popped rail render).
    (org-air-r26--pop-rail)
    (with-current-buffer org-air-rail-buffer-name
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "Actions" text))
        (should (string-match-p "RET open" text))
        (should (string-match-p "( flip" text))
        (should (string-match-p "o sort" text))
        (should (string-match-p "s/d/t group" text))
        (should (string-match-p "g refresh" text))))))

(ert-deftest org-air-r26-3-ret-opens-doc-same-window-rail-popped ()
  "RET with the side rail POPPED (the user's default flow) opens the doc's
FILE buffer in the SAME window the tree held — the R26-5 session model, no
`display-buffer' to fight.  Dispatched through `(key-binding (kbd RET))'."
  (skip-unless (locate-library "org-air"))
  (org-air-r26--with-live-project
    (org-air-r26--pop-rail)
    (goto-char (org-air-r26--first-doc-pos))
    (org-air-view--goto-row-title)
    (let* ((tree-win (selected-window))
           (doc (get-text-property (point) 'org-air-doc))
           (cmd (key-binding (kbd "RET"))))
      (should doc)
      (should (eq cmd 'org-air-project-open))
      (call-interactively cmd)
      ;; The SAME window object now shows the doc's file buffer.
      (should (eq (selected-window) tree-win))
      (let ((shown (window-buffer tree-win)))
        (should (equal (buffer-file-name shown)
                       (file-truename (org-air-doc-file doc))))
        (when (buffer-live-p shown)
          (with-current-buffer shown (set-buffer-modified-p nil))
          (kill-buffer shown))))))

(ert-deftest org-air-r26-3-pane-refusal-is-never-silent ()
  "When `display-buffer' refuses the bottom view pane, the user gets a
message — never a silent no-op (the R26-3b root cause on the BOARD)."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
   (let ((org-air-view-width 120))
     (save-window-excursion
       (org-air-r26--kill-aux-buffers)
       (let ((noninteractive nil)
             (bbuf (get-buffer-create org-air-view-buffer-name)))
         (with-current-buffer bbuf
           (org-air-view-mode)
           (setq org-air-view--items (org-air-query-items))
           (setq-local org-air-view--rail-popped-out nil)
           (org-air-view--render org-air-view--items nil))
         (unwind-protect
             (progn
               (switch-to-buffer bbuf)
               (goto-char (or (next-single-property-change
                               (point-min) 'org-air-item)
                              (point-min)))
               (let ((logged nil))
                 (cl-letf* (((symbol-function 'display-buffer)
                             (lambda (&rest _) nil))
                            ((symbol-function 'message)
                             (lambda (fmt &rest args)
                               (when fmt
                                 (push (apply #'format fmt args) logged))
                               nil)))
                   (org-air-view-pane-return))
                 (should (seq-find
                          (lambda (m)
                            (string-match-p "could not display" m))
                          logged))))
           (org-air-r26--kill-aux-buffers)))))))

(ert-deftest org-air-r26-3-sdt-group-and-flip-keys-bound ()
  "`s'/`d'/`t' run the state/directory/tag grouping (airctl -a/-Da/-Ta
parity ON KEYS) and re-render; `(' runs the R26-4 flip command."
  (skip-unless (locate-library "org-air"))
  (org-air-r26--with-live-project
    (should (eq (key-binding (kbd "s")) 'org-air-project-group-by-state))
    (should (eq (key-binding (kbd "d")) 'org-air-project-group-by-directory))
    (should (eq (key-binding (kbd "t")) 'org-air-project-group-by-tag))
    (should (eq (key-binding (kbd "(")) 'org-air-project-toggle-filenames))
    (should (eq (key-binding (kbd "?")) 'org-air-help))
    ;; drive the grouping keys — the group flips and the buffer re-renders.
    (call-interactively (key-binding (kbd "s")))
    (should (eq org-air-project-group 'state))
    (should (string-match-p "Draft" (buffer-string)))
    (call-interactively (key-binding (kbd "t")))
    (should (eq org-air-project-group 'tag))
    (call-interactively (key-binding (kbd "d")))
    (should (eq org-air-project-group 'directory))))

(provide 'org-air-round26-test)
;;; org-air-round26-test.el ends here
