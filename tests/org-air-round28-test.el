;;; org-air-round28-test.el --- executing ERTs for v0.5 round-28 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.5 round-28 (air/v0.5/org-air-round28-design.org).
;; R26/R27 harness discipline: live-window items run with `noninteractive'
;; bound nil (side/below windows really exist), keys dispatched through
;; `(key-binding (kbd ...))' exactly as a keypress would, timers driven
;; deterministically (the slot's function called directly).
;;
;;   R28-1  DIMMER-PROOF NAMING — every org-air-OWNED shown buffer carries
;;          the `*org-air' prefix (the pane indirect loses its hiding
;;          space), plus the soft-dep dimmer registration (zero config,
;;          dormant without dimmer).
;;   R28-2  PROGRESSIVE q — one surface per press: a live pane closes
;;          FIRST on both the board and the project tree; the project
;;          quit tears down the popped rail (no orphaned side window).
;;   R28-3  LIVE-BINDING LEGEND — the DOC-context rail legend derives its
;;          back cell from `where-is-internal' in the live session buffer
;;          (C-c C-q), never the hardcoded `q back' lie.
;;   R28-4  HEADING HIGHLIGHT — overlay-only current-heading follow in the
;;          doc-session rail outline (debounced, zero repaints, batch inert).
;;   R28-5  BASENAME FLIP — dir-grouped flipped rows show the basename;
;;          state/tag groupings keep the relpath; filter key untouched.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)        ; frozen-mtime macro
(require 'org-air-project-test)            ; project fixture root

(when (locate-library "org-air")
  (require 'org-air))

;;;; =====================================================================
;;;; Shared live-window harness (R26/R27 discipline).
;;;; =====================================================================

(defun org-air-r28--kill-aux-buffers ()
  "Kill the shared pane/rail/view buffers so tests never inherit stale windows."
  (let ((kill-buffer-query-functions nil))
    (dolist (name (list org-air-view-pane-buffer-name org-air-rail-buffer-name
                        org-air-view-buffer-name "*org-air-project*"))
      (when (get-buffer name) (kill-buffer name)))
    (dolist (b (buffer-list))
      ;; R28-1: the pane indirect is `*org-air-pane:…' now (no hiding
      ;; space); match both spellings so the sweep can never rot.
      (when (and (buffer-live-p b)
                 (string-match-p "\\` ?\\*org-air-pane:" (buffer-name b)))
        (with-current-buffer b (set-buffer-modified-p nil))
        (kill-buffer b)))))

(defmacro org-air-r28--with-frame-size (cols lines &rest body)
  "Resize the batch frame to COLSxLINES around BODY; restore 80x25 after."
  (declare (indent 2) (debug t))
  `(progn
     (set-frame-size (selected-frame) ,cols ,lines)
     (unwind-protect
         (progn ,@body)
       (set-frame-size (selected-frame) 80 25))))

(defmacro org-air-r28--with-live-board (&rest body)
  "Render the fixture board in a LIVE window; run BODY in it, selected.
`noninteractive' is bound nil so the bottom pane window really splits;
the frame is sized 120x40 so the `org-air-view-pane-height' split fits."
  (declare (indent 0) (debug t))
  `(org-air-test-with-fixtures
    (save-window-excursion
      (org-air-r28--kill-aux-buffers)
      (org-air-r28--with-frame-size 120 40
        (let ((noninteractive nil)
              (org-air-rail-focus-on-popout nil)
              (bbuf (get-buffer-create org-air-view-buffer-name)))
          (unwind-protect
              (progn
                (with-current-buffer bbuf
                  (org-air-view-mode)
                  (setq org-air-view--items (org-air-query-items))
                  (setq-local org-air-view--rail-popped-out nil))
                (switch-to-buffer bbuf)
                (delete-other-windows)
                (with-current-buffer bbuf
                  (org-air-view--render org-air-view--items nil)
                  ,@body))
            (org-air-r28--kill-aux-buffers)))))))

(defmacro org-air-r28--with-live-project (&rest body)
  "Open the fixture project in a LIVE window; run BODY in it, selected.
The R26-5 side-window placement default really pops the rail; the frame
is sized 130x40 so the pane split below the tree fits."
  (declare (indent 0) (debug t))
  `(progn
     (should (fboundp 'org-air-project))
     (let ((org-air-sources (list (list :air org-air-project-test-root)))
           (org-air-project-group 'directory)
           (org-air-project-view-width 120)
           (org-air-rail-focus-on-popout nil))
       (org-air-project-test--with-frozen-mtime
        (save-window-excursion
          (org-air-r28--kill-aux-buffers)
          (org-air-r28--with-frame-size 130 40
            (let ((noninteractive nil))
              (org-air-project)
              (let ((buf (get-buffer "*org-air-project*")))
                (should buf)
                (unwind-protect
                    (with-current-buffer buf
                      (when (get-buffer-window buf)
                        (select-window (get-buffer-window buf))
                        (delete-other-windows (get-buffer-window buf)))
                      ,@body)
                  (org-air-r28--kill-aux-buffers)
                  (when (buffer-live-p buf)
                    (let ((kill-buffer-query-functions nil))
                      (kill-buffer buf))))))))))))

(defun org-air-r28--press (key)
  "Dispatch KEY via `key-binding' in the SELECTED window's buffer.
Emulates the command loop: the command runs with the selected window's
buffer current, exactly as a real keypress would."
  (with-current-buffer (window-buffer (selected-window))
    (call-interactively (key-binding (kbd key)))))

(defun org-air-r28--first-prop-pos (prop)
  "Return the first buffer position carrying PROP, or nil."
  (save-excursion
    (goto-char (point-min))
    (if (get-text-property (point) prop)
        (point)
      (next-single-property-change (point) prop))))

(defun org-air-r28--pop-rail ()
  "Ensure the rail is POPPED in the current live buffer."
  (unless (org-air-rail--popped-p)
    (org-air-rail-toggle))
  (unless (window-live-p (org-air-rail--side-window))
    (org-air-view--refresh-current))
  (should (window-live-p (org-air-rail--side-window))))

;;;; =====================================================================
;;;; R28-2 — progressive q: one surface per press (pane first).
;;;; =====================================================================

(ert-deftest org-air-r28-2-board-q-progressive ()
  "Board + pane open, ONE `q': the pane window is DEAD, the board window
LIVE and selected, mode still `org-air-view-mode'; a second `q' buries
the board.  Trunk FAILED: one press tore down the pane AND the board."
  (skip-unless (locate-library "org-air"))
  (org-air-r28--with-live-board
    (goto-char (org-air-r28--first-prop-pos 'org-air-item))
    (org-air-r28--press "RET")             ; open the pane, focus stays here
    (should (org-air-view-pane--window-live-p))
    (let ((bwin (get-buffer-window (current-buffer))))
      (org-air-r28--press "q")
      (should-not (org-air-view-pane--window-live-p))
      (should (window-live-p bwin))
      (should (eq (selected-window) bwin))
      (with-current-buffer (window-buffer (selected-window))
        (should (derived-mode-p 'org-air-view-mode)))
      (org-air-r28--press "q")
      (should-not (get-buffer-window (current-buffer))))))

(ert-deftest org-air-r28-2-project-q-progressive-no-orphans ()
  "Project tree + `v' pane + popped rail: `q' #1 closes ONLY the pane
\(tree alive + selected, rail still popped); `q' #2 buries the tree AND
tears down the rail side window, leaving NO org-air pane window behind
\(window inventory asserted).  Trunk FAILED on both presses: the bare
`quit-window' buried the tree and ORPHANED the pane + rail on screen."
  (skip-unless (locate-library "org-air"))
  (org-air-r28--with-live-project
    (org-air-r28--pop-rail)
    (goto-char (org-air-r28--first-prop-pos 'org-air-doc))
    (org-air-r28--press "v")
    (should (org-air-view-pane--window-live-p))
    (let ((tree (current-buffer)))
      ;; q #1: pane closed; tree alive; rail still popped.
      (org-air-r28--press "q")
      (should-not (org-air-view-pane--window-live-p))
      (should (get-buffer-window tree))
      (should (eq (window-buffer (selected-window)) tree))
      (should (org-air-rail--popped-p))
      (should (window-live-p (org-air-rail--side-window)))
      ;; q #2: tree buried, rail side window gone, no pane window remains.
      (org-air-r28--press "q")
      (should-not (get-buffer-window tree))
      (should-not (window-live-p (org-air-rail--side-window)))
      (dolist (w (window-list (selected-frame) 'no-mini))
        (should-not (window-parameter w 'org-air-pane))
        (should-not (string-match-p "\\` ?\\*org-air-pane:"
                                    (buffer-name (window-buffer w)))))
      ;; the popped flag survives buffer-locally (a re-entry re-pops, R26-5).
      (should (eq (buffer-local-value 'org-air-view--rail-popped-out tree)
                  t)))))

(ert-deftest org-air-r28-2-day-view-peel-order ()
  "Single-day view with the pane open: `q' #1 closes the pane (STILL day
view), `q' #2 returns to the full board, `q' #3 quits — the full peel
order pinned."
  (skip-unless (locate-library "org-air"))
  (org-air-r28--with-live-board
    (org-air-view-day org-air-test-now)
    (should org-air-view--day)
    (let* ((item (car org-air-view--items))
           (ctx (list :marker (org-air-item-marker item)
                      :file (org-air-item-file item)
                      :title (org-air-item-title item)
                      :state (org-air-item-todo item))))
      (org-air-view-pane--show ctx))
    (should (org-air-view-pane--window-live-p))
    ;; q #1: the pane closes; the day view survives.
    (org-air-r28--press "q")
    (should-not (org-air-view-pane--window-live-p))
    (should org-air-view--day)
    (should (get-buffer-window (current-buffer)))
    ;; q #2: back to the full board.
    (org-air-r28--press "q")
    (should-not org-air-view--day)
    (should (get-buffer-window (current-buffer)))
    ;; q #3: quits (board buried).
    (org-air-r28--press "q")
    (should-not (get-buffer-window (current-buffer)))))

(ert-deftest org-air-r28-2-pane-side-q-unchanged ()
  "Focus INSIDE the read-only snapshot pane: `q' is still
`org-air-view-pane-quit' — the pane closes and the board is re-selected
\(the R20-3a contract is the regression net for the helper refactor)."
  (skip-unless (locate-library "org-air"))
  (org-air-r28--with-live-board
    (let ((org-air-view-pane-editable nil)) ; the READ-ONLY snapshot pane
      (goto-char (org-air-r28--first-prop-pos 'org-air-item))
      (org-air-r28--press "RET")
      (should (org-air-view-pane--window-live-p))
      (let ((bwin (get-buffer-window (current-buffer)))
            (pwin (org-air-view-pane--find-window)))
        (select-window pwin)
        (with-current-buffer (window-buffer pwin)
          (should (eq (key-binding (kbd "q")) 'org-air-view-pane-quit)))
        (org-air-r28--press "q")
        (should-not (org-air-view-pane--window-live-p))
        (should (eq (selected-window) bwin))))))

(provide 'org-air-round28-test)
;;; org-air-round28-test.el ends here
