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
;;   R26-5  RAIL PLACEMENT + DOC SESSION — `org-air-rail-placement'
;;          (project defaults side-window), IDEMPOTENT re-entry (no
;;          kill-all-local-variables wipe, no double rail), and the
;;          TREE<->DOC session state machine (RET / back / q-in-rail /
;;          sequences never strand) on top of the R25-6 invariant.

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
                 (string-match-p "\\` ?\\*org-air-pane:" (buffer-name b)))
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

(defun org-air-r26--press (key)
  "Dispatch KEY via `key-binding' in the SELECTED window's buffer.
Emulates the command loop's discipline: the command runs with the
selected window's buffer current, exactly as a real keypress would."
  (with-current-buffer (window-buffer (selected-window))
    (call-interactively (key-binding (kbd key)))))

(defun org-air-r26--first-doc-pos ()
  "Return the first buffer position carrying `org-air-doc', or nil."
  (save-excursion
    (goto-char (point-min))
    (if (get-text-property (point) 'org-air-doc)
        (point)
      (next-single-property-change (point) 'org-air-doc))))

(defun org-air-r26--pop-rail ()
  "Ensure the rail is POPPED (the R26-5 placement default already pops it)."
  (unless (org-air-rail--popped-p)
    (org-air-rail-toggle))
  (unless (window-live-p (org-air-rail--side-window))
    (org-air-view--refresh-current))
  (should (window-live-p (org-air-rail--side-window))))

(defun org-air-r26--rail-windows ()
  "Return the live windows showing the rail buffer on this frame."
  (let ((rb (get-buffer org-air-rail-buffer-name)))
    (and rb (get-buffer-window-list rb 'no-mini (selected-frame)))))

(defun org-air-r26--inline-rail-text-p (buf)
  "Non-nil when BUF's text carries the INLINE rail (the Summary block)."
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (and (search-forward "| Summary" nil t) t))))

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
the map again (table-driven off `org-air-project--actions-table').
R50-1 conjunct: the resolved binding must be a COMMAND and NOT a keymap —
a future prefixization of any legend key (the board's `g refresh'
mislabel class) fails this gate instead of silently mislabelling."
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
              (should (commandp cmd))
              ;; R50-1: a legend key must never be a bare prefix map.
              (should-not (keymapp cmd)))))))
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

;;;; =====================================================================
;;;; R26-5 — rail placement + the TREE<->DOC interactive session.
;;;; =====================================================================

(ert-deftest org-air-r26-5-placement-default-pops-project-rail ()
  "A fresh `org-air-project' pops its side rail WITHOUT `|' (the
`org-air-rail-placement' project default — R26-5, preserved through the
R49-2 shared resolver).  R49-3 re-bless: the fresh BOARD half moved with
the confirmed consistent default — the board now pops its rail too
\(asserted by org-air-r49-2-consistent-default-seeds-both); this test
keeps the LEGACY alist shape pinned instead: a harness binding the old
R26-5 per-view alist still gets its exact asymmetric behaviour (board
inline, project popped) with zero migration."
  (skip-unless (locate-library "org-air"))
  (org-air-r26--with-live-project
    ;; No `|' pressed: the placement seed popped the rail.
    (should (org-air-rail--popped-p))
    (should (window-live-p (org-air-rail--side-window)))
    (should (eq (org-air-rail--side-owner) (current-buffer)))
    ;; And the tree text carries NO inline rail.
    (should-not (org-air-r26--inline-rail-text-p (current-buffer))))
  ;; LEGACY alist shape (the OLD R26-5 default, still honoured): a fresh
  ;; board seeded under it stays inline — no side window.
  (let ((org-air-rail-placement '((board . inline) (project . side-window))))
    (org-air-test-with-fixtures
     (save-window-excursion
       (org-air-r26--kill-aux-buffers)
       (let ((noninteractive nil))
         (org-air-view))
       (unwind-protect
           (progn
             (should-not (org-air-rail--popped-p
                          (get-buffer org-air-view-buffer-name)))
             (should-not (window-live-p (org-air-rail--side-window))))
         (org-air-r26--kill-aux-buffers))))))

(ert-deftest org-air-r26-5-reentry-keeps-session-no-double-rail ()
  "Re-running `org-air-project' on the live buffer keeps EVERY per-buffer
preference (popped rail, cycled sort key, R26-4 flip) and never blesses a
double rail — the re-entry `kill-all-local-variables' wipe is fixed."
  (skip-unless (locate-library "org-air"))
  (org-air-r26--with-live-project
    (org-air-r26--pop-rail)
    ;; Cycle the sort key + flip filenames (per-buffer preferences).
    (org-air-view-sort-cycle)
    (let ((key org-air-view--sort-key))
      (org-air-project-toggle-filenames)
      (should org-air-project--show-filenames)
      ;; RE-ENTER through the real command (the board's `P' hop).
      (let ((noninteractive nil))
        (org-air-project))
      (let ((pbuf (get-buffer "*org-air-project*")))
        (with-current-buffer pbuf
          ;; popped survived; NO inline rail text; side window still live.
          (should (eq org-air-view--rail-popped-out t))
          (should-not (org-air-r26--inline-rail-text-p pbuf))
          (should (window-live-p (org-air-rail--side-window)))
          ;; preferences survived the hop.
          (should (eq org-air-view--sort-key key))
          (should org-air-project--show-filenames)
          ;; reconcile changes NOTHING (no double rail to bless).
          (org-air-rail--reconcile-frame (selected-frame))
          (should (eq org-air-view--rail-popped-out t))
          (should-not (org-air-r26--inline-rail-text-p pbuf))
          (should (<= (length (org-air-r26--rail-windows)) 1))
          ;; exactly ONE of {inline text, side window} exists.
          (should (window-live-p (org-air-rail--side-window))))))))

(ert-deftest org-air-r26-5-ret-doc-back-round-trip ()
  "RET on a doc row: the SAME window shows the doc's file buffer and the
side window flips to the DOC context (outline + back legend); `C-c C-q'
restores the tree in the same window, point back on the row, and the side
window shows the project rail (Summary) again."
  (skip-unless (locate-library "org-air"))
  (org-air-r26--with-live-project
    (org-air-r26--pop-rail)
    (goto-char (org-air-r26--first-doc-pos))
    (org-air-view--goto-row-title)
    (let* ((tree (current-buffer))
           (row-pt (point))
           (win (selected-window))
           (doc (get-text-property (point) 'org-air-doc)))
      (call-interactively (key-binding (kbd "RET")))
      (let ((docbuf (window-buffer win)))
        (unwind-protect
            (progn
              ;; DOC state: same window, doc file buffer, session minor mode.
              (should (eq (selected-window) win))
              (should (equal (buffer-file-name docbuf)
                             (file-truename (org-air-doc-file doc))))
              (should (buffer-local-value 'org-air-doc-session-mode docbuf))
              ;; the side window shows the DOC context: outline + legend.
              (should (window-live-p (org-air-rail--side-window)))
              (with-current-buffer org-air-rail-buffer-name
                (let ((text (substring-no-properties (buffer-string))))
                  (should (string-match-p "Outline" text))
                  (should (string-match-p "Notes" text)) ; the doc's heading
                  (should (string-match-p "back" text))))
              ;; back: C-c C-q dispatched through the session keymap.
              (with-current-buffer docbuf
                (should (eq (key-binding (kbd "C-c C-q"))
                            'org-air-project-back))
                (call-interactively (key-binding (kbd "C-c C-q"))))
              ;; TREE state again: same window, point on the row, project
              ;; rail content restored (Summary block).
              (should (eq (window-buffer win) tree))
              (should (eq (window-point win) row-pt))
              (should (window-live-p (org-air-rail--side-window)))
              (with-current-buffer org-air-rail-buffer-name
                (should (string-match-p
                         "Summary"
                         (substring-no-properties (buffer-string))))))
          (when (buffer-live-p docbuf)
            (with-current-buffer docbuf (set-buffer-modified-p nil))
            (unless (eq docbuf tree) (kill-buffer docbuf))))))))

(ert-deftest org-air-r26-5-q-in-side-window-goes-back ()
  "`q' pressed IN the DOC-context side window returns to the tree (the
read-only rail is where plain `q' is legal), focus lands on the main
window, and the side window is RESTORED to the project rail (Summary
block) — the full RET doc -> q tree -> rail restored round trip under
the side-window placement default."
  (skip-unless (locate-library "org-air"))
  (org-air-r26--with-live-project
    (org-air-r26--pop-rail)
    (goto-char (org-air-r26--first-doc-pos))
    (org-air-view--goto-row-title)
    (let ((tree (current-buffer))
          (win (selected-window)))
      (call-interactively (key-binding (kbd "RET")))
      (let ((docbuf (window-buffer win))
            (side (org-air-rail--side-window)))
        (unwind-protect
            (progn
              (should (window-live-p side))
              (select-window side)
              (with-current-buffer (window-buffer side)
                (should (eq (key-binding (kbd "q")) 'org-air-rail-quit))
                (call-interactively (key-binding (kbd "q"))))
              ;; DOC -> TREE restore; focus back on the main window.
              (should (eq (window-buffer win) tree))
              (should (eq (selected-window) win))
              ;; and the side window flips back to the PROJECT rail
              ;; (Summary block), not the doc outline.
              (should (window-live-p (org-air-rail--side-window)))
              (with-current-buffer org-air-rail-buffer-name
                (let ((text (substring-no-properties (buffer-string))))
                  (should (string-match-p "Summary" text))
                  (should-not (string-match-p "Outline" text)))))
          (when (and (buffer-live-p docbuf) (not (eq docbuf tree)))
            (with-current-buffer docbuf (set-buffer-modified-p nil))
            (kill-buffer docbuf)))))))

(ert-deftest org-air-r26-5-sequences-never-strand ()
  "RET, back, RET, |, |, back, g — after EVERY step at most one rail
window exists and the main window is never stranded (the R25-6 invariant
holds through the whole session)."
  (skip-unless (locate-library "org-air"))
  (org-air-r26--with-live-project
    (org-air-r26--pop-rail)
    (goto-char (org-air-r26--first-doc-pos))
    (org-air-view--goto-row-title)
    (let* ((tree (current-buffer))
           (win (selected-window))
           (check (lambda (step)
                    (should (<= (length (org-air-r26--rail-windows)) 1))
                    (should (window-live-p win))
                    (should (buffer-live-p (window-buffer win)))
                    (ignore step)))
           docbuf)
      ;; RET (through the selected window's buffer — the command loop).
      (org-air-r26--press "RET")
      (setq docbuf (window-buffer win))
      (should (buffer-local-value 'org-air-project--session-tree docbuf))
      (funcall check 'ret-1)
      (unwind-protect
          (progn
            ;; back
            (with-current-buffer docbuf (org-air-project-back))
            (funcall check 'back-1)
            (should (eq (window-buffer win) tree))
            ;; RET again (the doc buffer is REUSED, session re-arms)
            (org-air-r26--press "RET")
            (should (eq (window-buffer win) docbuf))
            (should (buffer-local-value 'org-air-project--session-tree docbuf))
            (funcall check 'ret-2)
            ;; | (toggle from the DOC session buffer: rail pops IN)
            (with-current-buffer docbuf (org-air-rail-toggle))
            (funcall check 'pop-in)
            (should-not (window-live-p (org-air-rail--side-window)))
            ;; | again (rail pops back OUT: the DOC context returns)
            (with-current-buffer docbuf (org-air-rail-toggle))
            (funcall check 'pop-out)
            (should (window-live-p (org-air-rail--side-window)))
            ;; back
            (with-current-buffer docbuf (org-air-project-back))
            (funcall check 'back-2)
            (should (eq (window-buffer win) tree))
            ;; g (refresh the tree; the rail must not duplicate)
            (with-current-buffer tree
              (call-interactively (key-binding (kbd "g"))))
            (funcall check 'refresh)
            (should (<= (length (org-air-r26--rail-windows)) 1)))
        (when (and (buffer-live-p docbuf) (not (eq docbuf tree)))
          (with-current-buffer docbuf (set-buffer-modified-p nil))
          (kill-buffer docbuf))))))

(ert-deftest org-air-r26-5-unset-never-pops-in-batch ()
  "A fresh project buffer rendered under `noninteractive' keeps the
`unset'->nil normalisation: NO side window, and the reconciler creates
none — the `unset'-is-truthy double-rail root cause can never return."
  (skip-unless (locate-library "org-air"))
  (org-air-project-test--render
   ;; batch render: the placement seed is gated off; not popped.
   (should-not (org-air-rail--popped-p))
   (should-not (window-live-p (org-air-rail--side-window)))
   ;; even a reconcile pass (bound interactive) creates no side window.
   (let ((noninteractive nil))
     (org-air-rail--reconcile-frame (selected-frame)))
   (should-not (window-live-p (org-air-rail--side-window)))))

;;;; =====================================================================
;;;; R26-7 — "o" sort cycle: rows VISIBLY move through the real render.
;;;; =====================================================================

(defmacro org-air-r26--with-live-board (&rest body)
  "Render the fixture BOARD (seam width/height) and run BODY in it.
The render composes its panes through `org-air-view--render-lines' — the
temp-buffer seam R26-7 fixes — and BODY drives the real key bindings."
  (declare (indent 0) (debug t))
  `(org-air-test-with-fixtures
    (let ((org-air-view-width 120)
          (org-air-view-height 50))
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
              (with-current-buffer bbuf
                (switch-to-buffer bbuf)
                ,@body)
            (org-air-r26--kill-aux-buffers)))))))

(defun org-air-r26--bucket-titles (bucket)
  "Return the RENDERED row titles of section BUCKET, in buffer order."
  (let (titles (cur nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((bol (line-beginning-position))
               (eol (line-end-position))
               (secpos (text-property-not-all bol eol 'org-air-section nil))
               (itempos (text-property-not-all bol eol 'org-air-item nil)))
          (cond
           ((and secpos (not itempos))
            (setq cur (get-text-property secpos 'org-air-section)))
           ((and itempos (eq cur bucket))
            (let ((it (get-text-property itempos 'org-air-item)))
              (push (org-air-item-title it) titles)))))
        (forward-line 1)))
    (nreverse titles)))

(defun org-air-r26--first-line ()
  "Return the banner (first) line's text."
  (save-excursion
    (goto-char (point-min))
    (buffer-substring-no-properties (point) (line-end-position))))

(ert-deftest org-air-r26-7-board-o-reorders-rows ()
  "Driving `o' to `title' through the REAL render path visibly reorders
the rows within each bucket (alphabetical) — the buffer-local sort state
now crosses the `--render-lines' temp-buffer seam.  FAILS on trunk (the
key cycled, the rows never moved)."
  (skip-unless (locate-library "org-air"))
  (org-air-r26--with-live-board
    (let ((baseline (org-air-r26--bucket-titles 'high-priority)))
      (should (> (length baseline) 1))
      ;; o: date -> priority; o: priority -> title (the board's key ring).
      (call-interactively (key-binding (kbd "o")))
      (call-interactively (key-binding (kbd "o")))
      (should (eq org-air-view--sort-key 'title))
      (let ((titles (org-air-r26--bucket-titles 'high-priority)))
        ;; visibly alphabetical within the bucket...
        (should (equal titles
                       (sort (copy-sequence titles)
                             (lambda (a b)
                               (string-lessp (downcase a) (downcase b))))))
        ;; ...and NOT the date-order baseline.
        (should-not (equal titles baseline))))))

(ert-deftest org-air-r26-7-board-O-reverses-rows ()
  "`O' after `o o' (title) reverses the visible row order within a bucket
that shows ALL its members.  FAILS on trunk (direction never reached the
render pass)."
  (skip-unless (locate-library "org-air"))
  (org-air-r26--with-live-board
    (call-interactively (key-binding (kbd "o")))
    (call-interactively (key-binding (kbd "o")))   ; -> title
    (let ((asc (org-air-r26--bucket-titles 'high-priority)))
      (should (> (length asc) 1))
      (call-interactively (key-binding (kbd "O")))  ; -> descending
      (should (eq org-air-view--sort-direction 'descending))
      (should (equal (org-air-r26--bucket-titles 'high-priority)
                     (reverse asc))))))

(ert-deftest org-air-r26-7-board-indicator-surfaces ()
  "The banner's `↕ KEY' indicator appears for a NON-default sort (it was
already coded — the temp-buffer seam starved it) and stays absent at the
byte-identical default."
  (skip-unless (locate-library "org-air"))
  (org-air-r26--with-live-board
    ;; default: no indicator text on the banner.
    (should-not (string-match-p "\\bpriority\\b" (org-air-r26--first-line)))
    (call-interactively (key-binding (kbd "o")))   ; -> priority
    (should (string-match-p "\\bpriority\\b" (org-air-r26--first-line)))))

(ert-deftest org-air-r26-7-board-o-echoes ()
  "Each `o' press echoes `org-air: sort by KEY' (message capture)."
  (skip-unless (locate-library "org-air"))
  (org-air-r26--with-live-board
    (let (logged)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when fmt (push (apply #'format fmt args) logged))
                   nil)))
        (call-interactively (key-binding (kbd "o"))))
      (should (seq-find (lambda (m)
                          (string-match-p "org-air: sort by priority" m))
                        logged)))))

(defun org-air-r26--project-doc-names ()
  "Return the doc names of the project buffer's rows, in buffer order."
  (let (names (pos (point-min)))
    (when (get-text-property pos 'org-air-doc)
      (push (org-air-doc-name (get-text-property pos 'org-air-doc)) names))
    (while (setq pos (next-single-property-change pos 'org-air-doc))
      (when-let* ((d (get-text-property pos 'org-air-doc)))
        (let ((name (org-air-doc-name d)))
          (unless (equal name (car names))
            (push name names)))))
    (nreverse names)))

(ert-deftest org-air-r26-7-project-dir-grouping-follows-key ()
  "In the DEFAULT directory grouping, `o' to `updated' reorders the docs
WITHIN their state runs (state-display-rank stays primary); at the
default `name' the tree keeps the byte-stable name order.  FAILS on trunk
\(`--sort-own-docs' never consulted the key)."
  (skip-unless (locate-library "org-air"))
  (let ((root (make-temp-file "org-air-r26-proj" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "v0.1" root))
          (write-region "" nil (expand-file-name "air-config.toml" root))
          ;; three docs, SAME state (one state run), name order A < B < C.
          (dolist (spec '(("able.org" . "Able") ("baker.org" . "Baker")
                          ("charlie.org" . "Charlie")))
            (write-region (format "#+title: %s\n#+state: ready\n" (cdr spec))
                          nil
                          (expand-file-name (concat "v0.1/" (car spec)) root)))
          ;; distinct UPDATED stamps: Baker oldest, then Charlie, Able newest
          ;; (updated ascending B C A != name order A B C).
          (set-file-times (expand-file-name "v0.1/able.org" root)
                          (encode-time 0 0 12 3 5 2026))
          (set-file-times (expand-file-name "v0.1/baker.org" root)
                          (encode-time 0 0 12 1 5 2026))
          (set-file-times (expand-file-name "v0.1/charlie.org" root)
                          (encode-time 0 0 12 2 5 2026))
          (let ((org-air-sources (list (list :air root)))
                (org-air-project-group 'directory)
                (org-air-project-view-width 120)
                (org-air-rail-placement '((board . inline)
                                          (project . inline))))
            (save-window-excursion
              (org-air-r26--kill-aux-buffers)
              (let ((noninteractive nil))
                (org-air-project))
              (unwind-protect
                  (with-current-buffer "*org-air-project*"
                    ;; byte-default: `name' ascending = the old order.
                    (should (equal (org-air-r26--project-doc-names)
                                   '("Able" "Baker" "Charlie")))
                    ;; o -> created; o -> updated (the project key ring).
                    (let ((noninteractive nil))
                      (call-interactively (key-binding (kbd "o")))
                      (call-interactively (key-binding (kbd "o"))))
                    (should (eq org-air-view--sort-key 'updated))
                    ;; rows reorder by updated WITHIN the state run.
                    (should (equal (org-air-r26--project-doc-names)
                                   '("Baker" "Charlie" "Able")))
                    ;; the header indicator names the key.
                    (should (string-match-p
                             "updated"
                             (buffer-substring-no-properties
                              (point-min)
                              (save-excursion (goto-char (point-min))
                                              (line-end-position))))))
                (org-air-r26--kill-aux-buffers)))))
      (delete-directory root t))))

;;;; =====================================================================
;;;; R26-4 — "(" flips doc rows filename<->title (Dired-style).
;;;; =====================================================================

(ert-deftest org-air-r26-4-flip-shows-relpath-and-back ()
  "`(' flips doc rows to the RAW file name (header gains the `files'
chip); a second `(' restores a byte-identical title render.  R28-5: this
is the DIRECTORY grouping, so the flipped row shows the BASENAME — the
nested tree already conveys every dir segment — and the full relpath is
ABSENT from doc rows."
  (skip-unless (locate-library "org-air"))
  (org-air-r26--with-live-project
    (let ((baseline (substring-no-properties (buffer-string))))
      (goto-char (org-air-r26--first-doc-pos))
      (let ((doc (get-text-property (point) 'org-air-doc)))
        ;; flip: dir-grouped rows show BASENAMES, the header the files chip.
        (call-interactively (key-binding (kbd "(")))
        (should org-air-project--show-filenames)
        (let ((text (substring-no-properties (buffer-string))))
          (should (string-match-p (regexp-quote (file-name-nondirectory
                                                 (org-air-doc-relpath doc)))
                                  text))
          (should-not (string-match-p (regexp-quote (org-air-doc-relpath doc))
                                      text))
          (should-not (string-match-p (regexp-quote (org-air-doc-name doc))
                                      text))
          (should (string-match-p "files"
                                  (car (split-string text "\n")))))
        ;; flip back: byte-identical to the first render (chip gone).
        (call-interactively (key-binding (kbd "(")))
        (should-not org-air-project--show-filenames)
        (should (equal (substring-no-properties (buffer-string))
                       baseline))))))

(ert-deftest org-air-r26-4-flip-survives-refresh-and-grouping ()
  "The flip is buffer-local state READ BY THE RENDER PASS: it survives
`g' refresh and holds across the s/d/t grouping modes.  R28-5: the
DIRECTORY-grouped legs (`g' on the default grouping + the `d' leg)
assert the BASENAME (the nested tree conveys the dir segments); the flat
state/tag legs keep the FULL relpath (the path IS the information
there)."
  (skip-unless (locate-library "org-air"))
  (org-air-r26--with-live-project
    (goto-char (org-air-r26--first-doc-pos))
    (let* ((relpath (org-air-doc-relpath
                     (get-text-property (point) 'org-air-doc)))
           (basename (file-name-nondirectory relpath)))
      (call-interactively (key-binding (kbd "(")))
      ;; survives `g' (the default DIRECTORY grouping: basename, R28-5).
      (call-interactively (key-binding (kbd "g")))
      (should org-air-project--show-filenames)
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p (regexp-quote basename) text))
        (should-not (string-match-p (regexp-quote relpath) text)))
      ;; the flat state / tag groupings render the FULL relpath...
      (dolist (key '("s" "t"))
        (call-interactively (key-binding (kbd key)))
        (should (string-match-p (regexp-quote relpath)
                                (substring-no-properties
                                 (buffer-string)))))
      ;; ...and the directory tree the BASENAME (R28-5).
      (call-interactively (key-binding (kbd "d")))
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p (regexp-quote basename) text))
        (should-not (string-match-p (regexp-quote relpath) text))))))

;;;; =====================================================================
;;;; R26-1 — one breathing-room space between the tree arm and the badge.
;;;; =====================================================================

(defmacro org-air-r26--with-dir-tree (&rest body)
  "Render the fixture project in DIRECTORY grouping (batch); run BODY."
  (declare (indent 0) (debug t))
  `(let ((org-air-project-group 'directory)
         (org-air-project-view-width 100))
     (org-air-project-test--render ,@body)))

(defun org-air-r26--doc-row-bol (name)
  "Return BOL position of the doc row titled NAME, or nil."
  (save-excursion
    (goto-char (point-min))
    (catch 'hit
      (while (not (eobp))
        (let* ((bol (line-beginning-position))
               (eol (line-end-position))
               (p (text-property-not-all bol eol 'org-air-doc nil)))
          (when (and p (equal (org-air-doc-name
                               (get-text-property p 'org-air-doc))
                              name))
            (throw 'hit bol)))
        (forward-line 1))
      nil)))

(defun org-air-r26--state-cell-start (bol)
  "Return the position where the doc row at BOL's state token begins.
Locates the row's own token via its `org-air-doc' state (word cells after
R26-2, bracket cells before), so the R26-1 gutter assertions are
token-shape agnostic."
  (save-excursion
    (goto-char bol)
    (let* ((eol (line-end-position))
           (p (text-property-not-all bol eol 'org-air-doc nil))
           (doc (and p (get-text-property p 'org-air-doc)))
           (word (string-trim (org-air-project--state-token
                               (org-air-doc-state doc)))))
      (search-forward word eol)
      (- (point) (length word)))))

(ert-deftest org-air-r26-1-one-space-before-the-badge ()
  "A leaf doc row's cell IMMEDIATELY LEFT of the state cell is a single
SPACE, and every cell between the corner and that space is the
`box-horizontal' fill faced `org-air-face-air-tree' (`+---- READY').
FAILS on trunk (R25-1 drew the fill flush against the badge)."
  (skip-unless (locate-library "org-air"))
  (org-air-r26--with-dir-tree
    (let ((bol (org-air-r26--doc-row-bol "Alpha feature")))
      (should bol)
      (goto-char bol)
      (let* ((open (org-air-r26--state-cell-start bol)))
        ;; the cell immediately left of the state cell is ONE space...
        (should (eq (char-after (1- open)) ?\s))
        ;; ...the cell before it is the arm fill (not another space)...
        (should (eq (char-after (- open 2)) ?-))
        ;; ...and the whole run corner+1 .. space-1 is `-' faced air-tree.
        (let ((corner (save-excursion (goto-char bol)
                                      (search-forward "+" open)
                                      (1- (point)))))
          (cl-loop for pos from (1+ corner) to (- open 2) do
                   (should (eq (char-after pos) ?-))
                   (let ((face (get-text-property pos 'face)))
                     (should (memq 'org-air-face-air-tree
                                   (if (listp face) face (list face)))))))))))

(ert-deftest org-air-r26-1-v6-gutter-width-frozen ()
  "The gutter TOTAL width is still `item-margin + 2*(1+depth)': the state
cell's `[' column is unchanged by the one-space join, at depth 0 AND for
the nested depth-1 doc (ancestor rail intact, corner rule intact)."
  (skip-unless (locate-library "org-air"))
  (org-air-r26--with-dir-tree
    (let ((margin-w (string-width (org-air-view--item-margin))))
      ;; depth 0 (Alpha): gutter = margin + 2.
      (let ((bol (org-air-r26--doc-row-bol "Alpha feature")))
        (should (= (- (org-air-r26--state-cell-start bol) bol)
                   (+ margin-w (* 2 (1+ 0))))))
      ;; depth 1 (Gamma, under air-context/): gutter = margin + 4, the
      ;; ancestor rail column intact and the corner still present.
      (let ((bol (org-air-r26--doc-row-bol "Gamma context")))
        (goto-char bol)
        (should (= (- (org-air-r26--state-cell-start bol) bol)
                   (+ margin-w (* 2 (1+ 1)))))
        (let ((line (buffer-substring-no-properties bol (line-end-position))))
          ;; corner + shortened arm + ONE space before the badge.
          (should (string-match-p "\\+-+ DRAFT" line)))))))

(ert-deftest org-air-r26-1-degenerate-depth-clamps ()
  "A doc so deep that no column remains after the corner renders
corner-flush with NO space and NO overflow (gutter width still clamped)."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
   (with-temp-buffer
     (let* ((org-air-project-view-width 100)
            (org-air-view-width 100)
            (org-air-project--meta-date-w 12)
            (org-air-project--meta-tags-w 10)
            (org-air-project--meta-origin-w 0)
            (doc (car (org-air-project--collect-docs
                       org-air-project-test-root)))
            ;; rails deep enough to eat the whole gutter at this depth.
            (rails (make-string 40 ?|)))
       (should doc)
       (org-air-project--insert-doc-row doc 100 2 rails t)
       (goto-char (point-min))
       (let* ((bol (point))
              (open (org-air-r26--state-cell-start bol))
              (margin-w (string-width (org-air-view--item-margin))))
         ;; gutter clamped to margin + 2*(1+2) — never wider, no crash.
         (should (= (- open bol) (+ margin-w (* 2 (1+ 2)))))
         ;; truncated lead: no trailing space squeezed in.
         (should-not (eq (char-after (1- open)) ?\s)))))))

;;;; =====================================================================
;;;; R26-2 — uniform WORD state pills (DRAFT/READY/WIP/COMP/DROP).
;;;; =====================================================================

(defun org-air-r26--display-image (s)
  "Return the `display' IMAGE on string S, or nil."
  (let ((disp (get-text-property 0 'display s)))
    (and (imagep disp) disp)))

(defun org-air-r26--svg-data (s)
  "Return the raw SVG string from string S's display image, or nil."
  (let ((img (org-air-r26--display-image s)))
    (and img (image-property img :data))))

(defmacro org-air-r26--with-gui-metrics (&rest body)
  "Run BODY with a stubbed graphical frame + fixed pill char metrics."
  (declare (indent 0) (debug t))
  `(let ((org-air-view--pill-char-w 8)
         (org-air-view--pill-char-h 16))
     (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) t)))
       (should (org-air-view--svg-available-p))
       ,@body)))

(ert-deftest org-air-r26-2-word-tokens-uniform-width ()
  "Each canonical state's byte token is the PADDED word — exactly 5 cols
for ALL states (uniformity at the byte layer).  FAILS on trunk (3-col
`[R]' tokens)."
  (skip-unless (locate-library "org-air"))
  (dolist (pair '(("draft" . "DRAFT") ("ready" . "READY")
                  ("work-in-progress" . "WIP  ") ("complete" . "COMP ")
                  ("dropped" . "DROP ")))
    (ert-info ((format "state %s" (car pair)))
      (let ((token (org-air-project--state-token (car pair))))
        (should (equal (substring-no-properties token) (cdr pair)))
        (should (= (string-width token)
                   org-air-project--state-cell-w)))))
  ;; the full cell = padded token + one separator (batch).
  (should (equal (substring-no-properties (org-air-project--state-cell "ready"))
                 "READY ")))

(ert-deftest org-air-r26-2-uniform-pill-boxes-gui-seam ()
  "With the svg seam forced available, the five state badges' overlay
images have IDENTICAL widths (equal 5-col capsules) and each svg draws the
BARE word (not the padded token) bold."
  (skip-unless (locate-library "org-air"))
  (org-air-r26--with-gui-metrics
    (let (widths)
      (dolist (pair '(("draft" . "DRAFT") ("ready" . "READY")
                      ("work-in-progress" . "WIP") ("complete" . "COMP")
                      ("dropped" . "DROP")))
        (ert-info ((format "state %s" (car pair)))
          (let* ((badge (org-air-project--state-svg-badge (car pair)))
                 (img (org-air-r26--display-image badge))
                 (svg (org-air-r26--svg-data badge)))
            (should img)
            (should svg)
            (push (image-property img :width) widths)
            ;; the drawn label is the BARE word, bold.
            (should (string-match-p (concat ">" (cdr pair) "<") svg))
            (should (string-match-p "font-weight=\"bold\"" svg)))))
      ;; all five capsules the SAME width = 5 cols * char-px.
      (should (= (length (seq-uniq widths)) 1))
      (should (= (car widths) (* org-air-project--state-cell-w 8))))))

(ert-deftest org-air-r26-2-v6-relock-title-shift ()
  "A doc row's title column = gutter + the 5-col cell + 1 separator (old
column + 2); the date/tag cluster columns are identical ACROSS rows; the
R26-1 one-space arm contract still holds against the new cell."
  (skip-unless (locate-library "org-air"))
  (org-air-r26--with-dir-tree
    (let ((margin-w (string-width (org-air-view--item-margin))))
      ;; title left edge: gutter + cell-w + 1 — relocked, uniform +2 shift.
      (let* ((bol (org-air-r26--doc-row-bol "Alpha feature"))
             (line (buffer-substring-no-properties bol
                    (save-excursion (goto-char bol) (line-end-position)))))
        (should (= (string-match "Alpha feature" line)
                   (+ margin-w (* 2 (1+ 0))
                      org-air-project--state-cell-w 1)))
        ;; R26-1 contract against the new cell: dashes, ONE space, the word.
        (should (string-match-p "\\+-+ READY" line)))
      ;; date glyph column identical across every doc row (V6 cluster).
      (let (cols)
        (dolist (name '("Alpha feature" "Beta CLI" "Zeta work in progress"))
          (let* ((bol (org-air-r26--doc-row-bol name))
                 (line (buffer-substring-no-properties bol
                        (save-excursion (goto-char bol) (line-end-position)))))
            (push (string-match "~ 2026" line) cols)))
        (should (= (length (seq-uniq cols)) 1))))))

(ert-deftest org-air-r26-2-rollup-letters-unchanged ()
  "The per-dir rollup summary stays compact LETTERS (`R4(+1) C14(+14)',
R25-4 map) — words live in the doc-row cells only."
  (skip-unless (locate-library "org-air"))
  (let ((summary (substring-no-properties
                  (org-air-project--dir-count-summary
                   '(("ready" . 4) ("complete" . 14))
                   '(("ready" . 1) ("complete" . 14))))))
    (should (equal summary "R4(+1) C14(+14)"))
    (should-not (string-match-p "READY\\|COMP" summary))))

(ert-deftest org-air-r26-2-unknown-state-fallback-word ()
  "A non-canonical state's token is the upcased 5-col truncation of its
name (`unknown' -> `UNKNO', faded face), and its pill label matches."
  (skip-unless (locate-library "org-air"))
  (let ((token (org-air-project--state-token "unknown")))
    (should (equal (substring-no-properties token) "UNKNO"))
    (should (= (string-width token) org-air-project--state-cell-w)))
  ;; the rendered fixture row shows the same word...
  (org-air-r26--with-dir-tree
    (let* ((bol (org-air-r26--doc-row-bol "Eta notes"))
           (line (buffer-substring-no-properties bol
                  (save-excursion (goto-char bol) (line-end-position)))))
      (should (string-match-p "UNKNO Eta notes" line))))
  ;; ...and the GUI pill draws the SAME label in the same 5-col capsule.
  (org-air-r26--with-gui-metrics
    (let* ((badge (org-air-project--state-svg-badge "unknown"))
           (img (org-air-r26--display-image badge))
           (svg (org-air-r26--svg-data badge)))
      (should img)
      (should svg)
      (should (string-match-p ">UNKNO<" svg))
      (should (= (image-property img :width)
                 (* org-air-project--state-cell-w 8))))))

;;;; =====================================================================
;;;; R26-6 — the "· r to file" row hint is gone from rows.
;;;; =====================================================================

(ert-deftest org-air-r26-6-row-hint-gone ()
  "NO board row carries the `· r to file' nudge any more — the dated
inbox row's date cell is exactly the date label (+ optional repeat
marker).  FAILS on trunk (the R19-2 nudge is baked into the date cell)."
  (skip-unless (locate-library "org-air"))
  (org-air-r17--with-denote-board 120
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should-not (string-match-p "r to file" text))
      ;; the dated inbox row: date, then straight into the tag cluster.
      (save-excursion
        (goto-char (point-min))
        (should (search-forward "Inbox 1" nil t))
        (forward-line 1)
        (let ((row (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position))))
          (should (string-match-p "Tomorrow +#inbox" row))
          (should-not (string-match-p "·" row)))))))

(ert-deftest org-air-r26-6-v6-inbox-tag-column-restored ()
  "The (formerly hinted) dated inbox row's tag column equals every other
row's tag column — the per-row date-cell expansion no longer fires.
FAILS on trunk (the hint pushed that one row's tags 12 cols right)."
  (skip-unless (locate-library "org-air"))
  (org-air-r17--with-denote-board 120
    (let (cols)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (when (string-match "#inbox" line)
              (push (match-beginning 0) cols)))
          (forward-line 1)))
      ;; the item renders in BOTH Inbox and Upcoming (dual membership).
      (should (>= (length cols) 2))
      ;; ...and the tag cluster sits in ONE column for all of them.
      (should (= (length (seq-uniq cols)) 1)))))

(ert-deftest org-air-r26-6-refile-still-works ()
  "`r' still resolves to `org-air-refile-item' on the board, and the `?'
help — the R50-2 `*org-air-help*' BUFFER now, not an echo-area line —
documents the refile verb.  The R26-6 discovery guarantee survives,
relocated: the old one-line `message' is deleted, so the assertion reads
the rendered help buffer (key derived via `where-is' from the live board
map, so the row really is `r  refile…')."
  (skip-unless (locate-library "org-air"))
  (should (eq (lookup-key org-air-view-mode-map (kbd "r"))
              'org-air-refile-item))
  ;; the board help BUFFER documents it (R50-2).
  (save-window-excursion
    (unwind-protect
        (with-temp-buffer
          (org-air-view-mode)          ; a board-context origin buffer
          (org-air-help)
          (let ((help (get-buffer org-air-help-buffer-name)))
            (should help)
            (with-current-buffer help
              (let ((text (substring-no-properties (buffer-string))))
                ;; the refile row: derived key `r' + the refile verb.
                (should (string-match-p "^  r +refile" text))
                ;; …and the true refresh sequence rides along (R50-1).
                (should (string-match-p "^  g r +refresh" text))))))
      (when (get-buffer org-air-help-buffer-name)
        (kill-buffer org-air-help-buffer-name)))))

;;;; =====================================================================
;;;; R26-8 — cache-first async (deterministic: the slice runner is driven
;;;; directly; zero timers, zero waits in the gate).
;;;; =====================================================================

(defmacro org-air-r26--with-cache-env (&rest body)
  "Fixtures + frozen clock + a TEMP `org-air-cache-file'; run BODY."
  (declare (indent 0) (debug t))
  `(org-air-test-with-fixtures
    (org-air-viewport-test--with-frozen-now
      (let ((org-air-view-width 120)
            (org-air-view-height 50)
            (org-air-cache-file
             (expand-file-name "cache/board.eld" org-air-test--dir))
            (org-air-view-buffer-name "*org-air-r26-8*")
            ;; R42-2: these suites drive the CHUNKED/paced machine directly
            ;; (slices, tokens, timers).  A 0 sync budget forces every
            ;; change through the paced path the tests are written for; the
            ;; sync fast path has its own coverage.
            (org-air-view--refresh-sync-budget 0))
        (unwind-protect
            (progn ,@body)
          (when (get-buffer org-air-view-buffer-name)
            (kill-buffer org-air-view-buffer-name)))))))

(defun org-air-r26--cache-board ()
  "Create + return a fresh board-mode buffer (not yet rendered)."
  (let ((buf (get-buffer-create org-air-view-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-air-view-mode) (org-air-view-mode)))
    buf))

(defun org-air-r26--run-slices (&optional max)
  "Drive the current buffer's refresh slices synchronously until done.
MAX (default 100) bounds the loop."
  (let ((token org-air-view--refresh-token)
        (n (or max 100)))
    (while (and (> n 0) (eq org-air-view--refresh-state 'refreshing))
      (org-air-view--refresh-run-slice (current-buffer) token)
      (cl-decf n))))

(defun org-air-r26--kill-file-buffers (dir)
  "Kill every buffer visiting a file under DIR (a fresh-session start).
The cache-first scenario is a NEW Emacs: the previous session's buffers
are gone, so the disk (not a stale open buffer) is the ground truth for
both the mtime check and the org-ql rescan."
  (dolist (buf (buffer-list))
    (let ((fn (buffer-file-name buf)))
      (when (and fn (string-prefix-p (file-truename dir) (file-truename fn)))
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf)))))

(defun org-air-r26--scan-and-cache ()
  "In the current board buffer: full machine scan -> render + cache write.
Returns the rendered board text."
  (org-air-view--refresh-start)
  (org-air-r26--run-slices)
  (should-not org-air-view--refresh-state)
  (substring-no-properties (buffer-string)))

(ert-deftest org-air-r26-8-cache-round-trip ()
  "Scan the fixtures through the machine (writes the cache); a fresh
buffer painted from the cache read is BYTE-IDENTICAL to the live-scan
board, with every marker slot cons-hydrated (FILE . POS)."
  (skip-unless (locate-library "org-air"))
  (org-air-r26--with-cache-env
    (let (live)
      (with-current-buffer (org-air-r26--cache-board)
        (setq live (org-air-r26--scan-and-cache)))
      (kill-buffer org-air-view-buffer-name)
      (should (file-exists-p org-air-cache-file))
      (let ((cache (org-air-view--cache-load)))
        (should cache)
        (should (null (cdr cache)))          ; nothing touched -> FRESH
        (should (car cache))
        (dolist (it (car cache))
          (should (consp (org-air-item-marker it)))
          (should (stringp (car (org-air-item-marker it)))))
        (with-current-buffer (org-air-r26--cache-board)
          (setq org-air-view--items (car cache))
          (org-air-view--render org-air-view--items nil)
          (should (equal (substring-no-properties (buffer-string))
                         live)))))))

(ert-deftest org-air-r26-8-stale-paint-marker-then-swap ()
  "Cache present + one file touched: the first paint equals the CACHED
board (no new item) with the `stale ∙ refreshing' marker; driving the
slices to completion repaints ONCE with the new item and CLEARS the
marker."
  (skip-unless (locate-library "org-air"))
  (org-air-r26--with-cache-env
    (with-current-buffer (org-air-r26--cache-board)
      (org-air-r26--scan-and-cache))
    (kill-buffer org-air-view-buffer-name)
    ;; the next session starts fresh: no open buffers from the scan.
    (org-air-r26--kill-file-buffers org-air-test--dir)
    ;; touch the inbox fixture: new capture + a decisive mtime bump.
    (write-region "* TODO Cache stale probe\n" nil org-air-inbox-file 'append)
    (set-file-times org-air-inbox-file (time-add (current-time) 5))
    (let ((cache (org-air-view--cache-load)))
      (should cache)
      (should (member (file-truename org-air-inbox-file)
                      (mapcar #'file-truename (cdr cache))))
      (with-current-buffer (org-air-r26--cache-board)
        ;; the CACHED dispatch: items from cache, machine started, paint.
        (setq org-air-view--items (car cache)
              org-air-view--cache-stale-files (cdr cache))
        (org-air-view--refresh-start)
        (org-air-view--render org-air-view--items nil)
        (let ((text (substring-no-properties (buffer-string))))
          (should (string-match-p "stale ∙ refreshing" text))
          (should-not (string-match-p "Cache stale probe" text)))
        ;; slices to completion: single swap, marker cleared.
        (org-air-r26--run-slices)
        (should-not org-air-view--refresh-state)
        (let ((text (substring-no-properties (buffer-string))))
          (should (string-match-p "Cache stale probe" text))
          (should-not (string-match-p "stale ∙ refreshing" text)))))))

(ert-deftest org-air-r26-8-mtime-fast-path-no-scan ()
  "Cache present, nothing touched: FRESH — no stale files, so the dispatch
never enters REFRESHING (empty queue, no timer, no slice ever needed)."
  (skip-unless (locate-library "org-air"))
  (org-air-r26--with-cache-env
    (with-current-buffer (org-air-r26--cache-board)
      (org-air-r26--scan-and-cache))
    (kill-buffer org-air-view-buffer-name)
    (let ((cache (org-air-view--cache-load)))
      (should cache)
      (should (null (cdr cache)))
      (with-current-buffer (org-air-r26--cache-board)
        (setq org-air-view--items (car cache))
        (org-air-view--render org-air-view--items nil)
        ;; FRESH: the machine never started.
        (should-not org-air-view--refresh-state)
        (should (null org-air-view--refresh-queue))
        (should-not org-air-view--refresh-timer)
        (should-not (string-match-p "refreshing"
                                    (substring-no-properties
                                     (buffer-string))))))))

(ert-deftest org-air-r26-8-interleaving-single-swap ()
  "Between slices the board does NOT repaint (single-swap rule) and stays
usable (motion runs); the completed swap lands point back on an item row."
  (skip-unless (locate-library "org-air"))
  (org-air-r26--with-cache-env
    (with-current-buffer (org-air-r26--cache-board)
      (org-air-r26--scan-and-cache))
    (kill-buffer org-air-view-buffer-name)
    (org-air-r26--kill-file-buffers org-air-test--dir)
    (write-region "* TODO Interleave probe\n" nil org-air-inbox-file 'append)
    (set-file-times org-air-inbox-file (time-add (current-time) 5))
    (let ((cache (org-air-view--cache-load))
          ;; R53 P1c (re-bless): slices are TIME-budgeted — the obsoleted
          ;; `org-air-refresh-files-per-slice' no longer forces >1 slice (one
          ;; budgeted slice drains a small fast queue).  A ZERO budget is the
          ;; design's own seam: the slice loop consumes files only while
          ;; under budget with a minimum of ONE file, so budget 0 = exactly
          ;; one file per slice — the mid-refresh window this test needs.
          (org-air-refresh-slice-budget 0))      ; min-1-file slices
      (with-current-buffer (org-air-r26--cache-board)
        (setq org-air-view--items (car cache)
              org-air-view--cache-stale-files (cdr cache))
        (org-air-view--refresh-start)
        (should (> (length org-air-view--refresh-queue) 1))
        (org-air-view--render org-air-view--items nil)
        (let ((before (substring-no-properties (buffer-string)))
              (token org-air-view--refresh-token))
          ;; ONE slice: data accumulated privately, buffer text untouched.
          (org-air-view--refresh-run-slice (current-buffer) token)
          (should (eq org-air-view--refresh-state 'refreshing))
          (should (equal (substring-no-properties (buffer-string)) before))
          ;; the board is USABLE mid-refresh: motion works.
          (org-air-view--goto-first-item)
          (should (org-air-view--row-property 'org-air-item))
          (org-air-next-item)
          ;; completion: exactly one swap, point back on an item row.
          (org-air-r26--run-slices)
          (should-not org-air-view--refresh-state)
          (should (string-match-p "Interleave probe"
                                  (substring-no-properties (buffer-string))))
          (should (org-air-view--row-property 'org-air-item)))))))

(ert-deftest org-air-r26-8-failure-honest-and-g-retries ()
  "A slice that signals keeps the painted board byte-intact, flips the
header to `refresh failed (g r retries)' — the TRUE retry sequence,
R50-1: `g' alone is the B4 prefix map — and `g r' (`org-air-refresh')
restarts the machine and completes."
  (skip-unless (locate-library "org-air"))
  (org-air-r26--with-cache-env
    (with-current-buffer (org-air-r26--cache-board)
      (org-air-r26--scan-and-cache)
      ;; R42-2: refresh is mtime-incremental, so dirty one file to give the
      ;; machine work (a no-change refresh short-circuits with no slice).
      (write-region "* TODO Failure probe\n" nil org-air-inbox-file 'append)
      (set-file-times org-air-inbox-file (time-add (current-time) 5))
      ;; start a refresh whose first slice blows up.
      (org-air-view--refresh-start)
      (let ((body-before
             ;; everything below the banner line (the header gains the
             ;; failure marker; the BODY must stay byte-intact).
             (substring-no-properties
              (buffer-string) (save-excursion (goto-char (point-min))
                                              (line-end-position)))))
        (cl-letf (((symbol-function 'org-air-query-items-in-files)
                   (lambda (&rest _) (error "disk on fire"))))
          (org-air-view--refresh-run-slice (current-buffer)
                                           org-air-view--refresh-token))
        (should (eq org-air-view--refresh-state 'failed))
        (let ((text (substring-no-properties (buffer-string))))
          ;; R50-1: the marker names the TRUE sequence, never the prefix.
          (should (string-match-p "refresh failed (g r retries)" text))
          (should-not (string-match-p "(g retries)" text))
          (should (equal (substring-no-properties
                          (buffer-string)
                          (save-excursion (goto-char (point-min))
                                          (line-end-position)))
                         body-before)))
        ;; g r retries: the interactive branch restarts the machine…
        (let ((noninteractive nil))
          (org-air-refresh))
        (should (eq org-air-view--refresh-state 'refreshing))
        ;; (batch hygiene: drop the timer the interactive branch armed).
        (when (timerp org-air-view--refresh-timer)
          (cancel-timer org-air-view--refresh-timer)
          (setq org-air-view--refresh-timer nil))
        ;; …and completes: marker gone, items live.
        (org-air-r26--run-slices)
        (should-not org-air-view--refresh-state)
        (should org-air-view--items)
        (should-not (string-match-p "refresh failed"
                                    (substring-no-properties
                                     (buffer-string))))))))

(ert-deftest org-air-r26-8-corrupt-cache-cold-path ()
  "A corrupt/garbage cache file — and a version or key mismatch — all read
as \"no cache\" silently (the cold path), never an error."
  (skip-unless (locate-library "org-air"))
  (org-air-r26--with-cache-env
    (make-directory (file-name-directory org-air-cache-file) t)
    ;; (a) unreadable garbage.
    (write-region "(((☃ not a plist" nil org-air-cache-file)
    (should-not (org-air-view--cache-load))
    ;; (b) readable but version-bumped.
    (write-region (prin1-to-string
                   (list :version -99
                         :key (list org-air-files org-air-inbox-file)
                         :mtimes nil :items nil))
                  nil org-air-cache-file)
    (should-not (org-air-view--cache-load))
    ;; (c) readable but the config key moved.
    (write-region (prin1-to-string
                   (list :version org-air-view--cache-version
                         :key '(("/elsewhere") "/elsewhere/inbox.org")
                         :mtimes nil :items nil))
                  nil org-air-cache-file)
    (should-not (org-air-view--cache-load))))

(ert-deftest org-air-r26-8-batch-purity-never-reads-cache ()
  "Under `noninteractive' the render NEVER consults the cache file, even
when one exists — the gate's byte path is the exact synchronous scan."
  (skip-unless (locate-library "org-air"))
  (should noninteractive)
  (org-air-r26--with-cache-env
    ;; a real cache exists…
    (with-current-buffer (org-air-r26--cache-board)
      (org-air-r26--scan-and-cache))
    (kill-buffer org-air-view-buffer-name)
    (should (file-exists-p org-air-cache-file))
    ;; …yet the batch entry point never touches it.
    (let ((reads 0))
      (cl-letf* ((real (symbol-function 'org-air-view--cache-load))
                 ((symbol-function 'org-air-view--cache-load)
                  (lambda () (cl-incf reads) (funcall real))))
        (save-window-excursion
          (org-air-view))
        (should (= reads 0))
        (with-current-buffer org-air-view-buffer-name
          (should org-air-view--items)
          ;; R53 P1 (re-bless, spec §P1 rule 3): the sync scan too now
          ;; yields the durable (FILE . POS) cons — the work-buffer scan
          ;; retains no source buffer, so "live marker" no longer proves
          ;; "scanned live".  The zero-reads counter above IS the purity
          ;; fence; here we pin the scan's new marker shape.
          (let ((m (org-air-item-marker (car org-air-view--items))))
            (should (consp m))
            (should (stringp (car m)))
            (should (integerp (cdr m)))))))))

(ert-deftest org-air-r26-8-token-cancels-stale-slice ()
  "`g' mid-refresh bumps the token: a late slice callback carrying the OLD
token is a silent no-op (the restarted queue is untouched)."
  (skip-unless (locate-library "org-air"))
  (org-air-r26--with-cache-env
    (with-current-buffer (org-air-r26--cache-board)
      ;; R53 P1c (re-bless): time-budgeted slices — budget 0 pins each
      ;; slice to its one-file minimum (the obsoleted files-per-slice
      ;; count no longer forces >1 slice), keeping a slice in flight
      ;; under the OLD token when the restart bumps it.
      (let ((org-air-refresh-slice-budget 0))
        (org-air-view--refresh-start)
        (let ((old-token org-air-view--refresh-token))
          ;; one slice under the old token…
          (org-air-view--refresh-run-slice (current-buffer) old-token)
          (should (eq org-air-view--refresh-state 'refreshing))
          ;; …then a restart (what g does): token bumps, queue resets.
          (org-air-view--refresh-start)
          (should (/= org-air-view--refresh-token old-token))
          (let ((queue-len (length org-air-view--refresh-queue))
                (acc org-air-view--refresh-acc))
            ;; the LATE stale callback: a no-op on queue AND accumulator.
            (org-air-view--refresh-run-slice (current-buffer) old-token)
            (should (= (length org-air-view--refresh-queue) queue-len))
            (should (eq org-air-view--refresh-acc acc)))
          ;; the new token drives to completion normally.
          (org-air-r26--run-slices)
          (should-not org-air-view--refresh-state)
          (should org-air-view--items))))))

(provide 'org-air-round26-test)
;;; org-air-round26-test.el ends here
