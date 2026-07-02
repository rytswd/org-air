;;; org-air-round24-test.el --- substantive ERTs for v0.5 round-24 -*- lexical-binding: t; -*-

;;; Commentary:
;; Test-seat SUBSTANTIVE ERTs for v0.5 round-24
;; (air/v0.5/org-air-round24-design.org).  These cover the six R24 fixes:
;;
;;   R24-1  REFILE "UNDER HEADING" PROMPT [UX] — the free `read-string' becomes
;;          a `completing-read' over the TARGET FILE's real headings, with a
;;          leading `(file end)' default and NO prompt when the file has none.
;;   R24-2  TREE RAILS TO LEAF DOCS [POLISH] — R23-3's faded `│ ├─ └─' guides
;;          thread DOWN to the doc rows so each doc hangs under its dir,
;;          matching `airctl status -Da'; the V6 columns do not move.
;;   R24-3  STATE BADGES → SVG / NERD [RE-REVERSAL] — `org-air-project-state-
;;          style' defaults to a FIXED-WIDTH `svg' chip (re-reversing R23-4's
;;          emoji), with `nerd'/`text'/`emoji' options and the byte/TTY token.
;;   R24-4  PROJECT RET + FILTER, DRIVEN [BUG] — RET on a NON-doc (dir-header)
;;          row errored `user-error "No org-air item at point"'; the shared
;;          resolver now falls forward to the nearest doc.  EXECUTING ERTs.
;;   R24-5  PROJECT RAIL SIDE-WINDOW PARITY [BUG] — the project installs the
;;          cooperative reconciler + `popin' dispatches per mode, so a popped
;;          project rail falls back to inline like the board's.
;;   R24-6  FREE-TEXT (CONTENT) FILTER [FEATURE] — `#token' = TAG, a BARE token
;;          = case-insensitive SUBSTRING over title+path+tag-names; shared by
;;          board + project on one matcher.
;;
;; The interaction items (R24-4/R24-5) are DRIVEN, not inspected: the windowed
;; assertions `let'-bind `noninteractive' to nil so the otherwise batch-gated
;; `display-buffer' path runs and a REAL window is asserted.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)
(require 'org-air-project-test)            ; project fixture root + render

(when (locate-library "org-air")
  (require 'org-air))

;;;; =====================================================================
;;;; Shared harness — drive commands in a LIVE project buffer/window.
;;;; =====================================================================

(defun org-air-r24--kill-aux-buffers ()
  "Kill the shared pane/rail buffers so a test never inherits stale windows."
  (let ((kill-buffer-query-functions nil))
    (dolist (name (list org-air-view-pane-buffer-name org-air-rail-buffer-name))
      (when (get-buffer name) (kill-buffer name)))
    (dolist (b (buffer-list))
      (when (and (buffer-live-p b)
                 (string-prefix-p " *org-air-pane:" (buffer-name b)))
        (with-current-buffer b (set-buffer-modified-p nil))
        (kill-buffer b)))))

(defmacro org-air-r24--with-live-project (&rest body)
  "Open the fixture project in a LIVE window (noninteractive nil); run BODY.
BODY runs in the `*org-air-project*' buffer with it selected in a window, so
`display-buffer' (the pane / rail side window) actually creates windows.
Unlike the golden harness this does NOT freeze the project PATH: the pane
must READ the real doc file (a `directory-abbrev-alist' rewrite would make
`find-file-noselect' visit a non-existent abbreviated path = empty buffer).
Frozen mtime keeps dates deterministic without touching file content."
  (declare (indent 0) (debug t))
  `(progn
     (should (fboundp 'org-air-project))
     (let ((org-air-sources (list (list :air org-air-project-test-root)))
           (org-air-project-group 'directory)
           (org-air-project-view-width 120)
           ;; R26-5 pin: the project now DEFAULTS to a popped side rail
           ;; (`org-air-rail-placement'); this harness drives the rail
           ;; explicitly from a known INLINE start, so pin it.
           (org-air-rail-placement '((board . inline) (project . inline))))
       (org-air-project-test--with-frozen-mtime
        (save-window-excursion
          (org-air-r24--kill-aux-buffers)
          (let ((noninteractive nil))
            (org-air-project))
          (let ((buf (get-buffer "*org-air-project*")))
            (should buf)
            (unwind-protect
                (let ((noninteractive nil))
                  (with-current-buffer buf
                    (when (get-buffer-window buf)
                      (select-window (get-buffer-window buf)))
                    ,@body))
              (org-air-r24--kill-aux-buffers)
              (when (buffer-live-p buf)
                (let ((kill-buffer-query-functions nil)) (kill-buffer buf))))))))))

(defun org-air-r24--first-doc-pos ()
  "Return the first buffer position carrying `org-air-doc', or nil."
  (save-excursion
    (goto-char (point-min))
    (if (get-text-property (point) 'org-air-doc)
        (point)
      (next-single-property-change (point) 'org-air-doc))))

(defun org-air-r24--dir-header-pos ()
  "Return a buffer position on a row carrying `org-air-section' but NO
`org-air-doc' anywhere on the line (a dir-header row), or nil."
  (save-excursion
    (goto-char (point-min))
    (catch 'hit
      (while (not (eobp))
        (when (and (get-text-property (line-beginning-position) 'org-air-section)
                   (not (org-air-view--row-property 'org-air-doc)))
          (throw 'hit (line-beginning-position)))
        (forward-line 1))
      nil)))

(defun org-air-r24--pane-window-text ()
  "Return the buffer text shown in the live pane window, or nil."
  (let ((win (org-air-view-pane--find-window)))
    (and (window-live-p win)
         (with-current-buffer (window-buffer win)
           (substring-no-properties (buffer-string))))))

;;;; =====================================================================
;;;; R24-4 — project RET + filter, DRIVEN (executing ERTs).
;;;; =====================================================================

(ert-deftest org-air-r24-4-ret-on-doc-row-opens-pane ()
  "RET driven on a DOC row opens the bottom pane showing that doc's FILE.
Executing: `call-interactively' through the keymap, `noninteractive' nil so
`display-buffer' runs and a REAL window is asserted."
  (skip-unless (locate-library "org-air"))
  (org-air-r24--with-live-project
   (let ((pos (org-air-r24--first-doc-pos)))
     (should pos)
     (goto-char pos)
     (should (org-air-view--row-property 'org-air-doc))
     (call-interactively 'org-air-view-pane-return)
     (should (org-air-view-pane--window-live-p))
     (let ((text (org-air-r24--pane-window-text)))
       (should text)
       (should (string-match-p "Alpha feature" text))))))

(ert-deftest org-air-r24-4-ret-at-column-0-still-opens ()
  "RET driven from COLUMN 0 of a doc row still opens the SAME doc (the line
scan resolves the dead leading column)."
  (skip-unless (locate-library "org-air"))
  (org-air-r24--with-live-project
   (let ((pos (org-air-r24--first-doc-pos)))
     (should pos)
     (goto-char pos)
     (beginning-of-line)
     (should (= (point) (line-beginning-position)))
     (call-interactively 'org-air-view-pane-return)
     (should (org-air-view-pane--window-live-p))
     (should (string-match-p "Alpha feature" (org-air-r24--pane-window-text))))))

(ert-deftest org-air-r24-4-ret-on-dir-header-opens-first-child-doc ()
  "RET driven on a DIR-HEADER row (carries `org-air-section', NOT `org-air-
doc') opens the pane for the NEAREST FOLLOWING doc instead of erroring.
Anti-tautology: the header row itself has no doc, yet a doc file is shown."
  (skip-unless (locate-library "org-air"))
  (org-air-r24--with-live-project
   (let ((pos (org-air-r24--dir-header-pos)))
     (should pos)
     (goto-char pos)
     ;; the header row carries no doc/item/marker anywhere on the line.
     (should (get-text-property (line-beginning-position) 'org-air-section))
     (should-not (org-air-view--row-property 'org-air-doc))
     (should-not (org-air-view--row-property 'org-air-item))
     ;; the shared resolver falls forward to the nearest doc...
     (let ((ctx (org-air-view-pane--context-at-point)))
       (should ctx)
       (should (plist-get ctx :title)))
     ;; ...so RET OPENS the pane (no `user-error').
     (call-interactively 'org-air-view-pane-return)
     (should (org-air-view-pane--window-live-p))
     (let ((text (org-air-r24--pane-window-text)))
       (should text)
       (should (> (length text) 0))))))

(ert-deftest org-air-r24-4-row-thing-near-point-falls-forward ()
  "`org-air-view-pane--row-thing-near-point' returns the on-row doc when one
is present, else the NEAREST following doc (the resolver under the fix)."
  (skip-unless (locate-library "org-air"))
  (org-air-r24--with-live-project
   ;; on a doc row -> that doc.
   (goto-char (org-air-r24--first-doc-pos))
   (let ((near (org-air-view-pane--row-thing-near-point)))
     (should (eq (car near) 'org-air-doc))
     (should (org-air-doc-p (cdr near))))
   ;; on a dir-header row -> the following doc (never nil here).
   (goto-char (org-air-r24--dir-header-pos))
   (let ((near (org-air-view-pane--row-thing-near-point)))
     (should (eq (car near) 'org-air-doc))
     (should (org-air-doc-p (cdr near))))))

(ert-deftest org-air-r24-4-click-shares-ret-resolver-on-dir-header ()
  "CLICK half of `RET/click' (R26-3 re-bless): `<mouse-1>' and `RET' still
resolve to the SAME command in the project — now `org-air-project-open',
the same-window doc open (click == RET holds; the pre-R26 'shared
pane-return resolver' claim is superseded).  Driven executing through the
mouse-1 binding (NOT RET): on a DOC row the SELECTED window swaps to the
doc's FILE buffer; on a dir-header row the command refuses honestly with
a `user-error' (no doc on the line), never a silent no-op."
  (skip-unless (locate-library "org-air"))
  (org-air-r24--with-live-project
   ;; click == RET: one command, both keys.
   (should (eq (key-binding (kbd "<mouse-1>")) 'org-air-project-open))
   (should (eq (key-binding (kbd "RET")) (key-binding (kbd "<mouse-1>"))))
   ;; a dir-header row carries no doc -> honest refusal.
   (goto-char (org-air-r24--dir-header-pos))
   (should-not (org-air-view--row-property 'org-air-doc))
   (should-error (call-interactively (key-binding (kbd "<mouse-1>")))
                 :type 'user-error)
   ;; drive the CLICK command on a DOC row -> the SAME window object now
   ;; shows the doc's file buffer (the R26-5 session contract).
   (goto-char (org-air-r24--first-doc-pos))
   (let ((win (selected-window))
         (doc (get-text-property (point) 'org-air-doc)))
     (should doc)
     (call-interactively (key-binding (kbd "<mouse-1>")))
     (should (eq (selected-window) win))
     (should (equal (buffer-file-name (window-buffer win))
                    (org-air-doc-file doc))))))

(ert-deftest org-air-r24-4-slash-filter-narrows-driven ()
  "`/' driven with a real tag query narrows the visible doc set, and `\\'
restores it.  Executing through the filter command + the shared core."
  (skip-unless (locate-library "org-air"))
  (org-air-r24--with-live-project
   (let ((all org-air-project--doc-count))
     (should (and all (> all 0)))
     ;; drive the filter command with the read stubbed to a real tag.
     (cl-letf (((symbol-function 'org-air-view--read-filter)
                (lambda (&rest _) '("ui"))))
       (call-interactively 'org-air-project-filter))
     (should (equal org-air-view--tag-filter '("ui")))
     (should (< org-air-project--doc-count all))
     ;; `\\' clears -> full set restored.
     (call-interactively 'org-air-filter-clear)
     (should (null org-air-view--tag-filter))
     (should (= org-air-project--doc-count all)))))

;;;; =====================================================================
;;;; R24-5 — project rail side-window lifecycle parity (executing ERTs).
;;;; =====================================================================

(defun org-air-r24--rail-window ()
  "Return the live `*org-air-rail*' side window on the selected frame, or nil."
  (get-buffer-window org-air-rail-buffer-name (selected-frame)))

(ert-deftest org-air-r24-5-bar-pops-shared-rail-in-project ()
  "`|' driven in the project pops the SAME `*org-air-rail*' side window the
board uses: a REAL window, the rail back-pointer is the PROJECT buffer, and
the carried inspector property is `org-air-doc' (the rail inspects DOCS)."
  (skip-unless (locate-library "org-air"))
  (org-air-r24--with-live-project
   (execute-kbd-macro (kbd "|"))
   (should (eq org-air-view--rail-popped-out t))
   (should (window-live-p (org-air-r24--rail-window)))
   (with-current-buffer org-air-rail-buffer-name
     (should (eq org-air-rail--board-buffer (get-buffer "*org-air-project*")))
     (should (eq org-air-view--inspector-property 'org-air-doc)))))

(ert-deftest org-air-r24-5-rail-blocks-shared-with-board ()
  "The popped PROJECT rail emits the SAME block headers as the board, each
faced `org-air-face-rail-header' (shared rail render, not forked).
R26-3 re-bless: the popped rail is height-CLAMPED to the live side
window and drops the Inspector region first when too short (so Actions
stays on-screen) — the all-blocks assertion therefore runs in a TALL
frame where nothing needs to shrink."
  (skip-unless (locate-library "org-air"))
  (set-frame-size (selected-frame) 100 40)
  (unwind-protect
      (org-air-r24--with-live-project
       (execute-kbd-macro (kbd "|"))
       (with-current-buffer org-air-rail-buffer-name
         (dolist (head '("Filter" "Source" "Summary" "Inspector" "Actions"))
           (goto-char (point-min))
           (should (search-forward head nil t))
           (let* ((faces (get-text-property (match-beginning 0) 'face))
                  (fl (if (listp faces) faces (list faces))))
             (should (memq 'org-air-face-rail-header fl))))))
    (set-frame-size (selected-frame) 80 25)))

(ert-deftest org-air-r24-5-inspector-shows-the-doc-in-side-window ()
  "With the inspector region reserved (tall), the popped PROJECT rail's
Inspector shows the DOC at point: its title + the State + Path fields fill
in the side window (the doc inspector, not the board's item inspector)."
  (skip-unless (locate-library "org-air"))
  (let ((org-air-view-height 60))
    (org-air-r24--with-live-project
     (goto-char (org-air-r24--first-doc-pos))
     (org-air-view--goto-row-title)
     (execute-kbd-macro (kbd "|"))
     (org-air-view--inspector-update-now (current-buffer))
     (with-current-buffer org-air-rail-buffer-name
       (let ((txt (substring-no-properties (buffer-string))))
         (should (string-match-p "Alpha feature" txt))
         (should (string-match-p "State" txt))
         (should (string-match-p "Path" txt)))))))

(ert-deftest org-air-r24-5-reconcile-hook-installed-in-project ()
  "The project mode init installs the SHARED cooperative reconciler on the
buffer-local `window-configuration-change-hook' (trunk: absent for project).
The mode init ran with `noninteractive' nil inside the live harness."
  (skip-unless (locate-library "org-air"))
  (org-air-r24--with-live-project
   (should (memq 'org-air-rail--reconcile
                 (buffer-local-value 'window-configuration-change-hook
                                     (current-buffer))))))

(ert-deftest org-air-r24-5-native-close-reconciles-to-inline ()
  "Pop the project rail OUT, close the side window NATIVELY (`delete-window'),
run the reconciler: `org-air-view--rail-popped-out' flips to nil = fall back
to the inline rail (on trunk the reconcile no-ops for the project).
R25-6 re-bless: `org-air-rail--reconcile' now DEFERS the window-mutating work
to a 0s timer (window mutation never runs inside the config-change hook), so
the synchronous flag-flip is driven via `org-air-rail--reconcile-frame'
(the deferred body) — the close-to-inline outcome is unchanged."
  (skip-unless (locate-library "org-air"))
  (org-air-r24--with-live-project
   (execute-kbd-macro (kbd "|"))
   (should (window-live-p (org-air-r24--rail-window)))
   ;; native close.
   (delete-window (org-air-r24--rail-window))
   (should-not (org-air-rail--window-live-p))
   ;; reconcile with a wide render width so it is a genuine user-close
   ;; (not a responsive board-only teardown that keeps the flag).  Run the
   ;; DEFERRED reconcile body synchronously (flush the 0s timer) so the
   ;; flag-flip is observable in the test.
   (let ((org-air-view-width 120))
     (should (org-air-rail--user-closed-p (current-buffer)))
     (org-air-rail--reconcile-frame (selected-frame)))
   (should (null org-air-view--rail-popped-out))))

;;;; =====================================================================
;;;; R24-2 — tree rails extend DOWN to the leaf doc rows.
;;;; =====================================================================

(defun org-air-r24-2--fixture-docs ()
  "Return the fixture project's docs (frozen path/mtime not needed here)."
  (org-air-project--collect-docs org-air-project-test-root))

(defun org-air-r24-2--insert-tree (tree width)
  "Insert TREE at WIDTH into the current buffer with the project render
dynamics bound (char dims + meta widths) so doc rows (incl. the GUI svg
pill path) render without unbound globals."
  (let* ((dims (org-air-view--char-dimensions))
         (org-air-view--pill-char-w (car dims))
         (org-air-view--pill-char-h (cdr dims))
         (org-air-view-width width)
         (mw (org-air-project--fit-meta-widths (org-air-r24-2--fixture-docs) width))
         (org-air-project--meta-date-w (nth 0 mw))
         (org-air-project--meta-tags-w (nth 1 mw))
         (org-air-project--meta-origin-w (nth 2 mw)))
    (org-air-project--insert-directory-tree tree width)))

(ert-deftest org-air-r24-2-doc-row-carries-tree-rail ()
  "A DOC row's leading gutter carries a faded tree CONNECTOR (box-tee-left/
box-bottom-left + box-horizontal, ascii `+-' in batch) in `org-air-face-air-
tree' — the rail reaches the leaf.  R26-1 re-bless: the run AFTER the corner
is `box-horizontal' (`-' batch) up to ONE breathing-room SPACE that joins
the arm to the badge (`+---- READY'; R25-1's flush `+-----[R]' contract is
superseded)."
  (skip-unless (locate-library "org-air"))
  (let* ((docs (org-air-r24-2--fixture-docs))
         (tree (org-air-project--directory-tree docs))
         (hbar (org-air-layout-glyph 'box-horizontal)))
    (org-air-test-with-frozen-project-path org-air-project-test-root
      (org-air-project-test--with-frozen-mtime
        (with-temp-buffer
          (org-air-r24-2--insert-tree tree 100)
          (goto-char (point-min))
          ;; the top dir's own doc `Alpha feature' leads with a connector at
          ;; the marker column (NO leading ancestor rail char before it),
          ;; then a box-horizontal run + ONE joining space before the badge.
          (should (re-search-forward "^ *\\([-+|]\\)\\([-+|]\\)-* READY Alpha feature"
                                     nil t))
          (let* ((c1 (match-beginning 1)) (c2 (match-beginning 2))
                 (badge (- (match-end 0) (length " Alpha feature")
                           (length "READY"))))
            (should (member (char-to-string (char-after c1))
                            (list (org-air-layout-glyph 'box-bottom-left)
                                  (org-air-layout-glyph 'box-tee-left))))
            (should (equal (char-to-string (char-after c2)) hbar))
            (should (eq (get-text-property c1 'face) 'org-air-face-air-tree))
            (should (eq (get-text-property c2 'face) 'org-air-face-air-tree))
            ;; R26-1: the run from the corner to the badge is box-horizontal
            ;; up to its LAST cell, which is exactly ONE space (the join).
            (let ((run (buffer-substring-no-properties (1+ c1) badge)))
              (should (> (length run) 1))
              (should (equal (substring run -1) " "))
              (should (cl-every (lambda (ch) (equal (char-to-string ch) hbar))
                                (substring run 0 -1))))
            ;; anti-tautology: a TOP doc's connector sits at the marker column
            ;; (no rail glyph in the two leading columns before it).
            (should (string-match-p "\\` *\\'"
                                    (buffer-substring-no-properties
                                     (line-beginning-position) c1)))))))))

(ert-deftest org-air-r24-2-nested-doc-has-ancestor-rail ()
  "Anti-tautology: a doc under a NON-last depth-1 dir carries a faded
`box-vertical' ANCESTOR rail (batch `|') to the LEFT of its own connector,
faced `org-air-face-air-tree' — the rail threads the leaf down the branch."
  (skip-unless (locate-library "org-air"))
  (let* ((docs (org-air-r24-2--fixture-docs))
         (doc (car docs))
         (tree (list
                (list :dir "v0.1" :depth 0 :path "v0.1" :own-docs nil
                      :direct-counts nil :desc-counts nil
                      :children
                      (list
                       (list :dir "air-template" :depth 1
                             :path "v0.1/air-template" :own-docs (list doc)
                             :direct-counts nil :desc-counts nil :children nil)
                       (list :dir "config" :depth 1 :path "v0.1/config"
                             :own-docs nil :direct-counts nil
                             :desc-counts nil :children nil)))))
         (vrail (org-air-layout-glyph 'box-vertical)))
    (org-air-test-with-frozen-project-path org-air-project-test-root
      (org-air-project-test--with-frozen-mtime
        (with-temp-buffer
          (org-air-r24-2--insert-tree tree 100)
          (goto-char (point-min))
          ;; find the doc row (it carries `org-air-doc').
          (let ((pos (text-property-not-all (point-min) (point-max)
                                            'org-air-doc nil)))
            (should pos)
            (goto-char pos)
            (let* ((bol (line-beginning-position))
                   (line (buffer-substring bol (line-end-position)))
                   (rail-pos (string-match (regexp-quote vrail) line)))
              (should rail-pos)
              (should (eq (get-text-property rail-pos 'face line)
                          'org-air-face-air-tree)))))))))

(ert-deftest org-air-r24-2-last-own-doc-uses-corner ()
  "A dir whose ONLY children are docs (GUI stub): its LAST own doc uses
`box-bottom-left' (└), the earlier ones `box-tee-left' (├)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (let* ((docs (org-air-r24-2--fixture-docs))
           (d1 (nth 0 docs)) (d2 (nth 1 docs))
           (tree (list (list :dir "v9" :depth 0 :path "v9"
                             :own-docs (list d1 d2) :children nil
                             :direct-counts nil :desc-counts nil)))
           (tee    (org-air-layout-glyph 'box-tee-left))
           (corner (org-air-layout-glyph 'box-bottom-left)))
      (should-not (equal tee corner))
      (org-air-test-with-frozen-project-path org-air-project-test-root
        (org-air-project-test--with-frozen-mtime
          (with-temp-buffer
            (org-air-r24-2--insert-tree tree 100)
            ;; collect the DOC rows by their `org-air-doc' property (the state
            ;; cell is a GUI badge here, not the `[R]' token).
            (let (doc-lines)
              (goto-char (point-min))
              (while (not (eobp))
                (when (text-property-not-all (line-beginning-position)
                                             (line-end-position)
                                             'org-air-doc nil)
                  (push (buffer-substring (line-beginning-position)
                                          (line-end-position))
                        doc-lines))
                (forward-line 1))
              (setq doc-lines (nreverse doc-lines))
              (should (= (length doc-lines) 2))
              ;; first own doc -> tee; last own doc -> corner.
              (should (string-match-p (concat "^ *" (regexp-quote tee))
                                      (nth 0 doc-lines)))
              (should (string-match-p (concat "^ *" (regexp-quote corner))
                                      (nth 1 doc-lines))))))))))



(ert-deftest org-air-r24-2-v6-state-cell-column-locked ()
  "V6 lock (R26-2 RELOCK): the rail repaints only the leading gutter — a
doc's state cell starts at EXACTLY the old indent column (margin + (* 2
(1+ depth))), and the TITLE starts at the state-cell column + 6 (the 5-col
word cell + one separator; +2 vs the 3-col bracket era), so the state cell
/ title / right cluster sit at the re-pinned columns."
  (skip-unless (locate-library "org-air"))
  (let* ((docs (org-air-r24-2--fixture-docs))
         (tree (org-air-project--directory-tree docs))
         (margin-w (string-width (org-air-view--item-margin))))
    (org-air-test-with-frozen-project-path org-air-project-test-root
      (org-air-project-test--with-frozen-mtime
        (with-temp-buffer
          (org-air-r24-2--insert-tree tree 100)
          ;; v0.1/ own doc `Alpha feature' (depth 0): the badge at margin + 2,
          ;; the title at badge + 6 (R26-2: cell-w 5 + separator).
          (goto-char (point-min))
          (should (re-search-forward "READY Alpha feature" nil t))
          (let ((col (save-excursion
                       (goto-char (match-beginning 0))
                       (current-column))))
            (should (= col (+ margin-w (* 2 (1+ 0)))))
            (should (= (+ col org-air-project--state-cell-w 1)
                       (save-excursion
                         (goto-char (match-beginning 0))
                         (search-forward "Alpha")
                         (goto-char (match-beginning 0))
                         (current-column)))))
          ;; air-context/ own doc `Gamma context' (depth 1): badge at margin+4.
          (goto-char (point-min))
          (should (re-search-forward "DRAFT Gamma context" nil t))
          (let ((col (save-excursion
                       (goto-char (match-beginning 0))
                       (current-column))))
            (should (= col (+ margin-w (* 2 (1+ 1)))))))))))

(ert-deftest org-air-r24-2-depth-2-leaf-carries-two-ancestor-rails ()
  "Depth>=2 gap: a doc nested under TWO non-last ancestor dirs carries
*exactly two* faded `box-vertical' ancestor rails (batch `|') to the LEFT of
its own connector — the rail COUNT scales with depth (airctl `-Da' threads
one `|' per non-last ancestor down to the leaf), each rail + the connector is
`org-air-face-air-tree', and the V6 state cell stays column-locked at
`margin + (* 2 (1+ 2))' (R26-2: located via the first UPPERCASE cell — the
word cells carry no `[').  Anti-tautology vs the flat depth-1 case: a
single-rail (or three-rail) gutter would fail the `= 2' count."
  (skip-unless (locate-library "org-air"))
  (let* ((docs (org-air-r24-2--fixture-docs))
         (doc (car docs))
         ;; v0.1/(0) -> A(1, NON-last: Z follows) -> B(2, NON-last: C follows)
         ;; -> doc.  So the leaf hangs under two non-last ancestors and must
         ;; carry two `|' rails (one per non-last ancestor).
         (tree (list
                (list :dir "v0.1" :depth 0 :path "v0.1" :own-docs nil
                      :direct-counts nil :desc-counts nil
                      :children
                      (list
                       (list :dir "A" :depth 1 :path "v0.1/A" :own-docs nil
                             :direct-counts nil :desc-counts nil
                             :children
                             (list
                              (list :dir "B" :depth 2 :path "v0.1/A/B"
                                    :own-docs (list doc) :direct-counts nil
                                    :desc-counts nil :children nil)
                              (list :dir "C" :depth 2 :path "v0.1/A/C"
                                    :own-docs nil :direct-counts nil
                                    :desc-counts nil :children nil)))
                       (list :dir "Z" :depth 1 :path "v0.1/Z" :own-docs nil
                             :direct-counts nil :desc-counts nil
                             :children nil)))))
         (vrail   (org-air-layout-glyph 'box-vertical))
         (hrail   (org-air-layout-glyph 'box-horizontal))
         (tee     (org-air-layout-glyph 'box-tee-left))
         (corner  (org-air-layout-glyph 'box-bottom-left))
         (margin-w (string-width (org-air-view--item-margin))))
    (org-air-test-with-frozen-project-path org-air-project-test-root
      (org-air-project-test--with-frozen-mtime
        (with-temp-buffer
          (org-air-r24-2--insert-tree tree 100)
          ;; locate the single doc row by its `org-air-doc' property.
          (let ((pos (text-property-not-all (point-min) (point-max)
                                            'org-air-doc nil)))
            (should pos)
            (goto-char pos)
            (let* ((bol (line-beginning-position))
                   (line (buffer-substring-no-properties
                          bol (line-end-position)))
                   ;; the connector is the FIRST tee/corner glyph; the gutter
                   ;; is everything before it.
                   (conn-col (or (string-match (regexp-quote tee) line)
                                 (string-match (regexp-quote corner) line)))
                   (gutter (and conn-col (substring line 0 conn-col))))
              (should conn-col)
              ;; EXACTLY two ancestor rails in the gutter (depth 2).
              (should (= 2 (cl-count (string-to-char vrail) gutter)))
              ;; both rails sit at the ancestor cell columns (2 and 5) and are
              ;; faded `org-air-face-air-tree'.
              (dolist (col '(2 5))
                (should (equal (char-to-string (char-after (+ bol col))) vrail))
                (should (eq (get-text-property (+ bol col) 'face)
                            'org-air-face-air-tree)))
              ;; the connector + its horizontal lead are also air-tree faced.
              (should (member (char-to-string (char-after (+ bol conn-col)))
                              (list tee corner)))
              (should (eq (get-text-property (+ bol conn-col) 'face)
                          'org-air-face-air-tree))
              (should (equal (char-to-string (char-after (+ bol conn-col 1)))
                             hrail))
              ;; V6 LOCK at depth 2: the state cell (the first UPPERCASE
              ;; cell) lands at margin + (* 2 (1+ 2)) -- the deeper rail
              ;; did not shove the cluster.
              (let ((badge (string-match "[A-Z]" line)))
                (should badge)
                (should (= badge (+ margin-w (* 2 (1+ 2)))))))))))))

;;;; =====================================================================
;;;; R24-3 — project state badges: fixed-width SVG (default) / nerd / token.
;;;; =====================================================================

(ert-deftest org-air-r24-3-default-style-is-svg ()
  "R24-3 re-reverses R23-4: `org-air-project-state-style' default flips
`emoji' -> `svg' (a fixed-width cell-locked chip that cannot jitter rails)."
  (skip-unless (locate-library "org-air"))
  (should (eq (default-value 'org-air-project-state-style) 'svg)))

(ert-deftest org-air-r24-3-batch-state-cell-is-token-byte-guard ()
  "BYTE GUARD: under --batch (no graphical frame) with the `svg' DEFAULT the
state cell's TRUE text is the padded 5-col WORD token (R26-2 re-bless of
the `[R]'-style cells) — no emoji, no nerd glyph leaks (the R21-4 contract
holds; the project goldens pin the same bytes)."
  (skip-unless (locate-library "org-air"))
  (should-not (display-graphic-p))                 ; batch precondition
  (should (eq org-air-project-state-style 'svg))   ; the shipped default
  (pcase-dolist (`(,state . ,token)
                 '(("ready" . "READY ") ("complete" . "COMP  ")
                   ("dropped" . "DROP  ") ("draft" . "DRAFT ")))
    (let ((cell (substring-no-properties (org-air-project--state-cell state))))
      (should (equal cell token))
      ;; no nerd PUA glyph + no emoji code point leaked into the batch cell.
      (dolist (g org-air-project-state-nerd-glyphs)
        (should-not (string-match-p (regexp-quote (cdr g)) cell))))))

(ert-deftest org-air-r24-3-svg-badge-on-gui-is-cell-locked-image ()
  "On a graphical frame (stubbed) `--state-svg-badge' returns the token
carrying a `display' IMAGE whose width is the token's cell box — R26-2:
the uniform 5-col word capsule, so 5 * char-px (was 3) — and the TRUE
text stays the padded word token `READY' (the contract)."
  (skip-unless (locate-library "org-air"))
  (let* ((dims (org-air-view--char-dimensions))
         (org-air-view--pill-char-w (or (car dims) 8))
         (org-air-view--pill-char-h (or (cdr dims) 16)))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) t)))
      (should (org-air-view--svg-available-p))
      (let* ((badge (org-air-project--state-svg-badge "ready"))
             (disp (get-text-property 0 'display badge)))
        (should (equal (substring-no-properties badge) "READY"))
        (should (imagep disp))
        (should (integerp (image-property disp :width)))
        (should (= (image-property disp :width)
                   (* org-air-project--state-cell-w
                      org-air-view--pill-char-w)))))))

(ert-deftest org-air-r24-3-nerd-and-text-styles ()
  "With style `nerd' (GUI, glyph displayable) `--state-nerd' returns the
configured glyph; with `text' the cell is the plain token; with `emoji' it
routes through `--state-emoji'."
  (skip-unless (locate-library "org-air"))
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) t))
            ((symbol-function 'char-displayable-p) (lambda (_c) t)))
    ;; nerd: the configured PUA glyph.
    (should (equal (substring-no-properties (org-air-project--state-nerd "ready"))
                   (cdr (assoc "ready" org-air-project-state-nerd-glyphs))))
    (let ((org-air-project-state-style 'nerd))
      (should (equal (substring-no-properties
                      (org-air-project--state-badge-cell "ready"))
                     (cdr (assoc "ready" org-air-project-state-nerd-glyphs)))))
    ;; text: plain token only (no image, no glyph).
    (let ((org-air-project-state-style 'text))
      (let ((cell (org-air-project--state-badge-cell "ready")))
        (should (equal (substring-no-properties cell) "READY"))
        (should-not (get-text-property 0 'display cell))))
    ;; emoji: routes through --state-emoji.
    (let ((org-air-project-state-style 'emoji))
      (should (string-match-p (regexp-quote (org-air-project--state-emoji "ready"))
                              (org-air-project--state-badge-cell "ready"))))))

(ert-deftest org-air-r24-3-state-cell-text-fits-fixed-cell ()
  "PIXEL-LOCK: for EVERY state the TEXT-layer width of `--state-badge-cell'
is <= `org-air-project--state-cell-w', so `--state-cell' always pads it to
the fixed cell and the title left edge / R24-2 rails never shift."
  (skip-unless (locate-library "org-air"))
  (dolist (state '("draft" "ready" "work-in-progress" "review"
                   "complete" "dropped" "unknown"))
    (ert-info ((format "state %s" state))
      (should (<= (string-width (substring-no-properties
                                 (org-air-project--state-badge-cell state)))
                  org-air-project--state-cell-w)))))

(ert-deftest org-air-r24-3-rails-stay-aligned-under-svg-default ()
  "Cross-item: with the `svg' DEFAULT the directory tree's doc-row state
cells stay V6-locked (batch => the R26-2 `READY' word token), so the R24-2
rails align — a doc's state-cell column equals margin + (* 2 (1+ depth))
and the title follows at cell-w + 1 (the uniform +2 shift is downstream of
the cell, never the gutter)."
  (skip-unless (locate-library "org-air"))
  (should (eq org-air-project-state-style 'svg))
  (let* ((docs (org-air-r24-2--fixture-docs))
         (tree (org-air-project--directory-tree docs))
         (margin-w (string-width (org-air-view--item-margin))))
    (org-air-test-with-frozen-project-path org-air-project-test-root
      (org-air-project-test--with-frozen-mtime
        (with-temp-buffer
          (org-air-r24-2--insert-tree tree 100)
          (goto-char (point-min))
          (should (re-search-forward "READY Alpha feature" nil t))
          (let ((col (save-excursion (goto-char (match-beginning 0))
                                     (current-column))))
            (should (= col (+ margin-w (* 2 (1+ 0)))))))))))

;;;; =====================================================================
;;;; R24-1 — refile "Under heading" prompt: complete over the file's headings.
;;;; =====================================================================

(defmacro org-air-r24-1--with-org-file (var content &rest body)
  "Write CONTENT to a fresh temp Org file bound to VAR; run BODY; clean up."
  (declare (indent 2) (debug t))
  `(let ((,var (make-temp-file "org-air-r24-1-" nil ".org")))
     (unwind-protect
         (progn
           (with-temp-file ,var (insert ,content))
           ,@body)
       (let ((kill-buffer-query-functions nil)
             (b (get-file-buffer ,var)))
         (when (buffer-live-p b)
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b)))
       (delete-file ,var))))

(ert-deftest org-air-r24-1-file-headings-top-and-nested-plain ()
  "`org-air-inbox--file-headings' collects top-level AND nested heading TEXT
as PLAIN strings (no leaked org-level face), in buffer order."
  (skip-unless (locate-library "org-air"))
  (org-air-r24-1--with-org-file f
      "* Projects\n** Alpha\n* Someday\n* Reading list\n"
    (let ((heads (org-air-inbox--file-headings f)))
      (should (equal heads '("Projects" "Alpha" "Someday" "Reading list")))
      (dolist (h heads)
        (should (null (text-properties-at 0 h)))))))

(ert-deftest org-air-r24-1-read-heading-default-is-file-end ()
  "`(file end)' / empty => nil (append at file end); the resolver then lands
at `point-max'."
  (skip-unless (locate-library "org-air"))
  (org-air-r24-1--with-org-file f "* Projects\n* Someday\n"
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "(file end)")))
      (should (null (org-air-inbox--read-heading f))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "")))
      (should (null (org-air-inbox--read-heading f))))
    ;; nil heading = append at file end (point-max).
    (let ((m (org-air-inbox--target-position f nil)))
      (with-current-buffer (marker-buffer m)
        (org-with-wide-buffer
         (should (= (marker-position m) (point-max))))))))

(ert-deftest org-air-r24-1-read-heading-resolves-chosen-subtree ()
  "A chosen heading is returned verbatim and resolves to the END of that
subtree via the unchanged `--target-position'."
  (skip-unless (locate-library "org-air"))
  (org-air-r24-1--with-org-file f
      "* Projects\nbody p\n* Someday\nbody s\n* Reading list\n"
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "Someday")))
      (should (equal (org-air-inbox--read-heading f) "Someday")))
    (let ((m (org-air-inbox--target-position f "Someday")))
      (with-current-buffer (marker-buffer m)
        (org-with-wide-buffer
         ;; the insertion point sits at the start of the NEXT subtree
         ;; (end of `Someday'), i.e. before `* Reading list'.
         (goto-char (marker-position m))
         (should (looking-at-p "\\*+ Reading list")))))))

(ert-deftest org-air-r24-1-no-headings-skips-the-prompt ()
  "A file with NO headings => `--read-heading' returns nil WITHOUT ever
calling `completing-read' (cl-letf a throwing stub to prove it)."
  (skip-unless (locate-library "org-air"))
  (org-air-r24-1--with-org-file f "Just a note, no headings here.\n"
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (error "completing-read must NOT be called"))))
      (should (null (org-air-inbox--read-heading f))))))

(ert-deftest org-air-r24-1-move-target-vocabulary-is-the-file-headings ()
  "Anti-tautology: driving `--read-move-target', the SECOND completion's
COLLECTION is the file's headings led by `(file end)' (not a free read)."
  (skip-unless (locate-library "org-air"))
  (org-air-r24-1--with-org-file f "* Projects\n** Alpha\n* Someday\n"
    (let ((calls 0) (heading-collection nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt collection &rest _)
                   (setq calls (1+ calls))
                   (cond
                    ;; 1st prompt: pick the file.
                    ((string-match-p "Move to file" prompt) f)
                    ;; 2nd prompt: capture the heading vocabulary, pick default.
                    (t (setq heading-collection collection)
                       "(file end)"))))
                ;; the file picker resolves the choice straight to F.
                ((symbol-function 'org-air-inbox--target-files)
                 (lambda (&rest _) (list f)))
                ((symbol-function 'org-air-inbox--decode-file-choice)
                 (lambda (&rest _) f)))
        (let ((res (org-air-inbox--read-move-target
                    (org-air-item-create :title "x" :file f))))
          (should (= calls 2))
          (should (member "(file end)" heading-collection))
          (should (member "Projects" heading-collection))
          (should (member "Alpha" heading-collection))
          (should (member "Someday" heading-collection))
          ;; `(file end)' default => no heading in the move target.
          (should (equal (car res) f))
          (should (null (cdr res))))))))

;;;; =====================================================================
;;;; R24-6 — free-text (content) filter: bare token = substring, #token = tag.
;;;; =====================================================================

(ert-deftest org-air-r24-6-bare-token-substring-matches-title ()
  "A BARE token = case-insensitive SUBSTRING over the searchable text
(title+path+tag-names); on trunk the tags-only matcher returned nil."
  (skip-unless (locate-library "org-air"))
  (let ((org-air-filter-match 'all))
    (let ((org-air-view--tag-filter '("feature")))
      ;; the title carries "feature"; the tags do NOT.
      (should (org-air-view--tokens-pass-filter-p "Alpha feature" '("ui" "core")))
      ;; a doc whose title/path/tags lack "feature" does NOT pass.
      (should-not (org-air-view--tokens-pass-filter-p "Beta CLI" '("core"))))))

(ert-deftest org-air-r24-6-hash-token-tag-matches ()
  "A `#tag' token = exact TAG membership; a BARE tag name still finds its
tagged items (the tag NAME is in the searchable text) AND any title substring."
  (skip-unless (locate-library "org-air"))
  (let ((org-air-filter-match 'all))
    ;; #ui tag-matches a #ui doc; rejects a non-#ui doc even if its text is ui-less.
    (let ((org-air-view--tag-filter '("#ui")))
      (should (org-air-view--tokens-pass-filter-p "Alpha feature" '("ui" "core")))
      (should-not (org-air-view--tokens-pass-filter-p "Beta CLI" '("core"))))
    ;; bare `ui' passes BOTH the #ui doc (tag name in text) and a title with ui.
    (let ((org-air-view--tag-filter '("ui")))
      (should (org-air-view--tokens-pass-filter-p "Beta CLI" '("ui")))
      (should (org-air-view--tokens-pass-filter-p "Delta UI exploration" '("core"))))))

(ert-deftest org-air-r24-6-mixed-tokens-and-or ()
  "Mixed #tag + bare tokens combine through the existing AND/OR combinator."
  (skip-unless (locate-library "org-air"))
  (let ((org-air-view--tag-filter '("#ui" "plan")))
    ;; AND: only a #ui doc whose text also says "plan".
    (let ((org-air-filter-match 'all))
      (should (org-air-view--tokens-pass-filter-p "Epsilon plan" '("ui" "context")))
      (should-not (org-air-view--tokens-pass-filter-p "Alpha feature" '("ui" "core")))
      (should-not (org-air-view--tokens-pass-filter-p "Master plan" '("core"))))
    ;; OR: a #ui doc OR a doc whose text says "plan".
    (let ((org-air-filter-match 'any))
      (should (org-air-view--tokens-pass-filter-p "Alpha feature" '("ui" "core")))
      (should (org-air-view--tokens-pass-filter-p "Master plan" '("core")))
      (should-not (org-air-view--tokens-pass-filter-p "Beta CLI" '("core"))))))

(ert-deftest org-air-r24-6-case-insensitive ()
  "The substring match is case-insensitive in BOTH directions."
  (skip-unless (locate-library "org-air"))
  (let ((org-air-filter-match 'all)
        (org-air-view--tag-filter '("GIT")))
    (should (org-air-view--tokens-pass-filter-p "migrate Git cache" '())))
  (let ((org-air-filter-match 'all)
        (org-air-view--tag-filter '("git")))
    (should (org-air-view--tokens-pass-filter-p "Migrate GIT cache" '()))))

(ert-deftest org-air-r24-6-board-passes-filter-shares-core ()
  "The board `--passes-filter-p' routes through the SAME core, passing the
item title+origin as searchable text: a `#tag' token tag-matches; a bare
token substring-matches a title even with NO matching tag."
  (skip-unless (locate-library "org-air"))
  (let ((item (org-air-item-create
               :title "migrate git cache" :tags '("core")
               :file "/tmp/notes.org")))
    (let ((org-air-filter-match 'all))
      ;; bare token matches the TITLE though there is no #git tag.
      (let ((org-air-view--tag-filter '("git")))
        (should (org-air-view--passes-filter-p item)))
      ;; #core tag-matches; #git does not (no such tag).
      (let ((org-air-view--tag-filter '("#core")))
        (should (org-air-view--passes-filter-p item)))
      (let ((org-air-view--tag-filter '("#git")))
        (should-not (org-air-view--passes-filter-p item))))))

(ert-deftest org-air-r24-6-project-filter-bare-token-narrows-driven ()
  "Driven end-to-end: `org-air-project-filter' with a BARE token narrows the
project docs by TITLE substring (trunk: 0 docs), and the Alpha feature doc
remains visible."
  (skip-unless (locate-library "org-air"))
  (let ((org-air-project-view-width 120))
    (org-air-project-test--render
     (let ((all org-air-project--doc-count))
       (should (and all (> all 0)))
       (org-air-project-filter '("feature"))
       ;; the bare token narrows (it is not a tag, yet matches a title).
       (should (> org-air-project--doc-count 0))
       (should (< org-air-project--doc-count all))
       ;; the Alpha feature doc (title carries "feature") survives.
       (should (string-match-p "Alpha feature"
                               (substring-no-properties (buffer-string))))
       ;; clearing restores the full set.
       (org-air-filter-clear)
       (should (= org-air-project--doc-count all))))))

(ert-deftest org-air-r24-6-display-label-verbatim ()
  "With `(#ui git)' active, the rail Filter line shows `#ui' verbatim and
the bare token quoted (`\"git\"'), joined by the combinator word."
  (skip-unless (locate-library "org-air"))
  (with-temp-buffer
    (let ((org-air-show-rail-filters t)
          (org-air-view--tag-filter '("#ui" "git"))
          (org-air-view--scope nil)
          (org-air-filter-match 'all))
      (org-air-view--insert-rail-filters 60)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "#ui" text))
        (should (string-match-p "\"git\"" text))
        (should (string-match-p "#ui AND \"git\"" text))))))

(provide 'org-air-round24-test)
;;; org-air-round24-test.el ends here
