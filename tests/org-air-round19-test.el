;;; org-air-round19-test.el --- round-19 substantive suite for org-air -*- lexical-binding: t; -*-

;;; Commentary:
;; Spec-true tests for v0.5 round-19 (air/v0.5/org-air-round19-design.org).
;; Four independently-implementable items, each tested against the REAL
;; impl (no fixture re-derivation — the byte goldens are re-blessed in the
;; regen commit; these are behaviour/assertion tests):
;;
;;   R19-1  cold first load — the `org-air-query-items-in-files' subset
;;          building block and the chrome-only loading skeleton survive
;;          R20-1; the chained-idle-timer loader is gone (its async ERTs
;;          moved to org-air-round20-test.el as the synchronous fast-paint
;;          contract).
;;   R19-2  refile UX — the prompt SHOWS the current tags, the edit-tags
;;          step PRE-FILLS them, and move-to-another-file actually
;;          RELOCATES the heading (incl. the directory-source decode bug
;;          regression net).
;;   R19-3  editable view pane — `org-air-view-pane--indirect' is a live,
;;          narrowed Org buffer on the file's base buffer whose edits hit
;;          DISK; the pane SPLITS the board window (below-selected), not a
;;          frame-level side window, so the rail keeps its height; batch
;;          keeps the read-only snapshot (byte-identical).
;;   R19-4  rail order — Calendar -> Filter -> Summary -> Inspector ->
;;          Actions (Scope between Filter and Summary), the `\\ clears'
;;          hint appears when a filter is active, scope candidates carry
;;          NO `#tag', and the rail mode-line is calm + buffer-local.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)
(require 'org-air)
(require 'org-air-inbox)
(require 'org-air-project)

;;;; ---------------------------------------------------------------------
;;;; Shared helpers.
;;;; ---------------------------------------------------------------------

(defmacro org-air-r19--with-temp-org (spec &rest body)
  "Bind dir/files per SPEC, write them, run BODY, clean up buffers + dir.
SPEC is ((DIRVAR) (VAR PATH CONTENT)...): DIRVAR holds a fresh temp dir;
each VAR is bound to an absolute path under it pre-populated with CONTENT."
  (declare (indent 1) (debug t))
  (let ((dirvar (caar spec))
        (files (cdr spec)))
    `(let* ((,dirvar (make-temp-file "org-air-r19-" t))
            ,@(mapcar (lambda (f)
                        `(,(nth 0 f) (expand-file-name ,(nth 1 f) ,dirvar)))
                      files))
       (unwind-protect
           (progn
             ,@(mapcar (lambda (f)
                         `(with-temp-file ,(nth 0 f) (insert ,(nth 2 f))))
                       files)
             ,@body)
         (let ((kill-buffer-query-functions nil))
           (dolist (buf (buffer-list))
             (let ((fn (buffer-file-name buf)))
               (when (and fn (string-prefix-p ,dirvar fn))
                 (with-current-buffer buf (set-buffer-modified-p nil))
                 (kill-buffer buf)))))
         (delete-directory ,dirvar t)))))

;;;; ---------------------------------------------------------------------
;;;; R19-1 — async first load: chrome first, query in the background.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r19-1-subset-query-equals-full-restricted ()
  "`org-air-query-items-in-files' over a 2-file subset returns exactly the
items the full `org-air-query-items' yields for those two files (the
batchable building block the timer-chunked loader stands on)."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (org-air-viewport-test--with-frozen-now
      (let* ((files (org-air-query-files))
             (subset (seq-take files 2))
             (subset-titles
              (sort (mapcar #'org-air-item-title
                            (org-air-query-items-in-files subset))
                    #'string<))
             (full-restricted
              (sort (mapcar #'org-air-item-title
                            (seq-filter
                             (lambda (it)
                               (member (file-truename (org-air-item-file it))
                                       subset))
                             (org-air-query-items)))
                    #'string<)))
        (should (= (length subset) 2))
        (should subset-titles)
        (should (equal subset-titles full-restricted))))))

(ert-deftest org-air-r19-1-loading-skeleton-is-chrome-only ()
  "The loading skeleton is a deterministic chrome paint: banner + rule +
a centred `Loading your board…' body + footer, sized to the render seam,
carrying NO item rows."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (org-air-viewport-test--with-frozen-now
      (let ((org-air-view-width 120)
            (org-air-view-height 50)
            (buf (get-buffer-create "*org-air-r19-skel*")))
        (unwind-protect
            (with-current-buffer buf
              (org-air-view-mode)
              (setq org-air-view--loading t)
              (org-air-view--render-loading)
              (let ((text (substring-no-properties (buffer-string))))
                (should (string-match-p "Loading your board" text))
                (should-not (string-match-p "Ship quarterly report" text)))
              ;; the chrome is composed to the width seam (a real paint).
              (org-air-viewport-test-assert-aligned 120))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

;;;; ---------------------------------------------------------------------
;;;; R19-2 — refile UX: show tags, pre-fill, real move-to-another-file.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r19-2-refile-prompt-shows-tags-and-move-relocates ()
  "The refile prompt SHOWS the item's current tags `[#urgent #work]', the
`⌂' move candidate is offered, and choosing it actually RELOCATES the
heading to the target file (gone from A, present in B on disk).  Driven
with a DIRECTORY `org-air-files' — the exact config that broke the move
before R19-2."
  (skip-unless (locate-library "org-air"))
  (org-air-r19--with-temp-org
      ((dir)
       (a "a.org" "* TODO Pay the invoice :urgent:work:\n  :PROPERTIES:\n  :CREATED: [2026-06-01 Mon]\n  :END:\n  body\n")
       (b "b.org" "* Existing target\n"))
    (let* ((org-air-files (list dir))         ; a DIRECTORY (the bug trigger)
           (org-air-inbox-file a)
           (b-cand (concat "⌂ " (file-name-nondirectory b)))
           (captured-prompt nil)
           (captured-coll nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt coll &rest _)
                   (setq captured-prompt prompt captured-coll coll)
                   b-cand)))
        (with-current-buffer (find-file-noselect a)
          (goto-char (point-min))
          (re-search-forward "^\\* TODO Pay the invoice")
          (goto-char (line-beginning-position))
          (let ((inhibit-message t))
            (call-interactively 'org-air-refile-item))))
      ;; (a) the prompt surfaced the CURRENT tags.
      (should (string-match-p (regexp-quote "[#urgent #work]") captured-prompt))
      ;; the real `⌂' file target was actually among the candidates.
      (should (member b-cand captured-coll))
      ;; (b) the heading RELOCATED.
      (with-temp-buffer
        (insert-file-contents a)
        (should-not (string-match-p "Pay the invoice" (buffer-string))))
      (with-temp-buffer
        (insert-file-contents b)
        (should (string-match-p "Pay the invoice" (buffer-string)))
        ;; tags rode along with the moved subtree.
        (should (string-match-p ":urgent:work:" (buffer-string)))))))

(ert-deftest org-air-r19-2-decode-target-directory-source-resolves-real-file ()
  "Regression net for the move bug: with `org-air-files' a DIRECTORY,
`org-air-inbox--decode-target' for a `⌂ <file>' choice resolves to the
REAL expanded target path (from `org-air-query-files'), NOT the item's own
file (the old silent fallback that made the move a no-op)."
  (skip-unless (locate-library "org-air"))
  (org-air-r19--with-temp-org
      ((dir)
       (a "a.org" "* TODO H :x:\n")
       (b "b.org" "* Existing\n"))
    (let* ((org-air-files (list dir))         ; DIRECTORY, not files
           (item (org-air-item-create
                  :title "H" :tags '("x") :file a
                  :marker (with-current-buffer (find-file-noselect a)
                            (goto-char (point-min)) (point-marker))))
           (cand (concat "⌂ " (file-name-nondirectory b)))
           (decoded (org-air-inbox--decode-target cand item)))
      ;; decoded = (ITEM TARGET-FILE HEADING TAGS SCHEDULED).
      (should (equal (file-truename (nth 1 decoded)) (file-truename b)))
      (should-not (equal (file-truename (nth 1 decoded)) (file-truename a))))))

(ert-deftest org-air-r19-2-edit-tags-prefills-current-and-replaces ()
  "The `# edit tags…' step opens `completing-read-multiple' PRE-FILLED with
the item's current tags (comma-joined, so the user SEES the full set), and
the chosen list REPLACES the tags — add AND remove both reflected."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (let* ((item (org-air-item-create
                  :title "x" :tags '("alpha" "beta")
                  :file (car org-air-files) :marker (point-marker)))
           (captured-initial 'unset))
      (cl-letf (((symbol-function 'completing-read-multiple)
                 (lambda (_prompt _coll &optional _pred _req initial &rest _)
                   (setq captured-initial initial)
                   '("alpha" "gamma"))))      ; remove beta, add gamma
        (let ((result (org-air-inbox--edit-tags item)))
          ;; pre-filled with the CURRENT tags, comma-joined.
          (should (equal captured-initial "alpha,beta"))
          ;; the returned list replaces the tags wholesale.
          (should (equal result '("alpha" "gamma"))))))))

;;;; ---------------------------------------------------------------------
;;;; R19-3 — editable view pane: live indirect Org buffer + board split.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r19-3-pane-indirect-edits-write-through-to-disk ()
  "`org-air-view-pane--indirect' is a LIVE, narrowed Org indirect buffer on
the file's base buffer — edits write through and `save-buffer' persists
them to DISK; killing the indirect never loses text (the base survives)."
  (skip-unless (locate-library "org-air"))
  (org-air-r19--with-temp-org
      ((dir)
       (file "doc.org"
             "* TODO First heading :a:\n  one\n* TODO Second heading :b:\n  two\n"))
    (let* ((base (find-file-noselect file))
           (pos (with-current-buffer base
                  (goto-char (point-min))
                  (re-search-forward "^\\* TODO First heading")
                  (match-beginning 0)))
           (ind (org-air-view-pane--indirect base pos "First heading")))
      (unwind-protect
          (with-current-buffer ind
            ;; a real Org indirect buffer on the file's base buffer,
            (should (derived-mode-p 'org-mode))
            (should (eq (buffer-base-buffer) base))
            ;; narrowed to JUST the first subtree (the second is hidden),
            (should (string-match-p "First heading" (buffer-string)))
            (should-not (string-match-p "Second heading" (buffer-string)))
            ;; edit + save -> the change reaches disk.
            (goto-char (point-max))
            (insert "  edited-via-pane\n")
            (let ((inhibit-message t)) (save-buffer)))
        (when (buffer-live-p ind) (kill-buffer ind)))
      ;; killing the indirect kept the base (and its text) alive,
      (should (buffer-live-p base))
      ;; and the edit is on DISK.
      (with-temp-buffer
        (insert-file-contents file)
        (should (string-match-p "edited-via-pane" (buffer-string)))))))

(ert-deftest org-air-r19-3-pane-render-is-readonly-snapshot-in-batch ()
  "Byte-identity gate: under `noninteractive' (the gate / regen), even with
`org-air-view-pane-editable' ON, `org-air-view-pane--render' returns the
unchanged READ-ONLY snapshot buffer — never an indirect buffer — so every
board/pane fixture stays byte-identical."
  (skip-unless (locate-library "org-air"))
  (org-air-r19--with-temp-org
      ((dir)
       (file "doc.org" "* TODO Solo heading :z:\n  body text\n"))
    (let* ((base (find-file-noselect file))
           (mk (with-current-buffer base
                 (goto-char (point-min))
                 (re-search-forward "^\\* TODO Solo heading")
                 (goto-char (match-beginning 0)) (point-marker)))
           (ctx (list :marker mk :file file :title "Solo heading" :state "TODO"))
           (org-air-view-pane-editable t)        ; editable ON...
           (buf (org-air-view-pane--render ctx))) ; ...batch forces snapshot
      (should (eq buf (get-buffer org-air-view-pane-buffer-name)))
      (with-current-buffer buf
        (should buffer-read-only)
        (should (null (buffer-base-buffer)))      ; NOT an indirect buffer
        (should (string-match-p "Solo heading" (buffer-string))))
      (when (get-buffer org-air-view-pane-buffer-name)
        (kill-buffer org-air-view-pane-buffer-name)))))

(ert-deftest org-air-r19-3-pane-window-params-split-board-not-side ()
  "The pane display action SPLITS the board window
\(`display-buffer-below-selected') — it is NOT a frame-level side window
\(no `(side . bottom)'), which is the structural cure for `opening the pane
shortens the rail'.  It is tagged `org-air-pane', survives `C-x 1'
\(`no-delete-other-windows'), and stays `other-window'-reachable."
  (skip-unless (locate-library "org-air"))
  (let ((params (org-air-view-pane--window-params)))
    (should (assq 'display-buffer-below-selected params))
    (should-not (assq 'side params))            ; NOT a side window
    (let ((wps (cdr (assq 'window-parameters params))))
      (should (assq 'org-air-pane wps))
      (should (assq 'no-delete-other-windows wps))
      (should-not (assq 'no-other-window wps))))) ; other-window-reachable

(ert-deftest org-air-r19-3-pane-splits-board-rail-height-intact ()
  "With a live display: opening the pane SPLITS the board window, so the
rail side window keeps its full frame-body height (the reported `pane
disturbs the rail' bug is cured).  The pane window is NOT a side window."
  (skip-unless (display-graphic-p))
  (org-air-test-with-fixtures
    (save-window-excursion
      (delete-other-windows)
      (with-temp-buffer
        (org-air-view-mode)
        (setq org-air-view--items (org-air-query-items))
        (let* ((board (current-buffer))
               (item (car org-air-view--items))
               (ctx (list :marker (org-air-item-marker item)
                          :file (org-air-item-file item)
                          :title (org-air-item-title item)
                          :state (org-air-item-todo item)))
               rail-win rail-h-before)
          (unwind-protect
              (progn
                (org-air-rail--ensure-window board 120)
                (setq rail-win (get-buffer-window
                                (get-buffer org-air-rail-buffer-name)))
                (should (window-live-p rail-win))
                (setq rail-h-before (window-body-height rail-win))
                (org-air-view-pane--show ctx)
                (should (org-air-view-pane--window-live-p))
                ;; the pane split the BOARD, not the frame: NOT a side window
                (should-not (window-parameter (org-air-view-pane--find-window)
                                              'window-side))
                ;; the rail keeps its full body height.
                (should (= rail-h-before (window-body-height rail-win))))
            (org-air-view-pane--teardown)
            (org-air-rail--hide board)))))))

;;;; ---------------------------------------------------------------------
;;;; R19-4 — rail order, clear-hint, scope/filter split, rail mode-line.
;;;; ---------------------------------------------------------------------

(defun org-air-r19--rail-headers (text marker)
  "Return the rail block header LABELS in TEXT, in order, by MARKER glyph.
Each `<marker> <Word>' occurrence contributes its first word (the calendar
header `▌ June 2026' -> \"June\")."
  (let ((rx (concat (regexp-quote marker) " \\([A-Za-z]+\\)"))
        (start 0) out)
    (while (string-match rx text start)
      (push (match-string 1 text) out)
      (setq start (match-end 0)))
    (nreverse out)))

(ert-deftest org-air-r19-4-rail-order-calendar-filter-summary-inspector-actions ()
  "R19-4c reorder: the rail blocks run Calendar -> Filter -> Summary ->
Inspector -> Actions (the Filter block MOVED UP, between Calendar and
Summary, so the active narrowing is seen before the Summary counts it
explains), with the new R19-4d `Scope' block sitting between Filter and
Summary."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (let ((mk (org-air-layout-glyph 'rail-marker)))
      (org-air-viewport-test-with-dashboard 160
        (let* ((text (substring-no-properties (buffer-string)))
               (headers (org-air-r19--rail-headers text mk))
               (idx (lambda (h) (cl-position h headers :test #'equal))))
          ;; every block is present,
          (dolist (h '("June" "Filter" "Scope" "Summary" "Inspector" "Actions"))
            (should (funcall idx h)))
          ;; the canonical R19-4c vertical order,
          (should (< (funcall idx "June") (funcall idx "Filter")))
          (should (< (funcall idx "Filter") (funcall idx "Summary")))
          (should (< (funcall idx "Summary") (funcall idx "Inspector")))
          (should (< (funcall idx "Inspector") (funcall idx "Actions")))
          ;; and Scope is wedged between Filter and Summary (R19-4d).
          (should (< (funcall idx "Filter") (funcall idx "Scope")))
          (should (< (funcall idx "Scope") (funcall idx "Summary"))))))))

(ert-deftest org-air-r19-4-clear-hint-shows-clear-key-when-filter-active ()
  "R19-4b: with a filter active the Filter block teaches BOTH verbs —
`M-/ toggles' AND the literal `\\ clears' — alongside the AND-combined
chips (the clear key was previously undiscoverable)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 160
      (let ((org-air-filter-match 'all))
        (setq org-air-view--tag-filter '("work" "client"))
        (org-air-view--render-current))
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "M-/ toggles" text))
        (should (string-match-p (regexp-quote "\\ clears") text))
        ;; the chips are AND-combined (R18 D-P2.3 combinator word).
        (should (string-match-p "#work AND #client" text))))))

(ert-deftest org-air-r19-4-scope-candidates-have-no-tag-option ()
  "R19-4d crisp split: `org-air-scope''s candidates are a purely STRUCTURAL
lens — `all' / `@group' / `⌂ file' — with NO `#tag' entry, so Scope and the
live tag Filter no longer overlap (tags belong entirely to `/')."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 160
    (let ((captured nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt coll &rest _) (setq captured coll) "all")))
        (call-interactively 'org-air-scope))
      (should captured)
      (should (member "all" captured))
      (should (cl-some (lambda (c) (string-prefix-p "@" c)) captured))
      (should (cl-some (lambda (c) (string-prefix-p "⌂ " c)) captured))
      ;; the overlap removal: no single-tag scope candidate survives.
      (should-not (cl-some (lambda (c) (string-prefix-p "#" c)) captured)))))

(ert-deftest org-air-r19-4-rail-mode-line-is-calm-and-buffer-local ()
  "R19-4a: the rail buffer carries the calm nano-style `mode-line-format'
\(mirroring the board + pane), and it is BUFFER-LOCAL — a plain temp buffer
and the global default are unchanged, so the rail mode-line cannot bleed to
other side windows."
  (skip-unless (locate-library "org-air"))
  (let ((org-air-modeline-style 'calm)
        (default-before (default-value 'mode-line-format)))
    (when (get-buffer org-air-rail-buffer-name)
      (kill-buffer org-air-rail-buffer-name))
    (let ((rail (org-air-rail--get-buffer)))
      (unwind-protect
          (progn
            (with-current-buffer rail
              (should (equal mode-line-format
                             (list org-air-view--calm-mode-line))))
            ;; no bleed: the GLOBAL default is untouched,
            (should (equal (default-value 'mode-line-format) default-before))
            ;; and a fresh plain buffer keeps the ordinary mode-line.
            (with-temp-buffer
              (should-not (equal mode-line-format
                                 (list org-air-view--calm-mode-line)))))
        (when (buffer-live-p rail) (kill-buffer rail))))))

(provide 'org-air-round19-test)
;;; org-air-round19-test.el ends here
