;;; org-air-round30-test.el --- executing ERTs for v0.5 round-30 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.5 round-30 (air/v0.5/org-air-round30-design.org).
;;
;;   R30-1  RAIL INSPECTOR TITLE — full-wrap (no truncate) + a compact
;;          title/state/tags identity header block atop, shared board +
;;          project.  `org-air-inspector-max-title-lines' now accepts nil
;;          (default) = no cap; a positive integer keeps the ellipsis-cap.
;;
;;   R30-2  MAIN-WINDOW C-c LEADER — a shared `C-c C-a' leader prefix on
;;          the content buffers reaches the rail actions where single keys
;;          self-insert; the legend derives each key context-correctly via
;;          `org-air-view--legend-key'.
;;
;;   R30-3  DASHBOARD COLUMN TOGGLES — `org-air-show-origin' (nil),
;;          `-dates' (t), `-tags' (t) defcustoms gate the V6 meta-width
;;          pass; z-prefix toggles; filter/scope still read the hidden data.
;;
;;   R30-4  org-air-outline-mode — an opt-in minor mode for ANY org buffer
;;          reusing the extracted outline + highlight primitives with NO
;;          org-air-project dependency.
;;
;;   R30-5  DOC-RAIL COVERAGE ERT — a fringe-less GUI-sim ERT revert-
;;          guarding `org-air-project--doc-rail-show' (R29-1 fix site).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)
(require 'org-air-project-test)            ; project fixture root
(require 'org-air-round27-test)            ; live-board/-project harness
(require 'org-air-round28-test)            ; doc-session harness
(require 'org-air-round29-test)            ; fringe-less GUI sim harness

(when (locate-library "org-air")
  (require 'org-air))

;;;; =====================================================================
;;;; R30-1 — rail inspector: full-wrap title + a compact identity block.
;;;; =====================================================================

(defun org-air-r30--line-face-p (line face)
  "Non-nil when any char of LINE carries FACE (directly or in a list)."
  (let ((found nil) (i 0) (n (length line)))
    (while (and (not found) (< i n))
      (let ((f (get-text-property i 'face line)))
        (when (or (eq f face) (and (listp f) (memq face f)))
          (setq found t)))
      (setq i (1+ i)))
    found))

(defun org-air-r30--title-lines (fields)
  "Return the title-face lines of FIELDS, stripped of text properties."
  (mapcar #'substring-no-properties
          (seq-filter (lambda (l) (org-air-r30--line-face-p
                                   l 'org-air-face-title))
                      fields)))

(defun org-air-r30--index (lines re)
  "Return the first index in LINES whose stripped text matches RE, or nil."
  (cl-position-if (lambda (l) (string-match-p
                               re (substring-no-properties l)))
                  lines))

(defconst org-air-r30--long-title
  "Trailer Path Resolution Accept Repo Root Relative Paths Warn on Unresolved"
  "A single-spaced 74-char title that wraps past 4 lines at a narrow rail.")

(defun org-air-r30--board-item ()
  "A synthetic board item with a long title, TODO+[#A], tags and an origin."
  (org-air-item-create
   :title org-air-r30--long-title
   :tags '("backend" "paths" "cli")
   :todo "TODO" :priority ?A
   :file "/tmp/org-air-r30/trailer-origin.org"))

(defun org-air-r30--doc ()
  "A synthetic project doc with a long title, state, tags and a group."
  (org-air-doc-create
   :name org-air-r30--long-title
   :file "/tmp/org-air-r30/v0.5/restore-summary.org"
   :state "ready"
   :tags '("status" "cli")
   :relpath "v0.5/restore-summary.org"))

(ert-deftest org-air-r30-1-title-wraps-not-truncates ()
  "With the default (`org-air-inspector-max-title-lines' nil) a long title
wraps FULLY at a narrow rail: NO title line carries the more glyph, and
re-joining the title lines reproduces the whole title (no lost words).
Trunk FAILED (the 4th line ellipsis-truncated)."
  (skip-unless (locate-library "org-air"))
  (let ((org-air-inspector-max-title-lines nil)
        (more (org-air-view--glyph 'more))
        (width 18))
    (let* ((fields (org-air-view--inspector-item-fields
                    (org-air-r30--board-item) "" width (current-time)))
           (titles (org-air-r30--title-lines fields)))
      ;; the title really wrapped past 4 lines at this width.
      (should (> (length titles) 4))
      ;; no more glyph anywhere in the title block.
      (dolist (tl titles)
        (should-not (string-match-p (regexp-quote more) tl)))
      ;; re-joining the wrapped title reproduces the full title.
      (should (equal (string-join titles " ") org-air-r30--long-title)))))

(ert-deftest org-air-r30-1-maxlines-cap-still-honoured ()
  "With `org-air-inspector-max-title-lines' bound to 2 (the back-compat
knob) a long title caps at exactly 2 title lines and the 2nd carries the
more glyph."
  (skip-unless (locate-library "org-air"))
  (let ((org-air-inspector-max-title-lines 2)
        (more (org-air-view--glyph 'more))
        (width 18))
    (let* ((fields (org-air-view--inspector-item-fields
                    (org-air-r30--board-item) "" width (current-time)))
           (titles (org-air-r30--title-lines fields)))
      (should (= (length titles) 2))
      (should (string-match-p (regexp-quote more) (nth 1 titles))))))

(ert-deftest org-air-r30-1-identity-block-order ()
  "The identity block leads: title line(s), then state, then tag line(s),
BEFORE any metadata KV row (origin/path/date).  Board AND project.  Trunk
FAILED (tags sat AFTER origin/path)."
  (skip-unless (locate-library "org-air"))
  (let ((org-air-inspector-max-title-lines nil)
        (width 40)
        (now (current-time)))
    ;; --- board: title / TODO / #tags / (blank) / origin(date) ---
    (let* ((fields (org-air-view--inspector-item-fields
                    (org-air-r30--board-item) "" width now))
           (state-i (org-air-r30--index fields "\\bTODO\\b"))
           (tags-i  (org-air-r30--index fields "#backend"))
           (origin-i (org-air-r30--index fields "trailer-origin"))
           (last-title (cl-position-if
                        (lambda (l) (org-air-r30--line-face-p
                                     l 'org-air-face-title))
                        fields :from-end t)))
      (should (and state-i tags-i origin-i last-title))
      (should (< last-title state-i))
      (should (< state-i tags-i))
      (should (< tags-i origin-i)))    ; tags ATOP, origin is a KV row below
    ;; --- project: title / State / #tags / (blank) / Path ---
    (let* ((fields (org-air-project--inspector-doc-fields
                    (org-air-r30--doc) "" width now))
           (state-i (org-air-r30--index fields "^State"))
           (tags-i  (org-air-r30--index fields "#status"))
           (path-i  (org-air-r30--index fields "^Path"))
           (last-title (cl-position-if
                        (lambda (l) (org-air-r30--line-face-p
                                     l 'org-air-face-title))
                        fields :from-end t)))
      (should (and state-i tags-i path-i last-title))
      (should (< last-title state-i))
      (should (< state-i tags-i))
      (should (< tags-i path-i)))))

(ert-deftest org-air-r30-1-fits-rail-width ()
  "Every inspector line (header + fields) fits the rail width at 28 / 34 /
44 — the full-wrap title never overflows (bounded by `pad-to')."
  (skip-unless (locate-library "org-air"))
  (let ((org-air-inspector-max-title-lines nil))
    (dolist (width '(28 34 44))
      (let ((lines (org-air-view--inspector-lines
                    (org-air-r30--board-item) width)))
        (dolist (l lines)
          (should (<= (string-width (substring-no-properties l)) width)))))))

(ert-deftest org-air-r30-1-maxtitle-defcustom-type ()
  "`org-air-inspector-max-title-lines' defaults to nil and its Custom
:type accepts BOTH nil (wrap fully) and a positive integer."
  (skip-unless (locate-library "org-air"))
  (should (null (default-value 'org-air-inspector-max-title-lines)))
  (let ((type (get 'org-air-inspector-max-title-lines 'custom-type)))
    (should (equal (car type) 'choice))
    (should (widget-apply (widget-convert type) :match nil))
    (should (widget-apply (widget-convert type) :match 2))))

;;;; =====================================================================
;;;; R30-2 — main-window C-c leader for the rail actions.
;;;; =====================================================================

(ert-deftest org-air-r30-2-leader-reaches-actions-from-doc ()
  "In the doc-session ORG buffer (main window focused, single keys
self-insert) the leader reaches the rail actions: `C-c C-a |' is
`org-air-rail-toggle', `C-c C-a o' jumps the outline
\(`org-air-outline-goto-current-heading'), `C-c C-a q' is
`org-air-project-back'.  Trunk FAILED (no leader; the keys self-insert)."
  (skip-unless (locate-library "org-air"))
  (org-air-r28--with-doc-session
    (with-current-buffer docbuf
      (should (buffer-local-value 'org-air-doc-session-mode docbuf))
      ;; the bare keys DO NOT reach the rail actions in the editable doc
      ;; buffer (they self-insert)...
      (should-not (eq (key-binding (kbd "|")) 'org-air-rail-toggle))
      ;; ...but the leader reaches every action.
      (should (eq (key-binding (kbd "C-c C-a |")) 'org-air-rail-toggle))
      (should (eq (key-binding (kbd "C-c C-a o"))
                  'org-air-outline-goto-current-heading))
      (should (eq (key-binding (kbd "C-c C-a n"))
                  'org-air-outline-next-heading))
      (should (eq (key-binding (kbd "C-c C-a p"))
                  'org-air-outline-prev-heading))
      (should (eq (key-binding (kbd "C-c C-a q")) 'org-air-project-back)))))

(ert-deftest org-air-r30-2-leader-outline-jump-moves-point ()
  "The shared heading-motion primitives really move point over the Org
headings of the current buffer (pure, Air-free): `next'/`prev' step
forward/back, `goto-current' jumps to the enclosing heading.  Anti-
tautology for the leader binding test."
  (skip-unless (locate-library "org-air"))
  (with-temp-buffer
    (org-mode)
    (insert "#+title: Demo\n* One\nbody one\n** Two\nbody two\n* Three\nbody three\n")
    (let ((heads (org-air-outline--heading-positions)))
      (should (= (length heads) 3))
      (goto-char (point-min))
      (call-interactively #'org-air-outline-next-heading)
      (should (= (point) (nth 0 heads)))
      (call-interactively #'org-air-outline-next-heading)
      (should (= (point) (nth 1 heads)))
      (call-interactively #'org-air-outline-next-heading)
      (should (= (point) (nth 2 heads)))
      (call-interactively #'org-air-outline-prev-heading)
      (should (= (point) (nth 1 heads)))
      ;; `o' (jump to the enclosing heading) from inside the second
      ;; section snaps back to that heading's start.
      (goto-char (+ (nth 1 heads) 4))
      (call-interactively #'org-air-outline-goto-current-heading)
      (should (= (point) (nth 1 heads))))))

(ert-deftest org-air-r30-2-legend-shows-context-key ()
  "The DOC-session rail Actions legend cells read the LEADER form
\(C-c C-a …) for the verbs that self-insert in the doc buffer (jump,
rail), while the board/project rail legend reads BARE keys — both derived
by `org-air-view--legend-key' from the correct buffer.  Trunk FAILED (the
doc legend hardcoded `RET jump' / `| rail', dead from the doc buffer)."
  (skip-unless (locate-library "org-air"))
  (org-air-r28--with-doc-session
    ;; the doc legend, live in the rail, shows the reachable LEADER keys.
    (with-current-buffer org-air-rail-buffer-name
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "C-c C-a o jump" text))
        (should (string-match-p "C-c C-a | rail" text))
        ;; no dead bare `RET jump' cell (trunk hardcoded it; RET newlines
        ;; in the doc buffer).
        (should-not (string-match-p "RET jump" text))))
    ;; the legend-key derivation itself: BARE in the read-only project
    ;; tree (where `|' toggles the rail), LEADER in the editable doc
    ;; buffer (where `|' self-inserts).
    (should (equal (org-air-view--legend-key #'org-air-rail-toggle tree)
                   "|"))
    (should (equal (org-air-view--legend-key #'org-air-rail-toggle docbuf)
                   "C-c C-a |"))))

(ert-deftest org-air-r30-2-bare-keys-still-work ()
  "The leader is ADDITIVE: in the read-only rail the bare `RET' / `|' /
`q' resolve to their commands unchanged, and on the board the bare `|'
still toggles the rail."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-board
    (org-air-r27--pop-rail)
    (with-current-buffer org-air-rail-buffer-name
      (should (eq (key-binding (kbd "RET")) 'org-air-rail-return))
      (should (eq (key-binding (kbd "|")) 'org-air-rail-popin))
      (should (eq (key-binding (kbd "q")) 'org-air-rail-quit)))
    (with-current-buffer (current-buffer)
      (should (eq (key-binding (kbd "|")) 'org-air-rail-toggle)))))

(ert-deftest org-air-r30-2-evil-state-agnostic ()
  "With REAL evil enabled in the doc buffer, `C-c C-a |' resolves to
`org-air-rail-toggle' in BOTH normal and insert state (a `C-c' leader is
left alone by evil in every state)."
  (skip-unless (locate-library "org-air"))
  (skip-unless (locate-library "evil"))
  (require 'evil)
  (org-air-r28--with-doc-session
    (with-current-buffer docbuf
      (evil-local-mode 1)
      (dolist (state '(normal insert))
        (evil-change-state state)
        (should (eq evil-state state))
        (should (eq (key-binding (kbd "C-c C-a |")) 'org-air-rail-toggle))
        (should (eq (key-binding (kbd "C-c C-a q")) 'org-air-project-back)))
      (evil-local-mode -1))))

(ert-deftest org-air-r30-2-leader-key-defcustom ()
  "`org-air-leader-key' is a typed key defcustom; setting it via the
Custom `:set' re-installs the prefix at the NEW key on every registered
host map and unbinds the OLD key (the legend follows via `where-is')."
  (skip-unless (locate-library "org-air"))
  (should (eq (get 'org-air-leader-key 'custom-type) 'key-sequence))
  (let ((orig org-air-leader-key))
    (unwind-protect
        (progn
          (customize-set-variable 'org-air-leader-key "C-c C-y")
          ;; the new key reaches the board leader; the old one is gone.
          (should (eq (lookup-key org-air-view-mode-map (kbd "C-c C-y"))
                      org-air-leader-map))
          (should-not (lookup-key org-air-view-mode-map (kbd "C-c C-a")))
          ;; the doc-session host moved too.
          (should (eq (lookup-key org-air-doc-session-mode-map (kbd "C-c C-y"))
                      org-air-doc-leader-map)))
      (customize-set-variable 'org-air-leader-key orig)
      (should (eq (lookup-key org-air-view-mode-map (kbd "C-c C-a"))
                  org-air-leader-map)))))

;;;; =====================================================================
;;;; R30-3 — dashboard column toggles (defcustom-backed).
;;;; =====================================================================

(ert-deftest org-air-r30-3-default-hides-origin ()
  "With defaults (`org-air-show-origin' nil, `-dates'/`-tags' t) the board
renders NO origin column (`org-air-view--meta-origin-w' is 0) while the
date and tag columns are present.  Trunk FAILED (origin shown)."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-board
    (should (null org-air-show-origin))
    (should org-air-show-dates)
    (should org-air-show-tags)
    (org-air-view--refresh-current)
    (should (= org-air-view--meta-origin-w 0))
    (should (> org-air-view--meta-date-w 0))
    (should (> org-air-view--meta-tags-w 0))))

(ert-deftest org-air-r30-3-toggle-origin-on ()
  "`org-air-toggle-origin' flips `org-air-show-origin' to t and the
re-render shows the origin cell (meta-origin-w > 0); toggling back hides
it again (0)."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-board
    (org-air-view--refresh-current)
    (should (= org-air-view--meta-origin-w 0))
    (call-interactively #'org-air-toggle-origin)
    (should (eq org-air-show-origin t))
    (should (> org-air-view--meta-origin-w 0))
    (call-interactively #'org-air-toggle-origin)
    (should (null org-air-show-origin))
    (should (= org-air-view--meta-origin-w 0))))

(ert-deftest org-air-r30-3-toggle-dates-tags-off ()
  "Hiding dates zeroes the date width AND the repeat reserve; hiding tags
zeroes the tag width; the flex title reclaims the freed width and no
composed line overflows the window (V6 alignment holds)."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-board
    (org-air-view--refresh-current)
    (should (> org-air-view--meta-date-w 0))
    (should (> org-air-view--meta-tags-w 0))
    (setq-local org-air-show-dates nil
                org-air-show-tags nil)
    (org-air-view--refresh-current)
    (should (= org-air-view--meta-date-w 0))
    (should (= org-air-view--meta-date-repeat 0))
    (should (= org-air-view--meta-tags-w 0))
    ;; every composed line still fits the window (no overflow).
    (org-air-r29--assert-lines-fit (get-buffer-window (current-buffer)))))

(ert-deftest org-air-r30-3-hidden-data-still-queryable ()
  "The toggles are DISPLAY-only: with the tag column HIDDEN,
`org-air-filter' by a tag still narrows the board (the filter reads
`org-air-item-tags' from the struct, not the rendered cell)."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-board
    (setq-local org-air-show-tags nil)
    (org-air-view--refresh-current)
    (should (= org-air-view--meta-tags-w 0))
    (let* ((all org-air-view--items)
           (total (length all))
           (counts (make-hash-table :test 'equal))
           tag)
      (dolist (it all)
        (dolist (tg (org-air-item-tags it))
          (puthash tg (1+ (gethash tg counts 0)) counts)))
      ;; a tag SOME but not ALL items carry (so the filter really narrows).
      (maphash (lambda (tg c) (when (and (null tag) (> c 0) (< c total))
                                (setq tag tg)))
               counts)
      (should tag)
      (setq-local org-air-view--tag-filter (list tag))
      (let* ((org-air-view--render-partition nil)
             (narrowed (org-air-view--visible-items all)))
        (should (< (length narrowed) total))
        (should (> (length narrowed) 0))
        ;; every surviving item really carries the hidden-column tag.
        (dolist (it narrowed)
          (should (member tag (org-air-item-tags it))))))))

(ert-deftest org-air-r30-3-defcustoms-typed ()
  "The three column toggles are boolean defcustoms in group `org-air' with
the stated defaults (origin nil, dates t, tags t)."
  (skip-unless (locate-library "org-air"))
  (dolist (spec '((org-air-show-origin . nil)
                  (org-air-show-dates . t)
                  (org-air-show-tags . t)))
    (let ((sym (car spec)))
      (should (eq (get sym 'custom-type) 'boolean))
      (should (custom-variable-p sym))
      (should (eq (default-value sym) (cdr spec))))))

;;;; =====================================================================
;;;; R30-4 — org-air-outline-mode: generic opt-in outline rail.
;;;; =====================================================================

(defmacro org-air-r30--with-outline-buffer (&rest body)
  "Show a plain (non-Air) Org buffer with THREE headings in a live window.
BODY runs in that buffer (its window selected), with `noninteractive'
nil so the rail really pops and the aux buffers reset around it."
  (declare (indent 0) (debug t))
  `(save-window-excursion
     (org-air-r27--kill-aux-buffers)
     (org-air-r27--reset-rail-globals)
     (let ((noninteractive nil)
           (org-air-rail-min-width 40)
           (buf (get-buffer-create "*org-air-r30-outline*")))
       (unwind-protect
           (with-current-buffer buf
             (let ((inhibit-read-only t)) (erase-buffer))
             (org-mode)
             (insert "#+title: Plain Notes\n"
                     "* Alpha\nalpha body\n"
                     "** Beta\nbeta body\n"
                     "* Gamma\ngamma body\n")
             (goto-char (point-min))
             (switch-to-buffer buf)
             (delete-other-windows)
             ,@body)
         (with-current-buffer buf
           (when (bound-and-true-p org-air-outline-mode)
             (org-air-outline-mode -1)))
         (org-air-r27--reset-rail-globals)
         (org-air-r27--kill-aux-buffers)
         (when (buffer-live-p buf)
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf))))))

(ert-deftest org-air-r30-4-headings-primitive-generic ()
  "`org-air-outline--headings' over a plain (non-Air) Org buffer returns
the (LEVEL TITLE POS) rows; the project's `org-air-project--doc-outline'
is byte-identical (the alias holds)."
  (skip-unless (locate-library "org-air"))
  (with-temp-buffer
    (org-mode)
    (insert "* One\n** Two\n* Three\n")
    (let ((rows (org-air-outline--headings (current-buffer))))
      (should (equal (mapcar (lambda (r) (list (nth 0 r) (nth 1 r)))
                             rows)
                     '((1 "One") (2 "Two") (1 "Three"))))
      ;; every row carries a real buffer position.
      (dolist (r rows) (should (integerp (nth 2 r))))
      ;; the project alias is byte-identical.
      (should (equal rows
                     (org-air-project--doc-outline (current-buffer)))))))

(ert-deftest org-air-r30-4-outline-mode-pops-rail ()
  "Enabling `org-air-outline-mode' in a plain Org buffer pops the rail and
the rail shows the buffer's headings (one row per heading, carrying
`org-air-doc-heading-pos'); disabling tears it down (no timer, no
overlay, rail hidden)."
  (skip-unless (locate-library "org-air"))
  (org-air-r30--with-outline-buffer
    (org-air-outline-mode 1)
    (should org-air-outline-mode)
    (let ((rail (get-buffer org-air-rail-buffer-name)))
      (should (buffer-live-p rail))
      (should (window-live-p (get-buffer-window rail)))
      (with-current-buffer rail
        (let ((text (substring-no-properties (buffer-string)))
              (heads 0))
          (should (string-match-p "Alpha" text))
          (should (string-match-p "Beta" text))
          (should (string-match-p "Gamma" text))
          ;; three rows carry the heading-pos property.
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (when (get-text-property (point) 'org-air-doc-heading-pos)
                (cl-incf heads))
              (forward-line 1)))
          (should (= heads 3)))))
    ;; disable: teardown.
    (org-air-outline-mode -1)
    (should-not org-air-outline-mode)
    (should (null org-air-outline--timer))
    ;; the overlay is cleared (deleted -> detached from any buffer); the
    ;; object may linger in the global slot but paints nothing.
    (should-not (and (overlayp org-air-rail--outline-overlay)
                     (overlay-buffer org-air-rail--outline-overlay)))
    (let ((rail (get-buffer org-air-rail-buffer-name)))
      (should-not (and rail (window-live-p (get-buffer-window rail)))))))

(ert-deftest org-air-r30-4-highlight-follows-point ()
  "Moving point past a heading in the plain Org buffer moves the single
overlay onto the corresponding rail row (the R28-4 core, Air-free);
degrades to no-highlight (never signals) when there is no rail."
  (skip-unless (locate-library "org-air"))
  (org-air-r30--with-outline-buffer
    (org-air-outline-mode 1)
    (let ((rail (get-buffer org-air-rail-buffer-name))
          (heads (org-air-outline--headings (current-buffer))))
      ;; point inside the Beta section -> overlay on the Beta rail row.
      (goto-char (+ (nth 2 (nth 1 heads)) 3))
      (org-air-outline--highlight-update (current-buffer) rail)
      (should (overlayp org-air-rail--outline-overlay))
      (with-current-buffer rail
        (let ((row (buffer-substring-no-properties
                    (overlay-start org-air-rail--outline-overlay)
                    (overlay-end org-air-rail--outline-overlay))))
          (should (string-match-p "Beta" row))))
      ;; point before the first heading -> no current row (overlay cleared
      ;; to a zero-length / detached state, never an error).
      (goto-char (point-min))
      (org-air-outline--highlight-update (current-buffer) rail)
      ;; a dead rail buffer degrades silently (no signal).
      (should-not (org-air-outline--highlight-update
                   (current-buffer) (generate-new-buffer " *dead*"))))))

(ert-deftest org-air-r30-4-off-by-default ()
  "The mode is nil unless enabled; a plain Org buffer has no rail and no
overlay until `org-air-outline-mode' is turned on."
  (skip-unless (locate-library "org-air"))
  (with-temp-buffer
    (org-mode)
    (should-not (bound-and-true-p org-air-outline-mode))))

(ert-deftest org-air-r30-4-no-air-dependency ()
  "`org-air-outline-mode' loads + functions with `org-air-project' NOT
required: in a FRESH batch Emacs only `org-air-view' is loaded, the mode
enables in a plain Org buffer, builds the outline and follows point — all
without `org-air-project' present in `features'."
  (skip-unless (locate-library "org-air"))
  (let* ((root (locate-dominating-file org-air-test-fixture-dir "Makefile"))
         (init (expand-file-name "tests/org-air-test-init.el" root))
         (script
          (prin1-to-string
           '(progn
              (require 'org-air-view)
              (when (featurep 'org-air-project) (kill-emacs 2))
              (unless (fboundp 'org-air-outline-mode) (kill-emacs 3))
              (unless (fboundp 'org-air-outline--headings) (kill-emacs 4))
              (with-temp-buffer
                (org-mode)
                (insert "* A\n** B\n* C\n")
                (let ((rows (org-air-outline--headings (current-buffer))))
                  (unless (= (length rows) 3) (kill-emacs 5)))
                ;; enabling must not require org-air-project.
                (org-air-outline-mode 1)
                (when (featurep 'org-air-project) (kill-emacs 6))
                (org-air-outline-mode -1))
              (kill-emacs 0)))))
    (should root)
    (with-temp-buffer
      (let ((status (call-process
                     (or (getenv "EMACS") "emacs") nil t nil
                     "-Q" "--batch" "-l" init "--eval" script)))
        (unless (eql status 0)
          (ert-fail (format "no-air-dep subprocess exited %s: %s"
                            status (buffer-string))))))))

;;;; =====================================================================
;;;; R30-5 — close the R29 coverage gap: doc-rail-show is now revert-guarded.
;;;; =====================================================================

(ert-deftest org-air-r30-5-doc-rail-fits-fringeless ()
  "Doc session, rail popped, FRINGE-LESS GUI sim (R29-1 harness): every
`*org-air-rail*' line fits the doc window's usable columns, AND the host
width `org-air-project--doc-rail-show' resolves equals
`org-air-layout--usable-columns' of the doc window — NOT raw
`window-body-width'.  Reverting the site to `window-body-width' makes the
width assertion FAIL (the site is now revert-guarded).  Byte-invisible;
pure test addition."
  (skip-unless (locate-library "org-air"))
  (org-air-r28--with-doc-session
    (org-air-r29--with-fringeless-gui
      (let* ((win (get-buffer-window docbuf))
             (usable (org-air-layout--usable-columns win))
             (body (window-body-width win))
             (real-show (symbol-function 'org-air-rail--show))
             (captured nil))
        (should (window-live-p win))
        ;; the fringe-less sim really makes usable one short of body
        ;; (else the guard would be vacuous).
        (should (= usable (1- body)))
        ;; capture the host width the doc-rail-show passes to rail--show.
        (cl-letf (((symbol-function 'org-air-rail--show)
                   (lambda (buf width)
                     (when (eq buf docbuf) (setq captured width))
                     (funcall real-show buf width))))
          (org-air-project--doc-rail-show docbuf))
        ;; the resolved host width is the USABLE columns (R29-1), not body.
        (should (eql captured (max 40 usable)))
        (should-not (eql captured body))
        ;; every rail line fits the doc window's usable columns.
        (let ((rail (get-buffer org-air-rail-buffer-name)))
          (should (buffer-live-p rail))
          (with-current-buffer rail
            (save-excursion
              (goto-char (point-min))
              (while (not (eobp))
                (should (<= (string-width
                             (buffer-substring-no-properties
                              (line-beginning-position) (line-end-position)))
                            usable))
                (forward-line 1)))))))))

(provide 'org-air-round30-test)
;;; org-air-round30-test.el ends here
