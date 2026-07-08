;;; org-air-round39-test.el --- R39 acceptance ERTs -*- lexical-binding: t; -*-

;;; Commentary:
;; Round-39 acceptance tests (air/v0.5/org-air-round39-design.org).  All four
;; items are EXECUTING and deterministic in batch.
;;
;; R39-1  HEADER LHS/RHS MARGIN SYMMETRY.  The banner reserves a right gutter
;;        equal to the left indent (`org-air-view--banner-indent') so the
;;        header is left/right symmetric — the right status ends banner-indent
;;        columns before the last usable column, mirroring the left
;;        `  org-air' indent.  Guard: LHS indent width == RHS gutter width ==
;;        banner-indent; the header still fits usable; reverting to the R36
;;        flush-right composition (status flush on the last usable column)
;;        yields an ASYMMETRIC header (LHS 2, RHS 0) — the guard rejects it.
;;
;; R39-2  NO-RAIL FENCE / METADATA-CLUSTER REALIGNMENT.  The no-rail fence
;;        column and every standard row's metadata cluster right-anchor are
;;        BOTH derived from the SAME live width via `org-air-view--fence-
;;        column', so they can never desync (the R37 usable / R38-2 inspector
;;        seams).  Guard: `(org-air-view--fence-column width)' equals the
;;        actual column a rendered board-only row's metadata cluster
;;        right-anchors to; a fence computed from a DIFFERENT width (the
;;        drift) lands off the cluster.
;;
;; R39-3  DROP `C-c C-a o' IN THE DOC BUFFER.  The leader `o' (outline jump)
;;        duplicated RET in the editable doc session, so it is removed from
;;        `org-air-doc-leader-map' ONLY; the board / project leaders keep
;;        their `o' (rail-return).  Guard: `o' is unbound in the doc leader
;;        (driving `C-c C-a o' does not jump), while the board leader `o'
;;        still resolves.
;;
;; R39-4  REPEATABLE LEADER p/n.  After the leader prefix, prev/next install
;;        a shared transient map (`org-air--repeat-pn-map' via
;;        `set-transient-map') so a bare `n'/`p' repeats the motion until any
;;        other key.  Guard: driving leader-n then three bare `n' advances the
;;        cursor 4 rows; reverting the transient-map tail (no re-arm) leaves a
;;        bare `n' self-inserting, so only the single leader motion lands (1
;;        row).  The same shape for `p'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-air)
(require 'org-air-project)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)

;;;; =====================================================================
;;;; R39-1 — header LHS/RHS margin symmetry.
;;;; =====================================================================

(defun org-air-r39--compose-banner (width &optional loading)
  "Return the ACTUAL composed banner line at usable WIDTH (no properties),
via the real `org-air-view--insert-banner' choke point (LOADING selects
the skeleton flavour)."
  (with-temp-buffer
    (let ((org-air-view--line-width width)
          (org-air-view--loading loading))
      (org-air-view--insert-banner nil)
      (goto-char (point-min))
      (buffer-substring-no-properties (line-beginning-position)
                                      (line-end-position)))))

(defun org-air-r39--banner-margins (line width)
  "Return (LHS . RHS) margin widths of banner LINE at usable WIDTH.
LHS is the count of leading spaces (the left indent baked into the token);
RHS is the trailing gutter WIDTH minus the last content column, i.e. how
many usable columns sit to the RIGHT of the status' last glyph."
  (let* ((lhs (- (length line) (length (string-trim-left line))))
         (rhs (- width (string-width (string-trim-right line)))))
    (cons lhs rhs)))

(ert-deftest org-air-r39-1-banner-margins-symmetric ()
  "R39-1: the banner is left/right symmetric — its LEFT indent width equals
its RIGHT gutter width, both equal to `org-air-view--banner-indent', at
several usable widths (odd + even), for BOTH the board and loading-skeleton
flavours.  The header still fits the usable width."
  (skip-unless (locate-library "org-air"))
  (should (boundp 'org-air-view--banner-indent))
  (should (> org-air-view--banner-indent 0))
  (dolist (width '(63 80 96 100 119 120 160 191))
    (dolist (loading '(nil t))
      (let* ((line (org-air-r39--compose-banner width loading))
             (m (org-air-r39--banner-margins line width)))
        (ert-info ((format "width %d loading %s line=%S margins=%S"
                           width loading line m))
          ;; the header fits the usable width.
          (should (<= (string-width line) width))
          ;; LHS indent == banner-indent (the `  org-air' left margin).
          (should (= (car m) org-air-view--banner-indent))
          ;; RHS gutter == banner-indent (the symmetric right margin).
          (should (= (cdr m) org-air-view--banner-indent))
          ;; and they are equal to each other (the reported symmetry).
          (should (= (car m) (cdr m))))))))

(ert-deftest org-air-r39-1-reverting-flush-right-is-asymmetric ()
  "NON-TAUTOLOGY / REVERT GUARD.  Reconstruct the pre-R39 flush-right
composition (`org-air-view--justify' the status to the FULL usable width,
so its last glyph sits ON the last usable column) and show it is
ASYMMETRIC: LHS 2, RHS 0.  The R39-1 symmetric-margin guard rejects it,
so reverting the fix (dropping the trailing gutter) FAILS the guard."
  (skip-unless (locate-library "org-air"))
  (dolist (width '(80 120 191))
    (let* ((fixed (org-air-r39--compose-banner width nil))
           (left "  org-air")
           ;; recover the composed status from the fixed line.
           (status (string-trim (substring fixed (length left))))
           ;; re-compose the OLD flush-right banner (justify to full width).
           (reverted (org-air-view--justify left status width))
           (fm (org-air-r39--banner-margins fixed width))
           (rm (org-air-r39--banner-margins reverted width)))
      (ert-info ((format "width %d fixed=%S reverted=%S fm=%S rm=%S"
                         width fixed reverted fm rm))
        ;; the FIXED banner is symmetric (sanity, same as the main guard).
        (should (= (car fm) (cdr fm) org-air-view--banner-indent))
        ;; the REVERTED (flush-right) banner has the left indent but NO
        ;; right gutter — its last glyph is flush on the usable edge.
        (should (= (car rm) org-air-view--banner-indent))
        (should (= (cdr rm) 0))
        ;; …so it is ASYMMETRIC (the R39-1 invariant LHS == RHS is broken).
        (should-not (= (car rm) (cdr rm)))))))

;;;; =====================================================================
;;;; R39-2 — no-rail fence column == metadata cluster right-anchor.
;;;; =====================================================================

(defun org-air-r39--row-cluster-anchor ()
  "Return the screen column a standard board row's metadata cluster
right-anchors to, measured from the live buffer, or nil.
Every dated row's DATE cell is the FIRST (left-most) cell of the cluster,
so its start column IS the cluster's right-anchor column.  All standard
rows share ONE such column (V6); this returns that shared column, or nil
if no dated row is found."
  (let (cols)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (when (string-match " \\(OVERDUE [0-9]+d\\|Today\\|Tomorrow\\)" line)
            (push (match-beginning 1) cols)))
        (forward-line 1)))
    (setq cols (delete-dups (nreverse cols)))
    (and cols (= (length cols) 1) (car cols))))

(ert-deftest org-air-r39-2-fence-column-equals-cluster-column ()
  "R39-2: render the board headless in NO-RAIL (board-only) mode; the
`org-air-view--fence-column' of the live width equals the actual column
every standard row's metadata cluster right-anchors to.  Both derive from
the ONE live width so they cannot desync; a fence computed from a
DIFFERENT width (the R37/R38-2 drift) lands OFF the cluster (revert-fail)."
  (skip-unless (locate-library "org-air"))
  (should (fboundp 'org-air-view--fence-column))
  (should (fboundp 'org-air-view--meta-cluster-width))
  (org-air-viewport-test-with-dashboard 70
    ;; below `org-air-rail-min-width' -> board-only, the no-rail path.
    (should (eq org-air-view--orientation 'board-only))
    (let* ((width (org-air-view--render-width))
           (fence (org-air-view--fence-column width))
           (anchor (org-air-r39--row-cluster-anchor)))
      (ert-info ((format "width=%d fence=%d anchor=%S cluster-w=%d"
                         width fence anchor (org-air-view--meta-cluster-width)))
        ;; a real anti-tautology: a genuine dated row column was measured.
        (should (integerp anchor))
        ;; the pure derivation: fence == width - meta-cluster-width.
        (should (= fence (- width (org-air-view--meta-cluster-width))))
        ;; THE R39-2 CONTRACT: the fence column IS the cluster anchor.
        (should (= fence anchor))
        ;; REVERT GUARD: a fence read from a DIFFERENT width (the seam
        ;; drift) lands OFF the cluster's live-width anchor.
        (should-not (= (org-air-view--fence-column (1- width)) anchor))
        (should-not (= (org-air-view--fence-column (1+ width)) anchor))))))

;;;; =====================================================================
;;;; R39-3 — `C-c C-a o' dropped from the doc leader ONLY.
;;;; =====================================================================

(defmacro org-air-r39--with-outline-doc (&rest body)
  "Run BODY in a temp org buffer with SIX headings and the doc-session
mode + default leader active.  Binds `heads' to the heading positions and
`hlines' to their line numbers (computed BEFORE any BODY mutation)."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (org-mode)
     (insert "#+title: Demo\npreamble\n"
             "* H1\nbody one\n* H2\nbody two\n* H3\nbody three\n"
             "* H4\nbody four\n* H5\nbody five\n* H6\nbody six\n")
     (let ((org-air-use-default-keybindings t))
       (org-air-doc-session-mode 1)
       (let* ((heads (org-air-outline--heading-positions))
              (hlines (mapcar #'line-number-at-pos heads))
              ;; keep the transient map local to this run (it is a GLOBAL
              ;; variable; bind it so an armed map never leaks across tests).
              (overriding-terminal-local-map nil))
         (ignore heads hlines overriding-terminal-local-map)
         ,@body))))

(ert-deftest org-air-r39-3-doc-leader-no-open ()
  "R39-3: the leader `o' (outline jump) is DROPPED from
`org-air-doc-leader-map' ONLY.  In the doc-session org buffer with the
defaults installed, `C-c C-a o' is NOT the open/jump command (it falls
through to nil); the board / project leaders keep their `o' (rail-return).
Driving `C-c C-a o' from inside a section does NOT jump to the heading."
  (skip-unless (locate-library "org-air"))
  (should (boundp 'org-air-doc-leader-map))
  ;; the doc leader dropped `o'; the board / project leaders keep it.
  (should (null (lookup-key org-air-doc-leader-map "o")))
  (should (eq (lookup-key org-air-leader-map "o") 'org-air-rail-return))
  (should (eq (lookup-key org-air-project-leader-map "o") 'org-air-rail-return))
  ;; the jump command still EXISTS as a primitive; it is simply no longer
  ;; reachable from the doc leader.
  (should (fboundp 'org-air-outline-goto-current-heading))
  (org-air-r39--with-outline-doc
    ;; live in the doc buffer: `C-c C-a o' resolves to NOTHING (dropped),
    ;; while a kept leader verb (`|' rail) still resolves.
    (should-not (eq (key-binding (kbd "C-c C-a o"))
                    'org-air-outline-goto-current-heading))
    (should (null (key-binding (kbd "C-c C-a o"))))
    (should (eq (key-binding (kbd "C-c C-a |")) 'org-air-rail-toggle))
    ;; driving the (now dead) leader `o' from mid-section does NOT jump the
    ;; cursor back to the enclosing heading — the old action is gone.
    (goto-char (+ (nth 2 heads) 5))       ; inside H3's body
    (let ((before (point))
          (cmd (key-binding (kbd "C-c C-a o"))))
      (when (commandp cmd)
        (setq this-command cmd)
        (call-interactively cmd))
      (should (= (point) before)))))

;;;; =====================================================================
;;;; R39-4 — repeatable leader p/n via the shared transient map.
;;;; =====================================================================

(defun org-air-r39--drive-keys (keys)
  "Drive KEYS (a list of `kbd' STRINGS) through the LIVE active keymaps,
faithfully reproducing the command loop: each key is resolved via
`key-binding' (which honours the armed `set-transient-map'
`overriding-terminal-local-map') and run with `call-interactively', with
`last-command-event' / `this-command' / `last-command' threaded so a
self-insert or a repeat wrapper behaves exactly as under the real loop.
\(`execute-kbd-macro' mis-dispatches a `C-c'-prefix + transient-map
sequence under `--batch', so we simulate the loop directly.)"
  (dolist (k keys)
    (let* ((seq (kbd k))
           (cmd (key-binding seq)))
      (should (commandp cmd))
      (setq last-command-event (aref seq (1- (length seq)))
            this-command cmd)
      (call-interactively cmd)
      (setq last-command cmd))))

(defun org-air-r39--point-heading-index (hlines)
  "Return the index into HLINES of the heading on point's current line.
Robust to the reverted case's self-inserts (they add no newlines, so the
heading LINE numbers computed before the drive are stable)."
  (cl-position (line-number-at-pos (point)) hlines))

(ert-deftest org-air-r39-4-pn-repeatable ()
  "R39-4: leader-n then three bare `n' advances the cursor exactly 4
headings (the shared p/n transient map re-arms after every motion); the
same shape for `p' walks back 4.  REVERT GUARD: with the transient-map
tail neutralised (`org-air--repeat-pn-arm' a no-op, i.e. no
`set-transient-map'), the bare `n'/`p' self-inserts and only the single
leader motion lands — 1 heading, not 4."
  (skip-unless (locate-library "org-air"))
  (should (fboundp 'org-air--repeat-next))
  (should (fboundp 'org-air--repeat-prev))
  (should (keymapp org-air--repeat-pn-map))
  ;; the leader n/p route through the repeatable wrappers.
  (should (eq (lookup-key org-air-doc-leader-map "n") 'org-air--repeat-next))
  (should (eq (lookup-key org-air-doc-leader-map "p") 'org-air--repeat-prev))

  ;; --- NEXT: leader-n + n n n advances 4 (start on H1 -> H5). ---
  (org-air-r39--with-outline-doc
    (goto-char (nth 0 heads))
    (org-air-r39--drive-keys '("C-c C-a n" "n" "n" "n"))
    (ert-info ((format "point line %d hlines %S" (line-number-at-pos (point)) hlines))
      (should (equal (org-air-r39--point-heading-index hlines) 4))))
  ;; REVERT: no transient map -> bare n self-inserts -> only 1 motion.
  (org-air-r39--with-outline-doc
    (goto-char (nth 0 heads))
    (cl-letf (((symbol-function 'org-air--repeat-pn-arm) (lambda () nil)))
      (org-air-r39--drive-keys '("C-c C-a n" "n" "n" "n")))
    (should (equal (org-air-r39--point-heading-index hlines) 1)))

  ;; --- PREV: leader-p + p p p walks back 4 (start on H6 -> H2). ---
  (org-air-r39--with-outline-doc
    (goto-char (nth 5 heads))
    (org-air-r39--drive-keys '("C-c C-a p" "p" "p" "p"))
    (should (equal (org-air-r39--point-heading-index hlines) 1)))
  ;; REVERT: no transient map -> bare p self-inserts -> only 1 motion.
  (org-air-r39--with-outline-doc
    (goto-char (nth 5 heads))
    (cl-letf (((symbol-function 'org-air--repeat-pn-arm) (lambda () nil)))
      (org-air-r39--drive-keys '("C-c C-a p" "p" "p" "p")))
    (should (equal (org-air-r39--point-heading-index hlines) 4))))

(provide 'org-air-round39-test)
;;; org-air-round39-test.el ends here
