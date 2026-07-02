;;; org-air-round21-test.el --- R21 test-seat substantive ERTs -*- lexical-binding: t; -*-

;;; Commentary:
;; Test-seat substantive ERTs for v0.5 round-21
;; (air/v0.5/org-air-round21-design.org).  These are the BEHAVIOUR guards
;; the byte-golden re-bless (project-view-*.txt, regenerated for the R21-5
;; one-line rows) cannot express — point stability, keyword recognition,
;; the shared-primitive project row, title landing, the svg-badge text
;; fallback, and the mode-line / pane-header contrast.  Mapped to the
;; prompt's punch list:
;;
;;   (a) R21-1  a same-item re-render PRESERVES the point COLUMN (not just
;;              the line) — the cursor-jump bug the side-window rail
;;              exposed; plus the clamp invariant and the resize gate that
;;              keeps plain motion from re-rendering at all.
;;   (b) R21-3  NEXT/WAIT are recognised as keyword cells (title clean)
;;              even with NO `#+TODO:' line, BUT a file's own `#+TODO:'
;;              stays authoritative.
;;   (c) R21-5  each Air doc renders as ONE board-style row through the
;;              SHARED `org-air-view--insert-row' (V6 column alignment,
;;              one line per doc, the whole row identifies the doc).
;;   (d) R21-2  motion / first-open land point on the TITLE, past the
;;              keyword / priority / state cell.
;;   (e) R21-4  the shared svg keyword/state badge is overlay-only with a
;;              mandatory TEXT fallback — the byte/TTY layer is intact.
;;   (f) R21-6  the mode-line and the pane header-line faces clear WCAG AA
;;              (no too-dim mode-line, no blue-on-blue title) in BOTH the
;;              light and dark specs.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)
(require 'org-air-project-test)            ; project fixture root + render helpers
(require 'org-air)

;;;; ---------------------------------------------------------------------
;;;; (a) R21-1 — a re-render keeps point on the same COLUMN, not col 0.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r21-1-render-current-preserves-point-column ()
  "R21-1: the re-render every live event routes through
\(`org-air-view--render-current') keeps point on the SAME column the user
was on — it no longer snaps to the row's leftmost glyph.  This is the
\"cursor jumps to column 0\" bug the side-window rail exposed on nearly
every motion; the fix is in `org-air-view--restore-position' /
`--restore-to-column', so it holds for ANY re-render regardless of the
rail orientation.  Anti-tautology: the saved column is deep inside the
title (not 0), and the assertion is on the COLUMN, not just the line."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
   (org-air-viewport-test-with-dashboard 120
     ;; Land on the first item's title, then move several columns INTO it so
     ;; the saved column is well past the row's first-visible glyph.
     (org-air-view--goto-first-item)
     (org-air-view--goto-row-title)
     (forward-char 4)
     (let ((col (current-column))
           (marker (get-text-property (point) 'org-air-marker)))
       (should marker)
       (should (> col 0))
       (org-air-view--render-current)
       ;; SAME item survived ...
       (should (equal (get-text-property (point) 'org-air-marker) marker))
       ;; ... and the COLUMN is preserved (the cursor stayed put).
       (should (= (current-column) col))))))

(ert-deftest org-air-r21-1-restore-to-column-clamps ()
  "R21-1: `org-air-view--restore-to-column' clamps both ways — it never
lands before the row's first visible glyph (so the cursor reads on a real
character, S5a) and never past end-of-line, even when the saved column
exceeds a now-shorter row's width (clamps onto the row, no error)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
   (org-air-viewport-test-with-dashboard 120
     (org-air-view--goto-first-item)
     (let ((first (save-excursion (org-air-view--beginning-of-visible)
                                  (current-column)))
           (eol (save-excursion (end-of-line) (current-column))))
       ;; floor: a saved column of 0 lands on the first VISIBLE glyph, never
       ;; on the leading indent whitespace.
       (org-air-view--restore-to-column '(:column 0))
       (should (= (current-column) first))
       (should-not (memq (char-after) '(?\s ?\t)))
       ;; ceil: an absurd saved column clamps onto the row, no error.
       (org-air-view--restore-to-column '(:column 9999))
       (should (>= (current-column) first))
       (should (<= (current-column) eol))))))

(ert-deftest org-air-r21-1-resize-gate-blocks-rerender-when-stable ()
  "R21-1 secondary: `org-air-view--resize-refresh' re-renders ONLY on a
real width/height change.  With stable dimensions (plain motion, a 1-col
side-window hscroll wobble) it is a NO-OP — so ordinary motion never
triggers the re-render, and thus never the column-loss path.  Proven by
trapping `org-air-view--render-current'."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
   (org-air-viewport-test-with-dashboard 120
     (let ((calls 0))
       (cl-letf (((symbol-function 'org-air-view--render-current)
                  (lambda (&rest _) (cl-incf calls))))
         ;; dims unchanged since the last render -> gate keeps it a no-op.
         (org-air-view--resize-refresh)
         (should (= calls 0))
         ;; a real width change -> the gate lets the re-render through.
         (setq org-air-view--rendered-width
               (1- (or org-air-view--rendered-width 80)))
         (org-air-view--resize-refresh)
         (should (= calls 1)))))))

(ert-deftest org-air-r21-1-rail-width-hysteresis-defcustom ()
  "R21-1: the orientation hysteresis band exists as a user option, so a
1-col redisplay wobble at the board-only threshold no longer flickers the
rail in and out (each flip was a real dimension change → a re-render)."
  (skip-unless (locate-library "org-air"))
  (should (boundp 'org-air-rail-width-hysteresis))
  (should (custom-variable-p 'org-air-rail-width-hysteresis))
  (should (integerp org-air-rail-width-hysteresis))
  (should (> org-air-rail-width-hysteresis 0)))

;;;; ---------------------------------------------------------------------
;;;; (b) R21-3 — NEXT/WAIT recognised w/o #+TODO; the file's own wins.
;;;; ---------------------------------------------------------------------

(defmacro org-air-r21-3--with-files (alist &rest body)
  "Write ALIST of (NAME . CONTENT) Org files into a temp dir for BODY.
Binds `org-air-files' to the written paths so `org-air-query-items' scans
them; kills the visiting buffers and removes the dir afterwards."
  (declare (indent 1) (debug t))
  `(let ((org-air-r21-3--dir (make-temp-file "org-air-r21-3-" t)))
     (unwind-protect
         (let ((org-air-files
                (mapcar (lambda (pair)
                          (let ((f (expand-file-name (car pair) org-air-r21-3--dir)))
                            (with-temp-file f (insert (cdr pair)))
                            f))
                        ,alist)))
           (org-air-viewport-test--with-frozen-now ,@body))
       (let ((kill-buffer-query-functions nil))
         (dolist (buf (buffer-list))
           (let ((fn (buffer-file-name buf)))
             (when (and fn (string-prefix-p org-air-r21-3--dir fn))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r21-3--dir t))))

(ert-deftest org-air-r21-3-recognises-next-wait-without-todo-header ()
  "R21-3: NEXT/WAIT are recognised as keyword cells — NOT swallowed into
the title — in a file with NO `#+TODO:' line, because the org-ql scan
let-binds org-air's vocabulary (`org-air-todo-keywords').  Plain TODO is
unaffected."
  (skip-unless (locate-library "org-air"))
  (org-air-r21-3--with-files
      '(("a.org" . "* NEXT Ship it\n* WAIT On review\n* TODO Plain task\n"))
    (let* ((items (org-air-query-items))
           (next (org-air-test-find-item "Ship it" items))
           (wait (org-air-test-find-item "On review" items))
           (todo (org-air-test-find-item "Plain task" items)))
      (should next) (should wait) (should todo)
      ;; keyword recognised (the keyword cell is populated) ...
      (should (equal (org-air-item-todo next) "NEXT"))
      (should (equal (org-air-item-todo wait) "WAIT"))
      (should (equal (org-air-item-todo todo) "TODO"))
      ;; ... and the title is CLEAN (the keyword is stripped off, not text).
      (should (equal (org-air-item-title next) "Ship it"))
      (should (equal (org-air-item-title wait) "On review"))
      (should (equal (org-air-item-title todo) "Plain task")))))

(ert-deftest org-air-r21-3-file-own-todo-still-wins ()
  "R21-3: a file's OWN `#+TODO:' is authoritative — org-air's fallback
vocabulary does NOT leak into a file that declares its own keywords.  A
file with `#+TODO: TODO PROG | DONE' recognises PROG (its own keyword)
but NOT NEXT (which it never declared), so `NEXT' stays in the title."
  (skip-unless (locate-library "org-air"))
  (org-air-r21-3--with-files
      '(("b.org" . "#+TODO: TODO PROG | DONE\n* PROG Building\n* NEXT Not a keyword here\n"))
    (let* ((items (org-air-query-items))
           (prog (org-air-test-find-item "Building" items))
           (next (org-air-test-find-item "Not a keyword here" items)))
      ;; the file's OWN keyword wins ...
      (should prog)
      (should (equal (org-air-item-todo prog) "PROG"))
      ;; ... and NEXT is NOT force-recognised — the file's #+TODO governs.
      (should next)
      (should (null (org-air-item-todo next)))
      (should (equal (org-air-item-title next) "NEXT Not a keyword here")))))

(ert-deftest org-air-r21-3-done-vocabulary-recognised ()
  "R21-3: the recognised :done vocabulary (CANCELLED/KILL/…) is treated as
done, so such headings drop off the board (no buckets) even without a
file `#+TODO:' line — the recognition fix keeps classify correct."
  (skip-unless (locate-library "org-air"))
  (org-air-r21-3--with-files
      '(("c.org" . "* CANCELLED Old idea\n* TODO Live task\n"))
    (let* ((items (org-air-query-items))
           (cancelled (org-air-test-find-item "Old idea" items)))
      (should cancelled)
      (should (equal (org-air-item-todo cancelled) "CANCELLED"))
      ;; recognised as DONE -> not actionable -> classified into no buckets.
      (should (null (org-air-classify-item cancelled org-air-test-now))))))

;;;; ---------------------------------------------------------------------
;;;; (c) R21-5 — project docs are ONE-LINE rows via the SHARED primitive.
;;;; ---------------------------------------------------------------------

(defun org-air-r21-5--doc-line-stats ()
  "In the current project buffer, return (LINES-WITH-DOC . DISTINCT-DOCS).
LINES-WITH-DOC is the number of buffer lines carrying an `org-air-doc'
property anywhere; DISTINCT-DOCS counts the distinct doc objects seen."
  (let ((docs '()) (lines-with-doc 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((found nil) (p (line-beginning-position)) (eol (line-end-position)))
          (while (and p (< p eol))
            (let ((d (get-text-property p 'org-air-doc)))
              (when d (setq found t) (unless (memq d docs) (push d docs))))
            (setq p (next-single-property-change p 'org-air-doc nil eol)))
          (when found (cl-incf lines-with-doc)))
        (forward-line 1)))
    (cons lines-with-doc (length docs))))

(ert-deftest org-air-r21-5-one-line-per-doc-no-two-line-residue ()
  "R21-5: each Air doc renders as exactly ONE line — the old two-line
`<state> Title' / `. relpath created…/updated…' block is gone.  The number
of buffer lines carrying `org-air-doc' equals the number of distinct docs
rendered (one line per doc), proving no second `. relpath …' line remains."
  (skip-unless (locate-library "org-air"))
  (let ((org-air-project-view-width 100))
    (org-air-project-test--render
     (when (commandp 'org-air-project-group-by-state)
       (call-interactively 'org-air-project-group-by-state))
     (let* ((stats (org-air-r21-5--doc-line-stats))
            (lines-with-doc (car stats))
            (distinct (cdr stats)))
       (should (>= distinct (length org-air-project-test-docs)))
       (should (= lines-with-doc distinct))))))

(ert-deftest org-air-r21-5-doc-row-carries-doc-and-marker-across-the-row ()
  "R21-5: `org-air-project--insert-doc-row' emits ONE board-style row that
carries `org-air-doc' + `org-air-marker' across its WHOLE width (point on
any cell identifies the doc, so RET / visit still resolve), built through
the SHARED `org-air-view--insert-row' (the row also gets the R21-2
`org-air-row-title' title mark).  R25-5 re-bless: the row no longer renders
the relpath ORIGIN cell (dropped as redundant) — `org-air-doc'/`org-air-
marker' still span every cell, so RET/visit resolve unchanged."
  (skip-unless (locate-library "org-air"))
  (let* ((org-air-view-width 100)
         (org-air-project--meta-date-w 12)
         (org-air-project--meta-tags-w 8)
         (org-air-project--meta-origin-w 28)
         (doc (org-air-doc-create
               :name "Alpha feature" :file "/tmp/air/v0.1/alpha-feature.org"
               :relpath "v0.1/alpha-feature.org" :state "ready"
               :tags '("ui" "core")
               :updated org-air-project-test-frozen-mtime
               :created org-air-project-test-frozen-ctime)))
    (with-temp-buffer
      (org-air-project--insert-doc-row doc 100 0)
      ;; exactly ONE line.
      (should (= 1 (count-lines (point-min) (point-max))))
      (goto-char (point-min))
      (let ((bol (line-beginning-position))
            (eol (line-end-position)))
        ;; the whole row identifies the doc + its file marker ...
        (should (eq (get-text-property bol 'org-air-doc) doc))
        (should (equal (get-text-property bol 'org-air-marker)
                       (org-air-doc-file doc)))
        (should (eq (get-text-property (/ (+ bol eol) 2) 'org-air-doc) doc))
        (should (eq (get-text-property (1- eol) 'org-air-doc) doc))
        ;; ... and the shared primitive marked the title's first glyph.
        (should (text-property-any bol eol 'org-air-row-title t))
        ;; the row reads the doc by NAME, with the V6 date cell.  R25-5: the
        ;; relpath origin cell is GONE from the visible row (the doc-file
        ;; marker prop still spans it, asserted above).
        (let ((text (buffer-substring-no-properties bol eol)))
          (should (string-match-p "Alpha feature" text))
          (should (string-match-p "20[0-9][0-9]-[0-9][0-9]-[0-9][0-9]" text))
          (should-not (string-match-p "v0\\.1/alpha-feature\\.org" text)))))))

(ert-deftest org-air-r21-5-doc-rows-share-v6-date-column ()
  "R21-5: the doc rows flow through the SHARED V6 `--insert-row', so the
date cell starts at the SAME screen column on EVERY doc row — even across
different directory-tree nesting depths (the cluster is right-justified to
the render width, independent of the prefix/indent).  A bespoke two-line
`concat' could never guarantee this; it is the shared-primitive proof."
  (skip-unless (locate-library "org-air"))
  (dolist (group-fn '(org-air-project-group-by-state
                      org-air-project-group-by-directory))
    (let* ((lines (org-air-project-test--render-lines group-fn 100))
           (cols '()))
      (dolist (l lines)
        (when (string-match "~ 20[0-9][0-9]-[0-9][0-9]-[0-9][0-9]" l)
          (push (match-beginning 0) cols)))
      (ert-info ((format "grouping %s" group-fn))
        ;; several dated doc rows ...
        (should (> (length cols) 2))
        ;; ... all share one date column (V6 alignment).
        (should (= 1 (length (delete-dups (copy-sequence cols)))))))))

;;;; ---------------------------------------------------------------------
;;;; (d) R21-2 — motion / open land point on the TITLE, not the prefix.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r21-2-board-motion-lands-on-title ()
  "R21-2: first-open and row motion (n/p) land point on the TITLE (the
row's identity), not the leading keyword / priority cell.  The
`org-air-row-title' mark under point is the proof — only the title's first
glyph carries it, never the keyword/priority prefix."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
   (org-air-viewport-test-with-dashboard 120
     ;; first-open lands on the first item's title.
     (org-air-view--goto-first-item)
     (should (get-text-property (point) 'org-air-row-title))
     ;; walking item rows keeps landing on the title — every n lands point
     ;; on the `org-air-row-title' mark (rows with a keyword and rows with a
     ;; priority square alike), never on the keyword/priority prefix.
     (let ((rows (list (line-number-at-pos))))
       (dotimes (_ 8)
         (org-air-next-item)
         (should (get-text-property (point) 'org-air-row-title))
         (cl-pushnew (line-number-at-pos) rows))
       ;; and we genuinely traversed several DISTINCT item rows.
       (should (> (length (delete-dups rows)) 1)))
     ;; prev-item lands on the title too.
     (org-air-prev-item)
     (should (get-text-property (point) 'org-air-row-title)))))

(ert-deftest org-air-r21-2-project-motion-lands-past-state-cell ()
  "R21-2: in the project view the doc rows share the mark, so landing via
`org-air-view--goto-row-title' lands on the doc TITLE — PAST the leading
state cell (R26-2: the padded 5-col WORD token, e.g. `READY'/`DRAFT',
sits to the LEFT of the landing point; the cell grew 3->5 so the landing
column moved right by 2 with it)."
  (skip-unless (locate-library "org-air"))
  (let ((org-air-project-view-width 100))
    (org-air-project-test--render
     (when (commandp 'org-air-project-group-by-state)
       (call-interactively 'org-air-project-group-by-state))
     ;; jump to the first doc row.
     (goto-char (or (text-property-not-all (point-min) (point-max) 'org-air-doc nil)
                    (point-min)))
     (org-air-view--goto-row-title)
     (should (get-text-property (point) 'org-air-row-title))
     ;; the state token is to the LEFT of point (point landed past it).
     (let ((prefix (buffer-substring-no-properties
                    (line-beginning-position) (point))))
       (should (string-match-p
                "\\(DRAFT\\|READY\\|WIP\\|COMP\\|DROP\\|UNKNO\\) "
                prefix))))))

;;;; ---------------------------------------------------------------------
;;;; (e) R21-4 — the svg keyword/state badge is overlay-only (text floor).
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r21-4-keyword-badge-text-fallback ()
  "R21-4: the shared svg keyword/state badge has a MANDATORY text fallback
— with svg unavailable (always true under --batch) it returns its TEXT
argument `eq'-unchanged, so the byte/TTY layer always keeps the keyword
text.  The `org-air-keyword-style' = `text' opt-out is likewise a no-op."
  (skip-unless (locate-library "org-air"))
  (let ((txt (propertize "NEXT" 'face 'org-air-face-popout)))
    ;; batch (no svg) -> unchanged.
    (should (eq (org-air-view--svg-keyword-badge txt 'org-air-face-popout) txt))
    ;; explicit `text' opt-out -> unchanged.
    (let ((org-air-keyword-style 'text))
      (should (eq (org-air-view--svg-keyword-badge txt 'org-air-face-popout) txt)))
    ;; a blank cell is returned unchanged (no chip over nothing).
    (let ((blank "   "))
      (should (eq (org-air-view--svg-keyword-badge blank 'org-air-face-popout)
                  blank)))))

(ert-deftest org-air-r21-4-keyword-and-state-cells-keep-text-contract ()
  "R21-4: the badge is OVERLAY-only and SHARED — the board keyword cell and
the project state cell both route through `--svg-keyword-badge', and their
TRUE text (properties stripped) is still the keyword / state token (the
byte contract), so the goldens stay byte-identical."
  (skip-unless (locate-library "org-air"))
  ;; board keyword cell keeps the keyword text.
  (let ((cell (org-air-view--todo-cell "NEXT" 6)))
    (should (string-match-p "NEXT" (substring-no-properties cell))))
  ;; project state cell keeps the padded WORD token (R26-2; NOT an emoji).
  (let ((cell (substring-no-properties (org-air-project--state-cell "ready"))))
    (should (string-match-p "READY" cell))
    ;; no GUI emoji leaked into the byte/TTY layer.
    (should-not (string-match-p "🎯\\|✅\\|📝\\|🗑" cell))))

(ert-deftest org-air-r21-4-board-byte-layer-shows-plain-keyword ()
  "R21-4 byte guard: rendered under --batch (no svg overlay), a board
keyword still appears as plain keyword TEXT in the buffer — the svg badge
never leaked into the text layer."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 120
    (should (string-match-p "TODO" (buffer-substring-no-properties
                                    (point-min) (point-max))))))

;;;; ---------------------------------------------------------------------
;;;; (f) R21-6 — readable mode-line + a non blue-on-blue pane header.
;;;; ---------------------------------------------------------------------

(defun org-air-r21-6--face-attr (face attr mode)
  "Resolve FACE's ATTR (`:foreground'/`:background') for MODE (light/dark).
Reads the explicit 256-colour clause of the defface spec for MODE,
following a single `:inherit' (so the chrome faces' inherited mid-tier
foreground resolves), falling back to the `t' clause."
  (let* ((spec (face-default-spec face))
         (clause (or (cl-find-if
                      (lambda (c) (and (listp (car c))
                                       (member (list 'background mode) (car c))))
                      spec)
                     (assq t spec)))
         (attrs (cdr clause))
         (plist (if (and (consp attrs) (consp (car attrs))) (car attrs) attrs))
         (val (plist-get plist attr)))
    (cond
     ((stringp val) val)
     ((plist-get plist :inherit)
      (let ((parent (plist-get plist :inherit)))
        (org-air-r21-6--face-attr
         (if (listp parent) (car parent) parent) attr mode)))
     (t nil))))

(defun org-air-r21-6--relative-luminance (hex)
  "Return the WCAG 2.x relative luminance of HEX colour \"#RRGGBB\"."
  (cl-flet ((lin (c) (let ((cc (/ c 255.0)))
                       (if (<= cc 0.03928) (/ cc 12.92)
                         (expt (/ (+ cc 0.055) 1.055) 2.4)))))
    (let ((r (lin (string-to-number (substring hex 1 3) 16)))
          (g (lin (string-to-number (substring hex 3 5) 16)))
          (b (lin (string-to-number (substring hex 5 7) 16))))
      (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b)))))

(defun org-air-r21-6--contrast (fg bg)
  "Return the WCAG contrast ratio between FG and BG colour strings."
  (let ((l1 (org-air-r21-6--relative-luminance fg))
        (l2 (org-air-r21-6--relative-luminance bg)))
    (/ (+ (max l1 l2) 0.05) (+ (min l1 l2) 0.05))))

(ert-deftest org-air-r21-6-contrast-helper-is-correct ()
  "Sanity-check the WCAG helper against known anchors: black-on-white is
21:1 and a colour against itself is 1:1."
  (should (< (abs (- (org-air-r21-6--contrast "#000000" "#FFFFFF") 21.0)) 0.1))
  (should (< (abs (- (org-air-r21-6--contrast "#777777" "#777777") 1.0)) 0.01)))

(ert-deftest org-air-r21-6-modeline-passes-wcag-aa ()
  "R21-6: the calm status mode-line is LEGIBLE — its foreground vs its
background clears WCAG AA (>= 4.5) in BOTH the light and dark specs.  On
trunk the faded foreground measured 2.15:1 light / 2.45:1 dark (the
too-dim-to-read report); the mid-tier readable foreground is ~6:1 / ~8:1."
  (skip-unless (locate-library "org-air"))
  (dolist (mode '(light dark))
    (let ((fg (org-air-r21-6--face-attr 'org-air-face-modeline :foreground mode))
          (bg (org-air-r21-6--face-attr 'org-air-face-modeline :background mode)))
      (ert-info ((format "mode-line %s fg=%s bg=%s" mode fg bg))
        (should fg) (should bg)
        (should (>= (org-air-r21-6--contrast fg bg) 4.5))))))

(ert-deftest org-air-r21-6-pane-header-readable-and-not-blue-on-blue ()
  "R21-6: the bottom view-pane header-line is readable and free of the
blue-on-blue clash — (1) its explicit BASE foreground vs the header bg
clears AA (the `▤' icon / `·' separators are legible), and (2) the title
foreground vs the header bg clears AA too (the strong title no longer
collides with the blue-slate header bg).  Both light and dark specs."
  (skip-unless (locate-library "org-air"))
  (dolist (mode '(light dark))
    (let ((base (org-air-r21-6--face-attr 'org-air-face-pane-header :foreground mode))
          (bg   (org-air-r21-6--face-attr 'org-air-face-pane-header :background mode))
          (title (org-air-r21-6--face-attr 'org-air-face-pane-title :foreground mode)))
      (ert-info ((format "pane-header %s base=%s bg=%s title=%s" mode base bg title))
        (should base) (should bg) (should title)
        ;; (1) the header base foreground is readable on the header bg.
        (should (>= (org-air-r21-6--contrast base bg) 4.5))
        ;; (2) the title is readable on the header bg (no blue-on-blue).
        (should (>= (org-air-r21-6--contrast title bg) 4.5))))))

(provide 'org-air-round21-test)
;;; org-air-round21-test.el ends here
