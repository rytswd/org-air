;;; org-air-round32-test.el --- executing ERTs for v0.5 round-32 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.5 round-32 (air/v0.5/org-air-round32-design.org).
;;
;;   R32-1  PROJECT HOVER HIGHLIGHTS THE WHOLE DOC BLOCK.  The shared row
;;          primitive `org-air-view--insert-row' applied `:props' (incl.
;;          `mouse-face 'org-air-face-cursor') over start..(point) — INCLUDING
;;          the trailing row-separator newline.  In the project's DIRECT-insert
;;          orientations (board-only, side-window) consecutive doc rows' mouse-
;;          face-bearing newlines FUSE adjacent rows into ONE `mouse-face' hover
;;          run, so hovering a single doc highlights the whole contiguous doc
;;          block.  The fix scopes `mouse-face' to the row BODY (strips it from
;;          the newline) while keeping org-air-doc / marker / row-title /
;;          font-lock-face over the full extent, so click/RET still open the
;;          single doc.
;;
;; The BOARD composes through `org-air-view--render-lines' (split on newline /
;; rejoin with fresh separators), which drops the separator props, so the
;; board never had the merge and the fix is board-invisible.
;;
;; Batch contract: byte-INVISIBLE — `mouse-face' is a text property; every
;; golden is captured with `buffer-substring-no-properties', so no fixture and
;; no manifest entry moves.  In batch (no live window, default width 80 < 90)
;; the project renders in the board-only DIRECT-insert path — exactly the
;; measured reproduction harness.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org-air-test-helpers)
(require 'org-air-project-test)            ; project fixture root + --render macro
(require 'org-air-viewport-helpers)        ; board dashboard harness

(when (locate-library "org-air")
  (require 'org-air))

;;;; ---------------------------------------------------------------------
;;;; Probes — the maximal `mouse-face' run is exactly what Emacs highlights.
;;;; ---------------------------------------------------------------------

(defun org-air-r32--mouse-face-run (pos)
  "Return (BEG . END) of the maximal `mouse-face' run covering POS, or nil.
A run is the contiguous span of positions sharing the SAME (`eq')
`mouse-face' value at POS — precisely what Emacs paints on hover."
  (let ((val (get-text-property pos 'mouse-face)))
    (when val
      (cons (or (previous-single-property-change (1+ pos) 'mouse-face)
                (point-min))
            (or (next-single-property-change pos 'mouse-face)
                (point-max))))))

(defun org-air-r32--mouse-face-runs ()
  "Return the list of maximal (BEG . END) `mouse-face' runs in the buffer."
  (let ((runs nil) (pos (point-min)) (max (point-max)))
    (while (< pos max)
      (if (get-text-property pos 'mouse-face)
          (let ((end (or (next-single-property-change pos 'mouse-face nil max)
                         max)))
            (push (cons pos end) runs)
            (setq pos end))
        (setq pos (or (next-single-property-change pos 'mouse-face nil max)
                      max))))
    (nreverse runs)))

(defun org-air-r32--multiline-run-count ()
  "Return how many buffer `mouse-face' runs span a newline (fuse >1 row)."
  (cl-count-if
   (lambda (r)
     (string-match-p "\n" (buffer-substring-no-properties (car r) (cdr r))))
   (org-air-r32--mouse-face-runs)))

(defun org-air-r32--doc-rows ()
  "Return (BOL . NL) for every doc row: BOL=line start, NL=row-separator pos.
A doc row is a line whose start carries `org-air-doc' (the shared primitive
props the whole row).  NL is `line-end-position' — the position of the row's
trailing newline (or `point-max' for the final, newline-trimmed row)."
  (let (rows)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (get-text-property (line-beginning-position) 'org-air-doc)
          (push (cons (line-beginning-position) (line-end-position)) rows))
        (forward-line 1)))
    (nreverse rows)))

;;;; ---------------------------------------------------------------------
;;;; R32-1 — a doc row's hover highlight spans only its own body.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r32-1-doc-row-mouse-face-own-span ()
  "Project board-only direct-insert path, ALL three groupings: every doc
row carries a `mouse-face 'org-air-face-cursor' hover band that STARTS at
the row's `org-air-row-title' mark (R47-2: the text-only title band — BOL,
the tree gutter and the state badge are OUTSIDE it), the row-separator
newline carries NONE, and the maximal `mouse-face' run embeds ZERO
newlines (spans exactly one row).  Original trunk FAILED — adjacent rows
fused into one 2-5 row run; R47-2 re-bless: the run's BOL edge moved to
the title mark (design air/v0.5/org-air-round47-design.org), the
no-fusion / no-newline-span assertions carry over verbatim."
  (skip-unless (locate-library "org-air"))
  (dolist (group '(org-air-project-group-by-state
                   org-air-project-group-by-directory
                   org-air-project-group-by-tag))
    (org-air-project-test--render
      (when (commandp group) (call-interactively group))
      ;; Batch: no live window, width 80 < `org-air-rail-min-width' 90 -> the
      ;; DIRECT-insert board-only path (the reproduction harness).
      (should (eq org-air-view--orientation 'board-only))
      (let ((rows (org-air-r32--doc-rows)))
        (should rows)
        (dolist (row rows)
          (let* ((bol (car row))
                 (nl  (cdr row))
                 (title (text-property-not-all bol nl 'org-air-row-title nil))
                 (run (and title (org-air-r32--mouse-face-run title))))
            (ert-info ((format "grouping=%s row @%d" group bol))
              ;; the TITLE BAND is the hover region...
              (should title)
              (should (eq (get-text-property title 'mouse-face)
                          'org-air-face-cursor))
              ;; ...starting exactly at the title mark — NOT at BOL: the
              ;; tree gutter / state badge carry no `mouse-face' (R47-2).
              (should run)
              (should (= (car run) title))
              (should (> (car run) bol))
              (should-not (get-text-property bol 'mouse-face))
              ;; ...the row-separator newline is NOT hoverable...
              (should-not (get-text-property nl 'mouse-face))
              (should (<= (cdr run) nl))
              ;; ...and the run painted on hover embeds no newline.
              (should-not
               (string-match-p
                "\n" (buffer-substring-no-properties (car run) (cdr run)))))))))))

(ert-deftest org-air-r32-1-adjacent-rows-independent ()
  "Two CONSECUTIVE doc rows in the same directory (v0.1/: Alpha, Beta) have
DISTINCT `mouse-face' runs, each confined to its OWN row's title band, so
each is its own hover region.  Original trunk FAILED — one fused run;
R47-2 re-bless: run edges moved from BOL..newline to the title-band edges
title-start..cluster-start (design air/v0.5/org-air-round47-design.org);
rows stay distinct and unfused."
  (skip-unless (locate-library "org-air"))
  (org-air-project-test--render
    (call-interactively 'org-air-project-group-by-directory)
    (should (eq org-air-view--orientation 'board-only))
    (let ((rows (org-air-r32--doc-rows))
          (found nil))
      (should rows)
      ;; find an adjacent pair: row A's newline directly precedes row B's bol.
      (cl-loop for (a b) on rows
               when (and b (= (1+ (cdr a)) (car b)))
               do (let* ((title-a (text-property-not-all
                                   (car a) (cdr a) 'org-air-row-title nil))
                         (title-b (text-property-not-all
                                   (car b) (cdr b) 'org-air-row-title nil))
                         (run-a (and title-a
                                     (org-air-r32--mouse-face-run title-a)))
                         (run-b (and title-b
                                     (org-air-r32--mouse-face-run title-b))))
                    (setq found t)
                    (should run-a) (should run-b)
                    ;; the two runs are DISTINCT (not one fused span).
                    (should-not (equal run-a run-b))
                    ;; A's run starts at ITS title mark and ends at/before
                    ;; its own row end — never past the separating newline...
                    (should (= (car run-a) title-a))
                    (should (<= (cdr run-a) (cdr a)))
                    ;; ...and B's run starts at B's OWN title mark, after
                    ;; the newline (never at B's BOL — R47-2).
                    (should (= (car run-b) title-b))
                    (should (> (car run-b) (car b)))
                    (should (< (cdr run-a) (car run-b)))))
      (should found))))

(ert-deftest org-air-r32-1-dir-and-heading-no-mouse-face ()
  "Directory-node lines and section-heading lines (the `org-air-section'
non-doc lines) carry NO `mouse-face' anywhere over their extent — dir/
heading rows get no hover highlight, in ALL THREE groupings (state group
headers, tag headers, and directory nodes+headers).  Regression guard;
passes now, stays."
  (skip-unless (locate-library "org-air"))
  (dolist (group '(org-air-project-group-by-state
                   org-air-project-group-by-directory
                   org-air-project-group-by-tag))
    (org-air-project-test--render
      (call-interactively group)
      (let ((found nil))
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (let ((bol (line-beginning-position))
                  (eol (line-end-position)))
              (when (and (get-text-property bol 'org-air-section)
                         (not (get-text-property bol 'org-air-doc)))
                (setq found t)
                (ert-info ((format "grouping=%s heading @%d" group bol))
                  (should-not (get-text-property bol 'mouse-face))
                  ;; no position over the heading extent carries mouse-face.
                  (should-not (text-property-not-all
                               bol eol 'mouse-face nil)))))
            (forward-line 1)))
        (should found)))))

(ert-deftest org-air-r32-1-click-opens-single-doc ()
  "`mouse-1' / RET resolve `org-air-doc' + `org-air-marker' to the SINGLE
doc under point, targeting the row's OWN file — unchanged by the mouse-face
narrowing (the identity props still cover the whole row body)."
  (skip-unless (locate-library "org-air"))
  (org-air-project-test--render
    (call-interactively 'org-air-project-group-by-state)
    (let ((rows (org-air-r32--doc-rows)))
      (should rows)
      (dolist (row rows)
        (let* ((bol (car row))
               (nl  (cdr row))
               (doc (get-text-property bol 'org-air-doc))
               (marker (get-text-property bol 'org-air-marker)))
          (ert-info ((format "row @%d" bol))
            ;; the row body identifies exactly one doc + its own file.
            (should (org-air-doc-p doc))
            (should (stringp marker))
            (should (equal marker (org-air-doc-file doc)))
            ;; every body position (what `org-air-project-open' reads at
            ;; point) resolves the SAME single doc — click anywhere opens it.
            (let ((mid (/ (+ bol nl) 2)))
              (should (eq (get-text-property mid 'org-air-doc) doc))
              (should (equal (get-text-property mid 'org-air-marker)
                             marker)))))))))

(ert-deftest org-air-r32-1-board-unchanged ()
  "The BOARD render (which composes through `org-air-view--render-lines',
split/rejoin) has ZERO multi-line `mouse-face' runs at every width tier —
the shared fix is board-invisible; board rows were already independent hover
regions.  (The visible-text byte-identity is covered by the golden gate:
`mouse-face' is a property, invisible to `buffer-substring-no-properties'.)"
  (skip-unless (locate-library "org-air"))
  (dolist (width '(80 120 160))               ; board-only, stacked, two-pane tiers
    (org-air-viewport-test-with-dashboard width
      (ert-info ((format "board width=%d" width))
        ;; the board DOES use mouse-face on rows...
        (should (org-air-r32--mouse-face-runs))
        ;; ...but no run ever spans a newline (no fused multi-row hover).
        (should (= 0 (org-air-r32--multiline-run-count)))))))

(provide 'org-air-round32-test)
;;; org-air-round32-test.el ends here
