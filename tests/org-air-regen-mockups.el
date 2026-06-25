;;; org-air-regen-mockups.el --- regenerate mockup fixtures from the honest renderer -*- lexical-binding: t; -*-

;;; Commentary:
;; One-shot regeneration of the byte-precise layout fixtures from the
;; REAL renderer (run via `make regen-mockups').  For every width in
;; `org-air-regen-widths' this renders the canonical fixture board with
;; the gate's exact conditions — frozen clock (Mon 15 Jun 2026), GUI
;; glyphs, unfiltered, anti-tautology render guards ACTIVE so a shim can
;; never write the fixtures from themselves — and writes the
;; right-trimmed lines to tests/fixtures/layout-mockup-WIDTH.txt.
;;
;; Canonical widths (80/120/160) are the design §3 contract; the
;; threshold widths bracket the responsive breakpoint (~95 with
;; hysteresis 3 and rail tiers 28/32/42 per impl2's D1).
;;
;; After regeneration: diff against design's §3 expectations and route
;; the diff to design for re-blessing before any gate verdict.

;;; Code:

(require 'cl-lib)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)
(require 'org-air)

(defconst org-air-regen-widths '(80 90 96 100 104 110 120 160)
  "Widths to regenerate: canonical 80/120/160 + breakpoint bracket.")

(defconst org-air-regen-board-only-widths '(70)
  "Sub-threshold widths (< `org-air-rail-min-width', default 90) that
render BOARD-ONLY (R13 D-P3): no rail / calendar / inspector, the item
pane fills the whole window.  Written as layout-mockup-WIDTH.txt.")

(defconst org-air-regen-heights '(nil 24 50)
  "Heights per width: natural (nil), overflow branch (24), fill (50).
Blessed by design/orchestrator for the S6 regen.")

(defconst org-air-regen-denote-widths '(80 120)
  "Widths for the R17 long-Denote origin goldens (`denote-origin-WIDTH.txt').
The isolated mini-board pins that a long Denote origin is CAPPED while the
title survives (the title-min budget).")

(defun org-air-regen--lines ()
  "Right-trimmed, trailing-blank-stripped lines of the current buffer."
  (org-air-viewport-test--drop-trailing-blanks
   (mapcar (lambda (line)
             (string-trim-right (substring-no-properties line)))
           (org-air-viewport-test-lines))))

(defun org-air-regen--emit (out lines)
  "Write LINES to OUT and report."
  (with-temp-file out
    (insert (mapconcat #'identity lines "\n") "\n"))
  (message "regen: %s (%d lines, max width %d)"
           (file-name-nondirectory out)
           (length lines)
           (apply #'max (cons 0 (mapcar #'string-width lines)))))

(defun org-air-regen--write (width height)
  "Render the canonical board at WIDTH×HEIGHT and write its fixture.
HEIGHT nil renders at natural height (layout-mockup-WIDTH.txt);
otherwise layout-mockup-WIDTHxHEIGHT.txt."
  (let ((out (expand-file-name
              (if height
                  (format "layout-mockup-%dx%d.txt" width height)
                (format "layout-mockup-%d.txt" width))
              org-air-test-fixture-dir)))
    (org-air-viewport-test-as-gui
      (org-air-viewport-test-with-dashboard (if height
                                                (cons width height)
                                              width)
        (org-air-regen--emit out (org-air-regen--lines))))))

(defun org-air-regen--write-empty (width height)
  "Render the EMPTY board at WIDTH×HEIGHT (the sparse S6 surface)."
  (let ((out (expand-file-name
              (format "layout-mockup-empty-%dx%d.txt" width height)
              org-air-test-fixture-dir)))
    (org-air-viewport-test-as-gui
      (org-air-viewport-test-with-empty-dashboard (cons width height)
        (org-air-regen--emit out (org-air-regen--lines))))))

(defconst org-air-regen-project-groupings
  '(("state" . org-air-project-group-by-state)
    ("dir"   . org-air-project-group-by-directory)
    ("tag"   . org-air-project-group-by-tag))
  "Project-view groupings → fixture suffix + grouping command (F5).")

(defun org-air-regen--write-project (label group-fn width)
  "Render the F5 project view of the ./air fixture in GROUP-FN at WIDTH;
write project-view-LABEL.txt.  Honest org-air-project render (TTY badges
in --batch), right-trimmed."
  (let* ((root (expand-file-name "air-project" org-air-test-fixture-dir))
         (out (expand-file-name (format "project-view-%s.txt" label)
                                org-air-test-fixture-dir))
         (org-air-sources (list (list :air root)))
         (org-air-project-view-width width))
    (org-air-viewport-test--with-frozen-now
     (org-air-project-test--with-frozen-mtime
      (save-window-excursion
        (org-air-project)
        (let ((buf (seq-find
                    (lambda (b) (with-current-buffer b
                                  (derived-mode-p 'org-air-project-mode)))
                    (buffer-list))))
          (with-current-buffer buf
            (when (and group-fn (commandp group-fn))
              (call-interactively group-fn))
            (org-air-regen--emit out (org-air-regen--lines)))
          (when (buffer-live-p buf) (kill-buffer buf))))))))

;; R16 D-P3: bottom *org-air-view* source-pane goldens.  The pane content
;; is the header-line (text contract) + the read-only entry snapshot; we
;; dump both, joined by a newline, so a single byte file pins the whole
;; pane.  The source is the isolated fixture (NOT a board *.org, so it is
;; never copied into the GTD board set), and the dead-marker variant pins
;; the calm `org-air-face-empty' hint.
(defconst org-air-regen-entry-view-source
  (expand-file-name "entry-view/entry-view-source.org" org-air-test-fixture-dir)
  "Deterministic source for the R16 D-P3 view-pane goldens.")

(defun org-air-regen--pane-dump (ctx)
  "Render CTX into the view pane and return `header-line\nbody' (no props)."
  (org-air-view-pane--render ctx)
  (with-current-buffer (get-buffer org-air-view-pane-buffer-name)
    (concat (format "%s" header-line-format)
            "\n"
            (buffer-substring-no-properties (point-min) (point-max)))))

(defun org-air-regen--write-entry-view ()
  "Write the R16 D-P3 view-pane goldens (live snapshot + dead marker)."
  (org-air-viewport-test--with-frozen-now
   (let* ((buf (find-file-noselect org-air-regen-entry-view-source))
          (mk (with-current-buffer buf
                (goto-char (point-min))
                (re-search-forward "^\\* TODO A heading" nil t)
                (goto-char (match-beginning 0))
                (point-marker)))
          (live-ctx (list :marker mk
                          :file org-air-regen-entry-view-source
                          :title "A heading" :state "TODO"))
          (missing (expand-file-name
                    "entry-view/entry-view-missing.org"
                    org-air-test-fixture-dir))
          (dead-ctx (list :marker missing :file missing
                          :title "Missing entry" :state "TODO")))
     (org-air-regen--emit
      (expand-file-name "entry-view-pane.txt" org-air-test-fixture-dir)
      (split-string (org-air-regen--pane-dump live-ctx) "\n"))
     (org-air-regen--emit
      (expand-file-name "entry-view-dead.txt" org-air-test-fixture-dir)
      (split-string (org-air-regen--pane-dump dead-ctx) "\n"))
     (when (buffer-live-p buf) (kill-buffer buf)))))

(defun org-air-regen--write-denote (width)
  "Render the R17 isolated long-Denote board at WIDTH; write its golden.
Emits tests/fixtures/denote-origin-WIDTH.txt from the SAME render path the
byte test asserts (`org-air-viewport-test-denote-board-lines'), guards
active.  Pins: the long Denote origin is capped at `org-air-origin-max-width'
AND the flex title survives (the title-min budget)."
  (let ((out (expand-file-name (format "denote-origin-%d.txt" width)
                               org-air-test-fixture-dir)))
    (org-air-regen--emit out (org-air-viewport-test-denote-board-lines width))))

(defun org-air-regen-mockups ()
  "Regenerate every mockup fixture from the honest renderer.
GTD board: widths × {natural, 24, 50} + the empty board at 120×50.
F5 project view: the ./air fixture in each grouping (state/dir/tag).
D-P3 view pane: the entry snapshot + dead-marker goldens."
  (dolist (width org-air-regen-widths)
    (dolist (height org-air-regen-heights)
      (org-air-regen--write width height)))
  ;; R13 D-P3: board-only fixtures below `org-air-rail-min-width'.
  (dolist (width org-air-regen-board-only-widths)
    (org-air-regen--write width nil))
  (org-air-regen--write-empty 120 50)
  ;; F5 project-view fixture family (one width, all three groupings).
  (when (fboundp 'org-air-project)
    (pcase-dolist (`(,label . ,group-fn) org-air-regen-project-groupings)
      (org-air-regen--write-project label group-fn 100)))
  ;; R16 D-P3 view-pane goldens.
  (when (fboundp 'org-air-view-pane--render)
    (org-air-regen--write-entry-view))
  ;; R17 D-P1 long-Denote origin goldens (isolated mini-board).
  (dolist (width org-air-regen-denote-widths)
    (org-air-regen--write-denote width))
  (message "regen: done — diff fixtures and route to design for re-blessing"))

(provide 'org-air-regen-mockups)
;;; org-air-regen-mockups.el ends here
