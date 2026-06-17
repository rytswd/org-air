;;; org-air-project-test.el --- round-8 F5/V6/V3/B1/B2/B4 contracts -*- lexical-binding: t; -*-

;;; Commentary:
;; Spec-true tests for v0.4 round-8 (design tstqmmxm, #ready), staged on
;; the design tip before impl2 builds the renderer/bugs.  The face-only
;; bits (air-state + air-tree faces) already land, so those pass; the
;; FEATURE/BYTE/behaviour contracts (F5 project mode + tree, V6 date
;; column, V3 tag-style, B1 TAB, B2 return, B4 g-map) fail until impl2
;; implements them and are listed in org-air-known-failures.el as the
;; GRIND punch list.  Exact byte project-view fixtures are pinned at the
;; regen, AFTER impl2's tree renderer lands (this file pins the
;; STRUCTURAL invariants + the detection/command contract).
;;
;; The fixture tests/fixtures/air-project/ is a tiny self-contained Air
;; project (air-config.toml + version folders + docs across all four
;; states / several dirs / several tags); ground truth is parsed from it.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org-air-test-helpers)

(when (locate-library "org-air")
  (require 'org-air))

(defconst org-air-project-test-root
  (expand-file-name "air-project" org-air-test-fixture-dir)
  "Root of the self-contained Air-project fixture tree.")

(defconst org-air-project-test-docs
  ;; Ground truth derived independently from the fixture files.  NAME is
  ;; the rendered name = the doc #+title (org-air shows the title when
  ;; present, else the filename).
  '(("alpha-feature.org" :title "Alpha feature"        :state ready             :dir "v0.1/"             :tags ("ui" "core"))
    ("beta-cli.org"      :title "Beta CLI"             :state complete          :dir "v0.1/"             :tags ("core"))
    ("gamma-context.org" :title "Gamma context"        :state draft             :dir "v0.1/air-context/" :tags ("context"))
    ("delta-ui.org"      :title "Delta UI exploration" :state dropped           :dir "v0.2/"             :tags ("ui"))
    ("epsilon-plan.org"  :title "Epsilon plan"         :state draft             :dir "v0.2/"             :tags ("context" "ui"))
    ("zeta-wip.org"      :title "Zeta work in progress" :state work-in-progress :dir "v0.2/"             :tags ("core" "ui")))
  "Each fixture doc with its rendered title and parsed state/dir/tags.")

(defmacro org-air-project-test--render (&rest body)
  "Open the Air-project view over the fixture tree; run BODY in its buffer.
Binds `org-air-sources' to the single fixture project so the command
opens it directly, finds the `org-air-project-mode' buffer, and kills it
afterwards.  Skips cleanly until impl2 provides the command."
  (declare (indent 0) (debug t))
  `(progn
     ;; GRIND: fail (not skip) until impl2 provides the command, so the
     ;; punch list shows red.  The render+assertions run once it exists.
     (should (fboundp 'org-air-project))
     (let ((org-air-sources (list (list :air org-air-project-test-root))))
       (org-air-project-test--with-frozen-mtime
        (save-window-excursion
         (org-air-project)
         (let ((buf (seq-find (lambda (b)
                                (with-current-buffer b
                                  (derived-mode-p 'org-air-project-mode)))
                              (buffer-list))))
           (should buf)
           (unwind-protect
               (with-current-buffer buf ,@body)
             (when (buffer-live-p buf) (kill-buffer buf)))))))))

;;;; F5f — air faces (face-only; land on the design tip → pass now).

(ert-deftest org-air-f5-air-faces-exist ()
  "The Air state-badge + tree faces exist (face-only, design tstqmmxm)."
  (skip-unless (locate-library "org-air"))
  (dolist (f '(org-air-face-air-state-draft org-air-face-air-state-ready
               org-air-face-air-state-complete org-air-face-air-state-dropped
               org-air-face-air-tree))
    (should (facep f))))

;;;; F5a — sources defcustom + Air-project detection.

(ert-deftest org-air-f5-sources-defcustom ()
  "`org-air-sources' is a user option (the unified content entry point)."
  (skip-unless (locate-library "org-air"))
  (should (boundp 'org-air-sources))
  (should (custom-variable-p 'org-air-sources)))

(ert-deftest org-air-f5-detect-air-project ()
  "`org-air-detect-air-project' recognises an Air root by air-config.toml
or an air/ subdir, and rejects a plain directory."
  (skip-unless (locate-library "org-air"))
  (should (fboundp 'org-air-detect-air-project))
  ;; The fixture root has air-config.toml.
  (should (org-air-detect-air-project org-air-project-test-root))
  ;; A plain temp dir is not an Air project.
  (let ((plain (make-temp-file "org-air-plain-" t)))
    (unwind-protect
        (should-not (org-air-detect-air-project plain))
      (delete-directory plain t))))

;;;; F5b — separate command + mode.

(ert-deftest org-air-f5-project-command-and-mode ()
  "`org-air-project' is an interactive command and `org-air-project-mode'
is a distinct major mode (the tree renderer, not the GTD board)."
  (skip-unless (locate-library "org-air"))
  (should (commandp 'org-air-project))
  (should (fboundp 'org-air-project-mode)))

(ert-deftest org-air-f5-board-P-opens-project ()
  "On the GTD board, `P' opens the project view (q returns)."
  (skip-unless (locate-library "org-air"))
  (should (eq (lookup-key org-air-view-mode-map (kbd "P")) 'org-air-project)))

;;;; F5d — tree render structural invariants (byte-testable text).

(ert-deftest org-air-f5-tree-structure ()
  "D-P5 [byte, full replace]: the project view is rebuilt as the SHARED
row renderer (`org-air-view--insert-row', the GTD board's primitive) — a
state-summary header, then grouped SECTIONS of shared rows.  The old
airctl box-drawing tree (┌─│├└) and the (+N) roll-up counts are GONE
(air/v0.4/org-air-round10-design.org §D-P5).  What remains is buffer
TEXT with: the state badges, version-folder section headers, doc rows by
title with the ↻ (~) date + #tags, and per-section count headers."
  (skip-unless (locate-library "org-air"))
  ;; Render at the blessed fixture width (100) so the shared-row titles are
  ;; whole for the title search (D-P5 rows truncate like the GTD board).
  (let ((org-air-project-view-width 100))
   (org-air-project-test--render
    ;; Grouping is a persistent global toggle; pin it to state so this test
    ;; is order-independent of the grouping-toggle test.
    (when (commandp 'org-air-project-group-by-state)
      (call-interactively 'org-air-project-group-by-state))
    (let ((text (buffer-string)))
      ;; D-P5: NO box-drawing tree frame / branches anywhere.
      (should-not (string-match-p "[┌└├│─]" text))
      ;; D-P5: NO (+N) roll-up counts (sections carry a plain count head).
      (should-not (string-match-p "(\\+[0-9]+)" text))
      ;; Every fixture doc renders by its TITLE (org-air shows #+title).
      (dolist (doc org-air-project-test-docs)
        (should (string-match-p (regexp-quote (plist-get (cdr doc) :title))
                                text)))
      ;; Version folders group the docs (section headers in dir grouping;
      ;; the origin column carries the path in every grouping).
      (should (string-match-p "v0\\.1/" text))
      (should (string-match-p "v0\\.2/" text))
      ;; TTY state badges (batch is a TTY): all five states.
      (dolist (badge '("[D]" "[R]" "[W]" "[C]" "[X]"))
        (should (string-match-p (regexp-quote badge) text)))
      ;; ↻ date marker degrades to ~ on TTY; a fixture date is present.
      (should (string-match-p "~ [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" text))
      ;; Tags as accent text.
      (should (string-match-p "#ui\\|#core\\|#context" text))
      ;; Shared-row SECTIONS: a state section header `[D] Draft 2' with a
      ;; trailing count (the replacement for the old roll-up).
      (should (string-match-p "\\[[DRWCX]\\] [A-Z][A-Za-z ]+ [0-9]+$" text))))))

;;;; F5d byte — the project tree matches the blessed fixtures (all modes).

(defun org-air-project-test--render-lines (group-fn width)
  "Render the project view in GROUP-FN at WIDTH; return right-trimmed lines."
  (let ((org-air-sources (list (list :air org-air-project-test-root)))
        (org-air-project-view-width width))
    (org-air-project-test--with-frozen-mtime
     (save-window-excursion
       (org-air-project)
       (let ((buf (seq-find
                   (lambda (b) (with-current-buffer b
                                 (derived-mode-p 'org-air-project-mode)))
                   (buffer-list))))
         (unwind-protect
             (with-current-buffer buf
               (when (and group-fn (commandp group-fn))
                 (call-interactively group-fn))
               ;; Right-trim + drop trailing blanks, exactly as the regen
               ;; tool writes the fixtures.
               (org-air-viewport-test--drop-trailing-blanks
                (mapcar #'string-trim-right
                        (split-string (buffer-string) "\n"))))
           (when (buffer-live-p buf) (kill-buffer buf))))))))

(ert-deftest org-air-f5-project-view-byte-mockups ()
  "The honest org-air-project render of the ./air fixture equals the
blessed project-view fixtures in all three groupings (state/dir/tag) at
width 100, right-trimmed.  The ↻ date is mtime-frozen for byte
stability; --batch renders the TTY [D]/[R]/[W]/[C]/[X] badge forms."
  (skip-unless (locate-library "org-air"))
  (should (fboundp 'org-air-project))
  (pcase-dolist (`(,label . ,group-fn)
                 '(("state" . org-air-project-group-by-state)
                   ("dir"   . org-air-project-group-by-directory)
                   ("tag"   . org-air-project-group-by-tag)))
    (let* ((file (expand-file-name (format "project-view-%s.txt" label)
                                   org-air-test-fixture-dir))
           (expected (and (file-readable-p file)
                          (org-air-viewport-test--drop-trailing-blanks
                           (mapcar #'string-trim-right
                                   (split-string
                                    (with-temp-buffer
                                      (insert-file-contents file)
                                      (buffer-string))
                                    "\n")))))
           (actual (org-air-project-test--render-lines group-fn 100)))
      (ert-info ((format "project-view grouping %s" label))
        (should expected)
        (should (equal actual expected))))))

;;;; F5c — grouping toggle (state / directory / tag), default state.

(ert-deftest org-air-f5-grouping-toggle ()
  "The state / directory / tag groupings produce DISTINCT renders.
The three renders must be pairwise BYTE-DIFFERENT — they group by
different sections (states vs directories vs tags), so a fallback that
renders dir/tag the same as state (the design-caught regression) can
never pass here.  D-P5 [byte] re-bless: the sections are now SHARED-ROW
blocks (no box-drawing glyphs); a section HEADER leads with its grouping
key — dir grouping with a version folder + count, tag grouping with a
#tag + count, state grouping with a `[badge] State N' header."
  (skip-unless (locate-library "org-air"))
  (let ((state (org-air-project-test--render-lines
                'org-air-project-group-by-state 100))
        (dir   (org-air-project-test--render-lines
                'org-air-project-group-by-directory 100))
        (tag   (org-air-project-test--render-lines
                'org-air-project-group-by-tag 100)))
    (should state) (should dir) (should tag)
    ;; Pairwise distinct — no grouping falls back to another.
    (should-not (equal state dir))
    (should-not (equal state tag))
    (should-not (equal dir tag))
    ;; D-P5: no box-drawing glyphs lead any line in any grouping.
    (dolist (lines (list state dir tag))
      (should-not (cl-some (lambda (l) (string-match-p "[┌└├│─]" l)) lines)))
    ;; A DIR section header is a version folder + count (`H v0.1/ 3');
    ;; doc rows end in `.org', so the `/ N' tail uniquely marks a header.
    (should (cl-some (lambda (l)
                       (string-match-p "v0\\.[0-9]+/ [0-9]+\\'" l))
                     dir))
    ;; A TAG section header is a `#tag count' (`#core 3'); doc rows in tag
    ;; grouping never END with `#tag N', so the tail uniquely marks it.
    (should (cl-some (lambda (l)
                       (string-match-p "\\`[[:space:]]*#[a-z]+ [0-9]+\\'" l))
                     tag))))

;;;; V6 — fixed date column (dates line up vertically down the list).

(ert-deftest org-air-v6-date-column-defcustom ()
  "`org-air-date-column' is a user option defaulting to 12 (fits
\"OVERDUE 12d\", \"· 273d quiet\", \"Tomorrow\", \"no date\")."
  (skip-unless (locate-library "org-air"))
  (should (boundp 'org-air-date-column))
  (should (= org-air-date-column 12)))

(ert-deftest org-air-v6-dates-align-in-column ()
  "V6: item-row dates occupy a fixed left-justified column, so the date
token starts at the SAME screen column on every dated row (a readable
table, not a ragged right-aligned cluster)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 120
      (let ((cols '()))
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (let* ((bol (line-beginning-position))
                   (item (or (get-text-property bol 'org-air-item)
                             (let ((p bol) (eol (line-end-position)) found)
                               (while (and (< p eol) (not found))
                                 (when (get-text-property p 'org-air-item)
                                   (setq found t))
                                 (setq p (1+ p)))
                               found)))
                   (line (buffer-substring-no-properties
                          bol (line-end-position))))
              (when (and item
                         (string-match
                          " \\(OVERDUE [0-9]+d\\|Today\\|Tomorrow\\|no date\\)" line))
                (push (match-beginning 1) cols)))
            (forward-line 1)))
        (should (> (length cols) 2))
        ;; All date tokens start at the same column (fixed date column).
        (should (= 1 (length (delete-dups (copy-sequence cols)))))))))

;;;; V3 — svg pills are GUI-only; the byte/text fallback is what we assert.

(ert-deftest org-air-v3-tag-style-defcustom ()
  "`org-air-tag-style' selects svg pill on GUI (when available) else
text; the soft svg dependency never breaks the text fallback."
  (skip-unless (locate-library "org-air"))
  (should (boundp 'org-air-tag-style)))

(ert-deftest org-air-v3-text-fallback-in-batch ()
  "In --batch (no svg display), tags render as plain accent TEXT (#tag),
never an image placeholder — the byte layer is pure text."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 120
    (let ((text (buffer-string)))
      (should (string-match-p "#work\\|#projects\\|#inbox" text)))))
;; (B1/B2/B4 behaviour + keymap tests live in org-air-bugs-test.el, the
;; round-8 bug-batch fast-tracked to main ahead of this F5 stream.)


(provide 'org-air-project-test)
;;; org-air-project-test.el ends here
