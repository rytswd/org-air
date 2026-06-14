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
  ;; Ground truth derived independently from the fixture files.
  '(("alpha-feature.org"  . (:state ready    :dir "v0.1/"             :tags ("ui" "core")))
    ("beta-cli.org"       . (:state complete :dir "v0.1/"             :tags ("core")))
    ("gamma-context.org"  . (:state draft    :dir "v0.1/air-context/" :tags ("context")))
    ("delta-ui.org"       . (:state dropped  :dir "v0.2/"             :tags ("ui")))
    ("epsilon-plan.org"   . (:state draft    :dir "v0.2/"             :tags ("context" "ui"))))
  "Each fixture doc and its parsed state/dir/tags (test ground truth).")

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
       (save-window-excursion
         (org-air-project)
         (let ((buf (seq-find (lambda (b)
                                (with-current-buffer b
                                  (derived-mode-p 'org-air-project-mode)))
                              (buffer-list))))
           (should buf)
           (unwind-protect
               (with-current-buffer buf ,@body)
             (when (buffer-live-p buf) (kill-buffer buf))))))))

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
  "The project tree is buffer TEXT with the airctl-status structure:
box-drawing frame + branches, the four state badges (TTY text form in
batch), version-folder grouping, doc rows with the ↻ (~) date and
#tags, and (+N) roll-up counts."
  (skip-unless (locate-library "org-air"))
  (org-air-project-test--render
    (let ((text (buffer-string)))
      ;; Box-drawing tree frame + branches.
      (should (string-match-p "[┌└][─-]" text))
      (should (string-match-p "[│|]" text))
      (should (string-match-p "[├└][─-] " text))
      ;; Every fixture doc renders.
      (dolist (doc org-air-project-test-docs)
        (should (string-match-p (regexp-quote (car doc)) text)))
      ;; Version folders group the docs.
      (should (string-match-p "v0\\.1/" text))
      (should (string-match-p "v0\\.2/" text))
      ;; TTY state badges (batch is a TTY): [D]/[R]/[C]/[X].
      (dolist (badge '("[D]" "[R]" "[C]" "[X]"))
        (should (string-match-p (regexp-quote badge) text)))
      ;; ↻ date marker degrades to ~ on TTY; a fixture date is present.
      (should (string-match-p "~ [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" text))
      ;; Tags as accent text.
      (should (string-match-p "#ui\\|#core\\|#context" text))
      ;; (+N) roll-up count somewhere.
      (should (string-match-p "(\\+[0-9]+)" text)))))

;;;; F5c — grouping toggle (state / directory / tag), default state.

(ert-deftest org-air-f5-grouping-toggle ()
  "The project view groups by STATE (default), DIRECTORY (d) and TAG (t):
the top-level boxes reflect the active grouping."
  (skip-unless (locate-library "org-air"))
  (org-air-project-test--render
    ;; Default grouping = by state: a state name heads a top box.
    (should (string-match-p "Draft\\|Ready\\|Complete\\|Dropped"
                            (buffer-string)))
    ;; Toggle to directory grouping.
    (when (commandp 'org-air-project-group-directory)
      (call-interactively 'org-air-project-group-directory)
      (should (string-match-p "v0\\.1/\\|v0\\.2/" (buffer-string))))
    ;; Toggle to tag grouping.
    (when (commandp 'org-air-project-group-tag)
      (call-interactively 'org-air-project-group-tag)
      (should (string-match-p "#ui\\|#core\\|#context" (buffer-string))))))

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
