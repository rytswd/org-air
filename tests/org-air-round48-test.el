;;; org-air-round48-test.el --- executing ERTs for v0.5 round-48 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.5 round-48 (air/v0.5/org-air-round48-design.org):
;; DROPPED docs in the org-air PROJECT view (a) GREY OUT — the row's title
;; band takes the new dim `org-air-face-project-dropped' (R51-1 removed
;; the R48 strike detail; grey is the sole affordance) via the
;; `org-air-project--doc-row-face' selector (R48-2) — and (b) COLLAPSE BY
;; DEFAULT — hidden per group behind a `… N dropped — TAB to show' fold row
;; (`org-air-project-collapse-dropped', default t;
;; `org-air-project-toggle-dropped' on TAB / RET on the fold row), with
;; per-buffer grouping-qualified expansion keys, a LIVE-filter bypass, and
;; NO count surface moving (R48-3).
;;
;; All tests render the REAL project view over the round-20 air-project
;; fixture root (one dropped doc: v0.2/delta-ui.org "Delta UI exploration",
;; #ui) — executing renders, not unit stubs.  Each behaviour test FAILS if
;; the R48 impl is reverted (no fold / no face / no command); r48-5 and
;; r48-7 are LOCK-style (pass before AND after — the count + scope fences).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)     ; dashboard harness + frozen mtime
(require 'org-air-project-test)         ; project fixture root + --render

(when (locate-library "org-air")
  (require 'org-air))

;;;; =====================================================================
;;;; Harness
;;;; =====================================================================

(defmacro org-air-r48--with-project (group &rest body)
  "Render the fixture project view grouped by GROUP; run BODY in its buffer.
GROUP is one of the symbols `state', `directory', `tag'.  Width 100 (the
blessed fixture width).  `org-air-project-group' is LET-bound so the
grouping never leaks into other tests (the group-by commands `setq' the
dynamic variable, which hits the binding)."
  (declare (indent 1) (debug t))
  `(let ((org-air-project-view-width 100)
         (org-air-project-group ,group))
     (org-air-project-test--render ,@body)))

(defun org-air-r48--fold-positions ()
  "Return one buffer position per `… N dropped' fold ROW, in order."
  (let (rows)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((bol (line-beginning-position)) (eol (line-end-position)))
          (when-let* ((pos (text-property-not-all
                            bol eol 'org-air-dropped-fold nil)))
            (push pos rows)))
        (forward-line 1)))
    (nreverse rows)))

(defun org-air-r48--fold-keys ()
  "Return the `org-air-dropped-fold' KEY of every fold row, in order."
  (mapcar (lambda (pos) (get-text-property pos 'org-air-dropped-fold))
          (org-air-r48--fold-positions)))

(defun org-air-r48--doc-positions (&optional state)
  "Return the first position of each DISTINCT visible doc row.
With STATE, only docs whose `org-air-doc-state' equals STATE."
  (let (seen positions)
    (save-excursion
      (goto-char (point-min))
      (let ((pos (point)))
        (while (and pos (< pos (point-max)))
          (let ((doc (get-text-property pos 'org-air-doc)))
            (when (and doc (not (memq doc seen))
                       (or (null state)
                           (equal (org-air-doc-state doc) state)))
              (push doc seen)
              (push pos positions)))
          (setq pos (next-single-property-change pos 'org-air-doc)))))
    (nreverse positions)))

(defun org-air-r48--fold-label ()
  "Return the batch fold-row label for ONE hidden dropped doc."
  (format "%s 1 dropped" (org-air-view--glyph 'more)))

(defun org-air-r48--line-of (pos)
  "Return the propertyless text of the line at POS."
  (save-excursion
    (goto-char pos)
    (buffer-substring-no-properties (line-beginning-position)
                                    (line-end-position))))

(defun org-air-r48--title-face-at (pos)
  "Return the `font-lock-face' on the row TITLE of the doc row at POS."
  (save-excursion
    (goto-char pos)
    (get-text-property (org-air-view--row-title-pos) 'font-lock-face)))

;;;; =====================================================================
;;;; r48-1 — dropped rows are FOLDED by default (directory grouping)
;;;; =====================================================================

(ert-deftest org-air-r48-1-dropped-rows-folded-by-default ()
  "R48-3: default knob, directory grouping — the dropped doc is NOT in the
visible tree; a fold row keyed (directory . \"v0.2\") with the text
`… 1 dropped' stands in under v0.2/; the v0.2/ header still reads X1
\(counts do not move).  The fold row carries NO `org-air-doc' (n/p + the
inspector skip it) and `mouse-face' over its text-only label.
Trunk FAILS (the DROP row renders inline)."
  (skip-unless (locate-library "org-air"))
  ;; The knob ships as a defcustom, DEFAULT collapsed (truthy spelling).
  (should (custom-variable-p 'org-air-project-collapse-dropped))
  (should (eq t (default-value 'org-air-project-collapse-dropped)))
  (org-air-r48--with-project 'directory
    ;; NO visible row carries a dropped doc; the Delta title is absent.
    (should-not (org-air-r48--doc-positions "dropped"))
    (should-not (string-match-p "Delta UI exploration" (buffer-string)))
    ;; EXACTLY one fold row, keyed to v0.2/ (grouping-qualified).
    (should (equal '((directory . "v0.2")) (org-air-r48--fold-keys)))
    (let* ((pos (car (org-air-r48--fold-positions)))
           (line (org-air-r48--line-of pos)))
      ;; the compact affordance text (batch `...' for the more glyph).
      (should (string-match-p (regexp-quote (org-air-r48--fold-label)) line))
      (should (string-match-p "TAB to show" line))
      ;; NO `org-air-doc' anywhere on the fold row (skipped by n/p and
      ;; the inspector by construction), but a mouse-face over the label.
      (save-excursion
        (goto-char pos)
        (should-not (org-air-view--row-property 'org-air-doc))
        (should (text-property-not-all (line-beginning-position)
                                       (line-end-position)
                                       'mouse-face nil))))
    ;; the v0.2/ dir header still counts the dropped doc (X1 rollup).
    (should (cl-some (lambda (l) (string-match-p "| v0\\.2/ .*X1" l))
                     (split-string (buffer-string) "\n")))))

;;;; =====================================================================
;;;; r48-2 — one fold row per GROUP, in every grouping
;;;; =====================================================================

(ert-deftest org-air-r48-2-fold-rows-per-group-all-groupings ()
  "R48-3: state grouping — the Dropped section keeps its heading + COUNT
\(discoverability) and its BODY is exactly the fold row, keyed
\(state . \"dropped\"); tag grouping — #ui carries a fold row keyed
\(tag . \"#ui\"), and sections without dropped docs carry NONE.
Trunk FAILS (no fold rows anywhere)."
  (skip-unless (locate-library "org-air"))
  ;; STATE grouping.
  (org-air-r48--with-project 'state
    (should-not (org-air-r48--doc-positions "dropped"))
    (should (equal '((state . "dropped")) (org-air-r48--fold-keys)))
    (let ((lines (split-string (buffer-string) "\n"))
          (heading-idx nil) (i 0))
      ;; the heading stays, with its FULL count.
      (dolist (l lines)
        (when (string-match-p "| DROP +Dropped 1" l) (setq heading-idx i))
        (setq i (1+ i)))
      (should heading-idx)
      ;; the section BODY is the fold row alone (the next non-blank line).
      (let ((body (seq-find (lambda (l) (not (string-empty-p (string-trim l))))
                            (nthcdr (1+ heading-idx) lines))))
        (should (string-match-p (regexp-quote (org-air-r48--fold-label))
                                body)))))
  ;; TAG grouping.
  (org-air-r48--with-project 'tag
    (should-not (org-air-r48--doc-positions "dropped"))
    ;; ONE fold row, under #ui only — no other section grows one.
    (should (equal '((tag . "#ui")) (org-air-r48--fold-keys)))))

;;;; =====================================================================
;;;; r48-3 — TAB toggles: reveal GREYED, re-collapse; point discipline
;;;; =====================================================================

(ert-deftest org-air-r48-3-toggle-reveals-greyed-and-hides ()
  "R48-2/R48-3: TAB on the v0.2/ fold row reveals the dropped Delta row at
the group BOTTOM (R51-2) with `org-air-face-project-dropped' (dim — R51-1
de-striked; asserted via the face DEFINITION, not pixels) on its title,
point on the revealed row's title, the visible doc-row count up by EXACTLY
the hidden count (anti-tautology), and NO residual fold row; TAB again from
the revealed dropped row (rule 2) folds it back — point lands on the fold
row and `org-air-project--expanded-dropped' round-trips to empty.
Trunk FAILS (no command, no face)."
  (skip-unless (locate-library "org-air"))
  ;; The face definition: a DISTINCT face inheriting the dim
  ;; `org-air-face-faded'.  R51-1 SUPERSEDES the R48-2 strike detail
  ;; (see air/v0.5/org-air-round51-design.org §R51-1): `:strike-through'
  ;; is GONE — nil/unspecified, never t — because the whole-row face drew
  ;; the strike clear across the inter-column fill as a full-width rule.
  ;; The inherit-faded conjunct STAYS (grey is retained, not dropped).
  (should (facep 'org-air-face-project-dropped))
  (should-not (eq 'org-air-face-project-dropped
                  'org-air-face-air-state-dropped))
  (should (memq (face-attribute 'org-air-face-project-dropped
                                :strike-through nil)
                '(nil unspecified)))
  (let ((inh (face-attribute 'org-air-face-project-dropped :inherit nil)))
    (should (memq 'org-air-face-faded (if (listp inh) inh (list inh)))))
  (org-air-r48--with-project 'directory
    (let ((before (length (org-air-r48--doc-positions)))
          (key '(directory . "v0.2")))
      (should-not org-air-project--expanded-dropped)
      ;; 1. EXPAND from the fold row.
      (goto-char (car (org-air-r48--fold-positions)))
      (org-air-project-toggle-dropped)
      (should (equal (list key) org-air-project--expanded-dropped))
      (let ((dropped (org-air-r48--doc-positions "dropped")))
        ;; the Delta row is revealed — visible rows grew by EXACTLY the
        ;; hidden count (1) and the fold row is GONE (no residual).
        (should (= 1 (length dropped)))
        (should (= (1+ before) (length (org-air-r48--doc-positions))))
        (should-not (org-air-r48--fold-keys))
        ;; R51-2 position: the revealed dropped row ranks LAST in its
        ;; group — below the draft Epsilon AND the unknown Eta (it
        ;; renders exactly where the fold row sat, so TAB is spatially
        ;; stable; the R48-era mid-list jump above the drafts is retired).
        (let ((text (buffer-string)))
          (should (< (string-match "Zeta work in progress" text)
                     (string-match "Delta UI exploration" text)))
          (should (< (string-match "Epsilon plan" text)
                     (string-match "Delta UI exploration" text)))
          (should (< (string-match "Eta notes" text)
                     (string-match "Delta UI exploration" text))))
        ;; the revealed title band carries the GREY face (R48-2).
        (should (eq 'org-air-face-project-dropped
                    (org-air-r48--title-face-at (car dropped))))
        ;; point landed on the revealed dropped row's TITLE.
        (let ((doc (get-text-property (point) 'org-air-doc)))
          (should doc)
          (should (equal "dropped" (org-air-doc-state doc))))
        (should (= (point) (org-air-view--row-title-pos))))
      ;; 2. RE-COLLAPSE from the revealed dropped row (rule 2 — expanded
      ;; groups render no residual fold row; TAB on the row is the verb).
      (org-air-project-toggle-dropped)
      (should-not org-air-project--expanded-dropped)
      (should-not (org-air-r48--doc-positions "dropped"))
      (should (= before (length (org-air-r48--doc-positions))))
      ;; point landed back on the fold row.
      (should (equal key (org-air-view--row-property 'org-air-dropped-fold))))))

;;;; =====================================================================
;;;; r48-4 — knob nil: inline in TODAY's positions, still GREYED
;;;; =====================================================================

(ert-deftest org-air-r48-4-collapse-dropped-nil-renders-inline-greyed ()
  "R48-2/R48-3: `org-air-project-collapse-dropped' nil — NO fold row in ANY
grouping; the dropped Delta row renders inline at the R51-2 group BOTTOM
\(dir tree: below the draft Epsilon AND the unknown Eta in v0.2/) AND its
title band carries the dim `org-air-face-project-dropped' — proving R48-2
is INDEPENDENT of the fold.  Trunk FAILS on the face conjunct."
  (skip-unless (locate-library "org-air"))
  (let ((org-air-project-collapse-dropped nil))
    (dolist (group '(directory state tag))
      (ert-info ((format "grouping %s" group))
        (org-air-r48--with-project group
          ;; NO folding anywhere: no fold property, no fold label.
          (should-not (org-air-r48--fold-keys))
          (should-not (string-match-p (regexp-quote (org-air-r48--fold-label))
                                      (buffer-string)))
          ;; the dropped doc renders inline…
          (let ((dropped (org-air-r48--doc-positions "dropped")))
            (should (= 1 (length dropped)))
            (should (string-match-p "Delta UI exploration"
                                    (org-air-r48--line-of (car dropped))))
            ;; …GREYED (the R48-2 face rides the knob-nil inline mode too).
            (should (eq 'org-air-face-project-dropped
                        (org-air-r48--title-face-at (car dropped)))))
          ;; directory tree: the R51-2 group BOTTOM — the inline DROP row
          ;; sinks below the WIP Zeta, the DRAFT Epsilon AND the unknown
          ;; Eta in v0.2/ (dropped ranks past every other state).
          (when (eq group 'directory)
            (let ((text (buffer-string)))
              (should (< (string-match "Zeta work in progress" text)
                         (string-match "Delta UI exploration" text)))
              (should (< (string-match "Epsilon plan" text)
                         (string-match "Delta UI exploration" text)))
              (should (< (string-match "Eta notes" text)
                         (string-match "Delta UI exploration" text))))))))))

;;;; =====================================================================
;;;; r48-5 — counts and summaries NEVER move while folded (lock-style)
;;;; =====================================================================

(ert-deftest org-air-r48-5-counts-and-summaries-unmoved-while-folded ()
  "R48-3 numeric fence (lock-style — passes BEFORE and AFTER the impl):
with the default knob (folded), every count surface still counts the
dropped doc — the dir letter rollup (v0.2/ reads X1), the state section
heading (Dropped 1), the tag heading (#ui 4 — dropped included), and the
rail Summary block (1 Dropped) in all three groupings.  Values pinned
against `org-air-project--collect-docs' ground truth (the airctl `-Da'
numeric parity surface — the fold partitions ONLY the row-insertion
loops).  The folded-p conjunct is guarded on the R48 command's presence
so the numeric fence passes on the PRE-impl trunk too (lock-style)."
  (skip-unless (locate-library "org-air"))
  ;; Ground truth from the doc collect (the airctl parity surface).
  (let* ((docs (org-air-project--collect-docs org-air-project-test-root))
         (ndropped (cl-count "dropped" docs
                             :key #'org-air-doc-state :test #'equal))
         (nui (cl-count-if (lambda (d) (member "ui" (org-air-doc-tags d)))
                           docs)))
    (should (= 1 ndropped))            ; the fixture's one dropped doc
    (should (= 4 nui))                 ; dropped Delta counts among #ui
    (pcase-dolist (`(,group . ,heading-re)
                   `((directory . ,(format "| v0\\.2/ .*X%d" ndropped))
                     (state     . ,(format "| DROP +Dropped %d" ndropped))
                     (tag       . ,(format "| #ui %d" nui))))
      (ert-info ((format "grouping %s" group))
        (org-air-r48--with-project group
          (let ((text (buffer-string)))
            ;; folded (the fence bites while rows hide) — guarded on the
            ;; R48 command so the COUNT pins below hold on trunk too…
            (when (fboundp 'org-air-project-toggle-dropped)
              (should (org-air-r48--fold-keys)))
            ;; …yet the group heading count includes the dropped doc…
            (should (string-match-p heading-re text))
            ;; …and the rail Summary still counts it.
            (should (string-match-p (format "%d +Dropped" ndropped)
                                    text))))))))

;;;; =====================================================================
;;;; r48-6 — a LIVE filter bypasses the fold entirely
;;;; =====================================================================

(ert-deftest org-air-r48-6-live-filter-bypasses-fold ()
  "R48-3 filter bypass: a live filter (`delta' — the R24-6 token filter
matches only the dropped doc's name) shows the Delta row (GREYED — the
R48-2 face conjunct trunk-FAILS) with NO fold row anywhere (a fold that
hides a match would make the filter lie); clearing the filter restores
the fold.  The visibility conjunct guards the bypass forever."
  (skip-unless (locate-library "org-air"))
  (org-air-r48--with-project 'directory
    ;; live filter matching ONLY the dropped doc.
    (setq org-air-view--tag-filter '("delta") org-air-filter-match 'all)
    (org-air-project-refresh)
    (let ((dropped (org-air-r48--doc-positions "dropped")))
      ;; the match IS visible — no fold hid it…
      (should (= 1 (length dropped)))
      (should (string-match-p "Delta UI exploration" (buffer-string)))
      (should-not (org-air-r48--fold-keys))
      ;; …and it is greyed (R48-2 applies to filter-revealed rows).
      (should (eq 'org-air-face-project-dropped
                  (org-air-r48--title-face-at (car dropped)))))
    ;; clearing the filter restores the fold.
    (org-air-filter-clear)
    (should-not (org-air-r48--doc-positions "dropped"))
    (should (equal '((directory . "v0.2")) (org-air-r48--fold-keys)))))

;;;; =====================================================================
;;;; r48-7 — the BOARD and the session locals are untouched (scope fence)
;;;; =====================================================================

(ert-deftest org-air-r48-7-board-and-session-unaffected ()
  "R48 scope fence (lock-style): the fixture BOARD render carries NO
`org-air-dropped-fold' property (byte-identity to the board goldens is
already pinned by the mockup suite); TAB on the board still runs
`org-air-toggle-section' while the project TAB is the R48 toggle; and the
fold expansion state survives a refresh (the R26-5 session locals are
never wiped).  The R48-side conjuncts are guarded on the command's
presence so the BOARD fence passes on the PRE-impl trunk too
(lock-style)."
  (skip-unless (locate-library "org-air"))
  ;; BOARD: no fold machinery leaks in; TAB keeps its section toggle.
  (org-air-viewport-test-with-dashboard 120
    (should-not (text-property-not-all (point-min) (point-max)
                                       'org-air-dropped-fold nil))
    (should (eq (lookup-key org-air-view-mode-map (kbd "TAB"))
                'org-air-toggle-section)))
  ;; PROJECT side (R48 conjuncts — guarded so the board fence above is
  ;; lock-style on trunk): TAB is the dropped toggle (no collision —
  ;; different maps) and the expansion survives `org-air-project-refresh'
  ;; (the R26-5 session locals).
  (when (fboundp 'org-air-project-toggle-dropped)
    (should (eq (lookup-key org-air-project-mode-map (kbd "TAB"))
                'org-air-project-toggle-dropped))
    (org-air-r48--with-project 'directory
      (goto-char (car (org-air-r48--fold-positions)))
      (org-air-project-toggle-dropped)
      (should (org-air-r48--doc-positions "dropped"))
      (org-air-project-refresh)
      (should (equal '((directory . "v0.2"))
                     org-air-project--expanded-dropped))
      (should (org-air-r48--doc-positions "dropped"))
      (should-not (org-air-r48--fold-keys)))))

(provide 'org-air-round48-test)
;;; org-air-round48-test.el ends here
