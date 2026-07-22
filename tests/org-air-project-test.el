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
       (org-air-test-with-frozen-project-path org-air-project-test-root
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
             (when (buffer-live-p buf) (kill-buffer buf))))))))))

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
  "R21-5 [byte] re-bless: the project view renders each doc as ONE
board-style row through the SHARED `org-air-view--insert-row' (state cell +
clean title + the V6 date/tags meta cluster), DROPPING the old
two-line emoji block.  Row shape: `[badge] Title  <date>  #tags' on a single
line.  R26-2 re-bless: the state cell is the uniform padded 5-col WORD
token (DRAFT/READY/WIP/COMP/DROP) from `org-air-project--state-words', not
the `[R]'-style bracket token; section headings carry the same word cells.
R25-5 re-bless: the redundant relpath origin cell is DROPPED from the
project row (the dir tree + title already carry that signal).  The old
`created…/updated…' second line and its labels are GONE — the date is now the
single V6 `↻ YYYY-MM-DD' cell (TTY `~ …').  The nested directory tree,
per-dir counts and the shared rail are UNCHANGED; section headers keep the
round-11 `▌'/`|' prefix + `[badge] State N'.  No box-drawing tree
(air/v0.5/org-air-round21-design.org §R21-5).
R48-3 re-bless: dropped docs FOLD by default
(`org-air-project-collapse-dropped' t), so the every-doc-by-title conjunct
splits — every NON-dropped doc renders by title while the dropped doc
hides behind the `… N dropped' fold row, and STILL renders by title once
the knob is nil (folding off) — the assertion stays strong, not weakened
(air/v0.5/org-air-round48-design.org §R48-3)."
  (skip-unless (locate-library "org-air"))
  ;; Render at the blessed fixture width (100).
  (let ((org-air-project-view-width 100))
   (org-air-project-test--render
    ;; Grouping is a persistent global toggle; pin it to state so this test
    ;; is order-independent of the grouping-toggle test.
    (when (commandp 'org-air-project-group-by-state)
      (call-interactively 'org-air-project-group-by-state))
    (let ((lines (split-string (buffer-string) "\n"))
          (text (buffer-string)))
      ;; NO box-drawing tree frame / branches (the TTY pane divider is a
      ;; plain `|', not a box glyph).
      (should-not (string-match-p "[┌┐└┘├┤┬┴┼]" text))
      ;; State grouping carries no descendant roll-up.
      (should-not (string-match-p "(\\+[0-9]+)" text))
      ;; R48-3 re-bless: every NON-dropped fixture doc renders by its
      ;; TITLE; the dropped doc is FOLDED by default (knob t) behind the
      ;; `… N dropped' fold row, so its title is ABSENT from the default
      ;; render.
      (dolist (doc org-air-project-test-docs)
        (if (eq (plist-get (cdr doc) :state) 'dropped)
            (should-not (string-match-p
                         (regexp-quote (plist-get (cdr doc) :title)) text))
          (should (string-match-p (regexp-quote (plist-get (cdr doc) :title))
                                  text))))
      ;; …the fold affordance stands in for the hidden dropped doc…
      (should (string-match-p
               (regexp-quote (format "%s 1 dropped"
                                     (org-air-view--glyph 'more)))
               text))
      ;; …and with the knob nil (folding OFF) EVERY doc — dropped
      ;; included — renders by its TITLE again (the strong half of the
      ;; re-blessed conjunct; a render that simply lost the dropped doc
      ;; could never pass this).
      (let ((org-air-project-collapse-dropped nil))
        (org-air-project-refresh)
        (let ((all (buffer-string)))
          (dolist (doc org-air-project-test-docs)
            (should (string-match-p
                     (regexp-quote (plist-get (cdr doc) :title)) all)))))
      (org-air-project-refresh)
      ;; R21-5 ONE-LINE shape: a doc's state badge, clean TITLE and the V6
      ;; date cell all sit on the SAME buffer line (the two-line block is
      ;; gone).  R25-5 re-bless: the doc row no longer carries the relpath
      ;; origin cell (redundant with the dir tree + title), so the line ends
      ;; with the date/tags cluster, NOT a `v0.N/...' origin.
      (should (cl-some
               (lambda (l)
                 (string-match-p
                  "READY Alpha feature .*~ 20[0-9][0-9]-[0-9][0-9]-[0-9][0-9]"
                  l))
               lines))
      ;; The SINGLE V6 date cell (`~ YYYY-MM-DD'); the OLD two-line
      ;; `created…/updated…' labels are GONE.
      (should (string-match-p "~ [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" text))
      (should-not (string-match-p "created [0-9]" text))
      (should-not (string-match-p "updated [0-9]" text))
      ;; R25-5: the project STATE grouping shows NO `v0.N/' relpath origin
      ;; cell anywhere (state grouping has no dir headers either, so the
      ;; version path is fully absent now the origin column is dropped).
      (should-not (string-match-p "v0\\.[0-9]/" text))
      ;; TTY state badges (batch is a TTY): all five states as the R26-2
      ;; uniform 5-col WORD tokens.
      (dolist (badge '("DRAFT" "READY" "WIP" "COMP" "DROP"))
        (should (string-match-p (regexp-quote badge) text)))
      ;; Tags as accent text, inline on the row.
      (should (string-match-p "#ui\\|#core\\|#context" text))
      ;; Section header: `▌'/`|' prefix marker + `WORD State N' (the padded
      ;; 5-col word cell, e.g. `| DRAFT Draft 2' / `| WIP   Work …').
      (should (string-match-p
               "| \\(DRAFT\\|READY\\|WIP\\|COMP\\|DROP\\) +[A-Z][A-Za-z ]+ [0-9]"
               text))
      ;; The project rail still carries a `Summary' and an `Inspector' block.
      (should (string-match-p "| Summary" text))
      (should (string-match-p "| Inspector" text))))))

;;;; F5d byte — the project tree matches the blessed fixtures (all modes).

(defun org-air-project-test--render-lines (group-fn width)
  "Render the project view in GROUP-FN at WIDTH; return right-trimmed lines.
The clock is frozen to `org-air-test-now' (matching the regen tool) so the
D-P1.B project inspector's relative date terms (created/updated `(Nd ago)')
are byte-stable, and the file mtime is pinned for the same reason."
  (let ((org-air-sources (list (list :air org-air-project-test-root)))
        (org-air-project-view-width width))
    (org-air-test-with-frozen-project-path org-air-project-test-root
     (org-air-viewport-test--with-frozen-now
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
           (when (buffer-live-p buf) (kill-buffer buf ))))))))))

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
never pass here.  R20-5(a) [byte] re-bless: the DIRECTORY grouping is now
the `airctl status -Da' NESTED directory tree (rolled-up top-dir header
with state-NAME totals, per-dir `BADGE N (+M)' counts, depth-indented
child dirs), replacing the old flat first-segment `v0.N/ <count>'
heading.  State grouping keeps its `[badge] State N' headers; tag
grouping its `#tag count' headers.  R22-6 re-bless: the directory tree now
emits ONE header per dir (the doubled rolled-up box header is gone) with a
quiet RIGHT-ALIGNED letter-count summary (`R1 C1 D(+1)' / `W1 X1 D1') in
place of the old `[X] N' badge wall + state-NAME totals; the `(+N)'
descendant roll-up and the deeper child-dir indent survive."
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
    ;; No box-drawing tree glyphs in any grouping (the TTY pane divider is
    ;; a plain `|', not a box glyph).
    (dolist (lines (list state dir tag))
      (should-not (cl-some (lambda (l) (string-match-p "[┌┐└┘├┤┬┴┼]" l)) lines)))
    ;; R20-5(a)/R22-6: the DIRECTORY grouping is the `airctl status -Da'
    ;; NESTED tree.  Assert the structural hallmarks the OLD flat
    ;; first-segment grouping could never produce (R22-6 re-bless: ONE
    ;; header/dir + a right-aligned letter-count summary, not the badge wall):
    ;;  (1) ONE version-dir header line carrying the quiet letter-count
    ;;      summary (`| v0.1/  …  R1 C1 D(+1)'), no `[R] Ready (1)' NAME wall.
    (should (cl-some (lambda (l)
                       (string-match-p "| v0\\.[0-9]+/ +[RWCXDU][0-9(]" l))
                     dir))
    ;;      and the OLD doubled state-NAME totals header is GONE.
    (should-not (cl-some (lambda (l)
                           (string-match-p
                            "v0\\.[0-9]+/ +\\[[RWCXD]\\] +[A-Z][a-z]" l))
                         dir))
    ;;  (2) a NESTED child-directory heading — the proof the tree recurses
    ;;      past the first path segment: `v0.1/air-context/' renders as its
    ;;      OWN `air-context/' node with its letter-count (never a heading in
    ;;      state/tag).  R23-3 re-bless: a child dir is led by a faded tree
    ;;      CONNECTOR (`+- ' in batch, GUI `└─'/`├─'), NOT the `|' marker
    ;;      reserved for top dirs, so nesting reads unmistakably.
    (should (cl-some (lambda (l)
                       (string-match-p "\\+- air-context/ +[RWCXDU][0-9]" l))
                     dir))
    ;;      and that child heading is NOT led by the top-dir `|' marker.
    (should-not (cl-some (lambda (l)
                           (string-match-p "| +air-context/ +[RWCXDU][0-9]" l))
                         dir))
    ;;  (3) a `(+N)' DESCENDANT roll-up count on a dir heading (the Draft
    ;;      Gamma lives in the child dir, so `v0.1/' shows `D(+1)') —
    ;;      neither state nor tag grouping carries a roll-up.
    (should (cl-some (lambda (l) (string-match-p "(\\+[0-9]+)" l)) dir))
    (should-not (cl-some (lambda (l) (string-match-p "(\\+[0-9]+)" l)) state))
    (should-not (cl-some (lambda (l) (string-match-p "(\\+[0-9]+)" l)) tag))
    ;; A TAG section header is the `|' marker + a `#tag count' (`| #core 3');
    ;; doc rows carry `#tag' inline but never `| #tag N'.
    (should (cl-some (lambda (l)
                       (string-match-p "| #[a-z]+ [0-9]+" l))
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
  (let ((org-air-show-origin t)) ; R30-3: date-align guard on the origin-ON board (its original layout)
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
        (should (= 1 (length (delete-dups (copy-sequence cols))))))))))

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


;;;; R18 D-P3 — the project reuses the dashboard view core.

(defun org-air-r18-dp3--doc-positions ()
  "Return the first buffer position of each DISTINCT project doc row.
Dedupes by the `org-air-doc' OBJECT so two spans of the same doc row never
count as two rows (the follow change-guard keys on the doc object)."
  (let (seen positions)
    (save-excursion
      (goto-char (point-min))
      (let ((pos (point)))
        (while (and pos (< pos (point-max)))
          (let ((doc (get-text-property pos 'org-air-doc)))
            (when (and doc (not (memq doc seen)))
              (push doc seen)
              (push pos positions)))
          (setq pos (next-single-property-change pos 'org-air-doc)))))
    (nreverse positions)))

(ert-deftest org-air-r18-dp3-project-filter-narrows ()
  "R18 D-P3: a tag filter thins the project docs exactly like the board.
AND shows only docs carrying ALL active tags; OR shows any; clearing
restores every doc.  Driven through the shared filter STATE + the doc-aware
`org-air-view--tags-pass-filter-p' (set the state, refresh, read the docs).
R69-5 re-bless (air/v0.1/org-air-round69-design.org): the project header
filter segment now routes through the R24-6
`org-air-view--filter-token-label', so the BARE `ui'/`core' tokens read
QUOTED (`\"ui\" AND \"core\"'), never falsely tag-dressed (`#ui AND
#core' — the header used to hand-prepend `#').  The NARROWING semantics
are untouched — every membership conjunct carries over verbatim.
R48-3 re-bless: with NO filter live the dropped doc is FOLDED by default
(`org-air-project-collapse-dropped' t), so the baseline and the
clear-restores conjuncts assert every NON-dropped title + the fold row
(dropped title ABSENT); the filter-LIVE conjuncts still assert the dropped
Delta doc VISIBLE — the R48-3 filter bypass
(air/v0.5/org-air-round48-design.org §R48-3)."
  (skip-unless (locate-library "org-air"))
  (let ((org-air-project-view-width 100))
    (org-air-project-test--render
     (when (commandp 'org-air-project-group-by-state)
       (call-interactively 'org-air-project-group-by-state))
     (let ((all (buffer-string)))
       ;; R48-3 baseline: NO filter live -> the dropped doc is FOLDED, so
       ;; every NON-dropped fixture doc title renders while the dropped
       ;; Delta title is ABSENT behind the `… N dropped' fold row.
       (dolist (doc org-air-project-test-docs)
         (if (eq (plist-get (cdr doc) :state) 'dropped)
             (should-not (string-match-p
                          (regexp-quote (plist-get (cdr doc) :title)) all))
           (should (string-match-p
                    (regexp-quote (plist-get (cdr doc) :title)) all))))
       (should (string-match-p
                (regexp-quote (format "%s 1 dropped"
                                      (org-air-view--glyph 'more)))
                all)))
     ;; #ui filter -> only the four ui-tagged docs.
     (setq org-air-view--tag-filter '("ui") org-air-filter-match 'all)
     (org-air-project-refresh)
     (let ((text (buffer-string)))
       (should (string-match-p "Alpha feature" text))
       (should (string-match-p "Delta UI exploration" text))
       (should (string-match-p "Epsilon plan" text))
       (should (string-match-p "Zeta work in progress" text))
       ;; the non-ui docs are gone.
       (should-not (string-match-p "Beta CLI" text))
       (should-not (string-match-p "Gamma context" text))
       ;; the header surfaces the active filter — the bare token QUOTED
       ;; (R69-5: `--filter-token-label', never hand-prepended `#').
       (should (string-match-p "\"ui\"" text)))
     ;; AND #ui #core -> only docs carrying BOTH (alpha, zeta).
     (setq org-air-view--tag-filter '("ui" "core") org-air-filter-match 'all)
     (org-air-project-refresh)
     (let ((text (buffer-string)))
       (should (string-match-p "\"ui\" AND \"core\"" text))
       ;; the old false tag-dressing never renders (the doc rows' own
       ;; `#ui'/`#core' pills are separate cells, never AND-joined).
       (should-not (string-match-p "#ui AND #core" text))
       (should (string-match-p "Alpha feature" text))
       (should (string-match-p "Zeta work in progress" text))
       (should-not (string-match-p "Delta UI exploration" text)) ; ui only
       (should-not (string-match-p "Epsilon plan" text)))         ; context+ui
     ;; OR #ui #core -> the union (everything with ui OR core).
     (setq org-air-filter-match 'any)
     (org-air-project-refresh)
     (let ((text (buffer-string)))
       (should (string-match-p "\"ui\" OR \"core\"" text))
       (should (string-match-p "Beta CLI" text))            ; core
       (should (string-match-p "Delta UI exploration" text)) ; ui
       (should-not (string-match-p "Gamma context" text)))  ; neither
     ;; clear -> the no-filter render back: every NON-dropped doc title
     ;; restored AND the R48-3 fold restored (the dropped Delta title —
     ;; visible under the live filters above via the fold BYPASS — folds
     ;; away again behind the fold row).
     (org-air-filter-clear)
     (let ((text (buffer-string)))
       (dolist (doc org-air-project-test-docs)
         (if (eq (plist-get (cdr doc) :state) 'dropped)
             (should-not (string-match-p
                          (regexp-quote (plist-get (cdr doc) :title)) text))
           (should (string-match-p
                    (regexp-quote (plist-get (cdr doc) :title)) text))))
       (should (string-match-p
                (regexp-quote (format "%s 1 dropped"
                                      (org-air-view--glyph 'more)))
                text))))))

(ert-deftest org-air-r18-dp3-project-filter-command-prefills ()
  "`org-air-project-filter' pre-fills the prompt with the active filter.
Shares `org-air-view--read-filter' with the board, so each invocation
continues narrowing."
  (skip-unless (locate-library "org-air"))
  (let ((org-air-project-view-width 100))
    (org-air-project-test--render
     (setq org-air-view--tag-filter '("ui" "core"))
     (let ((captured 'unset))
       (cl-letf (((symbol-function 'completing-read-multiple)
                  (lambda (_prompt _table &optional _pred _req initial &rest _)
                    (setq captured initial)
                    '("ui"))))
         (call-interactively #'org-air-project-filter))
       (should (equal captured "ui,core"))
       ;; the chosen filter was applied.
       (should (equal org-air-view--tag-filter '("ui")))))))

(ert-deftest org-air-r18-dp3-project-pane-follow-on-doc-change ()
  "R18 D-P3: the view pane auto-follows the SELECTED DOC in the project.
The project installs the same follow change-guard; only window I/O is
stubbed (batch-safe).  Moving to a different doc redraws; staying does not."
  (skip-unless (locate-library "org-air"))
  (let ((org-air-project-view-width 120)
        (org-air-view-pane-follow t)
        (shown '()))
    (org-air-project-test--render
     (let ((positions (org-air-r18-dp3--doc-positions)))
       (should (> (length positions) 1))
       (cl-letf (((symbol-function 'org-air-view-pane--window-live-p)
                  (lambda () t))
                 ((symbol-function 'org-air-view-pane--show)
                  (lambda (ctx)
                    (push (plist-get ctx :title) shown)
                    ctx)))
         (let ((board (current-buffer))
               (docA (nth 0 positions))
               (docB (nth 1 positions)))
           ;; move to A -> redraw (changed from the nil seed).
           (goto-char docA)
           (org-air-view--view-pane-update-now board)
           (should (= (length shown) 1))
           ;; stay on A -> no redraw.
           (org-air-view--view-pane-update-now board)
           (should (= (length shown) 1))
           ;; move to B -> redraw.
           (goto-char docB)
           (org-air-view--view-pane-update-now board)
           (should (= (length shown) 2))))))))

(provide 'org-air-project-test)
;;; org-air-project-test.el ends here
