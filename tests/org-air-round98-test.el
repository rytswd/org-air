;;; org-air-round98-test.el --- executing ERTs for v0.1 round-98 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.1 round-98 (air/v0.1/org-air-round98-design.org).
;; R98 answered two reports that came from DAILY USE, not from this suite,
;; so this file pins the USER-VISIBLE half of each of them.
;;
;;   ITEM 1 — TAB PAYS FOR WHAT IT SHOWS.
;;     r98-1  the BOUND: `org-air-section-expand-max' caps ONE expand at a
;;            batch of rows, and the cap is the knob's value — not a
;;            constant, and not a function of how big the section is.
;;     r98-2  the fold row's TWO SENTENCES: the collapsed one is the
;;            byte-frozen R51-3 text every board golden holds; the
;;            expanded-but-capped one names ITSELF as the TAB target and
;;            says what the next press costs.
;;     r98-3  PAGING: TAB on the fold row adds a batch until nothing is
;;            behind it, and COLLAPSING FORGETS the budget.
;;     r98-4  the bound OFF (`nil') restores the pre-R98 unbounded expand —
;;            the behaviour `r95-19' leg 2 used to assume.
;;     r98-5  EVERY ORIENTATION SPLICES: board-only, stacked, two-pane and
;;            side-window all rebuild only the body band (ZERO calls to
;;            `org-air-view--render'), and the result is identical —
;;            INCLUDING text properties — to a full render.
;;     r98-6  the R53 law survives the new branch: neither TAB branch
;;            queries, scans or opens a file, now that the `more' branch
;;            can no longer fall back to `(org-air-query-items)'.
;;
;;   ITEM 2 — `dropped' IS A FAMILY.
;;     r98-7   the ONE predicate over the ONE list; `complete' excluded.
;;     r98-8   each member folds INDEPENDENTLY (TAB on Out must not
;;             collapse Dropped) — the `:state' the section now carries.
;;     r98-9   `complete' NEVER folds and never recedes.
;;     r98-10  an UNKNOWN state renders visibly, unfolded, undimmed.
;;     r98-11  BADGES/LETTERS UNCHANGED: `K' for canceled/cancelled, never
;;             `C'; OUT/OFF/DROP tokens, faces and letters byte-frozen.
;;     r98-12  the three-tier sort ladder, with the R80 relations intact.
;;     r98-13  the per-directory rollup COUNTS the family (a folded doc is
;;             still counted where its siblings are).
;;     r98-14  the family dims the TITLE BAND, never the badge cell.
;;
;; Everything renders through the REAL renderer over real temp corpora;
;; nothing here stubs a row.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)     ; frozen clock + frozen mtime
(require 'org-air-project-test)         ; project fixture root
(require 'org-air-round48-test)         ; fold-row / doc-row helpers
(require 'org-air-round95-test)         ; scale corpus + rendered-row readers

(when (locate-library "org-air")
  (require 'org-air))

;;;; =====================================================================
;;;; Board harness — a real board over the R95 scale corpus.
;;;; =====================================================================

(defconst org-air-r98--board-name "*org-air-r98-board*"
  "Board buffer the R98 TAB tests render into.")

(defconst org-air-r98--files
  (mapcar (lambda (i) (format "/tmp/org-air-r98-scale/f%d.org" i))
          (number-sequence 1 4))
  "Synthetic file set the scale corpus spreads over (never opened).")

(defmacro org-air-r98--with-board (spec &rest body)
  "Render a real board over a scale corpus and run BODY in its buffer.
SPEC is a plist: :count (items, default 500), :width (default 120),
:height (default 40), :max (`org-air-section-expand-max', default 200),
:popped (non-nil pops the rail, giving the `side-window' orientation),
:expanded (initial `org-air-view--expanded-sections').
Every item lands in UNTRACKED — one section holding the whole corpus,
which is the shape the R98 user report describes."
  (declare (indent 1) (debug t))
  `(let ((org-air-r98--board (get-buffer-create org-air-r98--board-name)))
     (unwind-protect
         (with-current-buffer org-air-r98--board
           (org-air-view-mode)
           (when (plist-get ,spec :popped)
             (setq org-air-view--rail-popped-out t))
           (org-air-viewport-test--with-frozen-now
             (let* ((org-air-view-width (or (plist-get ,spec :width) 120))
                    (org-air-view-height (or (plist-get ,spec :height) 40))
                    (org-air-section-expand-max
                     (if (plist-member ,spec :max)
                         (plist-get ,spec :max)
                       200))
                    (org-air-view--items
                     (org-air-r95--scale-items
                      (or (plist-get ,spec :count) 500)
                      org-air-r98--files)))
               (setq org-air-view--items-key (org-air-view--cache-key)
                     org-air-view--expanded-sections
                     (copy-sequence (plist-get ,spec :expanded))
                     org-air-view--section-reveal nil)
               (org-air-view--render org-air-view--items nil)
               ,@body)))
       (when (buffer-live-p org-air-r98--board)
         (let ((kill-buffer-query-functions nil)
               (kill-buffer-hook nil))
           (kill-buffer org-air-r98--board))))))

(defun org-air-r98--untracked-rows ()
  "Return how many item rows the Untracked section currently paints."
  (length (org-air-r95--rows 'untracked)))

(defun org-air-r98--more-text ()
  "Return the Untracked fold row's own label text, or nil when none."
  (when-let* ((pos (org-air-r95--more-row-position 'untracked)))
    (save-excursion
      (goto-char pos)
      (let ((end (or (next-single-property-change pos 'org-air-more-row)
                     (point-max))))
        (string-trim (buffer-substring-no-properties pos end))))))

(defun org-air-r98--tab-header ()
  "Press TAB on the Untracked section HEADER."
  (goto-char (org-air-view--find-property 'org-air-section 'untracked))
  (org-air-toggle-section))

(defun org-air-r98--tab-more ()
  "Press TAB on the Untracked fold row; return nil when there is none."
  (when-let* ((pos (org-air-r95--more-row-position 'untracked)))
    (goto-char pos)
    (org-air-toggle-section)
    t))

;;;; =====================================================================
;;;; r98-1 — THE BOUND: one TAB paints a batch, not a section
;;;; =====================================================================

(ert-deftest org-air-r98-1-expand-is-bounded-by-section-expand-max ()
  "ONE TAB reveals at most `org-air-section-expand-max' rows (R98).

THE REPORT THIS PINS.  \"Expanding the hidden items using TAB feels quite
slow\" — measured as 0.588s of frozen frame at 3000 items, because TAB
built a full-fidelity row (face runs, badges, date labels, origin cells)
for every member of the section to fill a window that shows forty.  The
fix is a BOUND, and the design is explicit that the bound — not the
two-pane splice, not the O(n) sort repairs — is what fixed the report.

So the property asserted here is the one the user feels: what ONE TAB
costs does NOT grow with the section.

Four legs:

  1. collapsed, the section shows its ordinary cap and the fold row
     counts every hidden member;
  2. expanded, it shows EXACTLY the batch, and the fold row counts the
     remainder — the count never lies about what is still behind it;
  3. the batch is the KNOB's value, not a constant: a different
     `org-air-section-expand-max' moves the row count with it;
  4. CONSTANT IN SECTION SIZE — quadrupling the corpus leaves the
     revealed row count identical, which is the whole claim."
  (skip-unless (locate-library "org-air"))
  ;; 1 + 2
  (org-air-r98--with-board '(:count 500 :max 200)
    (let ((cap (org-air-view--section-limit 'untracked)))
      (should (= cap (org-air-r98--untracked-rows)))
      (should (= (- 500 cap) (org-air-r95--fold-count 'untracked)))
      (org-air-r98--tab-header)
      (should (memq 'untracked org-air-view--expanded-sections))
      (should (= 200 (org-air-r98--untracked-rows)))
      (should (= 300 (org-air-r95--fold-count 'untracked)))))
  ;; 3 — the bound is the knob
  (org-air-r98--with-board '(:count 500 :max 25)
    (org-air-r98--tab-header)
    (should (= 25 (org-air-r98--untracked-rows)))
    (should (= 475 (org-air-r95--fold-count 'untracked))))
  ;; 4 — and it does not grow with the section
  (let (small large)
    (org-air-r98--with-board '(:count 500 :max 200)
      (org-air-r98--tab-header)
      (setq small (org-air-r98--untracked-rows)))
    (org-air-r98--with-board '(:count 2000 :max 200)
      (org-air-r98--tab-header)
      (setq large (org-air-r98--untracked-rows))
      ;; anti-vacuity: the big corpus really is four times the members.
      (should (= 2000 (+ large (org-air-r95--fold-count 'untracked)))))
    (should (= 200 small))
    (should (= small large))))

;;;; =====================================================================
;;;; r98-2 — the fold row's TWO sentences
;;;; =====================================================================

(defun org-air-r98--collapsed-sentence-re ()
  "The COLLAPSED fold row, byte-frozen since R51-3 (board goldens hold it).
Built off the live `more' glyph so the pin is the SENTENCE, not the
ellipsis spelling (which differs between the GUI `…' and the TTY `...')."
  (concat "\\`" (regexp-quote (org-air-view--glyph 'more))
          "and \\([0-9]+\\) more — press TAB on the title to expand\\'"))

(defun org-air-r98--capped-sentence-re ()
  "The R98 fold row under an ALREADY-EXPANDED but still-capped section."
  (concat "\\`" (regexp-quote (org-air-view--glyph 'more))
          "and \\([0-9]+\\) more — press TAB here to reveal \\([0-9]+\\) more\\'"))

(ert-deftest org-air-r98-2-fold-row-says-what-the-next-press-costs ()
  "The fold row has TWO sentences, and each is only ever shown in its own
state (R98).

WHY A SECOND SENTENCE AT ALL.  Under an expanded-but-capped section the
old wording would have been a lie in two ways at once: TAB on the TITLE
collapses the section rather than revealing more, and the row said
nothing about what the next press would actually do.  So the row names
ITSELF as the target and states the size of the next batch.  This is the
round's whole feedback affordance — the design refused a progress banner
for a 28ms operation — which makes the wording user-visible behaviour
rather than chrome.

Five legs:

  1. COLLAPSED, the sentence is the byte-frozen R51-3 one, and its number
     is every hidden member;
  2. EXPANDED-BUT-CAPPED, the sentence is the R98 one, its first number
     is the remainder and its second is the batch the next press adds;
  3. the two are mutually exclusive — neither state ever prints the
     other's sentence;
  4. the promised batch is HONEST at the end: when fewer rows remain than
     a batch, the row promises only what is left (never `reveal 200 more'
     over 100 rows);
  5. and the row is still the actionable handle it has been since R51-3
     \(`org-air-more-row' over its full extent)."
  (skip-unless (locate-library "org-air"))
  (org-air-r98--with-board '(:count 500 :max 200)
    ;; 1
    (let ((text (org-air-r98--more-text)))
      (should text)
      (should (string-match (org-air-r98--collapsed-sentence-re) text))
      (should (= (- 500 (org-air-view--section-limit 'untracked))
                 (string-to-number (match-string 1 text))))
      ;; 3
      (should-not (string-match-p (org-air-r98--capped-sentence-re) text)))
    ;; 2
    (org-air-r98--tab-header)
    (let ((text (org-air-r98--more-text)))
      (should text)
      (should (string-match (org-air-r98--capped-sentence-re) text))
      (should (= 300 (string-to-number (match-string 1 text))))
      (should (= 200 (string-to-number (match-string 2 text))))
      ;; 3, the other way round
      (should-not (string-match-p (org-air-r98--collapsed-sentence-re) text))
      ;; 5
      (should (eq 'untracked
                  (get-text-property (org-air-r95--more-row-position 'untracked)
                                     'org-air-more-row))))
    ;; 4 — one more page leaves 100, and the row promises 100, not 200.
    (should (org-air-r98--tab-more))
    (let ((text (org-air-r98--more-text)))
      (should text)
      (should (string-match (org-air-r98--capped-sentence-re) text))
      (should (= 100 (string-to-number (match-string 1 text))))
      (should (= 100 (string-to-number (match-string 2 text)))))))

;;;; =====================================================================
;;;; r98-3 — PAGING, and the budget a collapse forgets
;;;; =====================================================================

(ert-deftest org-air-r98-3-fold-row-pages-and-collapse-forgets-the-budget ()
  "TAB on the fold row adds ONE batch; collapsing forgets how far it got.

Nothing is hidden — it is PAGED.  That is the promise the fold row's
count makes, so it is asserted end to end: press the row until the row
is gone, and every member is on screen.  The budget is deliberately NOT
sticky: a collapse throws it away so a re-expand starts at one batch
again, which is what keeps a re-open as cheap as the first open.

Four legs:

  1. each press adds exactly one batch and shrinks the remainder by it;
  2. the LAST press reveals the tail and the fold row DISAPPEARS — every
     member is reachable, so `bounded' never means `truncated';
  3. collapsing FORGETS the budget: a fresh expand paints one batch, not
     the four pages the section had been opened to;
  4. and the budget is per-SECTION — paging Untracked leaves another
     section's own expansion alone."
  (skip-unless (locate-library "org-air"))
  (org-air-r98--with-board '(:count 500 :max 200)
    ;; 1
    (org-air-r98--tab-header)
    (should (= 200 (org-air-r98--untracked-rows)))
    (should (org-air-r98--tab-more))
    (should (= 400 (org-air-r98--untracked-rows)))
    (should (= 100 (org-air-r95--fold-count 'untracked)))
    ;; 2
    (should (org-air-r98--tab-more))
    (should (= 500 (org-air-r98--untracked-rows)))
    (should-not (org-air-r95--more-row-position 'untracked))
    (should-not (org-air-r95--fold-count 'untracked))
    ;; 3
    (org-air-r98--tab-header)                ; collapse
    (should-not (memq 'untracked org-air-view--expanded-sections))
    (should (= (org-air-view--section-limit 'untracked)
               (org-air-r98--untracked-rows)))
    (should (null (alist-get 'untracked org-air-view--section-reveal)))
    (org-air-r98--tab-header)                ; re-expand
    (should (= 200 (org-air-r98--untracked-rows)))
    ;; 4 — the reveal budget is keyed per section.
    (should (org-air-r98--tab-more))
    (should (= 400 (org-air-r98--untracked-rows)))
    (should (= 400 (alist-get 'untracked org-air-view--section-reveal)))
    (should (null (alist-get 'notes org-air-view--section-reveal)))))

;;;; =====================================================================
;;;; r98-4 — the bound is a KNOB, and OFF is the pre-R98 behaviour
;;;; =====================================================================

(ert-deftest org-air-r98-4-bound-off-restores-unbounded-expand ()
  "`org-air-section-expand-max' nil paints the WHOLE section in one TAB.

The escape hatch is part of the contract: a user who would rather wait
than page can switch the bound off and get exactly the pre-R98
behaviour.  This is also the shape `r95-19' leg 2 used to assume before
its R98 re-bless, so it is pinned rather than left implicit.

Three legs:

  1. with the bound OFF one TAB shows every member and leaves NO fold
     row;
  2. a non-positive value is treated as OFF too (a `0' batch would
     otherwise page forever, revealing nothing);
  3. the knob is a real user option, so this is a supported
     configuration and not an internal flag."
  (skip-unless (locate-library "org-air"))
  ;; 3
  (should (custom-variable-p 'org-air-section-expand-max))
  (should (= 200 (default-value 'org-air-section-expand-max)))
  ;; 1
  (org-air-r98--with-board '(:count 500 :max nil)
    (org-air-r98--tab-header)
    (should (= 500 (org-air-r98--untracked-rows)))
    (should-not (org-air-r95--more-row-position 'untracked)))
  ;; 2
  (org-air-r98--with-board '(:count 500 :max 0)
    (org-air-r98--tab-header)
    (should (= 500 (org-air-r98--untracked-rows)))
    (should-not (org-air-r95--more-row-position 'untracked))))

;;;; =====================================================================
;;;; r98-5 — EVERY orientation splices; two-pane no longer full-renders
;;;; =====================================================================

(defconst org-air-r98--orientation-cases
  '((board-only  80 nil)
    (stacked     90 nil)
    (two-pane   120 nil)
    (side-window 120 t))
  "(ORIENTATION WIDTH POPPED) — one case per live board orientation.")

(ert-deftest org-air-r98-5-tab-splices-in-every-orientation ()
  "A TAB expand rebuilds ONLY the body band — in all four orientations.

WHAT THIS REPLACES.  `org-air-view--render-section' used to compose only
the board-only body and let every other orientation fall through to a
full `org-air-view--render', behind a comment that admitted
`correctness first'.  The R98 profile priced that admission: 67% of a
toggle under the full render, and 24% of the whole toggle recomputing
the CALENDAR MONTH, because the rail was recomposed by a render nobody
asked for.  The wide inline-rail layout — `two-pane' — is exactly the
one the user reported TAB being slow in.

Two-pane is not a free win, which is why the assertion is byte-level:
its fill row carries the pane divider, its band's trailing newline is
followed by a footer, and its INSPECTOR lives inside the very band being
rewritten.  Any of those going wrong shows up as a text-property or byte
difference against the full render, not as a crash.

Four legs, per orientation:

  1. the orientation under test really IS the one named (anti-vacuity:
     the four cases are four different layouts, not one repeated);
  2. the toggle calls `org-air-view--render' ZERO times — it splices;
  3. the spliced buffer is identical to a FULL render of the same
     expansion state, INCLUDING every text property (faces, `display'
     specs, the `org-air-*' handles) — the R18 equivalence law, now owed
     by every orientation;
  4. and the toggle really changed the board (else legs 2-3 are vacuous)."
  (skip-unless (locate-library "org-air"))
  (pcase-dolist (`(,orientation ,width ,popped) org-air-r98--orientation-cases)
    (ert-info ((format "orientation %s (width %d, popped %S)"
                       orientation width popped))
      (let (full collapsed spliced renders)
        ;; The source of truth: a full render with the section expanded.
        (org-air-r98--with-board (list :count 500 :width width :popped popped
                                       :expanded '(untracked))
          (should (eq orientation org-air-view--orientation))
          (setq full (buffer-substring (point-min) (point-max))))
        ;; The incremental path: render collapsed, then TAB.
        (org-air-r98--with-board (list :count 500 :width width :popped popped)
          ;; 1
          (should (eq orientation org-air-view--orientation))
          (setq collapsed (buffer-substring (point-min) (point-max))
                renders 0)
          ;; 2
          (let ((real (symbol-function 'org-air-view--render)))
            (cl-letf (((symbol-function 'org-air-view--render)
                       (lambda (&rest args)
                         (setq renders (1+ renders))
                         (apply real args))))
              (org-air-r98--tab-header)))
          (setq spliced (buffer-substring (point-min) (point-max))))
        (should (= 0 renders))
        ;; 4
        (should-not (equal collapsed spliced))
        ;; 3
        (should (equal-including-properties full spliced))))))

;;;; =====================================================================
;;;; r98-6 — the R53 law: no TAB branch ever queries
;;;; =====================================================================

(defmacro org-air-r98--counting-queries (counter &rest body)
  "Run BODY with every SCAN entry point spied on; COUNTER holds the total."
  (declare (indent 1) (debug t))
  `(let ((,counter 0))
     (cl-letf (((symbol-function 'org-air-query-items)
                (lambda (&rest _) (setq ,counter (1+ ,counter)) nil))
               ((symbol-function 'org-air-query-items-in-files)
                (lambda (&rest _) (setq ,counter (1+ ,counter)) nil))
               ((symbol-function 'org-air-query--scan-file)
                (lambda (&rest _) (setq ,counter (1+ ,counter)) nil))
               ((symbol-function 'find-file-noselect)
                (lambda (&rest _) (setq ,counter (1+ ,counter)) nil)))
       ,@body)))

(ert-deftest org-air-r98-6-neither-tab-branch-ever-requeries ()
  "R53 holds through R98: expanding and PAGING both stay off the scanner.

THE REGRESSION THIS FENCES.  Before R98 the fold-row branch resolved its
items as `(or org-air-view--items (org-air-query-items))' — a re-query
one nil away, on the very keystroke the round made cheap.  R98 splices
from `org-air-view--items' alone, so the fallback is gone; this test
makes its return a red gate rather than a slow board.

The reveal budget is a RENDER INPUT, not a side effect: changing it can
never re-derive the item set, never open a file, never touch the cache.

Four legs:

  1. the header TAB (expand) queries ZERO times;
  2. the fold-row TAB (page) queries ZERO times — the branch R98
     rewrote;
  3. the collapse TAB queries ZERO times either;
  4. ANTI-VACUITY — the spies are live: a direct call inside the same
     shim is counted, so a zero above means `never called', not
     `never installed'."
  (skip-unless (locate-library "org-air"))
  (org-air-r98--with-board '(:count 500 :max 200)
    (let (expand page collapse live)
      ;; 1
      (org-air-r98--counting-queries n
        (org-air-r98--tab-header)
        (setq expand n))
      (should (= 200 (org-air-r98--untracked-rows)))
      ;; 2
      (org-air-r98--counting-queries n
        (should (org-air-r98--tab-more))
        (setq page n))
      (should (= 400 (org-air-r98--untracked-rows)))
      ;; 3
      (org-air-r98--counting-queries n
        (org-air-r98--tab-header)
        (setq collapse n))
      (should-not (memq 'untracked org-air-view--expanded-sections))
      ;; 4
      (org-air-r98--counting-queries n
        (org-air-query-items)
        (setq live n))
      (should (= 0 expand))
      (should (= 0 page))
      (should (= 0 collapse))
      (should (= 1 live)))))

;;;; =====================================================================
;;;; Project harness — one temp Air tree holding the whole vocabulary.
;;;; =====================================================================

(defconst org-air-r98--doc-states
  '(("alpha"   . "ready")
    ("bravo"   . "complete")
    ("charlie" . "out")
    ("delta"   . "off")
    ("echo"    . "dropped")
    ("foxtrot" . "canceled")
    ("golf"    . "cancelled")
    ("hotel"   . "experimental")   ; UNKNOWN — never folded, never hidden
    ("india"   . "draft"))
  "One doc per state the R98 vocabulary has to place, plus an unknown one.")

(defvar org-air-r98--collapse-dropped t
  "Value `org-air-r98--with-project' gives `org-air-project-collapse-dropped'.
Let-bind it around the macro to render with the fold OFF.")

(defmacro org-air-r98--with-project (group &rest body)
  "Render a temp Air project holding every R98 state; run BODY in its buffer.
GROUP is the `org-air-project-group' symbol.  The clock and every file
time are frozen, so the rendered bytes do not depend on the day.  The
fold knob comes from `org-air-r98--collapse-dropped' (default on)."
  (declare (indent 1) (debug t))
  `(let ((root (make-temp-file "org-air-r98-proj" t)))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "air/v0.1" root) t)
           (write-region "" nil (expand-file-name "air-config.toml" root)
                         nil 'silent)
           (pcase-dolist (`(,name . ,state) org-air-r98--doc-states)
             (write-region (format "#+title: %s\n#+state: %s\n" name state)
                           nil
                           (expand-file-name (format "air/v0.1/%s.org" name) root)
                           nil 'silent))
           (let ((org-air-sources (list (list :air root)))
                 (org-air-projects (list root))
                 (org-air-project-view-width 100)
                 (org-air-project-group ,group)
                 (org-air-project-collapse-dropped org-air-r98--collapse-dropped)
                 (org-air-rail-focus-on-popout nil))
             (org-air-project-test--with-frozen-mtime
              (org-air-viewport-test--with-frozen-now
               (save-window-excursion
                 (org-air-project)
                 (let ((buf (seq-find
                             (lambda (b)
                               (with-current-buffer b
                                 (derived-mode-p 'org-air-project-mode)))
                             (buffer-list))))
                   (should buf)
                   (unwind-protect
                       (with-current-buffer buf ,@body)
                     (when (buffer-live-p buf)
                       (let ((kill-buffer-query-functions nil))
                         (kill-buffer buf))))))))))
       (delete-directory root t))))

(defun org-air-r98--doc-states-visible ()
  "Return the states of the doc rows the project view currently paints."
  (let (states)
    (dolist (pos (org-air-r48--doc-positions) (nreverse states))
      (push (org-air-doc-state (get-text-property pos 'org-air-doc)) states))))

(defun org-air-r98--goto-fold (state)
  "Move point to the fold row keyed on STATE; return non-nil when found."
  (when-let* ((pos (org-air-view--find-property 'org-air-dropped-fold
                                                (cons 'state state))))
    (goto-char pos)
    t))

;;;; =====================================================================
;;;; r98-7 — ONE predicate over ONE list
;;;; =====================================================================

(ert-deftest org-air-r98-7-dropped-family-is-one-predicate-over-one-list ()
  "`org-air-project--dropped-state-p' is the whole vocabulary, and
`complete' is deliberately outside it (R98).

Before R98 dropped-ness was `(equal state \"dropped\")' in NINE places,
so `out', `off', `canceled' and `cancelled' — states already in real use
— were treated as live docs with odd keywords.  One list, one predicate.

Six legs:

  1. every default member answers yes, and the list is the shipped one;
  2. matching is CASE-INSENSITIVE (a doc that writes `Dropped' is
     dropped), and nil / non-strings are not dropped-like;
  3. `complete' is NOT a member — the positive terminal state is never
     folded away, because burying finished work is exactly the number a
     planning view must not hide;
  4. nor is any live state, nor an unknown one;
  5. the predicate READS the option: rebinding
     `org-air-project-dropped-states' moves the answer, which is what
     makes the customisation real;
  6. and the position accessor agrees with the predicate, so the sort
     rank and the membership test can never disagree."
  (skip-unless (locate-library "org-air"))
  ;; 1
  (should (equal '("dropped" "canceled" "cancelled" "out" "off")
                 (default-value 'org-air-project-dropped-states)))
  (dolist (state org-air-project-dropped-states)
    (ert-info ((format "member %s" state))
      (should (org-air-project--dropped-state-p state))))
  ;; 2
  (should (org-air-project--dropped-state-p "DROPPED"))
  (should (org-air-project--dropped-state-p "Cancelled"))
  (should-not (org-air-project--dropped-state-p nil))
  (should-not (org-air-project--dropped-state-p 'dropped))
  (should-not (org-air-project--dropped-state-p ""))
  ;; 3
  (should-not (member "complete" org-air-project-dropped-states))
  (should-not (org-air-project--dropped-state-p "complete"))
  ;; 4
  (dolist (state '("draft" "ready" "work-in-progress" "experimental"))
    (ert-info ((format "not a member: %s" state))
      (should-not (org-air-project--dropped-state-p state))))
  ;; 5
  (let ((org-air-project-dropped-states '("shelved")))
    (should (org-air-project--dropped-state-p "shelved"))
    (should-not (org-air-project--dropped-state-p "dropped")))
  ;; 6
  (dolist (state (append org-air-project-dropped-states
                         '("complete" "draft" "experimental")))
    (should (eq (and (org-air-project--dropped-state-position state) t)
                (org-air-project--dropped-state-p state)))))

;;;; =====================================================================
;;;; r98-8 — each member folds INDEPENDENTLY
;;;; =====================================================================

(ert-deftest org-air-r98-8-each-family-member-folds-independently ()
  "TAB on `Out' must not collapse `Dropped' (R98).

THE BUG THIS WOULD HAVE BEEN.  Under `state' grouping the fold key used
to be the literal `(state . \"dropped\")' for every section.  Making the
family span five sections without giving each one its OWN key would have
made all five share a single expansion flag: opening Out would open
Dropped, and TAB on either would close both.  R98 puts the bucket's raw
state in the section plist (`:state') and keys the fold on it.

Five legs:

  1. every family member with docs gets its OWN section and its OWN fold
     row, keyed on its own state;
  2. folded, a family section keeps its heading and its COUNT — the fold
     hides rows, never the fact that the rows exist;
  3. TAB on Out's fold row reveals the OUT doc…
  4. …and leaves every OTHER family section folded — the independence
     claim, stated negatively;
  5. TAB again re-collapses only Out."
  (skip-unless (locate-library "org-air"))
  (org-air-r98--with-project 'state
    ;; 1
    (should (equal '((state . "out") (state . "off") (state . "dropped")
                     (state . "canceled") (state . "cancelled"))
                   (org-air-r48--fold-keys)))
    ;; 2 — the headings and their counts survive the fold.
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (dolist (title '("Out 1" "Off 1" "Dropped 1" "Canceled 1" "Cancelled 1"))
        (ert-info ((format "heading %s" title))
          (should (string-match-p (regexp-quote title) text)))))
    (should-not (member "out" (org-air-r98--doc-states-visible)))
    ;; 3
    (should (org-air-r98--goto-fold "out"))
    (org-air-project-toggle-dropped)
    (should (member "out" (org-air-r98--doc-states-visible)))
    ;; 4 — nobody else moved.
    (dolist (state '("off" "dropped" "canceled" "cancelled"))
      (ert-info ((format "still folded: %s" state))
        (should-not (member state (org-air-r98--doc-states-visible)))
        (should (org-air-r98--goto-fold state))))
    (should (equal '((state . "off") (state . "dropped")
                     (state . "canceled") (state . "cancelled"))
                   (org-air-r48--fold-keys)))
    ;; 5 — the revealed group renders NO residual fold row, so the
    ;; re-collapse verb is TAB on a revealed row (R48-3), and it puts
    ;; Out's fold row back without touching anyone else's.
    (should-not (org-air-r98--goto-fold "out"))
    (goto-char (car (org-air-r48--doc-positions "out")))
    (org-air-project-toggle-dropped)
    (should-not (member "out" (org-air-r98--doc-states-visible)))
    (should (org-air-r98--goto-fold "out"))
    (should (equal '((state . "out") (state . "off") (state . "dropped")
                     (state . "canceled") (state . "cancelled"))
                   (org-air-r48--fold-keys)))))

;;;; =====================================================================
;;;; r98-9 — `complete' never folds
;;;; =====================================================================

(ert-deftest org-air-r98-9-complete-never-folds-and-never-recedes ()
  "The POSITIVE terminal state stays on screen, in full colour (R98).

`complete' is the outcome the whole board exists to produce.  Folding it
with the abandoned work would hide finished work as though it had been
dropped, and the count of what got done is the one number a planning
view must never bury.  The family is about what is CLOSED-NEGATIVE, and
this is the line.

Four legs:

  1. the Complete doc row renders INLINE with the fold on and no filter
     — no fold row is ever keyed on `complete';
  2. its title band is NOT the receded dropped face;
  3. its badge face and letter are the Complete ones, untouched by the
     family;
  4. and adding `complete' to the list WOULD fold it — proving leg 1 is
     the default's deliberate choice and not an accident of the code."
  (skip-unless (locate-library "org-air"))
  (org-air-r98--with-project 'state
    ;; 1
    (should (member "complete" (org-air-r98--doc-states-visible)))
    (should-not (org-air-r98--goto-fold "complete"))
    (should-not (member '(state . "complete") (org-air-r48--fold-keys)))
    ;; 2
    (let ((pos (car (org-air-r48--doc-positions "complete"))))
      (should pos)
      (should-not (eq 'org-air-face-project-dropped
                      (org-air-r48--title-face-at pos)))
      (should (eq 'org-air-face-title (org-air-r48--title-face-at pos)))))
  ;; 3
  (should (eq 'org-air-face-air-state-complete
              (org-air-project--state-face "complete")))
  (should (equal "C" (org-air-project--state-letter "complete")))
  (should-not (org-air-project--dropped-state-p "complete"))
  ;; 4 — the exclusion is a CHOICE: opting in changes the answer.
  (let ((org-air-project-dropped-states
         (cons "complete" (default-value 'org-air-project-dropped-states))))
    (should (org-air-project--dropped-state-p "complete"))
    (should (eq 'org-air-face-project-dropped
                (org-air-project--doc-row-face "complete")))))

;;;; =====================================================================
;;;; r98-10 — an UNKNOWN state degrades honestly
;;;; =====================================================================

(ert-deftest org-air-r98-10-unknown-state-renders-visible-and-unfolded ()
  "A state org-air has never heard of is shown, not folded and not dimmed.

An unrecognised state is a metadata bug — a typo, or a state Air adds
next year — on a doc that is probably still ALIVE.  Hiding it behind the
terminal fold, or dimming it like abandoned work, would bury exactly the
row a user needs to see to fix it.

Five legs:

  1. the unknown state gets its OWN section, with its docs rendered
     inline;
  2. no fold row is ever keyed on it;
  3. its token is the upcased 5-column truncation (`EXPER'), which is
     how it reads on a TTY;
  4. its face is the generic FADED one — the unknown-state signal — and
     explicitly NOT the terminal-negative face the family resolves to;
  5. its row title band is NOT receded, and it ranks 6: past every live
     state (so it cannot outrank real work) but ahead of every family
     member (so a broken doc sorts above a dead one)."
  (skip-unless (locate-library "org-air"))
  (org-air-r98--with-project 'state
    ;; 1
    (should (member "experimental" (org-air-r98--doc-states-visible)))
    (should (string-match-p "Experimental 1"
                            (buffer-substring-no-properties (point-min)
                                                            (point-max))))
    ;; 2
    (should-not (org-air-r98--goto-fold "experimental"))
    ;; 5, first half
    (let ((pos (car (org-air-r48--doc-positions "experimental"))))
      (should pos)
      (should (eq 'org-air-face-title (org-air-r48--title-face-at pos)))))
  ;; 3
  (should (equal "EXPER" (substring-no-properties
                          (org-air-project--state-token "experimental"))))
  ;; 4
  (should (eq 'org-air-face-faded (org-air-project--state-face "experimental")))
  (should-not (eq 'org-air-face-air-state-dropped
                  (org-air-project--state-face "experimental")))
  (should-not (org-air-project--dropped-state-p "experimental"))
  ;; 5, second half
  (should (= 6 (org-air-project--state-sort-rank "experimental")))
  (dolist (live '("ready" "work-in-progress" "complete" "draft" "out" "off"))
    (should (< (org-air-project--state-sort-rank live)
               (org-air-project--state-sort-rank "experimental"))))
  (dolist (dead '("dropped" "canceled" "cancelled"))
    (should (> (org-air-project--state-sort-rank dead)
               (org-air-project--state-sort-rank "experimental")))))

;;;; =====================================================================
;;;; r98-11 — badges and letters are UNCHANGED (grouping, not identity)
;;;; =====================================================================

(ert-deftest org-air-r98-11-badges-and-letters-unchanged-k-never-c ()
  "The family decides what is CLOSED; it never decides what a state IS.

Nothing about the approved badge vocabulary moved in R98: OUT is still
OUT in OUT's colour on a receded row.  The one ADDITION is the letter
for the two `cancel' spellings, and it is `K' — never `C', which is
already Complete in the per-directory rollup.  A finished doc and an
abandoned one must never print the same letter.

Five legs:

  1. the four pinned tokens are byte-identical to their pre-R98 values;
  2. so are the four pinned badge faces, and out/off/dropped remain
     three DISTINCT faces (the family did not flatten them);
  3. `canceled' and `cancelled' both letter `K' — the same letter as
     each other (one state, two spellings) and never `C';
  4. every letter in the map is distinct except that deliberate pair, so
     the rollup can never print two states the same way by accident;
  5. a family member with no explicit face entry resolves to the
     terminal-negative face rather than the generic faded one — so
     `canceled' reads as finished, not as a metadata bug."
  (skip-unless (locate-library "org-air"))
  ;; 1
  (pcase-dolist (`(,state . ,token) '(("out" . "OUT  ") ("off" . "OFF  ")
                                      ("dropped" . "DROP ")
                                      ("complete" . "COMP ")))
    (ert-info ((format "token %s" state))
      (should (equal token (substring-no-properties
                            (org-air-project--state-token state))))))
  ;; 2
  (should (eq 'org-air-face-air-state-out (org-air-project--state-face "out")))
  (should (eq 'org-air-face-air-state-off (org-air-project--state-face "off")))
  (should (eq 'org-air-face-air-state-dropped
              (org-air-project--state-face "dropped")))
  (should (eq 'org-air-face-air-state-complete
              (org-air-project--state-face "complete")))
  (should (= 3 (length (delete-dups
                        (mapcar #'org-air-project--state-face
                                '("out" "off" "dropped"))))))
  ;; 3
  (should (equal "K" (org-air-project--state-letter "canceled")))
  (should (equal "K" (org-air-project--state-letter "cancelled")))
  (should-not (equal "C" (org-air-project--state-letter "canceled")))
  (should-not (equal "C" (org-air-project--state-letter "cancelled")))
  (should (equal "C" (org-air-project--state-letter "complete")))
  ;; and the ORIGINAL letters are untouched
  (pcase-dolist (`(,state . ,letter) '(("draft" . "D") ("ready" . "R")
                                       ("work-in-progress" . "W")
                                       ("out" . "O") ("off" . "F")
                                       ("dropped" . "X")))
    (ert-info ((format "letter %s" state))
      (should (equal letter (org-air-project--state-letter state)))))
  ;; 4
  (let* ((letters (mapcar #'cdr org-air-project--state-letters))
         (dups (seq-filter (lambda (l) (> (seq-count (lambda (x) (equal x l))
                                                     letters)
                                          1))
                           (seq-uniq letters))))
    (should (equal '("K") dups)))
  ;; 5
  (should (eq 'org-air-face-air-state-dropped
              (org-air-project--state-face "canceled")))
  (should (eq 'org-air-face-air-state-dropped
              (org-air-project--state-face "cancelled")))
  (should-not (eq 'org-air-face-faded (org-air-project--state-face "canceled")))
  ;; …and the two cancel spellings share the 5-col truncation token.
  (should (equal "CANCE" (substring-no-properties
                          (org-air-project--state-token "canceled"))))
  (should (equal "CANCE" (substring-no-properties
                          (org-air-project--state-token "cancelled")))))

;;;; =====================================================================
;;;; r98-12 — the three-tier sort ladder
;;;; =====================================================================

(ert-deftest org-air-r98-12-sort-rank-is-three-tiers-dead-after-broken ()
  "Live states, then UNKNOWN, then the family: dead sorts after broken.

R98 moved `out'/`off' from R80's ranks 3/4 to the END of the live block.
That is not cosmetic: with the fold on, a family member ranking ABOVE a
live `draft' made the fold visibly incoherent — collapsed, the `… N
dropped' row sat at the group BOTTOM; expanded, the very same rows
reappeared mid-list above the drafts.

Four legs:

  1. the exact pinned ladder;
  2. every R80 relation survives — out/off still rank after `complete',
     before unknown, before `dropped';
  3. two dead states never TIE, so the fold's revealed rows have a
     stable order;
  4. and the ladder is what the rendered rows actually obey."
  (skip-unless (locate-library "org-air"))
  ;; 1
  (pcase-dolist (`(,state . ,rank) '(("ready" . 0) ("work-in-progress" . 1)
                                     ("complete" . 2) ("draft" . 3)
                                     ("out" . 4) ("off" . 5)
                                     ("experimental" . 6)
                                     ("dropped" . 7) ("canceled" . 8)
                                     ("cancelled" . 9)))
    (ert-info ((format "rank %s" state))
      (should (= rank (org-air-project--state-sort-rank state)))))
  ;; 2
  (dolist (parked '("out" "off"))
    (should (> (org-air-project--state-sort-rank parked)
               (org-air-project--state-sort-rank "complete")))
    (should (< (org-air-project--state-sort-rank parked)
               (org-air-project--state-sort-rank "experimental")))
    (should (< (org-air-project--state-sort-rank parked)
               (org-air-project--state-sort-rank "dropped"))))
  ;; 3
  (let ((ranks (mapcar #'org-air-project--state-sort-rank
                       '("dropped" "canceled" "cancelled"))))
    (should (equal ranks (seq-uniq ranks))))
  ;; 4 — the rendered order obeys it (directory grouping, fold OFF so
  ;; every row is on screen and the ORDER is what is under test).
  (let ((org-air-r98--collapse-dropped nil))
    (org-air-r98--with-project 'directory
      (let* ((states (org-air-r98--doc-states-visible))
             (ranks (mapcar #'org-air-project--state-sort-rank states)))
        (should (= (length org-air-r98--doc-states) (length states)))
        (should (equal ranks (sort (copy-sequence ranks) #'<)))))))

;;;; =====================================================================
;;;; r98-13 — a folded doc is still COUNTED
;;;; =====================================================================

(ert-deftest org-air-r98-13-dir-rollup-counts-the-whole-family ()
  "The per-directory rollup counts family states the airctl order omits.

A doc behind the fold must still be COUNTED where its siblings are, or
the fold would make work disappear from the only per-directory number
the view prints.  R98 appends any family state the airctl letter order
does not know AFTER that order, so the pinned airctl prefix is
byte-unchanged and the new cells can only follow it.

Four legs:

  1. the airctl-known prefix is exactly what it was — same letters, same
     order, appended cells strictly after;
  2. a `canceled' doc is COUNTED, under `K';
  3. nested counts roll up the same way (`K1(+2)');
  4. an UNRECOGNISED state is still omitted here — the pre-R98 R22-6
     contract the goldens pin — and that is honest because the state has
     its own section, its own token and its own visible row (r98-10)."
  (skip-unless (locate-library "org-air"))
  ;; 1 + 2
  (let ((summary (substring-no-properties
                  (org-air-project--dir-count-summary
                   '(("ready" . 4) ("complete" . 1) ("canceled" . 2))
                   nil))))
    (should (equal "R4 C1 K2" summary)))
  ;; the airctl prefix cannot move: same input minus the family cell.
  (let ((plain (substring-no-properties
                (org-air-project--dir-count-summary
                 '(("ready" . 4) ("complete" . 1)) nil)))
        (withk (substring-no-properties
                (org-air-project--dir-count-summary
                 '(("ready" . 4) ("complete" . 1) ("cancelled" . 3)) nil))))
    (should (equal "R4 C1" plain))
    (should (string-prefix-p plain withk))
    (should (equal "R4 C1 K3" withk)))
  ;; 3
  (should (equal "K1(+2)"
                 (substring-no-properties
                  (org-air-project--dir-count-summary
                   '(("canceled" . 1)) '(("canceled" . 2))))))
  ;; 4
  (should (equal "R1"
                 (substring-no-properties
                  (org-air-project--dir-count-summary
                   '(("ready" . 1) ("experimental" . 9)) nil)))))

;;;; =====================================================================
;;;; r98-14 — the family dims the TITLE BAND, never the badge
;;;; =====================================================================

(ert-deftest org-air-r98-14-family-dims-the-title-band-not-the-badge ()
  "A receded row still reads OUT, in OUT's colour (R98).

`--doc-row-face' is the row's whole-row `font-lock-face'; the cells it
covers keep their own explicit `face'.  That is the mechanism by which a
terminal-negative row visibly recedes WITHOUT the state badge losing its
identity — the difference between grouping and flattening.

Four legs:

  1. every family member's row face is the receded one;
  2. a live state's is not;
  3. rendered, a revealed `out' row carries the receded title face…
  4. …while its badge cell still carries the OUT face — the two are
     different properties on the same row, which is the whole point."
  (skip-unless (locate-library "org-air"))
  ;; 1
  (dolist (state org-air-project-dropped-states)
    (ert-info ((format "row face %s" state))
      (should (eq 'org-air-face-project-dropped
                  (org-air-project--doc-row-face state)))))
  ;; 2
  (dolist (state '("ready" "draft" "complete" "work-in-progress"
                   "experimental"))
    (ert-info ((format "row face %s" state))
      (should (eq 'org-air-face-title
                  (org-air-project--doc-row-face state)))))
  ;; 3 + 4
  (org-air-r98--with-project 'state
    (should (org-air-r98--goto-fold "out"))
    (org-air-project-toggle-dropped)
    (let ((pos (car (org-air-r48--doc-positions "out"))))
      (should pos)
      (should (eq 'org-air-face-project-dropped
                  (org-air-r48--title-face-at pos)))
      ;; the badge cell on the SAME row keeps its own OUT face.
      (let* ((eol (save-excursion (goto-char pos) (line-end-position)))
             (found nil)
             (scan pos))
        (while (and (not found) (< scan eol))
          (let ((face (get-text-property scan 'face)))
            (when (eq face 'org-air-face-air-state-out)
              (setq found scan)))
          (setq scan (1+ scan)))
        (should found)))))

(provide 'org-air-round98-test)
;;; org-air-round98-test.el ends here
