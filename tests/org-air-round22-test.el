;;; org-air-round22-test.el --- substantive ERTs for v0.5 round-22 -*- lexical-binding: t; -*-

;;; Commentary:
;; Test-seat SUBSTANTIVE ERTs for v0.5 round-22
;; (air/v0.5/org-air-round22-design.org).  These cover the behaviour the
;; impl-track round-22 changes introduced but that the existing suites
;; were UNDER-covering — the closeout audit flagged each as a real gap:
;;
;;   R22-1  priorities A..E — only #A rendered on trunk; #B..#E now render
;;          their square + per-level face, an explicit [#B] (=org-default-
;;          priority) is RECORDED not dropped, and a no-cookie heading
;;          stays blank.  THE KEY GAP: no board golden exercises #B..#E,
;;          so this adds a dedicated fixture + render that does.
;;   R22-2  cursor/RET — a row action resolves the item from ANYWHERE on
;;          the line (incl. col 0 / the dead margin / the rail), and
;;          `--normalize-point' snaps point off a dead column onto the
;;          title after native motion / mouse, so RET then succeeds (it
;;          errored "No org-air item" on trunk).
;;   R22-3  dashboard sort — `o' cycles the within-bucket sort
;;          (date->priority->title->recency), `O' reverses, buckets stay
;;          intact, the default (`date') is byte-identical, the indicator
;;          shows only off-default, and the board + project SHARE one pair.
;;   R22-4  scope vs filter clarity — the rail names FILTER (`none' when
;;          empty) vs SOURCE (the dataset chip + `M loaded'); `N of M
;;          shown' appears only when a filter NARROWS; the mode-line reads
;;          `filter none ∙ source ...'.
;;   R22-5  project rail-toggle — `|' pops the rail in the PROJECT (not
;;          just the board) without the board-only crash, and v/V/RET open
;;          a pane showing the doc's whole FILE.
;;   R22-7  contrast — the pane filename/state + the origin column ride the
;;          readable (>= WCAG AA) faces, with the title still strongest.
;;
;; The 21 manifested re-blesses live in their original suites; these are
;; the NEW coverage.  Reuses the R21-6 WCAG contrast helper.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)
(require 'org-air-project-test)            ; project fixture root + render
(require 'org-air-round21-test)            ; R21-6 WCAG contrast helpers

(when (locate-library "org-air")
  (require 'org-air))

;;;; =====================================================================
;;;; R22-1 — priorities A..E (the KEY gap: render #B..#E + the #B parser).
;;;; =====================================================================

(defconst org-air-r22-1--bare-headings
  "* [#A] Alpha bare
* [#B] Bravo bare
* [#C] Charlie bare
* [#D] Delta bare
* [#E] Echo bare
* Foxtrot no cookie
"
  "Bare headings (no `#+TODO:', default Org) for the priority PARSER test.
[#B] = `org-default-priority', the value the old query dropped; #D/#E sit
below `org-priority-lowest' (?C) yet still parse + recover their char.")

(defconst org-air-r22-1--board-org
  (concat "* TODO [#A] Priority A row  :prio:\nDEADLINE: <2026-06-10 Wed>\n"
          "* TODO [#B] Priority B row  :prio:\nDEADLINE: <2026-06-10 Wed>\n"
          "* TODO [#C] Priority C row  :prio:\nDEADLINE: <2026-06-10 Wed>\n"
          "* TODO [#D] Priority D row  :prio:\nDEADLINE: <2026-06-10 Wed>\n"
          "* TODO [#E] Priority E row  :prio:\nDEADLINE: <2026-06-10 Wed>\n"
          "* TODO Plain no-cookie row  :prio:\nDEADLINE: <2026-06-10 Wed>\n")
  "An isolated board whose `attention' (overdue) bucket carries one row per
#A..#E plus a no-cookie row.  All six are overdue (the attention cap is 6),
so the renderer's priority slot is exercised for EVERY level on a SINGLE
bucket (no canonical golden contains #B..#E).")

(defmacro org-air-r22-1--with-priority-board (&rest body)
  "Render the #A..#E board (isolated dir, GUI glyphs, frozen clock); run BODY
in the `*org-air*' buffer.  Not part of the canonical fixture set, so the
25 layout goldens stay byte-identical."
  (declare (indent 0) (debug t))
  `(let ((dir (make-temp-file "org-air-r22-prio-" t)))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name "inbox.org" dir)
             (insert org-air-r22-1--board-org))
           (let ((org-air-files (list dir))
                 (org-air-inbox-file (expand-file-name "no-such-inbox.org" dir)))
             (org-air-viewport-test-as-gui
               (org-air-viewport-test--with-frozen-now
                 (let ((org-air-view-width 160))
                   (org-air)
                   (unwind-protect
                       (with-current-buffer "*org-air*" ,@body)
                     (when (get-buffer "*org-air*")
                       (kill-buffer "*org-air*"))))))))
       (delete-directory dir t))))

(defun org-air-r22-1--row-line (title)
  "Return the first buffer line whose text contains TITLE, properties kept."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward title nil t)
      (buffer-substring (line-beginning-position) (line-end-position)))))

(ert-deftest org-air-r22-1-defaults-and-faces-cover-a-through-e ()
  "R22-1: A..E are first-class — `org-air-priority-show' shows all five,
`org-air-priority-colors' carries a colour for each, the D/E faces exist,
and `org-air-view--priority-face' returns a DISTINCT face per level (none
of A..E collapses onto the `_' fallback that swallowed D/E on trunk)."
  (skip-unless (locate-library "org-air"))
  (dolist (c '(?A ?B ?C ?D ?E))
    (should (memq c org-air-priority-show))
    (should (assq c org-air-priority-colors)))
  (should (facep 'org-air-face-priority-d))
  (should (facep 'org-air-face-priority-e))
  (let ((faces (mapcar #'org-air-view--priority-face '(?A ?B ?C ?D ?E))))
    ;; five levels -> five distinct faces (no two equal).
    (should (= (length faces) (length (delete-dups (copy-sequence faces)))))
    ;; D and E resolve to their OWN faces, not the priority-c fallback.
    (should (eq (org-air-view--priority-face ?D) 'org-air-face-priority-d))
    (should (eq (org-air-view--priority-face ?E) 'org-air-face-priority-e))
    (should-not (eq (org-air-view--priority-face ?D) 'org-air-face-priority-c))
    (should-not (eq (org-air-view--priority-face ?E) 'org-air-face-priority-c))))

(ert-deftest org-air-r22-1-parser-records-explicit-b-and-recovers-d-e ()
  "R22-1 Fault-3 fix (FAILS on trunk for #B): a bare `[#A]'..`[#E]' heading
records its priority and `--priority-char' recovers A..E; in particular an
explicit `[#B]' (= `org-default-priority' ?B) is RECORDED, not dropped to
nil by the old value-equals-default heuristic, and a NO-cookie heading
stays nil (the regexp gate did not over-trigger)."
  (skip-unless (locate-library "org-air"))
  (let ((dir (make-temp-file "org-air-r22-parse-" t)))
    (unwind-protect
        (let ((file (expand-file-name "bare.org" dir)))
          (with-temp-file file (insert org-air-r22-1--bare-headings))
          (let* ((items (org-air-query-items-in-files (list file)))
                 (by-title (lambda (s) (org-air-test-find-item s items))))
            (should (= (length items) 6))
            ;; every explicit cookie recovers its char A..E.
            (pcase-dolist (`(,title . ,char)
                           '(("Alpha bare" . ?A) ("Bravo bare" . ?B)
                             ("Charlie bare" . ?C) ("Delta bare" . ?D)
                             ("Echo bare" . ?E)))
              (let ((item (funcall by-title title)))
                (should item)
                ;; the value is RECORDED (non-nil) — the #B fix.
                (should (org-air-item-priority item))
                (should (equal (org-air-view--priority-char item) char))))
            ;; the no-cookie heading records NO priority (stays blank).
            (let ((plain (funcall by-title "Foxtrot no cookie")))
              (should plain)
              (should (null (org-air-item-priority plain)))
              (should (null (org-air-view--priority-char plain))))))
      (delete-directory dir t))))

(ert-deftest org-air-r22-1-priority-slot-renders-square-and-face-a-through-e ()
  "R22-1 render unit: for EACH shown level #A..#E `--priority-slot' emits a
NON-blank cell — the square glyph carrying that level's face — while a
no-priority row emits the fixed 2-blank slot (the V6 pixel-lock holds, and
all five now render, not only #A)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (let ((sq (org-air-layout-glyph 'priority-square)))
      (dolist (c '(?A ?B ?C ?D ?E))
        (let ((slot (org-air-view--priority-slot c)))
          ;; the square glyph is present (non-blank slot),
          (should (string-match-p (regexp-quote sq) slot))
          ;; carrying THIS level's distinct face.
          (should (eq (get-text-property (string-match (regexp-quote sq) slot)
                                         'face slot)
                      (org-air-view--priority-face c)))))
      ;; no shown priority -> the reserved 2-blank slot (no square).
      (should (equal (org-air-view--priority-slot nil) "  "))
      (should-not (string-match-p (regexp-quote sq)
                                  (org-air-view--priority-slot nil))))))

(ert-deftest org-air-r22-1-board-renders-priority-squares-b-through-e ()
  "R22-1 KEY GAP closer: a real board with one row per #A..#E renders the
priority square on EVERY priority row (B..E included, the trunk regression
where only #A showed), each carrying its level face; the no-cookie row
renders with the BLANK slot (no square)."
  (skip-unless (locate-library "org-air"))
  (org-air-r22-1--with-priority-board
    (let ((sq (org-air-layout-glyph 'priority-square)))
      ;; #A..#E each render their row AND carry the square on that line.
      (pcase-dolist (`(,title . ,char)
                     '(("Priority A row" . ?A) ("Priority B row" . ?B)
                       ("Priority C row" . ?C) ("Priority D row" . ?D)
                       ("Priority E row" . ?E)))
        (let ((line (org-air-r22-1--row-line title)))
          (should line)
          (let ((pos (string-match (regexp-quote sq) line)))
            ;; the square is on the row, BEFORE the title (the slot),
            (should pos)
            (should (< pos (string-match (regexp-quote title) line)))
            ;; and it carries this level's face.
            (should (eq (get-text-property pos 'face line)
                        (org-air-view--priority-face char))))))
      ;; the no-cookie row renders with the blank slot (no square).
      (let ((line (org-air-r22-1--row-line "Plain no-cookie row")))
        (should line)
        (should-not (string-match-p (regexp-quote sq) line))))))

;;;; =====================================================================
;;;; R22-2 — cursor/RET: line-based row resolution + dead-column normalise.
;;;; =====================================================================

(defun org-air-r22-2--divider-pos ()
  "Return the position just RIGHT of the two-pane divider on the current
line, or nil when the line has no divider (board-only)."
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward "[\u2502|]" (line-end-position) t)
      (and (< (point) (line-end-position)) (point)))))

(ert-deftest org-air-r22-2-row-action-resolves-from-dead-col0 ()
  "R22-2 (FAILS on trunk): in the DEFAULT two-pane layout (w120) the
`org-air-item' property does NOT span col 0 (the leading margin), so a
point-ONLY lookup there returns nil and RET errored \"No org-air item\".
The line-based resolver fixes it: at col 0 the property is nil, YET
`--item-at-point' returns THAT row's item and `--context-at-point' is
non-nil (so RET resolves).  Anti-tautology: the failing column is col 0
and the resolved item is the row's title item."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 120
    (org-air-view--goto-first-item)
    (let ((title-item (org-air-view--item-at-point)))
      (should title-item)
      ;; jump to the dead leading margin (col 0).
      (beginning-of-line)
      (should (null (get-text-property (point) 'org-air-item)))
      ;; line-based resolution still finds the row.
      (should (org-air-view--row-property 'org-air-item))
      (should (org-air-view-pane--context-at-point))
      ;; and it is the SAME row's item (RET would act on the title row).
      (should (eq (org-air-view--item-at-point) title-item)))))

(ert-deftest org-air-r22-2-row-action-resolves-from-rail-column ()
  "R22-2: a native/mouse landing in the RAIL columns (right of the divider)
of an item line still resolves the BOARD item left of the divider — the
board item wins over any rail property — so RET/v/r act on the row."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 120
    (org-air-view--goto-first-item)
    (let ((title-item (org-air-view--item-at-point))
          (railpos (org-air-r22-2--divider-pos)))
      (should title-item)
      (should railpos)
      (goto-char railpos)
      ;; the rail column carries no board item under point,
      (should (null (get-text-property (point) 'org-air-item)))
      ;; yet the row resolves to the board item on the LEFT of the line.
      (should (eq (org-air-view--item-at-point) title-item)))))

(ert-deftest org-air-r22-2-normalize-point-snaps-off-dead-column ()
  "R22-2b (clause 3 re-blessed by R46-2): `--normalize-point' (the
post-command snap) moves point off a DEAD column onto the row title and
is a NO-OP when point already sits on the title.  Clause (3) UPDATED to
the R46-2 universal title-band clamp (air/v0.5/org-air-round46-design.org):
the banner top is no longer exempt — EVERY visible row now has a band
(the banner's is its first visible glyph, past the 2-col indent), so the
entry normalize clamps `point-min' (col 0) onto the banner's own text
instead of the pre-R46 no-op that parked point in the indent margin."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 120
    (org-air-view--goto-first-item)
    ;; (1) dead col 0 -> snapped to the title mark.
    (beginning-of-line)
    (should (null (get-text-property (point) 'org-air-item)))
    (org-air-view--normalize-point)
    (should (get-text-property (point) 'org-air-item))
    (should (get-text-property (point) 'org-air-row-title))
    ;; (2) already on the title -> NO-OP (point unchanged).
    (let ((p (point)))
      (org-air-view--normalize-point)
      (should (= (point) p)))
    ;; (3) R46-2: the banner top CLAMPS to its first visible glyph — the
    ;; band start (col 2, past the indent) — never left at col 0.
    (goto-char (point-min))
    (org-air-view--normalize-point)
    (should (> (current-column) 0))
    (should (= (point) (save-excursion
                         (goto-char (point-min))
                         (org-air-view--beginning-of-visible)
                         (point))))
    ;; ...and the clamp is idempotent there (in-band -> NO-OP).
    (let ((p (point)))
      (org-air-view--normalize-point)
      (should (= (point) p)))))

(ert-deftest org-air-r22-2-normalize-point-composes-with-restore ()
  "R22-2b composes with R21-1: when point is on a PROPERTIZED title column
\(the column R21-1 restores), `--normalize-point' leaves it byte-identical
— it never drags point back to col 0, so a deliberate in-title position
survives a re-render + the post-command hook."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 120
    (org-air-view--goto-first-item)
    ;; move a few columns INTO the title (still a propertized item column).
    (forward-char 3)
    (should (get-text-property (point) 'org-air-item))
    (let ((p (point)))
      (org-air-view--normalize-point)
      (should (= (point) p)))))

(ert-deftest org-air-r22-2-board-only-col0-unaffected ()
  "R22-2 (contract UPDATED by R29-2): in the BOARD-ONLY layout (w80 <
`org-air-rail-min-width') col 0 ALREADY carries `org-air-item', so the
RESOLVER works from col 0 unchanged — but the R29-2 dead zone is no
longer property-only: col 0 sits BEFORE the row's title mark (the
gutter), so the entry/line-motion snap now lands point ON the title
\(this exact shape — property-covered col 0 under board-only/side-window
composition — was where the R22-2 predicate was provably dead and the
cursor stuck at column 0).  A SAME-LINE command (snapshot recorded on
this line) still never moves point — in-row motion is not hijacked."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 80
    (org-air-view--goto-first-item)
    (beginning-of-line)
    ;; board-only: the row property reaches col 0...
    (should (get-text-property (point) 'org-air-item))
    (should (org-air-view--item-at-point))
    ;; ...and col 0 is in the R29-2 dead zone (before the title mark), so
    ;; the entry-semantics snap (no pre-command snapshot) lands the title.
    (setq org-air-view--pre-command-line nil)
    (org-air-view--normalize-point)
    (should (get-text-property (point) 'org-air-row-title))
    ;; a SAME-LINE command never hijacks: gutter position survives.
    (beginning-of-line)
    (setq org-air-view--pre-command-line (line-number-at-pos))
    (let ((p (point)))
      (org-air-view--normalize-point)
      (should (= (point) p)))))

;;;; =====================================================================
;;;; R22-3 — dashboard sort (o/O), within buckets, shared with the project.
;;;; =====================================================================

(defun org-air-r22-3--mk (title &optional priority)
  "A minimal board item with TITLE and optional PRIORITY value."
  (org-air-item-create :title title :priority priority))

(ert-deftest org-air-r22-3-sort-default-is-date-and-byte-identical ()
  "R22-3: the board seeds the shared sort spec — keys
`(date priority title recency)', default key `date', direction
`ascending' — and the default `date' reproduces the historical
within-bucket order EXACTLY (only attention/upcoming were date-sorted;
the rest keep query order), so the goldens stay byte-identical."
  (skip-unless (locate-library "org-air"))
  (with-temp-buffer
    (org-air-view-mode)
    (should (equal org-air-view--sort-keys '(date priority title recency)))
    (should (eq org-air-view--sort-key 'date))
    (should (eq org-air-view--sort-direction 'ascending))
    (should (org-air-view--sort-default-p))
    (let ((items (list (org-air-r22-3--mk "c") (org-air-r22-3--mk "a")
                       (org-air-r22-3--mk "b"))))
      ;; date order on a non-attention bucket = identity (query order).
      (should (equal (mapcar #'org-air-item-title
                             (org-air-view--sort-items items 'stale))
                     '("c" "a" "b"))))))

(ert-deftest org-air-r22-3-sort-cycle-advances-keys ()
  "R22-3: `org-air-view-sort-cycle' (bound to `o') advances the active key
date->priority->title->recency->date and calls the per-view refresh fn."
  (skip-unless (locate-library "org-air"))
  (with-temp-buffer
    (org-air-view-mode)
    (let ((refreshed 0))
      (setq org-air-view--sort-refresh (lambda () (cl-incf refreshed)))
      (let (seen)
        (dotimes (_ 4) (org-air-view-sort-cycle) (push org-air-view--sort-key seen))
        (should (equal (nreverse seen) '(priority title recency date))))
      (should (= refreshed 4)))))

(ert-deftest org-air-r22-3-sort-within-buckets-priority-title-recency ()
  "R22-3: `--sort-items' orders WITHIN a bucket by the active key — priority
puts #A before #C before a cookie-less item; title alphabetises — never
reordering the bucket itself."
  (skip-unless (locate-library "org-air"))
  (with-temp-buffer
    (org-air-view-mode)
    (let ((items (list (org-air-r22-3--mk "Charlie" 0)      ; #C
                       (org-air-r22-3--mk "None" nil)       ; no cookie
                       (org-air-r22-3--mk "Alpha" 2000))))  ; #A
      ;; priority ascending = #A (hottest) first, no-cookie last.
      (setq org-air-view--sort-key 'priority)
      (should (equal (mapcar #'org-air-item-title
                             (org-air-view--sort-items items 'stale))
                     '("Alpha" "Charlie" "None")))
      ;; title ascending = alphabetical.
      (setq org-air-view--sort-key 'title)
      (should (equal (mapcar #'org-air-item-title
                             (org-air-view--sort-items items 'stale))
                     '("Alpha" "Charlie" "None"))))))

(ert-deftest org-air-r22-3-sort-reverse-flips-within-bucket ()
  "R22-3: `org-air-view-sort-reverse' (bound to `O') flips the direction,
reversing the within-bucket order with a stable tiebreak."
  (skip-unless (locate-library "org-air"))
  (with-temp-buffer
    (org-air-view-mode)
    (setq org-air-view--sort-refresh #'ignore
          org-air-view--sort-key 'priority)
    (let ((items (list (org-air-r22-3--mk "Charlie" 0)
                       (org-air-r22-3--mk "Alpha" 2000))))
      (should (eq org-air-view--sort-direction 'ascending))
      (should (equal (mapcar #'org-air-item-title
                             (org-air-view--sort-items items 'stale))
                     '("Alpha" "Charlie")))
      (org-air-view-sort-reverse)
      (should (eq org-air-view--sort-direction 'descending))
      (should (equal (mapcar #'org-air-item-title
                             (org-air-view--sort-items items 'stale))
                     '("Charlie" "Alpha"))))))

(ert-deftest org-air-r22-3-sort-buckets-intact-across-keys ()
  "R22-3: sorting NEVER changes bucket membership — `--sort-items' is a pure
within-bucket PERMUTATION, so for every key the OUTPUT is the same SET of
items as the input (only the order differs).  This is the `buckets intact'
guarantee at its source: a sort can reorder a bucket but never add, drop or
move an item to another bucket."
  (skip-unless (locate-library "org-air"))
  (with-temp-buffer
    (org-air-view-mode)
    (let* ((items (list (org-air-r22-3--mk "Charlie" 0)
                        (org-air-r22-3--mk "None" nil)
                        (org-air-r22-3--mk "Alpha" 2000)
                        (org-air-r22-3--mk "Bravo" 1000)))
           (members (sort (mapcar #'org-air-item-title (copy-sequence items))
                          #'string-lessp)))
      (dolist (key '(date priority title recency))
        (setq org-air-view--sort-key key)
        (dolist (dir '(ascending descending))
          (setq org-air-view--sort-direction dir)
          (let ((got (org-air-view--sort-items items 'attention)))
            ;; same length (nothing dropped/duplicated),
            (should (= (length got) (length items)))
            ;; and the SAME membership set (a pure permutation).
            (should (equal (sort (mapcar #'org-air-item-title got)
                                 #'string-lessp)
                           members))))))))

(ert-deftest org-air-r22-3-sort-shared-board-and-project ()
  "R22-3: ONE sort pair drives both views — `o'/`O' resolve to
`org-air-view-sort-cycle'/`-reverse' in the board AND project maps (via the
shared `org-air-view-core-map'), and the shared indicator builder reflects
the active key + direction glyph."
  (skip-unless (locate-library "org-air"))
  ;; both maps inherit the same pair from the core.
  (dolist (map (list org-air-view-mode-map org-air-project-mode-map))
    (should (eq (lookup-key map (kbd "o")) 'org-air-view-sort-cycle))
    (should (eq (lookup-key map (kbd "O")) 'org-air-view-sort-reverse)))
  (should (eq (lookup-key org-air-view-core-map (kbd "o")) 'org-air-view-sort-cycle))
  (should (eq (lookup-key org-air-view-core-map (kbd "O")) 'org-air-view-sort-reverse))
  ;; the shared indicator reflects key + direction.
  (let ((asc (substring-no-properties
              (org-air-view--sort-indicator-text 'priority 'ascending)))
        (desc (substring-no-properties
               (org-air-view--sort-indicator-text 'title 'descending))))
    (should (string-match-p "priority" asc))
    (should (string-match-p "title" desc))
    ;; ascending and descending carry a DIFFERENT direction glyph.
    (should-not (equal asc desc))))

(ert-deftest org-air-r22-3-sort-indicator-only-off-default ()
  "R22-3: the within-bucket sort indicator is GATED on a non-default sort —
`--sort-default-p' is t for the byte-identical default (date ascending), so
the DEFAULT board render carries no indicator glyph (the goldens stay
clean); it is nil for any cycled key/direction, and the shared builder then
produces the `<glyph> <key> <dir>' text.  (The banner gates the indicator
on exactly this predicate.)"
  (skip-unless (locate-library "org-air"))
  ;; the GUI glyph + the default board render + the off-default builder all
  ;; share the same `as-gui' display so the glyph matches the render.
  (org-air-viewport-test-as-gui
    (let ((mk (org-air-layout-glyph 'sort-key)))
      ;; the DEFAULT board render is indicator-free (so the goldens hold).
      (org-air-viewport-test-with-dashboard 160
        (should (org-air-view--sort-default-p))
        (should-not (string-match-p (regexp-quote mk)
                                    (substring-no-properties (buffer-string)))))
      ;; off-default -> the builder yields the `<glyph> <key> <dir>' badge.
      (let ((txt (substring-no-properties
                  (org-air-view--sort-indicator-text 'priority 'descending))))
        (should (string-match-p (concat (regexp-quote mk) " priority") txt)))))
  ;; the gating predicate: default -> t (no indicator); off-default -> nil.
  (with-temp-buffer
    (org-air-view-mode)
    (should (org-air-view--sort-default-p))             ; date / ascending
    (setq org-air-view--sort-key 'priority)
    (should-not (org-air-view--sort-default-p))         ; non-default key
    (setq org-air-view--sort-key 'date
          org-air-view--sort-direction 'descending)
    (should-not (org-air-view--sort-default-p))))        ; non-default dir

;;;; =====================================================================
;;;; R22-4 — scope vs filter clarity: SOURCE/dataset vs the live FILTER.
;;;; =====================================================================

(defun org-air-r22-4--rail-filters-text (items filter)
  "Render the Filter+Source rail block for ITEMS under FILTER, wide; return
its plain text (a generous width so nothing truncates the count cues)."
  (with-current-buffer (get-buffer-create " *org-air-r22-4*")
    (erase-buffer)
    (org-air-view-mode)
    (setq buffer-read-only nil
          org-air-view--items items
          org-air-view--tag-filter filter
          org-air-filter-match 'all)
    (org-air-view--insert-rail-filters 72)
    (prog1 (substring-no-properties (buffer-string))
      (kill-buffer (current-buffer)))))

(ert-deftest org-air-r22-4-source-and-filter-named-distinctly ()
  "R22-4: the rail names the two roles UNMISTAKABLY — the empty FILTER reads
`none' (it drops the old `all items' dataset claim) and the SOURCE block
carries the dataset chip + a `M loaded' count; neither now reads the bare
duplicated `all items', and the mode-line mirrors it as
`filter none ∙ source ...'."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 160
      (let* ((items org-air-view--items)
             (text (org-air-r22-4--rail-filters-text items nil)))
        ;; empty filter -> `none' (no dataset claim), no old wording.
        (should (string-match-p "\\bnone\\b" text))
        (should-not (string-match-p "No filters" text))
        ;; Source block: dataset chip + the loaded count.
        (should (string-match-p "Source" text))
        ;; R33-1: chrome separator swapped U+00B7 (ambiguous) -> U+2219.
        (should (string-match-p "all items \u2219 [0-9]+ loaded" text))
        ;; unfiltered carries NO narrowing count.
        (should-not (string-match-p "[0-9]+ of [0-9]+ shown" text)))))
  ;; the mode-line says `filter none ∙ source ...' (not `scope ... all items').
  (with-temp-buffer
    (org-air-view-mode)
    (setq org-air-view--mode-line-count 3
          org-air-view--tag-filter nil
          org-air-view--scope nil)
    (let ((s (org-air-view--mode-line-content)))
      (should (string-match-p "filter none" s))
      (should (string-match-p "source all items" s))
      (should-not (string-match-p "scope all items" s)))))

(ert-deftest org-air-r22-4-n-of-m-shown-only-when-filter-narrows ()
  "R22-4: the `N of M shown' count appears ONLY when a live FILTER actually
narrows the dataset (shown < loaded) — present with an active tag filter,
absent when no filter is set."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 160
      ;; R24-6: a `#tag' filter token tag-matches AND shows verbatim (#work).
      (let* ((items org-air-view--items)
             (narrowed (org-air-r22-4--rail-filters-text items '("#work")))
             (open     (org-air-r22-4--rail-filters-text items nil)))
        ;; a narrowing filter reports `N of M shown' (and the chip + clear).
        (should (string-match-p "[0-9]+ of [0-9]+ shown" narrowed))
        (should (string-match-p "#work" narrowed))
        ;; no filter -> no narrowing count.
        (should-not (string-match-p "[0-9]+ of [0-9]+ shown" open))))))

;;;; =====================================================================
;;;; R22-5 — project rail-toggle (|) generalised + pane shows the doc file.
;;;; =====================================================================

(ert-deftest org-air-r22-5-rail-toggle-bound-in-board-and-project ()
  "R22-5: `|' is promoted into the shared `org-air-view-core-map', so BOTH
the board and the project resolve it to `org-air-rail-toggle'; v/V also
resolve to the shared pane commands in the project (it inherits the pane +
rail-toggle set).  R26-3 re-bless: RET forks deliberately — the board
keeps the shared pane-return, the project's RET is the same-window
`org-air-project-open' (v/V/| stay shared)."
  (skip-unless (locate-library "org-air"))
  (should (eq (lookup-key org-air-view-core-map (kbd "|")) 'org-air-rail-toggle))
  (dolist (map (list org-air-view-mode-map org-air-project-mode-map))
    (should (eq (lookup-key map (kbd "|")) 'org-air-rail-toggle))
    (should (eq (lookup-key map (kbd "v")) 'org-air-view-pane))
    (should (eq (lookup-key map (kbd "V")) 'org-air-view-pane-close)))
  ;; R26-3: RET — pane-return on the board, same-window open in the project.
  (should (eq (lookup-key org-air-view-mode-map (kbd "RET"))
              'org-air-view-pane-return))
  (should (eq (lookup-key org-air-project-mode-map (kbd "RET"))
              'org-air-project-open)))

(ert-deftest org-air-r22-5-rail-toggle-pops-rail-in-project-no-crash ()
  "R22-5 (errored on trunk): `org-air-rail-toggle' in a PROJECT buffer flips
the shared `org-air-view--rail-popped-out' flag and re-renders WITHOUT the
old board-only `user-error' (\"Not in an org-air board buffer\"); toggling
back restores inline.  The rail inspects a DOC, so the generalised path
must not hit an item-only accessor crash."
  (skip-unless (locate-library "org-air"))
  (let ((org-air-project-view-width 120))
    (org-air-project-test--render
     ;; the project never seeds the flag during a normal render.
     (should (eq org-air-view--rail-popped-out 'unset))
     ;; first toggle pops OUT (no crash), flips the flag to t.
     (org-air-rail-toggle)
     (should (eq org-air-view--rail-popped-out t))
     ;; toggling back pops IN, flag nil.
     (org-air-rail-toggle)
     (should (null org-air-view--rail-popped-out)))))

(ert-deftest org-air-r22-5-pane-shows-the-doc-whole-file ()
  "R22-5: v/V/RET-pane on a project DOC row resolve the doc's whole FILE —
`--context-at-point' returns the doc file as both :file and :marker, and
`--source-buffer-pos' opens that file at pos nil (whole-file, widened, from
the top) — so RET opens a pane showing the DOC, not an item subtree."
  (skip-unless (locate-library "org-air"))
  (let ((org-air-project-view-width 120))
    (org-air-project-test--render
     ;; land on the first doc row.
     (goto-char (or (text-property-not-all (point-min) (point-max)
                                           'org-air-doc nil)
                    (point-min)))
     (let* ((doc (org-air-view--row-property 'org-air-doc))
            (ctx (org-air-view-pane--context-at-point)))
       (should doc)
       (should ctx)
       ;; the context resolves the DOC's file (not an item marker).
       (should (equal (plist-get ctx :file) (org-air-doc-file doc)))
       (should (equal (plist-get ctx :marker) (org-air-doc-file doc)))
       (should (equal (plist-get ctx :title) (org-air-doc-name doc)))
       ;; the source resolves to the file buffer at pos nil = whole file.
       (let ((bp (org-air-view-pane--source-buffer-pos (plist-get ctx :marker))))
         (should (bufferp (car bp)))
         (should (null (cdr bp)))            ; pos nil = whole file, from the top
         ;; the resolved buffer IS the doc's file buffer (robust to the
         ;; test's path-abbrev: same path -> same `find-file-noselect' buffer).
         (should (eq (car bp) (find-file-noselect (org-air-doc-file doc))))
         (when (buffer-live-p (car bp)) (kill-buffer (car bp))))))))

;;;; =====================================================================
;;;; R22-6 — project by-directory grouping: ONE header/dir, quiet
;;;; letter-counts (airctl status -Da parity; LEFT-anchored next to the
;;;; name since R52-1), real nesting indent.
;;;; =====================================================================

(defun org-air-r22-6--dir-lines ()
  "Render the by-directory project view (fixture, w100); return its lines."
  (org-air-project-test--render-lines
   'org-air-project-group-by-directory 100))

(ert-deftest org-air-r22-6-dir-count-summary-matches-airctl ()
  "R22-6 airctl `-Da' PARITY: `org-air-project--dir-count-summary' renders
the quiet letter-count summary numerically identical to `airctl status -Da'
on the real Air repo — v0.1/'s airctl cells `Ready 4 (+1)  Complete 14
(+14)  Dropped 1 (+9)  Draft 2 (+8)' become `R4(+1) C14(+14) X1(+9)
D2(+8)' (own count + faded `(+M)' nested roll-up).  States absent from BOTH
direct and descendant counts are omitted; the display order is
`org-air-project--state-display-order'."
  (skip-unless (locate-library "org-air"))
  ;; the v0.1/ numbers verified against `airctl status -Da' (the r20-5-fix
  ;; guard): own R4 C14 X1 D2, descendants +1 +14 +9 +8.
  (should (equal (substring-no-properties
                  (org-air-project--dir-count-summary
                   '(("ready" . 4) ("complete" . 14) ("dropped" . 1) ("draft" . 2))
                   '(("ready" . 1) ("complete" . 14) ("dropped" . 9) ("draft" . 8))))
                 "R4(+1) C14(+14) X1(+9) D2(+8)"))
  ;; a leaf dir (no descendants) shows own counts only, no `(+M)'.
  (should (equal (substring-no-properties
                  (org-air-project--dir-count-summary
                   '(("work-in-progress" . 1) ("dropped" . 1) ("draft" . 1))
                   nil))
                 "W1 X1 D1"))
  ;; a pure roll-up state (direct 0) shows only the `(+M)'.
  (should (equal (substring-no-properties
                  (org-air-project--dir-count-summary nil '(("draft" . 1))))
                 "D(+1)"))
  ;; states absent from both are omitted entirely.
  (should (equal (org-air-project--dir-count-summary nil nil) "")))

(ert-deftest org-air-r22-6-one-header-per-directory ()
  "R22-6: ONE header per directory — the OLD doubled top-dir header (a
rolled-up box header PLUS a per-dir count heading on adjacent lines) is
gone, so each version dir's `| v0.N/' header line appears EXACTLY ONCE
(the doc-row origin cells carry `. v0.N/...', never the `| v0.N/' marker)."
  (skip-unless (locate-library "org-air"))
  (let ((lines (org-air-r22-6--dir-lines)))
    (should lines)
    (dolist (rx '("| v0\\.1/" "| v0\\.2/"))
      (should (= 1 (cl-count-if (lambda (l) (string-match-p rx l)) lines))))
    ;; the OLD `[R] Ready (N)' state-NAME rolled-up header is gone entirely.
    (should-not (cl-some (lambda (l)
                           (string-match-p "\\[[RWCXD]\\] [A-Z][a-z]+ ([0-9]" l))
                         lines))))

(ert-deftest org-air-r22-6-count-summaries-left-anchored ()
  "R22-6 rollup coverage, INVERTED in place by R52-1 (renamed from
`org-air-r22-6-count-summaries-right-aligned'; never silent): the per-dir
letter-count summary is LEFT-ANCHORED, exactly TWO columns after its
`dir/' name, so the rollup reads as the name's own annotation — no longer
RIGHT-justified to the pane width, where on wide frames it floated out by
the date column, visually detached from the name it summarizes
\(air/v0.5/org-air-round52-design.org supersedes the R22-6 rationale that
sibling headers share a vertical summary column).  Rendered without the
rail at width W: the v0.1/ and v0.2/ headers are NO LONGER W wide
\(right-trimmed width < W), each header ENDS with its summary, and the
summary starts exactly TWO columns after the name's `/'.  The same-length
names now put both summaries at the SAME start column while their right
edges DIFFER — the exact inversion of the old flush-right proof."
  (skip-unless (locate-library "org-air"))
  (let* ((w 80)
         (docs (org-air-project--collect-docs org-air-project-test-root))
         (tree (org-air-project--directory-tree docs))
         (lines (org-air-test-with-frozen-project-path org-air-project-test-root
                  (org-air-project-test--with-frozen-mtime
                   (with-temp-buffer
                     (org-air-project--insert-directory-tree tree w)
                     (mapcar #'substring-no-properties
                             (split-string (buffer-string) "\n"))))))
         (pick (lambda (rx) (cl-find-if (lambda (l) (string-match-p rx l)) lines)))
         (h1 (string-trim-right (or (funcall pick "| v0\\.1/") "")))
         (h2 (string-trim-right (or (funcall pick "| v0\\.2/") "")))
         (sum-start (lambda (l)
                      (and (string-match "  \\([RWCXDUV][0-9(].*[0-9)]\\)$" l)
                           (match-beginning 1))))
         (name-end (lambda (l rx) (and (string-match rx l) (match-end 0)))))
    (should-not (string-empty-p h1))
    (should-not (string-empty-p h2))
    ;; headers are NO LONGER justified to W: they END after the summary.
    (should (< (length h1) w))
    (should (< (length h2) w))
    ;; each header ENDS with its summary (right-trimmed tail is the rollup).
    (should (string-match-p "[0-9)]\\'" h1))
    (should (string-match-p "[0-9)]\\'" h2))
    ;; the summary starts exactly TWO columns after the name's `/'.
    (let ((s1 (funcall sum-start h1)) (s2 (funcall sum-start h2))
          (n1 (funcall name-end h1 "v0\\.1/"))
          (n2 (funcall name-end h2 "v0\\.2/")))
      (should s1) (should s2) (should n1) (should n2)
      (should (= s1 (+ n1 2)))
      (should (= s2 (+ n2 2)))
      ;; SAME-length names -> SAME summary start column, while the
      ;; DIFFERENT-length summaries give DIFFERENT right edges — the exact
      ;; inversion of the superseded flush-right proof.
      (should (= s1 s2))
      (should-not (= (length h1) (length h2))))))

(ert-deftest org-air-r22-6-nesting-indents-deepen ()
  "R22-6/R23-3 re-bless: real tree nesting — a CHILD dir header's NAME sits
DEEPER (higher column) than its parent dir's name, and a doc row hangs one
level DEEPER still, so the NAME / content columns strictly increase
parent -> child -> doc.  R23-3 leads child dirs with a tree CONNECTOR
\(`+- ' in batch, faded `box-bottom-left'/`box-tee-left' + `box-horizontal')
at the SAME leading column as the top-dir marker, so the pure leading-SPACE
no longer distinguishes parent from child — the metric is now the NAME
column (the connector pushes the child name one column past the parent's)."
  (skip-unless (locate-library "org-air"))
  (let* ((lines (org-air-r22-6--dir-lines))
         (pick (lambda (rx) (cl-find-if (lambda (l) (string-match-p rx l)) lines)))
         (col-of (lambda (l rx) (and l (string-match rx l))))
         ;; the parent v0.1/ header, its nested air-context/ child header,
         ;; and the child's own doc row (Gamma lives under air-context/).
         (parent (funcall pick "| v0\\.1/"))
         (child  (funcall pick "\\+- air-context/"))
         ;; R26-2: the Gamma doc row carries the DRAFT word cell now.
         (doc    (funcall pick "DRAFT Gamma context"))
         (parent-col (funcall col-of parent "v0\\.1/"))
         (child-col  (funcall col-of child "air-context/"))
         (doc-col    (funcall col-of doc "DRAFT")))
    (should parent) (should child) (should doc)
    ;; child dir NAME column is deeper than its parent dir NAME column,
    (should (> child-col parent-col))
    ;; and the child's doc-row content is deeper still than the child header.
    (should (> doc-col child-col))
    ;; R23-3: the child is led by a tree CONNECTOR (batch `+-'), never the
    ;; `|' rail-marker that is reserved for top dirs.
    (should (string-match-p "^ *\\+- air-context/" child))
    (should-not (string-match-p "^ *| air-context/" child))))

;;;; =====================================================================
;;;; R22-7 — pane filename/state + origin column contrast (WCAG AA).
;;;; Reuses the R21-6 WCAG helper (`org-air-r21-6--contrast' / `--face-attr').
;;;; =====================================================================

(ert-deftest org-air-r22-7-pane-header-filename-and-state-pass-aa ()
  "R22-7 (FAILS on trunk): the view-pane header FILENAME + STATE segments
ride the readable mid-tier `org-air-face-inspector-label' (the pane-header
base), so their foreground clears WCAG AA (>= 4.5) on the pane-header bg in
BOTH themes (2.15/2.45 -> 6.02/8.32), no longer the sub-AA faded tone."
  (skip-unless (locate-library "org-air"))
  ;; the segments are faced with the readable base, not faded.
  (let ((hl (org-air-view-pane--header-line
             (list :file "/x/foo.org" :title "A heading" :state "TODO"))))
    (should (eq (get-text-property (string-match "foo" hl) 'face hl)
                'org-air-face-inspector-label))
    (should (eq (get-text-property (string-match "TODO" hl) 'face hl)
                'org-air-face-inspector-label))
    (should-not (eq (get-text-property (string-match "foo" hl) 'face hl)
                    'org-air-face-faded)))
  ;; measured: that face clears AA on the pane-header bg, both themes.
  (dolist (mode '(light dark))
    (let ((fg (org-air-r21-6--face-attr 'org-air-face-inspector-label
                                        :foreground mode))
          (bg (org-air-r21-6--face-attr 'org-air-face-pane-header
                                        :background mode)))
      (ert-info ((format "pane filename/state %s fg=%s bg=%s" mode fg bg))
        (should fg) (should bg)
        (should (>= (org-air-r21-6--contrast fg bg) 4.5))))))

(ert-deftest org-air-r22-7-pane-title-is-strongest ()
  "R22-7: the pane TITLE stays the single strongest segment — its
foreground clears AA on the header bg AND its contrast is strictly GREATER
than the filename's (the title remains the salient element)."
  (skip-unless (locate-library "org-air"))
  (dolist (mode '(light dark))
    (let* ((bg    (org-air-r21-6--face-attr 'org-air-face-pane-header :background mode))
           (title (org-air-r21-6--face-attr 'org-air-face-pane-title :foreground mode))
           (file  (org-air-r21-6--face-attr 'org-air-face-inspector-label :foreground mode)))
      (ert-info ((format "%s bg=%s title=%s file=%s" mode bg title file))
        (should bg) (should title) (should file)
        (should (>= (org-air-r21-6--contrast title bg) 4.5))
        ;; strictly stronger than the (readable) filename.
        (should (> (org-air-r21-6--contrast title bg)
                   (org-air-r21-6--contrast file bg)))))))

(ert-deftest org-air-r22-7-origin-passes-aa-and-quieter-than-title ()
  "R22-7: the row ORIGIN cell rides the dedicated `org-air-face-origin'
mid-tier — its foreground clears AA (>= 4.5) on the board default bg in
both themes (2.48/2.72 -> >= 6), while staying QUIETER than (<=) the row
title, so the origin is legible but not loud."
  (skip-unless (locate-library "org-air"))
  (dolist (mode '(light dark))
    (let ((origin (org-air-r21-6--face-attr 'org-air-face-origin :foreground mode))
          (bg     (org-air-palette-color 'background mode))
          (title  (org-air-palette-color 'foreground mode)))
      (ert-info ((format "origin %s fg=%s bg=%s title=%s" mode origin bg title))
        (should origin) (should bg) (should title)
        (should (>= (org-air-r21-6--contrast origin bg) 4.5))
        ;; quieter than the title (the row's salient metadata stays calm).
        (should (<= (org-air-r21-6--contrast origin bg)
                    (org-air-r21-6--contrast title bg)))))))

(ert-deftest org-air-r22-7-pane-header-text-is-faces-only ()
  "R22-7 byte-invisible: the header-line VISIBLE text (properties stripped)
is the unchanged `<icon> <file> <dot> <title> <dot> <state>' contract — the
contrast fix is faces only, so the pane byte golden (which strips the
header-line) holds.  Glyphs are read from the table (GUI icon/dot vs the
TTY fallback) so the assertion does not rot in --batch (a TTY)."
  (skip-unless (locate-library "org-air"))
  (let* ((icon (org-air-view--glyph 'view-pane))
         (dot (org-air-view--glyph 'sep-dot))
         (hl (substring-no-properties
              (org-air-view-pane--header-line
               (list :file "/x/foo.org" :title "A heading" :state "TODO"))))
         (expected (concat icon " foo.org  " dot "  A heading  " dot "  TODO")))
    (should (equal hl expected))
    ;; the three segments survive in order (the text contract).
    (should (string-match-p "foo\\.org" hl))
    (should (string-match-p "A heading" hl))
    (should (string-match-p "TODO" hl))))

(provide 'org-air-round22-test)
;;; org-air-round22-test.el ends here
