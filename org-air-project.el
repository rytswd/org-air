;;; org-air-project.el --- Air-docs project tree view for org-air -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: org-air contributors
;; Keywords: outlines, files
;; Version: 0.1.0
;; URL: https://github.com/rytswd/org-air
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; F5: render an Air-managed documentation tree (like `airctl status') as
;; a separate view from the GTD task board.  Air docs carry a state
;; (draft/ready/work-in-progress/complete/dropped), tags and a
;; title; this module reads them and (D-P5) renders them through
;; org-air-view's shared row primitive (`org-air-view--insert-row'): state
;; buckets become sections (icon + title + count badge), each doc a row
;; with the title LEFT and a date / tags / path cluster (the same D-P1
;; svg pills as the board).  Group by state / directory / tag.  The view
;; is buffer TEXT so it is fully byte-testable; the state badge degrades
;; from emoji to a `[D]'-style label off a graphical frame.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-air-faces)
(require 'org-air-layout)
(require 'org-air-view)
(require 'org-air-calendar)

;;;; ---------------------------------------------------------------------
;;;; Configuration (F5a)
;;;; ---------------------------------------------------------------------

(defcustom org-air-sources nil
  "Where org-air finds content.
A list of entries, each either:
 - a STRING directory of Org files (a GTD task board), or
 - (:air ROOT)  an Air project root rendered as a doc tree, or
 - (:files (F...)) explicit files.
Air projects are also AUTO-DETECTED: a source directory that contains
`air-config.toml' or an `air/' subdirectory is treated as (:air ...).
The existing `org-air-files'/`org-air-inbox-file' (the GTD board) remain;
`org-air-sources' is the newer unified entry point feeding both the task
board (Org dirs) and the project view (Air roots)."
  :type '(repeat sexp)
  :group 'org-air)

(defcustom org-air-projects nil
  "Explicit list of Air project roots for `org-air-project'.
When nil the roots are derived from `org-air-sources' (its `:air' entries
and auto-detected directories)."
  :type '(repeat directory)
  :group 'org-air)

(defcustom org-air-project-view-width nil
  "Fixed render width for the Air project view, or nil to use the window.
Integer pins an exact composition width (the batch/test seam, mirroring
`org-air-view-width'); nil derives the width from the live window."
  :type '(choice (const :tag "Live window" nil) integer)
  :group 'org-air)

(defcustom org-air-project-show-inspector t
  "When non-nil, the project view hosts a mid-rail inspector (R14 D-P1.B).
Mirrors `org-air-show-inspector' for the board: above
`org-air-rail-min-width' the view is two-pane (doc sections + a project
rail of Summary + Inspector); below it the view is board-only."
  :type 'boolean
  :group 'org-air)

(defcustom org-air-project-group 'directory
  "Default grouping for the Air project view: `state', `directory' or `tag'.
Mirrors `airctl status' -a / -Da / -Ta.  R20-5: the default is `directory'
— the NESTED directory tree that matches `airctl status -Da' (the most
useful view).  The `state' / `tag' modes stay reachable via the commands
`org-air-project-group-by-state' / `org-air-project-group-by-tag' (no key),
so they never shadow the shared board keys s / d / t."
  :type '(choice (const state) (const directory) (const tag))
  :group 'org-air)

(defconst org-air-project--state-display-order
  '("ready" "work-in-progress" "complete" "dropped" "draft")
  "Canonical state order for the directory tree (R20-5), matching `airctl -Da'.
Drives BOTH the per-dir count badges (Ready · Work-In-Progress · Complete ·
Dropped · Draft, only those present) AND the state-first ordering of a
directory's own docs, so the two can never drift.  R25-3 dropped the
phantom `review' state (Air has no such state).")

(defcustom org-air-project-sort-key 'name
  "INITIAL sort key for the Air project view (R16 D-P4).
Keys: `name', `created', `updated' (reserved for later: scheduled,
deadline).  The runtime commands (`o' cycle, `g s' select) override this
per-buffer via `org-air-project--sort-key'.  The resulting doc order is
the byte contract."
  :type '(choice (const name) (const created) (const updated))
  :group 'org-air)

(defcustom org-air-project-sort-direction 'ascending
  "INITIAL sort direction for the Air project view (R16 D-P4).
`O' toggles it per-buffer via `org-air-project--sort-direction'."
  :type '(choice (const ascending) (const descending))
  :group 'org-air)

(defcustom org-air-project-states
  '("draft" "ready" "work-in-progress" "complete" "dropped")
  "Air doc states in display order.
The canonical Air lifecycle: draft -> ready -> work-in-progress ->
complete -> dropped.  R25-3 dropped the phantom `review' state (Air has no
such state; a doc that writes a non-canonical state ranks as Unknown)."
  :type '(repeat string)
  :group 'org-air)

(defcustom org-air-project-sections
  '("draft" "ready" "work-in-progress" "complete" "dropped")
  "State buckets, in order, rendered as project-view SECTIONS (D-P5.C).
Each present bucket becomes a section heading (badge icon + title + count
badge) with its doc rows beneath; empty buckets are omitted, exactly like
the board's empty sections."
  :type '(repeat string)
  :group 'org-air)

(defcustom org-air-project-state-badges
  '(("draft"            . ("\N{MEMO}\N{VARIATION SELECTOR-16}"               . "DRAFT"))
    ("ready"            . ("\N{DIRECT HIT}\N{VARIATION SELECTOR-16}"         . "READY"))
    ("work-in-progress" . ("\N{GEAR}\N{VARIATION SELECTOR-16}"               . "WIP"))
    ("complete"         . ("\N{WHITE HEAVY CHECK MARK}\N{VARIATION SELECTOR-16}" . "COMP"))
    ("dropped"          . ("\N{WASTEBASKET}\N{VARIATION SELECTOR-16}"        . "DROP")))
  "Per-state badge as (STATE . (EMOJI . TTY)).
The GUI shows EMOJI (R23-4) when `org-air-project-state-style' is `emoji';
the byte gate (no graphical frame) always shows TTY.  R26-2: the TTY slots
are the canonical short WORDS (`org-air-project--state-words'), padded to
the uniform `org-air-project--state-cell-w' cell by `--state-token'.  Each
emoji ends in `\N{VARIATION SELECTOR-16}' so it renders in COLOUR
presentation at a consistent width-2, matching the icons `airctl status
-Da' prints."
  :type '(alist :key-type string :value-type (cons string string))
  :group 'org-air)

(defcustom org-air-project-state-style 'svg
  "How the project per-doc STATE badge renders (R24-3).
`svg' (default) draws a LEGIBLE, cell-locked filled colour chip on a
graphical frame (`org-air-project--state-svg-badge', reusing
`org-air-view--svg-pillify'), occupying EXACTLY the token's text-cell box so
it can never jitter the R24-2 rails/columns; `nerd' shows a fixed nerd-font
glyph (`org-air-project-state-nerd-glyphs'); `text' is the plain coloured
token; `emoji' is the R23-4 colour emoji (opt-in, may misalign on some
fonts); `badge' keeps the R21-4 small hairline chip.  Every non-`svg' choice
degrades to the byte/TTY `[R]'... token off-GUI, so the byte goldens are
unchanged and the cell never grows past `org-air-project--state-cell-w'."
  :type '(choice (const :tag "Filled svg chip on GUI" svg)
                 (const :tag "Nerd-font glyph" nerd)
                 (const :tag "Plain token text" text)
                 (const :tag "Colour emoji (opt-in)" emoji)
                 (const :tag "Small hairline svg chip" badge))
  :group 'org-air)

(defcustom org-air-project-state-nerd-glyphs
  '(("draft"            . "\uf040")   ; nf-fa-pencil
    ("ready"            . "\uf192")   ; nf-fa-dot_circle_o  (target)
    ("work-in-progress" . "\uf013")   ; nf-fa-cog
    ("complete"         . "\uf058")   ; nf-fa-check_circle
    ("dropped"          . "\uf014"))  ; nf-fa-trash
  "Per-state nerd-font glyph for `org-air-project-state-style' = `nerd' (R24-3).
Used only on a graphical frame whose font can display the glyph; otherwise
the terse `[R]'... token shows.  Codepoints are the Nerd Fonts private-use
area; remap to taste (e.g. nf-md-* / nf-cod-*)."
  :type '(alist :key-type string :value-type string)
  :group 'org-air)

;;;; ---------------------------------------------------------------------
;;;; Detection + source resolution
;;;; ---------------------------------------------------------------------

(defun org-air-detect-air-project (root)
  "Return non-nil when ROOT is an Air project root.
That is, when ROOT contains `air-config.toml' or an `air/' subdirectory."
  (and (stringp root)
       (file-directory-p root)
       (or (file-exists-p (expand-file-name "air-config.toml" root))
           (file-directory-p (expand-file-name "air" root)))))

(defun org-air-project-roots ()
  "Return the list of Air project roots from config.
`org-air-projects' wins; otherwise derive from `org-air-sources' (explicit
`:air' entries and auto-detected source directories)."
  (or org-air-projects
      (delete-dups
       (delq nil
             (mapcar (lambda (entry)
                       (cond
                        ((and (consp entry) (eq (car entry) :air))
                         (cadr entry))
                        ((and (stringp entry)
                              (org-air-detect-air-project entry))
                         entry)))
                     org-air-sources)))))

;;;; ---------------------------------------------------------------------
;;;; Data model
;;;; ---------------------------------------------------------------------

(cl-defstruct (org-air-doc (:constructor org-air-doc-create))
  "A single Air document read from its `.org' file."
  name file state tags updated created relpath)

(defun org-air-project--air-dir (root)
  "Return the directory under ROOT containing the Air docs.
The `air/' subdirectory when present, else ROOT itself."
  (let ((air (expand-file-name "air" root)))
    (if (file-directory-p air) air root)))

(defconst org-air-project--non-tracked-file-stems '("readme" "overview" "skill")
  "Reserved summary/metadata file stems Air EXCLUDES from document tracking.
Mirrors airctl's `DocumentScanner::is_overview_file' (air-core scanner): a
file whose stem (case-insensitive, sans any supported extension) is one of
these is a directory-level summary (e.g. OVERVIEW.org) or tool metadata
such as README or SKILL, NOT a trackable work item -- so it must never
reach the per-dir state counts.")

(defun org-air-project--overview-file-p (file)
  "Non-nil when FILE is a non-trackable summary/metadata file (R20-5 fix).
Matches airctl exactly: the file stem (sans extension), case-folded, is
README, OVERVIEW or SKILL.  Air filters these directory-summary docs out
of document scanning, so org-air must not count their (stateless) bodies
as `draft' the way `org-air-project--collect-docs' silently did before."
  (and file
       (member (downcase (file-name-base file))
               org-air-project--non-tracked-file-stems)))

(defun org-air-project--read-keyword (key)
  "Return the first in-buffer #+KEY value as a string, or nil.
Point-independent; scans from the top of the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           (format "^[ \t]*#\\+%s:[ \t]*\\(.*\\)$" (regexp-quote key))
           nil t)
      (let ((v (string-trim (match-string 1))))
        (unless (string-empty-p v) v)))))

(defun org-air-project--parse-tags (filetags)
  "Return a list of tag strings from a FILETAGS value like \":a:b:\"."
  (when filetags
    (seq-remove #'string-empty-p (split-string filetags ":" t "[ \t]+"))))

(defun org-air-project--read-doc (file root)
  "Read FILE (under air dir of ROOT) into an `org-air-doc'."
  (let* ((air (org-air-project--air-dir root))
         (relpath (file-relative-name file air))
         state tags title)
    (with-temp-buffer
      (insert-file-contents file)
      ;; R20-5 fix: a doc WITHOUT a #+state: keyword is `unknown', exactly as
      ;; airctl does (`extracted.state.unwrap_or(DocumentState::Unknown)') --
      ;; never silently `draft'.  `unknown' ranks last and renders faded.
      (setq state (downcase (or (org-air-project--read-keyword "state") "unknown"))
            tags (org-air-project--parse-tags
                  (org-air-project--read-keyword "FILETAGS"))
            title (org-air-project--read-keyword "title")))
    (org-air-doc-create
     :name (or title (file-name-base file))
     :file file
     :state state
     :tags tags
     :updated (file-attribute-modification-time (file-attributes file))
     :created (org-air-project--doc-created file)
     :relpath relpath)))

(defun org-air-project--doc-created (file)
  "Return FILE's creation time (R14 D-P1.A): #+created:/#+date: else ctime.
Reads an in-buffer `#+created:' or `#+date:' keyword as an Org timestamp
when present, else the file's status-change (ctime) attribute."
  (or (ignore-errors
        (with-temp-buffer
          (insert-file-contents file nil 0 4096)
          (when-let* ((v (or (org-air-project--read-keyword "created")
                             (org-air-project--read-keyword "date")))
                      (ts (org-timestamp-from-string
                           (if (string-prefix-p "[" (string-trim v))
                               (string-trim v)
                             (format "[%s]" (string-trim v))))))
            (org-timestamp-to-time ts))))
      (file-attribute-status-change-time (file-attributes file))))

(defun org-air-project--collect-docs (root)
  "Return the list of `org-air-doc' under ROOT's Air directory.
Non-trackable summary/metadata files (OVERVIEW/README/SKILL) are EXCLUDED
exactly as `airctl status' excludes them (R20-5 fix,
`org-air-project--overview-file-p'), so the total doc count and every
per-dir state badge match `airctl status -Da' instead of inflating Draft
with the stateless directory-summary bodies."
  (let ((air (org-air-project--air-dir root)))
    (when (file-directory-p air)
      (mapcar (lambda (f) (org-air-project--read-doc f root))
              (seq-remove #'org-air-project--overview-file-p
                          (sort (directory-files-recursively air "\\.org\\'")
                                #'string-lessp))))))

;;;; ---------------------------------------------------------------------
;;;; Badges / glyphs
;;;; ---------------------------------------------------------------------

(defun org-air-project--state-face (state)
  "Return the face for STATE's badge."
  (pcase state
    ("draft" 'org-air-face-air-state-draft)
    ("ready" 'org-air-face-air-state-ready)
    ("work-in-progress" 'org-air-face-air-state-wip)
    ("complete" 'org-air-face-air-state-complete)
    ("dropped" 'org-air-face-air-state-dropped)
    (_ 'org-air-face-faded)))

(defun org-air-project--state-title (state)
  "Return a human title for STATE (e.g. \"Work In Progress\")."
  (mapconcat #'capitalize (split-string state "-") " "))

;;;; ---------------------------------------------------------------------
;;;; Row cells (D-P5.B — Air docs mapped onto the shared row primitive)
;;;; ---------------------------------------------------------------------

(defun org-air-project--doc-date-text (doc)
  "Return the faded updated-stamp date cell for DOC (D-P5.B): \"↻ YYYY-MM-DD\"."
  (propertize (concat (org-air-layout-glyph 'updated) " "
                      (format-time-string "%F" (org-air-doc-updated doc)))
              'face 'org-air-face-faded))

(defun org-air-project--doc-tagstr (doc)
  "Return DOC's #tags as the shared accent-faced svg pill string (D-P5.B).
Reuses `org-air-view--item-tagstr' so the project view's tag pills are the
SAME D-P1 pills as the board's (accent face + pad cols + svg overlay)."
  (let* ((tags (org-air-doc-tags doc))
         (n (length tags)))
    (org-air-view--item-tagstr tags n n)))

(defun org-air-project--doc-origin-text (doc)
  "Return DOC's origin cell (D-P5.B): \"⌂ relpath\" (the dir grouping lives here)."
  (concat (org-air-layout-glyph 'origin) " " (org-air-doc-relpath doc)))

(defun org-air-project--doc-widths (docs)
  "Return the fixed (DCOL TCOL OCOL) metadata column widths over DOCS.
Mirrors `org-air-view--compute-meta-widths' so the date / tags / path
columns line up exactly down the project list (board parity)."
  (let ((dw 0) (tw 0) (ow 0))
    (dolist (doc docs)
      (setq dw (max dw (string-width (org-air-project--doc-date-text doc)))
            tw (max tw (string-width (org-air-project--doc-tagstr doc)))
            ow (max ow (string-width (org-air-project--doc-origin-text doc)))))
    (list dw tw ow)))

;; R21-5: the per-render fixed metadata column widths over the DISPLAYED
;; docs, bound in `org-air-project--render' so the project's one-line rows
;; line up exactly like the board's `org-air-view--meta-*-w' (V6 + R20-6
;; "measure only what is shown").
(defvar org-air-project--meta-date-w 0
  "Per-render fixed date-cell width for the project rows (R21-5).")
(defvar org-air-project--meta-tags-w 0
  "Per-render fixed tags-cell width for the project rows (R21-5).")
(defvar org-air-project--meta-origin-w 0
  "Per-render fixed origin-cell width for the project rows (R21-5).")

(defconst org-air-project--state-words
  '(("draft" . "DRAFT") ("ready" . "READY") ("work-in-progress" . "WIP")
    ("complete" . "COMP") ("dropped" . "DROP"))
  "Canonical short-word state labels (R26-2).  Longest = 5 cols.
The single source for BOTH the TTY token (`--state-token', padded to the
uniform 5-col cell) and the GUI pill label (`--state-svg-badge', the bare
word centred in the same 5-col capsule).  DROP (not CANC, not X) because
Air's state is literally `dropped' — a truncation of the actual airctl
vocabulary, never an invented near-synonym.")

(defconst org-air-project--state-cell-w 5
  "Reserved width of the project state token cell (R26-2: 5-col words).
Was 3 (R21-5, `[R]'-style tokens); R26-2 relocks V6 at the word-pill
width — DRAFT/READY/WIP/COMP/DROP all pad to this one cell.")

(defun org-air-project--fit-meta-widths (docs width)
  "Return the fitted (DCOL TCOL OCOL) project column widths at WIDTH (R21-5).
Mirrors `org-air-view--compute-meta-widths' for the project rows: measures
the displayed DOCS, caps the origin at `org-air-origin-max-width', then
reclaims columns for the flex title (origin toward `org-air-origin-min',
then tags toward a 1-col floor) so the title keeps at least
`org-air-title-min-width'.  The left reserve mirrors the row prefix
\(margin + the fixed state cell + its separator) so a doc row and a board
task row share column positions (board parity, invariant #4)."
  (let* ((raw (org-air-project--doc-widths docs))
         (dw (nth 0 raw))
         (tw (nth 1 raw))
         ;; R25-5: the project view drops the origin/path cell (redundant
         ;; with the dir tree + title) -> NO origin column; the freed columns
         ;; reclaim to the flex title.  The cap/shrink steps have no origin
         ;; to shrink (ow 0), and `--insert-doc-row' passes no `:origin-text'.
         (ow 0)
         (gap 2)
         (left-reserve (+ (string-width (org-air-view--item-margin))
                          (1+ org-air-project--state-cell-w)))
         (cluster
          (lambda (o)
            (let ((cells (delq nil (list (and (> dw 0) dw)
                                         (and (> tw 0) tw)
                                         (and (> o  0) o)))))
              (+ (apply #'+ cells) (max 0 (1- (length cells)))))))
         (budget (lambda (o) (- width left-reserve gap (funcall cluster o)))))
    ;; 1) shrink the origin toward its floor until the title reaches min.
    (while (and (> ow org-air-origin-min)
                (< (funcall budget ow) org-air-title-min-width))
      (setq ow (1- ow)))
    ;; 2) still starved? shrink tags toward a 1-col floor.
    (let ((tw-floor (if (> tw 0) 1 0)))
      (while (and (> tw tw-floor)
                  (< (funcall budget ow) org-air-title-min-width))
        (setq tw (1- tw))))
    ;; 3) the date column is held (small, uniform); the title floor in
    ;;    `org-air-view--insert-row' (max 1) takes over on a board-only
    ;;    narrow tier -- never crash, never overflow.
    (list dw tw 0)))                       ; R25-5: ocol pinned 0

(defun org-air-project--state-token (state)
  "Return the uniform 5-col WORD state token for STATE (R26-2).
\"READY\" \"DRAFT\" \"WIP  \" \"COMP \" \"DROP \" — the word left-padded-right
to exactly `org-air-project--state-cell-w' cols, so every pill box is the
SAME size.  The user-visible `org-air-project-state-badges' TTY slot wins
when customized; canonical defaults come from
`org-air-project--state-words'.  A non-canonical state falls back to the
upcased 5-col truncation of its name (\"unknown\" -> \"UNKNO\") — replacing
the R25-4 letter fallback IN THE TOKEN ONLY (the per-dir rollup letters
stay `--state-letter').  This is the byte/TTY contract; the svg pill
overlays it on GUI."
  (org-air-view--pad-to
   (or (cdr (cdr (assoc state org-air-project-state-badges)))
       (cdr (assoc state org-air-project--state-words))
       (upcase (truncate-string-to-width state org-air-project--state-cell-w)))
   org-air-project--state-cell-w))

(defun org-air-project--state-emoji (state)
  "Return STATE's colour emoji when a graphical frame can show it, else nil.
The emoji carries `\N{VARIATION SELECTOR-16}' for colour presentation; it
is only offered on a graphical frame whose font can display the base glyph,
so `--batch' (no graphical frame) always returns nil and the cell falls
back to the terse `[R]'... token (the byte/TTY contract)."
  (let ((emoji (car (cdr (assoc state org-air-project-state-badges)))))
    (and emoji (display-graphic-p)
         (char-displayable-p (aref emoji 0))
         emoji)))

(defun org-air-project--state-svg-badge (state)
  "Return STATE's token carrying a uniform WORD-pill SVG chip (R26-2).
Reuses `org-air-view--svg-pillify' (shared box/pixel-lock/fallback) with the
state colour as BOTH a salient border and a stronger fill.  R26-2: the box
is the 5-col PADDED word token (the byte/TTY contract + the pixel-lock
box), so every state's capsule is the SAME 5-col × char-px size; the drawn
label is the BARE word (DRAFT/READY/WIP/COMP/DROP), centred + width-fitted
\(D-P1.FIT, never clips), bold, in the state colour.  One pad col is
reserved (the word never kisses the rounded edge) and the font-scale floor
drops to 0.62 — a 5-char word wants a smaller scale than R25-2's giant
single letter.  The fill stays the soft 0.22 tint.  Returns the plain
token unchanged off-GUI / when svg is unavailable."
  (let* ((face   (org-air-project--state-face state))
         (token  (propertize (org-air-project--state-token state) 'face face))
         (word   (string-trim (substring-no-properties token)))
         (color  (face-foreground face nil t)))
    (if (not (org-air-view--svg-available-p))
        token                                   ; byte/TTY fallback: READY
      ;; Every box is 5 cols × char-px — WIP and DROP get the same capsule
      ;; as READY (labels centre in the same box), rails stay ruler-straight.
      (let ((org-air-pill-pad-cols   1)
            (org-air-pill-fill-alpha (max org-air-pill-fill-alpha 0.22))
            (org-air-pill-font-scale (max org-air-pill-font-scale 0.62)))
        (org-air-view--svg-pillify token face
                                   :border-color color
                                   :label word
                                   :font-weight 'bold)))))

(defun org-air-project--state-nerd (state)
  "Return STATE's nerd glyph when a graphical frame can show it, else nil.
Mirrors `org-air-project--state-emoji': offered only on a graphical frame
whose font can display the glyph, so `--batch' returns nil and the cell
falls back to the terse `[R]'... token (the byte/TTY contract)."
  (let ((g (cdr (assoc state org-air-project-state-nerd-glyphs))))
    (and g (display-graphic-p) (char-displayable-p (aref g 0))
         (propertize g 'face (org-air-project--state-face state)))))

(defun org-air-project--state-badge-cell (state)
  "Return STATE's badge cell per `org-air-project-state-style' (R24-3).
`svg' (the default) draws the LEGIBLE filled colour chip
\(`org-air-project--state-svg-badge'); `nerd' a fixed nerd-font glyph;
`text' the plain coloured token; `emoji' the R23-4 colour emoji; `badge' the
R21-4 hairline chip.  Every branch degrades to the byte/TTY `[R]'... token
off-GUI, so the byte gate (non-graphic) stays byte-identical.  Whatever this
returns is the cell TEXT at the normal line height (never a `:height' face),
so the row never grows (svg-never-grows-line); `--state-cell' pads it to the
fixed `org-air-project--state-cell-w' so the title left edge / the R24-2
rails stay V6-locked."
  (let* ((face  (org-air-project--state-face state))
         (token (propertize (org-air-project--state-token state) 'face face)))
    (pcase org-air-project-state-style
      ('text  token)
      ('badge (org-air-view--svg-keyword-badge token face))
      ('emoji (or (org-air-project--state-emoji state)
                  (org-air-project--state-svg-badge state)))
      ('nerd  (or (org-air-project--state-nerd state)
                  (org-air-project--state-svg-badge state)))
      (_      (org-air-project--state-svg-badge state)))))

(defun org-air-project--state-cell (state)
  "Return a FIXED-width reserved STATE cell for the project row (R21-5).
Mirrors the board's `org-air-view--todo-cell': the state TOKEN in its
state face, left-justified and padded to `org-air-project--state-cell-w'
plus a single trailing separator, so every doc title shares one left
edge.  The token text is the byte/TTY contract; R21-4 overlays the svg
keyword/state badge on GUI."
  (concat (org-air-view--pad-to
           (org-air-project--state-badge-cell state)
           org-air-project--state-cell-w)
          " "))

;;;; ---------------------------------------------------------------------
;;;; Sections (D-P5.C — state buckets become sections; parity with board)
;;;; ---------------------------------------------------------------------

(defvar org-air-project--width 80
  "Effective render width for the current project-view render.")

(defun org-air-project--render-width ()
  "Return the width to render the project view at."
  (or org-air-project-view-width
      (and (get-buffer-window (current-buffer) t)
           (org-air-layout-current-width (current-buffer)))
      80))

(defun org-air-project--host-width ()
  "Return the project compose width, rail-geometry aware (R27-2).
With the rail POPPED (and not suspended, and no batch width seam) the
width is resolved through the shared `org-air-rail--host-width': the
pinned side window is ensured FIRST, then the project window's ACTUAL
body width is measured — so the doc rows and the V6 meta lock are
composed at the width they will really display at (trunk composed at a
width measured at the WRONG moment of the resize cycle and never
re-measured after the rail popped).  Every other path (inline, batch
seam) keeps `org-air-project--render-width' exactly as today."
  (if (and (not noninteractive)
           (null org-air-project-view-width)
           (org-air-rail--popped-p)
           (not org-air-view--rail-suspended))
      (org-air-rail--host-width (current-buffer)
                                (org-air-project--render-width))
    (org-air-project--render-width)))

(defconst org-air-project--attention-states '("ready" "work-in-progress")
  "States whose non-empty section count uses the attention badge (D-P5.C).")

(defun org-air-project--sections (docs)
  "Return the ordered render sections for DOCS under the current group mode.
Each section is a plist: :icon :icon-face :title :title-face :docs
:attention :show-state.  R16 D-P4/D-P5: every section's members are ordered
through the single comparator (`org-air-project--sort-section-docs') so
state-then-sort-key composition is uniform across all group modes."
  (let ((sections (pcase org-air-project-group
                    ('tag (org-air-project--sections-by-tag docs))
                    (_ (org-air-project--sections-by-state docs)))))
    (dolist (section sections sections)
      (plist-put section :docs
                 (org-air-project--sort-section-docs
                  (plist-get section :docs))))))

(defun org-air-project--sections-by-state (docs)
  "Return state-bucket sections for DOCS in `org-air-project-sections' order.
Buckets with zero docs are omitted; any state not listed is appended."
  (let ((order (append org-air-project-sections
                       (seq-remove (lambda (s) (member s org-air-project-sections))
                                   (seq-uniq (mapcar #'org-air-doc-state docs))))))
    (delq nil
          (mapcar
           (lambda (state)
             (let ((members (seq-filter
                             (lambda (d) (equal (org-air-doc-state d) state))
                             docs)))
               (when members
                 ;; R21.1: route the section icon through the SAME shared
                 ;; svg keyword/state badge as the doc rows (svg chip on
                 ;; GUI, terse `[R]' token on TTY) -- no GUI emoji.
                 (list :icon (org-air-project--state-badge-cell state)
                       :icon-face (org-air-project--state-face state)
                       :title (org-air-project--state-title state)
                       :title-face 'org-air-face-section
                       :docs members
                       :attention (member state org-air-project--attention-states)
                       :show-state nil))))
           order))))

;;;; ---------------------------------------------------------------------
;;;; Nested directory tree (R20-5 — match airctl status -Da)
;;;; ---------------------------------------------------------------------

(defun org-air-project--doc-dir-segments (doc)
  "Return DOC's directory path as a list of segments (R20-5).
\"v0.1/air-context/x.org\" -> (\"v0.1\" \"air-context\"); a root doc -> nil."
  (let ((dir (file-name-directory (or (org-air-doc-relpath doc) ""))))
    (and dir (split-string (directory-file-name dir) "/" t))))

(defun org-air-project--state-display-rank (state)
  "Return STATE's rank in `org-air-project--state-display-order' (unknown last)."
  (or (seq-position org-air-project--state-display-order state #'equal)
      (length org-air-project--state-display-order)))

(defun org-air-project--state-first-lessp (a b)
  "Non-nil when doc A precedes B state-first, then by the ACTIVE key (R26-7).
State-display-rank stays PRIMARY (the airctl -Da shape); the within-state
order delegates to `org-air-project--doc-compare-key' — the active `o'/`O'
sort — instead of the old fixed name tiebreak (which starved the sort key
in the DEFAULT directory grouping).  Byte-stable at the default: key
`name' ascending is `string-lessp' on names + the name/relpath tiebreak,
exactly the old order."
  (let ((ra (org-air-project--state-display-rank (org-air-doc-state a)))
        (rb (org-air-project--state-display-rank (org-air-doc-state b))))
    (if (/= ra rb) (< ra rb)
      (org-air-project--doc-compare-key a b))))

(defun org-air-project--sort-own-docs (docs)
  "Return DOCS state-first (display order), then by the active key (R26-7)."
  (sort (copy-sequence docs) #'org-air-project--state-first-lessp))

(defun org-air-project--count-by-state (docs)
  "Return an alist STATE -> count over DOCS."
  (let (table)
    (dolist (d docs table)
      (let ((cell (assoc (org-air-doc-state d) table)))
        (if cell (setcdr cell (1+ (cdr cell)))
          (push (cons (org-air-doc-state d) 1) table))))))

(defun org-air-project--counts-add (a b)
  "Return the per-state sum of count alists A and B."
  (let ((out (copy-alist a)))
    (dolist (cell b out)
      (let ((o (assoc (car cell) out)))
        (if o (setcdr o (+ (cdr o) (cdr cell)))
          (push (cons (car cell) (cdr cell)) out))))))

(defun org-air-project--make-dir-node (path depth subtree-docs)
  "Build the tree node for the directory PATH (a list of segments) at DEPTH.
SUBTREE-DOCS are all docs whose dir-segments have PATH as a prefix (or
equal it).  A node is a plist (see `org-air-project--directory-tree').
Counts are computed bottom-up: :direct-counts over the dir's OWN docs,
:desc-counts summed over descendant dirs, :total-counts = direct+desc."
  (let* ((plen (length path))
         (own (seq-filter
               (lambda (d)
                 (= (length (org-air-project--doc-dir-segments d)) plen))
               subtree-docs))
         (deeper (seq-filter
                  (lambda (d)
                    (> (length (org-air-project--doc-dir-segments d)) plen))
                  subtree-docs))
         (groups nil))                  ; alist next-seg -> docs (reversed)
    (dolist (d deeper)
      (let* ((seg (nth plen (org-air-project--doc-dir-segments d)))
             (cell (assoc seg groups)))
        (if cell (setcdr cell (cons d (cdr cell)))
          (push (cons seg (list d)) groups))))
    (setq groups (sort groups (lambda (a b) (string-lessp (car a) (car b)))))
    (let* ((children (mapcar
                      (lambda (g)
                        (org-air-project--make-dir-node
                         (append path (list (car g)))
                         (1+ depth)
                         (nreverse (cdr g))))
                      groups))
           (direct (org-air-project--count-by-state own))
           (desc (seq-reduce
                  (lambda (acc c)
                    (org-air-project--counts-add
                     acc (org-air-project--counts-add
                          (plist-get c :direct-counts)
                          (plist-get c :desc-counts))))
                  children nil)))
      (list :dir (car (last path))
            :depth depth
            :path (string-join path "/")
            :own-docs (org-air-project--sort-own-docs own)
            :children children
            :direct-counts direct
            :desc-counts desc
            :total-counts (org-air-project--counts-add direct desc)))))

(defun org-air-project--directory-tree (docs)
  "Return the ordered list of TOP-dir nodes for DOCS (R20-5).
Groups DOCS by their first path segment (top dirs, name-sorted); root
docs with no directory fold into a leading node with an empty :path."
  (let (groups root-docs)
    (dolist (d docs)
      (let ((segs (org-air-project--doc-dir-segments d)))
        (if (null segs)
            (push d root-docs)
          (let* ((seg (car segs)) (cell (assoc seg groups)))
            (if cell (setcdr cell (cons d (cdr cell)))
              (push (cons seg (list d)) groups))))))
    (setq groups (sort groups (lambda (a b) (string-lessp (car a) (car b)))))
    (let ((nodes (mapcar
                  (lambda (g)
                    (org-air-project--make-dir-node
                     (list (car g)) 0 (nreverse (cdr g))))
                  groups)))
      (if root-docs
          (cons (org-air-project--make-dir-node nil 0 (nreverse root-docs))
                nodes)
        nodes))))

(defun org-air-project--marker ()
  "Return the propertized rail-marker glyph (svg accent bar on GUI)."
  (let* ((img (org-air-layout-marker-image))
         (mk (org-air-layout-glyph 'rail-marker))
         (marker (propertize mk 'face 'org-air-face-rail-marker)))
    (if img (propertize marker 'display img) marker)))

(defconst org-air-project--state-letters
  '(("draft"            . "D")    ; 📝  airctl Draft
    ("ready"            . "R")    ; 🎯  airctl Ready
    ("work-in-progress" . "W")    ;      Work-In-Progress (W = Work/WIP)
    ("complete"         . "C")    ; ✅  airctl Complete
    ("dropped"          . "X"))   ; 🗑️  airctl Dropped (token is already [X])
  "Canonical per-state single LETTER (R25-4): airctl-aligned + DISTINCT.
Draft=D and Dropped=X never collide; `work-in-progress'=W is distinct from
all.  The single source for BOTH the per-doc badge glyph and the per-dir
rollup letter, so the two can never drift.")

(defun org-air-project--state-letter (state)
  "Return STATE's DISTINCT single-letter badge glyph (R25-4).
From `org-air-project--state-letters' for a canonical state; else the
upcased first char of the name — never derived in a way that collides
Draft/Dropped on `D' (the canonical map pins D=draft, X=dropped).  Drives
BOTH the per-doc badge label and the per-dir count summary."
  (or (cdr (assoc state org-air-project--state-letters))
      (and (> (length state) 0) (upcase (substring state 0 1)))
      "?"))

(defun org-air-project--dir-count-summary (direct desc)
  "Return the calm `R4(+1) C14(+14) ...' count summary for a dir header (R22-6).
DIRECT is the dir's OWN per-state counts, DESC its descendants' rollup.
State as a quiet faded LETTER (not the coloured badge), own count, faded
`(+M)' nested rollup; states absent from BOTH are omitted; display order =
`org-air-project--state-display-order'.  Numerically identical to the old
`--count-badges' / `airctl -Da' (own N + nested +M)."
  (let (cells)
    (dolist (state org-air-project--state-display-order)
      (let ((n (or (cdr (assoc state direct)) 0))
            (m (or (cdr (assoc state desc)) 0)))
        (when (or (> n 0) (> m 0))
          (push (concat
                 (propertize (org-air-project--state-letter state)
                             'face (org-air-project--state-face state))
                 (when (> n 0) (propertize (number-to-string n)
                                           'face 'org-air-face-count))
                 (when (> m 0) (propertize (format "(+%d)" m)
                                           'face 'org-air-face-faded)))
                cells))))
    (mapconcat #'identity (nreverse cells) " ")))

(defun org-air-project--insert-dir-node (node width &optional rails lastp)
  "Insert NODE (a dir tree node) and its subtree into the buffer at WIDTH.
ONE header per directory (R22-6) with classic TREE CONNECTORS (R23-3): a
top dir (depth 0) keeps the accent `org-air-project--marker' (the quiet
section bullet, blank-line separated, never railed); a child dir is led by
a faded `org-air-face-air-tree' guide — the accumulated ancestor RAILS
string followed by a `box-tee-left'/`box-bottom-left' + `box-horizontal'
connector (LASTP picks the corner).  Then the `dir/' name on the LEFT, the
quiet right-aligned letter-count summary (`R4(+1) C14(+14) ...') on the
RIGHT, the dir's OWN docs (state-first, indented one level DEEPER than the
header — unchanged), then recursion into the name-sorted children, each
extending RAILS by a `box-vertical' cell when THIS node has a following
sibling.  Glyphs route through `org-air-layout-glyph' so a TTY/batch frame
gets the ascii `|  ' / `+- ' fallback."
  (let* ((depth (plist-get node :depth))
         (children (plist-get node :children))
         (dir (plist-get node :dir))
         (path (plist-get node :path))
         (name (if (and dir (not (string-empty-p dir))) (concat dir "/") "·"))
         ;; Top dirs keep the accent marker; children get the faded ancestor
         ;; rail + a tee/corner connector (3-col cells, aligned under the
         ;; parent name).  All guide glyphs are `org-air-face-air-tree'.
         (guide (if (zerop depth)
                    (concat "  " (org-air-project--marker) " ")
                  (concat "  "
                          (propertize (or rails "") 'face 'org-air-face-air-tree)
                          (propertize
                           (concat (org-air-layout-glyph
                                    (if lastp 'box-bottom-left 'box-tee-left))
                                   (org-air-layout-glyph 'box-horizontal) " ")
                           'face 'org-air-face-air-tree))))
         (start (point))
         (left (concat guide (propertize name 'face 'org-air-face-section)))
         ;; Quiet, right-aligned count summary (the badge wall is gone).
         (summary (org-air-project--dir-count-summary
                   (plist-get node :direct-counts)
                   (plist-get node :desc-counts)))
         (header (if (string-empty-p summary)
                     left
                   (org-air-view--justify left summary width)))
         ;; Children of a TOP dir get no rail (top dirs aren't railed);
         ;; deeper nodes extend the rail with a `box-vertical' cell only when
         ;; THIS node has a following sibling, else 3 blanks.
         (child-rails (if (zerop depth)
                          ""
                        (concat (or rails "")
                                (if lastp "   "
                                  (concat (org-air-layout-glyph 'box-vertical)
                                          "  "))))))
    (insert header "\n")
    (add-text-properties start (point) (list 'org-air-section path))
    ;; R24-2: thread the rail DOWN to the OWN docs so each doc visibly hangs
    ;; under its directory (matching airctl -Da).  Own docs are emitted
    ;; BEFORE the child dirs, so the `last child overall' corner (└─) lands
    ;; on the FINAL own-doc ONLY when this dir has no child dirs; otherwise
    ;; the own docs are tees (├─) and the corner goes to the last child dir.
    (let* ((docs (plist-get node :own-docs))
           (ndocs (length docs))
           (nkids (length children))
           (i 0))
      (dolist (doc docs)
        (setq i (1+ i))
        (org-air-project--insert-doc-row
         doc width depth child-rails (and (= i ndocs) (zerop nkids))))
      ;; Recurse into the children (name-sorted), threading rails + last-child.
      (let ((j 0))
        (dolist (child children)
          (setq j (1+ j))
          (org-air-project--insert-dir-node child width child-rails (= j nkids)))))))

(defun org-air-project--insert-directory-tree (nodes width)
  "Insert the nested directory TREE (NODES) at content WIDTH (R20-5/R23-3).
Top-level nodes start with empty ancestor rails; LASTP per node tells the
connector logic whether it is the final sibling."
  (let ((n (length nodes)) (i 0))
    (dolist (node nodes)
      (org-air-project--insert-dir-node node width "" (= (1+ i) n))
      (setq i (1+ i))
      (insert "\n"))))

(defun org-air-project--sections-by-tag (docs)
  "Return tag sections for DOCS (a doc may appear under several tags)."
  (let ((tags (seq-uniq (apply #'append (mapcar #'org-air-doc-tags docs)))))
    (mapcar
     (lambda (tag)
       (list :icon nil
             :icon-face nil
             :title (concat "#" tag)
             :title-face (org-air-faces-tag-face tag)
             :docs (seq-filter (lambda (d) (member tag (org-air-doc-tags d))) docs)
             :attention nil
             :show-state t))
     (sort tags #'string-lessp))))

(defun org-air-project--insert-section-heading (section)
  "Insert SECTION's heading (R14 D-P1.A): ▌ marker + icon + title + count.
Adopts the round-11 prefix-svg `▌' header marker for parity with the rail
headers; the marker carries the GUI svg accent bar via
`org-air-layout-marker-image'."
  (let* ((start (point))
         (icon (plist-get section :icon))
         (count (length (plist-get section :docs)))
         (img (org-air-layout-marker-image))
         (mk (org-air-layout-glyph 'rail-marker))
         (marker (propertize mk 'face 'org-air-face-rail-marker))
         (marker (if img (propertize marker 'display img) marker)))
    (insert "  " marker " "
            (if icon
                (concat (propertize icon 'face (plist-get section :icon-face)) " ")
              "")
            (propertize (plist-get section :title)
                        'face (plist-get section :title-face))
            " "
            (propertize (format "%d" count)
                        'face (if (plist-get section :attention)
                                  'org-air-face-count-attention
                                'org-air-face-count))
            "\n")
    (add-text-properties start (point)
                         (list 'org-air-section (plist-get section :title)
                               'org-air-count-badge count))))

(defun org-air-project--deslug-relpath (relpath)
  "Return RELPATH with its Denote LEAF de-slugged (R17 D-P2).
Keeps the directory prefix and replaces only the leaf with its de-slugged
Denote title (`org-air-view--denote-title'), falling back to the raw leaf
for a non-Denote name.  So a long Denote filename reads as its title --
e.g. \"v0.1/weekly-invalidation-rate-upgr\" -- instead of the raw
identifier--slug__tags.org; `org-air-view--pad-to' still bounds line 2."
  (let* ((dir (file-name-directory relpath))
         (leaf (file-name-nondirectory relpath))
         (title (or (org-air-view--denote-title leaf) leaf)))
    (concat (or dir "") title)))

(defun org-air-project--insert-doc-row (doc _width &optional depth rails lastp)
  "Insert DOC as ONE board-style row via the shared primitive (R21-5/R24-2).
Maps DOC onto `org-air-view--insert-row' exactly as the board maps a task
\(invariant #4: parameterise the shared primitive, do not fork): the doc
STATE is the row PREFIX as a fixed-width cell, the updated stamp the DATE
cell, the #tags the SAME svg pills as the board, and the relpath the
right-justified ORIGIN cell.  The whole row carries `org-air-doc' +
`org-air-marker' so point on ANY cell identifies the doc (RET/visit still
resolve).

R24-2: in the DIRECTORY tree DEPTH is the dir's depth, RAILS the faded
ancestor rail string and LASTP the corner selector — the leading gutter is
PAINTED with the `org-air-face-air-tree' ancestor rails + this doc's own
`box-tee-left'/`box-bottom-left' connector, sized to EXACTLY the width the
old plain indent produced so the state cell / title / right cluster stay
V6-locked (the rail glyphs live purely in the left gutter; glyphs route
through `org-air-layout-glyph' for the TTY/batch `|`/`+-' fallback).  With
DEPTH nil (state-/tag-grouping, no dir tree) the prefix is the old plain
margin + state cell, byte-identical to today."
  (let* ((state  (org-air-doc-state doc))
         (prefix
          (if (null depth)
              ;; No dir tree (state/tag grouping): the old plain prefix.
              (concat (org-air-view--item-margin)
                      (org-air-project--state-cell state))
            ;; Directory tree: paint the faded ancestor rails + connector
            ;; into the gutter, to the SAME width the old plain indent
            ;; produced (so nothing to the right of the gutter moves).
            ;; R25-1: fill the run AFTER the corner with `box-horizontal'
            ;; glyphs (not spaces) so the arm REACHES the badge.  R26-1:
            ;; the arm run stops ONE column short and the freed column
            ;; renders as a single breathing-room SPACE joining arm to
            ;; badge (`+---- [R]', was `+-----[R]').  The gutter TOTAL
            ;; width stays EXACTLY `gutter-w' (truncate/pad clamp), so the
            ;; state cell / title / right cluster do not move (V6
            ;; pixel-lock).  Degenerate clamp: when the rails are so deep
            ;; that no column is left after the corner, there is no arm
            ;; AND no space — the corner alone touches the badge.
            (let* ((margin-w  (string-width (org-air-view--item-margin)))
                   (old-indent (* 2 (1+ depth)))
                   (gutter-w  (+ margin-w old-indent))
                   (hbar      (org-air-layout-glyph 'box-horizontal))
                   (corner    (org-air-layout-glyph
                               (if lastp 'box-bottom-left 'box-tee-left)))
                   (lead      (concat "  " (or rails "") corner)) ; margin+rails+corner
                   (armlen    (max 0 (- gutter-w (string-width lead) 1)))
                   (arm       (concat
                               (apply #'concat (make-list armlen hbar))
                               (if (> (- gutter-w (string-width lead)) 0)
                                   " " "")))
                   (guide     (org-air-view--pad-to
                               (propertize (truncate-string-to-width
                                            (concat lead arm) gutter-w)
                                           'face 'org-air-face-air-tree)
                               gutter-w)))
              (concat guide (org-air-project--state-cell state)))))
         (date   (org-air-project--doc-date-text doc))
         (tags   (org-air-project--doc-tagstr doc)))
    (org-air-view--insert-row
     :prefix prefix
     ;; R26-4: the `(' flip — ONE rule in ONE place: doc rows show the
     ;; RAW file name while the per-buffer flag is on (the point is the
     ;; real file name; the R24 deslug affordance stays title-mode-only),
     ;; the doc title otherwise.  R28-5: the rule is context-aware via the
     ;; argument it already receives — DEPTH is non-nil exactly when the
     ;; row renders inside the DIRECTORY tree (R24-2), where every path
     ;; segment is already on screen as an ancestor node, so the flip
     ;; shows the BASENAME there (information-preserving at any depth);
     ;; the flat state/tag groupings keep the FULL relpath (the path IS
     ;; the information — no tree conveys it).  The R24-6 filter key
     ;; stays the full relpath in every grouping (display-independent).
     :title (if org-air-project--show-filenames
                (if depth
                    (file-name-nondirectory (org-air-doc-relpath doc))
                  (org-air-doc-relpath doc))
              (org-air-doc-name doc))
     :date-text date
     :tags tags
     ;; R25-5: no `:origin-text' / `:origin-face' — the project view drops the
     ;; path cell (redundant with the dir tree + title); `--insert-row' omits
     ;; the 0-width origin cell.  The relpath stays in the FILTER search key
     ;; (`--render' uses `org-air-doc-relpath' directly), so path tokens still
     ;; match; only the DISPLAY column is gone.
     :widths (list org-air-project--meta-date-w
                   org-air-project--meta-tags-w
                   0)
     :props (list 'org-air-doc doc
                  'org-air-marker (org-air-doc-file doc)
                  'mouse-face 'org-air-face-cursor)
     :face 'org-air-face-title)))

;;;; ---------------------------------------------------------------------
;;;; View
;;;; ---------------------------------------------------------------------

(defvar-local org-air-project--root nil
  "Air root rendered in this project-view buffer.")

(defvar-local org-air-project--doc-count nil
  "Cached doc count for the calm status mode-line (R20-2); set per render.")

(defvar-local org-air-project--rendered-width nil
  "Width of the most recent project-view render (R14 D-P1.B resize guard).")

(defvar-local org-air-project--sort-key nil
  "Active per-buffer sort key (R16 D-P4); seeded from `org-air-project-sort-key'.")
(defvar-local org-air-project--sort-direction nil
  "Active per-buffer sort direction (R16 D-P4); seeded from the defcustom.")

(defvar-local org-air-project--show-filenames nil
  "Non-nil: doc rows render the project-relative FILE NAME, not the title.
Per-buffer (the Dired `(' convention, R26-4); read by the render pass, so
it survives `g' refresh, grouping changes and board<->project hops.")

(defvar-local org-air-project--session nil
  "TREE-side doc-session stash: (:point P :window W) (R26-5).
Set by `org-air-project-open' so `org-air-project-back' restores the SAME
window and lands point back on the originating doc row.")

(defvar-local org-air-project--session-tree nil
  "DOC-side back-pointer to the project tree buffer (R26-5).
Non-nil only in a doc FILE buffer opened by `org-air-project-open'; it
makes the doc buffer count as a rail HOST for the R25-6 sweep
\(`org-air-rail--host-buffer-p') and is the `back' target.")

(defun org-air-project--state-rank (state)
  "Return the canonical rank of STATE (R16 D-P5).
Uses `org-air-project-sections' (Draft/Ready/WIP/Complete/Dropped) as
the single source of truth for state order: a known state gets its index;
any unknown state shares one rank just past the known ones (so it sorts
AFTER them).  Unknown states are then ordered among themselves by their
state string in `org-air-project--doc-compare', not by this integer."
  (or (seq-position org-air-project-sections state #'equal)
      (length org-air-project-sections)))

(defun org-air-project--sort-key-active ()
  "Return the active sort key (R22-3: the SHARED sort state wins).
The project now drives `o'/`O' through `org-air-view--sort-key' (the shared
core); the project-local var remains a fallback (let-bindable in tests),
then the defcustom."
  (or org-air-view--sort-key org-air-project--sort-key org-air-project-sort-key))

(defun org-air-project--sort-direction-active ()
  "Return the active sort direction (R22-3: the SHARED sort state wins)."
  (or org-air-view--sort-direction org-air-project--sort-direction
      org-air-project-sort-direction))

(defun org-air-project--doc-key-value (doc key)
  "Return DOC's value for sort KEY (`name'/`created'/`updated') (R16 D-P4)."
  (pcase key
    ('created (org-air-doc-created doc))
    ('updated (org-air-doc-updated doc))
    (_ (org-air-doc-name doc))))

(defun org-air-project--doc-tiebreak-lessp (a b)
  "Return non-nil when doc A precedes doc B by the byte-stable tiebreak.
Name ascending, then relpath ascending."
  (let ((na (or (org-air-doc-name a) ""))
        (nb (or (org-air-doc-name b) "")))
    (if (not (string-equal na nb))
        (string-lessp na nb)
      (string-lessp (or (org-air-doc-relpath a) "")
                    (or (org-air-doc-relpath b) "")))))

(defun org-air-project--doc-compare (a b)
  "Strict total order over docs A and B (R16 D-P4/D-P5).
1. state-rank ascending (D-P5 — within-group state primary; constant within
   a state group so the key drives order there);
2. the active sort key in the active direction (a nil date sorts LAST in
   BOTH directions — the partition rule);
3. tiebreak: name then relpath ascending (byte-stable equal keys)."
  (let* ((sa (org-air-doc-state a))
         (sb (org-air-doc-state b))
         (ra (org-air-project--state-rank sa))
         (rb (org-air-project--state-rank sb))
         (unknown (length org-air-project-sections)))
    (cond
     ((/= ra rb) (< ra rb))
     ;; Both UNKNOWN (same tail rank) but different states -> order by the
     ;; state string (matches the docstring; byte-stable).
     ((and (= ra unknown) (not (equal sa sb)))
      (string-lessp (or sa "") (or sb "")))
     (t (org-air-project--doc-compare-key a b)))))

(defun org-air-project--doc-compare-key (a b)
  "Strict order over docs A and B by the active sort key only (R16 D-P4).
The D-P5 state-rank primary is applied by `org-air-project--doc-compare'."
  (let* ((key (org-air-project--sort-key-active))
         (desc (eq (org-air-project--sort-direction-active) 'descending))
         (va (org-air-project--doc-key-value a key))
         (vb (org-air-project--doc-key-value b key)))
    (cond
     ;; Nil-key partition: missing dates always sort LAST, never
     ;; flipping to the top under descending.
     ((and (null va) (null vb))
      (org-air-project--doc-tiebreak-lessp a b))
     ((null va) nil)
     ((null vb) t)
     (t
      (let ((lt (if (memq key '(created updated))
                    (time-less-p va vb)
                  (string-lessp (or va "") (or vb ""))))
            (gt (if (memq key '(created updated))
                    (time-less-p vb va)
                  (string-lessp (or vb "") (or va "")))))
        (cond
         ((and (not lt) (not gt))
          ;; equal key → byte-stable tiebreak (never reversed).
          (org-air-project--doc-tiebreak-lessp a b))
         (desc gt)
         (t lt)))))))

(defun org-air-project--sort-section-docs (docs)
  "Return DOCS ordered by the single comparator (R16 D-P4/D-P5).
Every group mode funnels its members through here so the comparator is
the single source of truth for display order."
  (sort (copy-sequence docs) #'org-air-project--doc-compare))

(defun org-air-project--sort-indicator ()
  "Return the active-sort badge text `↕ <key> <dir>' (R16 D-P4).
R22-3: delegates to the shared `org-air-view--sort-indicator-text' builder
so the board and the project show one indicator; byte-identical to the old
local builder (same glyphs + faces).  R27-3: when the key OR direction
differs from the defcustom seeds (`org-air-project-sort-key' /
`org-air-project-sort-direction' — the same seeds the mode body uses) the
badge takes the bold `org-air-face-sort-active'; at the default it keeps
today's quiet faces, so the default goldens are byte- and face-identical."
  (let ((key (org-air-project--sort-key-active))
        (dir (org-air-project--sort-direction-active)))
    (org-air-view--sort-indicator-text
     key dir
     (not (and (eq key org-air-project-sort-key)
               (eq dir org-air-project-sort-direction))))))

(defun org-air-project--filter-segment ()
  "Return the active filter + combinator as a header segment, or empty string.
R18 D-P3: mirrors the board banner (`#a AND #b' / `#a OR #b', single tag
shows no combinator) so the two views read identically.  Empty when no
filter is active, keeping the existing project goldens byte-identical."
  (let* ((filters (org-air-view--filter-tags))
         (sep (if (> (length filters) 1)
                  (concat " " (org-air-view--filter-combinator-word) " ")
                " ")))
    (if filters
        (propertize (concat " · "
                            (mapconcat (lambda (tag) (concat "#" tag)) filters sep)
                            " " (org-air-view--glyph 'clear))
                    'face 'org-air-face-faded)
      "")))

(defun org-air-project--files-chip ()
  "Return the quiet `⇄ files' header chip while the R26-4 flip is on.
Empty at the default (titles), so every golden is byte-identical; while
flipped it sits beside the sort indicator (same faded idiom) so a flipped
buffer is never mistaken for odd titles."
  (if org-air-project--show-filenames
      (propertize (concat (org-air-layout-glyph 'flip) " files · ")
                  'face 'org-air-face-faded)
    ""))

(defun org-air-project--header-line (width)
  "Return the project header line for WIDTH: title left, sort badge right.
The badge order is part of the byte contract (R16 D-P4).  R18 D-P3: an
active tag filter + combinator is surfaced beside the title (empty when
none, so the no-filter goldens are byte-identical).  R26-4: a flipped
buffer gains the `⇄ files' chip next to the sort indicator."
  (let* ((title (concat (propertize "  org-air · project" 'face 'org-air-face-title)
                        (org-air-project--filter-segment)))
         (badge (concat (org-air-project--files-chip)
                        (org-air-project--sort-indicator)))
         (lw (string-width title))
         (bw (string-width badge))
         ;; right-cluster the badge, leaving a trailing column like the rest.
         (pad (max 1 (- width lw bw 2))))
    (concat title (make-string pad ?\s) badge)))

(defun org-air-project--insert-doc-sections (sections width)
  "Insert all SECTIONS (headings + two-line doc blocks) at content WIDTH."
  (dolist (section sections)
    (org-air-project--insert-section-heading section)
    ;; R21-5: one board-style row per doc.  The row ALWAYS carries the
    ;; state cell now, so the per-section SHOW-STATE conditional is gone
    ;; (a doc under a state section reads identically to one under a dir).
    (dolist (doc (plist-get section :docs))
      (org-air-project--insert-doc-row doc width))
    (insert "\n")))

(defun org-air-project--insert-state-summary-line (docs)
  "Insert the compact one-line state-count summary for DOCS (board-only)."
  (insert "  "
          (mapconcat
           (lambda (state)
             (let ((n (seq-count (lambda (d) (equal (org-air-doc-state d) state))
                                 docs)))
               ;; R21.1: shared svg badge (no GUI emoji); `[R]' token on TTY.
               (concat (org-air-project--state-badge-cell state)
                       " " (number-to-string n))))
           org-air-project-states "   ")
          "\n\n"))

(defun org-air-project--insert-summary (docs width)
  "Insert the project rail Summary block for DOCS at rail WIDTH (R14 D-P1.B).
A `▌ Summary' header + a per-state count row (the board's top-line state
summary moved into the rail)."
  (org-air-view--rail-header "Summary" width)
  (let ((inset (org-air-view--rail-inset-str width)))
    (dolist (state org-air-project-states)
      (let ((n (seq-count (lambda (d) (equal (org-air-doc-state d) state)) docs)))
        (insert inset
                (propertize (format "%3d" n)
                            'face (if (= n 0) 'org-air-face-faded
                                    'org-air-face-summary-number))
                "  "
                (propertize (org-air-project--state-title state)
                            'face 'org-air-face-summary-label)
                "\n")))))

(defun org-air-project--calendar-marks (docs)
  "Return a date-key -> `created mark table over DOCS' updated stamps (R20-5).
The shared rail's Calendar marks each doc on the day it was last updated
with a quiet `created'-style dot, so the project Calendar reads like the
board's without needing Org deadline/scheduled timestamps."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (doc docs table)
      (when-let* ((u (org-air-doc-updated doc)))
        (let ((d (decode-time u)))
          (puthash (org-air-calendar--date-key (decoded-time-month d)
                                               (decoded-time-day d)
                                               (decoded-time-year d))
                   'created table))))))

(defconst org-air-project--actions-table
  '((("RET" . "open")   ("(" . "flip")      ("/" . "filter"))
    (("o" . "sort")     ("s/d/t" . "group") ("|" . "rail"))
    (("g" . "refresh")  ("?" . "help")      ("q" . "quit")))
  "Project rail Actions legend: three rows of three (KEY . VERB) cells (R26-3).
Every KEY here must resolve to a real command in `org-air-project-mode-map'
— the round-26 legend-truth ERT derives its assertions from THIS table, so
the legend text can never drift from the keymap again.  `s/d/t' names the
three grouping keys; `S-RET visit' and `\\ clear' surface in `?' help.")

(defun org-air-project--insert-actions (width)
  "Insert the project rail Actions block fitted to rail content WIDTH (R26-3).
Same SHAPE + keycap idiom as the board's Actions: three column-aligned verb
rows built from `org-air-project--actions-table' — the REAL project keys:
open / flip / filter, sort / group / rail, refresh / help / quit."
  (org-air-view--rail-header "Actions" width)
  (let* ((inset (org-air-view--rail-inset-str width))
         (rows org-air-project--actions-table)
         (cellw (lambda (cell) (+ (length (car cell)) 1 (length (cdr cell)))))
         (c1 (apply #'max (mapcar (lambda (r) (funcall cellw (nth 0 r))) rows)))
         (c2 (apply #'max (mapcar (lambda (r) (funcall cellw (nth 1 r))) rows)))
         (gap (if (>= width 38) "    " " ")))
    (dolist (row rows)
      (insert (org-air-view--pad-to
               (concat inset
                       (org-air-view--verb-cell
                        (car (nth 0 row)) (cdr (nth 0 row)) c1)
                       gap
                       (org-air-view--verb-cell
                        (car (nth 1 row)) (cdr (nth 1 row)) c2)
                       gap
                       (org-air-view--verb-cell
                        (car (nth 2 row)) (cdr (nth 2 row)) 0))
               width)
              "\n"))))

(defun org-air-project--two-pane-body (docs left-fn width)
  "Return (BODY-LINES . FILL-ROW) composing the LEFT pane | project-rail.
LEFT-FN is a one-arg closure that inserts the left pane content at a given
width (state/tag sections OR the R20-5 directory tree); the RIGHT rail is
the project rail (Summary + Inspector) for DOCS.  The rail is sized to the
doc-pane height so the divider runs the full body and the layout is
deterministic."
  (let* ((rail-width (org-air-view--rail-width width))
         (divider (org-air-view--divider))
         (item-width (max 20 (- width rail-width (string-width divider))))
         (doc-lines (org-air-view--render-lines
                     item-width
                     (lambda () (funcall left-fn item-width))))
         (doc-h (max 1 (length doc-lines)))
         ;; R20-5(b): render the SHARED board rail, sized (via the
         ;; descriptor's :rail-target-height) to MAX(doc pane, window body)
         ;; so the inspector fills the rail in a real (tall) window while
         ;; the divider still spans a long doc list (doc-h > the window).
         ;; `org-air-show-inspector' follows the project's own toggle.
         (target-h (max doc-h (max 1 (- (org-air-view--render-height) 3))))
         (rail-lines
          (let ((org-air-view--rail-descriptor
                 (plist-put (copy-sequence org-air-view--rail-descriptor)
                            :rail-target-height target-h))
                (org-air-show-inspector org-air-project-show-inspector))
            (mapcar
             (lambda (l) (org-air-view--pad-to l rail-width))
             (org-air-view--render-lines
              rail-width
              (lambda () (org-air-view--insert-rail docs rail-width)))))))
    (setq org-air-view--inspector-geom
          (list :item-width item-width :divider divider :rail-width rail-width
                :region-height org-air-view--inspector-region-height))
    (cons (org-air-view--compose-columns
           (list (cons doc-lines item-width) (cons rail-lines rail-width))
           divider)
          (concat (make-string item-width ?\s) divider
                  (make-string rail-width ?\s)))))

(defun org-air-project--inspector-doc-fields (doc inset content-w now)
  "Return the project DOC's inspector body lines (forward order) (R14 D-P1.B).
Title / State / Path (full) / tags / Group / Created / Updated, the same KV
layout + breathing as the board (`org-air-view--inspector-fields-function').
INSET is the spine prefix, CONTENT-W the wrap width, NOW the render clock."
  (let ((state (org-air-doc-state doc))
        lines)
    (dolist (tl (org-air-view--inspector-title-lines
                 (or (org-air-doc-name doc) "") content-w
                 org-air-inspector-max-title-lines))
      (push (concat inset (propertize tl 'face 'org-air-face-title)) lines))
    (push (org-air-view--inspector-kv
           "State"
           ;; R21.1: shared svg badge (no GUI emoji); `[R]' token on TTY.
           (concat (org-air-project--state-badge-cell state)
                   " "
                   (propertize (org-air-project--state-title state)
                               'face (org-air-project--state-face state)))
           inset)
          lines)
    (push (org-air-view--inspector-kv
           "Path"
           (propertize (abbreviate-file-name (org-air-doc-file doc))
                       'face 'org-air-face-faded)
           inset)
          lines)
    (let ((tagstr (mapconcat
                   (lambda (tg) (propertize (concat "#" tg)
                                            'face (org-air-faces-tag-face tg)))
                   (org-air-doc-tags doc) " ")))
      (unless (string-empty-p tagstr)
        (dolist (tl (org-air-view--word-wrap tagstr content-w))
          (push (concat inset tl) lines))))
    (let ((grp (car (split-string (org-air-doc-relpath doc) "/"))))
      (when (and grp (not (string-empty-p grp))
                 (string-match-p "/" (org-air-doc-relpath doc)))
        (push (org-air-view--inspector-kv
               "Group" (propertize grp 'face 'org-air-face-faded) inset)
              lines)))
    (push "" lines)
    (when-let* ((c (org-air-doc-created doc)))
      (push (org-air-view--inspector-kv
             "Created"
             (concat (propertize (format-time-string "%F" c) 'face 'org-air-face-faded)
                     "  "
                     (propertize (format "(%s)" (org-air-view--inspector-relative c now))
                                 'face 'org-air-face-faded))
             inset)
            lines))
    (when-let* ((u (org-air-doc-updated doc)))
      (push (org-air-view--inspector-kv
             "Updated"
             (concat (propertize (format-time-string "%F" u) 'face 'org-air-face-faded)
                     "  "
                     (propertize (format "(%s)" (org-air-view--inspector-relative u now))
                                 'face 'org-air-face-faded))
             inset)
            lines))
    (nreverse lines)))

(defun org-air-project--render (root)
  "Render the Air project view for ROOT into the current buffer (R14 D-P1).
Two-line doc blocks in state-bucket sections; two-pane (docs + a Summary/
Inspector rail) above `org-air-rail-min-width', board-only below it."
  ;; R26-5: seed the per-buffer rail placement ONCE (the `unset' sentinel)
  ;; from `org-air-rail-placement' — the project defaults to the popped
  ;; side-window rail, no `|' required.  Interactive only: batch never
  ;; touches the sentinel (the `unset'-is-not-popped normalisation lives in
  ;; `org-air-rail--popped-p'), so byte goldens and legacy sentinel
  ;; assertions are untouched.  Thereafter the toggle + reconciler own the
  ;; flag.  R27-2: seeded BEFORE the width resolution below, so the FIRST
  ;; render already ensures the rail and composes at the real (shrunk)
  ;; host width instead of the pre-pop width.
  (when (and (not noninteractive)
             (eq org-air-view--rail-popped-out 'unset))
    (setq-local org-air-view--rail-popped-out
                (eq (alist-get 'project org-air-rail-placement)
                    'side-window)))
  (let* ((inhibit-read-only t)
         ;; R27-1 S3: latch the reconciler for the FULL render extent (the
         ;; board binds the same latch) so a nested reconcile timer can
         ;; never mutate rail state mid-render.
         (org-air-rail--reconciling t)
         ;; R27-2: compose at the REAL window body width after the rail
         ;; geometry settles (the helper is a no-op for inline/batch).
         (width (org-air-project--host-width))
         (org-air-project--width width)
         ;; drive the shared row primitive's width seam.
         (org-air-view-width width)
         (dims (org-air-view--char-dimensions))
         (org-air-view--pill-char-w (car dims))
         (org-air-view--pill-char-h (cdr dims))
         ;; R18 D-P3 / R24-6: the shared filter core thins the docs exactly
         ;; as it thins board items, now via `--tokens-pass-filter-p' so a
         ;; bare token substring-matches the doc name + relpath (and tag
         ;; names) while a `#tag' token still tag-matches.
         (docs (seq-filter
                (lambda (d) (org-air-view--tokens-pass-filter-p
                             (concat (org-air-doc-name d) " "
                                     (org-air-doc-relpath d))
                             (org-air-doc-tags d)))
                (org-air-project--collect-docs root)))
         ;; R20-5: `directory' renders the NESTED tree (matching airctl
         ;; -Da); state/tag stay the flat state-bucket / tag sections.
         (directoryp (eq org-air-project-group 'directory))
         (tree (when directoryp (org-air-project--directory-tree docs)))
         (sections (unless directoryp (org-air-project--sections docs)))
         ;; R28-5: carry the buffer-local `(' flip across the R26-7 pane
         ;; seam — the inline two-pane body composes in a TEMP buffer
         ;; (`org-air-view--render-lines') where the flag falls back to
         ;; its global default, so a flipped inline render silently showed
         ;; titles again (the side-window/board-only paths run left-fn in
         ;; the real buffer and never hit this).
         (flip org-air-project--show-filenames)
         ;; R21-5: compute the fixed metadata column widths over the
         ;; DISPLAYED docs at the ACTUAL render width W (board parity:
         ;; cap + title-protecting fit), and bind them for the row pass so
         ;; the one-line rows line up exactly like the board's V6 table.
         (left-fn
          (lambda (w)
            (let* ((org-air-project--show-filenames flip)
                   (mw (org-air-project--fit-meta-widths docs w))
                   (org-air-project--meta-date-w (nth 0 mw))
                   (org-air-project--meta-tags-w (nth 1 mw))
                   (org-air-project--meta-origin-w (nth 2 mw)))
              (if directoryp
                  (org-air-project--insert-directory-tree tree w)
                (org-air-project--insert-doc-sections sections w))))))
    ;; R20-2: cache the doc count for the status mode-line :eval.
    (setq-local org-air-project--doc-count (length docs))
    ;; R22-5: expose the docs as the shared `org-air-view--items' so a
    ;; POPPED-OUT project rail (the shared `org-air-rail--render', which
    ;; reads this back-pointer) renders the project's docs in the side
    ;; window — the same primitive the board uses.
    (setq-local org-air-view--items docs)
    ;; R14 D-P1.B: this buffer hosts the SHARED mid-rail inspector with the
    ;; project's property + fields function.
    (setq-local org-air-view--inspector-active (and org-air-project-show-inspector t)
                org-air-view--inspector-property 'org-air-doc
                org-air-view--inspector-fields-function
                #'org-air-project--inspector-doc-fields)
    ;; R20-5(b): drive the SHARED board rail (Calendar/Filter/Scope/Summary/
    ;; Inspector/Actions) via a view descriptor, so the project rail is the
    ;; board rail — no bespoke parallel rail.
    (setq-local org-air-view--rail-descriptor
                (list :visible-fn (lambda (ds) ds)
                      :calendar-fn
                      (lambda (ds w inset)
                        (org-air-calendar-insert-month
                         org-air-view--cal-month ds w inset
                         (org-air-project--calendar-marks ds)))
                      :summary-fn #'org-air-project--insert-summary
                      ;; The inspector fills from point (`--setup-inspector');
                      ;; seed it on nothing, exactly as the old project rail.
                      :first-thing-fn (lambda (_ds) nil)
                      :actions-fn #'org-air-project--insert-actions))
    ;; R16 D-P4: seed the per-buffer sort state from the defcustoms once.
    (unless org-air-project--sort-key
      (setq-local org-air-project--sort-key org-air-project-sort-key))
    (unless org-air-project--sort-direction
      (setq-local org-air-project--sort-direction org-air-project-sort-direction))
    (erase-buffer)
    ;; R22-5: when the rail is POPPED OUT, render the doc pane LEFT-ONLY and
    ;; push the project rail into the shared `*org-air-rail*' side window
    ;; (reusing the board's side-window primitives).  `unset' (the initial
    ;; sentinel) is NOT popped out (`org-air-rail--popped-p', R26-5).
    (setq org-air-view--orientation
          (cond
           ((org-air-rail--popped-p) 'side-window)
           ((and org-air-project-show-inspector
                 (not (org-air-view--board-only-p width)))
            'two-pane)
           (t 'board-only)))
    (insert (org-air-project--header-line width) "\n\n")
    (cond
     ((null docs)
      (insert "  "
              (propertize "No Air documents found here." 'face 'org-air-face-empty)
              "\n"))
     ((eq org-air-view--orientation 'two-pane)
      (let ((body (car (org-air-project--two-pane-body docs left-fn width))))
        (org-air-view--insert-lines body)))
     ((eq org-air-view--orientation 'side-window)
      ;; R22-5: the rail lives in the side window now — doc pane only.
      (setq org-air-view--inspector-region-height nil)
      (funcall left-fn width))
     (t
      ;; board-only: the state summary + the full-width left pane.
      (setq org-air-view--inspector-region-height nil)
      (org-air-project--insert-state-summary-line docs)
      (funcall left-fn width)))
    ;; Drop the trailing newline so the buffer is exactly its line count.
    (goto-char (point-max))
    (when (and (bolp) (> (point-max) (point-min))) (delete-char -1))
    (goto-char (point-min))
    (org-air-project--next-doc)
    (setq org-air-project--rendered-width width)
    ;; Locate + fill the inspector region (real buffer; buffer-locals set).
    (org-air-view--setup-inspector)
    ;; R22-5: side-window rail lifecycle, mirroring the board: show/refresh
    ;; the popped-out rail; a responsive narrow teardown hides it.  Two-pane
    ;; (inline rail) leaves any rail buffer untouched — the pop-IN path in
    ;; `org-air-rail-toggle' hides the side window before re-rendering.
    (cond
     ((eq org-air-view--orientation 'side-window)
      (org-air-rail--show (current-buffer) width))
     ((eq org-air-view--orientation 'board-only)
      (org-air-rail--hide (current-buffer))))
    ;; R25-6: an INLINE (two-pane) self-render must also evict a stale side
    ;; rail owned by ANOTHER view (the cross-view sweep); when SELF is
    ;; popped `--show' already re-owned the window so this no-ops.
    (org-air-rail--evict-foreign-rail (current-buffer))))

(defun org-air-project--resize-refresh ()
  "Re-render the project view when the displaying window changed (R14 D-P1.B).
Rides the round-9 C1 resize path so widening/narrowing the window flips
between two-pane and board-only."
  (let ((width (org-air-project--host-width)))
    (unless (eql width org-air-project--rendered-width)
      (when org-air-project--root
        (org-air-project--render org-air-project--root)))))

(defun org-air-project--next-doc ()
  "Move point to the next doc row, if any (landing on its title; R21-2)."
  (let ((pos (next-single-property-change (point) 'org-air-doc)))
    (when pos
      (goto-char pos)
      (org-air-view--goto-row-title))))


;;;; ---------------------------------------------------------------------
;;;; Commands + mode
;;;; ---------------------------------------------------------------------

(defun org-air-project-refresh ()
  "Re-render the current Air project view."
  (interactive)
  (when org-air-project--root
    (org-air-project--render org-air-project--root)))

(defun org-air-project--render-current ()
  "Re-render the current project view (R22-5: the rail-toggle dispatch target).
Non-interactive sibling of `org-air-project-refresh' used by the shared
`org-air-view--refresh-current' so the rail toggle never forks."
  (when org-air-project--root
    (org-air-project--render org-air-project--root)))

(defun org-air-project-group-by-state ()
  "Group the project view by state (airctl -a)."
  (interactive)
  (setq org-air-project-group 'state)
  (org-air-project-refresh))

(defun org-air-project-group-by-directory ()
  "Group the project view by directory (airctl -Da)."
  (interactive)
  (setq org-air-project-group 'directory)
  (org-air-project-refresh))

(defun org-air-project-group-by-tag ()
  "Group the project view by tag (airctl -Ta)."
  (interactive)
  (setq org-air-project-group 'tag)
  (org-air-project-refresh))

(defun org-air-project-sort-cycle ()
  "Cycle the project sort key and refresh (R22-3: shared sort core).
Thin alias of `org-air-view-sort-cycle' (the inherited `o'); the project
mode seeds the shared spec (name/created/updated + refresh)."
  (interactive)
  (org-air-view-sort-cycle))

(defun org-air-project-sort-reverse ()
  "Toggle the project sort direction and refresh (R22-3: shared sort core).
Thin alias of `org-air-view-sort-reverse' (the inherited `O')."
  (interactive)
  (org-air-view-sort-reverse))

(defun org-air-project-sort-set (key)
  "Set the sort KEY directly (name/created/updated) and refresh (R16 D-P4).
R22-3: writes the SHARED `org-air-view--sort-key' the comparator reads."
  (interactive
   (list (intern (completing-read "Sort by: " '("name" "created" "updated")
                                  nil t))))
  (setq-local org-air-view--sort-key key)
  (org-air-project-refresh)
  (message "org-air project: sort by %s" key))

(defun org-air-project-next ()
  "Move point to the next doc row, landing on its title (R21-2)."
  (interactive)
  (let ((pos (next-single-property-change
              (line-end-position) 'org-air-doc)))
    (when pos
      (goto-char pos)
      (org-air-view--goto-row-title))))

(defun org-air-project-prev ()
  "Move point to the previous doc row, landing on its title (R21-2)."
  (interactive)
  (let ((pos (previous-single-property-change
              (line-beginning-position) 'org-air-doc)))
    (when pos
      (goto-char pos)
      (goto-char (or (previous-single-property-change pos 'org-air-doc)
                     (line-beginning-position)))
      (org-air-view--goto-row-title))))

(defun org-air-project-visit ()
  "Visit the Air doc on the current row."
  (interactive)
  (let ((doc (get-text-property (point) 'org-air-doc)))
    (if doc
        (find-file-other-window (org-air-doc-file doc))
      (user-error "No Air document on this line"))))

(defun org-air-project-open ()
  "Open the Air doc at point in the SAME window (R26-3 / R26-5 session).
TREE -> DOC: RET replaces the project tree with the doc's file buffer in
the window the tree occupies — no `display-buffer' to fight, nothing to
swallow (the R26-3b root cause).  The session is stashed (window + point)
so the back verbs restore the tree exactly; a popped side rail flips to
the DOC context (outline + meta + legend).  `v' keeps the bottom peek
pane; S-RET visits in the other window."
  (interactive)
  (let ((doc (get-text-property (point) 'org-air-doc)))
    (unless doc
      (user-error "No Air document on this line"))
    (let* ((tree (current-buffer))
           (win (selected-window))
           (popped (org-air-rail--popped-p))
           (buf (find-file-noselect (org-air-doc-file doc))))
      ;; Stash the session on the TREE side (restore target for `back').
      (setq-local org-air-project--session (list :point (point) :window win))
      (pop-to-buffer-same-window buf)
      (with-current-buffer buf
        (setq-local org-air-project--session-tree tree)
        ;; The DOC half carries the session's rail state so the R25-6
        ;; reconciler keeps (or re-pops) the side window for the session.
        (setq-local org-air-view--rail-popped-out (and popped t))
        (setq-local org-air-view--rail-descriptor
                    (org-air-project--doc-rail-descriptor buf doc))
        (org-air-doc-session-mode 1))
      ;; The side window flips to the DOC context (outline + legend).
      (when popped
        (org-air-project--doc-rail-show buf)))))

(defun org-air-project-back ()
  "DOC -> TREE: restore the project tree into the SAME window (R26-5).
Point lands back on the originating doc row; the side window shows the
project rail again.  The doc FILE buffer survives (unsaved edits are never
thrown away) — only the windows swap.  Bound in the doc buffer via
`org-air-doc-session-mode-map' (\\<org-air-doc-session-mode-map>\\[org-air-project-back],
and any `quit-window' remap), and to plain `q' in the read-only
DOC-context side rail."
  (interactive)
  (let ((docbuf (current-buffer))
        (tree org-air-project--session-tree))
    (unless tree
      (user-error "No project doc session in this buffer"))
    (unless (buffer-live-p tree)
      (user-error "The project tree buffer is gone"))
    (let* ((session (buffer-local-value 'org-air-project--session tree))
           (win (or (get-buffer-window docbuf)
                    (let ((w (plist-get session :window)))
                      (and (window-live-p w) w))
                    (selected-window)))
           ;; The session's CURRENT rail state: a user close during the
           ;; DOC state falls back inline (R25-6 user-close rule).
           (popped (org-air-rail--popped-p docbuf)))
      ;; Leave the session; the doc buffer survives for the next RET.
      (org-air-doc-session-mode -1)
      (setq-local org-air-project--session-tree nil)
      (kill-local-variable 'org-air-view--rail-popped-out)
      (kill-local-variable 'org-air-view--rail-descriptor)
      ;; The SAME window shows the tree again.
      (set-window-buffer win tree)
      (select-window win)
      (with-current-buffer tree
        (setq-local org-air-view--rail-popped-out (and popped t))
        (if popped
            ;; Re-own the side window with the PROJECT rail content.
            (org-air-rail--show tree (org-air-project--render-width))
          ;; Rail-less (inline or user-closed): re-render the tree so the
          ;; inline rail reflects the session's final state.
          (org-air-view--refresh-current))
        ;; Land point back on the originating row (after any re-render).
        (when-let* ((pt (plist-get session :point)))
          (let ((pt (min pt (point-max))))
            (goto-char pt)
            ;; R29-2: the doc-session return tail normalizes explicitly —
            ;; a restored dead column (before the doc title) is corrected
            ;; immediately, not on the next keystroke.
            (org-air-view--normalize-point-now)
            (set-window-point win (point))))))))

(defun org-air-project--doc-session-cleanup ()
  "Kill-buffer guard: a killed session DOC hands the window back (R26-5).
The dead owner's window shows the tree again and the side rail re-owns to
the tree buffer (TREE state), so killing the doc mid-session never strands
the session."
  (when (buffer-live-p org-air-project--session-tree)
    (let ((tree org-air-project--session-tree)
          (win (get-buffer-window (current-buffer)))
          (popped (org-air-rail--popped-p)))
      (setq-local org-air-project--session-tree nil)
      ;; R28-4: a killed session doc must not leave a pending highlight
      ;; tick or a stale overlay behind.
      (org-air-rail--outline-highlight-teardown)
      (when (window-live-p win)
        (set-window-buffer win tree))
      (with-current-buffer tree
        (setq-local org-air-view--rail-popped-out (and popped t))
        (when (and popped (window-live-p (org-air-rail--side-window)))
          (org-air-rail--show tree (org-air-project--render-width)))))))

(defun org-air-project--doc-outline (docbuf)
  "Return DOCBUF's Org outline as a list of (LEVEL TITLE POS) (R26-5)."
  (with-current-buffer docbuf
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let (rows)
          (while (re-search-forward "^\\(\\*+\\)[ \t]+\\(.*\\)$" nil t)
            (push (list (length (match-string 1))
                        (string-trim (match-string-no-properties 2))
                        (match-beginning 0))
                  rows))
          (nreverse rows))))))

(defun org-air-project--insert-doc-context (docbuf doc width)
  "Insert the DOC-context rail body for DOC shown in DOCBUF (R26-5).
A meta block (state badge + title + tags), then the Outline: one row per
heading, indented by level, each carrying `org-air-doc-heading-pos' so RET
in the rail jumps the main window to that heading."
  (let ((inset (org-air-view--rail-inset-str width)))
    (org-air-view--rail-header "Document" width)
    (insert (org-air-view--pad-to
             (concat inset
                     (org-air-project--state-badge-cell
                      (org-air-doc-state doc))
                     " "
                     (propertize (or (org-air-doc-name doc) "")
                                 'face 'org-air-face-title))
             width)
            "\n")
    (let ((tagstr (mapconcat
                   (lambda (tg) (propertize (concat "#" tg)
                                            'face (org-air-faces-tag-face tg)))
                   (org-air-doc-tags doc) " ")))
      (unless (string-empty-p tagstr)
        (insert (org-air-view--pad-to (concat inset tagstr) width) "\n")))
    (insert "\n")
    (org-air-view--rail-header "Outline" width)
    (let ((rows (org-air-project--doc-outline docbuf)))
      (if (null rows)
          (insert (org-air-view--pad-to
                   (concat inset (propertize "no headings"
                                             'face 'org-air-face-faded))
                   width)
                  "\n")
        (pcase-dolist (`(,level ,title ,pos) rows)
          (insert (propertize
                   (org-air-view--pad-to
                    (concat inset (make-string (* 2 (1- level)) ?\s) title)
                    width)
                   'org-air-doc-heading-pos pos)
                  "\n"))))))

(defun org-air-project--doc-back-key (docbuf)
  "Return the key text for the session back verb LIVE in DOCBUF (R28-3).
R30-2: a thin wrapper over the generalised `org-air-view--legend-key' —
the same `where-is' derivation with the session map's own back binding as
the defensive fallback — so the legend can never regress to the
self-inserting `q'."
  (org-air-view--legend-key #'org-air-project-back docbuf "C-c C-q"))

(defvar org-air-rail--outline-timer nil
  "Single debounce slot for the R28-4 rail-outline highlight tick.
Rescheduled (never stacked) on every doc-session command — the R27-1 S3
timer discipline.")

(defvar org-air-rail--outline-overlay nil
  "The ONE current-heading overlay in the rail outline (R28-4).
Overlay-only: overlays are not buffer text, so the rail byte goldens
\(`buffer-substring' reads) never move.  Deleted (no highlight) on any
error — graceful degrade, never flicker.")

(defun org-air-rail--outline-highlight-clear ()
  "Delete the R28-4 rail-outline overlay (the no-highlight degrade)."
  (when (overlayp org-air-rail--outline-overlay)
    (delete-overlay org-air-rail--outline-overlay)))

(defun org-air-rail--outline-highlight-update (docbuf)
  "Re-place the rail-outline current-heading overlay for DOCBUF (R28-4).
The current heading is the LAST rail outline row whose
`org-air-doc-heading-pos' is <= point in DOCBUF (a linear scan over the
rail's few dozen rows — no Org re-parse); point before the first heading
means NO current row.  Paints by `move-overlay' of the single overlay
onto the row's line — NO re-render, NO buffer text change (the R27-1
stamp guard sees nothing).  Wrapped in `condition-case': on ANY error
the overlay is deleted — no highlight, never a flicker or a message."
  (setq org-air-rail--outline-timer nil)
  (condition-case nil
      (let* ((rail (get-buffer org-air-rail-buffer-name))
             (pt (and (buffer-live-p docbuf)
                      (buffer-local-value 'org-air-doc-session-mode docbuf)
                      (buffer-local-value 'org-air-project--session-tree
                                          docbuf)
                      (with-current-buffer docbuf (point)))))
        (if (not (and pt (buffer-live-p rail)))
            (org-air-rail--outline-highlight-clear)
          (with-current-buffer rail
            (let (row-bol row-eol)
              (save-excursion
                (goto-char (point-min))
                (while (not (eobp))
                  (let ((hp (get-text-property
                             (point) 'org-air-doc-heading-pos)))
                    (when (and hp (<= hp pt))
                      (setq row-bol (line-beginning-position)
                            row-eol (line-end-position))))
                  (forward-line 1)))
              (if (null row-bol)
                  (org-air-rail--outline-highlight-clear)
                (if (overlayp org-air-rail--outline-overlay)
                    ;; `move-overlay' also revives an evaporated/detached
                    ;; overlay after a rail repaint's `erase-buffer'.
                    (move-overlay org-air-rail--outline-overlay
                                  row-bol row-eol rail)
                  (setq org-air-rail--outline-overlay
                        (make-overlay row-bol row-eol rail))
                  (overlay-put org-air-rail--outline-overlay
                               'face 'org-air-face-outline-current)
                  (overlay-put org-air-rail--outline-overlay
                               'evaporate t)))))))
    (error (org-air-rail--outline-highlight-clear))))

(defun org-air-project--outline-post-command ()
  "Doc-session hook: schedule the DEBOUNCED outline highlight (R28-4).
Buffer-local `post-command-hook' in the session DOC buffer only;
interactive-only (never installed under `noninteractive').  ONE idle
timer slot, rescheduled — never stacked (R27-1 S3)."
  (when (and (not noninteractive) org-air-doc-session-mode)
    (when (timerp org-air-rail--outline-timer)
      (cancel-timer org-air-rail--outline-timer))
    (setq org-air-rail--outline-timer
          (run-with-idle-timer 0.1 nil
                               #'org-air-rail--outline-highlight-update
                               (current-buffer)))))

(defun org-air-rail--outline-highlight-teardown ()
  "Cancel the R28-4 timer slot + delete the overlay (session end)."
  (when (timerp org-air-rail--outline-timer)
    (cancel-timer org-air-rail--outline-timer))
  (setq org-air-rail--outline-timer nil)
  (org-air-rail--outline-highlight-clear))

(defun org-air-project--insert-doc-actions (width &optional docbuf)
  "Insert the DOC-context rail Actions legend at WIDTH (R26-5/R28-3).
R28-3: the back cell's key text is DERIVED from the LIVE binding of
`org-air-project-back' in DOCBUF (the session buffer) — never the
hardcoded `q back' lie (`q' SELF-INSERTS in the editable doc file
buffer; the real verb is the mode map's back binding).  `RET jump' and
`| rail' stay
literal: they are rail-buffer bindings, truthful for the buffer the
legend lives in.  Cells are sized from their CONTENT and laid out
greedily left-to-right with the existing gap; a cell that would cross
WIDTH starts a NEW inset row — wrap, never overflow."
  (org-air-view--rail-header "Actions" width)
  (let* ((inset (org-air-view--rail-inset-str width))
         (gap (if (>= width 38) "    " " "))
         ;; R30-2: EVERY cell's key is derived from the LIVE binding in
         ;; DOCBUF via `org-air-view--legend-key' — `RET'/`|' self-insert
         ;; in the editable doc buffer, so the legend shows the LEADER
         ;; form (C-c C-a o / C-c C-a |) that is actually reachable there.
         (cells (list (org-air-view--verb-cell
                       (org-air-project--doc-back-key docbuf) "back" 0)
                      (org-air-view--verb-cell
                       (org-air-view--legend-key
                        #'org-air-outline-goto-current-heading docbuf
                        "C-c C-a o")
                       "jump" 0)
                      (org-air-view--verb-cell
                       (org-air-view--legend-key
                        #'org-air-rail-toggle docbuf "C-c C-a |")
                       "rail" 0)))
         (line inset))
    (dolist (cell cells)
      (cond
       ((equal line inset)                  ; first cell on the row: always
        (setq line (concat line cell)))
       ((<= (+ (string-width line) (string-width gap) (string-width cell))
            width)                          ; fits after the gap
        (setq line (concat line gap cell)))
       (t                                   ; would cross WIDTH: wrap
        (insert (org-air-view--pad-to line width) "\n")
        (setq line (concat inset cell)))))
    (unless (equal line inset)
      (insert (org-air-view--pad-to line width) "\n"))))

(defun org-air-project--doc-rail-descriptor (docbuf doc)
  "Return the DOC-context rail descriptor for DOCBUF showing DOC (R26-5).
An `:outline-fn' + `:actions-fn' pair on the EXISTING rail descriptor seam
\(one renderer, parameterised — never forked).  R28-3: the descriptor
closes over DOCBUF for `:actions-fn' too, so the Actions legend derives
its back cell from the LIVE session buffer's bindings."
  (list :outline-fn (lambda (w)
                      (org-air-project--insert-doc-context docbuf doc w))
        :actions-fn (lambda (w)
                      (org-air-project--insert-doc-actions w docbuf))))

(defun org-air-project--doc-rail-show (docbuf)
  "Show/re-render the DOC-context side rail owned by DOCBUF (R26-5).
R29-1: the host fallback width measures the window's USABLE columns
\(`org-air-layout--usable-columns', not raw `window-body-width') so a
fringe-less GUI never composes one column past the displayable area."
  (let ((win (get-buffer-window docbuf)))
    (org-air-rail--show docbuf (if (window-live-p win)
                                   (max 40 (org-air-layout--usable-columns win))
                                 80))))

(defun org-air-project--doc-rail-refresh (docbuf)
  "Refresh DOCBUF's doc-context rail per its popped flag (R26-5).
The doc-session leg of `org-air-view--refresh-current' (the `|' toggle)."
  (if (org-air-rail--popped-p docbuf)
      (org-air-project--doc-rail-show docbuf)
    (org-air-rail--hide docbuf)))

(defvar org-air-doc-session-mode-map
  (let ((map (make-sparse-keymap)))
    ;; R20-3a rule: the doc FILE buffer is EDITABLE, so plain `q' must
    ;; stay self-insert HERE; the back verbs are C-c C-q + the quit-window
    ;; remap.  Plain `q' -> back lives in the read-only side rail.
    (define-key map (kbd "C-c C-q") #'org-air-project-back)
    (define-key map [remap quit-window] #'org-air-project-back)
    map)
  "Keymap for `org-air-doc-session-mode' (R26-5).")

(defvar org-air-doc-leader-map
  (let ((map (make-sparse-keymap)))
    ;; R30-2: the doc-session subset of the main-window leader — rail
    ;; toggle, outline jump/next/prev, and the session back verb, all
    ;; reachable from the EDITABLE doc org buffer where single keys
    ;; self-insert.  Every binding reuses an existing command (no fork).
    (define-key map (kbd "|") #'org-air-rail-toggle)
    (define-key map (kbd "o") #'org-air-outline-goto-current-heading)
    (define-key map (kbd "n") #'org-air-outline-next-heading)
    (define-key map (kbd "p") #'org-air-outline-prev-heading)
    (define-key map (kbd "q") #'org-air-project-back)
    map)
  "Leader prefix map for the doc-session content buffer (R30-2).
Installed at `org-air-leader-key' on `org-air-doc-session-mode-map'.  The
direct back binding still wins `where-is', so the R28-3 back legend is
unchanged; this leader is purely ADDITIVE.")

;; R30-2: install the leader on the doc-session map (nav/back/rail subset).
(org-air-view--leader-install org-air-doc-session-mode-map
                              org-air-doc-leader-map)

(define-minor-mode org-air-doc-session-mode
  "Minor mode in a doc FILE buffer opened from the project tree (R26-5).
The buffer stays fully editable;
\\<org-air-doc-session-mode-map>\\[org-air-project-back] (or any
`quit-window' binding) returns to the tree in the same window.  The header
line names the back verb; a kill mid-session hands the window and side
rail back to the tree."
  :lighter " ↳air"
  :keymap org-air-doc-session-mode-map
  (if org-air-doc-session-mode
      (progn
        (setq-local header-line-format
                    (list (propertize " C-c C-q back to tree"
                                      'face 'org-air-face-faded)))
        (add-hook 'kill-buffer-hook
                  #'org-air-project--doc-session-cleanup nil t)
        ;; R28-4: the rail-outline current-heading follow — one
        ;; buffer-local hook, debounced through one timer slot,
        ;; interactive-only (batch installs NOTHING).
        (unless noninteractive
          (add-hook 'post-command-hook
                    #'org-air-project--outline-post-command nil t)))
    (kill-local-variable 'header-line-format)
    (remove-hook 'kill-buffer-hook
                 #'org-air-project--doc-session-cleanup t)
    (remove-hook 'post-command-hook
                 #'org-air-project--outline-post-command t)
    (org-air-rail--outline-highlight-teardown)))

(defun org-air-project-toggle-filenames ()
  "Flip project doc rows between doc title and relpath (R26-4).  Key `('."
  (interactive)
  (setq-local org-air-project--show-filenames
              (not org-air-project--show-filenames))
  (org-air-project-refresh)
  (message "org-air project: showing %s"
           (if org-air-project--show-filenames "file names" "titles")))

(defun org-air-project-filter (tags)
  "Filter the project doc tree to TAGS (R18 D-P3, shares the board core).
The prompt is PRE-FILLED with the active filter and the chosen terms
combine with the shared `org-air-filter-match' combinator (AND by default,
`M-/' toggles) — the same filter core the board uses, applied to
`org-air-doc-tags'."
  (interactive
   (list (org-air-view--read-filter
          (delete-dups
           (sort (apply #'append
                        (mapcar #'org-air-doc-tags
                                (if org-air-project--root
                                    (org-air-project--collect-docs
                                     org-air-project--root)
                                  nil)))
                 #'string<)))))
  (setq org-air-view--tag-filter (unless (null tags) tags))
  (org-air-project-refresh))

(defun org-air-project-quit ()
  "Quit the project view progressively — ONE surface per press (R28-2).
A live bottom pane closes FIRST (tree alive, point untouched); the next
press tears down a popped-out rail side window (the buffer-local popped
flag survives, so a re-entry re-pops per R26-5) and quits the tree — a
single press can no longer bury the tree while ORPHANING the pane and
the rail on screen."
  (interactive)
  (unless (org-air-view--quit-close-pane)
    (when (org-air-rail--popped-p)
      (org-air-rail--teardown))
    (quit-window)))

(defvar org-air-project-mode-map
  (let ((map (make-sparse-keymap)))
    ;; R20-5(b): a THIN child of the shared view-core map.  Every shared
    ;; board key keeps the board's meaning by INHERITANCE (RET pane,
    ;; mouse-1, v/V pane open/close, \ filter-clear, M-/ AND/OR toggle); the
    ;; old domain verbs that SHADOWED s / d / t / o / O are GONE — a user
    ;; who knows the board drives the project with no relearning.  The
    ;; state / tag groupings and the sort cycle stay reachable via
    ;; `M-x org-air-project-group-by-state' / `-by-tag' / `-sort-cycle', so
    ;; the airctl -a / -Ta parity exists without stealing the shared keys.
    (set-keymap-parent map org-air-view-core-map)
    (define-key map (kbd "n") #'org-air-project-next)
    (define-key map (kbd "p") #'org-air-project-prev)
    ;; R26-3: RET is the SAME-WINDOW doc open (the R26-5 session model) —
    ;; the shared pane-return stays the BOARD's RET only.  `v' keeps the
    ;; bottom peek pane; S-RET keeps the other-window visit.
    (define-key map (kbd "RET") #'org-air-project-open)
    (define-key map (kbd "<mouse-1>") #'org-air-project-open)
    (define-key map (kbd "<S-return>") #'org-air-project-visit)
    (define-key map (kbd "S-RET") #'org-air-project-visit)
    ;; R26-3: airctl -a/-Da/-Ta parity ON KEYS — s/d/t are free in the
    ;; project map (the board's triage verbs never applied here).
    (define-key map (kbd "s") #'org-air-project-group-by-state)
    (define-key map (kbd "d") #'org-air-project-group-by-directory)
    (define-key map (kbd "t") #'org-air-project-group-by-tag)
    ;; R26-4: dired-style `(' flips doc rows filename<->title.
    (define-key map (kbd "(") #'org-air-project-toggle-filenames)
    ;; R26-3: `?' help (project-aware section; special-mode's describe-mode
    ;; fallback is not the legend's promise).
    (define-key map (kbd "?") #'org-air-help)
    ;; The per-mode doc-tag filter (shares the board's pre-fill + AND
    ;; default + M-/ toggle core); `g' refreshes, `q' quits.
    (define-key map (kbd "/") #'org-air-project-filter)
    (define-key map (kbd "g") #'org-air-project-refresh)
    (define-key map (kbd "q") #'org-air-project-quit)
    map)
  "Keymap for `org-air-project-mode'.")

(defvar org-air-project-leader-map
  (let ((map (make-sparse-keymap)))
    ;; R30-2: the project's main-window leader subset — rail toggle,
    ;; outline jump, the shared sort, and the PROJECT doc-tag filter
    ;; (`/' on the project map is `org-air-project-filter', not the
    ;; board's `org-air-filter').  All existing commands (no fork).
    (define-key map (kbd "|") #'org-air-rail-toggle)
    (define-key map (kbd "o") #'org-air-rail-return)
    (define-key map (kbd "s") #'org-air-view-sort-cycle)
    (define-key map (kbd "/") #'org-air-project-filter)
    map)
  "Leader prefix map for the project content buffer (R30-2).
Installed at `org-air-leader-key' on `org-air-project-mode-map'.")

;; R30-2: install the leader on the project map (filter/sort/rail subset).
(org-air-view--leader-install org-air-project-mode-map
                              org-air-project-leader-map)

(define-derived-mode org-air-project-mode special-mode "Org-Air-Project"
  "Major mode for the Air-docs project tree view (F5)."
  (setq-local truncate-lines t)
  (setq-local cursor-type 'box)
  (setq-local line-spacing org-air-line-spacing)
  ;; R18 D-P5.1: the calm nano-style mode-line (status lives in the header).
  (org-air-view--install-modeline)
  ;; R14 D-P1.B: responsive re-render (two-pane <-> board-only) on resize,
  ;; riding the round-9 C1 window-size path.
  (setq-local org-air-layout-refresh-function #'org-air-project--resize-refresh)
  ;; R22-3: seed the SHARED sort spec so the inherited o/O cycle/reverse
  ;; drive the project's name/created/updated sort (one core, no fork).
  (setq-local org-air-view--sort-keys '(name created updated))
  (setq-local org-air-view--sort-refresh #'org-air-project-refresh)
  (unless org-air-view--sort-key
    (setq-local org-air-view--sort-key org-air-project-sort-key))
  (unless org-air-view--sort-direction
    (setq-local org-air-view--sort-direction org-air-project-sort-direction))
  ;; R14 D-P1.B: the project view hosts the shared mid-rail inspector; the
  ;; debounced point-tracking hook is INERT under batch (P0 contract).
  (unless noninteractive
    (add-hook 'post-command-hook #'org-air-view--inspector-post-command nil t)
    ;; R18 D-P3/D-P4: the bottom view pane auto-follows here too (same hook
    ;; as the board; guarded on a live pane window, inert under batch).
    (add-hook 'post-command-hook #'org-air-view--view-pane-post-command nil t)
    ;; R22-2b/R29-2: snap point off the dead gutter/margin/rail/pad columns
    ;; onto the doc row title (project rows carry `org-air-doc' via the
    ;; shared `--insert-row') after any LINE-crossing command — the
    ;; pre-command line snapshot gates the snap so in-row horizontal motion
    ;; is never hijacked; inert in batch.
    (add-hook 'pre-command-hook #'org-air-view--pre-command-snapshot nil t)
    (add-hook 'post-command-hook #'org-air-view--normalize-point nil t))
  ;; R22-5: tear down a popped-out rail side window + buffer when the
  ;; project buffer is killed (it must not outlive its host), mirroring the
  ;; board's kill-buffer-hook.
  (add-hook 'kill-buffer-hook #'org-air-rail--teardown nil t)
  ;; R24-5: install the SAME cooperative reconciler the board has, so a
  ;; natively-closed popped-out PROJECT rail falls back to the inline rail
  ;; (the reconcile guard + popin dispatch are now mode-generic).  Reactive
  ;; only; inert under batch (the window-config hook never fires there).
  (unless noninteractive
    (add-hook 'window-configuration-change-hook #'org-air-rail--reconcile nil t))
  ;; R27-4: the board's evil integration, applied to the project (trunk:
  ;; NONE — under evil's normal state every single project key resolved to
  ;; an evil command: `(' -> evil-backward-sentence-begin, `o' ->
  ;; evil-open-below, RET -> evil-ret…  "ALL the key bindings are weird").
  ;; Motion state + overriding map, exactly the board's proven U2 contract;
  ;; fboundp-gated soft dep — non-evil users untouched.
  (org-air-view--setup-evil 'org-air-project-mode org-air-project-mode-map)
  (org-air-layout-install-window-size-hook)
  (buffer-disable-undo))

;;;###autoload
(defun org-air-project (&optional root)
  "Open the Air project doc-tree view for ROOT.
With several configured projects, prompt for one (`org-air-projects' /
`org-air-sources'); ROOT non-nil opens it directly."
  (interactive)
  (let* ((roots (org-air-project-roots))
         (root (or root
                   (cond
                    ((null roots)
                     (read-directory-name "Air project root: "))
                    ((= (length roots) 1) (car roots))
                    (t (completing-read "Air project: " roots nil t)))))
         (buffer (get-buffer-create "*org-air-project*")))
    (with-current-buffer buffer
      ;; R26-5: IDEMPOTENT entry — re-running the mode on the live buffer
      ;; runs `kill-all-local-variables' and wipes the whole session (rail
      ;; placement, sort, filter, R26-4 flip, expanded sections), which is
      ;; how the re-entry DOUBLE RAIL was born.  Initialise the mode only
      ;; once; a re-entry (or a different root) just re-renders in place.
      (unless (derived-mode-p 'org-air-project-mode)
        (org-air-project-mode))
      (setq org-air-project--root (expand-file-name root)))
    ;; R27-2: display the buffer BEFORE the first render, so the width
    ;; resolution (`org-air-project--host-width') can ensure the rail side
    ;; window and measure the REAL project window body width — the first
    ;; popped render composes at the width it will actually display at
    ;; (trunk composed at the pre-pop width and never re-measured).
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (org-air-project--render org-air-project--root))))

(provide 'org-air-project)

;;; org-air-project.el ends here
