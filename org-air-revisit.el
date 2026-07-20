;;; org-air-revisit.el --- Revisit (evergreen notes) view for org-air -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: org-air contributors
;; Keywords: outlines, calendar
;; Version: 0.1.0
;; URL: https://github.com/rytswd/org-air
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; R54-3: ONE view surfacing KNOWLEDGE notes (the R54-2 note-type model)
;; by attention-age, dustiest first — the resurfacing surface the GTD
;; board deliberately is not.  Everything renders from the scan's
;; per-file fact table (`org-air-query--file-meta') — the DATA-PURE
;; render law: zero per-row file opens, ever; RET on a row is the single
;; user-initiated open.
;;
;; Three surfacing modes inside the one view, cycled on `m' (USER-RULED
;; D2): ALL (every revisit-scope note, oldest last-modified on top),
;; ORPHANS (the link-graph subset nothing links to / that links nowhere)
;; and SPACED (a small deterministic daily handful — notes-garden
;; grazing).  Bounded and paged at 5000+ per the R53 never-hang laws:
;; exactly min(total, `org-air-revisit-page-limit') rows plus the
;; standard fold row; TAB/RET on the fold extends by ONE page.
;;
;; Attention-age is pure file mtime by default (the D2 ruling); the
;; OPT-IN `org-air-revisit-visit-ledger' folds org-air-recorded opens
;; into the age (the ledger lives in org-air-query.el and is written
;; only from org-air's own open paths — never a global find-file hook).
;;
;; Kept out of the 8.7k-line view file per the module split convention;
;; the view/render machinery (shared row primitive, rail, sort core,
;; filter core) is REUSED, never forked.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'org-air-query)
(require 'org-air-view)

(defvar org-air-inbox-file)
;; R58: `bookmark-make-record-function' is bookmark.el's (not preloaded);
;; the mode sets it buffer-locally without requiring bookmark at load.
(defvar bookmark-make-record-function)

;;;; ---------------------------------------------------------------------
;;;; Knobs
;;;; ---------------------------------------------------------------------

(defcustom org-air-revisit-types '(knowledge)
  "Note types the Revisit view surfaces (R54-3, fork F3).
The default keeps journals OUT (timestamped logs are not evergreens);
add `journal' to graze them too."
  :type '(repeat (choice (const knowledge) (const journal)))
  :group 'org-air)

(defcustom org-air-revisit-rail-placement nil
  "REVISIT override for `org-air-rail-placement' (R62-1d).
nil (the default) inherits the shared `org-air-rail-placement'; `inline'
or `side-window' pins the revisit view regardless of the shared default.
Resolved through `org-air-rail--placement'."
  :type '(choice (const :tag "Inherit `org-air-rail-placement'" nil)
                 (const inline) (const side-window))
  :group 'org-air)

(defcustom org-air-revisit-page-limit 200
  "Rows one Revisit page renders before the fold row (R54-3).
TAB/RET on the `…and N more' fold row extends by ONE more page, so the
full 5000-note corpus appears only after deliberate repeated asks and
every render stays O(shown)."
  :type 'integer
  :group 'org-air)

(defcustom org-air-revisit-visit-ledger nil
  "When non-nil, fold org-air-recorded visits into the attention-age (R54-3).
DEFAULT nil (USER-RULED D2: last-modified is the age signal out of the
box; the ledger is never written).  Non-nil: opens initiated FROM org-air
views (board S-RET / g RET, the pane RET, revisit RET) are recorded in
the bounded cache-persisted ledger and age becomes max(mtime, last
visit); the SPACED mode also gains a visited-today tick.  A global
`find-file' hook remains rejected — org-air never instruments buffers it
does not own."
  :type 'boolean
  :group 'org-air)

(defcustom org-air-revisit-daily-count 5
  "Notes the SPACED mode surfaces per day (R54-3, fork F9)."
  :type 'integer
  :group 'org-air)

(defcustom org-air-revisit-orphan-rule 'disconnected
  "Which notes the ORPHANS mode shows (R54-3, fork F8).
`disconnected' (default): no note-links either way — nothing links to it
AND it links nowhere.  `no-inbound' / `no-outbound' test one direction;
`either' is the union reading of the ruling's phrasing."
  :type '(choice (const disconnected) (const no-inbound)
                 (const no-outbound) (const either))
  :group 'org-air)

(defconst org-air-revisit-buffer-name "*org-air revisit*"
  "Name of the Revisit view buffer (R54-3).")

;;;; ---------------------------------------------------------------------
;;;; Buffer state
;;;; ---------------------------------------------------------------------

(defvar-local org-air-revisit--surface 'all
  "Active surfacing mode: `all', `orphans' or `spaced' (R54-3, `m').")

(defvar-local org-air-revisit--pages 1
  "How many `org-air-revisit-page-limit' pages are currently shown.
Reset to 1 by mode/sort/filter changes; the fold row increments it.")

(defvar-local org-air-revisit--show-created nil
  "Non-nil: rows also show the Created date (denote ID / `#+date').
Toggled by `z c' (the columns-prefix convention).")

(defvar-local org-air-revisit--count nil
  "Visible note count of the last render (header + mode-line).")

(defvar-local org-air-revisit--rendered-width nil
  "Width of the most recent Revisit render (the resize-refresh guard).")

(defvar-local org-air-revisit--fill-token 0
  "Monotonic token guarding the cold-fill pacer's slices (R54-3).")

(defvar-local org-air-revisit--fill-queue nil
  "Files the in-flight cold fill has not scanned yet (R54-3).")

(defvar-local org-air-revisit--fill-total 0
  "Total file count of the in-flight cold fill (R54-3).")

(defvar-local org-air-revisit--fill-timer nil
  "The single repeating wall-clock pacer of the cold fill, or nil.")

(defvar-local org-air-revisit--fill-last-paint nil
  "Float time of the last progressive cold-fill repaint, or nil.")

(defvar-local org-air-revisit--bookmark-locator nil
  "Armed point locator of an in-flight bookmark restore, or nil (R58).
The revisit twin of `org-air-view--bookmark-locator': a plist
\(:item (FILE . POS) :title TITLE) consumed at the render tail; stays
armed across the paced cold-fill's progressive paints until the row
appears or the fill goes idle (one-shot either way).")

(defvar org-air-revisit--meta-date-w 0
  "Fixed age-chip column width for the current row pass (V6).")
(defvar org-air-revisit--meta-tags-w 0
  "Fixed tag column width for the current row pass (V6).")
(defvar org-air-revisit--meta-origin-w 0
  "Fixed origin column width for the current row pass (V6).")

;;;; ---------------------------------------------------------------------
;;;; Data — every accessor below reads file-meta / ledger slots ONLY
;;;; (the data-pure render law: no file opens anywhere in this section).
;;;; ---------------------------------------------------------------------

(defun org-air-revisit--scope-entries ()
  "Return the revisit-scope entries (FILE . META), file-name ordered.
The scope is every file-meta entry whose `:ntype' is in
`org-air-revisit-types' — ONE row per FILE (the denote unit), never per
heading — EXCEPT `org-air-inbox-file': the inbox is a triage queue with
its own surface (the Inbox bucket + capture flow), not an evergreen, so
excluding it is view policy at the scope seat (`:ntype' stays
content-derived), mirroring the board's Notes bucket.  The comparison
rides the MEMOISED `org-air-classify--truename' — a hash lookup per
entry, never a `file-truename' walk.  File-name order is the
deterministic base the sorts and the SPACED rotation build on."
  (let (out)
    (maphash (lambda (file meta)
               (when (and (memq (plist-get meta :ntype) org-air-revisit-types)
                          (not (and (boundp 'org-air-inbox-file)
                                    org-air-inbox-file
                                    (equal (org-air-classify--truename file)
                                           (org-air-classify--truename
                                            org-air-inbox-file)))))
                 (push (cons file meta) out)))
             org-air-query--file-meta)
    (sort out (lambda (a b) (string-lessp (car a) (car b))))))

(defun org-air-revisit--entry-age-base (entry)
  "Return ENTRY's attention epoch float (R54-3).
File mtime (the USER-RULED default signal); with the opt-in ledger on,
max(mtime, last org-air visit).  A meta with no mtime reads as epoch 0 —
maximally dusty, never an error."
  (let* ((mtime (or (plist-get (cdr entry) :mtime) 0.0))
         (visit (and org-air-revisit-visit-ledger
                     (org-air-query-note-visit (car entry)))))
    (if visit (max mtime visit) mtime)))

(defun org-air-revisit--entry-age-days (entry &optional now)
  "Return ENTRY's attention-age in whole days relative to NOW (or now)."
  (max 0 (floor (/ (- (float-time (or now (current-time)))
                      (org-air-revisit--entry-age-base entry))
                   86400))))

(defun org-air-revisit--entry-title (entry)
  "Return ENTRY's display title: `#+title' → denote slug → leaf name."
  (or (plist-get (cdr entry) :title)
      (file-name-base (car entry))))

(defun org-air-revisit--entry-origin (entry)
  "Return ENTRY's origin text: the parent directory name + `/'."
  (let ((dir (file-name-directory (car entry))))
    (if dir
        (concat (file-name-nondirectory (directory-file-name dir)) "/")
      "")))

(defun org-air-revisit--entry-orphan-p (entry)
  "Non-nil when ENTRY is an orphan under `org-air-revisit-orphan-rule'.
Two slot reads per row (`:links-in' count, `:links-out' list) — the link
graph is resolved once per scan by `org-air-query-link-graph-ensure',
never here."
  (let ((in (zerop (or (plist-get (cdr entry) :links-in) 0)))
        (out (null (plist-get (cdr entry) :links-out))))
    (pcase org-air-revisit-orphan-rule
      ('no-inbound in)
      ('no-outbound out)
      ('either (or in out))
      (_ (and in out)))))

(defun org-air-revisit--spaced-entries (entries &optional now)
  "Return the SPACED daily handful of ENTRIES (R54-3, USER-RULED D2).
A DETERMINISTIC rotation with zero state on disk: order the scope by the
stable file-name key (ENTRIES already are), take the K-wide window
starting at (mod (* day K) N) where day is `time-to-days' of NOW — the
same handful all day, a fresh handful tomorrow, full corpus coverage
every ceil(N/K) days."
  (let* ((n (length entries))
         (k (max 1 org-air-revisit-daily-count)))
    (if (<= n k)
        entries
      (let ((start (mod (* (time-to-days (or now (current-time))) k) n)))
        (cl-loop for i below k
                 collect (nth (mod (+ start i) n) entries))))))

(defun org-air-revisit--entry-visited-today-p (entry &optional now)
  "Non-nil when ENTRY was org-air-opened on NOW's day (opt-in ledger)."
  (and org-air-revisit-visit-ledger
       (when-let* ((visit (org-air-query-note-visit (car entry))))
         (= (time-to-days visit)
            (time-to-days (or now (current-time)))))))

(defun org-air-revisit--sort-entries (entries)
  "Return ENTRIES ordered by the shared sort key/direction (R22-3 core).
DEFAULT `age' ascending = oldest attention-age first, dustiest on top
\(USER-RULED D2); `created' (denote ID / `#+date', nil last) and `title'
cycle on the inherited `o'; `O' reverses.  Every key is a precomputed
float/string in file-meta — milliseconds-class at 10k entries."
  (let* ((key (or org-air-view--sort-key 'age))
         (desc (eq org-air-view--sort-direction 'descending))
         (sorted
          (pcase key
            ('created
             (sort (copy-sequence entries)
                   (lambda (a b)
                     (let ((ca (plist-get (cdr a) :created))
                           (cb (plist-get (cdr b) :created)))
                       (cond ((and ca cb) (< ca cb))
                             (ca t)
                             (t nil))))))
            ('title
             (sort (copy-sequence entries)
                   (lambda (a b)
                     (string-lessp
                      (downcase (org-air-revisit--entry-title a))
                      (downcase (org-air-revisit--entry-title b))))))
            (_
             (sort (copy-sequence entries)
                   (lambda (a b)
                     (< (org-air-revisit--entry-age-base a)
                        (org-air-revisit--entry-age-base b))))))))
    (if desc (nreverse sorted) sorted)))

(defun org-air-revisit--visible-entries (scope)
  "Return SCOPE after the live filter, the surfacing mode and the sort.
The `/' filter matches title + origin + file leaf + tags through the
shared `org-air-view--tokens-pass-filter-p' (the R24-6 mini-language);
the mode is a pure filter/selection; the sort is the shared core."
  (let* ((filtered
          (seq-filter
           (lambda (entry)
             (org-air-view--tokens-pass-filter-p
              (concat (org-air-revisit--entry-title entry) " "
                      (org-air-revisit--entry-origin entry) " "
                      (file-name-nondirectory (car entry)))
              (plist-get (cdr entry) :tags)))
           scope))
         (surfaced
          (pcase org-air-revisit--surface
            ('orphans (progn (org-air-query-link-graph-ensure)
                             (seq-filter #'org-air-revisit--entry-orphan-p
                                         filtered)))
            ('spaced (org-air-revisit--spaced-entries filtered))
            (_ filtered))))
    (org-air-revisit--sort-entries surfaced)))

;;;; ---------------------------------------------------------------------
;;;; Rows
;;;; ---------------------------------------------------------------------

(defun org-air-revisit--age-text (days)
  "Return the age chip text for DAYS: `dusty Ny Nm' / `dusty Nm' / `dusty Nd'."
  (cond
   ((>= days 365)
    (let ((years (/ days 365))
          (months (/ (% days 365) 30)))
      (if (> months 0)
          (format "dusty %dy %dm" years months)
        (format "dusty %dy" years))))
   ((>= days 60) (format "dusty %dm" (/ days 30)))
   (t (format "dusty %dd" days))))

(defun org-air-revisit--date-text (entry now surface show-created)
  "Return ENTRY's UNFACED date-cell text at NOW (R54-3).
The age chip; with SHOW-CREATED (`z c') the created date follows; in the
SPACED SURFACE a note visited today (opt-in ledger) gains the done-tick
— the rotation keeps its slot (fork F9)."
  (concat (org-air-revisit--age-text
           (org-air-revisit--entry-age-days entry now))
          (when show-created
            (when-let* ((created (plist-get (cdr entry) :created)))
              (format "  %s" (format-time-string "%F" created))))
          (when (and (eq surface 'spaced)
                     (org-air-revisit--entry-visited-today-p entry now))
            (concat " " (org-air-view--glyph 'visited)))))

(defun org-air-revisit--entry-tagstr (entry)
  "Return ENTRY's tag pills via the shared board renderer."
  (let* ((tags (plist-get (cdr entry) :tags))
         (n (length tags)))
    (if (zerop n) ""
      (org-air-view--item-tagstr tags (min org-air-tags-inline-max n) n))))

(defun org-air-revisit--entry-origin-cell (entry)
  "Return ENTRY's `▤ dir/' origin cell text (the F1 column idiom)."
  (let ((text (org-air-revisit--entry-origin entry))
        (budget (max 1 (- org-air-origin-max-width 2))))
    (concat (org-air-view--svg-file-icon (org-air-view--glyph 'origin))
            " "
            (if (<= (string-width text) budget)
                text
              (truncate-string-to-width text budget nil nil
                                        (org-air-view--glyph 'more))))))

(defun org-air-revisit--fit-meta-widths (entries width now surface created)
  "Return fitted (DCOL TCOL OCOL) over the displayed ENTRIES at WIDTH.
Mirrors the board's title-protected fit (R17): measure the shown rows
only — O(page) — cap the origin, then shrink origin → tags until the
flex title keeps `org-air-title-min-width'.  NOW, SURFACE and CREATED
parameterise the date-cell text exactly as it renders."
  (let ((dw 0) (tw 0) (ow 0))
    (dolist (entry entries)
      (setq dw (max dw (string-width (org-air-revisit--date-text
                                      entry now surface created))))
      (setq tw (max tw (string-width
                        (org-air-revisit--entry-tagstr entry))))
      (setq ow (max ow (string-width
                        (org-air-revisit--entry-origin-cell entry)))))
    (setq ow (min ow org-air-origin-max-width))
    (let* ((gap 2)
           (left-reserve (string-width (org-air-view--item-margin)))
           (cluster (lambda (o)
                      (let ((cells (delq nil (list (and (> dw 0) dw)
                                                   (and (> tw 0) tw)
                                                   (and (> o 0) o)))))
                        (+ (apply #'+ cells) (max 0 (1- (length cells)))))))
           (budget (lambda (o)
                     (- width left-reserve gap (funcall cluster o)))))
      (while (and (> ow org-air-origin-min)
                  (< (funcall budget ow) org-air-title-min-width))
        (setq ow (1- ow)))
      (let ((tw-floor (if (> tw 0) 1 0)))
        (while (and (> tw tw-floor)
                    (< (funcall budget ow) org-air-title-min-width))
          (setq tw (1- tw)))))
    (list dw tw ow)))

(defun org-air-revisit--insert-row (entry now surface created)
  "Insert ENTRY as one calm V6 row via the shared primitive.
The whole row carries `org-air-revisit' + `org-air-marker' so point on
any cell identifies the note; every cell is a file-meta/ledger slot.
NOW, SURFACE and CREATED parameterise the date cell."
  (org-air-view--insert-row
   :prefix (org-air-view--item-margin)
   :title (org-air-revisit--entry-title entry)
   :date-text (propertize
               (org-air-revisit--date-text entry now surface created)
               'face 'org-air-face-date)
   :tags (org-air-revisit--entry-tagstr entry)
   :origin-text (org-air-revisit--entry-origin-cell entry)
   :origin-face 'org-air-face-origin
   :widths (list org-air-revisit--meta-date-w
                 org-air-revisit--meta-tags-w
                 org-air-revisit--meta-origin-w)
   ;; The revisit pane composes its OWN cluster field (own globals), so it
   ;; anchors to this row's cluster width — the documented project-style
   ;; no-rail-board exception (R40-2).
   :own-fence t
   :props (list 'org-air-revisit entry
                'org-air-marker (car entry)
                'mouse-face 'org-air-face-cursor)))

(defun org-air-revisit--insert-rows (entries width pages surface created)
  "Insert the paged ENTRIES at WIDTH — the bounded left pane (R54-3).
Renders exactly min(total, PAGES × `org-air-revisit-page-limit') rows,
then the standard fold row (`org-air-more-row', the R51-3 actionable
contract) that TAB/RET extend by ONE page.  SURFACE `spaced' is K rows
by construction and never folds.  CREATED threads the `z c' column."
  (if (null entries)
      (insert (org-air-view--item-margin)
              (propertize (if org-air-revisit--fill-queue
                              "Scanning your notes…"
                            "Nothing to revisit here.")
                          'face 'org-air-face-empty)
              "\n")
    (let* ((now (current-time))
           (limit (max 1 org-air-revisit-page-limit))
           (shown-n (if (eq surface 'spaced)
                        (length entries)
                      (min (length entries) (* (max 1 pages) limit))))
           (shown (seq-take entries shown-n))
           (widths (org-air-revisit--fit-meta-widths
                    shown width now surface created))
           (org-air-revisit--meta-date-w (nth 0 widths))
           (org-air-revisit--meta-tags-w (nth 1 widths))
           (org-air-revisit--meta-origin-w (nth 2 widths)))
      (dolist (entry shown)
        (org-air-revisit--insert-row entry now surface created))
      (when (> (length entries) shown-n)
        (let ((start (point)))
          (insert (org-air-view--item-margin)
                  (propertize (format "%sand %d more — TAB for another page"
                                      (org-air-view--glyph 'more)
                                      (- (length entries) shown-n))
                              'face 'org-air-face-faded
                              'mouse-face 'org-air-face-cursor)
                  (propertize "\n" 'face 'org-air-face-faded))
          (add-text-properties start (point)
                               (list 'org-air-more-row 'revisit)))))))

;;;; ---------------------------------------------------------------------
;;;; Rail (the standard descriptor seam — parameterise, never fork)
;;;; ---------------------------------------------------------------------

(defun org-air-revisit--calendar-marks (entries)
  "Return a date-key → mark table over ENTRIES' created dates."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry entries table)
      (when-let* ((created (plist-get (cdr entry) :created)))
        (let ((d (decode-time created)))
          (puthash (org-air-calendar--date-key (decoded-time-month d)
                                               (decoded-time-day d)
                                               (decoded-time-year d))
                   'created table))))))

(defconst org-air-revisit--age-bands
  '((365 . "> 1y") (90 . "> 90d") (21 . "> 21d") (nil . "fresh"))
  "The Summary age bands: (MIN-DAYS-EXCLUSIVE . LABEL), tried in order.")

(defun org-air-revisit--insert-summary (entries width)
  "Insert the Revisit rail Summary: age-band counts over ENTRIES (R54-3).
Bands >1y / >90d / >21d / fresh, fitted to rail content WIDTH in the
board Summary's row idiom, with the short ledger rule and the total."
  (org-air-view--rail-header "Summary" width)
  (let* ((now (current-time))
         (inset (org-air-view--rail-inset-str width))
         (counts (mapcar (lambda (_band) 0)
                         org-air-revisit--age-bands)))
    (dolist (entry entries)
      (let ((days (org-air-revisit--entry-age-days entry now))
            (i 0))
        (catch 'placed
          (dolist (band org-air-revisit--age-bands)
            (when (or (null (car band)) (> days (car band)))
              (setf (nth i counts) (1+ (nth i counts)))
              (throw 'placed t))
            (setq i (1+ i))))))
    (let ((i 0))
      (dolist (band org-air-revisit--age-bands)
        (let ((count (nth i counts)))
          (insert inset
                  (propertize (format "%3d" count)
                              'face (if (zerop count) 'org-air-face-faded
                                      'org-air-face-summary-number))
                  "   "
                  (propertize (cdr band) 'face 'org-air-face-summary-label)
                  "\n"))
        (setq i (1+ i))))
    (insert inset
            (propertize (make-string
                         4 (string-to-char (org-air-view--glyph 'hrule)))
                        'face 'org-air-face-pane-border)
            "\n")
    (insert inset
            (propertize (format "%3d" (length entries))
                        'face 'org-air-face-summary-number)
            "   " (propertize "notes" 'face 'org-air-face-summary-label)
            "\n")))

(defconst org-air-revisit--actions-table
  '((("RET" . "open")    ("m" . "mode")   ("/" . "filter"))
    (("o" . "sort")      ("|" . "rail")   ("g" . "refresh"))
    (("P" . "project")   ("?" . "help")   ("q" . "quit")))
  "Revisit rail Actions legend: three rows of (KEY . VERB) cells.
Every KEY must resolve to a real command in `org-air-revisit-mode-map'
\(the round-26 legend-truth discipline).")

(defun org-air-revisit--insert-actions (width)
  "Insert the Revisit rail Actions block fitted to content WIDTH.
Same shape/keycap idiom as the board and project Actions blocks."
  (org-air-view--rail-header "Actions" width)
  (let* ((inset (org-air-view--rail-inset-str width))
         (rows org-air-revisit--actions-table)
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

(defun org-air-revisit--rail-descriptor ()
  "Return the Revisit rail descriptor (the R20-5 parameterisation seam)."
  (list :visible-fn #'identity
        :calendar-fn
        (lambda (entries w inset)
          (org-air-calendar-insert-month
           org-air-view--cal-month entries w inset
           (org-air-revisit--calendar-marks entries)))
        :summary-fn #'org-air-revisit--insert-summary
        :first-thing-fn (lambda (_entries) nil)
        :actions-fn #'org-air-revisit--insert-actions))

;;;; ---------------------------------------------------------------------
;;;; Render
;;;; ---------------------------------------------------------------------

(defun org-air-revisit--render-width ()
  "Return the width to render the Revisit view at.
An integer `org-air-view-width' is the batch/golden seam, exactly as the
board reads it; else the live window body; else 80."
  (or (and (integerp org-air-view-width) org-air-view-width)
      (and (get-buffer-window (current-buffer) t)
           (org-air-layout-current-width (current-buffer)))
      80))

(defun org-air-revisit--host-width ()
  "Return the compose width, rail-geometry aware (the R27-2 discipline)."
  (if (and (not noninteractive)
           (not (integerp org-air-view-width))
           (org-air-rail--popped-p)
           (not org-air-view--rail-suspended))
      (org-air-rail--host-width (current-buffer)
                                (org-air-revisit--render-width))
    (org-air-revisit--render-width)))

(defun org-air-revisit--sort-indicator ()
  "Return the shared `↕ key dir' header badge (R22-3 core)."
  (let ((key (or org-air-view--sort-key 'age))
        (dir (or org-air-view--sort-direction 'ascending)))
    (org-air-view--sort-indicator-text
     key dir (not (and (eq key 'age) (eq dir 'ascending))))))

(defun org-air-revisit--header-line (width count)
  "Return the Revisit header for WIDTH: title · mode · COUNT, sort badge."
  (let* ((title (concat
                 (propertize (concat "  org-air" (org-air-view--sep)
                                     "revisit" (org-air-view--sep)
                                     (symbol-name org-air-revisit--surface))
                             'face 'org-air-face-title)
                 (propertize (format "%s%d note%s" (org-air-view--sep)
                                     count (if (= count 1) "" "s"))
                             'face 'org-air-face-faded)))
         (badge (org-air-revisit--sort-indicator))
         (pad (max 1 (- width (string-width title) (string-width badge) 2))))
    (concat title (make-string pad ?\s) badge)))

(defun org-air-revisit--two-pane-body (entries left-fn width)
  "Return the composed rows-pane | rail body lines for ENTRIES (R54-3).
LEFT-FN inserts the row pane at the width it is given; the rail (fed
ENTRIES through the descriptor) is sized to one windowful of the total
WIDTH (the R49-4 rule), inspector-free."
  (let* ((rail-width (org-air-view--rail-width width))
         (divider (org-air-view--divider))
         (item-width (max 20 (- width rail-width (string-width divider))))
         (row-lines (org-air-view--render-lines
                     item-width
                     (lambda () (funcall left-fn item-width))))
         (target-h (max 1 (- (org-air-view--render-height) 3)))
         (rail-lines
          (let ((org-air-view--rail-descriptor
                 (plist-put (copy-sequence org-air-view--rail-descriptor)
                            :rail-target-height target-h))
                (org-air-show-inspector nil))
            (mapcar
             (lambda (l) (org-air-view--pad-to l rail-width))
             (org-air-view--render-lines
              rail-width
              (lambda ()
                (org-air-view--insert-rail entries rail-width)))))))
    (org-air-view--compose-columns
     (list (cons row-lines item-width) (cons rail-lines rail-width))
     divider)))

(defun org-air-revisit--goto-first-row ()
  "Place point on the first note row's title, if any."
  (goto-char (or (text-property-not-all (point-min) (point-max)
                                        'org-air-revisit nil)
                 (point-min)))
  (org-air-view--goto-row-title))

(defun org-air-revisit--render ()
  "Render the Revisit view into the current buffer (R54-3).
DATA-PURE: every cell reads file-meta / ledger slots — zero per-row file
opens; bounded to O(shown) via the page clamp.  Rail placement, popped
side-window lifecycle and the foreign-rail sweep mirror the project view
\(one machinery, parameterised)."
  (when (and (not noninteractive)
             (eq org-air-view--rail-popped-out 'unset))
    (setq-local org-air-view--rail-popped-out
                (eq (org-air-rail--placement 'revisit) 'side-window)))
  (let* ((inhibit-read-only t)
         (org-air-rail--reconciling t)
         (width (org-air-revisit--host-width))
         (org-air-view-width width)
         (dims (org-air-view--char-dimensions))
         (org-air-view--pill-char-w (car dims))
         (org-air-view--pill-char-h (cdr dims))
         (scope (org-air-revisit--scope-entries))
         (visible (org-air-revisit--visible-entries scope))
         (pages org-air-revisit--pages)
         (surface org-air-revisit--surface)
         (created org-air-revisit--show-created)
         (left-fn (lambda (w)
                    (org-air-revisit--insert-rows
                     visible w pages surface created))))
    (setq-local org-air-revisit--count (length visible))
    ;; The rail back-pointer: a popped-out side rail renders THESE entries
    ;; through the descriptor (the R22-5 shared primitive).
    (setq-local org-air-view--items scope)
    (setq-local org-air-view--rail-descriptor
                (org-air-revisit--rail-descriptor))
    (setq-local org-air-view--inspector-region-height nil)
    (erase-buffer)
    (setq org-air-view--orientation
          (cond
           ((org-air-rail--popped-p) 'side-window)
           ((not (org-air-view--board-only-p width)) 'two-pane)
           (t 'board-only)))
    (insert (org-air-revisit--header-line width (length visible)) "\n\n")
    (if (eq org-air-view--orientation 'two-pane)
        (org-air-view--insert-lines
         (org-air-revisit--two-pane-body visible left-fn width))
      (funcall left-fn width))
    (goto-char (point-max))
    (when (and (bolp) (> (point-max) (point-min))) (delete-char -1))
    (goto-char (point-min))
    (org-air-revisit--goto-first-row)
    ;; R58: an armed bookmark locator owns the landing; it stays armed
    ;; while the paced cold fill is still running (the row may not be
    ;; painted yet) and clears on match or fill-idle.
    (org-air-revisit--bookmark-consume)
    (setq org-air-revisit--rendered-width width)
    (cond
     ((eq org-air-view--orientation 'side-window)
      (org-air-rail--show (current-buffer) width))
     ((eq org-air-view--orientation 'board-only)
      ;; R63-1a: the responsive teardown is an OWNER privilege — a
      ;; narrow NON-owner (or suspended) render must never delete
      ;; another view's live rail (the fourth gated tail).
      ;; R58: an undisplayed (bookmark-restored) revisit view must not
      ;; delete the displayed layout's windows.
      (when (and (org-air-rail--tail-owner-p (current-buffer))
                 (not (org-air-rail--undisplayed-host-p (current-buffer))))
        (org-air-rail--hide (current-buffer)))))
    (org-air-rail--evict-foreign-rail (current-buffer))))

(defun org-air-revisit--render-current ()
  "Re-render the current Revisit buffer (the shared dispatch target)."
  (when (derived-mode-p 'org-air-revisit-mode)
    (org-air-revisit--render)))

(defun org-air-revisit--resize-refresh ()
  "Re-render when the displaying window's width changed (the C1 path)."
  (let ((width (org-air-revisit--host-width)))
    (unless (eql width org-air-revisit--rendered-width)
      (org-air-revisit--render-current))))

;;;; ---------------------------------------------------------------------
;;;; Cold fill — the never-hang data path (R53 laws inherited)
;;;; ---------------------------------------------------------------------

(defun org-air-revisit--file-meta-empty-p ()
  "Non-nil when the scan's file-meta table is still empty."
  (zerop (hash-table-count org-air-query--file-meta)))

(defun org-air-revisit--fill-disarm ()
  "Cancel the cold-fill pacer, if armed."
  (when (timerp org-air-revisit--fill-timer)
    (cancel-timer org-air-revisit--fill-timer))
  (setq org-air-revisit--fill-timer nil))

(defun org-air-revisit--fill-start ()
  "Start the paced cold fill of the file-meta table (R54-3).
NEVER a synchronous scan on the interactive path: the same budgeted
slices (`org-air-refresh-slice-budget') on the same repeating wall-clock
pace (`org-air-view--refresh-wallclock-pace') the board's rescue pacer
uses — progress independent of idleness, input latency bounded by one
slice.  Token-guarded; a re-entry supersedes the in-flight fill.  Under
`noninteractive' the callers scan synchronously instead (deterministic
ERT/regen), so no timer is ever armed in batch."
  (cl-incf org-air-revisit--fill-token)
  (org-air-revisit--fill-disarm)
  (org-air-query-skip-log-reset)
  (setq org-air-revisit--fill-queue (org-air-query-files)
        org-air-revisit--fill-total (length org-air-revisit--fill-queue)
        org-air-revisit--fill-last-paint (float-time))
  (if (null org-air-revisit--fill-queue)
      (org-air-revisit--fill-finish)
    (unless noninteractive
      (setq org-air-revisit--fill-timer
            (run-with-timer org-air-view--refresh-wallclock-pace
                            org-air-view--refresh-wallclock-pace
                            #'org-air-revisit--fill-slice
                            (current-buffer)
                            org-air-revisit--fill-token)))))

(defun org-air-revisit--fill-slice (buffer token)
  "Drain one budgeted cold-fill slice for BUFFER under TOKEN (R54-3).
Consumes queued files until `org-air-refresh-slice-budget' is exceeded
\(minimum 1 — the R53 P1c shape); repaints progressively at most once per
`org-air-cold-paint-interval'; a stale TOKEN or dead BUFFER is a silent
no-op, so a superseded fill can never touch the view."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (if (not (eq token org-air-revisit--fill-token))
          nil
        (let ((deadline (+ (float-time) org-air-refresh-slice-budget))
              (first t))
          ;; Budgeted slice, minimum ONE file per tick (the R53 P1c shape).
          (while (and org-air-revisit--fill-queue
                      (or first (< (float-time) deadline)))
            (setq first nil)
            (org-air-query--scan-file (pop org-air-revisit--fill-queue))))
        (if (null org-air-revisit--fill-queue)
            (org-air-revisit--fill-finish)
          (when (>= (- (float-time)
                       (or org-air-revisit--fill-last-paint 0))
                    org-air-cold-paint-interval)
            (setq org-air-revisit--fill-last-paint (float-time))
            (org-air-revisit--render-current)))))))

(defun org-air-revisit--fill-finish ()
  "Complete the cold fill: disarm, resolve the link graph, render once."
  (org-air-revisit--fill-disarm)
  (setq org-air-revisit--fill-queue nil)
  (org-air-query-link-graph-ensure)
  (org-air-revisit--render-current))

(defun org-air-revisit--fill-teardown ()
  "Kill-buffer hook: a dying Revisit buffer takes its pacer with it."
  (cl-incf org-air-revisit--fill-token)
  (org-air-revisit--fill-disarm))

(defun org-air-revisit--ensure-data ()
  "Make sure file-meta has data, without ever blocking the frame (R54-3).
Warm: the table already has entries (a board session scanned, or a prior
fill) — nothing to do.  Cache: hydrate the persisted `:file-meta' +
`:visits' (data-pure, no scan).  Cold: pace the fill (interactive) or
scan synchronously (batch only — deterministic ERT/regen)."
  (when (org-air-revisit--file-meta-empty-p)
    (when-let* ((data (org-air-view--cache-read)))
      (org-air-query-file-meta-hydrate (plist-get data :file-meta))
      (org-air-query-visits-hydrate (plist-get data :visits)))
    (when (org-air-revisit--file-meta-empty-p)
      (if noninteractive
          (progn (org-air-query-items)
                 (org-air-query-link-graph-ensure))
        (org-air-revisit--fill-start)))))

;;;; ---------------------------------------------------------------------
;;;; Commands
;;;; ---------------------------------------------------------------------

(defun org-air-revisit--entry-at-point ()
  "Return the (FILE . META) entry on the current row, or nil."
  (org-air-view--row-property 'org-air-revisit))

(defun org-air-revisit-open ()
  "RET: open the note at point (same window); extend the fold row.
The single user-initiated open of the data-pure view — the (FILE . POS 1)
visit path; records the opt-in visit ledger.  On the `…and N more' fold
row this extends by ONE page instead (the R51-3 actionable contract)."
  (interactive)
  (if (org-air-view--row-property 'org-air-more-row)
      (org-air-revisit-extend)
    (let ((entry (org-air-revisit--entry-at-point)))
      (unless entry
        (user-error "No note at point"))
      (org-air--note-visited (car entry))
      (find-file (car entry))
      (goto-char (point-min)))))

(defun org-air-revisit-visit ()
  "S-RET: open the note at point in the other window."
  (interactive)
  (let ((entry (org-air-revisit--entry-at-point)))
    (unless entry
      (user-error "No note at point"))
    (org-air--note-visited (car entry))
    (find-file-other-window (car entry))
    (goto-char (point-min))))

(defun org-air-revisit-extend ()
  "Reveal one more page of notes (the fold-row verb)."
  (interactive)
  (setq-local org-air-revisit--pages (1+ org-air-revisit--pages))
  (org-air-revisit--render-current))

(defun org-air-revisit-toggle-more ()
  "TAB: on the `…and N more' fold row, reveal one more page."
  (interactive)
  (if (org-air-view--row-property 'org-air-more-row)
      (org-air-revisit-extend)
    (message "org-air revisit: nothing to expand here")))

(defun org-air-revisit-cycle-surface ()
  "Cycle the surfacing mode: ALL → ORPHANS → SPACED (R54-3, key `m').
Each mode is a pure sort/filter over file-meta — same buffer, same
renderer, same paging (reset to page 1), zero per-row file opens."
  (interactive)
  (setq-local org-air-revisit--surface
              (pcase org-air-revisit--surface
                ('all 'orphans)
                ('orphans 'spaced)
                (_ 'all)))
  (setq-local org-air-revisit--pages 1)
  (org-air-revisit--render-current)
  (message "org-air revisit: %s" org-air-revisit--surface))

(defun org-air-revisit-filter (tags)
  "Filter the Revisit view to TAGS (the shared filter core, key `/').
Matches title/tags/origin in memory; resets the paging."
  (interactive
   (list (org-air-view--read-filter
          (delete-dups
           (sort (seq-mapcat (lambda (entry)
                               (copy-sequence
                                (plist-get (cdr entry) :tags)))
                             (org-air-revisit--scope-entries))
                 #'string<)))))
  (setq org-air-view--tag-filter (unless (null tags) tags))
  (setq-local org-air-revisit--pages 1)
  (org-air-revisit--render-current))

(defun org-air-revisit-toggle-created ()
  "Toggle the Created column (denote ID / `#+date').  Key `z c'."
  (interactive)
  (setq-local org-air-revisit--show-created
              (not org-air-revisit--show-created))
  (org-air-revisit--render-current)
  (message "org-air revisit: created column %s"
           (if org-air-revisit--show-created "shown" "hidden")))

(defun org-air-revisit-refresh ()
  "Refresh the Revisit data and re-render (key `g').
NEVER a synchronous scan interactively: the paced cold-fill machinery
rescans on the budgeted wall-clock slices and repaints progressively; in
batch (deterministic ERT/regen) the scan runs inline."
  (interactive)
  (if noninteractive
      (progn (org-air-query-items)
             (org-air-query-link-graph-ensure))
    (org-air-revisit--fill-start))
  (org-air-revisit--render-current))

(defun org-air-revisit-next ()
  "Move point to the next note row, landing on its title."
  (interactive)
  (let ((pos (next-single-property-change (point) 'org-air-revisit
                                          nil (point-max))))
    (while (and pos (not (get-text-property pos 'org-air-revisit))
                (< pos (point-max)))
      (setq pos (next-single-property-change pos 'org-air-revisit
                                             nil (point-max))))
    (when (and pos (get-text-property pos 'org-air-revisit))
      (goto-char pos)
      (org-air-view--goto-row-title))))

(defun org-air-revisit-prev ()
  "Move point to the previous note row, landing on its title."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'org-air-revisit
                                              nil (point-min))))
    (while (and pos
                (not (get-text-property (max (point-min) (1- pos))
                                        'org-air-revisit))
                (> pos (point-min)))
      (setq pos (previous-single-property-change pos 'org-air-revisit
                                                 nil (point-min))))
    (when pos
      (goto-char (max (point-min) (1- pos)))
      (org-air-view--goto-row-title))))

(defun org-air-revisit-quit ()
  "Quit the Revisit view progressively — one surface per press (R28-2).
A live bottom pane closes first; the next press tears down a popped-out
rail and quits back to the previous view (the shared quit convention)."
  (interactive)
  (unless (org-air-view--quit-close-pane)
    (when (org-air-rail--popped-p)
      (org-air-rail--teardown))
    (quit-window)))

;;;; ---------------------------------------------------------------------
;;;; Help groups (consumed by `org-air-help' via its revisit context)
;;;; ---------------------------------------------------------------------

(defconst org-air-revisit--help-groups
  '(("Navigation"
     (org-air-revisit-next . "next note")
     (org-air-revisit-prev . "previous note")
     (org-air-revisit-open . "open note, same window")
     (org-air-revisit-visit . "open note, other window")
     (org-air-revisit-toggle-more . "reveal one more page"))
    ("Surface"
     (org-air-revisit-cycle-surface . "cycle all/orphans/spaced")
     (org-air-view-sort-cycle . "cycle sort key (age/created/title)")
     (org-air-view-sort-reverse . "reverse sort")
     (org-air-revisit-toggle-created . "toggle created column"))
    ("Filter"
     (org-air-revisit-filter . "filter by tags/text (live)")
     (org-air-filter-clear . "clear filter")
     (org-air-filter-toggle-match . "toggle AND/OR combinator"))
    ("Rail"
     (org-air-rail-toggle . "pop rail out/in"))
    ("Refresh"
     (org-air-revisit-refresh . "refresh"))
    ("Session"
     (org-air-project . "project tree")
     (org-air-revisit-quit . "quit")
     (org-air-help . "this help")))
  "REVISIT help groups: (TITLE . ((COMMAND . DESCRIPTION) …)) (R50-2).")

;;;; ---------------------------------------------------------------------
;;;; Keymaps + mode
;;;; ---------------------------------------------------------------------

(defvar org-air-revisit-columns-map
  (make-sparse-keymap)
  "Column-toggle prefix map for the Revisit view (`z c' created).
Keys installed by `org-air--install-default-keybindings' (R35-1).")

(org-air--register-default-keys 'org-air-revisit-columns-map
  "c" #'org-air-revisit-toggle-created)

(defvar org-air-revisit-mode-map
  (let ((map (make-sparse-keymap)))
    ;; A THIN child of the shared view-core map (R18 D-P3): o/O sort, `|'
    ;; rail, `\' clear, M-/ combinator, j/k line motion all inherit.
    ;; PARENT stays at defvar time — always, even with the knob nil (R35-1).
    (set-keymap-parent map org-air-view-core-map)
    map)
  "Keymap for `org-air-revisit-mode'.
Keys installed by `org-air--install-default-keybindings' (R35-1).")

;; R35-1: the REVISIT default keys (installer-owned).  RET is the
;; same-window note open (the fold row extends a page instead); S-RET the
;; other-window visit; `m' cycles the surfacing mode; `/' the per-mode
;; filter; `P' the symmetric view switch to the project tree.
(org-air--register-default-keys 'org-air-revisit-mode-map
  "n" #'org-air-revisit-next
  "p" #'org-air-revisit-prev
  "RET" #'org-air-revisit-open
  "<mouse-1>" #'org-air-revisit-open
  "<S-return>" #'org-air-revisit-visit
  "S-RET" #'org-air-revisit-visit
  "m" #'org-air-revisit-cycle-surface
  "TAB" #'org-air-revisit-toggle-more
  "/" #'org-air-revisit-filter
  "g" #'org-air-revisit-refresh
  "P" #'org-air-project
  ;; R61-4: `W' opens the Review (week/period) surface.
  "W" #'org-air-review
  "z" '(:prefix . org-air-revisit-columns-map)
  "?" #'org-air-help
  "q" #'org-air-revisit-quit)

(defvar org-air-revisit-leader-map
  (make-sparse-keymap)
  "Leader prefix map for the Revisit content buffer (R30-2).
Installed at `org-air-leader-key' on `org-air-revisit-mode-map'.
Keys installed by `org-air--install-default-keybindings' (R35-1).")

(org-air--register-default-keys 'org-air-revisit-leader-map
  "|" #'org-air-rail-toggle
  "o" #'org-air-rail-return
  "s" #'org-air-view-sort-cycle
  "/" #'org-air-revisit-filter)

(org-air--register-default-leader 'org-air-revisit-mode-map
                                  'org-air-revisit-leader-map)

(define-derived-mode org-air-revisit-mode special-mode "Org-Air-Revisit"
  "Major mode for the Revisit (evergreen notes) view (R54-3)."
  ;; R35-1: reconcile the shared maps on the first revisit buffer.
  (org-air--sync-default-keybindings)
  (setq-local truncate-lines t)
  (setq-local cursor-type 'box)
  (setq-local line-spacing org-air-line-spacing)
  (org-air-view--install-modeline)
  ;; R58: the Revisit view is bookmarkable — a FULL record: surface, sort,
  ;; created column, plus the note-at-point locator (the revisit unit is
  ;; the FILE).  Restored by `org-air-revisit-bookmark-jump'.
  (setq-local bookmark-make-record-function
              #'org-air-revisit--bookmark-make-record)
  ;; Responsive re-render on resize (the round-9 C1 path).
  (setq-local org-air-layout-refresh-function
              #'org-air-revisit--resize-refresh)
  ;; R22-3: seed the SHARED sort spec so the inherited o/O drive the
  ;; age/created/title cycle (default: age ascending = dustiest first).
  (setq-local org-air-view--sort-keys '(age created title))
  (setq-local org-air-view--sort-refresh #'org-air-revisit--render-current)
  (unless org-air-view--sort-key
    (setq-local org-air-view--sort-key 'age))
  (unless org-air-view--sort-direction
    (setq-local org-air-view--sort-direction 'ascending))
  ;; R22-2b/R29-2: point normalization onto row titles; inert in batch.
  (unless noninteractive
    (add-hook 'pre-command-hook #'org-air-view--pre-command-snapshot nil t)
    (add-hook 'post-command-hook #'org-air-view--normalize-point nil t))
  ;; A dying revisit buffer takes its pacer AND its popped rail with it.
  (add-hook 'kill-buffer-hook #'org-air-revisit--fill-teardown nil t)
  (add-hook 'kill-buffer-hook #'org-air-rail--teardown nil t)
  ;; R24-5: the shared cooperative rail reconciler; inert under batch.
  (unless noninteractive
    (add-hook 'window-configuration-change-hook
              #'org-air-rail--reconcile nil t))
  ;; R27-4: the shared evil integration (motion state + overriding map);
  ;; fboundp-gated soft dep, skipped with the R35-1 knob off.
  (when org-air-use-default-keybindings
    (org-air-view--setup-evil 'org-air-revisit-mode
                              org-air-revisit-mode-map))
  (org-air-layout-install-window-size-hook)
  (buffer-disable-undo))

;;;###autoload
(defun org-air-revisit ()
  "Open the Revisit (evergreen notes) view (R54-3).
Surfaces KNOWLEDGE notes (`org-air-revisit-types') by attention-age,
dustiest first; `m' cycles ALL → ORPHANS → SPACED.  Reached from the
board via the Notes count row (RET) or `N', and from the project via
`N'; `q' returns to the previous view."
  (interactive)
  (let ((buffer (get-buffer-create org-air-revisit-buffer-name)))
    (with-current-buffer buffer
      ;; R26-5 idempotent entry: initialise the mode only once — a
      ;; re-entry re-renders in place (session state survives).
      (unless (derived-mode-p 'org-air-revisit-mode)
        (org-air-revisit-mode)))
    (pop-to-buffer buffer)
    (org-air-revisit--open-core buffer t)))

(defun org-air-revisit--open-core (buffer _display)
  "Run the Revisit entry's data+render body in BUFFER (R58 factoring).
Prep + `org-air-revisit--ensure-data' (never-blocking: warm / cache
hydrate / paced cold fill) + link-graph ensure + render — exactly the
command's body; the command is prep + `pop-to-buffer' + this core.  The
bookmark handler calls it with DISPLAY nil (undisplayed — the restorer
owns the windows).  Ensures the mode idempotently (R26-5); never
displays BUFFER."
  (with-current-buffer buffer
    (unless (derived-mode-p 'org-air-revisit-mode)
      (org-air-revisit-mode))
    (org-air-revisit--ensure-data)
    (org-air-query-link-graph-ensure)
    (org-air-revisit--render)))

;;;; ---------------------------------------------------------------------
;;;; R58 — Emacs bookmark support (see org-air-view.el's shared core).
;;;; ---------------------------------------------------------------------

(defun org-air-revisit--bookmark-name ()
  "Return the revisit record's `defaults' candidates (R58).
Surface-qualified first (\"org-air: revisit · orphans\") when off the
default `all', then the generic \"org-air: revisit\"."
  (delete-dups
   (delq nil
         (list (and (memq org-air-revisit--surface '(orphans spaced))
                    (format "org-air: revisit · %s"
                            org-air-revisit--surface))
               "org-air: revisit"))))

(defun org-air-revisit--bookmark-make-record ()
  "Return the Emacs bookmark record for the Revisit buffer (R58).
A FULL record: surface + sort + the created-column toggle plus the
note-at-point locator — (FILE . 1), the revisit unit IS the file.  Pure
buffer-local reads; never signals (degrades to the bare header record).
`org-air-revisit--pages' is deliberately NOT recorded: \"show more\" is
a within-session interaction and a restored page depth over a changed
corpus is meaningless (restore resets to 1 — the documented ruling)."
  (condition-case nil
      (append
       (org-air-view--bookmark-header 'revisit
                                      'org-air-revisit-bookmark-jump
                                      "org-air: revisit"
                                      (org-air-revisit--bookmark-name))
       (list (cons 'org-air-surface org-air-revisit--surface)
             (cons 'org-air-sort
                   (cons (or org-air-view--sort-key 'age)
                         (or org-air-view--sort-direction 'ascending)))
             (cons 'org-air-show-created
                   (and org-air-revisit--show-created t)))
       (let ((entry (org-air-view--row-property 'org-air-revisit)))
         (when (and (consp entry) (stringp (car entry)))
           (append
            (list (cons 'org-air-item (cons (car entry) 1)))
            (let ((title (org-air-revisit--entry-title entry)))
              (and (stringp title)
                   (list (cons 'org-air-item-title
                               (substring-no-properties title)))))))))
    (error (org-air-view--bookmark-header 'revisit
                                          'org-air-revisit-bookmark-jump
                                          "org-air: revisit"
                                          (list "org-air: revisit")))))

(defun org-air-revisit--bookmark-apply (record)
  "Apply RECORD's org-air fields to the current Revisit buffer (R58).
The revisit twin of `org-air-view--bookmark-apply': every field
optional, unknown fields ignored, malformed values dropped.  Always
resets `org-air-revisit--pages' to 1 (the documented ruling)."
  (let ((surface (cdr (assq 'org-air-surface record)))
        (sort (cdr (assq 'org-air-sort record)))
        (created (assq 'org-air-show-created record)))
    (when (memq surface '(all orphans spaced))
      (setq-local org-air-revisit--surface surface))
    (when (and (consp sort)
               (car sort) (symbolp (car sort))
               (cdr sort) (symbolp (cdr sort)))
      (setq-local org-air-view--sort-key (car sort)
                  org-air-view--sort-direction (cdr sort)))
    (when created
      (setq-local org-air-revisit--show-created (and (cdr created) t)))
    (setq-local org-air-revisit--pages 1)))

(defun org-air-revisit--bookmark-consume ()
  "Land point on the bookmarked note row; never signals (R58).
Matches on the note FILE (the shared `org-air-marker' property carries
it on revisit rows), then on the entry title.  With the paced cold fill
still in flight a miss stays ARMED for the next progressive paint;
otherwise the slot clears and the render's first-row landing stands."
  (when org-air-revisit--bookmark-locator
    (condition-case nil
        (let* ((slot org-air-revisit--bookmark-locator)
               (file (car-safe (plist-get slot :item)))
               (title (plist-get slot :title))
               (pos (or (and file (org-air-view--find-property
                                   'org-air-marker file))
                        (and title
                             (org-air-view--bookmark-scan
                              'org-air-revisit
                              (lambda (entry)
                                (equal (org-air-revisit--entry-title entry)
                                       title)))))))
          (cond
           (pos
            (setq org-air-revisit--bookmark-locator nil)
            (goto-char pos)
            (org-air-view--goto-row-title))
           ;; Cold fill still running: the row may simply not be painted
           ;; yet — stay armed for the next progressive paint.
           ((or org-air-revisit--fill-queue
                (timerp org-air-revisit--fill-timer)))
           (t (setq org-air-revisit--bookmark-locator nil))))
      (error (setq org-air-revisit--bookmark-locator nil)))))

;;;###autoload
(defun org-air-revisit-bookmark-jump (record)
  "Handler for org-air Revisit bookmarks (R58).
Rebuilds `*org-air revisit*' from RECORD without displaying it (the
bookmark caller owns display) through the existing never-blocking data
path (warm / cache hydrate / paced cold fill — the R53/R54 laws).  Never
signals: a malformed RECORD degrades to a plain Revisit open."
  (require 'org-air)
  (let ((buffer (get-buffer-create org-air-revisit-buffer-name)))
    (condition-case err
        (with-current-buffer buffer
          ;; R26-5 idempotent entry guard — identical to the command's.
          (unless (derived-mode-p 'org-air-revisit-mode)
            (org-air-revisit-mode))
          (org-air-revisit--bookmark-apply record)
          (setq org-air-revisit--bookmark-locator
                (org-air-view--bookmark-locator-of record))
          (org-air-revisit--open-core buffer nil))
      (error
       (message "org-air: bookmark restore degraded: %s"
                (org-air-view--short-error err))
       (with-current-buffer buffer
         (unless (derived-mode-p 'org-air-revisit-mode)
           (org-air-revisit-mode))
         (ignore-errors (org-air-revisit--open-core buffer nil)))))
    ;; The handler contract: make the target buffer CURRENT, never shown.
    (set-buffer buffer)))
;;;###autoload
(put 'org-air-revisit-bookmark-jump 'bookmark-handler-type "org-air")

;; R35-1: this file loads AFTER the load-time seed at the bottom of
;; org-air-project.el, so the revisit key registrations above missed that
;; sync.  Re-install once (idempotent) iff the defaults are currently ON,
;; so the revisit maps are populated under the default while a knob-off
;; setup stays bare.
(when (eq org-air--default-keybindings-state t)
  (org-air--install-default-keybindings))

(provide 'org-air-revisit)

;; Local Variables:
;; package-lint-main-file: "org-air.el"
;; End:
;;; org-air-revisit.el ends here
