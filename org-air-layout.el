;;; org-air-layout.el --- Viewport layout primitives for org-air -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: org-air contributors
;; Keywords: outlines, calendar
;; Version: 0.1.0
;; URL: https://github.com/rytswd/org-air
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Pure string composition helpers used by the org-air dashboard.  Panes are
;; rendered into strings first, then composed with display-width-aware padding
;; so text properties survive while columns line up in a monospaced buffer.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(declare-function svg-create "svg")
(declare-function svg-rectangle "svg")
(declare-function svg-image "svg")
(declare-function org-air-view--svg-image-cached "org-air-view")
;; Bound by `org-air-view--render' to the displaying window's live char
;; metrics (C2/C3); used here only to size the cosmetic header marker bar.
(defvar org-air-view--pill-char-w)
(defvar org-air-view--pill-char-h)

(set (intern "│") "│")

(defcustom org-air-glyphs
  '((origin . ("▤" . "."))
    (inbox . ("□" . "I"))
    (attention . ("▲" . "!"))
    (upcoming . ("◆" . ">"))
    (high-priority . ("★" . "*"))
    (stale . ("○" . "~"))
    (calendar-item . ("●" . "o"))
    (today . ("■" . "#"))
    (clear . ("✕" . "x"))
    (more . ("…" . "..."))
    (box-horizontal . ("─" . "-"))
    (box-vertical . ("│" . "|"))
    (vrule . ("│" . "|"))
    (hrule . ("─" . "-"))
    (hrule-cap . ("╶" . "-"))
    (cal-prev . ("‹" . "<"))
    (cal-next . ("›" . ">"))
    (box-top-left . ("┌" . "+"))
    (box-top-right . ("┐" . "+"))
    (box-bottom-left . ("└" . "+"))
    (box-bottom-right . ("┘" . "+"))
    (box-tee-left . ("├" . "+"))
    (box-tee-right . ("┤" . "+"))
    (updated . ("↻" . "~"))
    (priority-square . ("■" . "#"))
    (repeat . ("↻" . "~"))
    (arrow . ("→" . "->"))
    (rail-marker . ("▌" . "|"))
    (view-pane . ("▤" . "#"))
    (sep-dot . ("·" . "-"))
    (sort-asc . ("↑" . "^"))
    (sort-desc . ("↓" . "v"))
    (sort-key . ("↕" . "|"))
    (flip . ("⇄" . "<>")))
  "Glyph table used by org-air as (PREFERRED . ASCII) fallbacks.
PREFERRED is the GUI glyph (already the safer S5b default: stale ○, today
■, inbox □); ASCII is a pure-ASCII terminal fallback.  An intermediate
SAFE tier for thin GUI fonts lives in `org-air-layout-safe-glyphs'.
Selection is `org-air-layout-glyph'.  User-overridable."
  :type '(alist :key-type symbol
                :value-type (cons string string))
  :group 'org-air)

(defcustom org-air-layout-safe-glyphs
  '((origin . "·")
    (clear . "×"))
  "Intermediate SAFE glyphs for thin GUI fonts (S5b middle tier).
When a glyph's PREFERRED form is not `char-displayable-p' in a graphical
frame, `org-air-layout-glyph' tries the SAFE form here before the ASCII
fallback.  Names absent here degrade straight from PREFERRED to ASCII,
their PREFERRED already being a widely-covered default."
  :type '(alist :key-type symbol :value-type string)
  :group 'org-air)

(defcustom org-air-layout-gutter 2
  "Default number of spaces between side-by-side org-air panes."
  :type 'integer
  :group 'org-air)

(defcustom org-air-layout-two-pane-breakpoint 100
  "Minimum window body width for org-air two-pane layout."
  :type 'integer
  :group 'org-air)

(defcustom org-air-layout-resize-debounce 0.12
  "Idle seconds to wait before re-rendering org-air after a window resize.
A short idle delay coalesces the rapid-fire size-change events Emacs emits
while a frame or window is being dragged into a single re-render."
  :type 'number
  :group 'org-air)

(defvar-local org-air-layout-refresh-function nil
  "Buffer-local function called when an org-air layout window changes size.")

(defvar org-air-layout--resize-timer nil
  "Pending debounce timer for the org-air window-size re-render.")

(defun org-air-layout--displayable-p (string)
  "Return non-nil when every character of STRING is displayable now."
  (and (stringp string)
       (> (length string) 0)
       (seq-every-p #'char-displayable-p (append string nil))))

(defun org-air-layout-glyph (name)
  "Return the best displayable glyph for NAME (S5b 3-tier selection).
Graphical frame: PREFERRED if `char-displayable-p', else the SAFE tier
\(`org-air-layout-safe-glyphs') if displayable, else the ASCII fallback.
A TTY always gets ASCII."
  (let ((pair (cdr (assq name org-air-glyphs))))
    (cond
     ((null pair) "")
     ((not (display-graphic-p)) (cdr pair))
     ((org-air-layout--displayable-p (car pair)) (car pair))
     (t (let ((safe (cdr (assq name org-air-layout-safe-glyphs))))
          (if (and safe (org-air-layout--displayable-p safe))
              safe
            (cdr pair)))))))

(defun org-air-layout-current-height (&optional buffer)
  "Return the body height in lines of the window displaying BUFFER.
The vertical analogue of `org-air-layout-current-width' (S6): measures
the window actually showing BUFFER so the dashboard can fill its full
height.  Falls back to the selected window, then a sane default."
  (let* ((buffer (or buffer (current-buffer)))
         ;; C1: prefer the selected window when it is the one showing BUFFER
         ;; (the resize/config hook runs inside `with-selected-window' on the
         ;; window being re-rendered), so a split re-renders to ITS height —
         ;; never a stale full-frame value — then any window showing BUFFER.
         (window (or (and (eq buffer (window-buffer (selected-window)))
                          (selected-window))
                     (get-buffer-window buffer t))))
    (cond
     ((window-live-p window) (org-air-layout--usable-rows window))
     ((window-live-p (selected-window))
      (org-air-layout--usable-rows (selected-window)))
     (t 24))))

(defun org-air-layout--usable-rows (window)
  "Return the number of FULL text rows usable in WINDOW (R2).
Derived by flooring the body pixel height by the line pixel height
\(font height plus `line-spacing'), so a fractional final row — which
`window-body-height' rounds into its count but which cannot actually
hold a complete line above the mode-line — never causes a one-row
overflow.  Falls back to `window-body-height' when pixels are
unavailable (e.g. a TTY/batch frame)."
  (let* ((px (ignore-errors (window-body-height window t)))
         (lh (with-selected-window window
               (+ (frame-char-height (window-frame window))
                  (if (numberp line-spacing) line-spacing 0)))))
    (if (and (integerp px) (integerp lh) (> lh 0))
        (max 1 (/ px lh))
      (window-body-height window))))

(defun org-air-layout-current-width (&optional buffer)
  "Return the column width of the window actually displaying BUFFER.
BUFFER defaults to the current buffer.  Searches all visible frames for a
live window showing BUFFER and returns its text-area width in columns, so
composed lines are sized for the window the user is really looking at and
never overflow it.  Falls back to the selected window, then the frame
width, when no displaying window exists yet (e.g. during the first render
before the buffer is popped up).

Note: this deliberately measures in columns (not pixels); an earlier
revision passed PIXELWISE to `window-body-width', which made the renderer
compose against a pixel count and pushed the calendar rail far off the
visible area."
  (let* ((buffer (or buffer (current-buffer)))
         ;; C1: prefer the selected window when it displays BUFFER so a
         ;; split/narrow re-render (the hook runs in `with-selected-window'
         ;; on the affected window) measures the ACTUAL displaying-window
         ;; width and never overflows; otherwise any window showing BUFFER.
         (window (or (and (eq buffer (window-buffer (selected-window)))
                          (selected-window))
                     (get-buffer-window buffer t))))
    (cond
     ((window-live-p window) (org-air-layout--usable-columns window))
     ((window-live-p (selected-window))
      (org-air-layout--usable-columns (selected-window)))
     ;; SEAM A (R31-1): no live window shows BUFFER and no live selected
     ;; window to measure — the LAST raw-column term.  Route it through
     ;; the frame-tier usable primitive so a fringe-less graphic frame
     ;; reserves the continuation-glyph column here too (no composition
     ;; can ever be sized to usable+1).  TTY/batch reserve none, so the
     ;; value is byte-identical there.
     (t (org-air-layout--usable-frame-columns)))))

(defun org-air-layout--usable-frame-columns (&optional frame)
  "Return the columns usable for a full line on FRAME with no window to query.
Frame-tier analogue of `org-air-layout--usable-columns' (R31-1, Seam A):
when neither the buffer nor the selected window is live there is no
window whose `window-max-chars-per-line' we can read, so mirror the
reserve the fringe-less continuation column costs — on a graphical frame
return one less than `frame-width', on a TTY/batch frame the plain
`frame-width' (no reserve, keeping goldens byte-identical)."
  (if (display-graphic-p frame)
      (max 1 (1- (frame-width frame)))
    (frame-width frame)))

(defun org-air-layout--usable-columns (window)
  "Return the columns usable for a full line in WINDOW.
In a real graphical window this is `window-max-chars-per-line', which
reserves the column the continuation/truncation glyph occupies when the
right fringe is absent (common in minimal GUI configs) — so no composed
line (not just the S7-margined header) can clip off the right edge.  In a
terminal or a non-window mock it is the plain `window-body-width' in
columns (keeping the U1 width-derivation tests green)."
  (if (and (windowp window)
           (display-graphic-p (window-frame window))
           (fboundp 'window-max-chars-per-line))
      (window-max-chars-per-line window)
    (window-body-width window)))

(cl-defun org-air-layout-labelled-rule (label width &key suffix
                                              (label-face 'org-air-face-rail-title)
                                              (rule-face 'org-air-face-pane-border)
                                              (suffix-face 'org-air-face-rail-title))
  "Return a D5 labelled-rule string of display WIDTH for the context rail.
Form: ‹cap›‹rule› LABEL ‹fill…› [‹SUFFIX›], where the leading cap glyph
\(`hrule-cap', a rounded stub) echoes the rounded left edge of a D1-D3
pill so the rail rules and the item-pane pills share one rounded
language.  The rule glyphs render in RULE-FACE, LABEL in LABEL-FACE, and
the optional right-anchored SUFFIX (e.g. the calendar `‹ ›' nav) in
SUFFIX-FACE — the suffix is reserved its width first so it never
truncates.  An empty LABEL yields a bare capped rule."
  (let* ((cap (org-air-layout-glyph 'hrule-cap))
         (rule (org-air-layout-glyph 'hrule))
         (rchar (string-to-char rule))
         (suffix (or suffix ""))
         (left (if (string-empty-p label)
                   (propertize (concat cap rule) 'face rule-face)
                 (concat (propertize (concat cap rule) 'face rule-face)
                         " "
                         (propertize label 'face label-face)
                         " ")))
         (suffix-str (if (string-empty-p suffix)
                         ""
                       (concat " " (propertize suffix 'face suffix-face))))
         (fill-n (max 0 (- width (string-width left) (string-width suffix-str))))
         (fill (propertize (make-string fill-n rchar) 'face rule-face)))
    (concat left fill suffix-str)))

(defcustom org-air-rail-header-style 'marker
  "How a rail/calendar section header renders (D-P6).
`marker' (default) draws a clean prefix-marked header (a slim rounded svg
accent bar over a reserved 1-col marker on a GUI, the plain `rail-marker'
glyph in TTY) with a legible `org-air-face-rail-header' label.  `rule'
restores the round-10 hl-block labelled rule."
  :type '(choice (const marker) (const rule))
  :group 'org-air)

(defun org-air-layout-marker-image ()
  "Return an svg image of a slim rounded accent bar one char wide (D-P6), or nil.
GUI only; uses the displaying window's live char metrics when bound (C2/C3),
else the frame char metrics.  The image is locked to one char cell so the
reserved marker column's alignment is unchanged."
  (when (and (display-graphic-p) (require 'svg nil t))
    (ignore-errors
      (let* ((cw (or (bound-and-true-p org-air-view--pill-char-w)
                     (frame-char-width)))
             (ch (or (bound-and-true-p org-air-view--pill-char-h)
                     (frame-char-height)))
             (bw (max 1 (round (* cw 0.45))))
             (color (or (face-foreground 'org-air-face-rail-marker nil t) "gray"))
             ;; R18 D-P1a: the rail/project marker bar repeats per header — a
             ;; pure function of (colour, cw, ch); build it once and share the
             ;; image via the view layer's svg cache when it is loaded.
             (build (lambda ()
                      (let ((svg (svg-create cw ch)))
                        (svg-rectangle svg 0.5 (* ch 0.1) bw (* ch 0.8)
                                       :rx (/ ch 6.0) :ry (/ ch 6.0) :fill color)
                        (svg-image svg :ascent 'center :width cw :height ch)))))
        (if (fboundp 'org-air-view--svg-image-cached)
            (org-air-view--svg-image-cached (list 'marker color cw ch) build)
          (funcall build))))))

(cl-defun org-air-layout-rail-header-string (label width &key suffix
                                                   (suffix-face 'org-air-face-rail-header))
  "Return a D-P6 prefix-marked rail header line as TEXT, padded to WIDTH.
Form: `<marker> LABEL <fill> [SUFFIX]'.  The 1-col prefix marker is the
`rail-marker' glyph (left-half-block, ascii `|') faced `org-air-face-rail-
marker'; on a GUI it also carries a slim rounded svg accent bar via
`display' (locked to one char cell).  LABEL is faced `org-air-face-rail-
header' (legible, not faded); the optional right-anchored SUFFIX (e.g. the
calendar `‹ ›' nav) is faced SUFFIX-FACE.  No bg/overline/rule glyphs
\(reverses round-10 D-P2.A).  The byte/TTY layer is `<marker> LABEL'."
  (let* ((img (org-air-layout-marker-image))
         (mk (org-air-layout-glyph 'rail-marker))
         (marker (propertize mk 'face 'org-air-face-rail-marker))
         (marker (if img (propertize marker 'display img) marker))
         (left (concat marker " "
                       (propertize label 'face 'org-air-face-rail-header)))
         (suffix-str (if (and suffix (not (string-empty-p suffix)))
                         (propertize suffix 'face suffix-face)
                       ""))
         (fill-n (max 1 (- width (string-width left) (string-width suffix-str))))
         (line (concat left (make-string fill-n ?\s) suffix-str))
         (w (string-width line)))
    (cond
     ((> w width)
      (truncate-string-to-width line width nil nil (org-air-layout-glyph 'more)))
     ((< w width) (concat line (make-string (- width w) ?\s)))
     (t line))))

(defun org-air-layout-orientation (width &optional breakpoint)
  "Return layout orientation for WIDTH and BREAKPOINT.
The result is `two-pane' when WIDTH reaches BREAKPOINT, otherwise `stacked'."
  (if (>= width (or breakpoint org-air-layout-two-pane-breakpoint))
      'two-pane
    'stacked))

(defun org-air-layout-render-pane (renderer)
  "Return the string produced by calling RENDERER in a temporary buffer.
RENDERER may be a function of zero arguments or one argument, the temporary
buffer.  Text properties inserted by RENDERER are preserved in the returned
string."
  (with-temp-buffer
    (let* ((arity (func-arity renderer))
           (min (car arity))
           (max (cdr arity)))
      (if (and (<= min 1) (or (eq max 'many) (>= max 1)))
          (funcall renderer (current-buffer))
        (funcall renderer)))
    (buffer-string)))

(defun org-air-layout--pane-lines (pane)
  "Return PANE lines as a list of strings."
  (let ((content (or (plist-get pane :lines)
                     (plist-get pane :content)
                     "")))
    (cond
     ((listp content) content)
     ((stringp content)
      (let ((lines (split-string content "\n")))
        (if (and lines (equal (car (last lines)) ""))
            (butlast lines)
          lines)))
     (t (list (format "%s" content))))))

(defun org-air-layout--display-pad (string width)
  "Return STRING truncated and padded to display WIDTH.
Padding is literal spaces appended after STRING; existing text properties in
STRING are retained."
  (let* ((trimmed (truncate-string-to-width (or string "") width nil nil ""))
         (missing (- width (string-width trimmed))))
    (if (> missing 0)
        (concat trimmed (make-string missing ?\s))
      trimmed)))

(defun org-air-layout--normalize-lines (lines width)
  "Return LINES fitted to display WIDTH."
  (mapcar (lambda (line) (org-air-layout--display-pad line width)) lines))

(defun org-air-layout-box-lines (lines width)
  "Return LINES surrounded by a box of display WIDTH.
Box glyphs use `org-air-glyphs' GUI/TTY fallback pairs."
  (let* ((inner-width (max 0 (- width 2)))
         (h (org-air-layout-glyph 'box-horizontal))
         (v (org-air-layout-glyph 'box-vertical))
         (tl (org-air-layout-glyph 'box-top-left))
         (tr (org-air-layout-glyph 'box-top-right))
         (bl (org-air-layout-glyph 'box-bottom-left))
         (br (org-air-layout-glyph 'box-bottom-right))
         (rule (make-string inner-width (string-to-char h))))
    (append
     (list (concat tl rule tr))
     (mapcar (lambda (line)
               (concat v (org-air-layout--display-pad line inner-width) v))
             lines)
     (list (concat bl rule br)))))

(defun org-air-layout--prepare-pane (pane width)
  "Return PANE lines padded for column WIDTH."
  (let* ((padding (or (plist-get pane :padding) 0))
         (inner-width (max 0 (- width (* 2 padding))))
         (prefix (make-string padding ?\s))
         (lines (org-air-layout--pane-lines pane))
         (lines (mapcar (lambda (line)
                          (concat prefix
                                  (org-air-layout--display-pad line inner-width)
                                  prefix))
                        lines)))
    (if (plist-get pane :box)
        (org-air-layout-box-lines lines width)
      (org-air-layout--normalize-lines lines width))))

(defun org-air-layout--pane-widths (panes width gutter)
  "Return display widths for PANES fitting total WIDTH with GUTTER."
  (let* ((count (length panes))
         (available (max 1 (- width (* (max 0 (1- count)) gutter))))
         (fixed-total 0)
         (flex-count 0))
    (dolist (pane panes)
      (if-let* ((pane-width (plist-get pane :width)))
          (setq fixed-total (+ fixed-total pane-width))
        (setq flex-count (1+ flex-count))))
    (let* ((remaining (max 1 (- available fixed-total)))
           (flex-width (if (> flex-count 0) (/ remaining flex-count) 0))
           (extra (if (> flex-count 0) (% remaining flex-count) 0))
           widths)
      (dolist (pane panes (nreverse widths))
        (push (or (plist-get pane :width)
                  (prog1 (+ flex-width (if (> extra 0) 1 0))
                    (when (> extra 0) (setq extra (1- extra)))))
              widths)))))

;;;###autoload
(defun org-air-layout-compose (panes width &optional stacked)
  "Compose PANES into display-width-normalized lines for WIDTH.
Each pane is a plist accepting :content or :lines, :width, :padding, and :box.
When STACKED is non-nil, panes are arranged top-to-bottom; otherwise they are
zipped side-by-side with `org-air-layout-gutter' spaces between columns."
  (let ((panes (seq-filter #'identity panes)))
    (if (or stacked (<= (length panes) 1))
        (mapcan (lambda (pane)
                  (org-air-layout--prepare-pane pane width))
                panes)
      (let* ((gutter (or (plist-get (car panes) :gutter) org-air-layout-gutter))
             (widths (org-air-layout--pane-widths panes width gutter))
             (columns (cl-mapcar #'org-air-layout--prepare-pane panes widths))
             (height (apply #'max 0 (mapcar #'length columns)))
             (gutter-string (make-string gutter ?\s))
             lines)
        (cl-loop for row-number below height
                 do (let ((segments nil))
                      (cl-loop for column in columns
                               for column-width in widths
                               do (push (or (nth row-number column)
                                            (make-string column-width ?\s))
                                        segments))
                      (push (string-join (nreverse segments) gutter-string) lines)))
        (nreverse lines)))))

(defun org-air-layout-compose-responsive (panes width)
  "Compose PANES for WIDTH using the configured breakpoint API."
  (org-air-layout-compose panes width
                          (eq (org-air-layout-orientation width) 'stacked)))

(defun org-air-layout--refresh-windows ()
  "Re-render every visible org-air window through its refresh function.
Runs in the displaying window's context so width derivation is correct."
  (setq org-air-layout--resize-timer nil)
  (dolist (window (window-list-1 nil 'no-minibuf t))
    (let ((buffer (window-buffer window)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (functionp org-air-layout-refresh-function)
            (with-selected-window window
              (funcall org-air-layout-refresh-function))))))))

(defun org-air-layout--window-size-change (&optional _frame)
  "Debounced refresh of visible org-air buffers after a layout change.
Accepts the FRAME argument from `window-size-change-functions' and no
argument from `window-configuration-change-hook'."
  (when (timerp org-air-layout--resize-timer)
    (cancel-timer org-air-layout--resize-timer))
  (setq org-air-layout--resize-timer
        (run-with-idle-timer org-air-layout-resize-debounce nil
                             #'org-air-layout--refresh-windows)))

;;;###autoload
(defun org-air-layout-install-window-size-hook ()
  "Install the org-air window-size and -configuration refresh hooks."
  (add-hook 'window-size-change-functions #'org-air-layout--window-size-change)
  (add-hook 'window-configuration-change-hook
            #'org-air-layout--window-size-change))

(provide 'org-air-layout)

;; Local Variables:
;; package-lint-main-file: "org-air.el"
;; End:
;;; org-air-layout.el ends here
