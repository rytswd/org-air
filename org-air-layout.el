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

(set (intern "│") "│")

(defcustom org-air-glyphs
  '((origin . ("⌂" . "H"))
    (inbox . ("▤" . "I"))
    (attention . ("▲" . "!"))
    (upcoming . ("◆" . ">"))
    (high-priority . ("★" . "*"))
    (stale . ("◷" . "~"))
    (calendar-item . ("●" . "o"))
    (today . ("▮" . "#"))
    (clear . ("✕" . "x"))
    (more . ("…" . "..."))
    (box-horizontal . ("─" . "-"))
    (box-vertical . ("│" . "|"))
    (vrule . ("│" . "|"))
    (hrule . ("─" . "-"))
    (cal-prev . ("‹" . "<"))
    (cal-next . ("›" . ">"))
    (box-top-left . ("┌" . "+"))
    (box-top-right . ("┐" . "+"))
    (box-bottom-left . ("└" . "+"))
    (box-bottom-right . ("┘" . "+")))
  "Glyph pairs used by org-air as (GUI . TTY) fallbacks."
  :type 'alist
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

(defun org-air-layout-glyph (name)
  "Return glyph NAME with a TTY fallback."
  (let ((pair (cdr (assq name org-air-glyphs))))
    (cond
     ((not pair) "")
     ((display-graphic-p) (car pair))
     (t (cdr pair)))))

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
         (window (or (get-buffer-window buffer t)
                     (and (eq buffer (window-buffer (selected-window)))
                          (selected-window)))))
    (cond
     ((window-live-p window) (window-body-width window))
     ((window-live-p (selected-window)) (window-body-width (selected-window)))
     (t (frame-width)))))

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
