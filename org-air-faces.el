;;; org-air-faces.el --- Faces and palette for org-air -*- lexical-binding: t; -*-

;; Copyright (C) 2026 org-air contributors

;; Author: org-air contributors
;; Keywords: faces, calendar, outlines
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;;; Commentary:
;;
;; This file defines the complete visual palette for org-air.  It is the
;; single source of truth for colour and is required by every rendering
;; module (`org-air-view', `org-air-calendar', `org-air-inbox').
;;
;; Design language (after N Λ N O / rougier, "On the Design of Text
;; Editors", https://arxiv.org/abs/2008.06030).  A theme is fully defined
;; by ONE default face plus SIX semantic faces:
;;
;;   default   regular information                       (foreground)
;;   faded     less important: comments, dates, hints    (low intensity)
;;   subtle    suggests a physical area on screen        (faint background)
;;   salient   important, same nature: links, todo       (different hue)
;;   strong    structural: titles, section headers       (same hue, bolder)
;;   popout    needs attention: tags, upcoming deadline  (distinct hue)
;;   critical  requires immediate action: overdue        (used scarcely)
;;
;; All faces:
;;   - carry explicit light AND dark colour specs (auto-adapt per frame);
;;   - degrade gracefully on TTY / 16-colour terminals via :inherit;
;;   - load cleanly in batch with NO dependency on nano-theme:
;;        emacs -Q --batch -l org-air-faces.el
;;   - can OPT-IN to inherit a present nano-theme via
;;        (setq org-air-faces-prefer-nano t)        ;; before load, or
;;        M-x org-air-faces-link-nano               ;; at any time.
;;
;; Palette values follow nano-theme: Material Blue-Grey for light,
;; Nord for dark.  Where nano uses a tint that is too light to read as a
;; *foreground* on white (its popout/critical), org-air substitutes a
;; legibility-tuned shade and documents the divergence in
;; air/v0.1/org-air-design.org.

;;; Code:

(defgroup org-air nil
  "A modern, minimalist replacement for org-agenda."
  :group 'org
  :prefix "org-air-")

(defgroup org-air-faces nil
  "Faces and palette for org-air."
  :group 'org-air
  :prefix "org-air-face-")

(defcustom org-air-faces-prefer-nano nil
  "When non-nil, link org-air faces to nano-theme faces if available.
Consulted at load time.  Has no effect when nano-theme is not
installed.  Can also be applied interactively with
`org-air-faces-link-nano'."
  :type 'boolean
  :group 'org-air-faces)

;;;; ---------------------------------------------------------------------
;;;; Palette
;;;;
;;;; Each entry is (LIGHT . DARK).  These mirror nano-theme; tweak here to
;;;; retint the whole UI.  Foreground popout/critical are legibility-tuned
;;;; (see Commentary).
;;;; ---------------------------------------------------------------------

(defconst org-air-palette
  '((foreground . ("#37474F" . "#ECEFF4"))  ; BlueGrey800 / Nord6
    (background . ("#FFFFFF" . "#2E3440"))  ; White       / Nord0
    (highlight  . ("#FAFAFA" . "#3B4252"))  ; near-white  / Nord1
    (subtle     . ("#ECEFF1" . "#434C5E"))  ; BlueGrey50  / Nord2
    (faded      . ("#90A4AE" . "#677691"))  ; BlueGrey300 / muted
    (strong     . ("#263238" . "#FFFFFF"))  ; BlueGrey900 / White
    (salient    . ("#673AB7" . "#81A1C1"))  ; DeepPurple  / Nord9
    (popout     . ("#E0631E" . "#D08770"))  ; DeepOrange  / Nord12
    (critical   . ("#C62828" . "#EBCB8B"))) ; Red800      / Nord13
  "Core org-air palette as an alist of (NAME . (LIGHT . DARK)).")

(defun org-air-palette-color (name &optional mode)
  "Return colour string for palette NAME.
MODE is `light' or `dark'; defaults to the selected frame's
background mode, falling back to light."
  (let* ((cell (cdr (assq name org-air-palette)))
         (mode (or mode (frame-parameter nil 'background-mode) 'light)))
    (if (eq mode 'dark) (cdr cell) (car cell))))

(defun org-air-faces--spec (name &optional bg tty-inherit weight)
  "Build a `defface' SPEC for palette NAME.
When BG is non-nil the colour is applied to :background, otherwise
:foreground.  TTY-INHERIT is the face inherited on low-colour
terminals (default `default').  WEIGHT, when given, is added on
graphic displays."
  (let* ((cell  (cdr (assq name org-air-palette)))
         (light (car cell))
         (dark  (cdr cell))
         (attr  (if bg :background :foreground))
         (wl    (if weight (list :weight weight) nil)))
    (append
     `((((class color) (min-colors 256) (background light))
        (,attr ,light ,@wl))
       (((class color) (min-colors 256) (background dark))
        (,attr ,dark ,@wl)))
     `((t (:inherit ,(or tty-inherit 'default) ,@wl))))))

;;;; ---------------------------------------------------------------------
;;;; The 1 + 6 semantic faces
;;;; ---------------------------------------------------------------------

(custom-declare-face 'org-air-face-default
  '((t :inherit default))
  "Default face for regular org-air information."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-faded
  (org-air-faces--spec 'faded nil 'shadow)
  "Faded face for secondary information: dates, hints, counts."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-subtle
  (org-air-faces--spec 'subtle t 'highlight)
  "Subtle face: a barely-perceptible background marking an area."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-salient
  (org-air-faces--spec 'salient nil 'link)
  "Salient face for important, same-nature info: links, TODO state."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-strong
  (append (org-air-faces--spec 'strong nil 'default)
          nil)
  "Strong face for structural info: titles and section headers."
  :group 'org-air-faces)
;; Strong differs from default by weight as well as intensity.
(set-face-attribute 'org-air-face-strong nil :weight 'bold)

(custom-declare-face 'org-air-face-popout
  (org-air-faces--spec 'popout nil 'warning)
  "Popout face for information needing attention via a distinct hue."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-critical
  (append (org-air-faces--spec 'critical nil 'error)
          nil)
  "Critical face for information requiring immediate action.
Use scarcely (overdue items, errors)."
  :group 'org-air-faces)
(set-face-attribute 'org-air-face-critical nil :weight 'bold)

;;;; ---------------------------------------------------------------------
;;;; Structural / chrome faces
;;;; ---------------------------------------------------------------------

(custom-declare-face 'org-air-face-header
  (append (org-air-faces--spec 'strong nil 'mode-line)
          nil)
  "Face for the dashboard banner / top header line."
  :group 'org-air-faces)
(set-face-attribute 'org-air-face-header nil :weight 'bold :height 1.2)

(custom-declare-face 'org-air-face-section
  '((t :inherit org-air-face-strong :weight bold))
  "Face for a dashboard section heading (e.g. \"Inbox\")."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-section-icon
  '((t :inherit org-air-face-faded))
  "Face for the glyph/marker preceding a section heading."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-count
  `((((class color) (min-colors 256) (background light))
     (:foreground "#FFFFFF" :background ,(org-air-palette-color 'faded 'light)
      :weight bold :height 0.9))
    (((class color) (min-colors 256) (background dark))
     (:foreground "#2E3440" :background ,(org-air-palette-color 'faded 'dark)
      :weight bold :height 0.9))
    (t (:inherit org-air-face-faded :inverse-video t :weight bold)))
  "Face for an inline item-count badge, e.g. the 7 in \"Inbox  7\"."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-count-attention
  '((t :inherit org-air-face-count))
  "Count badge for a section that needs attention.
Recoloured at render time via popout/critical; defined for override."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-separator
  '((t :inherit org-air-face-subtle :height 0.3))
  "Face for thin horizontal rule / separator lines."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-cursor
  '((t :inherit org-air-face-subtle :extend t))
  "Face for the current-line / selected-item highlight."
  :group 'org-air-faces)

;;;; ---------------------------------------------------------------------
;;;; Item-content faces
;;;; ---------------------------------------------------------------------

(custom-declare-face 'org-air-face-title
  '((t :inherit org-air-face-default))
  "Face for an item's title text."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-todo
  '((t :inherit org-air-face-salient :weight bold))
  "Face for an active TODO keyword."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-done
  '((t :inherit org-air-face-faded))
  "Face for a DONE keyword and completed items."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-priority
  '((t :inherit org-air-face-popout :weight bold))
  "Face for a priority cookie, e.g. [#A]."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-date
  '((t :inherit org-air-face-faded))
  "Face for a neutral timestamp."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-scheduled
  '((t :inherit org-air-face-salient))
  "Face for a SCHEDULED date in the future."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-deadline
  '((t :inherit org-air-face-popout))
  "Face for an upcoming DEADLINE."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-overdue
  '((t :inherit org-air-face-critical :weight bold))
  "Face for an overdue date (past deadline / missed schedule)."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-group
  '((t :inherit org-air-face-faded))
  "Face for an item's category/group label and origin breadcrumb."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-empty
  '((t :inherit org-air-face-faded :slant italic))
  "Face for empty-state placeholder text."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-loading
  '((t :inherit org-air-face-faded :slant italic))
  "Face for loading / busy placeholder text."
  :group 'org-air-faces)

;;;; ---------------------------------------------------------------------
;;;; Tag faces
;;;;
;;;; A base tag face plus a 6-hue accent palette.  Tags are coloured
;;;; deterministically from their name via `org-air-faces-tag-face' so the
;;;; same tag always gets the same hue across sessions.
;;;; ---------------------------------------------------------------------

(custom-declare-face 'org-air-face-tag
  `((((class color) (min-colors 256) (background light))
     (:foreground ,(org-air-palette-color 'strong 'light)
      :background ,(org-air-palette-color 'subtle 'light)
      :box (:line-width (1 . -1) :color ,(org-air-palette-color 'faded 'light))
      :height 0.85))
    (((class color) (min-colors 256) (background dark))
     (:foreground ,(org-air-palette-color 'strong 'dark)
      :background ,(org-air-palette-color 'subtle 'dark)
      :box (:line-width (1 . -1) :color ,(org-air-palette-color 'faded 'dark))
      :height 0.85))
    (t (:inherit org-air-face-faded)))
  "Base face for a tag chip (used when no accent hue is assigned)."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-tag-active
  '((t :inherit org-air-face-tag :weight bold :box t))
  "Face for a tag chip that is the currently active filter."
  :group 'org-air-faces)

(defconst org-air-tag-accent-palette
  '(("#1E88E5" . "#81A1C1")   ; 1 blue
    ("#43A047" . "#A3BE8C")   ; 2 green
    ("#8E24AA" . "#B48EAD")   ; 3 purple
    ("#FB8C00" . "#D08770")   ; 4 orange
    ("#00897B" . "#88C0D0")   ; 5 teal
    ("#D81B60" . "#BF616A"))  ; 6 pink/red
  "Accent hues for tag chips as (LIGHT . DARK) pairs.")

(defun org-air-faces--define-tag-accents ()
  "Define `org-air-face-tag-accent-N' faces from the accent palette."
  (let ((n 0))
    (dolist (pair org-air-tag-accent-palette)
      (setq n (1+ n))
      (let ((light (car pair)) (dark (cdr pair)))
        (custom-declare-face (intern (format "org-air-face-tag-accent-%d" n))
          `((((class color) (min-colors 256) (background light))
             (:foreground ,light
              :box (:line-width (1 . -1) :color ,light) :height 0.85))
            (((class color) (min-colors 256) (background dark))
             (:foreground ,dark
              :box (:line-width (1 . -1) :color ,dark) :height 0.85))
            (t (:inherit org-air-face-tag)))
          (format "Tag accent face %d." n)
          :group 'org-air-faces)))))

(org-air-faces--define-tag-accents)

(defun org-air-faces-tag-face (tag)
  "Return a deterministic accent face symbol for TAG (a string).
The same TAG always maps to the same hue.  Use this so tag colours
are stable across renders and sessions."
  (let* ((n (length org-air-tag-accent-palette))
         (hash (abs (sxhash-equal tag)))
         (idx (1+ (mod hash n))))
    (intern (format "org-air-face-tag-accent-%d" idx))))

;;;; ---------------------------------------------------------------------
;;;; Calendar faces
;;;; ---------------------------------------------------------------------

(custom-declare-face 'org-air-face-calendar-header
  '((t :inherit org-air-face-strong))
  "Face for the calendar month/year title."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-calendar-day-name
  '((t :inherit org-air-face-faded))
  "Face for calendar weekday-name headers (Mo Tu We ...)."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-calendar-day
  '((t :inherit org-air-face-default))
  "Face for an ordinary calendar day number."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-calendar-weekend
  '((t :inherit org-air-face-faded))
  "Face for calendar weekend days."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-calendar-outday
  '((t :inherit org-air-face-subtle))
  "Face for days outside the current month (padding)."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-calendar-today
  '((t :inherit org-air-face-popout :weight bold))
  "Face for today's date in the calendar."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-calendar-event
  '((t :inherit org-air-face-salient :weight bold))
  "Face for a calendar day that carries one or more items."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-calendar-selected
  '((t :inherit org-air-face-subtle :weight bold :extend nil))
  "Face for the currently selected calendar day."
  :group 'org-air-faces)

;;;; ---------------------------------------------------------------------
;;;; Optional nano-theme linkage
;;;; ---------------------------------------------------------------------

(defconst org-air-faces--nano-map
  '((org-air-face-default  . nano-face-default)
    (org-air-face-faded    . nano-face-faded)
    (org-air-face-subtle   . nano-face-subtle)
    (org-air-face-salient  . nano-face-salient)
    (org-air-face-strong   . nano-face-strong)
    (org-air-face-popout   . nano-face-popout)
    (org-air-face-critical . nano-face-critical))
  "Mapping of org-air base faces to nano-theme faces for opt-in linkage.")

(defun org-air-faces-nano-available-p ()
  "Return non-nil when nano-theme's semantic faces are defined."
  (facep 'nano-face-default))

;;;###autoload
(defun org-air-faces-link-nano ()
  "Relink org-air's base faces to inherit nano-theme faces.
No-op (with a message) when nano-theme is not loaded.  Derived
faces follow automatically because they inherit the base faces."
  (interactive)
  (if (not (org-air-faces-nano-available-p))
      (when (called-interactively-p 'interactive)
        (message "org-air: nano-theme faces not found; keeping built-in palette"))
    (dolist (cell org-air-faces--nano-map)
      (when (facep (cdr cell))
        (set-face-attribute (car cell) nil :inherit (cdr cell))))
    (when (called-interactively-p 'interactive)
      (message "org-air: faces linked to nano-theme"))))

;; Apply opt-in linkage at load time when requested and possible.
(when (and org-air-faces-prefer-nano (org-air-faces-nano-available-p))
  (org-air-faces-link-nano))

(provide 'org-air-faces)
;;; org-air-faces.el ends here
