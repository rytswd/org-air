;;; org-air-faces.el --- Faces and palette for org-air -*- lexical-binding: t; -*-

;; Copyright (C) 2026 org-air contributors

;; Author: org-air contributors
;; Keywords: faces, calendar, outlines
;; Version: 0.1.0
;; URL: https://github.com/rytswd/org-air
;; SPDX-License-Identifier: GPL-3.0-or-later

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
;;        Emacs -Q --batch -l org-air-faces.el
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

(require 'cl-lib)

(defgroup org-air nil
  "A modern, minimalist replacement for `org-agenda'."
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
  '((t :inherit org-air-face-faded))
  "Quiet faded section count (the N in \"Inbox  N\").
A plain faded number — no chip, no inverse-video, no height shrink
(nano-agenda style).  The illegible inverse-video chip was replaced
after the GUI screenshot (S3); the rendered text is unchanged."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-count-attention
  '((t :inherit org-air-face-popout :weight bold))
  "Section count for a non-zero attention bucket (Inbox, Needs attention).
A bold popout number that pulls the eye without a chip — mirrors
`org-air-face-summary-number-attention' in the rail."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-separator
  '((t :inherit org-air-face-faded :weight normal))
  "Face for thin horizontal rules.
Foreground hairline only — no background fill and no height shrink, so a
row of `─' reads as one faint connected line rather than a solid bar
(fixed after the GUI screenshot, S2)."
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

(custom-declare-face 'org-air-face-todo-next
  '((t :inherit org-air-face-popout :weight bold))
  "Face for a hot active keyword (NEXT/STARTED) — modern coloured look."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-todo-wait
  '((((class color) (min-colors 256) (background light))
     (:foreground "#B26A00" :weight bold))
    (((class color) (min-colors 256) (background dark))
     (:foreground "#D8A657" :weight bold))
    (t (:inherit org-air-face-faded :weight bold)))
  "Face for a waiting/blocked keyword (WAIT/HOLD/BLOCKED) — muted amber."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-priority
  '((t :inherit org-air-face-popout :weight bold))
  "Face for a priority cookie, e.g. [#A] (fallback)."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-priority-a
  '((t :inherit org-air-face-critical :weight bold))
  "[#A] cookie: critical-hue bold TEXT (round-6 restraint: no pill/box)."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-priority-b
  '((t :inherit org-air-face-popout :weight bold))
  "[#B] cookie: popout bold text (no box)."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-priority-c
  '((t :inherit org-air-face-faded))
  "[#C] cookie: faded text (no box)."
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
  '((t :inherit org-air-face-faded))
  "Base face for a tag (used when no accent hue is assigned).
Round-6 restraint: quiet faded TEXT — no box, no background fill, no
height shrink.  Hue (not a rectangle) carries the tag; the accent faces
add the colour."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-tag-active
  '((t :inherit org-air-face-tag :weight bold :underline t))
  "Face for a tag that is the currently active filter — bold + underline
(no box), so the active state reads without adding chrome."
  :group 'org-air-faces)

(defconst org-air-tag-accent-palette
  ;; (LIGHT-FG LIGHT-BG DARK-FG DARK-BG).  Round-6 restraint uses only the
  ;; FOREGROUNDS — tags are quiet coloured text, no boxes or background
  ;; fills (the *-BG columns are retained for reference / future opt-in
  ;; tinting but are no longer applied).  The foregrounds are WCAG-legible
  ;; on their dashboard background and never collide with the popout/
  ;; critical hues used for TODO state and overdue dates.
  '(("#1565C0" "#E3F2FD" "#88C0D0" "#2E3440")   ; 1 blue
    ("#2E7D32" "#E8F5E9" "#A3BE8C" "#2E3440")   ; 2 green
    ("#6A1B9A" "#F3E5F5" "#B48EAD" "#2E3440")   ; 3 purple
    ("#E65100" "#FFF3E0" "#D08770" "#2E3440")   ; 4 orange
    ("#00695C" "#E0F2F1" "#8FBCBB" "#2E3440")   ; 5 teal
    ("#AD1457" "#FCE4EC" "#BF616A" "#2E3440"))  ; 6 pink/red
  "Accent palette for tag chips.
Each entry is (LIGHT-FG LIGHT-BG DARK-FG DARK-BG): a readable
foreground over a faint background tint, specified per background
mode for light/dark parity.")

(defun org-air-faces--define-tag-accents ()
  "Define `org-air-face-tag-accent-N' faces from the accent palette."
  (let ((n 0))
    (dolist (spec org-air-tag-accent-palette)
      (setq n (1+ n))
      (cl-destructuring-bind (lfg _lbg dfg _dbg) spec
        (custom-declare-face (intern (format "org-air-face-tag-accent-%d" n))
          ;; Round-6 restraint: quiet coloured TEXT only — the accent hue
          ;; as a foreground, no box / no background / no height shrink.
          `((((class color) (min-colors 256) (background light))
             (:foreground ,lfg))
            (((class color) (min-colors 256) (background dark))
             (:foreground ,dfg))
            (t (:inherit org-air-face-tag)))
          (format "Tag accent face %d (quiet coloured text)." n)
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
  '((((class color) (min-colors 256) (background light))
     (:background "#E0631E" :foreground "#FFFFFF" :weight bold))
    (((class color) (min-colors 256) (background dark))
     (:background "#D08770" :foreground "#2E3440" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Today's calendar cell (R7): a filled background highlight, not a glyph.
So obvious the legend needs no \"today\" entry.  The day number sits on a
popout-hue fill (white/dark fg for contrast); TTY falls back to
inverse-video.  Replaces the old underline+marker treatment."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-calendar-event
  '((t :inherit org-air-face-salient :weight bold))
  "Face for a calendar day that carries one or more items."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-calendar-selected
  '((t :inherit org-air-face-subtle :weight bold :extend nil))
  "Face for the currently selected calendar day."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-calendar-scheduled
  '((((class color) (min-colors 256) (background light))
     (:foreground "#7D6BA5"))
    (((class color) (min-colors 256) (background dark))
     (:foreground "#8891AE"))
    (t (:inherit org-air-face-salient)))
  "Calendar day carrying a scheduled item.
D4 (round-9): a *muted* hue, no bold — distinguishable from deadline but
quiet (the saturated salient+bold read as noise on the small marks)."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-calendar-deadline
  '((((class color) (min-colors 256) (background light))
     (:foreground "#C2724E"))
    (((class color) (min-colors 256) (background dark))
     (:foreground "#C49079"))
    (t (:inherit org-air-face-popout)))
  "Calendar day carrying a deadline (the strongest mark).
D4 (round-9): a *muted* terracotta, no bold — still the warmest of the
three but quieter than the full popout."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-calendar-created
  '((t :inherit org-air-face-faded))
  "Calendar day carrying only a created/activity stamp (T3b: quiet)."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-day-header
  '((t :inherit org-air-face-strong))
  "Face for the single-day focus-view title (R6), e.g. the
\"Tuesday 17 June 2026\" header of `org-air-view-day'."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-calendar-legend
  '((((class color) (min-colors 256) (background light))
     (:foreground "#546E7A"))
    (((class color) (min-colors 256) (background dark))
     (:foreground "#B0BCCE"))
    (t (:inherit default)))
  "Face for the calendar legend TEXT (\"has items\", \"today\").
Quiet but *legible* — `org-air-face-faded' fails WCAG AA on both
backgrounds for this small key line (2.7:1 dark / 2.6:1 light), so the
legend uses a mid-contrast grey that passes AA (5.4:1 light #546E7A /
6.5:1 dark #B0BCCE).  The marker SAMPLES in the legend are propertized
in their real cell faces (`org-air-face-calendar-event' for the
has-items dot, `org-air-face-calendar-today' for the today marker) so
the legend doubles as a key."
  :group 'org-air-faces)

;;;; ---------------------------------------------------------------------
;;;; Layout / pane faces (v0.2 full-viewport composition)
;;;;
;;;; The v0.2 dashboard composes several panes side by side: a flexible
;;;; item list, a persistent calendar, and a context rail (bucket summary,
;;;; filter chips, capture hint).  These faces dress the structure that
;;;; separates and labels those panes.  Restraint still applies: borders
;;;; are faint, numbers carry weight not hue, and everything degrades to
;;;; ASCII on TTY (see `org-air-glyphs' box-drawing additions).
;;;; ---------------------------------------------------------------------

(custom-declare-face 'org-air-face-pane-border
  '((t :inherit org-air-face-faded))
  "Face for pane borders: the vertical rule between panes and the
box/rule glyphs that frame the calendar and rail blocks.
Faint by design — structure should be felt, not shouted."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-frame-border
  '((t :inherit org-air-face-faded))
  "Face for the buffer-box outer frame (T7): the one-character border
drawn in window chrome (margins / header-line / mode-line / line-prefix
/ wrap-prefix).  A quiet 1px rule, like rougier/buffer-box."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-rail-title
  '((t :inherit org-air-face-faded :weight bold))
  "Face for a rail block label (e.g. the word in \"-- Summary --\").
Reads as a quiet section marker inside the context rail."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-summary-number
  '((t :inherit org-air-face-strong :weight bold))
  "Face for a bucket count in the summary block.
Weight, not hue, gives the number presence; an all-zero board stays
calm."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-summary-number-attention
  '((t :inherit org-air-face-popout :weight bold))
  "Face for a non-zero count in an attention bucket (Inbox, Needs
attention).  This is the one place the summary spends hue, to pull
the eye to work that is waiting."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-summary-label
  '((t :inherit org-air-face-faded))
  "Face for the bucket name beside a summary number."
  :group 'org-air-faces)

;;;; ---------------------------------------------------------------------
;;;; Air-docs project view (F5) — state badges + tree
;;;;
;;;; org-air can render an Air-managed doc tree (like `airctl status'),
;;;; grouped by state / directory / tag.  Docs carry a state
;;;; (draft/ready/complete/dropped); these faces colour the state badge,
;;;; and `org-air-face-air-tree' draws the box/branch glyphs.
;;;; ---------------------------------------------------------------------

(custom-declare-face 'org-air-face-air-state-draft
  '((t :inherit org-air-face-faded))
  "Air doc state badge: Draft (📝) — quiet/faded (unstarted)."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-air-state-ready
  '((t :inherit org-air-face-popout :weight bold))
  "Air doc state badge: Ready (🎯) — popout (actionable)."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-air-state-wip
  '((t :inherit org-air-face-salient :weight bold))
  "Air doc state badge: Work In Progress ([W]) — salient (actively worked).
The 5th Air state (between Ready and Complete)."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-air-state-complete
  '((((class color) (min-colors 256) (background light))
     (:foreground "#2E7D32" :weight bold))
    (((class color) (min-colors 256) (background dark))
     (:foreground "#A3BE8C" :weight bold))
    (t (:inherit success)))
  "Air doc state badge: Complete (✅) — a calm green."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-air-state-dropped
  '((t :inherit org-air-face-faded :strike-through t))
  "Air doc state badge: Dropped (🗑) — faded + struck through."
  :group 'org-air-faces)

(custom-declare-face 'org-air-face-air-tree
  '((t :inherit org-air-face-pane-border))
  "Face for the Air project-view box/branch glyphs (┌ │ ├─ └─).
Faint, like the dashboard pane border."
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

;; Local Variables:
;; package-lint-main-file: "org-air.el"
;; End:
;;; org-air-faces.el ends here
