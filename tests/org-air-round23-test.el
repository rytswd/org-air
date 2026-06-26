;;; org-air-round23-test.el --- substantive ERTs for v0.5 round-23 -*- lexical-binding: t; -*-

;;; Commentary:
;; Test-seat SUBSTANTIVE ERTs for v0.5 round-23
;; (air/v0.5/org-air-round23-design.org).  These cover the four R23 fixes
;; where the existing suites were under-covering the new behaviour:
;;
;;   R23-1  REFILE FACE-LEAK [BUG] — `org-get-heading' returns a FONTIFIED
;;          title (`face org-level-1') once the moved item's buffer is live
;;          + fontified after a refile; the row then renders in the raw big/
;;          orange org headline face, breaking the V6 pixel-lock.  The fix
;;          strips text-properties off the title at the data SOURCE
;;          (`--item-at-point' / `--interactive-item') AND defensively in the
;;          shared `org-air-view--insert-row'.  These guards FAIL if the
;;          strip is reverted (anti-tautology: the INPUT carries org-level-1;
;;          the OUTPUT must not).
;;   R23-2  MODE-LINE OFF BY DEFAULT [DEFAULT FLIP] — `org-air-modeline-style'
;;          now ships `default', so org-air leaves the user's own mode-line
;;          untouched in ALL four surfaces (board/project/rail/pane); `calm'
;;          is opt-in; a runtime flip back actively restores the user line.
;;   R23-3  PROJECT TREE CONNECTORS [POLISH] — child dir headers lead with
;;          faded `box' tree connectors (├─ └─ │ in `org-air-face-air-tree';
;;          batch `+- ' / `|  ') threaded down `--insert-dir-node'; top dirs
;;          keep the `▌'/`|' accent marker; ancestor rails thread to depth≥2.
;;   R23-4  LEGIBLE STATE BADGES [POLISH] — `org-air-project-state-style'
;;          (`emoji' default) renders the colour state emoji on a GRAPHICAL
;;          frame (matching `airctl status -Da'); batch/TTY falls back to the
;;          byte-stable `[R]'/`[C]' token; `text'/`badge' honoured; the cell
;;          stays width-locked (no `:height' growth); the R21-4 contract holds.
;;
;; The four manifested re-blesses (modeline default, dir-view golden, the
;; R22-6 nesting metric, the f5 child-dir regex) live in their original
;; suites; these are the NEW behaviour coverage.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)
(require 'org-air-project-test)            ; project fixture root + render

(when (locate-library "org-air")
  (require 'org-air))

;;;; =====================================================================
;;;; R23-1 — refile face-leak: titles carry org-air faces ONLY, no leak.
;;;; =====================================================================

(ert-deftest org-air-r23-1-query-item-title-is-plain-from-fontified-buffer ()
  "R23-1 SOURCE strip: `org-air-query--item-at-point' returns a PLAIN-string
title even from a FONTIFIED Org buffer (where `org-get-heading' carries
`face org-level-1').  Anti-tautology: the RAW heading DOES carry the leaked
face, so the data-layer `substring-no-properties' is what makes the struct
title clean (revert it and this fails)."
  (skip-unless (locate-library "org-air"))
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Ship quarterly report\n")
    (font-lock-mode 1)
    (font-lock-ensure)
    (goto-char (point-min))
    ;; anti-tautology: the raw heading text leaks the org-level-1 face.
    (should (eq (get-text-property 0 'face (org-get-heading t t t t))
                'org-level-1))
    ;; but the struct title the renderer consumes is a plain string.
    (let ((title (org-air-item-title (org-air-query--item-at-point))))
      (should (equal title "Ship quarterly report"))
      (should (null (text-properties-at 0 title))))))

(ert-deftest org-air-r23-1-inbox-interactive-item-title-is-plain ()
  "R23-1 SOURCE strip: `org-air-inbox--interactive-item' (built at point in
a fontified Org buffer) likewise returns a property-free title."
  (skip-unless (locate-library "org-air"))
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Refile me to a real file\n")
    (font-lock-mode 1)
    (font-lock-ensure)
    (goto-char (point-min))
    (let ((title (org-air-item-title (org-air-inbox--interactive-item))))
      (should (equal title "Refile me to a real file"))
      (should (null (text-properties-at 0 title))))))

(ert-deftest org-air-r23-1-insert-row-strips-leaked-title-faces ()
  "R23-1 RENDER GUARD (FAILS if the defensive strip is reverted): the shared
`org-air-view--insert-row' normalises the incoming TITLE to plain text, so a
title carrying a leaked org heading `face' (and any `display') renders
PURELY via org-air's own `font-lock-face' — the row never inherits the
caller's big/coloured `org-level-1' headline face (which would break the V6
pixel-lock), nor a stray `display'."
  (skip-unless (locate-library "org-air"))
  (with-temp-buffer
    (let* ((org-air-view-width 100)
           (org-air-view-height 40)
           (title (propertize "Ship quarterly report"
                              'face 'org-level-1 'display "X")))
      (org-air-view--insert-row
       :prefix "  " :title title
       :date-text "" :tags "" :origin-text ""
       :widths '(0 0 0)
       :face 'org-air-face-title)
      (goto-char (point-min))
      (let* ((bol (line-beginning-position))
             (eol (line-end-position))
             (s (progn (goto-char bol)
                       (re-search-forward "Ship quarterly report" eol t)
                       (match-beginning 0)))
             (e (match-end 0)))
        (should s)
        ;; the title's first glyph carries org-air's R21-2 row-title mark.
        (should (get-text-property s 'org-air-row-title))
        ;; every title glyph: org-air's own face only, NO leaked face/display.
        (cl-loop for p from s below e do
                 (should (eq (get-text-property p 'font-lock-face)
                             'org-air-face-title))
                 (should-not (get-text-property p 'face))
                 (should-not (get-text-property p 'display)))))))

(ert-deftest org-air-r23-1-refile-row-renders-with-org-air-faces-only ()
  "R23-1 END-TO-END (the reported bug): after a real refile the moved item's
dashboard row title carries org-air faces ONLY — NO `org-level-N', no
inherited big/coloured face — and the row keeps its V6 pixel-lock (a clean
single full-width line).  Drives `org-air-refile-item' with explicit args,
PRE-FONTIFIES the target buffer to force the leak condition, then
`org-air-refresh' (the same re-query the refile triggers)."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (org-air-viewport-test--with-frozen-now
      (let ((org-air-view-width 120))
        (org-air)
        (unwind-protect
            (with-current-buffer "*org-air*"
              (let* ((items (org-air-query-items))
                     (item (org-air-test-find-item "Prep client presentation"
                                                   items))
                     (target (expand-file-name
                              "personal.org"
                              (file-name-directory (org-air-item-file item)))))
                (should item)
                (org-air-refile-item item target)
                ;; force the leak: the moved item's file buffer is now live
                ;; + fontified (as it would be under the user's Emacs).
                (with-current-buffer (find-file-noselect target)
                  (font-lock-mode 1)
                  (font-lock-ensure))
                (org-air-refresh)
                (goto-char (point-min))
                (should (re-search-forward "Prep client presentation" nil t))
                (let* ((bol (line-beginning-position))
                       (eol (line-end-position))
                       (s (progn (goto-char bol)
                                 (re-search-forward "Prep client presentation"
                                                    eol t)
                                 (match-beginning 0)))
                       (e (match-end 0))
                       (line (buffer-substring-no-properties bol eol)))
                  ;; the moved row's title carries org-air's title face and
                  ;; NO leaked org heading face anywhere across the title.
                  (cl-loop for p from s below e do
                           (should (eq (get-text-property p 'font-lock-face)
                                       'org-air-face-title))
                           (should-not (get-text-property p 'face)))
                  ;; no org-level-N face leaked anywhere on the whole line.
                  (cl-loop for p from bol below eol do
                           (let ((f (get-text-property p 'face)))
                             (should-not
                              (and (symbolp f) f
                                   (string-prefix-p "org-level-"
                                                    (symbol-name f))))))
                  ;; V6 pixel-lock proxy: a clean single full-width row.
                  (should (= (string-width line) org-air-view-width)))))
          (when (get-buffer "*org-air*") (kill-buffer "*org-air*")))))))

;;;; =====================================================================
;;;; R23-2 — mode-line OFF by default; calm opt-in; runtime restore.
;;;; board + project + rail + pane.
;;;; =====================================================================

(defconst org-air-r23-2--modes
  '(org-air-view-mode org-air-rail-mode
    org-air-entry-view-mode org-air-project-mode)
  "The four org-air surfaces that route through `--install-modeline'.")

(ert-deftest org-air-r23-2-default-style-leaves-user-mode-line ()
  "R23-2: with the (new default) `default' style org-air does NOT
`setq-local' `mode-line-format' in ANY of its four major modes
(board/rail/pane/project) — the user's own normal Emacs mode-line shows
through (no buffer-local override)."
  (skip-unless (locate-library "org-air"))
  (dolist (mode org-air-r23-2--modes)
    (with-temp-buffer
      (let ((org-air-modeline-style 'default))
        (funcall mode)
        (ert-info ((format "mode %s" mode))
          (should-not (local-variable-p 'mode-line-format)))))))

(ert-deftest org-air-r23-2-calm-style-installs-status-line ()
  "R23-2: `calm' stays opt-in — it DOES install the faded nano status line
buffer-locally in all four modes, carrying `org-air-face-modeline'."
  (skip-unless (locate-library "org-air"))
  (dolist (mode org-air-r23-2--modes)
    (with-temp-buffer
      (let ((org-air-modeline-style 'calm))
        (funcall mode)
        (ert-info ((format "mode %s" mode))
          (should (local-variable-p 'mode-line-format))
          (should (equal mode-line-format
                         (list org-air-view--status-mode-line)))
          (should (memq 'org-air-face-modeline
                        (flatten-tree mode-line-format))))))))

(ert-deftest org-air-r23-2-runtime-toggle-back-restores-user-line ()
  "R23-2: toggling `calm' -> `default' at runtime and re-running
`org-air-view--install-modeline' actively DROPS the buffer-local override
(the symmetric `kill-local-variable' path), restoring the inherited user
mode-line — not just a no-op on a fresh buffer."
  (skip-unless (locate-library "org-air"))
  (with-temp-buffer
    (let ((org-air-modeline-style 'calm))
      (org-air-view-mode)
      (should (local-variable-p 'mode-line-format)))
    (let ((org-air-modeline-style 'default))
      (org-air-view--install-modeline)
      (should-not (local-variable-p 'mode-line-format)))))

;;;; =====================================================================
;;;; R23-3 — project tree connectors: faded box guides, top marker, rails.
;;;; =====================================================================

(ert-deftest org-air-r23-3-child-dir-has-connector-top-keeps-marker ()
  "R23-3: a CHILD directory header is led by a faded tree CONNECTOR
(`box-tee-left'/`box-bottom-left' + `box-horizontal', batch `+-') in
`org-air-face-air-tree', while a TOP dir keeps its accent `rail-marker'
(faced `org-air-face-rail-marker') — roots use the marker, children use
connectors, so nesting reads unmistakably."
  (skip-unless (locate-library "org-air"))
  (let* ((docs (org-air-project--collect-docs org-air-project-test-root))
         (tree (org-air-project--directory-tree docs)))
    (org-air-test-with-frozen-project-path org-air-project-test-root
      (org-air-project-test--with-frozen-mtime
        (with-temp-buffer
          (org-air-project--insert-directory-tree tree 80)
          ;; CHILD air-context/ : faded box connector at the guide.
          (goto-char (point-min))
          (should (re-search-forward
                   "^ *\\([-+|]\\)\\([-+|]\\) air-context/" nil t))
          (let ((c1 (match-beginning 1)) (c2 (match-beginning 2)))
            (should (member (char-to-string (char-after c1))
                            (list (org-air-layout-glyph 'box-bottom-left)
                                  (org-air-layout-glyph 'box-tee-left))))
            (should (equal (char-to-string (char-after c2))
                           (org-air-layout-glyph 'box-horizontal)))
            (should (eq (get-text-property c1 'face) 'org-air-face-air-tree))
            (should (eq (get-text-property c2 'face) 'org-air-face-air-tree)))
          ;; TOP v0.1/ : the rail-marker, faced rail-marker (NOT air-tree).
          (goto-char (point-min))
          (should (re-search-forward "^ *\\(.\\) v0\\.1/" nil t))
          (let ((mpos (match-beginning 1)))
            (should (equal (char-to-string (char-after mpos))
                           (org-air-layout-glyph 'rail-marker)))
            (should (eq (get-text-property mpos 'face)
                        'org-air-face-rail-marker))))))))

(ert-deftest org-air-r23-3-ancestor-rails-thread-through-depth-2 ()
  "R23-3: ancestor RAILS thread down the recursion — a depth-2 dir header
carries a `box-vertical' ancestor-rail cell (faded `org-air-face-air-tree')
to the LEFT of its connector when its depth-1 ancestor has a FOLLOWING
sibling, so its connector column is strictly DEEPER than that sibling's; the
LAST child uses `box-bottom-left' (└), a non-last uses `box-tee-left' (├).
Rendered under a GUI stub so the box glyphs are distinguishable (batch
collapses ├/└ to `+')."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (let* ((tree
            (list
             (list :dir "v0.1" :depth 0 :path "v0.1" :own-docs nil
                   :direct-counts nil :desc-counts nil
                   :children
                   (list
                    (list :dir "air-template" :depth 1
                          :path "v0.1/air-template" :own-docs nil
                          :direct-counts nil :desc-counts nil
                          :children
                          (list (list :dir "references" :depth 2
                                      :path "v0.1/air-template/references"
                                      :own-docs nil :direct-counts nil
                                      :desc-counts nil :children nil)))
                    (list :dir "config-management" :depth 1
                          :path "v0.1/config-management" :own-docs nil
                          :direct-counts nil :desc-counts nil
                          :children nil)))))
           (vrail  (org-air-layout-glyph 'box-vertical))
           (hbar   (org-air-layout-glyph 'box-horizontal))
           (tee    (org-air-layout-glyph 'box-tee-left))
           (corner (org-air-layout-glyph 'box-bottom-left)))
      ;; precondition: under the GUI stub the box glyphs are distinct (else a
      ;; TTY fallback would make ├/└/│ indistinguishable and the test moot).
      (should-not (equal tee corner))
      (should-not (equal tee vrail))
      (with-temp-buffer
        (org-air-project--insert-directory-tree tree 70)
        (let* ((lines (split-string (buffer-string) "\n"))
               (line-of (lambda (rx)
                          (cl-find-if (lambda (l) (string-match-p rx l)) lines)))
               (tmpl   (funcall line-of "air-template/"))
               (refs   (funcall line-of "references/"))
               (config (funcall line-of "config-management/")))
          (should tmpl) (should refs) (should config)
          ;; depth-1 air-template/ is a NON-last child -> tee connector.
          (should (string-match-p
                   (concat "^ *" (regexp-quote tee) (regexp-quote hbar)
                           " air-template/")
                   tmpl))
          ;; depth-1 config-management/ is the LAST child -> corner connector.
          (should (string-match-p
                   (concat "^ *" (regexp-quote corner) (regexp-quote hbar)
                           " config-management/")
                   config))
          ;; depth-2 references/ carries a faded ANCESTOR rail (box-vertical)
          ;; to the LEFT of its connector ...
          (let ((rail-pos (string-match (regexp-quote vrail) refs)))
            (should rail-pos)
            (should (eq (get-text-property rail-pos 'face refs)
                        'org-air-face-air-tree)))
          ;; ... and its connector column is strictly deeper than the depth-1
          ;; sibling's (the threaded rail pushes it right).
          (let ((refs-conn (string-match (regexp-quote corner) refs))
                (tmpl-conn (string-match (regexp-quote tee) tmpl)))
            (should refs-conn) (should tmpl-conn)
            (should (> refs-conn tmpl-conn))))))))

;;;; =====================================================================
;;;; R23-4 — legible project state badges (emoji default; token fallback).
;;;; =====================================================================

(ert-deftest org-air-r23-4-default-style-is-svg ()
  "R24-3 RE-REVERSAL (was R23-4 `emoji'): `org-air-project-state-style' now
ships `svg' — a fixed-width, cell-locked filled colour chip that cannot
jitter the R24-2 rails/columns the way the emoji advance width did.  `emoji'
is demoted to an explicit opt-in."
  (skip-unless (locate-library "org-air"))
  (should (eq (default-value 'org-air-project-state-style) 'svg)))

(ert-deftest org-air-r23-4-batch-state-cell-is-token-byte-stable ()
  "R23-4 BYTE GUARD: under --batch (no graphical frame) the `emoji' default
falls through to the terse `[R]'/`[C]'... token — the state cell's TRUE text
(properties stripped) is byte-IDENTICAL to before, no emoji leaks (the
project goldens are unchanged; the R21-4 contract holds), and
`--state-emoji' returns nil off a graphical frame."
  (skip-unless (locate-library "org-air"))
  (should-not (display-graphic-p))               ; batch precondition
  (let ((org-air-project-state-style 'emoji))
    (pcase-dolist (`(,state . ,token)
                   '(("ready" . "[R] ") ("complete" . "[C] ")
                     ("dropped" . "[X] ") ("draft" . "[D] ")
                     ("work-in-progress" . "[W] ") ("review" . "[V] ")))
      (ert-info ((format "state %s" state))
        ;; exact token cell, no emoji code points.
        (should (equal (substring-no-properties
                        (org-air-project--state-cell state))
                       token))
        (should-not (org-air-project--state-emoji state))))))

(ert-deftest org-air-r23-4-emoji-rendered-on-gui-styles-honoured ()
  "R23-4: on a graphical frame (stubbed) the `emoji' style renders STATE's
colour emoji in the badge cell; `text' renders the plain coloured token (no
svg, no emoji); `badge' routes through the shared svg keyword chip while the
TRUE token text stays `[R]'."
  (skip-unless (locate-library "org-air"))
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) t))
            ((symbol-function 'char-displayable-p) (lambda (_c) t)))
    ;; emoji default: --state-emoji returns the icon and the badge shows it.
    (let* ((org-air-project-state-style 'emoji)
           (emoji (org-air-project--state-emoji "ready")))
      (should emoji)
      (should (string-prefix-p "\N{DIRECT HIT}" emoji))
      (should (string-match-p (regexp-quote emoji)
                              (org-air-project--state-badge-cell "ready"))))
    ;; text: plain token only.
    (let ((org-air-project-state-style 'text))
      (let ((cell (org-air-project--state-badge-cell "ready")))
        (should (string-match-p "\\[R\\]" (substring-no-properties cell)))
        (should-not (string-match-p "\N{DIRECT HIT}" cell))))
    ;; badge: shared svg chip, token text preserved.
    (let ((org-air-project-state-style 'badge))
      (should (string-match-p
               "\\[R\\]"
               (substring-no-properties
                (org-air-project--state-badge-cell "ready")))))))

(ert-deftest org-air-r23-4-emoji-preserves-state-cell-pixel-lock ()
  "R23-4: every configured state emoji has `string-width' <=
`org-air-project--state-cell-w' (3), so `--state-cell' always pads it to the
fixed cell — the emoji never overflows the title's left edge (V6 pixel-
lock).  Even with the emoji forced on a GUI, the rendered state cell keeps
the reserved width (cell-w + 1 separator); the emoji is never given a
`:height' face (svg-never-grows-line)."
  (skip-unless (locate-library "org-air"))
  (dolist (pair org-air-project-state-badges)
    (let ((emoji (car (cdr pair))))
      (should (<= (string-width emoji) org-air-project--state-cell-w))))
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) t))
            ((symbol-function 'char-displayable-p) (lambda (_c) t)))
    (let ((org-air-project-state-style 'emoji))
      (dolist (state '("ready" "complete" "dropped" "draft"
                       "work-in-progress" "review"))
        (ert-info ((format "state %s" state))
          (should (= (string-width (org-air-project--state-cell state))
                     (1+ org-air-project--state-cell-w))))))))

(ert-deftest org-air-r23-4-emoji-set-has-vs16-and-matches-airctl ()
  "R23-4: each emoji carries `\\N{VARIATION SELECTOR-16}' (colour
presentation) and the ready/complete/dropped/draft icons EQUAL the ones
`airctl status -Da' prints (🎯/✅/🗑️/📝) — CLI<->GUI parity (guards a future
edit that drops the selector or diverges from the CLI set)."
  (skip-unless (locate-library "org-air"))
  (dolist (pair org-air-project-state-badges)
    (should (string-suffix-p "\N{VARIATION SELECTOR-16}"
                             (car (cdr pair)))))
  (let ((emoji-of (lambda (s)
                    (car (cdr (assoc s org-air-project-state-badges))))))
    (should (string-prefix-p "\N{DIRECT HIT}" (funcall emoji-of "ready")))
    (should (string-prefix-p "\N{WHITE HEAVY CHECK MARK}"
                             (funcall emoji-of "complete")))
    (should (string-prefix-p "\N{WASTEBASKET}" (funcall emoji-of "dropped")))
    (should (string-prefix-p "\N{MEMO}" (funcall emoji-of "draft")))))

(provide 'org-air-round23-test)
;;; org-air-round23-test.el ends here
