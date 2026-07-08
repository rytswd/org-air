;;; org-air-round33-test.el --- R33 acceptance ERTs -*- lexical-binding: t; -*-

;;; Commentary:
;; Round-33 acceptance tests (air/v0.5/org-air-round33-design.org), EXECUTING:
;;
;; R33-1  HEADER STILL OVERFLOWS EVEN EMPTY (Seam B — ambiguous-width chrome
;;        separator).  The header right status is right-filled to the usable
;;        columns by `string-width', but the middle-dot separator was `·'
;;        (U+00B7 MIDDLE DOT), an East-Asian *Ambiguous* glyph a GUI font may
;;        PAINT two columns wide -> the painted header runs to usable+1 and
;;        the last glyph falls off the edge, EVEN at 0 items (the one `·' is
;;        always present).  Fixed by swapping the chrome separator to `∙'
;;        (U+2219 BULLET OPERATOR, East-Asian *Neutral* -> painted one column
;;        everywhere) via `org-air-chrome-separator'.  `string-width' is
;;        identical (both 1) so every COLUMN and the V6/R31 width math are
;;        unchanged; only the painted width can no longer exceed the computed
;;        width.  These ERTs use a HEADLESS ambiguous-width detector (no font
;;        pixels): a glyph is East-Asian Ambiguous iff its `char-width' is 1
;;        by default but 2 under a CJK `char-width-table' (installed via a
;;        language-environment inside an `unwind-protect' that restores it).
;;        "Painted width" is `string-width' measured under that WIDE table.
;;
;; R33-2  PROJECT HOVER "TOO SLOW" — the hover path runs ZERO org-air Lisp
;;        (no `help-echo', no `track-mouse'/<mouse-movement> binding/hook,
;;        `mouse-face' a STATIC per-row text property, inspector/pane follow
;;        on POINT-move `post-command-hook' only) and the mouse-face is a
;;        pure lightweight background highlight (`org-air-face-cursor' carries
;;        NO metric-changing attribute).  The R32 per-row scoping and the
;;        `mouse-1'/RET single-doc open are re-asserted in round32-test.el;
;;        here we lock the remaining hover invariants so a future help-echo /
;;        mouse hook / hover-follow can never reintroduce per-motion work.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-air-viewport-helpers)
(require 'org-air-project-test)
(require 'org-air-round31-test)
(require 'org-air-round32-test)

;;;; =====================================================================
;;;; Shared helpers — the headless East-Asian-Ambiguous width detector.
;;;; =====================================================================

(defmacro org-air-r33--with-cjk-ambiguous-wide (&rest body)
  "Run BODY with the East-Asian *Ambiguous* set forced WIDE, then restore.
Installs a CJK `char-width-table' (via a language-environment that paints
the ambiguous set — `·', `…', arrows, box/geometric glyphs — two columns)
inside an `unwind-protect' that restores BOTH the previous language
environment and the `char-width-table', so the ERT has no global side
effect and needs no font.  Under this table `char-width'/`string-width'
model the worst-case font's painted width."
  (declare (indent 0) (debug t))
  `(let ((org-air-r33--saved-lang current-language-environment)
         (char-width-table (copy-sequence char-width-table)))
     (unwind-protect
         (progn
           ;; Korean installs the CJK ambiguous-wide char-width-table:
           ;; U+00B7 -> 2 while U+2219 (Neutral) stays 1.
           (set-language-environment "Korean")
           ,@body)
       (set-language-environment org-air-r33--saved-lang))))

(defun org-air-r33--ambiguous-p (ch)
  "Non-nil when CH is East-Asian *Ambiguous* width.
Ambiguous iff `char-width' is 1 by default but flips to 2 under a CJK
`char-width-table' (the worst-case-font model)."
  (and (= (char-width ch) 1)
       (= (org-air-r33--with-cjk-ambiguous-wide (char-width ch)) 2)))

(defun org-air-r33--painted-width (s)
  "Worst-case PAINTED display width of string S.
Every East-Asian *Ambiguous* glyph counts 2 columns, every other char its
default `char-width' — i.e. `string-width' evaluated under the CJK
ambiguous-wide table."
  (org-air-r33--with-cjk-ambiguous-wide (string-width s)))

(defun org-air-r33--header-line ()
  "Return the current dashboard's first line (the header band), stripped."
  (save-excursion
    (goto-char (point-min))
    (buffer-substring-no-properties
     (line-beginning-position) (line-end-position))))

(defun org-air-r33--compose-banner (items width)
  "Compose `org-air-view--insert-banner' for ITEMS at render WIDTH.
Returns the header line (properties stripped).  Mirrors R31's Seam-B
probe: the composed line's `string-width' is exactly WIDTH (S7)."
  (with-temp-buffer
    (let ((org-air-view--line-width width))
      (org-air-view--insert-banner items)
      (goto-char (point-min))
      (buffer-substring-no-properties
       (line-beginning-position) (line-end-position)))))

;;;; =====================================================================
;;;; R33-1 — the ambiguous-width chrome separator is eliminated.
;;;; =====================================================================

(ert-deftest org-air-r33-1-ambiguous-detector-sane ()
  "Meta / positive control: the headless detector correctly identifies the
old `·' (U+00B7) as East-Asian Ambiguous and the new `∙' (U+2219) as NOT,
and restores the language environment afterward (no global side effect)."
  (let ((lang current-language-environment))
    ;; the OLD chrome separator IS ambiguous (the Seam-B glyph)...
    (should (org-air-r33--ambiguous-p ?\u00b7))
    ;; ...the NEW one is not, nor are plain ASCII glyphs.
    (should-not (org-air-r33--ambiguous-p ?\u2219))
    (should-not (org-air-r33--ambiguous-p ?\s))
    (should-not (org-air-r33--ambiguous-p ?0))
    (should-not (org-air-r33--ambiguous-p ?a))
    ;; painted width models ambiguous-as-2 vs a clean `string-width'.
    (should (= (org-air-r33--painted-width "a\u00b7b") 4)) ; · paints 2
    (should (= (org-air-r33--painted-width "a\u2219b") 3)) ; ∙ paints 1
    ;; the detector left the language environment exactly as it found it.
    (should (equal current-language-environment lang))))

(ert-deftest org-air-r33-1-separator-width-preserved ()
  "V6/R31 guard: the chrome separator source of truth `∙' has the SAME
`string-width' as the old `·' (both 1) and each of its chars has
`char-width' 1, so every column position and the whole V6/R31 width math
are byte-identical in COLUMNS — the swap is a pure glyph substitution."
  (skip-unless (locate-library "org-air"))
  (should (boundp 'org-air-chrome-separator))
  (should (equal org-air-chrome-separator "\u2219"))
  (should (= (string-width org-air-chrome-separator) 1))
  (should (= (string-width "\u00b7") (string-width org-air-chrome-separator)))
  (mapc (lambda (ch) (should (= (char-width ch) 1)))
        (append org-air-chrome-separator nil))
  ;; the separator is NOT ambiguous (the whole point) while `·' was.
  (should-not (org-air-r33--ambiguous-p (string-to-char org-air-chrome-separator)))
  ;; and the `--sep' helper wraps it with single spaces (width unchanged).
  (should (equal (org-air-view--sep) (concat " " org-air-chrome-separator " "))))

(ert-deftest org-air-r33-1-empty-header-fits-ambiguous-model ()
  "R33-1 acceptance (the reported bug): the EMPTY-board header
\(`org-air-view--insert-banner' nil) composes to exactly the render width W
by `string-width' (S7 column contract) AND its worst-case PAINTED width
\(ambiguous-glyphs-as-2 model) is <= W at several widths, odd and even.
Trunk FAILED — painted 81 > usable 80 at W=80 (the lone `·').  Post-fix
`∙' is Neutral, so painted == string-width == W."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (dolist (w '(80 81 90 100 119 120))
      (let ((line (org-air-r33--compose-banner nil w)))
        (ert-info ((format "empty header W=%d line=%S" w line))
          ;; R39-1 column contract: the header ends banner-indent columns
          ;; before W (symmetric right gutter), so string-width == W - indent.
          (should (= (string-width line) (- w org-air-view--banner-indent)))
          ;; Seam B: the worst-case painted width never exceeds usable.
          (should (<= (org-air-r33--painted-width line) w))
          ;; and equivalently, NO ambiguous glyph survives on the header.
          (should (= (org-air-r33--painted-width line) (string-width line))))))))

(ert-deftest org-air-r33-1-populated-header-fits-ambiguous-model ()
  "The same Seam-B invariant for a POPULATED board header (a count + the
active-sort/scope segments): string-width == W and painted width <= W at
several widths.  Guards that no chrome segment reintroduces an
ambiguous-width glyph on the right-filled header."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 120
      (let ((items org-air-view--items))
        (should items)
        (dolist (w '(80 100 120 160))
          (let ((line (org-air-r33--compose-banner items w)))
            (ert-info ((format "populated header W=%d line=%S" w line))
              ;; R39-1: symmetric right gutter -> string-width == W - indent.
              (should (= (string-width line) (- w org-air-view--banner-indent)))
              (should (<= (org-air-r33--painted-width line) w))
              (should (= (org-air-r33--painted-width line)
                         (string-width line))))))))))

(ert-deftest org-air-r33-1-header-has-no-ambiguous-glyph ()
  "Direct chrome-glyph audit: the live in-buffer header band (the swept
right-filled chrome line, empty AND populated boards) contains NO
East-Asian-Ambiguous glyph — every char's painted width equals its
`char-width'.  Positive control: a `·'-bearing header line paints wider."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    ;; populated board.
    (org-air-viewport-test-with-dashboard 120
      (let ((line (org-air-r33--header-line)))
        (should (string-match-p "org-air" line))
        (dolist (ch (append line nil))
          (should-not (org-air-r33--ambiguous-p ch)))))
    ;; empty board.
    (org-air-viewport-test-with-empty-dashboard 120
      (let ((line (org-air-r33--header-line)))
        (should (string-match-p "0 items" line))
        (dolist (ch (append line nil))
          (should-not (org-air-r33--ambiguous-p ch))))))
  ;; positive control: the pre-fix `·' separator WOULD be caught.
  (should (org-air-r33--ambiguous-p ?\u00b7)))

;;;; =====================================================================
;;;; R33-2 — lightweight, Lisp-free per-row hover invariants.
;;;; =====================================================================

(ert-deftest org-air-r33-2-no-help-echo-on-rows ()
  "No doc row (project OR board) carries a `help-echo' text property — so
no per-motion tooltip function is recomputed as the pointer crosses rows.
\(A `help-echo' function is the classic per-mouse-move Lisp; org-air
installs none.)"
  (skip-unless (locate-library "org-air"))
  ;; project view (the reported surface).
  (org-air-project-test--render
    (call-interactively 'org-air-project-group-by-state)
    (let ((rows (org-air-r32--doc-rows)))
      (should rows)
      (should-not (text-property-not-all (point-min) (point-max) 'help-echo nil))
      (dolist (row rows)
        (should-not (text-property-not-all (car row) (cdr row) 'help-echo nil)))))
  ;; board (mouse-face rows too — same invariant).
  (org-air-viewport-test-with-dashboard 120
    (should-not (text-property-not-all (point-min) (point-max) 'help-echo nil))))

(ert-deftest org-air-r33-2-no-mouse-motion-machinery ()
  "Opening the project view installs NO mouse-motion machinery: no
<mouse-movement> binding is reachable in the buffer's keymaps,
`track-mouse' is not enabled, and no mouse-movement hook was installed by
org-air — the hover hot path is pure redisplay."
  (skip-unless (locate-library "org-air"))
  (org-air-project-test--render
    ;; org-air's OWN keymap binds NO <mouse-movement> handler (the global
    ;; default is a harmless `ignore', which org-air never overrides).
    (should-not (lookup-key (current-local-map) [mouse-movement]))
    (let ((global (key-binding [mouse-movement])))
      ;; whatever the reachable binding is, it is not an org-air command.
      (should-not (and (symbolp global)
                       (string-prefix-p "org-air" (symbol-name global)))))
    ;; track-mouse is off (org-air never turns it on).
    (should-not track-mouse)
    ;; org-air installs no mouse-movement hooks.
    (should-not (bound-and-true-p mouse-movement-hook))))

(ert-deftest org-air-r33-2-cursor-face-lightweight ()
  "`org-air-face-cursor' (the `mouse-face') is a PURE lightweight highlight:
it inherits `org-air-face-subtle' (a subtle :background) and carries NO
metric-changing attribute of its own (no :box / :weight / :height /
:underline), so a hover toggle re-blits the row without re-laying-out its
glyphs or re-rasterising the cached svg tag-pills."
  (skip-unless (locate-library "org-air"))
  (should (facep 'org-air-face-cursor))
  ;; inherits the subtle background face (not a bespoke heavy face).
  (should (memq 'org-air-face-subtle
                (let ((inh (face-attribute 'org-air-face-cursor :inherit nil)))
                  (if (listp inh) inh (list inh)))))
  ;; NO metric-changing attribute is set on the face itself.
  (dolist (attr '(:box :weight :height :underline :overline :strike-through))
    (should (eq (face-attribute 'org-air-face-cursor attr nil) 'unspecified))))

(ert-deftest org-air-r33-2-follow-is-point-move-not-hover ()
  "The inspector-follow and pane-follow are registered on the buffer-local
`post-command-hook' (POINT movement / commands), NOT on any mouse hook, and
they are `noninteractive'-guarded — so a HOVER (no command, no point move)
runs neither.  Positive control: a POINT move IS a command, which is what
`post-command-hook' fires on."
  (skip-unless (locate-library "org-air"))
  (org-air-project-test--render
    ;; they are NOT wired to mouse motion (hover never dispatches a command).
    (should-not (bound-and-true-p mouse-movement-hook))
    ;; HOVER simulation (batch, no command context): a bare call to either
    ;; follow handler arms NO timer — the P0 `noninteractive'-guard means
    ;; hovering schedules zero follow work.
    (let ((org-air-view--inspector-timer nil)
          (org-air-view--view-pane-timer nil))
      (org-air-view--inspector-post-command)
      (org-air-view--view-pane-post-command)
      (should-not (timerp org-air-view--inspector-timer))
      (should-not (timerp org-air-view--view-pane-timer)))
    ;; POSITIVE CONTROL: in a real command context (`noninteractive' nil)
    ;; with the inspector active, the inspector follow DOES arm its idle
    ;; timer — i.e. the follow is driven by commands / POINT movement, which
    ;; is precisely what a mouse HOVER is not.  (Cancel the timer we armed.)
    (let ((noninteractive nil)
          (org-air-view--inspector-active t)
          (org-air-view--inspector-timer nil))
      (unwind-protect
          (progn
            (org-air-view--inspector-post-command)
            (should (timerp org-air-view--inspector-timer)))
        (when (timerp org-air-view--inspector-timer)
          (cancel-timer org-air-view--inspector-timer))))))

(provide 'org-air-round33-test)
;;; org-air-round33-test.el ends here
