;;; org-air-round50-test.el --- executing ERTs for v0.5 round-50 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.5 round-50 (air/v0.5/org-air-round50-design.org):
;; legend key truth + the proper `*org-air-help*' buffer.
;;
;;   R50-1  LEGEND KEY TRUTH — the board Actions legend derives every
;;          cell's key from the LIVE binding in the board buffer via
;;          `org-air-view--legend-key' (where-is), so the refresh cell
;;          shows the TRUE sequence `g r' (`g' alone is the B4 prefix
;;          map), no legend key is ever a bare prefix, and a rebinding
;;          is followed automatically.  The failed-marker string names
;;          `(g r retries)' (pinned by the retuned r26-8 ERT).
;;   R50-2  `?' HELP → `*org-air-help*' BUFFER — a real, formatted,
;;          scrollable buffer (org-air-help-mode, special-mode child),
;;          context-aware (board / project / doc-session), rows derived
;;          from the origin buffer's ACTUAL keymaps, `q' quits back,
;;          knob-gated keys with `M-x' still working.
;;
;; Revert-FAILS discipline (verified against the pre-impl trunk):
;;   r50-1-board-legend-shows-g-r        — trunk hardcodes `g refresh';
;;   r50-1-legend-keys-are-commands-not-prefixes — trunk fails at exactly
;;          the `g' cell (bare prefix map displayed as a key);
;;   r50-1-legend-follows-rebinding      — a hardcoded string cannot
;;          follow a `define-key' move;
;;   r50-2-help-opens-buffer-from-board  — trunk `message's one echo
;;          line, no buffer, no window;
;;   r50-2-help-context-aware            — trunk has no project/doc help
;;          groups, no leader-derived rows;
;;   r50-2-help-q-quits-back             — no buffer to quit on trunk;
;;   r50-2-help-knob-gated               — trunk's echo line fails the
;;          buffer conjunct.
;;
;; Harness discipline: live-window items run through the R26/R27 pattern
;; (`noninteractive' nil, keys dispatched via `(key-binding (kbd …))');
;; batch renders ride the frozen-clock viewport harness.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)        ; frozen clock + dashboard harness
(require 'org-air-round28-test)            ; live board/project + doc-session
(require 'org-air-round35-test)            ; the keybindings knob macro

(when (locate-library "org-air")
  (require 'org-air))

;;;; =====================================================================
;;;; Shared helpers.
;;;; =====================================================================

(defun org-air-r50--kill-help ()
  "Kill a lingering `*org-air-help*' buffer (test hygiene)."
  (when (get-buffer org-air-help-buffer-name)
    (kill-buffer org-air-help-buffer-name)))

(defun org-air-r50--help-text ()
  "Return the current `*org-air-help*' buffer text, sans properties."
  (let ((help (get-buffer org-air-help-buffer-name)))
    (should help)
    (with-current-buffer help
      (substring-no-properties (buffer-string)))))

(defun org-air-r50--legend-cells ()
  "Parse the rendered board rail Actions verb cells from this buffer.
Returns the six \"KEY DESC\" cell strings: the two verb rows follow the
`Actions' rail header; the rail part of each line sits right of the LAST
divider column (`│' GUI / `|' batch), cells are separated by 2+ spaces
(key and desc inside a cell by exactly one)."
  (let (cells)
    (save-excursion
      (goto-char (point-min))
      (should (search-forward "Actions" nil t))
      (forward-line 1)
      (dotimes (_ 2)
        (let* ((line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))
               (rail (if (string-match ".*[│|]" line)
                         (substring line (match-end 0))
                       line)))
          (dolist (cell (split-string rail "  +" t "[ \t]+"))
            (push cell cells)))
        (forward-line 1)))
    (nreverse cells)))

;;;; =====================================================================
;;;; R50-1 — legend key truth.
;;;; =====================================================================

(ert-deftest org-air-r50-1-board-legend-shows-g-r ()
  "The board rail Actions legend shows the TRUE refresh sequence
`g r refresh' — never the D5f bare prefix `g refresh' (pressed alone,
`g' only waits for a second key) nor the round-8 squeezed `gr refresh'.
Reverting the derived-cell fix (back to the hardcoded string) FAILS."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 120
    (let ((text (buffer-string)))
      (should (string-match-p "g r refresh" text))
      (should-not (string-match-p "\\_<g refresh" text))
      (should-not (string-match-p "gr refresh" text))
      ;; the direct cells stay direct (no mislabel introduced by the fix).
      (should (string-match-p "c capture" text))
      (should (string-match-p "\\? help" text)))))

(ert-deftest org-air-r50-1-legend-keys-are-commands-not-prefixes ()
  "Every key the BOARD legend displays resolves via `key-binding' in the
board buffer to a `commandp' object and NOT a keymap — the guard that
keeps the `g refresh' mislabel class from ever returning (the project
side carries the same conjunct in the retuned r26-3 legend-truth ERT).
Trunk FAILS at exactly the `g' cell: `g' alone is the B4 prefix map."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 160
    (let ((cells (org-air-r50--legend-cells)))
      ;; anti-tautology: all six verb cells parsed.
      (should (= (length cells) 6))
      (dolist (cell cells)
        (ert-info ((format "legend cell %S" cell))
          (should (string-match "\\`\\(.*[^ ]\\) +\\([^ ]+\\)\\'" cell))
          (let* ((key (match-string 1 cell))
                 (cmd (key-binding (kbd key))))
            ;; the displayed key really fires a command here…
            (should cmd)
            (should (commandp cmd))
            ;; …and is never a bare prefix map.
            (should-not (keymapp cmd))
            (should-not (equal key "g")))))
      ;; the refresh cell is the true sequence.
      (should (member "g r refresh" cells))
      ;; the mislabel class is real and would be caught: bare `g' IS a
      ;; prefix map in this very buffer.
      (should (keymapp (key-binding (kbd "g")))))))

(ert-deftest org-air-r50-1-legend-follows-rebinding ()
  "With `org-air-refresh' rebound (F5 added, `g r' removed, scoped map
edit), a re-render shows the NEW key text in the refresh cell — the
legend derives from the live binding, it cannot go stale.  Trunk FAILS
(a hardcoded string cannot follow)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 120
    (unwind-protect
        (progn
          (define-key org-air-g-prefix-map (kbd "r") nil t)
          (define-key org-air-view-mode-map (kbd "<f5>") #'org-air-refresh)
          (org-air-view--render org-air-view--items nil)
          (let ((text (buffer-string)))
            (should (string-match-p "<f5> refresh" text))
            (should-not (string-match-p "g r refresh" text))))
      ;; restore: drop the scratch F5 key and force-resync the shared
      ;; installer-owned maps (`g r' comes back from the registry).
      (define-key org-air-view-mode-map (kbd "<f5>") nil t)
      (setq org-air--default-keybindings-state 'unset)
      (org-air--sync-default-keybindings))))

;;;; =====================================================================
;;;; R50-2 — `?' help is a proper buffer.
;;;; =====================================================================

(ert-deftest org-air-r50-2-help-opens-buffer-from-board ()
  "Dispatching `?' in a live board runs `org-air-help', which pops a
LIVE `*org-air-help*' buffer displayed in a window (NOT an echo-area
line): grouped rows derived from the board's actual keymaps — the
refresh row reads the true `g r' sequence and the refile verb is
documented (the relocated R26-6 discovery guarantee).  Trunk FAILS
(echo only, no buffer, no window)."
  (skip-unless (locate-library "org-air"))
  (org-air-r28--with-live-board
    (unwind-protect
        (let ((cmd (key-binding (kbd "?"))))
          (should (eq cmd 'org-air-help))
          (call-interactively cmd)
          ;; a REAL buffer, displayed — not echo-area-only.
          (let ((help (get-buffer org-air-help-buffer-name)))
            (should (buffer-live-p help))
            (should (get-buffer-window help))
            (with-current-buffer help
              (should (derived-mode-p 'org-air-help-mode))
              (should (derived-mode-p 'special-mode))
              (should buffer-read-only))
            (let ((text (org-air-r50--help-text)))
              (should (string-match-p "org-air help — board" text))
              (should (string-match-p "^  g r +refresh" text))
              (should (string-match-p "^  r +refile" text)))))
      (org-air-r50--kill-help))))

(ert-deftest org-air-r50-2-help-context-aware ()
  "Help picks its group set from the ORIGIN buffer: a live PROJECT lists
the project verbs (open / flip / group; refresh at the bare direct `g',
never `g r'); a DOC SESSION lists the `C-c C-q' back verb plus the
`C-c C-a' leader forms, and a customized `org-air-leader-key' shows the
NEW leader prefix (rows derive from the live maps).  Trunk FAILS."
  (skip-unless (locate-library "org-air"))
  ;; -- project context ---------------------------------------------------
  (org-air-r28--with-live-project
    (unwind-protect
        (progn
          (should (eq (key-binding (kbd "?")) 'org-air-help))
          (org-air-help)
          (let ((text (org-air-r50--help-text)))
            (should (string-match-p "org-air help — project" text))
            ;; the project's refresh really is the DIRECT bare `g'.
            (should (string-match-p "^  g +refresh" text))
            (should-not (string-match-p "^  g r" text))
            (should (string-match-p "^  RET +open doc" text))
            (should (string-match-p "flip filename" text))
            (should (string-match-p "group by state" text))))
      (org-air-r50--kill-help)))
  ;; -- doc-session context (+ leader move) -------------------------------
  (org-air-r28--with-doc-session
    (let ((saved-leader org-air-leader-key))
      (unwind-protect
          (progn
            (should (buffer-local-value 'org-air-doc-session-mode docbuf))
            (with-current-buffer docbuf
              ;; help is reachable as the LEADER form; bare `?' keeps
              ;; self-inserting in the editable doc buffer (R20-3a).
              (should (eq (key-binding (kbd "C-c C-a ?")) 'org-air-help))
              (should-not (eq (key-binding "?") 'org-air-help))
              (org-air-help))
            (let ((text (org-air-r50--help-text)))
              (should (string-match-p "org-air help — doc session" text))
              (should (string-match-p "C-c C-q" text))       ; back verb
              (should (string-match-p "C-c C-a n" text))     ; leader motion
              (should (string-match-p "C-c C-a |" text)))    ; leader rail
            ;; leader moved -> the doc help shows the NEW prefix.
            (customize-set-variable 'org-air-leader-key "C-c C-z")
            (with-current-buffer docbuf (org-air-help))
            (let ((text (org-air-r50--help-text)))
              (should (string-match-p "C-c C-z n" text))
              (should-not (string-match-p "C-c C-a n" text))))
        (customize-set-variable 'org-air-leader-key saved-leader)
        (org-air-r50--kill-help)))))

(ert-deftest org-air-r50-2-help-q-quits-back ()
  "`q' in `*org-air-help*' runs `quit-window' (the `special-mode' PARENT
binding, so it survives the keybindings knob) and restores the origin
buffer's window.  Trunk FAILS (no buffer to quit)."
  (skip-unless (locate-library "org-air"))
  (org-air-r28--with-live-board
    (unwind-protect
        (let ((bbuf (current-buffer)))
          (org-air-r28--press "?")
          ;; `pop-to-buffer' selected the help window.
          (should (eq (window-buffer (selected-window))
                      (get-buffer org-air-help-buffer-name)))
          (with-current-buffer org-air-help-buffer-name
            (should (eq (key-binding (kbd "q")) 'quit-window)))
          (org-air-r28--press "q")
          ;; help no longer displayed; the board window is live again.
          (should-not (get-buffer-window org-air-help-buffer-name))
          (should (eq (window-buffer (selected-window)) bbuf)))
      (org-air-r50--kill-help))))

(ert-deftest org-air-r50-2-help-knob-gated ()
  "`org-air-use-default-keybindings' nil: `?' is NOT bound in the
board/project maps and the doc leader entry is gone, yet `M-x
org-air-help' still produces the buffer — with honest faded `M-x
command-name' cells where keys are unbound, and `q' still quitting via
the `special-mode' parent.  Trunk FAILS on the buffer conjunct."
  (skip-unless (locate-library "org-air"))
  (org-air-r35--with-knob nil
    (should-not (eq (lookup-key org-air-view-mode-map (kbd "?"))
                    'org-air-help))
    (should-not (eq (lookup-key org-air-project-mode-map (kbd "?"))
                    'org-air-help))
    (should-not (eq (lookup-key org-air-doc-leader-map (kbd "?"))
                    'org-air-help))
    (save-window-excursion
      (unwind-protect
          (with-temp-buffer
            (org-air-view-mode)
            (org-air-help)                 ; the M-x path
            (should (buffer-live-p (get-buffer org-air-help-buffer-name)))
            (should (get-buffer-window org-air-help-buffer-name))
            (let ((text (org-air-r50--help-text)))
              ;; no live keys -> honest `M-x …' cells, never a lie.
              (should (string-match-p "M-x org-air-refresh" text))
              (should (string-match-p "M-x org-air-refile-item" text))
              (should-not (string-match-p "^  g r +refresh" text)))
            (with-current-buffer org-air-help-buffer-name
              ;; `q' rides the PARENT `special-mode' binding — the knob
              ;; only clears installer-owned keys, none live here.
              (should (eq (key-binding (kbd "q")) 'quit-window))))
        (org-air-r50--kill-help)))))

(provide 'org-air-round50-test)
;;; org-air-round50-test.el ends here
