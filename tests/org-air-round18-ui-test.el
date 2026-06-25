;;; org-air-round18-ui-test.el --- round-18 D-P2..D-P5 UI tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for round-18 D-P2..D-P5 (air/v0.5/org-air-round18-design.org):
;;   D-P2 filter — pre-fill the prompt, AND-by-default, an AND/OR toggle on
;;        `M-/', and the active combinator surfaced in the banner + rail;
;;   D-P3 project parity — the doc-aware shared filter core, the shared
;;        keymap parent, the view-pane hook in the project;
;;   D-P4 view pane — auto-follow by default, RET opens / 2nd-RET focuses,
;;        S-RET visits;
;;   D-P5 modernisation — calm mode-line, pane chrome, calmer colour, svg
;;        softening (each behind a defcustom, byte-invisible).
;; Interactive seams are batch-guarded the way the R16 pane tests are.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)
(require 'org-air)
(require 'org-air-view)
(require 'org-air-project)

(defun org-air-r18--goto-first-item ()
  "Move point to the first board row carrying an `org-air-item'."
  (goto-char (or (text-property-not-all (point-min) (point-max)
                                        'org-air-item nil)
                 (point-min))))

;;;; ---------------------------------------------------------------------
;;;; D-P2 — filter: pre-fill + AND default + AND/OR toggle + combinator show.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r18-dp2-filter-match-default-is-all ()
  "The shipped `org-air-filter-match' default is `all' (AND), the R18 flip.
Guards the default so a second filter term NARROWS by default."
  (should (eq (default-value 'org-air-filter-match) 'all)))

(ert-deftest org-air-r18-dp2-filter-prompt-prefills-current ()
  "`org-air-filter' PRE-FILLS the prompt with the active filter (crm-joined).
Stubs `completing-read-multiple' to capture INITIAL-INPUT; with a two-tag
active filter the interactive form passes the comma-joined current filter
so the user continues narrowing instead of restarting."
  (org-air-viewport-test-with-dashboard 140
    (setq org-air-view--tag-filter '("work" "home"))
    (let ((captured 'unset))
      (cl-letf (((symbol-function 'completing-read-multiple)
                 (lambda (_prompt _table &optional _pred _req initial &rest _)
                   (setq captured initial)
                   ;; return the same filter unchanged (no-op narrow)
                   '("work" "home"))))
        (call-interactively #'org-air-filter))
      (should (equal captured "work,home")))))

(ert-deftest org-air-r18-dp2-filter-by-tag-prefills-first ()
  "`org-air-filter-by-tag' pre-fills `read-string' with the first active tag."
  (org-air-viewport-test-with-dashboard 140
    (setq org-air-view--tag-filter '("work" "home"))
    (let ((captured 'unset))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (_prompt &optional initial &rest _)
                   (setq captured initial)
                   "work")))
        (call-interactively #'org-air-filter-by-tag))
      (should (equal captured "work")))))

(ert-deftest org-air-r18-dp2-toggle-match-flips-and-echoes ()
  "`org-air-filter-toggle-match' flips `all' <-> `any' and re-renders."
  (org-air-viewport-test-with-dashboard 140
    (setq org-air-filter-match 'all)
    (let ((org-air-filter-match 'all))
      (org-air-filter-toggle-match)
      (should (eq org-air-filter-match 'any))
      (org-air-filter-toggle-match)
      (should (eq org-air-filter-match 'all)))))

(ert-deftest org-air-r18-dp2-toggle-match-bound-to-meta-slash ()
  "`M-/' resolves to `org-air-filter-toggle-match' in the board map."
  (org-air-viewport-test-with-dashboard 140
    (should (eq (key-binding (kbd "M-/")) 'org-air-filter-toggle-match))
    ;; The sibling filter keys are unchanged.
    (should (eq (key-binding (kbd "/")) 'org-air-filter))
    (should (eq (key-binding (kbd "\\")) 'org-air-filter-clear))))

(ert-deftest org-air-r18-dp2-banner-shows-combinator ()
  "The banner joins >=2 filter tags with the active combinator word.
`#work AND #home' under `all'; `#work OR #home' under `any'; a single tag
shows no combinator (checked on the banner line, the rail keeps its
discoverability `Match:' cue).  `org-air-filter-match' is `let'-bound so
the test never leaks the global default."
  (org-air-viewport-test-with-dashboard 140
    (let ((org-air-filter-match 'all))
      (setq org-air-view--tag-filter '("work" "home"))
      (org-air-view--render-current)
      (let ((banner (car (split-string (buffer-string) "\n"))))
        (should (string-match-p "#work AND #home" banner))
        (should-not (string-match-p "#work OR #home" banner)))
      ;; OR
      (setq org-air-filter-match 'any)
      (org-air-view--render-current)
      (let ((banner (car (split-string (buffer-string) "\n"))))
        (should (string-match-p "#work OR #home" banner)))
      ;; single tag -> the BANNER shows no combinator word
      (setq org-air-view--tag-filter '("work"))
      (org-air-view--render-current)
      (let ((banner (car (split-string (buffer-string) "\n"))))
        (should (string-match-p "#work" banner))
        (should-not (string-match-p " AND " banner))
        (should-not (string-match-p " OR " banner))))))

(ert-deftest org-air-r18-dp2-rail-filters-show-match-cue ()
  "The rail Filters block joins chips with the combinator and shows a cue.
Calls `org-air-view--insert-rail-filters' directly (layout-agnostic) with a
two-tag filter and asserts the `Match: AND  (M-/ toggles)' cue + the joined
chips; the OR mode swaps the word."
  (with-temp-buffer
    (let ((org-air-show-rail-filters t)
          (org-air-view--tag-filter '("work" "home"))
          (org-air-view--scope nil)
          (org-air-filter-match 'all))
      (org-air-view--insert-rail-filters 40)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "#work AND #home" text))
        (should (string-match-p "Match: AND" text))
        (should (string-match-p "M-/ toggles" text))))
    (erase-buffer)
    (let ((org-air-show-rail-filters t)
          (org-air-view--tag-filter '("work" "home"))
          (org-air-view--scope nil)
          (org-air-filter-match 'any))
      (org-air-view--insert-rail-filters 40)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "#work OR #home" text))
        (should (string-match-p "Match: OR" text))))))

;;;; ---------------------------------------------------------------------
;;;; D-P4 — view pane: auto-follow default + RET opens / 2nd-RET focuses.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r18-dp4-pane-follow-default-is-t ()
  "The shipped `org-air-view-pane-follow' default is t (R18 D-P4 flip).
Guards the auto-inspect default: once the pane is open, point auto-follows."
  (should (eq (default-value 'org-air-view-pane-follow) t)))

(ert-deftest org-air-r18-dp4-on-return-is-obsolete ()
  "`org-air-view-pane-on-return' is marked obsolete (RET owns the pane now)."
  (should (get 'org-air-view-pane-on-return 'byte-obsolete-variable)))

(ert-deftest org-air-r18-dp4-return-opens-then-focuses ()
  "`org-air-view-pane-return': first RET opens (no focus), 2nd RET focuses.
Deterministic under batch: `--window-live-p' is stubbed to flip closed->open
and `--show' captures the dynamic `org-air-view-pane-focus' the command
bound.  First call sees a CLOSED pane => focus nil (point stays on board);
second call sees a LIVE pane => focus t (select the pane window)."
  (org-air-test-with-fixtures
    (let ((org-air-view-width 140)
          (org-air-view-height 40))
      (with-temp-buffer
        (org-air-view-mode)
        (setq org-air-view--items (org-air-query-items))
        (org-air-view--render org-air-view--items nil)
        (org-air-r18--goto-first-item)
        (should (get-text-property (point) 'org-air-item))
        (let ((focus-seen '())
              (live nil))
          (cl-letf (((symbol-function 'org-air-view-pane--window-live-p)
                     (lambda () live))
                    ((symbol-function 'org-air-view-pane--show)
                     (lambda (_ctx)
                       (push org-air-view-pane-focus focus-seen)
                       ;; simulate the pane becoming live after the 1st open
                       (setq live t)
                       nil)))
            ;; first RET: pane closed -> opened without focus.
            (org-air-view-pane-return)
            (should (= (length focus-seen) 1))
            (should (null (car focus-seen)))
            ;; second RET: pane now live -> focus the pane window.
            (org-air-view-pane-return)
            (should (= (length focus-seen) 2))
            (should (eq (car focus-seen) t))))))))

(ert-deftest org-air-r18-dp4-keymap-ret-and-sret-board-and-project ()
  "RET -> pane-return and S-RET -> visit in BOTH the board and project maps.
The board's S-RET visits the item; the project's S-RET visits the doc."
  ;; Board map.
  (should (eq (lookup-key org-air-view-mode-map (kbd "RET"))
              'org-air-view-pane-return))
  (should (eq (lookup-key org-air-view-mode-map (kbd "<S-return>"))
              'org-air-visit-item))
  (should (eq (lookup-key org-air-view-mode-map (kbd "O"))
              'org-air-visit-item))
  ;; Project map (mirrored until D-P3 folds the keymaps).
  (should (eq (lookup-key org-air-project-mode-map (kbd "RET"))
              'org-air-view-pane-return))
  (should (eq (lookup-key org-air-project-mode-map (kbd "<S-return>"))
              'org-air-project-visit))
  ;; The project's domain verbs are NOT shadowed by the mirror.
  (should (eq (lookup-key org-air-project-mode-map (kbd "O"))
              'org-air-project-sort-reverse))
  (should (eq (lookup-key org-air-project-mode-map (kbd "s"))
              'org-air-project-group-by-state)))

;;;; ---------------------------------------------------------------------
;;;; D-P3 — project parity: shared filter core + shared keymap parent.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r18-dp3-tags-pass-filter-core ()
  "`org-air-view--tags-pass-filter-p' is the shared AND/OR tag matcher.
Lists in, combinator honoured, case-insensitive, empty filter passes all."
  ;; AND: every active tag must be present.
  (let ((org-air-view--tag-filter '("ui" "core"))
        (org-air-filter-match 'all))
    (should (org-air-view--tags-pass-filter-p '("ui" "core" "x")))
    (should-not (org-air-view--tags-pass-filter-p '("ui")))
    (should-not (org-air-view--tags-pass-filter-p '("core"))))
  ;; OR: any one active tag suffices.
  (let ((org-air-view--tag-filter '("ui" "core"))
        (org-air-filter-match 'any))
    (should (org-air-view--tags-pass-filter-p '("ui")))
    (should (org-air-view--tags-pass-filter-p '("core")))
    (should-not (org-air-view--tags-pass-filter-p '("zzz"))))
  ;; case-insensitive both ways.
  (let ((org-air-view--tag-filter '("UI"))
        (org-air-filter-match 'all))
    (should (org-air-view--tags-pass-filter-p '("ui"))))
  ;; no filter -> everything passes (even an empty tag list).
  (let ((org-air-view--tag-filter nil))
    (should (org-air-view--tags-pass-filter-p '()))
    (should (org-air-view--tags-pass-filter-p '("whatever")))))

(ert-deftest org-air-r18-dp3-passes-filter-p-delegates ()
  "The board's `org-air-view--passes-filter-p' delegates to the shared core."
  (let ((org-air-view--tag-filter '("work"))
        (org-air-filter-match 'all))
    (let ((item (org-air-item-create :title "x" :tags '("work" "home"))))
      (should (org-air-view--passes-filter-p item)))
    (let ((item (org-air-item-create :title "y" :tags '("home"))))
      (should-not (org-air-view--passes-filter-p item)))))

(ert-deftest org-air-r18-dp3-project-keymap-inherits-core ()
  "The project map INHERITS the shared view-core keys via its parent.
RET/v/V/\\/M-/ resolve through `org-air-view-core-map'; `/' is the
per-mode doc filter; the project DOMAIN verbs are NOT shadowed."
  ;; The project map's parent is the shared core map.
  (should (eq (keymap-parent org-air-project-mode-map) org-air-view-core-map))
  (should (eq (keymap-parent org-air-view-mode-map) org-air-view-core-map))
  ;; Inherited view-core keys resolve in the project map.
  (should (eq (lookup-key org-air-project-mode-map (kbd "RET"))
              'org-air-view-pane-return))
  (should (eq (lookup-key org-air-project-mode-map (kbd "v"))
              'org-air-view-pane))
  (should (eq (lookup-key org-air-project-mode-map (kbd "V"))
              'org-air-view-pane-close))
  (should (eq (lookup-key org-air-project-mode-map (kbd "\\"))
              'org-air-filter-clear))
  (should (eq (lookup-key org-air-project-mode-map (kbd "M-/"))
              'org-air-filter-toggle-match))
  ;; Per-mode `/' filter differs between the two views (the candidate
  ;; source differs), so it stays in each child map.
  (should (eq (lookup-key org-air-project-mode-map (kbd "/"))
              'org-air-project-filter))
  (should (eq (lookup-key org-air-view-mode-map (kbd "/"))
              'org-air-filter))
  ;; Domain verbs are NOT shadowed by the shared parent.
  (should (eq (lookup-key org-air-project-mode-map (kbd "s"))
              'org-air-project-group-by-state))
  (should (eq (lookup-key org-air-project-mode-map (kbd "o"))
              'org-air-project-sort-cycle))
  (should (eq (lookup-key org-air-project-mode-map (kbd "g"))
              'org-air-project-refresh)))

(ert-deftest org-air-r18-dp3-core-map-keeps-special-mode-parent ()
  "The shared core map keeps `special-mode-map' as its grandparent.
So `special-mode' defaults (e.g. the scroll/quit chain) stay reachable
below the org-air bindings in both views."
  (should (eq (keymap-parent org-air-view-core-map) special-mode-map)))

(provide 'org-air-round18-ui-test)
;;; org-air-round18-ui-test.el ends here
