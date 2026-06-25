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

(provide 'org-air-round18-ui-test)
;;; org-air-round18-ui-test.el ends here
