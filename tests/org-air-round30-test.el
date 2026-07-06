;;; org-air-round30-test.el --- executing ERTs for v0.5 round-30 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.5 round-30 (air/v0.5/org-air-round30-design.org).
;;
;;   R30-1  RAIL INSPECTOR TITLE — full-wrap (no truncate) + a compact
;;          title/state/tags identity header block atop, shared board +
;;          project.  `org-air-inspector-max-title-lines' now accepts nil
;;          (default) = no cap; a positive integer keeps the ellipsis-cap.
;;
;;   R30-2  MAIN-WINDOW C-c LEADER — a shared `C-c C-a' leader prefix on
;;          the content buffers reaches the rail actions where single keys
;;          self-insert; the legend derives each key context-correctly via
;;          `org-air-view--legend-key'.
;;
;;   R30-3  DASHBOARD COLUMN TOGGLES — `org-air-show-origin' (nil),
;;          `-dates' (t), `-tags' (t) defcustoms gate the V6 meta-width
;;          pass; z-prefix toggles; filter/scope still read the hidden data.
;;
;;   R30-4  org-air-outline-mode — an opt-in minor mode for ANY org buffer
;;          reusing the extracted outline + highlight primitives with NO
;;          org-air-project dependency.
;;
;;   R30-5  DOC-RAIL COVERAGE ERT — a fringe-less GUI-sim ERT revert-
;;          guarding `org-air-project--doc-rail-show' (R29-1 fix site).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)
(require 'org-air-project-test)            ; project fixture root
(require 'org-air-round27-test)            ; live-board/-project harness
(require 'org-air-round28-test)            ; doc-session harness
(require 'org-air-round29-test)            ; fringe-less GUI sim harness

(when (locate-library "org-air")
  (require 'org-air))

;;;; =====================================================================
;;;; R30-2 — main-window C-c leader for the rail actions.
;;;; =====================================================================

(ert-deftest org-air-r30-2-leader-reaches-actions-from-doc ()
  "In the doc-session ORG buffer (main window focused, single keys
self-insert) the leader reaches the rail actions: `C-c C-a |' is
`org-air-rail-toggle', `C-c C-a o' jumps the outline
\(`org-air-outline-goto-current-heading'), `C-c C-a q' is
`org-air-project-back'.  Trunk FAILED (no leader; the keys self-insert)."
  (skip-unless (locate-library "org-air"))
  (org-air-r28--with-doc-session
    (with-current-buffer docbuf
      (should (buffer-local-value 'org-air-doc-session-mode docbuf))
      ;; the bare keys DO NOT reach the rail actions in the editable doc
      ;; buffer (they self-insert)...
      (should-not (eq (key-binding (kbd "|")) 'org-air-rail-toggle))
      ;; ...but the leader reaches every action.
      (should (eq (key-binding (kbd "C-c C-a |")) 'org-air-rail-toggle))
      (should (eq (key-binding (kbd "C-c C-a o"))
                  'org-air-outline-goto-current-heading))
      (should (eq (key-binding (kbd "C-c C-a n"))
                  'org-air-outline-next-heading))
      (should (eq (key-binding (kbd "C-c C-a p"))
                  'org-air-outline-prev-heading))
      (should (eq (key-binding (kbd "C-c C-a q")) 'org-air-project-back)))))

(ert-deftest org-air-r30-2-leader-outline-jump-moves-point ()
  "The shared heading-motion primitives really move point over the Org
headings of the current buffer (pure, Air-free): `next'/`prev' step
forward/back, `goto-current' jumps to the enclosing heading.  Anti-
tautology for the leader binding test."
  (skip-unless (locate-library "org-air"))
  (with-temp-buffer
    (org-mode)
    (insert "#+title: Demo\n* One\nbody one\n** Two\nbody two\n* Three\nbody three\n")
    (let ((heads (org-air-outline--heading-positions)))
      (should (= (length heads) 3))
      (goto-char (point-min))
      (call-interactively #'org-air-outline-next-heading)
      (should (= (point) (nth 0 heads)))
      (call-interactively #'org-air-outline-next-heading)
      (should (= (point) (nth 1 heads)))
      (call-interactively #'org-air-outline-next-heading)
      (should (= (point) (nth 2 heads)))
      (call-interactively #'org-air-outline-prev-heading)
      (should (= (point) (nth 1 heads)))
      ;; `o' (jump to the enclosing heading) from inside the second
      ;; section snaps back to that heading's start.
      (goto-char (+ (nth 1 heads) 4))
      (call-interactively #'org-air-outline-goto-current-heading)
      (should (= (point) (nth 1 heads))))))

(ert-deftest org-air-r30-2-legend-shows-context-key ()
  "The DOC-session rail Actions legend cells read the LEADER form
\(C-c C-a …) for the verbs that self-insert in the doc buffer (jump,
rail), while the board/project rail legend reads BARE keys — both derived
by `org-air-view--legend-key' from the correct buffer.  Trunk FAILED (the
doc legend hardcoded `RET jump' / `| rail', dead from the doc buffer)."
  (skip-unless (locate-library "org-air"))
  (org-air-r28--with-doc-session
    ;; the doc legend, live in the rail, shows the reachable LEADER keys.
    (with-current-buffer org-air-rail-buffer-name
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "C-c C-a o jump" text))
        (should (string-match-p "C-c C-a | rail" text))
        ;; no dead bare `RET jump' cell (trunk hardcoded it; RET newlines
        ;; in the doc buffer).
        (should-not (string-match-p "RET jump" text))))
    ;; the legend-key derivation itself: BARE in the read-only project
    ;; tree (where `|' toggles the rail), LEADER in the editable doc
    ;; buffer (where `|' self-inserts).
    (should (equal (org-air-view--legend-key #'org-air-rail-toggle tree)
                   "|"))
    (should (equal (org-air-view--legend-key #'org-air-rail-toggle docbuf)
                   "C-c C-a |"))))

(ert-deftest org-air-r30-2-bare-keys-still-work ()
  "The leader is ADDITIVE: in the read-only rail the bare `RET' / `|' /
`q' resolve to their commands unchanged, and on the board the bare `|'
still toggles the rail."
  (skip-unless (locate-library "org-air"))
  (org-air-r27--with-live-board
    (org-air-r27--pop-rail)
    (with-current-buffer org-air-rail-buffer-name
      (should (eq (key-binding (kbd "RET")) 'org-air-rail-return))
      (should (eq (key-binding (kbd "|")) 'org-air-rail-popin))
      (should (eq (key-binding (kbd "q")) 'org-air-rail-quit)))
    (with-current-buffer (current-buffer)
      (should (eq (key-binding (kbd "|")) 'org-air-rail-toggle)))))

(ert-deftest org-air-r30-2-evil-state-agnostic ()
  "With REAL evil enabled in the doc buffer, `C-c C-a |' resolves to
`org-air-rail-toggle' in BOTH normal and insert state (a `C-c' leader is
left alone by evil in every state)."
  (skip-unless (locate-library "org-air"))
  (skip-unless (locate-library "evil"))
  (require 'evil)
  (org-air-r28--with-doc-session
    (with-current-buffer docbuf
      (evil-local-mode 1)
      (dolist (state '(normal insert))
        (evil-change-state state)
        (should (eq evil-state state))
        (should (eq (key-binding (kbd "C-c C-a |")) 'org-air-rail-toggle))
        (should (eq (key-binding (kbd "C-c C-a q")) 'org-air-project-back)))
      (evil-local-mode -1))))

(ert-deftest org-air-r30-2-leader-key-defcustom ()
  "`org-air-leader-key' is a typed key defcustom; setting it via the
Custom `:set' re-installs the prefix at the NEW key on every registered
host map and unbinds the OLD key (the legend follows via `where-is')."
  (skip-unless (locate-library "org-air"))
  (should (eq (get 'org-air-leader-key 'custom-type) 'key-sequence))
  (let ((orig org-air-leader-key))
    (unwind-protect
        (progn
          (customize-set-variable 'org-air-leader-key "C-c C-y")
          ;; the new key reaches the board leader; the old one is gone.
          (should (eq (lookup-key org-air-view-mode-map (kbd "C-c C-y"))
                      org-air-leader-map))
          (should-not (lookup-key org-air-view-mode-map (kbd "C-c C-a")))
          ;; the doc-session host moved too.
          (should (eq (lookup-key org-air-doc-session-mode-map (kbd "C-c C-y"))
                      org-air-doc-leader-map)))
      (customize-set-variable 'org-air-leader-key orig)
      (should (eq (lookup-key org-air-view-mode-map (kbd "C-c C-a"))
                  org-air-leader-map)))))

(provide 'org-air-round30-test)
;;; org-air-round30-test.el ends here
