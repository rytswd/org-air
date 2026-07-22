;;; org-air-round35-test.el --- R35 acceptance ERTs -*- lexical-binding: t; -*-

;;; Commentary:
;; Round-35 acceptance tests (air/v0.5/org-air-round35-design.org): one
;; boolean, `org-air-use-default-keybindings' (default t), opts out of ALL
;; org-air default keys while the mode maps still EXIST and keep their
;; `special-mode' parent.  Every assertion reads `key-binding' /
;; `lookup-key' in a live org-air buffer (or drives the pure sync helper);
;; no golden is touched (keymaps are not in goldens — byte-invisible).
;;
;; Coverage:
;;   * knob t (default): representative default keys resolve to org-air
;;     commands in a board buffer, a project buffer, the `C-c C-a' leader,
;;     and the `z' / `g' prefixes (regression fence over R18/R27/R30).
;;   * knob nil: those keys are UNBOUND in org-air's own maps (fall through
;;     to `special-mode' / self-insert); the evil overriding setup is
;;     SKIPPED; the maps still exist and inherit `special-mode'.
;;   * use-package ordering: `:custom' (set AFTER load, honoured at the next
;;     mode init), `setq'-before-the-load-seed, and the defcustom `:set'
;;     (instant live re-sync) all take effect; toggling back to t re-installs.
;;   * non-default special-mode keys still work with knob nil.

;;; Code:

(require 'ert)
(require 'cl-lib)

;;;; =====================================================================
;;;; Shared helper — toggle the knob, then ALWAYS restore the installed
;;;; default (the shared maps are GLOBAL; a leaked nil-clear would break
;;;; every later keymap test in the run).
;;;; =====================================================================

(defmacro org-air-r35--with-knob (val &rest body)
  "Set `org-air-use-default-keybindings' to VAL, sync the maps, run BODY.
Always restores the previous knob value AND force-resyncs the shared maps
back to it on exit (the maps are global, so a leaked clear must never
survive the test)."
  (declare (indent 1) (debug t))
  `(let ((org-air-r35--saved org-air-use-default-keybindings))
     (unwind-protect
         (progn
           (setq org-air-use-default-keybindings ,val)
           (org-air--sync-default-keybindings)
           ,@body)
       (setq org-air-use-default-keybindings org-air-r35--saved)
       (setq org-air--default-keybindings-state 'unset)
       (org-air--sync-default-keybindings))))

(defmacro org-air-r35--in-board (&rest body)
  "Run BODY in a fresh board-mode temp buffer (mode init re-syncs the maps)."
  (declare (indent 0) (debug t))
  `(with-temp-buffer (org-air-view-mode) ,@body))

(defmacro org-air-r35--in-project (&rest body)
  "Run BODY in a fresh project-mode temp buffer (mode init re-syncs the maps)."
  (declare (indent 0) (debug t))
  `(with-temp-buffer (org-air-project-mode) ,@body))

(defun org-air-r35--parent-reaches-p (map target)
  "Non-nil when TARGET keymap is reachable via MAP's parent chain."
  (let ((p map) (found nil))
    (while (and p (keymapp p) (not found))
      (when (eq p target) (setq found t))
      (setq p (keymap-parent p)))
    found))

;;;; =====================================================================
;;;; knob t (default) — every representative default key resolves.
;;;; =====================================================================

(ert-deftest org-air-r35-1-default-board-keys-resolve ()
  "With the knob t (default) a board buffer resolves the core / view / `z' /
`g' / `C-c C-a' leader defaults to their org-air commands (R18/R27/R30
regression fence)."
  (skip-unless (locate-library "org-air"))
  (should (boundp 'org-air-use-default-keybindings))
  (should (eq org-air-use-default-keybindings t))   ; the shipped default
  (org-air-r35--with-knob t
    (org-air-r35--in-board
      ;; core + view single keys.
      (should (eq (key-binding (kbd "c")) 'org-air-capture))
      (should (eq (key-binding (kbd "e")) 'org-air-refile-item))
      (should (eq (key-binding (kbd "n")) 'org-air-next-item))
      (should (eq (key-binding (kbd "p")) 'org-air-prev-item))
      (should (eq (key-binding (kbd "/")) 'org-air-filter))
      (should (eq (key-binding (kbd "|")) 'org-air-rail-toggle))
      (should (eq (key-binding (kbd "q")) 'org-air-quit))
      (should (eq (key-binding (kbd "?")) 'org-air-help))
      ;; the `z' column-toggle prefix.
      (should (eq (key-binding (kbd "z d")) 'org-air-toggle-dates))
      (should (eq (key-binding (kbd "z t")) 'org-air-toggle-tags))
      (should (eq (key-binding (kbd "z f")) 'org-air-toggle-origin))
      ;; the `g' prefix.
      (should (eq (key-binding (kbd "g r")) 'org-air-refresh))
      (should (eq (key-binding (kbd "g g")) 'org-air-goto-top))
      ;; the `C-c C-a' leader.
      (should (eq (key-binding (kbd "C-c C-a |")) 'org-air-rail-toggle))
      (should (eq (key-binding (kbd "C-c C-a s")) 'org-air-view-sort-cycle)))))

(ert-deftest org-air-r35-1-default-project-keys-resolve ()
  "With the knob t a project buffer resolves its grouping / open / refresh
defaults and the shared `C-c C-a' leader."
  (skip-unless (locate-library "org-air"))
  (org-air-r35--with-knob t
    (org-air-r35--in-project
      (should (eq (key-binding (kbd "s")) 'org-air-project-group-by-state))
      (should (eq (key-binding (kbd "d")) 'org-air-project-group-by-directory))
      (should (eq (key-binding (kbd "t")) 'org-air-project-group-by-tag))
      (should (eq (key-binding (kbd "RET")) 'org-air-project-open))
      (should (eq (key-binding (kbd "g")) 'org-air-project-refresh))
      ;; the shared leader reaches the rail/sort actions from the project too.
      (should (eq (key-binding (kbd "C-c C-a |")) 'org-air-rail-toggle))
      (should (eq (key-binding (kbd "C-c C-a s")) 'org-air-view-sort-cycle)))))

;;;; =====================================================================
;;;; knob nil — org-air's own action keys are gone; special-mode survives.
;;;; =====================================================================

(ert-deftest org-air-r35-1-nil-board-keys-unbound ()
  "With the knob nil the board map installs NONE of org-air's action keys:
`c' / `e' / `z' / `C-c C-a' resolve to nil in the map (special-mode does
not bind them), and `g' no longer opens the org-air `g' prefix — it falls
THROUGH to the `special-mode' parent (`revert-buffer'), never an org-air
command."
  (skip-unless (locate-library "org-air"))
  (org-air-r35--with-knob nil
    (org-air-r35--in-board
      ;; org-air's own action keys are absent from its map (R70-1: `e' is
      ;; the editor key now; `r' stays trivially absent — dropped, no alias).
      (should (null (lookup-key org-air-view-mode-map (kbd "c"))))
      (should (null (lookup-key org-air-view-mode-map (kbd "e"))))
      (should (null (lookup-key org-air-view-mode-map (kbd "r"))))
      (should (null (lookup-key org-air-view-mode-map (kbd "z"))))
      (should (null (lookup-key org-air-view-mode-map (kbd "C-c C-a"))))
      ;; nothing on these keys resolves to an org-air command…
      (dolist (k '("c" "e" "r" "/" "|" "z d" "g r" "C-c C-a |"))
        (let ((b (key-binding (kbd k))))
          (ert-info ((format "key %S -> %S" k b))
            (should-not (and (symbolp b) b
                             (string-prefix-p "org-air" (symbol-name b)))))))
      ;; …and `g' fell through to special-mode's `revert-buffer' (removed,
      ;; not nil-shadowed), so it is NOT the org-air prefix keymap.
      (should (eq (key-binding (kbd "g")) 'revert-buffer)))))

(ert-deftest org-air-r35-1-nil-project-keys-unbound ()
  "With the knob nil the project map installs none of its own keys: `s' /
`RET' are nil in the map, `g' falls through to `revert-buffer', and no key
resolves to an org-air project command."
  (skip-unless (locate-library "org-air"))
  (org-air-r35--with-knob nil
    (org-air-r35--in-project
      (should (null (lookup-key org-air-project-mode-map (kbd "s"))))
      (should (null (lookup-key org-air-project-mode-map (kbd "RET"))))
      (dolist (k '("s" "d" "t" "RET"))
        (let ((b (key-binding (kbd k))))
          (should-not (and (symbolp b) b
                           (string-prefix-p "org-air" (symbol-name b))))))
      (should (eq (key-binding (kbd "g")) 'revert-buffer)))))

(ert-deftest org-air-r35-1-nil-special-mode-keys-still-work ()
  "With the knob nil the maps keep their `special-mode' chain, so the stock
navigation/quit keys still resolve in a board AND project buffer:
`q' -> quit-window, `g' -> revert-buffer, SPC -> scroll.  (The user can
still quit/scroll, and owns all org-air ACTION keys.)"
  (skip-unless (locate-library "org-air"))
  (org-air-r35--with-knob nil
    (dolist (setup '(org-air-view-mode org-air-project-mode))
      (with-temp-buffer
        (funcall setup)
        (ert-info ((format "mode=%s" setup))
          (should (eq (key-binding (kbd "q")) 'quit-window))
          (should (eq (key-binding (kbd "g")) 'revert-buffer))
          (should (eq (key-binding (kbd "SPC")) 'scroll-up-command)))))))

(ert-deftest org-air-r35-1-nil-evil-setup-skipped ()
  "With the knob nil the evil overriding-map setup is SKIPPED: after mode
init `org-air-view--evil-modes' stays empty and neither
`evil-make-overriding-map' nor `evil-set-initial-state' is invoked; with
the knob t both fire for the board AND project modes.  (Stubs record the
calls — evil itself is a soft dep, absent in batch.)"
  (skip-unless (locate-library "org-air"))
  (let (calls)
    (cl-letf (((symbol-function 'evil-make-overriding-map)
               (lambda (&rest _) (push 'overriding calls)))
              ((symbol-function 'evil-set-initial-state)
               (lambda (&rest _) (push 'initial calls))))
      ;; knob t: both modes register + both evil calls fire.
      (org-air-r35--with-knob t
        (let ((org-air-view--evil-modes nil))
          (setq calls nil)
          (org-air-r35--in-board)
          (org-air-r35--in-project)
          (should (assq 'org-air-view-mode org-air-view--evil-modes))
          (should (assq 'org-air-project-mode org-air-view--evil-modes))
          (should (memq 'overriding calls))
          (should (memq 'initial calls))))
      ;; knob nil: no registration, no evil call.
      (org-air-r35--with-knob nil
        (let ((org-air-view--evil-modes nil))
          (setq calls nil)
          (org-air-r35--in-board)
          (org-air-r35--in-project)
          (should-not org-air-view--evil-modes)
          (should-not calls))))))

(ert-deftest org-air-r35-1-maps-exist-and-inherit-special-mode ()
  "With the knob nil the map OBJECTS still exist, keep their `special-mode'
parent chain, and remain `define-key' targets — the variables are never
rebound."
  (skip-unless (locate-library "org-air"))
  (org-air-r35--with-knob nil
    (org-air-r35--in-board
      (should (keymapp org-air-view-mode-map))
      (should (keymapp org-air-view-core-map))
      (should (keymapp org-air-project-mode-map))
      ;; special-mode-map is reachable in the parent chain
      ;; (view-mode-map -> view-core-map -> special-mode-map).
      (should (org-air-r35--parent-reaches-p org-air-view-mode-map
                                             special-mode-map))
      (should (org-air-r35--parent-reaches-p org-air-project-mode-map
                                             special-mode-map))
      ;; a user bind sticks and resolves.
      (define-key org-air-view-mode-map (kbd "X") #'ignore)
      (unwind-protect
          (should (eq (key-binding (kbd "X")) 'ignore))
        (define-key org-air-view-mode-map (kbd "X") nil t)))))

;;;; =====================================================================
;;;; Reversibility + use-package ordering.
;;;; =====================================================================

(ert-deftest org-air-r35-1-toggle-back-to-t-reinstalls ()
  "install <=> clear is reversible and idempotent: from the nil state,
setting the knob t and syncing makes the board `c' / `z d' / leader keys
resolve again; running sync a second time changes nothing."
  (skip-unless (locate-library "org-air"))
  (org-air-r35--with-knob nil
    ;; start cleared.
    (org-air-r35--in-board
      (should-not (eq (key-binding (kbd "c")) 'org-air-capture)))
    ;; flip back on and re-sync.
    (setq org-air-use-default-keybindings t)
    (org-air--sync-default-keybindings)
    (org-air-r35--in-board
      (should (eq (key-binding (kbd "c")) 'org-air-capture))
      (should (eq (key-binding (kbd "z d")) 'org-air-toggle-dates))
      (should (eq (key-binding (kbd "C-c C-a |")) 'org-air-rail-toggle)))
    ;; idempotent: sync again, keys unchanged.
    (org-air--sync-default-keybindings)
    (org-air-r35--in-board
      (should (eq (key-binding (kbd "c")) 'org-air-capture)))))

(ert-deftest org-air-r35-1-use-package-custom-ordering ()
  "The knob is honoured for the two load-order styles the spec documents:
- `:custom' (set AFTER the feature loads): the load seed installed the
  defaults, then setting the knob nil and opening the FIRST board buffer
  re-syncs at mode init -> keys unbound;
- `setq'-before-the-load-seed: with the value already nil, the (simulated)
  load seed installs nothing -> unbound from the first buffer."
  (skip-unless (locate-library "org-air"))
  (let ((saved org-air-use-default-keybindings))
    (unwind-protect
        (progn
          ;; (a) :custom style — defaults already seeded at t, THEN nil is set
          ;; and the first board buffer's mode-init sync clears them.
          (setq org-air-use-default-keybindings t)
          (setq org-air--default-keybindings-state 'unset)
          (org-air--sync-default-keybindings)          ; the load seed (t)
          (setq org-air-use-default-keybindings nil)   ; use-package :custom
          (org-air-r35--in-board                       ; mode-init re-syncs
            (should (null (lookup-key org-air-view-mode-map (kbd "c"))))
            (should-not (eq (key-binding (kbd "z d")) 'org-air-toggle-dates)))
          ;; (b) setq-before-require style — value nil BEFORE the load seed.
          (setq org-air-use-default-keybindings nil
                org-air--default-keybindings-state 'unset)
          (org-air--sync-default-keybindings)          ; the load seed reads nil
          (org-air-r35--in-board
            (should (null (lookup-key org-air-view-mode-map (kbd "c"))))))
      ;; restore the installed default.
      (setq org-air-use-default-keybindings saved
            org-air--default-keybindings-state 'unset)
      (org-air--sync-default-keybindings))))

(ert-deftest org-air-r35-1-defcustom-set-flips-live-maps ()
  "The defcustom `:set' re-syncs the LIVE maps immediately (no fresh buffer
needed): a `customize-set-variable' to nil unbinds the board `c' on the
spot, and back to t re-installs it — exactly the runtime `customize' path."
  (skip-unless (locate-library "org-air"))
  (let ((saved org-air-use-default-keybindings))
    (unwind-protect
        (progn
          (customize-set-variable 'org-air-use-default-keybindings t)
          (should (eq org-air--default-keybindings-state t))
          (should (eq (lookup-key org-air-view-mode-map (kbd "c"))
                      'org-air-capture))
          ;; :set nil -> live clear.
          (customize-set-variable 'org-air-use-default-keybindings nil)
          (should (eq org-air--default-keybindings-state nil))
          (should (null (lookup-key org-air-view-mode-map (kbd "c"))))
          ;; :set t -> live re-install.
          (customize-set-variable 'org-air-use-default-keybindings t)
          (should (eq (lookup-key org-air-view-mode-map (kbd "c"))
                      'org-air-capture)))
      (customize-set-variable 'org-air-use-default-keybindings saved)
      (setq org-air--default-keybindings-state 'unset)
      (org-air--sync-default-keybindings))))

;;;; =====================================================================
;;;; R35.1 GAPS — the D-2 sync state-guard, the empty-key opt-out's
;;;; independence, and toggle-back re-installing the four stray sites too.
;;;; (The four gated sites' t-vs-nil bindings themselves are covered in
;;;; org-air-round35b-test.el; these lock the invariants around them.)
;;;; =====================================================================

(ert-deftest org-air-r35-1c-sync-state-guard-no-redundant-reinstall ()
  "D-2 restored: `org-air--sync-default-keybindings' acts ONLY when the
desired state differs from `org-air--default-keybindings-state', so it does
NOT re-install (or re-clear) on every mode init.  Spying on the installer /
clearer: a sync (and a mode init) whose value matches the current state is
a no-op; a genuine flip runs exactly once; a repeat flip is guarded."
  (skip-unless (locate-library "org-air"))
  (let ((install 0) (clear 0)
        (saved org-air-use-default-keybindings))
    (unwind-protect
        (cl-letf (((symbol-function 'org-air--install-default-keybindings)
                   (lambda () (cl-incf install) t))
                  ((symbol-function 'org-air--clear-default-keybindings)
                   (lambda () (cl-incf clear) t)))
          ;; already installed (state t): a sync is a NO-OP…
          (setq org-air-use-default-keybindings t
                org-air--default-keybindings-state t)
          (org-air--sync-default-keybindings)
          (should (= install 0))
          (should (= clear 0))
          ;; …and a fresh mode init (which calls sync) does NOT re-install.
          (with-temp-buffer (org-air-view-mode))
          (with-temp-buffer (org-air-project-mode))
          (should (= install 0))
          ;; a genuine flip to nil clears EXACTLY once…
          (setq org-air-use-default-keybindings nil)
          (org-air--sync-default-keybindings)
          (should (= clear 1))
          ;; …and a repeat sync at the same value is guarded (no 2nd clear).
          (org-air--sync-default-keybindings)
          (with-temp-buffer (org-air-view-mode))
          (should (= clear 1))
          (should (= install 0)))
      ;; restore a real, installed default.
      (setq org-air-use-default-keybindings saved
            org-air--default-keybindings-state 'unset)
      (org-air--sync-default-keybindings))))

(ert-deftest org-air-r35-1c-return-key-empty-opt-out-independent ()
  "The empty-string `org-air-return-key' opt-out is INDEPENDENT of the knob:
even with the knob t, `org-air-view--enable-return' installs NO return key
when `org-air-return-key' is \"\" — no map entry resolves to `org-air-return'
\(the pre-existing per-key opt-out still wins), and the return minor mode is
still enabled."
  (skip-unless (locate-library "org-air"))
  (org-air-r35--with-knob t
    (let ((org-air-return-key ""))
      (with-temp-buffer
        (org-mode)
        ;; no error, mode enabled, and NO key resolves to org-air-return.
        (org-air-view--enable-return nil nil)
        (should (bound-and-true-p org-air-return-mode))
        (should-not (where-is-internal 'org-air-return
                                       org-air-return-mode-map))))))

(ert-deftest org-air-r35-1c-toggle-back-reinstalls-all-four-sites ()
  "Toggling the knob back to t re-installs ALL of R35.1's gated sites, not
just the primary maps: the visited-file return key, the calendar day-cell
RET, the snapshot pane `q', and the indirect-pane close map all resolve
again (proving the installer table owns every stray site, reversibly)."
  (skip-unless (locate-library "org-air"))
  (let ((org-air-return-key "C-c b"))
    ;; go cleared first…
    (org-air-r35--with-knob nil
      (with-temp-buffer (org-mode) (org-air-view--enable-return nil nil)
        (should (null (lookup-key org-air-return-mode-map (kbd "C-c b")))))
      (should (null (lookup-key org-air-calendar-day-keymap (kbd "RET")))))
    ;; …then flip back on and confirm every site re-installs.
    (org-air-r35--with-knob t
      ;; (1) visited-file return key.
      (with-temp-buffer (org-mode) (org-air-view--enable-return nil nil)
        (should (eq (lookup-key org-air-return-mode-map (kbd "C-c b"))
                    'org-air-return)))
      ;; (2) calendar day cell.
      (should (eq (lookup-key org-air-calendar-day-keymap (kbd "RET"))
                  'org-air-view-day))
      ;; (3) snapshot pane q.
      (should (eq (lookup-key org-air-entry-view-mode-map (kbd "q"))
                  'org-air-view-pane-quit))
      ;; (4) indirect-pane close map.
      (with-temp-buffer (org-mode) (org-air-view-pane--install-close-map)
        (should (eq (key-binding (kbd "C-c C-q")) 'org-air-view-pane-quit)))
      ;; and a primary board key too (the whole set is back).
      (org-air-r35--in-board
        (should (eq (key-binding (kbd "c")) 'org-air-capture))))))

(ert-deftest org-air-r35-1c-readme-scope-accurate ()
  "README doc-accuracy fence (R35.1): the \"Disabling the default
keybindings\" section describes the ACCURATE scope — it does NOT overstate
that org-air \"installs none of its own keys\" (the maps keep their
`special-mode' parent), it names the crucial visited-files scope
\(`org-air-return-key' / the close keys in your OWN files), and it still
notes special-mode survives.  Guards the prose from drifting back to the
pre-R35.1 overstatement."
  (skip-unless (and (boundp 'org-air-test-root)
                    (file-exists-p (expand-file-name "README.org"
                                                     org-air-test-root))))
  (let ((readme (with-temp-buffer
                  (insert-file-contents
                   (expand-file-name "README.org" org-air-test-root))
                  (buffer-string))))
    ;; the section exists.
    (should (string-match-p "Disabling the default keybindings" readme))
    ;; the pre-R35.1 OVERSTATEMENT is gone (org-air never disowns the
    ;; special-mode parent, so "installs none of its own keys" was wrong).
    (should-not (string-match-p "installs \\*?none\\*? of its own keys" readme))
    ;; the ACCURATE scope is stated: no keys OF ITS OWN, incl. visited files.
    (should (string-match-p "no keys \\*?of its own\\*?" readme))
    (should (string-match-p "org-air-return-key" readme))
    (should (string-match-p "files you visit\\|your OWN files\\|visited" readme))
    ;; and it still tells the user special-mode (q / g / scroll) survives.
    (should (string-match-p "special-mode" readme))))

(provide 'org-air-round35-test)
;;; org-air-round35-test.el ends here
