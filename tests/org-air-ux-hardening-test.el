;;; org-air-ux-hardening-test.el --- U1/U2/U3 behaviour tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Render-byte-independent behaviour tests for the v0.2.x UX hardening
;; round (change nqvzxwnv):
;;
;;   U1 — live-window width derivation: `org-air-layout-current-width'
;;        measures the window DISPLAYING the dashboard in COLUMNS (the
;;        pixel-width regression pushed the rail ~1000 cols off-screen),
;;        and the resize hook re-renders only on actual width change.
;;   U2 — evil compatibility: motion initial state + the dashboard
;;        keymap overriding evil's state maps, with evil never required.
;;   U3 — auto-refresh: saving any configured org file refreshes an open
;;        dashboard; refresh preserves the active filter and point.
;;
;; None of these assert composed render bytes, so they are stable across
;; the in-flight D-delta render changes.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)

(when (locate-library "org-air")
  (require 'org-air))

;;;; U1 — width derivation.

(ert-deftest org-air-ux-u1-width-uses-displaying-window ()
  "Width comes from the window displaying the buffer, not the selected one."
  (skip-unless (fboundp 'org-air-layout-current-width))
  (with-temp-buffer
    (let ((buffer (current-buffer))
          (fake-window 'org-air-test--fake-window))
      (cl-letf (((symbol-function 'get-buffer-window)
                 (lambda (buf &optional _all-frames)
                   (when (eq buf buffer) fake-window)))
                ((symbol-function 'window-live-p)
                 (lambda (w) (eq w fake-window)))
                ((symbol-function 'window-body-width)
                 (lambda (&optional w &rest _)
                   (if (eq w fake-window) 123 80))))
        (should (= (org-air-layout-current-width buffer) 123))))))

(ert-deftest org-air-ux-u1-width-in-columns-not-pixels ()
  "The width is measured in columns: PIXELWISE must never be passed.
Regression guard for the pixel-width bug that composed two-pane against
a ~1000-col width."
  (skip-unless (fboundp 'org-air-layout-current-width))
  (with-temp-buffer
    (let ((buffer (current-buffer))
          (fake-window 'org-air-test--fake-window))
      (cl-letf (((symbol-function 'get-buffer-window)
                 (lambda (&rest _) fake-window))
                ((symbol-function 'window-live-p)
                 (lambda (w) (eq w fake-window)))
                ((symbol-function 'window-body-width)
                 (lambda (&optional _w pixelwise)
                   (if pixelwise 1000 100))))
        (should (= (org-air-layout-current-width buffer) 100))))))

(ert-deftest org-air-ux-u1-width-fallback-chain ()
  "With no displaying window, fall back to a live selected window, then
the frame width — always a sane positive column count in batch."
  (skip-unless (fboundp 'org-air-layout-current-width))
  (with-temp-buffer
    ;; A temp buffer is displayed nowhere: fallback path.
    (let ((width (org-air-layout-current-width (current-buffer))))
      (should (integerp width))
      (should (> width 0))
      (should (<= width (frame-width))))))

(ert-deftest org-air-ux-u1-resize-refresh-only-on-width-change ()
  "`org-air-view--resize-refresh' re-renders iff the width changed."
  (skip-unless (fboundp 'org-air-view--resize-refresh))
  (org-air-viewport-test-with-dashboard 120
    (let ((renders 0))
      (cl-letf (((symbol-function 'org-air-view--render-current)
                 (lambda (&rest _) (cl-incf renders))))
        ;; Same width as the last render: no re-render.
        (org-air-view--resize-refresh)
        (should (= renders 0))
        ;; Width changed: exactly one re-render.
        (let ((org-air-view-width 80))
          (org-air-view--resize-refresh))
        (should (= renders 1))))))

;;;; U2 — evil compatibility.

(ert-deftest org-air-ux-u2-evil-initial-state-motion ()
  "The dashboard mode registers motion as its evil initial state."
  (skip-unless (locate-library "evil"))
  (require 'evil)
  ;; Trigger the mode body's evil setup by opening a dashboard.
  (org-air-viewport-test-with-dashboard 120
    (should (eq (evil-initial-state 'org-air-view-mode) 'motion))))

(ert-deftest org-air-ux-u2-dashboard-keys-win-under-evil ()
  "With evil active in the dashboard buffer, single-key dashboard
bindings still resolve to org-air commands (overriding map), and RET
visits the item — no backslash prefix needed."
  (skip-unless (locate-library "evil"))
  (require 'evil)
  (org-air-viewport-test-with-dashboard 120
    (evil-local-mode 1)
    (unwind-protect
        (progn
          (evil-change-state 'motion)
          ;; Every dashboard key must resolve to exactly what the mode
          ;; map binds — evil's motion-state map must not shadow it.
          ;; Round-8 B4: g is a prefix map, so probe its sub-binding `g r'.
          (dolist (key (list (kbd "RET") (kbd "g r") "n" "p" "/" "c"))
            (let ((own (lookup-key org-air-view-mode-map key)))
              (should (commandp own))
              (should (eq (key-binding key) own)))))
      (evil-local-mode -1))))

(ert-deftest org-air-ux-u2-evil-never-required ()
  "org-air sources never (require 'evil) — soft dependency only."
  (let ((root (locate-dominating-file org-air-test-fixture-dir "Makefile")))
    (should root)
    (dolist (src (directory-files root t "\\`org-air.*\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents src)
        (goto-char (point-min))
        (when (re-search-forward "(require 'evil" nil t)
          (ert-fail (format "%s hard-requires evil (line %d)"
                            (file-name-nondirectory src)
                            (line-number-at-pos))))))))

;;;; U3 — auto-refresh.

(ert-deftest org-air-ux-u3-after-save-hook-installed ()
  "The scoped after-save refresh hook is installed globally."
  (skip-unless (fboundp 'org-air-view--after-save-refresh))
  (should (memq 'org-air-view--after-save-refresh after-save-hook)))

(ert-deftest org-air-ux-u3-save-of-tracked-file-refreshes ()
  "Saving a configured org file refreshes an open dashboard."
  (skip-unless (fboundp 'org-air-view--after-save-refresh))
  ;; D-P1.PAD widens the cluster (titles truncate sooner); render wide so
  ;; the freshly captured headline is whole for the string search.
  (org-air-viewport-test-with-dashboard 160
    (should-not (string-match-p "Freshly captured headline" (buffer-string)))
    (let ((file (car (seq-filter (lambda (f) (string-suffix-p "inbox.org" f))
                                 org-air-files))))
      (should file)
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-max))
        (insert "\n* TODO Freshly captured headline\n")
        (save-buffer)))
    ;; The dashboard re-rendered itself off the after-save-hook.
    (should (string-match-p "Freshly captured headline"
                            (with-current-buffer "*org-air*"
                              (buffer-string))))))

(ert-deftest org-air-ux-u3-save-of-unrelated-file-does-not-refresh ()
  "Saving an untracked file never touches the dashboard."
  (skip-unless (fboundp 'org-air-view--after-save-refresh))
  (org-air-viewport-test-with-dashboard 120
    (let ((refreshes 0)
          (unrelated (make-temp-file "org-air-unrelated-" nil ".org")))
      (unwind-protect
          (cl-letf (((symbol-function 'org-air-refresh)
                     (lambda (&rest _) (cl-incf refreshes))))
            (with-current-buffer (find-file-noselect unrelated)
              (insert "* TODO Not one of ours\n")
              (save-buffer))
            (should (= refreshes 0)))
        (delete-file unrelated)))))

(ert-deftest org-air-ux-u3-refresh-preserves-filter-and-point ()
  "Refresh keeps the active tag filter and restores point to the item."
  (skip-unless (fboundp 'org-air-view--save-position))
  ;; D-P1.PAD widens the cluster (titles truncate sooner); render wide so
  ;; the parked "Prepare standup notes" row title is whole for the search.
  (org-air-viewport-test-with-dashboard 160
    ;; Park point on a known item row.
    (goto-char (point-min))
    (should (search-forward "Prepare standup notes" nil t))
    (goto-char (match-beginning 0))
    (org-air-refresh)
    (should (string-match-p "Prepare standup notes"
                            (buffer-substring (line-beginning-position)
                                              (line-end-position))))
    ;; Filter survives a refresh.
    (org-air-filter '("work"))
    (let ((before org-air-view--tag-filter))
      (should before)
      (org-air-refresh)
      (should (equal org-air-view--tag-filter before)))))

(provide 'org-air-ux-hardening-test)
;;; org-air-ux-hardening-test.el ends here
