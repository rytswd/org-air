;;; org-air-bugs-test.el --- round-8 bug-batch (B1/B2/B4) -*- lexical-binding: t; -*-

;;; Commentary:
;; Round-8 bug-batch fast-track (design tstqmmxm / impl2 nrvkklvv),
;; landed ahead of the F5 project-mode stream because B1 is a critical
;; user-hitting hang.
;;
;;   B1 [CRIT] TAB on a non-header item must NEVER hang/loop; on a
;;             section header it toggles the fold AND keeps point on the
;;             header (so it can be collapsed again immediately).
;;   B2        org-air-return restores point to the originating item,
;;             not the top of the dashboard.
;;   B4        g-prefix map: g r = refresh, g g = top, g R = refresh+
;;             reset, G = bottom; the rail hint reflects it.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)

(when (locate-library "org-air")
  (require 'org-air))

;;;; B4 — g-prefix keymap (g r / g g / g R / G).

(ert-deftest org-air-b4-g-prefix-keymap ()
  "B4: `g' is a prefix map — g r = refresh, g g = top of pane, g R =
refresh+reset; `G' = bottom of pane (vim/evil g-prefix)."
  (skip-unless (locate-library "org-air"))
  (should (eq (lookup-key org-air-view-mode-map (kbd "g r")) 'org-air-refresh))
  (should (commandp (lookup-key org-air-view-mode-map (kbd "g g"))))
  (should (commandp (lookup-key org-air-view-mode-map (kbd "g R"))))
  (should (commandp (lookup-key org-air-view-mode-map (kbd "G"))))
  ;; `G' is a bottom-of-pane motion, NOT the old direct refresh-all.
  (should-not (eq (lookup-key org-air-view-mode-map (kbd "G"))
                  'org-air-refresh-all))
  ;; `g' alone is now a prefix (a keymap), not a direct command.
  (should (keymapp (lookup-key org-air-view-mode-map (kbd "g")))))

(ert-deftest org-air-b4-rail-hint-shows-g-prefix ()
  "B4 [byte]: the rail hint advertises the g-prefix (gr refresh) rather
than the old bare `g refresh'."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-as-gui
    (org-air-viewport-test-with-dashboard 120
      (let ((text (buffer-string)))
        (should (string-match-p "gr refresh\\|g r refresh" text))
        (should-not (string-match-p "[^r] g refresh" text))))))

;;;; B1 — TAB never hangs; header-only toggle keeps point on the header.

(ert-deftest org-air-b1-tab-on-non-header-is-safe ()
  "B1 [CRIT]: TAB on a NON-header item line must never hang or loop — it
returns promptly (a no-op or a move to the next header)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 120
    (goto-char (point-min))
    (should (search-forward "Prepare standup notes" nil t))
    (goto-char (line-beginning-position))
    (let ((cmd (key-binding (kbd "TAB"))))
      (should (commandp cmd))
      ;; A hang/infinite loop trips the timeout; we require prompt return.
      (should (eq 'ok (with-timeout (3 'org-air-b1--timeout)
                        (ignore-errors (call-interactively cmd))
                        'ok))))))

(ert-deftest org-air-b1-tab-on-header-keeps-point ()
  "B1: TAB on a section HEADER toggles its fold AND keeps point on the
header line (it must not jump to the first item)."
  (skip-unless (locate-library "org-air"))
  (org-air-viewport-test-with-dashboard 120
    (let ((hpos nil))
      (save-excursion
        (let ((p (point-min)))
          (while (and (< p (point-max)) (not hpos))
            (when (get-text-property p 'org-air-section) (setq hpos p))
            (setq p (1+ p)))))
      (should hpos)
      (goto-char hpos)
      (let ((line-before (line-number-at-pos)))
        (with-timeout (3 (ert-fail "TAB hung on a header"))
          (ignore-errors (call-interactively (key-binding (kbd "TAB")))))
        ;; Point stays on the same heading line (the section property sits
        ;; past the left margin, so scan the line rather than read BOL).
        (should (= (line-number-at-pos) line-before))
        (let ((p (line-beginning-position)) (eol (line-end-position)) found)
          (while (and (< p eol) (not found))
            (when (get-text-property p 'org-air-section) (setq found t))
            (setq p (1+ p)))
          (should found))))))

;;;; B2 — return restores point to the originating item.

(ert-deftest org-air-b2-return-restores-point ()
  "B2: after visiting an item and returning to the dashboard, point is
back on the ORIGINATING item row, not at the top."
  (skip-unless (locate-library "org-air"))
  (skip-unless (fboundp 'org-air-return))
  (org-air-viewport-test-with-dashboard 120
    (goto-char (point-min))
    (should (search-forward "Prepare standup notes" nil t))
    (goto-char (match-beginning 0))
    (org-air-visit-item)
    (org-air-return)
    (should (string-match-p "Prepare standup notes"
                            (buffer-substring-no-properties
                             (line-beginning-position) (line-end-position))))))

(provide 'org-air-bugs-test)
;;; org-air-bugs-test.el ends here
