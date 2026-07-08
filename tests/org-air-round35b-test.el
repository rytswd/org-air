;;; org-air-round35b-test.el --- executing ERTs for v0.5 round-35.1 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.5 round-35.1 (air/v0.5/org-air-round35-design.org):
;; the FOUR default-binding sites that lived OUTSIDE the R35 gate, now
;; folded under the SAME `org-air-use-default-keybindings' knob so org-air
;; binds NO key in ANY org-air buffer OR in the user's own visited files:
;;
;;   1. the `org-air-return-key' bound in a VISITED USER FILE by
;;      `org-air-view--enable-return' (HIGHEST PRIORITY — org-air must not
;;      touch the user's own file when the defaults are off);
;;   2. the calendar day-cell keymap (RET / mouse-1 -> `org-air-view-day');
;;   3. the read-only snapshot pane `q' (`org-air-entry-view-mode-map');
;;   4. the editable indirect-pane close map (`C-c C-q' / `quit-window'
;;      remap installed by `org-air-view-pane--install-close-map').
;;
;; Reuses the R35 test's `org-air-r35--with-knob' helper when present, else
;; defines an equivalent, so this file stands alone in the suite too.  No
;; golden is touched (keymaps are byte-invisible).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)

(when (locate-library "org-air")
  (require 'org-air))

;; Reuse the R35 verify harness' `org-air-r35--with-knob' when that file is
;; already loaded in the suite; otherwise define an equivalent so this file
;; stands alone.  We do NOT `require' it — the suite loads every test file
;; with `-l', and requiring here would load it a second time (redefining
;; its ERTs and erroring).
(unless (fboundp 'org-air-r35--with-knob)
  (defmacro org-air-r35--with-knob (value &rest body)
    "Run BODY with `org-air-use-default-keybindings' dynamically set to VALUE.
Reconciles the shared maps to VALUE first, then restores them to the OUTER
value on exit so no cleared/installed state leaks into a sibling test."
    (declare (indent 1))
    `(unwind-protect
         (let ((org-air-use-default-keybindings ,value))
           (org-air--sync-default-keybindings)
           ,@body)
       (org-air--sync-default-keybindings))))

;;;; ---------------------------------------------------------------------
;;;; 1. HIGHEST PRIORITY — the return key in a VISITED USER FILE.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r35-1b-return-key-in-visited-file-gated ()
  "With the knob nil org-air binds NO key in the user's OWN visited file.
Driving `org-air-view--enable-return' in an editable Org buffer leaves the
`org-air-return-key' UNBOUND in the shared return map; with the knob t the
key resolves to `org-air-return'."
  (skip-unless (locate-library "org-air"))
  (let ((org-air-return-key "C-c b"))
    ;; ON: the return key is installed in the shared return map.
    (org-air-r35--with-knob t
      (with-temp-buffer
        (org-mode)
        (org-air-view--enable-return nil nil)
        (should (eq (lookup-key org-air-return-mode-map (kbd "C-c b"))
                    'org-air-return))))
    ;; OFF: no key is bound in the user's file (a stale binding from a
    ;; prior knob-on visit is actively removed, too).
    (org-air-r35--with-knob nil
      (with-temp-buffer
        (org-mode)
        (org-air-view--enable-return nil nil)
        (should (null (lookup-key org-air-return-mode-map (kbd "C-c b"))))))))

;;;; ---------------------------------------------------------------------
;;;; 2. Calendar day cells.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r35-1b-calendar-day-cell-gated ()
  "The calendar day-cell keymap is gated: knob t binds RET / mouse-1 to
`org-air-view-day'; knob nil leaves the cell without an org-air keymap."
  (skip-unless (locate-library "org-air"))
  (org-air-r35--with-knob t
    (should (eq (lookup-key org-air-calendar-day-keymap (kbd "RET"))
                'org-air-view-day))
    (should (eq (lookup-key org-air-calendar-day-keymap [mouse-1])
                'org-air-view-day)))
  (org-air-r35--with-knob nil
    (should (null (lookup-key org-air-calendar-day-keymap (kbd "RET"))))
    (should (null (lookup-key org-air-calendar-day-keymap [mouse-1])))))

;;;; ---------------------------------------------------------------------
;;;; 3. The read-only snapshot pane `q'.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r35-1b-snapshot-pane-q-gated ()
  "The read-only snapshot pane's `q' is gated: knob t binds
`org-air-view-pane-quit'; knob nil removes it so `q' falls through to the
`special-mode' parent (`quit-window')."
  (skip-unless (locate-library "org-air"))
  (org-air-r35--with-knob t
    (should (eq (lookup-key org-air-entry-view-mode-map (kbd "q"))
                'org-air-view-pane-quit)))
  (org-air-r35--with-knob nil
    (with-temp-buffer
      (org-air-entry-view-mode)
      ;; org-air's own `q' is gone; the surviving parent provides quit.
      (should (eq (key-binding (kbd "q")) 'quit-window)))))

;;;; ---------------------------------------------------------------------
;;;; 4. The editable indirect-pane close map.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r35-1b-indirect-pane-close-map-gated ()
  "The editable indirect-pane close map is gated: knob t installs
`C-c C-q' / the `quit-window' remap -> `org-air-view-pane-quit'; knob nil
installs NO close map, so `C-c C-q' keeps its plain `org-mode' meaning."
  (skip-unless (locate-library "org-air"))
  (org-air-r35--with-knob t
    (with-temp-buffer
      (org-mode)
      (org-air-view-pane--install-close-map)
      (should (eq (key-binding (kbd "C-c C-q")) 'org-air-view-pane-quit))
      (should (eq (key-binding [remap quit-window]) 'org-air-view-pane-quit))))
  (org-air-r35--with-knob nil
    (with-temp-buffer
      (org-mode)
      (org-air-view-pane--install-close-map)
      (should-not (eq (key-binding (kbd "C-c C-q"))
                      'org-air-view-pane-quit)))))

(provide 'org-air-round35b-test)
;;; org-air-round35b-test.el ends here
