;;; fidelity-dump.el --- v0.2 layout fidelity render -*- lexical-binding: t; -*-
;; Usage: emacs -Q --batch -L tests -l tests/org-air-test-init.el -l fidelity-dump.el
(require 'org-air-test-helpers)
(require 'org-air)
(require 'org-air-view)
(require 'org-air-faces)

(defun fidelity--faces ()
  "Distinct faces actually applied in the current buffer."
  (let ((faces '()) (pos (point-min)))
    (while (< pos (point-max))
      (let ((f (get-text-property pos 'face))
            (next (next-single-property-change pos 'face nil (point-max))))
        (when f (cl-pushnew f faces :test #'equal))
        (setq pos next)))
    (nreverse faces)))

(defun fidelity--render (width items label)
  (let ((org-air-view-width width))
    (with-current-buffer (get-buffer-create "*org-air*")
      (let ((inhibit-read-only t)) (erase-buffer))
      (org-air-view-mode)
      (setq org-air-view-width width)
      (setq org-air-view--items items)
      (org-air-view--render items nil)
      (princ (format "\n========== %s (width=%d, items=%d) ==========\n"
                     label width (length items)))
      (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
             (lines (split-string text "\n")))
        (dolist (l lines)
          (princ (format "%3d|%s\n" (string-width l) l)))
        (princ "----- distinct faces applied -----\n")
        (dolist (f (fidelity--faces)) (princ (format "  %S\n" f)))
        ;; geometry summary
        (let ((bar-cols (delete-dups
                         (delq nil (mapcar (lambda (l)
                                             (let ((i (string-search "│" l)))
                                               (when i (string-width (substring l 0 i)))))
                                           lines))))
              (over (delq nil (mapcar (lambda (l) (when (> (string-width l) width)
                                                    (string-width l)))
                                      lines))))
          (princ (format "----- divider display-cols: %S ; lines-over-%d: %S -----\n"
                         bar-cols width over)))))))

(cl-letf (((symbol-function 'current-time) (lambda () org-air-test-now)))
  (org-air-test-with-fixtures
    (let ((items (org-air-query-items)))
      (dolist (w '(80 100 120 140 160 200))
        (fidelity--render w items (format "POPULATED %d" w)))
      ;; empty board at a representative two-pane and stacked width
      (fidelity--render 120 nil "EMPTY-BOARD 120")
      (fidelity--render 80 nil "EMPTY-BOARD 80"))))
