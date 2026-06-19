;;; org-air-round16-test.el --- round-16 D-P1/D-P3/D-P4/D-P5 tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for round-16 (air/v0.5/org-air-round16-design.org):
;;   D-P1 cooperative command-driven side-window rail,
;;   D-P3 mu4e-style bottom *org-air-view* source pane,
;;   D-P4 project-view sorting (name/created/updated + direction),
;;   D-P5 within-group state ordering (state-rank primary),
;;   D-P2 stable *org-air buffer-naming contract.
;; Window-config behaviour (real side windows) is GUI-only and guarded
;; `(skip-unless (display-graphic-p))'; everything else is batch-safe via
;; the render seams (text-is-the-contract, mirroring the inspector model).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-air-test-helpers)
(require 'org-air-view)
(require 'org-air-project)

;;;; ---------------------------------------------------------------------
;;;; D-P2 — stable buffer-naming contract.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r16-d2-buffer-names-share-prefix ()
  "Every on-demand org-air buffer name starts with the `*org-air' prefix."
  (dolist (name (list org-air-view-buffer-name
                      org-air-rail-buffer-name
                      org-air-view-pane-buffer-name
                      "*org-air-project*"))
    (should (string-prefix-p "*org-air" name))))

;;;; ---------------------------------------------------------------------
;;;; D-P1 — cooperative side-window (batch-safe state seams).
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r16-d1-rail-window-params-have-no-other-window-removed ()
  "The rail side window is `other-window'-reachable (no `no-other-window')."
  (let* ((params (org-air-rail--window-params 30))
         (wp (cdr (assq 'window-parameters params))))
    (should-not (assq 'no-other-window wp))
    (should (assq 'no-delete-other-windows wp))))

(ert-deftest org-air-r16-d1-toggle-flips-runtime-flag ()
  "`org-air-rail-toggle' flips the per-board runtime flag, not the defcustom."
  (org-air-test-with-fixtures
    (let ((org-air-rail-style 'inline))
      (with-temp-buffer
        (org-air-view-mode)
        (setq-local org-air-view--rail-popped-out nil)
        ;; popout
        (cl-letf (((symbol-function 'org-air-view--render-current) #'ignore)
                  ((symbol-function 'org-air-rail--hide) #'ignore))
          (org-air-rail-toggle)
          (should (eq org-air-view--rail-popped-out t))
          ;; popin
          (org-air-rail-toggle)
          (should (eq org-air-view--rail-popped-out nil)))
        ;; the defcustom is untouched.
        (should (eq org-air-rail-style 'inline))))))

(ert-deftest org-air-r16-d1-dispatch-reads-flag-not-style ()
  "Render dispatch chooses `side-window' from the runtime flag, not the style."
  (org-air-test-with-fixtures
    (let ((org-air-rail-style 'inline)
          (org-air-view-width 140)
          (org-air-view-height 40))
      (with-temp-buffer
        (org-air-view-mode)
        (setq org-air-view--items (org-air-query-items))
        ;; flag nil -> inline two-pane at a wide width.
        (setq-local org-air-view--rail-popped-out nil)
        (org-air-view--render org-air-view--items nil)
        (should (eq org-air-view--orientation 'two-pane))
        ;; flag t -> side-window even though the style is inline.
        (setq-local org-air-view--rail-popped-out t)
        (org-air-view--render org-air-view--items nil)
        (should (eq org-air-view--orientation 'side-window))))))

;;;; ---------------------------------------------------------------------
;;;; D-P3 — bottom source view pane (text-is-the-contract).
;;;; ---------------------------------------------------------------------

(defun org-air-r16--pane-text (ctx)
  "Render CTX into the pane buffer and return its text."
  (org-air-view-pane--render ctx)
  (with-current-buffer (get-buffer org-air-view-pane-buffer-name)
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest org-air-r16-d3-pane-snapshots-entry ()
  "The view pane snapshots the heading + body + drawers of the entry."
  (let ((tmp (make-temp-file "oa-r16" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "* TODO A heading :foo:\n"
                    "SCHEDULED: <2026-06-20 Sat>\n"
                    "Body line one\nBody line two\n"
                    "* Other heading\nshould not appear\n"))
          (let* ((buf (find-file-noselect tmp))
                 (mk (with-current-buffer buf
                       (goto-char (point-min)) (point-marker)))
                 (text (org-air-r16--pane-text
                        (list :marker mk :file tmp
                              :title "A heading" :state "TODO"))))
            (should (string-match-p "A heading" text))
            (should (string-match-p "Body line one" text))
            (should (string-match-p "Body line two" text))
            ;; the next subtree must NOT leak in.
            (should-not (string-match-p "Other heading" text))
            (kill-buffer buf)))
      (delete-file tmp))))

(ert-deftest org-air-r16-d3-pane-dead-marker-hint ()
  "An unresolvable source shows the calm hint, not an error."
  (let ((text (org-air-r16--pane-text
               (list :marker "/no/such/file-xyz.org" :title "X"))))
    (should (string-match-p "no longer available" text))))

(ert-deftest org-air-r16-d3-pane-header-line-contract ()
  "The header-line is the text `<icon> <file> . <title> . <state>'."
  (let ((hl (org-air-view-pane--header-line
             (list :file "/x/foo.org" :title "A heading" :state "TODO"))))
    (should (string-match-p "foo\\.org" hl))
    (should (string-match-p "A heading" hl))
    (should (string-match-p "TODO" hl))))

(ert-deftest org-air-r16-d3-pane-max-lines-caps ()
  "`org-air-view-pane-max-lines' caps the entry with a continuation marker."
  (let ((tmp (make-temp-file "oa-r16cap" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "* Big\n")
            (dotimes (i 40) (insert (format "line %d\n" i))))
          (let* ((buf (find-file-noselect tmp))
                 (mk (with-current-buffer buf
                       (goto-char (point-min)) (point-marker)))
                 (org-air-view-pane-max-lines 5)
                 (text (org-air-r16--pane-text
                        (list :marker mk :title "Big"))))
            (should (<= (length (split-string text "\n" t)) 7))
            (should (string-match-p "line 0" text))
            (should-not (string-match-p "line 30" text))
            (kill-buffer buf)))
      (delete-file tmp))))

;;;; ---------------------------------------------------------------------
;;;; D-P4 / D-P5 — comparator (the order is the contract).
;;;; ---------------------------------------------------------------------

(defun org-air-r16--doc (name state created)
  "Build a doc named NAME in STATE with CREATED/updated date."
  (org-air-doc-create :name name :state state :relpath name
                      :created created :updated created))

(ert-deftest org-air-r16-d4-nil-dates-sort-last-both-directions ()
  "Docs with a nil date always sort LAST, in ascending and descending."
  (let* ((docs (list (org-air-r16--doc "Zeta" "draft"
                                       (encode-time 0 0 0 1 1 2024))
                     (org-air-r16--doc "Alpha" "draft"
                                       (encode-time 0 0 0 1 1 2025))
                     (org-air-r16--doc "NilDoc" "draft" nil))))
    (let ((org-air-project--sort-key 'created)
          (org-air-project--sort-direction 'ascending))
      (should (equal (mapcar #'org-air-doc-name
                             (org-air-project--sort-section-docs docs))
                     '("Zeta" "Alpha" "NilDoc"))))
    (let ((org-air-project--sort-key 'created)
          (org-air-project--sort-direction 'descending))
      (should (equal (mapcar #'org-air-doc-name
                             (org-air-project--sort-section-docs docs))
                     '("Alpha" "Zeta" "NilDoc"))))))

(ert-deftest org-air-r16-d4-name-sort-is-default-order ()
  "The default key (name) orders alphabetically within a state."
  (let* ((docs (list (org-air-r16--doc "Charlie" "draft" nil)
                     (org-air-r16--doc "Alpha" "draft" nil)
                     (org-air-r16--doc "Bravo" "draft" nil)))
         (org-air-project--sort-key 'name)
         (org-air-project--sort-direction 'ascending))
    (should (equal (mapcar #'org-air-doc-name
                           (org-air-project--sort-section-docs docs))
                   '("Alpha" "Bravo" "Charlie")))))

(ert-deftest org-air-r16-d5-state-rank-is-within-group-primary ()
  "Within a mixed group, rows run by state-rank first, then the sort key.
Reversing direction flips only the secondary key, not the state order."
  (let* ((docs (list (org-air-r16--doc "Zed-complete" "complete" nil)
                     (org-air-r16--doc "Abe-ready" "ready" nil)
                     (org-air-r16--doc "Yan-draft" "draft" nil)
                     (org-air-r16--doc "Ben-draft" "draft" nil))))
    ;; name ascending: draft(Ben,Yan) -> ready(Abe) -> complete(Zed).
    (let ((org-air-project--sort-key 'name)
          (org-air-project--sort-direction 'ascending))
      (should (equal (mapcar #'org-air-doc-name
                             (org-air-project--sort-section-docs docs))
                     '("Ben-draft" "Yan-draft" "Abe-ready" "Zed-complete"))))
    ;; name descending: state order unchanged; only the draft pair flips.
    (let ((org-air-project--sort-key 'name)
          (org-air-project--sort-direction 'descending))
      (should (equal (mapcar #'org-air-doc-name
                             (org-air-project--sort-section-docs docs))
                     '("Yan-draft" "Ben-draft" "Abe-ready" "Zed-complete"))))))

(ert-deftest org-air-r16-d5-state-rank-unknown-last ()
  "An unknown state ranks after every known state."
  (should (> (org-air-project--state-rank "mystery")
             (org-air-project--state-rank "complete"))))

(provide 'org-air-round16-test)
;;; org-air-round16-test.el ends here
