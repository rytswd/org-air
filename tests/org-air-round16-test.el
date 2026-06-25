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

(ert-deftest org-air-r16-d1-board-only-keeps-popped-out-flag ()
  "Responsive board-only teardown is NOT a user close: the flag is kept.
Widening back past the rail-min-width must therefore re-pop the side
window rather than fall back to inline (design transition table)."
  (org-air-test-with-fixtures
    (let ((buf (get-buffer-create "*org-air-r16-test*")))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (org-air-view-mode)
              (setq-local org-air-view--rail-popped-out t))
            ;; Display the board in the batch selected window so the
            ;; predicate's `get-buffer-window' guard is satisfied; ensure no
            ;; rail window exists.
            (set-window-buffer (selected-window) buf)
            (let ((rb (get-buffer org-air-rail-buffer-name)))
              (when (buffer-live-p rb) (kill-buffer rb)))
            (with-current-buffer buf
              ;; Narrow -> board-only responsive teardown -> NOT a user close.
              (let ((org-air-view-width 60))
                (should (org-air-view--board-only-p (org-air-view--render-width)))
                (should-not (org-air-rail--user-closed-p buf)))
              ;; Wide + rail window absent -> a genuine user close.
              (let ((org-air-view-width 140))
                (should-not (org-air-view--board-only-p
                             (org-air-view--render-width)))
                (should (org-air-rail--user-closed-p buf)))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

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
;;;; D-P3 — view pane: byte goldens + dead marker + follow + window-config.
;;;; (Review-found gap: the bottom *org-air-view* pane shipped only loose
;;;;  smoke tests; these implement the design's full testability plan.)
;;;; ---------------------------------------------------------------------

(defconst org-air-r16--entry-view-source
  (org-air-test-fixture "entry-view/entry-view-source.org")
  "Isolated, deterministic source for the D-P3 view-pane goldens.
Lives under a sub-directory so the top-level `fixtures/*.org' board glob
never copies it into the GTD board set.")

(defmacro org-air-r16--frozen (&rest body)
  "Run BODY with `current-time' frozen to `org-air-test-now' (R16 D-P3)."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'current-time)
              (lambda () org-air-test-now)))
     ,@body))

(defun org-air-r16--pane-dump (ctx)
  "Render CTX into the view pane; return `header-line\nbody' (no props).
Identical to the regen dump so the byte golden is anti-tautological."
  (org-air-view-pane--render ctx)
  (with-current-buffer (get-buffer org-air-view-pane-buffer-name)
    (concat (format "%s" header-line-format)
            "\n"
            (buffer-substring-no-properties (point-min) (point-max)))))

(defun org-air-r16--drop-trailing-blanks (lines)
  "Drop trailing empty LINES (byte goldens carry an emit newline)."
  (let ((rev (reverse lines)))
    (while (and rev (string-empty-p (car rev)))
      (setq rev (cdr rev)))
    (nreverse rev)))

(defun org-air-r16--fixture-lines (name)
  "Return the lines of fixture NAME."
  (with-temp-buffer
    (insert-file-contents (org-air-test-fixture name))
    (split-string (buffer-string) "\n")))

(ert-deftest org-air-r16-d3-pane-content-byte-golden ()
  "The pane content (header-line + read-only entry snapshot) is the byte
contract, blessed by `make regen-mockups' at the frozen clock."
  (let ((buf (find-file-noselect org-air-r16--entry-view-source)))
    (unwind-protect
        (org-air-r16--frozen
          (let* ((mk (with-current-buffer buf
                       (goto-char (point-min))
                       (re-search-forward "^\\* TODO A heading" nil t)
                       (goto-char (match-beginning 0))
                       (point-marker)))
                 (ctx (list :marker mk :file org-air-r16--entry-view-source
                            :title "A heading" :state "TODO"))
                 (dump (org-air-r16--pane-dump ctx)))
            (should (equal (org-air-r16--drop-trailing-blanks
                            (split-string dump "\n"))
                           (org-air-r16--drop-trailing-blanks
                            (org-air-r16--fixture-lines "entry-view-pane.txt"))))
            ;; the snapshot is READ-ONLY and stops before the next subtree.
            (with-current-buffer (get-buffer org-air-view-pane-buffer-name)
              (should buffer-read-only)
              (should (derived-mode-p 'org-air-entry-view-mode))
              (should-not (string-match-p "Other heading"
                                          (buffer-string))))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest org-air-r16-d3-pane-dead-marker-byte-golden ()
  "A dead/unresolvable source shows the calm `org-air-face-empty' hint,
byte-pinned and faced (not an error)."
  (let* ((missing (org-air-test-fixture "entry-view/entry-view-missing.org"))
         (ctx (list :marker missing :file missing
                    :title "Missing entry" :state "TODO"))
         (dump (org-air-r16--pane-dump ctx)))
    (should (equal (org-air-r16--drop-trailing-blanks
                    (split-string dump "\n"))
                   (org-air-r16--drop-trailing-blanks
                    (org-air-r16--fixture-lines "entry-view-dead.txt"))))
    ;; the hint text carries the calm empty face.
    (with-current-buffer (get-buffer org-air-view-pane-buffer-name)
      (goto-char (point-min))
      (should (eq (get-text-property (point) 'face) 'org-air-face-empty)))))

(ert-deftest org-air-r16-d3-follow-redraws-on-item-change ()
  "Follow-mode redraws the pane only when the board item at point CHANGES.
The REAL debounce-callback seam `org-air-view--view-pane-update-now' drives
the change-guard (the post-command hook only schedules an idle timer, inert
under batch); only the window I/O is stubbed (batch-safe, inspector model)."
  (org-air-test-with-fixtures
    (let ((org-air-view-width 140)
          (org-air-view-height 40)
          (org-air-view-pane-follow t)
          (shown '()))
      (with-temp-buffer
        (org-air-view-mode)
        (setq org-air-view--items (org-air-query-items))
        (org-air-view--render org-air-view--items nil)
        (let (posA posB itemA itemB)
          (save-excursion
            (goto-char (point-min))
            (while (and (not posB) (not (eobp)))
              (let ((it (get-text-property (point) 'org-air-item)))
                (when it
                  (cond ((null posA) (setq posA (point) itemA it))
                        ((not (eq it itemA)) (setq posB (point) itemB it)))))
              (goto-char (or (next-single-property-change
                              (point) 'org-air-item)
                             (point-max)))))
          (should posA)
          (should posB)
          (cl-letf (((symbol-function 'org-air-view-pane--window-live-p)
                     (lambda () t))
                    ((symbol-function 'org-air-view-pane--show)
                     (lambda (ctx)
                       (push (plist-get ctx :title) shown)
                       (org-air-view-pane--render ctx))))
            (let ((board (current-buffer)))
              ;; move to A -> redraw (item changed from the nil seed).
              (goto-char posA)
              (org-air-view--view-pane-update-now board)
              (should (eq org-air-view--view-pane-item itemA))
              (should (= (length shown) 1))
              ;; staying on A (same item) -> NO redraw.
              (org-air-view--view-pane-update-now board)
              (should (= (length shown) 1))
              ;; move to B -> redraw.
              (goto-char posB)
              (org-air-view--view-pane-update-now board)
              (should (eq org-air-view--view-pane-item itemB))
              (should (= (length shown) 2)))))))))

;;;; D-P3 window-config (real side windows are GUI-only).

(ert-deftest org-air-r16-d3-pane-window-is-board-split-and-reachable ()
  "R19-3 re-bless: the pane SPLITS the board window
\(`display-buffer-below-selected'), so it is NOT a frame-level side window
\(`window-side' nil) — that is exactly what stops it from shortening the
rail.  It is tagged `org-air-pane', dedicated, and stays
`other-window'-reachable (no `no-other-window').  Found by the
`org-air-pane' parameter, since under the default editable pane the window
shows the per-heading indirect buffer, not `*org-air-view*'."
  (skip-unless (display-graphic-p))
  (save-window-excursion
    (delete-other-windows)
    (let ((ctx (list :marker org-air-r16--entry-view-source
                     :file org-air-r16--entry-view-source
                     :title "A heading" :state "TODO")))
      (unwind-protect
          (progn
            (org-air-view-pane--show ctx)
            (let ((win (org-air-view-pane--find-window)))
              (should (window-live-p win))
              (should-not (window-parameter win 'window-side)) ; NOT a side window
              (should (window-parameter win 'org-air-pane))
              (should-not (window-parameter win 'no-other-window))))
        (org-air-view-pane--teardown)))))

(ert-deftest org-air-r16-d3-pane-survives-delete-other-windows ()
  "`delete-other-windows' (C-x 1) preserves the pane (no-delete-other-windows)."
  (skip-unless (display-graphic-p))
  (save-window-excursion
    (delete-other-windows)
    (let ((ctx (list :marker org-air-r16--entry-view-source
                     :file org-air-r16--entry-view-source
                     :title "A heading" :state "TODO")))
      (unwind-protect
          (progn
            (org-air-view-pane--show ctx)
            (delete-other-windows)
            (should (org-air-view-pane--window-live-p)))
        (org-air-view-pane--teardown)))))

(ert-deftest org-air-r16-d3-pane-coexists-with-rail-side-window ()
  "The bottom view pane coexists with the right rail side window."
  (skip-unless (display-graphic-p))
  (org-air-test-with-fixtures
    (save-window-excursion
      (delete-other-windows)
      (with-temp-buffer
        (org-air-view-mode)
        (setq org-air-view--items (org-air-query-items))
        (let ((board (current-buffer))
              (ctx (list :marker org-air-r16--entry-view-source
                         :file org-air-r16--entry-view-source
                         :title "A heading" :state "TODO")))
          (unwind-protect
              (progn
                (org-air-rail--ensure-window board 120)
                (org-air-view-pane--show ctx)
                (let ((rail-win (get-buffer-window
                                 (get-buffer org-air-rail-buffer-name)))
                      (pane-win (org-air-view-pane--find-window)))
                  (should (window-live-p rail-win))
                  (should (window-live-p pane-win))
                  (should (eq (window-parameter rail-win 'window-side)
                              org-air-rail-side))
                  ;; R19-3 re-bless: the pane is the board-window SPLIT, not
                  ;; a frame-level side window (so the rail keeps its height).
                  (should-not (window-parameter pane-win 'window-side))))
            (org-air-view-pane--teardown)
            (org-air-rail--hide board)))))))

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
