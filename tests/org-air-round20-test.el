;;; org-air-round20-test.el --- round-20 substantive suite for org-air -*- lexical-binding: t; -*-

;;; Commentary:
;; Spec-true tests for v0.5 round-20 (air/v0.5/org-air-round20-design.org):
;; stability + polish + performance to make org-air dogfoodable.
;;
;;   R20-1  async first load HANGS -> a SYNCHRONOUS fast-paint load.  The
;;          chained-idle-timer path is gone; the cold open paints the
;;          chrome, forces it visible with `redisplay', then queries +
;;          renders inline, wrapped so a query error can never wedge the
;;          board (`--loading' is always cleared) nor dump a six-figure
;;          echo message (the bounded `--short-error' line).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)
(require 'org-air)

;;;; ---------------------------------------------------------------------
;;;; R20-1 — synchronous fast-paint first load (drop the idle-timer chain).
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r20-1-cold-load-sync-renders-board ()
  "A cold (non-cached) `org-air-view' renders the FULL board synchronously
and ends with `org-air-view--loading' nil — no idle-timer chain, no
half-painted skeleton left behind."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (org-air-viewport-test--with-frozen-now
      (let ((org-air-view-width 120)
            (org-air-view-height 50)
            (org-air-view-buffer-name "*org-air-r20-1-sync*"))
        (unwind-protect
            (progn
              (org-air-view)
              (with-current-buffer org-air-view-buffer-name
                (should-not org-air-view--loading)
                (should org-air-view--items)
                (let ((text (substring-no-properties (buffer-string))))
                  (should-not (string-match-p "Loading your board" text))
                  (should (string-match-p "Ship quarterly report" text)))))
          (when (get-buffer org-air-view-buffer-name)
            (kill-buffer org-air-view-buffer-name)))))))

(ert-deftest org-air-r20-1-cold-load-error-does-not-wedge ()
  "A query error in the COLD interactive load can never wedge the board: the
`unwind-protect' clears `org-air-view--loading', the buffer falls back to
the normal empty render (NOT the loading skeleton), and the surfaced echo
message is a single bounded line (< 200 chars) — locking out the
101 802-char `%S'-of-payload timer-error dump."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (org-air-viewport-test--with-frozen-now
      (let ((org-air-view-width 120)
            (org-air-view-height 50)
            (org-air-view-buffer-name "*org-air-r20-1-wedge*")
            (captured nil)
            ;; a realistic org-ql failure carrying a HUGE data payload (the
            ;; exact shape that made `%S' / `error-message-string' explode).
            (big (make-list 2000 (list :title "x" :tags '("a" "b" "c")))))
        (unwind-protect
            (cl-letf (((symbol-function 'org-air-query-items)
                       (lambda (&rest _)
                         (signal 'error (list "org-ql query failed" big))))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (setq captured (apply #'format fmt args))
                         captured)))
              ;; force the COLD interactive branch (batch normally takes the
              ;; synchronous cache-miss branch).
              (let ((noninteractive nil))
                (org-air-view))
              (with-current-buffer org-air-view-buffer-name
                ;; (a) never wedged:
                (should-not org-air-view--loading)
                ;; (b) the empty board, not the skeleton:
                (let ((text (substring-no-properties (buffer-string))))
                  (should-not (string-match-p "Loading your board" text)))
                ;; the failed query left no stale items:
                (should (null org-air-view--items)))
              ;; (c) the message is bounded and human (no payload dump):
              (should captured)
              (should (string-prefix-p "org-air: load failed:" captured))
              (should (< (length captured) 200)))
          (when (get-buffer org-air-view-buffer-name)
            (kill-buffer org-air-view-buffer-name)))))))

(ert-deftest org-air-r20-1-short-error-truncates-huge-payload ()
  "`org-air-view--short-error' returns a bounded single line even for an
error whose `error-message-string' is six figures long."
  (skip-unless (locate-library "org-air"))
  (let* ((big (make-list 4000 (list :a 1 :b 2)))
         (err (condition-case e
                  (signal 'error (list "boom" big))
                (error e)))
         (short (org-air-view--short-error err)))
    ;; the raw message is enormous; the short form is capped + single line.
    (should (> (length (error-message-string err)) 10000))
    (should (<= (length short) 161))
    (should-not (string-match-p "\n" short))))

;;;; ---------------------------------------------------------------------
;;;; R20-3 — view pane: `q'/`C-c C-q' closes it; cheap same-file follow.
;;;; ---------------------------------------------------------------------

(defmacro org-air-r20--with-temp-org (spec &rest body)
  "Bind dir/files per SPEC, write them, run BODY, clean up buffers + dir.
SPEC is ((DIRVAR) (VAR PATH CONTENT)...): DIRVAR holds a fresh temp dir;
each VAR is bound to an absolute path under it pre-populated with CONTENT."
  (declare (indent 1) (debug t))
  (let ((dirvar (caar spec))
        (files (cdr spec)))
    `(let* ((,dirvar (make-temp-file "org-air-r20-" t))
            ,@(mapcar (lambda (f)
                        `(,(nth 0 f) (expand-file-name ,(nth 1 f) ,dirvar)))
                      files))
       (unwind-protect
           (progn
             ,@(mapcar (lambda (f)
                         `(with-temp-file ,(nth 0 f) (insert ,(nth 2 f))))
                       files)
             ,@body)
         (let ((kill-buffer-query-functions nil))
           (dolist (buf (buffer-list))
             (let ((fn (buffer-file-name buf)))
               (when (and fn (string-prefix-p ,dirvar fn))
                 (with-current-buffer buf (set-buffer-modified-p nil))
                 (kill-buffer buf)))))
         (delete-directory ,dirvar t)))))

(defun org-air-r20--marker-at (base re)
  "Return a marker at the start of the line matching RE in BASE buffer."
  (with-current-buffer base
    (goto-char (point-min))
    (re-search-forward re)
    (goto-char (match-beginning 0))
    (point-marker)))

(defun org-air-r20--live-pane-indirects ()
  "Return the list of LIVE ` *org-air-pane:*' indirect buffers."
  (seq-filter (lambda (b)
                (and (buffer-live-p b)
                     (string-prefix-p " *org-air-pane:" (buffer-name b))))
              (buffer-list)))

(ert-deftest org-air-r20-3-pane-quit-tears-down-and-selects-board ()
  "`org-air-view-pane-quit' called for a focused pane runs the teardown: the
stashed indirect is killed (no ` *org-air-pane:*' survives), the host's
`org-air-view--pane-indirect' is cleared, and the board window is selected."
  (skip-unless (locate-library "org-air"))
  (org-air-r20--with-temp-org
      ((dir)
       (file "doc.org"
             "* TODO First heading :a:\n  one\n* TODO Second heading :b:\n  two\n"))
    (let* ((base (find-file-noselect file))
           (mk (org-air-r20--marker-at base "^\\* TODO First heading"))
           (ctx (list :marker mk :file file :title "First" :state "TODO"))
           (host (get-buffer-create "*org-air-r20-3-board*")))
      (unwind-protect
          (save-window-excursion
            (with-current-buffer host (org-air-view-mode))
            (set-window-buffer (selected-window) host)
            ;; build the editable indirect (the interactive follow path)
            (with-current-buffer host
              (let ((noninteractive nil))
                (org-air-view-pane--render ctx))
              (should (buffer-live-p org-air-view--pane-indirect)))
            (should (org-air-r20--live-pane-indirects))
            ;; quit from within the pane -> full teardown
            (org-air-view-pane-quit)
            (should-not (org-air-r20--live-pane-indirects))
            (with-current-buffer host
              (should-not org-air-view--pane-indirect))
            ;; the board window is the selected one again
            (should (eq (window-buffer (selected-window)) host)))
        (when (buffer-live-p host) (kill-buffer host))
        (dolist (b (org-air-r20--live-pane-indirects)) (kill-buffer b))))))

(ert-deftest org-air-r20-3-editable-pane-cq-quits-while-q-self-inserts ()
  "On the EDITABLE indirect pane the close verb is `C-c C-q' ->
`org-air-view-pane-quit', while `q' stays `self-insert-command' (it is a
live Org buffer); the snapshot `org-air-entry-view-mode-map' binds `q'."
  (skip-unless (locate-library "org-air"))
  (org-air-r20--with-temp-org
      ((dir)
       (file "doc.org" "* TODO Solo heading :z:\n  body\n"))
    (let* ((base (find-file-noselect file))
           (mk (org-air-r20--marker-at base "^\\* TODO Solo heading"))
           (ctx (list :marker mk :file file :title "Solo" :state "TODO"))
           (host (get-buffer-create "*org-air-r20-3-km*"))
           ind)
      (unwind-protect
          (with-current-buffer host
            (org-air-view-mode)
            (let ((noninteractive nil))
              (setq ind (org-air-view-pane--render ctx)))
            (should (buffer-live-p ind))
            (with-current-buffer ind
              (should (derived-mode-p 'org-mode))
              (should (eq (key-binding (kbd "C-c C-q"))
                          'org-air-view-pane-quit))
              ;; `q' is NOT a close key in the editable pane -- it inserts
              ;; (org-mode routes self-insert through `org-self-insert-command').
              (should (memq (key-binding (kbd "q"))
                            '(self-insert-command org-self-insert-command)))))
        ;; the read-only snapshot pane closes on plain `q'
        (should (eq (lookup-key org-air-entry-view-mode-map (kbd "q"))
                    'org-air-view-pane-quit))
        (when (buffer-live-p ind) (kill-buffer ind))
        (when (buffer-live-p host) (kill-buffer host))))))

(ert-deftest org-air-r20-3-follow-reuses-indirect-same-file-rebuilds-cross-file ()
  "The R20-3b cheap follow: re-rendering an item in the SAME base file REUSES
the existing indirect (same buffer object, only the narrowing moves); an
item in a DIFFERENT file builds a FRESH indirect on the new base."
  (skip-unless (locate-library "org-air"))
  (org-air-r20--with-temp-org
      ((dir)
       (a "a.org" "* TODO Head one :a:\n  one\n* TODO Head two :b:\n  two\n")
       (b "b.org" "* TODO Elsewhere :c:\n  three\n"))
    (let* ((basea (find-file-noselect a))
           (baseb (find-file-noselect b))
           (m1 (org-air-r20--marker-at basea "^\\* TODO Head one"))
           (m2 (org-air-r20--marker-at basea "^\\* TODO Head two"))
           (mb (org-air-r20--marker-at baseb "^\\* TODO Elsewhere"))
           (c1 (list :marker m1 :file a :title "Head one" :state "TODO"))
           (c2 (list :marker m2 :file a :title "Head two" :state "TODO"))
           (cb (list :marker mb :file b :title "Elsewhere" :state "TODO"))
           (host (get-buffer-create "*org-air-r20-3-follow*")))
      (unwind-protect
          (with-current-buffer host
            (org-air-view-mode)
            (let ((noninteractive nil))
              (let ((buf1 (org-air-view-pane--render c1)))
                (should (buffer-live-p buf1))
                (should (eq (buffer-base-buffer buf1) basea))
                ;; SAME file, different heading -> SAME indirect re-narrowed
                (let ((buf2 (org-air-view-pane--render c2)))
                  (should (eq buf2 buf1))
                  (should (eq (buffer-base-buffer buf2) basea))
                  ;; re-narrowed to the second subtree
                  (with-current-buffer buf2
                    (should (string-match-p "Head two" (buffer-string)))
                    (should-not (string-match-p "Head one" (buffer-string)))))
                ;; DIFFERENT file -> a FRESH indirect on the new base
                (let ((buf3 (org-air-view-pane--render cb)))
                  (should-not (eq buf3 buf1))
                  (should (eq (buffer-base-buffer buf3) baseb))))))
        (dolist (bb (org-air-r20--live-pane-indirects)) (kill-buffer bb))
        (when (buffer-live-p host) (kill-buffer host))))))

(provide 'org-air-round20-test)
;;; org-air-round20-test.el ends here
