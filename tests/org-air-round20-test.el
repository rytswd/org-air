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
;;          R26-8 re-bless: the INTERACTIVE cold path is the cache-first
;;          CACHED/COLD dispatch now (skeleton + token-guarded chunked
;;          refresh); `noninteractive' keeps the exact synchronous path,
;;          and the machine-START error keeps the R20-1 bounded-failure
;;          discipline (pinned below).

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
  "R26-8 re-bless of the retired sync-cold contract: the interactive COLD
path is the ASYNC machine now.
 (1) DISPATCH: with no cache, `org-air-view' RETURNS with the skeleton
     painted, `org-air-view--loading' t and the chunked refresh QUEUED
     (state `refreshing', file queue non-empty) — and NO synchronous
     query runs inside the call (an erroring `org-air-query-items' stub
     proves it; there is nothing synchronous left to fail).  The
     slice-error half of the old no-wedge guarantee lives in the machine
     and is pinned by org-air-r26-8-failure-honest-and-g-retries.
 (2) MACHINE-START error (the one failure still inside the call, e.g.
     the file list itself): the R20-1 discipline holds — `--loading'
     cleared (never a wedge), the EMPTY board (not the skeleton), no
     stale items, and ONE bounded `load failed' line (< 200 chars; locks
     out the 101 802-char `%S'-of-payload dump)."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (org-air-viewport-test--with-frozen-now
      (let ((org-air-view-width 120)
            (org-air-view-height 50)
            (org-air-view-buffer-name "*org-air-r20-1-wedge*")
            (org-air-cache-file nil)     ; no persisted cache -> COLD
            (captured nil)
            (sync-query-ran nil)
            ;; a realistic org-ql failure carrying a HUGE data payload (the
            ;; exact shape that made `%S' / `error-message-string' explode).
            (big (make-list 2000 (list :title "x" :tags '("a" "b" "c")))))
        (unwind-protect
            (progn
              ;; (1) the COLD dispatch: skeleton + loading + queued machine;
              ;; the call itself never runs the query synchronously.
              (cl-letf (((symbol-function 'org-air-query-items)
                         (lambda (&rest _)
                           (setq sync-query-ran t)
                           (signal 'error (list "sync query must not run")))))
                (let ((noninteractive nil))
                  (org-air-view))
                (with-current-buffer org-air-view-buffer-name
                  (should-not sync-query-ran)
                  (should org-air-view--loading)
                  (should (eq org-air-view--refresh-state 'refreshing))
                  (should org-air-view--refresh-queue)
                  (let ((text (substring-no-properties (buffer-string))))
                    (should (string-match-p "Loading your board" text)))
                  ;; cancel the queued machine (no timer leaks into the
                  ;; suite; the slice half has its own deterministic ERTs).
                  (org-air-view--refresh-teardown)))
              (kill-buffer org-air-view-buffer-name)
              ;; (2) an error STARTING the machine keeps the R20-1 bounded-
              ;; failure discipline: empty board, loading cleared, one
              ;; short line.
              (cl-letf (((symbol-function 'org-air-query-files)
                         (lambda (&rest _)
                           (signal 'error (list "file list failed" big))))
                        ((symbol-function 'message)
                         (lambda (fmt &rest args)
                           (setq captured (apply #'format fmt args))
                           captured)))
                (let ((noninteractive nil))
                  (org-air-view))
                (with-current-buffer org-air-view-buffer-name
                  ;; (a) never wedged:
                  (should-not org-air-view--loading)
                  (should-not (eq org-air-view--refresh-state 'refreshing))
                  ;; (b) the empty board, not the skeleton:
                  (let ((text (substring-no-properties (buffer-string))))
                    (should-not (string-match-p "Loading your board" text)))
                  ;; the failed start left no stale items:
                  (should (null org-air-view--items)))
                ;; (c) the message is bounded and human (no payload dump):
                (should captured)
                (should (string-prefix-p "org-air: load failed:" captured))
                (should (< (length captured) 200))))
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
  "Return the list of LIVE `*org-air-pane:*' indirect buffers (R28-1 name)."
  (seq-filter (lambda (b)
                (and (buffer-live-p b)
                     (string-match-p "\\` ?\\*org-air-pane:" (buffer-name b))))
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

;;;; ---------------------------------------------------------------------
;;;; R20-4 — refile: action-first menu, dedicated move, CRM tags/category.
;;;; ---------------------------------------------------------------------

;; org-air-r20-4-refile-menu-is-action-first-move-leads — RETIRED (R64-3,
;; air/v0.5/org-air-round64-design.org).  The R20-4 action-first
;; `completing-read' menu it pinned (`org-air-inbox--refile-candidates')
;; is GONE: the transient form `org-air-refile-transient' shows
;; destination AND tags/category/schedule/todo/priority together on one
;; screen, so there is no action LIST left to lead with `⌂ Move to
;; file…' and no `#tag'/`@group' soup for it to exclude — the menu
;; SHAPE has no successor to re-bless against.  Its surviving guarantees
;; are asserted at their new homes: the `⌂' file candidates + decode in
;; the `f' reader (re-blessed
;; `org-air-r19-2-refile-prompt-shows-tags-and-move-relocates' and
;; `org-air-r20-4-move-target-rehomes-in-file-and-path-readers' below),
;; the CRM category semantics in the `c' reader (re-blessed
;; `org-air-r20-4-decode-category-first-is-category-rest-are-tags'), the
;; schedule quicks incl. `someday' in the `s' reader's option table
;; (`org-air-inbox--schedule-options'), and the command/binding/reader
;; contracts in tests/org-air-round64-test.el (spec seam T8, r64-7).

(ert-deftest org-air-r20-4-edit-categories-crm-prefilled ()
  "`org-air-inbox--edit-categories' is `completing-read-multiple' PRE-FILLED
with the item's current category; picking two yields a 2-element list (the
caller makes the first the `:CATEGORY:' and the rest tags)."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (let* ((item (org-air-item-create
                  :title "x" :tags '("a") :file (car org-air-files)
                  :group "work" :marker (point-marker)))
           (captured-initial 'unset))
      (cl-letf (((symbol-function 'completing-read-multiple)
                 (lambda (_prompt _coll &optional _pred _req initial &rest _)
                   (setq captured-initial initial)
                   '("work" "finance"))))
        (let ((result (org-air-inbox--edit-categories item)))
          ;; pre-filled with the CURRENT category
          (should (equal captured-initial "work"))
          ;; multi-pick -> a 2-element list
          (should (equal result '("work" "finance"))))))))

(ert-deftest org-air-r20-4-decode-category-first-is-category-rest-are-tags ()
  "R64-3 re-bless: `org-air-inbox--decode-target' is RETIRED with the
action menu; the CRM first-is-category-rest-are-tags semantics live on
in the form's `c' infix reader (`org-air-refile-form-category' over the
unchanged `org-air-inbox--edit-categories') — the CRM is PRE-FILLED
with the item's current category, the FIRST pick becomes the form's
`:category' and every EXTRA pick is merged onto the form's `:tags'
\(the item's current tags kept) — nothing the user typed is lost."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (let ((item (org-air-item-create
                 :title "x" :tags '("keep") :file (car org-air-files)
                 :group "work" :marker (point-marker)))
          (org-air-inbox--refile-form nil)
          (captured-initial 'unset))
      (org-air-inbox--form-init item)
      (cl-letf (((symbol-function 'completing-read-multiple)
                 (lambda (_prompt _coll &optional _pred _req initial &rest _)
                   (setq captured-initial initial)
                   '("finance" "q3"))))
        (call-interactively 'org-air-refile-form-category))
      ;; pre-filled with the CURRENT category (the R20-4a CRM shape kept)
      (should (equal captured-initial "work"))
      (should (equal (org-air-inbox--form-get :category) "finance"))
      (should (member "q3" (org-air-inbox--form-get :tags)))
      (should (member "keep" (org-air-inbox--form-get :tags))))))

(ert-deftest org-air-r20-4-move-target-rehomes-in-file-and-path-readers ()
  "R64-3 re-bless (was `…-decode-move-routes-to-read-move-target' —
renamed with the retired router): `org-air-inbox--decode-target' is
GONE with the action menu; the `⌂ Move to file…' guarantees re-home in
the form's `f'/`p' infix readers (spec seam T8).  The `f' reader
resolves a `⌂' candidate to the REAL expanded target file — never the
item's own file, the original move bug — and the `p' reader completes
over that ONE file's real outline table: an existing heading resolves
with `:new' 0, typed-beyond input counts exactly the segments a refile
will CREATE, and NOTHING mutates at prompt time (creation is deferred
to execute — the R64-2 defer ruling)."
  (skip-unless (locate-library "org-air"))
  (org-air-r20--with-temp-org
      ((dir)
       (a "a.org" "* TODO H :x:\n")
       (b "b.org" "* Existing\n"))
    (let* ((org-air-files (list dir))
           (org-air-view-buffer-name "*org-air-r20-no-board*")
           (item (org-air-item-create
                  :title "H" :tags '("x") :file a
                  :marker (with-current-buffer (find-file-noselect a)
                            (goto-char (point-min)) (point-marker))))
           (b-cand (concat "⌂ " (file-name-nondirectory b)))
           (file nil))
      ;; `f': the real `⌂' candidate decodes to the REAL file.
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt &rest _)
                   (should (string-match-p "Move to file" prompt))
                   b-cand)))
        (setq file (org-air-inbox--read-target-file item)))
      (should (equal (file-truename file) (file-truename b)))
      (should-not (equal (file-truename file) (file-truename a)))
      ;; `p': the table is b's REAL outline table; an existing heading
      ;; resolves with `:new' 0.
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt coll &rest _)
                   (should (member "Existing" coll))
                   "Existing")))
        (should (equal (org-air-inbox--read-target-path file)
                       '(:olp ("Existing") :new 0))))
      ;; `p': typing beyond the tree means CREATE — counted, not built.
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "Existing/Under here")))
        (should (equal (org-air-inbox--read-target-path file)
                       '(:olp ("Existing" "Under here") :new 1))))
      ;; NOTHING mutated at prompt time — creation is deferred.
      (with-temp-buffer
        (insert-file-contents b)
        (should-not (string-match-p "Under here" (buffer-string)))))))

(ert-deftest org-air-r20-4-refile-applies-category-property ()
  "A `Category…' refile sets the moved heading's `:CATEGORY:' property and
merges the extra picks as tags (end-to-end through `org-air-refile-item')."
  (skip-unless (locate-library "org-air"))
  (org-air-r20--with-temp-org
      ((dir)
       (a "a.org" "* TODO Pay invoice :keep:\n  body\n"))
    (let* ((org-air-files (list dir))
           (item (org-air-item-create
                  :title "Pay invoice" :tags '("keep") :file a :group "inbox"
                  :marker (with-current-buffer (find-file-noselect a)
                            (goto-char (point-min))
                            (re-search-forward "^\\* TODO Pay invoice")
                            (goto-char (match-beginning 0)) (point-marker)))))
      (let ((inhibit-message t))
        (org-air-refile-item item a nil '("keep" "q3") nil "finance"))
      (with-temp-buffer
        (insert-file-contents a)
        (let ((text (buffer-string)))
          (should (string-match-p ":CATEGORY:" text))
          (should (string-match-p "finance" text))
          (should (string-match-p ":keep:" text))
          (should (string-match-p ":q3:" text)))))))

;;;; ---------------------------------------------------------------------
;;;; R20-6 — perf: compute-once partition + displayed-only meta-widths.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r20-6-partition-matches-per-section-path ()
  "The compute-once partition is behaviour-preserving: its visible set and
its per-bucket membership (and counts) are EQUAL to the old per-section
`--visible-items' / `--items-for-bucket' path — same items, same order."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (with-temp-buffer
      (org-air-view-mode)
      (let* ((items (org-air-query-items))
             (org-air-view--items items))
        (setq org-air-view--tag-filter nil)
        (org-air-view--classify-cache-ensure)
        (let* ((part (org-air-view--compute-partition items))
               (table (cddr part)))
          ;; visible set == a fresh scan (no memo)
          (let ((org-air-view--render-partition nil))
            (should (equal (cadr part) (org-air-view--visible-items items))))
          ;; per-bucket membership + counts == the fresh per-section path
          (dolist (descriptor org-air-view--sections)
            (let* ((bucket (car descriptor))
                   (fresh (let ((org-air-view--render-partition nil))
                            (org-air-view--items-for-bucket bucket items)))
                   (memo (gethash bucket table)))
              (should (equal memo fresh))
              (should (= (length memo) (length fresh))))))))))

(ert-deftest org-air-r20-6-meta-widths-cover-displayed-cells ()
  "`--compute-meta-widths' (displayed-only) covers every DISPLAYED tag/origin
cell (the alignment invariant) and is never WIDER than the all-items widths
would be (the deliberate displayed-only tightening)."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (with-temp-buffer
      (org-air-view-mode)
      (let* ((items (org-air-query-items))
             (org-air-show-origin t) ; R30-3: origin-coverage invariant -> origin ON
             (width 160))            ; wide -> the title-fit pass never shrinks
        (setq org-air-view--items items
              org-air-view--tag-filter nil)
        (org-air-view--classify-cache-ensure)
        (let ((org-air-view--render-partition
               (org-air-view--compute-partition items))
              (org-air-view--render-displayed
               (cons items (make-hash-table :test 'eq))))
          (org-air-view--compute-meta-widths items width)
          (let ((disp-tags 0) (disp-orig 0) (all-tags 0) (all-orig 0)
                (tagw (lambda (it)
                        (let* ((tags (org-air-item-tags it))
                                (n (length tags)))
                          (string-width
                           (org-air-view--item-tagstr
                            tags (min org-air-tags-inline-max n) n)))))
                (origw (lambda (it)
                         (string-width (org-air-view--item-origin-raw it)))))
            (dolist (descriptor org-air-view--sections)
              (let ((bucket (car descriptor)))
                (dolist (it (org-air-view--displayed-items-for-bucket bucket items))
                  (setq disp-tags (max disp-tags (funcall tagw it))
                        disp-orig (max disp-orig (funcall origw it))))
                (dolist (it (org-air-view--items-for-bucket bucket items))
                  (setq all-tags (max all-tags (funcall tagw it))
                        all-orig (max all-orig (funcall origw it))))))
            ;; covers the widest DISPLAYED cell (columns line up)
            (should (>= org-air-view--meta-tags-w disp-tags))
            (should (>= org-air-view--meta-origin-w
                        (min disp-orig org-air-origin-max-width)))
            ;; never wider than the all-items result (displayed-only tighten)
            (should (<= org-air-view--meta-tags-w all-tags))
            (should (<= org-air-view--meta-origin-w
                        (min all-orig org-air-origin-max-width)))))))))

(ert-deftest org-air-r20-6-warm-rerender-under-ceiling ()
  "BENCH (informational, OUT of the gate — set ORG_AIR_BENCH to run): a warm
re-render of a 2000-item board (R18 + R20-6 caches hot) stays under a
generous ceiling, so a future O(N) re-render regression trips here."
  (skip-unless (locate-library "org-air"))
  (skip-unless (getenv "ORG_AIR_BENCH"))
  (let ((dir (make-temp-file "org-air-r20-6-bench-" t)))
    (unwind-protect
        (progn
          (dotimes (f 10)
            (with-temp-file (expand-file-name (format "g-%d.org" f) dir)
              (insert (format "#+title: G %d\n\n" f))
              (dotimes (i 200)
                (let* ((n (+ i (* f 200)))
                       (todo (pcase (% n 4) (0 "TODO ") (1 "TODO ")
                               (2 "NEXT ") (_ "")))
                       (tags (pcase (% n 5) (0 "  :work:")
                               (1 "  :home:errand:") (2 "  :project:") (_ "")))
                       (day (1+ (% n 28))) (month (1+ (% n 12)))
                       (pl (pcase (% n 3)
                             (0 (format "SCHEDULED: <2026-%02d-%02d>\n" month day))
                             (1 (format "DEADLINE: <2026-%02d-%02d>\n" month day))
                             (_ ""))))
                  (insert (format "* %sHeading %04d%s\n" todo n tags) pl
                          (format "Body %d.\n" n))))))
          (let ((org-air-files (directory-files dir t "\\.org\\'"))
                (org-air-inbox-file (expand-file-name "g-0.org" dir))
                (org-air-view-width 120) (org-air-view-height 50))
            (let ((items (org-air-query-items)))
              (should (>= (length items) 2000))
              (with-temp-buffer
                (org-air-view-mode)
                (org-air-view--render items nil) ; cold: warms the caches
                (let ((start (current-time)))
                  (dotimes (_ 5) (org-air-view--render items nil))
                  (let ((per (/ (float-time (time-subtract (current-time) start))
                                5.0)))
                    (message "R20-6 bench: warm re-render %.4fs/render at %d items"
                             per (length items))
                    (should (< per 0.2))))))))
      (let ((kill-buffer-query-functions nil))
        (dolist (buf (buffer-list))
          (let ((fn (buffer-file-name buf)))
            (when (and fn (string-prefix-p dir fn))
              (with-current-buffer buf (set-buffer-modified-p nil))
              (kill-buffer buf)))))
      (delete-directory dir t))))

(provide 'org-air-round20-test)
;;; org-air-round20-test.el ends here
