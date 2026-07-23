;;; org-air-round76-test.el --- executing ERTs for round-76 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for round-76 (air/v0.1/org-air-round76-design.org):
;; the edit transient's `,' priority field stops being a one-way cycle
;; (`next = (1+ current)') and becomes a FULL-RANGE one-key picker —
;; the R68 chooser law applied to `,':
;;   - `org-air-inbox--read-priority-char' reads ONE
;;     `read-char-exclusive' over the WRITE TARGET's whole priority
;;     range (`org-air-inbox--target-priority-range' over
;;     `org-air-inbox--form-write-target' — the R67-4 law byte-kept),
;;     the prompt SHOWING the range, the SPC-clears option and the
;;     pre-filled current value;
;;   - `org-air-inbox--priority-normalize' is the PURE stub-free seam:
;;     in-range char (lowercase upcased per org's own rule) / SPC,`-'
;;     → `clear' / RET → `keep' / else nil (rejected honestly with
;;     org's own `user-error' wording — never clamped);
;;   - `:priority' becomes an honest TRI-STATE: nil untouched / CHAR
;;     set / ?\s CLEAR — org's OWN remove vocabulary, riding BOTH
;;     apply legs byte-unchanged (`(org-priority ?\s)' removes the
;;     cookie; ZERO apply-leg edits), armed STATE-AWARE (SPC on a
;;     cookie-less item stores nil — back to untouched — so the
;;     no-cookie `user-error' class is unreachable by construction);
;;   - field row + live preview render the tri-state (the "clear"
;;     label; a cleared pick previews with NO cookie — never "[# ]").
;;
;; All BATCH/headless through the r19/r64/r67 form idiom
;; (`--form-init' + `call-interactively' on the suffix — no transient
;; event loop); `read-char-exclusive' stubbed via `cl-letf' where a
;; press is simulated, the stub CAPTURING its PROMPT argument.  The
;; spec's ten seams map onto r76-1..r76-10:
;;
;;   r76-1  THE REPRO (revert-RED anchor) — form on the `[#C]' item:
;;          one stubbed press of ?A ⇒ `:priority' ?A (C→A in ONE
;;          press, straight UP — impossible pre-fix); again ?B ⇒ ?B;
;;          the reader IS consulted (the stub's prompt captured).
;;   r76-2  THE FULL DOMAIN, PURE — `--priority-normalize' over
;;          (?A . ?E): A..E → self, a..e → upcased, ?\s/?- → `clear',
;;          ?\r/?\n → `keep', ?F/?1/?q → nil.  No stub anywhere.
;;   r76-3  THE PROMPT DISCLOSES — on the C item the captured prompt
;;          contains "A-E", "SPC clears" and "RET keeps C"; on the
;;          bare item no "RET keeps" token (no stale current).
;;   r76-4  CLEAR END-TO-END — ?\s on the C item ⇒ `:priority' ?\s,
;;          field row "clear", preview cookie-less; execute in place
;;          ⇒ the SAVED bytes lose `[#C]' (heading otherwise intact)
;;          and the R73 ring recorded the edit.  ?- arms the same
;;          sentinel.  RED today: no clear exists at all.
;;   r76-5  CLEAR ON A COOKIE-LESS ITEM IS SAFE — ?B then ?\s on the
;;          bare item ⇒ `:priority' NIL (back to untouched), execute
;;          ⇒ the gentle "Nothing to change", bytes identical, NO
;;          error.  Revert-RED against blind ?\s storage (the apply
;;          leg user-errors "No priority cookie found in line").
;;   r76-6  KEEP — ?\r on the virgin form leaves `:priority' nil
;;          (untouched, not materialised; execute is the gentle
;;          no-op); after a ?B pick, ?\r keeps ?B.
;;   r76-7  OUT-OF-RANGE HONESTLY REJECTED — ?F (range A-E) signals
;;          `user-error' matching "must be between" naming A and E;
;;          `:priority' untouched; zero bytes anywhere; ?q (the
;;          reflex quit key) rejects rather than silently setting.
;;   r76-8  THE WRITE-TARGET RANGE GOVERNS (R67-4) — with `:file' set
;;          to narrow.org the prompt says "A-C", ?E is rejected, ?B
;;          lands; `:file' back to nil ⇒ "A-E" again.
;;   r76-9  APPLY ON RET, BOTH LEGS — in place ?A from C ⇒ saved
;;          bytes carry `[#A]'; refile leg ⇒ the MOVED heading in the
;;          TARGET's saved bytes carries `[#A]'; refile with the
;;          armed clear ⇒ the moved heading carries NO cookie (the
;;          engine's `(org-priority ?\s)' strips the carried `[#C]').
;;   r76-10 PREVIEW + FIELD TRI-STATE (WYSIWYG) — untouched ⇒ `[#C]'
;;          / "C"; picked ?A ⇒ `[#A]' / "A"; cleared ⇒ no cookie /
;;          "clear"; the broken "[# ]" NEVER renders.  Revert-RED
;;          against a preview that formats ?\s raw.
;;
;; GUI residue (screenshot-confirm, not ERT-able): the prompt's
;; readability in the echo area while the transient window is
;; showing, and the field-row repaint after a pick/clear.  Goldens:
;; ZERO shifts — the transient renders in no mockup/golden.
;; Known-failures manifest stays EMPTY.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'transient)
(require 'org-air-test-helpers)

(when (locate-library "org-air")
  (require 'org-air))

;;;; -------------------------------------------------------------------
;;;; Fixture: own (A-E, [#C]) / bare (no cookie) / narrow (A-C target)
;;;; -------------------------------------------------------------------

(defvar org-air-r76--dir nil
  "The temp corpus directory of the current `org-air-r76--with-corpus'.")

(defconst org-air-r76--default-specs
  '(("own.org" . "#+PRIORITIES: A E C\n\n* TODO [#C] Widget :inbox:\n  body\n")
    ("bare.org" . "#+title: bare\n\n* TODO Jot :inbox:\n  body\n")
    ("narrow.org" . "#+PRIORITIES: A C B\n\n* Existing\n"))
  "The spec fixture: a mid-range `[#C]' item in a file whose own
`#+PRIORITIES: A E C' line widens the range (room BOTH ways — the
user's exact shape), a cookie-less item, and a refile target with a
NARROWER `#+PRIORITIES: A C B' range (the r76-8 R67-4 seam).")

(defmacro org-air-r76--with-corpus (specs &rest body)
  "Create a temp Org corpus from SPECS and run BODY against it.
SPECS is a list of (NAME . CONTENT) files (nil = the default spec
corpus).  Binds `org-air-files' to the directory, `org-air-inbox-file'
to its own.org, a temp `org-air-cache-file', a DEAD board buffer name,
fresh form/last/ring state and `org-tags-column' 0 (the r67
byte-stability shape).  Kills every corpus-visiting buffer and deletes
the directory afterwards."
  (declare (indent 1) (debug t))
  `(let* ((org-air-r76--dir (make-temp-file "org-air-r76-" t)))
     (unwind-protect
         (progn
           (pcase-dolist (`(,name . ,content)
                          (or ,specs org-air-r76--default-specs))
             (let ((file-name-handler-alist nil)
                   (coding-system-for-write 'utf-8-unix))
               (write-region (or content "") nil
                             (expand-file-name name org-air-r76--dir)
                             nil 'silent)))
           (let ((org-air-files (list org-air-r76--dir))
                 (org-air-inbox-file
                  (expand-file-name "own.org" org-air-r76--dir))
                 (org-air-cache-file
                  (expand-file-name ".cache/board.eld" org-air-r76--dir))
                 (org-air-view-buffer-name "*org-air-r76-no-board*")
                 (org-air-inbox--refile-form nil)
                 (org-air-inbox--refile-last nil)
                 (org-air-view--edit-ring nil)
                 (org-air-view--edit-redo-ring nil)
                 (org-tags-column 0)
                 (create-lockfiles nil)
                 (inhibit-message t))
             ,@body))
       (when (fboundp 'org-air-query-teardown)
         (org-air-query-teardown))
       (let ((kill-buffer-query-functions nil))
         (dolist (buf (buffer-list))
           (let ((fn (buffer-file-name buf)))
             (when (and fn (string-prefix-p org-air-r76--dir fn))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r76--dir t))))

(defun org-air-r76--file (name)
  "Return the corpus file NAME's absolute path."
  (expand-file-name name org-air-r76--dir))

(defun org-air-r76--item (name text)
  "Build an editor item for the heading containing TEXT in corpus file NAME.
The `:priority' slot carries the heading's raw cookie CHAR (the
`org-air-inbox--item-priority-char' pass-through shape), or nil when
the heading has no cookie."
  (let ((file (org-air-r76--file name)))
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (re-search-forward (regexp-quote text))
       (org-back-to-heading t)
       (let ((line (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position))))
         (org-air-item-create
          :title (substring-no-properties (org-get-heading t t t t))
          :tags (org-get-tags nil t)
          :todo (org-get-todo-state)
          :priority (and (string-match "\\[#\\(.\\)\\]" line)
                         (aref (match-string 1 line) 0))
          :file file
          :marker (point-marker)))))))

(defun org-air-r76--text (name)
  "Return corpus file NAME's on-disk content as a string."
  (with-temp-buffer
    (insert-file-contents (org-air-r76--file name))
    (buffer-string)))

(defvar org-air-r76--prompt nil
  "The PROMPT the last stubbed `read-char-exclusive' received.")

(defun org-air-r76--press (key)
  "Drive the `,' suffix with a stubbed reader returning KEY.
The stub captures its PROMPT argument into `org-air-r76--prompt'
\(nil first — an uncalled reader leaves it nil, the r76-1 revert
tell)."
  (setq org-air-r76--prompt nil)
  (cl-letf (((symbol-function 'read-char-exclusive)
             (lambda (&optional prompt &rest _)
               (setq org-air-r76--prompt prompt)
               key)))
    (call-interactively 'org-air-refile-form-priority)))

(defun org-air-r76--field-row ()
  "Render the `,' suffix's field-row description string."
  (let ((proto (get 'org-air-refile-form-priority 'transient--suffix)))
    (funcall (oref proto description))))

(defun org-air-r76--execute (&optional msgs-cell)
  "Drive the execute suffix; push every `message' into MSGS-CELL's car."
  (cl-letf (((symbol-function 'message)
             (lambda (fmt &rest args)
               (when (and msgs-cell fmt)
                 (push (apply #'format fmt args) (car msgs-cell)))
               nil)))
    (call-interactively 'org-air-refile-form-execute)))

;;;; -------------------------------------------------------------------
;;;; r76-1 — THE REPRO: C→A in ONE press, straight up
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r76-1-full-range-one-press-up ()
  "The user's literal complaint killed: on the `[#C]' item ONE stubbed
press of ?A sets `:priority' ?A — straight UP, one keystroke; again
with ?B ⇒ ?B (any direction, any distance).  The reader IS consulted
\(the stub's prompt captured non-nil).  RED on revert: the one-way
cycle never calls the reader and one press from C yields ?D — C→A in
one press is IMPOSSIBLE pre-fix."
  (skip-unless (locate-library "org-air"))
  (org-air-r76--with-corpus nil
    (let ((item (org-air-r76--item "own.org" "Widget")))
      (org-air-inbox--form-init item)
      (org-air-r76--press ?A)
      (should org-air-r76--prompt)      ; the reader was consulted
      (should (equal (org-air-inbox--form-get :priority) ?A))
      (org-air-r76--press ?B)
      (should (equal (org-air-inbox--form-get :priority) ?B))
      ;; down still works too — a chooser, not an inverted cycle.
      (org-air-r76--press ?E)
      (should (equal (org-air-inbox--form-get :priority) ?E)))))

;;;; -------------------------------------------------------------------
;;;; r76-2 — the full domain, pure (no stub)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r76-2-normalize-pure-domain ()
  "`org-air-inbox--priority-normalize' over (?A . ?E): every in-range
letter maps to itself, lowercase upcased (org's own rule), SPC and
`-' → `clear', RET (CR and LF) → `keep', out-of-range → nil.  The
honest stub-free batch seam that picks ANY in-range priority."
  (skip-unless (locate-library "org-air"))
  (let ((range '(?A . ?E)))
    (dolist (c '(?A ?B ?C ?D ?E))
      (should (equal (org-air-inbox--priority-normalize c range) c)))
    (dolist (c '(?a ?b ?c ?d ?e))
      (should (equal (org-air-inbox--priority-normalize c range)
                     (upcase c))))
    (should (eq (org-air-inbox--priority-normalize ?\s range) 'clear))
    (should (eq (org-air-inbox--priority-normalize ?- range) 'clear))
    (should (eq (org-air-inbox--priority-normalize ?\r range) 'keep))
    (should (eq (org-air-inbox--priority-normalize ?\n range) 'keep))
    (dolist (c '(?F ?1 ?q))
      (should-not (org-air-inbox--priority-normalize c range)))))

;;;; -------------------------------------------------------------------
;;;; r76-3 — the prompt discloses range + pre-fill
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r76-3-prompt-discloses-range-and-current ()
  "On the C item the captured prompt contains \"A-E\" (own.org's own
`#+PRIORITIES' range), \"SPC clears\" and \"RET keeps C\"; on the
bare item there is NO \"RET keeps\" token (no stale current) while
the range and clear tokens still show."
  (skip-unless (locate-library "org-air"))
  (org-air-r76--with-corpus nil
    (org-air-inbox--form-init (org-air-r76--item "own.org" "Widget"))
    (org-air-r76--press ?\r)
    (should (string-match-p "A-E" org-air-r76--prompt))
    (should (string-match-p "SPC clears" org-air-r76--prompt))
    (should (string-match-p "RET keeps C" org-air-r76--prompt))
    ;; the bare item: no current value, so no RET token at all.
    (org-air-inbox--form-init (org-air-r76--item "bare.org" "Jot"))
    (org-air-r76--press ?\r)
    (should (string-match-p "SPC clears" org-air-r76--prompt))
    (should-not (string-match-p "RET keeps" org-air-r76--prompt))))

;;;; -------------------------------------------------------------------
;;;; r76-4 — clear end-to-end: sentinel, surfaces, saved bytes, ring
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r76-4-clear-end-to-end ()
  "?- and ?\\s both arm the ?\\s sentinel on the cookie'd item; the
field row renders \"clear\", the preview heading carries NO cookie;
execute (in place) saves bytes that lose `[#C]' with the heading
otherwise intact, and the R73 edit ring recorded the edit (`u'
covers it).  RED today: no clear exists at all — the cycle's nil
step writes nothing."
  (skip-unless (locate-library "org-air"))
  (org-air-r76--with-corpus nil
    ;; ?- arms the same sentinel…
    (org-air-inbox--form-init (org-air-r76--item "own.org" "Widget"))
    (org-air-r76--press ?-)
    (should (equal (org-air-inbox--form-get :priority) ?\s))
    ;; …and SPC is the canonical clear.
    (org-air-inbox--form-init (org-air-r76--item "own.org" "Widget"))
    (org-air-r76--press ?\s)
    (should (equal (org-air-inbox--form-get :priority) ?\s))
    ;; surfaces: the "clear" row label + a cookie-less preview.
    (should (equal (org-air-r76--field-row) "priority clear"))
    (should-not (string-match-p (regexp-quote "[#")
                                (org-air-inbox--form-preview)))
    ;; execute in place: the SAVED bytes lose the cookie, nothing else.
    (let ((old (org-air-r76--text "own.org")))
      (org-air-r76--execute)
      (let ((new (org-air-r76--text "own.org")))
        (should (string-match-p "^\\* TODO Widget :inbox:$" new))
        (should-not (string-match-p (regexp-quote "[#C]") new))
        (should (equal new
                       (replace-regexp-in-string
                        (regexp-quote "* TODO [#C] Widget")
                        "* TODO Widget" old t t)))))
    ;; the R73 ring recorded the in-place edit.
    (should (= 1 (length org-air-view--edit-ring)))
    (should (string-match-p "priority"
                            (plist-get (car org-air-view--edit-ring)
                                       :desc)))))

;;;; -------------------------------------------------------------------
;;;; r76-5 — clear on a cookie-less item is safe (state-aware arming)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r76-5-clear-on-cookieless-item-safe ()
  "On the bare item a ?B pick stores ?B; a following SPC stores NIL —
back to untouched, NOT the ?\\s sentinel (state-aware arming: there
is factually no cookie to remove).  Execute is the gentle \"Nothing
to change\" no-op, bytes identical, NO error.  Revert-RED against
blind ?\\s storage: the apply leg would `user-error' \"No priority
cookie found in line\" and roll back."
  (skip-unless (locate-library "org-air"))
  (org-air-r76--with-corpus nil
    (let ((old (org-air-r76--text "bare.org"))
          (msgs (list nil)))
      (org-air-inbox--form-init (org-air-r76--item "bare.org" "Jot"))
      (org-air-r76--press ?B)
      (should (equal (org-air-inbox--form-get :priority) ?B))
      (org-air-r76--press ?\s)
      (should (null (org-air-inbox--form-get :priority)))
      (org-air-r76--execute msgs)
      (should (seq-some (lambda (m) (string-match-p "Nothing to change" m))
                        (car msgs)))
      (should (equal (org-air-r76--text "bare.org") old)))))

;;;; -------------------------------------------------------------------
;;;; r76-6 — RET keeps
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r76-6-ret-keeps ()
  "RET on the virgin form leaves `:priority' nil — untouched, never
materialised (the R67 dirty-only law: execute is the gentle no-op);
after a ?B pick a RET press keeps ?B."
  (skip-unless (locate-library "org-air"))
  (org-air-r76--with-corpus nil
    (let ((old (org-air-r76--text "own.org"))
          (msgs (list nil)))
      (org-air-inbox--form-init (org-air-r76--item "own.org" "Widget"))
      (org-air-r76--press ?\r)
      (should (null (org-air-inbox--form-get :priority)))
      (org-air-r76--execute msgs)
      (should (seq-some (lambda (m) (string-match-p "Nothing to change" m))
                        (car msgs)))
      (should (equal (org-air-r76--text "own.org") old))
      ;; after a pick, RET keeps the pick.
      (org-air-inbox--form-init (org-air-r76--item "own.org" "Widget"))
      (org-air-r76--press ?B)
      (org-air-r76--press ?\r)
      (should (equal (org-air-inbox--form-get :priority) ?B)))))

;;;; -------------------------------------------------------------------
;;;; r76-7 — out-of-range honestly rejected
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r76-7-out-of-range-rejected ()
  "?F over own.org's A-E range signals `user-error' matching \"must
be between\" and naming A and E; `:priority' stays untouched; zero
bytes move anywhere.  ?q (the reflex quit key) likewise rejects
rather than silently setting — never clamped, never a hang."
  (skip-unless (locate-library "org-air"))
  (org-air-r76--with-corpus nil
    (let ((old (org-air-r76--text "own.org")))
      (org-air-inbox--form-init (org-air-r76--item "own.org" "Widget"))
      (let ((err (should-error (org-air-r76--press ?F)
                               :type 'user-error)))
        (should (string-match-p "must be between" (cadr err)))
        (should (string-match-p "A" (cadr err)))
        (should (string-match-p "E" (cadr err))))
      (should (null (org-air-inbox--form-get :priority)))
      (should-error (org-air-r76--press ?q) :type 'user-error)
      (should (null (org-air-inbox--form-get :priority)))
      (should (equal (org-air-r76--text "own.org") old))
      (should (equal (org-air-r76--text "narrow.org")
                     "#+PRIORITIES: A C B\n\n* Existing\n")))))

;;;; -------------------------------------------------------------------
;;;; r76-8 — the write-target range governs (R67-4)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r76-8-write-target-range-governs ()
  "With `:file' set to narrow.org the SAME press reads the
DESTINATION's `#+PRIORITIES: A C B' range: the prompt says \"A-C\",
?E is rejected, ?B lands; `:file' back to nil ⇒ the item's OWN A-E
range again (?E lands)."
  (skip-unless (locate-library "org-air"))
  (org-air-r76--with-corpus nil
    (org-air-inbox--form-init (org-air-r76--item "own.org" "Widget"))
    (org-air-inbox--form-put :file (org-air-r76--file "narrow.org"))
    (should-error (org-air-r76--press ?E) :type 'user-error)
    (should (string-match-p "A-C" org-air-r76--prompt))
    (should (null (org-air-inbox--form-get :priority)))
    (org-air-r76--press ?B)
    (should (equal (org-air-inbox--form-get :priority) ?B))
    ;; destination dropped: the item's own file governs again.
    (org-air-inbox--form-put :file nil)
    (org-air-r76--press ?E)
    (should (string-match-p "A-E" org-air-r76--prompt))
    (should (equal (org-air-inbox--form-get :priority) ?E))))

;;;; -------------------------------------------------------------------
;;;; r76-9 — apply on RET, both legs
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r76-9-apply-on-ret-both-legs ()
  "The pick applies through the UNTOUCHED legs: in place ?A from C ⇒
the saved source bytes carry `[#A]'; the refile leg ⇒ the MOVED
heading in the TARGET's saved bytes carries `[#A]'; the refile leg
with the armed clear ⇒ the moved heading carries NO cookie (the
engine's `(org-priority ?\\s)' strips the carried `[#C]' inside the
transaction)."
  (skip-unless (locate-library "org-air"))
  ;; leg 1: in place, straight up.
  (org-air-r76--with-corpus nil
    (org-air-inbox--form-init (org-air-r76--item "own.org" "Widget"))
    (org-air-r76--press ?A)
    (org-air-r76--execute)
    (should (string-match-p "^\\* TODO \\[#A\\] Widget :inbox:$"
                            (org-air-r76--text "own.org"))))
  ;; leg 2: refile with a pick — the moved heading carries [#A].
  (org-air-r76--with-corpus nil
    (org-air-inbox--form-init (org-air-r76--item "own.org" "Widget"))
    (org-air-inbox--form-put :file (org-air-r76--file "narrow.org"))
    (org-air-r76--press ?A)
    (org-air-r76--execute)
    (let ((target (org-air-r76--text "narrow.org")))
      (should (string-match-p "\\[#A\\] Widget" target))
      (should-not (string-match-p "Widget"
                                  (org-air-r76--text "own.org")))))
  ;; leg 3: refile with the armed clear — the carried [#C] stripped.
  (org-air-r76--with-corpus nil
    (org-air-inbox--form-init (org-air-r76--item "own.org" "Widget"))
    (org-air-inbox--form-put :file (org-air-r76--file "narrow.org"))
    (org-air-r76--press ?\s)
    (should (equal (org-air-inbox--form-get :priority) ?\s))
    (org-air-r76--execute)
    (let ((target (org-air-r76--text "narrow.org")))
      (should (string-match-p "Widget" target))
      (should-not (string-match-p (regexp-quote "[#") target))
      (should-not (string-match-p "Widget"
                                  (org-air-r76--text "own.org"))))))

;;;; -------------------------------------------------------------------
;;;; r76-10 — preview + field tri-state (WYSIWYG)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r76-10-preview-and-field-tri-state ()
  "The three states, three honest surfaces: untouched ⇒ the preview
shows the item's own `[#C]' and the row shows \"C\"; picked ?A ⇒
`[#A]' / \"A\"; cleared ⇒ no cookie / \"clear\" — and at no point
does the broken \"[# ]\" render.  Revert-RED for the sentinel arms
against a preview that formats ?\\s raw."
  (skip-unless (locate-library "org-air"))
  (org-air-r76--with-corpus nil
    (org-air-inbox--form-init (org-air-r76--item "own.org" "Widget"))
    ;; untouched: the item's own value on both surfaces.
    (should (equal (org-air-r76--field-row) "priority C"))
    (should (string-match-p (regexp-quote "[#C]")
                            (org-air-inbox--form-preview)))
    ;; picked: the pick on both surfaces.
    (org-air-r76--press ?A)
    (should (equal (org-air-r76--field-row) "priority A"))
    (should (string-match-p (regexp-quote "[#A]")
                            (org-air-inbox--form-preview)))
    (should-not (string-match-p (regexp-quote "[#C]")
                                (org-air-inbox--form-preview)))
    ;; cleared: no cookie, the "clear" label, never "[# ]".
    (org-air-r76--press ?\s)
    (should (equal (org-air-r76--field-row) "priority clear"))
    (should-not (string-match-p (regexp-quote "[#")
                                (org-air-inbox--form-preview)))
    (should-not (string-match-p (regexp-quote "[# ]")
                                (org-air-inbox--form-preview)))))

(provide 'org-air-round76-test)
;;; org-air-round76-test.el ends here
