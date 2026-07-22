;;; org-air-round71-test.el --- executing ERTs for round-71 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for round-71 (air/v0.1/org-air-round71-design.org):
;; the comment becomes a form FIELD — `n' drafts the note (minibuffer
;; read PRE-FILLED with the pending value, empty input CLEARS, the
;; dirty `:note' previewed live via the shared first-line label
;; `org-air-inbox--form-note-label'), and ONE RET confirms edit + note
;; together in BOTH legs:
;;   - in-place: `:note' is a first-class EDITS field of
;;     `org-air-inbox--apply-item-edits', applied via
;;     `org-air-inbox--append-log-note' as the change group's LAST
;;     form, AFTER the R68 flush (Decision 4 — the probed
;;     globals-overwrite hazard: note-before-flush silently LOSES a
;;     pending downgraded record);
;;   - refile: `org-air-refile-item' gains a trailing additive NOTE
;;     parameter applied at the MOVED heading inside the engine's
;;     transaction, drawer per the WRITE TARGET's own
;;     `org-log-into-drawer' (the R67-4 law).
;; The R70 S-RET action retires (S-<return> UNBOUND in the transient,
;; Decision 1) and `org-air-inbox--add-item-note' folds into the
;; applier (Decision 3 — no delegator kept).
;;
;; All BATCH/headless through the r19/r64/r67 form idiom
;; (`--form-init' + `--form-put' + `call-interactively' — no transient
;; event loop); reads stubbed via `cl-letf'; the note writer runs
;; UNMOCKED through org's own machinery.  The spec's twelve seams map
;; onto r71-1..r71-12; the test-seat audit adds five gap ERTs:
;;
;;   r71-1  S-RET GONE, n PRESENT — "S-<return>" names NO suffix in
;;          `org-air-refile-transient' (retired, unbound — no alias);
;;          "n" is `org-air-refile-form-note' (`transient' t), the
;;          LAST row of the METADATA group (keys t c s d k , n); RET
;;          is still `org-air-refile-form-execute', `q' still quits.
;;   r71-2  THE READ STORES, NOTHING WRITES — a read of "hi" puts
;;          `:note' "hi" with the DISK bytes IDENTICAL (prompt-time
;;          no-mutation, R64-2, now covers the note); a second press
;;          receives "hi" as the reader's INITIAL argument (spied) and
;;          its text REPLACES the field; an empty read CLEARS (`:note'
;;          nil — the form never holds "").
;;   r71-3  THE RENDERING — with `:note' "hi" the suffix description
;;          renders `note     hi' and `--form-preview' carries
;;          `note: hi'; a multi-line / long note carries the first
;;          line + "…" in BOTH (the shared label, WYSIWYG); with
;;          `:note' nil the row shows `–' and the preview has NO
;;          `note:' segment.
;;   r71-4  NOTE-ONLY RET, IN PLACE — one dated note in the SOURCE's
;;          saved bytes, heading byte-identical splice, no move,
;;          exactly ONE note line, the completion message enumerates
;;          `note', the form cleared, `--refile-last' still nil.
;;   r71-5  NOTE + FIELDS, ONE SAVE, FLUSH-FIRST — `:note' +
;;          `:priority' + a reschedule under a let-bound
;;          `(org-log-reschedule 'note)' on a SCHEDULED fixture item:
;;          ONE RET lands the new stamp, the downgraded `- Rescheduled
;;          from' record AND the full note in the SAME saved bytes;
;;          `save-buffer' spied to exactly ONE call.  RED against
;;          note-before-flush (the record vanishes — probed).
;;   r71-6  ATOMICITY OF THE COMPOSED APPLY — a junk-then-signal
;;          `org-store-log-note' stub under a note+priority RET: the
;;          store's OWN error propagates, disk + buffer byte-identical
;;          (metadata rolls back WITH the note — one group), no save.
;;   r71-7  REFILE LEG CARRIES THE NOTE — `:file' + `:note', ONE RET:
;;          the item is AT the target with the dated note under the
;;          MOVED heading, the source clean (no residue, no note
;;          line), `--refile-last' recorded; the engine spied to ONE
;;          call receiving the note as its TENTH argument.
;;   r71-8  DRAWER PER WRITE TARGET — refile INTO a `#+STARTUP:
;;          logdrawer' target: the note inside `:LOGBOOK:'…`:END:' in
;;          the TARGET's saved bytes with the GLOBAL knob nil
;;          (anti-tautology); the form-driven in-place leg on
;;          drawer.org likewise.
;;   r71-9  R68 DISJOINTNESS HOLDS THROUGH RET — the composed
;;          note+todo RET under `(org-inhibit-logging 'note)' and from
;;          within an `org-air-view--at-item-source' body: the note IN
;;          FULL, exactly one note line, hook clean at every flush
;;          (spied), no logging residue after (`post-command-hook'
;;          clean, `org-log-setup' nil — the globals restored).
;;   r71-10 EMPTY = NO-OP NOTE — an untouched form is the gentle
;;          "Nothing to change" (zero bytes moved); `:note' nil +
;;          `:todo' applies the todo and writes NO note line (and the
;;          message never claims one).
;;   r71-11 THE ENGINE PARAMETER IS ADDITIVE — a direct NINE-argument
;;          `org-air-refile-item' call (the pre-R71 shape) refiles
;;          with NO note line; the TEN-argument call lands the note.
;;   r71-12 THE FOLD — `org-air-inbox--add-item-note' is GONE
;;          (`fboundp' nil, no delegator); companion: the applier's
;;          lone-`:note' call reproduces the r70-7 byte contract
;;          (the absorption is lossless).
;;
;;   AUDIT GAPS (the test seat's own, each revert-RED where a revert
;;   target exists):
;;   r71-13 TWO SESSIONS DON'T CROSS-CONTAMINATE — session 1 confirms
;;          a note; session 2 (`--form-init' re-seeds `:note' nil)
;;          confirms a todo-only edit: still exactly ONE note line,
;;          session 2's applied set excludes `note'.
;;   r71-14 MULTI-LINE DRAFT THROUGH ONE RET — a two-line read stores
;;          verbatim and the saved bytes carry the dated line + BOTH
;;          continuation lines indented (org's own formatting).
;;   r71-15 C-g AT THE PROMPT KEEPS THE PREVIOUS VALUE — a quitting
;;          reader leaves the pending draft untouched (the sibling
;;          fields' behaviour, Decision 2), zero bytes moved.
;;   r71-16 REFILE-LEG ATOMICITY WITH THE NOTE — the junk-then-signal
;;          store under a refile+note RET: the error propagates, the
;;          SOURCE is restored byte-identical, the TARGET's disk AND
;;          buffer are byte-identical (the engine's rollback covers
;;          the note — Decision 5's at-the-target rationale), nothing
;;          recorded for `l'.
;;   r71-17 R66 FRONTMATTER STILL FIRES WITH THE NOTE RIDING — refile
;;          to a brand-new file (+ note): synthesised `#+title:' /
;;          `#+state:' frontmatter AND the dated note under the moved
;;          heading in the same created file; the source clean.
;;   r71-18 FLUSH-FIRST IN THE ENGINE LEG TOO — Decision 4 pins the
;;          order in BOTH legs, but the spec's r71-5 only drives it
;;          in place: a refile whose schedule stamp queues a
;;          DOWNGRADED reschedule record (org-log-reschedule 'note)
;;          PLUS a note lands the new stamp, the record AND the full
;;          note in the TARGET's saved bytes — RED against
;;          note-before-flush inside the engine (the same probed
;;          globals-overwrite hazard, other door).
;;
;; GUI residue (screenshot-confirm, not ERT-able): the live `n'
;; keypress inside the real transient event loop; the rendered
;; `note     …' field row + preview segment; S-RET's undefined-key
;; feedback in an open editor.  Goldens: ZERO shifts — no fixture
;; renders the transient (re-confirmed R70/R71).  Known-failures
;; manifest stays EMPTY.

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
;;;; Fixture: inbox / logdrawer / scheduled / refile target
;;;; -------------------------------------------------------------------

(defvar org-air-r71--dir nil
  "The temp corpus directory of the current `org-air-r71--with-corpus'.")

(defconst org-air-r71--default-specs
  '(("inbox.org" . "#+title: inbox\n\n* TODO Capture me :inbox:\n  body\n")
    ("drawer.org" . "#+STARTUP: logdrawer\n\n* TODO Drawer thing\n  body\n")
    ("sched.org" . "#+title: sched\n\n* TODO Scheduled thing\nSCHEDULED: <2026-07-20 Mon>\n  body\n")
    ("projects.org" . "#+title: projects\n\n* Existing\n"))
  "The r70 spec corpus + a SCHEDULED item (the r71-5 flush-first leg).")

(defmacro org-air-r71--with-corpus (specs &rest body)
  "Create a temp Org corpus from SPECS and run BODY against it.
SPECS is a list of (NAME . CONTENT) files (nil = the default spec
corpus).  Binds `org-air-files' to the directory, `org-air-inbox-file'
to its inbox.org, a temp `org-air-cache-file', a DEAD board buffer
name, fresh form/last state and `org-tags-column' 0 (the r67
byte-stability shape).  Kills every corpus-visiting buffer, any leaked
pre-filled note buffer, and deletes the directory afterwards."
  (declare (indent 1) (debug t))
  `(let* ((org-air-r71--dir (make-temp-file "org-air-r71-" t)))
     (unwind-protect
         (progn
           (pcase-dolist (`(,name . ,content)
                          (or ,specs org-air-r71--default-specs))
             (let ((file-name-handler-alist nil)
                   (coding-system-for-write 'utf-8-unix))
               (write-region (or content "") nil
                             (expand-file-name name org-air-r71--dir)
                             nil 'silent)))
           (let ((org-air-files (list org-air-r71--dir))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-r71--dir))
                 (org-air-cache-file
                  (expand-file-name ".cache/board.eld" org-air-r71--dir))
                 (org-air-view-buffer-name "*org-air-r71-no-board*")
                 (org-air-inbox--refile-form nil)
                 (org-air-inbox--refile-last nil)
                 (org-tags-column 0)
                 (create-lockfiles nil)
                 (inhibit-message t))
             ,@body))
       (when (fboundp 'org-air-query-teardown)
         (org-air-query-teardown))
       (let ((kill-buffer-query-functions nil))
         (dolist (buf (buffer-list))
           (let ((fn (buffer-file-name buf)))
             (when (or (and fn (string-prefix-p org-air-r71--dir fn))
                       (string-prefix-p " *org-air-note*" (buffer-name buf)))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r71--dir t))))

(defun org-air-r71--file (name)
  "Return the corpus file NAME's absolute path."
  (expand-file-name name org-air-r71--dir))

(defun org-air-r71--item (name text)
  "Build an editor item for the heading containing TEXT in corpus file NAME."
  (let ((file (org-air-r71--file name)))
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (re-search-forward (regexp-quote text))
       (org-back-to-heading t)
       (org-air-item-create
        :title (substring-no-properties (org-get-heading t t t t))
        :tags (org-get-tags nil t)
        :todo (org-get-todo-state)
        :file file
        :marker (point-marker))))))

(defun org-air-r71--text (name)
  "Return corpus file NAME's on-disk content as a string."
  (with-temp-buffer
    (insert-file-contents (org-air-r71--file name))
    (buffer-string)))

(defun org-air-r71--count (needle text)
  "Count the non-overlapping literal occurrences of NEEDLE in TEXT."
  (let ((n 0) (start 0) (re (regexp-quote needle)))
    (while (string-match re text start)
      (setq start (match-end 0))
      (cl-incf n))
    n))

(defconst org-air-r71--note-line-re
  "- Note taken on \\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [^]]*\\] \\\\\\\\\n"
  "Regexp for one dated `- Note taken on [ts] \\\\' log-note line.")

(defun org-air-r71--draft (text)
  "Drive the `n' field suffix with a stubbed reader returning TEXT."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) text)))
    (call-interactively 'org-air-refile-form-note)))

(defun org-air-r71--no-suffix-p (key)
  "Non-nil when KEY names NO suffix in `org-air-refile-transient'.
Tolerant of both \"not found\" shapes — a nil return and transient's
own not-found signal — so the pin is about the CONTRACT (the key is
unbound), not the library's error style."
  (null (condition-case nil
            (transient-get-suffix 'org-air-refile-transient (list key))
          (error nil))))

;;;; -------------------------------------------------------------------
;;;; r71-1 — S-RET gone (unbound, no alias); n is the last Metadata row
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r71-1-s-ret-gone-n-present ()
  "\"S-<return>\" names NO suffix in `org-air-refile-transient' (the
R70 action row retired, the key left UNBOUND — Decision 1, no alias);
\"n\" is `org-air-refile-form-note' with prototype slot `transient' t
(the stays-open pin), sitting LAST in the METADATA group (keys
t c s d k , n — the layout walk); \"RET\" is still
`org-air-refile-form-execute' and \"q\" still quits.  RED against the
R70 tree (S-RET bound, no `n')."
  (skip-unless (locate-library "org-air"))
  ;; S-RET: retired, nothing replaces it.
  (should (org-air-r71--no-suffix-p "S-<return>"))
  ;; n: the repurposed FIELD suffix…
  (let ((suffix (transient-get-suffix 'org-air-refile-transient '("n"))))
    (should suffix)
    (should (eq (plist-get (nth 2 suffix) :command)
                'org-air-refile-form-note)))
  (let ((proto (get 'org-air-refile-form-note 'transient--suffix)))
    (should proto)
    (should (eq (oref proto transient) t)))
  ;; …the LAST row of the METADATA group.
  (let* ((layout (get 'org-air-refile-transient 'transient--layout))
         (meta (seq-find
                (lambda (col)
                  (equal (plist-get (aref col 2) :description) "Metadata"))
                (aref (nth 0 layout) 3))))
    (should meta)
    (should (equal (mapcar (lambda (s) (plist-get (nth 2 s) :key))
                           (aref meta 3))
                   '("t" "c" "s" "d" "k" "," "n"))))
  ;; RET / q unchanged.
  (let ((ret (transient-get-suffix 'org-air-refile-transient '("RET"))))
    (should (eq (plist-get (nth 2 ret) :command)
                'org-air-refile-form-execute)))
  (let ((q (transient-get-suffix 'org-air-refile-transient '("q"))))
    (should (eq (plist-get (nth 2 q) :command) 'transient-quit-one))))

;;;; -------------------------------------------------------------------
;;;; r71-2 — the read STORES (pre-fill, replace, empty clears); no write
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r71-2-read-stores-nothing-writes ()
  "A read of \"hi\" puts `:note' \"hi\" and the DISK bytes are
IDENTICAL (the R64-2 prompt-time no-mutation contract now covers the
note — RED against the R70 immediate write); a second press receives
\"hi\" as the reader's INITIAL argument (spied — re-press = re-edit)
and its \"hi again\" REPLACES the field; an empty read CLEARS
(`:note' nil — the form never holds \"\")."
  (skip-unless (locate-library "org-air"))
  (org-air-r71--with-corpus nil
    (let* ((item (org-air-r71--item "inbox.org" "Capture me"))
           (before (org-air-r71--text "inbox.org"))
           (initials nil)
           (replies (list "hi" "hi again" "")))
      (org-air-inbox--form-init item)
      (should (null (org-air-inbox--form-get :note))) ; seeded nil
      (cl-letf (((symbol-function 'read-string)
                 (lambda (_prompt &optional initial &rest _)
                   (push initial initials)
                   (pop replies))))
        ;; first read: stores, writes nothing.
        (call-interactively 'org-air-refile-form-note)
        (should (equal (org-air-inbox--form-get :note) "hi"))
        (should (equal before (org-air-r71--text "inbox.org")))
        ;; second read: PRE-FILLED with the pending value; REPLACES.
        (call-interactively 'org-air-refile-form-note)
        (should (equal (org-air-inbox--form-get :note) "hi again"))
        ;; third read: empty CLEARS.
        (call-interactively 'org-air-refile-form-note)
        (should (null (org-air-inbox--form-get :note))))
      ;; the spied INITIAL arguments: nil, then the pending drafts.
      (should (equal (nreverse initials) '(nil "hi" "hi again")))
      ;; still zero bytes moved after all three reads.
      (should (equal before (org-air-r71--text "inbox.org"))))))

;;;; -------------------------------------------------------------------
;;;; r71-3 — the rendering: field row + preview segment, shared label
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r71-3-field-row-and-preview-render ()
  "With `:note' \"hi\" the suffix description renders `note     hi'
and `--form-preview' contains `note: hi'; a multi-line note carries
the first line + \"…\" in BOTH surfaces; a long first line truncates
to width 24 with \"…\"; with `:note' nil the row shows `–' and the
preview carries NO `note:' segment.  RED against the R70 static
\"add note\" label / segment-less preview."
  (skip-unless (locate-library "org-air"))
  (org-air-r71--with-corpus nil
    (let* ((item (org-air-r71--item "inbox.org" "Capture me"))
           (proto (get 'org-air-refile-form-note 'transient--suffix))
           (desc (lambda () (funcall (oref proto description)))))
      (org-air-inbox--form-init item)
      ;; unset: the `–' placeholder, no preview segment.
      (should (equal (funcall desc) "note     –"))
      (should-not (string-match-p "note:" (org-air-inbox--form-preview)))
      ;; a short single-line note: verbatim on both surfaces.
      (org-air-inbox--form-put :note "hi")
      (should (equal (funcall desc) "note     hi"))
      (should (string-match-p "note: hi" (org-air-inbox--form-preview)))
      ;; multi-line: first line + the trailing-lines ellipsis, WYSIWYG.
      (org-air-inbox--form-put :note "hi\nsecond line")
      (should (equal (funcall desc) "note     hi…"))
      (should (string-match-p "note: hi…" (org-air-inbox--form-preview)))
      (should-not (string-match-p "second line"
                                  (org-air-inbox--form-preview)))
      ;; a long first line: truncated to width 24 with the ellipsis.
      (org-air-inbox--form-put
       :note "this is a very long first line indeed")
      (let ((label (org-air-inbox--form-note-label
                    "this is a very long first line indeed")))
        (should (= 24 (string-width label)))
        (should (string-suffix-p "…" label))
        (should (equal (funcall desc) (concat "note     " label)))
        (should (string-match-p (regexp-quote (concat "note: " label))
                                (org-air-inbox--form-preview)))))))

;;;; -------------------------------------------------------------------
;;;; r71-4 — note-only RET, in place: one dated note, one atomic save
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r71-4-note-only-ret-in-place ()
  "A note-only form is a REAL edit: form-init + an `n' draft, then
execute with NO destination ⇒ the dated note in the SOURCE's saved
bytes with the heading byte-identical around the splice, no move,
exactly ONE note line, the completion message enumerating `note', the
form cleared, `--refile-last' still nil.  RED against an execute leg
that ignores `:note' (the \"Nothing to change\" no-op)."
  (skip-unless (locate-library "org-air"))
  (org-air-r71--with-corpus nil
    (let ((item (org-air-r71--item "inbox.org" "Capture me"))
          (msgs nil))
      (org-air-inbox--form-init item)
      (org-air-r71--draft "solo note")
      ;; nothing written at draft time — the note waits for RET.
      (should-not (string-match-p "- Note taken on"
                                  (org-air-r71--text "inbox.org")))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when fmt (push (apply #'format fmt args) msgs))
                   nil)))
        (call-interactively 'org-air-refile-form-execute))
      (let ((after (org-air-r71--text "inbox.org")))
        (should (string-match
                 (concat "\\`#\\+title: inbox\n\n"
                         "\\* TODO Capture me :inbox:\n"
                         org-air-r71--note-line-re
                         "[ \t]+solo note\n"
                         "  body\n\\'")
                 after))
        (should (= 1 (org-air-r71--count "- Note taken on" after)))
        (should (= 1 (org-air-r71--count "Capture me" after))))
      ;; the message enumerates `note' like any applied field.
      (should (seq-some (lambda (m)
                          (string-match-p "Edited \"Capture me\" — note" m))
                        msgs))
      (should (null org-air-inbox--refile-form))
      (should (null org-air-inbox--refile-last)))))

;;;; -------------------------------------------------------------------
;;;; r71-5 — note + fields: ONE save, flush FIRST (the probed hazard)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r71-5-flush-first-one-save ()
  "`:note' + `:priority' + a reschedule under a let-bound
`(org-log-reschedule 'note)' on the SCHEDULED fixture item, ONE RET ⇒
the NEW stamp, the downgraded `- Rescheduled from' record AND the
full note ALL in the SAME saved bytes; `save-buffer' spied to exactly
ONE call.  RED against note-before-flush — the note's
`org-add-log-setup' overwrites the pending record's shared
`org-log-note-*' globals and its dequeue silently DROPS the
reschedule line (the probed R71 Decision-4 hazard) — and RED against
a second save."
  (skip-unless (locate-library "org-air"))
  (org-air-r71--with-corpus nil
    (let ((item (org-air-r71--item "sched.org" "Scheduled thing"))
          (saves 0))
      (org-air-inbox--form-init item)
      (org-air-r71--draft "why it moved")
      (org-air-inbox--form-put :priority ?A)
      (org-air-inbox--form-put :scheduled "2026-08-01")
      ;; nothing written at draft time — ONE RET is the one confirm.
      (should-not (string-match-p "- Note taken on"
                                  (org-air-r71--text "sched.org")))
      (cl-letf* ((save-orig (symbol-function 'save-buffer))
                 ((symbol-function 'save-buffer)
                  (lambda (&rest args)
                    (cl-incf saves)
                    (apply save-orig args))))
        (let ((org-log-reschedule 'note))   ; the R68 downgrade target
          (call-interactively 'org-air-refile-form-execute)))
      (should (= 1 saves))
      (let ((after (org-air-r71--text "sched.org")))
        ;; the new stamp…
        (should (string-match-p "SCHEDULED: <2026-08-01 Sat>" after))
        (should-not (string-match-p "SCHEDULED: <2026-07-20" after))
        ;; …the priority…
        (should (string-match-p
                 "^\\* TODO \\[#A\\] Scheduled thing$" after))
        ;; …the DOWNGRADED reschedule record (flush-first kept it)…
        (should (string-match-p
                 "- Rescheduled from \"\\[2026-07-20 Mon\\]\" on \\["
                 after))
        ;; …AND the full explicit note — all in the same saved bytes.
        (should (string-match
                 (concat org-air-r71--note-line-re "[ \t]+why it moved\n")
                 after))
        (should (= 1 (org-air-r71--count "- Note taken on" after)))))))

;;;; -------------------------------------------------------------------
;;;; r71-6 — atomicity of the composed apply: one group, one rollback
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r71-6-composed-apply-rolls-back-together ()
  "A stubbed `org-store-log-note' that inserts REAL junk bytes and
then signals, under a note+priority RET: the store's OWN error
propagates (a void-function / wrapped error would not match) and disk
+ buffer are byte-identical — the METADATA rolls back WITH the note
(one `atomic-change-group'), no save.  RED against a split
transaction (priority saved, note failed)."
  (skip-unless (locate-library "org-air"))
  (org-air-r71--with-corpus nil
    (let* ((file (org-air-r71--file "inbox.org"))
           (item (org-air-r71--item "inbox.org" "Capture me"))
           (before (org-air-r71--text "inbox.org")))
      (org-air-inbox--form-init item)
      (org-air-r71--draft "doomed")
      (org-air-inbox--form-put :priority ?A)
      (cl-letf (((symbol-function 'org-store-log-note)
                 (lambda (&rest _)
                   (with-current-buffer (marker-buffer org-log-note-marker)
                     (save-excursion
                       (goto-char org-log-note-marker)
                       (insert "JUNK LINE\n")))
                   (error "boom: store failed"))))
        (let ((err (should-error
                    (call-interactively 'org-air-refile-form-execute))))
          (should (string-match-p "boom: store failed"
                                  (error-message-string err)))))
      ;; rollback covers metadata + junk + note: disk AND buffer
      ;; byte-identical, the priority never half-applied.
      (should (equal before (org-air-r71--text "inbox.org")))
      (with-current-buffer (find-file-noselect file)
        (should (equal before (buffer-substring-no-properties
                               (point-min) (point-max))))))))

;;;; -------------------------------------------------------------------
;;;; r71-7 — the refile leg: ONE RET, note at the MOVED heading
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r71-7-refile-leg-carries-note ()
  "`:file' projects.org + a drafted `:note', ONE RET ⇒ the item is AT
the target with the dated note under the MOVED heading, the source
clean (no residue, no note line), `--refile-last' recorded; the
engine spied to exactly ONE call receiving the note as its TENTH
argument.  RED against a noteless engine call (no parameter)."
  (skip-unless (locate-library "org-air"))
  (org-air-r71--with-corpus nil
    (let* ((projects (org-air-r71--file "projects.org"))
           (item (org-air-r71--item "inbox.org" "Capture me"))
           (calls nil))
      (org-air-inbox--form-init item)
      (org-air-r71--draft "target note")
      (org-air-inbox--form-put :file projects)
      (cl-letf* ((engine-orig (symbol-function 'org-air-refile-item))
                 ((symbol-function 'org-air-refile-item)
                  (lambda (&rest args)
                    (push args calls)
                    (apply engine-orig args))))
        (call-interactively 'org-air-refile-form-execute))
      ;; ONE engine call; the note is the trailing TENTH argument.
      (should (= 1 (length calls)))
      (should (= 10 (length (car calls))))
      (should (equal "target note" (nth 9 (car calls))))
      (let ((target (org-air-r71--text "projects.org"))
            (source (org-air-r71--text "inbox.org")))
        ;; the note under the MOVED heading, in the TARGET's saved bytes.
        (should (string-match
                 (concat "^\\* TODO Capture me[^\n]*\n"
                         org-air-r71--note-line-re
                         "[ \t]+target note\n")
                 target))
        (should (= 1 (org-air-r71--count "- Note taken on" target)))
        ;; the source clean: no item, no residue, no note line.
        (should-not (string-match-p "Capture me" source))
        (should-not (string-match-p "- Note taken on" source))
        (should-not (string-match-p "target note" source)))
      ;; the destination recorded for `l' — never the note.
      (should (equal org-air-inbox--refile-last (cons projects nil)))
      (should (null org-air-inbox--refile-form)))))

;;;; -------------------------------------------------------------------
;;;; r71-8 — the drawer decision is the WRITE TARGET's own (R67-4)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r71-8-drawer-per-write-target ()
  "Refile INTO the `#+STARTUP: logdrawer' target ⇒ the note inside
`:LOGBOOK:'…`:END:' under the MOVED heading in the TARGET's saved
bytes, with the GLOBAL knob nil (anti-tautology — the write-target
file's OWN setting governs, the R67-4 law); the form-driven IN-PLACE
leg on drawer.org drawers likewise.  RED against source-side
placement (the source has no logdrawer) and against a global-knob
implementation."
  (skip-unless (locate-library "org-air"))
  ;; the refile leg: inbox (no drawer) → drawer.org (logdrawer).
  (org-air-r71--with-corpus nil
    (should (null (default-value 'org-log-into-drawer))) ; anti-tautology
    (let ((item (org-air-r71--item "inbox.org" "Capture me")))
      (org-air-inbox--form-init item)
      (org-air-r71--draft "drawered at target")
      (org-air-inbox--form-put :file (org-air-r71--file "drawer.org"))
      (call-interactively 'org-air-refile-form-execute)
      (let ((target (org-air-r71--text "drawer.org")))
        (should (string-match
                 (concat "^\\* TODO Capture me[^\n]*\n"
                         "[ \t]*:LOGBOOK:\n"
                         "[ \t]*" org-air-r71--note-line-re
                         "[ \t]+drawered at target\n"
                         "[ \t]*:END:")
                 target)))
      (should-not (string-match-p "- Note taken on"
                                  (org-air-r71--text "inbox.org")))))
  ;; the in-place leg, form-driven, on the logdrawer file itself.
  (org-air-r71--with-corpus nil
    (should (null (default-value 'org-log-into-drawer)))
    (let ((item (org-air-r71--item "drawer.org" "Drawer thing")))
      (org-air-inbox--form-init item)
      (org-air-r71--draft "drawered in place")
      (call-interactively 'org-air-refile-form-execute)
      (should (string-match
               (concat "\\* TODO Drawer thing\n"
                       "[ \t]*:LOGBOOK:\n"
                       "[ \t]*" org-air-r71--note-line-re
                       "[ \t]+drawered in place\n"
                       "[ \t]*:END:")
               (org-air-r71--text "drawer.org"))))))

;;;; -------------------------------------------------------------------
;;;; r71-9 — R68 disjointness holds through RET; globals restored
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r71-9-r68-disjointness-through-ret ()
  "The composed note+todo RET under a let-bound
`(org-inhibit-logging 'note)' AND from within an
`org-air-view--at-item-source' body: the note lands IN FULL (never
downgraded to a bare record, never suppressed), exactly ONE note
line, the hook CLEAN at every flush (spied — the explicit note leaves
nothing behind for the flush), and afterwards no logging residue:
`post-command-hook' does not carry `org-add-log-note' and
`org-log-setup' is nil — the globals restored, nothing deferred can
fire against the user's next command.  RED against a note routed
through the downgrade."
  (skip-unless (locate-library "org-air"))
  (org-air-r71--with-corpus nil
    (let* ((item (org-air-r71--item "inbox.org" "Capture me"))
           (flush-calls 0)
           (hook-dirty-at-flush nil))
      (org-air-inbox--form-init item)
      (org-air-r71--draft "explicit through RET")
      (org-air-inbox--form-put :todo "DONE")
      ;; nothing written at draft time — the note rides the ONE RET.
      (should-not (string-match-p "- Note taken on"
                                  (org-air-r71--text "inbox.org")))
      (cl-letf* ((flush-orig
                  (symbol-function 'org-air-inbox--flush-pending-log-note))
                 ((symbol-function 'org-air-inbox--flush-pending-log-note)
                  (lambda ()
                    (cl-incf flush-calls)
                    (when (memq 'org-add-log-note post-command-hook)
                      (setq hook-dirty-at-flush t))
                    (funcall flush-orig))))
        (let ((org-inhibit-logging 'note))
          (org-air-view--at-item-source item
            (call-interactively 'org-air-refile-form-execute))))
      ;; the applier's flush AND the macro's post-body flush both ran,
      ;; each against an already-clean hook (disjoint by construction).
      (should (>= flush-calls 2))
      (should-not hook-dirty-at-flush)
      ;; no logging residue survives the confirm.
      (should-not (memq 'org-add-log-note post-command-hook))
      (should (null org-log-setup))
      (let ((after (org-air-r71--text "inbox.org")))
        (should (string-match-p "^\\* DONE Capture me :inbox:$" after))
        (should (= 1 (org-air-r71--count "- Note taken on" after)))
        (should (string-match
                 (concat org-air-r71--note-line-re
                         "[ \t]+explicit through RET\n")
                 after))))))

;;;; -------------------------------------------------------------------
;;;; r71-10 — empty/absent :note = no note written
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r71-10-empty-note-is-no-op ()
  "Execute with `:note' nil and no other field ⇒ the gentle \"Nothing
to change\" message and ZERO bytes moved; `:note' nil + `:todo' ⇒ the
todo applies and NO note line appears (and the applied message never
claims one).  RED against an unconditional note write."
  (skip-unless (locate-library "org-air"))
  (org-air-r71--with-corpus nil
    (let ((item (org-air-r71--item "inbox.org" "Capture me"))
          (before (org-air-r71--text "inbox.org"))
          (msgs nil))
      ;; leg 1: truly untouched form — the no-op.
      (org-air-inbox--form-init item)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when fmt (push (apply #'format fmt args) msgs))
                   nil)))
        (call-interactively 'org-air-refile-form-execute))
      (should (seq-some (lambda (m) (string-match-p "Nothing to change" m))
                        msgs))
      (should (equal before (org-air-r71--text "inbox.org")))
      ;; leg 2: a todo edit with :note nil — no note line, ever.
      (setq msgs nil)
      (org-air-inbox--form-init item)
      (org-air-inbox--form-put :todo "DONE")
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when fmt (push (apply #'format fmt args) msgs))
                   nil)))
        (call-interactively 'org-air-refile-form-execute))
      (let ((after (org-air-r71--text "inbox.org")))
        (should (string-match-p "^\\* DONE Capture me :inbox:$" after))
        (should-not (string-match-p "- Note taken on" after)))
      (should (seq-some (lambda (m)
                          (and (string-match-p "Edited \"Capture me\"" m)
                               (not (string-match-p "note" m))))
                        msgs)))))

;;;; -------------------------------------------------------------------
;;;; r71-11 — the engine parameter is strictly trailing + additive
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r71-11-engine-note-param-additive ()
  "A direct NINE-argument `org-air-refile-item' call (the pre-R71
shape) refiles with NO note line — every pre-R71 caller passes
unchanged; the TEN-argument call lands the note at the moved heading.
The trailing-parameter compatibility law (the R67-3 DEADLINE
precedent), pinned."
  (skip-unless (locate-library "org-air"))
  ;; the pre-R71 nine-argument shape: no note, ever.
  (org-air-r71--with-corpus nil
    (let ((item (org-air-r71--item "inbox.org" "Capture me"))
          (projects (org-air-r71--file "projects.org")))
      (org-air-refile-item item projects nil :none
                           nil nil nil nil nil)
      (let ((target (org-air-r71--text "projects.org")))
        (should (string-match-p "^\\* TODO Capture me" target))
        (should-not (string-match-p "- Note taken on" target)))
      (should-not (string-match-p "Capture me"
                                  (org-air-r71--text "inbox.org")))))
  ;; the ten-argument shape: the trailing NOTE lands.
  (org-air-r71--with-corpus nil
    (let ((item (org-air-r71--item "inbox.org" "Capture me"))
          (projects (org-air-r71--file "projects.org")))
      (org-air-refile-item item projects nil :none
                           nil nil nil nil nil "tenth argument")
      (let ((target (org-air-r71--text "projects.org")))
        (should (string-match
                 (concat "^\\* TODO Capture me[^\n]*\n"
                         org-air-r71--note-line-re
                         "[ \t]+tenth argument\n")
                 target))
        (should (= 1 (org-air-r71--count "- Note taken on" target)))))))

;;;; -------------------------------------------------------------------
;;;; r71-12 — the fold: the wrapper retired, the absorption lossless
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r71-12-wrapper-folded-losslessly ()
  "`org-air-inbox--add-item-note' is GONE (`fboundp' nil — Decision 3,
no delegator kept; revert-RED against a kept symbol); companion: the
applier with a lone `:note' edit reproduces the r70-7 byte contract —
the splice is EXACTLY the note block, heading + rest verbatim,
\\='(note) returned — the absorption is lossless."
  (skip-unless (locate-library "org-air"))
  (should-not (fboundp 'org-air-inbox--add-item-note))
  (org-air-r71--with-corpus nil
    (let ((item (org-air-r71--item "inbox.org" "Capture me"))
          (before (org-air-r71--text "inbox.org")))
      (should (equal '(note)
                     (org-air-inbox--apply-item-edits item '(:note "x"))))
      (let ((after (org-air-r71--text "inbox.org")))
        (should (string-match
                 (concat "\\`\\(#\\+title: inbox\n\n"
                         "\\* TODO Capture me :inbox:\n\\)"
                         org-air-r71--note-line-re
                         "[ \t]+x\n"
                         "\\(  body\n\\)\\'")
                 after))
        (should (equal before (concat (match-string 1 after)
                                      (match-string 2 after))))))))

;;;; -------------------------------------------------------------------
;;;; r71-13 — audit gap: two sessions never cross-contaminate the note
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r71-13-sessions-do-not-cross-contaminate ()
  "Session 1 confirms a note; session 2 (`--form-init' re-seeds
`:note' nil) confirms a TODO-only edit ⇒ still exactly ONE note line
on disk (session 1's), and session 2's completion message never
enumerates `note' — the note is per-confirm payload, never sticky
state.  RED against a form state that survives execute or an init
that fails to seed `:note'."
  (skip-unless (locate-library "org-air"))
  (org-air-r71--with-corpus nil
    (let ((item (org-air-r71--item "inbox.org" "Capture me"))
          (msgs nil))
      ;; session 1: a note, confirmed — by RET, never by the draft.
      (org-air-inbox--form-init item)
      (org-air-r71--draft "session one")
      (should-not (string-match-p "- Note taken on"
                                  (org-air-r71--text "inbox.org")))
      (call-interactively 'org-air-refile-form-execute)
      (should (= 1 (org-air-r71--count "- Note taken on"
                                       (org-air-r71--text "inbox.org"))))
      ;; session 2: fresh init — the note field is re-seeded nil…
      (org-air-inbox--form-init item)
      (should (null (org-air-inbox--form-get :note)))
      (org-air-inbox--form-put :todo "DONE")
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when fmt (push (apply #'format fmt args) msgs))
                   nil)))
        (call-interactively 'org-air-refile-form-execute))
      ;; …so the second confirm writes NO second note.
      (let ((after (org-air-r71--text "inbox.org")))
        (should (string-match-p "^\\* DONE Capture me :inbox:$" after))
        (should (= 1 (org-air-r71--count "- Note taken on" after)))
        (should (= 1 (org-air-r71--count "session one" after))))
      (should (seq-some (lambda (m)
                          (and (string-match-p "Edited \"Capture me\"" m)
                               (not (string-match-p "note" m))))
                        msgs)))))

;;;; -------------------------------------------------------------------
;;;; r71-14 — audit gap: a multi-line comment through ONE RET
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r71-14-multi-line-note-through-ret ()
  "A two-line read (the yank / C-q C-j shape) stores VERBATIM in
`:note' and ONE RET writes org's own multi-line note — the dated
`- Note taken on [ts] \\\\' line + BOTH continuation lines indented,
exactly one note.  RED against first-line-only storage or a writer
that flattens the newline."
  (skip-unless (locate-library "org-air"))
  (org-air-r71--with-corpus nil
    (let ((item (org-air-r71--item "inbox.org" "Capture me")))
      (org-air-inbox--form-init item)
      (org-air-r71--draft "line one\nline two")
      (should (equal (org-air-inbox--form-get :note) "line one\nline two"))
      (call-interactively 'org-air-refile-form-execute)
      (let ((after (org-air-r71--text "inbox.org")))
        (should (string-match
                 (concat org-air-r71--note-line-re
                         "[ \t]+line one\n"
                         "[ \t]+line two\n")
                 after))
        (should (= 1 (org-air-r71--count "- Note taken on" after)))))))

;;;; -------------------------------------------------------------------
;;;; r71-15 — audit gap: C-g at the prompt keeps the previous draft
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r71-15-quit-at-prompt-keeps-draft ()
  "A quitting reader (\\`C-g' at the minibuffer) leaves the pending
draft UNTOUCHED — the sibling fields' behaviour (Decision 2): the
quit propagates, `:note' still holds the previous value, zero bytes
moved.  RED against a reader that clears or half-commits on quit."
  (skip-unless (locate-library "org-air"))
  (org-air-r71--with-corpus nil
    (let ((item (org-air-r71--item "inbox.org" "Capture me"))
          (before (org-air-r71--text "inbox.org")))
      (org-air-inbox--form-init item)
      (org-air-r71--draft "keep me")
      (should (equal (org-air-inbox--form-get :note) "keep me"))
      (let ((quitted nil))
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) (signal 'quit nil))))
          (condition-case nil
              (call-interactively 'org-air-refile-form-note)
            (quit (setq quitted t))))
        ;; the quit PROPAGATED (never swallowed into a half-commit)…
        (should quitted))
      (should (equal (org-air-inbox--form-get :note) "keep me"))
      (should (equal before (org-air-r71--text "inbox.org"))))))

;;;; -------------------------------------------------------------------
;;;; r71-16 — audit gap: refile-leg atomicity WITH the note
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r71-16-refile-leg-atomic-with-note ()
  "The junk-then-signal `org-store-log-note' stub under a REFILE+note
RET: the store's OWN error propagates, the SOURCE is restored
byte-identical on disk (the engine's unwind), the TARGET's disk AND
buffer are byte-identical (the atomic-change-group rolls the paste,
the metadata AND the note back together — Decision 5's at-the-target
rationale: a half-applied confirm never survives a failed edit), and
nothing is recorded for the `l' recall.  RED against a source-side
pre-cut note write (the note would survive the restore) or a split
transaction."
  (skip-unless (locate-library "org-air"))
  (org-air-r71--with-corpus nil
    (let* ((projects (org-air-r71--file "projects.org"))
           (item (org-air-r71--item "inbox.org" "Capture me"))
           (src-before (org-air-r71--text "inbox.org"))
           (tgt-before (org-air-r71--text "projects.org")))
      (org-air-inbox--form-init item)
      (org-air-r71--draft "never lands")
      (org-air-inbox--form-put :file projects)
      (cl-letf (((symbol-function 'org-store-log-note)
                 (lambda (&rest _)
                   (with-current-buffer (marker-buffer org-log-note-marker)
                     (save-excursion
                       (goto-char org-log-note-marker)
                       (insert "JUNK LINE\n")))
                   (error "boom: store failed"))))
        (let ((err (should-error
                    (call-interactively 'org-air-refile-form-execute))))
          (should (string-match-p "boom: store failed"
                                  (error-message-string err)))))
      ;; the source restored byte-identically (no note inside the
      ;; restored subtree — the write-at-target law's whole point).
      (should (equal src-before (org-air-r71--text "inbox.org")))
      ;; the target untouched: disk AND buffer.
      (should (equal tgt-before (org-air-r71--text "projects.org")))
      (with-current-buffer (find-file-noselect projects)
        (should (equal tgt-before (buffer-substring-no-properties
                                   (point-min) (point-max)))))
      ;; a failed refile is never recorded for `l'.
      (should (null org-air-inbox--refile-last)))))

;;;; -------------------------------------------------------------------
;;;; r71-17 — audit gap: R66 frontmatter still fires with the note
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r71-17-frontmatter-synthesis-with-note ()
  "Refile to a BRAND-NEW file with a drafted note (ONE RET,
`org-air-refile-synthesize-frontmatter' \\='always): the R66 step-0
synthesis still fires — the created file carries `#+title:' +
`#+state:' frontmatter ABOVE the moved heading — AND the dated note
lands under that heading in the same created file; the source is
clean.  RED against a note parameter that disturbs the engine's
step-0/transaction shape."
  (skip-unless (locate-library "org-air"))
  (org-air-r71--with-corpus nil
    (let* ((newfile (expand-file-name "sub/notes.org" org-air-r71--dir))
           (item (org-air-r71--item "inbox.org" "Capture me"))
           (org-air-refile-synthesize-frontmatter 'always))
      (org-air-inbox--form-init item)
      (org-air-r71--draft "born with frontmatter")
      (org-air-inbox--form-put :file newfile)
      ;; nothing written at draft time — the note rides the ONE RET.
      (should-not (string-match-p "- Note taken on"
                                  (org-air-r71--text "inbox.org")))
      (call-interactively 'org-air-refile-form-execute)
      (should (file-exists-p newfile))
      (let ((target (with-temp-buffer
                      (insert-file-contents newfile)
                      (buffer-string))))
        ;; frontmatter synthesised above the moved heading…
        (should (string-match-p "\\`#\\+title: " target))
        (should (string-match-p "^#\\+state: draft$" target))
        (should (string-match-p "^\\* TODO Capture me" target))
        ;; …and the note under the MOVED heading in the same file.
        (should (string-match
                 (concat "^\\* TODO Capture me[^\n]*\n"
                         org-air-r71--note-line-re
                         "[ \t]+born with frontmatter\n")
                 target))
        (should (= 1 (org-air-r71--count "- Note taken on" target))))
      (should-not (string-match-p "Capture me"
                                  (org-air-r71--text "inbox.org"))))))

;;;; -------------------------------------------------------------------
;;;; r71-18 — audit gap: flush-first inside the ENGINE leg (Decision 4)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r71-18-engine-leg-flush-first ()
  "Decision 4 pins mutators → flush → note in BOTH legs; r71-5 drives
it in place only.  Refile the SCHEDULED fixture item with a NEW
schedule under a let-bound `(org-log-reschedule 'note)' (the R68
downgrade queues a `- Rescheduled from' record in the TARGET buffer)
AND a drafted note, ONE RET ⇒ the new stamp, the downgraded record
AND the full note ALL in the TARGET's saved bytes.  RED against
note-before-flush inside the engine — the note's `org-add-log-setup'
would clobber the pending record's shared globals and its dequeue
silently DROP the reschedule line (the probed hazard, refile door)."
  (skip-unless (locate-library "org-air"))
  (org-air-r71--with-corpus nil
    (let ((item (org-air-r71--item "sched.org" "Scheduled thing"))
          (projects (org-air-r71--file "projects.org")))
      (org-air-inbox--form-init item)
      (org-air-r71--draft "moved and noted")
      (org-air-inbox--form-put :scheduled "2026-08-01")
      (org-air-inbox--form-put :file projects)
      (let ((org-log-reschedule 'note))   ; the R68 downgrade target
        (call-interactively 'org-air-refile-form-execute))
      (let ((target (org-air-r71--text "projects.org")))
        ;; the moved heading with the NEW stamp…
        (should (string-match-p "^\\* TODO Scheduled thing" target))
        (should (string-match-p "SCHEDULED: <2026-08-01 Sat>" target))
        ;; …the DOWNGRADED reschedule record (flush-first kept it)…
        (should (string-match-p
                 "- Rescheduled from \"\\[2026-07-20 Mon\\]\" on \\["
                 target))
        ;; …AND the full explicit note, all in the same saved bytes.
        (should (string-match
                 (concat org-air-r71--note-line-re
                         "[ \t]+moved and noted\n")
                 target))
        (should (= 1 (org-air-r71--count "- Note taken on" target))))
      ;; the source clean — nothing half-applied on the way out.
      (should-not (string-match-p "Scheduled thing"
                                  (org-air-r71--text "sched.org")))
      (should-not (string-match-p "- Note taken on"
                                  (org-air-r71--text "sched.org"))))))

(provide 'org-air-round71-test)
;;; org-air-round71-test.el ends here
