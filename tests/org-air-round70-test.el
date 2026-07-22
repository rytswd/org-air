;;; org-air-round70-test.el --- executing ERTs for round-70 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for round-70 (air/v0.1/org-air-round70-design.org):
;; the item editor's board entry moves `r' → `e' (R70-1 — `r' DROPPED,
;; no alias, per Decision 1; help/legend derive the new key via
;; `where-is'; the transient heading reads `Edit "<title>"'), and the
;; edit transient gains a DATED LOGBOOK note written to the item's
;; SOURCE heading in place (R70-2 — `org-air-inbox--append-log-note';
;; org owns the formatting/placement via `org-add-log-setup' + a
;; SYNCHRONOUS pre-filled `org-store-log-note', NEVER the interactive
;; `*Org Note*' prompt — the R68 trap class).  All BATCH/headless
;; through the r19/r64/r67 form idiom (`--form-init' + `--form-put' +
;; `call-interactively' — no transient event loop); the note core runs
;; UNMOCKED through org's own machinery.  The spec's ten seams map onto
;; r70-1..r70-10; the test-seat audit adds two gap ERTs (r70-11/12).
;;
;; R71 FLIP (air/v0.1/org-air-round71-design.org): the note's DELIVERY
;; changed from the R70 S-RET immediate-write ACTION to a deferred form
;; FIELD on `n' — ONE RET confirms edit + note together in both legs,
;; `org-air-inbox--add-item-note' folded into
;; `org-air-inbox--apply-item-edits' (`:note' a first-class EDITS
;; field), and the engine gained a trailing additive NOTE parameter.
;; The action-shaped pins below (r70-4/6/7/8/9/10/11/12) were
;; re-pointed at the REAL executed path per the spec's flip table —
;; none weakened: same byte asserts, same discipline asserts, new
;; delivery.  r70-1/2/3/5 stand untouched — the rebind and the note
;; WRITER survive R71 byte-for-byte.
;;
;;   r70-1 THE REBIND — `e' resolves to `org-air-refile-item' in the
;;         board map and on a live board; `r' is NIL in the map and
;;         never reaches the command (dropped, no alias).  Companion
;;         route proof without an event loop: `call-interactively' on
;;         the `e' binding in batch signals the transient's OWN
;;         "interactive-only" guard — key → command → transient
;;         dispatch end-to-end.
;;   r70-2 DETERMINISTIC LEGEND KEY — `where-is-internal' over the
;;         board map returns exactly ONE binding, [?e] (RED against a
;;         kept-`r' alias; the R30-2/R50-1 derivation law).
;;   r70-3 HELP SHOWS e — the board `?' help buffer carries the derived
;;         `e  edit item (a destination refiles)' Triage row and never
;;         the old `r  refile' row.
;;   r70-4 TRANSIENT LAYOUT (R71 flip) — "n" is
;;         `org-air-refile-form-note' (`transient' t) in the METADATA
;;         group; "S-<return>" names NO suffix (retired, unbound); RET
;;         is still `org-air-refile-form-execute'; `q' still quits.
;;   r70-5 THE NOTE CORE, exact shape — `--append-log-note' at a
;;         heading yields `- Note taken on [ts] \\' + the indented
;;         text; NO `*Org Note*' buffer; `post-command-hook' clean;
;;         `org-log-setup' nil.
;;   r70-6 DRAWER HONOURED (R71 flip) — the core under
;;         `org-log-into-drawer' t lands inside `:LOGBOOK:'…`:END:';
;;         the APPLIER's `:note' leg on the `#+STARTUP: logdrawer'
;;         fixture drawers the SAVED bytes with the GLOBAL knob still
;;         nil — the file's own setting governs.
;;   r70-7 THE APPLIER's :note LEG (R71 flip — the wrapper folded) —
;;         in place (heading + rest byte-identical around the spliced
;;         note), saved to disk, no move, buffer unmodified post-save,
;;         triage-undo source recorded, and the cache-cold (FILE . POS)
;;         marker slot works (R26-8); returns \='(note).
;;   r70-8 NOT SUPPRESSED BY R68 (R71 flip) — the composed applier
;;         call under a let-bound `(org-inhibit-logging 'note)' AND
;;         invoked from within an `org-air-view--at-item-source' body
;;         lands the note IN FULL, exactly ONE note line; the macro's
;;         post-body flush runs against an ALREADY-CLEAN hook (spied)
;;         — the two mechanisms are disjoint by construction.
;;   r70-9 ATOMICITY (R71 flip) — a store stub that lands real junk
;;         bytes at the log marker and THEN signals under the applier's
;;         `:note' leg: the error propagates, the disk AND buffer bytes
;;         stay byte-identical (rollback + no save).
;;   r70-10 THE SUFFIX (R71 full flip: action → field) — `n' STORES
;;         the dirty `:note' with disk bytes IDENTICAL at read time,
;;         an EMPTY read CLEARS, then execute WITH a destination
;;         refiles AND lands the note at the TARGET (one RET).
;;   r70-11 (audit gap; R71 flip) NOTE DRAFT + IN-PLACE EDIT COMPOSE —
;;         `n' draft + a destination-less priority edit confirm on ONE
;;         RET: both land at the source, one note line, ONE
;;         `save-buffer', no move, `--refile-last' still nil.
;;   r70-12 (audit gap; R71 INVERSION) THE FIELD REPLACES — two `n'
;;         reads = ONE `:note' (the second text); execute writes
;;         exactly ONE dated note.
;;
;; GUI residue (screenshot-confirm, not ERT-able): the live `n'
;; keypress inside the real transient event loop; the rendered
;; `note     …' field row + preview segment; S-RET's undefined-key
;; feedback in an open editor.
;; Goldens: ZERO shifts — no fixture renders the transient, the help
;; buffer, or an Actions refile cell (audit: none exists).

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
;;;; Fixture: inbox / logdrawer / refile target (the spec corpus)
;;;; -------------------------------------------------------------------

(defvar org-air-r70--dir nil
  "The temp corpus directory of the current `org-air-r70--with-corpus'.")

(defconst org-air-r70--default-specs
  '(("inbox.org" . "#+title: inbox\n\n* TODO Capture me :inbox:\n  body\n")
    ("drawer.org" . "#+STARTUP: logdrawer\n\n* TODO Drawer thing\n  body\n")
    ("projects.org" . "#+title: projects\n\n* Existing\n"))
  "The spec fixture: an inbox capture, a `#+STARTUP: logdrawer' file
whose OWN setting must govern the note placement, and a titled refile
target.")

(defmacro org-air-r70--with-corpus (specs &rest body)
  "Create a temp Org corpus from SPECS and run BODY against it.
SPECS is a list of (NAME . CONTENT) files (nil = the default spec
corpus).  Binds `org-air-files' to the directory, `org-air-inbox-file'
to its inbox.org, a temp `org-air-cache-file', a DEAD board buffer
name, fresh form/last state and `org-tags-column' 0 (the r67
byte-stability shape, so the in-place asserts see ONLY the intended
splice).  Kills every corpus-visiting buffer, any leaked pre-filled
note buffer, and deletes the directory afterwards."
  (declare (indent 1) (debug t))
  `(let* ((org-air-r70--dir (make-temp-file "org-air-r70-" t)))
     (unwind-protect
         (progn
           (pcase-dolist (`(,name . ,content)
                          (or ,specs org-air-r70--default-specs))
             (let ((file-name-handler-alist nil)
                   (coding-system-for-write 'utf-8-unix))
               (write-region (or content "") nil
                             (expand-file-name name org-air-r70--dir)
                             nil 'silent)))
           (let ((org-air-files (list org-air-r70--dir))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-r70--dir))
                 (org-air-cache-file
                  (expand-file-name ".cache/board.eld" org-air-r70--dir))
                 (org-air-view-buffer-name "*org-air-r70-no-board*")
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
             (when (or (and fn (string-prefix-p org-air-r70--dir fn))
                       (string-prefix-p " *org-air-note*" (buffer-name buf)))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r70--dir t))))

(defmacro org-air-r70--with-board (&rest body)
  "Run BODY in a fresh `org-air-view-mode' board buffer (killed after)."
  (declare (indent 0) (debug t))
  `(let ((org-air-view-buffer-name "*org-air-r70*"))
     (unwind-protect
         (with-current-buffer (get-buffer-create org-air-view-buffer-name)
           (unless (derived-mode-p 'org-air-view-mode) (org-air-view-mode))
           ,@body)
       (when (get-buffer "*org-air-r70*")
         (let ((kill-buffer-query-functions nil))
           (kill-buffer "*org-air-r70*"))))))

(defun org-air-r70--file (name)
  "Return the corpus file NAME's absolute path."
  (expand-file-name name org-air-r70--dir))

(defun org-air-r70--item (name text)
  "Build an editor item for the heading containing TEXT in corpus file NAME."
  (let ((file (org-air-r70--file name)))
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

(defun org-air-r70--text (name)
  "Return corpus file NAME's on-disk content as a string."
  (with-temp-buffer
    (insert-file-contents (org-air-r70--file name))
    (buffer-string)))

(defun org-air-r70--count (needle text)
  "Count the non-overlapping literal occurrences of NEEDLE in TEXT."
  (let ((n 0) (start 0) (re (regexp-quote needle)))
    (while (string-match re text start)
      (setq start (match-end 0))
      (cl-incf n))
    n))

(defconst org-air-r70--note-line-re
  "- Note taken on \\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [^]]*\\] \\\\\\\\\n"
  "Regexp for one dated `- Note taken on [ts] \\\\' log-note line.")

;;;; -------------------------------------------------------------------
;;;; r70-1 — the rebind: e opens the editor, r is gone
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r70-1-rebind-e-opens-editor-r-gone ()
  "`e' resolves to `org-air-refile-item' (map + live board) and `r' is
NIL in the board map — dropped, no alias (Decision 1).  Companion route
proof without an event loop: `call-interactively' on the `e' binding in
batch reaches the transient's OWN noninteractive guard
(\"interactive-only\") — key → command → transient dispatch end-to-end.
Revert-RED both ways."
  (skip-unless (locate-library "org-air"))
  (should (eq (lookup-key org-air-view-mode-map (kbd "e"))
              'org-air-refile-item))
  (should (null (lookup-key org-air-view-mode-map (kbd "r"))))
  (org-air-r70--with-corpus nil
    (org-air-r70--with-board
      (should (eq (key-binding (kbd "e")) 'org-air-refile-item))
      (should-not (eq (key-binding (kbd "r")) 'org-air-refile-item))
      ;; the route proof: e → `org-air-refile-item' → the transient's
      ;; noninteractive guard (proving the dispatch chain is live).
      (let ((err (should-error (call-interactively (key-binding (kbd "e")))
                               :type 'user-error)))
        (should (string-match-p "interactive-only" (cadr err)))))))

;;;; -------------------------------------------------------------------
;;;; r70-2 — no alias: the legend key derives deterministically
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r70-2-where-is-exactly-one-binding ()
  "`where-is-internal' over the board map returns exactly ONE binding
for `org-air-refile-item' — [?e].  A kept-`r' alias would make the
derived legend/help key an accident of keymap internals (the
R30-2/R50-1 determinism law); RED against it."
  (skip-unless (locate-library "org-air"))
  (should (equal (where-is-internal #'org-air-refile-item
                                    (list org-air-view-mode-map))
                 '([?e]))))

;;;; -------------------------------------------------------------------
;;;; r70-3 — help shows e (edit wording), never the old r row
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r70-3-help-shows-e-edit-row ()
  "The board `?' help buffer carries the derived Triage row `e  edit
item (a destination refiles)' and does NOT carry the old `r  refile'
row.  Revert-RED."
  (skip-unless (locate-library "org-air"))
  (save-window-excursion
    (unwind-protect
        (with-temp-buffer
          (org-air-view-mode)          ; a board-context origin buffer
          (org-air-help)
          (let ((help (get-buffer org-air-help-buffer-name)))
            (should help)
            (let ((text (with-current-buffer help
                          (substring-no-properties (buffer-string)))))
              (should (string-match-p
                       (concat "^  e +"
                               (regexp-quote
                                "edit item (a destination refiles)"))
                       text))
              (should-not (string-match-p "^  r +refile" text)))))
      (when (get-buffer org-air-help-buffer-name)
        (kill-buffer org-air-help-buffer-name)))))

;;;; -------------------------------------------------------------------
;;;; r70-4 — the transient layout: S-<return> add note, :transient t
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r70-4-transient-layout-s-ret-note ()
  "R71 flip of the R70 action-shape pin (spec flip table): \"n\" in
`org-air-refile-transient' is `org-air-refile-form-note' with
prototype slot `transient' t (the stays-open pin, batch-readable),
the LAST row of the METADATA group (keys t c s d k , n — the layout
walk); \"S-<return>\" names NO suffix (the R70 action row retired,
the key left UNBOUND — R71 Decision 1, no alias:
`transient-get-suffix' signals its own \"not found\"); \"RET\" is
still `org-air-refile-form-execute' and \"q\" still quits."
  (skip-unless (locate-library "org-air"))
  ;; S-RET: gone, nothing replaces it — the lookup itself signals.
  (let ((err (should-error (transient-get-suffix
                            'org-air-refile-transient '("S-<return>")))))
    (should (string-match-p "not found" (error-message-string err))))
  ;; n: the repurposed FIELD suffix, still stays-open…
  (let ((suffix (transient-get-suffix 'org-air-refile-transient '("n"))))
    (should suffix)
    (should (eq (plist-get (nth 2 suffix) :command)
                'org-air-refile-form-note)))
  (let ((proto (get 'org-air-refile-form-note 'transient--suffix)))
    (should proto)
    (should (eq (oref proto transient) t)))
  ;; …sitting LAST in the METADATA group (the layout walk).
  (let* ((layout (get 'org-air-refile-transient 'transient--layout))
         (meta (seq-find
                (lambda (col)
                  (equal (plist-get (aref col 2) :description) "Metadata"))
                (aref (nth 0 layout) 3))))
    (should meta)
    (should (equal (mapcar (lambda (s) (plist-get (nth 2 s) :key))
                           (aref meta 3))
                   '("t" "c" "s" "d" "k" "," "n"))))
  (let ((ret (transient-get-suffix 'org-air-refile-transient '("RET"))))
    (should (eq (plist-get (nth 2 ret) :command)
                'org-air-refile-form-execute)))
  (let ((q (transient-get-suffix 'org-air-refile-transient '("q"))))
    (should (eq (plist-get (nth 2 q) :command) 'transient-quit-one))))

;;;; -------------------------------------------------------------------
;;;; r70-5 — the note core: exact dated shape, no interactive residue
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r70-5-note-core-exact-shape ()
  "`org-air-inbox--append-log-note' at a heading yields the
org-idiomatic dated note — `- Note taken on [ts] \\\\' + the indented
text, placed between the heading and its body; NO `*Org Note*' buffer
exists, `post-command-hook' does not carry `org-add-log-note', and
`org-log-setup' is nil (the interactive prompt can never fire).  RED
today (function absent)."
  (skip-unless (locate-library "org-air"))
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Thing\n  body\n")
    (goto-char (point-min))
    (let ((org-log-into-drawer nil)
          (org-adapt-indentation nil))
      (org-air-inbox--append-log-note "hello world"))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match
               (concat "\\`\\* TODO Thing\n"
                       org-air-r70--note-line-re
                       "[ \t]+hello world\n"
                       "  body\n\\'")
               text)))
    (should-not (get-buffer "*Org Note*"))
    (should-not (memq 'org-add-log-note post-command-hook))
    (should (null org-log-setup))))

;;;; -------------------------------------------------------------------
;;;; r70-6 — the drawer decision is org's own, per file
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r70-6-drawer-honoured-per-file ()
  "The note core under `org-log-into-drawer' t sits inside `:LOGBOOK:'
… `:END:'; and the APPLIER's `:note' leg (R71 flip — the R70 wrapper
folded into `org-air-inbox--apply-item-edits') on the `#+STARTUP:
logdrawer' fixture item drawers the note in the SAVED bytes while the
GLOBAL knob is still nil — the file's own setting governs, nothing
global."
  (skip-unless (locate-library "org-air"))
  ;; the core, global knob leg.
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Thing\n")
    (goto-char (point-min))
    (let ((org-log-into-drawer t)
          (org-adapt-indentation nil))
      (org-air-inbox--append-log-note "drawered"))
    (should (string-match
             (concat ":LOGBOOK:\n"
                     "[ \t]*" org-air-r70--note-line-re
                     "[ \t]+drawered\n"
                     "[ \t]*:END:")
             (buffer-substring-no-properties (point-min) (point-max)))))
  ;; the applier's :note leg, file-own-setting (SAVED bytes).
  (org-air-r70--with-corpus nil
    (should (null (default-value 'org-log-into-drawer))) ; anti-tautology
    (let ((item (org-air-r70--item "drawer.org" "Drawer thing")))
      (should (equal '(note)
                     (org-air-inbox--apply-item-edits
                      item '(:note "file setting"))))
      (should (string-match
               (concat "\\* TODO Drawer thing\n"
                       "[ \t]*:LOGBOOK:\n"
                       "[ \t]*" org-air-r70--note-line-re
                       "[ \t]+file setting\n"
                       "[ \t]*:END:")
               (org-air-r70--text "drawer.org"))))))

;;;; -------------------------------------------------------------------
;;;; r70-7 — the wrapper: in place, saved, undo source, cache-cold
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r70-7-wrapper-in-place-saved ()
  "R71 flip — the wrapper folded: `org-air-inbox--apply-item-edits'
with a lone `:note' edit writes the note to DISK under the SOURCE
heading with everything around the splice byte-identical (the heading
line untouched, the body following) — in place, no move; returns
\\='(note); the visiting buffer is unmodified post-save;
`org-air-view--triage-source-buffer' records the buffer (the board's
`u' covers a note); and a cache-cold (FILE . POS) marker slot works
(the R26-8 leg).  The absorption is lossless — same byte contract as
the R70 wrapper."
  (skip-unless (locate-library "org-air"))
  (org-air-r70--with-corpus nil
    (let* ((file (org-air-r70--file "inbox.org"))
           (item (org-air-r70--item "inbox.org" "Capture me"))
           (before (org-air-r70--text "inbox.org")))
      (should (equal '(note)
                     (org-air-inbox--apply-item-edits
                      item '(:note "quick thought"))))
      (let ((after (org-air-r70--text "inbox.org")))
        ;; the splice is EXACTLY the note block; heading + rest verbatim.
        (should (string-match
                 (concat "\\`\\(#\\+title: inbox\n\n"
                         "\\* TODO Capture me :inbox:\n\\)"
                         org-air-r70--note-line-re
                         "[ \t]+quick thought\n"
                         "\\(  body\n\\)\\'")
                 after))
        (should (equal before (concat (match-string 1 after)
                                      (match-string 2 after))))
        ;; no move: still exactly one heading, in its own file.
        (should (= 1 (org-air-r70--count "Capture me" after))))
      (should-not (string-match-p "Capture me"
                                  (org-air-r70--text "projects.org")))
      ;; saved: the visiting buffer carries no unsaved residue.
      (should-not (buffer-modified-p (find-file-noselect file)))
      ;; triage-undo source recorded.
      (should (eq org-air-view--triage-source-buffer
                  (find-file-noselect file)))))
  ;; the cache-cold (FILE . POS) marker slot (R26-8).
  (org-air-r70--with-corpus nil
    (let* ((file (org-air-r70--file "inbox.org"))
           (text (org-air-r70--text "inbox.org"))
           (pos (progn (string-match "\\* TODO Capture me" text)
                       (1+ (match-beginning 0))))
           (item (org-air-item-create
                  :title "Capture me" :tags '("inbox") :todo "TODO"
                  :file file :marker (cons file pos))))
      (should (equal '(note)
                     (org-air-inbox--apply-item-edits
                      item '(:note "cold note"))))
      (let ((after (org-air-r70--text "inbox.org")))
        (should (string-match-p org-air-r70--note-line-re after))
        (should (string-match-p "cold note" after))))))

;;;; -------------------------------------------------------------------
;;;; r70-8 — an EXPLICIT note is never suppressed by the R68 discipline
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r70-8-not-suppressed-by-r68-discipline ()
  "The composed applier call (R71 flip — `:note' through
`org-air-inbox--apply-item-edits') under a let-bound
`(org-inhibit-logging 'note)' AND invoked from within an
`org-air-view--at-item-source' body: the note text lands IN FULL,
exactly ONE note line, and the macro's post-body flush runs against
an ALREADY-CLEAN hook (spied) so it no-ops — the R68 discipline
downgrades IMPLICIT records; an explicit note is applied.  The two
mechanisms are disjoint by construction.  RED against a note path
routed through the downgrade."
  (skip-unless (locate-library "org-air"))
  (org-air-r70--with-corpus nil
    (let* ((item (org-air-r70--item "inbox.org" "Capture me"))
           (flush-calls 0)
           (hook-dirty-at-flush nil))
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
            (org-air-inbox--apply-item-edits item '(:note "explicit note")))))
      ;; the macro's post-body flush ran, against a clean hook.
      (should (> flush-calls 0))
      (should-not hook-dirty-at-flush)
      (let ((after (org-air-r70--text "inbox.org")))
        ;; the note landed IN FULL, exactly once — never downgraded to a
        ;; bare timestamp record, never suppressed.
        (should (= 1 (org-air-r70--count "- Note taken on" after)))
        (should (string-match
                 (concat org-air-r70--note-line-re "[ \t]+explicit note\n")
                 after))))))

;;;; -------------------------------------------------------------------
;;;; r70-9 — atomicity: a failing store rolls back byte-exactly
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r70-9-atomic-rollback-on-store-failure ()
  "A stubbed `org-store-log-note' that inserts REAL junk bytes at the
log marker and then signals under the applier's `:note' leg (R71 flip
— the wrapper folded): the error propagates out of
`org-air-inbox--apply-item-edits', and both the DISK bytes and the
visiting buffer are byte-identical to before (one
`atomic-change-group' rollback + no save).  RED against
store-then-save-regardless."
  (skip-unless (locate-library "org-air"))
  (org-air-r70--with-corpus nil
    (let* ((file (org-air-r70--file "inbox.org"))
           (item (org-air-r70--item "inbox.org" "Capture me"))
           (before (org-air-r70--text "inbox.org")))
      (cl-letf (((symbol-function 'org-store-log-note)
                 (lambda (&rest _)
                   ;; land real bytes in the SOURCE buffer, then fail.
                   (with-current-buffer (marker-buffer org-log-note-marker)
                     (save-excursion
                       (goto-char org-log-note-marker)
                       (insert "JUNK LINE\n")))
                   (error "boom: store failed"))))
        ;; the error is the STORE's own signal — proving the real path
        ;; ran through the stub (a void-function / wrapped error would
        ;; not match, keeping this seam revert-RED).
        (let ((err (should-error
                    (org-air-inbox--apply-item-edits
                     item '(:note "doomed")))))
          (should (string-match-p "boom: store failed"
                                  (error-message-string err)))))
      ;; rollback: disk AND buffer byte-identical, junk gone, no save.
      (should (equal before (org-air-r70--text "inbox.org")))
      (with-current-buffer (find-file-noselect file)
        (should (equal before (buffer-substring-no-properties
                               (point-min) (point-max))))))))

;;;; -------------------------------------------------------------------
;;;; r70-10 — the suffix (R71 flip): n STORES; RET refiles + lands note
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r70-10-suffix-writes-and-form-survives ()
  "R71 full flip (action → field): `org-air-refile-form-note' driven
batch — a minibuffer read of \"hi\" STORES the dirty `:note' with the
DISK bytes IDENTICAL at read time (the R64-2 prompt-time no-mutation
contract now covers the note) and the form's collected fields
untouched; an EMPTY read CLEARS the field (still zero bytes moved);
then execute WITH a destination refiles AND lands the drafted note at
the TARGET — ONE RET confirms edit + note together."
  (skip-unless (locate-library "org-air"))
  (org-air-r70--with-corpus nil
    (let* ((projects (org-air-r70--file "projects.org"))
           (item (org-air-r70--item "inbox.org" "Capture me"))
           (before (org-air-r70--text "inbox.org")))
      (org-air-inbox--form-init item)
      (org-air-inbox--form-put :priority ?A)   ; a collected field to watch
      ;; n with text: the note is STORED — nothing written…
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "hi")))
        (call-interactively 'org-air-refile-form-note))
      (should (equal (org-air-inbox--form-get :note) "hi"))
      (should (equal before (org-air-r70--text "inbox.org")))
      ;; …and the form session survives, fields untouched.
      (should org-air-inbox--refile-form)
      (should (eq (org-air-inbox--form-get :item) item))
      (should (equal (org-air-inbox--form-get :priority) ?A))
      (should (null (org-air-inbox--form-get :file)))
      ;; an EMPTY read CLEARS the field — still zero bytes moved.
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))
        (call-interactively 'org-air-refile-form-note))
      (should (null (org-air-inbox--form-get :note)))
      (should (equal before (org-air-r70--text "inbox.org")))
      ;; re-draft, then ONE RET with a destination: the refile AND the
      ;; note land TOGETHER — at the TARGET.
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "hi")))
        (call-interactively 'org-air-refile-form-note))
      (org-air-inbox--form-put :file projects)
      (call-interactively 'org-air-refile-form-execute)
      (let ((target (org-air-r70--text "projects.org"))
            (source (org-air-r70--text "inbox.org")))
        (should (string-match-p "Capture me" target))
        (should (string-match
                 (concat org-air-r70--note-line-re "[ \t]+hi\n")
                 target))
        (should-not (string-match-p "Capture me" source))
        (should-not (string-match-p "- Note taken on" source))))))

;;;; -------------------------------------------------------------------
;;;; r70-11 — audit gap: note + IN-PLACE metadata edit, one session
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r70-11-note-then-in-place-edit-compose ()
  "R71 flip: the `n' note DRAFT + a destination-less priority edit
confirm TOGETHER on ONE RET — both in the SOURCE's saved bytes (the
R67 in-place leg extended under the note), exactly one note line, no
move, exactly ONE `save-buffer' (the composed apply is one
transaction), `org-air-inbox--refile-last' still nil."
  (skip-unless (locate-library "org-air"))
  (org-air-r70--with-corpus nil
    (let ((item (org-air-r70--item "inbox.org" "Capture me"))
          (saves 0))
      (org-air-inbox--form-init item)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "session note")))
        (call-interactively 'org-air-refile-form-note))
      (org-air-inbox--form-put :priority ?A)
      ;; nothing written at draft time — the note waits for RET.
      (should-not (string-match-p "- Note taken on"
                                  (org-air-r70--text "inbox.org")))
      (cl-letf* ((save-orig (symbol-function 'save-buffer))
                 ((symbol-function 'save-buffer)
                  (lambda (&rest args)
                    (cl-incf saves)
                    (apply save-orig args))))
        (call-interactively 'org-air-refile-form-execute))
      (should (= 1 saves))
      (let ((after (org-air-r70--text "inbox.org")))
        (should (string-match-p "^\\* TODO \\[#A\\] Capture me :inbox:$"
                                after))
        (should (= 1 (org-air-r70--count "- Note taken on" after)))
        (should (string-match-p "session note" after))
        (should (= 1 (org-air-r70--count "Capture me" after))))
      (should (null org-air-inbox--refile-last))
      (should (null org-air-inbox--refile-form)))))

;;;; -------------------------------------------------------------------
;;;; r70-12 — audit gap (R71 INVERSION): the field REPLACES
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r70-12-two-s-rets-two-notes ()
  "R71 INVERSION of the R70 journaling law (spec flip table): the
field REPLACES — two `n' reads in one session leave ONE pending
`:note' (the SECOND text), and execute writes exactly ONE dated note
carrying it.  Journaling is RET + `e' again."
  (skip-unless (locate-library "org-air"))
  (org-air-r70--with-corpus nil
    (let ((item (org-air-r70--item "inbox.org" "Capture me"))
          (texts (list "first thought" "second thought")))
      (org-air-inbox--form-init item)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (pop texts))))
        (call-interactively 'org-air-refile-form-note)
        (call-interactively 'org-air-refile-form-note))
      ;; ONE pending value — the second read REPLACED the first.
      (should (equal (org-air-inbox--form-get :note) "second thought"))
      (call-interactively 'org-air-refile-form-execute)
      (let ((after (org-air-r70--text "inbox.org")))
        (should (= 1 (org-air-r70--count "- Note taken on" after)))
        (should (string-match-p "second thought" after))
        (should-not (string-match-p "first thought" after)))
      (should (null org-air-inbox--refile-form)))))

(provide 'org-air-round70-test)
;;; org-air-round70-test.el ends here
