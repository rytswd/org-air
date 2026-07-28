;;; org-air-round58-test.el --- executing ERTs for v0.5 round-58 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.5 round-58 (air/v0.5/org-air-round58-design.org):
;; first-class Emacs bookmark support for EVERY org-air view buffer — the
;; real activities.el failure ("this buffer's major mode does not support
;; the `bookmark' system").  Full records for board/project/revisit,
;; delegating records for the rail and the entry pane, a trivial record
;; for help; every handler autoloadable, no-display (the restorer owns
;; the windows), re-entering the EXISTING cache-first entry cores.
;;
;; All BATCH/headless, driven through the spec's named ERT seams T1-T11
;; via the REAL entry points (`bookmark-make-record' in the live buffer;
;; the handler funcalled on the produced — and prin1→read round-tripped —
;; record, exactly what `bookmark-handle-bookmark' / activities.el do):
;;
;;   r58-1-modes-wire-bookmark-record-fn (T1 wiring half) — every view
;;     mode (board/project/revisit/rail/entry/help) sets a BUFFER-LOCAL
;;     `bookmark-make-record-function' to its own org-air producer, while
;;     a file-visiting org buffer — plain, doc-session chrome on, and
;;     `org-air-outline-mode' on — is NEVER overridden (its default
;;     record still carries the correct `filename').  Reverting any
;;     mode's `setq-local' line FAILS.
;;   r58-2-board-record-shape (T1) — a scoped+filtered+sorted board with
;;     point on a known item row produces a record whose `handler' is
;;     `org-air-view-bookmark-jump', view kind `board', scope/filter/sort
;;     `equal' the buffer-locals, `org-air-item' = the row's DURABLE
;;     (FILE . POS) marker slot (never a raw buffer position), reserved
;;     keys `location'/`defaults' present, NO `filename'/`position', every
;;     key reserved-or-org-air-prefixed, and the whole alist prin1→read
;;     round-trips.  Reverting the wiring FAILS with today's exact
;;     breakage ("Buffer not visiting a file or directory").
;;   r58-3-board-roundtrip-restores (T2 + the persistence path) — the T1
;;     record survives prin1→read FIRST (the bookmark-default-file /
;;     activities.el shape), the board is KILLED, and the handler alone
;;     rebuilds `*org-air*' in `org-air-view-mode' with scope/filter/sort
;;     restored and point on the bookmarked row — which is NOT the first
;;     visible row, so the landing can never pass off the default
;;     first-item fallback.  Zero display calls (spied), windows
;;     untouched.  Reverting the handler or `--bookmark-apply' FAILS.
;;   r58-4-project-twin (T3) — root + sort + filename flip + doc-at-point
;;     recorded from a live project tree; kill; jump restores
;;     `org-air-project-mode', the root, the toggles and point on THAT
;;     doc's row (not the tree's first doc).  Revert FAILS.
;;   r58-5-revisit-twin (T4) — surface `orphans' + sort + created toggle
;;     + note-at-point recorded; `--pages' deliberately absent from the
;;     record and RESET to 1 on jump (the documented ruling); point on
;;     the bookmarked note row (not the surface's first).  Revert FAILS.
;;   r58-6-day-view (T5) — a day-board record carries `org-air-day' as a
;;     printable INTEGER epoch; jump decodes it back to the same day key
;;     with the day render active.  Revert FAILS.
;;   r58-7-degrade-never-signals (T6) — the handler on a bare header
;;     record AND on an `org-air-version' 999 record (with malformed +
;;     unknown fields) opens a PLAIN board, signals NOTHING and never
;;     even enters the degrade arm (message spy).  Revert FAILS.
;;   r58-8-no-display-contract (T7) — the window configuration compares
;;     equal across a handler jump inside the unchanged frame while
;;     `current-buffer' IS the restored view; pop-to-buffer /
;;     switch-to-buffer / display-buffer spied at ZERO calls.  A handler
;;     that pops FAILS here.
;;   r58-9-locator-drift (T8) — (a) the recorded file grows a preamble so
;;     the exact (FILE . POS) can no longer match: point lands via the
;;     FILE+title fallback (asserted: the landed row's marker differs
;;     from the recorded locator); (b) the recorded file is DELETED:
;;     point falls to the first item row, the one-shot slot clears, no
;;     signal.  Reverting the fallback chain FAILS.
;;   r58-10-mid-refresh-record (T9) — with `--refresh-state' forced to
;;     `refreshing' and the stale/queue lists populated,
;;     `bookmark-make-record' still returns a valid, fully printable
;;     record (prin1→read `equal'; no in-flight state leaks — every key
;;     reserved-or-org-air-prefixed).  Revert FAILS.
;;   r58-11-rail-delegation (T10) — the rail record delegates: view
;;     `rail', `org-air-host' `board', the HOST's scope/sort embedded,
;;     the host-derived " · rail" name.  Both buffers killed, the rail
;;     record ALONE rebuilds host + rail (rail current, rendered,
;;     back-pointer set; host undisplayed with the embedded scope).  The
;;     BOARD record jumped afterwards finds the live buffer through the
;;     R26-5 idempotent guard — a probe buffer-local survives, so
;;     `kill-all-local-variables' never ran (no double init).  Revert
;;     FAILS.
;;   r58-12-entry-pane-delegation — the snapshot pane's record carries
;;     `org-air-entry-ctx' = the snapshot's (FILE . POS) source + host;
;;     kill pane + board; the handler rebuilds BOTH (pane current with
;;     the entry text, board ensured undisplayed); a VANISHED source
;;     renders the pane's missing-source hint, never a signal.  Revert
;;     FAILS.
;;   r58-13-help-trivial-record — `*org-air-help*' records its context
;;     symbol; jump re-renders help for it (so no help buffer in a saved
;;     layout can ever raise the activities error).  Revert FAILS.
;;   r58-14-naming-and-handler-metadata (T11 + handler metadata) — the
;;     `defaults' HEAD reads "org-air: day …" on a day record and
;;     "org-air: board · file …" under a file scope (day wins over
;;     scope); `location' present; every handler is `fboundp', carries
;;     `(bookmark-handler-type . "org-air")' AND the literal
;;     `;;;###autoload' cookie on its defun in the owning source file
;;     (the fresh-Emacs activities restore contract).  Revert FAILS.
;;
;; REVERT-FAIL verified against the pre-impl trunk (krtyvmtm's parent) in
;; a scratch workspace: every ERT above goes RED there — the record
;; producers/handlers are void and `bookmark-make-record' signals the
;; exact reported "Buffer not visiting a file or directory".

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'bookmark)
(require 'org-air-test-helpers)
(require 'org-air-project-test)          ; the self-contained Air-project root

(when (locate-library "org-air")
  (require 'org-air))

;;;; -------------------------------------------------------------------
;;;; Corpus / record scaffolding
;;;; -------------------------------------------------------------------

(defvar org-air-r58--dir nil
  "The temp corpus directory of the current `org-air-r58--with-corpus'.")

(defun org-air-r58--reset-tables ()
  "Clear the GLOBAL query-layer tables (file-meta / visits / denote ids).
Session globals never cleared by a scan; every test starts and ends
empty so absolute temp paths from another test cannot leak in."
  (clrhash org-air-query--file-meta)
  (clrhash org-air-query--visits)
  (clrhash org-air-query--denote-id-index)
  (setq org-air-query--link-graph-dirty nil))

(defun org-air-r58--kill (&rest names)
  "Kill every live buffer in NAMES, no questions asked."
  (let ((kill-buffer-query-functions nil))
    (dolist (name names)
      (when (get-buffer name)
        (kill-buffer name)))))

(defmacro org-air-r58--with-corpus (specs &rest body)
  "Create a temp Org corpus from SPECS and run BODY against it.
SPECS is a list of (NAME . CONTENT) files written into a fresh temp
directory.  Binds `org-air-files' to the directory, `org-air-inbox-file'
to its inbox.org, a temp `org-air-cache-file', the 120x50 batch viewport
and a nil `bookmark-alist'; wraps BODY in `save-window-excursion'.
Starts from EMPTY query tables and cleans up the tables, every org-air
view buffer, every corpus-visiting buffer and the directory."
  (declare (indent 1) (debug t))
  `(let* ((org-air-r58--dir (make-temp-file "org-air-r58-" t)))
     (unwind-protect
         (progn
           (org-air-r58--reset-tables)
           (pcase-dolist (`(,name . ,content) ,specs)
             (let ((path (expand-file-name name org-air-r58--dir))
                   (coding-system-for-write 'utf-8-unix))
               (make-directory (file-name-directory path) t)
               (write-region (or content "") nil path nil 'silent)))
           (let ((org-air-files (list org-air-r58--dir))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-r58--dir))
                 (org-air-cache-file
                  (expand-file-name ".cache/board.eld" org-air-r58--dir))
                 (org-air-view-width 120)
                 (org-air-view-height 50)
                 (bookmark-alist nil))
             (save-window-excursion
               ,@body)))
       (org-air-query-teardown)
       (org-air-r58--reset-tables)
       (org-air-r58--kill org-air-view-buffer-name org-air-rail-buffer-name
                          org-air-view-pane-buffer-name
                          org-air-help-buffer-name "*org-air-project*"
                          org-air-revisit-buffer-name)
       (let ((kill-buffer-query-functions nil))
         (dolist (buf (buffer-list))
           (let ((fn (buffer-file-name buf)))
             (when (and fn (string-prefix-p org-air-r58--dir fn))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r58--dir t))))

(defun org-air-r58--file (name)
  "Return the absolute path of corpus file NAME."
  (expand-file-name name org-air-r58--dir))

(defconst org-air-r58--board-specs
  '(("inbox.org" . "* TODO Inbox capture\n")
    ("alpha.org" . "* TODO Ship the grant report :work:\nGrant body line.\n[2026-01-05 Mon 09:00]\n* TODO Alpha follow-up :work:\nFollow-up body.\n[2026-01-05 Mon 09:00]\n")
    ("beta.org" . "* TODO Beta errand\nBeta body.\n[2026-01-05 Mon 09:00]\n"))
  "Board corpus: dateless, QUIET TODO tasks (Attention rows) across three files.
R93: each task carries an inactive stamp in its own body, months before
the frozen now.  Needs attention is an aging rule now -- a heading
written this instant is fresh and renders NO row -- and every bookmark
record/jump law below needs rows on the board to record and land on.")

(defconst org-air-r58--revisit-specs
  '(("inbox.org" . "* TODO Inbox capture\n")
    ("evergreen.org" . "#+title: Evergreen spaced repetition\n\nEvergreen prose body.\n")
    ("zettel.org" . "#+title: Zettel habits\n\nZettel prose body.\n")
    ("chores.org" . "* TODO A task file\nTask body.\n"))
  "Revisit corpus: two knowledge notes; the task file + inbox stay off.")

(defun org-air-r58--alist (record)
  "Return RECORD's alist half (`bookmark-make-record' may prepend NAME)."
  (if (stringp (car-safe record)) (cdr record) record))

(defun org-air-r58--field (record key)
  "Return KEY's value in RECORD's alist, or nil."
  (cdr (assq key (org-air-r58--alist record))))

(defun org-air-r58--roundtrip (record)
  "Assert RECORD prin1→read round-trips `equal'; return the re-read copy.
The rule-5 printability fence: this is exactly what surviving
`bookmark-default-file' (and the activities.el/desktop persistence
path) requires — no marker, buffer, window or Emacs time object can
hide in a record that passes."
  (let* ((printed (prin1-to-string record))
         (reread (car (read-from-string printed))))
    (should (equal reread record))
    reread))

(defconst org-air-r58--reserved-keys
  '(handler location defaults position filename annotation
    front-context-string rear-context-string)
  "bookmark.el's own record keys (spec rule 2).")

(defun org-air-r58--assert-clean-keys (record)
  "Every RECORD key is bookmark.el-reserved or `org-air-'-prefixed.
The collision-proofing rule — and the T9 fence that no in-flight
machine state ever leaks into the alist under a foreign key."
  (dolist (cell (org-air-r58--alist record))
    (should (consp cell))
    (let ((key (car cell)))
      (should (symbolp key))
      (should (or (memq key org-air-r58--reserved-keys)
                  (string-prefix-p "org-air-" (symbol-name key)))))))

(defun org-air-r58--window-fingerprint ()
  "Return the frame's window tree as plain comparable data.
One entry per live window (minibuffer included): the window object, the
buffer it shows and its edges, plus the selected window/frame.  This is
the honest \"the handler touched no windows\" comparison: raw
`window-configuration-equal-p' is point-sensitive through the selected
window, so the handler's own `set-buffer' (its CONTRACT) — or a
restore-time point motion in an undisplayed buffer — would flip it
without any window being touched.  A handler that pops/switches/splits
changes this fingerprint."
  (list (selected-frame)
        (selected-window)
        (mapcar (lambda (w)
                  (list w (window-buffer w) (window-edges w)))
                (window-list nil t))))

(defvar org-air-r58--display-calls 0
  "Calls to the window-display entry points inside the no-display spy.")

(defmacro org-air-r58--asserting-no-display (&rest body)
  "Run BODY spying every window-display entry point (spec rule 3).
Asserts ZERO calls to `pop-to-buffer' / `switch-to-buffer' /
`display-buffer' and an unchanged window configuration after BODY — the
activities.el layout-safety guarantee: a bookmark handler makes its
buffer CURRENT (`set-buffer') and never shows it, because
`bookmark--jump-via' hands `current-buffer' to the CALLER's
display-function and a popping handler would fight the restored layout."
  (declare (indent 0) (debug t))
  `(let ((org-air-r58--display-calls 0)
         (org-air-r58--wc-before (org-air-r58--window-fingerprint)))
     (cl-letf* ((org-air-r58--real-ptb (symbol-function 'pop-to-buffer))
                ((symbol-function 'pop-to-buffer)
                 (lambda (&rest args)
                   (cl-incf org-air-r58--display-calls)
                   (apply org-air-r58--real-ptb args)))
                (org-air-r58--real-stb (symbol-function 'switch-to-buffer))
                ((symbol-function 'switch-to-buffer)
                 (lambda (&rest args)
                   (cl-incf org-air-r58--display-calls)
                   (apply org-air-r58--real-stb args)))
                (org-air-r58--real-db (symbol-function 'display-buffer))
                ((symbol-function 'display-buffer)
                 (lambda (&rest args)
                   (cl-incf org-air-r58--display-calls)
                   (apply org-air-r58--real-db args))))
       ,@body)
     (should (= 0 org-air-r58--display-calls))
     (should (equal org-air-r58--wc-before
                    (org-air-r58--window-fingerprint)))))

(defun org-air-r58--goto-item-row (title)
  "Move point onto the board row whose item TITLE matches exactly; assert."
  (let ((pos (point-min)) target)
    (while (and (not target)
                (setq pos (text-property-not-all pos (point-max)
                                                 'org-air-item nil)))
      (if (equal (org-air-item-title (get-text-property pos 'org-air-item))
                 title)
          (setq target pos)
        (setq pos (next-single-property-change pos 'org-air-item
                                               nil (point-max)))))
    (should target)
    (goto-char target)
    (org-air-view--goto-row-title)))

(defun org-air-r58--first-item ()
  "Return the first rendered row's `org-air-item', asserting one exists."
  (let ((pos (text-property-not-all (point-min) (point-max)
                                    'org-air-item nil)))
    (should pos)
    (get-text-property pos 'org-air-item)))

(defun org-air-r58--goto-doc-row (leaf)
  "Move point onto the project doc row whose file ends in LEAF; assert.
Returns the doc's absolute FILE."
  (let ((pos (point-min)) target file)
    (while (and (not target)
                (setq pos (text-property-not-all pos (point-max)
                                                 'org-air-doc nil)))
      (let ((doc (get-text-property pos 'org-air-doc)))
        (if (string-suffix-p leaf (org-air-doc-file doc))
            (setq target pos
                  file (org-air-doc-file doc))
          (setq pos (next-single-property-change pos 'org-air-doc
                                                 nil (point-max))))))
    (should target)
    (goto-char target)
    (org-air-view--goto-row-title)
    file))

(defun org-air-r58--revisit-row-files ()
  "Return the rendered revisit rows' FILEs, in buffer order."
  (let ((pos (point-min)) files)
    (while (setq pos (text-property-not-all pos (point-max)
                                            'org-air-revisit nil))
      (push (car (get-text-property pos 'org-air-revisit)) files)
      (setq pos (next-single-property-change pos 'org-air-revisit
                                             nil (point-max))))
    (nreverse files)))

(defun org-air-r58--goto-revisit-row (file)
  "Move point onto FILE's rendered revisit note row; assert it exists."
  (let ((pos (point-min)) target)
    (while (and (not target)
                (setq pos (text-property-not-all pos (point-max)
                                                 'org-air-revisit nil)))
      (if (equal (car (get-text-property pos 'org-air-revisit)) file)
          (setq target pos)
        (setq pos (next-single-property-change pos 'org-air-revisit
                                               nil (point-max)))))
    (should target)
    (goto-char target)))

(defun org-air-r58--open-scoped-board ()
  "Open the corpus board and compose the scoped/filtered/sorted T1 view.
File scope on alpha.org, `#work' tag filter, title/descending sort.
Leaves point on the `Alpha follow-up' row — under this sort it is NOT
the first visible row (`Ship the grant report' is), so a point-restore
assertion can never pass off the default first-item landing.  Returns
alpha.org's absolute path."
  (org-air-view)
  (with-current-buffer org-air-view-buffer-name
    (let ((alpha (org-air-r58--file "alpha.org")))
      (setq-local org-air-view--scope (list :file alpha))
      (setq-local org-air-view--tag-filter '("#work"))
      (setq-local org-air-view--sort-key 'title)
      (setq-local org-air-view--sort-direction 'descending)
      (org-air-view--render-current)
      (org-air-r58--goto-item-row "Alpha follow-up")
      (should (equal (org-air-item-title (org-air-r58--first-item))
                     "Ship the grant report"))
      alpha)))

(defvar-local org-air-r58--board-probe nil
  "Test probe local: `kill-all-local-variables' (a mode re-init) kills it.
The T10 no-double-init seam — it survives a bookmark jump onto a LIVE
view iff the handler entered through the R26-5 idempotent guard.")

;;;; -------------------------------------------------------------------
;;;; r58-1 — every view mode wires the record producer; org files never
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r58-1-modes-wire-bookmark-record-fn ()
  "Each org-air view mode sets a buffer-local record producer (T1 wiring).
Board / project / revisit / rail / entry pane / help each wire their own
`bookmark-make-record-function'; a file-visiting org buffer is NEVER
overridden — plain, with the doc-session chrome on, and with
`org-air-outline-mode' on — because a file buffer's built-in default
record (FILE + position) is already correct (the design ruling).
Reverting any mode's `setq-local' line fails its conjunct here."
  (skip-unless (locate-library "org-air"))
  (pcase-dolist (`(,mode . ,producer)
                 '((org-air-view-mode . org-air-view--bookmark-make-record)
                   (org-air-project-mode . org-air-project--bookmark-make-record)
                   (org-air-revisit-mode . org-air-revisit--bookmark-make-record)
                   (org-air-rail-mode . org-air-rail--bookmark-make-record)
                   (org-air-entry-view-mode . org-air-view-pane--bookmark-make-record)
                   (org-air-help-mode . org-air-help--bookmark-make-record)))
    (with-temp-buffer
      (funcall mode)
      (ert-info ((format "mode=%s" mode))
        (should (derived-mode-p mode))
        (should (local-variable-p 'bookmark-make-record-function))
        (should (eq bookmark-make-record-function producer)))))
  ;; File-visiting org buffers: org-air must NOT own their records.
  (let* ((file (make-temp-file "org-air-r58-doc-" nil ".org"
                               "#+title: Doc\n* One heading\nBody.\n"))
         (buf (find-file-noselect file)))
    (unwind-protect
        (with-current-buffer buf
          (should (derived-mode-p 'org-mode))
          (cl-flet ((org-air-owned-p ()
                      (and (local-variable-p 'bookmark-make-record-function)
                           (symbolp bookmark-make-record-function)
                           (string-prefix-p
                            "org-air-"
                            (symbol-name bookmark-make-record-function)))))
            (should-not (org-air-owned-p))
            ;; The built-in default record is already correct: the FILE.
            (let ((record (bookmark-make-record)))
              (should (equal (file-truename
                              (bookmark-prop-get record 'filename))
                             (file-truename file))))
            ;; Doc-session chrome on a project doc: still no override.
            (org-air-doc-session-mode 1)
            (should-not (org-air-owned-p))
            (org-air-doc-session-mode -1)
            ;; The opt-in outline rail: still no override.
            (let ((org-air-outline-rail-placement 'inline))
              (org-air-outline-mode 1)
              (should org-air-outline-mode)
              (should-not (org-air-owned-p))
              (org-air-outline-mode -1))))
      (let ((kill-buffer-query-functions nil))
        (when (buffer-live-p buf)
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf)))
      (delete-file file))))

;;;; -------------------------------------------------------------------
;;;; r58-2 — T1: the board record's shape
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r58-2-board-record-shape ()
  "T1: `bookmark-make-record' in a live board yields the full view record.
Handler + view kind + scope/filter/sort `equal' the buffer-locals, the
point locator is the row's DURABLE (FILE . POS) marker slot (the R53
model — never a raw buffer position), bookmark.el's reserved keys are
honoured (`location'/`defaults' present; NO `filename', NO `position' —
views are not file-visiting and point lives in the locator), every key
is reserved-or-org-air-prefixed and the alist prin1→read round-trips.
Reverting the mode's `bookmark-make-record-function' line fails with
today's exact breakage (`bookmark-make-record-default' signals \"Buffer
not visiting a file or directory\")."
  (skip-unless (locate-library "org-air"))
  (org-air-r58--with-corpus org-air-r58--board-specs
    (let ((alpha (org-air-r58--open-scoped-board)))
      (with-current-buffer org-air-view-buffer-name
        (let* ((row-marker (org-air-view--row-property 'org-air-marker))
               (record (bookmark-make-record))
               (alist (org-air-r58--alist record)))
          (should (eq (org-air-r58--field record 'handler)
                      'org-air-view-bookmark-jump))
          (should (eq (org-air-r58--field record 'org-air-view) 'board))
          (should (equal (org-air-r58--field record 'org-air-scope)
                         (list :file alpha)))
          (should (equal (org-air-r58--field record 'org-air-filter)
                         '("#work")))
          (should (equal (org-air-r58--field record 'org-air-sort)
                         '(title . descending)))
          ;; The durable (FILE . POS) locator — the row's own marker slot.
          (let ((loc (org-air-r58--field record 'org-air-item)))
            (should (consp loc))
            (should (equal loc row-marker))
            (should (equal (car loc) alpha))
            (should (integerp (cdr loc))))
          (should (equal (org-air-r58--field record 'org-air-item-title)
                         "Alpha follow-up"))
          ;; Reserved-key discipline (spec rule 2).
          (should (stringp (org-air-r58--field record 'location)))
          (should (consp (org-air-r58--field record 'defaults)))
          (should-not (assq 'filename alist))
          (should-not (assq 'position alist))
          (org-air-r58--assert-clean-keys record)
          (org-air-r58--roundtrip record))))))

;;;; -------------------------------------------------------------------
;;;; r58-3 — T2: jump on a killed board (through the persistence path)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r58-3-board-roundtrip-restores ()
  "T2 + the persistence path: prin1→read→handler rebuilds the killed board.
The record crosses the exact `bookmark-default-file' / activities.el
serialisation boundary BEFORE the jump; the handler alone (no org-air
buffer alive) reconstructs `*org-air*' in `org-air-view-mode' via the
cache-first core, makes it CURRENT, restores scope/filter/sort and lands
point on the bookmarked row — asserted NOT the first visible row, so the
landing can never pass off the default first-item fallback — with the
one-shot locator slot cleared and zero window-display calls.  Reverting
the handler or `org-air-view--bookmark-apply' fails."
  (skip-unless (locate-library "org-air"))
  (org-air-r58--with-corpus org-air-r58--board-specs
    (let* ((alpha (org-air-r58--open-scoped-board))
           (record (with-current-buffer org-air-view-buffer-name
                     (bookmark-make-record)))
           (loc (org-air-r58--field record 'org-air-item))
           (reread (org-air-r58--roundtrip record)))
      (org-air-r58--kill org-air-view-buffer-name)
      (should-not (get-buffer org-air-view-buffer-name))
      (org-air-r58--asserting-no-display
        (org-air-view-bookmark-jump reread))
      ;; The handler contract: the restored view is CURRENT, never shown.
      (should (eq (current-buffer) (get-buffer org-air-view-buffer-name)))
      (with-current-buffer org-air-view-buffer-name
        (should (derived-mode-p 'org-air-view-mode))
        (should (equal org-air-view--scope (list :file alpha)))
        (should (equal org-air-view--tag-filter '("#work")))
        (should (eq org-air-view--sort-key 'title))
        (should (eq org-air-view--sort-direction 'descending))
        ;; Point landed on the bookmarked row (exact marker match) …
        (should (equal (org-air-view--row-property 'org-air-marker) loc))
        (should (equal (org-air-item-title
                        (org-air-view--row-property 'org-air-item))
                       "Alpha follow-up"))
        ;; … which is NOT the first visible row (anti-tautology).
        (should (equal (org-air-item-title (org-air-r58--first-item))
                       "Ship the grant report"))
        ;; One-shot: the slot never survives a completed sync paint.
        (should-not org-air-view--bookmark-locator)))))

;;;; -------------------------------------------------------------------
;;;; r58-4 — T3: the project twin
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r58-4-project-twin ()
  "T3: project record (root/sort/flip/doc-at-point) → kill → jump restores.
The record is produced in a live `*org-air-project*', round-tripped
through prin1→read, and the handler alone rebuilds the tree: mode, root,
sort locals, the filename flip, and point on THAT doc's row — asserted
different from the tree's first doc row, so the landing is the locator's
doing.  Reverting `org-air-project--bookmark-make-record' /
`-apply' / the handler fails."
  (skip-unless (locate-library "org-air"))
  (let ((org-air-sources (list (list :air org-air-project-test-root)))
        (bookmark-alist nil))
    (unwind-protect
        (save-window-excursion
          (org-air-project)
          (let (record reread beta root)
            (with-current-buffer "*org-air-project*"
              (setq root org-air-project--root)
              (setq-local org-air-view--sort-key 'name)
              (setq-local org-air-view--sort-direction 'descending)
              (setq-local org-air-project--show-filenames t)
              (org-air-project--render org-air-project--root)
              (setq beta (org-air-r58--goto-doc-row "beta-cli.org"))
              (setq record (bookmark-make-record))
              (should (eq (org-air-r58--field record 'handler)
                          'org-air-project-bookmark-jump))
              (should (eq (org-air-r58--field record 'org-air-view) 'project))
              (should (equal (org-air-r58--field record 'org-air-root) root))
              (should (equal (org-air-r58--field record 'org-air-sort)
                             '(name . descending)))
              (should (eq (org-air-r58--field record 'org-air-show-filenames)
                          t))
              (should (equal (org-air-r58--field record 'org-air-item)
                             (cons beta 1)))
              (should (equal (org-air-r58--field record 'org-air-item-title)
                             "Beta CLI"))
              ;; T11's project name: root basename, most specific first.
              (should (equal (car (org-air-r58--field record 'defaults))
                             (format "org-air: project %s"
                                     (file-name-nondirectory
                                      (directory-file-name root)))))
              (should (stringp (org-air-r58--field record 'location)))
              (org-air-r58--assert-clean-keys record)
              (setq reread (org-air-r58--roundtrip record)))
            (org-air-r58--kill "*org-air-project*")
            (org-air-r58--asserting-no-display
              (org-air-project-bookmark-jump reread))
            (should (eq (current-buffer) (get-buffer "*org-air-project*")))
            (with-current-buffer "*org-air-project*"
              (should (derived-mode-p 'org-air-project-mode))
              (should (equal org-air-project--root root))
              (should (eq org-air-view--sort-key 'name))
              (should (eq org-air-view--sort-direction 'descending))
              (should (eq org-air-project--show-filenames t))
              (let ((doc (org-air-view--row-property 'org-air-doc)))
                (should doc)
                (should (equal (org-air-doc-file doc) beta)))
              ;; NOT the tree's default first-doc landing (anti-tautology:
              ;; the ready-ranked Alpha feature renders above Beta CLI).
              (let* ((first-pos (text-property-not-all
                                 (point-min) (point-max) 'org-air-doc nil))
                     (first-doc (get-text-property first-pos 'org-air-doc)))
                (should-not (equal (org-air-doc-file first-doc) beta))))))
      (org-air-r58--kill "*org-air-project*"))))

;;;; -------------------------------------------------------------------
;;;; r58-5 — T4: the revisit twin
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r58-5-revisit-twin ()
  "T4: revisit record (surface/sort/toggle/note-at-point) restores; pages=1.
Surface `orphans' + a non-default sort + the created toggle recorded from
a live Revisit session with point on the SECOND note under that sort;
`--pages' is deliberately NOT in the record and resets to 1 on jump (the
documented \"show more is a within-session interaction\" ruling).  The
jump into a fresh session restores surface + sort + toggle and lands
point on the bookmarked note row.  Reverting the revisit record fns or
handler fails."
  (skip-unless (locate-library "org-air"))
  (org-air-r58--with-corpus org-air-r58--revisit-specs
    (org-air-revisit)
    (let ((evergreen (org-air-r58--file "evergreen.org"))
          (zettel (org-air-r58--file "zettel.org"))
          record reread)
      (with-current-buffer org-air-revisit-buffer-name
        (setq-local org-air-revisit--surface 'orphans)
        (setq-local org-air-view--sort-key 'title)
        (setq-local org-air-view--sort-direction 'descending)
        (setq-local org-air-revisit--show-created t)
        (setq-local org-air-revisit--pages 3)
        (org-air-revisit--render)
        ;; Zettel sorts first under title-descending; bookmark Evergreen.
        (should (equal (car (org-air-r58--revisit-row-files)) zettel))
        (org-air-r58--goto-revisit-row evergreen)
        (setq record (bookmark-make-record))
        (should (eq (org-air-r58--field record 'handler)
                    'org-air-revisit-bookmark-jump))
        (should (eq (org-air-r58--field record 'org-air-view) 'revisit))
        (should (eq (org-air-r58--field record 'org-air-surface) 'orphans))
        (should (equal (org-air-r58--field record 'org-air-sort)
                       '(title . descending)))
        (should (eq (org-air-r58--field record 'org-air-show-created) t))
        (should (equal (org-air-r58--field record 'org-air-item)
                       (cons evergreen 1)))
        (should (equal (org-air-r58--field record 'org-air-item-title)
                       "Evergreen spaced repetition"))
        ;; The page depth never enters the record (the documented ruling).
        (should-not (seq-find (lambda (cell)
                                (and (consp cell)
                                     (string-match-p
                                      "page"
                                      (symbol-name (car cell)))))
                              (org-air-r58--alist record)))
        (should (equal (car (org-air-r58--field record 'defaults))
                       "org-air: revisit · orphans"))
        (should (stringp (org-air-r58--field record 'location)))
        (org-air-r58--assert-clean-keys record)
        (setq reread (org-air-r58--roundtrip record)))
      (org-air-r58--kill org-air-revisit-buffer-name)
      (org-air-r58--asserting-no-display
        (org-air-revisit-bookmark-jump reread))
      (should (eq (current-buffer) (get-buffer org-air-revisit-buffer-name)))
      (with-current-buffer org-air-revisit-buffer-name
        (should (derived-mode-p 'org-air-revisit-mode))
        (should (eq org-air-revisit--surface 'orphans))
        (should (eq org-air-view--sort-key 'title))
        (should (eq org-air-view--sort-direction 'descending))
        (should (eq org-air-revisit--show-created t))
        (should (= org-air-revisit--pages 1))
        (let ((entry (org-air-view--row-property 'org-air-revisit)))
          (should entry)
          (should (equal (car entry) evergreen)))
        ;; Still the second row — the landing is the locator's, not the
        ;; render's first-row default.
        (should (equal (car (org-air-r58--revisit-row-files)) zettel))))))

;;;; -------------------------------------------------------------------
;;;; r58-6 — T5: the day view round-trips through an integer epoch
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r58-6-day-view ()
  "T5: a day-board record carries `org-air-day' as a printable INTEGER.
The record's view kind is `day', the epoch decodes back to the same day
key on jump, and the day render is active in the restored buffer (its
dated header line is present).  Reverting the epoch encode (a raw Emacs
time in the alist) or the day half of `--bookmark-apply' fails."
  (skip-unless (locate-library "org-air"))
  (org-air-r58--with-corpus org-air-r58--board-specs
    (org-air-view)
    (let ((day-header (format-time-string "%A %-d %B %Y" org-air-test-now))
          record reread)
      (with-current-buffer org-air-view-buffer-name
        (setq-local org-air-view--day org-air-test-now)
        (org-air-view--render-current)
        (should (string-match-p (regexp-quote day-header)
                                (buffer-substring-no-properties
                                 (point-min) (point-max))))
        (setq record (bookmark-make-record))
        (should (eq (org-air-r58--field record 'org-air-view) 'day))
        (let ((epoch (org-air-r58--field record 'org-air-day)))
          (should (integerp epoch))
          (should (equal epoch (time-convert org-air-test-now 'integer))))
        (org-air-r58--assert-clean-keys record)
        (setq reread (org-air-r58--roundtrip record)))
      (org-air-r58--kill org-air-view-buffer-name)
      (org-air-r58--asserting-no-display
        (org-air-view-bookmark-jump reread))
      (with-current-buffer org-air-view-buffer-name
        (should (derived-mode-p 'org-air-view-mode))
        (should org-air-view--day)
        (should (equal (org-air-view--day-key org-air-view--day)
                       (org-air-view--day-key org-air-test-now)))
        ;; The day render is ACTIVE, not merely the local set.
        (should (string-match-p (regexp-quote day-header)
                                (buffer-substring-no-properties
                                 (point-min) (point-max))))))))

;;;; -------------------------------------------------------------------
;;;; r58-7 — T6: malformed / future records degrade, never signal
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r58-7-degrade-never-signals ()
  "T6: the handler on a bare header — and on version 999 — opens a plain
board and signals NOTHING.  The future-versioned record also carries an
unknown field (ignored), a malformed scope (dropped, not applied) and a
malformed day (dropped) — the every-field-optional forward-compat law.
A message spy proves the handler never even entered its degrade arm.
Reverting the optional-field reading in `--bookmark-apply' (or making
the handler trust the version) fails."
  (skip-unless (locate-library "org-air"))
  (org-air-r58--with-corpus org-air-r58--board-specs
    (let ((bare '((handler . org-air-view-bookmark-jump)
                  (location . "org-air: board")
                  (defaults . ("org-air: board"))
                  (org-air-version . 1)
                  (org-air-view . board)))
          (future '("org-air future"
                    (handler . org-air-view-bookmark-jump)
                    (location . "org-air: board")
                    (org-air-version . 999)
                    (org-air-view . board)
                    (org-air-scope . 42)
                    (org-air-flux-capacitor . "later")
                    (org-air-day . "not-a-day")))
          (messages nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when fmt (push (apply #'format fmt args) messages))
                   nil)))
        (org-air-view-bookmark-jump bare)
        (should (eq (current-buffer) (get-buffer org-air-view-buffer-name)))
        (with-current-buffer org-air-view-buffer-name
          (should (derived-mode-p 'org-air-view-mode))
          (should-not org-air-view--scope)
          (should-not org-air-view--tag-filter)
          (should-not org-air-view--day)
          (should org-air-view--items))   ; a REAL board, not a husk
        (org-air-r58--kill org-air-view-buffer-name)
        (org-air-view-bookmark-jump future)
        (should (eq (current-buffer) (get-buffer org-air-view-buffer-name)))
        (with-current-buffer org-air-view-buffer-name
          (should (derived-mode-p 'org-air-view-mode))
          (should-not org-air-view--scope)
          (should-not org-air-view--day)
          (should org-air-view--items)))
      ;; Neither jump entered the degrade arm, let alone signalled.
      (should-not (seq-find (lambda (m) (string-match-p "degraded" m))
                            messages)))))

;;;; -------------------------------------------------------------------
;;;; r58-8 — T7: the no-display contract
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r58-8-no-display-contract ()
  "T7: the handler never touches windows — activities' layout is safe.
The window configuration compares equal across the jump inside the
unchanged frame while `current-buffer' IS the restored view, and the
spied `pop-to-buffer' / `switch-to-buffer' / `display-buffer' count is
ZERO.  A handler that pops (like the command entry it wraps) fails
here."
  (skip-unless (locate-library "org-air"))
  (org-air-r58--with-corpus org-air-r58--board-specs
    (org-air-r58--open-scoped-board)
    (let ((record (with-current-buffer org-air-view-buffer-name
                    (bookmark-make-record))))
      (org-air-r58--kill org-air-view-buffer-name)
      (let ((before (org-air-r58--window-fingerprint)))
        (org-air-r58--asserting-no-display
          (org-air-view-bookmark-jump record))
        (should (equal before (org-air-r58--window-fingerprint)))
        (should (eq (current-buffer)
                    (get-buffer org-air-view-buffer-name)))))))

;;;; -------------------------------------------------------------------
;;;; r58-9 — T8: locator drift (edited file, vanished file)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r58-9-locator-drift ()
  "T8: the drift chain lands point without a signal.
\(a) The recorded file grows a preamble, so every heading POS moves and
the exact (FILE . POS) can no longer match any row: point lands via the
FILE+title fallback — asserted by the landed row's marker DIFFERING from
the recorded locator.  (b) The recorded file is DELETED before the jump:
point falls back to the first item row and the one-shot slot clears.
Reverting the fallback chain in `org-air-view--bookmark-consume' fails."
  (skip-unless (locate-library "org-air"))
  (org-air-r58--with-corpus org-air-r58--board-specs
    (org-air-view)
    (let ((alpha (org-air-r58--file "alpha.org"))
          (beta (org-air-r58--file "beta.org"))
          rec-a loc-a rec-b)
      (with-current-buffer org-air-view-buffer-name
        (org-air-r58--goto-item-row "Alpha follow-up")
        (setq rec-a (bookmark-make-record)
              loc-a (org-air-r58--field rec-a 'org-air-item)))
      (should (equal (car loc-a) alpha))
      ;; (a) POS drift: the file is edited, the heading moves.
      (org-air-r58--kill org-air-view-buffer-name)
      (let ((old (with-temp-buffer
                   (insert-file-contents alpha)
                   (buffer-string)))
            (coding-system-for-write 'utf-8-unix))
        (write-region (concat "# moved by an edit\n\n" old) nil alpha
                      nil 'silent))
      (org-air-view-bookmark-jump rec-a)
      (with-current-buffer org-air-view-buffer-name
        (let ((row-marker (org-air-view--row-property 'org-air-marker))
              (item (org-air-view--row-property 'org-air-item)))
          (should (equal (org-air-item-title item) "Alpha follow-up"))
          (should (equal (car row-marker) alpha))
          ;; The exact match was impossible — this IS the title fallback.
          (should-not (equal row-marker loc-a))
          (should-not org-air-view--bookmark-locator))
        ;; (b) The file vanishes entirely: first-item fallback, no signal.
        (org-air-r58--goto-item-row "Beta errand")
        (setq rec-b (bookmark-make-record)))
      (org-air-r58--kill org-air-view-buffer-name)
      (delete-file beta)
      (org-air-view-bookmark-jump rec-b)
      (with-current-buffer org-air-view-buffer-name
        (should-not org-air-view--bookmark-locator)
        (let* ((first-pos (text-property-not-all (point-min) (point-max)
                                                 'org-air-item nil))
               (item (org-air-view--row-property 'org-air-item)))
          (should item)
          (should-not (equal (org-air-item-file item) beta))
          (should (= (line-number-at-pos (point))
                     (line-number-at-pos first-pos))))))))

;;;; -------------------------------------------------------------------
;;;; r58-10 — T9: record production mid-refresh
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r58-10-mid-refresh-record ()
  "T9: `bookmark-make-record' over the live refresh machine stays pure.
With `--refresh-state' forced to `refreshing' and the stale/queue lists
populated (activities re-saves on timers — mid-refresh saves are
routine), the producer still returns a valid record: handler + view kind
present, the row locator intact, every key reserved-or-org-air-prefixed
\(no in-flight machine state leaks into the alist) and the whole record
prin1→read round-trips.  Reverting the record producer fails
\(`bookmark-make-record-default' signals in the non-file board)."
  (skip-unless (locate-library "org-air"))
  (org-air-r58--with-corpus org-air-r58--board-specs
    (org-air-view)
    (with-current-buffer org-air-view-buffer-name
      (org-air-r58--goto-item-row "Beta errand")
      (setq-local org-air-view--refresh-state 'refreshing)
      (setq-local org-air-view--cache-stale-files
                  (list (org-air-r58--file "alpha.org")
                        (org-air-r58--file "beta.org")))
      (setq-local org-air-view--refresh-queue
                  (list (org-air-r58--file "alpha.org")))
      (unwind-protect
          (let ((record (bookmark-make-record)))
            (should (eq (org-air-r58--field record 'handler)
                        'org-air-view-bookmark-jump))
            (should (eq (org-air-r58--field record 'org-air-view) 'board))
            (should (equal (org-air-r58--field record 'org-air-item-title)
                           "Beta errand"))
            (should (equal (car (org-air-r58--field record 'org-air-item))
                           (org-air-r58--file "beta.org")))
            (org-air-r58--assert-clean-keys record)
            (org-air-r58--roundtrip record))
        (setq-local org-air-view--refresh-state nil)
        (setq-local org-air-view--cache-stale-files nil)
        (setq-local org-air-view--refresh-queue nil)))))

;;;; -------------------------------------------------------------------
;;;; r58-11 — T10: the rail delegates to its host; no double init
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r58-11-rail-delegation ()
  "T10: the rail record restores host + rail; restore order never matters.
The rail record delegates — view `rail', `org-air-host' `board', the
HOST's scope/sort fields embedded through the back-pointer, the
host-derived \" · rail\" name.  With BOTH buffers killed the rail record
ALONE rebuilds the host board (undisplayed, embedded scope applied) and
the rail (current, `org-air-rail-mode', rendered, back-pointer set).
Jumping the BOARD record afterwards enters the live buffer through the
R26-5 idempotent guard: a probe buffer-local survives, proving
`kill-all-local-variables' never re-ran (no double init).  Reverting the
rail record/handler — or the guard — fails."
  (skip-unless (locate-library "org-air"))
  (org-air-r58--with-corpus org-air-r58--board-specs
    (let* ((alpha (org-air-r58--open-scoped-board))
           (board-record (with-current-buffer org-air-view-buffer-name
                           (bookmark-make-record)))
           rail-record reread)
      ;; Build the dependent rail beside its host (the batch-safe seam).
      (org-air-rail--render (get-buffer org-air-view-buffer-name)
                            (org-air-rail--window-cols org-air-view-width))
      (with-current-buffer org-air-rail-buffer-name
        (should (derived-mode-p 'org-air-rail-mode))
        (setq rail-record (bookmark-make-record))
        (should (eq (org-air-r58--field rail-record 'handler)
                    'org-air-rail-bookmark-jump))
        (should (eq (org-air-r58--field rail-record 'org-air-view) 'rail))
        (should (eq (org-air-r58--field rail-record 'org-air-host) 'board))
        ;; The HOST's view-defining fields are embedded (the delegation).
        (should (equal (org-air-r58--field rail-record 'org-air-scope)
                       (list :file alpha)))
        (should (equal (org-air-r58--field rail-record 'org-air-sort)
                       '(title . descending)))
        (should (string-suffix-p
                 " · rail"
                 (car (org-air-r58--field rail-record 'defaults))))
        (should (stringp (org-air-r58--field rail-record 'location)))
        (org-air-r58--assert-clean-keys rail-record)
        (setq reread (org-air-r58--roundtrip rail-record)))
      ;; Kill BOTH; the rail record alone restores the pair.
      (org-air-r58--kill org-air-rail-buffer-name org-air-view-buffer-name)
      (org-air-r58--asserting-no-display
        (org-air-rail-bookmark-jump reread))
      (should (eq (current-buffer) (get-buffer org-air-rail-buffer-name)))
      (with-current-buffer org-air-rail-buffer-name
        (should (derived-mode-p 'org-air-rail-mode))
        (should (> (buffer-size) 0))
        (should (eq org-air-rail--board-buffer
                    (get-buffer org-air-view-buffer-name))))
      ;; The host came back too — undisplayed, with the embedded state.
      (with-current-buffer org-air-view-buffer-name
        (should (derived-mode-p 'org-air-view-mode))
        (should (equal org-air-view--scope (list :file alpha)))
        (setq-local org-air-r58--board-probe t))
      ;; Restore-order independence: the board record restores LATER and
      ;; finds the live buffer — the R26-5 guard, no double init.
      (let ((host (get-buffer org-air-view-buffer-name)))
        (org-air-r58--asserting-no-display
          (org-air-view-bookmark-jump board-record))
        (should (eq (current-buffer) host))
        (with-current-buffer host
          (should org-air-r58--board-probe)
          (should (equal org-air-view--scope (list :file alpha))))))))

;;;; -------------------------------------------------------------------
;;;; r58-12 — the entry pane delegates to its host + (FILE . POS) source
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r58-12-entry-pane-delegation ()
  "The snapshot pane's delegating record restores pane + host from scratch.
The record carries `org-air-entry-ctx' = the snapshot's (FILE . POS)
source (from the printable stash the one snapshot writer left) plus the
host kind.  With pane AND board killed, the handler rebuilds both: the
pane is CURRENT in `org-air-entry-view-mode' showing the entry text, the
host board is ensured undisplayed.  A VANISHED source renders the pane's
existing missing-source hint — never a signal.  Reverting the pane
record producer / stash / handler fails."
  (skip-unless (locate-library "org-air"))
  (org-air-r58--with-corpus org-air-r58--board-specs
    (org-air-view)
    (let (marker title record reread)
      (with-current-buffer org-air-view-buffer-name
        (org-air-r58--goto-item-row "Ship the grant report")
        (setq marker (org-air-view--row-property 'org-air-marker)
              title (org-air-item-title
                     (org-air-view--row-property 'org-air-item)))
        ;; The pane's normal snapshot path, written from the HOST buffer.
        (org-air-view-pane--render-snapshot
         (list :file (car marker) :marker marker :title title)
         (org-air-view-pane--source-buffer-pos marker)))
      (with-current-buffer org-air-view-pane-buffer-name
        (should (derived-mode-p 'org-air-entry-view-mode))
        (setq record (bookmark-make-record))
        (should (eq (org-air-r58--field record 'handler)
                    'org-air-entry-view-bookmark-jump))
        (should (eq (org-air-r58--field record 'org-air-view) 'entry))
        (should (eq (org-air-r58--field record 'org-air-host) 'board))
        (should (equal (org-air-r58--field record 'org-air-entry-ctx)
                       marker))
        (should (equal (car (org-air-r58--field record 'defaults))
                       "org-air: board · entry"))
        (should (stringp (org-air-r58--field record 'location)))
        (org-air-r58--assert-clean-keys record)
        (setq reread (org-air-r58--roundtrip record)))
      (org-air-r58--kill org-air-view-pane-buffer-name
                         org-air-view-buffer-name)
      (org-air-r58--asserting-no-display
        (org-air-entry-view-bookmark-jump reread))
      (should (eq (current-buffer)
                  (get-buffer org-air-view-pane-buffer-name)))
      (with-current-buffer org-air-view-pane-buffer-name
        (should (derived-mode-p 'org-air-entry-view-mode))
        (should (string-match-p (regexp-quote "Ship the grant report")
                                (buffer-substring-no-properties
                                 (point-min) (point-max)))))
      ;; The host board was ensured, undisplayed.
      (should (get-buffer org-air-view-buffer-name))
      (with-current-buffer org-air-view-buffer-name
        (should (derived-mode-p 'org-air-view-mode)))
      ;; A vanished source: the missing-source rendering, never a signal.
      (let ((gone `((handler . org-air-entry-view-bookmark-jump)
                    (location . "org-air: entry")
                    (org-air-version . 1)
                    (org-air-view . entry)
                    (org-air-host . board)
                    (org-air-entry-ctx
                     . ,(cons (expand-file-name "vanished.org"
                                                org-air-r58--dir)
                              1))
                    (org-air-item-title . "Vanished"))))
        (org-air-entry-view-bookmark-jump gone)
        (with-current-buffer org-air-view-pane-buffer-name
          (should (string-match-p "entry no longer available"
                                  (buffer-substring-no-properties
                                   (point-min) (point-max)))))))))

;;;; -------------------------------------------------------------------
;;;; r58-13 — the help buffer's trivial record
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r58-13-help-trivial-record ()
  "Help records context-only; jump re-renders help for that context.
Included so no `*org-air-help*' in a saved activities layout can ever
raise the \"does not support the bookmark system\" error.  Reverting the
help record producer / handler fails."
  (skip-unless (locate-library "org-air"))
  (let ((bookmark-alist nil))
    (unwind-protect
        (save-window-excursion
          (let ((buf (get-buffer-create org-air-help-buffer-name))
                record reread)
            (org-air-help--render buf 'board buf)
            (with-current-buffer buf
              (should (derived-mode-p 'org-air-help-mode))
              (setq record (bookmark-make-record))
              (should (eq (org-air-r58--field record 'handler)
                          'org-air-help-bookmark-jump))
              (should (eq (org-air-r58--field record 'org-air-view) 'help))
              (should (eq (org-air-r58--field record 'org-air-help-context)
                          'board))
              (should (equal (org-air-r58--field record 'location)
                             "org-air: help"))
              (org-air-r58--assert-clean-keys record)
              (setq reread (org-air-r58--roundtrip record)))
            (org-air-r58--kill org-air-help-buffer-name)
            (org-air-r58--asserting-no-display
              (org-air-help-bookmark-jump reread))
            (should (eq (current-buffer)
                        (get-buffer org-air-help-buffer-name)))
            (with-current-buffer org-air-help-buffer-name
              (should (derived-mode-p 'org-air-help-mode))
              (should (> (buffer-size) 0))
              (should (eq org-air-help--context-sym 'board)))))
      (org-air-r58--kill org-air-help-buffer-name))))

;;;; -------------------------------------------------------------------
;;;; r58-14 — T11: naming + the autoload/handler-type metadata
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r58-14-naming-and-handler-metadata ()
  "T11 + handler metadata: names read honestly; handlers are restorable.
Every handler is `fboundp', carries `(bookmark-handler-type . \"org-air\")'
for the bmenu type column, and its defun sits under a LITERAL
`;;;###autoload' cookie in the owning source file — activities restores
in a fresh Emacs where org-air is installed but not loaded, so a
cookie-less handler would abort the whole-frame restore.  The `defaults'
HEAD reads \"org-air: board\" generically, \"org-air: board · file …\"
under a file scope and \"org-air: day …\" on a day board (day wins over
scope; the generic name stays as a later candidate); `location' is
present on every record.  Reverting a cookie, a `put', or the name
builder fails."
  (skip-unless (locate-library "org-air"))
  ;; (a) handler metadata: type property + the literal autoload cookie.
  (pcase-dolist (`(,sym . ,lib)
                 '((org-air-view-bookmark-jump . "org-air-view")
                   (org-air-rail-bookmark-jump . "org-air-view")
                   (org-air-entry-view-bookmark-jump . "org-air-view")
                   (org-air-help-bookmark-jump . "org-air-view")
                   (org-air-project-bookmark-jump . "org-air-project")
                   (org-air-revisit-bookmark-jump . "org-air-revisit")))
    (ert-info ((format "handler=%s" sym))
      (should (fboundp sym))
      (should (equal (get sym 'bookmark-handler-type) "org-air"))
      (let* ((loaded (locate-library lib))
             (el (and loaded
                      (concat (file-name-sans-extension loaded) ".el"))))
        (should (and el (file-readable-p el)))
        (with-temp-buffer
          (insert-file-contents el)
          (goto-char (point-min))
          (should (re-search-forward
                   (concat "^;;;###autoload[ \t]*\n(defun "
                           (regexp-quote (symbol-name sym)) " ")
                   nil t))))))
  ;; (b) naming: most specific first, `location' always present.
  (org-air-r58--with-corpus org-air-r58--board-specs
    (org-air-view)
    (with-current-buffer org-air-view-buffer-name
      (let ((alpha (org-air-r58--file "alpha.org")))
        ;; Generic board.
        (let ((record (bookmark-make-record)))
          (should (equal (car (org-air-r58--field record 'defaults))
                         "org-air: board"))
          (should (stringp (org-air-r58--field record 'location))))
        ;; File scope: the scoped name heads the candidates.
        (setq-local org-air-view--scope (list :file alpha))
        (let ((record (bookmark-make-record)))
          (should (equal (car (org-air-r58--field record 'defaults))
                         "org-air: board · file alpha.org"))
          (should (member "org-air: board"
                          (org-air-r58--field record 'defaults)))
          (should (stringp (org-air-r58--field record 'location))))
        ;; Day view: the day name wins over the scope.
        (setq-local org-air-view--day org-air-test-now)
        (let ((record (bookmark-make-record)))
          (should (equal (car (org-air-r58--field record 'defaults))
                         (format-time-string "org-air: day %Y-%m-%d"
                                             org-air-test-now)))
          (should (member "org-air: board"
                          (org-air-r58--field record 'defaults)))
          (should (stringp (org-air-r58--field record 'location))))))))

(provide 'org-air-round58-test)
;;; org-air-round58-test.el ends here
