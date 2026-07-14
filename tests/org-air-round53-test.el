;;; org-air-round53-test.el --- executing ERTs for v0.5 round-53 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.5 round-53 (air/v0.5/org-air-round53-design.org):
;; NEVER-HANG at 5000+ files — the work-buffer scan, the never-error law,
;; budgeted pacing, the data-pure render, and bounded headingless-note
;; file-items.  All BATCH/headless, driven through the spec's named seams
;; (the slice runner, the watchdog fire, `org-air-query--scan-file'); no
;; timers, no idle waits, no GUI.  Reverting the R53 impl fails each:
;;
;;   R53-1 NEVER-ERROR (P1b, seams 1/2) — a headingless file, a NUL/binary
;;         `.org', an empty file, an unreadable file, a garbage `.org.gpg'
;;         and a dangling symlink each yield 0 heading-items and NEVER
;;         signal/abort the scan (bad files sort FIRST, so a pre-R53 abort
;;         would lose the good file's items); skips are logged once, no
;;         `No headings' echo spam reaches *Messages*.
;;   R53-2 NO-RETENTION (P1, seam 3) — a full scan retains NO source
;;         buffer (`get-file-buffer' nil for every unopened corpus file;
;;         visible `buffer-list' grows by <=1: the ONE reused work buffer)
;;         and calls `find-file-noselect' ZERO times; a LIVE user buffer
;;         scans in place (unsaved edits respected — rule 1).
;;   R53-3 DATA-PURE RENDER (P2, seam 6) — painting a cache-hydrated
;;         board (every marker a (FILE . POS) cons; undated/headingless
;;         worst cases present) opens NO file: `find-file-noselect' spy
;;         counts 0 on the paint path, no buffer visits any source file
;;         after the render.  This is the 186s warm-paint fix.
;;   R53-4 BOUNDED FILE-ITEMS (P3, seam 1) — a headingless note yields
;;         exactly ONE openable file-item (`#+title', filename fallback;
;;         (FILE . 1) marker; RET-openable at the top); the inbox and
;;         binary junk never yield one.
;;   R53-5 NOTES SECTION BOUNDED (P3, seam 9) — the board shows ONE
;;         collapsed count row; expanded shows `org-air-notes-preview-
;;         limit' most-recent rows + the fold row; task buckets carry
;;         ZERO `kind' `file' items; the knob removes the section.
;;   R53-6 REFILE (P4, seam 7) — targets enumerate from the board's
;;         index (zero re-enumeration, zero file opens, zero scans for
;;         the Tags…/Category… vocab) and INCLUDE a headingless file;
;;         refiling onto it lands at file end under the `#+title' content
;;         (the refile-to-file-top contract).
;;   R53-7 PACING / NEVER-HANG (P1c, seams 4/5) — a >sync-budget change
;;         set paces (state `refreshing', ZERO synchronous scans at
;;         start); the watchdog NEVER force-scans that queue on the main
;;         thread; pending input aborts a slice with the queue intact
;;         (C-g abortable) and a file that keeps aborting is skip-logged
;;         `slow' (no livelock); the queue then CONVERGES by slices.
;;   R53-8 BUDGETED SLICES (P1c, seam 5) — one budget-0 slice consumes
;;         exactly its ONE-file minimum; a generous-budget slice consumes
;;         MANY files even with the obsoleted `org-air-refresh-files-per-
;;         slice' bound to 1 (the fixed count is dead — time governs).
;;
;; Perf probes stay OUT of the gate per the round instructions: every
;; corpus here is tiny/bounded (<= ~20 files); the order-of-magnitude
;; smoke lives in tests/org-air-perf-test.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'org-air-test-helpers)

(when (locate-library "org-air")
  (require 'org-air))

;;;; -------------------------------------------------------------------
;;;; Corpus / board scaffolding
;;;; -------------------------------------------------------------------

(defvar org-air-r53--dir nil
  "The temp corpus directory of the current `org-air-r53--with-corpus'.")

(defmacro org-air-r53--with-corpus (specs &rest body)
  "Create a temp Org corpus from SPECS and run BODY against it.
SPECS is a list of (NAME . CONTENT) files written into a fresh temp
directory (handlers disabled, so a `.org.gpg' fixture is written RAW —
epa must never intercept a test write).  Binds `org-air-files' to the
directory, `org-air-inbox-file' to its inbox.org, a temp
`org-air-cache-file' and the standard batch board geometry.  Cleans up
the scan work buffer, every corpus-visiting buffer and the directory."
  (declare (indent 1) (debug t))
  `(let* ((org-air-r53--dir (make-temp-file "org-air-r53-" t)))
     (unwind-protect
         (progn
           (pcase-dolist (`(,name . ,content) ,specs)
             (let ((file-name-handler-alist nil)
                   (coding-system-for-write 'utf-8-unix))
               (write-region (or content "") nil
                             (expand-file-name name org-air-r53--dir)
                             nil 'silent)))
           (let ((org-air-files (list org-air-r53--dir))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-r53--dir))
                 (org-air-cache-file
                  (expand-file-name ".cache/board.eld" org-air-r53--dir))
                 (org-air-view-width 120)
                 (org-air-view-height 50))
             ,@body))
       (org-air-query-teardown)
       (let ((kill-buffer-query-functions nil))
         (dolist (buf (buffer-list))
           (let ((fn (buffer-file-name buf)))
             (when (and fn (string-prefix-p org-air-r53--dir fn))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r53--dir t))))

(defmacro org-air-r53--with-board (&rest body)
  "Run BODY in a fresh `org-air-view-mode' board buffer (killed after)."
  (declare (indent 0) (debug t))
  `(let ((org-air-view-buffer-name "*org-air-r53*"))
     (unwind-protect
         (with-current-buffer (get-buffer-create org-air-view-buffer-name)
           (unless (derived-mode-p 'org-air-view-mode) (org-air-view-mode))
           ,@body)
       (when (get-buffer "*org-air-r53*")
         (let ((kill-buffer-query-functions nil))
           (kill-buffer "*org-air-r53*"))))))

(defvar org-air-r53--scan-calls 0
  "Calls to the org-ql scan entry points while counting (see R42 note:
`org-ql-select' is a macro, so a live count only exists at the named
entry functions `org-air-query-items' / `org-air-query-items-in-files').")
(defvar org-air-r53--enum-calls 0
  "Calls to `org-air-query-files' (a full enumeration) while counting.")
(defvar org-air-r53--ffns-calls 0
  "Calls to `find-file-noselect' while counting.")

(defmacro org-air-r53--counting (&rest body)
  "Run BODY with scan / enumeration / file-open call counters installed."
  (declare (indent 0) (debug t))
  `(let ((org-air-r53--scan-calls 0)
         (org-air-r53--enum-calls 0)
         (org-air-r53--ffns-calls 0))
     (cl-letf* ((inf-orig (symbol-function 'org-air-query-items-in-files))
                ((symbol-function 'org-air-query-items-in-files)
                 (lambda (&rest args)
                   (cl-incf org-air-r53--scan-calls)
                   (apply inf-orig args)))
                (items-orig (symbol-function 'org-air-query-items))
                ((symbol-function 'org-air-query-items)
                 (lambda (&rest args)
                   (cl-incf org-air-r53--scan-calls)
                   (apply items-orig args)))
                (files-orig (symbol-function 'org-air-query-files))
                ((symbol-function 'org-air-query-files)
                 (lambda (&rest args)
                   (cl-incf org-air-r53--enum-calls)
                   (apply files-orig args)))
                (ffns-orig (symbol-function 'find-file-noselect))
                ((symbol-function 'find-file-noselect)
                 (lambda (&rest args)
                   (cl-incf org-air-r53--ffns-calls)
                   (apply ffns-orig args))))
       ,@body)))

(defun org-air-r53--items-for-file (items file)
  "Return the subset of ITEMS whose source file is FILE."
  (seq-filter (lambda (it) (equal (org-air-item-file it) file)) items))

(defun org-air-r53--skip-reason (file)
  "Return FILE's reason in the last scan's skip log, or nil."
  (cdr (assoc file org-air-query--skip-log)))

;;;; -------------------------------------------------------------------
;;;; R53-1 — the never-error law (P1b, ERT seams 1/2)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r53-1-never-error-odd-files ()
  "A headingless file, a NUL/binary `.org', an empty file, an unreadable
file, a garbage `.org.gpg' and a dangling symlink each yield 0
heading-items and NEVER signal or abort the scan.  The bad files sort
FIRST (aa-/ab-/ac-/ad-), so the pre-R53 whole-batch abort (org-ql's
`bufferp' abort on unreadable/dangling, epa's `file-error' on garbage
gpg) would return 0 items for the GOOD files too — the user's `errored
out repeatedly' + permanently-empty-board loop.  Skips land ONCE in the
skip log; the org-ql `No headings in buffer' per-file message never
reaches *Messages* (P1's `inhibit-message' seam)."
  (skip-unless (locate-library "org-air"))
  (org-air-r53--with-corpus
      `(("aa-binary.org" . ,(concat (make-string 64 ?\0) "binary garbage"))
        ("ab-garbage.org.gpg" . "not pgp data at all")
        ("ac-unreadable.org" . "* TODO hidden behind chmod 000\n")
        ("ae-empty.org" . "")
        ("af-blank.org" . "  \n\t\n")
        ("ba-note.org" . "#+title: Corpus note\n\nProse, no headings.\n")
        ("bb-good.org" . "* TODO Good heading :work:\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let ((unreadable (expand-file-name "ac-unreadable.org" org-air-r53--dir))
          (dangling (expand-file-name "ad-dangling.org" org-air-r53--dir))
          (msg-tail (with-current-buffer (messages-buffer) (point-max))))
      (set-file-modes unreadable 0)
      (make-symbolic-link "/nonexistent/r53-nowhere.org" dangling)
      ;; the whole scan: a signal ANYWHERE here fails the test.
      (let ((items (org-air-query-items)))
        (should (listp items))
        ;; the GOOD file survived the bad ones that sort before it.
        (let ((good (org-air-test-find-item "Good heading" items)))
          (should good)
          (should (eq (org-air-item-kind good) 'heading)))
        ;; headingless note: 0 heading-items (one bounded file-item).
        (let ((note-items (org-air-r53--items-for-file
                           items
                           (expand-file-name "ba-note.org" org-air-r53--dir))))
          (should (= (length note-items) 1))
          (should (eq (org-air-item-kind (car note-items)) 'file))
          (should-not (seq-find (lambda (it)
                                  (eq (org-air-item-kind it) 'heading))
                                note-items)))
        ;; binary junk / empty / blank: 0 items of ANY kind, and the
        ;; quiet cases are not even skip-logged (0 items is their truth).
        (dolist (name '("aa-binary.org" "ae-empty.org" "af-blank.org"))
          (let ((f (expand-file-name name org-air-r53--dir)))
            (should-not (org-air-r53--items-for-file items f))
            (should-not (org-air-r53--skip-reason f))))
        ;; policy-table skips: one entry each, scan continued.
        (should (eq (org-air-r53--skip-reason
                     (expand-file-name "ab-garbage.org.gpg" org-air-r53--dir))
                    'encrypted))
        (unless (file-readable-p unreadable)   ; root would read through 000
          (should (eq (org-air-r53--skip-reason unreadable) 'unreadable))
          (should-not (org-air-r53--items-for-file items unreadable)))
        ;; the dangling symlink dedupes to its (nonexistent) target at
        ;; enumeration; the scan skips it with a file-level reason.
        (let ((entry (seq-find (lambda (e)
                                 (string-suffix-p "nowhere.org" (car e)))
                               org-air-query--skip-log)))
          (should entry)))
      ;; zero echo spam: nothing the scan logged mentions org-ql's
      ;; per-file `No headings in buffer' message.
      (should-not (with-current-buffer (messages-buffer)
                    (save-excursion
                      (goto-char msg-tail)
                      (search-forward "No headings" nil t)))))))

;;;; -------------------------------------------------------------------
;;;; R53-2 — the work-buffer scan retains NO source buffer (P1, seam 3)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r53-2-scan-retains-no-source-buffers ()
  "A full scan provisions org-ql from ONE reused work buffer: the visible
buffer list grows by at most 1 (`*org-air scan*'), `find-file-noselect'
is called ZERO times, and no buffer visits any scanned file afterwards.
A LIVE user buffer visiting a corpus file is scanned IN PLACE (its
UNSAVED heading is picked up — P1 rule 1) and its marker slot is still
the durable (FILE . POS) cons.  Reverting to org-ql's
`find-file-noselect' provisioning fails (one leaked buffer per file —
the measured O(n^2) 271.8s cold scan)."
  (skip-unless (locate-library "org-air"))
  (let ((specs (list '("inbox.org" . "* TODO Inbox capture :inbox:\n"))))
    (dotimes (i 8)
      (push (cons (format "h%02d.org" i)
                  (format "* TODO Task %02da\n* NEXT Task %02db\n" i i))
            specs))
    (dotimes (i 3)
      (push (cons (format "n%02d.org" i)
                  (format "#+title: R53 corpus note %02d\n\nProse.\n" i))
            specs))
    (org-air-r53--with-corpus specs
      ;; rule 1: ONE live user buffer with an unsaved edit.
      (let ((live-file (expand-file-name "h00.org" org-air-r53--dir)))
        (with-current-buffer (find-file-noselect live-file)
          (goto-char (point-max))
          (insert "* TODO Unsaved live edit\n"))
        (let ((visible-before
               (seq-remove (lambda (b) (string-prefix-p " " (buffer-name b)))
                           (buffer-list)))
              items)
          (org-air-r53--counting
            (setq items (org-air-query-items))
            (should (= org-air-r53--ffns-calls 0)))
          ;; exact item count: 8 headed x2 +1 unsaved +3 notes +1 inbox.
          (should (= (length items) (+ (* 8 2) 1 3 1)))
          (should (org-air-test-find-item "Unsaved live edit" items))
          ;; every marker is the durable cons — even from the live buffer.
          (dolist (it items)
            (should (consp (org-air-item-marker it)))
            (should (stringp (car (org-air-item-marker it)))))
          ;; no buffer visits any corpus file except the pre-existing live
          ;; one — the scan retained NOTHING.
          (dolist (f (org-air-query-files))
            (if (equal f live-file)
                (should (get-file-buffer f))
              (should-not (get-file-buffer f))))
          ;; visible buffer-list growth <= 1: the one reused work buffer.
          (let ((new (seq-difference
                      (seq-remove (lambda (b)
                                    (string-prefix-p " " (buffer-name b)))
                                  (buffer-list))
                      visible-before)))
            (should (<= (length new) 1))
            (dolist (b new)
              (should (equal (buffer-name b) "*org-air scan*"))))
          ;; a SECOND scan reuses the same work buffer (no growth at all).
          (let ((before2 (buffer-list)))
            (org-air-r53--counting
              (org-air-query-items)
              (should (= org-air-r53--ffns-calls 0)))
            (should (equal (buffer-list) before2))))))))

;;;; -------------------------------------------------------------------
;;;; R53-3 — the data-pure render: painting opens NO file (P2, seam 6)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r53-3-render-data-pure ()
  "Rendering a CACHE-HYDRATED board — every marker a (FILE . POS) cons,
with undated items and headingless notes present (the worst case: every
per-item render probe used to fall through to a file open) — calls
`find-file-noselect' ZERO times and leaves no buffer visiting any source
file.  This is the 186.6s/4905-buffer warm first paint fixed.  The
scan-time slots also carry the classification ground truth: a DONE item
classifies done (never attention) and a note classifies to exactly
\(notes) — pure struct reads.

The ONE design-blessed exception is pinned separately: the rail
INSPECTOR may hydrate the single SELECTED item's file (NOWARN, bounded,
per spec `inspector CREATED hydrates its ONE file with NOWARN' — the
design's measured `exactly 1 buffer touched') — never one per item."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (let ((note1 (expand-file-name "zz-note-alpha.org" org-air-test--dir))
          (note2 (expand-file-name "zz-note-beta.org" org-air-test--dir)))
      (write-region "#+title: R53 alpha note\n\nProse only.\n" nil note1
                    nil 'silent)
      (write-region "Just prose: no title, no headings.\n" nil note2
                    nil 'silent)
      (let ((org-air-files (append org-air-files (list note1 note2)))
            (org-air-cache-file
             (expand-file-name "cache/r53.eld" org-air-test--dir))
            (org-air-view-width 120)
            (org-air-view-height 50))
        ;; previous session: scan + persist the cache, then vanish.
        (let* ((files (org-air-query-files))
               (items (org-air-query-items))
               (snapshot (org-air-view--mtimes-snapshot files)))
          (should (org-air-test-find-item "R53 alpha note" items))
          (org-air-view--cache-write items snapshot))
        (org-air-query-teardown)
        ;; fresh session: hydrate from the cache, paint under the spy.
        (let* ((data (org-air-view--cache-read))
               (items (plist-get data :items)))
          (should (consp items))
          (dolist (it items)
            (should (consp (org-air-item-marker it))))
          ;; the worst-case rows are in the set: an undated heading and
          ;; the headingless notes (anti-tautology for the spy below).
          (should (seq-find (lambda (it)
                              (and (eq (org-air-item-kind it) 'heading)
                                   (null (org-air-item-scheduled it))
                                   (null (org-air-item-deadline it))))
                            items))
          (should (org-air-test-find-item "R53 alpha note" items))
          (org-air-r53--with-board
            (setq org-air-view--items items
                  org-air-view--items-key (list org-air-files
                                                org-air-inbox-file)
                  org-air-view--classify-cache nil)
            (let ((visible-before
                   (seq-remove (lambda (b)
                                 (string-prefix-p " " (buffer-name b)))
                               (buffer-list))))
              ;; THE fence: the paint path (rows, buckets, date labels,
              ;; calendar band) opens NO file — inspector region off, so
              ;; its separately-blessed one-file hydrate is out of frame.
              (let ((org-air-show-inspector nil))
                (org-air-r53--counting
                  (org-air-view--render items nil)
                  (should (= org-air-r53--ffns-calls 0)))
                ;; …and re-rendering (the classify-cache warm path) too.
                (org-air-r53--counting
                  (org-air-view--render items nil)
                  (should (= org-air-r53--ffns-calls 0))))
              ;; no buffer visits any source file; no visible buffer new.
              (dolist (f (org-air-query-files))
                (should-not (get-file-buffer f)))
              (should (equal (seq-remove
                              (lambda (b)
                                (string-prefix-p " " (buffer-name b)))
                              (buffer-list))
                             visible-before))
              ;; the DEFAULT render (inspector ON): only the inspector's
              ;; single NOWARN hydrate of the ONE selected item's file may
              ;; run — never a per-item open (>= 15 items here, and the
              ;; pre-R53 render opened every cache-hydrated item's file).
              (should (> (length items) 15))
              (let ((opened nil))
                (cl-letf* ((orig (symbol-function 'find-file-noselect))
                           ((symbol-function 'find-file-noselect)
                            (lambda (file &optional nowarn &rest rest)
                              (push (cons file nowarn) opened)
                              (apply orig file nowarn rest))))
                  (org-air-view--render items nil))
                (should (<= (length (delete-dups (mapcar #'car opened))) 1))
                (pcase-dolist (`(,_file . ,nowarn) opened)
                  (should nowarn))))
            ;; the board really painted: rows + the bounded Notes section.
            (let ((text (substring-no-properties (buffer-string))))
              (should (string-match-p "Prepare standup notes" text))
              (should (string-match-p "Notes 2" text)))
            ;; slot-driven classification ground truth (donep/kind).
            (let ((done (org-air-test-find-item "Pay invoices" items))
                  (note (org-air-test-find-item "R53 alpha note" items)))
              (should done)
              (should (org-air-item-donep done))
              (should-not (memq 'attention (org-air-classify-item done)))
              (should (equal (org-air-classify-item note) '(notes))))))))))

;;;; -------------------------------------------------------------------
;;;; R53-4 — bounded file-items for headingless notes (P3, seam 1)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r53-4-headingless-note-one-openable-file-item ()
  "A headingless note yields exactly ONE file-item: title from `#+title'
\(filename base fallback), tags from `#+filetags', marker (FILE . 1); it
is OPENABLE — `org-air-visit-item' lands at the top of the real file.
The inbox file and NUL/binary junk never yield one (bounded: junk and
chrome never become rows)."
  (skip-unless (locate-library "org-air"))
  (org-air-r53--with-corpus
      `(("titled.org" . ,(concat "#+title: R53 titled note\n"
                                 "#+filetags: :journal:ideas:\n\n"
                                 "Prose under the title.\n"))
        ("untitled.org" . "Only prose here, not even a title.\n")
        ("junk.org" . ,(concat (make-string 32 ?\0) "junk"))
        ("inbox.org" . "#+title: inbox\n\nempty chrome, not content\n"))
    (let ((titled (expand-file-name "titled.org" org-air-r53--dir))
          (untitled (expand-file-name "untitled.org" org-air-r53--dir))
          (junk (expand-file-name "junk.org" org-air-r53--dir)))
      ;; #+title note: exactly one item, fully described.
      (let ((items (org-air-query--scan-file titled)))
        (should (= (length items) 1))
        (let ((it (car items)))
          (should (eq (org-air-item-kind it) 'file))
          (should (equal (org-air-item-title it) "R53 titled note"))
          (should (member "journal" (org-air-item-tags it)))
          (should (member "ideas" (org-air-item-tags it)))
          (should (equal (org-air-item-marker it) (cons titled 1)))
          (should (equal (org-air-item-group it)
                         (file-name-nondirectory
                          (directory-file-name org-air-r53--dir))))
          (should (numberp (org-air-item-activity it)))
          ;; OPENABLE: the standard visit path opens the file at the top.
          (save-window-excursion
            (org-air-visit-item it)
            (should (equal (buffer-file-name) titled))
            (should (= (point) 1)))))
      ;; no #+title: the filename base is the title.
      (let ((items (org-air-query--scan-file untitled)))
        (should (= (length items) 1))
        (should (equal (org-air-item-title (car items)) "untitled")))
      ;; NUL junk and the inbox never synthesise a row.
      (should-not (org-air-query--scan-file junk))
      (should-not (org-air-query--scan-file
                   (expand-file-name "inbox.org" org-air-r53--dir))))))

(ert-deftest org-air-r53-5-notes-section-bounded ()
  "The board shows headingless notes as ONE collapsed count row; expanding
renders only the `org-air-notes-preview-limit' MOST RECENT notes plus the
standard fold row; the task buckets carry ZERO `kind' `file' items (the
GTD board stays a GTD board); `org-air-show-notes-section' nil removes
the section entirely.  Cost is O(preview) — the bound that can never
reintroduce a hang."
  (skip-unless (locate-library "org-air"))
  (let ((specs (list '("inbox.org" . "* TODO Inbox capture :inbox:\n")
                     '("tasks.org" . "* TODO A real task\n"))))
    (dotimes (i 8)
      (push (cons (format "n%02d.org" (1+ i))
                  (format "#+title: R53 note %02d\n\nProse.\n" (1+ i)))
            specs))
    (org-air-r53--with-corpus specs
      ;; recency is the scan-time activity slot = mtime: note 01 newest.
      (dotimes (i 8)
        (set-file-times (expand-file-name (format "n%02d.org" (1+ i))
                                          org-air-r53--dir)
                        (time-subtract (current-time)
                                       (seconds-to-time (* 3600 (1+ i))))))
      (let ((org-air-notes-preview-limit 3)
            (items (org-air-query-items)))
        ;; every note classifies to exactly (notes); no task bucket leaks.
        (dolist (bucket '(inbox attention upcoming high-priority stale))
          (should-not (seq-find (lambda (it) (eq (org-air-item-kind it) 'file))
                                (org-air-view--items-for-bucket bucket items))))
        (should (= (length (org-air-view--items-for-bucket 'notes items)) 8))
        (org-air-r53--with-board
          (setq org-air-view--items items
                org-air-view--items-key (list org-air-files
                                              org-air-inbox-file))
          ;; COLLAPSED: the single count row, zero note rows, no fold row.
          (org-air-view--render items nil)
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match-p "Notes 8" text))
            (should-not (string-match-p "R53 note" text)))
          ;; EXPANDED: exactly the 3 most recent + the fold row.
          (setq org-air-view--expanded-sections '(notes))
          (org-air-view--render items nil)
          (let ((text (substring-no-properties (buffer-string))))
            (dolist (n '("R53 note 01" "R53 note 02" "R53 note 03"))
              (should (string-match-p n text)))
            (dolist (n '("R53 note 04" "R53 note 05" "R53 note 06"
                         "R53 note 07" "R53 note 08"))
              (should-not (string-match-p n text)))
            (should (string-match-p "and 5 more" text)))
          ;; the knob: nil removes the section (the design's UX fork B).
          (setq org-air-view--expanded-sections nil)
          (let ((org-air-show-notes-section nil))
            (org-air-view--render items nil)
            (let ((text (substring-no-properties (buffer-string))))
              (should-not (string-match-p "Notes 8" text))
              (should-not (string-match-p "R53 note" text)))))))))

;;;; -------------------------------------------------------------------
;;;; R53-6 — refile: headingless targets, cache-enumerated data (P4, seam 7)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r53-6-refile-headingless-target-from-index ()
  "Refile target data comes from the board's INDEX, never a re-walk or a
mass file open: `org-air-inbox--target-files' + `--file-candidates' run
with ZERO `org-air-query-files' enumerations and ZERO `find-file-noselect'
calls; the Tags…/Category… vocabulary reads the IN-MEMORY board items
\(zero org-ql scans; the persisted cache is the fallback).  A HEADINGLESS
note file IS a target; `org-air-inbox--read-heading' yields nil for it
with no prompt, and the refile lands at FILE END under the `#+title'
content — the refile-to-file-top contract."
  (skip-unless (locate-library "org-air"))
  (org-air-r53--with-corpus
      '(("ba-note.org" . "#+title: R53 target note\n\nSome prose only.\n")
        ("bb-good.org" . "* TODO Good heading :work:\n")
        ("inbox.org" . "* TODO Move me :inbox:\n"))
    (let* ((note (expand-file-name "ba-note.org" org-air-r53--dir))
           (files (org-air-query-files))
           (items (org-air-query-items))
           (snapshot (org-air-view--mtimes-snapshot files)))
      (org-air-view--cache-write items snapshot)
      (org-air-r53--with-board
        (setq org-air-view--items items
              org-air-view--items-key (list org-air-files org-air-inbox-file)
              org-air-view--items-mtimes snapshot)
        (let ((item (org-air-test-find-item "Move me" items)))
          (should item)
          ;; picker data: from the board's index — zero enumerations, zero
          ;; opens, zero scans (the 271s-class menu-time rescan is dead).
          (org-air-r53--counting
            (let* ((targets (org-air-inbox--target-files item))
                   (cands (org-air-inbox--file-candidates targets)))
              (should (member note targets))
              (should (member "⌂ ba-note.org" cands))
              (should (eq (org-air-inbox--board-items) items)))
            (should (= org-air-r53--enum-calls 0))
            (should (= org-air-r53--ffns-calls 0))
            (should (= org-air-r53--scan-calls 0)))
          ;; the vocabulary the Tags… editor reads, from those items.
          (let ((vocab (delete-dups
                        (seq-mapcat #'org-air-item-tags
                                    (org-air-inbox--board-items)))))
            (should (member "inbox" vocab))
            (should (member "work" vocab)))
          ;; cache fallback: a board with no in-memory items reads the
          ;; persisted cache's items — still zero scans.
          (let ((org-air-view--items nil))
            (org-air-r53--counting
              (let ((cached (org-air-inbox--board-items)))
                (should (consp cached))
                (should (org-air-test-find-item "Move me" cached)))
              (should (= org-air-r53--scan-calls 0))))
          ;; headingless target: no heading prompt (nil, no completing-read).
          (should-not (org-air-inbox--read-heading note))
          ;; the refile-to-file-top contract: the moved heading lands at
          ;; file END, under the #+title content.
          (with-temp-buffer
            (org-air-refile-item item note nil)
            (let ((content (with-temp-buffer
                             (insert-file-contents note)
                             (buffer-string)))
                  (inbox-content (with-temp-buffer
                                   (insert-file-contents org-air-inbox-file)
                                   (buffer-string))))
              (should (string-match "Some prose only\\." content))
              (should (string-match "^\\* TODO Move me" content))
              (should (< (string-match "Some prose only\\." content)
                         (string-match "^\\* TODO Move me" content)))
              (should-not (string-match-p "Move me" inbox-content)))))))))

;;;; -------------------------------------------------------------------
;;;; R53-7 — pacing / never-hang above the sync budget (P1c, seams 4/5)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r53-7-over-budget-paces-never-force-scans ()
  "A change set ABOVE the sync budget paces: `org-air-view--refresh-start'
returns with state `refreshing', the full queue armed and ZERO
synchronous scans (the 271.8s-class cold scan can never run on the main
thread).  Firing the watchdog on that queue runs ZERO scans and leaves
the queue intact (it paces, never force-completes — the old sync drain
WAS the user's 4.5-minute freeze).  Pending input ABORTS a slice with
the queue/accumulator untouched (C-g abortability); a file that keeps
aborting is skip-logged `slow' and dropped (no livelock); the queue then
CONVERGES by budgeted slices to the terminal single-swap."
  (skip-unless (locate-library "org-air"))
  (let ((specs (list '("inbox.org" . "* TODO Inbox capture :inbox:\n"))))
    (dotimes (i 20)
      (push (cons (format "f%02d.org" i) (format "* TODO Item %02d\n" i))
            specs))
    (org-air-r53--with-corpus specs
      (org-air-r53--with-board
        (setq org-air-view--items nil
              org-air-view--items-mtimes nil)   ; everything is "changed"
        (should (> (length (org-air-query-files))
                   org-air-view--refresh-sync-budget))
        ;; (1) start: paced, nothing scanned synchronously.
        (org-air-r53--counting
          (org-air-view--refresh-start)
          (should (= org-air-r53--scan-calls 0)))
        (should (eq org-air-view--refresh-state 'refreshing))
        (should (= (length org-air-view--refresh-queue) 21))
        ;; (2) the watchdog on a >budget queue: ZERO scans, queue intact.
        (let ((queue-before org-air-view--refresh-queue))
          (org-air-r53--counting
            (org-air-view--refresh-watchdog-fire (current-buffer)
                                                 org-air-view--refresh-token)
            (should (= org-air-r53--scan-calls 0)))
          (should (eq org-air-view--refresh-state 'refreshing))
          (should (equal org-air-view--refresh-queue queue-before)))
        ;; (3) C-g abortability: pending input aborts the slice BETWEEN
        ;; files — queue, accumulator and state all untouched.
        (let ((head (car org-air-view--refresh-queue))
              (acc-before org-air-view--refresh-acc)
              (unread-command-events (list ?g)))
          (org-air-view--refresh-run-slice (current-buffer)
                                           org-air-view--refresh-token)
          (should (eq org-air-view--refresh-state 'refreshing))
          (should (= (length org-air-view--refresh-queue) 21))
          (should (eq org-air-view--refresh-acc acc-before))
          ;; (4) no livelock: `org-air-scan-abort-retries' aborts of the
          ;; SAME head file skip-log it `slow' and drop it from the queue.
          (dotimes (_ (1- org-air-scan-abort-retries))
            (org-air-view--refresh-run-slice (current-buffer)
                                             org-air-view--refresh-token))
          (should (= (length org-air-view--refresh-queue) 20))
          (should (eq (org-air-r53--skip-reason head) 'slow))
          (should-not (member head org-air-view--refresh-queue)))
        ;; (5) convergence by pacing: budgeted slices drain to the single
        ;; swap; every un-skipped file's item is on the board.
        (let ((token org-air-view--refresh-token) (n 60))
          (while (and (> n 0) (eq org-air-view--refresh-state 'refreshing))
            (org-air-view--refresh-run-slice (current-buffer) token)
            (cl-decf n)))
        (should-not org-air-view--refresh-state)
        (should (= (length org-air-view--items) 20))
        (should (org-air-test-find-item "Item 19" org-air-view--items))))))

(ert-deftest org-air-r53-8-slices-are-time-budgeted-not-counted ()
  "Slices are TIME-budgeted (`org-air-refresh-slice-budget'), minimum ONE
file: a budget-0 slice consumes exactly 1 queued file and leaves the
machine `refreshing'; a generous-budget slice consumes ALL the cheap
remaining files in ONE call and finishes — even with the OBSOLETED
`org-air-refresh-files-per-slice' bound to 1, proving the fixed per-slice
file count is dead (reverting to it fails: the second slice would consume
one file and leave the queue non-empty)."
  (skip-unless (locate-library "org-air"))
  (let ((specs (list '("inbox.org" . "* TODO Inbox capture :inbox:\n"))))
    (dotimes (i 5)
      (push (cons (format "f%02d.org" i) (format "* TODO Item %02d\n" i))
            specs))
    (org-air-r53--with-corpus specs
      (org-air-r53--with-board
        (setq org-air-view--items nil
              org-air-view--items-mtimes nil)
        (let ((org-air-view--refresh-sync-budget 0))   ; force the paced path
          (org-air-view--refresh-start))
        (should (eq org-air-view--refresh-state 'refreshing))
        (should (= (length org-air-view--refresh-queue) 6))
        ;; budget 0 = the one-file minimum per slice.
        (let ((org-air-refresh-slice-budget 0))
          (org-air-view--refresh-run-slice (current-buffer)
                                           org-air-view--refresh-token))
        (should (eq org-air-view--refresh-state 'refreshing))
        (should (= (length org-air-view--refresh-queue) 5))
        (should (= (length org-air-view--refresh-mtimes) 1))
        ;; a generous budget drains the WHOLE cheap remainder in ONE slice
        ;; — with the obsoleted fixed count bound to 1 and ignored.
        (let ((org-air-refresh-slice-budget 10.0)
              (org-air-refresh-files-per-slice 1))
          (org-air-view--refresh-run-slice (current-buffer)
                                           org-air-view--refresh-token))
        (should-not org-air-view--refresh-state)
        (should (null org-air-view--refresh-queue))
        (should (= (length org-air-view--items) 6))))))

(provide 'org-air-round53-test)
;;; org-air-round53-test.el ends here
