;;; org-air-round60-test.el --- executing ERTs for v0.5 round-60 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.5 round-60 (air/v0.5/org-air-round60-design.org):
;; user-driven exclusion — `org-air-exclude-regexps' (a list of regexps;
;; FILES match as plain absolute paths, DIRECTORIES in directory-name
;; form with a trailing "/") applied at DISCOVERY: a post-filter drops
;; matching files and the `directory-files-recursively' PREDICATE PRUNES
;; matching directories (refused descent, never post-filtered).
;; `org-air-inbox-file' is NEVER excluded — at either level.  The
;; exclude set is the FIFTH `org-air-view--cache-key' element, and
;; `org-air-view--refresh-start' gained the key guard that stops the
;; live-board resurrect of removed rows.
;;
;; All BATCH/headless over temp trees through the REAL discovery layer
;; (`org-air-query-files'), the real scan (`org-air-query-items'), the
;; real board render (`(org-air)' under the anti-tautology guards) and
;; the real refresh machine (`org-air-view--refresh-start').  The spec's
;; ERT seams T1-T11 map onto eight ERTs; revert of each FAILS:
;;
;;   r60-1 (T1/T3) an excluded FILE never appears — not in
;;         `org-air-query-files', not as a scanned item, not as a
;;         rendered board row — while the non-matching sibling survives
;;         everywhere; knob nil (anti-tautology) re-enumerates it.
;;         Reverting the file post-filter FAILS.
;;   r60-2 (T2) an excluded DIRECTORY is PRUNED, not post-filtered —
;;         covering a dot-dir ("\\.git/") and a named dir ("/archive/"):
;;         the trees' *.org never appear AND a `cl-letf' spy on
;;         `file-name-all-completions' (the one listing primitive
;;         `directory-files-recursively' uses) proves the excluded dirs
;;         were never LISTED — the planted deep files (archive/deep/,
;;         .git/objects/) would be walked and counted by a
;;         post-filter-only impl, which passes the membership asserts
;;         and FAILS the spy.
;;   r60-3 (T4/T11) the inbox is NEVER excluded: (a) file level — a
;;         regexp matching the inbox's own name leaves it enumerated
;;         while the same regexp drops the non-inbox twin; (b) ancestor
;;         guard — an inbox INSIDE the excluded tree is still
;;         enumerated (the spine is walked, the non-ancestor subdir
;;         still pruned, every OTHER file in the tree still dropped);
;;         (c) an excluded SOURCE ROOT is silenced whole (nil) unless
;;         the inbox lives inside it — then exactly the inbox.
;;   r60-4 (T6) exclude WINS over a file listed EXPLICITLY in
;;         `org-air-files'; knob nil re-admits it (anti-tautology).
;;   r60-5 (T5/T9) nil excludes = pre-R60 discovery EXACTLY: the result
;;         is `equal' to a direct nil-PREDICATE
;;         `directory-files-recursively' (the dot-dir leak of today
;;         still leaks) and a spy asserts PREDICATE was passed as
;;         LITERAL nil; an all-invalid set ("[") degrades to the same
;;         nil context; never-error — ("[" "/archive/") signals
;;         nowhere, "/archive/" still prunes, and "[" warns exactly
;;         ONCE per session.
;;   r60-6 (T7) the exclude set participates in the cache key: keys
;;         under different sets are un-`equal'; a cache written under
;;         set A hydrates under A (anti-vacuous) and NEVER under B or
;;         nil; a crafted pre-R60 4-element `:key' misses on length.
;;   r60-7 (T8) the refresh-start key guard: flip the exclude on a warm
;;         board and `g' drops the excluded file's rows (items AND
;;         rendered rows), the spy proves the sync path never handed
;;         the excluded path to `org-air-query-items-in-files', and the
;;         NEXT refresh does not resurrect them; the same guard closes
;;         the mid-session `org-air-files' NARROWING hole (the bonus
;;         bug: pre-R60, `file-exists-p' on the vanished branch
;;         resurrected removed rows on EVERY refresh).
;;   r60-8 (T10) symlink-truename dedupe holds with exclusion active,
;;         and exclusion is BY NAME (pre-truename): a symlink whose own
;;         path does not match survives even though its TARGET lies in
;;         the pruned tree (the R53 dedupe truenames actual symlinks,
;;         so the surviving entry is the target truename — exactly
;;         once); a symlink whose OWN path matches is dropped (its
;;         otherwise-unreachable target never surfaces — matching
;;         truenames instead FAILS both directions); and a symlink twin
;;         of an enumerated file still dedupes to ONE entry.
;;
;; REVERT-FAIL: every ERT above is red against the pre-R60 trunk by
;; construction — `org-air-exclude-regexps' and the R60 helpers do not
;; exist there (r60-1/2/3/4/8 enumerate the excluded files, r60-5's
;; leak assert alone passes but its sibling suites don't exist to
;; weaken, r60-6's key is 4 elements, r60-7 resurrects the rows via the
;; vanished/`file-exists-p' branch).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)

(when (locate-library "org-air")
  (require 'org-air))

(defvar org-air-exclude-regexps)
(defvar org-air-cache-file)
(defvar org-air-skip-container-headings)
(defvar org-air-log-cap)

;;;; -------------------------------------------------------------------
;;;; Corpus scaffolding
;;;; -------------------------------------------------------------------

(defvar org-air-r60--dir nil
  "The temp corpus root of the current `org-air-r60--with-tree'.")

(defun org-air-r60--reset-tables ()
  "Clear the GLOBAL query-layer tables (file-meta / visits / denote ids).
Session globals are never cleared by a scan; every test starts and ends
empty so absolute temp paths from another test can never leak."
  (clrhash org-air-query--file-meta)
  (clrhash org-air-query--visits)
  (clrhash org-air-query--denote-id-index)
  (setq org-air-query--link-graph-dirty nil))

(defmacro org-air-r60--with-tree (specs &rest body)
  "Create a temp Org tree from SPECS and run BODY against it.
SPECS is a list of (NAME . CONTENT) files written into a fresh temp
directory (subdirectories created; the root is TRUENAMED so path
spellings are stable).  Binds `org-air-files' to the root,
`org-air-inbox-file' to its inbox.org, `org-air-exclude-regexps' to nil
\(each test re-binds its own set), a temp `org-air-cache-file' and the
120x50 batch viewport.  Starts from EMPTY query tables and cleans up
the tables, the scan work buffer, the board buffer, every
corpus-visiting buffer and the directory."
  (declare (indent 1) (debug t))
  `(let* ((org-air-r60--dir (file-truename (make-temp-file "org-air-r60-" t))))
     (unwind-protect
         (progn
           (org-air-r60--reset-tables)
           (pcase-dolist (`(,name . ,content) ,specs)
             (let ((path (expand-file-name name org-air-r60--dir))
                   (coding-system-for-write 'utf-8-unix))
               (make-directory (file-name-directory path) t)
               (write-region (or content "") nil path nil 'silent)))
           (let ((org-air-files (list org-air-r60--dir))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-r60--dir))
                 (org-air-exclude-regexps nil)
                 (org-air-cache-file
                  (expand-file-name ".cache/board.eld" org-air-r60--dir))
                 (org-air-view-width 120)
                 (org-air-view-height 50))
             (save-window-excursion
               ,@body)))
       (org-air-query-teardown)
       (org-air-r60--reset-tables)
       (when (get-buffer org-air-view-buffer-name)
         (let ((kill-buffer-query-functions nil))
           (kill-buffer org-air-view-buffer-name)))
       (let ((kill-buffer-query-functions nil))
         (dolist (buf (buffer-list))
           (let ((fn (buffer-file-name buf)))
             (when (and fn (string-prefix-p org-air-r60--dir fn))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r60--dir t))))

(defun org-air-r60--file (name)
  "Return the absolute path of corpus file NAME."
  (expand-file-name name org-air-r60--dir))

(defconst org-air-r60--tree-specs
  '(("inbox.org" . "#+title: Inbox\n\n* TODO Inbox capture\n")
    ("top.org" . "* TODO Top task\n")
    ("keep/a.org" . "* TODO Keep task\n")
    ("keep/noise.org" . "* TODO Noise task\n")
    ("archive/b.org" . "* TODO Archived task B\n")
    ("archive/deep/c.org" . "* TODO Archived task C\n")
    ("archive/inbox.org" . "* TODO Non-inbox twin capture\n")
    (".git/objects/d.org" . "* TODO Git object noise\n"))
  "The spec's fixture tree: kept files, a named excluded dir with a
planted DEEP file, a dot-dir with a planted deep file (the leak
verified live today), and a non-inbox file NAMED inbox.org inside
archive/ (the file-level guard's anti-tautology twin).")

(defconst org-air-r60--small-specs
  '(("inbox.org" . "#+title: Inbox\n\n* TODO Inbox capture\n")
    ("top.org" . "* TODO Top task\n[2026-01-05 Mon 09:00]\n")
    ("keep/a.org" . "* TODO Keep task\n[2026-01-05 Mon 09:00]\n")
    ("keep/noise.org" . "* TODO Noise task\n[2026-01-05 Mon 09:00]\n"))
  "A render-sized subset: every task fits the attention section cap, so
board-row assertions can never be masked by the R48-3 fold row.
R93: each task carries a quiet stamp in its own body, months before the
frozen now, so it reaches the aging Needs-attention rule and RENDERS --
an exclusion test that proves nothing when the corpus renders no rows
at all would be vacuous in both directions.")

(defmacro org-air-r60--render-board (&rest body)
  "Render the real board over the bound corpus and run BODY in its buffer.
Frozen clock (`org-air-test-now'), the anti-tautology render guards;
the board buffer is killed afterwards so a warm in-buffer item cache
never leaks between renders."
  (declare (indent 0) (debug t))
  `(org-air-viewport-test--with-frozen-now
     (unwind-protect
         (org-air-viewport-test--with-render-guards
           (org-air)
           (with-current-buffer org-air-view-buffer-name
             ,@body))
       (when (get-buffer org-air-view-buffer-name)
         (let ((kill-buffer-query-functions nil))
           (kill-buffer org-air-view-buffer-name))))))

(defun org-air-r60--board-titles ()
  "Return the titles of every rendered item ROW (the `org-air-item' walk).
Property-based, not substring-based, so chrome text can never mask a
missing/present row assertion."
  (let ((pos (point-min)) titles)
    (while (setq pos (text-property-not-all pos (point-max)
                                            'org-air-item nil))
      (let ((item (get-text-property pos 'org-air-item)))
        (cl-pushnew (org-air-item-title item) titles :test #'equal))
      (setq pos (next-single-property-change pos 'org-air-item
                                             nil (point-max))))
    (nreverse titles)))

(defmacro org-air-r60--spying-listings (listed &rest body)
  "Run BODY with directory LISTINGS recorded into the variable LISTED.
`directory-files-recursively' lists a directory through exactly one
primitive — `file-name-all-completions' — so the recorded DIR arguments
\(normalised to absolute, no trailing slash) are proof of which
directories were LISTED: a pruned directory never appears."
  (declare (indent 1) (debug t))
  `(cl-letf* ((org-air-r60--fnac (symbol-function 'file-name-all-completions))
              ((symbol-function 'file-name-all-completions)
               (lambda (string dir)
                 (push (directory-file-name (expand-file-name dir)) ,listed)
                 (funcall org-air-r60--fnac string dir))))
     ,@body))

(defun org-air-r60--count (path files)
  "Return how many times PATH occurs in FILES."
  (seq-count (lambda (f) (equal f path)) files))

;;;; -------------------------------------------------------------------
;;;; r60-1 — T1/T3: an excluded FILE never appears; the sibling survives
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r60-1-excluded-file-never-appears ()
  "T1/T3: a file whose absolute path matches an exclude regexp is gone
from `org-air-query-files', from the scanned items AND from the
rendered board, while the non-matching sibling (same directory) and the
rest of the tree survive in enumeration order.  Knob nil re-enumerates
it (anti-tautology: the exclusion, not the corpus, hides it).
Reverting the R60-2 file post-filter FAILS."
  (skip-unless (locate-library "org-air"))
  (org-air-r60--with-tree org-air-r60--small-specs
    (let ((noise (org-air-r60--file "keep/noise.org")))
      (let ((org-air-exclude-regexps '("noise\\.org\\'")))
        ;; Discovery: the file is not enumerated; the sibling is.
        (let ((files (org-air-query-files)))
          (should-not (member noise files))
          (should (member (org-air-r60--file "keep/a.org") files))
          (should (member (org-air-r60--file "top.org") files))
          (should (member (org-air-r60--file "inbox.org") files)))
        ;; Scan: no item sources from the excluded file; the sibling's does.
        (let ((items (org-air-query-items)))
          (should-not (seq-some
                       (lambda (it) (equal (org-air-item-file it) noise))
                       items))
          (should-not (org-air-test-find-item "Noise task" items))
          (should (org-air-test-find-item "Keep task" items)))
        ;; Board: the real render carries the sibling row, never the
        ;; excluded file's row (every task fits the attention cap here).
        (org-air-r60--render-board
          (let ((titles (org-air-r60--board-titles)))
            (should (member "Keep task" titles))
            (should (member "Top task" titles))
            (should-not (member "Noise task" titles)))))
      ;; Anti-tautology: with the knob off the file IS discovered.
      (should (member noise (org-air-query-files))))))

;;;; -------------------------------------------------------------------
;;;; r60-2 — T2: excluded directories are PRUNED, never descended
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r60-2-excluded-dir-pruned-never-listed ()
  "T2: \"/archive/\" (named dir) and \"\\\\.git/\" (dot-dir) silence
their whole trees — every *.org under them absent, including the
planted DEEP files (archive/deep/c.org, .git/objects/d.org) — AND the
listing spy proves neither tree was ever LISTED: pruning, not
post-filtering.  A post-filter-only impl passes the membership asserts
and FAILS the spy.  The walk itself is real (the root and keep/ were
listed) and the survivors keep enumeration order.  Reverting the
PREDICATE prune FAILS."
  (skip-unless (locate-library "org-air"))
  (org-air-r60--with-tree org-air-r60--tree-specs
    (let ((org-air-exclude-regexps '("/archive/" "\\.git/"))
          (listed nil)
          files)
      (org-air-r60--spying-listings listed
        (setq files (org-air-query-files)))
      ;; The excluded trees' *.org never appear…
      (should-not (member (org-air-r60--file "archive/b.org") files))
      (should-not (member (org-air-r60--file "archive/deep/c.org") files))
      (should-not (member (org-air-r60--file "archive/inbox.org") files))
      (should-not (member (org-air-r60--file ".git/objects/d.org") files))
      ;; …the survivors do, in enumeration order (dedupe untouched)…
      (should (equal files
                     (list (org-air-r60--file "keep/a.org")
                           (org-air-r60--file "keep/noise.org")
                           (org-air-r60--file "inbox.org")
                           (org-air-r60--file "top.org"))))
      ;; …and the excluded dirs were never LISTED — the deep subdirs
      ;; (which the predicate is never even consulted for once the
      ;; parent is pruned) included.  This is the pruning-not-post-
      ;; filter fence: walking them would record them here.
      (dolist (dir '("archive" "archive/deep" ".git" ".git/objects"))
        (should-not (member (org-air-r60--file dir) listed)))
      ;; The spy saw the real walk: root + the kept subdir were listed.
      (should (member (directory-file-name org-air-r60--dir) listed))
      (should (member (org-air-r60--file "keep") listed)))))

;;;; -------------------------------------------------------------------
;;;; r60-3 — T4/T11: the inbox is NEVER excluded (guard wins everywhere)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r60-3-inbox-never-excluded ()
  "T4/T11: `org-air-inbox-file' survives every exclusion route.
\(a) file level: a regexp matching the inbox's own basename leaves the
inbox enumerated (and scanned) while the SAME regexp drops the
non-inbox twin archive/inbox.org — the guard, not a weak regexp, is
what protects it.  (b) ancestor guard: an inbox INSIDE the excluded
tree is still enumerated — the spine directory is walked (listed) while
the non-ancestor archive/deep/ is still pruned (never listed) and every
OTHER file in the tree is still dropped.  (c) an excluded SOURCE ROOT
is silenced whole when the inbox lives elsewhere, and yields EXACTLY
the inbox when it lives inside.  Reverting the file-level guard FAILS
\(a); reverting the ancestor guard FAILS (b)/(c)."
  (skip-unless (locate-library "org-air"))
  (org-air-r60--with-tree org-air-r60--tree-specs
    ;; (a) file-level guard.
    (let ((org-air-exclude-regexps '("inbox\\.org\\'")))
      (let ((files (org-air-query-files)))
        (should (member (org-air-r60--file "inbox.org") files))
        (should-not (member (org-air-r60--file "archive/inbox.org") files)))
      (should (org-air-test-find-item "Inbox capture"
                                      (org-air-query-items))))
    ;; (b) ancestor guard: inbox inside the excluded tree.
    (let ((org-air-inbox-file (org-air-r60--file "archive/inbox.org"))
          (org-air-exclude-regexps '("/archive/"))
          (listed nil)
          files)
      (org-air-r60--spying-listings listed
        (setq files (org-air-query-files)))
      ;; The inbox is enumerated; every OTHER archive file is dropped.
      (should (member (org-air-r60--file "archive/inbox.org") files))
      (should-not (member (org-air-r60--file "archive/b.org") files))
      (should-not (member (org-air-r60--file "archive/deep/c.org") files))
      ;; The guard is surgical: the spine (archive/) WAS walked, the
      ;; non-ancestor subtree (archive/deep/) still pruned.
      (should (member (org-air-r60--file "archive") listed))
      (should-not (member (org-air-r60--file "archive/deep") listed)))
    ;; (c) excluded source root.
    (let ((org-air-files (list (org-air-r60--file "archive")))
          (org-air-exclude-regexps '("/archive/")))
      ;; Inbox outside the source: the whole source is silenced.
      (should (null (org-air-query-files)))
      ;; Inbox inside: exactly the inbox, nothing else.
      (let ((org-air-inbox-file (org-air-r60--file "archive/inbox.org")))
        (should (equal (org-air-query-files)
                       (list (org-air-r60--file "archive/inbox.org"))))))))

;;;; -------------------------------------------------------------------
;;;; r60-4 — T6: exclude WINS over an explicit org-air-files listing
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r60-4-exclude-wins-over-explicit-listing ()
  "T6: a file listed EXPLICITLY in `org-air-files' that matches an
exclude is DROPPED — the exclude list is the single kill switch, so an
entry reachable both explicitly and via a directory source can never
behave differently per route.  Knob nil re-admits the explicit listing
\(anti-tautology).  Reverting the precedence FAILS."
  (skip-unless (locate-library "org-air"))
  (org-air-r60--with-tree org-air-r60--small-specs
    (let ((org-air-files (list (org-air-r60--file "keep/noise.org")
                               (org-air-r60--file "keep/a.org")
                               (org-air-r60--file "inbox.org"))))
      (let ((org-air-exclude-regexps '("noise\\.org\\'")))
        (should (equal (org-air-query-files)
                       (list (org-air-r60--file "keep/a.org")
                             (org-air-r60--file "inbox.org")))))
      ;; Anti-tautology: the explicit listing works when the knob is off.
      (should (member (org-air-r60--file "keep/noise.org")
                      (org-air-query-files))))))

;;;; -------------------------------------------------------------------
;;;; r60-5 — T5/T9: nil (or all-invalid) excludes = pre-R60 EXACTLY
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r60-5-nil-and-invalid-excludes-are-pre-r60-exactly ()
  "T5/T9: with the knob off the discovery path is today's byte-for-byte.
nil excludes: `org-air-query-files' is `equal' to the direct
nil-PREDICATE `directory-files-recursively' enumeration (no symlinks in
this corpus, so modulo-dedupe = exactly equal), the dot-dir file that
leaks today STILL leaks (.git/objects/d.org — old behaviour preserved
when the knob is off), and the spy pins PREDICATE = LITERAL nil at
every call (never a trivially-true closure).  An all-invalid set
\(\"[\") degrades to the same nil context.  Never-error: (\"[\"
\"/archive/\") signals nowhere, \"/archive/\" still prunes, and the
broken \"[\" warns exactly ONCE per session across repeated
enumerations.  Reverting the nil-context seam or the validation FAILS."
  (skip-unless (locate-library "org-air"))
  (org-air-r60--with-tree org-air-r60--tree-specs
    (let ((direct (directory-files-recursively
                   org-air-r60--dir "\\.org\\(?:\\.gpg\\)?\\'" nil)))
      ;; nil knob: identical result, literal-nil PREDICATE.
      (let ((org-air-exclude-regexps nil)
            (preds nil)
            files)
        (cl-letf* ((orig (symbol-function 'directory-files-recursively))
                   ((symbol-function 'directory-files-recursively)
                    (lambda (dir regexp &optional inc pred follow)
                      (push pred preds)
                      (funcall orig dir regexp inc pred follow))))
          (setq files (org-air-query-files)))
        (should (equal files direct))
        ;; The dot-dir leak of today still leaks — knob off preserves
        ;; the old behaviour exactly, warts included.
        (should (member (org-air-r60--file ".git/objects/d.org") files))
        (should (member (org-air-r60--file "archive/deep/c.org") files))
        ;; PREDICATE was LITERAL nil on every call (top level and the
        ;; recursion into each subdirectory).
        (should preds)
        (should (seq-every-p #'null preds)))
      ;; All-invalid set: the compiled context degrades to nil — same
      ;; result, still a literal-nil PREDICATE, no signal.
      (let ((org-air-query--exclude-warned nil)
            (org-air-exclude-regexps '("["))
            (preds nil)
            files)
        (cl-letf* ((orig (symbol-function 'directory-files-recursively))
                   ((symbol-function 'directory-files-recursively)
                    (lambda (dir regexp &optional inc pred follow)
                      (push pred preds)
                      (funcall orig dir regexp inc pred follow))))
          (setq files (org-air-query-files)))
        (should (equal files direct))
        (should (seq-every-p #'null preds)))
      ;; Never-error: the broken regexp is dropped-and-warned ONCE while
      ;; the valid one still prunes; repeated enumeration never re-warns
      ;; and never signals.
      (let ((org-air-query--exclude-warned nil)
            (org-air-exclude-regexps '("[" "/archive/"))
            (warnings 0))
        (cl-letf* ((orig (symbol-function 'message))
                   ((symbol-function 'message)
                    (lambda (fmt &rest args)
                      (when (and (stringp fmt)
                                 (string-match-p "invalid exclude regexp" fmt))
                        (cl-incf warnings))
                      (apply orig fmt args))))
          (let ((files (org-air-query-files)))
            (should-not (member (org-air-r60--file "archive/b.org") files))
            (should-not (member (org-air-r60--file "archive/deep/c.org")
                                files))
            (should (member (org-air-r60--file "top.org") files)))
          (org-air-query-files))
        (should (= warnings 1))))))

;;;; -------------------------------------------------------------------
;;;; r60-6 — T7: the exclude set is the FIFTH cache-key element
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r60-6-exclude-set-is-fifth-cache-key-element ()
  "T7: toggling the exclude set can never serve a stale file set.
Keys under exclude sets A and B (and nil) are pairwise un-`equal', with
the live set as the FIFTH element; a cache written under A hydrates
under A (anti-vacuous — the misses below are the KEY, not the version
or shape) and returns nil from both `org-air-view--cache-read' and
`org-air-view--cache-load' under B and under nil; a crafted pre-R60
4-element `:key' with the CURRENT version also misses (length
inequality — no version bump needed).  Reverting the key extension
hydrates the stale set and FAILS.
R61 re-bless (air/v0.5/org-air-round61-design.org R61-2, honest — no
conjunct weakened): the key is SIX elements now — the exclude set STAYS
the FIFTH (every detection/hydrate/miss conjunct above holds verbatim)
and `org-air-log-cap' follows as the SIXTH (tracked live, a flip is a
different key); the crafted short-`:key' misses now cover BOTH legacy
shapes — the pre-R60 4-element and the pre-R61 5-element key — each on
length inequality alone.
R77 re-bless (air/v0.1/org-air-round77-design.org, honest — no
conjunct weakened): the key is SEVEN elements now — the exclude set
STAYS the FIFTH (every detection/hydrate/miss conjunct above holds
verbatim, the log-cap stays the SIXTH) and `org-air-task-requires-todo'
follows as the SEVENTH (tracked live, a flip is a different key); the
crafted short-`:key' misses now cover ALL THREE legacy shapes — the
pre-R60 4-element, the pre-R61 5-element AND the pre-R77 6-element
key — each on length inequality alone."
  (skip-unless (locate-library "org-air"))
  (org-air-r60--with-tree org-air-r60--tree-specs
    ;; The key detects: pairwise distinct, fifth element = the live set
    ;; (R61: `org-air-log-cap' is the sixth; R77: seven elements total —
    ;; `org-air-task-requires-todo' is the seventh, and the exclude set
    ;; KEEPS its fifth seat).
    (let ((key-nil (org-air-view--cache-key))
          (key-a (let ((org-air-exclude-regexps '("/archive/")))
                   (org-air-view--cache-key)))
          (key-b (let ((org-air-exclude-regexps '("noise\\.org\\'")))
                   (org-air-view--cache-key))))
      (should (= (length key-a) 7))
      (should (equal (nth 4 key-a) '("/archive/")))
      (should (equal (nth 4 key-nil) nil))
      (should-not (equal key-a key-b))
      (should-not (equal key-a key-nil))
      (should-not (equal key-b key-nil))
      ;; R61: the sixth element is the live cap; a flip is a new key.
      (should (eq (nth 5 key-a) org-air-log-cap))
      (let ((org-air-log-cap 123))
        (should (equal (nth 5 (org-air-view--cache-key)) 123))
        (should-not (equal (org-air-view--cache-key) key-nil)))
      ;; R77: the seventh element is the live knob; a flip is a new key.
      (should (eq (nth 6 key-a) org-air-task-requires-todo))
      (should (null (nth 6 key-nil)))
      (let ((org-air-task-requires-todo t))
        (should (eq (nth 6 (org-air-view--cache-key)) t))
        (should-not (equal (org-air-view--cache-key) key-nil))))
    ;; A cache written under exclude set A…
    (let ((org-air-exclude-regexps '("/archive/")))
      (let ((files (org-air-query-files)))
        (org-air-view--cache-write (org-air-query-items)
                                   (org-air-view--mtimes-snapshot files)))
      ;; …hydrates under A (anti-vacuous)…
      (should (org-air-view--cache-read)))
    ;; …and NEVER under B or under nil.
    (let ((org-air-exclude-regexps '("noise\\.org\\'")))
      (should-not (org-air-view--cache-read))
      (should-not (org-air-view--cache-load)))
    (should-not (org-air-view--cache-read))
    (should-not (org-air-view--cache-load))
    ;; A pre-R60 4-element :key — and a pre-R61 5-element and pre-R77
    ;; 6-element :key — miss on length even with the CURRENT version and
    ;; the CURRENT leading elements (no version bump needed for any).
    (let ((org-air-exclude-regexps '("/archive/")))
      (dolist (drop '(3 2 1))
        (let ((print-length nil) (print-level nil))
          (write-region
           (prin1-to-string
            (list :version org-air-view--cache-version
                  :key (butlast (org-air-view--cache-key) drop)
                  :mtimes nil :file-meta nil :visits nil :items nil))
           nil (expand-file-name org-air-cache-file) nil 'silent))
        (should-not (org-air-view--cache-read))
        (should-not (org-air-view--cache-load))))))

;;;; -------------------------------------------------------------------
;;;; r60-7 — T8: the refresh-start key guard never resurrects rows
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r60-7-refresh-guard-never-resurrects ()
  "T8 + the bonus bug: narrowing the file set mid-session sticks.
A warm board holds rows for keep/noise.org under a matching baseline.
Flip `org-air-exclude-regexps' to match it and refresh: the key guard
voids the stale merge inputs, the machine re-derives, and afterwards no
item (and no rendered row) for the excluded file remains; the spy
proves `org-air-query-items-in-files' was never handed the excluded
path (the R60-3 intersect, not `file-exists-p' — the file still exists
on disk).  The NEXT refresh does not resurrect it (pre-R60 the
vanished/exists branch re-merged the rows on EVERY refresh).  The same
guard closes the `org-air-files' NARROWING hole: un-listing top.org
mid-session drops its rows for good while the un-excluded noise file
honestly returns (the cold re-derive is real, not a filter of old
rows).  Reverting the key guard AND the intersect FAILS."
  (skip-unless (locate-library "org-air"))
  (org-air-r60--with-tree org-air-r60--small-specs
    (let ((org-air-view-buffer-name "*org-air-r60*"))
      (unwind-protect
          (with-current-buffer (get-buffer-create "*org-air-r60*")
            (unless (derived-mode-p 'org-air-view-mode)
              (org-air-view-mode))
            ;; Warm board under NO exclusions: noise rows present.
            (let ((files (org-air-query-files)))
              (setq org-air-view--items (org-air-query-items)
                    org-air-view--items-key (org-air-view--cache-key)
                    org-air-view--items-mtimes
                    (org-air-view--mtimes-snapshot files)
                    org-air-view--classify-cache nil))
            (org-air-view--render org-air-view--items nil)
            (should (org-air-test-find-item "Noise task"
                                            org-air-view--items))
            (should (member "Noise task" (org-air-r60--board-titles)))
            ;; Flip the exclude mid-session and press `g'.
            (let ((noise (org-air-r60--file "keep/noise.org"))
                  (scan-args nil))
              (cl-letf* ((orig (symbol-function 'org-air-query-items-in-files))
                         ((symbol-function 'org-air-query-items-in-files)
                          (lambda (fs &rest rest)
                            (push (copy-sequence fs) scan-args)
                            (apply orig fs rest))))
                (let ((org-air-exclude-regexps '("noise\\.org\\'")))
                  (org-air-view--refresh-start)
                  ;; The small set takes the sync path: resolved at return.
                  (should-not (eq org-air-view--refresh-state 'refreshing))
                  ;; No noise item, no noise row — and the scan was real
                  ;; (anti-vacuous) yet never handed the excluded path.
                  (should-not (org-air-test-find-item
                               "Noise task" org-air-view--items))
                  (should-not (seq-some
                               (lambda (it)
                                 (equal (org-air-item-file it) noise))
                               org-air-view--items))
                  (should-not (member "Noise task"
                                      (org-air-r60--board-titles)))
                  (should scan-args)
                  (dolist (args scan-args)
                    (should-not (member noise args)))
                  ;; The NEXT refresh must not resurrect the rows (the
                  ;; pre-R60 trunk re-merged them on every `g').
                  (org-air-view--refresh-start)
                  (should-not (eq org-air-view--refresh-state 'refreshing))
                  (should-not (org-air-test-find-item
                               "Noise task" org-air-view--items))
                  (should-not (member "Noise task"
                                      (org-air-r60--board-titles))))))
            ;; The bonus bug: NARROWING `org-air-files' mid-session (the
            ;; exclude flip above left its key stamped, so this is a
            ;; second, independent key mismatch).
            (let ((org-air-files (list (org-air-r60--file "keep")
                                       (org-air-r60--file "inbox.org")))
                  (top (org-air-r60--file "top.org"))
                  (scan-args nil))
              (cl-letf* ((orig (symbol-function 'org-air-query-items-in-files))
                         ((symbol-function 'org-air-query-items-in-files)
                          (lambda (fs &rest rest)
                            (push (copy-sequence fs) scan-args)
                            (apply orig fs rest))))
                (org-air-view--refresh-start)
                (should-not (eq org-air-view--refresh-state 'refreshing))
                ;; The un-listed file's rows are gone — and stay gone.
                (should-not (org-air-test-find-item "Top task"
                                                    org-air-view--items))
                (should-not (member "Top task" (org-air-r60--board-titles)))
                (should scan-args)
                (dolist (args scan-args)
                  (should-not (member top args)))
                ;; Honest cold re-derive: the no-longer-excluded noise
                ;; file (still under keep/) returns — the guard re-scans
                ;; the CURRENT config, it does not just filter old rows.
                (should (org-air-test-find-item "Noise task"
                                                org-air-view--items))
                (org-air-view--refresh-start)
                (should-not (org-air-test-find-item "Top task"
                                                    org-air-view--items)))))
        (when (get-buffer "*org-air-r60*")
          (let ((kill-buffer-query-functions nil))
            (kill-buffer "*org-air-r60*")))))))

;;;; -------------------------------------------------------------------
;;;; r60-8 — T10: symlink by-name exclusion + the R53 dedupe still hold
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r60-8-symlink-by-name-dedupe-holds ()
  "T10: exclusion is BY NAME (pre-truename); the R53 dedupe is untouched.
With (\"/archive/\" \"xlink\\\\.org\\\\='\") active over a tree carrying
three symlinks: (1) keep/link.org -> archive/deep/c.org SURVIVES — its
own enumerated path matches nothing, the pruned target path is never
enumerated (no dedupe collision), and the R53 symlink dedupe truenames
it, so the note surfaces as its target truename EXACTLY once (matching
truenames instead of names would drop it — reverting the pre-truename
ruling FAILS here); (2) keep/xlink.org -> archive/b.org is DROPPED by
its OWN matching path — b.org is otherwise unreachable (its direct
spelling is pruned), so its truename appearing would prove the by-name
drop reverted; (3) keep/dup.org -> top.org still dedupes with the
directly-enumerated top.org to ONE entry — the dedupe holds with
exclusion active.  No raw symlink spelling ever leaks into the result."
  (skip-unless (locate-library "org-air"))
  (org-air-r60--with-tree org-air-r60--tree-specs
    (make-symbolic-link (org-air-r60--file "archive/deep/c.org")
                        (org-air-r60--file "keep/link.org"))
    (make-symbolic-link (org-air-r60--file "archive/b.org")
                        (org-air-r60--file "keep/xlink.org"))
    (make-symbolic-link (org-air-r60--file "top.org")
                        (org-air-r60--file "keep/dup.org"))
    ;; Anti-tautology baseline: knob off, every target enumerates once
    ;; (the symlink twins dedupe into the direct spellings).
    (let ((files (org-air-query-files)))
      (should (= 1 (org-air-r60--count
                    (org-air-r60--file "archive/deep/c.org") files)))
      (should (= 1 (org-air-r60--count
                    (org-air-r60--file "archive/b.org") files)))
      (should (= 1 (org-air-r60--count (org-air-r60--file "top.org")
                                       files))))
    (let ((org-air-exclude-regexps '("/archive/" "xlink\\.org\\'")))
      (let ((files (org-air-query-files)))
        ;; (1) The non-matching symlink into the pruned tree survives —
        ;; by NAME — and dedupes to its target truename exactly once.
        (should (= 1 (org-air-r60--count
                      (org-air-r60--file "archive/deep/c.org") files)))
        ;; (2) The symlink whose OWN path matches is dropped; its
        ;; otherwise-unreachable target never surfaces.
        (should (= 0 (org-air-r60--count
                      (org-air-r60--file "archive/b.org") files)))
        ;; (3) The dedupe holds: one top.org, never two.
        (should (= 1 (org-air-r60--count (org-air-r60--file "top.org")
                                         files)))
        ;; No raw symlink spelling leaks (actual symlinks are always
        ;; truenamed by the R53 dedupe, exclusion active or not).
        (should-not (seq-some
                     (lambda (f)
                       (string-match-p "\\(?:^\\|/\\)\\(?:x?link\\|dup\\)\\.org\\'" f))
                     files))
        ;; The kept plain files are untouched by the symlink traffic.
        (should (member (org-air-r60--file "keep/a.org") files))
        (should (member (org-air-r60--file "inbox.org") files))))))

(provide 'org-air-round60-test)
;;; org-air-round60-test.el ends here
