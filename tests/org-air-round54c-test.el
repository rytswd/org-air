;;; org-air-round54c-test.el --- executing ERTs for v0.5 round-54 part 2 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.5 round-54 part 2 (air/v0.5/org-air-round54-
;; design.org §R54-3): the Revisit (evergreen notes) view — new module
;; org-air-revisit.el over the R54-2 file-meta scope, plus the query
;; layer's link graph and bounded visit ledger and the board/project
;; entry points.  All BATCH/headless; every view assertion drives the
;; REAL entry point (`org-air-revisit') and the real keymaps
;; (`key-binding' -> `call-interactively'), so reverting the matching
;; impl seam fails it:
;;
;;   R54c-3a  the Revisit scope is KNOWLEDGE notes only
;;            (`org-air-revisit-types' default): task and journal FILES
;;            are absent while both headed and headingless knowledge
;;            files render one row per FILE; RET on a row opens the
;;            file at its top (the cons-marker path).  Reverting the
;;            `:ntype' scope filter fails.
;;   R54c-3b  DEFAULT sort = dustiest first: the shared R22-3 sort spec
;;            seeds (age . ascending) and rows render oldest
;;            attention-age (mtime) on top — the corpus mtimes are
;;            deliberately NOT in file-name order, so a sort revert (or
;;            a name-order fallback) fails.
;;   R54c-3c  the VISIT LEDGER (opt-in, D2): at the DEFAULT nil the
;;            revisit RET open records NOTHING and age stays pure
;;            mtime; with the knob t the same open (and the board
;;            `org-air-visit-item' path) records the file and the
;;            attention-age shifts to max(mtime, visit) — the visited
;;            note re-sorts to the bottom; the ledger PERSISTS in the
;;            cache and is BOUNDED (pruned to the enumerated files at
;;            write: a vanished file's entry never survives the
;;            roundtrip; size <= file count).
;;   R54c-3d  ORPHANS mode (`m' once) = notes with no note-links either
;;            way (`org-air-revisit-orphan-rule' 'disconnected):
;;            denote:/id:/file: links all resolve in the scan-time link
;;            graph (`https:' noise ignored), inversion counts correct,
;;            linked notes absent, disconnected notes present; the
;;            one-direction rules follow the knob.
;;   R54c-3e  SPACED mode (`m' twice) = a bounded deterministic daily
;;            handful: exactly `org-air-revisit-daily-count' rows, no
;;            fold row, STABLE within a pinned day, rotating on the
;;            next day, covering the scope exactly once over ceil(N/K)
;;            days (N=15, K=5: an exact 3-day partition).
;;   R54c-3f  ENTRY points, knob-gated: `N' resolves to
;;            `org-air-revisit' on BOTH the board and project maps and
;;            the board Notes count-row answers RET with the Revisit
;;            view (item rows keep the pane — the F4 doorway is the
;;            HEADING only); with `org-air-use-default-keybindings'
;;            nil every one of those keys is gone.
;;   R54c-3g  DATA-PURE + BOUNDED: a 300-entry synthetic file-meta
;;            table (files that do NOT exist) renders exactly
;;            `org-air-revisit-page-limit' rows + the `…and N more'
;;            fold row; TAB on the fold extends by ONE page; the whole
;;            build + paging opens NO file (`find-file-noselect' /
;;            `find-file' spies = 0) and grows the buffer list by
;;            nothing but the revisit buffer itself.
;;   R54c-3h  the INBOX is a triage queue, never a Revisit row (the
;;            R54c review blocker): a PROSE (taskless) inbox and an
;;            EMPTY `#+title: inbox' file both type `knowledge' at the
;;            file level (asserted — the anti-tautology half) yet are
;;            ABSENT from `org-air-revisit--scope-entries' and the
;;            rendered rows.  Reverting the scope predicate fails.
;;   R54c-3i  a PART-1-shaped v4 cache (file-metas lacking the link
;;            keys, written by the REAL cache writer) hydrates ZERO
;;            metas — the reopened Revisit re-scans real metas and
;;            ORPHANS never reports the linked pair (the false
;;            all-orphans regression); and `org-air-query-visits-
;;            hydrate' takes max — an older cached epoch never
;;            clobbers a newer in-session visit.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)     ; frozen clock harness
(require 'org-air-round35-test)         ; the keybindings knob macro

(when (locate-library "org-air")
  (require 'org-air))

;;;; -------------------------------------------------------------------
;;;; Corpus scaffolding
;;;; -------------------------------------------------------------------

(defvar org-air-r54c--dir nil
  "The temp corpus directory of the current `org-air-r54c--with-corpus'.")

(defun org-air-r54c--reset-tables ()
  "Clear the GLOBAL query-layer tables the Revisit view reads.
File-meta, the visit ledger and the denote-ID index are session globals
\(never cleared by the scan), so every test starts and ends empty —
entries from another test's absolute temp paths must never leak into
`org-air-revisit--scope-entries'."
  (clrhash org-air-query--file-meta)
  (clrhash org-air-query--visits)
  (clrhash org-air-query--denote-id-index)
  (setq org-air-query--link-graph-dirty nil))

(defmacro org-air-r54c--with-corpus (specs &rest body)
  "Create a temp Org corpus from SPECS and run BODY against it.
SPECS is a list of (NAME . CONTENT) files written into a fresh temp
directory; NAME may carry subdirectories (created).  Binds
`org-air-files' to the directory, `org-air-inbox-file' to its inbox.org,
a temp `org-air-cache-file' and the 120x50 batch viewport.  Starts from
EMPTY query tables and cleans up the tables, the revisit buffer, the
scan work buffer, every corpus-visiting buffer and the directory."
  (declare (indent 1) (debug t))
  `(let* ((org-air-r54c--dir (make-temp-file "org-air-r54c-" t)))
     (unwind-protect
         (progn
           (org-air-r54c--reset-tables)
           (pcase-dolist (`(,name . ,content) ,specs)
             (let ((path (expand-file-name name org-air-r54c--dir))
                   (coding-system-for-write 'utf-8-unix))
               (make-directory (file-name-directory path) t)
               (write-region (or content "") nil path nil 'silent)))
           (let ((org-air-files (list org-air-r54c--dir))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-r54c--dir))
                 (org-air-cache-file
                  (expand-file-name ".cache/board.eld" org-air-r54c--dir))
                 (org-air-view-width 120)
                 (org-air-view-height 50))
             ,@body))
       (org-air-query-teardown)
       (org-air-r54c--reset-tables)
       (let ((kill-buffer-query-functions nil))
         (when (get-buffer org-air-revisit-buffer-name)
           (kill-buffer org-air-revisit-buffer-name))
         (dolist (buf (buffer-list))
           (let ((fn (buffer-file-name buf)))
             (when (and fn (string-prefix-p org-air-r54c--dir fn))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r54c--dir t))))

(defun org-air-r54c--file (name)
  "Return the absolute path of corpus file NAME."
  (expand-file-name name org-air-r54c--dir))

(defun org-air-r54c--age (name days)
  "Set corpus file NAME's mtime to DAYS days before `org-air-test-now'."
  (set-file-times (org-air-r54c--file name)
                  (time-subtract org-air-test-now (days-to-time days))))

(defmacro org-air-r54c--in-revisit (&rest body)
  "Open the Revisit view through the REAL entry point; run BODY inside it.
Windows are restored afterwards; the buffer itself lives on (the corpus
macro kills it), so a later `with-current-buffer' can keep asserting on
the same session — exactly the R26-5 idempotent re-entry contract."
  (declare (indent 0) (debug t))
  `(save-window-excursion
     (org-air-revisit)
     (with-current-buffer org-air-revisit-buffer-name
       ,@body)))

(defun org-air-r54c--row-files ()
  "Return the rendered note rows' FILEs, in buffer order.
Reads the `org-air-revisit' row property the renderer stamps — the same
identity RET resolves."
  (let ((pos (point-min)) files)
    (while (setq pos (text-property-not-all pos (point-max)
                                            'org-air-revisit nil))
      (push (car (get-text-property pos 'org-air-revisit)) files)
      (setq pos (next-single-property-change pos 'org-air-revisit
                                             nil (point-max))))
    (nreverse files)))

(defun org-air-r54c--goto-row (file)
  "Move point onto FILE's rendered row; assert it exists."
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

(defun org-air-r54c--dispatch (key)
  "Dispatch KEY through the live keymap (the real command-loop seam).
Returns the command that ran, so callers can pin the binding too."
  (let ((cmd (key-binding (kbd key))))
    (should (commandp cmd))
    (call-interactively cmd)
    cmd))

(defun org-air-r54c--fold-row-pos ()
  "Return the buffer position of the `…and N more' fold row, or nil."
  (text-property-not-all (point-min) (point-max) 'org-air-more-row nil))

;;;; -------------------------------------------------------------------
;;;; R54c-3a — the scope: knowledge notes only, one row per file
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r54c-3a-revisit-surfaces-knowledge-only ()
  "The Revisit view lists KNOWLEDGE files only; RET opens one (seam 3a).
Both the headingless (#+title) and the headed prose knowledge files
render — one row per FILE — while the task file and the journal file
are ABSENT although both sit in the same file-meta table (typed away by
the R54-2 model, the anti-tautology half).  The inbox is PROSE captures
\(taskless — the R54c review edge): it types `knowledge' like any
evergreen yet stays absent, excluded by the scope predicate alone.  RET
on a row is the single user-initiated open: the file, point at its top
\(the cons-marker path).  Reverting the `:ntype' scope filter — or the
inbox predicate — fails."
  (skip-unless (locate-library "org-air"))
  (org-air-r54c--with-corpus
      '(("notes/alpha.org" . "#+title: Alpha evergreen\n\nProse only.\n")
        ("notes/bravo.org" . "* Bravo pruning wisdom :garden:\nProse.\n")
        ("tasks.org" . "* TODO Real board task\nSCHEDULED: <2026-06-16 Tue>\n")
        ("2026-06-14.org" . "* Journal entry\nDear diary.\n")
        ("inbox.org" .
         "#+title: inbox\n\n* a half-formed thought\nProse capture.\n"))
    (org-air-r54c--in-revisit
      (should (derived-mode-p 'org-air-revisit-mode))
      (let ((files (org-air-r54c--row-files)))
        (should (member (org-air-r54c--file "notes/alpha.org") files))
        (should (member (org-air-r54c--file "notes/bravo.org") files))
        (should-not (member (org-air-r54c--file "tasks.org") files))
        (should-not (member (org-air-r54c--file "2026-06-14.org") files))
        (should-not (member (org-air-r54c--file "inbox.org") files))
        (should (= (length files) 2)))
      ;; Anti-tautology: the excluded files ARE in file-meta — the scope
      ;; filter (not a lossy scan) is what keeps them off the surface.
      (should (eq (plist-get (org-air-query-file-meta
                              (org-air-r54c--file "tasks.org"))
                             :ntype)
                  'task))
      (should (eq (plist-get (org-air-query-file-meta
                              (org-air-r54c--file "2026-06-14.org"))
                             :ntype)
                  'journal))
      ;; The PROSE inbox types knowledge like any evergreen — ONLY the
      ;; scope predicate (view policy, R54c) keeps the triage queue out.
      (should (eq (plist-get (org-air-query-file-meta
                              (org-air-r54c--file "inbox.org"))
                             :ntype)
                  'knowledge))
      ;; The title cell reads file-meta (#+title), never the file.
      (should (string-match-p "Alpha evergreen"
                              (substring-no-properties (buffer-string))))
      ;; RET opens the note at its top — the (FILE . POS 1) visit path.
      (org-air-r54c--goto-row (org-air-r54c--file "notes/alpha.org"))
      (should (eq (org-air-r54c--dispatch "RET") 'org-air-revisit-open))
      (should (equal (buffer-file-name)
                     (org-air-r54c--file "notes/alpha.org")))
      (should (= (point) 1)))))

;;;; -------------------------------------------------------------------
;;;; R54c-3b — default sort: dustiest first
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r54c-3b-default-sort-dustiest-first ()
  "DEFAULT sort = oldest attention-age on top (seam 3b, USER-RULED D2).
The shared R22-3 sort spec seeds (age . ascending) and the rows render
in strict mtime order, oldest first.  The corpus mtimes are staggered
deliberately OUT of file-name order, so a revert to the scope's
file-name base order (or a descending flip) fails."
  (skip-unless (locate-library "org-air"))
  (org-air-r54c--with-corpus
      '(("alpha.org" . "#+title: Alpha note\n\nProse.\n")
        ("bravo.org" . "#+title: Bravo note\n\nProse.\n")
        ("charlie.org" . "#+title: Charlie note\n\nProse.\n")
        ("delta.org" . "#+title: Delta note\n\nProse.\n")
        ("echo.org" . "#+title: Echo note\n\nProse.\n")
        ("inbox.org" . "#+title: inbox\n\n* TODO Capture\n"))
    ;; Staggered ages, NOT in name order: delta > bravo > charlie >
    ;; alpha > echo is the expected dusty order.
    (org-air-r54c--age "alpha.org" 10)
    (org-air-r54c--age "bravo.org" 200)
    (org-air-r54c--age "charlie.org" 50)
    (org-air-r54c--age "delta.org" 400)
    (org-air-r54c--age "echo.org" 1)
    (org-air-r54c--in-revisit
      ;; The seeded shared sort spec IS the default: age ascending.
      (should (eq org-air-view--sort-key 'age))
      (should (eq org-air-view--sort-direction 'ascending))
      (should (equal (org-air-r54c--row-files)
                     (mapcar #'org-air-r54c--file
                             '("delta.org" "bravo.org" "charlie.org"
                               "alpha.org" "echo.org"))))
      ;; The age chip renders the dusty vocabulary from the mtime slot.
      (should (string-match-p "dusty"
                              (substring-no-properties (buffer-string)))))))

;;;; -------------------------------------------------------------------
;;;; R54c-3c — the opt-in visit ledger: record, shift, persist, bound
;;;; -------------------------------------------------------------------

(defun org-air-r54c--mtime-age-days (entry)
  "Return ENTRY's PURE mtime age in whole days (ledger ignored)."
  (floor (/ (- (float-time) (or (plist-get (cdr entry) :mtime) 0.0))
            86400)))

(ert-deftest org-air-r54c-3c-visit-ledger-opt-in-persists-bounded ()
  "The visit ledger records org-air opens, shifts age, persists, bounded (3d).
DEFAULT nil: the revisit RET open records NOTHING and age stays pure
mtime (the D2 ruling).  Knob t: the SAME RET open (and the board
`org-air-visit-item' path) records the file; attention-age becomes
max(mtime, visit) — the 90-day note reads fresh and re-sorts to the
BOTTOM while its mtime stays old.  The ledger roundtrips through the
cache (`:visits') with ZERO rescans, and the write-time prune IS the
bound: a vanished file's entry never persists and the hydrated ledger
can never exceed the enumerated file count."
  (skip-unless (locate-library "org-air"))
  (org-air-r54c--with-corpus
      '(("dusty-a.org" . "#+title: Dusty alpha\n\nProse.\n")
        ("dusty-b.org" . "#+title: Dusty beta\n\nProse.\n")
        ("inbox.org" . "#+title: inbox\n\n* TODO Capture\n"))
    (org-air-r54c--age "dusty-a.org" 90)
    (org-air-r54c--age "dusty-b.org" 30)
    (let* ((a (org-air-r54c--file "dusty-a.org"))
           (b (org-air-r54c--file "dusty-b.org"))
           (items (org-air-query-items))
           (files (org-air-query-files)))
      ;; DEFAULT (knob nil): the open path records NOTHING...
      (should-not org-air-revisit-visit-ledger)
      (org-air-r54c--in-revisit
        (org-air-r54c--goto-row a)
        (should (eq (org-air-r54c--dispatch "RET") 'org-air-revisit-open)))
      (should-not (org-air-query-note-visit a))
      ;; ...and age stays pure mtime.
      (let ((entry (cons a (org-air-query-file-meta a))))
        (should (>= (org-air-revisit--entry-age-days entry) 90)))
      ;; OPT-IN (knob t): the same open records + shifts the age.
      (let ((org-air-revisit-visit-ledger t))
        (save-window-excursion
          (with-current-buffer org-air-revisit-buffer-name
            (org-air-revisit--render-current)
            ;; pre-visit dusty order: a (90d) above b (30d).
            (should (equal (org-air-r54c--row-files) (list a b)))
            (org-air-r54c--goto-row a)
            (org-air-r54c--dispatch "RET")))
        (let ((visit (org-air-query-note-visit a))
              (entry (cons a (org-air-query-file-meta a))))
          (should (floatp visit))
          ;; age = max(mtime, visit): the 90-day note reads fresh now...
          (should (< (org-air-revisit--entry-age-days entry) 1))
          ;; ...while the mtime itself is untouched (the ledger shifts).
          (should (>= (org-air-r54c--mtime-age-days entry) 90)))
        ;; The visited note SINKS: freshest attention-age sorts last.
        (with-current-buffer org-air-revisit-buffer-name
          (org-air-revisit--render-current)
          (should (equal (org-air-r54c--row-files) (list b a))))
        ;; The board open path (S-RET / g RET verb) records too.
        (save-window-excursion
          (org-air-visit-item (org-air-test-find-item "Dusty beta" items)))
        (should (org-air-query-note-visit b))
        ;; BOUNDED at write: a vanished file's entry is pruned.
        (org-air--note-visited "/r54c-gone/vanished.org")
        (should (org-air-query-note-visit "/r54c-gone/vanished.org"))
        (let ((alist (org-air-query-visits-alist files)))
          (should (<= (length alist) (length files)))
          (should (assoc a alist))
          (should-not (assoc "/r54c-gone/vanished.org" alist)))
        ;; PERSISTS: cache write, cold tables, warm reopen — no rescan.
        (org-air-view--cache-write items (org-air-view--mtimes-snapshot files))
        (let ((recorded (org-air-query-note-visit a)))
          (clrhash org-air-query--visits)
          (clrhash org-air-query--file-meta)
          (let ((kill-buffer-query-functions nil))
            (kill-buffer org-air-revisit-buffer-name))
          (let ((scans 0))
            (cl-letf* ((orig (symbol-function 'org-air-query-items))
                       ((symbol-function 'org-air-query-items)
                        (lambda (&rest args)
                          (cl-incf scans) (apply orig args))))
              (org-air-r54c--in-revisit
                (should (member a (org-air-r54c--row-files)))))
            ;; warm open = cache hydrate, never a scan.
            (should (= scans 0)))
          (should (equal (org-air-query-note-visit a) recorded))
          ;; The prune survived the roundtrip: the cap held.
          (should-not (org-air-query-note-visit "/r54c-gone/vanished.org"))
          (should (<= (hash-table-count org-air-query--visits)
                      (length files))))))))

;;;; -------------------------------------------------------------------
;;;; R54c-3d — ORPHANS mode over the scan-time link graph
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r54c-3d-orphans-mode-over-link-graph ()
  "ORPHANS = notes with no note-links either way; graph from the scan (3e/2e).
The fixture web: hub links out via denote:, id: AND file: (plus https:
noise); spoke/target/prose each catch one inbound edge.  The graph
resolves every note-link kind to FILES (https: ignored), the inversion
counts land, and the `m'-cycled ORPHANS surface shows EXACTLY the
disconnected notes — linked pairs absent; the one-direction rules
follow `org-air-revisit-orphan-rule'.  Reverting the mode filter (or
the scan-time extraction) fails."
  (skip-unless (locate-library "org-air"))
  (org-air-r54c--with-corpus
      '(("hub.org" .
         "* Hub of the garden\nSee [[denote:20260102T130000][spoke]],\n\
[[id:ABC-123][target]], [[file:prose-note.org][prose]] and\n\
[[https://example.com][the web]].\n")
        ("20260102T130000--spoke__kb.org" .
         "* Spoke idea\nProse, no links out.\n")
        ("target.org" .
         "* Target ideas\n:PROPERTIES:\n:ID: ABC-123\n:END:\nProse.\n")
        ("prose-note.org" . "#+title: Prose note\n\nJust prose.\n")
        ("island.org" . "* Island idea\nNo links at all.\n")
        ("web-only.org" .
         "* Web linked idea\nSee [[https://example.com][the web]].\n")
        ("inbox.org" . "#+title: inbox\n\n* TODO Capture\n"))
    (let ((hub (org-air-r54c--file "hub.org"))
          (spoke (org-air-r54c--file "20260102T130000--spoke__kb.org"))
          (target (org-air-r54c--file "target.org"))
          (prose (org-air-r54c--file "prose-note.org"))
          (island (org-air-r54c--file "island.org"))
          (web-only (org-air-r54c--file "web-only.org")))
      (org-air-r54c--in-revisit
        ;; The resolved graph: all three note-link kinds land on FILES,
        ;; https: never enters, inversion counts are exact.
        (let ((hub-out (plist-get (org-air-query-file-meta hub) :links-out)))
          (should (member spoke hub-out))
          (should (member target hub-out))
          (should (member prose hub-out))
          (should-not (seq-find (lambda (l) (string-match-p "https" l))
                                hub-out)))
        (should (= (plist-get (org-air-query-file-meta spoke) :links-in) 1))
        (should (= (plist-get (org-air-query-file-meta target) :links-in) 1))
        (should (= (plist-get (org-air-query-file-meta prose) :links-in) 1))
        (should (= (plist-get (org-air-query-file-meta island) :links-in) 0))
        (should-not (plist-get (org-air-query-file-meta island) :links-out))
        ;; `m' once: ALL -> ORPHANS; the header names the mode.
        (should (eq (org-air-r54c--dispatch "m")
                    'org-air-revisit-cycle-surface))
        (should (eq org-air-revisit--surface 'orphans))
        (should (string-match-p "orphans"
                                (substring-no-properties (buffer-string))))
        ;; Default 'disconnected: only the notes with NO link either way.
        (let ((files (org-air-r54c--row-files)))
          (should (member island files))
          (should (member web-only files))   ; https: is not a note link
          (should-not (member hub files))    ; links out
          (should-not (member spoke files))  ; linked to (denote:)
          (should-not (member target files)) ; linked to (id:)
          (should-not (member prose files))) ; linked to (file:)
        ;; One-direction rules follow the knob.
        (let ((org-air-revisit-orphan-rule 'no-outbound))
          (org-air-revisit--render-current)
          (let ((files (org-air-r54c--row-files)))
            (should (member spoke files))    ; inbound-only, no outbound
            (should-not (member hub files))))
        (let ((org-air-revisit-orphan-rule 'either))
          (org-air-revisit--render-current)
          (let ((files (org-air-r54c--row-files)))
            (should (member hub files))      ; nothing links TO the hub
            (should (member spoke files))))))))

;;;; -------------------------------------------------------------------
;;;; R54c-3e — SPACED: bounded deterministic daily rotation
;;;; -------------------------------------------------------------------

(defun org-air-r54c--rows-on-day (days)
  "Re-render the current Revisit buffer DAYS after `org-air-test-now'.
Returns the rendered row files — the deterministic rotation's window."
  (cl-letf (((symbol-function 'current-time)
             (lambda () (time-add org-air-test-now (days-to-time days)))))
    (org-air-revisit--render-current)
    (org-air-r54c--row-files)))

(ert-deftest org-air-r54c-3e-spaced-mode-bounded-daily-rotation ()
  "SPACED = exactly K notes, stable within a day, rotating daily (3f).
With the clock pinned, repeated renders show the SAME
`org-air-revisit-daily-count' rows and never a fold row; advancing a
day rotates the window; over ceil(N/K) pinned days (N=15, K=5: an
exact partition) every scope note appears exactly once.  Reverting the
determinism (or the K clamp) fails."
  (skip-unless (locate-library "org-air"))
  (org-air-r54c--with-corpus
      (cons '("inbox.org" . "#+title: inbox\n\n* TODO Capture\n")
            (mapcar (lambda (i)
                      (cons (format "note-%02d.org" i)
                            (format "#+title: Garden note %02d\n\nProse.\n"
                                    i)))
                    (number-sequence 1 15)))
    (org-air-viewport-test--with-frozen-now
      (org-air-r54c--in-revisit
        (should (= org-air-revisit-daily-count 5))
        ;; `m' twice: ALL -> ORPHANS -> SPACED (the real key cycle).
        (should (eq (org-air-r54c--dispatch "m")
                    'org-air-revisit-cycle-surface))
        (org-air-r54c--dispatch "m")
        (should (eq org-air-revisit--surface 'spaced))
        (should (string-match-p "spaced"
                                (substring-no-properties (buffer-string))))
        (let ((day0 (org-air-r54c--row-files)))
          ;; BOUNDED: exactly K rows, and never a fold row in SPACED.
          (should (= (length day0) 5))
          (should-not (org-air-r54c--fold-row-pos))
          ;; STABLE within the day: a re-render shows the SAME handful.
          (org-air-revisit--render-current)
          (should (equal (org-air-r54c--row-files) day0))
          ;; ROTATION: fresh handfuls on the next days; the three days
          ;; cover the 15-note scope exactly once each.
          (let* ((day1 (org-air-r54c--rows-on-day 1))
                 (day2 (org-air-r54c--rows-on-day 2))
                 (all (append day0 day1 day2)))
            (should (= (length day1) 5))
            (should (= (length day2) 5))
            (should-not (equal (sort (copy-sequence day1) #'string<)
                               (sort (copy-sequence day0) #'string<)))
            (should (= (length (seq-uniq all)) 15))))))))

;;;; -------------------------------------------------------------------
;;;; R54c-3f — entry points: `N' + the Notes count-row RET, knob-gated
;;;; -------------------------------------------------------------------

(defun org-air-r54c--goto-notes-heading ()
  "Move point onto the board's Notes section HEADING row; assert it."
  (goto-char (point-min))
  (let (found)
    (while (and (not found) (not (eobp)))
      (if (and (eq (org-air-view--line-section) 'notes)
               (not (org-air-view--row-property 'org-air-item)))
          (setq found t)
        (forward-line 1)))
    (should found)))

(defun org-air-r54c--goto-item-row (title)
  "Move point onto the board item row whose title contains TITLE."
  (let ((pos (point-min)) target)
    (while (and (not target)
                (setq pos (text-property-not-all pos (point-max)
                                                 'org-air-item nil)))
      (let ((item (get-text-property pos 'org-air-item)))
        (if (string-match-p title (or (org-air-item-title item) ""))
            (setq target pos)
          (setq pos (next-single-property-change pos 'org-air-item
                                                 nil (point-max))))))
    (should target)
    (goto-char target)))

(ert-deftest org-air-r54c-3f-entry-points-knob-gated ()
  "`N' AND the board Notes count-row RET open Revisit; knob-gated (F4).
Real key dispatch on a live rendered board: RET on the Notes section
HEADING runs `org-air-revisit' and never the pane, RET on an ITEM row
keeps the pane and never Revisit (the doorway is the heading only);
`N' resolves to `org-air-revisit' on BOTH the board and the project
maps and really opens the view.  With
`org-air-use-default-keybindings' nil every one of those keys is gone
from the maps (the R35-1 gate)."
  (skip-unless (locate-library "org-air"))
  (org-air-r54c--with-corpus
      '(("note.org" . "#+title: Quiet garden note\n\nProse.\n")
        ("tasks.org" . "* TODO Board task\nSCHEDULED: <2026-06-16 Tue>\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let ((items (org-air-query-items))
          (org-air-view-buffer-name "*org-air-r54c-board*"))
      (unwind-protect
          (with-current-buffer (get-buffer-create org-air-view-buffer-name)
            (unless (derived-mode-p 'org-air-view-mode) (org-air-view-mode))
            (setq org-air-view--items items
                  org-air-view--items-key (list org-air-files
                                                org-air-inbox-file)
                  org-air-view--classify-cache nil)
            (org-air-view--render items nil)
            ;; The Notes count row rendered (the R53 P3 single row).
            (org-air-r54c--goto-notes-heading)
            (should (string-match-p "Notes 1" (or (thing-at-point 'line t)
                                                  "")))
            ;; Counter stubs: the heading RET is Revisit and ONLY that;
            ;; an item-row RET keeps the pane and never opens Revisit.
            (let ((pane 0) (revisit 0))
              (cl-letf (((symbol-function 'org-air-view-pane)
                         (lambda (&rest _) (cl-incf pane)))
                        ((symbol-function 'org-air-revisit)
                         (lambda (&rest _) (cl-incf revisit))))
                (org-air-r54c--goto-notes-heading)
                (should (eq (key-binding (kbd "RET"))
                            'org-air-view-pane-return))
                (call-interactively (key-binding (kbd "RET")))
                (should (= revisit 1))
                (should (= pane 0))
                (org-air-r54c--goto-item-row "Board task")
                (call-interactively (key-binding (kbd "RET")))
                (should (= pane 1))
                (should (= revisit 1))))
            ;; End-to-end: the real RET on the heading opens the view.
            (should-not (get-buffer org-air-revisit-buffer-name))
            (save-window-excursion
              (org-air-r54c--goto-notes-heading)
              (call-interactively (key-binding (kbd "RET"))))
            (let ((buf (get-buffer org-air-revisit-buffer-name)))
              (should buf)
              (with-current-buffer buf
                (should (derived-mode-p 'org-air-revisit-mode))
                (should (member (org-air-r54c--file "note.org")
                                (org-air-r54c--row-files)))
                ;; The EMPTY inbox types knowledge yet is NOT a row —
                ;; the R54c scope predicate (the review's flagged edge).
                (should-not (member (org-air-r54c--file "inbox.org")
                                    (org-air-r54c--row-files)))))
            ;; `N' — the symmetric switch — from the BOARD...
            (should (eq (key-binding (kbd "N")) 'org-air-revisit))
            ;; ...and from the PROJECT map, dispatched for real.
            (save-window-excursion
              (with-temp-buffer
                (org-air-project-mode)
                (should (eq (key-binding (kbd "N")) 'org-air-revisit))
                (call-interactively (key-binding (kbd "N")))))
            (should (get-buffer org-air-revisit-buffer-name))
            ;; The R35-1 gate: knob nil clears EVERY entry key.
            (org-air-r35--with-knob nil
              (should-not (eq (lookup-key org-air-view-mode-map (kbd "N"))
                              'org-air-revisit))
              (should-not (eq (lookup-key org-air-project-mode-map (kbd "N"))
                              'org-air-revisit))
              (should-not (eq (lookup-key org-air-view-mode-map (kbd "RET"))
                              'org-air-view-pane-return))
              (should-not (eq (lookup-key org-air-revisit-mode-map
                                          (kbd "RET"))
                              'org-air-revisit-open))
              (should-not (eq (lookup-key org-air-revisit-mode-map (kbd "m"))
                              'org-air-revisit-cycle-surface))))
        (when (get-buffer "*org-air-r54c-board*")
          (let ((kill-buffer-query-functions nil))
            (kill-buffer "*org-air-r54c-board*")))))))

;;;; -------------------------------------------------------------------
;;;; R54c-3g — data-pure and bounded at large N
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r54c-3g-render-data-pure-and-bounded ()
  "Building + paging Revisit opens NO file; bounded at large N (3b/3c).
A 300-entry SYNTHETIC file-meta table — paths that do not exist, so any
render cell falling back to a file read errors or trips the spies —
renders exactly `org-air-revisit-page-limit' (200) rows plus the
`…and 100 more' fold row; TAB on the fold extends by ONE page (all 300,
fold gone).  Across the whole build + paging `find-file-noselect' and
`find-file' run ZERO times and the visible buffer list grows by nothing
but the revisit buffer.  Reverting the page clamp or any cell to a file
read fails."
  (skip-unless (locate-library "org-air"))
  (unwind-protect
      (progn
        (org-air-r54c--reset-tables)
        (dotimes (i 300)
          (puthash (format "/r54c-synth/garden/note-%03d.org" i)
                   (list :title (format "Synth note %03d" i)
                         :org-title (format "Synth note %03d" i)
                         :tags '("kb") :ntype 'knowledge
                         :mtime (+ 1.0e9 (* i 3600.0)) :created nil
                         :ids nil :links-raw nil :links-out nil :links-in 0)
                   org-air-query--file-meta))
        (let ((org-air-view-width 120)
              (org-air-view-height 50)
              (before (seq-remove (lambda (b)
                                    (string-prefix-p " " (buffer-name b)))
                                  (buffer-list)))
              (ffns 0) (ff 0))
          (cl-letf* ((ffns-orig (symbol-function 'find-file-noselect))
                     ((symbol-function 'find-file-noselect)
                      (lambda (&rest args)
                        (cl-incf ffns) (apply ffns-orig args)))
                     (ff-orig (symbol-function 'find-file))
                     ((symbol-function 'find-file)
                      (lambda (&rest args)
                        (cl-incf ff) (apply ff-orig args))))
            (save-window-excursion
              (org-air-revisit)
              (with-current-buffer org-air-revisit-buffer-name
                ;; BOUNDED: exactly one page + the fold row at 300 > 200.
                (should (= org-air-revisit-page-limit 200))
                (should (= (length (org-air-r54c--row-files)) 200))
                (should (string-match-p
                         "and 100 more"
                         (substring-no-properties (buffer-string))))
                ;; TAB on the fold row extends by ONE page (R51-3 verb).
                (goto-char (org-air-r54c--fold-row-pos))
                (should (eq (key-binding (kbd "TAB"))
                            'org-air-revisit-toggle-more))
                (call-interactively (key-binding (kbd "TAB")))
                (should (= (length (org-air-r54c--row-files)) 300))
                (should-not (org-air-r54c--fold-row-pos)))))
          ;; DATA-PURE: the whole build + paging opened NO file.
          (should (= ffns 0))
          (should (= ff 0))
          (dolist (buf (seq-remove (lambda (b)
                                     (string-prefix-p " " (buffer-name b)))
                                   (buffer-list)))
            (should (or (memq buf before)
                        (eq buf (get-buffer org-air-revisit-buffer-name)))))))
    (org-air-r54c--reset-tables)
    (when (get-buffer org-air-revisit-buffer-name)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer org-air-revisit-buffer-name)))))

;;;; -------------------------------------------------------------------
;;;; R54c-3h — the inbox is a triage queue, never a Revisit row
;;;; -------------------------------------------------------------------

(defun org-air-r54c--assert-inbox-absent ()
  "Assert the corpus inbox types `knowledge' yet never reaches Revisit.
The anti-tautology half first: the inbox IS in file-meta, typed
`knowledge' by the content-derived F7 rule (no inbox case there), so
only the scope predicate — view policy — can be what excludes it.  Then
the seam both ways: `org-air-revisit--scope-entries' and the rendered
rows hold exactly the one evergreen note, never the inbox."
  (org-air-r54c--in-revisit
    (should (eq (plist-get (org-air-query-file-meta
                            (org-air-r54c--file "inbox.org"))
                           :ntype)
                'knowledge))
    (should (equal (mapcar #'car (org-air-revisit--scope-entries))
                   (list (org-air-r54c--file "note.org"))))
    (should (equal (org-air-r54c--row-files)
                   (list (org-air-r54c--file "note.org"))))))

(ert-deftest org-air-r54c-3h-inbox-absent-from-revisit ()
  "The inbox never surfaces in Revisit although it types knowledge (R54c).
The review-blocked edge: a PROSE (taskless) inbox — `#+title' plus
captures that are not yet tasks — and an EMPTY `#+title: inbox' file
both type `knowledge' at the FILE level (F7 has no inbox case;
asserted), yet BOTH are absent from `org-air-revisit--scope-entries'
AND from the rendered rows: the inbox is a triage queue with its own
surface (the Inbox bucket + capture flow), excluded as view policy at
the scope seat.  Reverting the scope predicate (the inbox reappears as
an evergreen row) fails both halves."
  (skip-unless (locate-library "org-air"))
  ;; A PROSE inbox: #+title + captures, no task heading anywhere.
  (org-air-r54c--with-corpus
      '(("note.org" . "#+title: Quiet garden note\n\nProse.\n")
        ("inbox.org" .
         "#+title: inbox\n\n* a half-formed thought\nnot yet a task\n"))
    (org-air-r54c--assert-inbox-absent))
  ;; An EMPTY inbox: nothing but the #+title line.
  (org-air-r54c--with-corpus
      '(("note.org" . "#+title: Quiet garden note\n\nProse.\n")
        ("inbox.org" . "#+title: inbox\n"))
    (org-air-r54c--assert-inbox-absent)))

;;;; -------------------------------------------------------------------
;;;; R54c-3i — part-1 v4 cache hydrate guard + the visits-hydrate max
;;;; -------------------------------------------------------------------

(defun org-air-r54c--strip-link-shape (meta)
  "Return a copy of META lacking the R54-3 link keys — the part-1 shape."
  (let (out)
    (while meta
      (let ((key (pop meta)) (val (pop meta)))
        (unless (memq key '(:ids :links-raw :links-out :links-in))
          (setq out (nconc out (list key val))))))
    out))

(ert-deftest org-air-r54c-3i-part1-cache-hydrate-guard-and-visit-max ()
  "A part-1 v4 cache hydrates NO metas; visits-hydrate takes max (R54c).
The review's MAJOR: a v4 cache written by part-1 code carries file-metas
WITHOUT the link keys — hydrated as-is they read as false ALL-orphans
and get re-persisted by the next warm write.  Forge exactly that cache
with the REAL writer (link keys stripped from the live table first),
then prove the guard: `org-air-query-file-meta-hydrate' fills ZERO
entries from it, so the reopened Revisit re-scans REAL metas (batch:
inline) and ORPHANS shows only the true island — never the linked
hub/spoke pair.  Then the ledger nit: `org-air-query-visits-hydrate'
takes max — an OLDER cached epoch never clobbers a newer in-session
visit, while fresh entries and NEWER cached epochs still land.
Reverting the `:links-out' hydrate guard (or the max) fails."
  (skip-unless (locate-library "org-air"))
  (org-air-r54c--with-corpus
      '(("hub.org" . "* Hub idea\nSee [[file:spoke.org][spoke]].\n")
        ("spoke.org" . "#+title: Spoke idea\n\nProse.\n")
        ("island.org" . "#+title: Island idea\n\nProse.\n")
        ("inbox.org" . "#+title: inbox\n\n* TODO Capture\n"))
    (let* ((hub (org-air-r54c--file "hub.org"))
           (spoke (org-air-r54c--file "spoke.org"))
           (island (org-air-r54c--file "island.org"))
           (items (org-air-query-items))
           (files (org-air-query-files)))
      (org-air-query-link-graph-ensure)
      ;; Sanity: the live scan resolved the pair (hub → spoke).
      (should (member spoke
                      (plist-get (org-air-query-file-meta hub)
                                 :links-out)))
      ;; Forge the part-1 shape IN the live table, then persist it with
      ;; the REAL writer — byte-for-byte what a part-1 binary's cache
      ;; write produced (its serialiser had no link keys to prune).
      (maphash (lambda (file meta)
                 (puthash file (org-air-r54c--strip-link-shape meta)
                          org-air-query--file-meta))
               org-air-query--file-meta)
      (setq org-air-query--link-graph-dirty nil)
      (org-air-view--cache-write items (org-air-view--mtimes-snapshot
                                        files))
      ;; Anti-tautology: the cache on disk REALLY carries part-1 metas
      ;; — one per file, none with the `:links-out' shape.
      (let ((metas (plist-get (org-air-view--cache-read) :file-meta)))
        (should (= (length metas) (length files)))
        (dolist (entry metas)
          (should-not (plist-member (cdr entry) :links-out))))
      ;; THE GUARD: hydrating that cache fills NOTHING — the table
      ;; starts empty instead of link-less.
      (org-air-r54c--reset-tables)
      (org-air-query-file-meta-hydrate
       (plist-get (org-air-view--cache-read) :file-meta))
      (should (zerop (hash-table-count org-air-query--file-meta)))
      ;; …so the reopened Revisit re-fills REAL metas and ORPHANS never
      ;; reports the linked pair (the false all-orphans regression).
      (org-air-r54c--in-revisit
        (should (member spoke
                        (plist-get (org-air-query-file-meta hub)
                                   :links-out)))
        (should (eq (org-air-r54c--dispatch "m")
                    'org-air-revisit-cycle-surface))
        (should (eq org-air-revisit--surface 'orphans))
        (let ((rows (org-air-r54c--row-files)))
          (should (member island rows))
          (should-not (member hub rows))
          (should-not (member spoke rows))))
      ;; VISITS-HYDRATE takes max: an in-session visit survives an
      ;; older cached epoch; fresh entries and newer epochs still land.
      (let ((org-air-revisit-visit-ledger t))
        (org-air--note-visited hub)
        (let ((recorded (org-air-query-note-visit hub)))
          (should (floatp recorded))
          (org-air-query-visits-hydrate
           (list (cons hub (- recorded 3600.0)) (cons island 123.0)))
          (should (= (org-air-query-note-visit hub) recorded))
          (should (= (org-air-query-note-visit island) 123.0))
          (org-air-query-visits-hydrate
           (list (cons hub (+ recorded 3600.0))))
          (should (= (org-air-query-note-visit hub)
                     (+ recorded 3600.0))))))))

(provide 'org-air-round54c-test)
;;; org-air-round54c-test.el ends here
