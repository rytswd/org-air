;;; org-air-round83-test.el --- executing ERTs for round-83 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for round-83 (air/v0.1/org-air-round83-design.org):
;; a one-key BACKLOG toggle (`b') that ack-and-defers an item off the
;; Needs-attention surfaces onto a Backlog lens via an org-native
;; `:backlog:' tag (`org-air-backlog-tag').  The classify gate
;; `org-air-classify--backlog-p' routes a board-active tagged item OFF
;; the four task buckets + Inbox into a single reachable `backlog'
;; bucket (conditional bottom section + a rail Summary count); an
;; `is:backlog' filter token is the bucket-exact twin of `#backlog';
;; the write reuses `org-air-view--at-item-source' (R68 log + atomic +
;; R73 ring, prompt-free); the repaint is R53 in-place (slot mutate +
;; classify-memo remhash, NO rescan); the tag NAME is a classify-memo
;; coherence-key input, NOT a scan cache-key input (no version bump).
;;
;; The spec's thirteen seams r83-1..r83-13 map onto the ERTs below:
;;
;;   r83-1   `b' defers off attention: the source gains `:backlog:', the
;;           cached item's tags carry it, and classify => (backlog).
;;   r83-2   `b' again round-trips; a co-tag survives BOTH toggles.
;;   r83-3   is:backlog = EXACTLY the backlog set (a done tagged heading
;;           fails the board-active gate; a plain overdue item fails).
;;   r83-4   Summary Backlog count + section iff non-empty; backlog-free
;;           board renders NO Backlog row and NO Backlog section.
;;   r83-5   a dated backlog heading keeps its day-view row + calendar
;;           dot; a dateless one stays reachable via all-items/#backlog.
;;   r83-6   board-active precedence: a DONE :backlog: heading is ZERO
;;           buckets, NOT (backlog).
;;   r83-7   no rescan on toggle (R53): the `org-air-query-items' counter
;;           stays at zero while the row moves buckets.
;;   r83-8   classify-memo drop: without the D3 remhash the memo would
;;           serve the stale attention list.
;;   r83-9   edit ring in-place: `u' reverts the source tag, `U' re-adds
;;           it; the record is the in-place class, other tags kept.
;;   r83-10  never-error: no item => user-error; a mid-refresh stale file
;;           soft-errors and writes nothing; a body signal rolls back
;;           the atomic-change-group with no save and no ring push.
;;   r83-11  `b' is installer-owned (R35-1): bound on the view + review
;;           maps under the default, unbound under the knob nil, absent
;;           from the shared core map.
;;   r83-12  cache-key placement: `org-air-view--cache-key' is UNCHANGED
;;           across a backlog-tag rename; the classify-memo key DIFFERS.
;;   r83-13  process-inbox `[b]' defer disposition drops the item out of
;;           the inbox bucket (single-home ruling).
;;
;; The test seat's STRENGTHENING seams r83-14..r83-17 (audit additions,
;; each revert-RED-driven vs a targeted mutation of the R83 code) close
;; the four gaps the shipped 13 left open against the brief:
;;
;;   r83-14  is:backlog COMPOSES under M-/ (the AND/OR combinator) with
;;           #tag / todo: / scheduled: / due: — the deferred set narrows
;;           under `all' and broadens under `any' (the shipped r83-3
;;           only tested is:backlog / #backlog in ISOLATION).
;;   r83-15  the Summary Backlog count + section materialise on the LIVE
;;           in-place repaint (0 re-queries) when `b' defers, and retract
;;           when `b' un-defers (r83-4 tested only a STATIC board).
;;   r83-16  R68 write discipline: the ONLY on-disk delta from `b' is the
;;           heading tag — no LOGBOOK drawer / `- State' note / CLOSED
;;           stamp, body byte-identical; and `org-air-item-archive' stays
;;           `archive' (structural) alongside the backlog `in-place'
;;           record (the deferred write did not demote the structural class).
;;   r83-17  R77 compose + dead item: an `org-air-task-requires-todo'
;;           keyword-less placeholder demotes off the task buckets (a
;;           knowledge note) yet stays day/calendar reachable; `b' in the
;;           DAY view never errors and never phantoms a backlog entry
;;           (backlog is board-active only); `b' on a DELETED source file
;;           degrades to a message with no ring push (never-error law).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)

(when (locate-library "org-air")
  (require 'org-air)
  (require 'org-air-review)
  (require 'org-air-calendar))

;;;; -------------------------------------------------------------------
;;;; Corpus scaffolding (the r73/r77 house idiom)
;;;; -------------------------------------------------------------------

(defvar org-air-r83--dir nil
  "The temp corpus directory of the current `org-air-r83--with-corpus'.")

(defun org-air-r83--reset-tables ()
  "Clear the GLOBAL query-layer tables the note surfaces read."
  (when (fboundp 'org-air-query-teardown)
    (clrhash org-air-query--file-meta)
    (clrhash org-air-query--visits)
    (clrhash org-air-query--denote-id-index)
    (setq org-air-query--link-graph-dirty nil)))

(defmacro org-air-r83--with-corpus (specs &rest body)
  "Create a temp Org corpus from SPECS and run BODY against it.
Binds the org-air roots, a temp cache, a round-local board buffer name,
FRESH global edit rings (test isolation) and quiets lockfiles/messages.
Starts from EMPTY query tables; cleans up tables, buffers and dir."
  (declare (indent 1) (debug t))
  `(let* ((org-air-r83--dir (make-temp-file "org-air-r83-" t)))
     (unwind-protect
         (progn
           (org-air-r83--reset-tables)
           (pcase-dolist (`(,name . ,content) ,specs)
             (let ((path (expand-file-name name org-air-r83--dir))
                   (coding-system-for-write 'utf-8-unix)
                   (file-name-handler-alist nil))
               (make-directory (file-name-directory path) t)
               (write-region (or content "") nil path nil 'silent)))
           (let ((org-air-files (list org-air-r83--dir))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-r83--dir))
                 (org-air-cache-file
                  (expand-file-name ".cache/board.eld" org-air-r83--dir))
                 (org-air-view-buffer-name "*org-air-r83*")
                 (org-air-view--edit-ring nil)
                 (org-air-view--edit-redo-ring nil)
                 (org-air-view--triage-source-buffer nil)
                 (org-air-backlog-tag "backlog")
                 (org-tags-column 0)
                 (create-lockfiles nil)
                 (inhibit-message t))
             ,@body))
       (when (fboundp 'org-air-query-teardown)
         (org-air-query-teardown))
       (org-air-r83--reset-tables)
       (let ((kill-buffer-query-functions nil))
         (dolist (buf (buffer-list))
           (let ((fn (buffer-file-name buf)))
             (when (and fn (string-prefix-p org-air-r83--dir fn))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r83--dir t))))

(defmacro org-air-r83--with-board (specs &rest body)
  "Render the real board over the SPECS corpus (clock frozen); run BODY."
  (declare (indent 1) (debug t))
  `(org-air-r83--with-corpus ,specs
     (org-air-viewport-test--with-frozen-now
       (unwind-protect
           (org-air-viewport-test--with-render-guards
             (let ((org-air-view-width 120)
                   (org-air-view-height 60))
               (org-air)
               (let ((buf (get-buffer org-air-view-buffer-name)))
                 (should buf)
                 (with-current-buffer buf
                   ,@body))))
         (let ((kill-buffer-query-functions nil)
               (buf (get-buffer org-air-view-buffer-name)))
           (when buf (kill-buffer buf)))))))

(defun org-air-r83--file (name)
  "Return the corpus file NAME's absolute path."
  (expand-file-name name org-air-r83--dir))

(defun org-air-r83--text (name)
  "Return corpus file NAME's on-disk content as a string."
  (with-temp-buffer
    (insert-file-contents (org-air-r83--file name))
    (buffer-string)))

(defun org-air-r83--goto-row (title)
  "Move point onto the board row whose item title contains TITLE.
Returns the row's item; fails the test when no such row renders."
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (not (eobp)))
      (let ((p (line-beginning-position))
            (eol (line-end-position)))
        (while (and (not found) (< p eol))
          (let ((it (get-text-property p 'org-air-item)))
            (when (and it
                       (string-match-p (regexp-quote title)
                                       (or (org-air-item-title it) "")))
              (goto-char p)
              (setq found it)))
          (setq p (1+ p))))
      (unless found (forward-line 1)))
    (should found)
    found))

(defun org-air-r83--item (title items)
  "Return the item in ITEMS whose title contains TITLE; assert it exists."
  (let ((item (org-air-test-find-item title items)))
    (should item)
    item))

(defun org-air-r83--passes-p (item tokens)
  "Non-nil when ITEM passes filter TOKENS under `all' at the frozen now.
Drives the REAL fold `org-air-view--passes-filter-p', the board path."
  (org-air-r83--passes-match-p item tokens 'all))

(defun org-air-r83--passes-match-p (item tokens match)
  "Non-nil when ITEM passes TOKENS under the M-/ combinator MATCH at now.
MATCH is `all' (AND) or `any' (OR) — `org-air-filter-match'.  Drives the
REAL fold `org-air-view--passes-filter-p', the board path."
  (let ((org-air-view--tag-filter tokens)
        (org-air-filter-match match)
        (org-air-view--filter-now org-air-test-now)
        (org-air-view--scope nil)
        (org-air-view--render-partition nil)
        (org-air-upcoming-days 7)
        (org-air-stale-days 21))
    (and (org-air-view--passes-filter-p item) t)))

;;;; -------------------------------------------------------------------
;;;; r83-1 — `b' defers a placeholder off attention into Backlog.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r83-1-b-defers-off-attention ()
  "`b' on a keyword-carrying dateless heading defers it (r83-1).
Before: the placeholder task classifies `attention' (the no-date
default).  After `org-air-item-backlog' at its row: the SOURCE heading
gains `:backlog:', the cached item's `tags' slot carries it, and
`org-air-classify-item' => (backlog) — NOT attention / upcoming /
high-priority / stale / inbox.  Reverting the D4 gate fails."
  (skip-unless (locate-library "org-air"))
  (org-air-r83--with-board
      '(("docs.org" . "#+title: docs\n\n* TODO Btrfs partition layout\n  fill in anytime\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let ((btrfs (org-air-r83--item "Btrfs" org-air-view--items)))
      ;; before: the no-date attention default.
      (should (equal '(attention)
                     (org-air-classify-item btrfs org-air-test-now)))
      (org-air-r83--goto-row "Btrfs")
      (org-air-item-backlog))
    ;; the source heading gained the org-native tag.
    (should (string-match-p ":backlog:" (org-air-r83--text "docs.org")))
    (let ((btrfs (org-air-r83--item "Btrfs" org-air-view--items)))
      ;; the cached slot carries it (no rescan).
      (should (member "backlog" (org-air-item-tags btrfs)))
      ;; and classify routes to the SINGLE backlog home.
      (should (equal '(backlog)
                     (org-air-classify-item btrfs org-air-test-now)))
      (dolist (bucket '(attention upcoming high-priority stale inbox))
        (should-not (memq bucket
                          (org-air-classify-item btrfs org-air-test-now)))))))

;;;; -------------------------------------------------------------------
;;;; r83-2 — `b' again round-trips; a co-tag survives BOTH toggles.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r83-2-b-again-round-trips-preserving-tags ()
  "A second `b' removes `:backlog:'; a co-tag survives both (r83-2).
From r83-1's deferred state, `org-air-item-backlog' again removes the
tag from the source, the item re-classifies to `attention', and a
pre-existing `:nix:' co-tag survives BOTH toggles.  Reverting the
explicit on/off direction or the `org-toggle-tag' choice fails."
  (skip-unless (locate-library "org-air"))
  (org-air-r83--with-board
      '(("docs.org" . "#+title: docs\n\n* TODO Btrfs partition layout :nix:\n")
        ("inbox.org" . "#+title: inbox\n"))
    ;; toggle ON.
    (org-air-r83--goto-row "Btrfs")
    (org-air-item-backlog)
    (let ((btrfs (org-air-r83--item "Btrfs" org-air-view--items)))
      (should (member "backlog" (org-air-item-tags btrfs)))
      (should (member "nix" (org-air-item-tags btrfs)))
      (should (equal '(backlog)
                     (org-air-classify-item btrfs org-air-test-now))))
    (should (string-match-p ":nix:" (org-air-r83--text "docs.org")))
    ;; R90: Backlog is header-only by default.  Explicitly reveal it before
    ;; locating the deferred row for the round-trip toggle.
    (setq org-air-view--expanded-sections '(backlog))
    (org-air-view--refresh-current)
    ;; toggle OFF.
    (org-air-r83--goto-row "Btrfs")
    (org-air-item-backlog)
    (let ((btrfs (org-air-r83--item "Btrfs" org-air-view--items)))
      (should-not (member "backlog" (org-air-item-tags btrfs)))
      (should (member "nix" (org-air-item-tags btrfs)))
      (should (equal '(attention)
                     (org-air-classify-item btrfs org-air-test-now))))
    ;; the co-tag survived on disk; the backlog tag is gone.
    (let ((text (org-air-r83--text "docs.org")))
      (should (string-match-p ":nix:" text))
      (should-not (string-match-p ":backlog:" text)))))

;;;; -------------------------------------------------------------------
;;;; r83-3 — is:backlog = EXACTLY the backlog set (R72 twin).
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r83-3-is-backlog-exact-set ()
  "`is:backlog' selects exactly the `backlog' bucket (r83-3).
The REAL `org-air-view--passes-filter-p' fold over `is:backlog' selects
the deferred item and ONLY it; a plain OVERDUE item passes `is:overdue'
but NOT `is:backlog'; a DONE heading carrying `:backlog:' fails
`is:backlog' (the board-active gate) — the R72 token<=>bucket theorem
over the tag.  For EVERY scanned item, is:backlog <=> (backlog) bucket.
Reverting the (is . backlog) arm or the filter-is-values entry fails."
  (skip-unless (locate-library "org-air"))
  (org-air-r83--with-corpus
      '(("board.org" . "#+title: board\n\n\
* TODO Deferred placeholder :backlog:\n\
* TODO Overdue thing\nDEADLINE: <2026-06-10 Wed>\n\
* DONE Done deferred :backlog:\nSCHEDULED: <2026-06-01 Mon>\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((items (org-air-query-items))
           (deferred (org-air-r83--item "Deferred placeholder" items))
           (overdue (org-air-r83--item "Overdue thing" items))
           (done (org-air-r83--item "Done deferred" items)))
      ;; the deferred item is the whole is:backlog set.
      (should (org-air-r83--passes-p deferred '("is:backlog")))
      (should-not (org-air-r83--passes-p overdue '("is:backlog")))
      (should-not (org-air-r83--passes-p done '("is:backlog")))
      ;; the plain overdue item owns is:overdue, not is:backlog.
      (should (org-air-r83--passes-p overdue '("is:overdue")))
      (should-not (org-air-r83--passes-p deferred '("is:overdue")))
      ;; #backlog is the SUPERSET: it also hits the DONE tagged heading.
      (should (org-air-r83--passes-p done '("#backlog")))
      ;; the agreement theorem: is:backlog <=> the (backlog) bucket.
      (dolist (item items)
        (should (eq (org-air-r83--passes-p item '("is:backlog"))
                    (and (memq 'backlog
                               (org-air-classify-item item org-air-test-now))
                         t))))
      ;; the token joins the closed is: set + is offered in / completion.
      (should (member "backlog" org-air-view--filter-is-values))
      (should (member "is:backlog" (org-air-view--filter-vocabulary)))
      (should (equal (org-air-view--filter-token-parse "is:backlog")
                     '(is . backlog))))))

;;;; -------------------------------------------------------------------
;;;; r83-4 — Summary Backlog count + section, iff non-empty.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r83-4-summary-and-section-conditional ()
  "The Backlog Summary row + section appear iff the bucket is non-empty
\(r83-4).  A cold board with two deferred items renders a `Backlog 2'
Summary row and a header-only Backlog section; TAB reveals exactly those
two item rows.  A backlog-free board renders NO Backlog Summary row and
NO Backlog section.  Reverting the conditional D5 append or R90 collapse
law fails one half or the other."
  (skip-unless (locate-library "org-air"))
  ;; two backlog items => the section + a count row.
  (org-air-r83--with-board
      '(("board.org" . "#+title: board\n\n\
* TODO Alpha placeholder :backlog:\n\
* TODO Beta placeholder :backlog:\n\
* TODO Plain work\nDEADLINE: <2026-06-10 Wed>\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let ((counts (org-air-view--section-counts org-air-view--items)))
      (should (equal 2 (cdr (assq 'backlog counts)))))
    (let ((descriptors (org-air-view--section-descriptors org-air-view--items)))
      (should (assq 'backlog descriptors)))
    ;; R90 cold contract: count/header chrome, zero item or fold rows.
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "Backlog" text))
      (should-not (string-match-p "Alpha placeholder" text))
      (should-not (string-match-p "Beta placeholder" text))
      (should-not (string-match-p "and [0-9]+ more" text)))
    ;; TAB on the header reveals every Backlog title.
    (goto-char (org-air-view--find-property 'org-air-section 'backlog))
    (org-air-toggle-section)
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "Alpha placeholder" text))
      (should (string-match-p "Beta placeholder" text))))
  ;; a backlog-free board => NO section, NO summary row (byte-identity).
  (org-air-r83--with-board
      '(("board.org" . "#+title: board\n\n\
* TODO Plain work\nDEADLINE: <2026-06-10 Wed>\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let ((counts (org-air-view--section-counts org-air-view--items)))
      (should-not (assq 'backlog counts)))
    (let ((descriptors (org-air-view--section-descriptors org-air-view--items)))
      (should-not (assq 'backlog descriptors)))
    ;; the Summary keeps exactly the fixed five buckets.
    (should (equal (mapcar #'car (org-air-view--summary-buckets
                                  org-air-view--items))
                   (mapcar #'car org-air-view--sections)))))

;;;; -------------------------------------------------------------------
;;;; r83-5 — a backlog item stays reachable by date / all-items.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r83-5-reachable-by-date-and-all-items ()
  "A deferred item keeps its date + note surfaces (r83-5).
A DATED backlog heading (SCHEDULED today, `:backlog:') is absent from
`attention'/`upcoming' yet PRESENT in `org-air-view--day-groups' under
Scheduled on its day and its day key is in
`org-air-calendar--marked-days'; a dateless backlog heading is reachable
via all-items and `#backlog'.  Gating day/calendar on the bucket fails."
  (skip-unless (locate-library "org-air"))
  (org-air-r83--with-corpus
      '(("board.org" . "#+title: board\n\n\
* TODO Dated placeholder :backlog:\nSCHEDULED: <2026-06-15 Mon>\n\
* TODO Dateless placeholder :backlog:\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((items (org-air-query-items))
           (dated (org-air-r83--item "Dated placeholder" items))
           (dateless (org-air-r83--item "Dateless placeholder" items))
           (day (encode-time '(0 0 12 15 6 2026 nil -1 nil))))
      ;; off the attention buckets…
      (should (equal '(backlog) (org-air-classify-item dated org-air-test-now)))
      (should-not (memq 'upcoming
                        (org-air-classify-item dated org-air-test-now)))
      ;; …but its day still lists it under the Scheduled group.
      (with-temp-buffer
        (let* ((org-air-view--scope nil)
               (org-air-view--render-partition nil)
               (groups (org-air-view--day-groups items day)))
          (should (memq dated (cdr (assoc "Scheduled" groups))))))
      ;; …and its calendar day carries the scheduled mark.
      (should (eq (gethash "2026-06-15"
                           (org-air-calendar--marked-days items))
                  'scheduled))
      ;; the dateless deferred item is still in the scan (all-items) and
      ;; still hit by the raw-tag #backlog filter (its superset).
      (should (memq dateless items))
      (should (org-air-r83--passes-p dateless '("#backlog"))))))

;;;; -------------------------------------------------------------------
;;;; r83-6 — board-active precedence: DONE backlog => ZERO buckets.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r83-6-board-active-precedence ()
  "A DONE `:backlog:' heading classifies into ZERO buckets (r83-6).
`org-air-classify--board-active-p' fronts the gate, so a done/archived
deferred heading is history — NOT (backlog).  Moving the gate ABOVE the
board-active test fails."
  (skip-unless (locate-library "org-air"))
  (org-air-r83--with-corpus
      '(("board.org" . "#+title: board\n\n\
* DONE Old placeholder :backlog:\nSCHEDULED: <2026-06-01 Mon>\n\
* TODO Archived placeholder :backlog:ARCHIVE:\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((items (org-air-query-items))
           (done (org-air-r83--item "Old placeholder" items))
           (archived (org-air-r83--item "Archived placeholder" items)))
      (should (equal '() (org-air-classify-item done org-air-test-now)))
      (should (equal '() (org-air-classify-item archived org-air-test-now))))))

;;;; -------------------------------------------------------------------
;;;; r83-7 — no rescan on toggle (R53).
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r83-7-no-rescan-on-toggle ()
  "`b' repaints from cache with ZERO org-ql re-queries (r83-7).
An instrumented `org-air-query-items' counter stays at 0 across the
toggle while the row still MOVES buckets (the cached slot flips from
attention to backlog).  Replacing the repaint with `org-air-refresh'
\(a re-query) fails the counter assertion."
  (skip-unless (locate-library "org-air"))
  (org-air-r83--with-board
      '(("docs.org" . "#+title: docs\n\n* TODO Btrfs partition layout\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let ((btrfs (org-air-r83--item "Btrfs" org-air-view--items))
          (queries 0))
      (should (equal '(attention)
                     (org-air-classify-item btrfs org-air-test-now)))
      (org-air-r83--goto-row "Btrfs")
      (cl-letf* ((orig (symbol-function 'org-air-query-items))
                 ((symbol-function 'org-air-query-items)
                  (lambda (&rest a) (cl-incf queries) (apply orig a))))
        (org-air-item-backlog))
      ;; NO re-query fired…
      (should (= 0 queries))
      ;; …yet the row moved buckets (same eq object, mutated slot).
      (should (equal '(backlog)
                     (org-air-classify-item btrfs org-air-test-now))))))

;;;; -------------------------------------------------------------------
;;;; r83-8 — classify-memo drop (the D3 remhash).
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r83-8-classify-memo-drop ()
  "Without the D3 remhash the memo serves the STALE attention list (r83-8).
The board render warms the classify memo with the item's pre-toggle
`attention' entry; after `b' the cached bucket list for that same `eq'
item is (backlog) — proving the single-item `remhash' fired (else the
warm `eq' entry would still read attention).  Removing the remhash fails."
  (skip-unless (locate-library "org-air"))
  (org-air-r83--with-board
      '(("docs.org" . "#+title: docs\n\n* TODO Btrfs partition layout\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let ((btrfs (org-air-r83--item "Btrfs" org-air-view--items)))
      ;; warm the memo with the pre-toggle bucketing.
      (should (equal '(attention)
                     (org-air-view--classify-cached btrfs org-air-test-now)))
      (should (equal '(attention)
                     (gethash btrfs org-air-view--classify-cache)))
      (org-air-r83--goto-row "Btrfs")
      (org-air-item-backlog)
      ;; the memo now reflects the deferred routing (remhash + rebuild).
      (should (equal '(backlog)
                     (org-air-view--classify-cached btrfs org-air-test-now))))))

;;;; -------------------------------------------------------------------
;;;; r83-9 — edit ring: in-place record, u reverts, U redoes.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r83-9-edit-ring-in-place ()
  "`b' enters the ring as an IN-PLACE record; u/U round-trip it (r83-9).
After `b' the ring head is an in-place record (kind NOT refile/archive)
carrying the `backlog' DESC; `org-air-edit-undo' reverts the source tag
and `org-air-edit-redo' re-applies it, the co-tag kept throughout, the
source bytes round-tripping.  Marking the record structural, or
bypassing the macro, fails."
  (skip-unless (locate-library "org-air"))
  (org-air-r83--with-board
      '(("docs.org" . "#+title: docs\n\n* TODO Btrfs partition layout :nix:\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let ((before (org-air-r83--text "docs.org")))
      (org-air-r83--goto-row "Btrfs")
      (org-air-item-backlog)
      ;; the record is in-place with the backlog desc.
      (should (= 1 (length org-air-view--edit-ring)))
      (let ((rec (car org-air-view--edit-ring)))
        (should (eq 'in-place (plist-get rec :kind)))
        (should (string-match-p "backlog" (plist-get rec :desc))))
      (should (string-match-p ":backlog:" (org-air-r83--text "docs.org")))
      ;; u reverts the source tag byte-exact; the co-tag survives.
      (setq last-command 'ignore)
      (org-air-edit-undo)
      (should (equal before (org-air-r83--text "docs.org")))
      (should (string-match-p ":nix:" (org-air-r83--text "docs.org")))
      (should (null org-air-view--edit-ring))
      ;; U re-applies it — the tag returns, the co-tag kept.
      (setq last-command 'ignore)
      (org-air-edit-redo)
      (let ((text (org-air-r83--text "docs.org")))
        (should (string-match-p ":backlog:" text))
        (should (string-match-p ":nix:" text))))))

;;;; -------------------------------------------------------------------
;;;; r83-10 — never-error on stale / dead / mid-body signal (R53).
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r83-10-never-error ()
  "`b' degrades gracefully on every dead/stale input (r83-10).
A no-item line signals a SOFT `user-error' (message-only); a mid-refresh
stale file soft-errors via the stale guard and writes NOTHING; a mid-body
signal rolls back the `atomic-change-group' with NO save and NO ring
push.  Reverting the guards / condition-case fails the never-error law."
  (skip-unless (locate-library "org-air"))
  ;; (a) no item at point => a soft user-error.
  (with-temp-buffer
    (org-air-view-mode)
    (should-error (org-air-item-backlog) :type 'user-error))
  ;; (b) mid-refresh stale file: the stale guard soft-errors, no write.
  (org-air-r83--with-board
      '(("docs.org" . "#+title: docs\n\n* TODO Btrfs partition layout\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let ((before (org-air-r83--text "docs.org")))
      (org-air-r83--goto-row "Btrfs")
      (setq-local org-air-view--refresh-state 'refreshing)
      (setq-local org-air-view--refresh-progressive nil)
      (setq-local org-air-view--cache-stale-files
                  (list (org-air-r83--file "docs.org")))
      (should-error (org-air-item-backlog) :type 'user-error)
      (should (equal before (org-air-r83--text "docs.org")))
      (should (null org-air-view--edit-ring))))
  ;; (c) a mid-body signal: rollback, no save, no ring push, no hard error.
  (org-air-r83--with-board
      '(("docs.org" . "#+title: docs\n\n* TODO Btrfs partition layout\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let ((before (org-air-r83--text "docs.org")))
      (org-air-r83--goto-row "Btrfs")
      (cl-letf (((symbol-function 'org-toggle-tag)
                 (lambda (&rest _) (error "boom in body"))))
        ;; the condition-case downgrades the residual error to a message.
        (org-air-item-backlog))
      (should (equal before (org-air-r83--text "docs.org")))
      (should (null org-air-view--edit-ring))
      ;; the cached slot was NOT mutated (setf never reached).
      (let ((btrfs (org-air-r83--item "Btrfs" org-air-view--items)))
        (should-not (member "backlog" (org-air-item-tags btrfs)))))))

;;;; -------------------------------------------------------------------
;;;; r83-11 — `b' installer-owned (R35-1): bound + strippable.
;;;; -------------------------------------------------------------------

(defmacro org-air-r83--with-knob (val &rest body)
  "Set `org-air-use-default-keybindings' to VAL, sync the maps, run BODY."
  (declare (indent 1) (debug t))
  `(let ((org-air-r83--saved org-air-use-default-keybindings))
     (unwind-protect
         (progn
           (setq org-air-use-default-keybindings ,val)
           (org-air--sync-default-keybindings)
           ,@body)
       (setq org-air-use-default-keybindings org-air-r83--saved)
       (setq org-air--default-keybindings-state 'unset)
       (org-air--sync-default-keybindings))))

(ert-deftest org-air-r83-11-key-bound-and-strippable ()
  "`b' is installer-owned via the R35-1 registry (r83-11).
With the defaults ON, `b' resolves to `org-air-item-backlog' on the board
AND the review map (and is ABSENT from the shared core map — inherited);
with `org-air-use-default-keybindings' nil, `b' is UNBOUND on both.
Reverting the registry entries (or hard-binding outside it) fails."
  (skip-unless (locate-library "org-air"))
  (org-air-r83--with-knob t
    (with-temp-buffer
      (org-air-view-mode)
      (should (eq (key-binding (kbd "b")) 'org-air-item-backlog)))
    (with-temp-buffer
      (org-air-review-mode)
      (should (eq (key-binding (kbd "b")) 'org-air-item-backlog)))
    ;; the shared core map never carries it (it is a board/review key).
    (should-not (lookup-key org-air-view-core-map (kbd "b"))))
  (org-air-r83--with-knob nil
    (with-temp-buffer
      (org-air-view-mode)
      (should-not (eq (key-binding (kbd "b")) 'org-air-item-backlog)))
    (with-temp-buffer
      (org-air-review-mode)
      (should-not (eq (key-binding (kbd "b")) 'org-air-item-backlog)))))

;;;; -------------------------------------------------------------------
;;;; r83-12 — cache-key placement (scan-key UNCHANGED, memo-key DIFFERS).
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r83-12-cache-key-placement ()
  "The backlog tag is a classify-memo key, NOT a scan cache-key (r83-12).
`org-air-view--cache-key' is UNCHANGED across a backlog-tag rename (no
cold re-derive — two keys `equal'), while the classify-memo coherence
key DIFFERS across the rename (a mid-session slot-fold rebuild).  Adding
the tag to the scan key, or omitting it from the memo key, fails."
  (skip-unless (locate-library "org-air"))
  (org-air-r83--with-corpus
      '(("docs.org" . "#+title: docs\n\n* TODO Btrfs partition layout\n")
        ("inbox.org" . "#+title: inbox\n"))
    ;; the SCAN key ignores the tag name (no version bump, no re-derive).
    (let ((key-a (let ((org-air-backlog-tag "backlog"))
                   (org-air-view--cache-key)))
          (key-b (let ((org-air-backlog-tag "later"))
                   (org-air-view--cache-key))))
      (should (equal key-a key-b)))
    ;; the classify-MEMO key carries the tag name (a rename rebuilds it).
    (with-temp-buffer
      (let (memo-a memo-b)
        (let ((org-air-backlog-tag "backlog"))
          (setq org-air-view--classify-cache nil
                org-air-view--classify-cache-day nil)
          (org-air-view--classify-cache-ensure org-air-test-now)
          (setq memo-a org-air-view--classify-cache-day))
        (let ((org-air-backlog-tag "later"))
          (org-air-view--classify-cache-ensure org-air-test-now)
          (setq memo-b org-air-view--classify-cache-day))
        (should-not (equal memo-a memo-b))
        ;; the tag name is the differing element (position 2).
        (should (equal "backlog" (nth 2 memo-a)))
        (should (equal "later" (nth 2 memo-b)))))))

;;;; -------------------------------------------------------------------
;;;; r83-13 — process-inbox `[b]' defer disposition (single-home).
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r83-13-process-inbox-defer-disposition ()
  "The guided walk's `?b' defers the item out of the Inbox (r83-13).
An inbox-file heading; `org-air-item-backlog' applies `:backlog:' so the
item leaves the `inbox' bucket (the single-home ruling) — the countdown
decrements.  Driving the real `org-air-process-inbox' with a queued `b'
then `q' confirms the pcase entry shrinks the inbox by one.  Reverting
the pcase entry (or the single-home gate) fails."
  (skip-unless (locate-library "org-air"))
  ;; direct disposition: an inbox item leaves the inbox bucket.
  (org-air-r83--with-board
      '(("inbox.org" . "#+title: inbox\n\n\
* TODO Capture one :inbox:\n\
* TODO Capture two :inbox:\n"))
    (let ((one (org-air-r83--item "Capture one" org-air-view--items)))
      (should (memq 'inbox (org-air-classify-item one org-air-test-now)))
      (org-air-r83--goto-row "Capture one")
      (org-air-item-backlog)
      (should-not (memq 'inbox (org-air-classify-item one org-air-test-now)))
      (should (equal '(backlog)
                     (org-air-classify-item one org-air-test-now)))))
  ;; the real guided walk: `b' then `q' shrinks the inbox by one.  The
  ;; single-column board-only width (< the two-pane breakpoint) is what
  ;; `org-air-view--goto-first-inbox-item' navigates (its col-0 row test),
  ;; so the disposition lands on the item exactly as it does interactively.
  (org-air-r83--with-corpus
      '(("inbox.org" . "#+title: inbox\n\n\
* TODO Capture one :inbox:\n\
* TODO Capture two :inbox:\n"))
    (org-air-viewport-test--with-frozen-now
      (org-air-viewport-test--with-render-guards
        (let ((org-air-view-width 90)
              (org-air-view-height 60))
          (unwind-protect
              (progn
                (org-air)
                (with-current-buffer org-air-view-buffer-name
                  (let ((keys (list ?b ?q)))
                    (cl-letf (((symbol-function 'read-char-exclusive)
                               (lambda (&rest _) (or (pop keys) ?q))))
                      (org-air-process-inbox))
                    ;; one item was deferred out of the inbox.
                    (should (string-match-p ":backlog:"
                                            (org-air-r83--text "inbox.org")))
                    (let ((inbox (org-air-view--items-for-bucket
                                  'inbox org-air-view--items)))
                      (should (= 1 (length inbox)))))))
            (let ((kill-buffer-query-functions nil)
                  (buf (get-buffer org-air-view-buffer-name)))
              (when buf (kill-buffer buf)))))))))

;;;; -------------------------------------------------------------------
;;;; r83-14 — is:backlog COMPOSES under M-/ with #tag/todo:/scheduled:/due:.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r83-14-is-backlog-composes-under-m-slash ()
  "`is:backlog' composes with every axis under the M-/ combinator (r83-14).
Three rows — Alpha (deferred, #proj, scheduled soon), Beta (scheduled
soon, #proj, NOT deferred) and Gamma (deferred, dateless) — exercise the
real `org-air-view--passes-filter-p' fold.  Under `all' (AND) is:backlog
NARROWS: `is:backlog #proj' => Alpha alone; `is:backlog scheduled:7d' =>
Alpha alone (dateless Gamma drops); `is:backlog todo:TODO' => the two
deferred rows.  Under `any' (OR) the SAME `is:backlog #proj' BROADENS to
all three.  A weakened combinator (ignoring the token, or dropping the
match mode) fails one direction or the other."
  (skip-unless (locate-library "org-air"))
  (org-air-r83--with-corpus
      '(("board.org" . "#+title: board\n\n\
* TODO Alpha deferred :backlog:proj:\nSCHEDULED: <2026-06-16 Tue>\n\
* TODO Beta scheduled :proj:\nSCHEDULED: <2026-06-16 Tue>\n\
* TODO Gamma deferred :backlog:\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((items (org-air-query-items))
           (alpha (org-air-r83--item "Alpha deferred" items))
           (beta (org-air-r83--item "Beta scheduled" items))
           (gamma (org-air-r83--item "Gamma deferred" items)))
      ;; the buckets are as intended.
      (should (equal '(backlog) (org-air-classify-item alpha org-air-test-now)))
      (should (memq 'upcoming (org-air-classify-item beta org-air-test-now)))
      (should (equal '(backlog) (org-air-classify-item gamma org-air-test-now)))
      ;; AND: is:backlog ∧ #proj => Alpha ALONE (the raw #tag axis).
      (should (org-air-r83--passes-match-p alpha '("is:backlog" "#proj") 'all))
      (should-not (org-air-r83--passes-match-p beta '("is:backlog" "#proj") 'all))
      (should-not (org-air-r83--passes-match-p gamma '("is:backlog" "#proj") 'all))
      ;; OR: the SAME two tokens broaden to all three (Beta via #proj,
      ;; Gamma via is:backlog).
      (should (org-air-r83--passes-match-p alpha '("is:backlog" "#proj") 'any))
      (should (org-air-r83--passes-match-p beta '("is:backlog" "#proj") 'any))
      (should (org-air-r83--passes-match-p gamma '("is:backlog" "#proj") 'any))
      ;; AND with the R79 keyword axis: is:backlog ∧ todo:TODO => the two
      ;; deferred rows; Beta (a TODO, but not deferred) drops.
      (should (org-air-r83--passes-match-p alpha '("is:backlog" "todo:TODO") 'all))
      (should (org-air-r83--passes-match-p gamma '("is:backlog" "todo:TODO") 'all))
      (should-not (org-air-r83--passes-match-p beta '("is:backlog" "todo:TODO") 'all))
      ;; AND with the R72 date window: is:backlog ∧ scheduled:7d => Alpha
      ;; ALONE (dateless Gamma drops; Beta is scheduled but not deferred).
      (should (org-air-r83--passes-match-p alpha '("is:backlog" "scheduled:7d") 'all))
      (should-not (org-air-r83--passes-match-p gamma '("is:backlog" "scheduled:7d") 'all))
      (should-not (org-air-r83--passes-match-p beta '("is:backlog" "scheduled:7d") 'all))
      ;; AND with due: (deadline OR scheduled) selects Alpha too; Beta
      ;; still fails the is:backlog conjunct.
      (should (org-air-r83--passes-match-p alpha '("is:backlog" "due:7d") 'all))
      (should-not (org-air-r83--passes-match-p beta '("is:backlog" "due:7d") 'all)))))

;;;; -------------------------------------------------------------------
;;;; r83-15 — the Summary count + section update on the LIVE toggle (R53).
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r83-15-summary-count-updates-live-on-toggle ()
  "The Summary Backlog count + section materialise on the live repaint (r83-15).
A single-attention board shows NO Backlog Summary row, NO Backlog section
and NO `Backlog' text.  After `b' (an R53 in-place repaint, spy = 0
re-queries) `org-air-view--section-counts' grows a `(backlog . 1)' row,
the section descriptors gain Backlog, and the repainted BUFFER now
renders a Backlog section; a second `b' retracts BOTH — the Summary
returns to the fixed five.  Complements r83-4 (which only measured a
STATIC board) and r83-7 (the spy) by pinning the DYNAMIC Summary update."
  (skip-unless (locate-library "org-air"))
  (org-air-r83--with-board
      '(("docs.org" . "#+title: docs\n\n* TODO Btrfs partition layout\n")
        ("inbox.org" . "#+title: inbox\n"))
    ;; before: no backlog anywhere (data + rendered text).
    (should-not (assq 'backlog (org-air-view--section-counts org-air-view--items)))
    (should-not (assq 'backlog (org-air-view--section-descriptors org-air-view--items)))
    (should-not (string-match-p
                 "Backlog" (buffer-substring-no-properties (point-min) (point-max))))
    ;; defer, counting re-queries: the repaint must NOT scan.
    (org-air-r83--goto-row "Btrfs")
    (let ((queries 0))
      (cl-letf* ((orig (symbol-function 'org-air-query-items))
                 ((symbol-function 'org-air-query-items)
                  (lambda (&rest a) (cl-incf queries) (apply orig a))))
        (org-air-item-backlog))
      (should (= 0 queries)))
    ;; after: the Summary count row + section materialised IN PLACE.
    (should (equal 1 (cdr (assq 'backlog
                                (org-air-view--section-counts org-air-view--items)))))
    (should (assq 'backlog (org-air-view--section-descriptors org-air-view--items)))
    (should (string-match-p
             "Backlog" (buffer-substring-no-properties (point-min) (point-max))))
    ;; R90: reveal the new header-only section before locating the row again.
    (setq org-air-view--expanded-sections '(backlog))
    (org-air-view--refresh-current)
    ;; un-defer: the count row + section retract, back to the fixed five.
    (org-air-r83--goto-row "Btrfs")
    (org-air-item-backlog)
    (should-not (assq 'backlog (org-air-view--section-counts org-air-view--items)))
    (should (equal (mapcar #'car (org-air-view--summary-buckets org-air-view--items))
                   (mapcar #'car org-air-view--sections)))))

;;;; -------------------------------------------------------------------
;;;; r83-16 — R68 clean write + refile/archive stay structural.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r83-16-r68-clean-write-and-archive-structural ()
  "`b' writes the tag CLEANLY (R68) and other verbs stay structural (r83-16).
The ONLY on-disk delta from `b' is the heading's tag: no `:LOGBOOK:'
drawer, no `- State' note, no `CLOSED:' stamp; the PROPERTIES drawer and
body line stay byte-identical and the file keeps its line count (the R68
board-context logging discipline the shared macro enforces).  The
backlog ring record is `in-place' while `org-air-item-archive' still
records `archive' (structural) — the deferred write did not demote the
structural class.  Reverting the R68 logging binds (a stray state note)
or the macro's structural leg fails."
  (skip-unless (locate-library "org-air"))
  (org-air-r83--with-board
      '(("docs.org" . "#+title: docs\n\n\
* TODO Btrfs partition layout :nix:\n\
:PROPERTIES:\n:CUSTOM_ID: btrfs\n:END:\n\
  body line stays.\n\
* TODO Archive me\nDEADLINE: <2026-06-10 Wed>\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let ((before-lines (split-string (org-air-r83--text "docs.org") "\n")))
      (org-air-r83--goto-row "Btrfs")
      (org-air-item-backlog)
      (let* ((after (org-air-r83--text "docs.org"))
             (after-lines (split-string after "\n")))
        ;; the tag landed…
        (should (string-match-p ":backlog:" after))
        ;; …and NOTHING logging-ish did.
        (should-not (string-match-p ":LOGBOOK:" after))
        (should-not (string-match-p "- State" after))
        (should-not (string-match-p "CLOSED:" after))
        ;; the file kept its shape: same line count, exactly ONE line
        ;; changed, and it is the Btrfs heading gaining the tag.
        (should (= (length before-lines) (length after-lines)))
        (let ((diffs (cl-loop for b in before-lines for a in after-lines
                              unless (equal b a) collect (cons b a))))
          (should (= 1 (length diffs)))
          (should (string-match-p "Btrfs partition layout" (car (car diffs))))
          (should (string-match-p ":backlog:" (cdr (car diffs))))
          ;; the PROPERTIES drawer + body line are untouched.
          (should (string-match-p ":CUSTOM_ID: btrfs" after))
          (should (string-match-p "body line stays\\." after))))
      ;; the backlog record is in-place…
      (should (eq 'in-place (plist-get (car org-air-view--edit-ring) :kind)))
      ;; …while archive stays STRUCTURAL (the class is not demoted).
      (org-air-r83--goto-row "Archive me")
      (org-air-item-archive)
      (should (eq 'archive (plist-get (car org-air-view--edit-ring) :kind))))))

;;;; -------------------------------------------------------------------
;;;; r83-17 — R77 keyword-less placeholder + `b'; never-error on a dead item.
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r83-17-r77-keywordless-and-dead-item ()
  "`b' on an R77-demoted placeholder + a DEAD item never errors (r83-17).
Under `org-air-task-requires-todo' t a keyword-less SCHEDULED heading
demotes OFF the task buckets (a `knowledge' note) yet stays reachable by
its day group AND its calendar mark.  Rendered in the DAY view (where the
note surfaces), `b' NEVER errors: it writes `:backlog:' to the source but
the item stays a NOTE — NOT routed to `backlog' (that bucket is
board-active only) and still off attention, still day-reachable.  And `b'
on an item whose SOURCE FILE has been deleted degrades to a message with
NO ring push (the never-error law).  Gating day/calendar on the bucket,
or letting a note phantom a backlog entry, or a hard throw on the dead
file, each fails."
  (skip-unless (locate-library "org-air"))
  ;; (a) the R77-demoted placeholder, toggled in the day view.
  (let ((org-air-task-requires-todo t))
    (org-air-r83--with-board
        '(("notes.org" . "#+title: notes\n\n\
* Btrfs notes placeholder\nSCHEDULED: <2026-06-16 Tue>\n")
          ("inbox.org" . "#+title: inbox\n"))
      ;; capture the buffer-local items list LEXICALLY (it reads nil inside
      ;; a `with-temp-buffer' otherwise — the r83-5 idiom).
      (let* ((items org-air-view--items)
             (note (org-air-r83--item "Btrfs notes" items))
             (day (encode-time '(0 0 12 16 6 2026 nil -1 nil))))
        ;; off the attention surfaces — a demoted note, not a task.
        (let ((buckets (org-air-classify-item note org-air-test-now)))
          (should-not (memq 'attention buckets))
          (should-not (memq 'upcoming buckets))
          (should-not (memq 'backlog buckets)))
        ;; reachable by date: its day lists it + its calendar day is marked.
        (with-temp-buffer
          (let* ((org-air-view--scope nil)
                 (org-air-view--render-partition nil)
                 (groups (org-air-view--day-groups items day)))
            (should (memq note (cdr (assoc "Scheduled" groups))))))
        (should (gethash "2026-06-16"
                         (org-air-calendar--marked-days items)))
        ;; render the DAY view; `b' on the note row never errors.
        (org-air-view-day day)
        (should (string-match-p
                 "Btrfs notes" (buffer-substring-no-properties (point-min) (point-max))))
        (org-air-r83--goto-row "Btrfs notes")
        (org-air-item-backlog)
        ;; the tag landed, but the item stays a NOTE (no phantom backlog),
        ;; off attention and still day-reachable.
        (should (string-match-p ":backlog:" (org-air-r83--text "notes.org")))
        (let* ((items2 org-air-view--items)
               (note2 (org-air-r83--item "Btrfs notes" items2)))
          (should-not (memq 'backlog (org-air-classify-item note2 org-air-test-now)))
          (should-not (memq 'attention (org-air-classify-item note2 org-air-test-now)))
          (with-temp-buffer
            (let* ((org-air-view--scope nil)
                   (org-air-view--render-partition nil)
                   (groups (org-air-view--day-groups items2 day)))
              (should (memq note2 (cdr (assoc "Scheduled" groups))))))))))
  ;; (b) a DEAD item: the source BUFFER killed AND its file deleted — the
  ;; at-item-source hydrate lands in a headless buffer, so `b' raises a
  ;; SOFT `user-error' (never a backtrace), pushes NO ring record and
  ;; writes nothing (a distinct never-error leg from r83-10's no-item /
  ;; mid-refresh-stale cases).
  (org-air-r83--with-board
      '(("docs.org" . "#+title: docs\n\n* TODO Btrfs partition layout\n")
        ("inbox.org" . "#+title: inbox\n"))
    (org-air-r83--goto-row "Btrfs")
    (dolist (b (buffer-list))
      (let ((fn (buffer-file-name b)))
        (when (and fn (string-match-p "docs\\.org\\'" fn))
          (with-current-buffer b (set-buffer-modified-p nil))
          (kill-buffer b))))
    (delete-file (org-air-r83--file "docs.org"))
    (should-error (org-air-item-backlog) :type 'user-error)
    (should (null org-air-view--edit-ring))))

(provide 'org-air-round83-test)
;;; org-air-round83-test.el ends here
