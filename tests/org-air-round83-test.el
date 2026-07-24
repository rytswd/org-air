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
  (let ((org-air-view--tag-filter tokens)
        (org-air-filter-match 'all)
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
\(r83-4).  A board with two deferred items renders a `Backlog 2' Summary
row and a Backlog section body holding exactly those two; a backlog-free
board renders NO Backlog Summary row and NO Backlog section.  Reverting
the conditional D5 append fails one half or the other."
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
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "Backlog" text))
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

(provide 'org-air-round83-test)
;;; org-air-round83-test.el ends here
