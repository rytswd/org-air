;;; org-air-invariant-echo-test.el --- invariant family P1 -*- lexical-binding: t; -*-

;;; Commentary:
;; INVARIANT FAMILY P1 — the ECHO AREA.
;;
;; The R91 distinguished review measured the hole: across 95 test files,
;; `current-message' appeared in ZERO of them.  The user reads the echo
;; line after every keystroke, and R90 ships behaviour-critical text
;; there — "Marked 3 items", "Pruned 2 stale marked items", the bulk-verb
;; written / already / ineligible / failed buckets, the undo and redo
;; claims, and every refusal.  A wrong count, a missing prune warning or a
;; "complete success" line over a partial write was invisible to the whole
;; suite.
;;
;; This file asserts the USER-VISIBLE TEXT of those messages.
;;
;; THE INSTRUMENT, stated honestly.  `current-message' is ALWAYS nil in
;; --batch: a batch Emacs has no echo area and `message' goes to stderr.
;; `org-air-landing-test-echo-of' therefore prefers `current-message'
;; when it exists (a real frame) and otherwise returns the LAST line the
;; keystroke appended to the message LOG, which is character for
;; character the string the echo area would have shown.  Refusals
;; (`user-error') reach the same echo area and are asserted with
;; `org-air-landing-test-refusal-of', on `error-message-string' — the
;; exact text Emacs echoes.  Nothing here is pixel-dependent, so nothing
;; here is a GUI skip.
;;
;; COVERAGE IS MECHANICAL, NOT ANECDOTAL.  `org-air-echo-test-inventory'
;; scans the four view modules for every literal `(message "…")' and
;; `(user-error "…")' format string, and
;; `org-air-echo-covered-messages-really-exist' fails if this file claims
;; to cover a string the source does not contain — so the coverage claim
;; can never drift into fiction.  The messages this round does NOT yet
;; cover (load/refresh failure paths, bookmark-degraded warnings, the
;; capture/refile/inbox-triage prompts and the bulk-history partial-commit
;; lines) are listed in the round-92 Air history, so the gap is VISIBLE
;; rather than implied.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)
(require 'org-air-landing-helpers)

;;;; =====================================================================
;;;; 0. The instrument itself
;;;; =====================================================================

(ert-deftest org-air-echo-instrument-captures-the-echo-text ()
  "The echo instrument returns the text a keystroke put in the echo area.
A capture helper that silently returned nil would make every assertion
below vacuous, so it is pinned first."
  (should (equal "org-air echo probe 7"
                 (org-air-landing-test-echo-of
                  (lambda () (message "org-air echo probe %d" 7)))))
  ;; A thunk that says nothing returns nothing.
  (should (null (org-air-landing-test-echo-of #'ignore)))
  ;; The LAST message wins, exactly as the echo area shows the last one.
  (should (equal "second"
                 (org-air-landing-test-echo-of
                  (lambda () (message "first") (message "second")))))
  ;; Refusals are captured as the text Emacs echoes.
  (should (equal "No org-air item at point"
                 (org-air-landing-test-refusal-of
                  (lambda () (user-error "No org-air item at point")))))
  (should (null (org-air-landing-test-refusal-of #'ignore))))

;;;; =====================================================================
;;;; 1. Marks — the R90 selection text
;;;; =====================================================================

(ert-deftest org-air-echo-mark-reports-the-running-count ()
  "`m' echoes the RUNNING mark count and the verbs it unlocks.
The count is the whole point: a mark the user cannot see the size of is
a selection they cannot trust before pressing a bulk verb."
  (skip-unless (fboundp 'org-air-toggle-mark))
  (org-air-landing-test-with-board
    (org-air-landing-test-goto "Task number 40")
    (should (equal "Marked 1 item — b backlog, t add tag, M clears"
                   (org-air-landing-test-echo-of #'org-air-toggle-mark)))
    (org-air-landing-test-goto "Task number 41")
    (should (equal "Marked 2 items — b backlog, t add tag, M clears"
                   (org-air-landing-test-echo-of #'org-air-toggle-mark)))
    ;; Un-marking says so, and re-reports the count that is LEFT.
    (should (equal "Unmarked 1 item — b backlog, t add tag, M clears"
                   (org-air-landing-test-echo-of #'org-air-toggle-mark)))))

(ert-deftest org-air-echo-clear-marks-distinguishes-empty-from-cleared ()
  "`M' says `Cleared all marks' only when there WERE marks to clear."
  (skip-unless (fboundp 'org-air-clear-marks))
  (org-air-landing-test-with-board
    (should (equal "No marked items"
                   (org-air-landing-test-echo-of #'org-air-clear-marks)))
    (org-air-landing-test-goto "Task number 40")
    (org-air-toggle-mark)
    (should (equal "Cleared all marks"
                   (org-air-landing-test-echo-of #'org-air-clear-marks)))
    (should (equal "No marked items"
                   (org-air-landing-test-echo-of #'org-air-clear-marks)))))

(ert-deftest org-air-echo-prune-warns-about-stale-marks ()
  "A refresh that drops stale marks WARNS, with the count it pruned.
A mark that silently disappears is worse than one that fails loudly: the
next bulk verb would act on a selection the user never saw shrink."
  (skip-unless (fboundp 'org-air-toggle-mark))
  (org-air-landing-test-with-board
    (org-air-landing-test-goto "Task number 40")
    (org-air-toggle-mark)
    (org-air-landing-test-goto "Task number 41")
    (org-air-toggle-mark)
    ;; Move every marked heading's byte position by editing ABOVE them.
    (let ((file (expand-file-name "tasks.org" org-air-landing-test--dir))
          (coding-system-for-write 'utf-8-unix))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (insert "* TODO Freshly inserted heading\n")
        (write-region (point-min) (point-max) file nil 'silent)))
    (let ((echo (org-air-landing-test-echo-of #'org-air-refresh)))
      (should (equal "Pruned 2 stale marked items" echo)))))

;;;; =====================================================================
;;;; 2. Bulk verbs — the written / already / ineligible / failed buckets
;;;; =====================================================================

(ert-deftest org-air-echo-bulk-backlog-reports-what-it-wrote ()
  "`b' over a selection reports the number of items it actually WROTE."
  (skip-unless (fboundp 'org-air-item-backlog))
  (org-air-landing-test-with-board
    (org-air-landing-test-goto "Task number 40")
    (org-air-toggle-mark)
    (org-air-landing-test-goto "Task number 41")
    (org-air-toggle-mark)
    (should (equal "Backlogged 2 marked items"
                   (org-air-landing-test-echo-of #'org-air-item-backlog)))))

(defun org-air-echo-test--expand-backlog ()
  "Expand the board's Backlog section so its rows are reachable.
The ordinary Backlog is header-only until TAB expands it (R90), so a
deferred item is off screen until this runs."
  (let ((pos (org-air-view--find-property 'org-air-section 'backlog)))
    (should pos)
    (goto-char pos)
    (org-air-toggle-section)))

(ert-deftest org-air-echo-bulk-backlog-reports-the-already-bucket ()
  "A selection that is PARTLY done already says so, bucket by bucket.
The `; 1 already backlog' clause is the honest half: without it the line
would read as two fresh writes where one item was untouched."
  (skip-unless (fboundp 'org-air-item-backlog))
  (org-air-landing-test-with-board
    ;; One item is deferred on its own first ...
    (org-air-landing-test-goto "Task number 40")
    (org-air-item-backlog)
    (org-air-echo-test--expand-backlog)
    ;; ... then both are selected and the verb runs over the pair.
    (org-air-landing-test-goto "Task number 40")
    (org-air-toggle-mark)
    (org-air-landing-test-goto "Task number 41")
    (org-air-toggle-mark)
    (let ((echo (org-air-landing-test-echo-of #'org-air-item-backlog)))
      (should (equal "Backlogged 2 marked items; 1 already backlog" echo)))))

(ert-deftest org-air-echo-bulk-un-backlog-names-the-inverse ()
  "Running `b' again over a fully deferred selection UN-defers it, and
says the inverse word rather than repeating the same claim."
  (skip-unless (fboundp 'org-air-item-backlog))
  (org-air-landing-test-with-board
    (org-air-landing-test-goto "Task number 40")
    (org-air-toggle-mark)
    (org-air-landing-test-goto "Task number 41")
    (org-air-toggle-mark)
    (should (equal "Backlogged 2 marked items"
                   (org-air-landing-test-echo-of #'org-air-item-backlog)))
    (org-air-echo-test--expand-backlog)
    (org-air-landing-test-goto "Task number 40")
    (org-air-toggle-mark)
    (org-air-landing-test-goto "Task number 41")
    (org-air-toggle-mark)
    (should (equal "Un-backlogged 2 marked items"
                   (org-air-landing-test-echo-of #'org-air-item-backlog)))))

;;;; =====================================================================
;;;; 3. Single-item verbs and their refusals
;;;; =====================================================================

(ert-deftest org-air-echo-single-item-verbs-name-the-item-they-touched ()
  "`b', `D' and `a' each name the item they acted on, in quotes."
  (skip-unless (fboundp 'org-air-item-backlog))
  (org-air-landing-test-with-board
    (org-air-landing-test-goto "Task number 40")
    (should (equal "Backlogged \"Task number 40\""
                   (org-air-landing-test-echo-of #'org-air-item-backlog)))
    ;; The deferred row now lives in the header-only Backlog section.
    (org-air-echo-test--expand-backlog)
    (org-air-landing-test-goto "Task number 40")
    (should (equal "Un-backlogged \"Task number 40\""
                   (org-air-landing-test-echo-of #'org-air-item-backlog)))
    (org-air-landing-test-goto "Task number 42")
    (should (equal "Marked DONE \"Task number 42\""
                   (org-air-landing-test-echo-of #'org-air-item-done)))
    (org-air-landing-test-goto "Task number 43")
    (should (equal "Archived \"Task number 43\""
                   (org-air-landing-test-echo-of #'org-air-item-archive)))
    ;; `T' names the item AND the transition it made, both keywords.  The
    ;; keyword itself comes from a single-key prompt, answered here the
    ;; way the command loop would answer it.
    (org-air-landing-test-goto "Task number 45")
    (let ((echo (cl-letf (((symbol-function 'completing-read)
                           (lambda (&rest _) "DONE")))
                  (org-air-landing-test-echo-of
                   #'org-air-item-cycle-todo))))
      (should (equal "Todo \"Task number 45\": TODO → DONE" echo)))
    ;; ... and it says so honestly when the answer changes nothing.
    (org-air-landing-test-goto "Task number 46")
    (should (equal "Todo unchanged"
                   (cl-letf (((symbol-function 'completing-read)
                              (lambda (&rest _) "TODO")))
                     (org-air-landing-test-echo-of
                      #'org-air-item-cycle-todo))))))

(ert-deftest org-air-echo-single-item-verb-refuses-while-marks-are-active ()
  "A single-item verb with a SELECTION active refuses, and says how to
clear it.  Silently acting on one item while two are marked would be a
data-integrity surprise, so the refusal text is part of the contract."
  (skip-unless (fboundp 'org-air-item-done))
  (org-air-landing-test-with-board
    (org-air-landing-test-goto "Task number 40")
    (org-air-toggle-mark)
    (org-air-landing-test-goto "Task number 41")
    (org-air-toggle-mark)
    (org-air-landing-test-goto "Task number 42")
    (should (equal "Marking DONE is single-item while 2 marks are active; M clears marks"
                   (org-air-landing-test-refusal-of #'org-air-item-done)))
    (should (equal "Archiving is single-item while 2 marks are active; M clears marks"
                   (org-air-landing-test-refusal-of #'org-air-item-archive)))))

(ert-deftest org-air-echo-verbs-refuse-off-a-row-with-the-view-s-own-words ()
  "Off a row, each view refuses in ITS OWN vocabulary: an item, a note,
an Air document.  Three views, three nouns — a shared \"nothing here\"
would be a worse product."
  (skip-unless (fboundp 'org-air-visit-item))
  (org-air-landing-test-with-board
    (goto-char (point-min))
    (should (equal "No org-air item at point"
                   (org-air-landing-test-refusal-of #'org-air-visit-item))))
  (org-air-landing-test-with-revisit
    (goto-char (point-min))
    (should (equal "No note at point"
                   (org-air-landing-test-refusal-of #'org-air-revisit-visit))))
  (org-air-landing-test-with-project
    (goto-char (point-min))
    (should (equal "No Air document on this line"
                   (org-air-landing-test-refusal-of #'org-air-project-visit)))))

(ert-deftest org-air-echo-rail-toggle-refuses-outside-an-org-air-buffer ()
  "The rail toggle refuses in a foreign buffer instead of misbehaving."
  (skip-unless (fboundp 'org-air-rail-toggle))
  (with-temp-buffer
    (should (equal "Not in an org-air board or project buffer"
                   (org-air-landing-test-refusal-of #'org-air-rail-toggle)))))

;;;; =====================================================================
;;;; 4. Undo / redo claims
;;;; =====================================================================

(ert-deftest org-air-echo-undo-and-redo-claim-only-what-they-did ()
  "`u' and `U' name the edit they reverted / re-applied and how many
steps remain, and refuse honestly when the ring is empty."
  (skip-unless (and (fboundp 'org-air-edit-undo) (fboundp 'org-air-item-done)))
  (org-air-landing-test-with-board
    (should (equal "Nothing to undo"
                   (org-air-landing-test-refusal-of #'org-air-edit-undo)))
    (should (equal "Nothing to redo"
                   (org-air-landing-test-refusal-of #'org-air-edit-redo)))
    (org-air-landing-test-goto "Task number 44")
    (org-air-item-done)
    (let ((undone (org-air-landing-test-echo-of #'org-air-edit-undo)))
      (should (string-match-p "\\`Undid: " undone))
      (should (string-match-p "Task number 44" undone))
      (should (string-match-p "(0 more)\\'" undone)))
    (let ((redone (org-air-landing-test-echo-of #'org-air-edit-redo)))
      (should (string-match-p "\\`Redid: " redone))
      (should (string-match-p "Task number 44" redone))
      (should (string-match-p "(0 more redoable)\\'" redone)))))

;;;; =====================================================================
;;;; 5. The view verbs R91 and R92 touched
;;;; =====================================================================

(ert-deftest org-air-echo-board-view-verbs-report-their-new-state ()
  "Sort and the filter combinator say what they switched TO."
  (skip-unless (fboundp 'org-air-view-sort-cycle))
  (org-air-landing-test-with-board
    (should (string-match-p "\\`org-air: sort by "
                            (or (org-air-landing-test-echo-of
                                 #'org-air-view-sort-cycle) "")))
    (should (string-match-p "\\`org-air: sort "
                            (or (org-air-landing-test-echo-of
                                 #'org-air-view-sort-reverse) "")))
    (should (member (org-air-landing-test-echo-of
                     #'org-air-filter-toggle-match)
                    '("Filter match: AND" "Filter match: OR")))))

(ert-deftest org-air-echo-project-verbs-report-their-new-state ()
  "The project's sort, grouping and fold verbs each report their state,
including the honest no-op when there is nothing to unfold."
  (skip-unless (fboundp 'org-air-project-toggle-filenames))
  (org-air-landing-test-with-project
    (should (equal "org-air project: showing file names"
                   (org-air-landing-test-echo-of
                    #'org-air-project-toggle-filenames)))
    (should (equal "org-air project: showing titles"
                   (org-air-landing-test-echo-of
                    #'org-air-project-toggle-filenames)))
    (should (string-match-p "\\`org-air: sort by "
                            (or (org-air-landing-test-echo-of
                                 #'org-air-view-sort-cycle) "")))
    ;; TAB away from any fold row, with every group expanded, is a no-op
    ;; that SAYS it is one rather than moving the cursor silently.
    (let ((pos (text-property-not-all (point-min) (point-max)
                                      'org-air-dropped-fold nil)))
      (should pos)
      (goto-char pos)
      (org-air-project-toggle-dropped)          ; expand: no fold row left
      (goto-char (point-min))
      (should (equal "org-air project: no dropped folds"
                     (org-air-landing-test-echo-of
                      #'org-air-project-toggle-dropped))))))

(ert-deftest org-air-echo-revisit-verbs-report-their-new-state ()
  "The revisit surface cycle, the created column and the TAB no-op."
  (skip-unless (fboundp 'org-air-revisit-cycle-surface))
  (org-air-landing-test-with-revisit
    (should (equal "org-air revisit: orphans"
                   (org-air-landing-test-echo-of
                    #'org-air-revisit-cycle-surface)))
    (should (string-match-p "\\`org-air revisit: created column "
                            (or (org-air-landing-test-echo-of
                                 #'org-air-revisit-toggle-created) "")))
    (org-air-landing-test-goto "Note number 03")
    (should (equal "org-air revisit: nothing to expand here"
                   (org-air-landing-test-echo-of
                    #'org-air-revisit-toggle-more)))))

(ert-deftest org-air-echo-review-verbs-report-their-new-state ()
  "The review rollup and the range ladder report their new state, and
the ladder says when it has hit its end instead of silently doing
nothing."
  (skip-unless (fboundp 'org-air-review-cycle-rollup))
  (org-air-landing-test-with-review
    (should (string-match-p "\\`org-air review: rollup by "
                            (or (org-air-landing-test-echo-of
                                 #'org-air-review-cycle-rollup) "")))
    (should (string-match-p "\\`org-air review: by "
                            (or (org-air-landing-test-echo-of
                                 #'org-air-review-cycle-range) "")))
    ;; Widen to the end of the ladder: the LAST widen must announce it.
    (let ((echo nil))
      (dotimes (_ 8)
        (setq echo (org-air-landing-test-echo-of
                    #'org-air-review-range-widen)))
      (should (string-match-p "\\`org-air review: widest range (" echo)))
    (let ((echo nil))
      (dotimes (_ 8)
        (setq echo (org-air-landing-test-echo-of
                    #'org-air-review-range-narrow)))
      (should (string-match-p "\\`org-air review: narrowest range (" echo)))))

;;;; =====================================================================
;;;; 6. The coverage claim is mechanical, and honest
;;;; =====================================================================

(defconst org-air-echo-test-modules
  '("org-air-view.el" "org-air-project.el" "org-air-revisit.el"
    "org-air-review.el")
  "The four view modules whose echo text this family owns.")

(defconst org-air-echo-test-covered
  '(;; marks (R90)
    "%s %d item%s — b backlog, t add tag, M clears"
    "No marked items"
    "Cleared all marks"
    "Pruned %d stale marked item%s"
    "%s is single-item while %d marks are active; M clears marks"
    ;; bulk verbs (R90) — the base line and the `already' bucket
    "%s%s"
    "%d already %s"
    ;; single-item verbs
    "%s \"%s\""
    "Marked DONE \"%s\""
    "Archived \"%s\""
    "Todo \"%s\": %s → %s"
    "Todo unchanged"
    ;; undo / redo (R73/R75/R90)
    "Nothing to undo"
    "Nothing to redo"
    "Undid: %s (%d more)"
    ;; refusals
    "No org-air item at point"
    "No note at point"
    "No Air document on this line"
    "Not in an org-air board or project buffer"
    ;; the view verbs R91/R92 touched
    "org-air: sort by %s"
    "org-air: sort %s"
    "Filter match: %s"
    "org-air project: showing %s"
    "org-air project: no dropped folds"
    "org-air revisit: %s"
    "org-air revisit: created column %s"
    "org-air revisit: nothing to expand here"
    "org-air review: rollup by %s"
    "org-air review: by %s"
    "org-air review: widest range (%s)"
    "org-air review: narrowest range (%s)")
  "The echo-area format strings this suite asserts the TEXT of.
Every entry must really appear in the source; see
`org-air-echo-covered-messages-really-exist'.  The messages NOT in this
list — the load/refresh failure paths, the bookmark-degraded warnings,
the capture / refile / inbox-triage prompts and the bulk-history
partial-commit lines — are named as the remaining gap in the round-92
Air history rather than left implied.")

(defun org-air-echo-test--module-text ()
  "Return the concatenated source text of the four view modules."
  (let ((root (file-name-directory (or (locate-library "org-air")
                                       default-directory)))
        (parts nil))
    (dolist (module org-air-echo-test-modules)
      (let ((file (expand-file-name module root)))
        (when (file-readable-p file)
          (with-temp-buffer
            (insert-file-contents file)
            (push (buffer-string) parts)))))
    (mapconcat #'identity (nreverse parts) "\n")))

(defun org-air-echo-test--inventory ()
  "Return every literal `message'/`user-error' format string in the four
view modules — the mechanical inventory the coverage claim is measured
against.  Strings are DECODED (`read'), so what comes back is the text
the format call really carries, not its source spelling."
  (let ((acc nil))
    (with-temp-buffer
      (insert (org-air-echo-test--module-text))
      (goto-char (point-min))
      (while (re-search-forward
              "(\\(?:message\\|user-error\\)[ \t\n]+\""
              nil t)
        (goto-char (1- (match-end 0)))
        (let ((s (ignore-errors (read (current-buffer)))))
          (when (stringp s) (push s acc)))))
    (delete-dups (nreverse acc))))

(ert-deftest org-air-echo-inventory-is-mechanically-derived ()
  "The inventory really is scraped from the shipped source, not typed.
A hand-written list would rot the moment a message changed; this asserts
the scan found the four modules and a substantial number of strings."
  (skip-unless (locate-library "org-air"))
  (let ((inventory (org-air-echo-test--inventory)))
    (should (< 50 (length inventory)))
    ;; Spot-check one string from each module so a scan that silently
    ;; read the wrong files cannot pass.
    (should (member "Pruned %d stale marked item%s" inventory))
    (should (member "org-air project: no dropped folds" inventory))
    (should (member "org-air revisit: nothing to expand here" inventory))
    (should (member "org-air review: rollup by %s" inventory))))

(ert-deftest org-air-echo-covered-messages-really-exist ()
  "Every message this suite CLAIMS to cover exists in the shipped source.
Without this the covered list could quietly describe a product that no
longer says any of it."
  (skip-unless (locate-library "org-air"))
  (let* ((text (org-air-echo-test--module-text))
         (phantom (seq-remove
                   (lambda (s)
                     ;; The source spelling of the string — so an entry
                     ;; carrying escaped quotes is matched as written.
                     (string-match-p (regexp-quote (prin1-to-string s)) text))
                   org-air-echo-test-covered)))
    (should (equal '() (sort (copy-sequence phantom) #'string<)))))

(provide 'org-air-invariant-echo-test)
;;; org-air-invariant-echo-test.el ends here
