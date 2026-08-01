;;; org-air-invariant-landing-test.el --- invariant family P2 -*- lexical-binding: t; -*-

;;; Commentary:
;; INVARIANT FAMILY P2 — where the cursor lands after EVERY command.
;;
;; The R91 distinguished review named this the highest-value missing
;; family: "There is no single law saying where point ends up.  Ten files
;; touch `--goto-row-title' opportunistically."  Two shipped defects lived
;; in that hole for ninety rounds — the project / revisit / review
;; first-row landing, and the two TAB commands that threw the cursor's row
;; 30-40 screen lines off — while 1204 tests were green.
;;
;; This file states the law once, for every bound command of all four
;; org-air mode maps:
;;
;;   1. point lands on a VISIBLE GLYPH of a row — never in the leading
;;      gutter, never past the row's last glyph, never on `point-min' by
;;      accident;
;;   2. the window's point equals the buffer's point, so the cursor the
;;      user sees is the cursor the command placed;
;;   3. the row's IDENTITY is preserved — unless the command is declared
;;      JUMP, or the row genuinely vanished from the buffer.
;;
;; THE TABLE IS THE TEST.  `org-air-landing-invariant-table' declares
;; every command reachable from the four maps as STABLE, JUMP or NOT-RUN
;; (with a reason).  `org-air-landing-invariant-table-is-complete' walks
;; the four maps with `map-keymap' and fails if any bound command is
;; missing from the table — so a NEW key cannot be added without someone
;; deciding, in writing, where its cursor lands.
;;
;; Identity is read out of the BUFFER TEXT (`Task number 40',
;; `Document number 33', `Note number 09'), never out of a struct or a
;; text property, so no assertion here names an org-air internal.
;;
;; Commands are invoked the way the command loop invokes them —
;; `pre-command-hook', `call-interactively', `post-command-hook' — so the
;; view's own point normalisation runs exactly as it does for a user.
;; Prompting commands get deterministic readers (see
;; `org-air-landing-invariant--with-answers'); a refusal (`user-error') is
;; a legitimate outcome and the landing law still applies to it: a command
;; that refuses must not move the cursor either.
;;
;; PROVEN TO HAVE TEETH: with R92's landing rule reverted (the three
;; non-board renders re-landing on their first row), the STABLE rows of
;; this table redden — see the round-92 Air history for the named list.

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
;;;; THE TABLE
;;;; =====================================================================
;;
;; Each row is (COMMAND CLASS PARK NOTE).
;;
;;   CLASS  stable   a repaint (or a no-op): the row the user is standing
;;                   on must still be under the cursor afterwards, unless
;;                   that row genuinely vanished from the buffer.
;;          jump     the command deliberately goes somewhere else — plain
;;                   motion, a view ENTRY, a board open, a visit into
;;                   another buffer, a bookmark restore.  The landing must
;;                   still be a real row glyph with the window following
;;                   it; the identity may change.
;;          not-run  declared and deliberately NOT executed.  NOTE says
;;                   why.  Every one of these either leaves the view, owns
;;                   another UI, or is an Emacs-core command inherited
;;                   from `special-mode-map' that org-air makes no landing
;;                   promise about.
;;
;;   PARK   row      park on the view's anchor item/doc/note row (default);
;;          section  park on the view's section header / fold row — the
;;                   row the structural verbs act on.

(defconst org-air-landing-invariant-table
  '(;; ---- board / shared repaint verbs -------------------------------
    (org-air-toggle-mark              stable  row)
    (org-air-clear-marks              stable  row)
    (org-air-item-backlog             stable  row)
    (org-air-item-done                stable  row)
    (org-air-item-cycle-todo          stable  row)
    (org-air-item-archive             stable  row)
    (org-air-item-kill                stable  row)
    (org-air-set-tag                  stable  row)
    (org-air-item-set-deadline        stable  row)
    (org-air-item-file-group          stable  row)
    (org-air-edit-undo                stable  row)
    (org-air-edit-redo                stable  row)
    (org-air-process-inbox            stable  row)
    (org-air-refresh                  stable  row)
    (org-air-refresh-all              stable  row)
    (org-air-filter                   stable  row)
    (org-air-filter-clear             stable  row)
    (org-air-filter-toggle-match      stable  row)
    (org-air-scope                    stable  row)
    (org-air-scope-clear              stable  row)
    (org-air-view-sort-cycle          stable  row)
    (org-air-view-sort-reverse        stable  row)
    (org-air-rail-toggle              stable  row)
    (org-air-toggle-dates             stable  row)
    (org-air-toggle-origin            stable  row)
    (org-air-toggle-tags              stable  row)
    (org-air-calendar-next            stable  row)
    (org-air-calendar-prev            stable  row)
    (org-air-calendar-today           stable  row)
    (org-air-toggle-section           stable  section)
    (org-air-peek-item                stable  row)
    ;; ---- board motion ------------------------------------------------
    (org-air-next-item                jump    row)
    (org-air-prev-item                jump    row)
    (org-air-next-line                jump    row)
    (org-air-prev-line                jump    row)
    (org-air-next-section             jump    row)
    (org-air-prev-section             jump    row)
    (org-air-forward-section          jump    row)
    (org-air-back-section             jump    row)
    (org-air-goto-top                 jump    row)
    (org-air-goto-bottom              jump    row)
    ;; ---- project -----------------------------------------------------
    (org-air-project-refresh          stable  row)
    (org-air-project-group-by-state   stable  row)
    (org-air-project-group-by-directory stable row)
    (org-air-project-group-by-tag     stable  row)
    (org-air-project-toggle-filenames stable  row)
    (org-air-project-filter           stable  row)
    (org-air-project-toggle-dropped   stable  section)
    (org-air-project-next             jump    row)
    (org-air-project-prev             jump    row)
    ;; ---- revisit -----------------------------------------------------
    (org-air-revisit-refresh          stable  row)
    (org-air-revisit-cycle-surface    stable  row)
    (org-air-revisit-toggle-created   stable  row)
    (org-air-revisit-filter           stable  row)
    (org-air-revisit-toggle-more      stable  section)
    (org-air-revisit-next             jump    row)
    (org-air-revisit-prev             jump    row)
    ;; ---- review ------------------------------------------------------
    (org-air-review-refresh           stable  row)
    (org-air-review-cycle-rollup      stable  row)
    (org-air-review-cycle-range       stable  row)
    (org-air-review-range-widen       stable  row)
    (org-air-review-range-narrow      stable  row)
    (org-air-review-scope             stable  row)
    (org-air-review-scope-clear       stable  row)
    (org-air-review-filter            stable  row)
    (org-air-review-toggle-section    stable  section)
    (org-air-review-period-prev       jump    row)
    (org-air-review-period-next       jump    row)
    (org-air-review-period-today      jump    row)
    (org-air-review-next              jump    row)
    (org-air-review-prev              jump    row)
    ;; ---- explicit JUMPS: entry, open, visit --------------------------
    (org-air-project                  jump    row)
    (org-air-revisit                  jump    row)
    (org-air-review                   jump    row)
    (org-air-visit-item               jump    row)
    (org-air-visit-item-stay          jump    row)
    (org-air-project-open             jump    row)
    (org-air-project-visit            jump    row)
    (org-air-revisit-open             jump    row)
    (org-air-revisit-visit            jump    row)
    ;; ---- declared, deliberately NOT executed --------------------------
    (org-air-quit          not-run nil "leaves the view: a buried buffer has no landing")
    (org-air-project-quit  not-run nil "leaves the view")
    (org-air-revisit-quit  not-run nil "leaves the view")
    (org-air-review-quit   not-run nil "leaves the view")
    (org-air-help          not-run nil "opens the help UI, which owns its own landing")
    (org-air-capture       not-run nil "opens an org-capture buffer, which owns its own landing")
    (org-air-refile-item   not-run nil "drives the org-refile UI; landing is org-refile's")
    (org-air-goto-date     not-run nil "prompts for a date and opens the day view: covered by the day-view suites")
    (org-air-rail-return   not-run nil "window-layout verb: returns to the rail side window")
    (org-air-view-pane     not-run nil "window-layout verb: the pane suites (r16-d3) are the GUI-skipped owner")
    (org-air-view-pane-close not-run nil "window-layout verb: pane lifecycle, see r16-d3")
    (org-air-view-pane-return not-run nil "window-layout verb: pane lifecycle, see r16-d3")
    ;; ---- Emacs core, inherited from `special-mode-map' ---------------
    (beginning-of-buffer   not-run nil "Emacs core (special-mode): org-air promises no landing")
    (end-of-buffer         not-run nil "Emacs core (special-mode)")
    (scroll-up-command     not-run nil "Emacs core (special-mode)")
    (scroll-down-command   not-run nil "Emacs core (special-mode)")
    (digit-argument        not-run nil "Emacs core: a prefix argument, not a landing")
    (negative-argument     not-run nil "Emacs core: a prefix argument, not a landing")
    (quit-window           not-run nil "Emacs core: leaves the view")
    (revert-buffer         not-run nil "Emacs core (special-mode): org-air rebinds `g'")
    (describe-mode         not-run nil "Emacs core: opens the help UI")
    (undefined             not-run nil "Emacs core: the self-insert guard, by definition a no-op"))
  "Where the cursor lands after EVERY bound command of the four maps.
See the commentary for the classes.  This table is the contract: adding
a key to any org-air mode map without adding it here fails
`org-air-landing-invariant-table-is-complete'.")

(defconst org-air-landing-invariant-maps
  '(org-air-view-mode-map org-air-project-mode-map
    org-air-revisit-mode-map org-air-review-mode-map)
  "The four mode maps the table must cover, in full.")

(defun org-air-landing-invariant--pseudo-event-p (key)
  "Non-nil when KEY is an internal MARKER event, not a key a user presses.

R98 ORDER-DEPENDENCE FIX.  Evil registers its state machinery INSIDE the
mode map, under symbolic pseudo-events (`override-state',
`intercept-state', `motion-state', …) whose stored value is an evil STATE
name or an auxiliary keymap — `[override-state] -> motion' is what
`evil-mode' + one `org-air-view-mode' buffer leaves in
`org-air-view-mode-map', permanently, for the rest of the Emacs process.

That made this family ORDER-DEPENDENT: run before the evil suites
\(alphabetically, `org-air-landing-*' precedes `org-air-r27-*') the maps
are clean and the table is complete; run after them — in reverse or any
shuffled order — the walk found the state symbol `motion', demanded a
landing classification for it, and reddened five tests.  A suite that
passes only in one running order is not a gate.

Skipping these is not a weakening: they are not reachable keys.  No key
sequence produces them (they are markers evil looks up by name), and
`motion' is not even a command.  The law — every command a USER can
press is classified — is untouched."
  (and (symbolp key)
       (string-suffix-p "-state" (symbol-name key))))

(defun org-air-landing-invariant--map-commands (map)
  "Return every COMMAND SYMBOL bound anywhere in MAP (parents included).
`map-keymap' walks the parent chain, so the shared view-core map and the
`special-mode-map' inheritance come along — which is exactly right: a
user pressing a key does not know which map answered.
Internal marker events (`org-air-landing-invariant--pseudo-event-p') are
skipped: they are not keys, so nothing a user can press is lost."
  (let ((acc nil))
    (map-keymap
     (lambda (key def)
       (cond
        ((org-air-landing-invariant--pseudo-event-p key) nil)
        ((keymapp def)
         (setq acc (append (org-air-landing-invariant--map-commands def) acc)))
        ((and (symbolp def) def) (push def acc))))
     map)
    (delete-dups acc)))

(defun org-air-landing-invariant--all-commands ()
  "Return every command bound in the four org-air mode maps."
  (delete-dups
   (apply #'append
          (mapcar (lambda (m)
                    (org-air-landing-invariant--map-commands (symbol-value m)))
                  org-air-landing-invariant-maps))))

(defun org-air-landing-invariant--entry (command)
  "Return COMMAND's table row, or nil."
  (assq command org-air-landing-invariant-table))

;;;; =====================================================================
;;;; The table is complete, and honest
;;;; =====================================================================

(ert-deftest org-air-landing-invariant-table-is-complete ()
  "Every command bound in the four mode maps is classified in the table.
This is the whole point of the family: a new key must be given a landing
class DELIBERATELY, in writing, or the build fails."
  (skip-unless (boundp 'org-air-view-mode-map))
  (let ((missing (seq-remove #'org-air-landing-invariant--entry
                             (org-air-landing-invariant--all-commands))))
    (should (equal '() (sort (mapcar #'symbol-name missing) #'string<)))))

(ert-deftest org-air-landing-invariant-table-has-no-dead-rows ()
  "Every table row names a command that is really bound somewhere.
Keeps the table from rotting into a list of commands that no longer
exist — a stale classification is as misleading as a missing one."
  (skip-unless (boundp 'org-air-view-mode-map))
  (let* ((bound (org-air-landing-invariant--all-commands))
         (dead (seq-remove (lambda (row) (memq (car row) bound))
                           org-air-landing-invariant-table)))
    (should (equal '() (sort (mapcar (lambda (r) (symbol-name (car r))) dead)
                             #'string<)))))

(ert-deftest org-air-landing-invariant-every-row-is-classified ()
  "Every table row carries one of the three declared classes, and every
NOT-RUN row carries a written reason."
  (dolist (row org-air-landing-invariant-table)
    (should (memq (nth 1 row) '(stable jump not-run)))
    (when (eq (nth 1 row) 'not-run)
      (should (stringp (nth 3 row)))
      (should (< 10 (length (nth 3 row)))))
    (when (memq (nth 1 row) '(stable jump))
      (should (memq (nth 2 row) '(row section))))))

;;;; =====================================================================
;;;; The runner
;;;; =====================================================================

(defvar org-air-landing-invariant--answers nil
  "Deterministic answers fed to prompting commands under test.")

(defmacro org-air-landing-invariant--with-answers (&rest body)
  "Run BODY with every minibuffer reader answered deterministically.
`yes-or-no-p'/`y-or-n-p' answer NO — a REFUSAL is a legitimate outcome
and the landing law applies to it too: a command the user declines must
not move the cursor.  The single-key transient prompts (`read-char' and
friends) answer `t' — the org-air deadline/file-group idiom's first
option — so those verbs run their real repaint instead of hanging a
batch Emacs on a key that never arrives."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'completing-read)
              (lambda (_p collection &rest _)
                (or (car (all-completions "" collection)) "all")))
             ((symbol-function 'completing-read-multiple)
              (lambda (_p collection &rest _)
                (let ((c (car (all-completions "" collection))))
                  (and c (list c)))))
             ((symbol-function 'read-string) (lambda (&rest _) ""))
             ((symbol-function 'read-from-minibuffer) (lambda (&rest _) ""))
             ((symbol-function 'read-directory-name)
              (lambda (&rest _) org-air-landing-test--dir))
             ((symbol-function 'read-file-name)
              (lambda (&rest _) org-air-landing-test--dir))
             ((symbol-function 'read-char) (lambda (&rest _) ?t))
             ((symbol-function 'read-char-exclusive) (lambda (&rest _) ?t))
             ((symbol-function 'read-char-choice) (lambda (&rest _) ?t))
             ;; NOT `read-event': `sit-for' is built on it, so stubbing it
             ;; turns every timed wait into an endless loop.
             ((symbol-function 'read-key) (lambda (&rest _) ?t))
             ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
             ((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
             ((symbol-function 'org-read-date) (lambda (&rest _) "2026-06-20")))
     ,@body))

(defun org-air-landing-invariant--invoke (command)
  "Run COMMAND the way the command loop does; return its error, if any.
`pre-command-hook' and `post-command-hook' are run around
`call-interactively' so the view's own point normalisation fires exactly
as it does for a user."
  (run-hooks 'pre-command-hook)
  (unwind-protect
      (condition-case err
          (progn (org-air-landing-invariant--with-answers
                   (call-interactively command))
                 nil)
        (error err))
    (run-hooks 'post-command-hook)))

(defun org-air-landing-invariant--assert (command class buffer win before)
  "Assert COMMAND's landing in BUFFER/WIN against the BEFORE snapshot.
BEFORE is (IDENTITY . POINT)."
  (let ((label (symbol-name command)))
    (cond
     ;; The command left the view entirely (a visit, a view switch).  The
     ;; landing law then applies to the buffer it went to only if that
     ;; buffer is an org-air view; the view we came from must simply not
     ;; have been disturbed.
     ((not (eq (current-buffer) buffer))
      (should (memq class '(jump)))
      (with-current-buffer buffer
        (should (equal (list label (cdr before)) (list label (point))))))
     (t
      ;; 1. the cursor is on a real glyph of a row.  An EMPTY view (a
      ;;    period with no work in it) has no row to land on, and that is
      ;;    stated here rather than hidden inside the predicate.
      (when (org-air-landing-test-any-row-p)
        (should (equal (list label "on a row glyph" t)
                       (list label "on a row glyph"
                             (org-air-landing-test-on-glyph-p)))))
      ;; 2. the window follows the buffer's point.
      (when (and (window-live-p win) (eq (window-buffer win) buffer))
        (should (equal (list label "window-point = point" t)
                       (list label "window-point = point"
                             (= (window-point win) (point))))))
      ;; 3. STABLE: the same row is still under the cursor, unless it
      ;;    genuinely vanished from the buffer.
      (when (eq class 'stable)
        (let ((id (car before)))
          (when (and id (save-excursion
                          (goto-char (point-min))
                          (search-forward id nil t)))
            (should (equal (list label "row identity" id)
                           (list label "row identity"
                                 (org-air-landing-test-row-id))))))))))
  t)

(defun org-air-landing-invariant--park (win park anchor section)
  "Park point on ANCHOR (PARK `row') or SECTION (PARK `section').
Returns non-nil when the requested row exists in this render."
  (goto-char (point-min))
  (let ((pos (if (eq park 'section)
                 (funcall section)
               (and (re-search-forward anchor nil t)
                    (progn (beginning-of-line) (point))))))
    (when pos
      (goto-char pos)
      (org-air-view--goto-row-title)
      ;; Park the row a few screen lines down where the buffer allows it —
      ;; a row near the very top cannot sit 4 lines below `window-start'.
      (org-air-landing-test-park win (min 4 (1- (line-number-at-pos))))
      t)))

(defun org-air-landing-invariant--run (map-symbol buffer anchors section reset)
  "Drive every STABLE/JUMP command of MAP-SYMBOL over BUFFER's view.
ANCHORS is a list of row regexps used round-robin, so a MUTATING verb
never destroys the row the next command needs.  SECTION returns the
position of the view's section / fold row.  RESET is called with the
anchor and restores the view to a baseline in which that row is on
screen.  Returns the commands exercised.

The window is re-derived every iteration and the window configuration is
restored after any command that left the view, so a visit or a view
switch cannot silently starve the rest of the run — every STABLE/JUMP
command really is executed."
  (let ((config (current-window-configuration))
        (queue anchors)
        (ran nil))
    (dolist (command (sort (org-air-landing-invariant--map-commands
                            (symbol-value map-symbol))
                           (lambda (a b) (string< (symbol-name a)
                                                  (symbol-name b)))))
      (let ((entry (org-air-landing-invariant--entry command)))
        (should entry)
        (when (and (memq (nth 1 entry) '(stable jump))
                   (commandp command))
          (set-buffer buffer)
          (let* ((anchor (car queue))
                 (win nil))
            (setq queue (or (cdr queue) anchors))
            (funcall reset anchor)
            (setq win (get-buffer-window buffer))
            (should (window-live-p win))
            (select-window win)
            (unless (org-air-landing-invariant--park win (nth 2 entry)
                                                     anchor section)
              ;; The previous command may have popped the rail into a side
              ;; window, which narrows the view and truncates row titles.
              ;; Pop it back in and re-baseline before giving up.
              (ignore-errors (org-air-rail-toggle))
              (funcall reset anchor)
              (setq win (get-buffer-window buffer))
              (when (window-live-p win) (select-window win)))
            (should (equal (list command anchor "parked" t)
                           (list command anchor "parked"
                                 (and (window-live-p win)
                                      (org-air-landing-invariant--park
                                       win (nth 2 entry) anchor section)))))
            (let ((before (cons (org-air-landing-test-row-id) (point))))
              (org-air-landing-invariant--invoke command)
              (org-air-landing-invariant--assert command (nth 1 entry)
                                                 buffer win before)
              (push command ran)
              ;; Come back to the view if the command left it.
              (unless (eq (current-buffer) buffer)
                (set-window-configuration config)
                (set-buffer buffer)
                (let ((w (get-buffer-window buffer)))
                  (when (window-live-p w) (select-window w)))))))))
    (nreverse ran)))

;;;; =====================================================================
;;;; One test per view
;;;; =====================================================================

(defun org-air-landing-invariant--anchors (format indices)
  "Return row regexps built from FORMAT over INDICES."
  (mapcar (lambda (i) (format format i)) indices))

(ert-deftest org-air-landing-invariant-board-commands-land-on-a-row ()
  "Every bound board command lands the cursor on a row, per the table."
  (skip-unless (fboundp 'org-air))
  (org-air-landing-test-with-board
    (let ((buffer (current-buffer)))
      (let ((ran (org-air-landing-invariant--run
                  'org-air-view-mode-map buffer
                  (org-air-landing-invariant--anchors
                   "Task number %02d" (number-sequence 5 54))
                  (lambda () (org-air-view--find-property
                              'org-air-section 'attention))
                  (lambda (anchor)
                    (ignore-errors (org-air-filter-clear))
                    (ignore-errors (org-air-scope-clear))
                    (ignore-errors (org-air-clear-marks))
                    ;; TAB may have collapsed the section the anchors live
                    ;; in (collapsed, it renders only its first few rows);
                    ;; re-expand so every command gets a real row.
                    (save-excursion
                      (goto-char (point-min))
                      (unless (re-search-forward anchor nil t)
                        (let ((pos (org-air-view--find-property
                                    'org-air-section 'attention)))
                          (when pos
                            (goto-char pos)
                            (org-air-toggle-section)))))))))
        ;; The run really exercised the board's own verbs.
        (should (memq 'org-air-toggle-mark ran))
        (should (memq 'org-air-toggle-section ran))
        (should (memq 'org-air-refresh ran))
        (should (memq 'org-air-visit-item ran))
        (should (< 25 (length ran)))))))

(ert-deftest org-air-landing-invariant-project-commands-land-on-a-row ()
  "Every bound project command lands the cursor on a row, per the table."
  (skip-unless (fboundp 'org-air-project))
  (org-air-landing-test-with-project
    (let ((buffer (current-buffer)))
      (let ((ran (org-air-landing-invariant--run
                  'org-air-project-mode-map buffer
                  (org-air-landing-invariant--anchors
                   "Document number %02d"
                   (seq-filter (lambda (i) (/= 1 (% i 3)))
                               (number-sequence 3 39)))
                  (lambda () (text-property-not-all (point-min) (point-max)
                                                    'org-air-dropped-fold nil))
                  (lambda (anchor)
                    (ignore-errors (org-air-project-filter nil))
                    (ignore-errors (org-air-project-group-by-state))
                    ;; `(' may have flipped the rows to filenames.
                    (save-excursion
                      (goto-char (point-min))
                      (unless (re-search-forward anchor nil t)
                        (org-air-project-toggle-filenames)))))))
        (should (memq 'org-air-project-refresh ran))
        (should (memq 'org-air-project-toggle-dropped ran))
        (should (memq 'org-air-project-group-by-tag ran))
        (should (memq 'org-air-project-open ran))
        (should (< 12 (length ran)))))))

(ert-deftest org-air-landing-invariant-revisit-commands-land-on-a-row ()
  "Every bound revisit command lands the cursor on a row, per the table."
  (skip-unless (fboundp 'org-air-revisit))
  (org-air-landing-test-with-revisit
    (let ((buffer (current-buffer)))
      (let ((ran (org-air-landing-invariant--run
                  'org-air-revisit-mode-map buffer
                  (org-air-landing-invariant--anchors
                   "Note number %02d" (number-sequence 0 9))
                  (lambda () (text-property-not-all (point-min) (point-max)
                                                    'org-air-more-row nil))
                  (lambda (anchor)
                    (ignore-errors (org-air-revisit-filter nil))
                    ;; `m' cycles to a surface that shows fewer notes and
                    ;; TAB pages; come back to ALL when the row is gone.
                    (save-excursion
                      (goto-char (point-min))
                      (unless (re-search-forward anchor nil t)
                        (dotimes (_ 3)
                          (goto-char (point-min))
                          (unless (re-search-forward anchor nil t)
                            (org-air-revisit-cycle-surface)))))))))
        (should (memq 'org-air-revisit-refresh ran))
        (should (memq 'org-air-revisit-toggle-more ran))
        (should (memq 'org-air-revisit-cycle-surface ran))
        (should (memq 'org-air-revisit-open ran))
        (should (< 12 (length ran)))))))

(ert-deftest org-air-landing-invariant-review-commands-land-on-a-row ()
  "Every bound review command lands the cursor on a row, per the table."
  (skip-unless (fboundp 'org-air-review))
  (org-air-landing-test-with-review
    (let ((buffer (current-buffer)))
      (let ((ran (org-air-landing-invariant--run
                  'org-air-review-mode-map buffer
                  (org-air-landing-invariant--anchors
                   "Task number %02d" (number-sequence 0 29))
                  (lambda () (org-air-view--find-property
                              'org-air-section 'carried))
                  (lambda (anchor)
                    (ignore-errors (org-air-review-period-today))
                    (ignore-errors (org-air-review-filter nil))
                    (ignore-errors (org-air-review-scope-clear))
                    ;; TAB may have collapsed the section the anchors are
                    ;; in; re-expand so every command gets a real row.
                    (save-excursion
                      (goto-char (point-min))
                      (unless (re-search-forward anchor nil t)
                        (let ((pos (org-air-view--find-property
                                    'org-air-section 'completed)))
                          (when pos
                            (goto-char pos)
                            (org-air-review-toggle-section)))))))))
        (should (memq 'org-air-review-refresh ran))
        (should (memq 'org-air-review-toggle-section ran))
        (should (memq 'org-air-item-backlog ran))
        (should (memq 'org-air-visit-item ran))
        (should (< 18 (length ran)))))))

(provide 'org-air-invariant-landing-test)
;;; org-air-invariant-landing-test.el ends here
