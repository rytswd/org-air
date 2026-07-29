;;; org-air-round97-test.el --- R97: the two instruments -*- lexical-binding: t; -*-

;;; Commentary:
;; Round 97 is not another hundred ERTs in the idiom that already has
;; 1345 of them.  The R96 reviewer was explicit:
;;
;;   "Do not add more ERTs in the current idiom.  The marginal 1346th
;;    `with-board' test is worth close to zero.  The marginal FIRST
;;    foreign-buffer test is worth more than the previous hundred."
;;
;; So this file builds the two instruments it named, and nothing else.
;;
;; ── INSTRUMENT B-1 — the foreign-buffer sweep ────────────────────────
;;
;; The gap: no test invoked an org-air command from a buffer that is not
;; an org-air buffer.  21% of the command surface misbehaved there, and
;; the suite scored 1345/1345 anyway.  Machinery in
;; `tests/org-air-foreign-buffer-sweep.el'; the contract is asserted
;; here, table-driven over every `commandp' org-air symbol discovered at
;; runtime, in a fresh `org-mode' FILE buffer and again in a
;; `fundamental-mode' one:
;;
;;   (a) the foreign buffer's text is unchanged
;;   (b) `buffer-modified-p' is nil            (so `C-x C-s' is harmless)
;;   (c) nothing was written to disk, and no file appeared in its tree
;;   (d) any signal is a `user-error' — never a bare `error', never a
;;       `wrong-type-argument'
;;   (e) NO prompt was issued before the refusal
;;
;; and, for the surface-scoped set, that the command actually REFUSES
;; rather than silently doing nothing.
;;
;; RED on the pre-R97 tree — measured, not assumed: `main' (R96) was
;; materialised into /tmp with these very test files copied over it, and
;; all FOUR B-1 tests fail there, naming their offenders with byte
;; counts (`org-air-refresh  destroys  bytes=30->476 MODIFIED').
;;
;;   | class              | pre-R97 org | pre-R97 fund | R97 (both) |
;;   |--------------------+-------------+--------------+------------|
;;   | destroys           |           8 |            8 |          0 |
;;   | raw error          |           4 |            7 |          0 |
;;   | prompts-then-fails |          18 |           18 |          2 |
;;   | refuses            |          24 |           24 |        117 |
;;   | silent no-op       |          78 |           75 |         13 |
;;
;; The 8 destroyers are the reviewer's set exactly.  See the header of
;; tests/org-air-foreign-buffer-sweep.el for why the raw-error and prompt
;; columns are 4/18 rather than the design doc's 3/19 (this sweep gives
;; each command a FRESH transient form state, which is stricter).
;;
;; The 2 remaining prompts and the 13 no-ops are the same 15 commands —
;; the declared `org-air-r97-anywhere-commands' allow-list below, each
;; with a REASON.  The allow-list is asserted exhaustive and non-stale in
;; both directions, so a command added next round cannot inherit an
;; exemption by accident: it either meets the refusal contract or someone
;; writes down why it does not (the R92 landing-table discipline).
;;
;; ── INSTRUMENT B-2 — the interactive/asynchronous axis ───────────────
;;
;; `noninteractive' is t in 100% of the gate, so the loading skeleton,
;; the adaptive pacer and the single swap have never been observed by any
;; test.  `org-air-test-with-async-scan' (tests/org-air-test-helpers.el)
;; binds it to nil and `org-air-test-pump-timers' lets the REAL timer
;; queue run; the sequence is pinned once, here.
;;
;; The other half of B-2 is `make check-gui' (tests/org-air-gui-runner.el),
;; which drives ERT under a real display so the 8 `display-graphic-p'
;; tests — never executed once in this project's history — finally run.
;; It is deliberately NOT in the default gate: it needs a display the CI
;; or the user may not have.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'org-air-test-helpers)

;; org-air FIRST, so every variable the sweep binds is already special
;; and `org-air-sweep-run''s `let' is dynamic (see the defvar block in
;; org-air-foreign-buffer-sweep.el for why that matters).
(eval-and-compile
  (when (locate-library "org-air")
    (require 'org-air)
    (require 'org-air-view)
    (require 'org-air-project)
    (require 'org-air-review)
    (require 'org-air-revisit)
    (require 'org-air-inbox)
    (require 'org-air-calendar)
    (require 'org-air-faces)))

(require 'org-air-foreign-buffer-sweep)

;;;; ---------------------------------------------------------------------
;;;; The declared allow-list: commands whose contract is to work ANYWHERE
;;;; ---------------------------------------------------------------------

(defconst org-air-r97-anywhere-commands
  '((org-air
     . "global entry point: opens the board in its OWN buffer and leaves \
yours alone — the one command a stranger types first")
    (org-air-view
     . "the `org-air' entry point under its explicit name; same contract")
    (org-air-review
     . "global entry point: opens the review view in its own buffer")
    (org-air-revisit
     . "global entry point: opens the revisit view in its own buffer")
    (org-air-project
     . "global entry point: READS a project root and then opens the tree \
— the prompt IS the contract, in any buffer")
    (org-air-goto-date
     . "global entry point: READS a date and then opens the board on it \
— the prompt IS the contract, in any buffer")
    (org-air-process-inbox
     . "global entry point: the inbox walk opens its own surface")
    (org-air-scan-report
     . "global diagnostic: reports what the last scan skipped (R97 D3); \
useful precisely when you are standing in the file that went missing")
    (org-air-help
     . "global help: opens the key table in its own buffer")
    (org-air-capture
     . "NOT exempt — listed here only to be explicit that it is not: R97 \
D2 makes it refuse before the title prompt when the inbox is outside \
`org-air-files'.  See `org-air-r97--anywhere-p'.")
    (org-air-outline-next-heading
     . "buffer-local MOTION for `org-air-outline-mode': its contract is \
the CURRENT buffer's headings, so it is correct anywhere and destroys \
nothing")
    (org-air-outline-prev-heading
     . "buffer-local motion for `org-air-outline-mode' (see above)")
    (org-air-outline-goto-current-heading
     . "buffer-local motion for `org-air-outline-mode' (see above)")
    (org-air--repeat-next
     . "buffer-local repeat-map motion for a project doc session; a \
no-op with no session, never a mutation")
    (org-air--repeat-prev
     . "buffer-local repeat-map motion for a project doc session (above)")
    (org-air-faces-link-nano
     . "face helper: links org-air faces to the nano theme; touches no \
buffer at all"))
  "Commands allowed to prompt or act from a NON-org-air buffer, with reasons.

Everything else in the org-air command surface must REFUSE out of
context: no prompt, no mutation, a clean `user-error'.  This table is
asserted exhaustive and non-stale in both directions
\(`org-air-r97-b1-3-allow-list-is-declared-and-honest'), so a command
added in a later round is CLASSIFIED deliberately rather than exempted by
accident.

`org-air-capture' carries a reason but is NOT exempt — the entry exists
so the next reader does not \"obviously\" add it.")

(defun org-air-r97--anywhere-p (cmd)
  "Non-nil when CMD is on the declared works-anywhere allow-list."
  (and (assq cmd org-air-r97-anywhere-commands)
       (not (eq cmd 'org-air-capture))))

(defconst org-air-r97--anywhere-count 15
  "How many commands the allow-list actually exempts (R97's own count).")

;;;; ---------------------------------------------------------------------
;;;; The sweep, memoised per mode (it is the expensive fixture)
;;;; ---------------------------------------------------------------------

(defvar org-air-r97--sweep-cache nil
  "Alist of MODE . RESULTS, so each mode is swept at most once per process.")

(defun org-air-r97--sweep (mode)
  "Return the foreign-buffer sweep results for MODE, computing them once."
  (or (cdr (assq mode org-air-r97--sweep-cache))
      (let ((results (org-air-sweep-run mode)))
        (push (cons mode results) org-air-r97--sweep-cache)
        results)))

(defun org-air-r97--offenders (results pred)
  "Return human-readable lines for every RESULTS entry failing PRED."
  (mapcar #'org-air-sweep-describe
          (seq-remove pred results)))

;;;; ---------------------------------------------------------------------
;;;; B-1/1-2 — the sweep contract, per foreign mode
;;;; ---------------------------------------------------------------------

(defun org-air-r97--assert-sweep (mode)
  "Assert the whole foreign-buffer contract for MODE.
Split into named legs so a failure says WHICH guarantee broke, and each
leg reports every offending command rather than only the first."
  (let* ((results (org-air-r97--sweep mode))
         (strict (seq-remove (lambda (r)
                               (org-air-r97--anywhere-p (plist-get r :command)))
                             results))
         (anywhere (seq-filter (lambda (r)
                                 (org-air-r97--anywhere-p
                                  (plist-get r :command)))
                               results)))
    ;; vacuity guard: the sweep must actually have swept the surface.
    (should (>= (length results) 130))
    (should (= (length anywhere) org-air-r97--anywhere-count))
    (should (>= (length strict) 110))

    ;; (a) text unchanged — the D1 data-loss leg, for EVERY command,
    ;;     allow-listed or not.  Nothing may edit the buffer you are in.
    (should (equal '() (org-air-r97--offenders
                        results (lambda (r) (not (plist-get r :text-changed))))))
    (should (equal '() (org-air-r97--offenders
                        results (lambda (r) (not (plist-get r :buffer-killed))))))

    ;; (b) `buffer-modified-p' nil — the leg that makes `C-x C-s' harmless.
    (should (equal '() (org-air-r97--offenders
                        results (lambda (r) (not (plist-get r :modified))))))

    ;; (c) nothing written to disk, and no file created in the tree.
    (should (equal '() (org-air-r97--offenders
                        results (lambda (r) (not (plist-get r :disk-changed))))))
    (should (equal '() (org-air-r97--offenders
                        results (lambda (r) (not (plist-get r :dir-changed))))))

    ;; (d) no raw error anywhere — the D5 leg (`wrong-type-argument' out
    ;;     of a package a stranger just installed, with a backtrace under
    ;;     `debug-on-error').
    (should (equal '() (org-air-r97--offenders
                        results (lambda (r)
                                  (not (eq (plist-get r :class) 'raw-error))))))

    ;; (e) no prompt before the refusal — the D4 leg.  Only the two
    ;;     allow-listed entry points that READ then OPEN may prompt.
    (should (equal '() (org-air-r97--offenders
                        strict (lambda (r) (= 0 (plist-get r :prompts))))))

    ;; and the positive contract: a surface-scoped verb REFUSES, loudly
    ;; and cleanly, rather than silently doing nothing.
    (should (equal '() (org-air-r97--offenders
                        strict (lambda (r) (eq (plist-get r :class) 'refuses)))))
    (should (equal '() (org-air-r97--offenders
                        strict (lambda (r) (eq (plist-get r :signal) 'user-error)))))

    ;; the classification totals, as a single readable summary.
    (should (equal '((destroys . 0) (raw-error . 0))
                   (seq-take (org-air-sweep-counts results) 2)))
    results))

(ert-deftest org-air-r97-b1-1-foreign-org-buffer-sweep ()
  "INSTRUMENT B-1: EVERY org-air command, driven by `call-interactively'
from a plain `org-mode' FILE buffer with `noninteractive' nil and every
reader stubbed to signal, leaves that buffer's text, modified-flag, file
bytes and directory untouched; never signals a bare `error' or a
`wrong-type-argument'; and — outside the declared
`org-air-r97-anywhere-commands' allow-list — refuses with a `user-error'
WITHOUT having asked a question first.

This is the test the R96 reviewer called the single highest-value one in
the report.  On the pre-R97 tree it is RED with 8 destroyers (which erase
the buffer, paint the board over it and leave it modified, so the next
`C-x C-s' writes the board to disk), 3 raw `wrong-type-argument' forms
and 19 prompt-then-fail commands."
  (skip-unless (locate-library "org-air"))
  (org-air-r97--assert-sweep 'org-mode))

(ert-deftest org-air-r97-b1-2-foreign-fundamental-buffer-sweep ()
  "The same sweep from a `fundamental-mode' file buffer.
`org-mode' alone would not discriminate: a guard written as
\"am I in Org?\" would pass there.  The refusal must hold in a buffer
that derives from nothing at all."
  (skip-unless (locate-library "org-air"))
  (org-air-r97--assert-sweep 'fundamental-mode))

;;;; ---------------------------------------------------------------------
;;;; B-1/3 — the allow-list is a DECLARATION, and it is honest
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r97-b1-3-allow-list-is-declared-and-honest ()
  "The works-anywhere allow-list is exhaustive, non-stale and reasoned.

Exhaustive: every command that does NOT refuse out of context appears in
it (otherwise the sweep's positive leg fails, so this is the same law
stated from the other side).  Non-stale: every entry names a live
`commandp' symbol AND still genuinely behaves as claimed — if a future
round makes one of them refuse, the exemption must be deleted, not left
lying around.  Reasoned: every entry carries a non-trivial reason string.

That is the R92 landing-table discipline applied to the command surface:
a command added next round is classified DELIBERATELY."
  (skip-unless (locate-library "org-air"))
  (let* ((results (org-air-r97--sweep 'org-mode))
         (swept (mapcar (lambda (r) (plist-get r :command)) results))
         (declared (mapcar #'car org-air-r97-anywhere-commands))
         (exempt (seq-filter #'org-air-r97--anywhere-p declared)))
    ;; every declared symbol is a real, swept command
    (should (equal '() (seq-remove (lambda (c) (memq c swept)) declared)))
    (should (equal '() (seq-remove #'commandp declared)))
    ;; no duplicates, and a real reason on each
    (should (= (length declared) (length (delete-dups (copy-sequence declared)))))
    (pcase-dolist (`(,cmd . ,reason) org-air-r97-anywhere-commands)
      (should (stringp reason))
      (should (> (length reason) 30))
      (should-not (string-match-p "\\`TODO" reason))
      (ignore cmd))
    ;; the exempt set is exactly the set that does not refuse
    (should (= org-air-r97--anywhere-count (length exempt)))
    (let ((non-refusing
           (mapcar (lambda (r) (plist-get r :command))
                   (seq-remove (lambda (r) (eq (plist-get r :class) 'refuses))
                               results))))
      (should (equal (sort (copy-sequence exempt) #'string<)
                     (sort non-refusing #'string<))))
    ;; NOT-stale in the other direction: `org-air-capture' is listed with
    ;; a reason but deliberately NOT exempt, and R97 D2 must keep it that
    ;; way — it refuses before the title prompt.
    (let ((cap (seq-find (lambda (r) (eq (plist-get r :command) 'org-air-capture))
                         results)))
      (should cap)
      (should (eq (plist-get cap :class) 'refuses))
      (should (= 0 (plist-get cap :prompts))))))

;;;; ---------------------------------------------------------------------
;;;; B-1/4 — the sweep is not vacuous: the named historical offenders
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r97-b1-4-the-named-offenders-are-covered ()
  "The sweep names its own witnesses, so it cannot quietly stop covering them.

The 8 commands that destroyed the user's file, the 4 transient suffixes
that signalled a raw `wrong-type-argument', and the prompt-first verbs
the reviewer listed by name are each asserted present in the sweep, in
the STRICT set (never allow-listed), and refusing cleanly with zero
prompts.  Without this, a rename or a lost `commandp' would silently
shrink the sweep to something that still passes."
  (skip-unless (locate-library "org-air"))
  (let* ((results (org-air-r97--sweep 'org-mode))
         (by-name (lambda (c) (seq-find (lambda (r) (eq (plist-get r :command) c))
                                        results)))
         (destroyers '(org-air-refresh org-air-refresh-all
                       org-air-calendar-today org-air-calendar-next
                       org-air-calendar-prev org-air-filter-clear
                       org-air-filter-toggle-match org-air-scope-clear))
         (raw-errors '(org-air-refile-form-category org-air-refile-form-file
                       org-air-refile-form-tags org-air-refile-form-todo))
         (prompters '(org-air-item-set-deadline org-air-item-schedule
                      org-air-filter org-air-scope org-air-filter-by-tag
                      org-air-project-sort-set org-air-review-filter
                      org-air-revisit-filter org-air-project-filter
                      org-air-refile-form-deadline org-air-refile-form-note
                      org-air-refile-form-schedule)))
    (should (= 8 (length destroyers)))
    (should (= 4 (length raw-errors)))
    (dolist (cmd (append destroyers raw-errors prompters))
      (let ((r (funcall by-name cmd)))
        (should (commandp cmd))
        (should r)
        (should-not (org-air-r97--anywhere-p cmd))
        (should (eq (plist-get r :class) 'refuses))
        (should (eq (plist-get r :signal) 'user-error))
        (should (= 0 (plist-get r :prompts)))
        (should-not (plist-get r :text-changed))
        (should-not (plist-get r :modified))))))

;;;; ---------------------------------------------------------------------
;;;; B-1/6 — the FUNNEL itself, and the ninth command nobody has written
;;;; ---------------------------------------------------------------------

(defun org-air-r97--foreign-buffer-refuses (thunk)
  "Run THUNK in a fresh foreign `org-mode' buffer; return the signal symbol.
Asserts, whatever happens, that the buffer is byte-identical afterwards
and unmodified."
  (let ((dir (make-temp-file "org-air-r97-funnel-" t)))
    (unwind-protect
        (let* ((file (expand-file-name "thesis.org" dir))
               (_ (write-region org-air-sweep-foreign-text nil file nil 'silent))
               (buf (find-file-noselect file))
               (sig nil))
          (unwind-protect
              (with-current-buffer buf
                (set-buffer-modified-p nil)
                (condition-case err
                    (let ((noninteractive nil) (inhibit-message t))
                      (funcall thunk))
                  (error (setq sig (car err))))
                (should (equal org-air-sweep-foreign-text (buffer-string)))
                (should-not (buffer-modified-p))
                sig)
            (when (buffer-live-p buf)
              (with-current-buffer buf (set-buffer-modified-p nil))
              (kill-buffer buf))))
      (delete-directory dir t))))

(ert-deftest org-air-r97-b1-6-the-render-funnel-refuses ()
  "The three buffer-ERASING funnels refuse directly, not only via commands.

R97's whole design bet is that ONE precondition in the funnel closes the
class, rather than eight `derived-mode-p' checks in the eight commands
that happened to be found:

  \"Do not fix D1 by adding a `derived-mode-p' check to each of the 8
   commands.  Patching the 8 known instances leaves the ninth, which will
   be written next round.\"           — REPORT-r96.md, §what NOT to do

So this test asserts the funnel on its own terms — `org-air-view--render',
`org-air-view--render-loading' and `org-air-view--render-current' each
refuse from a foreign buffer with a `user-error', erasing nothing — and
then writes THE NINTH COMMAND: a brand-new command that reaches
`org-air-view--render-current' with no guard of its own, exactly as the
eight destroyers did.  It must be refused by the funnel it did not know
about.  Delete the funnel guard and this test goes red even though every
real command still carries its own."
  (skip-unless (locate-library "org-air"))
  ;; the three erasing funnels, called directly
  (should (eq 'user-error
              (org-air-r97--foreign-buffer-refuses
               (lambda () (org-air-view--render-current)))))
  (should (eq 'user-error
              (org-air-r97--foreign-buffer-refuses
               (lambda () (org-air-view--render-loading)))))
  (should (eq 'user-error
              (org-air-r97--foreign-buffer-refuses
               (lambda () (org-air-view--render nil nil)))))
  ;; THE NINTH COMMAND: written today, guarded by nobody, funnelled anyway.
  (defalias 'org-air-r97--ninth-command
    (lambda ()
      "A command R97 never saw, reaching the funnel with no guard at all."
      (interactive)
      (org-air-view--render-current)))
  (unwind-protect
      (progn
        (should (commandp 'org-air-r97--ninth-command))
        (should (eq 'user-error
                    (org-air-r97--foreign-buffer-refuses
                     (lambda ()
                       (call-interactively 'org-air-r97--ninth-command))))))
    (fmakunbound 'org-air-r97--ninth-command)))

;;;; ---------------------------------------------------------------------
;;;; B-1/5 — `interactive' MODE scoping: the menu stays honest
;;;; ---------------------------------------------------------------------

(defconst org-air-r97-globally-offered-commands
  '(org-air org-air-view org-air-review org-air-revisit org-air-project
    org-air-capture org-air-goto-date org-air-process-inbox
    org-air-scan-report org-air-help org-air-faces-link-nano
    org-air-return org-air-return-to-dashboard org-air-view-day)
  "The commands `M-x' may offer from ANY buffer: entry points and returns.

Everything else in the surface carries `(interactive SPEC MODE…)' so it
vanishes from `M-x' completion outside its own view.  Scoping HIDES; it
does not forbid — a command typed by full name still runs, and then meets
its guard.  The guard is the protection; this is the menu.

Note that four of these fourteen (`org-air-capture', `org-air-return',
`org-air-return-to-dashboard', `org-air-view-day') are globally OFFERED
but still REFUSE out of context: being reachable and being applicable are
different questions, and R97 answers them separately.")

(ert-deftest org-air-r97-b1-5-interactive-forms-are-mode-scoped ()
  "R97 D6: 118 of the 132 commands carry `interactive' MODE scoping.

Before R97 the number was ZERO, so `M-x org-air-<TAB>' offered all 132
from every buffer in the user's Emacs — including the 8 that destroyed
the current buffer and the 4 that threw `wrong-type-argument'.  Asserted
by NAME in both directions (the globally-offered set is a written-down
list, not a count), and then MEASURED through the same predicate `M-x'
itself uses, from a real `org-mode' buffer."
  (skip-unless (locate-library "org-air"))
  (let* ((cmds (org-air-sweep-commands))
         (unscoped (seq-remove #'command-modes cmds))
         (scoped (seq-filter #'command-modes cmds)))
    (should (>= (length cmds) 130))
    ;; the globally-offered set, by name, exactly
    (should (equal (sort (mapcar #'symbol-name
                                 (copy-sequence
                                  org-air-r97-globally-offered-commands))
                         #'string<)
                   (sort (mapcar #'symbol-name unscoped) #'string<)))
    (should (= 118 (length scoped)))
    ;; every declared mode is a real org-air mode, never a foreign one
    (dolist (c scoped)
      (dolist (m (command-modes c))
        (should (string-prefix-p "org-air" (symbol-name m)))))
    ;; and the measured effect, through `M-x''s own predicate
    (with-temp-buffer
      (org-mode)
      (let ((offered (seq-filter
                      (lambda (c)
                        (command-completion-default-include-p
                         c (current-buffer)))
                      cmds)))
        (should (equal (sort (mapcar #'symbol-name
                                     (copy-sequence
                                      org-air-r97-globally-offered-commands))
                             #'string<)
                       (sort (mapcar #'symbol-name offered) #'string<)))))
    ;; … and the converse: inside a board, the board verbs ARE offered.
    (with-temp-buffer
      (org-air-view-mode)
      (should (command-completion-default-include-p
               'org-air-refresh (current-buffer)))
      (should (command-completion-default-include-p
               'org-air-filter (current-buffer)))
      (should-not (command-completion-default-include-p
                   'org-air-project-refresh (current-buffer))))))

;;;; ---------------------------------------------------------------------
;;;; B-2 — the loading skeleton -> swap sequence, pinned once
;;;; ---------------------------------------------------------------------

(defmacro org-air-r97--with-async-corpus (n &rest body)
  "Build an N-file Org corpus, then run BODY with the roots bound.
BODY runs OUTSIDE `org-air-test-with-async-scan' — each test enters the
interactive path where it wants to, so the synchronous control leg can
use the same corpus."
  (declare (indent 1) (debug t))
  `(let ((dir (make-temp-file "org-air-r97-async-" t)))
     (unwind-protect
         (progn
           (dotimes (i ,n)
             (let ((file-name-handler-alist nil)
                   (coding-system-for-write 'utf-8-unix))
               (write-region
                (format "* TODO Async item %d\n  SCHEDULED: <2026-06-16 Tue>\n" i)
                nil (expand-file-name (format "f%d.org" i) dir) nil 'silent)))
           (let ((org-air-files (list dir))
                 (org-air-inbox-file (expand-file-name "inbox.org" dir))
                 (org-air-cache-file (expand-file-name ".cache/board.eld" dir))
                 (org-air-view-buffer-name "*org-air-r97-async*")
                 (org-air-report-skipped-files nil)
                 (create-lockfiles nil)
                 (inhibit-message t))
             ,@body))
       (when (fboundp 'org-air-query-teardown) (org-air-query-teardown))
       (org-air-test-cancel-org-air-timers)
       (let ((kill-buffer-query-functions nil))
         (dolist (b (buffer-list))
           (let ((fn (buffer-file-name b)))
             (when (or (and fn (string-prefix-p dir fn))
                       (equal (buffer-name b) "*org-air-r97-async*"))
               (with-current-buffer b (set-buffer-modified-p nil))
               (kill-buffer b)))))
       (delete-directory dir t))))

(ert-deftest org-air-r97-b2-1-cold-open-paints-skeleton-then-swaps ()
  "INSTRUMENT B-2: the loading skeleton → single swap sequence, pinned.

`noninteractive' is t in 100% of the batch gate, so this sequence — what
the user sees 100% of the time — had never been observed by any test.
With it bound to nil (`org-air-test-with-async-scan') a cold open leaves
the command body having painted ONLY the skeleton:

  * `org-air-view--loading' is t and the buffer says
    \"Loading your board… (scanning 0/6)\" with a live `scanning 0/6…'
    banner segment;
  * ZERO items are in the buffer yet — the scan has not run;
  * the adaptive wall-clock pacer is ARMED and pending in `timer-list'
    (`org-air-view--refresh-chain-live-p'), which is the R56 P2a
    liveness definition, not merely `timerp'.

Then, letting the REAL timer queue run (`sleep-for' runs due timers), the
machine finishes on its own and swaps ONCE: `--loading' clears, the full
item set is in place, and the skeleton text is gone.  No callback is
invoked by hand and no timer is simulated.

Control leg: the identical open under `noninteractive' t is synchronous —
it never paints a skeleton and never arms a timer.  That difference IS
the blind spot this instrument closes."
  (skip-unless (locate-library "org-air"))
  (org-air-r97--with-async-corpus 6
    ;; --- the interactive path: skeleton first, swap later ---------------
    (org-air-test-with-async-scan
      (org-air)
      (let ((buf (get-buffer org-air-view-buffer-name)))
        (should (buffer-live-p buf))
        (with-current-buffer buf
          ;; the intermediate state no test had ever seen
          (should org-air-view--loading)
          (should (null org-air-view--items))
          (should (string-match-p "Loading your board…"
                                  (buffer-substring-no-properties
                                   (point-min) (point-max))))
          (should (string-match-p "scanning 0/6"
                                  (buffer-substring-no-properties
                                   (point-min) (point-max))))
          ;; the pacer is armed AND pending — R56 P2a liveness
          (should (org-air-view--refresh-chain-live-p))
          (should (eq org-air-view--refresh-state 'refreshing))
          ;; ... and now let the real queue run to the single swap
          (should (org-air-test-pump-timers
                   (lambda () (and (not org-air-view--loading)
                                   (not (eq org-air-view--refresh-state
                                            'refreshing))))
                   20))
          (should (= 6 (length org-air-view--items)))
          (should-not (org-air-view--refresh-chain-live-p))
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should-not (string-match-p "Loading your board…" text))
            (should (string-match-p "Async item" text)))))))
  ;; --- the control leg: batch is synchronous, no skeleton, no timer ----
  (org-air-r97--with-async-corpus 6
    (should noninteractive)
    (org-air)
    (let ((buf (get-buffer org-air-view-buffer-name)))
      (should (buffer-live-p buf))
      (with-current-buffer buf
        (should-not org-air-view--loading)
        (should (= 6 (length org-air-view--items)))
        (should-not (org-air-view--refresh-chain-live-p))
        (should-not (string-match-p "Loading your board…"
                                    (buffer-substring-no-properties
                                     (point-min) (point-max))))))))

(ert-deftest org-air-r97-b2-2-async-harness-leaks-no-timer ()
  "`org-air-test-with-async-scan' cancels every org-air timer on the way out.
The harness unlocks 6 idle-timer sites and a self-chaining wall-clock
pacer; if one survived the macro, a later test would be scanning a corpus
that no longer exists.  Asserted for the ABORTED case too: the body exits
mid-scan, with the chain still armed, and nothing is left behind."
  (skip-unless (locate-library "org-air"))
  (org-air-r97--with-async-corpus 6
    (let ((armed nil))
      (org-air-test-with-async-scan
        (org-air)
        (with-current-buffer (get-buffer org-air-view-buffer-name)
          (setq armed (org-air-view--refresh-chain-live-p))))
      (should armed)                    ; it really was mid-flight
      (should (equal '() (seq-filter
                          (lambda (tm)
                            (let ((fn (timer--function tm)))
                              (and (symbolp fn)
                                   (string-prefix-p "org-air" (symbol-name fn)))))
                          (append timer-list timer-idle-list)))))))

(provide 'org-air-round97-test)
;;; org-air-round97-test.el ends here
