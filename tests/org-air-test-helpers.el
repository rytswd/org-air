;;; org-air-test-helpers.el --- shared helpers for org-air test suites -*- lexical-binding: t; -*-

;;; Commentary:
;; Common helpers used by all org-air test suites.  Defines the canonical
;; fixed `now' used for deterministic classification tests, fixture path
;; helpers, and a macro that runs a test body against a scratch copy of
;; the fixtures.
;;
;; The fixture set is anchored on a frozen reference time,
;; `org-air-test-now' = Monday 2026-06-15 10:00 (local).  Relative to it:
;;
;;   upcoming      "Prepare standup notes"          SCHEDULED 2026-06-16
;;                 "Email finance about budget"     SCHEDULED 2026-06-16
;;   overdue       "Fix production outage runbook"  DEADLINE  2026-06-10
;;                 "Book dentist appointment"       DEADLINE  2026-06-12
;;   dateless      "Dust off old archive project"   created [2025-11-02]
;;                 "Learn lute"                     created [2025-09-15]
;;                 (R54-1: inactive-[ts]-only, so NEVER stale now — the
;;                 corpus renders Stale 0; stale tests append their own
;;                 dated scratch items)
;;   no schedule   "Untracked idea with no dates"
;;   high priority "Ship quarterly report" / "Fix production outage
;;                 runbook" / "Prep client presentation"  ([#A])
;;   inbox         everything in inbox.org

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Mark the org-air customisation variables special so that `let'-binding
;; them in `org-air-test-with-fixtures' is dynamic even when these test
;; files are loaded before (or without) org-air itself.
(defvar org-air-files)
(defvar org-air-inbox-file)

(defconst org-air-test-now (encode-time '(0 0 10 15 6 2026 nil -1 nil))
  "Frozen reference time for classification tests: Mon 2026-06-15 10:00.")

(defconst org-air-test-fixture-dir
  (expand-file-name "fixtures"
                    (file-name-directory
                     (or load-file-name buffer-file-name default-directory)))
  "Directory containing the canonical org fixture files.")

(defun org-air-test-fixture (name)
  "Return the absolute path of fixture NAME (e.g. \"projects.org\")."
  (expand-file-name name org-air-test-fixture-dir))

(defun org-air-test-fixture-files ()
  "Return all .org fixture files, sorted."
  (directory-files org-air-test-fixture-dir t "\\.org\\'"))

(defconst org-air-test-fixture-mtime
  (encode-time '(0 0 12 16 6 2026 nil -1 nil))
  "Frozen modification time of every scratch fixture copy: 2026-06-16 12:00.
ONE DAY AFTER `org-air-test-now', which reproduces the configuration the
board goldens were always blessed under -- a scratch copy's mtime is the
wall clock, and the wall clock is after the frozen now -- but pins it so
it no longer depends on the machine's real date.

This matters twice over.  R74 already relied on it: a slotless heading's
`Updated ~file' line is FUTURE-CLAMPED away, which is why the x50
inspector goldens carry no `Updated' line for a slotless row.  R93 made
it load-bearing a second time: the file mtime is the coarse floor of the
Needs-attention clock, so an unpinned mtime would put every historyless
fixture heading at age 0 on this machine and at age N on a machine whose
clock sits before 2026-06-15 -- i.e. the SECTION MEMBERSHIP of the byte
goldens would depend on the date the suite was run.")

(defmacro org-air-test-with-fixtures (&rest body)
  "Run BODY against a scratch copy of the fixtures.
Copies every fixture into a fresh temporary directory and dynamically
binds `org-air-files' to the copies and `org-air-inbox-file' to the
copied inbox.org, so tests may mutate files freely.  Every copy's mtime
is pinned to `org-air-test-fixture-mtime' so nothing rendered from these
fixtures depends on the machine's real clock.  Buffers visiting the
scratch files are killed afterwards and the directory removed."
  (declare (indent 0) (debug t))
  `(let* ((org-air-test--dir (make-temp-file "org-air-fixtures-" t)))
     (unwind-protect
         (progn
           (dolist (f (org-air-test-fixture-files))
             (copy-file f (file-name-as-directory org-air-test--dir))
             (set-file-times
              (expand-file-name (file-name-nondirectory f) org-air-test--dir)
              org-air-test-fixture-mtime))
           (let ((org-air-files
                  (directory-files org-air-test--dir t "\\.org\\'"))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-test--dir)))
             ,@body))
       (let ((kill-buffer-query-functions nil))
         (dolist (buf (buffer-list))
           (let ((fn (buffer-file-name buf)))
             (when (and fn (string-prefix-p org-air-test--dir fn))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-test--dir t))))

;;;; ---------------------------------------------------------------------
;;;; R93 — corpus recency helpers.
;;;;
;;;; R93 replaced the pre-R93 "a dateless board item needs attention"
;;;; default with an AGING rule: a board item surfaces in Needs attention
;;;; only once it has been QUIET for its priority's threshold
;;;; (`org-air-attention-days'; `#A' 0, `#B' 7, `#C' 14, everything else
;;;; 30).  Recency is the newest non-future INACTIVE timestamp in the
;;;; heading's own body, with the source file's mtime as the coarse floor
;;;; for a heading that carries no history at all.
;;;;
;;;; A corpus written by a test is therefore BRAND NEW: every historyless
;;;; heading ages off a just-written file and is (correctly) invisible.
;;;; Suites whose subject is NOT the attention rule -- marks, history,
;;;; scroll stability, landing, echo, bookmarks -- need a corpus that
;;;; looks like a real user's files instead, so they get one here rather
;;;; than weakening their assertions.

(defconst org-air-test-quiet-days 60
  "Corpus age in days used by the R93 recency helpers below.
60 > every default `org-air-attention-days' threshold (max 30), so an
aged heading surfaces in Needs attention on ANY priority, and the number
is stable under a `nil'-keyed retune up to 60.")

(defun org-air-test-quiet-time (&optional days)
  "Return a time DAYS before BOTH the real clock and `org-air-test-now'.
Suites run under a FROZEN clock (`org-air-test-now', Mon 2026-06-15)
while the files they write carry the machine's real timestamps, so an
age measured from the real clock alone would be wrong under the freeze
\(and vice versa).  Anchoring on the earlier of the two makes the corpus
unambiguously old under either clock, and never dated in the future."
  (time-subtract (if (time-less-p (current-time) org-air-test-now)
                     (current-time)
                   org-air-test-now)
                 (days-to-time (or days org-air-test-quiet-days))))

(defun org-air-test-age-file (path &optional days)
  "Backdate PATH's modification time by DAYS (default `org-air-test-quiet-days').
The R93 coarse floor: a heading with no history at all ages off its
file's scan-time mtime, so an old FILE is the cheapest honest way to say
\"this corpus is not brand new\" without touching a byte of its text."
  (set-file-times path (org-air-test-quiet-time days)))

(defun org-air-test-age-directory (dir &optional days)
  "Backdate every .org file under DIR by DAYS (`org-air-test-age-file')."
  (dolist (file (directory-files-recursively dir "\\.org\\'"))
    (org-air-test-age-file file days)))

(defun org-air-test-quiet-stamp (&optional days)
  "Return an INACTIVE Org timestamp DAYS ago (default `org-air-test-quiet-days').
The per-heading twin of `org-air-test-age-file': dropped in a heading's
OWN body it gives that heading its own R93 recency clock, which survives
later writes to the file (an mtime floor does not)."
  (format-time-string "[%Y-%m-%d %a %H:%M]" (org-air-test-quiet-time days)))

(defun org-air-test-stamp-org-text (text &optional days)
  "Return TEXT with a DAYS-old inactive stamp in every heading's OWN body.
The per-heading twin of `org-air-test-age-file', for corpora a test
WRITES to: org-air's own write refreshes the file mtime, which self-heals
the coarse floor back to age 0 and makes the rows vanish mid-test.  A
stamp in the heading's own body is the heading's own clock and survives
any later write -- exactly what `org-log-into-drawer' gives a real user.

The stamp is inserted after the heading's planning line, property drawer
and any existing drawer (`org-end-of-meta-data'), so no PROPERTIES or
LOGBOOK drawer is ever detached from its heading, and always BEFORE the
first child, so no child ever refreshes its parent."
  (with-temp-buffer
    (let ((org-inhibit-startup t)
          (org-mode-hook nil))
      (insert text)
      (org-mode)
      (let ((stamp (org-air-test-quiet-stamp days))
            (starts nil))
        (goto-char (point-min))
        (while (re-search-forward "^\\*+ " nil t)
          ;; Descending order: inserting low never invalidates a high one.
          (push (line-beginning-position) starts))
        (dolist (start starts)
          (goto-char start)
          (org-end-of-meta-data t)
          (unless (bolp) (insert "\n"))
          (insert stamp "\n"))))
    (buffer-string)))

(defun org-air-test-find-item (title items)
  "Return the first item in ITEMS whose title contains TITLE, else nil.
Uses `org-air-item-title'; only call when org-air is available."
  (cl-find-if (lambda (item)
                (string-match-p (regexp-quote title)
                                (or (org-air-item-title item) "")))
              items))

(defconst org-air-test-project-path-token "~air"
  "Deterministic stand-in for the air-project fixture ROOT in the project
inspector `Path' line.  The inspector renders `abbreviate-file-name' of the
doc's ABSOLUTE path, which differs by checkout location (~/… under HOME vs
/tmp/… on CI, where HOME does not abbreviate) — a harness artifact that made
the project-view byte goldens machine-dependent (they only matched on the
blessing machine).  Freezing the root to this token via
`directory-abbrev-alist' makes the Path field — and thus the project-view
goldens — path-INDEPENDENT and deterministic (R17 harness fix).")

(defmacro org-air-test-with-frozen-project-path (root &rest body)
  "Run BODY with the project ROOT abbreviated to a fixed token.
Binds `directory-abbrev-alist' so `abbreviate-file-name' of any doc path
under ROOT renders as `org-air-test-project-path-token'/… regardless of
where the checkout lives, so the project-view inspector `Path' line — and
the byte goldens that pin it — are deterministic (R17 harness fix).  The
entry is anchored at the start of the path and applied BEFORE the HOME→~
step in `abbreviate-file-name', so the result never depends on $HOME."
  (declare (indent 1) (debug t))
  `(let ((directory-abbrev-alist
          (cons (cons (concat "\\`" (regexp-quote (directory-file-name ,root)))
                      org-air-test-project-path-token)
                directory-abbrev-alist)))
     ,@body))

(provide 'org-air-test-helpers)
;;; org-air-test-helpers.el ends here
