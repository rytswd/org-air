;;; org-air-query.el --- Org-QL data layer for org-air -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: org-air contributors
;; Keywords: outlines, calendar
;; Version: 0.1.0
;; URL: https://github.com/rytswd/org-air
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Normalise Org headings from `org-air-files' into `org-air-item' records.
;;
;; R53: the scan is a WORK-BUFFER scan — org-ql stays the only query
;; engine, but org-air now hands it buffers IT manages (one reused work
;; buffer per session, or a user's live buffer) instead of letting it
;; `find-file-noselect' every file.  That kills the measured O(n^2)
;; `buffer-list' cost (271.8s -> 3.41s at 5006 files), retains ZERO source
;; buffers, and lets the per-file body be wrapped in the never-error law:
;; a signalling file (encrypted, unreadable, binary, vanished) contributes
;; 0 items and one skip-log entry — it can NEVER abort a whole scan.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-ql)
(require 'seq)

(defvar org-air-files)
(defvar org-air-inbox-file)
(defvar org-air-exclude-regexps)

(defcustom org-air-todo-keywords
  '(:not-done ("TODO" "NEXT" "STARTED" "READY" "WIP"
               "WAIT" "WAITING" "HOLD" "BLOCKED")
    :done     ("DONE" "COMP" "CANCELLED" "CANCELED" "KILL" "DROP"))
  "TODO keyword vocabulary org-air SUPPLEMENTS the user's with (R57-1).
The :not-done and :done keyword sets merged AFTER the user's own global
`org-todo-keywords' (deduplicated at bare-name level, see
`org-air-query--scan-todo-keywords') so a heading like `* NEXT Foo' is
parsed as a NEXT task even in a file without a `#+TODO:' line.  Only
ever a supplement, never a replacement: the user's global is the base,
and a file's OWN `#+TODO:'/`#+SEQ_TODO:' always wins — this fills the
gap for files declaring none.  R57-2 adds the Air-aligned keywords READY
and WIP (:not-done) plus COMP and DROP (:done), mirroring Air document
states draft/ready/work-in-progress/complete/dropped.  Defaults mirror
the keys of `org-air-todo-keyword-faces' plus the standard done
keywords."
  :type '(plist :key-type symbol :value-type (repeat string))
  :group 'org-air)

(defcustom org-air-note-type-tag-alist
  '(("task" . task) ("note" . knowledge) ("journal" . journal))
  "Tags that OVERRIDE the derived note type (R54-2, optional).
Alist of TAG (string, matched case-insensitively) to TYPE (`task',
`knowledge' — `note' accepted as a synonym — or `journal').  Matched
against an item's tags, which already include inherited `#+filetags', so
a `:note:' file tag (or denote-journal's `journal' keyword) types every
heading with zero mechanics.  Users who tag with `kb'/`evergreen' add one
entry.  Purely an escape hatch: the content-derived model needs no
tagging at all."
  :type '(alist :key-type string :value-type symbol)
  :group 'org-air)

(defcustom org-air-journal-directory-regexp
  "\\`\\(?:journal\\|diary\\|daily\\)\\'"
  "Regexp a PATH COMPONENT must match for a file to type `journal' (R54-2).
One of the journal sub-heuristic's three signals (with a date-shaped file
name and a date-shaped `#+title'); matched case-insensitively against
each directory component of the scanned file's path."
  :type 'regexp
  :group 'org-air)

(defcustom org-air-plain-heading-type 'knowledge
  "Type derived for a plain heading with no task signal (R54-2 step 6).
The USER-RULED default `knowledge' keeps dateless prose off the GTD board
\(everything else is a KNOWLEDGE note).  The legacy value `task'
restores the pre-R54 behaviour where every dateless heading was board
material (Needs attention by default) — for GTD purists whose bare
section headings must stay tasks.  R77: when
`org-air-task-requires-todo' is non-nil the fall-through is `knowledge'
regardless of this value — \"task requires a keyword\" and \"every
keyword-less heading is a task\" are contradictory, and the explicit
knob wins so its contract is total (see its docstring)."
  :type '(choice (const knowledge) (const task))
  :group 'org-air)

(defcustom org-air-task-requires-todo nil
  "When non-nil, only a TODO-keyworded heading types `task' (R77).
The R54-2 task signal narrows from \"TODO keyword OR scheduled/deadline\"
to the keyword alone: a scheduled/deadline heading WITHOUT a keyword — a
routine like \"* Water plants  SCHEDULED: <… ++2w>\" — types through the
rest of the R54 chain (journal heuristic, else knowledge) instead of
camping in the board's task sections.  It keeps its day-view row and
calendar mark (those surfaces read planning slots, not types) and stays
reachable through the note surfaces (Notes row / Revisit) via the F7
file vote.  The R54-2 overrides (`ORG_AIR_TYPE', `#+type:', the tag
alist) still outrank the knob, so any single routine can be forced back
onto the board.  \"Not-done\" is by composition: a DONE-keyword heading
still types `task' (a done task is a task) and is buried by
`org-air-classify--board-active-p' — never re-typed into Revisit
knowledge.  The knob shapes scan-time `ntype'/file-meta, so it is an
`org-air-view--cache-key' element: a flip takes the documented cold
re-derive, exactly like a vocabulary change.  Default nil — the R54 D1
USER-RULED signal, byte-identical behaviour."
  :type 'boolean
  :group 'org-air)

(defcustom org-air-skip-container-headings t
  "When non-nil, pure CONTAINER headings never render as items (R59).
A container is a heading that HAS child headings and carries NO
actionable signal of its OWN: no TODO keyword (the R57 merged
vocabulary decides what counts as one) and no own-body
scheduled/deadline/active timestamp (the R54 date model, scoped to the
heading's own text above its first child).  Such a heading is structure
— its children represent the content — and is skipped on the board
\(including the Inbox bucket), in the day view and in the R54 F7
file-type vote.  Set to nil to restore the pre-R59 behaviour where a
grouping heading in the inbox rendered as its own row."
  :type 'boolean
  :group 'org-air)

(defcustom org-air-max-file-size (* 4 1024 1024)
  "Largest file (bytes) the background scan will read; nil = no limit (R53).
A file over the limit is skipped with a `too-large' entry in the scan
report (`org-air-scan-report') instead of stalling a slice — the generic
monster-file valve of the never-hang contract."
  :type '(choice (const :tag "No limit" nil) integer)
  :group 'org-air)

(defcustom org-air-log-cap 5000
  "Most CLOCK intervals / LOGBOOK stamps retained per heading (R61-1).
The R61 harvest keeps at most this many entries in EACH of the `clocks'
and `logs' item slots (NEWEST kept); hitting either cap sets the item's
`rtrunc' flag, rendered as an inline \"⚠ history truncated\" marker on
the review surface — truncation is never silent.  A cap, deliberately
NOT a lookback window: a window slides with the wall clock, so an
unchanged file's cached fields would go stale with no mtime change (it
breaks the mtime-cache law) and old periods would read as silent zeros.
The default ≈ 13 years of daily clocking on a SINGLE heading.  The cap
shapes scanned-and-persisted data, so it is the SIXTH element of
`org-air-view--cache-key' (R61-2): a change invalidates the cache
exactly like a vocabulary change."
  :type 'integer
  :group 'org-air)

(defun org-air-query--todo-keyword-name (kw)
  "Return KW's bare keyword name, exactly as Org itself splits it.
Org's `org-set-regexps-and-options' splitter: the name is everything
before an optional trailing \"(...)\" fast-access/logging spec —
\"DONE(d!)\" => \"DONE\", \"DROPPED(x@)\" => \"DROPPED\", \"CLOSED\" =>
\"CLOSED\"."
  (if (string-match "^\\(.*?\\)\\(?:(\\([^!@/]\\)?.*?)\\)?$" kw)
      (match-string 1 kw)
    kw))

(defun org-air-query--scan-todo-keywords ()
  "The USER's global `org-todo-keywords' + org-air's supplement (R57-1).
BASE: `default-value' of `org-todo-keywords', kept VERBATIM — same
interpretation symbols, same keyword spellings (fast-access keys and
`!'/`@' logging specs intact), same order and `|' placement; a legacy
flat string list normalises to one sequence under
`org-todo-interpretation'.  SUPPLEMENT: ONE appended (sequence ...)
holding only the `org-air-todo-keywords' entries NOT already declared
anywhere in the base at bare-name level
\(`org-air-query--todo-keyword-name', case-sensitive `member' — exactly
as Org treats keywords), with an ALWAYS-explicit \"|\" so the done set
can never silently corrupt when the :done extras dedup away.  When both
extra sets are empty after dedup the user's value returns unchanged.
Let-bound around the work-buffer scan: this is the DEFAULT binding
`org-set-regexps-and-options' consults when a file declares no `#+TODO:'
of its own — a file's own declaration still wins, byte-identically to
before.  Never signals: a nil or malformed global degrades to Org's own
default base."
  (let* ((user (default-value 'org-todo-keywords))
         ;; Org's backward-compat rule: a legacy flat string list is one
         ;; sequence under `org-todo-interpretation'.
         (base (cond ((and (consp user) (stringp (car user)))
                      (list (cons org-todo-interpretation user)))
                     ((and (consp user) (cl-every #'consp user)) user)
                     (t nil)))
         ;; Org's own default — the never-signal floor for a nil or
         ;; malformed global.
         (base (or base '((sequence "TODO" "DONE"))))
         (declared (cl-loop for seq in base
                            append (cl-loop for kw in (cdr seq)
                                            unless (equal kw "|")
                                            collect (org-air-query--todo-keyword-name kw))))
         (extra (lambda (kws)
                  (cl-remove-if (lambda (kw)
                                  (member (org-air-query--todo-keyword-name kw)
                                          declared))
                                kws)))
         (extra-not-done (funcall extra (plist-get org-air-todo-keywords :not-done)))
         (extra-done (funcall extra (plist-get org-air-todo-keywords :done)))
         ;; Cross-plist dedup: a keyword in both halves supplements once,
         ;; on the :not-done side.
         (extra-done (cl-set-difference extra-done extra-not-done
                                        :test #'equal)))
    (if (or extra-not-done extra-done)
        (append base
                (list (append '(sequence) extra-not-done '("|") extra-done)))
      base)))

(defun org-air-query-merged-done-keywords ()
  "Return the bare DONE keyword names of the merged scan vocabulary (R57-1).
Derives the done set from `org-air-query--scan-todo-keywords' exactly as
Org does per sequence — the keywords after the \"|\" separator; when a
sequence declares no separator, its LAST keyword — at bare-name level
\(`org-air-query--todo-keyword-name').  Pure list data over the merged
vocabulary: the final fallback done set for items built OUTSIDE the scan
\(see `org-air-classify--done-keywords'), replacing the pre-R57
hard-wired (\"DONE\")."
  (cl-loop for seq in (org-air-query--scan-todo-keywords)
           append (let* ((kws (cdr seq))
                         (done (or (cdr (member "|" kws)) (last kws))))
                    (cl-loop for kw in done
                             unless (equal kw "|")
                             collect (org-air-query--todo-keyword-name kw)))))

(cl-defstruct (org-air-item
               (:constructor org-air-item-create)
               (:copier nil))
  "A normalised Org heading (or R53 note file) for org-air views.
R53 P2 (cache v2): `kind', `donep', `activity' and `body-deadline' are
SCAN-TIME slots — everything classify/render needs lives in the struct,
so painting a cache-hydrated board never opens a file.
R93 (cache v7): `updated' joins them as the RECENCY fact backing the
Needs-attention bucket."
  title tags file marker todo priority scheduled deadline group closed
  ;; R53 scan-time slots (data-pure render):
  kind          ; 'heading | 'file (P3 headingless note file-item)
  donep         ; non-nil when todo ∈ the file's own `org-done-keywords'
  activity      ; epoch float: closed‖scheduled‖deadline‖first subtree ts‖mtime
  subtree-ts    ; epoch float of the first timestamp in the subtree BODY,
                ; or nil (R53fix B1: the day view's Logged/created key —
                ; distinct from `activity', whose mtime fallback must
                ; never fill that group)
  body-deadline ; epoch float of the first subtree DEADLINE: when the
                ; heading itself has none (the calendar's origin check)
  ;; R54 scan-time slots (cache v4):
  active-ts     ; epoch float of the first ACTIVE <ts> in the subtree
                ; (`org-ts-regexp': planning lines in, inactive [..] out)
                ; — the R54-1 stale-eligibility signal, distinct from
                ; `subtree-ts' (regexp-both, the day view's key)
  ntype         ; 'task | 'journal | 'knowledge — the R54-2 content-
                ; derived note type; nil on items built outside the scan
                ; (treated as task by the classify routing)
  ;; R59 scan-time slots (cache v5):
  childp        ; t when the subtree contains a child heading — half of
                ; the container signal (`org-air-query-container-item-p');
                ; nil on items built outside the scan (never containers)
  own-active-ts ; epoch float of the first ACTIVE <ts> in the heading's
                ; OWN body (the region above its first child) — the
                ; own-scoped twin of the deliberately SUBTREE-wide
                ; `active-ts' (R54 stale eligibility): a child's date
                ; belongs to the child's row and must not make the
                ; parent test "dated".  For a leaf, ≡ `active-ts'.
  ;; R61 scan-time slots (cache v6) — the review harvest, bounded to the
  ;; heading's OWN body and capped by `org-air-log-cap':
  clocks        ; closed CLOCK intervals of the heading's own body,
                ; newest-first list of (START . END) INTEGER epoch pairs
                ; (END > START only; a running or malformed CLOCK line is
                ; dropped at scan time, never cached)
  logs          ; LOGBOOK stamps, newest-first list of (EPOCH . KIND):
                ; KIND `done'/`todo' (a state change classified against
                ; the buffer's live `org-done-keywords' at scan time —
                ; the file's own vocabulary under the R57-1 merged
                ; default) or nil (a plain "- Note taken on" stamp —
                ; an activity signal only, never state inference)
  created       ; INTEGER epoch of the `:CREATED:' property, or nil
  rtrunc        ; t when either list above hit `org-air-log-cap' —
                ; truncation is never silent (the review "⚠" marker)
  ;; R93 scan-time slot (cache v7) — the RECENCY fact:
  updated)      ; INTEGER epoch of the NEWEST INACTIVE [timestamp] in the
                ; heading's OWN body, or nil when it carries no history
                ; at all.  One bounded `org-ts-regexp-inactive' pass over
                ; the region the R61 harvest already walks, so it
                ; subsumes LOGBOOK state changes and notes, CLOCK-out
                ; ends, `CLOSED:' and `:CREATED:' as well as free-form
                ; body stamps — every shape Org writes when something
                ; HAPPENED.  Active <timestamps> are deliberately EXCLUDED
                ; (a plan is not an update).  Stamps dated after the scan
                ; day are ignored (a note ABOUT the future is not an
                ; update).  The `attention' bucket's clock (R93); nil
                ; falls back to the file mtime in classify, never here.

(defun org-air-query-container-item-p (item)
  "Non-nil when ITEM is a pure CONTAINER heading (R59).
Slot-only and knob-gated: `childp' set by the scan, no TODO keyword, no
own scheduled/deadline (the heading's planning line), no own-body active
timestamp (`own-active-ts' — deliberately NOT the subtree-wide
`active-ts': a child's date belongs to the child's row), and not
explicitly overridden to `task' (an `ORG_AIR_TYPE'/`#+type:'/tag
override wins, same philosophy as every R54 override).  Items built
outside the scan have nil slots and are never containers — the
conservative default: when in doubt, render."
  (and org-air-skip-container-headings
       (eq (org-air-item-kind item) 'heading)
       (org-air-item-childp item)
       (null (org-air-item-todo item))
       (null (org-air-item-scheduled item))
       (null (org-air-item-deadline item))
       (null (org-air-item-own-active-ts item))
       (not (eq (org-air-item-ntype item) 'task))))

(defun org-air-query--org-file-p (file)
  "Return non-nil when FILE is an Org file."
  (and (stringp file)
       (file-regular-p file)
       (string-match-p "\\.org\\(?:\\.gpg\\)?\\'" file)))

(defvar org-air-query--exclude-warned nil
  "Invalid `org-air-exclude-regexps' entries already warned about (R60-2).
Session-scoped: each distinct broken regexp (e.g. \"[\", a typo
mid-edit) is reported through ONE `message' — never a signal, never
echo-area spam — then silently dropped from the compiled set, so a
broken exclude can never kill the board (R53 never-error).")

(defun org-air-query--excluded-p (path regexps)
  "Return non-nil when any of REGEXPS matches PATH (R60-2).
Matching is case-SENSITIVE (`case-fold-search' bound nil): path
matching must not depend on the ambient fold, or the same config would
behave differently in batch vs a user session.  PATH arrives pre-shaped
by the caller — files as plain absolute paths, directories in
directory-name form (trailing slash) — so one regexp like \"/archive/\"
works at both levels.  Pure string work, zero I/O."
  (let ((case-fold-search nil))
    (seq-some (lambda (re) (string-match-p re path)) regexps)))

(defun org-air-query--exclude-context ()
  "Compile `org-air-exclude-regexps' ONCE per enumeration (R60-2).
Returns nil when the knob is nil or every entry is invalid — every
call site then takes the pre-R60 code path byte-for-byte (in
particular `directory-files-recursively' keeps its literal nil
PREDICATE).  Otherwise a list (REGEXPS INBOX INBOX-PATHS): the
validated regexps; the truename-normalised absolute
`org-air-inbox-file' (the `org-air-query--inbox-file-p' normalisation;
nil when no inbox is configured) backing the file-level inbox guard;
and the inbox path's spellings (expanded + truename) backing the
directory ancestor guard.  Never-error (R53): an invalid regexp is
dropped from the compiled set and warned about once per session via
`org-air-query--exclude-warned'."
  (let ((regexps nil))
    (dolist (re (and (boundp 'org-air-exclude-regexps)
                     org-air-exclude-regexps))
      (if (and (stringp re)
               (ignore-errors (or (string-match-p re "") t)))
          (push re regexps)
        (unless (member re org-air-query--exclude-warned)
          (push re org-air-query--exclude-warned)
          (message "org-air: dropping invalid exclude regexp %S" re))))
    (when regexps
      (let* ((raw (and (boundp 'org-air-inbox-file)
                       org-air-inbox-file
                       (expand-file-name org-air-inbox-file)))
             (inbox (and raw (or (ignore-errors (file-truename raw)) raw))))
        (list (nreverse regexps)
              inbox
              (and raw (delete-dups (list raw inbox))))))))

(defun org-air-query--exclude-file-p (file exclude)
  "Return non-nil when FILE is dropped by the compiled EXCLUDE context (R60-2).
FILE is absolute and PRE-truename — exclusion is BY NAME: it matches
what the user sees and configured; the R53 symlink-only truename dedupe
stays downstream and untouched.  The inbox guard wins over everything:
a FILE that truename-equals `org-air-inbox-file' is never dropped,
however the regexps read, so the capture target stays reachable (a
symlinked inbox is protected too; the truename is paid only for a file
that actually MATCHED an exclude)."
  (and (org-air-query--excluded-p file (nth 0 exclude))
       (not (and (nth 1 exclude)
                 (equal (or (ignore-errors (file-truename file)) file)
                        (nth 1 exclude))))))

(defun org-air-query--exclude-dir-p (dir exclude)
  "Return non-nil when directory DIR must be PRUNED under EXCLUDE (R60-2).
DIR is the absolute path `directory-files-recursively' hands its
PREDICATE (no trailing slash); it is matched in DIRECTORY-NAME form
\(`file-name-as-directory') so \"/archive/\" and \"\\\\.git/\" match the
way users write them.  Pruning refuses descent entirely — an excluded
tree is never even enumerated (the R53 scale win: a 5000-file archive/
costs zero stats, zero sorts, zero list allocation).  The ancestor
guard: a directory on the spine above `org-air-inbox-file' is never
pruned (one `string-prefix-p' per inbox spelling, no I/O), so an inbox
inside an excluded tree still gets enumerated while the file-level
filter drops every OTHER file in that tree — correctness beats the
pruning win in that one pathological layout."
  (let ((dirname (file-name-as-directory dir)))
    (and (org-air-query--excluded-p dirname (nth 0 exclude))
         (not (seq-some (lambda (inbox) (string-prefix-p dirname inbox))
                        (nth 2 exclude))))))

(defun org-air-query--expand-source (source &optional exclude)
  "Expand SOURCE, which may be a file or directory, to Org files.
EXCLUDE is the compiled `org-air-query--exclude-context', nil for none
— and with nil the body is the pre-R60 path byte-for-byte (PREDICATE
stays literal nil).  With a context (R60-2): a directory source PRUNES
matching subdirectories via the PREDICATE (refused descent, never
post-filtered — an excluded archive/ is never walked) and post-filters
the returned FILES (the belt for file-level regexps pruning cannot
see); a directory source that ITSELF matches is silenced whole (the
traversal predicate is never consulted for the root); a file source
listed explicitly in `org-air-files' that matches is DROPPED — exclude
wins over an explicit listing.  The inbox guard (in the helpers) wins
over everything."
  (let ((path (expand-file-name source)))
    (cond
     ((file-directory-p path)
      (cond
       ((null exclude)
        (directory-files-recursively path "\\.org\\(?:\\.gpg\\)?\\'" nil))
       ((org-air-query--exclude-dir-p path exclude) nil)
       (t
        (seq-remove
         (lambda (file) (org-air-query--exclude-file-p file exclude))
         (directory-files-recursively
          path "\\.org\\(?:\\.gpg\\)?\\'" nil
          (lambda (dir)
            (not (org-air-query--exclude-dir-p dir exclude))))))))
     ((org-air-query--org-file-p path)
      (unless (and exclude (org-air-query--exclude-file-p path exclude))
        (list path)))
     (t nil))))

(defun org-air-query-files ()
  "Return all existing Org files configured in `org-air-files'.
R53 P1d: order-preserving hash-table dedupe; `file-truename' is paid ONLY
for actual symlinks (`file-symlink-p' pre-check) so a 5000-file tree
enumerates in milliseconds while a symlinked duplicate still dedupes to
its target (measured 0.647s -> 0.044s at 5006 files).
R60-2: the `org-air-exclude-regexps' context is compiled ONCE here and
passed down explicitly, so every consumer (board scan, refile targets,
revisit queue, denote index) sees ONE coherent excluded set.  Exclusion
is BY NAME, applied pre-truename at enumeration — the symlink-only
dedupe below stays untouched."
  (let ((seen (make-hash-table :test #'equal))
        (out nil)
        (exclude (org-air-query--exclude-context)))
    (dolist (file (seq-mapcat
                   (lambda (source)
                     (org-air-query--expand-source source exclude))
                   org-air-files))
      (let ((path (if (file-symlink-p file)
                      (or (ignore-errors (file-truename file)) file)
                    file)))
        (unless (gethash path seen)
          (puthash path t seen)
          (push path out))))
    (nreverse out)))

(defun org-air-query--timestamp (property)
  "Return Org timestamp object for PROPERTY at point, or nil."
  (when-let* ((value (org-entry-get (point) property)))
    (ignore-errors (org-timestamp-from-string value))))

(defun org-air-query--time-float (timestamp)
  "Return TIMESTAMP (an Org timestamp object) as an epoch float, or nil."
  (when timestamp
    (ignore-errors (float-time (org-timestamp-to-time timestamp)))))

(defun org-air-query--group (file)
  "Return display group for heading in FILE."
  (or (org-entry-get (point) "CATEGORY")
      (file-name-base file)))

(defvar org-air-query--scan-mtime nil
  "The scanned file's modification time, bound per file by the scan (R53).
Lets `org-air-query--item-at-point' seed the `activity' slot's mtime
fallback from the stat the scan already paid, instead of a per-item
re-stat.")

(defun org-air-query--single-tag-value-p (value)
  "Return non-nil when VALUE exactly matches Org's native tag grammar."
  (and (stringp value)
       (string-match-p (concat "\\`\\(?:" org-tag-re "\\)\\'") value)))

(defun org-air-query--validate-single-tag-value (value)
  "Return VALUE, or refuse it when Org would not recognize it as one tag."
  (unless (org-air-query--single-tag-value-p value)
    (user-error "Tag must match Org's native tag grammar"))
  value)

(defun org-air-query--heading-projection (&optional position)
  "Return Org's native heading projection at POSITION (default point).
The plist contains `:title', `:local-tags', and `:effective-tags'.  Title,
TODO/priority/COMMENT handling, FILETAGS and selective inheritance all come
from Org itself.  Deferred element-cache data is disabled because source
preflight and post-save finalization require synchronous buffer truth."
  (save-excursion
    (when position (goto-char position))
    (org-back-to-heading t)
    (let ((org-element-use-cache nil))
      (list :title (substring-no-properties (org-get-heading t t t t))
            :local-tags
            (mapcar #'substring-no-properties (org-get-tags nil t))
            :effective-tags
            (mapcar #'substring-no-properties (org-get-tags nil nil))))))

(defun org-air-query--heading-line-parts (&optional line)
  "Return Org-native title and local tags from LINE or the heading at point."
  (if (null line)
      (let ((projection (org-air-query--heading-projection)))
        (list :title (plist-get projection :title)
              :local-tags (plist-get projection :local-tags)))
    (with-temp-buffer
      (insert line "\n")
      (org-mode)
      (goto-char (point-min))
      (org-air-query--heading-line-parts))))

(defun org-air-query--heading-line-set-local-tags (line tags)
  "Return heading LINE with Org-native local TAGS replacing its suffix.
All values are validated before a temporary native `org-set-tags' writer can
move point or change bytes.  Callers must never pass inherited tags."
  (mapc #'org-air-query--validate-single-tag-value tags)
  (with-temp-buffer
    (insert line "\n")
    (org-mode)
    (goto-char (point-min))
    (let ((org-element-use-cache nil)
          (org-tags-column 0))
      (org-set-tags tags))
    (buffer-substring-no-properties (line-beginning-position)
                                    (line-end-position))))

;;;; ---------------------------------------------------------------------
;;;; R61-1 — the review harvest: same pass, own body, never-error, capped.
;;;; ---------------------------------------------------------------------

(defconst org-air-query--clock-line-regexp
  "^[ \t]*CLOCK:[ \t]*\\[\\([^]\n]+\\)\\]\\(?:--\\[\\([^]\n]+\\)\\][ \t]*=>\\)?"
  "Anchored CLOCK-line regexp of the R61-1 harvest.
Group 1 is the start stamp; group 2 (nil on a RUNNING clock) the end
stamp.  The line shape is the contract, exactly like every other probe
in `org-air-query--item-at-point' — no drawer parsing.")

(defconst org-air-query--log-line-regexp
  (concat "^[ \t]*- \\(?:State[ \t]+\"\\([^\"\n]+\\)\"\\|Note taken on\\)"
          ".*\\[\\([^]\n]+\\)\\]")
  "Anchored LOGBOOK stamp regexp of the R61-1 harvest.
Matches the DEFAULT `org-log-note-headings' state shape
\(`- State \"KW\" … [TS]', group 1 = the quoted keyword) and the plain
`- Note taken on [TS]' stamp (group 1 nil); group 2 is the timestamp.
Known, accepted difference (R53 style): a user-customised state template
that no longer matches this shape harvests nothing from those lines —
Completed then rides `CLOSED:' stamps (which `org-log-done' writes
regardless), Time is unaffected, and no error or guess is produced.")

(defun org-air-query--stamp-epoch (ts)
  "Parse Org timestamp string TS to an INTEGER epoch second, or nil.
Never signals: an unparseable stamp folds to nil (the R61-1 skip rule).
A date-only stamp reads as local midnight; epochs are fixnums through
year 2100+ (measured), so the retained shapes carry no floats."
  (ignore-errors
    (let ((d (org-parse-time-string ts)))
      (floor (float-time (encode-time (list (or (nth 0 d) 0)
                                            (or (nth 1 d) 0)
                                            (or (nth 2 d) 0)
                                            (nth 3 d) (nth 4 d) (nth 5 d)
                                            nil -1 nil)))))))

(defvar org-air-query--scan-today nil
  "Today's `YYYY-MM-DD' string, bound per scan (R93).
The future-stamp guard of the `updated' probe reads it instead of
calling `format-time-string' once per heading; nil (outside a scan)
makes the probe compute it itself.")

(defconst org-air-query--planning-keyword-regexp
  "\\(SCHEDULED\\|DEADLINE\\|CLOSED\\):"
  "The three Org planning keywords, for the R94 plan-stamp exclusion.
Only the NEAREST one to the left of a stamp decides whose value that
stamp is, so a mixed planning line (`CLOSED: [x] DEADLINE: [y]') is
resolved keyword by keyword rather than line by line.")

(defun org-air-query--plan-stamp-p (pos)
  "Non-nil when the stamp starting at POS is a SCHEDULED:/DEADLINE: value (R94).
Org accepts a PLANNING date written as an INACTIVE stamp
\(`DEADLINE: [2026-06-14 Sun]'), which `org-ts-regexp-inactive' matches
just like a log stamp.  R93 claimed the plan/update exclusion was
mechanical — \"a SCHEDULED, a DEADLINE or a bare plan date can never move
this clock\" — but that was only true of the `<…>' spelling: the bracket
spelling read as BOTH a deadline (the row went to Overdue) and an update
\(age 1).  One stamp, two meanings.

The nearest planning keyword to the LEFT of POS on the same line decides:
`SCHEDULED:'/`DEADLINE:' own the stamp (a plan — skip it), `CLOSED:' owns
it (a completion — keep it, it is the one planning stamp that records
something that HAPPENED), and no keyword at all means an ordinary body or
LOGBOOK stamp (keep it).  Match data is preserved for the caller's walk."
  (save-excursion
    (save-match-data
      (goto-char pos)
      (let ((bol (line-beginning-position)))
        (and (re-search-backward org-air-query--planning-keyword-regexp bol t)
             (member (match-string-no-properties 1) '("SCHEDULED" "DEADLINE"))
             t)))))

(defun org-air-query--quoted-stamp-p (beg end)
  "Non-nil when the stamp spanning [BEG, END) is wrapped in double quotes (R94).
Org's default `org-log-note-headings' quote the OLD value of whatever
changed (`%S'):

  - Rescheduled from \"[2026-06-14 Sun]\" on [2026-06-01 Mon 08:00]
  - New deadline from \"[2026-06-13 Sat]\" on [2026-05-01 Fri 08:00]

The quoted date is the PLAN that was abandoned, not the moment the note
was written, and it is NEWER than the log's own stamp whenever a task is
moved earlier — so the naive newest-wins walk read the reschedule above
as age 1 instead of 14.  A quoted stamp is therefore never this clock's
witness; the unquoted `%t' stamp on the same line is.  (`%s'/`%S' are
quoted in every default heading, so the rule is format-independent: it
asks the punctuation, not the wording.)"
  (and (> beg (point-min))
       (eq (char-before beg) ?\")
       (eq (char-after end) ?\")))

(defun org-air-query--newest-inactive-stamp (start bound)
  "Return the newest non-future inactive stamp epoch in [START, BOUND) (R93).
ONE bounded `org-ts-regexp-inactive' pass over the region the R61
harvest already walks — no second pass over the buffer, no file access.
Every `[timestamp]' Org writes when something HAPPENED lives in that
region: LOGBOOK `- State \"X\" … [TS]' / `- Note taken on [TS]' lines,
`CLOCK: […]--[…]' ends, the `CLOSED:' planning stamp and the
`:CREATED:' property, plus any stamp the user typed in the body.  Active
`<timestamps>' are NOT matched by the regexp — a plan is not an update
\(the R93 recency ruling).

R94 closes the two holes in that ruling, both measured by the R93 review
and both in the \"looks fresher than it is\" direction:

  `org-air-query--plan-stamp-p'    an INACTIVE `SCHEDULED:'/`DEADLINE:'
                                   value is a plan, not an update
                                   \(`CLOSED:' still counts);
  `org-air-query--quoted-stamp-p'  the QUOTED old date inside a
                                   `Rescheduled from \"[…]\" on […]' log
                                   line is the abandoned plan, not the
                                   log's own moment.

Both are constant-cost per match (one bounded backward search on the
current line, two `char-after'/`char-before' reads) and neither adds a
pass or a file read.  The exclusion is now genuinely mechanical, which
is what the R93 design and README already claimed.

Stamps are compared as STRINGS, which is exact for Org's own
`YYYY-MM-DD Dow HH:MM' shape (the date sorts first and a given date
always carries the same day name), so the whole region costs one regexp
walk plus ONE `org-parse-time-string' at the end instead of one per
stamp.  A stamp dated AFTER today is skipped: a note about the future is
not an update, and letting it win would silence the heading forever.
Returns an INTEGER epoch, or nil when the region holds no usable stamp."
  (let ((today (or org-air-query--scan-today (format-time-string "%Y-%m-%d")))
        (newest nil))
    (save-excursion
      (goto-char start)
      (while (re-search-forward org-ts-regexp-inactive bound t)
        ;; Group 0 deliberately: whether `org-ts-regexp-inactive' group 1
        ;; includes the brackets has differed across Org versions, and
        ;; group 0 is the whole "[YYYY-MM-DD …]" in every one of them.
        ;; The bounds are copied out BEFORE either R94 guard runs — both
        ;; search, and `save-match-data' protects the walk, but the walk's
        ;; own strings must be taken first (the R61 clobber rule).
        (let ((raw (match-string-no-properties 0))
              (mbeg (match-beginning 0))
              (mend (match-end 0)))
          (when (and (> (length raw) 11)
                     (not (org-air-query--quoted-stamp-p mbeg mend))
                     (not (org-air-query--plan-stamp-p mbeg)))
            (let ((inner (substring raw 1 -1)))
              ;; A stamp dated after today is a note ABOUT the future.
              (unless (string< today (substring inner 0 10))
                (when (or (null newest) (string< newest inner))
                  (setq newest inner))))))))
    (and newest (org-air-query--stamp-epoch newest))))

(defun org-air-query--harvest-at-point (child-pos end)
  "Collect the R61-1 review facts for the heading at point.
Scans the heading's OWN body — the region above CHILD-POS (its first
child), bounded by the subtree END — with the two anchored line regexps,
so a child's clocks are never credited to the parent (rollups would
double-count).  Point sits on the heading in the positioned scan buffer;
the buffer's live `org-done-keywords' (the file's own vocabulary under
the R57-1 merged default) classifies state-change targets at scan time.
Returns (CLOCKS LOGS CREATED RTRUNC UPDATED) — the four `org-air-item'
review slots plus the R93 recency slot, integer epochs and interned
symbols only, each list newest-first and truncated to `org-air-log-cap'
\(NEWEST kept; a cap hit sets RTRUNC).  UPDATED is
`org-air-query--newest-inactive-stamp' over the same region — UNcapped
by construction (it keeps one number, not a list), so a truncated
LOGBOOK never truncates the recency fact.
Per matched line the match strings are copied out BEFORE parsing:
`org-parse-time-string' CLOBBERS the ambient match data (verified — the
naive loop died with `args-out-of-range' after the first parse).
NEVER-ERROR (the per-heading inner net): any signal degrades THIS
heading to nil review slots — the item is still built, the file still
scans, the R53 P1b outer net is not consumed, nothing is echoed."
  (condition-case nil
      (let ((bound (if child-pos (min child-pos end) end))
            (cap (max 1 org-air-log-cap))
            (clocks nil) (logs nil) (rtrunc nil) (updated nil))
        (save-excursion
          (forward-line 1)
          (let ((body-start (point)))
            (when (< body-start bound)
              ;; R93: the RECENCY probe — one more bounded walk over the
              ;; SAME region, reusing BODY-START/BOUND (zero extra
              ;; structural work) and riding the same never-error net.
              (setq updated (org-air-query--newest-inactive-stamp
                             body-start bound))
              (save-excursion
                (goto-char body-start)
                (while (re-search-forward org-air-query--clock-line-regexp
                                          bound t)
                  ;; Copy the match strings out FIRST (the match-data
                  ;; clobber documented above).
                  (let ((s1 (match-string-no-properties 1))
                        (s2 (match-string-no-properties 2)))
                    ;; A running clock (no second stamp), an unparseable
                    ;; stamp or an end-before-start pair drops THIS line,
                    ;; nothing else.
                    (when s2
                      (let ((t1 (org-air-query--stamp-epoch s1))
                            (t2 (org-air-query--stamp-epoch s2)))
                        (when (and t1 t2 (> t2 t1))
                          (push (cons t1 t2) clocks)))))))
              (save-excursion
                (goto-char body-start)
                (while (re-search-forward org-air-query--log-line-regexp
                                          bound t)
                  (let ((kw (match-string-no-properties 1))
                        (ts (match-string-no-properties 2)))
                    (let ((epoch (org-air-query--stamp-epoch ts)))
                      (when epoch
                        (push (cons epoch
                                    (cond ((null kw) nil)
                                          ((member kw org-done-keywords)
                                           'done)
                                          (t 'todo)))
                              logs)))))))))
        (setq clocks (sort clocks (lambda (a b) (> (car a) (car b)))))
        (setq logs (sort logs (lambda (a b) (> (car a) (car b)))))
        (when (> (length clocks) cap)
          (setq rtrunc t
                clocks (seq-take clocks cap)))
        (when (> (length logs) cap)
          (setq rtrunc t
                logs (seq-take logs cap)))
        (list clocks logs
              ;; `:CREATED:' — one properties-drawer read at the point org
              ;; is already positioned; nil when absent or unparseable.
              ;; The Started fallback (earliest LOGBOOK stamp) is derived
              ;; at RENDER time from the retained stamps — never baked.
              (when-let* ((value (org-entry-get (point) "CREATED")))
                (org-air-query--stamp-epoch value))
              rtrunc
              updated))
    (error (list nil nil nil nil nil))))

;;;; ---------------------------------------------------------------------
;;;; R54-2 — the content-derived note-type model + denote READ compat.
;;;; org-air reads denote's ON-DISK conventions only (ID file names,
;;;; `#+title'/`#+filetags' front matter, `denote:' links); it NEVER calls
;;;; a denote-* function and works with denote absent.
;;;; ---------------------------------------------------------------------

(defconst org-air-query--denote-id-regexp
  "\\`\\([0-9]\\{8\\}T[0-9]\\{6\\}\\)"
  "Anchored regexp capturing the denote identifier of a file NAME (R54-2).
The view's F1 regexp promoted into the query layer, loosened to not
require the `--' separator so a date-only journal name
\(`20260715T000000.org') matches too.")

(defun org-air-query--denote-file-id (file)
  "Return FILE's denote identifier (\"YYYYMMDDTHHMMSS\"), or nil (R54-2)."
  (let ((base (file-name-nondirectory (or file ""))))
    (when (string-match org-air-query--denote-id-regexp base)
      (match-string 1 base))))

(defun org-air-query--denote-id-time (id)
  "Parse denote identifier ID to an epoch float, or nil (R54-2)."
  (when (and (stringp id) (string-match-p "\\`[0-9]\\{8\\}T[0-9]\\{6\\}\\'" id))
    (ignore-errors
      (float-time
       (encode-time (string-to-number (substring id 13 15))
                    (string-to-number (substring id 11 13))
                    (string-to-number (substring id 9 11))
                    (string-to-number (substring id 6 8))
                    (string-to-number (substring id 4 6))
                    (string-to-number (substring id 0 4)))))))

(defun org-air-query--denote-slug (file)
  "Return FILE's raw denote title slug (hyphens kept), or nil (R54-2).
Data-layer twin of the view's F1 de-slug: the part between the `--'
separator and any `__tag' signature."
  (let ((base (file-name-nondirectory (or file ""))))
    (when (string-match (concat org-air-query--denote-id-regexp "--") base)
      (let* ((rest (file-name-sans-extension (substring base (match-end 0))))
             (slug (if (string-match "__" rest)
                       (substring rest 0 (match-beginning 0))
                     rest)))
        (unless (string-empty-p slug) slug)))))

(defun org-air-query--denote-filename-tags (file)
  "Return the `__tag_tag' keywords of FILE's denote name, or nil (R54-2).
Only the FALLBACK for files missing `#+filetags' front matter (denote
keeps the two in sync, so this is rare)."
  (let ((base (file-name-sans-extension
               (file-name-nondirectory (or file "")))))
    (when (and (string-match-p org-air-query--denote-id-regexp base)
               (string-match "__" base))
      (split-string (substring base (match-end 0)) "_" t))))

(defun org-air-query--parse-type (value)
  "Normalise VALUE (a string or symbol) to a note type symbol, or nil.
`note' and `knowledge' are synonyms; invalid values are IGNORED (fall
through the R54-2 precedence chain), never an error."
  (let ((sym (cond ((symbolp value) value)
                   ((stringp value) (intern (downcase (string-trim value))))
                   (t nil))))
    (pcase sym
      ('task 'task)
      ((or 'note 'knowledge) 'knowledge)
      ('journal 'journal)
      (_ nil))))

(defun org-air-query--tag-type (tags)
  "Return the type a tag in TAGS overrides to, or nil (R54-2 step 3).
First match in `org-air-note-type-tag-alist' wins (case-insensitive)."
  (cl-some (lambda (tag)
             (org-air-query--parse-type
              (cdr (assoc-string tag org-air-note-type-tag-alist t))))
           tags))

(defun org-air-query--date-shaped-name-p (base)
  "Non-nil when file name BASE is date-shaped (R54-2 journal heuristic).
ISO (`2026-07-15'), compact (`20260715'), or a denote ID-only name."
  (or (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" base)
      (string-match-p "\\`[0-9]\\{8\\}\\'" base)
      (string-match-p "\\`[0-9]\\{8\\}T[0-9]\\{6\\}\\'" base)))

(defun org-air-query--date-shaped-title-p (title)
  "Non-nil when TITLE is date-shaped (R54-2 journal heuristic).
ISO (`2026-07-15') or the denote-journal long form (`Tuesday 15 July
2026')."
  (and (stringp title)
       (let ((case-fold-search t))
         (or (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" title)
             (string-match-p
              "\\`[[:alpha:]]+day,?[ \t]+[0-9]\\{1,2\\}[ \t]+[[:alpha:]]+[ \t]+[0-9]\\{4\\}\\'"
              title)))))

(defun org-air-query--journal-file-p (file title)
  "Return non-nil when FILE (with `#+title' TITLE) is a journal (R54-2).
File-level, computed once per file: a date-shaped file name, a path
component matching `org-air-journal-directory-regexp', or a date-shaped
TITLE."
  (let ((case-fold-search t))
    (or (org-air-query--date-shaped-name-p (file-name-base (or file "")))
        (cl-some (lambda (component)
                   (string-match-p org-air-journal-directory-regexp component))
                 (split-string (or (file-name-directory (or file "")) "")
                               "/" t))
        (org-air-query--date-shaped-title-p title))))

(defvar org-air-query--scan-file-signals nil
  "The scanned file's per-FILE type signals, bound per file by the scan.
A plist (:override TYPE :journal FLAG :title TITLE :tags TAGS :created
FLOAT :ids IDS :links LINKS) computed ONCE per file by
`org-air-query--file-signals' — not per heading — and threaded to
`org-air-query--note-type' (R54-2; :ids/:links feed the R54-3 link
graph).")

(defun org-air-query--note-link (target)
  "Normalise raw bracket-link TARGET to a note-link string, or nil (R54-3).
Only the three note-link kinds survive: `denote:ID', `id:UUID' and
`file:' links landing on `.org' files (explicit `file:' prefix, or a
bare untyped bracket target naming an .org file — Org's own file-link
fallback); `https:' and every other link type returns nil (a web-linked
note can still be an orphan in the garden sense).  A `::search' suffix
is tolerated and stripped."
  (let ((target (car (split-string (or target "") "::"))))
    (cond
     ((string-prefix-p "denote:" target) target)
     ((string-prefix-p "id:" target) target)
     ((string-prefix-p "file:" target)
      (and (string-match-p "\\.org\\'" target) target))
     ((and (not (string-match-p "\\`[[:alpha:]][[:alnum:]+.-]*:" target))
           (string-match-p "\\.org\\'" target))
      (concat "file:" target))
     (t nil))))

(defun org-air-query--file-signals (file)
  "Compute FILE's per-file type signals in the current scan buffer (R54-2).
Bounded head-of-buffer regexps only (first 4KB): the `#+org_air_type:' /
`#+type:' keyword override (the namespaced spelling authoritative when
both are present), `#+title', `#+filetags', the journal heuristic and
the created date (denote filename ID, else `#+date:').  Returns the
plist documented on `org-air-query--scan-file-signals'."
  (org-with-wide-buffer
   (goto-char (point-min))
   (let* ((case-fold-search t)
          (bound (min (point-max) 4096))
          (title (save-excursion
                   (when (re-search-forward
                          "^#\\+title:[ \t]*\\(.+?\\)[ \t]*$" bound t)
                     (match-string-no-properties 1))))
          (tags (save-excursion
                  (when (re-search-forward
                         "^#\\+filetags:[ \t]*\\(.+?\\)[ \t]*$" bound t)
                    (split-string (match-string-no-properties 1)
                                  "[: \t]+" t))))
          (override
           (or (save-excursion
                 (when (re-search-forward
                        "^#\\+org_air_type:[ \t]*\\([^ \t\n]+\\)" bound t)
                   (org-air-query--parse-type
                    (match-string-no-properties 1))))
               (save-excursion
                 (when (re-search-forward
                        "^#\\+type:[ \t]*\\([^ \t\n]+\\)" bound t)
                   (org-air-query--parse-type
                    (match-string-no-properties 1))))))
          (created
           (or (org-air-query--denote-id-time
                (org-air-query--denote-file-id file))
               (save-excursion
                 (when (re-search-forward
                        "^#\\+date:[ \t]*\\(.+?\\)[ \t]*$" bound t)
                   (ignore-errors
                     (float-time
                      (org-timestamp-to-time
                       (org-timestamp-from-string
                        (match-string-no-properties 1)))))))))
          ;; R54-3 link graph: ONE bounded whole-buffer pass while the
          ;; file is already in the work buffer — `:ID:' property values
          ;; (the id: link resolution targets) and the note-to-note
          ;; outbound links.  Scan-TIME extraction only; resolution is a
          ;; finish-time PURE pass over the in-memory table
          ;; (`org-air-query--link-graph-finish') — never at render time,
          ;; never a file open.
          (ids (save-excursion
                 (let (acc)
                   (while (re-search-forward
                           "^[ \t]*:ID:[ \t]+\\([^ \t\n]+\\)" nil t)
                     (push (match-string-no-properties 1) acc))
                   (nreverse acc))))
          (links (save-excursion
                   (let (acc)
                     (while (re-search-forward org-link-bracket-re nil t)
                       (when-let* ((link (org-air-query--note-link
                                          (match-string-no-properties 1))))
                         (push link acc)))
                     (nreverse acc)))))
     (list :override override
           :journal (and (org-air-query--journal-file-p file title) t)
           :title title
           :tags tags
           :created created
           :ids ids
           :links links))))

(defun org-air-query--note-type (todo scheduled deadline tags)
  "Derive the note type for the heading at point (R54-2, USER-RULED).
Precedence: the inherited `ORG_AIR_TYPE' property, the file keyword
override, an override tag (`org-air-note-type-tag-alist'), the TASK
signal (a TODO keyword — done or not — OR scheduled OR deadline;
nothing else — a bare active <ts> is a note fact, not a task), the
journal file heuristic, else `org-air-plain-heading-type'.  TODO,
SCHEDULED, DEADLINE and TAGS are the already-parsed heading signals.
R77: under `org-air-task-requires-todo' the task signal narrows to the
keyword ALONE (step 4) and the fall-through is `knowledge' (step 6 —
the knob's total contract subsumes `org-air-plain-heading-type')."
  (or (org-air-query--parse-type
       (org-entry-get (point) "ORG_AIR_TYPE" t))
      (plist-get org-air-query--scan-file-signals :override)
      (org-air-query--tag-type tags)
      ;; R77 step 4: the task signal — knob-gated to the keyword alone.
      ;; Knob nil is byte-equivalent to the R54 disjunction.
      (and (or todo
               (and (not org-air-task-requires-todo)
                    (or scheduled deadline)))
           'task)
      (and (plist-get org-air-query--scan-file-signals :journal) 'journal)
      ;; R77 step 6: with the knob ON the fall-through is `knowledge'
      ;; even under the legacy `org-air-plain-heading-type' 'task —
      ;; "task requires a keyword" wins the contradiction (D2).
      (if org-air-task-requires-todo 'knowledge org-air-plain-heading-type)))

(defun org-air-query--file-ntype (signals items)
  "Return the FILE-level type from SIGNALS and its heading ITEMS (R54-2).
Override → tag override → journal → `task' iff EVERY NON-container
heading item is a task (and there is at least one — the F7 mixed-file
rule: a pure GTD file stays off the note surfaces while a KB note
containing one TODO stays a knowledge FILE) → else `knowledge'.
R59: containers ABSTAIN — they are structure, so a GTD file organised
as `* Projects' / `** TODO …' still votes `task'; the abstention is
knob-gated inside `org-air-query-container-item-p', so
`org-air-skip-container-headings' nil restores the pre-R59 vote
verbatim."
  (or (plist-get signals :override)
      (org-air-query--tag-type (plist-get signals :tags))
      (and (plist-get signals :journal) 'journal)
      (and items
           (let ((voters (cl-remove-if #'org-air-query-container-item-p
                                       items)))
             (and voters
                  (cl-every (lambda (item)
                              (eq (org-air-item-ntype item) 'task))
                            voters)))
           'task)
      'knowledge))

(defvar org-air-query--file-meta (make-hash-table :test #'equal)
  "The per-file fact table: FILE → plist (R54-2, cache v4).
Keys: `:title' (`#+title', else the denote slug, else nil — `:org-title'
holds the raw `#+title' alone so display fallbacks stay exact), `:tags'
\(`#+filetags', else the filename `__tags' fallback), `:ntype' (the
FILE's type, F7 rule), `:mtime' and `:created' (epoch floats).  R54-3
link-graph keys: `:ids' (the file's `:ID:' property values), `:links-raw'
\(scan-time outbound note links, unresolved), `:links-out' (the resolved
outbound list — FILE paths where resolvable, the raw link string where
not: unresolvable intent counts outbound but creates no inbound) and
`:links-in' (the inbound count, one pure inversion pass at scan finish).
Updated per scanned file; persisted in the cache as `:file-meta' and
hydrated back on cache load, so a warm board answers file-level
questions with ZERO file opens.")

(defvar org-air-query--link-graph-dirty nil
  "Non-nil when file-meta gained scan entries since the last resolution.
Set by `org-air-query--file-meta-record'; cleared by the pure
`org-air-query--link-graph-finish' pass (R54-3).")

(defvar org-air-query--denote-id-index (make-hash-table :test #'equal)
  "Index denote ID → FILE for the read-only `denote:' link shim (R54-2).
Pure filename derivation, populated as the scan enumerates files and
re-derived from the cache's `:file-meta' keys on hydration.")

(defun org-air-query-file-meta (file)
  "Return the recorded per-file plist for FILE, or nil (R54-2)."
  (gethash file org-air-query--file-meta))

(defun org-air-query--index-denote-id (file)
  "Record FILE under its denote identifier, when it carries one (R54-2)."
  (when-let* ((id (org-air-query--denote-file-id file)))
    (puthash id file org-air-query--denote-id-index)))

(defun org-air-query--file-meta-record (file signals items)
  "Record FILE's per-file facts from SIGNALS and its heading ITEMS (R54-2)."
  (puthash file
           (list :title (or (plist-get signals :title)
                            (org-air-query--denote-slug file))
                 :org-title (plist-get signals :title)
                 :tags (or (plist-get signals :tags)
                           (org-air-query--denote-filename-tags file))
                 :ntype (org-air-query--file-ntype signals items)
                 :mtime (when-let* ((mtime
                                     (or org-air-query--scan-mtime
                                         (and (file-exists-p file)
                                              (file-attribute-modification-time
                                               (file-attributes file))))))
                          (float-time mtime))
                 :created (plist-get signals :created)
                 ;; R54-3: the raw link-graph facts; resolution is the
                 ;; finish-time pure pass (`--link-graph-finish').
                 :ids (plist-get signals :ids)
                 :links-raw (plist-get signals :links))
           org-air-query--file-meta)
  (setq org-air-query--link-graph-dirty t))

(defun org-air-query--link-graph-finish ()
  "Resolve the note-link graph over the in-memory file-meta table (R54-3).
A PURE pass — zero file opens: builds the denote-ID / `:ID:' / normalised
path indexes from the table itself, resolves every file's `:links-raw'
into `:links-out' (resolved targets become FILE paths; an unresolvable
note link keeps its raw string — outbound intent with no inbound edge;
self-links are dropped) and computes `:links-in' by one in-memory
inversion.  Never called at render time per row — the Revisit view runs
it at most once per dirty table (`org-air-query-link-graph-ensure')."
  (let ((ids (make-hash-table :test #'equal))
        (paths (make-hash-table :test #'equal))
        (inbound (make-hash-table :test #'equal)))
    (maphash (lambda (file meta)
               (puthash (expand-file-name file) file paths)
               (dolist (id (plist-get meta :ids))
                 (puthash id file ids))
               (org-air-query--index-denote-id file))
             org-air-query--file-meta)
    (maphash
     (lambda (file meta)
       (let (out)
         (dolist (raw (plist-get meta :links-raw))
           (let* ((resolved
                   (cond
                    ((string-prefix-p "denote:" raw)
                     (gethash (substring raw (length "denote:"))
                              org-air-query--denote-id-index))
                    ((string-prefix-p "id:" raw)
                     (gethash (substring raw (length "id:")) ids))
                    ((string-prefix-p "file:" raw)
                     (let ((path (substring raw (length "file:"))))
                       (gethash (expand-file-name
                                 path (file-name-directory file))
                                paths)))))
                  (target (or resolved raw)))
             (unless (or (equal target file) (member target out))
               (push target out)
               (when resolved
                 (cl-incf (gethash resolved inbound 0))))))
         (puthash file (plist-put meta :links-out (nreverse out))
                  org-air-query--file-meta)))
     org-air-query--file-meta)
    (maphash (lambda (file meta)
               (puthash file
                        (plist-put meta :links-in (gethash file inbound 0))
                        org-air-query--file-meta))
             org-air-query--file-meta))
  (setq org-air-query--link-graph-dirty nil))

(defun org-air-query-link-graph-ensure ()
  "Run the link-graph resolution iff the table gained scans (R54-3).
Idempotent and cheap (one in-memory pass over the file-meta table);
safe to call before any consumer read (the Revisit ORPHANS mode, the
cache serialisation)."
  (when org-air-query--link-graph-dirty
    (org-air-query--link-graph-finish)))

(defun org-air-query-file-meta-alist (files)
  "Return the file-meta entries for FILES as a printable alist (R54-2).
The cache serialisation form: pruned to FILES, so vanished files never
persist.  R54-3: the link graph is resolved first, so the persisted
entries carry `:links-out'/`:links-in' and a warm ORPHANS render is
data-pure with no resolution pass."
  (org-air-query-link-graph-ensure)
  (let (out)
    (dolist (file files)
      (when-let* ((meta (gethash file org-air-query--file-meta)))
        (push (cons file meta) out)))
    (nreverse out)))

(defun org-air-query-file-meta-hydrate (alist)
  "Hydrate the file-meta table (and denote index) from cache ALIST (R54-2).
R54-3: metas lacking the link shape (`:links-out') are skipped — a
part-1 v4 cache would otherwise hydrate link-less metas that read as
ALL-orphans and, worse, get re-persisted by the next warm cache write.
Skipping them hydrates an empty file-meta table instead: Revisit's
ensure-data paces a fill (correct, never-hang); the board is untouched
since items hydrate separately."
  (pcase-dolist (`(,file . ,meta) alist)
    (when (and (stringp file) (listp meta) (plist-member meta :links-out))
      (puthash file meta org-air-query--file-meta)
      (org-air-query--index-denote-id file))))

;;;; ---------------------------------------------------------------------
;;;; R54-3 — the bounded VISIT LEDGER (opt-in after the D2 ruling).
;;;; org-air records opens IT initiates (board S-RET / g RET, the pane
;;;; RET, revisit RET) — NEVER a global `find-file' hook: org-air does
;;;; not instrument buffers it does not own.
;;;; ---------------------------------------------------------------------

(defvar org-air-query--visits (make-hash-table :test #'equal)
  "The visit ledger: FILE → epoch float of the last org-air open (R54-3).
Written only when `org-air-revisit-visit-ledger' is non-nil (the D2
ruling demoted the ledger to OPT-IN; age is pure mtime by default).
Persisted in the cache as `:visits' (alist) and hydrated back; BOUNDED:
pruned to the enumerated file set at cache write, so its size can never
exceed the configured file count.")

(defun org-air--note-visited (file)
  "Record an org-air-initiated open of FILE in the visit ledger (R54-3).
A no-op unless `org-air-revisit-visit-ledger' is non-nil (USER-RULED D2:
last-modified is the default attention-age signal; the ledger is the
opt-in refinement).  Called only from org-air's OWN open paths."
  (when (and (bound-and-true-p org-air-revisit-visit-ledger)
             (stringp file) (not (string-empty-p file)))
    (puthash file (float-time) org-air-query--visits)))

(defun org-air-query-note-visit (file)
  "Return the ledger epoch float of FILE's last org-air open, or nil."
  (gethash file org-air-query--visits))

(defun org-air-query-visits-alist (files)
  "Return the visit ledger pruned to FILES as a printable alist (R54-3).
The cache serialisation form — the prune IS the bound: entries for files
no longer enumerated are dropped here and, since this feeds the write,
never persist."
  (let (out)
    (dolist (file files)
      (when-let* ((time (gethash file org-air-query--visits)))
        (push (cons file time) out)))
    (nreverse out)))

(defun org-air-query-visits-hydrate (alist)
  "Hydrate the visit ledger from cache ALIST (R54-3).
An in-session visit newer than the cached epoch wins (`max') — a cache
read must never clobber a fresher visit with a staler one."
  (pcase-dolist (`(,file . ,time) alist)
    (when (and (stringp file) (numberp time))
      (puthash file (max time (or (gethash file org-air-query--visits) 0.0))
               org-air-query--visits))))

(defun org-air-query--denote-resolve (id)
  "Resolve denote identifier ID to a configured file, or nil (R54-2).
An O(1) hit on the scan's ID index, else one bounded pass over the
enumerated file list (a cold Emacs following a link before any scan)."
  (or (gethash id org-air-query--denote-id-index)
      (cl-find-if (lambda (file)
                    (string-prefix-p id (file-name-nondirectory file)))
                  (ignore-errors (org-air-query-files)))))

(defun org-air-query--denote-follow (link &optional _prefix)
  "Follow a `denote:' LINK read-only against `org-air-files' (R54-2).
Resolves the ID by filename convention (no denote required, no DB); a
`::search' suffix is tolerated but ignored.  Authoring (creation,
renaming, completion) stays denote's — this shim only keeps existing
links alive in a denote-less Emacs."
  (let* ((id (car (split-string (or link "") "::")))
         (file (org-air-query--denote-resolve id)))
    (if file
        (find-file file)
      (user-error "No note with denote ID %s under `org-air-files'" id))))

(defun org-air-query-register-denote-link ()
  "Register the read-only `denote:' follower IFF none exists (R54-2).
When denote (or anything else) already claims the link type, org-air
leaves it alone — never a `denote-*' call, works with denote absent."
  (unless (org-link-get-parameter "denote" :follow)
    (org-link-set-parameters "denote"
                             :follow #'org-air-query--denote-follow)))

(org-air-query-register-denote-link)

(defun org-air-query--item-at-point ()
  "Build an `org-air-item' for the heading at point.
R53 P2: also records the scan-time slots (`kind'/`donep'/`activity'/
`body-deadline') so classify/render never open the file again, and the
marker slot is the durable (FILE . POS) cons (source buffers are never
retained by scanning; live positions resolve on demand)."
  (let* ((file (or (buffer-file-name) ""))
         ;; R90 final: one Org-native projection owns title/local/effective
         ;; tags for query generation and source validation.  Literal suffixes
         ;; outside `org-tag-re' stay title bytes, exactly as Org reports.
         (projection (org-air-query--heading-projection))
         (title (plist-get projection :title))
         (todo (org-get-todo-state))
         (tags (plist-get projection :effective-tags))
         (scheduled (org-air-query--timestamp "SCHEDULED"))
         (deadline (org-air-query--timestamp "DEADLINE"))
         (closed (org-air-query--timestamp "CLOSED"))
         (subtree-ts nil)
         (active-ts nil)
         (active-ts-pos nil)
         (child-pos nil)
         (body-deadline nil)
         (harvest nil))
    ;; R53 P2: the two bounded subtree probes, run HERE in the already-
    ;; positioned scan buffer (they used to be per-item render-time file
    ;; opens — the 186s warm-paint hang).
    (save-excursion
      (let ((end (save-excursion (ignore-errors (org-end-of-subtree t t))
                                 (point))))
        (save-excursion
          (when (re-search-forward org-ts-regexp-both end t)
            (setq subtree-ts
                  (ignore-errors
                    (float-time
                     (org-timestamp-to-time
                      (org-timestamp-from-string
                       (match-string-no-properties 0))))))))
        ;; R54-1: the ACTIVE-only twin probe (`org-ts-regexp': planning
        ;; lines included, inactive [..] excluded) — the stale-eligibility
        ;; signal.  Distinct from `subtree-ts' (regexp-both), which the
        ;; day view's Logged/created group needs and which must keep
        ;; matching inactive stamps.
        (save-excursion
          (when (re-search-forward org-ts-regexp end t)
            (setq active-ts-pos (match-beginning 0))
            (setq active-ts
                  (ignore-errors
                    (float-time
                     (org-timestamp-to-time
                      (org-timestamp-from-string
                       (match-string-no-properties 0))))))))
        ;; R59: the CHILD probe — with the subtree END already in hand,
        ;; one bounded search from past the heading line; any
        ;; `org-outline-regexp-bol' match is a descendant (the first is
        ;; the heading's first child).  Same shape and cost class as the
        ;; subtree-ts/active-ts probes beside it.
        (save-excursion
          (forward-line 1)
          (when (re-search-forward org-outline-regexp-bol end t)
            (setq child-pos (match-beginning 0))))
        (unless deadline
          (save-excursion
            (when (re-search-forward org-deadline-time-regexp end t)
              (setq body-deadline
                    (ignore-errors
                      (float-time
                       (org-timestamp-to-time
                        (org-timestamp-from-string
                         (format "<%s>"
                                 (match-string-no-properties 1))))))))))
        ;; R61-1: the review harvest — SAME pass, same buffer, reusing END
        ;; and `child-pos' so the own-body region costs zero extra
        ;; structural work.  Per-heading never-error (the inner net lives
        ;; inside the helper); capped by `org-air-log-cap'.
        (setq harvest (org-air-query--harvest-at-point child-pos end))))
    (org-air-item-create
     :title title
     :tags tags
     :file file
     ;; R53 P1: (FILE . POS), first-class everywhere since R26-8 — the
     ;; scan retains NO buffer.  A file-less buffer (a test temp buffer)
     ;; keeps the live marker so at-point flows still resolve.
     :marker (if (string-empty-p file)
                 (copy-marker (point-marker))
               (cons file (point)))
     :todo todo
     ;; R22-1: detect an EXPLICIT [#X] cookie via `org-priority-regexp'
     ;; (group 2 = the letter, A..E), so [#B] is recorded even though its
     ;; value equals `org-default-priority' (=?B); a cookie-LESS heading
     ;; stays nil.  The old value-equals-default test dropped explicit [#B].
     :priority (let ((heading (org-get-heading t t nil t)))
                 (when (string-match org-priority-regexp heading)
                   (org-get-priority heading)))
     :scheduled scheduled
     :deadline deadline
     :closed closed
     :group (org-air-query--group file)
     :kind 'heading
     :donep (and todo (member todo org-done-keywords) t)
     :subtree-ts subtree-ts
     :activity (or (org-air-query--time-float closed)
                   (org-air-query--time-float scheduled)
                   (org-air-query--time-float deadline)
                   subtree-ts
                   (when-let* ((mtime
                                (or org-air-query--scan-mtime
                                    (and (not (string-empty-p file))
                                         (file-exists-p file)
                                         (file-attribute-modification-time
                                          (file-attributes file))))))
                     (float-time mtime)))
     :body-deadline body-deadline
     :active-ts active-ts
     ;; R54-2: the content-derived note type, computed here in the scan
     ;; buffer over signals already in hand (the per-FILE signals are
     ;; computed once per file, not per heading).
     :ntype (org-air-query--note-type todo scheduled deadline tags)
     ;; R59: the two container signals.  `own-active-ts' costs ZERO
     ;; extra regex work: the active-ts probe finds the subtree's FIRST
     ;; active match, and matches are ordered — the own body carries one
     ;; iff that first match precedes the first child (for a leaf,
     ;; own-active-ts ≡ active-ts).
     :childp (and child-pos t)
     :own-active-ts (and active-ts
                         (or (null child-pos)
                             (< active-ts-pos child-pos))
                         active-ts)
     ;; R61-1: the four review slots (integer epochs + interned symbols
     ;; only — data-pure period folds, zero render-time file opens).
     :clocks (nth 0 harvest)
     :logs (nth 1 harvest)
     :created (nth 2 harvest)
     :rtrunc (nth 3 harvest)
     ;; R93: the recency fact.  nil means "this heading carries NO
     ;; history at all"; classify then falls back to the file's
     ;; scan-time mtime (`org-air-query-file-meta' `:mtime' — a hash
     ;; lookup, never a stat), which is deliberately COARSE: one edit
     ;; anywhere in the file refreshes every historyless heading in it.
     :updated (nth 4 harvest))))

;;;; ---------------------------------------------------------------------
;;;; R53 P1/P1b — the never-error work-buffer scan.
;;;; ---------------------------------------------------------------------

(defvar org-air-query--skip-log nil
  "Per-scan list of (FILE . REASON) entries the scan skipped (R53 P1b).
Cleared at the start of every full scan (`org-air-query-skip-log-reset');
listed by `org-air-scan-report'.  REASON is a symbol (`encrypted',
`too-large', `slow') or an error string.")

(defun org-air-query-skip-log-reset ()
  "Clear the per-scan skip log (R53 P1b).  Called once per scan start."
  (setq org-air-query--skip-log nil))

(defun org-air-query--skip (file reason)
  "Record FILE as skipped for REASON in the scan's skip log; return nil.
Never messages per file — the scan reports ONE summary line itself and
`org-air-scan-report' lists the details (no echo spam at 5000 files)."
  (push (cons file reason) org-air-query--skip-log)
  nil)

;;;###autoload
(defun org-air-scan-report ()
  "List the files the last org-air scan skipped, and why (R53 P1b)."
  (interactive)
  (if (null org-air-query--skip-log)
      (message "org-air: the last scan skipped no files")
    (let ((entries (reverse org-air-query--skip-log)))
      (with-current-buffer (get-buffer-create "*org-air scan report*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "org-air scan report — %d file(s) skipped\n\n"
                          (length entries)))
          (pcase-dolist (`(,file . ,reason) entries)
            (insert (format "  %-12s %s\n"
                            (if (symbolp reason) (symbol-name reason)
                              (format "%s" reason))
                            file)))
          (goto-char (point-min)))
        (special-mode)
        (pop-to-buffer (current-buffer))))))

(defvar org-air-query--work-buffer nil
  "The single reused scan work buffer, or nil (R53 P1).
A NORMAL-named buffer (org-ql drops space-prefixed ones), the mode
`org-mode' initialised ONCE per session under the
symbol `delay-mode-hooks' (mode hooks never run), element cache ON but
`org-element-cache-persistent' nil buffer-locally (org-persist stays out
of the shared buffer).  Killed by `org-air-query-teardown'.")

(defun org-air-query--work-buffer ()
  "Return the live scan work buffer, creating it on first use (R53 P1)."
  (unless (buffer-live-p org-air-query--work-buffer)
    (setq org-air-query--work-buffer (generate-new-buffer "*org-air scan*"))
    (with-current-buffer org-air-query--work-buffer
      (delay-mode-hooks (org-mode))
      (setq-local org-element-cache-persistent nil)))
  org-air-query--work-buffer)

(defun org-air-query-teardown ()
  "Kill the session's scan work buffer, if any (R53 P1)."
  (when (buffer-live-p org-air-query--work-buffer)
    (kill-buffer org-air-query--work-buffer))
  (setq org-air-query--work-buffer nil))

(defun org-air-query--inbox-file-p (file)
  "Return non-nil when FILE is `org-air-inbox-file' (P3 exclusion)."
  (and (boundp 'org-air-inbox-file)
       org-air-inbox-file
       (equal (or (ignore-errors (file-truename (expand-file-name file)))
                  (expand-file-name file))
              (or (ignore-errors
                    (file-truename (expand-file-name org-air-inbox-file)))
                  (expand-file-name org-air-inbox-file)))))

(defun org-air-query--file-item (file)
  "Return a one-item list for FILE as a headingless note, or nil (R53 P3).
Called with the scanned content in the current buffer AFTER the heading
scan yielded nothing.  A REAL note — no headings, some non-blank content,
no NUL byte in the first 1KB (binary junk never becomes a row), and not
the inbox file (its emptiness is chrome, not content) — synthesises ONE
openable item: `kind' `file', title from `#+title' (file name base
fallback), tags from `#+filetags', group = parent directory name, marker
\(FILE . 1) so RET opens the file at the top."
  (let ((case-fold-search t))
    (org-with-wide-buffer
     (goto-char (point-min))
     (unless (or (string-empty-p file)
                 (re-search-forward org-outline-regexp-bol nil t)
                 (save-excursion
                   (goto-char (point-min))
                   (search-forward "\0" (min (point-max) 1024) t))
                 (not (save-excursion
                        (goto-char (point-min))
                        (re-search-forward "[^ \t\r\n]" nil t)))
                 (org-air-query--inbox-file-p file))
       (goto-char (point-min))
       (let ((title (when (re-search-forward
                           "^#\\+title:[ \t]*\\(.+?\\)[ \t]*$" nil t)
                      (match-string-no-properties 1)))
             (tags (save-excursion
                     (goto-char (point-min))
                     (when (re-search-forward
                            "^#\\+filetags:[ \t]*\\(.+?\\)[ \t]*$" nil t)
                       (split-string (match-string-no-properties 1)
                                     "[: \t]+" t)))))
         (list
          (org-air-item-create
           :title (if (and title (not (string-empty-p title)))
                      title
                    (file-name-base file))
           :tags tags
           :file file
           :marker (cons file 1)
           :todo nil :priority nil
           :scheduled nil :deadline nil :closed nil
           :group (file-name-nondirectory
                   (directory-file-name (file-name-directory file)))
           :kind 'file
           :donep nil
           :activity (when-let* ((mtime
                                  (or org-air-query--scan-mtime
                                      (and (file-exists-p file)
                                           (file-attribute-modification-time
                                            (file-attributes file))))))
                       (float-time mtime))
           :body-deadline nil
           :active-ts nil
           ;; R59: nil container signals — the predicate requires `kind'
           ;; `heading' anyway, so a 'file item is never a container.
           :childp nil
           :own-active-ts nil
           ;; R61-1: nil review slots — a file blob has no per-heading
           ;; LOGBOOK; the review sections ignore 'file items entirely.
           ;; R93: `updated' likewise — a 'file item routes to `notes'
           ;; and never reaches the attention clock.
           :clocks nil :logs nil :created nil :rtrunc nil :updated nil
           ;; R54-2: 'file items type from the FILE-level signals alone
           ;; (keyword/tag override → journal → knowledge); they route to
           ;; the 'notes bucket regardless, so this feeds the note
           ;; surfaces, not the board.
           :ntype (org-air-query--file-ntype
                   org-air-query--scan-file-signals nil))))))))

(defun org-air-query--scan-live-buffer (buffer file query)
  "Scan the live user BUFFER visiting FILE with org-ql QUERY (R53 P1 rule 1).
Unsaved edits are respected; every item's marker/file slot is rewritten to
FILE so the (FILE . POS) contract and the mtime bookkeeping stay coherent
even when the buffer's own name differs (a symlinked visit)."
  (let* (;; R53fix M2: same echo hygiene as the work-buffer path — a live
         ;; headingless buffer must not re-spam org-ql's "No headings in
         ;; buffer" message on every refresh.
         (inhibit-message t)
         (message-log-max nil)
         ;; R93: today's date string, computed ONCE per file instead of
         ;; once per heading (the `updated' probe's future-stamp guard).
         (org-air-query--scan-today (format-time-string "%Y-%m-%d"))
         ;; R54-2: the per-FILE type signals, computed ONCE per file.
         (org-air-query--scan-file-signals
          (with-current-buffer buffer
            (org-air-query--file-signals file)))
         (items (copy-sequence
                 (org-ql-select buffer (or query '(heading))
                   :action #'org-air-query--item-at-point))))
    (dolist (item items)
      (setf (org-air-item-file item) file)
      (let ((m (org-air-item-marker item)))
        (setf (org-air-item-marker item)
              (cons file (cond ((consp m) (or (cdr m) 1))
                               ((markerp m) (or (marker-position m) 1))
                               (t 1))))))
    (org-air-query--file-meta-record file org-air-query--scan-file-signals
                                     items)
    (or items
        (with-current-buffer buffer
          (org-air-query--file-item file)))))

(defun org-air-query--scan-work-buffer (file query)
  "Scan FILE in the reused work buffer with org-ql QUERY (R53 P1 rule 2).
One `erase-buffer' + `insert-file-contents' per file into the session's
single `org-mode' work buffer; the
variable `buffer-file-name' is set for the file's extent (so Org's
file-relative logic behaves) and always cleared again;
`org-set-regexps-and-options' makes the file's own
`#+TODO:' win, with the R57-1 MERGED default vocabulary otherwise (the
user's global `org-todo-keywords' as the base + org-air's supplement —
never a replacement; see `org-air-query--scan-todo-keywords').  Known,
accepted difference: file-local variable BLOCKS are not processed here
\(`#+…' keywords ARE); a file whose parsing genuinely depends on local
variables scans like the same Org file without them."
  (with-current-buffer (org-air-query--work-buffer)
    (let ((buffer-undo-list t)
          (create-lockfiles nil)
          ;; Kills org-ql's per-file "No headings in buffer" echo spam ×N;
          ;; the scan reports ONE summary line itself (R53 P1b).
          (inhibit-message t)
          (message-log-max nil)
          (org-todo-keywords (org-air-query--scan-todo-keywords))
          ;; R93: the `updated' probe's future-stamp guard, once per file.
          (org-air-query--scan-today (format-time-string "%Y-%m-%d"))
          (org-air-query--scan-mtime
           (file-attribute-modification-time (file-attributes file))))
      (unwind-protect
          (progn
            (erase-buffer)
            (insert-file-contents file)
            (setq buffer-file-name file)
            (org-set-regexps-and-options)
            ;; R54-2: the per-FILE type signals, computed ONCE per file
            ;; and threaded to the per-heading action via the scan-scoped
            ;; binding (like `org-air-query--scan-mtime').
            (let* ((org-air-query--scan-file-signals
                    (org-air-query--file-signals file))
                   (items (copy-sequence
                           (org-ql-select (current-buffer)
                             (or query '(heading))
                             :action #'org-air-query--item-at-point))))
              (org-air-query--file-meta-record
               file org-air-query--scan-file-signals items)
              (or items (org-air-query--file-item file))))
        (setq buffer-file-name nil)
        (set-buffer-modified-p nil)))))

(defun org-air-query--scan-file (file &optional query)
  "Return `org-air-item' records for FILE; NEVER signals (R53 P1/P1b).
The one per-file scan entry: a live user buffer visiting FILE is scanned
in place (rule 1, cheap `get-file-buffer' — never a `buffer-list' walk);
otherwise the file scans in the single reused work buffer (rule 2).  The
policy table applies BEFORE any read: an `.org.gpg' with no live buffer
is skipped `encrypted' (the background scan NEVER decrypts or prompts; a
live already-decrypted buffer scans normally), an over-
`org-air-max-file-size' file is skipped `too-large', an unreadable /
vanished / dangling-symlink file is skipped with its `file-error'.  ANY
signal inside the body degrades to 0 items + one skip-log entry — a bad
file can never abort the whole scan (the P1b never-error law).  QUERY is
the optional org-ql query (default: all headings).  A `quit' is NOT
swallowed: aborting always works."
  (org-air-query--index-denote-id file)
  (condition-case err
      (let ((live (get-file-buffer file)))
        (cond
         (live (org-air-query--scan-live-buffer live file query))
         ((string-match-p "\\.gpg\\'" file)
          (org-air-query--skip file 'encrypted))
         ((not (file-readable-p file))
          (org-air-query--skip file 'unreadable))
         ((let ((size (file-attribute-size (file-attributes file))))
            (and org-air-max-file-size size
                 (> size org-air-max-file-size)))
          (org-air-query--skip file 'too-large))
         (t (org-air-query--scan-work-buffer file query))))
    (error (org-air-query--skip file (error-message-string err)))))

;;;###autoload
(defun org-air-query-items (&optional query)
  "Return `org-air-item' records matching org-ql QUERY.

When QUERY is nil, return all headings from `org-air-files' (plus one
bounded file-item per headingless note file, R53 P3).  R53 P1: the scan
loops `org-air-query--scan-file' over the configured files — org-ql stays
the only query engine, but it runs over buffers org-air manages, so no
source buffer is ever retained and one bad file can never abort the scan.
Item order is file order × buffer order, exactly as before."
  (let ((files (org-air-query-files)))
    (org-air-query-skip-log-reset)
    (let (items)
      (dolist (file files)
        (setq items (nconc items (org-air-query--scan-file file query))))
      items)))

(defun org-air-query-items-in-files (files &optional query)
  "Return `org-air-item' records for FILES, a subset of the configured set.

Like `org-air-query-items' but restricted to FILES (already-expanded Org
file paths), so the query can be split into batches and run on an idle
timer without blocking the frame (R19-1).  QUERY defaults to all
headings.  Does NOT reset the skip log — the caller (the refresh machine)
owns the per-scan log across its slices."
  (let (items)
    (dolist (file files)
      (setq items (nconc items (org-air-query--scan-file file query))))
    items))

(provide 'org-air-query)

;; Local Variables:
;; package-lint-main-file: "org-air.el"
;; End:
;;; org-air-query.el ends here
