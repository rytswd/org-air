;;; org-air-round66-test.el --- executing ERTs for round-66 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for round-66 (air/v0.1/org-air-round66-design.org):
;; refiling to a NEW or frontmatter-less target synthesises Air
;; frontmatter — a derived `#+title:' (org's own parsers: TODO keyword
;; per the source buffer's merged R57 vocabulary, priority cookie, tags,
;; statistics cookies and COMMENT all stripped), `#+state:' from the
;; `org-air-refile-new-file-state' defcustom, `#+FILETAGS:' from the
;; moved heading's effective tags — gated on an Air-managed tree by the
;; three-valued `org-air-refile-synthesize-frontmatter' defcustom, all
;; inside the R64 disk-atomic transaction; the v0.2 target-directory
;; creation is folded in.  All BATCH/headless through the
;; non-interactive `org-air-refile-item' (the R64 T-idiom); no transient
;; event loop.  The spec's nine seams T1-T9 map onto the nine ERTs:
;;
;;   r66-1 THE REPRO (T1) — refile `* TODO [#A] Set up syncthing :#Nix:'
;;         to a fresh file in the Air workspace ⇒ the file BEGINS with
;;         the exact four-line block (title/state/FILETAGS/blank), then
;;         the moved subtree; `org-air-project--read-doc' (the airctl
;;         classification) yields name "Set up syncthing" and state
;;         "draft" — never "unknown".  RED pre-R66: subtree only.
;;   r66-2 IDEMPOTENCE (T2) — an already-titled target's frontmatter
;;         region is byte-identical after the refile, exactly ONE
;;         `#+title:' in the file, state untouched.  RED against a
;;         naive unconditional insert.
;;   r66-3 THE STRIP (T3) — file-local `#+TODO: WAIT | ARCHIVED'
;;         respected (WAIT stripped exactly when the buffer declares
;;         it), priority + tags + trailing AND inline statistics
;;         cookies + COMMENT all stripped; WAIT survives where it is
;;         NOT a keyword (anti-tautology).
;;   r66-4 STATE DEFCUSTOM + FALLBACK (T4) — a let-bound "ready" lands
;;         as `#+state: ready'; the degenerate `* TODO [1/2]' heading
;;         falls back to the target's `file-name-base'.
;;   r66-5 THE GATE (T5) — default t: a non-Air target stays BARE; the
;;         same target with `always' synthesises; an Air-tree target
;;         with nil stays bare; an explicit `org-air-projects' root
;;         with neither marker still gates ON (the configured-roots
;;         leg).  Each leg reverts RED independently.
;;   r66-6 FRONTMATTER-LESS EXISTING FILE (T6) — block inserted at the
;;         top, original content byte-preserved below, item appended;
;;         a file-level `:PROPERTIES:' drawer keeps its org-required
;;         first place (block AFTER it); a SECOND refile into the same
;;         file leaves exactly one `#+title:'.
;;   r66-7 NEW-FILE FAILURE = NO FILE (T7) — a metadata failure after
;;         the cut leaves NO target file on disk (not even title-only),
;;         the source byte-identical (R64 restore) and the created
;;         directory empty + inert (no enumeration row); the RETRY
;;         without the fault lands exactly ONE frontmatter block
;;         (residue-safe buffer-level idempotence).
;;   r66-8 DIRECTORY FOLD-IN + PROMPT PURITY (T8) — a target two
;;         missing directories deep succeeds (dirs created, doc reads
;;         back); the prompt-time readers (`f' via `⌂ other file…', the
;;         pure `p' resolver) mutate NOTHING — the directory stays
;;         non-existent (the R64 pin extended to mkdir); the preview's
;;         `(creates: …)' annotation notes `new file'.
;;   r66-9 FILETAGS SHAPES (T9) — TAGS-arg wins over item tags, `:none'
;;         and a tagless item omit the line, `inbox' is excluded, the
;;         `#'-prefixed user tag round-trips through
;;         `org-air-project--read-doc', and the moved heading's own tag
;;         list is untouched by synthesis.
;;   r66-10 STEP-0 ORDER LOCK (audit R66) — synthesis runs BEFORE
;;         `--resolve-target' (the spec's pos-1 marker hazard, Decision
;;         2), pinned by BYTES: a parent heading resolved at position 1
;;         of a frontmatter-less file still receives the item INSIDE
;;         its subtree (before the next sibling) — a resolve-first
;;         implementation strands the parent marker on the inserted
;;         `#+title:' line and misfiles the item at buffer end, under
;;         the LAST top-level heading (measured).

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
;;;; Fixture: an Air workspace + a non-Air sibling (the shared tree)
;;;; -------------------------------------------------------------------

(defvar org-air-r66--root nil
  "The temp root of the current `org-air-r66--with-tree'.")
(defvar org-air-r66--ws nil
  "The Air workspace root (carries an empty `air-config.toml').")
(defvar org-air-r66--plain nil
  "The NON-Air sibling directory (no marker anywhere up to the root).")

(defconst org-air-r66--repro-inbox
  "#+title: org-air inbox\n\n* TODO [#A] Set up syncthing :#Nix:\n"
  "The user's verbatim repro capture (spec Summary).")

(defmacro org-air-r66--with-tree (specs &rest body)
  "Create the shared R66 fixture tree from SPECS and run BODY.
A temp root holding an Air workspace `ws/' (empty `air-config.toml' —
the airctl marker) and a NON-Air sibling `plain/' outside any marker.
SPECS is a list of (NAME . CONTENT) files written under `ws/'.  Binds
`org-air-inbox-file' to ws/inbox.org and `org-air-projects' /
`org-air-sources' to nil so the configured-roots leg of the gate never
leaks in from anywhere but the test's own let.  Kills every
fixture-visiting buffer and deletes the tree afterwards."
  (declare (indent 1) (debug t))
  `(let* ((org-air-r66--root (make-temp-file "org-air-r66-" t))
          (org-air-r66--ws (expand-file-name "ws" org-air-r66--root))
          (org-air-r66--plain (expand-file-name "plain" org-air-r66--root)))
     (unwind-protect
         (progn
           (make-directory org-air-r66--ws t)
           (make-directory org-air-r66--plain t)
           (write-region "" nil
                         (expand-file-name "air-config.toml" org-air-r66--ws)
                         nil 'silent)
           (pcase-dolist (`(,name . ,content) ,specs)
             (let ((file (expand-file-name name org-air-r66--ws)))
               (make-directory (file-name-directory file) t)
               (write-region (or content "") nil file nil 'silent)))
           (let ((org-air-files (list org-air-r66--ws))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-r66--ws))
                 (org-air-view-buffer-name "*org-air-r66-no-board*")
                 (org-air-projects nil)
                 (org-air-sources nil)
                 (create-lockfiles nil)
                 (inhibit-message t))
             ,@body))
       (let ((kill-buffer-query-functions nil))
         (dolist (buf (buffer-list))
           (let ((fn (buffer-file-name buf)))
             (when (and fn (string-prefix-p org-air-r66--root fn))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r66--root t))))

(defun org-air-r66--item (file text)
  "Build a refile item for the heading whose line contains TEXT in FILE.
Tags/todo are read at the heading in FILE's own buffer (so the file's
`#+TODO:' vocabulary applies), the marker freshly positioned."
  (with-current-buffer (find-file-noselect file)
    (org-with-wide-buffer
     (goto-char (point-min))
     (re-search-forward (regexp-quote text))
     (org-back-to-heading t)
     (org-air-item-create
      :title (substring-no-properties (org-get-heading t t t t))
      :tags (org-get-tags nil t)
      :todo (org-get-todo-state)
      :file file
      :marker (point-marker)))))

(defun org-air-r66--text (file)
  "Return FILE's on-disk content as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun org-air-r66--count (needle text)
  "Count the non-overlapping literal occurrences of NEEDLE in TEXT."
  (let ((n 0) (start 0) (re (regexp-quote needle)))
    (while (string-match re text start)
      (setq start (match-end 0))
      (cl-incf n))
    n))

(defun org-air-r66--doc (file)
  "Read FILE back the airctl way: (NAME STATE TAGS) via `--read-doc'."
  (let ((doc (org-air-project--read-doc file org-air-r66--ws)))
    (list (org-air-doc-name doc) (org-air-doc-state doc)
          (org-air-doc-tags doc))))

;;;; -------------------------------------------------------------------
;;;; r66-1 — the repro: a fresh Air-tree target gains frontmatter (T1)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r66-1-repro-new-file-gains-frontmatter ()
  "Refiling `* TODO [#A] Set up syncthing :#Nix:' to a brand-new file
in the Air workspace (heading nil — the R53 P3 branch) writes a file
whose on-disk bytes BEGIN exactly with the spec's four-line block
\(`#+title: Set up syncthing' / `#+state: draft' /
`#+FILETAGS: :#Nix:' / blank), then the moved subtree; and the
airctl-style presence check passes: `org-air-project--read-doc' yields
name \"Set up syncthing\", state \"draft\" and tags (\"#Nix\") — NOT
\"unknown\".  RED pre-R66: the file holds only the subtree and
classifies Unknown (the reported symptom, mechanised)."
  (skip-unless (locate-library "org-air"))
  (org-air-r66--with-tree `(("inbox.org" . ,org-air-r66--repro-inbox))
    (let ((target (expand-file-name "Syncthing.org" org-air-r66--ws)))
      (org-air-refile-item
       (org-air-r66--item org-air-inbox-file "Set up syncthing") target)
      (let ((text (org-air-r66--text target)))
        ;; the exact four-line block, at the very top.
        (should (string-prefix-p
                 "#+title: Set up syncthing\n#+state: draft\n#+FILETAGS: :#Nix:\n\n"
                 text))
        ;; then the moved subtree (re-leveled top-level, R64).
        (should (string-match-p "^\\* TODO \\[#A\\] Set up syncthing" text))
        (should (= 1 (org-air-r66--count "#+title:" text))))
      ;; the airctl round-trip: never "unknown" again.
      (should (equal (org-air-r66--doc target)
                     '("Set up syncthing" "draft" ("#Nix"))))
      ;; the item left the inbox.
      (should-not (string-match-p "syncthing"
                                  (org-air-r66--text org-air-inbox-file))))))

;;;; -------------------------------------------------------------------
;;;; r66-2 — an already-titled target is byte-identically untouched (T2)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r66-2-titled-target-untouched ()
  "A target pre-seeded with `#+title: projects' / `#+state: ready' keeps
its frontmatter region BYTE-IDENTICAL through a refile: exactly ONE
`#+title:' in the file afterwards, state still \"ready\" — the
synthesis is a strict no-op on an already-titled target (the
duplicate/clobber anti-behaviour this seam exists to forbid)."
  (skip-unless (locate-library "org-air"))
  (org-air-r66--with-tree
      `(("inbox.org" . ,org-air-r66--repro-inbox)
        ("projects.org" . "#+title: projects\n#+state: ready\n"))
    (let* ((target (expand-file-name "projects.org" org-air-r66--ws))
           (front "#+title: projects\n#+state: ready\n"))
      (org-air-refile-item
       (org-air-r66--item org-air-inbox-file "Set up syncthing") target)
      (let ((text (org-air-r66--text target)))
        (should (string-prefix-p front text))
        (should (= 1 (org-air-r66--count "#+title:" text)))
        (should (= 1 (org-air-r66--count "#+state:" text)))
        (should (string-match-p "^\\* TODO \\[#A\\] Set up syncthing" text)))
      (should (equal (seq-take (org-air-r66--doc target) 2)
                     '("projects" "ready"))))))

;;;; -------------------------------------------------------------------
;;;; r66-3 — the derivation strips all four species, R57-lawfully (T3)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r66-3-title-strip-species-and-r57-vocab ()
  "The derived `#+title:' strips, via org's OWN parsers: the TODO
keyword per the SOURCE buffer's merged vocabulary (a file-local
`#+TODO: WAIT | ARCHIVED' makes WAIT strip), the `[#B]' priority
cookie, the trailing tag list, trailing AND inline statistics cookies
\(single-spaced result) and the COMMENT keyword.  Anti-tautology: in a
source WITHOUT the file-local vocab, WAIT is NOT a keyword and
survives into the title.  RED against a default-vocab-only strip or
any cookie-less derivation."
  (skip-unless (locate-library "org-air"))
  (org-air-r66--with-tree
      '(("inbox.org" . "#+title: org-air inbox\n#+TODO: WAIT | ARCHIVED\n\n* WAIT [#B] Plan the rollout [1/3] :a:b:\n* Progress [33%] on thing\n")
        ("bare.org" . "#+title: bare notes\n\n* WAIT Plan the rollout\n* TODO COMMENT Secret thing\n"))
    (let ((t1 (expand-file-name "rollout.org" org-air-r66--ws))
          (t2 (expand-file-name "progress.org" org-air-r66--ws))
          (t3 (expand-file-name "secret.org" org-air-r66--ws))
          (t4 (expand-file-name "wait.org" org-air-r66--ws))
          (bare (expand-file-name "bare.org" org-air-r66--ws)))
      ;; keyword (file-local WAIT) + priority + tags + trailing cookie.
      (org-air-refile-item
       (org-air-r66--item org-air-inbox-file "Plan the rollout") t1)
      (should (string-prefix-p "#+title: Plan the rollout\n"
                               (org-air-r66--text t1)))
      ;; inline cookie, single-spaced.
      (org-air-refile-item
       (org-air-r66--item org-air-inbox-file "Progress") t2)
      (should (string-prefix-p "#+title: Progress on thing\n"
                               (org-air-r66--text t2)))
      ;; COMMENT keyword (default vocab — bare.org: the WAIT file's
      ;; local `#+TODO:' REPLACES the default set there, so its TODO
      ;; would not even be a keyword; org semantics, not org-air's).
      (org-air-refile-item
       (org-air-r66--item bare "Secret thing") t3)
      (should (string-prefix-p "#+title: Secret thing\n"
                               (org-air-r66--text t3)))
      ;; anti-tautology: WAIT is NOT a keyword without the file-local
      ;; vocab — it survives into the title (the R57 supplement never
      ;; rebinds the user's globals).
      (org-air-refile-item
       (org-air-r66--item bare "Plan the rollout") t4)
      (should (string-prefix-p "#+title: WAIT Plan the rollout\n"
                               (org-air-r66--text t4))))))

;;;; -------------------------------------------------------------------
;;;; r66-4 — the state defcustom + the file-name-base fallback (T4)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r66-4-state-defcustom-and-title-fallback ()
  "With `org-air-refile-new-file-state' let-bound to \"ready\" the
synthesised block carries `#+state: ready'; the degenerate heading
`* TODO [1/2]' (whose derivation is empty) falls back to the target's
`file-name-base' — `#+title: syncthing' for syncthing.org, the same
fallback `org-air-project--read-doc' uses, so board and file agree."
  (skip-unless (locate-library "org-air"))
  (org-air-r66--with-tree
      '(("inbox.org" . "#+title: org-air inbox\n\n* TODO First thing\n* TODO [1/2]\n"))
    (let ((t1 (expand-file-name "committed.org" org-air-r66--ws))
          (t2 (expand-file-name "syncthing.org" org-air-r66--ws)))
      (let ((org-air-refile-new-file-state "ready"))
        (org-air-refile-item
         (org-air-r66--item org-air-inbox-file "First thing") t1))
      (should (string-prefix-p "#+title: First thing\n#+state: ready\n"
                               (org-air-r66--text t1)))
      ;; degenerate: the cookie-only heading falls back to the file name.
      (org-air-refile-item
       (org-air-r66--item org-air-inbox-file "[1/2]") t2)
      (should (string-prefix-p "#+title: syncthing\n#+state: draft\n"
                               (org-air-r66--text t2))))))

;;;; -------------------------------------------------------------------
;;;; r66-5 — the gate, all three values + the configured-roots leg (T5)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r66-5-air-tree-gate-three-values ()
  "The scope rule, each leg independently: under the DEFAULT t a new
target OUTSIDE any Air tree stays BARE (subtree only, no `#+title:' —
the 5000-file notes silo is untouched); the same shape with `always'
synthesises; a target INSIDE the Air tree with nil stays bare; and an
explicit `org-air-projects' root carrying NEITHER marker still gates
ON under t (the configured-roots leg of the detector)."
  (skip-unless (locate-library "org-air"))
  (org-air-r66--with-tree `(("inbox.org" . ,org-air-r66--repro-inbox))
    ;; leg 1: default t, non-Air target ⇒ bare (today's behaviour).
    (let ((target (expand-file-name "note-a.org" org-air-r66--plain)))
      (org-air-refile-item
       (org-air-r66--item org-air-inbox-file "Set up syncthing") target)
      (let ((text (org-air-r66--text target)))
        (should-not (string-match-p "#\\+title:" text))
        (should-not (string-match-p "#\\+state:" text))
        (should (string-match-p "^\\* TODO \\[#A\\] Set up syncthing" text))))
    ;; re-seed the inbox for each following leg.
    (dolist (leg '((always . t) (nil-gate . nil) (roots . t)))
      (write-region org-air-r66--repro-inbox nil org-air-inbox-file
                    nil 'silent)
      (when-let* ((buf (find-buffer-visiting org-air-inbox-file)))
        (with-current-buffer buf (revert-buffer t t t)))
      (pcase (car leg)
        ;; leg 2: 'always synthesises even outside Air.
        ('always
         (let ((target (expand-file-name "note-b.org" org-air-r66--plain))
               (org-air-refile-synthesize-frontmatter 'always))
           (org-air-refile-item
            (org-air-r66--item org-air-inbox-file "Set up syncthing") target)
           (should (string-prefix-p "#+title: Set up syncthing\n"
                                    (org-air-r66--text target)))))
        ;; leg 3: nil never synthesises, even inside the Air tree.
        ('nil-gate
         (let ((target (expand-file-name "note-c.org" org-air-r66--ws))
               (org-air-refile-synthesize-frontmatter nil))
           (org-air-refile-item
            (org-air-r66--item org-air-inbox-file "Set up syncthing") target)
           (should-not (string-match-p "#\\+title:"
                                       (org-air-r66--text target)))))
        ;; leg 4: an explicit markerless `org-air-projects' root gates ON.
        ('roots
         (let ((target (expand-file-name "note-d.org" org-air-r66--plain))
               (org-air-projects (list org-air-r66--plain)))
           (org-air-refile-item
            (org-air-r66--item org-air-inbox-file "Set up syncthing") target)
           (should (string-prefix-p "#+title: Set up syncthing\n"
                                    (org-air-r66--text target)))))))))

;;;; -------------------------------------------------------------------
;;;; r66-6 — frontmatter-less EXISTING file + drawer + idempotence (T6)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r66-6-existing-frontmatterless-file ()
  "An EXISTING file with prose + headings but no `#+title:' gets the
block inserted at its top with the original content byte-preserved
below and the item appended; a file BEGINNING with a file-level
`:PROPERTIES:' drawer keeps the drawer FIRST (org requires it) with
the block after; and a SECOND refile into the same file leaves exactly
ONE `#+title:' (buffer-level idempotence)."
  (skip-unless (locate-library "org-air"))
  (org-air-r66--with-tree
      '(("inbox.org" . "#+title: org-air inbox\n\n* TODO One :x:\n* TODO Two :x:\n* TODO Three :x:\n")
        ("notes.org" . "Some prose first.\n\n* Old heading\nBody.\n")
        ("iddoc.org" . ":PROPERTIES:\n:ID: r66-fixed-id\n:END:\nDrawer prose.\n"))
    (let ((notes (expand-file-name "notes.org" org-air-r66--ws))
          (iddoc (expand-file-name "iddoc.org" org-air-r66--ws))
          (original "Some prose first.\n\n* Old heading\nBody.\n"))
      ;; block at top, original content byte-preserved, item appended.
      (org-air-refile-item
       (org-air-r66--item org-air-inbox-file "One") notes)
      (let ((text (org-air-r66--text notes)))
        (should (string-prefix-p "#+title: One\n#+state: draft\n#+FILETAGS: :x:\n\n"
                                 text))
        (should (string-match-p (concat "\n" (regexp-quote original)) text))
        (should (string-match-p "^\\* TODO One :x:" text)))
      ;; the file-level property drawer stays FIRST; block lands after.
      (org-air-refile-item
       (org-air-r66--item org-air-inbox-file "Two") iddoc)
      (let ((text (org-air-r66--text iddoc)))
        (should (string-prefix-p ":PROPERTIES:\n:ID: r66-fixed-id\n:END:\n"
                                 text))
        (should (< (string-match ":END:" text)
                   (string-match "#\\+title: Two" text)))
        (should (string-match-p "^\\* TODO Two :x:" text)))
      ;; a SECOND refile into the same file: still exactly one block.
      (org-air-refile-item
       (org-air-r66--item org-air-inbox-file "Three") notes)
      (let ((text (org-air-r66--text notes)))
        (should (= 1 (org-air-r66--count "#+title:" text)))
        (should (= 1 (org-air-r66--count "#+state:" text)))
        (should (string-match-p "^\\* TODO Three :x:" text))))))

;;;; -------------------------------------------------------------------
;;;; r66-7 — a failed refile to a NEW file creates NO file at all (T7)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r66-7-new-file-failure-creates-no-file ()
  "The R64 transactional composition, extended to disk-atomic file
creation: a metadata step (`org-set-tags') forced to signal AFTER the
cut leaves NO target file on disk (`file-exists-p' nil — no
half-written, no title-only file), the source byte-identical (the R64
restore) and the created directory empty + inert (the enumeration
yields no row for it).  The RETRY without the stub then succeeds with
exactly ONE frontmatter block — the unsaved in-buffer residue is found
by the buffer-level idempotence check and never written twice.  RED
against a synthesis that saves eagerly in step 0."
  (skip-unless (locate-library "org-air"))
  (org-air-r66--with-tree `(("inbox.org" . ,org-air-r66--repro-inbox))
    (let* ((newdir (expand-file-name "fresh" org-air-r66--ws))
           (target (expand-file-name "Syncthing.org" newdir))
           (inbox-before (org-air-r66--text org-air-inbox-file)))
      (cl-letf (((symbol-function 'org-set-tags)
                 (lambda (&rest _)
                   (error "r66-7: simulated metadata failure"))))
        (should-error
         (org-air-refile-item
          (org-air-r66--item org-air-inbox-file "Set up syncthing")
          target nil '("keep"))
         :type 'error))
      ;; NO file came into existence — not even a title-only one.
      (should-not (file-exists-p target))
      ;; the source is byte-identical (R64 restore).
      (should (equal (org-air-r66--text org-air-inbox-file) inbox-before))
      ;; the directory residue is empty + inert: no file enumerates.
      (should (file-directory-p newdir))
      (should-not (directory-files newdir nil "\\.org\\'"))
      (should-not (member target (org-air-query-files)))
      ;; the retry (fault removed) lands ONE block + the item, once.
      (org-air-refile-item
       (org-air-r66--item org-air-inbox-file "Set up syncthing")
       target nil '("keep"))
      (let ((text (org-air-r66--text target)))
        (should (string-prefix-p
                 "#+title: Set up syncthing\n#+state: draft\n#+FILETAGS: :keep:\n\n"
                 text))
        (should (= 1 (org-air-r66--count "#+title:" text)))
        (should (= 1 (org-air-r66--count "Set up syncthing"
                                         (substring text (length "#+title: Set up syncthing\n")))))))))

;;;; -------------------------------------------------------------------
;;;; r66-8 — the directory fold-in + prompt-time purity + preview (T8)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r66-8-directory-creation-and-prompt-purity ()
  "The absorbed v0.2 refile-create-target-directory: a target TWO
missing directories deep under the Air root succeeds — directories
created at EXECUTE, frontmatter present, the `--read-doc' check passes.
The prompt-time readers stay pure (the R64-2 pin extended to mkdir):
the `f' reader's `⌂ other file…' escape naming the nested path and the
pure `p' resolver over a fixture table leave the directory
NON-existent.  And the R66-3 transient nicety: `--form-creates'
annotates `new file' (plus the missing segments) for a not-yet-existing
`:file', and stays nil when everything exists."
  (skip-unless (locate-library "org-air"))
  (org-air-r66--with-tree `(("inbox.org" . ,org-air-r66--repro-inbox))
    (let* ((deep (expand-file-name "new-dir/sub" org-air-r66--ws))
           (target (expand-file-name "new.org" deep))
           (item (org-air-r66--item org-air-inbox-file "Set up syncthing")))
      ;; prompt time: the `f' reader naming the nested path mutates
      ;; NOTHING — no directory, no file.
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "⌂ other file…"))
                ((symbol-function 'read-file-name)
                 (lambda (&rest _) target)))
        (should (equal (org-air-inbox--read-target-file item) target)))
      ;; the pure `p' resolver over a fixture table: still nothing.
      (should (equal (org-air-inbox--resolve-path "Infra/Cloud" '("X"))
                     '(:olp ("Infra" "Cloud") :new 2)))
      (should-not (file-directory-p deep))
      (should-not (file-exists-p target))
      ;; the preview annotation: `new file' + the missing segments.
      (let ((org-air-inbox--refile-form
             (list :item item :file target
                   :olp '("Infra" "Cloud") :new 2)))
        (should (equal (org-air-inbox--form-creates)
                       "  (creates: new file, Infra › Cloud)")))
      (let ((org-air-inbox--refile-form
             (list :item item :file org-air-inbox-file :olp nil :new 0)))
        (should-not (org-air-inbox--form-creates)))
      ;; execute: both directories created, frontmatter present.
      (org-air-refile-item item target '("Infra" "Cloud"))
      (should (file-directory-p deep))
      (should (string-prefix-p "#+title: Set up syncthing\n#+state: draft\n"
                               (org-air-r66--text target)))
      (should (equal (org-air-r66--doc target)
                     '("Set up syncthing" "draft" ("#Nix"))))
      ;; the OLP parents landed BELOW the block, above the item.
      (let ((text (org-air-r66--text target)))
        (should (< (string-match "#\\+title:" text)
                   (string-match "^\\* Infra" text)))
        (should (< (string-match "^\\* Infra" text)
                   ;; tag-alignment padding tolerated (org realigns).
                   (string-match "Set up syncthing.*:#Nix:$" text)))))))

;;;; -------------------------------------------------------------------
;;;; r66-9 — the FILETAGS shapes + the airctl round-trip (T9)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r66-9-filetags-shapes ()
  "The `#+FILETAGS:' source set is the moved heading's EFFECTIVE tags:
the TAGS argument when non-nil (it wins over the item's own), `:none'
⇒ NO line, else the item's own tags — always MINUS `inbox' (leaving
the inbox is what refiling is), `#'-prefixed user tags verbatim; a
tagless item also omits the line; and the moved heading's OWN tag list
is untouched by the synthesis (TAGS nil applies no `org-set-tags').
Cross-check: the synthesised file parses back through
`org-air-project--read-doc' with tags (\"#Nix\") — the airctl
round-trip."
  (skip-unless (locate-library "org-air"))
  (org-air-r66--with-tree
      '(("inbox.org" . "#+title: org-air inbox\n\n* TODO Set up syncthing :#Nix:inbox:\n* TODO Set up n8n :#Nix:inbox:\n* TODO Cleared thing :a:\n* TODO Tagless thing\n"))
    (let ((t1 (expand-file-name "nix.org" org-air-r66--ws))
          (t2 (expand-file-name "selfhosted.org" org-air-r66--ws))
          (t3 (expand-file-name "cleared.org" org-air-r66--ws))
          (t4 (expand-file-name "tagless.org" org-air-r66--ws)))
      ;; TAGS nil: the item's own tags, inbox excluded, `#' verbatim;
      ;; the heading's own tag list untouched by synthesis.
      (org-air-refile-item
       (org-air-r66--item org-air-inbox-file "Set up syncthing") t1)
      (let ((text (org-air-r66--text t1)))
        (should (string-match-p "^#\\+FILETAGS: :#Nix:$" text))
        (should (string-match-p ":#Nix:inbox:" text)))
      (should (equal (nth 2 (org-air-r66--doc t1)) '("#Nix")))
      ;; TAGS argument wins over the item's own tags.
      (org-air-refile-item
       (org-air-r66--item org-air-inbox-file "Set up n8n")
       t2 nil '("selfhosted"))
      (should (string-match-p "^#\\+FILETAGS: :selfhosted:$"
                              (org-air-r66--text t2)))
      ;; :none ⇒ no FILETAGS line at all (title + state still there).
      (org-air-refile-item
       (org-air-r66--item org-air-inbox-file "Cleared thing") t3 nil :none)
      (let ((text (org-air-r66--text t3)))
        (should (string-prefix-p "#+title: Cleared thing\n#+state: draft\n\n"
                                 text))
        (should-not (string-match-p "FILETAGS" text)))
      ;; a tagless item ⇒ no line either.
      (org-air-refile-item
       (org-air-r66--item org-air-inbox-file "Tagless thing") t4)
      (let ((text (org-air-r66--text t4)))
        (should (string-prefix-p "#+title: Tagless thing\n#+state: draft\n\n"
                                 text))
        (should-not (string-match-p "FILETAGS" text))))))

;;;; -------------------------------------------------------------------
;;;; r66-10 — step 0 BEFORE resolve: the pos-1 marker hazard, pinned
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r66-10-synthesis-before-resolve-pos1-parent ()
  "The engine ORDER is observable and pinned: frontmatter synthesis
runs BEFORE `--resolve-target' (spec Decision 2 — the pos-1 marker
hazard).  In a frontmatter-less existing file whose FIRST heading (at
position 1) is the refile parent, a resolve-first implementation
strands the parent (MARKER . LEVEL) marker on the inserted `#+title:'
line (a marker does not advance past an insertion AT its own
position); on Org 9.7 the paste then degrades via
`org-back-to-heading-or-point-min' to END OF BUFFER and the item is
MISFILED under the LAST top-level heading instead of the named parent
\(measured: it lands under `* Zother').  Pinned by bytes: the block on
top exactly once, the item INSIDE `* Infra' — after it, BEFORE its
`* Zother' sibling — re-leveled to `**'.  RED under a resolve-first
mutant; RED pre-R66 (no block at all)."
  (skip-unless (locate-library "org-air"))
  (org-air-r66--with-tree
      `(("inbox.org" . ,org-air-r66--repro-inbox)
        ("hazard.org" . "* Infra\nBody.\n* Zother\nOther body.\n"))
    (let ((target (expand-file-name "hazard.org" org-air-r66--ws)))
      (org-air-refile-item
       (org-air-r66--item org-air-inbox-file "Set up syncthing")
       target "Infra")
      (let ((text (org-air-r66--text target)))
        ;; the block on top, exactly once.
        (should (string-prefix-p
                 "#+title: Set up syncthing\n#+state: draft\n#+FILETAGS: :#Nix:\n\n"
                 text))
        (should (= 1 (org-air-r66--count "#+title:" text)))
        ;; the item INSIDE the pos-1 parent's subtree: after `* Infra',
        ;; BEFORE the `* Zother' sibling, re-leveled to `**'.
        (let ((infra (string-match "^\\* Infra$" text))
              (item (string-match
                     "^\\*\\* TODO \\[#A\\] Set up syncthing" text))
              (zother (string-match "^\\* Zother$" text)))
          (should infra)
          (should item)
          (should zother)
          (should (< infra item))
          (should (< item zother)))))))

(provide 'org-air-round66-test)
;;; org-air-round66-test.el ends here
