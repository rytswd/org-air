;;; org-air-round65-test.el --- executing ERTs for v0.5 round-65 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance ERTs for v0.5 round-65 (air/v0.5/org-air-round65-design.org):
;; the view pane must show the item's BODY — the editable indirect pane
;; reveals the narrowed subtree PANE-LOCALLY (body + sub-headings
;; visible, `:PROPERTIES:' drawers re-folded) via the never-error
;; `org-air-view-pane--reveal' helper at BOTH narrow sites (`--indirect'
;; and the same-file `--renarrow' reuse path), the source buffer's fold
;; state is provably untouched under BOTH org-fold styles, and the
;; read-only snapshot keeps its body visible while folding the raw
;; `:PROPERTIES:' dump DISPLAY-ONLY (bytes unchanged — every pane golden
;; stays byte-identical).  All BATCH/headless: `invisible-p' and
;; `org-fold-folded-p' consult buffer state (text properties + alias
;; alist + overlays + `buffer-invisibility-spec'), no redisplay needed;
;; the editable-path functions are plain functions called directly (the
;; R19/R28 idiom — `--render' itself gates the editable pane on
;; `noninteractive').
;;
;; The base fold recipe makes BOTH halves of the fix load-bearing:
;; `org-fold-show-all' then `org-fold-hide-sublevels 1' — the outline is
;; FOLDED (so dropping the reveal turns the body-visible asserts RED,
;; the measured S1 repro) while the drawer specs are OPEN underneath (so
;; dropping the `org-fold-hide-drawer-all' step turns the drawer-folded
;; asserts RED, the S4 shape).
;;
;;   r65-1 (T1) pane reveals body, drawer stays folded — default style
;;         (overlays on the gate's Emacs 30): real
;;         `org-air-view-pane--indirect' on the folded base ⇒ body
;;         paragraph, `** Child' heading AND its body all visible in the
;;         pane; the `:PROPERTIES:' interior `invisible-p' AND a real
;;         drawer fold (`org-fold-folded-p … 'drawer').
;;   r65-2 (T2) the reveal never leaks to the SOURCE: after the exact
;;         T1 ops the base's fold state is byte-for-byte as captured
;;         BEFORE the pane opened, the OPEN drawer is STILL open (no
;;         reverse leak from the pane's drawer re-fold), and
;;         `buffer-modified-p' stays nil.  The naive reveal-in-the-base
;;         alternative (probed: leaks) fails this immediately — the RED
;;         anchor; thereafter it is the pinned no-leak guard.
;;   r65-3 (T3) T1+T2 under `org-fold-core-style' 'text-properties (the
;;         stock Emacs 29 / Org 9.6 user), the style let wrapped around
;;         the ENTIRE scenario — base creation through assertion.
;;   r65-4 (T4) the `--renarrow' same-file reuse path re-reveals: pane
;;         on Parent, real `--renarrow' to Other ⇒ Other's body visible,
;;         Other's drawer folded, base untouched.  Dropping the reveal
;;         in `--renarrow' ONLY leaves T1 green and turns THIS red —
;;         both call sites are load-bearing.
;;   r65-5 (T5) snapshot: batch `org-air-view-pane--render' (falls to
;;         the snapshot) ⇒ body text present and NOT invisible (pins
;;         today's behavior), drawer interior IS invisible via the
;;         `org-air-pane-drawer' spec entry, and the pane's
;;         `buffer-substring-no-properties' equals the raw subtree
;;         bytes — the in-test byte capture (golden discipline in
;;         miniature; the R16 `entry-view-pane.txt' golden ERT stays
;;         green untouched).
;;   r65-6 (T6) heading-less `#+title:' file-item (pos-nil ⇒ the WIDE
;;         branch): prose visible, a base-folded example BLOCK revealed
;;         (`org-fold-show-all' is load-bearing), the file-level drawer
;;         re-folded pane-side while the base's stays OPEN, base
;;         untouched, no error signaled.
;;   r65-7 (T7) `v' resolves to the ONE `org-air-view-pane' command on
;;         all four host maps (board / project / review / revisit) via
;;         the SHARED `org-air-view-core-map' binding — no host forks
;;         the binding above the fixed `--render' funnel.
;;   r65-8 (T8, audit hardening) editable-path edge shapes from the
;;         spec's edge list: EMPTY subtree (heading only — reveal +
;;         drawer re-fold are no-ops, nothing signals), DRAWER-ONLY
;;         entry (drawer folds pane-side, base's stays open), and
;;         NESTED drawers (`:PROPERTIES:' + `:LOGBOOK:' + the child's
;;         own drawer ALL fold pane-side while both bodies stay
;;         visible) — base folds/modified-flag untouched throughout.
;;   r65-9 (T9, audit hardening) snapshot edge shapes: an UNTERMINATED
;;         `:PROPERTIES:' (no `:END:' anywhere) is SKIPPED — body
;;         visible, ZERO `invisible' properties, bytes = raw subtree,
;;         nothing signals; and with parent + child drawers BOTH
;;         properly terminated, both fold display-only while both
;;         bodies stay visible and the bytes stay raw.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'org-fold)

(when (locate-library "org-air")
  (require 'org-air)
  (require 'org-air-project)
  (require 'org-air-review)
  (require 'org-air-revisit))

;;;; -------------------------------------------------------------------
;;;; Shared fixture + helpers
;;;; -------------------------------------------------------------------

(defconst org-air-r65--fixture
  (concat "* TODO Parent :p:\n"
          ":PROPERTIES:\n"
          ":CREATED: [2026-07-01 Wed]\n"
          ":END:\n"
          "Parent body paragraph.\n"
          "** Child\n"
          "Child body line.\n"
          "* Other :o:\n"
          ":PROPERTIES:\n"
          ":KIND: other\n"
          ":END:\n"
          "Other body paragraph.\n")
  "The spec's shared fixture: Parent (drawer+body+child), Other (drawer+body).")

(defconst org-air-r65--headless-fixture
  (concat ":PROPERTIES:\n"
          ":ID: r65-headless\n"
          ":END:\n"
          "#+title: Headless note\n"
          "\n"
          "Prose paragraph.\n"
          "\n"
          "#+begin_example\n"
          "block body line\n"
          "#+end_example\n")
  "The R53 heading-less file-item shape, with a foldable example block.")

(defmacro org-air-r65--with-file (content &rest body)
  "Write CONTENT to a fresh temp Org file, visit it, and run BODY.
Binds `dir', `file' and `base' (the visiting buffer).  Kills the base
\(auto-killing any indirect pane on it) and deletes the directory
afterwards."
  (declare (indent 1) (debug t))
  `(let* ((dir (make-temp-file "org-air-r65-" t))
          (file (expand-file-name "note.org" dir))
          (base nil))
     (ignore file)
     (unwind-protect
         (progn
           (let ((coding-system-for-write 'utf-8-unix))
             (write-region ,content nil file nil 'silent))
           (setq base (find-file-noselect file))
           ,@body)
       (when (buffer-live-p base)
         (with-current-buffer base (set-buffer-modified-p nil))
         (kill-buffer base))
       (delete-directory dir t))))

(defun org-air-r65--pos (buf regexp)
  "Return the (widened) position of REGEXP's match start in BUF."
  (with-current-buffer buf
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (re-search-forward regexp)
        (match-beginning 0)))))

(defun org-air-r65--fold-recipe (buf)
  "Apply the RED-able base fold recipe to BUF.
`org-fold-show-all' then `org-fold-hide-sublevels' 1: the outline is
FOLDED (the reveal is load-bearing) with every drawer spec OPEN
underneath (the drawer re-fold is load-bearing)."
  (with-current-buffer buf
    (org-fold-show-all)
    (org-fold-hide-sublevels 1)))

(defun org-air-r65--vis (buf positions)
  "Return the normalized `invisible-p' booleans at POSITIONS in BUF."
  (with-current-buffer buf
    (mapcar (lambda (p) (not (not (invisible-p p)))) positions)))

(defun org-air-r65--reveal-scenario (assert-pane assert-base)
  "Run the shared T1/T2 scenario over the folded fixture base.
Opens the REAL `org-air-view-pane--indirect' on Parent.  With
ASSERT-PANE non-nil, assert the pane half (body + child visible, drawer
folded); with ASSERT-BASE non-nil, assert the no-leak half (base fold
state byte-for-byte as captured before the pane opened, the open drawer
still open, `buffer-modified-p' nil)."
  (org-air-r65--with-file org-air-r65--fixture
    (org-air-r65--fold-recipe base)
    (let* ((parent-pos (org-air-r65--pos base "^\\* TODO Parent"))
           (body-pos (org-air-r65--pos base "Parent body"))
           (child-pos (org-air-r65--pos base "^\\*\\* Child"))
           (child-body-pos (org-air-r65--pos base "Child body"))
           (drawer-pos (org-air-r65--pos base ":CREATED:"))
           (other-body-pos (org-air-r65--pos base "Other body"))
           (probes (list body-pos child-pos child-body-pos
                         drawer-pos other-body-pos))
           (before (org-air-r65--vis base probes))
           (ind nil))
      ;; Anti-tautology: the recipe really folded the base (the S1 repro
      ;; shape — the reveal is load-bearing) with the drawer spec OPEN
      ;; underneath (the S4 shape — the re-fold is load-bearing).
      (should (equal before '(t t t t t)))
      (with-current-buffer base
        (should-not (org-fold-folded-p drawer-pos 'drawer)))
      (unwind-protect
          (progn
            (setq ind (org-air-view-pane--indirect base parent-pos "Parent"))
            (when assert-pane
              (with-current-buffer ind
                ;; body + sub-heading + its body VISIBLE,
                (should-not (invisible-p body-pos))
                (should-not (invisible-p child-pos))
                (should-not (invisible-p child-body-pos))
                ;; :PROPERTIES: interior FOLDED — via the editable path
                ;; this is a REAL drawer fold, not display sugar.
                (should (invisible-p drawer-pos))
                (should (org-fold-folded-p drawer-pos 'drawer))))
            (when assert-base
              (with-current-buffer base
                ;; base fold state byte-for-byte as captured BEFORE,
                (should (equal (org-air-r65--vis base probes) before))
                ;; the OPEN drawer is STILL open (no reverse leak),
                (should-not (org-fold-folded-p drawer-pos 'drawer))
                ;; and org-fold never dirtied the user's file buffer.
                (should-not (buffer-modified-p)))))
        (when (buffer-live-p ind) (kill-buffer ind))))))

;;;; -------------------------------------------------------------------
;;;; T1/T2 — pane reveal + no source leak (default style)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r65-1-pane-reveals-body-drawer-stays-folded ()
  "The editable pane REVEALS the item's body; `:PROPERTIES:' stays folded.
Real `org-air-view-pane--indirect' on an outline-folded base (default
org-fold style — overlays on the gate's Emacs 30): the body paragraph,
the `** Child' sub-heading AND its body are all visible in the pane,
while the drawer interior is `invisible-p' and a genuine drawer fold
\(`org-fold-folded-p' … \\='drawer).  Dropping the `--reveal' call turns
the body asserts RED (the measured S1 repro); dropping its
`org-fold-hide-drawer-all' step turns the drawer asserts RED (the base
recipe leaves the drawer spec OPEN underneath)."
  (skip-unless (locate-library "org-air"))
  (org-air-r65--reveal-scenario t nil))

(ert-deftest org-air-r65-2-pane-reveal-never-leaks-to-source ()
  "The in-pane reveal NEVER mutates the source buffer's fold state.
After the exact T1 ops the BASE's `invisible-p' probes are byte-for-byte
as captured before the pane opened, the OPEN drawer is STILL open (no
reverse leak from the pane's drawer re-fold), and `buffer-modified-p'
stays nil.  The naive reveal-in-the-BASE alternative measurably leaks
\(the base body becomes visible) and fails this immediately — the RED
anchor; thereafter this is the pinned no-leak guard under the gate's
default (overlays) style."
  (skip-unless (locate-library "org-air"))
  (org-air-r65--reveal-scenario nil t))

;;;; -------------------------------------------------------------------
;;;; T3 — the same, pinned under text-properties style
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r65-3-pane-reveal-text-properties-style ()
  "T1+T2 under `org-fold-core-style' \\='text-properties (Emacs 29/Org 9.6).
The style let wraps the ENTIRE scenario — base creation through
assertion (the style variable is read at both fold-write and fold-read
time) — pinning the stock Emacs 29 / Org 9.6 user: folds are text
properties on the SHARED text, so this is exactly the configuration
where a leak looks plausible; the clone-time org-fold decouple keeps the
reveal pane-local anyway."
  (skip-unless (locate-library "org-air"))
  (skip-unless (boundp 'org-fold-core-style))
  (let ((org-fold-core-style 'text-properties))
    (org-air-r65--reveal-scenario t t)))

;;;; -------------------------------------------------------------------
;;;; T4 — the --renarrow reuse path re-reveals (the second call site)
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r65-4-renarrow-rereveals ()
  "The same-file `--renarrow' reuse path re-reveals the NEW item.
One pane on Parent, then the real `org-air-view-pane--renarrow' to
Other (non-nil return — the reuse succeeded): Other's body is visible,
Other's drawer folded, the base untouched (S6).  Dropping the `--reveal'
call in `--renarrow' ONLY leaves T1 green and turns THIS red — both
call sites are proven load-bearing separately."
  (skip-unless (locate-library "org-air"))
  (org-air-r65--with-file org-air-r65--fixture
    (org-air-r65--fold-recipe base)
    (let* ((parent-pos (org-air-r65--pos base "^\\* TODO Parent"))
           (other-pos (org-air-r65--pos base "^\\* Other"))
           (other-body-pos (org-air-r65--pos base "Other body"))
           (other-drawer-pos (org-air-r65--pos base ":KIND:"))
           (ind nil))
      (unwind-protect
          (progn
            (setq ind (org-air-view-pane--indirect base parent-pos "Parent"))
            ;; the REAL renarrow to the second heading (same-file reuse).
            (let ((ctx (list :file file :title "Other" :state "TODO")))
              (should (eq ind (org-air-view-pane--renarrow
                               ind (cons base other-pos) ctx))))
            (with-current-buffer ind
              ;; narrowed to Other now,
              (should (string-prefix-p "* Other" (buffer-string)))
              ;; body visible, drawer folded — the re-reveal ran here.
              (should-not (invisible-p other-body-pos))
              (should (invisible-p other-drawer-pos))
              (should (org-fold-folded-p other-drawer-pos 'drawer)))
            (with-current-buffer base
              ;; base untouched: Other still outline-folded, its drawer
              ;; spec still OPEN, buffer unmodified.
              (should (invisible-p other-body-pos))
              (should-not (org-fold-folded-p other-drawer-pos 'drawer))
              (should-not (buffer-modified-p))))
        (when (buffer-live-p ind) (kill-buffer ind))))))

;;;; -------------------------------------------------------------------
;;;; T5 — snapshot: body pinned visible, drawer display-only folded
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r65-5-snapshot-body-visible-drawer-display-folded ()
  "The read-only snapshot shows the body; `:PROPERTIES:' folds DISPLAY-ONLY.
Batch `org-air-view-pane--render' on the folded base falls to the
snapshot: the pane contains the body text NOT `invisible-p' (pins
today's behavior), the drawer interior IS invisible via the
`org-air-pane-drawer' invisibility-spec entry, and the pane's
`buffer-substring-no-properties' equals the raw subtree bytes — the
in-test byte capture (the golden discipline in miniature; the R16
`entry-view-pane.txt' golden ERT must stay green untouched).  Dropping
the R65-2 property walk turns the drawer asserts RED."
  (skip-unless (locate-library "org-air"))
  (org-air-r65--with-file org-air-r65--fixture
    (org-air-r65--fold-recipe base)
    (let* ((parent-pos (org-air-r65--pos base "^\\* TODO Parent"))
           ;; the pre-R65 snapshot bytes: the raw subtree text.
           (expected (with-current-buffer base
                       (save-excursion
                         (save-restriction
                           (widen)
                           (goto-char parent-pos)
                           (buffer-substring-no-properties
                            (point)
                            (progn (org-end-of-subtree t t) (point)))))))
           (mk (with-current-buffer base (copy-marker parent-pos)))
           (ctx (list :marker mk :file file :title "Parent" :state "TODO"))
           (buf (let ((noninteractive t))   ; the snapshot branch, always
                  (org-air-view-pane--render ctx))))
      (unwind-protect
          (with-current-buffer buf
            ;; BYTES unchanged by the display-only fold (byte-identity).
            (should (equal (buffer-substring-no-properties
                            (point-min) (point-max))
                           expected))
            (let ((bpos (org-air-r65--pos buf "Parent body"))
                  (cpos (org-air-r65--pos buf "Child body"))
                  (dpos (org-air-r65--pos buf ":CREATED:")))
              ;; the body (both prompt branches) is VISIBLE,
              (should-not (invisible-p bpos))
              (should-not (invisible-p cpos))
              ;; the drawer interior display-folded via the spec entry.
              (should (invisible-p dpos))
              (should (eq (get-text-property dpos 'invisible)
                          'org-air-pane-drawer))
              (should (member '(org-air-pane-drawer . t)
                              buffer-invisibility-spec))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

;;;; -------------------------------------------------------------------
;;;; T6 — heading-less file-item: the WIDE branch reveals
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r65-6-headingless-file-item-reveals ()
  "A heading-less `#+title:' note (pos-nil ⇒ WIDE branch) reveals fully.
Base recipe: the example BLOCK folded (`org-fold-show-all' in the wide
branch is load-bearing — dropping the reveal leaves it invisible) with
the file-level drawer spec OPEN.  The pane shows the prose AND the block
body, re-folds the drawer pane-side; the base keeps its folded block,
its OPEN drawer and a clean modified flag; nothing signals (E2)."
  (skip-unless (locate-library "org-air"))
  (org-air-r65--with-file org-air-r65--headless-fixture
    (with-current-buffer base
      (org-fold-hide-block-all))
    (let* ((block-pos (org-air-r65--pos base "block body"))
           (prose-pos (org-air-r65--pos base "Prose paragraph"))
           (id-pos (org-air-r65--pos base ":ID:"))
           (ind nil))
      ;; the recipe took: block folded, file-level drawer OPEN.
      (with-current-buffer base
        (should (invisible-p block-pos))
        (should-not (org-fold-folded-p id-pos 'drawer)))
      (unwind-protect
          (progn
            ;; pos nil → the wide branch; must not signal.
            (setq ind (org-air-view-pane--indirect base nil "Headless"))
            (with-current-buffer ind
              (should-not (invisible-p prose-pos))
              (should-not (invisible-p block-pos))   ; show-all revealed it
              (should (invisible-p id-pos))          ; drawer re-folded
              (should (org-fold-folded-p id-pos 'drawer)))
            (with-current-buffer base
              (should (invisible-p block-pos))       ; base block intact
              (should-not (org-fold-folded-p id-pos 'drawer)) ; still open
              (should-not (buffer-modified-p))))
        (when (buffer-live-p ind) (kill-buffer ind))))))

;;;; -------------------------------------------------------------------
;;;; T7 — one `v' command on all four host maps
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r65-7-v-is-one-command-on-all-four-hosts ()
  "`v' resolves to the ONE `org-air-view-pane' command on every host map.
Board, project, review and revisit each inherit the binding from the
SHARED `org-air-view-core-map' (no host map carries its OWN `v'), so the
R65 fix below the `--render' funnel applies everywhere `v' opens a pane
— a future host forking the binding above the funnel turns this RED."
  (skip-unless (locate-library "org-air"))
  ;; the ONE binding lives on the shared core map,
  (should (eq (lookup-key org-air-view-core-map (kbd "v"))
              #'org-air-view-pane))
  (dolist (map (list org-air-view-mode-map
                     org-air-project-mode-map
                     org-air-review-mode-map
                     org-air-revisit-mode-map))
    ;; every host resolves it (via keymap-parent inheritance),
    (should (eq (lookup-key map (kbd "v")) #'org-air-view-pane))
    ;; and NO host forks its own `v' above the funnel.
    (let ((own (copy-keymap map)))
      (set-keymap-parent own nil)
      (should-not (lookup-key own (kbd "v"))))))

;;;; -------------------------------------------------------------------
;;;; T8 — audit hardening: editable-path edge shapes
;;;; -------------------------------------------------------------------

(defconst org-air-r65--nested-fixture
  (concat "* TODO Deep\n"
          ":PROPERTIES:\n"
          ":A: 1\n"
          ":END:\n"
          ":LOGBOOK:\n"
          "- a note\n"
          ":END:\n"
          "Deep body.\n"
          "** Kid\n"
          ":PROPERTIES:\n"
          ":B: 2\n"
          ":END:\n"
          "Kid body.\n")
  "Entry with `:PROPERTIES:' + `:LOGBOOK:' + the child's own drawer.")

(ert-deftest org-air-r65-8-editable-edge-shapes ()
  "Editable-pane edge shapes: empty subtree, drawer-only, nested drawers.
Spec edge list (\"Item with no drawer / no body / no children —
show-subtree and hide-drawer-all are no-ops; nothing signals\"): an
EMPTY subtree opens without error and leaves the base unmodified; a
DRAWER-ONLY entry folds its drawer pane-side while the base's spec
stays open; NESTED drawers (`:PROPERTIES:' + `:LOGBOOK:' + the child's
own drawer) ALL fold pane-side — org's one-call `hide-drawer-all'
idiom — while both bodies stay visible and the base's folds, open
drawer specs and modified flag are untouched (the no-leak invariant on
a new shape)."
  (skip-unless (locate-library "org-air"))
  ;; --- empty subtree: heading only, then a sibling ---------------------
  (org-air-r65--with-file "* TODO Empty\n* Next\nNext body.\n"
    (org-air-r65--fold-recipe base)
    (let* ((pos (org-air-r65--pos base "^\\* TODO Empty"))
           (ind nil))
      (unwind-protect
          (progn
            ;; reveal + drawer re-fold are no-ops here; must not signal.
            (setq ind (org-air-view-pane--indirect base pos "Empty"))
            (with-current-buffer ind
              (should (string-prefix-p "* TODO Empty" (buffer-string))))
            (with-current-buffer base
              (should-not (buffer-modified-p))))
        (when (buffer-live-p ind) (kill-buffer ind)))))
  ;; --- drawer-only entry: heading + :PROPERTIES:, no body --------------
  (org-air-r65--with-file
      "* TODO Bare\n:PROPERTIES:\n:K: v\n:END:\n* Next\nBody.\n"
    (org-air-r65--fold-recipe base)
    (let* ((pos (org-air-r65--pos base "^\\* TODO Bare"))
           (dpos (org-air-r65--pos base ":K:"))
           (ind nil))
      ;; anti-tautology: the base recipe left the drawer spec OPEN.
      (with-current-buffer base
        (should-not (org-fold-folded-p dpos 'drawer)))
      (unwind-protect
          (progn
            (setq ind (org-air-view-pane--indirect base pos "Bare"))
            (with-current-buffer ind
              (should (invisible-p dpos))
              (should (org-fold-folded-p dpos 'drawer)))
            (with-current-buffer base
              ;; base drawer spec STILL open, buffer unmodified.
              (should-not (org-fold-folded-p dpos 'drawer))
              (should-not (buffer-modified-p))))
        (when (buffer-live-p ind) (kill-buffer ind)))))
  ;; --- nested drawers: PROPERTIES + LOGBOOK + child's drawer -----------
  (org-air-r65--with-file org-air-r65--nested-fixture
    (org-air-r65--fold-recipe base)
    (let* ((pos (org-air-r65--pos base "^\\* TODO Deep"))
           (a-pos (org-air-r65--pos base ":A:"))
           (log-pos (org-air-r65--pos base "- a note"))
           (b-pos (org-air-r65--pos base ":B:"))
           (body-pos (org-air-r65--pos base "Deep body"))
           (kid-body-pos (org-air-r65--pos base "Kid body"))
           (probes (list a-pos log-pos b-pos body-pos kid-body-pos))
           (before (org-air-r65--vis base probes))
           (ind nil))
      (unwind-protect
          (progn
            (setq ind (org-air-view-pane--indirect base pos "Deep"))
            (with-current-buffer ind
              ;; both bodies VISIBLE,
              (should-not (invisible-p body-pos))
              (should-not (invisible-p kid-body-pos))
              ;; ALL three drawers folded — including the nested
              ;; LOGBOOK and the child's own drawer.
              (should (invisible-p a-pos))
              (should (invisible-p log-pos))
              (should (invisible-p b-pos))
              (should (org-fold-folded-p a-pos 'drawer))
              (should (org-fold-folded-p b-pos 'drawer)))
            (with-current-buffer base
              ;; base byte-for-byte as before the pane opened.
              (should (equal (org-air-r65--vis base probes) before))
              (should-not (buffer-modified-p))))
        (when (buffer-live-p ind) (kill-buffer ind))))))

;;;; -------------------------------------------------------------------
;;;; T9 — audit hardening: snapshot edge shapes
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r65-9-snapshot-edge-shapes ()
  "Snapshot edges: unterminated drawer skipped; parent+child both fold.
Spec edge list (\"Unterminated `:PROPERTIES:' (no `:END:') in the
snapshot — no property applied for that drawer (never-error)\"): a
drawer with NO `:END:' anywhere renders with the body visible and ZERO
`invisible' text properties (the walk skipped it, nothing signaled),
bytes = the raw subtree.  And with parent + child drawers BOTH properly
terminated, both fold via the `org-air-pane-drawer' spec entry while
both bodies stay visible — bytes still raw (the byte-identity gate on a
multi-drawer shape)."
  (skip-unless (locate-library "org-air"))
  ;; --- unterminated: no :END: anywhere — the walk must skip -----------
  (org-air-r65--with-file "* TODO Broken\n:PROPERTIES:\n:K: v\nBody line.\n"
    (let* ((pos (org-air-r65--pos base "^\\* TODO Broken"))
           (expected (with-current-buffer base
                       (buffer-substring-no-properties (point-min) (point-max))))
           (mk (with-current-buffer base (copy-marker pos)))
           (ctx (list :marker mk :file file :title "Broken" :state "TODO"))
           (buf (let ((noninteractive t))
                  (org-air-view-pane--render ctx))))
      (unwind-protect
          (with-current-buffer buf
            ;; bytes untouched, body visible,
            (should (equal (buffer-substring-no-properties
                            (point-min) (point-max))
                           expected))
            (should-not (invisible-p (org-air-r65--pos buf "Body line")))
            ;; and ZERO invisible properties — the drawer was skipped.
            (should-not (next-single-property-change
                         (point-min) 'invisible)))
        (when (buffer-live-p buf) (kill-buffer buf)))))
  ;; --- parent + child drawers, both terminated: both fold -------------
  (org-air-r65--with-file
      (concat "* TODO Two\n:PROPERTIES:\n:A: 1\n:END:\nBody A.\n"
              "** Kid\n:PROPERTIES:\n:B: 2\n:END:\nBody B.\n")
    (let* ((pos (org-air-r65--pos base "^\\* TODO Two"))
           (expected (with-current-buffer base
                       (buffer-substring-no-properties (point-min) (point-max))))
           (mk (with-current-buffer base (copy-marker pos)))
           (ctx (list :marker mk :file file :title "Two" :state "TODO"))
           (buf (let ((noninteractive t))
                  (org-air-view-pane--render ctx))))
      (unwind-protect
          (with-current-buffer buf
            (should (equal (buffer-substring-no-properties
                            (point-min) (point-max))
                           expected))
            (let ((a (org-air-r65--pos buf ":A:"))
                  (b (org-air-r65--pos buf ":B:"))
                  (ba (org-air-r65--pos buf "Body A"))
                  (bb (org-air-r65--pos buf "Body B")))
              ;; BOTH drawers display-folded, BOTH bodies visible.
              (should (invisible-p a))
              (should (invisible-p b))
              (should (eq (get-text-property a 'invisible)
                          'org-air-pane-drawer))
              (should (eq (get-text-property b 'invisible)
                          'org-air-pane-drawer))
              (should-not (invisible-p ba))
              (should-not (invisible-p bb))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(provide 'org-air-round65-test)
;;; org-air-round65-test.el ends here
