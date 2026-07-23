;;; org-air-round80-test.el --- executing + audit ERTs for round-80 -*- lexical-binding: t; -*-

;;; Commentary:
;; Acceptance + audit ERTs for round-80 (air/v0.1/org-air-round80-design.org):
;; first-class OUT / OFF Air states across TWO surfaces, REUSING the existing
;; pill/badge machinery (no fork, no new svg code):
;;
;;   D1  register out/off as first-class states (org-air-project.el).  New
;;       DISTINCT standing-out faces `org-air-face-air-state-out' / `-off'
;;       (NOT `org-air-face-faded'); out/off join EVERY state list
;;       (--state-words / --state-face / --state-letters / --state-display-
;;       order / --state-sort-order / org-air-project-states / -sections /
;;       -state-badges / -state-nerd-glyphs) so a `#+state: out'/`off' doc
;;       reads first-class: its own face, section, rollup letter (O / F, NO
;;       collision), and rank (after complete, above unknown/dropped).
;;   D2  keyword badge NORMALIZED min-width (org-air-view.el).  New
;;       `org-air-keyword-badge-min-cols' (default 5 = the state cell) floors
;;       `org-air-view--svg-keyword-badge' and the `--compute-meta-widths'
;;       keyword column, so a SHORT keyword (OUT/OFF, 3 cols) renders a pill
;;       the SAME size as a 5-col DRAFT chip; WAITING (7) keeps its natural
;;       width (the floor is a no-op).  COLUMNS only, never a `:height'.
;;   D3  keyword face parity (org-air-view.el).  OUT/OFF plug into the SAME
;;       `org-air-todo-keyword-faces' alist pointing at the D1 faces, so a
;;       heading keyword OUT/OFF wears the SAME colour as the state chip
;;       (`org-air-view--todo-face' already consults the alist first).
;;
;; The spec's nine seams E1..E9 map onto r80-1..r80-9 below.  Harness: the
;; R25/R26 `--with-gui-metrics' idiom (display-graphic-p stubbed -> t; fixed
;; pill char metrics 8x16), box width read via `image-property :width', the
;; raw svg via `image-property :data'.  Exact colours / emoji / nerd glyphs
;; are GUI-confirm-only: every ERT asserts FACES / box-widths / list
;; membership, NEVER pixels, so a colour tweak never reddens tests.
;;
;; Cross-round invariants re-asserted: R53 (no rescan on the face/width work),
;; R26-2 (5-col uniform state pills — out/off join the SAME cell), R57-1 /
;; R79 (keyword face routing through `org-air-view--todo-face' — R80 only
;; ADDS to the alist, the resolver is untouched).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'org-air-test-helpers)

(when (locate-library "org-air")
  (require 'org-air)
  (require 'org-air-view)
  (require 'org-air-project))

;;;; -------------------------------------------------------------------
;;;; Harness — gui seam + fixed pill metrics + box/svg readers
;;;; -------------------------------------------------------------------

(defmacro org-air-r80--with-gui-metrics (&rest body)
  "Run BODY with a stubbed graphical frame + fixed pill char metrics (8x16)."
  (declare (indent 0) (debug t))
  `(let ((org-air-view--pill-char-w 8)
         (org-air-view--pill-char-h 16))
     (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) t)))
       (should (org-air-view--svg-available-p))
       ,@body)))

(defun org-air-r80--display-image (s)
  "Return the `display' IMAGE on string S, or nil."
  (let ((disp (get-text-property 0 'display s)))
    (and (imagep disp) disp)))

(defun org-air-r80--box-w (s)
  "Return the pixel width of string S's display image, or nil."
  (let ((img (org-air-r80--display-image s)))
    (and img (image-property img :width))))

(defun org-air-r80--svg-data (s)
  "Return the raw SVG string from string S's display image, or nil."
  (let ((img (org-air-r80--display-image s)))
    (and img (image-property img :data))))

(defconst org-air-r80--char-px 8
  "The `--with-gui-metrics' char-px (matches `org-air-view--pill-char-w').")

;;;; -------------------------------------------------------------------
;;;; Board corpus (parameterized by heading content) + meta-width helper
;;;; -------------------------------------------------------------------

(defvar org-air-r80--dir nil "The temp corpus dir of the current board fixture.")

(defmacro org-air-r80--with-board (content &rest body)
  "Scan a one-file board CONTENT into a temp corpus; bind `org-air-r80--items'.
CONTENT is the body of a single `board.org' (a `#+TODO:' line + headings).
Runs BODY with the scanned item list.  Cleans up buffers + directory."
  (declare (indent 1) (debug t))
  `(let ((org-air-r80--dir (make-temp-file "org-air-r80-" t)))
     (unwind-protect
         (progn
           (let ((file-name-handler-alist nil)
                 (coding-system-for-write 'utf-8-unix))
             (write-region "#+title: inbox\n" nil
                           (expand-file-name "inbox.org" org-air-r80--dir)
                           nil 'silent)
             (write-region ,content nil
                           (expand-file-name "board.org" org-air-r80--dir)
                           nil 'silent))
           (let* ((org-air-files (list org-air-r80--dir))
                  (org-air-inbox-file
                   (expand-file-name "inbox.org" org-air-r80--dir))
                  (org-air-cache-file
                   (expand-file-name ".cache/board.eld" org-air-r80--dir))
                  (org-air-view-width 120)
                  (org-air-view-height 50)
                  (org-air-r80--items (org-air-query-items)))
             ,@body))
       (org-air-query-teardown)
       (let ((kill-buffer-query-functions nil))
         (dolist (buf (buffer-list))
           (let ((fn (buffer-file-name buf)))
             (when (and fn (string-prefix-p org-air-r80--dir fn))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r80--dir t))))

(defconst org-air-r80--board-short
  "#+TITLE: r80 short-keyword board
#+TODO: TODO OUT OFF | DONE
* OUT Alpha parked task
SCHEDULED: <2026-07-23 Thu>
* OFF Beta inactive task
SCHEDULED: <2026-07-23 Thu>
"
  "A board whose widest SHOWN keyword is a 3-col OUT/OFF (below the floor).")

(defconst org-air-r80--board-wide
  "#+TITLE: r80 wide-keyword board
#+TODO: TODO WAITING | DONE
* WAITING Alpha blocked task
SCHEDULED: <2026-07-23 Thu>
* TODO Beta ordinary task
SCHEDULED: <2026-07-23 Thu>
"
  "A board whose widest SHOWN keyword is a 7-col WAITING (above the floor).")

(defun org-air-r80--meta-todo-w (items width)
  "Return `org-air-view--meta-todo-w' after `--compute-meta-widths' over ITEMS.
Mirrors the r20-6 setup dance so the section-descriptor walk is valid."
  (with-temp-buffer
    (org-air-view-mode)
    (setq org-air-view--items items
          org-air-view--tag-filter nil)
    (org-air-view--classify-cache-ensure)
    (let ((org-air-view--render-partition
           (org-air-view--compute-partition items))
          (org-air-view--render-displayed
           (cons items (make-hash-table :test 'eq))))
      (org-air-view--compute-meta-widths items width)
      org-air-view--meta-todo-w)))

;;;; -------------------------------------------------------------------
;;;; Air-dir corpus (#+state: out / off docs) for the STATE surface
;;;; -------------------------------------------------------------------

(defmacro org-air-r80--with-air-dir (&rest body)
  "Build a temp Air project with out/off/ready/complete docs; run BODY.
Binds `org-air-r80--root' and `org-air-r80--docs' (collected)."
  (declare (indent 0) (debug t))
  `(let ((org-air-r80--root (make-temp-file "org-air-r80-proj" t)))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "air/v0.1" org-air-r80--root) t)
           (write-region "" nil
                         (expand-file-name "air-config.toml" org-air-r80--root)
                         nil 'silent)
           (pcase-dolist (`(,file . ,state)
                          '(("air/v0.1/alpha.org"   . "ready")
                            ("air/v0.1/bravo.org"    . "complete")
                            ("air/v0.1/charlie.org"  . "out")
                            ("air/v0.1/delta.org"    . "off")))
             (write-region
              (format "#+title: %s\n#+state: %s\n"
                      (file-name-base file) state)
              nil (expand-file-name file org-air-r80--root) nil 'silent))
           (let* ((org-air-r80--docs
                   (org-air-project--collect-docs org-air-r80--root)))
             ,@body))
       (delete-directory org-air-r80--root t))))

;;;; ===================================================================
;;;; E1 / r80-1 — out/off are FIRST-CLASS states, not the UNKNOWN face
;;;; ===================================================================

(ert-deftest org-air-r80-1-out-off-are-first-class-states-not-unknown ()
  "`org-air-project--state-face' maps out/off to the NEW distinct
standing-out faces, NOT `org-air-face-faded' (the dim UNKNOWN face).
Anti-vacuity: both resolve to something non-nil AND differ from the
faded face AND from each other.  Reverting (dropping the pcase arms)
collapses BOTH to `org-air-face-faded' and reddens."
  (skip-unless (locate-library "org-air"))
  (let ((out (org-air-project--state-face "out"))
        (off (org-air-project--state-face "off")))
    (should out)
    (should off)
    (should (eq out 'org-air-face-air-state-out))
    (should (eq off 'org-air-face-air-state-off))
    (should-not (eq out 'org-air-face-faded))
    (should-not (eq off 'org-air-face-faded))
    (should-not (eq out off))
    ;; the faces really exist (D1 declared them).
    (should (facep 'org-air-face-air-state-out))
    (should (facep 'org-air-face-air-state-off))))

;;;; ===================================================================
;;;; E2 / r80-2 — the STATE token + box are DRAFT-sized (not small)
;;;; ===================================================================

(ert-deftest org-air-r80-2-state-token-and-box-equal-draft ()
  "The out/off state TOKEN is the 5-col padded word (`OUT  '/`OFF  '), and
under the gui seam the `--state-svg-badge' box for out/off == the DRAFT
badge box == 5 cols * char-px.  The drawn label is the bare word, bold."
  (skip-unless (locate-library "org-air"))
  (should (equal (substring-no-properties (org-air-project--state-token "out"))
                 "OUT  "))
  (should (equal (substring-no-properties (org-air-project--state-token "off"))
                 "OFF  "))
  (should (= (string-width (org-air-project--state-token "out"))
             org-air-project--state-cell-w))
  (org-air-r80--with-gui-metrics
    (let ((draft (org-air-r80--box-w (org-air-project--state-svg-badge "draft")))
          (out   (org-air-project--state-svg-badge "out"))
          (off   (org-air-project--state-svg-badge "off")))
      (should draft)
      (should (= (org-air-r80--box-w out) draft))
      (should (= (org-air-r80--box-w off) draft))
      (should (= draft (* org-air-project--state-cell-w org-air-r80--char-px)))
      ;; the drawn label is the bare word, bold (not the padded token).
      (should (string-match-p ">OUT<" (org-air-r80--svg-data out)))
      (should (string-match-p ">OFF<" (org-air-r80--svg-data off)))
      (should (string-match-p "font-weight=\"bold\"" (org-air-r80--svg-data out))))))

;;;; ===================================================================
;;;; E3 / r80-3 — the KEYWORD badge min-width == DRAFT (the headline fix)
;;;; ===================================================================

(ert-deftest org-air-r80-3-keyword-badge-min-width-equals-draft ()
  "Under the gui seam a SHORT keyword (OUT/OFF) pill box == the DRAFT state
badge box == 5 cols * char-px; a long keyword (WAITING) keeps its natural
7-col box (the floor is a no-op above the min).  Anti-regression: with
`org-air-keyword-badge-min-cols' let-bound to 1 (pre-R80) the OUT box
drops to the raw 3-col 24 px — proving the assertion is the MIN-WIDTH,
not a tautology."
  (skip-unless (locate-library "org-air"))
  (org-air-r80--with-gui-metrics
    (let* ((f 'org-air-face-air-state-out)
           (draft (org-air-r80--box-w (org-air-project--state-svg-badge "draft")))
           (out (org-air-r80--box-w
                 (org-air-view--svg-keyword-badge (propertize "OUT" 'face f) f)))
           (off (org-air-r80--box-w
                 (org-air-view--svg-keyword-badge (propertize "OFF" 'face f) f)))
           (waiting (org-air-r80--box-w
                     (org-air-view--svg-keyword-badge
                      (propertize "WAITING" 'face f) f))))
      (should (= out draft))
      (should (= off draft))
      (should (= out (* 5 org-air-r80--char-px)))
      ;; a keyword already >= the floor keeps its natural width.
      (should (= waiting (* 7 org-air-r80--char-px)))
      ;; anti-tautology: drop the floor and OUT shrinks to its raw 3 cols.
      (let* ((org-air-keyword-badge-min-cols 1)
             (raw (org-air-r80--box-w
                   (org-air-view--svg-keyword-badge
                    (propertize "OUT" 'face f) f))))
        (should (= raw (* 3 org-air-r80--char-px)))
        (should-not (= raw draft))))))

;;;; ===================================================================
;;;; E4 / r80-4 — keyword face PARITY with the state chip (one colour)
;;;; ===================================================================

(ert-deftest org-air-r80-4-keyword-face-parity-with-state ()
  "`org-air-view--todo-face' resolves OUT/OFF to the NEW faces (NOT the
generic `org-air-face-todo'), and each is EQ the face the STATE chip uses
— one colour, two surfaces.  Guard: with `org-air-keyword-face-source' at
its `own' default the alist face wins; under the `org' opt-in a user's own
`org-todo-keyword-faces' entry for OUT wins (R79 leg, unchanged)."
  (skip-unless (locate-library "org-air"))
  (should (eq (org-air-view--todo-face "OUT") 'org-air-face-air-state-out))
  (should (eq (org-air-view--todo-face "OFF") 'org-air-face-air-state-off))
  (should-not (eq (org-air-view--todo-face "OUT") 'org-air-face-todo))
  ;; parity: the keyword face IS the state face (single source).
  (should (eq (org-air-view--todo-face "OUT")
              (org-air-project--state-face "out")))
  (should (eq (org-air-view--todo-face "OFF")
              (org-air-project--state-face "off")))
  ;; R79 leg: the `org' opt-in lets the user's palette win for OUT.
  (let ((org-air-keyword-face-source 'org)
        (org-todo-keyword-faces '(("OUT" . font-lock-warning-face))))
    (should (eq (org-air-view--todo-face "OUT") 'font-lock-warning-face)))
  ;; but at the `own' default the alist face still wins over any user table.
  (let ((org-air-keyword-face-source 'own)
        (org-todo-keyword-faces '(("OUT" . font-lock-warning-face))))
    (should (eq (org-air-view--todo-face "OUT") 'org-air-face-air-state-out))))

;;;; ===================================================================
;;;; E5 / r80-5 — meta-todo-w FLOORED (short board) / no-op (wide board)
;;;; ===================================================================

(ert-deftest org-air-r80-5-meta-todo-w-floored-and-pill-fits-cell ()
  "Over a board whose widest SHOWN keyword is OUT/OFF (3 cols),
`--compute-meta-widths' floors `org-air-view--meta-todo-w' to at least
`org-air-keyword-badge-min-cols' (badge style), so the 5-col pill fits its
cell.  Byte guard: a board whose widest keyword is WAITING (7 >= floor)
leaves `--meta-todo-w' UNCHANGED at 7 — the floor is a no-op, no golden
churn.  Anti-tautology: with the floor let-bound to 1 the short board's
column drops back to the raw 3."
  (skip-unless (locate-library "org-air"))
  (org-air-r80--with-board org-air-r80--board-short
    (should (= (length org-air-r80--items) 2))
    ;; floored: raw widest is 3 (OUT/OFF) -> reserved >= 5.
    (should (>= (org-air-r80--meta-todo-w org-air-r80--items 120)
                org-air-keyword-badge-min-cols))
    ;; anti-tautology: without the floor the column is the raw 3.
    (let ((org-air-keyword-badge-min-cols 1))
      (should (= (org-air-r80--meta-todo-w org-air-r80--items 120) 3))))
  (org-air-r80--with-board org-air-r80--board-wide
    ;; no-op: widest is WAITING (7) >= floor, so the column stays 7.
    (should (= (org-air-r80--meta-todo-w org-air-r80--items 120) 7))))

;;;; ===================================================================
;;;; E6 / r80-6 — out/off get their OWN section, rank + letter
;;;; ===================================================================

(ert-deftest org-air-r80-6-out-off-render-in-own-section-and-rank ()
  "An Air dir with an out doc and an off doc yields an OUT section and an
OFF section (not folded into Unknown); `--state-sort-rank' out/off are
DISTINCT, members of `--state-sort-order', and rank between complete and
unknown/dropped; `--state-letter' out=\"O\" / off=\"F\" (distinct — NO
collision).  Reverting (dropping out/off from the lists) reddens (both
rank Unknown; both letters collide on \"O\")."
  (skip-unless (locate-library "org-air"))
  ;; letters: distinct, no O/O collision.
  (should (equal (org-air-project--state-letter "out") "O"))
  (should (equal (org-air-project--state-letter "off") "F"))
  (should-not (equal (org-air-project--state-letter "out")
                     (org-air-project--state-letter "off")))
  ;; ranks: distinct, members, and between complete and unknown/dropped.
  (let ((r-out (org-air-project--state-sort-rank "out"))
        (r-off (org-air-project--state-sort-rank "off"))
        (r-comp (org-air-project--state-sort-rank "complete"))
        (r-unknown (org-air-project--state-sort-rank "zzz-unknown"))
        (r-dropped (org-air-project--state-sort-rank "dropped")))
    (should (member "out" org-air-project--state-sort-order))
    (should (member "off" org-air-project--state-sort-order))
    (should-not (= r-out r-off))
    (should (> r-out r-comp))
    (should (> r-off r-comp))
    (should (< r-out r-unknown))
    (should (< r-off r-unknown))
    (should (< r-out r-dropped))
    (should (< r-off r-dropped)))
  ;; sections: an out doc and an off doc each get their own bucket.
  (org-air-r80--with-air-dir
    (let* ((org-air-project-group 'state)
           (sections (org-air-project--sections org-air-r80--docs))
           (titles (mapcar (lambda (s) (plist-get s :title)) sections)))
      (should (member "Out" titles))
      (should (member "Off" titles))
      ;; the out section carries the standing-out face, NOT faded.
      (let ((out-sec (seq-find (lambda (s) (equal (plist-get s :title) "Out"))
                               sections)))
        (should (eq (plist-get out-sec :icon-face)
                    'org-air-face-air-state-out))))))

;;;; ===================================================================
;;;; E7 / r80-7 — no line-height growth (COLUMNS only)
;;;; ===================================================================

(ert-deftest org-air-r80-7-no-line-height-growth ()
  "The OUT keyword badge and the OUT state chip carry a `display' IMAGE
\(imagep), never a `:height' face — the widening is COLUMNS only
\(svg-never-grows-line).  Assert no `:height' anywhere on the badge string."
  (skip-unless (locate-library "org-air"))
  (org-air-r80--with-gui-metrics
    (dolist (s (list (org-air-view--svg-keyword-badge
                      (propertize "OUT" 'face 'org-air-face-air-state-out)
                      'org-air-face-air-state-out)
                     (org-air-project--state-svg-badge "out")))
      (should (org-air-r80--display-image s))       ; an IMAGE overlay
      ;; no :height on any face anywhere on the string.
      (let ((faces (append (list (get-text-property 0 'face s))
                           (mapcar (lambda (i) (get-text-property i 'face s))
                                   (number-sequence 0 (1- (length s)))))))
        (dolist (f faces)
          (when (and (listp f) (not (null f)))
            (should-not (plist-member f :height))))))))

;;;; ===================================================================
;;;; E8 / r80-8 — byte / TTY fallback RESERVES the min width
;;;; ===================================================================

(ert-deftest org-air-r80-8-byte-tty-fallback-reserves-min-width ()
  "With svg unavailable (display-graphic-p -> nil) the keyword badge
returns the 5-col PADDED token `OUT  ' (no `display' image) — the
mandatory text fallback reserving the SAME width as a state token; and
`--state-token' out/off is likewise `OUT  '/`OFF  ' at the byte layer."
  (skip-unless (locate-library "org-air"))
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) nil)))
    (let* ((f 'org-air-face-air-state-out)
           (badge (org-air-view--svg-keyword-badge (propertize "OUT" 'face f) f)))
      (should (equal (substring-no-properties badge) "OUT  "))
      (should (= (string-width badge) org-air-keyword-badge-min-cols))
      (should-not (org-air-r80--display-image badge))))
  (should (equal (substring-no-properties (org-air-project--state-token "out"))
                 "OUT  "))
  (should (equal (substring-no-properties (org-air-project--state-token "off"))
                 "OFF  ")))

;;;; ===================================================================
;;;; E9 / r80-9 — no rescan + unrelated boards byte-identical (floor no-op)
;;;; ===================================================================

(defmacro org-air-r80--counting-queries (counter &rest body)
  "Run BODY counting `org-air-query-items' calls into COUNTER (R55 guard)."
  (declare (indent 1) (debug t))
  `(let ((,counter 0)
         (org-air-r80--real-query (symbol-function 'org-air-query-items)))
     (cl-letf (((symbol-function 'org-air-query-items)
                (lambda (&rest args)
                  (cl-incf ,counter)
                  (apply org-air-r80--real-query args))))
       ,@body)))

(ert-deftest org-air-r80-9-no-rescan-and-floor-is-noop-above-min ()
  "R53: the R80 faces/widths are PURE over cached items — computing the
keyword column + the state faces triggers ZERO `org-air-query-items'
calls.  And the floor is a genuine no-op for a board whose widest keyword
is already >= the min (a WAITING board reserves exactly 7, unchanged by
R80), so an OUT/OFF-free wide board is byte-identical."
  (skip-unless (locate-library "org-air"))
  (org-air-r80--with-board org-air-r80--board-wide
    ;; no OUT/OFF in this board.
    (should-not (seq-find (lambda (it) (member (org-air-item-todo it)
                                               '("OUT" "OFF")))
                          org-air-r80--items))
    (org-air-r80--counting-queries n
      (should (= (org-air-r80--meta-todo-w org-air-r80--items 120) 7))
      ;; the state faces are pure lookups too — no scan.
      (should (org-air-project--state-face "out"))
      (should (org-air-view--todo-face "OUT"))
      (should (= n 0)))))

(provide 'org-air-round80-test)
;;; org-air-round80-test.el ends here
