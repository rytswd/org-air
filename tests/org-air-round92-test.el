;;; org-air-round92-test.el --- acceptance tests for round 92 -*- lexical-binding: t; -*-

;;; Commentary:
;; Round-92 acceptance: ONE LANDING RULE FOR FOUR VIEWS.
;;
;; A repaint PRESERVES the row the user is standing on — in every org-air
;; view.  It lands on the view's first row ONLY when that row's identity
;; has genuinely vanished, and when it does the scroll seam stands down so
;; the view's own banner and state-summary line stay on screen.
;;
;; WHY THIS FILE EXISTS, stated bluntly.  R91 fixed the reported board
;; defect and shipped 31 new tests, 22 of them RED on their parent — and
;; those 31 tests were BLIND to the regression the same round introduced
;; and to two commands that were the reported defect verbatim in other
;; views.  The reason is structural and is the lesson this file is built
;; around: R91's other-view tests asserted `offset < window-body-height'
;; ("the landing is somewhere on screen"), which is true both with and
;; without the view's banner scrolled off the top.  An assertion with
;; teeth for a viewport rule has to name what the WINDOW SHOWS:
;;
;;     row X is on screen line N   AND   the view's banner is visible.
;;
;; Every test below is stated in that form.  The instruments are
;; `org-air-landing-test-visible-lines' (the lines the window really
;; shows, walked with `vertical-motion' from `window-start'), the 0-based
;; screen-line offset, `window-start', `window-point', the buffer text of
;; the row under the cursor and the character under the cursor.  No
;; assertion names an org-air internal; the row's IDENTITY is read out of
;; the buffer text (`Document number 33', `Note number 09',
;; `Task number 23'), never out of a struct or a text property.
;;
;; MEASURED ON THE PARENT (`pppqmqrtvqyz', R91) with the same corpora, so
;; every claim below is a real defect and not a hypothetical:
;;
;;   project, banner at `window-start', shared repaint
;;                        ws 1 -> 178 / 154 / 81 / 80, banner GONE
;;   revisit, same        ws 1 -> 81,  banner GONE
;;   review,  same        ws 1 -> 98,  banner and `+ Completed 30' GONE
;;   project TAB fold     ws 2041 -> 1, off 4 -> 32 in an 11-row window
;;   review  TAB section  ws 2549 -> 1, off 5 -> 40 in a 10-row window
;;   project `g'          row `Document number 33' -> `Document number 00'
;;   project sort/group/( / resize   same, every one
;;   revisit `g'/`m'/`z c'/TAB page  row `Note number 09' -> `Note 00'
;;   review `g'/`f'/`+'/`-'/`s'/`S'/`/'  row `Task number 23' -> `Task 00'
;;   review `b' backlog   row `Task number 17' -> `Task number 00'
;;                        (a MUTATION verb losing the row it acted on)
;;   bookmark jump        the BYSTANDER window ws 1493 -> 1, on the banner
;;   repaint that signals ws 2736 -> 1, off 6 -> 47 (off-screen)
;;   one `m'              2 `set-window-start' installs, not 1
;;   point above `window-start'   a FICTITIOUS offset of 10 anchored
;;
;; NOTE on instruments: `pos-visible-in-window-p' is unusable in --batch
;; (a batch frame never realises glyph matrices), so visibility is
;; screen-line arithmetic over `window-start' + `window-body-height',
;; which is exact.  Nothing here is pixel-dependent; nothing here is a
;; GUI skip.

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
;;;; Assertions
;;;; =====================================================================

(defun org-air-r92--snapshot (win)
  "Return the observable landing state of WIN as a plist."
  (list :start (window-start win)
        :offset (org-air-landing-test-offset win)
        :id (org-air-landing-test-row-id win)
        :column (current-column)))

(defun org-air-r92--assert-preserved (win before label &optional columnp)
  "Assert WIN still shows BEFORE's row, on BEFORE's screen line.
LABEL names the keystroke in the failure message.  With COLUMNP the
cursor's COLUMN must have been preserved too (only meaningful across a
repaint that does not change the row's shape)."
  (let ((now (org-air-r92--snapshot win)))
    ;; The user is still on the SAME row — read out of the buffer text.
    (should (equal (list label (plist-get before :id))
                   (list label (plist-get now :id))))
    ;; ... redrawn on the SAME screen line ...
    (should (equal (list label (plist-get before :offset))
                   (list label (plist-get now :offset))))
    ;; ... inside the window ...
    (should (< (plist-get now :offset) (window-body-height win)))
    ;; ... with the window following the buffer's point ...
    (should (= (window-point win) (point)))
    ;; ... and the cursor on a real glyph of the row, not in the gutter.
    (should (org-air-landing-test-on-glyph-p))
    (when columnp
      (should (equal (list label (plist-get before :column))
                     (list label (plist-get now :column)))))))

(defun org-air-r92--assert-chrome-visible (win mode label)
  "Assert WIN shows MODE's banner — the view's own header band.
This is the assertion R91 could not make: its other-view tests said only
that the landing was somewhere on screen, which stayed true while the
banner, the state-summary line and the column header scrolled off the
top."
  (let ((lines (org-air-landing-test-visible-lines win))
        (banner (org-air-landing-test-banner mode)))
    (should (equal (list label banner t)
                   (list label banner
                         (and (seq-find (lambda (l) (string-match-p banner l))
                                        lines)
                              t))))))

;;;; =====================================================================
;;;; 1. View CHROME after a repaint — the R91 regression
;;;; =====================================================================
;;
;; The reviewer's GUI repro: `M-x org-air-project', `M-<' (the cursor on
;; the banner, the viewport at the top), then the rail toggle — which
;; routes through the SHARED `org-air-view--refresh-current' dispatch.
;; On R91 the shared seam pinned the pre-repaint screen-line offset onto
;; the render's brand-new first-row landing, which pushed the banner, the
;; blank, the `DRAFT/READY/WIP…' state summary and the column header off
;; the top of the window.  Driving the shared dispatch directly is the
;; only way to reproduce it without also changing the window layout (the
;; rail toggle opens a side window), which is exactly what R91's own
;; other-view tests did — they just did not look at the chrome.

(ert-deftest org-air-r92-project-repaint-keeps-banner-and-state-summary-visible ()
  "A shared repaint with the cursor on the project's own chrome keeps it.
Four parked geometries, each with `window-start' at the top of the
buffer and the cursor on a NON-row line (banner / state summary / blank
/ group header).  Measured on the parent: `window-start' 1 -> 178, 154,
81, 80 — the banner and the state-summary line off the top every time."
  (skip-unless (fboundp 'org-air-project))
  (org-air-landing-test-with-project
    (let ((win (selected-window))
          (summary "DRAFT [0-9]+ +READY"))
      ;; The corpus really does render the chrome this test is about.
      (should (org-air-landing-test-visible-p
               win (org-air-landing-test-banner 'org-air-project-mode)))
      (should (org-air-landing-test-visible-p win summary))
      (dolist (spec '((0 . 0) (2 . 2) (3 . 3) (4 . 4)))
        (goto-char (point-min))
        (forward-line (car spec))
        (org-air-landing-test-park win (cdr spec))
        (should (= (point-min) (window-start win)))
        (org-air-view--refresh-current)
        (let ((label (format "shared repaint, cursor on line %d"
                             (1+ (car spec)))))
          (org-air-r92--assert-chrome-visible win 'org-air-project-mode label)
          ;; The state-summary line — the second thing the user reads —
          ;; is visible too, and the viewport did not scroll at all.
          (should (equal (list label t)
                         (list label (org-air-landing-test-visible-p
                                      win summary))))
          (should (equal (list label (point-min))
                         (list label (window-start win))))
          (should (= (window-point win) (point))))))))

(ert-deftest org-air-r92-revisit-repaint-keeps-banner-visible ()
  "The same shared repaint keeps the Revisit view's banner on screen.
Parent: `window-start' 1 -> 81, the banner off the top."
  (skip-unless (fboundp 'org-air-revisit))
  (org-air-landing-test-with-revisit
    (let ((win (selected-window)))
      (goto-char (point-min))
      (org-air-landing-test-park win 0)
      (org-air-view--refresh-current)
      (org-air-r92--assert-chrome-visible win 'org-air-revisit-mode
                                          "revisit shared repaint")
      (should (= (point-min) (window-start win)))
      (should (= (window-point win) (point))))))

(ert-deftest org-air-r92-review-repaint-keeps-banner-and-section-header-visible ()
  "The same shared repaint keeps the Review banner AND its first section.
Parent: `window-start' 1 -> 98 — banner and `+ Completed 30' both off."
  (skip-unless (fboundp 'org-air-review))
  (org-air-landing-test-with-review
    (let ((win (selected-window)))
      (goto-char (point-min))
      (org-air-landing-test-park win 0)
      (org-air-view--refresh-current)
      (org-air-r92--assert-chrome-visible win 'org-air-review-mode
                                          "review shared repaint")
      (should (org-air-landing-test-visible-p win "\\+ Completed [0-9]+"))
      (should (= (point-min) (window-start win)))
      (should (= (window-point win) (point))))))

;;;; =====================================================================
;;;; 2. The two TAB seams — the reported defect, verbatim, in other views
;;;; =====================================================================

(ert-deftest org-air-r92-project-tab-fold-row-holds-its-screen-line ()
  "TAB on a dropped-group fold row leaves the row on its screen line.
Both branches repaint and THEN re-land point on a row they compute
themselves, so the seam has to close after that landing.  Measured on
the parent: `window-start' 2041 -> 1 and the row from screen line 4 to
32 in an 11-row window — off-screen, the user's original report
character for character."
  (skip-unless (fboundp 'org-air-project-toggle-dropped))
  (org-air-landing-test-with-project
    (let* ((win (selected-window))
           (pos (text-property-not-all (point-min) (point-max)
                                       'org-air-dropped-fold nil)))
      (should pos)
      (goto-char pos)
      (org-air-view--goto-row-title)
      (org-air-landing-test-park win 4)
      (let ((start (window-start win))
            (fold (org-air-landing-test-row win)))
        (should (string-match-p "dropped" fold))
        ;; Expand: the revealed group's first row inherits the fold row's
        ;; screen line.
        (org-air-project-toggle-dropped)
        (should (= 4 (org-air-landing-test-offset win)))
        (should (= start (window-start win)))
        (should (< 4 (window-body-height win)))
        (should (= (window-point win) (point)))
        (should (org-air-landing-test-on-glyph-p))
        ;; Collapse: back onto the fold row, still on screen line 4.
        (org-air-project-toggle-dropped)
        (should (equal fold (org-air-landing-test-row win)))
        (should (= 4 (org-air-landing-test-offset win)))
        (should (= start (window-start win)))
        (should (= (window-point win) (point)))))))

(ert-deftest org-air-r92-review-tab-section-header-holds-its-screen-line ()
  "TAB on a Review section header leaves the header on its screen line.
Measured on the parent: `window-start' 2549 -> 1 and the header from
screen line 5 to 40 in a 10-row window — off-screen."
  (skip-unless (fboundp 'org-air-review-toggle-section))
  (org-air-landing-test-with-review
    (let* ((win (selected-window))
           (pos (org-air-view--find-property 'org-air-section 'carried)))
      (should pos)
      (goto-char pos)
      (org-air-view--goto-row-title)
      (org-air-landing-test-park win 5)
      (let ((start (window-start win))
            (header (org-air-landing-test-row win)))
        (should (string-match-p "Carried over" header))
        (dolist (label '("TAB collapse" "TAB expand"))
          (org-air-review-toggle-section)
          (should (equal (list label header)
                         (list label (org-air-landing-test-row win))))
          (should (equal (list label 5)
                         (list label (org-air-landing-test-offset win))))
          (should (equal (list label start) (list label (window-start win))))
          (should (< 5 (window-body-height win)))
          (should (= (window-point win) (point))))))))

;;;; =====================================================================
;;;; 3. The uniform landing rule — a repaint keeps the row you are on
;;;; =====================================================================

(ert-deftest org-air-r92-project-refresh-keeps-the-row-and-the-column ()
  "`g' in the project view keeps the row AND the cursor's column.
Parent: point 2212 -> 187, row `Document number 33' -> `…00'."
  (skip-unless (fboundp 'org-air-project-refresh))
  (org-air-landing-test-with-project
    (let ((win (selected-window)))
      (org-air-landing-test-goto "Document number 33")
      (org-air-landing-test-park win 5)
      (let ((before (org-air-r92--snapshot win)))
        (should (equal "Document number 33" (plist-get before :id)))
        (org-air-project-refresh)
        (org-air-r92--assert-preserved win before "g refresh" t)
        ;; A second `g' is idempotent: same row, same line, same start.
        (org-air-project-refresh)
        (org-air-r92--assert-preserved win before "g refresh (twice)" t)))))

(ert-deftest org-air-r92-project-sort-group-and-filenames-keep-the-row ()
  "Sort, the group-by keys and `(' all keep the project row you are on.
Parent: every one of them landed on `Document number 00'."
  (skip-unless (fboundp 'org-air-project-group-by-tag))
  (org-air-landing-test-with-project
    (let ((win (selected-window)))
      (dolist (case (list (cons "s sort-cycle" #'org-air-view-sort-cycle)
                          (cons "t group-by-tag" #'org-air-project-group-by-tag)
                          (cons "d group-by-directory"
                                #'org-air-project-group-by-directory)))
        (org-air-landing-test-goto "Document number 33")
        (org-air-landing-test-park win 5)
        (let ((before (org-air-r92--snapshot win)))
          (funcall (cdr case))
          (org-air-r92--assert-preserved win before (car case))))
      ;; `(' RENAMES the row (title -> filename), so its identity is
      ;; asserted on the doc the row still names rather than on the row's
      ;; whole text: same doc, same screen line, same viewport.
      (org-air-landing-test-goto "Document number 33")
      (org-air-landing-test-park win 5)
      (let ((start (window-start win)))
        (org-air-project-toggle-filenames)
        (should (string-match-p "doc-33\\.org"
                                (org-air-landing-test-row win)))
        (should (equal (list "( toggle-filenames" 5)
                       (list "( toggle-filenames"
                             (org-air-landing-test-offset win))))
        (should (equal (list "( toggle-filenames" start)
                       (list "( toggle-filenames" (window-start win))))
        (should (= (window-point win) (point)))
        (should (org-air-landing-test-on-glyph-p))))))

(ert-deftest org-air-r92-project-resize-repaint-keeps-the-row ()
  "The debounced resize repaint keeps the project row on its screen line.
Driven through `org-air-layout-refresh-function' — the buffer-local seam
the debounced window-size hook calls, whatever the mode installed there.
Parent: row `Document number 33' -> `Document number 00'."
  (skip-unless (fboundp 'org-air-project))
  (org-air-landing-test-with-project
    (let ((win (selected-window)))
      (org-air-landing-test-goto "Document number 33")
      (org-air-landing-test-park win 5)
      (let ((before (org-air-r92--snapshot win)))
        (should (functionp org-air-layout-refresh-function))
        ;; Force the width-change branch to fire a real repaint.
        (setq org-air-project--rendered-width nil)
        (funcall org-air-layout-refresh-function)
        (org-air-r92--assert-preserved win before "resize repaint")))))

(ert-deftest org-air-r92-revisit-refresh-surface-and-created-keep-the-row ()
  "`g', `m' (surface cycle) and `z c' keep the Revisit note you are on.
Parent: point 2436 -> 87, row `Note number 09' -> `Note number 00'."
  (skip-unless (fboundp 'org-air-revisit-refresh))
  (org-air-landing-test-with-revisit
    (let ((win (selected-window)))
      (dolist (case (list (cons "g refresh" #'org-air-revisit-refresh)
                          (cons "z c toggle-created"
                                #'org-air-revisit-toggle-created)
                          (cons "m cycle-surface"
                                #'org-air-revisit-cycle-surface)))
        (org-air-landing-test-goto "Note number 09")
        (org-air-landing-test-park win 5)
        (let ((before (org-air-r92--snapshot win)))
          (should (equal "Note number 09" (plist-get before :id)))
          (funcall (cdr case))
          (org-air-r92--assert-preserved win before (car case)))))))

(ert-deftest org-air-r92-revisit-paging-keeps-the-fold-row ()
  "TAB on the `…and N more' fold row keeps the fold row under the cursor.
The page really grows (the buffer gains rows) and the fold row moves
DOWN the buffer, so this is a landing that has to be re-resolved, not a
position that happens to survive.  Parent: the cursor was thrown to
`Note number 00' at screen line 2."
  (skip-unless (fboundp 'org-air-revisit-toggle-more))
  (org-air-landing-test-with-revisit
    (let* ((win (selected-window))
           (pos (text-property-not-all (point-min) (point-max)
                                       'org-air-more-row nil)))
      (should pos)
      (goto-char pos)
      (org-air-view--goto-row-title)
      (org-air-landing-test-park win 4)
      (let ((lines (line-number-at-pos (point-max))))
        (should (string-match-p "more" (org-air-landing-test-row win)))
        (org-air-revisit-toggle-more)
        ;; A page really was revealed ...
        (should (> (line-number-at-pos (point-max)) lines))
        ;; ... and the fold row is still under the cursor, on line 4.
        (should (string-match-p "more" (org-air-landing-test-row win)))
        (should (= 4 (org-air-landing-test-offset win)))
        (should (< 4 (window-body-height win)))
        (should (= (window-point win) (point)))
        (should (org-air-landing-test-on-glyph-p))))))

(ert-deftest org-air-r92-review-refresh-rollup-and-range-keep-the-row ()
  "`g', `f' rollup, `+'/`-' and `m' keep the Review item you are on.
Parent: point 4933 -> 106, row `Task number 23' -> `Task number 00'."
  (skip-unless (fboundp 'org-air-review-refresh))
  (org-air-landing-test-with-review
    (let ((win (selected-window)))
      (dolist (case (list (cons "g refresh" #'org-air-review-refresh)
                          (cons "f cycle-rollup" #'org-air-review-cycle-rollup)
                          (cons "+ range-widen" #'org-air-review-range-widen)
                          (cons "- range-narrow" #'org-air-review-range-narrow)
                          (cons "m cycle-range" #'org-air-review-cycle-range)))
        (org-air-landing-test-goto "Task number 23")
        (org-air-landing-test-park win 5)
        (let ((before (org-air-r92--snapshot win)))
          (should (equal "Task number 23" (plist-get before :id)))
          (funcall (cdr case))
          (org-air-r92--assert-preserved win before (car case)))))))

(ert-deftest org-air-r92-review-scope-and-filter-keep-the-row ()
  "`s' scope, `S' scope-clear and `/' filter keep the Review row.
The filter is chosen so the parked row SURVIVES it (`Task number 23' is
odd), which is the case that distinguishes preserving from jumping."
  (skip-unless (fboundp 'org-air-review-scope))
  (org-air-landing-test-with-review
    (let ((win (selected-window)))
      (org-air-landing-test-goto "Task number 23")
      (org-air-landing-test-park win 5)
      (let ((before (org-air-r92--snapshot win)))
        (org-air-review-scope "all")
        (org-air-r92--assert-preserved win before "s scope")
        (org-air-review-scope-clear)
        (org-air-r92--assert-preserved win before "S scope-clear"))
      ;; `/' changes the buffer's line count and the row keeps its line.
      (let ((lines (line-number-at-pos (point-max)))
            (before (org-air-r92--snapshot win)))
        (org-air-review-filter '("odd"))
        (should (< (line-number-at-pos (point-max)) lines))
        (should (equal "Task number 23" (org-air-landing-test-row-id win)))
        (should (= 5 (org-air-landing-test-offset win)))
        (should (= (window-point win) (point)))
        (should (org-air-landing-test-on-glyph-p))
        (ignore before)))))

(ert-deftest org-air-r92-review-backlog-verb-keeps-the-row-it-acted-on ()
  "`b' in the Review view keeps the row it just mutated under the cursor.
The review called this one indefensible: a mutation verb that loses the
row it acted on.  Parent: point 4933 -> 106, row `Task number 17' ->
`Task number 00'.  The row's TEXT must also show the write landed."
  (skip-unless (fboundp 'org-air-item-backlog))
  (org-air-landing-test-with-review
    (let ((win (selected-window)))
      (org-air-landing-test-goto "Task number 17")
      (org-air-landing-test-park win 5)
      (let ((before (org-air-r92--snapshot win)))
        (should (equal "Task number 17" (plist-get before :id)))
        (should-not (string-match-p "backlog" (org-air-landing-test-row win)))
        (org-air-item-backlog)
        (org-air-r92--assert-preserved win before "b backlog")
        ;; The verb really did something: the row now carries the tag.
        (should (string-match-p "backlog" (org-air-landing-test-row win)))))))

(ert-deftest org-air-r92-vanished-row-lands-on-first-row-and-says-so ()
  "When the row genuinely VANISHES the view lands on its first row —
and it says so, by leaving the viewport to redisplay so the view's own
banner is on screen instead of a stale screen line pinned onto a
brand-new landing.  This is the OTHER half of the rule, and the only
case where landing on row 1 is right."
  (skip-unless (fboundp 'org-air-project-filter))
  (org-air-landing-test-with-project
    (let ((win (selected-window)))
      (org-air-landing-test-goto "Document number 33")
      (org-air-landing-test-park win 5)
      (should (equal "Document number 33" (org-air-landing-test-row-id win)))
      (org-air-project-filter '("no-such-tag-anywhere"))
      ;; The row is gone from the buffer entirely ...
      (should-not (string-match-p "Document number 33" (buffer-string)))
      ;; ... the viewport was NOT pinned to the stale screen line ...
      (should (= (point-min) (window-start win)))
      ;; ... so the view's own chrome is what the user sees.
      (org-air-r92--assert-chrome-visible win 'org-air-project-mode
                                          "/ vanished row")
      (should (= (window-point win) (point)))))
  (org-air-landing-test-with-review
    (let ((win (selected-window)))
      (org-air-landing-test-goto "Task number 23")
      (org-air-landing-test-park win 5)
      ;; `>' moves to a period with no work in it at all.
      (org-air-review-period-next)
      (should-not (string-match-p "Task number 23" (buffer-string)))
      (should (= (point-min) (window-start win)))
      (org-air-r92--assert-chrome-visible win 'org-air-review-mode
                                          "> empty period")
      ;; `.' comes back to today and lands on the first row of the week.
      (org-air-review-period-today)
      (should (string-match-p "Task number 23" (buffer-string)))
      (should (= (point-min) (window-start win)))
      (org-air-r92--assert-chrome-visible win 'org-air-review-mode
                                          ". period today")
      (should (org-air-landing-test-first-row-p)))))

(ert-deftest org-air-r92-view-entry-still-lands-on-the-first-row ()
  "A view ENTRY is an explicit jump and still owns its landing (R26-5).
Re-entering the project view from a row well down the buffer lands on
the FIRST doc with the banner on screen — the rule preserves rows across
REPAINTS, not across a deliberate re-open."
  (skip-unless (fboundp 'org-air-project))
  (org-air-landing-test-with-project
    (let ((win (selected-window)))
      (org-air-landing-test-goto "Document number 33")
      (org-air-landing-test-park win 5)
      (org-air-project org-air-landing-test--dir)
      (let ((w (get-buffer-window (get-buffer "*org-air-project*"))))
        (should (window-live-p w))
        (with-current-buffer (window-buffer w)
          (should (org-air-landing-test-first-row-p))
          (org-air-r92--assert-chrome-visible w 'org-air-project-mode
                                              "project re-entry"))))))

;;;; =====================================================================
;;;; 4. The bookmark exclusion is PER WINDOW
;;;; =====================================================================

(ert-deftest org-air-r92-bookmark-jump-leaves-bystander-window-alone ()
  "A bookmark jump owns the JUMPING window's landing — and only its own.
Parent: the bystander window was dropped to `window-start' 1 with the
banner under its point, because the R58 exclusion was all-or-nothing."
  (skip-unless (and (boundp 'org-air-view--bookmark-locator)
                    (fboundp 'org-air-view--render-current)))
  (org-air-landing-test-with-board
    (let* ((w1 (selected-window))
           (w2 (split-window w1 nil 'below)))
      (set-window-buffer w2 (current-buffer))
      (should (= 2 (length (get-buffer-window-list (current-buffer)
                                                   'nomini t))))
      (org-air-landing-test-goto "Task number 40")
      (org-air-landing-test-park w1 5)
      (save-excursion
        (org-air-landing-test-goto "Task number 20")
        (set-window-point w2 (point))
        (set-window-start w2 (save-excursion (vertical-motion -3 w2) (point))
                          t))
      (select-window w1)
      (goto-char (window-point w1))
      (let ((s2 (window-start w2))
            (r2 (org-air-landing-test-row w2)))
        (should (= 3 (org-air-landing-test-offset w2)))
        (setq org-air-view--bookmark-locator (list :title "Task number 12"))
        (org-air-view--render-current)
        ;; The jumping window lands on the bookmarked row and follows it.
        (should (string-match-p "Task number 12"
                                (org-air-landing-test-row w1)))
        (should (= (window-point w1) (point)))
        (should (null org-air-view--bookmark-locator))
        ;; The BYSTANDER kept its own row, its own line and its own start.
        (should (equal (list "bystander row" r2)
                       (list "bystander row" (org-air-landing-test-row w2))))
        (should (equal (list "bystander offset" 3)
                       (list "bystander offset"
                             (org-air-landing-test-offset w2))))
        (should (equal (list "bystander window-start" s2)
                       (list "bystander window-start" (window-start w2))))))))

;;;; =====================================================================
;;;; 5. Seam correctness
;;;; =====================================================================

(ert-deftest org-air-r92-repaint-that-signals-keeps-the-viewport ()
  "A repaint that SIGNALS mid-render must not strand `window-start'.
The erase has already dropped every displaying window's `window-start'
marker to `point-min', so skipping the restore on the error path loses
the user's place in exactly the case where they most need it kept.
Parent: `window-start' 2736 -> 1, the row from screen line 6 to 47 —
off-screen."
  (skip-unless (fboundp 'org-air-view--with-scroll-stable))
  (org-air-landing-test-with-board
    (let ((win (selected-window)))
      (org-air-landing-test-goto "Task number 40")
      (org-air-landing-test-park win 6)
      (let ((start (window-start win))
            (text (buffer-string))
            (pt (point)))
        (should-error
         (org-air-view--with-scroll-stable
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert text)
             (goto-char pt)
             (error "repaint failed mid-render"))))
        ;; The buffer was re-inserted; the viewport survived the signal.
        (should (equal text (buffer-string)))
        (should (= start (window-start win)))
        (should (= 6 (org-air-landing-test-offset win)))
        (should (< 6 (window-body-height win)))
        (should (= (window-point win) (point)))))))

(ert-deftest org-air-r92-one-anchor-restore-pair-per-keystroke ()
  "One keystroke installs the board window's `window-start' exactly ONCE.
The seam nests (the shared dispatch calls the render, which calls the
repaint), and on the parent every level took its own pair: one `m'
installed `window-start' TWICE, the inner pair anchoring an intermediate
landing.  Asserted on the core Emacs API, not on any org-air internal."
  (skip-unless (fboundp 'org-air-toggle-mark))
  (org-air-landing-test-with-board
    (let* ((win (selected-window))
           (buf (current-buffer))
           (calls 0)
           (forced nil))
      (org-air-landing-test-goto "Task number 40")
      (org-air-landing-test-park win 6)
      (cl-letf* ((orig (symbol-function 'set-window-start))
                 ((symbol-function 'set-window-start)
                  (lambda (w pos &optional noforce)
                    (when (and (window-live-p w) (eq (window-buffer w) buf))
                      (setq calls (1+ calls))
                      (unless noforce (setq forced t)))
                    (funcall orig w pos noforce))))
        (org-air-toggle-mark))
      (should (equal (list "m installs" 1) (list "m installs" calls)))
      (should-not forced)
      (should (= 6 (org-air-landing-test-offset win))))))

(ert-deftest org-air-r92-point-above-window-start-is-not-given-a-fictitious-offset ()
  "A row that was NOT on screen has no screen line to preserve.
`count-screen-lines' takes the ABSOLUTE distance between its bounds, so
a window whose point sits ABOVE its `window-start' (the user scrolled
their cursor off the top) measured a FICTITIOUS offset and anchored it.
Parent: an offset of 10 invented and installed at `window-start' 2442.
The window's POINT is still repaired either way — only the invented
viewport is dropped, leaving redisplay to re-anchor as Emacs does for
any off-screen point."
  (skip-unless (fboundp 'org-air-toggle-mark))
  (org-air-landing-test-with-board
    (let ((win (selected-window)))
      (org-air-landing-test-goto "Task number 40")
      (set-window-point win (point))
      ;; Scroll the viewport 10 lines PAST point: point is now ABOVE
      ;; `window-start' and therefore off the top of the window.
      (set-window-start win (save-excursion (vertical-motion 10 win) (point))
                        t)
      (should (< (window-point win) (window-start win)))
      (let ((fictitious (max 0 (1- (count-screen-lines (window-start win)
                                                       (window-point win)
                                                       t win)))))
        (should (= 10 fictitious))
        (org-air-toggle-mark)
        ;; The repaint did NOT anchor the made-up distance ...
        (should-not (equal (list "fictitious offset" fictitious)
                           (list "fictitious offset"
                                 (org-air-landing-test-offset win))))
        ;; ... and the window's point was still repaired onto the row.
        (should (= (window-point win) (point)))
        (should (string-match-p "Task number 40"
                                (org-air-landing-test-row win)))))))

(provide 'org-air-round92-test)
;;; org-air-round92-test.el ends here
