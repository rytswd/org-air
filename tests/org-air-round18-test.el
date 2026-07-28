;;; org-air-round18-test.el --- round-18 D-P1 perf-cache tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for round-18 D-P1 (air/v0.5/org-air-round18-design.org): the three
;; internal perf optimisations that keep the V6 pixel-lock + byte/TTY
;; contract byte-identical (perf is INTERNAL — every byte fixture must still
;; pass, so the proof here is deterministic CALL-COUNT and UNIT assertions,
;; never wall-clock):
;;   (c) classify cache — classify each item once per render, zero on a
;;       TAB-expand / month-nav re-render (pure cache hits);
;;   (a) svg pill image cache (added with the second commit);
;;   (b) incremental render (added with the third commit).
;; The clock is frozen to `org-air-test-now' so the day-granular cache key
;; is constant across the re-renders under test.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)
(require 'org-air)
(require 'org-air-view)
(require 'org-air-classify)

(defmacro org-air-r18--frozen (&rest body)
  "Run BODY with `current-time' frozen to `org-air-test-now' (R18)."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'current-time)
              (lambda () org-air-test-now)))
     ,@body))

(defmacro org-air-r18--counting-classify (counter &rest body)
  "Run BODY with each `org-air-classify-item' call tallied into COUNTER.
COUNTER is a place (e.g. a let-bound variable) incremented per real call;
the original classify behaviour is preserved."
  (declare (indent 1) (debug t))
  (let ((orig (make-symbol "orig")))
    `(let ((,orig (symbol-function 'org-air-classify-item)))
       (cl-letf (((symbol-function 'org-air-classify-item)
                  (lambda (&rest args)
                    (setq ,counter (1+ ,counter))
                    (apply ,orig args))))
         ,@body))))

;;;; ---------------------------------------------------------------------
;;;; (c) classify cache — classify once per render, zero on re-render.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r18-classify-cache-once-per-render ()
  "A full render classifies each item AT MOST ONCE (not 5×N).
The old behaviour ran `org-air-classify-item' five times per item (once
per section bucket) every render; the cache funnels every call site through
`org-air-view--classify-cached', so the per-render real-call tally never
exceeds the number of items."
  (org-air-test-with-fixtures
    (let ((org-air-view-width 140)
          (org-air-view-height 40)
          (classify-calls 0))
      (with-temp-buffer
        (org-air-view-mode)
        (org-air-r18--frozen
          (setq org-air-view--items (org-air-query-items))
          (let ((n (length org-air-view--items)))
            (should (> n 0))
            (org-air-r18--counting-classify classify-calls
              (org-air-view--render org-air-view--items nil))
            ;; Each item classified at most once — NOT 5×N.
            (should (> classify-calls 0))
            (should (<= classify-calls n))))))))

(ert-deftest org-air-r18-classify-cache-zero-on-rerender ()
  "A TAB-expand and a month-nav add ZERO further classify calls.
They reuse the same `org-air-view--items' (same `eq' objects) at the same
frozen day, so every classify is a pure cache hit — the regression net for
\"classify ran 5×N on every interaction\"."
  (org-air-test-with-fixtures
    (let ((org-air-view-width 140)
          (org-air-view-height 40)
          (classify-calls 0))
      (with-temp-buffer
        (org-air-view-mode)
        (org-air-r18--frozen
          (setq org-air-view--items (org-air-query-items))
          (org-air-r18--counting-classify classify-calls
            ;; First render warms the cache.
            (org-air-view--render org-air-view--items nil)
            (let ((after-first classify-calls))
              (should (> after-first 0))
              ;; TAB-expand a section: position point on the first section
              ;; heading, then toggle.  Re-render must be a pure cache hit.
              (let ((pos (org-air-view--find-property
                          'org-air-section 'attention)))
                (when pos (goto-char pos))
                (org-air-toggle-section))
              (should (= classify-calls after-first))
              ;; Month-nav: pages the calendar, re-renders the board, all
              ;; cache hits.
              (org-air-calendar-next)
              (should (= classify-calls after-first)))))))))

(ert-deftest org-air-r18-classify-cached-memoises-and-day-rolls ()
  "`org-air-view--classify-cached' memoises per item and rebuilds on a new day.
Two calls with the same NOW make ONE real `org-air-classify-item' call; a
NOW on a different calendar day recomputes (day-granular key)."
  (org-air-test-with-fixtures
    (with-temp-buffer
      (org-air-view-mode)
      (let* ((items (org-air-query-items))
             (item (car items))
             (now org-air-test-now)
             (calls 0))
        (should item)
        (org-air-r18--counting-classify calls
          (let ((a (org-air-view--classify-cached item now))
                (b (org-air-view--classify-cached item now)))
            ;; Same item + same day -> exactly one real classify.
            (should (= calls 1))
            (should (equal a b))
            ;; A NOW on the NEXT day rebuilds the cache and recomputes.
            (org-air-view--classify-cached
             item (time-add now (days-to-time 1)))
            (should (= calls 2))))))))

(ert-deftest org-air-r18-classify-cache-cleared-on-refresh ()
  "`org-air-refresh' drops the classify cache table (memory bound + re-tune).
The table is set to nil on the re-query path and rebuilt by the refresh
render, so the live cache after a refresh is a DIFFERENT table object than
before — proof the `(setq org-air-view--classify-cache nil)' ran (a changed
classify-tuning defcustom would take effect on this fresh table)."
  (org-air-test-with-fixtures
    (let ((org-air-view-width 140)
          (org-air-view-height 40))
      (with-temp-buffer
        (org-air-view-mode)
        (org-air-r18--frozen
          (setq org-air-view--items (org-air-query-items))
          (org-air-view--render org-air-view--items nil)
          (let ((table-before org-air-view--classify-cache))
            (should (hash-table-p table-before))
            (cl-letf (((symbol-function 'org-air-view--save-position)
                       (lambda () nil))
                      ((symbol-function 'org-air-view--restore-position)
                       #'ignore))
              (org-air-refresh))
            ;; The refresh render rebuilt the cache, but the table object was
            ;; cleared first — so it is a fresh table, not the old one.
            (should (hash-table-p org-air-view--classify-cache))
            (should-not (eq table-before org-air-view--classify-cache))))))))

;;;; ---------------------------------------------------------------------
;;;; (a) svg pill image cache — build pixel-identical overlays once.
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r18-svg-cache-zero-rebuild-on-second-render ()
  "A second IDENTICAL render makes ZERO new `svg-image' calls (all cached).
Forces the GUI svg path (`display-graphic-p' stubbed), counts `svg-image';
the first render warms the global image cache, the second is pure hits — the
regression net for \"every pill/icon/divider rebuilt every render\".  A
simulated metric change (new char dimensions) re-keys and DOES rebuild."
  (org-air-test-with-fixtures
    (let ((org-air-view-width 140)
          (org-air-view-height 40)
          (svg-calls 0))
      (with-temp-buffer
        (org-air-view-mode)
        (org-air-viewport-test-as-gui
          (org-air-r18--frozen
            (setq org-air-view--items (org-air-query-items))
            (clrhash org-air-view--svg-image-cache)
            (let ((orig (symbol-function 'svg-image)))
              (cl-letf (((symbol-function 'svg-image)
                         (lambda (&rest args)
                           (setq svg-calls (1+ svg-calls))
                           (apply orig args))))
                ;; First render: cold cache, builds the images.
                (org-air-view--render org-air-view--items nil)
                (let ((after-first svg-calls))
                  (should (> after-first 0))
                  ;; Second identical render: every overlay is a cache hit.
                  (org-air-view--render org-air-view--items nil)
                  (should (= svg-calls after-first))
                  ;; A metric change (different char cell) re-keys -> rebuild.
                  (cl-letf (((symbol-function 'org-air-view--char-dimensions)
                             (lambda () (cons 99 199))))
                    (org-air-view--render org-air-view--items nil))
                  (should (> svg-calls after-first)))))))))))

(ert-deftest org-air-r18-svg-pillify-shares-image-object ()
  "Two `--svg-pillify' calls with identical inputs share the SAME image.
The `display' image object is `eq' across calls (built once, aliased);
differing colour/metric/style inputs produce a DIFFERENT image."
  (org-air-viewport-test-as-gui
    (clrhash org-air-view--svg-image-cache)
    (let* ((org-air-view--pill-char-w 8)
           (org-air-view--pill-char-h 16)
           (org-air-view--pill-style-sig
            (list org-air-pill-pad-cols org-air-pill-radius
                  org-air-pill-fill-alpha org-air-pill-font-scale
                  org-air-pill-border-opacity org-air-pill-vinset))
           (a (org-air-view--svg-pillify " @work " 'org-air-face-tag))
           (b (org-air-view--svg-pillify " @work " 'org-air-face-tag))
           (ia (get-text-property 0 'display a))
           (ib (get-text-property 0 'display b)))
      ;; Both produced an image overlay (GUI path), and they are the SAME
      ;; object — the wrapper strings are fresh but the image is shared.
      (should ia)
      (should (eq ia ib))
      (should-not (eq a b))
      ;; A different metric re-keys -> a DIFFERENT image object.
      (let* ((org-air-view--pill-char-w 9)
             (c (org-air-view--svg-pillify " @work " 'org-air-face-tag))
             (ic (get-text-property 0 'display c)))
        (should ic)
        (should-not (eq ia ic))))))

(ert-deftest org-air-r18-svg-cache-soft-cap-bounds-memory ()
  "The svg image cache clears itself past its soft cap (memory bound)."
  (clrhash org-air-view--svg-image-cache)
  (dotimes (i 4100)
    (org-air-view--svg-image-cached (list 'probe i) (lambda () i)))
  ;; Past 4000 entries the table clrhash'd and started over, so the live
  ;; count is well under the unbounded total.
  (should (< (hash-table-count org-air-view--svg-image-cache) 4100))
  (clrhash org-air-view--svg-image-cache))

;;;; ---------------------------------------------------------------------
;;;; (b) incremental render — splice the section / re-render only the rail.
;;;; ---------------------------------------------------------------------

(defun org-air-r18--render-board-only (expanded)
  "Render the fixtures board-only (width 80) with EXPANDED sections; return text.
Runs in the current temp buffer at the frozen clock."
  (setq org-air-view--items (org-air-query-items))
  (setq org-air-view--expanded-sections (copy-sequence expanded))
  (org-air-view--render org-air-view--items nil)
  (buffer-substring-no-properties (point-min) (point-max)))

(ert-deftest org-air-r18-section-splice-byte-equivalent-to-full-render ()
  "A TAB-expand splice yields a buffer BYTE-IDENTICAL to a full render.
The equivalence golden: the incremental section splice can NEVER diverge
from the source of truth (a full `org-air-view--render' with the same
`org-air-view--expanded-sections').

R93 re-bless: the toggled section moved from `attention' to `upcoming'.
The law is section-agnostic, but its ANTI-TAUTOLOGY guard is not: the
splice must actually CHANGE the board, which needs a section whose row
count exceeds its cap.  Under the R93 rules the standard fixture's
Needs attention holds 5 rows under a cap of 6 -- expanding it is a
no-op, so the guard would have been silently unfalsifiable.  Upcoming
holds 8 under a cap of 5 and is genuinely capped."
  (org-air-test-with-fixtures
    (let ((org-air-view-width 80)
          (org-air-view-height 40)
          full-str)
      ;; Source of truth: a full render with `upcoming' already expanded.
      (with-temp-buffer
        (org-air-view-mode)
        (org-air-r18--frozen
          (setq full-str (org-air-r18--render-board-only '(upcoming)))
          (should (eq org-air-view--orientation 'board-only))))
      ;; Incremental: render collapsed, then TAB `upcoming' (splice path).
      (with-temp-buffer
        (org-air-view-mode)
        (org-air-r18--frozen
          (let ((collapsed (org-air-r18--render-board-only nil)))
            (should (eq org-air-view--orientation 'board-only))
            (let ((pos (org-air-view--find-property 'org-air-section 'upcoming)))
              (should pos)
              (goto-char pos))
            (org-air-toggle-section)
            (should (memq 'upcoming org-air-view--expanded-sections))
            (let ((spliced (buffer-substring-no-properties
                            (point-min) (point-max))))
              ;; The splice actually changed the board…
              (should-not (equal spliced collapsed))
              ;; …and it is byte-identical to the full render.
              (should (equal spliced full-str))
              ;; Point landed back on the toggled section heading.
              (should (eq (get-text-property (point) 'org-air-section)
                          'upcoming)))))))))

(ert-deftest org-air-r18-section-splice-collapse-round-trip ()
  "Expand then collapse a section returns a buffer byte-identical to the start.
Exercises the shrink / pure-deletion splice path: after TAB-expand +
TAB-collapse on the same section the board is byte-identical to the
original collapsed full render."
  (org-air-test-with-fixtures
    (let ((org-air-view-width 80)
          (org-air-view-height 40))
      (with-temp-buffer
        (org-air-view-mode)
        (org-air-r18--frozen
          (let ((start (org-air-r18--render-board-only nil)))
            (should (eq org-air-view--orientation 'board-only))
            ;; expand
            (goto-char (org-air-view--find-property 'org-air-section 'attention))
            (org-air-toggle-section)
            (should (memq 'attention org-air-view--expanded-sections))
            ;; collapse back
            (goto-char (org-air-view--find-property 'org-air-section 'attention))
            (org-air-toggle-section)
            (should-not (memq 'attention org-air-view--expanded-sections))
            (should (equal (buffer-substring-no-properties (point-min) (point-max))
                           start))))))))

(ert-deftest org-air-r18-section-splice-leaves-header-and-earlier-sections ()
  "Toggling a section leaves the header + EARLIER sections byte-untouched.
Expanding a capped section rewrites only from its region to the end of
the body; everything above its heading is identical to the pre-toggle
buffer.

R93 re-bless — and a real strengthening.  The section was `stale', \"the
last section\", which the standard fixture renders with ZERO rows (R54-1
made every dateless fixture item stale-ineligible), so the toggle under
test changed NOTHING and the byte comparison below could not fail.
`upcoming' is capped (8 rows, cap 5) and has two sections above it, so
the splice really does rewrite a region — asserted explicitly now — and
the prefix comparison finally means something."
  (org-air-test-with-fixtures
    (let ((org-air-view-width 80)
          (org-air-view-height 40))
      (with-temp-buffer
        (org-air-view-mode)
        (org-air-r18--frozen
          (org-air-r18--render-board-only nil)
          (should (eq org-air-view--orientation 'board-only))
          (let* ((pos (org-air-view--find-property 'org-air-section 'upcoming))
                 (before (buffer-substring-no-properties (point-min) (point-max)))
                 (before-prefix (and pos
                                     (buffer-substring-no-properties
                                      (point-min) pos))))
            (should pos)
            ;; Two sections really are above it (Inbox, Overdue).
            (should (org-air-view--find-property 'org-air-section 'inbox))
            (should (< (org-air-view--find-property 'org-air-section 'overdue)
                       pos))
            (goto-char pos)
            (org-air-toggle-section)
            ;; Anti-tautology: the splice really rewrote something.
            (should-not (equal before (buffer-substring-no-properties
                                       (point-min) (point-max))))
            ;; The prefix up to the toggled heading is byte-identical (the
            ;; splice never rewrote the header or earlier sections).
            (let ((after-pos (org-air-view--find-property
                              'org-air-section 'upcoming)))
              (should after-pos)
              (should (equal (buffer-substring-no-properties
                              (point-min) after-pos)
                             before-prefix)))))))))

(ert-deftest org-air-r18-month-nav-side-window-leaves-board ()
  "Month-nav in `side-window' redraws ONLY the rail — the board is untouched.
The board buffer is byte-identical before/after `>'; only the rail is asked
to redraw, the full board render is NOT invoked, and the month advances."
  (org-air-test-with-fixtures
    (let ((org-air-view-width 140)
          (org-air-view-height 40))
      (with-temp-buffer
        (org-air-view-mode)
        (org-air-r18--frozen
          (setq-local org-air-view--rail-popped-out t)
          (setq org-air-view--items (org-air-query-items))
          (org-air-view--render org-air-view--items nil)
          (should (eq org-air-view--orientation 'side-window))
          (let ((before (buffer-substring-no-properties (point-min) (point-max)))
                (month-before org-air-view--cal-month)
                (rail-shown nil)
                (full-called nil))
            (cl-letf (((symbol-function 'org-air-rail--show)
                       (lambda (&rest _) (setq rail-shown t)))
                      ((symbol-function 'org-air-view--render-current)
                       (lambda (&rest _) (setq full-called t))))
              (org-air-calendar-next))
            ;; Board byte-identical; only the rail redrew; full render skipped.
            (should (equal (buffer-substring-no-properties (point-min) (point-max))
                           before))
            (should rail-shown)
            (should-not full-called)
            ;; The calendar month advanced by one.
            (should-not (equal org-air-view--cal-month month-before))))))))

(provide 'org-air-round18-test)
;;; org-air-round18-test.el ends here
