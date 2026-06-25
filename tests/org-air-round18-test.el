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

(provide 'org-air-round18-test)
;;; org-air-round18-test.el ends here
