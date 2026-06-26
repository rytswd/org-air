;;; org-air-round20-test.el --- round-20 substantive suite for org-air -*- lexical-binding: t; -*-

;;; Commentary:
;; Spec-true tests for v0.5 round-20 (air/v0.5/org-air-round20-design.org):
;; stability + polish + performance to make org-air dogfoodable.
;;
;;   R20-1  async first load HANGS -> a SYNCHRONOUS fast-paint load.  The
;;          chained-idle-timer path is gone; the cold open paints the
;;          chrome, forces it visible with `redisplay', then queries +
;;          renders inline, wrapped so a query error can never wedge the
;;          board (`--loading' is always cleared) nor dump a six-figure
;;          echo message (the bounded `--short-error' line).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)
(require 'org-air)

;;;; ---------------------------------------------------------------------
;;;; R20-1 — synchronous fast-paint first load (drop the idle-timer chain).
;;;; ---------------------------------------------------------------------

(ert-deftest org-air-r20-1-cold-load-sync-renders-board ()
  "A cold (non-cached) `org-air-view' renders the FULL board synchronously
and ends with `org-air-view--loading' nil — no idle-timer chain, no
half-painted skeleton left behind."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (org-air-viewport-test--with-frozen-now
      (let ((org-air-view-width 120)
            (org-air-view-height 50)
            (org-air-view-buffer-name "*org-air-r20-1-sync*"))
        (unwind-protect
            (progn
              (org-air-view)
              (with-current-buffer org-air-view-buffer-name
                (should-not org-air-view--loading)
                (should org-air-view--items)
                (let ((text (substring-no-properties (buffer-string))))
                  (should-not (string-match-p "Loading your board" text))
                  (should (string-match-p "Ship quarterly report" text)))))
          (when (get-buffer org-air-view-buffer-name)
            (kill-buffer org-air-view-buffer-name)))))))

(ert-deftest org-air-r20-1-cold-load-error-does-not-wedge ()
  "A query error in the COLD interactive load can never wedge the board: the
`unwind-protect' clears `org-air-view--loading', the buffer falls back to
the normal empty render (NOT the loading skeleton), and the surfaced echo
message is a single bounded line (< 200 chars) — locking out the
101 802-char `%S'-of-payload timer-error dump."
  (skip-unless (locate-library "org-air"))
  (org-air-test-with-fixtures
    (org-air-viewport-test--with-frozen-now
      (let ((org-air-view-width 120)
            (org-air-view-height 50)
            (org-air-view-buffer-name "*org-air-r20-1-wedge*")
            (captured nil)
            ;; a realistic org-ql failure carrying a HUGE data payload (the
            ;; exact shape that made `%S' / `error-message-string' explode).
            (big (make-list 2000 (list :title "x" :tags '("a" "b" "c")))))
        (unwind-protect
            (cl-letf (((symbol-function 'org-air-query-items)
                       (lambda (&rest _)
                         (signal 'error (list "org-ql query failed" big))))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (setq captured (apply #'format fmt args))
                         captured)))
              ;; force the COLD interactive branch (batch normally takes the
              ;; synchronous cache-miss branch).
              (let ((noninteractive nil))
                (org-air-view))
              (with-current-buffer org-air-view-buffer-name
                ;; (a) never wedged:
                (should-not org-air-view--loading)
                ;; (b) the empty board, not the skeleton:
                (let ((text (substring-no-properties (buffer-string))))
                  (should-not (string-match-p "Loading your board" text)))
                ;; the failed query left no stale items:
                (should (null org-air-view--items)))
              ;; (c) the message is bounded and human (no payload dump):
              (should captured)
              (should (string-prefix-p "org-air: load failed:" captured))
              (should (< (length captured) 200)))
          (when (get-buffer org-air-view-buffer-name)
            (kill-buffer org-air-view-buffer-name)))))))

(ert-deftest org-air-r20-1-short-error-truncates-huge-payload ()
  "`org-air-view--short-error' returns a bounded single line even for an
error whose `error-message-string' is six figures long."
  (skip-unless (locate-library "org-air"))
  (let* ((big (make-list 4000 (list :a 1 :b 2)))
         (err (condition-case e
                  (signal 'error (list "boom" big))
                (error e)))
         (short (org-air-view--short-error err)))
    ;; the raw message is enormous; the short form is capped + single line.
    (should (> (length (error-message-string err)) 10000))
    (should (<= (length short) 161))
    (should-not (string-match-p "\n" short))))

(provide 'org-air-round20-test)
;;; org-air-round20-test.el ends here
