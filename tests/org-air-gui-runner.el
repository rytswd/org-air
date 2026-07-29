;;; org-air-gui-runner.el --- the third gate mode: ERT under a real display -*- lexical-binding: t; -*-

;;; Commentary:
;; INSTRUMENT B-2, half one — `make check-gui'.
;;
;; `noninteractive' is t in 100% of `make check', and the eight
;; `(skip-unless (display-graphic-p))' tests in this suite have NEVER
;; been executed once in this project's history.  They are not rot: given
;; a frame, seven of them pass and the eighth skips on its own (perf)
;; condition.  They have simply never been RUN.
;;
;;   "That is a one-line Makefile away."      — REPORT-r96.md, §0
;;
;; It is a little more than one line, because of a trap the reviewer hit
;; and warned about:
;;
;;   "A naive `emacs --display=X -l script.el' does NOT work, because
;;    idle timers cannot fire while `-l' is still loading.  You must
;;    schedule through `run-with-timer' and let Emacs reach its command
;;    loop.  I got a false 'the board never loads' result before I
;;    noticed this; do not build the harness the naive way."
;;
;; So this file does NOT run anything at load time.  It ARMS a
;; `run-with-timer' one-shot and returns; `-l' finishes, Emacs reaches
;; the command loop, the frame is mapped and real, idle timers are live,
;; and only then does ERT start.  A second timer is a hard watchdog so
;; `make check-gui' can never hang a CI job.
;;
;; The other output problem: outside `--batch', `message' goes to the
;; echo area and *Messages*, not to stdout — a GUI ERT run is silent from
;; the shell's point of view.  So `message' is wrapped for the duration
;; of the run and every line is mirrored to `external-debugging-output'
;; (real stderr, GUI or not) and to a report file.
;;
;; WHICH tests run: by default the display-gated ones, discovered by
;; STATICALLY scanning tests/*-test.el for `ert-deftest' forms mentioning
;; `display-graphic-p' — so a display-gated test added next round is
;; picked up without editing this file.  Override with
;;
;;   make check-gui GUI_SELECTOR='org-air-r16'      # a regexp
;;   make check-gui GUI_SELECTOR='.*'               # the WHOLE suite, GUI
;;
;; This target is deliberately NOT part of `make check'.  It needs a
;; display the CI or the user may not have; a gate that cannot run
;; everywhere is a gate people learn to ignore.  Exit codes: 0 all
;; passed, 1 an ERT failure, 2 no graphic display (it refuses to report a
;; green run that proves nothing), 3 the watchdog fired.
;;
;; FIRST EXECUTION, EVER (Emacs 30.2, X, DISPLAY=:1, frame 80x58):
;; 7 display-gated tests found, 7 PASSED, 0 unexpected, 0.68s.  What they
;; actually assert, now that they finally run:
;;
;;   org-air-r16-d3-pane-window-is-board-split-and-reachable
;;     the entry pane SPLITS the board window
;;     (`display-buffer-below-selected') rather than becoming a
;;     frame-level side window (`window-side' is nil) — which is exactly
;;     what stops it shortening the rail; it carries the `org-air-pane'
;;     parameter, is dedicated, and stays `other-window'-reachable.
;;   org-air-r16-d3-pane-survives-delete-other-windows
;;     `C-x 1' does not kill the pane (`no-delete-other-windows').
;;   org-air-r16-d3-pane-coexists-with-rail-side-window
;;     pane at the bottom and rail at the right coexist on one frame.
;;   org-air-r19-3-pane-splits-board-rail-height-intact
;;     opening the pane leaves the rail side window its FULL frame-body
;;     height — the reported "pane disturbs the rail" bug, pinned cured.
;;   org-air-r34-1-real-frame-body-width-bound
;;     on a real frame with fringes + a right scroll bar sized to
;;     `window-body-width' 191 (and other widths), the composed header
;;     and EVERY row have `string-width' <= `window-body-width', and
;;     `org-air-layout--usable-columns' <= `window-body-width' — the
;;     assertion that failed when `window-max-chars-per-line' returned
;;     body+1 on a font whose average advance is under the canonical cell.
;;   org-air-r37-1-right-scrollbar-window-usable-is-body-1
;;     with a RIGHT scroll bar, `org-air-layout--usable-columns' returns
;;     body-1 — the last body column is reserved, not claimed.
;;   org-air-r38-1-banner-row-pixel-fits-gui
;;     the banner row's MEASURED `window-text-pixel-size' does not exceed
;;     the window's text-area pixel width.  Not a column count: real
;;     pixels, real font, real frame.
;;
;; Five of the seven are window-MANAGEMENT assertions (side windows,
;; dedication, `delete-other-windows' resistance, split direction) and
;; two are PIXEL assertions.  Neither class can be simulated in batch,
;; and both are the R91-92 axis — the viewport — that has already
;; produced two real defects.
;;
;; The 8th historically-skipped test, `org-air-r20-6-warm-rerender-
;; under-ceiling', is NOT display-gated: it is a bench behind
;; `ORG_AIR_BENCH' and correctly stays skipped here.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar org-air-gui-report-file
  (or (getenv "ORG_AIR_GUI_REPORT") "/tmp/org-air-gui-report.txt")
  "Where the GUI run's transcript is written.")

(defvar org-air-gui-timeout
  (string-to-number (or (getenv "ORG_AIR_GUI_TIMEOUT") "600"))
  "Hard watchdog, in seconds: `make check-gui' may never hang a job.")

(defvar org-air-gui--lines nil
  "Accumulated transcript lines.")

(defun org-air-gui--emit (string)
  "Mirror STRING to real stderr and to the transcript."
  (push string org-air-gui--lines)
  (princ (concat string "\n") #'external-debugging-output))

(defun org-air-gui--flush (exit)
  "Write the transcript, then `kill-emacs' with EXIT."
  (ignore-errors
    (with-temp-file org-air-gui-report-file
      (insert (mapconcat #'identity (reverse org-air-gui--lines) "\n") "\n")))
  (kill-emacs exit))

(defun org-air-gui--display-gated-body-p (text)
  "Non-nil when TEXT contains a `skip-unless' form mentioning `display-graphic-p'.
The form is READ, not pattern-matched, so
`(skip-unless (and (locate-library \"org-air\") (display-graphic-p)))'
counts exactly like the bare shape — while a test that merely `cl-letf's
`display-graphic-p' to t (a batch-safe GUI simulation, of which this
suite has many) correctly does not."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let (found)
      (while (and (not found) (search-forward "(skip-unless" nil t))
        (let* ((start (match-beginning 0))
               (end (ignore-errors
                      (save-excursion (goto-char start) (forward-sexp) (point)))))
          (when (and end (string-match-p
                          "display-graphic-p"
                          (buffer-substring-no-properties start end)))
            (setq found t))))
      found)))

(defun org-air-gui-display-gated-tests ()
  "Return every `ert-deftest' name gated on a graphic display.
A static scan of the test sources, so the set is discovered rather than
hard-coded: a display-gated test added in a later round is run by this
target without an edit here."
  (let ((root (if (boundp 'org-air-test-root) org-air-test-root default-directory))
        (names nil))
    (dolist (file (sort (file-expand-wildcards
                         (expand-file-name "tests/*-test.el" root))
                        #'string<))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "^(ert-deftest[ \t\n]+\\([^ \t\n()]+\\)" nil t)
          (let* ((name (match-string 1))
                 (start (match-beginning 0))
                 (end (or (ignore-errors
                            (save-excursion (goto-char start) (forward-sexp)
                                            (point)))
                          (point-max))))
            (when (org-air-gui--display-gated-body-p
                   (buffer-substring-no-properties start end))
              (push (intern name) names))))))
    (nreverse names)))

(defun org-air-gui-selector ()
  "The ERT selector for this run: $GUI_SELECTOR, else the display-gated set."
  (let ((env (getenv "GUI_SELECTOR")))
    (if (and env (not (string-empty-p env)))
        env
      (let ((tests (org-air-gui-display-gated-tests)))
        (if tests (cons 'member tests) "\\`org-air-nothing-matches\\'")))))

(defun org-air-gui-run ()
  "Run ERT under the live display and exit.
Called from a `run-with-timer' one-shot, never from `-l': idle timers
cannot fire while a file is still loading, and half the behaviour worth
testing on a display lives behind one."
  (let ((exit 1))
    (unwind-protect
        (condition-case err
            (let* ((selector (org-air-gui-selector))
                   (frame (selected-frame)))
              (org-air-gui--emit
               (format "org-air check-gui: display=%S window-system=%S \
graphic=%S frame=%dx%d emacs=%s"
                       (getenv "DISPLAY") window-system (display-graphic-p)
                       (frame-width frame) (frame-height frame)
                       emacs-version))
              (unless (display-graphic-p)
                (org-air-gui--emit
                 "org-air check-gui: NO GRAPHIC DISPLAY — refusing to \
report a green run that proves nothing.  Set DISPLAY (or run under Xvfb).")
                (org-air-gui--flush 2))
              (org-air-gui--emit
               (format "org-air check-gui: selector = %S" selector))
              (let* ((stats
                      (cl-letf* ((orig (symbol-function 'message))
                                 ((symbol-function 'message)
                                  (lambda (fmt &rest args)
                                    (when fmt
                                      (org-air-gui--emit
                                       (apply #'format fmt args)))
                                    (apply orig fmt args))))
                        (ert-run-tests-batch selector)))
                     (unexpected (ert-stats-completed-unexpected stats)))
                (setq exit (if (zerop unexpected) 0 1))))
          (error
           (org-air-gui--emit (format "org-air check-gui: ERROR %S" err))
           (setq exit 1)))
      (org-air-gui--flush exit))))

;; Arm, do not run.  This is the whole point of the file: `-l' must
;; RETURN so Emacs reaches its command loop with a real, mapped frame.
(run-with-timer 1 nil #'org-air-gui-run)
(run-with-timer org-air-gui-timeout nil
                (lambda ()
                  (org-air-gui--emit
                   (format "org-air check-gui: TIMEOUT after %ss"
                           org-air-gui-timeout))
                  (org-air-gui--flush 3)))

(provide 'org-air-gui-runner)
;;; org-air-gui-runner.el ends here
