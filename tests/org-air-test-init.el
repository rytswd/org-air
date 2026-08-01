;;; org-air-test-init.el --- batch bootstrap for the org-air test harness -*- lexical-binding: t; -*-

;;; Commentary:
;; Loaded first in every batch invocation (see Makefile).  Points
;; package.el at the repo-local `.deps/' directory, initialises installed
;; packages, and puts the repo root + tests/ on `load-path'.  Never
;; touches the user's ~/.emacs.d.

;;; Code:

(defconst org-air-test-root
  (expand-file-name
   (or (and load-file-name
            (locate-dominating-file load-file-name "Makefile"))
       default-directory))
  "Root of the org-air checkout.")

;; R98 -- PIN THE HARNESS'S TEXT ENCODING BEFORE ANYTHING IS LOADED.
;;
;; org-air's sources, its test files and every byte golden under
;; tests/fixtures/ are UTF-8, and the renderer's whole vocabulary is
;; multibyte (the box rules, the priority squares, the chrome middle dot,
;; the fold-row ellipsis).  Emacs picks the coding system for `load' and
;; `insert-file-contents' from the AMBIENT LOCALE, so under a non-UTF-8
;; locale (`LANG=C', a bare cron/CI shell, a container with no locale
;; archive) the very same sources decode as latin-1: every glyph turns to
;; mojibake and 27 byte-golden tests fail on pristine code.  Measured, not
;; guessed -- `LANG=C make check' was 27 unexpected before this line and 0
;; after it, while a UTF-8 locale is unaffected (the pin is a no-op there).
;;
;; A gate whose verdict depends on the caller's environment variables is
;; not a gate, so the harness states the encoding it needs instead of
;; inheriting one.
(prefer-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(setq locale-coding-system 'utf-8-unix)

(setq package-user-dir (expand-file-name ".deps" org-air-test-root))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(add-to-list 'load-path org-air-test-root)
(add-to-list 'load-path (expand-file-name "tests" org-air-test-root))

(provide 'org-air-test-init)
;;; org-air-test-init.el ends here
