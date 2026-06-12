;;; org-air-lint-baseline.el --- accepted lint findings -*- lexical-binding: t; -*-

;;; Commentary:
;; Baseline of accepted lint findings (regexps matched against
;; "checkdoc FILE: TEXT" / "package-lint FILE: TYPE TEXT" strings).
;; Owned by the impl track to burn down: fixing an issue makes its entry
;; STALE, which fails `make lint' until the entry is deleted here.
;; Recorded 2026-06-12 on RC1 (ykymmmuy): 6 checkdoc + 7 package-lint.

;;; Code:

(defconst org-air-lint-baseline
  '(;; checkdoc — docstring nits.
    "checkdoc org-air-classify\\.el: Argument ‘now’ should appear"
    "checkdoc org-air-faces\\.el: Name emacs should appear capitalized"
    "checkdoc org-air-faces\\.el: Lisp symbol ‘org-agenda’ should appear in quotes"
    "checkdoc org-air-view\\.el: Argument ‘face’ should appear"
    "checkdoc org-air-view\\.el: Argument ‘active’ should appear"
    "checkdoc org-air-view\\.el: Argument ‘attentionp’ should appear"
    ;; package-lint — multi-file package metadata.
    "package-lint org-air-calendar\\.el: error Package-Requires outside the main file"
    "package-lint org-air-classify\\.el: error Package-Requires outside the main file"
    "package-lint org-air-faces\\.el: error Package-Requires outside the main file"
    "package-lint org-air-inbox\\.el: error Package-Requires outside the main file"
    "package-lint org-air-query\\.el: error Package-Requires outside the main file"
    "package-lint org-air-view\\.el: error Package-Requires outside the main file"
    "package-lint org-air\\.el: error Package should have a Homepage or URL header")
  "Accepted lint findings; see Commentary for the burn-down contract.")

(provide 'org-air-lint-baseline)
;;; org-air-lint-baseline.el ends here
