;;; org-air.el --- Modern org planning dashboard -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: org-air contributors
;; Keywords: outlines, calendar
;; Package-Requires: ((emacs "29.1") (org "9.6") (org-ql "0.8"))
;; Version: 0.1.0
;; URL: https://github.com/rytswd/org-air
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; org-air provides a compact org-ql-backed dashboard and inbox workflow for
;; Org files, without using org-agenda internals.

;;; Code:

(require 'org)

(defgroup org-air nil
  "Modern org planning dashboard."
  :group 'org
  :prefix "org-air-")

(defcustom org-air-files nil
  "List of Org files or directories scanned by org-air."
  :type '(repeat (choice file directory))
  :group 'org-air)

(defcustom org-air-exclude-regexps nil
  "Regexps of files and directories org-air must never scan.
Each regexp is matched (case-sensitively) against the ABSOLUTE path:
a candidate FILE matches as-is (e.g. \"/home/u/org/notes/noise.org\");
a DIRECTORY is matched in directory-name form with a trailing slash
\(e.g. \"/home/u/org/archive/\"), and a matching directory is pruned —
never descended — so \"/archive/\" or \"\\\\.git/\" silences a whole
tree cheaply.  Matching wins over an explicit `org-air-files' listing.
The one exemption: `org-air-inbox-file' is never excluded.
To exclude one exact file, anchor a quoted path:
  (concat \"\\\\\=`\" (regexp-quote (expand-file-name \"~/org/big.org\")) \"\\\\\='\")"
  :type '(repeat regexp)
  :group 'org-air)

(defcustom org-air-inbox-file nil
  "Org file where `org-air-capture' stores new inbox items.
nil (the default) DERIVES the inbox from `org-air-files' — see
`org-air-inbox-effective-file' — so a capture always lands inside the
corpus the board scans and always shows up on the board.

R97 D2: before this the default was `org-air-inbox.org' under
`user-emacs-directory', which is NOT inside `org-air-files' unless the
user puts it there.  Following the README's plain-`require' snippet
\(`org-air-files' only) therefore wrote every capture to a file the scan
never read: the note was on disk and the board reported Inbox zero.  A
derived default cannot do that, and an explicit value that lies outside
the scan set is REFUSED by `org-air-capture' rather than silently lost.

Set this explicitly to pin the inbox; it must be inside `org-air-files'."
  :type '(choice (const :tag "Derive from `org-air-files'" nil) file)
  :group 'org-air)

(require 'org-air-query)
(require 'org-air-classify)
(require 'org-air-view)
(require 'org-air-project)
(require 'org-air-revisit)
(require 'org-air-review)

;;;###autoload
(defun org-air ()
  "Open or refresh the org-air dashboard."
  (interactive)
  (org-air-view))

;;;###autoload
(autoload 'org-air-capture "org-air-inbox" nil t)
;;;###autoload
(autoload 'org-air-refile-item "org-air-inbox" nil t)

(provide 'org-air)
;;; org-air.el ends here
