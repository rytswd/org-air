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

(defcustom org-air-inbox-file (locate-user-emacs-file "org-air-inbox.org")
  "Org file where `org-air-capture' stores new inbox items."
  :type 'file
  :group 'org-air)

(require 'org-air-query)
(require 'org-air-classify)
(require 'org-air-view)

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
