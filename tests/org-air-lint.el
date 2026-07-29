;;; org-air-lint.el --- batch lint gate: checkdoc + package-lint + dupdef -*- lexical-binding: t; -*-

;;; Commentary:
;; Run via `make lint'.  Lints every org-air*.el with checkdoc,
;; package-lint and the R96 DUPLICATE-DEFINITION rule, and compares the
;; findings against the accepted baseline in tests/org-air-lint-baseline.el.
;; Same self-policing contract as the known-failures manifest:
;;   - finding matched by a baseline entry -> accepted
;;   - finding NOT in the baseline         -> FAIL (new lint issue)
;;   - baseline entry matching nothing     -> FAIL (stale entry: the
;;     issue was fixed, delete the entry as closeout)
;; So `make lint' is binary and the baseline can only shrink honestly.
;;
;; `package-lint-main-file' is set to org-air.el so the multi-file
;; package is linted as one package (sub-file prefix/header noise is
;; thereby suppressed; genuine issues still surface).

;;; Code:

(require 'checkdoc)
(require 'package-lint)
(require 'org-air-lint-baseline)

(defconst org-air-lint--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root of the org-air checkout (parent of tests/).")

(defun org-air-lint--files ()
  "Return all org-air source files to lint."
  (directory-files org-air-lint--root t "^org-air.*\\.el\\'"))

;;;; ---------------------------------------------------------------------
;;;; R96: the duplicate-definition rule
;;;;
;;;; Byte compilation CANNOT see this class of defect.  `cl-defstruct'
;;;; installs a COMPILER MACRO for every accessor, so a static
;;;; `(org-air-item-deadline item)' call site is inlined to an `aref'
;;;; whether the tree is compiled OR interpreted — and keeps working even
;;;; when a `defun' elsewhere in the package has taken over that symbol's
;;;; FUNCTION CELL.  Everything that reaches the function cell instead —
;;;; `funcall', `apply', `mapcar'/`seq-map' with a sharp-quote, a hook, a
;;;; late `eval' — silently gets the OTHER definition.  R68 hit this with
;;;; the `todo' slot, R96 with the `deadline' slot.  This rule closes the
;;;; class: a package symbol may be defined at most ONCE per namespace.
;;;; ---------------------------------------------------------------------

(defconst org-air-lint--function-definers
  '(defun defmacro defsubst cl-defun cl-defmacro cl-defgeneric define-inline
     define-minor-mode define-derived-mode define-globalized-minor-mode)
  "Forms whose second element names a FUNCTION-namespace definition.
`cl-defmethod' is deliberately absent: many methods per generic is the
point of it.  `defalias' and `cl-defstruct' are handled specially.")

(defconst org-air-lint--variable-definers
  '(defvar defvar-local defconst defcustom)
  "Forms whose second element names a VARIABLE-namespace definition.
A bare `(defvar SYM)' is a forward DECLARATION, not a definition, and is
filtered out by `org-air-lint--scan-form' — the package uses dozens of
them to satisfy the byte-compiler across module boundaries.")


(defun org-air-lint--struct-slot-name (spec)
  "Return the slot symbol named by cl-defstruct slot SPEC."
  (if (consp spec) (car spec) spec))

(defun org-air-lint--struct-function-names (form)
  "Return the function-namespace symbols the `cl-defstruct' FORM defines.
Accessors (`:conc-name' honoured), the predicate, the copier and every
constructor.  A `nil' option value means NOT GENERATED and is skipped;
naming any `:constructor' suppresses the default `make-NAME'."
  (let* ((head (nth 1 form))
         (name (if (consp head) (car head) head))
         (opts (and (consp head) (cdr head)))
         (conc (format "%s-" name))
         (pred (intern (format "%s-p" name)))
         (copier (intern (format "copy-%s" name)))
         (ctors nil)
         (named-ctor nil)
         (body (cddr form))
         (names nil))
    (dolist (opt opts)
      (when (consp opt)
        (pcase (car opt)
          (:conc-name (setq conc (if (cadr opt) (format "%s" (cadr opt)) "")))
          (:predicate (setq pred (cadr opt)))
          (:copier (setq copier (cadr opt)))
          (:constructor (setq named-ctor t)
                        (when (cadr opt) (push (cadr opt) ctors))))))
    (unless named-ctor
      (push (intern (format "make-%s" name)) ctors))
    ;; Drop the struct docstring, then map the slot specs to accessors.
    (when (stringp (car body)) (setq body (cdr body)))
    (dolist (spec body)
      (let ((slot (org-air-lint--struct-slot-name spec)))
        (when (symbolp slot)
          (push (intern (format "%s%s" conc slot)) names))))
    (append names (and pred (list pred)) (and copier (list copier)) ctors)))

(defun org-air-lint--struct-accessor-names (form)
  "Return only the ACCESSOR symbols the `cl-defstruct' FORM defines."
  (let* ((head (nth 1 form))
         (name (if (consp head) (car head) head))
         (opts (and (consp head) (cdr head)))
         (conc (format "%s-" name))
         (body (cddr form))
         (names nil))
    (dolist (opt opts)
      (when (and (consp opt) (eq (car opt) :conc-name))
        (setq conc (if (cadr opt) (format "%s" (cadr opt)) ""))))
    (when (stringp (car body)) (setq body (cdr body)))
    (dolist (spec body)
      (let ((slot (org-air-lint--struct-slot-name spec)))
        (when (symbolp slot)
          (push (intern (format "%s%s" conc slot)) names))))
    names))

(defun org-air-lint--scan-form (form file line acc)
  "Record the definitions FORM makes into ACC, a list cell of records.
FILE and LINE locate the enclosing top-level form.  ACC is a cons whose
car accumulates records (SYMBOL NAMESPACE KIND FILE LINE).  Recurses
through bodies (`eval-and-compile', `when', `progn', a sharp-quoted
lambda …) but never into `quote'/backquote DATA, so quoted lists that
merely LOOK like definitions are not counted."
  (when (consp form)
    (let ((head (car form)))
      (cond
       ((memq head '(quote \`)) nil)         ; data, not code — stop here
       (t
        (cond
         ((and (memq head org-air-lint--function-definers)
               (symbolp (nth 1 form)) (nth 1 form))
          (push (list (nth 1 form) 'function head file line) (car acc)))
         ((and (eq head 'defalias)
               (consp (nth 1 form)) (eq (car (nth 1 form)) 'quote))
          (push (list (cadr (nth 1 form)) 'function 'defalias file line)
                (car acc)))
         ((eq head 'cl-defstruct)
          (let ((accessors (org-air-lint--struct-accessor-names form)))
            (dolist (sym (org-air-lint--struct-function-names form))
              (push (list sym 'function
                          (if (memq sym accessors)
                              'struct-accessor
                            'cl-defstruct)
                          file line)
                    (car acc)))))
         ((and (memq head org-air-lint--variable-definers)
               (symbolp (nth 1 form)) (nth 1 form)
               ;; `(defvar SYM)' with no value is a forward declaration.
               (or (not (eq head 'defvar)) (cddr form)))
          (push (list (nth 1 form) 'variable head file line) (car acc))))
        ;; Recurse into every sub-form (a definition can be wrapped).
        (let ((tail form))
          (while (consp tail)
            (org-air-lint--scan-form (car tail) file line acc)
            (setq tail (cdr tail)))))))))

(defun org-air-lint--definitions (files)
  "Return every definition record found in FILES.
Each record is (SYMBOL NAMESPACE KIND FILE LINE)."
  (let ((acc (list nil)))
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((short (file-name-nondirectory file)))
          (condition-case nil
              (while t
                (let* ((start (point))
                       (form (read (current-buffer))))
                  (org-air-lint--scan-form
                   form short (line-number-at-pos start) acc)))
            (end-of-file nil)))))
    (nreverse (car acc))))

(defun org-air-lint--collect-duplicate-definitions (files report)
  "Report every package symbol defined twice across FILES (R96).
Calls REPORT with one finding string per offending symbol.  A struct
accessor redefined by anything else is called out by name: that is the
R68/R96 landmine — the `cl-defstruct' compiler macro keeps every static
call site working while the function cell holds the OTHER definition, so
neither the compiled nor the interpreted gate can see it."
  (let ((table (make-hash-table :test #'equal))
        (order nil))
    (dolist (rec (org-air-lint--definitions files))
      (let ((key (list (nth 0 rec) (nth 1 rec))))
        (unless (gethash key table) (push key order))
        (puthash key (cons rec (gethash key table)) table)))
    (dolist (key (nreverse order))
      (let ((recs (nreverse (gethash key table))))
        (when (cdr recs)
          (let* ((sym (nth 0 key))
                 (ns (nth 1 key))
                 (accessorp (seq-find (lambda (r) (eq (nth 2 r) 'struct-accessor))
                                      recs))
                 (sites (mapconcat
                         (lambda (r) (format "%s at %s:%d"
                                             (nth 2 r) (nth 3 r) (nth 4 r)))
                         recs ", ")))
            (funcall report
                     (format "dupdef %s: `%s' has %d %s-namespace definitions%s (%s)"
                             (nth 3 (car recs)) sym (length recs) ns
                             (if accessorp
                                 " — one is a cl-defstruct ACCESSOR whose function cell is therefore NOT the accessor"
                               "")
                             sites))))))))

(defun org-air-lint--collect-checkdoc (file report)
  "Run checkdoc on FILE, calling REPORT with each finding string."
  (let ((checkdoc-create-error-function
         (lambda (text _start _end &optional _unfixable)
           (funcall report (format "checkdoc %s: %s"
                                   (file-name-nondirectory file) text))
           nil)))
    (checkdoc-file file)))

(defun org-air-lint--collect-package-lint (file report)
  "Run package-lint on FILE, calling REPORT with each finding string."
  (let ((package-lint-main-file (expand-file-name "org-air.el"
                                                  org-air-lint--root)))
    (with-temp-buffer
      (insert-file-contents file t)
      (emacs-lisp-mode)
      (dolist (issue (package-lint-buffer))
        (funcall report (format "package-lint %s: %s %s"
                                (file-name-nondirectory file)
                                (nth 2 issue) (nth 3 issue)))))))

(defun org-air-lint-batch ()
  "Lint all org-air sources against the baseline; exit non-zero on failure."
  (let ((findings nil))
    (let ((report (lambda (s) (push s findings))))
      (dolist (file (org-air-lint--files))
        (org-air-lint--collect-checkdoc file report)
        (org-air-lint--collect-package-lint file report))
      ;; R96: package-WIDE rule — runs once over the whole file set.
      (org-air-lint--collect-duplicate-definitions (org-air-lint--files)
                                                   report))
    (setq findings (nreverse findings))
    (let* ((unmatched-baseline (copy-sequence org-air-lint-baseline))
           (new nil))
      (dolist (finding findings)
        (let ((hit (seq-find (lambda (entry) (string-match-p entry finding))
                             unmatched-baseline)))
          (if hit
              (setq unmatched-baseline (delete hit unmatched-baseline))
            (push finding new))))
      (setq new (nreverse new))
      (dolist (f new) (message "lint: NEW finding: %s" f))
      (dolist (b unmatched-baseline)
        (message "lint: STALE baseline entry (issue fixed — delete it): %s" b))
      (if (or new unmatched-baseline)
          (progn
            (message "lint: FAIL (%d new, %d stale baseline)"
                     (length new) (length unmatched-baseline))
            (kill-emacs 1))
        (message "lint: ok (%d findings, all baselined)" (length findings))))))

(provide 'org-air-lint)
;;; org-air-lint.el ends here
