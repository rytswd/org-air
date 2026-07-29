;;; org-air-round96-test.el --- R96: struct function cells are the struct's -*- lexical-binding: t; -*-

;;; Commentary:
;; The DYNAMIC twin of R96's static `dupdef' lint rule
;; (tests/org-air-lint.el, `org-air-lint--collect-duplicate-definitions').
;;
;; R96 closed a compilation-masked defect: `org-air-item-deadline' was
;; BOTH the `org-air-item' struct accessor (org-air-query.el) and an
;; interactive command (org-air-view.el).  One symbol, one function cell,
;; and the COMMAND held it — `(commandp 'org-air-item-deadline)' was t.
;; Every reach that consults the function cell (`funcall', `apply',
;; `mapcar'/`seq-map' with a sharp quote, a hook, a late `eval') ran the
;; command instead of reading the slot.  Neither gate mode could see it:
;; `cl-defstruct' installs a COMPILER MACRO per accessor, and compiler
;; macros fire during macroexpansion — in the interpreter too — so every
;; STATIC call site in the tree was rewritten to an `aref' before the
;; function cell was ever consulted.  1342 tests passed, compiled and
;; interpreted, over a wrong function cell.
;;
;; The lint rule is static: it reads the sources and fails on a symbol
;; defined twice per namespace.  It cannot see a runtime `fset', a
;; `defalias' built by a macro, an advice that replaces a cell, or a name
;; assembled with `intern'.  These tests are the RUNTIME half: they ask
;; the loaded image, for EVERY slot of EVERY `cl-defstruct' in the
;; package, whether the accessor's function cell still IS the accessor —
;;
;;   (not (commandp ACCESSOR))
;;   (equal (funcall ACCESSOR obj) (cl-struct-slot-value STRUCT SLOT obj))
;;
;; …and the same for every generated constructor, predicate and copier.
;;
;; The struct SET is discovered by scanning the sources for `cl-defstruct'
;; forms, so a struct added in a later round is covered without an edit
;; here; the ASSERTIONS are made against the live image, so a collision
;; introduced by any means at all reddens them.
;;
;; RED on the pre-fix tree (R95/`main'): `org-air-item-deadline' is
;; `commandp' there, and funcalling it signals `user-error "No org-air
;; item at point"' instead of returning the slot.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(when (locate-library "org-air")
  (require 'org-air))

;;;; -------------------------------------------------------------------
;;;; Discovery — every `cl-defstruct' in the 11-module package
;;;; -------------------------------------------------------------------

(defconst org-air-r96--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root of the org-air checkout (parent of tests/).")

(defun org-air-r96--source-files ()
  "Return every `org-air*.el' source file in the package root."
  (directory-files org-air-r96--root t "\\`org-air.*\\.el\\'"))

(defun org-air-r96--slot-name (spec)
  "Return the slot symbol named by `cl-defstruct' slot SPEC."
  (if (consp spec) (car spec) spec))

(defun org-air-r96--struct-spec (form file line)
  "Return a plist describing the `cl-defstruct' FORM at FILE:LINE.
Keys: :type :conc :slots :pred :copier :ctors :file :line.  `:ctors' is
an alist of (NAME . ARGLIST); ARGLIST is nil for a keyword constructor
and the BOA arglist otherwise.  A nil option value means NOT GENERATED
and yields nil for `:pred'/`:copier'; naming any `:constructor'
suppresses the default `make-TYPE'."
  (let* ((head (nth 1 form))
         (type (if (consp head) (car head) head))
         (opts (and (consp head) (cdr head)))
         (conc (format "%s-" type))
         (pred (intern (format "%s-p" type)))
         (copier (intern (format "copy-%s" type)))
         (named-ctor nil)
         (ctors nil)
         (body (cddr form))
         (slots nil))
    (dolist (opt opts)
      (when (consp opt)
        (pcase (car opt)
          (:conc-name (setq conc (if (cadr opt) (format "%s" (cadr opt)) "")))
          (:predicate (setq pred (cadr opt)))
          (:copier (setq copier (cadr opt)))
          (:constructor
           (setq named-ctor t)
           (when (cadr opt) (push (cons (cadr opt) (nth 2 opt)) ctors))))))
    (unless named-ctor
      (push (cons (intern (format "make-%s" type)) nil) ctors))
    (when (stringp (car body)) (setq body (cdr body)))
    (dolist (spec body)
      (let ((slot (org-air-r96--slot-name spec)))
        (when (symbolp slot) (push slot slots))))
    (list :type type :conc conc :slots (nreverse slots)
          :pred pred :copier copier :ctors (nreverse ctors)
          :file (file-name-nondirectory file) :line line)))

(defun org-air-r96--struct-specs ()
  "Return a spec plist for every `cl-defstruct' in the package sources."
  (let ((specs nil))
    (dolist (file (org-air-r96--source-files))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (condition-case nil
            (while t
              (let* ((start (point))
                     (form (read (current-buffer))))
                (when (and (consp form) (eq (car form) 'cl-defstruct))
                  (push (org-air-r96--struct-spec
                         form file (line-number-at-pos start))
                        specs))))
          (end-of-file nil))))
    (nreverse specs)))

(defun org-air-r96--accessor (spec slot)
  "Return the accessor symbol for SLOT of struct SPEC."
  (intern (format "%s%s" (plist-get spec :conc) slot)))

(defun org-air-r96--sentinel (type slot)
  "Return a distinct probe value for SLOT of struct TYPE."
  (format "org-air-r96<%s.%s>" type slot))

(defun org-air-r96--probe-object (spec)
  "Return a live SPEC instance whose every slot holds a distinct sentinel.
Built with `record' rather than the constructor so that a broken
constructor cannot mask a broken accessor (the constructors get their
own pins in `org-air-r96-2-…')."
  (let ((type (plist-get spec :type)))
    (apply #'record type
           (mapcar (lambda (slot) (org-air-r96--sentinel type slot))
                   (plist-get spec :slots)))))

(defun org-air-r96--runtime-slots (type)
  "Return TYPE's slot names as the LOADED struct definition knows them."
  (delq 'cl-tag-slot (mapcar #'car (cl-struct-slot-info type))))

(defun org-air-r96--required-args (arglist)
  "Return the leading required argument symbols of ARGLIST."
  (let ((args nil))
    (catch 'done
      (dolist (a arglist)
        (when (memq a '(&optional &rest &key &aux)) (throw 'done nil))
        (push a args)))
    (nreverse args)))

;;;; -------------------------------------------------------------------
;;;; 1 — every accessor's FUNCTION CELL is still the accessor
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r96-1-struct-accessor-function-cells-are-the-accessors ()
  "Every `cl-defstruct' slot accessor in the package still owns its cell.
For each slot of each struct: the accessor is bound, is NOT a command
\(no `interactive' form), and every reach that consults the FUNCTION CELL
— `funcall', `apply', `mapcar' — returns exactly
`cl-struct-slot-value'.  Static call sites are deliberately NOT used:
`cl-defstruct' inlines those via a compiler macro, compiled AND
interpreted, which is precisely what hid the R96 defect.

RED on R95: `org-air-item-deadline' is `commandp' there and funcalling
it signals `user-error' instead of reading the `deadline' slot."
  (skip-unless (locate-library "org-air"))
  (let ((specs (org-air-r96--struct-specs))
        (checked 0))
    ;; Vacuity guard: the three known structs must be among those found.
    (should (>= (length specs) 3))
    (dolist (type '(org-air-item org-air-doc org-air-view--history-token))
      (should (seq-find (lambda (s) (eq (plist-get s :type) type)) specs)))
    (dolist (spec specs)
      (let* ((type (plist-get spec :type))
             (slots (plist-get spec :slots))
             (obj (org-air-r96--probe-object spec)))
        (ert-info ((format "struct %s (%s:%d)" type
                           (plist-get spec :file) (plist-get spec :line)))
          ;; The loaded struct agrees with the source we parsed.
          (should (equal slots (org-air-r96--runtime-slots type)))
          (should (cl-typep obj type))
          (dolist (slot slots)
            (let ((acc (org-air-r96--accessor spec slot))
                  (want (org-air-r96--sentinel type slot)))
              (ert-info ((format "accessor %s" acc))
                (should (fboundp acc))
                ;; The bug's signature: a command took the cell.
                (should-not (commandp acc))
                (should-not (interactive-form acc))
                (should (equal want (cl-struct-slot-value type slot obj)))
                ;; Three distinct function-cell reaches, all inlining-free.
                (should (equal want (funcall acc obj)))
                (should (equal want (apply acc (list obj))))
                (should (equal (list want) (mapcar acc (list obj))))
                (cl-incf checked)))))))
    ;; 24 (org-air-item) + 7 (org-air-doc) + 1 (history-token) = 32.
    (should (>= checked 32))))

;;;; -------------------------------------------------------------------
;;;; 2 — the other generated functions: constructors, predicates, copiers
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r96-2-struct-constructors-predicates-copiers-are-clean ()
  "Every generated constructor, predicate and copier is the struct's own.
None may be a command; the predicate must accept its own instance and
reject a non-instance; the copier must return an `equal' but not `eq'
object; every constructor must build an instance whose named arguments
landed in the matching slots.  Together with `org-air-r96-1-…' this
covers all 39 function-namespace names the package's three
`cl-defstruct' forms own."
  (skip-unless (locate-library "org-air"))
  (let ((specs (org-air-r96--struct-specs))
        (checked 0))
    (should (>= (length specs) 3))
    (dolist (spec specs)
      (let* ((type (plist-get spec :type))
             (slots (plist-get spec :slots))
             (obj (org-air-r96--probe-object spec))
             (pred (plist-get spec :pred))
             (copier (plist-get spec :copier)))
        (ert-info ((format "struct %s" type))
          (when pred
            (ert-info ((format "predicate %s" pred))
              (should (fboundp pred))
              (should-not (commandp pred))
              (should (funcall pred obj))
              (should-not (funcall pred nil))
              (should-not (funcall pred "not a struct"))
              (cl-incf checked)))
          (when copier
            (ert-info ((format "copier %s" copier))
              (should (fboundp copier))
              (should-not (commandp copier))
              (let ((copy (funcall copier obj)))
                (should (equal copy obj))
                (should-not (eq copy obj)))
              (cl-incf checked)))
          (pcase-dolist (`(,ctor . ,arglist) (plist-get spec :ctors))
            (ert-info ((format "constructor %s" ctor))
              (should (fboundp ctor))
              (should-not (commandp ctor))
              (let* ((args (org-air-r96--required-args arglist))
                     (made (if arglist
                               ;; BOA: positional, one sentinel per arg.
                               (apply ctor
                                      (mapcar (lambda (a)
                                                (org-air-r96--sentinel type a))
                                              args))
                             ;; Keyword: one :SLOT VALUE pair per slot.
                             (apply ctor
                                    (mapcan
                                     (lambda (slot)
                                       (list (intern (format ":%s" slot))
                                             (org-air-r96--sentinel type slot)))
                                     slots)))))
                (should (cl-typep made type))
                (when pred (should (funcall pred made)))
                (dolist (slot (if arglist args slots))
                  (when (memq slot slots)
                    (should (equal (org-air-r96--sentinel type slot)
                                   (funcall (org-air-r96--accessor spec slot)
                                            made)))))
                (cl-incf checked)))))))
    ;; 3 predicates + 1 copier (org-air-item has :copier nil) + 3 ctors.
    (should (>= checked 7))))

;;;; -------------------------------------------------------------------
;;;; 3 — the instance R96 fixed, named
;;;; -------------------------------------------------------------------

(ert-deftest org-air-r96-3-deadline-accessor-and-command-are-distinct-symbols ()
  "`org-air-item-deadline' is the SLOT; `org-air-item-set-deadline' is the verb.
The command was renamed in R96 and deliberately NOT aliased back: a
`defalias' under the accessor's name would re-take the same function
cell and restore the defect verbatim.  The `d' key is unchanged, so the
rename is invisible everywhere except `M-x' completion."
  (skip-unless (locate-library "org-air"))
  ;; The accessor is a plain function reading the slot, by every reach.
  (should (fboundp 'org-air-item-deadline))
  (should-not (commandp 'org-air-item-deadline))
  (let ((item (org-air-item-create :title "t" :deadline "<2026-08-15 Sat>")))
    (should (equal "<2026-08-15 Sat>" (funcall 'org-air-item-deadline item)))
    (should (equal "<2026-08-15 Sat>"
                   (car (mapcar #'org-air-item-deadline (list item))))))
  ;; The verb is the command, and no alias points the old name back at it.
  (should (commandp 'org-air-item-set-deadline))
  (should-not (eq (indirect-function 'org-air-item-deadline)
                  (indirect-function 'org-air-item-set-deadline)))
  ;; The key did not move.
  (should (eq (lookup-key org-air-view-mode-map (kbd "d"))
              'org-air-item-set-deadline)))

(provide 'org-air-round96-test)
;;; org-air-round96-test.el ends here
