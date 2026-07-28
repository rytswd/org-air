;;; org-air-round90-test.el --- acceptance tests for round 90 -*- lexical-binding: t; -*-

;;; Commentary:
;; Round-90 acceptance and mutation seams: local mutation landing, durable
;; source-key marks, file-coordinated bulk tag writes, compound history, and
;; collapsed-by-default Backlog.  The hostile failure-path tests are
;; intentionally strict: a source defect remains an unexpected failure rather
;; than entering the known-failures manifest.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'org-air-test-helpers)
(require 'org-air-viewport-helpers)

(when (locate-library "org-air")
  (require 'org-air)
  (require 'org-air-review))

(defvar org-air-r90--dir nil
  "Temporary corpus directory of the current round-90 test.")

(defun org-air-r90--reset-query-state ()
  "Reset global query tables between round-90 corpora."
  (when (fboundp 'org-air-query-teardown)
    (org-air-query-teardown)
    (clrhash org-air-query--file-meta)
    (clrhash org-air-query--visits)
    (clrhash org-air-query--denote-id-index)
    (setq org-air-query--link-graph-dirty nil)))

(defmacro org-air-r90--with-corpus (specs &rest body)
  "Create SPECS in a fresh corpus and run BODY with isolated global state."
  (declare (indent 1) (debug t))
  `(let ((org-air-r90--dir (make-temp-file "org-air-r90-" t)))
     (unwind-protect
         (progn
           (org-air-r90--reset-query-state)
           (pcase-dolist (`(,name . ,content) ,specs)
             (let ((path (expand-file-name name org-air-r90--dir))
                   (coding-system-for-write 'utf-8-unix)
                   (file-name-handler-alist nil))
               (make-directory (file-name-directory path) t)
               (write-region (or content "") nil path nil 'silent)))
           (let ((org-air-files (list org-air-r90--dir))
                 (org-air-inbox-file
                  (expand-file-name "inbox.org" org-air-r90--dir))
                 (org-air-cache-file
                  (expand-file-name ".cache/board.eld" org-air-r90--dir))
                 (org-air-view-buffer-name "*org-air-r90*")
                 (org-air-view--edit-ring nil)
                 (org-air-view--edit-redo-ring nil)
                 (org-air-view--history-identity-registry
                  (make-hash-table :test #'eq :weakness 'key-and-value))
                 (org-air-view--cache-sync-history
                  (make-hash-table :test #'eq :weakness 'key))
                 (org-air-view--source-tracking-owner nil)
                 (find-file-hook (copy-sequence find-file-hook))
                 (org-air-view--triage-source-buffer nil)
                 (org-air-backlog-tag "backlog")
                 (org-air-plain-heading-type 'task)
                 (org-tags-column 0)
                 (org-use-tag-inheritance t)
                 (create-lockfiles nil)
                 (inhibit-message t))
             ,@body))
       (org-air-r90--reset-query-state)
       (let ((kill-buffer-query-functions nil))
         (dolist (buf (buffer-list))
           (let ((file (buffer-file-name buf)))
             (when (and file (string-prefix-p org-air-r90--dir file))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf)))))
       (delete-directory org-air-r90--dir t))))

(defmacro org-air-r90--with-board (specs &rest body)
  "Render a frozen real board over SPECS and run BODY in its owner buffer."
  (declare (indent 1) (debug t))
  `(org-air-r90--with-corpus ,specs
     (org-air-viewport-test--with-frozen-now
       (org-air-viewport-test--with-render-guards
         (let ((org-air-view-width 120)
               (org-air-view-height 70))
           (unwind-protect
               (progn
                 (org-air)
                 (let ((buf (get-buffer org-air-view-buffer-name)))
                   (should buf)
                   (with-current-buffer buf ,@body)))
             (let ((kill-buffer-query-functions nil)
                   (buf (get-buffer org-air-view-buffer-name)))
               (when buf (kill-buffer buf)))))))))

(defun org-air-r90--file (name)
  "Return absolute corpus path NAME."
  (expand-file-name name org-air-r90--dir))

(defun org-air-r90--text (name)
  "Return on-disk text of corpus file NAME."
  (with-temp-buffer
    (insert-file-contents (org-air-r90--file name))
    (buffer-string)))

(defun org-air-r90--live-text (name)
  "Return live buffer text of corpus file NAME."
  (with-current-buffer (find-file-noselect (org-air-r90--file name))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun org-air-r90--sorted-tags (item)
  "Return ITEM's cached effective tags in canonical order."
  (sort (copy-sequence (org-air-item-tags item)) #'string<))

(defun org-air-r90--cached-position-exact-p (item name title)
  "Return non-nil when ITEM's cached position names TITLE in corpus NAME."
  (= (cdr (org-air-view--item-source-key item))
     (org-air-r90--actual-heading-position name title)))

(defun org-air-r90--item (title &optional items)
  "Return item named TITLE from ITEMS or the current board cache."
  (let ((item (org-air-test-find-item title (or items org-air-view--items))))
    (should item)
    item))

(defun org-air-r90--goto-row (title)
  "Move point to the rendered item row named TITLE and return its item."
  (goto-char (point-min))
  (let (found)
    (while (and (not found) (not (eobp)))
      (let ((item (org-air-view--row-property 'org-air-item)))
        (when (and item
                   (string-match-p (regexp-quote title)
                                   (or (org-air-item-title item) "")))
          (setq found item)))
      (unless found (forward-line 1)))
    (should found)
    (org-air-view--goto-row-title)
    found))

(defun org-air-r90--row-title ()
  "Return the title of the item at point, or nil."
  (when-let* ((item (org-air-view--row-property 'org-air-item)))
    (org-air-item-title item)))

(defun org-air-r90--rendered-rows (title)
  "Return beginning positions of every rendered row named TITLE."
  (save-excursion
    (goto-char (point-min))
    (let (out)
      (while (not (eobp))
        (when-let* ((item (org-air-view--row-property 'org-air-item))
                    ((string-match-p (regexp-quote title)
                                     (or (org-air-item-title item) ""))))
          (push (line-beginning-position) out))
        (forward-line 1))
      (nreverse out))))

(defun org-air-r90--title-columns (title)
  "Return title columns for every rendered occurrence of TITLE."
  (mapcar (lambda (pos)
            (save-excursion
              (goto-char pos)
              (org-air-view--goto-row-title)
              (current-column)))
          (org-air-r90--rendered-rows title)))

(defun org-air-r90--mark-title (title)
  "Mark the exact source heading rendered as TITLE."
  (org-air-r90--goto-row title)
  (org-air-toggle-mark))

(defun org-air-r90--actual-heading-position (name title)
  "Return live heading position for TITLE in corpus file NAME."
  (with-current-buffer (find-file-noselect (org-air-r90--file name))
    (org-with-wide-buffer
     (goto-char (point-min))
     (re-search-forward (regexp-quote title))
     (org-back-to-heading t)
     (point))))

(defun org-air-r90--file-has-tag-p (name title tag)
  "Return non-nil when TITLE's live heading in NAME has local TAG."
  (with-current-buffer (find-file-noselect (org-air-r90--file name))
    (org-with-wide-buffer
     (goto-char (point-min))
     (re-search-forward (regexp-quote title))
     (org-back-to-heading t)
     (member tag (org-get-tags nil t)))))

(defun org-air-r90--disk-has-tag-p (name title tag)
  "Return non-nil when on-disk TITLE in NAME carries exact TAG."
  (let ((text (org-air-r90--text name)))
    (and (string-match-p
          (format "^\\*+ .*%s.*:%s:"
                  (regexp-quote title) (regexp-quote tag))
          text)
         t)))

(defun org-air-r90--live-tags (name title &optional local)
  "Return TITLE's sorted live tags in NAME; with LOCAL exclude inheritance."
  (with-current-buffer (find-file-noselect (org-air-r90--file name))
    (org-with-wide-buffer
     (goto-char (point-min))
     (re-search-forward (regexp-quote title))
     (org-back-to-heading t)
     (sort (copy-sequence (org-get-tags nil local)) #'string<))))

(defun org-air-r90--assert-file-cache-exact (name items)
  "Assert every heading in ITEMS exactly mirrors live file NAME."
  (dolist (item items)
    (let ((title (org-air-item-title item)))
      (should (org-air-r90--cached-position-exact-p item name title))
      (should (equal (org-air-r90--sorted-tags item)
                     (org-air-r90--live-tags name title))))))

(defun org-air-r90--day-group-at-point ()
  "Return the rendered Deadline/Scheduled/Logged group above point."
  (save-excursion
    (let (found)
      (while (and (not found) (not (bobp)))
        (beginning-of-line)
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (dolist (label '("Deadline" "Scheduled" "Logged / created"))
            (when (string-match-p
                   (format "\\`[[:space:]]*%s[[:space:]]*\\(?:[|│]\\|$\\)"
                           (regexp-quote label))
                   line)
              (setq found label))))
        (unless found (forward-line -1)))
      found)))

(defun org-air-r90--warning-messages (messages)
  "Return bounded post-commit warnings selected from MESSAGES."
  (seq-filter
   (lambda (text)
     (string-prefix-p "org-air warning: saved changes kept after hook error:"
                      text))
   messages))

(defun org-air-r90--assert-bounded-warnings (messages count)
  "Assert MESSAGES contain COUNT bounded post-commit save warnings."
  (let ((warnings (org-air-r90--warning-messages messages)))
    (should (= count (length warnings)))
    (dolist (warning warnings)
      (should (<= (string-width warning) 180)))
    warnings))

(defun org-air-r90--plist-keys (plist)
  "Return the keys from proper property list PLIST."
  (let (keys)
    (while plist
      (push (car plist) keys)
      (setq plist (cddr plist)))
    (nreverse keys)))

(defun org-air-r90--same-key-set-p (actual expected)
  "Return non-nil when ACTUAL and EXPECTED contain the same unique keys."
  (and (= (length actual) (length expected))
       (seq-every-p (lambda (key) (memq key actual)) expected)))

(defun org-air-r90--assert-history-token (value)
  "Assert VALUE is a bounded opaque history identity."
  (should (org-air-view--history-token-p value))
  (should (memq (org-air-view--history-token-projection value) '(nil head))))

(defun org-air-r90--assert-history-plist-keys (plist required optional)
  "Assert PLIST has REQUIRED keys and only bounded OPTIONAL identity keys."
  (let ((keys (org-air-r90--plist-keys plist)))
    (dolist (key required) (should (memq key keys)))
    (dolist (key keys) (should (memq key (append required optional))))
    (dolist (key optional)
      (when (plist-member plist key)
        (org-air-r90--assert-history-token (plist-get plist key))))))

(defun org-air-r90--assert-history-has-no-snapshot ()
  "Assert both history rings retain only documented bounded metadata.
Exceptional retry identities are allowed only as opaque tokens; source text,
snapshots, markers, and raw undo identities have no schema seat."
  (dolist (record (append org-air-view--edit-ring
                          org-air-view--edit-redo-ring))
    (if (eq (plist-get record :kind) 'bulk)
        (progn
          (org-air-r90--assert-history-plist-keys
           record '(:desc :kind :parts :time) nil)
          (dolist (part (plist-get record :parts))
            (org-air-r90--assert-history-plist-keys
             part '(:buffer :file :tick :undo-head) '(:expected-undo))
            (org-air-r90--assert-history-token
             (plist-get part :undo-head))))
      (org-air-r90--assert-history-plist-keys
       record '(:desc :buffer :file :kind :tick :time) '(:expected-undo)))))

(defun org-air-r90--metadata-reachability (root needle undo-list)
  "Summarize bounded metadata reachable from ROOT without following buffers.
NEEDLE identifies deleted source text that history must not retain.
UNDO-LIST supplies the live source's raw list tails for an identity-alias
check; the traversal otherwise follows conses, vectors and hash tables only."
  (let ((seen (make-hash-table :test #'eq))
        (undo-tails (make-hash-table :test #'eq))
        (undo-heads (make-hash-table :test #'eq))
        (stack (list root))
        (conses 0)
        (vectors 0)
        (hash-tables 0)
        (buffers 0)
        (markers 0)
        (strings 0)
        (string-bytes 0)
        (largest-string 0)
        (distinctive-source-reachable nil)
        (undo-tail-reachable nil)
        (undo-head-reachable nil))
    (while (consp undo-list)
      (puthash undo-list t undo-tails)
      (when (or (consp (car undo-list))
                (stringp (car undo-list))
                (vectorp (car undo-list)))
        (puthash (car undo-list) t undo-heads))
      (setq undo-list (cdr undo-list)))
    (while stack
      (let ((object (pop stack)))
        (unless (or (null object) (gethash object seen))
          (puthash object t seen)
          (when (gethash object undo-heads)
            (setq undo-head-reachable t))
          (cond
           ((consp object)
            (cl-incf conses)
            (when (gethash object undo-tails)
              (setq undo-tail-reachable t))
            (push (car object) stack)
            (push (cdr object) stack))
           ((stringp object)
            (cl-incf strings)
            (cl-incf string-bytes (string-bytes object))
            (setq largest-string
                  (max largest-string (string-bytes object)))
            (when (string-match-p (regexp-quote needle) object)
              (setq distinctive-source-reachable t)))
           ((bufferp object) (cl-incf buffers))
           ((markerp object) (cl-incf markers))
           ((functionp object) nil)
           ((vectorp object)
            (cl-incf vectors)
            (dotimes (index (length object))
              (push (aref object index) stack)))
           ((hash-table-p object)
            (cl-incf hash-tables)
            (maphash (lambda (key value)
                       (push key stack)
                       (push value stack))
                     object))))))
    (list :conses conses :vectors vectors :hash-tables hash-tables
          :buffers buffers :markers markers
          :strings strings :string-bytes string-bytes
          :largest-string largest-string
          :distinctive-source-reachable distinctive-source-reachable
          :undo-tail-reachable undo-tail-reachable
          :undo-head-reachable undo-head-reachable)))

(defun org-air-r90--hostile-b-snapshot (first later first-key)
  "Inject post-save relocation failure and summarize every truth surface."
  (cl-letf* (((symbol-function 'org-air-view--relocation-commit)
              (lambda (&rest _) (error "post-save relocation failure")))
             ((symbol-function 'read-string)
              (lambda (&rest _) "long_shared_tag")))
    (org-air-set-tag))
  (let ((record (car org-air-view--edit-ring)))
    (list :disk-live (equal (org-air-r90--text "tasks.org")
                            (org-air-r90--live-text "tasks.org"))
          :first-tags (org-air-r90--sorted-tags first)
          :later-tags (org-air-r90--sorted-tags later)
          :first-position (org-air-r90--cached-position-exact-p
                           first "tasks.org" "First heading")
          :later-position (org-air-r90--cached-position-exact-p
                           later "tasks.org" "Later heading")
          :marked (and (member first-key org-air-view--marked-keys) t)
          :history (and record (plist-get record :kind))
          :parts (and record (length (plist-get record :parts))))))

(defun org-air-r90--intrinsic-org-timer-p (observation)
  "Return non-nil when OBSERVATION is intrinsic Org cache maintenance."
  (eq (plist-get observation :callback) 'org-element--cache-sync))

(defun org-air-r90--org-air-timer-p (observation)
  "Return non-nil when OBSERVATION's callback is rooted in org-air code."
  (let ((case-fold-search nil))
    (string-match-p "\\borg-air-"
                    (prin1-to-string (plist-get observation :callback)))))

(defun org-air-r90--assert-timer-seam (observations)
  "Assert OBSERVATIONS contain only intrinsic Org cache-maintenance timers.
Each observation retains constructor, timing, callback identity, and callback
arguments so an unexpected callback is printed in the ERT failure.  In
particular, org-air refresh/render, pane/inspector resync, mutation-landing,
and every other callback rooted in org-air must remain fully synchronous."
  (let ((org-air-owned (seq-filter #'org-air-r90--org-air-timer-p observations))
        (non-intrinsic
         (seq-remove #'org-air-r90--intrinsic-org-timer-p observations)))
    (ert-info ((format "org-air-owned timer constructors: %S" org-air-owned))
      (should-not org-air-owned))
    (ert-info ((format "non-intrinsic timer constructors: %S" non-intrinsic))
      (should-not non-intrinsic))))

(defmacro org-air-r90--with-timer-audit (observations &rest body)
  "Run BODY while collecting timer constructor identity in OBSERVATIONS."
  (declare (indent 1) (debug t))
  `(let ((,observations nil))
     (cl-letf (((symbol-function 'run-with-timer)
                (lambda (seconds repeat callback &rest arguments)
                  (push (list :constructor 'run-with-timer
                              :seconds seconds :repeat repeat
                              :callback callback :arguments arguments)
                        ,observations)
                  nil))
               ((symbol-function 'run-with-idle-timer)
                (lambda (seconds repeat callback &rest arguments)
                  (push (list :constructor 'run-with-idle-timer
                              :seconds seconds :repeat repeat
                              :callback callback :arguments arguments)
                        ,observations)
                  nil)))
       ,@body)
     (setq ,observations (nreverse ,observations))
     (org-air-r90--assert-timer-seam ,observations)))

(defmacro org-air-r90--record-messages (var &rest body)
  "Run BODY and collect formatted `message' calls in VAR."
  (declare (indent 1) (debug t))
  `(let ((,var nil))
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args)
                  (when fmt (push (apply #'format fmt args) ,var))
                  nil)))
       ,@body)))

(defmacro org-air-r90--with-keybinding-knob (value &rest body)
  "Set the installer-owned keybinding knob to VALUE while running BODY."
  (declare (indent 1) (debug t))
  `(let ((saved org-air-use-default-keybindings))
     (unwind-protect
         (progn
           (setq org-air-use-default-keybindings ,value)
           (org-air--sync-default-keybindings)
           ,@body)
       (setq org-air-use-default-keybindings saved
             org-air--default-keybindings-state 'unset)
       (org-air--sync-default-keybindings))))

(defconst org-air-r90--three
  '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha task :one:\n* TODO Beta task :two:\n* TODO Gamma task :three:\n")
    ("inbox.org" . "#+title: inbox\n"))
  "Three same-section tasks used by local-focus and mark tests.")

(defconst org-air-r90--invalid-tags
  '("" " " "bad tag" "bad\ttag" "bad\ntag" "bad:tag"
    "shared-tag" "not/a/tag" " leading" "trailing "
    "\tleading" "trailing\t" "\nleading" "trailing\n" ":")
  "Malformed singular tag values that must be refused before preflight.")

(defun org-air-r90--mutating-save-hook (label)
  "Return an after-save hook that inserts LABEL and then signals."
  (let ((line (format "# %s\n" label)))
    (lambda ()
      (goto-char (point-min))
      (insert line)
      (error "%s signaled" label))))

(defun org-air-r90--resolve-hook-insertion (source label)
  "Independently undo LABEL's after-save insertion in SOURCE."
  (with-current-buffer source
    (undo-boundary)
    (undo-only)
    (should-not (string-match-p (regexp-quote label) (buffer-string)))))

(defun org-air-r90--disk-heading-position (name title)
  "Return TITLE's heading position in the on-disk corpus file NAME."
  (with-temp-buffer
    (insert-file-contents (org-air-r90--file name))
    (org-mode)
    (goto-char (point-min))
    (re-search-forward (regexp-quote title))
    (org-back-to-heading t)
    (point)))

(defun org-air-r90--window-state ()
  "Return the current non-minibuffer window identity and position state."
  (mapcar (lambda (window)
            (list window (window-buffer window) (window-start window)
                  (window-point window)))
          (window-list nil 'nomini)))

(defun org-air-r90--command-state (name source)
  "Snapshot command-observable state for corpus NAME and SOURCE buffer."
  (list :disk (org-air-r90--text name)
        :live (org-air-r90--live-text name)
        :tick (with-current-buffer source (buffer-chars-modified-tick))
        :modified (buffer-modified-p source)
        :items org-air-view--items
        :item-data (mapcar #'copy-sequence org-air-view--items)
        :items-key org-air-view--items-key
        :classify org-air-view--classify-cache
        :classify-count (and org-air-view--classify-cache
                             (hash-table-count org-air-view--classify-cache))
        :marks org-air-view--marked-keys
        :mark-table org-air-view--marked-key-table
        :edit-ring org-air-view--edit-ring
        :redo-ring org-air-view--edit-redo-ring
        :point (point)
        :selected (selected-window)
        :windows (org-air-r90--window-state)
        :inspector org-air-view--inspector-item
        :pending org-air-view--pending-mutation-landing
        :pane (org-air-view-pane--context-at-point)))

(defun org-air-r90--assert-command-state (before name source)
  "Assert current command state exactly matches BEFORE for NAME and SOURCE."
  (should (equal (plist-get before :disk) (org-air-r90--text name)))
  (should (equal (plist-get before :live) (org-air-r90--live-text name)))
  (should (= (plist-get before :tick)
             (with-current-buffer source (buffer-chars-modified-tick))))
  (should (eq (plist-get before :modified) (buffer-modified-p source)))
  (should (eq (plist-get before :items) org-air-view--items))
  (should (equal (plist-get before :item-data)
                 (mapcar #'copy-sequence org-air-view--items)))
  (should (equal (plist-get before :items-key) org-air-view--items-key))
  (should (eq (plist-get before :classify) org-air-view--classify-cache))
  (should (equal (plist-get before :classify-count)
                 (and org-air-view--classify-cache
                      (hash-table-count org-air-view--classify-cache))))
  (should (eq (plist-get before :marks) org-air-view--marked-keys))
  (should (eq (plist-get before :mark-table) org-air-view--marked-key-table))
  (should (eq (plist-get before :edit-ring) org-air-view--edit-ring))
  (should (eq (plist-get before :redo-ring) org-air-view--edit-redo-ring))
  (should (= (plist-get before :point) (point)))
  (should (eq (plist-get before :selected) (selected-window)))
  (should (equal (plist-get before :windows) (org-air-r90--window-state)))
  (should (eq (plist-get before :inspector) org-air-view--inspector-item))
  (should (eq (plist-get before :pending)
              org-air-view--pending-mutation-landing))
  (should (equal (plist-get before :pane)
                 (org-air-view-pane--context-at-point))))

(defun org-air-r90--history-metadata-without-buffers (record)
  "Copy RECORD metadata while removing only documented buffer references."
  (let ((copy (copy-sequence record)))
    (if (eq (plist-get copy :kind) 'bulk)
        (plist-put
         copy :parts
         (mapcar (lambda (part)
                   (let ((part-copy (copy-sequence part)))
                     (plist-put part-copy :buffer nil)
                     part-copy))
                 (plist-get copy :parts)))
      (plist-put copy :buffer nil))
    copy))

(defun org-air-r90--same-object-order-p (actual expected)
  "Return non-nil when ACTUAL and EXPECTED contain identical objects in order."
  (and (= (length actual) (length expected))
       (cl-every #'eq actual expected)))

(defun org-air-r90--force-gc ()
  "Run enough full collections for weak-table assertions."
  (garbage-collect)
  (garbage-collect))

(defun org-air-r90--registry-drop-key-setup ()
  "Register an identity and return only its independently held raw value."
  (let* ((raw (list 'raw-key-drop))
         (_token (org-air-view--history-identity-register raw)))
    raw))

(defun org-air-r90--registry-drop-value-setup ()
  "Register an identity and return only its independently held token key."
  (let* ((raw (list 'raw-value-drop))
         (token (org-air-view--history-identity-register raw)))
    token))

;;; r90-1 — local successor, exclusion, and synchronous surface ordering.

(ert-deftest org-air-r90-1-local-focus-middle-successor-no-scan ()
  "A middle-row `b' lands on its same-section successor, never its new home."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board org-air-r90--three
    ;; Provision the one user-initiated source buffer before timer spying;
    ;; org-mode's own first-mode initialization is outside the landing seam.
    (find-file-noselect (org-air-r90--file "tasks.org"))
    (let* ((moved (org-air-r90--goto-row "Beta task"))
           (moved-key (org-air-view--item-source-key moved))
           (queries 0) (setup nil) (pane-final nil) (pane-resyncs 0))
      (org-air-r90--with-timer-audit timer-observations
        (cl-letf* ((setup-orig (symbol-function 'org-air-view--setup-inspector))
                   ((symbol-function 'org-air-view--setup-inspector)
                    (lambda ()
                      (push (list org-air-view--pending-mutation-landing
                                  (org-air-r90--row-title)) setup)
                      (funcall setup-orig)))
                   ((symbol-function 'org-air-view--view-pane-update-now)
                    (lambda (buf)
                      (cl-incf pane-resyncs)
                      (setq pane-final
                            (with-current-buffer buf (org-air-r90--row-title)))))
                   (query-orig (symbol-function 'org-air-query-items))
                   ((symbol-function 'org-air-query-items)
                    (lambda (&rest args)
                      (cl-incf queries) (apply query-orig args)))
                   (query-files-orig
                    (symbol-function 'org-air-query-items-in-files))
                   ((symbol-function 'org-air-query-items-in-files)
                    (lambda (&rest args)
                      (cl-incf queries) (apply query-files-orig args))))
          (org-air-item-backlog)))
      (should (equal "Gamma task" (org-air-r90--row-title)))
      (should-not (equal moved-key
                         (org-air-view--item-source-key
                          (org-air-view--row-property 'org-air-item))))
      (should (org-air-r90--file-has-tag-p "tasks.org" "Beta task" "backlog"))
      (should (equal "Gamma task" pane-final))
      (should (= pane-resyncs 1))
      (should (equal '(nil "Gamma task") (car setup)))
      (should-not org-air-view--pending-mutation-landing)
      (should (= queries 0)))))

;;; r90-2 — last/previous and un-backlog local focus.

(ert-deftest org-air-r90-2-last-and-unbacklog-use-local-neighbours ()
  "Last-row and expanded/lensed un-backlog never follow the moved item."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board org-air-r90--three
    (org-air-r90--goto-row "Gamma task")
    (org-air-item-backlog)
    (should (equal "Beta task" (org-air-r90--row-title))))
  (dolist (lens '(expanded exact-lens))
    (org-air-r90--with-board
        '(("tasks.org" . "#+title: tasks\n\n* TODO Back A :backlog:\n* TODO Back B :backlog:\n* TODO Plain task\n")
          ("inbox.org" . "#+title: inbox\n"))
      (pcase lens
        ('expanded
         (setq org-air-view--expanded-sections '(backlog))
         (org-air-view--render-current))
        ('exact-lens
         (setq org-air-view--tag-filter '("is:backlog"))
         (org-air-view--ensure-explicit-backlog-lens)
         (org-air-view--render-current)))
      (let ((moved-key (org-air-view--item-source-key
                        (org-air-r90--goto-row "Back A"))))
        (org-air-item-backlog)
        (should (equal "Back B" (org-air-r90--row-title)))
        (should-not (equal moved-key
                           (org-air-view--item-source-key
                            (org-air-view--row-property 'org-air-item))))))))

;;; r90-3 — exact ordered marks, mirrors, primitive, and repaint durability.

(ert-deftest org-air-r90-3-durable-ordered-mirror-marks ()
  "Marks are source-key ordered state rendered through every mirror row."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO [#A] Mirror task :proj:\nDEADLINE: <2026-06-10 Wed>\n* TODO Other task :other:\nSCHEDULED: <2026-06-15 Mon>\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let ((before-cols (org-air-r90--title-columns "Mirror task"))
          (primitive-marks 0))
      (should (> (length before-cols) 1))
      (let* ((mirror (org-air-r90--goto-row "Mirror task"))
             (mirror-key (org-air-view--item-source-key mirror)))
        (cl-letf* ((orig (symbol-function 'org-air-view--insert-row))
                   ((symbol-function 'org-air-view--insert-row)
                    (lambda (&rest args)
                      (when (plist-get args :marked)
                        (cl-incf primitive-marks))
                      (apply orig args))))
          (org-air-toggle-mark))
        (should (equal (list mirror-key) org-air-view--marked-keys)))
      (org-air-r90--mark-title "Other task")
      (should (= 2 (length org-air-view--marked-keys)))
      (should (> primitive-marks 1))
      (should (equal before-cols (org-air-r90--title-columns "Mirror task")))
      (dolist (pos (org-air-r90--rendered-rows "Mirror task"))
        (goto-char pos)
        (should (org-air-view--row-property 'org-air-marked))
        (should (string-match-p "•" (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (line-end-position)))))
      ;; Cached repaint, sort, filter/collapse, and day/board switches keep
      ;; the exact ordered source selection even while rows are hidden.
      (let ((keys (copy-sequence org-air-view--marked-keys)))
        (org-air-view--refresh-current)
        (org-air-view-sort-cycle)
        (setq org-air-view--tag-filter '("Other"))
        (org-air-view--render-current)
        (setq org-air-view--expanded-sections nil)
        (org-air-view-day org-air-test-now)
        (org-air-view-board)
        (should (equal keys org-air-view--marked-keys)))
      (org-air-clear-marks)
      (should-not org-air-view--marked-keys)
      (should-not (text-property-not-all (point-min) (point-max)
                                         'org-air-marked nil)))))

;;; r90-4/5 — marked backlog set-all in both directions.

(ert-deftest org-air-r90-4-marked-backlog-mixed-set-all-on ()
  "Mixed backlog state becomes all-ON once; co-tags and one-file grouping survive."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Plain A :red:\n* TODO Plain B :blue:\n* TODO Already C :backlog:green:\n")
        ("inbox.org" . "#+title: inbox\n"))
    (setq org-air-view--expanded-sections '(backlog))
    (org-air-view--render-current)
    (dolist (title '("Plain A" "Plain B" "Already C"))
      (org-air-r90--mark-title title))
    (let ((renders 0) (toggles nil))
      (cl-letf* ((render-orig (symbol-function 'org-air-view--render))
                 ((symbol-function 'org-air-view--render)
                  (lambda (&rest args) (cl-incf renders) (apply render-orig args)))
                 (toggle-orig (symbol-function 'org-toggle-tag))
                 ((symbol-function 'org-toggle-tag)
                  (lambda (tag state)
                    (push (list (org-get-heading t t t t) tag state) toggles)
                    (funcall toggle-orig tag state))))
        (org-air-item-backlog))
      (should (= renders 1))
      (should (= (length toggles) 2)))
    (dolist (title '("Plain A" "Plain B" "Already C"))
      (should (org-air-r90--file-has-tag-p "tasks.org" title "backlog")))
    (dolist (pair '(("Plain A" . "red") ("Plain B" . "blue")
                    ("Already C" . "green")))
      (should (org-air-r90--file-has-tag-p "tasks.org" (car pair) (cdr pair))))
    (should-not org-air-view--marked-keys)
    (should (= 1 (length org-air-view--edit-ring)))
    (let ((record (car org-air-view--edit-ring)))
      (should (eq 'bulk (plist-get record :kind)))
      (should (= 1 (length (plist-get record :parts)))))))

(ert-deftest org-air-r90-5-marked-backlog-all-on-set-all-off ()
  "An all-backlog marked set becomes all-OFF without disturbing co-tags."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Back A :backlog:red:\n* TODO Back B :backlog:blue:\n* TODO Back C :backlog:green:\n")
        ("inbox.org" . "#+title: inbox\n"))
    (setq org-air-view--expanded-sections '(backlog))
    (org-air-view--render-current)
    (dolist (title '("Back A" "Back B" "Back C"))
      (org-air-r90--mark-title title))
    (org-air-item-backlog)
    (dolist (title '("Back A" "Back B" "Back C"))
      (should-not (org-air-r90--file-has-tag-p "tasks.org" title "backlog")))
    (dolist (pair '(("Back A" . "red") ("Back B" . "blue")
                    ("Back C" . "green")))
      (should (org-air-r90--file-has-tag-p "tasks.org" (car pair) (cdr pair))))))

;;; r90-6 — hidden targets and rendered duplicate de-duplication.

(ert-deftest org-air-r90-6-hidden-marks-targeted-once-with-honest-counts ()
  "Filtering hides marks but bulk targets each unique source key exactly once."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO [#A] Alpha mirror\nDEADLINE: <2026-06-10 Wed>\n* TODO Beta hidden\n* TODO Gamma hidden\n")
        ("inbox.org" . "#+title: inbox\n"))
    (dolist (title '("Alpha mirror" "Beta hidden" "Gamma hidden"))
      (org-air-r90--mark-title title))
    (setq org-air-view--tag-filter '("Alpha"))
    (org-air-view--render-current)
    (should (equal "• 3 marked · 1 shown"
                   (org-air-view--marked-count-label org-air-view--items)))
    (let ((calls nil))
      (cl-letf* ((orig (symbol-function 'org-toggle-tag))
                 ((symbol-function 'org-toggle-tag)
                  (lambda (tag state)
                    (push (org-get-heading t t t t) calls)
                    (funcall orig tag state))))
        (org-air-item-backlog))
      (should (= 3 (length calls)))
      (should (= 3 (length (delete-dups calls)))))
    (dolist (title '("Alpha mirror" "Beta hidden" "Gamma hidden"))
      (should (org-air-r90--file-has-tag-p "tasks.org" title "backlog")))))

;;; r90-7 — stale/ineligible policy and exact generation reconciliation.

(ert-deftest org-air-r90-7-stale-pruned-ineligible-retained-and-exact-reconcile ()
  "Stale keys prune; ineligible keys remain; generation swaps use exact keys only."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Eligible task\n* DONE Finished task\n")
        ("note.org" . "#+title: Headingless note\n\nProse.\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((eligible (org-air-r90--item "Eligible task"))
           (done (org-air-r90--item "Finished task"))
           (note (org-air-r90--item "Headingless note"))
           (eligible-key (org-air-view--item-source-key eligible))
           (done-key (org-air-view--item-source-key done))
           (note-key (org-air-view--item-source-key note))
           (stale-key (cons (org-air-r90--file "missing.org") 42)))
      (setq org-air-view--marked-keys
            (list eligible-key done-key note-key stale-key)
            org-air-view--marked-generation org-air-view--items)
      (org-air-view--marked-table-rebuild)
      (org-air-r90--goto-row "Eligible task")
      (org-air-item-backlog)
      (should (org-air-r90--file-has-tag-p "tasks.org" "Eligible task" "backlog"))
      (should (= 2 (length org-air-view--marked-keys)))
      ;; Tag growth before Finished relocates its exact source key; surviving
      ;; ineligible marks follow that relocation rather than going stale.
      (let ((done-key-now (org-air-view--item-source-key done))
            (note-key-now (org-air-view--item-source-key note)))
        (should (member done-key-now org-air-view--marked-keys))
        (should (member note-key-now org-air-view--marked-keys))
        (should-not (member done-key org-air-view--marked-keys))
        (should-not (member stale-key org-air-view--marked-keys))
        ;; A fresh generation keeps an exact survivor but never guesses by title
        ;; and never reads a source file during reconciliation.
        (setq done-key done-key-now
              note-key note-key-now))
      (setq org-air-view--marked-keys (list done-key stale-key)
            org-air-view--marked-generation org-air-view--items)
      (org-air-view--marked-table-rebuild)
      (let ((fresh (mapcar #'copy-sequence org-air-view--items))
            (opens 0))
        (cl-letf (((symbol-function 'find-file-noselect)
                   (lambda (&rest _) (cl-incf opens) (error "file read"))))
          (org-air-view--marked-reconcile fresh))
        (should (= opens 0))
        (should (equal (list done-key) org-air-view--marked-keys))))))

;;; r90-8 — one shared tag prompt and no-op history laws.

(ert-deftest org-air-r90-8-marked-tag-one-prompt-noops-clear ()
  "Marked `t' prompts once, adds one value, clears successes, and groups history."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Tag A\n* TODO Tag B :shared:\n* TODO Tag C\n")
        ("inbox.org" . "#+title: inbox\n"))
    (dolist (title '("Tag A" "Tag B" "Tag C"))
      (org-air-r90--mark-title title))
    (let ((prompts 0))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (cl-incf prompts) "shared")))
        (org-air-set-tag))
      (should (= prompts 1)))
    (dolist (title '("Tag A" "Tag B" "Tag C"))
      (should (org-air-r90--file-has-tag-p "tasks.org" title "shared")))
    (should-not org-air-view--marked-keys)
    (should (eq 'bulk (plist-get (car org-air-view--edit-ring) :kind)))
    (should (= 1 (length (plist-get (car org-air-view--edit-ring) :parts))))))

(ert-deftest org-air-r90-8c-noop-and-stale-only-create-no-history-or-write ()
  "No-op-only and stale-prune-only commands never create source history."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Already tagged :shared:\n")
        ("inbox.org" . "#+title: inbox\n"))
    (org-air-r90--mark-title "Already tagged")
    (let ((before (org-air-r90--text "tasks.org")) (saves 0))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "shared"))
                ((symbol-function 'save-buffer)
                 (lambda (&rest _) (cl-incf saves) (error "unexpected save"))))
        (org-air-set-tag))
      (should (= saves 0))
      (should (equal before (org-air-r90--text "tasks.org")))
      (should-not org-air-view--edit-ring)
      (should-not org-air-view--marked-keys))
    (setq org-air-view--marked-keys
          (list (cons (org-air-r90--file "gone.org") 9))
          org-air-view--marked-generation org-air-view--items)
    (org-air-view--marked-table-rebuild)
    (let ((before (org-air-r90--text "tasks.org")) (saves 0))
      (cl-letf (((symbol-function 'save-buffer)
                 (lambda (&rest _) (cl-incf saves) (error "unexpected save"))))
        (org-air-view--marked-tag-action 'backlog))
      (should (= saves 0))
      (should (equal before (org-air-r90--text "tasks.org")))
      (should-not org-air-view--edit-ring)
      (should-not org-air-view--marked-keys))))

;;; r90-9 — unsupported command guards and non-mutation meanings.

(ert-deftest org-air-r90-9-unsupported-mutations-guard-before-prompts-writes ()
  "T/d/e/f/a/D/x/schedule/I all stop before any prompt or source write."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board org-air-r90--three
    (org-air-r90--mark-title "Alpha task")
    (let ((prompts 0) (writes 0))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (cl-incf prompts) "bad"))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) (cl-incf prompts) "bad"))
                ((symbol-function 'read-char-exclusive)
                 (lambda (&rest _) (cl-incf prompts) ?q))
                ((symbol-function 'yes-or-no-p)
                 (lambda (&rest _) (cl-incf prompts) t))
                ((symbol-function 'save-buffer)
                 (lambda (&rest _) (cl-incf writes) (error "write"))))
        (dolist (command '(org-air-item-cycle-todo org-air-item-deadline
                           org-air-refile-item org-air-item-file-group
                           org-air-item-archive org-air-item-done
                           org-air-item-kill org-air-item-schedule
                           org-air-process-inbox))
          (should-error (call-interactively command) :type 'user-error)))
      (should (= prompts 0))
      (should (= writes 0)))
    ;; Navigation, sort/filter/TAB and u/U retain their own dispatch rather
    ;; than being transformed into marked item actions.
    (let ((keys (copy-sequence org-air-view--marked-keys)))
      (org-air-r90--goto-row "Alpha task")
      (org-air-next-item)
      (should (equal "Beta task" (org-air-r90--row-title)))
      (org-air-view-sort-cycle)
      (setq org-air-view--tag-filter '("Alpha"))
      (org-air-view--render-current)
      (should-error (org-air-edit-undo) :type 'user-error)
      (should-error (org-air-edit-redo) :type 'user-error)
      (should (equal keys org-air-view--marked-keys)))))

;;; r90-10 — one preflight/file group/save, deterministic order, no scans.

(ert-deftest org-air-r90-10-write-discipline-one-repaint-zero-scans ()
  "Bulk write is one preflight, one group/save per file, descending and no-scan."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("a.org" . "#+title: a\n\n* TODO A first\n* TODO A second\n")
        ("b.org" . "#+title: b\n\n* TODO B only\n")
        ("inbox.org" . "#+title: inbox\n"))
    (dolist (title '("A first" "A second" "B only"))
      (org-air-r90--mark-title title))
    ;; Provision source buffers before the seam.  Constructor observations
    ;; below still retain and classify every command-time callback.
    (find-file-noselect (org-air-r90--file "a.org"))
    (find-file-noselect (org-air-r90--file "b.org"))
    (let ((preflights 0) (groups 0) (renders 0) (queries 0)
          (refreshes 0) saves toggles)
      (org-air-r90--with-timer-audit timer-observations
        (cl-letf* ((pre-orig (symbol-function 'org-air-view--bulk-preflight))
                   ((symbol-function 'org-air-view--bulk-preflight)
                    (lambda (&rest args)
                      (cl-incf preflights) (apply pre-orig args)))
                   (group-orig (symbol-function 'prepare-change-group))
                   ((symbol-function 'prepare-change-group)
                    (lambda (&rest args)
                      (cl-incf groups) (apply group-orig args)))
                   (save-orig (symbol-function 'save-buffer))
                   ((symbol-function 'save-buffer)
                    (lambda (&rest args)
                      (push (buffer-file-name) saves)
                      (apply save-orig args)))
                   (toggle-orig (symbol-function 'org-toggle-tag))
                   ((symbol-function 'org-toggle-tag)
                    (lambda (tag state)
                      (push (cons (buffer-file-name) (point)) toggles)
                      (funcall toggle-orig tag state)))
                   (render-orig (symbol-function 'org-air-view--render))
                   ((symbol-function 'org-air-view--render)
                    (lambda (&rest args)
                      (cl-incf renders) (apply render-orig args)))
                   ((symbol-function 'org-air-query-items)
                    (lambda (&rest _) (cl-incf queries) (error "query")))
                   ((symbol-function 'org-air-query-items-in-files)
                    (lambda (&rest _) (cl-incf queries) (error "query")))
                   ((symbol-function 'org-air-refresh)
                    (lambda (&rest _) (cl-incf refreshes) (error "refresh"))))
          (org-air-item-backlog)))
      (setq saves (nreverse saves) toggles (nreverse toggles))
      (should (= preflights 1))
      (should (= groups 2))
      (should (= renders 1))
      (should (= queries 0))
      (should (= refreshes 0))
      (should (equal saves (sort (copy-sequence saves) #'string<)))
      (should (= 1 (seq-count (lambda (file) (equal file (org-air-r90--file "a.org"))) saves)))
      (should (= 1 (seq-count (lambda (file) (equal file (org-air-r90--file "b.org"))) saves)))
      (let ((a-positions (mapcar #'cdr
                                 (seq-filter
                                  (lambda (entry)
                                    (equal (car entry) (org-air-r90--file "a.org")))
                                  toggles))))
        (should (= 2 (length a-positions)))
        (should (> (car a-positions) (cadr a-positions)))))))

;;; r90-11 — same-file rollback and cross-file stop-on-failure.

(ert-deftest org-air-r90-11-file-atomicity-and-partial-commit ()
  "A same-file signal rolls back all; a later-file failure stops the suffix."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("a.org" . "#+title: a\n\n* TODO A first\n* TODO A second\n")
        ("inbox.org" . "#+title: inbox\n"))
    (dolist (title '("A first" "A second")) (org-air-r90--mark-title title))
    (let ((before (org-air-r90--text "a.org")) (calls 0))
      (cl-letf* ((orig (symbol-function 'org-toggle-tag))
                 ((symbol-function 'org-toggle-tag)
                  (lambda (&rest args)
                    (cl-incf calls)
                    (if (= calls 2) (error "second heading failure")
                      (apply orig args)))))
        (org-air-item-backlog))
      (should (equal before (org-air-r90--text "a.org")))
      (should (equal before
                     (with-current-buffer (find-file-noselect
                                           (org-air-r90--file "a.org"))
                       (buffer-string))))
      (should (= 2 (length org-air-view--marked-keys)))
      (should-not org-air-view--edit-ring)))
  (org-air-r90--with-board
      '(("a.org" . "#+title: a\n\n* TODO A task\n")
        ("b.org" . "#+title: b\n\n* TODO B task\n")
        ("c.org" . "#+title: c\n\n* TODO C task\n")
        ("inbox.org" . "#+title: inbox\n"))
    (dolist (title '("A task" "B task" "C task")) (org-air-r90--mark-title title))
    (let ((save-orig (symbol-function 'save-buffer)))
      (cl-letf (((symbol-function 'save-buffer)
                 (lambda (&rest args)
                   (if (equal (buffer-file-name) (org-air-r90--file "b.org"))
                       (error "b save failure")
                     (apply save-orig args)))))
        (org-air-item-backlog)))
    (should (org-air-r90--file-has-tag-p "a.org" "A task" "backlog"))
    (should-not (org-air-r90--file-has-tag-p "b.org" "B task" "backlog"))
    (should-not (org-air-r90--file-has-tag-p "c.org" "C task" "backlog"))
    (should (= 2 (length org-air-view--marked-keys)))
    (should (= 1 (length org-air-view--edit-ring)))
    (should (= 1 (length (plist-get (car org-air-view--edit-ring) :parts))))))

;;; r90-12 — relocation, second no-scan write, inherited-tag audit.

(ert-deftest org-air-r90-12-relocates-all-later-headings-for-second-action ()
  "Tag growth relocates untouched later headings before a second no-scan action."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO First heading\n* TODO Second heading\n* TODO Later untouched\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((later (org-air-r90--item "Later untouched"))
           (old-pos (cdr (org-air-view--item-source-key later))))
      (org-air-r90--mark-title "First heading")
      (org-air-r90--mark-title "Second heading")
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "a_very_long_shared_tag")))
        (org-air-set-tag))
      (let ((new-pos (cdr (org-air-view--item-source-key later))))
        (should (> new-pos old-pos))
        (should (= new-pos (org-air-r90--actual-heading-position
                            "tasks.org" "Later untouched"))))
      (let ((queries 0))
        (org-air-r90--goto-row "Later untouched")
        (cl-letf (((symbol-function 'org-air-query-items)
                   (lambda (&rest _) (cl-incf queries) (error "query")))
                  ((symbol-function 'org-air-query-items-in-files)
                   (lambda (&rest _) (cl-incf queries) (error "query"))))
          (org-air-item-backlog))
        (should (= queries 0)))
      (should (org-air-r90--file-has-tag-p
               "tasks.org" "Later untouched" "backlog"))
      (should-not (org-air-r90--file-has-tag-p
                   "tasks.org" "Second heading" "backlog")))))

(ert-deftest org-air-r90-12-inherited-effective-tags-do-not-reject-valid-heading ()
  "Inherited preflight and write/u/U mirrors preserve effective/local truth."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* Project :inherited:\n** TODO Inherited child :local:\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((child (org-air-r90--item "Inherited child"))
           (before (org-air-r90--text "tasks.org"))
           (after "#+title: tasks\n\n* Project :inherited:\n** TODO Inherited child :local:shared:\n"))
      (should (equal '("inherited" "local")
                     (org-air-r90--sorted-tags child)))
      (org-air-r90--mark-title "Inherited child")
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "shared")))
        (org-air-set-tag))
      ;; Effective cached tags validate the heading, but only the requested
      ;; local tag is added to source; the inherited tag stays on the parent.
      (should (equal after (org-air-r90--text "tasks.org")))
      (should-not (org-air-r90--file-has-tag-p
                   "tasks.org" "Inherited child" "inherited"))
      (should (equal '("inherited" "local" "shared")
                     (org-air-r90--sorted-tags child)))
      (should-not org-air-view--marked-keys)
      (should (eq 'bulk (plist-get (car org-air-view--edit-ring) :kind)))
      (setq last-command 'ignore)
      (org-air-edit-undo)
      (should (equal before (org-air-r90--text "tasks.org")))
      (should (equal '("inherited" "local")
                     (org-air-r90--sorted-tags child)))
      (should-not (org-air-r90--file-has-tag-p
                   "tasks.org" "Inherited child" "inherited"))
      (setq last-command 'ignore)
      (org-air-edit-redo)
      (should (equal after (org-air-r90--text "tasks.org")))
      (should (equal '("inherited" "local" "shared")
                     (org-air-r90--sorted-tags child)))
      (should-not (org-air-r90--file-has-tag-p
                   "tasks.org" "Inherited child" "inherited")))))

(ert-deftest org-air-r90-hostile-b-post-save-sync-cannot-diverge ()
  "Post-save relocation failure stays exact and round-trips through u/U."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO First heading\n* TODO Later heading\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((first (org-air-r90--item "First heading"))
           (later (org-air-r90--item "Later heading"))
           (first-key (org-air-view--item-source-key first))
           (before (org-air-r90--text "tasks.org")))
      (org-air-r90--mark-title "First heading")
      ;; `save-buffer' succeeds inside the real coordinator.  The injected
      ;; signal is strictly after that save, in cache relocation sync.
      (let ((actual (org-air-r90--hostile-b-snapshot
                     first later first-key)))
        (should (equal actual
                       '(:disk-live t :first-tags ("long_shared_tag")
                         :later-tags nil :first-position t :later-position t
                         :marked nil :history bulk :parts 1))))
      (let ((committed (org-air-r90--text "tasks.org"))
            (record (car org-air-view--edit-ring)))
        (should (org-air-r90--disk-has-tag-p
                 "tasks.org" "First heading" "long_shared_tag"))
        (should (= 1 (length (plist-get record :parts))))
        (should (equal (org-air-r90--file "tasks.org")
                       (plist-get (car (plist-get record :parts)) :file)))
        (setq last-command 'ignore)
        (org-air-edit-undo)
        (should (equal before (org-air-r90--text "tasks.org")))
        (should (equal before (org-air-r90--live-text "tasks.org")))
        (should-not (org-air-r90--sorted-tags first))
        (should-not (org-air-r90--sorted-tags later))
        (should (org-air-r90--cached-position-exact-p
                 first "tasks.org" "First heading"))
        (should (org-air-r90--cached-position-exact-p
                 later "tasks.org" "Later heading"))
        (should-not org-air-view--marked-keys)
        (setq last-command 'ignore)
        (org-air-edit-redo)
        (should (equal committed (org-air-r90--text "tasks.org")))
        (should (equal committed (org-air-r90--live-text "tasks.org")))
        (should (equal '("long_shared_tag")
                       (org-air-r90--sorted-tags first)))
        (should-not (org-air-r90--sorted-tags later))
        (should (org-air-r90--cached-position-exact-p
                 first "tasks.org" "First heading"))
        (should (org-air-r90--cached-position-exact-p
                 later "tasks.org" "Later heading"))
        (should-not org-air-view--marked-keys)))))

;;; r90-13 — one top compound record for one/many/cross-file shapes.

(ert-deftest org-air-r90-13-one-top-record-and-one-part-per-file ()
  "One marked command creates one top record and exactly one part per file."
  (skip-unless (locate-library "org-air"))
  (dolist (shape '(one many cross))
    (org-air-r90--with-board
        (pcase shape
          ('one '(("a.org" . "#+title: a\n\n* TODO A one\n")
                  ("inbox.org" . "#+title: inbox\n")))
          ('many '(("a.org" . "#+title: a\n\n* TODO A one\n* TODO A two\n")
                   ("inbox.org" . "#+title: inbox\n")))
          ('cross '(("a.org" . "#+title: a\n\n* TODO A one\n")
                    ("b.org" . "#+title: b\n\n* TODO B one\n")
                    ("inbox.org" . "#+title: inbox\n"))))
      (dolist (title (pcase shape
                       ('one '("A one"))
                       ('many '("A one" "A two"))
                       ('cross '("A one" "B one"))))
        (org-air-r90--mark-title title))
      (org-air-item-backlog)
      (should (= 1 (length org-air-view--edit-ring)))
      (let ((record (car org-air-view--edit-ring)))
        (should (eq 'bulk (plist-get record :kind)))
        (should (= (if (eq shape 'cross) 2 1)
                   (length (plist-get record :parts))))))))

;;; r90-14 — healthy compound round trips and redo clearing.

(ert-deftest org-air-r90-14-compound-u-U-roundtrip-all-shapes ()
  "Healthy one/many/cross-file compound records byte-round-trip through u/U."
  (skip-unless (locate-library "org-air"))
  (dolist (shape '(one many cross))
    (org-air-r90--with-board
        (pcase shape
          ('one '(("a.org" . "#+title: a\n\n* TODO A one\n")
                  ("inbox.org" . "#+title: inbox\n")))
          ('many '(("a.org" . "#+title: a\n\n* TODO A one\n* TODO A two\n")
                   ("inbox.org" . "#+title: inbox\n")))
          ('cross '(("a.org" . "#+title: a\n\n* TODO A one\n")
                    ("b.org" . "#+title: b\n\n* TODO B one\n")
                    ("inbox.org" . "#+title: inbox\n"))))
      (let* ((names (if (eq shape 'cross) '("a.org" "b.org") '("a.org")))
             (before (mapcar (lambda (name) (cons name (org-air-r90--text name)))
                             names)))
        (dolist (title (pcase shape
                         ('one '("A one"))
                         ('many '("A one" "A two"))
                         ('cross '("A one" "B one"))))
          (org-air-r90--mark-title title))
        (org-air-item-backlog)
        (let ((edited (mapcar (lambda (name) (cons name (org-air-r90--text name)))
                              names)))
          (should-not org-air-view--marked-keys)
          (setq last-command 'ignore)
          (org-air-edit-undo)
          (dolist (entry before)
            (should (equal (cdr entry) (org-air-r90--text (car entry)))))
          (should-not org-air-view--marked-keys)
          (setq last-command 'ignore)
          (org-air-edit-redo)
          (dolist (entry edited)
            (should (equal (cdr entry) (org-air-r90--text (car entry)))))
          (should-not org-air-view--marked-keys)))))
  ;; A fresh edit after compound undo clears redo and marks never resurrect.
  (org-air-r90--with-board org-air-r90--three
    (org-air-r90--mark-title "Alpha task")
    (org-air-item-backlog)
    (org-air-edit-undo)
    (should (= 1 (length org-air-view--edit-redo-ring)))
    (org-air-r90--goto-row "Beta task")
    (org-air-item-backlog)
    (should-not org-air-view--edit-redo-ring)
    (should-not org-air-view--marked-keys)))

;;; r90-15 — compound preflight and hostile runtime residual laws.

(ert-deftest org-air-r90-15-compound-preflight-is-zero-byte-and-nonconsuming ()
  "Dead/tick/head blockers are detected up front and unrelated undo remains."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("a.org" . "#+title: a\n\n* TODO A task\n")
        ("b.org" . "#+title: b\n\n* TODO B task\n")
        ("inbox.org" . "#+title: inbox\n"))
    (dolist (title '("A task" "B task")) (org-air-r90--mark-title title))
    (org-air-item-backlog)
    (let* ((record (car org-air-view--edit-ring))
           (parts (plist-get record :parts))
           (live (plist-get (car parts) :buffer))
           (dead (generate-new-buffer "r90-dead-part")))
      (kill-buffer dead)
      (should (= 3 (length
                    (org-air-view--bulk-history-blockers
                     (list (list :buffer dead :file "/tmp/dead.org" :tick 0)
                           (list :buffer live :file "/tmp/tick.org"
                                 :tick (1- (buffer-chars-modified-tick live))
                                 :undo-head (org-air-view--undo-head live))
                           (list :buffer live :file "/tmp/head.org"
                                 :tick (buffer-chars-modified-tick live)
                                 :undo-head (list 'missing)))))))
      ;; Put one unrelated saved edit atop a committed bulk part.  The bulk
      ;; head/tick preflight must move zero bytes and not consume that step.
      (with-current-buffer live
        (goto-char (point-max))
        (insert "unrelated\n")
        (save-buffer))
      (let ((before-a (org-air-r90--text "a.org"))
            (before-b (org-air-r90--text "b.org")))
        (org-air-edit-undo)
        (should (equal before-a (org-air-r90--text "a.org")))
        (should (equal before-b (org-air-r90--text "b.org"))))
      (with-current-buffer live
        (undo-boundary)
        (undo-only)
        (save-buffer))
      (should-not (string-match-p "unrelated" (org-air-r90--text "a.org"))))))

(ert-deftest org-air-r90-15a-runtime-undo-retains-failed-current-part ()
  "Runtime undo failure retains the failed current and every untouched part."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("a.org" . "#+title: a\n\n* TODO A task\n")
        ("b.org" . "#+title: b\n\n* TODO B task\n")
        ("c.org" . "#+title: c\n\n* TODO C task\n")
        ("inbox.org" . "#+title: inbox\n"))
    (dolist (title '("A task" "B task" "C task")) (org-air-r90--mark-title title))
    (org-air-item-backlog)
    (let* ((b-buffer (find-file-noselect (org-air-r90--file "b.org")))
           (b-before (org-air-r90--live-text "b.org"))
           (b-modified (buffer-modified-p b-buffer))
           (b-undo-list (buffer-local-value 'buffer-undo-list b-buffer))
           (save-orig (symbol-function 'save-buffer))
           messages)
      (cl-letf* (((symbol-function 'save-buffer)
                  (lambda (&rest args)
                    (if (equal (buffer-file-name) (org-air-r90--file "b.org"))
                        (error "runtime undo save failure")
                      (apply save-orig args))))
                 ((symbol-function 'message)
                  (lambda (fmt &rest args)
                    (when fmt (push (apply #'format fmt args) messages)) nil)))
        (org-air-edit-undo))
      (should (seq-some (lambda (msg)
                          (string-match-p
                           "Undo incomplete: 1/3 files reverted; failed b.org"
                           msg))
                        messages))
      ;; The failed current file is restored to exact disk/live, modified,
      ;; and undo-list truth before residual history is decided.
      (should (equal b-before (org-air-r90--text "b.org")))
      (should (equal b-before (org-air-r90--live-text "b.org")))
      (should (eq b-modified (buffer-modified-p b-buffer)))
      (should (eq b-undo-list
                  (buffer-local-value 'buffer-undo-list b-buffer))))
    (should-not (org-air-r90--file-has-tag-p "c.org" "C task" "backlog"))
    (should (org-air-r90--file-has-tag-p "b.org" "B task" "backlog"))
    (should (org-air-r90--file-has-tag-p "a.org" "A task" "backlog"))
    (should-not org-air-view--edit-redo-ring)
    (let* ((residual (car org-air-view--edit-ring))
           (files (mapcar (lambda (part)
                            (file-name-nondirectory (plist-get part :file)))
                          (plist-get residual :parts))))
      ;; The restored failed b.org and untouched a.org remain in canonical
      ;; commit order; no speculative redo branch exists for reverted c.org.
      (should (equal '("a.org" "b.org") files))
      (should-not org-air-view--edit-redo-ring))))

(ert-deftest org-air-r90-15b-runtime-redo-has-truthful-residual ()
  "Runtime redo failure reports exact counts, creates no redo, and keeps truth."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("a.org" . "#+title: a\n\n* TODO A task\n")
        ("b.org" . "#+title: b\n\n* TODO B task\n")
        ("c.org" . "#+title: c\n\n* TODO C task\n")
        ("inbox.org" . "#+title: inbox\n"))
    (dolist (title '("A task" "B task" "C task")) (org-air-r90--mark-title title))
    (org-air-item-backlog)
    (org-air-edit-undo)
    (let* ((b-buffer (find-file-noselect (org-air-r90--file "b.org")))
           (b-before (org-air-r90--live-text "b.org"))
           (b-modified (buffer-modified-p b-buffer))
           (b-undo-list (buffer-local-value 'buffer-undo-list b-buffer))
           (save-orig (symbol-function 'save-buffer))
           messages)
      (cl-letf* (((symbol-function 'save-buffer)
                  (lambda (&rest args)
                    (if (equal (buffer-file-name) (org-air-r90--file "b.org"))
                        (error "runtime redo save failure")
                      (apply save-orig args))))
                 ((symbol-function 'message)
                  (lambda (fmt &rest args)
                    (when fmt (push (apply #'format fmt args) messages)) nil)))
        (org-air-edit-redo))
      (should (seq-some (lambda (msg)
                          (string-match-p
                           "Redo incomplete: 1/3 files reapplied; failed b.org"
                           msg))
                        messages))
      (should (equal b-before (org-air-r90--text "b.org")))
      (should (equal b-before (org-air-r90--live-text "b.org")))
      (should (eq b-modified (buffer-modified-p b-buffer)))
      (should (eq b-undo-list
                  (buffer-local-value 'buffer-undo-list b-buffer))))
    (let* ((parts (plist-get (car org-air-view--edit-ring) :parts))
           (actual
            (list :a-disk (org-air-r90--disk-has-tag-p
                           "a.org" "A task" "backlog")
                  :a-live (and (org-air-r90--file-has-tag-p
                                "a.org" "A task" "backlog") t)
                  :b-disk (org-air-r90--disk-has-tag-p
                           "b.org" "B task" "backlog")
                  :b-live (and (org-air-r90--file-has-tag-p
                                "b.org" "B task" "backlog") t)
                  :c-disk (org-air-r90--disk-has-tag-p
                           "c.org" "C task" "backlog")
                  :redo (and org-air-view--edit-redo-ring t)
                  :residual (mapcar
                             (lambda (part)
                               (file-name-nondirectory
                                (plist-get part :file)))
                             parts))))
      ;; b.org's failed redo is restored absent in live and disk.  Only the
      ;; successful a.org redo is honestly residual-undoable; no false redo.
      (should (equal actual
                     '(:a-disk t :a-live t :b-disk nil :b-live nil
                       :c-disk nil :redo nil :residual ("a.org")))))))

(ert-deftest org-air-r90-15c-operation-failure-preserves-unrelated-undo ()
  "An undo-operation signal restores machinery and leaves older undo usable."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Target task\n")
        ("inbox.org" . "#+title: inbox\n"))
    ;; Seed a real, older user undo group beneath the later bulk edit.
    (let ((source (find-file-noselect (org-air-r90--file "tasks.org"))))
      (with-current-buffer source
        (goto-char (point-max))
        (undo-boundary)
        (insert "# unrelated prior edit\n")
        (undo-boundary)
        (save-buffer))
      (org-air-refresh)
      (org-air-r90--mark-title "Target task")
      (org-air-item-backlog)
      (let* ((before (org-air-r90--live-text "tasks.org"))
             (modified (buffer-modified-p source))
             (undo-list (buffer-local-value 'buffer-undo-list source))
             (undo-orig (symbol-function 'undo-only))
             (tripped nil))
        (cl-letf (((symbol-function 'undo-only)
                   (lambda (&rest args)
                     (let ((result (apply undo-orig args)))
                       (unless tripped
                         (setq tripped t)
                         (error "runtime undo operation failure"))
                       result))))
          (org-air-edit-undo))
        (should tripped)
        (should (equal before (org-air-r90--text "tasks.org")))
        (should (equal before (org-air-r90--live-text "tasks.org")))
        (should (eq modified (buffer-modified-p source)))
        (should (eq undo-list
                    (buffer-local-value 'buffer-undo-list source)))
        (should-not org-air-view--edit-redo-ring)
        (should (= 1 (length (plist-get (car org-air-view--edit-ring)
                                        :parts)))))
      ;; Retry consumes the bulk group only.  The unrelated older group is
      ;; still independently undoable afterward.
      (setq last-command 'ignore)
      (org-air-edit-undo)
      (should-not (org-air-r90--disk-has-tag-p
                   "tasks.org" "Target task" "backlog"))
      (should (string-match-p "unrelated prior edit"
                              (org-air-r90--text "tasks.org")))
      (with-current-buffer source
        (setq last-command 'ignore)
        (undo-boundary)
        (undo-only)
        (save-buffer))
      (should-not (string-match-p "unrelated prior edit"
                                  (org-air-r90--text "tasks.org"))))))

;;; r90-16/17/18 — Backlog collapse, persistence, and explicit lens.

(ert-deftest org-air-r90-16-cold-backlog-header-tab-cycle-no-query ()
  "Cold Backlog is header-only; TAB reveals all then collapses without scans."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Cold A :backlog:\n* TODO Cold B :backlog:\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "Backlog" text))
      (should-not (string-match-p "Cold A" text))
      (should-not (string-match-p "Cold B" text))
      (should-not (string-match-p "and [0-9]+ more" text)))
    (should (= 2 (cdr (assq 'backlog
                            (org-air-view--section-counts org-air-view--items)))))
    (let ((queries 0) (key org-air-view--items-key))
      (cl-letf (((symbol-function 'org-air-query-items)
                 (lambda (&rest _) (cl-incf queries) (error "query")))
                ((symbol-function 'org-air-query-items-in-files)
                 (lambda (&rest _) (cl-incf queries) (error "query"))))
        (goto-char (org-air-view--find-property 'org-air-section 'backlog))
        (org-air-toggle-section)
        (should (string-match-p "Cold A" (buffer-substring-no-properties
                                           (point-min) (point-max))))
        (should (string-match-p "Cold B" (buffer-substring-no-properties
                                           (point-min) (point-max))))
        (goto-char (org-air-view--find-property 'org-air-section 'backlog))
        (org-air-toggle-section))
      (should (= queries 0))
      (should (equal key org-air-view--items-key)))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should-not (string-match-p "Cold A" text))
      (should-not (string-match-p "Cold B" text)))))

(ert-deftest org-air-r90-17-expansion-survives-refresh-and-bookmark-shapes ()
  "Explicit Backlog expansion survives repaint/swap/final and bookmark restore."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Persist A :backlog:\n* TODO Persist B :backlog:\n")
        ("inbox.org" . "#+title: inbox\n"))
    (setq org-air-view--expanded-sections '(backlog))
    (org-air-view--render-current)
    (org-air-view--refresh-repaint)
    (should (memq 'backlog org-air-view--expanded-sections))
    ;; Real no-change and sync-changed refresh tails.
    (org-air-refresh)
    (should (memq 'backlog org-air-view--expanded-sections))
    (with-temp-buffer
      (insert-file-contents (org-air-r90--file "tasks.org"))
      (goto-char (point-max))
      (insert "\n")
      (write-region nil nil (org-air-r90--file "tasks.org") nil 'silent))
    (org-air-refresh)
    (should (memq 'backlog org-air-view--expanded-sections))
    ;; The paced final-swap tail, driven without timers.
    (setq org-air-view--refresh-acc org-air-view--items
          org-air-view--refresh-mtimes org-air-view--items-mtimes
          org-air-view--cache-stale-files nil)
    (org-air-view--refresh-finish)
    (should (memq 'backlog org-air-view--expanded-sections))
    (let ((record (org-air-view--bookmark-make-record)))
      (should (equal '(backlog) (cdr (assq 'org-air-expanded record))))
      (setq org-air-view--expanded-sections nil)
      (org-air-view--bookmark-apply record)
      (should (memq 'backlog org-air-view--expanded-sections)))
    ;; Missing/old expansion fields default honestly collapsed.
    (org-air-view--bookmark-apply '((org-air-version . 1)))
    (should-not org-air-view--expanded-sections)))

(ert-deftest org-air-r90-18-explicit-lens-forces-reveal-knob-honest ()
  "is:backlog forces descriptor/reveal; #backlog and knob nil do not."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Lens task :backlog:\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let ((items org-air-view--items) (queries 0))
      (cl-letf (((symbol-function 'org-air-query-items)
                 (lambda (&rest _) (cl-incf queries) (error "query")))
                ((symbol-function 'org-air-query-items-in-files)
                 (lambda (&rest _) (cl-incf queries) (error "query"))))
        ;; Raw membership never auto-expands.
        (setq org-air-view--expanded-sections nil)
        (org-air-filter-by-tag "#backlog")
        (should-not (memq 'backlog org-air-view--expanded-sections))
        (should-not (string-match-p "Lens task" (buffer-substring-no-properties
                                                  (point-min) (point-max))))
        ;; Exact lens reveals under the normal knob.
        (org-air-filter-by-tag "is:backlog")
        (should (memq 'backlog org-air-view--expanded-sections))
        (should (assq 'backlog (org-air-view--section-descriptors items)))
        (should (string-match-p "Lens task" (buffer-substring-no-properties
                                              (point-min) (point-max))))
        ;; Exact lens also overrides knob nil; clearing it removes both
        ;; ordinary section and Summary row.
        (let ((org-air-show-backlog-section nil))
          (setq org-air-view--expanded-sections nil)
          (org-air-filter-by-tag "is:backlog")
          (should (assq 'backlog (org-air-view--section-descriptors items)))
          (should (memq 'backlog org-air-view--expanded-sections))
          (org-air-filter-clear)
          (should-not (assq 'backlog (org-air-view--section-descriptors items)))
          (should-not (assq 'backlog (org-air-view--section-counts items)))))
      (should (= queries 0))
      (should (eq items org-air-view--items)))))

;;; r90-19 — final-item chrome and direct pane/inspector convergence.

(ert-deftest org-air-r90-19-final-item-lands-on-chrome-before-resync ()
  "The final moved row leaves honest chrome, closes pane, and nils inspector."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Only visible task\n")
        ("inbox.org" . "#+title: inbox\n"))
    (org-air-r90--goto-row "Only visible task")
    (find-file-noselect (org-air-r90--file "tasks.org"))
    (let ((hide 0) setup-states (queries 0))
      (org-air-r90--with-timer-audit timer-observations
        (cl-letf* ((setup-orig (symbol-function 'org-air-view--setup-inspector))
                   ((symbol-function 'org-air-view--setup-inspector)
                    (lambda ()
                      (push (list org-air-view--pending-mutation-landing
                                  (org-air-r90--row-title)) setup-states)
                      (funcall setup-orig)))
                   ((symbol-function 'org-air-view-pane--window-live-p)
                    (lambda () t))
                   ((symbol-function 'org-air-view-pane--hide)
                    (lambda () (cl-incf hide)))
                   (query-orig (symbol-function 'org-air-query-items))
                   ((symbol-function 'org-air-query-items)
                    (lambda (&rest args)
                      (cl-incf queries) (apply query-orig args))))
          (org-air-item-backlog)))
      (should (> hide 0))
      (should (equal '(nil nil) (car setup-states)))
      (should-not (org-air-view--row-property 'org-air-item))
      (should-not (org-air-view-pane--context-at-point))
      (should-not org-air-view--inspector-item)
      (should-not org-air-view--pending-mutation-landing)
      (should (string-match-p "Backlog"
                              (buffer-substring-no-properties
                               (point-min) (point-max))))
      (should (= queries 0)))))

;;; r90-20 — discoverability, key ownership, review collision, and bytes.

(ert-deftest org-air-r90-20-key-help-status-readme-and-empty-bytes ()
  "Board keys are owned/strippable; mark UI teaches bulk; empty bytes restore."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-keybinding-knob t
    (with-temp-buffer
      (org-air-view-mode)
      (should (eq (key-binding (kbd "m")) 'org-air-toggle-mark))
      (should (eq (key-binding (kbd "M")) 'org-air-clear-marks))
      (should (eq (key-binding (kbd "b")) 'org-air-item-backlog))
      (should (eq (key-binding (kbd "t")) 'org-air-set-tag)))
    (with-temp-buffer
      (org-air-review-mode)
      (should (eq (key-binding (kbd "m")) 'org-air-review-cycle-range))))
  (org-air-r90--with-keybinding-knob nil
    (with-temp-buffer
      (org-air-view-mode)
      (pcase-dolist (`(,key . ,command)
                      '(("m" . org-air-toggle-mark)
                        ("M" . org-air-clear-marks)
                        ("b" . org-air-item-backlog)
                        ("t" . org-air-set-tag)))
        ;; `undefined' is Emacs's explicit unbound sentinel and is fine;
        ;; the installer-owned command itself must be absent.
        (should-not (eq (key-binding (kbd key)) command)))))
  (org-air-r90--with-board org-air-r90--three
    (let ((empty-bytes (buffer-substring-no-properties (point-min) (point-max))))
      (org-air-clear-marks)
      (should (equal empty-bytes
                     (buffer-substring-no-properties (point-min) (point-max))))
      (org-air-r90--mark-title "Alpha task")
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "• 1 marked" text))
        (should (string-match-p "backlog all" text))
        (should (string-match-p "tag all" text))
        (should (string-match-p "clear marks" text)))
      (unwind-protect
          (let ((help (org-air-help--render
                       (get-buffer-create "*r90-help*") 'board
                       (current-buffer))))
            (with-current-buffer help
              (let ((text (buffer-substring-no-properties (point-min) (point-max))))
                (should (string-match-p "Marked items" text))
                (should (string-match-p "hidden marks are included" text))
                (should (string-match-p "b backlogs all" text))
                (should (string-match-p "M clears marks" text)))))
        (when (get-buffer "*r90-help*") (kill-buffer "*r90-help*")))
      (org-air-clear-marks)
      (should (equal empty-bytes
                     (buffer-substring-no-properties (point-min) (point-max))))))
  (let ((readme (with-temp-buffer
                  (insert-file-contents
                   (expand-file-name "README.org" default-directory))
                  (buffer-string))))
    (should (string-match-p "header-only" readme))
    (should (string-match-p "hidden marks remain targets" readme))
    (should (string-match-p "set-all semantics" readme))))

;;; r90-21 — single-item real post-write save boundary.

(ert-deftest org-air-r90-21-single-after-save-signal-commits-every-surface ()
  "Single `b' treats a real post-write hook signal as committed success."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha task :one:\n* TODO Beta task :two:\n* TODO Later task :three:\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((alpha (org-air-r90--goto-row "Alpha task"))
           (beta (org-air-r90--item "Beta task"))
           (later (org-air-r90--item "Later task"))
           (source (find-file-noselect (org-air-r90--file "tasks.org")))
           (cache org-air-view--classify-cache)
           (cache-sentinel (list 'stale-classification))
           (invalidations 0)
           (hook-ran 0)
           hook-saw-disk hook-saw-live
           (pane-resyncs 0) pane-final
           (hostile
            (lambda ()
              (cl-incf hook-ran)
              (setq hook-saw-disk
                    (org-air-r90--disk-has-tag-p
                     "tasks.org" "Alpha task" "backlog")
                    hook-saw-live
                    (and (org-air-r90--file-has-tag-p
                          "tasks.org" "Alpha task" "backlog") t))
              (error "hostile single post-write %s" (make-string 300 ?x)))))
      (puthash alpha cache-sentinel cache)
      (with-current-buffer source
        (add-hook 'after-save-hook hostile nil t))
      (let ((before-hooks (with-current-buffer source
                            (copy-tree before-save-hook)))
            (after-hooks (with-current-buffer source
                           (copy-tree after-save-hook))))
        (org-air-r90--record-messages messages
          (cl-letf* ((remhash-orig (symbol-function 'remhash))
                     ((symbol-function 'remhash)
                      (lambda (key table)
                        (when (and (eq table cache) (eq key alpha))
                          (cl-incf invalidations))
                        (funcall remhash-orig key table)))
                     ((symbol-function 'org-air-view--view-pane-update-now)
                      (lambda (board)
                        (cl-incf pane-resyncs)
                        (setq pane-final
                              (with-current-buffer board
                                (org-air-r90--row-title))))))
            (org-air-item-backlog))
          (let ((warnings (org-air-r90--assert-bounded-warnings messages 1)))
            (should (string-match-p "hostile single post-write"
                                    (car warnings)))))
        ;; The real hostile hook ran only after the original disk write.
        (should (= hook-ran 1))
        (should hook-saw-disk)
        (should hook-saw-live)
        (should (equal (org-air-r90--text "tasks.org")
                       (org-air-r90--live-text "tasks.org")))
        (should (equal '("backlog" "one")
                       (org-air-r90--sorted-tags alpha)))
        (org-air-r90--assert-file-cache-exact
         "tasks.org" (list alpha beta later))
        (should (> invalidations 0))
        (should-not (eq cache-sentinel (gethash alpha cache)))
        (should (= 1 (length org-air-view--edit-ring)))
        (should (eq 'in-place
                    (plist-get (car org-air-view--edit-ring) :kind)))
        (should (equal "Beta task" (org-air-r90--row-title)))
        (should (eq beta (org-air-view--row-property 'org-air-item)))
        (should (eq beta org-air-view--inspector-item))
        (should (= pane-resyncs 1))
        (should (equal "Beta task" pane-final))
        (should (equal before-hooks
                       (with-current-buffer source before-save-hook)))
        (should (equal after-hooks
                       (with-current-buffer source after-save-hook)))
        (org-air-r90--assert-history-has-no-snapshot)))))

;;; r90-22 — marked backlog real post-write save boundary.

(ert-deftest org-air-r90-22-marked-backlog-after-save-signal-is-one-commit ()
  "Marked `b' finalizes one committed part after a real hook signal."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha task :one:\n* TODO Beta task :two:\n* TODO Gamma task :three:\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((items (mapcar #'org-air-r90--item
                          '("Alpha task" "Beta task" "Gamma task")))
           (source (find-file-noselect (org-air-r90--file "tasks.org")))
           (before (org-air-r90--text "tasks.org"))
           (hook-ran 0) hook-saw-commit
           toggles
           (hostile
            (lambda ()
              (cl-incf hook-ran)
              (setq hook-saw-commit
                    (and (org-air-r90--disk-has-tag-p
                          "tasks.org" "Alpha task" "backlog")
                         (org-air-r90--disk-has-tag-p
                          "tasks.org" "Beta task" "backlog")))
              (error "hostile marked backlog post-write %s"
                     (make-string 300 ?b)))))
      (org-air-r90--mark-title "Alpha task")
      (org-air-r90--mark-title "Beta task")
      (with-current-buffer source
        (add-hook 'after-save-hook hostile nil t))
      (let ((before-hooks (with-current-buffer source
                            (copy-tree before-save-hook)))
            (after-hooks (with-current-buffer source
                           (copy-tree after-save-hook))))
        (org-air-r90--record-messages messages
          (cl-letf* ((toggle-orig (symbol-function 'org-toggle-tag))
                     ((symbol-function 'org-toggle-tag)
                      (lambda (tag state)
                        (push (org-get-heading t t t t) toggles)
                        (funcall toggle-orig tag state))))
            (org-air-item-backlog))
          (org-air-r90--assert-bounded-warnings messages 1))
        (should (= hook-ran 1))
        (should hook-saw-commit)
        (should (= 2 (length toggles)))
        (should (= 2 (length (delete-dups (copy-sequence toggles)))))
        (should (equal (org-air-r90--text "tasks.org")
                       (org-air-r90--live-text "tasks.org")))
        (org-air-r90--assert-file-cache-exact "tasks.org" items)
        (dolist (title '("Alpha task" "Beta task"))
          (should (= 1 (seq-count
                        (lambda (tag) (equal tag "backlog"))
                        (org-air-r90--live-tags "tasks.org" title t)))))
        (should-not org-air-view--marked-keys)
        (should (= 1 (length org-air-view--edit-ring)))
        (should (eq 'bulk (plist-get (car org-air-view--edit-ring) :kind)))
        (should (= 1 (length
                      (plist-get (car org-air-view--edit-ring) :parts))))
        (should (equal before-hooks
                       (with-current-buffer source before-save-hook)))
        (should (equal after-hooks
                       (with-current-buffer source after-save-hook)))
        (org-air-r90--assert-history-has-no-snapshot)
        ;; The committed history part remains honestly usable; no retry or
        ;; duplicate tag is needed after removing only the hostile user hook.
        (with-current-buffer source
          (remove-hook 'after-save-hook hostile t))
        (let ((committed (org-air-r90--text "tasks.org")))
          (setq last-command 'ignore)
          (org-air-edit-undo)
          (should (equal before (org-air-r90--text "tasks.org")))
          (org-air-r90--assert-file-cache-exact "tasks.org" items)
          (setq last-command 'ignore)
          (org-air-edit-redo)
          (should (equal committed (org-air-r90--text "tasks.org")))
          (org-air-r90--assert-file-cache-exact "tasks.org" items)
          (should-not org-air-view--marked-keys))))))

;;; r90-23 — marked shared tag real post-write save boundary.

(ert-deftest org-air-r90-23-marked-tag-after-save-signal-shares-one-value ()
  "Marked `t' prompts once and commits one exact part after a hook signal."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Tag Alpha :one:\n* TODO Tag Beta :shared:two:\n* TODO Tag Later :three:\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((items (mapcar #'org-air-r90--item
                          '("Tag Alpha" "Tag Beta" "Tag Later")))
           (source (find-file-noselect (org-air-r90--file "tasks.org")))
           (before (org-air-r90--text "tasks.org"))
           (prompts 0) (toggles 0) (hook-ran 0) hook-saw-value
           (hostile
            (lambda ()
              (cl-incf hook-ran)
              (setq hook-saw-value
                    (and (org-air-r90--disk-has-tag-p
                          "tasks.org" "Tag Alpha" "shared")
                         (org-air-r90--disk-has-tag-p
                          "tasks.org" "Tag Beta" "shared")))
              (error "hostile marked tag post-write %s"
                     (make-string 300 ?t)))))
      (org-air-r90--mark-title "Tag Alpha")
      (org-air-r90--mark-title "Tag Beta")
      (with-current-buffer source
        (add-hook 'after-save-hook hostile nil t))
      (let ((before-hooks (with-current-buffer source
                            (copy-tree before-save-hook)))
            (after-hooks (with-current-buffer source
                           (copy-tree after-save-hook))))
        (org-air-r90--record-messages messages
          (cl-letf* (((symbol-function 'read-string)
                      (lambda (&rest _)
                        (cl-incf prompts)
                        "shared"))
                     (toggle-orig (symbol-function 'org-toggle-tag))
                     ((symbol-function 'org-toggle-tag)
                      (lambda (tag state)
                        (cl-incf toggles)
                        (funcall toggle-orig tag state))))
            (org-air-set-tag))
          (org-air-r90--assert-bounded-warnings messages 1))
        (should (= prompts 1))
        (should (= toggles 1))
        (should (= hook-ran 1))
        (should hook-saw-value)
        (should (equal (org-air-r90--text "tasks.org")
                       (org-air-r90--live-text "tasks.org")))
        (org-air-r90--assert-file-cache-exact "tasks.org" items)
        (should (equal '("one" "shared")
                       (org-air-r90--live-tags
                        "tasks.org" "Tag Alpha" t)))
        (should (equal '("shared" "two")
                       (org-air-r90--live-tags
                        "tasks.org" "Tag Beta" t)))
        (should-not org-air-view--marked-keys)
        (should (= 1 (length org-air-view--edit-ring)))
        (should (= 1 (length
                      (plist-get (car org-air-view--edit-ring) :parts))))
        (should (equal before-hooks
                       (with-current-buffer source before-save-hook)))
        (should (equal after-hooks
                       (with-current-buffer source after-save-hook)))
        (org-air-r90--assert-history-has-no-snapshot)
        (with-current-buffer source
          (remove-hook 'after-save-hook hostile t))
        (let ((committed (org-air-r90--text "tasks.org")))
          (setq last-command 'ignore)
          (org-air-edit-undo)
          (should (equal before (org-air-r90--text "tasks.org")))
          (should (equal '("shared" "two")
                         (org-air-r90--live-tags
                          "tasks.org" "Tag Beta" t)))
          (org-air-r90--assert-file-cache-exact "tasks.org" items)
          (setq last-command 'ignore)
          (org-air-edit-redo)
          (should (equal committed (org-air-r90--text "tasks.org")))
          (org-air-r90--assert-file-cache-exact "tasks.org" items)
          (should-not org-air-view--marked-keys))))))

;;; r90-24 — compound undo and redo real post-write save boundaries.

(ert-deftest org-air-r90-24-compound-u-U-after-save-signals-move-rings-honestly ()
  "Compound `u' and `U' both finalize post-write hook signals as success."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Undo Alpha :one:\n* TODO Undo Beta :two:\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((items (mapcar #'org-air-r90--item
                          '("Undo Alpha" "Undo Beta")))
           (source (find-file-noselect (org-air-r90--file "tasks.org")))
           (before (org-air-r90--text "tasks.org")))
      (org-air-r90--mark-title "Undo Alpha")
      (org-air-r90--mark-title "Undo Beta")
      (org-air-item-backlog)
      (let* ((committed (org-air-r90--text "tasks.org"))
             (hook-calls 0)
             hook-observations
             (hostile
              (lambda ()
                (cl-incf hook-calls)
                (push (list hook-calls
                            (org-air-r90--disk-has-tag-p
                             "tasks.org" "Undo Alpha" "backlog")
                            (and (org-air-r90--file-has-tag-p
                                  "tasks.org" "Undo Alpha" "backlog") t))
                      hook-observations)
                (error "hostile compound post-write %s"
                       (make-string 300 ?u)))))
        (with-current-buffer source
          (add-hook 'after-save-hook hostile nil t))
        (let ((before-hooks (with-current-buffer source
                              (copy-tree before-save-hook)))
              (after-hooks (with-current-buffer source
                             (copy-tree after-save-hook))))
          (org-air-r90--record-messages messages
            (setq last-command 'ignore)
            (org-air-edit-undo)
            (should (equal before (org-air-r90--text "tasks.org")))
            (should (equal before (org-air-r90--live-text "tasks.org")))
            (org-air-r90--assert-file-cache-exact "tasks.org" items)
            (should-not org-air-view--edit-ring)
            (should (= 1 (length org-air-view--edit-redo-ring)))
            (should (= 1 (length
                          (plist-get (car org-air-view--edit-redo-ring)
                                     :parts))))
            (should-not (seq-some (lambda (text)
                                    (string-match-p "incomplete" text))
                                  messages))
            (setq last-command 'ignore)
            (org-air-edit-redo)
            (should (equal committed (org-air-r90--text "tasks.org")))
            (should (equal committed (org-air-r90--live-text "tasks.org")))
            (org-air-r90--assert-file-cache-exact "tasks.org" items)
            (should (= 1 (length org-air-view--edit-ring)))
            (should-not org-air-view--edit-redo-ring)
            (should (= 1 (length
                          (plist-get (car org-air-view--edit-ring) :parts))))
            (org-air-r90--assert-bounded-warnings messages 2))
          (should (= hook-calls 2))
          (should (equal '((1 nil nil) (2 t t))
                         (nreverse hook-observations)))
          (should (equal before-hooks
                         (with-current-buffer source before-save-hook)))
          (should (equal after-hooks
                         (with-current-buffer source after-save-hook)))
          (should-not org-air-view--marked-keys)
          (org-air-r90--assert-history-has-no-snapshot))))))

;;; r90-25 — before-save relocation and a second no-scan write.

(ert-deftest org-air-r90-25-before-save-hook-relocates-total-cache ()
  "Ordinary before-save insertion relocates all headings before a second write."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Hook Alpha :one:\n* TODO Hook Middle :two:\n* TODO Hook Later :three:\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((items (mapcar #'org-air-r90--item
                          '("Hook Alpha" "Hook Middle" "Hook Later")))
           (alpha (nth 0 items))
           (later (nth 2 items))
           (source (find-file-noselect (org-air-r90--file "tasks.org")))
           (hook-calls 0)
           (inserted nil)
           (inserter
            (lambda ()
              (cl-incf hook-calls)
              (unless inserted
                (setq inserted t)
                (goto-char (point-min))
                (forward-line 2)
                (insert "# inserted by real before-save hook\n")))))
      (org-air-r90--mark-title "Hook Alpha")
      (with-current-buffer source
        (add-hook 'before-save-hook inserter nil t))
      (let ((before-hooks (with-current-buffer source
                            (copy-tree before-save-hook)))
            (after-hooks (with-current-buffer source
                           (copy-tree after-save-hook))))
        (org-air-item-backlog)
        (should inserted)
        (should (= hook-calls 1))
        (should (equal (org-air-r90--text "tasks.org")
                       (org-air-r90--live-text "tasks.org")))
        (should (string-match-p "inserted by real before-save hook"
                                (org-air-r90--text "tasks.org")))
        (org-air-r90--assert-file-cache-exact "tasks.org" items)
        (should (equal '("backlog" "one")
                       (org-air-r90--sorted-tags alpha)))
        (should (equal '("backlog" "one")
                       (org-air-r90--live-tags
                        "tasks.org" "Hook Alpha" t)))
        ;; The later row now relies on its committed post-hook locator.  Its
        ;; second action must remain cached/no-scan and target that heading.
        (org-air-r90--goto-row "Hook Later")
        (let ((queries 0))
          (cl-letf* ((query-orig (symbol-function 'org-air-query-items))
                     ((symbol-function 'org-air-query-items)
                      (lambda (&rest args)
                        (cl-incf queries)
                        (apply query-orig args)))
                     (files-orig
                      (symbol-function 'org-air-query-items-in-files))
                     ((symbol-function 'org-air-query-items-in-files)
                      (lambda (&rest args)
                        (cl-incf queries)
                        (apply files-orig args))))
            (org-air-item-backlog))
          (should (= queries 0)))
        (should (= hook-calls 2))
        (should (org-air-r90--disk-has-tag-p
                 "tasks.org" "Hook Later" "backlog"))
        (should-not (org-air-r90--disk-has-tag-p
                     "tasks.org" "Hook Middle" "backlog"))
        (org-air-r90--assert-file-cache-exact "tasks.org" items)
        (should (equal before-hooks
                       (with-current-buffer source before-save-hook)))
        (should (equal after-hooks
                       (with-current-buffer source after-save-hook)))
        (should (= 2 (length org-air-view--edit-ring)))
        (org-air-r90--assert-history-has-no-snapshot)
        (should (org-air-r90--cached-position-exact-p
                 later "tasks.org" "Hook Later"))))))

;;; r90-26/27 — tracked unsaved drift and incomplete-locator rejection.

(ert-deftest org-air-r90-26-preexisting-unsaved-drift-uses-tracked-marker ()
  "A pre-action tracked marker maps unrelated unsaved drift without guessing."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Drift Alpha :one:\n* TODO Drift Middle :two:\n* TODO Drift Later :three:\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((items (mapcar #'org-air-r90--item
                          '("Drift Alpha" "Drift Middle" "Drift Later")))
           (later (nth 2 items))
           (durable-before (cdr (org-air-view--item-source-key later)))
           ;; Visiting after board creation runs the R90 source-tracking hook.
           (source (find-file-noselect (org-air-r90--file "tasks.org")))
           (tracked (with-current-buffer source
                      (cdr (assq later org-air-view--source-tracked-locators)))))
      (should (markerp tracked))
      (should (= durable-before (marker-position tracked)))
      (with-current-buffer source
        (goto-char tracked)
        (insert "# pre-existing unsaved user drift\n"))
      ;; Durable UI identity intentionally remains numeric/stale while the
      ;; separate ephemeral exact marker follows the insertion.
      (should (= durable-before
                 (cdr (org-air-view--item-source-key later))))
      (should-not (= durable-before
                     (org-air-r90--actual-heading-position
                      "tasks.org" "Drift Later")))
      (should (= (marker-position tracked)
                 (org-air-r90--actual-heading-position
                  "tasks.org" "Drift Later")))
      (org-air-r90--mark-title "Drift Alpha")
      (org-air-item-backlog)
      (should (equal (org-air-r90--text "tasks.org")
                     (org-air-r90--live-text "tasks.org")))
      (should (string-match-p "pre-existing unsaved user drift"
                              (org-air-r90--text "tasks.org")))
      (should (org-air-r90--disk-has-tag-p
               "tasks.org" "Drift Alpha" "backlog"))
      (should-not (org-air-r90--disk-has-tag-p
                   "tasks.org" "Drift Later" "backlog"))
      (org-air-r90--assert-file-cache-exact "tasks.org" items)
      (should-not org-air-view--marked-keys)
      (should (= 1 (length org-air-view--edit-ring)))
      (should (= 1 (length
                    (plist-get (car org-air-view--edit-ring) :parts))))
      (org-air-r90--assert-history-has-no-snapshot))))

(ert-deftest org-air-r90-27-incomplete-locator-rejects-whole-file-prewrite ()
  "An untracked incomplete locator rejects the file before any source edit."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Reject Alpha :one:\n* TODO Reject Later :two:\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((alpha (org-air-r90--item "Reject Alpha"))
           (later (org-air-r90--item "Reject Later"))
           (before (org-air-r90--text "tasks.org"))
           (file (org-air-r90--file "tasks.org"))
           ;; `point-max' is in range but not a heading.
           (bad-position (1+ (length before)))
           (saves 0) (toggles 0))
      (org-air-r90--mark-title "Reject Alpha")
      ;; This intentionally models an old/incomplete cached shape.  If query
      ;; happened to leave the file visited, explicitly release only Later's
      ;; ephemeral marker so it cannot mask the malformed durable locator.
      (when-let* ((source (find-buffer-visiting file)))
        (with-current-buffer source
          (when-let* ((entry (assq later org-air-view--source-tracked-locators)))
            (set-marker (cdr entry) nil)
            (setq org-air-view--source-tracked-locators
                  (assq-delete-all later org-air-view--source-tracked-locators)))))
      (setf (org-air-item-marker later) (cons file bad-position))
      (org-air-r90--record-messages messages
        (cl-letf* ((save-orig (symbol-function 'save-buffer))
                   ((symbol-function 'save-buffer)
                    (lambda (&rest args)
                      (cl-incf saves)
                      (apply save-orig args)))
                   (toggle-orig (symbol-function 'org-toggle-tag))
                   ((symbol-function 'org-toggle-tag)
                    (lambda (&rest args)
                      (cl-incf toggles)
                      (apply toggle-orig args))))
          (org-air-item-backlog))
        (should (seq-some (lambda (text)
                            (string-match-p "run g r" text))
                          messages)))
      (should (= saves 0))
      (should (= toggles 0))
      (should (equal before (org-air-r90--text "tasks.org")))
      (should (equal before (org-air-r90--live-text "tasks.org")))
      (should-not (org-air-r90--disk-has-tag-p
                   "tasks.org" "Reject Alpha" "backlog"))
      (should (equal (list (org-air-view--item-source-key alpha))
                     org-air-view--marked-keys))
      (should-not org-air-view--edit-ring)
      (should-not org-air-view--edit-redo-ring))))

;;; r90-28 — permissive day focus for single and marked backlog.

(ert-deftest org-air-r90-28-day-backlog-retains-same-section-identity ()
  "Dated single/marked `b' keeps the exact Scheduled or Deadline row."
  (skip-unless (locate-library "org-air"))
  (dolist (kind '(scheduled deadline))
    (dolist (marked '(nil t))
      (let ((specs
             (list
              (cons "tasks.org"
                    (if (eq kind 'scheduled)
                        "#+title: tasks\n\n* TODO Day Alpha\nSCHEDULED: <2026-06-15 Mon>\n* TODO Day Beta\nSCHEDULED: <2026-06-15 Mon>\n"
                      "#+title: tasks\n\n* TODO Day Alpha\nDEADLINE: <2026-06-15 Mon>\n* TODO Day Beta\nDEADLINE: <2026-06-15 Mon>\n"))
              (cons "inbox.org" "#+title: inbox\n"))))
        (org-air-r90--with-board specs
          (org-air-view-day org-air-test-now)
          (let* ((item (org-air-r90--goto-row "Day Alpha"))
                 (key (org-air-view--item-source-key item))
                 (section (org-air-r90--day-group-at-point))
                 (queries 0) pane-key setup-key)
            (should (equal (if (eq kind 'scheduled)
                               "Scheduled" "Deadline")
                           section))
            (when marked
              (org-air-toggle-mark)
              (org-air-r90--goto-row "Day Alpha"))
            ;; Provision source mode before constructor auditing; command-time
            ;; intrinsic Org cache timers remain classified by identity.
            (find-file-noselect (org-air-r90--file "tasks.org"))
            (org-air-r90--with-timer-audit timer-observations
              (cl-letf* ((query-orig (symbol-function 'org-air-query-items))
                         ((symbol-function 'org-air-query-items)
                          (lambda (&rest args)
                            (cl-incf queries)
                            (apply query-orig args)))
                         (files-orig
                          (symbol-function 'org-air-query-items-in-files))
                         ((symbol-function 'org-air-query-items-in-files)
                          (lambda (&rest args)
                            (cl-incf queries)
                            (apply files-orig args)))
                         (ql-orig (symbol-function 'org-ql-select))
                         ((symbol-function 'org-ql-select)
                          (lambda (&rest args)
                            (cl-incf queries)
                            (apply ql-orig args)))
                         (refresh-orig (symbol-function 'org-air-refresh))
                         ((symbol-function 'org-air-refresh)
                          (lambda (&rest args)
                            (cl-incf queries)
                            (apply refresh-orig args)))
                         (setup-orig
                          (symbol-function 'org-air-view--setup-inspector))
                         ((symbol-function 'org-air-view--setup-inspector)
                          (lambda ()
                            (funcall setup-orig)
                            (setq setup-key
                                  (when-let* ((at (org-air-view--row-property
                                                  'org-air-item)))
                                    (org-air-view--item-source-key at)))))
                         ((symbol-function 'org-air-view--view-pane-update-now)
                          (lambda (board)
                            (setq pane-key
                                  (with-current-buffer board
                                    (when-let* ((at
                                                 (org-air-view--row-property
                                                  'org-air-item)))
                                      (org-air-view--item-source-key at)))))))
                (org-air-item-backlog)))
            (should (= queries 0))
            (should (equal key
                           (org-air-view--item-source-key
                            (org-air-view--row-property 'org-air-item))))
            (should (equal section (org-air-r90--day-group-at-point)))
            (should (eq item org-air-view--inspector-item))
            (should (equal key setup-key))
            (should (equal key pane-key))
            ;; Under batch the rail is inline; its inspector region therefore
            ;; shares this exact final identity and visibly names the item.
            (should (markerp org-air-view--inspector-beg))
            (should (string-match-p
                     "Day Alpha"
                     (buffer-substring-no-properties
                      org-air-view--inspector-beg
                      org-air-view--inspector-end)))
            (should (org-air-r90--disk-has-tag-p
                     "tasks.org" "Day Alpha" "backlog"))
            (should-not org-air-view--marked-keys)))))))

;;; r90-29 — pre-write restoration, hook cleanup, and reentrant hygiene.

(ert-deftest org-air-r90-29-save-boundary-hygiene-and-no-retained-snapshot ()
  "Save attempts restore only pre-write failure and always remove sentinels."
  (skip-unless (locate-library "org-air"))
  ;; A true pre-write failure restores exact source/undo/visited state and
  ;; leaves the failed mark available without recording success.
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Fail Alpha :one:\n* TODO Fail Later :two:\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((alpha (org-air-r90--item "Fail Alpha"))
           (source (find-file-noselect (org-air-r90--file "tasks.org")))
           (before-disk (org-air-r90--text "tasks.org"))
           (before-live (org-air-r90--live-text "tasks.org"))
           (before-modified (buffer-modified-p source))
           (before-undo (buffer-local-value 'buffer-undo-list source))
           (before-saved-size (buffer-local-value 'buffer-saved-size source))
           (before-modtime (with-current-buffer source
                             (copy-tree (visited-file-modtime))))
           (before-hooks (with-current-buffer source
                           (copy-tree before-save-hook)))
           (after-hooks (with-current-buffer source
                          (copy-tree after-save-hook)))
           (saves 0))
      (org-air-r90--mark-title "Fail Alpha")
      (cl-letf (((symbol-function 'save-buffer)
                 (lambda (&rest _)
                   (cl-incf saves)
                   (error "true pre-write save failure"))))
        (org-air-item-backlog))
      (should (= saves 1))
      (should (equal before-disk (org-air-r90--text "tasks.org")))
      (should (equal before-live (org-air-r90--live-text "tasks.org")))
      (should (eq before-modified (buffer-modified-p source)))
      (should (eq before-undo
                  (buffer-local-value 'buffer-undo-list source)))
      (should (equal before-saved-size
                     (buffer-local-value 'buffer-saved-size source)))
      (should (equal before-modtime
                     (with-current-buffer source (visited-file-modtime))))
      (should (equal before-hooks
                     (with-current-buffer source before-save-hook)))
      (should (equal after-hooks
                     (with-current-buffer source after-save-hook)))
      (should-not (member "backlog" (org-air-item-tags alpha)))
      (should (equal (list (org-air-view--item-source-key alpha))
                     org-air-view--marked-keys))
      (should-not org-air-view--edit-ring)
      (should-not org-air-view--edit-redo-ring)))
  ;; Reentrant same-buffer attempts are token-scoped: the inner preparation
  ;; and after sentinel cannot satisfy the outer attempt, and both hook pairs
  ;; disappear after two successful writes.
  (org-air-r90--with-corpus
      '(("tasks.org" . "#+title: tasks\n\n* TODO Nested save\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((source (find-file-noselect (org-air-r90--file "tasks.org")))
           (nested nil) inner-result outer-result
           (inner-prepares 0) (outer-prepares 0))
      (with-current-buffer source
        (org-mode)
        (goto-char (point-max))
        (insert "# nested/reentrant attempt\n")
        (let ((before-hooks (copy-tree before-save-hook))
              (after-hooks (copy-tree after-save-hook)))
          (setq outer-result
                (org-air-view--save-attempt
                 (lambda ()
                   (cl-incf outer-prepares)
                   (unless nested
                     (setq nested t
                           inner-result
                           (org-air-view--save-attempt
                            (lambda ()
                              (cl-incf inner-prepares)
                              'inner-state))))
                   'outer-state)))
          (should nested)
          (should (= inner-prepares 1))
          (should (= outer-prepares 1))
          (should (plist-get inner-result :committed))
          (should (eq 'inner-state (plist-get inner-result :state)))
          (should-not (plist-get inner-result :error))
          (should (plist-get outer-result :committed))
          (should (eq 'outer-state (plist-get outer-result :state)))
          (should-not (plist-get outer-result :error))
          (should (equal before-hooks before-save-hook))
          (should (equal after-hooks after-save-hook))
          (should-not org-air-view--save-attempt-token)))
      (should (equal (org-air-r90--text "tasks.org")
                     (org-air-r90--live-text "tasks.org")))
      (should (string-match-p "nested/reentrant attempt"
                              (org-air-r90--text "tasks.org")))
      (should-not org-air-view--edit-ring)
      (should-not org-air-view--edit-redo-ring))))

;;; r90-30 — exact retry identity must stay bounded and source-free.

(ert-deftest org-air-r90-30-history-identity-is-bounded-and-source-free ()
  "History metadata must not retain an undo-list tail or deleted source text.
A real older undo group contains a distinctive 128 KiB deletion before an
org-air write whose mutating/signalling `after-save-hook' requires retryable
identity.  Starting from a copy of the ring record with its already-known
`:buffer' reference removed, the transitive metadata must remain bounded,
must not alias any raw `buffer-undo-list' tail, and must not reach those
source bytes.  A tail pointer is rejected even if the current corpus happens
to be small: it keeps every older undo entry alive after buffer death."
  (skip-unless (locate-library "org-air"))
  (let* ((needle "R90-BOUNDED-HISTORY-DISTINCTIVE-DELETED-SOURCE-")
         (large (concat needle (make-string (* 128 1024) ?X) "\n")))
    (org-air-r90--with-board
        `(("tasks.org" .
           ,(concat "#+title: tasks\n\n* TODO Alpha task :one:\n"
                    large
                    "* TODO Beta task :two:\n"))
          ("inbox.org" . "#+title: inbox\n"))
      (let* ((source (find-file-noselect (org-air-r90--file "tasks.org")))
             (hook (lambda ()
                     (goto-char (point-min))
                     (insert "# ahead user after-save edit\n")
                     (error "after-save mutated live bytes"))))
        ;; Seed an unrelated OLDER deletion entry, then refresh so the source
        ;; generation and mtime baseline are truthful before org-air writes.
        (with-current-buffer source
          (goto-char (point-min))
          (search-forward needle)
          (let ((beginning (line-beginning-position)))
            (forward-line 1)
            (delete-region beginning (point)))
          (undo-boundary)
          (save-buffer))
        (org-air-refresh)
        (org-air-r90--goto-row "Alpha task")
        (with-current-buffer source
          (add-hook 'after-save-hook hook nil t))
        (unwind-protect
            (org-air-item-backlog)
          (with-current-buffer source
            (remove-hook 'after-save-hook hook t)))
        (let* ((record (car org-air-view--edit-ring))
               ;; Exclude the one explicitly accepted live-buffer reference;
               ;; every other record-local edge remains under audit.
               (metadata (plist-put (copy-sequence record) :buffer nil))
               (undo-list (buffer-local-value 'buffer-undo-list source)))
          ;; The global record survives source death.  Retaining UNDO-LIST
          ;; here permits the identity-alias check; METADATA's independent
          ;; graph must itself neither alias the tail nor reach its strings.
          (with-current-buffer source
            (set-buffer-modified-p nil))
          (kill-buffer source)
          (garbage-collect)
          (should-not (buffer-live-p source))
          (let ((summary (org-air-r90--metadata-reachability
                          metadata needle undo-list)))
            (ert-info ((format "transitive history metadata: %S" summary))
              (should-not (plist-get summary :undo-tail-reachable))
              (should-not
               (plist-get summary :distinctive-source-reachable))
              (should (zerop (plist-get summary :buffers)))
              (should (zerop (plist-get summary :markers)))
              (should (< (plist-get summary :string-bytes) 8192))
              (should (< (plist-get summary :conses) 256)))))))))

;;;; r90-31 — native v6 roundtrip and discarded v7 clean miss.

(ert-deftest org-air-r90-31-cache-v6-native-projection-and-v7-clean-miss ()
  "Native v6 tags/literal titles round-trip; unshipped v7 cleanly misses."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-corpus
      '(("tasks.org" . "#+title: tasks\n\n* TODO Native task :shared_tag:\n* TODO Literal hyphen :shared-tag:\n* TODO Literal slash :not/a/tag:\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((files (org-air-query-files))
           (items (org-air-query-items))
           (native (org-air-r90--item "Native task" items))
           (hyphen (org-air-r90--item "Literal hyphen" items))
           (slash (org-air-r90--item "Literal slash" items))
           (mtimes (org-air-view--mtimes-snapshot files)))
      (should (= 6 org-air-view--cache-version))
      (should (equal "Native task" (org-air-item-title native)))
      (should (equal '("shared_tag") (org-air-item-tags native)))
      ;; Org does not parse these suffixes as tags; they are literal titles.
      (should (equal "Literal hyphen :shared-tag:"
                     (org-air-item-title hyphen)))
      (should (equal "Literal slash :not/a/tag:"
                     (org-air-item-title slash)))
      (should-not (org-air-item-tags hyphen))
      (should-not (org-air-item-tags slash))
      (org-air-view--cache-write items mtimes)
      (let* ((data (org-air-view--cache-read))
             (hydrated (plist-get data :items)))
        (should (= 6 (plist-get data :version)))
        (should (equal '("shared_tag")
                       (org-air-item-tags
                        (org-air-r90--item "Native task" hydrated))))
        (should (equal "Literal hyphen :shared-tag:"
                       (org-air-item-title
                        (org-air-r90--item "Literal hyphen" hydrated))))
        (should (equal "Literal slash :not/a/tag:"
                       (org-air-item-title
                        (org-air-r90--item "Literal slash" hydrated)))))
      ;; The discarded experimental broad-projection schema was never shipped.
      (let ((print-length nil) (print-level nil) (print-circle t))
        (write-region
         (prin1-to-string
          (list :version 7 :key (org-air-view--cache-key)
                :mtimes mtimes :file-meta nil :visits nil
                :items (mapcar #'org-air-view--item-serialise items)))
         nil org-air-cache-file nil 'silent))
      (should-not (org-air-view--cache-read))
      (should-not (org-air-view--cache-load)))))

;;;; r90-32..35 — mutating after-save hooks and deep bounded history.

(ert-deftest org-air-r90-32-single-mutating-hook-blocks-then-roundtrips ()
  "Single `b' keeps hook text unsaved; u blocks, resolves, then u/U are exact."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha task :one:\n* TODO Beta task :two:\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((before (org-air-r90--text "tasks.org"))
           (alpha (org-air-r90--goto-row "Alpha task"))
           (source (find-file-noselect (org-air-r90--file "tasks.org")))
           (label "single user after-save mutation")
           (line (format "# %s\n" label))
           (hook (org-air-r90--mutating-save-hook label))
           warnings)
      (with-current-buffer source
        (add-hook 'after-save-hook hook nil t))
      (let ((before-hooks (with-current-buffer source
                            (copy-tree before-save-hook)))
            (after-hooks (with-current-buffer source
                           (copy-tree after-save-hook))))
        (cl-letf (((symbol-function 'display-warning)
                   (lambda (&rest args) (push args warnings))))
          (org-air-item-backlog))
        (should warnings)
        (should (equal before-hooks
                       (with-current-buffer source before-save-hook)))
        (should (equal after-hooks
                       (with-current-buffer source after-save-hook)))
        (should-not (with-current-buffer source
                      org-air-view--save-attempt-token)))
      (with-current-buffer source
        (remove-hook 'after-save-hook hook t))
      (let* ((disk (org-air-r90--text "tasks.org"))
             (live (org-air-r90--live-text "tasks.org"))
             (record (car org-air-view--edit-ring)))
        (should (org-air-r90--disk-has-tag-p
                 "tasks.org" "Alpha task" "backlog"))
        (should (equal live (concat line disk)))
        (should (buffer-modified-p source))
        (should (equal '("backlog" "one")
                       (org-air-r90--sorted-tags alpha)))
        ;; Durable slots describe committed disk; the separate live locator
        ;; follows the legitimate unsaved hook insertion.
        (should (= (cdr (org-air-view--item-source-key alpha))
                   (org-air-r90--disk-heading-position
                    "tasks.org" "Alpha task")))
        (should (plist-member record :expected-undo))
        (org-air-r90--assert-history-token
         (plist-get record :expected-undo))
        (should (gethash record org-air-view--cache-sync-history))
        (org-air-r90--assert-history-has-no-snapshot)
        ;; Immediate org-air undo changes no byte, saves nothing, keeps the
        ;; exact record, and manufactures no redo branch.
        (let ((saves 0))
          (cl-letf (((symbol-function 'save-buffer)
                     (lambda (&rest _) (cl-incf saves))))
            (org-air-edit-undo))
          (should (= saves 0)))
        (should (equal disk (org-air-r90--text "tasks.org")))
        (should (equal live (org-air-r90--live-text "tasks.org")))
        (should (eq record (car org-air-view--edit-ring)))
        (should-not org-air-view--edit-redo-ring)
        ;; Once the user independently undoes only their insertion, the exact
        ;; org-air step succeeds and its redo is honest.
        (org-air-r90--resolve-hook-insertion source label)
        (should (equal disk (org-air-r90--live-text "tasks.org")))
        (org-air-edit-undo)
        (should (equal before (org-air-r90--text "tasks.org")))
        (should (equal before (org-air-r90--live-text "tasks.org")))
        (should-not org-air-view--edit-ring)
        (should (= 1 (length org-air-view--edit-redo-ring)))
        (org-air-edit-redo)
        (should (equal disk (org-air-r90--text "tasks.org")))
        (should (equal disk (org-air-r90--live-text "tasks.org")))
        (should (= 1 (length org-air-view--edit-ring)))
        (should-not org-air-view--edit-redo-ring)))))

(ert-deftest org-air-r90-33-marked-b-and-t-mutating-hooks-resolve-exactly ()
  "Initial marked b/t commits one part while its hook edit blocks inverse u."
  (skip-unless (locate-library "org-air"))
  (dolist (action '(backlog tag))
    (org-air-r90--with-board
        '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha\n* TODO Beta\n")
          ("inbox.org" . "#+title: inbox\n"))
      (dolist (title '("Alpha" "Beta"))
        (org-air-r90--mark-title title))
      (let* ((before (org-air-r90--text "tasks.org"))
             (source (find-file-noselect (org-air-r90--file "tasks.org")))
             (label (format "initial marked %s hook" action))
             (hook (org-air-r90--mutating-save-hook label)))
        (with-current-buffer source
          (add-hook 'after-save-hook hook nil t))
        (if (eq action 'backlog)
            (org-air-item-backlog)
          (cl-letf (((symbol-function 'read-string)
                     (lambda (&rest _) "shared_tag")))
            (org-air-set-tag)))
        (with-current-buffer source
          (remove-hook 'after-save-hook hook t))
        (let* ((disk (org-air-r90--text "tasks.org"))
               (live (org-air-r90--live-text "tasks.org"))
               (record (car org-air-view--edit-ring))
               (part (car (plist-get record :parts))))
          (dolist (title '("Alpha" "Beta"))
            (should (org-air-r90--disk-has-tag-p
                     "tasks.org" title
                     (if (eq action 'backlog) "backlog" "shared_tag"))))
          (should (string-match-p (regexp-quote label) live))
          (should (buffer-modified-p source))
          (should (eq 'bulk (plist-get record :kind)))
          (should (= 1 (length (plist-get record :parts))))
          (org-air-r90--assert-history-token (plist-get part :undo-head))
          (org-air-r90--assert-history-token
           (plist-get part :expected-undo))
          (org-air-r90--assert-history-has-no-snapshot)
          (let ((saves 0))
            (cl-letf (((symbol-function 'save-buffer)
                       (lambda (&rest _) (cl-incf saves))))
              (org-air-edit-undo))
            (should (= saves 0)))
          (should (equal disk (org-air-r90--text "tasks.org")))
          (should (equal live (org-air-r90--live-text "tasks.org")))
          (should (eq record (car org-air-view--edit-ring)))
          (should-not org-air-view--edit-redo-ring)
          (org-air-r90--resolve-hook-insertion source label)
          (org-air-edit-undo)
          (should (equal before (org-air-r90--text "tasks.org")))
          (should (= 1 (length org-air-view--edit-redo-ring)))
          (org-air-edit-redo)
          (should (equal disk (org-air-r90--text "tasks.org")))
          (should-not org-air-view--edit-redo-ring))))))

(ert-deftest org-air-r90-34-compound-u-U-hook-directions-block-safely ()
  "Compound u/U commit their direction; the inverse blocks on hook text."
  (skip-unless (locate-library "org-air"))
  (dolist (direction '(undo redo))
    (org-air-r90--with-board
        '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha\n* TODO Beta\n")
          ("inbox.org" . "#+title: inbox\n"))
      (dolist (title '("Alpha" "Beta"))
        (org-air-r90--mark-title title))
      (org-air-item-backlog)
      (when (eq direction 'redo)
        (org-air-edit-undo))
      (let* ((source (find-file-noselect (org-air-r90--file "tasks.org")))
             (label (format "compound %s user hook" direction))
             (hook (org-air-r90--mutating-save-hook label)))
        (with-current-buffer source
          (add-hook 'after-save-hook hook nil t))
        (if (eq direction 'undo)
            (org-air-edit-undo)
          (org-air-edit-redo))
        (with-current-buffer source
          (remove-hook 'after-save-hook hook t))
        (let* ((source-ring (if (eq direction 'undo)
                                org-air-view--edit-redo-ring
                              org-air-view--edit-ring))
               (record (car source-ring))
               (disk (org-air-r90--text "tasks.org"))
               (live (org-air-r90--live-text "tasks.org"))
               (undo-order (copy-sequence org-air-view--edit-ring))
               (redo-order (copy-sequence org-air-view--edit-redo-ring))
               (saves 0))
          (should (string-match-p (regexp-quote label) live))
          (should (eq (eq direction 'redo)
                      (org-air-r90--disk-has-tag-p
                       "tasks.org" "Alpha" "backlog")))
          (cl-letf (((symbol-function 'save-buffer)
                     (lambda (&rest _) (cl-incf saves))))
            (if (eq direction 'undo)
                (org-air-edit-redo)
              (org-air-edit-undo)))
          (should (= saves 0))
          (should (equal disk (org-air-r90--text "tasks.org")))
          (should (equal live (org-air-r90--live-text "tasks.org")))
          (should (org-air-r90--same-object-order-p
                   org-air-view--edit-ring undo-order))
          (should (org-air-r90--same-object-order-p
                   org-air-view--edit-redo-ring redo-order))
          (should (eq record (car source-ring)))
          (should (buffer-modified-p source)))))))

(ert-deftest org-air-r90-35-deep-single-and-compound-order-never-speculates ()
  "Older same-buffer records block without ring reorder, then resolve exactly."
  (skip-unless (locate-library "org-air"))
  ;; Two ordinary records: undoing the newer record with a hook arms the older.
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha\n* TODO Beta\n")
        ("inbox.org" . "#+title: inbox\n"))
    (org-air-r90--goto-row "Alpha")
    (org-air-item-schedule "2026-08-01")
    (org-air-r90--goto-row "Beta")
    (org-air-item-schedule "2026-08-02")
    (let* ((source (find-file-noselect (org-air-r90--file "tasks.org")))
           (label "deep single hook")
           (hook (org-air-r90--mutating-save-hook label)))
      (with-current-buffer source (add-hook 'after-save-hook hook nil t))
      (org-air-edit-undo)
      (with-current-buffer source (remove-hook 'after-save-hook hook t))
      (let ((disk (org-air-r90--text "tasks.org"))
            (live (org-air-r90--live-text "tasks.org"))
            (undo-order (copy-sequence org-air-view--edit-ring))
            (redo-order (copy-sequence org-air-view--edit-redo-ring)))
        (org-air-edit-undo)
        (should (equal disk (org-air-r90--text "tasks.org")))
        (should (equal live (org-air-r90--live-text "tasks.org")))
        (should (org-air-r90--same-object-order-p
                 org-air-view--edit-ring undo-order))
        (should (org-air-r90--same-object-order-p
                 org-air-view--edit-redo-ring redo-order)))
      (org-air-r90--resolve-hook-insertion source label)
      (org-air-edit-undo)
      (let ((text (org-air-r90--text "tasks.org")))
        (should-not (string-match-p "2026-08-01" text))
        (should-not (string-match-p "2026-08-02" text)))))
  ;; An older ordinary record beneath a compound record obeys the same law.
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha\n* TODO Beta\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let ((alpha (org-air-r90--item "Alpha")))
      (org-air-view--at-item-source alpha "older Alpha schedule"
        (org-schedule nil "2026-08-01")))
    (dolist (title '("Alpha" "Beta"))
      (org-air-r90--mark-title title))
    (org-air-item-backlog)
    (let* ((source (find-file-noselect (org-air-r90--file "tasks.org")))
           (label "deep compound hook")
           (hook (org-air-r90--mutating-save-hook label)))
      (with-current-buffer source (add-hook 'after-save-hook hook nil t))
      (org-air-edit-undo)
      (with-current-buffer source (remove-hook 'after-save-hook hook t))
      (let ((disk (org-air-r90--text "tasks.org"))
            (live (org-air-r90--live-text "tasks.org"))
            (undo-order (copy-sequence org-air-view--edit-ring))
            (redo-order (copy-sequence org-air-view--edit-redo-ring)))
        (org-air-edit-undo)
        (should (equal disk (org-air-r90--text "tasks.org")))
        (should (equal live (org-air-r90--live-text "tasks.org")))
        (should (org-air-r90--same-object-order-p
                 org-air-view--edit-ring undo-order))
        (should (org-air-r90--same-object-order-p
                 org-air-view--edit-redo-ring redo-order)))
      (org-air-r90--resolve-hook-insertion source label)
      (org-air-edit-undo)
      (should-not (string-match-p "2026-08-01"
                                  (org-air-r90--text "tasks.org"))))))

;;;; r90-36/37 — compound metadata bounds and weak registry lifecycle.

(ert-deftest org-air-r90-36-compound-history-identities-are-bounded-tokens ()
  "Compound expected tail/head identities retain no raw undo graph or source."
  (skip-unless (locate-library "org-air"))
  (let* ((needle "R90-COMPOUND-BOUNDED-DISTINCTIVE-")
         (large (concat needle (make-string (* 128 1024) ?Z) "\n")))
    (org-air-r90--with-board
        `(("tasks.org" .
           ,(concat "#+title: tasks\n\n* TODO Alpha\n" large
                    "* TODO Beta\n"))
          ("inbox.org" . "#+title: inbox\n"))
      (let* ((source (find-file-noselect (org-air-r90--file "tasks.org")))
             (label "compound bounded hook")
             (hook (org-air-r90--mutating-save-hook label)))
        (with-current-buffer source
          (goto-char (point-min))
          (search-forward needle)
          (let ((beg (line-beginning-position)))
            (forward-line 1)
            (delete-region beg (point)))
          (undo-boundary)
          (save-buffer))
        (org-air-refresh)
        (dolist (title '("Alpha" "Beta"))
          (org-air-r90--mark-title title))
        (with-current-buffer source (add-hook 'after-save-hook hook nil t))
        (unwind-protect
            (org-air-item-backlog)
          (with-current-buffer source
            (remove-hook 'after-save-hook hook t)))
        (let* ((record (car org-air-view--edit-ring))
               (part (car (plist-get record :parts)))
               (metadata (org-air-r90--history-metadata-without-buffers
                          record))
               (undo-list (buffer-local-value 'buffer-undo-list source)))
          (org-air-r90--assert-history-token (plist-get part :undo-head))
          (should (eq 'head
                      (org-air-view--history-token-projection
                       (plist-get part :undo-head))))
          (org-air-r90--assert-history-token
           (plist-get part :expected-undo))
          (should-not (org-air-view--history-token-projection
                       (plist-get part :expected-undo)))
          (org-air-r90--assert-history-has-no-snapshot)
          (with-current-buffer source (set-buffer-modified-p nil))
          (kill-buffer source)
          (org-air-r90--force-gc)
          (let ((summary (org-air-r90--metadata-reachability
                          metadata needle undo-list)))
            (ert-info ((format "compound transitive metadata: %S" summary))
              (should-not (plist-get summary :undo-tail-reachable))
              (should-not (plist-get summary :undo-head-reachable))
              (should-not
               (plist-get summary :distinctive-source-reachable))
              (should (zerop (plist-get summary :buffers)))
              (should (zerop (plist-get summary :markers)))
              (should (< (plist-get summary :string-bytes) 8192))
              (should (< (plist-get summary :conses) 256)))))))))

(ert-deftest org-air-r90-37-weak-registry-both-live-and-lifecycle-bounded ()
  "Weak mappings need both sides; kill/clear/truncate stay bounded; GC is safe."
  (skip-unless (locate-library "org-air"))
  ;; Empirically distinguish `key-and-value': either independently dropped
  ;; side removes the mapping while the opposite side remains held here.
  (let ((org-air-view--history-identity-registry
         (make-hash-table :test #'eq :weakness 'key-and-value)))
    (let ((held-value (org-air-r90--registry-drop-key-setup)))
      (org-air-r90--force-gc)
      (should held-value)
      (should (zerop (hash-table-count
                      org-air-view--history-identity-registry))))
    (let ((held-key (org-air-r90--registry-drop-value-setup)))
      (org-air-r90--force-gc)
      (should (org-air-view--history-token-p held-key))
      (should (zerop (hash-table-count
                      org-air-view--history-identity-registry))))
    ;; Explicit ring truncation/clear cleans eagerly and repeated cycles do
    ;; not accumulate side-registry entries.
    (let ((org-air-view--edit-ring nil)
          (org-air-view--edit-redo-ring nil))
      (dotimes (index 80)
        (let* ((raw (list 'synthetic-undo-tail index))
               (token (org-air-view--history-identity-register raw))
               (record (list :desc (format "synthetic %d" index)
                             :buffer nil :file nil :kind 'in-place
                             :tick index :time nil
                             :expected-undo token)))
          (push record org-air-view--edit-ring)
          (org-air-view--history-ring-truncate
           'org-air-view--edit-ring)
          (should (<= (hash-table-count
                       org-air-view--history-identity-registry)
                      org-air-view--edit-ring-max)))
        (when (zerop (% (1+ index) 9))
          (org-air-view--history-ring-clear 'org-air-view--edit-ring)
          (should (zerop (hash-table-count
                          org-air-view--history-identity-registry)))))
      (org-air-view--history-ring-clear 'org-air-view--edit-ring)
      (org-air-view--history-ring-clear 'org-air-view--edit-redo-ring)
      (org-air-r90--force-gc)
      (should (zerop (hash-table-count
                      org-air-view--history-identity-registry)))))
  ;; Killing a real mutating-hook source eagerly removes its mapping even
  ;; while the bounded token remains in the global record.
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((source (find-file-noselect (org-air-r90--file "tasks.org")))
           (hook (org-air-r90--mutating-save-hook "registry kill hook")))
      (org-air-r90--goto-row "Alpha")
      (with-current-buffer source (add-hook 'after-save-hook hook nil t))
      (unwind-protect
          (org-air-item-backlog)
        (with-current-buffer source (remove-hook 'after-save-hook hook t)))
      (should (> (hash-table-count
                  org-air-view--history-identity-registry) 0))
      (with-current-buffer source (set-buffer-modified-p nil))
      (kill-buffer source)
      (org-air-r90--force-gc)
      (should (zerop (hash-table-count
                      org-air-view--history-identity-registry)))))
  ;; Forced GC after blocked -> user-resolved retry may either retain the exact
  ;; identity and succeed or lose it and safely remain blocked, never apply a
  ;; different step or manufacture a branch.
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha\n* TODO Beta\n")
        ("inbox.org" . "#+title: inbox\n"))
    (dolist (title '("Alpha" "Beta")) (org-air-r90--mark-title title))
    (let* ((source (find-file-noselect (org-air-r90--file "tasks.org")))
           (label "registry gc retry hook")
           (hook (org-air-r90--mutating-save-hook label)))
      (with-current-buffer source (add-hook 'after-save-hook hook nil t))
      (org-air-item-backlog)
      (with-current-buffer source (remove-hook 'after-save-hook hook t))
      (let ((record (car org-air-view--edit-ring)))
        (org-air-edit-undo)
        (should (eq record (car org-air-view--edit-ring)))
        (org-air-r90--resolve-hook-insertion source label)
        (let ((resolved (org-air-r90--live-text "tasks.org")))
          (org-air-r90--force-gc)
          (org-air-edit-undo)
          (if (eq record (car org-air-view--edit-ring))
              (progn
                (should (equal resolved (org-air-r90--text "tasks.org")))
                (should (equal resolved (org-air-r90--live-text "tasks.org")))
                (should-not org-air-view--edit-redo-ring))
            (should-not (org-air-r90--disk-has-tag-p
                         "tasks.org" "Alpha" "backlog"))
            (should (= 1 (length org-air-view--edit-redo-ring)))))))))

;;;; r90-38/39 — Org-native projection and singular input safety.

(ert-deftest org-air-r90-38-native-projection-inheritance-prefix-and-refresh ()
  "Native tags, Org inheritance, prefixes, literals, and second writes agree."
  (skip-unless (locate-library "org-air"))
  (let ((specs
         '(("tasks.org" . "#+title: tasks\n#+TODO: NEXT WAIT | DONE\n#+FILETAGS: :file_tag:dup_tag:blocked_file:\n\n* Parent :parent_tag:dup_tag:blocked_parent:\n** NEXT [#B] COMMENT Child exact :local_tag:dup_tag:\n* WAIT [#A] COMMENT Prefix exact\n")
           ("literals.org" . "#+title: literals\n\n* TODO Literal suffix :shared-tag:\n* TODO Slash suffix :not/a/tag:\n")
           ("inbox.org" . "#+title: inbox\n"))))
    ;; Org itself owns nil/list/regexp inheritance, exclusions and deduping.
    (org-air-r90--with-corpus specs
      (cl-labels
          ((scan-child ()
             (prog1
                 (org-air-r90--sorted-tags
                  (org-air-r90--item "Child exact" (org-air-query-items)))
               (org-air-query-teardown))))
        (let ((org-use-tag-inheritance nil))
          (should (equal '("dup_tag" "local_tag") (scan-child))))
        (let ((org-use-tag-inheritance
               '("file_tag" "parent_tag" "dup_tag" "blocked_parent"))
              (org-tags-exclude-from-inheritance '("blocked_parent")))
          ;; This exact list is Org's own list-valued inheritance behavior.
          (should (equal '("blocked_parent" "dup_tag" "file_tag"
                           "local_tag" "parent_tag")
                         (scan-child))))
        (let ((org-use-tag-inheritance
               "\\`\\(?:file\\|parent\\)_tag\\'")
              (org-tags-exclude-from-inheritance nil))
          (should (equal '("dup_tag" "file_tag" "local_tag" "parent_tag")
                         (scan-child))))))
    (let ((org-tags-exclude-from-inheritance
           '("blocked_file" "blocked_parent")))
      (org-air-r90--with-board specs
        (let ((child (org-air-r90--item "Child exact"))
              (literal (org-air-r90--item "Literal suffix"))
              (slash (org-air-r90--item "Slash suffix")))
          (should (equal "Child exact" (org-air-item-title child)))
          (should (equal "NEXT" (org-air-item-todo child)))
          (should (= (org-air-item-priority child)
                     (org-get-priority "[#B]")))
          (should (equal '("dup_tag" "file_tag" "local_tag" "parent_tag")
                         (org-air-r90--sorted-tags child)))
          (should (= 1 (seq-count (lambda (tag) (equal tag "dup_tag"))
                                   (org-air-item-tags child))))
          ;; Non-native colon suffixes are literal title text, never tags.
          (should (equal "Literal suffix :shared-tag:"
                         (org-air-item-title literal)))
          (should (equal "Slash suffix :not/a/tag:"
                         (org-air-item-title slash)))
          (should-not (org-air-item-tags literal))
          (should-not (org-air-item-tags slash)))
        (let ((accepted
               (delq nil
                     (list "shared_tag" "@context" "hash#tag"
                           (and (org-air-query--single-tag-value-p "日本語")
                                "日本語")))))
          ;; Marked and unmarked native writes survive real generation swaps.
          (org-air-r90--mark-title "Child exact")
          (cl-letf (((symbol-function 'read-string)
                     (lambda (&rest _) "shared_tag")))
            (org-air-set-tag))
          (dolist (tag (cdr accepted))
            (org-air-r90--goto-row "Child exact")
            (cl-letf (((symbol-function 'read-string)
                       (lambda (&rest _) tag)))
              (org-air-set-tag)))
          (org-air-refresh)
          (let* ((child (org-air-r90--item "Child exact"))
                 (source (find-file-noselect (org-air-r90--file "tasks.org"))))
            (dolist (tag accepted)
              (should (member tag (org-air-item-tags child)))
              (should (org-air-view--filter-token-match-p
                       (concat "#" tag) (org-air-item-title child)
                       (org-air-item-tags child) child))
              (should (= 1 (length
                            (org-ql-select source (list 'tags tag)
                              :action (lambda ()
                                        (org-get-heading t t t t)))))))
            (with-current-buffer source
              (org-with-wide-buffer
               (goto-char (point-min))
               (re-search-forward "Child exact")
               (org-back-to-heading t)
               (should (org-in-commented-heading-p))
               (dolist (tag accepted)
                 (should (member tag (org-get-tags nil t)))
                 (should (= 1 (length
                               (org-map-entries (lambda () (point)) tag nil)))))
               ;; The local writer never copied inherited source truth.
               (should-not (member "file_tag" (org-get-tags nil t)))
               (should-not (member "parent_tag" (org-get-tags nil t)))))))
        ;; Custom TODO/priority/COMMENT bytes and semantic slots survive.
        (org-air-r90--goto-row "Prefix exact")
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) "prefix_tag")))
          (org-air-set-tag))
        (should (string-match-p
                 "^\\* WAIT \\[#[A]\\] COMMENT Prefix exact :prefix_tag:$"
                 (org-air-r90--text "tasks.org")))
        (let ((prefix (org-air-r90--item "Prefix exact")))
          (should (equal "Prefix exact" (org-air-item-title prefix)))
          (should (equal "WAIT" (org-air-item-todo prefix)))
          (should (= (org-air-item-priority prefix)
                     (org-get-priority "[#A]"))))
        ;; Literal title/local-tag truth remains exact after every refresh.
        (org-air-refresh)
        (dolist (pair '(("Literal suffix" . "Literal suffix :shared-tag:")
                        ("Slash suffix" . "Slash suffix :not/a/tag:")))
          (let ((item (org-air-r90--item (car pair))))
            (should (equal (cdr pair) (org-air-item-title item)))
            (should-not (org-air-item-tags item))))
        ;; A second cached mutation targets Child exactly, without a query.
        (org-air-r90--goto-row "Child exact")
        (let ((queries 0))
          (cl-letf (((symbol-function 'org-air-query-items)
                     (lambda (&rest _) (cl-incf queries) (error "query")))
                    ((symbol-function 'org-air-query-items-in-files)
                     (lambda (&rest _) (cl-incf queries) (error "query"))))
            (org-air-item-backlog))
          (should (= queries 0)))
        (should (org-air-r90--disk-has-tag-p
                 "tasks.org" "Child exact" "backlog"))
        (let ((line (seq-find
                     (lambda (text) (string-match-p "Child exact" text))
                     (split-string (org-air-r90--text "tasks.org") "\n"))))
          (should line)
          (should-not (string-match-p
                       "file_tag\\|parent_tag\\|blocked_" line)))))))

(ert-deftest org-air-r90-39-invalid-shared-tags-are-immediate-and-inert ()
  "Every malformed shared value refuses after one prompt with zero side effect."
  (skip-unless (locate-library "org-air"))
  (dolist (dispatch '(marked unmarked))
    (dolist (tag org-air-r90--invalid-tags)
      (org-air-r90--with-board
          '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha task :one:\n* TODO Beta task\n")
            ("inbox.org" . "#+title: inbox\n"))
        (org-air-r90--goto-row "Alpha task")
        (when (eq dispatch 'marked)
          (org-air-toggle-mark))
        (let* ((source (find-file-noselect (org-air-r90--file "tasks.org")))
               (edit-sentinel (list :sentinel 'edit))
               (redo-sentinel (list :sentinel 'redo))
               (prompts 0)
               (calls 0))
          (org-air-view--classify-cached
           (org-air-r90--item "Alpha task") org-air-test-now)
          (setq org-air-view--edit-ring (list edit-sentinel)
                org-air-view--edit-redo-ring (list redo-sentinel))
          (let ((before (org-air-r90--command-state "tasks.org" source)))
            (ert-info ((format "invalid %S through %S dispatch" tag dispatch))
              (should-error
               (cl-letf (((symbol-function 'read-string)
                          (lambda (&rest _) (cl-incf prompts) tag))
                         ((symbol-function 'org-air-view--bulk-preflight)
                          (lambda (&rest _) (cl-incf calls)
                            (error "preflight reached")))
                         ((symbol-function 'org-air-view--run-source-transaction)
                          (lambda (&rest _) (cl-incf calls)
                            (error "source transaction reached")))
                         ((symbol-function 'find-file-noselect)
                          (lambda (&rest _) (cl-incf calls)
                            (error "source open reached")))
                         ((symbol-function 'save-buffer)
                          (lambda (&rest _) (cl-incf calls)
                            (error "save reached")))
                         ((symbol-function 'org-air-view--render)
                          (lambda (&rest _) (cl-incf calls)
                            (error "render reached")))
                         ((symbol-function 'org-air-view--panes-resync-now)
                          (lambda (&rest _) (cl-incf calls)
                            (error "pane reached")))
                         ((symbol-function 'run-with-timer)
                          (lambda (&rest _) (cl-incf calls)
                            (error "timer reached")))
                         ((symbol-function 'run-with-idle-timer)
                          (lambda (&rest _) (cl-incf calls)
                            (error "idle timer reached"))))
                 (org-air-set-tag))
               :type 'user-error)
              (should (= prompts 1))
              (should (= calls 0))
              (org-air-r90--assert-command-state
               before "tasks.org" source))))))))

(ert-deftest org-air-r90-40-low-level-tag-writers-refuse-with-zero-change ()
  "Direct toggle and pure line setter reject every malformed singular value."
  (skip-unless (locate-library "org-air"))
  (dolist (tag org-air-r90--invalid-tags)
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Alpha task :one:\n")
      (goto-char (point-min))
      (let ((before (buffer-string)) (before-point (point)))
        (ert-info ((format "low-level invalid tag %S" tag))
          (should-error (org-air-view--source-toggle-local-tag tag 'on)
                        :type 'user-error)
          (should (equal before (buffer-string)))
          (should (= before-point (point)))
          (should-error
           (org-air-query--heading-line-set-local-tags
            "* TODO Alpha task :one:" (list "one" tag))
           :type 'user-error)
          (should (equal before (buffer-string)))
          (should (= before-point (point))))))))

;;;; r90-41..43 — total committed finalization under isolated failures.

(ert-deftest org-air-r90-41-finalizer-cosmetic-and-relocation-errors-stay-total ()
  "Relocation helper plus cosmetic parser errors cannot skip slots or invalidation."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha\n* TODO Beta\n* TODO Later\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((items (mapcar #'org-air-r90--item '("Alpha" "Beta" "Later")))
           (old-items org-air-view--items)
           (source (find-file-noselect (org-air-r90--file "tasks.org")))
           (cache org-air-view--classify-cache)
           (after-write nil)
           (parser-errors 0)
           (queries 0)
           invalidated
           (orig-remhash (symbol-function 'remhash))
           (orig-element (symbol-function 'org-element-at-point)))
      (dolist (item items)
        (org-air-view--classify-cached item org-air-test-now))
      (dolist (title '("Alpha" "Beta")) (org-air-r90--mark-title title))
      (with-current-buffer source
        (add-hook 'after-save-hook (lambda () (setq after-write t)) -99 t))
      (org-air-r90--with-timer-audit timer-observations
        (cl-letf (((symbol-function 'org-air-view--relocation-commit)
                   (lambda (&rest _) (error "relocation helper failed")))
                  ((symbol-function 'org-element-at-point)
                   (lambda (&rest args)
                     (if (and after-write (eq (current-buffer) source))
                         (progn (cl-incf parser-errors)
                                (error "cosmetic parser failed"))
                       (apply orig-element args))))
                  ((symbol-function 'remhash)
                   (lambda (key table)
                     (when (eq table cache) (push key invalidated))
                     (funcall orig-remhash key table)))
                  ((symbol-function 'org-air-query-items)
                   (lambda (&rest _) (cl-incf queries) (error "query")))
                  ((symbol-function 'org-air-query-items-in-files)
                   (lambda (&rest _) (cl-incf queries) (error "query")))
                  ((symbol-function 'org-air-refresh)
                   (lambda (&rest _) (cl-incf queries) (error "refresh"))))
          (org-air-item-backlog)))
      (should (> parser-errors 0))
      (should (= queries 0))
      (should (eq old-items org-air-view--items))
      (org-air-r90--assert-file-cache-exact "tasks.org" items)
      (dolist (item items)
        (should (memq item invalidated)))
      (should (equal '(backlog)
                     (org-air-view--classify-cached
                      (car items) org-air-test-now)))
      (should (= 1 (length org-air-view--edit-ring)))
      (should-not org-air-view--marked-keys))))

(ert-deftest org-air-r90-42-remhash-failure-poisons-whole-classify-cache ()
  "A hostile mandatory remhash drops the whole memo and recomputes truthfully."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha\n* TODO Beta\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((alpha (org-air-r90--item "Alpha"))
           (beta (org-air-r90--item "Beta"))
           (old-items org-air-view--items)
           (old-cache org-air-view--classify-cache)
           (orig-remhash (symbol-function 'remhash))
           warnings)
      (dolist (item (list alpha beta))
        (org-air-view--classify-cached item org-air-test-now))
      (org-air-r90--mark-title "Alpha")
      (cl-letf (((symbol-function 'remhash)
                 (lambda (key table)
                   (if (eq table old-cache)
                       (error "hostile remhash for %S" key)
                     (funcall orig-remhash key table))))
                ((symbol-function 'display-warning)
                 (lambda (&rest args) (push args warnings))))
        (org-air-item-backlog))
      (should warnings)
      (should (eq old-items org-air-view--items))
      (should-not (eq old-cache org-air-view--classify-cache))
      (should (equal '(backlog)
                     (org-air-view--classify-cached alpha org-air-test-now)))
      (should (equal '(attention)
                     (org-air-view--classify-cached beta org-air-test-now)))
      (should (org-air-r90--disk-has-tag-p
               "tasks.org" "Alpha" "backlog")))))

(ert-deftest org-air-r90-43-mandatory-slot-failure-rebuilds-safe-generation ()
  "One mandatory slot failure drops generation/locators, warns, then rebuilds."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha\n* TODO Beta\n* TODO Gamma\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((source (find-file-noselect (org-air-r90--file "tasks.org")))
           (old-items org-air-view--items)
           (old-markers
            (with-current-buffer source
              (mapcar #'cdr org-air-view--source-tracked-locators)))
           (orig-write (symbol-function 'org-air-view--cache-sync-write-slots))
           (queries 0)
           calls warnings)
      (org-air-r90--mark-title "Alpha")
      (let ((query-orig (symbol-function 'org-air-query-items)))
        (cl-letf (((symbol-function 'org-air-view--cache-sync-write-slots)
                   (lambda (item file position tags)
                     (push (org-air-item-title item) calls)
                     (if (equal "Beta" (org-air-item-title item))
                         (error "mandatory Beta slot failed")
                       (funcall orig-write item file position tags))))
                  ((symbol-function 'org-air-query-items)
                   (lambda (&rest args)
                     (cl-incf queries) (apply query-orig args)))
                  ((symbol-function 'display-warning)
                   (lambda (&rest args) (push args warnings))))
          (org-air-item-backlog)))
      (should (= queries 1))
      (should (= (length warnings) 1))
      (should (equal '("Alpha" "Beta") (nreverse calls)))
      (should-not (eq old-items org-air-view--items))
      (should-not org-air-view--marked-keys)
      (dolist (marker old-markers) (should-not (marker-buffer marker)))
      (let ((entries (with-current-buffer source
                       org-air-view--source-tracked-locators)))
        (should (= 3 (length entries)))
        (dolist (entry entries)
          (should (memq (car entry) org-air-view--items))))
      (should (org-air-r90--disk-has-tag-p
               "tasks.org" "Alpha" "backlog"))
      (org-air-r90--assert-file-cache-exact "tasks.org" org-air-view--items)
      ;; The rebuilt generation is safe for an exact second cached write; it
      ;; must hit Gamma and never the adjacent Beta heading.
      (org-air-r90--goto-row "Gamma")
      (let ((second-queries 0))
        (cl-letf (((symbol-function 'org-air-query-items)
                   (lambda (&rest _) (cl-incf second-queries)
                     (error "second query")))
                  ((symbol-function 'org-air-query-items-in-files)
                   (lambda (&rest _) (cl-incf second-queries)
                     (error "second query"))))
          (org-air-item-backlog))
        (should (= second-queries 0)))
      (should (org-air-r90--disk-has-tag-p
               "tasks.org" "Gamma" "backlog"))
      (should-not (org-air-r90--disk-has-tag-p
                   "tasks.org" "Beta" "backlog")))))

;;;; r90-44/45 — canonical source-owner lifecycle and generation pruning.

(ert-deftest org-air-r90-44-source-owner-open-kill-reopen-lifecycle ()
  "Open order, incidental modes, source/owner kills, and cross-project isolation hold."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-corpus
      '(("a.org" . "#+title: a\n\n* TODO A one\n* TODO A two\n")
        ("b.org" . "#+title: b\n\n* TODO B one\n")
        ("inbox.org" . "#+title: inbox\n"))
    (org-air-viewport-test--with-frozen-now
      (org-air-viewport-test--with-render-guards
        (let* ((source-a (find-file-noselect (org-air-r90--file "a.org")))
               (outside-dir (make-temp-file "org-air-r90-outside-" t))
               (outside-file (expand-file-name "outside.org" outside-dir))
               outside fake-owner foreign-marker)
          (unwind-protect
              (progn
                ;; Source A is live before the board; B opens after it.
                (write-region "#+title: outside\n* TODO Outside\n"
                              nil outside-file nil 'silent)
                (org-air)
                (let ((board (get-buffer org-air-view-buffer-name)))
                  (should (eq board org-air-view--source-tracking-owner))
                  (should (= 1 (seq-count
                                (lambda (entry)
                                  (eq entry
                                      #'org-air-view--hydrate-open-source-markers))
                                find-file-hook)))
                  (should (= 2 (with-current-buffer source-a
                                 (length
                                  org-air-view--source-tracked-locators))))
                  (let ((source-b (find-file-noselect
                                   (org-air-r90--file "b.org"))))
                    (should (= 1 (with-current-buffer source-b
                                   (length
                                    org-air-view--source-tracked-locators))))
                    ;; An incidental mode buffer cannot steal or remove owner
                    ;; hooks/markers.
                    (let ((incidental (generate-new-buffer
                                       " *r90 incidental view*")))
                      (with-current-buffer incidental (org-air-view-mode))
                      (kill-buffer incidental))
                    (should (eq board org-air-view--source-tracking-owner))
                    (should (= 1 (seq-count
                                  (lambda (entry)
                                    (eq entry
                                        #'org-air-view--hydrate-open-source-markers))
                                  find-file-hook)))
                    (should (= 2 (with-current-buffer source-a
                                   (length
                                    org-air-view--source-tracked-locators))))
                    ;; A source kill removes only that tracked-buffer entry;
                    ;; reopening hydrates the current generation exactly once.
                    (kill-buffer source-a)
                    (with-current-buffer board
                      (should-not (memq source-a
                                       org-air-view--source-tracked-buffers)))
                    (setq source-a
                          (find-file-noselect (org-air-r90--file "a.org")))
                    (should (= 2 (with-current-buffer source-a
                                   (length
                                    org-air-view--source-tracked-locators))))
                    ;; A cross-project source receives no locator.  Its foreign
                    ;; marker/owner also proves board teardown clears only its
                    ;; own tracked buffers.
                    (setq outside (find-file-noselect outside-file)
                          fake-owner (generate-new-buffer " *r90 foreign owner*"))
                    (with-current-buffer outside
                      (setq-local org-air-view--source-locator-owner fake-owner)
                      (setq foreign-marker (copy-marker (point-min)))
                      (setq-local org-air-view--source-tracked-locators
                                  (list (cons 'foreign foreign-marker))))
                    (should (= 1 (with-current-buffer outside
                                   (length
                                    org-air-view--source-tracked-locators))))
                    (kill-buffer board)
                    (should-not org-air-view--source-tracking-owner)
                    (should-not (memq
                                 #'org-air-view--hydrate-open-source-markers
                                 find-file-hook))
                    (should-not (with-current-buffer source-a
                                  org-air-view--source-tracked-locators))
                    (should-not (with-current-buffer source-b
                                  org-air-view--source-tracked-locators))
                    (should (marker-buffer foreign-marker))
                    ;; Reopening the owner hydrates already-live A/B sources.
                    (org-air)
                    (setq board (get-buffer org-air-view-buffer-name))
                    (should (eq board org-air-view--source-tracking-owner))
                    (should (= 2 (with-current-buffer source-a
                                   (length
                                    org-air-view--source-tracked-locators))))
                    (should (= 1 (with-current-buffer source-b
                                   (length
                                    org-air-view--source-tracked-locators))))
                    (should-not (with-current-buffer outside
                                  (seq-find
                                   (lambda (entry)
                                     (org-air-item-p (car entry)))
                                   org-air-view--source-tracked-locators))))))
            (when (buffer-live-p (get-buffer org-air-view-buffer-name))
              (kill-buffer (get-buffer org-air-view-buffer-name)))
            (when (markerp foreign-marker) (set-marker foreign-marker nil))
            (when (buffer-live-p outside)
              (with-current-buffer outside (set-buffer-modified-p nil))
              (kill-buffer outside))
            (when (buffer-live-p fake-owner) (kill-buffer fake-owner))
            (delete-directory outside-dir t)))))))

(ert-deftest org-air-r90-45-generation-prune-hydrate-is-no-IO-and-exact ()
  "Twelve swaps kill stale markers, retain numeric keys, and create no IO/timer."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha\n* TODO Beta\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((source (find-file-noselect (org-air-r90--file "tasks.org")))
           (prior-items org-air-view--items)
           (prior-markers
            (with-current-buffer source
              (mapcar #'cdr org-air-view--source-tracked-locators)))
           (opens 0) (queries 0) (refreshes 0))
      (org-air-r90--with-timer-audit timer-observations
        (cl-letf (((symbol-function 'find-file-noselect)
                   (lambda (&rest _) (cl-incf opens) (error "open")))
                  ((symbol-function 'org-air-query-items)
                   (lambda (&rest _) (cl-incf queries) (error "query")))
                  ((symbol-function 'org-air-query-items-in-files)
                   (lambda (&rest _) (cl-incf queries) (error "query")))
                  ((symbol-function 'org-air-refresh)
                   (lambda (&rest _) (cl-incf refreshes) (error "refresh"))))
          (let ((org-air-show-inspector nil)
                (org-air-view--inspector-active nil))
            (dotimes (_ 12)
              (let ((fresh (mapcar #'copy-sequence prior-items)))
                (org-air-view--render fresh nil)
              (dolist (marker prior-markers)
                (should-not (marker-buffer marker)))
              (let ((entries (with-current-buffer source
                               org-air-view--source-tracked-locators)))
                (should (= (length fresh) (length entries)))
                (should (= (length fresh)
                           (length (delete-dups (mapcar #'car entries)))))
                (dolist (entry entries)
                  (should (memq (car entry) fresh)))
                (dolist (old prior-items)
                  (should-not (assq old entries)))
                (setq prior-markers (mapcar #'cdr entries)))
              (dolist (item fresh)
                (let ((marker (org-air-item-marker item))
                      (key (org-air-view--item-source-key item)))
                  (should (consp marker))
                  (should (integerp (cdr marker)))
                  (should (integerp (cdr key)))))
                (setq prior-items fresh))))))
      (should (= opens 0))
      (should (= queries 0))
      (should (= refreshes 0))
      (should (equal (list source)
                     org-air-view--source-tracked-buffers)))))

;;;; r90-46..49 — recursive save boundaries and conservative history.

(defun org-air-r90--nested-commit-hook (shape signalp cell)
  "Return a one-shot after-save hook mutating SHAPE, saving, then maybe signaling.
CELL is a mutable one-element list used as the recursion guard."
  (lambda ()
    (unless (car cell)
      (setcar cell t)
      (pcase shape
          ('comment
           (goto-char (point-min))
           (insert "# nested committed hook\n"))
          ('title
           (goto-char (point-min))
           (re-search-forward "^\\* TODO Alpha")
           (search-backward "Alpha")
           (replace-match "Alpha hooked"))
          ('tag
           (goto-char (point-min))
           (re-search-forward "^\\* TODO Alpha")
           (org-back-to-heading t)
           (org-toggle-tag "hook_tag" 'on))
          ('move
           (goto-char (point-min))
           (re-search-forward "^\\* TODO Alpha")
           (beginning-of-line)
           (let* ((beg (point))
                  (end (progn (forward-line 1) (point)))
                  (line (buffer-substring beg end)))
             (delete-region beg end)
             (goto-char (point-max))
             (unless (bolp) (insert "\n"))
             (insert line)))
          ('delete
           (goto-char (point-min))
           (re-search-forward "^\\* TODO Alpha")
           (beginning-of-line)
           (delete-region (point) (progn (forward-line 1) (point)))))
      (save-buffer)
      (when signalp
        (error "nested committed hook later signal")))))

(defun org-air-r90--owner-hook-count ()
  "Return the number of global org-air source hydration hooks."
  (seq-count (lambda (entry)
               (eq entry #'org-air-view--hydrate-open-source-markers))
             find-file-hook))

(defun org-air-r90--scale-content (count)
  "Return a native-tag Org source containing COUNT headings."
  (with-temp-buffer
    (insert "#+title: scale\n#+FILETAGS: :file_native:scale_tag:\n")
    (dotimes (index count)
      (insert (format "* TODO Task %05d\n" index)))
    (buffer-string)))

(defun org-air-r90--scale-items (content file)
  "Build durable cached items from CONTENT for FILE without visiting it."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (let (items)
      (while (re-search-forward "^\\* TODO \\(Task [0-9]+\\)$" nil t)
        (push (org-air-item-create
               :title (match-string-no-properties 1)
               :tags '("file_native" "scale_tag")
               :file file :marker (cons file (line-beginning-position))
               :todo "TODO" :priority 0 :kind 'heading :donep nil
               :ntype 'task)
              items))
      (nreverse items))))

(ert-deftest org-air-r90-46-nested-save-shapes-rebuild-final-disk-truth ()
  "Recursive comment/title/tag/move/delete commits block, resolve, and rebuild."
  (skip-unless (locate-library "org-air"))
  (dolist (shape '(comment title tag move delete))
    (dolist (signalp '(nil t))
      (org-air-r90--with-board
          '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha :one:\n* TODO Beta\n")
            ("inbox.org" . "#+title: inbox\n"))
        (let* ((before (org-air-r90--text "tasks.org"))
               (source (find-file-noselect (org-air-r90--file "tasks.org")))
               (old-items org-air-view--items)
               (old-markers
                (with-current-buffer source
                  (mapcar #'cdr org-air-view--source-tracked-locators)))
               (cell (list nil))
               (hook (org-air-r90--nested-commit-hook shape signalp cell))
               (query-orig (symbol-function 'org-air-query-items))
               (slot-orig (symbol-function
                           'org-air-view--cache-sync-write-slots))
               (queries 0) (slot-writes 0) warnings)
          (org-air-r90--goto-row "Alpha")
          (with-current-buffer source
            (add-hook 'after-save-hook hook nil t))
          (unwind-protect
              (cl-letf (((symbol-function 'org-air-query-items)
                         (lambda (&rest args)
                           (cl-incf queries) (apply query-orig args)))
                        ((symbol-function 'org-air-view--cache-sync-write-slots)
                         (lambda (&rest args)
                           (cl-incf slot-writes) (apply slot-orig args)))
                        ((symbol-function 'display-warning)
                         (lambda (&rest args) (push args warnings))))
                (org-air-item-backlog))
            (with-current-buffer source
              (remove-hook 'after-save-hook hook t)))
          (ert-info ((format "nested shape=%S signal=%S" shape signalp))
            (should (car cell))
            (should (= queries 1))
            ;; Recursive finalization invalidates directly: no old slot write.
            (should (= slot-writes 0))
            (should (= (length warnings) 1))
            (should (string-match-p
                     "intervening committed save hook"
                     (format "%S" (car warnings))))
            (should (equal (org-air-r90--text "tasks.org")
                           (org-air-r90--live-text "tasks.org")))
            (should-not (buffer-modified-p source))
            (should-not (eq old-items org-air-view--items))
            (dolist (marker old-markers)
              (should-not (marker-buffer marker)))
            (should-not org-air-view--marked-keys)
            (should-not org-air-view--pending-mutation-landing)
            (let ((record (car org-air-view--edit-ring)))
              (should (eq 'in-place (plist-get record :kind)))
              (should (plist-member record :expected-undo))
              (org-air-r90--assert-history-token
               (plist-get record :expected-undo))
              (should (eq 'intervening-commit
                          (gethash record org-air-view--cache-sync-history)))
              (pcase shape
                ('title
                 (should (org-air-test-find-item
                          "Alpha hooked" org-air-view--items)))
                ('move
                 (should (> (cdr (org-air-view--item-source-key
                                  (org-air-r90--item "Alpha")))
                            (cdr (org-air-view--item-source-key
                                  (org-air-r90--item "Beta"))))))
                ('delete
                 (should-not (org-air-test-find-item
                              "Alpha" org-air-view--items)))
                (_
                 (let ((alpha (org-air-r90--item "Alpha")))
                   (should (member "backlog" (org-air-item-tags alpha)))
                   (when (eq shape 'tag)
                     (should (member "hook_tag"
                                     (org-air-item-tags alpha)))))))
              ;; Immediate u is zero-save/zero-byte and stays on the same ring.
              (let ((disk (org-air-r90--text "tasks.org"))
                    (live (org-air-r90--live-text "tasks.org"))
                    (saves 0))
                (cl-letf (((symbol-function 'save-buffer)
                           (lambda (&rest _) (cl-incf saves))))
                  (org-air-edit-undo))
                (should (= saves 0))
                (should (equal disk (org-air-r90--text "tasks.org")))
                (should (equal live (org-air-r90--live-text "tasks.org")))
                (should (eq record (car org-air-view--edit-ring)))
                (should-not org-air-view--edit-redo-ring))
              ;; Resolve only the committed hook group, save it, then undo the
              ;; org-air group at its exact identity and rebuild once.
              (with-current-buffer source
                (undo-boundary)
                (undo-only)
                (save-buffer))
              (setq queries 0)
              (cl-letf (((symbol-function 'org-air-query-items)
                         (lambda (&rest args)
                           (cl-incf queries) (apply query-orig args))))
                (org-air-edit-undo))
              (should (= queries 1))
              (should (equal before (org-air-r90--text "tasks.org")))
              (should (equal before (org-air-r90--live-text "tasks.org")))
              (should-not (buffer-modified-p source))
              (should-not org-air-view--edit-ring)
              (should (= 1 (length org-air-view--edit-redo-ring)))
              (should-not org-air-view--marked-keys))))))))

(ert-deftest org-air-r90-47-save-attempt-is-one-shot-under-recursive-fallbacks ()
  "Prepare/state/undo/tick capture once under save-buffer and basic fallback."
  (skip-unless (locate-library "org-air"))
  (dolist (mechanism '(save-buffer basic-save-buffer))
    (org-air-r90--with-corpus
        '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha\n")
          ("inbox.org" . "#+title: inbox\n"))
      (let* ((source (find-file-noselect (org-air-r90--file "tasks.org")))
             (cell (list nil))
             (state (list 'outer-state mechanism))
             (prepares 0) (undo-captures 0) (redo-captures 0)
             (undo-orig (symbol-function 'org-air-view--expected-undo-step))
             (redo-orig (symbol-function 'org-air-view--expected-redo-step))
             hook result)
        (setq hook
              (lambda ()
                (unless (car cell)
                  (setcar cell t)
                  (goto-char (point-min))
                  (insert (format "# nested %s\n" mechanism))
                  (funcall mechanism))))
        (with-current-buffer source
          (goto-char (point-max))
          (insert "# outer dirty\n")
          (add-hook 'after-save-hook hook nil t)
          (let ((before-hooks (copy-tree before-save-hook))
                (after-hooks (copy-tree after-save-hook)))
            (cl-letf (((symbol-function 'org-air-view--expected-undo-step)
                       (lambda (&rest args)
                         (cl-incf undo-captures) (apply undo-orig args)))
                      ((symbol-function 'org-air-view--expected-redo-step)
                       (lambda (&rest args)
                         (cl-incf redo-captures) (apply redo-orig args))))
              (setq result
                    (org-air-view--save-attempt
                     (lambda () (cl-incf prepares) state))))
            (should (equal before-hooks before-save-hook))
            (should (equal after-hooks after-save-hook)))
          (remove-hook 'after-save-hook hook t))
        (should (plist-get result :committed))
        (should (plist-get result :recursive-commit))
        (should (eq state (plist-get result :state)))
        (should (= prepares 1))
        (should (= undo-captures 1))
        (should (= redo-captures 1))
        (should (integerp (plist-get result :expected-tick)))
        (should (equal (org-air-r90--text "tasks.org")
                       (org-air-r90--live-text "tasks.org")))
        (should-not (buffer-modified-p source))))))

(ert-deftest org-air-r90-48-nonsignal-and-missing-identities-block-conservatively ()
  "Unsaved non-signalling edits and unavailable undo identities move nothing."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((source (find-file-noselect (org-air-r90--file "tasks.org")))
           (hook (lambda ()
                   (goto-char (point-min))
                   (insert "# non-signalling unsaved hook\n"))))
      (org-air-r90--goto-row "Alpha")
      (with-current-buffer source (add-hook 'after-save-hook hook nil t))
      (org-air-item-backlog)
      (with-current-buffer source (remove-hook 'after-save-hook hook t))
      (let* ((record (car org-air-view--edit-ring))
             (token (plist-get record :expected-undo))
             (disk (org-air-r90--text "tasks.org"))
             (live (org-air-r90--live-text "tasks.org")))
        (should (buffer-modified-p source))
        (should (string-match-p "non-signalling unsaved hook" live))
        (org-air-r90--assert-history-token token)
        (dotimes (attempt 2)
          (when (= attempt 1)
            ;; Missing weak side state is independently conservative.
            (remhash token org-air-view--history-identity-registry))
          (let ((saves 0))
            (cl-letf (((symbol-function 'save-buffer)
                       (lambda (&rest _) (cl-incf saves))))
              (org-air-edit-undo))
            (should (= saves 0)))
          (should (equal disk (org-air-r90--text "tasks.org")))
          (should (equal live (org-air-r90--live-text "tasks.org")))
          (should (eq record (car org-air-view--edit-ring)))
          (should-not org-air-view--edit-redo-ring)))))
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let ((source (find-file-noselect (org-air-r90--file "tasks.org"))))
      (org-air-r90--goto-row "Alpha")
      (with-current-buffer source (setq buffer-undo-list t))
      (org-air-item-backlog)
      (let* ((record (car org-air-view--edit-ring))
             (disk (org-air-r90--text "tasks.org"))
             (saves 0))
        (org-air-r90--assert-history-token
         (plist-get record :expected-undo))
        (cl-letf (((symbol-function 'save-buffer)
                   (lambda (&rest _) (cl-incf saves))))
          (org-air-edit-undo))
        (should (= saves 0))
        (should (eq record (car org-air-view--edit-ring)))
        (should-not org-air-view--edit-redo-ring)
        (should (equal disk (org-air-r90--text "tasks.org")))
        (should (equal disk (org-air-r90--live-text "tasks.org")))))))

(ert-deftest org-air-r90-49-marked-and-compound-nested-directions-are-exact ()
  "Marked b/t and compound u/U recursive commits block and resolve exactly."
  (skip-unless (locate-library "org-air"))
  ;; Initial marked b and t each retain one honest compound part.
  (dolist (action '(backlog tag))
    (org-air-r90--with-board
        '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha\n* TODO Beta\n")
          ("inbox.org" . "#+title: inbox\n"))
      (let ((before (org-air-r90--text "tasks.org")))
        (dolist (title '("Alpha" "Beta")) (org-air-r90--mark-title title))
        (let* ((source (find-file-noselect (org-air-r90--file "tasks.org")))
               (cell (list nil))
               (hook (org-air-r90--nested-commit-hook 'comment nil cell)))
          (with-current-buffer source (add-hook 'after-save-hook hook nil t))
          (if (eq action 'backlog)
              (org-air-item-backlog)
            (cl-letf (((symbol-function 'read-string)
                       (lambda (&rest _) "shared_tag")))
              (org-air-set-tag)))
          (with-current-buffer source (remove-hook 'after-save-hook hook t))
          (let* ((record (car org-air-view--edit-ring))
                 (part (car (plist-get record :parts)))
                 (disk (org-air-r90--text "tasks.org"))
                 (saves 0))
            (should (eq 'intervening-commit
                        (gethash part org-air-view--cache-sync-history)))
            (cl-letf (((symbol-function 'save-buffer)
                       (lambda (&rest _) (cl-incf saves))))
              (org-air-edit-undo))
            (should (= saves 0))
            (should (eq record (car org-air-view--edit-ring)))
            (should (equal disk (org-air-r90--live-text "tasks.org")))
            (with-current-buffer source
              (undo-boundary) (undo-only) (save-buffer))
            (org-air-edit-undo)
            (should (equal before (org-air-r90--text "tasks.org")))
            (should-not org-air-view--edit-ring)
            (should (= 1 (length org-air-view--edit-redo-ring)))
            (should-not (string-match-p
                         "residual"
                         (plist-get (car org-air-view--edit-redo-ring)
                                    :desc))))))))
  ;; Recursive commits during compound u and U arm the inverse direction.
  ;; Redo runs first so both directions execute even if undo→redo resolution
  ;; exposes a regression.
  (dolist (direction '(redo undo))
    (let ((pending-undo-list nil)
          (undo-equiv-table (make-hash-table :test #'eq))
          (last-command nil)
          (this-command nil))
      (org-air-r90--with-board
          '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha\n* TODO Beta\n")
            ("inbox.org" . "#+title: inbox\n"))
        (let ((before (org-air-r90--text "tasks.org")))
        (dolist (title '("Alpha" "Beta")) (org-air-r90--mark-title title))
        (org-air-item-backlog)
        (let ((committed (org-air-r90--text "tasks.org")))
          (when (eq direction 'redo) (org-air-edit-undo))
          (let* ((source (find-file-noselect (org-air-r90--file "tasks.org")))
                 (cell (list nil))
                 (hook (org-air-r90--nested-commit-hook 'comment nil cell)))
            (with-current-buffer source (add-hook 'after-save-hook hook nil t))
            (if (eq direction 'undo)
                (org-air-edit-undo)
              (org-air-edit-redo))
            (with-current-buffer source (remove-hook 'after-save-hook hook t))
            (let* ((source-ring (if (eq direction 'undo)
                                    org-air-view--edit-redo-ring
                                  org-air-view--edit-ring))
                   (record (car source-ring))
                   (disk (org-air-r90--text "tasks.org"))
                   (undo-order (copy-sequence org-air-view--edit-ring))
                   (redo-order (copy-sequence org-air-view--edit-redo-ring))
                   (saves 0))
              (cl-letf (((symbol-function 'save-buffer)
                         (lambda (&rest _) (cl-incf saves))))
                (if (eq direction 'undo)
                    (org-air-edit-redo)
                  (org-air-edit-undo)))
              (should (= saves 0))
              (should (equal disk (org-air-r90--live-text "tasks.org")))
              (should (org-air-r90--same-object-order-p
                       undo-order org-air-view--edit-ring))
              (should (org-air-r90--same-object-order-p
                       redo-order org-air-view--edit-redo-ring))
              (should (eq record (car source-ring)))
              (with-current-buffer source
                (undo-boundary) (undo-only) (save-buffer))
              ;; Resolving the committed hook step must itself leave durable
              ;; disk/live truth before org-air attempts the inverse.
              (should (equal (org-air-r90--text "tasks.org")
                             (org-air-r90--live-text "tasks.org")))
              (should-not (buffer-modified-p source))
              (if (eq direction 'undo)
                  (org-air-edit-redo)
                (org-air-edit-undo))
              (should (equal (if (eq direction 'undo) committed before)
                             (org-air-r90--text "tasks.org")))
              (should (= 1 (+ (length org-air-view--edit-ring)
                              (length org-air-view--edit-redo-ring))))
              (let ((final-record (or (car org-air-view--edit-ring)
                                      (car org-air-view--edit-redo-ring))))
                (should (eq 'bulk (plist-get final-record :kind)))
                (should (= 1 (length (plist-get final-record :parts))))
                (should-not (string-match-p
                             "residual" (plist-get final-record :desc))))))))))))

;;;; r90-50 — linear source indexes and unchanged-generation fast paths.

(ert-deftest org-air-r90-50-source-tracking-is-linear-at-1k-and-5k ()
  "Native 1k/5k hydration is one-pass; current repaints do zero source work."
  (skip-unless (locate-library "org-air"))
  (dolist (count '(1000 5000))
    (let ((content (org-air-r90--scale-content count)))
      (org-air-r90--with-corpus
          `(("tasks.org" . ,content) ("inbox.org" . "#+title: inbox\n"))
        (let* ((file (org-air-r90--file "tasks.org"))
               (items (org-air-r90--scale-items content file))
               (board (get-buffer-create org-air-view-buffer-name))
               source old-markers)
          (unwind-protect
              (progn
                (with-current-buffer board
                  (org-air-view-mode)
                  (setq org-air-view--items items
                        org-air-view--items-key (org-air-view--cache-key)))
                ;; Opening the source hydrates exactly once in one native scan.
                (let ((map-orig (symbol-function 'org-map-entries))
                      (exact-orig
                       (symbol-function 'org-air-view--source-heading-exact-p))
                      (open-orig (symbol-function 'find-file-noselect))
                      (maps 0) (exacts 0) (opens 0) (queries 0) observations)
                  (cl-letf (((symbol-function 'org-map-entries)
                             (lambda (&rest args)
                               (cl-incf maps) (apply map-orig args)))
                            ((symbol-function
                              'org-air-view--source-heading-exact-p)
                             (lambda (&rest args)
                               (cl-incf exacts) (apply exact-orig args)))
                            ((symbol-function 'find-file-noselect)
                             (lambda (&rest args)
                               (cl-incf opens) (apply open-orig args)))
                            ((symbol-function 'org-air-query-items)
                             (lambda (&rest _)
                               (cl-incf queries) (error "query")))
                            ((symbol-function 'run-with-timer)
                             (lambda (seconds repeat callback &rest arguments)
                               (push (list :constructor 'run-with-timer
                                           :seconds seconds :repeat repeat
                                           :callback callback
                                           :arguments arguments)
                                     observations)
                               nil))
                            ((symbol-function 'run-with-idle-timer)
                             (lambda (seconds repeat callback &rest arguments)
                               (push (list :constructor 'run-with-idle-timer
                                           :seconds seconds :repeat repeat
                                           :callback callback
                                           :arguments arguments)
                                     observations)
                               nil)))
                    (setq source (find-file-noselect file)))
                  (should (= opens 1))
                  (should (= queries 0))
                  (should (= maps 1))
                  (should (= exacts count))
                  (should-not (seq-filter #'org-air-r90--org-air-timer-p
                                          observations)))
                (with-current-buffer board
                  (should (eq org-air-view--source-generation items))
                  (should (eq 'eq
                              (hash-table-test
                               org-air-view--source-item-membership)))
                  (should (= count
                             (hash-table-count
                              org-air-view--source-item-membership)))
                  (should (eq 'equal
                              (hash-table-test
                               org-air-view--source-items-by-file)))
                  (should (equal items
                                 (gethash file
                                          org-air-view--source-items-by-file))))
                (with-current-buffer source
                  (should (eq org-air-view--source-locator-generation items))
                  (should org-air-view--source-locator-complete)
                  (should (eq 'eq
                              (hash-table-test
                               org-air-view--source-locator-index)))
                  (should (= count
                             (hash-table-count
                              org-air-view--source-locator-index)))
                  (should (= count
                             (length org-air-view--source-tracked-locators)))
                  (should (= 0 (org-air-r90--undo-disk-guard-count))))
                ;; Current direct hydration plus render/filter/mark/collapse do
                ;; zero exact/scanner work, zero prune/hydrate, and no list
                ;; membership fallback or index replacement.
                (let* ((map-orig (symbol-function 'org-map-entries))
                       (exact-orig
                        (symbol-function 'org-air-view--source-heading-exact-p))
                       (prune-orig
                        (symbol-function 'org-air-view--source-prune-buffer))
                       (hydrate-orig
                        (symbol-function 'org-air-view--hydrate-source-items))
                       (find-orig (symbol-function 'find-buffer-visiting))
                       (memq-orig (symbol-function 'memq))
                       (assq-orig (symbol-function 'assq))
                       (membership
                        (buffer-local-value
                         'org-air-view--source-item-membership board))
                       (file-index
                        (buffer-local-value
                         'org-air-view--source-items-by-file board))
                       (locator-index
                        (buffer-local-value
                         'org-air-view--source-locator-index source))
                       (maps 0) (exacts 0) (prunes 0) (hydrates 0)
                       (finds 0) (forbidden 0) (opens 0) (queries 0)
                       (refreshes 0) observations)
                  (cl-letf (((symbol-function 'org-map-entries)
                             (lambda (&rest args)
                               (cl-incf maps) (apply map-orig args)))
                            ((symbol-function
                              'org-air-view--source-heading-exact-p)
                             (lambda (&rest args)
                               (cl-incf exacts) (apply exact-orig args)))
                            ((symbol-function 'org-air-view--source-prune-buffer)
                             (lambda (&rest args)
                               (cl-incf prunes) (apply prune-orig args)))
                            ((symbol-function 'org-air-view--hydrate-source-items)
                             (lambda (&rest args)
                               (cl-incf hydrates) (apply hydrate-orig args)))
                            ((symbol-function 'find-buffer-visiting)
                             (lambda (&rest args)
                               (cl-incf finds) (apply find-orig args)))
                            ((symbol-function 'memq)
                             (lambda (object list)
                               (when (eq (type-of object) 'org-air-item)
                                 (cl-incf forbidden))
                               (funcall memq-orig object list)))
                            ((symbol-function 'assq)
                             (lambda (object list)
                               (when (eq (type-of object) 'org-air-item)
                                 (cl-incf forbidden))
                               (funcall assq-orig object list)))
                            ((symbol-function 'find-file-noselect)
                             (lambda (&rest _)
                               (cl-incf opens) (error "open")))
                            ((symbol-function 'org-air-query-items)
                             (lambda (&rest _)
                               (cl-incf queries) (error "query")))
                            ((symbol-function 'org-air-query-items-in-files)
                             (lambda (&rest _)
                               (cl-incf queries) (error "query")))
                            ((symbol-function 'org-air-refresh)
                             (lambda (&rest _)
                               (cl-incf refreshes) (error "refresh")))
                            ((symbol-function 'run-with-timer)
                             (lambda (seconds repeat callback &rest arguments)
                               (push (list :constructor 'run-with-timer
                                           :seconds seconds :repeat repeat
                                           :callback callback
                                           :arguments arguments)
                                     observations)
                               nil))
                            ((symbol-function 'run-with-idle-timer)
                             (lambda (seconds repeat callback &rest arguments)
                               (push (list :constructor 'run-with-idle-timer
                                           :seconds seconds :repeat repeat
                                           :callback callback
                                           :arguments arguments)
                                     observations)
                               nil)))
                    (with-current-buffer board
                      ;; Direct already-current hydration itself is O(1).
                      (org-air-view--hydrate-source-items source)
                      (setq hydrates 0)
                      (let ((org-air-view-width 120)
                            (org-air-view-height 40)
                            (org-air-show-inspector nil)
                            (org-air-view--inspector-active nil))
                        (org-air-view--render items nil)
                        (org-air-filter '("#file_native"))
                        (setq org-air-view--tag-filter nil)
                        (org-air-view--render items nil)
                        (goto-char (point-min))
                        (org-air-view--goto-first-item)
                        (org-air-toggle-mark)
                        (goto-char
                         (or (text-property-not-all
                              (point-min) (point-max) 'org-air-section nil)
                             (point-min)))
                        (org-air-toggle-section)))
                  (should (= maps 0))
                  (should (= exacts 0))
                  (should (= prunes 0))
                  (should (= hydrates 0))
                  (should (= finds 0))
                  (should (= forbidden 0))
                  (should (= opens 0))
                  (should (= queries 0))
                  (should (= refreshes 0))
                  (should-not (seq-filter #'org-air-r90--org-air-timer-p
                                          observations))
                  (with-current-buffer board
                    (should (eq membership
                                org-air-view--source-item-membership))
                    (should (eq file-index
                                org-air-view--source-items-by-file)))
                  (with-current-buffer source
                    (should (eq locator-index
                                org-air-view--source-locator-index))
                    (should (= 0 (org-air-r90--undo-disk-guard-count))))
                  ;; A replacement generation does one scan + N validations,
                  ;; with indexed membership and no nested list lookup.
                  (setq maps 0 exacts 0 prunes 0 hydrates 0 finds 0
                        forbidden 0 opens 0 queries 0 refreshes 0
                        observations nil
                        old-markers
                        (with-current-buffer source
                          (mapcar #'cdr
                                  org-air-view--source-tracked-locators)))
                  (let ((replacement (mapcar #'copy-sequence items)))
                    (with-current-buffer board
                      (should-not (eq replacement org-air-view--source-generation))
                      (let ((org-air-view-width 120)
                            (org-air-view-height 40)
                            (org-air-show-inspector nil)
                            (org-air-view--inspector-active nil))
                        (org-air-view--render replacement nil))
                      (should (eq replacement org-air-view--source-generation))
                      (should (= count
                                 (hash-table-count
                                  org-air-view--source-item-membership))))
                    (ert-info ((format (concat "replacement counters map=%S exact=%S "
                                               "prune=%S hydrate=%S find=%S "
                                               "owner=%S current=%S tracked=%S")
                                       maps exacts prunes hydrates finds
                                       org-air-view--source-tracking-owner board
                                       (buffer-local-value
                                        'org-air-view--source-tracked-buffers
                                        board)))
                      (should (= maps 1))
                      (should (= exacts count))
                      (should (= prunes 1)))
                    (should (= hydrates 1))
                    (should (= finds 1))
                    (should (= forbidden 0))
                    (should (= opens 0))
                    (should (= queries 0))
                    (should (= refreshes 0))
                    (should-not (seq-filter #'org-air-r90--org-air-timer-p
                                            observations))
                    (with-current-buffer source
                      (should (eq replacement
                                  org-air-view--source-locator-generation))
                      (should org-air-view--source-locator-complete)
                      (should (= count
                                 (hash-table-count
                                  org-air-view--source-locator-index)))
                      (should (= count
                                 (length
                                  org-air-view--source-tracked-locators)))
                      (should (= 0 (org-air-r90--undo-disk-guard-count))))
                    (should-not (marker-buffer (car old-markers)))
                    (should-not (marker-buffer (car (last old-markers))))))))
            (when (buffer-live-p board) (kill-buffer board))))))))

;;;; r90-51 — canonical owner rename/mode/stale-hook recovery.

(ert-deftest org-air-r90-51-owner-rename-mode-and-stale-hook-recover ()
  "Rename/mode teardown recovers; stale hydration releases only exact owner."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-corpus
      '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha\n* TODO Beta\n")
        ("later.org" . "#+title: later\n\n* TODO Later\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let (old new source old-markers mode-markers)
      (unwind-protect
          (progn
            (org-air)
            (setq old (get-buffer org-air-view-buffer-name)
                  source (find-file-noselect (org-air-r90--file "tasks.org"))
                  old-markers
                  (with-current-buffer source
                    (mapcar #'cdr org-air-view--source-tracked-locators)))
            (with-current-buffer old
              (rename-buffer "*org-air-r90-renamed*" t))
            (org-air)
            (setq new (get-buffer org-air-view-buffer-name))
            (should (buffer-live-p old))
            (should (eq new org-air-view--source-tracking-owner))
            (should (= 1 (org-air-r90--owner-hook-count)))
            (should-not (buffer-local-value
                         'org-air-view--source-tracked-buffers old))
            (dolist (marker old-markers) (should-not (marker-buffer marker)))
            (should (eq new (buffer-local-value
                             'org-air-view--source-locator-owner source)))
            (should (= 2 (with-current-buffer source
                           (length org-air-view--source-tracked-locators))))
            (kill-buffer old)
            (org-air)
            (should (eq new org-air-view--source-tracking-owner))
            (should (= 1 (org-air-r90--owner-hook-count)))
            (setq mode-markers
                  (with-current-buffer source
                    (mapcar #'cdr org-air-view--source-tracked-locators)))
            (with-current-buffer new (fundamental-mode))
            (should-not org-air-view--source-tracking-owner)
            (should (= 0 (org-air-r90--owner-hook-count)))
            (should-not (buffer-local-value
                         'org-air-view--source-locator-owner source))
            (should-not (buffer-local-value
                         'org-air-view--source-tracked-locators source))
            (dolist (marker mode-markers) (should-not (marker-buffer marker)))
            ;; The already-live canonical buffer is reinitialized and claims.
            (org-air)
            (should (eq new org-air-view--source-tracking-owner))
            (should (with-current-buffer new
                      (derived-mode-p 'org-air-view-mode)))
            (should (= 1 (org-air-r90--owner-hook-count)))
            (should (= 2 (with-current-buffer source
                           (length org-air-view--source-tracked-locators)))))
        (when (buffer-live-p new) (kill-buffer new))
        (when (buffer-live-p old) (kill-buffer old))))
    ;; A stale find-file callback validates and relinquishes an invalid owner;
    ;; foreign owner metadata remains untouched.
    (let (board source foreign fake foreign-marker)
      (unwind-protect
          (progn
            (org-air)
            (setq board (get-buffer org-air-view-buffer-name)
                  source (find-file-noselect (org-air-r90--file "tasks.org"))
                  foreign (generate-new-buffer " *r90 foreign source*")
                  fake (generate-new-buffer " *r90 foreign owner*"))
            (with-current-buffer foreign
              (setq foreign-marker (copy-marker (point-min)))
              (setq-local org-air-view--source-locator-owner fake)
              (setq-local org-air-view--source-tracked-locators
                          (list (cons 'foreign foreign-marker))))
            (should (= 1 (org-air-r90--owner-hook-count)))
            (with-current-buffer board
              (rename-buffer "*org-air-r90-stale-owner*" t))
            ;; Opening a source runs the still-installed callback, which sees
            ;; the owner is no longer canonical and tears it down once.
            (find-file-noselect (org-air-r90--file "later.org"))
            (should-not org-air-view--source-tracking-owner)
            (should (= 0 (org-air-r90--owner-hook-count)))
            (should-not (buffer-local-value
                         'org-air-view--source-locator-owner source))
            (should (eq fake (buffer-local-value
                              'org-air-view--source-locator-owner foreign)))
            (should (marker-buffer foreign-marker))
            (should (= 1 (with-current-buffer foreign
                           (length org-air-view--source-tracked-locators)))))
        (when (markerp foreign-marker) (set-marker foreign-marker nil))
        (when (buffer-live-p foreign) (kill-buffer foreign))
        (when (buffer-live-p fake) (kill-buffer fake))
        (when (buffer-live-p board) (kill-buffer board))))))

;;;; r90-52 — native tag validation and dispatch safety.

(ert-deftest org-air-r90-52-native-tag-validator-backlog-and-chrome-dispatch ()
  "Validator follows org-tag-re; invalid backlog is inert; chrome dispatch is exact."
  (skip-unless (locate-library "org-air"))
  (dolist (tag '("shared_tag" "@context" "hash#tag" "日本語"
                 "shared-tag" "not/a/tag" "bad tag" "bad:tag" ""))
    (should (eq (and (string-match-p
                      (concat "\\`\\(?:" org-tag-re "\\)\\'") tag) t)
                (and (org-air-query--single-tag-value-p tag) t))))
  ;; An invalid custom backlog tag refuses in marked and unmarked paths with
  ;; no bytes, history, marks, focus, or cache identity changed.
  (dolist (marked '(nil t))
    (org-air-r90--with-board
        '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha\n* TODO Beta\n")
          ("inbox.org" . "#+title: inbox\n"))
      (org-air-r90--goto-row "Alpha")
      (when marked (org-air-toggle-mark))
      (let ((before-disk (org-air-r90--text "tasks.org"))
            (before-live (org-air-r90--live-text "tasks.org"))
            (before-items org-air-view--items)
            (before-marks org-air-view--marked-keys)
            (before-point (point))
            (org-air-backlog-tag "invalid-backlog"))
        (should-error (org-air-item-backlog) :type 'user-error)
        (should (equal before-disk (org-air-r90--text "tasks.org")))
        (should (equal before-live (org-air-r90--live-text "tasks.org")))
        (should (eq before-items org-air-view--items))
        (should (eq before-marks org-air-view--marked-keys))
        (should (= before-point (point)))
        (should-not org-air-view--edit-ring)
        (should-not org-air-view--edit-redo-ring))))
  ;; Unmarked chrome refuses before prompting; marked chrome prompts once and
  ;; applies to the durable marked target rather than point.
  (org-air-r90--with-board
      '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha\n* TODO Beta\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let ((prompts 0))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (cl-incf prompts) "shared_tag")))
        (should-error (org-air-set-tag) :type 'user-error))
      (should (= prompts 0))
      (org-air-r90--mark-title "Alpha")
      (goto-char (point-min))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (cl-incf prompts) "shared_tag")))
        (org-air-set-tag))
      (should (= prompts 1))
      (should (org-air-r90--disk-has-tag-p
               "tasks.org" "Alpha" "shared_tag"))
      (should-not (org-air-r90--disk-has-tag-p
                   "tasks.org" "Beta" "shared_tag"))
      (should-not org-air-view--marked-keys))))

;;;; r90-53 — finalizer stop across later files and history residuals.

(ert-deftest org-air-r90-53-finalizer-stops-later-files-and-keeps-residual-honest ()
  "Mandatory invalidation stops bulk/history loops with one rebuild and no redo."
  (skip-unless (locate-library "org-air"))
  ;; Initial bulk: A and failing B commit; detached C is never touched.
  (org-air-r90--with-board
      '(("a.org" . "#+title: a\n\n* TODO A\n")
        ("b.org" . "#+title: b\n\n* TODO B\n")
        ("c.org" . "#+title: c\n\n* TODO C\n")
        ("inbox.org" . "#+title: inbox\n"))
    (dolist (title '("A" "B" "C")) (org-air-r90--mark-title title))
    (let ((write-orig (symbol-function 'org-air-view--cache-sync-write-slots))
          (query-orig (symbol-function 'org-air-query-items))
          (queries 0) calls warnings)
      (cl-letf (((symbol-function 'org-air-view--cache-sync-write-slots)
                 (lambda (item file position tags)
                   (push (org-air-item-title item) calls)
                   (if (equal "B" (org-air-item-title item))
                       (error "mandatory B failure")
                     (funcall write-orig item file position tags))))
                ((symbol-function 'org-air-query-items)
                 (lambda (&rest args)
                   (cl-incf queries) (apply query-orig args)))
                ((symbol-function 'display-warning)
                 (lambda (&rest args) (push args warnings))))
        (org-air-item-backlog))
      (should (equal '("A" "B") (nreverse calls)))
      (should (= queries 1))
      (should (= (length warnings) 1)))
    (should (org-air-r90--disk-has-tag-p "a.org" "A" "backlog"))
    (should (org-air-r90--disk-has-tag-p "b.org" "B" "backlog"))
    (should-not (org-air-r90--disk-has-tag-p "c.org" "C" "backlog"))
    (should-not org-air-view--marked-keys)
    (should-not org-air-view--edit-redo-ring)
    (let ((parts (plist-get (car org-air-view--edit-ring) :parts)))
      (should (equal '("a.org" "b.org")
                     (mapcar (lambda (part)
                               (file-name-nondirectory
                                (plist-get part :file)))
                             parts)))))
  ;; History undo runs C then failing B, never detached A; only untouched A
  ;; remains honestly undoable and no speculative redo branch is created.
  (org-air-r90--with-board
      '(("a.org" . "#+title: a\n\n* TODO A\n")
        ("b.org" . "#+title: b\n\n* TODO B\n")
        ("c.org" . "#+title: c\n\n* TODO C\n")
        ("inbox.org" . "#+title: inbox\n"))
    (dolist (title '("A" "B" "C")) (org-air-r90--mark-title title))
    (org-air-item-backlog)
    (let ((write-orig (symbol-function 'org-air-view--cache-sync-write-slots))
          (query-orig (symbol-function 'org-air-query-items))
          (queries 0) calls warnings)
      (cl-letf (((symbol-function 'org-air-view--cache-sync-write-slots)
                 (lambda (item file position tags)
                   (push (org-air-item-title item) calls)
                   (if (equal "B" (org-air-item-title item))
                       (error "mandatory B undo failure")
                     (funcall write-orig item file position tags))))
                ((symbol-function 'org-air-query-items)
                 (lambda (&rest args)
                   (cl-incf queries) (apply query-orig args)))
                ((symbol-function 'display-warning)
                 (lambda (&rest args) (push args warnings))))
        (org-air-edit-undo))
      (should (equal '("C" "B") (nreverse calls)))
      (should (= queries 1))
      (should (= (length warnings) 1)))
    (should (org-air-r90--disk-has-tag-p "a.org" "A" "backlog"))
    (should-not (org-air-r90--disk-has-tag-p "b.org" "B" "backlog"))
    (should-not (org-air-r90--disk-has-tag-p "c.org" "C" "backlog"))
    (should-not org-air-view--edit-redo-ring)
    (let* ((record (car org-air-view--edit-ring))
           (parts (plist-get record :parts)))
      (should (string-match-p "residual 1 file" (plist-get record :desc)))
      (should (= 1 (length parts)))
      (should (equal "a.org"
                     (file-name-nondirectory
                      (plist-get (car parts) :file)))))))

;;;; r90-54..60 — FIX-7 recursive guard durability and disk-truth audit.

(defun org-air-r90--undo-disk-guard-entry-p (entry)
  "Return non-nil when ENTRY is the exact function-only disk guard."
  (equal entry '(apply org-air-view--undo-disk-truth-guard)))

(defun org-air-r90--undo-disk-guard-count (&optional undo-list)
  "Count disk guards in UNDO-LIST without looping over malformed cycles."
  (let ((tail (or undo-list buffer-undo-list))
        (seen (make-hash-table :test #'eq))
        (count 0))
    (while (and (consp tail) (not (gethash tail seen)))
      (puthash tail t seen)
      (when (org-air-r90--undo-disk-guard-entry-p (car tail))
        (cl-incf count))
      (setq tail (cdr tail)))
    count))

(defun org-air-r90--newest-undo-group (undo-list)
  "Return `(ENTRIES BOUNDARY)' for UNDO-LIST's newest bounded group."
  (let ((tail undo-list) entries)
    (while (and (consp tail) (car tail))
      (push (car tail) entries)
      (setq tail (cdr tail)))
    (list (nreverse entries) tail)))

(defun org-air-r90--assert-guard-old-edge (source part)
  "Assert SOURCE has one exact old-edge guard for history PART."
  (with-current-buffer source
    (let* ((raw (org-air-view--history-identity-resolve
                 (plist-get part :expected-undo)))
           (group (org-air-r90--newest-undo-group buffer-undo-list))
           (entries (car group))
           (boundary (cadr group))
           (guard (car (last entries))))
      (should (consp raw))
      (should (= 1 (org-air-r90--undo-disk-guard-count buffer-undo-list)))
      (should (consp boundary))
      (should-not (car boundary))
      (should (eq (cdr boundary) raw))
      ;; Head-to-old order is text, save-state, guard, terminating boundary.
      (should (org-air-r90--undo-disk-guard-entry-p guard))
      (should (= 2 (length guard)))
      (should (symbolp (cadr guard)))
      (should (seq-some (lambda (entry)
                          (and (consp entry) (eq (car entry) t)))
                        (butlast entries)))
      (should (= 1 (seq-count
                    (lambda (entry)
                      (and (consp entry) (eq (car entry) 'apply)))
                    entries))))))

(defun org-air-r90--same-command-state-p (before name source)
  "Return non-nil when all observable command state still equals BEFORE."
  (let ((after (org-air-r90--command-state name source)))
    (and (equal (plist-get before :disk) (plist-get after :disk))
         (equal (plist-get before :live) (plist-get after :live))
         (= (plist-get before :tick) (plist-get after :tick))
         (eq (plist-get before :modified) (plist-get after :modified))
         (eq (plist-get before :items) (plist-get after :items))
         (equal (plist-get before :item-data) (plist-get after :item-data))
         (eq (plist-get before :classify) (plist-get after :classify))
         (equal (plist-get before :classify-count)
                (plist-get after :classify-count))
         (eq (plist-get before :marks) (plist-get after :marks))
         (eq (plist-get before :mark-table) (plist-get after :mark-table))
         (org-air-r90--same-object-order-p
          (plist-get before :edit-ring) (plist-get after :edit-ring))
         (org-air-r90--same-object-order-p
          (plist-get before :redo-ring) (plist-get after :redo-ring))
         (= (plist-get before :point) (plist-get after :point))
         (eq (plist-get before :selected) (plist-get after :selected))
         (equal (plist-get before :windows) (plist-get after :windows))
         (eq (plist-get before :inspector) (plist-get after :inspector))
         (eq (plist-get before :pending) (plist-get after :pending))
         (equal (plist-get before :pane) (plist-get after :pane)))))

(defun org-air-r90--invoke-inverse (direction)
  "Invoke the inverse of committed history DIRECTION."
  (if (memq direction '(initial-backlog initial-tag redo))
      (org-air-edit-undo)
    (org-air-edit-redo)))

(defun org-air-r90--arm-recursive-compound (direction)
  "Create one recursive compound DIRECTION and return its audit facts."
  (let ((before (org-air-r90--text "tasks.org")))
    (dolist (title '("Alpha" "Beta")) (org-air-r90--mark-title title))
    (if (memq direction '(initial-backlog initial-tag))
        (let* ((source (find-file-noselect (org-air-r90--file "tasks.org")))
               (cell (list nil))
               (hook (org-air-r90--nested-commit-hook 'comment nil cell)))
          (with-current-buffer source (add-hook 'after-save-hook hook nil t))
          (unwind-protect
              (if (eq direction 'initial-tag)
                  (cl-letf (((symbol-function 'read-string)
                             (lambda (&rest _) "shared_tag")))
                    (org-air-set-tag))
                (org-air-item-backlog))
            (with-current-buffer source
              (remove-hook 'after-save-hook hook t)))
          (list :source source :before before
                :committed (org-air-r90--text "tasks.org")
                :record (car org-air-view--edit-ring)
                :source-ring 'undo))
      (org-air-item-backlog)
      (let ((committed (org-air-r90--text "tasks.org")))
        (when (eq direction 'redo) (org-air-edit-undo))
        (let* ((source (find-file-noselect (org-air-r90--file "tasks.org")))
               (cell (list nil))
               (hook (org-air-r90--nested-commit-hook 'comment nil cell)))
          (with-current-buffer source (add-hook 'after-save-hook hook nil t))
          (unwind-protect
              (if (eq direction 'undo)
                  (org-air-edit-undo)
                (org-air-edit-redo))
            (with-current-buffer source
              (remove-hook 'after-save-hook hook t)))
          (list :source source :before before :committed committed
                :record (car (if (eq direction 'undo)
                                 org-air-view--edit-redo-ring
                               org-air-view--edit-ring))
                :source-ring (if (eq direction 'undo) 'redo 'undo)))))))

(ert-deftest org-air-r90-54-unsaved-hook-resolution-blocks-before-manual-save ()
  "A hook-only undo is modified and every org-air inverse blocks until save."
  (skip-unless (locate-library "org-air"))
  (let (observations)
    (dolist (direction '(initial-backlog initial-tag undo redo))
      (let ((pending-undo-list nil)
            (undo-equiv-table (make-hash-table :test #'eq))
            (last-command nil)
            (this-command nil))
        (org-air-r90--with-board
            '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha\n* TODO Beta\n")
              ("inbox.org" . "#+title: inbox\n"))
          (let* ((facts (org-air-r90--arm-recursive-compound direction))
                 (source (plist-get facts :source))
                 (record (plist-get facts :record))
                 (part (car (plist-get record :parts)))
                 (immediate-disk (org-air-r90--text "tasks.org"))
                 (immediate-live (org-air-r90--live-text "tasks.org"))
                 (immediate-undo (copy-sequence org-air-view--edit-ring))
                 (immediate-redo (copy-sequence org-air-view--edit-redo-ring))
                 (immediate-saves 0))
            (org-air-r90--assert-guard-old-edge source part)
            ;; Existing r90-49's immediate inverse remains an exact zero-byte
            ;; same-ring block before the user resolves the hook group.
            (cl-letf (((symbol-function 'save-buffer)
                       (lambda (&rest _) (cl-incf immediate-saves))))
              (org-air-r90--invoke-inverse direction))
            (should (= immediate-saves 0))
            (should (equal immediate-disk (org-air-r90--text "tasks.org")))
            (should (equal immediate-live
                           (org-air-r90--live-text "tasks.org")))
            (should (org-air-r90--same-object-order-p
                     immediate-undo org-air-view--edit-ring))
            (should (org-air-r90--same-object-order-p
                     immediate-redo org-air-view--edit-redo-ring))
            ;; Undo ONLY the recursive hook group.  Its guard executes after
            ;; text/save-state and must expose a truthful unsaved resolution.
            (with-current-buffer source (undo-boundary) (undo-only))
            (let* ((disk (org-air-r90--text "tasks.org"))
                   (live (org-air-r90--live-text "tasks.org"))
                   (undo-list (buffer-local-value 'buffer-undo-list source))
                   (before-block
                    (org-air-r90--command-state "tasks.org" source))
                   (saves 0)
                   inverse-error)
              (should-not (equal disk live))
              (should (string-match-p "nested committed hook" disk))
              (should-not (string-match-p "nested committed hook" live))
              (should (buffer-modified-p source))
              ;; Exact undo equivalence is not permission to overwrite this
              ;; unsaved user resolution.  The injected save must be unreachable.
              (cl-letf (((symbol-function 'save-buffer)
                         (lambda (&rest _)
                           (cl-incf saves)
                           (error "org-air inverse reached source save"))))
                (setq inverse-error
                      (condition-case err
                          (progn (org-air-r90--invoke-inverse direction) nil)
                        ((error quit) err))))
              (let ((blocked
                     (and (= saves 0)
                          (null inverse-error)
                          (eq undo-list
                              (buffer-local-value 'buffer-undo-list source))
                          (org-air-r90--same-command-state-p
                           before-block "tasks.org" source))))
                ;; A correct implementation reaches the explicit resolution
                ;; path only after the user chooses to save live truth.
                (let ((retry-ok nil))
                  (when blocked
                    (with-current-buffer source (save-buffer))
                    (should (equal (org-air-r90--text "tasks.org")
                                   (org-air-r90--live-text "tasks.org")))
                    (should-not (buffer-modified-p source))
                    (condition-case nil
                        (progn
                          (org-air-r90--invoke-inverse direction)
                          (setq retry-ok t))
                      ((error quit) nil)))
                  (push (list direction :blocked blocked :saves saves
                              :retry retry-ok)
                        observations))))))))
    (setq observations (nreverse observations))
    (ert-info ((format "unsaved recursive resolution observations: %S"
                       observations))
      (should (equal
               observations
               '((initial-backlog :blocked t :saves 0 :retry t)
                 (initial-tag :blocked t :saves 0 :retry t)
                 (undo :blocked t :saves 0 :retry t)
                 (redo :blocked t :saves 0 :retry t)))))))

(ert-deftest org-air-r90-55-disk-truth-adversity-blocks-with-zero-source-write ()
  "Deleted/unreadable/replaced/stale/error disk truth blocks exact history."
  (skip-unless (locate-library "org-air"))
  (let (observations)
    (dolist (case '(deleted unreadable external-replaced stale-modtime
                   comparison-error))
      (let ((pending-undo-list nil)
            (undo-equiv-table (make-hash-table :test #'eq))
            (last-command nil)
            (this-command nil))
        (org-air-r90--with-board
            '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha\n* TODO Beta\n")
              ("inbox.org" . "#+title: inbox\n"))
          (let* ((facts (org-air-r90--arm-recursive-compound 'undo))
                 (source (plist-get facts :source))
                 (file (org-air-r90--file "tasks.org"))
                 (pre-adversity-disk (org-air-r90--text "tasks.org"))
                 (comparison-error (eq case 'comparison-error)))
            (if comparison-error
                (cl-letf (((symbol-function 'insert-file-contents)
                           (lambda (&rest _) (error "handler compare failed"))))
                  (with-current-buffer source (undo-boundary) (undo-only)))
              (with-current-buffer source (undo-boundary) (undo-only)))
            (when (eq case 'stale-modtime)
              ;; Resolve explicitly first, then make durability metadata stale
              ;; without changing bytes.
              (with-current-buffer source (save-buffer))
              (set-file-times file (time-add (current-time) 3600)))
            (pcase case
              ('deleted (delete-file file))
              ('unreadable (set-file-modes file #o000))
              ('external-replaced
               (write-region "EXTERNAL REPLACEMENT\n" nil file nil 'silent)))
            (let* ((existed (file-exists-p file))
                   (disk (pcase case
                           ('deleted nil)
                           ('unreadable pre-adversity-disk)
                           (_ (and existed
                                   (org-air-r90--text "tasks.org")))))
                   (live (with-current-buffer source
                           (buffer-substring-no-properties
                            (point-min) (point-max))))
                   (undo-list (buffer-local-value 'buffer-undo-list source))
                   (undo-order (copy-sequence org-air-view--edit-ring))
                   (redo-order (copy-sequence org-air-view--edit-redo-ring))
                   (saves 0))
              (cl-letf (((symbol-function 'save-buffer)
                         (lambda (&rest _)
                           (cl-incf saves)
                           (error "disk-truth inverse attempted save"))))
                (condition-case nil
                    (org-air-edit-redo)
                  ((error quit) nil)))
              (when (eq case 'unreadable) (set-file-modes file #o600))
              (push
               (list case :saves saves
                     :exists (eq existed (file-exists-p file))
                     :disk (equal disk
                                  (and (file-exists-p file)
                                       (condition-case nil
                                           (org-air-r90--text "tasks.org")
                                         (error :unreadable))))
                     :live (equal live
                                  (with-current-buffer source
                                    (buffer-substring-no-properties
                                     (point-min) (point-max))))
                     :undo-list
                     (eq undo-list
                         (buffer-local-value 'buffer-undo-list source))
                     :rings
                     (and (org-air-r90--same-object-order-p
                           undo-order org-air-view--edit-ring)
                          (org-air-r90--same-object-order-p
                           redo-order org-air-view--edit-redo-ring)))
               observations))))))
    (setq observations (nreverse observations))
    (ert-info ((format "disk-truth adversity observations: %S" observations))
      (dolist (observation observations)
        (should (= 0 (plist-get (cdr observation) :saves)))
        (should (plist-get (cdr observation) :exists))
        (should (plist-get (cdr observation) :disk))
        (should (plist-get (cdr observation) :live))
        (should (plist-get (cdr observation) :undo-list))
        (should (plist-get (cdr observation) :rings))))))

(ert-deftest org-air-r90-56-guard-shape-scope-lifecycle-and-retention ()
  "One old-edge function guard tracks undo/redo and never enters history."
  (skip-unless (locate-library "org-air"))
  (let ((pending-undo-list nil)
        (undo-equiv-table (make-hash-table :test #'eq))
        (last-command nil)
        (this-command nil))
    (org-air-r90--with-board
        '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha\n* TODO Beta\n")
          ("inbox.org" . "#+title: inbox\n"))
      (let* ((facts (org-air-r90--arm-recursive-compound 'initial-backlog))
             (source (plist-get facts :source))
             (record (plist-get facts :record))
             (part (car (plist-get record :parts)))
             (label "nested committed hook"))
        (org-air-r90--assert-guard-old-edge source part)
        (let ((printed (let ((print-circle t) (print-level nil)
                             (print-length nil))
                         (prin1-to-string
                          (list org-air-view--edit-ring
                                org-air-view--edit-redo-ring)))))
          (should-not (string-match-p "undo-disk-truth-guard" printed))
          (should-not (string-match-p label printed)))
        ;; Undo hook, save; redo hook, save; undo it once more.  The one custom
        ;; entry follows Emacs history without duplication or false cleanliness.
        (with-current-buffer source
          (undo-boundary) (undo-only)
          (should (buffer-modified-p))
          (save-buffer)
          (should-not (buffer-modified-p))
          (should (equal (org-air-r90--text "tasks.org")
                         (org-air-r90--live-text "tasks.org")))
          (undo-boundary) (undo-redo)
          (should (buffer-modified-p))
          (save-buffer)
          (should-not (buffer-modified-p))
          (undo-boundary) (undo-only)
          (should (buffer-modified-p))
          (should (= 1 (org-air-r90--undo-disk-guard-count))))
        (with-current-buffer source
          (set-buffer-modified-p nil))
        (kill-buffer source)
        (org-air-r90--force-gc)
        (should (zerop (hash-table-count
                        org-air-view--history-identity-registry)))
        (should-not (string-match-p
                     "undo-disk-truth-guard"
                     (let ((print-circle t))
                       (prin1-to-string
                        (list org-air-view--edit-ring
                              org-air-view--edit-redo-ring)))))))))

(ert-deftest org-air-r90-61-ordinary-save-and-no-op-paths-have-no-guard ()
  "Ordinary/no-op/nonrecursive signal or mutation saves install no guard."
  (skip-unless (locate-library "org-air"))
  (dolist (shape '(ordinary no-op signal-only unsaved-hook))
    (org-air-r90--with-corpus
        '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha\n")
          ("inbox.org" . "#+title: inbox\n"))
      (let ((source (find-file-noselect (org-air-r90--file "tasks.org")))
            hook)
        (with-current-buffer source
          (org-mode)
          (unless (eq shape 'no-op)
            (goto-char (point-max)) (insert "# ordinary dirty\n"))
          (setq hook
                (pcase shape
                  ('signal-only (lambda () (error "signal only")))
                  ('unsaved-hook
                   (lambda ()
                     (goto-char (point-min)) (insert "# unsaved hook\n")))))
          (when hook (add-hook 'after-save-hook hook nil t))
          (unwind-protect (org-air-view--save-attempt)
            (when hook (remove-hook 'after-save-hook hook t)))
          (should (= 0 (org-air-r90--undo-disk-guard-count))))))))

(defun org-air-r90--isolated-undo-shape (expected entries)
  "Return ENTRIES terminated by a boundary whose cdr is EXPECTED."
  (let ((boundary (cons nil expected))
        (head (copy-sequence entries)))
    (if head
        (setcdr (last head) boundary)
      (setq head boundary))
    head))

(defun org-air-r90--guard-child-result (name form)
  "Run guard audit FORM in child Emacs NAME and return its bounded result."
  (let* ((buffer (generate-new-buffer (format " *r90 %s child*" name)))
         (emacs (expand-file-name invocation-name invocation-directory))
         (init (expand-file-name "tests/org-air-test-init.el"
                                 default-directory))
         (process
          (make-process :name (format "r90-%s" name) :buffer buffer
                        :command (list emacs "-Q" "--batch" "-l" init
                                       "--eval" form)
                        :connection-type 'pipe :noquery t))
         (deadline (time-add (current-time) 2.0))
         timed-out output status)
    (unwind-protect
        (progn
          (while (and (process-live-p process)
                      (time-less-p (current-time) deadline))
            (accept-process-output process 0.05))
          (when (process-live-p process)
            (setq timed-out t)
            (delete-process process))
          (setq status (unless timed-out (process-exit-status process))
                output (with-current-buffer buffer (buffer-string)))
          (list :timeout timed-out :status status :output output))
      (when (process-live-p process) (delete-process process))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(defun org-air-r90--cyclic-guard-child-result ()
  "Run cyclic guard insertion in a child Emacs and return its bounded result."
  (org-air-r90--guard-child-result
   "cyclic-guard"
   (concat
    "(progn (require 'org-air-view)"
    " (with-temp-buffer"
    "  (let ((node (cons '(1 . 2) nil)))"
    "   (setcdr node node) (setq buffer-undo-list node)"
    "   (prin1 (org-air-view--undo-disk-truth-guard-install '(tail))))))")))

(defun org-air-r90--cyclic-expected-tail-child-result ()
  "Run a cyclic expected-tail guard audit in a bounded child Emacs."
  (org-air-r90--guard-child-result
   "cyclic-expected-tail"
   (concat
    "(progn (require 'org-air-view)"
    " (with-temp-buffer"
    "  (let* ((expected (cons 'tail nil)) (boundary (cons nil expected))"
    "         (second (cons '(t . 0) boundary))"
    "         (head (cons '(1 . 2) second)))"
    "   (setcdr expected expected) (setq buffer-undo-list head)"
    "   (let ((old-edge (cdr second)))"
    "    (prin1 (list"
    "     (org-air-view--undo-disk-truth-guard-install expected)"
    "     (eq old-edge (cdr second)))))))))")))

(defun org-air-r90--insertion-failure-child-result ()
  "Run interpreted guard insertion failure in a child Emacs."
  (let ((source (expand-file-name "org-air-view.el" default-directory)))
    (org-air-r90--guard-child-result
     "guard-insertion-failure"
     (format
      (concat
       "(progn (load %S nil nil t)"
       " (with-temp-buffer"
       "  (let* ((expected (list 'tail))"
       "         (boundary (cons nil expected))"
       "         (head (list '(1 . 2) '(t . 0))))"
       "   (setcdr (cdr head) boundary) (setq buffer-undo-list head)"
       "   (let ((before (copy-tree head)))"
       "    (cl-letf (((symbol-function 'setcdr)"
       "               (lambda (&rest _) (error \"insertion failed\"))))"
       "     (prin1 (list"
       "      (org-air-view--undo-disk-truth-guard-install expected)"
       "      (equal before buffer-undo-list))))))))")
      source))))

(ert-deftest org-air-r90-57-conservative-forms-and-equiv-restoration ()
  "Malformed/ambiguous history never guesses; equivalence restores on exits."
  (skip-unless (locate-library "org-air"))
  (with-temp-buffer
    (dolist (value '(t nil atomic malformed))
      (setq buffer-undo-list
            (pcase value
              ('t t) ('nil nil) ('atomic '(atomic))
              ('malformed (cons '(1 . 2) 'dotted))))
      (should-not (org-air-view--undo-disk-truth-guard-install '(tail))))
    (let* ((expected (list 'tail))
           (custom (org-air-r90--isolated-undo-shape
                    expected '((1 . 2) (t . 0) (apply ignore))))
           (missing-save (org-air-r90--isolated-undo-shape
                          expected '((1 . 2))))
           (second-boundary (cons '(1 . 2)
                                  (cons nil
                                        (org-air-r90--isolated-undo-shape
                                         expected '((t . 0)))))))
      (dolist (undo-list (list custom missing-save second-boundary))
        (setq buffer-undo-list undo-list)
        (let ((before (copy-tree undo-list)))
          (should-not
           (org-air-view--undo-disk-truth-guard-install expected))
          (should (equal before buffer-undo-list))))))
  ;; The interpreted child makes the hostile `setcdr' seam reliable even when
  ;; the gate byte-compiles `setcdr' to a primitive opcode in the parent.
  (let ((child (org-air-r90--insertion-failure-child-result)))
    (ert-info ((format "guard insertion failure child: %S" child))
      (should-not (plist-get child :timeout))
      (should (zerop (plist-get child :status)))
      (should (string-match-p "(nil t)" (plist-get child :output)))))
  ;; The special redo path restores the exact prior undo-equivalence mapping
  ;; after normal return, `error', and `quit', including an absent mapping.
  (dolist (outcome '(error quit success))
    (dolist (present '(nil t))
      (let* ((org-air-view--history-identity-registry
              (make-hash-table :test #'eq :weakness 'key-and-value))
             (org-air-view--cache-sync-history
              (make-hash-table :test #'eq :weakness 'key))
             (undo-equiv-table (make-hash-table :test #'eq))
             (raw (list '(synthetic exact tail)))
             (mapping (list 'older outcome present))
             (token (org-air-view--history-identity-register raw))
             (record (list :buffer (current-buffer)
                           :expected-undo token)))
        (puthash record 'intervening-commit org-air-view--cache-sync-history)
        (when present (puthash raw mapping undo-equiv-table))
        (cl-letf (((symbol-function 'org-air-view--expected-redo-step)
                   (lambda (&rest _) nil))
                  ((symbol-function 'org-air-view--expected-undo-step)
                   (lambda (&rest _) raw))
                  ((symbol-function 'undo-only)
                   (lambda (&rest _)
                     (should-not (gethash raw undo-equiv-table))
                     (pcase outcome
                       ('error (error "synthetic undo error"))
                       ('quit (signal 'quit nil))))))
          (condition-case nil
              (org-air-view--history-apply-operation record 'redo)
            ((error quit) nil)))
        (if present
            (should (eq mapping (gethash raw undo-equiv-table)))
          (should-not (gethash raw undo-equiv-table))))))
  ;; Cyclic input must terminate conservatively instead of spinning Emacs.
  (let ((child (org-air-r90--cyclic-guard-child-result)))
    (ert-info ((format "cyclic guard child: %S" child))
      (should-not (plist-get child :timeout))
      (should (zerop (plist-get child :status)))
      (should (string-match-p "nil" (plist-get child :output)))))
  ;; The exact older expected tail is also untrusted structure.  A cycle
  ;; behind the terminating boundary must neither be blessed nor gain a guard.
  (let ((child (org-air-r90--cyclic-expected-tail-child-result)))
    (ert-info ((format "cyclic expected-tail child: %S" child))
      (should-not (plist-get child :timeout))
      (should (zerop (plist-get child :status)))
      (should (string-match-p "(nil t)" (plist-get child :output))))))

(defun org-air-r90--recursive-matrix-mutate (shape index)
  "Mutate recursive save SHAPE for matrix iteration INDEX."
  (pcase shape
    ('comment
     (goto-char (point-min))
     (insert (format "# recursive comment %d\n" index)))
    ('title
     (goto-char (point-min))
     (re-search-forward "^\\* TODO Alpha")
     (search-backward "Alpha")
     (replace-match "Alpha hooked"))
    ('tag
     (goto-char (point-min))
     (re-search-forward "^\\* TODO Alpha")
     (org-back-to-heading t)
     (org-toggle-tag "hook_tag" 'on))
    ('move
     (goto-char (point-min))
     (re-search-forward "^\\* TODO Alpha")
     (beginning-of-line)
     (let* ((beg (point))
            (end (progn (forward-line 1) (point)))
            (line (buffer-substring beg end)))
       (delete-region beg end)
       (goto-char (point-max))
       (unless (bolp) (insert "\n"))
       (insert line)))
    ('delete
     (goto-char (point-min))
     (re-search-forward "^\\* TODO Alpha")
     (beginning-of-line)
     (delete-region (point) (progn (forward-line 1) (point))))))

(ert-deftest org-air-r90-58-recursive-mechanism-matrix-rejects-stale-first-guard ()
  "save/basic recursion gets one guard; repeated groups invalidate identity."
  (skip-unless (locate-library "org-air"))
  ;; Run the complete single-group cross-product before the deliberately RED
  ;; repeated-group cases, so one stale-first-guard failure hides no matrix row.
  (dolist (repeats '(1 2))
    (dolist (mechanism '(save-buffer basic-save-buffer))
      (dolist (signalp '(nil t))
        (dolist (shape (if (= repeats 1)
                           '(comment title tag move delete)
                         '(comment)))
          (org-air-r90--with-corpus
              '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha\n* TODO Beta\n")
                ("inbox.org" . "#+title: inbox\n"))
            (let* ((source
                    (find-file-noselect (org-air-r90--file "tasks.org")))
                   (cell (list nil))
                   hook result)
              (setq hook
                    (lambda ()
                      (unless (car cell)
                        (setcar cell t)
                        (dotimes (index repeats)
                          (org-air-r90--recursive-matrix-mutate shape index)
                          (funcall mechanism))
                        (when signalp (error "recursive matrix signal")))))
              (with-current-buffer source
                (org-mode)
                (goto-char (point-max)) (insert "# outer dirty\n")
                (add-hook 'after-save-hook hook nil t)
                (unwind-protect
                    (setq result
                          (org-air-view--save-attempt (lambda () 'state)))
                  (remove-hook 'after-save-hook hook t))
                (ert-info ((format "mechanism=%S signal=%S shape=%S repeats=%S"
                                   mechanism signalp shape repeats))
                  (should (plist-get result :committed))
                  (should (plist-get result :recursive-commit))
                  (should (= 1 (org-air-r90--undo-disk-guard-count)))
                  (if (= repeats 1)
                      (progn
                        (should (eq 'installed
                                    (plist-get result :undo-disk-guard)))
                        (should (plist-get result :identity-known))
                        (should (consp (plist-get result :expected-undo))))
                    ;; A second separately saved group is ambiguous: the first
                    ;; guard must never bless a stale expected tail.
                    (should-not (plist-get result :identity-known))
                    (should-not (plist-get result :expected-undo))
                    (should-not (plist-get result :expected-redo))))))))))))

(ert-deftest org-air-r90-59-two-file-guarded-preflight-is-all-parts-zero-byte ()
  "One guarded and one ordinary part preflight before either file is touched."
  (skip-unless (locate-library "org-air"))
  (let ((pending-undo-list nil)
        (undo-equiv-table (make-hash-table :test #'eq))
        (last-command nil)
        (this-command nil))
    (org-air-r90--with-board
        '(("a.org" . "#+title: a\n\n* TODO A1\n* TODO A2\n")
          ("b.org" . "#+title: b\n\n* TODO B1\n")
          ("inbox.org" . "#+title: inbox\n"))
      ;; Two same-file targets still form one A part; B forms the second.
      (dolist (title '("A1" "A2" "B1")) (org-air-r90--mark-title title))
      (org-air-item-backlog)
      (let* ((record (car org-air-view--edit-ring))
             (parts (plist-get record :parts))
             (a (find-file-noselect (org-air-r90--file "a.org")))
             (b (find-file-noselect (org-air-r90--file "b.org")))
             (cell (list nil))
             (hook (org-air-r90--nested-commit-hook 'comment nil cell)))
        (should (equal '("a.org" "b.org")
                       (mapcar (lambda (part)
                                 (file-name-nondirectory
                                  (plist-get part :file)))
                               parts)))
        ;; Undo order is B then A, so A's recursive hook leaves one guarded
        ;; part while B has already completed normally.
        (with-current-buffer a (add-hook 'after-save-hook hook nil t))
        (unwind-protect (org-air-edit-undo)
          (with-current-buffer a (remove-hook 'after-save-hook hook t)))
        (let* ((record (car org-air-view--edit-redo-ring))
               (parts (plist-get record :parts))
               (a-part (car parts))
               (b-part (cadr parts)))
          (should (eq 'intervening-commit
                      (gethash a-part org-air-view--cache-sync-history)))
          (should-not (eq 'intervening-commit
                          (gethash b-part org-air-view--cache-sync-history)))
          (org-air-r90--assert-guard-old-edge a a-part)
          (with-current-buffer a (undo-boundary) (undo-only))
          (should (buffer-modified-p a))
          (let ((a-disk (org-air-r90--text "a.org"))
                (b-disk (org-air-r90--text "b.org"))
                (a-live (org-air-r90--live-text "a.org"))
                (b-live (org-air-r90--live-text "b.org"))
                (a-undo (buffer-local-value 'buffer-undo-list a))
                (b-undo (buffer-local-value 'buffer-undo-list b))
                (undo-order (copy-sequence org-air-view--edit-ring))
                (redo-order (copy-sequence org-air-view--edit-redo-ring))
                (pending pending-undo-list)
                (equiv undo-equiv-table)
                (saves 0))
            (cl-letf (((symbol-function 'save-buffer)
                       (lambda (&rest _)
                         (cl-incf saves)
                         (error "two-file inverse attempted save"))))
              (condition-case nil (org-air-edit-redo) ((error quit) nil)))
            (ert-info ((format "two-file unsaved preflight saves=%S" saves))
              (should (= saves 0)))
            (should (equal a-disk (org-air-r90--text "a.org")))
            (should (equal b-disk (org-air-r90--text "b.org")))
            (should (equal a-live (org-air-r90--live-text "a.org")))
            (should (equal b-live (org-air-r90--live-text "b.org")))
            (should (eq a-undo (buffer-local-value 'buffer-undo-list a)))
            (should (eq b-undo (buffer-local-value 'buffer-undo-list b)))
            (should (eq pending pending-undo-list))
            (should (eq equiv undo-equiv-table))
            (should (org-air-r90--same-object-order-p
                     undo-order org-air-view--edit-ring))
            (should (org-air-r90--same-object-order-p
                     redo-order org-air-view--edit-redo-ring))))))))

(ert-deftest org-air-r90-60-normal-compound-order-and-ui-paths-have-no-guard ()
  "Compound order is reverse/forward and non-save board paths add no guard."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-board
      '(("a.org" . "#+title: a\n\n* TODO A1\n* TODO A2\n")
        ("b.org" . "#+title: b\n\n* TODO B1\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let ((a (find-file-noselect (org-air-r90--file "a.org")))
          (b (find-file-noselect (org-air-r90--file "b.org"))))
      (org-air-view--refresh-current)
      (org-air-r90--goto-row "A1")
      (org-air-toggle-mark)
      (org-air-toggle-mark)
      (org-air-view-sort-cycle)
      (setq org-air-view--tag-filter '("#missing"))
      (org-air-view--render-current)
      (setq org-air-view--tag-filter nil)
      (org-air-view--render-current)
      (goto-char (point-min))
      (org-air-next-section)
      (org-air-toggle-section)
      (org-air-view--panes-resync-now)
      (should (= 0 (with-current-buffer a
                       (org-air-r90--undo-disk-guard-count))))
      (should (= 0 (with-current-buffer b
                       (org-air-r90--undo-disk-guard-count))))
      (dolist (title '("A1" "A2" "B1")) (org-air-r90--mark-title title))
      (org-air-item-backlog)
      (let ((original
             (symbol-function 'org-air-view--history-apply-operation))
            calls)
        (cl-letf (((symbol-function 'org-air-view--history-apply-operation)
                   (lambda (part operation)
                     (push (list operation
                                 (file-name-nondirectory
                                  (plist-get part :file)))
                           calls)
                     (funcall original part operation))))
          (org-air-edit-undo)
          (org-air-edit-redo))
        (should (equal (nreverse calls)
                       '((undo "b.org") (undo "a.org")
                         (redo "a.org") (redo "b.org"))))))))

;;;; r90-62..65 — review-4 permanent roots: cross-file per-part TOCTOU,
;;;; narrowed full-buffer truth, guard comparison failure recovery, and
;;;; indirect-clone canonical source ownership.

(defun org-air-r90--full-live-text (buffer)
  "Return BUFFER's COMPLETE live text regardless of any user narrowing."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun org-air-r90--cross-hook-processing-order (operation)
  "Return (FIRST . NEXT) corpus names for compound OPERATION processing.
Compound undo runs in reverse commit order and redo in commit order, so the
first processed file differs per direction."
  (if (eq operation 'undo) '("b.org" . "a.org") '("a.org" . "b.org")))

(defun org-air-r90--cross-hook-run (operation)
  "Run compound OPERATION while the first part's save hook edits the next part.
The hook is a real buffer-local `after-save-hook' on the first processed
source; it runs strictly after the command-wide preflight and inserts one
isolated unsaved user undo group into the next, not-yet-processed source.
Return audited facts observed at that exact instant and after the command."
  (dolist (title '("A1" "B1")) (org-air-r90--mark-title title))
  (org-air-item-backlog)
  (when (eq operation 'redo) (org-air-edit-undo))
  (let* ((order (org-air-r90--cross-hook-processing-order operation))
         (first-name (car order))
         (next-name (cdr order))
         (first-title (if (eq operation 'undo) "B1" "A1"))
         (next-title (if (eq operation 'undo) "A1" "B1"))
         (first-buffer (find-file-noselect (org-air-r90--file first-name)))
         (next-buffer (find-file-noselect (org-air-r90--file next-name)))
         (note "# late unsaved user note\n")
         (next-disk (org-air-r90--text next-name))
         (next-tags (org-air-r90--live-tags next-name next-title t))
         (ran 0)
         next-tick next-live messages
         (hook
          (lambda ()
            (cl-incf ran)
            (with-current-buffer next-buffer
              (undo-boundary)
              (save-excursion (goto-char (point-max)) (insert note))
              (setq next-tick (buffer-chars-modified-tick)
                    next-live (buffer-substring-no-properties
                               (point-min) (point-max)))))))
    (with-current-buffer first-buffer (add-hook 'after-save-hook hook nil t))
    (unwind-protect
        (org-air-r90--record-messages collected
          (if (eq operation 'undo) (org-air-edit-undo) (org-air-edit-redo))
          (setq messages (nreverse collected)))
      (with-current-buffer first-buffer (remove-hook 'after-save-hook hook t)))
    (list :operation operation :ran ran :note note
          :first first-buffer :next next-buffer
          :first-name first-name :next-name next-name
          :first-title first-title :next-title next-title
          :next-tick next-tick :next-live next-live
          :next-disk next-disk :next-tags next-tags
          :messages messages)))

(ert-deftest org-air-r90-62-cross-file-save-hook-cannot-eat-the-next-part ()
  "A committed part's save hook must never let the next part erase user text.
The command-wide preflight is not permission for a later file: every part must
still be safe at the moment its own bytes would move.  The committed prefix is
honest unavoidable partiality; the record must not cross as complete."
  (skip-unless (locate-library "org-air"))
  (dolist (operation '(undo redo))
    (org-air-r90--with-board
        '(("a.org" . "#+title: a\n\n* TODO A1\n")
          ("b.org" . "#+title: b\n\n* TODO B1\n")
          ("inbox.org" . "#+title: inbox\n"))
      (let* ((facts (org-air-r90--cross-hook-run operation))
             (first-buffer (plist-get facts :first))
             (next-buffer (plist-get facts :next))
             (first-name (plist-get facts :first-name))
             (next-name (plist-get facts :next-name))
             (next-title (plist-get facts :next-title))
             (first-title (plist-get facts :first-title))
             (note (plist-get facts :note))
             (residual (car org-air-view--edit-ring)))
        (ert-info ((format "cross-file %s: ran=%S messages=%S"
                           operation (plist-get facts :ran)
                           (plist-get facts :messages)))
          ;; The hook ran exactly once, after preflight and after the first
          ;; processed file already committed its bytes.
          (should (= 1 (plist-get facts :ran)))
          (should (stringp (plist-get facts :next-live)))
          ;; Honest unavoidable partiality: the first file really moved.
          (should (equal (org-air-r90--text first-name)
                         (org-air-r90--live-text first-name)))
          (should-not (buffer-modified-p first-buffer))
          (should (eq (eq operation 'redo)
                      (org-air-r90--disk-has-tag-p
                       first-name first-title "backlog")))
          ;; ZERO bytes may move in the next part: the user's text stays live,
          ;; disk stays at its pre-user state, and the buffer stays modified.
          (should (string-match-p (regexp-quote note)
                                  (org-air-r90--live-text next-name)))
          (should-not (string-match-p (regexp-quote note)
                                      (org-air-r90--text next-name)))
          (should (equal (plist-get facts :next-live)
                         (org-air-r90--live-text next-name)))
          (should (equal (plist-get facts :next-disk)
                         (org-air-r90--text next-name)))
          (should (= (plist-get facts :next-tick)
                     (with-current-buffer next-buffer
                       (buffer-chars-modified-tick))))
          (should (buffer-modified-p next-buffer))
          ;; No false backlog result: the next part's org-air metadata is
          ;; exactly what it was before the command on disk and live.
          (should (eq (eq operation 'undo)
                      (org-air-r90--disk-has-tag-p
                       next-name next-title "backlog")))
          (should (equal (plist-get facts :next-tags)
                         (org-air-r90--live-tags next-name next-title t)))
          ;; Exactly one honest incomplete message; never a complete claim.
          (should (= 1 (seq-count
                        (lambda (text)
                          (string-match-p
                           (format "\\`%s incomplete: 1/2 files %s; failed %s\\'"
                                   (if (eq operation 'undo) "Undo" "Redo")
                                   (if (eq operation 'undo)
                                       "reverted" "reapplied")
                                   (regexp-quote next-name))
                           text))
                        (plist-get facts :messages))))
          (should-not (seq-some
                       (lambda (text)
                         (string-match-p "\\`\\(Undid\\|Redid\\):" text))
                       (plist-get facts :messages)))
          ;; Undo exposes the blocked current part plus untouched suffix as
          ;; canonical residual; redo exposes only the reapplied prefix.  A
          ;; runtime split never leaves a speculative redo branch.
          (should (= 1 (length org-air-view--edit-ring)))
          (should-not org-air-view--edit-redo-ring)
          (should (eq 'bulk (plist-get residual :kind)))
          (should (string-match-p "residual 1 file" (plist-get residual :desc)))
          (should (= 1 (length (plist-get residual :parts))))
          (should (equal (list (if (eq operation 'undo) next-name first-name))
                         (mapcar (lambda (part)
                                   (file-name-nondirectory
                                    (plist-get part :file)))
                                 (plist-get residual :parts))))
          ;; No wrong undo group was consumed: the user's own group is still
          ;; the newest undoable step in the next source.
          (with-current-buffer next-buffer
            (undo-boundary)
            (undo-only)
            (should-not (string-match-p (regexp-quote note) (buffer-string)))
            (should (equal (plist-get facts :next-disk)
                           (save-restriction
                             (widen)
                             (buffer-substring-no-properties
                              (point-min) (point-max)))))))))))

(ert-deftest org-air-r90-63-narrowed-source-is-compared-as-full-buffer ()
  "Narrowed sources are compared as complete visited buffers everywhere.
The custom undo guard must not invent modified state, the durability
predicate must not false-block, and the user's restriction must survive."
  (skip-unless (locate-library "org-air"))
  ;; Direct guard/predicate invocation on a narrowed but byte-equal source.
  (org-air-r90--with-corpus
      '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha\n* TODO Beta\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let ((source (find-file-noselect (org-air-r90--file "tasks.org"))))
      (with-current-buffer source
        (should-not (buffer-modified-p))
        (goto-char (point-min))
        (re-search-forward "^\\* TODO Alpha")
        (narrow-to-region (line-beginning-position) (line-end-position))
        (let ((beg (point-min))
              (end (point-max)))
          (should (< (- end beg) (buffer-size)))
          (should (equal (org-air-r90--text "tasks.org")
                         (save-restriction
                           (widen)
                           (buffer-substring-no-properties
                            (point-min) (point-max)))))
          ;; Full live bytes equal disk, so both comparison surfaces agree.
          (should (org-air-view--buffer-matches-visited-file-p))
          (org-air-view--undo-disk-truth-guard)
          (should-not (buffer-modified-p))
          (should (= beg (point-min)))
          (should (= end (point-max)))
          ;; A real divergence outside the restriction is still detected.
          (save-restriction
            (widen)
            (save-excursion
              (goto-char (point-max))
              (insert "# divergence outside the user's restriction\n")))
          (should (= beg (point-min)))
          (should (= end (point-max)))
          (should-not (org-air-view--buffer-matches-visited-file-p))
          (restore-buffer-modified-p nil)
          (org-air-view--undo-disk-truth-guard)
          (should (buffer-modified-p))
          (should (= beg (point-min)))
          (should (= end (point-max)))
          (restore-buffer-modified-p nil)))))
  ;; Integrated guarded initial `b'/`t' and compound `u'/`U': the user
  ;; independently resolves the recursive hook group and performs the
  ;; documented ordinary save, all while the source stays narrowed.
  (dolist (direction '(initial-backlog initial-tag undo redo))
    (let ((pending-undo-list nil)
          (undo-equiv-table (make-hash-table :test #'eq))
          (last-command nil)
          (this-command nil))
      (org-air-r90--with-board
          '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha\n* TODO Beta\n")
            ("inbox.org" . "#+title: inbox\n"))
        (let* ((facts (org-air-r90--arm-recursive-compound direction))
               (source (plist-get facts :source))
               (record (plist-get facts :record))
               (part (car (plist-get record :parts)))
               (source-ring (if (eq (plist-get facts :source-ring) 'redo)
                                'org-air-view--edit-redo-ring
                              'org-air-view--edit-ring))
               (other-ring (if (eq (plist-get facts :source-ring) 'redo)
                               'org-air-view--edit-ring
                             'org-air-view--edit-redo-ring))
               (head-before (car (symbol-value source-ring)))
               beg end before-disk)
          (ert-info ((format "narrowed guarded direction: %S" direction))
            (org-air-r90--assert-guard-old-edge source part)
            (with-current-buffer source
              (goto-char (point-min))
              (forward-line 3)
              (narrow-to-region (point-min) (point))
              (undo-boundary)
              (undo-only)
              ;; The guard proves a genuine full-buffer divergence here.
              (should (buffer-modified-p))
              (save-buffer)
              (setq beg (point-min) end (point-max))
              (should (< (- end beg) (buffer-size))))
            (should-not (buffer-modified-p source))
            (should (equal (org-air-r90--text "tasks.org")
                           (org-air-r90--full-live-text source)))
            (should (with-current-buffer source
                      (org-air-view--buffer-matches-visited-file-p)))
            ;; The exact expected identity is still reachable and the record
            ;; is durable: narrowing is not a durability failure.
            (should (consp (org-air-view--history-identity-resolve
                            (plist-get part :expected-undo))))
            (should (org-air-view--history-expected-durable-p part))
            (should-not (org-air-view--history-durability-blocker part))
            (setq before-disk (org-air-r90--text "tasks.org"))
            (org-air-r90--invoke-inverse direction)
            ;; The retry succeeds exactly once instead of false-blocking.
            (should-not (eq head-before (car (symbol-value source-ring))))
            (should (= 0 (length (symbol-value source-ring))))
            (should (= 1 (length (symbol-value other-ring))))
            (should-not (equal before-disk (org-air-r90--text "tasks.org")))
            (should (equal (org-air-r90--text "tasks.org")
                           (org-air-r90--full-live-text source)))
            (should-not (buffer-modified-p source))
            ;; The user's restriction is preserved exactly.
            (should (with-current-buffer source
                      (and (= beg (point-min)) (= end (point-max)))))))))))

(ert-deftest org-air-r90-64-guard-comparison-failure-keeps-recovery-reachable ()
  "An unprovable guard comparison must stay modified and stay recoverable.
The custom undo handler's inability to prove full-file equality may never
produce false clean state, because an ordinary `save-buffer' would then be a
no-op and the documented manual save + retry path becomes unreachable."
  (skip-unless (locate-library "org-air"))
  ;; Distinct, already-green seam: a COMMAND-TIME comparison read error blocks
  ;; with zero writes and recovers on an honest retry.  It is asserted here
  ;; only to keep it separate from the custom-undo-handler seam below.
  (let ((pending-undo-list nil)
        (undo-equiv-table (make-hash-table :test #'eq))
        (last-command nil)
        (this-command nil))
    (org-air-r90--with-board
        '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha\n* TODO Beta\n")
          ("inbox.org" . "#+title: inbox\n"))
      (let* ((facts (org-air-r90--arm-recursive-compound 'undo))
             (source (plist-get facts :source))
             (record (plist-get facts :record))
             (file (org-air-r90--file "tasks.org"))
             (insert-orig (symbol-function 'insert-file-contents))
             (saves 0))
        (with-current-buffer source (undo-boundary) (undo-only) (save-buffer))
        (should-not (buffer-modified-p source))
        (should (equal (org-air-r90--text "tasks.org")
                       (org-air-r90--live-text "tasks.org")))
        (cl-letf (((symbol-function 'insert-file-contents)
                   (lambda (name &rest args)
                     (if (and (stringp name)
                              (equal (expand-file-name name) file))
                         (error "command-time compare failed")
                       (apply insert-orig name args))))
                  ((symbol-function 'save-buffer)
                   (lambda (&rest _)
                     (cl-incf saves)
                     (error "command-time compare error reached save"))))
          (condition-case nil (org-air-edit-redo) ((error quit) nil)))
        (should (= 0 saves))
        (should (eq record (car org-air-view--edit-redo-ring)))
        (org-air-edit-redo)
        (should (eq record (car org-air-view--edit-ring)))
        (should-not org-air-view--edit-redo-ring))))
  ;; The custom undo handler seam: a transient readability/read/compare
  ;; failure during an independent hook-only undo.
  (dolist (shape '(error quit unreadable))
    (let ((pending-undo-list nil)
          (undo-equiv-table (make-hash-table :test #'eq))
          (last-command nil)
          (this-command nil))
      (org-air-r90--with-board
          '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha\n* TODO Beta\n")
            ("inbox.org" . "#+title: inbox\n"))
        (let* ((facts (org-air-r90--arm-recursive-compound 'undo))
               (source (plist-get facts :source))
               (record (plist-get facts :record))
               (part (car (plist-get record :parts)))
               (file (org-air-r90--file "tasks.org"))
               (label "nested committed hook")
               (committed-disk (org-air-r90--text "tasks.org"))
               (insert-orig (symbol-function 'insert-file-contents))
               (readable-orig (symbol-function 'file-readable-p)))
          (ert-info ((format "guard comparison failure shape: %S" shape))
            (org-air-r90--assert-guard-old-edge source part)
            ;; Undo ONLY the recursively committed hook group while the
            ;; guard's readability/read/compare path fails transiently.
            (condition-case nil
                (cl-letf (((symbol-function 'insert-file-contents)
                           (lambda (name &rest args)
                             (if (and (stringp name)
                                      (equal (expand-file-name name) file))
                                 (pcase shape
                                   ('error (error "guard compare failed"))
                                   ('quit (signal 'quit nil))
                                   (_ (apply insert-orig name args)))
                               (apply insert-orig name args))))
                          ((symbol-function 'file-readable-p)
                           (lambda (name &rest args)
                             (if (and (eq shape 'unreadable)
                                      (stringp name)
                                      (equal (expand-file-name name) file))
                                 nil
                               (apply readable-orig name args)))))
                  (with-current-buffer source (undo-boundary) (undo-only)))
              ((error quit) nil))
            ;; Disk and live have really diverged and no byte was written.
            (should (equal committed-disk (org-air-r90--text "tasks.org")))
            (should (string-match-p label (org-air-r90--text "tasks.org")))
            (should-not (string-match-p label
                                        (org-air-r90--live-text "tasks.org")))
            (should-not (equal (org-air-r90--text "tasks.org")
                               (org-air-r90--live-text "tasks.org")))
            ;; Equality could not be proven, so the buffer must stay modified.
            (should (buffer-modified-p source))
            ;; With the failure gone, an ORDINARY save converges full bytes.
            (with-current-buffer source (save-buffer))
            (should (equal (org-air-r90--text "tasks.org")
                           (org-air-r90--live-text "tasks.org")))
            (should-not (string-match-p label (org-air-r90--text "tasks.org")))
            (should-not (buffer-modified-p source))
            ;; The exact org-air retry then succeeds on the same record.
            (should (org-air-view--history-expected-durable-p part))
            (org-air-edit-redo)
            (should (eq record (car org-air-view--edit-ring)))
            (should-not org-air-view--edit-redo-ring)
            (should (equal (org-air-r90--text "tasks.org")
                           (org-air-r90--live-text "tasks.org")))
            (should-not (buffer-modified-p source))
            (should (org-air-r90--disk-has-tag-p
                     "tasks.org" "Alpha" "backlog"))))))))

(defun org-air-r90--base-locator-markers (base)
  "Return BASE's tracked locator markers in order."
  (mapcar #'cdr (buffer-local-value 'org-air-view--source-tracked-locators
                                    base)))

(defun org-air-r90--assert-canonical-base-locators (board base markers)
  "Assert BASE still owns exactly MARKERS for BOARD as live base locators."
  (should (= (length markers) (seq-count #'marker-position markers)))
  (should (= (length markers)
             (seq-count (lambda (marker) (eq (marker-buffer marker) base))
                        markers)))
  (should (eq board (buffer-local-value 'org-air-view--source-locator-owner
                                        base)))
  (should (buffer-local-value 'org-air-view--source-locator-complete base))
  (should (eq (buffer-local-value 'org-air-view--items board)
              (buffer-local-value 'org-air-view--source-locator-generation
                                  base)))
  (should (org-air-r90--same-object-order-p
           markers (org-air-r90--base-locator-markers base)))
  (let ((index (buffer-local-value 'org-air-view--source-locator-index base)))
    (should (hash-table-p index))
    (should (= (length markers) (hash-table-count index)))
    (maphash (lambda (_item marker)
               (should (marker-position marker))
               (should (eq (marker-buffer marker) base)))
             index))
  (let ((roster (buffer-local-value 'org-air-view--source-tracked-buffers
                                    board)))
    (should (= 1 (seq-count (lambda (buffer) (eq buffer base)) roster)))))

(ert-deftest org-air-r90-65-indirect-clone-never-owns-canonical-locators ()
  "A cloned indirect source must not steal or tear down base ownership.
Killing or re-moding a clone created with copied locals/hooks may only clear
clone-local aliases; the canonical base keeps exactly one usable locator set,
and `complete' state may never refer to dead or wrong-buffer markers."
  (skip-unless (locate-library "org-air"))
  (dolist (phase '(before-history after-history))
    (dolist (disposal '(kill mode))
      (org-air-r90--with-board
          '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha\n* TODO Middle\n* TODO Later\n")
            ("inbox.org" . "#+title: inbox\n"))
        (let* ((board (current-buffer))
               (base (find-file-noselect (org-air-r90--file "tasks.org")))
               (dehydrate-orig
                (symbol-function 'org-air-view--dehydrate-source-markers))
               (dehydrations 0)
               (clone nil)
               markers)
          (ert-info ((format "indirect phase=%S disposal=%S" phase disposal))
            (when (eq phase 'after-history)
              (org-air-r90--mark-title "Alpha")
              (org-air-item-backlog))
            (setq markers (org-air-r90--base-locator-markers base))
            (should (= 3 (length markers)))
            (org-air-r90--assert-canonical-base-locators board base markers)
            (unwind-protect
                (progn
                  ;; A normal cloned + narrowed indirect buffer: CLONE copies
                  ;; buffer-local variables and hooks, including org-air's.
                  (setq clone (make-indirect-buffer
                               base "org-air-r90-indirect" t))
                  (with-current-buffer clone
                    (goto-char (point-min))
                    (re-search-forward "^\\* TODO Middle")
                    (narrow-to-region (line-beginning-position) (point-max)))
                  ;; The owner roster holds the base exactly once, never the
                  ;; clone, before and after the clone is disposed of.
                  (should-not (memq clone (buffer-local-value
                                           'org-air-view--source-tracked-buffers
                                           board)))
                  (org-air-r90--assert-canonical-base-locators
                   board base markers)
                  (when (eq disposal 'mode)
                    (with-current-buffer clone (fundamental-mode))
                    (org-air-r90--assert-canonical-base-locators
                     board base markers))
                  (let ((kill-buffer-query-functions nil))
                    (kill-buffer clone))
                  (setq clone nil)
                  (org-air-r90--assert-canonical-base-locators
                   board base markers)
                  (should-not (memq clone (buffer-local-value
                                           'org-air-view--source-tracked-buffers
                                           board)))
                  ;; Same-generation hydration is an O(1) no-op and must never
                  ;; bless dead or wrong-buffer markers.
                  (with-current-buffer board
                    (org-air-view--hydrate-source-items base))
                  (org-air-r90--assert-canonical-base-locators
                   board base markers)
                  ;; Unsaved drift before a later heading still resolves
                  ;; through the exact marker, with no scan/query fallback.
                  (let* ((later (org-air-r90--item "Later"))
                         (durable (cdr (org-air-view--item-source-key later)))
                         (tracked
                          (cdr (assq later
                                     (buffer-local-value
                                      'org-air-view--source-tracked-locators
                                      base))))
                         (queries 0)
                         (query-orig (symbol-function 'org-air-query-items)))
                    (should (markerp tracked))
                    (should (= durable (marker-position tracked)))
                    (with-current-buffer base
                      (goto-char (point-min))
                      (re-search-forward "^\\* TODO Middle")
                      (beginning-of-line)
                      (insert "# unsaved user drift above the third heading\n"))
                    (should (= durable
                               (cdr (org-air-view--item-source-key later))))
                    (should-not (= durable
                                   (org-air-r90--actual-heading-position
                                    "tasks.org" "Later")))
                    (should (= (marker-position tracked)
                               (org-air-r90--actual-heading-position
                                "tasks.org" "Later")))
                    (org-air-r90--mark-title "Later")
                    (cl-letf (((symbol-function 'org-air-query-items)
                               (lambda (&rest args)
                                 (cl-incf queries)
                                 (apply query-orig args))))
                      (org-air-item-backlog))
                    (should (= 0 queries))
                    (should-not org-air-view--marked-keys)
                    (should (org-air-r90--disk-has-tag-p
                             "tasks.org" "Later" "backlog"))
                    (should-not (org-air-r90--disk-has-tag-p
                                 "tasks.org" "Middle" "backlog"))
                    (should (string-match-p "unsaved user drift"
                                            (org-air-r90--text "tasks.org")))
                    (should (equal (org-air-r90--text "tasks.org")
                                   (org-air-r90--full-live-text base)))
                    (should (equal (org-air-r90--text "tasks.org")
                                   (org-air-r90--live-text "tasks.org"))))
                  ;; A base kill still performs real teardown exactly once.
                  (setq markers (org-air-r90--base-locator-markers base))
                  (cl-letf (((symbol-function
                              'org-air-view--dehydrate-source-markers)
                             (lambda (&rest args)
                               (cl-incf dehydrations)
                               (apply dehydrate-orig args))))
                    (let ((kill-buffer-query-functions nil))
                      (with-current-buffer base (set-buffer-modified-p nil))
                      (kill-buffer base)))
                  (should (= 1 dehydrations))
                  (should (= 0 (seq-count #'marker-position markers)))
                  (should-not (memq base (buffer-local-value
                                          'org-air-view--source-tracked-buffers
                                          board))))
              (when (buffer-live-p clone)
                (let ((kill-buffer-query-functions nil))
                  (kill-buffer clone))))))))))

;;;; r90-66 — review-5 permanent root: an incomplete compound REDO must
;;;; report the true number of files it wrote to disk.

(defun org-air-r90--four-file-redo-order ()
  "Commit a four-file bulk, undo it, and return its redo processing order."
  (dolist (title '("A1" "B1" "C1" "D1")) (org-air-r90--mark-title title))
  (org-air-item-backlog)
  (org-air-edit-undo)
  (mapcar (lambda (part)
            (file-name-nondirectory (plist-get part :file)))
          (plist-get (car org-air-view--edit-redo-ring) :parts)))

(ert-deftest org-air-r90-66-incomplete-redo-counts-every-committed-file ()
  "An incomplete compound redo may never understate its committed prefix.
The partial-failure law is that the already-committed prefix stays committed
and the echo is the HONEST `K/N files reapplied'.  Both compound redo stop
shapes — a blocked/failed later part and a cache-generation rebuild — must
report the same K that the residual record and the disk agree on, because K
is the user's only statement of how many files org-air just rewrote."
  (skip-unless (locate-library "org-air"))
  (dolist (shape '(blocked invalidated))
    (org-air-r90--with-board
        '(("a.org" . "#+title: a\n\n* TODO A1\n")
          ("b.org" . "#+title: b\n\n* TODO B1\n")
          ("c.org" . "#+title: c\n\n* TODO C1\n")
          ("d.org" . "#+title: d\n\n* TODO D1\n")
          ("inbox.org" . "#+title: inbox\n"))
      (let* ((order (org-air-r90--four-file-redo-order))
             (first-buffer (find-file-noselect
                            (org-air-r90--file (nth 0 order))))
             (last-buffer (find-file-noselect
                           (org-air-r90--file (nth 3 order))))
             (write-orig
              (symbol-function 'org-air-view--cache-sync-write-slots))
             (ran 0)
             (hook (lambda ()
                     (cl-incf ran)
                     (when (= ran 1)
                       (with-current-buffer last-buffer
                         (undo-boundary)
                         (save-excursion
                           (goto-char (point-max))
                           (insert "# late unsaved user note\n")))))))
        (ert-info ((format "incomplete redo shape=%S order=%S" shape order))
          (should (equal '("a.org" "b.org" "c.org" "d.org") order))
          (when (eq shape 'blocked)
            (with-current-buffer first-buffer
              (add-hook 'after-save-hook hook nil t)))
          (org-air-r90--record-messages collected
            (unwind-protect
                (cl-letf (((symbol-function
                            'org-air-view--cache-sync-write-slots)
                           (lambda (item file position tags)
                             (if (and (eq shape 'invalidated)
                                      (equal "C1" (org-air-item-title item)))
                                 (error "mandatory C1 slot failure")
                               (funcall write-orig item file position tags))))
                          ((symbol-function 'display-warning)
                           (lambda (&rest _) nil)))
                  (org-air-edit-redo))
              (when (eq shape 'blocked)
                (with-current-buffer first-buffer
                  (remove-hook 'after-save-hook hook t))))
            (let ((messages (nreverse collected)))
              ;; Exactly three files really were rewritten; the fourth is not.
              (dolist (name '("a.org" "b.org" "c.org"))
                (should (org-air-r90--disk-has-tag-p
                         name (concat (upcase (substring name 0 1)) "1")
                         "backlog")))
              (should-not (org-air-r90--disk-has-tag-p "d.org" "D1" "backlog"))
              ;; The residual record already agrees that three files moved.
              (let ((residual (car org-air-view--edit-ring)))
                (should (= 3 (length (plist-get residual :parts))))
                (should (string-match-p "residual 3 files"
                                        (plist-get residual :desc))))
              ;; The echo is the user's ONLY statement of that fact and must
              ;; never understate it.
              (ert-info ((format "incomplete messages: %S" messages))
                (should (= 1 (seq-count
                              (lambda (text)
                                (string-match-p
                                 "\\`Redo incomplete: 3/4 files reapplied"
                                 text))
                              messages)))))))))))

;;;; r90-67 — retest-11 permanent root: an incomplete compound operation must
;;;; restamp EVERY buffer it really committed (R75 Decision 5, two-sided).

(defconst org-air-r90--restamp-board
  '(("a.org" . "#+title: a\n\n* TODO A1\n* TODO A2\n")
    ("b.org" . "#+title: b\n\n* TODO B1\n* TODO B2\n")
    ("c.org" . "#+title: c\n\n* TODO C1\n* TODO C2\n")
    ("d.org" . "#+title: d\n\n* TODO D1\n* TODO D2\n")
    ("inbox.org" . "#+title: inbox\n"))
  "Four two-heading source files plus an inbox for compound restamp tests.")

(defun org-air-r90--tick-guarded-records (buffer)
  "Return every tick-guarded ring entry naming BUFFER, both ring sides.
`:expected-undo' entries are exact opaque-token records that the restamp law
deliberately never touches, so they are excluded."
  (let (out)
    (dolist (rec (append org-air-view--edit-ring org-air-view--edit-redo-ring))
      (if (eq (plist-get rec :kind) 'bulk)
          (dolist (part (plist-get rec :parts))
            (when (and (eq (plist-get part :buffer) buffer)
                       (not (plist-member part :expected-undo)))
              (push part out)))
        (when (and (eq (plist-get rec :buffer) buffer)
                   (not (plist-member rec :expected-undo)))
          (push rec out))))
    (nreverse out)))

(defun org-air-r90--stale-stamped-records (buffer)
  "Return a readable description of BUFFER's ring entries with a stale tick."
  (let ((tick (buffer-chars-modified-tick buffer)))
    (delq nil
          (mapcar (lambda (entry)
                    (unless (eql (plist-get entry :tick) tick)
                      (list :desc (plist-get entry :desc)
                            :file (file-name-nondirectory
                                   (or (plist-get entry :file) "?"))
                            :stamped (plist-get entry :tick) :now tick)))
                  (org-air-r90--tick-guarded-records buffer)))))

(defconst org-air-r90--changed-since-re "\\`Cannot undo: .* changed since"
  "Echo shape of the R73/R75 stale-record degrade a bad stamp would cause.")

(defun org-air-r90--count-messages (messages regexp)
  "Return how many MESSAGES match REGEXP."
  (seq-count (lambda (text) (string-match-p regexp text)) messages))

(defun org-air-r90--single-record-for (buffer)
  "Return the ordinary single-buffer undo record naming BUFFER."
  (let ((found (seq-find (lambda (rec)
                           (and (not (eq (plist-get rec :kind) 'bulk))
                                (eq (plist-get rec :buffer) buffer)))
                         (append org-air-view--edit-ring
                                 org-air-view--edit-redo-ring))))
    (should found)
    found))

(defun org-air-r90--expand-section (bucket)
  "Expand BUCKET through the real board TAB toggle so every row renders."
  (let ((pos (org-air-view--find-property 'org-air-section bucket)))
    (should pos)
    (goto-char pos)
    (org-air-toggle-section)
    (should (memq bucket org-air-view--expanded-sections))))

(defun org-air-r90--restamp-setup ()
  "Create four ordinary records, then one committed four-file bulk record.
Return the compound record.  The ordinary `done' records are pushed newest
LAST for `a.org', so a later `u' walk reaches every source buffer in turn."
  (org-air-r90--expand-section 'attention)
  (dolist (title '("D2" "C2" "B2" "A2"))
    (org-air-r90--goto-row title)
    (org-air-item-done))
  (dolist (title '("A1" "B1" "C1" "D1")) (org-air-r90--mark-title title))
  (org-air-item-backlog)
  (let ((record (car org-air-view--edit-ring)))
    (should (eq 'bulk (plist-get record :kind)))
    (should (equal '("a.org" "b.org" "c.org" "d.org")
                   (mapcar (lambda (part)
                             (file-name-nondirectory (plist-get part :file)))
                           (plist-get record :parts))))
    record))

(ert-deftest org-air-r90-67-incomplete-compound-restamps-every-committed-buffer ()
  "Every buffer an incomplete compound op really committed must be restamped.
R75 Decision 5 (two-sided restamp) is what keeps the chars-tick guard meaning
\"no NON-ring change intervened\": after a ring op writes a buffer, that
buffer's OTHER ring records must carry the new tick or the next honest `u' is
refused with a truthful-looking but wrong \"changed since\" and the record is
consumed with zero bytes moved.  An incomplete compound operation commits a
real PREFIX of files, so the sweep must visit exactly the committed set —
every one of them, and none of the parts that never moved.  A destructive
`nreverse' on the newest-first accumulator leaves the sweep naming a
one-element tail, restamping only the LAST committed buffer while the echo
may still be honest, which is why this law needs its own permanent test."
  (skip-unless (locate-library "org-air"))
  (dolist (combo '((undo blocked) (undo invalidated)
                   (redo blocked) (redo invalidated)))
    (let ((direction (nth 0 combo))
          (shape (nth 1 combo))
          (pending-undo-list nil)
          (undo-equiv-table (make-hash-table :test #'eq))
          (last-command nil)
          (this-command nil))
      (org-air-r90--with-board org-air-r90--restamp-board
        (let* ((record (org-air-r90--restamp-setup))
               (parts (copy-sequence (plist-get record :parts)))
               (buffers (mapcar (lambda (name)
                                  (find-file-noselect (org-air-r90--file name)))
                                '("a.org" "b.org" "c.org" "d.org")))
               ;; Processing order is reverse commit order for undo and
               ;; commit order for redo, so the same shapes always stop
               ;; after exactly three committed files.
               (order (if (eq direction 'undo)
                          '("d.org" "c.org" "b.org" "a.org")
                        '("a.org" "b.org" "c.org" "d.org")))
               (committed-names (seq-take order 3))
               (untouched-name (nth 3 order))
               ;; The invalidated shape stops ON the third processed file
               ;; (it commits, then final disk truth replaces the
               ;; generation); the blocked shape stops BEFORE the fourth.
               (invalidate-title (concat (upcase (substring (nth 2 order) 0 1))
                                         "1"))
               (first-buffer (find-file-noselect
                              (org-air-r90--file (nth 0 order))))
               (last-buffer (find-file-noselect
                             (org-air-r90--file (nth 3 order))))
               (write-orig
                (symbol-function 'org-air-view--cache-sync-write-slots))
               (ran 0)
               (hook (lambda ()
                       (cl-incf ran)
                       (when (= ran 1)
                         (with-current-buffer last-buffer
                           (undo-boundary)
                           (save-excursion
                             (goto-char (point-max))
                             (insert "# late unsaved user note\n"))))))
               single-records before-ticks)
          (ert-info ((format "incomplete %S shape=%S order=%S" direction shape
                             order))
            ;; A redo case needs the record on the redo ring first: one
            ;; COMPLETE undo, which legitimately restamps all four buffers.
            (when (eq direction 'redo)
              (org-air-edit-undo)
              (should (eq record (car org-air-view--edit-redo-ring)))
              (should-not (eq record (car org-air-view--edit-ring))))
            (setq single-records
                  (mapcar #'org-air-r90--single-record-for buffers))
            (setq before-ticks
                  (mapcar (lambda (rec) (plist-get rec :tick)) single-records))
            ;; Before the incomplete operation every ordinary record is
            ;; correctly stamped: the initial bulk push does not restamp, so
            ;; only a real ring op can have healed them.
            (when (eq direction 'redo)
              (dolist (buffer buffers)
                (should (null (org-air-r90--stale-stamped-records buffer)))))
            (when (eq shape 'blocked)
              (with-current-buffer first-buffer
                (add-hook 'after-save-hook hook nil t)))
            (org-air-r90--record-messages collected
              (unwind-protect
                  (cl-letf (((symbol-function
                              'org-air-view--cache-sync-write-slots)
                             (lambda (item file position tags)
                               (if (and (eq shape 'invalidated)
                                        (equal invalidate-title
                                               (org-air-item-title item)))
                                   (error "mandatory %s slot failure"
                                          invalidate-title)
                                 (funcall write-orig item file position tags))))
                            ((symbol-function 'display-warning)
                             (lambda (&rest _) nil)))
                    (if (eq direction 'undo)
                        (org-air-edit-undo)
                      (org-air-edit-redo)))
                (when (eq shape 'blocked)
                  (with-current-buffer first-buffer
                    (remove-hook 'after-save-hook hook t))))
              (let ((messages (nreverse collected)))
                ;; 1. Disk agrees that exactly three files moved.
                (dolist (name '("a.org" "b.org" "c.org" "d.org"))
                  (let ((title (concat (upcase (substring name 0 1)) "1"))
                        (moved (member name committed-names)))
                    (should (eq (and (org-air-r90--disk-has-tag-p
                                      name title "backlog")
                                     t)
                                ;; undo removes the tag from what it moved,
                                ;; redo adds it to what it moved.
                                (if (eq direction 'undo)
                                    (not (and moved t))
                                  (and moved t))))))
                ;; 2. The echo states that same K/N, never a smaller K.
                (ert-info ((format "incomplete messages: %S" messages))
                  (should (= 1 (seq-count
                                (lambda (text)
                                  (string-match-p
                                   (format
                                    "\\`%s incomplete: 3/4 files %s"
                                    (if (eq direction 'undo) "Undo" "Redo")
                                    (if (eq direction 'undo)
                                        "reverted" "reapplied"))
                                   text))
                                messages))))))
            ;; 3. The residual record carries the ORIGINAL part objects, in
            ;;    order, and no speculative redo branch survives.
            (should-not org-air-view--edit-redo-ring)
            (let* ((residual (car org-air-view--edit-ring))
                   (expected (if (eq direction 'undo)
                                 ;; still-undoable: the stopped part and any
                                 ;; untouched suffix, in commit order.
                                 (seq-filter
                                  (lambda (part)
                                    (not (member (file-name-nondirectory
                                                  (plist-get part :file))
                                                 committed-names)))
                                  parts)
                               ;; reapplied prefix alone, in commit order.
                               (seq-filter
                                (lambda (part)
                                  (member (file-name-nondirectory
                                           (plist-get part :file))
                                          committed-names))
                                parts))))
              (should (eq 'bulk (plist-get residual :kind)))
              (should (org-air-r90--same-object-order-p
                       (plist-get residual :parts) expected))
              (should (string-match-p
                       (format "residual %d file" (length expected))
                       (plist-get residual :desc))))
            ;; 4. THE RESTAMP LAW.  Every committed buffer is current on both
            ;;    ring sides; the file that never moved is left exactly as it
            ;;    was, so the sweep is the committed set and nothing wider.
            (cl-loop
             for buffer in buffers
             for rec in single-records
             for before in before-ticks
             for name in '("a.org" "b.org" "c.org" "d.org")
             do (if (member name committed-names)
                    (let ((stale (org-air-r90--stale-stamped-records buffer)))
                      (ert-info ((format "committed %s stale=%S" name stale))
                        (should (null stale))
                        (should (eql (plist-get rec :tick)
                                     (buffer-chars-modified-tick buffer)))))
                  (ert-info ((format "untouched %s" name))
                    (should (equal name untouched-name))
                    (should (eql (plist-get rec :tick) before)))))
            ;; 5. FOLLOW-ON HISTORY.  A stale stamp would refuse the next `u'
            ;;    on a committed buffer's own ordinary record with "changed
            ;;    since" and consume it, moving zero bytes.
            (let ((residual (car org-air-view--edit-ring)))
              (org-air-r90--record-messages drained
                (org-air-edit-undo)
                (let ((messages (nreverse drained)))
                  (ert-info ((format "residual drain messages: %S" messages))
                    (if (and (eq direction 'undo) (eq shape 'blocked))
                        ;; The stopped part carries the user's own ahead edit,
                        ;; so its residual stays honestly retryable at the head.
                        (progn
                          (should (= 1 (org-air-r90--count-messages
                                        messages
                                        org-air-r90--changed-since-re)))
                          (should (eq residual (car org-air-view--edit-ring))))
                      (should (= 1 (org-air-r90--count-messages
                                    messages "\\`Undid: ")))
                      (should-not (eq residual
                                      (car org-air-view--edit-ring))))))))
            (unless (and (eq direction 'undo) (eq shape 'blocked))
              ;; Walk every ordinary record back.  Only the file carrying a
              ;; genuine unsaved user edit may be refused.
              (cl-loop for name in '("a.org" "b.org" "c.org" "d.org")
                       for rec in single-records
                       for title = (concat (upcase (substring name 0 1)) "2")
                       do (org-air-r90--record-messages walked
                            (org-air-edit-undo)
                            (let ((messages (nreverse walked))
                                  (dirty (and (eq shape 'blocked)
                                              (equal name untouched-name))))
                              (ert-info ((format "walk %s (%s): %S"
                                                 name (plist-get rec :desc)
                                                 messages))
                                (if dirty
                                    (should
                                     (= 1 (org-air-r90--count-messages
                                           messages
                                           org-air-r90--changed-since-re)))
                                  (should (= 1 (org-air-r90--count-messages
                                                messages "\\`Undid: ")))
                                  (should
                                   (string-match-p
                                    (format "^\\*+ TODO %s" title)
                                    (org-air-r90--text name)))))))))))))))


;;;; r90-68 — retest-11 permanent root: the undo disk-truth guard resolves the
;;;; CANONICAL visited buffer and is a strict no-op with no visited file.

(defmacro org-air-r90--with-guard-clone (spec &rest body)
  "Run BODY with BASE visiting a temp file holding SPEC and CLONE cloned off it.
CLONE is an `org-tree-to-indirect-buffer'-style clone: `make-indirect-buffer'
with CLONE-FLAG, sharing BASE's text and `buffer-undo-list' and visiting no
file of its own.  Both buffers and the directory are always cleaned up."
  (declare (indent 1) (debug t))
  `(let* ((dir (make-temp-file "org-air-r90-guard" t))
          (file (expand-file-name "tree.org" dir))
          base clone)
     (unwind-protect
         (progn
           (with-temp-file file (insert ,spec))
           (setq base (find-file-noselect file))
           (setq clone (make-indirect-buffer base "*org-air-r90-clone*" t))
           ,@body)
       (dolist (buffer (list clone base))
         (when (buffer-live-p buffer)
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (let ((kill-buffer-query-functions nil)) (kill-buffer buffer))))
       (when (file-directory-p dir)
         (set-file-modes dir #o700)
         (delete-directory dir t)))))

(defun org-air-r90--guard-is-total (buffer)
  "Run the guard in BUFFER, asserting it never signals and adds no undo entry.
The guard is a custom undo handler: it may never signal out of the undo
machinery and may never extend the buffer's own undo list."
  (with-current-buffer buffer
    (let ((before buffer-undo-list)
          (guards (org-air-r90--undo-disk-guard-count buffer-undo-list))
          (signalled 'none))
      (condition-case err
          (org-air-view--undo-disk-truth-guard)
        ((error quit) (setq signalled err)))
      (should (eq 'none signalled))
      (should (eq before buffer-undo-list))
      (should (= guards (org-air-r90--undo-disk-guard-count
                         buffer-undo-list))))))

(ert-deftest org-air-r90-68-undo-guard-is-canonical-and-file-less-safe ()
  "The disk-truth guard must resolve its subject and never invent disk truth.
An `org-tree-to-indirect-buffer' clone shares its base's text and
`buffer-undo-list' but visits no file, so an ordinary `C-/' inside the clone
runs this guard with the CLONE current.  Two independent halves are load
bearing.  (1) CANONICALIZATION: the comparison subject is the visited BASE, so
unprovable equality still restores the base's modified state and the
documented manual save + retry stays reachable; reading `buffer-file-name'
from the clone would prove nothing and silently leave a divergent base clean.
(2) THE FILE-LESS NO-OP: after that resolution a buffer with genuinely no
visited file has no disk truth to be dishonest about, so the guard must not
manufacture modified state and a spurious dirty-buffer prompt."
  (skip-unless (locate-library "org-air"))
  ;; A. The REAL product path: the product installs the guard, and the undo
  ;;    that runs it happens inside the clone.
  (let ((pending-undo-list nil)
        (undo-equiv-table (make-hash-table :test #'eq))
        (last-command nil)
        (this-command nil))
    (org-air-r90--with-board
        '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha\n* TODO Beta\n")
          ("inbox.org" . "#+title: inbox\n"))
      (let* ((facts (org-air-r90--arm-recursive-compound 'undo))
             (source (plist-get facts :source))
             (record (plist-get facts :record))
             (part (car (plist-get record :parts)))
             (label "nested committed hook")
             (committed-disk (org-air-r90--text "tasks.org"))
             clone)
        (unwind-protect
            (progn
              (org-air-r90--assert-guard-old-edge source part)
              (setq clone (make-indirect-buffer source "*org-air-r90-tree*" t))
              (should-not (buffer-local-value 'buffer-file-name clone))
              (should (eq source (buffer-base-buffer clone)))
              (should (eq source (org-air-view--source-canonical-buffer clone)))
              ;; Ordinary `C-/' inside the clone: it walks the SHARED undo
              ;; list, so the product's own guard entry runs with the clone
              ;; current and no file of its own.
              (should (= 1 (org-air-r90--undo-disk-guard-count
                            (buffer-local-value 'buffer-undo-list clone))))
              (with-current-buffer clone (undo-boundary) (undo-only))
              ;; Live and disk really diverged; equality cannot be proven, so
              ;; the CANONICAL BASE must carry the modified state.
              (should (equal committed-disk (org-air-r90--text "tasks.org")))
              (should-not (string-match-p
                           label (org-air-r90--live-text "tasks.org")))
              (should (buffer-modified-p source))
              ;; The documented recovery stays reachable from the base.
              (with-current-buffer source (save-buffer))
              (should-not (buffer-modified-p source))
              (should (equal (org-air-r90--text "tasks.org")
                             (org-air-r90--live-text "tasks.org")))
              (should (org-air-view--history-expected-durable-p part))
              (org-air-edit-redo)
              (should (eq record (car org-air-view--edit-ring)))
              (should (org-air-r90--disk-has-tag-p
                       "tasks.org" "Alpha" "backlog")))
          (when (buffer-live-p clone)
            (let ((kill-buffer-query-functions nil)) (kill-buffer clone)))))))
  ;; B. A clone whose base genuinely equals its file stays clean, and the
  ;;    user's restriction, point and mark survive the comparison.
  (org-air-r90--with-guard-clone
      "#+title: tree\n\n* TODO Alpha\n** child\n* TODO Beta\n"
    (should-not (buffer-modified-p base))
    ;; Both the clone AND the canonical base are narrowed like a real tree
    ;; clone; the comparison widens neither of the user's restrictions.
    (with-current-buffer base
      (narrow-to-region (point-min) (min (point-max) 18))
      (goto-char (point-max))
      (push-mark (point-min) t))
    (with-current-buffer clone
      (narrow-to-region (point-min) (min (point-max) 24))
      (goto-char (point-min))
      (push-mark (point-max) t)
      (let ((low (point-min)) (high (point-max))
            (pt (point)) (mk (mark t))
            (base-state (with-current-buffer base
                          (list (point-min) (point-max) (point) (mark t)))))
        (org-air-r90--guard-is-total clone)
        (should (= low (point-min)))
        (should (= high (point-max)))
        (should (= pt (point)))
        (should (= mk (mark t)))
        (should (equal base-state
                       (with-current-buffer base
                         (list (point-min) (point-max) (point) (mark t)))))))
    (with-current-buffer base (widen))
    (should-not (buffer-modified-p base))
    (should-not (buffer-modified-p clone))
    ;; A clone-of-clone still resolves to the ULTIMATE base, both ways.
    (let ((deep (make-indirect-buffer clone "*org-air-r90-clone-2*" t)))
      (unwind-protect
          (progn
            (should (eq base (buffer-base-buffer deep)))
            (should (eq base (org-air-view--source-canonical-buffer deep)))
            (should-not (buffer-local-value 'buffer-file-name deep))
            (org-air-r90--guard-is-total deep)
            (should-not (buffer-modified-p base))
            ;; Divergent base, guard run from the clone-of-clone: the
            ;; ultimate base is the subject and must end up modified.
            (with-current-buffer base
              (save-excursion (goto-char (point-max)) (insert "* TODO Gamma\n"))
              (restore-buffer-modified-p nil))
            (should-not (buffer-modified-p base))
            (org-air-r90--guard-is-total deep)
            (should (buffer-modified-p base))
            ;; An ordinary save converges and the guard is quiet again.
            (with-current-buffer base (save-buffer))
            (should-not (buffer-modified-p base))
            (org-air-r90--guard-is-total deep)
            (should-not (buffer-modified-p base)))
        (when (buffer-live-p deep)
          (let ((kill-buffer-query-functions nil)) (kill-buffer deep))))))
  ;; C. Genuinely file-less subjects are a STRICT no-op, cloned or not.
  (with-temp-buffer
    (insert "scratch only, never visited\n")
    (restore-buffer-modified-p nil)
    (should-not buffer-file-name)
    (should (eq (current-buffer)
                (org-air-view--source-canonical-buffer (current-buffer))))
    (org-air-r90--guard-is-total (current-buffer))
    (should-not (buffer-modified-p)))
  (let* ((orphan (generate-new-buffer "*org-air-r90-orphan*"))
         (orphan-clone (make-indirect-buffer orphan
                                             "*org-air-r90-orphan-clone*" t)))
    (unwind-protect
        (progn
          (with-current-buffer orphan
            (insert "* TODO never visited\n")
            (restore-buffer-modified-p nil))
          (should (eq orphan (org-air-view--source-canonical-buffer
                              orphan-clone)))
          (org-air-r90--guard-is-total orphan-clone)
          (should-not (buffer-modified-p orphan))
          (should-not (buffer-modified-p orphan-clone)))
      (dolist (buffer (list orphan-clone orphan))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (let ((kill-buffer-query-functions nil)) (kill-buffer buffer))))))
  ;; D. Once a visited file DOES resolve, every unprovable outcome is
  ;;    conservative and the guard stays total.
  (dolist (shape '(divergent unreadable deleted read-only read-error
                   quit restore-signals))
    (org-air-r90--with-guard-clone
        "#+title: tree\n\n* TODO Alpha\n* TODO Beta\n"
      (let ((insert-orig (symbol-function 'insert-file-contents))
            (restore-orig (symbol-function 'restore-buffer-modified-p)))
        (ert-info ((format "guard subject shape: %S" shape))
          (with-current-buffer base
            (save-excursion (goto-char (point-max)) (insert "* TODO Gamma\n"))
            (restore-buffer-modified-p nil)
            (when (eq shape 'read-only) (setq buffer-read-only t)))
          (should-not (buffer-modified-p base))
          (pcase shape
            ('unreadable (set-file-modes file #o000))
            ('deleted (delete-file file))
            (_ nil))
          (cl-letf (((symbol-function 'insert-file-contents)
                     (lambda (name &rest args)
                       (if (and (stringp name)
                                (equal (expand-file-name name) file))
                           (pcase shape
                             ('read-error (error "guard compare failed"))
                             ('quit (signal 'quit nil))
                             (_ (apply insert-orig name args)))
                         (apply insert-orig name args))))
                    ((symbol-function 'restore-buffer-modified-p)
                     (lambda (flag)
                       (if (eq shape 'restore-signals)
                           (error "guard cannot restore modified state")
                         (funcall restore-orig flag)))))
            ;; The guard is invoked from the FILE-LESS clone every time.
            (org-air-r90--guard-is-total clone))
          (if (eq shape 'restore-signals)
              ;; Total even when the restoration itself fails: no signal
              ;; escapes into the undo machinery.
              (should-not (buffer-modified-p base))
            (should (buffer-modified-p base)))
          (when (eq shape 'read-only)
            (should (buffer-local-value 'buffer-read-only base)))
          (when (eq shape 'unreadable)
            (set-file-modes file #o600)))))))

;;;; r90-69/70/71 — review-5 permanent root: a committed-buffer restamp may
;;;; only ever record that buffer's OWN authoritative post-commit state.

(defconst org-air-r90--bless-board
  '(("a.org" . "#+title: a\n\n* TODO A1\n* TODO A2\n* TODO A3\n")
    ("b.org" . "#+title: b\n\n* TODO B1\n* TODO B2\n* TODO B3\n")
    ("c.org" . "#+title: c\n\n* TODO C1\n* TODO C2\n* TODO C3\n")
    ("inbox.org" . "#+title: inbox\n"))
  "Three three-heading sources plus an inbox for the restamp blessing law.")

(defconst org-air-r90--bless-note "# UNRELATED UNSAVED USER NOTE\n"
  "Unsaved user text a committed save hook leaves in an ALREADY WRITTEN file.")

(defconst org-air-r90--bless-claim-re "\\`\\(Undid\\|Redid\\): "
  "Echo shape of a ring step org-air claims it completed.")

(defconst org-air-r90--bless-refusal-re "\\`Cannot \\(undo\\|redo\\): "
  "Echo shape of an honest ring refusal that moves zero bytes.")

(defun org-air-r90--bless-note-live-p (buffer)
  "Return non-nil when the user's unsaved note is live in BUFFER."
  (and (string-match-p (regexp-quote org-air-r90--bless-note)
                       (org-air-r90--full-live-text buffer))
       t))

(defun org-air-r90--bless-note-disk-p (name)
  "Return non-nil when the user's note reached corpus file NAME on disk."
  (and (string-match-p (regexp-quote org-air-r90--bless-note)
                       (org-air-r90--text name))
       t))

(defun org-air-r90--bless-ring-entries (buffer)
  "Return every ring entry naming BUFFER, exact-armed ones included.
Unlike `org-air-r90--tick-guarded-records' this keeps `:expected-undo'
entries: while a foreign unsaved change sits ahead in a committed buffer, NO
record for it may be safe, however it is guarded."
  (let (out)
    (dolist (rec (append org-air-view--edit-ring org-air-view--edit-redo-ring))
      (if (eq (plist-get rec :kind) 'bulk)
          (dolist (part (plist-get rec :parts))
            (when (eq (plist-get part :buffer) buffer) (push part out)))
        (when (eq (plist-get rec :buffer) buffer) (push rec out))))
    (nreverse out)))

(defun org-air-r90--find-record-by-desc (desc)
  "Return the single ring record described exactly by DESC."
  (let ((found (seq-find (lambda (record)
                           (equal desc (plist-get record :desc)))
                         (append org-air-view--edit-ring
                                 org-air-view--edit-redo-ring))))
    (should found)
    found))

(defun org-air-r90--bless-names (direction)
  "Return (VICTIM HOST THIRD) corpus names in compound DIRECTION order.
Compound undo processes files in reverse commit order and redo in commit
order.  VICTIM is the FIRST processed file — already written and committed by
the time HOST, the second, runs its own `after-save-hook'."
  (if (eq direction 'undo)
      '("c.org" "b.org" "a.org")
    '("a.org" "b.org" "c.org")))

(defun org-air-r90--bless-title (name suffix)
  "Return corpus NAME's heading title numbered SUFFIX."
  (format "%s%d" (upcase (substring name 0 1)) suffix))

(defun org-air-r90--bless-setup (direction)
  "Arm one three-file compound record plus one ordinary record per source.
Leaves the compound at the head of the ring DIRECTION pops from.  Return it."
  (org-air-r90--expand-section 'attention)
  (dolist (title '("A2" "B2" "C2"))
    (org-air-r90--goto-row title)
    (org-air-item-done))
  (dolist (title '("A1" "B1" "C1")) (org-air-r90--mark-title title))
  (org-air-item-backlog)
  ;; A redo case needs the record on the redo ring first: one COMPLETE undo,
  ;; which legitimately restamps all three buffers.
  (when (eq direction 'redo) (org-air-edit-undo))
  (let ((record (car (if (eq direction 'undo)
                         org-air-view--edit-ring
                       org-air-view--edit-redo-ring))))
    (should (eq 'bulk (plist-get record :kind)))
    (should (equal '("a.org" "b.org" "c.org")
                   (mapcar (lambda (part)
                             (file-name-nondirectory (plist-get part :file)))
                           (plist-get record :parts))))
    record))

(defun org-air-r90--bless-echo-re (direction shape)
  "Return the exact final echo the compound DIRECTION of SHAPE must produce."
  (pcase shape
    ('complete (format "\\`%s: backlog 3 marked items (3 files)\\'"
                       (if (eq direction 'undo) "Undid" "Redid")))
    ('failed (format "\\`%s incomplete: 2/3 files %s; failed %s\\'"
                     (if (eq direction 'undo) "Undo" "Redo")
                     (if (eq direction 'undo) "reverted" "reapplied")
                     (if (eq direction 'undo) "a\\.org" "c\\.org")))
    ('invalidated (format "\\`%s incomplete: 2/3 files %s; cache generation"
                          (if (eq direction 'undo) "Undo" "Redo")
                          (if (eq direction 'undo) "reverted" "reapplied")))))

(defun org-air-r90--bless-run (direction shape)
  "Run one compound DIRECTION of SHAPE under a FOREIGN committed hook edit.
The hook is a real buffer-local `after-save-hook' on the SECOND processed
source.  It runs strictly after that file's own irreversible write and
inserts one isolated unsaved user undo group into the FIRST processed
source — a buffer this very command already wrote and committed.  SHAPE
`failed' additionally blocks the third file with the user's own unsaved edit;
SHAPE `invalidated' makes the second file's mandatory cache slot fail so
final disk truth replaces the generation.  Return the audited facts."
  (let* ((names (org-air-r90--bless-names direction))
         (victim-name (nth 0 names))
         (host-name (nth 1 names))
         (third-name (nth 2 names))
         (record (org-air-r90--bless-setup direction))
         (victim (find-file-noselect (org-air-r90--file victim-name)))
         (host (find-file-noselect (org-air-r90--file host-name)))
         (third (find-file-noselect (org-air-r90--file third-name)))
         (write-orig (symbol-function 'org-air-view--cache-sync-write-slots))
         (invalidate-title (org-air-r90--bless-title host-name 1))
         (ran 0)
         (commit-tick nil)
         (hook (lambda ()
                 (cl-incf ran)
                 (when (= ran 1)
                   (with-current-buffer victim
                     ;; The victim's AUTHORITATIVE post-commit tick: exactly
                     ;; what org-air itself left behind in that buffer.
                     (setq commit-tick (buffer-chars-modified-tick))
                     (undo-boundary)
                     (save-excursion
                       (goto-char (point-max))
                       (insert org-air-r90--bless-note)))
                   (when (eq shape 'failed)
                     (with-current-buffer third
                       (undo-boundary)
                       (save-excursion
                         (goto-char (point-max))
                         (insert "# blocking unsaved user edit\n")))))))
         messages)
    (with-current-buffer host (add-hook 'after-save-hook hook nil t))
    (org-air-r90--record-messages collected
      (unwind-protect
          (cl-letf (((symbol-function 'org-air-view--cache-sync-write-slots)
                     (lambda (item file position tags)
                       (if (and (eq shape 'invalidated)
                                (equal invalidate-title
                                       (org-air-item-title item)))
                           (error "mandatory %s slot failure" invalidate-title)
                         (funcall write-orig item file position tags))))
                    ((symbol-function 'display-warning) (lambda (&rest _) nil)))
            (if (eq direction 'undo) (org-air-edit-undo) (org-air-edit-redo)))
        (with-current-buffer host (remove-hook 'after-save-hook hook t)))
      (setq messages (nreverse collected)))
    (list :direction direction :shape shape :record record
          :victim victim :victim-name victim-name
          :host host :host-name host-name
          :third third :third-name third-name
          ;; The committed set: the victim and the host always commit; the
          ;; third file only survives the complete-success shape.
          :committed (if (eq shape 'complete) names (seq-take names 2))
          :ran ran :commit-tick commit-tick :messages messages)))

(defun org-air-r90--bless-assert-scenario (facts)
  "Assert FACTS describe the exact intended foreign-hook scenario.
Every statement here is about what really happened, so a later law failure
can only be the restamp itself."
  (let* ((direction (plist-get facts :direction))
         (shape (plist-get facts :shape))
         (victim (plist-get facts :victim))
         (victim-name (plist-get facts :victim-name))
         (committed (plist-get facts :committed))
         (commit-tick (plist-get facts :commit-tick)))
    ;; 1. The hook ran exactly once, on an already-committed buffer, and left
    ;;    live-only user text there.
    (should (= 1 (plist-get facts :ran)))
    (should (integerp commit-tick))
    (should (> (buffer-chars-modified-tick victim) commit-tick))
    (should (buffer-modified-p victim))
    (should (org-air-r90--bless-note-live-p victim))
    (should-not (org-air-r90--bless-note-disk-p victim-name))
    ;; The victim's live buffer is EXACTLY org-air's committed disk truth plus
    ;; the user's own unsaved note.
    (should (equal (org-air-r90--full-live-text victim)
                   (concat (org-air-r90--text victim-name)
                           org-air-r90--bless-note)))
    ;; 2. The command's own echo is the honest one for its branch.
    (should (= 1 (org-air-r90--count-messages
                  (plist-get facts :messages)
                  (org-air-r90--bless-echo-re direction shape))))
    ;; 3. Disk, cache and marks agree with what really moved.
    (dolist (name '("a.org" "b.org" "c.org"))
      (let* ((title (org-air-r90--bless-title name 1))
             (moved (and (member name committed) t))
             (tagged (if (eq direction 'undo) (not moved) moved))
             (item (org-air-test-find-item title org-air-view--items))
             (buffer (find-file-noselect (org-air-r90--file name))))
        (ert-info ((format "%s moved=%S tagged=%S" name moved tagged))
          (should (eq (and (org-air-r90--disk-has-tag-p name title "backlog") t)
                      tagged))
          (should item)
          (should (eq (and (member "backlog" (org-air-r90--sorted-tags item)) t)
                      tagged))
          ;; No source buffer is ever left simultaneously unmodified and
          ;; divergent from its own file.
          (unless (buffer-modified-p buffer)
            (should (equal (org-air-r90--full-live-text buffer)
                           (org-air-r90--text name)))))))
    (should-not org-air-view--marked-keys)
    (should-not org-air-view--pending-mutation-landing)))

(ert-deftest org-air-r90-69-committed-restamp-records-only-its-own-commit ()
  "A restamp may only ever record a buffer's OWN authoritative commit state.
R75 Decision 5 gives the chars-tick guard its whole meaning: after a ring op
writes a buffer, that buffer's other ring records are restamped so the guard
keeps saying \"no NON-ring change intervened\".  The op may therefore only
ever stamp the state IT produced.  A compound `u'/`U' commits its files one
by one but sweeps the committed buffers ONCE, at command end, so a LATER
part's own committed `after-save-hook' can move an EARLIER, already-written
part's buffer in between.  That movement is a NON-ring change by a third
party; absorbing it into the new stamp blesses it, and the guard then says
\"nothing intervened\" about the user's own unsaved text.  Every branch that
sweeps must obey this: complete success, a blocked later part, and a cache
generation rebuild, in both directions."
  (skip-unless (locate-library "org-air"))
  (dolist (direction '(undo redo))
    (dolist (shape '(complete failed invalidated))
      (let ((pending-undo-list nil)
            (undo-equiv-table (make-hash-table :test #'eq))
            (last-command nil)
            (this-command nil))
        (org-air-r90--with-board org-air-r90--bless-board
          (let* ((facts (org-air-r90--bless-run direction shape))
                 (victim (plist-get facts :victim))
                 (commit-tick (plist-get facts :commit-tick))
                 (post-tick (buffer-chars-modified-tick victim))
                 (entries (org-air-r90--bless-ring-entries victim)))
            (ert-info ((format "%S/%S messages=%S" direction shape
                               (plist-get facts :messages)))
              (org-air-r90--bless-assert-scenario facts)
              ;; THE LAW.  A foreign unsaved change sits ahead in the victim,
              ;; so NO record naming it may pass the guard in either
              ;; direction, and no TICK-guarded record may carry the
              ;; post-hook tick the sweep happens to read at command end —
              ;; only a tick org-air itself produced in that buffer.
              (should entries)
              (dolist (entry entries)
                (ert-info ((format "entry %S in %s stamped=%S commit=%S post=%S"
                                   (plist-get entry :desc)
                                   (file-name-nondirectory
                                    (or (plist-get entry :file) "?"))
                                   (plist-get entry :tick)
                                   commit-tick post-tick))
                  (should-not (org-air-view--history-expected-safe-p
                               entry 'undo))
                  (should-not (org-air-view--history-expected-safe-p
                               entry 'redo))
                  (unless (plist-member entry :expected-undo)
                    (should-not (eql (plist-get entry :tick) post-tick)))))
              ;; The honest value was available to the sweep all along: on the
              ;; complete-success branch the compound's OWN part for the very
              ;; same buffer — restamped at commit time from the save result —
              ;; carries exactly the post-commit tick.
              (when (eq shape 'complete)
                (let ((part (seq-find (lambda (candidate)
                                        (eq (plist-get candidate :buffer)
                                            victim))
                                      (plist-get (plist-get facts :record)
                                                 :parts))))
                  (should part)
                  (should (eql (plist-get part :tick) commit-tick)))))))))))

(ert-deftest org-air-r90-70-blessed-buffer-keeps-its-unsaved-user-text ()
  "A ring walk after a compound op may never eat a foreign unsaved edit.
This is the user-visible half of the restamp law.  When the command-final
sweep stamps a later hook's edit onto an already-committed buffer's other
records, the very next `u' passes a guard that should have refused it: it
runs `undo-only' on the USER's newest group instead of org-air's, saves that
away, and still echoes `Undid: …'.  So for every branch and both directions:
the user's unsaved text survives each following ring press, a refusal moves
zero bytes, and a claimed step on the victim's own record must really have
happened in that buffer."
  (skip-unless (locate-library "org-air"))
  (dolist (direction '(undo redo))
    (dolist (shape '(complete failed invalidated))
      (let ((pending-undo-list nil)
            (undo-equiv-table (make-hash-table :test #'eq))
            (last-command nil)
            (this-command nil))
        (org-air-r90--with-board org-air-r90--bless-board
          (let* ((facts (org-air-r90--bless-run direction shape))
                 (victim (plist-get facts :victim))
                 (victim-name (plist-get facts :victim-name))
                 (reverted-title (org-air-r90--bless-title victim-name 2)))
            (ert-info ((format "%S/%S messages=%S" direction shape
                               (plist-get facts :messages)))
              (org-air-r90--bless-assert-scenario facts)
              ;; Keep pressing the ring the user would press next, stopping as
              ;; soon as a record is honestly refused and stays at the head.
              (catch 'org-air-r90--bless-done
                (dotimes (index 3)
                  (let* ((ring (if (and (eq direction 'redo)
                                        org-air-view--edit-redo-ring)
                                   'org-air-view--edit-redo-ring
                                 'org-air-view--edit-ring))
                         (next (car (symbol-value ring)))
                         (before-live (org-air-r90--full-live-text victim))
                         (before-disk (org-air-r90--text victim-name)))
                    (unless next (throw 'org-air-r90--bless-done nil))
                    (org-air-r90--record-messages walked
                      (if (eq ring 'org-air-view--edit-ring)
                          (org-air-edit-undo)
                        (org-air-edit-redo))
                      (let* ((steps (nreverse walked))
                             (after (org-air-r90--full-live-text victim))
                             (claims (org-air-r90--count-messages
                                      steps org-air-r90--bless-claim-re))
                             (refusals (org-air-r90--count-messages
                                        steps org-air-r90--bless-refusal-re)))
                        (ert-info ((format "step %d popped %S: %S"
                                           index (plist-get next :desc) steps))
                          ;; 1. The user's unsaved text is never destroyed.
                          (should (org-air-r90--bless-note-live-p victim))
                          ;; 2. An honest refusal moves zero bytes anywhere.
                          (when (> refusals 0)
                            (should (equal before-live after))
                            (should (equal before-disk
                                           (org-air-r90--text victim-name))))
                          ;; 3. A claimed step on the victim's own ordinary
                          ;;    record must really have taken that step.
                          (when (and (> claims 0)
                                     (not (eq (plist-get next :kind) 'bulk))
                                     (eq (plist-get next :buffer) victim))
                            (should (string-match-p
                                     (format "^\\*+ %s %s"
                                             (if (eq direction 'undo)
                                                 "TODO" "DONE")
                                             reverted-title)
                                     after))))))
                    ;; A refused record stays at the head: stop rather than
                    ;; press the same key forever.
                    (when (eq next (car (symbol-value ring)))
                      (throw 'org-air-r90--bless-done nil))))))))))))

(ert-deftest org-air-r90-71-complete-compound-never-restamps-a-dirtied-buffer ()
  "The complete-success sweep must skip a buffer its own hook left ahead.
When a part's own committed `after-save-hook' leaves unsaved text in the part's
own buffer, that buffer's commit-time tick already disagrees with reality, so
the command-final sweep must not restamp it: the one exact next identity is
armed instead and every other record for that buffer keeps its old stamp, so
the guard still refuses honestly.  Dropping that exclusion silently blesses
the user's ahead text on the whole-success path — the one branch where no
other permanent test looks."
  (skip-unless (locate-library "org-air"))
  (let ((pending-undo-list nil)
        (undo-equiv-table (make-hash-table :test #'eq))
        (last-command nil)
        (this-command nil))
    (org-air-r90--with-board org-air-r90--bless-board
      (org-air-r90--expand-section 'attention)
      ;; Two ordinary records in the SAME source: only the newest can be armed
      ;; with an exact identity, so the older one is exactly what a too-wide
      ;; sweep would bless.
      (dolist (title '("C3" "C2"))
        (org-air-r90--goto-row title)
        (org-air-item-done))
      (dolist (title '("A1" "B1" "C1")) (org-air-r90--mark-title title))
      (org-air-item-backlog)
      (let* ((victim (find-file-noselect (org-air-r90--file "c.org")))
             (newest (org-air-r90--find-record-by-desc "done \"C2\""))
             (older (org-air-r90--find-record-by-desc "done \"C3\""))
             (older-tick (plist-get older :tick))
             (ran 0)
             (commit-tick nil)
             (hook (lambda ()
                     (cl-incf ran)
                     (when (= ran 1)
                       (setq commit-tick (buffer-chars-modified-tick))
                       (undo-boundary)
                       (save-excursion
                         (goto-char (point-max))
                         (insert org-air-r90--bless-note))))))
        (should (integerp older-tick))
        (with-current-buffer victim (add-hook 'after-save-hook hook nil t))
        (org-air-r90--record-messages collected
          (unwind-protect (org-air-edit-undo)
            (with-current-buffer victim (remove-hook 'after-save-hook hook t)))
          (let ((messages (nreverse collected)))
            (ert-info ((format "complete compound messages: %S" messages))
              (should (= 1 (org-air-r90--count-messages
                            messages
                            (org-air-r90--bless-echo-re 'undo 'complete)))))))
        (let ((post-tick (buffer-chars-modified-tick victim)))
          (should (= 1 ran))
          (should (> post-tick commit-tick))
          (should (buffer-modified-p victim))
          (should (org-air-r90--bless-note-live-p victim))
          (should-not (org-air-r90--bless-note-disk-p "c.org"))
          ;; The one exact next identity is armed, so it is governed by the
          ;; opaque token and not by any tick at all.
          (should (plist-member newest :expected-undo))
          (org-air-r90--assert-history-token (plist-get newest :expected-undo))
          ;; THE LAW: every remaining tick-guarded record for that buffer keeps
          ;; the stamp it had, and none of them absorbs the ahead user edit.
          (should (equal (list older)
                         (org-air-r90--tick-guarded-records victim)))
          (should (eql (plist-get older :tick) older-tick))
          (should-not (eql (plist-get older :tick) post-tick))
          (should-not (org-air-view--history-expected-safe-p older 'undo))
          ;; The sweep still does its ordinary job for every buffer whose own
          ;; commit was the last word.
          (dolist (name '("a.org" "b.org"))
            (let ((buffer (find-file-noselect (org-air-r90--file name))))
              (should (null (org-air-r90--stale-stamped-records buffer)))
              (should-not (buffer-modified-p buffer))))
          ;; Product: the next `u' is refused honestly and moves zero bytes.
          (let ((before-live (org-air-r90--full-live-text victim))
                (before-disk (org-air-r90--text "c.org")))
            (org-air-r90--record-messages walked
              (org-air-edit-undo)
              (let ((messages (nreverse walked)))
                (ert-info ((format "refusal messages: %S" messages))
                  (should (= 1 (org-air-r90--count-messages
                                messages org-air-r90--bless-refusal-re)))
                  (should (= 1 (org-air-r90--count-messages
                                messages "ahead of the expected org-air step")))
                  (should (= 0 (org-air-r90--count-messages
                                messages org-air-r90--bless-claim-re))))))
            (should (equal before-live (org-air-r90--full-live-text victim)))
            (should (equal before-disk (org-air-r90--text "c.org")))
            (should (org-air-r90--bless-note-live-p victim))
            (should (eq newest (car org-air-view--edit-ring)))))))))

(defun org-air-r90--ring-entries-for (ring buffer)
  "Return every entry on RING naming BUFFER, in the order a press reaches them.
Compound parts count: the next same-buffer step can live inside one."
  (let (out)
    (dolist (rec (symbol-value ring))
      (if (eq (plist-get rec :kind) 'bulk)
          (dolist (part (plist-get rec :parts))
            (when (eq (plist-get part :buffer) buffer) (push part out)))
        (when (eq (plist-get rec :buffer) buffer) (push rec out))))
    (nreverse out)))

(defun org-air-r90--armed-records (ring buffer)
  "Return RING entries for BUFFER that carry an exact `:expected-undo' identity.
Arming is exactly how the restamp law leaves a skipped buffer retryable, so
counting these is how a caller observes what the sweep really armed."
  (seq-filter (lambda (entry) (plist-member entry :expected-undo))
              (org-air-r90--ring-entries-for ring buffer)))

(ert-deftest org-air-r90-72-skipped-committed-buffer-is-armed-not-abandoned ()
  "A buffer the restamp law skips must be left RETRYABLE, on the RIGHT side.
Refusing to stamp is only half of the law.  `org-air-view--history-restamp-committed'
must also arm the ONE exact next identity for that buffer — the identity its
OWN commit-time save result captured — on the ring a same-direction next press
pops from (`u' pops the undo side, `U' the redo side).  Arm nothing and the
buffer's records stay frozen behind a stale tick that no user action can ever
satisfy: the step becomes unreachable instead of retryable.  Arm the WRONG
side and the record the user will actually press is left unprotected while an
unrelated one is rewritten.  Neither mutation blesses anything, so every
no-blessing assertion (r90-69/70/71) stays green through both — this is where
they are caught, together with the sweep's proof requirement: a committed part
with NO authority entry is neither stamped nor armed."
  (skip-unless (locate-library "org-air"))
  ;; NO PROOF, NO STAMP.  Driving the real sweep with an empty authority alist
  ;; must leave every ring entry for those buffers exactly as it found them.
  (let ((pending-undo-list nil)
        (undo-equiv-table (make-hash-table :test #'eq))
        (last-command nil)
        (this-command nil))
    (org-air-r90--with-board org-air-r90--bless-board
      (org-air-r90--expand-section 'attention)
      (org-air-r90--goto-row "A2")
      (org-air-item-done)
      (dolist (title '("A1" "B1")) (org-air-r90--mark-title title))
      (org-air-item-backlog)
      (let* ((older (org-air-r90--find-record-by-desc "done \"A2\""))
             (stamped (plist-get older :tick))
             (parts (plist-get (car org-air-view--edit-ring) :parts)))
        (should (integerp stamped))
        (org-air-view--bulk-history-restamp-committed
         parts nil 'org-air-view--edit-ring)
        (should (eql stamped (plist-get older :tick)))
        (should-not (plist-member older :expected-undo)))))
  (dolist (direction '(undo redo))
    (dolist (shape '(complete failed invalidated))
      (let ((pending-undo-list nil)
            (undo-equiv-table (make-hash-table :test #'eq))
            (last-command nil)
            (this-command nil))
        (org-air-r90--with-board org-air-r90--bless-board
          (let* ((facts (org-air-r90--bless-run direction shape))
                 (victim (plist-get facts :victim))
                 (victim-name (plist-get facts :victim-name))
                 (source-ring (if (eq direction 'undo)
                                  'org-air-view--edit-ring
                                'org-air-view--edit-redo-ring))
                 (other-ring (if (eq direction 'undo)
                                 'org-air-view--edit-redo-ring
                               'org-air-view--edit-ring))
                 (reachable (org-air-r90--ring-entries-for source-ring victim))
                 (armed (org-air-r90--armed-records source-ring victim)))
            (ert-info ((format "%S/%S messages=%S armed=%d reachable=%d"
                               direction shape (plist-get facts :messages)
                               (length armed) (length reachable)))
              (org-air-r90--bless-assert-scenario facts)
              ;; 1. Never the wrong side, and never more than one identity.
              (should-not (org-air-r90--armed-records other-ring victim))
              (should (>= 1 (length armed)))
              ;; 2. No OTHER buffer this command committed is disturbed: none
              ;;    is armed and each carries its own authoritative stamp.
              (dolist (name (plist-get facts :committed))
                (unless (equal name victim-name)
                  (let ((buffer (find-file-noselect (org-air-r90--file name))))
                    (ert-info ((format "committed %s" name))
                      (should-not (org-air-r90--armed-records
                                   source-ring buffer))
                      (should-not (org-air-r90--armed-records
                                   other-ring buffer))
                      (should (null (org-air-r90--stale-stamped-records
                                     buffer)))))))
              (cond
               ((eq direction 'undo)
                ;; 3. A next same-buffer UNDO step demonstrably exists here, so
                ;;    exactly one bounded token is armed, and it is armed on the
                ;;    very entry the next `u' would reach first.
                (should (= 1 (length armed)))
                (should (eq (car armed) (car reachable)))
                (org-air-r90--assert-history-token
                 (plist-get (car armed) :expected-undo))
                ;; 4. Armed is not blessed: the foreign edit still sits ahead.
                (should-not (org-air-view--history-expected-safe-p
                             (car armed) 'undo))
                (should-not (org-air-view--history-expected-safe-p
                             (car armed) 'redo)))
               (t
                ;; 5. A compound `U' leaves no further redo step in a source it
                ;;    has just finished re-applying, so its save result carries
                ;;    no redo identity and there is nothing to arm.  The law
                ;;    then degrades to "stamp NOTHING": every remaining
                ;;    tick-guarded entry for that buffer must still be stale.
                (should-not armed)
                (should (org-air-r90--tick-guarded-records victim))
                (dolist (entry (org-air-r90--tick-guarded-records victim))
                  (should-not (eql (plist-get entry :tick)
                                   (buffer-chars-modified-tick victim))))))
              ;; 6. ARMED MEANS RETRYABLE, not abandoned.  The user's own
              ;;    resolution must hand the exact step back — otherwise
              ;;    "refuse" would silently mean "lose the step forever".
              (when (and (eq direction 'undo) (eq shape 'complete))
                (let ((record (car armed))
                      (before-disk (org-air-r90--text victim-name)))
                  (org-air-r90--record-messages refused
                    (org-air-edit-undo)
                    (let ((messages (nreverse refused)))
                      (ert-info ((format "refusal messages: %S" messages))
                        (should (= 1 (org-air-r90--count-messages
                                      messages
                                      org-air-r90--bless-refusal-re)))
                        (should (= 0 (org-air-r90--count-messages
                                      messages
                                      org-air-r90--bless-claim-re))))))
                  (should (equal before-disk (org-air-r90--text victim-name)))
                  (should (org-air-r90--bless-note-live-p victim))
                  ;; The user resolves their OWN group and saves, exactly as
                  ;; r90-54 prescribes for the same-buffer hook path.
                  (with-current-buffer victim
                    (undo-boundary)
                    (undo-only)
                    (save-buffer))
                  (should-not (org-air-r90--bless-note-live-p victim))
                  (should-not (buffer-modified-p victim))
                  (should (eq record (car org-air-view--edit-ring)))
                  (org-air-r90--record-messages walked
                    (org-air-edit-undo)
                    (let ((messages (nreverse walked)))
                      (ert-info ((format "retry messages: %S" messages))
                        (should (= 1 (org-air-r90--count-messages
                                      messages org-air-r90--bless-claim-re)))
                        (should (= 0 (org-air-r90--count-messages
                                      messages
                                      org-air-r90--bless-refusal-re))))))
                  ;; and the retry really took ITS step, on disk.
                  (should (string-match-p
                           (format "^\\*+ TODO %s"
                                   (org-air-r90--bless-title victim-name 2))
                           (org-air-r90--text victim-name))))))))))))

(provide 'org-air-round90-test)
;;; org-air-round90-test.el ends here
