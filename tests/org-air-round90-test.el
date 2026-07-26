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
              (lambda (&rest _) "long-shared-tag")))
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
    " leading" "trailing " "\tleading" "trailing\t"
    "\nleading" "trailing\n" ":")
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
                 (lambda (&rest _) "a-very-long-shared-tag")))
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
                       '(:disk-live t :first-tags ("long-shared-tag")
                         :later-tags nil :first-position t :later-position t
                         :marked nil :history bulk :parts 1))))
      (let ((committed (org-air-r90--text "tasks.org"))
            (record (car org-air-view--edit-ring)))
        (should (org-air-r90--disk-has-tag-p
                 "tasks.org" "First heading" "long-shared-tag"))
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
        (should (equal '("long-shared-tag")
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

;;;; r90-31 — cache v7 semantic invalidation and truthful roundtrip.

(ert-deftest org-air-r90-31-cache-v7-rejects-malformed-v6-projection ()
  "A v6 malformed title/tag projection misses; v7 broad truth round-trips."
  (skip-unless (locate-library "org-air"))
  (org-air-r90--with-corpus
      '(("tasks.org" . "#+title: tasks\n\n* TODO Alpha task :shared-tag:\n")
        ("inbox.org" . "#+title: inbox\n"))
    (let* ((files (org-air-query-files))
           (items (org-air-query-items))
           (truth (org-air-r90--item "Alpha task" items))
           (mtimes (org-air-view--mtimes-snapshot files))
           (malformed (copy-sequence truth)))
      (should (equal "Alpha task" (org-air-item-title truth)))
      (should (equal '("shared-tag") (org-air-item-tags truth)))
      (setf (org-air-item-title malformed) "Alpha task :shared-tag:"
            (org-air-item-tags malformed) nil)
      (let ((print-length nil) (print-level nil) (print-circle t))
        (make-directory (file-name-directory org-air-cache-file) t)
        (write-region
         (prin1-to-string
          (list :version 6 :key (org-air-view--cache-key)
                :mtimes mtimes :file-meta nil :visits nil
                :items (list (org-air-view--item-serialise malformed))))
         nil org-air-cache-file nil 'silent))
      ;; Semantic invalidation: the old malformed title/nil-tags object never
      ;; reaches either cache API, despite carrying the current coherence key.
      (should-not (org-air-view--cache-read))
      (should-not (org-air-view--cache-load))
      (should (= 7 org-air-view--cache-version))
      ;; Current broad projection is the only serialised truth accepted.
      (org-air-view--cache-write items mtimes)
      (let* ((data (org-air-view--cache-read))
             (hydrated (org-air-r90--item
                        "Alpha task" (plist-get data :items))))
        (should (= 7 (plist-get data :version)))
        (should (equal "Alpha task" (org-air-item-title hydrated)))
        (should (equal '("shared-tag") (org-air-item-tags hydrated)))))))

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
                     (lambda (&rest _) "shared-tag")))
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
                     (if (eq action 'backlog) "backlog" "shared-tag"))))
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

;;;; r90-38/39 — shared broad projection and singular input safety.

(ert-deftest org-air-r90-38-broad-projection-inheritance-prefix-and-refresh ()
  "Hyphen tags, inheritance controls, prefixes, refresh, and second writes agree."
  (skip-unless (locate-library "org-air"))
  (let ((specs
         '(("tasks.org" . "#+title: tasks\n#+TODO: NEXT WAIT | DONE\n#+FILETAGS: :file-hyphen:dup-tag:blocked-file:\n\n* Parent :parent-hyphen:dup-tag:blocked-parent:\n** NEXT [#B] COMMENT Child exact :local-hyphen:dup-tag:\n* WAIT [#A] COMMENT Prefix exact\n")
           ("inbox.org" . "#+title: inbox\n"))))
    ;; The shared projection follows nil/list/regexp inheritance semantics and
    ;; exclusions.  Local duplicates win once and always remain local.
    (org-air-r90--with-corpus specs
      (cl-labels
          ((scan-child ()
             (prog1
                 (org-air-r90--sorted-tags
                  (org-air-r90--item "Child exact" (org-air-query-items)))
               (org-air-query-teardown))))
        (let ((org-use-tag-inheritance nil))
          (should (equal '("dup-tag" "local-hyphen")
                         (scan-child))))
        (let ((org-use-tag-inheritance
               '("file-hyphen" "parent-hyphen" "dup-tag"
                 "blocked-parent"))
              (org-tags-exclude-from-inheritance '("blocked-parent")))
          (should (equal '("dup-tag" "file-hyphen" "local-hyphen"
                           "parent-hyphen")
                         (scan-child))))
        (let ((org-use-tag-inheritance
               "\\`\\(?:file\\|parent\\)-hyphen\\'")
              (org-tags-exclude-from-inheritance nil))
          (should (equal '("dup-tag" "file-hyphen" "local-hyphen"
                           "parent-hyphen")
                         (scan-child))))))
    (let ((org-tags-exclude-from-inheritance
           '("blocked-file" "blocked-parent")))
      (org-air-r90--with-board specs
        (let ((child (org-air-r90--item "Child exact")))
          (should (equal "Child exact" (org-air-item-title child)))
          (should (equal "NEXT" (org-air-item-todo child)))
          (should (equal '("dup-tag" "file-hyphen" "local-hyphen"
                           "parent-hyphen")
                         (org-air-r90--sorted-tags child)))
          (should (= 1 (seq-count (lambda (tag) (equal tag "dup-tag"))
                                   (org-air-item-tags child)))))
        ;; Marked shared-tag, then broad unmarked values, each survive a real
        ;; replacement generation as plain title plus exact effective tags.
        (org-air-r90--mark-title "Child exact")
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) "shared-tag")))
          (org-air-set-tag))
        (dolist (tag '("@context" "under_score" "日本語"))
          (org-air-r90--goto-row "Child exact")
          (cl-letf (((symbol-function 'read-string)
                     (lambda (&rest _) tag)))
            (org-air-set-tag))
          (let ((child (org-air-r90--item "Child exact")))
            (should (equal "Child exact" (org-air-item-title child)))
            (should (member tag (org-air-item-tags child)))))
        (org-air-refresh)
        (let* ((child (org-air-r90--item "Child exact"))
               (tags (org-air-item-tags child))
               (text (org-air-r90--text "tasks.org")))
          (dolist (tag '("shared-tag" "@context" "under_score" "日本語"
                         "file-hyphen" "parent-hyphen" "local-hyphen"))
            (should (member tag tags)))
          (should (= 1 (seq-count (lambda (tag) (equal tag "dup-tag")) tags)))
          (should (string-match-p
                   "^\\*\\* NEXT \\[#[B]\\] COMMENT Child exact :local-hyphen:dup-tag:shared-tag:@context:under_score:日本語:$"
                   text))
          (should-not (string-match-p
                       "^\\*\\* NEXT .*\\(?:file-hyphen\\|parent-hyphen\\|blocked-\\)"
                       text)))
        ;; Prefix bytes and semantic slots survive; only the local suffix moves.
        (let ((prefix (org-air-r90--item "Prefix exact")))
          (should (equal "WAIT" (org-air-item-todo prefix)))
          (should (= (org-air-item-priority prefix)
                     (org-get-priority "[#A]"))))
        (org-air-r90--goto-row "Prefix exact")
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) "prefix-tag")))
          (org-air-set-tag))
        (should (string-match-p
                 "^\\* WAIT \\[#[A]\\] COMMENT Prefix exact :prefix-tag:$"
                 (org-air-r90--text "tasks.org")))
        (let ((prefix (org-air-r90--item "Prefix exact")))
          (should (equal "Prefix exact" (org-air-item-title prefix)))
          (should (equal "WAIT" (org-air-item-todo prefix)))
          (should (= (org-air-item-priority prefix)
                     (org-get-priority "[#A]"))))
        ;; A second cached mutation resolves the exact broad-projected child
        ;; without a query, and inherited values still never become local.
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
                       "file-hyphen\\|parent-hyphen\\|blocked-" line)))))))

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
           warnings)
      (org-air-r90--mark-title "Alpha")
      (let ((query-orig (symbol-function 'org-air-query-items)))
        (cl-letf (((symbol-function 'org-air-view--cache-sync-write-slots)
                   (lambda (item file position tags)
                     (if (equal "Beta" (org-air-item-title item))
                         (error "mandatory Beta slot failed")
                       (funcall orig-write item file position tags))))
                  ((symbol-function 'org-air-query-items)
                   (lambda (&rest args)
                     (cl-incf queries) (apply query-orig args)))
                  ((symbol-function 'display-warning)
                   (lambda (&rest args) (push args warnings))))
          (org-air-item-backlog)))
      (should (> queries 0))
      (should warnings)
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

(provide 'org-air-round90-test)
;;; org-air-round90-test.el ends here
