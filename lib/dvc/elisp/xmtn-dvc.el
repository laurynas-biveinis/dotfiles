;;; xmtn-dvc.el --- DVC backend for monotone

;; Copyright (C) 2008 - 2011 Stephen Leake
;; Copyright (C) 2006, 2007, 2008 Christian M. Ohler

;; Author: Christian M. Ohler
;; Keywords: tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA  02110-1301  USA.

;;; Commentary:

;; This file implements a DVC backend for the distributed version
;; control system monotone.  The backend will only work with an
;; appropriate version of the mtn binary installed.

;;; Code:

;;; There are some notes on the design of xmtn in
;;; docs/xmtn-readme.txt.

(eval-and-compile
  (require 'cl) ;; yes, we are using cl at runtime; we're working towards eliminating that.
  (require 'dvc-unified)
  (require 'xmtn-basic-io)
  (require 'xmtn-base)
  (require 'xmtn-run)
  (require 'xmtn-automate)
  (require 'xmtn-conflicts)
  (require 'xmtn-ids)
  (require 'xmtn-match)
  (require 'xmtn-minimal)
  (require 'dvc-log)
  (require 'dvc-diff)
  (require 'dvc-status)
  (require 'dvc-core)
  (require 'ewoc))

;; For debugging.
(defun xmtn--load ()
  (require 'dvc-unified)
  (save-some-buffers)
  (mapc (lambda (file)
          (byte-compile-file file t))
        '("xmtn-minimal.el"
          "xmtn-compat.el"
          "xmtn-match.el"
          "xmtn-base.el"
          "xmtn-run.el"
          "xmtn-automate.el"
          "xmtn-basic-io.el"
          "xmtn-ids.el"
          "xmtn-dvc.el"
          "xmtn-revlist.el")))
;;; (xmtn--load)

;;;###autoload
(dvc-register-dvc 'xmtn "monotone")

(defmacro* xmtn--with-automate-command-output-basic-io-parser
    ((parser root-form command-form)
     &body body)
  (declare (indent 1) (debug (sexp body)))
  (let ((root (gensym))
        (command (gensym))
        (session (gensym))
        (handle (gensym)))
    `(let ((,root ,root-form)
           (,command ,command-form))
       (let* ((,session (xmtn-automate-cache-session ,root))
              (,handle (xmtn-automate--new-command ,session ,command)))
         (xmtn-automate-command-wait-until-finished ,handle)
         (prog1
             (xmtn-basic-io-with-stanza-parser
                 (,parser (xmtn-automate-command-buffer ,handle))
               ,@body)
           (xmtn-automate--cleanup-command ,handle))))))

;;;###autoload
(defun xmtn-dvc-log-edit-file-name-func (&optional root)
  (concat (file-name-as-directory (or root (dvc-tree-root)))
          "_MTN/log"))

(defun xmtn--toposort (root revision-hash-ids)
  (xmtn-automate-command-output-lines root
                                             `("toposort"
                                               ,@revision-hash-ids)))

;;;###autoload
(defun xmtn-dvc-log-edit (root other-frame no-init)
  (if no-init
      (dvc-dvc-log-edit root other-frame no-init)
    (progn
      (dvc-dvc-log-edit root other-frame nil)
      (setq buffer-file-coding-system 'xmtn--monotone-normal-form)
      )))

(defun xmtn-dvc-log-message ()
  "Return --message-file argument string, if any."
  (let ((log-edit-file "_MTN/log"))
    (if (file-exists-p log-edit-file)
        (concat "--message-file=" log-edit-file))))

;;;###autoload
(defun xmtn-dvc-log-edit-done (&optional prompt-branch)
  (let* ((root default-directory)
         (files (or (with-current-buffer dvc-partner-buffer
                      (dvc-current-file-list 'nil-if-none-marked))
                    'all))
         (normalized-files
          (case files
            (all 'all)
            (t
             ;; Need to normalize in original buffer, since
             ;; switching buffers changes default-directory and
             ;; therefore the semantics of relative file names.
             (with-current-buffer dvc-partner-buffer
               (xmtn--normalize-file-names root files)))))
         (excluded-files
          (with-current-buffer dvc-partner-buffer
            (xmtn--normalize-file-names root (dvc-fileinfo-excluded-files))))
         (branch (if prompt-branch
                     (progn
                       ;; an automate session caches the original
                       ;; options, and will not use the new branch.
                       (let ((session (xmtn-automate-get-cached-session (dvc-uniquify-file-name root))))
                         (if session (xmtn-automate--close-session session)))
                       (read-from-minibuffer "branch: " (xmtn--tree-default-branch root)))
                   (xmtn--tree-default-branch root))))
    (save-buffer)
    (dvc-save-some-buffers root)

    ;; check that the first line says something; it should be a summary of the rest
    (goto-char (point-min))
    (forward-line)
    (if (= (point) (1+ (point-min)))
        (error "Please put a summary comment on the first line"))

    ;; We used to check for things that would make commit fail;
    ;; missing files, nothing to commit. But that just slows things
    ;; down in the typical case; better to just handle the error
    ;; message, which is nicely informative anyway.
    (lexical-let* ((progress-message
                    (case normalized-files
                      (all (format "Committing all files in %s" root))
                      (t (case (length normalized-files)
                           (0 (assert nil))
                           (1 (format "Committing file %s in %s"
                                      (first normalized-files) root))
                           (t
                            (format "Committing %s files in %s"
                                    (length normalized-files)
                                    root)))))))
      (xmtn--run-command-async
       root
       `("commit" ,(xmtn-dvc-log-message)
         ,(concat "--branch=" branch)
         "--non-interactive"
         ,@(case normalized-files
             (all
              (if excluded-files
                  (mapcar (lambda (file) (concat "--exclude=" file)) excluded-files)
                '()))
             (t (list*
                 ;; Since we are specifying files explicitly, don't
                 ;; recurse into specified directories. Also commit
                 ;; normally excluded files if they are selected.
                 "--depth=0"
                 "--" normalized-files))))
       :error (lambda (output error status arguments)
                (dvc-default-error-function output error
                                            status arguments))
       :killed (lambda (output error status arguments)
                 (dvc-default-killed-function output error
                                              status arguments))
       :finished (lambda (output error status arguments)
                   (message "%s... done" progress-message)
                   ;; Monotone creates an empty log file when the
                   ;; commit was successful.  Let's not interfere with
                   ;; that.  (Calling `dvc-log-close' would.)

                   ;; we'd like to delete log-edit-buffer here, but we
                   ;; can't do that from a process sentinel. And we'd
                   ;; have to find it; it may not be current buffer,
                   ;; if log-edit-done was invoked from the ediff
                   ;; window.

                   (dvc-diff-clear-buffers 'xmtn
                                           default-directory
                                           "* Just committed! Please refresh buffer"
                                           (xmtn--status-header
                                            default-directory
                                            (xmtn--get-base-revision-hash-id-or-null default-directory)))
                   ))

       ;; Show message _after_ spawning command to override DVC's
       ;; debugging message.
       (message "%s... " progress-message))
    (set-window-configuration dvc-pre-commit-window-configuration)))

(defun xmtn-show-commit ()
  "Show commit command for use on command line"
  (interactive)
  (let ((excluded-files
	 (with-current-buffer dvc-partner-buffer
	   (xmtn--normalize-file-names default-directory (dvc-fileinfo-excluded-files)))))

    (save-buffer)
    (dvc-save-some-buffers default-directory)

    ;; check that the first line says something; it should be a summary of the rest
    (goto-char (point-min))
    (forward-line)
    (if (= (point) (1+ (point-min)))
        (error "Please put a summary comment on the first line"))

    (message
     (concat
      "mtn commit "
      (xmtn-dvc-log-message)
      " "
      (if excluded-files
	  (mapconcat (lambda (file) (concat "--exclude=" file)) excluded-files " "))))
    (pop-to-buffer "*Messages*")))

;; Add xmtn-show-commit to dvc-log-edit menu
(defvar xmtn-log-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c) (control ?s)] 'xmtn-show-commit)
    map))

(easy-menu-define xmtn-log-edit-mode-menu xmtn-log-edit-mode-map
  "Mtn specific log-edit menu."
  `("DVC-Mtn"
    ["Show commit command" xmtn-show-commit t]
    ))

(define-derived-mode xmtn-log-edit-mode dvc-log-edit-mode "xmtn-log-edit"
  "Add back-end-specific commands for dvc-log-edit.")

(dvc-add-uniquify-directory-mode 'xmtn-log-edit-mode)

;; The term "normalization" here has nothing to do with Unicode
;; normalization.
(defun xmtn--normalize-file-name (root file-name)
  (assert root)
  (let ((normalized-name (file-relative-name file-name root)))
    normalized-name))

(defun xmtn--normalize-file-names (root file-names)
  (check-type file-names list)
  (mapcar (lambda (file-name) (xmtn--normalize-file-name root file-name))
          file-names))

(defun xmtn--display-buffer-maybe (buffer dont-switch)
  (let ((orig-buffer (current-buffer)))
    (if dvc-switch-to-buffer-first
        (dvc-switch-to-buffer buffer)
      (set-buffer buffer))
    (when dont-switch (pop-to-buffer orig-buffer)))
  nil)

(defun xmtn--status-header (root base-revision)
  (let* ((branch (xmtn--tree-default-branch root))
         (head-revisions (xmtn--heads root branch))
         (head-count (length head-revisions)))

    (concat
      (format "Status for %s:\n" root)
      (if base-revision
          (format "  base revision %s\n" base-revision)
        "  tree has no base revision\n")
      (format "  branch %s\n" branch)
      (case head-count
        (0 "  branch is empty\n")
        (1 "  branch is merged\n")
        (t (dvc-face-add (format "  branch has %s heads; need merge\n" head-count) 'dvc-conflict)))
      (if (member base-revision head-revisions)
          "  base revision is a head revision\n"
        (dvc-face-add "  base revision is not a head revision; need update\n" 'dvc-conflict)))))

(defun xmtn--refresh-status-header (status-buffer)
  (with-current-buffer status-buffer
    ;; different modes use different names for the ewoc
    ;; FIXME: should have a separate function for each mode
    (if dvc-fileinfo-ewoc
      (ewoc-set-hf
       dvc-fileinfo-ewoc
       (xmtn--status-header default-directory (xmtn--get-base-revision-hash-id-or-null default-directory))
       ""))))

(defun xmtn--parse-diff-for-dvc (changes-buffer)
  (let ((excluded-files (dvc-default-excluded-files))
        matched)
    (flet ((add-entry
            (path status dir &optional orig-path)
            (with-current-buffer changes-buffer
              (ewoc-enter-last
               dvc-fileinfo-ewoc
               (if dir
                   (make-dvc-fileinfo-dir
                    :mark nil
                    :exclude (dvc-match-excluded excluded-files path)
                    :dir (file-name-directory path)
                    :file (file-name-nondirectory path)
                    :status status
                    :more-status "")
                 (make-dvc-fileinfo-file
                  :mark nil
                  :exclude (dvc-match-excluded excluded-files path)
                  :dir (file-name-directory path)
                  :file (file-name-nondirectory path)
                  :status status
                  :more-status (or orig-path ""))))))
           (likely-dir-p (path) (string-match "/\\'" path)))

      ;; First parse the basic_io contained in dvc-header, if any.
      (let ((revision
             (with-temp-buffer
               (insert dvc-header)
               (goto-char (point-min))
               (while (re-search-forward "^# ?" nil t)
                 (replace-match ""))
               (goto-char (point-min))
               (xmtn-basic-io-skip-blank-lines)
               (delete-region (point-min) (point))
               (xmtn-basic-io-with-stanza-parser
                   (parser (current-buffer))
                 (xmtn--parse-partial-revision parser)))))
        (loop
         for (path) in (xmtn--revision-delete revision)
         do (add-entry path 'deleted (likely-dir-p path)))
        (loop
         for (from to) in (xmtn--revision-rename revision)
         do (assert (eql (not (likely-dir-p from))
                         (not (likely-dir-p to))))
         do (add-entry to 'rename-target (likely-dir-p to) from)
         do (add-entry from 'rename-source (likely-dir-p from) to))
        (loop
         for (path) in (xmtn--revision-add-dir revision)
         do (add-entry path 'added t))
        (loop
         for (path contents)
         in (xmtn--revision-add-file revision)
         do (add-entry path 'added nil))
        (loop
         for (path from-contents to-contents)
         in (xmtn--revision-patch-file revision)
         do (add-entry path 'modified nil))
        ;; Do nothing about clear-attr and set-attr.
        ))

    (setq dvc-header
          (with-current-buffer changes-buffer
            (xmtn--status-header default-directory (xmtn--revision-hash-id dvc-diff-base))))
    nil))

;;;###autoload
(defun xmtn-show-base-revision ()
  "Show the base revision of the current monotone tree in the minibuffer."
  (interactive)
  (let* ((root (dvc-tree-root))
         (hash-id-or-null (xmtn--get-base-revision-hash-id-or-null root)))
    (if hash-id-or-null
        (message "Base revision of tree %s is %s" root hash-id-or-null)
      (message "Tree %s has no base revision" root))))


;;;###autoload
(defun xmtn-dvc-diff (&optional rev path dont-switch)
  ;; If rev is an ancestor of base-rev of path, then rev is from, path
  ;; is 'to', and vice versa.
  ;;
  ;; Note rev might be a string mtn selector, so we have to use
  ;; resolve-revision-id to process it.
  (let ((workspace (list 'xmtn (list 'local-tree (xmtn-tree-root path))))
        (base (xmtn--get-base-revision-hash-id-or-null path))
        (rev-string (cadr (xmtn--resolve-revision-id path rev))))
    (if (string= rev-string base)
        ;; local changes in workspace are 'to'
        (xmtn-dvc-delta rev workspace dont-switch)
      (let ((descendents (xmtn-automate-command-output-lines path (list "descendents" base)))
            (done nil))
        (while descendents
          (if (string= rev-string (car descendents))
              ;; rev is newer than workspace; rev is 'to'
              (progn
                (xmtn-dvc-delta workspace rev dont-switch)
                (setq done t)))
          (setq descendents (cdr descendents)))
        (if (not done)
            ;; rev is ancestor of workspace; workspace is 'to'
            (xmtn-dvc-delta rev workspace dont-switch))))))

(defun xmtn--rev-to-option (resolved from)
  "Return a string contaiing the mtn diff command-line option for RESOLVED.
If FROM is non-nil, RESOLVED is assumed older than workspace;
otherwise newer."
  (ecase (car resolved)
    ('local-tree
     (if from
	 "--reverse"
       ""))
    ('revision (concat "--revision=" (cadr resolved)))))

;;;###autoload
(defun xmtn-dvc-delta (from-revision-id to-revision-id &optional dont-switch)
  ;; See dvc-unified.el dvc-delta for doc string. If strings, they must be mtn selectors.
  (let* ((root (dvc-tree-root))
         (from-resolved (xmtn--resolve-revision-id root from-revision-id))
         (to-resolved (xmtn--resolve-revision-id root to-revision-id)))
    (let ((diff-buffer
           (dvc-prepare-changes-buffer `(xmtn ,from-resolved) `(xmtn ,to-resolved) 'diff root 'xmtn))
          (rev-specs (list (xmtn--rev-to-option from-resolved t)
                           (xmtn--rev-to-option to-resolved nil))))
      (buffer-disable-undo diff-buffer)
      (dvc-save-some-buffers root)
      (lexical-let* ((diff-buffer diff-buffer))
        (xmtn--run-command-async
         root `("diff" ,@rev-specs)
         :related-buffer diff-buffer
         :finished
         (lambda (output error status arguments)
           (with-current-buffer output
             (xmtn--remove-content-hashes-from-diff))
           (dvc-show-changes-buffer output 'xmtn--parse-diff-for-dvc
                                    diff-buffer t "^="))))

      (xmtn--display-buffer-maybe diff-buffer dont-switch)

      ;; The call site in `dvc-revlist-diff' needs this return value.
      diff-buffer)))

(defun xmtn--remove-content-hashes-from-diff ()
  ;; Hack: Remove mtn's file content hashes from diff headings since
  ;; `dvc-diff-diff-or-list' and `dvc-diff-find-file-name' gets
  ;; confused by them.
  (save-excursion
    (goto-char (point-min))
    (while
        (re-search-forward
         "^\\(\\+\\+\\+\\|---\\) \\(.*\\)\\(\t[0-9a-z]\\{40\\}\\)$"
         nil t)
      (replace-match "" t nil nil 3))))


(defun xmtn--simple-finished-notification (buffer)
  (lexical-let ((buffer buffer))
    (lambda (output error status arguments)
      (message "Process %s finished" buffer))))

;;;###autoload
(defun xmtn-dvc-command-version ()
  (fourth (xmtn--command-version xmtn-executable)))

(defun xmtn--changes-image (change)
  (ecase change
    (content "content")
    (attrs   "attrs  ")))

(defun xmtn--status-process-entry (ewoc path status changes old-path new-path
                                        old-type new-type fs-type
                                        excluded-files)
  "Create a file entry in ewoc."
  ;; Don't display root directory (""); if requested, don't
  ;; display known or ignored files.
  (if (and (or (not (equal '(known) status))
               (member 'content changes)
               dvc-status-display-known)
           (or (not (equal '(ignored) status))
               dvc-status-display-ignored)
           (not (equal path "")))
      (let ((main-status
             (or
              (if (member 'added status) 'added)
              (if (member 'deleted status) 'deleted)
              (if (member 'ignored status) 'ignored)
              (if (member 'invalid status) 'invalid)
              (if (member 'missing status) 'missing)
              (if (member 'rename-source status) 'rename-source)
              (if (member 'rename-target status) 'rename-target)
              (if (member 'unknown status) 'unknown)
              ;; check for known last; almost everything is known
              (if (member 'known status)
                  (if (member 'content changes)
                      'modified
                    'known))))

            (indexed (not (eq status 'missing))) ;; in terse mode, missing is represented as "D?"
            (more-status "")
            basic-need-more-status)

        (setq basic-need-more-status
              (or (not (equal status (list main-status)))
                  (not (eq changes nil))))

        (case main-status
          (added
           ;; if the file has been modified since is was marked
           ;; 'added', that's still just 'added', so we never need to
           ;; do anything here.
           nil)

          ((deleted missing)
           (if basic-need-more-status
               (setq more-status
                     (concat
                      (mapconcat 'dvc-fileinfo-status-image-full (delq main-status status) " ")
                      (mapconcat 'xmtn--changes-image changes " ")))))

          ((ignored invalid) nil)


          (rename-source
           (setq more-status new-path))

          (rename-target
           (setq more-status old-path))

          (modified
           (if (and (equal status '(known))
                    (equal changes '(content)))
               ;; just modified, nothing else
               nil
             (if basic-need-more-status
                 (setq more-status
                       (concat
                        (mapconcat 'dvc-fileinfo-status-image-full (delq main-status status) " ")
                        (mapconcat 'xmtn--changes-image changes " "))))))

          (known
           (if basic-need-more-status
               (setq more-status
                     (concat
                      (mapconcat 'dvc-fileinfo-status-image-full (delq main-status status) " ")
                      (mapconcat 'xmtn--changes-image changes " ")))))
          )

        (case (if (equal fs-type 'none)
                  (if (equal old-type 'none)
                      new-type
                    old-type)
                fs-type)
          (directory
           (ewoc-enter-last ewoc
                            (make-dvc-fileinfo-dir
                             :mark nil
                             :exclude (dvc-match-excluded excluded-files path)
                             :dir (file-name-directory path)
                             :file (file-name-nondirectory path)
                             :status main-status
                             :indexed indexed
                             :more-status more-status)))
          ((file none)
           ;; 'none' indicates a dropped (deleted) file
           (ewoc-enter-last ewoc
                            (make-dvc-fileinfo-file
                             :mark nil
                             :exclude (dvc-match-excluded excluded-files path)
                             :dir (file-name-directory path)
                             :file (file-name-nondirectory path)
                             :status main-status
                             :indexed indexed
                             :more-status more-status)))
          (t
           (error "path %s fs-type %s old-type %s new-type %s" path fs-type old-type new-type))
          ))))

(defun xmtn--parse-inventory (stanza-parser fn)
  (loop for stanza = (funcall stanza-parser)
        while stanza do
        (xmtn-match stanza
          ((("path" (string $path))
            . $rest)
           (let* ((status (loop for entry in (cdr (assoc "status" rest))
                                collect
                                (xmtn-match entry
                                  ((string "added") 'added)
                                  ((string "dropped") 'deleted)
                                  ((string "invalid") 'invalid)
                                  ((string "known") 'known)
                                  ((string "missing") 'missing)
                                  ((string "ignored") 'ignored)
                                  ((string "unknown") 'unknown)
                                  ((string "rename_target") 'rename-target)
                                  ((string "rename_source") 'rename-source))))
                  (fs-type (xmtn-match (cdr (assoc "fs_type" rest))
                             (((string "file")) 'file)
                             (((string "directory")) 'directory)
                             (((string "none")) 'none)))
                  (old-type (xmtn-match (cdr (assoc "new_type" rest))
                              (((string "file")) 'file)
                              (((string "directory")) 'directory)
                              (nil 'none)))
                  (new-type (xmtn-match (cdr (assoc "new_type" rest))
                              (((string "file")) 'file)
                              (((string "directory")) 'directory)
                              (nil 'none)))
                  (changes (loop for entry in (cdr (assoc "changes" rest))
                                 collect
                                 (xmtn-match entry
                                   ((string "content") 'content)
                                   ((string "attrs") 'attrs))))
                  (old-path-or-null (xmtn-match (cdr (assoc "old_path" rest))
                                      (((string $old-path)) old-path)
                                      (nil nil)))
                  (new-path-or-null (xmtn-match (cdr (assoc "new_path" rest))
                                      (((string $new-path)) new-path)
                                      (nil nil)))
                  )
             (funcall fn
                      path
                      status
                      changes
                      old-path-or-null
                      new-path-or-null
                      old-type
                      new-type
                      fs-type))))))

(defun xmtn--status-using-inventory (root)
  ;; We don't run automate inventory through xmtn-automate here as
  ;; that would block.  xmtn-automate doesn't support asynchronous
  ;; command execution yet.
  (let*
      ((base-revision (xmtn--get-base-revision-hash-id-or-null root))
       (branch (xmtn--tree-default-branch root))
       (head-revisions (xmtn--heads root branch))
       (head-count (length head-revisions))
       (status-buffer
        (dvc-status-prepare-buffer
         'xmtn
         root
         ;; base-revision
         (if base-revision (format "%s" base-revision) "none")
         ;; branch
         (format "%s" branch)
         ;; header-more
         (lambda ()
           (concat
            (case head-count
              (0 "  branch is empty\n")
              (1 "  branch is merged\n")
              (t (dvc-face-add (format "  branch has %s heads; need merge\n" head-count) 'dvc-conflict)))
            (if (member base-revision head-revisions)
                "  base revision is a head revision\n"
              (dvc-face-add "  base revision is not a head revision; need update\n" 'dvc-conflict))))
         ;; refresh
         'xmtn-dvc-status)))
    (dvc-save-some-buffers root)
    (lexical-let* ((status-buffer status-buffer))
      (xmtn--run-command-async
       root (list "automate" "inventory" "--no-unchanged" "--no-ignored")
       :finished (lambda (output error status arguments)
                   (dvc-status-inventory-done status-buffer)
                   (with-current-buffer status-buffer
                     (let ((excluded-files (dvc-default-excluded-files)))
                       (xmtn-basic-io-with-stanza-parser
                           (parser output)
                         (xmtn--parse-inventory
                          parser
                          (lambda (path status changes old-path new-path
                                        old-type new-type fs-type)
                            (xmtn--status-process-entry dvc-fileinfo-ewoc
                                                        path status
                                                        changes
                                                        old-path new-path
                                                        old-type new-type
                                                        fs-type
                                                        excluded-files))))
                       (when (not (ewoc-locate dvc-fileinfo-ewoc))
                         (ewoc-enter-last dvc-fileinfo-ewoc
                                          (make-dvc-fileinfo-message
                                           :text (concat " no changes in workspace")))
                         (ewoc-refresh dvc-fileinfo-ewoc)))))
       :error (lambda (output error status arguments)
                (dvc-diff-error-in-process ;; correct for status-mode as well
                 status-buffer
                 (format "Error running mtn with arguments %S" arguments)
                 output error))
       :killed (lambda (output error status arguments)
                 ;; Create an empty buffer as a fake output buffer to
                 ;; avoid printing all the output so far.
                 (with-temp-buffer
                   (dvc-diff-error-in-process
                    status-buffer
                    (format "Received signal running mtn with arguments %S"
                            arguments)
                    (current-buffer) error)))))))

(defun xmtn--status-inventory-sync (root)
  "Create or reuse a status buffer for ROOT; return `(buffer status)',
where `status' is 'ok or 'need-commit."
  (let*
      ((orig-buffer (current-buffer))
       (msg (concat "running inventory for " root " ..."))
       (base-revision (xmtn--get-base-revision-hash-id-or-null root))
       (branch (xmtn--tree-default-branch root))
       (head-revisions (xmtn--heads root branch))
       (head-count (length head-revisions))
       (output-buffer (generate-new-buffer " *xmtn-inventory*"))
       status
       (dvc-switch-to-buffer-first nil)
       (status-buffer
        (dvc-status-prepare-buffer
         'xmtn
         root
         ;; base-revision
         (if base-revision (format "%s" base-revision) "none")
         ;; branch
         (format "%s" branch)
         ;; header-more
         (lambda ()
           (concat
            (case head-count
              (0 "  branch is empty\n")
              (1 "  branch is merged\n")
              (t (dvc-face-add (format "  branch has %s heads; need merge\n" head-count) 'dvc-conflict)))
            (if (member base-revision head-revisions)
                "  base revision is a head revision\n"
              (dvc-face-add "  base revision is not a head revision; need update\n" 'dvc-conflict))))
         ;; refresh
         'xmtn-dvc-status)))
    (dvc-save-some-buffers root)
    (message msg)
    (xmtn-automate-command-output-buffer
       root output-buffer
       (list (list "no-unchanged" "" "no-ignored" "")
	     "inventory"))
    (with-current-buffer output-buffer
      (setq status
	    (if (> (point-max) (point-min))
		'need-commit
	      'ok)))
    (dvc-status-inventory-done status-buffer)
    (with-current-buffer status-buffer
      (let ((excluded-files (dvc-default-excluded-files)))
	(xmtn-basic-io-with-stanza-parser
	    (parser output-buffer)
	  (xmtn--parse-inventory
	   parser
	   (lambda (path status changes old-path new-path
			 old-type new-type fs-type)
	     (xmtn--status-process-entry dvc-fileinfo-ewoc
					 path status
					 changes
					 old-path new-path
					 old-type new-type
					 fs-type
					 excluded-files))))
	(when (not (ewoc-locate dvc-fileinfo-ewoc))
	  (ewoc-enter-last dvc-fileinfo-ewoc
			   (make-dvc-fileinfo-message
			    :text (concat " no changes in workspace")))
	  (ewoc-refresh dvc-fileinfo-ewoc))))
    (kill-buffer output-buffer)
    (set-buffer orig-buffer)
    (message (concat msg " done"))
    (list status-buffer status)))

;;;###autoload
(defun xmtn-dvc-status ()
  "Display status of monotone tree at `default-directory'."
  (xmtn--status-using-inventory default-directory))

;;;###autoload
(defun xmtn-dvc-revision-direct-ancestor (revision-id)
  (let* ((root (dvc-tree-root))
         (resolved-id (xmtn--resolve-revision-id root revision-id)))
    `(xmtn ,(xmtn--resolve-backend-id root
                                      `(previous-revision ,resolved-id 1)))))

;;;###autoload
(defun xmtn-dvc-name-construct (backend-revision)
  (check-type backend-revision xmtn--hash-id)
  backend-revision)

(defun xmtn--mtnignore-file-name (root)
  (concat (file-name-as-directory root) ".mtn-ignore"))

;;;###autoload
(defun xmtn-dvc-edit-ignore-files ()
  (find-file-other-window (xmtn--mtnignore-file-name (dvc-tree-root))))

(defun xmtn--quote-string-as-partial-perl-regexp (string)
  ;; The set of file names/patterns to be ignored by monotone is
  ;; customizable by the user through a hook.  So we can't guarantee
  ;; that writing something to .mtn-ignore really has the desired
  ;; effect.  However, we implement the correct behavior for the
  ;; default hook.
  ;;
  ;; The default hook uses the function regex.search, which is defined
  ;; in lua.cc, which, as of monotone revision
  ;; 341e4a18c594cec49896fa97bd4e74de7bee5827, uses Boost.Regex with
  ;; the default settings (Perl syntax).
  ;;
  ;; http://www.boost.org/libs/regex/doc/syntax_perl.html describes
  ;; this syntax.  This implementation is based on that description.
  (let ((special-chars ".[{()\*+?|^$"))
    (with-output-to-string
      (loop for char across string
            do
            (when (position char special-chars) (write-char ?\\))
            (write-char char)))))

(defun xmtn--perl-regexp-for-extension (extension)
  (format "\\.%s$" (xmtn--quote-string-as-partial-perl-regexp extension)))

(defun xmtn--perl-regexp-for-file-name (file-name)
  (format "^%s$" (xmtn--quote-string-as-partial-perl-regexp file-name)))

(defun xmtn--perl-regexp-for-files-in-directory (directory-file-name)
  (format "^%s" (xmtn--quote-string-as-partial-perl-regexp
                 (file-name-as-directory directory-file-name))))

(defun xmtn--perl-regexp-for-extension-in-dir (file-name)
  (format "^%s.*\\.%s$"
          (xmtn--quote-string-as-partial-perl-regexp
           (file-name-directory file-name))
          (xmtn--quote-string-as-partial-perl-regexp
           (file-name-extension file-name))))

(defun xmtn--add-patterns-to-mtnignore (root patterns interactive-p)
    (save-window-excursion
      ;; use 'find-file-other-window' to preserve current state if
      ;; user is already visiting the ignore file.
      (find-file-other-window (xmtn--mtnignore-file-name root))
      (save-excursion
        (let ((modified-p nil))
          (loop for pattern in patterns
                do
                (goto-char (point-min))
                (unless (re-search-forward (concat "^" (regexp-quote pattern)
                                                   "$")
                                           nil t)
                  (goto-char (point-max))
                  (unless (bolp) (insert "\n"))
                  (insert pattern "\n")
                  (setq modified-p t)))
          (when modified-p
            ;; 'sort-lines' moves all markers, which defeats save-excursion. Oh well!
            (sort-lines nil (point-min) (point-max))
            (if (and interactive-p
                     dvc-confirm-ignore)
                (lexical-let ((buffer (current-buffer)))
                  (save-some-buffers nil (lambda ()
                                           (eql (current-buffer) buffer))))
              (save-buffer))))))
    nil)

;;;###autoload
(defun xmtn-dvc-ignore-files (file-names)
  (assert (not (endp file-names)))
  (let* ((root (dvc-tree-root))
         (normalized-file-names (xmtn--normalize-file-names root file-names))
         (msg (case (length file-names)
                (1 (format "%s" (first normalized-file-names)))
                (t (format "%s files/directories"
                           (length normalized-file-names))))))
    (when (or (not dvc-confirm-ignore)
              (y-or-n-p (format "Ignore %s in monotone tree %s? " msg root)))
      (xmtn--add-patterns-to-mtnignore
       root
       (let ((default-directory root))
         (mapcan (lambda (file-name)
		   (list (xmtn--perl-regexp-for-file-name file-name)))
                 normalized-file-names))
       t))))

;;;###autoload
(defun xmtn-dvc-backend-ignore-file-extensions (extensions)
  (xmtn--add-patterns-to-mtnignore
   (dvc-tree-root)
   (mapcar #'xmtn--perl-regexp-for-extension extensions)
   t))

;;;###autoload
(defun xmtn-dvc-backend-ignore-file-extensions-in-dir (file-list)
  (xmtn--add-patterns-to-mtnignore
   (dvc-tree-root)
   (mapcar #'xmtn--perl-regexp-for-extension-in-dir file-list)
   t))

(defun xmtn--add-files (root file-names)
  (dolist (file-name file-names)
    ;; I don't know how mtn handles symlinks (and symlinks to
    ;; directories), so forbid them for now.
    (assert (not (file-symlink-p file-name))))
  (setq file-names (xmtn--normalize-file-names root file-names))
  (xmtn--run-command-sync root
                          `("add" "--" ,@file-names)))

;;;###autoload
(defun xmtn-dvc-add-files (&rest files)
  (xmtn--add-files (dvc-tree-root) files))

;; Appears redundant, given that there is `xmtn-dvc-add-files'.  But
;; it's part of the DVC API.
;;;###autoload
(defun xmtn-dvc-add (file)
  (xmtn--add-files (dvc-tree-root) (list file)))

(defun xmtn--do-remove (root file-names do-not-execute)
  (xmtn--run-command-sync
   root `("drop"
          ,@(if do-not-execute `("--bookkeep-only") `())
          "--" ,@(xmtn--normalize-file-names root file-names)))
  ;; return t to indicate we succeeded
  t)

;;;###autoload
(defun xmtn-dvc-remove-files (&rest files)
  (xmtn--do-remove (dvc-tree-root) files nil))

;;;###autoload
(defun xmtn-dvc-rename (from-name to-name bookkeep-only)
  ;; See `dvc-rename' for doc string.
  (let ((root (dvc-tree-root)))
    (let ((to-normalized-name (xmtn--normalize-file-name root to-name))
          (from-normalized-name (xmtn--normalize-file-name root from-name)))
      (xmtn--run-command-sync
       root `("rename"
              ,@(if bookkeep-only `("--bookkeep-only") `())
              "--" ,from-normalized-name ,to-normalized-name))))
  ;; FIXME: We should do something analogous to
  ;; `dvc-revert-some-buffers' (but for renaming) here.  But DVC
  ;; doesn't provide a function for that.
  )

(defun xmtn--insert-hint-into-process-buffer (string)
  (let ((inhibit-read-only t)
        deactivate-mark)
    (save-excursion
      (let ((start (point)))
        (insert string)
        (let ((end (1- (point))))
          (add-text-properties start end '(face (:slant italic))))))))

(defun xmtn--run-command-that-might-invoke-merger (root command post-process)
  ;; Run async, not sync; it might recursively invoke emacsclient for
  ;; merging; and we might need to send an enter keystroke when
  ;; finished.
  (lexical-let ((post-process post-process))
    (xmtn--run-command-async
     root command
     :finished
     (lambda (output error status arguments)
       (with-current-buffer output
         (save-excursion
           (goto-char (point-max))
           (xmtn--insert-hint-into-process-buffer "[process finished]\n")))
       (if post-process
           (funcall post-process)))
     :error
     (lambda (output error status arguments)
       (with-current-buffer output
         (save-excursion
           (goto-char (point-max))
           (xmtn--insert-hint-into-process-buffer
            "[process terminated with an error]\n")
           (dvc-show-error-buffer error))))))
  ;; Show process buffer.  Monotone might spawn an external merger and
  ;; ask the user to hit enter when finished.
  (dvc-show-process-buffer)
  (goto-char (point-min))
  (xmtn--insert-hint-into-process-buffer
   (substitute-command-keys
    (concat
     "This buffer will show the output of the mtn subprocess, if any."
     "\nTo send an \"enter\" keystroke to mtn, use"
     " \\[xmtn-send-enter-to-subprocess]"
     "\nin this buffer.  This might be necessary"
     " if mtn launches an external merger."
     "\nWhen mtn has finished, just bury this buffer, or kill it."
     "\n")))
  (goto-char (point-max))
  ;; I don't think DVC's process filter can deal with read-only
  ;; buffers yet.
  ;;(setq buffer-read-only t)
  )

;;;###autoload
(defun xmtn-send-enter-to-subprocess ()
  "Send an \"enter\" keystroke to a monotone subprocess.

To be used in an xmtn process buffer.  Useful when monotone
spawns an external merger and asks you to hit enter when
finished."
  (interactive)
  (let ((process (loop for (process nil) in dvc-process-running
                       when (eql (current-buffer) (process-buffer process))
                       return process)))
    (unless process
      (error "No active process for buffer %s found" (current-buffer)))
    (process-send-string process "\n")
    (save-excursion
      (goto-char (point-max))
      (xmtn--insert-hint-into-process-buffer "[sent enter keystroke]\n"))))

;;; It's kind of a wart that these "xmtn--do-<operation>" functions
;;; don't have the same contract with respect to
;;; synchronousness/asynchronousness, progress messages and return
;;; value.

(defun xmtn--do-update (root target-revision-hash-id post-update-p)
  (check-type root string)
  (check-type target-revision-hash-id xmtn--hash-id)
  (lexical-let ((progress-message (format "Updating tree %s to revision %s"
                                          root target-revision-hash-id))
                (post-update-p post-update-p))
    (let ((command `("update" "--move-conflicting-paths" ,(concat "--revision=" target-revision-hash-id)))
          (post-process
           (lambda ()
             (message "%s... done" progress-message)
             (if post-update-p
                 (progn
                   (dvc-revert-some-buffers default-directory)
                   (dvc-diff-clear-buffers 'xmtn
                                           default-directory
                                           "* Just updated; please refresh buffer"
                                           (xmtn--status-header
                                            default-directory
                                            (xmtn--get-base-revision-hash-id-or-null default-directory)))))))
          )

      (message "%s..." progress-message)
      ;; this used to have an option to call '--might-invoke-merger'; could be simplified.
      (xmtn--run-command-sync root command)
      (funcall post-process))
    nil))

(defun xmtn--update (root target-revision-hash-id check-id-p no-ding)
  ;; mtn will just give an innocuous message if already updated, which
  ;; the user won't see. So check that here - it's fast.
  ;; Don't throw an error; upper level might be doing other directories as well.
  (if (and check-id-p
           (equal (xmtn--get-base-revision-hash-id-or-null root) target-revision-hash-id))
      (progn
        (unless no-ding (ding))
        (message "Tree %s is already based on target revision %s"
                 root target-revision-hash-id))
    (dvc-save-some-buffers root)
    (xmtn--do-update root target-revision-hash-id check-id-p)))

;;;###autoload
(defun xmtn-dvc-update (&optional revision-id no-ding)
  (let ((root (dvc-tree-root)))
    (if revision-id
        (xmtn--update root (xmtn--revision-hash-id revision-id) t no-ding)

      (let* ((branch (xmtn--tree-default-branch root))
             (heads (xmtn--heads root branch)))
        (case (length heads)
          (0
           (error "branch %s has no revisions" branch))

          (1
           (xmtn--update root (first heads) t no-ding))

          (t
           ;; User can choose one head from a revlist, or merge them.
           (error (substitute-command-keys
                   (concat "Branch %s is unmerged (%s heads)."
                           "  Try \\[xmtn-view-heads-revlist] and \\[dvc-merge] or \\[dvc-revlist-update]"))
                  branch (length heads)))))))
  nil)

(defun xmtn-propagate-from (other &optional cached-branch)
  "Propagate from OTHER branch to CACHED-BRANCH (default local tree branch).
Conflict resolution taken from `default-directory', which must be
a workspace for CACHED-BRANCH."
  (interactive "MPropagate from branch: ")
  (let*
      ((root (dvc-tree-root))
       (local-branch (or cached-branch
                         (xmtn--tree-default-branch root)))
       (resolve-conflicts
        (if (file-exists-p (concat root "/_MTN/conflicts"))
            (progn
              "--resolve-conflicts-file=_MTN/conflicts")))
       (cmd (list "propagate" other local-branch resolve-conflicts
		  ;; may be resurrecting a suspended branch; doesn't hurt otherwise.
		  "--ignore-suspend-certs"
                  (xmtn-dvc-log-message)))
       (prompt
        (if resolve-conflicts
            (concat "Propagate from " other " to " local-branch " resolving conflicts? ")
          (concat "Propagate from " other " to " local-branch "? "))))

    (save-some-buffers t); conflicts file may be open.

    (if xmtn-confirm-operation
        (if (not (yes-or-no-p prompt))
            (error "user abort")))

    (lexical-let
        ((display-buffer (current-buffer))
         (msg (mapconcat (lambda (item) item) cmd " ")))
      (message "%s..." msg)
      (if xmtn-confirm-operation
          (xmtn--run-command-that-might-invoke-merger
           root cmd
           (lambda ()
             (xmtn--refresh-status-header display-buffer)
             (message "%s... done" msg)))
        (xmtn--run-command-sync root cmd)
        (xmtn--refresh-status-header display-buffer)
        (message "%s... done" msg)))))

(defun xmtn-dvc-merge-1 (root refresh-status)
  (xmtn--run-command-sync
   root
   (list
    "merge"
    (if (file-exists-p (concat root "/_MTN/conflicts"))
        "--resolve-conflicts-file=_MTN/conflicts")
    (xmtn-dvc-log-message)))
  (if refresh-status
      (xmtn--refresh-status-header (current-buffer))))

;;;###autoload
(defun xmtn-dvc-merge (&optional other)
  (if other
      (xmtn-propagate-from other)
    ;; else merge heads
    (let* ((root (dvc-tree-root))
           (branch (xmtn--tree-default-branch root))
           (heads (xmtn--heads root branch)))
      (case (length heads)
        (0 (assert nil))
        (1
         (message "already merged"))
        (t
         (xmtn-dvc-merge-1 root t)))))
  nil)

;;;###autoload
(defun xmtn-dvc-pull (&optional other)
  "Implement `dvc-pull' for xmtn."
  (lexical-let*
      ((root (dvc-tree-root))
       (name (concat "mtn pull " root)))
    (message "%s..." name)
    ;; mtn progress messages are put to stderr, and there is typically
    ;; nothing written to stdout from this command, so put both in the
    ;; same buffer.
    ;; This output is not useful; xmtn-sync, xmtn-sync-review is much better
    (xmtn--run-command-async root `("pull" ,other)
                             :output-buffer name
                             :error-buffer name
                             :finished
                             (lambda (output error status arguments)
                               (pop-to-buffer output)
                               (message "%s... done" name)))))

;;;###autoload
(defun xmtn-dvc-revert-files (&rest file-names)
  (when (stringp file-names) (setq file-names (list file-names)))
  (let ((root (dvc-tree-root)))
    (assert (not (endp file-names)))
    (dvc-save-some-buffers root)
    (let ((normalized-file-names (xmtn--normalize-file-names root file-names))
          (progress-message
           (if (eql (length file-names) 1)
               (format "Reverting file %s" (first file-names))
             (format "Reverting %s files" (length file-names)))))
      (message "%s..." progress-message)
      (xmtn--run-command-sync root `("revert" "--"
                                     ,@normalized-file-names))
      (message "%s... done" progress-message))
    (dvc-revert-some-buffers root))
  nil)

;;;###autoload
(defun xmtn-revision-get-previous-revision (file revision-id)
  (xmtn--revision-get-file-helper file (list 'previous-revision (cadr revision-id))))

;;;###autoload
(defun xmtn-revision-get-last-revision (file stuff)
  (xmtn--revision-get-file-helper file `(last-revision ,@stuff)))

;;;###autoload
(defun xmtn-revision-get-file-revision (file stuff)
  (xmtn--revision-get-file-helper file `(revision ,@stuff)))

(defun xmtn--revision-get-file-helper (file backend-id)
  "Fill current buffer with the contents of FILE in revision BACKEND-ID."
  (let ((root (dvc-tree-root)))
    (let ((normalized-file (xmtn--normalize-file-name root file))
          (temp-dir nil))
      (unwind-protect
          (progn
            (setq temp-dir (make-temp-file
                            "xmtn--revision-get-file-" t))
            ;; Going through a temporary file and using
            ;; `insert-file-contents' in conjunction with as
            ;; much of the original file name as possible seems
            ;; to be the best way to make sure that Emacs'
            ;; entire file coding system detection logic is
            ;; applied.  Functions like
            ;; `find-operation-coding-system' and
            ;; `find-file-name-handler' are not a complete
            ;; replacement since they don't look at the contents
            ;; at all.
            (let ((temp-file (concat temp-dir "/" normalized-file)))
              (make-directory (file-name-directory temp-file) t)
              (with-temp-file temp-file
                (set-buffer-multibyte nil)
                (setq buffer-file-coding-system 'binary)
                (xmtn--insert-file-contents-by-name root backend-id normalized-file (current-buffer)))
              (let ((output-buffer (current-buffer)))
                (with-temp-buffer
                  (insert-file-contents temp-file)
                  (let ((input-buffer (current-buffer)))
                    (with-current-buffer output-buffer
                      (insert-buffer-substring input-buffer)))))))
        (when temp-dir
          (dvc-delete-recursively temp-dir))))))

(defun xmtn--get-file-by-id (root file-id save-as)
  "Store contents of FILE-ID in file SAVE-AS."
  (with-temp-file save-as
    (set-buffer-multibyte nil)
    (setq buffer-file-coding-system 'binary)
    (xmtn--insert-file-contents root file-id (current-buffer))))

(defun xmtn--limit-length (list n)
  (or (null n) (<= (length list) n)))

(defun xmtn--get-corresponding-path (root normalized-file-name
                                          source-revision-backend-id
                                          target-revision-backend-id)
  ;; normalized-file-name is a file in
  ;; source-revision-backend-id. Return its name in
  ;; target-revision-backend-id.
  (block get-corresponding-path
    (let (source-revision-hash-id
          target-revision-hash-id
          (file-name-postprocessor #'identity))
      (let ((resolved-source-revision
             (xmtn--resolve-backend-id root source-revision-backend-id))
            (resolved-target-revision
             (xmtn--resolve-backend-id root target-revision-backend-id)))
        (xmtn-match resolved-source-revision
          ((revision $hash-id)
           (setq source-revision-hash-id hash-id))
          ((local-tree $path)
           (let ((base-revision-hash-id
                  (xmtn--get-base-revision-hash-id-or-null path)))
             (if (null base-revision-hash-id)
                 (xmtn-match resolved-target-revision
                   ((revision $hash-id)
                    (return-from get-corresponding-path nil))
                   ((local-tree $target-path)
                    (return-from get-corresponding-path normalized-file-name)))
               ;; Handle an uncommitted rename in the current workspace
               (setq normalized-file-name (xmtn--get-rename-in-workspace-to
                                           path normalized-file-name))
               (setq source-revision-hash-id base-revision-hash-id)))))

        (xmtn-match resolved-target-revision
          ((revision $hash-id)
           (setq target-revision-hash-id hash-id))
          ((local-tree $path)
           (let ((base-revision-hash-id
                  (xmtn--get-base-revision-hash-id-or-null path)))
             (if (null base-revision-hash-id)
                 (return-from get-corresponding-path nil)
               (setq target-revision-hash-id base-revision-hash-id)
               ;; Handle an uncommitted rename in the current workspace
               (setq file-name-postprocessor
                     (lexical-let ((path path))
                       (lambda (file-name)
                         (xmtn--get-rename-in-workspace-from path
                                                             file-name)))))))))
      (let ((result
             (xmtn--get-corresponding-path-raw root normalized-file-name
                                               source-revision-hash-id
                                               target-revision-hash-id)))
        (if (null result)
            nil
          (funcall file-name-postprocessor result))))))

(defun xmtn--get-rename-in-workspace-from (root normalized-source-file-name)
  ;; Given a workspace ROOT and a file name
  ;; NORMALIZED-SOURCE-FILE-NAME in the base revision of the
  ;; workspace, return the current name of that file in the workspace.
  ;; FIXME: need a better way to implement this
  (check-type normalized-source-file-name string)
  (block parse
    (xmtn--with-automate-command-output-basic-io-parser
        (parser root `("inventory"))
      (xmtn--parse-inventory parser
                             (lambda (path status changes old-path new-path
                                           old-type new-type fs-type)
                               (when (equal normalized-source-file-name
                                            old-path)
                                 (return-from parse
                                   path)))))
    normalized-source-file-name))

(defun xmtn--get-rename-in-workspace-to (root normalized-target-file-name)
  ;; Given a workspace ROOT and a file name
  ;; NORMALIZED-TARGET-FILE-NAME in the current revision of the
  ;; workspace, return the name of that file in the base revision of
  ;; the workspace.
  ;; FIXME: need a better way to implement this
  (check-type normalized-target-file-name string)
  (block parse
    (xmtn--with-automate-command-output-basic-io-parser
        (parser root `("inventory" ,normalized-target-file-name))
      (xmtn--parse-inventory parser
                             (lambda (path status changes old-path new-path
                                           old-type new-type fs-type)
                               (when (and old-path
                                          (equal normalized-target-file-name
                                                 path))
                                 (return-from parse
                                   old-path)))))
    normalized-target-file-name))

(defun xmtn--file-contents-as-string (root content-hash-id)
  (check-type content-hash-id xmtn--hash-id)
  (xmtn-automate-command-output-string
   root `("get_file" ,content-hash-id)))

(defstruct (xmtn--revision (:constructor xmtn--make-revision))
  ;; matches data output by 'mtn diff'
  new-manifest-hash-id
  old-revision-hash-ids
  delete
  rename
  add-dir
  add-file
  patch-file
  clear-attr
  set-attr
  )

(defun xmtn--parse-partial-revision (parser)
  "Parse basic_io output from get_revision, starting with the old_revision stanzas."
  (let ((old-revision-hash-ids (list))
        (delete (list))
        (rename (list))
        (add-dir (list))
        (add-file (list))
        (patch-file (list))
        (clear-attr (list))
        (set-attr (list)))
    (flet ((decode-path (path)
             (decode-coding-string path 'xmtn--monotone-normal-form)))
      (loop for stanza = (funcall parser)
            while stanza
            do
            (xmtn-match stanza
              ;; Most common case, "patch", first.
              ((("patch" (string $filename))
                ("from" (id $from-id))
                ("to" (id $to-id)))
               (push `(,(decode-path filename) ,from-id ,to-id)
                     patch-file))
              ((("old_revision" (null-id)))
               ;; Why doesn't mtn just skip this stanza?
               )
              ((("old_revision" (id $hash-id)))
               (push hash-id old-revision-hash-ids))
              ((("delete" (string $path)))
               (push `(,(decode-path path)) delete))
              ((("rename" (string $from-path))
                ("to" (string $to-path)))
               (push `(,(decode-path from-path) ,(decode-path to-path))
                     rename))
              ((("add_dir" (string $path)))
               (push `(,(decode-path path)) add-dir))
              ((("add_file" (string $path))
                ("content" (id $file-id)))
               (push `(,(decode-path path) ,file-id)
                     add-file))
              ;; "patch": See above.
              ((("clear" (string $path))
                ("attr" (string $attr-name)))
               (push `(,(decode-path path) ,attr-name)
                     clear-attr))
              ((("set" (string $path))
                ("attr" (string $attr-name))
                ("value" (string $attr-value)))
               (push `(,(decode-path path) ,attr-name ,attr-value)
                     set-attr)))))
    (setq old-revision-hash-ids (nreverse old-revision-hash-ids)
          delete (nreverse delete)
          rename (nreverse rename)
          add-dir (nreverse add-dir)
          add-file (nreverse add-file)
          patch-file (nreverse patch-file)
          clear-attr (nreverse clear-attr)
          set-attr (nreverse set-attr))
    (xmtn--make-revision
     :old-revision-hash-ids old-revision-hash-ids
     :delete delete
     :rename rename
     :add-dir add-dir
     :add-file add-file
     :patch-file patch-file
     :clear-attr clear-attr
     :set-attr set-attr
     )))


;;;###autoload
(defun xmtn-dvc-revision-nth-ancestor (&rest args)
  ;; There is a reasonable default implementation to fall back on.  It
  ;; will just call `xmtn-dvc-revision-direct-ancestor' N times.  We
  ;; can't do any better than linear-time anyway, since we have to
  ;; chase the ancestry links (and check the uniqueness at each step).
  (apply #'dvc-dvc-revision-nth-ancestor args))

(defalias 'xmtn-dvc-revlist 'xmtn-view-heads-revlist)

(provide 'xmtn-dvc)

;;; xmtn-dvc.el ends here
