;;; xmtn-sync.el --- database sync handling for DVC backend for monotone
;;
;; Copyright (C) 2010, 2011 Stephen Leake
;;
;; Author: Stephen Leake
;; Keywords: tools
;;
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

(eval-when-compile
  ;; these have macros we use
  )

(eval-and-compile
  ;; these have functions (and possibly macros) we use
  (require 'dvc-config)
  (require 'xmtn-automate)
  (require 'xmtn-basic-io)
  )

;;; User variables
(defvar xmtn-sync-executable
  (cond
    ((equal system-type 'windows-nt)
     ;; Native MinGW does not support file: or ssh: - assume Cygwin is
     ;; installed, but not first in path
     "c:/bin/mtn")
    (t
     ;; Unix or Cygwin; assume mtn is in path
     "mtn"))
  "Executable for running sync command on local db; overrides xmtn-executable.")

(defvar xmtn-sync-automate-args
  (cond
    ((equal system-type 'windows-nt)
     ;; Assume using Cygwin, which looks for .monotone/keys in a different place.
     (list "--keydir" "~/.monotone/keys"))
    (t
     ;; Unix or Cygwin
     nil))
  "Extra arguments (list of strings) used when starting a sync automate process;
overrides xmtn-automate-arguments.")

(defvar xmtn-sync-guess-workspace nil
  "User-supplied function to guess workspace location given branch.
Called with a string containing the mtn branch name; return a workspace root or nil.")

(defvar xmtn-sync-sort nil
  "User-supplied function to sort branches.
Called with a string containing the mtn branch name; return
'(node key) where node is the ewoc node to insert before (nil to
insert at end), key is the sort-key. Sync buffer is current.")

;;; Internal variables
(defconst xmtn-sync-save-file "sync"
  "File to save sync review state for later; relative to `dvc-config-directory'.")

(defconst xmtn-sync-review-file "sync.basic_io"
  "File to save shell sync basic_io output for input by `xmtn-sync-review'; relative to `dvc-config-directory'.")

(defconst xmtn-sync-branch-file "branches"
  "File associating branch name with workspace root; relative to `dvc-config-directory'.")

(defconst xmtn-sync-config "xmtn-sync-config"
  "File to store `xmtn-sync-branch-alist'; relative to `dvc-config-directory'.")

(defconst xmtn-sync-required-command-version '(0 99)
  ;; Sometimes the Cygwin version lags behind the MinGW version; this allows that.
  "Minimum version for `xmtn-sync-executable'; overrides xmtn--minimum-required-command-version.
Must support file:, ssh:, automate sync.")

;; loaded from xmtn-sync-config
(defvar xmtn-sync-branch-alist nil
  "Alist associating branch name with workspace root")

(defvar xmtn-sync-remote-exec-alist nil
  "Alist of host and remote command. Overrides `xmtn-sync-remote-exec-default'.")

;; buffer-local
(defvar xmtn-sync-local-db nil
  "Absolute path to local database.")
(make-variable-buffer-local 'xmtn-sync-local-db)

(defvar xmtn-sync-remote-db nil
  "Absolute path to remote database.")
(make-variable-buffer-local 'xmtn-sync-remote-db)

(defvar xmtn-sync-ewoc nil
  "Buffer-local ewoc for displaying sync.
All xmtn-sync functions operate on this ewoc.
The elements must all be of type xmtn-sync-sync.")
(make-variable-buffer-local 'xmtn-sync-ewoc)

(defstruct (xmtn-sync-branch
            (:copier nil))
  ;; ewoc element; data for a branch that was received
  name ;; monotone branch name
  rev-alist ;; alist of '(revid (date author changelog)) for received revs
  send-count ;; integer count of sent revs
  print-mode ;; 'summary | 'brief | 'full | 'started
  sort-key ;; for use by xmtn-sync-sort
  )

(defun xmtn-sync-print-rev (rev print-mode)
  "Print a REV (element of branch rev-alist) according to PRINT-MODE ('brief or 'full)."
  (let ((date (nth 0 (cadr rev)))
	(author (nth 1 (cadr rev)))
	(changelog (nth 2 (cadr rev))))
    (insert (dvc-face-add (format "\n   %s %s\n" date author) 'dvc-header))
    (ecase print-mode
      (brief
       (insert (substring changelog 0 (string-match "\n" changelog))))
      (full
       (insert changelog)))))

(defun xmtn-sync-printer (branch)
  "Print an ewoc element; BRANCH must be of type xmtn-sync-branch."
  ;; sometimes mtn will allow a revision with no branch!
  (if (xmtn-sync-branch-name branch)
      (insert (dvc-face-add (xmtn-sync-branch-name branch) 'dvc-keyword))
    (insert (dvc-face-add "<no branch>" 'dvc-keyword)))
  (insert (format " rx %d tx %d\n"
		  (length (xmtn-sync-branch-rev-alist branch))
		  (xmtn-sync-branch-send-count branch)))
  (ecase (xmtn-sync-branch-print-mode branch)
    (summary nil)

    ((brief full)
     (loop for rev in (xmtn-sync-branch-rev-alist branch) do
	(xmtn-sync-print-rev rev (xmtn-sync-branch-print-mode branch))))

    (started
     (insert " started\n")))
  )

(defun xmtn-sync-brief ()
  "Set display mode for current item to brief."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-sync-ewoc))
	 (data (ewoc-data elem)))
    (setf (xmtn-sync-branch-print-mode data) 'brief)
    (ewoc-invalidate xmtn-sync-ewoc elem)))

(defun xmtn-sync-full ()
  "Set display mode for current item to full."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-sync-ewoc))
	 (data (ewoc-data elem)))
    (setf (xmtn-sync-branch-print-mode data) 'full)
    (ewoc-invalidate xmtn-sync-ewoc elem)))

(defun xmtn-sync-summary ()
  "Set display mode for current item to summary."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-sync-ewoc))
	 (data (ewoc-data elem)))
    (setf (xmtn-sync-branch-print-mode data) 'summary)
    (ewoc-invalidate xmtn-sync-ewoc elem)))

(defun xmtn-sync-status ()
  "Start xmtn-status-one for current ewoc element."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-sync-ewoc))
	 (data (ewoc-data elem))
         (branch (xmtn-sync-branch-name data))
	 save-work
         (work (or
		(cadr (assoc branch xmtn-sync-branch-alist))
		(if (functionp xmtn-sync-guess-workspace)
		    (funcall xmtn-sync-guess-workspace branch))
		(prog1
		  (read-directory-name (format "workspace root for %s: " branch))
		  (setq save-work t))
		)))
    (setf (xmtn-sync-branch-print-mode data) 'started) ; indicate we've started work on it
    (ewoc-invalidate xmtn-sync-ewoc elem)

    (condition-case err
	(xmtn-status-one work)
      ('error
       (if (and (not save-work) (functionp xmtn-sync-guess-workspace))
	   ;; xmtn-sync-guess-workspace guessed wrong; prompt and try again
	   (progn
	     (setq work (read-directory-name (format "workspace root for %s: " branch)))
	     (setq save-work t)
	     (xmtn-status-one work)))))

    ;; don't save the workspace association until it is validated by xmtn-status-one
    (if save-work
	(progn
	  (push (list branch work) xmtn-sync-branch-alist)
	  (dvc-save-state
	   (list 'xmtn-sync-branch-alist)
	   (expand-file-name xmtn-sync-branch-file dvc-config-directory))))))

(defun xmtn-sync-update ()
  "Start xmtn-status-on for current ewoc element, do update if possible."
  (interactive)
  (xmtn-sync-status)
  (if (xmtn-status-updatep)
      (xmtn-status-update)))

(defun xmtn-sync-clean ()
  "Clean and delete current ewoc element."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-sync-ewoc))
	 (status-buffer (get-buffer-create "*xmtn-multi-status*"))
         (inhibit-read-only t))
    (if (buffer-live-p status-buffer)
	(kill-buffer status-buffer))
    (ewoc-delete xmtn-sync-ewoc elem)))

(dvc-make-ewoc-next xmtn-sync-next xmtn-sync-ewoc)
(dvc-make-ewoc-prev xmtn-sync-prev xmtn-sync-ewoc)

(defvar xmtn-sync-kbd-map
  (let ((map (make-sparse-keymap "action")))
    ;; last defined is first in displayed menu
    (define-key map [?c]  '(menu-item "c) clean" xmtn-sync-clean))
    (define-key map [?f]  '(menu-item "f) full" xmtn-sync-full))
    (define-key map [?b]  '(menu-item "b) brief" xmtn-sync-brief))
    (define-key map [?s]  '(menu-item "s) status" xmtn-sync-status))
    (define-key map [?u]  '(menu-item "u) update" xmtn-sync-update))
    map)
  "Keyboard menu keymap used in `xmtn-sync-mode'.")

(defvar xmtn-sync-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-d" xmtn-sync-kbd-map)
    (define-key map [?b]  'xmtn-sync-brief)
    (define-key map [?c]  'xmtn-sync-clean)
    (define-key map [?f]  'xmtn-sync-full)
    (define-key map [?n]  'xmtn-sync-next)
    (define-key map [?p]  'xmtn-sync-prev)
    (define-key map [?q]  'dvc-buffer-quit)
    (define-key map [?s]  'xmtn-sync-status)
    (define-key map [?u]  'xmtn-sync-update)
    (define-key map [?S]  'xmtn-sync-save)
    map)
  "Keymap used in `xmtn-sync-mode'.")

(easy-menu-define xmtn-sync-mode-menu xmtn-sync-mode-map
  "`xmtn-sync' menu"
  `("Xmtn-sync"
    ;; first item is top in display
    ["Status"        xmtn-sync-status t]
    ["Update"        xmtn-sync-update t]
    ["Brief display" xmtn-sync-brief t]
    ["Full display"  xmtn-sync-full t]
    ["Clean/delete"  xmtn-sync-clean t]
    ["Save"          xmtn-sync-save t]
    ["Save and Quit" (lambda () (kill-buffer (current-buffer))) t]
    ))

(define-derived-mode xmtn-sync-mode fundamental-mode "xmtn-sync"
  "Major mode to specify conflict resolutions."
  (setq dvc-buffer-current-active-dvc 'xmtn)
  (setq xmtn-sync-ewoc (ewoc-create 'xmtn-sync-printer))
  (setq dvc-buffer-refresh-function nil)
  (dvc-install-buffer-menu)
  (add-hook 'kill-buffer-hook 'xmtn-sync-save nil t)
  (buffer-disable-undo)
  (unless xmtn-sync-branch-alist
    (let ((branch-file (expand-file-name xmtn-sync-branch-file dvc-config-directory)))
      (if (file-exists-p branch-file)
	  (load branch-file)))))

(defun xmtn-sync-parse-revision-certs (direction)
  "Parse certs associated with a revision; return (branch changelog date author)."
  (let ((keyword (ecase direction
		   ('receive "receive_cert")
		   ('send    "send_cert")))
	cert-label branch date author changelog old-branch)
    (while (xmtn-basic-io-optional-line keyword (setq cert-label (cadar value)))
      (cond
       ((string= cert-label "branch")
	(xmtn-basic-io-check-line "value" (setq branch (cadar value)))
	(xmtn-basic-io-skip-line "key")
	(xmtn-basic-io-skip-line "revision"))

       ((string= cert-label "changelog")
	(xmtn-basic-io-check-line "value" (setq changelog (cadar value)))
	(xmtn-basic-io-skip-line "key")
	(xmtn-basic-io-skip-line "revision"))

       ((string= cert-label "date")
	(xmtn-basic-io-check-line "value" (setq date (cadar value)))
	(xmtn-basic-io-skip-line "key")
	(xmtn-basic-io-skip-line "revision"))

       ((string= cert-label "author")
	(xmtn-basic-io-check-line "value" (setq author (cadar value)))
	(xmtn-basic-io-skip-line "key")
	(xmtn-basic-io-skip-line "revision"))

       (t
	;; ignore other certs
	(xmtn-basic-io-skip-stanza))
       )
      (xmtn-basic-io-skip-blank-lines) ;; might be at end of parsing region
      ) ;; end while cert

    (list branch changelog date author)))

(defun xmtn-sync-enter-rev (revid branch date author changelog direction)
  "Enter data for REVID into ewoc."
  (let (old-branch)
    (ewoc-map
     (lambda (data)
       (if (string= branch (xmtn-sync-branch-name data))
	   ;; already some data for branch
	   (let ((rev-alist (xmtn-sync-branch-rev-alist data)))
	     (ecase direction
	       ('receive
		(setf (xmtn-sync-branch-rev-alist data)
		      ;; sync sends revs newest first, we want newest
		      ;; displayed last, so append to head of list
		      (push (list revid (list date author changelog)) rev-alist)))
	       ('send
		(setf (xmtn-sync-branch-send-count data) (+ 1 (xmtn-sync-branch-send-count data)))))
	     (setq old-branch t)
	     t; update ewoc
	     )))
     xmtn-sync-ewoc)

    (if (not old-branch)
	(let*
	    ((node-key (and (functionp xmtn-sync-sort)
			    (funcall xmtn-sync-sort branch)))
	     (data
	      (ecase direction
		('receive
		 (make-xmtn-sync-branch
		  :name branch
		  :rev-alist (list (list revid (list date author changelog)))
		  :send-count 0
		  :print-mode 'summary
		  :sort-key (nth 1 node-key)))
		('send
		 (make-xmtn-sync-branch
		  :name branch
		  :rev-alist nil
		  :send-count 1
		  :print-mode 'summary
		  :sort-key (nth 1 node-key))))))
	  (if (nth 0 node-key)
	      (ewoc-enter-before xmtn-sync-ewoc (nth 0 node-key) data)
	    (ewoc-enter-last xmtn-sync-ewoc data))
	  ))))

(defun xmtn-sync-parse-revisions (direction)
  "Parse revisions with associated certs."
  (let ((keyword (ecase direction
		   ('receive "receive_revision")
		   ('send    "send_revision")))
	revid)
    (while (xmtn-basic-io-optional-line keyword (setq revid (cadar value)))
      (xmtn-basic-io-skip-blank-lines)
      (let* ((cert-values (xmtn-sync-parse-revision-certs direction))
	     (branch (nth 0 cert-values))
	     (changelog (nth 1 cert-values))
	     (date (nth 2 cert-values))
	     (author (nth 3 cert-values)))

	(xmtn-sync-enter-rev revid branch date author changelog direction)))))

(defun xmtn-sync-parse-certs (direction)
  "Parse certs not associated with revisions."
  (let ((keyword (ecase direction
		   ('receive "receive_cert")
		   ('send    "send_cert")))
	revid
	cert-label
	branch
	(date "")
	(author "")
	(changelog "create or propagate branch\n")
	old-branch)

    (while (xmtn-basic-io-optional-line keyword (setq cert-label (cadar value)))
      (cond
       ((string= cert-label "branch")
	;; This happens when a new branch is created, or a branch is
	;; propagated without any conflicts.
	(xmtn-basic-io-check-line "value" (setq branch (cadar value)))
	(xmtn-basic-io-skip-line "key")
	(xmtn-basic-io-check-line "revision" (setq revid (cadar value)))

	(xmtn-sync-enter-rev revid branch date author changelog direction))

       (t
	;; ignore other certs
	(xmtn-basic-io-skip-stanza))
       )

      ;; move to next stanza or end of parsing region
      (xmtn-basic-io-skip-blank-lines)

      )))

(defun xmtn-sync-parse-keys (direction)
  ;; just ignore all keys
  (let ((keyword (ecase direction
		   ('receive "receive_key")
		   ('send    "send_key"))))
    (xmtn-basic-io-skip-blank-lines)
    (while (xmtn-basic-io-optional-skip-line keyword))))

(defun xmtn-sync-parse (begin)
  "Parse current buffer starting at BEGIN, fill in `xmtn-sync-ewoc' in current buffer, erase parsed text.
Return non-nil if anything parsed."
  (set-syntax-table xmtn-basic-io--*syntax-table*)
  (goto-char begin)

  ;; receive_cert "branch"
  ;;        value "foo2"
  ;;          key [46ec58576f9e4f34a9eede521422aa5fd299dc50]
  ;;     revision [e4352c1d28b38e87b5040f770a66be2ec9b2362d]
  ;;
  ;; ... more unattached certs
  ;;
  ;; receive_revision [e4352c1d28b38e87b5040f770a66be2ec9b2362d]
  ;;
  ;; receive_cert "branch"
  ;;        value "foo2"
  ;;          key [46ec58576f9e4f34a9eede521422aa5fd299dc50]
  ;;     revision [...]
  ;;
  ;; receive_cert "changelog"
  ;;        value "more
  ;; "
  ;;          key [46ec58576f9e4f34a9eede521422aa5fd299dc50]
  ;;     revision [...]
  ;;
  ;; receive_cert "date"
  ;;        value "2010-09-21T08:29:11"
  ;;          key [46ec58576f9e4f34a9eede521422aa5fd299dc50]
  ;;     revision [...]
  ;;
  ;; receive_cert "author"
  ;;        value "tester@test.net"
  ;;          key [46ec58576f9e4f34a9eede521422aa5fd299dc50]
  ;;     revision [...]
  ;;
  ;;     ... more certs
  ;;
  ;; ... more revisions with certs
  ;;
  ;; receive_key
  ;;
  ;; key [46ec58576f9e4f34a9eede521422aa5fd299dc50]
  ;; key [46ec58576f9e4f34a9eede521422aa5fd299dc50]
  ;; ... more keys
  ;;
  ;; send_cert ... (unattached)
  ;;
  ;; send_revision [...]
  ;;    send_cert ...
  ;;
  ;; send_key ...

  (xmtn-sync-parse-certs 'receive)
  (xmtn-sync-parse-revisions 'receive)
  (xmtn-sync-parse-keys 'receive)
  (xmtn-sync-parse-certs 'send)
  (xmtn-sync-parse-revisions 'send)
  (xmtn-sync-parse-keys 'send)

  (let ((result (not (= begin (point)))))
    (delete-region begin (point))
    result)
  )

(defun xmtn-sync-load-file (&optional noerror)
  "Add contents of `xmtn-sync-save-file' to current ewoc."
  (let ((save-file (expand-file-name xmtn-sync-save-file dvc-config-directory))
	stuff)
    (if (file-exists-p save-file)
	(progn
	  (load save-file)
	  (setq buffer-read-only nil)
	  (dolist (data stuff) (ewoc-enter-last xmtn-sync-ewoc data))
	  (setq buffer-read-only t)
	  (set-buffer-modified-p nil)))))

;;;###autoload
(defun xmtn-sync-sync (local-db scheme remote-host remote-db)
  "Sync LOCAL-DB with using SCHEME to connect to REMOTE-HOST REMOTE-DB, display sent and received branches.
Remote-db should include branch pattern in URI syntax. Uses `xmtn-sync-executable' to run sync."
  (interactive "flocal db: \nMscheme: \nMremote-host: \nMremote-db: ")

  (pop-to-buffer (get-buffer-create "*xmtn-sync*"))
  (setq buffer-read-only nil)
  (delete-region (point-min) (point-max))

  ;; `xmtn-sync-parse' creates ewoc entries, which are inserted into
  ;; the xmtn-sync buffer. Since it is parsing the same buffer, we
  ;; need them to be inserted _after_ the text that is being
  ;; parsed. `xmtn-sync-mode' creates the ewoc at point.

  (let ((opts xmtn-sync-automate-args)
	(remote-uri (concat scheme "://" remote-host remote-db))
	(msg "Running mtn sync ..."))

    (message msg)
    (redisplay) ;; show tickers in mode-line

    ;; Remote command (if needed by scheme) is determined by a custom
    ;; version of get_netsync_connect_command; see xmtn-hooks.lua.

    (if (eq system-type 'windows-nt)
	(add-to-list 'opts
		     (concat "--rcfile=" (substring (locate-library "xmtn-hooks.lua") 2)))
      (add-to-list 'opts
		   (concat "--rcfile=" (locate-library "xmtn-hooks.lua"))))

    ;; Always use mtn executable that supports file and ssh, so we
    ;; only need one session for all syncs.
    (let ((xmtn-executable xmtn-sync-executable)
	  (xmtn--minimum-required-command-version xmtn-sync-required-command-version)
	  (xmtn-automate-arguments opts))
      (xmtn-automate-command-output-buffer
       (expand-file-name "~/sync") ; root - one session for all syncs
       (current-buffer) ; output-buffer
       (list
	(list "db" local-db) ;; options
	"sync" remote-uri) ;; command, args
       '("revisions" "revs in" "revs out") ;; display-tickers
       ))

    (message (concat msg " done"))

    (goto-char (point-max))

    ;; don't lose what was saved from last sync; may not have been reviewed yet
    (xmtn-sync-mode)
    (xmtn-sync-load-file t)

    (setq buffer-read-only nil)
    (ewoc-set-hf
     xmtn-sync-ewoc
     (concat ;; header
      (format " local db: %s\n" local-db)
      (format "remote db: %s\n" remote-uri))
     "") ;; footer

    (xmtn-sync-parse (point-min))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (xmtn-sync-save)
    ))

(defun xmtn-sync-save ()
  "Save current sync results in `xmtn-sync-save-file' for later review."
  (interactive)
  (let ((save-file (expand-file-name xmtn-sync-save-file dvc-config-directory))
	stuff)
    ;; Directly saving the ewoc doesn't work; too complicated for
    ;; pp-to-string. So we turn the ewoc into a simpler list of data
    ;; items
    (ewoc-map
     (lambda (data)
       (setq stuff (add-to-list 'stuff data t))
       nil)
     xmtn-sync-ewoc)

    (dvc-save-state
     (list 'stuff)
     (expand-file-name xmtn-sync-save-file dvc-config-directory))))

;;;###autoload
(defun xmtn-sync-review (&optional file)
  "Display sync results in FILE (defaults to `xmtn-sync-review-file'), appended to content of `xmtn-sync-save-file'.
FILE should be output of 'automate sync'. (external sync handles tickers better)."
  (interactive)
  (if (buffer-live-p (get-buffer "*xmtn-sync*"))
      (progn
	(pop-to-buffer "*xmtn-sync*")
	(xmtn-sync-save))
    ;; else create
    (pop-to-buffer (get-buffer-create "*xmtn-sync*"))
    (setq buffer-read-only nil)
    (delete-region (point-min) (point-max))
    (xmtn-sync-mode)
    (xmtn-sync-load-file file))

  ;; now add FILE
  (setq file (or file
		 (expand-file-name xmtn-sync-review-file dvc-config-directory)))
  (if (file-exists-p file)
      (progn
	(goto-char (point-min))
	(setq buffer-read-only nil)
	(insert-file-contents-literally file)

	;; user may have run several syncs, dumping each output into FILE; loop thru each.
	(while (xmtn-sync-parse (point-min)))
	(setq buffer-read-only t)
	(set-buffer-modified-p nil)
	(xmtn-sync-save)
	(delete-file file))))

(provide 'xmtn-sync)

;; end of file
