;;; xmtn-automate.el --- Interface to monotone's "automate" functionality

;; Copyright (C) 2008 - 2011 Stephen Leake
;; Copyright (C) 2006, 2007 Christian M. Ohler

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

;; This library provides access to monotone's "automate" interface
;; from Emacs Lisp.
;;
;; see http://www.monotone.ca/docs/Automation.html#Automation for
;; details of the monotone automate command.
;;
;; mtn automate allows sending several commands to a single mtn
;; process, and provides the results in a form that is easy to
;; parse. It does some caching between command, and will do more in
;; the future, so this is a significant speed-up over spawning a new
;; subprocess for each command.
;;
;; To allow xmtn-automate to track how long an automate stdio process
;; needs to be kept around, and to store meta data, we introduce the
;; concept of a session.  To the programmer using this library, a
;; session is an opaque object that is needed to run automate
;; commands.  Each session is associated with a monotone workspace
;; ("root") that the commands will operate on.  A session can be
;; obtained using `xmtn-automate-cache-session'.  Note that
;; `xmtn-automate-cache-session' doesn't necessarily start a fresh
;; monotone process, if a session with that root already exists.  The
;; process must be killed with `xmtn-automate-kill-session'.
;;
;; Once you have a session object, you can use
;; `xmtn-automate--new-command' to send commands to monotone.
;;
;; A COMMAND is a list of strings (the command and its arguments), or
;; a cons of lists of strings. If car COMMAND is a list, car COMMAND
;; is options, cdr is the command and arguments. Options are always
;; specified as pairs of keyword and value, and without the leading
;; "--". If an option has no value, use ""; see
;; xmtn--status-inventory-sync in xmtn-dvc for an example.
;;
;; `xmtn-automate--new-command' returns a command handle.  You use this
;; handle to check the error code of the command and obtain its
;; output.  Your Emacs Lisp code can also do other computation while
;; the monotone command runs.  Allowing this kind of parallelism is
;; the main reason for introducing command handles.
;;
;; There are some notes on the design of xmtn in
;; docs/xmtn-readme.txt.

;;; Code:

(eval-and-compile
  (require 'cl)
  (require 'parse-time)                 ;for parse-integer
  (require 'xmtn-base)
  (require 'xmtn-run)
  (require 'xmtn-compat))

(defconst xmtn-automate-arguments nil
  "Arguments and options for 'mtn automate stdio' sessions.")

(defconst xmtn-sync-session-root "sync"
  "Name for unique automate session used for sync commands.")

(defun xmtn-automate-command-buffer (command)
  (xmtn-automate--command-handle-buffer command))

(defun xmtn-automate-command-write-marker-position (command)
  (marker-position (xmtn-automate--command-handle-write-marker command)))

(defun xmtn-automate-command-wait-until-finished (handle)
  (let ((session (xmtn-automate--command-handle-session handle)))
    (while (not (xmtn-automate--command-handle-finished-p handle))
      ;; We use a timeout here to allow debugging, and incremental
      ;; processing of tickers. We don't use a process filter, because
      ;; they are very hard to debug.
      (accept-process-output (xmtn-automate--session-process session) 0.01)
      (xmtn-automate--process-new-output session))
    (unless (eql (xmtn-automate--command-handle-error-code handle) 0)
      (xmtn-automate--cleanup-command handle)
      (pop-to-buffer (format dvc-error-buffer 'xmtn))
      (goto-char (point-max))
      (newline)
      (insert (format "command: %s" (xmtn-automate--command-handle-command handle)))
      (when (xmtn-automate--session-error-file session)
	(insert-file-contents (xmtn-automate--session-error-file session)))
      (error "mtn error %s" (xmtn-automate--command-handle-error-code handle)))
    (if (xmtn-automate--command-handle-warnings handle)
      (display-buffer (format dvc-error-buffer 'xmtn) t))
    )
  nil)

(defvar xmtn-automate--*sessions* '()
  "Assoc list of sessions, indexed by uniquified root directory.")

(defun xmtn-automate-cache-session (root)
  "If necessary, create a mtn automate session for workspace
ROOT, store it in session cache. Return session."
  ;; we require an explicit root argument here, rather than relying on
  ;; default-directory, because one application is to create several
  ;; sessions for several workspaces, and operate on them as a group
  ;; (see xmtn-multi-status.el, for example).
  (let* ((default-directory (dvc-uniquify-file-name root))
         (session (xmtn-automate-get-cached-session default-directory)))
    (or session
        (progn
          (setq session (xmtn-automate--make-session default-directory default-directory))
          (setq xmtn-automate--*sessions*
                (acons default-directory session xmtn-automate--*sessions*))
          session))))

(defun xmtn-automate-get-cached-session (key)
  "Return a session from the cache, or nil. KEY is uniquified
workspace root."
  (cdr (assoc key xmtn-automate--*sessions*)))

(defun xmtn-automate--command-output-as-string (handle)
  (with-current-buffer (xmtn-automate-command-buffer handle)
    (prog1
        (buffer-substring-no-properties (point-min) (point-max))
      (xmtn-automate--cleanup-command handle))))

(defun xmtn-automate-command-output-string (root command)
  "Send COMMAND to session for ROOT. Return result as a string."
  (let* ((session (xmtn-automate-cache-session root))
         (command-handle (xmtn-automate--new-command session command)))
    (xmtn-automate-command-wait-until-finished command-handle)
    (xmtn-automate--command-output-as-string command-handle)))

(defun xmtn-automate-command-output-buffer (root buffer command &optional display-tickers)
  "Send COMMAND to session for ROOT, insert result into BUFFER.
Optionally DISPLAY-TICKERS in mode-line of BUFFER."
  (let* ((session (xmtn-automate-cache-session root))
         (command-handle (xmtn-automate--new-command session command display-tickers buffer)))
    (xmtn-automate-command-wait-until-finished command-handle)
    (with-current-buffer buffer
      (insert-buffer-substring-no-properties
       (xmtn-automate-command-buffer command-handle)))
    (xmtn-automate--cleanup-command command-handle)))

(defun xmtn-automate-command-output-file (root file command)
  "Send COMMAND to session for ROOT, store result in FILE."
  (let* ((session (xmtn-automate-cache-session root))
         (command-handle (xmtn-automate--new-command session command nil nil)))
    (xmtn-automate-command-wait-until-finished command-handle)
    (with-current-buffer (xmtn-automate-command-buffer command-handle)
      (write-region nil nil file))
    (xmtn-automate--cleanup-command command-handle)))

(defun xmtn-automate-command-output-lines (root command)
  "Return list of strings containing output of COMMAND, one line per
string."
  (let* ((session (xmtn-automate-cache-session root))
         (handle (xmtn-automate--new-command session command)))
    (xmtn-automate-command-wait-until-finished handle)
    (with-current-buffer (xmtn-automate-command-buffer handle)
      (goto-char (point-max))
      (let (result)
	(while (= 0 (forward-line -1))
	  (setq result (cons (buffer-substring-no-properties
			      (point)
			      (progn (end-of-line) (point)))
			     result)))
	(xmtn-automate--cleanup-command handle)
	result))))

(defun xmtn-automate-command-output-line (root command)
  "Return the one line output from mtn automate as a string.

Signals an error if output contains zero lines or more than one line."
  (let ((lines (xmtn-automate-command-output-lines root command)))
    (unless (eql (length lines) 1)
      (error "Expected precisely one line of output from mtn automate, got %s: %s %S"
             (length lines)
             xmtn-executable
             command))
    (first lines)))

(defun xmtn-automate--set-process-session (process session)
  (process-put process 'xmtn-automate--session session))

(defun xmtn-automate--process-session (process)
  (process-get process 'xmtn-automate--session))

(defstruct (xmtn-automate--decoder-state
            (:constructor xmtn-automate--%make-raw-decoder-state))
  ;; State for decoding stdio output packets.
  (read-marker)
  ;; char position (not marker) of last character read. We use a
  ;; position, not a marker, because text gets inserted in front of
  ;; the marker, and it moves.

  (remaining-chars 0) ;; until end of packet
  (stream 0); determines output buffer
  )

(defstruct (xmtn-automate--session
            (:constructor xmtn-automate--%make-raw-session)
            (:copier xmtn-automate--copy-session))
  (root)
  (name)
  (buffer nil)
  (error-file nil)
  (process nil)
  (decoder-state)
  (next-command-number 0)
  (remaining-command-handles)
  (sent-kill-p)
  (closed-p nil))

(defstruct (xmtn-automate--command-handle
            (:constructor xmtn-automate--%make-raw-command-handle))
  (command)
  (mtn-command-number)
  (session-command-number)
  (session)
  (buffer)
  (write-marker)
  (finished-p nil)
  (error-code nil)
  (warnings nil)
  (tickers nil) ; alist of xmtn-automate--ticker by short name; nil if none active
  (display-tickers nil) ; list of long names of tickers to display
  (display-buffer nil) ; buffer in which to display tickers
  )

(defun* xmtn-automate--initialize-session (session &key root name)
  (xmtn--assert-optional (equal root (file-name-as-directory root)) t)
  (setf (xmtn-automate--session-root session) root
        (xmtn-automate--session-name session) name
        (xmtn-automate--session-process session) nil
        (xmtn-automate--session-closed-p session) nil)
  nil)

(defun xmtn-automate--make-session (root key)
  (dvc-trace "new session %s" key)
  (let* ((name (format "xmtn automate session for %s" key)))
    (let ((session (xmtn-automate--%make-raw-session)))
      (xmtn-automate--initialize-session session :root root :name name)
      session)))

(defun xmtn-automate--session-send-process-kill (session)
  (let ((process (xmtn-automate--session-process session)))
    (setf (xmtn-automate--session-sent-kill-p session) t)
    (with-current-buffer (xmtn-automate--session-buffer session)
      (let ((inhibit-read-only t)
            deactivate-mark)
        (save-excursion
          (goto-char (process-mark process))
          (insert "\n(killing process)\n")
          (set-marker (process-mark process) (point)))))

    (signal-process process 'KILL)

    ;; This call to `sit-for' is apparently needed in some situations to
    ;; make sure the process really gets killed.
    (sit-for 0))
  nil)

(defun xmtn-automate--close-session (session)
  "Kill session process, buffer."
  (setf (xmtn-automate--session-closed-p session) t)
  (let ((process (xmtn-automate--session-process session)))
    (cond
     ((null process)
      ;; Process was never created or was killed - most likely 'mtn
      ;; not found in path'. Don't warn if buffer hasn't been deleted;
      ;; that obscures the real error message
      nil)
     (t
      (ecase (process-status process)
        (run
         (process-send-eof process)
         (xmtn-automate--session-send-process-kill session)
         (sleep-for 1.0); let process die before deleting associated buffers
         )
        (exit t)
        (signal t))))

    (unless xmtn-automate--*preserve-buffers-for-debugging*
      (if (buffer-live-p (xmtn-automate--session-buffer session))
          (kill-buffer (xmtn-automate--session-buffer session)))))
  nil)

(defun xmtn-automate-kill-session (root)
  "Kill session for ROOT."
  (interactive)
  (let ((session (assoc (dvc-uniquify-file-name root) xmtn-automate--*sessions*)))
    ;; session may have already been killed
    (when session
      (when (xmtn-automate--session-error-file (cdr session))
	(delete-file (xmtn-automate--session-error-file session)))
      (xmtn-automate--close-session (cdr session))
      (setq xmtn-automate--*sessions*
	    (delete session xmtn-automate--*sessions*)))))

(defun xmtn-kill-all-sessions ()
  "Kill all xmtn-automate sessions."
  (interactive)
  (let ((count 0)
        (key " *xmtn automate session for"))
    (dolist (session xmtn-automate--*sessions*)
      (xmtn-automate--close-session (cdr session))
      (setq count (+ 1 count)))
    (setq xmtn-automate--*sessions* nil)
    (message "killed %d sessions" count)))

(defun xmtn-automate--start-process (session)
  (xmtn--check-cached-command-version)
  (let ((name (xmtn-automate--session-name session))
        (buffer (xmtn-automate--new-buffer session))
        (root (xmtn-automate--session-root session)))
    (let ((process-connection-type nil); use a pipe, not a tty
          (default-directory root)
	  ;; start-process merges stderr and stdout from the child,
	  ;; but stderr messages are not packetized, so they confuse
	  ;; the packet parser. This is only a problem when the
	  ;; session will run 'sync ssh:' or 'sync file:', since those
	  ;; spawn new mtn processes that can report errors on
	  ;; stderr. All other errors will be reported properly thru
	  ;; the stdout packetized error stream.  xmtn-sync uses the
	  ;; unique xmtn-sync-session-root for the session root, so we
	  ;; treat that specially.
	  (cmd (if (string= xmtn-sync-session-root (file-name-nondirectory root))
		   (progn
		     (setf (xmtn-automate--session-error-file session)
			   (dvc-make-temp-name (concat xmtn-sync-session-root "-errors")))
		     (list dvc-sh-executable
			   "-c"
			   (mapconcat
			    'concat
			    (append (list xmtn-executable "--db=:memory:" "automate" "stdio")
				    xmtn-automate-arguments
				    (list "2>"
					  (xmtn-automate--session-error-file session)))
			    " ")))
		 ;; not the sync session
		 (append (list xmtn-executable "automate" "stdio")
			 xmtn-automate-arguments))))
      (let ((process (apply 'start-process name buffer cmd)))
        (ecase (process-status process)
          (run
           ;; If the process started ok, it outputs the stdio
           ;; header. If there was an error (like default_directory is
           ;; not a mtn workspace), it outputs an error message and
           ;; exits.
           (accept-process-output process)
           (with-current-buffer buffer
             ;; If the format version changes, we probably need to
             ;; adapt. So we insist on an exact match.
             (goto-char (point-min))
             (if (looking-at "format-version: \\([0-9]+\\)\n\n")
                 (if (not (string-equal (match-string 1) xmtn--required-automate-format-version))
                     (error "unexpected mtn automate stdio format version %s" (match-string 0)))
               ;; Some error. Display the session buffer to show the error
               (pop-to-buffer buffer)
	       (let ((inhibit-read-only t))
		 (when (xmtn-automate--session-error-file session)
		   (insert-file-contents (xmtn-automate--session-error-file session))))
               (error "unexpected header from mtn automate process"))))
          ((exit signal)
           (pop-to-buffer buffer)
           (error "failed to create mtn automate process")))

        (setf (xmtn-automate--session-decoder-state session)
              (xmtn-automate--%make-raw-decoder-state
               :read-marker (with-current-buffer buffer (match-end 0))))

        (xmtn-automate--set-process-session process session)
        (xmtn--set-process-query-on-exit-flag process nil)
        ;; Need binary (or no-conversion or maybe raw-text-unix?)
        ;; since this is the format in which mtn automate stdio
        ;; computes the size of the output.
        (set-process-coding-system process 'binary 'binary)
        (setf (xmtn-automate--session-process session) process)
        (setf (xmtn-automate--session-remaining-command-handles session) (list))
        (setf (xmtn-automate--session-sent-kill-p session) nil)
        process))))

(defun xmtn-automate--ensure-process (session)
  "Ensure SESSION has an active process; restart it if it died."
  (let ((process (xmtn-automate--session-process session)))
    (when (or (null process)
              (ecase (process-status process)
                (run nil)
                (exit t)
                (signal t)))
      (setq process (xmtn-automate--start-process session))
      (setf (xmtn-automate--session-process session) process))
    (xmtn--assert-optional (buffer-live-p (xmtn-automate--session-buffer
                                           session)))
    process))

(defun xmtn-automate--new-buffer (session)
  (let* ((buffer-base-name (format " *%s: session*"
                                   (xmtn-automate--session-name session)))
         (buffer (generate-new-buffer buffer-base-name)))
    (with-current-buffer buffer
      (buffer-disable-undo)
      (set-buffer-multibyte nil)
      (setq buffer-read-only t))
    (setf (xmtn-automate--session-buffer session) buffer)
    buffer))

(defun xmtn-automate--append-encoded-strings (strings)
  "Encode STRINGS (a list of strings or nil) in automate stdio format,
insert into current buffer.  Assumes that point is at the end of
the buffer."
  (xmtn--assert-optional (eql (point) (point-max)))
  (dolist (string strings)
    (if string
        (progn
          (save-excursion (insert string))
          (encode-coding-region (point) (point-max) 'xmtn--monotone-normal-form)
          (insert (number-to-string (- (point-max) (point))) ":")
          (goto-char (point-max)))))
  nil)

(defun xmtn-automate--send-command-string (session command option-pairs session-number)
  "Send COMMAND and OPTION-PAIRS to SESSION."
  (let* ((buffer-name (format "*%s: input for command %s*"
                              (xmtn-automate--session-name session)
                              session-number))
         (buffer nil))
    (unwind-protect
        (progn
          (when (get-buffer buffer-name)
            ;; Make sure the buffer is in a clean state.
            (with-current-buffer buffer-name
              (let ((inhibit-read-only t))
                (erase-buffer))
              (fundamental-mode)))
          (setq buffer (get-buffer-create buffer-name))
          (with-current-buffer buffer
            (buffer-disable-undo)
            (set-buffer-multibyte t)
            (setq buffer-read-only t)
            (let ((inhibit-read-only t))
              (when option-pairs
                (insert "o")
                (xmtn-automate--append-encoded-strings option-pairs)
                (insert "e"))
              (insert "l")
              (xmtn-automate--append-encoded-strings command)
              (insert "e\n"))

            (dvc-trace "mtn automate: '%s'" (buffer-substring (point-min) (point-max)))

            (process-send-region (xmtn-automate--session-process session)
                                 (point-min) (point-max))))
      (when buffer
        (unless xmtn-automate--*preserve-buffers-for-debugging*
          (kill-buffer buffer))))))

(defun xmtn-automate--new-command (session command &optional display-tickers display-buffer)
  "Send COMMAND to SESSION. Optionally DISPLAY-TICKERS in DISPLAY-BUFFER mode-line.
DISPLAY-TICKERS is a list of strings; names of tickers to display."
  (xmtn-automate--ensure-process session)
  (let* ((command-number
          (1- (incf (xmtn-automate--session-next-command-number
                     session))))
         (buffer-name (format " *%s: output for command %s*"
                              (xmtn-automate--session-name session)
                              command-number))
         (buffer
          (progn (when (get-buffer buffer-name)
                   ;; Make sure no local variables or mode changes
                   ;; remain from the previous command parser.
                   (with-current-buffer buffer-name
                     (let ((inhibit-read-only t))
                       (erase-buffer))
                     (fundamental-mode)))
                 (get-buffer-create buffer-name))))
    (if (not (listp (car command)))
        (xmtn-automate--send-command-string session command '() command-number)
      (xmtn-automate--send-command-string session (cdr command) (car command) command-number))
    (with-current-buffer buffer
      (buffer-disable-undo)
      (set-buffer-multibyte nil)
      (setq buffer-read-only t)
      (xmtn--assert-optional (and (eql (point) (point-min))
                                  (eql (point) (point-max))))
      (let ((handle (xmtn-automate--%make-raw-command-handle
                     :session session
                     :command command
                     :session-command-number command-number
                     :buffer buffer
                     :write-marker (set-marker (make-marker) (point))
		     :display-tickers display-tickers
		     :display-buffer display-buffer)))
        (setf
         (xmtn-automate--session-remaining-command-handles session)
         (nconc (xmtn-automate--session-remaining-command-handles session)
                (list handle)))
        handle))))

(defun xmtn-automate--cleanup-command (handle)
  (unless xmtn-automate--*preserve-buffers-for-debugging*
    (kill-buffer (xmtn-automate--command-handle-buffer handle))))

(defstruct (xmtn-automate--ticker)
  (long-name)
  (display nil)
  (current 0)
  (total 0))

(defun xmtn-automate--ticker-process (ticker-string tickers display-tickers)
  "Process TICKER-STRING, updating tickers in alist TICKERS.
DISPLAY-TICKERS is list of ticker names to display.
Return updated value of TICKERS."
  ;; ticker-string is contents of one stdio ticker packet:
  ;; c:certificates;k:keys;r:revisions;   declare short and long names
  ;; c=0;k=0;r=0;                         set total values
  ;; c#7;k#1;r#2;                         set current values
  ;; c;k;r;                               close ticker
  (while (< 0 (length ticker-string))
    (let* ((tick (substring ticker-string 0 (search ";" ticker-string)))
	   (name (aref tick 0))
	   (ticker (cadr (assoc name tickers))))
      (if ticker
	  (cond
	   ((= 1 (length tick))
	    (setq tickers (assq-delete-all name tickers)))
	   ((= ?= (aref tick 1))
	    (setf (xmtn-automate--ticker-total ticker) (string-to-number (substring tick 2))))
	   ((= ?# (aref tick 1))
	    (setf (xmtn-automate--ticker-current ticker) (string-to-number (substring tick 2))))
	   )
	;; else create new ticker
	(setq tickers
	      (add-to-list
	       'tickers
	       (list name
		     (make-xmtn-automate--ticker
		      :long-name (substring tick 2)
		      :display (not (null (member (substring tick 2) display-tickers)))
		      ))))
	)
      (setq ticker-string (substring ticker-string (+ 1 (length tick))))
      ))
  tickers)

(defun xmtn-automate--ticker-mode-line (tickers buffer)
  "Display TICKERS alist in BUFFER mode-line-process"
  (with-current-buffer buffer
    (setq mode-line-process nil)
    (loop for item in tickers do
	  (let ((ticker (cadr item)))
	    (if (xmtn-automate--ticker-display ticker)
		(progn
		  (setq mode-line-process
			(concat mode-line-process
				(format " %s %d/%d"
					(xmtn-automate--ticker-long-name ticker)
					(xmtn-automate--ticker-current ticker)
					(xmtn-automate--ticker-total ticker))))
		  (force-mode-line-update)))))))

(defun xmtn-automate--process-new-output--copy (session)
  "Copy SESSION current packet output to command output or error buffer.
Return non-nil if some text copied."
  ;; We often get here with only a partial packet; the main channel
  ;; outputs very large packets.
  (let* ((session-buffer (xmtn-automate--session-buffer session))
         (state (xmtn-automate--session-decoder-state session))
         (command (first (xmtn-automate--session-remaining-command-handles
                          session)))
         (output-buffer
          (ecase (xmtn-automate--decoder-state-stream state)
            (?m
             (xmtn-automate--command-handle-buffer command))
	    (?t
	     ;; Display ticker in mode line of display buffer for
	     ;; current command.
	     (xmtn-automate--command-handle-display-buffer command))
            ((?e ?w ?p)
             (if (equal ?w (xmtn-automate--decoder-state-stream state))
                 (setf (xmtn-automate--command-handle-warnings command) t))
             ;; probably ought to do something else with p, but
             ;; this is good enough for now.
             (get-buffer-create (format dvc-error-buffer 'xmtn)))))
         (write-marker
          (xmtn-automate--command-handle-write-marker command)))

    (with-current-buffer session-buffer
      (let* ((end (min (+ (xmtn-automate--decoder-state-read-marker state)
                          (xmtn-automate--decoder-state-remaining-chars state))
                       (point-max)))
             (chars-to-read (- end (xmtn-automate--decoder-state-read-marker state))))
        (cond
         ((= chars-to-read 0)
          nil)
         ((> chars-to-read 0)
          (if (not (buffer-live-p output-buffer))
              ;; Buffer has already been killed, just discard input.
              t
	    (ecase (xmtn-automate--decoder-state-stream state)
	      (?t
	       ;; Display ticker in mode line of display buffer for
	       ;; current command. But only if we have the whole packet
	       (if (= chars-to-read (xmtn-automate--decoder-state-remaining-chars state))
		   (progn
		     (setf (xmtn-automate--command-handle-tickers command)
			   (xmtn-automate--ticker-process
			    (buffer-substring-no-properties (xmtn-automate--decoder-state-read-marker state)
							    end)
			    (xmtn-automate--command-handle-tickers command)
			    (xmtn-automate--command-handle-display-tickers command)))
		     (xmtn-automate--ticker-mode-line
		      (xmtn-automate--command-handle-tickers command)
		      output-buffer)
		     (setf (xmtn-automate--decoder-state-read-marker state) end)
		     (decf (xmtn-automate--decoder-state-remaining-chars state)
			   chars-to-read))
		 ;; not a whole packet; no text copied
		 nil))

	      ((?m ?e ?w ?p)
	       (with-current-buffer output-buffer
		 (save-excursion
		   (goto-char write-marker)
		   (let ((inhibit-read-only t)
			 deactivate-mark)
		     (insert-buffer-substring-no-properties session-buffer
							    (xmtn-automate--decoder-state-read-marker state)
							    end))
		   (set-marker write-marker (point))))
	       (setf (xmtn-automate--decoder-state-read-marker state) end)
	       (decf (xmtn-automate--decoder-state-remaining-chars state)
		     chars-to-read)
	       t)))
          ))))))

(defun xmtn--debug-mark-text-processed (buffer start end bold-p)
  (xmtn--assert-optional (< start end) t)
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (if bold-p
          (xmtn--assert-for-effect
           (add-text-properties start end
                                '(face
                                  (:strike-through
                                   t
                                   :weight semi-bold))))
        (xmtn--assert-for-effect
         (add-text-properties start end '(face (:strike-through
                                                t))))))))

(defun xmtn-automate--process-new-output (session)
  (let* ((state (xmtn-automate--session-decoder-state session))
         (write-marker (process-mark (xmtn-automate--session-process session)))
         (tag 'check-for-more))
    (with-current-buffer (xmtn-automate--session-buffer session)
      (loop
       for command = (first (xmtn-automate--session-remaining-command-handles
                             session))
       do
       (ecase tag
         (check-for-more
          (if (= (xmtn-automate--decoder-state-read-marker state) write-marker)
              (setq tag 'exit-loop)
            (setq tag 'again)))

         (again
          (cond
           ((> (xmtn-automate--decoder-state-remaining-chars state) 0)
	    (if (= ?l (xmtn-automate--decoder-state-stream state))
		;; got the rest of the last packet; process in t branch next loop
		(setf (xmtn-automate--decoder-state-remaining-chars state) 0)
	      (if (xmtn-automate--process-new-output--copy session)
		  (setq tag 'again)
		(setq tag 'check-for-more))))

           (t
            ;; new packet, or final packet
            (goto-char (xmtn-automate--decoder-state-read-marker state))
            ;; A packet has the structure:
            ;; <command number>:<stream>:<size>:<output>
            ;; Streams are:
            ;; m  main
            ;; e  error
            ;; w  warning
            ;; p  progress
            ;; t  ticker
            ;; l  last
            (cond
             ((looking-at "\\([0-9]+\\):\\([mewptl]\\):\\([0-9]+\\):")
              (let ((stream (aref (match-string 2) 0))
                    (size (parse-integer (match-string 3))))
		(setf (xmtn-automate--decoder-state-remaining-chars state) size)
		(setf (xmtn-automate--decoder-state-stream state) stream)
                (ecase stream
                  ((?m ?e ?w ?t ?p)
		   (setf (xmtn-automate--decoder-state-read-marker state) (match-end 0))
                   (setq tag 'again) )

                  (?l
		   (if (> (+ size (match-end 0)) (point-max))
		       ;; do not have the error code yet
		       (setq tag 'exit-loop)
		     (setf (xmtn-automate--decoder-state-read-marker state) (+ size (match-end 0)))
		     (setf (xmtn-automate--command-handle-error-code command)
			   (parse-integer
			    (buffer-substring-no-properties
			     (match-end 0) (xmtn-automate--decoder-state-read-marker state)) ))
		     (setf (xmtn-automate--command-handle-finished-p command) t)
		     (with-no-warnings
		       ;; suppress compiler warning about discarding result
		       (pop (xmtn-automate--session-remaining-command-handles session)))
		     (if (xmtn-automate--session-closed-p session)
			 (setq tag 'exit-loop)
		       (setq tag 'check-for-more)))
                   )
                  )))

             (t
              ;; Not a packet yet, or garbage in the stream from some
              ;; Lua hook. Most likely we are at the end of the
              ;; buffer, don't have a complete header, and there is
              ;; more output coming soon. A packet header has at least
              ;; 6 bytes; allowing 4 digits per integer takes that to
              ;; 12.
              (if (> 12 (- (point-max) (point)))
                  (setq tag 'exit-loop)
                (error "Unexpected output from mtn at '%s':%d:'%s'"
                       (current-buffer)
                       (point)
                       (buffer-substring (point) (min (point-max) (+ (point) 100))))
		))))))

         (exit-loop (return))))))
  nil)

(defvar xmtn-automate--*preserve-buffers-for-debugging* nil)

(defun xmtn--map-parsed-certs (xmtn--root xmtn--revision-hash-id xmtn--thunk)
  (lexical-let ((root xmtn--root)
                (revision-hash-id xmtn--revision-hash-id)
                (thunk xmtn--thunk))
    (xmtn--with-automate-command-output-basic-io-parser
        (xmtn--next-stanza root `("certs" ,revision-hash-id))
      (loop
       for xmtn--stanza = (funcall xmtn--next-stanza)
       while xmtn--stanza
       do (xmtn-match xmtn--stanza
            ((("key" (id $xmtn--key))
              ("signature" (string $xmtn--signature))
              ("name" (string $xmtn--name))
              ("value" (string $xmtn--value))
              ("trust" (string $xmtn--trust)))
             (setq xmtn--signature (xmtn-match xmtn--signature
                                     ("ok" 'ok)
                                     ("bad" 'bad)
                                     ("unknown" 'unknown)))
             (let ((xmtn--trusted (xmtn-match xmtn--trust
                                    ("trusted" t)
                                    ("untrusted" nil))))
               (macrolet ((decodef (var)
                            `(setq ,var (decode-coding-string
                                         ,var 'xmtn--monotone-normal-form))))
                 (decodef xmtn--key)
                 (decodef xmtn--name)
                 ;; I'm not sure this is correct.  The documentation
                 ;; mentions a cert_is_binary hook, but it doesn't
                 ;; exist; and even if it did, we would have no way of
                 ;; calling it from here.  But, since cert values are
                 ;; always passed on the command line, and command
                 ;; line arguments are converted to utf-8, I suspect
                 ;; certs will also always be in utf-8.
                 (decodef xmtn--value))
               (funcall thunk
                        xmtn--key xmtn--signature xmtn--name xmtn--value
                        xmtn--trusted))))))))

(defun xmtn--list-parsed-certs (root revision-hash-id)
  "Return a list of the contents of each cert attached to REVISION-HASH-ID.
Each element of the list is a list; key, signature, name, value, trust."
  (lexical-let ((accu '()))
    (xmtn--map-parsed-certs root revision-hash-id
                            (lambda (key signature name value trusted)
                              (push (list key signature name value trusted)
                                    accu)))
    (setq accu (nreverse accu))
    accu))

(defun xmtn--heads (root branch)
  (xmtn-automate-command-output-lines
   root
    (list "heads"
	  (or branch
	      (xmtn--tree-default-branch root)))))

(defun xmtn--rev-author (root rev)
  "Return first author of REV"
  (let (cert-name
	result)
    (with-temp-buffer
      (xmtn-automate-command-output-buffer root (current-buffer) (list "certs" rev))
      (goto-char (point-min))
      ;;       key [46ec58576f9e4f34a9eede521422aa5fd299dc50]
      ;; signature "ok"
      ;;      name "author"
      ;;     value "beth"
      ;;     trust "trusted"
      ;;
      ;; ...
      (while (not result)
	(xmtn-basic-io-skip-line "key")
	(xmtn-basic-io-skip-line "signature")
	(xmtn-basic-io-check-line "name" (setq cert-name (cadar value)))
	(xmtn-basic-io-check-line "value"
	  (if (string= cert-name "author")
	      (setq result (cadar value))))
	(xmtn-basic-io-skip-line "trust")
	(xmtn-basic-io-check-empty)))
    result))

(defun xmtn--tree-default-branch (root)
  (xmtn-automate-command-output-line root `("get_option" "branch")))

(defun xmtn--get-corresponding-path-raw (root normalized-file-name
					      source-revision-hash-id
					      target-revision-hash-id)
  "Given NORMALIZED-FILE-NAME in SOURCE-REVISION-HASH-ID, return file name in TARGET-REVISION-HASH-ID"
  (check-type normalized-file-name string)
  (xmtn--with-automate-command-output-basic-io-parser
      (next-stanza root `("get_corresponding_path"
                          ,source-revision-hash-id
                          ,normalized-file-name
                          ,target-revision-hash-id))
    (xmtn-match (funcall next-stanza)
      (nil nil)
      ((("file" (string $result)))
       (assert (null (funcall next-stanza)))
       result))))

(defun xmtn--insert-file-contents (root content-hash-id buffer)
  (check-type content-hash-id xmtn--hash-id)
  (xmtn-automate-command-output-buffer
   root buffer `("get_file" ,content-hash-id)))

(defun xmtn--insert-file-contents-by-name (root backend-id normalized-file-name buffer)
  (let* ((resolved-id (xmtn--resolve-backend-id root backend-id))
         (hash-id (case (car resolved-id)
                    (local-tree nil)
                    (revision (cadr resolved-id)))))
    (case (car backend-id)
      ((local-tree last-revision)
       ;;  file may have been renamed but not committed
       (setq normalized-file-name (xmtn--get-rename-in-workspace-to root normalized-file-name)))
      (t nil))

    (let ((cmd (if hash-id
                  (cons (list "revision" hash-id) (list "get_file_of" normalized-file-name))
                (list "get_file_of" normalized-file-name))))
      (xmtn-automate-command-output-buffer root buffer cmd))))

(defun xmtn--get-file-by-id (root file-id save-as)
  "Store contents of FILE-ID in file SAVE-AS."
  (with-temp-file save-as
    (set-buffer-multibyte nil)
    (setq buffer-file-coding-system 'binary)
    (xmtn--insert-file-contents root file-id (current-buffer))))

(provide 'xmtn-automate)

;;; xmtn-automate.el ends here
