;;; mu4e-autotask.el --- Email automation for mu4e -*- lexical-binding: t -*-
;; jscpd:ignore-start

;; Copyright (C) 2026 Laurynas Biveinis

;; Author: Laurynas Biveinis <laurynas.biveinis@gmail.com>
;; Assisted-by: Claude Code:claude-opus-4-8
;; Assisted-by: Claude Code:claude-fable-5
;; Package-Version: 1.0.0
;; Package-Revision: 1.0.0-0-gcaab869aaf67
;; URL: https://github.com/laurynas-biveinis/mu4e-autotask
;; Package-Requires: ((emacs "29.1"))
;; Keywords: mail

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;; jscpd:ignore-end

;;; Commentary:

;; Rule-based email automation for mu4e.  Configure `mu4e-autotask-rules' with a
;; list of rules that match an incoming message by sender and/or subject and run
;; an action function on it, then call `mu4e-autotask-initialize' to expose the
;; dispatcher as a mu4e view action.
;;
;; The package also provides building blocks for action functions: helpers to
;; read a message's raw text, MIME-part bodies, and attachments, plus an
;; `mu4e-autotask-email-template' struct and `mu4e-autotask-send-email' for
;; composing and sending templated outgoing mail.
;;
;; Calendar invitations (messages mu flags `calendar') that no rule matches
;; are handled by a built-in RSVP flow when `mu4e-autotask-handle-icalendar'
;; is non-nil: prompt for accept / tentative / decline, send the iTIP reply
;; to the organizer, and optionally record the event in a Google Calendar via
;; org-gcal (see `mu4e-autotask-icalendar-event-target-function').
;;
;; mu4e 1.12 or later is a runtime requirement; it ships with mu and is not an
;; ELPA package, so it is not listed in Package-Requires.

;;; Code:

(require 'cl-lib)
(require 'seq)
;; gnus-icalendar is a required built-in for the RSVP flow, unlike the
;; optional third-party org-gcal which is loaded on demand: it is always
;; present in the supported Emacs, and `gnus-icalendar-with-decoded-handle'
;; is a macro that needs compile-time availability, so a deferred require
;; would buy nothing and complicate byte-compilation.
(require 'gnus-icalendar)
(require 'mm-decode)
(require 'mml)
(require 'message)
(require 'mu4e-contacts)
(require 'mu4e-helpers)
(require 'mu4e-message)
(require 'mu4e-context)
(require 'mu4e-compose)
(require 'mu4e-draft)
;; `mu4e-view' provides `mu4e-view-actions' and pulls in `mu4e-view-mime-parts'.
;; Depend on it rather than the internal `mu4e-mime-parts' file, which older mu4e
;; versions do not ship.
(require 'mu4e-view)

;;; Customization

(defgroup mu4e-autotask nil
  "Email automation for mu4e."
  :group 'mu4e
  :prefix "mu4e-autotask-")

(defcustom mu4e-autotask-rules nil
  "List of email automation rules.
Each rule is a property list with the following properties:
  :sender-exact  - Exact sender email address to match.
  :sender-match  - Regexp to match against the sender email address.
  :subject-exact - Exact subject text to match.
  :subject-match - Regexp to match against the email subject.
  :action-fn     - Function called with the message when the rule matches.

Only one of :sender-exact or :sender-match may be specified per rule, and only
one of :subject-exact or :subject-match.  A rule with no subject criteria
matches any subject; a rule with no sender criteria never matches.  The list is
processed in order; having more than one rule match the same message is an
error.  Every rule must supply an :action-fn."
  :type
  '(repeat
    (plist
     :options
     ((:sender-exact string)
      (:sender-match string)
      (:subject-exact string)
      (:subject-match string)
      (:action-fn function))))
  :group 'mu4e-autotask)

(defcustom mu4e-autotask-handle-icalendar t
  "Whether to handle calendar invitations not matched by any rule.
When non-nil, dispatching a message that mu flagged `calendar' and that no
rule in `mu4e-autotask-rules' matched runs the built-in invitation handler:
prompt for accept / tentative / decline, compose the iTIP reply to the
meeting organizer, and confirm before sending."
  :type 'boolean
  :group 'mu4e-autotask)

(defcustom mu4e-autotask-icalendar-event-target-function nil
  "Function mapping an invitation message to its Google Calendar target.
Called with the mu4e message plist; returns a cons (ORG-FILE . CALENDAR-ID):
the org-gcal file the event entry is appended to and the Google calendar id
\(the calendar's email-address form) it is posted to, or nil to skip event
creation for that message.  When the variable itself is nil, the built-in
invitation handler only sends the RSVP reply and never records events.
Recording an event requires org-gcal, which is loaded on first use and is not
a declared package dependency.  Declined invitations never create events.

For example:

  (setq mu4e-autotask-icalendar-event-target-function
        (lambda (msg)
          (if (string-prefix-p \"/work/\"
                               (mu4e-message-field msg :maildir))
              \\='(\"~/org/gcal-work.org\" . \"me@work.example.com\")
            \\='(\"~/org/gcal-personal.org\" . \"me@gmail.com\"))))"
  :type '(choice (const :tag "Do not record events" nil) function)
  :group 'mu4e-autotask)

(defcustom mu4e-autotask-pre-action-hook nil
  "Hook run at the start of `mu4e-autotask-dispatch', before any rule action.
A function on this hook that signals an error aborts the dispatch.  For example,
add `org-autotask-require-org-clock' to require an active Org clock before any
automation runs."
  :type 'hook
  :group 'mu4e-autotask)

(defcustom mu4e-autotask-open-file-command "open"
  "Shell command used to open a file in its default application.
The file path is appended as a single shell-quoted argument.  The default,
\"open\", is the macOS opener."
  :type 'string
  :group 'mu4e-autotask)

(defcustom mu4e-autotask-open-delete-delay 1
  "Seconds to wait before deleting a temporary attachment after opening it.
`mu4e-autotask-open-all-attachments' hands each saved attachment to an external
opener that reads it asynchronously, then deletes the file after this delay.
Increase it if a slow-launching viewer or a large attachment is not done reading
before the file is removed."
  :type 'number
  :group 'mu4e-autotask)

;;; Rule matching and dispatch

(defun mu4e-autotask--validate-rule (rule)
  "Signal a `user-error' if RULE is malformed.
A rule may set at most one of `:sender-exact' / `:sender-match' and at most one
of `:subject-exact' / `:subject-match', and must supply a function as its
`:action-fn'."
  (when (and (plist-get rule :sender-exact)
             (plist-get rule :sender-match))
    (user-error "Both :sender-exact and :sender-match set in rule: %s"
                rule))
  (when (and (plist-get rule :subject-exact)
             (plist-get rule :subject-match))
    (user-error
     "Both :subject-exact and :subject-match set in rule: %s"
     rule))
  (unless (functionp (plist-get rule :action-fn))
    (user-error "Missing or invalid :action-fn in rule: %s" rule)))

(defun mu4e-autotask--sender-matches-rule (rule sender)
  "Return non-nil if SENDER matches the sender criteria of RULE.
RULE is a plist containing either `:sender-exact' or `:sender-match'."
  (let ((sender-exact (plist-get rule :sender-exact))
        (sender-match (plist-get rule :sender-match)))
    (or (and sender-exact (string= sender-exact sender))
        (and sender-match (string-match-p sender-match sender) t))))

(defun mu4e-autotask--subject-matches-rule (rule subject)
  "Return non-nil if SUBJECT matches the subject criteria of RULE.
RULE is a plist containing either `:subject-exact' or `:subject-match'."
  (let ((subject-exact (plist-get rule :subject-exact))
        (subject-match (plist-get rule :subject-match)))
    (or (and (not subject-exact) (not subject-match))
        (and subject-exact (string= subject-exact subject))
        (and subject-match (string-match-p subject-match subject)))))

;;;###autoload
(defun mu4e-autotask-dispatch (msg)
  "Run the matching `mu4e-autotask-rules' automation for mu4e MSG.
`mu4e-autotask-pre-action-hook' runs first; a member that signals an error
aborts the dispatch.  Signal a `user-error', without running any action, if
more than one rule matches.  Signal a `user-error' first if any rule in
`mu4e-autotask-rules' sets conflicting match criteria, regardless of whether it
would match MSG.  When no rule matches, MSG carries the mu `calendar' flag,
and `mu4e-autotask-handle-icalendar' is non-nil, run the built-in calendar
invitation handler instead.
Call this from a `mu4e-view-mode' or `mu4e-headers-mode' buffer, as the
`mu4e-view-actions' entry does: the calendar invitation handler replies via
`mu4e-compose-reply-to', which needs the message at point."
  (mapc #'mu4e-autotask--validate-rule mu4e-autotask-rules)
  (run-hooks 'mu4e-autotask-pre-action-hook)
  (let ((sender
         (mu4e-contact-email (car (mu4e-message-field msg :from))))
        (subject (mu4e-message-field msg :subject)))
    ;; From is a mandatory header (RFC 5322); a message without a parseable
    ;; sender is malformed, not ordinary input, so assert the invariant rather
    ;; than raise a `user-error'.  A missing Subject, by contrast, is normal and
    ;; `mu4e-message-field' already maps it to the empty string.
    (cl-assert (stringp sender))
    (let ((matches
           (seq-filter
            (lambda (rule)
              (and
               (mu4e-autotask--sender-matches-rule rule sender)
               (mu4e-autotask--subject-matches-rule rule subject)))
            mu4e-autotask-rules)))
      (cond
       ((null matches)
        (if (and mu4e-autotask-handle-icalendar
                 (memq 'calendar (mu4e-message-field msg :flags)))
            (mu4e-autotask--icalendar-rsvp msg)
          (message "No automation rule matched for this message")))
       ((cdr matches)
        (user-error
         "More than one rule matches this message, fix `mu4e-autotask-rules'"))
       (t
        (funcall (plist-get (car matches) :action-fn) msg))))))

;;;###autoload
(defun mu4e-autotask-initialize ()
  "Register the automation dispatcher as a mu4e view action.
Adds an \"Execute automation\" entry to `mu4e-view-actions'."
  (add-to-list
   'mu4e-view-actions '("Execute automation" . mu4e-autotask-dispatch)
   t))

;;; Reading the current message

(defun mu4e-autotask--attachment-like-p (part)
  "Return non-nil if mu4e MIME PART is an attachment, not an inline body part."
  (plist-get part :attachment-like))

(defun mu4e-autotask--attachment-parts ()
  "Return the attachment parts of the current mu4e message.
Filters `mu4e-view-mime-parts' to the parts mu4e marks as attachment-like,
excluding inline body parts (those for which mu4e invents a filename and leaves
`:attachment-like' nil).  Every returned part has a non-nil string `:filename',
so callers may match against it with `string-suffix-p' without a nil guard."
  (seq-filter
   #'mu4e-autotask--attachment-like-p (mu4e-view-mime-parts)))

(defun mu4e-autotask-raw-message (msg)
  "Return the raw contents of mu4e message MSG as a unibyte string.
Read the file literally, without coding-system decoding or end-of-line
conversion, so the returned bytes match the message as stored on disk."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally (mu4e-message-readable-path msg))
    (buffer-string)))

(defun mu4e-autotask-msg-content (mime-type)
  "Return the decoded body of the first current message part with MIME-TYPE.
Signal a `user-error' if the message has no part of MIME-TYPE."
  (let ((part
         (or (seq-find
              (lambda (part)
                (string= mime-type (plist-get part :mime-type)))
              (mu4e-view-mime-parts))
             (user-error "No %s part in this message" mime-type))))
    (mm-get-part (plist-get part :handle))))

(defun mu4e-autotask-html-content ()
  "Return the text/html body of the current mu4e message."
  (mu4e-autotask-msg-content "text/html"))

(defun mu4e-autotask-text-content ()
  "Return the text/plain body of the current mu4e message."
  (mu4e-autotask-msg-content "text/plain"))

(defun mu4e-autotask-browse-url-matching
    (regexp content &optional description group)
  "Browse the URL captured by REGEXP in CONTENT, and return it.
Match REGEXP against CONTENT and pass capture GROUP (default 1) to `browse-url'.
In the captured URL, quoted-printable soft line breaks (a trailing = before a
newline), any remaining bare CR/LF, and the HTML entity `&amp;' are removed or
decoded, after the match.  REGEXP can therefore span the folded lines of a raw
message body only when its own URL character class admits the newline (e.g.
`[^>]'); the cleanup then repairs the captured fold.  Matching is case-sensitive
regardless of the ambient `case-fold-search'.  Message \"Opening <url>\" on
success.  When REGEXP does not match, or its capture group is empty, message
about the missing DESCRIPTION (default \"URL\") and return nil."
  (save-match-data
    ;; Bind `case-fold-search' so URL matching and the case-sensitive `&amp;'
    ;; decode do not depend on the calling buffer's value.
    (let*
        ((case-fold-search nil)
         ;; Bind the raw capture first: a non-participating GROUP yields nil,
         ;; which must short-circuit before `replace-regexp-in-string' (which
         ;; rejects nil).  The newline cleanups nest inner-to-outer: strip the
         ;; quoted-printable soft break first, then any bare CR/LF, then decode
         ;; `&amp;'.  Order matters -- a bare-newline strip ahead of the soft
         ;; break would orphan its `='.
         (raw
          (and (string-match regexp content)
               (match-string (or group 1) content)))
         (url
          (and raw
               (replace-regexp-in-string
                "&amp;" "&"
                (replace-regexp-in-string
                 "[\r\n]"
                 ""
                 (replace-regexp-in-string "=\r?\n" "" raw))))))
      (if (and url (not (string-empty-p url)))
          (progn
            (message "Opening %s" url)
            (browse-url url)
            url)
        (message "Could not find the %s" (or description "URL"))
        nil))))

(defun mu4e-autotask-when-body-matches
    (msg regexp action &optional skip-message)
  "Call ACTION with MSG when REGEXP matches the raw message of MSG.
Match REGEXP against `mu4e-autotask-raw-message' of MSG (headers and body).  On
a match, call ACTION with MSG and return its value.  Otherwise message
SKIP-MESSAGE, when it is non-nil, and return nil without calling ACTION.
Matching is case-sensitive.

This guards an action function that should run only for a subset of the messages
its rule matches: a sender that sends several kinds of notification under one
subject, told apart only by a marker somewhere in the raw message."
  ;; Bind `case-fold-search' for a case-sensitive, caller-independent match.
  (let ((case-fold-search nil))
    (if (string-match-p regexp (mu4e-autotask-raw-message msg))
        (funcall action msg)
      (when skip-message
        (message "%s" skip-message))
      nil)))

(defun mu4e-autotask--part-filename-has-suffix-p (part suffix)
  "Return non-nil if the `:filename' of mu4e MIME PART ends with SUFFIX.
The comparison ignores case."
  (string-suffix-p suffix (plist-get part :filename) t))

(defun mu4e-autotask--csv-part-p (part)
  "Return non-nil if mu4e MIME PART has a .csv file name."
  (mu4e-autotask--part-filename-has-suffix-p part ".csv"))

(defun mu4e-autotask--pdf-part-p (part)
  "Return non-nil if mu4e MIME PART has a .pdf file name."
  (mu4e-autotask--part-filename-has-suffix-p part ".pdf"))

(defun mu4e-autotask-csv-part ()
  "Return the first .csv attachment part of the current message.
Only attachment parts are considered, not inline body parts.  Signal a
`user-error' if there is none."
  (or (seq-find
       #'mu4e-autotask--csv-part-p (mu4e-autotask--attachment-parts))
      (user-error "The expected .CSV attachment not found")))

(defun mu4e-autotask-pdf-part ()
  "Return the first .pdf attachment part of the current message, or nil.
Only attachment parts are considered, not inline body parts."
  (seq-find
   #'mu4e-autotask--pdf-part-p (mu4e-autotask--attachment-parts)))

(defun mu4e-autotask-save-part-file (part)
  "Save mu4e message PART to a file and return its path."
  (let ((file-path
         (mu4e-join-paths
          (plist-get part :target-dir) (plist-get part :filename))))
    (mm-save-part-to-file (plist-get part :handle) file-path)
    file-path))

(defun mu4e-autotask-save-csv-part ()
  "Save the first .csv part of the current message and return its path."
  (mu4e-autotask-save-part-file (mu4e-autotask-csv-part)))

(defun mu4e-autotask-for-each-attachment (fn)
  "Call FN with the handle and target path of each attachment of the message.
Only attachment parts are visited, not inline body parts."
  (dolist (part (mu4e-autotask--attachment-parts))
    (funcall fn
             (plist-get part :handle)
             (mu4e-join-paths
              (plist-get part :target-dir)
              (plist-get part :filename)))))

(defun mu4e-autotask--open-file (file)
  "Open FILE in its default application.
Use the program named by `mu4e-autotask-open-file-command'."
  (shell-command
   (concat
    mu4e-autotask-open-file-command " " (shell-quote-argument file))))

(defun mu4e-autotask--delete-file-after-delay (path delay)
  "Delete the file at PATH after DELAY seconds."
  (run-with-timer delay nil #'delete-file path nil))

(defun mu4e-autotask-open-all-attachments (suffix)
  "Save and open every attachment of the message whose name ends with SUFFIX."
  (mu4e-autotask-for-each-attachment
   (lambda (handle path)
     (when (string-suffix-p suffix path t)
       (mm-save-part-to-file handle path)
       (mu4e-autotask--open-file path)
       ;; Delay deletion so the async opener can read the file first; if the
       ;; tunable delay still races, replace it with lsof polling.
       (mu4e-autotask--delete-file-after-delay
        path mu4e-autotask-open-delete-delay)))))

(defun mu4e-autotask--save-part-if-jpg (handle path)
  "Save attachment HANDLE to PATH when PATH names a .jpg file."
  (when (string-suffix-p ".jpg" path t)
    (mm-save-part-to-file handle path)))

(defun mu4e-autotask-download-all-jpgs ()
  "Save every .jpg attachment of the current mu4e message."
  (mu4e-autotask-for-each-attachment
   #'mu4e-autotask--save-part-if-jpg))

;;; Sending templated mail

(cl-defstruct
 (mu4e-autotask-email-template (:copier nil))
 "An email template to be filled out and sent."
 (context
  ""
  :read-only t
  :type string
  :documentation "The mu4e context to use.")
 (to "" :read-only t :type string :documentation "To: field.")
 (subject
  ""
  :read-only t
  :type string
  :documentation "Subject: field.")
 (body "" :read-only t :type string :documentation "Email body."))

;;;###autoload
(defun mu4e-autotask-do-send-email (to success-fn)
  "Ask to send the message in the current compose buffer, to TO.
The \"To:\" field of the email must already be filled out; TO is used only for
the confirmation prompt.  Call SUCCESS-FN only after the message is sent; on a
declined send, discard the compose buffer and signal a `user-error'.  An error
or quit SUCCESS-FN signals is downgraded to a warning: the message is already
sent at that point, so the failure of a follow-up action must not present as a
failed send.

This is the building block behind `mu4e-autotask-send-email', exposed for action
functions that compose a message themselves (e.g. a forward or reply) and then
want the same confirm-and-send behavior."
  (let ((subject (message-field-value "Subject")))
    (if (y-or-n-p
         (format "Send email to %s with subject %s? " to subject))
        (progn
          ;; The buffer is destroyed right after the send attempt, so set the
          ;; buffer-local hook value without bothering to clean it up.
          (add-hook
           'message-sent-hook
           (lambda ()
             ;; `message-sent-hook' runs after the send but before the
             ;; send machinery's cleanup; an escaping error or quit would
             ;; leave the sent message in a re-sendable compose buffer.
             (condition-case err
                 (funcall success-fn)
               ((error quit)
                (display-warning
                 'mu4e-autotask
                 (format
                  "Message sent, but the follow-up action failed: %s"
                  (error-message-string err))
                 :error))))
           nil
           t)
          (message-send-and-exit))
      ;; The body and attachments leave the compose buffer modified; declining
      ;; the send means discarding it, so kill unconditionally rather than
      ;; prompting a second "kill anyway?" confirmation.
      (let ((message-kill-buffer-query nil))
        (message-kill-buffer))
      (user-error "Cancelled"))))

;;;###autoload
(defun mu4e-autotask-send-email (template attachments success-fn)
  "Compose and send mail from TEMPLATE, call SUCCESS-FN once it is sent.
TEMPLATE is a `mu4e-autotask-email-template'.  ATTACHMENTS is a list of file
paths to attach.  SUCCESS-FN is called only after the message is sent."
  (mu4e-context-switch
   'force (mu4e-autotask-email-template-context template))
  (let ((mu4e-compose-context-policy nil)
        (to (mu4e-autotask-email-template-to template)))
    (mu4e-compose-new
     to (mu4e-autotask-email-template-subject template))
    (message-goto-body)
    (insert (mu4e-autotask-email-template-body template))
    (dolist (attachment attachments)
      (mml-attach-file attachment))
    (mu4e-autotask-do-send-email to success-fn)))

;;; iCalendar RSVP

(declare-function org-gcal-post-at-point "org-gcal")
;; Value-less declarations: org-gcal is a soft dependency loaded at use time.
;; Loading it binds these; the test sandbox, which stubs org-gcal instead of
;; loading it, let-binds them.
(defvar org-gcal-drawer-name)
(defvar org-gcal-calendar-id-property)

(defun mu4e-autotask--find-mime-handle (handle mime-type)
  "Return the first leaf of the MIME HANDLE tree with MIME-TYPE, or nil.
A handle whose car is a string is a multipart; recurse into its children."
  (if (stringp (car handle))
      (seq-some
       (lambda (child)
         (mu4e-autotask--find-mime-handle child mime-type))
       (cdr handle))
    (when (string= mime-type (mm-handle-media-type handle))
      handle)))

(defun mu4e-autotask--dissect-message (msg)
  "Return the MIME handle tree of mu4e message MSG read from its file."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally (mu4e-message-readable-path msg))
    (mm-dissect-buffer t)))

(defmacro mu4e-autotask--with-mime-handle (spec &rest body)
  "Bind a fresh MIME handle from a message file and run BODY.
SPEC is (VAR MSG MIME-TYPE): bind VAR to the first MIME-TYPE leaf of MSG's
dissected message file.  Dissecting the file anew keeps the handle live
regardless of any stale cached view state.  All dissected parts are destroyed
when BODY exits, so VAR must not escape BODY.  Signal a `user-error' when the
message has no MIME-TYPE part."
  (declare (indent 1) (debug ((symbolp form form) body)))
  (let ((dissected (make-symbol "dissected"))
        (mtype (make-symbol "mime-type"))
        (var (nth 0 spec))
        (msg (nth 1 spec))
        (mime-type (nth 2 spec)))
    `(let ((,dissected (mu4e-autotask--dissect-message ,msg))
           (,mtype ,mime-type))
       (unwind-protect
           (let ((,var
                  (or (mu4e-autotask--find-mime-handle
                       ,dissected ,mtype)
                      (user-error "No %s part in this message"
                                  ,mtype))))
             ,@body)
         (mm-destroy-parts ,dissected)))))

(defun mu4e-autotask--icalendar-read-status ()
  "Prompt for the RSVP status of a calendar invitation.
Return one of the symbols `accepted', `tentative' or `declined'; signal a
`user-error' when quit is chosen."
  (pcase (car
          (read-multiple-choice
           "RSVP to this invitation? "
           '((?a "accept")
             (?t "tentative")
             (?d "decline")
             (?q "quit"))))
    (?a 'accepted)
    (?t 'tentative)
    (?d 'declined)
    (_ (user-error "RSVP cancelled"))))

(defun mu4e-autotask--icalendar-organizer (msg event)
  "Return the organizer email address of EVENT from invitation MSG.
Fall back to the Reply-To and From addresses of MSG; signal a `user-error'
when no address is found."
  (let ((organizer (gnus-icalendar-event:organizer event)))
    (or (and organizer (not (zerop (length organizer))) organizer)
        (mu4e-contact-email (car (mu4e-message-field msg :reply-to)))
        (mu4e-contact-email (car (mu4e-message-field msg :from)))
        (user-error "Cannot find the meeting organizer"))))

(defun mu4e-autotask--icalendar-fill-reply-buffer (reply)
  "Return a fresh buffer holding the iTIP REPLY text.
Each RSVP gets its own buffer rather than sharing gnus-icalendar's: the
draft attaches the buffer by name and mml reads it only at send time, so a
shared buffer would let a draft surviving past the flow mail a later
invitation's reply.  Fold lines longer than 75 octets in the UTF-8 wire
encoding, per RFC 5545 section 3.1, splitting only between characters.
REPLY is decoded text: `gnus-icalendar-with-decoded-handle' already applied
the part's charset."
  (with-current-buffer (generate-new-buffer " *mu4e-autotask-rsvp*")
    (insert reply)
    (goto-char (point-min))
    ;; RFC 5545 limits the physical line, so a continuation's leading fold
    ;; space counts toward its 75 octets.  `string-bytes' of a one-character
    ;; string equals the character's UTF-8 width, the encoding the part is
    ;; attached in.
    (while (not (eobp))
      (let ((octets 0))
        (while-let (((not (eolp)))
                    (width (string-bytes (string (char-after))))
                    ((<= (+ octets width) 75)))
          (cl-incf octets width)
          (forward-char))
        (if (eolp)
            (forward-line 1)
          (insert "\n ")
          (forward-line 0))))
    (current-buffer)))

;; This intentionally parallels mu4e's own `mu4e-icalendar-reply' rather
;; than delegating to it: loading mu4e-icalendar installs global advice on
;; `gnus-icalendar-reply'; its gnus-icalendar org-capture/diary side effects
;; would record the event before the send is confirmed, conflicting with the
;; org-gcal recording here; and it neither returns the organizer nor leaves
;; the compose buffer current as `mu4e-autotask-do-send-email' requires.
(defun mu4e-autotask--icalendar-compose-rsvp (msg handle event status)
  "Compose the iTIP STATUS reply to the invitation EVENT from MSG.
HANDLE is the text/calendar MIME handle the reply is built from.  Leave the
compose buffer current and return the organizer address the reply goes to."
  (let* ((gnus-icalendar-additional-identities
          (mu4e-personal-addresses 'no-regexp))
         (reply
          (gnus-icalendar-with-decoded-handle
           handle
           (gnus-icalendar-event-reply-from-buffer
            (current-buffer) status (gnus-icalendar-identities))))
         (organizer (mu4e-autotask--icalendar-organizer msg event))
         (message-signature nil)
         (message-cite-function #'mu4e-message-cite-nothing))
    (unless reply
      (user-error "Cannot build the iTIP reply for this invitation"))
    (let ((reply-buffer
           (mu4e-autotask--icalendar-fill-reply-buffer reply))
          (hook-installed nil))
      ;; Until the kill-buffer-hook below ties REPLY-BUFFER's lifetime to
      ;; the draft, an error from `mu4e-compose-reply-to' or the mml calls
      ;; would orphan it; kill it on such a non-local exit.
      (unwind-protect
          (progn
            ;; mu4e bug: `mu4e--view-add-mime-icons' assumes every
            ;; `gnus-data' text property holds an MM handle, but the
            ;; gnus-icalendar RSVP buttons store (HANDLE STATUS EVENT)
            ;; there, so the citation render inside the reply compose
            ;; crashes on any text/calendar message.  The icons are
            ;; cosmetic; skip the pass.
            (if (fboundp 'mu4e--view-add-mime-icons)
                (cl-letf (((symbol-function
                            'mu4e--view-add-mime-icons)
                           #'ignore))
                  (mu4e-compose-reply-to organizer))
              (mu4e-compose-reply-to organizer))
            (message-goto-body)
            (mml-insert-multipart "alternative")
            (mml-insert-empty-tag 'part 'type "text/plain")
            (mml-attach-buffer
             (buffer-name reply-buffer)
             "text/calendar; method=REPLY; charset=UTF-8")
            ;; mml reads the attached buffer at send time, so the reply
            ;; buffer must live exactly as long as the draft; kill them
            ;; together.
            (add-hook 'kill-buffer-hook
                      (lambda ()
                        (when (buffer-live-p reply-buffer)
                          (kill-buffer reply-buffer)))
                      nil t)
            (setq hook-installed t))
        (unless hook-installed
          (when (buffer-live-p reply-buffer)
            (kill-buffer reply-buffer)))))
    organizer))

(defun mu4e-autotask--icalendar-sanitize-drawer-line (line)
  "Return LINE made safe for an org-gcal drawer, or nil to drop it.
Drop a line that would close the drawer early; escape a column-zero heading
star with org-gcal's ✱ convention, which its drawer reader converts back to
*, so descriptions round-trip losslessly."
  ;; Org closes drawers case-insensitively regardless of user
  ;; configuration, so the `:END:' match must not depend on the ambient
  ;; `case-fold-search'.
  (let ((case-fold-search t))
    (cond
     ((string-match-p "^[ \t]*:END:[ \t]*$" line)
      nil)
     ((string-prefix-p "*" line)
      (concat "✱" (substring line 1)))
     (t
      line))))

(defun mu4e-autotask--icalendar-sanitize-drawer-text (text)
  "Return TEXT safe to insert inside an org-gcal drawer."
  ;; A trailing newline would split into a final empty element and, with
  ;; the caller's own newline, leave a blank line before the drawer's
  ;; `:END:'; trim it.  Interior blank lines are intentional and kept.
  (mapconcat #'identity
             (delq
              nil
              (mapcar
               #'mu4e-autotask--icalendar-sanitize-drawer-line
               (split-string (replace-regexp-in-string
                              "\n+\\'" "" text)
                             "\n")))
             "\n"))

(defun mu4e-autotask--icalendar-org-gcal-available-p ()
  "Return non-nil when org-gcal can be used to record an event.
Load it on demand; `ignore-errors' because NOERROR only covers a missing
file, not a load failure inside org-gcal's dependencies.  The `fboundp'
fallback covers environments where the function exists without the package
\(e.g. a test stub)."
  (or (ignore-errors
        (require 'org-gcal nil t))
      (fboundp 'org-gcal-post-at-point)))

(defun mu4e-autotask--icalendar-require-org-gcal ()
  "Load org-gcal, signaling a `user-error' when it is unavailable."
  (unless (mu4e-autotask--icalendar-org-gcal-available-p)
    (user-error "Recording the event requires org-gcal")))

(defun mu4e-autotask--icalendar-create-org-gcal-event
    (target title time description location)
  "Record a calendar event via org-gcal.
TARGET is a cons (ORG-FILE . CALENDAR-ID); the entry titled TITLE, located at
LOCATION, with the org timestamp TIME and body DESCRIPTION is appended to
ORG-FILE and posted to the CALENDAR-ID Google calendar.  Signal a
`user-error' when a buffer visits ORG-FILE with unsaved changes: recording
saves the file, and the user's unrelated edits must not be saved as a side
effect."
  ;; Load org-gcal before writing: its drawer and property name
  ;; customizations must be in effect for the entry text.
  (mu4e-autotask--icalendar-require-org-gcal)
  (let ((visiting (find-buffer-visiting (car target))))
    (when (and visiting (buffer-modified-p visiting))
      (user-error "%s has unsaved changes; not recording the event"
                  (car target))))
  (with-current-buffer (find-file-noselect (car target))
    ;; The user may already visit this buffer narrowed and with point
    ;; anywhere; append at the true end of file and restore their view.
    ;; The widen must cover `org-gcal-post-at-point', which reads the
    ;; entry at point.
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-max))
        (unless (bolp)
          (insert "\n"))
        (insert
         "* "
         title
         "\n"
         ":PROPERTIES:\n"
         ":"
         org-gcal-calendar-id-property
         ": "
         (cdr target)
         "\n")
        (when (and location (not (zerop (length location))))
          (insert
           ":LOCATION: "
           (replace-regexp-in-string "[\r\n]+" ", " location)
           "\n"))
        (insert ":END:\n" ":" org-gcal-drawer-name ":\n" time "\n")
        ;; Test the sanitized text, not the raw description: a description
        ;; of only newlines is non-empty yet sanitizes to "", which must
        ;; not be inserted as a blank line before the drawer's `:END:'.
        (when description
          (let ((sanitized
                 (mu4e-autotask--icalendar-sanitize-drawer-text
                  description)))
            (unless (string-empty-p sanitized)
              (insert sanitized "\n"))))
        (insert ":END:\n")
        (save-buffer)
        (org-gcal-post-at-point)))))

(defun mu4e-autotask--icalendar-rsvp (msg)
  "Run the built-in RSVP flow for the calendar invitation in MSG."
  (mu4e-autotask--with-mime-handle (handle msg "text/calendar")
    (let ((event
           (gnus-icalendar-event-from-handle
            handle (mu4e-personal-addresses 'no-regexp))))
      (unless (and event (gnus-icalendar-event-request-p event))
        (user-error "Not a calendar meeting request"))
      ;; gnus-icalendar derives the reply attendee's CN from the
      ;; `user-full-name' variable; an empty name yields an empty CN, which
      ;; Emacs 31's stricter icalendar validation rejects when building the
      ;; reply.  Require the name up front so the failure is an actionable
      ;; message rather than an obscure validation error after prompting.
      (unless (and user-full-name
                   (not (string-blank-p user-full-name)))
        (user-error
         "Sending a calendar RSVP requires setting `user-full-name'"))
      ;; Capture the event fields as plain strings: HANDLE, and the EVENT
      ;; derived from it, belong to the enclosing macro, so the send success
      ;; function must not close over them.
      (let*
          ((status (mu4e-autotask--icalendar-read-status))
           ;; The summary slot may be nil (SUMMARY is optional in RFC
           ;; 5545) or contain newlines, neither of which an org heading
           ;; tolerates.  A missing Subject is the empty string: never
           ;; nil, `mu4e-message-field' already maps it.
           (title
            (let ((summary (gnus-icalendar-event:summary event))
                  (subject (mu4e-message-field msg :subject)))
              (cond
               ((and summary (not (string-blank-p summary)))
                (replace-regexp-in-string "[\r\n]+" ", " summary))
               ((not (string-blank-p subject))
                subject)
               (t
                "(no title)"))))
           (time (gnus-icalendar-event:org-timestamp event))
           (description (gnus-icalendar-event:description event))
           (location (gnus-icalendar-event:location event))
           ;; Capture the target function in effect when the user answered
           ;; the prompt: the send success function below must not re-read
           ;; the variable, which could change before the hook fires.
           (target-fn mu4e-autotask-icalendar-event-target-function)
           (recording (and target-fn (not (eq status 'declined))))
           ;; When org-gcal is absent the target must be resolved before
           ;; the send (below) to fail fast; cache that result so the
           ;; post-send hook reuses it rather than calling the
           ;; (possibly side-effecting) target function a second time.
           (target-resolved nil)
           (pre-send-target nil))
        ;; A missing org-gcal is fully predictable, so check before the
        ;; send: an RSVP must not go out only for the recording to fail.
        ;; But the target function may decline this message (returning nil),
        ;; in which case nothing is recorded and org-gcal is not needed;
        ;; only error here, before the send, when org-gcal is absent and
        ;; the target actually wants to record.
        (when (and recording
                   (not
                    (mu4e-autotask--icalendar-org-gcal-available-p)))
          (setq
           pre-send-target (funcall target-fn msg)
           target-resolved t)
          (when pre-send-target
            (user-error "Recording the event requires org-gcal")))
        (mu4e-autotask-do-send-email
         (mu4e-autotask--icalendar-compose-rsvp
          msg handle event status)
         (lambda ()
           ;; Resolve the target only after the send is confirmed, unless
           ;; the pre-send org-gcal check already resolved it: the target
           ;; function may have side effects, so neither a cancelled send
           ;; nor that check must (re)run it.
           (when-let* ((target
                        (if target-resolved
                            pre-send-target
                          (and recording (funcall target-fn msg)))))
             (mu4e-autotask--icalendar-create-org-gcal-event
              target title time description location))))))))

(provide 'mu4e-autotask)
;;; mu4e-autotask.el ends here
