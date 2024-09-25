;;; my-lib.el --- helpers for other code.  -*- lexical-binding: t; -*-

;;; Commentary:

;; This contains internal helpers used elsewhere. Very much personalized.

;;; Code:

;;; Regexps for looking up information in e-mails

(defconst dotfiles--muspy-album-release-date-and-title
  "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\): \\(.*? - .*?\\) (Album)")

(defconst dotfiles--gh-view-run-results "^View results: \\(.*\\)")

(defconst dotfiles--gh-pr-in-subject "^.*\(PR #\\([0-9]+\\)\)$")
(defconst dotfiles--gh-url-prefix "https://github.com/")
(defconst dotfiles--gh-url-id-matcher
  (concat dotfiles--gh-url-prefix ".*/.*/pull/\\([0-9]+\\)"))
(defconst dotfiles--gh-url-id-format
  (concat "\\(" dotfiles--gh-url-prefix ".*/.*/pull/%s\\)"))
(defconst dotfiles--gh-closed-pr-url-format
  (concat "Closed .*" dotfiles--gh-url-id-format))
(defconst dotfiles--gh-commented-pr-url-format
  (concat dotfiles--gh-url-id-format "#.*"))
(defconst dotfiles--gh-org-and-project
  (concat dotfiles--gh-url-prefix "\\(.*\\)/pull/[0-9]+"))
(defconst dotfiles--gh-repo "\\(https://github.com/.*/.*\\)/pull/[0-9]+")

(defconst dotfiles--gh-release-in-subject
  "^\\[\\(.*\\)/\\(.*\\)\\] Release \\(.*?\\) - \\(.*?\\)$")

(defconst dotfiles--gh-issue-url
  "Reply to this email directly or view it on GitHub:\n\\(.*\\)$")

;;; string helpers

;; If dependencies are OK, then use `string-join' instead.
(defun dotfiles--concat-all (s)
  "Concatenates all strings in S with spaces."
  (mapconcat 'identity s " "))

;;; regex helpers

(defun dotfiles--string-match-string (regex string)
  "Return the 1st match for REGEX in STRING, nil otherwise."
  (when (string-match regex string)
    (match-string 1 string)))

;;; File helpers

(defun dotfiles--find-latest-pdf (directory)
  "Find the most recently created .pdf file in DIRECTORY."
  (let* ((files (directory-files-and-attributes directory nil "\\.pdf$" t))
         (sorted-files (sort files (lambda (a b)
                                     (time-less-p (nth 6 b) (nth 6 a))))))
    (if sorted-files
        (file-name-nondirectory (car (car sorted-files)))
      (user-error "No PDFs found in %s" directory))))

(defun dotfiles--read-pdf (dir prompt confirmation)
  "Read a PDF file in DIR with PROMPT, then confirm it with CONFIRMATION.
While reading, suggest to complete with the latest PDF in the directory.
CONFIRMATION must have a %s argument which will be replaced with the file path.
Returns the path or nil."
  (let* ((latest-pdf (dotfiles--find-latest-pdf dir))
         (pdf-fn (read-file-name prompt dir latest-pdf t latest-pdf))
         (pdf-path (expand-file-name pdf-fn dir)))
    (when (y-or-n-p (format confirmation pdf-path))
      pdf-path)))

(defun dotfiles--sibling-path (path fn)
  "For a given PATH, return a full path for FN in the same directory."
  (file-name-concat (file-name-directory path) fn))

;;; External program helpers

(defun dotfiles--open-file (file)
  "Open the FILE in its default app."
  (shell-command (concat "open " (shell-quote-argument file))))

;; Command-line program helpers

(defun dotfiles--run-program-process-output (program args success-fn)
  "Run PROGRAM with ARGS, executing SUCCESS-FN on zero exit.
ARGS must be a list of strings passed to PROGRAM.
SUCCESS-FN is executed on zero exit with a single string argument containing the
output of execution.
In the case of non-zero exit code it is printed as a user error together with
any output."
  (with-temp-buffer
    (let* ((exit-code (apply #'call-process program nil t nil args))
           (output (buffer-string)))
      (when (/= 0 exit-code)
        (user-error "%s %s failed with exit code %d and output %s" program
                    (dotfiles--concat-all args) exit-code output))
      (message "Output from %s %s:\n%s" program (dotfiles--concat-all args)
               output)
      (funcall success-fn output))))

(defun dotfiles--run-program (program args)
  "Run PROGRAM with ARGS, sending its output to the message buffer.
ARGS must be a list of strings passed to PROGRAM. In the case of non-zero exit
code it is printed as user error."
  (dotfiles--run-program-process-output program args (lambda (_))))

;; Command-line "gh" utility helper

(defun dotfiles--gh-get (args)
  "Run gh with ARGS, return its output with the final newline trimmed.
ARGS must be properly quoted if needed."
  ;; We want to remove the final character and the final character only. Hence,
  ;; `substring' instead of i.e. `string-trim-right'.
  (substring (shell-command-to-string (concat "gh " args)) 0 -1))

;; Google Drive helpers through "gdrive" command-line utility

(defmacro dotfiles--with-gdrive-file-as-txt (gdrive-id &rest body)
  "Export Google Drive document GDRIVE-ID as text and execute BODY.
A temporary buffer is created to hold the exported text and the body is executed
there. The temporary file is automatically cleaned up after BODY execution."
  (declare (indent 1) (debug t))
  ;; In another context this would be a security hole due the race window
  ;; between generating a temp name and using it. Here it's a single trusted
  ;; user invocation, thus OK.
  `(let ((temp-file-path (concat (make-temp-name
                                  (expand-file-name "gdrive-export-"
                                                    temporary-file-directory))
                                 ".txt")))
     (unwind-protect
         (progn
           (dotfiles--run-program "gdrive" (list "files" "export"
                                                 ,gdrive-id
                                                 temp-file-path))
           (with-temp-buffer
             (insert-file-contents temp-file-path)
             ,@body))
       (when (file-exists-p temp-file-path)
         (delete-file temp-file-path)))))

;;; Buffer management helpers

(defun dotfiles--get-org-buffer (name)
  "Get the buffer for an `org' file with NAME."
  (or (find-buffer-visiting name)
      (find-file-noselect name)))

(defmacro dotfiles--in-org-buffer (name &rest body)
  "Execute the forms in BODY with NAME `org' buffer temporarily current."
  (declare (indent 1) (debug t))
  `(with-current-buffer (dotfiles--get-org-buffer ,name)
     ,@body))

;;; `mu4e' helpers

(require 'seq)
(require 'mu4e-mime-parts)
(require 'mu4e-message)

(defun dotfiles--get-raw-message (msg)
  "Get the raw `mu4e' message MSG as string."
  (with-temp-buffer (insert-file-contents (mu4e-message-readable-path msg))
                    (buffer-string)))

(defun dotfiles--get-qp-encoded-html-part ()
  "For a `mu4e' message, get its first quoted-printable-encoded HTML part."
  (seq-find (lambda (part)
              (and (string= "text/html" (plist-get part :mime-type))
                   (eq 'quoted-printable (plist-get part :encoding))))
            (mu4e-view-mime-parts)))

(defun dotfiles--get-mu4e-msg-csv-part ()
  "For a `mu4e' message, get its first .csv attachment part, if any."
  (or (seq-find (lambda (part)
                  (string-suffix-p ".csv" (plist-get part :filename) t))
                (mu4e-view-mime-parts))
      (user-error "The expected .CSV attachment not found")))

(defun dotfiles--get-mu4e-msg-pdf-part ()
  "For a `mu4e' message, get its first .pdf attachment part, if any."
  (seq-find (lambda (part)
              (string-suffix-p ".pdf" (plist-get part :filename) t))
            (mu4e-view-mime-parts)))

(defun dotfiles--save-mu4e-msg-part-file (part)
  "For a `mu4e' message PART, save it as a file and return its path."
  (let* ((base-dir (plist-get part :target-dir))
         (file-path (mu4e-join-paths base-dir (plist-get part :filename))))
    (mm-save-part-to-file (plist-get part :handle) file-path)
    file-path))

(defun dotfiles--save-mu4e-msg-csv-part ()
  "For a `mu4e' mssage, save its first .csv part and return the path."
  (let ((csv-part (dotfiles--get-mu4e-msg-csv-part)))
    (dotfiles--save-mu4e-msg-part-file csv-part)))

(defun dotfiles--get-mu4e-msg-html-content ()
  "Get the current `mu4de' message HTML content."
  (mm-get-part (plist-get (dotfiles--get-qp-encoded-html-part) :handle)))

(defun dotfiles--for-each-attachment (fn)
  "Call FN for each attachment with its handle and path."
  (let ((mime-parts (mu4e-view-mime-parts)))
    (dolist (part mime-parts)
      (let* ((attachment-handle (plist-get part :handle))
             (file-name (plist-get part :filename))
             (file-path (mu4e-join-paths (plist-get part :target-dir)
                                         file-name)))
        (funcall fn attachment-handle file-path)))))

(defun dotfiles--open-mu4e-all-attachments (suffix)
  "Download all attachments with file name SUFFIX from a `mu4e' message."
  (dotfiles--for-each-attachment
   (lambda (handle path)
     (when (string-suffix-p suffix path t)
       (mm-save-part-to-file handle path)
       (dotfiles--open-file path)
       ;; TODO(laurynas): the above is asynchronuous. We cannot make it
       ;; synchronuous, but at least replace the blocking `sleep-for' below with
       ;; a `run-with-timer' call instead.
       (sleep-for 1)
       (delete-file path nil)))))

(defun dotfiles--download-mu4e-all-jpgs ()
  "Download all .jpg attachments from a `mu4e' message."
  (dotfiles--for-each-attachment
   (lambda (handle path)
     (when (string-suffix-p ".jpg" path t)
       (mm-save-part-to-file handle path)))))

(cl-defstruct (my-email-template (:copier nil))
  "An email template to be filled out and sent."
  (context
   nil :read-only t :type string :documentation "The `mu4e' context to use")
  (to
   nil :read-only t :type string :documentation "To: field")
  (subject
   nil :read-only t :type string :documentation "Subject: field")
  (body
   "" :read-only t :type string :documentation "E-mail body"))

(require 'mu4e-compose)
(require 'mu4e-draft)

(defun dotfiles--do-send-email (to success-fn)
  "Ask to send an already filled-out email to TO, call SUCCESS-FN on success.
\"To:\" field in the e-mail must be already filled out, as TO argument is only
used for diagnostics.  SUCCESS-FN is only called on success."
  (let ((subject (message-field-value "Subject")))
    (if (y-or-n-p (format "Send email to %s with subject %s?" to subject))
        (progn
          ;; Since the buffer will get destroyed immediately after the send
          ;; attempt, adjust the buffer-local hook value and don't bother with
          ;; cleaning it up.
          (add-hook 'message-sent-hook success-fn nil t)
          (message-send-and-exit))
      (message-kill-buffer)
      (user-error "Cancelled"))))

(defun dotfiles--send-email (template attachments success-fn)
  "Send a mail using TEMPLATE with ATTACHMENTS, call SUCCESS-FN on success."
  (mu4e-context-switch nil (my-email-template-context template))
  (let ((mu4e-compose-context-policy nil)
        (to (my-email-template-to template))
        (subject (my-email-template-subject template)))
    (mu4e-compose-new to subject)
    (message-goto-body)
    (insert (my-email-template-body template))
    (dolist (attachment attachments)
      (mml-attach-file attachment))
    (dotfiles--do-send-email to success-fn)))

;; GitHub helpers

(defun dotfiles--get-gh-name-from-url (url)
  "Get the GitHub organization/project from a PR URL."
  (dotfiles--string-match-string dotfiles--gh-org-and-project url))

;; GitHub / `mu4e' helpers

(defun dotfiles--get-closed-pr-url (pr-id html-content)
  "Return the URL of a closed GitHub PR with PR-ID in HTML-CONTENT or nil."
  (let ((closed-pr-url-regex (format dotfiles--gh-closed-pr-url-format pr-id)))
    (dotfiles--string-match-string closed-pr-url-regex html-content)))

(defun dotfiles--get-commented-pr-url (pr-id html-content)
  "Return the URL of a commented GitHub PR with PR-ID in HTML-CONTENT or nil."
  (let ((commented-pr-url-regex
         (format dotfiles--gh-commented-pr-url-format pr-id)))
    (dotfiles--string-match-string commented-pr-url-regex html-content)))

(defun dotfiles--parse-gh-release-subject (subject)
  "Parse out GitHub release email SUBJECT into a plist."
  (unless (string-match dotfiles--gh-release-in-subject subject)
    (user-error "Subject %s did not match against %s" subject
                dotfiles--gh-release-in-subject))
  (let ((gh-org (match-string 1 subject))
        (gh-project (match-string 2 subject))
        (rel-tag (match-string 3 subject))
        (rel-tag-2 (match-string 4 subject)))
    (unless (string= rel-tag rel-tag-2)
      (user-error "Unrecognized GitHub Release email subject format: %s"
                  subject))
    (list :gh-org gh-org :gh-project gh-project
          :gh-name (concat gh-org "/" gh-project) :rel-tag rel-tag)))

(defun dotfiles--get-run-results-url (msg)
  "Get a GitHub run URL from a `mu4e' MSG."
  (let ((raw-message (dotfiles--get-raw-message msg)))
    (dotfiles--string-match-string dotfiles--gh-view-run-results raw-message)))

(defun dotfiles--get-gh-issue-url (msg)
  "Get a GitHub issue URL from a `mu4e' MSG."
  (let ((raw-message (dotfiles--get-raw-message msg)))
    (dotfiles--string-match-string dotfiles--gh-issue-url raw-message)))

(defun dotfiles--get-pr-id (msg)
  "Return the PR id from a `mu4e' MSG subject."
  (let ((subject (mu4e-message-field msg :subject)))
    (dotfiles--string-match-string dotfiles--gh-pr-in-subject subject)))

;;; Development automation helpers

(require 'cl-lib)

(cl-defstruct (my-dev-project (:copier nil))
  "A single development project for the purposes of automation."
  (name
   nil :read-only t :type string
   :documentation "The name of the project, used in `org' tasks.")
  (gh-name
   nil :read-only t :type string
   :documentation "GitHub organization and project name, slash-separated.")
  (org-file
   nil :read-only t :type string
   :documentation "The `org' file.")
  (branch-root
   nil :read-only t :type string
   :documentation "The root path of the branches.")
  (main-branch-checkout
   nil :read-only t :type string
   :documentation
   "The name of the main branch checkout directory under `:branch-root'.")
  (push-remote
   nil :read-only t :type string
   :documentation "My push remote.")
  (prs-are-mine
   nil :read-only t :type boolean
   :documentation "Whether I handle the PRs myself.")
  (pr-waitingfor-template
   nil :read-only t :type string
   :documentation "An `org' waitingfor template for PRs.
The %s must be present and is substituted with a PR branch name.")
  (post-pr-url
   nil :read-only t :type string
   :documentation "An optional URL to visit after closing a PR."))

(defvar my-projects)

(defun dotfiles--find-project-by-name (name)
  "Find a development project by its NAME."
  (or (cl-find name my-projects :test #'string= :key #'my-dev-project-name)
      (user-error "Project %s not configured in `my-projects'" name)))

(defun dotfiles--find-project-by-gh (gh-name)
  "Find a development project by its GitHub name GH-NAME."
  (or (cl-find gh-name my-projects :test #'string= :key
               #'my-dev-project-gh-name)
      (user-error "GitHub project %s not configured in `my-projects'" gh-name)))

(defun dotfiles--find-project-for-cwd ()
  "Find a development project for the current working directory."
  (let ((gh-name (dotfiles--gh-get
                  "repo view --json nameWithOwner -q '.nameWithOwner'")))
    (unless gh-name
      (user-error "Could not find a GitHub project in %s" default-directory))
    (dotfiles--find-project-by-gh gh-name)))

(defun dotfiles--get-project-push-remote (project)
  "Get the push remote for PROJECT."
  (or (my-dev-project-push-remote project)
      (user-error "Project %s misconfigured in `my-projects'"
                  (my-dev-project-name project))))

(defun dotfiles--get-project-branch-root (project)
  "Get the branch root directory for PROJECT."
  (or (my-dev-project-branch-root project)
      (user-error "Project %s misconfigured in `my-projects'")))

(defun dotfiles--get-project-main-branch-dir (project)
  "Get the directory of the main branch checkout for PROJECT."
  (let ((main-branch-checkout (my-dev-project-main-branch-checkout project)))
    (unless main-branch-checkout
      (user-error "Project %s misconfigured in `my-projects'"
                  (my-dev-project-name project)))
    (concat (dotfiles--get-project-branch-root project) main-branch-checkout)))

(defun dotfiles--format-waitingfor-task-title (project branch-name)
  "Format the `org' task title for a PR of BRANCH-NAME in PROJECT."
  (let ((format-string (my-dev-project-pr-waitingfor-template project)))
    (unless format-string
      (user-error "Project %s misconfigured in `my-projects'"
                  (my-dev-project-name project)))
    (format format-string (concat "=" branch-name "="))))

(defun dotfiles--visit-post-pr-url (project)
  "Visit the URL for a PROJECT after a PR."
  (when-let ((post-pr-url (my-dev-project-post-pr-url project)))
    (browse-url post-pr-url)))

(defun dotfiles--create-pr (project branch-name)
  "Create a new PR from the current branch with provided data.
Pushes the branch to my remote first. The needed data are PROJECT and
BRANCH-NAME. Returns the URL of this PR."
  ;; TODO(laurynas): how to sync the push remote with `magit'?
  (let* ((remote-name (dotfiles--get-project-push-remote project))
         ;; Prefix `branch-name' with fork org per
         ;; https://github.com/cli/cli/issues/2691#issuecomment-1419845247
         (gh-args
          (format
           "repo view $(git remote get-url %s) --json owner -q .owner.login"
           remote-name))
         (gh-my-org (dotfiles--gh-get gh-args))
         (gh-head-arg (concat gh-my-org ":" branch-name))
         (pr-create-args `("pr" "create" "--fill" "--head" ,gh-head-arg))
         (pr-is-mine (my-dev-project-prs-are-mine project))
         (result nil))
    (dotfiles--run-program
     "git" `("push" "--force-with-lease" "-u" ,remote-name ,branch-name))
    (when pr-is-mine
      (setq pr-create-args (append pr-create-args '("-a" "@me"))))
    (dotfiles--run-program-process-output
     "gh" pr-create-args (lambda (output)
                           (setq result
                                 (car (last (split-string output "\n" t))))))
    result))

(cl-defstruct (my-3rd-party-submodule (:copier nil))
  "A mapping from a GitHub project to my submodule importing it."
  (3p-gh-name
   nil :read-only t :type string
   :documentation "3rd party GitHub organization and project name.")
  (project-name
   nil :read-only t :type :string
   :documentation
   "My project name that include the 3rd party project submodule.")
  (path
   nil :read-only t :type :string
   :documentation "Path to the submodule in my project."))

(defvar my-3rd-party-submodules)

(defun dotfiles--find-3rd-party-submodule (gh-name)
  "Find a 3rd party submodule by GH-NAME."
  (or (cl-find gh-name my-3rd-party-submodules :test #'string= :key
               #'my-3rd-party-submodule-3p-gh-name)
      (message "Nothing found in `my-3rd-party-submodules' for %s" gh-name)))

;;; `org' helpers

(require 'org-element)

(defun dotfiles--org-headline-has-url (headline url)
  "Return the HEADLINE if it has the URL property with the given value."
  (let ((url-property-value (org-element-property :URL headline)))
    (and url-property-value (string= url url-property-value)
         headline)))

(defun dotfiles--find-org-node-with-url-property-in-buffer (url)
  "Find an Org node with a given URL property value in the current buffer."
  (org-element-map (org-element-parse-buffer) 'headline
    (lambda (headline)
      (dotfiles--org-headline-has-url headline url)) nil t))

(defun dotfiles--find-org-node-with-url-property (url)
  "Find the Org node with a given URL property value across `org-agenda-files'."
  (let ((files (org-agenda-files))
        (found nil))
    (while (and files (not found))
      (let* ((file (pop files))
             (buffer (or (find-buffer-visiting file)
                         (find-file-noselect file t)))
             (node (with-current-buffer buffer
                     (dotfiles--find-org-node-with-url-property-in-buffer url))))
        (when node
          (setq found (list :buffer buffer :headline node)))))
    found))

(defmacro dotfiles--with-org-node-with-url (url &rest body)
  "Go to the `org' node with the URL property value, execute the forms of BODY."
  (declare (indent 1) (debug t))
  `(let ((org-info (dotfiles--find-org-node-with-url-property ,url)))
     (when (not org-info)
       (user-error "URL %s not found in Org!" ,url))
     (let* ((org-buffer (plist-get org-info :buffer))
            (org-headline (plist-get org-info :headline))
            (headline-pos (org-element-property :begin org-headline)))
       (with-current-buffer org-buffer
         (goto-char headline-pos)
         ,@body))))

(require 'org-clock)

(defun dotfiles--require-org-clock ()
  "Return user error if no `org' task is currently clocked in."
  (unless (org-clocking-p)
    (user-error "No org task is clocked-in")))

(defmacro dotfiles--with-different-org-clock (&rest body)
  "Save the current org clock, clock-in, execute the forms of BODY.

The marker must be at the new clock position."
  (declare (indent 1) (debug t))
  `(let ((current-clock-marker (when (org-clocking-p)
                                 (copy-marker org-clock-marker))))
     (unwind-protect
         (progn
           (org-clock-in)
           ,@body)
       (if current-clock-marker
           (org-with-point-at current-clock-marker
             (org-clock-in))
         (org-clock-out)))))

(defun dotfiles--read-org-headline ()
  "Get the target `org' headline for the capture."
  (let* ((refile-target (org-refile-get-location "File link to this under"))
         (file (nth 1 refile-target))
         (pos (nth 3 refile-target)))
    (switch-to-buffer (find-file-noselect file))
    (goto-char pos)
    (org-end-of-subtree)))

(defun dotfiles--clock-in-org-node-with-url (url)
  "Go to the `org' node with the given URL property value and clock it in."
  (dotfiles--with-org-node-with-url url
    (org-mark-ring-push)
    (goto-char headline-pos)
    (org-clock-in)
    (message "Clocking-in the `org' node with %s, use C-c & to go back" url)))

;;; `org-gcal' helpers

(require 'org-gcal)

(defun dotfiles--create-gcal-event (org-file calendar-id title time)
  "Create a Google Calendar event in the specified org file.
ORG-FILE is the path to the org file where the event will be added.
CALENDAR-ID is the ID of the Google Calendar.
TITLE is the title of the event.
TIME is the time of the event in `org' timestamp format."
  (dotfiles--in-org-buffer org-file
    (goto-char (point-max))
    (unless (bolp) (insert "\n")) ; Ensure at start of new line
    (insert "\n")
    (insert "* " title "\n")
    (insert ":PROPERTIES:\n")
    (insert ":calendar-id: " calendar-id "\n")
    (insert ":TRANSPARENCY: transparent\n")
    (insert ":END:\n")
    (insert ":org-gcal:\n")
    (insert time "\n")
    (insert "Added by dotfiles--create-gcal-event\n")
    (insert ":END:\n")
    (save-buffer)
    (org-gcal-post-at-point)))

(provide 'my-lib)
;;; my-lib.el ends here
