;;; my-lib.el --- helpers for other code.  -*- lexical-binding: t; -*-

;;; Commentary:

;; This contains internal helpers used elsewhere. Very much personalized.

;;; Code:

;;; Regexps for looking up information in e-mails

(defconst dotfiles--muspy-release-date-and-title
  (concat "\\([0-9]\\{4\\}\\(?:-[0-9]\\{2\\}\\)\\{0,2\\}\\): \\(.*? - .*?\\) "
          "(\\(?:Album\\|EP\\|Compilation\\|Single\\|Live\\))"))

(defconst dotfiles--gh-view-run-results "^View results: \\(.*\\)")

(defconst dotfiles--gh-pr-in-subject "^.*(PR #\\([0-9]+\\))$")
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

(defconst dotfiles--gdoc-open-comment-link "Open[[:space:]]*\n(\\(.*?\\))")
(defconst dotfiles--gdoc-open-shared-link "^\\(https://docs.google.com/.*\\)$")

;;; macOS Music library

(declare-function do-applescript "term/ns-win" (script))

(defun dotfiles--applescript-quote (s)
  "Return S wrapped as an AppleScript string literal.
Backslashes and double quotes in S are escaped so the result can be spliced into
AppleScript source.  S must contain no newlines -- a raw newline inside an
AppleScript string literal is a syntax error; signals an error if S does.
Normalize user-supplied strings with `dotfiles--music-normalize-key' first."
  (declare (ftype (function (string) string))
           (important-return-value t)
           (side-effect-free t))
  (when (string-match-p "[\r\n]" s)
    (error "dotfiles--applescript-quote: newline in argument %S" s))
  (concat "\"" (replace-regexp-in-string "[\\\"]" "\\\\\\&" s) "\""))

(defun dotfiles--music-normalize-key (s)
  "Return S downcased and trimmed, with any newline run collapsed to a space.
Produces a canonical comparison key stable across differing surrounding
whitespace or embedded newlines, so the same artist or album text yields the
same key regardless of such cosmetic differences."
  (declare (ftype (function (string) string))
           (important-return-value t)
           (side-effect-free t))
  (downcase (string-trim (replace-regexp-in-string "[\r\n]+" " " s))))

(defconst dotfiles--music-field-separator "::dotfiles-field::"
  "Separator joining a track's artist and album in the Music query output.
A multi-character sentinel that should not appear in artist or album metadata,
unlike a tab, so a tab in a title cannot shift field positions.")

(defconst dotfiles--music-record-separator "::dotfiles-record::"
  "Separator delimiting whole track records in the Music query output.
A sentinel like `dotfiles--music-field-separator', which joins the artist and
album within each record.")

(defun dotfiles--music-library-owned-album (artist album)
  "Return the Music library album by ARTIST matching ALBUM, or nil.
Both are matched ignoring case and surrounding whitespace but
otherwise exactly, so a differently decorated re-release (a remaster or
anniversary edition, say) does not match.
Returns nil off macOS (no `do-applescript'), when ARTIST or ALBUM is not a
non-empty string, when the library has no such album, or when the query fails
(Music unreachable or Automation permission not granted) -- in which case a
warning is emitted.
Tracks whose library artist or album metadata contains embedded newlines may be
silently skipped even when present: the AppleScript `contains' pre-filter
searches for the newline-collapsed key as a substring of the as-stored metadata,
and a space does not match a newline there.
Querying may launch the Music app if it is not running, and runs synchronously,
blocking Emacs until it completes.  The matched library album name is returned so
the caller can show what matched."
  (declare (ftype (function (string string) (or string null)))
           (important-return-value t))
  (and-let* (((fboundp 'do-applescript))
             ((stringp artist))
             ((stringp album))
             (artist-key (dotfiles--music-normalize-key artist))
             ((not (string= artist-key "")))
             (album-key (dotfiles--music-normalize-key album))
             ((not (string= album-key "")))
             ;; `contains' is a broad, case-insensitive substring pre-filter;
             ;; the Elisp `string=' pass below enforces the exact match.  Every
             ;; matching track is emitted -- not just the first -- because a
             ;; substring match can be a superset (a deluxe edition whose title
             ;; merely contains the queried album), and stopping at the first
             ;; would let such a superset hide a genuine exact match later in the
             ;; list.  Keys are normalized before splicing so a multi-line value
             ;; cannot break the AppleScript string literal.
             (raw (condition-case err
                      (progn
                        (message "Querying Music library...")
                        (do-applescript
                         (format "tell application \"Music\"
set out to \"\"
repeat with t in (every track whose artist contains %s and album contains %s)
set out to out & (artist of t) & %s & (album of t) & %s
end repeat
return out
end tell"
                                 (dotfiles--applescript-quote artist-key)
                                 (dotfiles--applescript-quote album-key)
                                 (dotfiles--applescript-quote
                                  dotfiles--music-field-separator)
                                 (dotfiles--applescript-quote
                                  dotfiles--music-record-separator))))
                    (error
                     (display-warning
                      'dotfiles
                      (format "Music library query failed: %s"
                              (error-message-string err))
                      :warning)
                     nil)))
             (records (split-string raw dotfiles--music-record-separator t)))
    (seq-some
     (lambda (record)
       (let ((fields (split-string record dotfiles--music-field-separator)))
         (if (/= (length fields) 2)
             (display-warning
              'dotfiles
              (format "Music library query: malformed record %S" record)
              :warning)
           (let ((album-raw (cadr fields)))
             (and (string= artist-key
                           (dotfiles--music-normalize-key (car fields)))
                  (string= album-key (dotfiles--music-normalize-key album-raw))
                  (string-trim album-raw))))))
     records)))

;;; string helpers

;; If dependencies are OK, then use `string-join' instead.
(defun dotfiles--concat-all (s)
  "Concatenates all strings in S with spaces."
  (declare (ftype (function (list) string))
           (important-return-value t)
           (side-effect-free t))
  (mapconcat 'identity s " "))

;;; regex helpers

(defun dotfiles--string-match-string (regex string)
  "Return the 1st match for REGEX in STRING, nil otherwise."
  (declare (ftype (function (string string) (or string null)))
           (important-return-value t)
           (side-effect-free t))
  (when (string-match regex string)
    (match-string 1 string)))

;;; Package helpers

(require 'package)

(defun dotfiles--ensure-optional-package (package)
  "Install PACKAGE if needed, without adding to selected packages."
  (unless (package-installed-p package)
    (message "Installing optional package: %s" package)
    (let ((package-selected-packages package-selected-packages))
      (condition-case err
          (package-install package)
        (error (message "Failed to install %s: %s" package err))))))

(defun dotfiles--ensure-optional-packages (packages)
  "Install PACKAGES without adding to selected packages."
  (dolist (package packages)
    (dotfiles--ensure-optional-package package)))

;;; File helpers

(defun dotfiles--set-exe-var (var name path)
  "Set VAR to PATH and warn if it is not an executable NAME."
  (set var path)
  (without-remote-files
    (unless (file-executable-p path)
      (display-warning
       'dotfiles
       (format "Executable %s not found at %s" name path) :warning))))

(defun dotfiles--find-latest-pdf (directory)
  "Find the most recently created .pdf file in DIRECTORY."
  (declare (ftype (function (string) string))
           (important-return-value t))
  (without-remote-files
    (let* ((files (directory-files-and-attributes directory nil "\\.pdf\\'" t))
           (sorted-files (sort files (lambda (a b)
                                       (time-less-p (nth 6 b) (nth 6 a))))))
      (if sorted-files
          (file-name-nondirectory (car (car sorted-files)))
        (user-error "No PDFs found in %s" directory)))))

(defun dotfiles--read-pdf (dir prompt confirmation)
  "Read a PDF file in DIR with PROMPT, then confirm it with CONFIRMATION.
While reading, suggest to complete with the latest PDF in the directory.
CONFIRMATION must have a %s argument which will be replaced with the file path.
Returns the path or nil."
  (declare (ftype (function (string string string) (or string null)))
           (important-return-value t))
  (without-remote-files
    (let* ((latest-pdf (dotfiles--find-latest-pdf dir))
           (pdf-fn (read-file-name prompt dir latest-pdf t latest-pdf))
           (pdf-path (expand-file-name pdf-fn dir)))
      (when (y-or-n-p (format confirmation pdf-path))
        pdf-path))))

(defun dotfiles--sibling-path (path fn)
  "For a given PATH, return a full path for FN in the same directory."
  (declare (ftype (function (string string) string))
           (important-return-value t)
           (side-effect-free t))
  (file-name-concat (file-name-directory path) fn))

;; Command-line program helpers

(defun dotfiles--run-program-process-output (program args success-fn)
  "Run PROGRAM with ARGS, executing SUCCESS-FN on zero exit.
ARGS must be a list of strings passed to PROGRAM.
SUCCESS-FN is executed on zero exit with a single string argument containing the
output of execution.
In the case of non-zero exit code it is printed as a user error together with
any output."
  (declare (ftype (function (string list function) t)))
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
  (declare (ftype (function (string list) t)))
  (dotfiles--run-program-process-output program args (lambda (_))))

;; Command-line "gh" utility helper

(defun dotfiles--gh-get (args)
  "Run gh with ARGS, return its output with the final newline trimmed.
ARGS must be properly quoted if needed."
  (declare (ftype (function (string) string))
           (important-return-value t))
  ;; We want to remove the final character and the final character only. Hence,
  ;; `substring' instead of i.e. `string-trim-right'.
  (substring (shell-command-to-string (concat "gh " args)) 0 -1))

;;; Buffer management helpers

(defun dotfiles--get-org-buffer (name)
  "Get the buffer for an `org' file with NAME."
  (declare (ftype (function (string) buffer))
           (important-return-value t))
  (or (find-buffer-visiting name)
      (find-file-noselect name)))

(defmacro dotfiles--in-org-buffer (name &rest body)
  "Execute the forms in BODY with NAME `org' buffer temporarily current."
  (declare (indent 1) (debug t))
  `(with-current-buffer (dotfiles--get-org-buffer ,name)
     ,@body))

;; Email automation and the helpers for action functions now live in the
;; `mu4e-autotask' package; some GitHub/`mu4e' helpers below still use
;; `mu4e-autotask-raw-message' and `mu4e-message-field'.
(require 'mu4e-message)
(require 'mu4e-autotask)

;; GitHub helpers

(defun dotfiles--get-gh-name-from-url (url)
  "Get the GitHub organization/project from a PR URL."
  (declare (ftype (function (string) (or string null)))
           (important-return-value t)
           (side-effect-free t))
  (dotfiles--string-match-string dotfiles--gh-org-and-project url))

;; GitHub / `mu4e' helpers

(defun dotfiles--get-closed-pr-url (pr-id html-content)
  "Return the URL of a closed GitHub PR with PR-ID in HTML-CONTENT or nil."
  (declare (ftype (function (string string) (or string null)))
           (important-return-value t)
           (side-effect-free t))
  (let ((closed-pr-url-regex (format dotfiles--gh-closed-pr-url-format pr-id)))
    (dotfiles--string-match-string closed-pr-url-regex html-content)))

(defun dotfiles--get-commented-pr-url (pr-id html-content)
  "Return the URL of a commented GitHub PR with PR-ID in HTML-CONTENT or nil."
  (declare (ftype (function (string string) (or string null)))
           (important-return-value t)
           (side-effect-free t))
  (let ((commented-pr-url-regex
         (format dotfiles--gh-commented-pr-url-format pr-id)))
    (dotfiles--string-match-string commented-pr-url-regex html-content)))

(defun dotfiles--parse-gh-release-subject (subject)
  "Parse out GitHub release email SUBJECT into a plist."
  (declare (ftype (function (string) list))
           (important-return-value t))
  (unless (string-match dotfiles--gh-release-in-subject subject)
    (user-error "Subject %s did not match against %s" subject
                dotfiles--gh-release-in-subject))
  (let ((gh-org (match-string 1 subject))
        (gh-project (match-string 2 subject))
        (rel-tag (match-string 3 subject))
        (rel-tag-2 (match-string 4 subject)))
    ;; Strip leading 'v' from first tag if present before comparison
    (let ((rel-tag-normalized (if (string-prefix-p "v" rel-tag)
                                  (substring rel-tag 1)
                                rel-tag)))
      (unless (string= rel-tag-normalized rel-tag-2)
        (user-error "Unrecognized GitHub Release email subject format: %s"
                    subject)))
    (list :gh-org gh-org :gh-project gh-project
          :gh-name (concat gh-org "/" gh-project) :rel-tag rel-tag)))

(defun dotfiles--get-run-results-url (msg)
  "Get a GitHub run URL from a `mu4e' MSG."
  (declare (ftype (function (list) (or string null)))
           (important-return-value t))
  (let ((raw-message (mu4e-autotask-raw-message msg)))
    (dotfiles--string-match-string dotfiles--gh-view-run-results raw-message)))

(defun dotfiles--get-gh-issue-url (msg)
  "Get a GitHub issue URL from a `mu4e' MSG."
  (declare (ftype (function (list) (or string null)))
           (important-return-value t))
  (let ((raw-message (mu4e-autotask-raw-message msg)))
    (dotfiles--string-match-string dotfiles--gh-issue-url raw-message)))

(defun dotfiles--get-pr-id (msg)
  "Return the PR id from a `mu4e' MSG subject."
  (declare (ftype (function (list) (or string null)))
           (important-return-value t))
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
  (declare (ftype (function (string) my-dev-project))
           (important-return-value t))
  (or (cl-find name my-projects :test #'string= :key #'my-dev-project-name)
      (user-error "Project %s not configured in `my-projects'" name)))

(defun dotfiles--find-project-by-gh (gh-name)
  "Find a development project by its GitHub name GH-NAME."
  (declare (ftype (function (string) my-dev-project))
           (important-return-value t))
  (or (cl-find gh-name my-projects :test #'string= :key
               #'my-dev-project-gh-name)
      (user-error "GitHub project %s not configured in `my-projects'" gh-name)))

(defun dotfiles--find-project-for-cwd ()
  "Find a development project for the current working directory."
  (declare (ftype (function () my-dev-project))
           (important-return-value t))
  (let ((gh-name (dotfiles--gh-get
                  "repo view --json nameWithOwner -q '.nameWithOwner'")))
    (unless gh-name
      (user-error "Could not find a GitHub project in %s" default-directory))
    (dotfiles--find-project-by-gh gh-name)))

(defun dotfiles--get-project-push-remote (project)
  "Get the push remote for PROJECT."
  (declare (ftype (function (my-dev-project) string))
           (important-return-value t))
  (or (my-dev-project-push-remote project)
      (user-error "Project %s misconfigured in `my-projects'"
                  (my-dev-project-name project))))

(defun dotfiles--get-project-branch-root (project)
  "Get the branch root directory for PROJECT."
  (declare (ftype (function (my-dev-project) string))
           (important-return-value t))
  (or (my-dev-project-branch-root project)
      (user-error "Project %s misconfigured in `my-projects'")))

(defun dotfiles--get-project-main-branch-dir (project)
  "Get the directory of the main branch checkout for PROJECT."
  (declare (ftype (function (my-dev-project) string))
           (important-return-value t))
  (let ((main-branch-checkout (my-dev-project-main-branch-checkout project)))
    (unless main-branch-checkout
      (user-error "Project %s misconfigured in `my-projects'"
                  (my-dev-project-name project)))
    (concat (dotfiles--get-project-branch-root project) main-branch-checkout)))

(defun dotfiles--format-waitingfor-task-title (project branch-name)
  "Format the `org' task title for a PR of BRANCH-NAME in PROJECT."
  (declare (ftype (function (my-dev-project string) string))
           (important-return-value t)
           (side-effect-free t))
  (let ((format-string (my-dev-project-pr-waitingfor-template project)))
    (unless format-string
      (user-error "Project %s misconfigured in `my-projects'"
                  (my-dev-project-name project)))
    (format format-string (concat "=" branch-name "="))))

(defun dotfiles--visit-post-pr-url (project)
  "Visit the URL for a PROJECT after a PR."
  (declare (ftype (function (my-dev-project) t)))
  (when-let ((post-pr-url (my-dev-project-post-pr-url project)))
    (browse-url post-pr-url)))

(defun dotfiles--create-pr (project branch-name)
  "Create a new PR from the current branch with provided data.
Pushes the branch to my remote first. The needed data are PROJECT and
BRANCH-NAME. Returns the URL of this PR."
  (declare (ftype (function (my-dev-project string) string))
           (important-return-value t))
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
  (declare (ftype (function (string) (or my-3rd-party-submodule null)))
           (important-return-value t))
  (or (cl-find gh-name my-3rd-party-submodules :test #'string= :key
               #'my-3rd-party-submodule-3p-gh-name)
      (message "Nothing found in `my-3rd-party-submodules' for %s" gh-name)))

;;; Lithuanian date parsing

(require 'calendar)
(require 'time-date)

(defconst dotfiles--lithuanian-month-genitives
  '(("sausio" . 1) ("vasario" . 2) ("kovo" . 3) ("balandžio" . 4)
    ("gegužės" . 5) ("birželio" . 6) ("liepos" . 7) ("rugpjūčio" . 8)
    ("rugsėjo" . 9) ("spalio" . 10) ("lapkričio" . 11) ("gruodžio" . 12))
  "Genitive Lithuanian month names mapped to month numbers.")

(defun dotfiles--lithuanian-genitive-date-to-iso (month-name day reference-time)
  "Return the ISO date for genitive Lithuanian MONTH-NAME and DAY number.
The source text carries no year; infer it from REFERENCE-TIME by choosing, among
the reference year and its two neighbours, the one that places the date closest
to REFERENCE-TIME.  This suits a delivery date, which may fall shortly before or
after the message that carries it.  Signal a `user-error' on an unknown
MONTH-NAME, or on a DAY out of range for the month in every candidate year."
  (declare (ftype (function (string integer t) string))
           (important-return-value t))
  (let ((month (or (cdr (assoc month-name
                               dotfiles--lithuanian-month-genitives))
                   (user-error "Unknown Lithuanian month name: %s" month-name)))
        (ref-year (string-to-number (format-time-string "%Y" reference-time)))
        (ref-days (time-to-days reference-time))
        (best nil)
        (best-distance nil))
    (dolist (year (list (1- ref-year) ref-year (1+ ref-year)))
      (when (<= 1 day (date-days-in-month year month))
        (let ((distance (abs (- (calendar-absolute-from-gregorian
                                 (list month day year))
                                ref-days))))
          (when (or (null best-distance) (< distance best-distance))
            (setq best (format "%04d-%02d-%02d" year month day)
                  best-distance distance)))))
    (or best
        (user-error "Invalid day %d for Lithuanian month %s"
                    day month-name))))

;;; `org' helpers

(require 'org-refile)
(require 'org-autotask)

(defun dotfiles--read-org-headline ()
  "Get the target `org' headline for the capture."
  (declare (ftype (function () t)))
  (let* ((refile-target (org-refile-get-location "File link to this under"))
         (file (nth 1 refile-target))
         (pos (nth 3 refile-target)))
    (switch-to-buffer (find-file-noselect file))
    (goto-char pos)
    (org-end-of-subtree)))

(defun dotfiles--org-append-mu4e-link (link msgid)
  "Append mu4e LINK at the end of the `org' subtree at point.
Do nothing when the subtree already contains a link to MSGID.  Move point to the
subtree's heading first, so the dedup scan covers the whole subtree wherever
point started.  Return non-nil when LINK was inserted, nil otherwise."
  (declare (ftype (function (string string) boolean)))
  (org-back-to-heading)
  (let ((end (save-excursion (org-end-of-subtree t) (point))))
    ;; Anchor on the link's closing `]' and match case-sensitively: a Message-ID
    ;; is case-sensitive and may be a prefix of another already linked here.
    (unless (let ((case-fold-search nil))
              (save-excursion
                (search-forward (concat "mu4e:msgid:" msgid "]") end t)))
      (goto-char end)
      (unless (bolp) (insert "\n"))
      (insert link "\n")
      t)))

(defun dotfiles--org-task-top-level-p ()
  "Return non-nil when the `org' task at point has no parent task.
True when the immediate parent heading carries no TODO keyword (the task sits
directly under a plain container heading such as \"Tasks\"); nil when it is a
sub-action under a project task."
  (declare (ftype (function () boolean))
           (important-return-value t))
  (save-excursion
    (not (and (org-up-heading-safe) (org-get-todo-state)))))

;;; Online-store order tracking

(defun dotfiles--store-order-task-title (store order-date order-id)
  "Return the order-task title for STORE, ORDER-DATE and ORDER-ID.
With ORDER-DATE non-nil, the full \"Iš STORE DATE ID užsakymo\" the order
confirmation creates; with ORDER-DATE nil, the ID-only form a shipping notice
creates and the confirmation later completes.  ORDER-DATE lets a date-only
payment email find the task; ORDER-ID lets a shipping notice find it."
  (declare (ftype (function (string (or null string) string) string))
           (important-return-value t)
           (side-effect-free t))
  (if order-date
      (format "Iš %s %s %s užsakymo" store order-date order-id)
    (format "Iš %s %s užsakymo" store order-id)))

(defun dotfiles--store-find-order-task (store key)
  "Return the position of the STORE @waitingfor order task containing KEY.
KEY is an order ID or an order date.  Search the current buffer for the first
@waitingfor \"Iš STORE ... užsakymo\" heading whose title carries KEY as a whole
space-delimited token, and return its position, or nil.  Anchoring to that title
shape, matching case-sensitively, and requiring a whole token keep a bare order
date from latching onto an unrelated dated task, and one order ID from matching
another that merely contains it as a substring."
  (declare (ftype (function (string string) (or null integer)))
           (important-return-value t))
  (let ((heading-rx (concat "\\`Iš " (regexp-quote store) " .*užsakymo\\'")))
    (catch 'found
      (org-map-entries
       (lambda ()
         (let ((heading (org-get-heading t t t t)))
           (when (and (let ((case-fold-search nil))
                        (string-match-p heading-rx heading))
                      (member key (split-string heading)))
             (throw 'found (point)))))
       (concat "+" (org-autotask-list-tag org-autotask-waitingfor)) 'file)
      nil)))

(defmacro dotfiles--with-store-order-task (org-file store msg key &rest body)
  "Locate the @waitingfor STORE order task for KEY in ORG-FILE and run BODY.
Capture MSG's mu4e link and message-id -- as `link' and `msgid' -- while the
email buffer is current, then switch to ORG-FILE (widened and left current) and
bind `task' to the position of the STORE order task carrying KEY, or nil.  BODY
runs with `task', `link', and `msgid' bound and decides how to handle a found or
missing task."
  (declare (indent 4) (debug (form form form form body)))
  `(let ((msgid (mu4e-message-field ,msg :message-id))
         (link (org-store-link nil)))
     (dotfiles--in-org-buffer ,org-file
       (org-with-wide-buffer
        (let ((task (dotfiles--store-find-order-task ,store ,key)))
          ,@body)))))

(defun dotfiles--store-file-order-email (org-file store msg order-id order-date
                                                  delivery-date)
  "File an `org' link to STORE order MSG into its @waitingfor task in ORG-FILE.
Create the task under \"Tasks\" when absent.  Locate the task by
ORDER-ID.  ORDER-DATE (the order day, or nil when MSG does not carry it)
completes an ID-only title; the order confirmation is authoritative for it and
corrects a stale one.  Reschedule the task to DELIVERY-DATE when non-nil.
Idempotent: append the link only when the task's subtree does not already hold
MSG's message-id."
  (declare (ftype (function (string string list string (or null string)
                                    (or null string))
                            t)))
  (dotfiles--with-store-order-task org-file store msg order-id
    (if task
        (progn
          (goto-char task)
          ;; The confirmation (ORDER-DATE non-nil) owns the order date: add it
          ;; to an ID-only title, or correct a stale one.
          (when order-date
            (let ((full (dotfiles--store-order-task-title
                         store order-date order-id)))
              (unless (string= (org-get-heading t t t t) full)
                (org-edit-headline full)))))
      (goto-char (or (org-find-exact-headline-in-buffer "Tasks")
                     (user-error "No \"Tasks\" heading in %s" org-file)))
      (org-insert-subheading '(4))
      (org-autotask-insert-waiting-for-next-action
       (dotfiles--store-order-task-title store order-date order-id))
      (setq task (dotfiles--store-find-order-task store order-id)))
    (goto-char task)
    (when delivery-date
      (org-schedule nil delivery-date))
    ;; Append at the end of the subtree, past any SCHEDULED line, so an active
    ;; clock cannot misplace the link into the clocked-in entry.
    (unless (dotfiles--org-append-mu4e-link link msgid)
      (message "%s link already filed: %s" store order-id))
    (save-buffer)))

(defun dotfiles--mu4e-complete-order-task (org-file store msg key)
  "Complete the STORE @waitingfor order task in ORG-FILE for a delivered MSG.
Find the task by KEY (an order ID) via `dotfiles--store-find-order-task', append
an `org' link to MSG, then prompt to mark it DONE; on yes, complete it and, when
it is a top-level task (not a project sub-action), archive its subtree.  Signal a
`user-error' when no such task exists."
  (declare (ftype (function (string string list string) t)))
  (dotfiles--with-store-order-task org-file store msg key
    (if (not task)
        (user-error "No %s order task found for %s" store key)
      (goto-char task)
      (unless (dotfiles--org-append-mu4e-link link msgid)
        (message "%s order link already filed: %s" store key))
      (goto-char task)
      (when (y-or-n-p (format "Mark %s order %s as completed? " store key))
        (org-autotask-complete-item)
        (when (dotfiles--org-task-top-level-p)
          (org-archive-subtree)))
      (save-buffer))))

;;; `org-gcal' helpers

;; Do not require `org-gcal' to avoid the warning about needing to call
;; `org-gcal-reload-client-id-secret'
(declare-function org-gcal-post-at-point "org-gcal" ())

(defun dotfiles--org-timestamp (date &optional start-time end-time)
  "Return an active `org' timestamp for DATE, an ISO YYYY-MM-DD string.
With START-TIME and END-TIME, HH:MM strings passed together, append the time
range."
  (declare (ftype (function (string &optional string string) string))
           (important-return-value t))
  (cl-assert (eq (null start-time) (null end-time)))
  (let ((weekday (format-time-string "%a" (org-time-string-to-time date))))
    (if start-time
        (format "<%s %s %s-%s>" date weekday start-time end-time)
      (format "<%s %s>" date weekday))))

(defun dotfiles--create-gcal-event (org-file calendar-id title time)
  "Create a Google Calendar event in the specified org file.
ORG-FILE is the path to the org file where the event will be added.
CALENDAR-ID is the ID of the Google Calendar.
TITLE is the title of the event.
TIME is the time of the event in `org' timestamp format."
  (declare (ftype (function (string string string string) t)))
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

(defun dotfiles--prompt-create-gcal-event (org-file calendar-id title time)
  "Prompt to create a Google Calendar event, creating it on confirmation.
ORG-FILE, CALENDAR-ID, TITLE, and TIME are as in `dotfiles--create-gcal-event'."
  (declare (ftype (function (string string string string) t)))
  (when (y-or-n-p (format "Add \"%s\" at %s to the Google Calendar? "
                          title time))
    (dotfiles--create-gcal-event org-file calendar-id title time)))

(provide 'my-lib)
;;; my-lib.el ends here
