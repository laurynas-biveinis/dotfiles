;;; my-lib.el --- helpers for other code.  -*- lexical-binding: t; -*-

;;; Commentary:

;; This contains internal helpers used elsewhere. Very much personalized.

;;; Code:

(require 'subr-x)

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
;; Restore arity checks for features stubbed during batch compilation.
(declare-function mu4e-autotask-raw-message "mu4e-autotask" (msg))
(declare-function mu4e-message-field "ext:mu4e-message" (msg field))

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
    ;; The release title and the tag must refer to the same version, but either
    ;; may carry a leading 'v'.
    (unless (string= (string-remove-prefix "v" rel-tag)
                     (string-remove-prefix "v" rel-tag-2))
      (user-error "Unrecognized GitHub Release email subject format: %s"
                  subject))
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
      (user-error "Project %s has no :push-remote in `my-projects'"
                  (my-dev-project-name project))))

(defun dotfiles--get-project-branch-root (project)
  "Get the branch root directory for PROJECT."
  (declare (ftype (function (my-dev-project) string))
           (important-return-value t))
  (or (my-dev-project-branch-root project)
      (user-error "Project %s has no :branch-root in `my-projects'"
                  (my-dev-project-name project))))

(defun dotfiles--get-project-main-branch-dir (project)
  "Get the directory of the main branch checkout for PROJECT."
  (declare (ftype (function (my-dev-project) string))
           (important-return-value t))
  (let ((main-branch-checkout (my-dev-project-main-branch-checkout project)))
    (unless main-branch-checkout
      (user-error "Project %s has no :main-branch-checkout in `my-projects'"
                  (my-dev-project-name project)))
    (concat (dotfiles--get-project-branch-root project) main-branch-checkout)))

(defun dotfiles--format-waitingfor-task-title (project branch-name)
  "Format the `org' task title for a PR of BRANCH-NAME in PROJECT."
  (declare (ftype (function (my-dev-project string) string))
           (important-return-value t)
           (side-effect-free t))
  (let ((format-string (my-dev-project-pr-waitingfor-template project)))
    (unless format-string
      (user-error "Project %s has no :pr-waitingfor-template in `my-projects'"
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

(require 'org-archive)
(require 'org-refile)
(require 'org-autotask)
;; Keep arity checks independent of package bytecode availability.
;; The generated accessor can only be file-checked by `check-declare'.
(declare-function org-autotask-list-tag "org-autotask" (gtd-list) t)
(declare-function org-autotask-insert-waiting-for-next-action "org-autotask"
                  (title))
(declare-function org-autotask-complete-item "org-autotask" ())

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
  "Append mu4e LINK at the end of the subtree of the `org' entry at point.
Do nothing when that entry's subtree already contains a link to MSGID.  Move
point back to the entry's own heading first, so the dedup scan covers its whole
subtree wherever in it point started.  Point must be in the target entry itself:
`org-back-to-heading' stops at the innermost enclosing entry, so from a
sub-entry both the scan and the insertion would be confined to that sub-entry.
Return non-nil when LINK was inserted, nil otherwise.  The link goes above any
trailing blank line, so the separator before the next entry survives.

Accept a folded heading: callers reach the entry through a marker from
`dotfiles--store-find-order-task' and do not control the buffer's fold state.
On a folded heading, taking the nearest visible ancestor instead would append
the link under \"Tasks\" rather than under the order task."
  (declare (ftype (function (string string) boolean)))
  (org-back-to-heading t)
  (let ((end (save-excursion (org-end-of-subtree t) (point))))
    ;; Anchor on the link's closing `]' and match case-sensitively: a Message-ID
    ;; is case-sensitive and may be a prefix of another already linked here.
    (unless (let ((case-fold-search nil))
              (save-excursion
                (search-forward (concat "mu4e:msgid:" msgid "]") end t)))
      (goto-char end)
      (insert "\n" link)
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

;; Three hazards shape the code below.  Each is a property of the subsystem
;; rather than of any one function, so each is stated here once and referenced
;; from the code it justifies.
;;
;; Save vs signal.  `dotfiles--with-store-order-task' saves ORG-FILE after BODY
;; returns normally.  A BODY that signals skips that save but not its edits:
;; they stay in the buffer, which `dotfiles--get-org-buffer' reuses, and the
;; next run's save persists them.  So whatever is knowable up front is rejected
;; before BODY runs -- KEY's shape is, in `dotfiles--store-find-order-task' --
;; while a condition that only the edit itself or a command-loop yield can
;; settle is checked afterwards, deliberately leaving the edit that preceded it
;; for a later save.  Such an edit must be one a re-run reproduces or dedups.
;; That rule covers edits in ORG-FILE's own buffer, and two checks escape it.
;; The re-find `error' on `dotfiles--store-file-order-email's creation path
;; asserts a state the code cannot produce: the invariant a safe re-run would
;; rely on is already broken when it fires, so the task it strands is one a
;; re-run duplicates rather than reproduces.  The archive guard in
;; `dotfiles--mu4e-complete-order-task' escapes it differently: a failed
;; archive can leave a destination copy that no re-run dedups.  That call
;; suppresses the archive's own save and wraps the destination edit in a change
;; group so failure rolls back the copy and Org's relocated markers.
;;
;; Point.  Point is not preserved across the steps BODY performs, for two
;; distinct reasons, so re-anchor with `goto-char' on `task' immediately before
;; every point-dependent operation.  First, the `org' helpers BODY calls restore
;; point to wherever they found it, which is not the task.  Second, buffer point
;; is taken from a window's `window-point' when a window showing the buffer is
;; selected (see the Emacs Lisp manual, "Windows and Point", and
;; `select-window'); BODY runs with the `org' buffer current but not selected,
;; so any step that reads input or otherwise yields to the command loop gives
;; the user a chance to select a window displaying ORG-FILE -- and, the
;; minibuffer not being modal, to edit it there rather than merely move point.
;; Re-anchoring handles the point half; the insertion half is why
;; `dotfiles--store-find-order-task' returns a marker.  Redisplay alone does not
;; move point, and neither does a bare `message'.
;;
;; Region.  Anchoring point is not by itself enough: `org-schedule', `org-todo',
;; `org-archive-subtree' and `org-do-demote' all switch from "the entry at
;; point" to "every heading in the region" when a region is active, and
;; `mark-active' is buffer-local, so one the user left in ORG-FILE survives
;; while they read mail.  Suppress each command's region dispatch at the call,
;; never across BODY as a whole: `org-do-demote' needs `org-ignore-region',
;; while the other commands honor the narrower
;; `org-loop-over-headlines-in-active-region'.  In particular, do not hold the
;; global `org-ignore-region' across hooks, prompts or other command-loop yields;
;; that would disable the region for the user's own Org commands in every
;; buffer.  The final save runs after every region-policy binding has closed.

(defun dotfiles--store-check-order-token (store what token)
  "Signal a `user-error' unless TOKEN, STORE's order WHAT, is a single token.
Rejects embedded and surrounding whitespace and the empty string alike.  One
tokenization for every input spliced into an order-task heading, so the rule
and its check cannot drift."
  (declare (ftype (function (string string string) t)))
  (unless (equal (split-string token) (list token))
    (user-error "%s order %s must be a single whitespace-free token: %S"
                store what token)))

(defun dotfiles--store-order-task-title (store order-date order-id)
  "Return the order-task title for STORE, ORDER-DATE and ORDER-ID.
With ORDER-DATE non-nil, the full \"Iš STORE DATE ID užsakymo\" the order
confirmation creates; with ORDER-DATE nil, the ID-only form a shipping notice
creates and the confirmation later completes.  ORDER-DATE lets a date-only
payment email find the task; ORDER-ID lets a shipping notice find it.

ORDER-DATE and ORDER-ID are spliced in raw and must each be a single
whitespace-free token: `dotfiles--store-check-order-token' rejects both before
anything is edited.  Unchecked, a newline would split the generated heading, and
other whitespace would re-tokenize ORDER-DATE and break the date-keyed lookup."
  (declare (ftype (function (string (or null string) string) string))
           (important-return-value t)
           (side-effect-free t))
  (if order-date
      (format "Iš %s %s %s užsakymo" store order-date order-id)
    (format "Iš %s %s užsakymo" store order-id)))

(defun dotfiles--store-find-order-task (store key)
  "Return a marker at the STORE @waitingfor order task containing KEY.
KEY is an order ID or an order date.  Search the current buffer for the first
@waitingfor \"Iš STORE ... užsakymo\" heading whose title carries KEY as a whole
space-delimited token, and return a marker at it, or nil.  Anchoring to that
title shape, matching case-sensitively, and requiring a whole token keep a bare
order date from latching onto an unrelated dated task, and one order ID from
matching another that merely contains it as a substring.

Signal a `user-error' when KEY is not itself a single whitespace-free token.
No heading token can match such a KEY, so a caller that created a task for it
could never re-find it; rejecting it here, before BODY of
`dotfiles--with-store-order-task' runs, keeps that caller from editing first.

Return a marker of insertion type t rather than a position: the buffer can be
edited while BODY of `dotfiles--with-store-order-task' runs, and that type keeps
the marker on the task even when text is inserted at its heading's own
beginning of line.  The caller owns the marker and releases it with
`set-marker' when done."
  (declare (ftype (function (string string) (or null marker)))
           (important-return-value t))
  (dotfiles--store-check-order-token store "key" key)
  (let ((heading-rx (concat "\\`Iš " (regexp-quote store) " .*užsakymo\\'")))
    (catch 'found
      (org-map-entries
       (lambda ()
         (let ((heading (org-get-heading t t t t)))
           (when (and (let ((case-fold-search nil))
                        (string-match-p heading-rx heading))
                      (member key (split-string heading)))
             (throw 'found (copy-marker (point) t)))))
       (concat "+" (org-autotask-list-tag org-autotask-waitingfor)) 'file)
      nil)))

(defmacro dotfiles--with-store-order-task (org-file store msg key &rest body)
  "Run BODY on ORG-FILE's @waitingfor STORE order task for KEY, then save it.
Capture MSG's mu4e link and message-id -- as `link' and `msgid' -- while the
email buffer is current, then switch to ORG-FILE (widened and left current) and
bind `task' to a marker at the STORE order task carrying KEY, or nil.  BODY runs
with `task', `link', and `msgid' bound and decides how to handle a found or
missing task; ORG-FILE is saved after BODY returns normally, and a BODY that
signals skips that save but not its edits.  BODY must `goto-char' on `task'
immediately before every point-dependent operation.  The commentary at the head
of this section states the save-vs-signal, point and region rules BODY works
within."
  (declare (indent 4) (debug (form form form form body)))
  `(let ((msgid (mu4e-message-field ,msg :message-id))
         (link (org-store-link nil)))
     (dotfiles--in-org-buffer ,org-file
       (org-with-wide-buffer
        (let ((task (dotfiles--store-find-order-task ,store ,key)))
          (unwind-protect
              (progn ,@body)
            ;; `task' is nil until BODY's re-find on the creation path, so
            ;; read it at exit and guard: `set-marker' on nil would signal
            ;; from a cleanup form and replace BODY's own error.
            (when task (set-marker task nil)))))
       (save-buffer))))

(defun dotfiles--store-file-order-email (org-file store msg order-id order-date
                                                  delivery-date)
  "File an `org' link to STORE order MSG into its @waitingfor task in ORG-FILE.
Create the task under \"Tasks\" when absent.  Locate the task by ORDER-ID,
which `dotfiles--store-find-order-task' requires to be a single whitespace-free
token and otherwise rejects with a `user-error' before anything is edited.
ORDER-DATE (the order day, or nil when MSG does not carry it) completes an
ID-only title; the order confirmation is authoritative for it and corrects a
stale one, and is rejected the same way when it is not a single whitespace-free
token.  Reschedule the task to DELIVERY-DATE when non-nil.  Idempotent: append
the link only when the task's subtree does not already hold MSG's message-id."
  (declare (ftype (function (string string list string (or null string)
                                    (or null string))
                            t)))
  (when order-date
    (dotfiles--store-check-order-token store "date" order-date))
  (dotfiles--with-store-order-task org-file store msg order-id
    (if task
        ;; The confirmation (ORDER-DATE non-nil) owns the order date: add it
        ;; to an ID-only title, or correct a stale one.
        (when order-date
          (goto-char task)
          (let ((full (dotfiles--store-order-task-title
                       store order-date order-id)))
            (unless (string= (org-get-heading t t t t) full)
              (org-edit-headline full))))
      (goto-char (or (org-find-exact-headline-in-buffer "Tasks")
                     (user-error "No \"Tasks\" heading in %s" org-file)))
      ;; `org-insert-subheading' does this for a `'(4)' arg, but forwards no
      ;; `invisible-ok' to `org-insert-heading'.  A fold does not hide its own
      ;; headline, so the case to guard is a "Tasks" that is itself invisible,
      ;; nested under a folded ancestor: without `invisible-ok' the insertion
      ;; re-anchors on that ancestor and files the task under an unrelated
      ;; sibling.  In that same case `invisible-ok' suppresses the reveal
      ;; `org-insert-heading' would otherwise do, so the new task, inserted
      ;; inside the ancestor's fold, ends up as folded as the siblings around
      ;; it.  That is deliberate, and is what `org-refile' does: it anchors
      ;; with the same `invisible-ok' and reveals a pasted entry only when the
      ;; insertion point was visible beforehand.
      (org-insert-heading '(4) t)
      (let ((org-ignore-region t))
        (org-do-demote))
      (let ((org-loop-over-headlines-in-active-region nil))
        (org-autotask-insert-waiting-for-next-action
         (dotfiles--store-order-task-title store order-date order-id)))
      (unless (setq task (dotfiles--store-find-order-task store order-id))
        (error "Created %s order task for %s but could not find it again"
               store order-id)))
    (when delivery-date
      (goto-char task)
      (let ((org-loop-over-headlines-in-active-region nil))
        (org-schedule nil delivery-date)))
    ;; Re-anchor before the append: point can have drifted since the reschedule,
    ;; and when the task was found but the title edit did not run, nothing above
    ;; has put point on it at all.
    (goto-char task)
    ;; Append at the end of the subtree, past any SCHEDULED line, so an active
    ;; clock cannot misplace the link into the clocked-in entry.
    (unless (dotfiles--org-append-mu4e-link link msgid)
      (message "%s link already filed: %s" store order-id))))

(defun dotfiles--store-order-subtree (task)
  "Return TASK's complete subtree text without properties."
  (declare (ftype (function (marker) string))
           (important-return-value t)
           (side-effect-free t))
  (save-excursion
    (org-with-wide-buffer
     (goto-char task)
     (org-back-to-heading t)
     (buffer-substring-no-properties
      (point) (progn (org-end-of-subtree t t) (point))))))

(defvar dotfiles--org-archive-invocation-token nil
  "Identity of the dynamically active `org-archive-subtree' invocation.")

(defvar dotfiles--org-archive-requested-token nil
  "Token requested by the direct order-task archive call.")

(defvar dotfiles--org-archive-protected-token nil
  "Invocation whose transaction-only archive policies are active.")

(defvar dotfiles--org-archive-ambient-loop-policy nil
  "Caller policy hidden by the protected archive invocation.")

(defvar dotfiles--org-archive-ambient-save-policy nil
  "Caller save policy hidden by the protected archive invocation.")

(defvar dotfiles--org-archive-ambient-ignore-region nil
  "Caller region policy hidden by the protected archive invocation.")

(defun dotfiles--org-archive-with-invocation-token (original &rest args)
  "Call ORIGINAL with ARGS under a unique archive-invocation identity."
  (let* ((requested-token dotfiles--org-archive-requested-token)
         (restore-ambient-policy
          (and dotfiles--org-archive-protected-token
               (eq dotfiles--org-archive-invocation-token
                   dotfiles--org-archive-protected-token)
               (not requested-token)))
         (dotfiles--org-archive-invocation-token
          (or requested-token (make-symbol "org-archive-invocation")))
        ;; A recursive archive belongs to a distinct invocation.
         (dotfiles--org-archive-requested-token nil)
         (org-loop-over-headlines-in-active-region
          (if restore-ambient-policy
              dotfiles--org-archive-ambient-loop-policy
            org-loop-over-headlines-in-active-region))
         (org-ignore-region
          (if restore-ambient-policy
              dotfiles--org-archive-ambient-ignore-region
            org-ignore-region))
         (org-archive-subtree-save-file-p
          (if restore-ambient-policy
              dotfiles--org-archive-ambient-save-policy
            org-archive-subtree-save-file-p)))
    (apply original args)))

(advice-add 'org-archive-subtree :around
            #'dotfiles--org-archive-with-invocation-token
            '((depth . -100)))

(defun dotfiles--store-org-marker-snapshot (task)
  "Return Org markers in TASK with their source buffers and positions."
  (declare (ftype (function (marker) list))
           (important-return-value t))
  (save-excursion
    (org-with-wide-buffer
     (goto-char task)
     (org-back-to-heading t)
     (let ((start (point))
           (end (progn (org-end-of-subtree t t) (point)))
           org-markers-to-move)
       (org-save-markers-in-region start end)
       (mapcar (lambda (entry)
                 (let ((marker (car entry)))
                   (vector marker (marker-buffer marker)
                           (marker-position marker)
                           (- (marker-position marker) start)
                           nil nil nil nil)))
               org-markers-to-move)))))

(defun dotfiles--store-capture-owned-org-markers (snapshot)
  "Record the transaction-owned state of every marker in SNAPSHOT."
  (dolist (entry snapshot)
    (let ((marker (aref entry 0)))
      (aset entry 4 (marker-buffer marker))
      (aset entry 5
            (and (marker-buffer marker)
                 (copy-marker marker (marker-insertion-type marker))))
      (aset entry 6 (not (marker-buffer marker)))
      (aset entry 7 t))))

(defun dotfiles--store-restore-org-markers
    (snapshot source-buffer restored-start)
  "Restore transaction-owned markers in SNAPSHOT.
Use RESTORED-START as the current source-task boundary in SOURCE-BUFFER."
  (dolist (entry snapshot)
    (let* ((marker (aref entry 0))
           (owned-position (aref entry 5))
           (owned-p
            (or (not (aref entry 7))
                (if (aref entry 6)
                    (not (marker-buffer marker))
                  (and (eq (marker-buffer marker) (aref entry 4))
                       (= (marker-position marker)
                          (marker-position owned-position)))))))
      (when owned-p
        (move-marker
         marker
         (if (and restored-start
                  (eq (aref entry 1) source-buffer))
             (+ restored-start (aref entry 3))
           (aref entry 2))
         (aref entry 1))))))

(defun dotfiles--store-release-org-marker-snapshot (snapshot)
  "Release transaction-owned boundary markers in SNAPSHOT."
  (dolist (entry snapshot)
    (when (markerp (aref entry 5))
      (set-marker (aref entry 5) nil))))

(defun dotfiles--store-buffer-text ()
  "Return the current buffer's complete property-free text."
  (declare (ftype (function () string))
           (important-return-value t)
           (side-effect-free t))
  (save-restriction
    (widen)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun dotfiles--store-read-file (file literally)
  "Return FILE's contents, read LITERALLY when non-nil."
  (declare (ftype (function (string boolean) (or null string)))
           (important-return-value t)
           (side-effect-free t))
  (when (file-exists-p file)
    (with-temp-buffer
      (if literally
          (progn
            (set-buffer-multibyte nil)
            (insert-file-contents-literally file))
        (insert-file-contents file))
      (buffer-string))))

(defun dotfiles--store-file-bytes (file)
  "Return FILE's bytes, or nil when FILE does not exist."
  (declare (ftype (function (string) (or null string)))
           (important-return-value t)
           (side-effect-free t))
  (dotfiles--store-read-file file t))

(defun dotfiles--store-file-text (file)
  "Return decoded text from FILE, or nil when FILE does not exist."
  (declare (ftype (function (string) (or null string)))
           (important-return-value t)
           (side-effect-free t))
  (dotfiles--store-read-file file nil))

(defun dotfiles--store-same-file-p (first second)
  "Return non-nil when FIRST and SECOND name the same file."
  (declare (ftype (function (string string) boolean))
           (important-return-value t)
           (side-effect-free t))
  (or (and (file-exists-p first)
           (file-exists-p second)
           (file-equal-p first second))
      (equal (file-truename first) (file-truename second))))

(defun dotfiles--store-check-archive-file (archive-file)
  "Reject an unsupported dangling symlink at ARCHIVE-FILE."
  (when (and (file-symlink-p archive-file)
             (not (file-exists-p archive-file)))
    (user-error "Dangling archive symlink is unsupported: %s" archive-file)))

(defun dotfiles--store-write-file-bytes (file bytes)
  "Replace FILE with unibyte string BYTES without visiting it."
  (declare (ftype (function (string string) t)))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert bytes)
    (let ((coding-system-for-write 'no-conversion))
      (write-region (point-min) (point-max) file nil 'silent))))

(defun dotfiles--store-write-file-text (file contents coding-system)
  "Replace FILE with CONTENTS encoded using CODING-SYSTEM."
  (declare (ftype (function (string string symbol) t)))
  (with-temp-buffer
    (insert contents)
    (let ((coding-system-for-write coding-system))
      (write-region (point-min) (point-max) file nil 'silent))))

(defun dotfiles--store-write-buffer-file (buffer file)
  "Write BUFFER's full text to FILE without running save hooks."
  (declare (ftype (function (buffer string) t)))
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (let ((coding-system-for-write buffer-file-coding-system))
        (write-region (point-min) (point-max) file nil 'silent)))
    (set-visited-file-modtime)
    (set-buffer-modified-p nil)))

(defun dotfiles--store-release-text-change (change)
  "Release the boundary markers owned by CHANGE."
  (declare (ftype (function ((or null list)) t)))
  (when change
    (set-marker (nth 0 change) nil)
    (set-marker (nth 1 change) nil)))

(defun dotfiles--store-hook-functions (value)
  "Return hook VALUE as a list without flattening a function value."
  (declare (ftype (function (t) list))
           (important-return-value t)
           (side-effect-free t))
  (if (or (functionp value) (not (listp value)))
      (list value)
    value))

(defun dotfiles--store-install-local-hook-functions (hook before after)
  "Install BEFORE and AFTER around HOOK in the current buffer.
Return whether HOOK was already buffer-local.  Preserve inheritance from the
default hook without copying its functions into the local value."
  (declare (ftype (function (symbol list list) boolean)))
  (let ((local-p (local-variable-p hook)))
    (set (make-local-variable hook)
         (append before
                 (if local-p
                     (dotfiles--store-hook-functions (symbol-value hook))
                   '(t))
                 after))
    local-p))

(defun dotfiles--store-remove-local-hook-functions
    (hook functions originally-local-p)
  "Remove FUNCTIONS from buffer-local HOOK in the current buffer.
Preserve mutations made while the functions ran.  When HOOK was not
ORIGINALLY-LOCAL-P, remove an otherwise unchanged inheritance-only binding."
  (declare (ftype (function (symbol list boolean) t)))
  (when (local-variable-p hook)
    (let ((remaining
           (copy-sequence
            (dotfiles--store-hook-functions (symbol-value hook)))))
      (dolist (function functions)
        (setq remaining (delq function remaining)))
      (if (and (not originally-local-p) (equal remaining '(t)))
          (kill-local-variable hook)
        (set hook remaining)))))

(defun dotfiles--store-match-length (comparison cap)
  "Decode COMPARISON, a `compare-strings' result, into a match length.
`compare-strings' returns t when the compared portions match entirely
-- CAP characters at our call sites -- and otherwise a number whose
absolute value is one plus the count of leading characters that agree.
Both callers compare portions from the start of their strings; the
decode assumes that."
  (declare (ftype (function ((or integer (member t)) integer) integer))
           (important-return-value t)
           (side-effect-free t))
  (if (eq comparison t)
      cap
    (1- (abs comparison))))

(defun dotfiles--store-record-text-change (before previous)
  "Record the current buffer's textual change from BEFORE.
Release PREVIOUS.  The returned markers bound one exact replacement hunk; its
unchanged middle can be broad, but rollback refuses the hunk if any of it has
subsequently changed."
  (declare (ftype (function (string (or null list)) (or null list)))
           (important-return-value t))
  (let* ((after (dotfiles--store-buffer-text))
         (before-length (length before))
         (after-length (length after))
         (shared-length (min before-length after-length))
         ;; Char-by-char `aref' scans are quadratic here: these are whole-file
         ;; multibyte snapshots, and alternating `aref' between two strings
         ;; defeats the global char-to-byte cache, so each access re-walks the
         ;; string.  `compare-strings' finds the first mismatch in one C pass;
         ;; the suffix reuses it over the reversed unshared tails.
         (prefix (dotfiles--store-match-length
                  (compare-strings before 0 shared-length
                                   after 0 shared-length)
                  shared-length))
         (max-suffix (- shared-length prefix))
         (suffix
          (if (zerop max-suffix)
              0
            (dotfiles--store-match-length
             (compare-strings
              (reverse (substring before (- before-length max-suffix)))
              nil nil
              (reverse (substring after (- after-length max-suffix)))
              nil nil)
             max-suffix))))
    (dotfiles--store-release-text-change previous)
    (unless (and (= prefix before-length) (= prefix after-length))
      (save-restriction
        (widen)
        (let* ((start-position (+ (point-min) prefix))
               (end-position (+ start-position
                                (- after-length prefix suffix))))
          (list (copy-marker start-position)
                (copy-marker end-position t)
                (substring before prefix (- before-length suffix))
                (substring after prefix (- after-length suffix))))))))

(defun dotfiles--store-replace-text-change
    (buffer change expected replacement)
  "In BUFFER, replace CHANGE's EXPECTED text with REPLACEMENT.
EXPECTED and REPLACEMENT are indexes into CHANGE.  Return `replaced',
`changed' or `unavailable'."
  (declare (ftype (function (buffer list integer integer) symbol))
           (important-return-value t))
  (let ((start (nth 0 change))
        (end (nth 1 change)))
    (cond
     ((not (and (buffer-live-p buffer)
                (eq (marker-buffer start) buffer)
                (eq (marker-buffer end) buffer)
                (<= (marker-position start) (marker-position end))))
      'unavailable)
     ((not (with-current-buffer buffer
             (save-restriction
               (widen)
               (equal (buffer-substring-no-properties start end)
                      (nth expected change)))))
      'changed)
     (t
      (with-current-buffer buffer
        (save-restriction
          (widen)
          (delete-region start end)
          (goto-char start)
          (insert (nth replacement change))))
      'replaced))))

(defun dotfiles--store-track-archive-change
    (archive-buffer archive-text paste change)
  "Track the finalized subtree and destination change in ARCHIVE-BUFFER.
ARCHIVE-TEXT is the destination's pre-transaction text.  PASTE and CHANGE are
the previous early-hook records, or nil.  Return their replacements, or nil
when the existing record can no longer be updated safely."
  (declare (ftype (function (buffer string (or null list) (or null list))
                            (or null list)))
           (important-return-value t))
  (when (eq (current-buffer) archive-buffer)
    (save-restriction
      (widen)
      (save-excursion
        (let ((start-position
               (if paste
                   (and (eq (marker-buffer (nth 0 paste)) archive-buffer)
                        (marker-position (nth 0 paste)))
                 (org-back-to-heading t)
                 (point))))
          (when start-position
            (goto-char start-position)
            (when (and (org-at-heading-p) (= (point) start-position))
              (let* ((start (copy-marker start-position t))
                     (end (copy-marker
                           (progn (org-end-of-subtree t t) (point))))
                     (new-paste
                      (list start end
                            (buffer-substring-no-properties start end)))
                     (new-change
                      (if change
                          (let* ((old-paste (nth 2 paste))
                                 (replacement (nth 2 new-paste))
                                 (expected (nth 3 change))
                                 (match
                                  (and (not (string-empty-p old-paste))
                                       (string-match
                                        (regexp-quote old-paste) expected))))
                            (when (and match
                                       (not (string-match
                                             (regexp-quote old-paste)
                                             expected
                                             (+ match (length old-paste)))))
                              (list
                               (nth 0 change) (nth 1 change) (nth 2 change)
                               (concat (substring expected 0 match)
                                       replacement
                                       (substring expected
                                                  (+ match
                                                     (length old-paste)))))))
                        (dotfiles--store-record-text-change
                         archive-text nil))))
                (if (not new-change)
                    (progn
                      (set-marker start nil)
                      (set-marker end nil)
                      nil)
                  (when paste
                    (set-marker (nth 0 paste) nil)
                    (set-marker (nth 1 paste) nil))
                  (list new-paste new-change))))))))))

(defun dotfiles--store-paste-unchanged-p (buffer paste)
  "Return non-nil when PASTE still bounds its recorded text in BUFFER."
  (declare (ftype (function (buffer (or null list)) boolean))
           (important-return-value t)
           (side-effect-free t))
  (and paste
       (buffer-live-p buffer)
       (eq (marker-buffer (nth 0 paste)) buffer)
       (eq (marker-buffer (nth 1 paste)) buffer)
       (with-current-buffer buffer
         (save-restriction
           (widen)
           (equal (buffer-substring-no-properties
                   (nth 0 paste) (nth 1 paste))
                  (nth 2 paste))))))

(defun dotfiles--store-remove-unique-text (text removal)
  "Return TEXT without its sole occurrence of REMOVAL, or nil."
  (declare (ftype (function (string string) (or null string)))
           (important-return-value t)
           (side-effect-free t))
  (let ((match (and (not (string-empty-p removal))
                    (string-match (regexp-quote removal) text))))
    (when (and match
               (not (string-match
                     (regexp-quote removal) text (+ match (length removal)))))
      (concat (substring text 0 match)
              (substring text (+ match (length removal)))))))

(defun dotfiles--store-archive-copy-durable-p (buffer paste file)
  "Return non-nil when PASTE is unchanged in BUFFER and persisted to FILE."
  (declare (ftype (function (buffer (or null list) string) boolean))
           (important-return-value t)
           (side-effect-free t))
  (and (dotfiles--store-paste-unchanged-p buffer paste)
       (file-exists-p file)
       (let ((live-text
              (with-current-buffer buffer (dotfiles--store-buffer-text))))
         (with-temp-buffer
           (insert-file-contents file)
           (equal (buffer-string) live-text)))))

(defun dotfiles--store-durable-archive-witness (buffer paste file)
  "Return FILE bytes when BUFFER and FILE agree on unchanged PASTE."
  (declare (ftype (function (buffer (or null list) string) (or null string)))
           (important-return-value t)
           (side-effect-free t))
  (and (dotfiles--store-archive-copy-durable-p buffer paste file)
       (dotfiles--store-file-bytes file)))

(defun dotfiles--store-text-occurrences (text needle)
  "Return the number of non-overlapping NEEDLE occurrences in TEXT."
  (declare (ftype (function (string string) integer))
           (important-return-value t)
           (side-effect-free t))
  (let ((regexp (regexp-quote needle))
        (start 0)
        (count 0))
    (while (string-match regexp text start)
      (setq count (1+ count)
            start (match-end 0)))
    count))

(defun dotfiles--store-save-source-durably
    (buffer task-text expected-count)
  "Save BUFFER and verify its complete saved state and TASK-TEXT count."
  (declare (ftype (function (buffer string integer) boolean))
           (important-return-value t))
  (let* ((saved-text nil)
         (save-observer
          (lambda () (setq saved-text (dotfiles--store-buffer-text)))))
    (with-current-buffer buffer
      ;; Prepending remains a leading local hook on Emacs 27, before hook
      ;; depths were supported.
      (add-hook 'after-save-hook save-observer nil t)
      (unwind-protect
          (save-buffer)
        (remove-hook 'after-save-hook save-observer t)))
    (and saved-text
         (buffer-live-p buffer)
         (with-current-buffer buffer
           (let ((file-text (dotfiles--store-file-text buffer-file-name)))
             (and (equal file-text saved-text)
                  (= (dotfiles--store-text-occurrences saved-text task-text)
                     expected-count)
                  (= (dotfiles--store-text-occurrences
                      (dotfiles--store-buffer-text) task-text)
                     expected-count)))))))

(defun dotfiles--store-rollback-saved-archive-copy
    (buffer paste file observed-bytes file-existed &optional initial-file)
  "Remove and persist PASTE from BUFFER when FILE still has OBSERVED-BYTES.
FILE-EXISTED records whether FILE existed before the transaction.  Return
`persisted', `removed-file', `changed' or `unavailable'.  When INITIAL-FILE
is a cons of its prior existence and bytes, restore that state after removing
PASTE from the live buffer."
  (declare (ftype (function
                   (buffer (or null list) string (or null string) boolean
                           &optional (or null cons))
                   symbol))
           (important-return-value t))
  (let* ((disk-text (and observed-bytes (dotfiles--store-file-text file)))
         (restored-disk
          (and disk-text paste
               (dotfiles--store-remove-unique-text
                disk-text (nth 2 paste)))))
    (if (not (and (buffer-live-p buffer)
                  restored-disk
                  (equal (dotfiles--store-file-bytes file) observed-bytes)
                  (dotfiles--store-paste-unchanged-p buffer paste)))
        'unavailable
      (let ((paste-change (append paste '("")))
            (live-modified
             (with-current-buffer buffer (buffer-modified-p))))
      (pcase (dotfiles--store-replace-text-change buffer paste-change 2 3)
        ('replaced
         (let ((restored-live
                (with-current-buffer buffer
                  (dotfiles--store-buffer-text))))
           (condition-case persistence-error
               (let ((status 'persisted)
                     persisted-text)
                 (cond
                  (initial-file
                   (if (car initial-file)
                       (dotfiles--store-write-file-bytes
                        file (cdr initial-file))
                     (delete-file file)
                     (setq status 'removed-file)))
                  ((and (not file-existed) (string-empty-p restored-disk))
                   (delete-file file)
                   (setq status 'removed-file))
                  (t
                   (dotfiles--store-write-file-text
                    file restored-disk
                    (with-current-buffer buffer buffer-file-coding-system))))
                 (when (eq status 'persisted)
                   (setq persisted-text (dotfiles--store-file-text file))
                   (with-current-buffer buffer
                     (set-visited-file-modtime)
                     (set-buffer-modified-p
                      (not (equal restored-live persisted-text)))))
                 status)
             (error
              (dotfiles--store-replace-text-change
               buffer paste-change 3 2)
              (with-current-buffer buffer
                (set-buffer-modified-p live-modified))
              (signal (car persistence-error) (cdr persistence-error))))))
        (status status))))))

(defun dotfiles--store-restore-source-cut (source-buffer cut)
  "Restore CUT in SOURCE-BUFFER without changing later source edits."
  (declare (ftype (function (buffer list) t)))
  (let ((boundary (nth 0 cut))
        (text (nth 1 cut)))
    (unless (and (buffer-live-p source-buffer)
                 (eq (marker-buffer boundary) source-buffer))
      (error "Archive source disappeared before its task could be restored"))
    (with-current-buffer source-buffer
      (save-restriction
        (widen)
        (let ((start (marker-position boundary)))
          (goto-char start)
          (insert text)
          (when org-provide-todo-statistics
            (goto-char start)
            (when (org-up-heading-safe)
              (org-update-statistics-cookies nil)))
          start)))))

(defvar dotfiles--store-active-archive-transactions nil
  "Active archive transaction contexts, innermost first.")

(defun dotfiles--store-archive-transaction
    (archive-file task invocation archive-call &optional source-hook-tail)
  "Run ARCHIVE-CALL transactionally for TASK under INVOCATION.
Run SOURCE-HOOK-TAIL after existing source archive hooks and before the
transaction's cut tracker.
After the cut, persist the destination and then the source.  On failure, restore
the source cut and markers, then compensate the exact recorded destination copy
independently in the live buffer and file once the restored source is verified
durable.  Preserve independently saved destination edits, later unsaved edits,
and successful nested archives outside that bounded copy.  Before accepting
success, verify the expected source and finalized destination states both live
and on disk."
  (declare (ftype (function
                   (string marker symbol function &optional list) t)))
  (let* ((_archive-file-check
          (dotfiles--store-check-archive-file archive-file))
         (source-buffer (current-buffer))
         (source-file (buffer-file-name (buffer-base-buffer)))
         (archive-file-existed (file-exists-p archive-file))
         (archive-disk-bytes (dotfiles--store-file-bytes archive-file))
         (existing-buffer (find-buffer-visiting archive-file))
         (archive-buffer
          (let ((buffer (find-file-noselect archive-file 'nowarn)))
            (with-current-buffer buffer
              (unless (derived-mode-p 'org-mode) (org-mode)))
            buffer))
         (created-buffer (not existing-buffer))
         (archive-text
          (with-current-buffer archive-buffer
            (dotfiles--store-buffer-text)))
         (archive-point (with-current-buffer archive-buffer (point)))
         (archive-modified
          (with-current-buffer archive-buffer (buffer-modified-p)))
         (destination-finalize-hook-local-p
          (with-current-buffer archive-buffer
            (local-variable-p 'org-archive-finalize-hook)))
         (marker-snapshot (dotfiles--store-org-marker-snapshot task))
         archive-paste archive-change archive-change-group
         archive-change-group-active archive-change-group-rolled-back
         source-change-group
         source-change-group-active source-cut source-cut-complete
         source-cut-occurrences source-restored-start
         archive-save-attempted archive-save-owned archive-owned-save-started
         archive-independent-save archive-observed-save-bytes
         archive-save-attempt-text archive-save-attempt-owned
         archive-save-pending
         archive-pending-write-bytes archive-current-witness-bytes
         archive-written-bytes saved-archive-candidate
         restore-initial-archive-file-p
         archive-copy-compensated source-durable
         saved-copy-rollback-error archive-change-rollback-error
         completed original-error rollback-errors result
         (active-archive-transactions
          dotfiles--store-active-archive-transactions)
         (archive-transaction
          (vector
           archive-file nil
           (lambda ()
             (when archive-change-group-active
               (with-current-buffer archive-buffer
                 (accept-change-group archive-change-group))
               (setq archive-change-group-active nil)))))
         (dotfiles--store-active-archive-transactions
          (cons archive-transaction active-archive-transactions))
         (archive-save-started
          (lambda ()
            (setq archive-save-attempted t
                  archive-save-pending t
                  archive-save-attempt-owned archive-save-owned
                  archive-save-attempt-text
                  (dotfiles--store-buffer-text))
            (if (and archive-save-owned
                     (not archive-owned-save-started))
                (setq archive-owned-save-started t)
              (setq archive-independent-save t))))
         (archive-save-finished
          (lambda ()
            (setq archive-save-pending nil
                  archive-observed-save-bytes
                  (dotfiles--store-file-bytes archive-file))
            (if (and archive-save-owned
                     (not archive-written-bytes)
                     (dotfiles--store-paste-unchanged-p
                      archive-buffer archive-paste))
                (setq archive-written-bytes
                      (dotfiles--store-file-bytes archive-file))
              (setq archive-independent-save t)))))
    (when (dotfiles--store-same-file-p archive-file source-file)
      (user-error "Same-file order-task archives are unsupported"))
    (dolist (active active-archive-transactions)
      (when (dotfiles--store-same-file-p archive-file (aref active 0))
        (aset active 1 t)
        (funcall (aref active 2))))
    (with-current-buffer archive-buffer
      (add-hook 'before-save-hook archive-save-started -99 t)
      (add-hook 'after-save-hook archive-save-finished -99 t)
      (setq archive-change-group (prepare-change-group))
      (activate-change-group archive-change-group)
      (setq archive-change-group-active t))
    (cl-labels
        ((record-rollback-error
          (error-data)
          (push error-data rollback-errors))
         (restore-archive-metadata
          ()
          (when (and (buffer-live-p archive-buffer)
                     (with-current-buffer archive-buffer
                       (equal (dotfiles--store-buffer-text) archive-text)))
            (with-current-buffer archive-buffer
              (save-restriction
                (widen)
                (goto-char
                 (min (max archive-point (point-min)) (point-max))))
              (set-buffer-modified-p archive-modified))
            t))
         (install-save-observers
          ()
          (with-current-buffer archive-buffer
            (add-hook 'before-save-hook archive-save-started -99 t)
            (add-hook 'after-save-hook archive-save-finished -99 t)))
         (track-destination
          ()
          (when (and (not (aref archive-transaction 1))
                     (eq invocation dotfiles--org-archive-invocation-token))
            ;; Org may have selected `org-mode' after the transaction opened a
            ;; new extension-less archive buffer, clearing its local hooks.
            (install-save-observers)
            (unless (and marker-snapshot (aref (car marker-snapshot) 7))
              (dotfiles--store-capture-owned-org-markers marker-snapshot))
            (let ((record (dotfiles--store-track-archive-change
                           archive-buffer archive-text
                           archive-paste archive-change)))
              (when record
                (setq archive-paste (nth 0 record)
                      archive-change (nth 1 record))
                (when archive-change-group-active
                  (accept-change-group archive-change-group)
                  (setq archive-change-group-active nil))))))
         (run-archive-call
          ()
          (let* ((destination-before-tracker
                  (lambda () (track-destination)))
                 (destination-after-tracker
                  (lambda () (track-destination)))
                 (destination-trackers
                  (list destination-before-tracker destination-after-tracker))
                 (source-cut-tracker
                  (lambda ()
                    (when (eq invocation
                              dotfiles--org-archive-invocation-token)
                      (unless (eq (current-buffer) source-buffer)
                        (user-error
                         "Archive source buffer changed before cut"))
                      (unless archive-change
                        (with-current-buffer archive-buffer
                          (track-destination)))
                      (unless
                          (and archive-change
                               (dotfiles--store-paste-unchanged-p
                                archive-buffer archive-paste))
                        (user-error
                         "Archive destination could not be verified"))
                      ;; No command-loop yield remains before the cut.
                      (let ((cut-text
                             (dotfiles--store-order-subtree task)))
                        (setq source-cut
                              (list (copy-marker task t) cut-text)
                              source-cut-occurrences
                              (dotfiles--store-text-occurrences
                               (dotfiles--store-buffer-text)
                               cut-text)
                              source-change-group
                              (prepare-change-group source-buffer)))
                      (activate-change-group source-change-group)
                      (setq source-change-group-active t))))
                 (source-owned-hooks
                  (append source-hook-tail (list source-cut-tracker)))
                 (source-hook-local-p
                  (with-current-buffer source-buffer
                    (local-variable-p 'org-archive-hook))))
            (with-current-buffer archive-buffer
              (dotfiles--store-install-local-hook-functions
               'org-archive-finalize-hook
               (list destination-before-tracker)
               (list destination-after-tracker)))
            (with-current-buffer source-buffer
              (dotfiles--store-install-local-hook-functions
               'org-archive-hook nil source-owned-hooks))
            (unwind-protect
                (with-current-buffer source-buffer (funcall archive-call))
              (when (buffer-live-p source-buffer)
                (with-current-buffer source-buffer
                  (dotfiles--store-remove-local-hook-functions
                   'org-archive-hook source-owned-hooks source-hook-local-p)))
              (when (buffer-live-p archive-buffer)
                (with-current-buffer archive-buffer
                  (dotfiles--store-remove-local-hook-functions
                   'org-archive-finalize-hook destination-trackers
                   destination-finalize-hook-local-p)))))))
      (unwind-protect
          (condition-case error-data
              (progn
                (setq result (run-archive-call))
                (unless source-change-group-active
                  (user-error "Archive source cut was not protected"))
                (accept-change-group source-change-group)
                (setq source-change-group-active nil
                      source-cut-complete t)
                (unless (buffer-live-p archive-buffer)
                  (user-error
                   "Archive destination disappeared before it could be saved"))
                (setq archive-save-owned t)
                (unwind-protect
                    (with-current-buffer archive-buffer (save-buffer))
                  (setq archive-save-owned nil))
                (unless (dotfiles--store-archive-copy-durable-p
                         archive-buffer archive-paste archive-file)
                  (user-error "Archive destination was not durably saved"))
                (unless (dotfiles--store-save-source-durably
                         source-buffer (nth 1 source-cut)
                         (1- source-cut-occurrences))
                  (user-error "Archive source cut was not durably saved"))
                (unless (dotfiles--store-archive-copy-durable-p
                         archive-buffer archive-paste archive-file)
                  (user-error
                   "Archive destination changed during the source save"))
                (setq completed t))
            (error (setq original-error error-data)))
        (when (buffer-live-p archive-buffer)
          (with-current-buffer archive-buffer
            (remove-hook 'before-save-hook archive-save-started t)
            (remove-hook 'after-save-hook archive-save-finished t)))
        (unless completed
          (when archive-change-group-active
            (condition-case cleanup-error
                (progn
                  (cancel-change-group archive-change-group)
                  (setq archive-change-group-rolled-back t
                        archive-copy-compensated t))
              (error (record-rollback-error cleanup-error)))
            (setq archive-change-group-active nil))
          (when source-change-group-active
            (condition-case cleanup-error
                (cancel-change-group source-change-group)
              (error (record-rollback-error cleanup-error)))
            (setq source-change-group-active nil))
          (when source-cut-complete
            (condition-case cleanup-error
                (setq source-restored-start
                      (dotfiles--store-restore-source-cut
                       source-buffer source-cut))
              (error (record-rollback-error cleanup-error))))
          (unless source-restored-start
            (setq source-restored-start
                  (and (eq (marker-buffer task) source-buffer)
                       (marker-position task))))
          (condition-case cleanup-error
              (dotfiles--store-restore-org-markers
               marker-snapshot source-buffer source-restored-start)
            (error (record-rollback-error cleanup-error)))
          (when source-cut-complete
            (condition-case cleanup-error
                (progn
                  (unless (dotfiles--store-save-source-durably
                           source-buffer (nth 1 source-cut)
                           source-cut-occurrences)
                    (error "Restored archive source was not durably saved"))
                  (setq source-durable t))
              (error
               (record-rollback-error cleanup-error))))
          (condition-case cleanup-error
              (progn
                (setq archive-pending-write-bytes
                      (and archive-save-pending
                           (equal (dotfiles--store-file-text archive-file)
                                  archive-save-attempt-text)
                           (dotfiles--store-file-bytes archive-file))
                      archive-current-witness-bytes
                      (dotfiles--store-durable-archive-witness
                       archive-buffer archive-paste archive-file)
                      saved-archive-candidate nil
                      restore-initial-archive-file-p nil)
                (cond
                 (archive-pending-write-bytes
                  (setq saved-archive-candidate archive-pending-write-bytes
                        restore-initial-archive-file-p
                        archive-save-attempt-owned))
                 ((and archive-independent-save
                       archive-current-witness-bytes)
                  (setq saved-archive-candidate
                        archive-current-witness-bytes))
                 ((and archive-independent-save archive-observed-save-bytes)
                  (setq saved-archive-candidate archive-observed-save-bytes))
                 (archive-written-bytes
                  (setq saved-archive-candidate archive-written-bytes
                        restore-initial-archive-file-p t))
                 (archive-current-witness-bytes
                  (setq saved-archive-candidate
                        archive-current-witness-bytes))))
            (error (record-rollback-error cleanup-error)))
          (when (and (or (not source-cut-complete) source-durable)
                     saved-archive-candidate
                     (not (equal saved-archive-candidate archive-disk-bytes))
                     (not archive-change-group-rolled-back))
            (condition-case cleanup-error
                (pcase (dotfiles--store-rollback-saved-archive-copy
                        archive-buffer archive-paste archive-file
                        saved-archive-candidate
                        archive-file-existed
                        (and restore-initial-archive-file-p
                             (cons archive-file-existed
                                   archive-disk-bytes)))
                  ('removed-file
                   (setq archive-copy-compensated t)
                   (when (restore-archive-metadata)
                     (when created-buffer
                       (with-current-buffer archive-buffer
                         (set-buffer-modified-p nil)
                         (kill-buffer archive-buffer)))))
                  ('persisted
                   (setq archive-copy-compensated t)
                   (restore-archive-metadata))
                  ((or 'changed 'unavailable)
                   (setq saved-copy-rollback-error
                         (list
                          'error
                          (format
                           "Saved archive copy could not be rolled back; left it in %s"
                           archive-file)))))
              (error (record-rollback-error cleanup-error))))
          (when (and (or (not source-cut-complete) source-durable)
                     (or (not archive-independent-save)
                         (equal (dotfiles--store-file-bytes archive-file)
                                archive-disk-bytes))
                     (not archive-copy-compensated)
                     (not archive-change-group-rolled-back))
            (condition-case cleanup-error
                (if (buffer-live-p archive-buffer)
                    (pcase (if archive-change
                               (dotfiles--store-replace-text-change
                                archive-buffer archive-change 3 2)
                             'unavailable)
                      ('replaced
                       (let ((restored-original
                              (with-current-buffer archive-buffer
                                (equal (dotfiles--store-buffer-text)
                                       archive-text))))
                         (condition-case persistence-error
                             (cond
                              (archive-file-existed
                               (let ((current-bytes
                                      (dotfiles--store-file-bytes
                                       archive-file)))
                                 (unless (equal current-bytes
                                                archive-disk-bytes)
                                   (unless (and archive-written-bytes
                                                (equal current-bytes
                                                       archive-written-bytes))
                                     (error
                                      "Archive file changed independently during rollback"))
                                   (dotfiles--store-write-file-bytes
                                    archive-file archive-disk-bytes))
                                 (with-current-buffer archive-buffer
                                   (set-visited-file-modtime))))
                              ((file-exists-p archive-file)
                               (unless (and archive-written-bytes
                                            (equal
                                             (dotfiles--store-file-bytes
                                              archive-file)
                                             archive-written-bytes))
                                 (error
                                  "New archive file changed independently during rollback"))
                               (if restored-original
                                   (delete-file archive-file)
                                 (dotfiles--store-write-buffer-file
                                  archive-buffer archive-file))))
                           (error
                            (dotfiles--store-replace-text-change
                             archive-buffer archive-change 2 3)
                            (signal (car persistence-error)
                                    (cdr persistence-error))))
                         (when (restore-archive-metadata)
                           (when created-buffer
                             (with-current-buffer archive-buffer
                               (set-buffer-modified-p nil)
                               (kill-buffer archive-buffer))))
                         (setq archive-copy-compensated t)))
                      ((or 'changed 'unavailable)
                       (setq archive-change-rollback-error
                             (list
                              'error
                              (format
                               "Archive change could not be rolled back; left it in %s"
                               archive-file)))))
                  (when (and archive-save-attempted
                             (file-exists-p archive-file))
                    (let ((current-bytes
                           (dotfiles--store-file-bytes archive-file)))
                      (unless (or (equal current-bytes archive-disk-bytes)
                                  (and archive-written-bytes
                                       (equal current-bytes
                                              archive-written-bytes)))
                        (error
                         "Archive file changed independently during rollback"))
                      (if archive-file-existed
                          (unless (equal current-bytes archive-disk-bytes)
                            (dotfiles--store-write-file-bytes
                             archive-file archive-disk-bytes))
                        (delete-file archive-file))))
                  (setq archive-copy-compensated t))
              (error (record-rollback-error cleanup-error))))
          (unless archive-copy-compensated
            (when saved-copy-rollback-error
              (record-rollback-error saved-copy-rollback-error))
            (when archive-change-rollback-error
              (record-rollback-error archive-change-rollback-error)))
        (when archive-paste
          (set-marker (nth 0 archive-paste) nil)
          (set-marker (nth 1 archive-paste) nil))
        (dotfiles--store-release-text-change archive-change)
        (dotfiles--store-release-org-marker-snapshot marker-snapshot)
        (when source-cut
          (set-marker (nth 0 source-cut) nil))))
    (when original-error
      (if rollback-errors
          (error "Archive failed (%s); rollback incomplete (%s)"
                 (error-message-string original-error)
                 (mapconcat #'error-message-string
                            (nreverse rollback-errors) "; "))
        (signal (car original-error) (cdr original-error))))
    result)))

(defun dotfiles--store-check-archive-cut
    (task expected-end expected expected-subtree store key invocation)
  "Abort STORE order KEY's archive unless it is cutting TASK unchanged.
EXPECTED is TASK's bare title and EXPECTED-SUBTREE is its complete text from
immediately before the archive started.
Runs from `org-archive-hook', with the subtree copied but not yet cut.  The
`goto-char' is load-bearing: `org-cut-subtree' takes `org-back-to-heading'
from point, so it re-anchors the cut as well as the heading read.

`org-archive-hook' is global and the binding spans the archive's own
command-loop yields, so another `org-archive-subtree' started during one runs
this too.  Do nothing unless INVOCATION and TASK's buffer both match.

The heading at TASK catches an edit made since EXPECTED was captured, or a
marker collapsed onto the following entry by a deletion.  The complete subtree
snapshot catches body, property and child edits made after the archive copy.

What no anchor at TASK can see is the copy having been taken from another
subtree.  `org-subtree-clip' records what was copied while the point restored
by `org-archive-subtree' records where that copy started.  Require both: either
witness can be invalidated independently by command-loop activity, and cutting
after losing either one cannot be verified.  Compare the complete clip with the
complete pre-archive subtree so body-only changes cannot pass this check."
  (declare (ftype (function
                   (marker marker string string string string symbol) t)))
  (when (and (eq invocation dotfiles--org-archive-invocation-token)
             (eq (current-buffer) (marker-buffer task)))
    ;; Read before the `goto-char' below overwrites it.  `org-back-to-heading'
    ;; signals before the first heading, which from a hook would abort the
    ;; archive with an unrelated message.
    (let ((archive-anchor (unless (org-before-first-heading-p)
                            (save-excursion (org-back-to-heading t) (point)))))
      (unless (and (eq (marker-buffer expected-end) (current-buffer))
                   (< (marker-position task) (marker-position expected-end)))
        (user-error "%s order task %s changed mid-archive; copy archived"
                    store key))
      (goto-char task)
      (unless (equal (org-get-heading t t t t) expected)
        (user-error "%s order task %s changed mid-archive; copy archived"
                    store key))
      (unless (equal (dotfiles--store-order-subtree task) expected-subtree)
        (user-error "%s order task %s changed mid-archive; copy archived"
                    store key))
      (org-back-to-heading t)
      (unless (and (= (save-excursion
                        (org-end-of-subtree t t)
                        (point))
                      (marker-position expected-end))
                   (equal org-subtree-clip expected-subtree)
                   (eql archive-anchor (point)))
        (user-error "%s order %s: archive copied another subtree; copy archived"
                    store key)))))

(defun dotfiles--store-order-archive-file (task source-file store key)
  "Return TASK's current archive file, or nil when it is a sub-action.
Reject SOURCE-FILE as STORE order KEY's destination."
  (declare (ftype (function (marker string string string) (or null string)))
           (important-return-value t))
  (goto-char task)
  (when (dotfiles--org-task-top-level-p)
    (let ((archive-file
           (car (org-archive--compute-location
                 (or (org-entry-get nil "ARCHIVE" 'inherit)
                     org-archive-location)))))
      (dotfiles--store-check-archive-file archive-file)
      (when (dotfiles--store-same-file-p archive-file source-file)
        (user-error "%s order %s uses a same-file archive destination"
                    store key))
      archive-file)))

(defun dotfiles--store-task-extent-current-p (task end)
  "Return non-nil when TASK through END is still one complete subtree."
  (declare (ftype (function (marker marker) boolean))
           (important-return-value t)
           (side-effect-free t))
  (and (eq (marker-buffer task) (current-buffer))
       (eq (marker-buffer end) (current-buffer))
       (< (marker-position task) (marker-position end))
       (save-excursion
         (org-with-wide-buffer
          (goto-char task)
          (and (org-at-heading-p)
               (= (progn (org-end-of-subtree t t) (point))
                  (marker-position end)))))))

(defun dotfiles--mu4e-complete-order-task (org-file store msg key)
  "Complete the STORE @waitingfor order task in ORG-FILE for a delivered MSG.
Find the task by KEY (an order ID) via `dotfiles--store-find-order-task', which
requires KEY to be a single whitespace-free token and otherwise rejects it with
a `user-error'.  Append an `org' link to MSG, then prompt to mark it DONE; on
yes, complete it and, when it is a top-level task (not a project sub-action),
archive its subtree.  Signal a `user-error' when no such task exists, when its
archive destination is the source file, when completion is blocked, or when the
task changed during the prompt or completion -- re-running the automation is
safe there, the append dedups.
Signal one as well when the archive is not cutting that task.  The archive's own
save is suppressed so its paste, source cut and Org-marker moves can be rolled
back before the error escapes."
  (declare (ftype (function (string string list string) t)))
  (dotfiles--with-store-order-task org-file store msg key
    (if (not task)
        (user-error "No %s order task found for %s" store key)
      (goto-char task)
      (let* ((expected (org-get-heading t t t t))
             (source-file (buffer-file-name (buffer-base-buffer))))
        ;; Keep the early check so a known same-file destination cannot even
        ;; add the mail link.  The plan is recomputed after completion.
        (dotfiles--store-order-archive-file task source-file store key)
        (unless (dotfiles--org-append-mu4e-link link msgid)
          (message "%s order link already filed: %s" store key))
        (let ((prompt-subtree (dotfiles--store-order-subtree task))
              (prompt-end
               (save-excursion
                 (org-with-wide-buffer
                  (goto-char task)
                  (org-back-to-heading t)
                  (org-end-of-subtree t t)
                  (copy-marker (point))))))
          (unwind-protect
              ;; Rebind across the prompt only: see
              ;; `dotfiles--with-store-order-task' for why the guard must not
              ;; span a command-loop yield.
              (when
                  (let ((org-ignore-region nil))
                    (y-or-n-p
                     (format "Mark %s order %s as completed? " store key)))
                ;; A start marker can collapse onto an identical successor when
                ;; its subtree is deleted.  The end marker makes that extent
                ;; empty, while the text snapshot catches every in-place edit.
                (unless (and (dotfiles--store-task-extent-current-p
                              task prompt-end)
                             (equal
                              (buffer-substring-no-properties task prompt-end)
                              prompt-subtree)
                             (equal (dotfiles--store-order-subtree task)
                                    prompt-subtree))
                  (user-error
                   "%s order task for %s changed during the prompt" store key))
                (goto-char task)
                (let ((org-loop-over-headlines-in-active-region nil))
                  (org-autotask-complete-item))
                (unless (and
                         (dotfiles--store-task-extent-current-p task prompt-end)
                         (save-excursion
                           (goto-char task)
                           (equal (org-get-heading t t t t) expected)))
                  (user-error
                   "%s order task for %s changed during completion" store key))
                (goto-char task)
                (unless (equal (org-get-todo-state)
                               org-autotask-keyword-done)
                  (user-error "%s order task for %s did not complete"
                              store key))
                ;; Completion hooks may change both project status and the
                ;; inherited archive location, so this is the authoritative
                ;; plan used immediately before the transaction.
                (let ((archive-file
                       (dotfiles--store-order-archive-file
                        task source-file store key)))
                  (when archive-file
                    ;; `org-archive-subtree' reads its region gate in its first
                    ;; form and then yields, so nothing can cover that gate
                    ;; without spanning the yields.  Per the section
                    ;; commentary's region rule, hold the narrowest policy
                    ;; variable that covers it:
                    ;; `org-loop-over-headlines-in-active-region', which reaches
                    ;; the archive buffer too.
                    ;;
                    ;; Those yields fall between the archive copy and the
                    ;; irreversible cut, so no check made before the archive
                    ;; started can cover the cut.  Re-check on
                    ;; `org-archive-hook', which runs with the subtree copied but
                    ;; not yet deleted; appended rather than consed so it is the
                    ;; last thing before the cut.
                    ;;
                    ;; `org-archive-subtree' would save the archive file before
                    ;; that hook runs, putting the copy beyond the guard's
                    ;; reach; suppress that and let the transaction save it once
                    ;; the cut has happened.  On abort, the transaction restores
                    ;; the cut and markers, and removes the invocation's paste
                    ;; only while its exact text is unchanged.
                    (let* ((expected-subtree
                            (dotfiles--store-order-subtree task))
                           (expected-end
                            (save-excursion
                              (org-with-wide-buffer
                               (goto-char task)
                               (org-end-of-subtree t t)
                               (copy-marker (point)))))
                           (archive-invocation
                            (make-symbol "order-task-archive"))
                           (ambient-archive-loop-policy
                            org-loop-over-headlines-in-active-region)
                           (ambient-archive-save-policy
                            org-archive-subtree-save-file-p)
                           (ambient-ignore-region org-ignore-region)
                           (dotfiles--org-archive-protected-token
                            archive-invocation)
                           (dotfiles--org-archive-ambient-loop-policy
                            ambient-archive-loop-policy)
                           (dotfiles--org-archive-ambient-save-policy
                            ambient-archive-save-policy)
                           (dotfiles--org-archive-ambient-ignore-region
                            ambient-ignore-region)
                           (org-ignore-region nil)
                           (org-loop-over-headlines-in-active-region nil)
                           (org-archive-subtree-save-file-p nil)
                           (archive-cut-check
                            (lambda ()
                              (dotfiles--store-check-archive-cut
                               task expected-end expected expected-subtree
                               store key archive-invocation))))
                      (unwind-protect
                          (dotfiles--store-archive-transaction
                           archive-file task archive-invocation
                           (lambda ()
                             (let ((dotfiles--org-archive-invocation-token
                                    archive-invocation)
                                   (dotfiles--org-archive-requested-token
                                    archive-invocation))
                               (org-archive-subtree)))
                           (list archive-cut-check))
                        (set-marker expected-end nil)))))
            (set-marker prompt-end nil))))))))

;;; `org-gcal' helpers

;; Avoid requiring `org-gcal', which warns until credentials are reloaded.
(declare-function org-gcal-post-at-point "ext:org-gcal"
                  (&optional skip-import skip-export existing-mode))

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
