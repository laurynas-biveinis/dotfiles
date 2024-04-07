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
(defconst dotfiles--gh-closed-pr-url-format
  (concat "Closed .*\\(" dotfiles--gh-url-prefix ".*/.*/pull/%s\\)"))
(defconst dotfiles--gh-org-and-project
  (concat dotfiles--gh-url-prefix "\\(.*\\)/pull/[0-9]+"))
(defconst dotfiles--gh-repo "\\(https://github.com/.*/.*\\)/pull/[0-9]+")

;;; External program helpers

(defun dotfiles--open-file (file)
  "Open the FILE in its default app."
  (shell-command (concat "open " (shell-quote-argument file))))

;; Command-line program helper

(defun dotfiles--run-program (program args success-fn)
  "Run PROGRAM with ARGS, executing SUCCESS-FN on zero exit..
ARGS must be a list of strings passed to PROGRAM.
SUCCESS-FN is executed on zero exit with a single string argument containing the
output of execution.
In the case of non-zero exit code it is printed as user error together with any
output."
  (with-temp-buffer
    (let ((exit-code (apply #'call-process program nil t nil args)))
      (when (/= 0 exit-code)
        (user-error "%s %s failed with exit code %d and output %s" program
                    (mapconcat 'identity args " ") exit-code (buffer-string)))
      (funcall success-fn (buffer-string)))))

;; Command-line "gh" utility helper

(defun dotfiles--gh-get (args)
  "Run gh with ARGS, return its output with the final newline trimmed.
ARGS must be properly quoted if needed."
  (substring (shell-command-to-string (concat "gh " args)) 0 -1))

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

(defun dotfiles--for-each-attachment (fn)
  "Call FN for each attachment with its handle and path."
  (let ((mime-parts (mu4e-view-mime-parts)))
    (dolist (part mime-parts)
      (let* ((attachment-handle (plist-get part :handle))
             (file-name (plist-get part :filename))
             (file-path (mu4e-join-paths (plist-get part :target-dir)
                                         file-name)))
        (funcall fn attachment-handle file-path)))))

(defun dotfiles--download-and-open-mu4e-all-attachments (suffix)
  "Download all attachments with file name SUFFIX from a `mu4e' message."
  (dotfiles--for-each-attachment
   (lambda (handle path)
     (when (string-suffix-p suffix path t)
       (mm-save-part-to-file handle path)
       (dotfiles--open-file path)))))

(defun dotfiles--download-mu4e-all-jpgs ()
  "Download all .jpg attachments from a `mu4e' message."
  (dotfiles--for-each-attachment
   (lambda (handle path)
     (when (string-suffix-p ".jpg" path t)
       (mm-save-part-to-file handle path)))))

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

(require 'org-clock)

(defun dotfiles--require-org-clock ()
  "Return user error if no `org' task is currently clocked in."
  (unless (org-clocking-p)
    (user-error "No org task is clocked-in")))

(defun dotfiles--with-different-org-clock (func &rest args)
  "Save the current org clock, clock-in, call FUNC with ARGS, restore clock.

The marker must be at the new clock position."
  (let ((current-clock-marker (when (org-clocking-p)
                                (copy-marker org-clock-marker))))
    (unwind-protect
        (progn
          (org-clock-in)
          (apply func args))
      (if current-clock-marker
          (org-with-point-at current-clock-marker
            (org-clock-in))
        (org-clock-out)))))

(provide 'my-lib)
;;; my-lib.el ends here
