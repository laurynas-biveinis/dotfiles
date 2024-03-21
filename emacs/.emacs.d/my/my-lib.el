;;; my-lib.el --- helpers for other code.  -*- lexical-binding: t; -*-

;;; Commentary:

;; This contains internal helpers used elsewhere.

;;; Code:

(defconst dotfiles--muspy-album-release-date-and-title
  "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\): \\(.*? - .*?\\) (Album)")

(defconst dotfiles--gh-pr-in-subject "^.*\(PR #\\([0-9]+\\)\)$")
(defconst dotfiles--gh-url-prefix "https://github.com/")
(defconst dotfiles--gh-closed-pr-url-format
  (concat "Closed .*\\(" dotfiles--gh-url-prefix ".*/.*/pull/%s\\)"))
(defconst dotfiles--gh-org-and-project
  (concat dotfiles--gh-url-prefix "\\(.*\\)/pull/[0-9]+"))
(defconst dotfiles--gh-repo "\\(https://github.com/.*/.*\\)/pull/[0-9]+")

(require 'seq)
(require 'mu4e-mime-parts)

(defun dotfiles--get-qp-encoded-html-part ()
  "For a `mu4e' message, get its first quoted-printable-encoded HTML part."
  (seq-find (lambda (part)
              (and (string= "text/html" (plist-get part :mime-type))
                   (eq 'quoted-printable (plist-get part :encoding))))
            (mu4e-view-mime-parts)))

(defun dotfiles--get-mu4e-msg-csv-part ()
  "For a `mu4e' message, get its first .csv attachment part, if any."
  (seq-find (lambda (part)
              (string-suffix-p ".csv" (plist-get part :filename) t))
            (mu4e-view-mime-parts)))

(defun dotfiles--save-mu4e-msg-part-file (part)
  "For a `mu4e' message PART, save it as a file and return its path."
  (let* ((base-dir (plist-get part :target-dir))
         (file-path (mu4e-join-paths base-dir (plist-get part :filename))))
    (mm-save-part-to-file (plist-get part :handle) file-path)
    file-path))

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
