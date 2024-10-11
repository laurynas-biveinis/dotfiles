;;; my-org-gtd.el --- My GTD setup in `org' -*- lexical-binding: t -*-

;; Version: 0.1
;; URL: https://github.com/laurynas-biveinis/dotfiles/
;; Package-Requires: ((emacs "27.1"))
;; Keywords: outlines

;;; Commentary:

;; This is a proto-package for my GTD implementation in `org'.

;;; Code:

;; Hard dependencies
(require 'cl-lib)
(require 'org)
(require 'org-agenda)
(require 'org-clock)
(require 'org-element)

;; Soft dependencies
(defvar org-gcal-cancelled-todo-keyword)

;; The context structure. FIXME(laurynas): rename, not only context
(cl-defstruct (my-org-gtd-context)
  "A single GTD context."
  (tag "" :type string :read-only t :documentation "The `org' tag.")
  (select-char
   ? :type character :read-only t
   :documentation "The `org' quick selection character for the tag.")
  (description "" :type string :read-only t
               :documentation "The description string for this context."))

(defun my-org-gtd-context-not-tag (gtd-context)
  "Get the substring for `org-agenda' blocks to exclude GTD-CONTEXT."
  (concat "-" (my-org-gtd-context-tag gtd-context)))

;; Customization
(defgroup my-org-gtd nil
  "Configure `org' for GTD."
  :group 'org)

(defcustom my-org-gtd-contexts nil
  "GTD contexts with `org' tags, quick selection characters, and descriptions.
The tags and the selection keys will be added to as a single group to
`org-tag-alist', together with (`my-org-gtd-waitingfor-tag' .
`my-org-gtd-waitingfor-select')  by`my-org-gtd-initialize'."
  :type '(repeat (struct :tag "Context"
                         (string :tag "`org' Tag")
                         (character :tag "Quick selection character")
                         (string :tag "Description")))
  :group 'my-org-gtd
  :package-version '(my-org-gtd . "0.1"))

(defcustom my-org-gtd-waitingfor-context
  (make-my-org-gtd-context :tag "@waitingfor" :select-char ?w
                           :description "Waiting-for items")
  "The GTD waiting-for context."
  :type '(struct :tag "GTD waiting-for context"
                 (string :tag "`org Tag")
                 (character :tag "Quick selection character")
                 (string :tag "Description"))
  :group 'my-org-gtd
  :package-version '(my-org-gtd . "0.1"))

(defcustom my-org-gtd-project-context
  (make-my-org-gtd-context :tag "project" :select-char ?p
                           :description "Projects")
  "The GTD project context."
  :type '(struct :tag "Projects. Not a GTD context but works as one in `org'."
                 (string :tag "`org Tag")
                 (character :tag "Quick selection character")
                 (string :tag "Description"))
  :group 'my-org-gtd
  :package-version '(my-org-gtd . "0.1"))

(defcustom my-org-gtd-somedaymaybe-context
  (make-my-org-gtd-context :tag "somedaymaybe" :select-char ?m
                           :description "Someday/maybe")
  "The GTD someday/maybe context."
  :type '(struct :tag "Someday/maybe item context."
                 (string :tag "`org Tag")
                 (character :tag "Quick selection character")
                 (string :tag "Description"))
  :group 'my-org-gtd
  :package-version '(my-org-gtd . "0.1"))

;; Action keywords
(defcustom my-org-gtd-next-action-keyword "TODO"
  "The TODO entry keyword that designates a GTD next action.
Projects also have this keyword (in addition to `my-org-gtd-project-context'
tag.) It must be present in `org-todo-keywords', either directly or through
per-file configuration, with an optional fast state selection character."
  :type '(string)
  :group 'my-org-gtd
  :package-version '(my-org-gtd . "0.1"))

(defcustom my-org-gtd-done-keyword "DONE"
  "The TODO entry keyword that designates a completed task or project.
It must be present in `org-todo-keyword', either directly or thorugh per-file
configuration, with an optional fast state selection character."
  :type '(string)
  :group 'my-org-gtd
  :package-version '(my-org-gtd . "0.1"))

(defcustom my-org-gtd-cancelled-keyword "KILL"
  "The TODO entry keyword that designates a cancelled task or project.
It must be present in `org-todo-keywords', either directly or through per-file
configuration, with an optional fast state selection character."
  :type '(string)
  :group 'my-org-gtd
  :package-version '(my-org-gtd . "0.1"))

;; Clocking automation

(defcustom my-org-gtd-clock-gated-commands '()
  "List of commands that should be gated by `my-org-gtd-require-org-clock'."
  :type '(repeat symbol)
  :group 'my-org-gtd
  :package-version '(my-org-gtd . "0.1"))

(defcustom my-org-gtd-clock-in-actions
  '((:property "URL" :action browse-url :multi t)
    (:property "APP" :action my-org-gtd--clock-in-open-macos-app)
    (:property "SHELL" :action shell-command)
    (:property "VISIT" :action my-org-gtd--clock-in-visit-file)
    (:property "EVAL" :action my-org-gtd--clock-in-eval-elisp))
  "Configuration for actions to perform when clocking in.
Each entry is a plist with `:property', `:action', and optionally `:multi' keys.
`:property' is the name of the Org property to look for.
`:action' is the function to call with the property value.
`:multi', if non-nil, indicates that multiple values are allowed for the
property."
  :type '(repeat (plist :options
                        ((:property (string :tag "Org node property"))
                         (:action (function :tag "Action function"))
                         (:multi (choice (const :tag "Single value" nil)
                                         (const :tag "Multiple values" t))))))
  :group 'my-org-gtd
  :package-version '(my-org-gtd . "0.1"))

(defun my-org-gtd--clock-in-open-macos-app (app)
  "Open APP on macOS."
  (shell-command (concat "open -a " app)))

(defun my-org-gtd--clock-in-visit-file (file)
  "Visit FILE and move to the end."
  (find-file file)
  (goto-char (point-max)))

(defun my-org-gtd--clock-in-eval-elisp (code)
  "Evaluate Elisp CODE."
  (eval (read code)))

(defun my-org-gtd--clock-in-actions ()
  "Perform configured actions for the clocked-in task."
  (dolist (action my-org-gtd-clock-in-actions)
    (let* ((property (plist-get action :property))
           (func (plist-get action :action))
           (multi (plist-get action :multi))
           (values (if multi
                       (org-entry-get-multivalued-property (point) property)
                     (list (org-entry-get (point) property)))))
      (dolist (value values)
        (when value
          (funcall func value))))))

(defun my-org-gtd-require-org-clock ()
  "Return user error if no `org' task is currently clocked in."
  (unless (org-clocking-p)
    (user-error "No org task is clocked-in")))

;; URL property support for custom automation

(defun my-org-gtd--org-headline-has-url (headline url)
  "Return the HEADLINE if it has the URL property with the given value."
  (let ((url-property-value (org-element-property :URL headline)))
    (and url-property-value (string= url url-property-value)
         headline)))

(defun my-org-gtd--find-org-node-with-url-property-in-buffer (url)
  "Find an Org node with a given URL property value in the current buffer."
  (org-element-map (org-element-parse-buffer) 'headline
    (lambda (headline)
      (my-org-gtd--org-headline-has-url headline url)) nil t))

(defun my-org-gtd--find-org-node-with-url-property (url)
  "Find the Org node with a given URL property value across `org-agenda-files'."
  (let ((files (org-agenda-files))
        (found nil))
    (while (and files (not found))
      (let* ((file (pop files))
             (buffer (or (find-buffer-visiting file)
                         (find-file-noselect file t)))
             (node (with-current-buffer buffer
                     (my-org-gtd--find-org-node-with-url-property-in-buffer
                      url))))
        (when node
          (setq found (list :buffer buffer :headline node)))))
    found))

(defmacro my-org-gtd-with-org-node-with-url (url &rest body)
  "Go to the `org' node with the URL property value, execute the forms of BODY."
  (declare (indent 1) (debug t))
  `(let ((org-info (my-org-gtd--find-org-node-with-url-property ,url)))
     (when (not org-info)
       (user-error "URL %s not found in `org-agenda-files'" ,url))
     (let* ((org-buffer (plist-get org-info :buffer))
            (org-headline (plist-get org-info :headline))
            (headline-pos (org-element-property :begin org-headline)))
       (with-current-buffer org-buffer
         (goto-char headline-pos)
         ,@body))))

;; FIXME(laurynas): remove mid -org-
(defun my-org-gtd-clock-in-org-node-with-url (url)
  "Go to the `org' node with the given URL property value and clock it in."
  (org-mark-ring-push)
  (my-org-gtd-with-org-node-with-url url
    (goto-char headline-pos)
    (org-clock-in)
    (message "Clocking-in the `org' node with %s, use C-c & to go back" url)))

(defmacro my-org-gtd-with-different-org-clock (&rest body)
  "Save the current org clock, clock-in, execute the forms of BODY.
The marker must be at the new clock position."
  (declare (indent defun) (debug t))
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

;; `org' setup
(defun my-org-gtd--check-keyword-in-org-todo-keywords (keyword)
  "Check that KEYWORD in present in `org-todo-keywords'."
  (unless (seq-some
           (lambda (todo-sequence)
             (seq-some (lambda (keyword-and-char)
                         (when (stringp keyword-and-char)
                           (string= (car (split-string keyword-and-char "("))
                                    keyword)))
                       todo-sequence))
           org-todo-keywords)
    (user-error "'%s' must be present in `org-todo-keywords'" keyword)))

(defun my-org-gtd--make-org-alist-cons-cell (context)
  "Convert a CONTEXT to a cons cell for `org-tag-alist'."
  (cons (my-org-gtd-context-tag context)
        (my-org-gtd-context-select-char context)))

(defun my-org-gtd--require-org-clock (&rest _args)
  "Block the command if no `org' task is clocked in."
  (my-org-gtd-require-org-clock))

(defun my-org-gtd-initialize ()
  "Initialize `my-org-gtd'.
Checks `org-todo-keywords' against keyword configuration, initializes
`org-todo-repeat-to-state', `org-enforce-todo-dependencies', and
`org-stuck-projects'. Adds to `org-use-tag-inheritance', and `org-tag-alist'
from the context configuration and sets up clock-in automation."
  ;; Validate config
  (my-org-gtd--check-keyword-in-org-todo-keywords
   my-org-gtd-next-action-keyword)
  (my-org-gtd--check-keyword-in-org-todo-keywords my-org-gtd-done-keyword)
  (my-org-gtd--check-keyword-in-org-todo-keywords my-org-gtd-cancelled-keyword)
  ;; Configure `org'
  (let ((somedaymaybe-tag (my-org-gtd-context-tag
                           my-org-gtd-somedaymaybe-context)))
    (cond
     ((eq org-use-tag-inheritance t)
      nil)
     ((stringp org-use-tag-inheritance)
      (unless (string-match-p org-use-tag-inheritance somedaymaybe-tag)
        (user-error
         "`my-org-gtd-somedaymaybe-context' tag %s does not match `org-use-tag-inheritance' regex %s"
         somedaymaybe-tag org-use-tag-inheritance)))
     ((listp org-use-tag-inheritance)
      (when (member somedaymaybe-tag org-use-tag-inheritance)
        (user-error
         "`my-org-gtd-somedaymaybe-context' tag %s already in `org-use-tag-inheritance' %S"
         somedaymaybe-tag org-use-tag-inheritance))
      (push somedaymaybe-tag org-use-tag-inheritance))
     (t (user-error "Don't know how handle `org-use-tag-inheritance' value %S"
                    org-use-tag-inheritance))))
  (setq org-todo-repeat-to-state my-org-gtd-next-action-keyword)
  (setq org-enforce-todo-dependencies t)
  (setq org-tag-alist
        (append (list (cons :startgroup nil))
                (mapcar #'my-org-gtd--make-org-alist-cons-cell
                        (append my-org-gtd-contexts
                                (list my-org-gtd-waitingfor-context)))
                (list (cons :endgroup nil))
                (list (my-org-gtd--make-org-alist-cons-cell
                       my-org-gtd-project-context))
                (list (my-org-gtd--make-org-alist-cons-cell
                       my-org-gtd-somedaymaybe-context))
                org-tag-alist))
  (setq org-stuck-projects `(,(concat "+" (my-org-gtd-context-tag
                                           my-org-gtd-project-context)
                                      (my-org-gtd-context-not-tag
                                       my-org-gtd-somedaymaybe-context) "/!"
                                      my-org-gtd-next-action-keyword)
                             (,my-org-gtd-next-action-keyword) nil ""))
  (add-hook 'org-clock-in-hook #'my-org-gtd--clock-in-actions)
  ;; Configure `org-gcal'
  (setq org-gcal-cancelled-todo-keyword my-org-gtd-cancelled-keyword)
  ;; Set up clock gating for commands
  (dolist (cmd my-org-gtd-clock-gated-commands)
    (advice-add cmd :before #'my-org-gtd--require-org-clock)))

;; Agenda views
(defun my-org-gtd--active-todo-search (&rest contexts)
  "Return an `org' search string for next actions in CONTEXTS."
  (let ((not-somedaymaybe
         (my-org-gtd-context-not-tag my-org-gtd-somedaymaybe-context)))
    (concat (mapconcat (lambda (context)
                         (concat (my-org-gtd-context-tag context)
                                 not-somedaymaybe))
                       contexts "|")
            "/!" my-org-gtd-next-action-keyword)))

(defun my-org-gtd-agenda-block (contexts &optional header)
  "Return a `tags-todo' block for CONTEXTS with optional HEADER.
CONTEXTS can be a single context or a list. If HEADER is not provided, take it
from the description of the only context."
  (let* ((single-context-p (and (not (listp contexts))
                                (my-org-gtd-context-p contexts)))
         (contexts-list (if single-context-p (list contexts) contexts))
         (header-string (or header
                            (and single-context-p
                                 (my-org-gtd-context-description contexts)))))
    `(tags-todo
      ,(apply #'my-org-gtd--active-todo-search contexts-list)
      ((org-agenda-overriding-header ,header-string)
       (org-agenda-dim-blocked-tasks 'invisible)))))

(defun my-org-gtd-agenda (context)
  "Return an `org-agenda' command part to show active items from CONTEXT.
TODO(laurynas) example (also to README)."
  (list (my-org-gtd-context-description context) 'tags-todo
        (my-org-gtd--active-todo-search context)))

(defun my-org-gtd-somedaymaybe-agenda ()
  "Return an `org-agenda' command part to show someday/maybe items.
TODO(laurynas) explanation for LEVEL=2."
  (list (my-org-gtd-context-description my-org-gtd-somedaymaybe-context)
        'tags-todo
        (concat (my-org-gtd-context-tag my-org-gtd-somedaymaybe-context)
                "+LEVEL=2")
        '((org-agenda-dim-blocked-tasks nil))))

(defun my-org-gtd-active-non-project-tasks-agenda ()
  "Return an `org-agenda' command part to show active non-project next actions."
  (list "Non-project next actions"
        'tags-todo
        (concat (my-org-gtd-context-not-tag my-org-gtd-project-context)
                (my-org-gtd-context-not-tag my-org-gtd-waitingfor-context)
                (my-org-gtd-context-not-tag my-org-gtd-somedaymaybe-context)
                "/!" my-org-gtd-next-action-keyword)
        `((org-use-tag-inheritance '(,(my-org-gtd-context-tag
                                       my-org-gtd-project-context)
                                     ,(my-org-gtd-context-tag
                                       my-org-gtd-somedaymaybe-context))))))

(defun my-org-gtd-archivable-tasks ()
  "Return an `org-agenda' command part to show archivable non-project tasks."
  (list 'tags
        (concat (my-org-gtd-context-not-tag my-org-gtd-project-context) "/+"
                my-org-gtd-done-keyword "|+" my-org-gtd-cancelled-keyword)
        `((org-agenda-overriding-header "Archivable tasks")
          (org-use-tag-inheritance '(,(my-org-gtd-context-tag
                                       my-org-gtd-project-context))))))

;; FIXME(laurynas): prefix my-org-gtd-agenda- here and everywhere applying
(defun my-org-gtd-contextless-tasks ()
  "Return an `org-agenda' command part to show contextless tasks."
  (list 'todo
        (concat
         (apply #'concat
                (mapcar (lambda (context)
                          (my-org-gtd-context-not-tag context))
                        my-org-gtd-contexts))
         (my-org-gtd-context-not-tag my-org-gtd-waitingfor-context)
         (my-org-gtd-context-not-tag my-org-gtd-project-context)
         (my-org-gtd-context-not-tag my-org-gtd-somedaymaybe-context))
        '((org-agenda-overriding-header "Contextless tasks"))))

;; Creating new tasks and completing them
(defun my-org-gtd--insert-item (title keyword tag)
  "Insert a new `org' item with TITLE, KEYWORD, & TAG at point.
The heading must be already created."
  (when (string-empty-p title)
    (user-error "Title cannot be empty"))
  (insert title)
  (org-todo keyword)
  (org-set-tags tag))

(defun my-org-gtd-insert-project (title)
  "Insert a new project task with TITLE at point.
The heading must be already created."
  (my-org-gtd--insert-item title my-org-gtd-next-action-keyword
                           (my-org-gtd-context-tag
                            my-org-gtd-project-context)))

(defun my-org-gtd-insert-waiting-for-next-action (title)
  "Insert a new next action waiting-for task with TITLE at point.
The heading must be already created."
  (my-org-gtd--insert-item title my-org-gtd-next-action-keyword
                           (my-org-gtd-context-tag
                            my-org-gtd-waitingfor-context)))

(defun my-org-gtd-complete-item ()
  "Mark the item (a task or a project) at point as done."
  (org-todo my-org-gtd-done-keyword))

;; TODO(laurynas): README.org. What constitutes a project?

(provide 'my-org-gtd)
;;; my-org-gtd.el ends here
