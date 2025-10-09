;;; mcp-server-lib-commands.el --- User commands for MCP server -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Laurynas Biveinis

;; This file is part of mcp-server-lib.el.

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

;;; Commentary:

;; This file provides interactive user commands for the MCP server library.

;;; Code:

(require 'mcp-server-lib)
(require 'mcp-server-lib-metrics)

;;;###autoload
(defun mcp-server-lib-start ()
  "Start the MCP server and begin handling client requests.

This function starts the MCP server that can process JSON-RPC
requests via `mcp-server-lib-process-jsonrpc'.  Once started, the server
will dispatch incoming requests to the appropriate tool
handlers that have been registered with `mcp-server-lib-register-tool'.

Resets all metrics when starting.

See also: `mcp-server-lib-stop'"
  (interactive)
  (when mcp-server-lib--running
    (error "MCP server is already running"))

  (clrhash mcp-server-lib-metrics--table)
  (when (called-interactively-p 'any)
    (message "Emacs starting handling MCP requests"))
  (setq mcp-server-lib--running t))

;;;###autoload
(defun mcp-server-lib-stop ()
  "Stop the MCP server from processing client requests.

Sets the server state to stopped, which prevents further processing of
client requests.  Note that this does not release any resources or unregister
tools, it simply prevents `mcp-server-lib-process-jsonrpc' from accepting new
requests.

See also: `mcp-server-lib-start'"
  (interactive)
  (unless mcp-server-lib--running
    (error "MCP server is not running"))

  (when (called-interactively-p 'any)
    (message "Emacs stopping handling MCP requests"))
  ;; Mark server as not running
  (setq mcp-server-lib--running nil)
  ;; Show metrics summary if there are any
  (when (> (hash-table-count mcp-server-lib-metrics--table) 0)
    (message "%s" (mcp-server-lib-metrics-summary)))
  t)

;;; Script Installation

(defun mcp-server-lib--package-script-path ()
  "Return the path to emacs-mcp-stdio.sh in the package directory.
Returns nil if not found."
  (let* ((library-path (locate-library "mcp-server-lib"))
         (package-dir
          (and library-path (file-name-directory library-path)))
         (script-path
          (and package-dir
               (expand-file-name "emacs-mcp-stdio.sh" package-dir))))
    (when (and script-path (file-exists-p script-path))
      script-path)))

(defun mcp-server-lib--installed-script-path ()
  "Return the path where the script should be installed."
  (expand-file-name "emacs-mcp-stdio.sh"
                    mcp-server-lib-install-directory))

;;;###autoload
(defun mcp-server-lib-install ()
  "Install emacs-mcp-stdio.sh to `mcp-server-lib-install-directory'."
  (interactive)
  (let ((source (mcp-server-lib--package-script-path))
        (target (mcp-server-lib--installed-script-path)))
    (unless source
      (error "Cannot find emacs-mcp-stdio.sh in package directory"))
    (when (file-exists-p target)
      (unless (yes-or-no-p
               (format "File already exists at %s. Overwrite? "
                       target))
        (user-error "Installation cancelled")))
    (make-directory (file-name-directory target) t)
    (copy-file source target t)
    (set-file-modes target #o755)
    (message "Script installed to: %s" target)))

;;;###autoload
(defun mcp-server-lib-uninstall ()
  "Remove installed emacs-mcp-stdio.sh from `mcp-server-lib-install-directory'."
  (interactive)
  (let ((target (mcp-server-lib--installed-script-path)))
    (unless (file-exists-p target)
      (user-error "No script found at: %s" target))
    (when (yes-or-no-p (format "Remove script at %s? " target))
      (delete-file target)
      (message "Script removed from: %s" target))))

;;; Metrics commands

;;;###autoload
(defun mcp-server-lib-reset-metrics ()
  "Reset all metrics."
  (interactive)
  (clrhash mcp-server-lib-metrics--table)
  (message "MCP metrics reset"))

(defun mcp-server-lib--get-handler-name (handler)
  "Get a human-readable name for HANDLER function.
Returns the function name for symbols, \"lambda\" for lambdas,
\"closure\" for closures, \"compiled-function\" for byte-code, or
\"unknown\" for unrecognized types."
  (cond
   ((symbolp handler)
    (symbol-name handler))
   ((byte-code-function-p handler)
    (let ((name (aref handler 2)))
      (if (symbolp name)
          (symbol-name name)
        "compiled-function")))
   ;; In Emacs 30.1+, interpreted lambdas with lexical binding are
   ;; represented as interpreted-function objects, not lists
   ((and (fboundp 'interpreted-function-p)
         (interpreted-function-p handler))
    "closure")
   ;; In Emacs 30.1+, closurep covers both interpreted and compiled closures
   ((and (fboundp 'closurep) (closurep handler))
    "closure")
   ;; For older Emacs versions, check list-based representations
   ((and (listp handler) (eq (car handler) 'lambda))
    "lambda")
   ((and (listp handler) (eq (car handler) 'closure))
    "closure")
   (t
    "unknown")))

(defun mcp-server-lib--insert-usage-metrics (metrics &optional indent)
  "Insert usage statistics for METRICS into the current buffer.
METRICS should be a mcp-server-lib-metrics struct or nil.
INDENT is an optional string prepended to the line (defaults to \"  \").
If METRICS is provided, inserts the call count and error count.
If METRICS is nil, inserts \"0 calls\" as the usage."
  (let ((prefix (or indent "  ")))
    (if metrics
        (let ((calls (mcp-server-lib-metrics-calls metrics))
              (errors (mcp-server-lib-metrics-errors metrics)))
          (insert
           (format "%s  Usage: %d calls, %d errors\n" prefix calls errors)))
      (insert (format "%s  Usage: 0 calls\n" prefix)))))

(defun mcp-server-lib--insert-item-properties
    (name description &optional extra-props indent)
  "Insert item properties NAME, DESCRIPTION, and EXTRA-PROPS.
NAME is always inserted (required property).
DESCRIPTION is only inserted if non-nil (optional property).
EXTRA-PROPS is an optional plist of additional properties to display.
INDENT is an optional string prepended to each line (defaults to \"  \").
Properties in EXTRA-PROPS with nil values are skipped."
  (let ((prefix (or indent "  ")))
    (insert (format "%s  Name: %s\n" prefix name))
    (when description
      (insert (format "%s  Description: %s\n" prefix description)))
    (let ((props extra-props))
      (while props
        (let ((key (car props))
              (value (cadr props)))
          (when value
            (insert
             (format "%s  %s: %s\n" prefix
                     (capitalize (substring (symbol-name key) 1))
                     value)))
          (setq props (cddr props)))))))

(defun mcp-server-lib--entry-key-lessp (a b)
  "Compare alist entries A and B by their keys alphabetically.
Returns t if the key of A is lexicographically less than the key of B."
  (string< (car a) (car b)))

(defun mcp-server-lib--hash-table-to-sorted-alist (hash-table)
  "Convert HASH-TABLE to a sorted alist of (KEY . VALUE) pairs.
Returns nil if HASH-TABLE is nil.
The returned alist is sorted alphabetically by key."
  (when hash-table
    (let ((alist nil))
      (maphash
       (lambda (key value) (push (cons key value) alist)) hash-table)
      (sort alist #'mcp-server-lib--entry-key-lessp))))

(defmacro mcp-server-lib--with-hash-table-entries
    (hash-table header &rest body)
  "Iterate over HASH-TABLE entries in sorted order.
Prints HEADER before iterating if the table is not empty.
Executes BODY for each entry with `entry' bound to (KEY . VALUE) cons cell.
Entries are processed in alphabetical order by key.
Does nothing if HASH-TABLE is nil or empty."
  (declare (indent 2) (debug (form form body)))
  `(when (and ,hash-table (> (hash-table-count ,hash-table) 0))
     (insert ,header)
     (dolist (entry
              (mcp-server-lib--hash-table-to-sorted-alist
               ,hash-table))
       ,@body)))

(defun mcp-server-lib--format-metrics-with-errors
    (key metrics &optional indent)
  "Format metrics KEY with METRICS including error rate.
Optional INDENT adds spaces before the key."
  (let* ((total (mcp-server-lib-metrics-calls metrics))
         (errors (mcp-server-lib-metrics-errors metrics))
         (rate (mcp-server-lib-metrics--error-rate total errors))
         (formatted-key
          (if indent
              (format "  %-38s" key)
            (format "%-40s" key))))
    (format "%s %6d %7d %9.1f%%\n" formatted-key total errors rate)))

;;;###autoload
(defun mcp-server-lib-describe-setup ()
  "Show the current MCP server setup including registered tools and resources."
  (interactive)
  (with-current-buffer (get-buffer-create "*MCP Server Setup*")
    (erase-buffer)
    (insert "MCP Server Setup\n\n")
    (insert
     (format "Status: %s\n\n"
             (if mcp-server-lib--running
                 "Running"
               "Stopped")))
    ;; Only show Tools section if there are actual tools
    (when (and mcp-server-lib--tools
               (> (hash-table-count mcp-server-lib--tools) 0)
               (cl-some (lambda (entry)
                          (let ((tools-table (cdr entry)))
                            (and tools-table
                                 (> (hash-table-count tools-table) 0))))
                        (mcp-server-lib--hash-table-to-sorted-alist
                         mcp-server-lib--tools)))
      (insert "Tools:\n"))
    (mcp-server-lib--with-hash-table-entries mcp-server-lib--tools
        ""
      (let* ((server-id (car entry))
             (tools-table (cdr entry))
             (multi-server (> (hash-table-count mcp-server-lib--tools) 1)))
        (when multi-server
          (insert (format "  Server: %s\n" server-id)))
        (mcp-server-lib--with-hash-table-entries tools-table
            ""
          (let* ((id (car entry))
                 (tool (cdr entry))
                 (description (plist-get tool :description))
                 (handler (plist-get tool :handler))
                 (handler-name (mcp-server-lib--get-handler-name handler))
                 (metrics-key (format "tools/call:%s" id))
                 (metrics
                  (gethash metrics-key mcp-server-lib-metrics--table))
                 (indent (if multi-server "    " "  ")))
            (insert (format "%s%s\n" indent id))
            (insert (format "%s  Description: %s\n" indent description))
            (when (plist-member tool :title)
              (insert (format "%s  Title: %s\n" indent (plist-get tool :title))))
            (when (plist-member tool :read-only)
              (insert
               (format "%s  Read-only: %s\n" indent
                       (plist-get tool :read-only))))
            (insert (format "%s  Handler: %s\n" indent handler-name))
            (mcp-server-lib--insert-usage-metrics metrics indent)
            (insert "\n")))))
    ;; Only show Resources section if there are actual resources
    (when (and mcp-server-lib--resources
               (> (hash-table-count mcp-server-lib--resources) 0)
               (cl-some (lambda (entry)
                          (let ((resources-table (cdr entry)))
                            (and resources-table
                                 (> (hash-table-count resources-table) 0))))
                        (mcp-server-lib--hash-table-to-sorted-alist
                         mcp-server-lib--resources)))
      (insert "\nResources:\n"))
    (mcp-server-lib--with-hash-table-entries mcp-server-lib--resources
        ""
      (let* ((server-id (car entry))
             (resources-table (cdr entry))
             (multi-server (> (hash-table-count mcp-server-lib--resources) 1)))
        (when multi-server
          (insert (format "  Server: %s\n" server-id)))
        (mcp-server-lib--with-hash-table-entries resources-table
            ""
          (let* ((uri (car entry))
                 (resource (cdr entry))
                 (name (plist-get resource :name))
                 (description (plist-get resource :description))
                 (mime-type
                  (or (plist-get resource :mime-type) "text/plain"))
                 (handler (plist-get resource :handler))
                 (handler-name (mcp-server-lib--get-handler-name handler))
                 (metrics-key (format "resources/read:%s" uri))
                 (metrics
                  (gethash metrics-key mcp-server-lib-metrics--table))
                 (indent (if multi-server "    " "  ")))
            (insert (format "%s%s\n" indent uri))
            (mcp-server-lib--insert-item-properties
             name description
             (list :mime-type mime-type)
             indent)
            (insert (format "%s  Handler: %s\n" indent handler-name))
            (mcp-server-lib--insert-usage-metrics metrics indent)
            (insert "\n")))))
    ;; Only show resource templates if there are any
    (when (and mcp-server-lib--resource-templates
               (> (hash-table-count mcp-server-lib--resource-templates) 0)
               (cl-some (lambda (entry)
                          (let ((templates-table (cdr entry)))
                            (and templates-table
                                 (> (hash-table-count templates-table) 0))))
                        (mcp-server-lib--hash-table-to-sorted-alist
                         mcp-server-lib--resource-templates)))
      (insert "\nResource Templates:\n")
      (mcp-server-lib--with-hash-table-entries
          mcp-server-lib--resource-templates
          ""
        (let* ((server-id (car entry))
               (templates-table (cdr entry))
               (multi-server (> (hash-table-count mcp-server-lib--resource-templates) 1)))
          (when multi-server
            (insert (format "  Server: %s\n" server-id)))
          (mcp-server-lib--with-hash-table-entries templates-table
              ""
            (let* ((uri (car entry))
                   (template (cdr entry))
                   (name (plist-get template :name))
                   (description (plist-get template :description))
                   (mime-type
                    (or (plist-get template :mime-type) "text/plain"))
                   (handler (plist-get template :handler))
                   (handler-name
                    (mcp-server-lib--get-handler-name handler))
                   (indent (if multi-server "    " "  ")))
              (insert (format "%s%s\n" indent uri))
              (mcp-server-lib--insert-item-properties
               name description
               (list :mime-type mime-type)
               indent)
              (insert (format "%s  Handler: %s\n" indent handler-name))
              (insert "\n"))))))
    (display-buffer (current-buffer))))

;;;###autoload
(defun mcp-server-lib-show-metrics ()
  "Display metrics in a buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*MCP Metrics*")
    (erase-buffer)
    (insert "MCP Usage Metrics\n")
    (insert "=================\n\n")
    (insert (format-time-string "Session started: %F %T\n\n"))

    ;; Separate into three categories
    (let ((method-metrics nil)
          (notification-metrics nil)
          (tool-metrics nil))
      (maphash
       (lambda (key metrics)
         (cond
          ((string-match-p ":" key)
           (push (cons key metrics) tool-metrics))
          ((string-prefix-p "notifications/" key)
           (push (cons key metrics) notification-metrics))
          (t
           (push (cons key metrics) method-metrics))))
       mcp-server-lib-metrics--table)

      ;; Display method-level metrics
      (when method-metrics
        (insert "Method Calls:\n")
        (insert
         "---------------------------------------- ------ ------- ----------\n")
        (dolist (entry
                 (sort
                  method-metrics #'mcp-server-lib--entry-key-lessp))
          (insert
           (mcp-server-lib--format-metrics-with-errors
            (car entry) (cdr entry))))
        (insert "\n"))

      ;; Display notifications
      (when notification-metrics
        (insert "Notifications:\n")
        (insert "---------------------------------------- ------\n")
        (dolist (entry
                 (sort
                  notification-metrics
                  #'mcp-server-lib--entry-key-lessp))
          (let* ((key (car entry))
                 (metrics (cdr entry))
                 (total (mcp-server-lib-metrics-calls metrics)))
            ;; Notifications don't have errors, so simpler display
            (insert (format "%-40s %6d\n" key total))))
        (insert "\n"))

      ;; Display tool-specific metrics
      (when tool-metrics
        (insert "Tool Usage:\n")
        (insert
         "---------------------------------------- ------ ------- ----------\n")
        (dolist (entry
                 (sort
                  tool-metrics #'mcp-server-lib--entry-key-lessp))
          (let* ((key (car entry))
                 (metrics (cdr entry))
                 (display-name
                  (if (string-match "tools/call:\\(.*\\)" key)
                      (match-string 1 key)
                    key)))
            (insert
             (mcp-server-lib--format-metrics-with-errors
              display-name metrics
              t)))))

      ;; Summary with totals
      (let ((method-total 0)
            (method-errors 0)
            (tool-total 0)
            (tool-errors 0))
        ;; Calculate totals
        (dolist (entry method-metrics)
          (let ((metrics (cdr entry)))
            (cl-incf
             method-total (mcp-server-lib-metrics-calls metrics))
            (cl-incf
             method-errors (mcp-server-lib-metrics-errors metrics))))
        (dolist (entry tool-metrics)
          (let ((metrics (cdr entry)))
            (cl-incf
             tool-total (mcp-server-lib-metrics-calls metrics))
            (cl-incf
             tool-errors (mcp-server-lib-metrics-errors metrics))))

        ;; Display summary
        (insert "\nSummary:\n")
        (insert "--------\n")
        (insert
         (format "Methods: %d calls, %d errors (%.1f%%)\n"
                 method-total method-errors
                 (mcp-server-lib-metrics--error-rate
                  method-total method-errors)))
        (insert
         (format "Tools: %d calls, %d errors (%.1f%%)\n"
                 tool-total tool-errors
                 (mcp-server-lib-metrics--error-rate
                  tool-total tool-errors))))))

  (display-buffer (current-buffer)))

(provide 'mcp-server-lib-commands)

;; Local Variables:
;; byte-compile-warnings: (not unresolved)
;; package-lint-main-file: "mcp-server-lib.el"
;; End:

;;; mcp-server-lib-commands.el ends here
