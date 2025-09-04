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
                  method-metrics
                  (lambda (a b) (string< (car a) (car b)))))
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
                  (lambda (a b) (string< (car a) (car b)))))
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
                  tool-metrics
                  (lambda (a b) (string< (car a) (car b)))))
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
