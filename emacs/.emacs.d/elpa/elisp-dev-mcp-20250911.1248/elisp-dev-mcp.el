;;; elisp-dev-mcp.el --- MCP server for agentic Elisp development -*- lexical-binding: t -*-

;; Copyright (C) 2025 Laurynas Biveinis

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Author: Laurynas Biveinis
;; Package-Version: 20250911.1248
;; Package-Revision: e20e34ea9f11
;; Package-Requires: ((emacs "27.1") (mcp-server-lib "0.1.0"))
;; Keywords: tools, development
;; URL: https://github.com/laurynas-biveinis/elisp-dev-mcp

;;; Commentary:

;; This package provides an MCP server for agentic Elisp development.

;;; Code:

(require 'mcp-server-lib)
(require 'help-fns)
(require 'pp)
(require 'info-look)


;;; System Directory Setup

(defvar elisp-dev-mcp--system-lisp-dir
  (let* ((data-parent
          (file-name-directory (directory-file-name data-directory)))
         (lisp-dir (expand-file-name "lisp/" data-parent)))
    (when (file-directory-p lisp-dir)
      lisp-dir))
  "System Lisp directory for Emacs installation.
Computed once at package load time from `data-directory'.")


;;; Utility Functions

(defun elisp-dev-mcp--non-empty-docstring-p (doc)
  "Return t if DOC is a non-empty documentation string, nil otherwise."
  (and doc (not (string-empty-p doc))))

;;; JSON Response Helpers

(defun elisp-dev-mcp--json-encode-source-location
    (source file-path start-line end-line)
  "Encode a source location response as JSON.
SOURCE is the source code string.
FILE-PATH is the absolute path to the source file.
START-LINE and END-LINE are 1-based line numbers."
  (json-encode
   `((source . ,source)
     (file-path . ,file-path)
     (start-line . ,start-line)
     (end-line . ,end-line))))

(defun elisp-dev-mcp--json-encode-not-found (symbol message)
  "Encode a not-found response as JSON.
SYMBOL is the symbol that was looked up.
MESSAGE is the error or not-found message."
  (json-encode
   `((found . :json-false) (symbol . ,symbol) (message . ,message))))

(defun elisp-dev-mcp--validate-symbol (name type &optional intern-p)
  "Validate that NAME is a non-empty string suitable for a symbol.
TYPE is a string describing the symbol type for error messages.
If INTERN-P is non-nil, return the interned symbol, otherwise just validate.
Throws an error if validation fails."
  (unless (stringp name)
    (mcp-server-lib-tool-throw (format "Invalid %s name" type)))
  (when (string-empty-p name)
    (mcp-server-lib-tool-throw (format "Empty %s name" type)))
  (when intern-p
    (intern name)))

;;; Property Collection

(defun elisp-dev-mcp--extract-function-properties (sym)
  "Extract all properties for function symbol SYM.
Returns an alist of properties or nil if not a function."
  (when (fboundp sym)
    (let* ((fn (symbol-function sym))
           (is-alias (symbolp fn))
           (aliased-to (and is-alias fn)))
      `((function . ,fn)
        (is-alias . ,is-alias)
        (aliased-to . ,aliased-to)
        (is-subr
         .
         ,(subrp
           (if is-alias
               aliased-to
             fn)))
        (doc . ,(documentation sym))
        (file . ,(find-lisp-object-file-name sym 'defun))))))

(defun elisp-dev-mcp--json-bool (value)
  "Convert Elisp boolean VALUE to JSON boolean representation.
In Elisp, nil is false and everything else is true.
For JSON encoding, returns t for truthy values and :json-false for nil.
This ensures proper JSON boolean serialization."
  (if value
      t
    :json-false))

(defun elisp-dev-mcp--variable-exists-p (props)
  "Check if variable exists based on its PROPS.
A variable exists if it is bound, documented, defined in a file,
is a custom variable, is obsolete, or is an alias."
  (or (alist-get 'bound-p props)
      (alist-get 'doc props)
      (alist-get 'file props)
      (alist-get 'custom-p props)
      (alist-get 'obsolete props)
      (alist-get 'is-alias props)))

;;; Tool Implementations

(defun elisp-dev-mcp--describe-function (function)
  "Get full documentation for Emacs Lisp FUNCTION.

MCP Parameters:
  function - The name of the function to describe"
  (mcp-server-lib-with-error-handling
   (let ((sym (elisp-dev-mcp--validate-symbol function "function" t)))
     (if (fboundp sym)
         (with-temp-buffer
           (let ((standard-output (current-buffer)))
             (describe-function-1 sym)
             (buffer-string)))
       (mcp-server-lib-tool-throw
        (format "Function %s is void" function))))))

;;; Function Definition Helpers

(defun elisp-dev-mcp--process-alias-source
    (source function aliased-to file-path start-line end-line)
  "Post-process SOURCE for function aliases to return a useful defalias form.
If source is just a quoted symbol, replace it with a synthetic defalias form.
Returns a JSON encoded response with enhanced alias information.

FUNCTION is the alias function name.
ALIASED-TO is the target function name.
FILE-PATH, START-LINE, and END-LINE specify source location information."
  (let ((doc (or (documentation (intern-soft function)) "")))
    (if (and source
             (string-match-p (format "['']%s\\>" function) source)
             (not (string-match-p "defalias" source)))
        ;; Generate synthetic defalias form
        (let ((func-def
               (format "(defalias '%s #'%s %S)"
                       function
                       aliased-to
                       doc)))
          (elisp-dev-mcp--json-encode-source-location
           func-def file-path start-line end-line))
      ;; Pass through original source
      (elisp-dev-mcp--json-encode-source-location
       source file-path start-line end-line))))

(defun elisp-dev-mcp--get-function-definition-c-function (fn-name)
  "Return response for C-implemented FN-NAME in get-function-definition."
  (json-encode
   `((is-c-function . t)
     (function-name . ,fn-name)
     (message .
              ,(format
                "Function `%s` is implemented in C source code. \
Use elisp-describe-function tool to get its docstring."
                fn-name)))))

(defun elisp-dev-mcp--extract-function-body (fn has-doc)
  "Extract body from function object FN.
HAS-DOC indicates whether the function has a docstring.
Returns nil if FN is not a function."
  (if (not (functionp fn))
      nil
    (cond
     ;; Emacs 30+ interpreted-function objects
     ((eq (type-of fn) 'interpreted-function)
      ;; Extract body from interpreted-function
      ;; Format: #[args body env bytecode doc]
      (aref fn 1))
     ;; Emacs 29 and earlier cons-based functions
     ((consp fn)
      ;; Function format: (closure ENV ARGS [DOCSTRING] . BODY)
      ;; or: (lambda ARGS [DOCSTRING] . BODY)
      ;; Skip: car (closure/lambda), cadr (env/args), caddr (args/docstring)
      ;; If has docstring, body starts at position 3 (0-indexed)
      ;; If no docstring, body starts at position 2 (0-indexed)
      (nthcdr
       (if has-doc
           3 ; Skip closure/lambda, env/args, and docstring
         2) ; Skip closure/lambda and args only
       fn))
     ;; Fallback for other types
     (t
      (mcp-server-lib-tool-throw
       (format "Don't know how to extract body from function type: %s"
               (type-of fn)))))))

(defun elisp-dev-mcp--reconstruct-function-definition
    (fn-name args doc body)
  "Reconstruct a function definition from its runtime components.
This is used for interactively defined functions where the source file
is not available.  Creates a synthetic defun form.

FN-NAME is the function name as a string.
ARGS is the argument list.
DOC is the documentation string (can be empty).
BODY is the list of body expressions."
  (unless body
    (mcp-server-lib-tool-throw
     (format "Failed to extract body for function %s" fn-name)))
  (let ((defun-form
         `(defun ,(intern fn-name) ,(or args '())
            ,@
            (when (elisp-dev-mcp--non-empty-docstring-p doc)
              (list doc))
            ,@body)))
    (pp-to-string defun-form)))

(defun elisp-dev-mcp--get-function-definition-interactive
    (fn-name sym fn)
  "Handle interactively defined function FN-NAME.
SYM is the function symbol, FN is the function object.
Returns JSON response for an interactively defined function."
  (let* ((args (help-function-arglist sym t))
         (doc (documentation sym))
         (body
          (elisp-dev-mcp--extract-function-body
           fn (elisp-dev-mcp--non-empty-docstring-p doc)))
         (func-def
          (elisp-dev-mcp--reconstruct-function-definition
           fn-name args doc body)))
    (elisp-dev-mcp--json-encode-source-location
     func-def "<interactively defined>" 1 1)))

;;; Variable Helpers

(defun elisp-dev-mcp--find-custom-group (sym)
  "Find the custom group that contain variable SYM.
Returns the group name as a string, or nil if not found."
  (catch 'found
    (mapatoms
     (lambda (group-sym)
       (when (get group-sym 'custom-group)
         (dolist (member (get group-sym 'custom-group))
           (when (and (eq (car member) sym)
                      (eq (cadr member) 'custom-variable))
             (throw 'found (symbol-name group-sym)))))))
    nil))

(defun elisp-dev-mcp--find-header-comment-start (point)
  "Find the start of header comments preceding POINT.
Returns the position of the first comment line, or POINT if no comments found."
  (save-excursion
    (goto-char point)
    (beginning-of-line)
    (forward-line -1)

    ;; Check if there's a header comment
    (if (looking-at "^[ \t]*;;")
        (progn
          ;; Find first line of the consecutive comment block
          (while (and (looking-at "^[ \t]*;;")
                      (> (forward-line -1) -1)))
          ;; We went one line too far back
          (forward-line 1)
          (point))
      point)))

(defun elisp-dev-mcp--extract-source-region (start-point end-point)
  "Extract source code between START-POINT and END-POINT.
Returns a list of (source start-line end-line)."
  (list
   (buffer-substring-no-properties start-point end-point)
   (line-number-at-pos start-point)
   (line-number-at-pos end-point)))

(defun elisp-dev-mcp--extract-variable-properties (sym)
  "Extract all properties for variable symbol SYM.
Returns an alist of properties."
  (let* ((doc (documentation-property sym 'variable-documentation))
         (file (find-lisp-object-file-name sym 'defvar))
         (custom-p (custom-variable-p sym))
         (obsolete (get sym 'byte-obsolete-variable))
         (bound-p (boundp sym))
         (alias-target (indirect-variable sym))
         (is-alias (not (eq sym alias-target)))
         (is-special (special-variable-p sym))
         (custom-group
          (when custom-p
            (elisp-dev-mcp--find-custom-group sym)))
         (custom-type
          (when custom-p
            (get sym 'custom-type))))
    `((doc . ,doc)
      (file . ,file)
      (custom-p . ,custom-p)
      (obsolete . ,obsolete)
      (bound-p . ,bound-p)
      (alias-target . ,alias-target)
      (is-alias . ,is-alias)
      (is-special . ,is-special)
      (custom-group . ,custom-group)
      (custom-type . ,custom-type))))

(defun elisp-dev-mcp--build-variable-json-response (variable props)
  "Build JSON response for VARIABLE using collected PROPS.
VARIABLE is the variable name string, PROPS is an alist of properties."
  (let ((bound-p (alist-get 'bound-p props))
        (doc (alist-get 'doc props))
        (file (alist-get 'file props))
        (custom-p (alist-get 'custom-p props))
        (obsolete (alist-get 'obsolete props))
        (is-alias (alist-get 'is-alias props))
        (alias-target (alist-get 'alias-target props))
        (is-special (alist-get 'is-special props))
        (custom-group (alist-get 'custom-group props))
        (custom-type (alist-get 'custom-type props)))
    (json-encode
     `((name . ,variable)
       (bound . ,(elisp-dev-mcp--json-bool bound-p))
       ,@
       (when bound-p
         `((value-type
            .
            ,(symbol-name
              (type-of (symbol-value (intern variable)))))))
       (documentation . ,doc)
       (source-file . ,(or file "<interactively defined>"))
       (is-custom . ,(elisp-dev-mcp--json-bool custom-p))
       ,@
       (when custom-group
         `((custom-group . ,custom-group)))
       ,@
       (when custom-type
         `((custom-type . ,(format "%S" custom-type))))
       (is-obsolete . ,(elisp-dev-mcp--json-bool obsolete))
       (is-alias . ,(elisp-dev-mcp--json-bool is-alias))
       ,@
       (when obsolete
         `((obsolete-since . ,(nth 2 obsolete))
           (obsolete-replacement . ,(nth 0 obsolete))))
       (is-special . ,(elisp-dev-mcp--json-bool is-special))
       ,@
       (when is-alias
         `((alias-target . ,(symbol-name alias-target))))))))

(defun elisp-dev-mcp--describe-variable (variable)
  "Get information about Emacs Lisp VARIABLE without exposing its value.

MCP Parameters:
  variable - The name of the variable to describe"
  (let* ((sym (elisp-dev-mcp--validate-symbol variable "variable" t))
         (props (elisp-dev-mcp--extract-variable-properties sym)))
    (if (elisp-dev-mcp--variable-exists-p props)
        (elisp-dev-mcp--build-variable-json-response variable props)
      (mcp-server-lib-tool-throw
       (format "Variable %s is not bound" variable)))))

;;; File-based Function Extraction

(defun elisp-dev-mcp--get-function-definition-from-file
    (fn-name sym func-file is-alias aliased-to)
  "Extract function definition for FN-NAME from FUNC-FILE.
SYM is the function symbol.
IS-ALIAS and ALIASED-TO are used for special handling of aliases."
  (with-temp-buffer
    (insert-file-contents func-file)
    (goto-char (point-min))
    (let ((def-pos
           (find-function-search-for-symbol sym nil func-file)))
      (unless def-pos
        (mcp-server-lib-tool-throw
         (format "Could not locate definition for %s" fn-name)))
      (goto-char (cdr def-pos))

      ;; Find the start point including any header comments
      (let* ((func-point (point))
             (start-point
              (elisp-dev-mcp--find-header-comment-start func-point))
             (end-point
              (progn
                (goto-char func-point)
                (forward-sexp)
                (point)))
             (source-info
              (elisp-dev-mcp--extract-source-region
               start-point end-point)))

        ;; Return the result, with special handling for aliases
        (if is-alias
            (elisp-dev-mcp--process-alias-source
             (nth 0 source-info)
             fn-name
             aliased-to
             func-file
             (nth 1 source-info)
             (nth 2 source-info))
          (elisp-dev-mcp--json-encode-source-location
           (nth 0 source-info)
           func-file
           (nth 1 source-info)
           (nth 2 source-info)))))))

(defun elisp-dev-mcp--extract-function-info (sym)
  "Extract function information for symbol SYM.
Returns (fn is-alias aliased-to) or nil if not a function."
  (when (fboundp sym)
    (let* ((fn (symbol-function sym))
           (is-alias (symbolp fn))
           (aliased-to (and is-alias (symbol-name fn))))
      (list fn is-alias aliased-to))))

(defun elisp-dev-mcp--get-function-definition-dispatch
    (function sym fn-info)
  "Dispatch to appropriate handler based on function type.
FUNCTION is the function name string.
SYM is the function symbol.
FN-INFO is the result from `elisp-dev-mcp--extract-function-info`."
  (let ((fn (nth 0 fn-info))
        (is-alias (nth 1 fn-info))
        (aliased-to (nth 2 fn-info)))
    (cond
     ;; C-implemented function
     ((subrp fn)
      (elisp-dev-mcp--get-function-definition-c-function function))

     ;; Has source file
     ((find-lisp-object-file-name sym 'defun)
      (elisp-dev-mcp--get-function-definition-from-file
       function
       sym
       (find-lisp-object-file-name sym 'defun)
       is-alias
       aliased-to))

     ;; Interactive alias
     (is-alias
      (elisp-dev-mcp--process-alias-source
       (format "'%s" function)
       function
       aliased-to
       "<interactively defined>"
       1
       1))

     ;; Interactive function
     (t
      (elisp-dev-mcp--get-function-definition-interactive
       function sym fn)))))

(defun elisp-dev-mcp--get-function-definition (function)
  "Get the source code definition for Emacs Lisp FUNCTION.

MCP Parameters:
  function - The name of the function to retrieve"
  (let* ((sym (elisp-dev-mcp--validate-symbol function "function" t))
         (fn-info (elisp-dev-mcp--extract-function-info sym)))
    (unless fn-info
      (mcp-server-lib-tool-throw
       (format "Function %s is not found" function)))
    (elisp-dev-mcp--get-function-definition-dispatch
     function sym fn-info)))

;;; Info Documentation Helpers

(defun elisp-dev-mcp--extract-info-node-content ()
  "Extract the complete content of the current Info node.
Assumes we're in an Info buffer at the correct node."
  (let ((start nil)
        (end nil))
    ;; Find the start of actual content (after the node header)
    (goto-char (point-min))
    (when (re-search-forward "^File: [^,]+,  Node: [^,\n]+.*\n" nil t)
      (setq start (point)))

    ;; Find the end of content
    (when start
      (goto-char start)
      ;; Look for the next node boundary or end of buffer
      (if (re-search-forward "^\^_" nil t)
          (setq end (match-beginning 0))
        (setq end (point-max))))

    ;; Extract and clean up the content
    (when (and start end)
      (elisp-dev-mcp--clean-info-content
       (buffer-substring-no-properties start end)))))

(defun elisp-dev-mcp--clean-info-content (content)
  "Clean up Info formatting from CONTENT.
Removes navigation markers while preserving documentation structure."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))

    ;; Remove footnote references like (*note ...)
    (while (re-search-forward "\\*[Nn]ote[ \n][^:]*::" nil t)
      (replace-match "[See: \\&]"))

    ;; Return cleaned content
    (buffer-string)))

(defun elisp-dev-mcp--perform-info-lookup (symbol)
  "Perform the actual Info lookup for SYMBOL.
Returns an alist with lookup results or nil if not found."
  (condition-case nil
      (with-temp-buffer
        ;; Set up for info-lookup
        (let ((mode 'emacs-lisp-mode)
              (info-buf nil)
              (node nil)
              (manual nil)
              (content nil))

          ;; info-lookup-symbol needs a buffer in the right mode
          (emacs-lisp-mode)

          ;; Perform the lookup - this will open an Info buffer
          (save-window-excursion
            (info-lookup-symbol symbol mode)

            ;; Get the Info buffer that was opened
            (setq info-buf (get-buffer "*info*"))

            (when info-buf
              (with-current-buffer info-buf
                ;; Extract node information
                (goto-char (point-min))
                (when (re-search-forward
                       "^File: \\([^,]+\\),  Node: \\([^,\n]+\\)"
                       nil t)
                  (setq manual (match-string 1))
                  (setq node (match-string 2))
                  ;; Remove .info extension if present
                  (when (string-match "\\.info\\'" manual)
                    (setq manual
                          (substring manual 0 (match-beginning 0)))))

                ;; Extract content
                (setq content
                      (elisp-dev-mcp--extract-info-node-content)))))

          ;; Return results if we found something
          (when (and node content)
            `((found . t)
              (symbol . ,symbol)
              (node . ,node)
              (manual . ,manual)
              (content . ,content)
              (info-ref . ,(format "(%s)%s" manual node))))))
    ;; If lookup fails, return nil
    (error
     nil)))

(defun elisp-dev-mcp--info-lookup-symbol (symbol)
  "Look up SYMBOL in Elisp Info documentation.

MCP Parameters:
  symbol - The symbol to look up (string)"
  (mcp-server-lib-with-error-handling
   ;; Validate input
   (elisp-dev-mcp--validate-symbol symbol "symbol")
   ;; Perform lookup
   (let ((result (elisp-dev-mcp--perform-info-lookup symbol)))
     (if result
         (json-encode result)
       (elisp-dev-mcp--json-encode-not-found
        symbol
        (format "Symbol '%s' not found in Elisp Info documentation"
                symbol))))))

(defun elisp-dev-mcp--read-source-file (file-path)
  "Read Elisp source file from allowed locations.
Accepts absolute FILE-PATH as returned by other elisp-dev tools.
Handles both .el and .el.gz files transparently.

MCP Parameters:
  file-path - Absolute path to .el file"
  (mcp-server-lib-with-error-handling
   ;; 1. Validate input format
   (unless (and (stringp file-path)
                (file-name-absolute-p file-path)
                (string-suffix-p ".el" file-path))
     (mcp-server-lib-tool-throw
      "Invalid path format: must be absolute path ending in .el"))

   ;; 2. Check for path traversal
   (when (string-match-p "\\.\\." file-path)
     (mcp-server-lib-tool-throw
      "Path contains illegal '..' traversal"))

   ;; 3. Resolve symlinks and validate location
   (let* ((true-path (file-truename file-path))
          ;; Build list of allowed package directories
          (allowed-dirs
           (append
            ;; Current package-user-dir
            (when (boundp 'package-user-dir)
              (list (file-truename (file-name-as-directory package-user-dir))))
            ;; All dirs from package-directory-list
            (mapcar #'file-truename package-directory-list)
            ;; System lisp directory
            (when elisp-dev-mcp--system-lisp-dir
              (list (file-truename elisp-dev-mcp--system-lisp-dir)))))
          ;; Check if file is under any allowed directory
          (allowed-p
           (cl-some (lambda (dir)
                      (and dir (string-prefix-p dir true-path)))
                    allowed-dirs)))

     (unless allowed-p
       (mcp-server-lib-tool-throw
        "Access denied: path outside allowed directories"))

     ;; 4. Find actual file (.el or .el.gz)
     (let ((actual-file
            (cond
             ((file-exists-p true-path)
              true-path)
             ((file-exists-p (concat true-path ".gz"))
              (concat true-path ".gz"))
             (t
              nil))))

       (unless actual-file
         (mcp-server-lib-tool-throw
          (format "File not found: %s (tried .el and .el.gz)"
                  file-path)))

       ;; 5. Read and return contents
       (with-temp-buffer
         (insert-file-contents actual-file)
         (buffer-string))))))

;;;###autoload
(defun elisp-dev-mcp-enable ()
  "Enable the Elisp development MCP tools."
  (mcp-server-lib-register-tool
   #'elisp-dev-mcp--describe-function
   :id "elisp-describe-function"
   :description
   "Get documentation for an Emacs Lisp function or check if it exists. Returns
function documentation from the current running Emacs environment, including all
currently loaded packages and libraries.

Supports:
- Regular functions (defun), macros (defmacro), inline functions (defsubst)
- Function aliases (shows both alias info and target function docs)
- Built-in C functions (subr)
- Byte-compiled functions
- Functions with or without documentation

Returns formatted documentation including:
- Function signature with argument names
- Full docstring with parameter descriptions
- Source file location
- Function type (closure, macro, subr, etc.)

Error cases:
- Non-existent functions return 'Function X is void'
- Invalid input types return 'Error: ...'"
   :read-only t)
  (mcp-server-lib-register-tool
   #'elisp-dev-mcp--get-function-definition
   :id "elisp-get-function-definition"
   :description
   "Get the source code definition of an Emacs Lisp function with any header
comments. Returns source code with file path and 1-based line numbers. For
functions defined in C, returns a suggestion to call elisp-describe-function
tool instead.

Returns JSON with:
- source: Complete function definition including header comments
- file-path: Absolute path to source file or '<interactively defined>'
- start-line: Line number where definition starts (1-based)
- end-line: Line number where definition ends

Special handling:
- Function aliases: Returns the defalias form with docstring
- C functions: Returns is-c-function=true with suggestion message
- Interactive functions: Reconstructs defun from runtime representation
- Byte-compiled functions: Retrieves original source if available

Error cases:
- Non-existent functions return 'Function X is not found'
- Non-string input returns 'Invalid function name'

Use this tool when you need to:
- View or analyze function implementation
- Extract function source for modification
- Understand function structure with comments"
   :read-only t)
  (mcp-server-lib-register-tool
   #'elisp-dev-mcp--describe-variable
   :id "elisp-describe-variable"
   :description
   "Get comprehensive information about an Emacs Lisp variable without
exposing its value. Essential for understanding variable definitions,
types, and relationships in Elisp code.

Parameters:
  variable - Variable name as a string (e.g., \"load-path\", \"custom-file\")

Returns JSON object with these fields:
  name - Variable name (string, always present)
  bound - Whether variable has a value (boolean, always present)
  value-type - Type of the current value like \"string\", \"cons\", \"integer\",
               \"symbol\" (string, only when bound is true)
  documentation - Variable's docstring (string or null, always present)
  source-file - File where defined, or \"<interactively defined>\"
                (string, always present)
  is-custom - Whether it's a defcustom variable (boolean, always present)
  custom-group - Which customization group it belongs to
                 (string, only when is-custom is true)
  custom-type - Type specification for customization like \"string\" or
                complex types (string, only when is-custom is true)
  is-obsolete - Whether marked as obsolete (boolean, always present)
  obsolete-since - Version when obsoleted
                   (string, only when is-obsolete is true)
  obsolete-replacement - Suggested replacement
                         (string, only when is-obsolete is true)
  is-alias - Whether this is an alias to another variable
             (boolean, always present)
  alias-target - The actual variable this aliases to
                 (string, only when is-alias is true)
  is-special - Whether it's a special/dynamic variable in lexical-binding
               context (boolean, always present)

Common use cases:
- Check if a configuration variable exists before using it
- Understand variable relationships (aliases, obsolescence)
- Verify variable types before setting values
- Find documentation for Emacs configuration options
- Discover which customization group a setting belongs to

Security: Never exposes actual values to prevent leaking sensitive data
like API keys, passwords, or personal information. Use this instead of
eval when exploring variables.

Error cases return error messages for:
- Non-string input
- Completely undefined variables (no binding, no documentation, no properties)"
   :read-only t)
  (mcp-server-lib-register-tool
   #'elisp-dev-mcp--info-lookup-symbol
   :id "elisp-info-lookup-symbol"
   :description
   "Look up Elisp symbols in Info documentation and return the complete
documentation node. Returns the full content of the Info node containing
the symbol's documentation from the Emacs Lisp Reference Manual.

Parameters:
  symbol - The Elisp symbol to look up (string)

Returns JSON with:
  found - Whether documentation was found (boolean)
  symbol - The symbol that was looked up (string)
  node - The Info node name containing the documentation (string, when found)
  manual - The Info manual name, typically 'elisp' (string, when found)
  content - The complete Info node content including all examples,
            cross-references, and related information (string, when found)
  info-ref - Info reference like '(elisp)Node Name' for direct access
             (string, when found)
  message - Error or not-found message (string, when not found)

The content field contains the entire Info node, ensuring you have full
context including:
- Complete function/variable descriptions
- All code examples and usage patterns
- Cross-references to related concepts
- Any warnings, notes, or special considerations

Common symbols that can be looked up:
- Special forms: defun, defvar, let, if, cond, lambda
- Functions: mapcar, apply, funcall, concat
- Macros: when, unless, dolist, defmacro
- Variables: load-path, emacs-version
- Concepts: 'lexical binding', 'dynamic binding'

Error cases:
- Symbol not found in documentation
- Invalid symbol name
- Info system unavailable"
   :read-only t)
  (mcp-server-lib-register-tool
   #'elisp-dev-mcp--read-source-file
   :id "elisp-read-source-file"
   :description
   "Read Elisp source files from Emacs system directories or ELPA packages.
Designed to work with paths returned by other elisp-dev tools.

Parameters:
  file-path - Absolute path to .el file (string)

Accepts paths like:
- System Elisp files (from Emacs installation)
- ELPA package files (from user-emacs-directory)

Security:
- Only reads from Emacs system lisp directories and ELPA directory
- Rejects paths with \"..\" traversal
- Resolves symlinks to prevent escaping allowed directories

Features:
- Transparently handles .el.gz compressed files
- Works directly with paths from elisp-get-function-definition
- Returns complete file contents as string

Error cases:
- Invalid path format
- Path traversal attempts
- Access outside allowed directories
- File not found"
   :read-only t))

;;;###autoload
(defun elisp-dev-mcp-disable ()
  "Disable the Elisp development MCP tools."
  (mcp-server-lib-unregister-tool "elisp-describe-function")
  (mcp-server-lib-unregister-tool "elisp-get-function-definition")
  (mcp-server-lib-unregister-tool "elisp-describe-variable")
  (mcp-server-lib-unregister-tool "elisp-info-lookup-symbol")
  (mcp-server-lib-unregister-tool "elisp-read-source-file"))

(provide 'elisp-dev-mcp)
;;; elisp-dev-mcp.el ends here
