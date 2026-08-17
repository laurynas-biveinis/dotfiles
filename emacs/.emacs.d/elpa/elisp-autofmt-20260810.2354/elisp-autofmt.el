;;; elisp-autofmt.el --- Emacs lisp auto-format -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2019-2022  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-elisp-autofmt
;; Package-Version: 20260810.2354
;; Package-Revision: be5c48683cef
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Auto format emacs-lisp code on save.

;;; Usage:

;; (elisp-autofmt-buffer) ; Auto-format the current buffer.
;;
;; You may also use the minor mode `elisp-autofmt-mode' which enables
;; formatting the buffer on save.

;;; Code:


;; ---------------------------------------------------------------------------
;; Compatibility

(eval-when-compile
  (when (version< emacs-version "31.1")
    (defmacro incf (place &optional delta)
      "Increment PLACE by DELTA or 1."
      (declare (debug (gv-place &optional form)))
      (gv-letplace (getter setter) place (funcall setter `(+ ,getter ,(or delta 1)))))
    (defmacro decf (place &optional delta)
      "Decrement PLACE by DELTA or 1."
      (declare (debug (gv-place &optional form)))
      (gv-letplace (getter setter) place (funcall setter `(- ,getter ,(or delta 1)))))))


;; ---------------------------------------------------------------------------
;; Public Custom Variables

(defgroup elisp-autofmt nil
  "Configure emacs-lisp auto-formatting behavior."
  :group 'tools)

;; Customization (Style).

(defcustom elisp-autofmt-style 'native
  "The formatting style to use."
  :type
  '(choice (const :tag "Native (Emacs indentation)" native)
           (const :tag "Fixed (Fixed indentation)" fixed)))
;;;###autoload
(put 'elisp-autofmt-style 'safe-local-variable #'symbolp)

(defcustom elisp-autofmt-format-quoted t
  "Format single-quoted S-expressions.
Otherwise existing line-breaks are kept and only indentation is performed."
  :type 'boolean)
;;;###autoload
(put 'elisp-autofmt-format-quoted 'safe-local-variable #'booleanp)

(defcustom elisp-autofmt-empty-line-max 2
  "The maximum number of blank lines to preserve."
  :type 'natnum)
;;;###autoload
(put 'elisp-autofmt-empty-line-max 'safe-local-variable #'integerp)

;; Customization (API Definitions).

(defcustom elisp-autofmt-use-function-defs t
  "When non-nil, generate function definitions for the auto-formatter to use."
  :type 'boolean)


(defcustom elisp-autofmt-use-default-override-defs t
  "When non-nil, make opinionated changes to how line breaks are handled."
  :type 'boolean)

(defcustom elisp-autofmt-load-packages-local nil
  "Additional packages/modules to include definitions from.

Each entry is a string which may be:
- A package name (e.g. \"pcase\"), taking the definitions from this Emacs.
  Formatting never loads the package, a package this session has not
  loaded contributes nothing and is reported.
- A buffer relative path (beginning with a \".\"),
  which is intended to support sharing definitions for multi-file packages.

This is intended to be set from file or directory locals and is marked safe."
  :type '(repeat string)
  :local t)
;;;###autoload
(put 'elisp-autofmt-load-packages-local 'safe-local-variable #'list-of-strings-p)

(defcustom elisp-autofmt-ignore-autoload-packages
  (list
   "babel"
   "gnus-fun"
   "gnus-xmas"
   "mailcrypt"
   "mc-toplev"
   "message"
   "messagexmas"
   "mh-tool-bar"
   "nnimap"
   "vcard")
  "Exclude these packages from inclusion in API definition lists.
Note that this should not need to be modified for typical use cases."
  :type '(repeat string))

;; Customization (Integration).

(defcustom elisp-autofmt-on-save-p 'elisp-autofmt-check-elisp-autofmt-exists
  "Only reformat on save if this function returns non-nil.

You may wish to choose one of the following options:
- `always': To always format on save.
- `elisp-autofmt-check-elisp-autofmt-exists':
  Only reformat when \".elisp-autofmt\" exists.

Otherwise you can set this to a user defined function."
  :type 'function)

(defcustom elisp-autofmt-python-bin nil
  "The Python binary to call to run the auto-formatting utility.

When nil, the default Python command is used."
  :type '(choice (const nil) string))

(defcustom elisp-autofmt-cache-directory
  (locate-user-emacs-file "elisp-autofmt-cache" ".elisp-autofmt-cache")
  "The directory to store cache data."
  :type 'directory)

(defcustom elisp-autofmt-use-diff-range nil
  "For whole buffer formatting, compute the changed region & only update that.

Note that this may be useful for systems where the sub-process overhead is significant."
  :type 'boolean)

;; Customization (Parallel Computation).

(defcustom elisp-autofmt-parallel-jobs 0
  "The number of jobs to run in parallel.

- Use 0 to select automatically.
- Use -1 to disable parallel computation entirely."
  :type 'integer)

(defcustom elisp-autofmt-parallel-threshold 32768
  "Buffers under this size will not use parallel computation.

- Use 0 to enable parallel computation for buffers of any size."
  :type 'natnum)


;; ---------------------------------------------------------------------------
;; Public Variables

(defvar elisp-autofmt-debug-extra-info nil
  "Show additional debug information.")

(defvar elisp-autofmt-debug-mode nil
  "Enable additional checks when formatting (enabled for tests).")


;; ---------------------------------------------------------------------------
;; Internal Variables

;; Run this command to format.
(defconst elisp-autofmt--this-file load-file-name)
(defconst elisp-autofmt--base (file-name-sans-extension elisp-autofmt--this-file))
(defconst elisp-autofmt--bin (concat elisp-autofmt--base ".py"))

;; Include these in the default emacs-binary API list.
;; Only use this for:
;; - Common packages so users don't have to manually list them.
;; - Packages that are not loaded by default.
(defconst elisp-autofmt--packages-default
  (list
   ;; For `pcase' & `pcase-let'.
   'pcase))

;; WIN32 hangs using `make-process'. Use `call-process' instead at the cost
;; of having to use a temporary file for the `stderr'.
(defconst elisp-autofmt--workaround-make-proc (memq system-type (list 'ms-dos 'windows-nt)))

;; Force process IO to use UTF8, see: #15.
(defconst elisp-autofmt--process-coding-system (cons 'utf-8 'utf-8))


;; ---------------------------------------------------------------------------
;; Internal Utilities

(defun elisp-autofmt--python-commands-or-empty ()
  "Return the Python command or an empty list.

An empty list means the script will be executed directly,
useful for systems that patch the SHEBANG for a custom Python location."
  (cond
   ((null elisp-autofmt-python-bin)
    (cond
     ((memq system-type (list 'ms-dos 'windows-nt))
      ;; Use "python", from the PATH.
      (list "python"))
     (t
      ;; Execute the script directly.
      (list))))
   (t
    (list elisp-autofmt-python-bin))))

(defun elisp-autofmt--python-env-prepend (env)
  "Return a new environment prepended to ENV."
  (cond
   (elisp-autofmt-debug-mode
    env)
   (t
    (cons "PYTHONOPTIMIZE=2" env))))

(defvar-local elisp-autofmt--message-once-table nil
  "Text shown by this buffer's run, see `elisp-autofmt--message-once'.")

(defvar-local elisp-autofmt--message-once-table-prev nil
  "Text shown by this buffer's previous run, see `elisp-autofmt--message-once'.")

(defun elisp-autofmt--message-once-begin ()
  "Begin a run for `elisp-autofmt--message-once'."
  (declare (important-return-value nil))
  (setq elisp-autofmt--message-once-table-prev elisp-autofmt--message-once-table)
  (setq elisp-autofmt--message-once-table (make-hash-table :test #'equal)))

(defun elisp-autofmt--message-once (format-string &rest args)
  "Show FORMAT-STRING with ARGS unless the previous run showed the same text.

The definitions are checked on every format, so a condition that stays
put (a mistyped path, a package this Emacs never loads) reports itself on
every save otherwise.

Only the previous run is remembered on purpose. Never forgetting means a
failure which is fixed and later returns stays hidden for the rest of the
session, silently formatting without those definitions.

Both tables are buffer local: one global pair let any save of another
buffer, which does not emit this buffer's diagnostics, clear the memory
between two saves of this one - bringing the repeats back in any
multi-buffer session."
  (declare (important-return-value nil))
  (let ((text (apply #'format format-string args)))
    (when elisp-autofmt--message-once-table
      (puthash text t elisp-autofmt--message-once-table))
    (unless (and elisp-autofmt--message-once-table-prev
                 (gethash text elisp-autofmt--message-once-table-prev))
      (message "%s" text))))

(defmacro elisp-autofmt--with-advice (advice &rest body)
  "Execute BODY with ADVICE temporarily enabled.

Each advice is a triplet of (SYMBOL HOW FUNCTION),
see `advice-add' documentation."
  (declare (indent 1))
  (let ((advice-list advice)
        (body-let nil)
        (body-advice-add nil)
        (body-advice-remove nil)
        (item nil))
    (unless (listp advice-list)
      (error "Advice must be a list"))
    (cond
     ((null advice-list)
      (macroexp-warn-and-return
       "An empty advice argument was found"
       `(progn
          ,@body)))
     (t
      (while (setq item (pop advice-list))
        (unless (and (listp item) (eq 3 (length item)))
          (error "Each advice must be a list of 3 items"))
        (let ((fn-sym (gensym))
              (fn-advise (pop item))
              (fn-advice-ty (pop item))
              (fn-body (pop item)))
          ;; Build the calls for each type.
          (push (list fn-sym fn-body) body-let)
          (push (list 'advice-add fn-advise fn-advice-ty fn-sym) body-advice-add)
          (push (list 'advice-remove fn-advise fn-sym) body-advice-remove)))
      (setq body-let (nreverse body-let))
      (setq body-advice-add (nreverse body-advice-add))

      ;; Compose the call.
      `(let ,body-let
         (unwind-protect
             (progn
               ,@body-advice-add
               ,@body)
           ,@body-advice-remove))))))

(defmacro elisp-autofmt--with-temp-file (name &rest body)
  "Bind NAME to the name of a new temporary file and evaluate BODY.
Delete the temporary file after BODY exits normally or non-locally.
NAME will be bound to the file name of the temporary file.

The following keyword arguments are supported:

:prefix STRING
  If non-nil, pass STRING to `make-temp-file' as the PREFIX argument.

:suffix STRING
  If non-nil, pass STRING to `make-temp-file' as the SUFFIX argument."
  (declare (indent 1) (debug (symbolp body)))
  (unless (symbolp name)
    (error "Expected name to be as symbol, found %S" (type-of name)))
  (let ((keyw nil)
        (prefix nil)
        (suffix nil)
        (extra-keywords nil))
    (while (keywordp (setq keyw (car body)))
      (setq body (cdr body))
      (pcase keyw
        (:prefix (setq prefix (pop body)))
        (:suffix (setq suffix (pop body)))
        (_
         (push keyw extra-keywords)
         (pop body))))
    (when extra-keywords
      (error "Invalid keywords: %s" (mapconcat #'symbol-name extra-keywords " ")))
    (let ((prefix (or prefix ""))
          (suffix (or suffix "")))
      `(let ((,name (make-temp-file ,prefix nil ,suffix nil)))
         (unwind-protect
             (progn
               ,@body)
           (ignore-errors
             (delete-file ,name)))))))

(defun elisp-autofmt--simple-search-forward-and-count (str limit)
  "Search forward by STR, within LIMIT."
  (declare (important-return-value t))
  (let ((done 0))
    (while (search-forward str limit t 40)
      (incf done 40))
    (while (search-forward str limit t 1)
      (incf done))
    done))

(defun elisp-autofmt--simple-search-forward-by-count (str limit-count)
  "Search forward by STR, LIMIT-COUNT times."
  (declare (important-return-value t))
  (search-forward str nil t limit-count))

(defun elisp-autofmt--simple-count-lines (beg end)
  "Simply count newlines between BEG and END."
  (declare (important-return-value t))
  ;; Emacs's `count-lines' includes extra logic that adds 1 in some cases,
  ;; making it not useful for a simple line counting function.
  (save-excursion
    (with-restriction beg end
      (goto-char beg)
      (let ((done 0))
        (while (re-search-forward "\n\\|\r[^\n]" nil t 40)
          (incf done 40))
        (while (re-search-forward "\n\\|\r[^\n]" nil t 1)
          (incf done))
        done))))


(defun elisp-autofmt--bol-unless-non-blank (pos)
  "Return the line-beginning of POS when there is only blank space before point."
  (declare (important-return-value t))
  (save-excursion
    (goto-char pos)
    (let ((bol (pos-bol)))
      (cond
       ((eq pos bol)
        bol)
       (t
        (goto-char bol)
        (skip-chars-forward "[:blank:]" (1+ pos))
        (when (< (point) pos)
          (setq bol nil))
        bol)))))

(defun elisp-autofmt--bool-as-int (val)
  "Return 0/1 from VAL, nil/t."
  (declare (important-return-value t))
  (cond
   (val
    1)
   (t
    0)))

(defun elisp-autofmt--s-expr-range-around-pos (pos)
  "Return range around POS or nil."
  (declare (important-return-value t))
  (let ((beg
         (ignore-errors
           (nth 1 (syntax-ppss pos)))))
    (cond
     (beg
      ;; Note that `end' may be nil for un-matched brackets.
      ;; The caller must handle this case.
      (let ((end
             (ignore-errors
               (scan-sexps beg 1))))
        (cons beg end)))
     (t
      nil))))

(defun elisp-autofmt--s-expr-range-around-pos-dwim (pos)
  "Return range around POS, context sensitive."
  (declare (important-return-value t))
  (save-excursion
    (goto-char pos)
    (let ((fmt-region-range (elisp-autofmt--s-expr-range-around-pos (pos-bol))))
      (unless fmt-region-range
        ;; Search for the widest range in this line.
        (let ((eol (pos-eol))
              (bol (pos-bol))

              (range-best-around-pos nil)
              (range-best-length-around-pos 0)

              (range-best nil)
              (range-best-length 0))

          (goto-char bol)
          (while (< (point) eol)
            (skip-syntax-forward "^()")
            (let ((syntax (car (syntax-after (point))))
                  (range-test nil)
                  (range-test-length nil))
              (cond
               ((eq syntax 4) ;; Opening bracket.
                ;; Matches `C-M-n', (forward-list 1).
                (let ((pos-other (scan-sexps (point) 1)))
                  (when pos-other
                    (setq range-test (cons (point) pos-other))
                    (setq range-test-length (- pos-other (point))))))
               ((eq syntax 5) ;; Closing bracket.
                ;; Matches `C-M-p', (forward-list -1).
                ;; Point must be after ')'.
                (let ((pos-other (scan-sexps (1+ (point)) -1)))
                  (when pos-other
                    (setq range-test (cons pos-other (point)))
                    (setq range-test-length (- (point) pos-other))))))

              (when range-test
                (when (< range-best-length range-test-length)
                  (setq range-best range-test)
                  (setq range-best-length range-test-length))
                (when (< range-best-length-around-pos range-test-length)
                  (when (and (<= (car range-test) pos) (<= pos (cdr range-test)))
                    (setq range-best-around-pos range-test)
                    (setq range-best-length-around-pos range-test-length)))))
            (forward-char 1))

          (setq fmt-region-range (or range-best-around-pos range-best))
          (when fmt-region-range
            (let ((beg-bol (elisp-autofmt--bol-unless-non-blank (car fmt-region-range))))
              (when beg-bol
                (setcar fmt-region-range beg-bol))))))
      fmt-region-range)))

(defun elisp-autofmt--call-process (proc-id command-with-args stdin-buffer stdout-buffer)
  "Run COMMAND-WITH-ARGS, using STDIN-BUFFER as input, writing to STDOUT-BUFFER.

Both STDIN-BUFFER and STDOUT-BUFFER can be nil.
PROC-ID is used as the identifier for this process.

Return a cons cell comprised of the:
- Exit-code, nil when the command could not be run at all.
- Standard-error (or nil when none found)."
  (declare (important-return-value t))
  ;; A command that cannot run is a configuration error rather than something
  ;; callers can act on, report it through the return value like any other
  ;; failure. Cache generation especially runs where nothing catches a signal.
  (condition-case-unless-debug err
      (cond
       (elisp-autofmt--workaround-make-proc
        (elisp-autofmt--with-temp-file temp-file-stderr
          :prefix (concat proc-id "-")
          :suffix "-stderr"

          (let ((exit-code
                 (let ((default-process-coding-system elisp-autofmt--process-coding-system))
                   (cond
                    (stdin-buffer
                     ;; Use the whole `stdin-buffer'
                     (apply #'call-process-region
                            (append
                             (list
                              ;; No min/max (whole buffer).
                              nil nil
                              ;; First argument (program).
                              (car command-with-args)
                              ;; Don't delete.
                              nil
                              ;; Destination.
                              (list stdout-buffer temp-file-stderr)
                              ;; No display.
                              nil)
                             ;; Remaining arguments.
                             (cdr command-with-args))))
                    (t
                     (apply #'call-process
                            (append
                             (list
                              ;; First argument (command).
                              (car command-with-args)
                              ;; No INFILE.
                              nil
                              ;; Destination.
                              (list stdout-buffer temp-file-stderr)
                              ;; No display.
                              nil)
                             ;; Remaining arguments.
                             (cdr command-with-args)))))))
                (stderr-as-string
                 (progn
                   (with-temp-buffer
                     (insert-file-contents temp-file-stderr)
                     (cond
                      ((zerop (buffer-size))
                       nil)
                      (t
                       (buffer-string)))))))
            (cons exit-code stderr-as-string))))
       (t
        ;; prevent "Process {proc-id} finished" text.
        (elisp-autofmt--with-advice ((#'internal-default-process-sentinel :override #'ignore))
          (let ((sentinel-called 0)
                (sentinel-called-expect 1)
                (this-buffer (current-buffer))
                (stderr-buffer nil)
                (default-coding
                 (cond
                  ((boundp 'default-buffer-file-coding-system)
                   default-buffer-file-coding-system)
                  (t
                   'utf-8)))
                (default-process-coding-system elisp-autofmt--process-coding-system))
            (with-temp-buffer
              (setq stderr-buffer (current-buffer))
              (with-current-buffer this-buffer
                (let ((proc-out
                       (make-process
                        :name proc-id
                        :buffer stdout-buffer
                        :stderr stderr-buffer
                        :connection-type 'pipe
                        :command command-with-args
                        :coding (cons default-coding default-coding)
                        :sentinel (lambda (_proc _msg) (incf sentinel-called))))
                      (proc-err (get-buffer-process stderr-buffer)))

                  ;; Unfortunately a separate process is set for the STDERR
                  ;; which uses its own sentinel.
                  ;; Needed to override the "Process .. finished" message.
                  (unless (eq proc-out proc-err)
                    (setq sentinel-called-expect 2)
                    (set-process-sentinel proc-err (lambda (_proc _msg) (incf sentinel-called))))

                  ;; Only send when there is input. A command taking its input from
                  ;; arguments (`--gen-defs') never reads `stdin' and may exit before
                  ;; the send completes, closing the pipe. In practice this only shows
                  ;; up once the buffer exceeds the pipe capacity (64 KiB on Linux),
                  ;; smaller writes complete before the command has a chance to exit.
                  (when stdin-buffer
                    ;; Widen, a narrowed buffer must still send its whole contents.
                    ;; The formatted result is written back over the whole buffer
                    ;; (see the `save-restriction' calls in `elisp-autofmt--region-impl'),
                    ;; so sending only the accessible portion silently discards
                    ;; everything outside the restriction. The `call-process-region'
                    ;; branch passes a nil range which ignores narrowing already.
                    (save-restriction
                      (widen)
                      (process-send-region proc-out (point-min) (point-max)))
                    (process-send-eof proc-out))

                  (while (/= sentinel-called sentinel-called-expect)
                    (accept-process-output))

                  (let ((exit-code
                         (cond
                          ;; `process-exit-status' reports the signal number for
                          ;; a child killed by one, which collides with the exit
                          ;; code used to mean success, so signal 2 would pass
                          ;; the killed process's partial output off as a
                          ;; complete format. Use the string `call-process'
                          ;; returns in this case, which is never a code.
                          ((eq (process-status proc-out) 'signal)
                           (format "signal %d" (process-exit-status proc-out)))
                          (t
                           (process-exit-status proc-out))))
                        (stderr-as-string
                         (cond
                          ((zerop (buffer-size stderr-buffer))
                           nil)
                          (t
                           (with-current-buffer stderr-buffer
                             (buffer-string))))))
                    (cons exit-code stderr-as-string)))))))))
    (file-missing
     (cons nil (format "program not found (%s)" (error-message-string err))))
    (file-error
     ;; Typically formatting exiting with an error, closing the `stdin' mid-send.
     ;; Cache generation reaches this handler too and never sends input, so the
     ;; message must not name a step which may never have run.
     ;; Even though the `stderr' will almost always be set,
     ;; store the error as it may show additional context.
     (cons nil (format "file error (%s)" (error-message-string err))))
    (error
     (cons nil (format "unexpected error (%s)" (error-message-string err))))))

(defun elisp-autofmt--call-checked (command-with-args)
  "Run COMMAND-WITH-ARGS, returning t on success.

Callers use the result to know the output was written, so nil is returned
both when the command ran and failed and when it could not run at all.
The exit code alone decides this, `stderr' is messaged either way since a
command can write advisory output and still succeed."
  (declare (important-return-value t))

  (when elisp-autofmt-debug-extra-info
    (message "elisp-autofmt: running command: %s" (mapconcat #'identity command-with-args " ")))

  (let ((this-buffer (current-buffer))
        (stdout-buffer nil)
        (proc-id "elisp-autofmt--call-checked"))
    (with-temp-buffer
      (setq stdout-buffer (current-buffer))
      (with-current-buffer this-buffer
        (pcase-let ((`(,exit-code . ,stderr-as-string)
                     (elisp-autofmt--call-process proc-id command-with-args nil stdout-buffer)))

          (when stderr-as-string
            (cond
             ;; Advisory output beside a good result, or the error text from
             ;; emacs when the program could not run at all (not multi-line).
             ;; Neither is the command failing, so neither is labeled as
             ;; error output - the docstring promises a command can write
             ;; advisory output and still succeed.
             ((or (null exit-code) (and (integerp exit-code) (zerop exit-code)))
              (elisp-autofmt--message-once "elisp-autofmt: %s" stderr-as-string))
             (t
              (elisp-autofmt--message-once "elisp-autofmt: error output\n%s" stderr-as-string))))

          ;; Calling the process is completed.
          (cond
           ((null exit-code)
            nil)
           ;; A child killed by a signal returns a description string instead of
           ;; a number, see `call-process', so anything but zero is a failure.
           ((null (and (integerp exit-code) (zerop exit-code)))
            (elisp-autofmt--message-once "elisp-autofmt: Command %S failed with exit code %S!"
                                         command-with-args
                                         exit-code)
            nil)
           (t
            ;; Do nothing.
            t)))))))

;; ---------------------------------------------------------------------------
;; Internal Introspection / Cache Functions

;; For `find-library-name'.
(require 'find-func)
;; For `file-loadhist-lookup'.
(require 'loadhist)
;; For `find-lisp-object-file-name'.
;; (require 'help-fns)

(defun elisp-autofmt--json-string (str)
  "Return STR as a JSON string, the enclosing quotes included."
  (declare (important-return-value t))
  ;; A symbol name may hold any character once escaped, so writing it out
  ;; directly can produce a broken string which the reader then rejects.
  ;;
  ;; `json-encode-string' would do this, however requiring `json' adds its own
  ;; definitions to `load-history', which the built-in cache is generated from,
  ;; so the definitions users format against would depend on this detail.
  (let ((result (string-replace "\"" "\\\"" (string-replace "\\" "\\\\" str))))

    ;; Control characters must be escaped as well. A symbol name containing one
    ;; is far fetched, the test keeps the output valid instead of assuming so.
    (when (string-match-p "[[:cntrl:]]" result)
      (setq result
            (replace-regexp-in-string
             "[[:cntrl:]]" (lambda (match) (format "\\u%04x" (aref match 0))) result
             t t)))
    (concat "\"" result "\"")))

(defun elisp-autofmt--cache-api-val-as-str (val)
  "Return the string representation of VAL (use for JSON encoding)."
  (declare (important-return-value t))
  (cond
   ((symbolp val)
    ;; Use the printed representation, matching the keys. An `indent' naming
    ;; another symbol is looked up in the same table, so a decoded name here
    ;; would never resolve. The arity tokens print unchanged.
    (elisp-autofmt--json-string (prin1-to-string val)))
   (t
    (number-to-string val))))

(defun elisp-autofmt--cache-api-file-is-older-list (file-test file-list)
  "Return t when FILE-TEST is older than any files in FILE-LIST."
  (declare (important-return-value t))
  (let ((file-test-time (file-attribute-modification-time (file-attributes file-test))))
    (cond
     ;; A missing FILE-TEST has no time to compare against, it needs writing.
     ((null file-test-time)
      t)
     (t
      (let ((is-older nil))
        ;; Stop at the first, the files after it cannot change the result.
        (while (and file-list (null is-older))
          (let ((file-new-time
                 (file-attribute-modification-time (file-attributes (pop file-list)))))
            ;; A source that isn't there can't make FILE-TEST out of date.
            ;; Without this `time-less-p' reads the nil as the current time,
            ;; so the cache is judged stale and regenerated on every call.
            (setq is-older (and file-new-time (time-less-p file-test-time file-new-time)))))
        is-older)))))

(defun elisp-autofmt--cache-api-file-is-older (file-test &rest file-list)
  "Return t when FILE-TEST is older than any files in FILE-LIST."
  (declare (important-return-value t))
  (elisp-autofmt--cache-api-file-is-older-list file-test file-list))

(defun elisp-autofmt--cache-api-file-state (filepath)
  "Return the state of FILEPATH used to detect it changing.

Nil for a file that isn't there, which is a state in its own right."
  (declare (important-return-value t))
  (file-attribute-modification-time (file-attributes filepath)))

(defun elisp-autofmt--cache-api-file-is-complete (filepath)
  "Return t when the cache at FILEPATH was written in full.

Freshness is otherwise decided by the modification time alone, so a file
left truncated by a crash reads as up to date forever, failing to parse
on every run with no way to recover from within Emacs.

Both writers end with the two closing braces, so anything else was cut
short. Parsing the JSON would be exact, however requiring `json' adds its
own definitions to `load-history', which the built-in cache is generated
from, so the definitions users format against would depend on it."
  (declare (important-return-value t))
  (let ((size (file-attribute-size (file-attributes filepath))))
    (cond
     ((null size)
      nil)
     (t
      (condition-case-unless-debug _err
          (with-temp-buffer
            ;; Only the tail is needed, these files reach several megabytes.
            (insert-file-contents filepath nil (max 0 (- size 8)) size)
            (and (string-match-p "}[[:space:]]*}[[:space:]]*\\'" (buffer-string)) t))
        ;; A file that can be found but not read (written by another user,
        ;; a stale NFS handle). Unlikely, but the callers run from
        ;; `before-save-hook' where a signal aborts the save itself, and
        ;; treating the cache as incomplete regenerates it, which is the
        ;; only route that can put a readable file back.
        (file-error
         nil))))))

(defun elisp-autofmt--cache-api-encode-name (filename)
  "Return the cache name in cache-dir from FILENAME."
  (declare (important-return-value t))
  (concat (url-hexify-string filename) ".json"))

;; Use a different name for externally generated definitions
;; because it's possible they contain less/different information.
;; In this case it's possible that the order of generating different
;; definitions files could give different results,
;; so name them differently to avoid confusion.
(defun elisp-autofmt--cache-api-encode-name-external (filename)
  "Return the Python cache name in cache-dir from FILENAME."
  (declare (important-return-value t))
  (concat (url-hexify-string filename) ".external.json"))

;; Seconds after which a temporary is certainly abandoned.
;; Writing one takes milliseconds, this only has to clear the machines
;; clock being adjusted and a very slow file-system.
(defconst elisp-autofmt--cache-api-temp-stale-seconds 3600)

(defvar elisp-autofmt--cache-api-temp-is-swept nil
  "Non-nil once abandoned temporaries were removed, once per session.")

(defun elisp-autofmt--cache-api-temp-sweep ()
  "Remove temporaries left behind by a write that never completed.

The cleanup on a failed write cannot run when the process is killed
outright, so these accumulate with nothing else to remove them."
  (declare (important-return-value nil))
  (setq elisp-autofmt--cache-api-temp-is-swept t)
  (ignore-errors
    (let ((time-stale (time-subtract (current-time) elisp-autofmt--cache-api-temp-stale-seconds)))
      (dolist (filepath (directory-files elisp-autofmt-cache-directory t "\\.incomplete\\'" t))
        (let ((time-file (elisp-autofmt--cache-api-file-state filepath)))
          ;; A temporary being written right now must be left alone,
          ;; its writer renames it into place when it finishes.
          (when (and time-file (time-less-p time-file time-stale))
            (ignore-errors
              (delete-file filepath))))))))

(defun elisp-autofmt--cache-api-directory-ensure ()
  "Ensure the cache API directory exists."
  (declare (important-return-value nil))
  (cond
   ((file-directory-p elisp-autofmt-cache-directory)
    (unless elisp-autofmt--cache-api-temp-is-swept
      (elisp-autofmt--cache-api-temp-sweep)))
   (t
    (make-directory elisp-autofmt-cache-directory t)
    ;; Nothing to sweep in a directory that was just created.
    (setq elisp-autofmt--cache-api-temp-is-swept t))))

(defun elisp-autofmt--cache-api-write-region-atomic (filepath)
  "Write the current buffer to FILEPATH via a temporary file.

Writing FILEPATH directly leaves a truncated file behind when the write
does not complete, which is newer than its source and so considered up to
date, failing to parse on every later run.

Signals when FILEPATH cannot be written."
  (declare (important-return-value nil))
  ;; A unique name, two Emacs instances can regenerate the same cache at once
  ;; (both formatting on save after an Emacs upgrade for e.g.), where a shared
  ;; name lets them write over each other and rename the result in. The process
  ;; ID is not enough, the cache directory is often on a network home shared
  ;; between machines, which hand out the same IDs.
  ;;
  ;; The temporary shares the cache directory, so this is a rename
  ;; within one file-system and never a copy. Its name is a short fixed
  ;; prefix, not FILEPATH's own: `url-hexify-string' has already inflated
  ;; that, so appending to a name near NAME_MAX failed the write for a
  ;; cache whose final name was valid.
  (let ((filepath-temp
         (make-temp-file (concat (file-name-directory filepath) "tmp") nil ".incomplete"))
        ;; The reader opens these as UTF-8. Without this a name holding a
        ;; non-ASCII character has no safe coding system for the locale and
        ;; `write-region' prompts for one, which no handler can catch and
        ;; which blocks the save when it happens inside `before-save-hook'.
        (coding-system-for-write 'utf-8))
    (unwind-protect
        (progn
          ;; `make-temp-file' creates the file private to this user (it is
          ;; made for secrets) and `rename-file' keeps that, where a direct
          ;; write followed the umask like every other file - on a shared
          ;; cache directory the next user's read then failed. Execute bits
          ;; are masked out, `default-file-modes' answers for directories.
          (set-file-modes filepath-temp (logand #o666(default-file-modes)))
          (write-region nil nil filepath-temp nil 0)
          (rename-file filepath-temp filepath t))
      ;; A successful rename leaves nothing behind, only a failed write does.
      (when (file-exists-p filepath-temp)
        (ignore-errors
          (delete-file filepath-temp))))))

(defun elisp-autofmt--cache-api-insert-function-to-file (sym-id sym-ty arity)
  "Insert JSON data from SYM-ID, SYM-TY and ARITY."
  (declare (important-return-value nil))
  ;; `arity' is an argument because built-in functions use different logic.

  ;; There are many other properties, however they don't relate to formatting so much.
  (let ((properties nil))
    (pcase-dolist (`(,prop-name . ,prop-id)
                   (list
                    (cons "indent" 'lisp-indent-function) (cons "doc-string" 'doc-string-elt)))
      (let ((val (function-get sym-id prop-id t)))
        ;; Only a number or a symbol has a representation here,
        ;; `lisp-indent-function' may hold a function for e.g.
        (when (or (numberp val) (and val (symbolp val)))
          (push
           (concat "\"" prop-name "\": " (elisp-autofmt--cache-api-val-as-str val)) properties))))

    ;; Write the printed representation, not `symbol-name'. Definitions are
    ;; looked up by the text as it appears in the source, where a name holding
    ;; a character needing an escape keeps its backslashes, which is the form
    ;; `--gen-defs' writes when it reads the same definition from a file.
    (insert (elisp-autofmt--json-string (prin1-to-string sym-id)) ": ")
    (insert
     "["
     (elisp-autofmt--json-string (symbol-name sym-ty))
     ", "
     (elisp-autofmt--cache-api-val-as-str (car arity))
     ", "
     (elisp-autofmt--cache-api-val-as-str (cdr arity))
     ;; Dictionary for additional hints.
     ", {"
     (cond
      (properties
       (mapconcat #'identity properties ", "))
      (t
       ""))
     "}],\n")))

(defun elisp-autofmt--fn-type (sym-id)
  "Return the type of function SYM-ID or nil."
  (declare (important-return-value t))
  (cond
   ((functionp sym-id)
    'func)
   ((macrop sym-id)
    'macro)
   ((special-form-p sym-id)
    'special)
   (t
    nil)))

(defun elisp-autofmt--fn-defs-insert (defs include-private)
  "Insert all function from DEFS into the current buffer.
When INCLUDE-PRIVATE is nil, exclude functions with \"--\" in their names."
  (declare (important-return-value nil))
  (while defs
    (let ((n (pop defs)))
      (when (consp n)
        (pcase-let ((`(,_sym-ty-xx . ,sym-id) n))
          (let ((sym-ty (elisp-autofmt--fn-type sym-id)))
            (when sym-ty
              (let ((sym-name (symbol-name sym-id)))
                ;; Ignore "--" separators as this is a convention for private names.
                (when (or include-private (null (string-match-p "--" sym-name)))
                  (elisp-autofmt--cache-api-insert-function-to-file
                   sym-id sym-ty (func-arity sym-id)))))))))))

(defun elisp-autofmt--cache-api-generate-for-builtins (filepath)
  "Generate API cache for built-in output at FILEPATH.

Return t, signaling when FILEPATH cannot be written."
  (declare (important-return-value t))
  (with-temp-buffer
    (insert "{\n")
    (insert "\"functions\": {\n")
    (let ((block-beg (point)))
      (mapatoms
       (lambda (sym-id)
         (let ((sym-fn (symbol-function sym-id)))
           (when sym-fn
             (let ((auto-load-pkg (and (autoloadp sym-fn) (cadr sym-fn)))
                   (sym-ty (elisp-autofmt--fn-type sym-id)))

               (when (and sym-ty
                          ;; Is it non-interactive?
                          ;; (not (commandp (symbol-function sym-id)))
                          ;; Is it built-in? (speeds up accessing the file-path which is slow).
                          (subrp sym-fn)
                          (or (null auto-load-pkg)
                              (null
                               (member auto-load-pkg elisp-autofmt-ignore-autoload-packages))))
                 ;; (autoload sym-id)

                 ;; Note that we could check for C-source only using.
                 ;; (find-lisp-object-file-name sym-id sym-fn)

                 (when t
                   ;; (eq file 'C-source)
                   (elisp-autofmt--cache-api-insert-function-to-file
                    sym-id sym-ty
                    (cond
                     ((subrp sym-fn)
                      (subr-arity sym-fn))
                     (t
                      (func-arity sym-id)))))))))))

      ;; Inline built-in packages:
      ;; This avoids the hassles of having to hand maintain a list of built-in packages.
      ;; While the result is much larger, it avoids a lot of knit-picking over what
      ;; should/shouldn't be included. Just include everything loaded as part of Emacs
      ;; (in batch mode), and script can manually include other packages they depend on.

      ;; Load some additional packages.
      (dolist (package-id elisp-autofmt--packages-default)
        (require package-id))

      (let ((item-list load-history))
        (while item-list
          (let ((item (pop item-list)))
            (let ((defs (cdr item)))
              (elisp-autofmt--fn-defs-insert defs nil)))))

      ;; Remove trailing comma (tsk).
      (delete-region (max block-beg (- (point) 2)) (max block-beg (- (point) 1))))

    (insert "}\n") ; "functions".
    (insert "}\n")
    (elisp-autofmt--cache-api-write-region-atomic filepath)
    t))

(defun elisp-autofmt--cache-api-generate-for-package (filepath package-id skip-require)
  "Generate API cache for PACKAGE-ID at FILEPATH.

When SKIP-REQUIRE is non-nil, the package is not required.

Return t when written, nil when the package could not be loaded,
signaling when FILEPATH cannot be written."
  (declare (important-return-value t))
  (let ((package-sym (intern package-id)))
    (and (cond
          (skip-require
           t)
          ((member package-id (list "subr"))
           t)
          ((with-demoted-errors "%S"
             (require package-sym)
             t)
           t)
          (t
           (message "Unable to load %s" package-id)
           nil))

         (let ((defs (file-loadhist-lookup package-id)))
           (cond
            ;; With SKIP-REQUIRE the definitions are only available when the
            ;; running Emacs happens to have loaded the package already.
            ;; Writing an empty table would be newer than the package source,
            ;; so it would be considered up to date from then on and the real
            ;; definitions would never be picked up.
            ((and skip-require (null defs))
             (elisp-autofmt--message-once "elisp-autofmt: no definitions loaded for %s" package-id)
             nil)
            (t
             ;; Ensure the cache is newer than its source.
             (with-temp-buffer
               (insert "{\n")
               ;; Allow for other kinds of data in these files in the future.
               (insert "\"functions\": {\n")
               (let ((block-beg (point)))
                 (elisp-autofmt--fn-defs-insert defs t)
                 ;; Remove trailing comma (tsk).
                 (delete-region (max block-beg (- (point) 2)) (max block-beg (- (point) 1))))
               (insert "}\n") ; "functions".
               (insert "}\n")
               (elisp-autofmt--cache-api-write-region-atomic filepath)
               t)))))))

(defun elisp-autofmt--gen-builtin-defs ()
  "Generate builtin definitions.

Writes outputs to `ELISP_AUTOFMT_OUTPUT'."
  (declare (important-return-value nil))
  (let ((output-path (getenv "ELISP_AUTOFMT_OUTPUT")))
    (unless output-path
      (error "elisp-autofmt: $ELISP_AUTOFMT_OUTPUT was not set for built-ins!"))
    ;; A failure to write signals, which a batch Emacs exits non-zero for,
    ;; this is what tells the caller nothing was written.
    (elisp-autofmt--cache-api-generate-for-builtins output-path)))

(defun elisp-autofmt--gen-package-defs ()
  "Generate package definitions.

Uses package from environment variable `ELISP_AUTOFMT_PACKAGE'.
Writes outputs to environment variable `ELISP_AUTOFMT_OUTPUT'."
  (declare (important-return-value nil))
  (let ((output-path (getenv "ELISP_AUTOFMT_OUTPUT"))
        (package-id (getenv "ELISP_AUTOFMT_PACKAGE")))
    (unless output-path
      (error "elisp-autofmt: $ELISP_AUTOFMT_OUTPUT was not set for package!"))
    (unless package-id
      (error "elisp-autofmt: $ELISP_AUTOFMT_PACKAGE was not set for package!"))
    ;; A failure to write signals, a package that could not be loaded only
    ;; returns nil, error so a batch Emacs exits non-zero either way.
    (unless (elisp-autofmt--cache-api-generate-for-package output-path package-id nil)
      (error "elisp-autofmt: unable to load %s!" package-id))))

(defvar elisp-autofmt--cache-api-generate-failed nil
  "Cache paths mapped to the generating state that failed.
See `elisp-autofmt--cache-api-generate-is-failed'.")

(defun elisp-autofmt--cache-api-generate-state (filename)
  "Return the state generating a cache from FILENAME depends on.

The source file's state alone is not enough, a failure can be caused by
the configuration (`elisp-autofmt-python-bin' naming a missing program
for e.g.), which the user then corrects with the source untouched, so
the memo has to see that change to retry."
  (declare (important-return-value t))
  (list (elisp-autofmt--cache-api-file-state filename) elisp-autofmt-python-bin))

(defun elisp-autofmt--cache-api-generate-is-failed (filename-cache-name-full filename)
  "Return t when FILENAME-CACHE-NAME-FULL already failed to generate from FILENAME.

Generating spawns a sub-process for some routes, and a source that could
not be read stays that way until it is edited, so retrying on every format
costs a sub-process per save with nothing to show for it.

Only this session is remembered, restarting Emacs retries everything."
  (declare (important-return-value t))
  (and elisp-autofmt--cache-api-generate-failed
       (let ((state
              (gethash filename-cache-name-full elisp-autofmt--cache-api-generate-failed 'unset)))
         (and (null (eq state 'unset))
              (equal state (elisp-autofmt--cache-api-generate-state filename))))))

(defun elisp-autofmt--cache-api-generate-is-failed-set
    (filename-cache-name-full filename is-generated)
  "Note whether FILENAME-CACHE-NAME-FULL generated from FILENAME.
IS-GENERATED is the result of generating it."
  (declare (important-return-value nil))
  (unless elisp-autofmt--cache-api-generate-failed
    (setq elisp-autofmt--cache-api-generate-failed (make-hash-table :test #'equal)))
  (cond
   (is-generated
    (remhash filename-cache-name-full elisp-autofmt--cache-api-generate-failed))
   (t
    (puthash
     filename-cache-name-full
     (elisp-autofmt--cache-api-generate-state filename)
     elisp-autofmt--cache-api-generate-failed))))

(defun elisp-autofmt--cache-api-ensure
    (filename filename-cache-name-only generate-fn &optional always-generate)
  "Ensure the cache for FILENAME is up to date, generating it when it is not.

FILENAME-CACHE-NAME-ONLY is the cache name (no directory).
GENERATE-FN is called with the full cache path, returning non-nil on success.
When ALWAYS-GENERATE is non-nil the modification time is not consulted,
for definitions whose source is the running Emacs rather than the file.

Return FILENAME-CACHE-NAME-ONLY, which may name a cache that is out of
date, or nil when there are no definitions to use at all."
  (declare (important-return-value t))
  (let ((filename-cache-name-full
         (file-name-concat elisp-autofmt-cache-directory filename-cache-name-only)))

    (cond
     ;; Up to date, nothing to generate.
     ((and (null always-generate)
           (elisp-autofmt--cache-api-file-is-complete filename-cache-name-full)
           (null
            (elisp-autofmt--cache-api-file-is-older
             filename-cache-name-full
             filename
             ;; The generators themselves: an upgrade can change what
             ;; they write (a corrected key encoding for e.g.), which no
             ;; source reflects, so an existing cache would keep the old
             ;; form until Emacs itself or the source happened to change.
             elisp-autofmt--this-file
             elisp-autofmt--bin)))
      filename-cache-name-only)

     ((let ((is-generated
             (cond
              ;; Generating this already failed and nothing about the source
              ;; has changed since, so it would only fail the same way.
              ((elisp-autofmt--cache-api-generate-is-failed filename-cache-name-full filename)
               nil)
              (t
               ;; Generating can fail on an unwritable cache directory, a
               ;; default package that fails to load or unexpected
               ;; `load-history' data, so the message must not assert which
               ;; of them it was.
               (condition-case-unless-debug err
                   (progn
                     ;; Creating the directory fails the same way writing
                     ;; into it does, so it belongs inside this handler.
                     ;; It's only needed when something is written.
                     (elisp-autofmt--cache-api-directory-ensure)
                     (let ((is-generated (funcall generate-fn filename-cache-name-full)))
                       ;; Only an attempt that ran to completion is remembered.
                       ;; A signal is an environment problem (an unwritable
                       ;; directory, a full disk), which clears with no change
                       ;; to the source, so memoizing it would refuse to retry
                       ;; a failure that no longer exists.
                       (elisp-autofmt--cache-api-generate-is-failed-set
                        filename-cache-name-full filename is-generated)
                       is-generated))
                 (error
                  (elisp-autofmt--message-once "elisp-autofmt: unable to generate %s (%s)"
                                               filename-cache-name-full
                                               (error-message-string err))
                  nil))))))
        is-generated)
      filename-cache-name-only)

     ;; Regenerating failed but the cache from an earlier run is still there.
     ;; It describes all but the most recent edits, which is a far better
     ;; result than formatting as if the definitions did not exist.
     ;; Only a cache that was never written in full can't be named here.
     ((elisp-autofmt--cache-api-file-is-complete filename-cache-name-full)
      (elisp-autofmt--message-once "elisp-autofmt: using an out of date cache %s"
                                   filename-cache-name-full)
      filename-cache-name-only)

     (t
      nil))))

(defun elisp-autofmt--cache-api-ensure-cache-for-emacs ()
  "Ensure cache exists.

An external Emacs generates these, so they describe a default session
instead of whatever the current one happens to have loaded.

Return the cache name only (no directory) or nil
if the definitions could not be generated."
  (declare (important-return-value t))
  ;; Emacs binary location `filename'.
  (let ((filename (expand-file-name invocation-name invocation-directory)))
    (elisp-autofmt--cache-api-ensure
     filename (elisp-autofmt--cache-api-encode-name filename)
     (lambda (filename-cache-name-full)
       (let ((process-environment
              (cons
               (concat "ELISP_AUTOFMT_OUTPUT=" filename-cache-name-full) process-environment)))

         (elisp-autofmt--call-checked
          (list
           filename
           ;; Site files can generate warnings, interfering with the batch operation.
           ;; For example a warning about a header not including lexical-binding
           ;; will cause the command to fail entirely.
           "--no-site-file"
           "--no-site-lisp"
           "--batch"
           "-l"
           elisp-autofmt--this-file
           "--eval"
           "(elisp-autofmt--gen-builtin-defs)")))))))

(defvar elisp-autofmt--cache-api-package-defs-state nil
  "Cache paths mapped to the definitions state they were written from.
See `elisp-autofmt--cache-api-ensure-cache-for-package'.")

(defun elisp-autofmt--cache-api-package-defs-state-calc (defs)
  "Return a value that changes when DEFS would serialize differently.

DEFS is a `load-history' entry. The definitions are listed in it, however
the `indent' and `doc-string' hints live on each symbol's property list,
which re-evaluating a `declare' updates with no trace here, so they are
folded in explicitly. An arity change alone still goes unseen - in
practice an edited definition is loaded again, which replaces the entry."
  (declare (important-return-value t))
  (let ((props nil))
    (dolist (item defs)
      (when (consp item)
        (let ((sym (cdr item)))
          (when (symbolp sym)
            (let ((indent (function-get sym 'lisp-indent-function t))
                  (doc-string (function-get sym 'doc-string-elt t)))
              (when (or indent doc-string)
                (push (list sym indent doc-string) props)))))))
    (sxhash-equal (cons defs props))))

(defun elisp-autofmt--cache-api-ensure-cache-for-package (package-id)
  "Ensure cache for PACKAGE-ID is up to date in CACHE-DIR.

The package is never required, so its definitions are only available
when this Emacs has loaded it.

Return the cache name only (no directory), the symbol `not-loaded' when
this Emacs cannot offer the package's definitions (never loaded, or not
found at all), or nil if the definitions could not be generated."
  (declare (important-return-value t))
  (let ((filename
         ;; Signals when the library isn't on the `load-path', a mistyped
         ;; name in `elisp-autofmt-load-packages-local' for e.g.
         (condition-case-unless-debug _err
             (find-library-name package-id)
           (error
            (elisp-autofmt--message-once "elisp-autofmt: unable to find library %s" package-id)
            nil))))

    (cond
     ;; Not on the `load-path' at all: the same outcome as a package the
     ;; session never loaded, there are no definitions to offer. A shared
     ;; directory local can name a package only some machines have
     ;; installed, so treating this as a failure would disable formatting
     ;; for the whole project on the machines without it.
     ((null filename)
      'not-loaded)
     ;; Nothing to generate for a package the session never loaded, which
     ;; formatting will not do itself, so this is the usual outcome for a
     ;; package the user names and not a failure to report as one.
     ;;
     ;; Decided before the failure memo can be consulted on purpose: the
     ;; memo keys on the file's state, which loading the package does not
     ;; change, so a memoized "not loaded" would refuse the package for
     ;; the rest of the session after it *is* loaded.
     ((null (file-loadhist-lookup package-id))
      (elisp-autofmt--message-once "elisp-autofmt: no definitions loaded for %s" package-id)
      'not-loaded)
     (t
      (let* ((filename-cache-name-only (elisp-autofmt--cache-api-encode-name filename))
             (filename-cache-name-full
              (file-name-concat elisp-autofmt-cache-directory filename-cache-name-only))
             ;; Cheap next to serializing: no file is read or written.
             (defs-state
              (elisp-autofmt--cache-api-package-defs-state-calc
               (file-loadhist-lookup package-id))))
        (cond
         ;; Written by this session from definitions in this same state, so
         ;; generating again would write the same bytes. Without this the
         ;; cache is re-serialized and rewritten on every format (the
         ;; modification time cannot say whether it matches, see the note
         ;; on ALWAYS-GENERATE below), a cost per save that grows with the
         ;; package and churns a cache directory that often lives on a
         ;; network home.
         ((and elisp-autofmt--cache-api-package-defs-state
               (equal
                defs-state
                (gethash filename-cache-name-full elisp-autofmt--cache-api-package-defs-state))
               (elisp-autofmt--cache-api-file-is-complete filename-cache-name-full))
          filename-cache-name-only)
         (t
          (elisp-autofmt--cache-api-ensure
           filename
           filename-cache-name-only
           (lambda (filename-cache-name-full)
             (let ((is-generated
                    (elisp-autofmt--cache-api-generate-for-package
                     filename-cache-name-full package-id t)))
               ;; Only a completed write records the state, a failure that
               ;; fell back to an older cache must not read as current.
               (when is-generated
                 (unless elisp-autofmt--cache-api-package-defs-state
                   (setq
                    elisp-autofmt--cache-api-package-defs-state (make-hash-table :test #'equal)))
                 (puthash
                  filename-cache-name-full defs-state elisp-autofmt--cache-api-package-defs-state))
               is-generated))
           ;; These come from `load-history', which changes when the package
           ;; is evaluated and not when its file is written, so the
           ;; modification time says nothing about whether the cache still
           ;; matches. It is generated in-process, no sub-process is paid
           ;; for this.
           t))))))))

(defun elisp-autofmt--cache-api-ensure-cache-for-filepath (filepath)
  "Generate cache for FILEPATH.

Return the cache name only (no directory) or nil
if the definitions could not be generated."
  (declare (important-return-value t))
  (cond
   ;; A mistyped entry in `elisp-autofmt-load-packages-local' for e.g.
   ;; Otherwise the missing file is only noticed by `--gen-defs', which
   ;; reports it as a Python trace-back.
   ;;
   ;; Checked before the cache is consulted so this is reported at all: a
   ;; missing source has no modification time to make the cache stale, so a
   ;; complete cache reads as fresh and generating (where this check once
   ;; lived) is skipped. The cache is still used, a file that is renamed or
   ;; briefly unavailable keeps formatting against the definitions it
   ;; already has - only now the entry names itself while doing so.
   ((null (file-exists-p filepath))
    (elisp-autofmt--message-once "elisp-autofmt: unable to find file %s" filepath)
    (let* ((filename-cache-name-only (elisp-autofmt--cache-api-encode-name-external filepath))
           (filename-cache-name-full
            (file-name-concat elisp-autofmt-cache-directory filename-cache-name-only)))
      (cond
       ((elisp-autofmt--cache-api-file-is-complete filename-cache-name-full)
        (elisp-autofmt--message-once "elisp-autofmt: using an out of date cache %s"
                                     filename-cache-name-full)
        filename-cache-name-only)
       (t
        nil))))
   ;; A directory passes `file-exists-p', so it reached `--gen-defs',
   ;; which died opening it with a Python trace-back. Unlikely, still a
   ;; message naming the entry beats a trace-back naming nothing.
   ((null (file-regular-p filepath))
    (elisp-autofmt--message-once "elisp-autofmt: not a regular file %s" filepath)
    nil)
   (t
    (elisp-autofmt--cache-api-ensure
     filepath (elisp-autofmt--cache-api-encode-name-external filepath)
     (lambda (filename-cache-name-full)
       (let ((command-with-args
              (append
               ;; Python command (or empty to directly execute the script)
               (elisp-autofmt--python-commands-or-empty)
               ;; Main command.
               (list
                elisp-autofmt--bin
                "--gen-defs"
                filepath
                (expand-file-name filename-cache-name-full))))
             (process-environment (elisp-autofmt--python-env-prepend process-environment)))
         (elisp-autofmt--call-checked command-with-args)))))))

(defun elisp-autofmt--cache-api-cache-update (buffer-directory)
  "Ensure packages are up to date for `current-buffer' in BUFFER-DIRECTORY.

Return a list of cache names (no directory),
nil when the built-in definitions are unavailable."
  (declare (important-return-value t))
  ;; Sweeping only when something generates never runs in the steady state
  ;; where every cache is already up to date - the whole rest of a session
  ;; once the caches exist, and exactly when a temporary abandoned by a
  ;; killed writer has nothing else to remove it.
  (unless elisp-autofmt--cache-api-temp-is-swept
    (when (file-directory-p elisp-autofmt-cache-directory)
      (elisp-autofmt--cache-api-temp-sweep)))
  ;; Unlike the packages below, the built-in definitions are not optional.
  ;; Without them indentation which depends on them collapses, so the caller
  ;; skips formatting rather than writing that result back.
  ;; A warning message will have already been displayed.
  (let ((filename-cache-name-emacs (elisp-autofmt--cache-api-ensure-cache-for-emacs)))
    (when filename-cache-name-emacs
      (let ((cache-files (list filename-cache-name-emacs))
            (package-list-paths (list))
            (package-list (list)))

        (let ((packages
               (cond
                ;; A bare string instead of a list of them is an easy mistake,
                ;; where `pop' signals and the error escapes `before-save-hook',
                ;; aborting the save. Not `listp', which accepts a dotted pair
                ;; (a mis-quoted two-entry list for e.g.) whose tail `pop'
                ;; still signals on, and reads a circular list as fine when
                ;; the `while' below would never finish.
                ((proper-list-p elisp-autofmt-load-packages-local)
                 elisp-autofmt-load-packages-local)
                (t
                 (elisp-autofmt--message-once
                  "elisp-autofmt: elisp-autofmt-load-packages-local must be a list, not %S"
                  elisp-autofmt-load-packages-local)
                 nil))))
          (while packages
            (let ((var (pop packages)))
              (cond
               ((null (stringp var))
                ;; Unlikely, just a helpful hint to users.
                (elisp-autofmt--message-once
                 "elisp-autofmt: skipping non-string feature reference %S"
                 var))
               ((string-prefix-p "." var)
                (push var package-list-paths))
               (t
                (push var package-list))))))

        ;; Either of these may fail, leaving a nil in the list.
        ;; A warning message will have already been displayed.

        ;; Merge default and any local features into a list.
        (let ((packages-all (delete-dups package-list)))
          (dolist (package-id packages-all)
            (let ((cache-file (elisp-autofmt--cache-api-ensure-cache-for-package package-id)))
              ;; A package this Emacs cannot offer (never loaded, or not
              ;; installed at all) has nothing to contribute and says so,
              ;; drop it instead of stopping the format. Naming one is no
              ;; guarantee the session has it, a batch Emacs loads almost
              ;; nothing and a shared directory local reaches machines with
              ;; other packages installed, so treating this as a failure
              ;; would leave such a buffer unformatted for good.
              (unless (eq cache-file 'not-loaded)
                (push cache-file cache-files)))))

        ;; Ensure external definitions.
        (dolist (var package-list-paths)
          ;; Expand instead of stripping the leading ".", which only handled
          ;; "./" and quietly turned "../" into the buffer's own directory.
          (push (elisp-autofmt--cache-api-ensure-cache-for-filepath
                 (expand-file-name var buffer-directory))
                cache-files))

        ;; A source the user named which could not be read at all is not a
        ;; lesser result, formatting collapses the lines depending on it and
        ;; saves that, so leave the buffer alone the way missing built-in
        ;; definitions do. An out of date cache still counts as available,
        ;; see `elisp-autofmt--cache-api-ensure'. A package the session never
        ;; loaded was dropped above and does not reach this.
        (unless (memq nil cache-files)
          ;; `push' built the list backwards and the forward order carries
          ;; meaning: the formatter reads these with the last file winning,
          ;; so the built-ins must come first for the user's definitions
          ;; (the more local, the later) to override them.
          (nreverse cache-files))))))


;; ---------------------------------------------------------------------------
;; Internal Functions

(defun elisp-autofmt--replace-buffer-contents-fmt-region (buf-src beg end)
  "Isolate the region to be replaced in BEG END to format the region/selection.
Argument BUF-SRC is the buffer containing the formatted text."
  (declare (important-return-value nil))
  ;; Use a simple trick, replace the beginning and of the formatted buffer
  ;; with the original (unformatted) text.

  ;; Keep the original beginning because we may want to expand back to the beginning
  ;; of the line if there is only white-space before the contracted bounds.
  ;; This is needed so formatting a block does not have wrong indentation.
  (let ((beg-orig beg)
        (changed nil)
        (skip-chars (list ?\s ?\t ?\n ?\r)))
    ;; Contract region to non white-space bounds.
    ;; Note that we are not strict about the syntax, it's possible these
    ;; characters are inside comments or strings. The logic will still work.
    (while (and beg (memq (char-after beg) skip-chars))
      (incf beg)
      (unless (<= beg end)
        (setq beg nil)))

    (unless beg
      (setq end nil))

    (while (and end (memq (char-before end) skip-chars))
      (decf end)
      (unless (<= beg end)
        (setq end nil)))

    (unless (and beg end)
      (user-error "Region contains no S-expressions or vector literals!"))

    (let* ((buf-dst (current-buffer))

           (buf-dst-pos-min (point-min))
           (buf-dst-pos-max (point-max))

           (beg-index 0)
           (end-index 0)

           (beg-char (char-after beg))
           (end-char (char-before end))

           (beg-str (char-to-string beg-char))
           (end-str (char-to-string end-char))

           (beg-dst-pos nil)
           (end-dst-pos nil)
           (beg-dst-pos-bol nil)

           (beg-src-pos nil)
           (end-src-pos nil)
           (beg-src-pos-bol nil))

      (save-excursion
        (goto-char (point-min))
        (let ((limit (1+ beg)))
          (save-match-data
            (setq beg-index (elisp-autofmt--simple-search-forward-and-count beg-str limit))
            ;; The point before the character.
            (setq beg-dst-pos (1- (point)))
            (setq beg-dst-pos-bol (elisp-autofmt--bol-unless-non-blank beg-dst-pos))
            (setq limit (1+ end))
            (setq end-index (elisp-autofmt--simple-search-forward-and-count end-str limit))
            (setq end-dst-pos (point))))

        ;; Load the formatted buffer and replace the head & tail with unformatted text
        ;; so as only to reformat the requested region.
        (with-current-buffer buf-src
          (save-match-data
            (goto-char (point-min))
            (unless (elisp-autofmt--simple-search-forward-by-count beg-str beg-index)
              ;; Sanity check, should never happen.
              (user-error "Failed to re-find the start of formatted region, abort!"))
            ;; The point before the character.
            (setq beg-src-pos (1- (point)))
            (setq beg-src-pos-bol (elisp-autofmt--bol-unless-non-blank beg-src-pos))
            (unless (elisp-autofmt--simple-search-forward-by-count end-str end-index)
              ;; Sanity check, should never happen.
              (user-error "Failed to re-find the end of formatted region, abort!"))
            (setq end-src-pos (point)))

          ;; Optionally expand the beginning to include indentation,
          ;; without this lines may be badly indented.
          ;; Only do this when:
          ;; - When white-space is included in the original region.
          ;; - When there is space before the formatted text both before & after formatting.
          (when (and beg-dst-pos-bol beg-src-pos-bol (<= beg-dst-pos-bol beg-orig))
            (setq beg-dst-pos beg-dst-pos-bol)
            (setq beg-src-pos beg-src-pos-bol))

          ;; Report if formatting was performed.
          (cond
           ((/= (- end-dst-pos beg-dst-pos) (- end-src-pos beg-src-pos))
            (setq changed t))
           (t
            (let ((str-src (buffer-substring-no-properties beg-src-pos end-src-pos))
                  (str-dst
                   (with-current-buffer buf-dst
                     (buffer-substring-no-properties beg-dst-pos end-dst-pos))))
              (unless (string-equal str-src str-dst)
                (setq changed t)))))

          ;; Replace unformatted code at the beginning and end.
          (delete-region end-src-pos (point-max))
          (delete-region (point-min) beg-src-pos)

          (goto-char (point-max))
          (insert-buffer-substring buf-dst end-dst-pos buf-dst-pos-max)

          (goto-char (point-min))
          (insert-buffer-substring buf-dst buf-dst-pos-min beg-dst-pos))))
    changed))

(defun elisp-autofmt--replace-region-contents-wrapper (pos-min pos-max buf is-interactive)
  "Replace POS-MIN - POS-MAX with BUF, fast-path when undo is disabled.

Argument IS-INTERACTIVE is set when running interactively."
  (let ((is-beg (bobp))
        (is-end (eobp)))
    (cond
     ;; No undo, use a simple method instead of `replace-region-contents',
     ;; which has no benefit unless undo is in use.
     ((and (eq t buffer-undo-list) (or is-beg is-end))
      (cond
       ((and (eq pos-min (point-min)) (eq pos-max (point-max)))
        (erase-buffer))
       (t
        (delete-region pos-min pos-max)
        (goto-char pos-min)))
      (insert-buffer-substring buf)
      (cond
       (is-beg
        (goto-char (point-min)))
       (is-end
        (goto-char (point-max)))))
     (t
      (let ((max-secs
             (cond
              (is-interactive
               1.0)
              (t
               nil)))
            ;; Once emacs-31 is the minimum supported version,
            ;; This can be dropped and `buf' can be passed in.
            (buf-fn (lambda () buf)))
        (replace-region-contents pos-min pos-max buf-fn max-secs))))))

(defun elisp-autofmt--region-impl
    (stdout-buffer fmt-region-range to-file is-interactive &optional assume-file-name)
  "Auto format the current region using temporary STDOUT-BUFFER.
Optional argument ASSUME-FILE-NAME overrides the file name used for this buffer.

Argument FMT-REGION-RANGE optionally defines a region to format.
Argument TO-FILE writes to the file directly, without updating the buffer.
Argument IS-INTERACTIVE is set when running interactively.

Return non-nil once the formatted result has been written, nil when the
buffer was left alone.  A buffer that was already formatted counts as
written, callers use this to know the text is not the unformatted input."
  (declare (important-return-value t))

  (unless assume-file-name
    (setq assume-file-name buffer-file-name))

  ;; Everything reported below is per format, see `elisp-autofmt--message-once'.
  (elisp-autofmt--message-once-begin)

  ;; Cache files.
  (let ((cache-defs
         (cond
          (elisp-autofmt-use-function-defs
           (elisp-autofmt--cache-api-cache-update
            (cond
             (assume-file-name
              (file-name-directory assume-file-name))
             (t
              ;; In this case, any relative path references
              ;; from a buffer without a path, uses the default directory.
              ;; In practice it seems unlikely the kinds of buffers that aren't backed
              ;; by a file would reference relative tags, nevertheless, there is no need
              ;; for this operation to fail with an error, see #2.
              default-directory))))
          (t
           nil))))
    (cond
     ;; Definitions the buffer needs could not be provided, and formatting
     ;; without them is not a lesser result, it collapses lines that depend on
     ;; them. Leave it alone. Both this and using none at all are nil, so the
     ;; setting has to be part of the test.
     ;;
     ;; Report every time, the cause above is only shown once and a buffer
     ;; which quietly stops formatting is worse than a repeat.
     ((and elisp-autofmt-use-function-defs (null cache-defs))
      (message "elisp-autofmt: not formatted, definitions unavailable")
      nil)
     (t
      (let* ((use-diff-range
              (and elisp-autofmt-use-diff-range (null fmt-region-range) (null to-file)))
             (proc-id "elisp-autofmt")

             ;; Optionally
             (line-range
              (cond
               (fmt-region-range
                ;; Widen, these lines index into the buffer as sent,
                ;; which is never narrowed, see `elisp-autofmt--call-process'.
                (save-restriction
                  (widen)
                  (let* ((line-beg
                          (1+ (elisp-autofmt--simple-count-lines
                               (point-min) (car fmt-region-range))))
                         (line-end
                          (+ line-beg
                             (elisp-autofmt--simple-count-lines
                              (car fmt-region-range) (cdr fmt-region-range)))))
                    (cons line-beg line-end))))
               (t
                (cons 0 0))))

             (command-with-args
              (append
               ;; Python command.
               (elisp-autofmt--python-commands-or-empty)
               ;; Main command.
               (list
                elisp-autofmt--bin
                ;; No messages.
                "--quiet"
                ;; Don't use the file, use the stdin instead.
                "--stdin"
                ;; Use the standard output.
                "--stdout")
               (cond
                (use-diff-range
                 (list "--use-diff-range"))
                (t
                 (list)))
               (list
                ;; Follow the 'fill-column' setting.
                (format "--fmt-fill-column=%d" fill-column)
                (format "--fmt-empty-lines=%d" elisp-autofmt-empty-line-max)
                ;; Range is optional, 0:0 is default for the full range.
                (format "--fmt-line-range=%d:%d" (car line-range) (cdr line-range))
                (format "--fmt-style=%s" (symbol-name elisp-autofmt-style))
                (format "--fmt-quoted=%d" (elisp-autofmt--bool-as-int elisp-autofmt-format-quoted))

                (format "--parallel-jobs=%d"
                        (cond
                         ((<= (cond
                               (fmt-region-range
                                (- (cdr fmt-region-range) (car fmt-region-range)))
                               (t
                                (buffer-size)))
                              elisp-autofmt-parallel-threshold)
                          -1)
                         (t
                          elisp-autofmt-parallel-jobs)))

                ;; Not 0 or 1.
                "--exit-code=2")

               ;; Optionally read in definitions.
               (cond
                ((or elisp-autofmt-use-function-defs elisp-autofmt-use-default-override-defs)
                 (list
                  (concat
                   "--fmt-defs-dir="
                   (convert-standard-filename (expand-file-name elisp-autofmt-cache-directory)))
                  (concat
                   "--fmt-defs="
                   (mapconcat #'identity
                              (let ((override-defs
                                     (cond
                                      (elisp-autofmt-use-default-override-defs
                                       (list (concat elisp-autofmt--base ".overrides.json")))
                                      (t
                                       (list)))))
                                ;; Later files override earlier ones, so the
                                ;; bundled overrides sit directly after the
                                ;; built-in definitions they adjust - the
                                ;; first entry, see
                                ;; `elisp-autofmt--cache-api-cache-update'.
                                ;; After everything they would win over the
                                ;; user's own definitions instead.
                                (cond
                                 (cache-defs
                                  (append (list (car cache-defs)) override-defs (cdr cache-defs)))
                                 (t
                                  override-defs)))
                              path-separator))))
                (t
                 (list)))))
             (process-environment (elisp-autofmt--python-env-prepend process-environment)))

        (when elisp-autofmt-debug-extra-info
          (message "elisp-autofmt: running piped process: %s"
                   (mapconcat #'identity command-with-args " ")))

        (pcase-let ((`(,exit-code . ,stderr-as-string)
                     (elisp-autofmt--call-process
                      proc-id command-with-args (current-buffer) stdout-buffer)))

          ;; Calling the process is completed.
          ;;
          ;; The exit code decides this, not whether anything was written to the
          ;; `stderr'. Advisory output (an unknown hint in a definitions file for
          ;; e.g.) accompanies a perfectly good result, and discarding it left
          ;; the buffer unformatted on every save with the formatter reporting
          ;; success. Anything the caller must act on exits non-zero.
          (cond
           ((null (eq exit-code 2))
            (cond
             (stderr-as-string
              (cond
               (exit-code
                (message "elisp-autofmt: error code %S, output\n%s" exit-code stderr-as-string))
               (t
                ;; The program could not run, this error will be from emacs (not multi-line)
                (message "elisp-autofmt: %s" stderr-as-string))))
             (exit-code
              ;; A formatter killed outright (the OOM killer for e.g.) has no
              ;; chance to say why and writes nothing, which took the silent
              ;; branch - and a buffer which quietly stops formatting is
              ;; worse than a terse report.
              (message "elisp-autofmt: error code %S, no output" exit-code)))

            ;; A command that could not run has no exit code to report,
            ;; the reason was messaged above, see `elisp-autofmt--call-process'.
            (when (and elisp-autofmt-debug-extra-info exit-code)
              (message "elisp-autofmt: Command %S failed with exit code %S!"
                       command-with-args
                       exit-code))
            nil)
           (t
            ;; The result is good, the output is advisory, still show it once.
            (when stderr-as-string
              (elisp-autofmt--message-once "elisp-autofmt: %s" stderr-as-string))
            (cond
             (to-file
              (with-current-buffer stdout-buffer
                (write-region (point-min) (point-max) assume-file-name)))
             (fmt-region-range
              ;; Widen over the replacement too, not only the region calculation.
              ;; `stdout-buffer' holds the whole formatted buffer, so replacing
              ;; the accessible portion with it duplicates the text outside a
              ;; narrowing restriction.
              (save-restriction
                (widen)
                (let ((changed
                       (elisp-autofmt--replace-buffer-contents-fmt-region
                        stdout-buffer (car fmt-region-range) (cdr fmt-region-range))))
                  ;; Even though only a small region changed, use logic that re-writes the buffer.
                  (when changed
                    (elisp-autofmt--replace-region-contents-wrapper
                     (point-min) (point-max) stdout-buffer is-interactive))
                  (when is-interactive
                    (message "elisp-autofmt: %s"
                             (cond
                              (changed
                               "reformat")
                              (t
                               "reformat (unnecessary)")))))))
             (use-diff-range
              (let ((diff-range-beg nil)
                    (diff-range-end nil))
                (with-current-buffer stdout-buffer
                  (goto-char (point-min))
                  ;; Read the first line, then remove it.
                  (let* ((header-eol (pos-eol))
                         (header (read (buffer-substring (point-min) header-eol))))
                    (setq diff-range-beg (car header))
                    (setq diff-range-end (cdr header))
                    (delete-region (point-min) (1+ header-eol))))
                (unless (and (eq -1 diff-range-beg) (eq -1 diff-range-end))
                  (save-restriction
                    (widen)
                    (elisp-autofmt--replace-region-contents-wrapper
                     diff-range-beg diff-range-end stdout-buffer is-interactive)))))
             (t
              (save-restriction
                (widen)
                (elisp-autofmt--replace-region-contents-wrapper
                 (point-min) (point-max) stdout-buffer is-interactive))))
            ;; Every branch above reached the formatted result, report that
            ;; instead of whatever the last call in each happens to return.
            ;; `write-region' returns nil, so did an already formatted buffer
            ;; under `elisp-autofmt-use-diff-range', which callers acting on
            ;; this took for a failure, see `elisp-autofmt-buffer-to-file'.
            t))))))))

(defun elisp-autofmt--region (fmt-region-range to-file is-interactive &optional assume-file-name)
  "Auto format the current buffer in FMT-REGION-RANGE.
Optional argument ASSUME-FILE-NAME overrides the file name used for this buffer.

See `elisp-autofmt--region-impl' for TO-FILE and IS-INTERACTIVE doc-strings."
  (declare (important-return-value t))
  (let ((stdout-buffer nil)
        (this-buffer (current-buffer)))
    (with-temp-buffer
      (setq stdout-buffer (current-buffer))
      (with-current-buffer this-buffer
        (elisp-autofmt--region-impl stdout-buffer fmt-region-range to-file is-interactive
                                    assume-file-name)))))

(defun elisp-autofmt--buffer-impl (buf fmt-region-range to-file is-interactive)
  "Auto-format the entire buffer BUF in FMT-REGION-RANGE.

See `elisp-autofmt--region-impl' for TO-FILE and IS-INTERACTIVE doc-strings."
  (declare (important-return-value t))
  (with-current-buffer buf
    (elisp-autofmt--region fmt-region-range to-file is-interactive)))

(defun elisp-autofmt--buffer-format-for-save-hook ()
  "The hook to run on buffer saving to format the buffer."
  (declare (important-return-value t))
  ;; Demote errors as this is user configurable, we can't be sure it won't error.
  (when (with-demoted-errors "elisp-autofmt: Error %S"
          (funcall elisp-autofmt-on-save-p))
    (elisp-autofmt-buffer))
  ;; Continue to save.
  nil)

(defun elisp-autofmt--enable ()
  "Setup an auto-format save hook for this buffer."
  (declare (important-return-value nil))
  ;; Buffer local hook.
  (add-hook 'before-save-hook #'elisp-autofmt--buffer-format-for-save-hook nil t))

(defun elisp-autofmt--disable ()
  "Disable the hooks associated with `elisp-autofmt-mode'."
  (declare (important-return-value nil))
  (remove-hook 'before-save-hook #'elisp-autofmt--buffer-format-for-save-hook t))


;; ---------------------------------------------------------------------------
;; Public Functions


;;;###autoload
(defun elisp-autofmt-buffer-to-file ()
  "Auto format the current buffer, writing its output to a file.

This is intended for use by batch processing scripts,
where loading changes back into the buffer is not important.

Signals when the buffer could not be formatted, a batch Emacs exits
non-zero so the caller doesn't take the unformatted text for a result."
  (declare (important-return-value nil))
  (unless buffer-file-name
    (error "A buffer with a valid file-name expected!"))
  (unless (elisp-autofmt--buffer-impl (current-buffer) nil t nil)
    (error "elisp-autofmt: unable to format %s!" buffer-file-name)))

;;;###autoload
(defun elisp-autofmt-buffer ()
  "Auto format the current buffer."
  (declare (important-return-value nil))
  (interactive "*")
  (let ((is-interactive (called-interactively-p 'interactive)))
    (elisp-autofmt--buffer-impl (current-buffer) nil nil is-interactive)))

;;;###autoload
(defun elisp-autofmt-region (&optional beg end is-interactive)
  "Auto format the active region of the current buffer.
Optionally use BEG & END, otherwise an active region is required.
Optionally pass in IS-INTERACTIVE to display a status message from formatting."
  (declare (important-return-value nil))
  (interactive "*")

  (unless (and beg end)
    (unless (region-active-p)
      (user-error "No active region"))
    (setq beg (region-beginning))
    (setq end (region-end)))

  (let ((is-interactive (or is-interactive (called-interactively-p 'interactive))))
    (elisp-autofmt--buffer-impl (current-buffer) (cons beg end) nil is-interactive)))

;;;###autoload
(defun elisp-autofmt-region-dwim ()
  "Context sensitive auto formatting of the current buffer.
When there is an active region, this is used,
otherwise format the surrounding S-expression."
  (declare (important-return-value nil))
  (interactive "*")
  (let ((is-interactive (called-interactively-p 'interactive)))
    (cond
     ((region-active-p)
      (elisp-autofmt-region (region-beginning) (region-end) is-interactive))
     (t
      (let ((fmt-region-range (elisp-autofmt--s-expr-range-around-pos-dwim (point))))
        (unless fmt-region-range
          (user-error "Unable to find surrounding brackets!"))
        (elisp-autofmt-region (car fmt-region-range) (cdr fmt-region-range) is-interactive))))))

;;;###autoload
(defun elisp-autofmt-check-elisp-autofmt-exists ()
  "Return non-nil when `.elisp-autofmt' is found in a parent directory."
  (declare (important-return-value t))
  ;; Unlikely but possible this is nil.
  (let ((filepath buffer-file-name))
    (cond
     ((and filepath (locate-dominating-file (file-name-directory filepath) ".elisp-autofmt"))
      t)
     (t
      nil))))

;;;###autoload
(define-minor-mode elisp-autofmt-mode
  "Elisp-AutoFMT minor mode."
  :global nil
  :lighter ""
  :keymap nil

  (cond
   (elisp-autofmt-mode
    (elisp-autofmt--enable))
   (t
    (elisp-autofmt--disable))))

(provide 'elisp-autofmt)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; elisp-autofmt-format-quoted: nil
;; End:
;;; elisp-autofmt.el ends here
