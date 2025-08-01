;;; plantuml-mode.el --- Major mode for PlantUML    -*- lexical-binding: t; -*-

;; Filename: plantuml-mode.el
;; Description: Major mode for PlantUML diagrams sources
;; Compatibility: Tested with Emacs 25 through 27 (current master)
;; Author: Zhang Weize (zwz)
;; Maintainer: Carlo Sciolla (skuro)
;; Keywords: files text processes tools
;; Package-Version: 1.8.0
;; Package-Revision: v1.8.0-0-g0eaf340303cf
;; Package-Requires: ((dash "2.0.0") (emacs "25.1") (deflate "0.0.3"))
;; Homepage: https://github.com/skuro/plantuml-mode

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A major mode for plantuml, see: http://plantuml.sourceforge.net/
;; Plantuml is an open-source tool in java that allows to quickly write :
;;     - sequence diagram,
;;     - use case diagram,
;;     - class diagram,
;;     - activity diagram,
;;     - component diagram,
;;     - state diagram
;;     - object diagram

;;; Change log:
;;
;; version 1.8.0, 2025-07-04 Support for `'hex' and `'deflate' modes for server URL encoding
;; version 1.7.0, 2025-05-24 Support for `completion-at-point'
;; version 1.6.0, 2025-05-15 Fix server exec mode; various indentation enhancements and bug fixes; better preview buffer management
;; version 1.5.0, 2025-05-14 Fixed warnings with new Java versions #157; updated versions to let CI work again
;; version 1.4.1, 2019-09-03 Better indentation; more bugfixing; actually adding `executable' mode
;; version 1.4.0, 2019-08-21 Added `executable' exec mode to use locally installed `plantuml' binaries, various bugfixes
;; version 1.3.1, 2019-08-02 Fixed interactive behavior of `plantuml-set-exec-mode'
;; version 1.3.0, 2019-05-31 Added experimental support for multiple rendering modes and, specifically, preview using a PlantUML server
;; version 1.2.11, 2019-04-09 Added `plantuml-download-jar'
;; version 1.2.10, 2019-04-03 Avoid messing with window layouts and buffers -- courtesy of https://github.com/wailo
;; version 1.2.9, Revamped indentation support, now working with a greater number of keywords
;; version 1.2.8, 2019-01-07 Support indentation for activate / deactivate blocks; allow customization of `plantuml-java-args'
;; version 1.2.7, 2018-08-15 Added support for indentation; Fixed the comiling error when installing with melpa
;; version 1.2.6, 2018-07-17 Introduced custom variable `plantuml-jar-args' to control which arguments are passed to PlantUML jar. Fix the warning of failing to specify types of 'defcustom' variables
;; version 1.2.5, 2017-08-19 #53 Fixed installation warnings
;; version 1.2.4, 2017-08-18 #60 Licensed with GPLv3+ to be compatible with Emacs
;; version 1.2.3, 2016-12-25 #50 unicode support in generated output
;; version 1.2.2, 2016-11-11 Fixed java commands handling under windows; support spaces in `plantuml-jar-path'
;; version 1.2.1, 2016-11-11 Support for paths like `~/.plantuml/plantuml.jar' for `plantuml-jar-path' (the tilde was previously unsupported)
;; version 1.2.0, 2016-11-09 Added `plantuml-preview-current-buffer', courtesy of @7mamu4
;; version 1.1.1, 2016-11-08 Fix process handling with Windows native emacs; better file extention match for autoloading the mode
;; version 1.1.0, 2016-10-18 Make PlantUML run headless by default; introduced custom variable `plantuml-java-args' to control which arguments are passed to Plantuml.
;; version 1.0.1, 2016-10-17 Bugfix release: proper auto-mode-alist regex; init delayed at mode load; avoid calling hooks twice.
;; version 1.0.0, 2016-10-16 Moved the mode to plantuml-mode, superseding zwz/plantuml-mode and skuro/puml-mode. Added preview for the currently selected region.
;; version 0.6.7, 2016-10-11 [from puml-mode] Added deprecation warning in favor of plantuml-mode
;; version 0.6.6, 2016-07-19 [from puml-mode] Added autoload, minor bug fixes
;; version 0.6.5, 2016-03-24 [from puml-mode] Added UTF8 support and open in new window / frame shortcuts
;; version 0.6.4, 2015-12-12 [from puml-mode] Added support for comments (single and multiline) -- thanks to https://github.com/nivekuil
;; version 0.6.3, 2015-11-07 [from puml-mode] Added per-buffer configurability of output type (thanks to https://github.com/davazp)
;; version 0.6.2, 2015-11-07 [from puml-mode] Added debugging capabilities to improve issue analysis
;; version 0.6.1, 2015-09-26 [from puml-mode] Bugfix: use eq to compare symbols instead of cl-equalp
;; version 0.6, 2015-09-26 [from puml-mode] Fixed PNG preview
;; version 0.5, 2015-09-21 [from puml-mode] Added preview capabilities
;; version 0.4, 2015-06-14 [from puml-mode] Use a puml- prefix to distinguish from the other plantuml-mode
;; version 0.3, 2015-06-13 [from puml-mode] Compatibility with Emacs 24.x
;; version 0.2, 2010-09-20 [from puml-mode] Initialize the keywords from the -language output of plantuml.jar instead of the hard-coded way.
;; version 0.1, 2010-08-25 [from puml-mode] First version

;;; Code:
(require 'cl-lib)
(require 'dash)
(require 'deflate)
(require 'thingatpt)
(require 'xml)

(defgroup plantuml nil  "Major mode for editing plantuml file."
  :group 'languages)

(defcustom plantuml-jar-path
  (expand-file-name "~/plantuml.jar")
  "The location of the PlantUML executable JAR."
  :type 'string
  :group 'plantuml)

(defcustom plantuml-executable-path
  "plantuml"
  "The location of the PlantUML executable."
  :type 'string
  :group 'plantuml)

(defvar plantuml-mode-hook nil "Standard hook for plantuml-mode.")

(defconst plantuml-mode-version "1.5.0" "The plantuml-mode version string.")

(defvar plantuml-mode-debug-enabled nil)

(defvar plantuml-font-lock-keywords nil)

(defvar plantuml-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c C-c") 'plantuml-preview)
    keymap)
  "Keymap for plantuml-mode.")

(defcustom plantuml-java-command "java"
  "The java command used to execute PlantUML."
  :type 'string
  :group 'plantuml)

(defcustom plantuml-java-args (list "-Djava.awt.headless=true" "-jar" "--illegal-access=deny")
  "The parameters passed to `plantuml-java-command' when executing PlantUML."
  :type '(repeat string)
  :group 'plantuml)

(defcustom plantuml-jar-args (list "-charset" "UTF-8" )
  "The parameters passed to `plantuml.jar', when executing PlantUML."
  :type '(repeat string)
  :group 'plantuml)

(defcustom plantuml-server-url "https://www.plantuml.com/plantuml"
  "The base URL of the PlantUML server."
  :type 'string
  :group 'plantuml)

(defcustom plantuml-executable-args (list "-headless")
  "The parameters passed to plantuml executable when executing PlantUML."
  :type '(repeat string)
  :group 'plantuml)

(defcustom plantuml-default-exec-mode 'server
  "Default execution mode for PlantUML.  Valid values are:
- `jar': run PlantUML as a JAR file
- `server': contact the PlantUML server at `plantuml-server-url'
- `executable' run the PlantUML executable at `plantuml-executable-path'

The `jar' exec mode requires a local install of the PlantUML JAR file,
see `plantuml-jar-path'.

The `executable' exec mode requires a local install of the PlantUML JAR file,
see `plantuml-executable-path'."
  :type 'symbol
  :group 'plantuml
  :options '(jar server executable))

(defcustom plantuml-suppress-deprecation-warning t
  "To silence the deprecation warning when `puml-mode' is found upon loading."
  :type 'boolean
  :group 'plantuml)

(defcustom plantuml-indent-level 8
  "Indentation level of PlantUML lines."
  :type 'natnum
  :group 'plantuml)

(defcustom plantuml-preview-default-theme nil
  "Sets the default theme to use when rendering diagrams.
Works only if `!theme' does not appear  in the diagram to be displayed."
  :type 'string
  :group 'plantuml
  :safe #'stringp)

(defcustom plantuml-server-encode-mode 'deflate
  "Whether to encode the server URL using HEX or DEFLATE."
  :type 'symbol
  :group 'plantuml
  :options '(deflate hex))

(defun plantuml-jar-render-command (&rest arguments)
  "Create a command line to execute PlantUML with arguments (as ARGUMENTS)."
  (let* ((cmd-list (append plantuml-java-args (list (expand-file-name plantuml-jar-path)) plantuml-jar-args arguments))
         (cmd (mapconcat #'identity cmd-list "|")))
    (plantuml-debug (format "Command is [%s]" cmd))
    cmd-list))

;;; syntax table
(defvar plantuml-mode-syntax-table
  (let ((synTable (make-syntax-table)))
    (modify-syntax-entry ?\/  ". 14c"   synTable)
    (modify-syntax-entry ?'   "< 23"    synTable)
    (modify-syntax-entry ?\n  ">"       synTable)
    (modify-syntax-entry ?\r  ">"       synTable)
    (modify-syntax-entry ?!   "w"       synTable)
    (modify-syntax-entry ?@   "w"       synTable)
    (modify-syntax-entry ?#   "'"       synTable)
    synTable)
  "Syntax table for `plantuml-mode'.")

(defvar plantuml-types nil)
(defvar plantuml-keywords nil)
(defvar plantuml-preprocessors nil)
(defvar plantuml-builtins nil)

;; keyword completion
(defvar plantuml-kwdList nil "The plantuml keywords.")

;; PlantUML execution mode
(defvar-local plantuml-exec-mode nil
  "The Plantuml execution mode override.
See `plantuml-default-exec-mode' for acceptable values.")

(defun plantuml-set-exec-mode (mode)
  "Set the execution mode MODE for PlantUML."
  (interactive (let* ((completion-ignore-case t)
                      (supported-modes        '("jar" "server" "executable")))
                 (list (completing-read (format "Exec mode [%s]: " plantuml-exec-mode)
                                        supported-modes
                                        nil
                                        t
                                        nil
                                        nil
                                        plantuml-exec-mode))))
  (if (member mode '("jar" "server" "executable"))
      (setq plantuml-exec-mode (intern mode))
    (error (concat "Unsupported mode:" mode))))

(defun plantuml-get-exec-mode ()
  "Retrieves the currently active PlantUML exec mode."
  (or plantuml-exec-mode
      plantuml-default-exec-mode))

(defun plantuml-enable-debug ()
  "Enables debug messages into the *PLANTUML Messages* buffer."
  (interactive)
  (setq plantuml-mode-debug-enabled t))

(defun plantuml-disable-debug ()
  "Stops any debug messages to be added into the *PLANTUML Messages* buffer."
  (interactive)
  (setq plantuml-mode-debug-enabled nil))

(defun plantuml-debug (msg)
  "Writes MSG into the *PLANTUML Messages* buffer without annoying the user."
  (if plantuml-mode-debug-enabled
      (let* ((log-buffer-name "*PLANTUML Messages*")
             (log-buffer (get-buffer-create log-buffer-name)))
        (save-excursion
          (with-current-buffer log-buffer
            (goto-char (point-max))
            (insert msg)
            (insert "\n"))))))

(defun plantuml-download-jar ()
  "Download the latest PlantUML JAR file and install it into `plantuml-jar-path'."
  (interactive)
  (if (y-or-n-p (format "Download the latest PlantUML JAR file into %s? " plantuml-jar-path))
      (if (or (not (file-exists-p plantuml-jar-path))
              (y-or-n-p (format "The PlantUML jar file already exists at %s, overwrite? " plantuml-jar-path)))
          (with-current-buffer (url-retrieve-synchronously "https://search.maven.org/solrsearch/select?q=g:net.sourceforge.plantuml+AND+a:plantuml&core=gav&start=0&rows=1&wt=xml")
            (mkdir (file-name-directory plantuml-jar-path) t)
            (let* ((parse-tree (xml-parse-region))
                   (doc        (->> parse-tree
                                    (assq 'response)
                                    (assq 'result)
                                    (assq 'doc)))
                   (strs       (xml-get-children doc 'str))
                   (version    (->> strs
                                    (--filter (string-equal "v" (xml-get-attribute it 'name)))
                                    (car)
                                    (xml-node-children)
                                    (car))))
              (message (concat "Downloading PlantUML v" version " into " plantuml-jar-path))
              (url-copy-file (format "https://search.maven.org/remotecontent?filepath=net/sourceforge/plantuml/plantuml/%s/plantuml-%s.jar" version version) plantuml-jar-path t)
              (kill-buffer)))
        (message "Aborted."))
    (message "Aborted.")))

(defun plantuml-jar-java-version ()
  "Inspects the Java runtime version of the configured Java command.
The actual command is taken from in `plantuml-java-command'."
  (save-excursion
    (save-match-data
      (with-temp-buffer
        (call-process plantuml-java-command nil t nil "-XshowSettings:properties" "-version")
        (re-search-backward "java.version = \\(1.\\)?\\([[:digit:]]+\\)")
        (string-to-number (match-string 2))))))

(defun plantuml-jar-get-language (buf)
  "Retrieve the language specification from the PlantUML JAR file.
The language spec is pasted into the buffer  BUF."
  (unless (or (eq system-type 'cygwin) (file-exists-p plantuml-jar-path))
    (error "Could not find plantuml.jar at %s" plantuml-jar-path))
  (with-current-buffer buf
    (let ((cmd-args (append (list plantuml-java-command nil t nil)
                            (plantuml-jar-render-command "-language"))))
      (apply #'call-process cmd-args)
      (goto-char (point-min)))))

(defun plantuml-server-get-language (buf)
  "Retrieve the language specification from the PlantUML server.
The language spec is pasted into the buffer  BUF."
  (let ((lang-url (concat plantuml-server-url "/language")))
    (with-current-buffer buf
      (url-insert-file-contents lang-url))))

(defun plantuml-executable-get-language (buf)
  "Retrieve the language specification from the PlantUML executable.
The language spec is pasted into the buffer  BUF."
  (with-current-buffer buf
    (let ((cmd-args (append (list plantuml-executable-path nil t nil) (list "-language"))))
      (apply #'call-process cmd-args)
      (goto-char (point-min)))))

(defun plantuml-get-language (mode buf)
  "Retrieve the language spec using the preferred PlantUML execution mode MODE.
Paste the result into BUF."
  (let ((get-fn (pcase mode
                  ('jar #'plantuml-jar-get-language)
                  ('server #'plantuml-server-get-language)
                  ('executable #'plantuml-executable-get-language))))
    (if get-fn
        (funcall get-fn buf)
      (error "Unsupported execution mode %s" mode))))

(defun plantuml-init (mode)
  "Initialize the keywords or builtins from the cmdline language output.
Use exec mode MODE to load the language details."
  (with-temp-buffer
    (plantuml-get-language mode (current-buffer))
    (let ((found (search-forward ";" nil t))
          (word "")
          (count 0)
          (pos 0))
      (while found
        (forward-char)
        (setq word (current-word))
        (if (string= word "EOF") (setq found nil)
          ;; else
          (forward-line)
          (setq count (string-to-number (current-word)))
          (beginning-of-line 2)
          (setq pos (point))
          (forward-line count)
          (cond ((string= word "type")
                 (setq plantuml-types
                       (split-string
                        (buffer-substring-no-properties pos (point)))))
                ((string= word "keyword")
                 (setq plantuml-keywords
                       (split-string
                        (buffer-substring-no-properties pos (point)))))
                ((string= word "preprocessor")
                 (setq plantuml-preprocessors
                       (split-string
                        (buffer-substring-no-properties pos (point)))))
                (t (setq plantuml-builtins
                         (append
                          plantuml-builtins
                          (split-string
                           (buffer-substring-no-properties pos (point)))))))
          (setq found (search-forward ";" nil nil)))))))

(defconst plantuml-preview-buffer "*PLANTUML Preview*")

(defvar plantuml-output-type
  (if (not (display-images-p))
      "txt"
    (cond ((image-type-available-p 'svg) "svg")
          ((image-type-available-p 'png) "png")
          (t "txt")))
  "Specify the desired output type to use for generated diagrams.")

(defun plantuml-read-output-type ()
  "Read from the minibuffer a output type."
  (let* ((completion-ignore-case t)
         (available-types
          (append
           (and (image-type-available-p 'svg) '("svg"))
           (and (image-type-available-p 'png) '("png"))
           '("txt"))))
    (completing-read (format "Output type [%s]: " plantuml-output-type)
                     available-types
                     nil
                     t
                     nil
                     nil
                     plantuml-output-type)))

(defun plantuml-set-output-type (type)
  "Set the desired output TYPE for the current buffer.
If the
major mode of the current buffer mode is not plantuml-mode, set the
default output type for new buffers."
  (interactive (list (plantuml-read-output-type)))
  (setq plantuml-output-type type))

(defun plantuml-is-image-output-p ()
  "Return t if `plantuml-output-type' denotes an image, nil if it's text based."
  (not (equal "txt" plantuml-output-type)))

(defun plantuml-jar-output-type-opt (output-type)
  "Create the flag to pass to PlantUML according to OUTPUT-TYPE.
Note that output type `txt' is promoted to `utxt' for better rendering."
  (concat "-t" (pcase output-type
                 ("txt" "utxt")
                 (_     output-type))))

(defun plantuml-jar-start-process (buf)
  "Run the PlantUML JAR and puts the output into the given buffer BUF."
  (let ((java-args (if (<= 8 (plantuml-jar-java-version))
                       (remove "--illegal-access=deny" plantuml-java-args)
                     plantuml-java-args)))
    (apply #'start-process
           "PLANTUML" buf plantuml-java-command
           `(,@java-args
             ,(expand-file-name plantuml-jar-path)
             ,(plantuml-jar-output-type-opt plantuml-output-type)
             ,@plantuml-jar-args
             "-p"))))

(defun plantuml-executable-start-process (buf)
  "Run the PlantUML executable and puts the output into the given buffer BUF."
  (apply #'start-process
         "PLANTUML" buf plantuml-executable-path
         `(,@plantuml-executable-args
           ,(plantuml-jar-output-type-opt plantuml-output-type)
           "-p")))

(defun plantuml-update-preview-buffer (prefix buf)
  "Show the preview in the preview buffer BUF.
Window is selected according to PREFIX:
- 4  (when prefixing the command with C-u) -> new window
- 16 (when prefixing the command with C-u C-u) -> new frame.
- else -> new buffer"
  (let ((imagep (and (display-images-p)
                     (plantuml-is-image-output-p))))
    (cond
     ((= prefix 16) (switch-to-buffer-other-frame buf))
     ((= prefix 4)  (switch-to-buffer-other-window buf))
     (t             (display-buffer buf)))
    (when imagep
      (with-current-buffer buf
        (image-mode)
        (set-buffer-multibyte t)))
    (set-window-point (get-buffer-window buf 'visible) (point-min))))

(defun plantuml-jar-preview-string (prefix string buf)
  "Preview the diagram from STRING by running the PlantUML JAR.
Put the result into buffer BUF.  Window is selected according to PREFIX:
- 4  (when prefixing the command with C-u) -> new window
- 16 (when prefixing the command with C-u C-u) -> new frame.
- else -> new buffer"
  (let* ((process-connection-type nil)
         (ps (plantuml-jar-start-process buf)))
    (process-send-string ps string)
    (process-send-eof ps)
    (set-process-sentinel ps
                          (lambda (_ps event)
                            (unless (equal event "finished\n")
                              (error "PLANTUML Preview failed: %s" event))
                            (plantuml-update-preview-buffer prefix buf)))))

(defun plantuml-server-hex-encode-url (string)
  "HEX-encode STRING into a URL suitable for PlantUML server interactions."
  (let* ((coding-system (or buffer-file-coding-system
                            'utf-8))
         (str (encode-coding-string string coding-system))
         (encoded-string (mapconcat (lambda(x)(format "%02X" x)) str)))
    (concat plantuml-server-url "/" plantuml-output-type "/~h" encoded-string)))

(defconst plantuml-server-base64-char-table
  (let ((translation-table (make-char-table 'translation-table))
        (base64-chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=")
        (plantuml-chars "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-_="))
    (dotimes (i (length base64-chars))
      (aset translation-table
            (aref base64-chars i)
            (aref plantuml-chars i)))
    translation-table))

(defun plantuml-server-deflate-encode-url (string)
  "DEFLATE-encode STRING into a URL suitable for PlantUML server interactions."
  (let* ((compressed-bytes (deflate-zlib-compress string 'dynamic))
         (base64-encoded (base64-encode-string (apply #'unibyte-string compressed-bytes))))
    (with-temp-buffer
      (insert base64-encoded)
      (translate-region (point-min) (point-max) plantuml-server-base64-char-table)
      (concat plantuml-server-url "/" plantuml-output-type "/~1" (buffer-string)))))

(defun plantuml-server-encode-url (string)
  "Encode STRING into a URL suitable for PlantUML server interactions."
  (let ((encode-mode (or plantuml-server-encode-mode 'deflate)))
    (cl-case encode-mode
      (deflate (plantuml-server-deflate-encode-url string))
      (hex (plantuml-server-hex-encode-url string)))))

(defun plantuml-server-preview-string (prefix string buf)
  "Preview the diagram from STRING as rendered by the PlantUML server.
Put the result into buffer BUF and place it according to PREFIX:
- 4  (when prefixing the command with C-u) -> new window
- 16 (when prefixing the command with C-u C-u) -> new frame.
- else -> new buffer"
  (let* ((url-request-location (plantuml-server-encode-url string)))
    (let* ((response-buf (url-retrieve-synchronously url-request-location)))
      (save-current-buffer
        (save-match-data
          (with-current-buffer response-buf
            (set-buffer-multibyte t)
            (decode-coding-region (point-min) (point-max) 'utf-8)
            (goto-char (point-min))
            (while (not (looking-at "\n"))
              (forward-line))
            (kill-region (point-min) (+ 1 (point)))
            (copy-to-buffer buf (point-min) (point-max))
            (plantuml-update-preview-buffer prefix buf)))))))

(defun plantuml-executable-preview-string (prefix string buf)
  "Preview the diagram from STRING by running the PlantUML JAR.
Put the result into buffer BUF.  Window is selected according to PREFIX:
- 4  (when prefixing the command with C-u) -> new window
- 16 (when prefixing the command with C-u C-u) -> new frame.
- else -> new buffer"
  (let* ((process-connection-type nil)
         (ps (plantuml-executable-start-process buf)))
    (process-send-string ps string)
    (process-send-eof ps)
    (set-process-sentinel ps
                          (lambda (_ps event)
                            (unless (equal event "finished\n")
                              (error "PLANTUML Preview failed: %s" event))
                            (plantuml-update-preview-buffer prefix buf)))))

(defun plantuml-exec-mode-preview-string (prefix mode string buf)
  "Preview the diagram from STRING using the execution mode MODE.
Put the result into buffer BUF, selecting the window according to PREFIX:
- 4  (when prefixing the command with C-u) -> new window
- 16 (when prefixing the command with C-u C-u) -> new frame.
- else -> new buffer"
  (let ((preview-fn (pcase mode
                      ('jar    #'plantuml-jar-preview-string)
                      ('server #'plantuml-server-preview-string)
                      ('executable #'plantuml-executable-preview-string))))
    (if preview-fn
        (funcall preview-fn prefix string buf)
      (error "Unsupported execution mode %s" mode))))

(defun plantuml-themed-p (string)
  "Return non-nil if STRING is a PlantUML source with explicit theme directive."
  ;; check for beginning of line with word boundary
  (string-match-p "^\\s-*!theme\\b" string))

(defun plantuml-set-theme (string theme)
  "Add the THEME to the diagram STRING."
  (replace-regexp-in-string "^@startuml"
                            (concat "@startuml\n!theme " theme)
                            string))

(defun plantuml-preview-string (prefix string)
  "Preview diagram from PlantUML sources (as STRING), using prefix (as PREFIX)
to choose where to display it.
Put the result into buffer BUF, selecting the window according to PREFIX:
- 4  (when prefixing the command with C-u) -> new window
- 16 (when prefixing the command with C-u C-u) -> new frame.
- else -> new buffer"
  (when-let ((b (get-buffer plantuml-preview-buffer))
             (inhibit-read-only t))
    (with-current-buffer b
      (erase-buffer)))

  (let* ((imagep (and (display-images-p)
                      (plantuml-is-image-output-p)))
         (buf (get-buffer-create plantuml-preview-buffer))
         (coding-system-for-read (and imagep 'binary))
         (coding-system-for-write (and imagep 'binary))
         (themed (plantuml-themed-p string)))
    (if (and (not (plantuml-themed-p string))
             plantuml-preview-default-theme)
        ;; override the theme
        (plantuml-exec-mode-preview-string prefix
                                           (plantuml-get-exec-mode)
                                           (plantuml-set-theme string plantuml-preview-default-theme)
                                           buf)
      (plantuml-exec-mode-preview-string prefix
                                         (plantuml-get-exec-mode)
                                         string
                                         buf))))

(defun plantuml-preview-buffer (prefix)
  "Preview diagram from the PlantUML sources in the current buffer.
Uses prefix (as PREFIX) to choose where to display it:
- 4  (when prefixing the command with C-u) -> new window
- 16 (when prefixing the command with C-u C-u) -> new frame.
- else -> new buffer"
  (interactive "p")
  (plantuml-preview-string prefix (buffer-string)))

(defun plantuml-preview-region (prefix begin end)
  "Preview diagram from the PlantUML sources in from BEGIN to END.
Uses the current region when called interactively.
Uses prefix (as PREFIX) to choose where to display it:
- 4  (when prefixing the command with C-u) -> new window
- 16 (when prefixing the command with C-u C-u) -> new frame.
- else -> new buffer"
  (interactive "p\nr")
  (plantuml-preview-string prefix (concat "@startuml\n"
                                          (buffer-substring-no-properties
                                           begin end)
                                          "\n@enduml")))

(defun plantuml-preview-current-block (prefix)
  "Preview diagram from the PlantUML sources for the current block.
The block is defined as starting from the previous @startuml to the next
@enduml.  Uses prefix (as PREFIX) to choose where to display it:
- 4  (when prefixing the command with C-u) -> new window
- 16 (when prefixing the command with C-u C-u) -> new frame.
- else -> new buffer"
  (interactive "p")
  (save-restriction
    (narrow-to-region
     (search-backward "@startuml") (search-forward "@enduml"))
    (plantuml-preview-buffer prefix)))

(defun plantuml-preview (prefix)
  "Preview diagram from the PlantUML sources.
Uses the current region if one is active, or the entire buffer otherwise.
Uses prefix (as PREFIX) to choose where to display it:
- 4  (when prefixing the command with C-u) -> new window
- 16 (when prefixing the command with C-u C-u) -> new frame.
- else -> new buffer"
  (interactive "p")
  (if mark-active
      (plantuml-preview-region prefix (region-beginning) (region-end))
    (plantuml-preview-buffer prefix)))

(defun plantuml-deprecation-warning ()
  "Warns the user about the deprecation of the `puml-mode' project."
  (if (and plantuml-suppress-deprecation-warning
           (featurep 'puml-mode))
      (display-warning :warning
                       "`puml-mode' is now deprecated and no longer updated, but it's still present in your system. \
You should move your configuration to use `plantuml-mode'. \
See more at https://github.com/skuro/puml-mode/issues/26")))

(defun plantuml-init-once (&optional mode)
  "Ensure initialization only happens once.
Use exec mode MODE to load the language details
or by first querying `plantuml-get-exec-mode'."
  (plantuml-deprecation-warning)
  (let ((mode (or mode (plantuml-get-exec-mode))))
    (unless plantuml-kwdList
      (plantuml-init mode)
      (defvar plantuml-types-regexp (concat "^\\s *\\(" (regexp-opt plantuml-types 'words) "\\|\\<\\(note\\s +over\\|note\\s +\\(left\\|right\\|bottom\\|top\\)\\s +\\(of\\)?\\)\\>\\|\\<\\(\\(left\\|center\\|right\\)\\s +\\(header\\|footer\\)\\)\\>\\)"))
      (defvar plantuml-keywords-regexp (concat "^\\s *" (regexp-opt plantuml-keywords 'words)  "\\|\\(<\\|<|\\|\\*\\|o\\)\\(\\.+\\|-+\\)\\|\\(\\.+\\|-+\\)\\(>\\||>\\|\\*\\|o\\)\\|\\.\\{2,\\}\\|-\\{2,\\}"))
      (defvar plantuml-builtins-regexp (regexp-opt plantuml-builtins 'words))
      (defvar plantuml-preprocessors-regexp (concat "^\\s *" (regexp-opt plantuml-preprocessors 'words)))

      ;; Below are the regexp's for indentation.
      ;; Notes:
      ;; - there is some control on what it is indented by overriding some of below
      ;;   X-start and X-end regexp before plantuml-mode is loaded. E.g., to disable
      ;;   indentation on activate, you might define in your .emacs something like
      ;;      (setq plantuml-indent-regexp-activate-start
      ;;         "NEVER MATCH THIS EXPRESSION"); define _before_ load plantuml-mode!
      ;;      (setq plantuml-indent-regexp-activate-end
      ;;          "NEVER MATCH THIS EXPRESSION"); define _before_ load plantuml-mode!
      ;; - due to the nature of using (context-insensitive) regexp, indentation have
      ;;   following limitations
      ;;   - commands commented out by /' ... '/ will _not_ be ignored
      ;;     and potentially lead to miss-indentation
      ;; - you can though somewhat correct mis-indentation by adding in '-comment lines
      ;;   PLANTUML_MODE_INDENT_INCREASE and/or PLANTUML_MODE_INDENT_DECREASE
      ;;   to increase and/or decrease the level of indentation
      ;;   (Note: the line with the comment should not contain any text matching other indent
      ;;    regexp or this user-control instruction will be ignored; also at most will count
      ;;    per line ...)
      (defvar plantuml-indent-regexp-block-start "^.*{\s*$"
        "Indentation regex for all plantuml elements that might define a {} block.
Plantuml elements like skinparam, rectangle, sprite, package, etc.
The opening { has to be the last visible character in the line (whitespace
might follow).")
      (defvar plantuml-indent-regexp-note-start "^\s*\\(floating\s+\\)?[hr]?note\s+\\(right\\|left\\|top\\|bottom\\|over\\|as\\)[^:]*\\(\\:\\:[^:]+\\)?$" "simplyfied regex; note syntax is especially inconsistent across diagrams")
      (defvar plantuml-indent-regexp-group-start "^\s*\\(alt\\|else\\|opt\\|loop\\|par\\|break\\|critical\\|group\\)\\(?:\s+.+\\|$\\)"
        "Indentation regex for plantuml group elements  defined for sequence diagrams.
Two variants for groups: keyword is either followed by whitespace and some text
or it is followed by line end.")
      (defvar plantuml-indent-regexp-activate-start "^\s*activate\s+.+$")
      (defvar plantuml-indent-regexp-box-start "^\s*box\s+.+$")
      (defvar plantuml-indent-regexp-ref-start "^\s*ref\s+over\s+[^:]+?$")
      (defvar plantuml-indent-regexp-title-start "^\s*title\s*\\('.*\\)?$")
      (defvar plantuml-indent-regexp-header-start "^\s*\\(?:\\(?:center\\|left\\|right\\)\s+header\\|header\\)\s*\\('.*\\)?$")
      (defvar plantuml-indent-regexp-footer-start "^\s*\\(?:\\(?:center\\|left\\|right\\)\s+footer\\|footer\\)\s*\\('.*\\)?$")
      (defvar plantuml-indent-regexp-legend-start "^\s*\\(?:legend\\|legend\s+\\(?:bottom\\|top\\)\\|legend\s+\\(?:center\\|left\\|right\\)\\|legend\s+\\(?:bottom\\|top\\)\s+\\(?:center\\|left\\|right\\)\\)\s*\\('.*\\)?$")
      (defvar plantuml-indent-regexp-oldif-start "^.*if\s+\".*\"\s+then\s*\\('.*\\)?$"
        "used in current activity diagram, sometimes already mentioned as deprecated")
      (defvar plantuml-indent-regexp-newif-start "^\s*\\(?:else\\)?if\s+(.*)\s+then\s*.*$")
      (defvar plantuml-indent-regexp-loop-start "^\s*\\(?:repeat\s*\\|while\s+(.*).*\\)$")
      (defvar plantuml-indent-regexp-fork-start "^\s*\\(?:fork\\|split\\)\\(?:\s+again\\)?\s*$")
      (defvar plantuml-indent-regexp-case-start "^\s*\\(?:switch\\|case\\)\s-*(.*)\s*$")
      (defvar plantuml-indent-regexp-macro-start "^\s*!definelong.*$")
      (defvar plantuml-indent-regexp-user-control-start "^.*'.*\s*PLANTUML_MODE_INDENT_INCREASE\s*.*$")
      (defvar plantuml-indent-regexp-start (list plantuml-indent-regexp-block-start
                                                 plantuml-indent-regexp-group-start
                                                 plantuml-indent-regexp-activate-start
                                                 plantuml-indent-regexp-box-start
                                                 plantuml-indent-regexp-ref-start
                                                 plantuml-indent-regexp-legend-start
                                                 plantuml-indent-regexp-note-start
                                                 plantuml-indent-regexp-newif-start
                                                 plantuml-indent-regexp-loop-start
                                                 plantuml-indent-regexp-fork-start
                                                 plantuml-indent-regexp-case-start
                                                 plantuml-indent-regexp-title-start
                                                 plantuml-indent-regexp-header-start
                                                 plantuml-indent-regexp-footer-start
                                                 plantuml-indent-regexp-macro-start
                                                 plantuml-indent-regexp-oldif-start
                                                 plantuml-indent-regexp-user-control-start))
      (defvar plantuml-indent-regexp-block-end "^\s*\\(?:}\\|endif\\|else\s*.*\\|end\\)\s*\\('.*\\)?$")
      (defvar plantuml-indent-regexp-note-end "^\s*\\(end\s+note\\|end[rh]note\\)\s*\\('.*\\)?$")
      (defvar plantuml-indent-regexp-group-end "^\s*end\s*\\('.*\\)?$")
      (defvar plantuml-indent-regexp-activate-end "^\s*deactivate\s+.+$")
      (defvar plantuml-indent-regexp-box-end "^\s*end\s+box\s*\\('.*\\)?$")
      (defvar plantuml-indent-regexp-ref-end "^\s*end\s+ref\s*\\('.*\\)?$")
      (defvar plantuml-indent-regexp-title-end "^\s*end\s+title\s*\\('.*\\)?$")
      (defvar plantuml-indent-regexp-header-end "^\s*endheader\s*\\('.*\\)?$")
      (defvar plantuml-indent-regexp-footer-end "^\s*endfooter\s*\\('.*\\)?$")
      (defvar plantuml-indent-regexp-legend-end "^\s*endlegend\s*\\('.*\\)?$")
      (defvar plantuml-indent-regexp-oldif-end "^\s*\\(endif\\|else\\)\s*\\('.*\\)?$")
      (defvar plantuml-indent-regexp-newif-end "^\s*\\(endif\\|elseif\\|else\\)\s*.*$")
      (defvar plantuml-indent-regexp-loop-end "^\s*\\(repeat\s*while\\|endwhile\\)\s*.*$")
      (defvar plantuml-indent-regexp-fork-end "^\s*\\(\\(fork\\|split\\)\s+again\\|end\s+\\(fork\\|split\\)\\)\s*\\(\{.*\}\\)?\s*$")
      (defvar plantuml-indent-regexp-case-end "^\s*\\(case\s-*(.*)\\|endswitch\\)\s*\\('.*\\)?$")
      (defvar plantuml-indent-regexp-macro-end "^\s*!enddefinelong\s*\\('.*\\)?$")
      (defvar plantuml-indent-regexp-user-control-end "^.*'.*\s*PLANTUML_MODE_INDENT_DECREASE\s*.*$")
      (defvar plantuml-indent-regexp-end (list plantuml-indent-regexp-block-end
                                               plantuml-indent-regexp-group-end
                                               plantuml-indent-regexp-activate-end
                                               plantuml-indent-regexp-box-end
                                               plantuml-indent-regexp-ref-end
                                               plantuml-indent-regexp-legend-end
                                               plantuml-indent-regexp-note-end
                                               plantuml-indent-regexp-newif-end
                                               plantuml-indent-regexp-loop-end
                                               plantuml-indent-regexp-fork-end
                                               plantuml-indent-regexp-case-end
                                               plantuml-indent-regexp-title-end
                                               plantuml-indent-regexp-header-end
                                               plantuml-indent-regexp-footer-end
                                               plantuml-indent-regexp-macro-end
                                               plantuml-indent-regexp-oldif-end
                                               plantuml-indent-regexp-user-control-end))
      (setq plantuml-font-lock-keywords
            `(
              (,plantuml-types-regexp . font-lock-type-face)
              (,plantuml-keywords-regexp . font-lock-keyword-face)
              (,plantuml-builtins-regexp . font-lock-builtin-face)
              (,plantuml-preprocessors-regexp . font-lock-preprocessor-face)
              ;; note: order matters
              ))

      (setq plantuml-kwdList (make-hash-table :test 'equal))
      (mapc (lambda (x) (puthash x t plantuml-kwdList)) plantuml-types)
      (mapc (lambda (x) (puthash x t plantuml-kwdList)) plantuml-keywords)
      (mapc (lambda (x) (puthash x t plantuml-kwdList)) plantuml-builtins)
      (mapc (lambda (x) (puthash x t plantuml-kwdList)) plantuml-preprocessors)
      (put 'plantuml-kwdList 'risky-local-variable t)

      ;; clear memory
      (setq plantuml-types nil)
      (setq plantuml-keywords nil)
      (setq plantuml-builtins nil)
      (setq plantuml-preprocessors nil)
      (setq plantuml-types-regexp nil)
      (setq plantuml-keywords-regexp nil)
      (setq plantuml-builtins-regexp nil)
      (setq plantuml-preprocessors-regexp nil))))

(defun plantuml-complete-symbol ()
  "Perform keyword completion on word before cursor."
  (interactive)
  (let ((posEnd (point))
        (meat (thing-at-point 'symbol))
        maxMatchResult)

    (when (not meat) (setq meat ""))

    (setq maxMatchResult (try-completion meat plantuml-kwdList))
    (cond ((eq maxMatchResult t))
          ((null maxMatchResult)
           (message "Can't find completion for \"%s\"" meat)
           (ding))
          ((not (string= meat maxMatchResult))
           (delete-region (- posEnd (length meat)) posEnd)
           (insert maxMatchResult))
          (t (message "Making completion list...")
             (with-output-to-temp-buffer "*Completions*"
               (display-completion-list
                (all-completions meat plantuml-kwdList)))
             (message "Making completion list...%s" "done")))))

(make-obsolete 'plantuml-complete-symbol
               "Use `completion-at-point' (C-M-i) instead"
               "1.7.0")

(defun plantuml-completion-at-point-function ()
  "Complete symbol at point using `plantuml-kwdList'.
See `completion-at-point-functions'."
  (let ((thing-start (beginning-of-thing 'symbol))
        (thing-end (end-of-thing 'symbol)))

    (list thing-start
          thing-end
          plantuml-kwdList
          '(:exclusive no))))


;; indentation


(defun plantuml-current-block-depth ()
  "Trace the current block indentation level by looking back line by line."
  (save-excursion
    (let ((relative-depth 0))
      ;; current line
      (beginning-of-line)
      (if (-any? 'looking-at plantuml-indent-regexp-end)
          (setq relative-depth (1- relative-depth)))

      ;; from current line backwards to beginning of buffer
      (while (not (bobp))
        (forward-line -1)
        (if (-any? 'looking-at plantuml-indent-regexp-end)
            (setq relative-depth (1- relative-depth)))
        (if (-any? 'looking-at plantuml-indent-regexp-start)
            (setq relative-depth (1+ relative-depth))))

      (if (<= relative-depth 0)
          0
        relative-depth))))

(defun plantuml-indent-line ()
  "Indent the current line to its desired indentation level.
Restore point to same position in text of the line as before indentation."
  (interactive)
  ;; store position of point in line measured from end of line
  (let ((original-position-eol (- (line-end-position) (point))))
    (save-excursion
      (beginning-of-line)
      (indent-line-to (* plantuml-indent-level (plantuml-current-block-depth))))

    ;; restore position in text of line, but not before the beginning of the
    ;; current line
    (goto-char (max (line-beginning-position)
                    (- (line-end-position) original-position-eol)))))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(pu\\|uml\\|plantuml\\|pum\\|plu\\)\\'" . plantuml-mode))

;;;###autoload
(define-derived-mode plantuml-mode prog-mode "plantuml"
  "Major mode for plantuml.

Shortcuts             Command Name
\\[plantuml-complete-symbol]      `plantuml-complete-symbol'"
  (plantuml-init-once)
  (make-local-variable 'plantuml-output-type)
  (set (make-local-variable 'comment-start-skip) "\\('+\\|/'+\\)\\s *")
  (set (make-local-variable 'comment-start) "/'")
  (set (make-local-variable 'comment-end) "'/")
  (set (make-local-variable 'comment-multi-line) t)
  (set (make-local-variable 'comment-style) 'extra-line)
  (set (make-local-variable 'indent-line-function) 'plantuml-indent-line)
  (make-local-variable 'plantuml-preview-default-theme)
  (setq font-lock-defaults '((plantuml-font-lock-keywords) nil t))
  (setq-local completion-at-point-functions (list #'plantuml-complete-symbol)))

(provide 'plantuml-mode)
;;; plantuml-mode.el ends here
