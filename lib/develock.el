;;; develock.el --- additional font-lock keywords for the developers

;; Copyright (C) 2001-2003, 2005-2009, 2012, 2013
;; Katsumi Yamaoka

;; Author: Katsumi Yamaoka  <yamaoka@jpl.org>
;;         Jun'ichi Shiono  <jun@fsas.fujitsu.com>
;;         Yasutaka SHINDOH <ring-pub@fan.gr.jp>
;;         Oscar Bonilla    <ob@bitmover.com>
;; Created: 2001/06/28
;; Revised: 2013/11/15
;; Keywords: font-lock emacs-lisp change-log texinfo c java perl html
;;           tcl ruby mail news

;; Develock is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Develock is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Develock; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Develock is a minor mode which provides the ability to make font-
;; lock highlight leading and trailing whitespace, long lines and
;; oddities in the file buffer for Lisp modes, ChangeLog mode, Texinfo
;; mode, C modes, Java mode, Jde-mode , CPerl mode, Perl mode, HTML
;; modes, some Mail modes, Tcl mode and Ruby mode.  Here is an example
;; of how to set up your startup file (possibly .emacs) to use
;; Develock:
;;
;;(cond ((featurep 'xemacs)
;;       (require 'develock)
;;       ;; `turn-on-develock' is equivalent to `turn-on-font-lock',
;;       ;;  except that it does not highlight the startup screen.
;;       (add-hook 'lisp-interaction-mode-hook 'turn-on-develock)
;;       (add-hook 'mail-setup-hook 'turn-on-font-lock))
;;      ((>= emacs-major-version 20)
;;       (require 'develock)
;;       (global-font-lock-mode 1)))
;;
;; Alternatively, you can use the following to enable font-lock for
;; each mode individually in Emacs.
;;
;;(cond ((featurep 'xemacs)
;;       (require 'develock)
;;       ;; `turn-on-develock' is equivalent to `turn-on-font-lock',
;;       ;;  except that it does not highlight the startup screen.
;;       (add-hook 'lisp-interaction-mode-hook 'turn-on-develock)
;;       (add-hook 'mail-setup-hook 'turn-on-font-lock))
;;      ((>= emacs-major-version 20)
;;       (require 'develock)
;;       (add-hook 'lisp-interaction-mode-hook 'turn-on-font-lock)
;;       (add-hook 'emacs-lisp-mode-hook 'turn-on-font-lock)
;;       (add-hook 'change-log-mode-hook 'turn-on-font-lock)
;;       (add-hook 'texinfo-mode-hook 'turn-on-font-lock)
;;       (add-hook 'c-mode-common-hook 'turn-on-font-lock)
;;       (add-hook 'cperl-mode-hook 'turn-on-font-lock)
;;       (add-hook 'perl-mode-hook 'turn-on-font-lock)
;;       (add-hook 'mail-setup-hook 'turn-on-font-lock)
;;       (add-hook 'java-mode-hook 'turn-on-font-lock)
;;       (add-hook 'html-mode-hook 'turn-on-font-lock)
;;       (add-hook 'html-helper-mode-hook 'turn-on-font-lock)
;;       (add-hook 'message-mode-hook 'turn-on-font-lock)
;;       (add-hook 'tcl-mode-hook 'turn-on-font-lock)
;;       (add-hook 'ruby-mode-hook 'turn-on-font-lock)))
;;
;; Note that `jde-mode' activates the `font-lock-mode' by default
;; because of the `jde-use-font-lock' variable.
;;
;; Develock will work with Emacs 20.4 and later and XEmacs 21.1 and
;; later.  Needless to say, you have to install this file as follows:
;;
;; # cp -p develock.el /usr/local/share/emacs/site-lisp
;; # cd /usr/local/share/emacs/site-lisp
;; # emacs -q -no-site-file -batch -f batch-byte-compile develock.el
;;
;; It is recommended that you use the demand-driven fontification tool
;; (e.g. jit-lock, lazy-shot) together with font-lock.
;;
;; You may find latest version of Develock in ftp.jpl.org:/pub/elisp/.

;; Texinfo
;; =======
;; The value of `fill-column' for `texinfo-mode' has changed to 70 in
;; Emacs 21.  However, many texinfo files are written on the condition
;; that the value is 72.  To against it, Develock provides the
;; `develock-fill-column-alist' variable which makes it possible to
;; set the value for `fill-column' automatically.  If Develock is on,
;; it is set to 72 and also the `auto-fill-mode' is enabled in the
;; `texinfo-mode' by default.
;;
;; Even if you disable it in `develock-fill-column-alist', you can
;; change the behavior of highlighting if too much highlighting
;; offends your eyes, as shown below:
;;
;;(if (and (not (featurep 'xemacs))
;;	 (>= emacs-major-version 21))
;;    (plist-put develock-max-column-plist 'texinfo-mode 72))

;; Ediff
;; =====
;; When you run Ediff on the Develock'ed buffers, you may feel
;; everything is in confusion.  For such a case, the following hooks
;; may help you see diffs clearly.
;;
;;(add-hook
;; 'ediff-prepare-buffer-hook
;; (lambda nil
;;   (if (and (boundp 'font-lock-mode) font-lock-mode
;;	    (boundp 'develock-mode) develock-mode)
;;       (progn
;;	 (develock-mode 0)
;;	 (set (make-local-variable 'develock-mode-suspended) t)))))
;;
;;(add-hook
;; 'ediff-cleanup-hook
;; (lambda nil
;;   (let ((buffers (list ediff-buffer-A ediff-buffer-B ediff-buffer-C)))
;;     (save-excursion
;;       (while buffers
;;	 (if (buffer-live-p (car buffers))
;;	     (progn
;;	       (set-buffer (car buffers))
;;	       (if (and (boundp 'develock-mode-suspended)
;;			develock-mode-suspended)
;;		   (progn
;;		     (develock-mode 1)
;;		     (makunbound 'develock-mode-suspended)))))
;;	 (setq buffers (cdr buffers)))))))

;; Wanderlust
;; ==========
;; Though Wanderlust does not support font-lock, you may use Develock
;; for `wl-draft-mode'.  Try to use the following codes in .wl file.
;;
;;(require 'develock)
;;(require 'message)
;;
;;(setq wl-highlight-body-too nil)
;;
;;(defadvice wl-highlight-headers (around dont-highlight-draft
;;					(&optional for-draft) activate)
;;  "Don't highlight draft buffers."
;;  (or for-draft ad-do-it))
;;
;;(defun wl-draft-setup-develock ()
;;  "Function used to setup to use Develock in `wl-draft-mode'."
;;  (if (featurep 'xemacs)
;;      (put 'wl-draft-mode 'font-lock-defaults
;;	   '(message-font-lock-keywords t))
;;    (make-local-variable 'font-lock-defaults)
;;    (setq font-lock-defaults '(message-font-lock-keywords t t)))
;;  (turn-on-font-lock))
;;
;;(add-hook 'wl-mail-setup-hook 'wl-draft-setup-develock)
;;
;;(setq develock-max-column-plist
;;      (plist-put develock-max-column-plist 'wl-draft-mode t))
;;
;;(let ((elem (copy-sequence (assq 'message-mode develock-keywords-alist))))
;;  (setcar elem 'wl-draft-mode)
;;  (setq develock-keywords-alist
;;	(cons elem (delq (assq 'wl-draft-mode develock-keywords-alist)
;;			 develock-keywords-alist))))
;;
;; If you feel that highlighting is insufficient in gaudiness,
;; customize the value of the variable `message-font-lock-keywords' to
;; make font-lock do what you want it to.

;; Mew
;; ===
;; Similarly, you can use Develock for `mew-draft-mode'.  The following
;; codes are for .mew file.  You will never know unless you try.
;;
;;(require 'develock)
;;(require 'message)
;;
;;(defadvice mew-highlight-header-region (around dont-highlight-draft activate)
;;  "Don't highlight draft buffers."
;;  (or (eq 'mew-draft-mode major-mode) ad-do-it))
;;
;;(defadvice mew-highlight-body-region (around dont-highlight-draft activate)
;;  "Don't highlight draft buffers."
;;  (or (eq 'mew-draft-mode major-mode) ad-do-it))
;;
;;(defun mew-draft-setup-develock ()
;;  "Function used to setup to use Develock in `mew-draft-mode'."
;;  (if (featurep 'xemacs)
;;      (put 'mew-draft-mode 'font-lock-defaults
;;	   '(message-font-lock-keywords t))
;;    (make-local-variable 'font-lock-defaults)
;;    (setq font-lock-defaults '(message-font-lock-keywords t t)))
;;  (turn-on-font-lock))
;;
;;(add-hook 'mew-draft-mode-hook 'mew-draft-setup-develock)
;;
;;(setq develock-max-column-plist
;;      (plist-put develock-max-column-plist 'mew-draft-mode t))
;;
;;(let ((elem (copy-sequence (assq 'message-mode develock-keywords-alist))))
;;  (setcar elem 'mew-draft-mode)
;;  (setq develock-keywords-alist
;;	(cons elem (delq (assq 'mew-draft-mode develock-keywords-alist)
;;			 develock-keywords-alist))))

;;; Code:

(defconst develock-version "0.45"
  "Version number for this version of Develock.")

(require 'advice)
(require 'font-lock)

(defgroup develock nil
  "A tool for the developers."
  :group 'development)

(make-variable-buffer-local
 (defvar develock-mode nil
   "Internal buffer-local variable used to indicate whether Develock is on."))

(defcustom develock-mode-strings '(" Develock" " Font")
  "List of identifications used to show whether Develock is on or off.
Each element should be a string beginning with a space, a null string
or a symbol with a similar string as its value.  You can use faces for
each element if you are using Emacs 21 as follows:

\(make-face 'develock-mode)
\(setq develock-mode-strings
      (list (propertize \" Develock\"
			'face '(develock-mode :foreground \"ForestGreen\"))
	    (propertize \" Font\"
			'face '(develock-mode :foreground \"DarkGoldenRod\"))))
\(when (featurep 'develock)
  (add-minor-mode 'font-lock-mode
		  (cons 'develock-mode develock-mode-strings)))

Otherwise if you are using XEmacs, you can use a face for all the
minor mode strings as shown below:

\(copy-face 'modeline 'modeline-mousable-minor-mode)
\(set-face-foreground 'modeline-mousable-minor-mode \"PaleTurquoise\")
"
  :type '(list (sexp :format "On: %v\n" :size 0)
	       (sexp :format "Off: %v\n" :size 0))
  :set (lambda (symbol value)
	 (prog1
	     (if (fboundp 'custom-set-default)
		 (custom-set-default symbol value)
	       (set-default symbol value))
	   (if (and (not noninteractive)
		    (featurep 'develock))
	       (let ((id (cons 'develock-mode value))
		     mmode)
		 (if (fboundp 'add-minor-mode)
		     (add-minor-mode 'font-lock-mode id)
		   (if (setq mmode (assq 'font-lock-mode minor-mode-alist))
		       (setcdr mmode (list id))
		     (setq minor-mode-alist (cons (list 'font-lock-mode id)
						  minor-mode-alist))))))))
  :group 'develock
  :group 'font-lock)

(if (fboundp 'add-minor-mode)
    (eval-after-load "font-lock"
      '(add-minor-mode 'font-lock-mode
		       (cons 'develock-mode develock-mode-strings)))
  (eval-after-load "font-lock"
    '(setcdr (assq 'font-lock-mode minor-mode-alist)
	     (list (cons 'develock-mode develock-mode-strings)))))

(defface develock-whitespace-1
  '((t (:background "Red")))
  "Develock face used to highlight whitespace."
  :group 'develock
  :group 'font-lock-highlighting-faces
  :group 'font-lock-faces)

(defface develock-whitespace-2
  '((t (:background "Orange")))
  "Develock face used to highlight whitespace."
  :group 'develock
  :group 'font-lock-highlighting-faces
  :group 'font-lock-faces)

(defface develock-whitespace-3
  '((t (:background "Yellow")))
  "Develock face used to highlight whitespace."
  :group 'develock
  :group 'font-lock-highlighting-faces
  :group 'font-lock-faces)

(defface develock-long-line-1
  '((((background dark))
     (:foreground "Orange"))
    (t
     (:foreground "Red")))
  "Develock face used to highlight long lines."
  :group 'develock
  :group 'font-lock-highlighting-faces
  :group 'font-lock-faces)

(defface develock-long-line-2
  '((t (:foreground "Red" :background "Yellow")))
  "Develock face used to highlight long lines."
  :group 'develock
  :group 'font-lock-highlighting-faces
  :group 'font-lock-faces)

(defface develock-lonely-parentheses
  '((t (:foreground "Blue" :background "PaleTurquoise")))
  "Develock face used to highlight lonely parentheses."
  :group 'develock
  :group 'font-lock-highlighting-faces
  :group 'font-lock-faces)

(defface develock-bad-manner
  '((((background dark))
     (:foreground "Black" :background "Yellow"))
    (t
     (:foreground "Yellow" :background "Black")))
  "Develock face used to highlight unwanted characters in messages.
Those might violate the manners of mail messages or news articles."
  :group 'develock
  :group 'font-lock-highlighting-faces
  :group 'font-lock-faces)

(defface develock-upper-case-tag
  '((((background dark))
     (:foreground "Black" :background "PowderBlue"))
    (t
     (:foreground "Snow" :background "FireBrick")))
  "Develock face used to highlight upper case character tags."
  :group 'develock
  :group 'font-lock-highlighting-faces
  :group 'font-lock-faces)

(defface develock-upper-case-attribute
  '((((background dark))
     (:foreground "Black" :background "Wheat"))
    (t
     (:foreground "Snow" :background "ForestGreen")))
  "Develock face used to highlight upper case character tags."
  :group 'develock
  :group 'font-lock-highlighting-faces
  :group 'font-lock-faces)

(defface develock-reachable-mail-address
  '((t (:foreground "DarkGreen" :background "LemonChiffon")))
  "Develock face used to highlight reachable E-mail addresses.
That would be defenseless to spammers."
  :group 'develock
  :group 'font-lock-highlighting-faces
  :group 'font-lock-faces)

(defface develock-attention
  '((((background dark))
     (:foreground "OrangeRed" :bold t))
    (t
     (:foreground "Red" :bold t)))
  "Develock face used to highlight things to be paid attention."
  :group 'develock
  :group 'font-lock-highlighting-faces
  :group 'font-lock-faces)

(defcustom develock-auto-enable t
  "If non-nil, turn Develock mode on when font-lock is turned on."
  :type '(boolean :format "%{%t%}: %[%v%]" :on "On" :off "Off")
  :group 'develock
  :group 'font-lock)

(defun develock-refontify-buffers ()
  "Refontify all Develocked buffers."
  (interactive)
  (let ((buffers (buffer-list)))
    (save-excursion
      (while buffers
	(set-buffer (car buffers))
	(if develock-mode
	    (let ((develock-auto-enable t))
	      (font-lock-mode 0)
	      (font-lock-mode 1)))
	(setq buffers (cdr buffers))))))

(defun develock-custom-set-and-refontify (symbol value)
  "Function used to refontify buffers when customizing Develock options."
  (prog1
      (if (fboundp 'custom-set-default)
	  (custom-set-default symbol value)
	(set-default symbol value))
    (if (and (not noninteractive)
	     (featurep 'develock))
	(develock-refontify-buffers))))

(defcustom develock-max-column-plist
  (list 'emacs-lisp-mode 79
	'lisp-interaction-mode 'w
	'change-log-mode t
	'texinfo-mode t
	'c-mode 79
	'c++-mode 79
	'java-mode 79
	'jde-mode 79
	'html-mode 79
	'html-helper-mode 79
	'cperl-mode 79
	'perl-mode 79
	'mail-mode t
	'message-mode t
	'cmail-mail-mode t
	'tcl-mode 79
	'ruby-mode 79)
  "Plist of `major-mode's and limitation values for long lines.
The part of a line that is longer than the limitation value according
to the `major-mode' is highlighted.  Value `w' means one subtracted
from the window width.  Value t means the return value of
`current-fill-column'.  You can inhibit to highlight long lines using
the value nil."
  :type '(repeat
	  (list :indent 2 :inline t :format "%v"
		(symbol :format "Major-Mode: %v\n" :size 0)
		(radio :format "Value: %v"
		       (const :format "Off  " nil)
		       (const :format "Fill_column  " t)
		       (const :format "Window_width -1  " w)
		       (integer :format "Integer: %v\n" :size 0))))
  :set 'develock-custom-set-and-refontify
  :group 'develock
  :group 'font-lock)

(defcustom develock-fill-column-alist
  '((change-log-mode . 74)
    (texinfo-mode . 72))
  "Alist of `major-mode's and the values for `fill-column'.
When Develock is turned on, `auto-fill-mode' is automatically enabled
in the buffer where `major-mode' included in this list runs.
Each value is applied to `fill-column' in the buffer overriding the
one in the directory-local variables specified in the .dir-locals.el
file (if any).  If `fill-column' is specified as a local variable and
is allowed in the buffer, this variable will be made buffer-local and
the value will be modified."
  :type '(repeat (cons :format "%v"
		       (symbol :tag "Major mode")
		       (integer :tag "Fill column")))
  :group 'develock
  :group 'font-lock)

(defcustom develock-mode-ignore-kinsoku-list
  '(emacs-lisp-mode lisp-interaction-mode c-mode c++-mode java-mode jde-mode
		    cperl-mode perl-mode tcl-mode)
  "List of `major-mode's that ignore kinsoku at the end of lines."
  :type '(repeat (symbol :format "Major-Mode: %v\n" :size 0))
  :set 'develock-custom-set-and-refontify
  :group 'develock
  :group 'font-lock)

(defcustom develock-inhibit-highlighting-kinsoku-chars nil
  "If non-nil, inhibit highlighting kinsoku chars at the end of lines.
It has no effect if the buffer's major-mode is contained in the
`develock-mode-ignore-kinsoku-list' variable."
  :type '(boolean :format "%{%t%}: %[%v%]"
		  :on "Inhibited" :off "Not inhibited")
  :set 'develock-custom-set-and-refontify
  :group 'develock
  :group 'font-lock)

(defcustom develock-mode-ignore-long-message-header-list
  '(mail-mode message-mode cmail-mail-mode)
  "List of `major-mode's to stop highlighting long message header lines.
If this list contains the value of the current `major-mode', there is
apparently a message header and even if there are long header lines,
they will not be highlighted by Develock."
  :type '(repeat (symbol :format "Major-Mode: %v\n" :size 0))
  :set 'develock-custom-set-and-refontify
  :group 'develock
  :group 'font-lock)

(defcustom develock-ignored-buffer-name-regexp nil
  "Regexp matching buffer names that will not be highlighted by Develock.
If you don't want to perform Develock on the buffer which name begins
with \"*\" or \" *\" (typically \"*scratch*\"), use the following value:

\(setq develock-ignored-buffer-name-regexp \"^ ?\\\\*[^*]+\\\\*$\")

If you want to inhibit highlighting those buffers also by font-lock,
try the following advice in your startup file.

\(defadvice font-lock-mode (before dont-turn-on-for-some-buffers
				  activate)
  \"Don't turn on font-lock for some buffers which specified by the
`develock-ignored-buffer-name-regexp' variable.\"
  (if (and (boundp 'develock-ignored-buffer-name-regexp)
	   (symbol-value 'develock-ignored-buffer-name-regexp)
	   (string-match
	    (symbol-value 'develock-ignored-buffer-name-regexp)
	    (buffer-name)))
      (ad-set-arg 0 0)))"
  :type '(radio (const :format "Omitted " nil)
		(regexp :format "%t: %v\n" :size 0))
  :set 'develock-custom-set-and-refontify
  :group 'develock
  :group 'font-lock)

(defcustom develock-ignored-file-name-regexp
  "\\`\\.newsrc\\(-.+\\)?\\.eld\\'"
  "Regexp matching file names that will not be highlighted by Develock.
If you want to inhibit highlighting those files also by font-lock,
try the following advice in your startup file.

\(defadvice font-lock-mode (before dont-turn-on-for-some-files
				  activate)
  \"Don't turn on font-lock for some files which specified by the
`develock-ignored-file-name-regexp' variable.\"
  (if (and (boundp 'develock-ignored-file-name-regexp)
	   (symbol-value 'develock-ignored-file-name-regexp)
	   buffer-file-name
	   (string-match
	    (symbol-value 'develock-ignored-file-name-regexp)
	    (file-name-nondirectory buffer-file-name)))
      (ad-set-arg 0 0)))"
  :type '(radio (const :format "Omitted " nil)
		(regexp :format "%t: %v\n" :size 0))
  :set 'develock-custom-set-and-refontify
  :group 'develock
  :group 'font-lock)

(eval-and-compile
  (if (featurep 'xemacs)
      (progn
	(defalias 'develock-point-at-bol 'point-at-bol)
	(defalias 'develock-point-at-eol 'point-at-eol))
    (defalias 'develock-point-at-bol 'line-beginning-position)
    (defalias 'develock-point-at-eol 'line-end-position)))

(defvar lisp-font-lock-keywords-x nil
  "Extraordinary level font-lock keywords for the Lisp modes.")

(defvar change-log-font-lock-keywords-x nil
  "Extraordinary level font-lock keywords for the Change Log mode.")

(defvar texinfo-font-lock-keywords-x nil
  "Extraordinary level font-lock keywords for the TeXinfo mode.")

(defvar c-font-lock-keywords-x nil
  "Extraordinary level font-lock keywords for the C modes.")

(defvar java-font-lock-keywords-x nil
  "Extraordinary level font-lock keywords for the Java mode.")

(defvar cperl-font-lock-keywords-x nil
  "Extraordinary level font-lock keywords for the CPerl mode.")

(defvar perl-font-lock-keywords-x nil
  "Extraordinary level font-lock keywords for the Perl mode.")

(defvar mail-font-lock-keywords-x nil
  "Extraordinary level font-lock keywords for the Mail mode.")

(defvar message-font-lock-keywords-x nil
  "Extraordinary level font-lock keywords for the Message mode.")

(defvar cmail-font-lock-keywords-x nil
  "Extraordinary level font-lock keywords for the Cmail mode.")

(defvar html-font-lock-keywords-x nil
  "Extraordinary level font-lock keywords for the HTML mode.")

(defvar tcl-font-lock-keywords-x nil
  "Extraordinary level font-lock keywords for the Tcl mode.")

(defvar ruby-font-lock-keywords-x nil
  "Extraordinary level font-lock keywords for the Ruby mode.")

(defvar develock-keywords-alist
  '((emacs-lisp-mode lisp-font-lock-keywords-x
		     develock-lisp-font-lock-keywords)
    (lisp-interaction-mode lisp-font-lock-keywords-x
			   develock-lisp-font-lock-keywords)
    (change-log-mode change-log-font-lock-keywords-x
		     develock-change-log-font-lock-keywords)
    (texinfo-mode texinfo-font-lock-keywords-x
		  develock-texinfo-font-lock-keywords)
    (c-mode c-font-lock-keywords-x
	    develock-c-font-lock-keywords)
    (c++-mode c-font-lock-keywords-x
	      develock-c-font-lock-keywords)
    (java-mode java-font-lock-keywords-x
	       develock-java-font-lock-keywords)
    (jde-mode java-font-lock-keywords-x
	      develock-java-font-lock-keywords)
    (html-mode html-font-lock-keywords-x
	       develock-html-font-lock-keywords)
    (html-helper-mode html-font-lock-keywords-x
		      develock-html-font-lock-keywords)
    (cperl-mode cperl-font-lock-keywords-x
		develock-cperl-font-lock-keywords)
    (perl-mode perl-font-lock-keywords-x
	       develock-perl-font-lock-keywords)
    (mail-mode mail-font-lock-keywords-x
	       develock-mail-font-lock-keywords)
    (message-mode message-font-lock-keywords-x
		  develock-mail-font-lock-keywords)
    (cmail-mail-mode cmail-font-lock-keywords-x
		     develock-mail-font-lock-keywords)
    (tcl-mode tcl-font-lock-keywords-x
	      develock-tcl-font-lock-keywords)
    (ruby-mode ruby-font-lock-keywords-x
	       develock-ruby-font-lock-keywords))
  "*Alist of keyword symbols for major modes.
Each element should be triple symbols of the following form:

\(major-mode internal-keywords user-defined-keywords)")

(defvar develock-keywords-custom-type
  (let* ((args
	  '(option :format "    %v"
		   (list :inline t :tag "Args"
			 (radio :format "  Override: %v"
				(const :format "%v " nil)
				(const :format "%v " t)
				(const :format "%v " keep)
				(const :format "%v " prepend)
				(const append))
			 (boolean :tag "  LaxMatch"))))
	 (fixed-function
	  `(list
	    :convert-widget
	    (lambda (widget)
	      `(function
		:format "  %t: %v\n"
		:size 0 :value (lambda (limit))
		,@(if (not (widget-get widget :copy))
		      ;; Emacs versions prior to 21.4.
		      '(:match
			(lambda (widget value) (functionp value))
			:value-to-internal
			(lambda (widget value)
			  (widget-sexp-value-to-internal
			   widget
			   (if (and (stringp value)
				    (string-match "\\`\".*\"\\'" value))
			       (substring value 1 -1)
			     value)))))))))
	 (face-widget
	  (lambda (prompt)
	    `(group
	      :convert-widget (lambda (widget)
				(apply 'widget-convert (widget-type widget)
				       (eval (car (widget-get widget :args)))))
	      (list
	       '(const :format "" quote)
	       (append '(face :indent 6 :format)
		       (if (condition-case nil
			       (string-match
				"%{sample%}"
				(widget-get (get 'face 'widget-type) :format))
			     (error nil))
			   (list (concat ,prompt "(%{sample%}) %v\n") :size 0)
			 (list (concat ,prompt "%[select face%] %v"))))))))
	 (face1 (funcall face-widget "  %{%t%}: "))
	 (face2 (funcall face-widget "    %{%t%}: ")))
    `(repeat
      (choice
       :format "%[%t%] %v\n" :indent 0 :tag "Keyword Type"
       (list :tag "Find Long Lines"
	     (const :format "  Function: %v\n"
		    :value develock-find-long-lines)
	     (group (const :format "" 1)
		    ,(funcall face-widget "\
  %t used in a boundary (subexpression #1):\n    ")
		    ,args)
	     (group (const :format "" 2)
		    ,(funcall face-widget "\
  %t used out of a boundary (subexpression #2):\n    ")
		    ,args))
       (list :tag "Find Tabs or Long Spaces"
	     :sample-face widget-documentation-face
	     :format "Find Tabs or Long Spaces:
   %{(If `indent-tabs-mode' is nil, this type highlights tabs.%}\n%v"
	     (const :sample-face widget-documentation-face
		    :format "\
    %{Otherwise, it highlights spaces longer than `tab-width'.)%}
  Function: %v\n"
		    :value develock-find-tab-or-long-space)
	     (group (const :format "" 1)
		    ,(funcall face-widget "\
  %t used for tabs before long spaces (subexpression #1):\n    ")
		    ,args)
	     (group (const :format "" 2)
		    ,(funcall face-widget "\
  %t used for long spaces (subexpression #2):\n    ")
		    ,args))
       (list :tag "(REGEXP NUM 'FACE...)"
	     (regexp :format "  %t: %v\n" :size 0)
	     (integer :format "  Match Subexpression Number: %v\n"
		      :size 0)
	     ,face1
	     ,args)
       (list :tag "(FUNCTION NUM 'FACE...)"
	     ,fixed-function
	     (integer :format "  Match Subexpression Number: %v\n"
		      :size 0)
	     ,face1
	     ,args)
       (cons :tag "(REGEXP (NUM 'FACE...) (NUM 'FACE...)...)"
	     (regexp :format "  %t: %v\n" :size 0)
	     (repeat
	      :format "%v%i\n" :indent 2
	      (list :format "\n%v" :indent 0
		    (integer
		     :format "    Match Subexpression Number: %v\n"
		     :size 0)
		    ,face2
		    ,args)))
       (cons :tag "(CASE_SENSITIVE_REGEXP (NUM 'FACE...)...)"
	     (group
	      (const :format "" lambda) (const :format "" (limit))
	      (group
	       (const :format "" let) (const :format "" (case-fold-search))
	       (group
		(const :format "" re-search-forward)
		(regexp :format "  %t: %v\n" :size 0)
		(const :format "" limit) (const :format "" t))))
	     (repeat
	      :format "%v%i\n" :indent 2
	      (list :format "\n%v" :indent 0
		    (integer
		     :format "    Match Subexpression Number: %v\n"
		     :size 0)
		    ,face2
		    ,args)))
       (cons :tag "(FUNCTION (NUM 'FACE...) (NUM 'FACE...)...)"
	     ,fixed-function
	     (repeat
	      :format "%v%i\n" :indent 2
	      (list :format "\n%v" :indent 0
		    (integer
		     :format "    Match Subexpression Number: %v\n"
		     :size 0)
		    ,face2
		    ,args)))
       (cons :tag "(REGEXP (NUM (IF COND 'FACE)...)...)"
	     (regexp :format "  %t: %v\n" :size 0)
	     (repeat
	      :format "%v%i\n" :indent 2
	      (list :format "\n%v" :indent 0
		    (integer
		     :format "    Match Subexpression Number: %v\n"
		     :size 0)
		    (group :format "%v"
			   (const :format "" if)
			   (sexp :format "    Condition: %v\n" :size 0)
			   ,face2)
		    ,args)))
       (sexp :size 0))))
  "*Customizing widget for the extraordinary level font-lock keywords.")

(defun develock-keywords-custom-set (symbol value)
  "Function used to the :set keyword for `develock-*-font-lock-keywords'."
  (prog1
      (if (fboundp 'custom-set-default)
	  (custom-set-default symbol value)
	(set-default symbol value))
    (if (and (not noninteractive)
	     (featurep 'develock))
	(let ((buffers (buffer-list))
	      (alist develock-keywords-alist)
	      modes)
	  (while alist
	    (if (eq symbol (nth 2 (car alist)))
		(setq modes (cons (car (car alist)) modes)))
	    (setq alist (cdr alist)))
	  (save-excursion
	    (while buffers
	      (set-buffer (car buffers))
	      (if (memq major-mode modes)
		  (develock-mode 1))
	      (setq buffers (cdr buffers))))))))

(defcustom develock-lisp-font-lock-keywords
  '(;; a long line
    (develock-find-long-lines
     (1 'develock-long-line-1 t)
     (2 'develock-long-line-2 t))
    ;; long spaces
    (develock-find-tab-or-long-space
     (1 'develock-whitespace-2)
     (2 'develock-whitespace-3 nil t))
    ;; trailing whitespace
    ("[^\t\n ]\\([\t ]+\\)$"
     (1 'develock-whitespace-1 t))
    ;; spaces before tabs
    ("\\( +\\)\\(\t+\\)"
     (1 'develock-whitespace-1 t)
     (2 'develock-whitespace-2 t))
    ;; tab space tab
    ("\\(\t\\) \t"
     (1 'develock-whitespace-2 append))
    ;; only tabs or spaces in the line
    ("^[\t ]+$"
     (0 'develock-whitespace-2 append))
    ;; lonely left parentheses or brackets without a comment
    ("^[\t ]*\\(['`]?[\t ([]*[([]\\)[\t ]*$"
     1 'develock-lonely-parentheses)
    ;; lonely right parentheses or brackets
    ("^[\t ]*\\([]\t )]*[])]\\)\\([\t ;]+\\|[\t ]*$\\)"
     1 'develock-lonely-parentheses)
    ;; whitespace after a left parenthesis
    ("(\\([\t ]+\\)[^\n;]"
     1 'develock-whitespace-2)
    ;; whitespace before a right parenthesis
    ("([^\n]\\([\t ]+\\))"
     1 'develock-whitespace-2)
    ;; reachable E-mail addresses
    ("<?[-+.0-9A-Z_a-z]+@[-0-9A-Z_a-z]+\\(\\.[-0-9A-Z_a-z]+\\)+>?"
     (0 'develock-reachable-mail-address t))
    ;; things to be paid attention
    ("\\<\\(?:[Ff][Ii][Xx][Mm][Ee]\\|[Tt][Oo][Dd][Oo]\\)\\(?::\\|\\>\\)"
     (0 'develock-attention t)))
  "Extraordinary level highlighting for the Lisp modes."
  :type develock-keywords-custom-type
  :set 'develock-keywords-custom-set
  :group 'develock
  :group 'font-lock)

(defvar change-log-indent-text)

(defcustom develock-change-log-font-lock-keywords
  '(;; a long line
    (develock-find-long-lines
     (1 'develock-long-line-1 append)
     (2 'develock-long-line-2 append))
    ;; an ugly entry line
    (develock-find-ugly-change-log-entry-line
     (1 'develock-whitespace-1 nil t)
     (2 'develock-whitespace-1 t t)
     (3 'develock-whitespace-1 t t)
     (4 'develock-whitespace-1 t t)
     (5 'develock-whitespace-1 t t)
     (6 'develock-whitespace-1 t t)
     (7 'develock-whitespace-1 t t)
     (8 'develock-whitespace-2 t t)
     (9 'develock-whitespace-2 t t)
     (10 'develock-whitespace-3 t t)
     (11 'develock-whitespace-1 t t)
     (12 'develock-whitespace-1 t t)
     (13 'develock-whitespace-1 t t)
     (14 'develock-whitespace-1 t t))
    ;; leading spaces
    ("^\\( +\\)[^\t\n]"
     (1 'develock-whitespace-3))
    ;; leading 2 or more tabs
    ("^\\([\t ][\t ]+\\)[^\t\n]"
     ;; Improved by NISHIYAMA-san; cf. [Elips: 0005541].
     (1 (if (and (eq (char-after (develock-point-at-bol)) ?\t)
		 (not (= (- (match-end 1) (match-beginning 1) 1)
			 change-log-indent-text)))
	    'develock-whitespace-2)))
    ;; trailing whitespace
    ("[^\t\n ]\\([\t ]+\\)$"
     (1 'develock-whitespace-1 t))
    ;; tabs or 2 or more spaces in the log line
    ("[^\t\n ]\\(\t[\t ]*\\|  +\\)[^\t ]"
     (1 (if (and (memq (char-after (develock-point-at-bol)) '(?\t ?\ ))
		 (not (string-equal ".  "
				    (buffer-substring (1- (match-beginning 1))
						      (match-end 1)))))
	    'develock-whitespace-2)
	prepend))
    ;; spaces before tabs
    ("\\( +\\)\\(\t+\\)\\([\t ]*\\)"
     (1 'develock-whitespace-1 t)
     (2 'develock-whitespace-2 append)
     (3 'develock-whitespace-2 append t))
    ;; only tabs or spaces in the line
    ("^[\t ]+$"
     (0 'develock-whitespace-2 append))
    ;; whitespace between a file name and a colon
    ("^\t\\* [^\t\n ]+\\( (.+)\\)?\\([\t ]+\\):"
     (2 'develock-whitespace-1 t)))
  "Extraordinary level highlighting for the Change Log mode."
  :type develock-keywords-custom-type
  :set 'develock-keywords-custom-set
  :group 'develock
  :group 'font-lock)

(defcustom develock-texinfo-font-lock-keywords
  '(;; a long line
    (develock-find-long-lines
     (1 'develock-long-line-1 t)
     (2 'develock-long-line-2 t))
    ;; trailing whitespace
    ("[^\t\n ]\\([\t ]+\\)$"
     (1 'develock-whitespace-1 t))
    ;; spaces before tabs
    ("\\( +\\)\\(\t+\\)"
     (1 'develock-whitespace-1 t)
     (2 'develock-whitespace-2 t))
    ;; tab space tab
    ("\\(\t\\) \t"
     (1 'develock-whitespace-2 append))
    ;; only tabs or spaces in the line
    ("^[\t ]+$"
     (0 'develock-whitespace-2 append))
    ;; tabs
    ("\t+"
     (0 'develock-whitespace-1 append))
    ;; things to be paid attention
    ("\\<\\(?:[Ff][Ii][Xx][Mm][Ee]\\|[Tt][Oo][Dd][Oo]\\)\\(?::\\|\\>\\)"
     (0 'develock-attention t)))
  "Extraordinary level highlighting for the TeXinfo mode."
  :type develock-keywords-custom-type
  :set 'develock-keywords-custom-set
  :group 'develock
  :group 'font-lock)

(defcustom develock-c-font-lock-keywords
  '(;; a long line
    (develock-find-long-lines
     (1 'develock-long-line-1 t)
     (2 'develock-long-line-2 t))
    ;; long spaces
    (develock-find-tab-or-long-space
     (1 'develock-whitespace-2)
     (2 'develock-whitespace-3 nil t))
    ;; trailing whitespace
    ("[^\t\n ]\\([\t ]+\\)$"
     (1 'develock-whitespace-1 t))
    ;; spaces before tabs
    ("\\( +\\)\\(\t+\\)"
     (1 'develock-whitespace-1 t)
     (2 'develock-whitespace-2 t))
    ;; tab space tab
    ("\\(\t\\) \t"
     (1 'develock-whitespace-2 append))
    ;; only tabs or spaces in the line
    ("^[\t ]+$"
     (0 'develock-whitespace-2 append))
    ;; reachable E-mail addresses
    ("<?[-+.0-9A-Z_a-z]+@[-0-9A-Z_a-z]+\\(\\.[-0-9A-Z_a-z]+\\)+>?"
     (0 'develock-reachable-mail-address t))
    ;; things to be paid attention
    ("\\<\\(?:[Ff][Ii][Xx][Mm][Ee]\\|[Tt][Oo][Dd][Oo]\\)\\(?::\\|\\>\\)"
     (0 'develock-attention t)))
  "Extraordinary level highlighting for the C modes."
  :type develock-keywords-custom-type
  :set 'develock-keywords-custom-set
  :group 'develock
  :group 'font-lock)

(defcustom develock-java-font-lock-keywords
  '(;; a long line
    (develock-find-long-lines
     (1 'develock-long-line-1 t)
     (2 'develock-long-line-2 t))
    ;; long spaces
    (develock-find-tab-or-long-space
     (1 'develock-whitespace-2)
     (2 'develock-whitespace-3 nil t))
    ;; trailing whitespace
    ("[^\t\n ]\\([\t ]+\\)$"
     (1 'develock-whitespace-1 t))
    ;; spaces before tabs
    ("\\( +\\)\\(\t+\\)"
     (1 'develock-whitespace-1 t)
     (2 'develock-whitespace-2 t))
    ;; tab space tab
    ("\\(\t\\) \t"
     (1 'develock-whitespace-2 append))
    ;; only tabs or spaces in the line
    ("^[\t ]+$"
     (0 'develock-whitespace-2 append))
    ;; reachable E-mail addresses
    ("<?[-+.0-9A-Z_a-z]+@[-0-9A-Z_a-z]+\\(\\.[-0-9A-Z_a-z]+\\)+>?"
     (0 'develock-reachable-mail-address t))
    ;; things to be paid attention
    ("\\<\\(?:[Ff][Ii][Xx][Mm][Ee]\\|[Tt][Oo][Dd][Oo]\\)\\(?::\\|\\>\\)"
     (0 'develock-attention t)))
  "Extraordinary level highlighting for the Java mode."
  :type develock-keywords-custom-type
  :set 'develock-keywords-custom-set
  :group 'develock
  :group 'font-lock)

(defcustom develock-html-font-lock-keywords
  '(;; a long line
    (develock-find-long-lines
     (1 'develock-long-line-1 t)
     (2 'develock-long-line-2 t))
    ;; long spaces
    (develock-find-tab-or-long-space
     (1 'develock-whitespace-2)
     (2 'develock-whitespace-3 nil t))
    ;; trailing whitespace
    ("[^\t\n ]\\([\t ]+\\)$"
     (1 'develock-whitespace-1 t))
    ;; spaces before tabs
    ("\\( +\\)\\(\t+\\)"
     (1 'develock-whitespace-1 t)
     (2 'develock-whitespace-2 t))
    ;; tab space tab
    ("\\(\t\\) \t"
     (1 'develock-whitespace-2 append))
    ;; only tabs or spaces in the line
    ("^[\t ]+$"
     (0 'develock-whitespace-2 append))
    ;; using upper case tag
    ((lambda (limit)
       (let (case-fold-search)
	 (re-search-forward "</?\\([A-Z]+\\)" limit t)))
     (1 'develock-upper-case-tag t))
    ;; using upper case attribute
    ((lambda (limit)
       (let (case-fold-search)
	 (re-search-forward "<[a-zA-Z]+[\t ]+\\([A-Z]+\\)[\t ]*=" limit t)))
     (1 'develock-upper-case-attribute t)))
  "Extraordinary level highlighting for the HTML mode."
  :type develock-keywords-custom-type
  :set 'develock-keywords-custom-set
  :group 'develock
  :group 'font-lock)

(defcustom develock-cperl-font-lock-keywords
  '(;; a long line
    (develock-find-long-lines
     (1 'develock-long-line-1 t)
     (2 'develock-long-line-2 t))
    ;; long spaces
    (develock-find-tab-or-long-space
     (1 'develock-whitespace-2)
     (2 'develock-whitespace-3 nil t))
    ;; trailing whitespace
    ("[^\t\n ]\\([\t ]+\\)$"
     (1 'develock-whitespace-1 t))
    ;; spaces before tabs
    ("\\( +\\)\\(\t+\\)"
     (1 'develock-whitespace-1 t)
     (2 'develock-whitespace-2 t))
    ;; tab space tab
    ("\\(\t\\) \t"
     (1 'develock-whitespace-2 append))
    ;; only tabs or spaces in the line
    ("^[\t ]+$"
     (0 'develock-whitespace-2 append))
    ;; reachable E-mail addresses
    ("<?[-+.0-9A-Z_a-z]+@[-0-9A-Z_a-z]+\\(\\.[-0-9A-Z_a-z]+\\)+>?"
     (0 'develock-reachable-mail-address t))
    ;; things to be paid attention
    ("\\<\\(?:[Ff][Ii][Xx][Mm][Ee]\\|[Tt][Oo][Dd][Oo]\\)\\(?::\\|\\>\\)"
     (0 'develock-attention t)))
  "Extraordinary level highlighting for the CPerl mode."
  :type develock-keywords-custom-type
  :set 'develock-keywords-custom-set
  :group 'develock
  :group 'font-lock)

(defcustom develock-perl-font-lock-keywords
  '(;; a long line
    (develock-find-long-lines
     (1 'develock-long-line-1 t)
     (2 'develock-long-line-2 t))
    ;; long spaces
    (develock-find-tab-or-long-space
     (1 'develock-whitespace-2)
     (2 'develock-whitespace-3 nil t))
    ;; trailing whitespace
    ("[^\t\n ]\\([\t ]+\\)$"
     (1 'develock-whitespace-1 t))
    ;; spaces before tabs
    ("\\( +\\)\\(\t+\\)"
     (1 'develock-whitespace-1 t)
     (2 'develock-whitespace-2 t))
    ;; tab space tab
    ("\\(\t\\) \t"
     (1 'develock-whitespace-2 append))
    ;; only tabs or spaces in the line
    ("^[\t ]+$"
     (0 'develock-whitespace-2 append))
    ;; reachable E-mail addresses
    ("<?[-+.0-9A-Z_a-z]+@[-0-9A-Z_a-z]+\\(\\.[-0-9A-Z_a-z]+\\)+>?"
     (0 'develock-reachable-mail-address t))
    ;; things to be paid attention
    ("\\<\\(?:[Ff][Ii][Xx][Mm][Ee]\\|[Tt][Oo][Dd][Oo]\\)\\(?::\\|\\>\\)"
     (0 'develock-attention t)))
  "Extraordinary level highlighting for the Perl mode."
  :type develock-keywords-custom-type
  :set 'develock-keywords-custom-set
  :group 'develock
  :group 'font-lock)

(defcustom develock-mail-font-lock-keywords
  `(;; a long line
    (develock-find-long-lines
     (1 'develock-long-line-1 t)
     (2 'develock-long-line-2 t))
    ;; long spaces
    (develock-find-tab-or-long-space
     (1 'develock-whitespace-2)
     (2 'develock-whitespace-3 nil t))
    ;; trailing whitespace
    ("[^\t\n ]\\([\t ]+\\)$"
     (1 'develock-whitespace-1 t))
    ;; spaces before tabs
    ("\\( +\\)\\(\t+\\)"
     (1 'develock-whitespace-1 t)
     (2 'develock-whitespace-2 t))
    ;; tab space tab
    ("\\(\t\\) \t"
     (1 'develock-whitespace-2 append))
    ;; only tabs or spaces in the line
    ("^[\t ]+$"
     (0 'develock-whitespace-2 append))
    ,(if (featurep 'mule)
	 ;; Japanese hankaku katakana
	 '("\\ck+" (0 'develock-bad-manner t))))
  "Extraordinary level highlighting for the Mail modes."
  :type develock-keywords-custom-type
  :set 'develock-keywords-custom-set
  :group 'develock
  :group 'font-lock)

(defcustom develock-tcl-font-lock-keywords
  '(;; a long line
    (develock-find-long-lines
     (1 'develock-long-line-1 t)
     (2 'develock-long-line-2 t))
    ;; long spaces
    (develock-find-tab-or-long-space
     (1 'develock-whitespace-2)
     (2 'develock-whitespace-3 nil t))
    ;; trailing whitespace
    ("[^\t\n ]\\([\t ]+\\)$"
     (1 'develock-whitespace-1 t))
    ;; spaces before tabs
    ("\\( +\\)\\(\t+\\)"
     (1 'develock-whitespace-1 t)
     (2 'develock-whitespace-2 t))
    ;; tab space tab
    ("\\(\t\\) \t"
     (1 'develock-whitespace-2 append))
    ;; only tabs or spaces in the line
    ("^[\t ]+$"
     (0 'develock-whitespace-2 append))
    ;; reachable E-mail addresses
    ("<?[-+.0-9A-Z_a-z]+@[-0-9A-Z_a-z]+\\(\\.[-0-9A-Z_a-z]+\\)+>?"
     (0 'develock-reachable-mail-address t))
    ;; things to be paid attention
    ("\\<\\(?:[Ff][Ii][Xx][Mm][Ee]\\|[Tt][Oo][Dd][Oo]\\)\\(?::\\|\\>\\)"
     (0 'develock-attention t)))
  "Extraordinary level highlighting for the Tcl mode."
  :type develock-keywords-custom-type
  :set 'develock-keywords-custom-set
  :group 'develock
  :group 'font-lock)

(defcustom develock-ruby-font-lock-keywords
  '(;; a long line
    (develock-find-long-lines
     (1 'develock-long-line-1 t)
     (2 'develock-long-line-2 t))
    ;; long spaces
    (develock-find-tab-or-long-space
     (1 'develock-whitespace-2)
     (2 'develock-whitespace-3 nil t))
    ;; trailing whitespace
    ("[^\t\n ]\\([\t ]+\\)$"
     (1 'develock-whitespace-1 t))
    ;; spaces before tabs
    ("\\( +\\)\\(\t+\\)"
     (1 'develock-whitespace-1 t)
     (2 'develock-whitespace-2 t))
    ;; tab space tab
    ("\\(\t\\) \t"
     (1 'develock-whitespace-2 append))
    ;; only tabs or spaces in the line
    ("^[\t ]+$"
     (0 'develock-whitespace-2 append))
    ;; reachable E-mail addresses
    ("<?[-+.0-9A-Z_a-z]+@[-0-9A-Z_a-z]+\\(\\.[-0-9A-Z_a-z]+\\)+>?"
     (0 'develock-reachable-mail-address t))
    ;; things to be paid attention
    ("\\<\\(?:[Ff][Ii][Xx][Mm][Ee]\\|[Tt][Oo][Dd][Oo]\\)\\(?::\\|\\>\\)"
     (0 'develock-attention t)))
  "Extraordinary level highlighting for the Ruby mode."
  :type develock-keywords-custom-type
  :set 'develock-keywords-custom-set
  :group 'develock
  :group 'font-lock)

(defmacro develock-find-font-lock-defaults (modesym)
  "Get the font-lock defaults based on the major mode."
  (if (featurep 'xemacs)
      `(let ((defaults (or font-lock-defaults
			   (font-lock-find-font-lock-defaults ,modesym))))
	 (if (not (consp (car defaults)))
	     (setcar defaults (list (car defaults))))
	 defaults)
    `(let ((defaults
	     (or font-lock-defaults
		 (cdr (assq ,modesym
			    (symbol-value 'font-lock-defaults-alist))))))
       (cons (if (consp (car defaults))
		 (copy-sequence (car defaults))
	       (list (car defaults)))
	     (copy-sequence (cdr defaults))))))

(defmacro develock-set-defaults (defaults form)
  "Set fontification defaults appropriately for the current `major-mode'."
  (if (featurep 'xemacs)
      `(progn
	 (setq font-lock-defaults-computed nil
	       font-lock-keywords nil)
	 ,form)
    `(progn
       (set (make-local-variable 'font-lock-defaults) ,defaults)
       (setq font-lock-set-defaults nil)
       ,form)))

(defvar develock-inhibit-advice-font-lock-set-defaults nil
  "Non-nil means inhibit advising of the `font-lock-set-defaults' function.
In XEmacs, it is the `font-lock-set-defaults-1' function instead.
It will be set to t when the function is called recursively by itself.
Users should never modify the value.")

(defvar develock-original-fill-configuration nil
  "Internal variable used to save `fill-column' and `auto-fill-mode'.
It becomes buffer-local in the buffer in which Develock is on, and
keeps the values as a cons cell before Develock is turned on.")

(if (boundp 'file-local-variables-alist)
    ;; Emacs 23
    (progn
      (defadvice hack-dir-local-variables (after
					   hack-file-local-variables-alist
					   activate)
	"Advised by Develock.
Remove `fill-column' element from `file-local-variables-alist' if
`develock-fill-column-alist' specifies it."
	(and develock-mode
	     (cdr (assq major-mode develock-fill-column-alist))
	     (setq file-local-variables-alist
		   (delq (assq 'fill-column file-local-variables-alist)
			 file-local-variables-alist))))
      (defadvice hack-local-variables (after hack-file-local-variables-alist
					     activate)
	"Advised by Develock.
Merge `fill-column' local variable into `develock-fill-column-alist'."
	(let (old new)
	  (and develock-mode
	       (setq old (assq major-mode develock-fill-column-alist))
	       (setq new (assq 'fill-column file-local-variables-alist))
	       (not (eq (cdr old) (cdr new)))
	       (progn
		 (set (make-local-variable 'develock-fill-column-alist)
		      (copy-sequence develock-fill-column-alist))
		 (setcar (memq (assq major-mode develock-fill-column-alist)
			       develock-fill-column-alist)
			 (cons major-mode (cdr new)))
		 (setcar develock-original-fill-configuration (cdr new)))))))
  ;; Emacs 22 or earlier
  (defadvice hack-local-variables (around develock-allow-local-variables
					  activate)
    "Advised by Develock.
Merge `fill-column' local variable into `develock-fill-column-alist'."
    (let ((fc fill-column))
      ad-do-it
      (if (and (assq major-mode develock-fill-column-alist)
	       (not (eq fc fill-column)))
	  (progn
	    (set (make-local-variable 'develock-fill-column-alist)
		 (copy-sequence develock-fill-column-alist))
	    (setcar (memq (assq major-mode develock-fill-column-alist)
			  develock-fill-column-alist)
		    (cons major-mode fill-column))
	    (setcar develock-original-fill-configuration fill-column))))))

(let (current-load-list)
  (eval
   `(defadvice ,(if (featurep 'xemacs)
		    'font-lock-set-defaults-1
		  'font-lock-set-defaults)
      (around energize-font-lock-keywords activate)
      "Advised by Develock.
Energize font-lock keywords for extraordinary level highlighting."
      (if develock-inhibit-advice-font-lock-set-defaults
	  ad-do-it
	(let ((develock-inhibit-advice-font-lock-set-defaults t)
	      (def (cdr (assq major-mode develock-keywords-alist))))
	  (if def
	      (let* ((defaults (develock-find-font-lock-defaults major-mode))
		     (keywords (delq (car def) (car defaults)))
		     (max-decor (nth (1- (length keywords)) keywords))
		     (oldmode develock-mode)
		     (font-lock-maximum-decoration
		      font-lock-maximum-decoration))
		(setq develock-mode
		      (and (not (and
				 develock-ignored-buffer-name-regexp
				 (string-match
				  develock-ignored-buffer-name-regexp
				  (buffer-name))))
			   (not (and
				 buffer-file-name
				 develock-ignored-file-name-regexp
				 (string-match
				  develock-ignored-file-name-regexp
				  (file-name-nondirectory buffer-file-name))))
			   develock-auto-enable))
		(if develock-mode
		    (let (fc)
		      (setq font-lock-maximum-decoration t)
		      (set (car def)
			   (append (if (fboundp max-decor)
				       (funcall max-decor)
				     (symbol-value max-decor))
				   (symbol-value (car (cdr def)))))
		      (setcdr (nthcdr (1- (length keywords)) keywords)
			      (list (car def)))
		      (if (and (not oldmode)
			       (setq fc
				     (cdr (assq major-mode
						develock-fill-column-alist))))
			  (progn
			    (set (make-local-variable
				  'develock-original-fill-configuration)
				 (cons (current-fill-column)
				       (and auto-fill-function t)))
			    (setq fill-column fc)
			    (auto-fill-mode 1)))))
		(setcar defaults keywords)
		(develock-set-defaults defaults ad-do-it))
	    ad-do-it))))))

(if (featurep 'xemacs)
    ;; Highlight def* forms even if they aren't started from the
    ;; beginning of the line.
    (let ((def (car (car lisp-font-lock-keywords-1))))
      (if (and (stringp def)
	       (string-match "^\\^\\((\\\\(def\\)" def))
	  (setcar (car lisp-font-lock-keywords-1)
		  (replace-match "^[\t ]*\\1" nil nil def))))
  (defadvice font-lock-add-keywords (around inhibit-develock activate)
    "Run `font-lock-set-defaults' being not advised by Develock."
    (let ((develock-inhibit-advice-font-lock-set-defaults t))
      ad-do-it))
  (defadvice font-lock-remove-keywords (around inhibit-develock activate)
    "Run `font-lock-set-defaults' being not advised by Develock."
    (let ((develock-inhibit-advice-font-lock-set-defaults t))
      ad-do-it))

  ;; Those functions call `font-lock-set-defaults' in Emacs 22.
  (defadvice font-lock-fontify-buffer (around inhibit-develock activate)
    "Run `font-lock-set-defaults' being not advised by Develock."
    (let ((develock-inhibit-advice-font-lock-set-defaults t))
      ad-do-it))
  (defadvice font-lock-fontify-region (around inhibit-develock activate)
    "Run `font-lock-set-defaults' being not advised by Develock."
    (let ((develock-inhibit-advice-font-lock-set-defaults t))
      ad-do-it)))

(defmacro develock-max-column ()
  "Return the maximum column number from which lines won't be highlighted."
  '(let ((value (plist-get develock-max-column-plist major-mode)))
     (cond ((natnump value)
	    value)
	   ((eq 'w value)
	    (1- (window-width)))
	   ((eq t value)
	    (current-fill-column)))))

(eval-and-compile
  (or (fboundp 'kinsoku-bol-p)
      (fboundp 'kinsoku-longer)
      (load "kinsoku" t t)))

(defmacro develock-kinsoku-longer ()
  "Try to leap over the kinsoku characters.  Return t if leaping is done."
  (cond ((featurep 'xemacs)
	 (if (fboundp 'kinsoku-bol-p)
	     '(let (done)
		(while (and (not (eolp))
			    (kinsoku-bol-p))
		  (setq done t)
		  (forward-char 1))
		done)))
	((fboundp 'kinsoku-longer)
	 '(if enable-kinsoku
	      (let (done)
		(while (and (not (eolp))
			    (aref (char-category-set (following-char)) ?>))
		  (setq done t)
		  (forward-char 1))
		done)))))

(defun develock-find-long-lines (limit)
  "Function used to find a long line between the point and LIMIT.
If a long line is found, it returns non-nil and sets the `match-data'
#0, #1 and #2 with the beginning and the end positions to be
highlighted.  This function is affected by the values of
`develock-max-column-plist',
`develock-mode-ignore-long-message-header-list' and
`develock-inhibit-highlighting-kinsoku-chars'."
  (let ((max-column (develock-max-column))
	start inhibit end)
    (if (and
	 max-column
	 (progn
	   (if (memq major-mode
		     develock-mode-ignore-long-message-header-list)
	       (save-restriction
		 (widen)
		 (setq start (point))
		 (goto-char (point-min))
		 (if (or (not (re-search-forward
			       (concat "^\\("
				       (regexp-quote mail-header-separator)
				       "\\)?$")
			       nil t))
			 (> start (match-end 0)))
		     (goto-char start))
		 (setq start nil)))
	   (while (and (not start)
		       (< (point) limit))
	     (if (or (and (not (eolp))
			  (> (current-column) max-column))
		     (and (or (not (eolp))
			      (zerop (forward-line 1)))
			  (progn
			    (while (progn
				     (end-of-line)
				     (and (<= (point) limit)
					  (<= (current-column) max-column)
					  (zerop (forward-line 1)))))
			    (> (move-to-column (1+ max-column)) max-column))))
		 (progn
		   (if (> (move-to-column max-column) max-column)
		       (forward-char -1))
		   (setq start (min (point) limit))
		   (if (memq major-mode develock-mode-ignore-kinsoku-list)
		       nil
		     (setq inhibit
			   (and (develock-kinsoku-longer)
				develock-inhibit-highlighting-kinsoku-chars
				(eolp)))
		     (if (> (point) limit)
			 (goto-char limit))))
	       (forward-line 1)))
	   start)
	 (< start limit))
	(if inhibit
	    (progn
	      (goto-char (min (develock-point-at-eol) limit))
	      nil)
	  (setq start (point-marker)
		end (set-marker (make-marker)
				(min (develock-point-at-eol) limit)))
	  (or (memq (char-after) '(?\t ?\ ))
	      (skip-chars-backward "0-9A-Za-z"))
	  (if (> (current-column) max-column)
	      (if (> (move-to-column max-column) max-column)
		  (forward-char -1)))
	  (let ((mk (point-marker)))
	    (store-match-data (list mk end mk start start end)))
	  (goto-char end)))))

(defun develock-find-tab-or-long-space (limit)
  "Find tabs or a long space between the point and LIMIT.
If `indent-tabs-mode' is nil, this function highlights tabs.
Otherwise, it highlights spaces longer than `tab-width'.  It returns
non-nil and sets beginning and ending points as the `match-data' #0,
#1 and #2."
  (re-search-forward (if indent-tabs-mode
			 (concat "\\(\t*\\)\\("
				 (make-string tab-width ?\ )
				 "+\\)")
		       "\\(\t+\\)")
		     limit t))

(defun develock-find-ugly-change-log-entry-line (limit)
  "Find an ugly entry line between the point and LIMIT in ChangeLog file."
  (and (re-search-forward "\
^\\( +\\|\t[\t ]*\\)?\\(?:[12][0-9][0-9][0-9]-[01][0-9]-[0-3][0-9]\
\\(?:\\(?: \\|\\(  +\\|\t[\t ]*\\)\\)[012][0-9]:[0-5][0-9]:[0-5][0-9]\\)?\
\\|\\(?:Sun\\|Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\)\
\\(?: \\|\\(  +\\|\t[\t ]*\\)\\)\
\\(?:Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\)\
\\(?: \\|\\(  +\\|\t[\t ]*\\)\\)[ 0-3][0-9]\
\\(?: \\|\\(  +\\|\t[\t ]*\\)\\)[012][0-9]:[0-5][0-9]:[0-5][0-9]\
\\(?: \\|\\(  +\\|\t[\t ]*\\)\\)[12][0-9][0-9][0-9]\\)\
\\(?:  \\|\\( \\|   +\\|\t[\t ]*\\)\\)[^\t <>]+\
\\(?:\\(?: \\|\\(  +\\|\t[\t ]*\\)\\)[^\t <>]+\\)?\
\\(?:\\(?: \\|\\(  +\\|\t[\t ]*\\)\\)[^\t <>]+\\)?\
\\(\\(?:[\t ]+[^\t <>]+\\)*\\)\
\\(?:\\(?:  \\|\\( \\|   +\\|\t[\t ]*\\)\\)<\\|\\(<\\)\\)[^<>]+>\
\\(?:\\(?:  \\|\\( \\|   +\\|\t[\t ]*\\)\\)\(\\|\\([^\n ].*\\)\\)?"
			  limit t)
       (prog2
	   (goto-char (match-beginning 0))
	   (or (not (zerop (forward-line -1)))
	       (save-match-data (looking-at "[\t ]*$")))
	 (goto-char (match-end 0)))))

(eval-when-compile
  (defvar lisp-interaction-mode-hook))

(eval-when-compile
  (defmacro develock-called-interactively-p (kind)
    (if (fboundp 'called-interactively-p)
	`(called-interactively-p ,kind)
      '(interactive-p))))

;;;###autoload
(defun develock-mode (arg)
  "Toggle Develock mode.
With ARG, turn Develock on if and only if ARG is positive.
In Develock mode, highlighting leading and trailing whitespace,
long lines and oddities."
  (interactive "P")
  (if (assq major-mode develock-keywords-alist)
      (cond ((and (not develock-mode)
		  develock-ignored-buffer-name-regexp
		  (string-match develock-ignored-buffer-name-regexp
				(buffer-name)))
	     (if (develock-called-interactively-p 'any)
		 (message "Develock is inhibited for this buffer")))
	    ((and (not develock-mode)
		  buffer-file-name
		  develock-ignored-file-name-regexp
		  (string-match develock-ignored-file-name-regexp
				(file-name-nondirectory buffer-file-name)))
	     (if (develock-called-interactively-p 'any)
		 (message "Develock is inhibited for this file")))
	    (t
	     (let ((oldmode develock-mode)
		   fc)
	       (setq develock-mode (if (numberp arg)
				       (> arg 0)
				     (if font-lock-mode
					 (not develock-mode)
				       t)))
	       (if develock-mode
		   (if (and (not oldmode)
			    (setq fc (cdr (assq major-mode
						develock-fill-column-alist))))
		       (progn
			 (set (make-local-variable
			       'develock-original-fill-configuration)
			      (cons (current-fill-column)
				    (and auto-fill-function t)))
			 (setq fill-column fc)
			 (auto-fill-mode 1)))
		 (if (and oldmode
			  (local-variable-p
			   'develock-original-fill-configuration
			   (current-buffer))
			  (car develock-original-fill-configuration))
		     (progn
		       (setq fill-column
			     (car develock-original-fill-configuration))
		       (auto-fill-mode
			(if (cdr develock-original-fill-configuration)
			    1
			  0))))))
	     (if font-lock-mode
		 (progn
		   (font-lock-mode 0)
		   (let ((develock-auto-enable develock-mode))
		     (font-lock-mode 1)))
	       (if develock-mode
		   (let ((develock-auto-enable t))
		     (font-lock-mode 1))))))
    (error "Develock does not support `%s'" major-mode)))

;;;###autoload
(defun turn-on-develock ()
  "Turn on font-lock after splashing the startup screen under XEmacs.
This function is used for `lisp-interaction-mode-hook' exclusively."
  (if (and (boundp 'inhibit-warning-display)
	   (symbol-value 'inhibit-warning-display))
      (add-hook 'pre-command-hook 'turn-on-develock)
    (let ((hooks (cond ((consp lisp-interaction-mode-hook)
			(memq 'turn-on-develock lisp-interaction-mode-hook))
		       ((eq 'turn-on-develock lisp-interaction-mode-hook)
			(setq lisp-interaction-mode-hook
			      '(turn-on-develock))))))
      (if hooks
	  (setcar hooks 'turn-on-font-lock)))
    (if (and (consp pre-command-hook)
	     (memq 'turn-on-develock pre-command-hook))
	(save-excursion
	  (remove-hook 'pre-command-hook 'turn-on-develock)
	  (let ((buffers (buffer-list)))
	    (while buffers
	      (set-buffer (car buffers))
	      (if (eq 'lisp-interaction-mode major-mode)
		  (font-lock-set-defaults))
	      (setq buffers (cdr buffers)))))
      (font-lock-set-defaults))))


;; The following functions are useful to make source codes clean.

(defvar develock-advice-plist nil
  "Internal variable used to keep advice definitions.")

(defcustom develock-energize-functions-plist
  '(indent-region t lisp-indent-line t c-indent-line t newline-and-indent nil)
  "Plist of commands and flags; command is advised when flag is non-nil.
When command is advised, it removes useless whitespace."
  :type
  '(list
    :convert-widget
    (lambda (widget)
      "This function generates the following type of a widget:
\(list (group :inline t
	     (const :format \"-SPACES-%v: \" FUNCTION)
	     (boolean :format \"%[%v%]\\n\" :on \"t\" :off \"nil\"))
      [...])"
      (let ((value (default-value 'develock-energize-functions-plist))
	    (maxlen 0)
	    fn rest)
	(while value
	  (setq fn (car value)
		value (nthcdr 2 value)
		rest (nconc rest (list fn))
		maxlen (max (length (symbol-name fn)) maxlen)))
	(setq value rest
	      rest nil)
	(while value
	  (setq fn (car value)
		value (cdr value)
		rest (nconc
		      rest
		      `((group
			 :inline t
			 (const :format
				,(concat (make-string
					  (- maxlen (length (symbol-name fn)))
					  ?\ )
					 "%v: ")
				,fn)
			 (boolean :format "%[%v%]\n" :on "t" :off "nil"))))))
	`(list :args ,rest))))
  :set (lambda (symbol value)
	 (prog1
	     (if (fboundp 'custom-set-default)
		 (custom-set-default symbol value)
	       (set-default symbol value))
	   (if (and (not noninteractive)
		    (featurep 'develock))
	       (let (fn flag)
		 (while value
		   (setq fn (car value)
			 flag (car (cdr value))
			 value (cdr (cdr value)))
		   (condition-case nil
		       (if flag
			   (ad-add-advice fn
					  (plist-get develock-advice-plist fn)
					  'around 'first)
			 (ad-remove-advice fn 'around
					   'remove-useless-whitespace))
		     (error)))))))
  :group 'develock)

(eval-and-compile
  (or (fboundp 'develock-Orig-lisp-indent-line)
      (defalias 'develock-Orig-lisp-indent-line
	;; The genuine function definition of `lisp-indent-line'.
	(or (ad-real-orig-definition 'lisp-indent-line)
	    (symbol-function 'lisp-indent-line)))))

(defun develock-lisp-indent-line (&optional whole-exp)
  "Internal function used to advise some Lisp indent functions."
  (save-restriction
    (widen)
    (let ((st (set-marker (make-marker) (point)))
	  pt mod orig)
      (save-excursion
	(end-of-line)
	(setq pt (point))
	(or (zerop (skip-chars-backward "\t "))
	    (nth 3 (parse-partial-sexp (point-min) (point)))
	    (delete-region (point) pt)))
      (if (nth 3 (parse-partial-sexp (point-min) (develock-point-at-bol)))
	  (develock-Orig-lisp-indent-line whole-exp)
	(save-excursion
	  (beginning-of-line)
	  (or (setq mod (buffer-modified-p))
	      (setq orig (progn
			   (looking-at "[\t ]*")
			   (match-string 0))))
	  (insert "\n")
	  (forward-char -1)
	  (save-restriction
	    (widen)
	    (develock-Orig-lisp-indent-line whole-exp))
	  (if (prog1
		  (bolp)
		(delete-char 1)
		(setq pt (point)))
	      (set-buffer-modified-p mod)
	    (or (zerop (skip-chars-forward "\t "))
		(delete-region pt (point)))
	    (or mod
		(progn
		  (beginning-of-line)
		  (if (string-equal orig (progn
					   (looking-at "[\t ]*")
					   (match-string 0)))
		      (set-buffer-modified-p nil))))))
	(goto-char st)
	(set-marker st nil)
	(if (bolp) (skip-chars-forward "\t "))))))

(defun develock-Orig-c-indent-line (&optional syntax quiet ignore-point-pos)
  "This function should be redefined to the genuine `c-indent-line'."
  (let* ((fn (or (ad-real-orig-definition 'c-indent-line)
		 (symbol-function 'c-indent-line)))
	 ;; Checking how many arguments `c-indent-line' accepts.
	 (nargs (length (ad-arglist fn))))
    (cond ((= nargs 4)
	   ;; The genuine function definition of `c-indent-line'.
	   (defalias 'develock-Orig-c-indent-line fn))
	  ((= nargs 3)
	   (defalias 'develock-Orig-c-indent-line
	     `(lambda (&optional syntax quiet ignore-point-pos) "\
Identical to `c-indent-line', but the optional argument
IGNORE-POINT-POS is ignored."
		(funcall ,fn syntax quiet))))
	  (t
	   (defalias 'develock-Orig-c-indent-line
	     `(lambda (&optional syntax quiet ignore-point-pos) "\
Identical to `c-indent-line', but the optional arguments QUIET and
IGNORE-POINT-POS are ignored."
		(funcall ,fn syntax))))))
  (if (and (featurep 'bytecomp)
	   (eq (car-safe (symbol-function 'develock-Orig-c-indent-line))
	       'lambda))
      (byte-compile 'develock-Orig-c-indent-line))
  (develock-Orig-c-indent-line syntax quiet ignore-point-pos))

(eval-when-compile
  (defvar c-syntactic-indentation))

(eval-and-compile
  (if (not (fboundp 'c-guess-basic-syntax))
      (defalias 'c-guess-basic-syntax 'ignore)))

(defun develock-c-indent-line (&optional syntax quiet ignore-point-pos)
  "Internal function used to advise some C indent functions."
  (save-restriction
    (widen)
    (if (and (or (not (boundp 'c-syntactic-indentation))
		 c-syntactic-indentation)
	     (assq 'string (c-guess-basic-syntax)))
	;; Since this line looks like a continued string, we do nothing
	;; in the line beginning area.
	(progn
	  (if (eq this-command 'c-indent-command)
	      ;; Except, we remove trailing whitespace when the command
	      ;; `c-indent-command' is performed.
	      (save-excursion
		(end-of-line)
		(let ((pt (point)))
		  (or (zerop (skip-chars-backward "\t "))
		      (delete-region (point) pt)))))
	  (develock-Orig-c-indent-line syntax quiet ignore-point-pos))
      (let ((move (string-match "\\`[\t ]*\\'"
				(buffer-substring (develock-point-at-bol)
						  (point))))
	    pt mod orig)
	(save-excursion
	  (end-of-line)
	  (setq pt (point))
	  (or (zerop (skip-chars-backward "\t "))
	      (delete-region (point) pt))
	  (beginning-of-line)
	  (looking-at "[\t ]*")
	  (or (setq mod (buffer-modified-p))
	      (setq orig (match-string 0)))
	  (if indent-tabs-mode
	      (tabify (point) (match-end 0))
	    (untabify (point) (match-end 0)))
	  (develock-Orig-c-indent-line syntax quiet ignore-point-pos)
	  (setq pt (point))
	  (or mod
	      (progn
		(beginning-of-line)
		(if (string-equal orig (progn
					 (looking-at "[\t ]*")
					 (match-string 0)))
		    (set-buffer-modified-p nil)))))
	(if move
	    (goto-char pt))))))

(let (current-load-list)
  (defadvice indent-region (around remove-useless-whitespace
				   (start end column) activate)
    "Advised by Develock.
If Develock is on, remove useless leading and trailing whitespace in
Lisp modes, C modes and Java mode.  You can turn off this advice
permanently by customizing the `develock-energize-functions-plist'
variable."
    (if (and develock-mode font-lock-mode
	     (plist-get develock-energize-functions-plist 'indent-region)
	     (memq major-mode '(emacs-lisp-mode
				lisp-interaction-mode
				c-mode c++-mode java-mode jde-mode)))
	(save-excursion
	  ;; Meddle with out of the region.
	  (goto-char end)
	  (while (and (zerop (forward-line 1))
		      (looking-at "[\t ]+$")))
	  (let ((to (point))
		(fn (cdr (assq
			  major-mode
			  '((emacs-lisp-mode . develock-lisp-indent-line)
			    (lisp-interaction-mode . develock-lisp-indent-line)
			    (c-mode . develock-c-indent-line)
			    (c++-mode . develock-c-indent-line)
			    (java-mode . develock-c-indent-line)
			    (jde-mode . develock-c-indent-line))))))
	    (goto-char start)
	    (while (and (zerop (forward-line -1))
			(or (looking-at "[\t ]+$")
			    (progn
			      (forward-line 1)
			      nil))))
	    (save-restriction
	      (if (prog1
		      (zerop (forward-line -1))
		    (narrow-to-region (point) to))
		  (forward-line 1))
	      (while (not (eobp))
		(or (eolp)
		    (progn
		      (funcall fn)
		      (if (and (not (bolp))
			       (eolp))
			  (delete-region (develock-point-at-bol) (point)))))
		(forward-line 1)))))
      ad-do-it))

  (defadvice lisp-indent-line (around remove-useless-whitespace
				      (&optional whole-exp) activate)
    "Advised by Develock.
If Develock is on, remove useless leading and trailing whitespace.
You can turn off this advice permanently by customizing the
`develock-energize-functions-plist' variable."
    (if (and develock-mode font-lock-mode
	     (plist-get develock-energize-functions-plist 'lisp-indent-line))
	(develock-lisp-indent-line whole-exp)
      ad-do-it))

  (defadvice c-indent-line (around remove-useless-whitespace
				   (&optional syntax quiet ignore-point-pos)
				   activate)
    "Advised by Develock.
If Develock is on, remove useless leading and trailing whitespace.
You can turn off this advice permanently by customizing the
`develock-energize-functions-plist' variable."
    (if (and develock-mode font-lock-mode
	     (plist-get develock-energize-functions-plist 'c-indent-line))
	(develock-c-indent-line syntax quiet ignore-point-pos)
      ad-do-it))

  (defadvice newline-and-indent (around remove-useless-whitespace activate)
    "Advised by Develock.
If Develock is on, remove useless leading and trailing whitespace and
indent appropriately in Lisp modes, C modes and Java mode.  You can
turn off this advice permanently by customizing the
`develock-energize-functions-plist' variable."
    (if (and develock-mode font-lock-mode
	     (plist-get develock-energize-functions-plist 'newline-and-indent))
	(cond ((memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
	       (develock-lisp-indent-line))
	      ((memq major-mode '(c-mode c++-mode java-mode jde-mode))
	       (develock-c-indent-line))))
    ad-do-it))

(let ((plist develock-energize-functions-plist)
      fn flag def)
  (while plist
    (setq fn (car plist)
	  flag (car (cdr plist))
	  plist (cdr (cdr plist)))
    (if (setq def (assq 'remove-useless-whitespace
			(cdr (assq 'around (ad-get-advice-info fn)))))
	(setq develock-advice-plist (plist-put develock-advice-plist fn def)))
    (if (not flag)
	(ad-remove-advice fn 'around 'remove-useless-whitespace))))

(provide 'develock)

;;; develock.el ends here
