;;; cc-fix.el --- compatibility library for old (X)Emacs versions

;; Copyright (C) 1985,1987,1992-2003, 2004, 2005, 2006 Free Software
;; Foundation, Inc.

;; Authors:    2003- Alan Mackenzie
;;             1998- Martin Stjernholm
;;             1997-1999 Barry A. Warsaw
;; Maintainer: bug-cc-mode@gnu.org
;; Created:    03-Jul-1997 (as cc-mode-19.el)
;; Version:    See cc-mode.el
;; Keywords:   c languages oop

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file is necessary in order to run CC Mode in older (X)Emacs
;; versions.  It's not needed at all for the latest versions of Emacs
;; and XEmacs.

;;; Code:

(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-dest-file)
		  (stringp byte-compile-dest-file))
	     (cons (file-name-directory byte-compile-dest-file) load-path)
	   load-path)))
    (load "cc-bytecomp" nil t)))

;; Silence the compiler (in case this file is compiled by other
;; Emacsen even though it isn't used by them).
(cc-bytecomp-obsolete-fun byte-code-function-p)
(cc-bytecomp-defun regexp-opt-depth)

(cc-external-require 'advice)

;; Emacs 20.n doesn't have the macros push and pop.  Here're the Emacs 21
;; definitions.
(or (fboundp 'push)
    (defmacro push (newelt listname)
      "Add NEWELT to the list stored in the symbol LISTNAME.
This is equivalent to (setq LISTNAME (cons NEWELT LISTNAME)).
LISTNAME must be a symbol."
      (list 'setq listname
	    (list 'cons newelt listname))))

(or (fboundp 'pop)
    (defmacro pop (listname)
      "Return the first element of LISTNAME's value, and remove it from the list.
LISTNAME must be a symbol whose value is a list.
If the value is nil, `pop' returns nil but does not actually
change the list."
      (list 'prog1 (list 'car listname)
	    (list 'setq listname (list 'cdr listname)))))


(if (/= (regexp-opt-depth "\\(\\(\\)\\)") 2)
    (progn
      ;; Emacs 21.1 has a buggy regexp-opt-depth which prevents CC
      ;; Mode building.  Those in Emacs 21.[23] are not entirely
      ;; accurate.  The following definition comes from Emacs's
      ;; regexp-opt.el CVS version 1.25 and is believed to be a
      ;; rigorously correct implementation.
      (defconst regexp-opt-not-groupie*-re
	(let* ((harmless-ch "[^\\\\[]")
	       (esc-pair-not-lp "\\\\[^(]")
	       (class-harmless-ch "[^][]")
	       (class-lb-harmless "[^]:]")
	       (class-lb-colon-maybe-charclass ":\\([a-z]+:]\\)?")
	       (class-lb (concat "\\[\\(" class-lb-harmless
				 "\\|" class-lb-colon-maybe-charclass "\\)"))
	       (class
		(concat "\\[^?]?"
			"\\(" class-harmless-ch
			"\\|" class-lb "\\)*"
			"\\[?]")) ; special handling for bare [ at end of re
	       (shy-lp "\\\\(\\?:"))
	  (concat "\\(" harmless-ch "\\|" esc-pair-not-lp
		  "\\|" class "\\|" shy-lp "\\)*"))
	"Matches any part of a regular expression EXCEPT for non-shy \"\\\\(\"s")

      (defun regexp-opt-depth (regexp)
	"Return the depth of REGEXP.
This means the number of regexp grouping constructs (parenthesised expressions)
in REGEXP."
	(save-match-data
	  ;; Hack to signal an error if REGEXP does not have balanced
	  ;; parentheses.
	  (string-match regexp "")
	  ;; Count the number of open parentheses in REGEXP.
	  (let ((count 0) start)
	    (while
		(progn
		  (string-match regexp-opt-not-groupie*-re regexp start)
		  (setq start ( + (match-end 0) 2)) ; +2 for "\\(" after match-end.
		  (<= start (length regexp)))
	      (setq count (1+ count)))
	    count)))
      ))

;; Some XEmacs versions have a bug in which font-lock-compile-keywords
;; overwrites the variable font-lock-keywords with its result.  This causes
;; havoc when what the function is compiling is font-lock-SYNTACTIC-keywords,
;; hence....
(eval-after-load "font-lock"
  '(when (and (featurep 'xemacs) ; There is now (2005/12) code in GNU Emacs CVS
				 ; to make the call to f-l-c-k throw an error.
	      (let (font-lock-keywords)
		(font-lock-compile-keywords '("\\<\\>"))
		font-lock-keywords))	; did the previous call foul this up?
     (defun font-lock-compile-keywords (keywords)
       "Compile KEYWORDS (a list) and return the list of compiled keywords.
Each keyword has the form (MATCHER HIGHLIGHT ...).  See `font-lock-keywords'."
       (if (eq (car-safe keywords) t)
	   keywords
	 (cons t (mapcar 'font-lock-compile-keyword keywords))))
     (defadvice font-lock-fontify-keywords-region (before c-compile-font-lock-keywords
							  activate preactivate)
       (unless (eq (car-safe font-lock-keywords) t)
	 (setq font-lock-keywords
	       (font-lock-compile-keywords font-lock-keywords))))
     ))


(cc-provide 'cc-fix)
;;; cc-fix.el ends here
