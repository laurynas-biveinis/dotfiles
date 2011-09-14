;;; cc-lobotomy.el --- excise portions of cc-mode's brain... for speed

;; Copyright (C) 1985,1987,1992-2003, 2005, 2006 Free Software Foundation,
;; Inc.

;; Author:     1995 Barry A. Warsaw
;; Maintainer: Unmaintained
;; Created:    March 1995, split from cc-mode.el
;; Version:    See cc-mode.el
;; Keywords:   c languages oop

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Every effort has been made to improve the performance of
;; cc-mode. However, due to the nature of the C, C++, and Objective-C
;; language definitions, a trade-off is often required between
;; accuracy of construct recognition and speed. I believe it is always
;; best to be correct, and that the mode is currently fast enough for
;; most normal usage.  Others disagree.	 I have no intention of
;; including these hacks in the main distribution.  When cc-mode
;; version 5 comes out, it will include a rewritten indentation engine
;; so that performance will be greatly improved automatically.	This
;; was not included in this release of version 4 so that Emacs 18
;; could still be supported.  Note that this implies that cc-mode
;; version 5 will *not* work on Emacs 18!
;;
;; To use, see the variable cc-lobotomy-pith-list and the function
;; cc-lobotomize.   The variable contains a good explanation of the
;; speed/accuracy trade-offs for each option.  Set it to what you'd
;; like, and call cc-lobotomy in your c-mode-hook.
;;
;; This will redefine certain cc-mode functions and affect all cc-mode
;; buffers globally.
;;
;; This file is completely unsupported!	 Although it has been patched
;; superficially to keep pace with the rest of CC Mode, it hasn't been
;; tested for a long time.

;;; Code:

(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-dest-file)
		  (stringp byte-compile-dest-file))
	     (cons (file-name-directory byte-compile-dest-file) load-path)
	   load-path)))
    (load "cc-bytecomp" nil t)))

(cc-require 'cc-defs)
(cc-require 'cc-engine)
(cc-require 'cc-cmds)


(defvar cc-lobotomy-pith-list ()
  "*List of things to dumb-ify to speed up cc-mode.  Note that each
incurs a penalty in correct identification of certain code constructs.
Possible values to put on this list:

  'literal -- `c-in-literal' is lobotomized.  This will significantly
	      speed up parsing over large lists of cpp macros, as seen
	      for instance in header files.  The penalty is that you
	      cannot put the `#' character as the first non-whitespace
	      character on a line inside other multi-line literals
	      (i.e. comments or strings)

  'class   -- `c-narrow-out-enclosing-class' and `c-search-uplist for
	      classkey' are lobotomized.  This speeds up some
	      indenting inside and around class and struct
	      definitions.  The penalty is that elements inside of
	      classes and structs may not indent correctly.

  'lists   -- `c-inside-bracelist-p' is lobotomized.  This speeds up
	      indenting inside and around brace lists (e.g. aggregate
	      initializers, enum lists, etc.).	The penalty is that
	      elements inside these lists may not indent correctly.")

(defun cc-lobotomize ()
  "Perform lobotomies on cc-mode as described in `cc-lobotomy-pith-list'."
  (let (pithedp)
    (if (memq 'literal cc-lobotomy-pith-list)
	(progn
	  (fset 'c-in-literal 'cc-in-literal-lobotomized)
	  (setq pithedp t)))
    (if (memq 'class cc-lobotomy-pith-list)
	(progn
	  (fset 'c-narrow-out-enclosing-class
		'cc-narrow-out-enclosing-class-lobotomized)
	  (fset 'c-search-uplist-for-classkey
		'cc-search-uplist-for-classkey-lobotomized)
	  (setq pithedp t)))
    (if (memq 'lists cc-lobotomy-pith-list)
	(progn
	  (fset 'c-inside-bracelist-p 'cc-inside-bracelist-p-lobotomized)
	  (setq pithedp t)))
    (if pithedp
	(add-hook 'c-prepare-bug-report-hooks 'cc-lobo-bug-report-blurb))
    ))


;; This is a faster version of c-in-literal.  It trades speed for one
;; approximation, namely that within other literals, the `#' character
;; cannot be the first non-whitespace on a line.  This only happens if
;; detect-cpp is non-nil, which isn't very often.
(defun cc-in-literal-lobotomized (&optional lim detect-cpp)
  ;; first check the cache
  (if (and (vectorp c-in-literal-cache)
	   (= (point) (aref c-in-literal-cache 0)))
      (aref c-in-literal-cache 1)
    ;; quickly check for cpp macro. this breaks if the `#' character
    ;; appears as the first non-whitespace on a line inside another
    ;; literal.
    (let* (state
	   (char-at-boi (char-after (c-point 'boi)))
	   (rtn (cond
		 ((and detect-cpp char-at-boi (= char-at-boi ?#))
		  'pound)
		 ((nth 3 (setq state (save-excursion
				       (parse-partial-sexp
					(or lim (c-point 'bod))
					(point)))))
		  'string)
		 ((nth 4 state) (if (nth 7 state) 'c++ 'c))
		 (t nil))))
      ;; cache this result if the cache is enabled
      (if (not c-in-literal-cache)
	  (setq c-in-literal-cache (vector (point) rtn)))
      rtn)))

(defun cc-narrow-out-enclosing-class-lobotomized (dummy1 dummy2) nil)

(defun cc-search-uplist-for-classkey-lobotomized (dummy) nil)

(defun cc-inside-bracelist-p-lobotomized (dummy1 dummy2) nil)

(defun cc-lobo-bug-report-blurb ()
  (insert
   "\nYou are using cc-lobotomy.el.  You realize that by doing\n"
   "so you have already made the decision to trade off accuracy\n"
   "for speed?	Don't set your hopes too high that your problem\n"
   "will be fixed.\n\n"))


(cc-provide 'cc-lobotomy)
;;; cc-lobotomy.el ends here
