;;; xmtn-base.el --- Basic definitions for accessing monotone

;; Copyright (C) 2009, 2010 Stephen Leake
;; Copyright (C) 2006, 2007, 2009 Christian M. Ohler

;; Author: Christian M. Ohler
;; Keywords: tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA  02110-1301  USA.

;;; Commentary:

;; This file contains basic definitions for accessing the distributed
;; version control system monotone.

;;; Code:

;;; There are some notes on the design of xmtn and its related
;;; files in docs/xmtn-readme.txt.

(eval-and-compile
  (require 'cl))

(defvar xmtn-executable "mtn"
  "*The monotone executable command.")

(defvar xmtn-additional-arguments '()
  "*Additional arguments to pass to monotone.

A list of strings.")

(defvar xmtn-confirm-operation t
  "May be let-bound to nil to bypass confirmations.")

(deftype xmtn--hash-id ()
  `(and string
        (satisfies xmtn--hash-id-p)))

(defun xmtn--hash-id-p (thing)
  (and (stringp thing)
       ;; This is twenty times faster than an equivalent Elisp loop.
       (save-match-data
         (string-match "\\`[0-9a-f]\\{40\\}\\'" thing))))

(defun xmtn--filter-non-ws (dir)
  "Return list of all mtn workspaces in DIR."
  (let ((default-directory dir)
        (subdirs (directory-files dir)))
    (setq subdirs
          (mapcar (lambda (filename)
                    (if (and (file-directory-p filename)
                             (not (string= "." filename))
                             (not (string= ".." filename))
			     (file-directory-p (concat filename "/_MTN")))
                        filename))
                  subdirs))
    (delq nil subdirs)))

(defvar xmtn--*enable-assertions* nil
  "Effective at macroexpansion time.")

(defmacro xmtn--assert-for-effect (form &rest more-assert-args)
  (if xmtn--*enable-assertions*
      `(assert ,form ,@more-assert-args)
    `(progn ,form nil)))

(defmacro xmtn--assert-optional (form &rest more-assert-args)
  (if xmtn--*enable-assertions*
      `(assert ,form ,@more-assert-args)
    `nil))

(defmacro xmtn--assert-nil ()
  `(assert nil))

(provide 'xmtn-base)

;;; xmtn-base.el ends here
