;;; xmtn-compat.el --- xmtn compatibility with different Emacs versions

;; Copyright (C) 2008, 2009 Stephen Leake
;; Copyright (C) 2006, 2007 Christian M. Ohler

;; Author: Christian M. Ohler
;; Keywords: extensions

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

;; Wrappers and fallback implementations for various Emacs functions
;; needed by xmtn that don't exist in all versions of Emacs.

;;; Code:

;;; There are some notes on the design of xmtn in
;;; docs/xmtn-readme.txt.

(eval-and-compile
  (require 'cl))

(defmacro xmtn--set-process-query-on-exit-flag (process value)
  (if (fboundp 'set-process-query-on-exit-flag)
      ;; emacs 22.2 and greater
      `(set-process-query-on-exit-flag ,process ,value)
    `(progn
       ;; emacs 22.1
       (process-kill-without-query ,process ,value)
       ,value)))

(provide 'xmtn-compat)

;;; xmtn-compat.el ends here
