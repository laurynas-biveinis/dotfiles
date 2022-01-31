;;; helm-tree-sitter-utils.el --- Helm interface for tree-sitter -*- lexical-binding: t -*-

;; Copyright (C) 2021 Giedrius Jonikas <giedriusj1@gmail.com>

;; Author: Giedrius Jonikas <giedriusj1@gmail.com>
;; Version: 0.1.0
;; URL: https://gitlab.com/giedriusj1/helm-tree-sitter

;; Package-Requires: ((emacs "25.1") (helm "3.6.2") (tree-sitter "0.16.1"))

;; This program is free software: you can redistribute it and/or modify
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
;; Simple helm interface to tree-sitter.

;;; Commentary:
;; Utility functions, mainly dealing with strings and copying propertized
;; text from buffers.

;;; Code:

(require 'tsc)
(require 'helm-tree-sitter-core)

(defun helm-tree-sitter-utils-node-children-to-alist (node)
  "Helm-tree-sitter utility function for creating an alist of nodes children.
Argument NODE is `helm-tree-sitter-core-elem' representing the node."
  (let ((al '()))
    (dotimes (e (tsc-count-named-children node))
      (let* ((child-node (tsc-get-nth-named-child node e)))
        (setf (alist-get (tsc-node-type child-node) al) child-node)))
    al))

(defun helm-tree-sitter-utils-strip-newlines-and-whitespaces (str)
  "Helm-tree-sitter utility function for stripping off newlines and whitespaces.
Argument STR is a string."

  (unless (stringp str)
    (signal 'wrong-type-argument (list 'stringp str)))

  (replace-regexp-in-string
   "\n" ""
   (replace-regexp-in-string
    "[\s\t]+" " "
    str)))

;; Copy text from buffer between node-start-byte and node-end-byte.
;; We use this instead of (tsc-node-text node), because this way
;; we can get fontified text.
(defun helm-tree-sitter-utils-get-node-text (node)
  "Helm-tree-sitter utility function for copying propertized text from buffer.
Argument NODE is tree-sitter node.
Empty string is returned if no node is provided."

  (if (tsc-node-p node)
      (buffer-substring
       (tsc-node-start-position node)
       (tsc-node-end-position node))
    ""))

;; Same as function above, but we'll return nil if no node is
;; provided.
(defun helm-tree-sitter-utils-get-node-text-or-nil (node)
  "Helm-tree-sitter utility function for copying propertized text from buffer.
Argument NODE is tree-sitter node."
  (when (tsc-node-p node)
    (buffer-substring
     (tsc-node-start-position node)
     (tsc-node-end-position node))))

(defun helm-tree-sitter-utils-append-space-if-not-empty(str)
  "Append space if provided string is not empty.
Argument STR is a string."
  (if (not (helm-tree-sitter-utils-empty-string str))
      (concat str " ") str))

(defun helm-tree-sitter-utils-prepend-if-not-empty (str prepend)
  "Prepend to string if provided string is not empty.
Arguments STR and PREPEND are strings."
  (when (not (= (length str) 0))
    (concat prepend str)))

(defun helm-tree-sitter-utils-strip-newlines (str)
  "Strip newlines from provided string.
Argument STR is a string."
  (replace-regexp-in-string "\n" "" str))

(defun helm-tree-sitter-utils-empty-string (str)
  "Return t if provided string is empty.
Argument STR is a string."
  (when (stringp str)
    (= (length str) 0)))

(defun helm-tree-sitter-utils-prepend-depth-if-needed (elem)
  "Prepend some spaces to helm candidate.
This helps to identify depth of the node in question.
Argument ELEM is `helm-tree-sitter-core-elem' representing the node."
  (if (< (helm-tree-sitter-core-elem-depth elem) 2) ""
    (concat
     (make-string (helm-tree-sitter-core-elem-depth elem) ?\ )
     "├► ")))

(provide 'helm-tree-sitter-utils)
;;; helm-tree-sitter-utils.el ends here
