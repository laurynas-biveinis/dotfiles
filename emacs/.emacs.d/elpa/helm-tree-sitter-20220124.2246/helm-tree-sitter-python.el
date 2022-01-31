;;; helm-tree-sitter-python.el --- Helm interface for tree-sitter -*- lexical-binding: t -*-

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
;; Provides function for dealing with Python code

;;; Code:

(require 'helm-tree-sitter-utils)

(defvar helm-tree-sitter-python-candidate-producer
  '(("import_statement"      . helm-tree-sitter-python-import-statement-fn)
    ("import_from_statement" . helm-tree-sitter-python-import-statement-fn)
    ("function_definition"   . helm-tree-sitter-python-function-definition-fn)
    ("class_definition"      . helm-tree-sitter-python-class-definition-fn)))

(defun helm-tree-sitter-python-import-statement-fn (elem)
  "Helm-tree-sitter handler for import_statement nodes in python mode.
Argument ELEM is `helm-tree-sitter-core-elem' representing the node."
    
  (unless (helm-tree-sitter-core-elem-p elem)
    (signal 'wrong-type-argument (list 'helm-tree-sitter-core-elem-p elem)))

  (concat
   (helm-tree-sitter-utils-prepend-depth-if-needed elem)
   (propertize "Dependency: "
               'face 'italic)

   (helm-tree-sitter-utils-get-node-text (helm-tree-sitter-core-elem-node elem))))

(defun helm-tree-sitter-python-function-definition-fn (elem)
  "Helm-tree-sitter handler for function_definition nodes in python mode.
Argument ELEM is `helm-tree-sitter-core-elem' representing the node."
  
  (unless (helm-tree-sitter-core-elem-p elem)
    (signal 'wrong-type-argument (list 'helm-tree-sitter-core-elem-p elem)))

  (let* ((children-alist (helm-tree-sitter-utils-node-children-to-alist (helm-tree-sitter-core-elem-node elem)))
         (identifier (helm-tree-sitter-utils-get-node-text (alist-get 'identifier children-alist)))
         (parameters (helm-tree-sitter-utils-get-node-text (alist-get 'parameters children-alist))))

    (concat
     (helm-tree-sitter-utils-prepend-depth-if-needed elem)
     (propertize "Function: "
                 'face 'italic)
     (concat
      identifier
      (helm-tree-sitter-utils-strip-newlines-and-whitespaces parameters)))))


(defun helm-tree-sitter-python-class-definition-fn (elem)
  "Helm-tree-sitter handler for class_definition nodes in python mode.
Argument ELEM is `helm-tree-sitter-core-elem' representing the node."
  
  (unless (helm-tree-sitter-core-elem-p elem)
    (signal 'wrong-type-argument (list 'helm-tree-sitter-core-elem-p elem)))

  (let* ((children-alist (helm-tree-sitter-utils-node-children-to-alist (helm-tree-sitter-core-elem-node elem)))
         (identifier (helm-tree-sitter-utils-get-node-text (alist-get 'identifier children-alist))))

    (concat
     (helm-tree-sitter-utils-prepend-depth-if-needed elem)
     (propertize "Class: "
                 'face 'italic)
     identifier)))

(provide 'helm-tree-sitter-python)

;;; helm-tree-sitter-python.el ends here
