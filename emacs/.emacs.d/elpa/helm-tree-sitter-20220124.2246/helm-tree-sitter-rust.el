;;; helm-tree-sitter-rust.el --- Helm interface for tree-sitter -*- lexical-binding: t -*-

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
;; Provides function for dealing with Rust code

;;; Code:

(require 'helm-tree-sitter-utils)

(defvar helm-tree-sitter-rust-candidate-producer
  '(("use_declaration" . helm-tree-sitter-rust-use-declaration-fn)
    ("struct_item" . helm-tree-sitter-rust-struct-item-fn)
    ("function_item" . helm-tree-sitter-rust-function-definition-fn)
    ("impl_item" . helm-tree-sitter-rust-impl-item-fn)
    ("macro_definition" . helm-tree-sitter-rust-macro-definition-fn)))

(defun helm-tree-sitter-rust-use-declaration-fn (elem)
  "Helm-tree-sitter handler for use_declaration nodes in Rust mode.
Argument ELEM is `helm-tree-sitter-core-elem' representing the node."

  (unless (helm-tree-sitter-core-elem-p elem)
    (signal 'wrong-type-argument (list 'helm-tree-sitter-core-elem-p elem)))

  (concat
   (helm-tree-sitter-utils-prepend-depth-if-needed elem)
   (propertize "Use: "
               'face 'italic)

   (helm-tree-sitter-utils-get-node-text (helm-tree-sitter-core-elem-node elem))))

(defun helm-tree-sitter-rust-function-definition-fn (elem)
  "Helm-tree-sitter handler for function_definition nodes in Rust mode.
Argument ELEM is `helm-tree-sitter-core-elem' representing the node."
  (unless (helm-tree-sitter-core-elem-p elem)
    (signal 'wrong-type-argument (list 'helm-tree-sitter-core-elem-p elem)))

  (let* ((children-alist (helm-tree-sitter-utils-node-children-to-alist (helm-tree-sitter-core-elem-node elem)))
         (visibility-modifier (helm-tree-sitter-utils-get-node-text (alist-get 'visibility_modifier children-alist)))
         (identifier (helm-tree-sitter-utils-get-node-text (alist-get 'identifier children-alist)))
         (type-identifier (helm-tree-sitter-utils-get-node-text (alist-get 'type_identifier children-alist)))
         (parameters (helm-tree-sitter-utils-get-node-text (alist-get 'parameters children-alist))))

    (concat
     (helm-tree-sitter-utils-prepend-depth-if-needed elem)
     (propertize "Fn: "
                 'face 'italic)

     (helm-tree-sitter-utils-strip-newlines-and-whitespaces
      (concat
       (helm-tree-sitter-utils-append-space-if-not-empty visibility-modifier)
       (helm-tree-sitter-utils-append-space-if-not-empty type-identifier)
       identifier
       parameters

       (helm-tree-sitter-utils-prepend-if-not-empty type-identifier " -> "))))))

(defun helm-tree-sitter-rust-struct-item-fn (elem)
  "Helm-tree-sitter handler for struct_item nodes in Rust mode.
Argument ELEM is `helm-tree-sitter-core-elem' representing the node."

  (unless (helm-tree-sitter-core-elem-p elem)
    (signal 'wrong-type-argument (list 'helm-tree-sitter-core-elem-p elem)))

  (let* ((children-alist (helm-tree-sitter-utils-node-children-to-alist (helm-tree-sitter-core-elem-node elem)))
         (identifier (helm-tree-sitter-utils-get-node-text (alist-get 'type_identifier children-alist))))

    (concat
     (helm-tree-sitter-utils-prepend-depth-if-needed elem)
     (propertize "Struct: "
                 'face 'italic)

     identifier)))

(defun helm-tree-sitter-rust-impl-item-fn (elem)
  "Helm-tree-sitter handler for impl_item nodes in Rust mode.
Argument ELEM is `helm-tree-sitter-core-elem' representing the node."

  (unless (helm-tree-sitter-core-elem-p elem)
    (signal 'wrong-type-argument (list 'helm-tree-sitter-core-elem-p elem)))

  (let* ((children-alist (helm-tree-sitter-utils-node-children-to-alist (helm-tree-sitter-core-elem-node elem)))
         (identifier (helm-tree-sitter-utils-get-node-text (alist-get 'type_identifier children-alist))))

    (concat
     (helm-tree-sitter-utils-prepend-depth-if-needed elem)
     (propertize "Impl: "
                 'face 'italic)
     identifier)))

(defun helm-tree-sitter-rust-macro-definition-fn (elem)
  "Helm-tree-sitter handler for macro_definition nodes in Rust mode.
Argument ELEM is `helm-tree-sitter-core-elem' representing the node."

  (unless (helm-tree-sitter-core-elem-p elem)
    (signal 'wrong-type-argument (list 'helm-tree-sitter-core-elem-p elem)))

  (let* ((children-alist (helm-tree-sitter-utils-node-children-to-alist (helm-tree-sitter-core-elem-node elem)))
         (identifier (helm-tree-sitter-utils-get-node-text (alist-get 'identifier children-alist))))

    (concat
     (helm-tree-sitter-utils-prepend-depth-if-needed elem)
     (propertize "Macro: "
                 'face 'italic)
     identifier)))

(provide 'helm-tree-sitter-rust)

;;; helm-tree-sitter-rust.el ends here
