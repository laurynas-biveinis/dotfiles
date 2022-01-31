;;; helm-tree-sitter-c.el --- Helm interface for tree-sitter -*- lexical-binding: t -*-

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
;; Provides function for dealing with C code

;;; Code:

(require 'helm-tree-sitter-utils)

(defvar helm-tree-sitter-c-candidate-producer
  '(("function_definition" . helm-tree-sitter-c-function-definition-fn)
    ("preproc_include" . helm-tree-sitter-c-preproc-include-fn)
    ("struct_specifier" . helm-tree-sitter-c-struct-specifier-fn)
    ("enum_specifier" . helm-tree-sitter-c-enum-specifier-fn)
    ("union_specifier" . helm-tree-sitter-c-union-specifier-fn)

    ;; We get very spammy output if we try to show every declaration,
    ;; so we'll just ignore them for now.
    ;; ("declaration" . helm-tree-sitter-c-declaration-fn)
    ))

(defun helm-tree-sitter-c-preproc-include-fn (elem)
  "Helm-tree-sitter handler for proproc_include nodes in C mode.
Argument ELEM is `helm-tree-sitter-core-elem' representing the node."

  (unless (helm-tree-sitter-core-elem-p elem)
    (signal 'wrong-type-argument (list 'helm-tree-sitter-core-elem-p elem)))

  (let* ((children-alist (helm-tree-sitter-utils-node-children-to-alist (helm-tree-sitter-core-elem-node elem)))
         (system-lib (helm-tree-sitter-utils-get-node-text (alist-get 'system_lib_string children-alist)))
         (string-literal (helm-tree-sitter-utils-get-node-text (alist-get 'string_literal children-alist)))

         ;; In case of preprocessor include
         (identifier (helm-tree-sitter-utils-get-node-text (alist-get 'identifier children-alist))))

    (concat
     (propertize "Include: "
                 'face 'italic)
     (concat identifier
             system-lib
             (replace-regexp-in-string "\"" "" string-literal)))))


(defun helm-tree-sitter-c-function-definition-fn (elem)
  "Helm-tree-sitter handler for function_definition nodes in C mode.
Argument ELEM is `helm-tree-sitter-core-elem' representing the node."

  (unless (helm-tree-sitter-core-elem-p elem)
    (signal 'wrong-type-argument (list 'helm-tree-sitter-core-elem-p elem)))

  (let* ((children-alist (helm-tree-sitter-utils-node-children-to-alist (helm-tree-sitter-core-elem-node elem)))
         (storage-class-specifier (helm-tree-sitter-utils-get-node-text (alist-get 'storage_class_specifier children-alist)))
         (primitive-type (helm-tree-sitter-utils-get-node-text (alist-get 'primitive_type children-alist)))
         (type-identifier (helm-tree-sitter-utils-get-node-text (alist-get 'type_identifier children-alist)))
         (sized-type-specifier (helm-tree-sitter-utils-get-node-text (alist-get 'sized_type_specifier children-alist)))
         (struct-specifier (helm-tree-sitter-utils-get-node-text (alist-get 'struct_specifier children-alist)))
         (function-declarator (helm-tree-sitter-utils-get-node-text (alist-get 'function_declarator children-alist)))
         (function-pointer-declarator (helm-tree-sitter-utils-get-node-text (alist-get 'pointer_declarator children-alist))))

    (concat
     (propertize "Function: "
                 'face 'italic)

     (helm-tree-sitter-utils-strip-newlines-and-whitespaces
      (concat
       (helm-tree-sitter-utils-append-space-if-not-empty storage-class-specifier)

       struct-specifier
       sized-type-specifier
       primitive-type
       type-identifier
       
       " "
       (helm-tree-sitter-utils-strip-newlines function-declarator)
       (helm-tree-sitter-utils-strip-newlines function-pointer-declarator))))))

(defun helm-tree-sitter-c-struct-specifier-fn (elem)
  "Helm-tree-sitter handler for struct_specifier nodes in C mode.
Argument ELEM is `helm-tree-sitter-core-elem' representing the node."
  (unless (helm-tree-sitter-core-elem-p elem)
    (signal 'wrong-type-argument (list 'helm-tree-sitter-core-elem-p elem)))

  (let* ((children-alist (helm-tree-sitter-utils-node-children-to-alist (helm-tree-sitter-core-elem-node elem)))
         (type-identifier (helm-tree-sitter-utils-get-node-text (alist-get 'type_identifier children-alist)))
         (field-declaration-list-node (alist-get 'field_declaration_list children-alist)))

    ;; To prevent output from being too verbose, we'll only show structs that have
    ;; field declarations too.
    (when (tsc-node-p field-declaration-list-node)

      (if (not (string= "" type-identifier))
          (concat
           (propertize "Struct: "
                       'face 'italic)

           type-identifier)

        ;; We are dealing with struct that has a field declaration list, but no type-identifier...
        ;; This must be a typedef case.
        ;; Let's check if our parent has a type_identifier:
        (let* ((parent-node (tsc-get-parent (helm-tree-sitter-core-elem-node elem))))
          (when parent-node
            (let* ((parent-children-alist (helm-tree-sitter-utils-node-children-to-alist parent-node))
                   (parent-type-identifier (helm-tree-sitter-utils-get-node-text (alist-get 'type_identifier parent-children-alist))))
              (concat
               (propertize "typedef Struct: "
                           'face 'italic)

               parent-type-identifier))))))))


(defun helm-tree-sitter-c-enum-specifier-fn (elem)
  "Helm-tree-sitter handler for enum_specifier nodes in C mode.
Argument ELEM is `helm-tree-sitter-core-elem' representing the node."

  (unless (helm-tree-sitter-core-elem-p elem)
    (signal 'wrong-type-argument (list 'helm-tree-sitter-core-elem-p elem)))

  (let* ((children-alist (helm-tree-sitter-utils-node-children-to-alist (helm-tree-sitter-core-elem-node elem)))
         (type-identifier (helm-tree-sitter-utils-get-node-text (alist-get 'type_identifier children-alist)))
         (enumerator-list-list-node (alist-get 'enumerator_list children-alist)))

    ;; To prevent output from being too verbose, we'll only show enums that have
    ;; field declarations too.
    (when (tsc-node-p enumerator-list-list-node)
      (if (not (string= "" type-identifier))
          (concat
           (helm-tree-sitter-utils-prepend-depth-if-needed elem)
           (propertize "Enum: "
                       'face 'italic)

           type-identifier)

        ;; We are dealing with enum that has a field declaration list, but no type-identifier...
        ;; This must be a typedef case.
        ;; Let's check if our parent has a type_identifier:
        (let* ((parent-node (tsc-get-parent (helm-tree-sitter-core-elem-node elem))))
          (when parent-node
            (let* ((parent-children-alist (helm-tree-sitter-utils-node-children-to-alist parent-node))
                   (parent-type-identifier (helm-tree-sitter-utils-get-node-text (alist-get 'type_identifier parent-children-alist))))
              (concat
               (helm-tree-sitter-utils-prepend-depth-if-needed elem)
               (propertize "typedef Enum: "
                           'face 'italic)

               parent-type-identifier))))))))

(defun helm-tree-sitter-c-union-specifier-fn (elem)
  "Helm-tree-sitter handler for union_specifier nodes in C mode.
Argument ELEM is `helm-tree-sitter-core-elem' representing the node."

  (unless (helm-tree-sitter-core-elem-p elem)
    (signal 'wrong-type-argument (list 'helm-tree-sitter-core-elem-p elem)))

  (let* ((children-alist (helm-tree-sitter-utils-node-children-to-alist (helm-tree-sitter-core-elem-node elem)))
         (type-identifier (helm-tree-sitter-utils-get-node-text (alist-get 'type_identifier children-alist)))
         (field-declaration-list (alist-get 'field_declaration_list children-alist)))

    ;; To prevent output from being too verbose, we'll only show enums that have
    ;; field declarations too.
    (when (tsc-node-p field-declaration-list)
      (if (not (string= "" type-identifier))
          (concat
           (helm-tree-sitter-utils-prepend-depth-if-needed elem)
           (propertize "Union: "
                       'face 'italic)

           type-identifier)

        ;; We are dealing with union that has a field declaration list, but no type-identifier...
        ;; This must be a typedef case.
        ;; Let's check if our parent has a type_identifier:
        (let* ((parent-node (tsc-get-parent (helm-tree-sitter-core-elem-node elem))))
          (when parent-node
            (let* ((parent-children-alist (helm-tree-sitter-utils-node-children-to-alist parent-node))
                   (parent-type-identifier (helm-tree-sitter-utils-get-node-text (alist-get 'type_identifier parent-children-alist))))
              (concat
               (helm-tree-sitter-utils-prepend-depth-if-needed elem)
               (propertize "typedef Union: "
                           'face 'italic)

               parent-type-identifier))))))))

(provide 'helm-tree-sitter-c)
;;; helm-tree-sitter-c.el ends here
