;;; helm-tree-sitter-debug.el --- Helm interface for tree-sitter -*- lexical-binding: t -*-

;; Copyright (C) 2021 Giedrius Jonikas <giedriusj1@gmail.com>

;; Author: Giedrius Jonikas <giedriusj1@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/Giedriusj1/helm-tree-sitter

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
;; Simple helm interface to tree-sitter with all existing nodes show.

;; This works a lot like 'tree-sitter-debug'

;;; Code:
(require 'helm)
(require 'tree-sitter)

(require 'helm-tree-sitter-core)
(require 'helm-tree-sitter-utils)

;;;###autoload
(defun helm-tree-sitter-debug ()
  "Helm interface for tree-sitter with all elements shown."
  (interactive)

  (if (not tree-sitter-tree)
      (error "Tree-sitter minor mode is not loaded"))

  (helm :sources
        (helm-build-sync-source "Tree-sitter debug"
          :candidates (helm-tree-sitter-debug-elements
                       (helm-tree-sitter-core-build-node-list (tsc-root-node tree-sitter-tree) 0))
          :action (lambda (x)
                    (goto-char (helm-tree-sitter-core-elem-start-pos x)))
          :fuzzy-match t)
        :candidate-number-limit 9999
        :buffer "*helm tree-sitter debug*"))


(defun helm-tree-sitter-debug-elements (elements)
  "Helm-tree-sitter internal function.
Argument ELEMENTS is a flat list of `helm-tree-sitter-core-elem's.
All elements are then turned into helm elements for debugging."
  (remq nil
        (mapcar
         (lambda (node)
           ;; Great, we have a handler for the element node type
           (let ((fun-ret (helm-tree-sitter-debug-elem-fn node))) ; Let's get the actual text
             (when fun-ret
               ;; Each candidate will consist of a list containing (text-string . tree)
               (cons
                fun-ret
                ;; Store the tree too, so additional actions can be performed later
                node))))

         elements)))


(defun helm-tree-sitter-debug-elem-fn (elem)
  "Helm-tree-sitter handler for debug mode..
Argument ELEM is `helm-tree-sitter-core-elem' representing the node."

  (unless (helm-tree-sitter-core-elem-p elem)
    (signal 'wrong-type-argument (list 'helm-tree-sitter-core-elem-p elem)))

  (concat
   (make-string (* 2 (helm-tree-sitter-core-elem-depth elem )) ?\ )
   (format "%s" (helm-tree-sitter-core-elem-node-type elem))))


(provide 'helm-tree-sitter-debug)
;;; helm-tree-sitter-debug.el ends here
