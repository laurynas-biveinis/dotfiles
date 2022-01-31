;;; helm-tree-sitter-core.el --- Helm interface for tree-sitter -*- lexical-binding: t -*-

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
;; Some common functionality that is needed by various parts of helm-tree-sitter

;;; Code:

(require 'tsc)

(defvar helm-tree-sitter-producer-mode-maps
      '((python-mode . helm-tree-sitter-python-candidate-producer)
        (c++-mode . helm-tree-sitter-cpp-candidate-producer)
        (c-mode . helm-tree-sitter-c-candidate-producer)
        (rust-mode . helm-tree-sitter-rust-candidate-producer)
        (rustic-mode . helm-tree-sitter-rust-candidate-producer)))

;; tree-sitter element. Holds everything we care about for each of the candidates.
(cl-defstruct helm-tree-sitter-core-elem node node-type node-text start-pos depth)

(defun helm-tree-sitter-core-elements-to-helm-candidates (elements)
  "Helm-tree-sitter internal function.
Argument ELEMENTS is a flat list of `helm-tree-sitter-core-elem's. This
function looks up `helm-tree-sitter-producer-mode-maps' for `major-mode'
appropriate candidate producer map, and then iterates through provided
list applying candidate producer functions"

  (let* ((current-mode-producer (symbol-value (assoc-default major-mode helm-tree-sitter-producer-mode-maps))))
    (if (not current-mode-producer)
      (error "Major mode is not supported by helm-tree-sitter"))

    (remq nil
          (mapcar
           (lambda (node)
             (let* ((my-fn (assoc-default
                            (format "%s" (helm-tree-sitter-core-elem-node-type node))
                            current-mode-producer)))
               (when my-fn
                 ;; Great, we have a handler for the element node type
                 (let ((fun-ret (funcall my-fn node))) ; Let's get the actual text
                   (when fun-ret
                     ;; Each candidate will consist of a list containing (text-string . tree)
                     (cons
                      fun-ret
                      ;; Store the tree too, so additional actions can be performed later
                      node))))))
           elements))))

(defun helm-tree-sitter-core-build-node-list (node depth)
"Helm-tree-sitter internal function.
This is a recursive function, and initially NODE is the tree-sitter root node.
Argument DEPTH is used to track how deep in the tree the element was.
This function flattens the tree and returns a list of
`helm-tree-sitter-core-elem's for further processing."
  (let ((elements '()))
    ;; Add the current node
    (push (make-helm-tree-sitter-core-elem
           :node node
           :node-type (tsc-node-type node)
           :node-text (tsc-node-text node)
           :start-pos (tsc-node-start-position node)
           :depth depth)
          elements)

    ;; And now all the child nodes..
    (dotimes (e (tsc-count-named-children node))
      (setq elements (append  elements (helm-tree-sitter-core-build-node-list (tsc-get-nth-named-child node e) (1+ depth)))))

    elements))


(provide 'helm-tree-sitter-core)
;;; helm-tree-sitter-core.el ends here
