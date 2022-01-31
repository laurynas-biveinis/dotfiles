;;; helm-tree-sitter.el --- Helm interface for tree-sitter -*- lexical-binding: t -*-

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
;; Simple helm interface to tree-sitter.

;; Currently only C, C++, Python and Rust are supported, but adding more
;; languages in the future should be trivial.


;; Usage:
;; (require 'helm-tree-sitter)
;; and then (helm-tree-sitter) or (helm-tree-sitter-or-imenu)

;; A debug utility is also provided:
;; (require 'helm-tree-sitter-debug)
;; (helm-tree-sitter-debug)
;; This works with all modes and simply lists all available nodes
;; in the existing tree-sitter-tree.


;;; Code:
(require 'helm)
(require 'helm-imenu)
(require 'tree-sitter)

(require 'helm-tree-sitter-core)
(require 'helm-tree-sitter-utils)

(require 'helm-tree-sitter-c)
(require 'helm-tree-sitter-cpp)
(require 'helm-tree-sitter-python)
(require 'helm-tree-sitter-rust)

;;;###autoload
(defun helm-tree-sitter-or-imenu ()
  "Helm interface for tree-sitter.
If tree-sitter is enabled and we
know how to deal with major mode, we'll use `helm-tree-sitter'.
Otherwise we'll default to `helm-imenu'."
  (interactive)

  (if (and tree-sitter-tree
           (symbol-value (assoc-default major-mode helm-tree-sitter-producer-mode-maps)))
      (helm-tree-sitter)
    (helm-imenu)))

;;;###autoload
(defun helm-tree-sitter ()
  "Helm interface for tree-sitter."
  (interactive)

  (if (not tree-sitter-tree)
      (error "Tree-sitter minor mode is not loaded"))

  ;; We'll be copying fontified text from the buffer, so we want to
  ;; make sure that it's been properly fontifier before we do anything.
  (if (fboundp 'font-lock-ensure)
      (font-lock-ensure)
    (with-no-warnings
      (font-lock-fontify-buffer)))

  (helm :sources
        (helm-build-sync-source "Tree-sitter"
          :candidates (helm-tree-sitter-core-elements-to-helm-candidates
                       (helm-tree-sitter-core-build-node-list (tsc-root-node tree-sitter-tree) 0))
          :action (lambda (x)
                    (goto-char (helm-tree-sitter-core-elem-start-pos x)))
          :fuzzy-match t)
        :candidate-number-limit 9999
        :buffer "*helm tree-sitter*"))


(provide 'helm-tree-sitter)

;;; helm-tree-sitter.el ends here
