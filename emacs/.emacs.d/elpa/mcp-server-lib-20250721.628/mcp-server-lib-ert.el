;;; mcp-server-lib-ert.el --- ERT test utilities for MCP server -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Laurynas Biveinis

;; Author: Laurynas Biveinis <laurynas.biveinis@gmail.com>
;; Keywords: tools, testing
;; URL: https://github.com/laurynas-biveinis/mcp-server-lib.el

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
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

;; Test utilities for writing ERT tests for MCP servers.

;;; Code:

(require 'ert)

(defun mcp-server-lib-ert-check-text-response
    (response &optional expected-error)
  "Validate RESPONSE structure and extract text content.
If EXPECTED-ERROR is non-nil, expects isError to be true.
Returns the text content string on success.
Signals test failure if response structure is invalid."
  (let ((result (alist-get 'result response)))
    ;; Response must have a result field
    (should result)
    ;; Check result has exactly the expected fields
    (let ((result-keys (mapcar #'car result)))
      (should (= 2 (length result-keys)))
      (should (member 'content result-keys))
      (should (member 'isError result-keys)))
    ;; Check content field structure
    (let ((content (alist-get 'content result)))
      (should (arrayp content))
      (should (= 1 (length content)))
      ;; Check content item structure
      (let* ((text-item (aref content 0))
             (item-keys (mapcar #'car text-item)))
        (should (= 2 (length item-keys)))
        (should (member 'type item-keys))
        (should (member 'text item-keys))
        ;; Check content item values
        (should (string= "text" (alist-get 'type text-item)))
        (let ((text (alist-get 'text text-item)))
          (should (stringp text))
          ;; Check isError field
          (should
           (eq
            (alist-get 'isError result)
            (if expected-error
                t
              :json-false)))
          ;; Return the text content
          text)))))

(provide 'mcp-server-lib-ert)

;; Local Variables:
;; package-lint-main-file: "mcp-server-lib.el"
;; End:

;;; mcp-server-lib-ert.el ends here
