;;; xmtn-ids.el --- Resolver routines for xmtn revision ids

;; Copyright (C) 2008 - 2011 Stephen Leake
;; Copyright (C) 2006, 2007 Christian M. Ohler

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

;; This file is part of xmtn and implements an extension of DVC's
;; REVISION-IDs (see docs/DVC-API) for the monotone backend.
;;
;; We extend DVC's definition of a REVISION-ID for xmtn as follows.
;; This way, previous-revision can contain any nested BACKEND-ID.
;; This simplifies the code and may be useful.
;;
;; REVISION-ID :: (xmtn BACKEND-ID) | "selector"
;;
;; BACKEND-ID :: BACKEND-REVISION
;;             | (revision BACKEND-REVISION)
;;   ;; An already commited revision
;;             | (local-tree PATH)
;;   ;; Uncommited revision in the local tree PATH
;;             | (last-revision PATH NUM)
;;   ;; Last committed revision in tree PATH if NUM = 1
;;   ;; Last but NUM-1 revision in tree PATH if NUM > 1
;;   ;;
;;   ;; Note that dvc-dvc-file-diff abuses this syntax and specifies the
;;   ;; name of a file inside the tree as PATH.
;;   ;;
;;   ;; For xmtn, "last committed revision" here refers to the base
;;   ;; revision of the tree PATH, not the head in the database.
;;   ;; This is because the other backends use `(last-revision ,path
;;   ;; 1) as a default base for diffs, and we copy them, so we have
;;   ;; to define it this way.
;;             | (previous-revision BACKEND-ID NUM)
;;   ;; NUMth ancestor of BACKEND-ID.
;;             | (previous-revision BACKEND-ID)
;;   ;; Parent of BACKEND-ID.  (DVC requires this extension but
;;   ;; doesn't document it.)
;;
;; PATH :: string
;;
;; NUM :: number
;;
;; BACKEND-REVISION :: a 40-char string containing mtn's hash of 40 hex digits
;;
;;
;; Using the routines below, such IDs can be resolved to
;; RESOLVED-BACKEND-IDs.
;;
;; RESOLVED-BACKEND-ID :: (revision BACKEND-REVISION)
;;                      | (local-tree PATH)

;;; Code:

;;; There are some notes on the design of xmtn in
;;; docs/xmtn-readme.txt.

(eval-and-compile
  (require 'cl)
  (require 'xmtn-automate)
  (require 'xmtn-match))

(defun xmtn--revision-hash-id (revision-id)
  "Return the hash-id from a REVISION-ID"
  (car (cdadr revision-id)))

(defun xmtn--resolve-revision-id-1 (root revision-id)
  "Resolve dvc REVISION-ID to a RESOLVED-BACKEND-ID."
  (ecase (car revision-id)
    ('xmtn
     (xmtn--resolve-backend-id root (cadr revision-id)))))

(defun xmtn--resolve-revision-id (root revision-id)
  "Resolve REVISION-ID to a RESOLVED-BACKEND-ID. REVISION-ID may
be a dvc revision (list starting with 'xmtn) or a string
containing a mtn selector."
  (unless root (setq root (dvc-tree-root)))
  (cond
   ((listp revision-id)
    (xmtn--resolve-revision-id-1 root revision-id))
   ((stringp revision-id)
    (xmtn--resolve-revision-id-1
     root
     (list 'xmtn (list 'revision (car (xmtn--expand-selector root revision-id))))))
   (t
    (error "revision-id must be a list or string"))))

(defun xmtn--resolve-backend-id (root backend-id)
  "Resolve BACKEND-ID to a RESOLVED-BACKEND-ID.

See file commentary for details."
  (let ((resolved-backend-id
         (etypecase backend-id
           (xmtn--hash-id
            (list 'revision backend-id))
           (list
            (xmtn-match backend-id
              ((revision $backend-revision)
               backend-id)
              ((local-tree $path)
               backend-id)
              ((last-revision $path $num)
               (xmtn--resolve--last-revision root path num))
              ((previous-revision $base-backend-id . $optional-num)
               (destructuring-bind (&optional num) optional-num
                 (unless num (setq num 1))
                 (xmtn--resolve--previous-revision root
                                                   base-backend-id
                                                   num))))))))
    ;; Small sanity check.  Also implicit documentation.
    (xmtn-match resolved-backend-id
      ((revision $hash-id) (assert (typep hash-id 'xmtn--hash-id)))
      ((local-tree $string) (assert (typep string 'string))))
    resolved-backend-id))

(defun xmtn--resolve--last-revision (root path num)
  (check-type path string)
  (check-type num (integer 1 *))
  (let ((path-root (xmtn-tree-root path t)))
    (unless path-root
      (error "Path is not in a monotone tree: %S" `(last-revision ,path ,num)))
    (let ((base-revision-hash-id (xmtn--get-base-revision-hash-id path-root)))
      (xmtn--resolve-backend-id path-root
                                `(previous-revision
                                  ,base-revision-hash-id
                                  ,(1- num))))))

(defun xmtn--get-parent-revision-hash-id (root hash-id local-branch)
  (check-type hash-id xmtn--hash-id)
  (let ((parents (xmtn-automate-command-output-lines root `("parents"
                                                                   ,hash-id))))
    (case (length parents)
      (0 (error "Revision has no parents: %s" hash-id))
      (1 (let ((parent (first parents)))
           (assert (typep parent 'xmtn--hash-id))
           parent))
      (t
       ;; If this revision is the result of a propagate, there are two parents, one of which is on the local branch
       (let ((first-parent-branch (xmtn--branch-of root (first parents))))
         (if (equal local-branch first-parent-branch)
             (first parents)
           (second parents)))
       ))))

(defun xmtn--resolve--previous-revision (root backend-id num)
  (check-type num (integer 0 *))
  (let ((local-branch (xmtn--tree-default-branch root))
        (resolved-id (xmtn--resolve-backend-id root backend-id)))
    (if (zerop num)
        resolved-id
      (ecase (first resolved-id)
        (local-tree
         (let ((other-root (second resolved-id)))
           (xmtn--resolve-backend-id other-root
                                     `(previous-revision
                                       ,(xmtn--get-base-revision-hash-id
                                         other-root)
                                       ,(1- num)))))
        (revision
         (let ((hash-id (second resolved-id)))
           (check-type hash-id xmtn--hash-id)
           (loop repeat num
                 ;; If two parents of this rev, use parent on same branch as rev.
                 do (setq hash-id (xmtn--get-parent-revision-hash-id root hash-id local-branch)))
           `(revision ,hash-id)))))))

(defun xmtn--error-unless-revision-exists (root hash-id)
  (let ((lines (xmtn--expand-selector root (concat "i:" hash-id))))
    (when (endp lines)
      (error "Revision %s unknown in workspace %s" hash-id root))
    (assert (eql (length lines) 1))
    (let ((db-hash (first lines)))
      (assert (equal db-hash hash-id))))
  nil)

(defun xmtn--expand-selector (root selector)
  (xmtn-automate-command-output-lines root `("select" ,selector)))

(defun xmtn--branch-of (root hash-id)
  (let ((certs (xmtn--list-parsed-certs root hash-id))
        result
        cert)
    (while (not result)
      (setq cert (car certs))
      (if (equal "branch" (nth 2 cert))
          (setq result (nth 3 cert)))
      (setq certs (cdr certs)))
    result))

(defun xmtn--branches-of (hash-id)
  "Return list of branch names for HASH-ID. `default-directory'
must be a workspace."
  (let* (result
         (session (xmtn-automate-cache-session default-directory))
         (handle (xmtn-automate--new-command session `("certs" ,hash-id))))
    (xmtn-automate-command-wait-until-finished handle)
    (with-current-buffer (xmtn-automate-command-buffer handle)
      ;; now in buffer containing basic_io certs; find the branch certs
      (goto-char (point-min))
      (while (not (xmtn-basic-io-eof))
        (xmtn-basic-io-optional-line "name"
          (if (and (eq 'string (caar value))
                   (string= "branch" (cadar value)))
              (xmtn-basic-io-parse-line
                  (if (string= symbol "value")
                      (add-to-list 'result (cadar value)))))
          )))
    (xmtn-automate--cleanup-command handle)
    result))

(defun xmtn--get-base-revision-hash-id-or-null (root)
  (let ((hash-id (xmtn-automate-command-output-line
                  root `("get_base_revision_id"))))
    (when (equal hash-id "") (setq hash-id nil))
    (assert (typep hash-id '(or xmtn--hash-id null)))
    hash-id))

(defun xmtn--get-base-revision-hash-id (root)
  (let ((hash-id-or-null (xmtn--get-base-revision-hash-id-or-null root)))
    (unless hash-id-or-null
      (error "Tree has no base revision: %S" root))
    hash-id-or-null))

(provide 'xmtn-ids)

;;; xmtn-ids.el ends here
