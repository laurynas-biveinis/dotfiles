;;; org-footnote.el --- Footnote support in Org and elsewhere
;;
;; Copyright (C) 2009, 2010 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 7.7
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the code dealing with footnotes in Org-mode.
;; The code can also be used in arbitrary text modes to provide
;; footnotes.  Compared to Steven L Baur's footnote.el it provides
;; better support for resuming editing.  It is less configurable than
;; Steve's code, though.

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'org-macs)
(require 'org-compat)

(declare-function org-combine-plists "org" (&rest plists))
(declare-function org-in-commented-line "org" ())
(declare-function org-in-indented-comment-line "org" ())
(declare-function org-in-regexp "org" (re &optional nlines visually))
(declare-function org-in-block-p "org" (names))
(declare-function org-mark-ring-push "org" (&optional pos buffer))
(declare-function outline-next-heading "outline")
(declare-function org-trim "org" (s))
(declare-function org-show-context "org" (&optional key))
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-end-of-subtree "org"  (&optional invisible-ok to-heading))
(declare-function org-in-verbatim-emphasis "org" ())
(declare-function org-inside-latex-macro-p "org" ())
(declare-function org-id-uuid "org" ())
(declare-function org-fill-paragraph "org" (&optional justify))
(declare-function org-export-preprocess-string "org-exp"
		  (string &rest parameters))

(defvar org-outline-regexp-bol) ; defined in org.el
(defvar org-odd-levels-only) ;; defined in org.el
(defvar org-bracket-link-regexp) ; defined in org.el
(defvar message-signature-separator) ;; defined in message.el

(defconst org-footnote-re
  ;; Only [1]-like footnotes are closed in this regexp, as footnotes
  ;; from other types might contain square brackets (i.e. links) in
  ;; their definition.
  ;;
  ;; `org-re' is used for regexp compatibility with XEmacs.
  (org-re (concat "\\[\\(?:"
		  ;; Match inline footnotes.
		  "fn:\\([-_[:word:]]+\\)?:\\|"
		  ;; Match other footnotes.
		  "\\(?:\\([0-9]+\\)\\]\\)\\|"
		  "\\(fn:[-_[:word:]]+\\)"
		  "\\)"))
  "Regular expression for matching footnotes.")

(defconst org-footnote-definition-re
  (org-re "^\\(\\[\\([0-9]+\\|fn:[-_[:word:]]+\\)\\]\\)")
  "Regular expression matching the definition of a footnote.")

(defvar org-footnote-forbidden-blocks '("example" "verse" "src" "ascii" "beamer"
					"docbook" "html" "latex" "odt")
  "Names of blocks where footnotes are not allowed.")

(defgroup org-footnote nil
  "Footnotes in Org-mode."
  :tag "Org Footnote"
  :group 'org)

(defcustom org-footnote-section "Footnotes"
  "Outline heading containing footnote definitions before export.
This can be nil, to place footnotes locally at the end of the current
outline node.  If can also be the name of a special outline heading
under which footnotes should be put.
This variable defines the place where Org puts the definition
automatically, i.e. when creating the footnote, and when sorting the notes.
However, by hand you may place definitions *anywhere*.
If this is a string, during export, all subtrees starting with this
heading will be removed after extracting footnote definitions."
  :group 'org-footnote
  :type '(choice
	  (string :tag "Collect footnotes under heading")
	  (const :tag "Define footnotes locally" nil)))

(defcustom org-footnote-tag-for-non-org-mode-files "Footnotes:"
  "Tag marking the beginning of footnote section.
The Org-mode footnote engine can be used in arbitrary text files as well
as in Org-mode.  Outside Org-mode, new footnotes are always placed at
the end of the file.  When you normalize the notes, any line containing
only this tag will be removed, a new one will be inserted at the end
of the file, followed by the collected and normalized footnotes."
  :group 'org-footnote
  :type 'string)

(defcustom org-footnote-define-inline nil
  "Non-nil means define footnotes inline, at reference location.
When nil, footnotes will be defined in a special section near
the end of the document.  When t, the [fn:label:definition] notation
will be used to define the footnote at the reference position."
  :group 'org-footnote
  :type 'boolean)

(defcustom org-footnote-auto-label t
  "Non-nil means define automatically new labels for footnotes.
Possible values are:

nil        prompt the user for each label
t          create unique labels of the form [fn:1], [fn:2], ...
confirm    like t, but let the user edit the created value.  In particular,
           the label can be removed from the minibuffer, to create
           an anonymous footnote.
random	   Automatically generate a unique, random label.
plain      Automatically create plain number labels like [1]"
  :group 'org-footnote
  :type '(choice
	  (const :tag "Prompt for label" nil)
	  (const :tag "Create automatic [fn:N]" t)
	  (const :tag "Offer automatic [fn:N] for editing" confirm)
	  (const :tag "Create a random label" random)
	  (const :tag "Create automatic [N]" plain)))

(defcustom org-footnote-auto-adjust nil
  "Non-nil means automatically adjust footnotes after insert/delete.
When this is t, after each insertion or deletion of a footnote,
simple fn:N footnotes will be renumbered, and all footnotes will be sorted.
If you want to have just sorting or just renumbering, set this variable
to `sort' or `renumber'.

The main values of this variable can be set with in-buffer options:

#+STARTUP: fnadjust
#+STARTUP: nofnadjust"
  :group 'org-footnote
  :type '(choice
	  (const :tag "Renumber" renumber)
	  (const :tag "Sort" sort)
	  (const :tag "Renumber and Sort" t)))

(defcustom org-footnote-fill-after-inline-note-extraction nil
  "Non-nil means fill paragraphs after extracting footnotes.
When extracting inline footnotes, the lengths of lines can change a lot.
When this option is set, paragraphs from which an inline footnote has been
extracted will be filled again."
  :group 'org-footnote
  :type 'boolean)

(defun org-footnote-in-valid-context-p ()
  "Is point in a context where footnotes are allowed?"
  (save-match-data
    (not (or (org-in-commented-line)
	     (org-in-indented-comment-line)
	     (org-in-verbatim-emphasis)
	     ;; Avoid literal example.
	     (save-excursion
	       (beginning-of-line)
	       (looking-at "[ \t]*:[ \t]+"))
	     ;; Avoid cited text and headers in message-mode.
	     (and (derived-mode-p 'message-mode)
		  (or (save-excursion
			(beginning-of-line)
			(looking-at message-cite-prefix-regexp))
		      (message-point-in-header-p)))
	     ;; Avoid forbidden blocks.
	     (org-in-block-p org-footnote-forbidden-blocks)))))

(defun org-footnote-at-reference-p ()
  "Is the cursor at a footnote reference?

If so, return a list containing its label, beginning and ending
positions, and the definition, when inlined."
  (when (and (org-footnote-in-valid-context-p)
	     (or (looking-at org-footnote-re)
		 (org-in-regexp org-footnote-re)
		 (save-excursion (re-search-backward org-footnote-re nil t)))
	     ;; Only inline footnotes can start at bol.
	     (or (eq (char-before (match-end 0)) 58)
		 (/= (match-beginning 0) (point-at-bol))))
    (let* ((beg (match-beginning 0))
	   (label (or (match-string 2) (match-string 3)
		      ;; Anonymous footnotes don't have labels
		      (and (match-string 1) (concat "fn:" (match-string 1)))))
	   ;; Inline footnotes don't end at (match-end 0) as
	   ;; `org-footnote-re' stops just after the second colon.
	   ;; Find the real ending with `scan-sexps', so Org doesn't
	   ;; get fooled by unrelated closing square brackets.
	   (end (ignore-errors (scan-sexps beg 1))))
      ;; Point is really at a reference if it's located before true
      ;; ending of the footnote.
      (when (and end (< (point) end)
		 ;; Verify match isn't a part of a link.
		 (not (save-excursion
			(goto-char beg)
			(let ((linkp
			       (save-match-data
				 (org-in-regexp org-bracket-link-regexp))))
			  (and linkp (< (point) (cdr linkp))))))
		 ;; Verify point doesn't belong to a LaTeX macro.
		 ;; Beware though, when two footnotes are side by
		 ;; side, once the first one is changed into LaTeX,
		 ;; the second one might then be considered as an
		 ;; optional argument of the command.  Thus, check
		 ;; the `org-protected' property of that command.
		 (or (not (org-inside-latex-macro-p))
		     (and (get-text-property (1- beg) 'org-protected)
			  (not (get-text-property beg 'org-protected)))))
	(list label beg end
	      ;; Definition: ensure this is an inline footnote first.
	      (and (or (not label) (match-string 1))
		   (org-trim (buffer-substring (match-end 0) (1- end)))))))))

(defun org-footnote-at-definition-p ()
  "Is the cursor at a footnote definition?

This matches only pure definitions like [1] or [fn:name] at the beginning
of a line.  It does not match references like [fn:name:definition], where the
footnote text is included and defined locally.

The return value will be nil if not at a footnote definition, and a list with
label, start, end and definition of the footnote otherwise."
  (when (org-footnote-in-valid-context-p)
    (save-excursion
      (end-of-line)
      (let ((lim (save-excursion (re-search-backward
				  (concat org-outline-regexp-bol
					  "\\|^[ \t]*$") nil t))))
	(when (re-search-backward org-footnote-definition-re lim t)
	  (end-of-line)
	  (list (match-string 2)
		(match-beginning 0)
		(save-match-data
		  ;; In a message, limit search to signature.
		  (let ((bound (and (derived-mode-p 'message-mode)
				    (save-excursion
				      (goto-char (point-max))
				      (re-search-backward
				       message-signature-separator nil t)))))
		    (or (and (re-search-forward
			      (org-re
			       (concat "^[ \t]*$" "\\|"
				       org-outline-regexp-bol
				       "\\|"
				       "^\\[\\([0-9]+\\|fn:[-_[:word:]]+\\)\\]"))
			      bound 'move)
			     (progn (skip-chars-forward " \t\n") (point-at-bol)))
			(point))))
		(org-trim (buffer-substring (match-end 0) (point)))))))))

(defun org-footnote-get-next-reference (&optional label backward limit)
  "Return complete reference of the next footnote.

If LABEL is provided, get the next reference of that footnote.  If
BACKWARD is non-nil, find previous reference instead.  LIMIT is
the buffer position bounding the search.

Return value is a list like those provided by `org-footnote-at-reference-p'.
If no footnote is found, return nil."
  (save-excursion
    (let* ((label-fmt (if label (format "\\[%s[]:]" label) org-footnote-re)))
      (catch 'exit
	(while t
	  (unless (funcall (if backward #'re-search-backward #'re-search-forward)
			   label-fmt limit t)
	    (throw 'exit nil))
	  (unless backward (backward-char))
	  (let ((ref (org-footnote-at-reference-p)))
	    (when ref (throw 'exit ref))))))))

(defun org-footnote-next-reference-or-definition (limit)
  "Move point to next footnote reference or definition.

LIMIT is the buffer position bounding the search.

Return value is a list like those provided by
`org-footnote-at-reference-p' or `org-footnote-at-definition-p'.
If no footnote is found, return nil."
  (let* (ref)
    (catch 'exit
      (while t
	(unless (re-search-forward org-footnote-re limit t)
	  (throw 'exit nil))
	;; Beware: with [1]-like footnotes point will be just after
	;; the closing square bracket.
	(backward-char)
	(cond
	 ((setq ref (org-footnote-at-reference-p))
	  (throw 'exit ref))
	 ;; Definition: also grab the last square bracket, only
	 ;; matched in `org-footnote-re' for [1]-like footnotes.
	 ((save-match-data (org-footnote-at-definition-p))
	  (let ((end (match-end 0)))
	    (throw 'exit
		   (list nil (match-beginning 0)
			 (if (eq (char-before end) 93) end (1+ end)))))))))))

(defun org-footnote-get-definition (label)
  "Return label, boundaries and definition of the footnote LABEL."
  (let* ((label (regexp-quote (org-footnote-normalize-label label)))
	 (re (format "^\\[%s\\]\\|.\\[%s:" label label))
	 pos)
    (save-excursion
      (when (or (re-search-forward re nil t)
		(and (goto-char (point-min))
		     (re-search-forward re nil t))
		(and (progn (widen) t)
		     (goto-char (point-min))
		     (re-search-forward re nil t)))
	(let ((refp (org-footnote-at-reference-p)))
	  (cond
	   ((and (nth 3 refp) refp))
	   ((org-footnote-at-definition-p))))))))

(defun org-footnote-goto-definition (label)
  "Move point to the definition of the footnote LABEL."
  (interactive "sLabel: ")
  (org-mark-ring-push)
  (let ((def (org-footnote-get-definition label)))
    (if (not def)
	(error "Cannot find definition of footnote %s" label)
      (goto-char (nth 1 def))
      (looking-at (format "\\[%s\\]\\|\\[%s:" label label))
      (goto-char (match-end 0))
      (org-show-context 'link-search)
      (message "Edit definition and go back with `C-c &' or, if unique, with `C-c C-c'."))))

(defun org-footnote-goto-previous-reference (label)
  "Find the first closest (to point) reference of footnote with label LABEL."
  (interactive "sLabel: ")
  (org-mark-ring-push)
  (let* ((label (org-footnote-normalize-label label)) ref)
    (save-excursion
      (setq ref (or (org-footnote-get-next-reference label t)
		    (org-footnote-get-next-reference label)
		    (save-restriction
		      (widen)
		      (or
		       (org-footnote-get-next-reference label t)
		       (org-footnote-get-next-reference label))))))
    (if (not ref)
	(error "Cannot find reference of footnote %s" label)
      (goto-char (nth 1 ref))
      (org-show-context 'link-search))))

(defun org-footnote-normalize-label (label)
  "Return LABEL as an appropriate string."
  (cond
   ((numberp label) (number-to-string label))
   ((equal "" label) nil)
   ((not (string-match "^[0-9]+$\\|^fn:" label))
    (concat "fn:" label))
   (t label)))

(defun org-footnote-all-labels (&optional with-defs)
  "Return list with all defined foot labels used in the buffer.

If WITH-DEFS is non-nil, also associate the definition to each
label.  The function will then return an alist whose key is label
and value definition."
  (let* (rtn
	 (push-to-rtn
	  (function
	   ;; Depending on WITH-DEFS, store label or (label . def) of
	   ;; footnote reference/definition given as argument in RTN.
	   (lambda (el)
	     (let ((lbl (car el)))
	       (push (if with-defs (cons lbl (nth 3 el)) lbl) rtn))))))
    (save-excursion
      (save-restriction
	(widen)
	;; Find all labels found in definitions.
	(goto-char (point-min))
	(let (def)
	  (while (re-search-forward org-footnote-definition-re nil t)
	    (when (setq def (org-footnote-at-definition-p))
	      (funcall push-to-rtn def))))
	;; Find all labels found in references.
	(goto-char (point-min))
	(let (ref)
	  (while (setq ref (org-footnote-get-next-reference))
	    (goto-char (nth 2 ref))
	    (and (car ref)		; ignore anonymous footnotes
		 (not (funcall (if with-defs #'assoc #'member) (car ref) rtn))
		 (funcall push-to-rtn ref))))))
    rtn))

(defun org-footnote-unique-label (&optional current)
  "Return a new unique footnote label.
The returns the firsts fn:N labels that is currently not used."
  (unless current (setq current (org-footnote-all-labels)))
  (let ((fmt (if (eq org-footnote-auto-label 'plain) "%d" "fn:%d"))
	(cnt 1))
    (while (member (format fmt cnt) current)
      (incf cnt))
    (format fmt cnt)))

(defvar org-footnote-label-history nil
  "History of footnote labels entered in current buffer.")
(make-variable-buffer-local 'org-footnote-label-history)

(defun org-footnote-new ()
  "Insert a new footnote.
This command prompts for a label.  If this is a label referencing an
existing label, only insert the label.  If the footnote label is empty
or new, let the user edit the definition of the footnote."
  (interactive)
  (unless (and (not (bolp)) (org-footnote-in-valid-context-p))
    (error "Cannot insert a footnote here"))
  (let* ((labels (and (not (equal org-footnote-auto-label 'random))
		      (org-footnote-all-labels)))
	 (propose (org-footnote-unique-label labels))
	 (label
	  (org-footnote-normalize-label
	   (cond
	    ((member org-footnote-auto-label '(t plain))
	     propose)
	    ((equal org-footnote-auto-label 'random)
	     (require 'org-id)
	     (substring (org-id-uuid) 0 8))
	    (t
	     (completing-read
	      "Label (leave empty for anonymous): "
	      (mapcar 'list labels) nil nil
	      (if (eq org-footnote-auto-label 'confirm) propose nil)
	      'org-footnote-label-history))))))
    (cond
     ((not label)
      (insert "[fn:: ]")
      (backward-char 1))
     ((member label labels)
      (insert "[" label "]")
      (message "New reference to existing note"))
     (org-footnote-define-inline
      (insert "[" label ": ]")
      (backward-char 1)
      (org-footnote-auto-adjust-maybe))
     (t
      (insert "[" label "]")
      (org-footnote-create-definition label)
      (org-footnote-auto-adjust-maybe)))))

(defun org-footnote-create-definition (label)
  "Start the definition of a footnote with label LABEL."
  (interactive "sLabel: ")
  (let ((label (org-footnote-normalize-label label)))
    (cond
     ((org-mode-p)
      ;; No section, put footnote into the current outline node Try to
      ;; find or make the special node
      (when org-footnote-section
	(goto-char (point-min))
	(let ((re (concat "^\\*+[ \t]+" org-footnote-section "[ \t]*$")))
	  (unless (or (re-search-forward re nil t)
		      (and (progn (widen) t)
			   (re-search-forward re nil t)))
	  (goto-char (point-max))
	  (insert "\n\n* " org-footnote-section "\n"))))
      ;; Now go to the end of this entry and insert there.
      (org-footnote-goto-local-insertion-point)
      (org-show-context 'link-search))
     (t
      ;; In a non-Org file.  Search for footnote tag, or create it if
      ;; necessary (at the end of buffer, or before a signature if in
      ;; Message mode).  Set point after any definition already there.
      (let ((tag (concat "^" org-footnote-tag-for-non-org-mode-files "[ \t]*$"))
	    (max (save-excursion
		   (if (and (derived-mode-p 'message-mode)
			    (re-search-forward
			     message-signature-separator nil t))
		       (copy-marker (point-at-bol) t)
		     (copy-marker (point-max) t)))))
	(goto-char max)
	(unless (re-search-backward tag nil t)
	  (skip-chars-backward " \t\r\n")
	  (delete-region (point) max)
	  (insert "\n\n" org-footnote-tag-for-non-org-mode-files "\n"))
	;; Skip existing footnotes.
	(while (re-search-forward org-footnote-definition-re max t))
	(let ((def (org-footnote-at-definition-p)))
	  (when def (goto-char (nth 2 def))))
	(set-marker max nil))))
    ;; Insert footnote label, position point and notify user.
    (unless (bolp) (insert "\n"))
    (insert "\n[" label "] \n")
    (backward-char)
    (message "Edit definition and go back with `C-c &' or, if unique, with `C-c C-c'.")))

;;;###autoload
(defun org-footnote-action (&optional special)
  "Do the right thing for footnotes.

When at a footnote reference, jump to the definition.

When at a definition, jump to the references if they exist, offer
to create them otherwise.

When neither at definition or reference, create a new footnote,
interactively.

With prefix arg SPECIAL, offer additional commands in a menu."
  (interactive "P")
  (let (tmp c)
    (cond
     (special
      (message "Footnotes: [s]ort  |  [r]enumber fn:N  |  [S]=r+s |->[n]umeric  |  [d]elete")
      (setq c (read-char-exclusive))
      (cond
       ((eq c ?s) (org-footnote-normalize 'sort))
       ((eq c ?r) (org-footnote-renumber-fn:N))
       ((eq c ?S)
	(org-footnote-renumber-fn:N)
	(org-footnote-normalize 'sort))
       ((eq c ?n) (org-footnote-normalize))
       ((eq c ?d) (org-footnote-delete))
       (t (error "No such footnote command %c" c))))
     ((setq tmp (org-footnote-at-reference-p))
      (cond
       ;; Anonymous footnote: move point at the beginning of its
       ;; definition.
       ((not (car tmp))
	(goto-char (nth 1 tmp))
	(forward-char 5))
       ;; A definition exists: move to it.
       ((ignore-errors (org-footnote-goto-definition (car tmp))))
       ;; No definition exists: offer to create it.
       ((yes-or-no-p (format "No definition for %s.  Create one? " (car tmp)))
	(org-footnote-create-definition (car tmp)))))
     ((setq tmp (org-footnote-at-definition-p))
      (org-footnote-goto-previous-reference (car tmp)))
     (t (org-footnote-new)))))

(defvar org-footnote-insert-pos-for-preprocessor 'point-max
  "See `org-footnote-normalize'.")

(defvar org-export-footnotes-seen nil) ; silence byte-compiler
(defvar org-export-footnotes-data nil) ; silence byte-compiler

;;;###autoload
(defun org-footnote-normalize (&optional sort-only export-props)
  "Collect the footnotes in various formats and normalize them.

This finds the different sorts of footnotes allowed in Org, and
normalizes them to the usual [N] format that is understood by the
Org-mode exporters.

When SORT-ONLY is set, only sort the footnote definitions into the
referenced sequence.

If Org is amidst an export process, EXPORT-PROPS will hold the
export properties of the buffer.

When EXPORT-PROPS is non-nil, the default action is to insert
normalized footnotes towards the end of the pre-processing buffer.
Some exporters like docbook, odt, etc. expect that footnote
definitions be available before any references to them.  Such
exporters can let bind `org-footnote-insert-pos-for-preprocessor' to
symbol 'point-min to achieve the desired behaviour.

Additional note on `org-footnote-insert-pos-for-preprocessor':
1. This variable has not effect when FOR-PREPROCESSOR is nil.
2. This variable (potentially) obviates the need for extra scan
   of pre-processor buffer as witnessed in
   `org-export-docbook-get-footnotes'."
  ;; This is based on Paul's function, but rewritten.
  ;;
  ;; Re-create `org-with-limited-levels', but not limited to Org
  ;; buffers.
  (let* ((limit-level
	  (and (boundp 'org-inlinetask-min-level)
	       org-inlinetask-min-level
	       (1- org-inlinetask-min-level)))
	 (nstars (and limit-level
		      (if org-odd-levels-only
			  (and limit-level (1- (* limit-level 2)))
			limit-level)))
	 (org-outline-regexp
	  (concat "\\*" (if nstars (format "\\{1,%d\\} " nstars) "+ ")))
	 ;; Determine the highest marker used so far.
	 (ref-table (when export-props org-export-footnotes-seen))
	 (count (if (and export-props ref-table)
		    (apply 'max (mapcar (lambda (e) (nth 1 e)) ref-table))
		  0))
	 ins-point ref)
    (save-excursion
      ;; 1. Find every footnote reference, extract the definition, and
      ;;    collect that data in REF-TABLE.  If SORT-ONLY is nil, also
      ;;    normalize references.
      (goto-char (point-min))
      (while (setq ref (org-footnote-get-next-reference))
	(let* ((lbl (car ref))
	       ;; When footnote isn't anonymous, check if it's label
	       ;; (REF) is already stored in REF-TABLE.  In that case,
	       ;; extract number used to identify it (MARKER).  If
	       ;; footnote is unknown, increment the global counter
	       ;; (COUNT) to create an unused identifier.
	       (a (and lbl (assoc lbl ref-table)))
	       (marker (or (nth 1 a) (incf count)))
	       ;; Is the reference inline or pointing to an inline
	       ;; footnote?
	       (inlinep (or (stringp (nth 3 ref)) (nth 3 a))))
	  ;; Replace footnote reference with [MARKER].  Maybe fill
	  ;; paragraph once done.  If SORT-ONLY is non-nil, only move
	  ;; to the end of reference found to avoid matching it twice.
	  ;; If EXPORT-PROPS isn't nil, also add `org-footnote'
	  ;; property to it, so it can be easily recognized by
	  ;; exporters.
	  (if sort-only
	      (goto-char (nth 2 ref))
	    (delete-region (nth 1 ref) (nth 2 ref))
	    (goto-char (nth 1 ref))
	    (let ((new-ref (format "[%d]" marker)))
	      (when export-props (org-add-props new-ref '(org-footnote t)))
	      (insert new-ref))
	    (and inlinep
		 org-footnote-fill-after-inline-note-extraction
		 (org-fill-paragraph)))
	  ;; Add label (REF), identifier (MARKER) and definition (DEF)
	  ;; to REF-TABLE if data was unknown.
	  (unless a
	    (let ((def (or (nth 3 ref)	; inline
			   (and export-props
				(cdr (assoc lbl org-export-footnotes-data)))
			   (nth 3 (org-footnote-get-definition lbl)))))
	      (push (list lbl marker
			  ;; When exporting, each definition goes
			  ;; through `org-export-preprocess-string' so
			  ;; it is ready to insert in the
			  ;; backend-specific buffer.
			  (if export-props
			      (let ((parameters
				     (org-combine-plists
				      export-props
				      '(:todo-keywords t :tags t :priority t))))
				(org-export-preprocess-string def parameters))
			    def)
			  inlinep) ref-table)))
	  ;; Remove definition of non-inlined footnotes.
	  (unless inlinep (org-footnote-delete-definitions lbl))))
      ;; 2. Find and remove the footnote section, if any.  Also
      ;;    determine where footnotes shall be inserted (INS-POINT).
      (goto-char (point-min))
      (cond
       ((org-mode-p)
	(if (and org-footnote-section
		 (re-search-forward
		  (concat "^\\*[ \t]+" (regexp-quote org-footnote-section)
			  "[ \t]*$")
		  nil t))
	    (progn
	      (setq ins-point (match-beginning 0))
	      (delete-region (match-beginning 0) (org-end-of-subtree t)))
	  (setq ins-point (point-max))))
       (t
	(when (re-search-forward
	       (concat "^"
		       (regexp-quote org-footnote-tag-for-non-org-mode-files)
		       "[ \t]*$")
	       nil t)
	  (replace-match ""))
	;; In message-mode, ensure footnotes are inserted before the
	;; signature.
	(let ((pt-max
	       (or (and (derived-mode-p 'message-mode)
			(save-excursion
			  (goto-char (point-max))
			  (re-search-backward
			   message-signature-separator nil t)
			  (1- (point))))
		   (point-max))))
	  (goto-char pt-max)
	  (skip-chars-backward " \t\n\r")
	  (forward-line)
	  (delete-region (point) pt-max))
	(setq ins-point (point))))
      ;; 3. Clean-up REF-TABLE.
      (setq ref-table
	    (delq nil
		  (mapcar
		   (lambda (x)
		     (cond
		      ;; When only sorting, ignore inline footnotes.
		      ((and sort-only (nth 3 x)) nil)
		      ;; No definition available: provide one.
		      ((not (nth 2 x))
		       (append (butlast x 2)
			       (list (format "DEFINITION NOT FOUND: %s" (car x))
				     (nth 3 x))))
		      (t x)))
		   ref-table)))
      (setq ref-table (nreverse ref-table))
      ;; 4. Insert the footnotes again in the buffer, at the
      ;;    appropriate spot.
      (goto-char (or
		  (and export-props
		       (eq org-footnote-insert-pos-for-preprocessor 'point-min)
		       (point-min))
		  ins-point
		  (point-max)))
      (cond
       ;; No footnote: exit.
       ((not ref-table))
       ;; Cases when footnotes should be inserted in one place.
       ((or (not (org-mode-p))
	    org-footnote-section
	    (not sort-only))
	;; Insert again the section title.
	(cond
	 ((not (org-mode-p))
	  (insert "\n\n" org-footnote-tag-for-non-org-mode-files "\n"))
	 ((and org-footnote-section (not export-props))
	  (or (bolp) (insert "\n"))
	  (insert "* " org-footnote-section "\n")))
	;; Insert the footnotes.
	(insert "\n"
		(mapconcat (lambda (x) (format "[%s] %s"
					  (nth (if sort-only 0 1) x) (nth 2 x)))
			   ref-table "\n\n")
		"\n\n")
	;; When exporting, add newly inserted markers along with their
	;; associated definition to `org-export-footnotes-seen'.
	(when export-props
	  (setq org-export-footnotes-seen ref-table)))
       ;; Else, insert each definition at the end of the section
       ;; containing their first reference.  Happens only in Org files
       ;; with no special footnote section, and only when doing
       ;; sorting.
       (t (mapc 'org-insert-footnote-reference-near-definition
		ref-table))))))

(defun org-insert-footnote-reference-near-definition (entry)
  "Find first reference of footnote ENTRY and insert the definition there.
ENTRY is (fn-label num-mark definition)."
  (when (car entry)
    (goto-char (point-min))
    (let ((ref (org-footnote-get-next-reference (car entry))))
      (when ref
	(goto-char (nth 2 ref))
	(org-footnote-goto-local-insertion-point)
	(insert (format "\n[%s] %s\n" (car entry) (nth 2 entry)))))))

(defun org-footnote-goto-local-insertion-point ()
  "Find insertion point for footnote, just before next outline heading."
  (org-with-limited-levels (outline-next-heading))
  (or (bolp) (newline))
  (beginning-of-line 0)
  (while (and (not (bobp)) (= (char-after) ?#))
    (beginning-of-line 0))
  (if (looking-at "[ \t]*#\\+TBLFM:") (beginning-of-line 2))
  (end-of-line 1)
  (skip-chars-backward "\n\r\t ")
  (forward-line))

(defun org-footnote-delete-references (label)
  "Delete every reference to footnote LABEL.
Return the number of footnotes removed."
  (save-excursion
    (goto-char (point-min))
    (let (ref (nref 0))
      (while (setq ref (org-footnote-get-next-reference label))
	(goto-char (nth 1 ref))
	(delete-region (nth 1 ref) (nth 2 ref))
	(incf nref))
      nref)))

(defun org-footnote-delete-definitions (label)
  "Delete every definition of the footnote LABEL.
Return the number of footnotes removed."
  (save-excursion
    (goto-char (point-min))
    (let ((def-re (concat "^\\[" (regexp-quote label) "\\]"))
	  (ndef 0))
      (while (re-search-forward def-re nil t)
	(let ((full-def (org-footnote-at-definition-p)))
	  (delete-region (nth 1 full-def) (nth 2 full-def)))
	(incf ndef))
      ndef)))

(defun org-footnote-delete (&optional label)
  "Delete the footnote at point.
This will remove the definition (even multiple definitions if they exist)
and all references of a footnote label.

If LABEL is non-nil, delete that footnote instead."
  (catch 'done
    (let* ((nref 0) (ndef 0) x
	   ;; 1. Determine LABEL of footnote at point.
	   (label (cond
		   ;; LABEL is provided as argument.
		   (label)
		   ;; Footnote reference at point. If the footnote is
		   ;; anonymous, delete it and exit instead.
		   ((setq x (org-footnote-at-reference-p))
		    (or (car x)
			(progn
			  (delete-region (nth 1 x) (nth 2 x))
			  (message "Anonymous footnote removed")
			  (throw 'done t))))
		   ;; Footnote definition at point.
		   ((setq x (org-footnote-at-definition-p))
		    (car x))
		   (t (error "Don't know which footnote to remove")))))
      ;; 2. Now that LABEL is non-nil, find every reference and every
      ;; definition, and delete them.
      (setq nref (org-footnote-delete-references label)
	    ndef (org-footnote-delete-definitions label))
      ;; 3. Verify consistency of footnotes and notify user.
      (org-footnote-auto-adjust-maybe)
      (message "%d definition(s) of and %d reference(s) of footnote %s removed"
	       ndef nref label))))

(defun org-footnote-renumber-fn:N ()
  "Renumber the simple footnotes like fn:17 into a sequence in the document."
  (interactive)
  (let (map i (n 0))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(while (re-search-forward "\\[fn:\\([0-9]+\\)[]:]" nil t)
	  (setq i (string-to-number (match-string 1)))
	  (when (and (string-match "\\S-" (buffer-substring
					   (point-at-bol) (match-beginning 0)))
		     (not (assq i map)))
	    (push (cons i (number-to-string (incf n))) map)))
	(goto-char (point-min))
	(while (re-search-forward "\\(\\[fn:\\)\\([0-9]+\\)\\([]:]\\)" nil t)
	  (replace-match (concat "\\1" (cdr (assq (string-to-number (match-string 2)) map)) "\\3")))))))

(defun org-footnote-auto-adjust-maybe ()
  "Renumber and/or sort footnotes according to user settings."
  (when (memq org-footnote-auto-adjust '(t renumber))
    (org-footnote-renumber-fn:N))
  (when (memq org-footnote-auto-adjust '(t sort))
    (let ((label (car (org-footnote-at-definition-p))))
      (org-footnote-normalize 'sort)
      (when label
	(goto-char (point-min))
	(and (re-search-forward (concat "^\\[" (regexp-quote label) "\\]")
				nil t)
	     (progn (insert " ")
		    (just-one-space)))))))

(provide 'org-footnote)

;; arch-tag: 1b5954df-fb5d-4da5-8709-78d944dbfc37

;;; org-footnote.el ends here
