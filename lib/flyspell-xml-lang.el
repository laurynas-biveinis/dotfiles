;; flyspell-xml-lang.el -- Switch flyspell language according to
;;                         xml:lang attributes
;;
;; Copyright (C) 2004, 2005 P J Heslin
;;
;; Author: Peter Heslin <p.j.heslin@dur.ac.uk>
;; URL: http://www.dur.ac.uk/p.j.heslin/Software/Emacs/Download/flyspell-xml-lang.el
;; Version: 2.1
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; If you do not have a copy of the GNU General Public License, you
;; can obtain one by writing to the Free Software Foundation, Inc., 59
;; Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Overview:
;;
;; Flyspell is an Emacs package that highlights misspelled words as
;; you type; for XML documents, this package will switch the current
;; language dictionary used by flyspell to match the language declared
;; by the xml:lang attribute of the current document element.  The
;; ispell dictionary currently in force is shown in the modeline.

;;; Installation:
;;
;; This package requires ispell-multi.el to be installed; it may be
;; found in the same directory indicated by the URL above.  It also
;; requires flyspell to be installed (version 1.7f or better) and
;; flyspell-mode to be active. I have only tested this package with
;; GNU Emacs.  If you are using Emacs 21.3 or earlier, you will need
;; to upgrade to ispell.el version 3.6, available from
;; http://www.kdstevens.com/~stevens/ispell-page.html.
;;
;; To use this package, put it somewhere in your load-path, and add
;; this to your .emacs file:
;;
;;    (autoload 'flyspell-xml-lang-setup "flyspell-xml-lang")
;;    (add-hook 'xml-mode-hook 'flyspell-xml-lang-setup)
;;
;; If you use nxml-mode, then put this line instead of the second one
;; above:
;;
;;    (add-hook 'nxml-mode-hook 'flyspell-xml-lang-setup)
;;
;; You will need to reload the present file if you install any new
;; ispell languages or language aliases.

;;; Customization (languages):
;;
;; You may not need to do any customization (apart from making sure
;; that the relevant ispell/aspell dictionaries are installed) if you
;; are using languages which are supported out-of-the-box by
;; ispell.el; as of this writing, these include: English, Czech,
;; Danish, German, Esperanto, Spanish, French, Italian, Dutch,
;; Norwegian, Polish, Portuguese, Russian, Slovak, and Swedish.
;;
;; If you want to use any of the many other languages supported by
;; aspell/ispell, you will first need to configure ispell.el to work
;; with them, since flyspell.el depends on ispell.el.  First, install
;; the relevant dictionary for ispell/aspell, and make sure that it
;; works correctly with ispell/aspell at the command-line.
;;
;; To explain how to configure ispell.el to add support for other
;; languages, here is an illustration, using the Irish language as an
;; example.
;;
;; First, you need to add some lines like these to your .emacs file;
;; see the documentation of ispell-local-dictionary-alist in ispell.el
;; for the details:
;;
;;    (setq ispell-local-dictionary-alist
;;          `(("irish"			
;;        "[A-Z\301\311\315\323\332a-z\341\351\355\363\372]"
;;        "[^A-Z\301\311\315\323\332a-z\341\351\355\363\372]"
;;        "[-']" nil ("-B" "--lang=ga") "~latin1" iso-8859-1)))
;;
;; Note that "irish" is the name of the dictionary for ispell.el, but
;; for aspell it is called "ga", and this information is passed to
;; aspell via "--lang=ga".
;;
;; Now restart Emacs, and do M-x ispell-change-dictionary RET irish
;; RET; make sure ispell for Irish is working by doing M-x
;; ispell-buffer in a buffer with some Irish words.
;;
;; Once ispell.el is happy, you can configure flyspell-xml-lang like
;; so (where "ga", "gai", and "iri" are possible ISO 639 language
;; codes that might appear as values of the xml:lang attribute, and
;; "irish" is the name of the ispell dictionary configured above):
;;
;;    (setq flyspell-xml-lang-to-ispell-local-alist
;;          `(("ga" "irish")
;;            ("gai" "irish")
;;            ("iri" "irish")))
;;
;; Now reload the present package and test it on a file like this:
;;     
;;    <main xml:lang="en">
;;      <foreign xml:lang="it">ciao</foreign> world!
;;      <quote xml:lang="ga">
;;        tá riabh go maith agat
;;      </quote>
;;    Good-bye!
;;    </main>
;;
;; Now all languages should be spell-checked appropriately.
;;
;; For other aspell dictionaries, see
;; http://aspell.sourceforge.net/man-html/Supported.html
;; and for ISO 639 codes, see
;; http://www.w3.org/WAI/ER/IG/ert/iso639.htm

;;; Customization (misc.)
;;
;; The forthcoming version of the Text Encoding Initiative (P5) will
;; recommend using the xml:lang attribute, but older documents may use
;; the lang attribute.  You can change the attribute that controls the
;; language by customizing flyspell-xml-lang-attr to "lang" or
;; anything you like.  The value of that attribute will be looked up
;; in flyspell-xml-lang-to-ispell-alist.
;;
;; You can give a language a value of 'nil in
;; flyspell-xml-lang-to-ispell-alist if you want to inhibit
;; spell-checking for that language.

;;; Bugs
;;
;; 1) This is not an XML parser.  It may get confused by things out of
;;    the ordinary: not well-formed documents, comments, processing
;;    instructions, etc.  For example, if you are using Emacs'
;;    built-in xml-mode and you comment out a tag with an xml:lang
;;    attribute, and expect it to be ignored, you will be
;;    disappointed.  On the other hand, if you are using nxml-mode,
;;    comments should be ignored correctly, because nxml-mode includes
;;    a real parser, and flyspell-xml-lang makes some use of its
;;    facilities.  But it is very possible that, even when using
;;    nxml-mode, there may be times when flyspell-xml-lang
;;    misinterprets the syntax of the XML.
;;
;; 2) If you add or change the language of an existing element in your
;;    document, you may find that at first you get the wrong language
;;    dictionary, and so words are wrongly marked as misspelled.  If
;;    this happens, just stop typing and leave point inside the
;;    element.  After a short (configurable) interval of idleness
;;    (default is 5 seconds), the element will be re-parsed, the
;;    correct language should be discovered, and then moving the
;;    cursor over the words should remove the indications of
;;    misspelling.
;;
;;    If you map an ISO language code to nil, that means that such
;;    elements will not be spell-checked at all, which can be a useful
;;    feature for languages that lack a spell-check dictionary.
;;    Unfortunately, that means that, if you try the trick above,
;;    after changing a pre-existing element to a language mapped to
;;    nil, any unwanted, previously existing misspelling indications
;;    will remain (since these words are now not spell-checked at all,
;;    but completely ignored).  In this circumstance, the way to get
;;    rid of the unwanted misspelling marks is to switch flyspell-mode
;;    off and then on again.

;;; Changes
;;
;; 2.1 Made flyspell-xml-lang-parse-element non-recursive to avoid
;;     blowing max-lisp-eval-depth in long files
;; 2.0 Code re-factored into ispell-multi.el.
;; 1.0 Initial public release.

;;; Code:

(require 'ispell-multi)
(require 'flyspell)
; For 21.3, we have to use an updated ispell.el (3.6), and for some
; reason we may have to load it again to get ispell-dictionary-alist
; set properly.
;(unless (assoc "english" ispell-dictionary-alist)
;  (load "ispell"))
; For updated ispell.el with emacs < 21.3.5
;(when (not (fboundp 'set-process-query-on-exit-flag))
;  (defun set-process-query-on-exit-flag (one two) ()))

(defgroup flyspell-xml-lang nil
  "Switch flyspell language according to xml:lang attributes"
  :tag "Switch flyspell language according to xml:lang attributes"
  :group 'flyspell
  :prefix "flyspell-xml-lang-")

(defcustom flyspell-xml-lang-to-ispell-local-alist 'nil
  "User additions to the list that maps ISO 639 language codes to
  ispell dictionaries.  You must first customize
  ispell-local-dictionary-alist, so that ispell accepts it as a
  value for ispell-change-dictionary.  Then provide a mapping
  here from ISO 639 to the ispell name.  A value of nil means not
  to spellcheck this language."
  :type 'alist
  :group 'flyspell-xml-lang)

(defcustom flyspell-xml-lang-local-dictionary nil
  "Normally, we use ispell-local-dictionary or ispell-dictionary
  to determine the language to assume for XML elements whose
  language is not defined via an xml:lang attribute.  If you want
  to override that value, set it here."
  :type 'string
  :group 'flyspell-xml-lang)

(defcustom flyspell-xml-lang-delay 5
  "Seconds of idleness before current element is re-parsed."
  :type 'integer
  :group 'flyspell-xml-lang)

(defcustom flyspell-xml-lang-attr "xml:lang"
  "The attribute that declares the language for the element"
  :type 'string
  :group 'flyspell-xml-lang)

(defvar flyspell-xml-lang-verbose nil
  "Print status messages if non-nil")


(defvar flyspell-xml-lang-to-ispell-alist-built-in nil
 "These are the dictionaries supported by ispell.el")
(setq flyspell-xml-lang-to-ispell-alist-built-in
`(
   ("cs" "czech")
   ("ces" "czech")
   ("cze" "czech")

   ("da" "dansk")
   ("dan" "dansk")

   ("de" "german8")
   ("deu" "german8")
   ("ger" "german8")

   ("en" "english")
   ("eng" "english")

   ("en-GB" "british")
   ("eng-GB" "british")

   ("en-US" "american")
   ("eng-US" "american")

   ("eo" "esperanto")
   ("epo" "esperanto")

   ("es" "castellano")
   ("esl" "castellano")
   ("spa" "castellano")

   ("fr" "francais")
   ("fra" "francais")
   ("fre" "francais")

   ("it" "italiano")
   ("ita" "italiano")

   ("nl" "nederlands")
   ("nla" "nederlands")
   ("dut" "nederlands")

   ("no" "norsk")

   ("pl" "polish")
   ("pol" "polish")

   ("pt" "portugues")
   ("por" "portugues")

   ("ru" "russian")
   ("rus" "russian")

   ("sk" "slovak")
   ("slk" "slovak")
   ("slo" "slovak")

   ("sv" "svenska")
   ("sve" "svenska")
   ("swe" "svenska")

   ))

(defvar flyspell-xml-lang-to-ispell-alist 'nil)
(setq flyspell-xml-lang-to-ispell-alist
      (append flyspell-xml-lang-to-ispell-local-alist
	      flyspell-xml-lang-to-ispell-alist-built-in))

(defvar flyspell-xml-lang-valid-dictionary-list nil
  "Cached value of ispell-valid-dictionary-list")
(when (fboundp 'ispell-valid-dictionary-list)
  (setq flyspell-xml-lang-valid-dictionary-list
        (ispell-valid-dictionary-list)))

(setq flyspell-xml-lang-attr-regexp
      (concat "\\=<\\([^ \t\n\r>]+\\)[^>]*"
              flyspell-xml-lang-attr
               "[ \t\n\r]*=[ \t\n\r]*\"\\([^\"]+\\)\""))

(defun flyspell-xml-lang-parse-buffer ()
  (save-excursion
    (goto-char (point-max))
    (flyspell-xml-lang-parse-element)))

(defun flyspell-xml-lang-parse ()
  "Parse backwards from point until we find a containing element
  with xml:lang or bob; then run forward and flag elements."
  (let ((current-position (point))
        beg end lang finished tag-list)
    (while (and (not finished)
                (not (input-pending-p))
                (re-search-backward "<[^/!?]" nil t))
      ;; If we are using nxml-mode, and we are in a comment, CDATA
      ;; section or processing instruction, this might move us out.
      (when (eq major-mode 'nxml-mode)
        (nxml-move-outside-backwards))
      (setq beg (point))
      (when (re-search-forward flyspell-xml-lang-attr-regexp nil t)
          (setq elem (match-string 1))
          (setq lang (match-string 2))
          (if (re-search-forward "\\=[^>]*/>" nil t)
              ;; Tag with empty content
              (setq end (point))
            (goto-char beg)
            (flyspell-xml-lang-find-end-of-element)
            (setq end (point)))
          (when (< current-position end)
            (setq finished t)
            ;; Got it -- but stuff between point and end of elem is
            ;; still unparsed.  As an optimization, we flag the text
            ;; after point as far as the start of the next tag.
            (goto-char current-position)
            (re-search-forward "<" end t)
            ;; If the next tag is the end of the current element, we
            ;; can flag through to the end of the tag.
            (when (looking-at "/")
              (re-search-forward ">" nil t))
            (setq end (point)))
          (setq tag-list (cons (list beg end lang) tag-list))
          (goto-char beg)))
    ;; Background is default lang
    (setq tag-list (cons (list (point-min) current-position "no-command-found") tag-list))
    ;; We now run through in a forward direction
    (while tag-list
      (apply 'flyspell-xml-lang-flag-region (car tag-list))
      (setq tag-list (cdr tag-list)))))

(defun flyspell-xml-lang-find-end-of-element ()
  (unless (looking-at "<")
    (flyspell-xml-lang-message "internal error 1")
    (flyspell-xml-lang-mode -1))
  (forward-char 1)
  (re-search-forward "\\=[^ \t\r\n>]+" nil t)
  (let ((elem (match-string 0))
        (level 1))
    (while (and (< 0 level)
                (re-search-forward
                 (concat "</?" elem "[ \t\r\n>]") nil t))
      (if (string= "</" (substring (match-string 0) 0 2))
          (setq level (1- level))
        (setq level (1+ level))))
    (when (> level 0)
      ;; no end tag
      (goto-char (point-max)))))
      
(defun flyspell-xml-lang-flag-region (beg end lang)
  (let ((trans (assoc lang flyspell-xml-lang-to-ispell-alist))
        (buffer-modified-before (buffer-modified-p))
        (after-change-functions nil))
    (when trans
      ;; We have a translation of an ISO language name to ispell
      ;; nomenclature
      (setq lang (cadr trans)))
    (cond
     ((not lang)
      ;; A parsed region with a nil dict -- don't spell-check.
      (setq lang "void"))
     ((equal lang "no-command-found")
      (setq lang "default"))
     ((and ispell-multi-valid-dictionary-list
           (not (member lang ispell-multi-valid-dictionary-list)))
      ;; A parsed region with an uninstalled dict
      (message "Flyspell-babel warning: no dictionary installed for %s" lang)
      (setq lang "void"))) 
    (when (> end (point-max)) (setq end (point-max)))
    (put-text-property (1+ beg) end 'ispell-multi-lang lang)
    (set-buffer-modified-p buffer-modified-before)))

(defun flyspell-xml-lang-message (mess &optional force)
  (when (or flyspell-xml-lang-verbose force)
    (message "flyspell-xml-lang -- %s" mess)))

(defun flyspell-xml-lang-start ()
  (setq ispell-multi-nil-callback 'flyspell-xml-lang-parse)
  (make-local-variable 'flyspell-large-region)
  (setq flyspell-large-region 'nil)
  (flyspell-mode 1)
  (setq flyspell-generic-check-word-p 'ispell-multi-verify)
  (setq ispell-multi-idler-callback 'flyspell-xml-lang-parse-buffer)
  (ispell-multi-idler-setup flyspell-xml-lang-delay)
  (ispell-multi-hack-flyspell-modeline))
  
(defun flyspell-xml-lang-stop ()
;  (ispell-multi-idler-cancel)
  (setq flyspell-generic-check-word-p nil)
  (ispell-multi-unhack-flyspell-modeline)
  (flyspell-mode -1))

(define-minor-mode flyspell-xml-lang-mode
  "Mode to make flyspell language selection obey xml:lang attributes" nil 
      :group 'flyspell-xml-lang
      (if flyspell-xml-lang-mode
          (flyspell-xml-lang-start)
	(flyspell-xml-lang-stop)))

(defun flyspell-xml-lang-setup ()
  (flyspell-xml-lang-mode 1))

(provide 'flyspell-xml-lang)

