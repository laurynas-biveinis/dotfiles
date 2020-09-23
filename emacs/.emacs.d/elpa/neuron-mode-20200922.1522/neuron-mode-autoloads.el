;;; neuron-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "neuron-mode" "neuron-mode.el" (0 0 0 0))
;;; Generated autoloads from neuron-mode.el

(autoload 'neuron-zettelkasten "neuron-mode" "\
The location of the current Zettelkasten directory.
First, it tries to detect automatically the current zettelkasten assuming
the working directory is PWD, by traversing upwards in the directory
hierarchy until a neuron.dhall file is met, and returns
`neuron-default-zettelkasten-directory' when no neuron.dhall was found.
If in turn `neuron-default-zettelkasten-directory' doesn't point to an
existing directory, throw an user error.

\(fn &optional PWD)" nil nil)

(autoload 'neuron-refresh "neuron-mode" "\
Regenerate the zettel cache and the title overlays in all neuron-mode buffers." t nil)

(autoload 'neuron-new-zettel "neuron-mode" "\
Create a new zettel and open it in a new buffer.
The new zettel will be generated with the given TITLE and ID if specified.
When TITLE is nil, prompt the user.

\(fn &optional TITLE ID)" t nil)

(autoload 'neuron-open-daily-notes "neuron-mode" "\
Create or open today's daily notes." t nil)

(autoload 'neuron-edit-zettel "neuron-mode" "\
Select and edit ZETTEL.

\(fn ZETTEL)" t nil)

(autoload 'neuron-edit-zettelkasten-configuration "neuron-mode" "\
Open the neuron.dhall configuration file at the root of the zettelkasten." t nil)

(autoload 'neuron-query-tags "neuron-mode" "\
Select and edit a zettel from those that are tagged by TAGS.

\(fn &rest TAGS)" t nil)

(autoload 'neuron-open-zettel "neuron-mode" "\
Select a zettel and open the associated HTML file." t nil)

(autoload 'neuron-follow-thing-at-point "neuron-mode" "\
Open the zettel link at point." t nil)

(autoload 'neuron-rib-watch "neuron-mode" "\
Start a web app for browsing the zettelkasten." t nil)

(autoload 'neuron-rib-serve "neuron-mode" "\
Start a web app for browsing the zettelkasten." t nil)

(autoload 'neuron-rib-generate "neuron-mode" "\
Do an one-off generation of the web interface of the zettelkasten." t nil)

(autoload 'neuron-rib-open-page "neuron-mode" "\
Open the web-application at page PAGE.

\(fn PAGE)" nil nil)

(autoload 'neuron-rib-open-z-index "neuron-mode" "\
Open the web application in the web browser at z-index." t nil)

(autoload 'neuron-rib-open-zettel "neuron-mode" "\
Open a zettel in the web application." t nil)

(autoload 'neuron-replace-tag "neuron-mode" "\
Map all tags matching PATTERN to a REPL.
PATTERN is a tag glob as used in neuron queries.
REPL is a string that may contain substrings like `\\N' where
N denotes the tag components that were matched by the Nth glob
pattern.
Example:
`(neuron-add-tag \"**/theorem\" \"math/theorem/\\1\")'
will replace number-theory/theorem to math/theorem/number-theory
and algebra/linear/theorem to math/theorem/algebra/linear.

\(fn PATTERN REPL)" t nil)

(autoload 'neuron-toggle-id-visiblity "neuron-mode" "\
Toggle the visibility of IDs in simple links.
This can be useful to debug when searching for ID, explicitly seeing whether the
link is a folgezettel of ordinary connection." t nil)

(autoload 'company-neuron "neuron-mode" "\
Defines a company completion backend that completes zettels by title.
COMMAND is the relevant command provided by company.
ARG is the command argument, depending on which command was received.
IGNORED is the rest of the arguments, not sure why it's there.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(autoload 'company-neuron-setup "neuron-mode" "\
Setup company to use the neuron backend." nil nil)

(autoload 'neuron-mode "neuron-mode" "\
A major mode to edit Zettelkasten notes with neuron.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "neuron-mode" '("company-neuron--" "neuron-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; neuron-mode-autoloads.el ends here
