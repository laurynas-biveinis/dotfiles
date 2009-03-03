; ---------------
; Config defaults
; ---------------
; User info
(setq user-full-name "Laurynas Biveinis")
(setq user-mail-address "laurynas.biveinis@gmail.com")

; Proper clipboard, why oh why this isn't the default?
(setq x-select-enable-clipboard t)

; C-k kills line including its newline
(setq kill-whole-line t)

; Bookmarks are saved automatically
(setq bookmark-save-flag 1)

; Trailing newlines are highlighted
(setq default-indicate-empty-lines t)

; 24h time format
(setq display-time-24hr-format t)

; Should files end with newline?
(setq require-final-newline 'query)

; Display trailing whitespace
(setq show-trailing-whitespace t)

; Compilation window follows last output
(setq compilation-scroll-window t)

; No annoying beeps
(setq visible-bell t)

; Preserve hard links to the file you are editing
; From http://www.emacswiki.org/cgi-bin/wiki/DotEmacsChallenge
(setq backup-by-copying-when-linked t)

; Preserve the owner and group of the file you are editing
; From http://www.emacswiki.org/cgi-bin/wiki/DotEmacsChallenge
(setq backup-by-copying-when-mismatch t)

; Do not backup
(setq make-backup-files nil)

; Ask for initial file checking comment
(setq vc-initial-comment t)

; Use Unix-style line endings.
(setq-default buffer-file-coding-system 'undecided-unix)

; XXI century encodings
(set-language-environment "UTF-8")

; No fancy input encodings
(set-input-method nil)

; No startup message
(setq inhibit-startup-message t)
