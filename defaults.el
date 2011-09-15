; ---------------
; Config defaults
; ---------------
; User info
(setq user-full-name "Laurynas Biveinis")
(setq user-mail-address "laurynas.biveinis@gmail.com")

; Keep all messages
(setq message-log-max t)

; Proper clipboard, why oh why this isn't the default?
(setq x-select-enable-clipboard t)

; Emacs 23.2: Active region becomes primary selection
(if (fboundp 'select-active-regions)
    (setq select-active-regions t))

; C-k kills line including its newline
(setq kill-whole-line t)

; Emacs 23.2+: do not store duplicate kills
(if (fboundp 'kill-do-not-save-duplicates)
    (setq kill-do-not-save-duplicates t))

; Bookmarks are saved automatically
(setq bookmark-save-flag 1)

; Trailing newlines are highlighted
; indicate-empty-lines for Emacs 23.2+, default-indicate-empty-lines for
; earlier versions
(if (fboundp 'indicate-empty-lines)
    (setq indicate-empty-lines t)
  (setq default-indicate-empty-lines t))

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

; Indentation can only insert spaces by default
(setq-default indent-tabs-mode nil)

; Diff options
(setq diff-switches "-c -p")

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
(setq-default buffer-file-coding-system 'utf-8-unix)

; XXI century encodings
(set-language-environment "UTF-8")

; No fancy input encodings
(set-input-method nil)

; No startup message
(setq inhibit-startup-message t)

; Initial frame positioned in the top left corner
(add-to-list 'initial-frame-alist '(top . 1))
(add-to-list 'initial-frame-alist '(left . 1))

; Frame geometry
(defun reset-frame-size ()
  "Re-size the current frame to be 98 columns x full height"
  (interactive)
  (if window-system
      (progn
        (let ((new-width 98)
              (new-height (/ (- (x-display-pixel-height)
                                110)
                             (frame-char-height)))
              (width-cell (assq 'width default-frame-alist))
              (height-cell (assq 'height default-frame-alist))
              )
          (set-frame-width (selected-frame) new-width)
          (set-frame-height (selected-frame) new-height)
          (if (consp width-cell)
              (setcdr width-cell new-width)
            (add-to-list 'default-frame-alist `(width . ,new-width)))
          (if (consp height-cell)
              (setcdr height-cell new-height)
            (add-to-list 'default-frame-alist (cons 'height new-height)))
          )
        )
    )
)

(reset-frame-size)

; Treat new (empty) files as modified
(add-hook 'find-file-hooks
          '(lambda ()
             (when (not (file-exists-p (buffer-file-name)))
               (set-buffer-modified-p t))))

; Custom keybindings
(defun smart-home ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.  If point was
already at that position, move point to the beginning of line."
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line)
         )
    )
  )

; Keybindings
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-cn" 'next-error)
(global-set-key "\C-cp" 'previous-error)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key [(control shift up)] 'enlarge-window)
(global-set-key [(control shift down)] 'shrink-window)
(global-set-key [(control shift left)] 'enlarge-window-horizontally)
(global-set-key [(control shift right)] 'shrink-window-horizontally)

(global-set-key [home] 'smart-home)
