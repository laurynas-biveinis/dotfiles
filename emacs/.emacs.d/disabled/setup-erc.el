;;; setup-erc.el --- ERC config.  -*- lexical-binding: t; -*-
;;; Commentary:
;; ERC configuration
;;; Code:

;; Defined in secrets.el:
(declare-function start-erc-chats "" ())

;; If ever re-enabled, add `erc-mode' to the `'not' list in
;; `whitespace-global-modes'.

(require 'erc)

(add-to-list 'aggressive-indent-excluded-modes #'erc-mode)

(setq erc-user-full-name user-full-name)

(require 'erc-spelling)
(add-hook 'erc-mode-hook #'erc-spelling-mode)

(require 'erc-log)

;; (setq erc-log-insert-log-on-open t)

(setq erc-save-buffer-on-part nil
      erc-save-queries-on-quit nil
      erc-log-write-after-send t
      erc-log-write-after-insert t)

(setq erc-join-buffer 'bury)

(require 'erc-networks)
(defun dotfiles--erc-generate-log-file-name-channel-network
    (buffer target _nick server _port)
  "Generate an ERC log file name in form of #channel@network.txt.
BUFFER, TARGET, NICK, SERVER, and PORT are ERC-provided."
  (require 'erc-networks)
  (let ((file (concat
               target "!"
               (or (with-current-buffer buffer (erc-network-name)) server)
               ".txt")))
    ;; we need a make-safe-file-name function.
    (convert-standard-filename file)))

(setq erc-generate-log-file-name-function
      #'dotfiles--erc-generate-log-file-name-channel-network)

(erc-log-enable)

(require 'erc-track)
(setq erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE"))

(setq erc-track-faces-priority-list
      '(erc-current-nick-face erc-keyword-face))

(setq erc-paranoid t)

;; TODO(laurynas): this is in-buffer highlight, right?
;; (require 'erc-highlight-nicknames)
;; (add-to-list 'erc-modules 'highlight-nicknames)
;; (erc-update-modules)

(require 'erc-services)
(erc-services-mode 1)

(setq erc-prompt-for-nickserv-password nil)

(setq erc-server-coding-system '(utf-8 . utf-8))

(setq erc-server-reconnect-attempts 0)

(require 'erc-join)
(setq erc-autojoin-timing 'ident)

(defun start-chats ()
  "Connect to all chats."
  (interactive)
  (start-erc-chats))

(defun stop-chats ()
  "Disconnect from all chats."
  (interactive)
  (erc-cmd-GQ "Leaving"))

(provide 'setup-erc)
;;; setup-erc.el ends here
