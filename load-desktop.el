;; Finally, load the desktop
(desktop-save-mode 1)
(setq history-length 250)
(add-to-list 'desktop-globals-to-save 'file-name-history)
; Compatibility with Semantic/ECB
(add-to-list 'desktop-minor-mode-table '(ecb-minor-mode nil))
(add-to-list 'desktop-minor-mode-table
             '(semantic-show-unmatched-syntax-mode nil))
(add-to-list 'desktop-minor-mode-table '(semantic-stickyfunc-mode nil))
(add-to-list 'desktop-minor-mode-table '(senator-minor-mode nil))
(add-to-list 'desktop-minor-mode-table '(semantic-idle-scheduler-mode nil))
