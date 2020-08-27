Colorized mode line indicator with icons to display the `flycheck-mode' status.

This package provides the `flycheck-indicator-mode' minor mode which
displays a colorized mode line with icons for `flycheck-mode' status.

To enable this mode in Flycheck, add it to `flycheck-mode-hook':

(add-hook 'flycheck-mode-hook 'flycheck-indicator-mode)
