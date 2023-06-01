;;;; early-init.el --- my Emacs early init.  -*- lexical-binding: t; -*-
;;; Commentary:

;; This file contains settings that are set early in the startup
;; process of Emacs, before package and UI initialization takes place.

;;; Code:

;; Increase the garbage collection threshold to maximum to minimize the impact
;; of garbage collection during startup. This will be reset to a reasonable
;; default after initialization.
(setq gc-cons-threshold most-positive-fixnum)

;;; General early settings

(setq load-prefer-newer t)

;; The cursor should blink forever
(setq blink-cursor-blinks -1)

(setq x-stretch-cursor t)

;; https://github.com/doomemacs/doomemacs/blob/master/lisp/doom-start.el -
;; Prevent the glimpse of un-styled Emacs by disabling these UI elements (menu
;; bar, tool bar, and vertical scroll bars) early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; Temporarily unset the file name handler alist for faster startup. It's restored
;; after initialization.
(unless (daemonp)
  (defvar dotfiles--initial-file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil))

;;; early-init.el ends here
