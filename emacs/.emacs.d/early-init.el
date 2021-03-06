;;;; early-init.el --- my Emacs early init.  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; GC less during initialization
(setq gc-cons-threshold most-positive-fixnum)

;;; General early settings
(setq load-prefer-newer t)

(setq blink-cursor-blinks -1)

(setq x-stretch-cursor t)

;; https://github.com/hlissner/doom-emacs/blob/develop/early-init.el -
;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
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

;;; early-init.el ends here
