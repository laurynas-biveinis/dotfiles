;;;; my-ui-geometry.el --- my UI geometry.  -*- lexical-binding: t; -*-
;;; Commentary:

;; This provides commands for setting up windows in my preferred ways, as well
;; as adjusting frame and window geometry based on the current screen size,
;; useful for docking and undocking laptops. Depends on the `dispwatch' package.

;;; Code:

(defun two-windows ()
  "Make frame contain two vertical windows."
  (interactive)
  (delete-other-windows)
  (split-window-right))

(defun six-windows ()
  "Make frame contain 2x3 windows."
  (interactive)
  (delete-other-windows)
  (split-window-below)
  (split-window-right)
  (split-window-right)
  (windmove-down)
  (split-window-right)
  (split-window-right)
  (balance-windows))

(defun eight-windows ()
  "Make frame contain 2x4 windows."
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "*scratch*")
  (split-window-below)
  (split-window-right)
  (split-window-right)
  (split-window-right)
  (windmove-down)
  (split-window-right)
  (split-window-right)
  (split-window-right)
  (balance-windows))

(require 'cl-lib)
(cl-defstruct dotfiles--frame-dimensions top left height width)

(defun dotfiles--add-frame-geometry-to-initial-alist (geometry)
  "Add frame GEOMETRY to `initial-frame-alist'."
  (add-to-list 'initial-frame-alist
               `(top . ,(dotfiles--frame-dimensions-top geometry)))
  (add-to-list 'initial-frame-alist
               `(left . ,(dotfiles--frame-dimensions-left geometry)))
  (add-to-list 'initial-frame-alist
               `(height . ,(dotfiles--frame-dimensions-height geometry)))
  (add-to-list 'initial-frame-alist
               `(width . ,(dotfiles--frame-dimensions-width geometry))))

(defun dotfiles--move-to-frame-geometry (geometry)
  "Resize and reposition frame to GEOMETRY."
  (set-frame-position nil (dotfiles--frame-dimensions-left
                           geometry)
                      (dotfiles--frame-dimensions-top geometry))
  (set-frame-size nil (dotfiles--frame-dimensions-width geometry)
                  (dotfiles--frame-dimensions-height geometry)))

(defconst dotfiles--darkstar-laptop-display-size '(1440 . 900))
(defconst dotfiles--darkstar-laptop-frame-dimensions
  (make-dotfiles--frame-dimensions :top 1 :left 1 :height 55 :width 202))

(defconst dotfiles--darkstar-external-display-size '(7456 . 1692))
(defconst dotfiles--darkstar-external-frame-dimensions
  (make-dotfiles--frame-dimensions :top 4 :left 3011 :height 117 :width 426))

(defconst dotfiles--m1star-laptop-display-size '(1728 . 1117))
(defconst dotfiles--m1star-laptop-frame-dimensions
  (make-dotfiles--frame-dimensions :top 1 :left 1 :height 67 :width 242))

(defconst dotfiles--m1star-external-display-size '(3360 . 1890))
(defconst dotfiles--m1star-external-frame-dimensions
  (make-dotfiles--frame-dimensions :top 0 :left 5568 :height 135 :width 477))

(defconst dotfiles--work-laptop-display-size '(1728 . 1117))
(defconst dotfiles--work-laptop-frame-dimensions
  (make-dotfiles--frame-dimensions :top 1 :left 1 :height 67 :width 242))

(defconst dotfiles--work-external-display-size '(3008 . 1692))
(defconst dotfiles--work-external-frame-dimensions
  (make-dotfiles--frame-dimensions :top 0 :left 3008 :height 117 :width 427))

(defconst dotfiles--ignored-display-sizes
  [(3600 . 1080) (5520 . 1080) (4688 . 1692) (3600 . 1692) (7744 . 1692)
   (1920 . 1080) (3840 . 2160) (8928 . 2160)]
  "Possible interim screen resolutions while docking/undocking to be ignored.")

(defun dotfiles--warn-about-unknown-display-geometry (display-geometry)
  "Diagnose unknown DISPLAY-GEOMETRY."
  (message "Unknown display size %sx%s"
           (car display-geometry) (cdr display-geometry)))

(require 'seq)
(let ((display-geometry (cons (display-pixel-width) (display-pixel-height))))
  (cond ((equal display-geometry dotfiles--darkstar-laptop-display-size)
         ;; darkstar without external screens: initial frame positioned in the
         ;; top left corner
         (dotfiles--add-frame-geometry-to-initial-alist
          dotfiles--darkstar-laptop-frame-dimensions)
         (two-windows))
        ((equal display-geometry dotfiles--darkstar-external-display-size)
         ;; darkstar with external screens: initial frame maximized in the
         ;; middle screen
         (dotfiles--add-frame-geometry-to-initial-alist
          dotfiles--darkstar-external-frame-dimensions)
         (add-to-list 'initial-frame-alist '(fullscreen . fullboth))
         (add-to-list 'initial-frame-alist '(fullscreen-restore . maximized))
         (eight-windows))
        ((equal display-geometry dotfiles--m1star-laptop-display-size)
         ;; m1star without external screens: initial frame positioned in the top
         ;; left corner
         (dotfiles--add-frame-geometry-to-initial-alist
          dotfiles--m1star-laptop-frame-dimensions)
         (two-windows))
        ((equal display-geometry dotfiles--m1star-external-display-size)
         ;; m1star with external screens: initial frame maximized in the middle
         ;; screen
         (dotfiles--add-frame-geometry-to-initial-alist
          dotfiles--m1star-external-frame-dimensions)
         (add-to-list 'initial-frame-alist '(fullscreen . fullboth))
         (add-to-list 'initial-frame-alist '(fullscreen-restore . maximized))
         (eight-windows))
        ((equal display-geometry dotfiles--work-laptop-display-size)
         ;; work without external screens: initial frame positioned in the top
         ;; left corner
         (dotfiles--add-frame-geometry-to-initial-alist
          dotfiles--work-laptop-frame-dimensions)
         (two-windows))
        ((equal display-geometry dotfiles--work-external-display-size)
         ;; work with external screens: initial frame maximized in the middle
         ;; screen
         (dotfiles--add-frame-geometry-to-initial-alist
          dotfiles--work-external-frame-dimensions)
         (add-to-list 'initial-frame-alist '(fullscreen . fullboth))
         (add-to-list 'initial-frame-alist '(fullscreen-restore . maximized))
         (eight-windows))
        ;; Ignored and unknown geometries
        ((seq-position dotfiles--ignored-display-sizes display-geometry)
         ())
        (t (dotfiles--warn-about-unknown-display-geometry display-geometry))))


;;; dispwatch
(require 'dispwatch)
(defvar dotfiles--old-display-geometry nil)
(defun dotfiles--display-changed-hook (new-display-geometry)
  "Reconfigure windows on screen resolution change to NEW-DISPLAY-GEOMETRY."
  (unless (equal dotfiles--old-display-geometry new-display-geometry)
    (message "Resizing from %s to %s" dotfiles--old-display-geometry
             new-display-geometry)
    (setq dotfiles--old-display-geometry new-display-geometry)
    (cond ((equal new-display-geometry dotfiles--work-laptop-display-size)
           (dotfiles--move-to-frame-geometry
            dotfiles--work-laptop-frame-dimensions)
           (set-frame-parameter nil 'fullscreen 'maximized)
           (two-windows))
          ((equal new-display-geometry dotfiles--m1star-laptop-display-size)
           (dotfiles--move-to-frame-geometry
            dotfiles--m1star-laptop-frame-dimensions)
           (set-frame-parameter nil 'fullscreen 'maximized)
           (two-windows))
          ((equal new-display-geometry dotfiles--darkstar-laptop-display-size)
           (dotfiles--move-to-frame-geometry
            dotfiles--darkstar-laptop-frame-dimensions)
           (set-frame-parameter nil 'fullscreen 'maximized)
           (two-windows))
          ((equal new-display-geometry dotfiles--darkstar-external-display-size)
           (dotfiles--move-to-frame-geometry
            dotfiles--darkstar-external-frame-dimensions)
           (set-frame-parameter nil 'fullscreen 'fullboth)
           (eight-windows))
          ((equal new-display-geometry dotfiles--work-external-display-size)
           (dotfiles--move-to-frame-geometry
            dotfiles--work-external-frame-dimensions)
           (set-frame-parameter nil 'fullscreen 'fullboth)
           (eight-windows))
          ((equal new-display-geometry dotfiles--m1star-external-display-size)
           (dotfiles--move-to-frame-geometry
            dotfiles--m1star-external-frame-dimensions)
           (set-frame-parameter nil 'fullscreen 'fullboth)
           (eight-windows))
          ;; Ignored and unknown geometries
          ((seq-position dotfiles--ignored-display-sizes
                         new-display-geometry) ())
          (t (dotfiles--warn-about-unknown-display-geometry
              new-display-geometry)))))

(add-hook 'dispwatch-display-change-hooks #'dotfiles--display-changed-hook)
(dispwatch-mode 1)

(provide 'my-ui-geometry)
;;; my-ui-geometry.el ends here
