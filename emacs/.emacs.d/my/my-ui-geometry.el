;;;; my-ui-geometry.el --- my UI geometry.  -*- lexical-binding: t; -*-
;;; Commentary:

;; This provides commands for setting up windows in my preferred ways, as well
;; as adjusting frame and window geometry based on the current screen size,
;; useful for docking and undocking laptops. Depends on the `dispwatch' package.

;;; Code:

(defun dotfiles--make-window-grid (columns rows)
  "Create a grid of windows with COLUMNS columns and ROWS rows.
Only supports 1 or 2 rows."
  (delete-other-windows)
  (switch-to-buffer "*scratch*")
  (when (> rows 1)
    (split-window-below))
  ;; Create columns in top row
  (dotimes (_ (1- columns))
    (split-window-right))
  ;; If we have a second row, move down and create columns there
  (when (> rows 1)
    (windmove-down)
    (dotimes (_ (1- columns))
      (split-window-right)))
  (balance-windows))

(defun two-windows ()
  "Make frame contain two vertical windows."
  (interactive)
  (dotfiles--make-window-grid 2 1))

(defun six-windows ()
  "Make frame contain 2x3 windows."
  (interactive)
  (dotfiles--make-window-grid 3 2))

(defun eight-windows ()
  "Make frame contain 2x4 windows."
  (interactive)
  (dotfiles--make-window-grid 4 2))

(require 'cl-lib)

(cl-defstruct (dotfiles--frame-dimensions
               (:constructor dotfiles--make-frame-dimensions))
  "Frame dimensions and position."
  (top nil :type integer :read-only t
       :documentation "Top position in pixels.")
  (left nil :type integer :read-only t
        :documentation "Left position in pixels.")
  (height nil :type integer :read-only t
          :documentation "Frame height in lines.")
  (width nil :type integer :read-only t
         :documentation "Frame width in columns."))

(cl-defstruct (dotfiles--display-config
               (:constructor dotfiles--make-display-config))
  "Complete display configuration."
  (display-size nil :type cons :read-only t
                :documentation "Display size in pixels as (width . height).")
  (name nil :type string :read-only t
        :documentation "Name of this configuration.")
  (frame-dimensions nil :type dotfiles--frame-dimensions :read-only t
                    :documentation "Frame dimensions and position.")
  (window-columns nil :type integer :read-only t
                  :documentation "Number of window columns to create.")
  (window-rows nil :type integer :read-only t
               :documentation "Number of window rows to create.")
  (fullscreen nil :type symbol :read-only t
              :documentation "Fullscreen: nil, `maximized', or `fullboth'."))

(defun dotfiles--configure-initial-frame-alist (config)
  "Configure `initial-frame-alist' with CONFIG."
  (let ((dimensions (dotfiles--display-config-frame-dimensions config))
        (fullscreen (dotfiles--display-config-fullscreen config)))
    (add-to-list 'initial-frame-alist
                 `(top . ,(dotfiles--frame-dimensions-top dimensions)))
    (add-to-list 'initial-frame-alist
                 `(left . ,(dotfiles--frame-dimensions-left dimensions)))
    (add-to-list 'initial-frame-alist
                 `(height . ,(dotfiles--frame-dimensions-height dimensions)))
    (add-to-list 'initial-frame-alist
                 `(width . ,(dotfiles--frame-dimensions-width dimensions)))
    (add-to-list 'initial-frame-alist `(fullscreen . ,fullscreen))
    (when (eq fullscreen 'fullboth)
      (add-to-list 'initial-frame-alist '(fullscreen-restore . maximized)))))

(defun dotfiles--move-to-frame-geometry (geometry)
  "Resize and reposition frame to GEOMETRY."
  (set-frame-position nil (dotfiles--frame-dimensions-left
                           geometry)
                      (dotfiles--frame-dimensions-top geometry))
  (set-frame-size nil (dotfiles--frame-dimensions-width geometry)
                  (dotfiles--frame-dimensions-height geometry)))

(defconst dotfiles--display-configs
  (vector (dotfiles--make-display-config
           :display-size '(1440 . 900)
           :name "darkstar-laptop"
           :frame-dimensions (dotfiles--make-frame-dimensions
                              :top 1 :left 1 :height 55 :width 202)
           :window-columns 2
           :window-rows 1
           :fullscreen 'maximized)
          (dotfiles--make-display-config
           :display-size '(7456 . 1692)
           :name "darkstar-external"
           :frame-dimensions (dotfiles--make-frame-dimensions
                              :top 4 :left 3011 :height 117 :width 426)
           :window-columns 4
           :window-rows 2
           :fullscreen 'fullboth)
          (dotfiles--make-display-config
           :display-size '(1728 . 1117)
           :name "m1-laptop"
           :frame-dimensions (dotfiles--make-frame-dimensions
                              :top 1 :left 1 :height 67 :width 242)
           :window-columns 2
           :window-rows 1
           :fullscreen 'maximized)
          (dotfiles--make-display-config
           :display-size '(3200 . 1800)
           :name "m1star-external"
           :frame-dimensions (dotfiles--make-frame-dimensions
                              :top 0 :left 5568 :height 135 :width 477)
           :window-columns 4
           :window-rows 2
           :fullscreen 'fullboth)
          (dotfiles--make-display-config
           :display-size '(3008 . 1692)
           :name "work-external"
           :frame-dimensions (dotfiles--make-frame-dimensions
                              :top 0 :left 3008 :height 117 :width 427)
           :window-columns 4
           :window-rows 2
           :fullscreen 'fullboth))
  "Display configurations for different machines and display setups.")

(require 'seq)

(defun dotfiles--find-config-for-display (geometry)
  "Find the configuration for GEOMETRY."
  (seq-find (lambda (config)
              (equal (dotfiles--display-config-display-size config) geometry))
            dotfiles--display-configs))

(defconst dotfiles--ignored-display-sizes
  [(3600 . 1080) (5520 . 1080) (4688 . 1692) (3600 . 1692) (7744 . 1692)
   (1920 . 1080) (3200 . 1800) (3840 . 2160)]
  "Possible interim screen resolutions while docking/undocking to be ignored.")

(defun dotfiles--warn-about-unknown-display-geometry (geometry)
  "Diagnose unknown GEOMETRY."
  (message "Unknown display size %sx%s" (car geometry) (cdr geometry)))

(defun dotfiles--apply-display-config-grid (config)
  "Apply the window grid configuration from CONFIG."
  (dotfiles--make-window-grid
   (dotfiles--display-config-window-columns config)
   (dotfiles--display-config-window-rows config)))

;; FIXME(laurynas): does not appear to work
(let* ((geometry (cons (display-pixel-width) (display-pixel-height)))
       (config (seq-find (lambda (cfg)
                           (equal (dotfiles--display-config-display-size cfg)
                                  geometry))
                         dotfiles--display-configs)))
  (cond
   (config
    (dotfiles--configure-initial-frame-alist config)
    (dotfiles--apply-display-config-grid config))
   ;; FIXME(laurynas): interim geometry for the initial frame does not make
   ;; sense
   ((seq-position dotfiles--ignored-display-sizes geometry) nil)
   (t (dotfiles--warn-about-unknown-display-geometry geometry))))

;;; `dispwatch'
(require 'dispwatch)

(defun dotfiles--apply-display-config (config)
  "Apply the display CONFIG to the current frame."
  (dotfiles--move-to-frame-geometry
   (dotfiles--display-config-frame-dimensions config))
  (set-frame-parameter nil 'fullscreen
                       (dotfiles--display-config-fullscreen config))
  (dotfiles--apply-display-config-grid config))

(defvar dotfiles--old-display-geometry nil)

(defun dotfiles--display-changed-hook (new-geometry)
  "Reconfigure frame on screen resolution change to NEW-GEOMETRY."
  (unless (equal dotfiles--old-display-geometry new-geometry)
    (message "Resizing from %s to %s" dotfiles--old-display-geometry
             new-geometry)
    (setq dotfiles--old-display-geometry new-geometry)
    (let ((config (seq-find (lambda (cfg)
                              (equal (dotfiles--display-config-display-size cfg)
                                     new-geometry))
                            dotfiles--display-configs)))
      (cond
       (config (dotfiles--apply-display-config config))
       ((seq-position dotfiles--ignored-display-sizes new-geometry) nil)
       (t (dotfiles--warn-about-unknown-display-geometry new-geometry))))))

(add-hook 'dispwatch-display-change-hooks #'dotfiles--display-changed-hook)
(dispwatch-mode 1)

(provide 'my-ui-geometry)
;;; my-ui-geometry.el ends here
