; System specific setup for Darwin
; TODO: factor out common bits with linux.el

; Default font
(defconst my-frame-font
  "-apple-Menlo-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1"
  "My default frame font on Darwin")

(defun system-specific-presetup()
  "Things that must be set on Darwin before main setup"

  (progn
    (add-to-list
     'default-frame-alist
     `(font . ,my-frame-font))
    (set-frame-font my-frame-font nil t)))

(defun system-specific-setup()
  "Setup specifics for Darwin"
  ())

