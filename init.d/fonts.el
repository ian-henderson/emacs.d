;;; Package --- fonts.el
;;; Commentary:
;;; Code:

(defvar default-font-height 150)
(setq-default line-spacing 0.2)  ; line height

;; "GoMono Nerd Font"
(set-face-attribute
 'default nil :family "Maple Mono" :height default-font-height)

(set-face-attribute
 'variable-pitch nil :family "Open Sans")

(defun increase-font-size ()
  "Increase font size."
  (interactive)
  (set-face-attribute
   'default nil :height (ceiling (* 1.1 (face-attribute 'default :height)))))

(defun decrease-font-size ()
  "Decrease font size."
  (interactive)
  (set-face-attribute
   'default nil :height (floor (* 0.9 (face-attribute 'default :height)))))

(defun reset-font-size ()
  "Reset font size."
  (interactive)
  (set-face-attribute
   'default nil :height default-font-height))

(global-set-key (kbd "C-=") 'increase-font-size)
(global-set-key (kbd "C--") 'decrease-font-size)
(global-set-key (kbd "C-0") 'reset-font-size)

(provide 'fonts)
;;; fonts.el ends here
