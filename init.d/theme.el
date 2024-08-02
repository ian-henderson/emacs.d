;;; Package --- theme.el
;;; Commentary:
;;; Code:

(defvar dark-theme 'moe-dark)
(defvar light-theme 'moe-light)
(defvar current-theme dark-theme)

(defun my-toggle-theme ()
  "Toggle between light and dark themes."
  (interactive)
  (disable-theme current-theme)
  (setq current-theme
	(if (eq current-theme dark-theme) light-theme dark-theme))
  (load-theme current-theme t))

(add-hook 'after-init-hook
          (lambda () (load-theme current-theme t)))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame (load-theme current-theme t))))

(global-set-key (kbd "<f5>") 'my-toggle-theme)

(provide 'theme)

;;; theme.el ends here
