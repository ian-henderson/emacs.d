;;; init.el --- Emacs Initialization
;;; Commentary:
;;; Code:

(add-to-list 'load-path "~/.emacs.d/scripts")
(load "base")
(load "keybindings")
(load "packages")

(provide 'init)
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(magit doom-modeline yaml-mode use-package projectile flycheck evil-collection doom-themes auto-package-update auto-complete))
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
