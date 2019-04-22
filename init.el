(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "356e5cbe0874b444263f3e1f9fffd4ae4c82c1b07fe085ba26e2a6d332db34dd" default)))
 '(package-selected-packages
   (quote
    (web-mode projectile magit flycheck exec-path-from-shell evil doom-themes auto-package-update auto-complete add-node-modules-path use-package))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'load-path "~/.emacs.d/scripts")
(load "base")
(load "packages")
