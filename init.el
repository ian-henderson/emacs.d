;;; Package --- init.el
;;; Commentary:
;;; Code:

(let ((init-dir "~/.emacs.d/init.d"))
  (when (file-directory-p init-dir)
    (dolist (file (directory-files init-dir t "\\.el$"))
      (load-file file))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(fish-mode exec-path-from-shell delight yaml-mode rainbow-delimiters projectile org-bullets moe-theme markdown-mode magit format-all flycheck evil-collection dashboard beacon auto-package-update auto-complete)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)

;;; init.el ends here
