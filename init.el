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
   '(auto-complete
     auto-package-update
     beacon
     dashboard
     evil
     evil-collection
     exec-path-from-shell
     delight
     fish-mode
     flycheck
     format-all
     magit
     markdown-mode
     moe-theme
     org-bullets
     projectile
     rainbow-delimiters
     yaml-mode)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)

;;; init.el ends here
