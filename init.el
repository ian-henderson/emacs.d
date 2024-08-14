;;; Package --- init.el
;;; Commentary:
;;; Code:

(let ((init-directory (expand-file-name "init.d" user-emacs-directory)))
  (when (file-directory-p init-directory)
    (dolist (file (directory-files init-directory t "\\.el$"))
      (load-file file))))

(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(circadian delight yaml-mode rainbow-delimiters projectile org-bullets moe-theme modus-themes markdown-mode magit use-package magit-section go-mode git-gutter git-commit format-all flycheck fish-mode exec-path-from-shell evil-collection ef-themes doom-themes dashboard beacon auto-package-update auto-complete)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
