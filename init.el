;;; init.el --- Emacs Initialization
;;; Commentary:
;;; Code:

(defun load-directory (dir)
  "Load all Emacs Lisp files in DIR."
  (let ((files (directory-files dir t "\\.el$")))
    (dolist (file files)
      (load file))))

(load-directory "~/.emacs.d/init.d/")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(calibredb nov git-gutter-fringe git-gutter auto-complete auto-package-update doom-themes emacs-guix evil-collection flycheck markdown-mode use-package yaml-mode))
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number ((t (:inherit (shadow default) :family monospace-font))))
 '(line-number-current-line ((t (:inherit (hl-line default) :family monospace-font)))))

(provide 'init)
;;; init.el ends here
