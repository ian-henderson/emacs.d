;;; packages.el --- Melpa Packages
;;; Code:
;;; Commentary:

;; Milkypostman's Emacs Lisp Package Archive
(require 'package)
(add-to-list 'package-archives
 '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; use-package (https://github.com/jwiegley/use-package)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

;; auto-complete (https://github.com/auto-complete/auto-complete)
(use-package auto-complete
  :config (ac-config-default))

;; auto-package-update (http://github.com/rranelli/auto-package-update.el)
(use-package auto-package-update
  :config (auto-package-update-maybe))

;; doom-themes (https://github.com/doomemacs/themes)
(use-package doom-themes
  :config (load-theme 'doom-tomorrow-night t))

;; emacs-guix (https://github.com/alezost/guix.el)
(use-package emacs-guix)

;; (https://github.com/emacs-evil/evil)
(use-package evil
  :config (evil-mode 1)
  :init (setq evil-want-keybinding nil))

(defun toggle-evil-mode ()
  "Toggle Evil mode."
  (interactive)
  (evil-mode (if (bound-and-true-p evil-mode) -1 1)))

(global-set-key (kbd "C-c e") #'toggle-evil-mode)

;; evil-collection (https://github.com/emacs-evil/evil-collection)
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; flycheck (https://github.com/)
(use-package flycheck
  :init (global-flycheck-mode))

;; magit
(use-package magit)

;; markdown-mode (https://github.com/jrblevin/markdown-mode)
(use-package markdown-mode
  :init
  (setq markdown-command "multimarkdown")
  :config
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))

;; yaml-mode (https://github.com/yoshiki/yaml-mode)
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))

(provide 'packages)
;;; packages.el ends here
