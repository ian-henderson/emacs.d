(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("84890723510d225c45aaff941a7e201606a48b973f0121cb9bcb0b9399be8cba" default))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Package Archives
(require 'package)
(add-to-list 'package-archives
             ;; Milkypostman’s Emacs Lisp Package Archive
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

(eval-when-compile
  (require 'use-package))

;; https://github.com/auto-complete/auto-complete
(use-package auto-complete
  :config (auto-complete-mode))

;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t
  :config (exec-path-from-shell-initialize))

;; https://magit.vc/manual/magit/
(use-package magit
  :bind ("C-x g" . magit-status)
  :config (setq magit-refresh-status-buffer nil))

;; https://docs.projectile.mx/
(use-package projectile
  :bind (("s-p" . projectile-command-map)
         ("C-c p" . projectile-command-map))
  :config (projectile-mode +1))

;; https://github.com/felipeochoa/rjsx-mode
(use-package rjsx-mode
  :init (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
        (setq js-indent-level 2))

(use-package zenburn-theme
  :init (load-theme 'zenburn))

(defun init ()
  "Loads ~/.emacs.d/init.el"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; Command key as meta
(setq mac-command-modifier 'meta
      mac-option-modifier 'none)

;; Typeface
(set-face-attribute 'default nil :font "Ubuntu Mono-14")

;; Line height
(setq-default line-spacing 0.2)

;; Scroll bar
(scroll-bar-mode -1)

;; Full screen
(set-frame-parameter nil 'fullscreen 'fullboth)

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil
   'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

;; Turns off bell
(setq ring-bell-function 'ignore)

;; Electric Pair Mode
(electric-pair-mode)

;; Line Numbers
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode)
  (setq-default display-line-numbers-type 'relative))

;; Column Number
(column-number-mode)

;; Highlight parenthesis
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Disable tab characters
(setq-default indent-tabs-mode nil)

;; Deletes trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)
