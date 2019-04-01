(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (zenburn-theme add-node-modules-path flycheck web-mode use-package projectile magit exec-path-from-shell autothemer auto-complete))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun reload ()
  "Reloads user init file."
  (interactive)
  (load-file user-init-file))

;; Package Archives
(require 'package)
(add-to-list
 ;; Milkypostman's Emacs Lisp Package Archive
 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

(eval-when-compile
  (require 'use-package))

;; https://github.com/auto-complete/auto-complete
(use-package auto-complete
  :config
  (ac-config-default)
  (global-auto-complete-mode t))

;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package flycheck
  :config (flycheck-add-mode 'javascript-eslint 'web-mode))

;; https://magit.vc/manual/magit/
(use-package magit
  :bind ("C-x g" . magit-status)
  :init (setq magit-refresh-status-buffer nil))

;; https://docs.projectile.mx/
(use-package projectile
  :bind (("s-p" . projectile-command-map)
         ("C-c p" . projectile-command-map))
  :config (projectile-mode +1))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  (setq web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-markup-indent-offset 2)
  (if (equal web-mode-content-type "javascript")
      (web-mode-set-content-type "jsx")
    (message "content-type set to %s" web-mode-content-type)))

;; http://web-mode.org/
(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
  (add-hook 'web-mode-hook #'add-node-modules-path)
  (add-hook 'web-mode-hook 'flycheck-mode)
  (add-hook 'web-mode-hook 'my-web-mode-hook))

(use-package zenburn-theme
  :config (load-theme 'zenburn))

;; Disables global eldoc mode
(setq global-eldoc-mode nil)

;; Command key as meta
(setq mac-command-modifier 'meta
      mac-option-modifier 'none)

;; Typeface
(set-face-attribute 'default nil :font "Ubuntu Mono-24")

;; Line height
(setq-default line-spacing 0.2)

;; Scroll bar
(scroll-bar-mode -1)

;; Full screen
(set-frame-parameter nil 'fullscreen 'fullboth)

(defun toggle-fullscreen ()
  "Toggle full screen."
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

;; Column number
(column-number-mode)

;; Highlight parenthesis
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Disable tab characters
(setq-default indent-tabs-mode nil)

;; Deletes trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Case Insensitive Completion
(setq completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)
