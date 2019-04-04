(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(package-selected-packages
   (quote
    (auto-package-update evil solarized-theme add-node-modules-path flycheck web-mode use-package projectile magit exec-path-from-shell autothemer auto-complete))))

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

(global-set-key (kbd "C-c r") 'reload)

;; Package Archives
(require 'package)

 ;; Milkypostman's Emacs Lisp Package Archive
(add-to-list
 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

;; https://github.com/jwiegley/use-package
(eval-when-compile (require 'use-package))

;; https://github.com/codesuki/add-node-modules-path
(use-package add-node-modules-path :ensure t)

;; https://github.com/auto-complete/auto-complete
(use-package auto-complete
  :config
  (ac-config-default)
  (global-auto-complete-mode t)
  :ensure t)

;; https://github.com/rranelli/auto-package-update.el
(use-package auto-package-update :config (auto-package-update-maybe) :ensure t)

;; https://github.com/emacs-evil/evil
(use-package evil :config (evil-mode 1) :ensure t)

;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize)
  :ensure t
  :if (memq window-system '(mac ns x)))

;; https://flycheck.readthedocs.io/en/latest/
(use-package flycheck
  :config
  (add-hook 'scss-mode-hook 'flycheck-mode)
  (flycheck-add-mode 'sass/scss-sass-lint 'scss-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  :ensure t)

;; https://magit.vc/manual/magit/
(use-package magit
  :bind ("C-x g" . magit-status)
  :ensure t
  :init (setq magit-refresh-status-buffer nil))

;; https://docs.projectile.mx/
(use-package projectile
  :bind (("s-p" . projectile-command-map)
         ("C-c p" . projectile-command-map))
  :config (projectile-mode +1)
  :ensure t)

;; https://github.com/bbatsov/solarized-emacs
(use-package solarized-theme
  :config (load-theme 'solarized-dark)
  :ensure t
  :init (setq solarized-distinct-fringe-background t
              solarized-high-contrast-mode-line t
              solarized-use-more-italic t))

(defun my-web-mode-hook ()
  "web-mode configuration."
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  (setq web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-markup-indent-offset 2)
  (if (equal web-mode-content-type "javascript")
      (web-mode-set-content-type "jsx")
    (message "content-type set to %s" web-mode-content-type)))

;; http://web-mode.org/
(use-package web-mode
  :config
  (add-hook 'web-mode-hook #'add-node-modules-path)
  (add-hook 'web-mode-hook 'flycheck-mode)
  (add-hook 'web-mode-hook 'my-web-mode-hook)
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode)))

;; Disables global eldoc mode
(setq global-eldoc-mode nil)

;; Command key as meta
(setq mac-command-modifier 'meta
      mac-option-modifier 'none)

;; Typeface
(set-face-attribute 'default nil :font "Roboto Mono-13")

;; Line height
(setq-default line-spacing 0.15)

;; Scroll bar
(scroll-bar-mode -1)

;; Full-screen and maximize key bindings and startup maximized
(global-set-key (kbd "C-c f") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-c m") 'toggle-frame-maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

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

;; Open a url in browser
(global-set-key (kbd "C-c u") 'browse-url-at-point)
