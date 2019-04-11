;; Package Archives
(require 'package)

 ;; Milkypostman's Emacs Lisp Package Archive
(add-to-list
 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

;; https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

;; https://github.com/codesuki/add-node-modules-path
(use-package add-node-modules-path)

;; https://github.com/auto-complete/auto-complete
(use-package auto-complete
  :config
  (ac-config-default)
  (global-auto-complete-mode t))

;; https://github.com/rranelli/auto-package-update.el
(use-package auto-package-update :config (auto-package-update-maybe))

(defun toggle-theme ()
  "Toggles dark and light themes."
  (interactive)
  (catch 'unset-variables
    (when (null (and (boundp 'current-theme)
                     (boundp 'dark-theme)
                     (boundp 'light-theme)))
      (throw 'unset-variables
             "Requires current-theme, dark-theme, and light-theme to be set."))
    (setq current-theme
          (if (equal current-theme dark-theme) light-theme dark-theme))
    (load-theme current-theme t)
    (message "theme: %s" current-theme)))

;; https://github.com/hlissner/emacs-doom-themes
(use-package doom-themes
  :bind
  ("C-c t" . 'toggle-theme)
  :config
  (doom-themes-org-config)
  (doom-themes-visual-bell-config)
  (setq dark-theme 'doom-nord
        light-theme 'doom-nord-light
        current-theme dark-theme)
  (load-theme current-theme t))

;; https://github.com/emacs-evil/evil
(use-package evil :config (evil-mode 1))

;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize)
  :if (memq window-system '(mac ns x)))

;; https://flycheck.readthedocs.io/en/latest/
(use-package flycheck
  :config
  (add-hook 'scss-mode-hook 'flycheck-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'sass/scss-sass-lint 'scss-mode))

;; https://magit.vc/manual/magit/
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-file-dispatch)))

;; https://docs.projectile.mx/
(use-package projectile
  :bind (("s-p" . projectile-command-map)
         ("C-c p" . projectile-command-map))
  :config (projectile-mode +1))

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
  :init
  (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode)))
