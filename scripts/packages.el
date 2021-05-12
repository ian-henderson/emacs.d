;;; packages.el --- Melpa Packages
;;; Code:
;;; Commentary:

;; Package Archives
(require 'package)

 ;; Milkypostman's Emacs Lisp Package Archive
(add-to-list
 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; https://github.com/jwiegley/use-package
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

;; https://github.com/codesuki/add-node-modules-path
(use-package add-node-modules-path)

;; https://github.com/domtronn/all-the-icons.el
(use-package all-the-icons :if (string-equal system-type "darwin"))

;; https://github.com/auto-complete/auto-complete
(use-package auto-complete
  :config
  (ac-config-default)
  (global-auto-complete-mode t))

;; https://github.com/rranelli/auto-package-update.el
(use-package auto-package-update :config (auto-package-update-maybe))

;; https://github.com/Silex/docker.el
(use-package docker :bind ("C-c d" . docker) :ensure t)

;; https://github.com/emacs-pe/docker-tramp.el
(use-package docker-tramp)

;; https://github.com/seagle0128/doom-modeline (uses all-the-icons)
(use-package doom-modeline
  :ensure t
  :if (string-equal system-type "darwin")
  :init (doom-modeline-mode 1))

;; https://github.com/hlissner/emacs-doom-themes
(eval-when-compile
  (defvar current-theme)
  (defvar dark-theme)
  (defvar light-theme))

(defun toggle-theme ()
  "Toggle between light and dark themes."
  (interactive)
  (setq current-theme
        (if (equal current-theme dark-theme) light-theme dark-theme))
  (load-theme current-theme t)
  (message "loaded %s" current-theme))

(use-package doom-themes
  :bind ("C-c t" . 'toggle-theme)
  :config
  (doom-themes-org-config)
  (doom-themes-visual-bell-config)
  (setq dark-theme 'doom-one
        light-theme 'doom-one-light
        current-theme dark-theme)
  (load-theme current-theme t))

;; https://github.com/emacs-evil/evil
(use-package evil
  :hook
  (conf-mode . evil-local-mode)
  (css-mode . evil-local-mode)
  (elixir-mode . evil-local-mode)
  (emacs-lisp-mode . evil-local-mode)
  (markdown-mode . evil-local-mode)
  (rust-mode . evil-local-mode)
  (shell-script-mode . evil-local-mode)
  (solidity-mode . evil-local-mode)
  (web-mode . evil-local-mode))

;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize)
  :if (memq window-system '(mac ns x)))

;; https://flycheck.readthedocs.io/en/latest/
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; https://magit.vc/manual/magit/
(use-package magit
  :bind (("C-x g" . magit-status) ("C-c g" . magit-file-dispatch)))

;; https://github.com/jrblevin/markdown-mode
(use-package markdown-mode
  :ensure t
  :init (setq markdown-command "multimarkdown")
  :mode ("\\.(md|markdown)\\'" . gfm-mode))

;; https://github.com/jscheid/prettier.el
(use-package prettier
  :config (add-hook 'after-init-hook #'global-prettier-mode))

;; https://github.com/rust-lang/rust-mode
(use-package rust-mode)

;; http://web-mode.org/
(eval-when-compile
  (defvar web-mode-code-indent-offset)
  (defvar web-mode-content-type)
  (defvar web-mode-css-indent-offset)
  (defvar web-mode-indentation-params)
  (defvar web-mode-markup-indent-offset))

(defun web-mode-config ()
  "Web mode config."
  (flycheck-mode)
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  (setq web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-markup-indent-offset 2))

(use-package web-mode
  :config
  (declare-function web-mode-set-content-type ())
  (if (equal web-mode-content-type "javascript")
      (web-mode-set-content-type "jsx")
    (message "content-type set to %s" web-mode-content-type))
  (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ts[x]?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
  :hook
  (web-mode . web-mode-config))

(provide 'packages)
;;; packages.el ends here
