;;; Package --- packages.el
;;; Commentary:
;;; Code:

;; MELPA (Milkypostman's Emacs Lisp Package Archive)
;; https://melpa.org
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; use-package
;; https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'use-package-ensure)
(setq-default use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; auto-complete
;; https://github.com/auto-complete/auto-complete
;; https://auto-complete.github.io/doc/manual.html
(use-package auto-complete
  :config
  (ac-config-default)
  (global-auto-complete-mode))

;; auto-package-update
;; https://github.com/rranelli/auto-package-update.el
(use-package auto-package-update
  :config
  (setq-default auto-package-update-delete-old-versions t
                auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; beacon
;; https://github.com/Malabarba/beacon
(use-package beacon
  :config
  (beacon-mode 1))

;; dashboard
;; https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

;; evil
;; https://github.com/emacs-evil/evil
(use-package evil
  :init
  (setq-default evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (defun toggle-evil-mode ()
    "Toggle evil mode."
    (interactive)
    (evil-mode (if (bound-and-true-p evil-mode) -1 1)))
  (global-set-key (kbd "C-c e") #'toggle-evil-mode))

;; evil-collection
;; https://github.com/emacs-evil/evil-collection
(use-package evil-collection
  :after
  evil
  :config
  (evil-collection-init))

;; flycheck
;; https://www.flycheck.org/en/latest/user/installation.html
(use-package flycheck
  :config
  (global-flycheck-mode 1))

;; format-all
;; https://github.com/lassik/emacs-format-all-the-code
;; https://astyle.sourceforge.net/astyle.html
(use-package format-all
  :config
  (defvar astyle-c-options
    '("--add-braces"
      "--align-pointer=name"
      "--align-reference=name"
      "--indent-col1-comments"
      "--indent-labels"
      "--max-code-length=80"
      "--pad-comma"
      "--pad-header"
      "--pad-oper"
      ;; "--squeeze-lines=1"
      ;; "--squeeze-ws"
      "--unpad-paren"))
  (defvar astyle-c-options-gnu
    (append astyle-c-options
            '("--break-return-type"
              "--break-return-type-decl"
              "--indent=spaces=2"
              "--style=gnu")))
  (defvar astyle-c-options-linux
    (append astyle-c-options
            '("--attach-return-type"
              "--attach-return-type-decl"
              "--indent=force-tab=8"
              "--style=linux")))
  (setq-default
   format-all-formatters
   `(("C"    (astyle ,@(append astyle-c-options-gnu '("--mode=c"))))
     ("C++"  (astyle ,@(append astyle-c-options-gnu '("--mode=c++"))))
     ("HTML" html-tidy))
   format-all-show-errors 'errors)
  (add-hook 'prog-mode-hook 'format-all-mode))

;; lsp-mode TODO: set this up

;; magit
;; https://github.com/magit/magit
;; https://magit.vc/manual
(use-package magit)

;; markdown-mode
;; https://github.com/jrblevin/markdown-mode
(use-package markdown-mode
  :config
  (setq-default markdown-command "multimarkdown")
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))

;; moe-theme
;; https://github.com/kuanyui/moe-theme.el
(defvar dark-theme 'moe-dark)
(defvar light-theme 'moe-light)
(defvar current-theme dark-theme)

(defun my-toggle-theme ()
  "Toggle between light and dark themes."
  (interactive)
  (disable-theme current-theme)
  (setq current-theme (if (eq current-theme dark-theme) light-theme dark-theme))
  (load-theme current-theme t))

(add-hook 'after-init-hook
          (lambda ()
            (load-theme current-theme t)))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (load-theme current-theme t))))

(use-package moe-theme
  :bind
  ("<f5>" . my-toggle-theme))

;; nix-mode
;; https://github.com/NixOS/nix-mode
(use-package nix-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode)))

;; org-bullets
;; https://github.com/sabof/org-bullets
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; projectile
;; https://docs.projectile.mx/projectile/index.html
(use-package projectile
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-commander))

;; rainbow-delimiters
;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook (lambda () (rainbow-delimiters-mode 1))))

;; yaml-mode
;; https://github.com/yoshiki/yaml-mode
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))

;; delight
;; (deliberately placed at end)
;; https://www.emacswiki.org/emacs/DelightedModes
(use-package delight
  :config
  (delight '((auto-complete-mode nil "auto-complete")
             (beacon-mode nil "beacon")
             (eldoc-mode nil "eldoc")
             (evil-mode nil "evil")
             (evil-collection-unimpaired-mode nil "evil-collection")
             (flycheck-mode nil "flycheck")
             (format-all-mode nil "format-all")
             (hs-minor-mode nil "hideshow")
             (projectile-mode nil "projectile")
             (visual-line-mode nil "simple"))))

(provide 'packages)
;;; packages.el ends here
