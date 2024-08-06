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

;; https://github.com/auto-complete/auto-complete
;; https://auto-complete.github.io/doc/manual.html
(use-package auto-complete
  :config
  (ac-config-default)
  (global-auto-complete-mode))

;; https://github.com/rranelli/auto-package-update.el
(use-package auto-package-update
  :config
  (setq-default auto-package-update-delete-old-versions t
                auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; https://github.com/Malabarba/beacon
(use-package beacon
  :config
  (beacon-mode 1))

;; https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

;; https://github.com/doomemacs
(use-package doom-themes
  :config
  (setq-default doom-themes-enable-bold nil
		doom-themes-enable-italic nil))

;; https://protesilaos.com/emacs/ef-themes
(use-package ef-themes)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("LIBERA_USERNAME" "LIBERA_PASSWORD")))

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

;; https://github.com/emacs-evil/evil-collection
(use-package evil-collection
  :after
  evil
  :config
  (evil-collection-init))

;; https://github.com/wwwjfy/emacs-fish
(use-package fish-mode)

;; https://www.flycheck.org/en/latest/user/installation.html
(use-package flycheck
  :config
  (global-flycheck-mode 1))

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
     ("Go"   gofmt)
     ("HTML" html-tidy))
   format-all-show-errors 'errors)

  (add-hook 'prog-mode-hook 'format-all-mode))

;; https://github.com/emacsorphanage/git-gutter
(use-package git-gutter
  :config
  (global-git-gutter-mode 1))

;; lsp-mode TODO: set this up

;; https://github.com/dominikh/go-mode.el
(use-package go-mode)

;; https://github.com/magit/magit
;; https://magit.vc/manual
(use-package magit)

;; https://github.com/jrblevin/markdown-mode
(use-package markdown-mode
  :config
  (setq-default markdown-command "multimarkdown")
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))

;; https://protesilaos.com/emacs/modus-themes
(use-package modus-themes
  :config
  (setq modus-themes-common-palette-overrides
	`(;; tab bar
	  (bg-tab-bar bg-main)
	  (bg-tab-current bg-active)
	  (bg-tab-other bg-dim)
	  ;; mode line
	  (border-mode-line-active bg-mode-line-active)
	  (border-mode-line-inactive bg-mode-line-inactive)
	  ;; cursor
	  (cursor blue-faint)
	  ,@modus-themes-preset-overrides-faint)))

(use-package moe-theme)

;; https://github.com/sabof/org-bullets
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; https://docs.projectile.mx/projectile/index.html
(use-package projectile
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-commander))

;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook (lambda () (rainbow-delimiters-mode 1))))

;; https://github.com/yoshiki/yaml-mode
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))

;; https://www.emacswiki.org/emacs/DelightedModes
(use-package delight
  :config
  (delight '((abbrev-mode nil "simple")
	     (auto-complete-mode nil "auto-complete")
	     (auto-revert-mode nil "simple")
             (beacon-mode nil "beacon")
             (eldoc-mode nil "eldoc")
             (evil-mode nil "evil")
             (evil-collection-unimpaired-mode nil "evil-collection")
             (flycheck-mode nil "flycheck")
	     (flyspell-mode nil "simple")
	     (font-lock-mode nil "simple")
             (format-all-mode nil "format-all")
	     (git-gutter-mode nil "git-gutter")
             (hs-minor-mode nil "hideshow")
             (projectile-mode nil "projectile")
             (visual-line-mode nil "simple"))))

;; Themes
(defvar dark-theme 'modus-vivendi-tinted)
(defvar light-theme 'modus-operandi-tinted)
(defvar current-theme dark-theme)

(defun my-toggle-theme ()
  "Toggle between light and dark themes."
  (interactive)
  (disable-theme current-theme)
  (setq current-theme
	(if (eq current-theme dark-theme) light-theme dark-theme))
  (load-theme current-theme t))

(global-set-key (kbd "<f5>") 'my-toggle-theme)

(defvar calendar-latitude  38.8339)
(defvar calendar-longitude -104.8214)

;; https://github.com/guidoschmidt/circadian.el
(use-package circadian
  :config
  (setq circadian-themes `((:sunrise . ,light-theme)
			   (:sunset  . ,dark-theme)))
  (add-hook 'circadian-after-load-theme-hook
	    (lambda (theme) (setq current-theme theme)))
  (add-hook 'emacs-startup-hook #'circadian-setup)
  (circadian-setup))

(provide 'packages)

;;; packages.el ends here
