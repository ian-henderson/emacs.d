(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(fci-rule-color "#073642")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (heaven-and-hell auto-package-update evil solarized-theme add-node-modules-path flycheck web-mode use-package projectile magit exec-path-from-shell autothemer auto-complete)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#839496" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))))

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
  (flycheck-add-mode 'sass/scss-sass-lint 'scss-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode))

;; https://github.com/valignatev/heaven-and-hell
(use-package heaven-and-hell
  :bind ("C-c s" . heaven-and-hell-toggle-theme)
  :hook (after-init . heaven-and-hell-init-hook)
  :init (setq heaven-and-hell-theme-type 'dark
              heaven-and-hell-themes '((dark . solarized-dark)
                                       (light . solarized-light))))

;; https://magit.vc/manual/magit/
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-file-dispatch)))

;; https://docs.projectile.mx/
(use-package projectile
  :bind (("s-p" . projectile-command-map)
         ("C-c p" . projectile-command-map))
  :config (projectile-mode +1))

;; https://github.com/bbatsov/solarized-emacs
(use-package solarized-theme
  :bind ("C-c s" . 'toggle-solarized)
  :config (load-theme 'solarized-dark)
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
(set-face-attribute 'default nil :font "PT Mono-12")

;; Line height
(setq-default line-spacing 0.3)

;; Scroll bar
(scroll-bar-mode -1)

;; Maximized at startup and fullscreen, maximize key bindings
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(global-set-key (kbd "C-c f") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-c m") 'toggle-frame-maximized)

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
