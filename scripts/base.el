(defun reload ()
  "Reloads user init file."
  (interactive)
  (load-file user-init-file))

(global-set-key (kbd "C-c r") 'reload)

;; Disables global eldoc mode
(setq global-eldoc-mode nil)

;; Command key as meta
(setq mac-command-modifier 'meta
      mac-option-modifier 'none)

;; Typeface
(set-face-attribute 'default nil :font "Fantasque Sans Mono-18")

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
  (setq-default display-line-numbers-type 'relative)
  (global-display-line-numbers-mode))

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

;; Hides tool-bar
(tool-bar-mode -1)

;; Hides menu-bar
(menu-bar-mode -1)
