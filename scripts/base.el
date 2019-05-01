;; Wraps lines without breaking words
(global-visual-line-mode t)

;; Disable tab characters
(setq-default indent-tabs-mode nil)

;; Deletes trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Case Insensitive Completion
(setq completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)

;; Hides tool-bar
(tool-bar-mode -1)

;; Hides menu-bar
(menu-bar-mode -1)

;; Command key as meta
(setq mac-command-modifier 'meta
      mac-option-modifier 'none)

;; Fonts
(let ((monospace "Fira Mono") (sans-serif "Fira Sans") (size "18"))
  (when (find-font (font-spec :name monospace))
    (set-frame-font (format "%s-%s" monospace size) t t)
    (set-face-font 'fixed-pitch-serif monospace))
  (when (find-font (font-spec :name sans-serif))
    (set-face-font 'variable-pitch sans-serif)))

;; Line height
(setq-default line-spacing 0.2)

;; Disables scroll-bar
(scroll-bar-mode -1)

;; Disables bell
(setq ring-bell-function 'ignore)

;; Line Numbers
(when (version<= "26.0.50" emacs-version)
  (setq-default display-line-numbers-type 'relative)
  (global-display-line-numbers-mode))

;; Column number
(column-number-mode)

;; Highlight parenthesis
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Disables global eldoc mode
(setq global-eldoc-mode nil)

;; Electric Pair Mode
(electric-pair-mode)

;; Displays “lambda” as “λ”
(global-prettify-symbols-mode 1)

;; https://www.emacswiki.org/emacs/GlobalTextScaleMode
(define-globalized-minor-mode global-text-scale-mode
  text-scale-mode (lambda () (text-scale-mode 1)))

(defun global-text-scale-adjust (inc) (interactive)
       (text-scale-set 1)
       (kill-local-variable 'text-scale-mode-amount)
       (setq-default text-scale-mode-amount (+ text-scale-mode-amount inc))
       (global-text-scale-mode 1))

(defun global-text-scale-reset () (interactive)
       (global-text-scale-adjust (- text-scale-mode-amount))
       (global-text-scale-mode -1))

(global-set-key (kbd "M-=") (lambda () (interactive) (global-text-scale-adjust 1)))
(global-set-key (kbd "M--") (lambda () (interactive) (global-text-scale-adjust -1)))
(global-set-key (kbd "M-0") 'global-text-scale-reset)

;; Reloads user init file
(global-set-key (kbd "C-c r") (lambda () (interactive) (load-file user-init-file)))

;; Maximized at startup and fullscreen, maximize key bindings
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(global-set-key (kbd "C-c f") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-c m") 'toggle-frame-maximized)

;; Open a url in browser
(global-set-key (kbd "C-c u") 'browse-url-at-point)
