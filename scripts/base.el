;;; base.el --- Base Config
;;; Commentary:
;;; Code:

(defun is-mac ()
  "Check if system is macOS."
  (string-equal system-type "darwin"))

;; Saves desktop state on exit
(desktop-save-mode 1)

;; Wraps lines without breaking words
(global-visual-line-mode t)

;; Disable tab characters
(setq-default indent-tabs-mode nil)

;; Deletes trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Case Insensitive Completion
(setq-default completion-ignore-case t
              read-buffer-completion-ignore-case t
              read-file-name-completion-ignore-case t)

;; Hides tool-bar
(tool-bar-mode 0)

;; Hides menu-bar
(menu-bar-mode 0)

;; Disables scroll-bar
(scroll-bar-mode 0)

;; Fonts
(let ((monospace (if (is-mac) "Anonymous Pro" "JetBrains Mono"))
      (sans-serif (if (is-mac) "Alegreya Sans" "Arial"))
      (size "18"))
  (when (find-font (font-spec :name monospace))
    (set-frame-font (format "%s-%s" monospace size) t t)
    (set-face-font 'fixed-pitch-serif monospace))
  (when (find-font (font-spec :name sans-serif))
    (set-face-font 'variable-pitch sans-serif)))

;; Line height
(if (is-mac) (setq-default line-spacing 0.4))

;; Disables bell
(setq ring-bell-function 'ignore)

;; Column number
(column-number-mode)

;; Highlight parenthesis
(eval-when-compile (defvar show-paren-delay))
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Disables global eldoc mode
(global-eldoc-mode 0)

;; Electric Pair Mode
(electric-pair-mode)

;; Displays “lambda” as “λ”
(global-prettify-symbols-mode 1)

;; Hides titlebar icon and text. There was a bug where the text was black on a
;; dark titlebar background ¯\_(ツ)_/¯
(setq frame-title-format nil ns-use-proxy-icon nil)

(provide 'base)
;;; base.el ends here
