;;; Package --- base.el
;;; Commentary:
;;; Code:

(column-number-mode)
(display-battery-mode 1)
(electric-pair-mode)
(global-hl-line-mode 1)
(global-visual-line-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq-default initial-buffer-choice
	      (lambda () (get-buffer "*dashboard*")))

;; desktop-save-mode
(desktop-save-mode 1)
(setq-default desktop-auto-save-timeout 1800
              desktop-lazy-verbose nil
              desktop-load-locked-desktop t
              desktop-save t)


;; flyspell
(require 'flyspell)
(dolist (hook '(markdown-mode-hook org-mode-hook text-mode-hook))
  (add-hook hook 'flyspell-mode))

;; display-time-mode
(setq-default display-time-24hr-format t)
(display-time-mode 1)

;; Deletes trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Turns off bell
(setq ring-bell-function 'ignore)

;; Case-insensitive completion
(setq-default completion-ignore-case t
              read-buffer-completion-ignore-case t
              read-file-name-completion-ignore-case t)

;; org-mode setup hook
(add-hook 'org-mode-hook
	  (lambda () (local-set-key (kbd "C-c u") 'org-open-at-point)))

;; eww
(setq-default eww-search-prefix "https://lite.duckduckgo.com/lite?q="
	      shr-width         70)

(defun eww-new-buffer ()
  "Open a new eww buffer."
  (interactive)
  (let ((url (read-from-minibuffer "Enter URL or keywords: ")))
    (switch-to-buffer (generate-new-buffer "*eww*"))
    (eww-mode)
    (eww url)))

;; Copy, cut, and paste
(global-set-key (kbd "C-c c") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-c v") 'clipboard-yank)
(global-set-key (kbd "C-c x") 'clipboard-kill-region)

;; Opens a url in browser
(global-set-key (kbd "C-c u") 'browse-url-at-point)

(provide 'base)

;;; base.el ends here
