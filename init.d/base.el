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

;; custom file
(defvar my-custom-file
  (expand-file-name "init.d/custom.el" user-emacs-directory))

(setq custom-file my-custom-file)

(unless (file-exists-p my-custom-file)
  (with-temp-buffer
    (insert ";;; Package --- custom.el\n")
    (insert ";;; Commentary:\n")
    (insert ";;; Code:\n\n")
    (insert "(provide 'custom)\n\n")
    (insert ";;; custom.el ends here\n")
    (write-region (point-min) (point-max) my-custom-file)))

(load my-custom-file)

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

(add-hook 'org-mode-hook
	  (lambda () (local-set-key (kbd "C-c u") 'org-open-at-point)))

;; Copy, cut, and paste
(global-set-key (kbd "C-c c") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-c v") 'clipboard-yank)
(global-set-key (kbd "C-c x") 'clipboard-kill-region)

;; Opens a url in browser
(global-set-key (kbd "C-c u") 'browse-url-at-point)

(provide 'base)

;;; base.el ends here
