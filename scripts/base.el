;;; base.el --- Base Config
;;; Commentary:
;;; Code:

;; Deletes trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Turns off bell
(setq ring-bell-function 'ignore)

;; Case-insensitive completion
(setq-default completion-ignore-case t
              read-buffer-completion-ignore-case t
              read-file-name-completion-ignore-case t)

;; Desktop mode
(desktop-save-mode 1)

;; Disables tab character
(setq-default indent-tabs-mode nil)

;; Electric Pair Mode
(electric-pair-mode)

;; Hides unneccesary ui elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Line height
(setq-default line-spacing 0.2)

;; Mode line
(column-number-mode)

;; Line numbers
(global-display-line-numbers-mode t)
(setq-default display-line-numbers-type 'relative)

(defun disable-line-numbers ()
  "Turn off line numbers."
  (display-line-numbers-mode 0))

(add-hook 'shell-mode-hook 'disable-line-numbers)

;; rcirc
(setq-default
 rcirc-authinfo     `(("libera" nickserv ,(getenv "LIBERA_USERNAME") ,(getenv "LIBERA_PASSWORD")))
 rcirc-server-alist '(("irc.libera.chat"
                       :channels ("##politics"
                                  "#debian"
                                  "#django"
                                  "#emacs"
                                  "#gnome"
                                  "#guix"
                                  "#linux"
                                  "#systemcrafters")
                       :encryption tls
                       :nick "iancurtis"
                       :port 6697)))

(global-set-key (kbd "C-c i") 'irc)
(add-hook 'rcirc-mode-hook 'disable-line-numbers)

;; Reload
(defun reload ()
  "Reload ~/.emacs.d/init.el."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))
(global-set-key (kbd "<f5>") 'reload)  ; Maps 'reload to f5

(provide 'base)
;;; base.el ends here
