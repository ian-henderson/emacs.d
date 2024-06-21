;;; base.el --- Base Config
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(column-number-mode)      ; column, row in mode line
(desktop-save-mode 1)     ; desktop-save, desktop-read
(display-battery-mode 1)
(electric-pair-mode)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Clock
(display-time-mode 1)
(setq-default display-time-format "%H:%M")  ; %a, %d %b %Y

;; Deletes trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Turns off bell
(setq ring-bell-function 'ignore)

;; Case-insensitive completion
(setq-default completion-ignore-case t
              read-buffer-completion-ignore-case t
              read-file-name-completion-ignore-case t)

(setq-default indent-tabs-mode nil)  ; Disables tab character
(setq-default shr-width 50)          ; eww max width

;; Line numbers
(setq-default display-line-numbers-type 'relative)
(defun enable-line-numbers ()
  "Enable line numbers."
  (display-line-numbers-mode 1))

(dolist (mode '(emacs-lisp-mode scheme-mode shell-script-mode))
  (add-hook mode #'enable-line-numbers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rcirc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default
 rcirc-authinfo     `(("libera" nickserv ,(getenv "LIBERA_USERNAME") ,(getenv "LIBERA_PASSWORD")))
 rcirc-server-alist '(("irc.libera.chat"
                       :channels ("##politics"
                                  "#debian"
                                  "#django"
                                  "#emacs"
                                  "#gnome"
                                  "#guile"
                                  "#guix"
                                  "#linux"
                                  "#lisp"
                                  "#scheme"
                                  "#systemcrafters")
                       :encryption tls
                       :nick "iancurtis"
                       :port 6697)))

(global-set-key (kbd "C-c i") 'irc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reload ()
  "Reload ~/.emacs.d/init.el."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(global-set-key (kbd "C-c r") 'reload)

;; Opens a url in browser
(global-set-key (kbd "C-c u") 'browse-url-at-point)

;; Maximizing windows
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ; maximizes on start
(global-set-key (kbd "C-c f") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-c m") 'toggle-frame-maximized)

;; Moves cursor across panes
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c k") 'windmove-up)
(global-set-key (kbd "C-c l") 'windmove-right)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fonts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar monospace-font "Iosevka SS08")
(defvar proportional-font "Iosevka Aile")
(set-face-attribute 'default nil :family monospace-font)
(set-face-attribute 'variable-pitch nil :family proportional-font)

(defvar default-font-height 200)
(set-face-attribute 'default nil :height default-font-height)

(setq-default line-spacing 0.0)  ; line height

(defun increase-font-size ()
  "Increase font size."
  (interactive)
  (set-face-attribute
   'default nil :height (ceiling (* 1.1 (face-attribute 'default :height)))))

(defun decrease-font-size ()
  "Decrease font size."
  (interactive)
  (set-face-attribute
   'default nil :height (floor (* 0.9 (face-attribute 'default :height)))))

(defun reset-font-size ()
  "Reset font size."
  (interactive)
  (set-face-attribute
   'default nil :height default-font-height))

(global-set-key (kbd "C-=") 'increase-font-size)
(global-set-key (kbd "C--") 'decrease-font-size)
(global-set-key (kbd "C-0") 'reset-font-size)

(provide 'base)
;;; base.el ends here
