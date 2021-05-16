;;; keybindings.el --- Keybindings
;;; Commentary:
;;; Code:

;; Command key as meta
(setq mac-command-modifier 'meta
      mac-option-modifier 'none)

;; https://www.emacswiki.org/emacs/GlobalTextScaleMode
(eval-when-compile
  (defvar text-scale-mode)
  (defvar text-scale-mode-amount))
(declare-function text-scale-mode ())

(define-globalized-minor-mode global-text-scale-mode
  text-scale-mode (lambda () (text-scale-mode 1)))

(defun global-text-scale-adjust (inc)
  "Adjust global text scale by INC."
  (text-scale-set 1)
  (kill-local-variable 'text-scale-mode-amount)
  (setq-default text-scale-mode-amount (+ text-scale-mode-amount inc))
  (global-text-scale-mode 1))

(defun global-text-scale-reset ()
  "Reset global text scale."
  (interactive)
  (global-text-scale-adjust (- text-scale-mode-amount))
  (global-text-scale-mode -1))

(global-set-key (kbd "M-=") (lambda () (interactive) (global-text-scale-adjust 1)))
(global-set-key (kbd "M--") (lambda () (interactive) (global-text-scale-adjust -1)))
(global-set-key (kbd "M-0") 'global-text-scale-reset)

;; Reloads user init file
(global-set-key (kbd "C-c r") (lambda ()
                                "Reloads init.el"
                                (interactive)
                                (load-file user-init-file)))

;; Maximized at startup and fullscreen, maximize key bindings
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
(global-set-key (kbd "C-c f") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-c m") 'toggle-frame-maximized)

;; Open a url in browser
(global-set-key (kbd "C-c u") 'browse-url-at-point)

;; Moves cursor across windows
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c k") 'windmove-up)
(global-set-key (kbd "C-c j") 'windmove-down)

;; buffer-move
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)


(provide 'keybindings)
;;; keybindings.el ends here
