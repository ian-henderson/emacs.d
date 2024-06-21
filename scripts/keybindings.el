;;; keybindings.el --- Keybindings Config
;;; Commentary:
;;; Code:

;; Maximizing windows
;; (add-to-list 'default-frame-alist '(fullscreen . maximized)) ; maximizes on start
(global-set-key (kbd "C-c f") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-c m") 'toggle-frame-maximized)

;; Moves cursor across panes
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c k") 'windmove-up)
(global-set-key (kbd "C-c l") 'windmove-right)

;; Opens a url in default browser
(global-set-key (kbd "C-c u") 'browse-url-at-point)

(provide 'keybindings)
;;; keybindings.el ends here
