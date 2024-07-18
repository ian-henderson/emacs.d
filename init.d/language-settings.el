;;; Package --- language-settings.el
;;; Commentary:
;;; Code:

;; C/C++

(defun c-mode-hook-gnu ()
  "Custom C programming settings for following the Linux kernel style."
  (setq-default tab-width 8)
  (c-set-style "gnu"))

(defun c-mode-hook-linux ()
  "Custom C programming settings for following the Linux kernel style."
  (setq-default c-argdecl-indent 8
                c-basic-offset 8
                c-brace-imaginary-offset 0
                c-brace-offset 0
                c-continued-statement-offset 8
                c-indent-level 8
                c-label-offset -8
                indent-tabs-mode t
                tab-width 8)
  (c-set-style "linux"))

(add-hook 'c-mode-hook 'c-mode-hook-gnu)

;; Hideshow minor mode
;; key bindings:
;; C-c @ C-h	hs-hide-block
;; C-c @ C-M-h	hs-hide-all
;; C-c @ C-s	hs-show-block
;; C-c @ C-M-s	hs-show-all
(defvar hs-minor-mode-whitelist
  '(c-mode
    emacs-lisp-mode
    mhtml-mode nix-mode)
  "List of modes where hw-minor-mode should be disabled.")
(dolist (mode hs-minor-mode-whitelist)
  (let ((hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook hook (lambda () (hs-minor-mode)))))

(provide 'language-settings)
;;; language-settings.el ends here
