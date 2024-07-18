;;; Package --- init.el
;;; Commentary:
;;; Code:

(let ((init-dir "~/.emacs.d/init.d"))
  (when (file-directory-p init-dir)
    (dolist (file (directory-files init-dir t "\\.el$"))
      (load-file file))))

(provide 'init)
;;; init.el ends here
