;;; Package --- save-state.el
;;; Commentary:
;;; Code:

(desktop-save-mode 1)

(setq-default desktop-auto-save-timeout 1800
              desktop-lazy-verbose nil
              desktop-load-locked-desktop t
              desktop-save t)

(defvar framesets-dir "~/.emacs.d/framesets.d/")

(defun frameset-write-to-file ()
  "Save frameset to file."
  (interactive)
  (let ((default-directory framesets-dir))
    (unless (file-directory-p framesets-dir)
      (make-directory framesets-dir t))
    (let ((file (read-file-name
                 "Save frameset to file: "
                 framesets-dir)))
      (with-temp-file file
        (let ((frameset (frameset-save (frame-list))))
          (prin1 frameset (current-buffer))
          (message "Saved frameset to %s." file))))))

(global-set-key (kbd "C-c w") 'frameset-write-to-file)

(defun frameset-restore-from-file ()
  "Restore frameset from file."
  (interactive)
  (let ((default-directory framesets-dir)
        (file (read-file-name
               "Restore frameset from file: "
               framesets-dir)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (let ((frameset (read (current-buffer))))
          (frameset-restore frameset :reuse-frames t)
          (message "Restored frameset %s." file))))))

(global-set-key (kbd "C-c r") 'frameset-restore-from-file)

(provide 'save-state)
;;; save-state.el ends here
