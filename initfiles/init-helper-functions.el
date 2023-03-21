(defun find-file-or-projectile ()
  "If on projectile tries to find file with projectile if not with helm"
  (interactive)
  (if (projectile-project-p)
      (call-interactively 'helm-projectile-find-file)
    (call-interactively 'helm-find-files)))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun server-shutdown ()
  "Save buffers, quit, and shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(provide 'init-helper-functions)
