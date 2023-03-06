(defun server-shutdown ()
  "Save buffers, quit, and shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(provide 'init-helper-functions)
