;;;; Projectile configs

(use-package projectile
  :ensure t
  :config
  (global-set-key (kbd "C-c p") 'projectile-command-map))

(use-package helm-projectile
  :ensure t)

(provide 'init-projectile)
