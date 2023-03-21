;;;; Projectile configs

(use-package projectile
  :ensure t
  :bind
  ("C-c p" . 'projectile-command-map))

(use-package helm-projectile
  :ensure t)

(provide 'init-projectile)
