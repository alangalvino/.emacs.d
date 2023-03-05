(server-start)

;;; Load initfiles and use-package

(setq initfiles-folder (concat user-emacs-directory "initfiles"))
(setq load-path (cons initfiles-folder load-path))

(require 'init-use-package)

;;; Load packages using use-package

(use-package init-gui
  :ensure nil)

(use-package init-preferences
  :ensure nil)

(use-package init-meow
  :ensure nil)

(use-package init-evil
  :disabled t
  :ensure nil)

(use-package init-common-lisp
  :ensure nil)

(use-package init-helm
  :ensure nil)

(use-package init-projectile
  :ensure nil)

(use-package solarized-theme
  :ensure t
  :config
  (setq solarized-use-variable-pitch nil)
  (setq solarized-scale-org-headlines nil)
  (load-theme 'solarized-dark t))

(use-package magit
  :ensure t)

(use-package paredit
  :ensure t
  :config
  (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode))

(use-package aggressive-indent
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook       #'aggressive-indent-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'aggressive-indent-mode)
  (add-hook 'ielm-mode-hook             #'aggressive-indent-mode)
  (add-hook 'lisp-mode-hook             #'aggressive-indent-mode)
  (add-hook 'lisp-interaction-mode-hook #'aggressive-indent-mode)
  (add-hook 'scheme-mode-hook           #'aggressive-indent-mode))

(use-package treemacs 
  :ensure t)

(use-package init-org-mode
  :ensure nil)

(use-package init-osx
  :ensure nil
  :if (eq system-type 'darwin))

(use-package init-global-keybindings
  :ensure nil)
