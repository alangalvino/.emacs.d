;;; Constants

(defconst +linuxp+   (eq system-type 'gnu/linux))
(defconst +osxp+     (eq system-type 'darwin))

;;; User variables

(defvar user-initfiles-directory      (expand-file-name "initfiles" user-emacs-directory))
(defvar user-orgmode-dir              "~/Orgnotes/")
(defvar user-local-packages-directory (expand-file-name "local/packages/"  user-emacs-directory))

;;; Load initfiles, local-packages and use-package

(setq load-path (cons user-initfiles-directory load-path))

(let ((default-directory user-local-packages-directory))
  (normal-top-level-add-subdirs-to-load-path))

(require 'init-use-package)

;;; Load packages using use-package

(use-package init-helper-functions
  :ensure nil)

(use-package init-gui
  :ensure nil)

(use-package init-preferences
  :ensure nil)

(use-package delight 
  :ensure t)

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
  :delight
  :init
  (progn
    (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
    (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
    (add-hook 'lisp-mode-hook #'paredit-mode)
    (add-hook 'scheme-mode-hook #'paredit-mode)
    (add-hook 'ielm-mode-hook #'paredit-mode)
    (add-hook 'clojure-mode-hook #'paredit-mode)))

(use-package aggressive-indent
  :ensure t
  :delight
  :config
  (add-hook 'emacs-lisp-mode-hook       #'aggressive-indent-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'aggressive-indent-mode)
  (add-hook 'ielm-mode-hook             #'aggressive-indent-mode)
  (add-hook 'lisp-mode-hook             #'aggressive-indent-mode)
  (add-hook 'lisp-interaction-mode-hook #'aggressive-indent-mode)
  (add-hook 'scheme-mode-hook           #'aggressive-indent-mode))

(use-package treemacs 
  :ensure t
  :delight)

(use-package darkroom 
  :ensure t)

(use-package company
  :ensure t
  :delight
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package init-org-mode
  :ensure nil)

(use-package init-osx
  :ensure nil
  :if +osxp+)

(use-package helpful
  :ensure t
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point))

(use-package init-global-keybindings
  :ensure nil)
