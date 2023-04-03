;;; Constants

(defconst +linuxp+   (eq system-type 'gnu/linux))
(defconst +osxp+     (eq system-type 'darwin))

;;; User variables

(defvar user-initfiles-directory      (expand-file-name "initfiles" user-emacs-directory))
;; (defvar user-local-packages-directory (expand-file-name "local/packages/"  user-emacs-directory))
(defvar user-orgmode-dir              "~/orgnotes/")
(defvar user-workspace-dir            "~/workspace/")

;;; Load initfiles, local-packages and use-package

(setq load-path (cons user-initfiles-directory load-path))

;; (let ((default-directory user-local-packages-directory))
;;   (normal-top-level-add-subdirs-to-load-path))

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

;; (use-package init-meow
;;   :ensure nil)

(use-package init-evil
  :ensure nil)

(use-package init-slime
  :ensure nil)

(use-package init-helm
  :ensure nil)

(use-package init-projectile
  :ensure nil)

(use-package solarized-theme
  :ensure t
  :custom
  (solarized-use-variable-pitch nil)
  (solarized-scale-org-headlines nil)
  :config
  (load-theme 'solarized-dark t))

(use-package magit
  :ensure t
  :bind
  ("C-c g" . 'magit-status))

(use-package paredit
  :ensure t
  :delight
  :hook
  ('slime-repl-mode . #'paredit-mode)
  ('emacs-lisp-mode . #'paredit-mode)
  ('lisp-mode . #'paredit-mode))

(use-package aggressive-indent
  :ensure t
  :delight
  :hook
  ('emacs-lisp-mode . #'aggressive-indent-mode)
  ('lisp-mode .       #'aggressive-indent-mode))

(use-package treemacs 
  :ensure t
  :custom
  (treemacs-no-png-images t)
  :delight
  :bind
  ("C-c d" . 'treemacs))

(use-package darkroom 
  :ensure t)

(use-package company
  :ensure t
  :delight
  :hook
  ('after-init . 'global-company-mode)
  :config
  (define-key company-mode-map (kbd "<tab>") 'company-complete))

(use-package which-key
  :ensure t
  :delight
  :config
  (which-key-mode))

(use-package init-org-mode
  :ensure nil)

(use-package init-osx
  :ensure nil
  :if +osxp+)

(use-package helpful
  :ensure t
  :bind
  ("C-x h f" . 'helpful-callable)
  ("C-x h v" . 'helpful-variable)
  ("C-x h k" . 'helpful-key)
  ("C-x h c" . 'helpful-command)
  ("C-c C-d" . 'helpful-at-point))

(use-package init-vterm
  :ensure nil)

(use-package init-global-keybindings
  :ensure nil)
