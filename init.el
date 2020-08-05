(require 'cl)

;; Inits 'package
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

; Bootstraps 'use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Sets up misc configurations
(setq org-notes-home-dir "~/Dropbox/Koala/orgnotes/")
(setq-default ispell-dictionary "american")
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default fill-column 100)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq epa-pinentry-mode 'loopback)
(setq ring-bell-function 'ignore)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq sentence-end-double-space nil)
(setq whitespace-line-column 120)
(setq whitespace-style '(face lines-tail)) ;; highlights chars in lines over whitespace-line-column size

(display-time-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(electric-pair-mode t)
(show-paren-mode t)
(global-hl-line-mode t)
(blink-cursor-mode 0)

(set-face-attribute 'default nil
                    :family "Inconsolata"
                    :height 150
                    :weight 'normal)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook (lambda ()
                            'whitespace-mode
                            'flyspell-mode))

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; My Functions

(defun helm-find-in-dir (dir)
  (let ((default-directory dir))
    (helm-find-files nil)))

(defun helm-find-my-notes ()
  (interactive)
  (helm-find-in-dir org-notes-home-dir))

(defun helm-find-my-workspace ()
  (interactive)
  (helm-find-in-dir "~/workspace/"))

;; Changes evil cursor behavior to be integrated with dvorak layout
;; qwerty -> dvorak
;; j -> h
;; h -> d
;; k -> t
;; l -> n
(defun dvorak-configurations ()
  "My helpful evil-dvorak customizations"
  (interactive)
  ;;normal mode customizations
  (evil-define-key 'normal evil-dvorak-mode-map
    (kbd "j") 'evil-next-line
    (kbd "k") 'evil-previous-line))

;; Installs and configures new packages
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

(use-package diminish
  :ensure t
  :config
  (diminish 'undo-tree-mode)
  (diminish 'eldoc-mode)
  (diminish 'auto-fill-mode)
  (diminish 'org-indent-mode))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))

(use-package go-mode
  :ensure t)

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook
  ((go-mode .lsp)))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode))

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package company
  :ensure t
  :diminish
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-lsp
  :ensure t)

(use-package treemacs
  :ensure t)

(use-package treemacs-evil
  :ensure t)

(use-package flymake-shellcheck
  :ensure t
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

(use-package ace-jump-mode
  :ensure t
  :config
  (define-key global-map (kbd "C-c SPC") 'ace-jump-mode))

(use-package web-mode
  :ensure t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
    (setq web-mode-markup-indent-offset 2)))

(use-package helm
  :config
  (require 'helm-config)
  (setq helm-split-window-in-side-p t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-semantic-fuzzy-match t)
  (setq helm-apropos-fuzzy-match t)
  (setq helm-lisp-fuzzy-completion t)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x C-o") 'helm-occur)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line))

(use-package helm-ag
  :ensure t)

(use-package evil
  :init (evil-mode t)
  :diminish
  :config
  (define-key evil-motion-state-map "H" 'beginning-of-line-text)
  (define-key evil-motion-state-map "L" 'evil-end-of-line))

;; (use-package evil-dvorak
;;   :diminish
;;   :config
;;   (global-evil-dvorak-mode 1)
;;   (dvorak-configurations))

(use-package evil-leader
  :init (global-evil-leader-mode)
  :after evil
  :diminish
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key ":" 'helm-M-x)
  (evil-leader/set-key
    "f f" 'helm-find-files
    "f o" 'helm-find-my-notes
    "f w" 'helm-find-my-workspace
    "f p" 'projectile-switch-project
    "f r" 'helm-recentf
    "g c" 'ace-jump-char-mode
    "g w" 'ace-jump-word-mode
    "g o" 'helm-occur
    "b"   'helm-buffers-list
    "m"   'magit-status
    "o"   'other-frame
    "/"   'helm-projectile-ag
    "t"   'vterm
    "d"   'treemacs
    "f s" 'toggle-frame-fullscreen
    "s v" 'split-window-horizontally
    "s h" 'split-window-vertically))

(use-package key-chord
  :after evil
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "hh" 'evil-normal-state))

(use-package evil-surround
  :ensure t
  :config
  :after evil
  :config
  (global-evil-surround-mode t))

(use-package magit
  :ensure t)

(use-package evil-magit
  :after evil
  :ensure t)

(use-package paredit
  :ensure t
  :diminish
  :config
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode))

(use-package markdown-mode
  :ensure t
  :config
  (set-face-attribute 'markdown-code-face nil :background nil)
  (add-hook 'markdown-mode 'auto-fill-mode))

(use-package solarized-theme
  :ensure t
  :config
  (setq solarized-use-variable-pitch nil)
  (setq solarized-scale-org-headlines nil)
  (load-theme 'solarized-dark t))

(use-package projectile
  :ensure t
  :diminish
  :init
  (setq projectile-completion-system 'helm)
  :config
  (projectile-global-mode))

(use-package helm-projectile
  :ensure t
  :init
  (setq helm-projectile-fuzzy-match t)
  :config
  (progn
    (helm-projectile-on)))

;; Org mode
(org-babel-do-load-languages 'org-babel-load-languages
    '((shell . t)))

(add-hook 'org-mode-hook (lambda ()
                           (auto-fill-mode)
                           (org-indent-mode)))

(use-package org-download
  :ensure t
  :config
  (setq-default org-download-image-dir (concat org-notes-home-dir "pictures")))

(use-package org-bullets
  :config
  (setq org-hide-emphasis-markers t)
  (add-hook 'org-mode-hook (lambda()
                             (org-bullets-mode 1))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" default)))
 '(package-selected-packages
   (quote
    (flymake-shellcheck diminish zenburn-theme which-key web-mode vterm use-package treemacs-evil solarized-theme slime request paredit org-download org-bullets neotree lsp-ui lsp-treemacs key-chord helm-projectile helm-org helm-lsp helm-ag go-mode exec-path-from-shell evil-surround evil-org evil-magit evil-lispy evil-leader company-lsp clojure-mode ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
